-- This is currently the complete, authoritative schema, as it is easier to
-- understand all in one place and unnecessary to use proper evolutions until
-- production.  Play only checks for changes in the most recent evolution, so
-- changing this file will not prompt an evolution while 2.sql is unchanged.

-- Theoretically this could be maintained as authoritative during production,
-- with further evolutions only making defensive changes, but it may be easier
-- to keep separate.

-- Currently these are largely under-indexed.

# --- !Ups
;

----------------------------------------------------------- utilities

-- Note that the double-semicolons are necessary for play's poor evolution parsing
CREATE FUNCTION create_abstract_parent ("parent" name, "children" name[]) RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	parent_table CONSTANT text := quote_ident(parent);;
	kind_type CONSTANT text := quote_ident(parent || '_kind');;
BEGIN
	EXECUTE $macro$
		CREATE TYPE $macro$ || kind_type || $macro$ AS ENUM ('$macro$ || array_to_string(children, $$','$$) || $macro$');;
		CREATE TABLE $macro$ || parent_table || $macro$ (
			"id" serial NOT NULL Primary Key,
			"kind" $macro$ || kind_type || $macro$ NOT NULL
		);;
		CREATE FUNCTION $macro$ || quote_ident(parent || '_trigger') || $macro$ () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
			IF TG_OP = 'INSERT' THEN
				INSERT INTO $macro$ || parent_table || $macro$ (id, kind) VALUES (NEW.id, TG_TABLE_NAME::$macro$ || kind_type || $macro$);;
			ELSIF TG_OP = 'DELETE' THEN
				DELETE FROM $macro$ || parent_table || $macro$ WHERE id = OLD.id AND kind = TG_TABLE_NAME::$macro$ || kind_type || $macro$;;
			ELSIF TG_OP = 'UPDATE' THEN
				IF NEW.id = OLD.id THEN
					RETURN NEW;;
				END IF;;
				UPDATE $macro$ || parent_table || $macro$ SET id = NEW.id WHERE id = OLD.id AND kind = TG_TABLE_NAME::$macro$ || kind_type || $macro$;;
			END IF;;
			IF NOT FOUND THEN
				RAISE EXCEPTION 'inconsistency for %:% parent $macro$ || parent || $macro$', TG_TABLE_NAME::$macro$ || kind_type || $macro$, OLD.id;;
			END IF;;
			IF TG_OP = 'DELETE' THEN
				RETURN OLD;;
			ELSE
				RETURN NEW;;
			END IF;;
		END;;$$
	$macro$;;
END;; $create$;
COMMENT ON FUNCTION "create_abstract_parent" (name, name[]) IS 'A "macro" to create an abstract parent table and trigger function.  This could be done with a single function using dynamic EXECUTE but this way is more efficient and not much more messy.';

CREATE FUNCTION "cast_int" ("input" text) RETURNS integer LANGUAGE plpgsql IMMUTABLE STRICT AS $$
DECLARE
	i integer;;
BEGIN
	SELECT input::integer INTO i;;
	RETURN i;;
EXCEPTION WHEN invalid_text_representation THEN
	RETURN NULL;;
END;; $$;

----------------------------------------------------------- auditing

CREATE TYPE audit_action AS ENUM ('login', 'logout', 'add', 'change', 'remove', 'download');
COMMENT ON TYPE audit_action IS 'The various activities for which we keep audit records (in audit or a derived table).';

CREATE TABLE "audit" (
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"who" int NOT NULL, -- References "account" ("party"),
	"ip" inet NOT NULL,
	"action" audit_action NOT NULL
) WITH (OIDS = FALSE);
COMMENT ON TABLE "audit" IS 'Logs of all activities on the site, including access and modifications to any data. Each table has an associated audit table inheriting from this one.';

----------------------------------------------------------- users

CREATE TABLE "party" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"orcid" char(16)
);
COMMENT ON TABLE "party" IS 'Users, groups, organizations, and other logical identities';

-- special party (SERIAL starts at 1):
INSERT INTO "party" VALUES (-1, 'Everybody'); -- NOBODY
INSERT INTO "party" VALUES (0, 'Databrary'); -- ROOT

CREATE TABLE "audit_party" (
	LIKE "party"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "account" (
	"id" integer NOT NULL Primary Key References "party",
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"password" varchar(60), -- standard unix-style hash, currently $2a$ bcrypt
	"openid" varchar(256) -- split out (multiple/user)?
);
COMMENT ON TABLE "account" IS 'Login information for parties associated with registered individuals.';

CREATE TABLE "audit_account" (
	LIKE "account"
) INHERITS ("audit") WITH (OIDS = FALSE);

----------------------------------------------------------- permissions

CREATE TYPE permission AS ENUM ('NONE',
	'VIEW', -- study view, but no access to protected data (PUBLIC access)
	'DOWNLOAD', -- full read access to shared data (BROWSE access)
	'CONTRIBUTE', -- create and edit studies of own/target (FULL access)
	'ADMIN' -- perform administrative tasks on site/target such as changing permissions
);
COMMENT ON TYPE permission IS 'Levels of access parties can have to the site, studies, and assets.';

CREATE TYPE consent AS ENUM (
	-- 		permission required (on study)
	'PRIVATE', 	-- CONTRIBUTE	did not consent to any sharing
	'SHARED', 	-- DOWNLOAD	consented to share on databrary
	'EXCERPTS', 	-- DOWNLOAD	SHARED, but consented that excerpts may be PUBLIC
	'PUBLIC' 	-- VIEW		consented to share openly
);
COMMENT ON TYPE consent IS 'Levels of sharing that participants may consent to.';

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "party" ON DELETE Cascade,
	"parent" integer NOT NULL References "party",
	"access" permission NOT NULL DEFAULT 'NONE',
	"delegate" permission NOT NULL DEFAULT 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND ("child" > 0 OR "parent" = -1))
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between parties';
COMMENT ON COLUMN "authorize"."child" IS 'Party granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Party granting permissions';
COMMENT ON COLUMN "authorize"."access" IS 'Level of independent site access granted to child (effectively minimum level on path to ROOT)';
COMMENT ON COLUMN "authorize"."delegate" IS 'Permissions for which child may act as parent (not inherited)';

CREATE TABLE "audit_authorize" (
	LIKE "authorize"
) INHERITS ("audit") WITH (OIDS = FALSE);

-- To allow normal users to inherit from nobody:
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (0, -1, 'ADMIN', 'ADMIN');

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE FUNCTION "authorize_access_parents" (IN "child" integer, OUT "parent" integer, INOUT "access" permission = NULL) RETURNS SETOF RECORD LANGUAGE sql STABLE AS $$
	WITH RECURSIVE closure AS (
		SELECT $1 AS parent, enum_last(null::permission) AS access
		UNION
		SELECT p.parent, LEAST(p.access, c.access)
			FROM authorize_valid p, closure c
			WHERE p.child = c.parent AND ($2 IS NULL OR p.access >= $2)
	)
	SELECT * FROM closure
$$;
COMMENT ON FUNCTION "authorize_access_parents" (integer, permission) IS 'All ancestors (recursive) of a given child';

CREATE FUNCTION "authorize_access_check" ("child" integer, "parent" integer = 0, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT max(access) FROM authorize_access_parents($1, $3) WHERE parent = $2
$$;
COMMENT ON FUNCTION "authorize_access_check" (integer, integer, permission) IS 'Test if a given child inherits the given permission [any] from the given parent [root]';

CREATE FUNCTION "authorize_delegate_check" ("child" integer, "parent" integer, "delegate" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(delegate)) ELSE max(delegate) END FROM authorize_valid WHERE child = $1 AND parent = $2
$$;
COMMENT ON FUNCTION "authorize_delegate_check" (integer, integer, permission) IS 'Test if a given child has the given permission [any] over the given parent';

----------------------------------------------------------- containers

SELECT create_abstract_parent('container', ARRAY['study','slot']);
COMMENT ON TABLE "container" IS 'Parent table for anything assets can be attached to.';


CREATE TABLE "study" (
	"id" integer NOT NULL DEFAULT nextval('container_id_seq') Primary Key References "container" Deferrable Initially Deferred,
	"title" text NOT NULL,
	"description" text
);
COMMENT ON TABLE "study" IS 'Basic organizational unit for data.';
CREATE TRIGGER "container" BEFORE INSERT OR UPDATE OR DELETE ON "study" FOR EACH ROW EXECUTE PROCEDURE "container_trigger" ();

CREATE TABLE "audit_study" (
	LIKE "study"
) INHERITS ("audit") WITH (OIDS = FALSE);
CREATE INDEX "study_creation_idx" ON audit_study (id) WHERE action = 'add';
COMMENT ON INDEX "study_creation_idx" IS 'Allow efficient retrieval of study creation information, specifically date.';

CREATE TABLE "study_access" (
	"study" integer NOT NULL References "study",
	"party" integer NOT NULL References "party",
	"access" permission NOT NULL DEFAULT 'NONE',
	"inherit" permission NOT NULL DEFAULT 'NONE' Check ("inherit" < 'ADMIN'),
	Check ("access" >= "inherit"),
	Primary Key ("study", "party")
);
COMMENT ON TABLE "study_access" IS 'Permissions over studies assigned to users.';

CREATE TABLE "audit_study_access" (
	LIKE "study_access"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE FUNCTION "study_access_check" ("study" integer, "party" integer, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH sa AS (
		SELECT party, access, inherit
		  FROM study_access 
		 WHERE study = $1 AND ($3 IS NULL OR access >= $3)
	)
	SELECT max(access) FROM (
		SELECT access 
		  FROM sa
		 WHERE party = $2
	UNION ALL
		SELECT LEAST(sa.inherit, aap.access) 
		  FROM sa JOIN authorize_access_parents($2, $3) aap ON party = parent 
	UNION ALL
		SELECT LEAST(sa.access, ad.delegate)
		  FROM sa JOIN authorize_valid ad ON party = parent 
		 WHERE child = $2
	) a WHERE $3 IS NULL OR access >= $3
$$;
COMMENT ON FUNCTION "study_access_check" (integer, integer, permission) IS 'Test if a given party has the given permission [any] on the given study, either directly, inherited through site access, or delegated.';


CREATE TABLE "slot" (
	"id" integer NOT NULL DEFAULT nextval('container_id_seq') Primary Key References "container" Deferrable Initially Deferred,
	"study" integer NOT NULL References "study",
	"consent" consent,
	"date" date
);
COMMENT ON TABLE "slot" IS 'Organizational unit within study containing raw data, usually corresponding to an individual data acqusition (single visit/participant/group/sample).';
CREATE INDEX ON "slot" ("study");
CREATE TRIGGER "container" BEFORE INSERT OR UPDATE OR DELETE ON "slot" FOR EACH ROW EXECUTE PROCEDURE "container_trigger" ();

CREATE TABLE "audit_slot" (
	LIKE "slot"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE VIEW "containers" AS
	SELECT container.id, kind, study.id AS "study", title, description, slot.id AS "slot" FROM container 
		LEFT JOIN slot USING (id)
		     JOIN study ON study.id = container.id OR study.id = slot.study;
COMMENT ON VIEW "containers" IS 'All containers (studies and slots) in expanded form.';

CREATE FUNCTION "container_study" ("container" integer) RETURNS integer LANGUAGE sql STABLE STRICT AS $$
	SELECT id FROM study WHERE id = $1 UNION ALL SELECT study FROM slot WHERE id = $1
$$;

----------------------------------------------------------- assets

CREATE TYPE classification AS ENUM (
	'IDENTIFIED', 	-- data containing HIPPA identifiers, requiring appropriate consent and DOWNLOAD permission
	'EXCERPT', 	-- IDENTIFIED data that has been selected as a releasable excerpt
	'DEIDENTIFIED', -- "raw" data which has been de-identified, requiring only DOWNLOAD permission
	'ANALYSIS', 	-- un/de-identified derived, generated, summarized, or aggregated data measures
	'PRODUCT',	-- research products such as results, summaries, commentaries, discussions, manuscripts, or articles
	'MATERIAL'	-- materials not derived from data, such as proposals, procedures, stimuli, manuals, (blank) forms, or documentation
);

CREATE FUNCTION "interval_mi_epoch" (interval, interval) RETURNS double precision LANGUAGE sql IMMUTABLE STRICT AS 
	$$ SELECT date_part('epoch', interval_mi($1, $2)) $$;
CREATE TYPE segment AS RANGE (
	SUBTYPE = interval HOUR TO SECOND,
	SUBTYPE_DIFF = "interval_mi_epoch"
);
COMMENT ON TYPE "segment" IS 'Intervals of time, used primarily for representing clips of timeseries data.';

CREATE FUNCTION "duration" (segment) RETURNS interval HOUR TO SECOND LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT CASE WHEN isempty($1) THEN '0' ELSE interval_mi(upper($1), lower($1)) $$;
COMMENT ON FUNCTION "duration" (segment) IS 'Determine the length of a segment, or NULL if unbounded.';
CREATE FUNCTION "singleton" (segment) RETURNS interval LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT lower($1) WHERE lower_inc($1) AND upper_inc($1) AND lower($1) = upper($1) $$;
COMMENT ON FUNCTION "singleton" (segment) IS 'Determine if a segment represents a single point and return it, or NULL if not.';

CREATE TABLE "format" (
	"id" smallserial NOT NULL Primary Key,
	"mimetype" varchar(128) NOT NULL Unique,
	"extension" varchar(8),
	"name" text NOT NULL
);
COMMENT ON TABLE "format" IS 'Possible types for assets, sufficient for producing download headers.';

CREATE TABLE "timeseries_format" (
	Primary Key ("id"),
	Unique ("mimetype")
) INHERITS ("format");
COMMENT ON TABLE "timeseries_format" IS 'Special asset types that correspond to internal formats representing timeseries data.';

-- The privledged formats with special handling (image and video for now) have hard-coded IDs:
INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-1, 'image/jpeg', 'jpg', 'JPEG');
INSERT INTO "timeseries_format" ("id", "mimetype", "extension", "name") VALUES (-2, 'video/mp4', 'mp4', 'Databrary video');

-- The above video format will change to reflect internal storage, these are used for uploaded files:
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/plain', 'txt', 'Plain text');
-- INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/html', 'html', 'Hypertext markup');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/pdf', 'pdf', 'Portable document');
-- INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mp4', 'mp4', 'MPEG-4 Part 14');
-- INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/webm', 'webm', 'WebM');

SELECT create_abstract_parent('asset', ARRAY['file', 'timeseries', 'clip']);
COMMENT ON TABLE "asset" IS 'Parent table for all uploaded data in storage.';

CREATE TABLE "file" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
	"format" smallint NOT NULL References "format",
	"classification" classification NOT NULL
	-- "date" date
);
COMMENT ON TABLE "file" IS 'Assets in storage along with their "constant" metadata.';
CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "file" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();

CREATE TABLE "audit_file" (
	LIKE "file"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "timeseries" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
	"format" smallint NOT NULL References "timeseries_format",
	"duration" interval HOUR TO SECOND NOT NULL Check ("duration" > interval '0')
) INHERITS ("file");
CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "timeseries" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();

CREATE TABLE "audit_timeseries" (
	LIKE "timeseries"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "clip" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
	"source" integer NOT NULL References "timeseries",
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	"excerpt" boolean NOT NULL Default 'f'
);
CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "clip" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();
ALTER TABLE "clip" ALTER COLUMN "segment" SET STORAGE plain;
CREATE INDEX ON "clip" ("source");
COMMENT ON TABLE "clip" IS 'Sections of timeseries assets selected for referencing.';


CREATE TABLE "asset_link" (
	"container" integer NOT NULL References "container",
	"asset" integer NOT NULL References "asset",
	"title" text NOT NULL,
	"description" text,
	Primary Key ("container", "asset")
);
CREATE INDEX ON "asset_link" ("asset");
COMMENT ON TABLE "asset_link" IS 'Asset linkages into containers along with "dynamic" metadata.';

CREATE TABLE "audit_asset_link" (
	LIKE "asset_link"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE VIEW "asset_nesting" ("child", "parent") AS 
	SELECT id, id FROM file UNION ALL SELECT id, source FROM clip UNION ALL
	SELECT cc.id, cp.id FROM clip cc JOIN clip cp ON cc.source = cp.source AND cc.segment <@ cp.segment;

CREATE FUNCTION "asset_parents" ("file" integer, "segment" segment = NULL) RETURNS SETOF integer LANGUAGE sql STABLE AS 
	$$ SELECT $1 UNION ALL SELECT id FROM clip WHERE source = $1 AND segment @> $2 $$;
COMMENT ON FUNCTION "asset_parents" (integer, segment) IS 'Set of assets that provide the specified asset.  Note that the contains relation here is rather liberal, as we are depending on researchers and/or other parts of the system to ensure than overlapping consents don''t disagree.';

CREATE FUNCTION "asset_consent" ("file" integer, "segment" segment = NULL) RETURNS consent LANGUAGE sql STABLE AS $$
	SELECT MIN(consent) FROM asset_parents($1, $2) JOIN asset_link ON (asset_parents = asset) JOIN slot ON (container = slot.id)
$$;
COMMENT ON FUNCTION "asset_consent" (integer, segment) IS 'Effective (minimal) consent level granted on the specified asset.';

----------------------------------------------------------- annotations

SELECT create_abstract_parent('annotation', ARRAY['comment','tag','record']);
COMMENT ON TABLE "annotation" IS 'Parent table for metadata annotations.';


CREATE TABLE "comment" (
	"id" integer NOT NULL DEFAULT nextval('annotation_id_seq') Primary Key References "annotation" Deferrable Initially Deferred,
	"who" integer NOT NULL References "account",
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"text" text NOT NULL
);
CREATE TRIGGER "annotation" BEFORE INSERT OR UPDATE OR DELETE ON "comment" FOR EACH ROW EXECUTE PROCEDURE "annotation_trigger" ();
COMMENT ON TABLE "comment" IS 'Free-text comments that can be added to nodes (unaudited, immutable).';


CREATE TABLE "record_class" (
	"id" smallserial Primary Key,
	"name" varchar(64) NOT NULL Unique
);
COMMENT ON TABLE "record_class" IS 'Types of records that are relevant for data organization.';
INSERT INTO "record_class" ("name") VALUES ('participant');

CREATE TABLE "record" (
	"id" integer NOT NULL DEFAULT nextval('annotation_id_seq') Primary Key References "annotation" Deferrable Initially Deferred,
	"class" smallint References "record_class"
);
CREATE TRIGGER "annotation" BEFORE INSERT OR UPDATE OR DELETE ON "record" FOR EACH ROW EXECUTE PROCEDURE "annotation_trigger" ();
COMMENT ON TABLE "record" IS 'Sets of metadata measurements organized into or applying to a single cohesive unit.  These belong to the object(s) they''re attached to, which are expected to be within a single study.';

CREATE TYPE data_type AS ENUM ('text', 'number', 'date');
COMMENT ON TYPE data_type IS 'Types of measurement data corresponding to measure_* tables.';

CREATE TABLE "metric" (
	"id" serial Primary Key,
	"name" varchar(64) NOT NULL,
	"classification" classification NOT NULL Default 'DEIDENTIFIED',
	"type" data_type NOT NULL,
	"values" text[] -- options for text enumerations, not enforced (could be pulled out to separate kind/table)
);
COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.  Rough prototype.';
INSERT INTO "metric" ("name", "type") VALUES ('ident', 'text');
INSERT INTO "metric" ("name", "classification", "type") VALUES ('birthday', 'IDENTIFIED', 'date');
INSERT INTO "metric" ("name", "type", "values") VALUES ('gender', 'text', ARRAY['F','M']);

CREATE TABLE "measure" ( -- ABSTRACT
	"record" integer NOT NULL References "record",
	"metric" integer NOT NULL References "metric", -- WHERE kind = table_name
	Primary Key ("record", "metric"),
	Check ('f') NO INHERIT
);
COMMENT ON TABLE "measure" IS 'Abstract parent of all measure tables containing data values.  Rough prototype.';

CREATE TABLE "measure_text" (
	"record" integer NOT NULL References "record",
	"metric" integer NOT NULL References "metric", -- WHERE kind = "text"
	"datum" text NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_number" (
	"record" integer NOT NULL References "record",
	"metric" integer NOT NULL References "metric", -- WHERE kind = "number"
	"datum" numeric NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_date" (
	"record" integer NOT NULL References "record",
	"metric" integer NOT NULL References "metric", -- WHERE kind = "date"
	"datum" date NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE VIEW "measure_view" AS
	SELECT record, metric, datum FROM measure_text UNION ALL
	SELECT record, metric, text(datum) FROM measure_number UNION ALL
	SELECT record, metric, text(datum) FROM measure_date;
COMMENT ON VIEW "measure_view" IS 'Data from all measure tables, coerced to text.';

CREATE TABLE "audit_measure" (
	LIKE "measure",
	"datum" text NOT NULL
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "container_annotation" (
	"annotation" integer NOT NULL References "annotation",
	"container" integer NOT NULL References "container",
	Primary Key ("annotation", "container")
);
COMMENT ON TABLE "container_annotation" IS 'Attachment of annotations to containers.';

CREATE TABLE "audit_container_annotation" (
	LIKE "container_annotation"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "asset_annotation" (
	"annotation" integer NOT NULL References "annotation",
	"asset" integer NOT NULL References "asset",
	Primary Key ("annotation", "asset")
);
COMMENT ON TABLE "asset_annotation" IS 'Attachment of annotations to assets.';

CREATE TABLE "audit_asset_annotation" (
	LIKE "asset_annotation"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE FUNCTION "asset_annotations" ("asset" integer) RETURNS SETOF integer LANGUAGE sql STABLE STRICT AS $$
	SELECT annotation FROM asset_nesting JOIN asset_annotation ON child = asset WHERE parent = $1
$$;

CREATE FUNCTION "container_annotations" ("container" integer) RETURNS SETOF integer LANGUAGE sql STABLE STRICT AS $$
	WITH containers (container) AS (SELECT $1 UNION ALL SELECT id AS container FROM slot WHERE study = $1)
	SELECT annotation FROM containers JOIN container_annotation USING (container) UNION
	SELECT annotation FROM containers JOIN asset_link USING (container) JOIN asset_nesting ON parent = asset JOIN asset_annotation ON child = asset_annotation.asset
$$;


# --- !Downs
;

DROP FUNCTION "container_annotations" (integer);
DROP FUNCTION "asset_annotations" (integer);
DROP TABLE "asset_annotation";
DROP TABLE "container_annotation";
DROP TABLE "comment";
DROP TABLE "measure" CASCADE;
DROP TABLE "metric";
DROP TYPE data_type;
DROP TABLE "record";
DROP TABLE "record_class";
DROP TABLE "annotation";
DROP FUNCTION "annotation_trigger" ();
DROP TYPE "annotation_kind";

DROP FUNCTION "asset_consent" (integer, segment);
DROP FUNCTION "asset_parents" (integer, segment);
DROP VIEW "asset_nesting";
DROP TABLE "asset_link";
DROP TABLE "clip";
DROP TABLE "timeseries";
DROP TABLE "file";
DROP TABLE "timeseries_format";
DROP TABLE "format";
DROP TABLE "asset";
DROP FUNCTION "asset_trigger" ();
DROP TYPE "asset_kind";
DROP FUNCTION "singleton" (segment);
DROP FUNCTION "duration" (segment);
DROP TYPE segment;
DROP FUNCTION "interval_mi_epoch" (interval, interval);
DROP TYPE classification;

DROP FUNCTION "container_study" (integer);
DROP VIEW "containers";
DROP TABLE "slot";
DROP FUNCTION "study_access_check" (integer, integer, permission);
DROP TABLE "study_access";
DROP TABLE "study";
DROP TABLE "container";
DROP FUNCTION "container_trigger" ();
DROP TYPE "container_kind";

DROP FUNCTION "authorize_delegate_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_parents" (integer, permission);
DROP VIEW "authorize_valid";
DROP TABLE "authorize";
DROP TYPE consent;
DROP TYPE permission;
DROP TABLE "account";
DROP TABLE "party";

DROP TABLE "audit" CASCADE;
DROP TYPE audit_action;

DROP FUNCTION cast_int (text);
DROP FUNCTION create_abstract_parent (name, name[]);
