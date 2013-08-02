-- This is currently the complete, authoritative schema, as it is easier to
-- understand all in one place and unnecessary to use proper evolutions until
-- production.  Play only checks for changes in the most recent evolution, so
-- adding an empty 2.sql will prevent it from automatically applying changes to
-- this file, if desired.

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
BEGIN
	EXECUTE $macro$
		CREATE TABLE $macro$ || parent_table || $macro$ ( -- ABSTRACT
			"id" serial NOT NULL Primary Key,
			"kind" name NOT NULL Check ("kind" IN ('$macro$ || array_to_string(children, $$','$$) || $macro$'))
		);;
		CREATE FUNCTION $macro$ || quote_ident(parent || '_trigger') || $macro$ () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
			IF TG_OP = 'INSERT' THEN
				INSERT INTO $macro$ || parent_table || $macro$ (id, kind) VALUES (NEW.id, TG_TABLE_NAME);;
			ELSIF TG_OP = 'DELETE' THEN
				DELETE FROM $macro$ || parent_table || $macro$ WHERE id = OLD.id AND kind = TG_TABLE_NAME;;
			ELSIF TG_OP = 'UPDATE' THEN
				IF NEW.id = OLD.id THEN
					RETURN NEW;;
				END IF;;
				UPDATE $macro$ || parent_table || $macro$ SET id = NEW.id WHERE id = OLD.id AND kind = TG_TABLE_NAME;;
			END IF;;
			IF NOT FOUND THEN
				RAISE EXCEPTION 'inconsistency for %:% parent $macro$ || parent || $macro$', TG_TABLE_NAME, OLD.id;;
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

----------------------------------------------------------- auditing

CREATE TYPE audit_action AS ENUM ('login', 'logout', 'add', 'change', 'remove', 'download');
COMMENT ON TYPE audit_action IS 'The various activities for which we keep audit records (in audit or a derived table).';

CREATE TABLE "audit" (
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"who" int NOT NULL, -- References "account" ("entity"),
	"ip" inet NOT NULL,
	"action" audit_action NOT NULL
) WITH (OIDS = FALSE);
COMMENT ON TABLE "audit" IS 'Logs of all activities on the site, including access and modifications to any data. Each table has an associated audit table inheriting from this one.';

----------------------------------------------------------- users

CREATE TABLE "entity" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"orcid" char(16)
);
COMMENT ON TABLE "entity" IS 'Users, groups, organizations, and other logical identities';

-- special entity (SERIAL starts at 1):
INSERT INTO "entity" VALUES (-1, 'Everybody'); -- NOBODY
INSERT INTO "entity" VALUES (0, 'Databrary'); -- ROOT

CREATE TABLE "audit_entity" (
	LIKE "entity"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "account" (
	"id" integer NOT NULL Primary Key References "entity",
	"username" varchar(32) NOT NULL Unique,
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"openid" varchar(256) -- split out (multiple/user)?
);
COMMENT ON TABLE "account" IS 'Login information for entities associated with registered individuals.';

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

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "entity" ON DELETE Cascade,
	"parent" integer NOT NULL References "entity",
	"access" permission NOT NULL DEFAULT 'NONE',
	"delegate" permission NOT NULL DEFAULT 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND ("child" > 0 OR "parent" = -1))
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between entities';
COMMENT ON COLUMN "authorize"."child" IS 'Entity granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Entity granting permissions';
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
COMMENT ON TABLE "container" IS 'Parent table for anything objects can be attached to.';


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

CREATE TABLE "study_access" (
	"study" integer NOT NULL References "study",
	"entity" integer NOT NULL References "entity",
	"access" permission NOT NULL DEFAULT 'NONE',
	"inherit" permission NOT NULL DEFAULT 'NONE' Check ("inherit" < 'ADMIN'),
	Check ("access" >= "inherit"),
	Primary Key ("study", "entity")
);
COMMENT ON TABLE "study_access" IS 'Permissions over studies assigned to users.';

CREATE TABLE "audit_study_access" (
	LIKE "study_access"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE FUNCTION "study_access_check" ("study" integer, "entity" integer, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH sa AS (
		SELECT entity, access, inherit
		  FROM study_access 
		 WHERE study = $1 AND ($3 IS NULL OR access >= $3)
	)
	SELECT max(access) FROM (
		SELECT access 
		  FROM sa
		 WHERE entity = $2
	UNION ALL
		SELECT LEAST(sa.inherit, aap.access) 
		  FROM sa JOIN authorize_access_parents($2, $3) aap ON entity = parent 
	UNION ALL
		SELECT LEAST(sa.access, ad.delegate)
		  FROM sa JOIN authorize_valid ad ON entity = parent 
		 WHERE child = $2
	) a WHERE $3 IS NULL OR access >= $3
$$;
COMMENT ON FUNCTION "study_access_check" (integer, integer, permission) IS 'Test if a given entity has the given permission [any] on the given study, either directly, inherited through site access, or delegated.';


CREATE TABLE "slot" (
	"id" integer NOT NULL DEFAULT nextval('container_id_seq') Primary Key References "container" Deferrable Initially Deferred,
	"study" integer NOT NULL References "study",
	"ident" varchar(16) NOT NULL,
	Unique ("id", "study"), -- for FKs
	Unique ("study", "ident")
);
COMMENT ON TABLE "slot" IS 'Data container: organizational unit within study, usually corresponding to an individual participant.';
CREATE TRIGGER "container" BEFORE INSERT OR UPDATE OR DELETE ON "slot" FOR EACH ROW EXECUTE PROCEDURE "container_trigger" ();

CREATE TABLE "audit_slot" (
	LIKE "slot"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE FUNCTION "cast_int" ("input" text) RETURNS integer LANGUAGE plpgsql IMMUTABLE STRICT AS $$
DECLARE
	i integer;;
BEGIN
	SELECT input::integer INTO i;;
	RETURN i;;
EXCEPTION WHEN invalid_text_representation THEN
	RETURN NULL;;
END;; $$;

CREATE FUNCTION "next_slot_ident" ("study" integer) RETURNS varchar(16) LANGUAGE sql STABLE STRICT AS $$
	SELECT (GREATEST(max(cast_int(ident)), count(ident))+1)::varchar(16) FROM slot WHERE study = $1
$$;


CREATE VIEW "containers" AS
	SELECT container.id, kind, study.id AS "study", title, description, slot.id AS "slot", ident FROM
		container LEFT JOIN slot USING (id) 
		JOIN study ON study.id = container.id OR study.id = slot.study;
COMMENT ON VIEW "containers" IS 'All containers (studies and slots) in expanded form.';

CREATE FUNCTION "container_study" ("container" integer) RETURNS integer LANGUAGE sql STABLE STRICT AS $$
	SELECT id FROM study WHERE id = container UNION SELECT study FROM slot WHERE id = container
$$;

----------------------------------------------------------- objects

CREATE TYPE consent AS ENUM (
	-- required permission:	site		study
	'PUBLIC', 	-- 	-		VIEW		non-subject data
	'DEIDENTIFIED', -- 	-		VIEW		subject data without identifiers
	'EXCERPTS', 	-- 	DOWNLOAD	DOWNLOAD	SHARED, but consented that excerpts may be PUBLIC
	'SHARED', 	-- 	DOWNLOAD	DOWNLOAD	identified data authorized to be shared on databrary
	'PRIVATE' 	-- 	-		CONTRIBUTE	identified data not authorized for sharing
);
COMMENT ON TYPE consent IS 'Sensitivity levels that may apply to data according to the presence of protected identifiers and granted sharing level.  Does not necessarily map clearly to permission levels.';

SELECT create_abstract_parent('object', ARRAY['file', 'timeseries', 'excerpt']);
COMMENT ON TABLE "object" IS 'Parent table for all uploaded data in storage.';

CREATE TABLE "format" (
	"id" smallserial NOT NULL Primary Key,
	"mimetype" varchar(128) NOT NULL Unique,
	"extension" varchar(8),
	"name" text NOT NULL
);
COMMENT ON TABLE "format" IS 'Possible types for objects, sufficient for producing download headers. Abstract parent of file_format and timeseries_format.';

CREATE TABLE "file_format" (
	Primary Key ("id"),
	Unique ("mimetype")
) INHERITS ("format");
INSERT INTO "file_format" (mimetype, extension, name) VALUES ('text/plain', 'txt', 'Plain text');
INSERT INTO "file_format" (mimetype, extension, name) VALUES ('text/html', 'html', 'Hypertext markup');
INSERT INTO "file_format" (mimetype, extension, name) VALUES ('application/pdf', 'pdf', 'Portable document');
INSERT INTO "file_format" (mimetype, extension, name) VALUES ('image/jpeg', 'jpg', 'JPEG');

CREATE TABLE "file" (
	"id" integer NOT NULL DEFAULT nextval('object_id_seq') Primary Key References "object" Deferrable Initially Deferred,
	"format" smallint NOT NULL References "file_format",
	"owner" integer References "study" ON DELETE SET NULL,
	"consent" consent NOT NULL,
	"date" date
);
COMMENT ON TABLE "file" IS 'Objects in storage along with their "constant" metadata.';
CREATE TRIGGER "object" BEFORE INSERT OR UPDATE OR DELETE ON "file" FOR EACH ROW EXECUTE PROCEDURE "object_trigger" ();

CREATE TABLE "audit_file" (
	LIKE "file"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "timeseries_format" (
	Primary Key ("id"),
	Unique ("mimetype")
) INHERITS ("format");

CREATE TABLE "timeseries" (
	"id" integer NOT NULL DEFAULT nextval('object_id_seq') Primary Key References "object" Deferrable Initially Deferred,
	"format" smallint NOT NULL References "timeseries_format",
	"length" interval HOUR TO SECOND NOT NULL
) INHERITS ("file");
CREATE TRIGGER "object" BEFORE INSERT OR UPDATE OR DELETE ON "timeseries" FOR EACH ROW EXECUTE PROCEDURE "object_trigger" ();

CREATE TABLE "audit_timeseries" (
	LIKE "timeseries"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "excerpt" (
	"id" integer NOT NULL DEFAULT nextval('object_id_seq') Primary Key References "object" Deferrable Initially Deferred,
	"source" integer NOT NULL References "timeseries",
	"offset" interval HOUR TO SECOND NOT NULL,
	"length" interval HOUR TO SECOND,
	"public" boolean NOT NULL Default 'f' -- only if object.consent = EXCERPTS
);
CREATE TRIGGER "object" BEFORE INSERT OR UPDATE OR DELETE ON "excerpt" FOR EACH ROW EXECUTE PROCEDURE "object_trigger" ();
COMMENT ON TABLE "excerpt" IS 'Sections of timeseries objects selected for referencing.';

CREATE TABLE "audit_excerpt" (
	LIKE "excerpt"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "object_link" (
	"container" integer NOT NULL References "container",
	"object" integer NOT NULL References "object",
	"title" text NOT NULL,
	"description" text,
	Primary Key ("container", "object")
);
COMMENT ON TABLE "object_link" IS 'Object linkages into containers along with "dynamic" metadata.';

CREATE TABLE "audit_object_link" (
	LIKE "object_link"
) INHERITS ("audit") WITH (OIDS = FALSE);

----------------------------------------------------------- annotations

CREATE TABLE "annotation" ( -- ABSTRACT
	"id" serial NOT NULL Primary Key,
	"who" integer NOT NULL References "entity",
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	-- consider possible ON DELETE actions:
	"container" integer NOT NULL References "container",
	"object" integer References "object",
	Foreign Key ("container", "object") References "object_link"
);
COMMENT ON TABLE "annotation" IS 'Abstract base table for various types of annotations that can be added by users to nodes (unaudited, no updates).';

CREATE TABLE "comment" (
	"text" text NOT NULL,
	Primary Key ("id"),
	Foreign Key ("container", "object") References "object_link"
) INHERITS ("annotation");
COMMENT ON TABLE "comment" IS 'Free-text comments.';

# --- !Downs
;

DROP TABLE "comment";
DROP TABLE "annotation";
DROP TABLE "audit_object_link";
DROP TABLE "object_link";
DROP TABLE "audit_excerpt";
DROP TABLE "excerpt";
DROP TABLE "audit_timeseries";
DROP TABLE "timeseries";
DROP TABLE "timeseries_format";
DROP TABLE "audit_file";
DROP TABLE "file";
DROP TABLE "file_format";
DROP TABLE "format";
DROP TABLE "object";
DROP FUNCTION "object_trigger" ();
DROP TYPE consent;

DROP TABLE "audit_slot";
DROP TABLE "slot";
DROP FUNCTION "study_access_check" (integer, integer, permission);
DROP TABLE "audit_study_access";
DROP TABLE "study_access";
DROP TABLE "audit_study";
DROP TABLE "study";
DROP TABLE "container";
DROP FUNCTION "container_trigger" ();

DROP FUNCTION "authorize_delegate_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_parents" (integer, permission);
DROP VIEW "authorize_valid";
DROP TABLE "audit_authorize";
DROP TABLE "authorize";
DROP TYPE permission;
DROP TABLE "audit_account";
DROP TABLE "account";
DROP TABLE "audit_entity";
DROP TABLE "entity";

DROP TABLE "audit";
DROP TYPE audit_action;

DROP FUNCTION create_abstract_parent (name, name[]);
