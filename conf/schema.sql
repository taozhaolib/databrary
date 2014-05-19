-- This is currently the complete, authoritative schema, as it is easier to
-- understand all in one place.

-- Whenever you make changes to this file, you must also write a new evolution
-- in evolutions/default and check the result using "project/runsql check".
-- Note that this while this file is valid SQL, evolutions require semi-colons
-- to be doubled when they do not terminate statements.

-- A general convention is that hard-coded fixtures get non-positive ids.

----------------------------------------------------------- utilities

CREATE FUNCTION cast_int ("input" text) RETURNS integer LANGUAGE plpgsql IMMUTABLE STRICT AS $$
DECLARE
	i integer;
BEGIN
	SELECT input::integer INTO i;
	RETURN i;
EXCEPTION WHEN invalid_text_representation THEN
	RETURN NULL;
END; $$;

CREATE FUNCTION singleton (int4) RETURNS int4range LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT int4range($1, $1, '[]') $$;

----------------------------------------------------------- auditing

CREATE SCHEMA audit;

CREATE TYPE audit_action AS ENUM ('attempt', 'open', 'close', 'add', 'change', 'remove', 'superuser');
COMMENT ON TYPE audit_action IS 'The various activities for which we keep audit records (in audit or a derived table).  This is not kept in the audit schema due to jdbc type search limitations.';

CREATE FUNCTION audit.SET_PRIVILEGES (name) RETURNS void LANGUAGE plpgsql AS $set$ BEGIN
	EXECUTE $$REVOKE UPDATE, DELETE, TRUNCATE ON TABLE audit.$$ || quote_ident($1) || $$ FROM $$ || quote_ident(current_user);
END; $set$;
COMMENT ON FUNCTION audit.SET_PRIVILEGES (name) IS 'REVOKE UPDATE, DELETE, TRUNCATE ON TABLE audit.$1 FROM current_user.  Unfortunately you cannot remove default privileges per-schema, so we do this instead for each audit table.  The function is necessary to interpolate current_user.';

CREATE TABLE audit."audit" (
	"audit_time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"audit_user" int NOT NULL, -- References "account" ("party"),
	"audit_ip" inet NOT NULL,
	"audit_action" audit_action NOT NULL
) WITH (OIDS = FALSE);
COMMENT ON TABLE audit."audit" IS 'Logs of all activities on the site, including access and modifications to any data. Each table has an associated audit table inheriting from this one.';
SELECT audit.SET_PRIVILEGES ('audit');
CREATE INDEX "audit_login_idx" ON audit."audit" ("audit_user", "audit_time") WHERE "audit_action" IN ('attempt', 'open');
COMMENT ON INDEX audit."audit_login_idx" IS 'Allow efficient determination of recent login attempts for security.';

CREATE FUNCTION audit.CREATE_TABLE (name) RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	table_name CONSTANT text := quote_ident($1);
BEGIN
	EXECUTE $$CREATE TABLE audit.$$ || table_name || $$ (LIKE public.$$ || table_name || $$) INHERITS (audit."audit") WITH (OIDS = FALSE)$$;
	PERFORM audit.SET_PRIVILEGES($1);
END; $create$;
COMMENT ON FUNCTION audit.CREATE_TABLE (name) IS 'Create an audit.$1 table mirroring public.$1.';

----------------------------------------------------------- users

CREATE TABLE "party" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"orcid" char(16),
	"affiliation" text,
	"duns" numeric(9)
);
COMMENT ON TABLE "party" IS 'Users, groups, organizations, and other logical identities';
COMMENT ON COLUMN "party"."orcid" IS 'http://en.wikipedia.org/wiki/ORCID';
COMMENT ON COLUMN "party"."duns" IS 'http://en.wikipedia.org/wiki/DUNS';

-- special parties (SERIAL starts at 1):
INSERT INTO "party" VALUES (-1, 'Everybody'); -- NOBODY
INSERT INTO "party" VALUES (0, 'Databrary'); -- ROOT

SELECT audit.CREATE_TABLE ('party');


CREATE TABLE "account" (
	"id" integer NOT NULL Primary Key References "party",
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"password" varchar(60), -- standard unix-style hash, currently $2a$ bcrypt
	"openid" varchar(256) -- split out (multiple/user)?
);
COMMENT ON TABLE "account" IS 'Login information for parties associated with registered individuals.';

SELECT audit.CREATE_TABLE ('account');

----------------------------------------------------------- permissions

CREATE TYPE permission AS ENUM ('NONE',
	'VIEW', -- list view, but no access to protected data (PUBLIC access)
	'DOWNLOAD', -- full read access to shared data (BROWSE access)
	'CONTRIBUTE', -- create and edit data of own/target (FULL access)
	'ADMIN' -- perform administrative tasks on site/target such as changing permissions
);
COMMENT ON TYPE permission IS 'Levels of access parties can have to the site data.';

CREATE TYPE consent AS ENUM (
	-- 		permission required
	'PRIVATE', 	-- CONTRIBUTE	did not consent to any sharing
	'SHARED', 	-- DOWNLOAD	consented to share on databrary
	'EXCERPTS', 	-- DOWNLOAD	SHARED, but consented that excerpts may be PUBLIC
	'PUBLIC' 	-- VIEW		consented to share openly
);
COMMENT ON TYPE consent IS 'Levels of sharing that participants may consent to.';

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "party" ON DELETE CASCADE,
	"parent" integer NOT NULL References "party",
	"inherit" permission NOT NULL DEFAULT 'NONE',
	"direct" permission NOT NULL DEFAULT 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND "child" > 0)
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between parties';
COMMENT ON COLUMN "authorize"."child" IS 'Party granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Party granting permissions';
COMMENT ON COLUMN "authorize"."inherit" IS 'Level of site/group access granted to child, inherited (but degraded) from parent';
COMMENT ON COLUMN "authorize"."direct" IS 'Permissions that child is granted directly on parent''s data';

SELECT audit.CREATE_TABLE ('authorize');

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize WHERE authorized IS NOT NULL
		UNION
		SELECT a.child, aa.parent, LEAST(a.inherit,
			CASE WHEN aa.child <= 0 THEN aa.inherit
			     WHEN aa.inherit = 'ADMIN' THEN 'CONTRIBUTE'
			     WHEN aa.inherit = 'CONTRIBUTE' THEN 'DOWNLOAD'
			     ELSE 'NONE' END), NULL, GREATEST(a.authorized, aa.authorized), LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent WHERE a.authorized IS NOT NULL
	) SELECT * FROM aa
	UNION ALL SELECT id, id, enum_last(NULL::permission), enum_last(NULL::permission), NULL, NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, enum_last(NULL::permission), NULL, NULL, NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE FUNCTION "authorize_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "authorize_inherit";
	RETURN null;
END; $$;
CREATE TRIGGER "authorize_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "authorize" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();
CREATE TRIGGER "party_created" AFTER INSERT ON "party" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized <= CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE VIEW "authorize_view" ("child", "parent", "inherit", "direct") AS
	SELECT child, parent, MAX(inherit), MAX(direct)
	  FROM authorize_inherit
	 WHERE (authorized IS NULL OR authorized <= CURRENT_TIMESTAMP) AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';


CREATE TABLE "authorize_info" (
	"child" integer NOT NULL References "party" ON DELETE CASCADE,
	"parent" integer NOT NULL References "party",
	"info" text NOT NULL,
	Primary Key ("parent", "child"),
	Foreign Key ("parent", "child") References "authorize" ON DELETE CASCADE
);
COMMENT ON TABLE "authorize_info" IS 'Additional information provided with authorization requests for internal staff stuff.  Temporary.';


----------------------------------------------------------- volumes

CREATE TABLE "volume" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"body" text,
	"alias" varchar(64)
);
COMMENT ON TABLE "volume" IS 'Basic organizational unit for data.';
COMMENT ON COLUMN "volume"."alias" IS 'Short, internal, code name for this volume, for contributors to reference their own data.';

SELECT audit.CREATE_TABLE ('volume');
CREATE INDEX "volume_creation_idx" ON audit."volume" ("id") WHERE "audit_action" = 'add';
COMMENT ON INDEX audit."volume_creation_idx" IS 'Allow efficient retrieval of volume creation information, specifically date.';

CREATE FUNCTION "volume_creation" ("volume" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."volume" WHERE "id" = $1 AND "audit_action" = 'add' $$;

CREATE TABLE "volume_access" (
	"volume" integer NOT NULL References "volume",
	"party" integer NOT NULL References "party",
	"access" permission NOT NULL DEFAULT 'NONE',
	"inherit" permission NOT NULL DEFAULT 'NONE' Check ("inherit" < 'ADMIN'),
	"funding" text,
	Check ("access" >= "inherit"),
	Primary Key ("volume", "party")
);
COMMENT ON TABLE "volume_access" IS 'Permissions over volumes assigned to users.';

SELECT audit.CREATE_TABLE ('volume_access');

CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH v AS (
		SELECT party, access, inherit
		  FROM volume_access 
		 WHERE volume = $1
	)
	SELECT access FROM (
		SELECT access 
		  FROM v
		 WHERE party = $2
	UNION ALL
		SELECT MAX(GREATEST(LEAST(v.access, a.direct), LEAST(v.inherit, a.inherit)))
		  FROM v JOIN authorize_view a ON party = parent
		 WHERE child = $2
	) a LIMIT 1
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer) IS 'Permission level the party has on the given volume, either directly, delegated, or inherited.';

CREATE TABLE "volume_citation" (
	"volume" integer NOT NULL References "volume",
	"head" text NOT NULL,
	"url" text,
	"body" text,
	"study" boolean NOT NULL Default false
);
CREATE INDEX ON "volume_citation" ("volume");
CREATE UNIQUE INDEX ON "volume_citation" ("volume") WHERE "study";
COMMENT ON TABLE "volume_citation" IS 'Quick and dirty citation list.  Not intended to be permanent.  No PK: only updated in bulk on volume.';
COMMENT ON COLUMN "volume_citation"."study" IS 'Primary external citation associated with each volume, in the case of studies, supplementals, excerpts, or other volumes directly attached to publications.';

----------------------------------------------------------- time intervals

CREATE FUNCTION "interval_mi_epoch" (interval, interval) RETURNS double precision LANGUAGE sql IMMUTABLE STRICT AS 
	$$ SELECT date_part('epoch', interval_mi($1, $2)) $$;
CREATE TYPE segment AS RANGE (
	SUBTYPE = interval HOUR TO SECOND (3),
	SUBTYPE_DIFF = "interval_mi_epoch"
);
COMMENT ON TYPE "segment" IS 'Intervals of time, used primarily for representing clips of timeseries data.';

CREATE FUNCTION "segment" (interval) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT segment('0', $1) $$;
COMMENT ON FUNCTION "segment" (interval) IS 'The segment [0,X) but strict in X.';
CREATE FUNCTION "duration" (segment) RETURNS interval HOUR TO SECOND (3) LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT CASE WHEN isempty($1) THEN '0' ELSE interval_mi(upper($1), lower($1)) END $$;
COMMENT ON FUNCTION "duration" (segment) IS 'Determine the length of a segment, or NULL if unbounded.';
CREATE FUNCTION "singleton" (interval HOUR TO SECOND (3)) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT segment($1, $1, '[]') $$;
CREATE FUNCTION "singleton" (segment) RETURNS interval LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT lower($1) WHERE lower_inc($1) AND upper_inc($1) AND lower($1) = upper($1) $$;
COMMENT ON FUNCTION "singleton" (segment) IS 'Determine if a segment represents a single point and return it, or NULL if not.';

CREATE FUNCTION "segment_shift" (segment, interval) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE WHEN isempty($1) THEN 'empty' ELSE
		segment(lower($1) + $2, upper($1) + $2,
			CASE WHEN lower_inc($1) THEN '[' ELSE '(' END || CASE WHEN upper_inc($1) THEN ']' ELSE ')' END)
	END
$$;
COMMENT ON FUNCTION "segment_shift" (segment, interval) IS 'Shift both end points of a segment by the specified interval.';

-- this probably needs an order by segment for most uses:
CREATE AGGREGATE "segment_union" (segment) (SFUNC = range_union, STYPE = segment, INITCOND = 'empty');
CREATE AGGREGATE "segment_intersect" (segment) (SFUNC = range_intersect, STYPE = segment, INITCOND = '(,)');


----------------------------------------------------------- containers

CREATE TABLE "container" (
	"id" serial NOT NULL Primary Key,
	"volume" integer NOT NULL References "volume",
	"top" boolean NOT NULL DEFAULT false,
	"name" text,
	"date" date
);
CREATE INDEX ON "container" ("volume");
CREATE INDEX "container_top_idx" ON "container" ("volume") WHERE "top";
COMMENT ON TABLE "container" IS 'Organizational unit within volume containing related files (with common annotations), often corresponding to an individual data session (single visit/acquisition/participant/group/day).';

SELECT audit.CREATE_TABLE ('container');

CREATE FUNCTION "container_top_create" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO container (volume, top) VALUES (NEW.id, true);
	RETURN null;
END; $$;
CREATE TRIGGER "container_top_create" AFTER INSERT ON "volume" FOR EACH ROW EXECUTE PROCEDURE "container_top_create" ();
COMMENT ON TRIGGER "container_top_create" ON "volume" IS 'Always create a top container for each volume.  Unfortunately nothing currently prevents them from being removed/changed.';


----------------------------------------------------------- slots

CREATE TABLE "slot" ( -- ABSTRACT
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Check (false) NO INHERIT
);
ALTER TABLE "slot" ALTER COLUMN "segment" SET STORAGE plain;
COMMENT ON TABLE "slot" IS 'Generic table for objects associated with a temporal sub-sections of a container.  Inherit from this table to use the functions below.';


CREATE TABLE "slot_consent" (
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"consent" consent NOT NULL,
	Primary Key ("container", "segment"),
	Exclude USING gist (singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
COMMENT ON TABLE "slot_consent" IS 'Sharing/release permissions granted by participants on (portions of) contained data.';

SELECT audit.CREATE_TABLE ('slot_consent');


----------------------------------------------------------- studies

CREATE TABLE "volume_inclusion" (
	"volume" integer NOT NULL References "volume",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("volume", "container") -- this may be too strong but seems reasonable
) INHERITS ("slot");
COMMENT ON TABLE "volume_inclusion" IS 'Inclusions of slots (sessions) from "dataset" (provider) volumes in "study" (consumer/reuse) volumes.';

SELECT audit.CREATE_TABLE ('volume_inclusion');


----------------------------------------------------------- assets

CREATE TYPE classification AS ENUM (
	'IDENTIFIED', 	-- data containing HIPPA identifiers, requiring appropriate consent and DOWNLOAD permission
	'EXCERPT', 	-- IDENTIFIED data that has been selected as a releasable excerpt
	'DEIDENTIFIED', -- "raw" data which has been de-identified, requiring only DOWNLOAD permission
	'ANALYSIS', 	-- un/de-identified derived, generated, summarized, or aggregated data measures
	'PRODUCT',	-- research products such as results, summaries, commentaries, discussions, manuscripts, or articles
	'MATERIAL'	-- materials not derived from data, such as proposals, procedures, stimuli, manuals, (blank) forms, or documentation
);

CREATE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "excerpt" boolean = false, "top" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE 
		WHEN p > 'DOWNLOAD' OR p < 'VIEW' OR p IS NULL
			THEN p
		WHEN t > 'DEIDENTIFIED' OR p = 'DOWNLOAD' AND t >= CASE
			WHEN c >= 'PUBLIC'
				OR c >= 'SHARED' AND a >= 'DOWNLOAD'
				OR c >= 'EXCERPTS' AND excerpt
				THEN 'IDENTIFIED'
			WHEN c IS NULL AND top
				THEN 'EXCERPT'
			ELSE 	'DEIDENTIFIED'
		END::classification
			THEN 'DOWNLOAD'
		ELSE	'VIEW'
	END
$$;
COMMENT ON FUNCTION "data_permission" (permission, consent, classification, permission, boolean, boolean) IS 'Effective permission level on a data object in a volume with the given permission, in a slot with the given consent, having the given classification, when the user has the given access permission level, when it''s marked as an excerpt, when it''s in a top container.';

CREATE TABLE "format" (
	"id" smallserial NOT NULL Primary Key,
	"mimetype" varchar(128) NOT NULL Unique,
	"extension" varchar(8),
	"name" text NOT NULL
);
COMMENT ON TABLE "format" IS 'Possible types for assets, sufficient for producing download headers.';

-- The privledged formats with special handling (image and video for now) have hard-coded IDs:
INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-700, 'image/jpeg', 'jpg', 'JPEG image');

-- The above video format will change to reflect internal storage, these are used for uploaded files:
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/plain', 'txt', 'Plain text');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/csv', 'csv', 'Comma-separated values');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/html', 'html', 'Hypertext markup');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/rtf', 'rtf', 'Rich text format');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('image/png', 'png', 'Portable network graphics');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/pdf', 'pdf', 'Portable document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/msword', 'doc', 'Microsoft Word document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.text', 'odf', 'OpenDocument text');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.wordprocessingml.document', 'docx', 'Microsoft Word (Office Open XML) document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-excel', 'xls', 'Microsoft Excel spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.spreadsheet', 'ods', 'OpenDocument spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'xlsx', 'Microsoft Excel (Office Open XML) workbook');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-powerpoint', 'ppt', 'Microsoft PowerPoint presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.presentation', 'odp', 'OpenDocument presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.presentationml.presentation', 'pptx', 'Microsoft PowerPoint (Office Open XML) presentation');
SELECT nextval('format_id_seq'); -- placeholder for old video/mp4
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/webm', 'webm', 'WebM video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mpeg', 'mpg', 'MPEG program stream (MPEG-1/MPEG-2 video)');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/quicktime', 'mov', 'QuickTime video');
INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-800, 'video/mp4', 'mp4', 'MPEG-4 video');

CREATE TABLE "asset" (
	"id" serial Primary Key,
	"volume" integer NOT NULL References "volume",
	"format" smallint NOT NULL References "format",
	"classification" classification NOT NULL,
	"duration" interval HOUR TO SECOND (3) Check ("duration" > interval '0'),
	"name" text,
	"sha1" bytea NOT NULL Check (octet_length("sha1") = 20)
);
COMMENT ON TABLE "asset" IS 'Assets reflecting files in primary storage.';

SELECT audit.CREATE_TABLE ('asset');
ALTER TABLE audit."asset" ALTER "sha1" DROP NOT NULL;

CREATE INDEX "asset_creation_idx" ON audit."asset" ("id") WHERE "audit_action" = 'add';
COMMENT ON INDEX audit."asset_creation_idx" IS 'Allow efficient retrieval of asset creation information, specifically date.';
CREATE FUNCTION "asset_creation" ("asset" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."asset" WHERE "id" = $1 AND "audit_action" = 'add' $$;

CREATE TABLE "slot_asset" (
	"asset" integer NOT NULL Primary Key References "asset",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL
) INHERITS ("slot");
CREATE INDEX "slot_asset_slot_idx" ON "slot_asset" ("container", "segment");
COMMENT ON TABLE "slot_asset" IS 'Attachment point of assets, which, in the case of timeseries data, should match asset.duration.';

SELECT audit.CREATE_TABLE ('slot_asset');

CREATE TABLE "asset_revision" (
	"prev" integer NOT NULL References "asset",
	"next" integer NOT NULL References "asset",
	Primary Key ("next", "prev")
);
COMMENT ON TABLE "asset_revision" IS 'Assets that reflect different versions of the same content, either generated automatically from reformatting or a replacement provided by the user.';

CREATE VIEW "asset_revisions" AS
	WITH RECURSIVE r AS (
		SELECT * FROM asset_revision
		UNION ALL
		SELECT asset_revision.prev, r.next FROM asset_revision JOIN r ON asset_revision.next = r.prev
	) SELECT * FROM r;
COMMENT ON VIEW "asset_revisions" IS 'Transitive closure of asset_revision.  Revisions must never form a cycle or this will not terminate.';

CREATE FUNCTION "asset_supersede" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM next FROM asset_revision WHERE prev = asset_new;
	IF FOUND THEN
		RAISE 'Asset % already superseded', asset_new;
	END IF;
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;
	INSERT INTO asset_revision VALUES (asset_old, asset_new);
END; $$;


CREATE TABLE "excerpt" (
	"asset" integer NOT NULL References "slot_asset" ON DELETE CASCADE,
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Primary Key ("asset", "segment"),
	Exclude USING gist (singleton("asset") WITH =, "segment" WITH &&)
);
COMMENT ON TABLE "excerpt" IS 'Slot asset segments that have been selected for possible public release and top-level display.';
COMMENT ON COLUMN "excerpt"."segment" IS 'Segment within slot_asset.container space (not asset).';

SELECT audit.CREATE_TABLE ('excerpt');


CREATE TABLE "transcode" (
	"asset" integer NOT NULL Primary Key References "asset" ON DELETE CASCADE,
	"owner" integer NOT NULL References "party",
	"start" timestamp Default now(),
	"process" integer,
	"result" text
);

----------------------------------------------------------- comments

CREATE TABLE "comment" (
	"id" serial NOT NULL Primary Key,
	"who" integer NOT NULL References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"text" text NOT NULL,
	"parent" integer References "comment"
) INHERITS ("slot");
CREATE INDEX "comment_slot_idx" ON "comment" ("container", "segment");
CREATE INDEX ON "comment" ("who");
CREATE INDEX ON "comment" ("parent");
COMMENT ON TABLE "comment" IS 'Free-text comments on objects (unaudited, immutable).';

CREATE MATERIALIZED VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
CREATE INDEX "comment_thread_slot_idx" ON "comment_thread" ("container", "segment");
CREATE INDEX ON "comment_thread" ("who");
COMMENT ON MATERIALIZED VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

CREATE FUNCTION "comment_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "comment_thread";
	RETURN null;
END; $$;
CREATE TRIGGER "comment_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "comment" FOR EACH STATEMENT EXECUTE PROCEDURE "comment_refresh" ();

----------------------------------------------------------- tags

CREATE TABLE "tag" (
	"id" serial NOT NULL Primary Key,
	"name" varchar(32) NOT NULL Unique
);
COMMENT ON TABLE "tag" IS 'Tag/keywords that can be applied to objects.';

CREATE FUNCTION "get_tag" ("tag_name" varchar(32)) RETURNS integer STRICT LANGUAGE plpgsql AS $$
DECLARE
	tag_id integer;
BEGIN
	LOOP
		SELECT id INTO tag_id FROM tag WHERE name = tag_name;
		IF FOUND THEN
			RETURN tag_id;
		END IF;
		BEGIN
			INSERT INTO tag (name) VALUES (tag_name) RETURNING id INTO tag_id;
			RETURN tag_id;
		EXCEPTION WHEN unique_violation THEN
		END;
	END LOOP;
END; $$;


CREATE TABLE "tag_use" (
	"tag" integer NOT NULL References "tag",
	"who" integer NOT NULL References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"up" boolean NOT NULL DEFAULT true,
	Primary Key ("tag", "who", "container", "segment"),
	Exclude USING gist (singleton("tag") WITH =, singleton("who") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX ON "tag_use" ("who");
CREATE INDEX "tag_use_slot_idx" ON "tag_use" ("container", "segment");
COMMENT ON TABLE "tag_use" IS 'Applications of tags to objects along with their weight (+-1).';


----------------------------------------------------------- records

CREATE TABLE "record_category" (
	"id" smallserial Primary Key,
	"name" varchar(64) NOT NULL Unique
);
COMMENT ON TABLE "record_category" IS 'Types of records that are relevant for data organization.';
INSERT INTO "record_category" ("id", "name") VALUES (-500, 'participant');
INSERT INTO "record_category" ("id", "name") VALUES (-200, 'group');
INSERT INTO "record_category" ("id", "name") VALUES (-800, 'pilot');
INSERT INTO "record_category" ("id", "name") VALUES (-700, 'exclusion');
INSERT INTO "record_category" ("id", "name") VALUES (-400, 'condition');
INSERT INTO "record_category" ("id", "name") VALUES (-100, 'location');
INSERT INTO "record_category" ("id", "name") VALUES (-300, 'task');

CREATE TABLE "record" (
	"id" serial NOT NULL Primary Key,
	"volume" integer NOT NULL References "volume",
	"category" smallint References "record_category" ON UPDATE CASCADE ON DELETE SET NULL
);
CREATE INDEX ON "record" ("volume");
COMMENT ON TABLE "record" IS 'Sets of metadata measurements organized into or applying to a single cohesive unit.  These belong to the object(s) they''re attached to, which are expected to be within a single volume.';

CREATE TYPE data_type AS ENUM ('text', 'number', 'date');
COMMENT ON TYPE data_type IS 'Types of measurement data corresponding to measure_* tables.';

CREATE TABLE "metric" (
	"id" serial Primary Key,
	"name" varchar(64) NOT NULL Unique,
	"classification" classification NOT NULL,
	"type" data_type NOT NULL,
	"options" text[] -- (suggested) options for text enumerations, not enforced
);
COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.';
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-900, 'ident', 'DEIDENTIFIED', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-590, 'birthdate', 'IDENTIFIED', 'date');
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-550, 'race', 'DEIDENTIFIED', 'text', ARRAY['American Indian or Alaska Native','Asian','Native Hawaiian or Other Pacific Islander','Black or African American','White','Multiple']);
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-540, 'ethnicity', 'DEIDENTIFIED', 'text', ARRAY['Not Hispanic or Latino','Hispanic or Latino']);
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-510, 'language', 'DEIDENTIFIED', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-580, 'gender', 'DEIDENTIFIED', 'text', ARRAY['Female','Male']);
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-520, 'disability', 'IDENTIFIED', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-150, 'country', 'DEIDENTIFIED', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-140, 'state', 'DEIDENTIFIED', 'text', ARRAY['AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','MD','MA','MI','MN','MS','MO','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']);
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-90, 'info', 'MATERIAL', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-650, 'summary', 'MATERIAL', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-600, 'description', 'MATERIAL', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-700, 'reason', 'DEIDENTIFIED', 'text', ARRAY['Did not meet inclusion criteria','Procedural/experimenter error','Withdrew/fussy/tired','Outlier']);
INSERT INTO "metric" ("id", "name", "classification", "type", "options") VALUES (-180, 'setting', 'MATERIAL', 'text', ARRAY['Lab','Home','Classroom','Outdoor','Clinic']);

CREATE TABLE "record_template" (
	"category" smallint References "record_category" ON UPDATE CASCADE ON DELETE CASCADE,
	"metric" integer References "metric" ON UPDATE CASCADE ON DELETE CASCADE,
	Primary Key ("category", "metric")
);
COMMENT ON TABLE "record_template" IS 'Default set of measures defining a given record category.';
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -590);
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -580);
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -550);
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -540);
INSERT INTO "record_template" ("category", "metric") VALUES (-200, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-800, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-700, -700);
INSERT INTO "record_template" ("category", "metric") VALUES (-400, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-100, -180);
INSERT INTO "record_template" ("category", "metric") VALUES (-100, -140);
INSERT INTO "record_template" ("category", "metric") VALUES (-300, -600);

CREATE TABLE "measure" ( -- ABSTRACT
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = table_name
	Primary Key ("record", "metric"),
	Check (false) NO INHERIT
);
COMMENT ON TABLE "measure" IS 'Abstract parent of all measure tables containing data values.';

CREATE TABLE "measure_text" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "text"
	"datum" text NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_number" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "number"
	"datum" numeric NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_date" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "date"
	"datum" date NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE VIEW "measure_view" AS
	SELECT record, metric, datum FROM measure_text UNION ALL
	SELECT record, metric, text(datum) FROM measure_number UNION ALL
	SELECT record, metric, text(datum) FROM measure_date;
COMMENT ON VIEW "measure_view" IS 'Data from all measure tables, coerced to text.';

CREATE VIEW "measures" ("record", "measures") AS
	SELECT record, array_agg(metric || ':' || datum ORDER BY metric) FROM measure_view GROUP BY record;
COMMENT ON VIEW "measures" IS 'All measures for each record aggregated into a single array.';

CREATE VIEW "measure_all" ("record", "metric", "datum_text", "datum_number", "datum_date") AS
	SELECT record, metric, datum, NULL::numeric, NULL::date FROM measure_text UNION ALL
	SELECT record, metric, NULL, datum, NULL FROM measure_number UNION ALL
	SELECT record, metric, NULL, NULL, datum FROM measure_date;
COMMENT ON VIEW "measure_all" IS 'Data from all measure tables, coerced to text.';

CREATE TABLE "record_measures" (
	"id" integer NOT NULL Primary Key References "record" ON UPDATE CASCADE ON DELETE CASCADE,
	"volume" integer NOT NULL,
	"category" smallint,
	"measures" text[] NOT NULL Default '{}'
);
CREATE INDEX ON "record_measures" ("volume");
COMMENT ON TABLE "record_measures" IS 'Automatically updated table representing "record JOIN measures".';

-- record_measures triggers
CREATE FUNCTION "record_measures_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	TRUNCATE record_measures;
	INSERT INTO record_measures SELECT id, volume, category, COALESCE(measures, '{}') FROM record LEFT JOIN measures ON id = record;
	RETURN null;
END; $$;

CREATE FUNCTION "record_measures_ri" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO record_measures SELECT NEW.*;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ri" ();

CREATE FUNCTION "record_measures_ru" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET volume = NEW.volume, category = NEW.category WHERE id = NEW.id;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ru" ();

CREATE FUNCTION "record_measures_mi" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = NEW.record;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();

CREATE FUNCTION "record_measures_mu" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id IN (OLD.record, NEW.record);
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();

CREATE FUNCTION "record_measures_md" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = OLD.record;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();

CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_text" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_number" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_date" EXECUTE PROCEDURE "record_measures_refresh" ();


CREATE TABLE "slot_record" (
	"record" integer NOT NULL References "record",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("record", "container", "segment"),
	Exclude USING gist (singleton("record") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX "slot_record_slot_idx" ON "slot_record" ("container", "segment");
COMMENT ON TABLE "slot_record" IS 'Attachment of records to slots.';


-- TODO review this: other places we may effectively enforce the much looser MAX(consent) with &&
CREATE FUNCTION "record_consent" ("record" integer) RETURNS consent LANGUAGE sql STABLE STRICT AS
	$$ SELECT MIN(consent) FROM slot_record JOIN slot_consent ON slot_record.container = slot_consent.container AND slot_record.segment <@ slot_consent.segment WHERE record = $1 $$;
COMMENT ON FUNCTION "record_consent" (integer) IS 'Effective (minimal) consent level granted on the specified record.';

CREATE FUNCTION "record_daterange" ("record" integer) RETURNS daterange LANGUAGE sql STABLE STRICT AS $$
	SELECT daterange(min(date), max(date), '[]') 
	  FROM slot_record
	  JOIN container ON slot_record.container = container.id
	 WHERE record = $1
$$;
COMMENT ON FUNCTION "record_daterange" (integer) IS 'Range of container dates covered by the given record.';

----------------------------------------------------------- tokens

CREATE TABLE "token" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL,
	Check (false) NO INHERIT
);
COMMENT ON TABLE "token" IS 'Generic tokens issued to automatically perform actions such as logins or authorizations.';

CREATE TABLE "account_token" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL,
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	Check (false) NO INHERIT
) INHERITS ("token");
COMMENT ON TABLE "account_token" IS 'Generic tokens associated with particular accounts.';

CREATE TABLE "login_token" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '1 week',
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	"password" boolean NOT NULL DEFAULT false
) INHERITS ("account_token");
CREATE UNIQUE INDEX "login_token_account_idx" ON "login_token" ("account") WHERE "password";
COMMENT ON TABLE "login_token" IS 'Tokens issued to automatically login/register users or reset passwords.';

CREATE TABLE "session" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '4 weeks',
	"account" integer NOT NULL References "account" ON DELETE CASCADE
) INHERITS ("account_token");
COMMENT ON TABLE "session" IS 'Tokens associated with currently logged-in sessions.';

----------------------------------------------------------- avatars

CREATE TABLE "avatar" (
	"party" integer NOT NULL Primary Key References "party" ON DELETE CASCADE,
	"asset" integer NOT NULL References "asset"
);
COMMENT ON TABLE "avatar" IS 'Image assets used to represent parties on the site.  These assets are expected to be in the CORE volume.';

SELECT audit.CREATE_TABLE ('avatar');

----------------------------------------------------------- analytics

CREATE TABLE audit."analytic" (
	"route" text NOT NULL,
	"data" json NOT NULL
) INHERITS (audit."audit");
COMMENT ON TABLE audit."analytic" IS 'Analytics data collected and reported by the browser.';


----------------------------------------------------------- bootstrap/test data

INSERT INTO party (id, name, orcid, affiliation) VALUES (1, 'Dylan Simon', '0000000227931679', 'Databrary');
INSERT INTO party (id, name, affiliation) VALUES (2, 'Mike Continues', 'Databrary');
INSERT INTO party (id, name, affiliation) VALUES (3, 'Lisa Steiger', 'Databrary');
INSERT INTO party (id, name, affiliation) VALUES (4, 'Andrea Byrne', 'Databrary');
INSERT INTO party (id, name, affiliation) VALUES (5, 'Karen Adolph', 'New York University');
INSERT INTO party (id, name, affiliation) VALUES (6, 'Rick Gilmore', 'Penn State University');
SELECT setval('party_id_seq', 6);

INSERT INTO account (id, email, openid) VALUES (1, 'dylan@databrary.org', 'http://dylex.net/');
INSERT INTO account (id, email, openid) VALUES (2, 'mike@databrary.org', NULL);
INSERT INTO account (id, email, openid) VALUES (3, 'lisa@databrary.org', NULL);
INSERT INTO account (id, email, openid) VALUES (4, 'andrea@databrary.org', NULL);

INSERT INTO authorize (child, parent, inherit, direct, authorized) VALUES (1, 0, 'ADMIN', 'ADMIN', '2013-3-1');
INSERT INTO authorize (child, parent, inherit, direct, authorized) VALUES (2, 0, 'ADMIN', 'ADMIN', '2013-8-1');
INSERT INTO authorize (child, parent, inherit, direct, authorized) VALUES (3, 0, 'CONTRIBUTE', 'NONE', '2013-4-1');
INSERT INTO authorize (child, parent, inherit, direct, authorized) VALUES (4, 0, 'CONTRIBUTE', 'NONE', '2013-9-1');

INSERT INTO volume (id, name, body) VALUES (1, 'Databrary', 'Databrary is an open data library for developmental science. Share video, audio, and related metadata. Discover more, faster.
Most developmental scientists rely on video recordings to capture the complexity and richness of behavior. However, researchers rarely share video data, and this has impeded scientific progress. By creating the cyber-infrastructure and community to enable open video sharing, the Databrary project aims to facilitate deeper, richer, and broader understanding of behavior.
The Databrary project is dedicated to transforming the culture of developmental science by building a community of researchers committed to open video data sharing, training a new generation of developmental scientists and empowering them with an unprecedented set of tools for discovery, and raising the profile of behavioral science by bolstering interest in and support for scientific research among the general public.');
SELECT setval('volume_id_seq', 1);

INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, -1, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 2, 'ADMIN', 'NONE');

INSERT INTO asset (id, volume, format, classification, duration, name, sha1) VALUES (1, 1, -800, 'MATERIAL', interval '40', 'counting', '\x3dda3931202cbe06a9e4bbb5f0873c879121ef0a');
INSERT INTO slot_asset VALUES (1, '[0,40)'::segment, 1);
SELECT setval('asset_id_seq', 1);
SELECT setval('container_id_seq', 2);

-- special volumes (SERIAL starts at 1), done after container triggers:
INSERT INTO "volume" (id, name) VALUES (0, 'Core'); -- CORE
INSERT INTO "volume_access" VALUES (0, -1, 'DOWNLOAD', 'DOWNLOAD');


----------------------------------------------------------- ingest logs

CREATE SCHEMA ingest;

CREATE TABLE ingest."asset" (
	"id" integer Primary Key References "asset",
	"file" text NOT NULL
);
