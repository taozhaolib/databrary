# --- !Ups

CREATE FUNCTION "get_slot" ("container" integer, "seg" segment) RETURNS integer STRICT LANGUAGE plpgsql AS $$
DECLARE
	slot_id integer;;
BEGIN
	LOOP
		SELECT id INTO slot_id FROM slot WHERE source = container AND segment = seg;;
		IF FOUND THEN
			RETURN slot_id;;
		END IF;;
		BEGIN
			INSERT INTO slot (source, segment) VALUES (container, seg) RETURNING id INTO slot_id;;
			RETURN slot_id;;
		EXCEPTION WHEN unique_violation THEN
		END;;
	END LOOP;;
END;; $$;

ALTER TABLE "asset" RENAME TO "asset_old";
ALTER TABLE "asset_old" RENAME CONSTRAINT "asset_pkey" TO "asset_old_pkey";

CREATE TABLE "asset" (
	"id" integer Primary Key Default nextval('asset_id_seq'),
	"volume" integer NOT NULL References "volume",
	"format" smallint NOT NULL References "format",
	"classification" classification NOT NULL,
	"duration" interval HOUR TO SECOND Check ("duration" > interval '0'),
	"name" text NOT NULL,
	"body" text
);
COMMENT ON TABLE "asset" IS 'Assets reflecting files in primary storage.';
ALTER SEQUENCE "asset_id_seq" OWNED BY "asset"."id";

ALTER TABLE "file" DROP CONSTRAINT "file_format_fkey";
UPDATE "file" SET format = -800 WHERE format = (SELECT id FROM ONLY format WHERE mimetype = 'video/mp4');
UPDATE ONLY "format" SET "id" = -800, "name" = 'MPEG-4 video' WHERE mimetype = 'video/mp4';

INSERT INTO "asset"
	SELECT asset.id, container.volume, file.format, file.classification, timeseries.duration, COALESCE(container_asset.name, audit_container_asset.name), COALESCE(container_asset.body, audit_container_asset.body)
	  FROM asset_old asset
	  LEFT JOIN file LEFT JOIN timeseries ON file.id = timeseries.id ON asset.id = file.id 
	  LEFT JOIN file superseded_file ON file.superseded = superseded_file.id
	  LEFT JOIN container_asset ON asset.id = container_asset.asset OR superseded_file.id = container_asset.asset
          LEFT JOIN audit.container_asset audit_container_asset ON asset.id = audit_container_asset.asset AND audit_container_asset.audit_action = 'remove'
	  LEFT JOIN container ON container_asset.container = container.id OR audit_container_asset.container = container.id;

CREATE TABLE "asset_slot" (
	"asset" integer NOT NULL Primary Key References "asset",
	"slot" integer NOT NULL References "slot"
);
COMMENT ON TABLE "asset_slot" IS 'Attachment point of assets, which, in the case of timeseries data, should match asset.duration.';

INSERT INTO "asset_slot"
	SELECT asset, get_slot(container, segment(position, position + COALESCE(duration, interval '0'), CASE WHEN duration IS NULL THEN '[]' ELSE '[)' END))
	  FROM container_asset 
	  LEFT JOIN timeseries ON container_asset.asset = timeseries.id;

CREATE TABLE "asset_revision" (
	"prev" integer NOT NULL References "asset",
	"next" integer NOT NULL References "asset",
	Primary Key ("next", "prev")
);
COMMENT ON TABLE "asset_revision" IS 'Assets that reflect different versions of the same content, either generated automatically from reformatting or a replacement provided by the user.';

INSERT INTO "asset_revision"
	SELECT id, superseded FROM file
	 WHERE superseded IS NOT NULL;

CREATE TABLE "excerpt" (
 	"asset" integer NOT NULL References "asset",
	"slot" integer NOT NULL References "slot",
	Primary Key ("asset", "slot")
);
COMMENT ON TABLE "excerpt" IS 'Slot asset (segments) that have been selected for possible public release and top-level display.';

INSERT INTO "excerpt"
	SELECT asset, slot FROM toplevel_asset WHERE excerpt;

SELECT audit.CREATE_TABLE ('asset');

CREATE INDEX "asset_creation_idx" ON audit."asset" ("id") WHERE "audit_action" = 'add';
COMMENT ON INDEX audit."asset_creation_idx" IS 'Allow efficient retrieval of asset creation information, specifically date.';
CREATE FUNCTION "asset_creation" ("asset" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."asset" WHERE "id" = $1 AND "audit_action" = 'add' $$;

SELECT audit.CREATE_TABLE ('asset_slot');

SELECT audit.CREATE_TABLE ('excerpt');

INSERT INTO audit."asset"
	SELECT a.audit_time, a.audit_user, a.audit_ip, a.audit_action, a.id, container.volume, a.format, a.classification, NULL, c.name, c.body
	  FROM audit.file a
	  LEFT JOIN audit.container_asset c
	    ON c.audit_time - a.audit_time < '1 minute'
	   AND a.audit_user = c.audit_user
	   AND a.audit_ip = c.audit_ip
	   AND a.audit_action = c.audit_action
	   AND a.id = c.asset
	  LEFT JOIN container ON c.container = container.id
	 WHERE a.audit_action = 'add';

INSERT INTO audit."asset"
	SELECT c.audit_time, c.audit_user, c.audit_ip, c.audit_action, c.asset, container.volume, a.format, a.classification, NULL, c.name, c.body
	  FROM audit.container_asset c
	  LEFT JOIN asset a ON c.asset = a.id
	  LEFT JOIN container ON c.container = container.id
	 WHERE audit_action NOT IN ('add', 'remove');

INSERT INTO audit."asset_slot"
	SELECT c.audit_time, c.audit_user, c.audit_ip, c.audit_action, c.asset, slot.id
	  FROM audit.container_asset c
	  LEFT JOIN slot ON c.container = slot.source AND CASE WHEN c.position IS NULL THEN slot.segment = '(,)' ELSE lower(slot.segment) = c.position END
	 WHERE audit_action = 'remove';

INSERT INTO audit."excerpt"
	SELECT audit_time, audit_user, audit_ip, audit_action, asset, slot
	  FROM audit.toplevel_asset WHERE excerpt;

ALTER TABLE ingest."asset" DROP CONSTRAINT "asset_id_fkey";
ALTER TABLE ingest."asset" ADD Foreign Key ("id") References "asset";

DROP VIEW audit."timeseries";
DROP FUNCTION audit."timeseries_file" ();
DROP TABLE audit."file";
DROP TABLE audit."container_asset";
DROP TABLE audit."toplevel_asset";

DROP FUNCTION "file_modification" (integer);
DROP TABLE "toplevel_asset";
DROP TABLE "container_asset";
DROP TABLE "clip";
DROP TABLE "timeseries";
DROP TABLE "timeseries_format";
DROP TABLE "file";
DROP TABLE "asset_old";
DROP TYPE "asset_kind";
DROP FUNCTION "asset_trigger" ();
 
CREATE VIEW "slot_asset" ("asset", "segment", "slot", "excerpt") AS
	SELECT asset_slot.asset, slot_asset.segment, slot.id, excerpt.asset IS NOT NULL
	  FROM asset_slot 
	  JOIN slot AS slot_asset ON asset_slot.slot = slot_asset.id
	  JOIN slot ON slot_asset.source = slot.source AND slot_asset.segment && slot.segment
	  LEFT JOIN excerpt
	       JOIN slot AS slot_excerpt ON excerpt.slot = slot_excerpt.id
	       ON asset_slot.asset = excerpt.asset AND slot.source = slot_excerpt.source AND slot.segment <@ slot_excerpt.segment;
COMMENT ON VIEW "slot_asset" IS 'Expanded set of all slots and the assets they include.';

# --- !Downs

DROP VIEW "slot_asset";

ALTER TABLE "asset" RENAME TO "asset_new";
ALTER TABLE "asset_new" RENAME CONSTRAINT "asset_pkey" TO "asset_new_pkey";

DROP FUNCTION "get_slot" (integer, segment);


CREATE TABLE "timeseries_format" (
	Primary Key ("id"),
	Unique ("mimetype")
) INHERITS ("format");
COMMENT ON TABLE "timeseries_format" IS 'Special asset types that correspond to internal formats representing timeseries data.';

ALTER TABLE "asset_new" DROP CONSTRAINT "asset_format_fkey";
UPDATE ONLY "format" SET "id" = (SELECT id - 1 FROM format WHERE mimetype = 'video/webm'), "name" = 'MPEG-4 Part 14 video' WHERE id = -800;
UPDATE "asset_new" SET format = (SELECT id FROM ONLY format WHERE mimetype = 'video/mp4') WHERE format = -800 AND duration IS NULL;
INSERT INTO "timeseries_format" ("id", "mimetype", "extension", "name") VALUES (-800, 'video/mp4', 'mp4', 'Databrary video');

ALTER SEQUENCE "asset_id_seq" RENAME TO "asset_new_id_seq";
SELECT CREATE_ABSTRACT_PARENT ('asset', ARRAY['file', 'timeseries', 'clip']);
COMMENT ON TABLE "asset" IS 'Parent table for all uploaded data in storage.';
SELECT setval('asset_id_seq', nextval('asset_new_id_seq')-1, 't');

CREATE TABLE "file" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
 	"format" smallint NOT NULL References "format",
 	"classification" classification NOT NULL,
	"superseded" integer References "asset"
);

CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "file" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();
CREATE INDEX ON "file" ("superseded") WHERE "superseded" IS NOT NULL;
COMMENT ON TABLE "file" IS 'Assets in storage along with their "constant" metadata.';
COMMENT ON COLUMN "file"."superseded" IS 'Newer version of this asset, either generated automatically from reformatting or a replacement provided by the user.';

SELECT audit.CREATE_TABLE ('file');
CREATE INDEX "file_modification_idx" ON audit."file" ("id") WHERE "audit_action" IN ('add', 'change');
COMMENT ON INDEX audit."file_modification_idx" IS 'Allow efficient retrieval of file modification information, specifically date.';

CREATE FUNCTION "file_modification" ("file" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."file" WHERE "id" = $1 AND "audit_action" IN ('add', 'change') $$;

CREATE TABLE "timeseries" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
	"format" smallint NOT NULL References "timeseries_format",
	"duration" interval HOUR TO SECOND NOT NULL Check ("duration" > interval '0')
) INHERITS ("file");
ALTER TABLE "timeseries" ADD Foreign Key ("superseded") References "asset";
CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "timeseries" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();
COMMENT ON TABLE "timeseries" IS 'File assets representing interpretable and sub-selectable timeseries data (e.g., videos).';

CREATE VIEW audit."timeseries" AS
	SELECT *, NULL::interval AS "duration" FROM audit."file";
COMMENT ON VIEW audit."timeseries" IS 'Timeseries are audited together with files.  This view provides glue to make that transparent.';
CREATE FUNCTION audit."timeseries_file" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO audit."file" (audit_time, audit_user, audit_ip, audit_action, id, format, classification, superseded) VALUES (NEW.audit_time, NEW.audit_user, NEW.audit_ip, NEW.audit_action, NEW.id, NEW.format, NEW.classification, NEW.superseded);
	RETURN NEW;
END; $$;
COMMENT ON FUNCTION audit."timeseries_file" () IS 'Trigger function for INSTEAD OF INSERT ON audit.timeseries to propagate to audit.file.';
CREATE TRIGGER "file" INSTEAD OF INSERT ON audit."timeseries" FOR EACH ROW EXECUTE PROCEDURE audit."timeseries_file" ();

CREATE TABLE "clip" (
	"id" integer NOT NULL DEFAULT nextval('asset_id_seq') Primary Key References "asset" Deferrable Initially Deferred,
	"source" integer NOT NULL References "timeseries",
	"segment" segment NOT NULL Check (lower("segment") >= '0'::interval AND NOT upper_inf("segment")), -- segment <@ [0,source.duration]
	Unique ("source", "segment")
) INHERITS ("object_segment");
CREATE TRIGGER "asset" BEFORE INSERT OR UPDATE OR DELETE ON "clip" FOR EACH ROW EXECUTE PROCEDURE "asset_trigger" ();
CREATE INDEX ON "clip" ("source");
COMMENT ON TABLE "clip" IS 'Sections of timeseries assets selected for use.  When placed into containers, they are treated independently of their source timeseries.';

CREATE TABLE "container_asset" (
	"asset" integer NOT NULL References "asset" Primary Key,
	"container" integer NOT NULL References "container",
	"position" interval HOUR TO SECOND,
	"name" text NOT NULL,
	"body" text
);
CREATE INDEX ON "container_asset" ("container");
COMMENT ON TABLE "container_asset" IS 'Asset linkages into containers along with "dynamic" metadata.';
COMMENT ON COLUMN "container_asset"."position" IS 'Start point or position of this asset within the container, such that this asset occurs or starts position time after the beginning of the container session.  NULL positions are treated as universal (existing at all times).';

SELECT audit.CREATE_TABLE ('container_asset');

CREATE TABLE "toplevel_asset" (
	"slot" integer NOT NULL References "slot",
 	"asset" integer NOT NULL References "asset",
	"excerpt" boolean NOT NULL DEFAULT false,
	Primary Key ("slot", "asset")
);
COMMENT ON TABLE "toplevel_asset" IS 'Slot assets which are promoted to the top volume level for display.';
COMMENT ON COLUMN "toplevel_asset"."excerpt" IS 'Asset segments that may be released publically if so permitted.';

SELECT audit.CREATE_TABLE ('toplevel_asset');

INSERT INTO "file" (id, format, classification, superseded)
	SELECT id, format, classification, next
	  FROM asset_new asset LEFT JOIN asset_revision ON asset.id = asset_revision.prev
	 WHERE duration IS NULL;
INSERT INTO "timeseries" (id, format, classification, superseded, duration)
	SELECT id, format, classification, next, duration
	  FROM asset_new asset LEFT JOIN asset_revision ON asset.id = asset_revision.prev
	 WHERE duration IS NOT NULL;

INSERT INTO "container_asset" (asset, container, position, name, body)
	SELECT asset.id, slot.source, lower(slot.segment), asset.name, asset.body
	  FROM asset_new asset
	  JOIN asset_slot ON asset.id = asset_slot.asset
	  JOIN slot ON asset_slot.slot = slot.id;

INSERT INTO "toplevel_asset"
	SELECT slot, asset, true FROM excerpt;

INSERT INTO audit."file"
	SELECT audit_time, audit_user, audit_ip, audit_action, id, format, classification
	  FROM audit.asset;

-- XXX: audit.container_asset

INSERT INTO audit."toplevel_asset"
	SELECT audit_time, audit_user, audit_ip, audit_action, slot, asset, true
	  FROM audit.excerpt;

ALTER TABLE "asset_slot" DROP CONSTRAINT "asset_slot_slot_fkey";
DELETE FROM slot USING asset_slot WHERE slot.id = asset_slot.slot AND segment <> '(,)' AND consent IS NULL;
SELECT setval('slot_id_seq', max(id), 't') FROM slot;

ALTER TABLE ingest."asset" DROP CONSTRAINT "asset_id_fkey";
ALTER TABLE ingest."asset" ADD Foreign Key ("id") References "asset";

DROP TABLE audit."excerpt";
DROP TABLE audit."asset_slot";
DROP FUNCTION "asset_creation" (integer);
DROP TABLE audit."asset";

DROP TABLE "excerpt";
DROP TABLE "asset_slot";
DROP TABLE "asset_revision";
DROP TABLE "asset_new";
