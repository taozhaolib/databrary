# --- !Ups

INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mp4', 'mp4', 'MPEG-4 Part 14 video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/webm', 'webm', 'WebM video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mpeg', 'mpg', 'MPEG program stream (MPEG-1/MPEG-2 video)');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/quicktime', 'mov', 'QuickTime video');

DROP VIEW "asset_duration";

ALTER TABLE "file" ADD "superseded" integer References "asset";
COMMENT ON COLUMN "file"."superseded" IS 'Newer version of this asset, either generated automatically from reformatting or a replacement provided by the user.';

ALTER TABLE audit."file" ADD "superseded" integer;
CREATE INDEX ON "file" ("superseded") WHERE "superseded" IS NOT NULL;
ALTER TABLE "timeseries" RENAME "duration" TO "duration_old";
ALTER TABLE "timeseries" ADD Foreign Key ("superseded") References "asset",
			 DROP CONSTRAINT "timeseries_duration_check",
			 ADD "duration" interval HOUR TO SECOND Check ("duration" > interval '0');
UPDATE timeseries SET duration = duration_old;
ALTER TABLE "timeseries" ALTER "duration" SET NOT NULL,
			 DROP "duration_old";

DROP VIEW audit."timeseries";
CREATE VIEW audit."timeseries" AS
	SELECT *, NULL::interval AS "duration" FROM audit."file";
COMMENT ON VIEW audit."timeseries" IS 'Timeseries are audited together with files.  This view provides glue to make that transparent.';
CREATE OR REPLACE FUNCTION audit."timeseries_file" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO audit."file" (audit_time, audit_user, audit_ip, audit_action, id, format, classification, superseded) VALUES (NEW.audit_time, NEW.audit_user, NEW.audit_ip, NEW.audit_action, NEW.id, NEW.format, NEW.classification, NEW.superseded);;
	RETURN NEW;;
END;; $$;
COMMENT ON FUNCTION audit."timeseries_file" () IS 'Trigger function for INSTEAD OF INSERT ON audit.timeseries to propagate to audit.file.';
CREATE TRIGGER "file" INSTEAD OF INSERT ON audit."timeseries" FOR EACH ROW EXECUTE PROCEDURE audit."timeseries_file" ();

CREATE SCHEMA ingest;
CREATE TABLE ingest."asset" (
	"id" integer Primary Key References "asset",
	"file" text NOT NULL
);

# --- !Downs

DROP TABLE ingest."asset";
DROP SCHEMA ingest;

DROP VIEW audit."timeseries";
CREATE VIEW audit."timeseries" AS
	SELECT audit_time, audit_user, audit_ip, audit_action, id, format, classification, NULL::interval AS "duration" FROM audit."file";
COMMENT ON VIEW audit."timeseries" IS 'Timeseries are audited together with files.  This view provides glue to make that transparent.';
CREATE OR REPLACE FUNCTION audit."timeseries_file" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO audit."file" (audit_time, audit_user, audit_ip, audit_action, id, format, classification) VALUES (NEW.audit_time, NEW.audit_user, NEW.audit_ip, NEW.audit_action, NEW.id, NEW.format, NEW.classification);;
	RETURN NEW;;
END;; $$;
COMMENT ON FUNCTION audit."timeseries_file" () IS 'Trigger function for INSTEAD OF INSERT ON audit.timeseries to propagate to audit.file.';
CREATE TRIGGER "file" INSTEAD OF INSERT ON audit."timeseries" FOR EACH ROW EXECUTE PROCEDURE audit."timeseries_file" ();

ALTER TABLE "audit"."file" DROP "superseded";
ALTER TABLE "file" DROP "superseded";

CREATE VIEW "asset_duration" ("id", "duration") AS
	SELECT id, NULL FROM ONLY file UNION ALL
	SELECT id, duration FROM timeseries UNION ALL
	SELECT id, duration(segment) FROM clip;
COMMENT ON VIEW "asset_duration" IS 'All assets along with their temporal durations, NULL for non-timeseries.';

DELETE FROM ONLY "format" WHERE "mimetype" IN ('video/mp4', 'video/webm', 'video/mpeg', 'video/quicktime');
SELECT setval('format_id_seq', max(id), true) FROM format;

