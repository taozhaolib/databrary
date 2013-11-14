# --- !Ups

INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mp4', 'mp4', 'MPEG-4 Part 14 video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/webm', 'webm', 'WebM video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mpeg', 'mpg', 'MPEG program stream (MPEG-1/MPEG-2 video)');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/quicktime', 'mov', 'QuickTime video');

ALTER TABLE "file" ADD "superseded" integer References "asset";
COMMENT ON COLUMN "file"."superseded" IS 'Newer version of this asset, either generated automatically from reformatting or a replacement provided by the user.';

ALTER TABLE "timeseries" ADD Foreign Key ("superseded") References "asset";

# --- !Downs

DELETE FROM ONLY "format" WHERE "mimetype" IN ('video/mp4', 'video/webm', 'video/mpeg', 'video/quicktime');

ALTER TABLE "file" DROP "superseded";
