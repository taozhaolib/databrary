# --- !Ups

INSERT INTO "volume" (id, name) VALUES (0, 'Core'); -- CORE
INSERT INTO "volume_access" VALUES (0, -1, 'DOWNLOAD', 'DOWNLOAD');

CREATE TABLE "avatar" (
       "party" integer NOT NULL Primary Key References "party",
       "asset" integer NOT NULL References "asset"
);
COMMENT ON TABLE "avatar" IS 'Image assets used to represent parties on the site.  These assets are expected to be in the CORE volume.';

SELECT audit.CREATE_TABLE ('avatar');

# --- !Downs

DROP TABLE audit."avatar";
DROP TABLE "avatar";

DELETE FROM "volume_access" WHERE volume = 0;
DELETE FROM "container" WHERE volume = 0;
DELETE FROM "volume" WHERE id = 0;
