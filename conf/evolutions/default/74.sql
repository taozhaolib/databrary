# --- !Ups

ALTER TABLE "party"
	ALTER "name" SET STORAGE EXTERNAL,
	ALTER "affiliation" SET STORAGE EXTERNAL;
ALTER TABLE "account"
	ALTER "email" SET STORAGE EXTERNAL,
	ALTER "password" SET STORAGE EXTERNAL;
ALTER TABLE "volume"
	ALTER "name" SET STORAGE EXTERNAL,
	ALTER "alias" SET STORAGE EXTERNAL;
ALTER TABLE "container"
	ALTER "name" SET STORAGE EXTERNAL;
ALTER TABLE "format"
	ALTER "mimetype" SET STORAGE EXTERNAL,
	ALTER "extension" SET STORAGE EXTERNAL;
ALTER TABLE "asset"
	ALTER "name" SET STORAGE EXTERNAL,
	ALTER "sha1" DROP NOT NULL,
	ALTER "sha1" SET STORAGE EXTERNAL,
	ADD "size" bigint Check ("size" >= 0);
ALTER TABLE audit."asset"
	ADD "size" bigint;
ALTER TABLE "excerpt"
	ALTER "segment" SET STORAGE PLAIN;
ALTER TABLE "tag"
	ALTER "name" SET STORAGE EXTERNAL;
ALTER TABLE "record_category"
	ALTER "name" SET STORAGE EXTERNAL;
ALTER TABLE "metric"
	ALTER "name" SET STORAGE EXTERNAL;
ALTER TABLE "token"
	ALTER "token" SET STORAGE EXTERNAL;

ALTER TABLE "transcode"
	ALTER "asset" DROP DEFAULT,
	ADD Foreign Key ("asset") References "asset" ON DELETE CASCADE;

ALTER TABLE ingest."asset"
	DROP CONSTRAINT "asset_pkey",
	ADD Unique ("id"),
	DROP CONSTRAINT "asset_id_fkey",
	ADD Foreign Key ("id") References "asset" ON DELETE CASCADE,
	ADD Unique ("file");
ALTER TABLE ingest."asset"
	ALTER "file" SET STORAGE EXTERNAL;
CREATE TABLE ingest."container" (
	"id" integer NOT NULL Unique References "container" ON DELETE CASCADE,
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE,
	"key" text NOT NULL,
	Unique ("volume", "key")
);
ALTER TABLE ingest."container"
	ALTER "key" SET STORAGE EXTERNAL;
CREATE TABLE ingest."record" (
	"id" integer NOT NULL Unique References "record" ON DELETE CASCADE,
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE,
	"key" text NOT NULL,
	Unique ("volume", "key")
);
ALTER TABLE ingest."record"
	ALTER "key" SET STORAGE EXTERNAL;

# --- !Downs

ALTER TABLE "transcode"
	DROP CONSTRAINT "transcode_asset_fkey",
	ALTER "asset" SET DEFAULT nextval('asset_id_seq');
	
DELETE FROM "asset" WHERE sha1 IS NULL;

ALTER TABLE "party"
	ALTER "name" SET STORAGE EXTENDED,
	ALTER "affiliation" SET STORAGE EXTENDED;
ALTER TABLE "account"
	ALTER "email" SET STORAGE EXTENDED,
	ALTER "password" SET STORAGE EXTENDED;
ALTER TABLE "volume"
	ALTER "name" SET STORAGE EXTENDED,
	ALTER "alias" SET STORAGE EXTENDED;
ALTER TABLE "container"
	ALTER "name" SET STORAGE EXTENDED;
ALTER TABLE "format"
	ALTER "mimetype" SET STORAGE EXTENDED,
	ALTER "extension" SET STORAGE EXTENDED;
ALTER TABLE "asset"
	ALTER "name" SET STORAGE EXTENDED,
	ALTER "sha1" SET NOT NULL,
	ALTER "sha1" SET STORAGE EXTENDED,
	DROP "size";
ALTER TABLE audit."asset"
	DROP "size";
ALTER TABLE "excerpt"
	ALTER "segment" SET STORAGE EXTENDED;
ALTER TABLE "tag"
	ALTER "name" SET STORAGE EXTENDED;
ALTER TABLE "record_category"
	ALTER "name" SET STORAGE EXTENDED;
ALTER TABLE "metric"
	ALTER "name" SET STORAGE EXTENDED;
ALTER TABLE "token"
	ALTER "token" SET STORAGE EXTENDED;
ALTER TABLE ingest."asset"
	ALTER "file" SET STORAGE EXTENDED;

ALTER TABLE ingest."asset"
	DROP CONSTRAINT "asset_id_key",
	ADD Primary Key ("id"),
	DROP CONSTRAINT "asset_id_fkey",
	ADD Foreign Key ("id") References "asset",
	DROP CONSTRAINT "asset_file_key";
DROP TABLE ingest."container", ingest."record";
