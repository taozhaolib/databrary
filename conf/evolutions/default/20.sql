# --- !Ups

-- this should've been done in the previous evolution
ALTER TABLE audit."asset" ADD "sha1" bytea;

UPDATE "asset" SET sha1 = '\x3dda3931202cbe06a9e4bbb5f0873c879121ef0a' WHERE id = 1 and sha1 IS NULL;
ALTER TABLE "asset" ALTER "sha1" SET NOT NULL;

# --- !Downs

ALTER TABLE "asset" ALTER "sha1" DROP NOT NULL;
ALTER TABLE audit."asset" DROP "sha1";
