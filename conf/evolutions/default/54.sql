# --- !Ups

ALTER TABLE "volume_citation" ADD "study" boolean NOT NULL Default false;
CREATE UNIQUE INDEX ON "volume_citation" ("volume") WHERE "study";
COMMENT ON COLUMN "volume_citation"."study" IS 'Primary external citation associated with each volume, in the case of studies, supplementals, excerpts, or other volumes directly attached to publications.';

# --- !Downs

ALTER TABLE "volume_citation" DROP "study";
