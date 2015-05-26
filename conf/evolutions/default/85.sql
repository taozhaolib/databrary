# --- !Ups

CREATE TABLE "volume_doi" (
	"volume" integer NOT NULL Unique References "volume",
	"doi" varchar(16) NOT NULL Unique
);
COMMENT ON TABLE "volume_doi" IS 'DOIs issued for volumes (currently via EZID).';

INSERT INTO "volume_doi" VALUES (1, '10.17910/B7159Q');

# --- !Downs

DROP TABLE "volume_doi";
