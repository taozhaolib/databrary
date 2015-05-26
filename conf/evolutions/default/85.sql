# --- !Ups

CREATE TABLE "volume_doi" (
	"volume" integer NOT NULL Unique References "volume",
	"doi" varchar(16) NOT NULL Unique
);
COMMENT ON TABLE "volume_doi" IS 'DOIs issued for volumes (currently via EZID).';

# --- !Downs

DROP TABLE "volume_doi";
