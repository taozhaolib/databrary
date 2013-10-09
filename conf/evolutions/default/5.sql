# --- !Ups

CREATE TABLE "volume_funding" (
	"volume" integer NOT NULL References "volume",
	"funder" integer NOT NULL References "party",
	"grant" text
);
CREATE INDEX ON "volume_funding" ("volume");
COMMENT ON TABLE "volume_funding" IS 'Quick and dirty funding list.  No PK: only updated in bulk on volume.';

# --- !Downs

DROP TABLE "volume_funding";
