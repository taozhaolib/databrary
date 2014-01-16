# --- !Ups

ALTER TABLE "volume_access" ADD "funding" text;
ALTER TABLE audit."volume_access" ADD "funding" text;

-- we assume there are no conflicts here:
INSERT INTO "volume_access" (volume, party, funding) SELECT volume, funder, COALESCE("grant", '') FROM volume_funding;

DROP TABLE "volume_funding";

# --- !Downs

CREATE TABLE "volume_funding" (
       "volume" integer NOT NULL References "volume",
       "funder" integer NOT NULL References "party",
       "grant" text
);
CREATE INDEX ON "volume_funding" ("volume");
CREATE INDEX ON "volume_funding" ("funder");
COMMENT ON TABLE "volume_funding" IS 'Quick and dirty funding list.  No PK: only updated in bulk on volume.';

INSERT INTO volume_funding SELECT volume, party, NULLIF(funding, '') FROM volume_access WHERE funding IS NOT NULL;
DELETE FROM volume_access WHERE funding IS NOT NULL AND access = 'NONE' AND inherit = 'NONE';

ALTER TABLE audit."volume_access" DROP "funding";
ALTER TABLE "volume_access" DROP "funding";
