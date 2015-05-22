# --- !Ups

ALTER TABLE "party" ADD "duns" numeric(9);
COMMENT ON COLUMN "party"."orcid" IS 'http://en.wikipedia.org/wiki/ORCID';
COMMENT ON COLUMN "party"."duns" IS 'http://en.wikipedia.org/wiki/DUNS';

ALTER TABLE audit."party" ADD "duns" numeric(9);

# --- !Downs

ALTER TABLE audit."party" DROP "duns";

ALTER TABLE "party" DROP "duns";
COMMENT ON COLUMN "party"."orcid" IS NULL;
