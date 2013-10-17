# --- !Ups

CREATE INDEX ON "volume_funding" ("funder");

# --- !Downs

DROP INDEX "volume_funding_funder_idx";
