# --- !Ups

COMMENT ON COLUMN "metric"."options" IS '(Suggested) options for text enumerations, not enforced.';
ALTER TABLE "record_template" ADD "ident" boolean NOT NULL Default false;
UPDATE record_template SET ident = true WHERE metric = -900;
INSERT INTO record_template VALUES (-300, -900, true);
UPDATE record_template SET ident = true WHERE category = -700 AND metric = -700;
UPDATE record_template SET ident = true WHERE category = -100 AND metric IN (-180, -150, -140);
INSERT INTO record_template VALUES (-100, -150, true);

# --- !Downs

COMMENT ON COLUMN "metric"."options" IS NULL;
ALTER TABLE "record_template" DROP "ident";
DELETE FROM record_template WHERE category = -100 AND metric = -150;
DELETE FROM record_template WHERE category = -300 AND metric = -900;
