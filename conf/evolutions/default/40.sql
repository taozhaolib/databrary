# --- !Ups

UPDATE "record_category" SET "name" = 'group' WHERE "id" = -200;
INSERT INTO "record_category" ("id", "name") VALUES (-800, 'pilot');
INSERT INTO "record_category" ("id", "name") VALUES (-700, 'exclusion');
INSERT INTO "record_category" ("id", "name") VALUES (-400, 'condition');
INSERT INTO "record_category" ("id", "name") VALUES (-100, 'location');

ALTER TABLE "metric" RENAME "values" TO "options";
COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.';

INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-520, 'disability', 'IDENTIFIED', 'text');
INSERT INTO "metric" ("id", "name", "type", "options") VALUES (-700, 'reason', 'text', ARRAY['did not meet criteria','procedural/experimenter error','withdrew/fussy/tired','outlier']); 
INSERT INTO "metric" ("id", "name", "type", "options") VALUES (-180, 'setting', 'text', ARRAY['lab','home','museum','classroom','outdoor','clinic']);
INSERT INTO "metric" ("id", "name", "type") VALUES (-150, 'country', 'text');
INSERT INTO "metric" ("id", "name", "type", "options") VALUES (-140, 'state', 'text', ARRAY['AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','MD','MA','MI','MN','MS','MO','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']);
INSERT INTO "metric" ("id", "name", "type") VALUES (-90, 'info', 'text');

INSERT INTO "record_template" ("category", "metric") VALUES (-800, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-700, -700);
INSERT INTO "record_template" ("category", "metric") VALUES (-400, -900);
INSERT INTO "record_template" ("category", "metric") VALUES (-100, -180);
INSERT INTO "record_template" ("category", "metric") VALUES (-100, -140);

COMMENT ON TABLE "measure" IS 'Abstract parent of all measure tables containing data values.';

# --- !Downs

COMMENT ON TABLE "measure" IS 'Abstract parent of all measure tables containing data values.  Rough prototype.';

DELETE FROM "record_category" WHERE "id" IN (-800, -700, -400, -100);
UPDATE "record_category" SET "name" = 'visit' WHERE "id" = -200;

DELETE FROM "metric" WHERE "id" IN (-520, -700, -180, -150, -140, -90);

COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.  Rough prototype.';
ALTER TABLE "metric" RENAME "options" TO "values";
