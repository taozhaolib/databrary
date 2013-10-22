# --- !Ups

INSERT INTO "record_category" ("id", "name") VALUES (-200, 'visit');
INSERT INTO "record_template" ("category", "metric") VALUES (-200, -900);

# --- !Downs

DELETE FROM "record_template" WHERE category = -200;
DELETE FROM "record_category" WHERE id = -200;
