# --- !Ups

INSERT INTO "record_category" ("id", "name") VALUES (-300, 'task');

ALTER TABLE "metric" ALTER "classification" DROP DEFAULT;
UPDATE "metric" SET "classification" = 'MATERIAL' WHERE "id" IN (-180, -90);
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-800, 'title', 'MATERIAL', 'text');
INSERT INTO "metric" ("id", "name", "classification", "type") VALUES (-600, 'description', 'MATERIAL', 'text');

INSERT INTO "record_template" ("category", "metric") VALUES (-300, -800);
INSERT INTO "record_template" ("category", "metric") VALUES (-300, -600);

# --- !Downs

DELETE FROM "record_category" WHERE "id" = -300 AND "name" = 'task';

UPDATE "metric" SET "classification" = 'DEIDENTIFIED' WHERE "id" IN (-180, -90);
ALTER TABLE "metric" ALTER "classification" SET DEFAULT 'DEIDENTIFIED';
DELETE FROM "metric" WHERE "id" IN (-800, -600);

