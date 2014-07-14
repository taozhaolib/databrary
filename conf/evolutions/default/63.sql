# --- !Ups

ALTER TABLE "metric" ADD "assumed" TEXT;
UPDATE "metric" SET "assumed" = 'typical' WHERE "id" = -520;
UPDATE "metric" SET "assumed" = 'English' WHERE "id" = -510;
UPDATE "metric" SET "assumed" = 'US' WHERE "id" = -150;
UPDATE "record_category" SET "name" = 'context' WHERE "id" = -100;

# --- !Downs

ALTER TABLE "metric" DROP "assumed";
UPDATE "record_category" SET "name" = 'location' WHERE "id" = -100;
