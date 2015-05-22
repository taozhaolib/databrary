# --- !Ups

ALTER TABLE "metric" ADD Unique ("name");

# --- !Downs

ALTER TABLE "metric" DROP CONSTRAINT "metric_name_key";
