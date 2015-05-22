# --- !Ups

ALTER TABLE "volume" ADD "alias" varchar(64);
COMMENT ON COLUMN "volume"."alias" IS 'Short, internal, code name for this volume, for contributors to reference their own data.';
ALTER TABLE audit."volume" ADD "alias" varchar(64);

# --- !Downs

ALTER TABLE audit."volume" DROP "alias";
ALTER TABLE "volume" DROP "alias";
