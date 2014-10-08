# --- !Ups

DROP TABLE "authorize_info";

DELETE FROM "record_template" WHERE "category" = -800 AND "metric" = -900;

# --- !Downs

CREATE TABLE "authorize_info" (
       "child" integer NOT NULL References "party" ON DELETE CASCADE,
       "parent" integer NOT NULL References "party",
       "info" text NOT NULL,
       Primary Key ("parent", "child"),
       Foreign Key ("parent", "child") References "authorize" ON DELETE CASCADE
);
COMMENT ON TABLE "authorize_info" IS 'Additional information provided with authorization requests for internal staff stuff.  Temporary.';

INSERT INTO "record_template" VALUES(-800, -900, true);
