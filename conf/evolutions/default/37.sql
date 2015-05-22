# --- !Ups

CREATE TABLE "authorize_info" (
	"child" integer NOT NULL References "party" ON DELETE Cascade,
	"parent" integer NOT NULL References "party",
	"info" text NOT NULL,
	Primary Key ("parent", "child"),
	Foreign Key ("parent", "child") References "authorize" ON DELETE CASCADE
);
COMMENT ON TABLE "authorize_info" IS 'Additional information provided with authorization requests for internal staff stuff.  Temporary.';

# --- !Downs

DROP TABLE "authorize_info";
