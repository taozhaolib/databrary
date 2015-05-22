# --- !Ups

CREATE TABLE "upload" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '1 week',
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	"filename" text NOT NULL
) INHERITS ("account_token");
COMMENT ON TABLE "upload" IS 'Tokens issued to track active uploads.';

# --- !Downs

DROP TABLE "upload";
