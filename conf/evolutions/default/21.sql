# --- !Ups

CREATE OR REPLACE FUNCTION "random_string" ("length" smallint, "charset" text = '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz') RETURNS varchar LANGUAGE sql VOLATILE STRICT AS
	$$ SELECT string_agg(substr($2, (length($2)*random()+0.5)::smallint, 1), '') FROM generate_series(1,$1) $$;

ALTER TABLE "token"
	ALTER "token" SET DEFAULT "random_string"(64::smallint),
	ALTER "expires" DROP DEFAULT,
	ADD Check (false) NO INHERIT;

CREATE TABLE "account_token" (
	"token" char(64) Primary Key,
	"expires" timestamp NOT NULL,
	"account" integer NOT NULL References "account",
	Check (false) NO INHERIT
) INHERITS ("token");
COMMENT ON TABLE "account_token" IS 'Generic tokens associated with particular accounts.';

ALTER TABLE "login_token"
	INHERIT "account_token",
	NO INHERIT "token",
	ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week';

CREATE TABLE "session" (
	"token" char(64) Primary Key NOT NULL,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '4 weeks',
	"account" integer NOT NULL References "account"
) INHERITS ("account_token");
COMMENT ON TABLE "session" IS 'Tokens associated with currently logged-in sessions.';

# --- !Downs

ALTER TABLE "token"
	ALTER "token" DROP DEFAULT,
	ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week',
	DROP CONSTRAINT "token_check";
ALTER TABLE "login_token"
	INHERIT "token",
	NO INHERIT "account_token",
	ALTER "token" SET DEFAULT "random_string"(64::smallint);
DROP TABLE "account_token" CASCADE;

CREATE OR REPLACE FUNCTION "random_string" ("length" smallint, "charset" text = '()-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~') RETURNS varchar LANGUAGE sql VOLATILE STRICT AS
	$$ SELECT string_agg(substr($2, (length($2)*random()+0.5)::smallint, 1), '') FROM generate_series(1,$1) $$;
