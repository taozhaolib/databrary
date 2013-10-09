# --- !Ups

CREATE FUNCTION "random_string" ("length" smallint, "charset" text = '()-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~') RETURNS varchar LANGUAGE sql VOLATILE STRICT AS
	$$ SELECT string_agg(substr($2, (length($2)*random()+0.5)::smallint, 1), '') FROM generate_series(1,$1) $$;
COMMENT ON FUNCTION "random_string" (smallint, text) IS 'Generate a random string of the given length drawn from the given list of characters.  This uses the postgres random function, which is not cryptographically secure.';

CREATE TABLE "token" (
	"token" char(64) Primary Key NOT NULL,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '1 week'
);
COMMENT ON TABLE "token" IS 'Generic tokens issued to automatically perform actions such as logins or authorizations.';

CREATE TABLE "login_token" (
	"token" char(64) Primary Key NOT NULL DEFAULT "random_string"(64::smallint), -- could generate pk violations
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '1 week',
	"account" integer NOT NULL References "account",
	"password" boolean NOT NULL DEFAULT false
) INHERITS ("token");
CREATE UNIQUE INDEX "login_token_account_idx" ON "login_token" ("account") WHERE "password";
COMMENT ON TABLE "login_token" IS 'Tokens issued to automatically login/register users or reset passwords.';

# --- !Downs

DROP TABLE "login_token";
DROP TABLE "token";
DROP FUNCTION "random_string" (smallint, text);
