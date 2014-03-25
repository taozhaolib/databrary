# --- !Ups

ALTER TABLE "account_token"
	DROP CONSTRAINT "account_token_account_fkey",
	ADD Foreign Key ("account") References "account" ON DELETE CASCADE;
ALTER TABLE "login_token"
	DROP CONSTRAINT "login_token_account_fkey",
	ADD Foreign Key ("account") References "account" ON DELETE CASCADE;
ALTER TABLE "session"
	DROP CONSTRAINT "session_account_fkey",
	ADD Foreign Key ("account") References "account" ON DELETE CASCADE;
ALTER TABLE "avatar"
	DROP CONSTRAINT "avatar_party_fkey",
	ADD Foreign Key ("party") References "party" ON DELETE CASCADE;

# --- !Downs

ALTER TABLE "account_token"
	DROP CONSTRAINT "account_token_account_fkey",
	ADD Foreign Key ("account") References "account";
ALTER TABLE "login_token"
	DROP CONSTRAINT "login_token_account_fkey",
	ADD Foreign Key ("account") References "account";
ALTER TABLE "session"
	DROP CONSTRAINT "session_account_fkey",
	ADD Foreign Key ("account") References "account";
ALTER TABLE "avatar"
	DROP CONSTRAINT "avatar_party_fkey",
	ADD Foreign Key ("party") References "party";
