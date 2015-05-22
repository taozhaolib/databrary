# --- !Ups

ALTER TABLE "asset" ALTER "name" DROP NOT NULL,
	DROP "body";
ALTER TABLE audit."asset" ALTER "name" DROP NOT NULL,
	DROP "body";

# --- !Downs

ALTER TABLE "asset" RENAME "sha1" TO "sha1_old";
ALTER TABLE "asset" ALTER "name" SET NOT NULL,
	ADD "body" text,
	DROP CONSTRAINT "asset_sha1_check",
	ADD "sha1" bytea Check (octet_length("sha1") = 20);
UPDATE "asset" SET sha1 = sha1_old;
ALTER TABLE "asset" ALTER "sha1" SET NOT NULL,
	DROP "sha1_old";

ALTER TABLE audit."asset" RENAME "sha1" TO "sha1_old";
ALTER TABLE audit."asset" ALTER "name" SET NOT NULL,
	ADD "body" text,
	ADD "sha1" bytea;
DO $do$ BEGIN
	EXECUTE $$GRANT UPDATE ON TABLE audit."asset" TO $$ || quote_ident(current_user);;
	UPDATE audit."asset" SET sha1 = sha1_old;;
	EXECUTE $$REVOKE UPDATE ON TABLE audit."asset" FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."asset" DROP "sha1_old";
