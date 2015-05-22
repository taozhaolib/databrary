# --- !Ups

ALTER TABLE "party" DROP "duns";
ALTER TABLE audit."party" DROP "duns";

# --- !Downs

ALTER TABLE "party" ADD "duns" numeric(9);
ALTER TABLE "party" RENAME "url" TO "url_old";
ALTER TABLE "party" ADD "url" text;
UPDATE "party" SET url = url_old;
ALTER TABLE "party" DROP "url_old";

COMMENT ON COLUMN "party"."duns" IS 'http://en.wikipedia.org/wiki/DUNS';

UPDATE party SET duns = 0 WHERE id > 0 AND NOT EXISTS (SELECT * FROM account WHERE account.id = party.id);

ALTER TABLE audit."party" ADD "duns" numeric(9);
ALTER TABLE audit."party" RENAME "url" TO "url_old";
ALTER TABLE audit."party" ADD "url" text;
DO $do$ BEGIN
EXECUTE $$GRANT UPDATE ON TABLE audit.party TO $$ || quote_ident(current_user);;
UPDATE audit.party SET url = url_old;;
EXECUTE $$REVOKE UPDATE ON TABLE audit.party FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."party" DROP "url_old";

