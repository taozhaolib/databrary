# --- !Ups

ALTER TABLE "token" ALTER "token" TYPE char(32) USING substring(token FOR 32);

ALTER TABLE "slot_consent"
	DROP CONSTRAINT "slot_consent_container_fkey",
	ADD Foreign Key ("container") References "container" ON DELETE CASCADE;

ALTER TABLE "comment" ADD Check ("parent" < "id");

# --- !Downs

ALTER TABLE "token" ALTER "token" TYPE char(64);

ALTER TABLE "slot_consent"
	DROP CONSTRAINT "slot_consent_container_fkey",
	ADD Foreign Key ("container") References "container";

ALTER TABLE "comment" DROP CONSTRAINT "comment_check";
