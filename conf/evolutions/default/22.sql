# --- !Ups

SELECT setval('container_id_seq', nextval('slot_id_seq')-1, 't');
ALTER TABLE "slot"
	ALTER "id" SET DEFAULT nextval('container_id_seq'),
	DROP CONSTRAINT "slot_source_fkey",
	ALTER "segment" DROP DEFAULT,
	ADD Foreign Key ("source") References "container" ON UPDATE CASCADE ON DELETE CASCADE;
DROP SEQUENCE "slot_id_seq";

-- Unfortunately we can't do this with a simple UPDATE because we can't control the order of updates:
DO $do$ DECLARE
	slot_id integer;;
	container_id integer;;
BEGIN
	EXECUTE $$GRANT UPDATE ON TABLE audit."container" TO $$ || quote_ident(current_user);;
	LOCK TABLE "container", "slot";;
	FOR slot_id, container_id IN SELECT id, source FROM slot WHERE segment = '(,)' ORDER BY source DESC LOOP
		UPDATE audit."container" SET id = slot_id WHERE id = container_id;;
		UPDATE "container" SET id = slot_id WHERE id = container_id;;
	END LOOP;;
	EXECUTE $$REVOKE UPDATE ON TABLE audit."container" FROM $$ || quote_ident(current_user);;
END;; $do$;

ALTER TABLE "container"
	ADD Foreign Key ("id") References "slot" Deferrable Initially Deferred;

ALTER TABLE "slot"
	ADD Check ((id = source) = (segment = '(,)'));
DROP INDEX "slot_full_container_idx";

CREATE OR REPLACE FUNCTION "slot_full_create" () RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
	INSERT INTO slot (id, source, segment) VALUES (NEW.id, NEW.id, '(,)');;
	RETURN null;;
END;; $$;

# --- !Downs

ALTER TABLE "container"
	DROP CONSTRAINT "container_id_fkey";

CREATE SEQUENCE "slot_id_seq" OWNED BY "slot"."id";
SELECT setval('slot_id_seq', nextval('container_id_seq')-1, 't');
ALTER TABLE "slot"
	ALTER "id" SET DEFAULT nextval('slot_id_seq'),
	DROP CONSTRAINT "slot_source_fkey",
	ADD Foreign Key ("source") References "container",
	ALTER "segment" SET DEFAULT '(,)',
	DROP CONSTRAINT "slot_check";
CREATE UNIQUE INDEX "slot_full_container_idx" ON "slot" ("source") WHERE "segment" = '(,)';

SELECT setval('container_id_seq', max(id), 't') FROM container;

CREATE OR REPLACE FUNCTION "slot_full_create" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	slot_id integer;;
BEGIN
	INSERT INTO slot (source, segment) VALUES (NEW.id, '(,)') RETURNING id INTO STRICT slot_id;;
	RETURN null;;
END;; $$;
