# --- !Ups

CREATE OR REPLACE FUNCTION "slot_full_create" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	slot_id integer;;
BEGIN
	INSERT INTO slot (source, segment) VALUES (NEW.id, '(,)') RETURNING id INTO STRICT slot_id;;
	RETURN null;;
END;; $$;

DROP TABLE audit."toplevel_slot";
DROP TABLE "toplevel_slot";

# --- !Downs

CREATE TABLE "toplevel_slot" (
	"slot" integer NOT NULL Primary Key References "slot"
);
COMMENT ON TABLE "toplevel_slot" IS 'Slots whose assets are promoted to the top volume level for display.';

SELECT audit.CREATE_TABLE ('toplevel_slot');

CREATE OR REPLACE FUNCTION "slot_full_create" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	slot_id integer;;
BEGIN
	INSERT INTO slot (source, segment) VALUES (NEW.id, '(,)') RETURNING id INTO STRICT slot_id;;
	IF NEW.top THEN
		INSERT INTO toplevel_slot VALUES (slot_id);;
	END IF;;
	RETURN null;;
END;; $$;

INSERT INTO toplevel_slot VALUES (1);
