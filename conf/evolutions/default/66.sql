# --- !Ups

DROP FUNCTION audit.CREATE_TABLE (name);
CREATE OR REPLACE FUNCTION audit.CREATE_TABLE (name, "parent" name = 'audit') RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	table_name CONSTANT text := quote_ident($1);;
BEGIN
	EXECUTE $$CREATE TABLE audit.$$ || table_name || $$ (LIKE public.$$ || table_name || $$) INHERITS (audit. $$ || quote_ident(parent) || $$) WITH (OIDS = FALSE)$$;;
	PERFORM audit.SET_PRIVILEGES($1);;
END;; $create$;
COMMENT ON FUNCTION audit.CREATE_TABLE (name, name) IS 'Create an audit.$1 table mirroring public.$1.';

ALTER TABLE audit."volume" ALTER "name" DROP NOT NULL;

SELECT audit.CREATE_TABLE ('slot');

ALTER TABLE audit."slot_consent" INHERIT audit."slot", NO INHERIT audit."audit";
ALTER TABLE audit."volume_inclusion" INHERIT audit."slot", NO INHERIT audit."audit";
ALTER TABLE audit."slot_asset" INHERIT audit."slot", NO INHERIT audit."audit";
ALTER TABLE audit."slot_record" INHERIT audit."slot", NO INHERIT audit."audit";

# --- !Downs

DROP FUNCTION audit.CREATE_TABLE (name, name);
CREATE OR REPLACE FUNCTION audit.CREATE_TABLE (name) RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	table_name CONSTANT text := quote_ident($1);;
BEGIN
	EXECUTE $$CREATE TABLE audit.$$ || table_name || $$ (LIKE public.$$ || table_name || $$) INHERITS (audit."audit") WITH (OIDS = FALSE)$$;;
	PERFORM audit.SET_PRIVILEGES($1);;
END;; $create$;
COMMENT ON FUNCTION audit.CREATE_TABLE (name) IS 'Create an audit.$1 table mirroring public.$1.';

ALTER TABLE audit."volume" ALTER "name" SET NOT NULL;

ALTER TABLE audit."slot_consent" INHERIT audit."audit", NO INHERIT audit."slot";
ALTER TABLE audit."volume_inclusion" INHERIT audit."audit", NO INHERIT audit."slot";
ALTER TABLE audit."slot_asset" INHERIT audit."audit", NO INHERIT audit."slot";
ALTER TABLE audit."slot_record" INHERIT audit."audit", NO INHERIT audit."slot";
DROP TABLE audit."slot";
