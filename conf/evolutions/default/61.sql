# --- !Ups

SELECT audit.CREATE_TABLE ('record');

SELECT audit.CREATE_TABLE ('measure');
ALTER TABLE audit."measure" ADD "datum" text NOT NULL;

CREATE FUNCTION audit."measure_i" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	IF NEW.audit_time IS NULL THEN
		NEW.audit_time := CURRENT_TIMESTAMP;;
	END IF;;
	INSERT INTO audit."measure" SELECT NEW.*;;
	RETURN NEW;;
END;; $$;
CREATE VIEW audit."measure_text" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_text" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_number" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_number" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_date" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_date" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();

SELECT audit.CREATE_TABLE ('slot_record');

# --- !Downs

DROP TABLE audit.slot_record;
DROP VIEW audit.measure_date;
DROP VIEW audit.measure_number;
DROP VIEW audit.measure_text;
DROP FUNCTION audit.measure_i ();
DROP TABLE audit.measure;
DROP TABLE audit.record;
