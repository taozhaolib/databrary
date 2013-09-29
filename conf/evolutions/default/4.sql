# --- !Ups

CREATE VIEW "audit_timeseries" AS
	SELECT *, NULL::interval AS "duration" FROM audit_file;
COMMENT ON VIEW "audit_timeseries" IS 'Timeseries are audited together with files.  This view provides glue to make that transparent.';
CREATE FUNCTION "audit_timeseries_file" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO audit_file ("when", who, ip, action, id, format, classification) VALUES (NEW."when", NEW.who, NEW.ip, NEW.action, NEW.id, NEW.format, NEW.classification);;
	RETURN NEW;;
END;; $$;
COMMENT ON FUNCTION "audit_timeseries_file" () IS 'Trigger function for INSTEAD OF INSERT ON audit_timeseries to propagate to audit_file.';
CREATE TRIGGER "audit_file" INSTEAD OF INSERT ON "audit_timeseries" FOR EACH ROW EXECUTE PROCEDURE "audit_timeseries_file" ();

# --- !Downs

DROP VIEW "audit_timeseries";
DROP FUNCTION "audit_timeseries_file" ();
