# --- !Ups

CREATE FUNCTION "excerpt_shift" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	shift interval := lower(NEW.segment) - lower(OLD.segment);;
BEGIN
	IF NEW.segment = OLD.segment THEN
	ELSIF shift IS NULL THEN
		DELETE FROM excerpt WHERE asset = NEW.asset AND segment <> '(,)';;
	ELSE
		UPDATE excerpt SET segment = segment_shift(segment, shift) WHERE asset = NEW.asset;;
	END IF;;
	RETURN null;;
END;; $$;
CREATE TRIGGER "excerpt_shift" AFTER UPDATE OF "segment" ON "slot_asset" FOR EACH ROW EXECUTE PROCEDURE "excerpt_shift" ();
COMMENT ON TRIGGER "excerpt_shift" ON "slot_asset" IS 'Move or clear excerpts on repositioning of asset, just based on lower bound.';

# --- !Downs

DROP TRIGGER "excerpt_shift" ON "slot_asset";
DROP FUNCTION "excerpt_shift" ();
