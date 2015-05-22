# --- !Ups

CREATE INDEX ON "record" ("volume");

CREATE TABLE "record_measures" (
	"id" integer NOT NULL Primary Key References "record" ON UPDATE CASCADE ON DELETE CASCADE,
	"volume" integer NOT NULL,
	"category" smallint,
	"measures" text[] NOT NULL Default '{}'
);
CREATE INDEX ON "record_measures" ("volume");
COMMENT ON TABLE "record_measures" IS 'Automatically updated table representing "record JOIN measures".';

INSERT INTO record_measures SELECT id, volume, category, COALESCE(measures, '{}') FROM record LEFT JOIN measures ON id = record;

CREATE FUNCTION "record_measures_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	TRUNCATE record_measures;;
	INSERT INTO record_measures SELECT id, volume, category, COALESCE(measures, '{}') FROM record LEFT JOIN measures ON id = record;;
	RETURN null;;
END;; $$;

CREATE FUNCTION "record_measures_ri" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO record_measures SELECT NEW.*;;
	RETURN null;;
END;; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ri" ();

CREATE FUNCTION "record_measures_ru" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET volume = NEW.volume, category = NEW.category WHERE id = NEW.id;;
	RETURN null;;
END;; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ru" ();

CREATE FUNCTION "record_measures_mi" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = NEW.record;;
	RETURN null;;
END;; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();

CREATE FUNCTION "record_measures_mu" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id IN (OLD.record, NEW.record);;
	RETURN null;;
END;; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();

CREATE FUNCTION "record_measures_md" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = OLD.record;;
	RETURN null;;
END;; $$;
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();

CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_text" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_number" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_date" EXECUTE PROCEDURE "record_measures_refresh" ();

# --- !Downs

DROP INDEX "record_volume_idx";

DROP TABLE "record_measures";

DROP FUNCTION "record_measures_refresh" () CASCADE;
DROP FUNCTION "record_measures_ri" () CASCADE;
DROP FUNCTION "record_measures_ru" () CASCADE;
DROP FUNCTION "record_measures_mi" () CASCADE;
DROP FUNCTION "record_measures_mu" () CASCADE;
DROP FUNCTION "record_measures_md" () CASCADE;
