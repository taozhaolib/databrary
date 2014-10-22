# --- !Ups

CREATE AGGREGATE "tsvector_agg" (tsvector) (SFUNC = tsvector_concat, STYPE = tsvector, INITCOND = '');

CREATE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, unnest(authors) FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.classification >= 'SHARED'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id WHERE up;
COMMENT ON VIEW "volume_text" IS 'All (searchable) text data associated with a volume.';

CREATE TABLE "volume_text_idx" (
	"volume" integer Primary Key References "volume" ON DELETE CASCADE ON UPDATE CASCADE,
	"ts" tsvector NOT NULL
);
COMMENT ON TABLE "volume_text_idx" IS 'Overall tsvector for each volume, automatically updated to represent "SELECT volume, tsvector_agg(to_tsvector(text)) FROM volume_text GROUP BY volume".';
CREATE INDEX ON "volume_text_idx" USING gin("ts");

CREATE FUNCTION "volume_text_refresh" (volume_id integer = NULL) RETURNS void LANGUAGE plpgsql AS $$ BEGIN
	IF volume_id IS NULL THEN
		TRUNCATE volume_text_idx;;
		INSERT INTO volume_text_idx SELECT volume, tsvector_agg(to_tsvector('english', text)) FROM volume_text GROUP BY volume;;
	ELSE
		DELETE FROM volume_text_idx WHERE volume = volume_id;;
		INSERT INTO volume_text_idx VALUES (volume_id, (SELECT tsvector_agg(to_tsvector('english', text)) FROM volume_text WHERE volume = volume_id));;
	END IF;;
END;; $$;
CREATE FUNCTION "volume_text_changed" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	vol integer;;
BEGIN
	IF TG_TABLE_NAME = 'volume' THEN
		vol = NEW.id;;
	ELSE
		vol = NEW.volume;;
	END IF;;
	PERFORM volume_text_refresh(vol);;
	RETURN null;;
END;; $$;
CREATE TRIGGER "volume_changed_text" AFTER INSERT OR UPDATE OF "name", "body" ON "volume" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();
CREATE TRIGGER "volume_citation_changed_text" AFTER INSERT OR UPDATE OF "head", "authors" ON "volume_citation" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();

SELECT volume_text_refresh();

# --- !Downs

DROP TRIGGER "volume_changed_text" ON "volume";
DROP TRIGGER "volume_citation_changed_text" ON "volume_citation";
DROP FUNCTION "volume_text_changed" ();
DROP FUNCTION "volume_text_refresh" (integer);
DROP TABLE "volume_text_idx";
DROP VIEW "volume_text";
DROP AGGREGATE "tsvector_agg" (tsvector);
