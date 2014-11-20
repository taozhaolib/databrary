# --- !Ups

CREATE OR REPLACE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.classification >= 'SHARED'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id;

DROP TRIGGER "volume_citation_changed_text" ON "volume_citation";
CREATE TRIGGER "volume_citation_changed_text" AFTER INSERT OR UPDATE OF "head", "year" ON "volume_citation" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();

ALTER TABLE "volume_citation" DROP "authors";
ALTER TABLE audit."volume_citation" DROP "authors";

# --- !Downs

ALTER TABLE "volume_citation" RENAME "year" TO "year_old";
ALTER TABLE "volume_citation"
	DROP CONSTRAINT "volume_citation_year_check",
	ADD "authors" text[],
	ADD "year" smallint Check ("year" BETWEEN 1900 AND 2900);
UPDATE volume_citation SET year = year_old;

ALTER TABLE audit."volume_citation" RENAME "year" TO "year_old";
ALTER TABLE audit."volume_citation"
	ADD "authors" text[],
	ADD "year" smallint;
DO $do$ BEGIN
EXECUTE $$GRANT UPDATE ON TABLE audit.volume_citation TO $$ || quote_ident(current_user);;
UPDATE audit.volume_citation SET year = year_old;;
EXECUTE $$REVOKE UPDATE ON TABLE audit.volume_citation FROM $$ || quote_ident(current_user);;
END;; $do$;

CREATE OR REPLACE VIEW "volume_text" ("volume", "text") AS
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
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id;

DROP TRIGGER "volume_citation_changed_text" ON "volume_citation";
CREATE TRIGGER "volume_citation_changed_text" AFTER INSERT OR UPDATE OF "head", "authors" ON "volume_citation" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();

ALTER TABLE "volume_citation" DROP "year_old";
ALTER TABLE audit."volume_citation" DROP "year_old";
