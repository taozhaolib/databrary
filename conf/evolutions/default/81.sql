# --- !Ups

ALTER TABLE "party" RENAME "name" TO "sortname";
ALTER TABLE "party" RENAME "orcid" TO "orcid_old";
ALTER TABLE "party" RENAME "affiliation" TO "affiliation_old";
ALTER TABLE "party" RENAME "url" TO "url_old";
ALTER TABLE "party"
	ADD "prename" text,
	ALTER "prename" SET STORAGE EXTERNAL,
	ADD "orcid" char(16),
	ADD "affiliation" text,
	ALTER "affiliation" SET STORAGE EXTERNAL,
	ADD "url" text;
UPDATE party SET prename = regexp_replace(sortname, ' [^ ]*$', ''), sortname = regexp_replace(sortname, '^.* ', '') FROM account WHERE position(' ' in sortname) > 0 AND party.id = account.id;
UPDATE party SET orcid = orcid_old, affiliation = affiliation_old, url = url_old;
ALTER TABLE "party"
	DROP "orcid_old",
	DROP "affiliation_old",
	DROP "url_old";
COMMENT ON COLUMN "party"."orcid" IS 'http://en.wikipedia.org/wiki/ORCID';

ALTER TABLE audit."party" RENAME "name" TO "sortname";
ALTER TABLE audit."party" RENAME "orcid" TO "orcid_old";
ALTER TABLE audit."party" RENAME "affiliation" TO "affiliation_old";
ALTER TABLE audit."party" RENAME "url" TO "url_old";
ALTER TABLE audit."party" ADD "prename" text,
	ADD "orcid" char(16),
	ADD "affiliation" text,
	ADD "url" text;
DO $do$ BEGIN
EXECUTE $$GRANT UPDATE ON TABLE audit.party TO $$ || quote_ident(current_user);;
UPDATE audit.party SET orcid = orcid_old, affiliation = affiliation_old, url = url_old;;
EXECUTE $$REVOKE UPDATE ON TABLE audit.party FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."party"
	DROP "orcid_old",
	DROP "affiliation_old",
	DROP "url_old";

CREATE OR REPLACE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || sortname FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.classification >= 'SHARED'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here

# --- !Downs

CREATE OR REPLACE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, sortname FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.classification >= 'SHARED'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here

UPDATE party SET sortname = COALESCE(prename || ' ', '') || sortname;
ALTER TABLE "party" RENAME "sortname" TO "name";
ALTER TABLE "party" DROP "prename";

DO $do$ BEGIN
EXECUTE $$GRANT UPDATE ON TABLE audit.party TO $$ || quote_ident(current_user);;
UPDATE audit.party SET sortname = COALESCE(prename || ' ', '') || sortname;;
EXECUTE $$REVOKE UPDATE ON TABLE audit.party FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."party" RENAME "sortname" TO "name";
ALTER TABLE audit."party" DROP "prename";
