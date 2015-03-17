# --- !Ups

ALTER TABLE "party" RENAME "name" TO "sortname";
ALTER TABLE "party"
	ADD "prename" text,
	ALTER "prename" SET STORAGE EXTERNAL;
UPDATE party SET prename = regexp_replace(sortname, ' [^ ]*$', ''), sortname = regexp_replace(sortname, '^.* ', '') FROM account WHERE position(' ' in sortname) > 0 AND party.id = account.id;

ALTER TABLE audit."party" RENAME "name" TO "sortname";
ALTER TABLE audit."party" ADD "prename" text;

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

-- UPDATE audit.party SET sortname = COALESCE(prename || ' ', '') || sortname;
ALTER TABLE audit."party" RENAME "sortname" TO "name";
ALTER TABLE audit."party" DROP "prename";
