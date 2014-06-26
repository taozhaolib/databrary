# --- !Ups

DROP INDEX "volume_citation_volume_idx", "volume_citation_volume_idx1";
DELETE FROM "volume_citation" WHERE NOT "study" AND volume IN (SELECT volume FROM volume_citation GROUP BY volume HAVING count(head) > 1);
ALTER TABLE "volume_citation" DROP "body", DROP "study",
	ADD "authors" text[],
	ADD "year" smallint Check ("year" BETWEEN 1900 AND 2900),
	ADD Unique ("volume");
COMMENT ON TABLE "volume_citation" IS 'Publications/products corresponding to study volumes.';

SELECT audit.CREATE_TABLE ('volume_citation');

# --- !Downs

DROP TABLE audit."volume_citation";

ALTER TABLE "volume_citation" DROP "authors", DROP "year",
	DROP CONSTRAINT "volume_citation_volume_key",
	ADD "body" text,
	ADD "study" boolean NOT NULL Default false;
CREATE INDEX ON "volume_citation" ("volume");
CREATE UNIQUE INDEX ON "volume_citation" ("volume") WHERE "study";
COMMENT ON TABLE "volume_citation" IS 'Quick and dirty citation list.  Not intended to be permanent.  No PK: only updated in bulk on volume.';
COMMENT ON COLUMN "volume_citation"."study" IS 'Primary external citation associated with each volume, in the case of studies, supplementals, excerpts, or other volumes directly attached to publications.';
