# --- !Ups

ALTER TABLE "asset_revision" RENAME "prev" TO "orig";
ALTER TABLE "asset_revision" RENAME "next" TO "asset";
ALTER TABLE "asset_revision"
	DROP CONSTRAINT "asset_revision_pkey",
	DROP CONSTRAINT "asset_revision_prev_fkey",
	DROP CONSTRAINT "asset_revision_next_fkey",
	ADD Foreign Key ("orig") References "asset" ON DELETE CASCADE,
	ADD Foreign Key ("asset") References "asset" ON DELETE CASCADE,
	ADD Unique ("asset");

DROP VIEW "asset_revisions";
CREATE RECURSIVE VIEW "asset_revisions" ("orig", "asset") AS
	SELECT * FROM asset_revision
	UNION
	SELECT o.orig, a.asset FROM asset_revision o JOIN asset_revisions a ON o.asset = a.orig;
COMMENT ON VIEW "asset_revisions" IS 'Transitive closure of asset_revision.';

CREATE OR REPLACE FUNCTION "asset_supersede" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM asset FROM asset_revision WHERE orig = asset_new;;
	IF FOUND THEN
		RAISE 'Asset % already superseded', asset_new;;
	END IF;;
	INSERT INTO asset_revision VALUES (asset_old, asset_new);;
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;;
END;; $$;

DROP TABLE "transcode";
CREATE TABLE "transcode" (
	"asset" integer NOT NULL Primary Key Default nextval('asset_id_seq'),
	"owner" integer NOT NULL References "party",
	"orig" integer NOT NULL References "asset" ON DELETE CASCADE,
	"segment" segment NOT NULL Default '(,)',
	"options" text[] NOT NULL Default '{}',
	"start" timestamp Default now(),
	"process" integer,
	"log" text
) INHERITS ("asset_revision");
COMMENT ON TABLE "transcode" IS 'Format conversions that are being or have been applied to transform in input asset.';

# --- !Downs

DROP TABLE "transcode";
CREATE TABLE "transcode" (
	"asset" integer NOT NULL Primary Key References "asset" ON DELETE CASCADE,
	"owner" integer NOT NULL References "party",
	"start" timestamp Default now(),
	"process" integer,
	"result" text
);

ALTER TABLE "asset_revision" RENAME "orig" TO "prev";
ALTER TABLE "asset_revision" RENAME "asset" TO "next";
ALTER TABLE "asset_revision"
	DROP CONSTRAINT "asset_revision_asset_key",
	DROP CONSTRAINT "asset_revision_orig_fkey",
	DROP CONSTRAINT "asset_revision_asset_fkey",
	ADD Foreign Key ("prev") References "asset",
	ADD Foreign Key ("next") References "asset",
	ADD Primary Key ("next", "prev");

DROP VIEW "asset_revisions";
CREATE VIEW "asset_revisions" AS
	WITH RECURSIVE r AS (
		SELECT * FROM asset_revision
		UNION ALL
		SELECT asset_revision.prev, r.next FROM asset_revision JOIN r ON asset_revision.next = r.prev
	) SELECT * FROM r;
COMMENT ON VIEW "asset_revisions" IS 'Transitive closure of asset_revision.  Revisions must never form a cycle or this will not terminate.';

CREATE OR REPLACE FUNCTION "asset_supersede" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM next FROM asset_revision WHERE prev = asset_new;;
	IF FOUND THEN
		RAISE 'Asset % already superseded', asset_new;;
	END IF;;
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;;
	INSERT INTO asset_revision VALUES (asset_old, asset_new);;
END;; $$;

