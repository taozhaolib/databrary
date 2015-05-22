# --- !Ups

ALTER TABLE "slot" RENAME TO "slot_old";
ALTER TABLE "slot_old" DROP CONSTRAINT "slot_check";

---- up slots

DROP FUNCTION "slot_full_create" () CASCADE;
DROP FUNCTION "get_slot" (integer, segment);

CREATE TABLE "slot" ( -- ABSTRACT
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Check (false) NO INHERIT
);
ALTER TABLE "slot" ALTER COLUMN "segment" SET STORAGE plain;
COMMENT ON TABLE "slot" IS 'Generic table for objects associated with a temporal sub-sections of a container.  Inherit from this table to use the functions below.';


CREATE TABLE "slot_consent" (
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"consent" consent NOT NULL,
	Primary Key ("container", "segment"),
	Exclude USING gist (singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
COMMENT ON TABLE "slot_consent" IS 'Sharing/release permissions granted by participants on (portions of) contained data.';

SELECT audit.CREATE_TABLE ('slot_consent');

INSERT INTO slot_consent (container, segment, consent)
	SELECT source, segment, consent FROM slot_old WHERE consent IS NOT NULL;
INSERT INTO audit.slot_consent (audit_time, audit_user, audit_ip, audit_action, container, segment, consent)
	SELECT audit_time, audit_user, audit_ip, audit_action, source, segment, consent FROM audit.slot;

---- up assets

DROP VIEW "slot_asset";

CREATE TABLE "slot_asset" (
	"asset" integer NOT NULL Primary Key References "asset",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL
) INHERITS ("slot");
CREATE INDEX "slot_asset_slot_idx" ON "slot_asset" ("container", "segment");
COMMENT ON TABLE "slot_asset" IS 'Attachment point of assets, which, in the case of timeseries data, should match asset.duration.';
 
SELECT audit.CREATE_TABLE ('slot_asset');

INSERT INTO slot_asset (container, segment, asset)
	SELECT source, segment, asset FROM asset_slot JOIN slot_old ON slot = slot_old.id;
INSERT INTO audit.slot_asset (audit_time, audit_user, audit_ip, audit_action, container, segment, asset)
	SELECT audit_time, audit_user, audit_ip, audit_action, source, segment, asset FROM audit.asset_slot JOIN slot_old ON slot = slot_old.id;

DROP TABLE audit."asset_slot";
DROP TABLE "asset_slot";


ALTER TABLE "excerpt" RENAME TO "excerpt_old";
ALTER TABLE "excerpt_old" DROP CONSTRAINT "excerpt_pkey",
	DROP CONSTRAINT "excerpt_asset_fkey";

CREATE TABLE "excerpt" (
	"asset" integer NOT NULL References "slot_asset" ON DELETE CASCADE,
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Primary Key ("asset", "segment"),
	Exclude USING gist (singleton("asset") WITH =, "segment" WITH &&)
);
COMMENT ON TABLE "excerpt" IS 'Slot asset segments that have been selected for possible public release and top-level display.';
COMMENT ON COLUMN "excerpt"."segment" IS 'Segment within slot_asset.container space (not asset).';

-- this drops invalid excerpts:
INSERT INTO excerpt (asset, segment)
	SELECT excerpt_old.asset, slot_old.segment FROM excerpt_old JOIN slot_old ON slot = slot_old.id JOIN slot_asset ON excerpt_old.asset = slot_asset.asset AND slot_old.source = slot_asset.container;

-- this does not:
ALTER TABLE audit."excerpt" ADD "segment" segment;
DO $do$ BEGIN
	EXECUTE $$GRANT UPDATE ON TABLE audit."excerpt" TO $$ || quote_ident(current_user);;
	UPDATE audit."excerpt" SET segment = slot_old.segment FROM slot_old WHERE slot = slot_old.id;;
	EXECUTE $$REVOKE UPDATE ON TABLE audit."excerpt" FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."excerpt" DROP "slot", ALTER "segment" SET NOT NULL;

DROP TABLE "excerpt_old";

---- up comments

ALTER TABLE "comment" RENAME TO "comment_old";
ALTER TABLE "comment_old" DROP CONSTRAINT "comment_pkey" CASCADE,
	DROP CONSTRAINT "comment_who_fkey";
DROP INDEX "comment_slot_idx", "comment_parent_idx";

CREATE TABLE "comment" (
	"id" integer NOT NULL DEFAULT nextval('comment_id_seq') Primary Key,
	"who" integer NOT NULL References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"text" text NOT NULL,
	"parent" integer References "comment"
) INHERITS ("slot");
ALTER SEQUENCE "comment_id_seq" OWNED BY "comment"."id";
CREATE INDEX "comment_slot_idx" ON "comment" ("container", "segment");
CREATE INDEX ON "comment" ("parent");
COMMENT ON TABLE "comment" IS 'Free-text comments on objects (unaudited, immutable).';

INSERT INTO comment (id, who, container, segment, time, text, parent)
	SELECT comment_old.id, who, source, segment, time, text, parent FROM comment_old JOIN slot_old ON slot = slot_old.id;

DROP TABLE "comment_old" CASCADE;

CREATE VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
COMMENT ON VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

---- up tags

ALTER TABLE "tag_use" RENAME TO "tag_use_old";
ALTER TABLE "tag_use_old" DROP CONSTRAINT "tag_use_pkey",
	DROP CONSTRAINT "tag_use_tag_fkey",
	DROP CONSTRAINT "tag_use_who_fkey";
DROP INDEX "tag_use_slot_idx";

CREATE TABLE "tag_use" (
	"tag" integer NOT NULL References "tag",
	"who" integer NOT NULL References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"up" boolean NOT NULL DEFAULT true,
	Primary Key ("tag", "who", "container", "segment"),
	Exclude USING gist (singleton("tag") WITH =, singleton("who") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX ON "tag_use" ("who");
CREATE INDEX "tag_use_slot_idx" ON "tag_use" ("container", "segment");
COMMENT ON TABLE "tag_use" IS 'Applications of tags to objects along with their weight (+-1).';

INSERT INTO tag_use (tag, who, container, segment, up)
	SELECT tag, who, source, segment, up FROM tag_use_old JOIN slot_old ON slot = slot_old.id;

DROP TABLE "tag_use_old" CASCADE;

---- up records

ALTER TABLE "slot_record" RENAME TO "slot_record_old";
ALTER TABLE "slot_record_old" DROP CONSTRAINT "slot_record_pkey",
	DROP CONSTRAINT "slot_record_record_fkey";

CREATE TABLE "slot_record" (
	"record" integer NOT NULL References "record",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("record", "container", "segment"),
	Exclude USING gist (singleton("record") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX "slot_record_slot_idx" ON "slot_record" ("container", "segment");
COMMENT ON TABLE "slot_record" IS 'Attachment of records to slots.';

INSERT INTO slot_record (container, segment, record)
	SELECT source, segment, record FROM slot_record_old JOIN slot_old ON slot = slot_old.id;

DROP TABLE "slot_record_old";

CREATE OR REPLACE FUNCTION "record_consent" ("record" integer) RETURNS consent LANGUAGE sql STABLE STRICT AS
	$$ SELECT MIN(consent) FROM slot_record JOIN slot_consent ON slot_record.container = slot_consent.container AND slot_record.segment <@ slot_consent.segment WHERE record = $1 $$;
COMMENT ON FUNCTION "record_consent" (integer) IS 'Effective (minimal) consent level granted on the specified record.';

CREATE OR REPLACE FUNCTION "record_daterange" ("record" integer) RETURNS daterange LANGUAGE sql STABLE STRICT AS $$
	SELECT daterange(min(date), max(date), '[]') 
	  FROM slot_record
	  JOIN container ON slot_record.container = container.id
	 WHERE record = $1
$$;

---- up cleanup

DROP TABLE audit."slot";
DROP TABLE "object_segment" CASCADE;

# --- !Downs

ALTER TABLE "slot" RENAME TO "slot_new";
ALTER TABLE "slot_new" DROP CONSTRAINT "slot_check";

---- down slots

CREATE TABLE "object_segment" ( -- ABSTRACT
	"id" integer NOT NULL,
	"source" integer NOT NULL, -- References "source_table"
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Check (false) NO INHERIT
);
ALTER TABLE "object_segment" ALTER COLUMN "segment" SET STORAGE plain;
COMMENT ON TABLE "object_segment" IS 'Generic table for objects defined as a temporal sub-sequence of another object.  Inherit from this table to use the functions below.';

CREATE FUNCTION "object_segment_contains" ("object_segment", "object_segment") RETURNS boolean LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT $1.id = $2.id OR $1.source = $2.source AND $1.segment @> $2.segment $$;
CREATE FUNCTION "object_segment_within" ("object_segment", "object_segment") RETURNS boolean LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT $1.id = $2.id OR $1.source = $2.source AND $1.segment <@ $2.segment $$;
CREATE OPERATOR @> (PROCEDURE = "object_segment_contains", LEFTARG = "object_segment", RIGHTARG = "object_segment", COMMUTATOR = <@);
CREATE OPERATOR <@ (PROCEDURE = "object_segment_within", LEFTARG = "object_segment", RIGHTARG = "object_segment", COMMUTATOR = @>);

SELECT setval('container_id_seq', max(id), 't') FROM container;

CREATE TABLE "slot" (
	"id" integer NOT NULL DEFAULT nextval('container_id_seq') Primary Key,
	"source" integer NOT NULL References "container" ON UPDATE CASCADE ON DELETE CASCADE,
	"segment" segment NOT NULL,
	"consent" consent,
	Check ((id = source) = (segment = '(,)')),
	Unique ("source", "segment"),
	Exclude USING gist (singleton("source") WITH =, "segment" WITH &&) WHERE ("consent" IS NOT NULL)
) INHERITS ("object_segment");
COMMENT ON TABLE "slot" IS 'Sections of containers selected for referencing, annotating, consenting, etc.';
COMMENT ON COLUMN "slot"."consent" IS 'Sharing/release permissions granted by participants on (portions of) contained data.  This could equally well be an annotation, but hopefully won''t add too much space here.';

SELECT audit.CREATE_TABLE ('slot');
COMMENT ON TABLE audit."slot" IS 'Partial auditing for slot table covering only consent changes.';

CREATE FUNCTION "slot_full_create" () RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
	INSERT INTO slot (id, source, segment) VALUES (NEW.id, NEW.id, '(,)');;
	RETURN null;;
END;; $$;
CREATE TRIGGER "slot_full_create" AFTER INSERT ON "container" FOR EACH ROW EXECUTE PROCEDURE "slot_full_create" ();
COMMENT ON TRIGGER "slot_full_create" ON "container" IS 'Always create a "full"-range slot for each container.  Unfortunately nothing currently prevents them from being removed/changed.';

CREATE FUNCTION "get_slot" ("container" integer, "seg" segment) RETURNS integer STRICT LANGUAGE plpgsql AS $$
DECLARE
	slot_id integer;;
BEGIN
	LOOP
		SELECT id INTO slot_id FROM slot WHERE source = container AND segment = seg;;
		IF FOUND THEN
			RETURN slot_id;;
		END IF;;
		BEGIN
			INSERT INTO slot (source, segment) VALUES (container, seg) RETURNING id INTO slot_id;;
			RETURN slot_id;;
		EXCEPTION WHEN unique_violation THEN
		END;;
	END LOOP;;
END;; $$;

INSERT INTO slot (id, source, segment, consent)
	SELECT id, id, '(,)', consent FROM container LEFT JOIN slot_consent ON container.id = container AND segment = '(,)';

ALTER TABLE "container"
	ADD Foreign Key ("id") References "slot" Deferrable Initially Deferred;

INSERT INTO slot (source, segment, consent)
	SELECT container, segment, consent FROM slot_consent WHERE segment <> '(,)';
INSERT INTO audit.slot (audit_time, audit_user, audit_ip, audit_action, id, source, segment, consent)
	SELECT audit_time, audit_user, audit_ip, audit_action, id, container, slot_consent.segment, slot_consent.consent FROM audit.slot_consent JOIN slot ON container = source AND slot_consent.segment = slot.segment;

DROP TABLE audit."slot_consent";

---- down assets

CREATE TABLE "asset_slot" (
	"asset" integer NOT NULL Primary Key References "asset",
	"slot" integer NOT NULL References "slot"
);
COMMENT ON TABLE "asset_slot" IS 'Attachment point of assets, which, in the case of timeseries data, should match asset.duration.';

SELECT audit.CREATE_TABLE ('asset_slot');

INSERT INTO asset_slot (asset, slot)
	SELECT asset, get_slot(container, segment) FROM slot_asset;
INSERT INTO audit.asset_slot (audit_time, audit_user, audit_ip, audit_action, asset, slot)
	SELECT audit_time, audit_user, audit_ip, audit_action, asset, get_slot(container, segment) FROM audit.slot_asset;


ALTER TABLE "excerpt" RENAME TO "excerpt_new";
ALTER TABLE "excerpt_new" DROP CONSTRAINT "excerpt_pkey",
	DROP CONSTRAINT "excerpt_asset_fkey";

CREATE TABLE "excerpt" (
	"asset" integer NOT NULL References "asset",
	"slot" integer NOT NULL References "slot",
	Primary Key ("asset", "slot")
);
COMMENT ON TABLE "excerpt" IS 'Slot asset (segments) that have been selected for possible public release and top-level display.';

INSERT INTO excerpt (asset, slot)
	SELECT asset, get_slot(container, excerpt_new.segment) FROM excerpt_new JOIN slot_asset USING (asset);

ALTER TABLE audit."excerpt" ADD "slot" integer;
DO $do$ BEGIN
	EXECUTE $$GRANT UPDATE ON TABLE audit."excerpt" TO $$ || quote_ident(current_user);;
	UPDATE audit."excerpt" SET slot = get_slot(container, excerpt.segment) FROM slot_asset WHERE slot_asset.asset = excerpt.asset;;
	EXECUTE $$REVOKE UPDATE ON TABLE audit."excerpt" FROM $$ || quote_ident(current_user);;
END;; $do$;
ALTER TABLE audit."excerpt" DROP "segment", ALTER "slot" SET NOT NULL;

DROP TABLE "excerpt_new";
DROP TABLE audit."slot_asset";
DROP TABLE "slot_asset";

CREATE VIEW "slot_asset" ("asset", "segment", "slot", "excerpt") AS
	SELECT asset_slot.asset, slot_asset.segment, slot.id, slot_excerpt.segment
	  FROM asset_slot 
	  JOIN slot AS slot_asset ON asset_slot.slot = slot_asset.id
	  JOIN slot ON slot_asset.source = slot.source AND slot_asset.segment && slot.segment
	  LEFT JOIN excerpt
	       JOIN slot AS slot_excerpt ON excerpt.slot = slot_excerpt.id
	       ON asset_slot.asset = excerpt.asset AND slot.source = slot_excerpt.source AND slot.segment <@ slot_excerpt.segment;
COMMENT ON VIEW "slot_asset" IS 'Expanded set of all slots and the assets they include.';

---- down comments

ALTER TABLE "comment" RENAME TO "comment_new";
ALTER TABLE "comment_new" DROP CONSTRAINT "comment_pkey" CASCADE,
	DROP CONSTRAINT "comment_who_fkey";
DROP INDEX "comment_slot_idx", "comment_parent_idx";

CREATE TABLE "comment" (
	"id" integer NOT NULL DEFAULT nextval('comment_id_seq') Primary Key,
	"who" integer NOT NULL References "account",
	"slot" integer NOT NULL References "slot",
	"time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"text" text NOT NULL,
	"parent" integer References "comment"
);
ALTER SEQUENCE "comment_id_seq" OWNED BY "comment"."id";
CREATE INDEX ON "comment" ("slot");
CREATE INDEX ON "comment" ("parent");
COMMENT ON TABLE "comment" IS 'Free-text comments on objects (unaudited, immutable).';

INSERT INTO comment (id, who, slot, time, text, parent)
	SELECT id, who, get_slot(container, segment), time, text, parent FROM comment_new;

DROP TABLE "comment_new" CASCADE;

CREATE VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
COMMENT ON VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

---- down tags

ALTER TABLE "tag_use" RENAME TO "tag_use_new";
ALTER TABLE "tag_use_new" DROP CONSTRAINT "tag_use_pkey",
	DROP CONSTRAINT "tag_use_tag_fkey",
	DROP CONSTRAINT "tag_use_who_fkey";
DROP INDEX "tag_use_slot_idx";

CREATE TABLE "tag_use" (
	"tag" integer NOT NULL References "tag",
	"who" integer NOT NULL References "account",
	"slot" integer NOT NULL References "slot",
	"up" boolean NOT NULL DEFAULT true,
	Primary Key ("tag", "who", "slot")
);
CREATE INDEX ON "tag_use" ("slot");
COMMENT ON TABLE "tag_use" IS 'Applications of tags to objects along with their weight (+-1).';

INSERT INTO tag_use (tag, who, slot, up)
	SELECT tag, who, get_slot(container, segment), up FROM tag_use_new;

DROP TABLE "tag_use_new";

CREATE VIEW "tag_weight" ("tag", "slot", "weight") AS
	SELECT tag, slot, SUM(CASE WHEN up THEN 1::integer ELSE -1::integer END)::integer FROM tag_use GROUP BY tag, slot;

---- down records

ALTER TABLE "slot_record" RENAME TO "slot_record_new";
ALTER TABLE "slot_record_new" DROP CONSTRAINT "slot_record_pkey",
	DROP CONSTRAINT "slot_record_record_fkey";

CREATE TABLE "slot_record" (
	"slot" integer NOT NULL References "slot",
	"record" integer NOT NULL References "record",
	Primary Key ("slot", "record")
);
CREATE INDEX ON "slot_record" ("record");
COMMENT ON TABLE "slot_record" IS 'Attachment of records to slots.';

INSERT INTO slot_record (slot, record)
	SELECT get_slot(container, segment), record FROM slot_record_new;

DROP TABLE "slot_record_new";

CREATE OR REPLACE FUNCTION "record_consent" ("record" integer) RETURNS consent LANGUAGE sql STABLE STRICT AS
	$$ SELECT MIN(consent) FROM slot_record JOIN slot ON slot = slot.id WHERE record = $1 $$;
COMMENT ON FUNCTION "record_consent" (integer) IS 'Effective (minimal) consent level granted on the specified record.  This should really consider containing slots, too, though for now this is sufficient because the only identified measure is on participants, which should coincide with consents.';

CREATE OR REPLACE FUNCTION "record_daterange" ("record" integer) RETURNS daterange LANGUAGE sql STABLE STRICT AS $$
	SELECT daterange(min(date), max(date), '[]') 
	  FROM slot_record
	  JOIN slot ON slot = slot.id
	  JOIN container ON slot.source = container.id
	 WHERE record = $1
$$;

---- down cleanup

DROP TABLE "slot_new" CASCADE;
