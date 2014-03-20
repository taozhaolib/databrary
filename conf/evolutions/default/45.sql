# --- !Ups

CREATE INDEX ON "comment" ("who");

DROP VIEW "comment_thread";
CREATE MATERIALIZED VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;;
CREATE INDEX "comment_thread_slot_idx" ON "comment_thread" ("container", "segment");
CREATE INDEX ON "comment_thread" ("who");
COMMENT ON MATERIALIZED VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

CREATE FUNCTION "comment_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "comment_thread";;
	RETURN null;;
END;; $$;
CREATE TRIGGER "comment_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "comment" FOR EACH STATEMENT EXECUTE PROCEDURE "comment_refresh" ();

# --- !Downs

DROP TRIGGER "comment_changed" ON "comment";
DROP FUNCTION "comment_refresh" ();

DROP MATERIALIZED VIEW "comment_thread";
CREATE VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;;
COMMENT ON VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

DROP INDEX "comment_who_idx";
