# --- !Ups

ALTER TABLE "comment" ADD "parent" integer References "comment";
CREATE INDEX ON "comment" ("parent");

CREATE VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
COMMENT ON VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

# --- !Downs

DROP VIEW "comment_thread";
ALTER TABLE "comment" DROP "parent";
