-- All for lack of "CREATE OR RELPACE MATERIALIZED VIEW"

# --- !Ups

DELETE FROM "authorize" WHERE child = 0 AND parent = -1;

DROP VIEW "authorize_view";
DROP MATERIALIZED VIEW "authorize_inherit";

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize WHERE authorized IS NOT NULL
		UNION
		SELECT a.child, aa.parent, LEAST(a.inherit,
			CASE WHEN aa.child <= 0 THEN aa.inherit
			     WHEN aa.inherit = 'ADMIN' THEN 'CONTRIBUTE'
			     WHEN aa.inherit = 'CONTRIBUTE' THEN 'DOWNLOAD'
			     ELSE 'NONE' END), NULL, GREATEST(a.authorized, aa.authorized), LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent WHERE a.authorized IS NOT NULL
	) SELECT * FROM aa
	UNION ALL SELECT id, id, enum_last(NULL::permission), enum_last(NULL::permission), NULL, NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, enum_last(NULL::permission), NULL, NULL, NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE VIEW "authorize_view" ("child", "parent", "inherit", "direct") AS
	SELECT child, parent, MAX(inherit), MAX(direct)
	  FROM authorize_inherit
	 WHERE (authorized IS NULL OR authorized <= CURRENT_TIMESTAMP) AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';

# --- !Downs

INSERT INTO "authorize" ("child", "parent", "inherit", "authorized") VALUES (0, -1, 'ADMIN', '2013-1-1');

DROP VIEW "authorize_view";
DROP MATERIALIZED VIEW "authorize_inherit";

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize WHERE authorized IS NOT NULL
		UNION
		SELECT a.child, aa.parent, LEAST(a.inherit,
			CASE WHEN aa.child <= 0 THEN aa.inherit
			     WHEN aa.inherit = 'ADMIN' THEN 'CONTRIBUTE'
			     WHEN aa.inherit = 'CONTRIBUTE' THEN 'DOWNLOAD'
			     ELSE 'NONE' END), NULL, GREATEST(a.authorized, aa.authorized), LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent WHERE a.authorized IS NOT NULL
	) SELECT * FROM aa
	UNION ALL SELECT id, id, enum_last(NULL::permission), enum_last(NULL::permission), NULL, NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE VIEW "authorize_view" ("child", "parent", "inherit", "direct") AS
	SELECT child, parent, MAX(inherit), MAX(direct)
	  FROM authorize_inherit
	 WHERE (authorized IS NULL OR authorized <= CURRENT_TIMESTAMP) AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';
