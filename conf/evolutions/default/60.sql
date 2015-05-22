# --- !Ups

DROP VIEW "authorize_view";
DROP MATERIALIZED VIEW "authorize_inherit";

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize
		UNION
		SELECT a.child, aa.parent, CASE
		         WHEN aa.site = 'ADMIN' THEN LEAST(a.site, 'EDIT')
			 WHEN aa.site = 'EDIT' THEN LEAST(a.site, 'READ')
			 ELSE LEAST(aa.site, a.site, 'PUBLIC')
		       END, 'NONE', LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent
	) SELECT * FROM aa
	UNION ALL SELECT id, id, 'ADMIN', 'ADMIN', NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, 'ADMIN', 'NONE', NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE VIEW "authorize_view" ("child", "parent", "site", "member") AS
	SELECT child, parent, MAX(site), MAX(member)
	  FROM authorize_inherit
	 WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';

# --- !Downs

DROP VIEW "authorize_view";
DROP MATERIALIZED VIEW "authorize_inherit";

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize
		UNION
		SELECT a.child, aa.parent, CASE
		         WHEN aa.site = 'ADMIN' THEN LEAST(a.site, 'EDIT')
			 WHEN aa.site = 'EDIT' THEN LEAST(a.site, 'READ')
			 ELSE 'NONE'::permission
		       END, 'NONE', LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent
	) SELECT * FROM aa
	UNION ALL SELECT id, id, 'ADMIN', 'ADMIN', NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, 'ADMIN', 'NONE', NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE VIEW "authorize_view" ("child", "parent", "site", "member") AS
	SELECT child, parent, MAX(site), MAX(member)
	  FROM authorize_inherit
	 WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';
