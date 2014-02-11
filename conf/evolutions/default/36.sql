# --- !Ups

DROP FUNCTION "volume_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_delegate_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_parents" (integer, permission);
DROP VIEW "authorize_valid";

ALTER TABLE "authorize" RENAME "access" to "inherit";
ALTER TABLE "authorize" RENAME "delegate" to "direct";
ALTER TABLE audit."authorize" RENAME "access" to "inherit";
ALTER TABLE audit."authorize" RENAME "delegate" to "direct";
COMMENT ON COLUMN "authorize"."inherit" IS 'Level of site/group access granted to child, inherited (but degraded) from parent';
COMMENT ON COLUMN "authorize"."direct" IS 'Permissions that child is granted directly on parent''s data';

UPDATE "authorize" SET "direct" = 'NONE' WHERE "child" = 0 AND "parent" = -1;

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

CREATE FUNCTION "authorize_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "authorize_inherit";;
	RETURN null;;
END;; $$;
CREATE TRIGGER "authorize_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "authorize" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();
CREATE TRIGGER "party_created" AFTER INSERT ON "party" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized <= CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE VIEW "authorize_view" ("child", "parent", "inherit", "direct") AS
	SELECT child, parent, MAX(inherit), MAX(direct)
	  FROM authorize_inherit
	 WHERE (authorized IS NULL OR authorized <= CURRENT_TIMESTAMP) AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';

CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH va AS (
		SELECT party, access, inherit
		  FROM volume_access 
		 WHERE volume = $1
	)
	SELECT access FROM (
		SELECT access 
		  FROM va
		 WHERE party = $2
	UNION ALL
		SELECT MAX(LEAST(va.access, ad.direct))
		  FROM va JOIN authorize_valid ad ON party = parent 
		 WHERE child = $2
	UNION ALL
		SELECT MAX(LEAST(va.inherit, ai.inherit))
		  FROM va JOIN authorize_view ai ON party = parent 
		 WHERE child = $2
	) a LIMIT 1
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer) IS 'Permission level the party has on the given volume, either directly, delegated, or inherited.';

# --- !Downs

DROP FUNCTION "volume_access_check" (integer, integer);
DROP VIEW "authorize_view";
DROP TRIGGER "party_created" ON "party";
DROP TRIGGER "authorize_changed" ON "authorize";
DROP FUNCTION "authorize_refresh" ();
DROP MATERIALIZED VIEW "authorize_inherit";
DROP VIEW "authorize_valid";

ALTER TABLE audit."authorize" RENAME "direct" to "delegate";
ALTER TABLE audit."authorize" RENAME "inherit" to "access";
ALTER TABLE "authorize" RENAME "direct" to "delegate";
ALTER TABLE "authorize" RENAME "inherit" to "access";
COMMENT ON COLUMN "authorize"."access" IS 'Level of independent site access granted to child (effectively minimum level on path to ROOT)';
COMMENT ON COLUMN "authorize"."delegate" IS 'Permissions for which child may act as parent (not inherited)';

UPDATE "authorize" SET "delegate" = 'ADMIN' WHERE "child" = 0 AND "parent" = -1;

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE FUNCTION "authorize_access_parents" (IN "child" integer, OUT "parent" integer, INOUT "access" permission = NULL) RETURNS SETOF RECORD LANGUAGE sql STABLE AS $$
	WITH RECURSIVE closure AS (
		SELECT $1 AS parent, enum_last(null::permission) AS access
		UNION
		SELECT p.parent, LEAST(p.access, c.access)
			FROM authorize_valid p, closure c
			WHERE p.child = c.parent AND ($2 IS NULL OR p.access >= $2)
	)
	SELECT * FROM closure
$$;
COMMENT ON FUNCTION "authorize_access_parents" (integer, permission) IS 'All ancestors (recursive) of a given child';

CREATE FUNCTION "authorize_access_check" ("child" integer, "parent" integer = 0, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT max(access) FROM authorize_access_parents($1, $3) WHERE parent = $2
$$;
COMMENT ON FUNCTION "authorize_access_check" (integer, integer, permission) IS 'Test if a given child inherits the given permission [any] from the given parent [root]';

CREATE FUNCTION "authorize_delegate_check" ("child" integer, "parent" integer, "delegate" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(delegate)) ELSE max(delegate) END FROM authorize_valid WHERE child = $1 AND parent = $2
$$;
COMMENT ON FUNCTION "authorize_delegate_check" (integer, integer, permission) IS 'Test if a given child has the given permission [any] over the given parent';

CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH sa AS (
		SELECT party, access, inherit
		  FROM volume_access 
		 WHERE volume = $1 AND ($3 IS NULL OR access >= $3)
	)
	SELECT max(access) FROM (
		SELECT access 
		  FROM sa
		 WHERE party = $2
	UNION ALL
		SELECT LEAST(sa.inherit, aap.access) 
		  FROM sa JOIN authorize_access_parents($2, $3) aap ON party = parent 
	UNION ALL
		SELECT LEAST(sa.access, ad.delegate)
		  FROM sa JOIN authorize_valid ad ON party = parent 
		 WHERE child = $2
	) a WHERE $3 IS NULL OR access >= $3
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer, permission) IS 'Test if a given party has the given permission [any] on the given volume, either directly, inherited through site access, or delegated.';
