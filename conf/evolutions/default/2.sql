# --- !Ups
;

CREATE TYPE site_permission AS ENUM ('NONE', 'BROWSE', 'FULL', 'GRANT');
CREATE TYPE user_permission AS ENUM ('NONE', 'VIEW', 'EDIT', 'ADMIN');

CREATE TABLE "trust" (
	"child" integer NOT NULL References "entity",
	"parent" integer NOT NULL References "entity",
	"access" site_permission NOT NULL,
	"delegate" user_permission NOT NULL,
	"authorized" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("parent" <> "child")
);
COMMENT ON TABLE "trust" IS 'Relationships and permissions granted between entities';
COMMENT ON COLUMN "trust"."child" IS 'Entity granted permissions';
COMMENT ON COLUMN "trust"."parent" IS 'Entity granting permissions';
COMMENT ON COLUMN "trust"."access" IS 'Level of site access inherited by child from parent';
COMMENT ON COLUMN "trust"."delegate" IS 'Direct permissions child has over parent (not inherited)';

CREATE VIEW "trust_valid" AS
	SELECT * FROM trust WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP;
COMMENT ON VIEW "trust_valid" IS 'Non-expired records from "trust"';

CREATE FUNCTION "trust_parents" (IN "child" integer, OUT "parent" integer, INOUT "access" site_permission = NULL) RETURNS SETOF RECORD LANGUAGE sql STABLE AS $$
	WITH RECURSIVE closure AS (
		SELECT parent, access FROM trust_valid WHERE child = $1 AND ($2 IS NULL OR access >= $2)
		UNION
		SELECT p.parent, LEAST(p.access, c.access)
			FROM trust_valid p, closure c
			WHERE p.child = c.parent AND ($2 IS NULL OR p.access >= $2)
	)
	SELECT * FROM closure
$$;
COMMENT ON FUNCTION "trust_parents" (integer, site_permission) IS 'All ancestors (recursive) of a given child';

CREATE FUNCTION "trust_check" ("child" integer, "parent" integer = 0, "access" site_permission = NULL) RETURNS site_permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(access)) ELSE max(access) END FROM trust_parents($1, $3) WHERE parent = $2
$$;
COMMENT ON FUNCTION "trust_check" (integer, integer, site_permission) IS 'Test if a given child has the given permission [any] from the given parent [root]';

CREATE VIEW "trust_closure" ("child", "parent", "access") AS
	WITH RECURSIVE closure AS (
		SELECT child, parent, access FROM trust_valid
		UNION
		SELECT c.child, p.parent, LEAST(p.access, c.access)
			FROM trust_valid p, closure c
			WHERE p.child = c.parent
	)
	SELECT child, parent, max(access) FROM closure GROUP BY child, parent;
COMMENT ON VIEW "trust_closure" IS 'The full transitive closure of "trust"';

# --- !Downs
;

DROP VIEW "trust_closure";
DROP FUNCTION "trust_check" (integer, integer, site_permission);
DROP FUNCTION "trust_parents" (integer, site_permission);
DROP VIEW "trust_valid";
DROP TABLE "trust";
DROP TYPE user_permission;
DROP TYPE site_permission;
