# --- !Ups
;

CREATE TABLE "entity" (
	"id" SERIAL NOT NULL Primary Key,
	"name" text NOT NULL
);
COMMENT ON TABLE "entity" IS 'Users, groups, organizations, and other logical identities';

-- ROOT entity (SERIAL starts at 1):
INSERT INTO "entity" VALUES (0, 'Databrary');


CREATE TABLE "account" (
	"entity" integer NOT NULL Unique References "entity",
	"username" varchar(32) NOT NULL Primary Key,
	"created" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"openid" varchar(256) -- split out (multiple/user)?
);
COMMENT ON TABLE "account" IS 'Login information for entities associated with registered individuals.';


CREATE TYPE permission AS ENUM ('NONE',
	'VIEW', -- study view, but no access to protected data (PUBLIC access)
	'DOWNLOAD', -- full read access to shared data (BROWSE access)
	'CONTRIBUTE', -- create and edit studies of own/target (FULL access)
	'ADMIN' -- perform administrative tasks on site/target such as changing permissions
);

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "entity" ON DELETE Cascade,
	"parent" integer NOT NULL References "entity",
	"access" permission NOT NULL Default 'NONE',
	"delegate" permission NOT NULL Default 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND "child" <> 0)
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between entities';
COMMENT ON COLUMN "authorize"."child" IS 'Entity granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Entity granting permissions';
COMMENT ON COLUMN "authorize"."access" IS 'Level of independent site access granted to child (effectively minimum level on path to ROOT)';
COMMENT ON COLUMN "authorize"."delegate" IS 'Permissions for which child may act as parent (not inherited)';

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE FUNCTION "authorize_access_parents" (IN "child" integer, OUT "parent" integer, INOUT "access" permission = NULL) RETURNS SETOF RECORD LANGUAGE sql STABLE AS $$
	WITH RECURSIVE closure AS (
		SELECT parent, access FROM authorize_valid WHERE child = $1 AND ($2 IS NULL OR access >= $2)
		UNION
		SELECT p.parent, LEAST(p.access, c.access)
			FROM authorize_valid p, closure c
			WHERE p.child = c.parent AND ($2 IS NULL OR p.access >= $2)
	)
	SELECT * FROM closure
$$;
COMMENT ON FUNCTION "authorize_access_parents" (integer, permission) IS 'All ancestors (recursive) of a given child';

CREATE FUNCTION "authorize_access_check" ("child" integer, "parent" integer = 0, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(access)) ELSE max(access) END FROM authorize_access_parents($1, $3) WHERE parent = $2
$$;
COMMENT ON FUNCTION "authorize_access_check" (integer, integer, permission) IS 'Test if a given child has the given permission [any] from the given parent [root]';

CREATE FUNCTION "authorize_delegate_check" ("child" integer, "parent" integer, "delegate" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(delegate)) ELSE max(delegate) END FROM authorize_valid WHERE child = $1 AND parent = $2
$$;
COMMENT ON FUNCTION "authorize_delegate_check" (integer, integer, permission) IS 'Test if a given child has the given permission [any] over the given parent';


# --- !Downs
;

DROP FUNCTION "authorize_delegate_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_parents" (integer, permission);
DROP VIEW "authorize_valid";
DROP TABLE "authorize";
DROP TYPE permission;
DROP TABLE "account";
DROP TABLE "entity";

