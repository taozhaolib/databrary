# --- !Ups
;

CREATE TABLE "entity" (
	"id" SERIAL NOT NULL PRIMARY KEY,
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
COMMENT ON TABLE "account" IS 'Login information for entities associated with real, registered people.';


CREATE TYPE site_permission AS ENUM ('NONE', 'BROWSE', 'FULL', 'GRANT');
CREATE TYPE user_permission AS ENUM ('NONE', 'VIEW', 'EDIT', 'ADMIN');

CREATE TABLE "trust" (
	"child" integer NOT NULL References "entity" ON DELETE Cascade,
	"parent" integer NOT NULL References "entity",
	"access" site_permission NOT NULL Default 'NONE',
	"delegate" user_permission NOT NULL Default 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND "child" <> 0)
);
COMMENT ON TABLE "trust" IS 'Relationships and permissions granted between entities';
COMMENT ON COLUMN "trust"."child" IS 'Entity granted permissions';
COMMENT ON COLUMN "trust"."parent" IS 'Entity granting permissions';
COMMENT ON COLUMN "trust"."access" IS 'Level of site access inherited by child from parent';
COMMENT ON COLUMN "trust"."delegate" IS 'Direct permissions child has over parent (not inherited)';

CREATE VIEW "trust_valid" AS
	SELECT * FROM trust WHERE authorized < CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "trust_valid" IS 'Active records from "trust"';

CREATE FUNCTION "trust_access_parents" (IN "child" integer, OUT "parent" integer, INOUT "access" site_permission = NULL) RETURNS SETOF RECORD LANGUAGE sql STABLE AS $$
	WITH RECURSIVE closure AS (
		SELECT parent, access FROM trust_valid WHERE child = $1 AND ($2 IS NULL OR access >= $2)
		UNION
		SELECT p.parent, LEAST(p.access, c.access)
			FROM trust_valid p, closure c
			WHERE p.child = c.parent AND ($2 IS NULL OR p.access >= $2)
	)
	SELECT * FROM closure
$$;
COMMENT ON FUNCTION "trust_access_parents" (integer, site_permission) IS 'All ancestors (recursive) of a given child';

CREATE FUNCTION "trust_access_check" ("child" integer, "parent" integer = 0, "access" site_permission = NULL) RETURNS site_permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(access)) ELSE max(access) END FROM trust_access_parents($1, $3) WHERE parent = $2
$$;
COMMENT ON FUNCTION "trust_access_check" (integer, integer, site_permission) IS 'Test if a given child has the given permission [any] from the given parent [root]';

CREATE FUNCTION "trust_delegate_check" ("child" integer, "parent" integer, "delegate" user_permission = NULL) RETURNS user_permission LANGUAGE sql STABLE AS $$
	SELECT CASE WHEN $1 = $2 THEN enum_last(max(delegate)) ELSE max(delegate) END FROM trust_valid WHERE child = $1 AND parent = $2
$$;
COMMENT ON FUNCTION "trust_delegate_check" (integer, integer, user_permission) IS 'Test if a given child has the given permission [any] over the given parent';


# --- !Downs
;

DROP FUNCTION "trust_delegate_check" (integer, integer, user_permission);
DROP FUNCTION "trust_access_check" (integer, integer, site_permission);
DROP FUNCTION "trust_access_parents" (integer, site_permission);
DROP VIEW "trust_valid";
DROP TABLE "trust";
DROP TYPE user_permission;
DROP TYPE site_permission;
DROP TABLE "account";
DROP TABLE "entity";

