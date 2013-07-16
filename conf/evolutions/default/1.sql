-- This is currently the complete, authoritative schema, as it is easier to
-- understand all in one place and unnecessary to use proper evolutions until
-- production.  Play only checks for changes in the most recent evolution, so
-- adding an empty 2.sql will prevent it from automatically applying changes to
-- this file, if desired.

-- Theoretically this could be maintained as authoritative during production,
-- with further evolutions only making defensive changes, but it may be easier
-- to keep separate.

-- Currently these are largely under-indexed.

# --- !Ups
;

CREATE TYPE audit_action AS ENUM ('login', 'logout', 'add', 'change', 'remove', 'download');
COMMENT ON TYPE audit_action IS 'The various activities for which we keep audit records (in audit or a derived table).';

CREATE TABLE "audit" (
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"who" int NOT NULL, -- References "account" ("entity"),
	"ip" inet NOT NULL,
	"action" audit_action NOT NULL
) WITH (OIDS = FALSE);
COMMENT ON TABLE "audit" IS 'Logs of all activities on the site, including access and modifications to any data. Each table has an associated audit table inheriting from this one.';


CREATE TABLE "entity" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"orcid" char(16)
);
COMMENT ON TABLE "entity" IS 'Users, groups, organizations, and other logical identities';

-- special entity (SERIAL starts at 1):
INSERT INTO "entity" VALUES (-1, 'Everybody'); -- NOBODY
INSERT INTO "entity" VALUES (0, 'Databrary'); -- ROOT

CREATE TABLE "audit_entity" (
	LIKE "entity"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "account" (
	"id" integer NOT NULL Primary Key References "entity",
	"username" varchar(32) NOT NULL Unique,
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"openid" varchar(256) -- split out (multiple/user)?
);
COMMENT ON TABLE "account" IS 'Login information for entities associated with registered individuals.';

CREATE TABLE "audit_account" (
	LIKE "account"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TYPE permission AS ENUM ('NONE',
	'VIEW', -- study view, but no access to protected data (PUBLIC access)
	'DOWNLOAD', -- full read access to shared data (BROWSE access)
	'CONTRIBUTE', -- create and edit studies of own/target (FULL access)
	'ADMIN' -- perform administrative tasks on site/target such as changing permissions
);

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "entity" ON DELETE Cascade,
	"parent" integer NOT NULL References "entity",
	"access" permission NOT NULL DEFAULT 'NONE',
	"delegate" permission NOT NULL DEFAULT 'NONE',
	"authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND ("child" > 0 OR "parent" = -1))
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between entities';
COMMENT ON COLUMN "authorize"."child" IS 'Entity granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Entity granting permissions';
COMMENT ON COLUMN "authorize"."access" IS 'Level of independent site access granted to child (effectively minimum level on path to ROOT)';
COMMENT ON COLUMN "authorize"."delegate" IS 'Permissions for which child may act as parent (not inherited)';

CREATE TABLE "audit_authorize" (
	LIKE "authorize"
) INHERITS ("audit") WITH (OIDS = FALSE);

-- To allow normal users to inherit from nobody:
INSERT INTO "authorize" ("child", "parent", "access") VALUES (0, -1, 'ADMIN', 'ADMIN');

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


CREATE TABLE "study" (
	"id" serial NOT NULL Primary Key,
	"title" text NOT NULL,
	"description" text
);
COMMENT ON TABLE "study" IS 'Basic organizational unit for data.';

CREATE TABLE "audit_study" (
	LIKE "study"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "study_access" (
	"study" integer NOT NULL References "study",
	"entity" integer NOT NULL References "entity",
	"access" permission NOT NULL DEFAULT 'NONE',
	"inherit" permission NOT NULL DEFAULT 'NONE' Check ("inherit" < 'ADMIN'),
	Check ("access" >= "inherit"),
	Primary Key ("study", "entity")
);
COMMENT ON TABLE "study_access" IS 'Permissions over studies assigned to users.';

CREATE TABLE "audit_study_access" (
	LIKE "study_access"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE FUNCTION "study_access_check" ("study" integer, "entity" integer, "access" permission = NULL) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH sa AS (
		SELECT entity, access, inherit
		  FROM study_access 
		 WHERE study = $1 AND ($3 IS NULL OR access >= $3)
	)
	SELECT max(access) FROM (
		SELECT access 
		  FROM sa
		 WHERE entity = $2
	UNION ALL
		SELECT LEAST(sa.inherit, aap.access) 
		  FROM sa JOIN authorize_access_parents($2, $3) aap ON entity = parent 
	UNION ALL
		SELECT LEAST(sa.access, ad.delegate)
		  FROM sa JOIN authorize_valid ad ON entity = parent 
		 WHERE child = $2
	) a WHERE $3 IS NULL OR access >= $3
$$;
COMMENT ON FUNCTION "study_access_check" (integer, integer, permission) IS 'Test if a given entity has the given permission [any] on the given study, either directly, inherited through site access, or delegated.';


CREATE TABLE "slot" (
	"id" serial NOT NULL Primary Key,
	"study" integer NOT NULL References "study",
	"ident" varchar(16) NOT NULL,
	Unique ("id", "study"), -- for FKs
	Unique ("study", "ident")
);
COMMENT ON TABLE "slot" IS 'Data container: organizational unit within study, usually corresponding to an individual participant.';


CREATE TYPE consent AS ENUM (
	-- required permission:	site		study
	'PUBLIC', 	-- 	-		VIEW		non-subject data
	'DEIDENTIFIED', -- 	-		VIEW		subject data without identifiers
	'EXCERPTS', 	-- 	DOWNLOAD	DOWNLOAD	SHARED, but consented that excerpts may be PUBLIC
	'SHARED', 	-- 	DOWNLOAD	DOWNLOAD	identified data authorized to be shared on databrary
	'PRIVATE' 	-- 	-		CONTRIBUTE	identified data not authorized for sharing
);
COMMENT ON TYPE consent IS 'Sensitivity levels that may apply to data according to the presence of protected identifiers and granted sharing level.  Does not necessarily map clearly to permission levels.';

CREATE TABLE "format" (
	"format" smallserial NOT NULL Primary Key,
	"mimetype" varchar(128) NOT NULL,
	"extension" varchar(8),
	"name" text NOT NULL, -- an awful name but convenient to be distinct from other object fields
	"timeseries" boolean NOT NULL Default FALSE
);
COMMENT ON TABLE "format" IS 'Possible types for objects, sufficient for producing download headers.';
COPY "format" (mimetype, extension, name) FROM STDIN;
text/plain	txt	Plain text
text/html	html	Hypertext markup
application/pdf	pdf	Portable document
image/jpeg	jpg	JPEG
\.

CREATE TABLE "object" (
	"id" serial NOT NULL Primary Key,
	"format" smallint NOT NULL References "format",
	"consent" consent NOT NULL,
	"date" date,
	"length" interval -- if format.timeseries
);
COMMENT ON TABLE "object" IS 'Objects in storage along with their "constant" metadata.';

CREATE TABLE "audit_object" (
	LIKE "object"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "excerpt" (
	"id" serial NOT NULL Primary Key,
	"object" integer NOT NULL References "object",
	"offset" interval NOT NULL,
	"length" interval,
	"public" boolean NOT NULL Default 'f' -- only if object.consent = EXCERPTS
);
COMMENT ON TABLE "excrept" IS 'Sections of timeseries objects selected for referencing.';

CREATE TABLE "audit_excerpt" (
	LIKE "excerpt"
) INHERITS ("audit") WITH (OIDS = FALSE);


CREATE TABLE "study_object" (
	"study" integer NOT NULL References "study",
	"object" integer NOT NULL References "object",
	"slot" integer References "slot", -- not properly normalized: either slot_object or common slot/study parent
	"title" text NOT NULL,
	"description" text,
	Primary Key ("study", "object"),
	Unique ("study", "object", "slot"), -- for FKs
	Foreign Key ("slot", "study") References "slot" ("id", "study")
);
COMMENT ON TABLE "study_object" IS 'Object linkages into studies along with "dynamic" metadata.';

CREATE TABLE "audit_study_object" (
	LIKE "study_object"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "study_excerpt" ( -- unfortunately redundant with study_object: common object/excerpt parent?
	"study" integer NOT NULL References "study",
	"excerpt" integer NOT NULL References "excerpt",
	"slot" integer References "slot", -- see above
	"title" text NOT NULL,
	Primary Key ("study", "excerpt"),
	Unique ("study", "excerpt", "slot"), -- for FKs
	Foreign Key ("slot", "study") References "slot" ("id", "study")
);
COMMENT ON TABLE "study_excerpt" IS 'Specific excerpts selected to highlight a study.';

CREATE TABLE "audit_study_excerpt" (
	LIKE "study_excerpt"
) INHERITS ("audit") WITH (OIDS = FALSE);

CREATE TABLE "annotation" ( -- ABSTRACT
	"id" serial NOT NULL Primary Key,
	"who" integer NOT NULL References "entity",
	"when" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	-- horribly unnormalized (hence FK mess); will be fixed by study/slot/object normalization above
	"study" integer NOT NULL References "study",
	"slot" integer References "slot",
	"object" integer References "object",
	"excerpt" integer References "excerpt",,
	-- even these are incomplete due to lack of MATCH PARTIAL; refactor
	Foreign Key ("slot", "study") References "slot" ("id", "study"),
	Foreign Key ("study", "object") References "study_object",
	Foreign Key ("study", "object", "slot") References "study_object" ("study", "object", "slot"),
	Foreign Key ("excerpt", "object") References "excerpt" ("id", "object")
	Foreign Key ("study", "excerpt") References "study_excerpt",
	Foreign Key ("study", "excerpt", "slot") References "study_excerpt" ("study", "excerpt", "slot"),
);
COMMENT ON TABLE "annotation" IS 'Abstract base table for various types of annotations that can be added by users to nodes (unaudited, no updates).';

CREATE TABLE "comment" (
	"text" text NOT NULL,
	Primary Key ("id"),
	Foreign Key ("slot", "study") References "slot" ("id", "study"),
	Foreign Key ("study", "object") References "study_object",
	Foreign Key ("study", "object", "slot") References "study_object" ("study", "object", "slot"),
	Foreign Key ("excerpt", "object") References "excerpt" ("id", "object")
	Foreign Key ("study", "excerpt") References "study_excerpt",
	Foreign Key ("study", "excerpt", "slot") References "study_excerpt" ("study", "excerpt", "slot"),
) INHERITS ("annotation");
COMMENT ON TABLE "comment" IS 'Free-text comments.';

# --- !Downs
;

DROP TABLE "comment";
DROP TABLE "annotation";
DROP TABLE "audit_study_excerpt";
DROP TABLE "study_excerpt";
DROP TABLE "audit_excerpt";
DROP TABLE "excerpt";
DROP TABLE "audit_study_object";
DROP TABLE "study_object";
DROP TABLE "audit_object";
DROP TABLE "object";
DROP TABLE "format";
DROP TYPE consent;

DROP TABLE "slot";
DROP FUNCTION "study_access_check" (integer, integer, permission);
DROP TABLE "study_access";
DROP TABLE "study";

DROP FUNCTION "authorize_delegate_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_check" (integer, integer, permission);
DROP FUNCTION "authorize_access_parents" (integer, permission);
DROP VIEW "authorize_valid";
DROP TABLE "audit_authorize";
DROP TABLE "authorize";
DROP TYPE permission;
DROP TABLE "audit_account";
DROP TABLE "account";
DROP TABLE "audit_entity";
DROP TABLE "entity";

DROP TABLE "audit";
DROP TYPE audit_action;

