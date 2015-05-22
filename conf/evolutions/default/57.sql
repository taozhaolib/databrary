# --- !Ups

ALTER TABLE "party" ADD "url" text;
ALTER TABLE audit."party" ADD "url" text;

DROP TRIGGER "authorize_changed" ON "authorize";
DROP FUNCTION "data_permission" (permission, consent, classification, permission, boolean, boolean);
DROP VIEW "authorize_view";
DROP VIEW "authorize_valid";
DROP MATERIALIZED VIEW "authorize_inherit";

ALTER TYPE permission RENAME TO old_permission;
CREATE TYPE permission AS ENUM ('NONE',
	'PUBLIC', 	-- read access to metadata and PUBLIC data
	'SHARED',	-- read access to SHARED data
	'READ', 	-- full read access to all data
	'EDIT', 	-- view and edit all data
	'ADMIN' 	-- perform administrative tasks on site/target such as changing permissions
);
COMMENT ON TYPE permission IS 'Levels of access parties can have to the site data.';

ALTER TYPE classification RENAME TO old_classification;
CREATE TYPE classification AS ENUM (
	'PRIVATE',	-- private data, never shared beyond those with full access
	'RESTRICTED', 	-- data containing HIPPA identifiers, requiring appropriate consent and authorization
	'SHARED',	-- available with any SHARED access
	'PUBLIC' 	-- available with any PUBLIC access
);
COMMENT ON TYPE classification IS 'Data (file or measure)-level settings affecting permissions.';

CREATE FUNCTION "read_classification" ("p" permission, "c" consent) RETURNS classification LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE
		WHEN p >= 'READ' THEN 'PRIVATE'::classification
		WHEN p >= 'SHARED' THEN CASE
			WHEN c >= 'SHARED' THEN 'RESTRICTED'::classification
			ELSE 'SHARED'::classification
		END
		WHEN p >= 'PUBLIC' THEN CASE
			WHEN c >= 'PUBLIC' THEN 'RESTRICTED'::classification
			ELSE 'PUBLIC'::classification
		END
	END
$$;
COMMENT ON FUNCTION "read_classification" (permission, consent) IS 'Minimum classification level readable at the given permission level, in a slot with the given consent.';

CREATE FUNCTION "read_permission" ("t" classification, "c" consent) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE
		WHEN t = 'PRIVATE' THEN 'READ'::permission
		WHEN t = 'PUBLIC' OR c >= 'PUBLIC' THEN 'PUBLIC'::permission
		WHEN t = 'SHARED' OR c >= 'SHARED' THEN 'SHARED'::permission
		ELSE 'READ'::permission
	END
$$;
COMMENT ON FUNCTION "read_permission" (classification, consent) IS 'Necessary permission level to read a data object with the given classification, in a slot with the given consent.';

CREATE FUNCTION "check_permission" ("p" permission, "t" classification, "c" consent) RETURNS boolean LANGUAGE sql IMMUTABLE AS $$
	SELECT p >= read_permission(t, c)
$$;
COMMENT ON FUNCTION "check_permission" (permission, classification, consent) IS 'Effective permission level on a data object with the given access level and classification, in a slot with the given consent.';


ALTER TABLE "authorize" RENAME "inherit" TO "site";
ALTER TABLE "authorize" RENAME "direct" TO "member";
ALTER TABLE "authorize"
	ALTER "site" DROP DEFAULT,
	ALTER "member" DROP DEFAULT,
	ALTER "site" TYPE permission USING CASE
		WHEN site = 'NONE' OR authorized IS NULL THEN 'NONE'
		WHEN site = 'VIEW' THEN 'PUBLIC'
		WHEN site = 'DOWNLOAD' THEN 'SHARED'
		WHEN site = 'CONTRIBUTE' THEN 'EDIT'
		WHEN site = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "member" TYPE permission USING CASE
		WHEN member = 'NONE' OR authorized IS NULL THEN 'NONE'
		WHEN member = 'VIEW' THEN 'PUBLIC'
		WHEN member = 'DOWNLOAD' THEN 'SHARED'
		WHEN member = 'CONTRIBUTE' THEN 'EDIT'
		WHEN member = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "site" SET DEFAULT 'NONE',
	ALTER "member" SET DEFAULT 'NONE',
	DROP "authorized",
	DROP CONSTRAINT "authorize_check",
	ADD Check ("child" <> "parent" AND "child" > 0 AND "parent" >= 0);
COMMENT ON COLUMN "authorize"."site" IS 'Level of site access granted to child, inherited (but degraded) from parent';
COMMENT ON COLUMN "authorize"."member" IS 'Level of permission granted to the child as a member of the parent''s group';

ALTER TABLE audit."authorize" RENAME "inherit" TO "site";
ALTER TABLE audit."authorize" RENAME "direct" TO "member";
ALTER TABLE audit."authorize"
	ALTER "site" TYPE permission USING CASE
		WHEN site = 'NONE' OR authorized IS NULL THEN 'NONE'
		WHEN site = 'VIEW' THEN 'PUBLIC'
		WHEN site = 'DOWNLOAD' THEN 'SHARED'
		WHEN site = 'CONTRIBUTE' THEN 'EDIT'
		WHEN site = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "member" TYPE permission USING CASE
		WHEN member = 'NONE' OR authorized IS NULL THEN 'NONE'
		WHEN member = 'VIEW' THEN 'PUBLIC'
		WHEN member = 'DOWNLOAD' THEN 'SHARED'
		WHEN member = 'CONTRIBUTE' THEN 'EDIT'
		WHEN member = 'ADMIN' THEN 'ADMIN'
	END::permission,
	DROP "authorized";

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
CREATE TRIGGER "authorize_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "authorize" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP;
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE VIEW "authorize_view" ("child", "parent", "site", "member") AS
	SELECT child, parent, MAX(site), MAX(member)
	  FROM authorize_inherit
	 WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';


ALTER TABLE "volume_access" RENAME "access" TO "individual";
ALTER TABLE "volume_access" RENAME "inherit" TO "children";
ALTER TABLE "volume_access"
	DROP CONSTRAINT "volume_access_inherit_check",
	ALTER "individual" DROP DEFAULT,
	ALTER "children" DROP DEFAULT,
	ALTER "individual" TYPE permission USING CASE
		WHEN individual = 'NONE' THEN 'NONE'
		WHEN individual = 'VIEW' THEN 'PUBLIC'
		WHEN individual = 'DOWNLOAD' THEN 'SHARED'
		WHEN individual = 'CONTRIBUTE' THEN 'EDIT'
		WHEN individual = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "children" TYPE permission USING CASE
		WHEN children = 'NONE' THEN 'NONE'
		WHEN children = 'VIEW' THEN 'PUBLIC'
		WHEN children = 'DOWNLOAD' THEN 'SHARED'
		WHEN children = 'CONTRIBUTE' THEN 'EDIT'
		WHEN children = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "individual" SET DEFAULT 'NONE',
	ALTER "children" SET DEFAULT 'NONE',
	DROP CONSTRAINT "volume_access_check",
	ADD Check ("individual" >= "children");

ALTER TABLE audit."volume_access" RENAME "access" TO "individual";
ALTER TABLE audit."volume_access" RENAME "inherit" TO "children";
ALTER TABLE audit."volume_access"
	ALTER "individual" TYPE permission USING CASE
		WHEN individual = 'NONE' THEN 'NONE'
		WHEN individual = 'VIEW' THEN 'PUBLIC'
		WHEN individual = 'DOWNLOAD' THEN 'SHARED'
		WHEN individual = 'CONTRIBUTE' THEN 'EDIT'
		WHEN individual = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "children" TYPE permission USING CASE
		WHEN children = 'NONE' THEN 'NONE'
		WHEN children = 'VIEW' THEN 'PUBLIC'
		WHEN children = 'DOWNLOAD' THEN 'SHARED'
		WHEN children = 'CONTRIBUTE' THEN 'EDIT'
		WHEN children = 'ADMIN' THEN 'ADMIN'
	END::permission;

UPDATE volume_access SET individual = 'PUBLIC', children = 'PUBLIC' WHERE party = -1 AND volume <= 1;

DROP FUNCTION "volume_access_check" (integer, integer);
CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT access FROM (
		SELECT individual AS access
		  FROM volume_access
		 WHERE volume = $1 AND party = $2
	UNION ALL
		SELECT MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END)) AS access
		  FROM volume_access JOIN authorize_view ON party = parent
		 WHERE volume = $1 AND child = $2
	) a LIMIT 1
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer) IS 'Permission level the party has on the given volume, either directly, delegated, or inherited.';


ALTER TABLE "excerpt" ADD "classification" classification NOT NULL DEFAULT 'PRIVATE';
COMMENT ON TABLE "excerpt" IS 'Asset segments that have been selected for reclassification to possible public release or top-level display.';
ALTER TABLE audit."excerpt" ADD "classification" classification NOT NULL DEFAULT 'PRIVATE';

INSERT INTO excerpt SELECT asset, '(,)', CASE WHEN top THEN 'SHARED'::classification ELSE 'PRIVATE'::classification END FROM asset JOIN slot_asset ON id = asset JOIN container ON container = container.id WHERE classification = 'EXCERPT';

ALTER TABLE "asset"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'IDENTIFIED' OR classification = 'EXCERPT' THEN 'RESTRICTED'
		WHEN classification = 'MATERIAL' THEN 'PUBLIC'
		ELSE 'SHARED'
	END::classification;
ALTER TABLE audit."asset"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'IDENTIFIED' OR classification = 'EXCERPT' THEN 'RESTRICTED'
		WHEN classification = 'MATERIAL' THEN 'PUBLIC'
		ELSE 'SHARED'
	END::classification;


ALTER TABLE "metric"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'IDENTIFIED' OR classification = 'EXCERPT' THEN 'RESTRICTED'
		WHEN classification = 'MATERIAL' THEN 'PUBLIC'
		ELSE 'SHARED'
	END::classification;


DROP TYPE old_permission, old_classification;

DROP FUNCTION "asset_creation" (integer);

# --- !Downs

ALTER TABLE audit."party" DROP "url";
ALTER TABLE "party" DROP "url";

DROP TRIGGER "authorize_changed" ON "authorize";
DROP FUNCTION "read_classification" (permission, consent);
DROP FUNCTION "read_permission" (classification, consent);
DROP FUNCTION "check_permission" (permission, classification, consent);
DROP VIEW "authorize_view";
DROP VIEW "authorize_valid";
DROP MATERIALIZED VIEW "authorize_inherit";


ALTER TYPE permission RENAME TO new_permission;
CREATE TYPE permission AS ENUM ('NONE',
	'VIEW', -- list view, but no access to protected data (PUBLIC access)
	'DOWNLOAD', -- full read access to shared data (BROWSE access)
	'CONTRIBUTE', -- create and edit data of own/target (FULL access)
	'ADMIN' -- perform administrative tasks on site/target such as changing permissions
);
COMMENT ON TYPE permission IS 'Levels of access parties can have to the site data.';

ALTER TYPE classification RENAME TO new_classification;
CREATE TYPE classification AS ENUM (
	'IDENTIFIED', 	-- data containing HIPPA identifiers, requiring appropriate consent and DOWNLOAD permission
	'EXCERPT', 	-- IDENTIFIED data that has been selected as a releasable excerpt
	'DEIDENTIFIED', -- "raw" data which has been de-identified, requiring only DOWNLOAD permission
	'ANALYSIS', 	-- un/de-identified derived, generated, summarized, or aggregated data measures
	'PRODUCT',	-- research products such as results, summaries, commentaries, discussions, manuscripts, or articles
	'MATERIAL'	-- materials not derived from data, such as proposals, procedures, stimuli, manuals, (blank) forms, or documentation
);


ALTER TABLE "authorize" RENAME "site" TO "inherit";
ALTER TABLE "authorize" RENAME "member" TO "direct";
ALTER TABLE "authorize" RENAME "expires" TO "new_expires";
ALTER TABLE "authorize"
	ALTER "inherit" DROP DEFAULT,
	ALTER "direct" DROP DEFAULT,
	ALTER "inherit" TYPE permission USING CASE
		WHEN inherit = 'NONE' THEN 'NONE'
		WHEN inherit = 'PUBLIC' THEN 'VIEW'
		WHEN inherit = 'SHARED' THEN 'DOWNLOAD'
		WHEN inherit = 'READ' THEN 'DOWNLOAD'
		WHEN inherit = 'EDIT' THEN 'CONTRIBUTE'
		WHEN inherit = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "direct" TYPE permission USING CASE
		WHEN direct = 'NONE' THEN 'NONE'
		WHEN direct = 'PUBLIC' THEN 'VIEW'
		WHEN direct = 'SHARED' THEN 'DOWNLOAD'
		WHEN direct = 'READ' THEN 'DOWNLOAD'
		WHEN direct = 'EDIT' THEN 'CONTRIBUTE'
		WHEN direct = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "inherit" SET DEFAULT 'NONE',
	ALTER "direct" SET DEFAULT 'NONE',
	ADD "authorized" timestamp DEFAULT CURRENT_TIMESTAMP,
	ADD "expires" timestamp,
	DROP CONSTRAINT "authorize_check",
	ADD Check ("child" <> "parent" AND "child" > 0);
COMMENT ON COLUMN "authorize"."inherit" IS 'Level of site/group access granted to child, inherited (but degraded) from parent';
COMMENT ON COLUMN "authorize"."direct" IS 'Permissions that child is granted directly on parent''s data';

ALTER TABLE audit."authorize" RENAME "site" TO "inherit";
ALTER TABLE audit."authorize" RENAME "member" TO "direct";
ALTER TABLE audit."authorize" RENAME "expires" TO "new_expires";
ALTER TABLE audit."authorize"
	ALTER "inherit" TYPE permission USING CASE
		WHEN inherit = 'NONE' THEN 'NONE'
		WHEN inherit = 'PUBLIC' THEN 'VIEW'
		WHEN inherit = 'SHARED' THEN 'DOWNLOAD'
		WHEN inherit = 'READ' THEN 'DOWNLOAD'
		WHEN inherit = 'EDIT' THEN 'CONTRIBUTE'
		WHEN inherit = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "direct" TYPE permission USING CASE
		WHEN direct = 'NONE' THEN 'NONE'
		WHEN direct = 'PUBLIC' THEN 'VIEW'
		WHEN direct = 'SHARED' THEN 'DOWNLOAD'
		WHEN direct = 'READ' THEN 'DOWNLOAD'
		WHEN direct = 'EDIT' THEN 'CONTRIBUTE'
		WHEN direct = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ADD "authorized" timestamp,
	ADD "expires" timestamp;

UPDATE authorize SET authorized = CURRENT_TIMESTAMP WHERE inherit <> 'NONE' OR direct <> 'NONE';
UPDATE authorize SET expires = new_expires;

DO $do$ BEGIN
EXECUTE $$GRANT UPDATE ON TABLE audit.authorize TO $$ || quote_ident(current_user);;
UPDATE audit.authorize SET authorized = CURRENT_TIMESTAMP WHERE inherit <> 'NONE' OR direct <> 'NONE';;
UPDATE audit.authorize SET expires = new_expires;;
EXECUTE $$REVOKE UPDATE ON TABLE audit.authorize FROM $$ || quote_ident(current_user);;
END;; $do$;

ALTER TABLE "authorize" DROP "new_expires";
ALTER TABLE audit."authorize" DROP "new_expires";

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
CREATE TRIGGER "authorize_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "authorize" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE authorized <= CURRENT_TIMESTAMP AND (expires IS NULL OR expires > CURRENT_TIMESTAMP);
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE VIEW "authorize_view" ("child", "parent", "inherit", "direct") AS
	SELECT child, parent, MAX(inherit), MAX(direct)
	  FROM authorize_inherit
	 WHERE (authorized IS NULL OR authorized <= CURRENT_TIMESTAMP) AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';

CREATE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "excerpt" boolean = false, "top" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE 
		WHEN p > 'DOWNLOAD' OR p < 'VIEW' OR p IS NULL
			THEN p
		WHEN t > 'DEIDENTIFIED' OR p = 'DOWNLOAD' AND t >= CASE
			WHEN c >= 'PUBLIC'
				OR c >= 'SHARED' AND a >= 'DOWNLOAD'
				OR c >= 'EXCERPTS' AND excerpt
				THEN 'IDENTIFIED'
			WHEN c IS NULL AND top
				THEN 'EXCERPT'
			ELSE 	'DEIDENTIFIED'
		END::classification
			THEN 'DOWNLOAD'
		ELSE	'VIEW'
	END
$$;
COMMENT ON FUNCTION "data_permission" (permission, consent, classification, permission, boolean, boolean) IS 'Effective permission level on a data object in a volume with the given permission, in a slot with the given consent, having the given classification, when the user has the given access permission level, when it''s marked as an excerpt, when it''s in a top container.';


ALTER TABLE "volume_access" RENAME "individual" TO "access";
ALTER TABLE "volume_access" RENAME "children" TO "inherit";
ALTER TABLE "volume_access"
	ALTER "access" DROP DEFAULT,
	ALTER "inherit" DROP DEFAULT,
	ALTER "access" TYPE permission USING CASE
		WHEN access = 'NONE' THEN 'NONE'
		WHEN access = 'PUBLIC' THEN 'VIEW'
		WHEN access = 'SHARED' THEN 'DOWNLOAD'
		WHEN access = 'READ' THEN 'DOWNLOAD'
		WHEN access = 'EDIT' THEN 'CONTRIBUTE'
		WHEN access = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "inherit" TYPE permission USING CASE
		WHEN inherit = 'NONE' THEN 'NONE'
		WHEN inherit = 'PUBLIC' THEN 'VIEW'
		WHEN inherit = 'SHARED' THEN 'DOWNLOAD'
		WHEN inherit = 'READ' THEN 'DOWNLOAD'
		WHEN inherit = 'EDIT' THEN 'CONTRIBUTE'
		WHEN inherit = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "access" SET DEFAULT 'NONE',
	ALTER "inherit" SET DEFAULT 'NONE',
	ADD Check ("inherit" < 'ADMIN'),
	DROP CONSTRAINT "volume_access_check",
	ADD Check ("access" >= "inherit");

ALTER TABLE audit."volume_access" RENAME "individual" TO "access";
ALTER TABLE audit."volume_access" RENAME "children" TO "inherit";
ALTER TABLE audit."volume_access"
	ALTER "access" TYPE permission USING CASE
		WHEN access = 'NONE' THEN 'NONE'
		WHEN access = 'PUBLIC' THEN 'VIEW'
		WHEN access = 'SHARED' THEN 'DOWNLOAD'
		WHEN access = 'READ' THEN 'DOWNLOAD'
		WHEN access = 'EDIT' THEN 'CONTRIBUTE'
		WHEN access = 'ADMIN' THEN 'ADMIN'
	END::permission,
	ALTER "inherit" TYPE permission USING CASE
		WHEN inherit = 'NONE' THEN 'NONE'
		WHEN inherit = 'PUBLIC' THEN 'VIEW'
		WHEN inherit = 'SHARED' THEN 'DOWNLOAD'
		WHEN inherit = 'READ' THEN 'DOWNLOAD'
		WHEN inherit = 'EDIT' THEN 'CONTRIBUTE'
		WHEN inherit = 'ADMIN' THEN 'ADMIN'
	END::permission;

UPDATE volume_access SET access = 'DOWNLOAD', inherit = 'DOWNLOAD' WHERE party = -1 AND volume <= 1;

DROP FUNCTION "volume_access_check" (integer, integer);
CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	WITH v AS (
		SELECT party, access, inherit
		  FROM volume_access 
		 WHERE volume = $1
	)
	SELECT access FROM (
		SELECT access 
		  FROM v
		 WHERE party = $2
	UNION ALL
		SELECT MAX(GREATEST(LEAST(v.access, a.direct), LEAST(v.inherit, a.inherit)))
		  FROM v JOIN authorize_view a ON party = parent
		 WHERE child = $2
 	) a LIMIT 1
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer) IS 'Permission level the party has on the given volume, either directly, delegated, or inherited.';


ALTER TABLE "asset"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'RESTRICTED' THEN 'IDENTIFIED'
		WHEN classification = 'SHARED' THEN 'DEIDENTIFIED'
		WHEN classification = 'PUBLIC' THEN 'MATERIAL'
	END::classification;
ALTER TABLE audit."asset"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'RESTRICTED' THEN 'IDENTIFIED'
		WHEN classification = 'SHARED' THEN 'DEIDENTIFIED'
		WHEN classification = 'PUBLIC' THEN 'MATERIAL'
	END::classification;

ALTER TABLE "excerpt"
	DROP "classification";
COMMENT ON TABLE "excerpt" IS 'Slot asset segments that have been selected for possible public release and top-level display.';
ALTER TABLE audit."excerpt"
	DROP "classification";

UPDATE asset SET classification = 'EXCERPT' FROM excerpt WHERE classification = 'IDENTIFIED' AND id = asset AND segment = '(,)';
DELETE FROM excerpt USING asset WHERE classification = 'EXCERPT' AND id = asset AND segment = '(,)';


ALTER TABLE "metric"
	ALTER "classification" TYPE classification USING CASE
		WHEN classification = 'RESTRICTED' THEN 'IDENTIFIED'
		WHEN classification = 'SHARED' THEN 'DEIDENTIFIED'
		WHEN classification = 'PUBLIC' THEN 'MATERIAL'
	END::classification;


DROP TYPE new_permission, new_classification;

CREATE FUNCTION "asset_creation" ("asset" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."asset" WHERE "id" = $1 AND "audit_action" = 'add' $$;
