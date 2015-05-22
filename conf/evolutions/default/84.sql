# --- !Ups

CREATE VIEW "volume_access_view" ("volume", "party", "access") AS
	SELECT volume, party, individual FROM volume_access
	UNION ALL
	SELECT volume, child, MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END))
	  FROM volume_access JOIN authorize_view ON party = parent GROUP BY volume, child;
COMMENT ON VIEW "volume_access_view" IS 'Expanded list of effective volume access.';

CREATE OR REPLACE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT access FROM volume_access_view WHERE volume = $1 AND party = $2 LIMIT 1
$$;

# --- !Downs

CREATE OR REPLACE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
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

DROP VIEW "volume_access_view";
