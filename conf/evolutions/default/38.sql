# --- !Ups

CREATE OR REPLACE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
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

# --- !Downs

CREATE OR REPLACE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
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
