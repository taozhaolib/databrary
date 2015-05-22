# --- !Ups

CREATE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "e" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE 
		WHEN p > 'DOWNLOAD' OR p < 'VIEW' THEN p
		WHEN t > 'DEIDENTIFIED' OR p = 'DOWNLOAD' AND t >= CASE
			WHEN c >= 'PUBLIC' OR c >= 'SHARED' AND a >= 'DOWNLOAD' THEN 'IDENTIFIED'
			WHEN c >= 'EXCERPTS' THEN CASE WHEN e THEN 'IDENTIFIED' ELSE 'EXCERPT' END
			ELSE 'DEIDENTIFIED'
		END::classification THEN 'DOWNLOAD'
		ELSE 'VIEW'
	END
$$;
COMMENT ON FUNCTION "data_permission" (permission, consent, classification, permission, boolean) IS 'Effective permission level on a data object in a volume with the given permission, in a slot with the given consent, having the given classification, when the user has the given access permission level, when it''s marked as an excerpt.';

# --- !Downs

DROP FUNCTION "data_permission" (permission, consent, classification, permission, boolean);
