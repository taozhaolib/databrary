# --- !Ups

CREATE OR REPLACE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "e" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE 
		WHEN p > 'DOWNLOAD' OR p < 'VIEW' OR p IS NULL THEN p
		WHEN t > 'DEIDENTIFIED' OR p = 'DOWNLOAD' AND t >= CASE
			WHEN c >= 'PUBLIC' OR c >= 'SHARED' AND a >= 'DOWNLOAD' THEN 'IDENTIFIED'
			WHEN c >= 'EXCERPTS' THEN CASE WHEN e THEN 'IDENTIFIED' ELSE 'EXCERPT' END
			ELSE 'DEIDENTIFIED'
		END::classification THEN 'DOWNLOAD'
		ELSE 'VIEW'
	END
$$;

# --- !Downs

CREATE OR REPLACE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "e" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE STRICT AS $$
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
