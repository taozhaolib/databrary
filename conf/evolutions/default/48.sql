# --- !Ups

DROP INDEX "container_top_idx";
CREATE INDEX "container_top_idx" ON "container" ("volume") WHERE "top";

DROP FUNCTION "data_permission" (permission, consent, classification, permission, boolean);
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

# --- !Downs

DROP FUNCTION "data_permission" (permission, consent, classification, permission, boolean, boolean);
CREATE FUNCTION "data_permission" ("p" permission, "c" consent, "t" classification, "a" permission, "e" boolean = false) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
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
COMMENT ON FUNCTION "data_permission" (permission, consent, classification, permission, boolean) IS 'Effective permission level on a data object in a volume with the given permission, in a slot with the given consent, having the given classification, when the user has the given access permission level, when it''s marked as an excerpt.';

DROP INDEX "container_top_idx";
CREATE UNIQUE INDEX "container_top_idx" ON "container" ("volume") WHERE "top";

