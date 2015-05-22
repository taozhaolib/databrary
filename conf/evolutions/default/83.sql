# --- !Ups

DROP VIEW "volume_text";

CREATE TYPE release AS ENUM (
	'PRIVATE', 	-- no sharing beyond those with full access
	'SHARED', 	-- restricted sharing to authorized users
	'EXCERPTS', 	-- SHARED, but excerpts may be shown externally
	'PUBLIC' 	-- shared publically with anyone
);
COMMENT ON TYPE release IS 'Levels at which participants or researchers may choose to share data.';

DROP FUNCTION "read_classification" (permission, consent);
CREATE FUNCTION "read_release" ("p" permission) RETURNS release LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN p >= 'READ' THEN 'PRIVATE'::release -- should include NULL
		WHEN p >= 'SHARED' THEN 'SHARED'::release
		WHEN p >= 'PUBLIC' THEN 'PUBLIC'::release
	END
$$;
COMMENT ON FUNCTION "read_release" (permission) IS 'Minimum release level readable at the given permission level.';

DROP FUNCTION "read_permission" (classification, consent);
CREATE FUNCTION "read_permission" ("r" release) RETURNS permission LANGUAGE sql IMMUTABLE CALLED ON NULL INPUT AS $$
	SELECT CASE
		WHEN r >= 'PUBLIC' THEN 'PUBLIC'::permission
		WHEN r >= 'SHARED' THEN 'SHARED'::permission
		ELSE 'READ'::permission
	END
$$;
COMMENT ON FUNCTION "read_permission" (release) IS 'Minimum permission level to read a data object with the given release.';

DROP FUNCTION "check_permission" (permission, classification,  consent);
CREATE FUNCTION "check_permission" ("p" permission, "r" release) RETURNS boolean LANGUAGE sql IMMUTABLE AS $$
	SELECT p >= read_permission(r)
$$;
COMMENT ON FUNCTION "check_permission" (permission, release) IS 'Effective permission level on a data object with the given permission level and release.';

CREATE FUNCTION consent_release ("c" consent) RETURNS release LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN c = 'PRIVATE' THEN 'PRIVATE'::release
		WHEN c = 'SHARED' THEN 'SHARED'::release
		WHEN c = 'EXCERPTS' THEN 'EXCERPTS'::release
		WHEN c = 'PUBLIC' THEN 'PUBLIC'::release
	END
$$;

ALTER TABLE "slot_consent" RENAME TO "slot_release";
ALTER TABLE "slot_release" RENAME CONSTRAINT "slot_consent_pkey" TO "slot_release_pkey";
ALTER TABLE "slot_release" RENAME CONSTRAINT "slot_consent_singleton_segment_excl" TO "slot_release_singleton_segment_excl";
ALTER TABLE "slot_release" RENAME CONSTRAINT "slot_consent_container_fkey" TO "slot_release_container_fkey";
ALTER TABLE "slot_release" RENAME "consent" TO "release";
ALTER TABLE "slot_release" ALTER "release" TYPE release USING consent_release(release);

ALTER TABLE audit."slot_consent" RENAME TO "slot_release";
ALTER TABLE audit."slot_release" RENAME "consent" TO "release";
ALTER TABLE audit."slot_release" ALTER "release" TYPE release USING consent_release(release);

DROP FUNCTION consent_release (consent);

CREATE FUNCTION classification_release ("c" classification) RETURNS release LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN c = 'PRIVATE' THEN 'PRIVATE'::release
		WHEN c = 'RESTRICTED' THEN NULL::release
		WHEN c = 'SHARED' THEN 'EXCERPTS'::release
		WHEN c = 'PUBLIC' THEN 'PUBLIC'::release
	END
$$;

ALTER TABLE "asset" RENAME "classification" TO "release";
ALTER TABLE "asset" ALTER "release" DROP NOT NULL, ALTER "release" TYPE release USING classification_release(release);

ALTER TABLE audit."asset" RENAME "classification" TO "release";
ALTER TABLE audit."asset" ALTER "release" DROP NOT NULL, ALTER "release" TYPE release USING classification_release(release);

ALTER TABLE "metric" RENAME "classification" TO "release";
ALTER TABLE "metric" ALTER "release" DROP NOT NULL, ALTER "release" DROP DEFAULT, ALTER "release" TYPE release USING classification_release(release);

DROP FUNCTION classification_release (classification);

CREATE FUNCTION excerpt_release ("c" classification) RETURNS release LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN c = 'PRIVATE' THEN NULL::release
		WHEN c = 'RESTRICTED' THEN 'SHARED'::release
		WHEN c = 'SHARED' THEN 'EXCERPTS'::release
		WHEN c = 'PUBLIC' THEN 'PUBLIC'::release
	END
$$;

ALTER TABLE "excerpt" RENAME "classification" TO "release";
ALTER TABLE "excerpt" ALTER "release" DROP NOT NULL, ALTER "release" DROP DEFAULT, ALTER "release" TYPE release USING excerpt_release(release);
COMMENT ON COLUMN "excerpt"."release" IS 'Override (by relaxing only) asset''s original release.';
COMMENT ON TABLE "excerpt" IS 'Asset segments that have been selected for rerelease and/or top-level display.';

ALTER TABLE audit."excerpt" RENAME "classification" TO "release";
ALTER TABLE audit."excerpt" ALTER "release" DROP NOT NULL, ALTER "release" TYPE release USING excerpt_release(release);

DROP FUNCTION excerpt_release (classification);

DROP FUNCTION "record_consent" (integer);
CREATE FUNCTION "record_release" ("record" integer) RETURNS release LANGUAGE sql STABLE STRICT AS
	$$ SELECT MAX(release) FROM slot_record JOIN slot_release ON slot_record.container = slot_release.container AND slot_record.segment <@ slot_release.segment WHERE record = $1 $$;
COMMENT ON FUNCTION "record_release" (integer) IS 'Effective (maximal) release level granted on the specified record.';

CREATE FUNCTION "record_release_party" ("record" integer, "party" integer) RETURNS boolean LANGUAGE sql STABLE STRICT AS
	$$ SELECT check_permission(volume_access_check(volume, $2), record_release($1)) FROM record WHERE id = $1 $$;
COMMENT ON FUNCTION "record_release_party" (integer, integer) IS 'This function has no (good) reason for existing.';

CREATE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || sortname FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.release >= 'EXCERPTS'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here
COMMENT ON VIEW "volume_text" IS 'All (searchable) text data associated with a volume.';

DROP TYPE consent;
DROP TYPE classification;

# --- !Downs

DROP VIEW "volume_text";

CREATE TYPE consent AS ENUM (
	'PRIVATE', 	-- did not consent to any sharing
	'SHARED', 	-- consented to share on databrary
	'EXCERPTS', 	-- SHARED, but consented that excerpts may be "PUBLIC"
	'PUBLIC' 	-- consented to share openly
);
COMMENT ON TYPE consent IS 'Levels of sharing that participants may consent to.';

CREATE TYPE classification AS ENUM (
	'PRIVATE',	-- private data, never shared beyond those with full access
	'RESTRICTED', 	-- data containing HIPPA identifiers, requiring appropriate consent and authorization
	'SHARED',	-- available with any SHARED access
	'PUBLIC' 	-- available with any PUBLIC access
);
COMMENT ON TYPE classification IS 'Data (file or measure)-level settings affecting permissions.';

DROP FUNCTION "read_release" (permission);
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

DROP FUNCTION "read_permission" (release);
CREATE FUNCTION "read_permission" ("t" classification, "c" consent) RETURNS permission LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE
		WHEN t = 'PRIVATE' THEN 'READ'::permission
		WHEN t = 'PUBLIC' OR c >= 'PUBLIC' THEN 'PUBLIC'::permission
		WHEN t = 'SHARED' OR c >= 'SHARED' THEN 'SHARED'::permission
		ELSE 'READ'::permission
	END
$$;
COMMENT ON FUNCTION "read_permission" (classification, consent) IS 'Necessary permission level to read a data object with the given classification, in a slot with the given consent.';

DROP FUNCTION "check_permission" (permission, release);
CREATE FUNCTION "check_permission" ("p" permission, "t" classification, "c" consent) RETURNS boolean LANGUAGE sql IMMUTABLE AS $$
	SELECT p >= read_permission(t, c)
$$;
COMMENT ON FUNCTION "check_permission" (permission, classification, consent) IS 'Effective permission level on a data object with the given access level and classification, in a slot with the given consent.';

CREATE FUNCTION release_consent ("r" release) RETURNS consent LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN r = 'PRIVATE' THEN 'PRIVATE'::consent
		WHEN r = 'SHARED' THEN 'SHARED'::consent
		WHEN r = 'EXCERPTS' THEN 'EXCERPTS'::consent
		WHEN r = 'PUBLIC' THEN 'PUBLIC'::consent
	END
$$;

ALTER TABLE "slot_release" RENAME TO "slot_consent";
ALTER TABLE "slot_consent" RENAME CONSTRAINT "slot_release_pkey" TO "slot_consent_pkey";
ALTER TABLE "slot_consent" RENAME CONSTRAINT "slot_release_singleton_segment_excl" TO "slot_consent_singleton_segment_excl";
ALTER TABLE "slot_consent" RENAME CONSTRAINT "slot_release_container_fkey" TO "slot_consent_container_fkey";
ALTER TABLE "slot_consent" RENAME "release" TO "consent";
ALTER TABLE "slot_consent" ALTER "consent" TYPE consent USING release_consent(consent);

ALTER TABLE audit."slot_release" RENAME TO "slot_consent";
ALTER TABLE audit."slot_consent" RENAME "release" TO "consent";
ALTER TABLE audit."slot_consent" ALTER "consent" TYPE consent USING release_consent(consent);

DROP FUNCTION release_consent (release);

CREATE FUNCTION release_classification ("r" release) RETURNS classification LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE
		WHEN r IS NULL THEN 'RESTRICTED'::classification
		WHEN r = 'PRIVATE' THEN 'PRIVATE'::classification
		WHEN r = 'SHARED' THEN 'SHARED'::classification
		WHEN r = 'EXCERPTS' THEN 'SHARED'::classification
		WHEN r = 'PUBLIC' THEN 'PUBLIC'::classification
	END
$$;

ALTER TABLE "asset" RENAME "release" TO "classification";
ALTER TABLE "asset" ALTER "classification" TYPE classification USING release_classification(classification), ALTER "classification" SET NOT NULL;

ALTER TABLE audit."asset" RENAME "release" TO "classification";
ALTER TABLE audit."asset" ALTER "classification" TYPE classification USING release_classification(classification), ALTER "classification" SET NOT NULL;

ALTER TABLE "metric" RENAME "release" TO "classification";
ALTER TABLE "metric" ALTER "classification" TYPE classification USING release_classification(classification), ALTER "classification" SET NOT NULL;

DROP FUNCTION release_classification (release);

CREATE FUNCTION release_excerpt ("r" release) RETURNS classification LANGUAGE sql IMMUTABLE AS $$
	SELECT CASE
		WHEN r IS NULL THEN 'PRIVATE'::classification
		WHEN r = 'PRIVATE' THEN 'PRIVATE'::classification
		WHEN r = 'SHARED' THEN 'RESTRICTED'::classification
		WHEN r = 'EXCERPTS' THEN 'SHARED'::classification
		WHEN r = 'PUBLIC' THEN 'PUBLIC'::classification
	END
$$;

ALTER TABLE "excerpt" RENAME "release" TO "classification";
ALTER TABLE "excerpt" ALTER "classification" TYPE classification USING release_excerpt(classification), ALTER "classification" SET NOT NULL, ALTER "classification" SET DEFAULT 'PRIVATE';
COMMENT ON TABLE "excerpt" IS 'Asset segments that have been selected for reclassification to possible public release or top-level display.';
COMMENT ON COLUMN "excerpt"."classification" IS 'Override (by relaxing only) asset''s original classification.';

ALTER TABLE audit."excerpt" RENAME "release" TO "classification";
ALTER TABLE audit."excerpt" ALTER "classification" TYPE classification USING release_excerpt(classification), ALTER "classification" SET NOT NULL;

DROP FUNCTION release_excerpt (release);

DROP FUNCTION "record_release" (integer);
CREATE FUNCTION "record_consent" ("record" integer) RETURNS consent LANGUAGE sql STABLE STRICT AS
	$$ SELECT MIN(consent) FROM slot_record JOIN slot_consent ON slot_record.container = slot_consent.container AND slot_record.segment <@ slot_consent.segment WHERE record = $1 $$;
COMMENT ON FUNCTION "record_consent" (integer) IS 'Effective (minimal) consent level granted on the specified record.';

DROP FUNCTION "record_release_party" (integer, integer);

CREATE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || sortname FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
 	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.classification >= 'SHARED'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here
COMMENT ON VIEW "volume_text" IS 'All (searchable) text data associated with a volume.';

DROP TYPE release;
