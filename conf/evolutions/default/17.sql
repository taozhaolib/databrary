# --- !Ups

CREATE INDEX "file_modification_idx" ON audit."file" ("id") WHERE "audit_action" IN ('add', 'change');
COMMENT ON INDEX audit."file_modification_idx" IS 'Allow efficient retrieval of file modification information, specifically date.';

CREATE FUNCTION "file_modification" ("file" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."file" WHERE "id" = $1 AND "audit_action" IN ('add', 'change') $$;

# --- !Downs

DROP FUNCTION "file_modification" (integer);
DROP INDEX audit."file_modification_idx";
