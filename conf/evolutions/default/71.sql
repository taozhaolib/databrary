# --- !Ups

CREATE TABLE "volume_reference" (
	"volume" integer NOT NULL References "volume",
	"head" text NOT NULL,
	"url" text NOT NULL,
	Unique ("volume", "url")
);
COMMENT ON TABLE "volume_reference" IS 'Links from volumes to externals resources.';
SELECT audit.CREATE_TABLE ('volume_reference');

# --- !Downs

DROP TABLE audit."volume_reference";
DROP TABLE "volume_reference";
