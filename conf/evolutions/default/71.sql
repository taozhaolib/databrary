# --- !Ups

CREATE TABLE "volume_link" (
	"volume" integer NOT NULL References "volume",
	"head" text NOT NULL,
	"url" text NOT NULL,
	Unique ("volume", "url")
);
COMMENT ON TABLE "volume_link" IS 'Links from volumes to externals resources.';
SELECT audit.CREATE_TABLE ('volume_link');

# --- !Downs

DROP TABLE audit."volume_link";
DROP TABLE "volume_link";
