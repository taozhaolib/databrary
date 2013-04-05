# --- !Ups
;

CREATE TABLE "entity" (
	"id" SERIAL NOT NULL PRIMARY KEY,
	"name" text NOT NULL
);
COMMENT ON TABLE "entity" IS 'Users, groups, organizations, and other logical identities';

INSERT INTO "entity" VALUES (0, 'Databrary');

# --- !Downs
;

DROP TABLE "entity";

