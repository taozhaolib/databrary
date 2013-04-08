# --- !Ups
;

CREATE TABLE "account" (
	"entity" integer NOT NULL Unique References "entity",
	"username" varchar(32) NOT NULL Primary Key,
	"email" varchar(256) NOT NULL,
	"created" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

# --- !Downs
;

DROP TABLE "account";
