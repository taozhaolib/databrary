# --- !Ups
;

CREATE TABLE "account" (
	"entity" integer NOT NULL Unique References "entity",
	"username" varchar(32) NOT NULL Primary Key,
	"created" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"email" varchar(256) NOT NULL,
	"openid" varchar(256)
);

# --- !Downs
;

DROP TABLE "account";
