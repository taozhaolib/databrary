# --- !Ups

CREATE FUNCTION "asset_supersede" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM next FROM asset_revision WHERE prev = asset_new;;
	IF FOUND THEN
		RAISE 'Asset % already superseded', asset_new;;
	END IF;;
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;;
	INSERT INTO asset_revision VALUES (asset_old, asset_new);;
END;; $$;

CREATE TABLE "transcode" (
	"asset" integer NOT NULL Primary Key References "asset" ON DELETE CASCADE,
	"owner" integer NOT NULL References "party",
	"start" timestamp Default now(),
	"process" integer,
	"result" text
);

# --- !Downs

DROP FUNCTION "asset_supersede" (integer, integer);
DROP TABLE "transcode";
