# --- !Ups

ALTER TABLE "tag" ADD Check ("name" ~ '^[a-z][-a-z ]+[a-z]$');

CREATE TABLE "keyword_use" (
	Primary Key ("tag", "container", "segment"),
	Exclude USING gist (singleton("tag") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("tag_use");
CREATE INDEX "keyword_use_slot_idx" ON "keyword_use" ("container", "segment");
COMMENT ON TABLE "keyword_use" IS 'Special "keyword" tags editable as volume data.';

# --- !Downs

ALTER TABLE "tag" DROP CONSTRAINT "tag_name_check";

DROP TABLE "keyword_use";
