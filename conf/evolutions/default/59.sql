# --- !Ups

ALTER TABLE audit.excerpt ALTER classification DROP DEFAULT;
COMMENT ON COLUMN excerpt.classification IS 'Override (by relaxing only) asset''s original classification.';

# --- !Downs

ALTER TABLE audit.excerpt ALTER classification SET DEFAULT 'PRIVATE';
COMMENT ON COLUMN excerpt.classification IS NULL;
