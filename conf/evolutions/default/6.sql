# --- !Ups

INSERT INTO "metric" ("id", "name", "type", "values") VALUES (-510, 'language', 'text');

# --- !Downs

DELETE FROM metric WHERE id = -510;
