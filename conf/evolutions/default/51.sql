# --- !Ups

UPDATE format SET name = 'JPEG image' WHERE id = -700;

DELETE FROM record_template WHERE category = -300 AND metric = -800;
UPDATE metric SET id = -650, name = 'summary' WHERE id = -800;

# --- !Downs

UPDATE format SET name = 'JPEG' WHERE id = -700;

UPDATE metric SET id = -800, name = 'title' WHERE id = -650;
INSERT INTO "record_template" ("category", "metric") VALUES (-300, -800);
