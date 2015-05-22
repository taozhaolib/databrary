# --- !Ups

UPDATE metric SET values = ARRAY['female','male'] WHERE id = -580;
UPDATE measure_text SET datum = 'female' WHERE datum = 'F' AND metric = -580;
UPDATE measure_text SET datum = 'male'   WHERE datum = 'M' AND metric = -580;

# --- !Downs

UPDATE metric SET values = ARRAY['F','M'] WHERE id = -580;
UPDATE measure_text SET datum = 'F' WHERE datum = 'female' AND metric = -580;
UPDATE measure_text SET datum = 'M' WHERE datum = 'male'   AND metric = -580;
