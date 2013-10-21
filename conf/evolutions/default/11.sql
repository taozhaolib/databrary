# --- !Ups

UPDATE metric SET values = ARRAY['Female','Male'] WHERE id = -580;
UPDATE measure_text SET datum = 'Female' WHERE datum = 'female' AND metric = -580;
UPDATE measure_text SET datum = 'Male'   WHERE datum = 'male'   AND metric = -580;

# --- !Downs

UPDATE metric SET values = ARRAY['female','male'] WHERE id = -580;
UPDATE measure_text SET datum = 'female' WHERE datum = 'Female' AND metric = -580;
UPDATE measure_text SET datum = 'male'   WHERE datum = 'Male'   AND metric = -580;
