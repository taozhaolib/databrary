# --- !Ups

UPDATE metric SET options = ARRAY['Did not meet inclusion criteria','Procedural/experimenter error','Withdrew/fussy/tired','Outlier'] WHERE id = -700;
UPDATE metric SET options = ARRAY['Lab','Home','Classroom','Outdoor','Clinic'] WHERE id = -180;

UPDATE measure_text SET datum = 'Did not meet inclusion criteria' WHERE metric = -700 AND datum = 'did not meet criteria';
UPDATE measure_text SET datum = 'Procedural/experimenter error' WHERE metric = -700 AND datum = 'procedural/experimenter error';
UPDATE measure_text SET datum = 'Withdrew/fussy/tired' WHERE metric = -700 AND datum = 'withdrew/fussy/tired';
UPDATE measure_text SET datum = 'Outlier' WHERE metric = -700 AND datum = 'outlier';
UPDATE measure_text SET datum = 'Lab' WHERE metric = -180 AND datum = 'lab';
UPDATE measure_text SET datum = 'Home' WHERE metric = -180 AND datum = 'home';
UPDATE measure_text SET datum = 'Museum' WHERE metric = -180 AND datum = 'museum';
UPDATE measure_text SET datum = 'Classroom' WHERE metric = -180 AND datum = 'classroom';
UPDATE measure_text SET datum = 'Outdoor' WHERE metric = -180 AND datum = 'outdoor';
UPDATE measure_text SET datum = 'Clinic' WHERE metric = -180 AND datum = 'clinic';

# --- !Downs

UPDATE metric SET options = ARRAY['did not meet criteria','procedural/experimenter error','withdrew/fussy/tired','outlier'] WHERE id = -700;
UPDATE metric SET options = ARRAY['lab','home','museum','classroom','outdoor','clinic'] WHERE id = -180;

UPDATE measure_text SET datum = 'did not meet criteria' WHERE metric = -700 AND datum = 'Did not meet criteria';
UPDATE measure_text SET datum = 'procedural/experimenter error' WHERE metric = -700 AND datum = 'Procedural/experimenter error';
UPDATE measure_text SET datum = 'withdrew/fussy/tired' WHERE metric = -700 AND datum = 'Withdrew/fussy/tired';
UPDATE measure_text SET datum = 'outlier' WHERE metric = -700 AND datum = 'Outlier';
UPDATE measure_text SET datum = 'lab' WHERE metric = -180 AND datum = 'Lab';
UPDATE measure_text SET datum = 'home' WHERE metric = -180 AND datum = 'Home';
UPDATE measure_text SET datum = 'museum' WHERE metric = -180 AND datum = 'Museum';
UPDATE measure_text SET datum = 'classroom' WHERE metric = -180 AND datum = 'Classroom';
UPDATE measure_text SET datum = 'outdoor' WHERE metric = -180 AND datum = 'Outdoor';
UPDATE measure_text SET datum = 'clinic' WHERE metric = -180 AND datum = 'Clinic';

