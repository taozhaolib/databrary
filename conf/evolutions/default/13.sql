# --- !Ups

DROP VIEW "record_ident_view";
DROP VIEW "record_participant_view";
CREATE VIEW "record_view" ("id", "volume", "category", "ident", "birthdate", "gender") AS
	SELECT record.*, ident.datum, birthdate.datum, gender.datum
	  FROM record
	       LEFT JOIN measure_text     ident ON id =     ident.record AND     ident.metric = -900
	       LEFT JOIN measure_date birthdate ON id = birthdate.record AND birthdate.metric = -590
	       LEFT JOIN measure_text    gender ON id =    gender.record AND    gender.metric = -580;
COMMENT ON VIEW "record_view" IS 'Records joined with their basic information.  This is temporary until better measure extraction is in place.';

# --- !Downs

DROP VIEW "record_view";
CREATE VIEW "record_ident_view" ("id", "volume", "category", "ident") AS
	SELECT record.*, ident.datum
	  FROM record
	       LEFT JOIN measure_text     ident ON record.id =     ident.record AND     ident.metric = -900;
COMMENT ON VIEW "record_ident_view" IS 'Records joined with their optional idents.';
CREATE VIEW "record_participant_view" ("id", "volume", "ident", "birthdate", "gender") AS
	SELECT id, volume, ident.datum, birthdate.datum, gender.datum
	  FROM record
	       LEFT JOIN measure_text     ident ON id =     ident.record AND     ident.metric = -900
	       LEFT JOIN measure_date birthdate ON id = birthdate.record AND birthdate.metric = -590
	       LEFT JOIN measure_text    gender ON id =    gender.record AND    gender.metric = -580
	 WHERE record.category = -500;
COMMENT ON VIEW "record_participant_view" IS 'Subject records joined with their optional basic information.';
