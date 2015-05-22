# --- !Ups

CREATE VIEW "measures" ("record", "measures") AS
	SELECT record, array_agg(metric || ':' || datum ORDER BY metric) FROM measure_view GROUP BY record;
COMMENT ON VIEW "measures" IS 'All measures for each record aggregated into a single array.';

DROP VIEW "record_view";

# --- !Downs

DROP VIEW "measures";

CREATE VIEW "record_view" ("id", "volume", "category", "ident", "birthdate", "gender") AS
	SELECT record.*, ident.datum, birthdate.datum, gender.datum
	  FROM record
	       LEFT JOIN measure_text     ident ON id =     ident.record AND     ident.metric = -900
	       LEFT JOIN measure_date birthdate ON id = birthdate.record AND birthdate.metric = -590
	       LEFT JOIN measure_text    gender ON id =    gender.record AND    gender.metric = -580;
COMMENT ON VIEW "record_view" IS 'Records joined with their basic information.  This is temporary until better measure extraction is in place.';
