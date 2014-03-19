# --- !Ups

CREATE TABLE "volume_inclusion" (
	"volume" integer NOT NULL References "volume",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("volume", "container") -- this may be too strong but seems reasonable
) INHERITS ("slot");
COMMENT ON TABLE "volume_inclusion" IS 'Inclusions of slots (sessions) from "dataset" (provider) volumes in "study" (consumer/reuse) volumes.';

SELECT audit.CREATE_TABLE ('volume_inclusion');

ALTER TABLE "record"
	DROP CONSTRAINT "record_category_fkey",
	ADD Foreign Key ("category") References "record_category" ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE "record_template"
	DROP CONSTRAINT "record_template_category_fkey",
	DROP CONSTRAINT "record_template_metric_fkey",
	ADD Foreign Key ("category") References "record_category" ON UPDATE CASCADE ON DELETE CASCADE,
	ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE ON DELETE CASCADE;

ALTER TABLE "measure"
	DROP CONSTRAINT "measure_metric_fkey",
	ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_text"
	DROP CONSTRAINT "measure_text_metric_fkey",
	ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_number"
	DROP CONSTRAINT "measure_number_metric_fkey",
	ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_date"
	DROP CONSTRAINT "measure_date_metric_fkey",
	ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;

# --- !Downs

ALTER TABLE "record"
	DROP CONSTRAINT "record_category_fkey",
	ADD Foreign Key ("category") References "record_category" ON DELETE SET NULL;
ALTER TABLE "record_template"
	DROP CONSTRAINT "record_template_category_fkey",
	DROP CONSTRAINT "record_template_metric_fkey",
	ADD Foreign Key ("category") References "record_category" ON DELETE CASCADE,
	ADD Foreign Key ("metric") References "metric";

ALTER TABLE "measure"
	DROP CONSTRAINT "measure_metric_fkey",
	ADD Foreign Key ("metric") References "metric";
ALTER TABLE "measure_text"
	DROP CONSTRAINT "measure_text_metric_fkey",
	ADD Foreign Key ("metric") References "metric";
ALTER TABLE "measure_number"
	DROP CONSTRAINT "measure_number_metric_fkey",
	ADD Foreign Key ("metric") References "metric";
ALTER TABLE "measure_date"
	DROP CONSTRAINT "measure_date_metric_fkey",
	ADD Foreign Key ("metric") References "metric";

DROP TABLE audit."volume_inclusion";

DROP TABLE "volume_inclusion";
