# --- !Ups
;

INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('image/png', 'png', 'Portable network graphics');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/csv', 'csv', 'Comma-separated values');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/html', 'html', 'Hypertext markup');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/rtf', 'rtf', 'Rich text format');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/msword', 'doc', 'Microsoft Word document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.text', 'odf', 'OpenDocument text');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.wordprocessingml.document', 'docx', 'Microsoft Word (Office Open XML) document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-excel', 'xls', 'Microsoft Excel spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.spreadsheet', 'ods', 'OpenDocument spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'xlsx', 'Microsoft Excel (Office Open XML) workbook');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-powerpoint', 'ppt', 'Microsoft PowerPoint presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.presentation', 'odp', 'OpenDocument presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.presentationml.presentation', 'pptx', 'Microsoft PowerPoint (Office Open XML) presentation');

INSERT INTO "metric" ("id", "name", "type", "values") VALUES (-550, 'race', 'text', ARRAY['American Indian or Alaska Native','Asian','Native Hawaiian or Other Pacific Islander','Black or African American','White','Multiple']);
INSERT INTO "metric" ("id", "name", "type", "values") VALUES (-540, 'ethnicity', 'text', ARRAY['Not Hispanic or Latino','Hispanic or Latino']);

INSERT INTO "record_template" ("category", "metric") VALUES (-500, -550);
INSERT INTO "record_template" ("category", "metric") VALUES (-500, -540);

# --- !Downs
;

DELETE FROM record_template WHERE metric IN (-550, -540);
DELETE FROM metric WHERE id IN (-550, -540);
DELETE FROM format WHERE id > 2;
SELECT setval('format_id_seq', 2);
