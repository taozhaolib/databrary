-- This populates some demo data to help in testing
-- It should probably not be used in production, but could be.

# --- !Ups
;

INSERT INTO party (id, name) VALUES (1, 'Dylan Simon');
INSERT INTO party (id, name) VALUES (2, 'Alex Sokoloff');
INSERT INTO party (id, name) VALUES (3, 'Mike Continues');
INSERT INTO party (id, name) VALUES (4, 'Lisa Steiger');
INSERT INTO party (id, name) VALUES (5, 'Karen Adolph');
INSERT INTO party (id, name) VALUES (6, 'Rick Gilmore');
SELECT setval('party_id_seq', 6);

INSERT INTO account (id, username, email, openid) VALUES (1, 'dylan', 'dylan@databrary.org', 'http://dylex.net/');
INSERT INTO account (id, username, email, openid) VALUES (2, 'alex', 'alex@databrary.org', NULL);
INSERT INTO account (id, username, email, openid) VALUES (3, 'mike', 'mike@databrary.org', NULL);
INSERT INTO account (id, username, email, openid) VALUES (4, 'lisa', 'lisa@databrary.org', NULL);

INSERT INTO authorize (child, parent, access, delegate) VALUES (1, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (2, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (3, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (4, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (5, 0, 'CONTRIBUTE', 'NONE');
INSERT INTO authorize (child, parent, access, delegate) VALUES (6, 0, 'CONTRIBUTE', 'NONE');

INSERT INTO study (id, title) VALUES (1, 'Demo sandbox');
SELECT setval('container_id_seq', 1);

INSERT INTO study_access (study, party, access, inherit) VALUES (1, -1, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO study_access (study, party, access, inherit) VALUES (1, 0, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO study_access (study, party, access, inherit) VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO study_access (study, party, access, inherit) VALUES (1, 2, 'ADMIN', 'NONE');
INSERT INTO study_access (study, party, access, inherit) VALUES (1, 3, 'ADMIN', 'NONE');

INSERT INTO timeseries (id, format, classification, duration) VALUES (1, -2, 'MATERIAL', interval '40');
SELECT setval('asset_id_seq', 1);

INSERT INTO asset_link (container, asset, title) VALUES (1, 1, 'counting');

# --- !Downs
;

TRUNCATE party, study, container, timeseries, asset, asset_link CASCADE;
SELECT setval('party_id_seq', 1, 'f');
SELECT setval('container_id_seq', 1, 'f');
SELECT setval('asset_id_seq', 1, 'f');

INSERT INTO party VALUES (-1, 'Everybody');
INSERT INTO party VALUES (0, 'Databrary');
INSERT INTO authorize (child, parent, access) VALUES (0, -1, 'ADMIN');

