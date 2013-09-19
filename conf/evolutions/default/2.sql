-- This populates some demo data to help in testing

# --- !Ups
;

INSERT INTO party (id, name, orcid) VALUES (1, 'Dylan Simon', '0000000227931679');
INSERT INTO party (id, name) VALUES (2, 'Mike Continues');
INSERT INTO party (id, name) VALUES (3, 'Lisa Steiger');
INSERT INTO party (id, name) VALUES (4, 'Andrea Byrne');
INSERT INTO party (id, name) VALUES (5, 'Karen Adolph');
INSERT INTO party (id, name) VALUES (6, 'Rick Gilmore');
SELECT setval('party_id_seq', 6);

INSERT INTO account (id, email, openid) VALUES (1, 'dylan@databrary.org', 'http://dylex.net/');
INSERT INTO account (id, email, openid) VALUES (2, 'mike@databrary.org', NULL);
INSERT INTO account (id, email, openid) VALUES (3, 'lisa@databrary.org', NULL);
INSERT INTO account (id, email, openid) VALUES (4, 'andrea@databrary.org', NULL);

INSERT INTO authorize (child, parent, access, delegate, authorized) VALUES (1, 0, 'ADMIN', 'ADMIN', '2013-3-1');
INSERT INTO authorize (child, parent, access, delegate, authorized) VALUES (2, 0, 'ADMIN', 'ADMIN', '2013-8-1');
INSERT INTO authorize (child, parent, access, delegate, authorized) VALUES (3, 0, 'CONTRIBUTE', 'NONE', '2013-4-1');
INSERT INTO authorize (child, parent, access, delegate, authorized) VALUES (4, 0, 'CONTRIBUTE', 'NONE', '2013-9-1');

INSERT INTO volume (id, name) VALUES (1, 'Databrary');
SELECT setval('volume_id_seq', 1);

INSERT INTO container (id, volume, name) VALUES (1, 1, 'Global');
SELECT setval('container_id_seq', 1);

INSERT INTO toplevel_slot (slot) VALUES (1);

INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, -1, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 2, 'ADMIN', 'NONE');

INSERT INTO timeseries (id, format, classification, duration) VALUES (1, -800, 'MATERIAL', interval '40');
SELECT setval('asset_id_seq', 1);

INSERT INTO container_asset (container, asset, position, name) VALUES (1, 1, '0', 'counting');

# --- !Downs
;

TRUNCATE party, volume, container, timeseries, asset CASCADE;
SELECT setval('party_id_seq', 1, false);
SELECT setval('container_id_seq', 1, false);
SELECT setval('slot_id_seq', 1, false);
SELECT setval('volume_id_seq', 1, false);
SELECT setval('asset_id_seq', 1, false);

-- Restore fixtures
INSERT INTO party VALUES (-1, 'Everybody');
INSERT INTO party VALUES (0, 'Databrary');
INSERT INTO authorize (child, parent, access, delegate) VALUES (0, -1, 'ADMIN', 'ADMIN');

