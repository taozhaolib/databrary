-- This populates some demo data to help in testing
-- It should probably not be used in production, but could be.

# --- !Ups
;

INSERT INTO "entity" ("id", "name") VALUES (1, 'Dylan Simon');
INSERT INTO "entity" ("id", "name") VALUES (2, 'Alex Sokoloff');
INSERT INTO "entity" ("id", "name") VALUES (3, 'Mike Continues');
INSERT INTO "entity" ("id", "name") VALUES (4, 'Lisa Steiger');
INSERT INTO "entity" ("id", "name") VALUES (5, 'Karen Adolph');
INSERT INTO "entity" ("id", "name") VALUES (6, 'Rick Gilmore');
SELECT setval('entity_id_seq', 6);

INSERT INTO "account" ("id", "username", "email", "openid") VALUES (1, 'dylan', 'dylan@databrary.org', 'http://dylex.net/');
INSERT INTO "account" ("id", "username", "email", "openid") VALUES (2, 'alex', 'alex@databrary.org', NULL);
INSERT INTO "account" ("id", "username", "email", "openid") VALUES (3, 'mike', 'mike@databrary.org', NULL);
INSERT INTO "account" ("id", "username", "email", "openid") VALUES (4, 'lisa', 'lisa@databrary.org', NULL);

INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (1, 0, 'ADMIN', 'ADMIN');
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (2, 0, 'ADMIN', 'ADMIN');
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (3, 0, 'ADMIN', 'ADMIN');
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (4, 0, 'ADMIN', 'ADMIN');
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (5, 0, 'CONTRIBUTE', 'NONE');
INSERT INTO "authorize" ("child", "parent", "access", "delegate") VALUES (6, 0, 'CONTRIBUTE', 'NONE');

INSERT INTO "study" ("id", "title") VALUES (1, 'Demo sandbox');
INSERT INTO "slot" ("id", "study", "ident") VALUES (2, 1, 'S1');
SELECT setval('container_id_seq', 2);

INSERT INTO "study_access" ("study", "entity", "access", "inherit") VALUES (1, -1, 'VIEW', 'VIEW');
INSERT INTO "study_access" ("study", "entity", "access", "inherit") VALUES (1, 0, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO "study_access" ("study", "entity", "access", "inherit") VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO "study_access" ("study", "entity", "access", "inherit") VALUES (1, 2, 'ADMIN', 'NONE');
INSERT INTO "study_access" ("study", "entity", "access", "inherit") VALUES (1, 3, 'ADMIN', 'NONE');

# --- !Downs
;

TRUNCATE "entity", "study", "container" CASCADE;
SELECT setval('entity_id_seq', 1, 'f');
SELECT setval('container_id_seq', 1, 'f');

INSERT INTO "entity" VALUES (-1, 'Everybody');
INSERT INTO "entity" VALUES (0, 'Databrary');
INSERT INTO "authorize" ("child", "parent", "access") VALUES (0, -1, 'ADMIN');

