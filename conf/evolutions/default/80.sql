# --- !Ups

DELETE FROM account WHERE id IN (2, 4);
DELETE FROM authorize WHERE child IN (2, 4) AND parent = 0;
UPDATE authorize SET site = 'ADMIN', member = 'ADMIN' WHERE child = 3 AND parent = 0;
UPDATE volume_access SET party = 3 WHERE volume = 1 AND party = 2;

# --- !Downs

INSERT INTO account (id, email) VALUES (2, 'mike@databrary.org');
INSERT INTO account (id, email) VALUES (4, 'andrea@databrary.org');
INSERT INTO authorize (child, parent, site, member) VALUES (2, 0, 'ADMIN', 'ADMIN');
UPDATE authorize SET site = 'EDIT', member = 'NONE' WHERE child = 3 AND parent = 0;
INSERT INTO authorize (child, parent, site, member) VALUES (4, 0, 'EDIT', 'NONE');
UPDATE volume_access SET party = 2 WHERE volume = 1 AND party = 3;
