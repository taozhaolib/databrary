# --- !Ups

ALTER TABLE "party" ADD "affiliation" text;
ALTER TABLE audit."party" ADD "affiliation" text;

UPDATE party child SET affiliation = parent.name FROM authorize JOIN party parent ON parent = parent.id WHERE child = child.id AND access >= 'CONTRIBUTE' AND parent.id >= 0;
UPDATE party SET affiliation = 'New York University' WHERE id = 5;
UPDATE party SET affiliation = 'Penn State University' WHERE id = 6;

# --- !Downs

ALTER TABLE audit."party" DROP "affiliation";
ALTER TABLE "party" DROP "affiliation";
