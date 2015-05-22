# --- !Ups

UPDATE metric SET name = 'ID' WHERE name = 'ident';

# --- !Downs

UPDATE metric SET name = 'ident' WHERE name = 'ID';
