# --- !Ups

UPDATE ingest.asset SET file = substring(file FROM 1+length('/databrary/stage/')) WHERE file LIKE '/databrary/stage/%';

# --- !Downs

UPDATE ingest.asset SET file = '/databrary/stage/' || file WHERE file NOT LIKE '/%';
