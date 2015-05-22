# --- !Ups

ALTER TABLE "asset" ADD "sha1" bytea Check (octet_length("sha1") = 20);

# --- !Downs

ALTER TABLE "asset" DROP "sha1";
