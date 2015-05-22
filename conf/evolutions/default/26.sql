# --- !Ups

-- In fact we added the precision annotation in a number of places, but this is
-- the only one that actually has an effect on postgres.  It is unfortunate
-- that segment does not use the restricted representation.
ALTER TABLE "asset" ALTER "duration" TYPE interval HOUR TO SECOND (3);
ALTER TABLE "audit"."asset" ALTER "duration" TYPE interval HOUR TO SECOND (3);

# --- !Downs

ALTER TABLE "asset" ALTER "duration" TYPE interval HOUR TO SECOND;
ALTER TABLE "audit"."asset" ALTER "duration" TYPE interval HOUR TO SECOND;
