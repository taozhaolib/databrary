# --- !Ups

ALTER TABLE "excerpt" DROP CONSTRAINT "excerpt_asset_fkey",
	ADD CONSTRAINT "excerpt_asset_fkey" FOREIGN KEY ("asset") References slot_asset ON UPDATE CASCADE ON DELETE CASCADE;

# --- !Downs

ALTER TABLE "excerpt" DROP CONSTRAINT "excerpt_asset_fkey",
	ADD CONSTRAINT "excerpt_asset_fkey" FOREIGN KEY ("asset") References slot_asset ON DELETE CASCADE;

