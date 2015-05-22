# --- !Ups

DROP VIEW "slot_asset";
CREATE VIEW "slot_asset" ("asset", "segment", "slot", "excerpt") AS
	SELECT asset_slot.asset, slot_asset.segment, slot.id, slot_excerpt.segment
	  FROM asset_slot 
	  JOIN slot AS slot_asset ON asset_slot.slot = slot_asset.id
	  JOIN slot ON slot_asset.source = slot.source AND slot_asset.segment && slot.segment
	  LEFT JOIN excerpt
	       JOIN slot AS slot_excerpt ON excerpt.slot = slot_excerpt.id
	       ON asset_slot.asset = excerpt.asset AND slot.source = slot_excerpt.source AND slot.segment <@ slot_excerpt.segment;
COMMENT ON VIEW "slot_asset" IS 'Expanded set of all slots and the assets they include.';

# --- !Downs

DROP VIEW "slot_asset";
CREATE VIEW "slot_asset" ("asset", "segment", "slot", "excerpt") AS
	SELECT asset_slot.asset, slot_asset.segment, slot.id, excerpt.asset IS NOT NULL
	  FROM asset_slot 
	  JOIN slot AS slot_asset ON asset_slot.slot = slot_asset.id
	  JOIN slot ON slot_asset.source = slot.source AND slot_asset.segment && slot.segment
	  LEFT JOIN excerpt
	       JOIN slot AS slot_excerpt ON excerpt.slot = slot_excerpt.id
	       ON asset_slot.asset = excerpt.asset AND slot.source = slot_excerpt.source AND slot.segment <@ slot_excerpt.segment;
COMMENT ON VIEW "slot_asset" IS 'Expanded set of all slots and the assets they include.';

