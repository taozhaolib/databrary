# --- !Ups

CREATE VIEW "asset_revisions" AS
	WITH RECURSIVE r AS (
		SELECT * FROM asset_revision
		UNION ALL
		SELECT asset_revision.prev, r.next FROM asset_revision JOIN r ON asset_revision.next = r.prev
	) SELECT * FROM r;
COMMENT ON VIEW "asset_revisions" IS 'Transitive closure of asset_revision.  Revisions must never form a cycle or this will not terminate.';

# --- !Downs

DROP VIEW "asset_revisions";
