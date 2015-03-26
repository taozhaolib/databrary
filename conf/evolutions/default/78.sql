# --- !Ups

CREATE INDEX "authorize_activity_idx" ON audit."authorize" ("audit_time" DESC) WHERE "audit_action" IN ('add', 'change') AND "site" > 'NONE';
CREATE INDEX "volume_share_activity_idx" ON audit."volume_access" ("audit_time" DESC) WHERE "audit_action" = 'add' AND "party" = 0 AND "children" > 'NONE';

# --- !Downs

DROP INDEX audit."authorize_activity_idx";
DROP INDEX audit."volume_share_activity_idx";
