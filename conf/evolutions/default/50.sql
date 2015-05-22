# --- !Ups

CREATE INDEX "audit_login_idx" ON audit."audit" ("audit_user", "audit_time") WHERE "audit_action" IN ('attempt', 'open');
COMMENT ON INDEX audit."audit_login_idx" IS 'Allow efficient determination of recent login attempts for security.';

# --- !Downs

DROP INDEX audit."audit_login_idx";
