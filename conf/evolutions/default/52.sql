# --- !Ups

ALTER TABLE "authorize"
	DROP CONSTRAINT "authorize_check",
	ADD Check ("child" <> "parent" AND "child" > 0);

CREATE TABLE audit."analytic" (
	"route" text NOT NULL,
	"data" json NOT NULL
) INHERITS (audit."audit");
COMMENT ON TABLE audit."analytic" IS 'Analytics data collected and reported by the browser.';

# --- !Downs

ALTER TABLE "authorize"
	DROP CONSTRAINT "authorize_check",
	ADD Check ("child" <> "parent" AND ("child" > 0 OR "parent" = -1));

DROP TABLE audit."analytic";
