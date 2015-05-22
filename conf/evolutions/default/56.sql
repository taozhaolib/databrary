# --- !Ups

CREATE TABLE "funder" (
	"fundref_id" bigint NOT NULL Primary Key,
	"name" text NOT NULL,
	"party" integer References "party"
);
COMMENT ON TABLE "funder" IS 'Sources of funding, basically a mirror of fundref data, with local party associations (primarily for transition).';
COMMENT ON COLUMN "funder"."fundref_id" IS 'Identifiers from fundref.org, under the http://dx.doi.org/10.13039/ namespace. Specifications suggest these may not be numeric, but they seem to be.';

INSERT INTO funder VALUES (100000001, 'National Science Foundation');
INSERT INTO funder VALUES (100000002, 'National Institutes of Health');
INSERT INTO funder VALUES (100000071, 'National Institute of Child Health and Human Development');
INSERT INTO funder VALUES (100000073, 'Autism Speaks');
UPDATE funder SET party = party.id, name = party.name FROM party WHERE party.name LIKE funder.name || '%';
DELETE FROM funder WHERE party IS NULL;

CREATE TABLE "volume_funding" (
	"volume" integer NOT NULL References "volume",
	"funder" bigint NOT NULL References "funder",
	"awards" text[] NOT NULL Default '{}',
	Primary Key ("volume", "funder")
);
COMMENT ON TABLE "volume_funding" IS 'Funding sources associated with a volume, based on fundref.org.';
COMMENT ON COLUMN "volume_funding"."awards" IS 'Individual grant identifiers associated with this funder.';

INSERT INTO volume_funding SELECT volume, funder.fundref_id, string_to_array(funding, ', ') FROM volume_access JOIN funder USING (party) WHERE funding IS NOT NULL;
DELETE FROM volume_access USING funder WHERE volume_access.party = funder.party AND funding IS NOT NULL AND access = 'NONE' AND inherit = 'NONE';

ALTER TABLE audit."volume_access" DROP "funding";
ALTER TABLE "volume_access" DROP "funding";

# --- !Downs

ALTER TABLE "volume_access" ADD "funding" text;
ALTER TABLE audit."volume_access" ADD "funding" text;

INSERT INTO volume_access (volume, party, funding) SELECT volume, party, array_to_string(awards, ',') FROM volume_funding JOIN funder ON funder = funder.fundref_id WHERE party IS NOT NULL;

DROP TABLE "volume_funding";
DROP TABLE "funder";
