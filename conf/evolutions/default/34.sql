# --- !Ups

ALTER TABLE "token" ALTER "token" DROP DEFAULT;
DROP FUNCTION "random_string" (smallint, text);
DROP FUNCTION "slot_consent" (integer);
DROP VIEW "slot_nesting";
DROP FUNCTION CREATE_ABSTRACT_PARENT (name, name[]);

# --- !Downs

CREATE FUNCTION CREATE_ABSTRACT_PARENT ("parent" name, "children" name[]) RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	parent_table CONSTANT text := quote_ident(parent);;
	kind_type CONSTANT text := quote_ident(parent || '_kind');;
BEGIN
	EXECUTE $macro$
		CREATE TYPE $macro$ || kind_type || $macro$ AS ENUM ('$macro$ || array_to_string(children, $$','$$) || $macro$');;
		CREATE TABLE $macro$ || parent_table || $macro$ (
			"id" serial NOT NULL Primary Key,
			"kind" $macro$ || kind_type || $macro$ NOT NULL
		);;
		CREATE FUNCTION $macro$ || quote_ident(parent || '_trigger') || $macro$ () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
			IF TG_OP = 'INSERT' THEN
				INSERT INTO $macro$ || parent_table || $macro$ (id, kind) VALUES (NEW.id, TG_TABLE_NAME::$macro$ || kind_type || $macro$);;
			ELSIF TG_OP = 'DELETE' THEN
				DELETE FROM $macro$ || parent_table || $macro$ WHERE id = OLD.id AND kind = TG_TABLE_NAME::$macro$ || kind_type || $macro$;;
			ELSIF TG_OP = 'UPDATE' THEN
				IF NEW.id = OLD.id THEN
					RETURN NEW;;
				END IF;;
				UPDATE $macro$ || parent_table || $macro$ SET id = NEW.id WHERE id = OLD.id AND kind = TG_TABLE_NAME::$macro$ || kind_type || $macro$;;
			END IF;;
			IF NOT FOUND THEN
				RAISE EXCEPTION 'inconsistency for %:% parent $macro$ || parent || $macro$', TG_TABLE_NAME::$macro$ || kind_type || $macro$, OLD.id;;
			END IF;;
			IF TG_OP = 'DELETE' THEN
				RETURN OLD;;
			ELSE
				RETURN NEW;;
			END IF;;
		END;;$$
	$macro$;;
END;; $create$;
COMMENT ON FUNCTION CREATE_ABSTRACT_PARENT (name, name[]) IS 'A "macro" to create an abstract parent table and trigger function.  This could be done with a single function using dynamic EXECUTE but this way is more efficient and not much more messy.';

CREATE VIEW "slot_nesting" ("child", "parent", "consent") AS 
	SELECT c.id, p.id, p.consent FROM slot c JOIN slot p ON c <@ p;
COMMENT ON VIEW "slot_nesting" IS 'Transitive closure of slots containtained within other slots.';

CREATE FUNCTION "slot_consent" ("slot" integer) RETURNS consent LANGUAGE sql STABLE STRICT AS
	$$ SELECT consent FROM slot_nesting WHERE child = $1 AND consent IS NOT NULL $$;
COMMENT ON FUNCTION "slot_consent" (integer) IS 'Effective consent level on a given slot.';

CREATE FUNCTION "random_string" ("length" smallint, "charset" text = '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz') RETURNS varchar LANGUAGE sql VOLATILE STRICT AS
	$$ SELECT string_agg(substr($2, (length($2)*random()+0.5)::smallint, 1), '') FROM generate_series(1,$1) $$;
COMMENT ON FUNCTION "random_string" (smallint, text) IS 'Generate a random string of the given length drawn from the given list of characters.  This uses the postgres random function, which is not cryptographically secure.';

ALTER TABLE "token" ALTER "token" SET DEFAULT "random_string"(64::smallint);
