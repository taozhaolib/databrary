# --- !Ups

CREATE FUNCTION "get_tag" ("tag_name" varchar(32)) RETURNS integer STRICT LANGUAGE plpgsql AS $$
DECLARE
	tag_id integer;;
BEGIN
	LOOP
		SELECT id INTO tag_id FROM tag WHERE name = tag_name;;
		IF FOUND THEN
			RETURN tag_id;;
		END IF;;
		BEGIN
			INSERT INTO tag (name) VALUES (tag_name) RETURNING id INTO tag_id;;
			RETURN tag_id;;
		EXCEPTION WHEN unique_violation THEN
		END;;
	END LOOP;;
END;; $$;

# --- !Downs

DROP FUNCTION "get_tag" (varchar(32));
