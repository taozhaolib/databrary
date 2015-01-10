# --- !Ups

CREATE FUNCTION "segments" (segment) RETURNS segment[] LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE WHEN isempty($1) THEN ARRAY[]::segment[] ELSE ARRAY[$1] END
$$;
-- this needs to be done as SU, so we use a placeholder:
CREATE FUNCTION segments_union(segment[], segment[]) RETURNS segment[] IMMUTABLE STRICT LANGUAGE
	sql AS $$ SELECT NULL::segment[] $$;
	-- C AS 'pgranges.so', 'ranges_union';
CREATE FUNCTION segments_union(segment[], segment) RETURNS segment[] IMMUTABLE STRICT LANGUAGE sql AS
	$$ SELECT segments_union($1, segments($2)) $$;
CREATE AGGREGATE "segments_union" (segment) (SFUNC = segments_union, STYPE = segment[], INITCOND = '{}');

# --- !Downs

DROP AGGREGATE "segments_union" (segment);
DROP FUNCTION segments_union(segment[], segment);
DROP FUNCTION segments_union(segment[], segment[]);
DROP FUNCTION "segments" (segment);
