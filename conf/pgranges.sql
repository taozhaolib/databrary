CREATE FUNCTION segments_union(segment[], segment[]) RETURNS segment[] IMMUTABLE STRICT LANGUAGE
	sql AS $$ SELECT NULL::segment[] $$;
CREATE FUNCTION segments_union(segment[], segment) RETURNS segment[] IMMUTABLE STRICT LANGUAGE
	sql AS $$ SELECT segments_union($1, segments($2)) $$;
CREATE AGGREGATE "segments_union" (segment) (SFUNC = segments_union, STYPE = segment[], INITCOND = '{}');
