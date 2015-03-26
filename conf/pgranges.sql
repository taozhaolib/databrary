CREATE OR REPLACE FUNCTION segments_union(segment[], segment[]) RETURNS segment[] IMMUTABLE STRICT LANGUAGE C AS 'pgranges.so', 'ranges_union';
CREATE OR REPLACE FUNCTION segments_union(segment[], segment) RETURNS segment[] IMMUTABLE STRICT LANGUAGE C AS 'pgranges.so', 'ranges_union1';
CREATE AGGREGATE "segments_union" (segment) (SFUNC = segments_union, STYPE = segment[], INITCOND = '{}');
