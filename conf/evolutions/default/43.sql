# --- !Ups

CREATE AGGREGATE "segment_union" (segment) (SFUNC = range_union, STYPE = segment, INITCOND = 'empty');
CREATE AGGREGATE "segment_intersect" (segment) (SFUNC = range_intersect, STYPE = segment, INITCOND = '(,)');

# --- !Downs

DROP AGGREGATE "segment_union" (segment);
DROP AGGREGATE "segment_intersect" (segment);
