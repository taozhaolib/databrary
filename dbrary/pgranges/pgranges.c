/* This provides a function
 *   FUNCTION ranges_union(rangetype[], rangetype[]) RETURNS rangetype[] LANGUAGE C STRICT IMMUTABLE
 * that takes two sorted list of non-overlapping ranges (of any type) and unions them.
 * With it one can build a type representing any non-contiguous subset using rangetype[].
 */

#include <string.h>
#include <stdlib.h>
#include <postgres.h>
#include <fmgr.h>
#include <funcapi.h>
#include <access/tupmacs.h>
#include <utils/array.h>
#include <utils/rangetypes.h>
#include <utils/lsyscache.h>

PG_MODULE_MAGIC;

struct range {
	RangeBound l, u;
	bool e;
};

static void get_range(struct range *r, char **p, TypeCacheEntry *typcache, int16 len, bool byval, char align)
{
	Datum d = fetch_att(*p, byval, len);
	RangeType *t = DatumGetRangeType(d);
	range_deserialize(typcache, t, &r->l, &r->u, &r->e);
	if (r->e)
		ereport(ERROR, (errcode(ERRCODE_DATA_EXCEPTION), errmsg("empty range in ranges")));
	*p = att_addlength_pointer(*p, len, *p);
	*p = (char *)att_align_nominal(*p, align);
}

PG_FUNCTION_INFO_V1(ranges_union);
Datum ranges_union(PG_FUNCTION_ARGS);
Datum ranges_union(PG_FUNCTION_ARGS)
{
	ArrayType *aa = PG_GETARG_ARRAYTYPE_P(0);
	ArrayType *ba = PG_GETARG_ARRAYTYPE_P(1);
	ArrayType *ra;
	Oid typ;
	int16 typlen;
	bool typbyval;
	char typalign;
	TypeCacheEntry *typcache;
	int an, bn;
	int ai = 0, bi = 0, ri = 0;
	char *ap, *bp;
	Datum *rb;
	struct range a, b, r = { .e = true };

	if (ARR_NDIM(aa) > 1 || array_contains_nulls(aa) ||
	    ARR_NDIM(ba) > 1 || array_contains_nulls(ba) || 
	    ARR_ELEMTYPE(aa) != ARR_ELEMTYPE(ba))
		ereport(ERROR, (errcode(ERRCODE_DATA_EXCEPTION), errmsg("arguments must be one-dimensional non-null arrays of the same type")));

	if (ARR_NDIM(ba) < 1 || !(bn = *ARR_DIMS(ba)))
		PG_RETURN_ARRAYTYPE_P(aa);
	if (ARR_NDIM(aa) < 1 || !(an = *ARR_DIMS(aa)))
		PG_RETURN_ARRAYTYPE_P(ba);

	typ = ARR_ELEMTYPE(aa);
	get_typlenbyvalalign(typ, &typlen, &typbyval, &typalign);
	typcache = range_get_typcache(fcinfo, typ);
	rb = palloc0(sizeof(*rb)*(an + bn));

	ap = ARR_DATA_PTR(aa);
	bp = ARR_DATA_PTR(ba);

#define NEXT(C) \
	if (C##i < C##n) { \
		get_range(&C, &C##p, typcache, typlen, typbyval, typalign); \
		C##i++; \
	} else { \
		C.e = true; \
	}
	
	NEXT(a)
	NEXT(b)

	while (1) {
		if (r.e) {
			if (a.e && b.e)
				break;
			if (!a.e && (b.e || range_cmp_bounds(typcache, &a.l, &b.l) <= 0)) {
				r = a;
				NEXT(a)
			} else {
				r = b;
				NEXT(b)
			}
		}
		if (!a.e && (range_cmp_bounds(typcache, &r.u, &a.l) >= 0 || bounds_adjacent(typcache, r.u, a.l))) {
			if (range_cmp_bounds(typcache, &a.u, &r.u) > 0)
				r.u = a.u;
			NEXT(a)
		}
		else if (!b.e && (range_cmp_bounds(typcache, &r.u, &b.l) >= 0 || bounds_adjacent(typcache, r.u, b.l))) {
			if (range_cmp_bounds(typcache, &b.u, &r.u) > 0)
				r.u = b.u;
			NEXT(b)
		}
		else {
			RangeType *t = range_serialize(typcache, &r.l, &r.u, r.e);
			rb[ri ++] = RangeTypeGetDatum(t);
			r.e = true;
		}
	}

	ra = construct_array(rb, ri, typ, typlen, typbyval, typalign);
	pfree(rb);
	PG_RETURN_ARRAYTYPE_P(ra);
}

void _PG_init(void);
void _PG_init()
{
}
