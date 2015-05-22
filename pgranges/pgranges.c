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
	Datum d;
	RangeBound l, u;
	bool e;
};

static Datum get_datum(char **p, int *i, int n, TypeCacheEntry *typcache)
{
	Datum d;
	if (*i >= n)
		return 0;
	d = fetch_att(*p, typcache->typbyval, typcache->typlen);
	*p = att_addlength_pointer(*p, typcache->typlen, *p);
	*p = (char *)att_align_nominal(*p, typcache->typalign);
	(*i) ++;
	return d;
}

static bool get_range(struct range *r, char **p, int *i, int n, TypeCacheEntry *typcache)
{
	RangeType *t;

	r->d = get_datum(p, i, n, typcache);
	if (!r->d) {
		r->e = true;
		return false;
	}
	t = DatumGetRangeType(r->d);
	range_deserialize(typcache, t, &r->l, &r->u, &r->e);
	if (r->e)
		ereport(ERROR, (errcode(ERRCODE_DATA_EXCEPTION), errmsg("empty range in ranges")));
	return true;
}

static Datum put_range(struct range *r, TypeCacheEntry *typcache)
{
	RangeType *t;

	if (r->d)
		return r->d;
	t = range_serialize(typcache, &r->l, &r->u, r->e);
	return r->d = RangeTypeGetDatum(t);
}

static bool bounds_overlap(TypeCacheEntry *typcache, RangeBound *u, RangeBound *l) {
	return range_cmp_bounds(typcache, u, l) >= 0 || bounds_adjacent(typcache, *u, *l);
}

#define NEXTD(C) \
	(C.d = get_datum(&C##p, &C##i, C##n, typcache))
#define NEXT(C) \
	get_range(&C, &C##p, &C##i, C##n, typcache)
	
PG_FUNCTION_INFO_V1(ranges_union);
Datum ranges_union(PG_FUNCTION_ARGS);
Datum ranges_union(PG_FUNCTION_ARGS)
{
	ArrayType *aa = PG_GETARG_ARRAYTYPE_P(0);
	ArrayType *ba = PG_GETARG_ARRAYTYPE_P(1);
	ArrayType *ra;
	Oid typ;
	TypeCacheEntry *typcache;
	int an, bn;
	int ai = 0, bi = 0, ri = 0;
	char *ap, *bp;
	Datum *rb;
	struct range a, b, r = { .e = true };

	typ = ARR_ELEMTYPE(aa);
	if (ARR_NDIM(aa) > 1 || array_contains_nulls(aa) ||
	    ARR_NDIM(ba) > 1 || array_contains_nulls(ba) || ARR_ELEMTYPE(ba) != typ)
		ereport(ERROR, (errcode(ERRCODE_DATA_EXCEPTION), errmsg("ranges must be one-dimensional non-null arrays of the same type")));

	if (ARR_NDIM(ba) < 1 || !(bn = *ARR_DIMS(ba)))
		PG_RETURN_ARRAYTYPE_P(aa);
	if (ARR_NDIM(aa) < 1 || !(an = *ARR_DIMS(aa)))
		PG_RETURN_ARRAYTYPE_P(ba);

	typcache = range_get_typcache(fcinfo, typ);
	rb = palloc0(sizeof(*rb)*(an + bn));

	ap = ARR_DATA_PTR(aa);
	bp = ARR_DATA_PTR(ba);

	NEXT(a);
	NEXT(b);

	while (1) {
		if (r.e) {
			if (a.e && b.e)
				break;
			if (!a.e && (b.e || range_cmp_bounds(typcache, &a.l, &b.l) <= 0)) {
				r = a;
				NEXT(a);
			} else {
				r = b;
				NEXT(b);
			}
		}
		if (!a.e && bounds_overlap(typcache, &r.u, &a.l)) {
			if (range_cmp_bounds(typcache, &a.u, &r.u) > 0) {
				r.d = 0;
				r.u = a.u;
			}
			NEXT(a);
		}
		else if (!b.e && bounds_overlap(typcache, &r.u, &b.l)) {
			if (range_cmp_bounds(typcache, &b.u, &r.u) > 0) {
				r.d = 0;
				r.u = b.u;
			}
			NEXT(b);
		}
		else {
			rb[ri ++] = put_range(&r, typcache);
			r.d = 0;
			r.e = true;
		}
	}

	ra = construct_array(rb, ri, typ, typcache->typlen, typcache->typbyval, typcache->typalign);
	pfree(rb);
	PG_RETURN_ARRAYTYPE_P(ra);
}

PG_FUNCTION_INFO_V1(ranges_union1);
Datum ranges_union1(PG_FUNCTION_ARGS);
Datum ranges_union1(PG_FUNCTION_ARGS)
{
	ArrayType *aa = PG_GETARG_ARRAYTYPE_P(0);
	RangeType *br = PG_GETARG_RANGE(1);
	ArrayType *ra;
	Oid typ;
	TypeCacheEntry *typcache;
	int an;
	int ai = 0, ri = 0;
	char *ap;
	Datum *rb;
	struct range a, b;

	typ = ARR_ELEMTYPE(aa);
	if (ARR_NDIM(aa) > 1 || array_contains_nulls(aa) ||
	    RangeTypeGetOid(br) != typ)
		ereport(ERROR, (errcode(ERRCODE_DATA_EXCEPTION), errmsg("ranges must be one-dimensional non-null arrays of the same type")));

	typcache = range_get_typcache(fcinfo, typ);
	b.d = RangeTypeGetDatum(br);
	range_deserialize(typcache, br, &b.l, &b.u, &b.e);

	if (b.e)
		PG_RETURN_ARRAYTYPE_P(aa);
	if (ARR_NDIM(aa) < 1 || !(an = *ARR_DIMS(aa))) {
		ra = create_singleton_array(fcinfo, typ, RangeTypeGetDatum(br), false, 1);
		PG_RETURN_ARRAYTYPE_P(ra);
	}

	rb = palloc0(sizeof(*rb)*(an + 1));

	ap = ARR_DATA_PTR(aa);
	NEXT(a);

	while (!a.e && !bounds_overlap(typcache, &a.u, &b.l)) {
		rb[ri ++] = a.d;
		NEXT(a);
	}

	if (a.e || !bounds_overlap(typcache, &b.u, &a.l))
		rb[ri ++] = b.d;
	else {
		struct range r = {};
		if (range_cmp_bounds(typcache, &a.l, &b.l) <= 0)
			r.l = a.l;
		else
			r.l = b.l;

		while (1) {
			if (range_cmp_bounds(typcache, &a.u, &b.u) >= 0) {
				r.u = a.u;
				NEXTD(a);
				break;
			}
			NEXT(a);
			if (a.e || !bounds_overlap(typcache, &b.u, &a.l)) {
				r.u = b.u;
				break;
			}
		}
		rb[ri ++] = put_range(&r, typcache);
	}

	while (a.d) {
		rb[ri ++] = a.d;
		NEXTD(a);
	}

	ra = construct_array(rb, ri, typ, typcache->typlen, typcache->typbyval, typcache->typalign);
	pfree(rb);
	PG_RETURN_ARRAYTYPE_P(ra);
}

void _PG_init(void);
void _PG_init()
{
}
