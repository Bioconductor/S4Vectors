/****************************************************************************
 * Low-level sorting utilities                                              *
 * ---------------------------                                              *
 *                                                                          *
 * All sortings/orderings are based on the qsort() function from the        *
 * standard C lib.                                                          *
 * Note that C qsort() is NOT "stable" so the ordering functions below      *
 * (_get_order_of_*() functions) need to ultimately break ties by position  *
 * (this is done by adding a little extra code at the end of the comparison *
 * function used in the call to qsort()).                                   *
 ****************************************************************************/
#include "S4Vectors.h"
#include <stdlib.h> /* for qsort() */


static const int *aa, *bb, *cc, *dd;


/****************************************************************************
 * Sorting or getting the order of an int array.
 */

static int compar_ints_for_asc_sort(const void *p1, const void *p2)
{
	return *((const int *) p1) - *((const int *) p2);
}

static int compar_ints_for_desc_sort(const void *p1, const void *p2)
{
	return compar_ints_for_asc_sort(p2, p1);
}

void _sort_int_array(int *x, int nelt, int desc)
{
	int (*compar)(const void *, const void *);

	compar = desc ? compar_ints_for_desc_sort : compar_ints_for_asc_sort;
	qsort(x, nelt, sizeof(int), compar);
	return;
}

static int compar_aa_for_stable_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_aa_for_stable_desc_order(p1, p2) to be
 * compar_aa_for_stable_asc_order(p2, p1) because of the tie-break
 * by position. */
static int compar_aa_for_stable_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = aa[i2] - aa[i1];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

void _get_order_of_int_array(const int *x, int nelt,
		int desc, int *out, int out_shift)
{
	int i, (*compar)(const void *, const void *);

	aa = x - out_shift;
	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	compar = desc ? compar_aa_for_stable_desc_order :
			compar_aa_for_stable_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}


/****************************************************************************
 * Getting the order of 2 int arrays of the same length.
 * The second array ('b') is used to break ties in the first array ('a').
 */

static int compar_int_pairs(int a1, int b1, int a2, int b2)
{
	int ret;

	ret = a1 - a2;
	if (ret != 0)
		return ret;
	ret = b1 - b2;
	return ret;
}

/* Vectorized comparison of 2 vectors of integer pairs. */
void _compare_int_pairs(const int *a1, const int *b1, int nelt1,
			const int *a2, const int *b2, int nelt2,
			int *out, int out_len, int with_warning)
{
	int i, j, k;

	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= nelt1)
			i = 0; /* recycle i */
		if (j >= nelt2)
			j = 0; /* recycle j */
		out[k] = compar_int_pairs(a1[i], b1[i], a2[j], b2[j]);
	}
	/* This warning message is meaningful only when 'out_len' is
	   'max(nelt1, nelt2)' and is consistent with the warning we get from
	   binary arithmetic/comparison operations on numeric vectors. */
	if (with_warning && out_len != 0 && (i != nelt1 || j != nelt2))
		warning("longer object length is not a multiple "
			"of shorter object length");
	return;
}

int _int_pairs_are_sorted(const int *a, const int *b, int nelt,
			  int desc, int strict)
{
	int a1, b1, a2, b2, i, ret;

	if (nelt == 0)
		return 1;
	a2 = a[0];
	b2 = b[0];
	for (i = 1; i < nelt; i++) {
		a1 = a2;
		b1 = b2;
		a2 = a[i];
		b2 = b[i];
		ret = compar_int_pairs(a1, b1, a2, b2);
		if (ret == 0) {
			if (strict) return 0;
			continue;
		}
		if (desc != (ret > 0))
			return 0;
	}
	return 1;
}

static int compar_aabb(int i1, int i2)
{
	int ret;

	ret = aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	ret = bb[i1] - bb[i2];
	return ret;
}

static int compar_aabb_for_stable_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_aabb(i1, i2);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_aabb_for_stable_desc_order(p1, p2) to be
 * compar_aabb_for_stable_asc_order(p2, p1) because of the tie-break
 * by position. */
static int compar_aabb_for_stable_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_aabb(i2, i1);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

void _get_order_of_int_pairs(const int *a, const int *b, int nelt,
		int desc, int *out, int out_shift)
{
	int i, (*compar)(const void *, const void *);

	aa = a - out_shift;
	bb = b - out_shift;
	for (i = 0; i < nelt; i++, out_shift++)
		out[i] = out_shift;
	compar = desc ? compar_aabb_for_stable_desc_order :
			compar_aabb_for_stable_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}

void _get_matches_of_ordered_int_pairs(
		const int *a1, const int *b1, const int *o1, int nelt1,
		const int *a2, const int *b2, const int *o2, int nelt2,
		int nomatch, int *out, int out_shift)
{
	int i1, i2, ret;

	i2 = 0;
	ret = 0;
	for (i1 = 0; i1 < nelt1; i1++, o1++) {
		while (i2 < nelt2) {
			ret = compar_int_pairs(
				a1[*o1], b1[*o1],
				a2[*o2], b2[*o2]);
			if (ret <= 0)
				break;
			i2++, o2++;
		}
		out[*o1] = ret == 0 ? *o2 + out_shift : nomatch;
	}
	return;
}


/****************************************************************************
 * Getting the order of 4 int arrays of the same length.
 * 2nd, 3rd and 4th arrays are used to successively break ties.
 */

static int compar_int_quads(int a1, int b1, int c1, int d1,
			    int a2, int b2, int c2, int d2)
{
	int ret;

	ret = compar_int_pairs(a1, b1, a2, b2);
	if (ret != 0)
		return ret;
	ret = c1 - c2;
	if (ret != 0)
		return ret;
	ret = d1 - d2;
	return ret;
}

int _int_quads_are_sorted(const int *a, const int *b,
			  const int *c, const int *d, int nelt,
			  int desc, int strict)
{
	int a1, b1, c1, d1, a2, b2, c2, d2, i, ret;

	if (nelt == 0)
		return 1;
	a2 = a[0];
	b2 = b[0];
	c2 = c[0];
	d2 = d[0];
	for (i = 1; i < nelt; i++) {
		a1 = a2;
		b1 = b2;
		c1 = c2;
		d1 = d2;
		a2 = a[i];
		b2 = b[i];
		c2 = c[i];
		d2 = d[i];
		ret = compar_int_quads(a1, b1, c1, d1, a2, b2, c2, d2);
		if (ret == 0) {
			if (strict) return 0;
			continue;
		}
		if (desc != (ret > 0))
			return 0;
	}
	return 1;
}

static int compar_aabbccdd(int i1, int i2)
{
	int ret;

	ret = compar_aabb(i1, i2);
	if (ret != 0)
		return ret;
	ret = cc[i1] - cc[i2];
	if (ret != 0)
		return ret;
	ret = dd[i1] - dd[i2];
	return ret;
}

static int compar_aabbccdd_for_stable_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_aabbccdd(i1, i2);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_aabbccdd_for_stable_desc_order(p1, p2) to be
 * compar_aabbccdd_for_stable_asc_order(p2, p1) because of the tie-break
 * by position. */
static int compar_aabbccdd_for_stable_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_aabbccdd(i2, i1);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

void _get_order_of_int_quads(const int *a, const int *b,
		const int *c, const int *d, int nelt,
		int desc, int *out, int out_shift)
{
	int i, (*compar)(const void *, const void *);

	aa = a - out_shift;
	bb = b - out_shift;
	cc = c - out_shift;
	dd = d - out_shift;
	for (i = 0; i < nelt; i++, out_shift++)
		out[i] = out_shift;
	compar = desc ? compar_aabbccdd_for_stable_desc_order :
			compar_aabbccdd_for_stable_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}

void _get_matches_of_ordered_int_quads(
		const int *a1, const int *b1, const int *c1, const int *d1,
		const int *o1, int nelt1,
		const int *a2, const int *b2, const int *c2, const int *d2,
		const int *o2, int nelt2,
		int nomatch, int *out, int out_shift)
{
	int i1, i2, ret;

	i2 = 0;
	ret = 0;
	for (i1 = 0; i1 < nelt1; i1++, o1++) {
		while (i2 < nelt2) {
			ret = compar_int_quads(
				a1[*o1], b1[*o1], c1[*o1], d1[*o1],
				a2[*o2], b2[*o2], c2[*o2], d2[*o2]);
			if (ret <= 0)
				break;
			i2++, o2++;
		}
		out[*o1] = ret == 0 ? *o2 + out_shift : nomatch;
	}
	return;
}

