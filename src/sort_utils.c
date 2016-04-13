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


static int aa_desc, bb_desc, cc_desc, dd_desc;
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

/* If efficiency matters, use _sort_ints() in radix mode instead. */
void _sort_int_array(int *x, int nelt, int desc)
{
	int (*compar)(const void *, const void *);

	compar = desc ? compar_ints_for_desc_sort : compar_ints_for_asc_sort;
	qsort(x, nelt, sizeof(int), compar);
	return;
}

static int compar_aa_for_stable_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = aa_desc ? aa[i2] - aa[i1] : aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* If efficiency matters, use _sort_ints() in radix mode instead. */
void _get_order_of_int_array(const int *x, int nelt,
		int desc, int *out, int out_shift)
{
	int i;

	aa_desc = desc;
	aa = x - out_shift;
	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	qsort(out, nelt, sizeof(int), compar_aa_for_stable_order);
	return;
}

/*
 * A radix-based sort for integers.
 * The current implementation assumes that sizeof(int) is 4 and
 * sizeof(unsigned short int) is 2.
 */

static int can_use_rxsort()
{
	return sizeof(int) == 4 && sizeof(unsigned short int) == 2;
}

#define	MAX_RXTARGETS		4
static const int *rxtargets[MAX_RXTARGETS];
static int rxdescs[MAX_RXTARGETS];
static int last_level;
static unsigned short int *ushort_rxbucket_idx_buf;

#define	RXLEVELS_PER_RXTARGET	2
#define	BITS_PER_RXLEVEL	(sizeof(unsigned short int) * CHAR_BIT)
#define	RXBUCKETS		(1 << BITS_PER_RXLEVEL)
static int rxbucket_sizes_bufs[RXBUCKETS *
			       RXLEVELS_PER_RXTARGET * MAX_RXTARGETS];
static int rxbucket_offsets[RXBUCKETS];

static void rxsort_rec(int *base, int base_len, int level, int *out)
{
	static const int *target;
	static int i, bucket_size;
	static unsigned short int ushort_bucket_idx;
	int *bucket_sizes_buf, bucket_idx;

	if (base_len == 0)
		return;
	if (base_len == 1) {
		*out = *base;
		return;
	}
	target = rxtargets[level >> 1];
	aa_desc = rxdescs[level >> 1];
	if (base_len < RXBUCKETS >> 1) {
		aa = target;
		qsort(base, base_len, sizeof(int), compar_aa_for_stable_order);
		memcpy(out, base, sizeof(int) * base_len);
		return;
	}

	/* Compute bucket indices and bucket sizes. */
	bucket_sizes_buf = rxbucket_sizes_bufs + level * RXBUCKETS;
	memset(bucket_sizes_buf, 0, sizeof(int) * RXBUCKETS);
	if (level % 2 == 0) {
		/* Use 16 bits on the left to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			ushort_bucket_idx = target[base[i]] >>
					    BITS_PER_RXLEVEL;
			ushort_bucket_idx += 0x8000;
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			bucket_sizes_buf[ushort_bucket_idx]++;
		}
	} else {
		/* Use 16 bits on the right to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			ushort_bucket_idx = target[base[i]];
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			bucket_sizes_buf[ushort_bucket_idx]++;
		}
	}

	/* Compute bucket offsets. */
	if (aa_desc) {
		/* Last bucket goes first. */
		rxbucket_offsets[RXBUCKETS - 1] = 0;
		for (bucket_idx = RXBUCKETS - 1; bucket_idx > 0; bucket_idx--) {
			bucket_size = bucket_sizes_buf[bucket_idx];
			rxbucket_offsets[bucket_idx - 1] =
				rxbucket_offsets[bucket_idx] + bucket_size;
		}
	} else {
		rxbucket_offsets[0] = 0;
		for (bucket_idx = 0; bucket_idx < RXBUCKETS - 1; bucket_idx++) {
			bucket_size = bucket_sizes_buf[bucket_idx];
			rxbucket_offsets[bucket_idx + 1] =
				rxbucket_offsets[bucket_idx] + bucket_size;
		}
	}

	/* Sort 'base' in 'out'. */
	for (i = 0; i < base_len; i++)
		out[rxbucket_offsets[ushort_rxbucket_idx_buf[i]]++] = base[i];
	if (level == last_level)
		return;

	/* Order each bucket. */
	level++;
	if (aa_desc) {
		/* Last bucket goes first. */
		for (bucket_idx = RXBUCKETS - 1; bucket_idx >= 0; bucket_idx--)
		{
			base_len = bucket_sizes_buf[bucket_idx];
			rxsort_rec(out, base_len, level, base);
			out += base_len;
			base += base_len;
		}
	} else {
		for (bucket_idx = 0; bucket_idx < RXBUCKETS; bucket_idx++)
		{
			base_len = bucket_sizes_buf[bucket_idx];
			rxsort_rec(out, base_len, level, base);
			out += base_len;
			base += base_len;
		}
	}
	return;
}

/* base: 0-based indices into 'x'.
   rxbuf1, rxbuf2: NULL or user-allocated buffers of length 'base_len'. */
int _sort_ints(int *base, int base_len,
	       const int *x,
	       int desc,
	       int use_radix, unsigned short int *rxbuf1, int *rxbuf2)
{
	int auto_rxbuf1, auto_rxbuf2;

	if (!use_radix || !can_use_rxsort()) {
		aa_desc = desc;
		aa = x;
		qsort(base, base_len, sizeof(int),
		      compar_aa_for_stable_order);
		return 0;
	}
	auto_rxbuf1 = rxbuf1 == NULL;
	if (auto_rxbuf1) {
		rxbuf1 = (unsigned short int *)
			 malloc(sizeof(unsigned short int) * base_len);
		if (rxbuf1 == NULL)
			return -1;
	}
	auto_rxbuf2 = rxbuf2 == NULL;
	if (auto_rxbuf2) {
		rxbuf2 = (int *) malloc(sizeof(int) * base_len);
		if (rxbuf2 == NULL) {
			if (auto_rxbuf1)
				free(rxbuf1);
			return -2;
		}
	}
	rxtargets[0] = x;
	rxdescs[0] = desc;
	last_level = 1;
	ushort_rxbucket_idx_buf = rxbuf1;
	rxsort_rec(base, base_len, 0, rxbuf2);
	if (auto_rxbuf2)
		free(rxbuf2);
	if (auto_rxbuf1)
		free(rxbuf1);
	return 0;
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
void _pcompare_int_pairs(const int *a1, const int *b1, int nelt1,
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

	ret = aa_desc ? aa[i2] - aa[i1] : aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	ret = bb_desc ? bb[i2] - bb[i1] : bb[i1] - bb[i2];
	return ret;
}

static int compar_aabb_for_stable_order(const void *p1, const void *p2)
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

/* If efficiency matters, use _sort_int_pairs() in radix mode instead. */
void _get_order_of_int_pairs(const int *a, const int *b, int nelt,
		int a_desc, int b_desc, int *out, int out_shift)
{
	int i;

	aa_desc = a_desc;
	bb_desc = b_desc;
	aa = a - out_shift;
	bb = b - out_shift;
	for (i = 0; i < nelt; i++, out_shift++)
		out[i] = out_shift;
	qsort(out, nelt, sizeof(int), compar_aabb_for_stable_order);
	return;
}

/* base: 0-based indices into 'a' and 'b'.
   rxbuf1, rxbuf2: NULL or user-allocated buffers of length 'base_len'. */
int _sort_int_pairs(int *base, int base_len,
		const int *a, const int *b,
		int a_desc, int b_desc,
		int use_radix, unsigned short int *rxbuf1, int *rxbuf2)
{
	int auto_rxbuf1, auto_rxbuf2;

	if (!use_radix || !can_use_rxsort()) {
		aa_desc = a_desc;
		bb_desc = b_desc;
		aa = a;
		bb = b;
		qsort(base, base_len, sizeof(int),
		      compar_aabb_for_stable_order);
		return 0;
	}
	auto_rxbuf1 = rxbuf1 == NULL;
	if (auto_rxbuf1) {
		rxbuf1 = (unsigned short int *)
			 malloc(sizeof(unsigned short int) * base_len);
		if (rxbuf1 == NULL)
			return -1;
	}
	auto_rxbuf2 = rxbuf2 == NULL;
	if (auto_rxbuf2) {
		rxbuf2 = (int *) malloc(sizeof(int) * base_len);
		if (rxbuf2 == NULL) {
			if (auto_rxbuf1)
				free(rxbuf1);
			return -2;
		}
	}
	rxtargets[0] = a;
	rxtargets[1] = b;
	rxdescs[0] = a_desc;
	rxdescs[1] = b_desc;
	last_level = 3;
	ushort_rxbucket_idx_buf = rxbuf1;
	rxsort_rec(base, base_len, 0, rxbuf2);
	if (auto_rxbuf2)
		free(rxbuf2);
	if (auto_rxbuf1)
		free(rxbuf1);
	return 0;
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
	ret = cc_desc ? cc[i2] - cc[i1] : cc[i1] - cc[i2];
	if (ret != 0)
		return ret;
	ret = dd_desc ? dd[i2] - dd[i1] : dd[i1] - dd[i2];
	return ret;
}

static int compar_aabbccdd_for_stable_order(const void *p1, const void *p2)
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

void _get_order_of_int_quads(const int *a, const int *b,
		const int *c, const int *d, int nelt,
		int a_desc, int b_desc, int c_desc, int d_desc,
		int *out, int out_shift)
{
	int i;

	aa_desc = a_desc;
	bb_desc = b_desc;
	cc_desc = c_desc;
	dd_desc = d_desc;
	aa = a - out_shift;
	bb = b - out_shift;
	cc = c - out_shift;
	dd = d - out_shift;
	for (i = 0; i < nelt; i++, out_shift++)
		out[i] = out_shift;
	qsort(out, nelt, sizeof(int), compar_aabbccdd_for_stable_order);
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

