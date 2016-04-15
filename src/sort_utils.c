/****************************************************************************
 * Low-level sorting utilities                                              *
 * ---------------------------                                              *
 ****************************************************************************/
#include "S4Vectors.h"
#include <stdlib.h>  /* for qsort() */
#include <limits.h>  /* for INT_MIN and INT_MAX */


/****************************************************************************
 * 4 wrappers to qsort()
 */

static const int *aa, *bb, *cc, *dd;
static int aa_desc, bb_desc, cc_desc, dd_desc;

#define	COMPARE_TARGET_INTS(target, i1, i2, desc) \
	((desc) ? (target)[(i2)] - (target)[(i1)] \
		: (target)[(i1)] - (target)[(i2)])

static int compar1_stable(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = COMPARE_TARGET_INTS(aa, i1, i2, aa_desc);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

static int compar2_stable(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = COMPARE_TARGET_INTS(aa, i1, i2, aa_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(bb, i1, i2, bb_desc);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

static int compar3_stable(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = COMPARE_TARGET_INTS(aa, i1, i2, aa_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(bb, i1, i2, bb_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(cc, i1, i2, cc_desc);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

static int compar4_stable(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = COMPARE_TARGET_INTS(aa, i1, i2, aa_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(bb, i1, i2, bb_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(cc, i1, i2, cc_desc);
	if (ret != 0)
		return ret;
	ret = COMPARE_TARGET_INTS(dd, i1, i2, dd_desc);
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

static void qsort1(int *base, int base_len, const int *a, int a_desc)
{
	aa = a;
	aa_desc = a_desc;
	qsort(base, base_len, sizeof(int), compar1_stable);
}

static void qsort2(int *base, int base_len,
		   const int *a, const int *b,
		   int a_desc, int b_desc)
{
	aa = a;
	bb = b;
	aa_desc = a_desc;
	bb_desc = b_desc;
	qsort(base, base_len, sizeof(int), compar2_stable);
}

static void qsort3(int *base, int base_len,
		   const int *a, const int *b, const int *c,
		   int a_desc, int b_desc, int c_desc)
{
	aa = a;
	bb = b;
	cc = c;
	aa_desc = a_desc;
	bb_desc = b_desc;
	cc_desc = c_desc;
	qsort(base, base_len, sizeof(int), compar3_stable);
}

static void qsort4(int *base, int base_len,
		   const int *a, const int *b, const int *c, const int *d,
		   int a_desc, int b_desc, int c_desc, int d_desc)
{
	aa = a;
	bb = b;
	cc = c;
	dd = d;
	aa_desc = a_desc;
	bb_desc = b_desc;
	cc_desc = c_desc;
	dd_desc = d_desc;
	qsort(base, base_len, sizeof(int), compar4_stable);
}


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

/* If efficiency matters, use _sort_ints() in radix mode instead. */
void _get_order_of_int_array(const int *x, int nelt,
		int desc, int *out, int out_shift)
{
	int i;

	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	qsort1(out, nelt, x - out_shift, desc);
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

/* Dummy rxqsort() below would need to be modified if were to support more
   than 4 targets. */
#define	MAX_RXTARGETS		4
static const int *rxtargets[MAX_RXTARGETS];
static int rxdescs[MAX_RXTARGETS];
static int last_rxlevel;
static unsigned short int *ushort_rxbucket_idx_buf;

#define	RXLEVELS_PER_RXTARGET	2
#define	BITS_PER_RXLEVEL	(sizeof(unsigned short int) * CHAR_BIT)
#define	MAX_RXLEVELS		(MAX_RXTARGETS * RXLEVELS_PER_RXTARGET)
#define	RXBUCKETS		(1 << BITS_PER_RXLEVEL)
static int rxbucket_counts_bufs[RXBUCKETS * MAX_RXLEVELS];
static int rxbucket_used_bufs[RXBUCKETS * MAX_RXLEVELS];
static int rxbucket_offsets[RXBUCKETS];

static int base_is_sorted1(const int *base, int base_len,
			   const int *target, int desc)
{
	int prev_tval, tval, i;

	if (desc) {
		prev_tval = INT_MAX;
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			if (tval > prev_tval)
				return 0;
			prev_tval = tval;
		}
	} else {
		prev_tval = INT_MIN;
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			if (tval < prev_tval)
				return 0;
			prev_tval = tval;
		}
	}
	return 1;
}

static int base_is_sorted2(const int *base, int base_len,
			   const int **targets, int *descs, int ntarget)
{
	int i, j, desc, tval, prev_tval;
	const int *target;

	if (ntarget == 1)
		return base_is_sorted1(base, base_len, targets[0], descs[0]);
	for (i = 0; i < base_len; i++) {
		for (j = 0; j < ntarget; j++) {
			target = targets[j];
			desc = descs[j];
			tval = target[base[i]];
			prev_tval = i != 0 ? target[base[i - 1]]
					   : (desc ? INT_MAX : INT_MIN);
			if (tval != prev_tval) {
				if (desc != (tval < prev_tval))
					return 0;
				break;
			}
		}
	}
	return 1;
}

/* Pretty dummy and doesn't scale :-(
   Should be easy to change. */
static void rxqsort(int *base, int base_len,
		    const int **targets, int *descs, int ntarget)
{
	if (ntarget == 1) {
		qsort1(base, base_len, targets[0], descs[0]);
		return;
	}
	if (ntarget == 2) {
		qsort2(base, base_len, targets[0], targets[1],
				       descs[0], descs[1]);
		return;
	}
	if (ntarget == 3) {
		qsort3(base, base_len, targets[0], targets[1], targets[2],
				       descs[0], descs[1], descs[2]);
		return;
	}
	if (ntarget == 4) {
		qsort4(base, base_len, targets[0], targets[1],
				       targets[2], targets[3],
				       descs[0], descs[1], descs[2], descs[3]);
		return;
	}
	error("S4Vectors internal error in rxqsort(): "
	      "ntarget must be between >= 1 and <= 4");
	return;
}

static void rxsort_rec(int *base, int base_len, int *out,
		       int level, int flipped)
{
	static int target_no, desc, is_sorted, qsort_cutoff, tval, offset,
		   bucket_count, *tmp;
	static const int *target;
	static unsigned short int ushort_bucket_idx;
	int i, *bucket_counts_buf, *bucket_used_buf, nbucket,
	       first_bucket, last_bucket, bucket_idx;

	/* --- HANDLE THE EASY SITUATIONS --- */

	if (base_len == 0)
		return;
	if (base_len == 1) {
		if (flipped)
			*out = *base;
		return;
	}
	target_no = level >> 1;
	target = rxtargets[target_no];
	desc = rxdescs[target_no];

	/* Find out whether 'base' is already sorted with respect to current
	   target.
	   TODO: Should probably use base_is_sorted2() instead. If 'base' is
	   already sorted with respect to *all* targets then we can bailout
	   anytime, not only when we are on the last target. */
	is_sorted = base_is_sorted1(base, base_len, target, desc);
	/* Bailout if 'base' is sorted with respect to current target. Can
	   only do this if we are on the last 2 levels (i.e. on the last
	   target). */
	if (is_sorted && level >= last_rxlevel - 1) {
		if (flipped)
			memcpy(out, base, sizeof(int) * base_len);
		return;
	}

	/* The formula for computing the qsort cut-off makes the bold
	   assumption that the cost of rxqsort() is linear with respect to
	   the number of targets involved in the sort ('ntarget' argument).
	   That tends to be the case when there is a high percentage of ties
	   but the reality is more complex.
	   The current formula leads to the following cut-off values:

	          target_no |     0     |     1     |     2     |     3
	     --------------------------------------------------------------
	      with 1 target | 512 * 1/1 |           |           |
	     with 2 targets | 512 * 1/2 | 512 * 2/2 |           |
	     with 4 targets | 512 * 1/4 | 512 * 2/4 | 512 * 3/4 | 512 * 4/4

	   The choice of 512 as max cut-off is based on empirical observation.
	   TODO: All these things need more fine tuning...
	*/
	qsort_cutoff = 512 * (target_no + 1) / ((last_rxlevel + 1) >> 1);
	if (base_len <= qsort_cutoff) {
		rxqsort(base, base_len,
			rxtargets + target_no, rxdescs + target_no,
			((last_rxlevel - level) >> 1) + 1);
		if (flipped)
			memcpy(out, base, sizeof(int) * base_len);
		return;
	}

	/* --- COMPUTE BUCKET INDICES AND COUNTS --- */

	bucket_counts_buf = rxbucket_counts_bufs + RXBUCKETS * level;
	memset(bucket_counts_buf, 0, sizeof(int) * RXBUCKETS);
	bucket_used_buf = rxbucket_used_bufs + RXBUCKETS * level;
	nbucket = 0;
	if (level % 2 == 0) {
		/* Use 16 bits on the left to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			ushort_bucket_idx = tval >> BITS_PER_RXLEVEL;
			ushort_bucket_idx += 0x8000;
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			if (bucket_counts_buf[ushort_bucket_idx]++ == 0)
				bucket_used_buf[nbucket++] = ushort_bucket_idx;
		}
	} else {
		/* Use 16 bits on the right to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			ushort_bucket_idx = tval;
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			if (bucket_counts_buf[ushort_bucket_idx]++ == 0)
				bucket_used_buf[nbucket++] = ushort_bucket_idx;
		}
	}

	/* --- COMPUTE BUCKET OFFSETS --- */

	offset = 0;
#define	MAX_BUCKETS_FOR_QSORT 512  // also based on empirical observation
	if (nbucket <= MAX_BUCKETS_FOR_QSORT) {
		/* Walk only on BUCKETS IN USE. */
		int sort_buckets_in_use = 0;
		for (i = 1; i < nbucket; i++) {
			if (bucket_used_buf[i] < bucket_used_buf[i - 1]) {
				sort_buckets_in_use = 1;
				break;
			}
		}
		if (sort_buckets_in_use) {
			/* Sort buckets in use. */
			_sort_int_array(bucket_used_buf, nbucket, 0);
		}
		if (desc) {
			/* Process buckets from last to first. */
			for (i = nbucket - 1; i >= 0; i--) {
				bucket_idx = bucket_used_buf[i];
				bucket_count = bucket_counts_buf[bucket_idx];
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_count;
			}
		} else {
			/* Process buckets from first to last. */
			for (i = 0; i < nbucket; i++) {
				bucket_idx = bucket_used_buf[i];
				bucket_count = bucket_counts_buf[bucket_idx];
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_count;
			}
		}
	} else {
		/* Walk on ALL BUCKETS. */
		first_bucket = last_bucket = -1;
		if (desc) {
			/* Process buckets from last to first. */
			for (bucket_idx = RXBUCKETS - 1;
			     bucket_idx >= 0;
			     bucket_idx--)
			{
				bucket_count = bucket_counts_buf[bucket_idx];
				if (bucket_count != 0) {
					first_bucket = bucket_idx;
					if (last_bucket == -1)
						last_bucket = bucket_idx;
				}
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_count;
			}
		} else {
			/* Process buckets from first to last. */
			for (bucket_idx = 0;
			     bucket_idx < RXBUCKETS;
			     bucket_idx++)
			{
				bucket_count = bucket_counts_buf[bucket_idx];
				if (bucket_count != 0) {
					if (first_bucket == -1)
						first_bucket = bucket_idx;
					last_bucket = bucket_idx;
				}
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_count;
			}
		}
	}

	/* --- SORT 'base' WITH RESPECT TO CURRENT RADIX LEVEL --- */

	if (!(is_sorted || nbucket == 1)) {
		for (i = 0; i < base_len; i++)
			out[rxbucket_offsets[ushort_rxbucket_idx_buf[i]]++] =
				base[i];
		/* Flip 'base' and 'out'. */
		tmp = out;
		out = base;
		base = tmp;
		flipped = !flipped;
	}

	if (level == last_rxlevel) {
		if (flipped)
			memcpy(out, base, sizeof(int) * base_len);
		return;
	}

	/* --- ORDER EACH BUCKET --- */

	level++;
	if (nbucket <= MAX_BUCKETS_FOR_QSORT) {
		/* Walk only on BUCKETS IN USE. */
		if (desc) {
			/* Process buckets from last to first. */
			for (i = nbucket - 1; i >= 0; i--) {
				bucket_idx = bucket_used_buf[i];
				base_len = bucket_counts_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		} else {
			/* Process buckets from first to last. */
			for (i = 0; i < nbucket; i++) {
				bucket_idx = bucket_used_buf[i];
				base_len = bucket_counts_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		}
	} else {
		/* Walk on ALL BUCKETS in the first:last bucket range. */
		if (desc) {
			/* Process buckets from last to first. */
			for (bucket_idx = last_bucket;
			     bucket_idx >= first_bucket;
			     bucket_idx--)
			{
				base_len = bucket_counts_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		} else {
			/* Process buckets from first to last. */
			for (bucket_idx = first_bucket;
			     bucket_idx <= last_bucket;
			     bucket_idx++)
			{
				base_len = bucket_counts_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		}
	}
	return;
}

static unsigned short int *alloc_rxbuf1(int base_len)
{
	return (unsigned short int *)
		malloc(sizeof(unsigned short int) * base_len);
}

static int *alloc_rxbuf2(int base_len,
		unsigned short int *rxbuf1, int auto_rxbuf1)
{
	int *rxbuf2;

	rxbuf2 = (int *) malloc(sizeof(int) * base_len);
	if (rxbuf2 == NULL && auto_rxbuf1)
		free(rxbuf1);
	return rxbuf2;
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
		qsort1(base, base_len, x, desc);
		return 0;
	}

	auto_rxbuf1 = rxbuf1 == NULL;
	if (auto_rxbuf1) {
		rxbuf1 = alloc_rxbuf1(base_len);
		if (rxbuf1 == NULL)
			return -1;
	}
	auto_rxbuf2 = rxbuf2 == NULL;
	if (auto_rxbuf2) {
		rxbuf2 = alloc_rxbuf2(base_len, rxbuf1, auto_rxbuf1);
		if (rxbuf2 == NULL)
			return -2;
	}

	rxtargets[0] = x;
	rxdescs[0] = desc;
	last_rxlevel = 1;
	ushort_rxbucket_idx_buf = rxbuf1;
	rxsort_rec(base, base_len, rxbuf2, 0, 0);

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

/* If efficiency matters, use _sort_int_pairs() in radix mode instead. */
void _get_order_of_int_pairs(const int *a, const int *b, int nelt,
		int a_desc, int b_desc, int *out, int out_shift)
{
	int i;

	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	qsort2(out, nelt, a - out_shift, b - out_shift, a_desc, b_desc);
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
		qsort2(base, base_len, a, b, a_desc, b_desc);
		return 0;
	}

	auto_rxbuf1 = rxbuf1 == NULL;
	if (auto_rxbuf1) {
		rxbuf1 = alloc_rxbuf1(base_len);
		if (rxbuf1 == NULL)
			return -1;
	}
	auto_rxbuf2 = rxbuf2 == NULL;
	if (auto_rxbuf2) {
		rxbuf2 = alloc_rxbuf2(base_len, rxbuf1, auto_rxbuf1);
		if (rxbuf2 == NULL)
			return -2;
	}

	rxtargets[0] = a;
	rxtargets[1] = b;
	rxdescs[0] = a_desc;
	rxdescs[1] = b_desc;
	last_rxlevel = 3;
	ushort_rxbucket_idx_buf = rxbuf1;
	rxsort_rec(base, base_len, rxbuf2, 0, 0);

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

void _get_order_of_int_quads(const int *a, const int *b,
		const int *c, const int *d, int nelt,
		int a_desc, int b_desc, int c_desc, int d_desc,
		int *out, int out_shift)
{
	int i;

	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	qsort4(out, nelt, a - out_shift, b - out_shift,
			  c - out_shift, d - out_shift,
			  a_desc, b_desc, c_desc, d_desc);
	return;
}

/* base: 0-based indices into 'a' and 'b'.
   rxbuf1, rxbuf2: NULL or user-allocated buffers of length 'base_len'. */
int _sort_int_quads(int *base, int base_len,
		const int *a, const int *b, const int *c, const int *d,
		int a_desc, int b_desc, int c_desc, int d_desc,
		int use_radix, unsigned short int *rxbuf1, int *rxbuf2)
{
	int auto_rxbuf1, auto_rxbuf2;

	if (!use_radix || !can_use_rxsort()) {
		qsort4(base, base_len, a, b, c, d,
				       a_desc, b_desc, c_desc, d_desc);
		return 0;
	}

	auto_rxbuf1 = rxbuf1 == NULL;
	if (auto_rxbuf1) {
		rxbuf1 = alloc_rxbuf1(base_len);
		if (rxbuf1 == NULL)
			return -1;
	}
	auto_rxbuf2 = rxbuf2 == NULL;
	if (auto_rxbuf2) {
		rxbuf2 = alloc_rxbuf2(base_len, rxbuf1, auto_rxbuf1);
		if (rxbuf2 == NULL)
			return -2;
	}

	rxtargets[0] = a;
	rxtargets[1] = b;
	rxtargets[2] = c;
	rxtargets[3] = d;
	rxdescs[0] = a_desc;
	rxdescs[1] = b_desc;
	rxdescs[2] = c_desc;
	rxdescs[3] = d_desc;
	last_rxlevel = 7;
	ushort_rxbucket_idx_buf = rxbuf1;
	rxsort_rec(base, base_len, rxbuf2, 0, 0);

	if (auto_rxbuf2)
		free(rxbuf2);
	if (auto_rxbuf1)
		free(rxbuf1);
	return 0;
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

