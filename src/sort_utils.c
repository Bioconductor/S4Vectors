/****************************************************************************
 * Low-level sorting utilities                                              *
 * ---------------------------                                              *
 ****************************************************************************/
#include "S4Vectors.h"
#include <stdlib.h>  /* for qsort() */
#include <limits.h>  /* for INT_MIN and INT_MAX */


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

static int compar_aa_stable(const void *p1, const void *p2)
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
	qsort(out, nelt, sizeof(int), compar_aa_stable);
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
static int (*rxcompar)(const void *, const void *);
static const int *rxtargets[MAX_RXTARGETS];
static int rxdescs[MAX_RXTARGETS];
static int last_rxlevel;
static unsigned short int *ushort_rxbucket_idx_buf;

#define	RXLEVELS_PER_RXTARGET	2
#define	BITS_PER_RXLEVEL	(sizeof(unsigned short int) * CHAR_BIT)
#define	RXBUCKETS		(1 << BITS_PER_RXLEVEL)
static int rxbucket_sizes_bufs[RXBUCKETS *
			       RXLEVELS_PER_RXTARGET * MAX_RXTARGETS];
static int rxbucket_used_bufs[RXBUCKETS *
			       RXLEVELS_PER_RXTARGET * MAX_RXTARGETS];
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

static void rxsort_rec(int *base, int base_len, int *out,
		       int level, int flipped)
{
	static const int *target;
	static int desc, is_sorted, tval, offset, bucket_size, *tmp;
	static unsigned short int ushort_bucket_idx;
	int i, *bucket_sizes_buf, *bucket_used_buf, nbucket,
	       first_bucket, last_bucket, bucket_idx;

	if (base_len == 0)
		return;

	if (base_len == 1) {
		if (flipped)
			*out = *base;
		return;
	}

	target = rxtargets[level >> 1];
	desc = rxdescs[level >> 1];

	/* Find out whether 'base' is already sorted with respect to current
	   target. */
	is_sorted = base_is_sorted1(base, base_len, target, desc);

	/* Special treatment of last 2 levels. */
	if (level >= last_rxlevel - 1) {
		if (is_sorted) {
			if (flipped)
				memcpy(out, base, sizeof(int) * base_len);
			return;
		}
		//if (level == last_rxlevel - 1 && base_len < RXBUCKETS) {
		//if (base_len < RXBUCKETS << 3) {
		if (base_len < 128) {
			qsort(base, base_len, sizeof(int), rxcompar);
			if (flipped)
				memcpy(out, base, sizeof(int) * base_len);
			return;
		}
	}

	/* Compute bucket indices and bucket sizes. */
	bucket_sizes_buf = rxbucket_sizes_bufs + level * RXBUCKETS;
	memset(bucket_sizes_buf, 0, sizeof(int) * RXBUCKETS);
	bucket_used_buf = rxbucket_used_bufs + level * RXBUCKETS;
	nbucket = 0;
	if (level % 2 == 0) {
		/* Use 16 bits on the left to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			ushort_bucket_idx = tval >> BITS_PER_RXLEVEL;
			ushort_bucket_idx += 0x8000;
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			if (bucket_sizes_buf[ushort_bucket_idx]++ == 0)
				bucket_used_buf[nbucket++] = ushort_bucket_idx;
		}
	} else {
		/* Use 16 bits on the right to compute the bucket indices. */
		for (i = 0; i < base_len; i++) {
			tval = target[base[i]];
			ushort_bucket_idx = tval;
			ushort_rxbucket_idx_buf[i] = ushort_bucket_idx;
			if (bucket_sizes_buf[ushort_bucket_idx]++ == 0)
				bucket_used_buf[nbucket++] = ushort_bucket_idx;
		}
	}
	//printf("level=%d nbucket=%d\n", nbucket);

	offset = 0;
#define	MAX_BUCKETS_FOR_QSORT 512
	if (nbucket <= MAX_BUCKETS_FOR_QSORT) {
		int buckets_need_qsort = 0;
		for (i = 1; i < nbucket; i++) {
			if (bucket_used_buf[i] < bucket_used_buf[i - 1]) {
				buckets_need_qsort = 1;
				break;
			}
		}
		if (buckets_need_qsort) {
			/* Sort buckets in use. */
			_sort_int_array(bucket_used_buf, nbucket, 0);
		}
		/* Compute bucket offsets. */
		if (desc) {
			/* Process buckets from last to first. */
			for (i = nbucket - 1; i >= 0; i--) {
				bucket_idx = bucket_used_buf[i];
				bucket_size = bucket_sizes_buf[bucket_idx];
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_size;
			}
		} else {
			/* Process buckets from first to last. */
			for (i = 0; i < nbucket; i++) {
				bucket_idx = bucket_used_buf[i];
				bucket_size = bucket_sizes_buf[bucket_idx];
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_size;
			}
		}
		//printf("level=%d first_bucket=%d last_bucket=%d\n",
		//	level, bucket_used_buf[0],
		//	bucket_used_buf[nbucket - 1]);
	} else {
		/* Find indices of first and last non-empty buckets, and
		   compute bucket offsets. */
		first_bucket = last_bucket = -1;
		if (desc) {
			/* Process buckets from last to first. */
			for (bucket_idx = RXBUCKETS - 1;
			     bucket_idx >= 0;
			     bucket_idx--)
			{
				bucket_size = bucket_sizes_buf[bucket_idx];
				if (bucket_size != 0) {
					first_bucket = bucket_idx;
					if (last_bucket == -1)
						last_bucket = bucket_idx;
				}
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_size;
			}
		} else {
			/* Process buckets from first to last. */
			for (bucket_idx = 0;
			     bucket_idx < RXBUCKETS;
			     bucket_idx++)
			{
				bucket_size = bucket_sizes_buf[bucket_idx];
				if (bucket_size != 0) {
					if (first_bucket == -1)
						first_bucket = bucket_idx;
					last_bucket = bucket_idx;
				}
				rxbucket_offsets[bucket_idx] = offset;
				offset += bucket_size;
			}
		}
		//printf("level=%d first_bucket=%d last_bucket=%d\n",
		//	level, first_bucket, last_bucket);
	}

	/* Sort 'base' with respect to current radix level. */
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

	/* Order each bucket. */
	level++;
	if (nbucket <= MAX_BUCKETS_FOR_QSORT) {
		if (desc) {
			/* Process buckets from last to first. */
			for (i = nbucket - 1; i >= 0; i--) {
				bucket_idx = bucket_used_buf[i];
				base_len = bucket_sizes_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		} else {
			/* Process buckets from first to last. */
			for (i = 0; i < nbucket; i++) {
				bucket_idx = bucket_used_buf[i];
				base_len = bucket_sizes_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
		}
	} else {
		if (desc) {
			/* Process buckets from last to first. */
			for (bucket_idx = last_bucket;
			     bucket_idx >= first_bucket;
			     bucket_idx--)
			{
				base_len = bucket_sizes_buf[bucket_idx];
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
				base_len = bucket_sizes_buf[bucket_idx];
				rxsort_rec(base, base_len, out, level, flipped);
				base += base_len;
				out += base_len;
			}
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

	aa = x;
	aa_desc = desc;
	rxcompar = compar_aa_stable;

	if (!use_radix || !can_use_rxsort()) {
		qsort(base, base_len, sizeof(int), rxcompar);
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

static int compar_aabb(int i1, int i2)
{
	int ret;

	ret = aa_desc ? aa[i2] - aa[i1] : aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	ret = bb_desc ? bb[i2] - bb[i1] : bb[i1] - bb[i2];
	return ret;
}

static int compar_aabb_stable(const void *p1, const void *p2)
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
	qsort(out, nelt, sizeof(int), compar_aabb_stable);
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

	aa = a;
	bb = b;
	aa_desc = a_desc;
	bb_desc = b_desc;
	rxcompar = compar_aabb_stable;

	if (!use_radix || !can_use_rxsort()) {
		qsort(base, base_len, sizeof(int), rxcompar);
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

static int compar_aabbccdd_stable(const void *p1, const void *p2)
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
	qsort(out, nelt, sizeof(int), compar_aabbccdd_stable);
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

