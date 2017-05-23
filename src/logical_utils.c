#include "S4Vectors.h"

#include <limits.h> /* for CHAR_BIT */
#include <stdlib.h> /* for div() */

#define BIT7_MASK (1 << (CHAR_BIT-1))

#define END_OP 0
#define S_H_OP 1
#define N_OP   2
#define BAD_OP 3
#define P_OP   4
#define I_OP   5
#define D_OP   6
#define M_OP   7

static const unsigned char BitsSetTable256[256] =
{
#   define B2(n) n,     n+1,     n+1,     n+2
#   define B4(n) B2(n), B2(n+1), B2(n+1), B2(n+2)
#   define B6(n) B4(n), B4(n+1), B4(n+1), B4(n+2)
    B6(0), B6(1), B6(1), B6(2)
};

/* Turns a logical vector into a "compact bit vector" */
SEXP logical_as_compact_bitvector(SEXP x)
{
	SEXP ans;
	Rbyte *ans_elt;
	int x_length, ans_length, i, j, x_elt;
	div_t q;

	x_length = LENGTH(x);
	q = div(x_length, CHAR_BIT);
	ans_length = q.quot;
	if (q.rem != 0)
		ans_length++;
	PROTECT(ans = NEW_RAW(ans_length));
	for (i = j = 0, ans_elt = RAW(ans); i < x_length; i++, j++) {
		if (j >= CHAR_BIT) {
			j = 0;
			ans_elt++;
		}
		*ans_elt <<= 1;
		x_elt = LOGICAL(x)[i];
		if (x_elt == NA_INTEGER) {
			UNPROTECT(1);
			error("'x' contains NAs");
		}
		if (x_elt)
			*ans_elt |= 1;
	}
	if (q.rem != 0)
		*ans_elt <<= CHAR_BIT - q.rem;
	UNPROTECT(1);
	return ans;
}

/* Turns a "compact bit vector" into a logical vector */
SEXP compact_bitvector_as_logical(SEXP x, SEXP length_out)
{
	SEXP ans;
	Rbyte x_elt;
	int ans_length, x_length, i, j, k;

	ans_length = INTEGER(length_out)[0];
	x_length = LENGTH(x);
	if (ans_length > x_length * CHAR_BIT)
		error("'length_out' is > 'length(x)' * %d", CHAR_BIT);
	PROTECT(ans = NEW_LOGICAL(ans_length));
	for (i = j = 0, x_elt = RAW(x)[k = 0]; i < ans_length; i++, j++) {
		if (j >= CHAR_BIT) {
			j = 0;
			x_elt = RAW(x)[++k];
		}
		LOGICAL(ans)[i] = (x_elt & BIT7_MASK) != 0;
		x_elt <<= 1;
	}
	UNPROTECT(1);
	return ans;
}

/* Subsets a "compact bit vector" */
SEXP subset_compact_bitvector(SEXP x, SEXP subscript)
{
	SEXP ans;
	Rbyte *ans_elt;
	int x_length, subscript_length, ans_length, i, j, sub_i;
	div_t q, q2;

	x_length = LENGTH(x);
	subscript_length = LENGTH(subscript);
	q = div(subscript_length, CHAR_BIT);
	ans_length = q.quot;
	if (q.rem != 0)
		ans_length++;
	PROTECT(ans = NEW_RAW(ans_length));
	for (i = j = 0, ans_elt = RAW(ans); i < subscript_length; i++, j++) {
		if (j >= CHAR_BIT) {
			j = 0;
			ans_elt++;
		}
		*ans_elt <<= 1;
		sub_i = INTEGER(subscript)[i];
		if (sub_i == NA_INTEGER) {
			UNPROTECT(1);
			error("subscript contains NAs");
		}
		sub_i--;
		q2 = div(sub_i, CHAR_BIT);
		if (sub_i < 0 || q2.quot >= x_length) {
			UNPROTECT(1);
			error("subscript out of bounds");
		}
		if (RAW(x)[q2.quot] & (BIT7_MASK >> q2.rem))
			*ans_elt |= 1;
	}
	if (q.rem != 0)
		*ans_elt <<= CHAR_BIT - q.rem;
	UNPROTECT(1);
	return ans;
}

SEXP compact_bitvector_bit_count(SEXP x)
{
	SEXP ans;
	Rbyte *x_elt;
	int *ans_elt, ans_length, i;

	ans_length = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, x_elt = RAW(x), ans_elt = INTEGER(ans); i < ans_length;
			i++, x_elt++, ans_elt++) {
		*ans_elt = BitsSetTable256[*x_elt];
	}
	UNPROTECT(1);
	return(ans);
}

SEXP compact_bitvector_last_bit(SEXP x)
{
	SEXP ans;
	Rbyte LAST_MASK, *x_elt;
	int *ans_elt, ans_length, i;

	LAST_MASK = BIT7_MASK >> 7;
	ans_length = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, x_elt = RAW(x), ans_elt = INTEGER(ans); i < ans_length;
			i++, x_elt++, ans_elt++) {
		*ans_elt = (*x_elt & LAST_MASK) != 0;
	}
	UNPROTECT(1);
	return(ans);
}

