#include "S4Vectors.h"

// R_XLEN_T_MAX is 2^52
// LLONG_MAX    is 2^63-1

SEXP logical_sum(SEXP x, SEXP na_rm)
{
	R_xlen_t x_len, sum, i;
	const int *x_dataptr;
	int na_rm0, x_elt;
	SEXP ans;

	x_len = XLENGTH(x);
	x_dataptr = LOGICAL(x);
	na_rm0 = LOGICAL(na_rm)[0];
	sum = 0;
	for (i = 0; i < x_len; i++) {
		x_elt = x_dataptr[i];
		if (x_elt == NA_LOGICAL) {
			if (na_rm0)
				continue;
			return ScalarInteger(NA_INTEGER);
		}
		/* IIRC some comments in the R source code seem to suggest
		   that TRUEs are not guaranteed to be represented by ones
		   at the C level. */
		if (x_elt)
			sum++;
	}
	if (sum <= INT_MAX)
		ans = ScalarInteger((int) sum);
	else
		ans = ScalarReal((double) sum);
	return ans;
}

