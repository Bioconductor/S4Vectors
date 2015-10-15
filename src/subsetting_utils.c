/****************************************************************************
 *                 Low-level internal subsetting utilities                  *
 ****************************************************************************/
#include "S4Vectors.h"


/*
 * --- .Call ENTRY POINT ---
 * Args:
 *   x:          An atomic vector or a list.
 *   start, end: Single integer values defining a valid window on 'x'.
 */
SEXP vector_extract_window(SEXP x, SEXP start, SEXP end)
{
	int x_len, nranges, start0, end0, offset, ans_len;
	const int *start_p, *end_p;
	SEXP ans, x_names, ans_names;

	x_len = LENGTH(x);
	nranges = _check_integer_pairs(start, end,
				       &start_p, &end_p,
				       "start", "end");
	if (nranges != 1)
		error("'start' and 'end' must be of length 1");
	start0 = start_p[0];
	end0 = end_p[0];
	if (start0 == NA_INTEGER || start0 < 1 || start0 > x_len + 1)
		error("'start' must be >= 1 and <= 'length(x)' + 1");
	if (end0 == NA_INTEGER || end0 < 0 || end0 > x_len)
		error("'end' must be >= 0 and <= 'length(x)'");
	offset = start0 - 1;
	if (end0 < offset)
		error("'end' must be >= 'start' - 1");
	ans_len = end0 - offset;
	PROTECT(ans = allocVector(TYPEOF(x), ans_len));
	_vector_memcpy(ans, 0, x, offset, ans_len);
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(ans_len));
		_vector_memcpy(ans_names, 0, x_names, offset, ans_len);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

