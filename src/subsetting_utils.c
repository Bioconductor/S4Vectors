/****************************************************************************
 *                 Low-level internal subsetting utilities                  *
 ****************************************************************************/
#include "S4Vectors.h"


SEXP _extract_window_from_vectorORfactor(SEXP x, int start, int end)
{
	int x_len, offset, ans_len;
	SEXP ans, x_names, ans_names, ans_class, ans_levels;

	x_len = LENGTH(x);
	if (start == NA_INTEGER || start < 1 || start > x_len + 1)
		error("'start' must be >= 1 and <= 'length(x)' + 1");
	if (end == NA_INTEGER || end < 0 || end > x_len)
		error("'end' must be >= 0 and <= 'length(x)'");
	offset = start - 1;
	if (end < offset)
		error("'end' must be >= 'start' - 1");
	ans_len = end - offset;
	PROTECT(ans = allocVector(TYPEOF(x), ans_len));
	_copy_vector_block(ans, 0, x, offset, ans_len);
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(ans_len));
		_copy_vector_block(ans_names, 0, x_names, offset, ans_len);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	if (isFactor(x)) {
		PROTECT(ans_class = duplicate(GET_CLASS(x)));
		SET_CLASS(ans, ans_class);
		UNPROTECT(1);
		PROTECT(ans_levels = duplicate(GET_LEVELS(x)));
		SET_LEVELS(ans, ans_levels);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}
/*
 * --- .Call ENTRY POINT ---
 * Args:
 *   x:          An atomic vector or a list.
 *   start, end: Single integer values defining a valid window on 'x'.
 */
SEXP vectorORfactor_extract_window(SEXP x, SEXP start, SEXP end)
{
	int npair;
	const int *start_p, *end_p;

	npair = _check_integer_pairs(start, end,
				     &start_p, &end_p,
				     "start", "end");
	if (npair != 1)
		error("'start' and 'end' must be of length 1");
	return _extract_window_from_vectorORfactor(x, start_p[0], end_p[0]);
}

