/****************************************************************************
 *                      Low-level subsetting utilities                      *
 ****************************************************************************/
#include "S4Vectors.h"


/****************************************************************************
 * Copy a block of elements from a vector to a vector of the same type
 *
 * Return the new 'dest' offset.
 */
long long int _copy_vector_block(SEXP dest, long long int dest_offset,
		SEXP src, long long int src_offset,
		long long int block_nelt)
{
	long long int new_dest_offset, i;

	if (block_nelt < 0)
		error("negative widths are not allowed");
	new_dest_offset = dest_offset + block_nelt;
	if (dest_offset < 0 || new_dest_offset > XLENGTH(dest)
	 || src_offset < 0 || src_offset + block_nelt > XLENGTH(src))
		error("subscript contains out-of-bounds indices");
	switch (TYPEOF(dest)) {
	    case LGLSXP:
	    {
		int *dest2 = LOGICAL(dest) + dest_offset;
		const int *src2 = LOGICAL(src) + src_offset;
		for (i = 0; i < block_nelt; i++)
			dest2[i] = src2[i];
	    }
	    break;
	    case INTSXP:
	    {
		int *dest2 = INTEGER(dest) + dest_offset;
		const int *src2 = INTEGER(src) + src_offset;
		for (i = 0; i < block_nelt; i++)
			dest2[i] = src2[i];
	    }
	    break;
	    case REALSXP:
	    {
		double *dest2 = REAL(dest) + dest_offset;
		const double *src2 = REAL(src) + src_offset;
		for (i = 0; i < block_nelt; i++)
			dest2[i] = src2[i];
	    }
	    break;
	    case CPLXSXP:
	    {
		Rcomplex *dest2 = COMPLEX(dest) + dest_offset;
		const Rcomplex *src2 = COMPLEX(src) + src_offset;
		for (i = 0; i < block_nelt; i++)
			dest2[i] = src2[i];
	    }
	    break;
	    case STRSXP:
	    {
		SEXP src_elt;  // dest_elt;
		for (i = 0; i < block_nelt; i++) {
			src_elt = STRING_ELT(src, src_offset + i);
			SET_STRING_ELT(dest, dest_offset + i, src_elt);
			//PROTECT(dest_elt = duplicate(src_elt));
			//SET_STRING_ELT(dest, dest_offset + i, dest_elt);
			//UNPROTECT(1);
		}
	    }
	    break;
	    case RAWSXP:
	    {
		Rbyte *dest2 = RAW(dest) + dest_offset;
		const Rbyte *src2 = RAW(src) + src_offset;
		for (i = 0; i < block_nelt; i++)
			dest2[i] = src2[i];
	    }
	    break;
	    case VECSXP:
	    {
		SEXP src_elt;  // dest_elt;
		for (i = 0; i < block_nelt; i++) {
			src_elt = VECTOR_ELT(src, src_offset + i);
			SET_VECTOR_ELT(dest, dest_offset + i, src_elt);
			//PROTECT(dest_elt = duplicate(src_elt));
			//SET_VECTOR_ELT(dest, dest_offset + i, dest_elt);
			//UNPROTECT(1);
		}
	    }
	    break;
	    default:
		error("S4Vectors internal error in _copy_vector_block(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(dest))));
	}
	return new_dest_offset;
}

/* Return new 'dest_offset'. */
int _copy_vector_positions(SEXP dest, int dest_offset,
		SEXP src, const int *pos, int npos)
{
	int i;

	for (i = 0; i < npos; i++)
		dest_offset = _copy_vector_block(
				dest, (long long int) dest_offset,
				src, (long long int) pos[i] - 1LL,
				1LL);
	return dest_offset;
}

int _copy_vector_ranges(SEXP dest, int dest_offset,
		SEXP src, const int *start, const int *width, int nranges)
{
	int i;

	for (i = 0; i < nranges; i++)
		dest_offset = _copy_vector_block(
				dest, (long long int) dest_offset,
				src, (long long int) start[i] - 1LL,
				(long long int) width[i]);
	return dest_offset;
}


/****************************************************************************
 * _subset_vector_OR_factor_by_positions() and
 * _subset_vector_OR_factor_by_ranges()
 */

SEXP _subset_vector_OR_factor_by_positions(SEXP x, const int *pos, int npos)
{
	SEXP ans, x_names, ans_names, ans_class, ans_levels;

	PROTECT(ans = allocVector(TYPEOF(x), npos));

	/* Extract the values from 'x'. */
	_copy_vector_positions(ans, 0, x, pos, npos);

	/* Extract the names from 'x'. */
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(npos));
		_copy_vector_positions(ans_names, 0, x_names,
				       pos, npos);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}

	/* 'x' could be a factor in which case we need to propagate
	   its levels.  */
	if (isFactor(x)) {
		/* Levels must be set before class. */
		PROTECT(ans_levels = duplicate(GET_LEVELS(x)));
		SET_LEVELS(ans, ans_levels);
		UNPROTECT(1);
		PROTECT(ans_class = duplicate(GET_CLASS(x)));
		SET_CLASS(ans, ans_class);
		UNPROTECT(1);
	}

	UNPROTECT(1);
	return ans;
}

SEXP _subset_vector_OR_factor_by_ranges(SEXP x,
		const int *start, const int *width, int nranges)
{
	int x_len, i, ans_len, start_i, width_i, end_i;
	SEXP ans, x_names, ans_names, ans_class, ans_levels;

	x_len = LENGTH(x);
	_reset_ovflow_flag();
	for (i = ans_len = 0; i < nranges; i++) {
		start_i = start[i];
		if (start_i == NA_INTEGER || start_i < 1)
			error("'start' must be >= 1");
		width_i = width[i];
		if (width_i == NA_INTEGER || width_i < 0)
			error("'width' must be >= 0");
		end_i = start_i - 1 + width_i;
		if (end_i > x_len)
			error("'end' must be <= 'length(x)'");
		ans_len = _safe_int_add(ans_len, width_i);
	}
	if (_get_ovflow_flag())
		error("subscript is too big");
	PROTECT(ans = allocVector(TYPEOF(x), ans_len));

	/* Extract the values from 'x'. */
	_copy_vector_ranges(ans, 0, x, start, width, nranges);

	/* Extract the names from 'x'. */
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(ans_len));
		_copy_vector_ranges(ans_names, 0, x_names,
				    start, width, nranges);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}

	/* 'x' could be a factor in which case we need to propagate
	   its levels.  */
	if (isFactor(x)) {
		/* Levels must be set before class. */
		PROTECT(ans_levels = duplicate(GET_LEVELS(x)));
		SET_LEVELS(ans, ans_levels);
		UNPROTECT(1);
		PROTECT(ans_class = duplicate(GET_CLASS(x)));
		SET_CLASS(ans, ans_class);
		UNPROTECT(1);
	}

	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * vector_OR_factor_extract_positions() and vector_OR_factor_extract_ranges()
 */

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:   An atomic vector, or factor, or list.
 *   pos: Integer vector of positions to extract.
 * Return an object of the same type as 'x' (names and levels are propagated).
 */
SEXP vector_OR_factor_extract_positions(SEXP x, SEXP pos)
{
	int npos;

	npos = LENGTH(pos);
	return _subset_vector_OR_factor_by_positions(x, INTEGER(pos), npos);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:            An atomic vector, or factor, or list.
 *   start, width: Integer vectors of the same length defining the ranges to
 *                 extract.
 * Return an object of the same type as 'x' (names and levels are propagated).
 */
SEXP vector_OR_factor_extract_ranges(SEXP x, SEXP start, SEXP width)
{
	int nranges;
	const int *start_p, *width_p;

	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	return _subset_vector_OR_factor_by_ranges(x, start_p, width_p, nranges);
}

