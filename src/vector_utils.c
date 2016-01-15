/****************************************************************************
 *               Low-level manipulation of ordinary R vectors               *
 ****************************************************************************/
#include "S4Vectors.h"


/****************************************************************************
 * memcmp()-based comparison of 2 vectors of the same type.
 * NOTE: Doesn't support STRSXP and VECSXP.
 */

int _vector_memcmp(SEXP x1, int x1_offset, SEXP x2, int x2_offset, int nelt)
{
	const void *s1 = NULL, *s2 = NULL; /* gcc -Wall */
	size_t eltsize = 0; /* gcc -Wall */

	if (x1_offset < 0 || x1_offset + nelt > LENGTH(x1)
	 || x2_offset < 0 || x2_offset + nelt > LENGTH(x2))
		error("S4Vectors internal error in _vector_memcmp(): "
		      "elements to compare are out of vector bounds");
	switch (TYPEOF(x1)) {
	case RAWSXP:
		s1 = (const void *) (RAW(x1) + x1_offset);
		s2 = (const void *) (RAW(x2) + x2_offset);
		eltsize = sizeof(Rbyte);
		break;
	case LGLSXP:
	case INTSXP:
		s1 = (const void *) (INTEGER(x1) + x1_offset);
		s2 = (const void *) (INTEGER(x2) + x2_offset);
		eltsize = sizeof(int);
		break;
	case REALSXP:
		s1 = (const void *) (REAL(x1) + x1_offset);
		s2 = (const void *) (REAL(x2) + x2_offset);
		eltsize = sizeof(double);
		break;
	case CPLXSXP:
		s1 = (const void *) (COMPLEX(x1) + x1_offset);
		s2 = (const void *) (COMPLEX(x2) + x2_offset);
		eltsize = sizeof(Rcomplex);
		break;
	default:
		error("S4Vectors internal error in _vector_memcmp(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(x1))));
	}
	return s1 == s2 ? 0 : memcmp(s1, s2, nelt * eltsize);
}


/****************************************************************************
 * memcpy()-based copy of data from a vector to a vector of the same type.
 */

/* Return new 'dest_offset'. */
int _copy_vector_block(SEXP dest, int dest_offset,
		SEXP src, int src_offset, int block_width)
{
	int new_dest_offset, i;
	void *dest_p;
	const void *src_p;
	size_t elt_size;
	SEXP src_elt; // dest_elt;

	if (block_width < 0)
		error("negative widths are not allowed");
	new_dest_offset = dest_offset + block_width;
	if (dest_offset < 0 || new_dest_offset > LENGTH(dest)
	 || src_offset < 0 || src_offset + block_width > LENGTH(src))
		error("subscripts out of bounds");
	switch (TYPEOF(dest)) {
	    case RAWSXP:
		dest_p = (void *) (RAW(dest) + dest_offset);
		src_p = (const void *) (RAW(src) + src_offset);
		elt_size = sizeof(Rbyte);
		break;
	    case LGLSXP:
		dest_p = (void *) (LOGICAL(dest) + dest_offset);
		src_p = (const void *) (LOGICAL(src) + src_offset);
		elt_size = sizeof(int);
		break;
	    case INTSXP:
		dest_p = (void *) (INTEGER(dest) + dest_offset);
		src_p = (const void *) (INTEGER(src) + src_offset);
		elt_size = sizeof(int);
		break;
	    case REALSXP:
		dest_p = (void *) (REAL(dest) + dest_offset);
		src_p = (const void *) (REAL(src) + src_offset);
		elt_size = sizeof(double);
		break;
	    case CPLXSXP:
		dest_p = (void *) (COMPLEX(dest) + dest_offset);
		src_p = (const void *) (COMPLEX(src) + src_offset);
		elt_size = sizeof(Rcomplex);
		break;
	    case STRSXP:
		for (i = 0; i < block_width; i++) {
			src_elt = STRING_ELT(src, src_offset + i);
			SET_STRING_ELT(dest, dest_offset + i, src_elt);
			//PROTECT(dest_elt = duplicate(src_elt));
			//SET_STRING_ELT(dest, dest_offset + i, dest_elt);
			//UNPROTECT(1);
		}
		return new_dest_offset;
	    case VECSXP:
		for (i = 0; i < block_width; i++) {
			src_elt = VECTOR_ELT(src, src_offset + i);
			SET_VECTOR_ELT(dest, dest_offset + i, src_elt);
			//PROTECT(dest_elt = duplicate(src_elt));
			//SET_VECTOR_ELT(dest, dest_offset + i, dest_elt);
			//UNPROTECT(1);
		}
		return new_dest_offset;
	    default:
		error("S4Vectors internal error in _copy_vector_block(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(dest))));
	}
	memcpy(dest_p, src_p, elt_size * block_width);
	return new_dest_offset;
}

/* Return new 'dest_offset'. */
int _copy_vector_blocks(SEXP dest, int dest_offset,
		SEXP src, const int *src_offset, const int *block_width,
		int nblock)
{
	int i;

	for (i = 0; i < nblock; i++)
		dest_offset = _copy_vector_block(dest, dest_offset,
						 src, src_offset[i],
						 block_width[i]);
	return dest_offset;
}

/*
 * Same as _copy_vector_blocks() above except that it takes a vector of starts
 * instead of a vector of offsets.
 */
int _copy_vector_ranges(SEXP dest, int dest_offset,
		SEXP src, const int *start, const int *width,
		int nranges)
{
	int i;

	for (i = 0; i < nranges; i++)
		dest_offset = _copy_vector_block(dest, dest_offset,
						 src, start[i] - 1,
						 width[i]);
	return dest_offset;
}


/****************************************************************************
 * vector_subsetByRanges() and vector_seqselect()
 *
 * TODO: These 2 functions are redundant. We only need one: the fastest.
 */

/* --- .Call ENTRY POINT ---
 * 'start' and 'width': integer vectors of the same length with no NAs. 'width'
 * is assumed to be >= 0.
 */
SEXP vector_subsetByRanges(SEXP x, SEXP start, SEXP width)
{
	int x_len, nranges, ans_len, i, offset_i, width_i, end_i;
	const int *start_p, *width_p;
	SEXP ans, x_names, ans_names;

	x_len = LENGTH(x);
	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	ans_len = 0;
	_reset_ovflow_flag();
	for (i = 0; i < nranges; i++) {
		width_i = width_p[i];
		if (width_i == NA_INTEGER || width_i < 0)
			error("'width' cannot contain NAs or negative values");
		offset_i = start_p[i] - 1;
		end_i = offset_i + width_i;
		if (offset_i < 0 || end_i > x_len)
			error("some ranges are out of bounds");
		ans_len = _safe_int_add(ans_len, width_i);
	}
	if (_get_ovflow_flag())
		error("Subsetting %s object by subscript containing "
		      "ranges produces a result\n  of length > %d. "
		      "This is not supported yet.",
		      CHAR(type2str(TYPEOF(x))), INT_MAX);
	PROTECT(ans = allocVector(TYPEOF(x), ans_len));
	_copy_vector_ranges(ans, 0, x, start_p, width_p, nranges);
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(ans_len));
		_copy_vector_ranges(ans_names, 0, x_names,
				    start_p, width_p, nranges);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * TODO: Remove this at some point (use vector_subsetByRanges instead).
 */
SEXP vector_seqselect(SEXP x, SEXP start, SEXP width)
{
	int ans_offset, i, j, s, w;
	SEXP ans, ans_names;

	if (!IS_INTEGER(start))
		error("'start' must be an integer vector");
	if (!IS_INTEGER(width))
		error("'width' must be an integer vector");
	if (LENGTH(start) != LENGTH(width))
		error("length of 'start' must equal length of 'width'");

	ans_offset = 0;
	_reset_ovflow_flag();
	for (i = 0; i < LENGTH(start); i++) {
		s = INTEGER(start)[i];
		w = INTEGER(width)[i];
		if (s == NA_INTEGER || s < 1)
			error("each element in 'start' must be a positive integer");
		if (w == NA_INTEGER || w < 0)
			error("each element in 'width' must be a non-negative integer");
		if (LENGTH(x) < s + w - 1)
			error("some ranges are out of bounds");
		ans_offset = _safe_int_add(ans_offset, w);
	}
	if (_get_ovflow_flag())
		error("Subsetting %s object by subscript containing "
		      "ranges produces a result\n  of length > %d. "
		      "This is not supported yet.",
		      CHAR(type2str(TYPEOF(x))), INT_MAX);
	PROTECT(ans = allocVector(TYPEOF(x), ans_offset));

	for (i = ans_offset = 0; i < LENGTH(start); i++, ans_offset += w) {
		s = INTEGER(start)[i] - 1;
		w = INTEGER(width)[i];
		switch (TYPEOF(x)) {
		    case LGLSXP:
		    case INTSXP:
			memcpy(INTEGER(ans) + ans_offset, INTEGER(x) + s, w * sizeof(int));
			break;
		    case REALSXP:
			memcpy(REAL(ans) + ans_offset, REAL(x) + s, w * sizeof(double));
			break;
		    case CPLXSXP:
			memcpy(COMPLEX(ans) + ans_offset, COMPLEX(x) + s, w * sizeof(Rcomplex));
			break;
		    case STRSXP:
			for (j = 0; j < w; j++)
				SET_STRING_ELT(ans, ans_offset + j, STRING_ELT(x, s + j));
			break;
		    case VECSXP:
			for (j = 0; j < w; j++)
				SET_VECTOR_ELT(ans, ans_offset + j, VECTOR_ELT(x, s + j));
			break;
		    case RAWSXP:
			memcpy(RAW(ans) + ans_offset, RAW(x) + s, w * sizeof(char));
			break;
		    default:
			error("S4Vectors internal error in vector_seqselect(): "
			      "%s type not supported",
			      CHAR(type2str(TYPEOF(x))));
		}
	}
	ans_names = GET_NAMES(x);
	if (ans_names != R_NilValue)
		SET_NAMES(ans, vector_seqselect(ans_names, start, width));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * sapply_NROW()
 */

static int get_NROW(SEXP x)
{
	SEXP x_dim, x_rownames;

	if (x == R_NilValue)
		return 0;
	if (!IS_VECTOR(x))
		error("get_NROW() defined only on a vector (or NULL)");
	/* A data.frame doesn't have a "dim" attribute but the dimensions can
	   be inferred from the "names" and "row.names" attributes. */
	x_rownames = getAttrib(x, R_RowNamesSymbol);
	if (x_rownames != R_NilValue)
		return LENGTH(x_rownames);
	x_dim = GET_DIM(x);
	if (x_dim == R_NilValue || LENGTH(x_dim) == 0)
		return LENGTH(x);
	return INTEGER(x_dim)[0];
}

/*
 * --- .Call ENTRY POINT ---
 * A C implementation of 'sapply(x, NROW)' that works only on a list of
 * vectors (or NULLs).
 */
SEXP sapply_NROW(SEXP x)
{
	SEXP ans, x_elt;
	int x_len, i, *ans_elt;

	x_len = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		x_elt = VECTOR_ELT(x, i);
		if (x_elt != R_NilValue && !IS_VECTOR(x_elt)) {
			UNPROTECT(1);
			error("element %d not a vector (or NULL)", i + 1);
		}
		*ans_elt = get_NROW(x_elt);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * _list_as_data_frame()
 */

/* Performs IN-PLACE coercion of list 'x' into a data frame! */
SEXP _list_as_data_frame(SEXP x, int nrow)
{
	SEXP rownames, class;
	int i;

	if (!IS_LIST(x) || GET_NAMES(x) == R_NilValue)
		error("S4Vectors internal error in _list_as_data_frame(): "
		      "'x' must be a named list");

	/* Set the "row.names" attribute. */
	PROTECT(rownames = NEW_INTEGER(nrow));
	for (i = 0; i < nrow; i++)
		INTEGER(rownames)[i] = i + 1;
	SET_ATTR(x, R_RowNamesSymbol, rownames);
	UNPROTECT(1);

	/* Set the "class" attribute. */
	PROTECT(class = mkString("data.frame"));
	SET_CLASS(x, class);
	UNPROTECT(1);
	return x;
}

