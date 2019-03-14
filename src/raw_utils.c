#include "S4Vectors.h"
#include <stdlib.h>  /* for free() */


static int memcpy_with_translation(char *dest, const char *src, int n,
				   const int *lkup, int lkup_length)
{
	int i, c;

	for (i = 0; i < n; i++) {
		c = translate_byte(src[i], lkup, lkup_length);
		if (c == NA_INTEGER)
			break;
		dest[i] = (char) c;
	}
	return i;
}

static void invalid_byte_error(char byte, int pos)
{
	error("'x' contains an invalid byte (%d = char '%c') at position %d",
	      (int) byte, byte, pos);
}

/* Return a character vector of length 1 (single string). */
static SEXP extract_bytes_by_positions_as_one_string(const char *x,
		const int *pos, int npos,
		SEXP lkup)
{
	char *dest;
	int i, c;
	const char *src;
	SEXP ans, ans_elt;

	dest = (char *) malloc(npos);
	if (dest == NULL)
		error("memory allocation error in .Call entry point "
		      "C_extract_raw_positions_as_character()");
	for (i = 0; i < npos; i++) {
		src = x + pos[i] - 1;
		if (lkup == R_NilValue) {
			dest[i] = *src;
		} else {
			c = translate_byte(*src, INTEGER(lkup), LENGTH(lkup));
			if (c == NA_INTEGER) {
				free(dest);
				invalid_byte_error(*src, pos[i]);
			}
			dest[i] = (char) c;
		}
	}
	ans_elt = PROTECT(mkCharLen(dest, npos));
	ans = PROTECT(ScalarString(ans_elt));
	free(dest);
	UNPROTECT(2);
	return ans;
}

/* Return a character vector with one 1-letter string per position. */
static SEXP extract_bytes_by_positions_as_strings(const char *x,
		const int *pos, int npos,
		SEXP lkup)
{
	char dest[1];
	int i, c;
	const char *src;
	SEXP ans, ans_elt;

	ans = PROTECT(NEW_CHARACTER(npos));
	for (i = 0; i < npos; i++) {
		src = x + pos[i] - 1;
		if (lkup == R_NilValue) {
			dest[0] = *src;
		} else {
			c = translate_byte(*src, INTEGER(lkup), LENGTH(lkup));
			if (c == NA_INTEGER)
				invalid_byte_error(*src, pos[i]);
			dest[0] = (char) c;
		}
		ans_elt = PROTECT(mkCharLen(dest, 1));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* Return a character vector of length 1 (single string). */
static SEXP extract_bytes_by_ranges_as_one_string(const char *x,
		const int *start, const int *width, int nranges,
		int totalchars, SEXP lkup)
{
	char *dest;
	int i, width_i, off;
	const char *src;
	SEXP ans, ans_elt;

	dest = (char *) malloc(totalchars);
	if (dest == NULL)
		error("memory allocation error in .Call entry point "
		      "C_extract_raw_ranges_as_character()");
	totalchars = 0;
	for (i = 0; i < nranges; i++) {
		src = x + start[i] - 1;
		width_i = width[i];
		if (lkup == R_NilValue) {
			memcpy(dest + totalchars, src, width_i);
		} else {
			off = memcpy_with_translation(dest + totalchars, src,
						      width_i,
						      INTEGER(lkup),
						      LENGTH(lkup));
			if (off != width_i) {
				free(dest);
				invalid_byte_error(src[off], start[i] + off);
			}
		}
		totalchars += width_i;
	}
	ans_elt = PROTECT(mkCharLen(dest, totalchars));
	ans = PROTECT(ScalarString(ans_elt));
	free(dest);
	UNPROTECT(2);
	return ans;
}

/* Return a character vector with one string per range. */
static SEXP extract_bytes_by_ranges_as_strings(const char *x,
		const int *start, const int *width, int nranges,
		int maxwidth, SEXP lkup)
{
	char *dest = NULL;
	int i, width_i, off;
	const char *src;
	SEXP ans, ans_elt;

	if (lkup != R_NilValue) {
		dest = (char *) malloc(maxwidth);
		if (dest == NULL)
			error("memory allocation error in "
			      "C_extract_raw_ranges_as_character()");
	}
	ans = PROTECT(NEW_CHARACTER(nranges));
	for (i = 0; i < nranges; i++) {
		src = x + start[i] - 1;
		width_i = width[i];
		if (lkup == R_NilValue) {
			ans_elt = PROTECT(mkCharLen(src, width_i));
		} else {
			off = memcpy_with_translation(dest, src, width_i,
						      INTEGER(lkup),
						      LENGTH(lkup));
			if (off != width_i) {
				free(dest);
				UNPROTECT(1);
				invalid_byte_error(src[off], start[i] + off);
			}
			ans_elt = PROTECT(mkCharLen(dest, width_i));
		}
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	if (dest != NULL)
		free(dest);
	UNPROTECT(1);
	return ans;
}

/*
 * Return a character vector with one 1-letter string per position if 'collapse'
 * is FALSE. Otherwise return a character vector of length 1 (single string).
 */
SEXP _extract_bytes_by_positions(const char *x, int x_len,
				 const int *pos, int npos,
				 int collapse, SEXP lkup)
{
	int i, pos_i;

	if (!(lkup == R_NilValue || IS_INTEGER(lkup)))
		error("'lkup' must an integer vector or NULL");

	/* 1st pass: Check the positions. */
	for (i = 0; i < npos; i++) {
		pos_i = pos[i];
		if (pos_i == NA_INTEGER || pos_i < 1 || pos_i > x_len)
			error("'pos[%d]' is NA or < 1 or > length(x)", i + 1);
	}

	/* 2nd pass: Extract the data into a character string. */
	return collapse ?
		extract_bytes_by_positions_as_one_string(x, pos, npos, lkup) :
		extract_bytes_by_positions_as_strings(x, pos, npos, lkup);
}

/*
 * Return a character vector with one string per range if 'collapse' is FALSE.
 * Otherwise return a character vector of length 1 (single string).
 */
SEXP _extract_bytes_by_ranges(const char *x, int x_len,
			      const int *start, const int *width, int nranges,
			      int collapse, SEXP lkup)
{
	int maxwidth, i, start_i, width_i, end_i;
	unsigned int totalchars;

	if (!(lkup == R_NilValue || IS_INTEGER(lkup)))
		error("'lkup' must an integer vector or NULL");

	/* 1st pass: Check the ranges and compute the total number of
	   characters to extract or the width of the biggest range. */
	if (collapse) {
		totalchars = 0;
	} else {
		maxwidth = 0;
	}
	for (i = 0; i < nranges; i++) {
		start_i = start[i];
		if (start_i == NA_INTEGER || start_i < 1)
			error("'start[%d]' is NA or < 1", i + 1);
		width_i = width[i];
		if (width_i == NA_INTEGER || width_i < 0)
			error("'width[%d]' is NA or < 0", i + 1);
		end_i = start_i - 1 + width_i;
		if (end_i > x_len)
			error("the range defined by 'start[%d]' and "
			      "'width[%d]' is not a\n  valid range on 'x'",
			      i + 1, i + 1);
		if (collapse) {
			totalchars += width_i;
			if (totalchars > INT_MAX)
				error("too many characters to extract");
		} else {
			if (width_i > maxwidth)
				maxwidth = width_i;
		}
	}

	/* 2nd pass: Extract the data into a character string. */
	return collapse ?
		extract_bytes_by_ranges_as_one_string(x, start, width,
					nranges, (int) totalchars, lkup) :
		extract_bytes_by_ranges_as_strings(x, start, width,
					nranges, maxwidth, lkup);
}

/* --- .Call ENTRY POINT --- */
SEXP C_extract_raw_positions_as_character(SEXP x, SEXP pos,
					  SEXP collapse, SEXP lkup)
{
	if (!IS_RAW(x))
		error("'x' must be a raw vector");

	if (!IS_INTEGER(pos))
		error("'pos' must be an integer vector");

	if (!(IS_LOGICAL(collapse) && LENGTH(collapse) == 1))
		error("'collapse' must be TRUE or FALSE");

	return _extract_bytes_by_positions((const char *) RAW(x), LENGTH(x),
				INTEGER(pos), LENGTH(pos),
				LOGICAL(collapse)[0], lkup);
}

/* --- .Call ENTRY POINT --- */
SEXP C_extract_raw_ranges_as_character(SEXP x, SEXP start, SEXP width,
				       SEXP collapse, SEXP lkup)
{
	int nranges;
	const int *start_p, *width_p;

	if (!IS_RAW(x))
		error("'x' must be a raw vector");

	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");

	if (!(IS_LOGICAL(collapse) && LENGTH(collapse) == 1))
		error("'collapse' must be TRUE or FALSE");

	return _extract_bytes_by_ranges((const char *) RAW(x), LENGTH(x),
				start_p, width_p, nranges,
				LOGICAL(collapse)[0], lkup);
}

