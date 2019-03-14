#include "S4Vectors.h"
#include <stdlib.h>  /* for free() */

static size_t memcpy_with_translation(char *dest, const Rbyte *src, size_t n,
				      const int *lkup, int lkup_length)
{
	size_t i;
	int c;

	for (i = 0; i < n; i++) {
		c = translate_byte(src[i], lkup, lkup_length);
		if (c == NA_INTEGER)
			break;
		*(dest++) = c;
	}
	return i;
}

static void invalid_byte_error(char byte, size_t at)
{
	error("'x' contains an invalid byte (%d = char '%c') at position %lu",
	      (int) byte, byte, at);
}

static SEXP read_data_as_one_string(const Rbyte *x_dataptr,
				    const int *start_p, const int *width_p,
				    int nranges, int totalchars, SEXP lkup)
{
	char *dest;
	int i, width_i;
	const Rbyte *src;
	size_t off;
	SEXP ans, ans_elt;

	dest = (char *) malloc(totalchars);
	if (dest == NULL)
		error("memory allocation error in .Call entry point "
		      "C_extract_raw_ranges_as_character()");
	totalchars = 0;
	for (i = 0; i < nranges; i++) {
		src = x_dataptr + start_p[i] - 1;
		width_i = width_p[i];
		if (lkup == R_NilValue) {
			memcpy(dest + totalchars, src, width_i);
		} else {
			off = memcpy_with_translation(dest + totalchars, src,
						      width_i,
						      INTEGER(lkup),
						      LENGTH(lkup));
			if (off != width_i) {
				free(dest);
				invalid_byte_error(src[off], start_p[i] + off);
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

static SEXP read_data_as_strings(const Rbyte *x_dataptr,
				 const int *start_p, const int *width_p,
				 int nranges, int maxwidth, SEXP lkup)
{
	char *dest = NULL;
	int i, width_i;
	const Rbyte *src;
	size_t off;
	SEXP ans, ans_elt;

	if (lkup != R_NilValue) {
		dest = (char *) malloc(maxwidth);
		if (dest == NULL)
			error("memory allocation error in "
			      "C_extract_raw_ranges_as_character()");
	}
	ans = PROTECT(NEW_CHARACTER(nranges));
	for (i = 0; i < nranges; i++) {
		src = x_dataptr + start_p[i] - 1;
		width_i = width_p[i];
		if (lkup == R_NilValue) {
			ans_elt = PROTECT(mkCharLen((const char *) src,
					  width_i));
		} else {
			off = memcpy_with_translation(dest, src, width_i,
						      INTEGER(lkup),
						      LENGTH(lkup));
			if (off != width_i) {
				free(dest);
				UNPROTECT(1);
				invalid_byte_error(src[off], start_p[i] + off);
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

/* --- .Call ENTRY POINT ---
 * Return a character vector with 1 string per range if 'collapse' is FALSE.
 * Otherwise return a character vector of length 1 (single string).
 */
SEXP C_extract_raw_ranges_as_character(SEXP x, SEXP start, SEXP width,
				       SEXP collapse, SEXP lkup)
{
	int x_len, nranges, collapse0, maxwidth, i, start_i, width_i, end_i;
	const Rbyte *x_dataptr;
	const int *start_p, *width_p;
	unsigned int totalchars;

	if (!IS_RAW(x))
		error("'x' must be a raw vector");
	x_len = LENGTH(x);
	x_dataptr = RAW(x);
	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	if (!(IS_LOGICAL(collapse) && LENGTH(collapse) == 1))
		error("'collapse' must be TRUE or FALSE");
	collapse0 = INTEGER(collapse)[0];
	if (!(lkup == R_NilValue || IS_INTEGER(lkup)))
		error("'lkup' must an integer vector or NULL");

	/* 1st pass: Check the ranges and compute the total number of
           characters to read or the width of the biggest range. */
	if (collapse0) {
		totalchars = 0;
	} else {
		maxwidth = 0;
	}
	for (i = 0; i < nranges; i++) {
		start_i = start_p[i];
		if (start_i == NA_INTEGER || start_i < 1)
			error("'start[%d]' is NA or < 1", i + 1);
		width_i = width_p[i];
		if (width_i == NA_INTEGER || width_i < 0)
			error("'width[%d]' is NA or < 0", i + 1);
		end_i = start_i - 1 + width_i;
		if (end_i > x_len)
			error("the range defined by 'start[%d]' and "
			      "'width[%d]' is not a\n  valid range on 'x'",
			      i + 1, i + 1);
		if (collapse0) {
			totalchars += width_i;
			if (totalchars > INT_MAX)
				error("too many characters to read");
		} else {
			if (width_i > maxwidth)
				maxwidth = width_i;
		}
	}

	/* 2nd pass: Read the data in the ranges into a character string. */
	return collapse0 ?
		read_data_as_one_string(x_dataptr, start_p, width_p,
					nranges, (int) totalchars, lkup) :
		read_data_as_strings(x_dataptr, start_p, width_p,
					nranges, maxwidth, lkup);
}

