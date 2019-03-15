#include "S4Vectors.h"

#include <stdlib.h>  /* for free() */

//#include <time.h>  /* for clock() */

static void invalid_byte_error(char byte, int pos)
{
	error("'x' contains an invalid byte (%d = char '%c') at position %d",
	      (int) byte, byte, pos);
}

static int memcpy_with_translation(char *dest, const char *src, int n,
				   const int *lkup, int lkup_len)
{
	int i, c;

	for (i = 0; i < n; i++) {
		c = translate_byte(src[i], lkup, lkup_len);
		if (c == NA_INTEGER)
			break;
		dest[i] = (char) c;
	}
	return i;
}

/* Return a single string (i.e. character vector of length 1). */
static SEXP extract_bytes_by_positions_as_one_string(
		const char *x, int x_len,
		const int *pos, int npos,
		const int *lkup, int lkup_len)
{
	char *dest, byte;
	int i, pos_i, c;
	SEXP ans, ans_elt;

	//clock_t t0 = clock();
	dest = (char *) malloc(npos);
	if (dest == NULL)
		error("memory allocation error in .Call entry point "
		      "C_extract_character_from_raw_by_positions()");
	//double dt = (1.0 * clock() - t0) / CLOCKS_PER_SEC;
	//printf("time for malloc(): %e\n", dt);

	x--;  /* so we can just do 'x[pos_i]' instead of 'x[pos_i - 1]'
	         in the loop below */

	//Surprisingly my timings show that the for loop below is faster
	//when 'lkup' is not NULL (i.e. when bytes are translated) than
	//when it's NULL (i.e. when bytes are NOT translated)!!! How could
	//this possibly make any sense?!!
	//t0 = clock();
	for (i = 0; i < npos; i++) {
		pos_i = pos[i];
		if (pos_i == NA_INTEGER || pos_i < 1 || pos_i > x_len) {
			free(dest);
			error("'pos[%d]' is NA or < 1 or > length(x)", i + 1);
		}
		byte = x[pos_i];
		if (lkup == NULL) {
			dest[i] = byte;
		} else {
			c = translate_byte(byte, lkup, lkup_len);
			if (c == NA_INTEGER) {
				free(dest);
				invalid_byte_error(byte, pos_i);
			}
			dest[i] = (char) c;
		}
	}
	//dt = (1.0 * clock() - t0) / CLOCKS_PER_SEC;
	//printf("time for for-loop: %e\n", dt);

	//t0 = clock();
	ans_elt = PROTECT(mkCharLen(dest, npos));
	ans = PROTECT(ScalarString(ans_elt));
	free(dest);
	UNPROTECT(2);
	//dt = (1.0 * clock() - t0) / CLOCKS_PER_SEC;
	//printf("time for making SEXP: %e\n", dt);
	return ans;
}

/* Return a character vector **parallel** to the set of positions specified
   via the 'pos' argument. Each element in the character vector is a 1-letter
   string. */
static SEXP extract_bytes_by_positions_as_strings(
		const char *x, int x_len,
		const int *pos, int npos,
		const int *lkup, int lkup_len)
{
	char dest[1], byte;
	int i, pos_i, c;
	SEXP ans, ans_elt;

	ans = PROTECT(NEW_CHARACTER(npos));
	x--;  /* so we can just do 'x[pos_i]' instead of 'x[pos_i - 1]'
	         in the loop below */
	for (i = 0; i < npos; i++) {
		pos_i = pos[i];
		if (pos_i == NA_INTEGER || pos_i < 1 || pos_i > x_len) {
			UNPROTECT(1);
			error("'pos[%d]' is NA or < 1 or > length(x)", i + 1);
		}
		byte = x[pos_i];
		if (lkup == NULL) {
			dest[0] = byte;
		} else {
			c = translate_byte(byte, lkup, lkup_len);
			if (c == NA_INTEGER)
				invalid_byte_error(byte, pos_i);
			dest[0] = (char) c;
		}
		ans_elt = PROTECT(mkCharLen(dest, 1));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* Return a single string (i.e. character vector of length 1). */
static SEXP extract_bytes_by_ranges_as_one_string(const char *x,
		const int *start, const int *width, int nranges,
		int totalchars,
		const int *lkup, int lkup_len)
{
	char *dest;
	int i, width_i, off;
	const char *src;
	SEXP ans, ans_elt;

	dest = (char *) malloc(totalchars);
	if (dest == NULL)
		error("memory allocation error in .Call entry point "
		      "C_extract_character_from_raw_by_ranges()");
	totalchars = 0;
	x--;  /* so we can just do 'x + start[i]' instead of 'x + start[i] - 1'
	         in the loop below */
	for (i = 0; i < nranges; i++) {
		src = x + start[i];
		width_i = width[i];
		if (lkup == NULL) {
			memcpy(dest + totalchars, src, width_i);
		} else {
			off = memcpy_with_translation(dest + totalchars, src,
						      width_i,
						      lkup, lkup_len);
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

/* Return a character vector **parallel** to the set of ranges specified
   via the 'start' and 'width' arguments. */
static SEXP extract_bytes_by_ranges_as_strings(const char *x,
		const int *start, const int *width, int nranges,
		int maxwidth,
		const int *lkup, int lkup_len)
{
	char *dest = NULL;
	int i, width_i, off;
	const char *src;
	SEXP ans, ans_elt;

	if (lkup != NULL) {
		dest = (char *) malloc(maxwidth);
		if (dest == NULL)
			error("memory allocation error in "
			      "C_extract_character_from_raw_by_ranges()");
	}
	ans = PROTECT(NEW_CHARACTER(nranges));
	x--;  /* so we can just do 'x + start[i]' instead of 'x + start[i] - 1'
	         in the loop below */
	for (i = 0; i < nranges; i++) {
		src = x + start[i];
		width_i = width[i];
		if (lkup == NULL) {
			ans_elt = PROTECT(mkCharLen(src, width_i));
		} else {
			off = memcpy_with_translation(dest, src, width_i,
						      lkup, lkup_len);
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
 * Use a single-pass algorithm.
 * If 'collapse' is FALSE, return a character vector **parallel** to the set
 * of positions specified via the 'pos' argument. Each element in the character
 * vector is a 1-letter string.
 * Otherwise return a single string (i.e. character vector of length 1).
 */
SEXP _extract_bytes_by_positions(const char *x, int x_len,
				 const int *pos, int npos,
				 int collapse, SEXP lkup)
{
	const int *lkup_p = NULL;
	int lkup_len = 0;

	if (lkup != R_NilValue) {
		if (!IS_INTEGER(lkup))
			error("'lkup' must an integer vector or NULL");
		lkup_p = INTEGER(lkup);
		lkup_len = LENGTH(lkup);
	}

	return collapse ?
		extract_bytes_by_positions_as_one_string(x, x_len,
							pos, npos,
							lkup_p, lkup_len) :
		extract_bytes_by_positions_as_strings(x, x_len,
							pos, npos,
							lkup_p, lkup_len);
}

/*
 * Use a 2-pass algorithm.
 * If 'collapse' is FALSE, return a character vector **parallel** to the set
 * of ranges specified via the 'start' and 'width' arguments.
 * Otherwise return a single string (i.e. character vector of length 1).
 */
SEXP _extract_bytes_by_ranges(const char *x, int x_len,
			      const int *start, const int *width, int nranges,
			      int collapse, SEXP lkup)
{
	const int *lkup_p = NULL;
	int lkup_len = 0, maxwidth, i, start_i, width_i, end_i;
	unsigned int totalchars;

	if (lkup != R_NilValue) {
		if (!IS_INTEGER(lkup))
			error("'lkup' must an integer vector or NULL");
		lkup_p = INTEGER(lkup);
		lkup_len = LENGTH(lkup);
	}

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
		extract_bytes_by_ranges_as_one_string(x,
				start, width, nranges, (int) totalchars,
				lkup_p, lkup_len) :
		extract_bytes_by_ranges_as_strings(x,
				start, width, nranges, maxwidth,
				lkup_p, lkup_len);
}

/* --- .Call ENTRY POINT --- */
SEXP C_extract_character_from_raw_by_positions(SEXP x, SEXP pos,
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
SEXP C_extract_character_from_raw_by_ranges(SEXP x, SEXP start, SEXP width,
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

