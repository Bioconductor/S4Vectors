/****************************************************************************
 *                  Map a set of ranges to a set of "runs"                  *
 *               ("runs" are just non-empty adjacent ranges)                *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"

#include <stdlib.h>  /* for malloc, free */


static char errmsg_buf[200];


/****************************************************************************
 * 2 low-level mappers that handle a single range only
 */

const char *simple_range_mapper(const int *run_lengths, int nrun,
		int range_start, int range_end,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int offset, i, j;

	if (range_start == NA_INTEGER || range_start < 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'start' must be >= 1");
		return errmsg_buf;
	}
	if (range_end == NA_INTEGER || range_end < range_start - 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'end' must be >= 'start' - 1");
		return errmsg_buf;
	}
	offset = 0;
	if (range_end >= range_start) {
		for (i = 0; i < nrun; i++) {
			offset += run_lengths[i];
			if (offset >= range_start)
				break;
		}
		if (i < nrun)
			*mapped_range_Ltrim = range_start - offset +
					      run_lengths[i] - 1;
		if (offset >= range_end) {
			j = i;
		} else {
			for (j = i + 1; j < nrun; j++) {
				offset += run_lengths[j];
				if (offset >= range_end)
					break;
			}
		}
		*mapped_range_Rtrim = offset - range_end;
		*mapped_range_span = j - i + 1;
	} else {
		/* Zero-width range. */
		*mapped_range_span = 0;
		j = -1;
		while (offset < range_end) {
			j++;
			if (j >= nrun)
				break;
			offset += run_lengths[j];
		}
		if (offset == range_end)
			i = j + 1;
		else
			i = j;
	}
	if (range_end > offset) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'end' must be <= 'length(x)'");
		return errmsg_buf;
	}
	*mapped_range_offset = i;
	return NULL;
}

static int int_bsearch(int x, const int *breakpoints, int nbreakpoints)
{
	int n1, n2, n, bp;

	if (nbreakpoints == 0)
		return nbreakpoints;

	/* Check last element. */
	n2 = nbreakpoints - 1;
	bp = breakpoints[n2];
	if (x > bp)
		return nbreakpoints;
	if (x == bp)
		return n2;

	/* Check first element. */
	n1 = 0;
	bp = breakpoints[n1];
	if (x <= bp)
		return n1;

	/* Binary search.
	   Seems that using >> 1 instead of / 2 is faster, even when compiling
	   with 'gcc -O2' (one would hope that the optimizer is able to do that
	   kind of optimization). */
	while ((n = (n1 + n2) >> 1) != n1) {
		bp = breakpoints[n];
		if (x == bp)
			return n;
		if (x > bp)
			n1 = n;
		else
			n2 = n;
	}
	return n2;
}

/*
 * Like simple_range_mapper() but takes 'run_breakpoints' (obtained with
 * 'cumsum(run_lengths)') instead of 'run_lengths' as input and uses a binary
 * search.
 */
static const char *bsearch_range_mapper(const int *run_breakpoints, int nrun,
		int range_start, int range_end,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int x_len, end_run;

	if (range_start == NA_INTEGER || range_start < 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'start' must be >= 1");
		return errmsg_buf;
	}
	x_len = nrun == 0 ? 0 : run_breakpoints[nrun - 1];
	if (range_end == NA_INTEGER || range_end > x_len) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'end' must be <= 'length(x)'");
		return errmsg_buf;
	}
	if (range_end < range_start - 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'end' must be >= 'start' - 1");
		return errmsg_buf;
	}
	*mapped_range_offset = int_bsearch(range_start, run_breakpoints, nrun);
	if (range_end >= range_start) {
		end_run = int_bsearch(range_end, run_breakpoints, nrun);
		*mapped_range_span = end_run - *mapped_range_offset + 1;
		*mapped_range_Ltrim = range_start - 1;
		if (*mapped_range_offset >= 1)
			*mapped_range_Ltrim -=
				run_breakpoints[*mapped_range_offset - 1];
		*mapped_range_Rtrim = run_breakpoints[end_run] - range_end;
	} else {
		/* Zero-width range. */
		*mapped_range_span = 0;
	}
	return NULL;
}


/****************************************************************************
 * map_ranges()
 */

/* Method 1: Naive algo (inefficient if more than 1 range). */
static const char *ranges_mapper1(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int i, start_i, end_i;
	const char *errmsg;

	errmsg = NULL;
	for (i = 0; i < nranges; i++) {
		start_i = start[i];
		end_i = start_i - 1 + width[i];
		errmsg = simple_range_mapper(run_lengths, nrun,
				start_i, end_i,
				mapped_range_offset + i,
				mapped_range_span + i,
				mapped_range_Ltrim + i,
				mapped_range_Rtrim + i);
		if (errmsg != NULL)
			break;
	}
	return errmsg;
}

/* Method 2: Binary search. */
static const char *ranges_mapper2(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int *run_breakpoints, breakpoint, i, start_i, end_i;
	const char *errmsg;

	run_breakpoints = (int *) malloc(sizeof(int) * nrun);
	if (run_breakpoints == NULL) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "ranges_mapper2: memory allocation failed");
		return errmsg_buf;
	}
	breakpoint = 0;
	for (i = 0; i < nrun; i++) {
		breakpoint += run_lengths[i];
		run_breakpoints[i] = breakpoint;
	}
	errmsg = NULL;
	for (i = 0; i < nranges; i++) {
		start_i = start[i];
		end_i = start_i - 1 + width[i];
		errmsg = bsearch_range_mapper(run_breakpoints, nrun,
				start_i, end_i,
				mapped_range_offset + i,
				mapped_range_span + i,
				mapped_range_Ltrim + i,
				mapped_range_Rtrim + i);
		if (errmsg != NULL)
			break;
	}
	free(run_breakpoints);
	return errmsg; 
}

/* Method 3: Sort the starts and ends of the ranges. */
static const char *ranges_mapper3(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int SEbuf_len, *SEbuf, *SEorder,
	    *SEbuf2, SE, breakpoint, i, j, k, SE_run;

	SEbuf_len = 2 * nranges;
	SEbuf = (int *) malloc(sizeof(int) * SEbuf_len);
	SEorder = (int *) malloc(sizeof(int) * SEbuf_len);
	if (SEbuf == NULL || SEorder == NULL) {
		if (SEbuf != NULL)
			free(SEbuf);
		if (SEorder != NULL)
			free(SEorder);
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "ranges_mapper3: memory allocation failed");
		return errmsg_buf;
	}
	memcpy(SEbuf, start, sizeof(int) * nranges);
	SEbuf2 = SEbuf + nranges;
	for (i = 0; i < nranges; i++)
		SEbuf2[i] = start[i] - 1 + width[i];
	_get_order_of_int_array(SEbuf, SEbuf_len, 0, SEorder, 0);
	breakpoint = j = 0;
	for (k = 0; k < SEbuf_len; k++) {
		i = SEorder[k];
		SE = SEbuf[i];
		while (breakpoint < SE && j < nrun)
			breakpoint += run_lengths[j++];
		if (i < nranges) {
			/* SE is a start. */
			if (SE < 1) {
				free(SEbuf);
				free(SEorder);
				snprintf(errmsg_buf, sizeof(errmsg_buf),
					 "'start' must be >= 1");
				return errmsg_buf;
			}
			mapped_range_Ltrim[i] = - breakpoint;
			if (SE > breakpoint) {
				SE_run = j;
			} else {
				SE_run = j - 1;
				mapped_range_Ltrim[i] += run_lengths[SE_run];
			}
			mapped_range_offset[i] = SE_run;
		} else {
			/* SE is an end. */
			if (SE > breakpoint) {
				free(SEbuf);
				free(SEorder);
				snprintf(errmsg_buf, sizeof(errmsg_buf),
					 "'end' must be <= 'length(x)'");
				return errmsg_buf;
			}
			i -= nranges;
			mapped_range_Rtrim[i] = breakpoint;
			SE_run = j - 1;
			mapped_range_span[i] = SE_run;
		}
	}
	for (i = 0; i < nranges; i++) {
		if (width[i] != 0) {
			mapped_range_span[i] -= mapped_range_offset[i] - 1;
			mapped_range_Ltrim[i] += start[i] - 1;
			mapped_range_Rtrim[i] -= SEbuf2[i];
		} else {
			/* Zero-width range. */
			mapped_range_span[i] = 0;
		}
	}
	free(SEbuf);
	free(SEorder);
	return NULL; 
}

/* If 'method' is not >= 0 and <= 3, then the function does nothing (no-op). */
const char *ranges_mapper(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim,
		int method)
{
	const char *(*fun)(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim);

	if (method == 0) {
		if (nranges == 1) {
			method = 1;
		} else {
			/* If nranges > 0.05 * nrun then use algo based on
			   binary search (method 2), otherwise use algo based
			   on qsort (method 3). The 5% cutoff is empirical
			   (based on timings obtained in January 2016 on a
			   Dell LATITUDE E6440 laptop running 64-bit Ubuntu
			   14.04.3 LTS). */
			method = nranges > 0.05 * nrun ? 2 : 3;
		}
	}
	switch (method) {
		case 1: fun = ranges_mapper1; break;
		case 2: fun = ranges_mapper2; break;
		case 3: fun = ranges_mapper3; break;
		default: return NULL;  /* do nothing */
	}
	return fun(run_lengths, nrun,
		   start, width, nranges,
		   mapped_range_offset,
		   mapped_range_span,
		   mapped_range_Ltrim,
		   mapped_range_Rtrim);
}

/* --- .Call ENTRY POINT ---
 * Return an *unnamed* list of 4 integer vectors. Each integer vector is
 * parallel to the input ranges (i.e. parallel to 'start' and 'width').
 * The i-th element of each integer vector forms a quadruplet of integers
 * that represents the i-th "mapped range". The 4 integers in the quadruplet
 * are:
 *   1. The "mapped range offset": this is the first run spanned by the
 *      mapped range (specified as a 0-based index).
 *   2. The "mapped range span": this is the nb of runs spanned by the
 *      mapped range.
 *   3. The "mapped range Ltrim": this is the nb of unspanned positions in the
 *      first spanned run.
 *   4. The "mapped range Rtrim": this is the nb of unspanned positions in the
 *      last spanned run.
 *
 * Example:
 *   - with 'run_lengths' set to c(9L, 15L, 17L, 11L) (i.e. 4 runs of lengths
 *     9, 15, 17, and 11, respectively).
 *   - with 'start' and 'width' set to 21L and 30L, respectively (i.e. a
 *     single range spanning positions 21 to 50).
 *
 *                1         2         3         4         5
 *       1234567890123456789012345678901234567890123456789012
 *
 *       <-run 1-><----run 2----><-----run 3-----><--run 4-->
 *                           <--range to map to the runs-->
 *
 *   Then the quadruplet of integers representing the "mapped range" is:
 *     1. mapped range offset:  1
 *     2. mapped range span:    3
 *     3. mapped range Ltrim:  11
 *     4. mapped range Rtrim:   2
 *   So S4Vectors:::map_ranges_to_runs(c(9L, 15L, 17L, 11L), 21L, 30L) will
 *   return list(1L, 3L, 11L, 2L).
 */
SEXP map_ranges(SEXP run_lengths, SEXP start, SEXP width, SEXP method)
{
	SEXP mapped_range_offset, mapped_range_span,
	     mapped_range_Ltrim, mapped_range_Rtrim, ans;
	int nrun, nranges;
	const int *start_p, *width_p;
	const char *errmsg;

	nrun = LENGTH(run_lengths);
	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	PROTECT(mapped_range_offset = NEW_INTEGER(nranges));
	PROTECT(mapped_range_span = NEW_INTEGER(nranges));
	PROTECT(mapped_range_Ltrim = NEW_INTEGER(nranges));
	PROTECT(mapped_range_Rtrim = NEW_INTEGER(nranges));
	errmsg = ranges_mapper(INTEGER(run_lengths), nrun,
			start_p, width_p, nranges,
			INTEGER(mapped_range_offset),
			INTEGER(mapped_range_span),
			INTEGER(mapped_range_Ltrim),
			INTEGER(mapped_range_Rtrim),
			INTEGER(method)[0]);
	if (errmsg != NULL) {
		UNPROTECT(4);
		error(errmsg);
	}
	PROTECT(ans = NEW_LIST(4));
	SET_VECTOR_ELT(ans, 0, mapped_range_offset);
	SET_VECTOR_ELT(ans, 1, mapped_range_span);
	SET_VECTOR_ELT(ans, 2, mapped_range_Ltrim);
	SET_VECTOR_ELT(ans, 3, mapped_range_Rtrim);
	UNPROTECT(5);
	return ans;
}

