/****************************************************************************
 *                  Map a set of ranges to a set of "runs"                  *
 *               ("runs" are just non-empty adjacent ranges)                *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"

#include <stdlib.h>  /* for malloc, free */
#include <limits.h>  /* for INT_MAX */

static char errmsg_buf[200];

/* Mapping ranges or positions to a set of run is used in the context
   of subsetting some Vector derivative (like Rle and GPos objects), so
   we try to display error messages that makes sense in that context. */

static char *VECTOR_TOO_LONG_errmsg()
{
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "subsetting a Vector derivative of length "
		 "2^31 or more is not suppported yet");
	return errmsg_buf;
}

static char *NA_INDICES_errmsg()
{
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "subscript contains NAs");
	return errmsg_buf;
}

static char *OUTOFBOUND_INDICES_errmsg()
{
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "subscript contains out-of-bounds indices");
	return errmsg_buf;
}

static char *INVALID_RANGES_errmsg()
{
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "subscript contains invalid ranges "
		 "(in a valid range 'start'/'end'/'width'\n"
		 "  cannot be NA and 'width' must be >= 0)");
	return errmsg_buf;
}

static char *OUTOFBOUND_RANGES_errmsg()
{
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "subscript contains out-of-bounds ranges");
	return errmsg_buf;
}


/****************************************************************************
 * 1st mapping method
 *
 * Use a naive algo (inefficient if more than 1 range to map).
 * Advantage: simple, memory efficient (unlike the other methods, it doesn't
 * require allocating any temporary vector), and can be used as a reference
 * to validate the other slightly more complex methods.
 */

/* Low-level mapper that takes as input a single range only */
const char *_simple_range_mapper(
		const int *run_lengths, int nrun,
		int range_start, int range_end,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	unsigned int offset;
	int i, j;

	if (range_start == NA_INTEGER
	 || range_end == NA_INTEGER
	 || range_end < range_start - 1)
		return INVALID_RANGES_errmsg();
	if (range_start < 1)
		return OUTOFBOUND_RANGES_errmsg();
	offset = 0;
	if (range_end >= range_start) {
		for (i = 0; i < nrun; i++) {
			offset += run_lengths[i];
			if (offset > INT_MAX)
				return VECTOR_TOO_LONG_errmsg();
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
				if (offset > INT_MAX)
					return VECTOR_TOO_LONG_errmsg();
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
			if (offset > INT_MAX)
				return VECTOR_TOO_LONG_errmsg();
		}
		if (offset == range_end)
			i = j + 1;
		else
			i = j;
	}
	if (range_end > offset)
		return OUTOFBOUND_RANGES_errmsg();
	*mapped_range_offset = i;
	return NULL;
}

/* Low-level mapper that takes as input a single position only */
const char *_simple_position_mapper(
		const int *run_lengths, int nrun,
		int pos, int *mapped_pos)
{
	unsigned int offset;
	int i;

	if (pos == NA_INTEGER)
		return NA_INDICES_errmsg();
	if (pos < 1)
		return OUTOFBOUND_INDICES_errmsg();
	offset = 0;
	for (i = 0; i < nrun; i++) {
		offset += run_lengths[i];
		if (offset > INT_MAX)
			return VECTOR_TOO_LONG_errmsg();
		if (offset >= pos)
			break;
	}
	if (pos > offset)
		return OUTOFBOUND_INDICES_errmsg();
	*mapped_pos = i + 1;
	return NULL;
}

static const char *ranges_mapper1(
		const int *run_lengths, int nrun,
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
		errmsg = _simple_range_mapper(
				run_lengths, nrun,
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

static const char *positions_mapper1(
		const int *run_lengths, int nrun,
		const int *pos, int npos, int *mapped_pos)
{
	int i;
	const char *errmsg;

	errmsg = NULL;
	for (i = 0; i < npos; i++) {
		errmsg = _simple_position_mapper(
				run_lengths, nrun,
				pos[i], mapped_pos + i);
		if (errmsg != NULL)
			break;
	}
	return errmsg;
}


/****************************************************************************
 * 2nd mapping method
 *
 * Use a binary search to map the ranges to the ending positions of the runs
 * (called "run breakpoints").
 */

/* Binary search. */
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

/* Low-level mapper that takes as input a single range only */
static const char *bsearch_range_mapper(
		const int *run_breakpoints, int nrun,
		int range_start, int range_end,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int x_len, end_run;

	if (range_start == NA_INTEGER
	 || range_end == NA_INTEGER
	 || range_end < range_start - 1)
		return INVALID_RANGES_errmsg();
	x_len = nrun == 0 ? 0 : run_breakpoints[nrun - 1];
	if (range_start < 1 || range_end > x_len)
		return OUTOFBOUND_RANGES_errmsg();
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

/* Low-level mapper that takes as input a single position only */
static const char *bsearch_position_mapper(
		const int *run_breakpoints, int nrun,
		int pos, int *mapped_pos)
{
	int x_len;

	x_len = nrun == 0 ? 0 : run_breakpoints[nrun - 1];
	if (pos == NA_INTEGER)
		return NA_INDICES_errmsg();
	if (pos < 1 || pos > x_len)
		return OUTOFBOUND_INDICES_errmsg();
	*mapped_pos = int_bsearch(pos, run_breakpoints, nrun) + 1;
	return NULL;
}

static int *alloc_and_compute_run_breakpoints(const int *run_lengths, int nrun)
{
	int *run_breakpoints;
	unsigned int breakpoint;
	int i;

	run_breakpoints = (int *) malloc(sizeof(int) * nrun);
	if (run_breakpoints == NULL) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "failed to allocate temporary vector of breakpoints");
		return NULL;
	}
	breakpoint = 0;
	for (i = 0; i < nrun; i++) {
		breakpoint += run_lengths[i];
		if (breakpoint > INT_MAX) {
			free(run_breakpoints);
			VECTOR_TOO_LONG_errmsg();
			return NULL;
		}
		run_breakpoints[i] = breakpoint;
	}
	return run_breakpoints;
}

static const char *ranges_mapper2(
		const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int *run_breakpoints, i, start_i, end_i;
	const char *errmsg;

	run_breakpoints = alloc_and_compute_run_breakpoints(run_lengths, nrun);
	if (run_breakpoints == NULL)
		return errmsg_buf;
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

static const char *positions_mapper2(
		const int *run_lengths, int nrun,
		const int *pos, int npos, int *mapped_pos)
{
	int *run_breakpoints, i;
	const char *errmsg;

	run_breakpoints = alloc_and_compute_run_breakpoints(run_lengths, nrun);
	if (run_breakpoints == NULL)
		return errmsg_buf;
	errmsg = NULL;
	for (i = 0; i < npos; i++) {
		errmsg = bsearch_position_mapper(run_breakpoints, nrun,
				pos[i], mapped_pos + i);
		if (errmsg != NULL)
			break;
	}
	free(run_breakpoints);
	return errmsg; 
}


/****************************************************************************
 * 3rd mapping method
 *
 * Use a radix sort to sort the ranges or positions to map.
 */

/* Sort the starting and ending positions of the ranges in ascending order
   before mapping them to the runs. */
static const char *ranges_mapper3(
		const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim)
{
	int SEbuf_len, *SEbuf, *SEorder, *SEbuf2, SE, i, j, k, SE_run;
	unsigned int breakpoint;

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

	/* Use radix sort to find order of values in 'SEbuf'. */
	for (i = 0; i < SEbuf_len; i++)
		SEorder[i] = i;
	_sort_ints(SEorder, SEbuf_len, SEbuf, 0, 1, NULL, NULL);

	breakpoint = j = 0;
	for (k = 0; k < SEbuf_len; k++) {
		i = SEorder[k];
		SE = SEbuf[i];
		while (breakpoint < SE && j < nrun) {
			breakpoint += run_lengths[j++];
			if (breakpoint > INT_MAX) {
				free(SEbuf);
				free(SEorder);
				return VECTOR_TOO_LONG_errmsg();
			}
		}
		if (i < nranges) {
			/* SE is a start. */
			if (SE < 1) {
				free(SEbuf);
				free(SEorder);
				return OUTOFBOUND_RANGES_errmsg();
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
				return OUTOFBOUND_RANGES_errmsg();
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

/* Sort the positions in ascending order before mapping them to the runs. */
static const char *positions_mapper3(
		const int *run_lengths, int nrun,
		const int *pos, int npos, int *mapped_pos)
{
	int *POSorder, POS, i, j, k, POS_run;
	unsigned int breakpoint;

	POSorder = (int *) malloc(sizeof(int) * npos);
	if (POSorder == NULL) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "positions_mapper3: memory allocation failed");
		return errmsg_buf;
	}

	/* Use radix sort to find order of values in 'pos'. */
	for (i = 0; i < npos; i++)
		POSorder[i] = i;
	_sort_ints(POSorder, npos, pos, 0, 1, NULL, NULL);

	breakpoint = j = 0;
	for (k = 0; k < npos; k++) {
		i = POSorder[k];
		POS = pos[i];
		while (breakpoint < POS && j < nrun) {
			breakpoint += run_lengths[j++];
			if (breakpoint > INT_MAX) {
				free(POSorder);
				return VECTOR_TOO_LONG_errmsg();
			}
		}
		if (POS == NA_INTEGER) {
			free(POSorder);
			return NA_INDICES_errmsg();
		}
		if (POS < 1 || POS > breakpoint) {
			free(POSorder);
			return OUTOFBOUND_INDICES_errmsg();
		}
		if (POS > breakpoint) {
			POS_run = j + 1;
		} else {
			POS_run = j;
		}
		mapped_pos[i] = POS_run;
	}
	free(POSorder);
	return NULL; 
}


/****************************************************************************
 * _ranges_mapper() and _positions_mapper()
 *
 * If 'method' is 0, then the "best" method is automatically choosen.
 * If 'method' is not >= 0 and <= 3, then these functions do nothing (no-op).
 */

static int choose_best_method(int nranges, int nrun, double cutoff)
{
	if (nranges == 0)
		return -1;  /* will do nothing */
	if (nranges == 1)
		return 1;
	return nranges <= cutoff * nrun ? 3 : 2;
}

const char *_ranges_mapper(
		const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim,
		int method)
{
	const char *(*fun)(
		const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *mapped_range_offset,
		int *mapped_range_span,
		int *mapped_range_Ltrim,
		int *mapped_range_Rtrim);

	if (method == 0) {
		/* If nranges <= 0.25 * nrun then use algo based on radix
		   sort (method 3), otherwise use algo based on binary
		   search (method 2). This cutoff is totally empirical and
		   is based on some very shallow testing and timings obtained
		   in June 2017 on my laptop (Dell LATITUDE E6440 with 4Gb of
		   RAM and running 64-bit Ubuntu 14.04.5 LTS). */
		method = choose_best_method(nranges, nrun, 0.25);
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

const char *_positions_mapper(
		const int *run_lengths, int nrun,
		const int *pos, int npos, int *mapped_pos,
		int method)
{
	const char *(*fun)(
		const int *run_lengths, int nrun,
		const int *pos, int npos, int *mapped_pos);

	if (method == 0) {
		/* If npos <= 0.75 * nrun then use algo based on radix
		   sort (method 3), otherwise use algo based on binary
		   search (method 2). This cutoff is totally empirical and
		   is based on some very shallow testing and timings obtained
		   in June 2017 on my laptop (Dell LATITUDE E6440 with 4Gb of
		   RAM and running 64-bit Ubuntu 14.04.5 LTS). */
		method = choose_best_method(npos, nrun, 0.75);
	}
	switch (method) {
		case 1: fun = positions_mapper1; break;
		case 2: fun = positions_mapper2; break;
		case 3: fun = positions_mapper3; break;
		default: return NULL;  /* do nothing */
	}
	return fun(run_lengths, nrun, pos, npos, mapped_pos);
}


/****************************************************************************
 * map_ranges() and map_positions()
 *
 * Both functions assume that 'run_lengths' is an integer vector of positive
 * values with no NAs. For efficiency reasons this is trusted and the
 * functions don't check it.
 */

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
	errmsg = _ranges_mapper(INTEGER(run_lengths), nrun,
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

/* --- .Call ENTRY POINT --- */
SEXP map_positions(SEXP run_lengths, SEXP pos, SEXP method)
{
	SEXP mapped_pos;
	int nrun, npos;
	const char *errmsg;

	nrun = LENGTH(run_lengths);
	npos = LENGTH(pos);
	PROTECT(mapped_pos = NEW_INTEGER(npos));
	errmsg = _positions_mapper(INTEGER(run_lengths), nrun,
			INTEGER(pos), npos,
			INTEGER(mapped_pos),
			INTEGER(method)[0]);
	if (errmsg != NULL) {
		UNPROTECT(1);
		error(errmsg);
	}
	UNPROTECT(1);
	return mapped_pos;
}

