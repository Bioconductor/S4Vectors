/****************************************************************************
 *                  Low-level manipulation of Hits objects                  *
 ****************************************************************************/
#include "S4Vectors.h"


/****************************************************************************
 * C-level constructors
 */

static SEXP new_Hits0(SEXP queryHits, SEXP subjectHits,
		      int q_len, int s_len)
{
	SEXP classdef, ans, ans_queryLength, ans_subjectLength;

	PROTECT(classdef = MAKE_CLASS("Hits"));
	PROTECT(ans = NEW_OBJECT(classdef));

	SET_SLOT(ans, install("queryHits"), queryHits);
	SET_SLOT(ans, install("subjectHits"), subjectHits);

	PROTECT(ans_queryLength = ScalarInteger(q_len));
	SET_SLOT(ans, install("queryLength"), ans_queryLength);
	UNPROTECT(1);

	PROTECT(ans_subjectLength = ScalarInteger(s_len));
	SET_SLOT(ans, install("subjectLength"), ans_subjectLength);
	UNPROTECT(1);

	UNPROTECT(2);
	return ans;
}

static SEXP new_Hits1(const int *q_hits, const int *s_hits, int nhit,
		      int q_len, int s_len)
{
	SEXP ans_queryHits, ans_subjectHits, ans;
	size_t n;

	PROTECT(ans_queryHits = NEW_INTEGER(nhit));
	PROTECT(ans_subjectHits = NEW_INTEGER(nhit));
	n = sizeof(int) * nhit;
	memcpy(INTEGER(ans_queryHits), q_hits, n);
	memcpy(INTEGER(ans_subjectHits), s_hits, n);
	ans = new_Hits0(ans_queryHits, ans_subjectHits, q_len, s_len);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * High-level user-friendly constructor
 */

/* Based on qsort(). Time is O(nhit*log(nhit)). */
static void qsort_hits(int *qh_in, const int *sh_in,
		       int *qh_out, int *sh_out, int nhit)
{
	int k;

	_get_order_of_int_array(qh_in, nhit, 0, sh_out, 0);
	for (k = 0; k < nhit; k++)
		qh_out[k] = qh_in[sh_out[k]];
	memcpy(qh_in, sh_out, sizeof(int) * nhit);
	for (k = 0; k < nhit; k++)
		sh_out[k] = sh_in[qh_in[k]];
	return;
}

/* Tabulated sorting. Time is O(nhit). WARNING: 'nhit' MUST be >= 'q_len'. */
static void tsort_hits(int *qh_in, const int *sh_in,
		       int *qh_out, int *sh_out, int nhit, int q_len)
{
	int i, k, offset, count, prev_offset, j;

	/* Compute nb of hits per query. We need a place for this so we
	   temporarily use 'qh_out' which is assumed to have at least 'q_len'
	   elements. */
	for (i = 0; i < q_len; i++)
		qh_out[i] = 0;
	for (k = 0; k < nhit; k++)
		qh_out[--qh_in[k]]++;  /* make 'qh_in[k]' 0-based */
	/* Replace counts with offsets. */
	offset = 0;
	for (i = 0; i < q_len; i++) {
		count = qh_out[i];
		qh_out[i] = offset;
		offset += count;
	}
	/* Fill 'sh_out'. */
	for (k = 0; k < nhit; k++) {
		offset = qh_out[qh_in[k]]++;
		sh_out[offset] = sh_in[k];
	}
	/* Fill 'qh_out'. */
	memcpy(qh_in, qh_out, sizeof(int) * nhit);
	k = offset = 0;
	for (i = 1; i <= q_len; i++) {
		prev_offset = offset;
		offset = qh_in[i - 1];
		for (j = prev_offset; j < offset; j++)
			qh_out[k++] = i;
	}
	return;
}

static void sort_hits(int *qh_in, const int *sh_in,
		      int *qh_out, int *sh_out, int nhit, int q_len)
{
	if (nhit >= q_len)
		tsort_hits(qh_in, sh_in, qh_out, sh_out, nhit, q_len);
	else
		qsort_hits(qh_in, sh_in, qh_out, sh_out, nhit);
	return;
}

SEXP _new_Hits(int *q_hits, const int *s_hits, int nhit,
	       int q_len, int s_len, int already_sorted)
{
	SEXP ans_queryHits, ans_subjectHits, ans;

	if (already_sorted || nhit <= 1 || q_len <= 1)
		return new_Hits1(q_hits, s_hits, nhit, q_len, s_len);
	PROTECT(ans_queryHits = NEW_INTEGER(nhit));
	PROTECT(ans_subjectHits = NEW_INTEGER(nhit));
	sort_hits(q_hits, s_hits,
		  INTEGER(ans_queryHits), INTEGER(ans_subjectHits),
		  nhit, q_len);
	ans = new_Hits0(ans_queryHits, ans_subjectHits, q_len, s_len);
	UNPROTECT(2);
	return ans;
}

static int get_q_len_or_s_len(SEXP len, const char *what)
{
	int len0;

	if (!IS_INTEGER(len) || LENGTH(len) != 1)
		error("'%s' must be a single integer", what);
	len0 = INTEGER(len)[0];
	if (len0 == NA_INTEGER || len0 < 0)
		error("'%s' must be a single non-negative integer", what);
	return len0;
}

/* Return 1 if 'q_hits' is already sorted and 0 otherwise. */
static int check_hits(const int *q_hits, const int *s_hits, int nhit,
		      int q_len, int s_len)
{
	int already_sorted, prev_i, k, i, j;

	already_sorted = 1;
	prev_i = -1;
	for (k = 0; k < nhit; k++, q_hits++, s_hits++) {
		i = *q_hits;
		if (i == NA_INTEGER || i < 1 || i > q_len)
			error("'queryHits' must contain non-NA values "
			      ">= 1 and <= 'queryLength'");
		if (i < prev_i)
			already_sorted = 0;
		prev_i = i;
		j = *s_hits;
		if (j == NA_INTEGER || j < 1 || j > s_len)
			error("'subjectHits' must contain non-NA values "
			      ">= 1 and <= 'subjectLength'");
	}
	return already_sorted;
}

/* --- .Call ENTRY POINT --- */
SEXP Hits_new(SEXP q_hits, SEXP s_hits, SEXP q_len, SEXP s_len)
{
	int nhit, q_len0, s_len0, already_sorted, *q_hits_p2;
	const int *q_hits_p, *s_hits_p;

	nhit = _check_integer_pairs(q_hits, s_hits,
				    &q_hits_p, &s_hits_p,
				    "queryHits", "subjectHits");
	q_len0 = get_q_len_or_s_len(q_len, "queryLength");
	s_len0 = get_q_len_or_s_len(s_len, "subjectLength");
	already_sorted = check_hits(q_hits_p, s_hits_p, nhit, q_len0, s_len0);
	if (already_sorted)
		return new_Hits1(q_hits_p, s_hits_p, nhit, q_len0, s_len0);
	q_hits_p2 = (int *) R_alloc(sizeof(int), nhit);
	memcpy(q_hits_p2, q_hits_p, sizeof(int) * nhit);
	return _new_Hits(q_hits_p2, s_hits_p, nhit,
			 q_len0, s_len0, already_sorted);
}


/****************************************************************************
 * select_hits()
 */

int _get_select_mode(SEXP select)
{
	const char *select0;

	if (!IS_CHARACTER(select) || LENGTH(select) != 1)
		error("'select' must be a single string");
	select = STRING_ELT(select, 0);
	if (select == NA_STRING)
		error("'select' cannot be NA");
	select0 = CHAR(select);
	if (strcmp(select0, "all") == 0)
		return ALL_HITS;
	if (strcmp(select0, "first") == 0)
		return FIRST_HIT;
	if (strcmp(select0, "last") == 0)
		return LAST_HIT;
	if (strcmp(select0, "arbitrary") == 0)
		return ARBITRARY_HIT;
	if (strcmp(select0, "count") == 0)
		return COUNT_HITS;
	error("'select' must be \"all\", \"first\", "
	      "\"last\", \"arbitrary\", or \"count\"");
	return 0;
}

/* --- .Call ENTRY POINT --- */
SEXP select_hits(SEXP q_hits, SEXP s_hits, SEXP q_len, SEXP select)
{
	int nhit, ans_len, select_mode, init_val, i, k, j1;
	const int *q_hits_p, *s_hits_p;
	SEXP ans;

	nhit = _check_integer_pairs(q_hits, s_hits,
				    &q_hits_p, &s_hits_p,
				    "queryHits(x)", "subjectHits(x)");
	ans_len = INTEGER(q_len)[0];
	select_mode = _get_select_mode(select);
	PROTECT(ans = NEW_INTEGER(ans_len));
	init_val = select_mode == COUNT_HITS ? 0 : NA_INTEGER;
	for (i = 0; i < ans_len; i++)
		INTEGER(ans)[i] = init_val;
	for (k = 0; k < nhit; k++, q_hits_p++, s_hits_p++) {
		i = *q_hits_p - 1;
		if (select_mode == COUNT_HITS) {
			INTEGER(ans)[i]++;
			continue;
		}
		j1 = *s_hits_p;
		if (INTEGER(ans)[i] == NA_INTEGER
		 || (select_mode == FIRST_HIT) == (j1 < INTEGER(ans)[i]))
			INTEGER(ans)[i] = j1;
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * make_all_group_inner_hits()
 *
 * --- .Call ENTRY POINT ---
 * 'hit_type' must be 0, -1 or 1 (single integer).
 */
SEXP make_all_group_inner_hits(SEXP group_sizes, SEXP hit_type)
{
	int ngroup, htype, ans_len, i, j, k, gs, nhit,
	    iofeig, *left, *right;
	const int *group_sizes_elt;
	SEXP ans_q_hits, ans_s_hits, ans;

	ngroup = LENGTH(group_sizes);
	htype = INTEGER(hit_type)[0];
	for (i = ans_len = 0, group_sizes_elt = INTEGER(group_sizes);
	     i < ngroup;
	     i++, group_sizes_elt++)
	{
		gs = *group_sizes_elt;
		if (gs == NA_INTEGER || gs < 0)
			error("'group_sizes' contains NAs or negative values");
		nhit = htype == 0 ? gs * gs : (gs * (gs - 1)) / 2;
		ans_len += nhit;
		
	}
	PROTECT(ans_q_hits = NEW_INTEGER(ans_len));
	PROTECT(ans_s_hits = NEW_INTEGER(ans_len));
	left = INTEGER(ans_q_hits);
	right = INTEGER(ans_s_hits);
	iofeig = 0; /* 0-based Index Of First Element In Group */
	for (i = 0, group_sizes_elt = INTEGER(group_sizes);
	     i < ngroup;
	     i++, group_sizes_elt++)
	{
		gs = *group_sizes_elt;
		if (htype > 0) {
			for (j = 1; j < gs; j++) {
				for (k = j + 1; k <= gs; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		} else if (htype < 0) {
			for (j = 2; j <= gs; j++) {
				for (k = 1; k < j; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		} else {
			for (j = 1; j <= gs; j++) {
				for (k = 1; k <= gs; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		}
		iofeig += gs;
	}
	ans = new_Hits0(ans_q_hits, ans_s_hits, iofeig, iofeig);
	UNPROTECT(2);
	return ans;
}

