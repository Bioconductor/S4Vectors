/****************************************************************************
 *                  Low-level manipulation of Hits objects                  *
 ****************************************************************************/
#include "S4Vectors.h"


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

/*
 * --- .Call ENTRY POINT ---
 */
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

/*
 * --- .Call ENTRY POINT ---
 * 'hit_type' must be 0, -1 or 1 (single integer).
 */
SEXP make_all_group_inner_hits(SEXP group_sizes, SEXP hit_type)
{
	int ngroup, htype, ans_len, i, j, k, gs, nhit,
	    iofeig, *left, *right;
	const int *group_sizes_elt;
	SEXP ans_q_hits, ans_s_hits,
	     ans_q_len, ans_s_len, ans;

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
	PROTECT(ans_q_len = ScalarInteger(iofeig));
	PROTECT(ans_s_len = ScalarInteger(iofeig));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Hits")));
	SET_SLOT(ans, install("queryHits"), ans_q_hits);
	SET_SLOT(ans, install("subjectHits"), ans_s_hits);
	SET_SLOT(ans, install("queryLength"), ans_q_len);
	SET_SLOT(ans, install("subjectLength"), ans_s_len);
	UNPROTECT(5);
	return ans;
}

