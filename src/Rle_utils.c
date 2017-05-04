#include "S4Vectors.h"
#include <R_ext/Utils.h>

/*
 * roundingScale() function taken from the src/lib/common.c file from the Kent
 * source tree:
 *   http://genome-source.cse.ucsc.edu/gitweb/?p=kent.git;a=blob_plain;f=src/lib/common.c
 */
static int roundingScale(int a, int p, int q)
/* returns rounded a*p/q */
{
if (a > 100000 || p > 100000)
    {
    double x = a;
    x *= p;
    x /= q;
    return round(x);
    }
else
    return (a*p + q/2)/q;
}

static R_xlen_t compute_nrun_out(int nrun_in,
	const void *lengths_in, int lengths_in_is_L, int k)
{
	R_xlen_t nrun_out, i;
	long long int len_in;

	nrun_out = 0;
	for (i = 0; i < nrun_in; i++) {
		len_in = GET_INT_OR_LLINT(lengths_in, lengths_in_is_L, i);
		nrun_out += k < len_in ? k : len_in;
	}
	if (nrun_out < k)
		error("S4Vectors internal error in compute_nrun_out(): "
		      "k > length of Rle vector");
	nrun_out -= k - 1;
	return nrun_out;
}

static void compute_runsum_integer_runs(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	int k, int narm,
	R_xlen_t nrun_out, int *values_out, void *lengths_out)
{
	R_xlen_t i, j, i2;
	long long int len_in, offset_in_run, k2, times;
	int val_in, val_out, val2_in;

	j = 0;
	for (i = 0; i < nrun_in; i++) {
		len_in = GET_INT_OR_LLINT(lengths_in, lengths_in_is_L, i);
		val_in = values_in[i];
		if (narm && val_in == NA_INTEGER)
			val_in = 0;
		if (k <= len_in) {
			values_out[j] = _safe_int_mult(k, val_in);
			offset_in_run = len_in - k + 1;
			SET_INT_OR_LLINT(lengths_out, lengths_in_is_L,
					 j, offset_in_run);
			if (++j == nrun_out)
				return;
			if (j % 500000 == 0)
				R_CheckUserInterrupt();
		} else {
			offset_in_run = 0;
		}
		while (offset_in_run < len_in) {
			k2 = len_in - offset_in_run;  /* < k */
			val_out = _safe_int_mult(k2, val_in);
			i2 = i;
			do {
				i2++;
				k2 += times = GET_INT_OR_LLINT(lengths_in,
							       lengths_in_is_L,
							       i2);
				if (k2 > k)
					times -= k2 - k;
				val2_in = values_in[i2];
				if (narm && val2_in == NA_INTEGER)
					val2_in = 0;
				val_out = _safe_int_add(val_out,
					      _safe_int_mult(times, val2_in));
			} while (k2 < k);
			values_out[j] = val_out;
			SET_INT_OR_LLINT(lengths_out, lengths_in_is_L, j, 1);
			if (++j == nrun_out)
				return;
			if (j % 500000 == 0)
				R_CheckUserInterrupt();
			offset_in_run++;
		}
	}
	return;
}

static void compute_runsum_numeric_runs(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L,
	int k, int narm,
	R_xlen_t nrun_out, double *values_out, void *lengths_out)
{
	R_xlen_t i, j, i2;
	long long int len_in, offset_in_run, k2, times;
	double val_in, val_out, val2_in;

	j = 0;
	for (i = 0; i < nrun_in; i++) {
		len_in = GET_INT_OR_LLINT(lengths_in, lengths_in_is_L, i);
		val_in = values_in[i];
		if (narm && ISNAN(val_in))
			val_in = 0.0;
		if (k <= len_in) {
			values_out[j] = k * val_in;
			offset_in_run = len_in - k + 1;
			SET_INT_OR_LLINT(lengths_out, lengths_in_is_L,
					 j, offset_in_run);
			if (++j == nrun_out)
				return;
			if (j % 500000 == 0)
				R_CheckUserInterrupt();
		} else {
			offset_in_run = 0;
		}
		while (offset_in_run < len_in) {
			k2 = len_in - offset_in_run;  /* < k */
			val_out = k2 * val_in;
			i2 = i;
			do {
				i2++;
				k2 += times = GET_INT_OR_LLINT(lengths_in,
							       lengths_in_is_L,
							       i2);
				if (k2 > k)
					times -= k2 - k;
				val2_in = values_in[i2];
				if (narm && ISNAN(val2_in))
					val2_in = 0.0;
				val_out += times * val2_in;
			} while (k2 < k);
			values_out[j] = val_out;
			SET_INT_OR_LLINT(lengths_out, lengths_in_is_L, j, 1);
			if (++j == nrun_out)
				return;
			if (j % 500000 == 0)
				R_CheckUserInterrupt();
			offset_in_run++;
		}
	}
	return;
}

/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_runsum(SEXP x, SEXP k, SEXP na_rm)
{
	int k0, narm, lengths_in_is_L;
	SEXP x_lengths, x_values;
	R_xlen_t nrun_in, nrun_out;
	const void *lengths_in;
	void *lengths_out, *values_out;

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
	    (k0 = INTEGER(k)[0]) == NA_INTEGER || k0 <= 0)
		error("'k' must be a positive integer");
	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 ||
	    (narm = LOGICAL(na_rm)[0]) == NA_LOGICAL)
		error("'na_rm' must be TRUE or FALSE");

	x_lengths = GET_SLOT(x, install("lengths"));

	if (IS_INTEGER(x_lengths)) {
		nrun_in = XLENGTH(x_lengths);
		lengths_in = INTEGER(x_lengths);
		lengths_in_is_L = 0;
	} else {
		nrun_in = _get_LLint_length(x_lengths);
		lengths_in = _get_LLint_dataptr(x_lengths);
		lengths_in_is_L = 1;
	}
	nrun_out = compute_nrun_out(nrun_in, lengths_in, lengths_in_is_L, k0);
	if (lengths_in_is_L) {
		lengths_out = (long long int *)
				R_alloc(nrun_out, sizeof(long long int));
	} else {
		lengths_out = (int *) R_alloc(nrun_out, sizeof(int));
	}

	x_values = GET_SLOT(x, install("values"));

	if (IS_INTEGER(x_values)) {
		values_out = (int *) R_alloc(nrun_out, sizeof(int));
		_reset_ovflow_flag();
		compute_runsum_integer_runs(nrun_in,
			INTEGER(x_values), lengths_in, lengths_in_is_L,
			k0, narm,
			nrun_out, values_out, lengths_out);
		if (_get_ovflow_flag())
			warning("NAs produced by integer overflow. "
			  "You can use:\n"
			  "      runValue(x) <- as.numeric(runValue(x))\n"
			  "      runsum(x, ...)\n"
			  "  to work around it.");
		return _construct_integer_Rle(nrun_out, values_out,
					      lengths_out, lengths_in_is_L);
	}

	if (IS_NUMERIC(x_values)) {
		values_out = (double *) R_alloc(nrun_out, sizeof(double));
		compute_runsum_numeric_runs(nrun_in,
			REAL(x_values), lengths_in, lengths_in_is_L,
			k0, narm,
			nrun_out, values_out, lengths_out);
		return _construct_numeric_Rle(nrun_out, values_out,
					      lengths_out, lengths_in_is_L);
	}

	error("runsum only supported for integer- and numeric-Rle vectors");
	return R_NilValue;
}

SEXP Rle_integer_runwtsum(SEXP x, SEXP k, SEXP wt, SEXP na_rm)
{
	int i, j, nrun, window_len, buf_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int stat_na;
	int *curr_value_na, *values_elt_na;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *values_elt, *curr_value;
	double *wt_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths;
	SEXP orig_values, na_index;
	const int narm = LOGICAL(na_rm)[0];
	
	if (!IS_INTEGER(k) || LENGTH(k) != 1 
	                   || INTEGER(k)[0] == NA_INTEGER 
	                   || INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	/* Set NA values to 0 
	 * Create NA index : 1 = NA; 0 = not NA
	 */
	orig_values = GET_SLOT(x, install("values"));
	values = PROTECT(Rf_allocVector(INTSXP, LENGTH(orig_values)));
	na_index = PROTECT(Rf_allocVector(INTSXP, LENGTH(orig_values)));
	int *vlu = INTEGER(orig_values);
	for(i = 0; i < LENGTH(orig_values); i++) {
		if (vlu[i] == NA_INTEGER) {
			INTEGER(na_index)[i] = 1;
			INTEGER(values)[i] = 0;
		} else {
			INTEGER(na_index)[i] = 0;
			INTEGER(values)[i] = INTEGER(orig_values)[i];
		}
	}

	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];
 
	if (!IS_NUMERIC(wt) || LENGTH(wt) != window_len)
		error("'wt' must be a numeric vector of length 'k'");

	ans_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = INTEGER(values);
		values_elt_na = INTEGER(na_index);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			/* calculate stat */
			stat = 0;
			stat_na = 0;
			curr_value = values_elt;
			curr_value_na = values_elt_na;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, wt_elt = REAL(wt); j < window_len; j++, wt_elt++) {
				stat += (*wt_elt) * (*curr_value);
				stat_na += *curr_value_na;
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_value_na++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			/* assign value */
			if (ans_len == 0) {
				ans_len = 1;
			} else {
				/* increment values and lengths based on stat */
				    if (narm | (stat_na == 0)) {
					if (stat != *buf_values_elt) {
						ans_len++;
						buf_values_elt++;
						buf_lengths_elt++;
					}
				} else {
					if ((stat_na != 0) && 
					    (*buf_values_elt != NA_REAL)) {
						ans_len++;
						buf_values_elt++;
						buf_lengths_elt++;
					}
				}
			}
			/* NA handling */
			if (!narm && (stat_na != 0))
				*buf_values_elt = NA_REAL;
			else
				*buf_values_elt = stat;

			/* determine length */
			if (window_len < start_offset) {
				*buf_lengths_elt += *lengths_elt - window_len + 1;
				start_offset = window_len - 1;
			} else {
				*buf_lengths_elt += 1;
				start_offset--;
			}

			/* move pointers if end of run */
			if (start_offset == 0) {
				values_elt++;
				values_elt_na++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}
	UNPROTECT(2);
	return _construct_numeric_Rle(ans_len, buf_values, buf_lengths, 0);
}

SEXP Rle_real_runwtsum(SEXP x, SEXP k, SEXP wt, SEXP na_rm)
{
	int i, j, nrun, window_len, buf_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *values_elt, *curr_value;
	double *wt_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths;
	SEXP orig_values;
	const int narm = LOGICAL(na_rm)[0];

	if (!IS_INTEGER(k) || LENGTH(k) != 1 
	                   || INTEGER(k)[0] == NA_INTEGER 
	                   || INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	window_len = INTEGER(k)[0];
	if (!IS_NUMERIC(wt) || LENGTH(wt) != window_len)
		error("'wt' must be a numeric vector of length 'k'");

	if (narm) {
		/* set NA and NaN values to 0 */
		orig_values = GET_SLOT(x, install("values"));
		values = PROTECT(Rf_allocVector(REALSXP, LENGTH(orig_values)));
		double *vlu = REAL(orig_values);
		for(i = 0; i < LENGTH(orig_values); i++) {
			if (ISNAN(vlu[i]))
				REAL(values)[i] = 0;
			else
				REAL(values)[i] = REAL(orig_values)[i];
		}
	} else {
		values = GET_SLOT(x, install("values"));
	}
	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);
	ans_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = REAL(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			/* calculate stat */
			stat = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, wt_elt = REAL(wt); j < window_len;
			     j++, wt_elt++) {
				stat += (*wt_elt) * (*curr_value);
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			/* assign value */
			if (ans_len == 0) {
				ans_len = 1;
			} else if (!R_FINITE(stat) && !R_FINITE(*buf_values_elt)) {
				if ((R_IsNA(stat) && !R_IsNA(*buf_values_elt)) 
                                    || (!R_IsNA(stat) && R_IsNA(*buf_values_elt)) 
				    || (R_IsNaN(stat) && !R_IsNaN(*buf_values_elt)) 
				    || (!R_IsNaN(stat) && R_IsNaN(*buf_values_elt)) 
				    || ((stat == R_PosInf) && 
						(*buf_values_elt != R_PosInf)) 
				    || ((stat != R_PosInf) && 
						(*buf_values_elt == R_PosInf)) 
				    || ((stat == R_NegInf) && 
						(*buf_values_elt != R_NegInf)) 
				    || ((stat != R_NegInf) && 
						(*buf_values_elt == R_NegInf))) {
					ans_len++;
					buf_values_elt++;
					buf_lengths_elt++;
				}
			} else {
				if (stat != *buf_values_elt) {
					ans_len++;
					buf_values_elt++;
					buf_lengths_elt++;
				}
			}
			*buf_values_elt = stat;

			/* determine length */
			if (window_len < start_offset) {
				*buf_lengths_elt += *lengths_elt - window_len + 1;
				start_offset = window_len - 1;
			} else {
				*buf_lengths_elt += 1;
				    start_offset--;
			}
			/* move pointers if end of run */
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}

	if (narm)
		UNPROTECT(1);
	return _construct_numeric_Rle(ans_len, buf_values, buf_lengths, 0);
}

/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_runwtsum(SEXP x, SEXP k, SEXP wt, SEXP na_rm)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
	case INTSXP:
		PROTECT(ans =  Rle_integer_runwtsum(x, k, wt, na_rm));
		break;
	case REALSXP:
		PROTECT(ans =  Rle_real_runwtsum(x, k, wt, na_rm));
		break;
	default:
		error("runwtsum only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}

SEXP Rle_integer_runq(SEXP x, SEXP k, SEXP which, SEXP na_rm)
{
	int i, j, nrun, window_len, buf_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	int stat, count_na, window_len_na;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths;
	const int narm = LOGICAL(na_rm)[0];
	const int constw = INTEGER(which)[0];
	const int constk = INTEGER(k)[0];

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(k)[0])
		error("'i' must be an integer in [0, k]");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

	ans_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		window = (int *) R_alloc(window_len, sizeof(int));
		buf_values = (int *) R_alloc((long) buf_len, sizeof(int));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = INTEGER(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			/* create window */
			count_na = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			window_len_na = INTEGER(k)[0];
			q_index = INTEGER(which)[0] - 1;
			for(j = 0; j < window_len; j++) {
				if (*curr_value == NA_INTEGER)
					count_na += 1;
				window[j] = *curr_value;
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			/* calculate stat */
			if (!narm && count_na > 0) {
				stat = NA_INTEGER;
			} else {
				/* NA handling */
				if (count_na != 0) {
					window_len_na = window_len - count_na;
					q_index = roundingScale(window_len_na,
							constw, constk);
					if (q_index > 0)
						q_index = q_index - 1;
				}
				/* If window shrank to 0, return NA. */
				if (window_len_na == 0) {
					stat = NA_INTEGER;
				} else {
					/* NA's sorted last in iPsort */
					iPsort(window, window_len, q_index);
					stat = window[q_index];
				}
			}
			if (ans_len == 0) {
				ans_len = 1;
			} else if (stat != *buf_values_elt) {
				ans_len++;
				buf_values_elt++;
				buf_lengths_elt++;
			}
			*buf_values_elt = stat;

			/* determine length */
			if (window_len < start_offset) {
				*buf_lengths_elt += *lengths_elt - window_len + 1;
				start_offset = window_len - 1;
			} else {
				*buf_lengths_elt += 1;
			        start_offset--;
			}
			/* move pointers if end of run */
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}
	return _construct_integer_Rle(ans_len, buf_values, buf_lengths, 0);
}

SEXP Rle_real_runq(SEXP x, SEXP k, SEXP which, SEXP na_rm)
{
	int i, j, nrun, window_len, buf_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	double stat;
	int count_na, window_len_na;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths;
	const int narm = LOGICAL(na_rm)[0];
	const int constw = INTEGER(which)[0];
	const int constk = INTEGER(k)[0];

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(k)[0])
		error("'which' must be an integer in [0, k]");


	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

	ans_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		window = (double *) R_alloc(window_len, sizeof(double));
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = REAL(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			/* create window */
			count_na = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			window_len_na = INTEGER(k)[0];
			q_index = INTEGER(which)[0] - 1;
			for(j = 0; j < window_len; j++) {
				if (ISNAN(*curr_value))
					count_na += 1;	
				window[j] = *curr_value;
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			/* calculate stat */
			if (!narm && count_na > 0) {
				stat = NA_REAL;
			} else {
				/* NA handling */
				if (count_na != 0)
					window_len_na = window_len - count_na;
					q_index = roundingScale(window_len_na,
							constw, constk);
					if (q_index >0)
						q_index = q_index - 1;
				/* If window shrank to 0, return NA. */
				if (window_len_na == 0) {
					stat = NA_REAL;
				} else {
					/* NA's sorted last in rPsort */
					rPsort(window, window_len, q_index);
					stat = window[q_index];
				}
			}
			if (ans_len == 0) {
				ans_len = 1;
			} else if (stat != *buf_values_elt) {
				ans_len++;
				buf_values_elt++;
				buf_lengths_elt++;
			}
			*buf_values_elt = stat;
			/* determine length */
			if (window_len < start_offset) {
				*buf_lengths_elt += *lengths_elt - window_len + 1;
				start_offset = window_len - 1;
			} else {
				*buf_lengths_elt += 1;
				start_offset--;
			}
			/* move pointers if end of run */
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}
	return _construct_numeric_Rle(ans_len, buf_values, buf_lengths, 0);
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_runq(SEXP x, SEXP k, SEXP which, SEXP na_rm)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
	case INTSXP:
		PROTECT(ans = Rle_integer_runq(x, k, which, na_rm));
		break;
	case REALSXP:
		PROTECT(ans = Rle_real_runq(x, k, which, na_rm));
		break;
	default:
		error("runq only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}
