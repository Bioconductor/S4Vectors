#include "S4Vectors.h"

static SEXP _new_Rle(SEXP values, SEXP lengths)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS("Rle"));
	PROTECT(ans = NEW_OBJECT(classdef));
	SET_SLOT(ans, install("values"), values);
	SET_SLOT(ans, install("lengths"), lengths);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * Rle_length()
 */

static long long int sum_int_lengths(const int *lengths,
	R_xlen_t nrun)
{
	long long int sum;
	R_xlen_t i;

	sum = 0;
	for (i = 0; i < nrun; i++, lengths++)
		sum += *lengths;
	return sum;
}

static long long int sum_llint_lengths(const long long int *lengths,
	R_xlen_t nrun)
{
	long long int sum;
	R_xlen_t i;

	sum = 0;
	for (i = 0; i < nrun; i++, lengths++)
		sum += *lengths;
	return sum;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_length(SEXP x)
{
	SEXP x_lengths, ans;
	R_xlen_t x_nrun;
	void *x_lengths_dataptr;
	long long int sum;

	x_lengths = GET_SLOT(x, install("lengths"));
	if (IS_INTEGER(x_lengths)) {
		x_nrun = XLENGTH(x_lengths);
		x_lengths_dataptr = INTEGER(x_lengths);
		sum = sum_int_lengths(x_lengths_dataptr, x_nrun);
	} else if (_is_LLint(x_lengths)) {
		x_nrun = _get_LLint_length(x_lengths);
		x_lengths_dataptr = _get_LLint_dataptr(x_lengths);
		sum = sum_llint_lengths(x_lengths_dataptr, x_nrun);
	} else {
		error("S4Vectors internal error in Rle_length(): "
		      "'runLengths(x)' is not an integer\n"
		      "  or LLint vector");
	}
	if (sum < 0)
		error("S4Vectors internal error in Rle_length(): "
		      "Rle vector has a negative length");
	if (sum > R_XLEN_T_MAX)
		error("S4Vectors internal error in Rle_length(): "
		      "Rle vector is too long");
	PROTECT(ans = _alloc_LLint("LLint", 1));
	_get_LLint_dataptr(ans)[0] = sum;
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Rle_valid()
 */

static char validity_msg[200];

static int check_int_lengths(const int *lengths, R_xlen_t nrun)
{
	R_xlen_t i;

	for (i = 0; i < nrun; i++, lengths++) {
		if (*lengths == NA_INTEGER) {
			snprintf(validity_msg, sizeof(validity_msg),
				 "some run lengths are NA");
			return 1;
		}
		if (*lengths <= 0) {
			snprintf(validity_msg, sizeof(validity_msg),
				 "some run lengths are non-positive");
			return 1;
		}
	}
	return 0;
}

static int check_llint_lengths(const long long int *lengths, R_xlen_t nrun)
{
	int no_big_lengths;
	R_xlen_t i;

	no_big_lengths = 1;
	for (i = 0; i < nrun; i++, lengths++) {
		if (*lengths == NA_LLINT) {
			snprintf(validity_msg, sizeof(validity_msg),
				 "some run lengths are NA");
			return 1;
		}
		if (*lengths <= 0) {
			snprintf(validity_msg, sizeof(validity_msg),
				 "some run lengths are non-positive");
			return 1;
		}
		if (*lengths > INT_MAX)
			no_big_lengths = 0;
	}
	if (no_big_lengths) {
		snprintf(validity_msg, sizeof(validity_msg),
			 "the run lengths are stored in an LLint vector\n"
			 "  when they could be in an integer vector");
		return 1;
	}
	return 0;
}

static int valid_run_lengths(SEXP lengths)
{
	R_xlen_t nrun;
	void *lengths_dataptr;

	if (IS_INTEGER(lengths)) {
		nrun = XLENGTH(lengths);
		lengths_dataptr = INTEGER(lengths);
		return check_int_lengths(lengths_dataptr, nrun);
	}
	if (_is_LLint(lengths)) {
		nrun = _get_LLint_length(lengths);
		lengths_dataptr = _get_LLint_dataptr(lengths);
		return check_llint_lengths(lengths_dataptr, nrun);
	}
	snprintf(validity_msg, sizeof(validity_msg),
		 "'runLengths(x)' must be an integer or LLint vector");
	return 1;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_valid(SEXP x)
{
	SEXP x_lengths;

	/* Check 'lengths' slot. */
	x_lengths = GET_SLOT(x, install("lengths"));
	if (valid_run_lengths(x_lengths))
		return mkString(validity_msg);
	return R_NilValue;
}


/****************************************************************************
 * Low-level helpers used by "The C level Rle smart constructors".
 */

#define	CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L)			   \
{									   \
	if (lengths_in_is_L) {						   \
		if ((len_in) == NA_LLINT)				   \
			error("some run lengths are NA");		   \
		if ((len_in) > R_XLEN_T_MAX)				   \
			error("Rle vector is too long");		   \
	} else {							   \
		if ((len_in) == NA_INTEGER)				   \
			error("some run lengths are NA");		   \
	}								   \
	if ((len_in) == 0)						   \
		continue;						   \
	if ((len_in) < 0)						   \
		error("some run lengths are negative");			   \
}

static R_xlen_t check_integer_runs(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	unsigned long long int *max_len_out_p)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in;
	unsigned long long int sum, len_out;
	int val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	*max_len_out_p = 0;
	sum = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L);
		}
		val_in = *values_in;
		if (not_empty) {
			if (val_in == val_out) {
				sum += len_in;
				if (sum > R_XLEN_T_MAX)
					error("Rle vector is too long");
				len_out += len_in;
				continue;
			}
			/* End of run */
			if (len_out > *max_len_out_p)
				*max_len_out_p = len_out;
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		sum += len_out = len_in;
		if (sum > R_XLEN_T_MAX)
			error("Rle vector is too long");
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		if (len_out > *max_len_out_p)
			*max_len_out_p = len_out;
		nrun_out++;
	}
	return nrun_out;
}

static void fill_integer_runs(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	int *values_out, void *lengths_out, int lengths_out_is_L)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in, len_out;
	int val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			if (len_in == 0)
				continue;
		}
		val_in = *values_in;
		if (not_empty) {
			if (val_in == val_out) {
				len_out += len_in;
				continue;
			}
			/* End of run */
			SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
					 nrun_out, len_out);
			values_out[nrun_out] = val_out;	
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		len_out = len_in;
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
				 nrun_out, len_out);
		values_out[nrun_out] = val_out;	
	}
	return;
}

#define	SAME_DOUBLE_VALS(x, y) \
	((x) == (y) || (R_IsNA(x) && R_IsNA(y)) || (R_IsNaN(x) && R_IsNaN(y)))

static R_xlen_t check_numeric_runs(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L,
	unsigned long long int *max_len_out_p)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in;
	unsigned long long int sum, len_out;
	double val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	*max_len_out_p = 0;
	sum = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L);
		}
		val_in = *values_in;
		if (not_empty) {
			if (SAME_DOUBLE_VALS(val_in, val_out)) {
				sum += len_in;
				if (sum > R_XLEN_T_MAX)
					error("Rle vector is too long");
				len_out += len_in;
				continue;
			}
			/* End of run */
			if (len_out > *max_len_out_p)
				*max_len_out_p = len_out;
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		sum += len_out = len_in;
		if (sum > R_XLEN_T_MAX)
			error("Rle vector is too long");
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		if (len_out > *max_len_out_p)
			*max_len_out_p = len_out;
		nrun_out++;
	}
	return nrun_out;
}

static void fill_numeric_runs(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L,
	double *values_out, void *lengths_out, int lengths_out_is_L)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in, len_out;
	double val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			if (len_in == 0)
				continue;
		}
		val_in = *values_in;
		if (not_empty) {
			if (SAME_DOUBLE_VALS(val_in, val_out)) {
				len_out += len_in;
				continue;
			}
			/* End of run */
			SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
					 nrun_out, len_out);
			values_out[nrun_out] = val_out;	
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		len_out = len_in;
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
				 nrun_out, len_out);
		values_out[nrun_out] = val_out;	
	}
	return;
}

static R_xlen_t check_complex_runs(R_xlen_t nrun_in,
	const Rcomplex *values_in, const void *lengths_in, int lengths_in_is_L,
	unsigned long long int *max_len_out_p)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in;
	unsigned long long int sum, len_out;
	Rcomplex val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	*max_len_out_p = 0;
	sum = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L);
		}
		val_in = *values_in;
		if (not_empty) {
			if (SAME_DOUBLE_VALS(val_in.r, val_out.r) &&
			    SAME_DOUBLE_VALS(val_in.i, val_out.i))
			{
				sum += len_in;
				if (sum > R_XLEN_T_MAX)
					error("Rle vector is too long");
				len_out += len_in;
				continue;
			}
			/* End of run */
			if (len_out > *max_len_out_p)
				*max_len_out_p = len_out;
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		sum += len_out = len_in;
		if (sum > R_XLEN_T_MAX)
			error("Rle vector is too long");
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		if (len_out > *max_len_out_p)
			*max_len_out_p = len_out;
		nrun_out++;
	}
	return nrun_out;
}

static void fill_complex_runs(R_xlen_t nrun_in,
	const Rcomplex *values_in, const void *lengths_in, int lengths_in_is_L,
	Rcomplex *values_out, void *lengths_out, int lengths_out_is_L)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in, len_out;
	Rcomplex val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			if (len_in == 0)
				continue;
		}
		val_in = *values_in;
		if (not_empty) {
			if (SAME_DOUBLE_VALS(val_in.r, val_out.r) &&
			    SAME_DOUBLE_VALS(val_in.i, val_out.i))
			{
				len_out += len_in;
				continue;
			}
			/* End of run */
			SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
					 nrun_out, len_out);
			values_out[nrun_out] = val_out;	
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		len_out = len_in;
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
				 nrun_out, len_out);
		values_out[nrun_out] = val_out;	
	}
	return;
}

static R_xlen_t check_character_runs(
	SEXP values_in, const void *lengths_in, int lengths_in_is_L,
	unsigned long long int *max_len_out_p)
{
	R_xlen_t nrun_in, nrun_out, i;
	int not_empty;
	long long int len_in;
	unsigned long long int sum, len_out;
	SEXP val_in, val_out;

	nrun_in = XLENGTH(values_in);
	nrun_out = 0;
	not_empty = 0;
	*max_len_out_p = 0;
	sum = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L);
		}
		val_in = STRING_ELT(values_in, i);
		if (not_empty) {
			if (val_in == val_out) {
				sum += len_in;
				if (sum > R_XLEN_T_MAX)
					error("Rle vector is too long");
				len_out += len_in;
				continue;
			}
			/* End of run */
			if (len_out > *max_len_out_p)
				*max_len_out_p = len_out;
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		sum += len_out = len_in;
		if (sum > R_XLEN_T_MAX)
			error("Rle vector is too long");
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		if (len_out > *max_len_out_p)
			*max_len_out_p = len_out;
		nrun_out++;
	}
	return nrun_out;
}

static void fill_character_runs(
	SEXP values_in, const void *lengths_in, int lengths_in_is_L,
	SEXP values_out, void *lengths_out, int lengths_out_is_L)
{
	R_xlen_t nrun_in, nrun_out, i;
	int not_empty;
	long long int len_in, len_out;
	SEXP val_in, val_out;

	nrun_in = XLENGTH(values_in);
	nrun_out = 0;
	not_empty = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			if (len_in == 0)
				continue;
		}
		val_in = STRING_ELT(values_in, i);
		if (not_empty) {
			if (val_in == val_out) {
				len_out += len_in;
				continue;
			}
			/* End of run */
			SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
					 nrun_out, len_out);
			SET_STRING_ELT(values_out, nrun_out, val_out);
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		len_out = len_in;
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
				 nrun_out, len_out);
		SET_STRING_ELT(values_out, nrun_out, val_out);
	}
	return;
}

static R_xlen_t check_raw_runs(R_xlen_t nrun_in,
	const Rbyte *values_in, const void *lengths_in, int lengths_in_is_L,
	unsigned long long int *max_len_out_p)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in;
	unsigned long long int sum, len_out;
	Rbyte val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	*max_len_out_p = 0;
	sum = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			CHECK_RUN_LENGTH_IN(len_in, lengths_in_is_L);
		}
		val_in = *values_in;
		if (not_empty) {
			if (val_in == val_out) {
				sum += len_in;
				if (sum > R_XLEN_T_MAX)
					error("Rle vector is too long");
				len_out += len_in;
				continue;
			}
			/* End of run */
			if (len_out > *max_len_out_p)
				*max_len_out_p = len_out;
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		sum += len_out = len_in;
		if (sum > R_XLEN_T_MAX)
			error("Rle vector is too long");
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		if (len_out > *max_len_out_p)
			*max_len_out_p = len_out;
		nrun_out++;
	}
	return nrun_out;
}

static void fill_raw_runs(R_xlen_t nrun_in,
	const Rbyte *values_in, const void *lengths_in, int lengths_in_is_L,
	Rbyte *values_out, void *lengths_out, int lengths_out_is_L)
{
	R_xlen_t nrun_out, i;
	int not_empty;
	long long int len_in, len_out;
	Rbyte val_in, val_out;

	nrun_out = 0;
	not_empty = 0;
	for (i = 0, len_in = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			len_in = GET_INT_OR_LLINT(lengths_in,
						  lengths_in_is_L, i);
			if (len_in == 0)
				continue;
		}
		val_in = *values_in;
		if (not_empty) {
			if (val_in == val_out) {
				len_out += len_in;
				continue;
			}
			/* End of run */
			SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
					 nrun_out, len_out);
			values_out[nrun_out] = val_out;	
			nrun_out++;
		} else {
			not_empty = 1;
		}
		/* Beginning of run */
		len_out = len_in;
		val_out = val_in;
	}
	if (not_empty) {
		/* End of run */
		SET_INT_OR_LLINT(lengths_out, lengths_out_is_L,
				 nrun_out, len_out);
		values_out[nrun_out] = val_out;	
	}
	return;
}


/****************************************************************************
 * The C level Rle smart constructors.
 */

static SEXP alloc_lengths(R_xlen_t nrun_out, int lengths_out_is_L,
			  void **dataptr_p)
{
	SEXP lengths;

	/* No need to PROTECT() */
	if (lengths_out_is_L) {
		lengths = _alloc_LLint("LLint", nrun_out);
		*dataptr_p = _get_LLint_dataptr(lengths);
	} else {
		lengths = NEW_INTEGER(nrun_out);
		*dataptr_p = INTEGER(lengths);
	}
	return lengths;
}

SEXP _construct_logical_Rle(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	int *values_out;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_integer_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_LOGICAL(nrun_out));
	values_out = LOGICAL(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_integer_runs(nrun_in, values_in, lengths_in, lengths_in_is_L,
				   values_out, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_integer_Rle(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	int *values_out;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_integer_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_INTEGER(nrun_out));
	values_out = INTEGER(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_integer_runs(nrun_in, values_in, lengths_in, lengths_in_is_L,
				   values_out, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_numeric_Rle(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	double *values_out;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_numeric_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_NUMERIC(nrun_out));
	values_out = REAL(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_numeric_runs(nrun_in, values_in, lengths_in, lengths_in_is_L,
				   values_out, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_complex_Rle(R_xlen_t nrun_in,
	const Rcomplex *values_in, const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	Rcomplex *values_out;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_complex_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_COMPLEX(nrun_out));
	values_out = COMPLEX(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_complex_runs(nrun_in, values_in, lengths_in, lengths_in_is_L,
				   values_out, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_character_Rle(SEXP values_in,
	const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_character_runs(
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_CHARACTER(nrun_out));
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_character_runs(values_in, lengths_in, lengths_in_is_L,
			    ans_values, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_raw_Rle(R_xlen_t nrun_in,
	const Rbyte *values_in, const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_out;
	unsigned long long int max_len_out;
	void *lengths_out;
	int lengths_out_is_L;
	Rbyte *values_out;
	SEXP ans_lengths, ans_values, ans;

	nrun_out = check_raw_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			&max_len_out);
	lengths_out_is_L = max_len_out > INT_MAX;
	PROTECT(ans_values = NEW_RAW(nrun_out));
	values_out = RAW(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	fill_raw_runs(nrun_in, values_in, lengths_in, lengths_in_is_L,
			       values_out, lengths_out, lengths_out_is_L);
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_Rle(SEXP values_in,
		    const void *lengths_in, int lengths_in_is_L)
{
	R_xlen_t nrun_in;
	SEXP ans, ans_values, ans_values_class, ans_values_levels;

	nrun_in = XLENGTH(values_in);
	switch (TYPEOF(values_in)) {
	    case LGLSXP:
		PROTECT(ans = _construct_logical_Rle(nrun_in,
					LOGICAL(values_in), lengths_in,
					lengths_in_is_L));
		break;
	    case INTSXP:
		PROTECT(ans = _construct_integer_Rle(nrun_in,
					INTEGER(values_in), lengths_in,
					lengths_in_is_L));
		/* 'values_in' could be a factor in which case we need to
		   propagate its levels.  */
		if (isFactor(values_in)) {
			ans_values = GET_SLOT(ans, install("values"));
			/* Levels must be set before class. */
			PROTECT(ans_values_levels =
					duplicate(GET_LEVELS(values_in)));
			SET_LEVELS(ans_values, ans_values_levels);
			UNPROTECT(1);
			PROTECT(ans_values_class =
					duplicate(GET_CLASS(values_in)));
			SET_CLASS(ans_values, ans_values_class);
			UNPROTECT(1);
		}
		break;
	    case REALSXP:
		PROTECT(ans = _construct_numeric_Rle(nrun_in,
					REAL(values_in), lengths_in,
					lengths_in_is_L));
		break;
	    case CPLXSXP:
		PROTECT(ans = _construct_complex_Rle(nrun_in,
					COMPLEX(values_in), lengths_in,
					lengths_in_is_L));
		break;
	    case STRSXP:
		PROTECT(ans = _construct_character_Rle(
					values_in, lengths_in,
					lengths_in_is_L));
		break;
	    case RAWSXP:
		PROTECT(ans = _construct_raw_Rle(nrun_in,
					RAW(values_in), lengths_in,
					lengths_in_is_L));
		break;
	    default:
		error("Rle of type '%s' is not supported",
		      CHAR(type2str(TYPEOF(values_in))));
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * The Rle constructor.
 *
 * --- .Call ENTRY POINT ---
 * Args:
 *   lengths_in: An integer or LLint vector of the same length as 'values'
 *               with no NAs or negative values, or a NULL. If NULL then
 *               all the runs are considered to be of length 1 like if
 *               lengths_in was 'rep(1, length(values))'.
 */

SEXP Rle_constructor(SEXP values_in, SEXP lengths_in)
{
	R_xlen_t nrun_in, lengths_in_len;
	/* If lengths_in_is_L == 1 then 'lengths_in_dataptr' points to an
	   array of long long ints. Otherwise it points to an array of ints. */
	int lengths_in_is_L;
	const void *lengths_in_dataptr;

	nrun_in = XLENGTH(values_in);
	lengths_in_is_L = 0;
	if (isNull(lengths_in)) {
		lengths_in_dataptr = NULL;
	} else {
		if (IS_INTEGER(lengths_in)) {
			lengths_in_len = XLENGTH(lengths_in);
			lengths_in_dataptr = INTEGER(lengths_in);
		} else if (_is_LLint(lengths_in)) {
			lengths_in_is_L = 1;
			lengths_in_len = _get_LLint_length(lengths_in);
			lengths_in_dataptr = _get_LLint_dataptr(lengths_in);
		} else {
			error("the supplied 'lengths' must be an integer or "
			      "LLint vector, or a NULL");
		}
		if (nrun_in != lengths_in_len)
			error("'length(values)' != 'length(lengths)'");
	}
	return _construct_Rle(values_in, lengths_in_dataptr, lengths_in_is_L);
}


/****************************************************************************
 * The Rle start() and end() getters (.Call ENTRY POINTS).
 */

SEXP Rle_start(SEXP x)
{
	int i, nrun, *len_elt, *prev_start, *curr_start;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(nrun));

	if (nrun > 0) {
		INTEGER(ans)[0] = 1;
		for(i = 1, len_elt = INTEGER(lengths),
			prev_start = INTEGER(ans), curr_start = INTEGER(ans) + 1;
		    i < nrun; i++, len_elt++, prev_start++, curr_start++) {
			*curr_start = *prev_start + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}

SEXP Rle_end(SEXP x)
{
	int i, nrun, *len_elt, *prev_end, *curr_end;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(nrun));

	if (nrun > 0) {
		INTEGER(ans)[0] = INTEGER(lengths)[0];
		for(i = 1, len_elt = INTEGER(lengths) + 1,
			prev_end = INTEGER(ans), curr_end = INTEGER(ans) + 1;
		    i < nrun; i++, len_elt++, prev_end++, curr_end++) {
			*curr_end = *prev_end + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}


/****************************************************************************
 * Rle_extract_range(), Rle_extract_ranges(), and Rle_extract_positions()
 */

static SEXP extract_Rle_mapped_range(SEXP x_values, const int *x_lengths,
		int mapped_range_start,
		int mapped_range_span,
		int mapped_range_Ltrim,
		int mapped_range_Rtrim)
{
	SEXP ans_values, ans_lengths, ans;

	PROTECT(ans_values = _subset_vector_OR_factor_by_ranges(x_values,
						&mapped_range_start,
						&mapped_range_span, 1));
	PROTECT(ans_lengths = NEW_INTEGER(mapped_range_span));
	if (mapped_range_span != 0) {
		memcpy(INTEGER(ans_lengths),
		       x_lengths + mapped_range_start - 1,
		       sizeof(int) * mapped_range_span);
		INTEGER(ans_lengths)[0] -= mapped_range_Ltrim;
		INTEGER(ans_lengths)[mapped_range_span - 1] -=
						mapped_range_Rtrim;
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

/*
 * Extract 'nranges' Rle's from 'x'. The i-th Rle to extract corresponds to
 * the i-th "mapped range", which is defined by 4 int values:
 *   1. mapped_range_start[i]: The first run in 'x' spanned by the mapped
 *                             range (specified as 1-based index).
 *   2. mapped_range_span[i]:  The nb of runs in 'x' spanned by the mapped
 *                             range.
 *   3. mapped_range_Ltrim[i]: The nb of unspanned positions in the first
 *                             spanned run.
 *   4. mapped_range_Rtrim[i]: The nb of unspanned positions in the last
 *                             spanned run.
 * If 'as_list' is TRUE, then the extracted Rle's are returned in a list of
 * length 'nranges'. Otherwise, the single Rle obtained by concatenating them
 * all together is returned.
 */
static SEXP subset_Rle_by_mapped_ranges(SEXP x,
		const int *mapped_range_start,
		const int *mapped_range_span,
		const int *mapped_range_Ltrim,
		const int *mapped_range_Rtrim,
		int nranges, int as_list)
{
	SEXP x_values, x_lengths, tmp_values, ans, ans_elt;
	int tmp_nrun, *tmp_lengths, i, n;

	x_values = GET_SLOT(x, install("values"));
	x_lengths = GET_SLOT(x, install("lengths"));
	if (as_list == 1) {
		PROTECT(ans = NEW_LIST(nranges));
		for (i = 0; i < nranges; i++) {
			PROTECT(ans_elt = extract_Rle_mapped_range(x_values,
						INTEGER(x_lengths),
						mapped_range_start[i],
						mapped_range_span[i],
						mapped_range_Ltrim[i],
						mapped_range_Rtrim[i]));
			SET_VECTOR_ELT(ans, i, ans_elt);
			UNPROTECT(1);
		}
		UNPROTECT(1);
		return ans;
	}
	if (nranges == 1)
		return extract_Rle_mapped_range(x_values, INTEGER(x_lengths),
				mapped_range_start[0],
				mapped_range_span[0],
				mapped_range_Ltrim[0],
				mapped_range_Rtrim[0]);
	PROTECT(tmp_values = _subset_vector_OR_factor_by_ranges(x_values,
					mapped_range_start,
					mapped_range_span,
					nranges));
	tmp_nrun = LENGTH(tmp_values);
	tmp_lengths = (int *) R_alloc(sizeof(int), tmp_nrun);
	for (i = tmp_nrun = 0; i < nranges; i++) {
		n = mapped_range_span[i];
		if (n == 0)
			continue;
		memcpy(tmp_lengths + tmp_nrun,
		       INTEGER(x_lengths) + mapped_range_start[i] - 1,
		       sizeof(int) * n);
		tmp_lengths[tmp_nrun] -= mapped_range_Ltrim[i];
		tmp_nrun += n;
		tmp_lengths[tmp_nrun - 1] -= mapped_range_Rtrim[i];
	}
	PROTECT(ans = _construct_Rle(tmp_values, tmp_lengths, 0));
	UNPROTECT(2);
	return ans;
}

static SEXP subset_Rle_by_mapped_pos(SEXP x, const int *mapped_pos, int npos)
{
	SEXP x_values, tmp_values, ans;

	x_values = GET_SLOT(x, install("values"));
	PROTECT(tmp_values = _subset_vector_OR_factor_by_positions(x_values,
					mapped_pos, npos));
	PROTECT(ans = _construct_Rle(tmp_values, NULL, 0));
	UNPROTECT(2);
	return ans;
}

SEXP _subset_Rle_by_ranges(SEXP x,
		const int *start, const int *width, int nranges,
		int method, int as_list)
{
	SEXP x_lengths;
	int x_nrun,
	    *mapped_range_start, *mapped_range_span,
	    *mapped_range_Ltrim, *mapped_range_Rtrim, i;
	const char *errmsg;

	x_lengths = GET_SLOT(x, install("lengths"));
	x_nrun = LENGTH(x_lengths);
	mapped_range_start = (int *) R_alloc(sizeof(int), nranges);
	mapped_range_span = (int *) R_alloc(sizeof(int), nranges);
	mapped_range_Ltrim = (int *) R_alloc(sizeof(int), nranges);
	mapped_range_Rtrim = (int *) R_alloc(sizeof(int), nranges);
	errmsg = _ranges_mapper(INTEGER(x_lengths), x_nrun,
			start, width, nranges,
			mapped_range_start,  /* will be filled with offsets */
			mapped_range_span,
			mapped_range_Ltrim,
			mapped_range_Rtrim,
			method);
	if (errmsg != NULL)
		error(errmsg);
	for (i = 0; i < nranges; i++)
		mapped_range_start[i]++;  /* add 1 to get the starts */
	return subset_Rle_by_mapped_ranges(x,
			mapped_range_start,
			mapped_range_span,
			mapped_range_Ltrim,
			mapped_range_Rtrim,
			nranges, as_list);
}

SEXP _subset_Rle_by_positions(SEXP x, const int *pos, int npos, int method)
{
	SEXP x_lengths;
	int x_nrun, *mapped_pos;
	const char *errmsg;

	x_lengths = GET_SLOT(x, install("lengths"));
	x_nrun = LENGTH(x_lengths);
	mapped_pos = (int *) R_alloc(sizeof(int), npos);
	errmsg = _positions_mapper(INTEGER(x_lengths), x_nrun,
			pos, npos,
			mapped_pos,
			method);
	if (errmsg != NULL)
		error(errmsg);
	return subset_Rle_by_mapped_pos(x, mapped_pos, npos);
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_extract_range(SEXP x, SEXP start, SEXP end)
{
	int nranges, x_nrun,
	    mapped_range_offset, mapped_range_span,
	    mapped_range_Ltrim, mapped_range_Rtrim;
	const int *range_start_p, *range_end_p;
	SEXP x_values, x_lengths;
	const char *errmsg;

	nranges = _check_integer_pairs(start, end,
				       &range_start_p, &range_end_p,
				       "start", "end");
	if (nranges != 1)
		error("'start' and 'end' must be of length 1");
	x_values = GET_SLOT(x, install("values"));
	x_lengths = GET_SLOT(x, install("lengths"));
	x_nrun = LENGTH(x_lengths);
	errmsg = _simple_range_mapper(INTEGER(x_lengths), x_nrun,
				range_start_p[0], range_end_p[0],
				&mapped_range_offset,
				&mapped_range_span,
				&mapped_range_Ltrim,
				&mapped_range_Rtrim);
	if (errmsg != NULL)
		error(errmsg);
	mapped_range_offset++;  /* add 1 to get the start */
	return extract_Rle_mapped_range(x_values, INTEGER(x_lengths),
				mapped_range_offset,
				mapped_range_span,
				mapped_range_Ltrim,
				mapped_range_Rtrim);
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_extract_ranges(SEXP x, SEXP start, SEXP width,
			SEXP method, SEXP as_list)
{
	int nranges;
	const int *start_p, *width_p;

	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	return _subset_Rle_by_ranges(x, start_p, width_p, nranges,
				     INTEGER(method)[0], LOGICAL(as_list)[0]);
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_extract_positions(SEXP x, SEXP pos, SEXP method)
{
	int npos;

	npos = LENGTH(pos);
	return _subset_Rle_by_positions(x, INTEGER(pos), npos,
					INTEGER(method)[0]);
}


/****************************************************************************
 * Rle_getStartEndRunAndOffset()
 */

static SEXP get_StartEndRunAndOffset_from_runLength(
		const int *runlength, int runlength_len,
		const int *start, const int *end, int length)
{
	int i, *soff_elt, *eoff_elt;
	const int *start_elt, *end_elt, *erun_elt;
	SEXP info_start, info_end, ans, ans_names;
	SEXP ans_start, ans_start_names, ans_end, ans_end_names;
	SEXP start_run, start_offset, end_run, end_offset;

	PROTECT(info_start = _find_interv_and_start_from_width(start, length,
					runlength, runlength_len));
	PROTECT(info_end = _find_interv_and_start_from_width(end, length,
					runlength, runlength_len));

	start_run = VECTOR_ELT(info_start, 0);
	start_offset = VECTOR_ELT(info_start, 1);
	end_run = VECTOR_ELT(info_end, 0);
	end_offset = VECTOR_ELT(info_end, 1);
	for (i = 0, start_elt = start, end_elt = end,
		    soff_elt = INTEGER(start_offset),
		    eoff_elt = INTEGER(end_offset),
		    erun_elt = INTEGER(end_run);
	     i < length;
	     i++, start_elt++, end_elt++, soff_elt++, eoff_elt++, erun_elt++)
	{
		*soff_elt = *start_elt - *soff_elt;
		*eoff_elt = *eoff_elt + runlength[*erun_elt - 1] - 1 - *end_elt;
	}

	PROTECT(ans_start = NEW_LIST(2));
	PROTECT(ans_start_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans_start, 0, start_run);
	SET_VECTOR_ELT(ans_start, 1, start_offset);
	SET_STRING_ELT(ans_start_names, 0, mkChar("run"));
	SET_STRING_ELT(ans_start_names, 1, mkChar("offset"));
	SET_NAMES(ans_start, ans_start_names);

	PROTECT(ans_end = NEW_LIST(2));
	PROTECT(ans_end_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans_end, 0, end_run);
	SET_VECTOR_ELT(ans_end, 1, end_offset);
	SET_STRING_ELT(ans_end_names, 0, mkChar("run"));
	SET_STRING_ELT(ans_end_names, 1, mkChar("offset"));
	SET_NAMES(ans_end, ans_end_names);

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans, 0, ans_start);
	SET_VECTOR_ELT(ans, 1, ans_end);
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("end"));
	SET_NAMES(ans, ans_names);

	UNPROTECT(8);

	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_getStartEndRunAndOffset(SEXP x, SEXP start, SEXP end)
{
	int n;
	SEXP lengths;

	n = LENGTH(start);
	if (LENGTH(end) != n)
		error("length of 'start' must equal length of 'end'");
	lengths = GET_SLOT(x, install("lengths"));
	return get_StartEndRunAndOffset_from_runLength(
			INTEGER(lengths), LENGTH(lengths),
			INTEGER(start), INTEGER(end), n);
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_window_aslist(SEXP x, SEXP runStart, SEXP runEnd,
		               SEXP offsetStart, SEXP offsetEnd)
{
	SEXP values, lengths, runWidth, ans, ans_names, ans_values, ans_lengths;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	if (!IS_INTEGER(runStart) || LENGTH(runStart) != 1 ||
		INTEGER(runStart)[0] == NA_INTEGER || INTEGER(runStart)[0] < 1)
		error("invalid 'runStart' argument");

	if (!IS_INTEGER(runEnd) || LENGTH(runEnd) != 1 ||
		INTEGER(runEnd)[0] == NA_INTEGER ||
		(INTEGER(runEnd)[0] + 1) < INTEGER(runStart)[0] ||
		INTEGER(runEnd)[0] > LENGTH(values))
		error("invalid 'runWidth' argument");

	PROTECT(runWidth = NEW_INTEGER(1));
	INTEGER(runWidth)[0] = INTEGER(runEnd)[0] - INTEGER(runStart)[0] + 1;

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	PROTECT(ans_values = vector_OR_factor_extract_ranges(values,
					runStart, runWidth));
	PROTECT(ans_lengths = vector_OR_factor_extract_ranges(lengths,
					runStart, runWidth));

    if (INTEGER(runWidth)[0] > 0) {
        INTEGER(ans_lengths)[0] -= INTEGER(offsetStart)[0];
    	INTEGER(ans_lengths)[INTEGER(runWidth)[0] - 1] -= INTEGER(offsetEnd)[0];
    }

	SET_VECTOR_ELT(ans, 0, ans_values);
	SET_VECTOR_ELT(ans, 1, ans_lengths);
	SET_STRING_ELT(ans_names, 0, mkChar("values"));
	SET_STRING_ELT(ans_names, 1, mkChar("lengths"));
	SET_NAMES(ans, ans_names);

	UNPROTECT(5);

	return ans;
}

