#include "S4Vectors.h"

#include <stdlib.h>  /* for malloc, free */


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
 * 1st set of low-level helpers used by "The C level Rle smart constructors".
 */

static R_xlen_t sum_int_lengths(const int *lengths, R_xlen_t nrun)
{
	unsigned long long int sum;
	R_xlen_t i;

	sum = 0ULL;
	for (i = 0; i < nrun; i++, lengths++) {
		if (*lengths == NA_INTEGER || *lengths < 0)
			error("some run lengths are NAs or negative");
		sum += *lengths;
		if (sum > R_XLEN_T_MAX)
			error("Rle object is too long");
	}
	return (R_xlen_t) sum;
}

static R_xlen_t sum_llint_lengths(const long long int *lengths, R_xlen_t nrun)
{
	unsigned long long int sum;
	R_xlen_t i;

	sum = 0ULL;
	for (i = 0; i < nrun; i++, lengths++) {
		if (*lengths == NA_LINTEGER || *lengths < 0)
			error("some run lengths are NAs or negative");
		sum += *lengths;
		if (sum > R_XLEN_T_MAX)
			error("Rle object is too long");
	}
	return (R_xlen_t) sum;
}

static int lengths_out_storage(const void *lengths_in, R_xlen_t nrun_in,
			       int lengths_in_is_L)
{
	R_xlen_t sum;

	if (lengths_in == NULL)
		sum = nrun_in;
	else if (lengths_in_is_L)
		sum = sum_llint_lengths((const long long int *) lengths_in,
					nrun_in);
	else
		sum = sum_int_lengths((const int *) lengths_in, nrun_in);
	return sum > INT_MAX;
}

static void *alloc_values_buf(R_xlen_t buflength, int size)
{
	if (buflength == 0)
		return NULL;
	return R_alloc(buflength, size);
}

static void *alloc_lengths_buf(R_xlen_t buflength, int lengths_out_is_L)
{
	int size;

	if (buflength == 0)
		return NULL;
	size = lengths_out_is_L ? sizeof(long long int) : sizeof(int);
	return R_alloc(buflength, size);
}

static SEXP alloc_lengths(R_xlen_t nrun_out, int lengths_out_is_L,
			  void **dataptr_p)
{
	SEXP lengths;

	/* No need to PROTECT() */
	if (lengths_out_is_L) {
		lengths = _alloc_Linteger("Linteger", nrun_out);
		*dataptr_p = _get_Linteger_dataptr(lengths);
	} else {
		lengths = NEW_INTEGER(nrun_out);
		*dataptr_p = INTEGER(lengths);
	}
	return lengths;
}

static void memcpy_lengths(void *lengths_out, const void *lengths_buf,
			   R_xlen_t nrun_out, int lengths_out_is_L)
{
	int size;

	size = lengths_out_is_L ? sizeof(long long int) : sizeof(int);
	memcpy(lengths_out, lengths_buf, nrun_out * size);
	return;
}


/****************************************************************************
 * 2nd set of low-level helpers used by "The C level Rle smart constructors".
 *
 * All these functions return the final nb of runs (which is guaranteed to be
 * <= 'nrun_in'). If the pointers to the 'values_out' and 'lengths_out' arrays
 * are not NULL, they also write the final run values and lengths to these
 * arrays.
 */

static void set_Lin_lin(const long long int **Lin_p, const int **lin_p,
			const void *lengths_in, int lengths_in_is_L)
{
	if (lengths_in_is_L)
		*Lin_p = (const long long int *) lengths_in;
	else
		*lin_p = (const int *) lengths_in;
	return;
}

static void set_Lout_lout(long long int **Lout_p, int **lout_p,
			  const void *lengths_out, int lengths_out_is_L)
{
	if (lengths_out_is_L)
		*Lout_p = (long long int *) lengths_out;
	else
		*lout_p = (int *) lengths_out;
	return;
}

static R_xlen_t compute_int_runs(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	int *values_out, void *lengths_out, int lengths_out_is_L)
{
	const long long int *Lin;
	const int *lin;
	long long int *Lout, run_length;
	int *lout, val, val0 = 0;
	R_xlen_t nrun_out, i;

	set_Lin_lin(&Lin, &lin, lengths_in, lengths_in_is_L);
	set_Lout_lout(&Lout, &lout, lengths_out, lengths_out_is_L);
	nrun_out = 0;
	for (i = 0, run_length = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			run_length = lengths_in_is_L ? Lin[i] : lin[i];
			if (run_length == 0)
				continue;
		}
		val = *values_in;
		if (nrun_out != 0 && val == val0) {
			if (lengths_out != NULL) {
				if (lengths_out_is_L)
					Lout[nrun_out - 1] += run_length;
				else
					lout[nrun_out - 1] += run_length;
			}
			continue;
		}
		if (lengths_out != NULL) {
			if (lengths_out_is_L)
				Lout[nrun_out] = run_length;
			else
				lout[nrun_out] = run_length;
			values_out[nrun_out] = val;
		}
		nrun_out++;
		val0 = val;
	}
	return nrun_out;
}

#define	SAME_DOUBLE_VALS(x, y) \
	((x) == (y) || (R_IsNA(x) && R_IsNA(y)) || (R_IsNaN(x) && R_IsNaN(y)))

static R_xlen_t compute_double_runs(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L,
	double *values_out, void *lengths_out, int lengths_out_is_L)
{
	const long long int *Lin;
	const int *lin;
	long long int *Lout, run_length;
	int *lout;
	double val, val0;
	R_xlen_t nrun_out, i;

	set_Lin_lin(&Lin, &lin, lengths_in, lengths_in_is_L);
	set_Lout_lout(&Lout, &lout, lengths_out, lengths_out_is_L);
	nrun_out = 0;
	for (i = 0, run_length = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			run_length = lengths_in_is_L ? Lin[i] : lin[i];
			if (run_length == 0)
				continue;
		}
		val = *values_in;
		if (nrun_out != 0 && SAME_DOUBLE_VALS(val, val0)) {
			if (lengths_out != NULL) {
				if (lengths_out_is_L)
					Lout[nrun_out - 1] += run_length;
				else
					lout[nrun_out - 1] += run_length;
			}
			continue;
		}
		if (lengths_out != NULL) {
			if (lengths_out_is_L)
				Lout[nrun_out] = run_length;
			else
				lout[nrun_out] = run_length;
			values_out[nrun_out] = val;
		}
		nrun_out++;
		val0 = val;
	}
	return nrun_out;
}

static R_xlen_t compute_Rcomplex_runs(R_xlen_t nrun_in,
	const Rcomplex *values_in, const void *lengths_in, int lengths_in_is_L,
	Rcomplex *values_out, void *lengths_out, int lengths_out_is_L)
{
	const long long int *Lin;
	const int *lin;
	long long int *Lout, run_length;
	int *lout;
	Rcomplex val, val0 = {0.0, 0.0};
	R_xlen_t nrun_out, i;

	set_Lin_lin(&Lin, &lin, lengths_in, lengths_in_is_L);
	set_Lout_lout(&Lout, &lout, lengths_out, lengths_out_is_L);
	nrun_out = 0;
	for (i = 0, run_length = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			run_length = lengths_in_is_L ? Lin[i] : lin[i];
			if (run_length == 0)
				continue;
		}
		val = *values_in;
		if (nrun_out != 0 && SAME_DOUBLE_VALS(val.r, val0.r)
				  && SAME_DOUBLE_VALS(val.i, val0.i))
		{
			if (lengths_out != NULL) {
				if (lengths_out_is_L)
					Lout[nrun_out - 1] += run_length;
				else
					lout[nrun_out - 1] += run_length;
			}
			continue;
		}
		if (lengths_out != NULL) {
			if (lengths_out_is_L)
				Lout[nrun_out] = run_length;
			else
				lout[nrun_out] = run_length;
			values_out[nrun_out] = val;
		}
		nrun_out++;
		val0 = val;
	}
	return nrun_out;
}

static R_xlen_t compute_CHARSXP_runs(
	SEXP values_in, const void *lengths_in, int lengths_in_is_L,
	SEXP values_out, void *lengths_out, int lengths_out_is_L)
{
	const long long int *Lin;
	const int *lin;
	long long int *Lout, run_length;
	int *lout;
	SEXP val, val0 = NULL;
	R_xlen_t nrun_in, nrun_out, i;

	set_Lin_lin(&Lin, &lin, lengths_in, lengths_in_is_L);
	set_Lout_lout(&Lout, &lout, lengths_out, lengths_out_is_L);
	nrun_in = XLENGTH(values_in);
	nrun_out = 0;
	for (i = 0, run_length = 1; i < nrun_in; i++) {
		if (lengths_in != NULL) {
			run_length = lengths_in_is_L ? Lin[i] : lin[i];
			if (run_length == 0)
				continue;
		}
		val = STRING_ELT(values_in, i);
		if (nrun_out != 0 && val == val0) {
			if (lengths_out != NULL) {
				if (lengths_out_is_L)
					Lout[nrun_out - 1] += run_length;
				else
					lout[nrun_out - 1] += run_length;
			}
			continue;
		}
		if (lengths_out != NULL) {
			if (lengths_out_is_L)
				Lout[nrun_out] = run_length;
			else
				lout[nrun_out] = run_length;
			SET_STRING_ELT(values_out, nrun_out, val);
		}
		nrun_out++;
		val0 = val;
	}
	return nrun_out;
}

static R_xlen_t compute_Rbyte_runs(R_xlen_t nrun_in,
	const Rbyte *values_in, const void *lengths_in, int lengths_in_is_L,
	Rbyte *values_out, void *lengths_out, int lengths_out_is_L)
{
	const long long int *Lin;
	const int *lin;
	long long int *Lout, run_length;
	int *lout;
	Rbyte val, val0 = 0;
	R_xlen_t nrun_out, i;

	set_Lin_lin(&Lin, &lin, lengths_in, lengths_in_is_L);
	set_Lout_lout(&Lout, &lout, lengths_out, lengths_out_is_L);
	nrun_out = 0;
	for (i = 0, run_length = 1; i < nrun_in; i++, values_in++) {
		if (lengths_in != NULL) {
			run_length = lengths_in_is_L ? Lin[i] : lin[i];
			if (run_length == 0)
				continue;
		}
		val = *values_in;
		if (nrun_out != 0 && val == val0) {
			if (lengths_out != NULL) {
				if (lengths_out_is_L)
					Lout[nrun_out - 1] += run_length;
				else
					lout[nrun_out - 1] += run_length;
			}
			continue;
		}
		if (lengths_out != NULL) {
			if (lengths_out_is_L)
				Lout[nrun_out] = run_length;
			else
				lout[nrun_out] = run_length;
			values_out[nrun_out] = val;
		}
		nrun_out++;
		val0 = val;
	}
	return nrun_out;
}


/****************************************************************************
 * The C level Rle smart constructors.
 *
 * 'buflength' is the length of the temporary buffers allocated internally by
 * the smart constructor for computing the runs. If set to 0, then a 3-pass
 * (instead of 2-pass) algo is used that doesn't use any temporary buffer,
 * typically leading to 20%-30% less memory used (it also seems slightly
 * faster on my machine).
 * Setting 'buflength' to 'nrun_in' is safe because the number of runs can
 * only be <= 'nrun_in'. If 'buflength' is > 'nrun_in', then 'nrun_in' is
 * used instead.
 * WARNING: Don't call _construct_logical_Rle with a 'buflength' that is > 0
 * and < 'nrun_in' unless you know what you are doing!
 */

SEXP _construct_logical_Rle(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	int lengths_out_is_L;
	int *values_buf, *values_out;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP ans_lengths, ans_values, ans;

	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = (int *) alloc_values_buf(buflength, sizeof(int));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_int_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	/* Allocate 'ans_values' and 'ans_lengths' */
	PROTECT(ans_values = NEW_LOGICAL(nrun_out));
	values_out = LOGICAL(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	/* Fill 'ans_values' and 'ans_lengths' */
	if (buflength == 0) {
		/* 3rd pass */
		compute_int_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_out, lengths_out, lengths_out_is_L);
	} else {
		memcpy(values_out, values_buf, nrun_out * sizeof(int));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_integer_Rle(R_xlen_t nrun_in,
	const int *values_in, const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	int lengths_out_is_L;
	int *values_buf, *values_out;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP ans_lengths, ans_values, ans;

	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = (int *) alloc_values_buf(buflength, sizeof(int));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_int_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	/* Allocate 'ans_values' and 'ans_lengths' */
	PROTECT(ans_values = NEW_INTEGER(nrun_out));
	values_out = INTEGER(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	/* Fill 'ans_values' and 'ans_lengths' */
	if (buflength == 0) {
		/* 3rd pass */
		compute_int_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_out, lengths_out, lengths_out_is_L);
	} else {
		memcpy(values_out, values_buf, nrun_out * sizeof(int));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_numeric_Rle(R_xlen_t nrun_in,
	const double *values_in, const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	int lengths_out_is_L;
	double *values_buf, *values_out;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP ans_lengths, ans_values, ans;

	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = (double *) alloc_values_buf(buflength, sizeof(double));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_double_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	/* Allocate 'ans_values' and 'ans_lengths' */
	PROTECT(ans_values = NEW_NUMERIC(nrun_out));
	values_out = REAL(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	/* Fill 'ans_values' and 'ans_lengths' */
	if (buflength == 0) {
		/* 3rd pass */
		compute_double_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_out, lengths_out, lengths_out_is_L);
	} else {
		memcpy(values_out, values_buf, nrun_out * sizeof(double));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_complex_Rle(R_xlen_t nrun_in,
	const Rcomplex *values_in, const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	int lengths_out_is_L;
	Rcomplex *values_buf, *values_out;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP ans_lengths, ans_values, ans;

	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = (Rcomplex *) alloc_values_buf(buflength, sizeof(Rcomplex));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_Rcomplex_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	/* Allocate 'ans_values' and 'ans_lengths' */
	PROTECT(ans_values = NEW_COMPLEX(nrun_out));
	values_out = COMPLEX(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	/* Fill 'ans_values' and 'ans_lengths' */
	if (buflength == 0) {
		/* 3rd pass */
		compute_Rcomplex_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_out, lengths_out, lengths_out_is_L);
	} else {
		memcpy(values_out, values_buf, nrun_out * sizeof(Rcomplex));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_character_Rle(SEXP values_in,
	const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	R_xlen_t nrun_in;
	int lengths_out_is_L, i;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP values_buf, ans_lengths, ans_values, ans;

	nrun_in = XLENGTH(values_in);
	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = buflength == 0 ? NULL : PROTECT(NEW_CHARACTER(buflength));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_CHARSXP_runs(
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	PROTECT(ans_values = NEW_CHARACTER(nrun_out));
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_CHARSXP_runs(
			values_in, lengths_in, lengths_in_is_L,
			ans_values, lengths_out, lengths_out_is_L);
	} else {
		for (i = 0; i < nrun_out; i++)
		    SET_STRING_ELT(ans_values, i, STRING_ELT(values_buf, i));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(buflength == 0 ? 3 : 4);
	return ans;
}

SEXP _construct_raw_Rle(R_xlen_t nrun_in,
	const Rbyte *values_in, const void *lengths_in, int lengths_in_is_L,
	R_xlen_t buflength)
{
	int lengths_out_is_L;
	Rbyte *values_buf, *values_out;
	void *lengths_buf, *lengths_out;
	R_xlen_t nrun_out;
	SEXP ans_lengths, ans_values, ans;

	/* 1st pass: find out 'lengths_out' storage base on length of Rle */
	lengths_out_is_L = lengths_out_storage(lengths_in, nrun_in,
					       lengths_in_is_L);
	if (buflength > nrun_in)
		buflength = nrun_in;
	values_buf = (Rbyte *) alloc_values_buf(buflength, sizeof(Rbyte));
	lengths_buf = alloc_lengths_buf(buflength, lengths_out_is_L);
	/* 2nd pass */
	nrun_out = compute_Rbyte_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_buf, lengths_buf, lengths_out_is_L);
	/* Allocate 'ans_values' and 'ans_lengths' */
	PROTECT(ans_values = NEW_RAW(nrun_out));
	values_out = RAW(ans_values);
	PROTECT(ans_lengths = alloc_lengths(nrun_out, lengths_out_is_L,
					    &lengths_out));
	/* Fill 'ans_values' and 'ans_lengths' */
	if (buflength == 0) {
		/* 3rd pass */
		compute_Rbyte_runs(nrun_in,
			values_in, lengths_in, lengths_in_is_L,
			values_out, lengths_out, lengths_out_is_L);
	} else {
		memcpy(values_out, values_buf, nrun_out * sizeof(Rbyte));
		memcpy_lengths(lengths_out, lengths_buf, nrun_out,
			       lengths_out_is_L);
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _construct_Rle(SEXP values_in, const void *lengths_in,
		    int lengths_in_is_L, R_xlen_t buflength)
{
	R_xlen_t nrun_in;
	SEXP ans, ans_values, ans_values_class, ans_values_levels;

	nrun_in = XLENGTH(values_in);
	switch (TYPEOF(values_in)) {
	    case LGLSXP:
		PROTECT(ans = _construct_logical_Rle(nrun_in,
					LOGICAL(values_in), lengths_in,
					lengths_in_is_L, buflength));
		break;
	    case INTSXP:
		PROTECT(ans = _construct_integer_Rle(nrun_in,
					INTEGER(values_in), lengths_in,
					lengths_in_is_L, buflength));
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
					lengths_in_is_L, buflength));
		break;
	    case CPLXSXP:
		PROTECT(ans = _construct_complex_Rle(nrun_in,
					COMPLEX(values_in), lengths_in,
					lengths_in_is_L, buflength));
		break;
	    case STRSXP:
		PROTECT(ans = _construct_character_Rle(
					values_in, lengths_in,
					lengths_in_is_L, buflength));
		break;
	    case RAWSXP:
		PROTECT(ans = _construct_raw_Rle(nrun_in,
					RAW(values_in), lengths_in,
					lengths_in_is_L, buflength));
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
 *   lengths_in: An integer or Linteger vector of the same length as 'values'
 *               with no NAs or negative values, or a NULL. If NULL then all
 *               the runs are considered to be of length 1 like if
 *               lengths_in was 'rep(1, length(values))'.
 *   buflength: A single double.
 */

SEXP Rle_constructor(SEXP values_in, SEXP lengths_in, SEXP buflength)
{
	R_xlen_t nrun_in, lengths_in_len, buflength0;
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
		} else if (isObject(lengths_in) &&
			   strcmp(CHAR(STRING_ELT(GET_CLASS(lengths_in), 0)),
				  "Linteger") == 0)
		{
			lengths_in_is_L = 1;
			lengths_in_len = _get_Linteger_length(lengths_in);
			lengths_in_dataptr = _get_Linteger_dataptr(lengths_in);
		} else {
			error("the supplied 'lengths' must be an integer or "
			      "Linteger vector or NULL");
		}
		if (nrun_in != lengths_in_len)
			error("'length(values)' != 'length(lengths)'");
	}
	buflength0 = (R_xlen_t) REAL(buflength)[0];
	return _construct_Rle(values_in, lengths_in_dataptr, lengths_in_is_L,
			      buflength0);
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
 * ranges_to_runs_mapper()
 */

static char errmsg_buf[200];

static const char *range2runs_mapper1(const int *run_lengths, int nrun,
		int range_start, int range_end,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim)
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
			*Ltrim = range_start - offset + run_lengths[i] - 1;
		if (offset >= range_end) {
			j = i;
		} else {
			for (j = i + 1; j < nrun; j++) {
				offset += run_lengths[j];
				if (offset >= range_end)
					break;
			}
		}
		*Rtrim = offset - range_end;
		*spanned_nrun = j - i + 1;
	} else {
		/* Zero-width range. */
		*spanned_nrun = 0;
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
	*offset_nrun = i;
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
 * Like range2runs_mapper1() but takes 'run_breakpoints' (obtained with
 * 'cumsum(run_lengths)') instead of 'run_lengths' as input and uses a binary
 * search.
 */
static const char *range2runs_mapper2(const int *run_breakpoints, int nrun,
		int range_start, int range_end,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim)
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
	*offset_nrun = int_bsearch(range_start, run_breakpoints, nrun);
	if (range_end >= range_start) {
		end_run = int_bsearch(range_end, run_breakpoints, nrun);
		*spanned_nrun = end_run - *offset_nrun + 1;
		*Ltrim = range_start - 1;
		if (*offset_nrun >= 1)
			*Ltrim -= run_breakpoints[*offset_nrun - 1];
		*Rtrim = run_breakpoints[end_run] - range_end;
	} else {
		/* Zero-width range. */
		*spanned_nrun = 0;
	}
	return NULL;
}

/* Method 1: Naive algo (inefficient if more than 1 range). */
static const char *ranges2runs_mapper1(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim)
{
	int i, start_i, end_i;
	const char *errmsg;

	errmsg = NULL;
	for (i = 0; i < nranges; i++) {
		start_i = start[i];
		end_i = start_i - 1 + width[i];
		errmsg = range2runs_mapper1(run_lengths, nrun,
					start_i, end_i,
					offset_nrun + i, spanned_nrun + i,
					Ltrim + i, Rtrim + i);
		if (errmsg != NULL)
			break;
	}
	return errmsg;
}

/* Method 2: Binary search. */
static const char *ranges2runs_mapper2(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim)
{
	int *run_breakpoints, breakpoint, i, start_i, end_i;
	const char *errmsg;

	run_breakpoints = (int *) malloc(sizeof(int) * nrun);
	if (run_breakpoints == NULL) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "ranges2runs_mapper2: memory allocation failed");
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
		errmsg = range2runs_mapper2(run_breakpoints, nrun,
					start_i, end_i,
					offset_nrun + i, spanned_nrun + i,
					Ltrim + i, Rtrim + i);
		if (errmsg != NULL)
			break;
	}
	free(run_breakpoints);
	return errmsg; 
}

/* Method 3: Sort the starts and ends of the ranges. */
static const char *ranges2runs_mapper3(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim)
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
			 "ranges2runs_mapper3: memory allocation failed");
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
			Ltrim[i] = - breakpoint;
			if (SE > breakpoint) {
				SE_run = j;
			} else {
				SE_run = j - 1;
				Ltrim[i] += run_lengths[SE_run];
			}
			offset_nrun[i] = SE_run;
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
			Rtrim[i] = breakpoint;
			SE_run = j - 1;
			spanned_nrun[i] = SE_run;
		}
	}
	for (i = 0; i < nranges; i++) {
		if (width[i] != 0) {
			spanned_nrun[i] -= offset_nrun[i] - 1;
			Ltrim[i] += start[i] - 1;
			Rtrim[i] -= SEbuf2[i];
		} else {
			/* Zero-width range. */
			spanned_nrun[i] = 0;
		}
	}
	free(SEbuf);
	free(SEorder);
	return NULL; 
}

/* If 'method' is not >= 0 and <= 3, then the function does nothing (no-op). */
static const char *ranges2runs_mapper(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim,
		int method)
{
	const char *(*fun)(const int *run_lengths, int nrun,
		const int *start, const int *width, int nranges,
		int *offset_nrun, int *spanned_nrun, int *Ltrim, int *Rtrim);

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
		case 1: fun = ranges2runs_mapper1; break;
		case 2: fun = ranges2runs_mapper2; break;
		case 3: fun = ranges2runs_mapper3; break;
		default: return NULL;  /* do nothing */
	}
	return fun(run_lengths, nrun,
		   start, width, nranges,
		   offset_nrun, spanned_nrun, Ltrim, Rtrim);
}

/* --- .Call ENTRY POINT --- */
SEXP ranges_to_runs_mapper(SEXP run_lengths, SEXP start, SEXP width,
		SEXP method)
{
	SEXP offset_nrun, spanned_nrun, Ltrim, Rtrim, ans;
	int nrun, nranges;
	const int *start_p, *width_p;
	const char *errmsg;

	nrun = LENGTH(run_lengths);
	nranges = _check_integer_pairs(start, width,
				       &start_p, &width_p,
				       "start", "width");
	PROTECT(offset_nrun = NEW_INTEGER(nranges));
	PROTECT(spanned_nrun = NEW_INTEGER(nranges));
	PROTECT(Ltrim = NEW_INTEGER(nranges));
	PROTECT(Rtrim = NEW_INTEGER(nranges));
	errmsg = ranges2runs_mapper(INTEGER(run_lengths), nrun,
				start_p, width_p, nranges,
				INTEGER(offset_nrun), INTEGER(spanned_nrun),
				INTEGER(Ltrim), INTEGER(Rtrim),
				INTEGER(method)[0]);
	if (errmsg != NULL) {
		UNPROTECT(4);
		error(errmsg);
	}
	PROTECT(ans = NEW_LIST(4));
	SET_VECTOR_ELT(ans, 0, offset_nrun);
	SET_VECTOR_ELT(ans, 1, spanned_nrun);
	SET_VECTOR_ELT(ans, 2, Ltrim);
	SET_VECTOR_ELT(ans, 3, Rtrim);
	UNPROTECT(5);
	return ans;
}


/****************************************************************************
 * Rle_extract_range() and Rle_extract_ranges()
 */

static SEXP extract_Rle_range(SEXP x_values, const int *x_lengths,
		int start_nrun, int spanned_nrun, int Ltrim, int Rtrim)
{
	SEXP ans_values, ans_lengths, ans;

	PROTECT(ans_values = _subset_vectorORfactor_by_ranges(x_values,
					&start_nrun, &spanned_nrun, 1));
	PROTECT(ans_lengths = NEW_INTEGER(spanned_nrun));
	if (spanned_nrun != 0) {
		memcpy(INTEGER(ans_lengths),
		       x_lengths + start_nrun - 1,
		       sizeof(int) * spanned_nrun);
		INTEGER(ans_lengths)[0] -= Ltrim;
		INTEGER(ans_lengths)[spanned_nrun - 1] -= Rtrim;
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

/*
 * Extract 'nranges' Rle's from 'x'. Each Rle to extract is specified by
 * 'start_nrun[i]', 'spanned_nrun[i]', 'Ltrim[i]', and 'Rtrim[i]'.
 * If 'as_list' is TRUE, then the extracted Rle's are returned in a list of
 * length 'nranges'. Otherwise, the single Rle obtained by concatenating them
 * all together is returned.
 */
static SEXP subset_Rle_by_runs(SEXP x,
		const int *start_nrun, const int *spanned_nrun,
		const int *Ltrim, const int *Rtrim, int nranges,
		int as_list)
{
	SEXP x_values, x_lengths, tmp_values, ans, ans_elt;
	int tmp_nrun, *tmp_lengths, i, n;

	x_values = GET_SLOT(x, install("values"));
	x_lengths = GET_SLOT(x, install("lengths"));
	if (as_list == 1) {
		PROTECT(ans = NEW_LIST(nranges));
		for (i = 0; i < nranges; i++) {
			PROTECT(ans_elt = extract_Rle_range(x_values,
						INTEGER(x_lengths),
						start_nrun[i], spanned_nrun[i],
						Ltrim[i], Rtrim[i]));
			SET_VECTOR_ELT(ans, i, ans_elt);
			UNPROTECT(1);
		}
		UNPROTECT(1);
		return ans;
	}
	if (nranges == 1)
		return extract_Rle_range(x_values, INTEGER(x_lengths),
				start_nrun[0], spanned_nrun[0],
				Ltrim[0], Rtrim[0]);
	PROTECT(tmp_values = _subset_vectorORfactor_by_ranges(x_values,
					start_nrun, spanned_nrun, nranges));
	tmp_nrun = LENGTH(tmp_values);
	tmp_lengths = (int *) R_alloc(sizeof(int), tmp_nrun);
	for (i = tmp_nrun = 0; i < nranges; i++) {
		n = spanned_nrun[i];
		if (n == 0)
			continue;
		memcpy(tmp_lengths + tmp_nrun,
		       INTEGER(x_lengths) + start_nrun[i] - 1,
		       sizeof(int) * n);
		tmp_lengths[tmp_nrun] -= Ltrim[i];
		tmp_nrun += n;
		tmp_lengths[tmp_nrun - 1] -= Rtrim[i];
	}
	PROTECT(ans = _construct_Rle(tmp_values, tmp_lengths, 0, 0));
	UNPROTECT(2);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_extract_range(SEXP x, SEXP start, SEXP end)
{
	int nranges, x_nrun, offset_nrun, spanned_nrun, Ltrim, Rtrim;
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
	errmsg = range2runs_mapper1(INTEGER(x_lengths), x_nrun,
				range_start_p[0], range_end_p[0],
				&offset_nrun, &spanned_nrun, &Ltrim, &Rtrim);
	if (errmsg != NULL)
		error(errmsg);
	offset_nrun++;  /* add 1 to get the start */
	return extract_Rle_range(x_values, INTEGER(x_lengths),
				 offset_nrun, spanned_nrun, Ltrim, Rtrim);
}

SEXP _subset_Rle_by_ranges(SEXP x,
		const int *start, const int *width, int nranges,
		int method, int as_list)
{
	SEXP x_lengths;
	int x_nrun, *offset_nrun, *spanned_nrun, *Ltrim, *Rtrim, i;
	const char *errmsg;

	x_lengths = GET_SLOT(x, install("lengths"));
	x_nrun = LENGTH(x_lengths);
	offset_nrun = (int *) R_alloc(sizeof(int), nranges);
	spanned_nrun = (int *) R_alloc(sizeof(int), nranges);
	Ltrim = (int *) R_alloc(sizeof(int), nranges);
	Rtrim = (int *) R_alloc(sizeof(int), nranges);
	errmsg = ranges2runs_mapper(INTEGER(x_lengths), x_nrun,
				start, width, nranges,
				offset_nrun, spanned_nrun, Ltrim, Rtrim,
				method);
	if (errmsg != NULL)
		error(errmsg);
	for (i = 0; i < nranges; i++)
		offset_nrun[i]++;  /* add 1 to get the start */
	return subset_Rle_by_runs(x, offset_nrun, spanned_nrun,
				     Ltrim, Rtrim, nranges, as_list);
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
	PROTECT(ans_values = vectorORfactor_extract_ranges(values,
					runStart, runWidth));
	PROTECT(ans_lengths = vectorORfactor_extract_ranges(lengths,
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

