/****************************************************************************
 *                Low-level manipulation of Linteger objects                *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"

#include <ctype.h>  /* for isspace() and isdigit() */

#define	BYTES_PER_LINTEGER	(sizeof(long long int) / sizeof(char))
#define	NEW_LINTEGER(n)		_alloc_Linteger("Linteger", (n))
#define	LINTEGER(x)		_get_Linteger_dataptr(x)

/* --- .Call ENTRY POINT --- */
SEXP make_RAW_from_NA_LINTEGER()
{
	SEXP ans;

	PROTECT(ans = NEW_RAW(BYTES_PER_LINTEGER));
	*((long long int *) RAW(ans)) = NA_LINTEGER;
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * C-level getters and setter.
 */

static SEXP bytes_symbol = NULL;

static SEXP get_Linteger_bytes(SEXP x)
{
	INIT_STATIC_SYMBOL(bytes)
	return GET_SLOT(x, bytes_symbol);
}

R_xlen_t _get_Linteger_length(SEXP x)
{
	return XLENGTH(get_Linteger_bytes(x)) / BYTES_PER_LINTEGER;
}

long long int *_get_Linteger_dataptr(SEXP x)
{
	return (long long int *) RAW(get_Linteger_bytes(x));
}

static void set_Linteger_bytes(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(bytes)
	SET_SLOT(x, bytes_symbol, value);
	return;
}


/****************************************************************************
 * C-level constructors.
 *
 * Be aware that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
 * Thus they cannot be made .Call entry points!
 */

static SEXP new_Linteger_from_bytes(const char *classname, SEXP bytes)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_Linteger_bytes(ans, bytes);
	UNPROTECT(2);
	return ans;
}

/* Allocation WITHOUT initialization. */
SEXP _alloc_Linteger(const char *classname, R_xlen_t length)
{
	SEXP bytes, ans;

	PROTECT(bytes = NEW_RAW(length * BYTES_PER_LINTEGER));
	PROTECT(ans = new_Linteger_from_bytes(classname, bytes));
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * Low-level coercion helper functions
 */

static void from_ints_to_llints(const int *from, long long int *to,
				R_xlen_t n)
{
	R_xlen_t i;
	int from_elt;

	for (i = 0; i < n; i++, from++, to++) {
		from_elt = *from;
		if (from_elt == NA_INTEGER) {
			*to = NA_LINTEGER;
			continue;
		}
		*to = (long long int) from_elt;
	}
	return;
}

static void from_doubles_to_llints(const double *from, long long int *to,
				   R_xlen_t n)
{
	int first_time;
	R_xlen_t i;
	double from_elt;

	first_time = 1;
	for (i = 0; i < n; i++, from++, to++) {
		from_elt = *from;
		if (from_elt == NA_REAL) {
			*to = NA_LINTEGER;
			continue;
		}
		if (from_elt > (double) LLONG_MAX ||
		    from_elt < (double) -LLONG_MAX)
		{
			if (first_time) {
				warning("out-of-range values coerced to NAs "
					"in coercion to Linteger");
				first_time = 0;
			}
			*to = NA_LINTEGER;
			continue;
		}
		*to = (long long int) from_elt;
	}
	return;
}

/* Return 1 if the string to parse 's' contains a number that is syntactically
   correct AND cannot be represented by a long long int (overflow).
   Otherwise return 0. */
static int scan_llint(const char *s, long long int *out)
{
	char c, sign;
	long long int val;

	*out = NA_LINTEGER;
	/* Skip leading spaces. */
	while (isspace(c = *(s++))) {};
	if (c == '\0') 
		return 0;  /* syntactically incorrect */
	/* Scan unary +/- sign. */
	if (c == '+' || c == '-') {
		sign = c;
		c = *(s++);
	} else {
		sign = '+';
	}
	if (!isdigit(c))
		return 0;  /* syntactically incorrect */
	/* Scan digits. */
	_reset_ovflow_flag();
	val = c - '0';
	while (isdigit(c = *(s++))) {
		val = _safe_llint_mult(val, 10LL);
		val = _safe_llint_add(val, (long long int) c - '0');
	}
	if (sign == '-')
		val = -val;
	if (c == '\0')
		goto syntactically_correct;
	/* Scan decimal part. */
	if (c == '.') {
		/* Decimal part is ignored. */
		while (isdigit(c = *(s++))) {};
		if (c == '\0')
			goto syntactically_correct;
	}
	/* Skip trailing spaces. */
	if (isspace(c))
		while (isspace(c = *(s++))) {};
	if (c != '\0')
		return 0;  /* syntactically incorrect */
	syntactically_correct:
	*out = val;
	return _get_ovflow_flag();
}

static void from_STRSXP_to_llints(SEXP from, long long int *to)
{
	R_xlen_t n, i;
	int first_time1, first_time2;
	SEXP from_elt;

	n = XLENGTH(from);
	first_time1 = first_time2 = 1;
	for (i = 0; i < n; i++, to++) {
		from_elt = STRING_ELT(from, i);
		if (from_elt == NA_STRING) {
			*to = NA_LINTEGER;
			continue;
		}
		if (scan_llint(CHAR(from_elt), to)) {
			/* syntactically correct number but overflow */
			if (first_time1) {
				warning("out-of-range values coerced to NAs "
					"in coercion to Linteger");
				first_time1 = 0;
			}
			continue;
		}
		if (*to != NA_LINTEGER)
			continue;
		if (first_time2) {
			/* syntactically incorrect number */
			warning("syntactically incorrect numbers "
				"coerced to NAs in coercion to Linteger");
			first_time2 = 0;
		}
	}
	return;
}

static void from_llints_to_bools(const long long int *from, int *to,
				 R_xlen_t n)
{
	R_xlen_t i;
	long long int from_elt;

	for (i = 0; i < n; i++, from++, to++) {
		from_elt = *from;
		if (from_elt == NA_LINTEGER) {
			*to = NA_LOGICAL;
			continue;
		}
		*to = from_elt != 0LL;
	}
	return;
}

static void from_llints_to_ints(const long long int *from, int *to,
				R_xlen_t n)
{
	int first_time;
	R_xlen_t i;
	long long int from_elt;

	first_time = 1;
	for (i = 0; i < n; i++, from++, to++) {
		from_elt = *from;
		if (from_elt == NA_LINTEGER) {
			*to = NA_INTEGER;
			continue;
		}
		if (from_elt > (long long int) INT_MAX ||
		    from_elt < (long long int) -INT_MAX)
		{
			if (first_time) {
				warning("out-of-range values coerced to NAs "
					"in coercion to integer");
				first_time = 0;
			}
			*to = NA_INTEGER;
			continue;
		}
		*to = (int) from_elt;
	}
	return;
}

static void from_llints_to_doubles(const long long int *from, double *to,
				   R_xlen_t n)
{
	int first_time;
	R_xlen_t i;
	long long int from_elt;

	first_time = 1;
	for (i = 0; i < n; i++, from++, to++) {
		from_elt = *from;
		if (from_elt == NA_LINTEGER) {
			*to = NA_REAL;
			continue;
		}
		*to = (double) from_elt;
		if (first_time && (long long int) *to != from_elt) {
			warning("non reversible coercion to double "
				"(integer values > 2^53 cannot be exactly\n"
                                "  represented by double values)");
			first_time = 0;
		}
	}
	return;
}

static void from_llints_to_STRSXP(const long long int *from, SEXP to)
{
	R_xlen_t n, i;
	long long int from_elt;
	/* LLONG_MAX is 19 digits + sign + terminating null byte */
	char val_buf[21];
	SEXP to_elt;

	n = XLENGTH(to);
	for (i = 0; i < n; i++, from++) {
		from_elt = *from;
		if (from_elt == NA_LINTEGER) {
			SET_STRING_ELT(to, i, NA_STRING);
			continue;
		}
		/* sprintf() should always succeed here but we check for an
		   error anyway, just to be safe. */
		if (sprintf(val_buf, "%lld", from_elt) < 0)
			error("S4Vectors internal error in "
			      "from_llints_to_STRSXP(): "
			      "sprintf() returned a negative value");
		PROTECT(to_elt = mkChar(val_buf));
		SET_STRING_ELT(to, i, to_elt);
		UNPROTECT(1);
	}
	return;
}


/****************************************************************************
 * Coercion.
 */

static SEXP new_Linteger_from_ints(const int *x, R_xlen_t x_len)
{
	SEXP ans;

	PROTECT(ans = NEW_LINTEGER(x_len));
	from_ints_to_llints(x, LINTEGER(ans), x_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_from_LOGICAL(SEXP x)
{
	return new_Linteger_from_ints(LOGICAL(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_from_INTEGER(SEXP x)
{
	return new_Linteger_from_ints(INTEGER(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_from_NUMERIC(SEXP x)
{
	R_xlen_t x_len;
	SEXP ans;

	x_len = XLENGTH(x);
	PROTECT(ans = NEW_LINTEGER(x_len));
	from_doubles_to_llints(REAL(x), LINTEGER(ans), x_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_from_CHARACTER(SEXP x)
{
	R_xlen_t x_len;
	SEXP ans;

	x_len = XLENGTH(x);
	PROTECT(ans = NEW_LINTEGER(x_len));
	from_STRSXP_to_llints(x, LINTEGER(ans));
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_LOGICAL_from_Linteger(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_Linteger_length(x);
	PROTECT(ans = NEW_LOGICAL(ans_len));
	from_llints_to_bools(LINTEGER(x), LOGICAL(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_INTEGER_from_Linteger(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_Linteger_length(x);
	PROTECT(ans = NEW_INTEGER(ans_len));
	from_llints_to_ints(LINTEGER(x), INTEGER(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NUMERIC_from_Linteger(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_Linteger_length(x);
	PROTECT(ans = NEW_NUMERIC(ans_len));
	from_llints_to_doubles(LINTEGER(x), REAL(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_CHARACTER_from_Linteger(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_Linteger_length(x);
	PROTECT(ans = NEW_CHARACTER(ans_len));
	from_llints_to_STRSXP(LINTEGER(x), ans);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Operations from "Ops" group
 */

static void print_not_multiple_warning()
{
	warning("longer object length is not a multiple "
		"of shorter object length");
	return;
}

static R_xlen_t compute_ans_length(R_xlen_t e1_len, R_xlen_t e2_len)
{
	if (e1_len == 0 || e2_len == 0)
		return 0;
	if (e1_len >= e2_len) {
		if (e1_len % e2_len != 0)
			print_not_multiple_warning();
		return e1_len;
	}
	if (e2_len % e1_len != 0)
		print_not_multiple_warning();
	return e2_len;
}

static long long int llint_div(long long int x, long long int y)
{
	long long int z;

	if (x == NA_LINTEGER || y == NA_LINTEGER || y == 0LL)
		return NA_LINTEGER;
	z = x / y;
	if (x == 0LL || (x > 0LL) == (y > 0LL) || y * z == x)
		return z;
	return z - 1LL;
}

static long long int llint_mod(long long int x, long long int y)
{
	long long int z;

	if (x == NA_LINTEGER || y == NA_LINTEGER || y == 0LL)
		return NA_LINTEGER;
	z = x % y;
	/* The contortions below are meant to make sure that the result
	   has the sign of 'y'. */
	if (z == 0LL || (z > 0LL) == (y > 0LL))
		return z;
	/* z and y have opposite signs. */
	return z + y;  /* same sign as 'y' */
}

static double llint_div_as_double(long long int x, long long int y)
{
	if (x == NA_LINTEGER || y == NA_LINTEGER)
		return NA_REAL;
	return (double) x / (double) y;
}

static double llint_pow_as_double(long long int x, long long int y)
{
	if (x == 1LL || y == 0LL)
		return 1.0;
	if (x == NA_LINTEGER || y == NA_LINTEGER)
		return NA_REAL;
	return pow((double) x, (double) y);
}

typedef long long int (*Arith1FunType)(long long int x, long long int y);
typedef double (*Arith2FunType)(long long int x, long long int y);

static Arith1FunType get_arith1_fun(const char *generic)
{
	if (strcmp(generic, "+") == 0)
		return _safe_llint_add;
	if (strcmp(generic, "-") == 0)
		return _safe_llint_subtract;
	if (strcmp(generic, "*") == 0)
		return _safe_llint_mult;
	if (strcmp(generic, "%/%") == 0)
		return llint_div;
	if (strcmp(generic, "%%") == 0)
		return llint_mod;
	return NULL;
}

static Arith2FunType get_arith2_fun(const char *generic)
{
	if (strcmp(generic, "/") == 0)
		return llint_div_as_double;
	if (strcmp(generic, "^") == 0)
		return llint_pow_as_double;
	return NULL;
}

static void llints_arith1(Arith1FunType arith_fun,
		const long long int *x, R_xlen_t x_len,
		const long long int *y, R_xlen_t y_len,
		long long int *out, R_xlen_t out_len)
{
	R_xlen_t i, j, k;

	_reset_ovflow_flag();
	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0;
		if (j >= y_len)
			j = 0;
		out[k] = arith_fun(x[i], y[j]);
	}
	if (_get_ovflow_flag())
		warning("NAs produced by Linteger overflow");
	return;
}

static void llints_arith2(Arith2FunType arith_fun,
		const long long int *x, R_xlen_t x_len,
		const long long int *y, R_xlen_t y_len,
		double *out, R_xlen_t out_len)
{
	R_xlen_t i, j, k;

	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0;
		if (j >= y_len)
			j = 0;
		out[k] = arith_fun(x[i], y[j]);
	}
	return;
}

/* Operations from "Compare" group */
#define	EQ_OP	1  /* equal to */
#define	NEQ_OP	2  /* not equal to */
#define	LEQ_OP	3  /* less than or equal to */
#define	GEQ_OP	4  /* greater than or equal to */
#define	LT_OP	5  /* less than */
#define	GT_OP	6  /* greater than */

static int get_compare_op(const char *generic)
{
	if (strcmp(generic, "==") == 0)
		return EQ_OP;
	if (strcmp(generic, "!=") == 0)
		return NEQ_OP;
	if (strcmp(generic, "<=") == 0)
		return LEQ_OP;
	if (strcmp(generic, ">=") == 0)
		return GEQ_OP;
	if (strcmp(generic, "<") == 0)
		return LT_OP;
	if (strcmp(generic, ">") == 0)
		return GT_OP;
	return 0;
}

static void llints_compare(int op,
		const long long int *x, R_xlen_t x_len,
		const long long int *y, R_xlen_t y_len,
		int *out, R_xlen_t out_len)
{
	R_xlen_t i, j, k;
	long long int x_elt, y_elt;

	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0;
		if (j >= y_len)
			j = 0;
		x_elt = x[i];
		y_elt = y[j];
		if (x_elt == NA_LINTEGER || y_elt == NA_LINTEGER) {
			out[k] = NA_LOGICAL;
			continue;
		}
		switch (op) {
			case EQ_OP:
				out[k] = x_elt == y_elt;
				break;
			case NEQ_OP:
				out[k] = x_elt != y_elt;
				break;
			case LEQ_OP:
				out[k] = x_elt <= y_elt;
				break;
			case GEQ_OP:
				out[k] = x_elt >= y_elt;
				break;
			case LT_OP:
				out[k] = x_elt < y_elt;
				break;
			case GT_OP:
				out[k] = x_elt > y_elt;
				break;
		}
	}
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP Linteger_Ops(SEXP Generic, SEXP e1, SEXP e2)
{
	R_xlen_t e1_len, e2_len, ans_len;
	const long long int *e1_elts, *e2_elts;
	const char *generic;
	Arith1FunType arith1_fun;
	Arith2FunType arith2_fun;
	int compare_op;
	SEXP ans;

	e1_len = _get_Linteger_length(e1);
	e2_len = _get_Linteger_length(e2);
	ans_len = compute_ans_length(e1_len, e2_len);
	e1_elts = LINTEGER(e1);
	e2_elts = LINTEGER(e2);
	generic = CHAR(STRING_ELT(Generic, 0));

	/* Operations from "Arith" group */
	arith1_fun = get_arith1_fun(generic);
	if (arith1_fun != NULL) {
		PROTECT(ans = NEW_LINTEGER(ans_len));
		llints_arith1(arith1_fun, e1_elts, e1_len, e2_elts, e2_len,
					  LINTEGER(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}
	arith2_fun = get_arith2_fun(generic);
	if (arith2_fun != NULL) {
		PROTECT(ans = NEW_NUMERIC(ans_len));
		llints_arith2(arith2_fun, e1_elts, e1_len, e2_elts, e2_len,
					  REAL(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}

	/* Operations from "Compare" group */
	compare_op = get_compare_op(generic);
	if (compare_op != 0) {
		PROTECT(ans = NEW_LOGICAL(ans_len));
		llints_compare(compare_op, e1_elts, e1_len, e2_elts, e2_len,
					   LOGICAL(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}

	error("\"%s\": operation not supported on Linteger objects", generic);
	return R_NilValue;
}


/****************************************************************************
 * Operations from "Summary" group
 */

#define	MAX_OP	1
#define	MIN_OP	2
#define	SUM_OP	3
#define	PROD_OP	4

static int get_summary_op(const char *generic)
{
	if (strcmp(generic, "max") == 0)
		return MAX_OP;
	if (strcmp(generic, "min") == 0)
		return MIN_OP;
	if (strcmp(generic, "sum") == 0)
		return SUM_OP;
	if (strcmp(generic, "prod") == 0)
		return PROD_OP;
	return 0;
}

static long long int llints_summary(int op,
		const long long int *in, R_xlen_t in_len, int na_rm)
{
	R_xlen_t i;
	long long int res, in_elt;

	switch (op) {
		case MAX_OP:
		case MIN_OP:
			res = NA_LINTEGER;
			break;
		case SUM_OP:
			res = 0LL;
			break;
		case PROD_OP:
			res = 1LL;
			break;
	}
	for (i = 0; i < in_len; i++) {
		in_elt = in[i];
		if (in_elt == NA_LINTEGER) {
			if (na_rm)
				continue;
			return NA_LINTEGER;
		}
		switch (op) {
			case MAX_OP:
				if (res == NA_LINTEGER || in_elt > res)
					res = in_elt;
				break;
			case MIN_OP:
				if (res == NA_LINTEGER || in_elt < res)
					res = in_elt;
				break;
			case SUM_OP:
				res = _safe_llint_add(res, in_elt);
				if (res == NA_LINTEGER) {
					warning("Linteger overflow - "
						"use sum(as.numeric(.))");
					return res;
				}
				break;
			case PROD_OP:
				res = _safe_llint_mult(res, in_elt);
				if (res == NA_LINTEGER) {
					warning("Linteger overflow - "
						"use prod(as.numeric(.))");
					return res;
				}
				break;
		}
	}
	return res;
}

SEXP Linteger_Summary(SEXP Generic, SEXP x, SEXP na_rm)
{
	R_xlen_t x_len;
	const long long int *x_elts;
	const char *generic;
	int summary_op;
	SEXP ans;

	x_len = _get_Linteger_length(x);
	x_elts = LINTEGER(x);
	generic = CHAR(STRING_ELT(Generic, 0));

	summary_op = get_summary_op(generic);
	if (summary_op != 0) {
		PROTECT(ans = NEW_LINTEGER(1));
		LINTEGER(ans)[0] = llints_summary(summary_op, x_elts, x_len,
						  LOGICAL(na_rm)[0]);
		UNPROTECT(1);
		return ans;
	}
	if (strcmp(generic, "range") == 0) {
		PROTECT(ans = NEW_LINTEGER(2));
		LINTEGER(ans)[0] = llints_summary(MIN_OP, x_elts, x_len,
						  LOGICAL(na_rm)[0]);
		LINTEGER(ans)[1] = llints_summary(MAX_OP, x_elts, x_len,
						  LOGICAL(na_rm)[0]);
		UNPROTECT(1);
		return ans;
	}

	error("\"%s\": operation not supported on Linteger objects", generic);
	return R_NilValue;
}

