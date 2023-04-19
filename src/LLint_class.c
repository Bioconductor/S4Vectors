/****************************************************************************
 *                Low-level manipulation of LLint objects                *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"

#include <ctype.h>  /* for isspace() and isdigit() */

#define	BYTES_PER_LLINT	(sizeof(long long int) / sizeof(char))
#define	NEW_LLINT(n)	_alloc_LLint("LLint", (n))
#define	LLINT(x)	_get_LLint_dataptr(x)

int _is_LLint(SEXP x)
{
	return isObject(x) &&
	       strcmp(CHAR(STRING_ELT(GET_CLASS(x), 0)), "LLint") == 0;
}

/* --- .Call ENTRY POINT --- */
SEXP make_RAW_from_NA_LLINT()
{
	SEXP ans;

	PROTECT(ans = NEW_RAW(BYTES_PER_LLINT));
	*((long long int *) RAW(ans)) = NA_LLINT;
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * sscan_llint()
 *
 * If 'maxparse' is < 0, parsing stops at the "deal-breaker character" i.e.
 * at the first character in the string pointed by 's' that is not considered
 * part of the representation of the long long integer (this includes the '\0'
 * character found at the end of a C string).
 * The caller can set the maximum number of characters to parse by setting
 * 'maxparse' to a non-negative value, in which case parsing stops when
 * reaching the "deal-breaker character" or when this maximum has been
 * reached, whichever occurs first.
 * After parsing stops, sscan_llint() returns the number of characters that
 * got parsed, counting the "deal-breaker character" that was reached. Note
 * that when setting 'maxparse' to a negative value, this number will always
 * be >= 1 because sscan_llint() always parses at least the first character
 * in the string (i.e. s[0]). Also in this case it's the responsibility of
 * the caller to make sure that the string contains a "deal-breaker character".
 * Note that caller can always safely access the last parsed character with
 * s[n - 1] (except when 'maxparse' is set to 0).
 * On return, 'val' will contain one of the following:
 *   (a) the value of the parsed long long integer;
 *   (b) NA_LLINT.
 * (b) occurs in one of the 2 following situations:
 *   (b1) no integer could be parsed i.e. parsing stopped before reaching
 *        any digit;
 *   (b2) an integer was successfully parsed but was too big to be
 *        represented as a long long int (overflow).
 * It's the responsibility of the caller to reset the global overflow flag
 * (with _reset_ovflow_flag()) before calling sscan_llint() so that
 * _get_ovflow_flag() can be called after sscan_llint() returns as a reliable
 * mean to differentiate between (b1) and (b2).
 */

int sscan_llint(const char *s, int maxparse, long long int *val, int parse_dot)
{
	int n;
	char c, sign;

	n = 0;
	*val = NA_LLINT;
	/* Skip leading spaces. */
	do {
		if (maxparse >= 0 && n >= maxparse)
			return n;
	} while (isspace(c = s[n++]));
	/* Scan unary +/- sign. */
	if (c == '+' || c == '-') {
		sign = c;
		if (maxparse >= 0 && n >= maxparse)
			return n;
		c = s[n++];
	} else {
		sign = '+';
	}
	if (isdigit(c)) {
		/* Scan digits. */
		*val = 0;
		do {
			*val = _safe_llint_mult(*val, 10LL);
			*val = _safe_llint_add(*val, (long long int) c - '0');
			if (maxparse >= 0 && n >= maxparse)
				goto bailout;
		} while (isdigit(c = s[n++]));
		if (c == '.' && parse_dot) {
			/* Parse decimal part but ignore it. */
			do {
				if (maxparse >= 0 && n >= maxparse)
					goto bailout;
			} while (isdigit(c = s[n++]));
		}
		if (isspace(c)) {
			/* Skip trailing spaces. */
			do {
				if (maxparse >= 0 && n >= maxparse)
					goto bailout;
			} while (isspace(c = s[n++]));
		}
		bailout:
		if (sign == '-')
			*val = -(*val);
	}
	return n;
}


/****************************************************************************
 * C-level getters and setter.
 */

static SEXP bytes_symbol = NULL;

static SEXP get_LLint_bytes(SEXP x)
{
	INIT_STATIC_SYMBOL(bytes)
	return GET_SLOT(x, bytes_symbol);
}

R_xlen_t _get_LLint_length(SEXP x)
{
	return XLENGTH(get_LLint_bytes(x)) / BYTES_PER_LLINT;
}

long long int *_get_LLint_dataptr(SEXP x)
{
	return (long long int *) RAW(get_LLint_bytes(x));
}

static void set_LLint_bytes(SEXP x, SEXP value)
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

static SEXP new_LLint_from_bytes(const char *classname, SEXP bytes)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_LLint_bytes(ans, bytes);
	UNPROTECT(2);
	return ans;
}

/* Allocation WITHOUT initialization. */
SEXP _alloc_LLint(const char *classname, R_xlen_t length)
{
	SEXP bytes, ans;

	PROTECT(bytes = NEW_RAW(length * BYTES_PER_LLINT));
	PROTECT(ans = new_LLint_from_bytes(classname, bytes));
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
			*to = NA_LLINT;
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
			*to = NA_LLINT;
			continue;
		}
		if (from_elt > (double) LLONG_MAX ||
		    from_elt < (double) -LLONG_MAX)
		{
			if (first_time) {
				warning("out-of-range values coerced to NAs "
					"in coercion to LLint");
				first_time = 0;
			}
			*to = NA_LLINT;
			continue;
		}
		*to = (long long int) from_elt;
	}
	return;
}

static void from_STRSXP_to_llints(SEXP from, long long int *to)
{
	R_xlen_t from_len, i;
	int first_time1, first_time2, n;
	SEXP from_elt;
	const char *s;

	from_len = XLENGTH(from);
	first_time1 = first_time2 = 1;
	for (i = 0; i < from_len; i++, to++) {
		from_elt = STRING_ELT(from, i);
		if (from_elt == NA_STRING) {
			*to = NA_LLINT;
			continue;
		}
		s = CHAR(from_elt);
		_reset_ovflow_flag();
		n = sscan_llint(s, -1, to, 1);
		if (s[n - 1] == '\0') {
			if (*to != NA_LLINT)
				continue;
			if (_get_ovflow_flag()) {
				/* syntactically correct number but overflow */
				if (first_time1) {
					warning("out-of-range values coerced "
						"to NAs in coercion to LLint");
					first_time1 = 0;
				}
				continue;
			}
		}
		/* syntactically incorrect number */
		if (first_time2) {
			warning("syntactically incorrect numbers "
				"coerced to NAs in coercion to LLint");
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
		if (from_elt == NA_LLINT) {
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
		if (from_elt == NA_LLINT) {
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
		if (from_elt == NA_LLINT) {
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
	int ret;
	SEXP to_elt;

	n = XLENGTH(to);
	for (i = 0; i < n; i++, from++) {
		from_elt = *from;
		if (from_elt == NA_LLINT) {
			SET_STRING_ELT(to, i, NA_STRING);
			continue;
		}
		/* Even though 'val_buf' should be big enough, we still check
		   for a potentially truncated output, just to be safe. */
		ret = snprintf(val_buf, sizeof(val_buf), "%lld", from_elt);
		if (ret >= sizeof(val_buf))
			error("S4Vectors internal error in "
			      "from_llints_to_STRSXP(): "
			      "output of snprintf() got truncated");
		PROTECT(to_elt = mkChar(val_buf));
		SET_STRING_ELT(to, i, to_elt);
		UNPROTECT(1);
	}
	return;
}


/****************************************************************************
 * Coercion.
 */

static SEXP new_LLint_from_ints(const int *x, R_xlen_t x_len)
{
	SEXP ans;

	PROTECT(ans = NEW_LLINT(x_len));
	from_ints_to_llints(x, LLINT(ans), x_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_LLint_from_LOGICAL(SEXP x)
{
	return new_LLint_from_ints(LOGICAL(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_LLint_from_INTEGER(SEXP x)
{
	return new_LLint_from_ints(INTEGER(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_LLint_from_NUMERIC(SEXP x)
{
	R_xlen_t x_len;
	SEXP ans;

	x_len = XLENGTH(x);
	PROTECT(ans = NEW_LLINT(x_len));
	from_doubles_to_llints(REAL(x), LLINT(ans), x_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_LLint_from_CHARACTER(SEXP x)
{
	R_xlen_t x_len;
	SEXP ans;

	x_len = XLENGTH(x);
	PROTECT(ans = NEW_LLINT(x_len));
	from_STRSXP_to_llints(x, LLINT(ans));
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_LOGICAL_from_LLint(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_LLint_length(x);
	PROTECT(ans = NEW_LOGICAL(ans_len));
	from_llints_to_bools(LLINT(x), LOGICAL(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_INTEGER_from_LLint(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_LLint_length(x);
	PROTECT(ans = NEW_INTEGER(ans_len));
	from_llints_to_ints(LLINT(x), INTEGER(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NUMERIC_from_LLint(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_LLint_length(x);
	PROTECT(ans = NEW_NUMERIC(ans_len));
	from_llints_to_doubles(LLINT(x), REAL(ans), ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_CHARACTER_from_LLint(SEXP x)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = _get_LLint_length(x);
	PROTECT(ans = NEW_CHARACTER(ans_len));
	from_llints_to_STRSXP(LLINT(x), ans);
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

	if (x == NA_LLINT || y == NA_LLINT || y == 0LL)
		return NA_LLINT;
	z = x / y;
	if (x == 0LL || (x > 0LL) == (y > 0LL) || y * z == x)
		return z;
	return z - 1LL;
}

static long long int llint_mod(long long int x, long long int y)
{
	long long int z;

	if (x == NA_LLINT || y == NA_LLINT || y == 0LL)
		return NA_LLINT;
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
	if (x == NA_LLINT || y == NA_LLINT)
		return NA_REAL;
	return (double) x / (double) y;
}

static double llint_pow_as_double(long long int x, long long int y)
{
	if (x == 1LL || y == 0LL)
		return 1.0;
	if (x == NA_LLINT || y == NA_LLINT)
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
		warning("NAs produced by LLint overflow");
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
		if (x_elt == NA_LLINT || y_elt == NA_LLINT) {
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
SEXP LLint_Ops(SEXP Generic, SEXP e1, SEXP e2)
{
	R_xlen_t e1_len, e2_len, ans_len;
	const long long int *e1_elts, *e2_elts;
	const char *generic;
	Arith1FunType arith1_fun;
	Arith2FunType arith2_fun;
	int compare_op;
	SEXP ans;

	e1_len = _get_LLint_length(e1);
	e2_len = _get_LLint_length(e2);
	ans_len = compute_ans_length(e1_len, e2_len);
	e1_elts = LLINT(e1);
	e2_elts = LLINT(e2);
	generic = CHAR(STRING_ELT(Generic, 0));

	/* Operations from "Arith" group */
	arith1_fun = get_arith1_fun(generic);
	if (arith1_fun != NULL) {
		PROTECT(ans = NEW_LLINT(ans_len));
		llints_arith1(arith1_fun, e1_elts, e1_len, e2_elts, e2_len,
					  LLINT(ans), ans_len);
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

	error("\"%s\": operation not supported on LLint objects", generic);
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
			res = NA_LLINT;
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
		if (in_elt == NA_LLINT) {
			if (na_rm)
				continue;
			return NA_LLINT;
		}
		switch (op) {
			case MAX_OP:
				if (res == NA_LLINT || in_elt > res)
					res = in_elt;
				break;
			case MIN_OP:
				if (res == NA_LLINT || in_elt < res)
					res = in_elt;
				break;
			case SUM_OP:
				res = _safe_llint_add(res, in_elt);
				if (res == NA_LLINT) {
					warning("LLint overflow - "
						"use sum(as.numeric(.))");
					return res;
				}
				break;
			case PROD_OP:
				res = _safe_llint_mult(res, in_elt);
				if (res == NA_LLINT) {
					warning("LLint overflow - "
						"use prod(as.numeric(.))");
					return res;
				}
				break;
		}
	}
	return res;
}

SEXP LLint_Summary(SEXP Generic, SEXP x, SEXP na_rm)
{
	R_xlen_t x_len;
	const long long int *x_elts;
	const char *generic;
	int summary_op;
	SEXP ans;

	x_len = _get_LLint_length(x);
	x_elts = LLINT(x);
	generic = CHAR(STRING_ELT(Generic, 0));

	summary_op = get_summary_op(generic);
	if (summary_op != 0) {
		PROTECT(ans = NEW_LLINT(1));
		LLINT(ans)[0] = llints_summary(summary_op, x_elts, x_len,
					       LOGICAL(na_rm)[0]);
		UNPROTECT(1);
		return ans;
	}
	if (strcmp(generic, "range") == 0) {
		PROTECT(ans = NEW_LLINT(2));
		LLINT(ans)[0] = llints_summary(MIN_OP, x_elts, x_len,
					       LOGICAL(na_rm)[0]);
		LLINT(ans)[1] = llints_summary(MAX_OP, x_elts, x_len,
					       LOGICAL(na_rm)[0]);
		UNPROTECT(1);
		return ans;
	}

	error("\"%s\": operation not supported on LLint objects", generic);
	return R_NilValue;
}

