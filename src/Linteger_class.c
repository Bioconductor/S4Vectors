/****************************************************************************
 *                Low-level manipulation of Linteger objects                *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "S4Vectors.h"

#include <ctype.h>  /* for isspace() and isdigit() */

#define	BYTES_PER_LINTEGER	(sizeof(long long int) / sizeof(char))
#define	NEW_LINTEGER(n)		alloc_Linteger("Linteger", (n))
#define	LINTEGER(x)		get_Linteger_dataptr(x)


/****************************************************************************
 * C-level getters and setter.
 */

static SEXP bytes_symbol = NULL;

static SEXP get_Linteger_bytes(SEXP x)
{
	INIT_STATIC_SYMBOL(bytes)
	return GET_SLOT(x, bytes_symbol);
}

static R_xlen_t get_Linteger_length(SEXP x)
{
	return LENGTH(get_Linteger_bytes(x)) / BYTES_PER_LINTEGER;
}

static long long int *get_Linteger_dataptr(SEXP x)
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
static SEXP alloc_Linteger(const char *classname, R_xlen_t length)
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
				warning("out-of-range values coerced to "
					"NAs in coercion to Linteger");
				first_time = 0;
			}
			*to = NA_LINTEGER;
			continue;
		}
		*to = (long long int) from_elt;
	}
	return;
}

static long long int scan_llint(const char *s)
{
	char c, sign;
	long long int val;

	/* Skip leading spaces. */
	while (isspace(c = *(s++))) {};
	if (c == '\0')
		return NA_LINTEGER;
	/* Scan unary +/- sign. */
	if (c == '+' || c == '-') {
		sign = c;
		c = *(s++);
	} else {
		sign = '+';
	}
	if (!isdigit(c))
		return NA_LINTEGER;
	/* Scan digits. */
	val = c - '0';
	while (isdigit(c = *(s++))) {
		val *= 10LL;
		val += c - '0';
	}
	if (sign == '-')
		val = -val;
	if (c == '\0')
		return val;
	/* Scan decimal part. */
	if (c == '.') {
		/* Decimal part is ignored. */
		while (isdigit(c = *(s++))) {};
		if (c == '\0')
			return val;
	}
	/* Skip trailing spaces. */
	if (isspace(c))
		while (isspace(c = *(s++))) {};
	return c == '\0' ? val : NA_LINTEGER;
}

static void from_STRSXP_to_llints(SEXP from, long long int *to)
{
	R_xlen_t n, i;
	int first_time;
	SEXP from_elt;

	n = XLENGTH(from);
	first_time = 1;
	for (i = 0; i < n; i++, to++) {
		from_elt = STRING_ELT(from, i);
		if (from_elt == NA_STRING) {
			*to = NA_LINTEGER;
			continue;
		}
		*to = scan_llint(CHAR(from_elt));
		if (*to != NA_LINTEGER)
			continue;
		if (first_time) {
			warning("NAs introduced by coercion");
			first_time = 0;
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
				warning("out-of-range values coerced to "
					"NAs in coercion to integer");
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
				"(a double is not guaranteed to represent "
				"exactly a long long int > 2^53)");
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

static SEXP make_Linteger_bytes_from_ints(const int *x, R_xlen_t x_len)
{
	SEXP ans;

	PROTECT(ans = NEW_RAW(x_len * BYTES_PER_LINTEGER));
	from_ints_to_llints(x, (long long int *) RAW(ans), x_len);
	UNPROTECT(1);
	return ans;
}

static SEXP make_Linteger_bytes_from_doubles(const double *x, R_xlen_t x_len)
{
	SEXP ans;

	PROTECT(ans = NEW_RAW(x_len * BYTES_PER_LINTEGER));
	from_doubles_to_llints(x, (long long int *) RAW(ans), x_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_bytes_from_LOGICAL(SEXP x)
{
	return make_Linteger_bytes_from_ints(LOGICAL(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_bytes_from_INTEGER(SEXP x)
{
	return make_Linteger_bytes_from_ints(INTEGER(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_bytes_from_NUMERIC(SEXP x)
{
	return make_Linteger_bytes_from_doubles(REAL(x), XLENGTH(x));
}

/* --- .Call ENTRY POINT --- */
SEXP new_Linteger_bytes_from_CHARACTER(SEXP x)
{
	R_xlen_t x_len;
	SEXP ans;

	x_len = XLENGTH(x);
	PROTECT(ans = NEW_RAW(x_len * BYTES_PER_LINTEGER));
	from_STRSXP_to_llints(x, (long long int *) RAW(ans));
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_LOGICAL_from_Linteger_bytes(SEXP bytes)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = XLENGTH(bytes) / BYTES_PER_LINTEGER;
	PROTECT(ans = NEW_LOGICAL(ans_len));
	from_llints_to_bools((const long long int *) RAW(bytes), LOGICAL(ans),
                             ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_INTEGER_from_Linteger_bytes(SEXP bytes)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = XLENGTH(bytes) / BYTES_PER_LINTEGER;
	PROTECT(ans = NEW_INTEGER(ans_len));
	from_llints_to_ints((const long long int *) RAW(bytes), INTEGER(ans),
			    ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NUMERIC_from_Linteger_bytes(SEXP bytes)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = XLENGTH(bytes) / BYTES_PER_LINTEGER;
	PROTECT(ans = NEW_NUMERIC(ans_len));
	from_llints_to_doubles((const long long int *) RAW(bytes), REAL(ans),
			       ans_len);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP new_CHARACTER_from_Linteger_bytes(SEXP bytes)
{
	R_xlen_t ans_len;
	SEXP ans;

	ans_len = XLENGTH(bytes) / BYTES_PER_LINTEGER;
	PROTECT(ans = NEW_CHARACTER(ans_len));
	from_llints_to_STRSXP((const long long int *) RAW(bytes), ans);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * "Ops" group generics
 */

static void print_not_multiple_warning()
{
	warning("longer object length is not a multiple "
		"of shorter object length");
	return;
}

static long long int llint_div(long long int x, long long int y)
{
	if (x == NA_LINTEGER || y == NA_LINTEGER || y == 0LL)
		return NA_LINTEGER;
	return x / y;
}

static long long int llint_mod(long long int x, long long int y)
{
	long long int z;

	if (x == NA_LINTEGER || y == NA_LINTEGER || y == 0LL)
		return NA_LINTEGER;
	z = x % y;
	/* The contortions below are meant to make sure that the result
	   has the sign of 'y'. */
	if (z == 0L || (z > 0LL) == (y > 0LL))
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

typedef long long int (*Op1FunType)(long long int x, long long int y);
typedef double (*Op2FunType)(long long int x, long long int y);

static Op1FunType get_op1_fun(const char *op)
{
	if (strcmp(op, "+") == 0)
		return _safe_llint_add;
	if (strcmp(op, "-") == 0)
		return _safe_llint_subtract;
	if (strcmp(op, "*") == 0)
		return _safe_llint_mult;
	if (strcmp(op, "%/%") == 0)
		return llint_div;
	if (strcmp(op, "%%") == 0)
		return llint_mod;
	return NULL;
}

static Op2FunType get_op2_fun(const char *op)
{
	if (strcmp(op, "/") == 0)
		return llint_div_as_double;
	if (strcmp(op, "^") == 0)
		return llint_pow_as_double;
	return NULL;
}

static void llints_op1(Op1FunType op_fun,
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
		out[k] = op_fun(x[i], y[j]);
	}
	if (_get_ovflow_flag())
		warning("NAs produced by Linteger overflow");
	return;
}

static void llints_op2(Op2FunType op_fun,
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
		out[k] = op_fun(x[i], y[j]);
	}
	return;
}

#define	EQ_OP	1  /* equal to */
#define	NEQ_OP	2  /* not equal to */
#define	LEQ_OP	3  /* less than or equal to */
#define	GEQ_OP	4  /* greater than or equal to */
#define	LT_OP	5  /* less than */
#define	GT_OP	6  /* greater than */

static int get_op3(const char *op)
{
	if (strcmp(op, "==") == 0)
		return EQ_OP;
	if (strcmp(op, "!=") == 0)
		return NEQ_OP;
	if (strcmp(op, "<=") == 0)
		return LEQ_OP;
	if (strcmp(op, ">=") == 0)
		return GEQ_OP;
	if (strcmp(op, "<") == 0)
		return LT_OP;
	if (strcmp(op, ">") == 0)
		return GT_OP;
	return 0;
}

static void llints_op3(int op,
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
SEXP Linteger_Ops(SEXP Generic, SEXP e1_bytes, SEXP e2_bytes)
{
	const long long int *x, *y;
	R_xlen_t x_len, y_len, ans_len;
	const char *op;
	Op1FunType op1_fun;
	Op2FunType op2_fun;
	int op3;
	SEXP ans;

	x = (const long long int *) RAW(e1_bytes);
	y = (const long long int *) RAW(e2_bytes);
	x_len = XLENGTH(e1_bytes) / BYTES_PER_LINTEGER;
	y_len = XLENGTH(e2_bytes) / BYTES_PER_LINTEGER;
	if (x_len == 0 || y_len == 0) {
		ans_len = 0;
	} else if (x_len >= y_len) {
		ans_len = x_len;
		if (x_len % y_len != 0)
			print_not_multiple_warning();
	} else {
		ans_len = y_len;
		if (y_len % x_len != 0)
			print_not_multiple_warning();
	}
	op = CHAR(STRING_ELT(Generic, 0));

	/* "Arith" group */
	op1_fun = get_op1_fun(op);
	if (op1_fun != NULL) {
		PROTECT(ans = NEW_LINTEGER(ans_len));
		llints_op1(op1_fun, x, x_len, y, y_len,
				    LINTEGER(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}
	op2_fun = get_op2_fun(op);
	if (op2_fun != NULL) {
		PROTECT(ans = NEW_NUMERIC(ans_len));
		llints_op2(op2_fun, x, x_len, y, y_len,
				    REAL(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}

	/* "Compare" group */
	op3 = get_op3(op);
	if (op3 != 0) {
		PROTECT(ans = NEW_LOGICAL(ans_len));
		llints_op3(op3, x, x_len, y, y_len,
				LOGICAL(ans), ans_len);
		UNPROTECT(1);
		return ans;
	}

	error("\"%s\": operation not supported on Linteger objects", op);
	return R_NilValue;
}

