/****************************************************************************
 * Safe signed integer arithmetic                                           *
 * ------------------------------                                           *
 * TODO: Extend to support safe double arithmetic when the need arises.     *
 ****************************************************************************/
#include "S4Vectors.h"

#include <limits.h>  /* for INT_MAX, INT_MIN, LLONG_MAX, and LLONG_MIN */

static int ovflow_flag;

void _reset_ovflow_flag()
{
	ovflow_flag = 0;
	return;
}

int _get_ovflow_flag()
{
	return ovflow_flag;
}


/****************************************************************************
 * Safe arithmetic on int values
 *
 * Reference:
 *   The CERT C Secure Coding Standard
 *     Rule INT32-C. Ensure that operations on signed integers do not result
 *     in overflow
 */

int _safe_int_add(int x, int y)
{
	if (x == NA_INTEGER || y == NA_INTEGER)
		return NA_INTEGER;
	if ((y > 0 && x > INT_MAX - y) ||
	    (y < 0 && x < INT_MIN - y))
	{
		ovflow_flag = 1;
		return NA_INTEGER;
	}
	return x + y;
}

int _safe_int_subtract(int x, int y)
{
	if (x == NA_INTEGER || y == NA_INTEGER)
		return NA_INTEGER;
	if ((y < 0 && x > INT_MAX + y) ||
	    (y > 0 && x < INT_MIN + y))
	{
		ovflow_flag = 1;
		return NA_INTEGER;
	}
	return x - y;
}

int _safe_int_mult(int x, int y)
{
	if (x == NA_INTEGER || y == NA_INTEGER)
		return NA_INTEGER;
	if (x > 0) { /* x is positive */
		if (y > 0) { /* x and y are positive */
			if (x > (INT_MAX / y)) {
				ovflow_flag = 1;
				return NA_INTEGER;
			}
		} else { /* x is positive, y is non-positive */
			if (y < (INT_MIN / x)) {
				ovflow_flag = 1;
				return NA_INTEGER;
			}
		}
	} else { /* x is non-positive */
		if (y > 0) { /* x is non-positive, y is positive */
			if (x < (INT_MIN / y)) {
				ovflow_flag = 1;
				return NA_INTEGER;
			}
	  	} else { /* x and y are non-positive */
			if ((x != 0) && (y < (INT_MAX / x))) {
				ovflow_flag = 1;
				return NA_INTEGER;
			}
		}
	}
	return x * y;
}


/****************************************************************************
 * Safe arithmetic on long long int values
 */

long long int _safe_llint_add(long long int x, long long int y)
{
	if (x == NA_LINTEGER || y == NA_LINTEGER)
		return NA_LINTEGER;
	if ((y > 0LL && x > LLONG_MAX - y) ||
	    (y < 0LL && x < LLONG_MIN - y))
	{
		ovflow_flag = 1;
		return NA_LINTEGER;
	}
	return x + y;
}

long long int _safe_llint_subtract(long long int x, long long int y)
{
	if (x == NA_LINTEGER || y == NA_LINTEGER)
		return NA_LINTEGER;
	if ((y < 0LL && x > LLONG_MAX + y) ||
	    (y > 0LL && x < LLONG_MIN + y))
	{
		ovflow_flag = 1;
		return NA_LINTEGER;
	}
	return x - y;
}

long long int _safe_llint_mult(long long int x, long long int y)
{
	if (x == NA_LINTEGER || y == NA_LINTEGER)
		return NA_LINTEGER;
	if (x > 0LL) { /* x is positive */
		if (y > 0LL) { /* x and y are positive */
			if (x > (LLONG_MAX / y)) {
				ovflow_flag = 1;
				return NA_LINTEGER;
			}
		} else { /* x is positive, y is non-positive */
			if (y < (LLONG_MIN / x)) {
				ovflow_flag = 1;
				return NA_LINTEGER;
			}
		}
	} else { /* x is non-positive */
		if (y > 0LL) { /* x is non-positive, y is positive */
			if (x < (LLONG_MIN / y)) {
				ovflow_flag = 1;
				return NA_LINTEGER;
			}
	  	} else { /* x and y are non-positive */
			if ((x != 0LL) && (y < (LLONG_MAX / x))) {
				ovflow_flag = 1;
				return NA_LINTEGER;
			}
		}
	}
	return x * y;
}

