#include "S4Vectors.h"

// R_XLEN_T_MAX is 2^52
// LLONG_MAX    is 2^63-1

static SEXP sum_as_SEXP(R_xlen_t sum)
{
	/* If 'sum' is <= INT_MAX, we return it as an integer vector of length
	   1. Otherwise, as a double vector of length 1. Since it's guaranteed
	   to be <= R_XLEN_T_MAX, then it can always be exactly represented as
	   a double. */
	return sum <= INT_MAX ? ScalarInteger((int) sum) :
				ScalarReal((double) sum);
}

/*
  Unlike base::sum() which can overflow (and return NA_integer_) on a long
  logical vector, logical_sum() never overflows. It returns a double if the
  result cannot be represented as an int (which is what length() does).
  Note that logical_sum() is slightly faster than base::sum():

            length(x)  base::sum()  logical_sum()  speedup
            ---------  -----------  -------------  -------
    rhino3:
               1e8         83 ms          74 ms       12%
               1e9        0.84 s         0.75 s       12%
               3e9        2.52 s         2.35 s       13%  <-- long vector
    malbec1:
               1e8         93 ms          74 ms       26%
               1e9        0.92 s         0.75 s       23%
    veracruz1:
               1e8        121 ms          93 ms       30%
               1e9        1.27 s         1.01 s       26%

  - rhino3: Linux server, Intel(R) Xeon(R) CPU E5-2697 v3 @ 2.60GHz (56
    cores), 384 GB of RAM, with Ubuntu Ubuntu 14.04.3 LTS, gcc 4.8.4,
    R 3.4.0 installed from source (default compiler options and flags).
  - malbec1: HP ProLiant DL360 Gen9 server, Intel(R) Xeon(R) CPU E5-2640
    v4 @ 2.40GHz (20 cores), 32 GB of RAM, with Ubuntu 16.04.2 LTS, gcc 5.4.0,
    R 3.4.0 installed from source (default compiler options and flags).
  - veracruz1: virtualized Mac Pro Server at Mac Stadium
    (https://www.macstadium.com), Quad-Core Intel Xeon E5 3.7 GHz,
    32 GB of RAM, with El Capitan, clang 4.0.0, R 3.4.0 (CRAN binary).

  I did not time this on Windows.
*/
SEXP logical_sum(SEXP x, SEXP na_rm)
{
	R_xlen_t x_len, sum, i;
	const int *x_dataptr;
	int na_rm0, x_elt;

	x_len = XLENGTH(x);
	x_dataptr = LOGICAL(x);
	na_rm0 = LOGICAL(na_rm)[0];
	sum = 0;
	for (i = 0; i < x_len; i++) {
		x_elt = x_dataptr[i];
		if (x_elt == NA_LOGICAL) {
			if (na_rm0)
				continue;
			return ScalarInteger(NA_INTEGER);
		}
		/* IIRC some comments in the R source code seem to suggest
		   that TRUEs are not guaranteed to be represented by ones
		   at the C level. */
		if (x_elt)
			sum++;
	}
	return sum_as_SEXP(sum);
}

/*
  Playing around with logical vectors stored in char arrays.

  Storing logical vectors in int arrays like R does is such a waste of memory!
  By using chars instead of ints very common operations like sum(x < 0.9) (this
  is probably the primary use case for sum()!) would require 4x less memory.
  This is particularly relevant if 'x' is a long vector (e.g. length(x) = 3e9)
  where R currently spends a significant amount of time allocating memory (e.g.
  12Gb) to store the temporary logical vector.

  Unfortunately walking on a char array is significantly faster than
  base::sum() on Linux but not on Mac where it's more than 3x slower:

            length(x)  base::sum()  logical2_sum()  speedup
            ---------  -----------  --------------  -------
    rhino3:
               1e8         83 ms           66 ms       25%
               1e9        0.84 s          0.66 s       27%
               3e9        2.52 s          1.93 s       30%  <-- long vector
    malbec1:
               1e8         93 ms           64 ms       45%
               1e9        0.92 s          0.63 s       46%
    veracruz1:
               1e8        121 ms          398 ms    not so good!
               1e9        1.27 s          4.05 s    not so good!

  To compare base::sum() vs logical_sum() vs logical2_sum():

    library(S4Vectors)
    sum1 <- function(x, na.rm=FALSE)
            .Call("logical_sum", x, na.rm, PACKAGE="S4Vectors")
    sum2 <- function(x, na.rm=FALSE)
            .Call("logical2_sum", x, na.rm, PACKAGE="S4Vectors")

    x <- as.logical(sample(2L, 1e8, replace=TRUE) - 1L)
    x2 <- as.raw(x)

    ## Correctness
    res0 <- sum(x, na.rm=FALSE)
    res1 <- sum1(x, na.rm=FALSE)
    res2 <- sum2(x2, na.rm=FALSE)
    stopifnot(identical(res0, res1))
    stopifnot(identical(res0, res2))

    ## Speed
    system.time(replicate(20, sum(x, na.rm=FALSE)))
    system.time(replicate(20, sum1(x, na.rm=FALSE)))
    system.time(replicate(20, sum2(x2, na.rm=FALSE)))

*/
#define	NA_LOGICAL2	127  /*	Arbitrary choice. Could be set to anything
				but 0 or 1. */
#define	LOGICAL2(x)	((char *) RAW(x))
SEXP logical2_sum(SEXP x, SEXP na_rm)
{
	R_xlen_t x_len, sum, i;
	const char *x_dataptr;
	int na_rm0;
	char x_elt;

	x_len = XLENGTH(x);
	x_dataptr = LOGICAL2(x);
	na_rm0 = LOGICAL(na_rm)[0];
	sum = 0;
	for (i = 0; i < x_len; i++) {
		x_elt = x_dataptr[i];
		if (x_elt == NA_LOGICAL2) {
			if (na_rm0)
				continue;
			return ScalarInteger(NA_INTEGER);
		}
		if (x_elt)
			sum++;
	}
	return sum_as_SEXP(sum);
}

