#include "S4Vectors.h"

// R_XLEN_T_MAX is 2^52
// LLONG_MAX    is 2^63-1

/* Unlike base::sum() which can overflow (and return NA_integer_) on a long
   logical vector, logical_sum() never overflows. It returns a double if the
   result cannot be represented as an int. Note that this is what length()
   does. */
SEXP logical_sum(SEXP x, SEXP na_rm)
{
	R_xlen_t x_len, sum, i;
	const int *x_dataptr;
	int na_rm0, x_elt;
	SEXP ans;

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
	if (sum <= INT_MAX)
		ans = ScalarInteger((int) sum);
	else
		ans = ScalarReal((double) sum);
	return ans;
}

/*
   Storing logical vectors in int arrays like R does is such a waste of
   memory!

   Playing around with logical vectors stored in char arrays:

   - Very common operations like sum(x < 0.9) (this is probably the primary
     use case for sum()!) would require 4x less memory. This is particularly
     relevant if 'x' is a long vector (e.g. length(x) = 3e9) where R currently
     spends a significant amount of time allocating memory (e.g. 12Gb) to store
     the temporary logical vector.

   - As a bonus logical2_sum() below is slightly faster (about 7%) than
     base::sum() on my laptop (Ubuntu Dell LATITUDE E6440 with 4Gb of RAM
     and running 64-bit Ubuntu 14.04.5 LTS). This might be platform specific
     though...

   To compare base::sum() vs logical_sum() vs logical2_sum():

     library(S4Vectors)
     sum1 <- function(x, na.rm=FALSE) .Call("logical_sum", x, na.rm, PACKAGE="S4Vectors")
     sum2 <- function(x, na.rm=FALSE) .Call("logical2_sum", x, na.rm, PACKAGE="S4Vectors")

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
	SEXP ans;

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
	if (sum <= INT_MAX)
		ans = ScalarInteger((int) sum);
	else
		ans = ScalarReal((double) sum);
	return ans;
}

