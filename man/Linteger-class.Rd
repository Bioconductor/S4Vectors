\name{Linteger-class}
\docType{class}

\alias{class:Linteger}
\alias{Linteger-class}

\alias{is.Linteger}
\alias{length,Linteger-method}

\alias{coerce,logical,Linteger-method}
\alias{coerce,integer,Linteger-method}
\alias{coerce,numeric,Linteger-method}
\alias{coerce,character,Linteger-method}

\alias{as.Linteger}

\alias{as.logical.Linteger}
\alias{as.logical,Linteger-method}
\alias{as.integer.Linteger}
\alias{as.integer,Linteger-method}
\alias{as.numeric.Linteger}
\alias{as.numeric,Linteger-method}
\alias{as.character.Linteger}
\alias{as.character,Linteger-method}

\alias{Linteger}

\alias{show,Linteger-method}
\alias{showAsCell,Linteger-method}

\alias{c,Linteger-method}

\alias{NA_Linteger_}
\alias{is.na,Linteger-method}

\alias{Ops,Linteger,Linteger-method}
\alias{Ops,Linteger,numeric-method}
\alias{Ops,numeric,Linteger-method}

\alias{Summary,Linteger-method}

\title{Linteger vectors}

\description{
  The Linteger class is a container for storing a vector of \emph{large
  integers} (i.e. long long int values at the C level).
}

\usage{
Linteger(length=0L)
as.Linteger(x)
is.Linteger(x)
}

\arguments{
  \item{length}{
    A non-negative number (i.e. integer, double, or Linteger value)
    specifying the desired length.
  }
  \item{x}{
    Object to be coerced or tested.
  }
}

\details{
  Linteger vectors aim to provide the same functionality as integer vectors
  in base R but their values are stored as long long int values at the C
  level vs int values for integer vectors.
  Note that on Intel platforms long long int values are 64-bit and int values
  32-bit only. Therefore Linteger vectors can hold values in the +/-9.223e18
  range (approximately) vs +/-2.147e9 only for integer vectors.

  NAs are supported and the \code{NA_Linteger_} constant is predefined for
  convenience as \code{as(NA, "Linteger")}.

  Names are not supported for now.

  Coercions from/to logical, integer, double, and character are supported.

  Operations from the \code{\link{Arith}}, \code{\link{Compare}} and
  \code{\link{Summary}} groups are supported.

  More operations coming soon...
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link[base]{integer} vectors in base R.

    \item The \code{\link{Arith}}, \code{\link{Compare}} and
          \code{\link{Summary}} group generics in the \pkg{methods} package.
  }
}

\examples{
## A long long int uses 8 bytes (i.e. 64 bits) in C:
.Machine$sizeof.longlong

## ---------------------------------------------------------------------
## SIMPLE EXAMPLES
## ---------------------------------------------------------------------

Linteger()
Linteger(10)

as.Linteger(3e9)
as.Linteger("3000000000")

x <- as.Linteger(1:10 * 111111111)
x * x
5 * x   # result as vector of doubles (i.e. 'x' coerced to double)
5L * x  # result as Linteger vector (i.e. 5L coerced to Linteger vector)
max(x)
min(x)
range(x)
sum(x)

x <- as.Linteger(1:20)
prod(x)
x <- as.Linteger(1:21)
prod(x)  # result is out of Linteger range (+/-9.223e18)
prod(as.numeric(x))

x <- as.Linteger(1:75000)
sum(x * x * x) == sum(x) * sum(x)

## Note that max(), min() and range() *always* return an Linteger vector
## when called on an Linteger vector, even when the vector is empty:
max(Linteger())  # NA with no warning
min(Linteger())  # NA with no warning

## This differs from how max(), min() and range() behave on an empty
## integer vector:
max(integer())  # -Inf with a warning
min(integer())  #  Inf with a warning

## ---------------------------------------------------------------------
## GOING FROM STRINGS TO INTEGERS
## ---------------------------------------------------------------------

## as.integer() behaves like as.integer(as.double()) on a character
## vector. With the following consequence:
s <- "-2.9999999999999999"
as.integer(s)   # -3

## as.Linteger() converts the string *directly* to Linteger, without
## coercing to double first:
as.Linteger(s)  # decimal part ignored

## ---------------------------------------------------------------------
## GOING FROM DOUBLE-PRECISION VALUES TO INTEGERS AND VICE-VERSA
## ---------------------------------------------------------------------

## Be aware that a double-precision value is not guaranteed to represent
## exactly an integer > 2^53. This can cause some surprises:
2^53 == 2^53 + 1  # TRUE, yep!

## And therefore:
as.Linteger(2^53) == as.Linteger(2^53 + 1)  # also TRUE

## This can be even more disturbing when passing a big literal integer
## value because the R parser will turn it into a double-precision value
## before passing it to as.Linteger():
x1 <- as.Linteger(9007199254740992)  # same as as.Linteger(2^53)
x1
x2 <- as.Linteger(9007199254740993)  # same as as.Linteger(2^53 + 1)
x2
x1 == x2  # still TRUE

## However, no precision is lost if a string literal is used instead:
x1 <- as.Linteger("9007199254740992")
x1
x2 <- as.Linteger("9007199254740993")
x2
x1 == x2  # FALSE
x2 - x1

d1 <- as.double(x1)
d2 <- as.double(x2)  # warning!
d1 == d2  # TRUE

## ---------------------------------------------------------------------
## Linteger IS IMPLEMENTED AS AN S4 CLASS
## ---------------------------------------------------------------------

class(Linteger(10))
typeof(Linteger(10))        # S4
storage.mode(Linteger(10))  # S4
is.vector(Linteger(10))     # FALSE
is.atomic(Linteger(10))     # FALSE

## This means that an Linteger vector cannot go in an ordinary data
## frame:
\dontrun{
data.frame(id=as.Linteger(1:5))  # error!
}
## A DataFrame needs to be used instead:
DataFrame(id=as.Linteger(1:5))

## ---------------------------------------------------------------------
## SANITY CHECKS
## ---------------------------------------------------------------------

x <- as.integer(c(0, 1, -1, -3, NA, -99))
y <- as.integer(c(-6, NA, -4:3, 0, 1999, 6:10, NA))
xx <- as.Linteger(x)
yy <- as.Linteger(y)

## Operations from "Arith" group:
stopifnot(identical(x + y, as.integer(xx + yy)))
stopifnot(identical(as.Linteger(y + x), yy + xx))
stopifnot(identical(x - y, as.integer(xx - yy)))
stopifnot(identical(as.Linteger(y - x), yy - xx))
stopifnot(identical(x * y, as.integer(xx * yy)))
stopifnot(identical(as.Linteger(y * x), yy * xx))
stopifnot(identical(x / y, xx / yy))
stopifnot(identical(y / x, yy / xx))
stopifnot(identical(x \%/\% y, as.integer(xx \%/\% yy)))
stopifnot(identical(as.Linteger(y \%/\% x), yy \%/\% xx))
stopifnot(identical(x \%\% y, as.integer(xx \%\% yy)))
stopifnot(identical(as.Linteger(y \%\% x), yy \%\% xx))
stopifnot(identical(x ^ y, xx ^ yy))
stopifnot(identical(y ^ x, yy ^ xx))

## Operations from "Compare" group:
stopifnot(identical(x == y, xx == yy))
stopifnot(identical(y == x, yy == xx))
stopifnot(identical(x != y, xx != yy))
stopifnot(identical(y != x, yy != xx))
stopifnot(identical(x <= y, xx <= yy))
stopifnot(identical(y <= x, yy <= xx))
stopifnot(identical(x >= y, xx >= yy))
stopifnot(identical(y >= x, yy >= xx))
stopifnot(identical(x < y, xx < yy))
stopifnot(identical(y < x, yy < xx))
stopifnot(identical(x > y, xx > yy))
stopifnot(identical(y > x, yy > xx))

## Operations from "Summary" group:
stopifnot(identical(max(y), as.integer(max(yy))))
stopifnot(identical(max(y, na.rm=TRUE), as.integer(max(yy, na.rm=TRUE))))
stopifnot(identical(min(y), as.integer(min(yy))))
stopifnot(identical(min(y, na.rm=TRUE), as.integer(min(yy, na.rm=TRUE))))
stopifnot(identical(range(y), as.integer(range(yy))))
stopifnot(identical(range(y, na.rm=TRUE), as.integer(range(yy, na.rm=TRUE))))
stopifnot(identical(sum(y), as.integer(sum(yy))))
stopifnot(identical(sum(y, na.rm=TRUE), as.integer(sum(yy, na.rm=TRUE))))
stopifnot(identical(prod(y), as.double(prod(yy))))
stopifnot(identical(prod(y, na.rm=TRUE), as.double(prod(yy, na.rm=TRUE))))
}

\keyword{methods}
\keyword{classes}
