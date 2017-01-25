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

\alias{NA_Linteger_}
\alias{is.na,Linteger-method}

\alias{Ops,Linteger,Linteger-method}
\alias{Ops,Linteger,numeric-method}
\alias{Ops,numeric,Linteger-method}

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

  Operations from the \code{\link{Arith}} and \code{\link{Compare}} groups
  are supported.

  More operations coming soon...
}

\seealso{
  \itemize{
    \item \link[base]{integer} vectors in base R.

    \item The \code{\link{Arith}} and \code{\link{Compare}} group generics
          in the \pkg{methods} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## SIMPLE EXAMPLES
## ---------------------------------------------------------------------

## A long long int uses 8 bytes (i.e. 64 bits) in C:
.Machine$sizeof.longlong

Linteger()
Linteger(10)

x <- as.Linteger(1:10 * 111111111)
x * x
5 * x   # result as vector of doubles (i.e. 'x' coerced to double)
5L * x  # result as Linteger vector (i.e. 5L coerced to Linteger vector)

## Linteger is an S4 class:
class(Linteger(10))
typeof(Linteger(10))        # S4
storage.mode(Linteger(10))  # S4
is.vector(Linteger(10))     # FALSE
is.atomic(Linteger(10))     # FALSE

as.Linteger(3e9)
as.Linteger("3000000000")

## Be aware that a double-precision value is not guaranteed to represent
## exactly an integer > 2^53. This can cause some surprises:
as.Linteger(2^53)
as.Linteger(2^53 + 1)  # same as 'as.Linteger(2^53)'

## This can be even more disturbing when passing a big literal integer
## value because the R parser will turn it into a double-precision value
## before passing it to as.Linteger():
as.Linteger(9007199254740992)  # as.Linteger(2^53)
as.Linteger(9007199254740993)  # as.Linteger(2^53 + 1)

## No precision is lost when using a string literal instead:
as.Linteger("9007199254740992")
as.Linteger("9007199254740993")

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
}

\keyword{methods}
\keyword{classes}
