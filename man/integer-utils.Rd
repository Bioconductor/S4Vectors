\name{integer-utils}

\alias{isSequence}

\alias{toListOfIntegerVectors}

\alias{orderIntegerPairs}
\alias{matchIntegerPairs}
\alias{selfmatchIntegerPairs}
\alias{duplicatedIntegerPairs}
\alias{orderIntegerQuads}
\alias{matchIntegerQuads}
\alias{selfmatchIntegerQuads}
\alias{duplicatedIntegerQuads}

\title{Some utility functions to operate on integer vectors}

\description{
  Some low-level utility functions to operate on ordinary
  integer vectors.
}

\usage{
isSequence(x, of.length=length(x))

toListOfIntegerVectors(x, sep=",")

## more to come...
}

\arguments{
  \item{x}{
    For \code{isSequence()}: An integer vector.

    For \code{toListOfIntegerVectors()}:
    A character vector where each element is a string containing
    comma-separated integers in decimal representation.
    Alternatively \code{x} can be a list of raw vectors, in which
    case it's treated like if it was \code{sapply(x, rawToChar)}.
  }
  \item{of.length}{
    The expected length of the integer sequence.
  }
  \item{sep}{
    The separator represented as a single-letter string.
  }
}

\details{
  \code{isSequence()} returns \code{TRUE} or \code{FALSE} depending
  on whether \code{x} is identical to \code{seq_len(of.length)} or not.

  \code{toListOfIntegerVectors()} is a fast and memory-efficient
  implementation of
\preformatted{    lapply(strsplit(x, sep, fixed=TRUE), as.integer)}
  but, unlike the above code, it will raise an error if the input
  contains NAs or strings that don't represent integer values.
}

\value{
  A list \emph{parallel} to \code{x} where each list element is an integer
  vector.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item The \code{\link[base]{seq_len}} function in the \pkg{base}
          package.

    \item The \code{\link[base]{strsplit}} function in the \pkg{base}
          package.
  }
}

\examples{
## ---------------------------------------------------------------------
## isSequence()
## ---------------------------------------------------------------------
isSequence(1:5)               # TRUE
isSequence(5:1)               # FALSE
isSequence(0:5)               # FALSE
isSequence(integer(0))        # TRUE
isSequence(1:5, of.length=5)  # TRUE (the expected length)
isSequence(1:5, of.length=6)  # FALSE (not the expected length)

## ---------------------------------------------------------------------
## toListOfIntegerVectors()
## ---------------------------------------------------------------------

x <- c("1116,0,-19",
       " +55291 , 2476,",
       "19184,4269,5659,6470,6721,7469,14601",
       "7778889, 426900, -4833,5659,6470,6721,7096",
       "19184 , -99999")

y <- toListOfIntegerVectors(x)
y

## When it doesn't choke on an NA or string that doesn't represent
## an integer value, toListOfIntegerVectors() is equivalent to
## the function below but is faster and more memory-efficient:
toListOfIntegerVectors2 <- function(x, sep=",")
{
    lapply(strsplit(x, sep, fixed=TRUE), as.integer)
}
y2 <- toListOfIntegerVectors2(x)
stopifnot(identical(y, y2))
}

\keyword{utilities}
