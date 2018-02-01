\name{character-utils}

\alias{unstrsplit}
\alias{unstrsplit,list-method}
\alias{unstrsplit,character-method}
\alias{safeExplode}
\alias{svn.time}

\title{Some utility functions to operate on strings}

\description{
  Some low-level string utilities to operate on ordinary
  character vectors. For more advanced string manipulations,
  see the \pkg{Biostrings} package.
}

\usage{
unstrsplit(x, sep="")

## more to come...
}

\arguments{
  \item{x}{
    A list-like object where each list element is a character vector,
    or a character vector (identity).
  }
  \item{sep}{
    A single string containing the separator.
  }
}

\details{
  \code{unstrsplit(x, sep)} is equivalent to (but much faster than)
  \code{sapply(x, paste0, collapse=sep)}. It performs the reverse
  transformation of \code{\link{strsplit}( , fixed=TRUE)}, that is,
  if \code{x} is a character vector with no NAs and \code{sep} a single
  string, then \code{unstrsplit(strsplit(x, split=sep, fixed=TRUE), sep)}
  is identical to \code{x}. A notable exception to this though is when
  \code{strsplit} finds a match at the end of a string, in which case the
  last element of the output (which should normally be an empty string)
  is not returned (see \code{?strsplit} for the details).
}

\value{
  A character vector with one string per list element in \code{x}.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item The \code{\link[base]{strsplit}} function in the \pkg{base}
          package.
  }
}

\examples{
x <- list(A=c("abc", "XY"), B=NULL, C=letters[1:4])
unstrsplit(x)
unstrsplit(x, sep=",")
unstrsplit(x, sep=" => ")

data(islands)
x <- names(islands)
y <- strsplit(x, split=" ", fixed=TRUE)
x2 <- unstrsplit(y, sep=" ")
stopifnot(identical(x, x2))

## But...
names(x) <- x
y <- strsplit(x, split="in", fixed=TRUE)
x2 <- unstrsplit(y, sep="in")
y[x != x2]
## In other words: strsplit() behavior sucks :-/
}

\keyword{utilities}
