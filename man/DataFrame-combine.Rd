\name{DataFrame-combine}
\docType{methods}

\alias{DataFrame-combine}

\alias{bindROWS,DataFrame-method}
\alias{c,DataFrame-method}
\alias{cbind.DataFrame}
\alias{cbind,DataFrame-method}
\alias{combineRows,DataFrame-method}
\alias{combineCols,DataFrame-method}
\alias{merge,DataFrame,DataFrame-method}
\alias{merge,data.frame,DataFrame-method}
\alias{merge,DataFrame,data.frame-method}

\title{Combine DataFrame objects along their rows or columns, or merge them}

\description{
  Various methods are provided to combine \link{DataFrame} objects
  along their rows or columns, or to merge them.
}

\details{
  In the code snippets below, all the input objects are expected to be
  \link{DataFrame} objects.
  \describe{
    \item{\code{rbind(...)}:}{ Creates a new \link{DataFrame} object by
      aggregating the rows of the input objects.
      Very similar to \code{\link{rbind.data.frame}()}, except
      in the handling of row names. If all elements have row names, they
      are concatenated and made unique. Otherwise, the result does not
      have row names.
      The returned \link{DataFrame} object inherits its metadata and
      metadata columns from the first input object.
    }
    \item{\code{cbind(...)}:}{ Creates a new \link{DataFrame} object by
      aggregating the columns of the input objects.
      Very similar to \code{\link{cbind.data.frame}()}.
      The returned \link{DataFrame} object inherits its metadata from
      the first input object.
      The metadata columns of the returned \link{DataFrame} object are
      obtained by combining the metadata columns of the input object with
      \code{combineRows()}.
    }
    \item{\code{combineRows(x, ...)}:}{ \code{combineRows()} is a generic
      function documented in the man page for \link{RectangularData}
      objects (see \code{?\link{RectangularData}}).
      The method for \link{DataFrame} objects behaves as documented in
      that man page.
    }
    \item{\code{combineCols(x, ..., use.names=TRUE)}:}{ \code{combineCols()}
      is a generic function documented in the man page for
      \link{RectangularData} objects (see \code{?\link{RectangularData}}).
      The method for \link{DataFrame} objects behaves as documented in
      that man page.
    }
    \item{\code{combineUniqueCols(x, ..., use.names=TRUE)}:}{ This function
      is documented in the man page for \link{RectangularData} objects
      (see \code{?\link{RectangularData}}).
    }
    \item{\code{merge(x, y, ...)}:}{ Merges two \link{DataFrame} objects
      \code{x} and \code{y}, with arguments in \code{...} being
      the same as those allowed by the base \code{\link{merge}()}. It is
      allowed for either \code{x} or \code{y} to be a \code{data.frame}.
    }
  }
}

\author{Michael Lawrence, Hervé Pagès, and Aaron Lun}

\seealso{
  \itemize{
    \item \link{DataFrame-utils} for other common operations on DataFrame
          objects.

    \item \link{DataFrame} objects.

    \item \link{TransposedDataFrame} objects.

    \item \link{RectangularData} objects.

    \item \code{\link{cbind}} and \code{\link{merge}} in the \pkg{base}
          package.
  }
}

\examples{
## ---------------------------------------------------------------------
## rbind()
## ---------------------------------------------------------------------

x1 <- DataFrame(A=1:5, B=letters[1:5], C=11:15)
y1 <- DataFrame(B=c(FALSE, NA, TRUE), C=c(FALSE, NA, TRUE), A=101:103)
rbind(x1, y1)

x2 <- DataFrame(A=Rle(101:103, 3:1), B=Rle(51:52, c(1, 5)))
y2 <- DataFrame(A=runif(2), B=Rle(c("a", "b")))
rbind(x2, y2)

## ---------------------------------------------------------------------
## combineRows()
## ---------------------------------------------------------------------

y3 <- DataFrame(A=runif(2))
combineRows(x2, y3)

y4 <- DataFrame(B=Rle(c("a", "b")), C=runif(2))
combineRows(x2, y4)
combineRows(y4, x2)
combineRows(y4, x2, DataFrame(D=letters[1:3], B=301:303))

## ---------------------------------------------------------------------
## combineCols()
## ---------------------------------------------------------------------

X <- DataFrame(x=1)
Y <- DataFrame(y="A")
Z <- DataFrame(z=TRUE)

combineCols(X, Y, Z, use.names=FALSE)

Y <- DataFrame(y=LETTERS[1:2])
rownames(X) <- "foo"
rownames(Y) <- c("foo", "bar")
rownames(Z) <- "bar"

combineCols(X, Y, Z)

## ---------------------------------------------------------------------
## combineUniqueCols()
## ---------------------------------------------------------------------

X <- DataFrame(x=1)
Y <- DataFrame(y=LETTERS[1:2], dup=1:2)
Z <- DataFrame(z=TRUE, dup=2L)

rownames(X) <- "foo"
rownames(Y) <- c("foo", "bar")
rownames(Z) <- "bar"

combineUniqueCols(X, Y, Z)

Z$dup <- 3
combineUniqueCols(X, Y, Z)

## ---------------------------------------------------------------------
## merge()
## ---------------------------------------------------------------------

x6 <- DataFrame(key=c(155, 2, 33, 17, 2, 26, 1), aa=1:7)
y6 <- DataFrame(key=1:26, bb=LETTERS)
merge(x6, y6, by="key")
merge(x6, y6, by="key", all.x=TRUE)
}

\keyword{utilities}
\keyword{methods}
