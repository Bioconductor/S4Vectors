\name{DataFrame-combine}
\docType{methods}

\alias{DataFrame-combine}

\alias{bindROWS,DataFrame-method}
\alias{c,DataFrame-method}
\alias{cbind.DataFrame}
\alias{cbind,DataFrame-method}
\alias{combineRows,DataFrame,DataFrame-method}
\alias{combineRows,DataFrame,missing-method}
\alias{combineRows,missing,DataFrame-method}
\alias{combineCols,DataFrame,DataFrame-method}
\alias{combineCols,missing,DataFrame-method}
\alias{combineCols,DataFrame,missing-method}
\alias{combineUniqueCols}
\alias{merge,DataFrame,DataFrame-method}
\alias{merge,data.frame,DataFrame-method}
\alias{merge,DataFrame,data.frame-method}

\title{Combine DataFrame objects by rows or columns, or merge them}

\description{
  Various methods are provided to combine \link{DataFrame} objects
  by rows or columns, or to merge them.
}

\details{
  \describe{
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataFrame} by
      combining the rows of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{rbind.data.frame}}, except
      in the handling of row names. If all elements have row names, they
      are concatenated and made unique. Otherwise, the result does not
      have row names. The return value inherits its metadata from
      the first argument.
    }
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataFrame} by
      combining the columns of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{cbind.data.frame}}. The
      return value inherits its metadata from the first argument.
    }
    \item{}{
      \code{combineRows(x, y, ...)}: Creates a new \code{DataFrame} by
      combining the rows of the \code{DataFrame} objects. Unlike \code{rbind},
      \code{combineRows} will handle cases involving differences in the
      column names of its inputs. Missing columns in any individual input
      \code{DataFrame} are filled with \code{NA}s, such that the column names
      of the returned object are a union of the column names of the inputs.
    }
    \item{}{
      \code{combineCols(x, y, ..., use.names=TRUE)}: Creates a new
      \code{DataFrame} by combining the columns of the \code{DataFrame}
      objects. Unlike \code{cbind}, \code{combineCols} will handle cases
      involving differences in the number of the rows among its inputs. 

      If \code{use.names=TRUE}, all objects are expected to have
      non-\code{NULL}, non-duplicated row names. These row names do not have to
      be the same, or even shared, across the input objects. Missing rows in
      any individual input \code{DataFrame} are filled with \code{NA}s, such
      that the row names of the returned object are a union of the row names of
      the inputs.

      If \code{use.names=FALSE}, all inputs are expected to have the same
      number of rows, and this function behaves the same as \code{cbind}. The
      row names of the returned object is set to \code{rownames(x)}. Differences
      in the row names between input objects are ignored.
    }
    \item{}{
      \code{combineUniqueCols(x, y, ..., use.names=TRUE)}: Same as 
      \code{combineCols}, but this function will attempt to collapse multiple
      columns with the same name across the input \code{DataFrame}s into a
      single column in the output. This guarantees that the column names in the
      output \code{DataFrame} are always unique. The function works on any
      rectangular objects for which \code{combineCols()} works; if any of the
      objects do not have column names, no collapsing is performed.

      When \code{use.names=TRUE}, collapsing is only performed if the
      duplicated column has identical values for the shared rows in the
      \code{DataFrame}s involved. Otherwise, the contents of the later
      \code{DataFrame} is simply ignored with a warning. Similarly, if
      \code{use.names=FALSE}, the duplicated columns must be identical for all
      rows in the affected \code{DataFrame}s.
    }
    \item{}{
      \code{merge(x, y, ...)}: Merges two \link{DataFrame} objects
      \code{x} and \code{y}, with arguments in \code{...} being
      the same as those allowed by the base \code{\link{merge}}. It is
      allowed for either \code{x} or \code{y} to be a \code{data.frame}.
    }
  }
}

\author{Michael Lawrence, Hervé Pagès, and Aaron Lun}

\seealso{
  \itemize{
    \item \code{\link{cbind}} and \code{\link{merge}} in the \pkg{base}
          package.

    \item \link{DataFrame-utils} for other common operations on DataFrame
          objects.

    \item \link{DataFrame} objects.

    \item \link{TransposedDataFrame} objects.
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
