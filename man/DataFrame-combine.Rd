\name{DataFrame-combine}
\docType{methods}

\alias{DataFrame-combine}

\alias{bindROWS,DataFrame-method}
\alias{c,DataFrame-method}
\alias{cbind.DataFrame}
\alias{cbind,DataFrame-method}
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
      \code{merge(x, y, ...)}: Merges two \link{DataFrame} objects
      \code{x} and \code{y}, with arguments in \code{...} being
      the same as those allowed by the base \code{\link{merge}}. It is
      allowed for either \code{x} or \code{y} to be a \code{data.frame}.
    }
  }
}

\author{Michael Lawrence}

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
x1 <- DataFrame(A=1:5, B=letters[1:5], C=11:15)
y1 <- DataFrame(B=c(FALSE, NA, TRUE), C=c(FALSE, NA, TRUE), A=101:103)
rbind(x1, y1)

x2 <- DataFrame(A=Rle(101:103, 3:1), B=Rle(51:52, c(1, 5)))
y2 <- DataFrame(A=runif(2), B=Rle(c("a", "b")))
rbind(x2, y2)

x3 <- DataFrame(key=c(155, 2, 33, 17, 2, 26, 1), aa=1:7)
y3 <- DataFrame(key=1:26, bb=LETTERS)
merge(x3, y3, by="key")
merge(x3, y3, by="key", all.x=TRUE)
}

\keyword{utilities}
\keyword{methods}
