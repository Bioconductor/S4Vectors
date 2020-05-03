\name{DataFrame-utils}

\alias{DataFrame-utils}

\alias{transform.DataFrame}
\alias{transform,DataFrame-method}

\alias{relistToClass,DataFrame-method}
\alias{relistToClass,data.frame-method}

\title{Common operations on DataFrame objects}

\description{
  Common operations on \link{DataFrame} objects.
}

\section{Transforming}{
  In the code snippet below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{}{
      \code{transform(`_data`, ...)}: adds or replaces columns based on
      expressions in \code{\dots}. See \code{\link{transform}}.
    }
  }
}

\section{Splitting}{
  In the code snippet below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{}{\code{split(x, f, drop = FALSE)}:
      Splits \code{x} into a \link[IRanges]{SplitDataFrameList}
      object, according to \code{f}, dropping elements corresponding
      to unrepresented levels if \code{drop} is \code{TRUE}.
    }
  }
}

\author{Michael Lawrence}

\seealso{
  \itemize{
    \item \link{DataFrame} objects.

    \item \link[IRanges]{SplitDataFrameList} objects in the \pkg{IRanges}
          package.

    \item \code{\link{transform}} in the \pkg{base} package.

    \item \code{\link{splitAsList}} in this package (\pkg{S4Vectors}).
  }
}

\examples{
## split
sw <- DataFrame(swiss)
swsplit <- split(sw, sw[["Education"]])

## rbind & cbind
do.call(rbind, as.list(swsplit))
cbind(DataFrame(score), DataFrame(counts))
}

\keyword{utilities}
\keyword{methods}
