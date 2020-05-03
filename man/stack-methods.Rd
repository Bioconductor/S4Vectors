\name{stack-methods}

\alias{stack-methods}

\alias{stack,List-method}
\alias{stack,matrix-method}

\alias{mstack}
\alias{mstack,Vector-method}
\alias{mstack,vector-method}
\alias{mstack,DataFrame-method}

\title{Stack objects}

\description{
  The \pkg{S4Vectors} package defines \code{\link[utils]{stack}} methods
  for \link{List} and \code{matrix} objects.

  It also introduces \code{mstack()}, a variant of \code{\link{stack}}
  where the list is taken as the list of arguments in \code{...}.
}

\usage{
\S4method{stack}{List}(x, index.var="name", value.var="value", name.var=NULL)

\S4method{stack}{matrix}(x, row.var=names(dimnames(x))[1L],
      col.var=names(dimnames(x))[2L],
      value.var="value")

mstack(..., .index.var="name")
}

\arguments{
  \item{x}{
    A \link{List} derivative (for the \code{stack} method for \link{List}
    objects), or a \code{matrix} (for the \code{stack} method for \code{matrix}
    objects).
  }
  \item{index.var, .index.var}{
    A single string specifying the column name for the index (source name)
    column.
  }
  \item{value.var}{
    A single string specifying the column name for the values.
  }
  \item{name.var}{
    TODO
  }
  \item{row.var, col.var}{
    TODO
  }
  \item{...}{
    The objects to stack. Each of them should be a \link{Vector}
    or \code{vector} (mixing the two will not work).
  }
}

\details{
  As with \code{\link[utils]{stack}} on a \code{list}, \code{stack} on a
  \link{List} derivative constructs a \link{DataFrame} with two columns:
  one for the unlisted values, the other indicating the name of the
  element from which each value was obtained. \code{index.var}
  specifies the column name for the index (source name) column and
  \code{value.var} specifies the column name for the values.

  [TODO: Document \code{stack()} method for \code{matrix} objects.]
}

\seealso{
  \itemize{
    \item \code{\link[utils]{stack}} in the \pkg{utils} package.

    \item \link{List} and \link{DataFrame} objects.
  }
}

\examples{
library(IRanges)
starts <- IntegerList(c(1, 5), c(2, 8))
ends <- IntegerList(c(3, 8), c(5, 9))
rgl <- IRangesList(start=starts, end=ends)
rangeDataFrame <- stack(rgl, "space", "ranges")
}

\keyword{methods}
\keyword{utilities}
