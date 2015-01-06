\name{Hits-setops}

\alias{Hits-setops}
\alias{setops-methods}

\alias{union,Hits,Hits-method}
\alias{intersect,Hits,Hits-method}
\alias{setdiff,Hits,Hits-method}

\title{Set operations on Hits objects}

\description{
  Perform set operations on \link{Hits} objects.
}

\usage{
\S4method{union}{Hits,Hits}(x, y)

\S4method{intersect}{Hits,Hits}(x, y)

\S4method{setdiff}{Hits,Hits}(x, y)
}

\arguments{
  \item{x, y}{
    \emph{Compatible} \link{Hits} objects, that is, \link{Hits} objects with
    the same subject and query lengths.
  }
}

\details{
  The \code{union}, \code{intersect}, and \code{setdiff} methods for
  \link{Hits} objects return a \link{Hits} object containing respectively
  the union, intersection, and (asymmetric!) difference of the sets of hits
  in \code{x} and \code{y}.

  These methods only work if \code{x} and \code{y} are \emph{compatible}
  \link{Hits} objects, that is, if they have the same subject and query
  lengths.
}

\value{
  \code{union} returns a \link{Hits} object obtained by appending to \code{x}
  the hits in \code{y} that are not already in \code{x}. The original metadata
  columns are dropped.

  \code{intersect} returns a \link{Hits} object obtained by keeping only
  the hits in \code{x} that are also in \code{y}.

  \code{setdiff} returns a \link{Hits} object obtained by dropping from
  \code{x} the hits that are in \code{y}.

  Both \code{intersect} and \code{setdiff} propagate the metadata columns
  from \code{x}.
}

\author{H. Pages and Michael Lawrence}

\seealso{
  \itemize{
    \item \link{Hits} objects.

    \item \link{Hits-comparison} for comparing and ordering hits.

    \item \code{BiocGenerics::\link[BiocGenerics]{union}},
          \code{BiocGenerics::\link[BiocGenerics]{intersect}},
          and \code{BiocGenerics::\link[BiocGenerics]{setdiff}}
          in the \pkg{BiocGenerics} package for general information about
          these generic functions.
  }
}

\examples{
x <- Hits(c(2, 4, 4, 4, 5, 5), c(3, 1, 3, 2, 3, 2), 6, 3)
x

y <- Hits(c(1, 3, 4, 4, 5, 5, 5), c(3, 3, 2, 1, 2, 1, 3), 6, 3)
y

union(x, y)
union(y, x)  # same hits as in union(x, y), but in different order

intersect(x, y)
intersect(y, x)  # same hits as in intersect(x, y), but in
                 # different order

setdiff(x, y)
setdiff(y, x)
}

\keyword{methods}
