\name{HitsList-class}
\docType{class}

\alias{class:HitsList}
\alias{HitsList-class}
\alias{HitsList}

\alias{class:SelfHitsList}
\alias{SelfHitsList-class}
\alias{SelfHitsList}

\alias{class:SortedByQueryHitsList}
\alias{SortedByQueryHitsList-class}
\alias{SortedByQueryHitsList}

\alias{class:SortedByQuerySelfHitsList}
\alias{SortedByQuerySelfHitsList-class}
\alias{SortedByQuerySelfHitsList}

% accessors
\alias{space}
\alias{space,HitsList-method}
\alias{subjectHits,HitsList-method}
\alias{queryHits,HitsList-method}

% coercion
\alias{coerce,HitsList,SortedByQueryHitsList-method}
\alias{coerce,SortedByQueryHitsList,HitsList-method}
\alias{as.matrix,HitsList-method}
\alias{as.table,HitsList-method}

% splitting
\alias{relistToClass,Hits-method}
\alias{relistToClass,SortedByQueryHits-method}
\alias{splitAsList,SortedByQueryHits,ANY-method}

% other
\alias{t,HitsList-method}

\title{List of Hits objects}
\description{
  The HitsList class stores a set of \link{Hits} objects. It's typically
  used to represent the result of \code{\link[IRanges]{findOverlaps}} on
  two \link[IRanges]{IntegerRangesList} objects.
}

\details{
  Roughly the same set of utilities are provided for HitsList as for
  \link{Hits}:
  
  The \code{as.matrix} method coerces a HitsList object in a
  similar way to \link{Hits}, except a column is prepended that indicates
  which space (or element in the query \link[IRanges]{IntegerRangesList})
  to which the row corresponds.

  The \code{as.table} method flattens or unlists the list, counts the
  number of hits for each query range and outputs the counts as a
  \code{table}, which has the same shape as from a single \link{Hits}
  object.

  To transpose a HitsList object \code{x}, so that the subject
  and query in each space are interchanged, call \code{t(x)}. This
  allows, for example, counting the number of hits for each subject
  element using \code{as.table}.
}

\section{Accessors}{
  \describe{
    \item{\code{queryHits(x)}:}{ Equivalent to
      \code{unname(as.matrix(x)[,1])}.
    }
    \item{\code{subjectHits(x)}:}{ Equivalent to
      \code{unname(as.matrix(x)[,2])}.
    }
    \item{\code{space(x)}:}{ gets the character vector naming the space
      in the query \link[IRanges]{IntegerRangesList} for each hit,
      or \code{NULL} if the query did not have any names.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a HitsList object.
  
  \describe{
    \item{\code{as.matrix(x)}:}{ calls \code{as.matrix} on each
      \link{Hits}, combines them row-wise and offsets the
      indices so that they are aligned with the result of calling
      \code{unlist} on the query and subject.
    }
    \item{\code{as.table(x)}:}{ counts the number of hits for each
      query element in \code{x} and outputs the counts as a \code{table},
      which is aligned with the result of calling \code{unlist}
      on the query.
    }
    \item{\code{t(x)}:}{ Interchange the query and subject in each space
      of \code{x}, returns a transposed HitsList object.}
  }
}

\note{This class is highly experimental. It has not been well tested and
  may disappear at any time.}

\author{ Michael Lawrence }
\seealso{
  \itemize{
    \item \code{\link[IRanges]{findOverlaps}} in the \pkg{IRanges} package,
          which returns a HitsList object when the query and subject are
          \link[IRanges]{IntegerRangesList} objects.
  }
}

\examples{
hits <- Hits(rep(1:20, each=5), 100:1, 20, 100)
hlist <- splitAsList(hits, 1:5)
hlist
hlist[[1]]
hlist[[2]]

## Some sanity checks:

hits1 <- Hits(c(4, 4, 15, 15), c(1, 2, 3, 4), 20, 4)
hits2 <- Hits(c(4, 4, 15, 15), c(1, 2, 3, 4), 20, 4, sort.by.query=TRUE)

fA <- c(1, 1, 2, 2)
hlist1A <- split(hits1, fA)
hlist2A <- split(hits2, fA)
stopifnot(identical(as(hlist1A, "SortedByQueryHitsList"), hlist2A))
stopifnot(identical(hlist1A, as(hlist2A, "HitsList")))

fB <- c(1, 2, 1, 2)
hlist1B <- split(hits1, fB)
hlist2B <- split(hits2, fB)
stopifnot(identical(as(hlist1B, "SortedByQueryHitsList"), hlist2B))
stopifnot(identical(hlist1B, as(hlist2B, "HitsList")))
}

\keyword{methods}
\keyword{classes}
