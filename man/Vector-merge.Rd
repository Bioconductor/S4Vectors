\name{Vector-merge}

\alias{Vector-merge}

\alias{merge}
\alias{merge,Vector,Vector-method}

\title{Merge vector-like objects}

\description{
  A \code{merge} method for vector-like objects.
}

\usage{
\S4method{merge}{Vector,Vector}(x, y, ..., all=FALSE, all.x=NA, all.y=NA, sort=TRUE)
}

\arguments{
  \item{x, y, ...}{
    Vector-like objects, typically all of the same class and typically not
    list-like objects (even though some list-like objects like
    \link[IRanges]{Ranges} and \link[Biostrings]{DNAStringSet} are
    supported).
    Duplicated elements in each object are removed with a warning.
  }
  \item{all}{
    \code{TRUE} or \code{FALSE}.
    Whether the vector elements in the result should be the union
    (when \code{all=TRUE}) or intersection (when \code{all=FALSE})
    of the vector elements in \code{x}, \code{y}, \code{...}.
  }
  \item{all.x, all.y}{
    To be used only when merging 2 objects (binary merge).
    Both \code{all.x} and \code{all.y} must be single logicals.
    If any of them is \code{NA}, then it's set to the value of \code{all}.
    Setting both of them to \code{TRUE} or both of them to \code{FALSE}
    is equivalent to setting \code{all} to \code{TRUE} or to \code{FALSE},
    respectively (see above).

    If \code{all.x} is \code{TRUE} and \code{all.y} is \code{FALSE} then the
    vector elements in the result will be the unique elements in \code{x}.
    If \code{all.x} is \code{FALSE} and \code{all.y} is \code{TRUE} then the
    vector elements in the result will be the unique elements in \code{y}.
  }
  \item{sort}{
    Whether to sort the merged result.
  }
}

\details{
  This \code{merge} method acts much like \code{\link{merge.data.frame}},
  except for 3 important differences:
  \enumerate{
    \item The matching is based on the vector values, not arbitrary columns
          in a table.
    \item Self merging is a no-op if \code{sort=FALSE} (or object already
          sorted) and if the object has no duplicates.
    \item This \code{merge} method accepts an arbitrary number of vector-like
          objects (n-ary merge).
  }

  If some of the objects to merge are list-like objects not supported by
  the method described here, then the merging is simply done by calling
  \code{base::merge()} on the objects. This might succeed or not...
}

\value{
  A vector-like object of the same class as the input objects (if they
  all have the same class) containing the merged vector values and metadata
  columns.
}

\seealso{
  \itemize{
    \item The \link{Vector} class.

    \item \link{Vector-comparison} for comparing and ordering vector-like
          objects.

    \item \link{Vector-setops} for set operations on vector-like objects.
  }
}

\examples{
library(GenomicRanges)
x <- GRanges(c("chr1:1-1000", "chr2:2000-3000"),
             score=c(0.45, 0.1), a1=c(5L, 7L), a2=c(6, 8))
y <- GRanges(c("chr2:150-151", "chr1:1-10", "chr2:2000-3000"),
             score=c(0.7, 0.82, 0.1), b1=c(0L, 5L, 1L), b2=c(1, -2, 1))
merge(x, y)
merge(x, y, all=TRUE)
merge(x, y, all.x=TRUE)
merge(x, y, all.y=TRUE)

## Shared metadata columns must agree:
mcols(x)$score[2] <- 0.11
#merge(x, y)  # error!

## NAs agree with anything:
mcols(x)$score[2] <- NA
merge(x, y)
}

\keyword{methods}
