\name{List-comparison}

\alias{List-comparison}

\alias{pcompare,list,List-method}
\alias{pcompare,List,list-method}
\alias{pcompare,List,List-method}
\alias{pcompareRecursively}
\alias{pcompareRecursively,list-method}
\alias{pcompareRecursively,List-method}

\alias{==,list,List-method}
\alias{==,List,list-method}
\alias{==,List,List-method}

\alias{<=,list,List-method}
\alias{<=,List,list-method}
\alias{<=,List,List-method}

\alias{match,list,List-method}
\alias{match,List,list-method}
\alias{match,List,List-method}
\alias{match,List,vector-method}
\alias{match,List,Vector-method}

\alias{duplicated,List-method}
\alias{unique,List-method}
\alias{unique,SimpleList-method}

\alias{sort.List}
\alias{sort,List-method}
\alias{order,List-method}
\alias{rank,List-method}
\alias{is.unsorted,List-method}

\title{Compare and order list-like objects}

\description{
  Methods for comparing and ordering \link{List} objects and ordinary
  \link[base]{list}s by recursing into their elements.
}

\usage{
## Element-wise (aka "parallel") comparison of list-like objects
## -------------------------------------------------------------

\S4method{pcompare}{list,List}(x, y)
\S4method{pcompare}{List,list}(x, y)
\S4method{pcompare}{List,List}(x, y)

\S4method{==}{list,List}(e1, e2)
\S4method{==}{List,list}(e1, e2)
\S4method{==}{List,List}(e1, e2)

\S4method{<=}{list,List}(e1, e2)
\S4method{<=}{List,list}(e1, e2)
\S4method{<=}{List,List}(e1, e2)

## match(), duplicated(), and unique()
## -----------------------------------

\S4method{match}{list,List}(x, table, nomatch = NA_integer_,
    incomparables = NULL, ...)
\S4method{match}{List,list}(x, table, nomatch = NA_integer_,
    incomparables = NULL, ...)
\S4method{match}{List,List}(x, table, nomatch = NA_integer_,
    incomparables = NULL, ...)
\S4method{match}{List,vector}(x, table, nomatch = NA_integer_,
    incomparables = NULL, ...)
\S4method{match}{List,Vector}(x, table, nomatch = NA_integer_,
    incomparables = NULL, ...)

\S4method{duplicated}{List}(x, incomparables=FALSE, fromLast=FALSE, ...)
\S4method{unique}{List}(x, incomparables=FALSE, ...)

## order() and related methods
## ---------------------------

\S4method{order}{List}(..., na.last = TRUE, decreasing = FALSE,
    method = c("auto", "shell", "radix"))
\S4method{sort}{List}(x, decreasing=FALSE, na.last=NA, by)
\S4method{rank}{List}(x, na.last = TRUE,
    ties.method = c("average", "first", "random", "max", "min"))
\S4method{is.unsorted}{List}(x, na.rm = FALSE, strictly = FALSE)

## Low-level helper for recursive comparison
## -----------------------------------------

pcompareRecursively(x)
}

\arguments{
  \item{x, y, e1, e2, table}{
    List-like objects.
  }
  \item{nomatch}{
    See \code{?base::\link[base]{match}}.
  }
  \item{incomparables}{
    See \code{?base::\link[base]{duplicated}},
    \code{?base::\link[base]{unique}}, and
    \code{?base::\link[base]{match}}.
  }
  \item{fromLast}{
    See \code{?base::\link[base]{duplicated}}.
  }
  \item{ties.method}{
    See \code{?base::\link[base]{rank}}.
  }
  \item{na.rm, strictly}{
    See \code{?base::\link[base]{is.unsorted}}.
  }
  \item{method}{
    See \code{?base::\link[base]{order}}.
  }
  \item{decreasing, na.last}{
    See \code{?base::\link[base]{sort}}.
  }
  \item{by}{
    Not supported for recursively compared list-like objects.
  }
  \item{...}{
    Extra arguments supported by specific methods and propagated to the
    underlying element-wise operations.
  }
}

\details{
  Ordinary \link{List} objects and ordinary \link[base]{list}s compare
  recursively by default. That is, \code{pcompare()}, the binary comparison
  operators, and related helpers are applied element-wise to the list
  elements, and typically return list-like results parallel to the inputs
  rather than a flat integer or logical vector.

  Whether a list-like object should compare recursively is controlled by the
  low-level \code{pcompareRecursively()} generic. The default methods return
  \code{TRUE} for \link{List} objects and ordinary lists. Specific subclasses
  can override this and return \code{FALSE} when they should be compared as
  atomic vector-like entities rather than by recursing into their elements.

  The recursive methods are implemented by applying the corresponding scalar
  or vector operation to each list element. For example, \code{match()} and
  \code{duplicated()} return list-like objects parallel to \code{x}, and
  \code{order()} returns an \link[IRanges]{IntegerList} containing the order
  within each list element. Similarly, \code{is.unsorted()} is applied to
  each list element and returns one logical value per element of \code{x}.
}

\value{
  For \code{pcompare}: typically an \link[IRanges]{IntegerList} parallel to
  the longer input.

  For \code{pcompareRecursively}: a single logical indicating whether
  comparisons on \code{x} should recurse into its elements.

  For \code{match}: typically an \link[IRanges]{IntegerList} parallel to
  \code{x}.

  For \code{duplicated}: typically an \link[IRanges]{LogicalList} parallel to
  \code{x}.

  For \code{unique}: an object like \code{x}, with duplicates removed within
  each list element when recursive comparison is used.

  For \code{order}: an \link[IRanges]{IntegerList} parallel to the input.

  For \code{sort}: an object like \code{x}, with each list element sorted.

  For \code{rank}: an \link[IRanges]{IntegerList} parallel to the input.

  For \code{is.unsorted}: a logical vector of length \code{length(x)},
  indicating for each list element whether that element is unsorted.
}

\seealso{
  \itemize{
    \item The \link{List} class.

    \item \link{Vector-comparison} for the corresponding methods on
          vector-like objects.

    \item \link[IRanges]{IPosRanges-comparison} in the \pkg{IRanges}
          package for comparing and ordering ranges.
  }
}

\examples{
x <- splitAsList(6:1, c(1, 1, 2, 2, 3, 3))
y <- splitAsList(c(6, 1, 4, 3, 1, 2), c(1, 1, 2, 2, 3, 3))

pcompare(x, y)
duplicated(x)
sort(x)
}

\keyword{methods}
