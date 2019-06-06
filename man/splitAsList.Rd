\name{splitAsList}

\alias{relistToClass}
\alias{relistToClass,ANY-method}

\alias{splitAsList}
\alias{splitAsList,ANY,ANY-method}

\alias{split}
\alias{split,Vector,ANY-method}
\alias{split,ANY,Vector-method}
\alias{split,Vector,Vector-method}
\alias{split,list,Vector-method}

\title{Divide a vector-like object into groups}

\description{
  \code{split} divides the data in a vector-like object \code{x} into the
  groups defined by \code{f}.

  NOTE: This man page is for the \code{split} methods defined in the
  \pkg{S4Vectors} package. See \code{?base::\link[base]{split}} for the
  default method (defined in the \pkg{base} package).
}

\usage{
\S4method{split}{Vector,ANY}(x, f, drop=FALSE, ...)

\S4method{split}{ANY,Vector}(x, f, drop=FALSE, ...)

\S4method{split}{Vector,Vector}(x, f, drop=FALSE, ...)

\S4method{split}{list,Vector}(x, f, drop=FALSE, ...)

splitAsList(x, f, drop=FALSE, ...)

relistToClass(x)
}

\arguments{
  \item{x, f}{
    2 vector-like objects of the same length. \code{f} will typically be a
    factor, but not necessarily.
  }
  \item{drop}{
    Logical indicating if levels that do not occur should be dropped (if
    \code{f} is a factor).
  }
  \item{...}{
    Extra arguments passed to any of the first 3 \code{split()} methods
    will be passed to \code{splitAsList()} (see Details below).

    Extra arguments passed to the last \code{split()} method will be passed
    to \code{base::\link[base]{split}()} (see Details below).

    Extra arguments passed to \code{splitAsList()} will be passed to the
    specific method selected by method dispatch.
  }
}

\details{
  The first 3 \code{split()} methods just delegate to \code{splitAsList()}.

  The last \code{split()} method just does:
\preformatted{    split(x, as.vector(f), drop=drop, ...)
}
  \code{splitAsList()} is an S4 generic function. It is the workhorse
  behind the first 3 \code{split()} methods above. It behaves like
  \code{base::split()} except that it returns a \link{List} derivative
  instead of an ordinary list. The exact class of this \link{List}
  derivative depends only on the class of \code{x} and can be obtained
  independently with \code{relistToClass(x)}.

  Note that \code{relistToClass(x)} is the opposite of \code{elementType(y)}
  in the sense that the former returns the class of the result of relisting
  (or splitting) \code{x} while the latter returns the class of the result
  of unlisting (or unsplitting) \code{y}.
  More formally, if \code{x} is an object that is relistable and \code{y}
  a list-like object:
\preformatted{    relistToClass(x) is class(relist(x, some_skeleton))
    elementType(y) is class(unlist(y))
}
  Therefore, for any object \code{x} for which \code{relistToClass(x)}
  is defined and returns a valid class,
  \code{elementType(new(relistToClass(x)))} should return \code{class(x)}.
}

\value{
  \code{splitAsList()} and the first 3 \code{split()} methods behave like
  \code{base::\link[base]{split}()} except that they return a \link{List}
  derivative (of class \code{relistToClass(x)}) instead of an
  ordinary list. Like with \code{base::\link[base]{split}()}, all the
  list elements in this object have the same class as \code{x}.
}

\seealso{
  \itemize{
    \item The \code{\link[base]{split}} function in the \pkg{base} package.

    \item The \code{\link[IRanges]{relist}} methods and
          \code{\link[IRanges]{extractList}} generic function defined
          in the \pkg{IRanges} package.

    \item \link{Vector} and \link{List} objects.

    \item \link{Rle} and \link{DataFrame} objects.
  }
}

\examples{
## On an Rle object:
x <- Rle(101:105, 6:2)
split(x, c("B", "B", "A", "B", "A"))

## On a DataFrame object:
groups <- c("group1", "group2")
DF <- DataFrame(
    a=letters[1:10],
    i=101:110,
    group=rep(factor(groups, levels=groups), c(3, 7))
)
split(DF, DF$group)

## Use splitAsList() if you need to split an ordinary vector into a
## List object:
split(letters, 1:2)        # ordinary list
splitAsList(letters, 1:2)  # List object
}

\keyword{methods}
