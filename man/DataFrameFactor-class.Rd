\name{DataFrameFactor-class}
\docType{class}

\alias{class:DataFrameFactor}
\alias{DataFrameFactor-class}
\alias{DataFrameFactor}

\alias{dim,DataFrameFactor-method}
\alias{dimnames,DataFrameFactor-method}

\alias{[,DataFrameFactor-method}
\alias{[,DataFrameFactor,ANY,ANY,ANY-method}
\alias{[[,DataFrameFactor-method}
\alias{[[,DataFrameFactor,ANY,ANY-method}
\alias{$,DataFrameFactor-method}
\alias{show,DataFrameFactor-method}

\title{DataFrameFactor objects}

\description{
  The DataFrameFactor class is a subclass of the \link{Factor} class where the
  levels are the rows of a \link{DataFrame}. It provides a few methods to mimic
  the behavior of an actual \link{DataFrame} while retaining the memory
  efficiency of the \link{Factor} structure.
}

\usage{
DataFrameFactor(x, levels, index=NULL, ...)  # constructor function
}

\arguments{
  \item{x, levels}{
    \link{DataFrame} objects. At least one of \code{x} and \code{levels} must
    be specified. If \code{index} is \code{NULL}, both can be specified.

    When \code{levels} is specified, it must be a \link{DataFrame} with no
    duplicate rows (i.e. \code{anyDuplicated(levels)} must return 0).

    See \code{?\link{Factor}} for more details.
  }
  \item{index}{
    \code{NULL} or an integer (or numeric) vector of valid positive indices
    (no \code{NA}s) into \code{levels}. See \code{?\link{Factor}} for details.
  }
  \item{...}{
    Optional metadata columns.
  }
}

\value{
  A DataFrameFactor object.
}

\section{Accessors}{
  DataFrameFactor objects support the same set of accessors as \link{Factor}
  objects.  In addition, it mimics some aspects of the \link{DataFrame}
  interface. The general principle is that, for these methods, a
  DataFrameFactor \code{x} behaves like the expanded DataFrame
  \code{\link{unfactor}(x)}.

  \itemize{
    \item \code{x$name} will return column \code{name} from
    \code{\link{levels}(x)} and expand it according to the indices in \code{x}.

    \item \code{x[i, j, ..., drop=TRUE]} will return a new DataFrameFactor
    subsetted to entries \code{i}, where the levels are subsetted by column to
    contain only columns \code{j}. If the resulting levels only have one column
    and \code{drop=TRUE}, the expanded values of the column are returned
    directly.

    \item \code{dim(x)} will return the length of the DataFrameFactor and the
    number of columns in its levels.

    \item \code{dimnames(x)} will return the names of the DataFrameFactor and
    the column names in its levels.
  }
}

\section{Caution}{
The \link{DataFrame}-like methods implemented here are for convenience only.
Users should not assume that the DataFrameFactor complies with other aspects of
the DataFrame interface, due to fundamental differences between a DataFrame and
the \link{Factor} parent class, e.g., in the interpretation of their
\dQuote{length}. Outside of the methods listed above, the DataFrameFactor is
not guaranteed to work as a drop-in replacement for a DataFrame - use
\code{unfactor(x)} instead.
}

\author{Aaron Lun}

\seealso{
\link{Factor} objects for the parent class.
}

\examples{
df <- DataFrame(X=sample(5, 100, replace=TRUE), Y=sample(c("A", "B"), 100, replace=TRUE))
dffac <- DataFrameFactor(df)
dffac

dffac$X
dffac[,c("Y", "X")]
dffac[1:10,"X"]
colnames(dffac)

# The usual Factor methods may also be used:
unfactor(dffac)
levels(dffac)
as.integer(dffac)
}

\keyword{methods}
\keyword{classes}
