\name{RectangularData-class}
\docType{class}

\alias{class:RectangularData}
\alias{RectangularData-class}
\alias{RectangularData}

\alias{ROWNAMES}
\alias{ROWNAMES,ANY-method}
\alias{ROWNAMES,RectangularData-method}

\alias{head.RectangularData}
\alias{head,RectangularData-method}
\alias{tail.RectangularData}
\alias{tail,RectangularData-method}
\alias{subset,RectangularData-method}

\alias{rbind.RectangularData}
\alias{rbind,RectangularData-method}
\alias{cbind.RectangularData}
\alias{cbind,RectangularData-method}

\title{RectangularData objects}

\description{
  RectangularData is a virtual class with no slots to be extended by
  classes that aim at representing objects with a rectangular shape.
  Current RectangularData derivatives are \link{DataFrame},
  \link[DelayedArray]{DelayedMatrix},
  \link[SummarizedExperiment]{SummarizedExperiment}, and
  \link[SummarizedExperiment]{Assays} objects.

  RectangularData derivatives are expected to support the 2D API: at least
  \code{dim()}, but also typically \code{dimnames}, \code{[} (the 2D form
  \code{x[i, j]}), \code{bindROWS()}, and \code{bindCOLS()}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a RectangularData derivative.
  \describe{
    \item{}{
      \code{nrow(x)}, \code{ncol(x)}:
      Get the number of rows and columns, respectively.
    }
    \item{}{
      \code{NROW(x)}, \code{NCOL(x)}:
      Same as \code{nrow(x)} and \code{ncol(x)}, respectively.
    }
    \item{}{
      \code{dim(x)}:
      Length two integer vector defined as \code{c(nrow(x), ncol(x))}.
    }
    \item{}{
      \code{rownames(x)}, \code{colnames(x)}:
      Get the names of the rows and columns, respectively.
    }
    \item{}{
      \code{dimnames(x)}:
      Length two list of character vectors defined as
      \code{list(rownames(x), colnames(x))}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a RectangularData derivative.
  \describe{
    \item{}{
      \code{x[i, j, drop=TRUE]}:
      Return a new RectangularData derivative of the same class as \code{x}
      made of the selected rows and columns.

      For single row and/or column selection, the \code{drop} argument
      specifies whether or not to "drop the dimensions" of the result.
      More precisely, when \code{drop=TRUE} (the default), a single
      row or column is returned as a vector-like object (of length/NROW
      equal to \code{ncol(x)} if a single row, or equal to \code{nrow(x)}
      if a single column).

      Not all RectangularData derivatives support the \code{drop} argument.
      For example \link{DataFrame} and \link[DelayedArray]{DelayedMatrix}
      objects support it (only for a single column
      selection for \link{DataFrame} objects), but
      \link[SummarizedExperiment]{SummarizedExperiment} objects don't
      (\code{drop} is ignored for these objects and subsetting always
      returns a \link[SummarizedExperiment]{SummarizedExperiment}
      derivative of the same class as \code{x}).
    }
    \item{}{
      \code{head(x, n=6L)}:
      If \code{n} is non-negative, returns the first n rows of the
      RectangularData derivative.
      If \code{n} is negative, returns all but the last \code{abs(n)}
      rows of the RectangularData derivative.
    }
    \item{}{
      \code{tail(x, n=6L)}:
      If \code{n} is non-negative, returns the last n rows of the
      RectangularData derivative.
      If \code{n} is negative, returns all but the first \code{abs(n)}
      rows of the RectangularData derivative.
    }
    \item{}{
      \code{subset(x, subset, select, drop=FALSE)}:
      Return a new RectangularData derivative using:
      \describe{
        \item{subset}{logical expression indicating rows to keep, where missing
          values are taken as FALSE.}
        \item{select}{expression indicating columns to keep.}
        \item{drop}{passed on to \code{[} indexing operator.}
      }
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a RectangularData derivative.
  \describe{
    \item{}{
      \code{rbind(...)}: Creates a new RectangularData derivative by
      combining the rows of the RectangularData derivatives in \code{...}.
    }
    \item{}{
      \code{cbind(...)}: Creates a new RectangularData derivative by
      combining the columns of the RectangularData derivatives in \code{...}.
    }
  }
}

\seealso{
  \itemize{
    \item \link{DataFrame} for an implementation that mimics \code{data.frame}.

    \item \link{data.frame}
  }
}

\examples{
showClass("RectangularData")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
