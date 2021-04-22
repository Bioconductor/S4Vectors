\name{RectangularData-class}
\docType{class}

\alias{class:RectangularData}
\alias{RectangularData-class}
\alias{RectangularData}

\alias{vertical_slot_names}
\alias{horizontal_slot_names}

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
  classes that aim at representing objects with a 2D rectangular shape.

  Some examples of RectangularData extensions are:
  \itemize{
    \item The \link{DataFrame} class defined in this package (\pkg{S4Vectors}).
    \item The \link[DelayedArray]{DelayedMatrix} class defined in the
          \pkg{DelayedArray} package.
    \item The \link[SummarizedExperiment]{SummarizedExperiment} and
          \link[SummarizedExperiment]{Assays} classes defined in the
          \pkg{SummarizedExperiment} package.
  }
}

\details{
  Any object that belongs to a class that extends RectangularData is called
  a \emph{RectangularData derivative}. Users should be able to access a
  RectangularData derivative via the standard \emph{2D API} defined in
  base R, that is, via things like \code{dim()}, \code{dimnames()}, and the
  2D form of \code{[} (\code{x[i, j]}).

  Not all RectangularData derivatives will necessarily support the full
  2D API but they must support at least \code{dim()}, \code{nrow(x)},
  \code{ncol(x)}, \code{NROW(x)}, and \code{NCOL(x)}. And of course,
  \code{dim()} is required to return an integer vector of length 2 on
  these objects.

  Developers who implement RectangularData extensions should also make
  sure that they support low-level operations \code{bindROWS()} and
  \code{bindCOLS()}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a RectangularData derivative.
  Not all RectangularData derivatives will support all these accessors.
  \describe{
    \item{}{
      \code{dim(x)}:
      Length two integer vector defined as \code{c(nrow(x), ncol(x))}.
      Must work on any RectangularData derivative.
    }
    \item{}{
      \code{nrow(x)}, \code{ncol(x)}:
      Get the number of rows and columns, respectively.
      Must work on any RectangularData derivative.
    }
    \item{}{
      \code{NROW(x)}, \code{NCOL(x)}:
      Same as \code{nrow(x)} and \code{ncol(x)}, respectively.
      Must work on any RectangularData derivative.
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

    \item \link{data.frame} objecds in base R.
  }
}

\examples{
showClass("RectangularData")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
