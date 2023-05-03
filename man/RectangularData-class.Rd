\name{RectangularData-class}
\docType{class}

\alias{class:RectangularData}
\alias{RectangularData-class}
\alias{RectangularData}

\alias{vertical_slot_names}
\alias{horizontal_slot_names}

\alias{dim,RectangularData-method}
\alias{dimnames,RectangularData-method}
\alias{dimnames<-,RectangularData-method}

\alias{ROWNAMES}
\alias{ROWNAMES,ANY-method}
\alias{ROWNAMES,RectangularData-method}
\alias{ROWNAMES<-}
\alias{ROWNAMES<-,ANY-method}
\alias{ROWNAMES<-,RectangularData-method}

\alias{head.RectangularData}
\alias{head,RectangularData-method}
\alias{tail.RectangularData}
\alias{tail,RectangularData-method}
\alias{subset,RectangularData-method}

\alias{rbind.RectangularData}
\alias{rbind,RectangularData-method}
\alias{cbind.RectangularData}
\alias{cbind,RectangularData-method}

\alias{combineRows}
\alias{combineCols}
\alias{combineUniqueCols}

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
  a \emph{RectangularData derivative}.

  Users should be able to access and manipulate RectangularData derivatives
  via the \emph{standard 2D API} defined in base R, that is, using things like
  \code{dim()}, \code{nrow()}, \code{ncol()}, \code{dimnames()}, the 2D form
  of \code{[} (\code{x[i, j]}), \code{rbind()}, \code{cbind()}, etc...

  Not all RectangularData derivatives will necessarily support the full
  2D API but they must support at least \code{dim()}, \code{nrow(x)},
  \code{ncol(x)}, \code{NROW(x)}, and \code{NCOL(x)}. And of course,
  \code{dim()} must return an integer vector of length 2 on any of these
  objects.

  Developers who implement RectangularData extensions should also make
  sure that they support low-level operations \code{bindROWS()} and
  \code{bindCOLS()}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a RectangularData derivative.
  Not all RectangularData derivatives will support all these accessors.
  \describe{
    \item{\code{dim(x)}:}{
      Length two integer vector defined as \code{c(nrow(x), ncol(x))}.
      Must work on any RectangularData derivative.
    }
    \item{\code{nrow(x)}, \code{ncol(x)}:}{
      Get the number of rows and columns, respectively.
      Must work on any RectangularData derivative.
    }
    \item{\code{NROW(x)}, \code{NCOL(x)}:}{
      Same as \code{nrow(x)} and \code{ncol(x)}, respectively.
      Must work on any RectangularData derivative.
    }
    \item{\code{dimnames(x)}:}{
      Length two list of character vectors defined as
      \code{list(rownames(x), colnames(x))}.
    }
    \item{\code{rownames(x)}, \code{colnames(x)}:}{
      Get the names of the rows and columns, respectively.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a RectangularData derivative.
  \describe{
    \item{\code{x[i, j, drop=TRUE]}:}{
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
    \item{\code{head(x, n=6L)}:}{
      If \code{n} is non-negative, returns the first n rows of the
      RectangularData derivative.
      If \code{n} is negative, returns all but the last \code{abs(n)}
      rows of the RectangularData derivative.
    }
    \item{\code{tail(x, n=6L)}:}{
      If \code{n} is non-negative, returns the last n rows of the
      RectangularData derivative.
      If \code{n} is negative, returns all but the first \code{abs(n)}
      rows of the RectangularData derivative.
    }
    \item{\code{subset(x, subset, select, drop=FALSE)}:}{
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
  In the code snippets below, all the input objects are expected to be
  RectangularData derivatives.
  \describe{
    \item{\code{rbind(...)}:}{ Creates a new RectangularData derivative by
      aggregating the rows of the input objects.
    }
    \item{\code{cbind(...)}:}{ Creates a new RectangularData derivative by
      aggregating the columns of the input objects.
    }
    \item{\code{combineRows(x, ...)}:}{ Creates a new RectangularData derivative
      (of the same class as \code{x}) by aggregating the rows of the input
      objects. Unlike \code{rbind()}, \code{combineRows()} will handle cases
      involving differences in the column names of the input objects by
      adding the missing columns to them, and filling these
      columns with \code{NA}s. The column names of the returned object are a
      union of the column names of the input objects.

      Behaves like an \emph{endomorphism} with respect to its first argument
      i.e. returns an object of the same class as \code{x}.

      Finally note that this is a generic function with methods defined
      for \link{DataFrame} objects and other RectangularData derivatives.
    }
    \item{\code{combineCols(x, ..., use.names=TRUE)}:}{ Creates a new
      RectangularData derivative (of the same class as \code{x}) by
      aggregating the columns of the input objects. Unlike \code{cbind()},
      \code{combineCols()} will handle cases involving differences in the
      number of rows of the input objects.

      If \code{use.names=TRUE}, all objects are expected to have
      non-\code{NULL}, non-duplicated row names. These row names do not have
      to be the same, or even shared, across the input objects. Missing rows
      in any individual input object are filled with \code{NA}s, such
      that the row names of the returned object are a union of the row names
      of the input objects.

      If \code{use.names=FALSE}, all objects are expected to have the same
      number of rows, and this function behaves the same as \code{cbind()}.
      The row names of the returned object is set to \code{rownames(x)}.
      Differences in the row names between input objects are ignored.

      Behaves like an \emph{endomorphism} with respect to its first argument
      i.e. returns an object of the same class as \code{x}.

      Finally note that this is a generic function with methods defined
      for \link{DataFrame} objects and other RectangularData derivatives.
    }
    \item{\code{combineUniqueCols(x, ..., use.names=TRUE)}:}{ Same as
      \code{combineCols()}, but this function will attempt to collapse
      multiple columns with the same name across the input objects into a
      single column in the output. This guarantees that the column names
      in the output object are always unique. The only exception is for
      unnamed columns, which are not collapsed. The function works on any
      rectangular objects for which \code{combineCols()} works.

      When \code{use.names=TRUE}, collapsing is only performed if the
      duplicated column has identical values for the shared rows in the
      input objects involved. Otherwise, the contents of the later
      input object is simply ignored with a warning. Similarly, if
      \code{use.names=FALSE}, the duplicated columns must be identical
      for all rows in the affected input objects.

      Behaves like an \emph{endomorphism} with respect to its first argument
      i.e. returns an object of the same class as \code{x}.

      Finally note that this function is implemented on top of
      \code{combineCols()} and is expected to work on any RectangularData
      derivatives for which \code{combineCols()} works.
    }
  }
}

\author{Hervé Pagès and Aaron Lun}

\seealso{
  \itemize{
    \item \link{DataFrame} for a RectangularData extension that mimics
          \code{data.frame} objects from base R.

    \item \link{DataFrame-combine} for \code{combineRows()},
          \code{combineCols()}, and \code{combineUniqueCols()} examples
          involving \link{DataFrame} objects.

    \item \link{data.frame} objects in base R.
  }
}

\examples{
showClass("RectangularData")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
