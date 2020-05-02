\name{DataTable-class}
\docType{class}

\alias{class:DataTable}
\alias{DataTable-class}
\alias{DataTable}

\alias{na.omit,DataTable-method}
\alias{na.exclude,DataTable-method}
\alias{is.na,DataTable-method}
\alias{complete.cases,DataTable-method}

\alias{merge,DataTable,DataTable-method}
\alias{merge,data.frame,DataTable-method}
\alias{merge,DataTable,data.frame-method}

\alias{by,DataTable-method}

\alias{transform.DataTable}
\alias{transform,DataTable-method}

\alias{duplicated,DataTable-method}
\alias{duplicated.DataTable}
\alias{unique,DataTable-method}
\alias{unique.DataTable}

\alias{as.env}
\alias{as.env,NULL-method}
\alias{as.data.frame.DataTable}
\alias{as.data.frame,DataTable-method}

\alias{makeNakedCharacterMatrixForDisplay,DataTable-method}
\alias{show,DataTable-method}

\alias{xtabs,DataTable-method}


\title{DataTable objects}

\description{
  The DataTable class will go away. It sits between the \link{RectangularData}
  and \link{DataFrame} classes.
  The DataTable methods documented here will need to become
  RectangularData or DataFrame methods, or to be removed.
}

\section{High-level subsetting}{
  In the code snippets below, \code{x} is a DataTable object.
  \describe{
    \item{}{
      \code{\link[stats:na.fail]{na.omit}(object)}:
      Returns a subset with incomplete cases removed.
    }
    \item{}{
      \code{\link[stats:na.fail]{na.exclude}(object)}:
      Returns a subset with incomplete cases removed (but to be
      included with NAs in statistical results).
    }
    \item{}{
      \code{\link[base:NA]{is.na}(x)}:
      Returns a logical matrix indicating which cells are missing.
    }
    \item{}{
      \code{\link[stats]{complete.cases}(x)}:
      Returns a logical vector identifying which cases have no missing values.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a DataTable object.
  \describe{
    \item{}{
      \code{merge(x, y, ...)}: Merges two \code{DataTable}
      objects \code{x} and \code{y}, with arguments in \code{...} being
      the same as those allowed by the base \code{\link{merge}}. It is
      allowed for either \code{x} or \code{y} to be a \code{data.frame}.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a DataTable object.
  \describe{
    \item{}{
      \code{by(data, INDICES, FUN, ..., simplify = TRUE)}:
      Apply \code{FUN} to each group of \code{data}, a \code{DataTable},
      formed by the factor (or list of factors) \code{INDICES}. Exactly 
      the same contract as \code{\link{as.data.frame}}.
    }
  }
}

\section{Transforming}{
  In the code snippets below, \code{x} is a DataTable object.
  \describe{
    \item{}{
      \code{transform(`_data`, ...)}: adds or replaces columns based on
      expressions in \code{\dots}. See \code{\link{transform}}.
    }
  }
}

\section{Utilities}{
  \describe{
    \item{}{\code{duplicated(x)}: Returns a logical vector indicating the rows
      that are identical to a previous row.
    }
    \item{}{\code{unique(x)}: Returns a new \code{DataTable} after
      removing the duplicated rows from \code{x}.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an environment from \code{x} with a symbol for each
      \code{colnames(x)}. The values are not actually copied into the
      environment. Rather, they are dynamically bound using
      \code{\link{makeActiveBinding}}. This prevents unnecessary copying
      of the data from the external vectors into R vectors. The values
      are cached, so that the data is not copied every time the symbol
      is accessed.
    }
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE,
	...)}: 
      Creates the corresponding data.frame.
    }
  }
}

\section{Statistical modeling with DataTable}{
  A number of wrappers are implemented for performing statistical
  procedures, such as model fitting, with DataTable objects.
  \subsection{Tabulation}{
    \describe{
      \item{}{\code{xtabs(formula = ~., data, subset, na.action,
          exclude = c(NA, NaN), drop.unused.levels = FALSE)}:
        Like the original \code{\link{xtabs}}, except \code{data} is a
        \code{DataTable}.
      }
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
showClass("DataTable")  # shows (some of) the known subclasses

library(IRanges)
df <- DataFrame(as.data.frame(UCBAdmissions))
xtabs(Freq ~ Gender + Admit, df)
}

\keyword{methods}
\keyword{classes}
