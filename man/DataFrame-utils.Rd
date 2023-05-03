\name{DataFrame-utils}

\alias{DataFrame-utils}

\alias{relistToClass,DataFrame-method}
\alias{relistToClass,data.frame-method}

\alias{na.omit,DataFrame-method}
\alias{na.exclude,DataFrame-method}
\alias{is.na,DataFrame-method}
\alias{complete.cases,DataFrame-method}

\alias{transform.DataFrame}
\alias{transform,DataFrame-method}

\alias{xtabs,DataFrame-method}

\title{Common operations on DataFrame objects}

\description{
  Common operations on \link{DataFrame} objects.
}

\section{Splitting}{
  In the code snippet below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{\code{split(x, f, drop = FALSE)}:}{
      Splits \code{x} into a \link[IRanges]{SplitDataFrameList}
      object, according to \code{f}, dropping elements corresponding
      to unrepresented levels if \code{drop} is \code{TRUE}.
    }
  }
}

\section{Looping}{
  In the code snippet below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{\code{by(data, INDICES, FUN, ..., simplify = TRUE)}:}{
      Apply \code{FUN} to each group of \code{data}, a \link{DataFrame},
      formed by the factor (or list of factors) \code{INDICES}. Exactly
      the same contract as \code{\link{as.data.frame}}.
    }
  }
}

\section{Subsetting based on NA content}{
  In the code snippets below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{\code{\link{na.omit}(object)}:}{
      Returns a subset with incomplete cases removed.
    }
    \item{\code{\link{na.exclude}(object)}:}{
      Returns a subset with incomplete cases removed (but to be
      included with NAs in statistical results).
    }
    \item{\code{\link[base]{is.na}(x)}:}{
      Returns a logical matrix indicating which cells are missing.
    }
    \item{\code{\link{complete.cases}(x)}:}{
      Returns a logical vector identifying which cases have no missing values.
    }
  }
}

\section{Transforming}{
  In the code snippet below, \code{x} is a \link{DataFrame} object.
  \describe{
    \item{\code{transform(`_data`, ...)}:}{ adds or replaces columns based on
      expressions in \code{\dots}. See \code{\link{transform}}.
    }
  }
}

\section{Statistical modeling with DataFrame}{
  A number of wrappers are implemented for performing statistical
  procedures, such as model fitting, with \link{DataFrame} objects.
  \subsection{Tabulation}{
    \describe{
      \item{\code{xtabs(formula = ~., data, subset, na.action,
          exclude = c(NA, NaN), drop.unused.levels = FALSE)}:}{
        Like the original \code{\link{xtabs}}, except \code{data} is a
        \link{DataFrame}.
      }
    }
  }
}

\author{Michael Lawrence}

\seealso{
  \itemize{
    \item \code{\link{by}} in the \pkg{base} package.

    \item \code{\link{na.omit}} in the \pkg{stats} package.

    \item \code{\link{transform}} in the \pkg{base} package.

    \item \code{\link{xtabs}} in the \pkg{stats} package.

    \item \code{\link{splitAsList}} in this package (\pkg{S4Vectors}).

    \item \link[IRanges]{SplitDataFrameList} objects in the \pkg{IRanges}
          package.

    \item \link{DataFrame} objects.
  }
}

\examples{
## split
sw <- DataFrame(swiss)
swsplit <- split(sw, sw[["Education"]])

## rbind & cbind
do.call(rbind, as.list(swsplit))
cbind(DataFrame(score), DataFrame(counts))

df <- DataFrame(as.data.frame(UCBAdmissions))
xtabs(Freq ~ Gender + Admit, df)
}

\keyword{utilities}
\keyword{methods}
