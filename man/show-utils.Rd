\name{show-utils}

\alias{show-utils}

\alias{coolcat}

\alias{showHeadLines}
\alias{get_showHeadLines}
\alias{set_showHeadLines}
\alias{showTailLines}
\alias{get_showTailLines}
\alias{set_showTailLines}

\alias{printAtomicVectorInAGrid}

\alias{classNameForDisplay}
\alias{classNameForDisplay,ANY-method}

\alias{showAsCell}
\alias{showAsCell,ANY-method}
\alias{showAsCell,numeric-method}
\alias{showAsCell,character-method}
\alias{showAsCell,AsIs-method}
\alias{showAsCell,list-method}
\alias{showAsCell,data.frame-method}

\alias{makeNakedCharacterMatrixForDisplay}
\alias{makeNakedCharacterMatrixForDisplay,ANY-method}

\alias{cbind_mcols_for_display}
\alias{makePrettyMatrixForCompactPrinting}
\alias{makeClassinfoRowForCompactPrinting}

\title{Display utilities}

\description{
  Low-level utilities that control display of vector-like objects.
}

\usage{
get_showHeadLines()
set_showHeadLines(n=5)

get_showTailLines()
set_showTailLines(n=5)
}

\arguments{
  \item{n}{
    A non-negative integer that controls the number of vector elements to
    display.
  }
}

\details{
  For the sake of keeping display compact, the \code{show()} methods
  for \link{Vector} derivatives only display 5 head and 5 tail vector
  elements.

  However, the number of head and tail elements to display can be changed
  by setting global options \code{showHeadLines} and \code{showTailLines}
  to the desired values.

  \code{get_showHeadLines()}, \code{set_showHeadLines()},
  \code{get_showTailLines()}, and \code{set_showTailLines()} are
  convenience functions for getting/setting these global options.
}

\value{
  \code{get_showHeadLines()} and \code{get_showTailLines()} return the
  current \code{showHeadLines} and \code{showTailLines} values.

  \code{set_showHeadLines()} and \code{set_showTailLines()} return the
  \code{showHeadLines} and \code{showTailLines} values before the
  change, invisibly.
}

\seealso{
  \itemize{
    \item \code{\link[base]{options}} in base R.

    \item \link{Vector} objects.
  }
}

\examples{
library(IRanges)
ir <- IRanges(start=11:45, width=10)
ir  # displays 5 head and 5 tail ranges

set_showHeadLines(18)
ir  # displays 18 head ranges

set_showHeadLines()  # back to default
}

\keyword{utilities}
