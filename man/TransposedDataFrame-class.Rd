\name{TransposedDataFrame-class}
\docType{class}

\alias{class:TransposedDataFrame}
\alias{TransposedDataFrame-class}
\alias{TransposedDataFrame}

\alias{t.DataFrame}
\alias{t,DataFrame-method}
\alias{t.TransposedDataFrame}
\alias{t,TransposedDataFrame-method}

\alias{nrow,TransposedDataFrame-method}
\alias{ncol,TransposedDataFrame-method}
\alias{rownames,TransposedDataFrame-method}
\alias{colnames,TransposedDataFrame-method}
\alias{length,TransposedDataFrame-method}
\alias{names,TransposedDataFrame-method}

\alias{dimnames<-,TransposedDataFrame-method}
\alias{names<-,TransposedDataFrame-method}

\alias{extractROWS,TransposedDataFrame,ANY-method}
\alias{extractCOLS,TransposedDataFrame-method}
\alias{[,TransposedDataFrame-method}
\alias{getListElement,TransposedDataFrame-method}

\alias{normalizeSingleBracketReplacementValue,TransposedDataFrame-method}
\alias{[<-,TransposedDataFrame-method}

\alias{coerce,DataFrame,TransposedDataFrame-method}
\alias{coerce,TransposedDataFrame,DataFrame-method}
\alias{as.matrix,TransposedDataFrame-method}
\alias{as.list.TransposedDataFrame}
\alias{as.list,TransposedDataFrame-method}
\alias{coerce,ANY,TransposedDataFrame-method}

\alias{makeNakedCharacterMatrixForDisplay,TransposedDataFrame-method}
\alias{show,TransposedDataFrame-method}

\alias{bindROWS,TransposedDataFrame-method}
\alias{bindCOLS,TransposedDataFrame-method}

\title{TransposedDataFrame objects}

\description{
  The TransposedDataFrame class is a container for representing a transposed
  \link{DataFrame} object, that is, a rectangular data container where the
  rows are the variables and the columns the observations.

  A typical situation for using a TransposedDataFrame object
  is when one needs to store a \link{DataFrame} object in the
  \code{\link[SummarizedExperiment]{assay}()} component of
  a \link[SummarizedExperiment]{SummarizedExperiment} object
  but the rows in the \link{DataFrame} object should correspond to the
  samples and the columns to the features. In this case the
  \link{DataFrame} object must first be transposed so that the variables
  in it run "horizontally" instead of "vertically". See the Examples
  section at the bottom of this man page for an example.
}

\details{
  TransposedDataFrame objects are constructed by calling \code{t()}
  on a \link{DataFrame} object.

  Like for a \link{DataFrame} object, or, more generally, for a
  data-frame-like object, the length of a TransposedDataFrame object
  is its number of variables. However, \emph{unlike} for a data-frame-like
  object, its length is also its number of rows, not its number of columns.
  For this reason, a TransposedDataFrame object is NOT considered to be
  a data-frame-like object.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link{DataFrame} objects.

    \item \link[SummarizedExperiment]{SummarizedExperiment} objects in the
          \pkg{SummarizedExperiment} package.
  }
}

\examples{
## A DataFrame object with 3 variables:
df <- DataFrame(aa=101:126, bb=letters, cc=Rle(c(TRUE, FALSE), 13),
                row.names=LETTERS)
dim(df)
length(df)
df$aa

tdf <- t(df)
tdf
dim(tdf)
length(tdf)
tdf$aa

t(tdf)  # back to 'df'
stopifnot(identical(df, t(tdf)))

tdf$aa <- 0.05 * tdf$aa

x1 <- DataFrame(A=1:5, B=letters[1:5], C=11:15)
y1 <- DataFrame(B=c(FALSE, NA, TRUE), C=c(FALSE, NA, TRUE), A=101:103)
cbind(t(x1), t(y1))
stopifnot(identical(t(rbind(x1, y1)), cbind(t(x1), t(y1))))

## A TransposedDataFrame object can be used in the assay() component of a
## SummarizedExperiment object if the transposed layout is needed i.e. if
## the rows and columns of the original DataFrame object need to be treated
## as the samples and features (in this order) of the SummarizedExperiment
## object:
library(SummarizedExperiment)
se1 <- SummarizedExperiment(df)
se1
assay(se1)  # the 3 variables run "vertically"

se2 <- SummarizedExperiment(tdf)
se2
assay(se2)  # the 3 variables run "horizontally"
}
\keyword{methods}
\keyword{classes}
