\name{DataFrame-comparison}
\docType{methods}

\alias{sameAsPreviousROW,DataFrame-method}
\alias{match,DataFrame,DataFrame-method}

\alias{duplicated.DataFrame}
\alias{duplicated,DataFrame-method}
\alias{unique.DataFrame}
\alias{unique,DataFrame-method}

\alias{order,DataFrame-method}
\alias{sort.DataFrame}
\alias{sort,DataFrame-method}

\alias{pcompare,DataFrame,DataFrame-method}
\alias{==,DataFrame,DataFrame-method}
\alias{<=,DataFrame,DataFrame-method}

\title{DataFrame comparison methods}

\description{
  The \code{DataFrame} class provides methods to compare across
  rows of the \code{DataFrame}, including ordering and matching. Each
  \code{DataFrame} is effectively treated as a vector of rows.
}

\details{
  The treatment of a \code{DataFrame} as a \dQuote{vector of rows}
  is useful in many cases, e.g., when each row is a record that needs
  to be ordered or matched. The methods provided here allow the use of
  all methods described in \code{?\link{Vector-comparison}}, including
  sorting, matching, de-duplication, and so on.
  
  Careful readers will notice this behaviour differs from the usual 
  semantics of a \code{data.frame}, which acts as a list-like vector 
  of columns. This discrepancy rarely causes problems, as it is not
  particularly common to compare columns of a \code{data.frame} in 
  the first place.

  Note that a \code{match} method for \code{DataFrame} objects is 
  explicitly defined to avoid calling the corresponding method for 
  \code{\link{List}} objects, which would yield the (undesired) list-like
  semantics. The same rationale is behind the explicit definition of
  \code{<=} and \code{==} despite the availability of \code{pcompare}.
}

\usage{
\S4method{sameAsPreviousROW}{DataFrame}(x)

\S4method{match}{DataFrame,DataFrame}(x, table, nomatch = NA_integer_, incomparables = NULL, ...)

\S4method{order}{DataFrame}(..., na.last = TRUE, decreasing = FALSE, method = c("auto", 
    "shell", "radix"))

\S4method{pcompare}{DataFrame,DataFrame}(x, y)

\S4method{==}{DataFrame,DataFrame}(e1, e2)

\S4method{<=}{DataFrame,DataFrame}(e1, e2)
}

\arguments{
  \item{x, table, y, e1, e2}{
    A \code{\linkS4class{DataFrame}} object.
  }
  \item{nomatch, incomparables}{
    See \code{?base::\link[base]{match}}.
  }
  \item{...}{
    For \code{match}, further arguments to pass to \code{\link[base]{match}}.

    For \code{order}, one or more \code{\linkS4class{DataFrame}} objects.
  }
  \item{decreasing, na.last, method}{
    See \code{?base::\link[base]{order}}.
  }
}

\value{
  For \code{sameAsPreviousROW}: see \code{\link{sameAsPreviousROW}}.

  For \code{match}: see \code{\link[base]{match}}.

  For \code{order}: see \code{\link[base]{order}}.

  For \code{pcompare}, \code{==} and \code{<=}: see \code{\link{pcompare}}.
}

\author{
  Aaron Lun
}

\examples{
# Mocking up a DataFrame.
DF <- DataFrame(
    A=sample(LETTERS, 100, replace=TRUE),
    B=sample(5, 100, replace=TRUE)
)

# Matching:
match(DF, DF[1:10,])
selfmatch(DF)
unique(DF)

# Ordering, alone and with other vectors:
sort(DF)
order(DF, runif(nrow(DF)))

# Parallel comparison:
DF==DF
DF==DF[1,]
}

\keyword{methods}
