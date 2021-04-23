\name{bindROWS}

\alias{bindROWS}
\alias{bindROWS,NULL-method}
\alias{bindROWS,ANY-method}

\alias{bindCOLS}

\title{Combine objects along their ROWS or COLS}

\description{
  \code{bindROWS} and \code{bindCOLS} are low-level generic functions
  defined in the \pkg{S4Vectors} package for binding objects along their
  1st or 2nd dimension. They are the workhorses behind higher-level
  operations like \code{c()}, \code{rbind()}, or \code{cbind()} on
  most vector-like or rectangular objects defined in Bioconductor.

  They are not intended to be used directly by the end user.
}

\usage{
bindROWS(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
bindCOLS(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
}

\arguments{
  \item{x}{
    An S4 object.
  }
  \item{objects}{
    A list of S4 objects to bind to \code{x}. They should typically (but not
    necessarily) have the same class as \code{x}.
  }
  \item{use.names}{
    Should the names on the input objects be propagated? By default they are.
  }
  \item{ignore.mcols}{
    Should the metadata columns on the input objects be ignored? By defaut
    they are not (i.e. they are propagated).
  }
  \item{check}{
    Should the result object be validated before being returned to the user?
    By default it is.
  }
}

\value{
  An object of the same class as \code{x}.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item The \code{\link[BiocGenerics]{NROW}} and
          \code{\link[BiocGenerics]{NCOL}} generic functions
          defined in the \pkg{BiocGenerics} package.
  }
}

\keyword{utilities}
\keyword{methods}
