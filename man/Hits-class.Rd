\name{Hits-class}
\docType{class}

\alias{class:Hits}
\alias{Hits-class}

% accessors
\alias{queryHits}
\alias{queryHits,Hits-method}
\alias{subjectHits}
\alias{subjectHits,Hits-method}
\alias{queryLength}
\alias{queryLength,Hits-method}
\alias{subjectLength}
\alias{subjectLength,Hits-method}
\alias{countQueryHits}
\alias{countQueryHits,Hits-method}
\alias{countSubjectHits}
\alias{countSubjectHits,Hits-method}

% constructor
\alias{Hits}

% coercion
\alias{as.matrix,Hits-method}
\alias{as.table,Hits-method}

% subsetting
\alias{extractROWS,Hits-method}

% displaying
\alias{show,Hits-method}

% other transformations
\alias{t,Hits-method}
\alias{remapHits}

% auto-hits
\alias{isSelfHit}
\alias{isRedundantHit}

\title{Hits objects}

\description{
  The \code{Hits} class stores a set of hits between 2 vector-like objects,
  one called the "query" and the other one the "subject".

  For example, the \code{\link[IRanges]{findOverlaps}} function, defined
  and documented in the \pkg{IRanges} package, returns the hits between
  the \code{query} and \code{subject} arguments in a \code{Hits} object.
}

\details{
  The \code{as.matrix} method coerces a \code{Hits} object to a two column
  \code{matrix} with one row for each hit, where the value in the first
  column is the index of an element in the query and the value in the second
  column is the index of an element in the subject.

  The \code{as.table} method counts the number of hits for each
  query element and outputs the counts as a \code{table}.

  To transpose a \code{Hits} \code{x}, so that the subject and query
  are interchanged, call \code{t(x)}. This allows, for example, counting
  the number of hits for each subject element using \code{as.table}.
}

\section{Accessors}{
  In the code snippets below, \code{x} is a \code{Hits} object.
  \describe{
    \item{}{\code{length(x)}: get the number of hits}
    \item{}{\code{queryHits(x)}: Equivalent to \code{as.data.frame(x)[[1]]}.}
    \item{}{\code{subjectHits(x)}: Equivalent
      to \code{as.data.frame(x)[[2]]}.}
    \item{}{\code{queryLength(x)}, \code{nrow(x)}: get the number of
      elements in the query}
    \item{}{\code{subjectLength(x)}, \code{ncol(x)}: get the number of
      elements in the subject}
    \item{}{\code{countQueryHits(x)}: Counts the number of hits for
      each query, returning an integer vector.
    }
    \item{}{\code{countSubjectHits(x)}: Counts the number of hits for
      each subject, returning an integer vector.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{Hits} object.
  \describe{
    \item{}{\code{as.matrix(x)}: Coerces \code{x} to a two
      column integer matrix, with each row representing a hit
      between a query index (first column) and subject index (second
      column).
    }
    \item{}{\code{as.table(x)}: counts the number of hits for each
      query element in \code{x} and outputs the counts as a \code{table}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a \code{Hits} object.
  \describe{
    \item{}{\code{x[i]}: Subset the Hits object.}
  }
}

\section{Other transformations}{
  In the code snippets below, \code{x} is a \code{Hits} object.
  \describe{
    \item{}{\code{t(x)}: Interchange the query and subject in \code{x},
      returns a transposed \code{Hits}.
    }
    \item{}{\code{remapHits(x, query.map=NULL, new.queryLength=NA,
                               subject.map=NULL, new.subjectLength=NA)}:
      Remaps the hits in \code{x} thru a "query map" and/or a "subject map"
      map. The query hits are remapped thru the "query map", which is specified
      via the \code{query.map} and \code{new.queryLength} arguments. The
      subject hits are remapped thru the "subject map", which is specified via
      the \code{subject.map} and \code{new.subjectLength} arguments.

      The "query map" is conceptually a function (in the mathematical sense)
      and is also known as the "mapping function". It must be defined on the
      1..M interval and take values in the 1..N interval, where N is
      \code{queryLength(x)} and M is the value specified by the user via the
      \code{new.queryLength} argument. Note that this mapping function doesn't
      need to be injective or surjective. Also it is not represented by an R
      function but by an integer vector of length M with no NAs. More precisely
      \code{query.map} can be NULL (identity map), or a vector of
      \code{queryLength(x)} non-NA integers that are >= 1 and
      <= \code{new.queryLength}, or a factor of length \code{queryLength(x)}
      with no NAs (a factor is treated as an integer vector, and, if missing,
      \code{new.queryLength} is taken to be its number of levels). Note that
      a factor will typically be used to represent a mapping function that is
      not injective.

      The same apply to the "subject map".

      \code{remapHits} returns a Hits object where all the query and subject
      hits (accessed with \code{queryHits} and \code{subjectHits},
      respectively) have been remapped thru the 2 specified maps. This
      remapping is actually only the 1st step of the transformation, and is
      followed by 2 additional steps: (2) the removal of duplicated hits,
      and (3) the reordering of the hits (first by query hits, then by subject
      hits). Note that if the 2 maps are injective then the remapping won't
      introduce duplicated hits, so, in that case, step (2) is a no-op (but
      is still performed). Also if the "query map" is strictly ascending and
      the "subject map" ascending then the remapping will preserve the order
      of the hits, so, in that case, step (3) is also a no-op (but is still
      performed).
    }
  }
}

\section{Auto-hits}{
  When the query and subject are the same, the hits between them are
  \emph{auto-hits}. A Hits object containing auto-hits must have its
  \code{queryLength} equal to its \code{subjectLength}. It can be seen
  as an oriented graph where \code{queryLength} is the nb of nodes and
  the hits are the (oriented) edges.
  We provide two little utilities to operate on auto-hits.

  In the code snippets below, \code{x} is a \code{Hits} object.
  \describe{
    \item{}{\code{isSelfHit(x)}: A \emph{self hit} is an edge from a node to
      itself. \code{isSelfHit(x)} returns a logical vector \emph{parallel}
      to \code{x} indicating which elements in \code{x} are self hits.
    }
    \item{}{\code{isRedundantHit(x)}: When there is more than 1 edge between
      2 given nodes (regardless of orientation), the extra edges are considered
      to be \emph{redundant hits}. \code{isRedundantHit(x)} returns a logical
      vector \emph{parallel} to \code{x} indicating which elements in \code{x}
      are redundant hits.
    }
  }
}

\author{Michael Lawrence and H. Pages}

\seealso{
  \itemize{
    \item \link{Hits-comparison} for comparing and ordering hits.

    \item \code{\link[IRanges]{findOverlaps}} in the \pkg{IRanges} package,
          which generates an instance of this class.

    \item \link[IRanges]{Hits-examples} in the \pkg{IRanges} package, for
          some examples of Hits object basic manipulation.

    \item \link[IRanges]{setops-methods} in the \pkg{IRanges} package,
          for set operations on Hits objects.
  }
}

\examples{
query <- c("a", "b", "a", "c", "d")
subject <- c("a", "e", "d", "a", "a", "d")
hits <- findMatches(query, subject)
hits

## ---------------------------------------------------------------------
## selectHits()
## ---------------------------------------------------------------------

selectHits(hits, select="all")  # no-op
selectHits(hits, select="first")
selectHits(hits, select="last")
selectHits(hits, select="arbitrary")
selectHits(hits, select="count")

## ---------------------------------------------------------------------
## remapHits()
## ---------------------------------------------------------------------

query.map=factor(c(a="A", b="B", c="C", d="D")[query], levels=LETTERS[1:4])
remapHits(hits, query.map=query.map)

## See ?`Hits-examples` in the IRanges package for more examples of basic
## manipulation of Hits objects.

## ---------------------------------------------------------------------
## Auto-hits
## ---------------------------------------------------------------------

hits2 <- Hits(c(2, 3, 3, 3, 3, 3, 4, 4, 4), c(4, 3, 2:4, 2, 2:3, 2), 4, 4)
## Hits 2 and 4 are self hits (from 3rd node to itself):
which(isSelfHit(hits2))
## Hits 4, 6, 7, 8, and 9, are redundant hits:
which(isRedundantHit(hits2))

hits3 <- findMatches(query, query)
hits3[!isSelfHit(hits3)]
hits3[!(isSelfHit(hits3) | isRedundantHit(hits3))]
}
\keyword{methods}
\keyword{classes}
