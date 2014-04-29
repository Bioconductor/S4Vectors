\name{Vector-class}
\docType{class}

\alias{class:DataTableORNULL}
\alias{DataTableORNULL-class}
\alias{DataTableORNULL}

% Vector class, functions and methods:
\alias{class:Vector}
\alias{Vector-class}
\alias{Vector}

\alias{fixedColumnNames}
\alias{fixedColumnNames,ANY-method}

\alias{NROW,Vector-method}
\alias{ROWNAMES,Vector-method}

\alias{elementMetadata}
\alias{elementMetadata,Vector-method}
\alias{mcols}
\alias{mcols,Vector-method}
\alias{values}
\alias{values,Vector-method}

\alias{elementMetadata<-}
\alias{elementMetadata<-,Vector-method}
\alias{mcols<-}
\alias{mcols<-,Vector-method}
\alias{values<-}
\alias{values<-,Vector-method}
\alias{rename}
\alias{rename,vector-method}
\alias{rename,Vector-method}

\alias{as.logical,Vector-method}
\alias{as.integer,Vector-method}
\alias{as.numeric,Vector-method}
\alias{as.double,Vector-method}
\alias{as.complex,Vector-method}
\alias{as.character,Vector-method}
\alias{as.raw,Vector-method}
\alias{coerce,Vector,vector-method}
\alias{coerce,Vector,logical-method}
\alias{coerce,Vector,integer-method}
\alias{coerce,Vector,numeric-method}
\alias{coerce,Vector,double-method}
\alias{coerce,Vector,complex-method}
\alias{coerce,Vector,character-method}
\alias{coerce,Vector,raw-method}
\alias{coerce,Vector,data.frame-method}
\alias{as.data.frame.Vector}
\alias{as.data.frame,Vector-method}
\alias{as.env,Vector-method}

\alias{[,Vector-method}
\alias{[<-,Vector-method}
\alias{extractROWS,ANY-method}
\alias{extractROWS,matrix-method}
\alias{replaceROWS,Vector-method}

\alias{showAsCell}
\alias{showAsCell,ANY-method}
\alias{showAsCell,Vector-method}

\alias{c,Vector-method}
\alias{append,Vector,Vector-method}


\title{Vector objects}

\description{
  The Vector virtual class serves as the heart of the S4Vectors package
  and has over 90 subclasses. It serves a similar role as \link[base]{vector}
  in base R.

  The Vector class supports the storage of \emph{global} and
  \emph{element-wise} metadata:
  \enumerate{
  \item The \emph{global} metadata annotates the object as a whole:
        this metadata is accessed via the \code{metadata} accessor and
        is represented as an ordinary list;
  \item The \emph{element-wise} metadata annotates individual elements
        of the object: this metadata is accessed via the \code{mcols}
        accessor (\code{mcols} stands for \emph{metadata columns}) and
        is represented as a \link{DataTable} object (i.e. as an instance
        of a concrete subclass of \link{DataTable}, e.g. a
        \link[IRanges]{DataFrame} object), with a row for each element
        and a column for each metadata variable. Note that the element-wise
        metadata can also be \code{NULL}.
  }

  To be functional, a class that inherits from Vector must define at
  least a \code{length} and a \code{"["} method.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a Vector object.
  
  \describe{
    \item{}{
      \code{length(x)}:
      Get the number of elements in \code{x}.
    }
    \item{}{
      \code{NROW(x)}:
      Defined as \code{length(x)} for any Vector object that is
      \emph{not} a \link{DataTable} object.
      If \code{x} is a \link{DataTable} object, then it's
      defined as \code{nrow(x)}.
    }
    \item{}{
      \code{names(x)}, \code{names(x) <- value}:
      Get or set the names of the elements in the Vector.
    }
    \item{}{
      \code{rename(x, value, ...)}:
      Replace the names of \code{x} according to a mapping defined by a named
      character vector, formed by concatenating \code{value} with any
      arguments in \code{...}. The names of the character vector
      indicate the source names, and the corresponding values the
      destination names. This also works on a plain old \code{vector}.
    }
    \item{}{
      \code{nlevels(x)}:
      Returns the number of factor levels.
    }
    \item{}{
      \code{mcols(x, use.names=FALSE)}, \code{mcols(x) <- value}:
      Get or set the metadata columns.
      If \code{use.names=TRUE} and the metadata columns are not \code{NULL},
      then the names of \code{x} are propagated as the row names of the
      returned \link{DataTable} object.
      When setting the metadata columns, the supplied value must be \code{NULL}
      or a \link{DataTable} object holding element-wise metadata.
    }
    \item{}{
       \code{elementMetadata(x, use.names=FALSE)},
       \code{elementMetadata(x) <- value},
       \code{values(x, use.names=FALSE)},
       \code{values(x) <- value}:
       Alternatives to \code{mcols} functions. Their use is discouraged.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "data.frame")}, \code{as.data.frame(from)}:
      Coerces \code{from}, a \code{Vector}, to a \code{data.frame} by
      first coercing the \code{Vector} to a \code{vector} via
      \code{as.vector}. Note that many \code{Vector} derivatives do not
      support \code{as.vector}, so this coercion is possible only for
      certain types.
    }
    \item{}{\code{as.env(x)}: Constructs an environment object
      containing the elements of \code{mcols(x)}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Vector object or regular R vector
  object. The R vector object methods for \code{window} are defined in this
  package and the remaining methods are defined in base R.

  \describe{
    \item{}{
      \code{x[i, drop=TRUE]}:
      If defined, returns a new Vector object made of selected elements
      \code{i}, which can be missing; an NA-free logical, numeric, or
      character vector; or a logical Rle object. The \code{drop} argument
      specifies whether or not to coerce the returned sequence to a standard
      vector.
    }
    \item{}{
      \code{x[i] <- value}:
      Replacement version of \code{x[i]}.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Vector objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
    }
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{Vector} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
  }
}

\seealso{
  \itemize{
    \item \link[IRanges]{Rle}, \link[IRanges]{IRanges} and \link[XVector]{XRaw}
          for example implementations.

    \item \link[IRanges]{Vector-comparison} for comparing, ordering, and
          tabulating vector-like objects.

    \item \link[IRanges]{List} for a direct extension that serves a similar role
          as \link[base]{list} in base R.

    \item \link[IRanges]{extractList} for grouping elements of a vector-like
          object into a list-like object.

    \item \link{DataTable} which is the type of objects returned by the
          \code{mcols} accessor.

    \item The \link{Annotated} class, which Vector extends.
  }
}

\examples{
  showClass("Vector")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
