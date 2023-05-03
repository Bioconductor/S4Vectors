\name{Vector-class}
\docType{class}

\alias{class:Vector}
\alias{Vector-class}
\alias{Vector}

\alias{class:vector_OR_Vector}
\alias{vector_OR_Vector-class}
\alias{vector_OR_Vector}

\alias{parallel_slot_names}
\alias{parallel_slot_names,Vector-method}

\alias{updateObject,Vector-method}

\alias{length,Vector-method}
\alias{lengths,Vector-method}
\alias{elementMetadata}
\alias{elementMetadata,Vector-method}
\alias{mcols}
\alias{mcols,Vector-method}
\alias{values}
\alias{values,Vector-method}
\alias{anyNA,Vector-method}
\alias{is.na,Vector-method}

\alias{elementMetadata<-}
\alias{elementMetadata<-,Vector-method}
\alias{mcols<-}
\alias{mcols<-,Vector-method}
\alias{values<-}
\alias{values<-,Vector-method}
\alias{rename}
\alias{rename,vector-method}
\alias{rename,Vector-method}
\alias{unname,Vector-method}

\alias{as.logical,Vector-method}
\alias{as.integer,Vector-method}
\alias{as.numeric,Vector-method}
\alias{as.double,Vector-method}
\alias{as.complex,Vector-method}
\alias{as.character,Vector-method}
\alias{as.raw,Vector-method}
\alias{as.matrix,Vector-method}
\alias{as.matrix.Vector}
\alias{coerce,Vector,vector-method}
\alias{coerce,Vector,logical-method}
\alias{coerce,Vector,integer-method}
\alias{coerce,Vector,numeric-method}
\alias{coerce,Vector,double-method}
\alias{coerce,Vector,complex-method}
\alias{coerce,Vector,character-method}
\alias{coerce,Vector,raw-method}
\alias{coerce,Vector,factor-method}
\alias{coerce,Vector,data.frame-method}
\alias{as.data.frame.Vector}
\alias{as.data.frame,Vector-method}
\alias{as.env}
\alias{as.env,NULL-method}
\alias{as.env,Vector-method}
\alias{as.list.Vector}
\alias{as.list,Vector-method}

\alias{[}
\alias{[,Vector-method}
\alias{[<-,Vector-method}
\alias{replaceROWS,Vector,ANY-method}
\alias{mergeROWS,Vector,ANY-method}

\alias{subset}
\alias{subset.Vector}
\alias{subset,Vector-method}
\alias{window}
\alias{window.Vector}
\alias{window,Vector-method}
\alias{head}
\alias{head.Vector}
\alias{head,Vector-method}
\alias{tail}
\alias{tail.Vector}
\alias{tail,Vector-method}
\alias{rev}
\alias{rev.Vector}
\alias{rev,Vector-method}
\alias{rep}
\alias{rep,Vector-method}
\alias{rep.int}
\alias{rep.int,Vector-method}

\alias{summary.Vector}
\alias{summary,Vector-method}

\alias{bindROWS,Vector-method}
\alias{c}
\alias{c,Vector-method}
\alias{append}
\alias{append,Vector,Vector-method}

\alias{transform.Vector}
\alias{transform,Vector-method}

\alias{expand.grid}
\alias{expand.grid,Vector-method}

\alias{parallelVectorNames}
\alias{parallelVectorNames,ANY-method}

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
        is represented as a \link{DataFrame} object with a row for each
        element and a column for each metadata variable. Note that the
        element-wise metadata can also be \code{NULL}.
  }

  To be functional, a class that inherits from Vector must define at
  least a \code{length} and a \code{"["} method.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a Vector object.
  
  \describe{
    \item{\code{length(x)}:}{
      Get the number of elements in \code{x}.
    }
    \item{\code{lengths(x, use.names=TRUE)}:}{
      Get the length of each of the elements.

      Note: The \code{lengths} method for Vector objects is currently
      defined as an alias for \code{\link{elementNROWS}} (with addition
      of the \code{use.names} argument), so is equivalent to
      \code{sapply(x, NROW)}, not to \code{sapply(x, length)}.
    }
    \item{\code{NROW(x)}:}{
      Equivalent to either \code{nrow(x)} or \code{length(x)}, depending on
      whether \code{x} has dimensions (i.e. \code{dim(x)} is not \code{NULL})
      or not (i.e. \code{dim(x)} is \code{NULL}).
    }
    \item{\code{names(x)}, \code{names(x) <- value}:}{
      Get or set the names of the elements in the Vector.
    }
    \item{\code{rename(x, value, ...)}:}{
      Replace the names of \code{x} according to a mapping defined by a named
      character vector, formed by concatenating \code{value} with any
      arguments in \code{...}. The names of the character vector
      indicate the source names, and the corresponding values the
      destination names. This also works on a plain old \code{vector}.
    }
    \item{\code{unname(x)}:}{ removes the names from \code{x}, if any.
    }
    \item{\code{nlevels(x)}:}{
      Returns the number of factor levels.
    }
    \item{\code{mcols(x, use.names=TRUE)}, \code{mcols(x) <- value}:}{
      Get or set the metadata columns.
      If \code{use.names=TRUE} and the metadata columns are not \code{NULL},
      then the names of \code{x} are propagated as the row names of the
      returned \link{DataFrame} object.
      When setting the metadata columns, the supplied value must be \code{NULL}
      or a \link{DataFrame} object holding element-wise metadata.
    }
    \item{\code{elementMetadata(x, use.names=FALSE)},
       \code{elementMetadata(x) <- value},
       \code{values(x, use.names=FALSE)},
       \code{values(x) <- value}:}{
       Alternatives to \code{mcols} functions. Their use is discouraged.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{\code{as(from, "data.frame")}, \code{as.data.frame(from)}:}{
      Coerces \code{from}, a \code{Vector}, to a \code{data.frame} by
      first coercing the \code{Vector} to a \code{vector} via
      \code{as.vector}. Note that many \code{Vector} derivatives do not
      support \code{as.vector}, so this coercion is possible only for
      certain types.
    }
    \item{\code{as.env(x)}:}{
      Constructs an environment object containing the elements of
      \code{mcols(x)}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{\code{x[i]}:}{
      When supported, return a new Vector object of the same class as \code{x}
      made of the elements selected by \code{i}. \code{i} can be missing;
      an NA-free logical, numeric, or character vector or factor (as ordinary
      vector or \link{Rle} object); or a \link[IRanges]{IntegerRanges} object.
    }
    \item{\code{x[i, j]}:}{
      Like the above, but allow the user to conveniently subset the metadata
      columns thru \code{j}.

      NOTE TO DEVELOPERS: A Vector subclass with a true 2-D semantic (e.g.
      \link[SummarizedExperiment]{SummarizedExperiment}) needs to overwrite
      the \code{"["} method for Vector objects. This means that code intended
      to operate on an arbitrary Vector derivative \code{x} should not use
      this feature as there is no guarantee that \code{x} supports it. For
      this reason this feature should preferrably be used \emph{interactively}
      only.
    }
    \item{\code{x[i] <- value}:}{
      Replacement version of \code{x[i]}.
    }
  }
}

\section{Convenience wrappers for common subsetting operations}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{\code{subset(x, subset, select, drop=FALSE, ...)}:}{
      Return a new Vector object made of the subset using logical vector
      \code{subset}, where missing values are taken as FALSE.
      TODO: Document \code{select}, \code{drop}, and \code{...}.
    }
    \item{\code{window(x, start=NA, end=NA, width=NA)}:}{
      Extract the subsequence from \code{x} that corresponds to the window
      defined by \code{start}, \code{end}, and \code{width}.
      At most 2 of \code{start}, \code{end}, and \code{width} can be set
      to a non-\code{NA} value, which must be a non-negative integer.
      More precisely:
      \itemize{
        \item If \code{width} is set to \code{NA}, then \code{start} or
              \code{end} or both can be set to \code{NA}. In this case
              \code{start=NA} is equivalent to \code{start=1} and
              \code{end=NA} is equivalent to \code{end=length(x)}.
        \item If \code{width} is set to a non-negative integer value, then
              one of \code{start} or \code{end} must be set to a non-negative
              integer value and the other one to \code{NA}.
      }
    }
    \item{\code{head(x, n=6L)}:}{
      If \code{n} is non-negative, returns the first n elements of the Vector
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the Vector object.
    }
    \item{\code{tail(x, n=6L)}:}{
      If \code{n} is non-negative, returns the last n elements of the Vector
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the Vector object.
    }
    \item{\code{rev(x)}:}{
      Return a new Vector object made of the original elements in the reverse
      order.
    }
    \item{\code{rep(x, times, length.out, each)}:}{ and \code{rep.int(x, times)}:
      Repeats the values in \code{x} through one of the following conventions:
      \itemize{
        \item \code{times}: Vector giving the number of times to repeat each
              element if of length \code{length(x)}, or to repeat the whole
              vector if of length 1.
        \item \code{length.out}: Non-negative integer. The desired length of
              the output vector.
        \item \code{each}: Non-negative integer.  Each element of \code{x} is
              repeated \code{each} times.
      }
    }
  }
}

\section{Concatenation}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{\code{c(x, ..., ignore.mcols=FALSE)}:}{
      Concatenate \code{x} and the Vector objects in \code{...} together.
      Any object in \code{...} should belong to the same class as \code{x}
      or to one of its subclasses. If not, then an attempt will be made to
      coerce it with \code{as(object, class(x), strict=FALSE)}.
      \code{NULL}s are accepted and ignored.
      The result of the concatenation is an object of the same class
      as \code{x}.

      Handling of the metadata columns:
      \itemize{
        \item If only one of the Vector objects has metadata columns,
              then the corresponding metadata columns are attached to
              the other Vector objects and set to \code{NA}.
        \item When multiple Vector objects have their own metadata columns,
              the user must ensure that each such \link{DataFrame} have
              identical layouts to each other (same columns defined),
              in order for the concatenation to be successful, otherwise
              an error will be thrown.
        \item The user can call \code{c(x, ..., ignore.mcols=FALSE)} in
              order to concatenate Vector objects with differing sets of
              metadata columns, which will result in the concatenated
              object having NO metadata columns.
      }

      IMPORTANT NOTE: Be aware that calling \code{c} with named arguments
      (e.g. \code{c(a=x, b=y)}) tends to break method dispatch so please
      make sure that \code{args} is an \emph{unnamed} list when using
      \code{do.call(c, args)} to concatenate a list of objects together.
    }
    \item{\code{append(x, values, after=length(x))}:}{
      Insert the \code{Vector} \code{values} onto \code{x} at the position
      given by \code{after}. \code{values} must have an \code{elementType}
      that extends that of \code{x}.
    }
    \item{\code{expand.grid(...)}:}{ Find cartesian product of every
      vector in \code{\dots} and return a data.frame, each column of
      which corresponds to an argument.
      See \code{\link[base]{expand.grid}}.
    }
  }
}

\section{Displaying}{
  [FOR ADVANCED USERS OR DEVELOPERS]

  Displaying of a Vector object is controlled by 2 internal helpers,
  \code{classNameForDisplay} and \code{showAsCell}.

  For most objects \code{classNameForDisplay(x)} just returns \code{class(x)}.
  However, for some objects it can return the name of a parent class that is
  more suitable for display because it's simpler and as informative as the
  real class name. See \link{SimpleList} objects (defined in this package)
  and \link[IRanges]{CompressedList} objects (defined in the \pkg{IRanges}
  package) for examples of objects for which \code{classNameForDisplay}
  returns the name of a parent class.

  \code{showAsCell(x)} produces a character vector \emph{parallel} to
  \code{x} (i.e. with one string per vector element in \code{x}) that
  contains compact string representations of each elements in \code{x}.

  Note that \code{classNameForDisplay} and \code{showAsCell} are generic
  functions so developers can implement methods to control how their own
  Vector extension gets displayed.
}

\seealso{
  \itemize{
    \item \link{Rle}, \link{Hits}, \link[IRanges]{IRanges} and
          \link[XVector]{XRaw} for example implementations.

    \item \link{Vector-comparison} for comparing, ordering, and tabulating
          vector-like objects.

    \item \link{Vector-setops} for set operations on vector-like objects.

    \item \link{Vector-merge} for merging vector-like objects.

    \item \link{Factor} for a direct Vector extension that serves a similar
          role as \link[base]{factor} in base R.

    \item \link{List} for a direct Vector extension that serves a similar
          role as \link[base]{list} in base R.

    \item \link[IRanges]{extractList} for grouping elements of a vector-like
          object into a list-like object.

    \item \link{DataFrame} which is the type of object returned by the
          \code{mcols} accessor.

    \item The \link{Annotated} class, which Vector extends.
  }
}

\examples{
showClass("Vector")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
