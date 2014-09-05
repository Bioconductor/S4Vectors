\name{List-class}
\docType{class}

% List class, functions and methods:
\alias{class:List}
\alias{List-class}
\alias{List}

\alias{elementType}
\alias{elementType,List-method}
\alias{elementType,vector-method}
\alias{elementLengths}
\alias{elementLengths,ANY-method}
\alias{elementLengths,List-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}
\alias{isEmpty,List-method}

\alias{show,List-method}

\alias{[[,List-method}
\alias{[[<-,List-method}
\alias{$,List-method}
\alias{$<-,List-method}
\alias{[,List-method}
\alias{[<-,List-method}

\alias{coerce,List,list-method}
\alias{as.list.List}
\alias{coerce,ANY,List-method}
\alias{coerce,integer,List-method}
\alias{as.list,List-method}
\alias{as.env,List-method}
\alias{unlist,List-method}
\alias{as.data.frame.List}
\alias{as.data.frame,List-method}
\alias{coerce,List,data.frame-method}


\title{List objects}

\description{
  List objects are \link{Vector} objects with a \code{"[["},
  \code{elementType} and \code{elementLengths} method.
  The List class serves a similar role as \link[base]{list} in base R.

  It adds one slot, the \code{elementType} slot, to the two slots shared by
  all \link{Vector} objects.

  The \code{elementType} slot is the preferred location for List
  subclasses to store the type of data represented in the sequence. It is
  designed to take a character of length 1 representing the class of the
  sequence elements. While the List class performs no validity checking
  based on \code{elementType}, if a subclass expects elements to be of a
  given type, that subclass is expected to perform the necessary validity
  checking. For example, the subclass \link[IRanges]{IntegerList} (defined
  in the \pkg{IRanges} package) has \code{elementType = "integer"} and its
  validity method checks if this condition is TRUE.

  To be functional, a class that inherits from List must define at least
  a \code{"[["} method (in addition to the minimum set of \link{Vector}
  methods).
}

\section{Construction}{
  List objects are typically constructed using one of the 3 following methods:
  \itemize{
    \item Use of a constructor function. Many constructor functions are
          provided for List objects e.g. \code{List},
          \code{\link[IRanges]{IntegerList}}, \code{\link[IRanges]{RleList}},
          \code{\link[IRanges]{RangesList}},
          \code{\link[GenomicRanges]{GRangesList}}, etc...
          Which one to use depends on the particular type of List object
          to construct. The name of a constructor function is always
          the name of a valid class. If it's the name of a \emph{concrete}
          class (e.g. the \code{\link[GenomicRanges]{GRangesList}} constructor
          defined in the \pkg{GenomicRanges} package), then the constructor
          function returns an instance of that class. If it's the name of a
          \emph{virtual} class (e.g. the \code{List} constructor defined in
          this package, or the \code{\link[IRanges]{IntegerList}} or
          \code{\link[IRanges]{RleList}} or \code{\link[IRanges]{RangesList}}
          constructors defined in the \pkg{IRanges} package), then the returned
          object belongs to a concrete subclass of that virtual class. Which
          subclass exactly depends on each constructor function (see man page
          of a particular constructor function for the details).

    \item Coercion to List or to a List subclass. Many coercion methods are
          provided to turn any object into a List object. One general and
          convenient way to convert any vector-like object into a List is to
          call \code{as(x, "List")}. This will typically yield an object from
          a subclass of \link[IRanges]{CompressedList}.

    \item Use of \code{\link[IRanges]{extractList}}. This function, defined
          in the \pkg{IRanges} package, extracts user-specified groups of
          elements from a vector-like object and returns them in a List (or
          sometimes list) object.
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a List object.
  
  \describe{
    \item{}{
      \code{elementType(x)}:
      Get the scalar string naming the class from which all elements must
      derive.
    }
    \item{}{
      \code{elementLengths(x)}:
      Get the length (or nb of row for a matrix-like object) of each of
      the elements. Equivalent to \code{sapply(x, NROW)}.
    }
    \item{}{
      \code{isEmpty(x)}:
      Returns a logical indicating either if the sequence has no elements
      or if all its elements are empty.
    }
  }
}

\section{Element extraction (list style)}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{}{
      \code{x[[i]]}:
      If defined, return the selected element \code{i}, where \code{i} is an
      numeric or character vector of length 1.
    }
    \item{}{
      \code{x$name}:
      Similar to \code{x[[name]]}, but \code{name} is taken literally as an
      element name.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an environment from \code{x} with a symbol for each
      \code{names(x)}. The values are not actually copied into the
      environment. Rather, they are dynamically bound using
      \code{\link{makeActiveBinding}}. This prevents unnecessary copying
      of the data from the external vectors into R vectors. The values
      are cached, so that the data is not copied every time the symbol
      is accessed.
    }
    \item{}{
      \code{as.list(x, ...)}, \code{as(from, "list")}:
      Turns \code{x} into a standard list.
    }
    \item{}{\code{unlist(x, recursive = TRUE, use.names = TRUE)}: Concatenates
      the elements of \code{x} into a single \code{elementType(x)} object.
    }
    \item{}{\code{as.data.frame(x, row.names = NULL, optional = FALSE ,
      value.name = "value", use.outer.mcols = FALSE, 
      group_name.as.factor = FALSE, ...)}: 
      Coerces a \code{List} to a \code{data.frame}. The result has the
      same length as unlisted \code{x} with two additional columns,
      \code{group} and \code{group_name}. \code{group} is an \code{integer}
      that indicates which list element the record came from. 
      \code{group_name} holds the list name associated with each
      record; value is \code{character} by default and \code{factor} when 
      \code{group_name.as.factor} is TRUE. 

      When \code{use.outer.mcols} is TRUE the metadata columns on the 
      outer list elements of \code{x} are replicated out and included 
      in the \code{data.frame}. \code{List} objects that unlist to a 
      single vector (column) are given the column name `value` by default.
      A custom name can be provided in \code{value.name}.

      Splitting values in the resulting \code{data.frame} by the original 
      groups in \code{x} should be done using the \code{group} column as 
      the \code{f} argument to \code{splitAsList}. To relist data, use
      \code{x} as the \code{skeleton} argument to \code{relist}.
    }
  }
}

\author{P. Aboyoun and H. Pages}

\seealso{
  \itemize{
    \item \link{Vector} objects for the parent class.

    \item The \link[IRanges]{SimpleList} and \link[IRanges]{CompressedList}
          classes defined in the \pkg{IRanges} package for direct extensions
          of the \link{List} class.

    \item The \link[IRanges]{IntegerList}, \link[IRanges]{RleList}, and
          \link[IRanges]{IRanges} classes and constructors defined in the
          \pkg{IRanges} package for examples of concrete \link{List}
          subclasses.

    \item The \link[IRanges]{extractList} function defined in the
          \pkg{IRanges} package for grouping elements of a vector-like
          object into a list-like object.

    \item \link{List-utils} for common operations on List objects.
  }
}

\examples{
showClass("List")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
