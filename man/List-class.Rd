\name{List-class}
\docType{class}

% List class, functions and methods:
\alias{class:List}
\alias{List-class}
\alias{List}

\alias{class:list_OR_List}
\alias{list_OR_List-class}
\alias{list_OR_List}

\alias{elementType}
\alias{elementType,List-method}
\alias{elementType,vector-method}
\alias{elementNROWS}
\alias{elementNROWS,ANY-method}
\alias{elementNROWS,List-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}
\alias{isEmpty,List-method}
\alias{parallelVectorNames,List-method}

\alias{coerce,List,list-method}
\alias{coerce,ANY,List-method}
\alias{coerce,integer,List-method}
\alias{as.list,List-method}
\alias{unlist,List-method}
\alias{as.data.frame,List-method}

\alias{[,List-method}
\alias{[<-,List-method}
\alias{[[,List-method}
\alias{[[<-,List-method}
\alias{$,List-method}
\alias{$<-,List-method}
\alias{setListElement,List-method}
\alias{getListElement,List-method}

\alias{show,List-method}
\alias{showAsCell,List-method}

\title{List objects}

\description{
  List objects are \link{Vector} objects with a \code{"[["},
  \code{elementType} and \code{elementNROWS} method.
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
  List objects and derivatives are typically constructed using one of the
  following methods:

  \subsection{Use of a constructor function}{
    Many constructor functions are provided in \pkg{S4Vectors} and other
    Bioconductor packages for List objects and derivatives e.g.
    \code{List()}, \code{\link[IRanges]{IntegerList}()},
    \code{\link[IRanges]{RleList}()},
    \code{\link[IRanges]{IntegerRangesList}()},
    \code{\link[GenomicRanges]{GRangesList}()}, etc...

    Which one to use depends on the particular type of List derivative
    one wishes to construct e.g. use \code{\link[IRanges]{IntegerList}()}
    to get an \link[IRanges]{IntegerList} object,
    \code{\link[IRanges]{RleList}()} to get an \link[IRanges]{RleList}
    object, etc...

    Note that the name of a constructor function is always the name of
    a valid class. See the man page of a particular constructor function
    for the details.
  }

  \subsection{Coercion to List or to a List subclass}{
    Many coercion methods are defined in \pkg{S4Vectors} and other
    Bioconductor packages to turn all kinds of objects into List objects.

    One general and convenient way to convert any vector-like object
    \code{x} into a List is to call \code{as(x, "List")}. This will
    yield an object from a subclass of List. Note that this subclass
    will typically extend \link[IRanges]{CompressedList} but not necessarily
    (see \code{?\link[IRanges]{CompressedList}} in the \pkg{IRanges}
    package for more information about \link[IRanges]{CompressedList}
    objects).

    However, if a specific type of List derivative is desired (e.g.
    \link[GenomicRanges]{CompressedGRangesList}), then coercing
    explicitly to that class is preferrable as it is more robust
    and more readable.
  }

  \subsection{Use of \code{splitAsList()}, \code{relist()},
              or \code{extractList()}}{
    \code{\link{splitAsList}()} behaves like \code{base::split()}
    except that it returns a List derivative instead of an ordinary
    list. See \code{?\link{splitAsList}} for more information.

    The \code{\link[IRanges]{relist}()} methods for List objects and
    derivatives, as well as the \code{\link[IRanges]{extractList}()}
    function, are defined in the \pkg{IRanges} package.
    They provide very efficient ways to construct a List derivative from
    the vector-like object passed to their first argument (\code{flesh}
    for \code{relist()} and \code{x} for \code{extractList()}).
    See \code{?\link[IRanges]{extractList}} in the \pkg{IRanges}
    package for more information.
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a List object.

  \describe{
    \item{\code{length(x)}:}{
      Get the number of list elements in \code{x}.
    }
    \item{\code{names(x)}, \code{names(x) <- value}:}{
      Get or set the names of the elements in the List.
    }
    \item{\code{mcols(x, use.names=FALSE)}, \code{mcols(x) <- value}:}{
      Get or set the metadata columns. See \link{Vector} man page for
      more information.
    }
    \item{\code{elementType(x)}:}{
      Get the scalar string naming the class from which all elements must
      derive.
    }
    \item{\code{elementNROWS(x)}:}{
      Get the length (or nb of row for a matrix-like object) of each of
      the elements. Equivalent to \code{sapply(x, NROW)}.
    }
    \item{\code{isEmpty(x)}:}{
      Returns a logical indicating either if the sequence has no elements
      or if all its elements are empty.
    }
  }
}

\section{Coercion}{
  To List.
  \describe{
    \item{\code{as(x, "List")}:}{ Converts a vector-like object into a
      List, usually a \link[IRanges]{CompressedList} derivative.
      One notable exception is when \code{x} is an ordinary list,
      in which case \code{as(x, "List")} returns a \link{SimpleList}
      derivative.

      To explicitly request a \link{SimpleList} derivative, call
      \code{as(x, "SimpleList")}.

      See \code{?\link[IRanges]{CompressedList}} (you might need to load
      the \pkg{IRanges} package first) and \code{?\link{SimpleList}} for
      more information about the CompressedList and SimpleList
      representations.
    }
  }

  From List. In the code snippets below, \code{x} is a List object.
  \describe{
    \item{\code{as.list(x, ...)}, \code{as(from, "list")}:}{
      Turns \code{x} into an ordinary list.
    }
    \item{\code{unlist(x, recursive=TRUE, use.names=TRUE)}:}{
      Concatenates the elements of \code{x} into a single vector-like
      object (of class \code{elementType(x)}).
    }
    \item{\code{as.data.frame(x, row.names=NULL, optional=FALSE ,
                          value.name="value", use.outer.mcols=FALSE,
                          group_name.as.factor=FALSE, ...)}:}{
      Coerces a \code{List} to a \code{data.frame}. The result has the
      same length as unlisted \code{x} with two additional columns,
      \code{group} and \code{group_name}. \code{group} is an \code{integer}
      that indicates which list element the record came from.
      \code{group_name} holds the list name associated with each
      record; value is \code{character} by default and \code{factor} when
      \code{group_name.as.factor} is TRUE.

      When \code{use.outer.mcols} is TRUE the metadata columns on the
      outer list elements of \code{x} are replicated out and included
      in the \code{data.frame}. List objects that unlist to a
      single vector (column) are given the column name `value` by default.
      A custom name can be provided in \code{value.name}.

      Splitting values in the resulting \code{data.frame} by the original
      groups in \code{x} should be done using the \code{group} column as
      the \code{f} argument to \code{splitAsList}. To relist data, use
      \code{x} as the \code{skeleton} argument to \code{relist}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{\code{x[i]}:}{
      Return a new List object made of the list elements selected by
      subscript \code{i}. Subscript \code{i} can be of any type supported
      by subsetting of a Vector object (see \link{Vector} man page for the
      details), plus the following types: \link[IRanges]{IntegerList},
      \link[IRanges]{LogicalList}, \link[IRanges]{CharacterList},
      integer-\link[IRanges]{RleList}, logical-\link[IRanges]{RleList},
      character-\link[IRanges]{RleList}, and \link[IRanges]{IntegerRangesList}.
      Those additional types perform subsetting within the list elements
      rather than across them.
    }
    \item{\code{x[i] <- value}:}{
      Replacement version of \code{x[i]}.
    }
    \item{\code{x[[i]]}:}{
      Return the selected list element \code{i}, where \code{i} is an
      numeric or character vector of length 1.
    }
    \item{\code{x[[i]] <- value}:}{
      Replacement version of \code{x[[i]]}.
    }
    \item{\code{x$name}, \code{x$name <- value}:}{
      Similar to \code{x[[name]]} and \code{x[[name]] <- value}, but
      \code{name} is taken literally as an element name.
    }
  }
}

\author{P. Aboyoun and H. Pagès}

\seealso{
  \itemize{
    \item \link{splitAsList} for splitting a vector-like object into a
          List object.

    \item \link[IRanges]{relist} and \link[IRanges]{extractList} in the
          \pkg{IRanges} package for efficiently constructing a List
          derivative from a vector-like object.

    \item \link{List-utils} for common operations on List objects.

    \item \link{Vector} objects for the parent class.

    \item The \link{SimpleList} class for a direct extension of the List class.

    \item The \link[IRanges]{CompressedList} class defined in the
          \pkg{IRanges} package for another direct extension of the List class.

    \item The \link[IRanges]{IntegerList}, \link[IRanges]{RleList}, and
          \link[IRanges]{IRanges} classes and constructors defined in the
          \pkg{IRanges} package for some examples of List derivatives.
  }
}

\examples{
showClass("List")  # shows only the known subclasses define in this package

## ---------------------------------------------------------------------
## A. CONSTRUCTION
## ---------------------------------------------------------------------
x <- sample(500, 20)
y0 <- splitAsList(x, x \%\% 4)
y0

levels <- paste0("G", 1:10)
f1 <- factor(sample(levels, length(x), replace=TRUE), levels=levels)
y1 <- splitAsList(x, f1)
y1

f2 <- factor(sample(levels, 26, replace=TRUE), levels=levels)
y2 <- splitAsList(letters, f2)
y2

library(IRanges)  # for the NumericList() constructor and the
                  # coercion to CompressedCharacterList

NumericList(A=runif(10), B=NULL, C=runif(3))

## Another way to obtain 'splitAsList(letters, f2)' but using
## 'splitAsList()' should be preferred as it is a lot more efficient:
y2b <- as(split(letters, f2), "CompressedCharacterList")  # inefficient!
stopifnot(identical(y2, y2b))

## ---------------------------------------------------------------------
## B. SUBSETTING
## ---------------------------------------------------------------------
## Single-bracket and double-bracket subsetting behave like on ordinary
## lists:
y1[c(10, 1, 2, 2)]
y1[c(-10, -1, -2)]
y1[c(TRUE, FALSE)]
y1[c("G8", "G1")]
head(y1)
tail(y1, n=3)
y1[[2]]     # note the difference with y1[2]
y1[["G2"]]  # note the difference with y1["G2"]

y0[["3"]]
y0[[3]]

## In addition to all the forms of subscripting supported by ordinary
## lists, List objects and derivatives accept a subscript that is a
## list-like object. This form of subsetting is called "list-style
## subsetting":
i <- list(4:3, -2, 1)    # ordinary list
y1[i]
i <- y1 >= 200           # LogicalList object
y1[i]

## List-style subsetting also works with an RleList or IntegerRangesList
## subscript:
i <- RleList(y1 >= 200)  # RleList object
y1[i]
i <- IRangesList(RleList(y1 >= 200))  # IRangesList object
y1[i]

## ---------------------------------------------------------------------
## C. THE "UNLIST -> TRANFORM -> RELIST" IDIOM
## ---------------------------------------------------------------------
## The "unlist -> transform -> relist" idiom is a very efficient way to
## apply the same simple transformation to all the **inner elements** of
## a list-like object (i.e. to all the elements of its list elements).
## The result is another list-like object with the same shape as the
## original object (but not necessarily the same class):
relist(sqrt(unlist(y1)), y1)
relist(toupper(unlist(y2)), y2)

## However note that sqrt(), toupper(), and many other base functions,
## can be used directly on a List derivative. This is because the IRanges
## package defines methods for these functions that know how to handle
## List objects:
sqrt(y1)     # same as 'relist(sqrt(unlist(y1)), y1)'
toupper(y2)  # same as 'relist(toupper(unlist(y2)), y2)'
}

\keyword{methods}
\keyword{classes}
