### =========================================================================
### Split a vector-like object as a list-like object
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### relistToClass()
###
### 'relistToClass(x)' is the opposite of 'elementType(y)' in the sense that
### the former returns the class of the result of relisting (or splitting)
### 'x' while the latter returns the class of the result of unlisting (or
### unsplitting) 'y'.
###
### More formally, if 'x' is an object that is relistable and 'y' a list-like
### object:
###    relistToClass(x) == class(relist(x, some_skeleton))
###    elementType(y) == class(unlist(y))
###
### Therefore, for any object 'x' for which relistToClass() is defined
### and returns a valid class, 'elementType(new(relistToClass(x)))' should
### return 'class(x)'.
###

setGeneric("relistToClass", function(x) standardGeneric("relistToClass"))

.selectListClassName <- function(x) {
  cn <- listClassName("Compressed", x)
  if (cn == "CompressedList")
    cn <- listClassName("Simple", x)
  cn
}

setMethod("relistToClass", "ANY", function(x) .selectListClassName(class(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### splitAsList()
###

setGeneric("splitAsList", signature=c("x", "f"),
    function(x, f, drop=FALSE, ...) standardGeneric("splitAsList")
)

### The default splitAsList() method is actually implemented in the
### IRanges package.
setMethod("splitAsList", c("ANY", "ANY"),
    function(x, f, drop=FALSE)
    {
        if (!requireNamespace("IRanges", quietly=TRUE))
            stop(wmsg("Couldn't load the IRanges package. Please install ",
                      "the IRanges package before you try splitting ",
                      "a Vector derivative."))
        IRanges:::default_splitAsList(x, f, drop=drop)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### split()
###

### Delegate to splitAsList().
setMethods("split", list(c("Vector", "ANY"),
                         c("ANY", "Vector"),
                         c("Vector", "Vector")),
    function(x, f, drop=FALSE, ...) splitAsList(x, f, drop=drop, ...)
)

setMethod("split", c("list", "Vector"),
    function(x, f, drop=FALSE, ...) split(x, as.vector(f), drop=drop, ...)
)

