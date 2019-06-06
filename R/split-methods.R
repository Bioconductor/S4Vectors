### =========================================================================
### Split a vector-like object as a list-like object
### -------------------------------------------------------------------------


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

