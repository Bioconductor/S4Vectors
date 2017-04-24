### =========================================================================
### Split a vector-like object as a list-like object
### -------------------------------------------------------------------------


setMethod("split", c("list", "Vector"),
    function(x, f, drop=FALSE, ...) split(x, as.vector(f), drop=drop, ...)
)

### The remaining methods delegate to IRanges::splitAsList().

setMethod("split", c("Vector", "ANY"),
    function(x, f, drop=FALSE)
    {
        if (!requireNamespace("IRanges", quietly=TRUE))
            stop("Couldn't load the IRanges package. You need to install ",
                 "the IRanges\n  package in order to split a Vector object.")
        IRanges::splitAsList(x, f, drop=drop)
    }
)

setMethod("split", c("ANY", "Vector"),
    function(x, f, drop=FALSE)
    {
        if (!requireNamespace("IRanges", quietly=TRUE))
            stop("Couldn't load the IRanges package. You need to install ",
                 "the IRanges\n  package in order to split by a Vector object.")
        IRanges::splitAsList(x, f, drop=drop)
    }
)

setMethod("split", c("Vector", "Vector"),
    function(x, f, drop=FALSE)
    {
        if (!requireNamespace("IRanges", quietly=TRUE))
            stop("Couldn't load the IRanges package. You need to install ",
                 "the IRanges\n  package in order to split a Vector object.")
        IRanges::splitAsList(x, f, drop=drop)
    }
)

