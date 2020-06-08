### =========================================================================
### Combine objects by ROWS or COLS
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### bindROWS()
###
### A low-level generic function for binding objects along their 1st dimension.
### It is intended to be the workhorse behind:
### - the rbind() methods for rectangular objects (e.g. RectangularData
###   derivatives);
### - the c() methods for vector-like objects that are not data-frame-like
###   objects (e.g. Vector derivatives that are not DataFrame derivatives);
### - the unlist() methods for list-like objects (e.g. List derivatives).
###

setGeneric("bindROWS", signature="x",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
        standardGeneric("bindROWS")
)

### NOT exported.
### Low-level utility used by various bindROWS() and bindCOLS() methods.
### Prepare 'objects' by deleting NULLs from it, dropping its names, and
### making sure that each of its list element belongs to the same class
### as 'x' (or to one of its subclasses) by coercing it if necessary.
prepare_objects_to_bind <- function(x, objects=list())
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    lapply(unname(delete_NULLs(objects)), coerce2, x)
}

setMethod("bindROWS", "NULL",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
    {
        if (!is.list(objects))
            stop("'objects' must be a list")
        objects <- delete_NULLs(objects)
        if (length(objects) == 0L)
            return(NULL)
        x <- objects[[1L]]
        objects <- objects[-1L]
        callGeneric()
    }
)

### Works on atomic vectors, factors, lists, 1D arrays, matrices, and
### data frames. Arguments 'ignore.mcols' and 'check' are ignored.
.default_bindROWS <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")

    ## We do not call prepare_objects_to_bind() because we do not want
    ## to force all the objects in 'objects' to be of the type of 'x'. This
    ## way we are consistent with what c() and unlist() do when combining
    ## atomic vectors of mixed types.
    objects <- lapply(unname(objects),
        function(object)
            if (is(object, "Rle")) decodeRle(object) else object)
    all_objects <- c(list(x), objects)

    x_ndim <- length(dim(x))
    if (x_ndim == 0L) {
        ## Use unlist() if 'x' is an atomic vector, a factor, or a list.
        ## Otherwise use c().
        if (is.vector(x) || is.factor(x)) {
            ans <- unlist(all_objects, recursive=FALSE)
        } else {
            ans <- do.call(c, all_objects)
        }
        if (!use.names)
            names(ans) <- NULL
    } else if (x_ndim == 1L) {
        ## 'x' is a 1D array.
        ## base::rbind() is broken on 1D arrays so we need to handle this
        ## specially.
        ## Note that all objects in 'objects' are also treated as if they
        ## were 1D arrays (even if they have >= 2 dimensions). This is
        ## probably too laxist!
        ans <- unlist(all_objects, recursive=FALSE)
        if (use.names)
            ans_rownames <- names(ans)
        dim(ans) <- length(ans)  # this drops the names
        if (use.names)
            rownames(ans) <- ans_rownames
    } else if (x_ndim == 2L) {
        ## 'x' is a matrix or data frame.
        ans <- do.call(rbind, all_objects)
        if (!use.names)
            rownames(ans) <- NULL
    } else {
        ## 'x' is an array with more than 2 dimensions.
        ## Binding multi-dimensional arrays along the rows is exactly what
        ## the DelayedArray::arbind() generic does so we should probably move
        ## this generic to S4Vectors (or to BiocGenerics?).
        stop(wmsg("bindROWS() does not support arrays ",
                  "with more than 2 dimensions yet"))
    }
    ans
}

### Even though is(x, "vector") and is.vector(x) are FALSE when 'x'
### is a data frame, calling bindROWS() on 'x' will actually dispatch
### on the bindROWS,vector method (this can be checked with
### selectMethod("bindROWS", "data.frame")) so we don't need to
### define a bindROWS,data.frame method.
#setMethod("bindROWS", "vector", .default_bindROWS)

### Even though calling bindROWS() on an array would dispatch on the
### bindROWS,vector method (is(x, "vector") is TRUE) we still need to
### define the bindROWS,array method. Otherwise, the dispatch mechanism
### seems to remove the dim attribute from 'x' **before** passing it to
### the bindROWS,vector method.
### See https://stat.ethz.ch/pipermail/r-devel/2018-May/076205.html for
### the bug report.
#setMethod("bindROWS", "array", .default_bindROWS)

### In the end, all the above trouble can be avoided by simply defining
### this method.
setMethod("bindROWS", "ANY", .default_bindROWS)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### bindCOLS()
###
### A low-level generic function for binding objects along their 2nd dimension.
### It is intended to be the workhorse behind:
### - the cbind() methods for rectangular objects (e.g. RectangularData
###   derivatives);
### - the c() method for data-frame-like objects (e.g. DataFrame derivatives).
###

setGeneric("bindCOLS", signature="x",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
        standardGeneric("bindCOLS")
)

