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
### bindROWS2()
###
### A thin wrapper around bindROWS().
###
### NOT exported.
###
### If all the objects passed to bindROWS() have the same type, the result of
### the binding will be an object of that type (endomorphism). But when the
### objects passed to bindROWS() have mixed types, what bindROWS() will return
### exactly is a little unpredictable.
### The purpose of the wrapper below is to improve handling of mixed type
### objects by pre-processing some of them before calling bindROWS().
### More precisely, it tries to work around the 2 following problems that
### direct use of bindROWS() on mixed type objects would pose:
###  1) When the objects to bind are a mix of ordinary lists and other
###     list-like objects like IntegerList, the type of the object returned
###     by bindROWS() depends on the type of the 1st object. To avoid this
###     undesirable effect, if one object to bind is an ordinary list then
###     we pass all objects that are not ordinary lists thru as.list().
###  2) When the objects to bind are a mix of Rle and non-Rle objects,
###     the type of the object returned by bindROWS() also depends on the
###     type of the 1st object. More precisely it's an Rle if and only if
###     objects[[1]] is an Rle. The wrapper below **mitigate** this by
###     decoding the Rle objects first. Note that this is a mitigation
###     process only. For example it will help if Rle objects are mixed
###     with atomic vectors or factors, but it won't help if objects[[1]]
###     is an Rle and the other objects are IntegerList objects.
###  3) When the objects to bind are a mix of atomic vectors and factors,
###     bindROWS() would **always** return an atomic vector (whatever
###     objects[[1]] is, i.e. atomic vector or factor). However we **always**
###     want a factor. This is an intended deviation with respect to what
###     rbind() does when concatenating the the columns of ordinary data
###     frames where the 1st data frame passed to rbind() dictates what the
###     result is going to be (i.e. a column in the result will be atomic
###     vector or factor depending on what the corresponding column in the
###     1st data frame is).
bindROWS2 <- function(x, objects=list())
{
    all_objects <- c(list(x), objects)
    is_list <- vapply(all_objects, is.list, logical(1L))
    if (any(is_list)) {
        coerce_idx <- which(!is_list)
        if (length(coerce_idx) != 0L)
            all_objects[coerce_idx] <- lapply(all_objects[coerce_idx], as.list)
    } else {
        is_Rle <- vapply(all_objects, is, logical(1L), "Rle")
        if (any(is_Rle) && !all(is_Rle))
            all_objects[is_Rle] <- lapply(all_objects[is_Rle], decodeRle)
        is_factor <- vapply(all_objects, is.factor, logical(1L))
        if (any(is_factor)) {
            all_objects[!is_factor] <- lapply(all_objects[!is_factor],
                function(object) {
                    object <- as.character(object)
                    factor(object, levels=unique(object))
                })
            all_levels <- unique(unlist(lapply(all_objects, levels),
                                        use.names=FALSE))
            all_objects <- lapply(all_objects, factor, levels=all_levels)
        }
    }
    nonempty_idx <- which(vapply(all_objects, NROW, integer(1L)) != 0L)
    if (length(nonempty_idx) == 0L)
        return(all_objects[[1L]])
    all_objects <- all_objects[nonempty_idx]
    if (length(all_objects) == 1L)
        return(all_objects[[1L]])
    bindROWS(all_objects[[1L]], all_objects[-1L])
}


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

