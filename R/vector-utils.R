### =========================================================================
### Some low-level (not exported) utility functions to operate on ordinary
### vectors (including lists and data frames)
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
###


last_or <- function(x, or)
{
    x_len <- length(x)
    if (x_len != 0L) x[[x_len]] else or
}

### TODO: Maybe implement sapply_isNULL in C? Also maybe
### Implement (in C) fast 'elementIs(objects, class)' that does
###
###     sapply(objects, is, class, USE.NAMES=FALSE)
###
### and use it here. 'elementIs(objects, "NULL")' should work and be
### equivalent to 'sapply_isNULL(objects)'.
sapply_isNULL <- function(objects)
    vapply(objects, is.null, logical(1), USE.NAMES=FALSE)

### TODO: Maybe implement this in C?
delete_NULLs <- function(objects)
{
    NULL_idx <- which(sapply_isNULL(objects))
    if (length(NULL_idx) != 0L)
        objects <- objects[-NULL_idx]
    objects
}

sapply_NROW <- function(x)
{
    if (!is.list(x))
        x <- as.list(x)
    ans <- try(.Call2("sapply_NROW", x, PACKAGE="S4Vectors"), silent=TRUE)
    if (!inherits(ans, "try-error")) {
        names(ans) <- names(x)
        return(ans)
    }
    ## From here, 'length(x)' is guaranteed to be != 0
    return(vapply(x, NROW, integer(1)))
}

### Return the common ancestor class **among** the classes of the list elements
### in 'x', or "ANY". In other words, if all the classes in 'x' extend one of
### them, then lowestListElementClass() returns it. Otherwise, it returns "ANY".
### As a consequence, lowestListElementClass() is guaranteed to always return a
### **concrete** class or "ANY".
###
### For example:
###
###   classes in 'x'              lowestListElementClass
###   -------------------------   ----------------------
###   all the same                common class
###   integer,numeric             "numeric"
###   integer,factor              "integer"
###   numeric,factor              "numeric"
###   integer,numeric,character   "ANY"
###   character,factor            "ANY"
###   matrix, data.frame          "ANY"
###   character,list              "ANY"
###
lowestListElementClass <- function(x)
{
    stopifnot(is.list(x))
    if (length(x) == 0L)
        return("ANY")
    all_classes <- unique(vapply(x, function(x_elt) class(x_elt)[[1L]],
                                 character(1), USE.NAMES=FALSE))
    nclasses <- length(all_classes)
    if (nclasses == 1L)
        return(all_classes)
    ## If all the classes in 'all_classes' have a common ancestor **among**
    ## 'all_classes', then return it. Otherwise return "ANY".
    ans <- all_classes[[1L]]
    for (i in 2:nclasses) {
        class <- all_classes[[i]]
        if (extends(class, ans))
            next
        if (!extends(ans, class))
            return("ANY")
        ans <- class
    }
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### map_inner_ROWS_to_list_elements()
###

map_inner_ROWS_to_list_elements <- function(NROWS, as.factor=FALSE)
{
    stopifnot(is.integer(NROWS), isTRUEorFALSE(as.factor))
    groups <- seq_along(NROWS)
    ans <- rep.int(groups, NROWS)
    if (as.factor)
        ans <- structure(ans, levels=as.character(groups), class="factor")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation along the ROWS
###

### Exported!
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

setMethod("bindROWS", "ANY", .default_bindROWS)

### Assumes that 'x' is a list of length >= 1 with no names, and that the
### list elements in 'x' have the same type. This is NOT checked!
### TODO: quick_unlist() is superseded by bindROWS(). Search code
### for use of quick_unlist() and replace with use of bindROWS().
### Then remove quick_unlist() definition below.
quick_unlist <- function(x)
{
    x1 <- x[[1L]]
    if (is.factor(x1)) {
        ## Fast unlisting of a list of factors that all have the same levels
        ## in the same order.
        structure(unlist(x), levels=levels(x1), class="factor")
    } else {
        do.call(c, x)  # doesn't work on list of factors
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### quick_unsplit()
###
### Assumes that 'x' is a list of length >= 1 with no names, and that the
### list elements in 'x' have the same type. This is NOT checked!
###

quick_unsplit <- function(x, f)
{
    idx <- split(seq_along(f), f)
    idx <- unlist(idx, use.names=FALSE)
    revidx <- integer(length(idx))
    revidx[idx] <- seq_along(idx)
    quick_unlist(x)[revidx]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_data_frame_rows()
###
### A fast version of {df <- df[i, , drop=FALSE]; rownames(df) <- NULL}.
### Can be up to 20x or 30x faster when extracting millions of rows.
### What kills [.data.frame is the overhead of propagating the original
### rownames and trying to keep them unique with make.unique(). However, most
### of the time, nobody cares about the rownames so this effort is pointless
### and only a waste of time.
###

extract_data_frame_rows <- function(x, i)
{
    stopifnot(is.data.frame(x))
    ## The commented code should be as fast (or even faster, because 'i' is
    ## normalized only once) as the code below but unfortunately it's not.
    ## TODO: Investigate why and make it as fast as the code below.
    #i <- normalizeSingleBracketSubscript(i, x, exact=FALSE, as.NSBS=TRUE)
    #ans <- lapply(x, extractROWS, i)
    i <- normalizeSingleBracketSubscript(i, x, exact=FALSE)
    ans <- lapply(x, "[", i)
    ## Do NOT use data.frame() or as.data.frame() here as it adds a lot of
    ## overhead and will mess up non-atomic columns.
    #data.frame(ans, check.names=FALSE, stringsAsFactors=FALSE)
    attr(ans, "row.names") <- seq_along(i)
    attr(ans, "class") <- "data.frame"
    ans
}

