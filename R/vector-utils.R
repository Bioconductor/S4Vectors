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
### Concatenation
###

### Exported!
### Works on atomic vectors, factors, lists, matrices, and data frames.
### Arguments 'ignore.mcols' and 'check' are ignored.
.concatenate_vectors <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")

    ## We do not call prepare_objects_to_concatenate() because we do not want
    ## to force all the objects in 'objects' to be of the type of 'x'. This
    ## way we are consistent with what c() and unlist() do when combining
    ## atomic vectors of mixed types.
    objects <- lapply(unname(objects),
        function(object)
            if (is(object, "Rle")) decodeRle(object) else object)
    all_objects <- c(list(x), objects)

    if (length(dim(x)) == 2L) {
        ans <- do.call(rbind, all_objects)
        if (!use.names)
            rownames(ans) <- NULL
    } else {
        ans <- unlist(all_objects, recursive=FALSE)
        if (!use.names)
            names(ans) <- NULL
    }
    ans
}

setMethod("concatenateObjects", "vector", .concatenate_vectors)
setMethod("concatenateObjects", "matrix", .concatenate_vectors)

### Assumes that 'x' is a list of length >= 1 with no names, and that the
### list elements in 'x' have the same type. This is NOT checked!
### TODO: quick_unlist() is superseded by concatenateObjects(). Search code
### for use of quick_unlist() and replace with use of concatenateObjects().
### Then remove quick_unlist() definition below.
quick_unlist <- function(x)
{
    x1 <- x[[1L]]
    if (is.factor(x1)) {
        ## Fast unlisting of a list of factors that all have the same levels
        ## in the same order.
        structure(unlist(x), class="factor", levels=levels(x1))
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

