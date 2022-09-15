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
### If the class to return is an S4 class, make sure to return it with
### its "package" attribute!

.unique_classes <- function(classes)
{
    stopifnot(is.list(classes))

    classnames <- vapply(classes,
        function(cl) cl[[1L]],
        character(1), USE.NAMES=FALSE)

    pkgs <- vapply(classes,
        function(cl) {
            pkg <- attr(cl, "package")
            if (is.null(pkg)) "" else pkg
        }, character(1), USE.NAMES=FALSE)

    is_dup <- duplicatedIntegerPairs(selfmatch(classnames),
                                     selfmatch(pkgs))
    classes[!is_dup]
}

lowestListElementClass <- function(objects)
{
    stopifnot(is.list(objects))

    if (length(objects) == 0L)
        return("ANY")

    all_classes <- lapply(objects,
        function(object) {
            cl <- class(object)
            if (is.null(attr(cl, "package"))) cl[[1L]] else cl
        })
    unique_classes <- .unique_classes(all_classes)
    n <- length(unique_classes)
    if (n == 1L)
        return(unique_classes[[1L]])

    ## Find common ancestor candidate.
    ans <- unique_classes[[1L]]
    for (i in 2:n) {
        cl <- unique_classes[[i]]
        if (extends(ans, cl))
            ans <- cl
    }

    ## Do all classes extend 'ans'?
    for (i in seq_len(n)) {
        cl <- unique_classes[[i]]
        if (!extends(cl, ans))
            return("ANY")  # no
    }

    ans  # yes!
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
### quick_unlist()
###
### Assumes that 'x' is a list of length >= 1 with no names, and that the
### list elements in 'x' have the same type. This is NOT checked!
### TODO: quick_unlist() is superseded by bindROWS(). Search code
### for use of quick_unlist() and replace with use of bindROWS().
### Then remove quick_unlist() definition below.
###

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
### and a waste of time.
###

extract_data_frame_rows <- function(x, i)
{
    stopifnot(is.data.frame(x))
    i <- normalizeSingleBracketSubscript(i, x, exact=FALSE, allow.NAs=TRUE)
    ans <- lapply(x, "[", i)
    ## Do NOT use data.frame() or as.data.frame() here as it adds a lot of
    ## overhead and will mess up non-atomic columns.
    #data.frame(ans, check.names=FALSE, stringsAsFactors=FALSE)
    attr(ans, "row.names") <- seq_along(i)
    attr(ans, "class") <- "data.frame"
    ans
}

