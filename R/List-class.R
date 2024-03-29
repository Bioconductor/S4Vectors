### =========================================================================
### List objects
### -------------------------------------------------------------------------
###
### List objects are Vector objects with "[[", "elementType" and
### "elementNROWS" methods.
###

setClass("List",
    contains="Vector",
    representation(
        "VIRTUAL",
        elementType="character"
    ),
    prototype(elementType="ANY")
)

setClassUnion("list_OR_List", c("list", "List"))  # list-like objects


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods
###

setGeneric("elementType", function(x, ...) standardGeneric("elementType"))
setMethod("elementType", "List", function(x) x@elementType)
setMethod("elementType", "vector", function(x) storage.mode(x))

setGeneric("elementNROWS", function(x) standardGeneric("elementNROWS"))

setMethod("elementNROWS", "ANY", sapply_NROW)

### Used in the SGSeq package!
quick_togroup <- function(x) map_inner_ROWS_to_list_elements(elementNROWS(x))

setMethod("elementNROWS", "List",
    function(x)
    {
        y <- as.list(x)
        if (length(y) == 0L) {
            ans <- integer(0)
            ## We must return a named integer(0) if 'x' is named
            names(ans) <- names(x)
            return(ans)
        }
        if (length(dim(y[[1L]])) < 2L)
            return(elementNROWS(y))
        return(sapply(y, NROW))
    }
)

setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
setMethod("isEmpty", "ANY",
          function(x)
          {
              if (is.atomic(x))
                  return(length(x) == 0L)
              if (!is(x, "list_OR_List"))
                  stop("isEmpty() is not defined for objects of class ",
                       class(x)[[1L]])
              ## Recursive definition
              if (length(x) == 0L)
                  return(TRUE)
              all(vapply(x, isEmpty, logical(1L)))
          })
### A List object is considered empty iff all its elements are empty.
setMethod("isEmpty", "List", function(x) all(elementNROWS(x) == 0L))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

List <- function(...)
{
    args <- list(...)
    if (length(args) == 1L && is.list(args[[1L]])) 
        args <- args[[1L]]
    as(args, "List")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("show", "List",
          function(object)
          {
              lo <- length(object)
              cat(classNameForDisplay(object), " of length ", lo,
                  "\n", sep = "")
              if (!is.null(names(object)))
                cat(labeledLine("names", names(object)))
          })

setMethod("showAsCell", "List", showAsCell_list)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unlist()
###

### 'inner_names' and 'outer_names' can be either NULL or character vectors.
### If both are character vectors, then they must have the same length.
.make_unlisted_names <- function(inner_names, outer_names)
{
    if (is.null(outer_names))
        return(inner_names)
    if (is.null(inner_names))
        return(outer_names)
    ## Replace missing outer names with inner names.
    no_outer <- is.na(outer_names) | outer_names == ""
    if (any(no_outer)) {
        idx <- which(no_outer)
        outer_names[idx] <- inner_names[idx]
    }
    ## Paste *outer* and *inner* names together when both are present.
    no_inner <- is.na(inner_names) | inner_names == ""
    both <- !(no_outer | no_inner)
    if (any(both)) {
        idx <- which(both)
        outer_names[idx] <- paste(outer_names[idx], inner_names[idx], sep=".")
    }
    outer_names
}

### 'unlisted_x' is assumed to have the *inner* names of 'x' on it.
set_unlisted_names <- function(unlisted_x, x)
{
    x_names <- names(x)
    if (is.null(x_names))
        return(unlisted_x)
    inner_names <- ROWNAMES(unlisted_x)
    outer_names <- rep.int(x_names, elementNROWS(x))
    unlisted_names <- .make_unlisted_names(inner_names, outer_names)
    if (length(dim(unlisted_x)) < 2L) {
        res <- try(names(unlisted_x) <- unlisted_names, silent=TRUE)
        what <- "names"
    } else {
        res <- try(rownames(unlisted_x) <- unlisted_names, silent=TRUE)
        what <- "rownames"
    }
    if (is(res, "try-error"))
        warning("failed to set ", what, " on the ",
                "unlisted ", class(x)[[1L]], " object")
    unlisted_x
}

### If 'use.names' is FALSE or 'x' has no *outer* names, then we propagate
### the *inner* names on the unlisted object. Note that this deviates from
### base::unlist() which doesn't propagate any names (outer or inner) when
### 'use.names' is FALSE.
### Otherwise (i.e. if 'use.names' is TRUE and 'x' has *outer* names), the
### names we propagate are obtained by pasting the *outer* and *inner* names
### together. Note that, unlike base::unlist(), we never mangle the *outer*
### names when they have no corresponding *inner* names (a terrible feature
### of base::unlist()).
setMethod("unlist", "List",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (length(x) == 0L) {
            elt_type <- elementType(x)
            if (isVirtualClass(elt_type))
                return(NULL)
            return(new(elt_type))
        }
        xx <- unname(as.list(x))
        if (length(dim(xx[[1L]])) < 2L) {
            ## This propagates the *inner* names of 'x'.
            unlisted_x <- do.call(c, xx)
        } else {
            ## This propagates the *inner* names of 'x'.
            unlisted_x <- do.call(rbind, xx)
        }
        if (use.names)
            unlisted_x <- set_unlisted_names(unlisted_x, x)
        unlisted_x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Returns TRUE iff 'i' contains positive values that are compatible
### with the shape of 'x'. NAs are allowed.
.is_valid_NL_subscript <- function(i, x)
{
    unlisted_i <- unlist(i, use.names=FALSE)
    if (!is.integer(unlisted_i))
        unlisted_i <- as.integer(unlisted_i)
    if (any(unlisted_i < 1L, na.rm=TRUE))
        return(FALSE)
    x_eltNROWS <- elementNROWS(x)
    i_eltNROWS <- elementNROWS(i)
    if (any(unlisted_i > rep.int(x_eltNROWS, i_eltNROWS), na.rm=TRUE))
        return(FALSE)
    return(TRUE)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Returns the name of one of the 3 supported fast paths ("LL", "NL", "RL")
### or NA if no fast path can be used.
.select_fast_path <- function(i, x)
{
    ## LEPType (List Element Pseudo-Type): same as "elementType" except for
    ## RleList objects.
    if (is(i, "RleList")) {
        i_runvals <- runValue(i)
        i_LEPType <- elementType(i_runvals)
    } else {
        i_LEPType <- elementType(i)
    }
    if (extends(i_LEPType, "logical")) {
        ## 'i' is a List of logical vectors or logical-Rle objects.
        ## We select the "LL" fast path ("Logical List").
        return("LL")
    }
    if (extends(i_LEPType, "numeric")) {
        ## 'i' is a List of numeric vectors or numeric-Rle objects.
        if (is(i, "RleList")) {
            i2 <- i_runvals
        } else {
            i2 <- i
        }
        if (.is_valid_NL_subscript(i2, x)) {
            ## We select the "NL" fast path ("Number List").
            return("NL")
        }
    }
    if (extends(i_LEPType, "IntegerRanges")) {
        ## 'i' is a List of IntegerRanges objects.
        ## We select the "RL" fast path ("IntegerRanges List").
        return("RL")
    }
    return(NA_character_)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Truncate or recycle each list element of 'i' to the length of the
### corresponding element in 'x'.
.adjust_elt_lengths <- function(i, x)
{
    x_eltNROWS <- unname(elementNROWS(x))
    i_eltNROWS <- unname(elementNROWS(i))
    idx <- which(x_eltNROWS != i_eltNROWS)
    ## FIXME: This is rough and doesn't follow exactly the truncate-or-recycle
    ## semantic of normalizeSingleBracketSubscript() on a logical vector or
    ## logical-Rle object.
    for (k in idx)
        i[[k]] <- rep(i[[k]], length.out=x_eltNROWS[k])
    return(i)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of logical vectors or logical-Rle objects.
.unlist_LL_subscript <- function(i, x)
{
    i <- .adjust_elt_lengths(i, x)
    unlist(i, use.names=FALSE)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of numeric vectors or numeric-Rle objects.
.unlist_NL_subscript <- function(i, x)
{
    offsets <- c(0L, end(IRanges::PartitioningByEnd(x))[-length(x)])
    i <- i + offsets
    unlist(i, use.names=FALSE)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of IntegerRanges objects.
.unlist_RL_subscript <- function(i, x)
{
    unlisted_i <- unlist(i, use.names=FALSE)
    offsets <- c(0L, end(IRanges::PartitioningByEnd(x))[-length(x)])
    IRanges::shift(unlisted_i, shift=rep.int(offsets, elementNROWS(i)))
}

### Fast subset by List of logical vectors or logical-Rle objects.
### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Propagate 'names(x)' only. Caller is responsible for propagating 'mcols(x)'
### and 'metadata(x)'.
.fast_subset_List_by_LL <- function(x, i)
{
    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_LL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    group <- rep.int(seq_along(x), elementNROWS(x))
    group <- extractROWS(group, unlisted_i)
    ans_partitioning <- IRanges::PartitioningByEnd(group, NG=length(x),
                                                   names=names(x))
    relist(unlisted_ans, ans_partitioning)
}

### Fast subset by List of numeric vectors or numeric-Rle objects.
### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Propagate 'names(x)' only. Caller is responsible for propagating 'mcols(x)'
### and 'metadata(x)'.
.fast_subset_List_by_NL <- function(x, i)
{
    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_NL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    ans_breakpoints <- cumsum(unname(elementNROWS(i)))
    ans_partitioning <- IRanges::PartitioningByEnd(ans_breakpoints,
                                                   names=names(x))
    relist(unlisted_ans, ans_partitioning)
}

### Fast subset by List of IntegerRanges objects.
### Assume 'x' and 'i' are parallel List objects (i.e. same length).
### Propagate 'names(x)' only. Caller is responsible for propagating 'mcols(x)'
### and 'metadata(x)'.
.fast_subset_List_by_RL <- function(x, i)
{
    i_eltNROWS <- elementNROWS(i)
    if (all(i_eltNROWS == 1L)) {
        unlisted_i <- unlist(i, use.names=FALSE)
        return(IRanges::windows(x, unlisted_i))
    }

    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_RL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    ans_breakpoints <- cumsum(unlist(sum(width(i)), use.names=FALSE))
    ans_partitioning <- IRanges::PartitioningByEnd(ans_breakpoints,
                                                   names=names(x))
    relist(unlisted_ans, ans_partitioning)
}

### Subset a List object by a list-like subscript.
subset_List_by_List <- function(x, i)
{
    li <- length(i)
    if (is.null(names(i))) {
        lx <- length(x)
        if (li > lx)
            stop("list-like subscript is longer than ",
                 "list-like object to subset")
        if (li < lx)
            x <- x[seq_len(li)]
    } else {
        if (is.null(names(x)))
            stop("cannot subscript an unnamed list-like object ",
                 "by a named list-like object")
        if (!identical(names(i), names(x))) {
            i2x <- match(names(i), names(x))
            if (anyMissing(i2x))
                stop("list-like subscript has names not in ",
                     "list-like object to subset")
            x <- x[i2x]
        }
    }
    ## From here, 'x' and 'i' are guaranteed to have the same length.
    if (li == 0L)
        return(x)
    if (!is(x, "SimpleList")) {
        ## We'll try to take a fast path.
        if (is(i, "List")) {
            fast_path <- .select_fast_path(i, x)
        } else {
            i2 <- as(i, "List")
            i2_elttype <- elementType(i2)
            if (length(i2) == li && all(sapply(i, is, i2_elttype))) {
                fast_path <- .select_fast_path(i2, x)
                if (!is.na(fast_path))
                    i <- i2
            } else {
                fast_path <- NA_character_
            }
        }
        if (!is.na(fast_path)) {
            fast_path_FUN <- switch(fast_path,
                                    LL=.fast_subset_List_by_LL,
                                    NL=.fast_subset_List_by_NL,
                                    RL=.fast_subset_List_by_RL)
            ans <- as(fast_path_FUN(x, i), class(x))  # fast path
            ## Propagate 'metadata(x)' and 'mcols(x)'.
            metadata(ans) <- metadata(x)
            mcols(ans) <- mcols(x, use.names=FALSE)
            return(ans)
        }
    }
    ## Slow path (loops over the list elements of 'x').
    for (k in seq_len(li))
        x[[k]] <- extractROWS(x[[k]], i[[k]])
    return(x)
}

.adjust_value_length <- function(value, i_len)
{
    value_len <- length(value)
    if (value_len == i_len)
        return(value)
    if (i_len %% value_len != 0L)
        warning("number of values supplied is not a sub-multiple ",
                "of the number of values to be replaced")
    rep(value, length.out=i_len)
}

### Assume 'x' and 'i' are parallel List objects (i.e. same length).
.fast_lsubset_List_by_List <- function(x, i, value)
{
    ## Unlist 'x', 'i', and 'value'.
    unlisted_x <- unlist(x, use.names=FALSE)
    fast_path <- .select_fast_path(i, x)
    unlist_subscript_FUN <- switch(fast_path,
                                   LL=.unlist_LL_subscript,
                                   NL=.unlist_NL_subscript,
                                   RL=.unlist_RL_subscript)
    unlisted_i <- unlist_subscript_FUN(i, x)
    if (length(value) != 1L) {
        value <- .adjust_value_length(value, length(i))
        value <- .adjust_elt_lengths(value, i)
    }
    unlisted_value <- unlist(value, use.names=FALSE)

    ## Subset.
    unlisted_ans <- replaceROWS(unlisted_x, unlisted_i, unlisted_value)

    ## Relist.
    ans <- as(relist(unlisted_ans, x), class(x))
    metadata(ans) <- metadata(x)
    ans
}

lsubset_List_by_List <- function(x, i, value)
{
    lx <- length(x)
    li <- length(i)
    if (li == 0L) {
        ## Surprisingly, in that case, `[<-` on standard vectors does not
        ## even look at 'value'. So neither do we...
        return(x)
    }
    lv <- length(value)
    if (lv == 0L)
        stop("replacement has length zero")
    value <- normalizeSingleBracketReplacementValue(value, x)
    if (is.null(names(i))) {
        if (li != lx)
            stop("when list-like subscript is unnamed, it must have the ",
                 "length of list-like object to subset")
        if (!is(x, "SimpleList")) {
            ## We'll try to take a fast path.
            if (is(i, "List")) {
                fast_path <- .select_fast_path(i, x)
            } else {
                i2 <- as(i, "List")
                i2_elttype <- elementType(i2)
                if (length(i2) == li && all(sapply(i, is, i2_elttype))) {
                    fast_path <- .select_fast_path(i2, x)
                    if (!is.na(fast_path))
                        i <- i2
                } else {
                    fast_path <- NA_character_
                }
            }
            if (!is.na(fast_path))
                return(.fast_lsubset_List_by_List(x, i, value))  # fast path
        }
        i2x <- seq_len(li)
    } else {
        if (is.null(names(x)))
            stop("cannot subset an unnamed list-like object ",
                 "by a named list-like subscript")
        i2x <- match(names(i), names(x))
        if (anyMissing(i2x))
            stop("list-like subscript has names not in ",
                 "list-like object to subset")
        if (anyDuplicated(i2x))
            stop("list-like subscript has duplicated names")
    }
    value <- .adjust_value_length(value, li)
    ## Slow path (loops over the list elements of 'x').
    for (k in seq_len(li))
        x[[i2x[k]]] <- replaceROWS(x[[i2x[k]]], i[[k]], value[[k]])
    return(x)
}

setMethod("[", "List",
    function(x, i, j, ..., drop=TRUE)
    {
        if (length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i) || !is(i, "list_OR_List") || is(i, "IntegerRanges")) {
            ans <- subset_along_ROWS(x, i, drop=drop)
        } else {
            ans <- subset_List_by_List(x, i)
        }
        if (!missing(j))
            mcols(ans) <- mcols(ans, use.names=FALSE)[ , j, drop=FALSE]
        ans
    }
)

setReplaceMethod("[", "List",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (!missing(i) && is(i, "list_OR_List") && !is(i, "IntegerRanges"))
            return(lsubset_List_by_List(x, i, value))
        callNextMethod(x, i, value=value)
    }
)

setMethod("[[", "List",
    function(x, i, j, ...)
    {
        dotArgs <- list(...)
        if (length(dotArgs) > 0L)
            dotArgs <- dotArgs[names(dotArgs) != "exact"]
        if (!missing(j) || length(dotArgs) > 0L)
            stop("incorrect number of subscripts")
        ## '...' is either empty or contains only the 'exact' arg.
        getListElement(x, i, ...)
    }
)

setMethod("$", "List", function(x, name) x[[name, exact=FALSE]])

setReplaceMethod("[[", "List",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid replacement")
        setListElement(x, i, value)
    }
)

setReplaceMethod("$", "List",
                 function(x, name, value) {
                   x[[name]] <- value
                   x
                 })

setMethod("setListElement", "List", setListElement_default)

setMethod("getListElement", "List",
          function(x, i) getListElement(as.list(x), i))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("List", "list", function(from) as.list(from))

.as.list.List <- function(x, use.names=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ans <- lapply(x, identity)
    if (!use.names)
        names(ans) <- NULL
    ans
}
setMethod("as.list", "List", .as.list.List)

setMethod("parallelVectorNames", "List",
          function(x) setdiff(callNextMethod(), c("group", "group_name")))

listClassName <- function(impl, element.type) {
  if (is.null(impl))
    impl <- ""
  listClass <- paste0(impl, "List")
  if (!is.null(element.type)) {
    cl <- c(element.type, names(getClass(element.type)@contains))
    cl <- capitalize(cl)
    listClass <- c(paste0(cl, "List"), paste0(cl, "Set"),
                   paste0(impl, cl, "List"), listClass)
  }
  clExists <- which(sapply(listClass, isClass) &
                    sapply(listClass, extends, paste0(impl, "List")))
  listClass[[clExists[[1L]]]]
}

setAs("ANY", "List", function(from) {
  ## since list is directed to SimpleList, we assume 'from' is non-list-like
  relist(from, IRanges::PartitioningByEnd(seq_along(from), names=names(from)))
})

## Special cased, because integer extends ANY (somehow) and numeric,
## so ambiguities are introduced due to method caching.
setAs("integer", "List", getMethod(coerce, c("ANY", "List")))

.make_group_and_group_name <- function(x_eltNROWS, group_name.as.factor=FALSE)
{
    if (!isTRUEorFALSE(group_name.as.factor))
        stop("'group_name.as.factor' must be TRUE or FALSE")
    group <- rep.int(seq_along(x_eltNROWS), x_eltNROWS)
    x_names <- names(x_eltNROWS)
    if (is.null(x_names)) {
        group_name <- rep.int(NA_character_, length(group))
        if (group_name.as.factor)
            group_name <- factor(group_name, levels=character(0))
    } else {
        group_name <- rep.int(x_names, x_eltNROWS)
        if (group_name.as.factor)
            group_name <- factor(group_name, levels=unique(x_names))
    }
    data.frame(group=group, group_name=group_name, stringsAsFactors=FALSE)
}

.as.data.frame.List <- 
    function(x, row.names=NULL, optional=FALSE, ..., value.name="value",
             use.outer.mcols=FALSE, group_name.as.factor=FALSE)
{
    if (!isSingleString(value.name))
        stop("'value.name' must be a single string")
    if (!isTRUEorFALSE(use.outer.mcols))
        stop("'use.outer.mcols' must be TRUE or FALSE")
    ans <- as.data.frame(unlist(x, use.names=FALSE),
                         row.names=row.names, optional=optional, ...)
    if (ncol(ans) == 1L)
        colnames(ans)[1L] <- value.name
    group_and_group_name <- .make_group_and_group_name(elementNROWS(x),
                                                       group_name.as.factor)
    ans <- cbind(group_and_group_name, ans)
    if (use.outer.mcols) {
        x_mcols <- mcols(x, use.names=FALSE)
        if (length(x_mcols) != 0L) {
            extra_cols <- as.data.frame(x_mcols)
            extra_cols <- extract_data_frame_rows(extra_cols, ans[[1L]])
            ans <- cbind(ans, extra_cols)
        }
    }
    ans
}
setMethod("as.data.frame", "List", .as.data.frame.List)

