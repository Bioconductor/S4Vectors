### =========================================================================
### Merging vector-like objects
### -------------------------------------------------------------------------
###


### Compute the n-ary union (if 'all' is TRUE) or n-ary intersection (if 'all'
### is FALSE) of a list of vector-like objects with no metadata columns.
### The objects must support c() for the n-ary union (i.e. when 'all' is TRUE),
### and %in% and [ for the n-ary intersection (i.e. when 'all' is FALSE).
### They must also support sort() if 'sort' is TRUE, as well as unique().
.merge_naked_objects <- function(naked_objects,
                                 all=FALSE, all.x=NA, all.y=NA,
                                 sort=TRUE)
{
    if (!isTRUEorFALSE(all))
        stop("'all' must be TRUE or FALSE")
    if (!(is.logical(all.x) && length(all.x) == 1L))
        stop("'all.x' must be a single logical")
    if (!(is.logical(all.y) && length(all.y) == 1L))
        stop("'all.y' must be a single logical")
    if (!isTRUEorFALSE(sort))
        stop("'sort' must be TRUE or FALSE")

    if (length(naked_objects) == 1L) {
        ## Unary union or intersection.
        ## 'all', 'all.x', and 'all.y' are ignored.
        ans <- naked_objects[[1L]]
    } else if (length(naked_objects) == 2L) {
        ## Binary union or intersection.
        ## Behavior is controlled by 'all.x' and 'all.y' (after setting each
        ## of them to 'all' if it's NA).
        if (is.na(all.x))
            all.x <- all
        if (is.na(all.y))
            all.y <- all
        x <- naked_objects[[1L]]
        y <- naked_objects[[2L]]
        if (all.x && all.y) {
            ans <- c(x, y)
        } else if (all.x) {
            ans <- x
        } else if (all.y) {
            ans <- y
        } else {
            ans <- x[x %in% y]
        }
    } else {
        ## N-ary union or intersection (N > 2).
        ## 'all.x' and 'all.y' must be NAs.
        if (!(is.na(all.x) && is.na(all.y)))
            stop(wmsg("You need to use 'all' instead of the 'all.x' or ",
                      "'all.y' argument when merging more than 2 objects."))
        if (all) {
            ans <- do.call("c", naked_objects)
        } else {
            ans <- naked_objects[[1L]]
            for (i in 2:length(naked_objects))
                ans <- ans[ans %in% naked_objects[[i]]]
        }
    }
    if (sort)
        ans <- sort(ans)
    unique(ans)
}

### The list can contain NULLs, which are ignored. Non-NULL list elements are
### assumed to be of same lengths. This is not checked.
.collapse_list_of_equal_vectors <- function(x, colname)
{
    x <- x[!sapply_isNULL(x)]
    ans <- x[[1L]]
    if (length(x) >= 2L) {
        na_idx <- which(is.na(ans))
        for (i in 2:length(x)) {
            x_elt <- x[[i]]
            if (is.null(x_elt))
                next
            if (!all(x_elt == ans, na.rm=TRUE))
                stop(wmsg("metadata column \"", colname, "\" contains ",
                          "incompatible values across the objects to merge"))
            if (length(na_idx) != 0L) {
                ans[na_idx] <- x_elt[na_idx]
                na_idx <- which(is.na(ans))
            }
        }
    }
    ans
}

.merge_mcols <- function(x, objects)
{
    all_mcolnames <- unique(unlist(
        lapply(objects, function(object) colnames(mcols(object)))
    ))

    revmaps <- lapply(objects, match, x=x)

    merge_mcol <- function(colname) {
        cols <- mapply(
            function(object, revmap) {
                col <- mcols(object)[[colname]]
                if (is.null(col))
                    return(NULL)
                col <- col[revmap]
            },
            objects, revmaps,
            SIMPLIFY=FALSE
        )
        .collapse_list_of_equal_vectors(cols, colname)
    }

    all_mcols <- lapply(setNames(all_mcolnames, all_mcolnames), merge_mcol)
    DataFrame(all_mcols)
}

### 'objects' must be a list of vector-like objects. See .merge_naked_objects()
### above for what operations these objects must support in order for
### .merge_Vector_objects() to work.
.merge_Vector_objects <- function(objects,
                                  all=FALSE, all.x=NA, all.y=NA,
                                  sort=TRUE)
{
    objects <- unname(objects)
    naked_objects <- lapply(objects,
        function(object) {
            mcols(object) <- NULL
            if (any(duplicated(object)))
                ## We don't actually apply unique() to the input objects but
                ## .merge_Vector_objects() behaves like if we did.
                warning(wmsg("Some of the objects to merge contain ",
                             "duplicated elements. These elements were ",
                             "removed by applying unique() to each object ",
                             "before the merging."))
            object
        }
    )
    ans <- .merge_naked_objects(naked_objects,
                                all=all, all.x=all.x, all.y=all.y,
                                sort=sort)
    mcols(ans) <- .merge_mcols(ans, objects)
    ans
}

### 3 important differences with base::merge.data.frame():
###   1) The matching is based on the vector values (vs arbitrary columns for
###      base::merge.data.frame()).
###   2) Self merge is a no-op if 'sort=FALSE' (or object already sorted) and
###      if the object has no duplicates.
###   3) This an n-ary merge() of vector-like objects (vs binary for
###      base::merge.data.frame()).
setMethod("merge", c("Vector", "Vector"),
    function(x, y, ..., all=FALSE, all.x=NA, all.y=NA, sort=TRUE)
    {
        if (missing(x)) {
            if (missing(y)) {
                objects <- list(...)
            } else {
                objects <- list(y, ...)
            }
        } else {
            if (missing(y)) {
                objects <- list(x, ...)
            } else {
                objects <- list(x, y, ...)
            }
        }
        .merge_Vector_objects(objects,
                              all=all, all.x=all.x, all.y=all.y,
                              sort=sort)
    }
)

