### =========================================================================
### Combining DataFrame objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .combine_DFrame_rows()
###
### The workhorse behind the rbind() and combineRows() methods for DataFrame
### objects.
###

### 'all_colnames' must be a list of colnames vectors (character vectors).
### Returns an integer matrix describing how each colnames vector aligns to
### the "aggregated colnames". The "aggregated colnames" is the vector of
### colnames obtained by taking the duplicates-preserving union of all the
### colnames vectors (e.g. the aggregation of c("a", "b", "c", "b") and
### c("e", "b", "e", "a", "a") is c("a", "b", "c", "b", "e", "e", "a")).
### The returned matrix has one row per colnames vector and one column
### per "aggregated colname". The "aggregated colnames" are set as the
### colnames of the matrix.
### If 'strict.colnames' is TRUE, all the supplied colnames vectors must be
### the same (modulo the order of their elements). An error is raised if
### they are not.
### If 'strict.colnames' is FALSE, the returned matrix can contain NAs.
.aggregate_and_align_all_colnames <- function(all_colnames,
                                              strict.colnames=FALSE,
                                              what="DFrame objects")
{
    stopifnot(is.list(all_colnames),
              length(all_colnames) >= 1L,
              isTRUEorFALSE(strict.colnames))
    ans <- matrix(seq_along(all_colnames[[1L]]), nrow=1L,
                  dimnames=list(NULL, all_colnames[[1L]]))
    for (colnames in all_colnames[-1L]) {
        colnames_hits <- findMatches(colnames, colnames(ans))
        colnames_map <- selectHits(colnames_hits, select="first", nodup=TRUE)
        unmapped_idx <- which(is.na(colnames_map))
        if (strict.colnames) {
            if (length(colnames) != ncol(ans) || length(unmapped_idx) != 0L)
                stop(wmsg("the ", what, " to combine ",
                          "must have the same column names"))
        }
        mapped_idx <- which(!is.na(colnames_map))
        colnames_revmap <- rep.int(NA_integer_, ncol(ans))
        colnames_revmap[colnames_map[mapped_idx]] <- mapped_idx
        ans <- rbind(ans, matrix(colnames_revmap, nrow=1L))
        if (length(unmapped_idx) != 0L) {
            m <- matrix(NA_integer_,
                        nrow=nrow(ans)-1L, ncol=length(unmapped_idx),
                        dimnames=list(NULL, colnames[unmapped_idx]))
            m <- rbind(m, matrix(unmapped_idx, nrow=1L))
            ans <- cbind(ans, m)
        }
    }
    ans
}

### 'x' must be a DFrame object or derivative.
### Behaves like an endomorphism with respect to 'x' i.e. returns an object
### of the same class as 'x'.
### NOT exported.
.combine_DFrame_rows <- function(x, objects=list(), strict.colnames=FALSE,
                                                    use.names=TRUE, check=TRUE)
{
    if (!is(x, "DFrame"))
        stop(wmsg("the objects to combine must be ",
                  "DFrame objects or derivatives"))
    if (!isTRUEorFALSE(strict.colnames))
        stop(wmsg("'strict.colnames' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(use.names))
        stop(wmsg("'use.names' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(check))
        stop(wmsg("'check' must be TRUE or FALSE"))

    objects <- prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)
    all_nrows <- unlist(lapply(all_objects, nrow), use.names=FALSE)
    all_colnames <- lapply(all_objects, colnames)
    colmap <- .aggregate_and_align_all_colnames(all_colnames,
                                                strict.colnames=strict.colnames)
    ## Unfortunately there seems to be no way to put colnames on a 0-col
    ## matrix. So when the 'colmap' matrix has 0 cols, 'colnames(colmap)'
    ## will always be NULL, even though we'd like it to be 'character(0)'.
    if (ncol(colmap) == 0L) {
        ans_colnames <- character(0)
    } else {
        ans_colnames <- colnames(colmap)
    }

    ## Compute 'ans_listData'.
    ans_listData <- lapply(setNames(seq_along(ans_colnames), ans_colnames),
        function(j) {
            all_cols <- lapply(seq_along(all_objects),
                function(i) {
                    j2 <- colmap[i, j]
                    if (is.na(j2)) {
                        Rle(NA, all_nrows[[i]])
                    } else {
                        all_objects[[i]][[j2]]
                    }
                }
            )
            tryCatch(
                bindROWS2(all_cols[[1L]], all_cols[-1L]),
                error=function(err) {
                    stop(wmsg("failed to rbind column '", ans_colnames[[j]],
                              "' across DataFrame objects:\n  ",
                              conditionMessage(err)))
                }
            )
        }
    )

    ## Compute 'ans_nrow'.
    ans_nrow <- sum(all_nrows)

    ## Compute 'ans_rownames'.
    if (use.names) {
        ## Bind the rownames.
        ans_rownames <- unlist(lapply(all_objects, rownames), use.names=FALSE)
        if (!is.null(ans_rownames)) {
            if (length(ans_rownames) != ans_nrow) {
                ## What we do here is surprising and inconsistent with
                ## ordinary data frames.
                ## TODO: Maybe reconsider this?
                ans_rownames <- NULL  # why?
            }
        }
    } else {
        ans_rownames <- NULL
    }

    ## Create 'x0', a 0-row DataFrame derivative of the same class as 'x'
    ## but with all the additional columns that result from the combining
    ## operation. Also the original metadata columns on 'x' must propagate
    ## to 'x0'.
    x0 <- extractROWS(x, integer(0))
    if (length(ans_colnames) > ncol(x0)) {
        ## It doesn't really matter what value we use here as long it's of
        ## length zero.
        dummy_col <- normalizeSingleBracketReplacementValue(logical(0), x0)
        i <- (ncol(x0)+1L):length(ans_colnames)
        ## If 'x0' carries metadata columns, 'replaceCOLS()' will take care
        ## of extending them by appending NA-filled rows to 'mcols(x0)'.
        ## The workhorse behind this process is also '.combine_DFrame_rows()'.
        ## Also note that we don't care about the colnames of the object
        ## returned by this call to 'replaceCOLS()' because they're going
        ## to be ignored anyways.
        x0 <- replaceCOLS(x0, i, value=dummy_col)
    }
    ## Sanity check. Should never fail.
    stopifnot(ncol(x0) == length(ans_colnames))
    ## The only reason we created 'x0' is so that we can use it here to
    ## propagate its class and metadata columns.
    BiocGenerics:::replaceSlots(x0, listData=ans_listData,
                                    nrows=ans_nrow,
                                    rownames=ans_rownames,
                                    check=check)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rbind()
###

### Ignore the 'ignore.mcols' argument!
.bindROWS_DFrame_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    all_objects <- c(list(x), objects)
    has_rows <- vapply(all_objects, nrow, integer(1L), USE.NAMES=FALSE) > 0L
    has_cols <- vapply(all_objects, ncol, integer(1L), USE.NAMES=FALSE) > 0L
    if (!any(has_rows)) {
        if (!any(has_cols))
            return(x)
        return(all_objects[[which(has_cols)[[1L]]]])
    }
    all_objects <- all_objects[has_rows]
    x <- all_objects[[1L]]
    if (!is(x, "DFrame"))
        x <- as(x, "DFrame")
    objects <- all_objects[-1L]
    .combine_DFrame_rows(x, objects,
                         strict.colnames=TRUE,
                         use.names=use.names, check=check)
}

### Defining bindROWS() gives us rbind().
### FIXME: Note that .bindROWS_DFrame_objects() doesn't work on DataFrame
### objects in general but only on those that are DFrame objects or
### derivatives. So this method should really be defined for DFrame
### objects, not for DataFrame objects.
setMethod("bindROWS", "DataFrame", .bindROWS_DFrame_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### combineRows()
###

setMethod("combineRows", "DataFrame",
    function(x, ...)
    {
        objects <- list(...)
        .combine_DFrame_rows(x, objects, strict.colnames=FALSE)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### combineCols() by Aaron Lun
###

.combine_DFrame_cols <- function(all_df, use.names=TRUE) {
    # Either all DFs have rownames, or no DFs have rownames. 
    if (use.names) {
        all_names <- lapply(all_df, rownames)

        checkNames <- function(x) {
            !is.null(x) && anyDuplicated(x)==0L
        }
        if (!all(vapply(all_names, checkNames, TRUE))) {
            stop(wmsg("DataFrames must have non-NULL, non-duplicated rownames when 'use.names=TRUE'"))
        }

        common <- Reduce(union, all_names)
        all_df <- lapply(all_df, function(x) {
            out <- x[common,,drop=FALSE]
            rownames(out) <- common
            out
        })

    } else {
        out <- vapply(all_df, nrow, 0L)
        if (length(unique(out))!=1L) {
            stop(wmsg("DataFrames must have same number of rows when 'use.names=FALSE'"))
        }
    }

    do.call(cbind, all_df)
}

setMethod("combineCols", "DataFrame",
    function(x, ..., use.names=TRUE)
    {
        all_df <- list(x, ...)
        .combine_DFrame_cols(all_df, use.names=use.names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cbind()
###

### S3/S4 combo for cbind.DataFrame
cbind.DataFrame <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the cbind() method for DataFrame objects ",
                     "ignores the 'deparse.level' argument"))
    ## It's important that the call to DataFrame() below is able to deparse
    ## the arguments in ... so for example
    ##   b <- 11:13
    ##   selectMethod("cbind", "DataFrame")(b)
    ## returns a DataFrame with a column named "b".
    ## This prevents us from calling DataFrame() via do.call() e.g. we can't
    ## do something like
    ##   objects <- delete_NULLs(list(...))
    ##   do.call(DataFrame, c(objects, list(check.names=FALSE)))
    ## because then DataFrame() wouldn't be able to deparse what was in ...
    ## and selectMethod("cbind", "DataFrame")(b) would produce a DataFrame
    ## with a column named "11:13".
    DataFrame(..., check.names=FALSE)
}
setMethod("cbind", "DataFrame", cbind.DataFrame)

### If we didn't define this method, calling c() on DataFrame objects would
### call the "c" method for Vector objects, which just delegates to bindROWS()
### so the binding would happen along the rows. This is not what we want so we
### overwrite the "c" method for Vector objects with a method that binds along
### the columns.
setMethod("c", "DataFrame",
    function(x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!isTRUEorFALSE(ignore.mcols))
            stop("'ignore.mcols' must be TRUE or FALSE")
        if (!identical(recursive, FALSE))
            stop(wmsg("\"c\" method for DataFrame objects ",
                      "does not support the 'recursive' argument"))
        objects <- unname(delete_NULLs(list(x, ...)))
        ans <- do.call(cbind, objects)
        if (ignore.mcols)
            mcols(ans) <- NULL
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rbind2() and cbind2()
###
### H.P. 4/1/2021: Do we need these methods? For what? They're not even
### exported!
###

setMethod("rbind2", c("ANY", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  rbind(x, y, ...)
})
setMethod("rbind2", c("DataFrame", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})
setMethod("rbind2", c("DataFrame", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})
setMethod("cbind2", c("ANY", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  cbind(x, y, ...)
})
setMethod("cbind2", c("DataFrame", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
})
setMethod("cbind2", c("DataFrame", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### merge()
###

.mergeByHits <- function(x, y, by, all.x=FALSE, all.y=FALSE, sort = TRUE,
                         suffixes = c(".x", ".y"))
{
    nm.x <- colnames(x)
    nm.y <- colnames(y)
    cnm <- nm.x %in% nm.y
    if (any(cnm) && nzchar(suffixes[1L]))
        nm.x[cnm] <- paste0(nm.x[cnm], suffixes[1L])
    cnm <- nm.y %in% nm.x
    if (any(cnm) && nzchar(suffixes[2L]))
        nm.y[cnm] <- paste0(nm.y[cnm], suffixes[2L])

    if (all.x) {
        x.alone <- which(countLnodeHits(by) == 0L)
    }
    x <- x[c(from(by), if (all.x) x.alone), , drop = FALSE]
    if (all.y) {
        y.alone <- which(countRnodeHits(by) == 0L)
        xa <- x[rep.int(NA_integer_, length(y.alone)), , drop = FALSE]
        x <- rbind(x, xa)
    }
    y <- y[c(to(by), if (all.x) rep.int(NA_integer_, length(x.alone)),
             if (all.y) y.alone), , drop = FALSE]

    cbind(x, y)
}

setMethod("merge", c("DataFrame", "DataFrame"), function(x, y, by, ...) {
    if (is(by, "Hits")) {
        return(.mergeByHits(x, y, by, ...))
    }
    as(merge(as(x, "data.frame"), as(y, "data.frame"), by, ...), class(x))
})

setMethod("merge", c("data.frame", "DataFrame"), function(x, y, ...) {
  as(merge(x, as(y, "data.frame"), ...), class(y))
})

setMethod("merge", c("DataFrame", "data.frame"), function(x, y, ...) {
  as(merge(as(x, "data.frame"), y, ...), class(x))
})

