combine_df <- function(x, y, ...) {
    all_df <- list(x, y, ...)

    colnames_list <- lapply(all_df, colnames)
    all_colnames <- unique(unlist(colnames_list, use.names = FALSE))

    fillCols <- function(df) {
        if (nrow(df))
            df[setdiff(all_colnames, colnames(df))] <- NA
        df
    }

    do.call(rbind, lapply(all_df, fillCols))
}

setMethod("combineRows", "DataFrame", combine_df)

setMethod("combineCols", "DataFrame", function(x, y, ..., use.names=TRUE) {
    all_df <- list(x, y, ...)

    # Either all DFs have names, or no DFs have names. 
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
})
