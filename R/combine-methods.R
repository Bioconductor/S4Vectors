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

# This breaks the combineCols contract by consolidating data together from
# the same column name, such that the ncol() of combineCols's output is no
# longer equal to the sum of the ncols() of its inputs. However, it's very
# convenient for avoiding 2-3 copies of the same column during the merge,
# so we still provide it. Note that we call combineCols so that we handle
# other DataFrame representations that might have their own methods.
combineUniqueCols <- function(x, y, ..., use.names=TRUE) {
    combined <- combineCols(x, y, ..., use.names=use.names)
    combined <- combined[,!duplicated(colnames(combined)),drop=FALSE]

    all_df <- list(x, y, ...)
    shared <- lapply(all_df, colnames)
    indices <- rep(seq_along(shared), lengths(shared))
    by.colname <- split(indices, unlist(shared))
    dupped <- names(by.colname)[lengths(by.colname) > 1]

    for (d in dupped) {
        affected <- by.colname[[d]]
        reference <- combined[[d]]

        if (use.names) {
            filled <- rownames(combined) %in% rownames(all_df[[affected[1]]])

            for (i in affected[-1]) {
                cur_df <- all_df[[i]]
                candidates <- match(rownames(cur_df), rownames(combined))

                overlapped <- filled[candidates]
                previous <- reference[candidates]
                replacements <- cur_df[[d]]

                # Only doing the replacement if the overlaps are identical.
                # Incidentally, this also checks for the right type. We could
                # be more aggressive and do a partial replacement, but
                # something is probably already wrong if this warning fires.
                if (!identical(previous[overlapped], replacements[overlapped])) {
                    warning(wmsg("column '", d, "' is present in DataFrames ", affected[1], " and ", i,
                        " but contains different values for the shared rows; ",
                        "only values from the former will be reported"))
                } else {
                    reference[candidates] <- replacements
                    filled[candidates] <- TRUE
                }
            }

            combined[[d]] <- reference

        } else {
            for (i in affected[-1]) {
                if (!identical(all_df[[i]][[d]], reference)) {
                    # In this case, the warning is only emitted if they are not identical.
                    warning(wmsg("column '", d, "' is present in DataFrames ", affected[1], " and ", i,
                        " but contains different values; only values from the former will be reported"))
                }
            }
        }
    }

    combined
}
