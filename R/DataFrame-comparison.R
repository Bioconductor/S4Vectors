
setMethod("match", c("DataFrame", "DataFrame"), function(x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
    # table first, so stable ordering means that, of matched entries,
    # the one from 'table' gets sorted before that from 'x'.
    combined <- rbind(table, x)
    origins <- c(seq_len(nrow(table)), rep(NA_integer_, nrow(x))) 

    if (nrow(table)==0L) {
        return(origins)
    } else if (nrow(x)==0L) {
        return(integer(0))
    }

    o <- order(combined)
    combined <- combined[o,,drop=FALSE]
    origins <- origins[o]

    is.diff <- lapply(as.list(combined), FUN=function(y) c(TRUE, y[-1]!=y[-nrow(combined)]))
    is.unique <- Reduce("|", is.diff)
    m <- cumsum(is.unique)

    origins <- origins[is.unique][m]
    origins[o] <- origins
    origins <- tail(origins, nrow(x))

    origins[is.na(origins)] <- nomatch
    origins
})

setMethod("selfmatch", "DataFrame", function(x, ...) {
    o <- order(x)
    x <- x[o,,drop=FALSE]

    is.diff <- lapply(as.list(x), FUN=function(y) c(TRUE, y[-1]!=y[-nrow(x)]))
    is.unique <- Reduce("|", is.diff)
    m <- cumsum(is.unique)

    origins <- o[is.unique][m]
    origins[o] <- origins
    origins
})

# One method for pure DF ordering, and xtfrm for mixed DF/other things.
setMethod("order", "DataFrame", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    contents <- lapply(list(...), as.list)
    contents <- unlist(contents, recursive=FALSE)
    do.call(order, c(contents, list(na.last=na.last, decreasing=decreasing, method=method)))
})

setMethod("xtfrm", "DataFrame", function (x) {
    if (nrow(x)==0L) return(integer(0))

    o <- do.call(order, as.list(x))
    x <- x[o,,drop=FALSE]
    is.diff <- lapply(as.list(x), FUN=function(y) c(TRUE, y[-1]!=y[-nrow(x)]))
    is.unique <- Reduce("|", is.diff)

    out.rank <- cumsum(is.unique)
    out.rank[o] <- out.rank
    out.rank
})

