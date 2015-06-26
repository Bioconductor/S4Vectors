### =========================================================================
### "aggregate" methods
### -------------------------------------------------------------------------
###
### This is messy and broken! E.g.
###
###   aggregate(DataFrame(state.x77), FUN=mean, start=1:20, width=10)
###
### doesn't work as expected. Or:
###
###   aggregate(Rle(2:-2, 5:9), FUN=mean, start=1:20, width=17)
###
### doesn't give the same result as:
###
###   aggregate(rep(2:-2, 5:9), FUN=mean, start=1:20, width=17)
###
### FIXME: Fix the aggregate() mess (may be simplify it first by gettting
### rid of the 'frequency' and 'delta' args). Alternatively deprecate these
### "aggregate" methods (except the method for Rle objects, but it should have
### the same arguments as stats:::aggregate.data.frame and behave exactly like
### stats::aggregate on atomic vectors), and replace with aggregateByRanges()
### new generic.
###

setMethod("aggregate", "matrix", stats:::aggregate.default)
setMethod("aggregate", "data.frame", stats:::aggregate.data.frame)
setMethod("aggregate", "ts", stats:::aggregate.ts)

### S3/S4 combo for aggregate.Vector
aggregate.Vector <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                             frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        if (is.list(by)) {
            ans <- aggregate(as.data.frame(x), by=by, FUN=FUN, ...,
                             simplify=simplify)
            return(ans)
        }
        start <- start(by)
        end <- end(by)
    } else {
        if (!is.null(width)) {
            if (is.null(start))
                start <- end - width + 1L
            else if (is.null(end))
                end <- start + width - 1L
        }
        start <- as(start, "integer")
        end <- as(end, "integer")
    }
    if (length(start) != length(end))
        stop("'start', 'end', and 'width' arguments have unequal length")
    n <- length(start)
    if (!is.null(names(start)))
        indices <- structure(seq_len(n), names = names(start))
    else
        indices <- structure(seq_len(n), names = names(end))
    if (is.null(frequency) && is.null(delta)) {
        sapply(indices, function(i)
               FUN(window(x, start = start[i], end = end[i]), ...),
               simplify = simplify)
    } else {
        frequency <- rep(frequency, length.out = n)
        delta <- rep(delta, length.out = n)
        sapply(indices, function(i)
               FUN(window(x, start = start[i], end = end[i],
                   frequency = frequency[i], delta = delta[i]),
                   ...),
               simplify = simplify)
    }
}
setMethod("aggregate", "Vector", aggregate.Vector)

### FIXME: This "aggregate" method for vector overrides stats::aggregate()
### on vector without preserving its behavior! For example:
###
###   aggregate(c(NA, 12:20), by=list(rep(1:2, 5)), is.unsorted, TRUE)
###
### doesn't give the same result if S4Vectors is loaded or not.
### As a general rule we should not mask base functionalities with our own,
### and even less when they behave differently.
setMethod("aggregate", "vector", aggregate.Vector)

### S3/S4 combo for aggregate.Rle
aggregate.Rle <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                          frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        start <- start(by)
        end <- end(by)
    } else {
        if (!is.null(width)) {
            if (is.null(start))
                start <- end - width + 1L
            else if (is.null(end))
                end <- start + width - 1L
        }
        start <- as(start, "integer")
        end <- as(end, "integer")
    }
    if (length(start) != length(end))
        stop("'start', 'end', and 'width' arguments have unequal length")
    n <- length(start)
    if (!is.null(names(start)))
        indices <- structure(seq_len(n), names = names(start))
    else
        indices <- structure(seq_len(n), names = names(end))
    if (is.null(frequency) && is.null(delta)) {
        info <- getStartEndRunAndOffset(x, start, end)
        runStart <- info[["start"]][["run"]]
        offsetStart <- info[["start"]][["offset"]]
        runEnd <- info[["end"]][["run"]]
        offsetEnd <- info[["end"]][["offset"]]
        ## Performance Optimization
        ## Use a stripped down loop with empty Rle object
        newRle <- new(class(x))
        sapply(indices,
               function(i)
               FUN(.Call2("Rle_window",
                          x, runStart[i], runEnd[i],
                          offsetStart[i], offsetEnd[i],
                          newRle, PACKAGE = "S4Vectors"),
                   ...),
               simplify = simplify)
    } else {
        frequency <- rep(frequency, length.out = n)
        delta <- rep(delta, length.out = n)
        sapply(indices,
               function(i)
               FUN(window(x, start = start[i], end = end[i],
                          frequency = frequency[i], delta = delta[i]),
                   ...),
               simplify = simplify)
    }
}
setMethod("aggregate", "Rle", aggregate.Rle)

### S3/S4 combo for aggregate.List
aggregate.List <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                           frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    if (missing(by)
     || !requireNamespace("IRanges", quietly=TRUE)
     || !is(by, "RangesList")) {
        ans <- aggregate.Vector(
                   x, by, FUN, start=start, end=end, width=width,
                   frequency=frequency, delta=delta, ..., simplify=simplify)
        return(ans)
    }
    if (length(x) != length(by))
        stop("for Ranges 'by', 'length(x) != length(by)'")
    y <- as.list(x)
    result <- lapply(structure(seq_len(length(x)), names = names(x)),
                     function(i)
                         aggregate(y[[i]], by = by[[i]], FUN = FUN,
                                   frequency = frequency, delta = delta,
                                   ..., simplify = simplify))
    ans <- try(SimpleAtomicList(result), silent = TRUE)
    if (inherits(ans, "try-error"))
        ans <- new_SimpleList_from_list("SimpleList", result)
    ans
}
setMethod("aggregate", "List", aggregate.List)

