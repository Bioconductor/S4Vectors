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
### See also the FIXME note down below (the one preceding the definition of
### the method for vector) for more mess.
###
### FIXME: Fix the aggregate() mess. Before fixing, it would be good to
### simplify by gettting rid of the 'frequency' and 'delta' arguments.
### Then the 'start', 'end', and 'width' arguments wouldn't be needed
### anymore because the user can aggregate by range by passing
### IRanges(start, end, width) to 'by'. After removing these arguments,
### the remaining arguments would be as in stats:::aggregate.data.frame.
### Finally make sure that, when 'by' is not a Ranges, the "aggregate" method
### for vector objects behaves exactly like stats:::aggregate.data.frame
### (the easiest way would be to delegate to it).
###
### A nice extension would be to have 'by' accept an IntegerList object, not
### just a Ranges (which is a special case of IntegerList), to let the user
### specify the subsets of 'x'. When 'by' is an IntegerList, aggregate() would
### be equivalent to:
###
###   sapply(seq_along(by),
###          function(i) FUN(x[by[[i]]], ...), simplify=simplify)
###
### This could be how it is implemented, except for the common use case where
### 'by' is a Ranges (needs special treatment in order to remain as fast as it
### is at the moment). This could even be extended to 'by' being a List (e.g.
### CharacterList, RleList, etc...)
###
### Other options (non-exclusive) to explore:
###
### (a) aggregateByRanges() new generic (should go in IRanges). aggregate()
###     would simply delegate to it when 'by' is a Ranges object (but that
###     means that the "aggregate" methods should also go in IRanges).
###
### (b) lapply/sapply on Views objects (but only works if Views(x, ...)
###     works and views can only be created on a few specific types of
###     objects).
###  


setMethod("aggregate", "matrix", stats:::aggregate.default)
setMethod("aggregate", "data.frame", stats:::aggregate.data.frame)
setMethod("aggregate", "ts", stats:::aggregate.ts)

### S3/S4 combo for aggregate.Vector
aggregate.Vector <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                             frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    aggregate(x, by, FUN, start, end, width, frequency, delta, ...,
              simplify=simplify)
}

.aggregate.Vector <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                              frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        if (is.list(by)) {
            ans <- aggregate(as.data.frame(x), by=by, FUN=FUN, ...,
                             simplify=simplify)
            return(ans)
        }
        start <- structure(start(by), names=names(by))
        end <- end(by)
    } else {
        if (!is.null(width)) {
            if (is.null(start))
                start <- end - width + 1L
            else if (is.null(end))
                end <- start + width - 1L
        }
        ## Unlike as.integer(), as( , "integer") propagates the names.
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
               FUN(Vector_window(x, start = start[i], end = end[i]), ...),
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
setMethod("aggregate", "Vector", .aggregate.Vector)

.aggregate.Rle <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                          frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        start <- structure(start(by), names=names(by))
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
setMethod("aggregate", "Rle", .aggregate.Rle)

.aggregate.List <- function(x, by, FUN, start=NULL, end=NULL, width=NULL,
                           frequency=NULL, delta=NULL, ..., simplify=TRUE)
{
    if (missing(by)
     || !requireNamespace("IRanges", quietly=TRUE)
     || !is(by, "RangesList")) {
        ans <- callNextMethod()
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
    as(result, "List")
}
setMethod("aggregate", "List", .aggregate.List)

