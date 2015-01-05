### =========================================================================
### Comparing and ordering hits
### -------------------------------------------------------------------------
###


.compatible_Hits <- function(x, y)
{
    queryLength(x) == queryLength(y) && subjectLength(x) == subjectLength(y)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###
### Hits are ordered by query hit first and then by subject hit.
### This way, the space of hits is totally ordered.
###

setMethod("compare", c("Hits", "Hits"),
    function(x, y)
    {
        if (!.compatible_Hits(x, y))
            stop("'x' and 'y' are incompatible Hits objects ",
                 "by subject and/or query length")
        compareIntegerPairs(queryHits(x), subjectHits(x),
                            queryHits(y), subjectHits(y))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###

setMethod("match", c("Hits", "Hits"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL,
                       method=c("auto", "quick", "hash"))
    {
        if (!.compatible_Hits(x, table))
            stop("'x' and 'table' are incompatible Hits objects ",
                 "by subject and/or query length")
        if (!is.null(incomparables))
            stop("\"match\" method for Hits objects ",
                 "only accepts 'incomparables=NULL'")
        matchIntegerPairs(queryHits(x), subjectHits(x),
                          queryHits(table), subjectHits(table),
                          nomatch=nomatch, method=method)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selfmatch()
###

setMethod("selfmatch", "Hits",
    function (x, method=c("auto", "quick", "hash"))
        selfmatchIntegerPairs(queryHits(x), subjectHits(x), method=method)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Ordering hits
###
### order(), sort(), rank() on Hits objects are consistent with the order
### on hits implied by compare().
###

### The current implementation doesn't take advantage of the fact that Hits
### objects are already sorted by query hit but maybe a significant speedup
### could be achieved by doing so.
### 'na.last' is irrelevant (Hits objects don't contain NAs) so is ignored.
setMethod("order", "Hits",
    function(..., na.last=TRUE, decreasing=FALSE)
    {
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## All arguments in '...' are guaranteed to be Hits objects.
        args <- list(...)
        if (length(args) == 1L) {
            x <- args[[1L]]
            return(S4Vectors:::orderIntegerPairs(queryHits(x), subjectHits(x),
                                                 decreasing=decreasing))
        }
        order_args <- vector("list", 2L * length(args))
        idx <- 2L * seq_along(args)
        order_args[idx - 1L] <- lapply(args, queryHits)
        order_args[idx] <- lapply(args, subjectHits)
        do.call(order, c(order_args, list(decreasing=decreasing)))
    }
)

