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
            stop("'x' and 'y' are incompatible by subject and/or query length")
        compareIntegerPairs(queryHits(x), subjectHits(x),
                            queryHits(y), subjectHits(y))
    }
)

