### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### union(), intersect(), and setdiff() are endomorphisms with respect to
### their first argument 'x'.
###

setMethod("union", c("Hits", "Hits"),
    function(x, y)
    {
        m <- match(y, x)
        y <- y[is.na(m)]
        q_hits <- c(queryHits(x), queryHits(y))
        s_hits <- c(subjectHits(x), subjectHits(y))
        Hits(q_hits, s_hits, queryLength(x), subjectLength(x))
    }
)

### Because a Hits object is not expected to contain duplicated we don't
### need to call unique() on the returned object (like base::intersect()
### and base::setdiff() do).

setMethod("intersect", c("Hits", "Hits"), function(x, y) x[x %in% y])

setMethod("setdiff", c("Hits", "Hits"), function(x, y) x[!(x %in% y)])

