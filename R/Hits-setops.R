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
        ans <- unique(c(as(x, "Hits"), as(y, "Hits")))
        as(ans, class(x))
    }
)

setMethod("intersect", c("Hits", "Hits"), function(x, y) unique(x[x %in% y]))

setMethod("setdiff", c("Hits", "Hits"), function(x, y) unique(x[!(x %in% y)]))

