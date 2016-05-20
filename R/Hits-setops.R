### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagates the names and metadata columns.
###


setMethod("union", c("Hits", "Hits"),
    function(x, y)
        as(callNextMethod(as(x, "Hits"), as(y, "Hits")), class(x))
)

