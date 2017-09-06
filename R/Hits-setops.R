### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagate the names and metadata columns.
###


setMethod("union", c("Hits", "Hits"),
    function(x, y)
        ## callNextMethod() is broken in R <= 3.4 if there is more than
        ## one "union" generic in the cache (e.g. which is the case if the
        ## user loads the lubridate package). So we avoid its use for now.
        #as(callNextMethod(as(x, "Hits"), as(y, "Hits")), class(x))
        as(union.Vector(as(x, "Hits"), as(y, "Hits")), class(x))
)

