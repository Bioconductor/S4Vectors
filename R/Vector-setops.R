### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagates the names and metadata columns.
###


setMethod("union", c("Vector", "Vector"),
    function(x, y) unique(c(x, y))
)

setMethod("intersect", c("Vector", "Vector"),
    function(x, y) unique(x[x %in% y])
)

setMethod("setdiff", c("Vector", "Vector"),
    function(x, y) unique(x[!(x %in% y)])
)

setMethod("setequal", c("Vector", "Vector"),
    function(x, y) all(x %in% y) && all(y %in% x)
)

