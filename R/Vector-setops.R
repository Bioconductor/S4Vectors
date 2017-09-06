### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagates the names and metadata columns.
###

### S3/S4 combo for union.Vector
union.Vector <- function(x, y) unique(c(x, y))
setMethod("union", c("Vector", "Vector"), union.Vector)

### S3/S4 combo for intersect.Vector
intersect.Vector <- function(x, y) unique(x[x %in% y])
setMethod("intersect", c("Vector", "Vector"), intersect.Vector)

### S3/S4 combo for setdiff.Vector
setdiff.Vector <- function(x, y) unique(x[!(x %in% y)])
setMethod("setdiff", c("Vector", "Vector"), setdiff.Vector)

### S3/S4 combo for setequal.Vector
setequal.Vector <- function(x, y) all(x %in% y) && all(y %in% x)
setMethod("setequal", c("Vector", "Vector"), setequal.Vector)

