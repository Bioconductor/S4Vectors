### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagates the names and metadata columns.
###

### S3/S4 combo for union.Vector
setMethod("union", c("Vector", "Vector"), function(x, y) unique(c(x, y)))
union.Vector <- function(x, y, ...) union(x, y, ...)

### S3/S4 combo for intersect.Vector
setMethod("intersect", c("Vector", "Vector"),
          function(x, y) unique(x[x %in% y]))
intersect.Vector <- function(x, y, ...) intersect(x, y, ...)

### S3/S4 combo for setdiff.Vector
setMethod("setdiff", c("Vector", "Vector"),
          function(x, y) unique(x[!(x %in% y)]))
setdiff.Vector <- function(x, y, ...) setdiff(x, y, ...)

### S3/S4 combo for setequal.Vector
setMethod("setequal", c("Vector", "Vector"),
          function(x, y) all(x %in% y) && all(y %in% x))
setequal.Vector <- function(x, y, ...) setequal(x, y, ...)
