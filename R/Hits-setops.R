### =========================================================================
### Set operations
### -------------------------------------------------------------------------
###
### The methods below are endomorphisms with respect to their first argument
### 'x'. They propagate the names and metadata columns.
###

### The default method for Vector objects works fine except when 'x' is a
### SortedByQueryHits object, in which case the result of the union needs
### to be sorted again.
setMethod("union", c("SortedByQueryHits", "Hits"),
    function(x, y)
    {
        ans_class <- class(x)
        x <- as(x, "Hits")
        as(callNextMethod(), ans_class)  # sort, and restore original class
    }
)

