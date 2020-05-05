### =========================================================================
### Comparing and ordering DataFrame objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### sameAsPreviousROW()
###

# Slightly more efficient than relying on the Vector method,
# which would need to invoke pcompare() and related checks.
setMethod("sameAsPreviousROW", "DataFrame", function(x) {
    is.diff <- lapply(x, FUN=sameAsPreviousROW)
    Reduce("&", is.diff)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###

# Necessary to avoid using match,List,List-method.
setMethod("match", c("DataFrame", "DataFrame"), function (x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
    FUN <- selectMethod("match", c("Vector", "Vector"))
    FUN(x, table, nomatch=nomatch, incomparables=incomparables)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated() and unique()
###
### FIXME: These methods coerce to data.frame which is inefficient and also
### I'm not sure that this guarantees to achieve the same semantic as
### selfmatch() and other comparison methods defined in this file when the
### DataFrame has S4 columns i.e. vector-like objects implemented as S4
### objects. The semantic of the latter is driven by how comparison/ordering
### is defined for the individual S4 columns while the semantic of the methods
### below will ignore that and delagate to the semantic used to compare the
### rows of an ordinary data.frame.
### They also issue an annoying warning:
###   > duplicated(DataFrame(aa=IRanges(c(1:5, 1:0), 6)))
###   [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
###   Warning message:
###   In .local(x, row.names, optional, ...) : 'optional' argument was ignored

### S3/S4 combo for duplicated.DataFrame
duplicated.DataFrame <- function(x, incomparables=FALSE, fromLast=FALSE, ...)
{
    duplicated(as(x, "data.frame"),
               incomparables=incomparables, fromLast=fromLast, ...)
}
setMethod("duplicated", "DataFrame", duplicated.DataFrame)

### S3/S4 combo for unique.DataFrame
unique.DataFrame <- unique.data.frame
setMethod("unique", "DataFrame", unique.DataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() & sort()
###

setMethod("order", "DataFrame", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    contents <- as.list(cbind(...))
    do.call(order, c(contents, list(na.last=na.last, decreasing=decreasing, method=method)))
})

### S3/S4 combo for sort.DataFrame
sort.DataFrame <- sort.Vector
setMethod("sort", "DataFrame", sort.DataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pcompare()
###

setMethod("pcompare", c("DataFrame", "DataFrame"), function(x, y) {
    fields <- colnames(x)
    N <- max(NROW(x), NROW(y))
    if (!identical(sort(fields), sort(colnames(y)))) {
        return(logical(N))
    }

    compared <- integer(N)
    for (f in fields) {
        current <- pcompare(x[[f]], y[[f]])
        keep <- compared==0L
        compared[keep] <- current[keep]
    }

    compared
})

# Necessary to avoid using Ops for Lists.
setMethod("==", c("DataFrame", "DataFrame"), function(e1, e2) pcompare(e1, e2) == 0L)

setMethod("<=", c("DataFrame", "DataFrame"), function(e1, e2) pcompare(e1, e2) <= 0L)

