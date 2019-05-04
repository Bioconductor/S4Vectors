# Slightly more efficient than relying on the Vector method,
# which would need to invoke pcompare() and related checks.
setMethod("sameAsLastROW", "DataFrame", function(x) {
    is.diff <- lapply(x, FUN=sameAsLastROW)
    Reduce("&", is.diff)
})

# Necessary to avoid using match,List,List-method.
setMethod("match", c("DataFrame", "DataFrame"), function (x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
    FUN <- selectMethod("match", c("Vector", "Vector"))
    FUN(x, table, nomatch=nomatch, incomparables=incomparables)
})

setMethod("order", "DataFrame", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    contents <- as.list(cbind(...))
    do.call(order, c(contents, list(na.last=na.last, decreasing=decreasing, method=method)))
})
