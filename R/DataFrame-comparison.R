# Slightly more efficient than relying on the Vector method,
# which would need to invoke pcompare() and related checks.
setMethod("sameAsLastROW", "DataFrame", function(x) {
    is.diff <- lapply(x, FUN=sameAsLastROW)
    Reduce("&", is.diff)
})

setMethod("order", "DataFrame", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    contents <- lapply(list(...), as.list)
    contents <- unlist(contents, recursive=FALSE)
    do.call(order, c(contents, list(na.last=na.last, decreasing=decreasing, method=method)))
})
