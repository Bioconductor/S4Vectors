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

