setClass("DataFrameFactor", contains="Factor", slots=c(levels="DataFrame"))

DataFrameFactor <- function (x, levels, index = NULL, ...) {
    f <- Factor(x, levels, index=index, ...)
    f@levels <- DataFrame(as.list(f@levels)) # hack
    as(f, "DataFrameFactor")
}

setMethod("dim", "DataFrameFactor", function(x) c(length(x), ncol(levels(x))))

setMethod("dimnames", "DataFrameFactor", function(x) list(names(x), colnames(levels(x))))

setMethod("[[", "DataFrameFactor", function(x, i, j, ...) levels(x)[as.integer(x),i])

setMethod("$", "DataFrameFactor", function(x, name) x[[name]])

setMethod("[", "DataFrameFactor", function(x, i, j, ..., drop=TRUE) {
    if (!missing(j)) {
        sub.levels <- levels(x)[,j,drop=FALSE]
        new.levels <- unique(sub.levels)
        x@index <- match(sub.levels, new.levels)[x@index]
        x@levels <- new.levels
    }

    if (!missing(i)) {
        x <- extractROWS(x, i)
    }

    if (drop && ncol(x)==1L) {
        x <- x[[1]]
    }

    x
})

setMethod("show", "DataFrameFactor", function(object) {
    callNextMethod()
    coolcat("colnames(%i): %s\n", colnames(object))
})
