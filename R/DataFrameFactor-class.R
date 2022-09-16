setClass("DataFrameFactor", contains="Factor", slots=c(levels="DataFrame"))

DataFrameFactor <- function(x, levels, index = NULL, ...) {
    if (is.null(index)) {
        levels <- sort(unique(x))
        index <- match(x, levels)
        names(index) <- rownames(x)
    }
    out <- Factor(levels=seq_len(nrow(levels)), index=index, ...)
    out@levels <- levels
    as(out, "DataFrameFactor")
}

setMethod("dim", "DataFrameFactor", function(x) c(length(x), ncol(levels(x))))

setMethod("dimnames", "DataFrameFactor", function(x) list(names(x), colnames(levels(x))))

setMethod("$", "DataFrameFactor", function(x, name) levels(x)[as.integer(x),name])

setMethod("[", "DataFrameFactor", function(x, i, j, ..., drop=TRUE) {
    if (!missing(i)) {
        x <- callNextMethod()
    }

    if (!missing(j)) {
        x@levels <- levels(x)[,j,drop=FALSE]
    }

    if (drop && ncol(levels(x))==1L) {
        x <- levels(x)[as.integer(x),1]
    }

    x
})

setMethod("show", "DataFrameFactor", function(object) {
    callNextMethod()
    coolcat("colnames(%i): %s\n", colnames(object))
})
