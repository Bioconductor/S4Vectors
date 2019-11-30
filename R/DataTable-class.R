### =========================================================================
### DataTable objects
### -------------------------------------------------------------------------
###
### DataTable is an API only (i.e. virtual class with no slots) for
### accessing objects with a rectangular shape like DataFrame or
### RangedData objects.  It mimics the API for standard data.frame
### objects, except derivatives do not necessarily behave as a list of
### columns.
###


setClass("DataTable", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors.
###

setMethod("NROW", "DataTable", function(x) nrow(x))

setMethod("NCOL", "DataTable", function(x) ncol(x))

setMethod("dim", "DataTable", function(x) c(nrow(x), ncol(x)))

setGeneric("ROWNAMES", function(x) standardGeneric("ROWNAMES"))

setMethod("ROWNAMES", "ANY",
    function (x) if (length(dim(x)) != 0L) rownames(x) else names(x)
)

setMethod("ROWNAMES", "DataTable", function(x) rownames(x))

setMethod("dimnames", "DataTable",
          function(x) {
            list(rownames(x), colnames(x))
          })

setReplaceMethod("dimnames", "DataTable",
                 function(x, value)
                 {
                   if (!is.list(value))
                     stop("replacement value must be a list")
                   rownames(x) <- value[[1L]]
                   colnames(x) <- value[[2L]]
                   x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

head.DataTable <- head.matrix
setMethod("head", "DataTable", head.DataTable)

tail.DataTable <- tail.matrix
setMethod("tail", "DataTable", tail.DataTable)

setMethod("subset", "DataTable",
          function(x, subset, select, drop = FALSE, ...)
          {
              i <- evalqForSubset(subset, x, ...)
              j <- evalqForSelect(select, x, ...)
              x[i, j, drop=drop]
          })

### FIXME: na.omit() and na.exclude() set non-slot attributes,
###        and will fail with things like Rle.
setMethod("na.omit", "DataTable",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.omit <- stats:::na.omit.data.frame(object)
            attr(object.omit, "row.names") <- NULL
            object.omit
          })

setMethod("na.exclude", "DataTable",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.ex <- stats:::na.exclude.data.frame(object)
            attr(object.ex, "row.names") <- NULL
            object.ex
          })

setMethod("is.na", "DataTable", function(x) {
  na <- do.call(cbind, lapply(seq(ncol(x)), function(xi) decode(is.na(x[[xi]]))))
  rownames(na) <- rownames(x)
  na
})

setMethod("complete.cases", "DataTable", function(...) {
  args <- list(...)
  if (length(args) == 1) {
    x <- args[[1L]]
    rowSums(is.na(x)) == 0
  } else complete.cases(args[[1L]]) & do.call(complete.cases, args[-1L])
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transforming.
###

setGeneric("column<-",
           function(x, name, value) standardGeneric("column<-"),
           signature="x")

setReplaceMethod("column", "DataTable", function(x, name, value) {
    x[,name] <- value
    x
})

transformColumns <- function(`_data`, ...) {
    exprs <- as.list(substitute(list(...))[-1L])
    if (any(names(exprs) == "")) {
        stop("all arguments in '...' must be named")
    }
    ## elements in '...' can originate from different environments
    env <- setNames(top_prenv_dots(...), names(exprs))
    for (colName in names(exprs)) { # for loop allows inter-arg dependencies
        value <- safeEval(exprs[[colName]], `_data`, env[[colName]])
        column(`_data`, colName) <- value
    }
    `_data`
}

transform.DataTable <- transformColumns

setMethod("transform", "DataTable", transform.DataTable)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

setMethod("cbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'cbind' method for DataTable class ",
               class(list(...)[[1L]])))

setMethod("rbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'rbind' method for DataTable class ",
               class(list(...)[[1L]])))

setMethod("cbind2", c("ANY", "DataTable"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  cbind(x, y, ...)
})

setMethod("cbind2", c("DataTable", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
})

setMethod("cbind2", c("DataTable", "DataTable"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
})

setMethod("rbind2", c("ANY", "DataTable"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  rbind(x, y, ...)
})

setMethod("rbind2", c("DataTable", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})

setMethod("rbind2", c("DataTable", "DataTable"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})

.mergeByHits <- function(x, y, by, all.x=FALSE, all.y=FALSE, sort = TRUE,
                         suffixes = c(".x", ".y"))
{
    nm.x <- colnames(x)
    nm.y <- colnames(y)
    cnm <- nm.x %in% nm.y
    if (any(cnm) && nzchar(suffixes[1L]))
        nm.x[cnm] <- paste0(nm.x[cnm], suffixes[1L])
    cnm <- nm.y %in% nm.x
    if (any(cnm) && nzchar(suffixes[2L]))
        nm.y[cnm] <- paste0(nm.y[cnm], suffixes[2L])

    if (all.x) {
        x.alone <- which(countLnodeHits(by) == 0L)
    }
    x <- x[c(from(by), if (all.x) x.alone), , drop = FALSE]
    if (all.y) {
        y.alone <- which(countRnodeHits(by) == 0L)
        xa <- x[rep.int(NA_integer_, length(y.alone)), , drop = FALSE]
        x <- rbind(x, xa)
    }
    y <- y[c(to(by), if (all.x) rep.int(NA_integer_, length(x.alone)),
             if (all.y) y.alone), , drop = FALSE]

    cbind(x, y)
}

setMethod("merge", c("DataTable", "DataTable"), function(x, y, by, ...) {
    if (is(by, "Hits")) {
        return(.mergeByHits(x, y, by, ...))
    }
    as(merge(as(x, "data.frame"), as(y, "data.frame"), by, ...), class(x))
})
setMethod("merge", c("data.frame", "DataTable"), function(x, y, ...) {
  as(merge(x, as(y, "data.frame"), ...), class(y))
})
setMethod("merge", c("DataTable", "data.frame"), function(x, y, ...) {
  as(merge(as(x, "data.frame"), y, ...), class(x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Summary methods.
###

### S3/S4 combo for duplicated.DataTable
duplicated.DataTable <- function(x, incomparables=FALSE, fromLast=FALSE, ...)
{
    duplicated(as(x, "data.frame"),
               incomparables=incomparables, fromLast=fromLast, ...)
}

setMethod("duplicated", "DataTable", duplicated.DataTable)

### S3/S4 combo for unique.DataTable
unique.DataTable <- unique.data.frame
setMethod("unique", "DataTable", unique.DataTable)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison methods.
###

.sort.Vector <- function(x, decreasing=FALSE, na.last=NA, by)
{
    if (!missing(by)) {
        i <- orderBy(by, x, decreasing=decreasing, na.last=na.last)
    } else {
        i <- order(x, na.last=na.last, decreasing=decreasing)
    }
    extractROWS(x, i)
}
setMethod("sort", "DataTable", .sort.Vector)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setGeneric("as.env", function(x, ...) standardGeneric("as.env"))

setMethod("as.env", "NULL", function(x, enclos, tform = identity) {
  new.env(parent=enclos)
})

addSelfRef <- function(x, env) {
  env$.. <- x
  env
}

as.data.frame.DataTable <- function(x, row.names=NULL, optional=FALSE, ...) {
    as.data.frame(x, row.names=NULL, optional=optional, ...)
}
setMethod("as.data.frame", "DataTable",
          function(x, row.names=NULL, optional=FALSE, ...) {
              as.data.frame(as(x, "DataFrame"),
                            row.names=row.names, optional=optional, ...)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###
### Even though the "show" method and its helper functions below are defined
### for DataTable derivatives, they're totally geared towards data-frame-like
### objects. Don't get fooled!
### TODO: Either make them work for DataTable derivatives **in general** or
### move this to DataFrame-class.R
###

setMethod("makeCharacterMatrixForDisplay", "DataTable",
    function(x)
    {
        df <- as.data.frame(lapply(x, showAsCell), optional=TRUE)
        as.matrix(format(df))
    }
)

make_rownames_for_DataTable_display <- function(x_rownames, nrow, nhead, ntail)
{
    p1 <- ifelse(nhead == 0L, 0L, 1L)
    p2 <- ifelse(ntail == 0L, 0L, ntail - 1L)
    s1 <- s2 <- character(0)
    if (is.null(x_rownames)) {
        if (nhead > 0L)
            s1 <- paste0(as.character(p1:nhead))
        if (ntail > 0L)
            s2 <- paste0(as.character((nrow-p2):nrow))
    } else {
        if (nhead > 0L)
            s1 <- paste0(head(x_rownames, nhead))
        if (ntail > 0L)
            s2 <- paste0(tail(x_rownames, ntail))
    }
    c(s1, "...", s2)
}

make_class_info_for_DataTable_display <- function(x)
{
    vapply(x, function(xi) paste0("<", classNameForDisplay(xi), ">"),
           character(1), USE.NAMES=FALSE)
}

.show_DataTable <- function(x)
{
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    x_nrow <- nrow(x)
    x_ncol <- ncol(x)
    cat(classNameForDisplay(x), " with ",
        x_nrow, " row", ifelse(x_nrow == 1L, "", "s"),
        " and ",
        x_ncol, " column", ifelse(x_ncol == 1L, "", "s"),
        "\n", sep="")
    if (x_nrow != 0L && x_ncol != 0L) {
        x_rownames <- rownames(x)
        if (x_nrow <= nhead + ntail + 1L) {
            m <- makeCharacterMatrixForDisplay(x)
            if (!is.null(x_rownames))
                rownames(m) <- x_rownames
        } else {
            m <- rbind(makeCharacterMatrixForDisplay(head(x, nhead)),
                       rbind(rep.int("...", x_ncol)),
                       makeCharacterMatrixForDisplay(tail(x, ntail)))
            rownames(m) <- make_rownames_for_DataTable_display(
                                                   x_rownames, x_nrow,
                                                   nhead, ntail)
        }
        m <- rbind(make_class_info_for_DataTable_display(x), m)
        print(m, quote=FALSE, right=TRUE)
    }
    invisible(NULL)
}

setMethod("show", "DataTable", function(object) .show_DataTable(object))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Statistical routines
###

setMethod("xtabs", signature(data = "DataTable"),
          function(formula = ~., data, subset, na.action, exclude = c(NA, NaN),
                   drop.unused.levels = FALSE)
          {
            data <- as(data, "data.frame")
            callGeneric()
          })

setMethod("table", "DataTable", function(...) {
  table(as.list(cbind(...)))
})

## TODO: lm, glm, loess, ...

