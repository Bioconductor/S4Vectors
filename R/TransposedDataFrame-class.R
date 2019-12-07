### =========================================================================
### TransposedDataFrame objects
### -------------------------------------------------------------------------


setClass("TransposedDataFrame",
    contains=c("DataTable", "List"),
    slots=c(data="DataFrame")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transposition
###

### S3/S4 combo for t.DataFrame
t.DataFrame <- function(x)
{
    x_mcols <- mcols(x, use.names=FALSE)
    if (!is.null(x_mcols))
        mcols(x) <- NULL
    new2("TransposedDataFrame", data=x,
                                elementMetadata=x_mcols,
                                check=FALSE)
}
setMethod("t", "DataFrame", t.DataFrame)

### S3/S4 combo for t.TransposedDataFrame
t.TransposedDataFrame <- function(x)
{
    ans <- x@data
    mcols(ans) <- mcols(x, use.names=FALSE)
    ans
}
setMethod("t", "TransposedDataFrame", t.TransposedDataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("dim", "TransposedDataFrame", function(x) rev(dim(x@data)))
setMethod("length", "TransposedDataFrame", function(x) ncol(x@data))

setMethod("dimnames", "TransposedDataFrame", function(x) rev(dimnames(x@data)))
setMethod("names", "TransposedDataFrame", function(x) colnames(x@data))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

setReplaceMethod("rownames", "TransposedDataFrame",
    function(x, value)
    {
        if (is.null(value))
            stop(wmsg("the names of a ", class(x), " object cannot be NULL"))
        colnames(x@data) <- value
        x
    }
)
setReplaceMethod("colnames", "TransposedDataFrame",
    function(x, value)
    {
        rownames(x@data) <- value
        x
    }
)
setReplaceMethod("names", "TransposedDataFrame",
    function(x, value) `rownames<-`(x, value)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("extractROWS", "TransposedDataFrame",
    function(x, i) t(extractCOLS(t(x), i))
)

setMethod("extractCOLS", "TransposedDataFrame",
    function(x, i) t(extractROWS(t(x), i))
)

.subset_TransposedDataFrame <- function(x, i, j, ..., drop=TRUE)
{
    if (!isTRUEorFALSE(drop))
        stop("'drop' must be TRUE or FALSE")
    linear_subsetting <- (nargs() - !missing(drop)) < 3L
    if (linear_subsetting) {
        if (!missing(drop))
            warning("'drop' argument ignored by linear subsetting")
        if (missing(i))
            return(x)
        return(extractROWS(x, i))
    }
    tx <- t(x)
    ## Use 'drop=FALSE' to make sure 'ans' is a DataFrame.
    if (missing(i) && missing(j)) {
        ans <- tx[ ,  , ..., drop=FALSE]
    } else if (missing(i)) {
        ans <- tx[j,  , ..., drop=FALSE]
    } else if (missing(j)) {
        ans <- tx[ , i, ..., drop=FALSE]
    } else {
        ans <- tx[j, i, ..., drop=FALSE]
    }
    if (drop && ncol(ans) == 1L)
        return(ans[[1L]])
    t(ans)
}
setMethod("[", "TransposedDataFrame", .subset_TransposedDataFrame)

setMethod("getListElement", "TransposedDataFrame",
    function(x, i, exact=TRUE) getListElement(x@data, i, exact=exact)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercions
###

setAs("DataFrame", "TransposedDataFrame", function(from) t(from))
setAs("TransposedDataFrame", "DataFrame", function(from) t(from))

setMethod("as.matrix", "TransposedDataFrame",
    function(x, ...) t(as.matrix(x@data, ...))
)

setMethod("as.list", "TransposedDataFrame",
    function(x, use.names=TRUE) as.list(x@data, use.names=use.names)
)

setAs("list", "TransposedDataFrame", function(from) t(as(from, "DataFrame")))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("makeCharacterMatrixForDisplay", "TransposedDataFrame",
    function(x)
    {
        m <- t(makeCharacterMatrixForDisplay(x@data))
        x_colnames <- rownames(x@data)
        if (!is.null(x_colnames))
            colnames(m) <- x_colnames
        m
    }
)

.show_TransposedDataFrame <- function(x)
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
        classinfo <- make_class_info_for_DataTable_display(x@data)
        rownames(m) <- paste(format(rownames(m)), classinfo)
        print(m, quote=FALSE, right=TRUE)
    }
    invisible(NULL)
}

setMethod("show", "TransposedDataFrame",
    function(object) .show_TransposedDataFrame(object)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rbind/cbind
###

.rbind_TransposedDataFrame_objects <- function(x, objects=list(),
                                               ignore.mcols=FALSE)
{
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")
    objects <- prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)
    if (ignore.mcols)
        all_objects <- lapply(all_objects, `mcols<-`, value=NULL)
    t(do.call(cbind, lapply(all_objects, t)))
}

.cbind_TransposedDataFrame_objects <- function(x, objects=list())
{
    objects <- prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)
    t(do.call(rbind, lapply(all_objects, t)))
}

### Defining bindROWS() gives us c() for free.
### Ignores the 'check' argument!
setMethod("bindROWS", "TransposedDataFrame",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
    {
        if (!identical(use.names, TRUE))
            stop(wmsg("the bindROWS() method for TransposedDataFrame objects ",
                      "only accepts 'use.names=TRUE'"))
        .rbind_TransposedDataFrame_objects(x, objects=objects,
                                              ignore.mcols=ignore.mcols)
    }
)

setMethod("rbind", "TransposedDataFrame",
    function(..., deparse.level=1)
    {
        if (!identical(deparse.level, 1))
            warning(wmsg("the rbind() method for TransposedDataFrame objects ",
                         "ignores the 'deparse.level' argument"))
        all_objects <- list(...)
        .rbind_TransposedDataFrame_objects(all_objects[[1L]], all_objects[-1L])
    }
)

setMethod("cbind", "TransposedDataFrame",
    function(..., deparse.level=1)
    {
        if (!identical(deparse.level, 1))
            warning(wmsg("the cbind() method for TransposedDataFrame objects ",
                         "ignores the 'deparse.level' argument"))
        all_objects <- list(...)
        .cbind_TransposedDataFrame_objects(all_objects[[1L]], all_objects[-1L])
    }
)

