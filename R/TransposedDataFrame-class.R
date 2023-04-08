### =========================================================================
### TransposedDataFrame objects
### -------------------------------------------------------------------------


### TransposedDataFrame extends List and the List elements are considered to
### be the rows of the TransposedDataFrame object. This means that the length
### of a TransposedDataFrame object which is the length of its underlying
### List is its number of rows.
setClass("TransposedDataFrame",
    contains=c("RectangularData", "List"),
    slots=c(data="DataFrame")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transposition
###
### Note that transposing a DataFrame derivative is currently the only way
### to construct a TransposedDataFrame object. We don't provide a dedicated
### constructor function.
###
### An important property of transposition is that it preserves the length
### and names of the object.
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

setMethod("nrow", "TransposedDataFrame", function(x) ncol(x@data))
setMethod("ncol", "TransposedDataFrame", function(x) nrow(x@data))

setMethod("rownames", "TransposedDataFrame",
    function(x, do.NULL=TRUE, prefix="row")
    {
        if (!(identical(do.NULL, TRUE) && identical(prefix, "row")))
            stop(wmsg("argument 'do.NULL' and 'prefix' are not supported"))
        colnames(x@data)
    }
)
setMethod("colnames", "TransposedDataFrame",
    function(x, do.NULL=TRUE, prefix="col")
    {
        if (!(identical(do.NULL, TRUE) && identical(prefix, "col")))
            stop(wmsg("argument 'do.NULL' and 'prefix' are not supported"))
        rownames(x@data)
    }
)

### For a TransposedDataFrame object, the length is the number of rows
### and the names are the rownames. Note that 'length(x)' and 'names(x)'
### will ultimately end up calling 'length(x@data)' and 'names(x@data)',
### repectively.
setMethod("length", "TransposedDataFrame", function(x) nrow(x))
setMethod("names", "TransposedDataFrame", function(x) rownames(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

### base::`rownames<-`() and base::`colnames<-`() work as long as
### `dimnames<-`() works.
setReplaceMethod("dimnames", "TransposedDataFrame",
    function(x, value)
    {
        if (!(is.list(value) && length(value) == 2L))
            stop("dimnames replacement value must be a list of length 2")
        dimnames(x@data) <- rev(value)
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
### Subassignment
###

setMethod("normalizeSingleBracketReplacementValue", "TransposedDataFrame",
    function(value, x)
    {
        is_empty_list <- is(value, "list_OR_List") && length(value) == 0L
        if (is.null(value) || is_empty_list)
            return(NULL)
        as(value, class(x), strict=FALSE)
    }
)

setReplaceMethod("[", "TransposedDataFrame",
    function(x, i, j, ..., value)
    {
        value <- normalizeSingleBracketReplacementValue(value, x)
        if (!is.null(value))
            value <- t(value)
        t(callGeneric(t(x), j, i, ..., value=value))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercions
###

setAs("DataFrame", "TransposedDataFrame", function(from) t(from))
setAs("TransposedDataFrame", "DataFrame", function(from) t(from))

setMethod("as.matrix", "TransposedDataFrame",
    function(x, ...) t(as.matrix(x@data, ...))
)

### S3/S4 combo for as.list.TransposedDataFrame
.as.list.TransposedDataFrame <-
    function(x, use.names=TRUE) as.list(x@data, use.names=use.names)
as.list.TransposedDataFrame <-
    function(x, ...) .as.list.TransposedDataFrame(x, ...)
setMethod("as.list", "TransposedDataFrame",
    as.list.TransposedDataFrame
)

setAs("ANY", "TransposedDataFrame", function(from) t(as(from, "DataFrame")))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("makeNakedCharacterMatrixForDisplay", "TransposedDataFrame",
    function(x)
    {
        m <- t(makeNakedCharacterMatrixForDisplay(x@data))
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
        classinfo <- make_class_info_for_DataFrame_display(x@data)
        if (x_nrow <= nhead + ntail + 1L) {
            m <- makeNakedCharacterMatrixForDisplay(x)
            if (!is.null(x_rownames))
                rownames(m) <- x_rownames
        } else { 
            m <- rbind(makeNakedCharacterMatrixForDisplay(head(x, nhead)),
                       rbind(rep.int("...", x_ncol)),
                       makeNakedCharacterMatrixForDisplay(tail(x, ntail)))
            rownames(m) <- make_rownames_for_RectangularData_display(
                                             x_rownames, x_nrow,
                                             nhead, ntail)
            classinfo <- make_rownames_for_RectangularData_display(
                                             classinfo, x_nrow,
                                             nhead, ntail)
        }
        rownames(m) <- paste(format(rownames(m)), classinfo)
        print(m, quote=FALSE, right=TRUE)
    }
    invisible(NULL)
}

setMethod("show", "TransposedDataFrame",
    function(object) .show_TransposedDataFrame(object)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

### Defining bindROWS() gives us c() and rbind().
### Ignore the 'check' argument!
setMethod("bindROWS", "TransposedDataFrame",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
    {
        if (!identical(use.names, TRUE))
            stop(wmsg("the bindROWS() method for TransposedDataFrame objects ",
                      "only accepts 'use.names=TRUE'"))
        if (!isTRUEorFALSE(ignore.mcols))
            stop("'ignore.mcols' must be TRUE or FALSE")
        objects <- prepare_objects_to_bind(x, objects)
        all_objects <- c(list(x), objects)
        if (ignore.mcols)
            all_objects <- lapply(all_objects, `mcols<-`, value=NULL)
        t(do.call(cbind, lapply(all_objects, t)))
    }
)

### Defining bindCOLS() gives us cbind().
### Ignore the 'ignore.mcols' argument!
setMethod("bindCOLS", "TransposedDataFrame",
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
    {
        objects <- prepare_objects_to_bind(x, objects)
        t(bindROWS(t(x), objects=lapply(objects, t),
                   use.names=use.names, check=check))
    }
)

