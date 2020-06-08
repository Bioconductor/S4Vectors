### =========================================================================
### RectangularData objects
### -------------------------------------------------------------------------
###
### RectangularData is a virtual class with no slots to be extended by
### classes that aim at representing objects with a rectangular shape.
### Current RectangularData derivatives are DataFrame, DelayedMatrix,
### SummarizedExperiment, and Assays objects.
### RectangularData derivatives are expected to support the 2D API: at
### least 'dim()', but also typically 'dimnames()', `[` (the 2D form
### 'x[i, j]'), 'bindROWS()', and 'bindCOLS()'.
###

setClass("RectangularData", representation("VIRTUAL"))

.validate_RectangularData <- function(x)
{
    x_dim <- try(dim(x), silent=TRUE)
    if (inherits(x_dim, "try-error"))
        return("'dim(x)' must work")
    if (!(is.vector(x_dim) && is.numeric(x_dim)))
        return("'dim(x)' must return a numeric vector")
    if (length(x_dim) != 2L)
        return("'x' must have exactly 2 dimensions")
    TRUE
}

setValidity2("RectangularData", .validate_RectangularData)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names() and horizontal_slot_names()
###
### For internal use only.
###

### vertical_slot_names() must return the names of all the slots in
### RectangularData derivative 'x' that are **parallel** to its 1st
### dimension. Slot "foo" in 'x' is considered to be parallel to its
### 1st dimension if it's guaranteed to contain a value that is either
### NULL or such that 'NROW(x@foo)' is equal to 'nrow(x)' and the i-th
### ROW in 'x@foo' is associated with the i-th row in 'x'.
setGeneric("vertical_slot_names",
    function(x) standardGeneric("vertical_slot_names")
)

### horizontal_slot_names() must return the names of all the slots in
### RectangularData derivative 'x' that are **parallel** to its 2nd
### dimension. Slot "bar" in 'x' is considered to be parallel to its
### 2nd dimension if it's guaranteed to contain a value that is either
### NULL or such that 'NROW(x@bar)' is equal to 'ncol(x)' and the j-th
### ROW in 'x@bar' is associated with the j-th col in 'x'.
setGeneric("horizontal_slot_names",
    function(x) standardGeneric("horizontal_slot_names")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("ROWNAMES", function(x) standardGeneric("ROWNAMES"))

setMethod("ROWNAMES", "ANY",
    function (x) if (length(dim(x)) != 0L) rownames(x) else names(x)
)

setMethod("ROWNAMES", "RectangularData", function(x) rownames(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

head.RectangularData <- utils::head.matrix
setMethod("head", "RectangularData", head.RectangularData)

tail.RectangularData <- utils::tail.matrix
setMethod("tail", "RectangularData", tail.RectangularData)

setMethod("subset", "RectangularData",
    function(x, subset, select, drop=FALSE, ...)
    {
        i <- evalqForSubset(subset, x, ...)
        j <- evalqForSelect(select, x, ...)
        x[i, j, drop=drop]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Binding
###

### S3/S4 combo for rbind.RectangularData
rbind.RectangularData <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the rbind() method for RectangularData objects ",
                     "ignores the 'deparse.level' argument"))
    objects <- list(...)
    bindROWS(objects[[1L]], objects=objects[-1L])
}
setMethod("rbind", "RectangularData", rbind.RectangularData)

### S3/S4 combo for cbind.RectangularData
cbind.RectangularData <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the cbind() method for RectangularData objects ",
                     "ignores the 'deparse.level' argument"))
    objects <- list(...)
    bindCOLS(objects[[1L]], objects=objects[-1L])
}
setMethod("cbind", "RectangularData", cbind.RectangularData)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### make_rownames_for_RectangularData_display()
###

### NOT exported but used in package RNAmodR.
make_rownames_for_RectangularData_display <-
    function(x_rownames, nrow, nhead, ntail)
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

