### =========================================================================
### RectangularData objects
### -------------------------------------------------------------------------
###
### RectangularData is a virtual class with no slots to be extended by
### classes that aim at representing objects with a rectangular shape.
### Current RectangularData derivatives are DataFrame, DelayedMatrix,
### SummarizedExperiment, and Assays objects.
### RectangularData derivatives are expected to support the 2D API i.e.
### at least dim(), [, bindROWS(), and bindCOLS().
###

setClass("RectangularData", representation("VIRTUAL"))


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

