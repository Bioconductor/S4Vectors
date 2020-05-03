### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transforming
###

setReplaceMethod("column", "DataFrame", function(x, name, value) {
    x[,name] <- value
    x
})

### S3/S4 combo for transform.DataFrame
transform.DataFrame <- transformColumns
setMethod("transform", "DataFrame", transform.DataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting
###

.relistToClass_DataFrame <- function(x)
{
    ## Technically speaking IRanges is not strictly needed for the sole
    ## purpose of returning class name "CompressedSplitDFrameList" but
    ## we'd rather return the name of a class that actually exists from
    ## a user point of view.
    if (!requireNamespace("IRanges", quietly=TRUE))
        stop(wmsg("Couldn't load the IRanges package. Please install ",
                  "the IRanges package before you try to relist or ",
                  "split a data.frame."))
    "CompressedSplitDFrameList"
}

setMethod("relistToClass", "DataFrame", .relistToClass_DataFrame)

setMethod("relistToClass", "data.frame", .relistToClass_DataFrame)

