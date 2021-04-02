### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------


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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting based on NA content
###

### FIXME: na.omit() and na.exclude() set non-slot attributes,
###        and will fail with things like Rle.
setMethod("na.omit", "DataFrame",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.omit <- stats:::na.omit.data.frame(object)
            attr(object.omit, "row.names") <- NULL
            object.omit
          })

setMethod("na.exclude", "DataFrame",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.ex <- stats:::na.exclude.data.frame(object)
            attr(object.ex, "row.names") <- NULL
            object.ex
          })

setMethod("is.na", "DataFrame", function(x) {
  na <- do.call(cbind, lapply(seq(ncol(x)), function(xi) decode(is.na(x[[xi]]))))
  rownames(na) <- rownames(x)
  na
})

setMethod("complete.cases", "DataFrame", function(...) {
  args <- list(...)
  if (length(args) == 1) {
    x <- args[[1L]]
    rowSums(is.na(x)) == 0
  } else complete.cases(args[[1L]]) & do.call(complete.cases, args[-1L])
})


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
### Statistical routines
###

setMethod("xtabs", signature(data = "DataFrame"),
          function(formula = ~., data, subset, na.action, exclude = c(NA, NaN),
                   drop.unused.levels = FALSE)
          {
            data <- as(data, "data.frame")
            callGeneric()
          })

setMethod("table", "DataFrame", function(...) {
  table(as.list(cbind(...)))
})

## TODO: lm, glm, loess, ...

