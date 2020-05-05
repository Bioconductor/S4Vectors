### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

### Do we need all these rbind2()/cbind2() methods? They're not even exported!
setMethod("rbind2", c("ANY", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  rbind(x, y, ...)
})
setMethod("rbind2", c("DataFrame", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})
setMethod("rbind2", c("DataFrame", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  rbind(x, y, ...)
})
setMethod("cbind2", c("ANY", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  cbind(x, y, ...)
})
setMethod("cbind2", c("DataFrame", "ANY"), function(x, y, ...) {
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
})
setMethod("cbind2", c("DataFrame", "DataFrame"), function(x, y, ...) {
  x <- as(x, "DataFrame")
  y <- as(y, "DataFrame")
  cbind(x, y, ...)
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

setMethod("merge", c("DataFrame", "DataFrame"), function(x, y, by, ...) {
    if (is(by, "Hits")) {
        return(.mergeByHits(x, y, by, ...))
    }
    as(merge(as(x, "data.frame"), as(y, "data.frame"), by, ...), class(x))
})
setMethod("merge", c("data.frame", "DataFrame"), function(x, y, ...) {
  as(merge(x, as(y, "data.frame"), ...), class(y))
})
setMethod("merge", c("DataFrame", "data.frame"), function(x, y, ...) {
  as(merge(as(x, "data.frame"), y, ...), class(x))
})


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

