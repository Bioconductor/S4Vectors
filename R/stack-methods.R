### =========================================================================
### stack() and mstack() methods
### -------------------------------------------------------------------------


### NOT exported but used in package IRanges.
stack_index <- function(x, index.var = "name") {
  if (length(names(x)) > 0) {
    spaceLabels <- names(x)
  } else {
    spaceLabels <- seq_len(length(x))
  }
  ind <- Rle(factor(spaceLabels, levels = unique(spaceLabels)),
             elementNROWS(x))
  do.call(DataFrame, structure(list(ind), names = index.var))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### stack()
###

### FIXME: need a recursive argument, when TRUE we call stack on
### unlist result, instead of coercing to DataFrame.
setMethod("stack", "List",
          function(x, index.var = "name", value.var = "value", name.var = NULL)
          {
            if (!requireNamespace("IRanges", quietly=TRUE))
              stop(wmsg("Couldn't load the IRanges package. Please install ",
                        "the IRanges package before you call stack() on ",
                        "List derivative."))
            value <- unlist(x, use.names=FALSE)
            index <- stack_index(x, index.var)
            unlistsToVector <- is(value, "Vector")
            if (unlistsToVector) {
              df <- cbind(index, ensureMcols(unname(value)))
            } else {
              df <- DataFrame(index, as(unname(value), "DataFrame"))
              colnames(df)[2] <- value.var
            }
            if (!is.null(name.var)) {
              nms <- as.character(unlist(lapply(x, names)))
              if (length(nms) == 0L) {
                rngs <- IRange::IRanges(1L, width=elementNROWS(x))
                nms <- as.integer(rngs)
              } else {
                nms <- factor(nms, unique(nms))
              }
              df[[name.var]] <- nms
              df <- df[c(index.var, name.var, value.var)]
            }
            x_mcols <- mcols(x, use.names=FALSE)
            if (!is.null(x_mcols) && nrow(x_mcols) > 0L) {
                group <- IRanges::togroup(IRanges::PartitioningByEnd(x))
                df <- cbind(df, x_mcols[group, , drop=FALSE])
            }
            if (unlistsToVector) {
              mcols(value) <- df
              value
            } else {
              df
            }
          })

setMethod("stack", "matrix",
          function(x, row.var = names(dimnames(x))[1L],
                   col.var = names(dimnames(x))[2L],
                   value.var = "value")
          {
              l <- x
              attributes(l) <- NULL
              lens <- elementNROWS(l)
              rn <- rownames(x)
              if (is.null(rn))
                  rn <- seq_len(nrow(x))
              else rn <- factor(rn, unique(rn))
              cn <- colnames(x)
              if (is.null(cn))
                  cn <- seq_len(ncol(x))
              else cn <- factor(cn, unique(cn))
              if (is.list(l))
                  l <- stack(List(l))
              ans <- DataFrame(row=rep(rn[row(x)], lens),
                               col=rep(Rle(cn, rep(nrow(x), ncol(x))), lens),
                               value=l)
              if (is.null(row.var)) row.var <- "row"
              if (is.null(col.var)) col.var <- "col"
              colnames(ans) <- c(row.var, col.var, value.var)
              ans
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### mstack()
###

### Hackery to avoid R CMD check warning for using an internal...
.islistfactor <- function(x) {
    eval(as.call(list(quote(.Internal),
                      substitute(islistfactor(x, FALSE), list(x=x)))))
}

### NOT exported but used in package IRanges.
### TODO: Do we really need this? Sounds like what bindROWS() does.
compress_listData <- function(objects, elementType = NULL) {
    if (length(objects) > 0L) {
        if (.islistfactor(objects)) {
            ans <- unlist(objects, recursive=FALSE, use.names=FALSE)
        } else if (length(dim(objects[[1L]])) < 2L) {
            ans <- do.call(c, unname(objects))
        } else {
            ans <- do.call(rbind, unname(objects))
        }
    } else {
        ans <- vector()
    }
    ans
}

setGeneric("mstack", function(..., .index.var = "name")
           standardGeneric("mstack"), signature = "...")

setMethod("mstack", "Vector", function(..., .index.var = "name") {
  if (!isSingleString(.index.var))
    stop("'.index.var' must be a single, non-NA string")
  objects <- list(...)
  combined <- compress_listData(objects)
  df <- stack_index(objects, .index.var)
  if (!is.null(mcols(combined, use.names=FALSE)))
    df <- cbind(df, mcols(combined, use.names=FALSE))
  mcols(combined) <- df
  combined
})

setMethod("mstack", "vector",
          function(..., .index.var = "name")
          {
            if (!isSingleString(.index.var))
              stop("'.index.var' must be a single, non-NA string")
            objects <- list(...)
            combined <- compress_listData(objects)
            df <- DataFrame(stack_index(objects, .index.var), combined)
            if (ncol(df) == 2L)
              colnames(df)[2] <- "value"
            df
          })

setMethod("mstack", "DataFrame",
    function(..., .index.var="name")
    {
        if (!requireNamespace("IRanges", quietly=TRUE))
            stop(wmsg("Couldn't load the IRanges package. Please install ",
                      "the IRanges package before you call mstack() on ",
                      "DataFrame objects."))
        stack(IRanges::DataFrameList(...), index.var=.index.var)
    }
)

