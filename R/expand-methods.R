### =========================================================================
### expand methods
### -------------------------------------------------------------------------
###

setGeneric("expand", signature="x",
           function(x, ...) 
               standardGeneric("expand")
)

## A helper function to do the work
.expandCols <- function(x, colnames, keepEmptyRows)
{
    if (!is(x, "DataFrame"))
        stop("'x' must be a DataFrame object")
    if (anyNA(colnames) || length(colnames) == 0L)
        stop("'colnames' must contain at least one element, but without NAs")
    cols <- x[colnames]
    if (length(unique(lapply(cols, elementNROWS))) > 1L) {
        stop("columns to expand must all have the same skeleton")
    }
    enr <- elementNROWS(cols[[1L]])
    if(keepEmptyRows){
        cols <- lapply(cols, function(col) {
            col[enr == 0L] <- NA
            col
        })
    }
    idx <- rep(seq_len(nrow(x)), elementNROWS(cols[[1L]]))
    ans <- x[idx, setdiff(colnames(x), colnames), drop=FALSE]
    ans[colnames] <- lapply(cols, unlist, use.names=FALSE)
    ans[colnames(x)]
}

## A better helper
.expand <- function(x, colnames, keepEmptyRows, recursive) {
    if (recursive) {
        for(colname in colnames) {
            x <- .expandCols(x, colname, keepEmptyRows)
        }
    } else {
        x <- .expandCols(x, colnames, keepEmptyRows)
    }
  x
}

### FIXME: should make is.recursive a generic in base R
isRecursive <- function(x) is.recursive(x) || is(x, "List")

defaultIndices <- function(x) {
    which(vapply(x, isRecursive, logical(1L)))
}

setMethod("expand", "DataFrame",
          function(x, colnames, keepEmptyRows = FALSE, recursive = TRUE) {
              stopifnot(isTRUEorFALSE(keepEmptyRows), isTRUEorFALSE(recursive))
              if (missing(colnames)) {
                  colnames <- defaultIndices(x)
              }
              .expand(x, colnames, keepEmptyRows, recursive)
          }
          )

setMethod("expand", "Vector",
          function(x, colnames, keepEmptyRows = FALSE, recursive = TRUE) {
              stopifnot(isTRUEorFALSE(keepEmptyRows), isTRUEorFALSE(recursive))
              if (missing(colnames)) {
                  colnames <- defaultIndices(mcols(x, use.names=FALSE))
              }
              df <- mcols(x, use.names=FALSE)
              df[["__index__"]] <- seq_along(x)
              ex <- .expand(df, colnames, keepEmptyRows, recursive)
              mcols(x) <- NULL
              ans <- x[ex[["__index__"]]]
              ex[["__index__"]] <- NULL
              mcols(ans) <- ex
              ans
          }
          )

## NOT exported but used in VariantAnnotation package.
## Assume that the named columns have the same geometry and expand
## them simultaneously; this is different from the cartesian product
## expansion above.
expandByColumnSet <- function(x, colnames, keepEmptyRows) {
  if (length(colnames) == 0L)
    return(x)
  if(keepEmptyRows) {
    emptyRows <- elementNROWS(col) == 0L
    x[emptyRows, colnames] <- rep(NA, sum(emptyRows))
  }
  ans <- x[quick_togroup(x[[colnames[1L]]]),,drop=FALSE]
  ans[colnames] <- lapply(x[colnames], unlist, use.names = FALSE)
  ans
}

