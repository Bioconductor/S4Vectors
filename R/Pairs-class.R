### =========================================================================
### Pairs objects
### -------------------------------------------------------------------------
###
### Two parallel vectors. Could result from "dereferencing" a Hits.
###

setClass("Pairs",
         contains="Vector",
         representation(first="ANY",
                        second="ANY",
                        NAMES="character_OR_NULL"),
         prototype(first=logical(0L),
                   second=logical(0L),
                   elementMetadata=DataFrame()))

setMethod("parallel_slot_names", "Pairs", function(x)
    c("first", "second", "NAMES", callNextMethod()))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("first", function(x, ...) standardGeneric("first"))
setGeneric("second", function(x, ...) standardGeneric("second"))

setMethod("first", "Pairs", function(x) x@first)
setMethod("second", "Pairs", function(x) x@second)

setGeneric("first<-", function(x, ..., value) standardGeneric("first<-"),
           signature="x")
setGeneric("second<-", function(x, ..., value) standardGeneric("second<-"),
           signature="x")

setReplaceMethod("first", "Pairs", function(x, value) {
                     x@first <- value
                     x
                 })
setReplaceMethod("second", "Pairs", function(x, value) {
                     x@second <- value
                     x
                 })

setMethod("names", "Pairs", function(x) x@NAMES)
setReplaceMethod("names", "Pairs", function(x, value) {
                     x@NAMES <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

Pairs <- function(first, second, ..., names = NULL, hits = NULL) {
    if (!is.null(hits)) {
        stopifnot(is(hits, "Hits"),
                  queryLength(hits) == length(first),
                  subjectLength(hits) == length(second))
        first <- first[queryHits(hits)]
        second <- second[subjectHits(hits)]
    }
    stopifnot(NROW(first) == NROW(second),
              is.null(names) || length(names) == NROW(first))
    if (!missing(...)) {
        elementMetadata <- DataFrame(...)
    } else {
        elementMetadata <- make_zero_col_DFrame(NROW(first))
    }
    new("Pairs", first=first, second=second, NAMES=names,
                 elementMetadata=elementMetadata)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison
### 

setMethod("order", "Pairs", function (..., na.last = TRUE, decreasing = FALSE, 
    method = c("auto", "shell", "radix"))
{
    collected <- lapply(list(...), FUN=function(x) list(first(x), second(x)))
    do.call(order, c(unlist(collected, recursive=TRUE), 
        list(na.last=na.last, decreasing=decreasing, method=method)))
})

setMethod("sameAsPreviousROW", "Pairs", function(x) {
    N <- length(x)
    if (N==0L) {
        return(logical(0))
    }
    a1 <- first(x)
    a2 <- second(x)
    c(FALSE, a1[-1L]==a1[-N] & a2[-1L]==a2[-N])
})

setMethod("pcompare", c("Pairs", "Pairs"), function(x, y) {
    ans1 <- pcompare(first(x), first(y))
    ans2 <- pcompare(second(x), second(y))
    ifelse(ans1!=0, ans1, ans2)
})

setMethod("match", c("Pairs", "Pairs"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
              if (!is.null(incomparables))
                  stop("'incomparables' must be NULL")
              hits <- intersect(findMatches(first(x), first(table), ...),
                                findMatches(second(x), second(table), ...))
              ans <- selectHits(hits, "first")
              if (!identical(nomatch, NA_integer_)) {
                  ans[is.na(ans)] <- nomatch
              }
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coerce
### 
### We use 'zipup' and 'zipdown' because '(un)zip' already taken by utils.
###

setGeneric("zipup", function(x, y, ...) standardGeneric("zipup"))

setMethod("zipup", c("ANY", "ANY"), function(x, y) {
              stopifnot(NROW(x) == NROW(y))
              linear <- bindROWS(x, list(y))
              collate_subscript <- make_XYZxyz_to_XxYyZz_subscript(NROW(x))
              linear <- extractROWS(linear, collate_subscript)
              names <- if (!is.null(ROWNAMES(x))) ROWNAMES(x) else ROWNAMES(y)
              p <- IRanges::PartitioningByWidth(rep(2L, NROW(x)), names=names)
              relist(linear, p)
          })

setMethod("zipup", c("Pairs", "missing"), function(x, y, ...) {
              zipped <- zipup(first(x), second(x), ...)
              names(zipped) <- names(x)
              mcols(zipped) <- mcols(x, use.names=FALSE)
              zipped
          })

setGeneric("zipdown", function(x, ...) standardGeneric("zipdown"))

setMethod("zipdown", "ANY", function(x) {
              stopifnot(all(lengths(x) == 2L))
              p <- IRanges::PartitioningByEnd(x)
              v <- unlist(x, use.names=FALSE)
              Pairs(extractROWS(v, start(p)), extractROWS(v, end(p)),
                    names=names(x))
          })

setMethod("zipdown", "List", function(x) {
              unzipped <- callNextMethod()
              mcols(unzipped) <- mcols(x, use.names=FALSE)
              unzipped
          })

setAs("Pairs", "DFrame", function(from) {
          df <- DataFrame(first=first(from), second=second(from),
                          mcols(from, use.names=FALSE), check.names=FALSE)
          df$names <- names(from)
          df
      })

setMethod("as.data.frame", "Pairs",
          function (x, row.names = NULL, optional = FALSE, ...) {
              as.data.frame(as(x, "DataFrame"), optional=optional,
                            row.names=row.names, ...)
          })

setAs("list_OR_List", "Pairs",
          function(from) {
              zipdown(from)
     })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

setMethod("t", "Pairs", function(x) {
    tx <- x
    first(tx) <- second(x)
    second(tx) <- first(x)
    tx
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.from_Pairs_to_naked_character_matrix_for_display <- function(x)
{
    m <- cbind(first=showAsCell(first(x)),
               second=showAsCell(second(x)))
    cbind_mcols_for_display(m, x)
}
setMethod("makeNakedCharacterMatrixForDisplay", "Pairs",
    .from_Pairs_to_naked_character_matrix_for_display
)

showPairs <- function(x, margin = "", print.classinfo = FALSE) {
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 
                 0L
             else ncol(x_mcols)
    cat(x_class, " object with ", x_len, " pair",
        ifelse(x_len ==  1L, "", "s"), " and ", x_nmc, " metadata column",
        ifelse(x_nmc == 1L, "", "s"), ":\n", sep = "")
    out <- makePrettyMatrixForCompactPrinting(x)
    if (print.classinfo) {
        .COL2CLASS <- c(first = class(first(x)), second = class(second(x)))
        classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L) 
        rownames(out) <- paste0(margin, rownames(out))
    print(out, quote = FALSE, right = TRUE, max = length(out))
}

setMethod("show", "Pairs", function(object) {
              showPairs(object, margin = "  ", print.classinfo = TRUE)
          })
