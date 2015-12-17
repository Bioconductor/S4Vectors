### =========================================================================
### HitsList objects
### -------------------------------------------------------------------------

### FIXME: Rename this class SimpleHitsList and make HitsList a virtual
### class that SimpleHitsList (and possibly CompressedHitsList, defined in
### IRanges) extend directly.
setClass("HitsList",
    contains="SimpleList",
    representation(
        subjectOffsets="integer"
    ),
    prototype(elementType="Hits")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("space", function(x, ...) standardGeneric("space"))

setMethod("space", "HitsList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <-
                rep.int(space, sapply(as.list(x, use.names = FALSE), length))
            space
          })

setMethod("subjectHits", "HitsList", function(x) {
  as.matrix(x)[,2L,drop=TRUE]
})

setMethod("queryHits", "HitsList", function(x) {
  as.matrix(x)[,1L,drop=TRUE]
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

HitsList <- function(list_of_hits, subject)
{
  subjectOffsets <- c(0L, head(cumsum(sapply(subject, length)), -1))
  subjectToQuery <- seq_along(list_of_hits)
  if (!is.null(names(list_of_hits)) && !is.null(names(subject)))
    subjectToQuery <- match(names(list_of_hits), names(subject))
  subjectOffsets <- subjectOffsets[subjectToQuery]
  new_SimpleList_from_list("HitsList", list_of_hits,
                           subjectOffsets = subjectOffsets)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from Hits to HitsList with extractList() and family.
###

setMethod("relistToClass", "Hits", function(x) "HitsList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on Hits, with indices adjusted

setMethod("as.matrix", "HitsList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0L, head(cumsum(sapply(x, queryLength)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep.int(rows, nr), rep.int(x@subjectOffsets, nr))
})

## count up the matches for each query in every matching

setMethod("as.table", "HitsList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})

setMethod("t", "HitsList", function(x) {
  x@elements <- lapply(as.list(x, use.names = FALSE), t)
  x
})

### TODO: many convenience methods

