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
    prototype=prototype(elementType="Hits")
)

setClass("SelfHitsList",
    contains="HitsList",
    prototype=prototype(elementType="SelfHits")
)

setClass("SortedByQueryHitsList",
    contains="HitsList",
    prototype=prototype(elementType="SortedByQueryHits")
)

setClass("SortedByQuerySelfHitsList",
    contains=c("SelfHitsList", "SortedByQueryHitsList"),
    prototype=prototype(elementType="SortedByQuerySelfHits")
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

setMethod("from", "HitsList", function(x) {
  as.matrix(x)[,1L,drop=TRUE]
})

setMethod("to", "HitsList", function(x) {
  as.matrix(x)[,2L,drop=TRUE]
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### This constructor always returns a SortedByQueryHitsList instance at the
### moment.
### TODO: Maybe add the 'sort.by.query' argument to let the user choose
### between getting a HitsList or SortedByQueryHitsList instance.
HitsList <- function(list_of_hits, subject)
{
  subjectOffsets <- c(0L, head(cumsum(sapply(subject, length)), -1))
  subjectToQuery <- seq_along(list_of_hits)
  if (!is.null(names(list_of_hits)) && !is.null(names(subject)))
    subjectToQuery <- match(names(list_of_hits), names(subject))
  subjectOffsets <- subjectOffsets[subjectToQuery]
  new_SimpleList_from_list("SortedByQueryHitsList", list_of_hits,
                           subjectOffsets = subjectOffsets)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

.from_HitsList_SortedByQueryHitsList <- function(from)
{
    class(from) <- class(new("SortedByQueryHitsList"))  # temporarily broken
                                                        # instance!
    from@elementType <- "SortedByQueryHits"
    from@listData <- lapply(from@listData, as, "SortedByQueryHits")
    from  # now fixed :-)
}
setAs("HitsList", "SortedByQueryHitsList",.from_HitsList_SortedByQueryHitsList)

### Of course we want 'as(SortedByQueryHitsList, "HitsList", strict=FALSE)'
### to do the right thing (i.e. to be a no-op), but, unfortunately, as()
### won't do that if the coerce,SortedByQueryHitsList,HitsList method
### is defined, because, in this case, as() will **always** call the method,
### EVEN WHEN strict=FALSE AND THE OBJECT TO COERCE ALREADY DERIVES
### FROM THE TARGET CLASS! (This is a serious flaw in as() current
### design/implementation but I wouldn't be surprised if someone argued
### that this is a feature and working as intended.)
### Anyway, a workaround is to support the 'strict=FALSE' case at the level
### of the coerce() method itself. However setAs() doesn't let us do that
### so this is why we use setMethod("coerce", ...) to define these methods.
.from_SortedByQueryHitsList_to_HitsList <-
    function(from, to="HitsList", strict=TRUE)
{
    if (!isTRUEorFALSE(strict))
        stop("'strict' must be TRUE or FALSE")
    if (!strict)
        return(from)
    class(from) <- class(new("HitsList"))  # temporarily broken instance!
    from@elementType <- "Hits"
    from@listData <- lapply(from@listData, as, "Hits")
    from  # now fixed :-)
}
setMethod("coerce", c("SortedByQueryHitsList", "HitsList"),
    .from_SortedByQueryHitsList_to_HitsList
)

## return as.matrix as on Hits, with indices adjusted

setMethod("as.matrix", "HitsList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0L, head(cumsum(sapply(x, nLnode)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep.int(rows, nr), rep.int(x@subjectOffsets, nr))
})

## count up the matches for each left node in every matching

setMethod("as.table", "HitsList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from Hits to HitsList with splitAsList() and family (i.e. relist()
### and extractList())
###

setMethod("relistToClass", "Hits",
    function(x) "HitsList")

setMethod("relistToClass", "SortedByQueryHits",
    function(x) "SortedByQueryHitsList")

setMethod("splitAsList", c("SortedByQueryHits", "ANY"),
    function(x, f, drop=FALSE)
    {
        ans_class <- relistToClass(x)
        x <- as(x, "Hits")
        as(callNextMethod(), ans_class)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other methods
###

t.HitsList <- function(x) t(x)
setMethod("t", "HitsList", function(x) {
  x@elements <- lapply(as.list(x, use.names = FALSE), t)
  x
})

### TODO: many convenience methods

