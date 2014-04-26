### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


ROWNAMES <- function (x) if (length(dim(x)) != 0L) rownames(x) else names(x)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Formal representation of a normalized subscript that can be used for
### single bracket subsetting ([ or [<-).
setClass("NormalizedSubscript", 
    representation(
        "VIRTUAL",
        upper_bound="integer",            # Single integer >= 0.
        upper_bound_is_strict="logical",  # TRUE or FALSE.
        ## 'subscript' is an object specified by NormalizedSubscript
        ## subclasses that holds integer values >= 1 and <= upper_bound.
        ## E.g. an integer vector, or integer-Rle, or IRanges.
        subscript="ANY"
    ),
    prototype(
        upper_bound=0L,
        upper_bound_is_strict=TRUE
    )
)

### There are currently 3 NormalizedSubscript concrete subclasses:
###   1) NativeNormalizedSubscript:  subscript="integer"
###   2) RleNormalizedSubscript:     subscript="Rle" (must be integer-Rle)
###   3) IRangesNormalizedSubscript: subscript="IRanges"


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NormalizedSubscript API:
###   - NormalizedSubscript() constructor function
###   - upperBound()
###   - upperBoundIsStrict()
###   - as.integer()
###   - length()
###   - isStrictlySorted()

setGeneric("NormalizedSubscript", signature="i",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
        standardGeneric("NormalizedSubscript")
)

setGeneric("upperBound", function(x) standardGeneric("upperBound"))

setGeneric("upperBoundIsStrict",
    function(x) standardGeneric("upperBoundIsStrict")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Default methods.
###

setMethod("NormalizedSubscript", "NormalizedSubscript",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        if (upperBound(i) != x_NROW ||
            upperBoundIsStrict(i) < upperBoundIsStrict)
            stop("subscript is a NormalizedSubscript object that is ",
                 "incompatible\n  with the current subsetting operation")
        i
    }
)

setMethod("upperBound", "NormalizedSubscript", function(x) x@upper_bound)

setMethod("upperBoundIsStrict", "NormalizedSubscript",
    function(x) x@upper_bound_is_strict
)

### Typically overriden by NormalizedSubscript subclasses.
setMethod("length", "NormalizedSubscript", function(x) length(as.integer(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NativeNormalizedSubscript objects.
###

setClass("NativeNormalizedSubscript",
    contains="NormalizedSubscript",
    representation(
        subscript="integer"
    ),
    prototype(
        subscript=integer(0)
    )
)

### Construction methods. Supplied arguments are trusted, we don't check them!

.NativeNormalizedSubscript <- function(subscript,
                                       upper_bound,
                                       upper_bound_is_strict)
{
    new("NativeNormalizedSubscript",
        subscript=subscript,
        upper_bound=upper_bound,
        upper_bound_is_strict=upper_bound_is_strict)
}

setMethod("NormalizedSubscript", "NULL",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        i <- integer(0)
        .NativeNormalizedSubscript(i, x_NROW, upperBoundIsStrict)
    }
)

setMethod("NormalizedSubscript", "missing",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        i <- seq_len(x_NROW)
        .NativeNormalizedSubscript(i, x_NROW, upperBoundIsStrict)
    }
)

setMethod("NormalizedSubscript", "numeric",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        if (!is.integer(i))
            i <- as.integer(i)
        if (upperBoundIsStrict) {
            if (anyMissingOrOutside(i, upper=x_NROW))
                stop("subscript contains NAs or out-of-bounds indices")
        } else {
            if (any(is.na(i)))
                stop("subscript contains NAs")
        }
        nonzero_idx <- which(i != 0L)
        i <- i[nonzero_idx]
        if (length(i) != 0L) {
            any_pos <- any(i > 0L)
            any_neg <- any(i < 0L)
            if (any_neg && any_pos)
                stop("cannot mix negative with positive indices")
            ## From here, indices are guaranteed to be either all positive or
            ## all negative.
            if (any_neg)
                i <- seq_len(x_NROW)[i]
        }
        .NativeNormalizedSubscript(i, x_NROW, upperBoundIsStrict)
    }
)

setMethod("NormalizedSubscript", "logical",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        if (anyMissing(i))
            stop("subscript contains NAs")
        li <- length(i)
        if (upperBoundIsStrict && li > x_NROW) {
            if (any(i[(x_NROW+1L):li]))
                stop("subscript is a logical vector with out-of-bounds ",
                     "TRUE values")
            i <- i[seq_len(x_NROW)]
        }
        if (li < x_NROW)
            i <- rep(i, length.out=x_NROW)
        i <- which(i)
        .NativeNormalizedSubscript(i, x_NROW, upperBoundIsStrict)
    }
)

.NormalizedSubscript.characterORfactor <-
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
{
    x_NROW <- NROW(x)
    x_ROWNAMES <- ROWNAMES(x)
    what <- if (length(dim(x)) != 0L) "rownames" else "names"
    if (is.null(x_ROWNAMES)) {
        if (upperBoundIsStrict)
            stop("cannot subset by character when ", what, " are NULL")
        i <- x_NROW + seq_along(i)
        return(i)
    }
    if (exact) {
        i <- match(i, x_ROWNAMES, incomparables=c(NA_character_, ""))
    } else {
        i <- pmatch(i, x_ROWNAMES, duplicates.ok=TRUE)
    }
    if (!upperBoundIsStrict) {
        na_idx <- which(is.na(i))
        i[na_idx] <- x_NROW + seq_along(na_idx)
        return(i)
    }
    if (anyMissing(i))
        stop("subscript contains invalid ", what)
    .NativeNormalizedSubscript(i, x_NROW, upperBoundIsStrict)
}

setMethod("NormalizedSubscript", "character",
    .NormalizedSubscript.characterORfactor
)

setMethod("NormalizedSubscript", "factor",
    .NormalizedSubscript.characterORfactor
)

setMethod("as.integer", "NativeNormalizedSubscript", function(x) x@subscript)

