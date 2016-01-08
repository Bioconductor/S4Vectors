### =========================================================================
### Low-level subsetting utilities
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Formal representation of a Normalized Single Bracket Subscript, i.e. a
### subscript that holds positive integer values that can be used for single
### bracket subsetting ([ or [<-).
###
### NSBS and its subclasses are for internal use only.
###

setClass("NSBS", 
    representation(
        "VIRTUAL",
        upper_bound="integer",            # single integer >= 0
        upper_bound_is_strict="logical",  # TRUE or FALSE
        ## 'subscript' is an object that holds integer values >= 1 and
        ## <= upper_bound. The precise type of the object depends on the NSBS
        ## subclass and is specified in the subclass definition.
        subscript="ANY"
    ),
    prototype(
        upper_bound=0L,
        upper_bound_is_strict=TRUE
    )
)

### There are currently 4 NSBS concrete subclasses:
### - in S4Vectors:
###     1) NativeNSBS: subscript slot is a vector of positive integers
###     2) WindowNSBS: subscript slot is c(start, end)
###     3) RleNSBS:    subscript slot is an integer-Rle
### - in IRanges:
###     4) RangesNSBS: subscript slot is an IRanges


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NSBS API:
###   - NSBS() constructor function
###   - upperBound()
###   - upperBoundIsStrict()
###   - as.integer()
###   - length()
###   - anyDuplicated()
###   - isStrictlySorted()
###

setGeneric("NSBS", signature="i",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
        standardGeneric("NSBS")
)

setGeneric("upperBound", function(x) standardGeneric("upperBound"))

setGeneric("upperBoundIsStrict",
    function(x) standardGeneric("upperBoundIsStrict")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Default methods.
###

### Used in IRanges.
### We use 'call.=FALSE' to hide the function call because displaying it might
### confuse some users.
.subscript_error <- function(...) stop(wmsg(...), call.=FALSE)

setMethod("NSBS", "NSBS",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        if (upperBound(i) != x_NROW ||
            upperBoundIsStrict(i) < upperBoundIsStrict)
            .subscript_error(
                "subscript is a NSBS object that is incompatible ",
                "with the current subsetting operation"
            )
        i
    }
)

setMethod("upperBound", "NSBS", function(x) x@upper_bound)

setMethod("upperBoundIsStrict", "NSBS", function(x) x@upper_bound_is_strict)

### The 3 default methods below are overriden by NSBS subclasses: WindowNSBS,
### RleNSBS, and RangesNSBS.

setMethod("length", "NSBS", function(x) length(as.integer(x)))

## S3/S4 combo for anyDuplicated.NSBS
anyDuplicated.NSBS <- function(x, incomparables=FALSE, ...)
    anyDuplicated(x, incomparables=incomparables, ...)
setMethod("anyDuplicated", "NSBS", function(x, incomparables=FALSE, ...)
    anyDuplicated(as.integer(x)))

setMethod("isStrictlySorted", "NSBS",
    function(x) isStrictlySorted(as.integer(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NativeNSBS objects.
###

setClass("NativeNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="integer"
    ),
    prototype(
        subscript=integer(0)
    )
)

### Construction methods.
### Supplied arguments are trusted so we don't check them!

.NativeNSBS <- function(subscript, upper_bound, upper_bound_is_strict)
    new("NativeNSBS", subscript=subscript,
                      upper_bound=upper_bound,
                      upper_bound_is_strict=upper_bound_is_strict)

setMethod("NSBS", "missing",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        i <- seq_len(x_NROW)
        .NativeNSBS(i, x_NROW, upperBoundIsStrict)
    }
)

setMethod("NSBS", "NULL",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        i <- integer(0)
        .NativeNSBS(i, x_NROW, upperBoundIsStrict)
    }
)

.NSBS.numeric <- function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
{
    x_NROW <- NROW(x)
    if (!is.integer(i))
        i <- as.integer(i)
    if (upperBoundIsStrict) {
        if (anyMissingOrOutside(i, upper=x_NROW))
            .subscript_error("subscript contains NAs or out-of-bounds indices")
    } else {
        if (any(is.na(i)))
            .subscript_error("subscript contains NAs")
    }
    nonzero_idx <- which(i != 0L)
    i <- i[nonzero_idx]
    if (length(i) != 0L) {
        any_pos <- any(i > 0L)
        any_neg <- any(i < 0L)
        if (any_neg && any_pos)
            .subscript_error("cannot mix negative with positive indices")
        ## From here, indices are guaranteed to be either all positive or
        ## all negative.
        if (any_neg)
            i <- seq_len(x_NROW)[i]
    }
    .NativeNSBS(i, x_NROW, upperBoundIsStrict)
}

setMethod("NSBS", "numeric", .NSBS.numeric)

setMethod("NSBS", "logical",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        if (anyMissing(i))
            .subscript_error("subscript contains NAs")
        li <- length(i)
        if (upperBoundIsStrict && li > x_NROW) {
            if (any(i[(x_NROW+1L):li]))
                .subscript_error(
                    "subscript is a logical vector with out-of-bounds ",
                     "TRUE values"
                )
            i <- i[seq_len(x_NROW)]
        }
        if (li < x_NROW)
            i <- rep(i, length.out=x_NROW)
        i <- which(i)
        .NativeNSBS(i, x_NROW, upperBoundIsStrict)
    }
)

.NSBS.characterORfactor <- function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
{
    x_NROW <- NROW(x)
    x_ROWNAMES <- ROWNAMES(x)
    what <- if (length(dim(x)) != 0L) "rownames" else "names"
    if (is.null(x_ROWNAMES)) {
        if (upperBoundIsStrict)
            .subscript_error("cannot subset by character when ", what,
                             " are NULL")
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
        .subscript_error("subscript contains invalid ", what)
    .NativeNSBS(i, x_NROW, upperBoundIsStrict)
}

setMethod("NSBS", "character", .NSBS.characterORfactor)

setMethod("NSBS", "factor", .NSBS.characterORfactor)

setMethod("NSBS", "array",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        warning("subscript is an array, passing it thru as.vector() first")
        i <- as.vector(i)
        callGeneric()
    }
)

### Other methods.

setMethod("as.integer", "NativeNSBS", function(x) x@subscript)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### WindowNSBS objects.
###

setClass("WindowNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="integer"
    ),
    prototype(
        subscript=c(1L, 0L)
    )
)

### Constructor.

.normarg_window_start <- function(start, argname="start")
{
    if (!isSingleNumberOrNA(start))
        .subscript_error("'", argname, "' must be a single number or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

### Replacement for IRanges:::solveUserSEWForSingleSeq()
### TODO: Get rid of IRanges:::solveUserSEWForSingleSeq() and use WindowNSBS()
### instead.
WindowNSBS <- function(x, start=NA, end=NA, width=NA)
{
    x_NROW <- NROW(x)
    start <- .normarg_window_start(start, "start")
    end <- .normarg_window_start(end, "end")
    width <- .normarg_window_start(width, "width")
    if (is.na(width)) {
        if (is.na(start))
            start <- 1L
        if (is.na(end))
            end <- x_NROW
    } else if (is.na(start) != is.na(end)) {
        if (is.na(start)) {
            start <- end - width + 1L
        } else {
            end <- start + width - 1L
        }
    } else {
        if (is.na(start) && is.na(end)) {
            start <- 1L
            end <- x_NROW
        }
        if (width != end - start + 1L)
            stop("the supplied 'start', 'end', and 'width' are incompatible")
    }
    if (!(start >= 1L && start <= x_NROW + 1L && end <= x_NROW && end >= 0L))
        stop("the specified window is out-of-bounds")
    if (end < start - 1L)
        stop("the specified window has a negative width")
    new("WindowNSBS", subscript=c(start, end), upper_bound=x_NROW)
}

setMethod("as.integer", "WindowNSBS",
    function(x)
    {
        start_end <- x@subscript
        if (diff(start_end) < 0L)
            return(integer(0))
        seq.int(start_end[[1L]], start_end[[2L]])
    }
)

setMethod("length", "WindowNSBS",
    function(x)
    {
        start_end <- x@subscript
        start_end[[2L]] - start_end[[1L]] + 1L
    }
)

setMethod("anyDuplicated", "WindowNSBS",
          function(x, incomparables=FALSE, ...) 0L)

setMethod("isStrictlySorted", "WindowNSBS", function(x) TRUE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeSingleBracketSubscript()
###

normalizeSingleBracketSubscript <- function(i, x, exact=TRUE,
                                            allow.append=FALSE,
                                            as.NSBS=FALSE)
{
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.append))
        stop("'allow.append' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.NSBS))
        stop("'as.NSBS' must be TRUE or FALSE")
    if (missing(i)) {
        i <- NSBS( , x, exact=exact, upperBoundIsStrict=!allow.append)
    } else {
        i <- NSBS(i, x, exact=exact, upperBoundIsStrict=!allow.append)
    }
    if (!as.NSBS)
        i <- as.integer(i)
    i
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeSingleBracketReplacementValue()
###

### Dispatch on the 2nd argument!
setGeneric("normalizeSingleBracketReplacementValue", signature="x",
    function(value, x, i)
        standardGeneric("normalizeSingleBracketReplacementValue")
)

### Default method.
setMethod("normalizeSingleBracketReplacementValue", "ANY",
    function(value, x)
    {
        if (is(value, class(x)))
            return(value)
        lv <- length(value)
        value <- try(as(value, class(x)), silent=TRUE)
        if (inherits(value, "try-error"))
            stop("'value' must be a ", class(x), " object (or coercible ",
                 "to a ", class(x), " object)")
        if (length(value) != lv)
            stop("coercing replacement value to ", class(x), "\n",
                 "  changed its length!\n",
                 "  Please do the explicit coercion ",
                 "yourself with something like:\n",
                 "    x[...] <- as(value, \"", class(x), "\")\n",
                 "  but first make sure this coercion does what you want.")
        value
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extractROWS(), replaceROWS()
###
### 2 internal generics to ease implementation of [ and [<- subsetting for
### Vector subclasses.
###
### A Vector subclass Foo should only need to implement an "extractROWS" and
### "replaceROWS" method to make "[" and "[<-" work out-of-the-box.
### extractROWS() does NOT need to support a missing 'i' so "extractROWS"
### methods don't need to do 'if (missing(i)) return(x)'.
### For replaceROWS(), it's OK to assume that 'value' is "compatible" with 'x'
### i.e. that it has gone thru normalizeSingleBracketReplacementValue().
### See "extractROWS" and "replaceROWS" methods for Hits objects for an
### example.
###

setGeneric("extractROWS", signature=c("x", "i"),
    function(x, i) standardGeneric("extractROWS")
)

setGeneric("replaceROWS", signature="x",
    function(x, i, value) standardGeneric("replaceROWS")
)

### Used in IRanges!
extractROWSWithBracket <- function(x, i)
{
  if (missing(i))
    return(x)
  ## dynamically call [i,,,..,drop=FALSE] with as many "," as length(dim)-1
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x)
  args <- rep(alist(foo=), ndim)
  names(args) <- NULL
  args[[1]] <- i
  args <- c(list(x), args, list(drop=FALSE))
  do.call(`[`, args)
}

replaceROWSWithBracket <- function(x, i, value)
{
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE)
  args <- rep(alist(foo=), ndim)
  names(args) <- NULL
  args[[1]] <- i
  args <- c(list(x), args, list(value=value))
  do.call(`[<-`, args)
}

setMethod("extractROWS", "ANY", extractROWSWithBracket)
setMethod("replaceROWS", "ANY", replaceROWSWithBracket)
setMethod("extractROWS", "NULL", function(x, i) NULL)
setMethod("replaceROWS", "NULL", function(x, i, value) NULL)

setMethod("extractROWS", c("vectorORfactor", "WindowNSBS"),
    function(x, i)
    {
        start_end <- i@subscript
        .Call2("vectorORfactor_extract_window",
               x, start_end[[1L]], start_end[[2L]],
               PACKAGE="S4Vectors")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeDoubleBracketSubscript()
###
### Supported types for 'i': single NA, or numeric or character vector of
### length 1, or numeric- or character-Rle of length 1.
### Always returns a single integer. When called with 'error.if.nomatch=FALSE',
### returns an NA_integer_ if no match is found. Otherwise (the default),
### raises an error if no match is found so the returned integer is guaranteed
### to be a non-NA positive integer referring to a valid position in 'x'.
###

normalizeDoubleBracketSubscript <- function(i, x, exact=TRUE,
                                            error.if.nomatch=TRUE)
{
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(error.if.nomatch))
        stop("'error.if.nomatch' must be TRUE or FALSE")
    if (missing(i))
        stop("subscript is missing")
    subscript_type <- class(i)
    if (is(i, "Rle")) {
        i <- decodeRle(i)
        subscript_type <- paste0(class(i), "-", subscript_type)
    }
    if (is.vector(i) && length(i) == 1L && is.na(i)) {
        if (error.if.nomatch)
            stop("subsetting by NA returns no match")
        return(NA_integer_)
    }
    if (!is.numeric(i) && !is.character(i))
        stop("invalid [[ subscript type: ", subscript_type)
    if (length(i) < 1L)
        stop("attempt to extract less than one element")
    if (length(i) > 1L)
        stop("attempt to extract more than one element")
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (i < 1L || length(x) < i)
            stop("subscript is out of bounds")
        return(i)
    }
    ## 'i' is a character string
    x_names <- names(x)
    if (is.null(x_names)) {
        if (error.if.nomatch)
            stop("attempt to extract by name when elements have no names")
        return(NA_integer_)
    }
    #if (i == "")
    #    stop("invalid subscript \"\"")
    if (exact) {
        ans <- match(i, x_names, incomparables=c(NA_character_, ""))
    } else {
        ## Because 'i' has length 1, it doesn't matter whether we use
        ## 'duplicates.ok=FALSE' (the default) or 'duplicates.ok=TRUE' but
        ## the latter seems to be just a little bit faster.
        ans <- pmatch(i, x_names, duplicates.ok=TRUE)
    }
    if (is.na(ans) && error.if.nomatch)
        stop("subscript \"", i, "\" matches no name")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 internal generics to ease implementation of [[ and [[<- subsetting for
### new List subclasses.
###

setGeneric("getListElement", signature="x",
    function(x, i, exact=TRUE) standardGeneric("getListElement")
)

setGeneric("setListElement", signature="x",
    function(x, i, value) standardGeneric("setListElement")
)

setMethod("getListElement", "list",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=FALSE)
        x[[i]]
    }
)

