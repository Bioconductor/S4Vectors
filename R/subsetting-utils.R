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
        ## 'subscript' is an object that holds integer values >= 1 and
        ## <= upper_bound, or NA_integer_ values. The precise type of the
        ## object depends on the NSBS subclass and is specified in the
        ## definition of the subclass.
        subscript="ANY",
        upper_bound="integer",            # single integer >= 0
        upper_bound_is_strict="logical",  # TRUE or FALSE
        has_NAs="logical"
    ),
    prototype(
        upper_bound=0L,
        upper_bound_is_strict=TRUE,
        has_NAs=FALSE
    )
)

### There are currently 4 NSBS concrete subclasses:
### - in S4Vectors:
###     1) NativeNSBS: subscript slot is a vector of positive integers
###     2) RangeNSBS:  subscript slot is c(start, end)
###     3) RleNSBS:    subscript slot is an integer-Rle
### - in IRanges:
###     4) RangesNSBS: subscript slot is an IRanges


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NSBS API:
###   - NSBS() constructor function
###   - as.integer()
###   - length()
###   - anyDuplicated()
###   - isStrictlySorted()
###

setGeneric("NSBS", signature="i",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
        standardGeneric("NSBS")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Default methods.
###

### Used in IRanges.
### We use 'call.=FALSE' to hide the function call because displaying it seems
### to confuse some users.
.subscript_error <- function(...) stop(wmsg(...), call.=FALSE)

setMethod("NSBS", "NSBS",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        x_NROW <- NROW(x)
        if (i@upper_bound != x_NROW ||
            i@upper_bound_is_strict < strict.upper.bound)
            .subscript_error(
                "subscript is a NSBS object that is incompatible ",
                "with the current subsetting operation"
            )
        if (!allow.NAs && i@has_NAs)
            .subscript_error("subscript contains NAs")
        i
    }
)

### NSBS concrete subclasses NativeNSBS, RangeNSBS, and RleNSBS override this
### default method.
setMethod("as.integer", "NSBS", function(x) as.integer(x@subscript))

### The 3 default methods below work out-of-the-box on NSBS objects for which
### as.integer() works. However, concrete subclasses RangeNSBS, RleNSBS, and
### RangesNSBS override some of them with more efficient versions that avoid
### expanding 'x' into an integer vector.

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

NativeNSBS <- function(subscript, upper_bound, upper_bound_is_strict, has_NAs)
    new2("NativeNSBS", subscript=subscript,
                       upper_bound=upper_bound,
                       upper_bound_is_strict=upper_bound_is_strict,
                       has_NAs=has_NAs,
                       check=FALSE)

setMethod("NSBS", "missing",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        x_NROW <- NROW(x)
        i <- seq_len(x_NROW)
        NativeNSBS(i, x_NROW, strict.upper.bound, FALSE)
    }
)

setMethod("NSBS", "NULL",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        x_NROW <- NROW(x)
        i <- integer(0)
        NativeNSBS(i, x_NROW, strict.upper.bound, FALSE)
    }
)

.NSBS.numeric <- function(i, x, exact=TRUE, strict.upper.bound=TRUE,
                                allow.NAs=FALSE)
{
    x_NROW <- NROW(x)
    if (!is.integer(i))
        i <- as.integer(i)
    has_NAs <- anyNA(i)
    if (!allow.NAs && has_NAs)
        .subscript_error("subscript contains NAs")
    ## Strangely, this is much faster than using range().
    i_max <- suppressWarnings(max(i, na.rm=TRUE))
    i_min <- suppressWarnings(min(i, na.rm=TRUE))
    if (strict.upper.bound && i_max > x_NROW)
        .subscript_error("subscript contains out-of-bounds indices")
    if (i_min < 0L) {
        ## Translate into positive indices.
        i <- seq_len(x_NROW)[i]
    } else {
        ## Remove 0's from subscript.
        zero_idx <- which(!is.na(i) & i == 0L)
        if (length(zero_idx) != 0L)
            i <- i[-zero_idx]
    }
    NativeNSBS(i, x_NROW, strict.upper.bound, has_NAs)
}

setMethod("NSBS", "numeric", .NSBS.numeric)

setMethod("NSBS", "logical",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        x_NROW <- NROW(x)
        if (anyNA(i))
            .subscript_error("logical subscript contains NAs")
        li <- length(i)
        if (strict.upper.bound && li > x_NROW) {
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
        NativeNSBS(i, x_NROW, strict.upper.bound, FALSE)
    }
)

.NSBS.characterORfactor <- function(i, x, exact=TRUE, strict.upper.bound=TRUE,
                                          allow.NAs=FALSE)
{
    x_NROW <- NROW(x)
    x_ROWNAMES <- ROWNAMES(x)
    what <- if (length(dim(x)) != 0L) "rownames" else "names"
    if (is.null(x_ROWNAMES)) {
        if (strict.upper.bound)
            .subscript_error("cannot subset by character when ", what,
                             " are NULL")
        i <- x_NROW + seq_along(i)
        return(NativeNSBS(i, x_NROW, FALSE, FALSE))
    }
    if (exact) {
        i <- match(i, x_ROWNAMES, incomparables=c(NA_character_, ""))
    } else {
        i <- pmatch(i, x_ROWNAMES, duplicates.ok=TRUE)
    }
    if (!strict.upper.bound) {
        na_idx <- which(is.na(i))
        i[na_idx] <- x_NROW + seq_along(na_idx)
        return(NativeNSBS(i, x_NROW, FALSE, FALSE))
    }
    has_NAs <- anyNA(i)
    if (!allow.NAs && has_NAs)
        .subscript_error("subscript contains invalid ", what)
    NativeNSBS(i, x_NROW, strict.upper.bound, has_NAs)
}

setMethod("NSBS", "character", .NSBS.characterORfactor)

setMethod("NSBS", "factor", .NSBS.characterORfactor)

setMethod("NSBS", "array",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        warning("subscript is an array, passing it thru as.vector() first")
        i <- as.vector(i)
        callGeneric()
    }
)

### Other methods.

setMethod("as.integer", "NativeNSBS", function(x) x@subscript)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RangeNSBS objects.
###

setClass("RangeNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="integer"
    ),
    prototype(
        subscript=c(1L, 0L)
    )
)

### Constructor.

.normarg_range_start <- function(start, argname="start")
{
    if (!isSingleNumberOrNA(start))
        .subscript_error("'", argname, "' must be a single number or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

### Replacement for IRanges:::solveUserSEWForSingleSeq()
### TODO: Get rid of IRanges:::solveUserSEWForSingleSeq() and use RangeNSBS()
### instead.
RangeNSBS <- function(x, start=NA, end=NA, width=NA)
{
    x_NROW <- NROW(x)
    start <- .normarg_range_start(start, "start")
    end <- .normarg_range_start(end, "end")
    width <- .normarg_range_start(width, "width")
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
        stop("the specified range is out-of-bounds")
    if (end < start - 1L)
        stop("the specified range has a negative width")
    new2("RangeNSBS", subscript=c(start, end),
                      upper_bound=x_NROW,
                      check=FALSE)
}

setMethod("as.integer", "RangeNSBS",
    function(x)
    {
        range <- x@subscript
        range_start <- range[[1L]]
        range_end <- range[[2L]]
        if (range_end < range_start)
            return(integer(0))
        seq.int(range_start, range_end)
    }
)

setMethod("length", "RangeNSBS",
    function(x)
    {
        range <- x@subscript
        range_start <- range[[1L]]
        range_end <- range[[2L]]
        range_end - range_start + 1L
    }
)

setMethod("anyDuplicated", "RangeNSBS",
    function(x, incomparables=FALSE, ...) 0L
)

setMethod("isStrictlySorted", "RangeNSBS", function(x) TRUE)

setMethod("show", "RangeNSBS",
    function(object)
    {
        range <- object@subscript
        range_start <- range[[1L]]
        range_end <- range[[2L]]
        cat(sprintf("%d:%d%s / 1:%d%s\n",
                    range_start, range_end,
                    if (length(object) == 0L) " (empty)" else "",
                    object@upper_bound,
                    if (object@upper_bound == 0L) " (empty)" else ""))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeSingleBracketSubscript()
###

normalizeSingleBracketSubscript <- function(i, x,
                                            exact=TRUE, allow.append=FALSE,
                                            allow.NAs=FALSE,
                                            as.NSBS=FALSE)
{
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.append))
        stop("'allow.append' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.NSBS))
        stop("'as.NSBS' must be TRUE or FALSE")
    if (missing(i)) {
        i <- NSBS( , x, exact=exact, strict.upper.bound=!allow.append,
                        allow.NAs=allow.NAs)
    } else {
        i <- NSBS(i, x, exact=exact, strict.upper.bound=!allow.append,
                        allow.NAs=allow.NAs)
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

.extractROWSWithBracket <- function(x, i)
{
  if (is.null(x) || missing(i))
    return(x)
  ## dynamically call [i,,,..,drop=FALSE] with as many "," as length(dim)-1
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x, allow.NAs=TRUE)
  args <- rep.int(alist(foo=), ndim)
  args[[1]] <- i
  args <- c(list(x), args, list(drop=FALSE))
  do.call(`[`, args)
}

.replaceROWSWithBracket <- function(x, i, value)
{
  if (is.null(x))
    return(x)
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE)
  args <- rep.int(alist(foo=), ndim)
  args[[1]] <- i
  args <- c(list(x), args, list(value=value))
  do.call(`[<-`, args)
}

setMethod("extractROWS", c("ANY", "ANY"), .extractROWSWithBracket)

### NOT exported but used in IRanges package (by "extractROWS" method with
### signature vectorORfactor,RangesNSBS).
extract_ranges_from_vectorORfactor <- function(x, start, width)
{
    .Call2("vectorORfactor_extract_ranges", x, start, width,
                                            PACKAGE="S4Vectors")
}

setMethod("extractROWS", c("vectorORfactor", "RangeNSBS"),
    function(x, i)
    {
        start <- i@subscript[[1L]]
        width <- i@subscript[[2L]] - start + 1L
        extract_ranges_from_vectorORfactor(x, start, width)
    }
)

### NOT exported but will be used in IRanges package (by "extractROWS" method
### with signature Linteger,RangesNSBS).
extract_ranges_from_Linteger <- function(x, start, width)
{
    start <- (start - 1L) * BYTES_PER_LINTEGER + 1L
    width <- width * BYTES_PER_LINTEGER
    x@bytes <- extract_ranges_from_vectorORfactor(x@bytes, start, width)
    x
}

setMethod("extractROWS", c("Linteger", "RangeNSBS"),
    function(x, i)
    {
        start <- i@subscript[[1L]]
        width <- i@subscript[[2L]] - start + 1L
        extract_ranges_from_Linteger(x, start, width)
    }
)

setMethod("extractROWS", c("Linteger", "NSBS"),
    function(x, i)
    {
        start <- as.integer(i)
        width <- rep.int(1L, length(start))
        extract_ranges_from_Linteger(x, start, width)
    }
)

setMethod("extractROWS", c("Linteger", "ANY"),
    function (x, i)
    {
        ## We don't support NAs in the subscript yet.
        #i <- normalizeSingleBracketSubscript(i, x, allow.NAs=TRUE,
        #                                           as.NSBS=TRUE)
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        callGeneric()
    }
)

subset_along_ROWS <- function(x, i, j, ..., drop=TRUE)
{
    if (!missing(j) || length(list(...)) > 0L)
        stop("invalid subsetting")
    if (missing(i))
        return(x)
    extractROWS(x, i)
}

setMethod("[", "Linteger", subset_along_ROWS)

setMethod("replaceROWS", "ANY", .replaceROWSWithBracket)


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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### window(), head(), tail(), rep.int()
###

### S3/S4 combo for window.Linteger
window_along_ROWS <- function(x, start=NA, end=NA, width=NA)
{
    i <- RangeNSBS(x, start=start, end=end, width=width)
    extractROWS(x, i)
}
window.Linteger <- function(x, ...) window_along_ROWS(x, ...)
setMethod("window", "Linteger", window.Linteger)

### S3/S4 combo for head.Linteger
head_along_ROWS <- function(x, n=6L)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(x_NROW, n)
    } else {
        n <- max(0L, x_NROW + n)
    }
    window(x, start=1L, width=n)
}
head.Linteger <- function(x, ...) head_along_ROWS(x, ...)
setMethod("head", "Linteger", head.Linteger)

### S3/S4 combo for tail.Linteger
tail_along_ROWS <- function(x, n=6L)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(x_NROW, n)
    } else {
        n <- max(0L, x_NROW + n)
    }
    window(x, end=x_NROW, width=n)
}
tail.Linteger <- function(x, ...) tail_along_ROWS(x, ...)
setMethod("tail", "Linteger", tail.Linteger)

setMethod("rep.int", "Linteger",
    function(x, times)
    {
        x_len <- length(x)
        times_len <- length(times)
        if (times_len == 1L && times == 1L)
            return(x)
        if (times_len == x_len) {
            i <- Rle(seq_len(x_len), times)
        } else if (times_len == 1L) {
            i <- IRanges(rep.int(1L, times), rep.int(x_len, times))
        } else {
            stop("invalid 'times' value")
        }
        extractROWS(x, i)
    }
)

