### =========================================================================
### Low-level subsetting utilities
### -------------------------------------------------------------------------
###


.match_name <- function(i, x_names, exact=TRUE)
{
    if (exact) {
        match(i, x_names, incomparables=c(NA_character_, ""))
    } else {
        ## When 'i' has length 1, it doesn't matter whether we use
        ## 'duplicates.ok=FALSE' (the default) or 'duplicates.ok=TRUE' but
        ## the latter seems to be just a little bit faster.
        pmatch(i, x_names, duplicates.ok=TRUE)
    }
}


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
        ## 'subscript' is an object that represents a vector of integer
        ## values that are >= 1 and <= upper_bound, or NA_integer_ values.
        ## The precise type of the object depends on the NSBS subclass and
        ## is specified via the definition of the subclass.
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
### - defined in S4Vectors:
###     1) NativeNSBS: 'subscript' slot is a vector of positive integers
###     2) RangeNSBS:  'subscript' slot is c(start, end)
###     3) RleNSBS:    'subscript' slot is an integer-Rle
### - defined in IRanges:
###     4) RangesNSBS: 'subscript' slot is an IRanges


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NSBS API:
###   - NSBS() constructor function
###   - as.integer()
###   - length()
###   - anyDuplicated()
###   - isStrictlySorted()
###   - max()
###   - complementNSBS() -- internal generic

### Fallback methods.
### The 5 fallback methods below will work out-of-the-box on any NSBS
### derivative for which as.integer() works. However, concrete subclasses
### RangeNSBS, RleNSBS, and RangesNSBS, should override them with more
### efficient versions that avoid expanding 'x' into an integer vector.

setMethod("length", "NSBS", function(x) length(as.integer(x)))

## S3/S4 combo for anyDuplicated.NSBS
anyDuplicated.NSBS <- function(x, incomparables=FALSE, ...)
    anyDuplicated(as.integer(x), incomparables=incomparables, ...)
setMethod("anyDuplicated", "NSBS", anyDuplicated.NSBS)

setMethod("isStrictlySorted", "NSBS",
    function(x) isStrictlySorted(as.integer(x))
)

setMethod("max", "NSBS",
    function(x, ..., na.rm=FALSE) max(as.integer(x), ..., na.rm=na.rm)
)

### Not exported.
### Must set the 'upper_bound_is_strict' and 'has_NAs' slots of the returned
### NSBS derivative to their default values (TRUE and FALSE, respectively).
setGeneric("complementNSBS", function(x) standardGeneric("complementNSBS"))

setMethod("complementNSBS", "NSBS",
    function(x)
    {
        subscript <- which(tabulate(as.integer(x), x@upper_bound) == 0L)
        NativeNSBS(subscript, x@upper_bound)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NSBS() constructor
###

setGeneric("NSBS", signature="i",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
        standardGeneric("NSBS")
)

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

### Handle any array-like object with at most one effective dimension, and
### turn it into a vector with as.vector().
setMethod("NSBS", "ANY",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        i_dim <- dim(i)
        if (length(i_dim) == 0L)
            .subscript_error("invalid subscript")
        if (sum(i_dim != 1L) >= 2L)
            .subscript_error("array-like subscript has more ",
                             "than one effective dimension")
        #warning(wmsg("subscript is an array or array-like object, ",
        #             "passing it thru as.vector() first"))
        i <- as.vector(i)
        callGeneric()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NativeNSBS objects
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

### Low-level constructor.
### Supplied arguments are trusted so we don't check them!
NativeNSBS <- function(subscript, upper_bound,
                       upper_bound_is_strict=TRUE, has_NAs=FALSE)
    new2("NativeNSBS", subscript=subscript,
                       upper_bound=upper_bound,
                       upper_bound_is_strict=upper_bound_is_strict,
                       has_NAs=has_NAs,
                       check=FALSE)

setMethod("as.integer", "NativeNSBS", function(x) x@subscript)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RangeNSBS objects
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

### Low-level constructor.
### Replacement for IRanges:::solveUserSEWForSingleSeq()
### TODO: Get rid of IRanges:::solveUserSEWForSingleSeq() and use RangeNSBS()
### instead.

.normarg_range_start <- function(start, argname="start")
{
    if (!isSingleNumberOrNA(start))
        .subscript_error("'", argname, "' must be a single number or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

RangeNSBS <- function(x, start=NA, end=NA, width=NA)
{
    x_NROW <- NROW(x)
    start <- .normarg_range_start(start, "start")
    end   <- .normarg_range_start(end, "end")
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
    if (!(start >= 1L && start - 1L <= x_NROW && end <= x_NROW && end >= 0L))
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

### We override the fallback methods defined for NSBS objects with more
### efficient ones.

setMethod("length", "RangeNSBS",
    function(x)
    {
        range <- x@subscript
        range_start <- range[[1L]]
        range_end   <- range[[2L]]
        range_end - range_start + 1L
    }
)

setMethod("anyDuplicated", "RangeNSBS",
    function(x, incomparables=FALSE, ...) 0L
)

setMethod("isStrictlySorted", "RangeNSBS", function(x) TRUE)

setMethod("max", "RangeNSBS",
    function(x, ..., na.rm=FALSE) max(x@subscript[[2L]], ..., na.rm=na.rm)
)

### Can return a NativeNSBS, RangeNSBS, or RangesNSBS object.
setMethod("complementNSBS", "RangeNSBS",
    function(x)
    {
        range <- x@subscript
        range_start <- range[[1L]]
        range_end   <- range[[2L]]
        if (range_start <= 1L) {
            if (range_end >= x@upper_bound)
                return(NativeNSBS(integer(0), x@upper_bound))
            ## Complement has 1 range.
            subscript <- c(range_end + 1L, x@upper_bound)
        } else {
            if (range_end < x@upper_bound) {
                ## Complement has 2 ranges.
                if (!requireNamespace("IRanges", quietly=TRUE))
                    stop(wmsg("This operation requires the IRanges package. ",
                              "Please install it and try again."))
                starts <- c(1L, range_end + 1L)
                ends   <- c(range_start - 1L, x@upper_bound)
                subscript <- IRanges::IRanges(starts, ends)
                ans <- new2("RangesNSBS", subscript=subscript,
                                          upper_bound=x@upper_bound,
                                          check=FALSE)
                return(ans)
            }
            ## Complement has 1 range.
            subscript <- c(1L, range_start - 1L)
        }
        new2("RangeNSBS", subscript=subscript,
                          upper_bound=x@upper_bound,
                          check=FALSE)
    }
)

setMethod("show", "RangeNSBS",
    function(object)
    {
        range <- object@subscript
        range_start <- range[[1L]]
        range_end   <- range[[2L]]
        cat(sprintf("%d:%d%s / 1:%d%s\n",
                    range_start, range_end,
                    if (length(object) == 0L) " (empty)" else "",
                    object@upper_bound,
                    if (object@upper_bound == 0L) " (empty)" else ""))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More NSBS() constructor methods
###

setMethod("NSBS", "missing",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        RangeNSBS(x, start=1L, end=NROW(x))
    }
)


setMethod("NSBS", "NULL",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        x_NROW <- NROW(x)
        i <- integer(0)
        NativeNSBS(i, x_NROW, strict.upper.bound)
    }
)

.NSBS.numeric <- function(i, x, exact=TRUE,
                          strict.upper.bound=TRUE, allow.NAs=FALSE)
{
    x_NROW <- NROW(x)
    if (is.integer(i)) {
        if (!is.null(names(i)))
            names(i) <- NULL
    } else {
        i <- as.integer(i)  # this also drops the names
    }
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
        if (!is.null(names(i)))
            names(i) <- NULL
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
        NativeNSBS(i, x_NROW, strict.upper.bound)
    }
)

.NSBS.character_OR_factor <- function(i, x, exact=TRUE,
                                      strict.upper.bound=TRUE, allow.NAs=FALSE)
{
    x_NROW <- NROW(x)
    x_ROWNAMES <- ROWNAMES(x)
    ## The only reason we use suppressWarnings() here is to suppress the
    ## deprecation warning we get at the moment (BioC 3.14) when calling dim()
    ## on a DataFrameList derivative. Remove when the method is gone (when
    ## this happens, dim() will return NULL on a DataFrameList derivative).
    x_dim <- suppressWarnings(dim(x))
    what <- if (length(x_dim) != 0L) "rownames" else "names"
    if (is.null(x_ROWNAMES)) {
        if (strict.upper.bound)
            .subscript_error("cannot subset by character when ", what,
                             " are NULL")
        i <- x_NROW + seq_along(i)
        return(NativeNSBS(i, x_NROW, FALSE))
    }
    i <- .match_name(i, x_ROWNAMES, exact=exact)
    if (!strict.upper.bound) {
        na_idx <- which(is.na(i))
        i[na_idx] <- x_NROW + seq_along(na_idx)
        return(NativeNSBS(i, x_NROW, FALSE))
    }
    has_NAs <- anyNA(i)
    if (!allow.NAs && has_NAs)
        .subscript_error("subscript contains invalid ", what)
    NativeNSBS(i, x_NROW, strict.upper.bound, has_NAs)
}

setMethod("NSBS", "character", .NSBS.character_OR_factor)

setMethod("NSBS", "factor", .NSBS.character_OR_factor)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeSingleBracketSubscript()
###
### Must return an unnamed integer vector when 'as.NSBS' is FALSE.
###

normalizeSingleBracketSubscript <- function(i, x, exact=TRUE,
                                            allow.append=FALSE,
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
    function(value, x)
        standardGeneric("normalizeSingleBracketReplacementValue")
)

### Default method.
setMethod("normalizeSingleBracketReplacementValue", "ANY",
    function(value, x)
    {
        if (is(value, class(x)))
            return(value)
        value_len <- length(value)
        value <- try(as(value, class(x)), silent=TRUE)
        if (inherits(value, "try-error"))
            stop("'value' must be a ", class(x), " object (or coercible ",
                 "to a ", class(x), " object)")
        if (length(value) != value_len)
            stop("coercing replacement value to ", class(x), "\n",
                 "  changed its length!\n",
                 "  Please do the explicit coercion ",
                 "yourself with something like:\n",
                 "    x[...] <- as(value, \"", class(x), "\")\n",
                 "  but first make sure this coercion does what you want.")
        value
    }
)

setMethod("normalizeSingleBracketReplacementValue", "List",
          function(value, x)
          {
              if (is.null(value))
                  return(NULL)
              callNextMethod()
          }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### recycleSingleBracketReplacementValue()
###

recycleSingleBracketReplacementValue <- function(value, x, i) {
    if (is.null(value))
        return(NULL)
    i <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE, as.NSBS=TRUE)
    li <- length(i)
    if (li == 0L)
        return(value)
    lv <- NROW(value)
    if (lv == 0L)
        stop("replacement has length zero")
    if (li != lv) {
        if (li %% lv != 0L)
            warning("number of values supplied is not a sub-multiple ",
                    "of the number of values to be replaced")
        value <- extractROWS(value, rep(seq_len(lv), length.out=li))
    }
    value
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extractROWS(), replaceROWS(), mergeROWS(), extractCOLS(), replaceCOLS()
###
### 5 internal generics to ease implementation of [ and [<- subsetting for
### Vector and DataFrame subclasses.
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
### mergeROWS() is a composition of replaceROWS() and bindROWS() to
### support appending in [<-(). Vector subclasses never need to
### implement mergeROWS(), but a custom method may be useful for
### e.g. optimization.
###

setGeneric("extractROWS", signature=c("x", "i"),
    function(x, i) standardGeneric("extractROWS")
)

setGeneric("replaceROWS", signature=c("x", "i"),
    function(x, i, value) standardGeneric("replaceROWS")
)

setGeneric("mergeROWS", signature=c("x", "i"),
    function(x, i, value) standardGeneric("mergeROWS")
)

setGeneric("extractCOLS", signature=c("x", "i"),
    function(x, i) standardGeneric("extractCOLS")
)

setGeneric("replaceCOLS", signature=c("x", "i"),
    function(x, i, value) standardGeneric("replaceCOLS")
)

default_extractROWS <- function(x, i)
{
    if (is.null(x) || missing(i))
        return(x)
    i <- normalizeSingleBracketSubscript(i, x, allow.NAs=TRUE,
                                               allow.append=TRUE)
    x_dim <- dim(x)
    if (is.null(x_dim))
        return(x[i])
    ## Call x[i,,,..,drop=FALSE] with as many "," as 'ndim'.
    ndim <- max(length(x_dim), 1L)
    args <- rep.int(list(quote(expr=)), ndim)
    args[[1L]] <- i
    args <- c(list(x), args, list(drop=FALSE))
    do.call(`[`, args)
}

default_replaceROWS <- function(x, i, value)
{
    mergeROWS(x, i, value)
}

default_mergeROWS <- function(x, i, value)
{
  if (is.null(x))
    return(x)
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE)
  args <- rep.int(list(quote(expr=)), ndim)
  args[[1]] <- i
  args <- c(list(x), args, list(value=value))
  do.call(`[<-`, args)
}

setMethod("extractROWS", c("ANY", "ANY"), default_extractROWS)

### NOT exported but used in IRanges package (by "extractROWS" method with
### signature vector_OR_factor,RangesNSBS).
extract_ranges_from_vector_OR_factor <- function(x, start, width)
{
    .Call2("vector_OR_factor_extract_ranges", x, start, width,
                                              PACKAGE="S4Vectors")
}

setMethod("extractROWS", c("vector_OR_factor", "RangeNSBS"),
    function(x, i)
    {
        start <- i@subscript[[1L]]
        width <- i@subscript[[2L]] - start + 1L
        extract_ranges_from_vector_OR_factor(x, start, width)
    }
)

setMethod("extractROWS", c("array", "RangeNSBS"), default_extractROWS)
setMethod("extractROWS", c("data.frame", "RangeNSBS"), default_extractROWS)

### NOT exported but will be used in IRanges package (by "extractROWS" method
### with signature LLint,RangesNSBS).
extract_ranges_from_LLint <- function(x, start, width)
{
    start <- (start - 1L) * BYTES_PER_LLINT + 1L
    width <- width * BYTES_PER_LLINT
    x@bytes <- extract_ranges_from_vector_OR_factor(x@bytes, start, width)
    x
}

setMethod("extractROWS", c("LLint", "RangeNSBS"),
    function(x, i)
    {
        start <- i@subscript[[1L]]
        width <- i@subscript[[2L]] - start + 1L
        extract_ranges_from_LLint(x, start, width)
    }
)

setMethod("extractROWS", c("LLint", "NSBS"),
    function(x, i)
    {
        start <- as.integer(i)
        width <- rep.int(1L, length(start))
        extract_ranges_from_LLint(x, start, width)
    }
)

setMethod("extractROWS", c("LLint", "ANY"),
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

setMethod("[", "LLint", subset_along_ROWS)

setMethod("replaceROWS", c("ANY", "ANY"), default_replaceROWS)

setMethod("mergeROWS", c("ANY", "ANY"), default_mergeROWS)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeDoubleBracketSubscript()
###
### The supplied subscript 'i' must represent (1) a single non-NA number,
### or (2) a single non-NA string, or (3) a single NA (only if 'allow.NA'
### is TRUE). It must be represented as an ordinary atomic vector or Rle
### object of length 1. More precisely:
###  (1) A single non-NA number must be represented as an integer or numeric
###      vector of length 1, or as an integer- or numeric-Rle object of
###      length 1. It must be >= 1 and <= length(x), except if 'allow.append'
###      is TRUE, in which case it must be >= 1 and <= length(x) + 1.
###      If these conditions are satisfied, the subscript is returned as a
###      single integer. Otherwise an error is raised.
###  (2) A single non-NA string must be represented as a character vector or
###      factor of length 1, or as a character- or factor-Rle object of
###      length 1. It must match a name on 'x', except if 'allow.nomatch' is
###      TRUE, in which case it doesn't have to match a name on 'x'.
###      If these conditions are satisfied, the position of the match or NA
###      is returned. Otherwise an error is raised.
###  (3) A single NA must be represented as an atomic vector (of any type)
###      or Rle object of length 1. It is returned as a single logical NA.
### Return a single integer that is >= 1 and <= length(x).
###

normalizeDoubleBracketSubscript <- function(i, x, exact=TRUE,
                                            allow.append=FALSE,
                                            allow.NA=FALSE,
                                            allow.nomatch=FALSE)
{
    if (missing(i))
        stop("subscript is missing")
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.append))
        stop("'allow.append' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.NA))
        stop("'allow.NA' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nomatch))
        stop("'allow.nomatch' must be TRUE or FALSE")
    subscript_type <- class(i)
    if (is(i, "Rle")) {
        i <- decodeRle(i)
        subscript_type <- paste0(class(i), "-", subscript_type)
    }
    if (is.factor(i))
        i <- as.character(i)
    if (is.vector(i) && length(i) == 1L && is.na(i)) {
        if (!allow.NA)
            stop("NA is not a valid [[ subscript")
        return(NA)
    }
    if (!(is.numeric(i) || is.character(i)))
        stop("invalid [[ subscript type: ", subscript_type)
    if (length(i) < 1L)
        stop("attempt to extract less than one element")
    if (length(i) > 1L)
        stop("attempt to extract more than one element")
    x_len <- length(x)
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (i < 1L)
            stop("[[ subscript must be >= 1")
        if (allow.append) {
            if (i > x_len + 1L)
                stop("[[ subscript must be <= length(x) + 1")
        } else {
            if (i > x_len)
                stop("subscript is out of bounds")
        }
        return(i)
    }
    ## 'i' is a single non-NA string.
    x_names <- names(x)
    if (is.null(x_names)) {
        if (!allow.nomatch)
            stop("attempt to extract by name when elements have no names")
        return(NA)
    }
    #if (i == "")
    #    stop("invalid subscript \"\"")
    ans <- .match_name(i, x_names, exact=exact)
    if (is.na(ans)) {
        if (!allow.nomatch)
            stop("subscript \"", i, "\" matches no name")
        return(NA)
    }
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

### Note that although is(x, "list") is FALSE on a data.frame (a non-sense
### that some people will find a way to justify), dispatch will call this
### method if 'x' is a data.frame.
setMethod("getListElement", "list",
    function(x, i, exact=TRUE)
    {
        i2 <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                              allow.NA=TRUE,
                                              allow.nomatch=TRUE)
        if (is.na(i2))
            return(NULL)
        x[[i2]]
    }
)

### Based on `[`. This should automatically take care of removing the
### corresponding row in 'mcols(x)' if 'x' is a Vector derivative.
.remove_list_element <- function(x, i)
{
    stopifnot(isSingleNumberOrNA(i))
    if (is.na(i) || i < 1L || i > length(x))
        return(x)  # no-op
    ## `[<-.data.frame` does some terrible mangling of the colnames
    ## if they contain duplicates so we can't use it here.
    if (is.data.frame(x)) {
        x[[i]] <- NULL
        return(x)
    }
    x[-i]
}

.wrap_in_length_one_list_like_object <- function(value, name, x)
{
    stopifnot(is(x, "list_OR_List"))
    stopifnot(is.null(name) || isSingleStringOrNA(name))
    if (is(x, "List")) {
        tmp <- try(as(value, elementType(x), strict=FALSE), silent=TRUE)
        if (!inherits(tmp, "try-error"))
            value <- tmp
    }
    value <- setNames(list(value), name)
    value <- try(coerce2(value, x), silent=TRUE)
    if (inherits(value, "try-error"))
        stop(wmsg("failed to coerce 'list(value)' to a ", class(x),
                  " object of length 1"))
    value
}

### Based on 'c()'. This should automatically take care of adjusting the
### metadata columns (by rbind'ing a row of NAs to 'mcols(x)') if 'x' is
### a Vector derivative.
.append_list_element <- function(x, value, name=NULL)
{
    if (is.null(name) && !is.null(names(x)))
        name <- ""
    value <- .wrap_in_length_one_list_like_object(value, name, x)
    coerce2(c(x, value), x)
}

### Based on `[<-`.
.replace_list_element <- function(x, i, value)
{
    value <- .wrap_in_length_one_list_like_object(value, names(x)[[i]], x)
    ## `[<-` propagates the metadata columns from 'value' to 'x' but here
    ## we don't want that.
    if (is(x, "Vector"))
        x_mcols <- mcols(x, use.names=FALSE)
    x[i] <- value
    if (is(x, "Vector"))
        mcols(x) <- x_mcols
    x
}

### Work on any list-like object for which `[<-`, c(), and `[` work.
### Also, if 'value' is not NULL, 'list(value)' must be coercible to a
### length-one list-like object of the same class as 'x'.
setListElement_default <- function(x, i, value)
{
    i2 <- normalizeDoubleBracketSubscript(i, x,
                                          allow.append=TRUE,
                                          allow.nomatch=TRUE)
    if (is.null(value))
        return(.remove_list_element(x, i2))
    if (is.na(i2) || i2 > length(x)) {
        name <- if (is.na(i2)) as.character(i) else NULL
        return(.append_list_element(x, value, name))
    }
    .replace_list_element(x, i2, value)
}

### Note that although is(x, "list") is FALSE on a data.frame (a non-sense
### that some people will find a way to justify), dispatch will call this
### method if 'x' is a data.frame.
setMethod("setListElement", "list", setListElement_default)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### window(), head(), tail(), rep.int()
###

### S3/S4 combo for window.LLint
window_along_ROWS <- function(x, start=NA, end=NA, width=NA)
{
    i <- RangeNSBS(x, start=start, end=end, width=width)
    extractROWS(x, i)
}
window.LLint <- function(x, ...) window_along_ROWS(x, ...)
setMethod("window", "LLint", window.LLint)

### S3/S4 combo for head.LLint
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
head.LLint <- function(x, ...) head_along_ROWS(x, ...)
setMethod("head", "LLint", head.LLint)

### S3/S4 combo for tail.LLint
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
tail.LLint <- function(x, ...) tail_along_ROWS(x, ...)
setMethod("tail", "LLint", tail.LLint)

rep.int_along_ROWS <- function(x, times)
{
    if (!(is.numeric(times) || is.LLint(times)))
        stop("'times' must be a numeric or LLint vector")
    x_NROW <- NROW(x)
    times_len <- length(times)
    if (times_len == 1L) {
        if (times == 1L)
            return(x)
        if (times == 0L)
            return(extractROWS(x, integer(0)))
    }
    if (times_len == x_NROW) {
        i <- Rle(seq_len(x_NROW), times)
    } else if (times_len == 1L) {
        if (is.LLint(times))
            times <- as.double(times)
        i <- IRanges::IRanges(rep.int(1L, times), rep.int(x_NROW, times))
    } else {
        stop("invalid 'times' value")
    }
    extractROWS(x, i)
}

setMethod("rep.int", "LLint", rep.int_along_ROWS)

