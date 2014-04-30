### =========================================================================
### Utility functions for checking/fixing user-supplied arguments
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### For checking only.
###

isTRUEorFALSE <- function(x)
{
    is.logical(x) && length(x) == 1L && !is.na(x)
}

isSingleInteger <- function(x)
{
    is.integer(x) && length(x) == 1L && !is.na(x)
}

isSingleNumber <- function(x)
{
    is.numeric(x) && length(x) == 1L && !is.na(x)
}

isSingleString <- function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x)
}

### We want these functions to return TRUE when passed an NA of whatever type.
isSingleNumberOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1L && (is.numeric(x) || is.na(x))
}

isSingleStringOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1L && (is.character(x) || is.na(x))
}

### NOT exported.
anyMissing <- function(x) .Call2("anyMissing", x, PACKAGE="S4Vectors")

### NOT exported.
isNumericOrNAs <- function(x)
{
    is.numeric(x) || (is.atomic(x) && is.vector(x) && all(is.na(x)))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### For checking AND fixing (aka normalizing).
###

### NOT exported.
numeric2integer <- function(x)
{
    if (is.numeric(x) && !is.integer(x)) as.integer(x) else x
}

### NOT exported.
### recycleVector() vs rep(x, length.out=length):
###   - The former seems a little bit faster (1.5x - 2x).
###   - The former will issue a warning that "number of items to replace is not
###     a multiple of replacement length". The latter will always remain silent.
recycleVector <- function(x, length.out)
{
    if (length(x) == length.out) {
        x
    } else {
        ans <- vector(storage.mode(x), length.out)
        ans[] <- x
        ans
    }
}

### NOT exported.
### Must always drop the names of 'arg'.
recycleArg <- function(arg, argname, length.out)
{
    if (length.out == 0L) {
        if (length(arg) > 1L)
            stop("invalid length for '", argname, "'")
        if (length(arg) == 1L && is.na(arg))
            stop("'", argname, "' contains NAs")
        return(recycleVector(arg, length.out))  # drops the names
    }
    if (length(arg) == 0L)
        stop("'", argname, "' has no elements")
    if (length(arg) > length.out)
        stop("'", argname, "' is longer than 'x'")
    if (anyMissing(arg))
        stop("'", argname, "' contains NAs")
    if (length(arg) < length.out)
        arg <- recycleVector(arg, length.out)  # drops the names
    else
        arg <- unname(arg)
    arg
}

recycleIntegerArg <- function(arg, argname, length.out)
{
    if (!is.numeric(arg))
        stop("'", argname, "' must be a vector of integers")
    if (!is.integer(arg))
        arg <- as.integer(arg)
    recycleArg(arg, argname, length.out)
}

recycleNumericArg <- function(arg, argname, length.out)
{
    if (!is.numeric(arg))
        stop("'", argname, "' must be a numeric vector")
    recycleArg(arg, argname, length.out)
}

### We use a signature in the style of IRanges::successiveIRanges() or
### IRanges::successiveViews().
### The current implementation should be fast enough if length(x)/circle.length
### is small (i.e. < 10 or 20). This will actually be the case for the typical
### usecase which is the calculation of "circular coverage vectors", that is,
### we use fold() on the "linear coverage vector" to turn it into a "circular
### coverage vector" of length 'circle.length' where 'circle.length' is the
### length of the circular sequence.  
fold <- function(x, circle.length, from=1)
{
    if (typeof(x) != "S4" && !is.numeric(x) && !is.complex(x))
        stop("'x' must be a vector-like object with elements that can be added")
    if (!isSingleNumber(circle.length))
        stop("'circle.length' must be a single integer")
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (circle.length <= 0L)
        stop("'circle.length' must be positive")
    if (!isSingleNumber(from))
        stop("'from' must be a single integer")
    if (!is.integer(from))
        from <- as.integer(from)
    from <- 1L + (from - 1L) %% circle.length
    if (typeof(x) == "S4") {
        ans <- as(rep.int(0L, circle.length), class(x))
        if (length(ans) != circle.length)
            stop("don't know how to handle 'x' of class ", class(x))
    } else {
        ans <- vector(typeof(x), length=circle.length)
    }
    if (from > length(x)) {
        ## Nothing to fold
        jj <- seq_len(length(x)) + circle.length - from + 1L
        ans[jj] <- x
        return(ans)
    }
    if (from > 1L) {
        ii <- seq_len(from - 1L)
        jj <- ii + circle.length - from + 1L
        ans[jj] <- x[ii]
    }
    max_from <- length(x) - circle.length + 1L
    while (from <= max_from) {
        ii <- from:(from+circle.length-1L)
        ans[] <- ans[] + x[ii]
        from <- from + circle.length
    }
    if (from > length(x))
        return(ans)
    ii <- from:length(x)
    jj <- ii - from + 1L
    ans[jj] <- ans[jj] + x[ii]
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other non exported normarg* functions.
###

normargSingleStartOrNA <- function(start)
{
    if (!isSingleNumberOrNA(start))
        stop("'start' must be a single integer or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

normargSingleEndOrNA <- function(end)
{
    if (!isSingleNumberOrNA(end))
        stop("'end' must be a single integer or NA")
    if (!is.integer(end)) 
        end <- as.integer(end)
    end
}

normargUseNames <- function(use.names)
{
    if (is.null(use.names))
        return(TRUE)
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    use.names
}

normargRunK <- function(k, n, endrule)
{
    if (!is.numeric(k))
        stop("'k' must be a numeric vector")
    if (k < 0)
        stop("'k' must be positive")
    if ((endrule != "drop") && (k %% 2 == 0)) {
        k <- 1L + 2L * (k %/% 2L)
        warning(paste("'k' must be odd when 'endrule != \"drop\"'!",
                      "Changing 'k' to ", k))
    }
    if (k > n) {
        k <- 1L + 2L * ((n - 1L) %/% 2L)
        warning("'k' is bigger than 'n'! Changing 'k' to ", k)
    }
    as.integer(k)
}

normargSubset2_iOnly <-
    function(x, i, j, ..., .conditionPrefix=character())
{
    if (!missing(j) || length(list(...)) > 0)
        warning(.conditionPrefix, "arguments beyond 'i' ignored")
    if (missing(i))
        stop(.conditionPrefix, "subscript 'i' is missing")
    if (!is.character(i) && !is.numeric(i))
        stop(.conditionPrefix, "invalid subscript 'i' type")
    if (length(i) < 1L)
        stop(.conditionPrefix, "attempt to select less than one element")
    if (length(i) > 1L)
        stop(.conditionPrefix, "attempt to select more than one element")
    if (is.numeric(i) && (i < 1L || i > length(x)+1))
        stop(.conditionPrefix, "subscript 'i' out of bounds")
    if (is.character(i)) {
        i <- match(i, names(x))
        if (is.na(i))
            i <- length(x) + 1L
    }
    i
}

extraArgsAsList <- function(.valid.argnames, ...)
{
    args <- list(...)
    argnames <- names(args)
    if (length(args) != 0L
        && (is.null(argnames) || any(argnames %in% c("", NA))))
        stop("all extra arguments must be named")
    if (!is.null(.valid.argnames) && !all(argnames %in% .valid.argnames))
        stop("valid extra argument names are ",
             paste("'", .valid.argnames, "'", sep="", collapse=", "))
    if (anyDuplicated(argnames))
        stop("argument names must be unique")
    args
}

