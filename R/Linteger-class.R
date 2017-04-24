### =========================================================================
### Linteger objects
### -------------------------------------------------------------------------
###
### The Linteger class is a container for storing a vector of "large integers"
### (i.e. long long int in C). It supports NAs.
###


### We don't support names for now. We will when we need them.
setClass("Linteger", representation(bytes="raw"))

setClassUnion("integer_OR_Linteger", c("integer", "Linteger"))

is.Linteger <- function(x) is(x, "Linteger")

BYTES_PER_LINTEGER <- .Machine$sizeof.longlong

setMethod("length", "Linteger",
    function(x) length(x@bytes) %/% BYTES_PER_LINTEGER
)

### Called from the .onLoad() hook in zzz.R
make_NA_Linteger_ <- function()
{
    ans_bytes <- .Call2("make_RAW_from_NA_LINTEGER", PACKAGE="S4Vectors")
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

.from_logical_to_Linteger <- function(from)
{
    .Call2("new_Linteger_from_LOGICAL", from, PACKAGE="S4Vectors")
}
setAs("logical", "Linteger", .from_logical_to_Linteger)

.from_integer_to_Linteger <- function(from)
{
    .Call2("new_Linteger_from_INTEGER", from, PACKAGE="S4Vectors")
}
setAs("integer", "Linteger", .from_integer_to_Linteger)

.from_numeric_to_Linteger <- function(from)
{
    .Call2("new_Linteger_from_NUMERIC", from, PACKAGE="S4Vectors")
}
setAs("numeric", "Linteger", .from_numeric_to_Linteger)

.from_character_to_Linteger <- function(from)
{
    .Call2("new_Linteger_from_CHARACTER", from, PACKAGE="S4Vectors")
}
setAs("character", "Linteger", .from_character_to_Linteger)

as.Linteger <- function(x) as(x, "Linteger")

### S3/S4 combo for as.logical.Linteger
.from_Linteger_to_logical <- function(x)
{
    .Call2("new_LOGICAL_from_Linteger", x, PACKAGE="S4Vectors")
}
as.logical.Linteger <- function(x, ...) .from_Linteger_to_logical(x, ...)
setMethod("as.logical", "Linteger", as.logical.Linteger)

### S3/S4 combo for as.integer.Linteger
.from_Linteger_to_integer <- function(x)
{
    .Call2("new_INTEGER_from_Linteger", x, PACKAGE="S4Vectors")
}
as.integer.Linteger <- function(x, ...) .from_Linteger_to_integer(x, ...)
setMethod("as.integer", "Linteger", as.integer.Linteger)

### S3/S4 combo for as.numeric.Linteger
.from_Linteger_to_numeric <- function(x)
{
    .Call2("new_NUMERIC_from_Linteger", x, PACKAGE="S4Vectors")
}
as.numeric.Linteger <- function(x, ...) .from_Linteger_to_numeric(x, ...)
setMethod("as.numeric", "Linteger", as.numeric.Linteger)

### S3/S4 combo for as.character.Linteger
.from_Linteger_to_character <- function(x)
{
    .Call2("new_CHARACTER_from_Linteger", x, PACKAGE="S4Vectors")
}
as.character.Linteger <- function(x, ...) .from_Linteger_to_character(x, ...)
setMethod("as.character", "Linteger", as.character.Linteger)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

.MAX_VECTOR_LENGTH <- 2^52  # see R_XLEN_T_MAX in Rinternals.h

### Return a single double value.
.normarg_vector_length <- function(length=0L, max_length=.MAX_VECTOR_LENGTH)
{
    if (is.Linteger(length)) {
        if (length(length) != 1L || is.na(length) || length < as.Linteger(0L))
            stop("invalid 'length' argument")
        if (length > as.Linteger(max_length))
            stop("'length' is too big")
        return(as.double(length))
    }
    if (!isSingleNumber(length) || length < 0L)
        stop("invalid 'length' argument")
    if (is.integer(length)) {
        length <- as.double(length)
    } else {
        length <- trunc(length)
    }
    if (length > max_length)
        stop("'length' is too big")
    length
}

Linteger <- function(length=0L)
{
    max_length <- .MAX_VECTOR_LENGTH / BYTES_PER_LINTEGER
    length <- .normarg_vector_length(length, max_length=max_length)
    ans_bytes <- raw(length * BYTES_PER_LINTEGER)
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Displaying
###

.show_Linteger <- function(x)
{
    x_len <- length(x)
    if (x_len == 0L) {
        cat(class(x), "(0)\n", sep="")
        return()
    }
    cat(class(x), " of length ", x_len, ":\n", sep="")
    print(as.character(x), quote=FALSE, na.print="NA")
    return()
}

setMethod("show", "Linteger", function(object) .show_Linteger(object))

setMethod("showAsCell", "Linteger", function(object) as.character(object))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

combine_Linteger_objects <- function(objects)
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    ## If one of the objects to combine is a character vector, then all the
    ## objects are coerced to character and combined.
    if (any(vapply(objects, is.character, logical(1), USE.NAMES=FALSE))) {
        ans <- unlist(lapply(objects, as.character), use.names=FALSE)
        return(ans)
    }
    ## If one of the objects to combine is a double vector, then all the
    ## objects are coerced to double and combined.
    if (any(vapply(objects, is.double, logical(1), USE.NAMES=FALSE))) {
        ans <- unlist(lapply(objects, as.double), use.names=FALSE)
        return(ans)
    }
    ## Combine "bytes" slots.
    bytes_slots <- lapply(objects,
        function(x) {
            if (is.null(x))
                return(NULL)
            if (is.logical(x) || is.integer(x))
                x <- as.Linteger(x)
            if (is.Linteger(x))
                return(x@bytes)
            stop(wmsg("cannot combine Linteger objects ",
                      "with objects of class ", class(x)))
        }
    )
    ans_bytes <- unlist(bytes_slots, use.names=FALSE)
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}

setMethod("c", "Linteger",
    function (x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for Linteger objects ",
                 "does not support the 'recursive' argument")
        if (missing(x)) {
            objects <- list(...)
            x <- objects[[1L]]
        } else {
            objects <- list(x, ...)
        }
        combine_Linteger_objects(objects)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### is.na(), anyNA()
###

setMethod("is.na", "Linteger", function(x) is.na(as.logical(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Operations from "Ops" group
###

setMethod("Ops", c("Linteger", "Linteger"),
    function(e1, e2)
    {
        .Call("Linteger_Ops", .Generic, e1, e2, PACKAGE="S4Vectors")
    }
)

### If one operand is Linteger and the other one is integer, then the latter
### is coerced to Linteger.
### If one operand is Linteger and the other one is double, then the former
### is coerced to double.
setMethod("Ops", c("Linteger", "numeric"),
    function(e1, e2)
    {
        if (is.integer(e2)) {
            e2 <- as.Linteger(e2)
        } else {
            ## Suppress "non reversible coercion to double" warning.
            e1 <- suppressWarnings(as.double(e1))
        }
        callGeneric()
    }
)

setMethod("Ops", c("numeric", "Linteger"),
    function(e1, e2)
    {
        if (is.integer(e1)) {
            e1 <- as.Linteger(e1)
        } else {
            ## Suppress "non reversible coercion to double" warning.
            e2 <- suppressWarnings(as.double(e2))
        }
        callGeneric()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Operations from "Summary" group
###

setMethod("Summary", "Linteger",
    function(x, ..., na.rm=FALSE)
    {
        if (length(list(...)) != 0L)
            stop(wmsg("\"", .Generic, "\" method for Linteger objects ",
                      "takes only one object"))
        if (!isTRUEorFALSE(na.rm))
            stop("'na.rm' must be TRUE or FALSE")
        .Call("Linteger_Summary", .Generic, x, na.rm=na.rm,
              PACKAGE="S4Vectors")
    }
)

