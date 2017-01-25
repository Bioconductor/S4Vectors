### =========================================================================
### Linteger objects
### -------------------------------------------------------------------------
###
### The Linteger class is a container for storing a vector of "large integers"
### (i.e. long long int in C). It supports NAs.
###


### We don't support names for now. We will when we need them.
setClass("Linteger", representation(bytes="raw"))

.BYTES_PER_LINTEGER <- .Machine$sizeof.longlong

setMethod("length", "Linteger",
    function(x) length(x@bytes) %/% .BYTES_PER_LINTEGER
)

Linteger <- function(length=0L)
{
    if (!isSingleNumber(length) || length < 0L)
        stop("invalid 'length' argument")
    ans_bytes <- raw(length * .BYTES_PER_LINTEGER)
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coerce
###

### Called from the .onLoad hook in zzz.R
from_logical_to_Linteger <- function(from)
{
    ans_bytes <- .Call2("new_Linteger_bytes_from_LOGICAL", from,
                        PACKAGE="S4Vectors")
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}
setAs("logical", "Linteger", from_logical_to_Linteger)

.from_integer_to_Linteger <- function(from)
{
    ans_bytes <- .Call2("new_Linteger_bytes_from_INTEGER", from,
                        PACKAGE="S4Vectors")
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}
setAs("integer", "Linteger", .from_integer_to_Linteger)

.from_numeric_to_Linteger <- function(from)
{
    ans_bytes <- .Call2("new_Linteger_bytes_from_NUMERIC", from,
                        PACKAGE="S4Vectors")
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}
setAs("numeric", "Linteger", .from_numeric_to_Linteger)

.from_character_to_Linteger <- function(from)
{
    ans_bytes <- .Call2("new_Linteger_bytes_from_CHARACTER", from,
                        PACKAGE="S4Vectors")
    new2("Linteger", bytes=ans_bytes, check=FALSE)
}
setAs("character", "Linteger", .from_character_to_Linteger)

as.Linteger <- function(x) as(x, "Linteger")

### S3/S4 combo for as.logical.Linteger
.from_Linteger_to_logical <- function(x)
{
    .Call2("new_LOGICAL_from_Linteger_bytes", x@bytes, PACKAGE="S4Vectors")
}
as.logical.Linteger <- function(x, ...) .from_Linteger_to_logical(x, ...)
setMethod("as.logical", "Linteger", as.logical.Linteger)

### S3/S4 combo for as.integer.Linteger
.from_Linteger_to_integer <- function(x)
{
    .Call2("new_INTEGER_from_Linteger_bytes", x@bytes, PACKAGE="S4Vectors")
}
as.integer.Linteger <- function(x, ...) .from_Linteger_to_integer(x, ...)
setMethod("as.integer", "Linteger", as.integer.Linteger)

### S3/S4 combo for as.numeric.Linteger
.from_Linteger_to_numeric <- function(x)
{
    .Call2("new_NUMERIC_from_Linteger_bytes", x@bytes, PACKAGE="S4Vectors")
}
as.numeric.Linteger <- function(x, ...) .from_Linteger_to_numeric(x, ...)
setMethod("as.numeric", "Linteger", as.numeric.Linteger)

### S3/S4 combo for as.character.Linteger
.from_Linteger_to_character <- function(x)
{
    .Call2("new_CHARACTER_from_Linteger_bytes", x@bytes, PACKAGE="S4Vectors")
}
as.character.Linteger <- function(x, ...) .from_Linteger_to_character(x, ...)
setMethod("as.character", "Linteger", as.character.Linteger)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "Ops" group generics
###

setMethod("is.na", "Linteger", function(x) is.na(as.logical(x)))

setMethod("Ops", c("Linteger", "Linteger"),
    function(e1, e2)
    {
        .Call("Linteger_Ops", .Generic, e1@bytes, e2@bytes,
              PACKAGE="S4Vectors")
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

