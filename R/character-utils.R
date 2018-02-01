### =========================================================================
### Some utility functions to operate on strings
### -------------------------------------------------------------------------


### NOT exported
capitalize <- function(x)
{
    substring(x, 1L, 1L) <- toupper(substring(x, 1L, 1L))
    x
}

### NOT exported
### Reduce size of each input string by keeping only its head and tail
### separated by 3 dots. Each returned strings is guaranteed to have a number
### characters <= width.
sketchStr <- function(x, width=23) 
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (!isSingleNumber(width))
        stop("'width' must be a single integer")
    if (!is.integer(width))
        width <- as.integer(width)
    if (width < 7L) 
        width <- 7L
    x_nchar <- nchar(x, type="width")
    idx <- which(x_nchar > width)
    if (length(idx) != 0L) {
        xx <- x[idx]
        xx_nchar <- x_nchar[idx]
        w1 <- (width - 2L) %/% 2L
        w2 <- (width - 3L) %/% 2L
        x[idx] <- paste0(substr(xx, start=1L, stop=w1),
                         "...",
                         substr(xx, start=xx_nchar - w2 + 1L, stop=xx_nchar))
    }
    x
}

setGeneric("unstrsplit", signature="x",
    function(x, sep="") standardGeneric("unstrsplit")
)

setMethod("unstrsplit", "list",
    function(x, sep="") .Call2("unstrsplit_list", x, sep, PACKAGE="S4Vectors")
)

setMethod("unstrsplit", "character",
    function(x, sep="") x
)

### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1L]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call2("safe_strexplode", x, PACKAGE="S4Vectors")
}

### svn.time() returns the time in Subversion format, e.g.:
###   "2007-12-07 10:03:15 -0800 (Fri, 07 Dec 2007)"
### The -0800 part will be adjusted if daylight saving time is in effect.
### TODO: Find a better home for this function.
svn.time <- function() .Call2("svn_time", PACKAGE="S4Vectors")

