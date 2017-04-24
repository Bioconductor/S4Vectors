### =========================================================================
### "shiftApply" methods
### -------------------------------------------------------------------------


setGeneric("shiftApply", signature=c("X", "Y"),
    function(SHIFT, X, Y, FUN, ..., OFFSET=0L, simplify=TRUE, verbose=FALSE)
        standardGeneric("shiftApply")
)

.Vector_shiftApply <- function(SHIFT, X, Y, FUN, ..., OFFSET=0L, simplify=TRUE,
                               verbose=FALSE)
{
    FUN <- match.fun(FUN)
    N <- length(X)
    if (N != length(Y))
        stop("'X' and 'Y' must be of equal length")

    if (!is.integer(SHIFT))
        SHIFT <- as.integer(SHIFT)
    if (length(SHIFT) == 0 || anyMissingOrOutside(SHIFT, 0L))
        stop("all 'SHIFT' values must be non-negative")

    if (!is.integer(OFFSET))
        OFFSET <- as.integer(OFFSET)
    if (length(OFFSET) == 0 || anyMissingOrOutside(OFFSET, 0L))
        stop("'OFFSET' must be non-negative")

    ## Perform X setup
    shiftedStartX <- rep.int(1L + OFFSET, length(SHIFT))
    shiftedEndX <- N - SHIFT

    ## Perform Y setup
    shiftedStartY <- 1L + SHIFT
    shiftedEndY <- rep.int(N - OFFSET, length(SHIFT))

    if (verbose) {
        maxI <- length(SHIFT)
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i) {
                     cat("\r", i, "/", maxI)
                     FUN(Vector_window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         Vector_window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...)
                 }, simplify = simplify)
        cat("\n")
    } else {
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i)
                     FUN(Vector_window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         Vector_window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...),
                 simplify = simplify)
    }
    ans
}

setMethod("shiftApply", signature(X="Vector", Y="Vector"),
          .Vector_shiftApply)

setMethod("shiftApply", signature(X="vector", Y="vector"),
          .Vector_shiftApply)

