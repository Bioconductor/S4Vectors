test_Vector_comparison <- function() {
    # Creating a dummy Vector class, and implementing the
    # minimum operations required to get all comparison methods.
    setClass("AaronStuff", contains="Vector", slots=c(stuff="integer"))
    setMethod("parallel_slot_names", "AaronStuff", function(x) c("stuff", callNextMethod()))

    setMethod("sameAsPreviousROW", "AaronStuff", function(x) sameAsPreviousROW(x@stuff))
    setMethod("order", "AaronStuff",
        function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
            everything <- list(...)
            everything <- lapply(everything, slot, "stuff")
            do.call(order, c(everything, list(na.last=na.last, decreasing=decreasing, method=method)))
        }
    )

    x <- as.integer(c(9,1,3,5,6,3,2,7,6,3,2,21))
    a <- new("AaronStuff", stuff=x)

    # Basic checks.
    checkIdentical(order(x), order(a))

    checkIdentical(sameAsPreviousROW(x), c(FALSE, x[-1]==head(x, -1)))
    checkIdentical(sameAsPreviousROW(x), sameAsPreviousROW(a))

    sx <- sort(x)
    checkIdentical(sameAsPreviousROW(sx), c(FALSE, sx[-1]==head(sx, -1)))
    checkIdentical(sameAsPreviousROW(sx), sameAsPreviousROW(sort(a)))

    checkIdentical(sameAsPreviousROW(x[0]), logical(0)) # robust to empty inputs.
    checkIdentical(sameAsPreviousROW(a[0]), logical(0))

    checkIdentical(sameAsPreviousROW(c(NA, 1L, 2L)), logical(3)) # robust to NA values.
    checkIdentical(sameAsPreviousROW(c(NA, NA, 2L)), c(FALSE, TRUE, FALSE))
    checkIdentical(sameAsPreviousROW(c(NA, NA, NaN, NaN)), c(FALSE, TRUE, FALSE, TRUE))

    # Checking selfmatch.
    checkIdentical(selfmatch(x), match(x, x))
    checkIdentical(selfmatch(a), selfmatch(x))
    checkIdentical(selfmatch(a[0]), integer(0))

    # Checking xtfrm.
    checkIdentical(order(xtfrm(a)), order(x))
    checkIdentical(rank(xtfrm(a)), rank(x)) # checking ties are the same.

    # Checking match.
    y <- as.integer(c(7, 2, 4, 4, 6, 5, 9, 6, 4))
    b <- new("AaronStuff", stuff=y)

    checkIdentical(match(a, b), match(x, y))
    checkIdentical(match(b, a), match(y, x))

    # Checking pcompare.
    ref <- pcompare(x, rev(x))
    checkEqualsNumeric(ref, sign(x - rev(x)))
    checkIdentical(ref, pcompare(a, rev(a)))

    checkIdentical(pcompare(a, a), integer(length(a)))
    checkIdentical(pcompare(a, new("AaronStuff", stuff=x-1L)), rep(1L, length(a)))
    checkIdentical(pcompare(a, new("AaronStuff", stuff=x+1L)), rep(-1L, length(a)))

    checkIdentical(pcompare(x, x), integer(length(x)))
    checkIdentical(pcompare(x, x-1L), rep(1L, length(x)))
    checkIdentical(pcompare(x, x+1L), rep(-1L, length(x)))
}
