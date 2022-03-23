test_bindROWS2 <- function() {

    ## Combine atomic vectors.

    x0 <- character(0)
    x1 <- 11:14
    checkIdentical(x1, S4Vectors:::bindROWS2(x0, list(x1)))
    checkIdentical(x1, S4Vectors:::bindROWS2(x1, list(x0)))

    checkIdentical(as.character(10:14), S4Vectors:::bindROWS2("10", list(x1)))
    checkIdentical(as.character(11:15), S4Vectors:::bindROWS2(x1, list("15")))

    ## Combine factors.

    f1 <- factor(levels=c("z", "a", "t"))
    f2 <- factor(c("a", "Y", "a"), levels=c("X", "t", "a", "B", "Y"))

    current <- S4Vectors:::bindROWS2(f1, list(f2))
    target <- factor(c("a", "Y", "a"), levels=c("z", "a", "t", "X", "B", "Y"))
    checkIdentical(target, current)
    checkIdentical(target, S4Vectors:::bindROWS2(f1, list(f1, f2, f1)))

    current <- S4Vectors:::bindROWS2(f2, list(f1))
    target <- factor(c("a", "Y", "a"), levels=c("X", "t", "a", "B", "Y", "z"))
    checkIdentical(target, current)

    current <- S4Vectors:::bindROWS2(f1, list(f1))
    checkIdentical(f1, current)

    ## Combine factors and atomic vectors.

    x <- c("B", "A", "z", "z", "t", "Y", "A", "z")

    current <- S4Vectors:::bindROWS2(f1, list(x, f2))
    target <- factor(c(x, "a", "Y", "a"),
                     levels=unique(c(levels(f1), x, levels(f2))))
    checkIdentical(target, current)

    ## Combine Rle's and non-Rle's.

    checkIdentical(x1, S4Vectors:::bindROWS2(Rle(x0), list(x1)))
    checkIdentical(x1, S4Vectors:::bindROWS2(x0, list(Rle(x1))))
    checkIdentical(x1, S4Vectors:::bindROWS2(Rle(x1), list(x0)))
    checkIdentical(x1, S4Vectors:::bindROWS2(x1, list(Rle(x0))))

    target <- S4Vectors:::bindROWS2(f1, list(f2))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(f1, list(Rle(f2))))
    target <- S4Vectors:::bindROWS2(f2, list(f1))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f2), list(f1)))
    checkIdentical(target, S4Vectors:::bindROWS2(f2, list(Rle(f1))))

    target <- S4Vectors:::bindROWS2(f1, list(x, f2))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(x, f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(f1, list(Rle(x), f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(f1, list(x, Rle(f2))))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(Rle(x), f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(x, Rle(f2))))
    checkIdentical(target, S4Vectors:::bindROWS2(f1, list(Rle(x), Rle(f2))))

    ## Combine Rle's.

    checkIdentical(Rle(x1), S4Vectors:::bindROWS2(Rle(x0), list(Rle(x1))))
    checkIdentical(Rle(x1), S4Vectors:::bindROWS2(Rle(x1), list(Rle(x0))))

    a <- Rle(factor(levels="a"))
    b <- Rle(factor("b"))
    target <- Rle(factor("b", levels=c("a", "b")))
    checkIdentical(target, S4Vectors:::bindROWS2(a, list(b)))
    target <- Rle(factor("b", levels=c("b", "a")))
    checkIdentical(target, S4Vectors:::bindROWS2(b, list(a)))

    target <- Rle(S4Vectors:::bindROWS2(f1, list(f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(Rle(f2))))
    target <- Rle(S4Vectors:::bindROWS2(f2, list(f1)))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f2), list(Rle(f1))))

    target <- Rle(S4Vectors:::bindROWS2(f1, list(x, f2)))
    checkIdentical(target, S4Vectors:::bindROWS2(Rle(f1), list(Rle(x), Rle(f2))))
}

