### Make toy ordinary factor with 10 levels.
.make_toy_factor1 <- function()
{
    f1 <- factor(c("L4", "L4", "L2"), levels=paste0("L", 1:10))
    names(f1) <- letters[1:3]
    f1
}

### Make toy ordinary factor with 256 levels.
.make_toy_factor2 <- function()
{
    f2 <- factor(c("L259", "L5", "L4", "L10"), levels=paste0("L", 259:4))
    names(f2) <- LETTERS[1:4]
    f2
}

test_Factor_constructor <- function()
{
    x <- c(a="C", b="C", c="D", d="A", e="B")
    F0 <- Factor(x)
    checkTrue(validObject(F0))
    checkIdentical(unique(x), levels(F0))
    checkTrue(is.raw(F0@index))
    checkIdentical(x, unfactor(F0))

    f1 <- .make_toy_factor1()
    F1 <- Factor(as.character(f1), levels(f1))
    checkTrue(validObject(F1))
    checkIdentical(levels(f1), levels(F1))
    checkTrue(is.raw(F1@index))
    checkIdentical(as.integer(f1), as.integer(F1))

    f2 <- .make_toy_factor2()
    F2 <- Factor(as.character(f2), levels(f2))
    checkTrue(validObject(F2))
    checkIdentical(levels(f2), levels(F2))
    checkTrue(is.integer(F2@index))
    checkIdentical(as.integer(f2), as.integer(F2))
}

test_Factor_droplevels <- function()
{
    F0 <- Factor(levels=paste0("L", 500:1), index=c(9:1, 248:255, 1:9))
    names(F0) <- letters

    F0pruned <- droplevels(F0)
    checkTrue(validObject(F0pruned))
    checkIdentical(unfactor(F0), unfactor(F0pruned))
    checkIdentical(droplevels(as.factor(F0)), as.factor(F0pruned))
    checkTrue(is.raw(F0pruned@index))

    F1 <- Factor(levels=levels(F0), index=as.raw(F0))
    F1pruned <- droplevels(F1)
    checkTrue(validObject(F1pruned))
    checkIdentical(unfactor(F1), unfactor(F1pruned))
    checkIdentical(droplevels(as.factor(F1)), as.factor(F1pruned))
    checkTrue(is.raw(F1pruned@index))
}

test_Factor_coercion <- function()
{
    f0 <- factor()
    F0 <- as(f0, "Factor")
    checkTrue(validObject(F0))
    checkIdentical(levels(f0), levels(F0))
    checkTrue(is.raw(F0@index))
    checkIdentical(f0, as.factor(F0))

    f1 <- .make_toy_factor1()
    F1 <- as(f1, "Factor")
    checkTrue(validObject(F1))
    checkIdentical(levels(f1), levels(F1))
    checkTrue(is.raw(F1@index))
    checkIdentical(f1, as.factor(F1))

    f2 <- .make_toy_factor2()
    F2 <- as(f2, "Factor")
    checkTrue(validObject(F2))
    checkIdentical(levels(f2), levels(F2))
    checkTrue(is.integer(F2@index))
    checkIdentical(f2, as.factor(F2))
}

test_Factor_concatenation <- function()
{
    f1 <- .make_toy_factor1()
    f2 <- .make_toy_factor2()
    F1 <- as(f1, "Factor")
    F2 <- as(f2, "Factor")

    F1F1 <- c(F1, F1)
    checkTrue(validObject(F1F1))
    checkIdentical(c(f1, f1), as.factor(F1F1))

    F2F2 <- c(F2, F2)
    checkTrue(validObject(F2F2))
    checkIdentical(c(f2, f2), as.factor(F2F2))

    F1F2 <- c(F1, F2)
    checkTrue(validObject(F1F2))
    checkIdentical(c(f1, f2), as.factor(F1F2))

    F2F1 <- c(F2, F1)
    checkTrue(validObject(F2F1))
    checkIdentical(c(f2, f1), as.factor(F2F1))

    ## some edge cases

    f0 <- factor()
    F0 <- as(f0, "Factor")

    F0F1 <- c(F0, F1)
    checkTrue(validObject(F0F1))
    checkIdentical(c(f0, f1), as.factor(F0F1))
    F1F0 <- c(F1, F0)
    checkTrue(validObject(F1F0))
    checkIdentical(c(f1, f0), as.factor(F1F0))

    F0F2 <- c(F0, F2)
    checkTrue(validObject(F0F2))
    checkIdentical(c(f0, f2), as.factor(F0F2))
    F2F0 <- c(F2, F0)
    checkTrue(validObject(F2F0))
    checkIdentical(c(f2, f0), as.factor(F2F0))
}

