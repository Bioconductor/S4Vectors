
test_selfmatchIntegerQuads <- function()
{
    a <- c(  1L,   1L,   1L, 1L,   1L,   2L,   1L)
    b <- c(  0L,   0L,   0L, 0L,   1L,   0L,   0L)
    c <- c(-50L, -50L, -50L, 8L, -50L, -50L, -50L)
    d <- c(  6L,   5L,   6L, 6L,   6L,   6L,   5L)

    target <- c(1L, 2L, 1L, 4L, 5L, 6L, 2L)
    current <- selfmatchIntegerQuads(a, b, c, d, method="hash")
    checkIdentical(current, target)
    current <- selfmatchIntegerQuads(a, b, c, d, method="quick")
    checkIdentical(current, target)
}

