# Tests the various combining methods.

test_combineRows <- function() {
    X <- DataFrame(x=1)
    Y <- DataFrame(x=2, y="A")
    Z <- DataFrame(z=TRUE)
    
    out <- combineRows(X, Y, Z)
    checkIdentical(out$x, c(1,2,NA))
    checkIdentical(out$y, c(NA,"A",NA))
    checkIdentical(out$z, c(NA,NA,TRUE))

    # Robust to no-rows.
    out <- combineRows(X, Y, Z[0,,drop=FALSE])
    checkIdentical(out$x, c(1,2))
    checkIdentical(out$y, c(NA,"A"))
    checkIdentical(out$z, c(NA,NA))
}

test_combineCols <- function() {
    X <- DataFrame(x=1)
    Y <- DataFrame(y="A")
    Z <- DataFrame(z=TRUE)

    # Checking cbind-like behavior is consistent.
    checkIdentical(combineCols(X, Y, Z, use.names=FALSE), cbind(X, Y, Z))
    checkException(combineCols(X, Y, Z[0,,drop=FALSE], use.names=FALSE), silent=TRUE)

    Y <- DataFrame(y=LETTERS[1:2])
    rownames(X) <- "foo"
    rownames(Y) <- c("foo", "bar")
    checkException(combineCols(X, Y, Z), silent=TRUE)

    rownames(Z) <- "bar"
    out <- combineCols(X, Y, Z)
    checkIdentical(out$x, c(1, NA))
    checkIdentical(out$y, LETTERS[1:2])
    checkIdentical(out$z, c(NA, TRUE))
}
