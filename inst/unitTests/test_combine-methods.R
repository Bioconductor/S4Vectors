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

test_combineUniqueCols <- function() {
    X <- DataFrame(x=1, dup=letters[1:3])
    Y <- DataFrame(y="A", dup=letters[1:3])
    Z <- DataFrame(z=TRUE, dup=letters[1:3])

    out <- combineUniqueCols(X, Y, Z, use.names=FALSE)
    checkIdentical(colnames(out), c("x", "dup", "y", "z"))
    checkIdentical(out$dup, letters[1:3])

    Y$dup <- letters[4:6]
    out <- combineUniqueCols(X, Y, Z, use.names=FALSE) # should trigger a warning.
    checkIdentical(colnames(out), c("x", "dup", "y", "z"))
    checkIdentical(out$dup, letters[1:3])

    # Trying again with some more complexity. 
    X <- DataFrame(x=1, dup=letters[1:3], row.names=c("foo", "bar", "whee"))
    Y <- DataFrame(y="A", dup=letters[1], row.names="foo")
    Z <- DataFrame(z=TRUE, dup=letters[3:4], row.names=c("whee", "zun"))

    out <- combineUniqueCols(X, Y, Z)
    checkIdentical(rownames(out), c("foo", "bar", "whee", "zun"))
    checkIdentical(out$dup, letters[1:4])
    checkIdentical(out$x, c(1, 1, 1, NA))
    checkIdentical(out$y, c("A", NA, NA, NA))
    checkIdentical(out$z, c(NA, NA, TRUE, TRUE))

    # Fills in the offending column with NA's. 
    AA <- DataFrame(aa=5:6, row.names=c("foo", "BLAH"))
    out <- combineUniqueCols(X, Y, Z, AA)
    checkIdentical(rownames(out), c("foo", "bar", "whee", "zun", "BLAH"))
    checkIdentical(out$dup, c(letters[1:4], NA))
    checkIdentical(out$aa, c(5L, NA, NA, NA, 6L))

    Y$dup <- "bobbity"
    out <- combineUniqueCols(X, Y, Z) # should trigger a warning.
    checkIdentical(out$dup, letters[1:4])
}
