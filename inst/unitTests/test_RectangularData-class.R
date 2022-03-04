test_RectangularData_basic <- function() {
  x <- DataFrame(a = 1:10, b = 11:20)
  y <- as.data.frame(x)

  checkIdentical(x[,1], y[,1])
  checkIdentical(as.data.frame(x[,2:1]), y[,2:1])
#  checkIdentical(as.data.frame(cbind(x,x)), cbind(y,y))
  checkIdentical(dim(x), dim(y))
  checkIdentical(nrow(x), nrow(y))
  checkIdentical(ncol(x), ncol(y))
  checkIdentical(as.data.frame(head(x)), head(y))
  checkIdentical(as.data.frame(rbind(x,x)), rbind(y,y))
#  checkIdentical(as.data.frame(tail(x)), tail(y))
}

test_RectangularData_subset <- function() {
  y <- airquality
  rownames(y) <- as.character(seq_len(nrow(y)))
  x <- as(y, "DataFrame")
  checkIdentical(as.data.frame(subset(x, Temp > 80, select = c(Ozone, Temp))),
                 subset(y, Temp > 80, select = c(Ozone, Temp)))
  checkIdentical(as.data.frame(subset(x, Day == 1, select = -Temp)),
                 subset(y, Day == 1, select = -Temp))
  checkIdentical(as.data.frame(subset(x, select = Ozone:Wind)),
                 subset(y, select = Ozone:Wind))
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

    # This should trigger a warning.
    Y$dup <- "bobbity"
    out <- combineUniqueCols(X, Y, Z)
    checkIdentical(out$dup, letters[1:4])

    # Unary case works correctly.
    checkIdentical(combineUniqueCols(X), X)

    # Handles nested DFs properly.
    x <- DataFrame(X=I(DataFrame(A=1:5)), row.names=1:5)
    y <- DataFrame(X=I(DataFrame(A=1:5)), row.names=1:5)
    out <- combineUniqueCols(x, y)
    checkIdentical(out$X$A, 1:5)

    y2 <- DataFrame(X=I(DataFrame(A=2:6, B=letters[1:5])), row.names=2:6)
    out <- combineUniqueCols(x, y2) # should trigger a warning where X$B is effectively dropped.
    checkIdentical(colnames(out), "X")
    checkIdentical(colnames(out$X), "A")
    checkIdentical(out$X$A, c(1:5, NA))
}

test_combineUniqueCols_unnamed <- function() {
    # Incidentally, this also checks that we use the 2D API.
    setMethod("combineCols", "matrix", function(x, ..., use.names=TRUE) cbind(x, ...))
    m1 <- m2 <- matrix(1:12, ncol=3)

    # Handles unnamed inputs.
    out <- combineUniqueCols(m1, m2)
    checkIdentical(out, cbind(m1, m2))

    # Supports mixed named/unnamed inputs.
    colnames(m2) <- LETTERS[1:3]
    out <- combineUniqueCols(m1, m2)
    checkIdentical(out, cbind(m1, m2))

    # Duplicate named columns are removed.
    out <- combineUniqueCols(m1, m2, m1, m2)
    checkIdentical(out, cbind(m1, m2, m1))
}

