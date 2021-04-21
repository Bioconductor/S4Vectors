test_DataFrame_rbind <- function() {
  data(swiss)
  rn <- rownames(swiss)
  sw <- DataFrame(swiss, row.names=rn)
  swisssplit <- split(swiss, swiss$Education)
 
  ## rbind
  checkIdentical(rbind(DataFrame(), DataFrame()), DataFrame())
  score <- c(X=1L, Y=3L, Z=NA)
  DF <- DataFrame(score)
  checkIdentical(rbind(DF, DF)[[1]], c(score, score))
  zr <- sw[FALSE,]
  checkIdentical(rbind(DataFrame(), zr, zr[,1:2]), zr)
  checkIdentical(as.data.frame(rbind(DataFrame(), zr, sw)), swiss)
  target <- do.call(rbind, swisssplit)
  current <- do.call(rbind, lapply(swisssplit, DataFrame))
  rownames(target) <- rownames(current) <- NULL
  checkIdentical(target, as.data.frame(current))
  DF <- DataFrame(A=I(list(1:3)))
  df <- as.data.frame(DF)
  checkIdentical(as.data.frame(rbind(DF, DF)), rbind(df, df))
  
  ## combining factors
  df1 <- data.frame(species = c("Mouse", "Chicken"), n = c(5, 6))
  DF1 <- DataFrame(df1)
  df2 <- data.frame(species = c("Human", "Chimp"), n = c(1, 2))
  DF2 <- DataFrame(df2)
  df12 <- rbind(df1, df2)
  rownames(df12) <- NULL
  checkIdentical(as.data.frame(rbind(DF1, DF2)), df12)
 
  checkIdentical(rownames(rbind(sw, DataFrame(swiss))),
                 c(rownames(swiss), rownames(swiss)))
  checkIdentical(rownames(do.call(rbind, lapply(swisssplit, DataFrame))),
                 unlist(lapply(swisssplit, rownames), use.names=FALSE))

  checkException(rbind(sw[,1:2], sw), silent = TRUE)
  other <- sw
  colnames(other)[1] <- "foo"
  checkException(rbind(other, sw), silent = TRUE)
}

test_DataFrame_combineRows <- function() {
    X <- DataFrame(x=1)
    Y <- DataFrame(x=2, y="A")
    Z <- DataFrame(z=TRUE)

    checkIdentical(Y, combineRows(Y))

    out <- combineRows(X, Y, Z)
    checkIdentical(out$x, c(1,2,NA))
    checkIdentical(out$y, c(NA,"A",NA))
    checkIdentical(out$z, c(NA,NA,TRUE))

    # Robust to no-rows.
    out <- combineRows(X, Y, Z[0,,drop=FALSE])
    checkIdentical(out$x, c(1,2))
    checkIdentical(out$y, c(NA,"A"))
    checkIdentical(out$z, c(NA,NA))

    # A more complex situation.
    x <- DataFrame(A=Rle(101:103, 3:1), A=letters[1:6], B=Rle(51:52, c(1, 5)),
                   check.names=FALSE)
    y <- DataFrame(B=Rle(c("a", "b")), A=runif(2))
    target <- DataFrame(A=c(S4Vectors:::decodeRle(x[[1]]), y[[2]]),
                        A=c(x[[2]], c(NA, NA)),
                        B=c(x[[3]], y[[1]]),
                        check.names=FALSE)
    current <- combineRows(x, y)
    checkIdentical(target, current)
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

    # Unary cases work correctly.
    checkIdentical(combineCols(x=X), X)
    checkIdentical(combineCols(y=X), X)
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
    checkIdentical(combineUniqueCols(x=X), X)
    checkIdentical(combineUniqueCols(y=X), X)
}

test_combineUniqueCols_unnamed <- function() {
    # Incidentally, this also checks that we use the 2D API.
    setMethod("combineCols", "matrix", function(x, y, ..., use.names=TRUE) cbind(x, y, ...))
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
