library(IRanges)  # for IntegerList

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

  ## Combining ordinary lists with other list-like objects
  DF1 <- DataFrame(A=I(list(11:12, 21:23)), B=IntegerList(101:105, 201))
  target <- DataFrame(A=I(c(DF1[[1]], DF1[[1]])),
                      B=I(c(DF1[[2]], DF1[[2]])))
  checkIdentical(target, rbind(DF1, DF1))
  DF2 <- DataFrame(A=IntegerList(31:34), B=I(list(301:302)))
  target <- DataFrame(A=I(c(DF1[[1]], as.list(DF2[[1]]))),
                      B=I(c(as.list(DF1[[2]]), DF2[[2]])))
  checkIdentical(target, rbind(DF1, DF2))
  target <- DataFrame(A=I(c(as.list(DF2[[1]]), DF1[[1]])),
                      B=I(c(DF2[[2]], as.list(DF1[[2]]))))
  checkIdentical(target, rbind(DF2, DF1))

  ## Combining factors
  df1 <- data.frame(species = factor(c("Z", "Y"), levels = LETTERS),
                    n = c(5, 6))
  DF1 <- DataFrame(df1)
  df2 <- data.frame(species = c("Human", "Chimp"),
                    n = c(1, 2))
  DF2 <- DataFrame(df2)
  df12 <- rbind(df1, df2)
  rownames(df12) <- NULL
  checkIdentical(as.data.frame(rbind(DF1, DF2)), df12)
  DF21 <- rbind(DF2, DF1)  # deviates from base::rbind.data.frame()
  target_species <- c(factor(DF2$species, levels=unique(DF2$species)),
                      DF1$species)
  checkIdentical(target_species, DF21$species)
 
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

    # A slightly tricky situation.
    DF1 <- DataFrame()
    DF2 <- DataFrame(ref=IRanges(1:2, 10))
    checkIdentical(DF2, combineRows(DF1, DF2))
    checkIdentical(DF2, combineRows(DF2, DF1))

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
    checkIdentical(combineCols(X), X)
}

