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

