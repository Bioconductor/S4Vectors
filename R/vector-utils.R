### =========================================================================
### Some low-level (not exported) utility functions to operate on ordinary
### vectors
### -------------------------------------------------------------------------


sapply_NROW <- function(x)
{
    if (!is.list(x))
        x <- as.list(x)
    ans <- try(.Call2("sapply_NROW", x, PACKAGE="S4Vectors"), silent=TRUE)
    if (!inherits(ans, "try-error")) {
        names(ans) <- names(x)
        return(ans)
    }
    ## From here, 'length(x)' is guaranteed to be != 0
    return(sapply(x, NROW))
}

listElementType <- function(x) {
  cl <- lapply(x, class)
  clnames <- unique(unlist(cl, use.names=FALSE))
  if (length(clnames == 1L)) {
    clnames
  } else {
    contains <- lapply(cl, function(x) getClass(x, TRUE)@contains)
    clnames <- c(clnames,
                 unlist(lapply(contains, names), use.names=FALSE))
    cltab <- table(factor(clnames, unique(clnames)))
    clnames <- names(cltab)[cltab == length(x)]
    if (length(clnames) > 0L) {
      clnames[1]
    } else {
      NULL
    }
  }
}

