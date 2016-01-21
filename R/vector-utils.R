### =========================================================================
### Some low-level (not exported) utility functions to operate on ordinary
### vectors (including lists and data frames)
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
###


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
  if (length(clnames) == 1L) {
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### quick_unlist() and quick_unsplit()
###
### Both functions *assume* that 'x' is a list of length >= 1 with no names,
### and that the list elements in 'x' have the same type. But they don't
### actually check this!
###

quick_unlist <- function(x)
{
    x1 <- x[[1L]]
    if (is.factor(x1)) {
        ## Fast unlisting of a list of factors that all have the same levels
        ## in the same order.
        structure(unlist(x), class="factor", levels=levels(x1))
    } else {
        do.call(c, x)  # doesn't work on list of factors
    }
}

quick_unsplit <- function(x, f)
{
    idx <- split(seq_along(f), f)
    idx <- unlist(idx, use.names=FALSE)
    revidx <- integer(length(idx))
    revidx[idx] <- seq_along(idx)
    quick_unlist(x)[revidx]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_ranges_from_vectorORfactor()
###

### NOT exported.
extract_ranges_from_vectorORfactor <- function(x, start, width)
    .Call2("vectorORfactor_extract_ranges", x, start, width,
                                            PACKAGE="S4Vectors")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_data_frame_rows()
###
### A fast version of {df <- df[i, , drop=FALSE]; rownames(df) <- NULL}.
### Can be up to 20x or 30x faster when extracting millions of rows.
### What kills [.data.frame is the overhead of propagating the original
### rownames and trying to keep them unique with make.unique(). However, most
### of the time, nobody cares about the rownames so this effort is pointless
### and only a waste of time.
###

### NOT exported.
extract_data_frame_rows <- function(x, i)
{
    stopifnot(is.data.frame(x))
    ## The commented code should be as fast (or even faster, because 'i' is
    ## normalized only once) as the code below but unfortunately it's not.
    ## TODO: Investigate why and make it as fast as the code below.
    #i <- normalizeSingleBracketSubscript(i, x, exact=FALSE, as.NSBS=TRUE)
    #data.frame(lapply(x, extractROWS, i),
    #           check.names=FALSE, stringsAsFactors=FALSE)
    i <- normalizeSingleBracketSubscript(i, x, exact=FALSE)
    data.frame(lapply(x, "[", i),
               check.names=FALSE, stringsAsFactors=FALSE)
}

