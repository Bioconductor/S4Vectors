### =========================================================================
### Some low-level (not exported) utility functions used by various "show"
### methods
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selectSome()
###

### taken directly from Biobase, then added 'ellipsisPos' argument
selectSome <- function(obj, maxToShow = 5, ellipsis = "...",
                       ellipsisPos = c("middle", "end", "start"), quote=FALSE) 
{
  if(is.character(obj) && quote)
      obj <- sQuote(obj)
  ellipsisPos <- match.arg(ellipsisPos)
  len <- length(obj)
  if (maxToShow < 3) 
    maxToShow <- 3
  if (len > maxToShow) {
    maxToShow <- maxToShow - 1
    if (ellipsisPos == "end") {
      c(head(obj, maxToShow), ellipsis)
    } else if (ellipsisPos == "start") {
      c(ellipsis, tail(obj, maxToShow))
    } else {
      bot <- ceiling(maxToShow/2)
      top <- len - (maxToShow - bot - 1)
      nms <- obj[c(1:bot, top:len)]
      c(as.character(nms[1:bot]), ellipsis, as.character(nms[-c(1:bot)]))
    }
  } else {
    obj
  }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### coolcat()
###

### Exported!
coolcat <- function(fmt, vals=character(), exdent=2, ...)
{
    vals <- ifelse(nzchar(vals), vals, "''")
    lbls <- paste(selectSome(vals), collapse=" ")
    txt <- sprintf(fmt, length(vals), lbls)
    cat(strwrap(txt, exdent=exdent, ...), sep="\n")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### labeledLine()
###

.qualifyByName <- function(x, qualifier="=") {
    nms <- names(x)
    x <- as.character(x)
    aliased <- nzchar(nms)
    x[aliased] <- paste0(nms[aliased], qualifier, x[aliased])
    x
}

.padToAlign <- function(x) {
    whitespace <- paste(rep(" ", getOption("width")), collapse="")
    padlen <- max(nchar(x)) - nchar(x)
    substring(whitespace, 1L, padlen)
}

.ellipsize <-
  function(obj, width = getOption("width"), sep = " ", ellipsis = "...",
           pos = c("middle", "end", "start"))
{
  pos <- match.arg(pos)
  if (is.null(obj))
    obj <- "NULL"
  if (is.factor(obj))
    obj <- as.character(obj)
  ## get order selectSome() would print
  if (pos == "middle") {
    if (length(obj) > 2 * width)
      obj <- c(head(obj, width), tail(obj, width))
    half <- seq_len(ceiling(length(obj) / 2L))
    ind <- as.vector(rbind(half, length(obj) - half + 1L))
  } else if (pos == "end") {
    obj <- head(obj, width)
    ind <- seq_len(length(obj))
  } else {
    obj <- tail(obj, width)
    ind <- rev(seq_len(length(obj)))
  }
  str <- encodeString(obj)
  nc <- cumsum(nchar(str[ind]) + nchar(sep)) - nchar(sep)
  last <- findInterval(width, nc)
  if (length(obj) > last) {
    ## make sure ellipsis fits
    while (last &&
           (nc[last] + nchar(sep)*2^(last>1) + nchar(ellipsis)) > width)
      last <- last - 1L
    if (last == 0) { ## have to truncate the first/last element
      if (pos == "start") {
        str <-
          paste(ellipsis,
                substring(tail(str, 1L),
                          nchar(tail(str, 1L))-(width-nchar(ellipsis))+1L,
                          nchar(ellipsis)),
                sep = "")
      } else {
        str <-
          paste(substring(str[1L], 1, width - nchar(ellipsis)), ellipsis,
                sep = "")
      }
    }
    else if (last == 1) { ## can only show the first/last
      if (pos == "start")
        str <- c(ellipsis, tail(str, 1L))
      else str <- c(str[1L], ellipsis)
    }
    else {
      str <- selectSome(str, last + 1L, ellipsis, pos)
    }
  }
  paste(str, collapse = sep)
}

labeledLine <-
    function(label, els, count = TRUE, labelSep = ":", sep = " ",
             ellipsis = "...", ellipsisPos = c("middle", "end", "start"),
             vectorized = FALSE, pad = vectorized)
{
  if (!is.null(els)) {
      label[count] <- paste(label, "(",
                            if (vectorized) lengths(els) else length(els),
                            ")", sep = "")[count]
      if (!is.null(names(els))) {
          els <- .qualifyByName(els)
      }
  }
  label <- paste(label, labelSep, " ", sep = "")
  if (pad) {
      label <- paste0(label, .padToAlign(label))
  }
  width <- getOption("width") - nchar(label)
  ellipsisPos <- match.arg(ellipsisPos)
  if (vectorized) {
      .ellipsize <- Vectorize(.ellipsize)
  }
  line <- .ellipsize(els, width, sep, ellipsis, ellipsisPos)
  paste(label, line, "\n", sep = "")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### get_showHeadLines(), set_showHeadLines()
### get_showTailLines(), set_showTailLines()
###

### Guaranteed to return a non-negative integer.
.get_showLines <- function(option, default=5L)
{
    n <- getOption(option, default=default)
    if (!isSingleNumber(n) || n > .Machine$integer.max)
        return(default)
    if (n <= 0)
        return(0L)
    if (!is.integer(n))
        n <- as.integer(n)
    n
}

### Return the previous 'n' value.
.set_showLines <- function(option, n)
{
    if (!isSingleNumber(n))
        stop(wmsg("'n' must be a single number"))
    if (n > .Machine$integer.max)
        stop(wmsg("'n' is too big"))
    if (n < 0)
        stop(wmsg("'n' cannot be negative"))
    prev <- .get_showLines(option)
    if (!is.integer(n))
        n <- as.integer(n)
    do.call(options, setNames(list(n), option))
    invisible(prev)
}

### Exported!
get_showHeadLines <- function() .get_showLines("showHeadLines")
set_showHeadLines <- function(n=5) .set_showLines("showHeadLines", n)
get_showTailLines <- function() .get_showLines("showTailLines")
set_showTailLines <- function(n=5) .set_showLines("showTailLines", n)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### printAtomicVectorInAGrid() and toNumSnippet()
###

### Exported!
printAtomicVectorInAGrid <- function(x, prefix="", justify="left")
{
    if (!is.character(x))
        x <- setNames(as.character(x), names(x))

    ## Nothing to print if length(x) is 0.
    if (length(x) == 0L)
        return(invisible(x))

    ## Determine the nb of cols in the grid.
    grid_width <- getOption("width") + 1L - nchar(prefix)
    cell_width <- max(3L, nchar(x), nchar(names(x)))
    ncol <- grid_width %/% (cell_width + 1L)

    ## Determine the nb of rows in the grid.    
    nrow <- length(x) %/% ncol
    remainder <- length(x) %% ncol
    if (remainder != 0L) {
        nrow <- nrow + 1L
        x <- c(x, character(ncol - remainder))
    }

    ## Print the grid.
    print_line <- function(y)
    {
        cells <- format(y, justify=justify, width=cell_width)
        cat(prefix, paste0(cells, collapse=" "), "\n", sep="")
    }
    print_grid_row <- function(i)
    {
        idx <- (i - 1L) * ncol + seq_len(ncol)
        slice <- x[idx]
        if (!is.null(names(slice)))
            print_line(names(slice))
        print_line(slice)
    }
    n1 <- get_showHeadLines()
    n2 <- get_showTailLines()
    if (nrow <= n1 + n2) {
        for (i in seq_len(nrow)) print_grid_row(i)
    } else {
        idx1 <- seq_len(n1)
        idx2 <- nrow - n2 + seq_len(n2)
        for (i in idx1) print_grid_row(i)
        print_line(rep.int("...", ncol))
        for (i in idx2) print_grid_row(i)
    }
    invisible(x)
}

### Works as long as length(), "[" and as.numeric() work on 'x'.
### Not exported.
toNumSnippet <- function(x, max.width)
{
    if (length(x) <= 2L)
        return(paste(format(as.numeric(x)), collapse=" "))
    if (max.width < 0L)
        max.width <- 0L
    ## Elt width and nb of elt to display if they were all 0.
    elt_width0 <- 1L
    nelt_to_display0 <- min(length(x), (max.width+1L) %/% (elt_width0+1L))
    head_ii0 <- seq_len(nelt_to_display0 %/% 2L)
    tail_ii0 <- length(x) + head_ii0 - length(head_ii0)
    ii0 <- c(head_ii0, tail_ii0)
    ## Effective elt width and nb of elt to display
    elt_width <- format.info(as.numeric(x[ii0]))[1L]
    nelt_to_display <- min(length(x), (max.width+1L) %/% (elt_width+1L))
    if (nelt_to_display == length(x))
        return(paste(format(as.numeric(x), width=elt_width), collapse=" "))
    head_ii <- seq_len((nelt_to_display+1L) %/% 2L)
    tail_ii <- length(x) + seq_len(nelt_to_display %/% 2L) - nelt_to_display %/% 2L
    ans_head <- format(as.numeric(x[head_ii]), width=elt_width)
    ans_tail <- format(as.numeric(x[tail_ii]), width=elt_width)
    ans <- paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
    if (nchar(ans) <= max.width || length(ans_head) == 0L)
        return(ans)
    ans_head <- ans_head[-length(ans_head)]
    ans <- paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
    if (nchar(ans) <= max.width || length(ans_tail) == 0L)
        return(ans)
    ans_tail <- ans_tail[-length(ans_tail)]
    paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### classNameForDisplay()
###

### Exported!
setGeneric("classNameForDisplay",
    function(x) standardGeneric("classNameForDisplay")
)

setMethod("classNameForDisplay", "ANY",
   function(x)
   {
       ## Selecting the 1st element guarantees that we return a single string
       ## (e.g. on an ordered factor, class(x) returns a character vector of
       ## length 2).
       class(x)[1L]
   }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### showAsCell()
###
### All "showAsCell" methods must return a character vector.
###

### Exported!
setGeneric("showAsCell", function(object) standardGeneric("showAsCell"))

### Should work on any matrix-like object e.g. ordinary matrix, Matrix,
### data.frame, DataFrame, data.table, etc...
### Should also work on any array-like object with more than 2 dimensions
### that supports "reshaping" via the dim() setter. Note that DelayedArray
### objects don't support this reshaping in general.
showAsCell_array <- function(object)
{
    if (length(dim(object)) > 2L) {
        ## Reshape 'object' as a 2D object.
        dim1 <- dim(object)[-1L]
        dim(object) <- c(nrow(object), prod(dim1))
    }
    object_ncol <- ncol(object)
    if (object_ncol == 0L)
        return(rep.int("", nrow(object)))
    first_cols <- lapply(seq_len(min(object_ncol, 3L)),
        function(j) showAsCell(object[ , j, drop=TRUE])
    )
    ans <- do.call(paste, c(first_cols, list(sep=":")))
    if (object_ncol > 3L)
        ans <- paste0(ans, ":...")
    ans
}

.default_showAsCell <- function(object)
{
    ## Some objects like SplitDataFrameList have a "dim" method that
    ## returns a non-MULL object (a matrix!) even though they don't have
    ## an array-like semantic.
    if (length(dim(object)) >= 2L && !is.matrix(dim(object)))
        return(showAsCell_array(object))
    object_NROW <- NROW(object)
    if (object_NROW == 0L)
        return(character(0L))
    attempt <- try(as.character(object), silent=TRUE)
    if (!is(attempt, "try-error"))
        return(showAsCell(attempt))
    if (object_NROW == 1L)
        return(paste0("<", classNameForDisplay(object), ">"))
    rep.int("####", object_NROW)
}

setMethod("showAsCell", "ANY", .default_showAsCell)

setMethod("showAsCell", "numeric",
    function(object)
    {
        if (is.integer(object))
            return(as.character(object))
        format(object, digits=6L)
    }
)

setMethod("showAsCell", "character",
    function(object)
    {
        nc <- nchar(object, type="chars")
        trim_idx <- which(nc > 22L)
        if (length(trim_idx) != 0L) {
            tmp <- substr(object[trim_idx], start=1L, stop=20L)
            object[trim_idx] <- paste0(tmp, "..")
        }
        object
    }
)

setMethod("showAsCell", "AsIs",
    function(object) showAsCell(drop_AsIs(object))
)

showAsCell_list <- function(object)
{
    vapply(object,
        function(x) {
            ## 'x' is not necessarily subsettable so if its length is 1 (e.g.
            ## 'x' is a BamFile object) we avoid the risky subsetting.
            if (NROW(x) == 1L)
                return(showAsCell(x))
            ## 'head(x, 3L)' is still no guaranteed to work.
            x3 <- try(head(x, 3L), silent=TRUE)
            if (is(x3, "try-error"))
                return("####")
            str <- paste(showAsCell(x3), collapse=",")
            if (length(x) > 3L)
                str <- paste0(str, ",...")
            str
        },
        character(1L),
        USE.NAMES=FALSE
    )
}
setMethod("showAsCell", "list", showAsCell_list)

setMethod("showAsCell", "data.frame", showAsCell_array)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### makeNakedCharacterMatrixForDisplay() and
### makePrettyMatrixForCompactPrinting()
###

### Exported!
setGeneric("makeNakedCharacterMatrixForDisplay",
    function(x) standardGeneric("makeNakedCharacterMatrixForDisplay")
)

setMethod("makeNakedCharacterMatrixForDisplay", "ANY",
    function(x) as.matrix(x)
)

### Exported!
### For use within makeNakedCharacterMatrixForDisplay() methods.
cbind_mcols_for_display <- function(m, x)
{
    x_len <- length(x)
    stopifnot(identical(nrow(m), x_len))
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    if (x_nmc == 0L)
        return(m)
    ## cbind() must be called with unnamed arguments to avoid problems
    ## in the unlikely situation where some of the argument names are
    ## 'deparse.level'. So we drop the names with unname() and add them
    ## back on the matrix returned by cbind().
    tmp <- do.call(cbind, unname(lapply(x_mcols, showAsCell)))
    colnames(tmp) <- colnames(x_mcols)
    cbind(m, `|` = rep.int("|", x_len), tmp)
}

### Exported!
### 'makeNakedMat.FUN' for backward compatibility with code that predates
### the makeNakedCharacterMatrixForDisplay() generic above.
makePrettyMatrixForCompactPrinting <- function(x, makeNakedMat.FUN=NULL)
{
    if (!is.null(makeNakedMat.FUN))
        makeNakedCharacterMatrixForDisplay <- makeNakedMat.FUN
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    x_NROW <- NROW(x)
    x_ROWNAMES <- ROWNAMES(x)
    wrap_in_square_brackets <- function(idx) {
        if (length(idx) == 0L)
            return(character(0))
        paste0("[", idx, "]")
    }
    if (x_NROW <= nhead + ntail + 1L) {
        ## Compute 'ans' (the matrix).
        ans <- makeNakedCharacterMatrixForDisplay(x)
        ## Compute 'ans_rownames' (the matrix row names).
        if (is.null(x_ROWNAMES)) {
            ans_rownames <- wrap_in_square_brackets(seq_len(x_NROW))
        } else {
            ans_rownames <- x_ROWNAMES
        }
    } else {
        ## Compute 'ans' (the matrix).
        ans_top <- makeNakedCharacterMatrixForDisplay(head(x, n=nhead))
        ans_bottom <- makeNakedCharacterMatrixForDisplay(tail(x, n=ntail))
        ellipses <- rep.int("...", ncol(ans_top))
        ellipses[colnames(ans_top) %in% "|"] <- "."
        ans <- rbind(ans_top, matrix(ellipses, nrow=1L), ans_bottom)
        ## Compute 'ans_rownames' (the matrix row names).
        if (is.null(x_ROWNAMES)) {
            idx1 <- seq(from=1L, by=1L, length.out=nhead)
            idx2 <- seq(to=x_NROW, by=1L, length.out=ntail)
            s1 <- wrap_in_square_brackets(idx1)
            s2 <- wrap_in_square_brackets(idx2)
        } else {
            s1 <- head(x_ROWNAMES, n=nhead)
            s2 <- tail(x_ROWNAMES, n=ntail)
        }
        max_width <- max(nchar(s1, type="width"), nchar(s2, type="width"))
        if (max_width <= 1L) {
            ellipsis <- "."
        } else if (max_width == 2L) {
            ellipsis <- ".."
        } else {
            ellipsis <- "..."
        }
        ans_rownames <- c(s1, ellipsis, s2)
    }
    rownames(ans) <- format(ans_rownames, justify="right")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### makeClassinfoRowForCompactPrinting()
###

### Exported!
makeClassinfoRowForCompactPrinting <- function(x, col2class)
{
    ans_names <- names(col2class)
    no_bracket <- ans_names == ""
    ans_names[no_bracket] <- col2class[no_bracket]
    left_brackets <- right_brackets <- character(length(col2class))
    left_brackets[!no_bracket] <- "<"
    right_brackets[!no_bracket] <- ">"
    ans <- paste0(left_brackets, col2class, right_brackets)
    names(ans) <- ans_names
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    if (x_nmc > 0L) {
        tmp <- sapply(x_mcols,
                      function(xx) paste0("<", classNameForDisplay(xx), ">"))
        ans <- c(ans, `|`="|", tmp)
    }
    matrix(ans, nrow=1L, dimnames=list("", names(ans)))
}

