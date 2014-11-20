### =========================================================================
### Some low-level helper functions
### -------------------------------------------------------------------------
###


### Wrap the message in lines that don't exceed the terminal width (obtained
### with 'getOption("width")'). Usage:
###   stop(wmsg(...))
###   warning(wmsg(...))
###   message(wmsg(...))
wmsg <- function(...)
    paste0(strwrap(paste0(c(...), collapse="")), collapse="\n  ")

errorIfWarning <- function(expr)
{
    old_options <- options(warn=2)        
    on.exit(options(old_options))
    eval(expr)
}

AEbufs.use.malloc <- function(x)
    .Call("AEbufs_use_malloc", x, PACKAGE="S4Vectors")

AEbufs.free <- function()
    .Call("AEbufs_free", PACKAGE="S4Vectors")

### Exported!
.Call2 <- function(.NAME, ..., PACKAGE)
{
    #Turning off malloc-based Auto-Extending buffers again until I find the
    #time to troubleshoot 'R CMD check' segfault on moscato1 and pitt. 
    #AEbufs.use.malloc(TRUE)
    #on.exit({AEbufs.free(); AEbufs.use.malloc(FALSE)})    
    .Call(.NAME, ..., PACKAGE=PACKAGE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Pretty printing
###

### showHeadLines and showTailLines robust to NA, Inf and non-integer 
.get_showLines <- function(default, option)
{
    opt <- getOption(option, default=default)
    if (!is.infinite(opt))
        opt <- as.integer(opt)
    if (is.na(opt))
        opt <- default
    opt 
}

get_showHeadLines <- function()
{
    .get_showLines(5L, "showHeadLines") 
}

get_showTailLines <- function()
{
    .get_showLines(5L, "showTailLines") 
}

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

.rownames2 <- function(names=NULL, len=NULL, tindex=NULL, bindex=NULL)
{
  if (is.null(tindex) && is.null(bindex)) {
    ## all lines
    if (len == 0L)
      character(0)
    else if (is.null(names))
      paste0("[", seq_len(len), "]")
    else
      names
  } else {
    ## head and tail 
    if (!is.null(names)) {
      c(names[tindex], "...", names[bindex])
    } else {
      s1 <- paste0("[", tindex, "]")
      s2 <- paste0("[", bindex, "]")
      if (all(tindex == 0)) 
        s1 <- character(0) 
      if (all(bindex == 0)) 
        s2 <- character(0) 
      c(s1, "...", s2)
    }
  }
}

### 'makeNakedMat.FUN' must be a function returning a character matrix.
makePrettyMatrixForCompactPrinting <- function(x, makeNakedMat.FUN)
{
  lx <- NROW(x)
  nhead <- get_showHeadLines()
  ntail <- get_showTailLines()

  if (lx < (nhead + ntail + 1L)) {
    ans <- makeNakedMat.FUN(x)
    ans_rownames <- .rownames2(names(x), lx)
  } else {
    top_idx <- 1:nhead
    if (nhead == 0)
      top_idx <- 0 
    bottom_idx=(lx-ntail+1L):lx
    if (ntail == 0)
      bottom_idx <- 0 
    ans_top <- makeNakedMat.FUN(x[top_idx,,drop=FALSE])
    ans_bottom <- makeNakedMat.FUN(x[bottom_idx,,drop=FALSE])
    ans <- rbind(ans_top,
                 matrix(rep.int("...", ncol(ans_top)), nrow=1L),
                 ans_bottom)
    ans_rownames <- .rownames2(names(x), lx, top_idx, bottom_idx)
  }
  rownames(ans) <- format(ans_rownames, justify="right")
  ans
}

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
    x_mcols <- mcols(x)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    if (x_nmc > 0L) {
        tmp <- sapply(x_mcols,
                      function(xx) paste0("<", classNameForDisplay(xx), ">"))
        ans <- c(ans, `|`="|", tmp)
    }
    matrix(ans, nrow=1L, dimnames=list("", names(ans)))
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
### Functional fun
###

Has <- function(FUN) {
  function(x) {
    !is.null(FUN(x))
  }
}

