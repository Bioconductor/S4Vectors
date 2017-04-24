### =========================================================================
### Miscellaneous low-level utils
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
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

.AEbufs_use_malloc <- function(x)
    .Call("AEbufs_use_malloc", x, PACKAGE="S4Vectors")

.AEbufs_free <- function()
    .Call("AEbufs_free", PACKAGE="S4Vectors")

### Exported!
.Call2 <- function(.NAME, ..., PACKAGE)
{
    ## Uncomment the 2 lines below to switch from R_alloc- to malloc-based
    ## Auto-Extending buffers.
    #.AEbufs_use_malloc(TRUE)
    #on.exit({.AEbufs_free(); .AEbufs_use_malloc(FALSE)})    
    .Call(.NAME, ..., PACKAGE=PACKAGE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Functional fun
###

Has <- function(FUN) {
  function(x) {
    !is.null(FUN(x))
  }
}

