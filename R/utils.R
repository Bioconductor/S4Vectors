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
### Argument 'margin' added in S4Vectors 0.45.1 and used in package igblastr.
wmsg <- function(..., margin=2)
{
    width <- getOption("width") - margin
    paste0(strwrap(paste0(c(...), collapse=""), width=width),
           collapse=paste0("\n", strrep(" ", margin)))
}

load_package_gracefully <- function(package, ...)
{
    if (!requireNamespace(package, quietly=TRUE))
        stop("Could not load package ", package, ". Is it installed?\n\n  ",
             wmsg("Note that the ", package, " package is ",
                  "required ", ..., ". Please install it with:"),
             "\n\n    BiocManager::install(\"", package, "\")")
}

errorIfWarning <- function(expr)
{
    old_options <- options(warn=2)
    on.exit(options(old_options))
    eval(expr)
}

AEbufs_use_malloc <- function(x=TRUE)
{
    stopifnot(isTRUEorFALSE(x))
    .Call("AEbufs_use_malloc", x, PACKAGE="S4Vectors")
}

AEbufs_free <- function()
{
    .Call("AEbufs_free", PACKAGE="S4Vectors")
}

### Exported!
.Call2 <- function(.NAME, ..., PACKAGE)
{
    ## Uncomment the 2 lines below to switch from R_alloc- to malloc-based
    ## Auto-Extending buffers.
    #AEbufs_use_malloc(TRUE)
    #on.exit({AEbufs_free(); AEbufs_use_malloc(FALSE)})
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

