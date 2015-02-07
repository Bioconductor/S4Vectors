### =========================================================================
### Vector objects
### -------------------------------------------------------------------------
###
### The Vector virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###


setClassUnion("DataTableORNULL", c("DataTable", "NULL"))

setClass("Vector",
    contains="Annotated",
    representation(
        "VIRTUAL",
        elementMetadata="DataTableORNULL"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###
### For internal use only.
###
### Must return the names of all the slots in Vector object 'x' that are
### "parallel" to 'x'. Slot 'foo' is considered to be "parallel" to 'x' if:
###   (a) 'x@foo' is NULL or an object for which NROW() is equal to
###       'length(x)', and
###   (b) the i-th element in 'x@foo' describes some component of the i-th
###       element in 'x'.
### For example, the "start", "width", "NAMES", and "elementMetadata" slots
### of an IRanges object are parallel to the object. Note that the "NAMES"
### and "elementMetadata" slots can be set to NULL.
### The *first" slot name returned by parallelSlotNames() is used to get the
### length of 'x'.
###

setGeneric("parallelSlotNames",
    function(x) standardGeneric("parallelSlotNames")
)

setMethod("parallelSlotNames", "Vector", function(x) "elementMetadata")

### Methods for Vector subclasses only need to specify the parallel slots they
### add to their parent class. See Hits-class.R file for an example.

### fixedColumnNames() is for internal use only.
### TODO: Deprecate fixedColumnNames(). Use parallelSlotNames() instead.
setGeneric("fixedColumnNames", function(x) standardGeneric("fixedColumnNames"))
setMethod("fixedColumnNames", "ANY", function(x) character())


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters.
###

setMethod("length", "Vector",
    function(x) NROW(slot(x, parallelSlotNames(x)[[1L]]))
)

setMethod("NROW", "Vector", function(x) length(x))
setMethod("ROWNAMES", "Vector", function(x) names(x))

### 3 accessors for the same slot: elementMetadata(), mcols(), and values().
### mcols() is the recommended one, use of elementMetadata() or values() is
### discouraged.
setGeneric("elementMetadata",
    function(x, use.names=FALSE, ...) standardGeneric("elementMetadata")
)

setMethod("elementMetadata", "Vector",
    function(x, use.names=FALSE, ...)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        ans <- x@elementMetadata
        if (use.names && !is.null(ans))
            rownames(ans) <- names(x)
        ans
    }
)

setGeneric("mcols",
    function(x, use.names=FALSE, ...) standardGeneric("mcols")
)

setMethod("mcols", "Vector",
    function(x, use.names=FALSE, ...)
        elementMetadata(x, use.names=use.names, ...)
)

setGeneric("values", function(x, ...) standardGeneric("values"))

setMethod("values", "Vector", function(x, ...) elementMetadata(x, ...))

setMethod("anyNA", "Vector", function(x) any(is.na(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Vector.length <- function(x)
{
    x_len <- length(x)
    if (!isSingleInteger(x_len) || x_len < 0L)
        return("'length(x)' must be a single non-negative integer")
    if (!is.null(attributes(x_len)))
        return("'length(x)' must be a single integer with no attributes")
    NULL
}

.valid.Vector.parallelSlots <- function(x)
{
    x_len <- length(x)
    x_pslotnames <- parallelSlotNames(x)
    if (!is.character(x_pslotnames)
     || anyMissing(x_pslotnames)
     || anyDuplicated(x_pslotnames)) {
        msg <- c("'parallelSlotNames(x)' must be a character vector ",
                 "with no NAs and no duplicates")
        return(paste(msg, collapse=""))
    }
    if (x_pslotnames[[length(x_pslotnames)]] != "elementMetadata") {
        msg <- c("last string in 'parallelSlotNames(x)' ",
                 "must be \"elementMetadata\"")
        return(paste(msg, collapse=""))
    }
    for (slotname in x_pslotnames) {
        tmp <- slot(x, slotname)
        if (!(is.null(tmp) || NROW(tmp) == x_len)) {
            msg <- c("'x@", slotname, "' is not parallel to 'x'")
            return(paste(msg, collapse=""))
        }
    }
    NULL
}

.valid.Vector.names <- function(x)
{
    x_names <- names(x)
    if (is.null(x_names))
        return(NULL)
    if (!is.character(x_names) || !is.null(attributes(x_names))) {
        msg <- c("'names(x)' must be NULL or a character vector ",
                 "with no attributes")
        return(paste(msg, collapse=""))
    }
    if (length(x_names) != length(x))
        return("'names(x)' must be NULL or have the length of 'x'")
    NULL
}

.valid.Vector.mcols <- function(x)
{
    x_mcols <- mcols(x)
    if (!is(x_mcols, "DataTableORNULL"))
        return("'mcols(x)' must be a DataTable object or NULL")
    if (is.null(x_mcols))
        return(NULL)
    ## 'x_mcols' is a DataTable object.
    x_mcols_rownames <- rownames(x_mcols)
    if (is.null(x_mcols_rownames))
        return(NULL)
    if (!identical(x_mcols_rownames, names(x)))
    {
        msg <- c("the rownames of DataTable 'mcols(x)' ",
                 "must match the names of 'x'")
        return(paste(msg, collapse=""))
    }
    NULL
}

.valid.Vector <- function(x)
{
    c(.valid.Vector.length(x),
      .valid.Vector.parallelSlots(x),
      .valid.Vector.names(x),
      .valid.Vector.mcols(x))
}

setValidity2("Vector", .valid.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.logical", "Vector",
    function(x) as.vector(x, mode="logical")
)
setMethod("as.integer", "Vector",
    function(x) as.vector(x, mode="integer")
)
setMethod("as.numeric", "Vector",
    function(x) as.vector(x, mode="numeric")
)
### Even though as.double() is a generic function (as reported by
### 'getGeneric("as.double")', it seems impossible to define methods for this
### generic. Trying to do so like in the code below actually creates an
### "as.numeric" method.
#setMethod("as.double", "Vector",
#    function(x) as.vector(x, mode="double")
#)
setMethod("as.complex", "Vector",
    function(x) as.vector(x, mode="complex")
)
setMethod("as.character", "Vector",
    function(x) as.vector(x, mode="character")
)
setMethod("as.raw", "Vector",
    function(x) as.vector(x, mode="raw")
)

setAs("Vector", "vector", function(from) as.vector(from))
setAs("Vector", "logical", function(from) as.logical(from))
setAs("Vector", "integer", function(from) as.integer(from))
setAs("Vector", "numeric", function(from) as.numeric(from))
setAs("Vector", "complex", function(from) as.complex(from))
setAs("Vector", "character", function(from) as.character(from))
setAs("Vector", "raw", function(from) as.raw(from))

setAs("Vector", "data.frame", function(from) as.data.frame(from))

### S3/S4 combo for as.data.frame.Vector
as.data.frame.Vector <- function(x, row.names=NULL, optional=FALSE, ...)
{
    x <- as.vector(x)
    as.data.frame(x, row.names=NULL, optional=optional, ...)
}
setMethod("as.data.frame", "Vector", as.data.frame.Vector)

makeFixedColumnEnv <- function(x, parent, tform = identity) {
  env <- new.env(parent=parent)
  lapply(fixedColumnNames(x), function(nm) {
    accessor <- get(nm, parent, mode="function")
    makeActiveBinding(nm, function() {
      val <- tform(accessor(x))
      rm(list=nm, envir=env)
      assign(nm, val, env)
      val
    }, env)
  })
  env
}

setMethod("as.env", "Vector", function(x, enclos, tform = identity) {
  addSelfRef(x, makeFixedColumnEnv(x, as.env(mcols(x), enclos, tform), tform))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters.
###

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))

setReplaceMethod("elementMetadata", "Vector",
    function(x, ..., value)
    {
        if (!is(value, "DataTableORNULL"))
            stop("replacement 'elementMetadata' value must be ",
                 "a DataTable object or NULL")
        if ("elementMetadata" %in% names(attributes(x))) {
            if (!is.null(value) && length(x) != nrow(value))
                stop("supplied metadata colums must have the length of 'x'")
            if (!is.null(value))
                rownames(value) <- NULL
            x@elementMetadata <- value
        }
        x
    }
)

setGeneric("mcols<-", function(x, ..., value) standardGeneric("mcols<-"))

setReplaceMethod("mcols", "Vector",
    function(x, ..., value) `elementMetadata<-`(x, ..., value=value)
)

setGeneric("values<-", function(x, ..., value) standardGeneric("values<-"))

setReplaceMethod("values", "Vector",
                 function(x, value) {
                     elementMetadata(x) <- value
                     x
                 })

setGeneric("rename", function(x, value, ...) standardGeneric("rename"))

.renameVector <- function(x, value, ...) {
  if (missing(value))
    newNames <- c(...)
  else newNames <- c(value, ...)
  badOldNames <- setdiff(names(newNames), names(x))
  if (length(badOldNames))
    stop("Some 'from' names in value not found on 'x': ",
         paste(badOldNames, collapse = ", "))
  names(x)[match(names(newNames), names(x))] <- newNames
  x
}

setMethod("rename", "vector", .renameVector)
setMethod("rename", "Vector", .renameVector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###
### The "[" and "[<-" methods for Vector objects are just delegating to
### extractROWS() and replaceROWS() for performing the real work. Most of
### the times, the author of a Vector subclass only needs to implement an
### "extractROWS" and "replaceROWS" method for his/her objects.
###

setMethod("[", "Vector",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        extractROWS(x, i)
    }
)

### Used in IRanges!
extractROWSWithBracket <- function(x, i) {
  if (missing(i))
    return(x)
  ## dynamically call [i,,,..,drop=FALSE] with as many "," as length(dim)-1
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x)
  args <- rep(alist(foo=), ndim)
  names(args) <- NULL
  args[[1]] <- i
  args <- c(list(x), args, list(drop = FALSE))
  do.call(`[`, args)
}

setMethod("extractROWS", "ANY", extractROWSWithBracket)

setMethod("extractROWS", "matrix", extractROWSWithBracket)

### We provide a default "extractROWS" method for Vector objects that only
### subsets the individual parallel slots. That should be enough for most
### Vector derivatives that have parallelSlotNames() properly set.
setMethod("extractROWS", "Vector",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        x_pslotnames <- parallelSlotNames(x)
        ans_pslots <- lapply(setNames(x_pslotnames, x_pslotnames),
                             function(slotname)
                                 extractROWS(slot(x, slotname), i))
        ## Does NOT validate the object before returning it, because, most of
        ## the times, this is not needed. There are exceptions though. See
        ## for example the "extractROWS" method for Hits objects.
        do.call(BiocGenerics:::updateS4, c(list(x),
                                           ans_pslots,
                                           list(check=FALSE)))
    }
)

setReplaceMethod("[", "Vector",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        li <- length(i)
        if (li == 0L) {
            ## Surprisingly, in that case, `[<-` on standard vectors does not
            ## even look at 'value'. So neither do we...
            return(x)
        }
        lv <- NROW(value)
        if (lv == 0L)
            stop("replacement has length zero")
        value <- normalizeSingleBracketReplacementValue(value, x)
        if (li != lv) {
            if (li %% lv != 0L)
                warning("number of values supplied is not a sub-multiple ",
                        "of the number of values to be replaced")
            value <- extractROWS(value, rep(seq_len(lv), length.out=li))
        }
        replaceROWS(x, i, value)
    }
)

### Works on any Vector object for which c() and [ work. Assumes 'value' is
### compatible with 'x'.
setMethod("replaceROWS", "Vector",
    function(x, i, value)
    {
        idx <- seq_along(x)
        i <- extractROWS(setNames(idx, names(x)), i)
        ## Assuming that objects of class 'class(x)' can be combined with c().
        ans <- c(x, value)
        idx[i] <- length(x) + seq_len(length(value))
        ## Assuming that [ works on objects of class 'class(x)'.
        ans <- ans[idx]
        ## Restore the original decoration.
        metadata(ans) <- metadata(x)
        names(ans) <- names(x)
        mcols(ans) <- mcols(x)
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convenience wrappers for common subsetting operations.
###

### S3/S4 combo for window.Vector
.window.Vector <- function(x, start=NA, end=NA, width=NA)
{
    i <- WindowNSBS(x, start=start, end=end, width=width)
    extractROWS(x, i)
}
window.Vector <- function(x, ...) .window.Vector(x, ...)
setMethod("window", "Vector", window.Vector)

### S3/S4 combo for head.Vector
head.Vector <- function(x, n=6L, ...)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(x_NROW, n)
    } else {
        n <- max(0L, x_NROW + n)
    }
    window(x, start=1L, width=n)
}
setMethod("head", "Vector", head.Vector)

## S3/S4 combo for tail.Vector
tail.Vector <- function(x, n=6L, ...)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(x_NROW, n)
    } else {
        n <- max(0L, x_NROW + n)
    }
    window(x, end=x_NROW, width=n)
}
setMethod("tail", "Vector", tail.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Internal helpers used by the "show" method of various Vector subclasses.
###

setGeneric("classNameForDisplay",
    function(x) standardGeneric("classNameForDisplay"))

setMethod("classNameForDisplay", "ANY",
   function(x)
   {
       ## Selecting the 1st element guarantees that we return a single string
       ## (e.g. on an ordered factor, class(x) returns a character vector of
       ## length 2).
       class(x)[1L]
   }
)

setMethod("classNameForDisplay", "AsIs", function(x) {
  class(x) <- setdiff(class(x), "AsIs")
  classNameForDisplay(x)
})


setGeneric("showAsCell",
    function(object) standardGeneric("showAsCell")
)

setMethod("showAsCell", "ANY", function(object) {
  if (length(dim(object)) > 2)
    dim(object) <- c(nrow(object), prod(tail(dim(object), -1)))
  if (NCOL(object) > 1) {
    class(object) <- setdiff(class(object), "AsIs")
    df <- as.data.frame(object[, head(seq_len(ncol(object)), 3), drop = FALSE])
    attempt <- do.call(paste, df)
    if (ncol(object) > 3)
      attempt <- paste(attempt, "...")
    attempt
  } else if (NCOL(object) == 0L) {
    rep.int("", NROW(object))
  } else {
    attempt <- try(as.vector(object), silent=TRUE)
    if (is(attempt, "try-error"))
      rep.int("########", length(object))
    else attempt
  }
})

setMethod("showAsCell", "Vector", function(object)
          rep.int("########", length(object)))

setMethod("showAsCell", "Date", function(object) object)
setMethod("showAsCell", "POSIXt", function(object) object)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

rbindRowOfNAsToMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (!is.null(x_mcols))
    mcols(x)[nrow(x_mcols)+1L,] <- NA
  x
}

### FIXME: This method doesn't work properly on DataTable objects if 'after'
### is >= 1 and < length(x).
setMethod("append", c("Vector", "Vector"),
    function(x, values, after=length(x))
    {
        if (!isSingleNumber(after))
            stop("'after' must be a single number")
        x_len <- length(x)
        if (after == 0L)
            c(values, x)
        else if (after >= x_len)
            c(x, values)
        else
            c(head(x, n=after), values, tail(x, n=-after))
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###

setMethod("eval", c("expression", "Vector"),
          function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
          )

setMethod("eval", c("language", "Vector"),
          function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
          )

setMethod("with", "Vector",
          function(data, expr, ...)
          {
            safeEval(substitute(expr), data, parent.frame(), ...)
          })
