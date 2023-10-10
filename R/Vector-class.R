### =========================================================================
### Vector objects
### -------------------------------------------------------------------------
###
### The Vector virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###

setClass("Vector",
    contains="Annotated",
    representation(
        "VIRTUAL",
        elementMetadata="DataFrame_OR_NULL"
    )
)

### Beware that:
###   > is(factor(), "vector_OR_Vector")
###   [1] TRUE
### even though:
###   > is(factor(), "vector")
###   [1] FALSE
###   > is(factor(), "Vector")
###   [1] FALSE
### See R/S4-utils.R for other examples of messed up inheritance with union
### classes.
### TODO: Should we explicitely add "factor" to this union?
setClassUnion("vector_OR_Vector", c("vector", "Vector"))  # vector-like objects


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallel_slot_names()
###
### For internal use only.
###

### parallel_slot_names() must return the names of all the slots in Vector
### derivative 'x' that are **parallel** to the object. Slot "foo" in 'x'
### is considered to be parallel to 'x' if it's guaranteed to contain a
### value that is either NULL or such that 'NROW(x@foo)' is equal to
### 'length(x)' and the i-th ROW in 'x@foo' is associated with the i-th
### vector element in 'x'.
### For example, the "start", "width", "NAMES", and "elementMetadata" slots
### of an IRanges object 'x' are parallel to 'x'. Note that the "NAMES"
### and "elementMetadata" slots can be set to NULL.
setGeneric("parallel_slot_names",
    function(x) standardGeneric("parallel_slot_names")
)

### Methods for Vector derivatives should be defined in an incremental
### fashion, that is, they should only explicitly list the new "parallel
### slots" (i.e. the parallel slots that they add to their parent class).
### See above for what slots should or should not be considered "parallel".
### See Hits-class.R file for an example of a parallel_slot_names() method
### defined for a Vector derivative.
setMethod("parallel_slot_names", "Vector", function(x) "elementMetadata")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelVectorNames()
###
### For internal use only.
###

setGeneric("parallelVectorNames",
           function(x) standardGeneric("parallelVectorNames"))
setMethod("parallelVectorNames", "ANY",
          function(x) setdiff(colnames(as.data.frame(new(class(x)))), "value"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### updateObject()
###
### The default method (defined in BiocGenerics) does complicated, costly,
### and dangerous things, and sometimes it actually breaks valid objects
### (e.g. it breaks valid OverlapEncodings objects). So we overwrite it with
### a method for Vector objects that does nothing! That way it's simple,
### cheap, and safe ;-). And that's really all it needs to do at the moment.
### UPDATE: Starting with S4Vectors 0.23.19, all DataFrame instances need
### to be replaced with DFrame instances. So the updateObject() method for
### Vector objects got updated from doing nothing (no-op) to call
### updateObject() on the elementMetadata component of the object.

setMethod("updateObject", "Vector",
    function(object, ..., verbose=FALSE)
    {
        ## Update from DataFrame to DFrame.
        object@elementMetadata <- updateObject(object@elementMetadata,
                                               ..., verbose=verbose)
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

### We use the **first** slot returned by parallel_slot_names() to infer
### the length of 'x' so hopefully this is not a slot that can contain a
### NULL (like the "elementMetadata" of a Vector derivative or the "NAMES"
### slot of an IRanges object).
setMethod("length", "Vector",
    function(x) NROW(slot(x, parallel_slot_names(x)[[1L]]))
)

setMethod("lengths", "Vector",
     function(x, use.names=TRUE)
     {
         if (!isTRUEorFALSE(use.names))
             stop("'use.names' must be TRUE or FALSE")
         ans <- elementNROWS(x)  # This is wrong! See ?Vector for the details.
         if (!use.names)
             names(ans) <- NULL
         ans
     }
)

### 3 accessors for the same slot: elementMetadata(), mcols(), and values().
### mcols() is the recommended one, use of elementMetadata() or values() is
### discouraged.
setGeneric("elementMetadata", signature="x",
    function(x, use.names=TRUE, ...) standardGeneric("elementMetadata")
)

setMethod("elementMetadata", "Vector",
    function(x, use.names=TRUE, ...)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        ans <- updateObject(x@elementMetadata, check=FALSE)
        if (use.names && !is.null(ans))
            rownames(ans) <- names(x)
        ans
    }
)

setGeneric("mcols", signature="x",
    function(x, use.names=TRUE, ...) standardGeneric("mcols")
)

setMethod("mcols", "Vector",
    function(x, use.names=TRUE, ...)
        elementMetadata(x, use.names=use.names, ...)
)

setGeneric("values", function(x, ...) standardGeneric("values"))

setMethod("values", "Vector", function(x, ...) elementMetadata(x, ...))

setMethod("anyNA", "Vector", function(x, recursive=FALSE) FALSE)

setMethod("is.na", "Vector", function(x) rep.int(FALSE, length(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_Vector_length <- function(x)
{
    x_len <- length(x)
    if (!isSingleNumber(x_len) || x_len < 0L)
        return("'length(x)' must be a single non-negative number")
    if (!is.null(attributes(x_len)))
        return("'length(x)' must be a single integer with no attributes")
    NULL
}

.validate_Vector_parallel_slots <- function(x)
{
    x_len <- length(x)
    x_pslotnames <- parallel_slot_names(x)
    if (!is.character(x_pslotnames)
     || anyMissing(x_pslotnames)
     || anyDuplicated(x_pslotnames)) {
        msg <- c("'parallel_slot_names(x)' must be a character vector ",
                 "with no NAs and no duplicates")
        return(paste(msg, collapse=""))
    }
    if (x_pslotnames[[length(x_pslotnames)]] != "elementMetadata") {
        msg <- c("last string in 'parallel_slot_names(x)' ",
                 "must be \"elementMetadata\"")
        return(paste(msg, collapse=""))
    }
    msg <- NULL
    for (slotname in head(x_pslotnames, -1L)) {
        tmp <- slot(x, slotname)
        if (!(is.null(tmp) || NROW(tmp) == x_len)) {
            what <- paste0("x@", slotname)
            msg <- c(msg, paste0("'", what, "' is not parallel to 'x'"))
        }
    }
    tmp <- mcols(x, use.names=FALSE)
    if (!(is.null(tmp) || nrow(tmp) == x_len)) {
        msg <- c(msg, "'mcols(x)' is not parallel to 'x'")
    }
    msg
}

.validate_Vector_names <- function(x)
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

.validate_Vector_mcols <- function(x)
{
    x_mcols <- mcols(x, use.names=FALSE)
    if (is.null(x_mcols))
        return(NULL)
    if (!is(x_mcols, "DataFrame"))
        return("'mcols(x)' must be a DataFrame object or NULL")
    ## 'x_mcols' is a DataFrame derivative.
    x_mcols_rownames <- rownames(x_mcols)
    if (is.null(x_mcols_rownames))
        return(NULL)
    if (!identical(x_mcols_rownames, names(x)))
    {
        msg <- c("the rownames of DataFrame 'mcols(x)' ",
                 "must match the names of 'x'")
        return(paste(msg, collapse=""))
    }
    NULL
}

.validate_Vector <- function(x)
{
    c(.validate_Vector_length(x),
      .validate_Vector_parallel_slots(x),
      .validate_Vector_names(x),
      .validate_Vector_mcols(x))
}

setValidity2("Vector", .validate_Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))

### NOT exported but used in the IRanges and GenomicRanges packages.
normarg_mcols <- function(mcols, x_class, x_len)
{
    ## Note that 'mcols_target_class' could also be obtained with
    ## 'getClassDef(x_class)@slots[["elementMetadata"]]', in which
    ## case the class name would be returned with the "package" attribute.
    mcols_target_class <- getSlots(x_class)[["elementMetadata"]]
    ok <- is(mcols, mcols_target_class)
    if (is.null(mcols)) {
        if (ok)
            return(mcols)  # NULL
        mcols <- make_zero_col_DFrame(x_len)
    } else if (is.list(mcols)) {
        ## Note that this will also handle an 'mcols' that is a data.frame
        ## or a data.frame derivative (e.g. data.table object).
        if (ok)
            return(mcols)
        mcols <- new_DataFrame(mcols)
    } else {
        mcols <- updateObject(mcols, check=FALSE)
    }
    ok <- is(mcols, mcols_target_class)
    if (!ok)
        mcols <- as(mcols, mcols_target_class)

    ## From now on, 'mcols' is guaranteed to be a DataFrame derivative.
    if (!is.null(rownames(mcols)))
        rownames(mcols) <- NULL

    mcols_nrow <- nrow(mcols)
    if (mcols_nrow == x_len)
        return(mcols)
    one <- ncol(mcols) == 1L
    if (mcols_nrow > x_len && mcols_nrow != 1L)
        stop(wmsg("trying to set ", if (one) "a " else "",
                  "metadata column", if (one) "" else "s", " ",
                  "of length ", mcols_nrow, " on an object of length ", x_len))
    if (mcols_nrow == 0L)
        stop(wmsg("trying to set ", if (one) "a " else "", "zero length ",
                  "metadata column", if (one) "" else "s", " ",
                  "on a non-zero length object "))
    if (x_len %% mcols_nrow != 0L)
        warning(wmsg("You supplied ", if (one) "a " else "",
                     "metadata column", if (one) "" else "s", " ",
                     "of length ", mcols_nrow, " to set on an object ",
                     "of length ", x_len, ". However please note that ",
                     "the latter is not a multiple of the former."))
    i <- rep(seq_len(mcols_nrow), length.out=x_len)
    extractROWS(mcols, i)
}

setReplaceMethod("elementMetadata", "Vector",
    function(x, ..., value)
    {
        value <- normarg_mcols(value, class(x), length(x))
        BiocGenerics:::replaceSlots(x, elementMetadata=value, check=FALSE)
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

setGeneric("rename", function(x, ...) standardGeneric("rename"))

.renameVector <- function(x, ...) {
  newNames <- c(...)
  if (!is.character(newNames) || any(is.na(newNames))) {
      stop("arguments in '...' must be character and not NA")
  }
  badOldNames <- setdiff(names(newNames), names(x))
  if (length(badOldNames))
    stop("Some 'from' names in value not found on 'x': ",
         paste(badOldNames, collapse = ", "))
  names(x)[match(names(newNames), names(x))] <- newNames
  x
}

setMethod("rename", "vector", .renameVector)
setMethod("rename", "Vector", .renameVector)

setGeneric("unname", signature="obj")

setMethod("unname", "Vector", function(obj, force = FALSE) {
    names(obj) <- NULL
    obj
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
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

setAs("Vector", "factor", function(from) as.factor(from))

setAs("Vector", "data.frame", function(from) as.data.frame(from, optional=TRUE))

### S3/S4 combo for as.data.frame.Vector
as.data.frame.Vector <- function(x, row.names=NULL, optional=FALSE, ...) {
    as.data.frame(x, row.names=NULL, optional=optional, ...)
}
setMethod("as.data.frame", "Vector",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
              x <- as.vector(x)
              as.data.frame(x, row.names=row.names, optional=optional, ...)
          })

as.matrix.Vector <- function(x, ...) {
    as.matrix(x)
}

setMethod("as.matrix", "Vector", function(x) {
              as.matrix(as.vector(x))
          })

classNamespace <- function(x) {
    pkg <- packageSlot(class(x))
    pvnEnv <- .GlobalEnv
    if (!is.null(pkg)) {
        pvnEnv <- getNamespace(pkg)
    }
    pvnEnv
}

makeFixedColumnEnv <- function(x, parent, tform = identity) {
  env <- new.env(parent=parent)
  pvnEnv <- classNamespace(x)
  lapply(c("names", parallelVectorNames(x)), function(nm) {
    accessor <- get(nm, pvnEnv, mode="function")
    makeActiveBinding(nm, function() {
      val <- tform(accessor(x))
      rm(list=nm, envir=env)
      assign(nm, val, env)
      val
    }, env)
  })
  env
}

setGeneric("as.env", function(x, ...) standardGeneric("as.env"))

setMethod("as.env", "NULL", function(x, enclos, tform = identity) {
  new.env(parent=enclos)
})

addSelfRef <- function(x, env) {
  env$.. <- x
  env
}

setMethod("as.env", "Vector", function(x, enclos, tform = identity) {
  parent <- as.env(mcols(x, use.names=FALSE), enclos, tform)
  addSelfRef(x, makeFixedColumnEnv(x, parent, tform))
})

### S3/S4 combo for as.list.Vector
as.list.Vector <- function(x, ...) as.list(as(x, "List"), ...)
setMethod("as.list", "Vector", as.list.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###
### The "[" and "[<-" methods for Vector objects are just delegating to
### extractROWS() and replaceROWS() for performing the real work. Most of
### the times, the author of a Vector subclass only needs to implement
### an "extractROWS" and "replaceROWS" method for his/her objects.
###

### The "[" method for Vector objects supports the 'x[i, j]' form to
### allow the user to conveniently subset the metadata columns thru 'j'.
### Note that a Vector subclass with a true 2-D semantic (e.g.
### SummarizedExperiment) needs to overwrite this. This means that code
### intended to operate on an arbitrary Vector derivative 'x' should not
### use this feature as there is no guarantee that 'x' supports it. For
### this reason this feature should preferrably be used interactively only.
setMethod("[", "Vector",
    function(x, i, j, ..., drop=TRUE)
    {
        ans <- subset_along_ROWS(x, i, , ..., drop=drop)
        if (!missing(j))
            mcols(ans) <- mcols(ans, use.names=FALSE)[ , j, drop=FALSE]
        ans
    }
)

### We provide a default extractROWS() method for Vector objects. It calls
### the extractROWS() generic internally to subset all the "parallel slots".
### It behaves like an endomorphism with respect to 'x'.
### NOTE TO THE DEVELOPERS OF Vector SUBCLASSES: The default extractROWS()
### method below will work out-of-the-box and do the right thing on your
### objects as long as calling parallel_slot_names() on them reports all
### the "parallel slots". So please make sure to register all the parallel
### slots via a parallel_slot_names() method.
### If that simple approach does not work for your objects (typically
### because some slots require special treatment) then you should override
### the extractROWS() method for Vector objects (you should never need to
### override the "[" method for Vector objects). In addition to taking care
### of the slots that require special treatment, your specialized extractROWS()
### method will typically delegate to the default extractROWS() method below
### via the use of callNextMethod(). See extractROWS() method for Hits objects
### for an example.
setMethod("extractROWS", "Vector",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        x_pslotnames <- parallel_slot_names(x)
        ans_pslots <- lapply(setNames(x_pslotnames, x_pslotnames),
                             function(slotname)
                                 extractROWS(slot(x, slotname), i))
        ## Does NOT validate the object before returning it, because, most of
        ## the times, this is not needed. There are exceptions though. See
        ## for example the "extractROWS" method for Hits objects.
        do.call(BiocGenerics:::replaceSlots,
                c(list(x), ans_pslots, list(check=FALSE)))
    }
)

setReplaceMethod("[", "Vector",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        nsbs <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE,
                                                allow.append=TRUE)
        li <- length(nsbs)
        if (li == 0L) {
            ## Surprisingly, in that case, `[<-` on standard vectors does not
            ## even look at 'value'. So neither do we...
            return(x)
        }
        value <- normalizeSingleBracketReplacementValue(value, x)
        if (is.null(value)) {
            return(extractROWS(x, complementNSBS(nsbs)))
        }
        value <- recycleSingleBracketReplacementValue(value, x, nsbs)
        mergeROWS(x, i, value)
    }
)

setMethod("mergeROWS", c("Vector", "ANY"),
    function(x, i, value)
    {
        nsbs <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE,
                                                allow.append=TRUE)
        if (max(nsbs) <= NROW(x)) {
            nsbs@upper_bound_is_strict <- TRUE
            return(replaceROWS(x, nsbs, value))
        }
        idx <- as.integer(nsbs)
        oob <- idx > NROW(x)
        value_idx <- integer(max(nsbs) - NROW(x))
        ## handles replacement in the appended region
        value_idx[idx[oob] - NROW(x)] <- seq_along(value)[oob]
        if (any(value_idx == 0L)) {
            stop("appending gaps is not supported")
        }
        new_values <- extractROWS(value, value_idx)
        names(new_values) <- if (is.character(i)) i[oob] else NULL
        x <- bindROWS(x, list(new_values), check=FALSE)
        replaceROWS(x, idx[!oob], extractROWS(value, !oob))
    }
)

### Work on any Vector object on which bindROWS() and extractROWS() work.
### Assume that 'value' is compatible with 'x'.
setMethod("replaceROWS", c("Vector", "ANY"),
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        stopifnot(length(i) == NROW(value))

        ## --<1>-- Concatenate 'x' and 'value' with bindROWS() -----

        ## We assume that bindROWS() works on objects of class 'class(x)'
        ## and does the right thing i.e. that it returns an object of the
        ## same class as 'x' and of NROW 'NROW(x) + NROW(value)'. We skip
        ## validation.
        ans <- bindROWS(x, list(value), check=FALSE)

        ## --<2>-- Subset 'ans' with extractROWS() -----

        idx <- replaceROWS(seq_along(x), i, seq_along(value) + NROW(x))
        ## Because of how we constructed it, 'idx' is guaranteed to be a valid
        ## subscript to use in 'extractROWS(ans, idx)'. By wrapping it inside a
        ## NativeNSBS object, extractROWS() won't waste time checking it or
        ## trying to normalize it.
        idx <- NativeNSBS(idx, NROW(ans))
        ## We assume that extractROWS() works on an object of class 'class(x)'.
        ## For some objects (e.g. Hits), extractROWS() will take care of
        ## validating the returned object.
        ans <- extractROWS(ans, idx)

        ## --<3>-- Restore the original names -----

        names(ans) <- names(x)
        ## Note that we want the elements coming from 'value' to bring their
        ## metadata columns into 'x' so we do NOT restore the original metadata
        ## columns. See this thread on bioc-devel:
        ##  https://stat.ethz.ch/pipermail/bioc-devel/2015-November/008319.html
        #mcols(ans) <- mcols(x, use.names=FALSE)

        ans
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convenience wrappers for common subsetting operations
###

### S3/S4 combo for subset.Vector
subset.Vector <- function(x, ...) subset(x, ...)
subset_Vector <- function(x, subset, select, drop=FALSE, ...)
{
    i <- evalqForSubset(subset, x, ...)
    x_mcols <- mcols(x, use.names=FALSE)
    if (!is.null(x_mcols)) {
        j <- evalqForSelect(select, x_mcols, ...)
        mcols(x) <- x_mcols[ , j, drop=FALSE]
    }
    x[i, drop=drop]
}
setMethod("subset", "Vector", subset_Vector)

### S3/S4 combo for window.Vector
window.Vector <- function(x, ...) window(x, ...)
Vector_window <- function(x, start=NA, end=NA, width=NA)
{
    i <- RangeNSBS(x, start=start, end=end, width=width)
    extractROWS(x, i)
}
setMethod("window", "Vector", Vector_window)

### S3/S4 combo for head.Vector
head.Vector <- function(x, ...) head(x, ...)
setMethod("head", "Vector", head_along_ROWS)

## S3/S4 combo for tail.Vector
tail.Vector <- function(x, ...) tail(x, ...)
setMethod("tail", "Vector", tail_along_ROWS)

setMethod("rep.int", "Vector", rep.int_along_ROWS)

## NOT exported.
revROWS <- function(x) extractROWS(x, rev(seq_len(NROW(x))))

### S3/S4 combo for rev.Vector
rev.Vector <- revROWS
setMethod("rev", "Vector", revROWS)

## NOT exported.
repROWS <- function(x, ...) extractROWS(x, rep(seq_len(NROW(x)), ...))

setMethod("rep", "Vector", repROWS)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Displaying
###

.Vector_summary <- function(object)
{
    object_len <- length(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object of length ", object_len,
           " with ", object_nmc, " metadata ",
           ifelse(object_nmc == 1L, "column", "columns"))
}
### S3/S4 combo for summary.Vector
summary.Vector <- function(object, ...)
    .Vector_summary(object, ...)
setMethod("summary", "Vector", summary.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation along the ROWS
###
### Note that supporting "extractROWS" and "c" makes "replaceROWS" (and thus
### "[<-") work out-of-the-box!
###

ensureMcols <- function(x) {
  ans <- mcols(x, use.names=FALSE)
  if (is.null(ans))
    ans <- make_zero_col_DFrame(length(x))
  ans
}

combine_mcols <- function(objects)
{
    if (length(objects) == 1L)
        return(mcols(objects[[1L]], use.names=FALSE))
    all_mcols <- lapply(objects, mcols, use.names=FALSE)
    is_null <- sapply_isNULL(all_mcols)
    if (all(is_null))
        return(NULL)
    all_mcols[is_null] <- lapply(
        objects[is_null],
        function(object) make_zero_col_DFrame(length(object))
    )
    do.call(combineRows, all_mcols)
}

### We provide a default bindROWS() method for Vector objects. It calls the
### bindROWS() generic internally to concatenate all the "parallel slots"
### from all the input objects. It behaves like an endomorphism with respect
### to its first input object 'x'.
### NOTE TO THE DEVELOPERS OF Vector SUBCLASSES: The default bindROWS()
### method below will work out-of-the-box and do the right thing on your
### objects as long as calling parallel_slot_names() on them reports all
### the "parallel slots". So please make sure to register all the parallel
### slots via a parallel_slot_names() method.
### If that simple approach does not work for your objects (typically
### because some slots require special treatment) then you should override
### the bindROWS() method for Vector objects (you should never need to
### override the c() method for Vector objects). In addition to taking care
### of the slots that require special treatment, your specialized bindROWS()
### method will typically delegate to the default bindROWS() method below
### via the use of callNextMethod(). See bindROWS() methods for Hits and Rle
### objects for some examples.
bindROWS_Vector_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")

    objects <- prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    ## Concatenate all the parallel slots except "NAMES" and "elementMetadata".
    x_pslotnames <- parallel_slot_names(x)
    pslotnames <- setdiff(x_pslotnames, c("NAMES", "elementMetadata"))
    ans_pslots <- lapply(setNames(pslotnames, pslotnames),
        function(slotname) {
            x_slot <- slot(x, slotname)
            if (is.null(x_slot))
                return(NULL)
            slot_list <- lapply(objects, slot, slotname)
            bindROWS2(x_slot, slot_list)
        }
    )

    if ("NAMES" %in% x_pslotnames) {
        ans_NAMES <- NULL
        if (use.names) {
            names_list <- lapply(all_objects, slot, "NAMES")
            object_has_no_names <- sapply_isNULL(names_list)
            if (!all(object_has_no_names)) {
                ## Concatenate the "NAMES" slots.
                names_list[object_has_no_names] <-
                    lapply(all_objects[object_has_no_names],
                           function(object) character(length(object)))
                ans_NAMES <- unlist(names_list, use.names=FALSE)
            }
        }
        ans_pslots <- c(ans_pslots, list(NAMES=ans_NAMES))
    }

    if (!ignore.mcols) {
        ## Concatenate the "elementMetadata" slots.
        ans_mcols <- combine_mcols(all_objects)
        ans_pslots <- c(ans_pslots, list(elementMetadata=ans_mcols))
    }

    ans <- do.call(BiocGenerics:::replaceSlots,
                   c(list(x), ans_pslots, list(check=FALSE)))

    if (ignore.mcols)
        mcols(ans) <- NULL

    if (check)
        validObject(ans)
    ans
}

setMethod("bindROWS", "Vector", bindROWS_Vector_objects)

### Thin wrapper around bindROWS(). Behave like an endomorphism i.e. return
### an object of the same class as 'x'. In particular 'c(x)' should return 'x'.
### No Vector subclass should need to override this method. See the
### "bindROWS" method for Vector objects above for more information.
setMethod("c", "Vector",
    function(x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop(wmsg("\"c\" method for Vector objects ",
                      "does not support the 'recursive' argument"))
        bindROWS(x, list(...), ignore.mcols=ignore.mcols)
    }
)

### FIXME: This method doesn't work properly on DataFrame derivatives
### if 'after' is >= 1 and < length(x).
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
### Evaluating
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### transform()
###

### NOT exported.
setGeneric("column<-",
           function(x, name, value) standardGeneric("column<-"),
           signature="x")

setReplaceMethod("column", "Vector", function(x, name, value) {
    if (name %in% parallelVectorNames(x)) {
        setter <- get(paste0(name, "<-"), classNamespace(x), mode="function")
        setter(x, value=value)
    } else {
        mcols(x)[[name]] <- value
        x
    }
})

transformColumns <- function(`_data`, ...) {
    exprs <- as.list(substitute(list(...))[-1L])
    if (any(names(exprs) == "")) {
        stop("all arguments in '...' must be named")
    }
    ## elements in '...' can originate from different environments
    env <- setNames(top_prenv_dots(...), names(exprs))
    for (colName in names(exprs)) { # for loop allows inter-arg dependencies
        value <- safeEval(exprs[[colName]], `_data`, env[[colName]])
        column(`_data`, colName) <- value
    }
    `_data`
}

### S3/S4 combo for transform.Vector
transform.Vector <- transformColumns
setMethod("transform", "Vector", transform.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

setGeneric("expand.grid", signature="...")

setMethod("expand.grid", "Vector",
          function(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) {
              args <- list(...)
              inds <- lapply(args, seq_along)
              grid <- do.call(expand.grid,
                              c(inds,
                                KEEP.OUT.ATTRS=KEEP.OUT.ATTRS,
                                stringsAsFactors=stringsAsFactors))
              names(args) <- names(grid)
              ans <- DataFrame(mapply(`[`, args, grid, SIMPLIFY=FALSE),
                               check.names=FALSE)
              metadata(ans)$out.attrs <- attr(grid, "out.attrs")
              ans
          })

### FIXME: tapply method still in IRanges
setMethod("by", "Vector",
          function(data, INDICES, FUN, ..., simplify = TRUE)
          {
              if (!is.list(INDICES)) {
                  INDICES <- setNames(list(INDICES),
                                      deparse(substitute(INDICES))[1L])
              }
              FUNx <- function(i) FUN(extractROWS(data, i), ...)
              structure(tapply(seq_len(NROW(data)), INDICES, FUNx,
                               simplify = simplify),
                        call = match.call(), class = "by")
          })

diff.Vector <- function(x, ...) diff(x, ...)

