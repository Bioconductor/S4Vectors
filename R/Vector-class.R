### =========================================================================
### Vector objects
### -------------------------------------------------------------------------
###
### The Vector virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###


setClassUnion("DataTable_OR_NULL", c("DataTable", "NULL"))

setClass("Vector",
    contains="Annotated",
    representation(
        "VIRTUAL",
        elementMetadata="DataTable_OR_NULL"
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

### parallelVectorNames() is for internal use only.
setGeneric("parallelVectorNames",
           function(x) standardGeneric("parallelVectorNames"))
setMethod("parallelVectorNames", "ANY",
          function(x) setdiff(colnames(as.data.frame(new(class(x)))), "value"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("length", "Vector",
    function(x) NROW(slot(x, parallelSlotNames(x)[[1L]]))
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

setMethod("anyNA", "Vector", function(x, recursive=FALSE) any(is.na(x)))

setMethod("is.na", "Vector", function(x) rep(FALSE, length(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.Vector.length <- function(x)
{
    x_len <- length(x)
    if (!isSingleNumber(x_len) || x_len < 0L)
        return("'length(x)' must be a single non-negative number")
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
    msg <- NULL
    for (slotname in head(x_pslotnames, -1L)) {
        tmp <- slot(x, slotname)
        if (!(is.null(tmp) || NROW(tmp) == x_len)) {
            what <- paste0("x@", slotname)
            msg <- c(msg, paste0("'", what, "' is not parallel to 'x'"))
        }
    }
    tmp <- mcols(x)
    if (!(is.null(tmp) || nrow(tmp) == x_len)) {
        msg <- c(msg, "'mcols(x)' is not parallel to 'x'")
    }
    msg
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
    if (!is(x_mcols, "DataTable_OR_NULL"))
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
### updateObject()
###
### The default method (defined in BiocGenerics) does complicated, costly,
### and dangerous things, and sometimes it actually breaks valid objects
### (e.g. it breaks valid OverlapEncodings objects). So we overwrite it with
### a method for Vector objects that does nothing! That way it's simple,
### cheap, and safe ;-). And that's really all it needs to do at the moment.
###

setMethod("updateObject", "Vector",
    function(object, ..., verbose=FALSE) object
)


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

setAs("Vector", "data.frame", function(from) as.data.frame(from))

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

setMethod("as.env", "Vector", function(x, enclos, tform = identity) {
  addSelfRef(x, makeFixedColumnEnv(x, as.env(mcols(x), enclos, tform), tform))
})

as.list.Vector <- function(x, ...) as.list(x, ...)
setMethod("as.list", "Vector", function(x, ...) as.list(as(x, "List"), ...))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))

### NOT exported but used in packages IRanges, GenomicRanges,
### SummarizedExperiment, GenomicAlignments, and maybe more...
### 3x faster than new("DataFrame", nrows=nrow).
### 500x faster than DataFrame(matrix(nrow=nrow, ncol=0L)).
make_zero_col_DataFrame <- function(nrow) new_DataFrame(nrows=nrow)

.normalize_mcols_replacement_value <- function(value, x)
{
    x_slots <- getSlots(class(x))
    ## Should never happen because 'x' should always be a Vector object so
    ## should always have the 'elementMetadata' slot.
    if (!("elementMetadata" %in% names(x_slots)))
        stop(wmsg("trying to set metadata columns on an object that does ",
                  "not support them (i.e. with no 'elementMetadata' slot)"))
    target_class <- x_slots[["elementMetadata"]]
    if (is.null(value)) {
        if (is(NULL, target_class))
            return(NULL)
        value <- make_zero_col_DataFrame(length(x))
    }
    if (!is(value, target_class))
        value <- as(value, target_class)
    ## From here 'value' is guaranteed to be a DataTable object.
    if (!is.null(rownames(value)))
        rownames(value) <- NULL
    V_recycle(value, x, x_what="value", skeleton_what="x")
}

setReplaceMethod("elementMetadata", "Vector",
    function(x, ..., value)
    {
        value <- .normalize_mcols_replacement_value(value, x)
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
            mcols(ans) <- mcols(ans)[ , j, drop=FALSE]
        ans
    }
)

### We provide a default "extractROWS" method for Vector objects that subsets
### all the parallel slots.
### Note that this method will work out-of-the-box and do the right thing
### on most Vector subclasses as long as parallelSlotNames() reports the
### names of all the parallel slots on objects of the subclass (some Vector
### subclasses might require a "parallelSlotNames" method for this to happen).
### For those Vector subclasses on which extractROWS() does not work
### out-of-the-box nor do the right thing, it is strongly advised to override
### the method for Vector objects rather than trying to override the "["
### method for Vector objects with a specialized method. The specialized
### "extractROWS" method will typically delegate to the method below via the
### use of callNextMethod(). See "extractROWS" method for Hits for an example.
setMethod("extractROWS", "Vector",
    function(x, i)
    {
        ## Fix old objects on-the-fly (e.g. old GRanges or GAlignments
        ## instances).
        x <- updateObject(x, check=FALSE)
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        x_pslotnames <- parallelSlotNames(x)
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

### Work on any Vector object on which c() and extractROWS() work.
### Assume 'value' is compatible with 'x'.
setMethod("replaceROWS", "Vector",
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)

        ## --<1>-- Concatenate 'x' and 'value' with c() -----

        ## We assume that bindROWS() works on objects of class 'class(x)'
        ## and does the right thing i.e. that it returns an object of the
        ## same class as 'x' and of NROW 'NROW(x) + NROW(value)'. We skip
        ## validation.
        ans <- bindROWS(x, list(value), check=FALSE)

        ## --<2>-- Subset 'c(x, value)' with extractROWS() -----

        idx <- replaceROWS(seq_along(x), i, seq_along(value) + length(x))
        ## Because of how we constructed it, 'idx' is guaranteed to be a valid
        ## subscript to use in 'extractROWS(ans, idx)'. By wrapping it inside a
        ## NativeNSBS object, extractROWS() won't waste time checking it or
        ## trying to normalize it.
        idx <- NativeNSBS(idx, length(ans), TRUE, FALSE)
        ## We assume that extractROWS() works on an object of class 'class(x)'.
        ## For some objects (e.g. Hits), extractROWS() will take care of
        ## validating the returned object.
        ans <- extractROWS(ans, idx)

        ## --<3>-- Restore the original decoration -----

        metadata(ans) <- metadata(x)
        names(ans) <- names(x)
        ## However, we want the replaced elements in 'x' to get their
        ## metadata columns from 'value' so we do not restore the original
        ## metadata columns. See this thread on bioc-devel:
        ##  https://stat.ethz.ch/pipermail/bioc-devel/2015-November/008319.html
        #mcols(ans) <- mcols(x)

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
    if (!is.null(mcols(x))) {
        j <- evalqForSelect(select, mcols(x), ...)
        mcols(x) <- mcols(x)[ , j, drop=FALSE]
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
### Concatenation along the ROWS
###
### Note that supporting "extractROWS" and "c" makes "replaceROWS" (and thus
### "[<-") work out-of-the-box!
###

### Somewhat painful that we do not always have a DataFrame in elementMetadata
ensureMcols <- function(x) {
  mc <- mcols(x)
  if (is.null(mc)) {
    mc <- make_zero_col_DataFrame(length(x))
  }
  mc
}

rbind_mcols <- function(...)
{
    objects <- unname(list(...))
    mcols_list <- lapply(objects, mcols)
    if (length(mcols_list) == 1L)
        return(mcols_list[[1L]])
    mcols_is_null <- sapply_isNULL(mcols_list)
    if (all(mcols_is_null))
        return(NULL)
    mcols_list[mcols_is_null] <- lapply(
        objects[mcols_is_null],
        function(object) make_zero_col_DataFrame(length(object))
    )
    colnames_list <- lapply(mcols_list, colnames)
    all_colnames <- unique(unlist(colnames_list, use.names=FALSE))
    fillCols <- function(df) {
        if (nrow(df))
            df[setdiff(all_colnames, colnames(df))] <- DataFrame(NA)
        df
    }
    do.call(rbind, lapply(mcols_list, fillCols))
}

### We provide a default "bindROWS" method for Vector objects that uses
### bindROWS() internally to concatenate the parallel slots along the ROWS.
### The method behaves like an endomorphism with respect to its first
### argument 'x'. Note that this method will work out-of-the-box and do the
### right thing on most Vector subclasses as long as parallelSlotNames()
### reports the names of all the parallel slots on objects of the subclass
### (some Vector subclasses might require a "parallelSlotNames" method for
### this to happen). For those Vector subclasses on which bindROWS() does not
### work out-of-the-box nor do the right thing, it is strongly advised to
### override the method for Vector objects rather than trying to override
### the "c" method for Vector objects with a specialized method. The
### specialized "bindROWS" method will typically delegate to the method
### below via the use of callNextMethod(). See "bindROWS" methods for
### Hits and Rle objects for some examples.
### No Vector subclass should need to override the "c" method for
### Vector objects.
concatenate_Vector_objects <-
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
    x_pslotnames <- parallelSlotNames(x)
    pslotnames <- setdiff(x_pslotnames, c("NAMES", "elementMetadata"))
    ans_pslots <- lapply(setNames(pslotnames, pslotnames),
        function(slotname) {
            x_slot <- slot(x, slotname)
            if (is.null(x_slot))
                return(NULL)
            slot_list <- lapply(objects, slot, slotname)
            bindROWS(x_slot, slot_list)
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
        ans_mcols <- do.call(rbind_mcols, all_objects)
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

setMethod("bindROWS", "Vector", concatenate_Vector_objects)

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

setReplaceMethod("column", "Vector", function(x, name, value) {
    if (name %in% parallelVectorNames(x)) {
        setter <- get(paste0(name, "<-"), classNamespace(x), mode="function")
        setter(x, value=value)
    } else {
        mcols(x)[[name]] <- value
        x
    }
})

transform.Vector <- transformColumns

setMethod("transform", "Vector", transform.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

setGeneric("expand.grid", signature="...")
BiocGenerics:::apply_hotfix73465(getGeneric("expand.grid"))

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

