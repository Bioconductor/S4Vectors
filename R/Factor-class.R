### =========================================================================
### Factor objects
### -------------------------------------------------------------------------
###
### The Factor class serves a similar role as factor in base R except that
### the levels of a Factor object can be any vector-like object.
### Note that Factor objects don't support NAs at the moment!
###

setClassUnion("integer_OR_raw", c("integer", "raw"))

setClass("Factor",
    contains="Vector",
    representation(
        levels="vector_OR_Vector",  # Will also accept a factor! (see
                                    # Vector-class.R)
        index="integer_OR_raw"      # No NAs for now.
    ),
    prototype(
        index=raw(0)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallel_slot_names()
###

### Combine the new "parallel slots" with those of the parent class. Make
### sure to put the new parallel slots **first**. See Vector-class.R file
### for what slots should or should not be considered "parallel".
setMethod("parallel_slot_names", "Factor",
    function(x) c("index", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_levels_slot <- function(x)
{
    if (!is(x@levels, "vector_OR_Vector"))
        return("'levels' slot must be a vector_OR_Vector derivative")
    if (anyDuplicated(x@levels))
        return("'levels' slot contains duplicates")
    TRUE
}

.validate_index_slot <- function(x)
{
    if (!(is.integer(x@index) || is.raw(x@index)))
        return("'index' slot must be an integer vector or raw vector")
    if (length(x@index) == 0L)
        return(TRUE)

    ## Check that all values in 'index' are >= 1 and <= nlevels.
    ## We will compute 'min(index)' and 'max(index)' for that
    ## which is slightly more efficient than doing something
    ## like 'all(index >= 1L) && all(index <= nlevels)'.
    ## Also, surprisingly, calling min() and max() separately is much
    ## faster than using range().

    nlevels <- NROW(x@levels)
    x_index <- x@index
    ## min() and max() don't work on raw vectors.
    if (is.raw(x_index))
        x_index <- as.integer(x_index)
    index_min <- min(x_index)
    ## Factor objects don't support NAs at the moment.
    if (is.na(index_min))
        return(c("'index' slot contains NAs (but Factor ",
                 "objects don't support NAs at the moment)"))
    index_max <- max(x_index)
    if (index_min < 1L || index_max > nlevels)
        return("'index' slot contains out-of-bounds indices")
    TRUE
}

.validate_Factor <- function(x)
{
    msg <- .validate_levels_slot(x)
    if (!isTRUE(msg))
        return(msg)
    msg <- .validate_index_slot(x)
    if (!isTRUE(msg))
        return(msg)
    TRUE
}

setValidity2("Factor", .validate_Factor)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Kind of follows the naming style of the relistToClass() generic. Yes,
### ugly names, I know :-/
### TODO: Maybe rename these generics class_after_relist() and
### class_after_Factor()? Or target_class_for_relist() and
### target_class_for_Factor()?
setGeneric("FactorToClass", function(x) standardGeneric("FactorToClass"))

setMethod("FactorToClass", "vector_OR_Vector", function(x) "Factor")

.infer_Factor_class <- function(x, levels)
{
    if (!missing(levels))
        return(FactorToClass(levels))
    if (!missing(x))
        return(FactorToClass(x))
    stop(wmsg("at least 'x' or 'levels' must be specified"))
}

### Preserves the names.
.set_index_storage_mode <- function(index, levels)
{
    ## We use `storage.mode<-` instead of as.*(), to preserve the names.
    if (NROW(levels) <= 255L) {
        if (storage.mode(index) != "raw")
            storage.mode(index) <- "raw"
    } else {
        if (storage.mode(index) != "integer")
            storage.mode(index) <- "integer"
    }
    index
}

.encode_as_Factor <- function(x, levels, mcols=NULL, Class="Factor")
{
    if (missing(levels)) {
        levels <- unique(x)
        check <- FALSE
    } else {
        check <- TRUE
    }
    index <- match(x, levels)
    if (check && anyNA(index))
        stop(wmsg("Factor objects don't support NAs at the moment so ",
                  "every element in 'x' must be represented in 'levels'"))
    index <- .set_index_storage_mode(index, levels)
    x_names <- ROWNAMES(x)
    if (!is.null(x_names))
        names(index) <- x_names
    if (!is.null(mcols)) {
        mcols <- normarg_mcols(mcols, Class, length(index))
    } else if (is(x, "Vector")) {
        mcols <- mcols(x, use.names=FALSE)
    }
    new2(Class, levels=levels, index=index, elementMetadata=mcols, check=check)
}

### One of 'x' or 'levels' can be missing, but not both.
.new_Factor <- function(x, levels, index=NULL, mcols=NULL, Class="Factor")
{
    if (is.null(index)) {
        ## 'index' is not specified.
        if (!missing(x)) {
            ans <- .encode_as_Factor(x, levels, mcols=mcols, Class=Class)
            return(ans)
        }
        ## Factor(levels=levels)
        index <- .set_index_storage_mode(raw(0), levels)
    } else {
        ## 'index' is specified.
        if (!missing(x)) {
            if (!missing(levels))  # Factor(x, levels, index)
                stop(wmsg("at most two out of the 'x', 'levels', and 'index' ",
                          "arguments can be specified"))
            ## Factor(x, index=index)
            levels <- x
        }
        if (!is.raw(index)) {
            if (!is.numeric(index))
                stop(wmsg("'index' must be an integer vector or raw vector"))
            ## We use `storage.mode<-` instead of as.integer(), to preserve
            ## the names.
            if (storage.mode(index) != "integer")
                storage.mode(index) <- "integer"
        }
    }
    mcols <- normarg_mcols(mcols, Class, length(index))
    new2(Class, levels=levels, index=index, elementMetadata=mcols, check=TRUE)
}

Factor <- function(x, levels, index=NULL, ...)
{
    Class <- .infer_Factor_class(x, levels)
    if (length(list(...)) == 0L) {
        mcols <- NULL
    } else {
        mcols <- DataFrame(..., check.names=FALSE)
    }
    .new_Factor(x, levels, index=index, mcols=mcols, Class=Class)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("names", "Factor", function(x) names(x@index))

setReplaceMethod("names", "Factor",
    function(x, value)
    {
        names(x@index) <- value
        x
    }
)

### base::levels() works out-of-the-box but base::`levels<-` does NOT.
### Unlike base::`levels<-`, the method below supports reduction of the number
### of levels.
setReplaceMethod("levels", "Factor",
    function(x, value)
    {
        x@levels <- value
        ## We must validate 'x' **before** calling .set_index_storage_mode().
        ## This will validate the supplied levels, and, in case the number of
        ## levels went down, will ensure that it remains >= 'max(x@index)'.
        validObject(x)
        x@index <- .set_index_storage_mode(x@index, x@levels)
        x
    }
)

### base::nlevels(x) returns 'length(levels(x))' so we need to override it.
setMethod("nlevels", "Factor", function(x) NROW(x@levels))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### droplevels()
###

.droplevels.Factor <- function(x)
{
    x_nlevels <- nlevels(x)
    lvl_ranks <- seq_len(x_nlevels)
    x_index <- .set_index_storage_mode(x@index, lvl_ranks)
    if (is.raw(x_index))
        lvl_ranks <- as.raw(lvl_ranks)
    keep_ix <- which(lvl_ranks %in% x_index)
    new_levels <- x@levels[keep_ix]
    new_index <- match(x_index, keep_ix)
    new_index <- .set_index_storage_mode(new_index, new_levels)
    names(new_index) <- names(x@index)
    BiocGenerics:::replaceSlots(x, levels=new_levels,
                                   index=new_index,
                                   check=FALSE)
}

### S3/S4 combo for droplevels.Factor
droplevels.Factor <- function(x, ...) .droplevels.Factor(x, ...)
setMethod("droplevels", "Factor", droplevels.Factor)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unfactor()
###

setGeneric("unfactor", signature="x",
    function(x, use.names=TRUE, ignore.mcols=FALSE) standardGeneric("unfactor")
)

setMethod("unfactor", "factor",
    function(x, use.names=TRUE, ignore.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop(wmsg("'use.names' must be TRUE or FALSE"))
        if (!identical(ignore.mcols, FALSE))
            warning(wmsg("the 'ignore.mcols' argument is ignored ",
                         "when calling unfactor() on a factor"))
        ans <- as.character(x)
        if (use.names)
            names(ans) <- names(x)
        ans
    }
)

### Use same logic as set_unlisted_names() (see R/List-class.R).
.set_names_on_unfactor_ans <- function(ans, x_names)
{
    if (is.null(x_names))
        return(ans)
    if (length(dim(ans)) < 2L) {
        res <- try(names(ans) <- x_names, silent=TRUE)
        what <- "names"
    } else {
        res <- try(rownames(ans) <- x_names, silent=TRUE)
        what <- "rownames"
    }
    if (is(res, "try-error"))
        warning(wmsg("failed to set ", what, " on the result of unfactor() ",
                     "(you can use unfactor(..., use.names=FALSE) to avoid ",
                     "this warning)"))
    ans
}

setMethod("unfactor", "Factor",
    function(x, use.names=TRUE, ignore.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop(wmsg("'use.names' must be TRUE or FALSE"))
        if (!isTRUEorFALSE(ignore.mcols))
            stop(wmsg("'ignore.mcols' must be TRUE or FALSE"))
        ans <- extractROWS(x@levels, as.integer(x@index))
        if (use.names)
            ans <- .set_names_on_unfactor_ans(ans, names(x))
        if (!ignore.mcols && is(ans, "Vector"))
            mcols(ans) <- mcols(x, use.names=FALSE)
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### 'as(x, "Factor")' is the same as 'Factor(x)' WITH 2 IMPORTANT EXCEPTIONS:
###   (1) If 'x' is an ordinary factor, 'as(x, "Factor")' returns a Factor
###       with the same levels, encoding, and names, as 'x'.
###       Note that after coercing an ordinary factor to Factor, going back
###       to factor again (with as.factor()) restores the original object
###       with no loss.
###   (2) If 'x' is a Factor object, 'as(x, "Factor")' is either a no-op
###       (when 'x' is a Factor **instance**), or a demotion to Factor
###       (when 'x' is a Factor derivative like GRangesFactor).
setAs("vector_OR_Vector", "Factor",
    function(from) .encode_as_Factor(from, Class=FactorToClass(from))
)

### Implement exception (1) (see above).
setAs("factor", "Factor",
    function(from)
    {
        if (anyNA(from))
            stop(wmsg("coercing an ordinary factor with NAs to Factor ",
                      "is not supported at the moment"))
        ans_levels <- levels(from)
        ans_index <- .set_index_storage_mode(as.integer(from), ans_levels)
        names(ans_index) <- names(from)
        ## In order to be as fast as possible and skip validation, we
        ## don't use 'Factor(levels=ans_levels, index=ans_index)'.
        new2("Factor", levels=ans_levels, index=ans_index, check=FALSE)
    }
)

### Propagates the names.
setMethod("as.factor", "Factor",
    function(x)
    {
        ans <- as.integer(x)
        attributes(ans) <- list(levels=as.character(levels(x)),
                                class="factor",
                                names=names(x))
        ans
    }
)

### Propagates the names. Note that this is a slight inconsistency with
### what as.integer() does on an ordinary factor.
setMethod("as.integer", "Factor",
    function(x)
    {
        index <- x@index
        ## We use `storage.mode<-` instead of as.integer(), to preserve
        ## the names.
        if (storage.mode(index) != "integer")
            storage.mode(index) <- "integer"
        index
    }
)

### Propagates the names. Note that this is a slight inconsistency with
### what as.raw() does on an ordinary factor.
setMethod("as.raw", "Factor",
    function(x)
    {
        index <- x@index
        ## We use `storage.mode<-` instead of as.raw(), to preserve
        ## the names.
        if (storage.mode(index) != "raw")
            storage.mode(index) <- "raw"
        index
    }
)

setMethod("as.character", "Factor",
    function(x)
    {
        ## 'unfactor(as.factor(x))' and 'as.character(unfactor(x))' are
        ## semantically equivalent. However, depending on whether 'length(x)'
        ## is > 'nlevels(x)' one will be more performant than the other.
        if (length(x) > nlevels(x)) {
            unfactor(as.factor(x))
        } else {
            as.character(unfactor(x, ignore.mcols=TRUE))
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.show_Factor <- function(x)
{
    cat(summary(x), "\n", sep="")
    x_levels <- levels(x)
    x_nlevels <- NROW(x_levels)
    cat("Levels:", class(x_levels), "object ")
    if (length(dim(x_levels)) < 2L) {
        cat("of length", x_nlevels)
    } else {
        cat("with", x_nlevels, if (x_nlevels == 1L) "row" else "rows")
    }
    cat("\n")
}

setMethod("show", "Factor", function(object) .show_Factor(object))

setMethod("showAsCell", "Factor",
    function(object)
        showAsCell(unfactor(object, use.names=FALSE, ignore.mcols=TRUE))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

### Returns TRUE if Factor objects 'x' and 'y' have the same levels in
### the same order.
### Note that using 'identical(x@levels, y@levels)' for this would be too
### strigent e.g. it would also compare the names or/and metadata columns
### of 'x@levels' and 'y@levels'. Furthermore, identical() is not reliable
### in general e.g. it can produce false positives on objects that use
### external pointers internally like DNAStringSet objects.
.same_levels <- function(x_levels, y_levels)
{
    if (class(x_levels) != class(y_levels))
        return(FALSE)
    x_levels_dim <- dim(x_levels)
    y_levels_dim <- dim(y_levels)
    if (!identical(x_levels_dim, y_levels_dim))
        return(FALSE)
    if (is.null(x_levels_dim) && NROW(x_levels) != NROW(y_levels))
        return(FALSE)
    all(x_levels == y_levels)
}

### We trust that 'x' and 'y' are Factor objects. No need to check this.
### Does NOT validate the result.
.concatenate_two_Factor_objects <- function(x, y, use.names=TRUE,
                                                  ignore.mcols=FALSE)
{
    ## 1. Take care of the parallel slots

    ## Use bindROWS_Vector_objects() to concatenate parallel slots "index"
    ## and "elementMetadata". Note that the resulting 'ans' can be an invalid
    ## Factor instance e.g. some indices in 'ans@index' can be wrong if
    ## 'x' and 'y' don't have the same levels. We'll fix this in 4. below.
    ans <- bindROWS_Vector_objects(x, list(y), use.names=FALSE,
                                               ignore.mcols=ignore.mcols,
                                               check=FALSE)

    ## 2. Expedite a common situation

    if (.same_levels(x@levels, y@levels))
        return(ans)  # all indices in 'ans@index' are correct

    ## 3. Combine levels of 'x' and 'y'

    m <- match(y@levels, x@levels)
    na_idx <- which(is.na(m))
    ans_levels <- bindROWS(x@levels, list(extractROWS(y@levels, na_idx)))

    ## 4. Compute 'ans_index'

    m[na_idx] <- NROW(x@levels) + seq_along(na_idx)
    new_y_index <- m[as.integer(y@index)]

    x_index <- .set_index_storage_mode(x@index, ans_levels)
    new_y_index <- .set_index_storage_mode(new_y_index, ans_levels)
    if (use.names) {
        names(new_y_index) <- names(y@index)
    } else {
        names(x_index) <- NULL
    }
    ans_index <- c(x_index, new_y_index)

    BiocGenerics:::replaceSlots(ans, levels=ans_levels,
                                     index=ans_index,
                                     check=FALSE)
}

.bindROWS_Factor_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop(wmsg("'use.names' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(ignore.mcols))
        stop(wmsg("'ignore.mcols' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(check))
        stop(wmsg("'check' must be TRUE or FALSE"))

    objects <- prepare_objects_to_bind(x, objects)
    for (object in objects)
        x <- .concatenate_two_Factor_objects(x, object,
                                             use.names=use.names,
                                             ignore.mcols=ignore.mcols)
    if (check)
        validObject(x)
    x
}

setMethod("bindROWS", "Factor", .bindROWS_Factor_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparing and ordering
###

.two_factor_comparison <- function(x, y, unfactored.FUN, combined.FUN, ...) {
    if (max(length(x), length(y)) < max(length(x@levels), length(y@levels))) {
        x <- unfactor(x, use.names = FALSE, ignore.mcols = TRUE)
        y <- unfactor(y, use.names = FALSE, ignore.mcols = TRUE)
        unfactored.FUN(x, y, ...)
    } else {
        if (!.same_levels(x@levels, y@levels)) {
            combined <- c(x, y)
            x <- head(combined, length(x))
            y <- tail(combined, length(y))
        }
        combined.FUN(x, y, ...)
    }
}

setMethod("pcompare", c("Factor", "Factor"),
    function(x, y)
    {
        .two_factor_comparison(x, y,
            unfactored.FUN=pcompare,
            combined.FUN=function(x, y) {
                i <- xtfrm(x@levels)
                pcompare(i[as.integer(x)], i[as.integer(y)])
            }
        )
    }
)

setMethod("match", c("Factor", "Factor"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        .two_factor_comparison(x, table,
            unfactored.FUN=match,
            combined.FUN=function(x, table, ...) {
                match(as.integer(x), as.integer(table), ...)
            },
            nomatch=nomatch, incomparables=incomparables, ...
        )
    }
)

setMethod("selfmatch", "Factor",
    function(x, ...)
    {
        x <- x@index
        callGeneric()
    }
)

setMethod("xtfrm", "Factor", function(x) xtfrm(x@levels)[as.integer(x@index)])

