### =========================================================================
### Factor objects
### -------------------------------------------------------------------------
###
### The Factor class serves a similar role as factor in base R except that
### the levels of a Factor object can be any vector-like object.
###

setClass("Factor",
    contains="Vector",
    representation(
        levels="vector_OR_Vector",  # will also accept a factor! (see
                                    # Vector-class.R)
        index="integer"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "Factor",
    function(x) c("index", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_Factor <- function(x)
{
    ## 'levels' slot
    if (!is(x@levels, "vector_OR_Vector"))
        return("'levels' slot must be a vector_OR_Vector derivative")
    if (anyDuplicated(x@levels))
        return("'levels' slot contains duplicates")

    ## 'index' slot
    if (!is.integer(x@index))
        return("'index' slot must be an integer vector")
    if (length(x@index) != 0L) {
        ## Strangely, calling min() and max() separately is much faster
        ## than using range().
        index_min <- min(x@index)
        ## Factor objects don't support NAs at the moment.
        if (is.na(index_min))
            return("'index' slot contains NAs")
        index_max <- max(x@index)
        if (index_min < 1L || index_max > NROW(x@levels))
            return("'index' slot contains out-of-bounds indices")
    }

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
### target_class_for_Factor()? Or simply relist_as() and Factor_as()?
setGeneric("FactorToClass", function(x) standardGeneric("FactorToClass"))

setMethod("FactorToClass", "vector_OR_Vector", function(x) "Factor")

.Factor_as <- function(x, levels)
{
    if (!missing(levels))
        return(FactorToClass(levels))
    if (!missing(x))
        return(FactorToClass(x))
    stop(wmsg("at least 'x' or 'levels' must be specified"))
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
        stop(wmsg("all the elements in 'x' must be represented in 'levels'"))
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
        index <- integer(0)
    } else {
        ## 'index' is specified.
        if (!missing(x)) {
            if (!missing(levels))  # Factor(x, levels, index)
                stop(wmsg("at most two out of the 'x', 'levels', and 'index' ",
                          "arguments can be specified"))
            ## Factor(x, index=index)
            levels <- x
        }
        if (!is.numeric(index))
            stop(wmsg("'index' must be an integer vector"))
        if (!is.integer(index))
            index <- as.integer(index)
    }
    mcols <- normarg_mcols(mcols, Class, length(index))
    new2(Class, levels=levels, index=index, elementMetadata=mcols)
}

Factor <- function(x, levels, index=NULL, ...)
{
    Class <- .Factor_as(x, levels)
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

setReplaceMethod("levels", "Factor",
    function(x, value)
    {
        x@levels <- value
        validObject(x)
        x
    }
)

setMethod("nlevels", "Factor", function(x) NROW(x@levels))


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
        ans <- extractROWS(x@levels, x@index)
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

### 'as(x, "Factor")' is the same as 'Factor(x)' with 2 IMPORTANT EXCEPTIONS:
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
        ## In order to be as fast as possible and skip validation, we
        ## don't use 'Factor(levels=levels(from), index=as.integer(from))'.
        new2("Factor", levels=levels(from),
                       index=as.integer(from),
                       check=FALSE)
)

setMethod("as.integer", "Factor", function(x) x@index)

setMethod("as.factor", "Factor",
    function(x)
        structure(x@index, levels=as.character(levels(x)), class="factor")
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
### Note that using identical(x@levels, y@levels) for this would be too
### strigent and identical() is not reliable anyway (can produce false
### positives on objects that use external pointers internally like
### DNAStringSet objects).
.same_levels <- function(x_levels, y_levels)
{
    if (class(x_levels) != class(y_levels))
        return(FALSE)
    x_levels_dim <- dim(x_levels)
    y_levels_dim <- dim(y_levels)
    if (!identical(x_levels_dim, y_levels_dim))
        return(FALSE)
    if (is.null(x_levels_dim) && length(x_levels) != length(y_levels))
        return(FALSE)
    all(x_levels == y_levels)
}

### We trust that 'x' and 'y' are Factor objects. No need to check this.
### Does NOT validate the result.
.concatenate_two_Factor_objects <- function(x, y, use.names=TRUE,
                                                  ignore.mcols=FALSE)
{
    ## 1. Take care of the parallel slots

    ## Use concatenate_Vector_objects() to concatenate parallel slots "index"
    ## and "elementMetadata". Note that the resulting 'ans' can be an invalid
    ## Factor instance (e.g. some indices in 'ans@index' can be wrong).
    ans <- concatenate_Vector_objects(x, list(y), use.names=FALSE,
                                                  ignore.mcols=ignore.mcols,
                                                  check=FALSE)

    ## 2. Take care of slot "levels"

    ## Expedite a common situation.
    if (.same_levels(x@levels, y@levels))
        return(ans)  # all indices in 'ans@index' are correct

    ## Prepare 'ans_levels'.
    m <- match(y@levels, x@levels)
    na_idx <- which(is.na(m))
    ans_levels <- bindROWS(x@levels, list(extractROWS(y@levels, na_idx)))

    ## Prepare 'ans_index'.
    m[na_idx] <- NROW(x@levels) + seq_along(na_idx)
    new_y_index <- m[y@index]

    x_index <- x@index
    if (use.names) {
        names(new_y_index) <- names(y@index)
    } else {
        names(x_index) <- NULL
    }
    ans_index <- c(x_index, new_y_index)

    BiocGenerics:::replaceSlots(ans, index=ans_index,
                                     levels=ans_levels,
                                     check=FALSE)
}

.concatenate_Factor_objects <-
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

setMethod("bindROWS", "Factor", .concatenate_Factor_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparing and ordering
###

setMethod("pcompare", c("Factor", "Factor"),
    function(x, y)
    {
        if (!.same_levels(x@levels, y@levels))
            stop(wmsg("comparing Factor objects ",
                      "is only supported when the objects to compare have ",
                      "the same levels in the same order at the moment"))
        x <- x@index
        y <- y@index
        callGeneric()
    }
)

setMethod("match", c("Factor", "Factor"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        if (!.same_levels(x@levels, table@levels))
            stop(wmsg("matching Factor object 'x' ",
                      "against Factor object 'table' ",
                      "is only supported when 'x' and 'table' have ",
                      "the same levels in the same order at the moment"))
        x <- x@index
        table <- table@index
        callGeneric()
    }
)

setMethod("selfmatch", "Factor",
    function(x, ...)
    {
        x <- x@index
        callGeneric()
    }
)

setMethod("xtfrm", "Factor", function(x) x@index)

