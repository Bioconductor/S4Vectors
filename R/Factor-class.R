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
        if (index_min < 1L || index_max > length(x@levels))
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
    x_names <- names(x)
    if (!is.null(x_names))
        names(index) <- x_names
    if (is.null(mcols)) {
        if (is(x, "Vector"))
            mcols <- mcols(x, use.names=FALSE)
    } else {
        if (nrow(mcols) != length(x))
            stop(wmsg("the supplied metadata columns must be parallel to 'x'"))
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

setMethod("unfactor", "Factor",
    function(x, use.names=TRUE, ignore.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop(wmsg("'use.names' must be TRUE or FALSE"))
        if (!isTRUEorFALSE(ignore.mcols))
            stop(wmsg("'ignore.mcols' must be TRUE or FALSE"))
        ans <- x@levels[x@index]
        if (use.names)
            names(ans) <- names(x)
        if (!ignore.mcols)
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
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(x_class, " object of length ", x_len,
        " and ", x_nmc, " metadata ",
        ifelse(x_nmc == 1L, "column", "columns"),
        "\n", sep="")
    x_levels <- levels(x)
    cat("Levels: ", class(x_levels), " object of length ", length(x_levels),
        "\n", sep="")
}

setMethod("show", "Factor", function(object) .show_Factor(object))

setMethod("showAsCell", "Factor",
    function(object)
        showAsCell(unfactor(object, use.names=FALSE, ignore.mcols=TRUE))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

### Returns TRUE if Factor objects 'x' and 'y' have the same levels in the
### same order.
### Note that using identical(x@levels, y@levels) for this would be too
### strigent and identical() is not reliable anyway (can produce false
### positives on objects that use external pointers internally like
### DNAStringSet objects).
.have_same_levels <- function(x, y)
{
    class(x@levels) == class(y@levels) &&
        length(x@levels) == length(y@levels) &&
            all(x@levels == y@levels)
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
    if (.have_same_levels(x, y))
        return(ans)  # all indices in 'ans@index' are correct

    ## Prepare 'ans_levels'.
    m <- match(y@levels, x@levels)
    na_idx <- which(is.na(m))
    ans_levels <- c(x@levels, y@levels[na_idx])  # technically a union

    ## Prepare 'ans_index'.
    m[na_idx] <- length(x@levels) + seq_along(na_idx)
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
        if (!.have_same_levels(x, y))
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
        if (!.have_same_levels(x, table))
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

