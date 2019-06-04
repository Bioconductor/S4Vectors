### =========================================================================
### Factor objects
### -------------------------------------------------------------------------
###
### The Factor class serves a similar role as factor in base R except that
### the levels of a Factor object can be any Vector derivative.
###


setClass("Factor",
    contains="Vector",
    representation(
        levels="Vector",
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
    if (!is(x@levels, "Vector"))
        return("'levels' slot must be a Vector derivative")
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

.encode_Vector_as_Factor <- function(x, levels, mcols=NULL)
{
    if (!is(x, "Vector"))
        stop(wmsg("'x' must be a Vector derivative"))
    if (missing(levels)) {
        levels <- unique(x)
        check <- FALSE
    } else {
        if (!is(levels, "Vector"))
            stop(wmsg("'levels' must be a Vector derivative"))
        check <- TRUE
    }
    index <- match(x, levels)
    if (check && anyNA(index))
        stop(wmsg("all the elements in 'x' must be represented in 'levels'"))
    x_names <- names(x)
    if (!is.null(x_names))
        names(index) <- x_names
    if (is.null(mcols)) {
        mcols <- mcols(x, use.names=FALSE)
    } else {
        if (nrow(mcols) != length(x))
            stop(wmsg("the supplied metadata columns must be parallel to 'x'"))
    }
    new2("Factor", levels=levels, index=index, elementMetadata=mcols,
                   check=check)
}

### Creating an empty Factor object by calling Factor() with no arguments
### is not supported.
Factor <- function(x, levels, index=NULL, ...)
{
    if (length(list(...)) == 0L) {
        mcols <- NULL
    } else {
        mcols <- DataFrame(..., check.names=FALSE)
    }
    if (is.null(index)) {
        ## 'index' is not specified.
        if (!missing(x))
            return(.encode_Vector_as_Factor(x, levels, mcols=mcols))
        if (missing(levels))  # Factor()
            stop(wmsg("at least 'x' or 'levels' must be specified"))
        ## Factor(levels=levels)
        index <- integer(0)
    } else {
        ## 'index' is specified.
        if (missing(x)) {
            if (missing(levels))  # Factor(index=index)
                stop(wmsg("either 'x' or 'levels' must be specified ",
                          "when 'index' is specified"))
            ## Factor(levels=levels, index=index)
            if (!is(levels, "Vector"))
                stop(wmsg("'levels' must be a Vector derivative"))
        } else {
            if (!missing(levels))  # Factor(x, levels, index)
                stop(wmsg("at most two out of the 'x', 'levels', and 'index' ",
                          "arguments can be specified"))
            ## Factor(x, index=index)
            if (!is(x, "Vector"))
                stop(wmsg("'x' must be a Vector derivative"))
            levels <- x
        }
        if (!is.numeric(index))
            stop(wmsg("'index' must be an integer vector"))
        if (!is.integer(index))
            index <- as.integer(index)
    }
    new2("Factor", levels=levels, index=index, elementMetadata=mcols)
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

setAs("Vector", "Factor", function(from) .encode_Vector_as_Factor(from))

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
    if (identical(x@levels, y@levels))
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
        if (!identical(x@levels, y@levels))
            stop(wmsg("comparing Factor objects 'x' and 'y' is only ",
                      "supported when 'levels(x)' and 'levels(y))' ",
                      "are identical at the moment"))
        x <- x@index
        y <- y@index
        callGeneric()
    }
)

setMethod("match", c("Factor", "Factor"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        if (!identical(x@levels, table@levels))
            stop(wmsg("matching Factor object 'x' ",
                      "against Factor object 'table' is only ",
                      "supported when 'levels(x)' and 'levels(table))' ",
                      "are identical at the moment"))
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

