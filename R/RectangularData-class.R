### =========================================================================
### RectangularData objects
### -------------------------------------------------------------------------
###
### RectangularData is a virtual class with no slots to be extended by
### classes that aim at representing objects with a rectangular shape.
### Current RectangularData derivatives are DataFrame, DelayedMatrix,
### SummarizedExperiment, and Assays objects.
### RectangularData derivatives are expected to support the 2D API: at
### least 'dim()', but also typically 'dimnames()', `[` (the 2D form
### 'x[i, j]'), 'bindROWS()', and 'bindCOLS()'.
###

setClass("RectangularData", representation("VIRTUAL"))

.validate_RectangularData <- function(x)
{
    x_dim <- try(dim(x), silent=TRUE)
    if (inherits(x_dim, "try-error"))
        return("'dim(x)' must work")
    if (!(is.vector(x_dim) && is.numeric(x_dim)))
        return("'dim(x)' must return a numeric vector")
    if (length(x_dim) != 2L)
        return("'x' must have exactly 2 dimensions")
    TRUE
}

setValidity2("RectangularData", .validate_RectangularData)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names() and horizontal_slot_names()
###
### For internal use only.
###

### vertical_slot_names() must return the names of all the slots in
### RectangularData derivative 'x' that are **parallel** to its 1st
### dimension. Slot "foo" in 'x' is considered to be parallel to its
### 1st dimension if it's guaranteed to contain a value that is either
### NULL or such that 'NROW(x@foo)' is equal to 'nrow(x)' and the i-th
### ROW in 'x@foo' is associated with the i-th row in 'x'.
setGeneric("vertical_slot_names",
    function(x) standardGeneric("vertical_slot_names")
)

### horizontal_slot_names() must return the names of all the slots in
### RectangularData derivative 'x' that are **parallel** to its 2nd
### dimension. Slot "bar" in 'x' is considered to be parallel to its
### 2nd dimension if it's guaranteed to contain a value that is either
### NULL or such that 'NROW(x@bar)' is equal to 'ncol(x)' and the j-th
### ROW in 'x@bar' is associated with the j-th col in 'x'.
setGeneric("horizontal_slot_names",
    function(x) standardGeneric("horizontal_slot_names")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("dim", "RectangularData", function(x) c(nrow(x), ncol(x)))

simplify_NULL_dimnames <- function(dimnames)
{
    if (all(sapply_isNULL(dimnames)))
        return(NULL)
    dimnames
}

setMethod("dimnames", "RectangularData",
    function(x)
    {
        ans <- list(rownames(x), colnames(x))
        simplify_NULL_dimnames(ans)
    }
)

setReplaceMethod("dimnames", "RectangularData",
    function(x, value)
    {
        if (is.null(value)) {
            new_rownames <- new_colnames <- NULL
        } else {
            if (!(is.list(value) && length(value) == 2L))
                stop(wmsg("dimnames replacement value must ",
                          "be NULL or a list of length 2"))
            new_rownames <- value[[1L]]
            new_colnames <- value[[2L]]
        }
        rownames(x) <- new_rownames
        colnames(x) <- new_colnames
        x
    }
)

setGeneric("ROWNAMES", function(x) standardGeneric("ROWNAMES"))

setMethod("ROWNAMES", "ANY",
    function (x) if (length(dim(x)) != 0L) rownames(x) else names(x)
)

setMethod("ROWNAMES", "RectangularData", function(x) rownames(x))

setGeneric("ROWNAMES<-", function(x, value) standardGeneric("ROWNAMES<-"))

setReplaceMethod("ROWNAMES", "ANY",
                 function (x, value) {
                     if (length(dim(x)) != 0L)
                         rownames(x) <- value
                     else names(x) <- value
                     x
                 })

setReplaceMethod("ROWNAMES", "RectangularData", function(x, value) {
    rownames(x) <- value
    x
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

head.RectangularData <- utils::head.matrix
setMethod("head", "RectangularData", head.RectangularData)

tail.RectangularData <- utils::tail.matrix
setMethod("tail", "RectangularData", tail.RectangularData)

setMethod("subset", "RectangularData",
    function(x, subset, select, drop=FALSE, ...)
    {
        i <- evalqForSubset(subset, x, ...)
        j <- evalqForSelect(select, x, ...)
        x[i, j, drop=drop]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Binding
###

### S3/S4 combo for rbind.RectangularData
rbind.RectangularData <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the rbind() method for RectangularData objects ",
                     "ignores the 'deparse.level' argument"))
    objects <- list(...)
    bindROWS(objects[[1L]], objects=objects[-1L])
}
setMethod("rbind", "RectangularData", rbind.RectangularData)

### S3/S4 combo for cbind.RectangularData
cbind.RectangularData <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the cbind() method for RectangularData objects ",
                     "ignores the 'deparse.level' argument"))
    objects <- list(...)
    bindCOLS(objects[[1L]], objects=objects[-1L])
}
setMethod("cbind", "RectangularData", cbind.RectangularData)

### Two additional generic functions to bind rectangular objects by rows
### or columns. Unlike rbind() or cbind(), these will handle cases involving
### differences in the colnames or rownames of their input objects by adding
### the missing rows or columns and filling them with NAs.

setGeneric("combineRows",
    function(x, ...) standardGeneric("combineRows")
)

setGeneric("combineCols",
    function(x, ..., use.names=TRUE) standardGeneric("combineCols")
)

### Finally, a more specialized function by Aaron Lun. Implemented on top
### of combineCols() and expected to work on any input objects for which
### combineCols() works.
### Unlike with combineCols(), the ncol() of combineUniqueCols's output is not
### equal to the sum of the ncols() of its inputs. As such, it is a separate
### function rather than being an option in combineCols().
combineUniqueCols <- function(x, ..., use.names=TRUE)
{
    if (missing(x)) {
        all_objects <- list(...)
    } else {
        all_objects <- list(x, ...)
    }

    combined <- do.call(combineCols, c(all_objects, list(use.names=use.names)))
    if (is.null(colnames(combined))) {
        return(combined)
    }

    # Unnamed columns are never considered duplicates of each other.
    retain <- !duplicated(colnames(combined)) | colnames(combined)==""
    combined <- combined[,retain,drop=FALSE]

    all_colnames <- lapply(all_objects, colnames)
    object_indices <- rep(seq_along(all_colnames), lengths(all_colnames))
    col_indices <- sequence(lengths(all_colnames))

    all_colnames <- unlist(all_colnames)
    objects_by_colname <- split(object_indices, all_colnames)
    col_by_colname <- split(col_indices, all_colnames)
    dupped <- names(objects_by_colname)[lengths(objects_by_colname) > 1]

    dupped <- setdiff(dupped, "")

    for (d in dupped) {
        object_affected <- objects_by_colname[[d]]
        col_affected <- col_by_colname[[d]]
        reference <- combined[,d]
        first_object <- object_affected[1]

        if (use.names) {
            filled <- rownames(combined) %in% rownames(all_objects[[first_object]])

            for (i in seq_along(object_affected)[-1]) {
                i_object <- object_affected[i]
                i_col <- col_affected[i]
                cur_object <- all_objects[[i_object]]
                replacements <- cur_object[,i_col]

                candidates <- match(rownames(cur_object), rownames(combined))
                overlapped <- filled[candidates]
                previous <- extractROWS(reference, candidates)

                # Only doing the replacement if the overlaps are identical.
                # Incidentally, this also checks for the right type. We could
                # be more aggressive and do a partial replacement, but
                # something is probably already wrong if this warning fires.
                if (!identical(extractROWS(previous, overlapped), extractROWS(replacements, overlapped))) {
                    warning(wmsg("different values for shared rows in multiple instances of column '",
                        d, "', ignoring this column in ", class(all_objects[[i_object]]), " ", i_object))
                } else {
                    reference <- replaceROWS(reference, candidates, replacements)
                    filled[candidates] <- TRUE
                }
            }

            # Can't use 'combined[ , j] <- col' to replace the column of a
            # data-frame-like object!
            # See https://github.com/Bioconductor/S4Vectors/issues/100
            if (is.data.frame(combined) || is(combined, "DataFrame")) {
                combined[[d]] <- reference
            } else {
                # Expected to work on any rectangular object (e.g. matrix,
                # dgCMatrix, DelayedMatrix, SummarizedExperiment, etc...)
                # except data-frame-like objects.
                combined[ , d] <- reference
            }

        } else {
            for (i in seq_along(object_affected)[-1]) {
                i_object <- object_affected[i]
                i_col <- col_affected[i]
                if (!identical(all_objects[[i_object]][,i_col], reference)) {
                    # In this case, the warning is only emitted if they are not identical.
                    warning(wmsg("different values in multiple instances of column '",
                        d, "', ignoring this column in ", class(all_objects[[i_object]]), " ", i_object))
                }
            }
        }
    }

    combined
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### make_rownames_for_RectangularData_display()
###

### NOT exported but used in package RNAmodR.
make_rownames_for_RectangularData_display <-
    function(x_rownames, nrow, nhead, ntail)
{
    p1 <- ifelse(nhead == 0L, 0L, 1L)
    p2 <- ifelse(ntail == 0L, 0L, ntail - 1L)
    s1 <- s2 <- character(0)
    if (is.null(x_rownames)) {
        if (nhead > 0L)
            s1 <- paste0(as.character(p1:nhead))
        if (ntail > 0L)
            s2 <- paste0(as.character((nrow-p2):nrow))
    } else {
        if (nhead > 0L)
            s1 <- paste0(head(x_rownames, nhead))
        if (ntail > 0L)
            s2 <- paste0(tail(x_rownames, ntail))
    }
    c(s1, "...", s2)
}

