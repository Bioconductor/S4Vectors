### =========================================================================
### DataFrame objects
### -------------------------------------------------------------------------

## A data.frame-like interface for S4 objects that implement length() and `[`

## NOTE: Normal data.frames always have rownames (sometimes as integers),
## but we allow the rownames to be NULL for efficiency. This means that we
## need to store the number of rows (nrows).
setClass("DataFrame",
         representation(
                        rownames = "character_OR_NULL",
                        nrows = "integer"
                        ),
         prototype(rownames = NULL,
                   nrows = 0L,
                   listData = structure(list(), names = character())),
         contains = c("RectangularData", "SimpleList"))

### Add DataFrame to the DataFrame_OR_NULL union.
setIs("DataFrame", "DataFrame_OR_NULL")

## Just a direct DataFrame extension with no additional slot for now. Once all
## serialized DataFrame instances are replaced with DFrame instances (which
## will take several BioC release cycles) we'll be able to move the DataFrame
## slots from the DataFrame class definition to the DFrame class definition.
## The final goal is to have DataFrame become a virtual class with no slots
## that only extends RectangularData, and DFrame a concrete DataFrame and
## SimpleList subclass that has the same slots as the current DataFrame class.
setClass("DFrame", contains="DataFrame")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names() and horizontal_slot_names()
###

setMethod("vertical_slot_names", "DataFrame",
    function(x) "rownames"
)

setMethod("horizontal_slot_names", "DataFrame",
    function(x) parallel_slot_names(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### updateObject()
###

setMethod("updateObject", "DataFrame",
    function(object, ..., verbose=FALSE)
    {
        ## class attribute.
        if (class(object) == "DataFrame") {
            ## Starting with S4Vectors 0.23.19, all DataFrame instances need
            ## to be replaced with DFrame instances. Note that this is NOT a
            ## change of the internals, only a change of the class attribute.
            if (verbose)
                message("[updateObject] Setting class attribute of DataFrame ",
                        "instance to \"DFrame\" ... ", appendLF=FALSE)
            class(object) <- class(new("DFrame"))
            if (verbose)
                message("OK")
        } else {
            if (verbose)
                message("[updateObject] ", class(object), " object ",
                        "is current.\n",
                        "[updateObject] Nothing to update.")
        }

        callNextMethod()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("nrow", "DataFrame", function(x) x@nrows)

setMethod("ncol", "DataFrame", function(x) length(x))

setMethod("dim", "DataFrame", function(x) c(nrow(x), ncol(x)))

setMethod("rownames", "DataFrame",
          function(x, do.NULL = TRUE, prefix = "row")
          {
            rn <- x@rownames
            if (is.null(rn) && !do.NULL) {
              nr <- NROW(x)
              if (nr > 0L)
                rn <- paste(prefix, seq_len(nr), sep = "")
              else
                rn <- character(0L)
            }
            rn
          })

setMethod("colnames", "DataFrame",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (!identical(do.NULL, TRUE)) warning("do.NULL arg is ignored ",
                "in this method")
            cn <- names(x@listData)
            if (!is.null(cn))
                return(cn)
            if (length(x@listData) != 0L)
                stop("DataFrame object with NULL colnames, please fix it ",
                     "with colnames(x) <- value")
            return(character(0))
          })

setMethod("dimnames", "DataFrame",
    function(x) list(rownames(x), colnames(x))
)

setReplaceMethod("rownames", "DataFrame",
                 function(x, value)
                 {
                   if (!is.null(value)) {
                     if (anyMissing(value))
                       stop("missing values not allowed in rownames")
                     if (length(value) != nrow(x))
                       stop("invalid rownames length")
                     if (!is(value, "XStringSet"))
                       value <- as.character(value)
                   }
                   x@rownames <- value
                   x
                 })

setReplaceMethod("colnames", "DataFrame",
                 function(x, value)
                 {
                   if (!is.character(value))
                       stop("'value' must be a character vector ",
                            "in colnames(x) <- value")
                   if (length(value) > length(x))
                     stop("more column names than columns")
                   names(x) <- value
                   x
                 })

setReplaceMethod("dimnames", "DataFrame",
    function(x, value)
    {
        if (!(is.list(value) && length(value) == 2L))
            stop("dimnames replacement value must be a list of length 2")
        rownames(x) <- value[[1L]]
        colnames(x) <- value[[2L]]
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.DataFrame.dim <- function(x)
{
  nr <- dim(x)[1L]
  if (!length(nr) == 1)
    return("length of 'nrows' slot must be 1")
  if (nr < 0)
    return("number of rows must be non-negative")
  NULL
}

.valid.DataFrame.rownames <- function(x)
{
  if (is.null(rownames(x)))
    return(NULL)
  if (length(rownames(x)) != nrow(x))
    return("number of row names and number of rows differ")
  NULL
}

.valid.DataFrame.names <- function(x)
{
  if (is.null(names(x)))
    return("column names should not be NULL")
  if (length(names(x)) != ncol(x))
    return("number of columns and number of column names differ")
  NULL
}

.OLD_DATAFRAME_INSTANCE_MSG <- c(
    "Note that starting with BioC 3.10, the class attribute ",
    "of all DataFrame **instances** should be set to ",
    "\"DFrame\". Please update this object ",
    "with 'updateObject(object, verbose=TRUE)' and ",
    "re-serialize it."
)

.valid.DataFrame <- function(x)
{
  ## class() is broken when used within a validity method. See:
  ##   https://stat.ethz.ch/pipermail/r-devel/2019-August/078337.html
  #if (class(x) == "DataFrame")
  #  return(paste(.OLD_DATAFRAME_INSTANCE_MSG, collapse=""))
  c(.valid.DataFrame.dim(x),
    .valid.DataFrame.rownames(x),
    .valid.DataFrame.names(x))
}

setValidity2("DataFrame", .valid.DataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Low-level constructor. For internal use only.
### Note that, when supplied, 'nrows' is trusted (except when 'listData' is a
### data.frame or data-frame-like object).
### Calling 'new_DataFrame(x)' on an ordinary named list or an ordinary
### data.frame 'x' will turn it into a DataFrame in the most possibly
### straightforward way. In particular calling 'as.list()' or 'as.data.frame()'
### on the returned DataFrame will bring back 'x'.
### This is unlike 'DataFrame(x)' or 'as(x, "DataFrame")' which can do all
### kind of hard-to-predict mangling to 'x', unless the user does something
### like 'DataFrame(lapply(x, I))'. Not super convenient or intuitive!
new_DataFrame <- function(listData=list(), nrows=NA, what="arguments")
{
    stopifnot(is.list(listData))
    stopifnot(isSingleNumberOrNA(nrows))
    if (!is.integer(nrows))
        nrows <- as.integer(nrows)
    listData_nrow <- nrow(listData)
    if (is.null(listData_nrow)) {
        ## 'listData' is NOT a data.frame or data-frame-like object.
        if (length(listData) == 0L) {
            if (is.na(nrows))
                nrows <- 0L
            names(listData) <- character(0)
        } else {
            if (is.na(nrows)) {
                elt_nrows <- elementNROWS(listData)
                nrows <- elt_nrows[[1L]]
                if (!all(elt_nrows == nrows))
                    stop(wmsg(what, " imply differing number of rows"))
            }
            if (is.null(names(listData)))
                names(listData) <- paste0("V", seq_along(listData))
        }
    } else {
        ## 'listData' is a data.frame or data-frame-like object.
        if (is.na(nrows)) {
            nrows <- listData_nrow
        } else if (nrows != listData_nrow) {
            stop(wmsg("the supplied 'nrows' does not match ",
                      "the nb of rows in 'listData'"))
        }
        listData <- as.list(listData)
    }
    new2("DFrame", nrows=nrows, listData=listData, check=FALSE)
}

DataFrame <- function(..., row.names = NULL, check.names = TRUE,
                      stringsAsFactors)
{
  ## build up listData, with names from arguments
  if (!isTRUEorFALSE(check.names))
    stop("'check.names' must be TRUE or FALSE")
  if (!missing(stringsAsFactors))
    warning("'stringsAsFactors' is ignored")
  nr <- 0
  listData <- list(...)
  varlist <- vector("list", length(listData))
  metadata <- list()
  if (length(listData) > 0) {
    if (is(listData[[1L]], getClass("Annotated")))
        metadata <- metadata(listData[[1L]])
    dotnames <- names(listData)
    if (is.null(dotnames)) {
        dotnames <- rep("", length(listData))
    }
    qargs <- as.list(substitute(list(...)))[-1L]
    varnames <- as.list(dotnames)
    varnames[dotnames == ""] <- list(NULL)
    nrows <- ncols <- integer(length(varnames))
    for (i in seq_along(listData)) {
      element <- try(as(listData[[i]], "DFrame"), silent = TRUE)
      if (inherits(element, "try-error"))
        stop("cannot coerce class \"", class(listData[[i]])[1L],
             "\" to a DataFrame")
      nrows[i] <- nrow(element)
      ncols[i] <- ncol(element)
      varlist[[i]] <- element
      if (is(listData[[i]], "AsIs")) {
        listData[[i]] <- drop_AsIs(listData[[i]])
      } else {
        if ((length(dim(listData[[i]])) > 1L) || (ncol(element) > 1L) ||
             is.list(listData[[i]]))
          {
            if (is.null(varnames[[i]]))
              varnames[[i]] <- colnames(element)
            else
              varnames[[i]] <- paste(varnames[[i]], colnames(element), sep = ".")
          }
      }
      if (is.null(varnames[[i]])) {
          varnames[[i]] <- deparse(qargs[[i]])[1L]
      }
      if (missing(row.names))
        row.names <- rownames(element)
    }
    mcols <- do.call(rbind_mcols, varlist)
    varlist <- lapply(varlist, as.list, use.names = FALSE)
    nr <- max(nrows)
    for (i in which((nrows > 0L) & (nrows < nr) & (nr %% nrows == 0L))) {
      recycle <- rep(seq_len(nrows[i]), length.out = nr)
      varlist[[i]] <- lapply(varlist[[i]], `[`, recycle, drop=FALSE)
      nrows[i] <- nr
    }
    if (!all(nrows == nr))
      stop("different row counts implied by arguments")
    varlist <- unlist(varlist, recursive = FALSE, use.names = FALSE)
    nms <- as.character(unlist(varnames[ncols > 0L]))
    if (check.names)
      nms <- make.names(nms, unique = TRUE)
    names(varlist) <- nms
  } else {
      names(varlist) <- character(0)
      mcols <- NULL
  }

  if (!is.null(row.names)) {
    if (anyMissing(row.names))
      stop("missing values in 'row.names'")
    if (length(varlist) && length(row.names) != nr)
      stop("invalid length of row names")
    row.names <- as.character(row.names)
  }

  ans <- new_DataFrame(varlist, nrows=as.integer(max(nr, length(row.names))))
  ans@rownames <- row.names
  mcols(ans) <- mcols
  metadata(ans) <- metadata
  ans
}

### Exported. Intended for developers to use in other packages and typically
### not needed by the end user.
### 3x faster than new("DFrame", nrows=nrow).
### 500x faster than DataFrame(matrix(nrow=nrow, ncol=0L)).
make_zero_col_DFrame <- function(nrow=0L)
{
    stopifnot(isSingleNumber(nrow))
    if (!is.integer(nrow))
        nrow <- as.integer(nrow)
    stopifnot(nrow >= 0L)
    new2("DFrame", nrows=nrow, check=FALSE)
}

### Alias for backward compatibility.
### NOT exported but used in packages IRanges, GenomicRanges,
### SummarizedExperiment, GenomicAlignments, and maybe more...
make_zero_col_DataFrame <- make_zero_col_DFrame


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("[[", "DataFrame", function(x, i, j, ...)
{
    if (!missing(j)) {
        x[[j, ...]][[i]]
    } else {
        callNextMethod()
    }
})

setReplaceMethod("[[", "DataFrame",
                 function(x, i, j,..., value)
                 {
                   nrx <- nrow(x)
                   lv <- NROW(value)
                   if (!missing(j) || length(list(...)) > 0)
                     warning("arguments beyond 'i' ignored")
                   if (missing(i))
                     stop("subscript is missing")
                   if (!is.character(i) && !is.numeric(i))
                     stop("invalid subscript type")
                   if (length(i) < 1L)
                     stop("attempt to select less than one element")
                   if (length(i) > 1L)
                     stop("attempt to select more than one element")
                   if (is.numeric(i) && (i < 1L || i > ncol(x) + 1L))
                     stop("subscript out of bounds")
                   if (!is.null(value) && (nrx != lv)) {
                     if ((nrx == 0) || (lv == 0) || (nrx %% lv != 0))
                       stop(paste(lv, "elements in value to replace",
                                  nrx, "elements"))
                     else
                       value <- rep(value, length.out = nrx)
                   }
                   callNextMethod(x, i, value=value)
                 })

setMethod("extractROWS", "DataFrame",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, exact=FALSE, allow.NAs=TRUE,
                                             as.NSBS=TRUE)
        slot(x, "listData", check=FALSE) <- lapply(as.list(x), extractROWS, i)
        slot(x, "nrows", check=FALSE) <- length(i)
        if (!is.null(rownames(x)))
            slot(x, "rownames", check=FALSE) <- extractROWS(rownames(x), i)
        x
    }
)

setMethod("extractCOLS", "DataFrame", function(x, i) {
    if (missing(i))
        return(x)
    if (!is(i, "IntegerRanges")) {
        xstub <- setNames(seq_along(x), names(x))
        i <- normalizeSingleBracketSubscript(i, xstub)
    }
    new_listData <- extractROWS(x@listData, i)
    new_mcols <- extractROWS(mcols(x, use.names=FALSE), i)
    x <- initialize(x, listData=new_listData,
                    elementMetadata=new_mcols)
    if (anyDuplicated(names(x)))
        names(x) <- make.unique(names(x))
    x
})

setMethod("[", "DataFrame",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!isTRUEorFALSE(drop))
            stop("'drop' must be TRUE or FALSE")
        if (length(list(...)) > 0L)
            warning("parameters in '...' not supported")

        ## NOTE: matrix-style subsetting by logical matrix not supported.
        list_style_subsetting <- (nargs() - !missing(drop)) < 3L
        if (list_style_subsetting || !missing(j)) {
            if (list_style_subsetting) {
                if (!missing(drop))
                    warning("'drop' argument ignored by list-style subsetting")
                if (missing(i))
                    return(x)
                j <- i
            }
            x <- extractCOLS(x, j)
            if (list_style_subsetting)
                return(x)
        }
        if (!missing(i))
            x <- extractROWS(x, i)
        if (missing(drop))  # drop by default if only one column left
            drop <- ncol(x) == 1L
        if (drop) {
            ## one column left
            if (ncol(x) == 1L)
                return(x[[1L]])
            ## one row left
            if (nrow(x) == 1L)
                return(as(x, "list"))
        }
        x
    }
)

.make_rownames <- function(x, i, nsbs, value)
{
    x_nrow <- nrow(x)
    x_rownames <- rownames(x)
    if (!missing(i) && is.character(i)) {
        value_rownames <- i
    } else {
        value_rownames <- rownames(value)
    }
    nsbs <- as.integer(nsbs)
    i_max <- max(nsbs, x_nrow)
    if (i_max <= x_nrow || is.null(x_rownames) && is.null(value_rownames))
        return(x_rownames)
    if (is.null(value_rownames))
        value_rownames <- as.character(nsbs)
    if (is.null(x_rownames))
        x_rownames <- as.character(seq_len(x_nrow))
    replaceROWS(x_rownames, nsbs[nsbs > x_nrow], value_rownames[nsbs > x_nrow])
}

.subassign_columns <- function(x, nsbs, value) {
    x_ncol <- ncol(x)
    value_ncol <- length(value)
    if (value_ncol > x_ncol)
        stop("provided ", value_ncol, " variables ",
             "to replace ", x_ncol, " variables")
    if (x_ncol != 0L) {
        if (value_ncol == 0L)
            stop("replacement has length zero")
        FUN <- if (nsbs@upper_bound_is_strict) replaceROWS else mergeROWS
        new_listData <-
            lapply(structure(seq_len(ncol(x)), names=names(x)),
                   function(j)
                       FUN(x[[j]], nsbs,
                           value[[((j - 1L) %% value_ncol) + 1L]]))
        slot(x, "listData", check=FALSE) <- new_listData
    }
    x
}

setMethod("replaceROWS", c("DataFrame", "ANY"),
          function(x, i, value)
          {
              nsbs <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
              if (length(nsbs) == 0L) {
                  return(x)
              }
              .subassign_columns(x, nsbs, value)
          })

setMethod("mergeROWS", c("DataFrame", "ANY"),
    function(x, i, value)
    {
        nsbs <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE,
                                                as.NSBS=TRUE)
        if (length(nsbs) == 0L) {
            return(x)
        }
        x <- .subassign_columns(x, nsbs, value)
        i_max <- max(as.integer(nsbs))
        x_nrow <- nrow(x)
        if (i_max > x_nrow) {
            x@rownames <- .make_rownames(x, i, nsbs, value)
            x@nrows <- i_max
        }
        x
    }
)

.make_colnames <- function(x, i, x_len, value) {
    if (!missing(i) && is.numeric(i) && length(i) > 0L) {
        appended <- i > x_len
        if (!is.null(names(value))) {
            newcn <- names(value)[appended]
        } else {
            newcn <- paste0("V", i[appended])
        }
        names(x)[i[appended]] <- newcn
        names(x) <- make.unique(names(x))
    }
    names(x)
}

.fill_short_columns <- function(x, max_len) {
    short <- lengths(x) < max_len
    x[short] <- SimpleList(lapply(x[short], function(xi) {
        length(xi) <- max_len
        xi
    }))
    x
}

### Not a real default replaceCOLS() method for DataFrame objects (it actually
### assumes that 'x' derives from SimpleList i.e. that 'x' is a DFrame object
### or derivative).
setMethod("replaceCOLS", c("DataFrame", "ANY"), function(x, i, value) {
    stopifnot(is.null(value) || is(value, "DataFrame"))
    sl <- as(x, "SimpleList")
    value_sl <- if (!is.null(value)) as(value, "SimpleList")
    if (missing(i))
        sl[] <- value_sl
    else sl[i] <- value_sl
    max_len <- max(lengths(sl), nrow(x))
    sl <- .fill_short_columns(sl, max_len)
    names(sl) <- .make_colnames(sl, i, length(x), value)
    ri <- seq_len(max_len)
    ## Assumes that 'x' has a "listData" slot i.e. that 'x' is a DFrame object
    ## or derivative.
    BiocGenerics:::replaceSlots(x, listData=sl@listData,
                                   elementMetadata=sl@elementMetadata,
                                   rownames=.make_rownames(x, ri, ri, value),
                                   nrows=max_len)
})

setMethod("normalizeSingleBracketReplacementValue", "DataFrame",
          function(value, x)
          {
              hasColumns <- is(value, "DataFrame") || is.list(value) ||
                  length(dim(value)) >= 2L
              if (is.null(value) || (hasColumns && length(value) == 0L))
                  return(NULL)
              value <- as(value, "DataFrame", strict=FALSE)
              if (!hasColumns) {
                  names(value) <- NULL # don't try this at home
              }
              value
          })

.add_missing_columns <- function(x, j) {
    if (!missing(j)) {
        j2 <- normalizeSingleBracketSubscript(j, as.list(x),
                                              allow.append=TRUE)
        x[j[j2 > ncol(x)]] <- NA
    }
    x
}

setReplaceMethod("[", "DataFrame",
                 function(x, i, j, ..., value)
{
    if (length(list(...)) > 0)
        warning("parameters in '...' not supported")
    value <- normalizeSingleBracketReplacementValue(value, x)
    if (nargs() < 4) {
        value <- recycleSingleBracketReplacementValue(value, x)
        replaceCOLS(x, i, value)
    } else {
        value <- recycleSingleBracketReplacementValue(value, x, i)
        if (!missing(i)) {
            x <- .add_missing_columns(x, j)
            value <- mergeROWS(extractCOLS(x, j), i, value)
        }
        replaceCOLS(x, j, value)
    }
})

hasNonDefaultMethod <- function(f, signature) {
  any(selectMethod(f, signature)@defined != "ANY")
}

hasS3Method <- function(f, signature) {
  !is.null(getS3method(f, signature, optional=TRUE))
}

droplevels.DataFrame <- function(x, except=NULL) {
  canDropLevels <- function(xi) {
    hasNonDefaultMethod(droplevels, class(xi)[1L]) ||
      hasS3Method("droplevels", class(xi))
  }
  drop.levels <- vapply(x, canDropLevels, NA)
  if (!is.null(except))
    drop.levels[except] <- FALSE
  x@listData[drop.levels] <- lapply(x@listData[drop.levels], droplevels)
  x
}
setMethod("droplevels", "DataFrame", droplevels.DataFrame)

setMethod("rep", "DataFrame", function(x, ...) {
  x[rep(seq_len(nrow(x)), ...),,drop=FALSE]
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

.as.data.frame.DataFrame <- function(x, row.names=NULL, optional=FALSE,
                                     stringsAsFactors=FALSE, ...)
{
    stopifnot(identical(stringsAsFactors, FALSE))
    if (length(list(...)))
        warning("Arguments in '...' ignored")
    if (is.null(row.names)) {
        row.names <- rownames(x)
        if (!is.null(row.names))
            row.names <- make.unique(row.names)
        else if (ncol(x) == 0L)
            row.names <- seq_len(nrow(x))
    }
    old_option <- getOption("stringsAsFactors")
    options(stringsAsFactors=FALSE)
    on.exit(options(stringsAsFactors=old_option))
    x_colnames <- colnames(x)
    df_list <- lapply(setNames(seq_along(x), x_colnames),
        function(j) {
            col <- x[[j]]
            if (is.data.frame(col))
                return(col)
            if (is(col, "DataFrame"))
                return(as.data.frame(col, optional=optional))
            ## If 'col is an AtomicList derivative (e.g. IntegerList,
            ## CharacterList, etc...) or other List derivative that compares
            ## recursively (i.e. not an IRanges, GRanges, or DNAStringSet,
            ## etc... object), we turn it into an ordinary list. This is
            ## because the "as.data.frame" method for List objects produces
            ## this weird data frame:
            ##   > as.data.frame(IntegerList(11:12, 21:23))
            ##     group group_name value
            ##   1     1       <NA>    11
            ##   2     1       <NA>    12
            ##   3     2       <NA>    21
            ##   4     2       <NA>    22
            ##   5     2       <NA>    23
            ## which is not what we want here.
            ## List derivatives that compare recursively should not need this
            ## because they are expected to override the "as.data.frame" method
            ## for List objects with a method that returns a data.frame with
            ## one row per list element.
            if (is(col, "List") && pcompareRecursively(col))
                col <- as.list(col)
            if (is.list(col))
                col <- I(col)
            df <- as.data.frame(col, optional=optional)
            if (is.null(colnames(col)) && ncol(df) == 1L)
                colnames(df) <- x_colnames[[j]]
            df
        })
    do.call(data.frame,
            c(df_list, list(row.names=row.names,
                            check.names=!optional,
                            stringsAsFactors=FALSE)))
}
setMethod("as.data.frame", "DataFrame", .as.data.frame.DataFrame)

setMethod("as.matrix", "DataFrame", function(x) {
  if (length(x) == 0L)
    m <- matrix(logical(), nrow = nrow(x), ncol = 0L)
  else m <- do.call(cbind, as.list(x))
  rownames(m) <- rownames(x)
  m
})

## take data.frames to DataFrames
setAs("data.frame", "DFrame",
      function(from) {
        rn <- attributes(from)[["row.names"]]
        if (is.integer(rn))
          rn <- NULL
        nr <- nrow(from)
### FIXME: this should be:
        ## from <- as.list(from)
### But unclass() causes deep copy
        attr(from, "row.names") <- NULL
        class(from) <- NULL
        ans <- new_DataFrame(from, nrows=nr)
        ans@rownames <- rn
        ans
      })

setAs("data.table", "DFrame",
    function(from)
    {
        df <- data.table:::as.data.frame.data.table(from)
        as(df, "DFrame")
    }
)

setAs("table", "DFrame",
      function(from) {
        df <- as.data.frame(from)
        factors <- sapply(df, is.factor)
        factors[1] <- FALSE
        do.call(DataFrame, c(df[1], lapply(df[factors], Rle), df["Freq"]))
      })

setOldClass(c("xtabs", "table"))
setAs("xtabs", "DFrame",
      function(from) {
        class(from) <- "table"
        as(from, "DFrame")
      })

.defaultAsDataFrame <- function(from) {
  if (length(dim(from)) == 2L) {
    df <- as.data.frame(from, stringsAsFactors=FALSE)
    if (0L == ncol(from))
      ## colnames on matrix with 0 columns are 'NULL'
      names(df) <- character()
    as(df, "DFrame")
  } else {
    ans <- new_DataFrame(setNames(list(from), "X"), nrows=length(from))
    ans@rownames <- names(from)
    ans
  }
}

setAs("ANY", "DFrame", .defaultAsDataFrame)

setAs("ANY", "DataFrame", function(from) as(from, "DFrame"))
setAs("SimpleList", "DataFrame", function(from) as(from, "DFrame"))

## Only temporarily needed (until we make DataFrame VIRTUAL).
setAs("DFrame", "DataFrame", function(from) from)

.VectorAsDataFrame <- function(from) {
  ans <- .defaultAsDataFrame(from)
  from_mcols <- mcols(from, use.names=FALSE)
  if (!is.null(from_mcols))
    ans <- cbind(ans, from_mcols)
  ans
}

## overriding the default inheritance-based coercion from methods package
setAs("SimpleList", "DFrame", .VectorAsDataFrame)
setAs("Vector", "DFrame", .VectorAsDataFrame)

## note that any element named 'row.names' will be interpreted differently
## is this a bug or a feature?
setAs("list", "DFrame",
      function(from) {
        do.call(DataFrame, c(from, list(check.names=is.null(names(from)))))
      })

setAs("NULL", "DFrame", function(from) as(list(), "DFrame"))

setAs("AsIs", "DFrame",
      function(from) {
        new_DataFrame(setNames(list(drop_AsIs(from)), "X"))
      })

setAs("ANY", "DataFrame_OR_NULL", function(from) as(from, "DFrame"))

setMethod("coerce2", "DataFrame",
    function(from, to)
    {
        to_class <- class(to)
        if (class(from) == "list") {
            ## Turn an ordinary list into a DataFrame in the most possibly
            ## straightforward way.
            ans <- new_DataFrame(from, what="list elements")
            if (is(ans, to_class))
                return(ans)
            ans <- as(ans, to_class, strict=FALSE)
            ## Even though coercion from DataFrame to 'class(to)' "worked", it
            ## can return a broken object. This happens when an automatic
            ## coercion method gets in the way. The problem with these methods
            ## is that they often do the wrong thing and don't even bother to
            ## validate the object they return!
            ## One possible problem with an automatic coercion method from
            ## DataFrame to a DataFrame subclass is that it will set the
            ## elementType slot to "ANY" which could be wrong. So we fix this.
            ans@elementType <- to@elementType
            validObject(ans)
            return(ans)
        }
        ## Some objects like SplitDataFrameList have a "dim" method that
        ## returns a non-MULL object (a matrix!) even though they don't have
        ## an array-like (or matrix-like) semantic.
        from_dim <- dim(from)
        if (length(from_dim) == 2L && !is.matrix(from_dim)) {
            if (is(from, to_class))
                return(from)
            ans <- as(from, to_class, strict=FALSE)
            if (!identical(dim(ans), from_dim))
                stop(wmsg("coercion of ", class(from), " object to ", to_class,
                          " didn't preserve its dimensions"))
            ## Try to restore the dimnames if they were lost or altered.
            from_dimnames <- dimnames(from)
            if (!identical(dimnames(ans), from_dimnames)) {
                tmp <- try(`dimnames<-`(ans, value=from_dimnames), silent=TRUE)
                if (!inherits(tmp, "try-error"))
                    ans <- tmp
            }
            return(ans)
        }
        callNextMethod()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("classNameForDisplay", "DFrame",
    function(x) if (class(x) == "DFrame") "DataFrame" else class(x)
)

setMethod("makeNakedCharacterMatrixForDisplay", "DataFrame",
    function(x)
    {
        df <- data.frame(lapply(x, showAsCell), check.names=FALSE,
                         row.names=NULL)
        as.matrix(format(df))
    }
)

make_class_info_for_DataFrame_display <- function(x)
{
    vapply(x, function(xi) paste0("<", classNameForDisplay(xi), ">"),
           character(1), USE.NAMES=FALSE)
}

.show_DataFrame <- function(x)
{
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    x_nrow <- nrow(x)
    x_ncol <- ncol(x)
    cat(classNameForDisplay(x), " with ",
        x_nrow, " row", ifelse(x_nrow == 1L, "", "s"),
        " and ",
        x_ncol, " column", ifelse(x_ncol == 1L, "", "s"),
        "\n", sep="")
    if (x_nrow != 0L && x_ncol != 0L) {
        x_rownames <- rownames(x)
        if (x_nrow <= nhead + ntail + 1L) {
            m <- makeNakedCharacterMatrixForDisplay(x)
            if (!is.null(x_rownames))
                rownames(m) <- x_rownames
        } else {
            m <- rbind(makeNakedCharacterMatrixForDisplay(head(x, nhead)),
                       rbind(rep.int("...", x_ncol)),
                       makeNakedCharacterMatrixForDisplay(tail(x, ntail)))
            rownames(m) <- make_rownames_for_RectangularData_display(
                                             x_rownames, x_nrow,
                                             nhead, ntail)
        }
        m <- rbind(make_class_info_for_DataFrame_display(x), m)
        print(m, quote=FALSE, right=TRUE)
    }
    invisible(NULL)
}

setMethod("show", "DataFrame",
    function(object)
    {
        if (class(object) == "DataFrame") {
            ## Aug 20, 2019: Too early for this warning.
            #warning(wmsg(.OLD_DATAFRAME_INSTANCE_MSG))
            object <- updateObject(object, check=FALSE)
        }
        .show_DataFrame(object)
    }
)

setMethod("showAsCell", "DataFrame", showAsCell_array)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

.format_mismatch_message <- function(x, y) {
    universe <- union(x, y)
    tx <- table(factor(x, levels=universe))
    ty <- table(factor(y, levels=universe))
    common <- pmin(tx, ty)

    leftx <- tx - common
    lefty <- ty - common
    misleft <- names(leftx)[leftx > 0]
    misright <- names(lefty)[lefty > 0]

    .format_failed_colnames <- function(x) {
        x <- sprintf("'%s'", x)
        if (length(x)>3) x <- c(head(x, 3), "...")
        paste(x, collapse=", ")
    }

    if (length(misleft) && length(misright)) {
        msg <- paste0("(", .format_failed_colnames(misleft),
            " vs ", .format_failed_colnames(misright), ")")
    } else if (length(misleft)) {
        msg <- paste0("(", .format_failed_colnames(misleft), " ",
            if (length(misleft) > 1) "are" else "is",
            " unique)")
    } else {
        msg <- paste0("(", .format_failed_colnames(misright), " ",
            if (length(misright) > 1) "are" else "is",
            " unique)")
    }

    stop(wmsg("the DataFrame objects to rbind do not have ",
              "the same column names ", msg))
}

### Return an integer matrix with 1 column per object in 'objects' and 1 row
### per column in 'x'.
.make_colmaps <- function(x, objects)
{
    x_colnames <- colnames(x)
    x_ncol <- length(x_colnames)
    map_x_colnames_to_object_colnames <- function(object_colnames) {
        if (length(object_colnames) != x_ncol) {
            .format_mismatch_message(x_colnames, object_colnames)
        }
        colmap <- selectHits(findMatches(x_colnames, object_colnames),
                             select="first", nodup=TRUE)
        if (anyNA(colmap)) {
            .format_mismatch_message(x_colnames, object_colnames)
        }
        colmap
    }
    if (length(objects) == 0L) {
        colmaps <- integer(0)
    } else {
        colmaps <- lapply(objects,
            function(object)
                map_x_colnames_to_object_colnames(colnames(object)))
        colmaps <- unlist(colmaps, use.names=FALSE)
    }
    dim(colmaps) <- c(x_ncol, length(objects))
    colmaps
}

### A thin wrapper around bindROWS()
###
### If all the columns to bind have the same type, the result of the binding
### should be a column of that type (endomorphism). Note that this is what
### happens with ordinary data frames. So we could just use bindROWS() for
### that. However, when the columns to bind have mixed types, bindROWS()
### won't necessarily do the right thing.
### The purpose of the wrapper below is to improve handling of mixed type
### columns by pre-processing some of them before calling bindROWS() on them.
### More precisely, it tries to work around the 2 following problems that
### direct use of bindROWS() on mixed type columns would pose:
###  1) When the columns to bind are a mix of Rle and non-Rle objects,
###     the type of the column returned by bindROWS() depends on the type
###     of cols[[1]]. More precisely it's an Rle if and only if cols[[1]]
###     is an Rle. The wrapper below **mitigate** this by decoding the Rle
###     columns first. Note that this is a mitigation process only. For
###     example it will help if Rle columns are mixed with atomic vectors
###     or factors, but it won't help if cols[[1]] is an Rle and the other
###     columns are IntegerList objects.
###  2) When the columns to bind are a mix of atomic vectors and factors,
###     bindROWS() would **always** return an atomic vector (whatever
###     cols[[1]] is, i.e. atomic vector or factor). However we **always**
###     want a factor. This is an intended deviation with respect to what
###     rbind() does on ordinary data frames where the 1st data frame
###     involved in the binding operation governs (i.e. a column in the
###     result will be atomic vector or factor depending on what the
###     corresponding column in the 1st data frame is).
.bind_cols_along_their_ROWS <- function(cols)
{
    is_Rle <- vapply(cols, is, logical(1L), "Rle")
    if (any(is_Rle) && !all(is_Rle))
        cols[is_Rle] <- lapply(cols[is_Rle], decodeRle)
    is_factor <- vapply(cols, is.factor, logical(1L))
    if (any(is_factor)) {
        cols <- lapply(cols, as.factor)
        all_levels <- unique(unlist(lapply(cols, levels), use.names=FALSE))
        cols <- lapply(cols, factor, all_levels)
    }
    bindROWS(cols[[1L]], cols[-1L])
}

### Ignore the 'ignore.mcols' argument!
.bindROWS_DataFrame_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    objects <- prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)
    has_rows <- vapply(all_objects, nrow, integer(1L), USE.NAMES=FALSE) > 0L
    has_cols <- vapply(all_objects, ncol, integer(1L), USE.NAMES=FALSE) > 0L
    if (!any(has_rows)) {
        if (!any(has_cols))
            return(x)
        return(all_objects[[which(has_cols)[[1L]]]])
    }
    all_objects <- all_objects[has_rows]

    ## From now on, all the objects to bind have rows.
    x <- all_objects[[1L]]
    objects <- all_objects[-1L]
    colmaps <- .make_colmaps(x, objects)
    if (ncol(x) == 0L) {
        ans_listData <- x@listData
        ans_nrow <- sum(unlist(lapply(all_objects, nrow), use.names=FALSE))
    } else {
        ans_listData <- lapply(setNames(seq_along(x), colnames(x)),
            function(i) {
                 x_col <- x[[i]]
                 other_cols <- lapply(seq_along(objects),
                                      function(j) objects[[j]][[colmaps[i, j]]])
                 tryCatch(
                     .bind_cols_along_their_ROWS(c(list(x_col), other_cols)),
                     error=function(err) {
                        stop("failed to rbind column '", colnames(x)[i],
                            "' across DataFrame objects:\n  ",
                            conditionMessage(err))
                     }
                 )
            }
        )
        ans_nrow <- NROW(ans_listData[[1L]])
    }
    if (use.names) {
        ## Bind the rownames.
        ans_rownames <- unlist(lapply(all_objects, rownames), use.names=FALSE)
        if (!is.null(ans_rownames)) {
            if (length(ans_rownames) != ans_nrow) {
                ## What we do here is surprising and inconsistent with
                ## ordinary data frames.
                ## TODO: Maybe reconsider this?
                ans_rownames <- NULL  # why?
            }
        }
    } else {
        ans_rownames <- NULL
    }
    BiocGenerics:::replaceSlots(x, listData=ans_listData,
                                   nrows=ans_nrow,
                                   rownames=ans_rownames,
                                   check=check)
}

### Defining bindROWS() gives us rbind().
setMethod("bindROWS", "DataFrame", .bindROWS_DataFrame_objects)

### S3/S4 combo for cbind.DataFrame
cbind.DataFrame <- function(..., deparse.level=1)
{
    if (!identical(deparse.level, 1))
        warning(wmsg("the cbind() method for DataFrame objects ",
                     "ignores the 'deparse.level' argument"))
    ## It's important that the call to DataFrame() below is able to deparse
    ## the arguments in ... so for example
    ##   b <- 11:13
    ##   selectMethod("cbind", "DataFrame")(b)
    ## returns a DataFrame with a column named "b".
    ## This prevents us from calling DataFrame() via do.call() e.g. we can't
    ## do something like
    ##   objects <- delete_NULLs(list(...))
    ##   do.call(DataFrame, c(objects, list(check.names=FALSE)))
    ## because then DataFrame() wouldn't be able to deparse what was in ...
    ## and selectMethod("cbind", "DataFrame")(b) would produce a DataFrame
    ## with a column named "11:13".
    DataFrame(..., check.names=FALSE)
}
setMethod("cbind", "DataFrame", cbind.DataFrame)

### If we didn't define this method, calling c() on DataFrame objects would
### call the "c" method for Vector objects, which just delegates to bindROWS()
### so the binding would happen along the rows. This is not what we want so we
### overwrite the "c" method for Vector objects with a method that binds along
### the columns.
setMethod("c", "DataFrame",
    function(x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!isTRUEorFALSE(ignore.mcols))
            stop("'ignore.mcols' must be TRUE or FALSE")
        if (!identical(recursive, FALSE))
            stop(wmsg("\"c\" method for DataFrame objects ",
                      "does not support the 'recursive' argument"))
        objects <- unname(delete_NULLs(list(x, ...)))
        ans <- do.call(cbind, objects)
        if (ignore.mcols)
            mcols(ans) <- NULL
        ans
    }
)

