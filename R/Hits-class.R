### =========================================================================
### Hits objects
### -------------------------------------------------------------------------
###
### The Hits class hierarchy (4 concrete classes):
###
###                  Hits    <----    SortedByQueryHits
###                   ^                      ^
###                   |                      |
###                SelfHits  <----  SortedByQuerySelfHits
###

### Vector of hits between a set of left nodes and a set of right nodes.
setClass("Hits",
    contains="Vector",
    representation(
        from="integer",    # integer vector of length N
        to="integer",      # integer vector of length N
        nLnode="integer",  # single integer: number of Lnodes ("left nodes")
        nRnode="integer"   # single integer: number of Rnodes ("right nodes")
    ),
    prototype(
        nLnode=0L,
        nRnode=0L
    )
)

### A SelfHits object is a Hits object where the left and right nodes are
### identical.
setClass("SelfHits", contains="Hits")

### Hits objects where the hits are sorted by query. Coercion from
### SortedByQueryHits to IntegerList takes advantage of this and is very fast.
setClass("SortedByQueryHits", contains="Hits")
setClass("SortedByQuerySelfHits", contains=c("SelfHits", "SortedByQueryHits"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallel_slot_names()
###

### Combine the new "parallel slots" with those of the parent class. Make
### sure to put the new parallel slots **first**. See Vector-class.R file
### for what slots should or should not be considered "parallel".
setMethod("parallel_slot_names", "Hits",
    function(x) c("from", "to", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("from", function(x, ...) standardGeneric("from"))
setMethod("from", "Hits", function(x) x@from)

setGeneric("to", function(x, ...) standardGeneric("to"))
setMethod("to", "Hits", function(x) x@to)

setGeneric("nLnode", function(x, ...) standardGeneric("nLnode"))
setMethod("nLnode", "Hits", function(x) x@nLnode)

setGeneric("nRnode", function(x, ...) standardGeneric("nRnode"))
setMethod("nRnode", "Hits", function(x) x@nRnode)

setGeneric("nnode", function(x, ...) standardGeneric("nnode"))
setMethod("nnode", "SelfHits", function(x) nLnode(x))

setGeneric("countLnodeHits", function(x, ...) standardGeneric("countLnodeHits"))

.count_Lnode_hits <- function(x) tabulate(from(x), nbins=nLnode(x))
setMethod("countLnodeHits", "Hits", .count_Lnode_hits)

setGeneric("countRnodeHits", function(x, ...) standardGeneric("countRnodeHits"))

.count_Rnode_hits <- function(x) tabulate(to(x), nbins=nRnode(x))
setMethod("countRnodeHits", "Hits", .count_Rnode_hits)

### query/subject API
queryHits <- function(x, ...) from(x, ...)
subjectHits <- function(x, ...) to(x, ...)
queryLength <- function(x, ...) nLnode(x, ...)
subjectLength <- function(x, ...) nRnode(x, ...)
countQueryHits <- function(x, ...) countLnodeHits(x, ...)
countSubjectHits <- function(x, ...) countRnodeHits(x, ...)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.Hits.nnode <- function(nnode, side)
{
    if (!isSingleInteger(nnode) || nnode < 0L) {
        msg <- wmsg("'n", side, "node(x)' must be a single non-negative ",
                    "integer")
        return(msg)
    }
    if (!is.null(attributes(nnode))) {
        msg <- wmsg("'n", side, "node(x)' must be a single integer with ",
                    "no attributes")
        return(msg)
    }
    NULL
}

.valid.Hits.from_or_to <- function(from_or_to, nnode, what, side)
{
    if (!(is.integer(from_or_to) && is.null(attributes(from_or_to)))) {
        msg <- wmsg("'", what, "' must be an integer vector ",
                    "with no attributes")
        return(msg)
    }
    if (anyMissingOrOutside(from_or_to, 1L, nnode)) {
        msg <- wmsg("'", what, "' must contain non-NA values ",
                    ">= 1 and <= 'n", side, "node(x)'")
        return(msg)
    }
    NULL
}

.valid.Hits <- function(x)
{
    c(.valid.Hits.nnode(nLnode(x), "L"),
      .valid.Hits.nnode(nRnode(x), "R"),
      .valid.Hits.from_or_to(from(x), nLnode(x), "from(x)", "L"),
      .valid.Hits.from_or_to(to(x), nRnode(x), "to(x)", "R"))
}

setValidity2("Hits", .valid.Hits)

.valid.SelfHits <- function(x)
{
    if (nLnode(x) != nRnode(x))
        return("'nLnode(x)' and 'nRnode(x)' must be equal")
    NULL
}

setValidity2("SelfHits", .valid.SelfHits)

.valid.SortedByQueryHits <- function(x)
{
    if (isNotSorted(from(x)))
        return("'queryHits(x)' must be sorted")
    NULL
}

setValidity2("SortedByQueryHits", .valid.SortedByQueryHits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

### Very low-level constructor. Doesn't try to sort the hits by query.
.new_Hits <- function(Class, from, to, nLnode, nRnode, mcols)
{
    new2(Class, from=from, to=to, nLnode=nLnode, nRnode=nRnode,
                elementMetadata=mcols,
                check=TRUE)
}

### Low-level constructor. Sort the hits by query if Class extends
### SortedByQueryHits.
new_Hits <- function(Class, from=integer(0), to=integer(0),
                            nLnode=0L, nRnode=0L,
                            mcols=NULL)
{
    if (!isSingleString(Class))
        stop("'Class' must be a single character string")
    if (!extends(Class, "Hits"))
        stop("'Class' must be the name of a class that extends Hits")

    if (!(is.numeric(from) && is.numeric(to)))
        stop("'from' and 'to' must be integer vectors")
    if (!is.integer(from))
        from <- as.integer(from)
    if (!is.integer(to))
        to <- as.integer(to)

    if (!(isSingleNumber(nLnode) && isSingleNumber(nRnode)))
        stop("'nLnode' and 'nRnode' must be single integers")
    if (!is.integer(nLnode))
        nLnode <- as.integer(nLnode)
    if (!is.integer(nRnode))
        nRnode <- as.integer(nRnode)

    mcols <- normarg_mcols(mcols, Class, length(from))

    if (!extends(Class, "SortedByQueryHits")) {
        ## No need to sort the hits by query.
        ans <- .new_Hits(Class, from, to, nLnode, nRnode, mcols)
        return(ans)
    }

    ## Sort the hits by query.
    if (!is.null(mcols)) {
        revmap_envir <- new.env(parent=emptyenv())
    } else {
        revmap_envir <- NULL
    }
    ans <- .Call2("Hits_new", Class, from, to, nLnode, nRnode, revmap_envir,
                              PACKAGE="S4Vectors")
    if (!is.null(mcols)) {
        if (exists("revmap", envir=revmap_envir)) {
            revmap <- get("revmap", envir=revmap_envir)
            mcols <- extractROWS(mcols, revmap)
        }
        mcols(ans) <- mcols
    }
    ans
}

.make_mcols <- function(...)
{
    if (nargs() == 0L)
        return(NULL)
    ## We use 'DataFrame(..., check.names=FALSE)' rather than
    ## 'new_DataFrame(list(...))' because we want to make use of the
    ## former's ability to deparse unnamed arguments to generate column
    ## names for them. Unfortunately this means that the user won't be
    ## able to pass metadata columns named "row.names" or "check.names"
    ## because things like '.make_mcols(11:13, row.names=21:23)'
    ## or '.make_mcols(11:13, check.names=21:23)' won't work as expected.
    ## The solution would be to have a mid-level DataFrame constructor
    ## that has no extra arguments after the ellipsis and implements the
    ## same deparsing mechanism as DataFrame(), and to use it here.
    DataFrame(..., check.names=FALSE)
}

### 2 high-level constructors.

Hits <- function(from=integer(0), to=integer(0), nLnode=0L, nRnode=0L, ...,
                 sort.by.query=FALSE)
{
    if (!isTRUEorFALSE(sort.by.query))
        stop("'sort.by.query' must be TRUE or FALSE")
    Class <- if (sort.by.query) "SortedByQueryHits" else "Hits"
    mcols <- .make_mcols(...)
    new_Hits(Class, from, to, nLnode, nRnode, mcols)
}

SelfHits <- function(from=integer(0), to=integer(0), nnode=0L, ...,
                     sort.by.query=FALSE)
{
    if (!isTRUEorFALSE(sort.by.query))
        stop("'sort.by.query' must be TRUE or FALSE")
    Class <- if (sort.by.query) "SortedByQuerySelfHits" else "SelfHits"
    mcols <- .make_mcols(...)
    new_Hits(Class, from, to, nnode, nnode, mcols)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Conversion from old to new internal representation
###

setMethod("updateObject", "Hits",
    function(object, ..., verbose=FALSE)
    {
        if (!is(try(object@queryHits, silent=TRUE), "try-error")) {
            object_metadata <- object@metadata
            object <- new_Hits("SortedByQueryHits", object@queryHits,
                                                    object@subjectHits,
                                                    object@queryLength,
                                                    object@subjectLength,
                                                    object@elementMetadata)
            object@metadata <- object_metadata
        }

        callNextMethod()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### --- Coercion within the Hits class hierarchy ---

### There are 4 classes in the Hits class hierarchy. We want to support back
### and forth coercion between all of them. That's 12 possible coercions.
### They can be devided in 3 groups:
###   - Group A: 5 demotions
###   - Group B: 5 promotions
###   - Group C: 2 transversal coercions (from SelfHits to SortedByQueryHits
###              and vice-versa)
###
### Group A: Demotions are taken care of by the "automatic coercion methods".
### (These methods that get automatically defined at run time by the methods
### package the 1st time a given demotion is requested e.g. when doing
### as(x, "Hits") where 'x' is any Hits derivative.)
###
### Group B: The methods package also defines automatic coercion methods for
### promotions. Unfortunately, these methods almost never get it right. In
### particular, a serious problem with these automatic promotion methods is
### that they don't even try to validate the promoted object so they tend to
### silently produce invalid objects. This means that we need to define
### methods for all the coercions in group B.
###
### Group C: Note that coercions from SelfHits to SortedByQueryHits and
### vice-versa will actually be taken care of by the coercion methods from
### Hits to SortedByQueryHits and from Hits to SelfHits, respectively (both
### defined in group B).
###
### So the good news is that we only need to define coercion methods for
### group B.

.from_Hits_to_SelfHits <- function(from, to)
{
    if (nLnode(from) != nRnode(from))
        stop(wmsg(class(from), " object to coerce to ", to,
                  " must satisfy 'nLnode(x) == nRnode(x)'"))
    class(from) <- class(new(to))
    from
}
setAs("Hits", "SelfHits", .from_Hits_to_SelfHits)
setAs("SortedByQueryHits", "SortedByQuerySelfHits", .from_Hits_to_SelfHits)

### Note that the 'from' and 'to' arguments below are the standard arguments
### for coercion methods. They should not be confused with the 'from()'
### and 'to()' accessors for Hits objects!
.from_Hits_to_SortedByQueryHits <- function(from, to)
{
    new_Hits(to, from(from), to(from), nLnode(from), nRnode(from),
                 mcols(from, use.names=FALSE))
}
setAs("Hits", "SortedByQueryHits", .from_Hits_to_SortedByQueryHits)
setAs("SelfHits", "SortedByQuerySelfHits", .from_Hits_to_SortedByQueryHits)

### 2 possible routes for this coercion:
###   1. Hits -> SelfHits -> SortedByQuerySelfHits
###   2. Hits -> SortedByQueryHits -> SortedByQuerySelfHits
### They are equivalent. However, the 1st route will fail early rather
### than after a possibly long and expensive coercion from Hits to
### SortedByQueryHits.
setAs("Hits", "SortedByQuerySelfHits",
    function(from) as(as(from, "SelfHits"), "SortedByQuerySelfHits")
)

### --- Other coercions ---

setMethod("as.matrix", "Hits",
    function(x)
    {
        ans <- cbind(from=from(x), to=to(x))
        if (is(x, "SortedByQueryHits"))
            colnames(ans) <- c("queryHits", "subjectHits")
        ans
    }
)

setMethod("as.table", "Hits", .count_Lnode_hits)

### FIXME: Coercions of Vector derivatives to DFrame are inconsistent.
### For some Vector derivatives (e.g. IRanges, GRanges) the object is stored
### "as is" in the 1st column of the returned DFrame, whereas for others (e.g.
### Hits below) the object is "dismantled" into various parallel components
### that end up in separate columns of the returned DFrame.
setAs("Hits", "DFrame",
    function(from)
    {
        from_mcols <- mcols(from, use.names=FALSE)
        if (is.null(from_mcols))
            from_mcols <- make_zero_col_DFrame(length(from))
        DataFrame(as.matrix(from), from_mcols, check.names=FALSE)
    }
)

### S3/S4 combo for as.data.frame.Hits
as.data.frame.Hits <- function(x, row.names=NULL, optional=FALSE, ...)
{
    x <- as(x, "DFrame")
    as.data.frame(x, row.names=row.names, optional=optional, ...)
}
setMethod("as.data.frame", "Hits", as.data.frame.Hits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### The "extractROWS" method for Vector objects doesn't test the validity of
### the result so we override it.
setMethod("extractROWS", "SortedByQueryHits",
    function(x, i)
    {
        ans <- callNextMethod()
        pbs <- validObject(ans, test=TRUE)
        if (is.character(pbs))
            stop(wmsg("Problem(s) found when testing validity of ", class(ans),
                      " object returned by subsetting operation: ",
                      paste0(pbs, collapse=", "), ". Make sure to use a ",
                      "subscript that results in a valid ", class(ans),
                      " object."))
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("classNameForDisplay", "SortedByQueryHits",
    function(x) sub("^SortedByQuery", "", class(x))
)

.Hits_summary <- function(object)
{
    object_len <- length(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object with ", object_len, " ",
           ifelse(object_len == 1L, "hit", "hits"),
           " and ", object_nmc, " metadata ",
           ifelse(object_nmc == 1L, "column", "columns"))
}
### S3/S4 combo for summary.Hits
summary.Hits <- function(object, ...)
    .Hits_summary(object, ...)
setMethod("summary", "Hits", summary.Hits)

.from_Hits_to_naked_character_matrix_for_display <- function(x)
{
    m <- cbind(from=showAsCell(from(x)),
               to=showAsCell(to(x)))
    if (is(x, "SortedByQueryHits"))
        colnames(m) <- c("queryHits", "subjectHits")
    cbind_mcols_for_display(m, x)
}
setMethod("makeNakedCharacterMatrixForDisplay", "Hits",
    .from_Hits_to_naked_character_matrix_for_display
)

.show_Hits <- function(x, margin="", print.classinfo=FALSE,
                                     print.nnode=FALSE)
{
    cat(margin, summary(x), ":\n", sep="")
    ## makePrettyMatrixForCompactPrinting() assumes that head() and tail()
    ## work on 'x'.
    out <- makePrettyMatrixForCompactPrinting(x)
    if (print.classinfo) {
        .COL2CLASS <- c(
            from="integer",
            to="integer"
        )
        if (is(x, "SortedByQueryHits"))
            names(.COL2CLASS) <- c("queryHits", "subjectHits")
        classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L)
        rownames(out) <- paste0(margin, "  ", rownames(out))
    ## We set 'max' to 'length(out)' to avoid the getOption("max.print")
    ## limit that would typically be reached when 'showHeadLines' global
    ## option is set to Inf.
    print(out, quote=FALSE, right=TRUE, max=length(out))
    if (print.nnode) {
        cat(margin, "  -------\n", sep="")
        if (is(x, "SortedByQueryHits")) {
            cat(margin, "  queryLength: ", nLnode(x),
                " / subjectLength: ", nRnode(x), "\n", sep="")
        } else {
            if (is(x, "SelfHits")) {
                cat(margin, "  nnode: ", nnode(x), "\n", sep="")
            } else {
                cat(margin, "  nLnode: ", nLnode(x),
                    " / nRnode: ", nRnode(x), "\n", sep="")
            }
        }
    }
}

setMethod("show", "Hits",
    function(object)
        .show_Hits(object, print.classinfo=TRUE, print.nnode=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.check_that_Hits_objects_are_concatenable <- function(x, objects)
{
    objects_nLnode <- vapply(objects, slot, integer(1), "nLnode",
                             USE.NAMES=FALSE)
    objects_nRnode <- vapply(objects, slot, integer(1), "nRnode",
                             USE.NAMES=FALSE)
    if (!(all(objects_nLnode == x@nLnode) &&
          all(objects_nRnode == x@nRnode)))
        stop(wmsg("the objects to concatenate are incompatible Hits ",
                  "objects by number of left and/or right nodes"))
}

.bindROWS_Hits_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- prepare_objects_to_bind(x, objects)
    .check_that_Hits_objects_are_concatenable(x, objects)
    callNextMethod()
}

setMethod("bindROWS", "Hits", .bindROWS_Hits_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Sorting
###

setMethod("sort", "SortedByQueryHits",
          function(x, decreasing = FALSE, na.last = NA, by) {
    byQueryHits <- missing(by) || is(by, "formula") &&
        all.vars(by)[1L] == "queryHits" && !decreasing
    if (!byQueryHits)
        x <- as(x, "Hits")
    callNextMethod()
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selectHits()
###

### Return an integer vector parallel to the query (i.e. of length
### 'nLnode(hits)') except when select="all", in which case it's a no-op.
###
### 'nodup' must be TRUE or FALSE (the default) and can only be set to TRUE
### when 'select' is "first", "last" or "arbitrary", and when the input hits
### are sorted by query. When 'nodup=TRUE', a given element in the subject is
### not allowed to be assigned to more than one element in the query, which is
### achieved by following a simple "first come first served" pairing strategy.
### So the returned vector is guaranteed to contain unique non-NA values.
### Note that such vector represents a mapping between the query and subject
### that is one-to-zero-or-one in *both* directions. So it represents a
### pairing between the elements in query and subject, where a given element
### belongs to at most one pair.
### A note about the "first come first served" pairing strategy: This strategy
### is simple and fast, but, in general, it won't achieve a "maximal pairing"
### (i.e. a pairing with the most possible number of pairs) for a given input
### Hits object. However it actually does produce a maximal pairing if the
### Hits object is the result of call to findMatches() (with select="all")'.
### Also, in that case, this pairing strategy is symetric i.e. the resulting
### pairs are not affected by switching 'x' and 'table' in the call to
### findMatches() (or by transposing the input Hits object).
###
### Finally note that when 'select' is "first" or "last" and 'nodup' is FALSE,
### or when 'select' is "count", the output of selectHits() is not affected
### by the order of the hits in the input Hits object.
selectHits <- function(hits,
    select=c("all", "first", "last", "arbitrary", "count"),
    nodup=FALSE,
    rank)
{
    if (!is(hits, "Hits"))
        stop("'hits' must be a Hits object")
    select <- match.arg(select)
    if (!isTRUEorFALSE(nodup))
        stop(wmsg("'nodup' must be TRUE or FALSE"))
    if (nodup && !(select %in% c("first", "last", "arbitrary")))
        stop(wmsg("'nodup=TRUE' is only supported when 'select' ",
                  "is \"first\", \"last\", or \"arbitrary\""))
    if (!missing(rank) && (!(select %in% c("first", "last")) || nodup))
        stop(wmsg("'rank' is only supported when 'select' ",
                  "is \"first\" or \"last\" and 'nodup' is FALSE"))
    if (select == "all")
        return(hits)  # no-op

    hits_from <- from(hits)
    hits_to <- to(hits)
    hits_nLnode <- nLnode(hits)
    hits_nRnode <- nRnode(hits)

    if (!missing(rank)) {
        r <- rank(hits, ties.method="first", by=rank)
        revmap <- integer()
        revmap[r] <- hits_to
        hits_to <- r
    }
    ans <- .Call2("select_hits", hits_from, hits_to, hits_nLnode, hits_nRnode,
                                 select, nodup,
                                 PACKAGE="S4Vectors")
    if (!missing(rank))
        ans <- revmap[ans]
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### breakTies()
###
### Redundant with selectHits. The only difference is that it returns a Hits
### object. That alone doesn't justify introducing a new verb. Should be
### controlled via an extra arg to selectHits() e.g. 'as.Hits' (FALSE by
### default). H.P. -- Oct 16, 2016

breakTies <- function(x, method=c("first", "last"), rank) {
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    method <- match.arg(method)
    to <- selectHits(x, method, rank=rank)
    .new_Hits("SortedByQueryHits", which(!is.na(to)), to[!is.na(to)],
              nLnode(x), nRnode(x), NULL)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### revmap()
###

### NOT exported (but used in IRanges).
### TODO: Move revmap() generic from AnnotationDbi to S4Vectors. Then split
### the code below in 2 revmap() methods: one for SortedByQueryHits objects
### and one for Hits objects.
revmap_Hits <- function(x)
{
    if (is(x, "SortedByQueryHits")) {
        ## Note that:
        ## - If 'x' is a valid SortedByQueryHits object (i.e. the hits in it
        ##   are sorted by query), then 'revmap_Hits(x)' returns a
        ##   SortedByQueryHits object where hits are "fully sorted" i.e.
        ##   sorted by query first and then by subject.
        ## - Because revmap_Hits() reorders the hits by query, doing
        ##   'revmap_Hits(revmap_Hits(x))' brings back 'x' but with the hits
        ##   in it now "fully sorted".
        return(new_Hits(class(x), to(x), from(x), nRnode(x), nLnode(x),
                                  mcols(x, use.names=FALSE)))
    }
    BiocGenerics:::replaceSlots(x, from=to(x), to=from(x),
                                   nLnode=nRnode(x), nRnode=nLnode(x),
                                   check=FALSE)
}

### FIXME: Replace this with "revmap" method for Hits objects.
t.Hits <- function(x) t(x)
setMethod("t", "Hits", revmap_Hits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Remap the left and/or right nodes of a Hits object.
###

### Returns 'arg' as a NULL, an integer vector, or a factor.
.normarg_nodes.remapping <- function(arg, side, old.nnode)
{
    if (is.null(arg))
        return(arg)
    if (!is.factor(arg)) {
        if (!is.numeric(arg))
            stop("'" , side, "nodes.remappping' must be a vector ",
                 "of integers")
        if (!is.integer(arg))
            arg <- as.integer(arg)
    }
    if (length(arg) != old.nnode)
        stop("'" , side, "nodes.remapping' must be of length 'n",
             side, "node(x)'")
    arg
}

.normarg_new.nnode <- function(arg, side, map)
{
    if (!isSingleNumberOrNA(arg))
        stop("'new.n", side, "node' must be a single number or NA")
    if (!is.integer(arg))
        arg <- as.integer(arg)
    if (is.null(map))
        return(arg)
    if (is.factor(map)) {
        if (is.na(arg))
            return(nlevels(map))
        if (arg < nlevels(map))
            stop("supplied 'new.n", side, "node' must ",
                 "be >= 'nlevels(", side, "nodes.remapping)'")
        return(arg)
    }
    if (is.na(arg))
        stop("'new.n", side, "node' must be specified when ",
             "'" , side, "s.remapping' is specified and is not a factor")
    arg
}

remapHits <- function(x, Lnodes.remapping=NULL, new.nLnode=NA,
                         Rnodes.remapping=NULL, new.nRnode=NA,
                         with.counts=FALSE)
{
    if (!is(x, "SortedByQueryHits"))
        stop("'x' must be a SortedByQueryHits object")
    Lnodes.remapping <- .normarg_nodes.remapping(Lnodes.remapping, "L",
                                                 nLnode(x))
    new.nLnode <- .normarg_new.nnode(new.nLnode, "L", Lnodes.remapping)
    Rnodes.remapping <- .normarg_nodes.remapping(Rnodes.remapping, "R",
                                                 nRnode(x))
    new.nRnode <- .normarg_new.nnode(new.nRnode, "R", Rnodes.remapping)
    if (!isTRUEorFALSE(with.counts))
        stop("'with.counts' must be TRUE or FALSE")
    x_from <- from(x)
    if (is.null(Lnodes.remapping)) {
        if (is.na(new.nLnode))
            new.nLnode <- nLnode(x)
    } else {
        if (is.factor(Lnodes.remapping))
            Lnodes.remapping <- as.integer(Lnodes.remapping)
        if (anyMissingOrOutside(Lnodes.remapping, 1L, new.nLnode))
            stop(wmsg("'Lnodes.remapping' cannot contain NAs, or values that ",
                      "are < 1, or > 'new.nLnode'"))
        x_from <- Lnodes.remapping[x_from]
    }
    x_to <- to(x)
    if (is.null(Rnodes.remapping)) {
        if (is.na(new.nRnode))
            new.nRnode <- nRnode(x)
    } else {
        if (is.factor(Rnodes.remapping))
            Rnodes.remapping <- as.integer(Rnodes.remapping)
        if (anyMissingOrOutside(Rnodes.remapping, 1L, new.nRnode))
            stop(wmsg("'Rnodes.remapping' cannot contain NAs, or values that ",
                      "are < 1, or > 'new.nRnode'"))
        x_to <- Rnodes.remapping[x_to]
    }
    x_mcols <- mcols(x, use.names=FALSE)
    add_counts <- function(counts) {
        if (is.null(x_mcols))
            return(DataFrame(counts=counts))
        if ("counts" %in% colnames(x_mcols))
            warning("'x' has a \"counts\" metadata column, replacing it")
        x_mcols$counts <- counts
        x_mcols
    }
    if (is.null(Lnodes.remapping) && is.null(Rnodes.remapping)) {
        if (with.counts) {
            counts <- rep.int(1L, length(x))
            x_mcols <- add_counts(counts)
        }
    } else {
        sm <- selfmatchIntegerPairs(x_from, x_to)
        if (with.counts) {
            counts <- tabulate(sm, nbins=length(sm))
            x_mcols <- add_counts(counts)
            keep_idx <- which(counts != 0L)
        } else {
            keep_idx <- which(sm == seq_along(sm))
        }
        x_from <- x_from[keep_idx]
        x_to <- x_to[keep_idx]
        x_mcols <- extractROWS(x_mcols, keep_idx)
    }
    new_Hits(class(x), x_from, x_to, new.nLnode, new.nRnode, x_mcols)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SelfHits methods
###
### TODO: Make isSelfHit() and isRedundantHit() generic functions with
### methods for SelfHits objects.
###

### A "self hit" is an edge from a node to itself. For example, the 2nd hit
### in the SelfHits object below is a self hit (from 3rd node to itself):
###     SelfHits(c(3, 3, 3, 4, 4), c(2:4, 2:3), 4)
isSelfHit <- function(x)
{
    if (!is(x, "SelfHits"))
        stop("'x' must be a SelfHits object")
    from(x) == to(x)
}

### When there is more than 1 edge between 2 given nodes (regardless of
### orientation), the extra edges are considered to be "redundant hits". For
### example, hits 3, 5, 7, and 8, in the SelfHits object below are redundant
### hits:
###     SelftHits(c(3, 3, 3, 3, 3, 4, 4, 4), c(3, 2:4, 2, 2:3, 2), 4, 4)
### Note that this is regardless of the orientation of the edge so hit 7 (edge
### 4-3) is considered to be redundant with hit 4 (edge 3-4).
isRedundantHit <- function(x)
{
    if (!is(x, "SelfHits"))
        stop("'x' must be a SelfHits object")
    duplicatedIntegerPairs(pmin.int(from(x), to(x)),
                           pmax.int(from(x), to(x)))
}

### Specialized constructor.
### Return a SortedByQuerySelfHits object.
### About 10x faster and uses 4x less memory than my first attempt in pure
### R below.
### NOT exported.
makeAllGroupInnerHits <- function(group.sizes, hit.type=0L)
{
    if (!is.integer(group.sizes))
        stop("'group.sizes' must be an integer vector")
    if (!isSingleNumber(hit.type))
        stop("'hit.type' must be a single integer")
    if (!is.integer(hit.type))
        hit.type <- as.integer(hit.type)
    .Call2("make_all_group_inner_hits", group.sizes, hit.type,
           PACKAGE="S4Vectors")
}

### Return a SortedByQuerySelfHits object.
### NOT exported.
### TODO: Remove this.
makeAllGroupInnerHits.old <- function(GS)
{
    NG <- length(GS)  # nb of groups
    ## First Element In group i.e. first elt associated with each group.
    FEIG <- cumsum(c(1L, GS[-NG]))
    GSr <- c(0L, GS[-NG])
    CGSr2 <- cumsum(GSr * GSr)
    GS2 <- GS * GS
    nnode <- sum(GS)  # length of original vector (i.e. before grouping)

    ## Original Group Size Assignment i.e. group size associated with each
    ## element in the original vector.
    OGSA <- rep.int(GS, GS)  # is of length 'nnode'
    ans_from <- rep.int(seq_len(nnode), OGSA)
    NH <- length(ans_from)  # same as sum(GS2)

    ## Hit Group Assignment i.e. group associated with each hit.
    HGA <- rep.int(seq_len(NG), GS2)
    ## Hit Group Size Assignment i.e. group size associated with each hit.
    HGSA <- GS[HGA]
    ans_to <- (0:(NH-1L) - CGSr2[HGA]) %% GS[HGA] + FEIG[HGA]
    SelfHits(ans_from, ans_to, nnode, sort.by.query=TRUE)
}

