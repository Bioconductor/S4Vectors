### =========================================================================
### Hits objects
### -------------------------------------------------------------------------
###


setClass("Hits",
    contains="Vector",
    representation(
        queryHits="integer",     # integer vector of length N
        subjectHits="integer",   # integer vector of length N
        queryLength="integer",   # single integer
        subjectLength="integer"  # single integer
    ),
    prototype(
        queryLength=0L,
        subjectLength=0L
    )
)

### Hits objects where the hits are sorted by query. Coercion from
### SortedByQueryHits to List takes advantage of this and is very fast.
setClass("SortedByQueryHits", contains="Hits")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "Hits",
    function(x) c("queryHits", "subjectHits", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("queryHits", function(x, ...) standardGeneric("queryHits"))

setMethod("queryHits", "Hits", function(x) x@queryHits)

setGeneric("subjectHits", function(x, ...) standardGeneric("subjectHits"))

setMethod("subjectHits", "Hits", function(x) x@subjectHits)

setGeneric("queryLength", function(x, ...) standardGeneric("queryLength"))

setMethod("queryLength", "Hits", function(x) x@queryLength)

setGeneric("subjectLength", function(x, ...) standardGeneric("subjectLength"))

setMethod("subjectLength", "Hits", function(x) x@subjectLength)

setGeneric("countQueryHits",
    function(x, ...) standardGeneric("countQueryHits")
)

.count_query_hits <- function(x)
    tabulate(queryHits(x), nbins=queryLength(x))

setMethod("countQueryHits", "Hits", .count_query_hits)

setGeneric("countSubjectHits",
    function(x, ...) standardGeneric("countSubjectHits")
)

.count_subject_hits <- function(x)
    tabulate(subjectHits(x), nbins=subjectLength(x))

setMethod("countSubjectHits", "Hits", .count_subject_hits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.Hits.queryLength_or_subjectLength <- function(q_len, what)
{
    if (!isSingleInteger(q_len) || q_len < 0L) {
        msg <- wmsg("'", what, "Length(x)' must be a single non-negative ",
                    "integer")
        return(msg)
    }
    if (!is.null(attributes(q_len))) {
        msg <- wmsg("'", what, "Length(x)' must be a single integer ",
                    "with no attributes")
        return(msg)
    }
    NULL
}

.valid.Hits.queryHits_or_subjectHits <- function(q_hits, q_len, what)
{
    if (!(is.integer(q_hits) && is.null(attributes(q_hits)))) {
        msg <- wmsg("'", what, "Hits(x)' must be an integer vector ",
                    "with no attributes")
        return(msg)
    }
    if (anyMissingOrOutside(q_hits, 1L, q_len)) {
        msg <- wmsg("'", what, "Hits(x)' must contain non-NA values ",
                    ">= 1 and <= '", what, "Length(x)'")
        return(msg)
    }
    NULL
}

.valid.Hits <- function(x)
{
    q_len <- queryLength(x)
    s_len <- subjectLength(x)
    c(.valid.Hits.queryLength_or_subjectLength(q_len, "query"),
      .valid.Hits.queryLength_or_subjectLength(s_len, "subject"),
      .valid.Hits.queryHits_or_subjectHits(queryHits(x), q_len, "query"),
      .valid.Hits.queryHits_or_subjectHits(subjectHits(x), s_len, "subject"))
}

setValidity2("Hits", .valid.Hits)

.valid.SortedByQueryHits <- function(x)
{
    if (isNotSorted(queryHits(x)))
        return("'queryHits(x)' must be sorted")
    NULL
}

setValidity2("SortedByQueryHits", .valid.SortedByQueryHits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Very low-level constructor. Doesn't try to sort the hits by query.
.new_Hits <- function(Class, queryHits, subjectHits,
                             queryLength, subjectLength,
                             mcols)
{
    new2(Class, queryHits=queryHits,
                subjectHits=subjectHits,
                queryLength=queryLength,
                subjectLength=subjectLength,
                elementMetadata=mcols,
                check=TRUE)
}

### Low-level constructor. Sort the hits by query if Class extends
### SortedByQueryHits.
new_Hits <- function(Class,
                     queryHits=integer(0), subjectHits=integer(0),
                     queryLength=0L, subjectLength=0L,
                     mcols=NULL)
{
    if (!isSingleString(Class))
        stop("'Class' must be a single character string")
    if (!extends(Class, "Hits"))
        stop("'Class' must be the name of a class that extends Hits")

    if (!(is.numeric(queryHits) && is.numeric(subjectHits)))
        stop("'queryHits' and 'subjectHits' must be integer vectors")
    if (!is.integer(queryHits))
        queryHits <- as.integer(queryHits)
    if (!is.integer(subjectHits))
        subjectHits <- as.integer(subjectHits)

    if (!(isSingleNumber(queryLength) && isSingleNumber(subjectLength)))
        stop("'queryLength' and 'subjectLength' must be single integers")
    if (!is.integer(queryLength))
        queryLength <- as.integer(queryLength)
    if (!is.integer(subjectLength))
        subjectLength <- as.integer(subjectLength)

    if (!(is.null(mcols) || is(mcols, "DataFrame")))
        stop("'mcols' must be NULL or a DataFrame object")

    if (!extends(Class, "SortedByQueryHits")) {
        ## No need to sort the hits by query.
        ans <- .new_Hits(Class, queryHits, subjectHits,
                                queryLength, subjectLength,
                                mcols)
        return(ans)
    }

    ## Sort the hits by query.
    if (!is.null(mcols)) {
        revmap_envir <- new.env(parent=emptyenv())
    } else {
        revmap_envir <- NULL
    }
    ans <- .Call2("Hits_new", Class,
                              queryHits, subjectHits,
                              queryLength, subjectLength,
                              revmap_envir,
                              PACKAGE="S4Vectors")
    if (!is.null(mcols)) {
        if (nrow(mcols) != length(ans))
            stop("length of supplied metadata columns ",
                 "must equal number of hits")
        if (exists("revmap", envir=revmap_envir)) {
            revmap <- get("revmap", envir=revmap_envir)
            mcols <- mcols[revmap, , drop=FALSE]
        }
        mcols(ans) <- mcols
    }
    ans
}

### High-level constructor.
### This constructor currently returns a SortedByQueryHits instance by
### default.
### TODO: Change the default for 'sort.by.query' from TRUE to FALSE.
Hits <- function(queryHits=integer(0), subjectHits=integer(0),
                 queryLength=0L, subjectLength=0L,
                 ..., sort.by.query=TRUE)
{
    if (!isTRUEorFALSE(sort.by.query))
        stop("'sort.by.query' must be TRUE or FALSE")
    if (sort.by.query) {
        Class <- "SortedByQueryHits"
    } else {
        Class <- "Hits"
    }
    if (length(list(...)) == 0L) {
        mcols <- NULL
    } else {
        mcols <- DataFrame(..., check.names=FALSE)
    }
    new_Hits(Class, queryHits, subjectHits, queryLength, subjectLength, mcols)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

.from_Hits_to_SortedByQueryHits <- function(from)
{
    new_Hits("SortedByQueryHits", queryHits(from), subjectHits(from),
                                  queryLength(from), subjectLength(from),
                                  mcols(from))
}

    
setAs("Hits", "SortedByQueryHits", .from_Hits_to_SortedByQueryHits)

setMethod("as.matrix", "Hits",
    function(x) cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
)

setMethod("as.table", "Hits", .count_query_hits)


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
### Displaying
###

setMethod("classNameForDisplay", "Hits", function(x) "Hits")

.makeNakedMatFromHits <- function(x)
{
    x_len <- length(x)
    x_mcols <- mcols(x)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    ans <- cbind(queryHits=as.character(queryHits(x)),
                 subjectHits=as.character(subjectHits(x)))
    if (x_nmc > 0L) {
        tmp <- do.call(data.frame, c(lapply(x_mcols, showAsCell),
                                     list(check.names=FALSE)))
        ans <- cbind(ans, `|`=rep.int("|", x_len), as.matrix(tmp))
    }
    ans
}

showHits <- function(x, margin="", print.classinfo=FALSE,
                                   print.qslengths=FALSE)
{
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(classNameForDisplay(x), " object with ",
        x_len, " hit", ifelse(x_len == 1L, "", "s"),
        " and ",
        x_nmc, " metadata column", ifelse(x_nmc == 1L, "", "s"),
        ":\n", sep="")
    out <- makePrettyMatrixForCompactPrinting(x, .makeNakedMatFromHits)
    if (print.classinfo) {
        .COL2CLASS <- c(
            queryHits="integer",
            subjectHits="integer"
        )
        classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L)
        rownames(out) <- paste0(margin, rownames(out))
    ## We set 'max' to 'length(out)' to avoid the getOption("max.print")
    ## limit that would typically be reached when 'showHeadLines' global
    ## option is set to Inf.
    print(out, quote=FALSE, right=TRUE, max=length(out))
    if (print.qslengths) {
        cat(margin, "-------\n", sep="")
        cat(margin, "queryLength: ", queryLength(x), "\n", sep="")
        cat(margin, "subjectLength: ", subjectLength(x), "\n", sep="")
    }
}

setMethod("show", "Hits",
    function(object)
        showHits(object, margin="  ", print.classinfo=TRUE,
                                      print.qslengths=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###
### Note that supporting "extractROWS" and "c" makes "replaceROWS" (and thus
### "[<-") work out-of-the-box!
###

### 'Class' must be "Hits" or the name of a concrete subclass of Hits.
### 'objects' must be a list of Hits objects.
### Returns an instance of class 'Class'.
combine_Hits_objects <- function(Class, objects,
                                 use.names=TRUE, ignore.mcols=FALSE)
{
    if (!isSingleString(Class))
        stop("'Class' must be a single character string")
    if (!extends(Class, "Hits"))
        stop("'Class' must be the name of a class that extends Hits")
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ### TODO: Support 'use.names=TRUE'.
    if (use.names)
        stop("'use.names=TRUE' is not supported yet")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")

    if (length(objects) != 0L) {
        ## TODO: Implement (in C) fast 'elementIsNull(objects)' in S4Vectors
        ## that does 'sapply(objects, is.null, USE.NAMES=FALSE)', and use it
        ## here.
        null_idx <- which(sapply(objects, is.null, USE.NAMES=FALSE))
        if (length(null_idx) != 0L)
            objects <- objects[-null_idx]
    }
    if (length(objects) == 0L)
        return(new(Class))

    ## TODO: Implement (in C) fast 'elementIs(objects, class)' in S4Vectors
    ## that does 'sapply(objects, is, class, USE.NAMES=FALSE)', and use it
    ## here. 'elementIs(objects, "NULL")' should work and be equivalent to
    ## 'elementIsNull(objects)'.
    if (!all(sapply(objects, is, Class, USE.NAMES=FALSE)))
        stop("the objects to combine must be ", Class, " objects (or NULLs)")
    objects_names <- names(objects)
    names(objects) <- NULL  # so lapply(objects, ...) below returns an
                            # unnamed list

    ## Combine "queryLength" slots.
    queryLength_slots <- lapply(objects, function(x) x@queryLength)
    ans_queryLength <- unlist(queryLength_slots, use.names=FALSE)

    ## Combine "subjectLength" slots.
    subjectLength_slots <- lapply(objects, function(x) x@subjectLength)
    ans_subjectLength <- unlist(subjectLength_slots, use.names=FALSE)

    if (!(all(ans_queryLength == ans_queryLength[[1L]]) &&
          all(ans_subjectLength == ans_subjectLength[[1L]])))
        stop(wmsg("the objects to combine are incompatible Hits objects ",
                  "by query and/or subject length"))
    ans_queryLength <- ans_queryLength[[1L]]
    ans_subjectLength <- ans_subjectLength[[1L]]

    ## Combine "queryHits" slots.
    queryHits_slots <- lapply(objects, function(x) x@queryHits)
    ans_queryHits <- unlist(queryHits_slots, use.names=FALSE)

    ## Combine "subjectHits" slots.
    subjectHits_slots <- lapply(objects, function(x) x@subjectHits)
    ans_subjectHits <- unlist(subjectHits_slots, use.names=FALSE)

    ## Combine "mcols" slots.
    if (ignore.mcols) {
        ans_mcols <- NULL
    } else {
        ans_mcols <- do.call(S4Vectors:::rbind_mcols, objects)
    }

    ## Make 'ans' and return it.
    .new_Hits(Class, ans_queryHits, ans_subjectHits,
                     ans_queryLength, ans_subjectLength,
                     ans_mcols)
}

setMethod("c", "Hits",
    function (x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for Hits objects ",
                 "does not support the 'recursive' argument")
        if (missing(x)) {
            objects <- list(...)
            x <- objects[[1L]]
        } else {
            objects <- list(x, ...)
        }
        combine_Hits_objects(class(x), objects,
                             use.names=FALSE,
                             ignore.mcols=ignore.mcols)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selectHits()
###

selectHits <- function(x, select=c("all", "first", "last", "arbitrary",
                                   "count"))
{
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    select <- match.arg(select)
    if (select == "all")
        return(x)
    .Call2("select_hits",
           queryHits(x), subjectHits(x), queryLength(x), select,
           PACKAGE="S4Vectors")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### revmap()
###

### NOT exported (but used in IRanges).
### TODO: Move revmap() generic from AnnotationDbi to S4Vectors, and make this
### the "revmap" method for SortedByQueryHits objects.
### Note that:
###   - If 'x' is a valid SortedByQueryHits object (i.e. the hits in it are
###     sorted by query), then 'revmap_Hits(x)' returns a SortedByQueryHits
###     object where hits are "fully sorted" i.e. sorted by query first and
###     then by subject.
###   - Because revmap_Hits() reorders the hits by query, doing
###     'revmap_Hits(revmap_Hits(x))' brings back 'x' but with the hits in it
###     now "fully sorted".
revmap_Hits <- function(x)
    new_Hits(class(x), subjectHits(x), queryHits(x),
                       subjectLength(x), queryLength(x),
                       mcols(x))

### FIXME: Replace this with "revmap" method for Hits objects.
setMethod("t", "Hits", revmap_Hits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Remap the query and/or subject hits
###

### Returns 'arg' as a NULL, an integer vector, or a factor.
.normargMap <- function(arg, sidename, old.length)
{
    if (is.null(arg))
        return(arg)
    if (!is.factor(arg)) {
        if (!is.numeric(arg))
            stop("'" , sidename, ".map' must be a vector of integers")
        if (!is.integer(arg))
            arg <- as.integer(arg)
    }
    if (length(arg) != old.length)
        stop("'" , sidename, ".map' must have the length of the ", sidename)
    arg
}

.normargNewLength <- function(arg, sidename, map)
{
    if (!isSingleNumberOrNA(arg))
        stop("'new.", sidename, "Length' must be a single number or NA")
    if (!is.integer(arg))
        arg <- as.integer(arg)
    if (is.null(map))
        return(arg)
    if (is.factor(map)) {
        if (is.na(arg))
            return(nlevels(map))
        if (arg < nlevels(map))
            stop("supplied 'new.", sidename, "Length' must ",
                 "be >= 'nlevels(", sidename, ".map)'")
        return(arg)
    }
    if (is.na(arg))
        stop("'new.", sidename, "Length' must be specified when ",
             "'" , sidename, ".map' is specified and is not a factor")
    arg
}

remapHits <- function(x, query.map=NULL, new.queryLength=NA,
                         subject.map=NULL, new.subjectLength=NA,
                         with.counts=FALSE)
{
    if (!is(x, "SortedByQueryHits"))
        stop(wmsg("remapHits() only works on a SortedByQueryHits object ",
                  "at the moment"))
    query.map <- .normargMap(query.map, "query", queryLength(x))
    new.queryLength <- .normargNewLength(new.queryLength,
                                         "query", query.map)
    subject.map <- .normargMap(subject.map, "subject", subjectLength(x))
    new.subjectLength <- .normargNewLength(new.subjectLength,
                                           "subject", subject.map)
    if (!isTRUEorFALSE(with.counts))
        stop("'with.counts' must be TRUE or FALSE")
    q_hits <- queryHits(x)
    if (is.null(query.map)) {
        if (is.na(new.queryLength))
            new.queryLength <- queryLength(x)
    } else {
        if (is.factor(query.map))
            query.map <- as.integer(query.map)
        if (anyMissingOrOutside(query.map, 1L, new.queryLength))
            stop("'query.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.queryLength'")
        q_hits <- query.map[q_hits]
    }
    s_hits <- subjectHits(x)
    if (is.null(subject.map)) {
        if (is.na(new.subjectLength))
            new.subjectLength <- subjectLength(x)
    } else {
        if (is.factor(subject.map))
            subject.map <- as.integer(subject.map)
        if (anyMissingOrOutside(subject.map, 1L, new.subjectLength))
            stop("'subject.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.subjectLength'")
        s_hits <- subject.map[s_hits]
    }
    x_mcols <- mcols(x)
    add_counts <- function(counts) {
        if (is.null(x_mcols))
            return(DataFrame(counts=counts))
        if ("counts" %in% colnames(x_mcols))
            warning("'x' has a \"counts\" metadata column, replacing it")
        x_mcols$counts <- counts
        x_mcols
    }
    if (is.null(query.map) && is.null(subject.map)) {
        if (with.counts) {
            counts <- rep.int(1L, length(x))
            x_mcols <- add_counts(counts)
        }
    } else {
        sm <- selfmatchIntegerPairs(q_hits, s_hits)
        if (with.counts) {
            counts <- tabulate(sm, nbins=length(sm))
            x_mcols <- add_counts(counts)
            keep_idx <- which(counts != 0L)
        } else {
            keep_idx <- which(sm == seq_along(sm))
        }
        q_hits <- q_hits[keep_idx]
        s_hits <- s_hits[keep_idx]
        x_mcols <- extractROWS(x_mcols, keep_idx)
    }
    do.call(Hits, c(list(q_hits, s_hits,
                         new.queryLength, new.subjectLength),
                    as.list(x_mcols)))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Auto-hits
###
### When the query and subject are the same, the hits between them are
### "auto-hits". A Hits object containing auto-hits must have its queryLength
### equal to its subjectLength. It can be seen as an oriented graph where
### queryLength is the nb of nodes and the hits are the (oriented) edges.
###

.error_if_not_auto_hits <- function(x)
{
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    if (queryLength(x) != subjectLength(x))
        stop("'queryLength(x)' and 'subjectLength(x)' must be equal")
}

### A "self hit" is an edge from a node to itself. For example, the 2nd hit in
### the Hits object below is a self hit (from 3rd node to itself):
###     Hits(c(3, 3, 3, 4, 4), c(2:4, 2:3), 4, 4)
isSelfHit <- function(x)
{
    .error_if_not_auto_hits(x)
    queryHits(x) == subjectHits(x)
}

### When there is more than 1 edge between 2 given nodes (regardless of
### orientation), the extra edges are considered to be "redundant hits". For
### example, hits 3, 5, 7, and 8, in the Hits object below are redundant hits:
###     Hits(c(3, 3, 3, 3, 3, 4, 4, 4), c(3, 2:4, 2, 2:3, 2), 4, 4)
### Note that this is regardless of the orientation of the edge so hit 7 (edge
### 4-3) is considered to be redundant with hit 4 (edge 3-4).
isRedundantHit <- function(x)
{
    .error_if_not_auto_hits(x)
    duplicatedIntegerPairs(pmin.int(queryHits(x), subjectHits(x)),
                           pmax.int(queryHits(x), subjectHits(x)))
}

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
    N <- sum(GS)  # length of original vector (i.e. before grouping)

    ## Original Group Size Assignment i.e. group size associated with each
    ## element in the original vector.
    OGSA <- rep.int(GS, GS)  # has length N
    q_hits <- rep.int(seq_len(N), OGSA)
    NH <- length(q_hits)  # same as sum(GS2)

    ## Hit Group Assignment i.e. group associated with each hit.
    HGA <- rep.int(seq_len(NG), GS2)
    ## Hit Group Size Assignment i.e. group size associated with each hit.
    HGSA <- GS[HGA]
    s_hits <- (0:(NH-1L) - CGSr2[HGA]) %% GS[HGA] + FEIG[HGA]
    Hits(q_hits, s_hits, N, N)
}

