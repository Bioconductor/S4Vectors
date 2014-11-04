### =========================================================================
### Hits objects
### -------------------------------------------------------------------------

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

setMethod("countQueryHits", "Hits",
    function(x) tabulate(queryHits(x), nbins=queryLength(x))
)

setGeneric("countSubjectHits",
    function(x, ...) standardGeneric("countSubjectHits")
)

setMethod("countSubjectHits", "Hits",
    function(x) tabulate(subjectHits(x), nbins=subjectLength(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.Hits.queryLength <- function(x)
{
    x_q_len <- queryLength(x)
    if (!isSingleInteger(x_q_len) || x_q_len < 0L)
        return("'queryLength(x)' must be a single non-negative integer")
    if (!is.null(attributes(x_q_len)))
        return("'queryLength(x)' must be a single integer with no attributes")
    NULL
}

.valid.Hits.subjectLength <- function(x)
{
    x_s_len <- subjectLength(x)
    if (!isSingleInteger(x_s_len) || x_s_len < 0L) 
        return("'subjectLength(x)' must be a single non-negative integer")
    if (!is.null(attributes(x_s_len)))
        return("'subjectLength(x)' must be a single integer with no attributes")
    NULL
}

.valid.Hits.queryHits_or_subjectHits <- function(q_hits, q_len, what)
{
    if (!(is.integer(q_hits) && is.null(attributes(q_hits)))) {
        msg <- c("'", what, "Hits(x)' must be an integer vector ",
                 "with no attributes")
        return(paste(msg, collapse=""))
    }
    if (anyMissingOrOutside(q_hits, 1L, q_len)) {
        msg <- c("'", what, "Hits(x)' must contain non-NA values ",
                 ">= 1 and <= '", what, "Length(x)'")
        return(paste(msg, collapse=""))
    }
    NULL
}

### Coercion from Hits to List is very fast because it assumes that the hits
### are already sorted by query. So for a Hits object to be valid we require
### that the hits in it are already sorted by query.
.valid.Hits.queryHits_ordering <- function(q_hits)
{
    if (isNotSorted(q_hits))
        return("'queryHits(x)' must be sorted")
    NULL
}

.valid.Hits.queryHits <- function(x)
{
    x_q_hits <- queryHits(x)
    x_q_len <- queryLength(x)
    c(.valid.Hits.queryHits_or_subjectHits(x_q_hits, x_q_len, "query"),
      .valid.Hits.queryHits_ordering(x_q_hits))
}

.valid.Hits.subjectHits <- function(x)
{
    x_s_hits <- subjectHits(x)
    x_s_len <- subjectLength(x)
    .valid.Hits.queryHits_or_subjectHits(x_s_hits, x_s_len, "subject")
}

.valid.Hits <- function(x)
{
    c(.valid.Hits.queryLength(x),
      .valid.Hits.subjectLength(x),
      .valid.Hits.queryHits(x),
      .valid.Hits.subjectHits(x))
}

setValidity2("Hits", .valid.Hits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### The "extractROWS" method for Vector objects doesn't test the validity of
### the result so we override it.
setMethod("extractROWS", "Hits",
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
### Coercion
###

setMethod("as.matrix", "Hits",
    function(x) cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
)

## count up the hits for each query

setMethod("as.table", "Hits", function(x, ...) {
  tabulate(queryHits(x), queryLength(x))
})

### FIXME: this needs a new name given the switch to Vector
### Note that, because this method reorders the hits by query, you should NOT
### expect 't(t(x))' to bring back 'x'.
setMethod("t", "Hits", function(x) {
  tmp <- x@queryHits
  x@queryHits <- x@subjectHits
  x@subjectHits <- tmp
  tmp <- x@queryLength
  x@queryLength <- x@subjectLength
  x@subjectLength <- tmp
  ## Reorder the hits by query so the returned value is a valid Hits object.
  extractROWS(x, orderInteger(x@queryHits))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### makeAllGroupInnerHits()
###
### NOT exported.

### About 10x faster and uses 4x less memory than my first attempt in pure
### R below.
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
    query_hits <- rep.int(seq_len(N), OGSA)
    NH <- length(query_hits)  # same as sum(GS2)

    ## Hit Group Assignment i.e. group associated with each hit.
    HGA <- rep.int(seq_len(NG), GS2)
    ## Hit Group Size Assignment i.e. group size associated with each hit.
    HGSA <- GS[HGA]
    subject_hits <- (0:(NH-1L) - CGSr2[HGA]) %% GS[HGA] + FEIG[HGA]
    new2("Hits", queryHits=query_hits, subjectHits=subject_hits,
                 queryLength=N, subjectLength=N, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###

compatibleHits <- function(x, y) {
  subjectLength(x) == subjectLength(y) && queryLength(x) == queryLength(y)
}

setMethod("match", c("Hits", "Hits"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        if (!compatibleHits(x, table))
            stop("'x' and 'table' are incompatible by subject and query length")
        if (!is.null(incomparables))
            stop("\"match\" method for Hits objects ",
                 "only accepts 'incomparables=NULL'")
        matchIntegerPairs(queryHits(x), subjectHits(x),
                          queryHits(table), subjectHits(table),
                          nomatch=nomatch)
    }
)

setMethod("selfmatch", "Hits",
    function (x, method=c("auto", "quick", "hash"))
        selfmatchIntegerPairs(queryHits(x), subjectHits(x), method=method)
)


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
    if (is.null(map)) {
        if (!is.na(arg))
            stop("'new.", sidename, "Length' must be NA ",
                 "when '" , sidename, ".map' is NULL")
        return(arg)
    }
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
                         subject.map=NULL, new.subjectLength=NA)
{
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    query.map <- .normargMap(query.map, "query", queryLength(x))
    new.queryLength <- .normargNewLength(new.queryLength,
                                         "query", query.map)
    subject.map <- .normargMap(subject.map, "subject", subjectLength(x))
    new.subjectLength <- .normargNewLength(new.subjectLength,
                                           "subject", subject.map)
    query_hits <- queryHits(x)
    subject_hits <- subjectHits(x)
    if (is.null(query.map)) {
        new.queryLength <- queryLength(x)
    } else {
        if (is.factor(query.map))
            query.map <- as.integer(query.map)
        if (anyMissingOrOutside(query.map, 1L, new.queryLength))
            stop("'query.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.queryLength'")
        query_hits <- query.map[query_hits]
    }
    if (is.null(subject.map)) {
        new.subjectLength <- subjectLength(x)
    } else {
        if (is.factor(subject.map))
            subject.map <- as.integer(subject.map)
        if (anyMissingOrOutside(subject.map, 1L, new.subjectLength))
            stop("'subject.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.subjectLength'")
        subject_hits <- subject.map[subject_hits]
    }
    not_dup <- !duplicatedIntegerPairs(query_hits, subject_hits)
    query_hits <- query_hits[not_dup]
    subject_hits <- subject_hits[not_dup]
    oo <- orderIntegerPairs(query_hits, subject_hits)
    new("Hits", queryHits=query_hits[oo], subjectHits=subject_hits[oo],
                queryLength=new.queryLength, subjectLength=new.subjectLength)
}

