### =========================================================================
### SimpleList objects
### -------------------------------------------------------------------------


setClass("SimpleList",
    contains="List",
    representation(
        listData="list"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "SimpleList",
    function(x) c("listData", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods
###

setMethod("names", "SimpleList", function(x) names(as.list(x)))

setReplaceMethod("names", "SimpleList",
                 function(x, value) {
                     names(x@listData) <- value
                     x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Low-level. NOT exported.
### Stuff to put in elementMetadata slot can be passed either with
###   new_SimpleList_from_list(..., elementMetadata=somestuff)
### or with
###   new_SimpleList_from_list(..., mcols=somestuff)
### The latter is the new recommended form.
new_SimpleList_from_list <- function(Class, x, ..., mcols)
{
    if (!extends(Class, "SimpleList"))
        stop("class ", Class, " must extend SimpleList")
    if (!is.list(x))
        stop("'x' must be a list")
    if (is.array(x)) { # drop any unwanted dimensions
        tmp_names <- names(x)
        dim(x) <- NULL # clears the names
        names(x) <- tmp_names
    }
    class(x) <- "list"
    ans_elementType <- elementType(new(Class))
    if (!all(sapply(x, function(xi) extends(class(xi), ans_elementType))))
        stop("all elements in 'x' must be ", ans_elementType, " objects")
    if (missing(mcols))
        return(new2(Class, listData=x, ..., check=FALSE))
    new2(Class, listData=x, ..., elementMetadata=mcols, check=FALSE)
}

SimpleList <- function(...)
{
    args <- list(...)
    ## The extends(class(x), "list") test is NOT equivalent to is.list(x) or
    ## to is(x, "list") or to inherits(x, "list"). Try for example with
    ## x <- data.frame() or x <- matrix(list()). We use the former below
    ## because it seems to closely mimic what the methods package uses for
    ## checking the "listData" slot of the SimpleList object that we try to
    ## create later with new(). For example if we were using is.list() instead
    ## of extends(), the test would pass on matrix(list()) but new() then would
    ## fail with the following message:
    ## Error in validObject(.Object) :
    ##   invalid class “SimpleList” object: invalid object for slot "listData"
    ##   in class "SimpleList": got class "matrix", should be or extend class
    ##   "list"
    if (length(args) == 1L && extends(class(args[[1L]]), "list"))
        args <- args[[1L]]
    new2("SimpleList", listData=args, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.SimpleList.listData <- function(x)
{
    elementTypeX <- elementType(x)
    if (!all(sapply(as.list(x),
                    function(xi) extends(class(xi), elementTypeX))))
        return(paste("the 'listData' slot must be a list containing",
                     elementTypeX, "objects"))
    NULL
}
.valid.SimpleList <- function(x)
{
    c(.valid.SimpleList.listData(x))
}
setValidity2("SimpleList", .valid.SimpleList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### classNameForDisplay()
###

setMethod("classNameForDisplay", "SimpleList",
    function(x) sub("^Simple", "", class(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("getListElement", "SimpleList",
    function(x, i, exact=TRUE)
        getListElement(x@listData, i, exact=exact)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping
###

### TODO: easily generalized to List
setMethod("lapply", "SimpleList",
          function(X, FUN, ...)
              lapply(as.list(X), FUN = FUN, ...))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### Unfortunately, not all SimpleList subclasses (e.g. BamFileList or
### ExperimentList) support coercion from an ordinary list (even though
### they probably should), so the default "coerce2" method will fail to
### convert an ordinary list to one of these classes. The good news is that
### coercion from SimpleList to one of these classes does work. For example:
###
###   library(Rsamtools)
###   as(list(), "BamFileList")           # error
###   as(SimpleList(), "BamFileList")     # works
###
###   library(MultiAssayExperiment)
###   as(list(), "ExperimentList")        # error
###   as(SimpleList(), "ExperimentList")  # works
###
### So when the default "coerce2" method fails to coerce an ordinary list,
### we wrap the list in a SimpleList instance and try again. Note that this
### should help in general because it brings 'from' a little bit closer to
### 'class(to)'.
setMethod("coerce2", "SimpleList",
    function(from, to)
    {
        ans <- try(callNextMethod(), silent=TRUE)
        if (!inherits(ans, "try-error"))
            return(ans)
        if (!is.list(from))
            stop(wmsg(attr(ans, "condition")$message))
        ## We use the SimpleList() constructor function to wrap 'from' in a
        ## SimpleList instance instead of coercion to SimpleList (which is
        ## too high level and tries to be too smart).
        from <- SimpleList(from)
        mcols(from) <- mcols(to)[rep.int(NA_integer_, length(from)), ,
                                 drop=FALSE]
        ans <- callNextMethod()
        ## Even though coercion from SimpleList to 'class(to)' "worked", it
        ## can return a broken object. This happens when an automatic coercion
        ## method gets in the way. For example:
        ##
        ##   selectMethod("coerce", c("SimpleList", "BamFileList"))
        ##
        ## shows one of these methods (it's not coming from the Rsamtools or
        ## S4Vectors package). The problem with these methods is that they
        ## often do the wrong thing and don't even bother to validate the
        ## object they return!
        ## One known problem with the automatic coercion method from SimpleList
        ## to one of its subclass is that it sets the elementType slot to "ANY"
        ## which is generally wrong. So we fix this.
        ans@elementType <- to@elementType
        validObject(ans)
        ans
    }
)

.as.list.SimpleList <- function(x, use.names=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ans <- x@listData
    if (!use.names)
        names(ans) <- NULL
    ans
}
setMethod("as.list", "SimpleList", .as.list.SimpleList)

setAs("ANY", "SimpleList", function(from) {
  coerceToSimpleList(from)
})

setAs("list", "List", function(from) {
  coerceToSimpleList(from)
})

coerceToSimpleList <- function(from, element.type, ...) {
  if (missing(element.type)) {
    if (is(from, "List"))
      element.type <- from@elementType
    else if (is.list(from))
      element.type <- lowestListElementClass(from)
    else element.type <- class(from)
  }
  SimpleListClass <- listClassName("Simple", element.type)
  if (!is(from, SimpleListClass)) {
    listData <- as.list(from)
    if (!is.null(element.type))
      listData <- lapply(listData, coercerToClass(element.type), ...)
    new_SimpleList_from_list(SimpleListClass, listData)
  } else {
    from
  }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unique()
###

### TODO: easily generalized to List
.unique.SimpleList <- function(x, incomparables=FALSE, ...) {
    as(lapply(x, unique, incomparables=incomparables, ...), class(x))
}
setMethod("unique", "SimpleList", .unique.SimpleList)

