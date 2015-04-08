### =========================================================================
### Some low-level S4 classes and utilities
### -------------------------------------------------------------------------
###


setClassUnion("characterORNULL", c("character", "NULL"))

### WARNING: The behavior of is.vector(), is( , "vector"), is.list(), and
### is( ,"list") makes no sense:
###   1. is.vector(matrix()) is FALSE but is(matrix(), "vector") is TRUE.
###   2. is.list(data.frame()) is TRUE but is(data.frame(), "list") is FALSE.
###   3. is.vector(data.frame()) is FALSE but is.list(data.frame()) and
###      is.vector(list()) are both TRUE. In other words: a data frame is a
###      list and a list is a vector but a data frame is not a vector.
###   4. I'm sure there is more but you get it!
### Building our software on top of such a mess won't give us anything good.
### For example, it's not too surprising that the union class we define below
### is broken:
###   5. is(data.frame(), "vectorORfactor") is TRUE even though
###      is(data.frame(), "vector") and is(data.frame(), "factor") are both
###      FALSE.
### Results above obtained with R-3.1.2 and R-3.2.0.
### TODO: Be brave and report this craziness to the R bug tracker.
setClassUnion("vectorORfactor", c("vector", "factor"))


### We define the coercion method below as a workaround to the following
### bug in R:
###
###   setClass("A", representation(stuff="numeric"))
###   setMethod("as.vector", "A", function(x, mode="any") x@stuff)
###
###   a <- new("A", stuff=3:-5)
###   > as.vector(a)
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
###   > as(a, "vector")
###   Error in as.vector(from) : 
###     no method for coercing this S4 class to a vector
###   > selectMethod("coerce", c("A", "vector"))
###   Method Definition:
###
###   function (from, to, strict = TRUE) 
###   {
###       value <- as.vector(from)
###       if (strict) 
###           attributes(value) <- NULL
###       value
###   }
###   <environment: namespace:methods>
###
###   Signatures:
###           from  to      
###   target  "A"   "vector"
###   defined "ANY" "vector"
###   > setAs("ANY", "vector", function(from) as.vector(from))
###   > as(a, "vector")
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
###
###   ML: The problem is that the default coercion method is defined
###   in the methods namespace, which does not see the as.vector()
###   generic we define. Solution in this case would probably be to
###   make as.vector a dispatching primitive like as.character(), but
###   the "mode" argument makes things complicated.
setAs("ANY", "vector", function(from) as.vector(from))

coercerToClass <- function(class) {
  if (extends(class, "vector"))
    .as <- get(paste0("as.", class))
  else .as <- function(from) as(from, class)
  function(from) {
    to <- .as(from)
    if (!is.null(names(from)) && is.null(names(to))) {
      names(to) <- names(from)
    }
    to
  }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### setValidity2(), new2()
###
### Give more contol over when object validation should happen.
###

.validity_options <- new.env(hash=TRUE, parent=emptyenv())

assign("debug", FALSE, envir=.validity_options)
assign("disabled", FALSE, envir=.validity_options)

debugValidity <- function(debug)
{
    if (missing(debug))
        return(get("debug", envir=.validity_options))
    debug <- isTRUE(debug)
    assign("debug", debug, envir=.validity_options)
    debug
}

disableValidity <- function(disabled)
{
    if (missing(disabled))
        return(get("disabled", envir=.validity_options))
    disabled <- isTRUE(disabled)
    assign("disabled", disabled, envir=.validity_options)
    disabled
}

setValidity2 <- function(Class, valid.func, where=topenv(parent.frame()))
{
    setValidity(Class,
        function(object)
        {
            if (disableValidity())
                return(TRUE)
            if (debugValidity()) {
                whoami <- paste("validity method for", Class, "object")
                cat("[debugValidity] Entering ", whoami, "\n", sep="")
                on.exit(cat("[debugValidity] Leaving ", whoami, "\n", sep=""))
            }
            problems <- valid.func(object)
            if (isTRUE(problems) || length(problems) == 0L)
                return(TRUE)
            problems
        },
        where=where
    )
}

new2 <- function(..., check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    old_val <- disableValidity()
    on.exit(disableValidity(old_val))
    disableValidity(!check)
    new(...)
}

stopIfProblems <- function(problems)
    if (!is.null(problems)) stop(paste(problems, collapse="\n  "))

### 'signatures' must be a list of character vectors. To use when many methods
### share the same implementation.
setMethods <- function(f, signatures=list(), definition,
                       where=topenv(parent.frame()), ...)
{
    for (signature in signatures)
        setMethod(f, signature=signature, definition, where=where, ...)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Manipulating the prototype of an S4 class.
###

### Gets or sets the default value of the given slot of the given class by
### reading or altering the prototype of the class. setDefaultSlotValue() is
### typically used in the .onLoad() hook of a package when the DLL of the
### package needs to be loaded *before* the default value of a slot can be
### computed.
getDefaultSlotValue <- function(classname, slotname, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname, exact=TRUE)
}

setDefaultSlotValue <- function(classname, slotname, value, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname) <- value
    assignClassDef(classname, classdef, where=where)
    ## Re-compute the complete definition of the class. methods::setValidity()
    ## does that after calling assignClassDef() so we do it too.
    resetClass(classname, classdef, where=where)
}

setPrototypeFromObject <- function(classname, object, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (class(object) != classname)
        stop("'object' must be a ", classname, " instance")
    object_attribs <- attributes(object)
    object_attribs$class <- NULL
    ## Sanity check.
    stopifnot(identical(names(object_attribs),
                        names(attributes(classdef@prototype))))
    attributes(classdef@prototype) <- object_attribs
    assignClassDef(classname, classdef, where=where)
    ## Re-compute the complete definition of the class. methods::setValidity()
    ## does that after calling assignClassDef() so we do it too.
    resetClass(classname, classdef, where=where)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
### allEqualsS4: just a hack that auomatically digs down
### deeply nested objects to detect differences.
###

.allEqualS4 <- function(x, y) {
  eq <- all.equal(x, y)
  canCompareS4 <- !isTRUE(eq) && isS4(x) && isS4(y) && class(x) == class(y)
  if (canCompareS4) {
    child.diffs <- mapply(.allEqualS4, attributes(x), attributes(y),
                          SIMPLIFY=FALSE)
    child.diffs$class <- NULL
    dfs <- mapply(function(d, nm) {
      if (!is.data.frame(d)) {
        data.frame(comparison = I(list(d)))
      } else d
    }, child.diffs, names(child.diffs), SIMPLIFY=FALSE)
    do.call(rbind, dfs)
  } else {
    eq[1]
  }
}

allEqualS4 <- function(x, y) {
  eq <- .allEqualS4(x, y)
  setNames(eq$comparison, rownames(eq))[sapply(eq$comparison, Negate(isTRUE))]
}
