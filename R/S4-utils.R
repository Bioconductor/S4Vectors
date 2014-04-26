### 
### Utility functions for reducing redundant testing of object validity.
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

