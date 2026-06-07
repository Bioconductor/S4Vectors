test_setValidity2_environment <- function()
{
    classname <- "S4VectorsSetValidity2Env"
    where <- new.env(parent=globalenv())
    enclos <- environment(setValidity2)
    remove <- function()
        suppressWarnings(try(removeClass(classname, where=where), silent=TRUE))
    remove()
    on.exit(remove())

    limit <- 0L
    setClass(classname, slots=c(x="integer"), where=where)
    method <- function(object) {
        if (object@x <= limit)
            "x must be positive"
        else TRUE
    }
    setValidity2(classname, method, where=where)

    classdef <- getClass(classname, where=where)
    validity <- classdef@validity
    checkIdentical(environment(validity), enclos)
    checkTrue("debugValidity" %in% ls(enclos, all.names=TRUE))
    checkTrue(validObject(new(classdef, x=1L)))
    checkException(new(classdef, x=0L))
}
