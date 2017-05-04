###

.onLoad <- function(libname, pkgname)
{
    ns <- asNamespace(pkgname)

    objname <- "NA_LLint_"
    NA_LLint_ <- make_NA_LLint_()
    assign(objname, NA_LLint_, envir=ns)
    namespaceExport(ns, objname)
}

.onUnload <- function(libpath)
{
    library.dynam.unload("S4Vectors", libpath)
}

.test <- function() BiocGenerics:::testPackage("S4Vectors")

