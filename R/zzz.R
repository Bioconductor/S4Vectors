###

.onLoad <- function(libname, pkgname)
{
    ns <- asNamespace(pkgname)

    objname <- "NA_Linteger_"
    NA_Linteger_ <- make_NA_Linteger_()
    assign(objname, NA_Linteger_, envir=ns)
    namespaceExport(ns, objname)
}

.onUnload <- function(libpath)
{
    library.dynam.unload("S4Vectors", libpath)
}

.test <- function() BiocGenerics:::testPackage("S4Vectors")

