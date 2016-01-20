.onUnload <- function(libpath)
{
    library.dynam.unload("S4Vectors", libpath)
}

.test <- function() BiocGenerics:::testPackage("S4Vectors")

