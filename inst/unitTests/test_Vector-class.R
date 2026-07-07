test_parallelVectorNames_uses_class_prototype <- function()
{
    if (!requireNamespace("GenomicRanges", quietly=TRUE))
        return()

    gr <- GenomicRanges::GRanges("chr1", IRanges::IRanges(1:3, width=10),
                                 gc=c(0.1, 0.9, 0.2))
    checkIdentical(parallelVectorNames(gr),
                   c("seqnames", "start", "end", "width", "strand"))

    gr2 <- transform(gr, gc=gc * 10)
    checkIdentical(mcols(gr2, use.names=FALSE)$gc, c(1, 9, 2))

    env <- as.env(gr, parent.frame())
    checkIdentical(get("gc", env), c(0.1, 0.9, 0.2))
}

test_parallelVectorNames_supports_oldClass_prototype <- function()
{
    x <- structure(Sys.time() + 1:3, class=c("POSIXct", "POSIXt"))
    checkException(new("POSIXct"), silent=TRUE)
    checkIdentical(parallelVectorNames(x), "proto")
}
