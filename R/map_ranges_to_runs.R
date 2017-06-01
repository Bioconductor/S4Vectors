### =========================================================================
### map_ranges_to_runs()
### -------------------------------------------------------------------------
###


### Used in GenomicRanges.
map_ranges_to_runs <- function(run_lens, start, width, method=0L)
{
    method <- .normarg_method(method)
    .Call2("map_ranges", run_lens, start, width, method, PACKAGE="S4Vectors")
}

