### =========================================================================
### map_ranges_to_runs()
### -------------------------------------------------------------------------
###

normarg_method <- function(method)
{
    if (!(isSingleNumber(method) && method >= 0 && method <= 3))
        stop("'method' must be a single integer between 0 and 3")
    if (!is.integer(method))
        method <- as.integer(method)
    method
}

### Used in GenomicRanges.
map_ranges_to_runs <- function(run_lens, start, width, method=0L)
{
    method <- normarg_method(method)
    .Call2("map_ranges", run_lens, start, width, method, PACKAGE="S4Vectors")
}

### Note that
###
###     map_positions_to_runs(run_lengths, pos)
###
### is equivalent to
###
###     findInterval(pos - 1L, cumsum(run_lengths)) + 1L
###
### but is more efficient, specially when the number of runs is big and the
### number of positions to map relatively small with respect to the number of
### runs (in which case map_positions_to_runs() can be 10x or 20x faster than
### findInterval()).
map_positions_to_runs <- function(run_lens, pos, method=0L)
{
    method <- normarg_method(method)
    .Call2("map_positions", run_lens, pos, method, PACKAGE="S4Vectors")
}

