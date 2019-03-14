### =========================================================================
### Some low-level utility functions to operate on raw vectors
### -------------------------------------------------------------------------
###
### Unless stated otherwise, the functions defined in this file are not
### exported.
###


extract_raw_positions_as_character <- function(x, pos,
                                               collapse=FALSE, lkup=NULL)
{
    .Call("C_extract_raw_positions_as_character",
          x, pos, collapse, lkup,
          PACKAGE="S4Vectors")
}

extract_raw_ranges_as_character <- function(x, start, width,
                                            collapse=FALSE, lkup=NULL)
{
    .Call("C_extract_raw_ranges_as_character",
          x, start, width, collapse, lkup,
          PACKAGE="S4Vectors")
}

