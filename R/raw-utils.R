### =========================================================================
### Some low-level utility functions to operate on raw vectors
### -------------------------------------------------------------------------
###
### Unless stated otherwise, the functions defined in this file are not
### exported.
###


extract_character_from_raw_by_positions <- function(x, pos,
                                                    collapse=FALSE, lkup=NULL)
{
    .Call("C_extract_character_from_raw_by_positions",
          x, pos, collapse, lkup,
          PACKAGE="S4Vectors")
}

extract_character_from_raw_by_ranges <- function(x, start, width,
                                                 collapse=FALSE, lkup=NULL)
{
    .Call("C_extract_character_from_raw_by_ranges",
          x, start, width, collapse, lkup,
          PACKAGE="S4Vectors")
}

