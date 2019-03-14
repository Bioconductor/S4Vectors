### =========================================================================
### Some low-level utility functions to operate on raw vectors
### -------------------------------------------------------------------------
###
### Unless stated otherwise, the functions defined in this file are not
### exported.
###


TOUPPER_LOOKUP <- c(0:96, 65:90, 123:255)
TOLOWER_LOOKUP <- c(0:64, 97:122, 91:255)

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

