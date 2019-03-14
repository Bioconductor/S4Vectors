test_extract_character_from_raw_by_positions <- function()
{
    TOLOWER_LOOKUP <- S4Vectors:::TOLOWER_LOOKUP
    extract_character_from_raw_by_positions <-
        S4Vectors:::extract_character_from_raw_by_positions

    do_tests <- function(x, pos, target0, lkup, target1) {
        current <- extract_character_from_raw_by_positions(x, pos)
        checkIdentical(target0, current)

        current <- extract_character_from_raw_by_positions(x, pos,
                                                           collapse=TRUE)
        target <- paste0(target0, collapse="")
        checkIdentical(target, current)

        current <- extract_character_from_raw_by_positions(x, pos, lkup=lkup)
        checkIdentical(target1, current)

        current <- extract_character_from_raw_by_positions(x, pos,
                                                           collapse=TRUE,
                                                           lkup=lkup)
        target <- paste0(target1, collapse="")
        checkIdentical(target, current)
    }

    x <- charToRaw("ABCDEFAAA")
    weird_lkup <- c(rep.int(NA_integer_, 65L), 122:117)

    pos <- integer(0)
    target0 <- target1 <- character(0)
    do_tests(x, pos, target0, TOLOWER_LOOKUP, target1)
    do_tests(x, pos, target0, weird_lkup, target1)

    pos <- c(6L, 9L, 1L)
    target0 <- substring(rawToChar(x), pos, pos)
    target1 <- c("f", "a", "a")
    do_tests(x, pos, target0, TOLOWER_LOOKUP, target1)
    target1 <- c("u", "z", "z")
    do_tests(x, pos, target0, weird_lkup, target1)

    pos <- seq_along(x)
    target0 <- safeExplode(rawToChar(x))
    target1 <- c("a", "b", "c", "d", "e", "f", "a", "a", "a")
    do_tests(x, pos, target0, TOLOWER_LOOKUP, target1)
    target1 <- c("z", "y", "x", "w", "v", "u", "z", "z", "z")
    do_tests(x, pos, target0, weird_lkup, target1)

    ## With byte not mapped in lookup table.
    x <- charToRaw("ABCDEFAAAGF")  # 'G' is not mapped in 'weird_lkup'
    pos <- seq_along(x)
    checkException(extract_character_from_raw_by_positions(x, pos,
                                                           lkup=weird_lkup))
    checkException(extract_character_from_raw_by_positions(x, pos,
                                                           collapse=TRUE,
                                                           lkup=weird_lkup))
    pos <- 1:9
    target0 <- substring(rawToChar(x), pos, pos)
    target1 <- c("z", "y", "x", "w", "v", "u", "z", "z", "z")
    do_tests(x, pos, target0, weird_lkup, target1)

    x <- charToRaw("ABCDEFAAA8F")  # '8' is not mapped in 'weird_lkup'
    pos <- seq_along(x)
    checkException(extract_character_from_raw_by_positions(x, pos,
                                                           lkup=weird_lkup))
    checkException(extract_character_from_raw_by_positions(x, pos,
                                                           collapse=TRUE,
                                                           lkup=weird_lkup))
    pos <- 1:9
    target0 <- substring(rawToChar(x), pos, pos)
    target1 <- c("z", "y", "x", "w", "v", "u", "z", "z", "z")
    do_tests(x, pos, target0, weird_lkup, target1)
}

test_extract_character_from_raw_by_ranges <- function()
{
    TOLOWER_LOOKUP <- S4Vectors:::TOLOWER_LOOKUP
    extract_character_from_raw_by_ranges <-
        S4Vectors:::extract_character_from_raw_by_ranges

    do_tests <- function(x, start, width, target0, lkup, target1) {
        current <- extract_character_from_raw_by_ranges(x, start, width)
        checkIdentical(target0, current)

        current <- extract_character_from_raw_by_ranges(x, start, width,
                                                        collapse=TRUE)
        target <- paste0(target0, collapse="")
        checkIdentical(target, current)

        current <- extract_character_from_raw_by_ranges(x, start, width,
                                                        lkup=lkup)
        checkIdentical(target1, current)

        current <- extract_character_from_raw_by_ranges(x, start, width,
                                                        collapse=TRUE,
                                                        lkup=lkup)
        target <- paste0(target1, collapse="")
        checkIdentical(target, current)
    }

    x <- charToRaw("ABCDEFAAA")
    weird_lkup <- c(rep.int(NA_integer_, 65L), 122:117)

    start <- width <- integer(0)
    target0 <- target1 <- character(0)
    do_tests(x, start, width, target0, TOLOWER_LOOKUP, target1)
    do_tests(x, start, width, target0, weird_lkup, target1)

    start <- c(6L, 10L, 1L)
    width <- c(2L,  0L, 9L)
    target0 <- substring(rawToChar(x), start, start + width - 1L)
    target1 <- c("fa", "", "abcdefaaa")
    do_tests(x, start, width, target0, TOLOWER_LOOKUP, target1)
    target1 <- c("uz", "", "zyxwvuzzz")
    do_tests(x, start, width, target0, weird_lkup, target1)

    start <- seq_along(x)
    width <- rep.int(1L, length(x))
    target0 <- safeExplode(rawToChar(x))
    target1 <- c("a", "b", "c", "d", "e", "f", "a", "a", "a")
    do_tests(x, start, width, target0, TOLOWER_LOOKUP, target1)
    target1 <- c("z", "y", "x", "w", "v", "u", "z", "z", "z")
    do_tests(x, start, width, target0, weird_lkup, target1)

    ## Error when too many characters to read.
    xx <- rep.int(x, 1e6)
    start <- rep.int(1L, 239)
    width <- rep.int(length(xx), 239)
    checkException(extract_character_from_raw_by_ranges(xx, start, width,
                                                        collapse=TRUE))

    ## With byte not mapped in lookup table.
    x <- charToRaw("ABCDEFAAAGF")  # 'G' is not mapped in 'weird_lkup'
    start <- c(6L, 10L, 9L)
    width <- c(2L,  0L, 3L)
    checkException(extract_character_from_raw_by_ranges(x, start, width,
                                                        lkup=weird_lkup))
    checkException(extract_character_from_raw_by_ranges(x, start, width,
                                                        collapse=TRUE,
                                                        lkup=weird_lkup))
    start <- c(6L, 10L, 11L)
    width <- c(2L,  0L,  1L)
    target0 <- substring(rawToChar(x), start, start + width - 1L)
    target1 <- c("uz", "", "u")
    do_tests(x, start, width, target0, weird_lkup, target1)

    x <- charToRaw("ABCDEFAAA8F")  # '8' is not mapped in 'weird_lkup'
    start <- c(6L, 10L, 9L)
    width <- c(2L,  0L, 3L)
    checkException(extract_character_from_raw_by_ranges(x, start, width,
                                                        lkup=weird_lkup))
    checkException(extract_character_from_raw_by_ranges(x, start, width,
                                                        collapse=TRUE,
                                                        lkup=weird_lkup))
    start <- c(6L, 10L, 11L)
    width <- c(2L,  0L,  1L)
    target0 <- substring(rawToChar(x), start, start + width - 1L)
    target1 <- c("uz", "", "u")
    do_tests(x, start, width, target0, weird_lkup, target1)
}

