test_extract_raw_ranges_as_character <- function()
{
    extract_raw_ranges_as_character <-
        S4Vectors:::extract_raw_ranges_as_character

    do_tests <- function(x, start, width, target0, lkup, target1) {
        current <- extract_raw_ranges_as_character(x, start, width)
        checkIdentical(target0, current)

        current <- extract_raw_ranges_as_character(x, start, width,
                                                   collapse=TRUE)
        target <- paste0(target0, collapse="")
        checkIdentical(target, current)

        current <- extract_raw_ranges_as_character(x, start, width, lkup=lkup)
        checkIdentical(target1, current)

        current <- extract_raw_ranges_as_character(x, start, width,
                                                   collapse=TRUE, lkup=lkup)
        target <- paste0(target1, collapse="")
        checkIdentical(target, current)
    }

    x <- charToRaw("ABCDEFAAA")
    lkup <- c(rep.int(NA_integer_, 65L), 122:117)

    start <- width <- integer(0)
    target0 <- target1 <- character(0)
    do_tests(x, start, width, target0, lkup, target1)

    start <- c(6L, 10L, 1L)
    width <- c(2L,  0L, 9L)
    target0 <- substring(rawToChar(x), start, start + width - 1L)
    target1 <- c("uz", "", "zyxwvuzzz")
    do_tests(x, start, width, target0, lkup, target1)

    start <- seq_along(x)
    width <- rep.int(1L, length(x))
    target0 <- safeExplode(rawToChar(x))
    target1 <- c("z", "y", "x", "w", "v", "u", "z", "z", "z")
    do_tests(x, start, width, target0, lkup, target1)

    ## Error when too many characters to read.
    xx <- rep.int(x, 1e6)
    start <- rep.int(1L, 239)
    width <- rep.int(length(xx), 239)
    checkException(extract_raw_ranges_as_character(xx, start, width,
                                                   collapse=TRUE))

    ## With byte not in lookup table.
    x <- charToRaw("ABCDEFAAAGF")  # 'G' is not represented in 'lkup'
    start <- c(6L, 10L, 9L)
    width <- c(2L,  0L, 3L)
    checkException(extract_raw_ranges_as_character(x, start, width,
                                                   lkup=lkup))
    checkException(extract_raw_ranges_as_character(x, start, width,
                                                   collapse=TRUE, lkup=lkup))
    start <- c(6L, 10L, 11L)
    width <- c(2L,  0L,  1L)
    target0 <- substring(rawToChar(x), start, start + width - 1L)
    target1 <- c("uz", "", "u")
    do_tests(x, start, width, target0, lkup, target1)
}

