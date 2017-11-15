
test_normalizeDoubleBracketSubscript <- function()
{
    ## These "core tests" don't even look at 'x'.
    do_core_tests <- function(x, exact=TRUE) {
        for (i in list(TRUE, FALSE, 1i, as.raw(1),
                       integer(0), 1:3, character(0), c("A", "b"))) {
            checkException(normalizeDoubleBracketSubscript(i, x,
                                                           exact=exact))
            checkException(normalizeDoubleBracketSubscript(Rle(i), x,
                                                           exact=exact))
        }

        for (i in list(NA, NA_integer_, NA_real_, NA_character_, NA_complex_)) {
            checkException(normalizeDoubleBracketSubscript(i, x, exact=exact))
            current <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                                       allow.NA=TRUE)
            checkIdentical(NA, current)
            checkException(normalizeDoubleBracketSubscript(Rle(i), x,
                                                           exact=exact))
            current <- normalizeDoubleBracketSubscript(Rle(i), x, exact=exact,
                                                       allow.NA=TRUE)
            checkIdentical(NA, current)
        }

        ## Error: [[ subscript must be >= 1
        for (i in list(0L, 0.99, -1)) {
            checkException(normalizeDoubleBracketSubscript(i, x,
                                                           exact=exact))
            checkException(normalizeDoubleBracketSubscript(Rle(i), x,
                                                           exact=exact))
            checkException(normalizeDoubleBracketSubscript(i, x,
                                                           exact=exact,
                                                           allow.append=TRUE))
            checkException(normalizeDoubleBracketSubscript(Rle(i), x,
                                                           exact=exact,
                                                           allow.append=TRUE))
        }
    }

    test_invalid_position <- function(i, x, allow.append=FALSE) {
        for (exact in list(TRUE, FALSE)) {
            for (allow.NA in list(FALSE, TRUE)) {
                for (allow.nomatch in list(FALSE, TRUE)) {
                    checkException(normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=allow.nomatch))
                    checkException(normalizeDoubleBracketSubscript(Rle(i), x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=allow.nomatch))
                }
            }
        }
    }

    test_valid_position <- function(i, x, target, allow.append=FALSE) {
        for (exact in list(TRUE, FALSE)) {
            for (allow.NA in list(FALSE, TRUE)) {
                for (allow.nomatch in list(FALSE, TRUE)) {
                    current <- normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=allow.nomatch)
                    checkIdentical(target, current)
                    current <- normalizeDoubleBracketSubscript(Rle(i), x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=allow.nomatch)
                    checkIdentical(target, current)
                }
            }
        }
    }

    test_invalid_name <- function(name, x, exact=TRUE) {
        for (i in list(name, Rle(name), factor(name), Rle(factor(name)))) {
            for (allow.append in list(FALSE, TRUE)) {
                for (allow.NA in list(FALSE, TRUE)) {
                    checkException(normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA))
                    checkException(normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=FALSE))
                    current <- normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=TRUE)
                    checkIdentical(NA, current)
                }
            }
        }
    }

    test_valid_name <- function(name, x, target, exact=TRUE) {
        for (i in list(name, Rle(name), factor(name), Rle(factor(name)))) {
            for (allow.append in list(FALSE, TRUE)) {
                for (allow.NA in list(FALSE, TRUE)) {
                    for (allow.nomatch in list(FALSE, TRUE)) {
                        current <- normalizeDoubleBracketSubscript(i, x,
                                              exact=exact,
                                              allow.append=allow.append,
                                              allow.NA=allow.NA,
                                              allow.nomatch=allow.nomatch)
                        checkIdentical(target, current)
                    }
                }
            }
        }
    }

    ## ----------------------------------------------------------------- ##

    x <- list()
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## (1) With a single non-NA number.

    ## Error: subscript is out of bounds
    test_invalid_position(1L, x, allow.append=FALSE)
    test_invalid_position(1, x, allow.append=FALSE)

    test_valid_position(1L, x, 1L, allow.append=TRUE)
    test_valid_position(1.99, x, 1L, allow.append=TRUE)

    ## Error: [[ subscript must be <= length(x) + 1
    test_invalid_position(2L, x, allow.append=TRUE)
    test_invalid_position(2, x, allow.append=TRUE)

    ## (2) With a single non-NA string.

    test_invalid_name("A", x, exact=TRUE)
    test_invalid_name("A", x, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    names(x) <- character(0)
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## With a single non-NA string.

    test_invalid_name("A", x, exact=TRUE)
    test_invalid_name("A", x, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    x <- as.list(letters[1:9])
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## (1) With a single non-NA number.

    test_valid_position(1L, x, 1L, allow.append=FALSE)
    test_valid_position(1L, x, 1L, allow.append=TRUE)
    test_valid_position(1.99, x, 1L, allow.append=FALSE)
    test_valid_position(1.99, x, 1L, allow.append=TRUE)

    test_valid_position(9L, x, 9L, allow.append=FALSE)
    test_valid_position(9L, x, 9L, allow.append=TRUE)
    test_valid_position(9.99, x, 9L, allow.append=FALSE)
    test_valid_position(9.99, x, 9L, allow.append=TRUE)

    ## Error: subscript is out of bounds
    test_invalid_position(10L, x, allow.append=FALSE) 
    test_invalid_position(10.99, x, allow.append=FALSE) 

    test_valid_position(10L, x, 10L, allow.append=TRUE)
    test_valid_position(10.99, x, 10L, allow.append=TRUE)

    ## Error: [[ subscript must be <= length(x) + 1
    test_invalid_position(11L, x, allow.append=TRUE)
    test_invalid_position(11, x, allow.append=TRUE)

    ## (2) With a single non-NA string.

    test_invalid_name("A", x, exact=TRUE)
    test_invalid_name("A", x, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    names(x) <- c("C", "AA", "BB", "A", "", "A", "AA", "BB", "DD")
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## Exact matching.

    test_invalid_name("Z", x, exact=TRUE)
    test_invalid_name("B", x, exact=TRUE)
    test_invalid_name("D", x, exact=TRUE)

    test_valid_name("C", x, 1L, exact=TRUE)
    test_valid_name("BB", x, 3L, exact=TRUE)
    test_valid_name("A", x, 4L, exact=TRUE)
    test_valid_name("AA", x, 2L, exact=TRUE)
    test_valid_name("DD", x, 9L, exact=TRUE)

    ## Partial matching.

    test_invalid_name("Z", x, exact=FALSE)
    test_invalid_name("B", x, exact=FALSE)  # ambiguous partial matching

    test_valid_name("C", x, 1L, exact=FALSE)
    test_valid_name("BB", x, 3L, exact=FALSE)
    test_valid_name("A", x, 4L, exact=FALSE)
    test_valid_name("AA", x, 2L, exact=FALSE)
    test_valid_name("DD", x, 9L, exact=FALSE)
    test_valid_name("D", x, 9L, exact=FALSE)
}

test_getListElement_list <- function()
{
    ## These "core tests" don't even look at 'x'.
    do_core_tests <- function(x, exact=TRUE) {
        for (i in list(TRUE, FALSE, 1i, as.raw(1),
                       integer(0), 1:3, character(0), c("A", "b"))) {
            checkException(getListElement(x, i, exact=exact))
            checkException(getListElement(x, Rle(i), exact=exact))
        }

        for (i in list(NA, NA_integer_, NA_real_, NA_character_, NA_complex_)) {
            current <- getListElement(x, i, exact=exact)
            checkIdentical(NULL, current)
            current <- getListElement(x, Rle(i), exact=exact)
            checkIdentical(NULL, current)
        }

        ## Error: [[ subscript must be >= 1
        for (i in list(0L, 0.99, -1)) {
            checkException(getListElement(x, i, exact=exact))
            checkException(getListElement(x, Rle(i), exact=exact))
        }
    }

    test_invalid_position <- function(x, i) {
        for (exact in list(TRUE, FALSE)) {
            checkException(getListElement(x, i, exact=exact))
            checkException(getListElement(x, Rle(i), exact=exact))
        }
    }

    test_valid_position <- function(x, i, target) {
        for (exact in list(TRUE, FALSE)) {
            current <- getListElement(x, i, exact=exact)
            checkIdentical(target, current)
            current <- getListElement(x, Rle(i), exact=exact)
            checkIdentical(target, current)
        }
    }

    test_valid_name <- function(x, name, target, exact=TRUE) {
        for (i in list(name, Rle(name), factor(name), Rle(factor(name)))) {
            current <- getListElement(x, i, exact=exact)
            checkIdentical(target, current)
        }
    }

    ## ----------------------------------------------------------------- ##

    x <- list()
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## (1) With a single non-NA number.

    ## Error: subscript is out of bounds
    test_invalid_position(x, 1L)
    test_invalid_position(x, 1)

    ## (2) With a single non-NA string.

    test_valid_name(x, "A", NULL, exact=TRUE)
    test_valid_name(x, "A", NULL, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    names(x) <- character(0)
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## With a single non-NA string.

    test_valid_name(x, "A", NULL, exact=TRUE)
    test_valid_name(x, "A", NULL, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    x <- as.list(letters[1:9])
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## (1) With a single non-NA number.

    test_valid_position(x, 1L, "a")
    test_valid_position(x, 1.99, "a")

    test_valid_position(x, 9L, "i")
    test_valid_position(x, 9.99, "i")

    test_invalid_position(x, 10L)
    test_invalid_position(x, 10)
    test_invalid_position(x, 10.99)

    ## (2) With a single non-NA string.

    test_valid_name(x, "A", NULL, exact=TRUE)
    test_valid_name(x, "A", NULL, exact=FALSE)

    ## ----------------------------------------------------------------- ##

    names(x) <- c("C", "AA", "BB", "A", "", "A", "AA", "BB", "DD")
    do_core_tests(x, exact=TRUE)
    do_core_tests(x, exact=FALSE)

    ## Exact matching.

    test_valid_name(x, "Z", NULL, exact=TRUE)
    test_valid_name(x, "B", NULL, exact=TRUE)
    test_valid_name(x, "D", NULL, exact=TRUE)

    test_valid_name(x, "C", "a", exact=TRUE)
    test_valid_name(x, "BB", "c", exact=TRUE)
    test_valid_name(x, "A", "d", exact=TRUE)
    test_valid_name(x, "AA", "b", exact=TRUE)
    test_valid_name(x, "DD", "i", exact=TRUE)

    ## Partial matching.
 
    test_valid_name(x, "Z", NULL, exact=FALSE)
    test_valid_name(x, "B", NULL, exact=FALSE)  # ambiguous partial matching

    test_valid_name(x, "C", "a", exact=FALSE)
    test_valid_name(x, "BB", "c", exact=FALSE)
    test_valid_name(x, "A", "d", exact=FALSE)
    test_valid_name(x, "AA", "b", exact=FALSE)
    test_valid_name(x, "DD", "i", exact=FALSE)
    test_valid_name(x, "D", "i", exact=FALSE)
}

test_setListElement_list <- function()
{
    ## These "core tests" don't even look at 'x' or 'value'.
    do_core_tests <- function(x, value) {
        for (i in list(TRUE, FALSE, 1i, as.raw(1),
                       integer(0), 1:3, character(0), c("A", "b"))) {
            checkException(setListElement(x, i, value))
            checkException(setListElement(x, Rle(i), value))
        }

        for (i in list(NA, NA_integer_, NA_real_, NA_character_, NA_complex_)) {
            checkException(setListElement(x, i, value))
            checkException(setListElement(x, Rle(i), value))
        }

        ## Error: [[ subscript must be >= 1
        for (i in list(0L, 0.99, -1)) {
            checkException(setListElement(x, i, value))
            checkException(setListElement(x, Rle(i), value))
        }
    }

    ## Does not look at 'value'.
    test_invalid_position <- function(x, i, value) {
        checkException(setListElement(x, i, value))
        checkException(setListElement(x, Rle(i), value))
    }

    test_valid_position <- function(x, i, value, target) {
        current <- setListElement(x, i, value)
        checkIdentical(target, current)
        current <- setListElement(x, Rle(i), value)
        checkIdentical(target, current)
    }

    test_valid_name <- function(x, name, value, target) {
        for (i in list(name, Rle(name), factor(name), Rle(factor(name)))) {
            current <- setListElement(x, i, value)
            checkIdentical(target, current)
        }
    }

    ## ----------------------------------------------------------------- ##

    x <- list()
    value97 <- 9:7
    do_core_tests(x, NULL)
    do_core_tests(x, value97)

    ## (1) With a single non-NA number.

    ## No-op
    test_valid_position(x, 1L, NULL, x)
    test_valid_position(x, 1, NULL, x)
    test_valid_position(x, 1.99, NULL, x)

    ## Append naked 'value' to 'x'.
    target <- c(x, list(value97))
    test_valid_position(x, 1L, value97, target)
    test_valid_position(x, 1, value97, target)
    test_valid_position(x, 1.99, value97, target)

    ## Error: [[ subscript must be <= length(x) + 1
    test_invalid_position(x, 2L, NULL)
    test_invalid_position(x, 2, value97)

    ## (2) With a single non-NA string.

    test_valid_name(x, "A", NULL, x)  # no-op
    target <- c(x, list(A=value97))  # append named 'value' to 'x'
    test_valid_name(x, "A", value97, target)

    ## ----------------------------------------------------------------- ##

    names(x) <- character(0)
    do_core_tests(x, NULL)
    do_core_tests(x, value97)

    ## With a single non-NA string.

    test_valid_name(x, "A", NULL, x)  # no-op
    target <- c(x, list(A=value97))  # append named 'value' to 'x'
    test_valid_name(x, "A", value97, target)

    ## ----------------------------------------------------------------- ##

    x <- as.list(letters[1:9])
    do_core_tests(x, NULL)
    do_core_tests(x, value97)

    ## (1) With a single non-NA number.

    target <- tail(x, n=-1)  # remove 1st list element
    test_valid_position(x, 1L, NULL, target)
    test_valid_position(x, 1.99, NULL, target)

    target <- c(list(value97), tail(x, n=-1))  # replace 1st list element
    test_valid_position(x, 1L, value97, target)
    test_valid_position(x, 1.99, value97, target)

    target <- head(x, n=-1)  # remove last list element
    test_valid_position(x, 9L, NULL, target)
    test_valid_position(x, 9.99, NULL, target)

    target <- c(head(x, n=-1), list(value97))  # replace last list element
    test_valid_position(x, 9L, value97, target)
    test_valid_position(x, 9.99, value97, target)

    ## No-op
    test_valid_position(x, 10L, NULL, x)
    test_valid_position(x, 10, NULL, x)
    test_valid_position(x, 10.99, NULL, x)

    ## Append naked 'value' to 'x'.
    target <- c(x, list(value97))
    test_valid_position(x, 10L, value97, target)
    test_valid_position(x, 10, value97, target)
    test_valid_position(x, 10.99, value97, target)

    ## Error: [[ subscript must be <= length(x) + 1
    test_invalid_position(x, 11L, NULL)
    test_invalid_position(x, 11, value97)

    ## (2) With a single non-NA string.

    test_valid_name(x, "A", NULL, x)  # no-op
    target <- c(x, list(A=value97))  # append named 'value' to 'x'
    test_valid_name(x, "A", value97, target)

    ## ----------------------------------------------------------------- ##

    names(x) <- c("C", "AA", "BB", "A", "", "A", "AA", "BB", "DD")
    do_core_tests(x, NULL)
    do_core_tests(x, value97)

    ## No-op
    test_valid_name(x, "Z", NULL, x)
    test_valid_name(x, "B", NULL, x)
    test_valid_name(x, "D", NULL, x)

    ## Append named 'value' to 'x'
    test_valid_name(x, "Z", value97, c(x, list(Z=value97)))
    test_valid_name(x, "B", value97, c(x, list(B=value97)))
    test_valid_name(x, "D", value97, c(x, list(D=value97)))

    ## Remove named list element
    test_valid_name(x, "C", NULL, x[-1])
    test_valid_name(x, "BB", NULL, x[-3])
    test_valid_name(x, "A", NULL, x[-4])
    test_valid_name(x, "AA", NULL, x[-2])
    test_valid_name(x, "DD", NULL, x[-9])

    ## Replace named list element
    test_valid_name(x, "C", value97, `[[<-`(x, "C", value97))
    test_valid_name(x, "BB", value97, `[[<-`(x, "BB", value97))
    test_valid_name(x, "A", value97, `[[<-`(x, "A", value97))
    test_valid_name(x, "AA", value97, `[[<-`(x, "AA", value97))
    test_valid_name(x, "DD", value97, `[[<-`(x, "DD", value97))
}

