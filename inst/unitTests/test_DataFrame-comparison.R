test_DataFrame_comparison <- function() {
    DF <- DataFrame(
        stuff=c("C", "D", "D", "A", "D", "B", "E", "A", "E"),
        things=c(1L, 2L, 1L, 3L, 4L, 2L, 1L, 1L, 2L)
    )

    # Checking basics.
    checkIdentical(order(DF), order(DF$stuff, DF$things))
    checkIdentical(order(DF[,2:1]), order(DF$things, DF$stuff))

    checkIdentical(sameAsPreviousROW(DF),
        sameAsPreviousROW(DF$stuff) & sameAsPreviousROW(DF$things))

    DF0 <- DF[c(1,1,2,3,3,4,4,4,5,5,6,7,8),] # A less trivial example.
    checkIdentical(sameAsPreviousROW(DF0),
        sameAsPreviousROW(DF0$stuff) & sameAsPreviousROW(DF0$things))

    # Checking robustness to internal NAs.
    ids <- c(1:10, 1:10)
    extra <- c(10:1, 10:1)
    ids[1] <- NA
    extra[2] <- NA

    a <- DataFrame(ids, extra)
    checkIdentical(selfmatch(a), c(1:10, 11:12, 3:10))

    # Checking methods to override List behaviour.
    checkIdentical(match(DF, DF), selfmatch(DF))

    keys <- paste0(DF$stuff, ".", DF$things)
    keys0 <- paste0(DF0$stuff, ".", DF0$things)
    checkIdentical(match(DF, DF0), match(keys, keys0))
    checkIdentical(match(DF0, DF), match(keys0, keys))

    DF2 <- DataFrame(
        stuff=c("C", "E", "D", "A", "D", "B", "E", "C", "E"),
        things=c(1L, 2L, 1L, 1L, 4L, 3L, 1L, 1L, 2L)
    )
    keys2 <- paste0(DF2$stuff, ".", DF2$things)
    checkIdentical(pcompare(DF, DF2), pcompare(keys, keys2))
    checkIdentical(pcompare(DF2, DF), -pcompare(keys, keys2))

    checkIdentical(DF==DF, !logical(nrow(DF)))
    checkIdentical(DF<=DF, !logical(nrow(DF)))
}
