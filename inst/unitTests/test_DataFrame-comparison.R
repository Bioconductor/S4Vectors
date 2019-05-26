test_DataFrame_comparison <- function() {
    DF <- DataFrame(
        stuff=c("C", "D", "D", "A", "D", "B", "E", "A", "E"),
        things=c(1L, 2L, 1L, 3L, 4L, 2L, 1L, 1L, 2L)
    )

    # Checking basics.
    checkIdentical(order(DF), order(DF$stuff, DF$things))
    checkIdentical(order(DF[,2:1]), order(DF$things, DF$stuff))

    checkIdentical(sameAsLastROW(DF),
        sameAsLastROW(DF$stuff) & sameAsLastROW(DF$things))

    DF0 <- DF[c(1,1,2,3,3,4,4,4,5,5,6,7,8),] # A less trivial example.
    checkIdentical(sameAsLastROW(DF0),
        sameAsLastROW(DF0$stuff) & sameAsLastROW(DF0$things))

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
