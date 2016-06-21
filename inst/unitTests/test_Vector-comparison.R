test_Vector_merge <- function() {
    gr <- GRanges(c("chr1:1-1000", "chr1:2000-3000"), a=1:2, b=2:1)
    gr2 <- GRanges(c("chr1:1-1000", "chr1:2000-3000"), b=c(1,3), c=c(3,1))
    exp <- granges(gr)
    mcols(exp) <- DataFrame(a=1:2, b=2:1, b.1=c(1,3), c=c(3,1))
    test <- merge(gr, gr2)
    checkIdentical(test, exp)
        
    gr <- GRanges(c("chr1:1-1000", "chr1:2000-3000", "chr1:1-10"),
                  a=1:3, b=c(2,1,3))
    exp <- granges(gr2)
    mcols(exp) <- DataFrame(a=1:2, b=2:1, b.1=c(1,3), c=c(3,1))
    test <- merge(gr, gr2)
    checkEquals(test, exp)

    exp <- sort(granges(gr))
    mcols(exp) <- DataFrame(a=c(3L,1L,2L), b=c(3L,2L,1L), b.1=c(NA,1,3),
                            c=c(NA,3,1))
    test <- merge(gr, gr2, all.x=TRUE)
    checkEquals(test, exp)
        
    gr2 <- GRanges(c("chr1:1-1000", "chr1:2000-3000", "chr2:1-10"),
                   b=c(1,3,2), c=c(3,1,2))
    exp <- sort(c(granges(gr), granges(gr2[3])))
    mcols(exp) <- DataFrame(a=c(3L,1L,2L, NA), b=c(3L,2L,1L, NA),
                            b.1=c(NA,1,3,2), c=c(NA,3,1,2))
    test <- merge(gr, gr2, all=TRUE)
    checkEquals(test, exp)
}
