test_DataFrameFactor <- function() {
     df <- DataFrame(X=sample(5, 100, replace=TRUE), Y=sample(c("A", "B"), 100, replace=TRUE))

     dffac <- DataFrameFactor(df)
     checkIdentical(dim(dffac), dim(df))
     checkIdentical(dimnames(dffac), dimnames(df))

     checkIdentical(dffac$X, df$X)
     checkIdentical(dffac[,"X"], df$X)

     checkIdentical(unfactor(dffac[,c("Y", "X")]), df[,c("Y", "X")])
     checkIdentical(dffac[1:10,"X"], df$X[1:10])

     # The usual Factor methods may also be used:
     checkIdentical(unfactor(dffac), df)
     checkTrue(!anyDuplicated(levels(dffac)))

     # Check that the row names are registered correctly.
     names(dffac) <- 1:100
     checkIdentical(names(dffac), as.character(1:100))
}
