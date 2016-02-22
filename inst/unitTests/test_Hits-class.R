test_Hits_coercion <- function() {
  ## sparse
  from <- c(1L, 1L, 3L)
  to <- 1:3
  hits <- Hits(from, to, 3, 3)
  checkIdentical(as.matrix(hits),
                 cbind(queryHits=from,
                       subjectHits=to))
  checkIdentical(as.table(hits), c(2L, 0L, 1L))
  checkIdentical(as.table(t(hits)), c(1L, 1L, 1L))

  ## dense
  from <- rep(1:2, each=2)
  to <- rep(1:2, 2)
  hits <- Hits(from, to, 3, 2)
  checkIdentical(as.matrix(hits),
                 cbind(queryHits=from,
                       subjectHits=to))
  checkIdentical(as.table(hits), c(2L, 2L, 0L))
  checkIdentical(as.table(t(hits)), c(2L, 2L))
}

test_remapHits <- function()
{
    from0 <- c(1L, 1L, 2L, 3L, 3L)
    to0 <- c(1L, 2L, 5L, 2L, 4L)
    hits0 <- Hits(from0, to0, 3L, 6L)

    ## No remapping (i.e. map is missing or is the identity function).
    checkIdentical(remapHits(hits0), hits0)

    Lnodes.remapping1 <- seq_len(nLnode(hits0))
    new.nLnode1 <- nLnode(hits0)
    Rnodes.remapping1 <- seq_len(nRnode(hits0))
    new.nRnode1 <- nRnode(hits0)

    hits10 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping1,
                               new.nLnode=new.nLnode1)
    checkIdentical(hits10, hits0)

    hits01 <- remapHits(hits0, Rnodes.remapping=Rnodes.remapping1,
                               new.nRnode=new.nRnode1)
    checkIdentical(hits01, hits0)

    hits11 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping1,
                               new.nLnode=new.nLnode1,
                               Rnodes.remapping=Rnodes.remapping1,
                               new.nRnode=new.nRnode1)
    checkIdentical(hits11, hits0)

    ## With maps that add a fixed offset to from(x), and a fixed offset
    ## to to(x).
    Lnodes.remapping2 <- Lnodes.remapping1 + 20L
    new.nLnode2 <- new.nLnode1 + 20L
    Rnodes.remapping2 <- Rnodes.remapping1 + 30L
    new.nRnode2 <- new.nRnode1 + 30L

    hits20 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping2,
                               new.nLnode=new.nLnode2)
    expected_hits20 <- Hits(from0 + 20L, to0, 23, 6)
    checkIdentical(hits20, expected_hits20)

    hits02 <- remapHits(hits0, Rnodes.remapping=Rnodes.remapping2,
                               new.nRnode=new.nRnode2)
    expected_hits02 <- Hits(from0, to0 + 30L, 3, 36)
    checkIdentical(hits02, expected_hits02)

    hits22 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping2,
                               new.nLnode=new.nLnode2,
                               Rnodes.remapping=Rnodes.remapping2,
                               new.nRnode=new.nRnode2)
    expected_hits22 <- Hits(from0 + 20L, to0 + 30L, 23, 36)
    checkIdentical(hits22, expected_hits22)

    ## With injective and non-ascending maps.
    Lnodes.remapping3 <- 100L * rev(Lnodes.remapping1) + Lnodes.remapping1
    new.nLnode3 <- 400L
    Rnodes.remapping3 <- 100L * rev(Rnodes.remapping1) + Rnodes.remapping1
    new.nRnode3 <- 700L
    
    hits30 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping3,
                               new.nLnode=new.nLnode3)
    expected_hits30 <- Hits(c(103, 103, 202, 301, 301),
                            c(  2,   4,   5,   1,   2), 400, 6)
    checkIdentical(hits30, expected_hits30)

    hits03 <- remapHits(hits0, Rnodes.remapping=Rnodes.remapping3,
                               new.nRnode=new.nRnode3)
    expected_hits03 <- Hits(from0, c(502, 601, 205, 304, 502), 3, 700)
    checkIdentical(t(hits03), t(expected_hits03))

    hits33 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping3,
                               new.nLnode=new.nLnode3,
                               Rnodes.remapping=Rnodes.remapping3,
                               new.nRnode=new.nRnode3)
    expected_hits33 <- Hits(c(103, 103, 202, 301, 301),
                            c(304, 502, 205, 502, 601), 400, 700)
    checkIdentical(t(hits33), t(expected_hits33))

    ## With non-injective maps (as factors).
    Lnodes.remapping4 <- factor(c("B", "A", "B"), levels=c("A", "B"))
    Rnodes.remapping4 <- factor(c("a", "b", "a", "b", "a", "b"), levels=c("a", "b"))

    hits40 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping4)
    expected_hits40 <- Hits(c(1, 2, 2, 2), c(5, 1, 2, 4), 2, 6)
    checkIdentical(hits40, expected_hits40)

    hits04 <- remapHits(hits0, Rnodes.remapping=Rnodes.remapping4)
    expected_hits04 <- Hits(c(1, 1, 2, 3), c(1, 2, 1, 2), 3, 2)
    checkIdentical(hits04, expected_hits04)

    hits44 <- remapHits(hits0, Lnodes.remapping=Lnodes.remapping4,
                               Rnodes.remapping=Rnodes.remapping4)
    expected_hits44 <- Hits(c(1, 2, 2), c(1, 1, 2), 2, 2)
    checkIdentical(hits44, expected_hits44)
}

