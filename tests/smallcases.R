require(devtools)
require(testthat)
require(network)
require(sna)
require(networkstats)

test_that("change score method works on a simple triangle example", {
  nw <- network.initialize(3)
  ns <- network.for.changescores(nw ~ edges + triangles)
  edges <- cbind(c(1,2,3),c(2,3,1))
  z <- get.changescore(ns,edges)
  expect_that(c(3,1), is_equivalent_to(z))
})

test_that("change score method still correct when adding, then subtracting", {
  nw <- network.initialize(3)
  ns <- network.for.changescores(nw ~ edges + triangles)
  edges <- cbind(c(1,2,3),c(2,3,1))
  edges <- rbind(edges,edges)
  z <- get.changescore(ns,edges)
  expect_that(c(0,0), is_equivalent_to(z))
})

test_that("change score works with an initial network",{
  edges <- cbind(c(1,2,3),c(2,3,1))
  nw <- as.network(edges)
  ns <- network.for.changescores(nw ~ edges + triangles)
  oldstat <- ergm.getglobalstats(ns$nw,ns$m)
  edges <- cbind(2,1)
  z <- get.changescore(ns,edges)
  ns$nw[2,1] <- 1
  newstat <- ergm.getglobalstats(ns$nw,ns$m)
  expect_that(newstat-oldstat, is_equivalent_to(z))
  # Delete same edge that we just added
  z <- get.changescore(ns,edges)
  ns$nw[2,1] <- 0
  oldstat <- newstat
  newstat <- ergm.getglobalstats(ns$nw,ns$m)
  expect_that(newstat-oldstat, is_equivalent_to(z))
})

test_that("edge and tri change scores match ERGM's globalstats for a quick example", {
  set.seed(1)
  N <- 20
  nw <- network.initialize(N)
  ns <- network.for.changescores(nw ~ edges + triangles)
  # Create example data
  rg <- rgraph(N,tprob=.3)
  edges <- as.edgelist(as.network(rg))
  # Check correctness after each edge entry
  oldstat <- ergm.getglobalstats(ns$nw,ns$m)
  for (i in 1:nrow(edges)) {
    e <- edges[i,,drop=FALSE]
    z <- get.changescore(ns,e)
    ns$nw[e[1],e[2]] <- 1
    newstat <- ergm.getglobalstats(ns$nw,ns$m)
    expect_that(newstat-oldstat, is_equivalent_to(z))
    oldstat <- newstat
  }
})

