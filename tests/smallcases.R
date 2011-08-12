require(devtools)
require(testthat)
require(network)
require(sna)
require(ergm)
require(networkstats)

test_that("we can perform toggles", {
  nw <- network.initialize(5,directed=FALSE)
  edges <- cbind(c(1,2,3,4,2),c(1,1,1,1,3))
  ns <- network.for.changescores(nw ~ edges + triangles)
  ns <- do.toggles(ns,edges)
  expect_that(network.edgecount(ns$nw),is_equivalent_to(4))
  ns <- do.toggles(ns,edges)  
  expect_that(network.edgecount(ns$nw),is_equivalent_to(0))

  # This example shows a bug in get.changescore
  nw <- network.initialize(5,directed=FALSE)
  edges <- cbind(c(1,1,1,2),c(2,3,5,3))
  ns <- network.for.changescores(nw ~ edges + triangles)
  ns <- do.toggles(ns,edges)
  nedges <- cbind(2,3)
  z <- get.changescore(ns,nedges)
  expect_that(z[1],is_equivalent_to(-1))
  ns <- do.toggles(ns,nedges)
  network.edgecount(ns$nw)
  
  edges <- cbind(c(1,1,1,2),c(2,3,5,3))
  nw <- as.network(edges,directed=FALSE)
  ns <- network.for.changescores(nw ~ edges + triangles)
  nedges <- cbind(2,3)
  z <- get.changescore(ns,nedges)
  expect_that(z[1],is_equivalent_to(-1))
  ns$Clist$dir <-  TRUE
  z <- get.changescore(ns,nedges)
  
  expect_that(network.edgecount(ns$nw),is_equivalent_to(3))  
})

test_that("change score works when removing an edge", {
  nw <- network.initialize(3)
  edges <- cbind(c(1,2,3),c(2,3,1))
  ns <- network.for.changescores(nw ~ edges)
  ns$nw[edges] <- 1
  expect_that(get.changescore(ns,cbind(3,1)),is_equivalent_to(-1))
})

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

test_that("edge and tri change scores match ERGM's globalstats for an undirected example", {
  set.seed(1)
  N <- 20
  nw <- network.initialize(N)
  ns <- network.for.changescores(nw ~ edges + triangles)
  # Create example data
  rg <- rgraph(N,tprob=.15)
#  rg <- 1*((rg + t(rg))>0)
  edges <- as.edgelist(as.network(rg,directed=TRUE))
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

test_that("in place network structure", {
  # Idea: Function for creating network, model data structures in C,
  # which passes back the pointers to these structures.  (Another
  # function exists for tearing down these structures.)  To update we
  # pass a C function the pointers to these structures as well as
  # other arguments for getting changescores.  We'll need getter
  # methods to grab the data.
  nw <- network.initialize(5,directed=FALSE)
  edges <- cbind(c(1,2,3,4,2),c(1,1,1,1,3))
  ns <- network.for.changescores(nw ~ edges + triangles)
  ns <- do.toggles(ns,edges)
  tmp <- get.changescore.network(ns,edges)
  ergm.getglobalstats(ns$nw,ns$m)


})

library(networkstats)
dyn.load("~/Documents/networkstats/src/networkstats.so")

example.return.pointer(5)

body <- paste(readLines("src/example.cpp"),collapse="\n")

body <- '
NumericVector xx(x);
int ii = as<int>(i);
xx = xx * ii;
return( xx );
'

body <- '
using namespace Rcpp;
double norm( double x, double y ) {
  return sqrt( x*x + y*y );
}
RCPP_MODULE(mod) {
  function( "norm", &norm );
}
'

body <- '
const char* hello( std::string who ){
  std::string result( "hello " ) ;
  result += who ;
  return result.c_str() ;
}
double norm( double x, double y ) {
  return sqrt( x*x + y*y );
}
RCPP_MODULE(yada){
  using namespace Rcpp ;
  function( "hello", &hello ) ;
  function("norm",&norm);
}
'

require(inline)
require(Rcpp)
testfun = cxxfunction(signature(x="numeric"), body = body, plugin="Rcpp")
testfun(1:5, 3)

dd3 = modfunction('yada', 'src/example.cpp', plugin='Rcpp', verbose=F)
dd3$norm(5,10)

