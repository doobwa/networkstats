require(pkg)
library(inline)
library(network)
require(sna)
require(ergm)
source('R/utils.R')

r <- Module("yada")
r$bla()
s <- new(r)

inc <- paste(readLines('tests/convertCharExample.txt.cpp'),collapse="\n")
fx <- cxxfunction( signature(), "" , include = inc, plugin = "Rcpp" )
a <- Module( "foo_mod", getDynLib(fx) )
b <- new(a$Foo,1:5)
b$convertExample()

inc <- paste(readLines('tests/ChangeScoreNetwork.txt.cpp'),collapse="\n")
fx <- cxxfunction( signature(), "" , include = inc, plugin = "Rcpp" )
a <- Module( "change_score_network2", getDynLib(fx) )

csn <- new(a$ChangeScoreNetwork2,1:5,10:20)

#nw <- network.initialize(5,directed=FALSE)
source("R/networkcs.R")
edges <- cbind(c(1,2,3,4,2),c(1,1,1,1,3))
nw <- as.network(edges)
csn.stuff <- network.for.changescores(nw ~ edges + triangles)

edgelist <- as.edgelist(csn.stuff$nw)
tails <- edgelist[,2]
heads <- edgelist[,1]
nedges <- nrow(edgelist)

csn$initializeNetwork(as.integer(tails), as.integer(heads),as.integer(nedges),
                      as.integer(csn.stuff$Clist$n),as.integer(csn.stuff$Clist$dir),
                      as.integer(csn.stuff$Clist$bipartite))
csn$getNumEdges()

csn$toggleEdge(1,2)
csn$getNumEdges()

funnames <- csn.stuff$Clist$fnamestring
sonames <-  csn.stuff$Clist$snamestring
inputs <-   csn.stuff$Clist$inputs
nterms <-   csn.stuff$Clist$nterms

csn$initializeModel(as.character(funnames),as.character(sonames),
                    as.double(inputs),as.integer(nterms))
