require(devtools)
require(network)
require(sna)
require(ergm)
load_all("~/Downloads/ergm")
set.seed(1)
nw <- as.network(rgraph(20,tprob=.3),directed=FALSE)
object <- nw ~ edges + triangles 
ns <- network.for.changescores(object)

# Testing
ns$nw <- network.initialize(10)
toggle.edges <- cbind(c(1,2,3),c(2,3,1))
ergm.getglobalstats(ns$nw,ns$m)
z <- get.changescore(ns,toggle.edges)
ns$nw <- as.network(toggle.edges)
ergm.getglobalstats(ns$nw,ns$m)

# Timing experiments
toggle.edges <- cbind(sample(1:1000),sample(1:1000))
nw <- as.network(toggle.edges)
ns <- network.for.changescores(nw ~ edges + triangles)
for (i in 1:1000) {
  ergm.getglobalstats(ns$nw,ns$m)
}
z <- get.changescore(ns,toggle.edges)
