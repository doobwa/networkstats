library(networkstats)
dyn.load("src/networkstats.so")
x <- .Call("create",list("blah",1:5))
.Call("get",x)
.Call("set",x,"new words")
.Call("get",x)
x <- NULL

x <- .Call("createNetwork",5,1:5,6:10,10,1,0)
.Call("getNetworkEdges",x)

z <- .C("changescore",
        as.integer(length(nedges)),
        as.integer(nedges),as.integer(tails), as.integer(heads),
        as.integer(ntoggles),as.integer(toggletails), as.integer(toggleheads),
        as.integer(Clist$maxpossibleedges), as.integer(Clist$n),

