require(pkg)
library(inline)
source('R/utils.R')
inc <- '#include "/home/chris/Documents/networkstats/pkg/src/edgetree.h"'
a <- modfunction('change_score_network2', 'tests/ChangeScoreNetwork.txt.cpp',
                 includes=inc, plugin='Rcpp', verbose=F)

nw <- new(a$ChangeScoreNetwork2,1:5,10:20)
nw$currentEdgelist()
nw <- new(a$ChangeScoreNetwork2,5,c(5,5))
nw$currentEdgelist()
nw$greet()

nw$set("hello everybody")
nw$greet()
