library(Rcpp)
library(inline)
source("R/utils.R")
m <- modfunction('yada', 'src/inplace.example.cpp', plugin='Rcpp', verbose=F)
u <- modfunction('unif_module', 'src/inplace.example.cpp', plugin='Rcpp', verbose=F)

m$norm(5,10)
Uniform <- u$Uniform
u <- new( Uniform, 0, 10 )
u$draw( 10L )
u$range()
u$max <- 1
u$range()
u$draw( 10 )

inc <- '#include <netstats.h>'
u <- modfunction('unif_module','src/inplace.cpp',includes=inc,plugin='Rcpp', verbose=TRUE)
