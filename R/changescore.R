dyn.load("~/Documents/networkstats/src/networkstats.so")


##' (decription will go here)
##'
##' @title Create an object that allows for online updating of network statistics.
##' @param object ergm.formula object
##' @return a networkcs object (short for "changescore")
##' @author Chris DuBois
network.for.changescores <- function(object) {
  nw <- ergm.getnetwork(object)
  basis <- nw
  form <- ergm.update.formula(object, basis ~ .)
  m <- ergm.getmodel(form, basis, drop=FALSE)
  Clist <- ergm.Cprepare(basis, m)
  constraints <- ~.
  control <- control.ergm()
  MHproposal <- MHproposal(constraints,arguments=control$prop.args, nw=nw, model=m, weights=control$prop.weights, class="c")  
  theta0 <- rep(0,Clist$nstats)
  verbose <- TRUE
  eta0 <- ergm.eta(theta0, m$etamap)
                                        # Create vector of current statistics
  curstats<-summary(form)
  names(curstats) <- m$coef.names
                                        # prepare MCMCparams object
  burnin <- 1
  interval <- 1
  nsim <- 10
  MCMCparams <- list(samplesize=1,
                     maxedges = 1+max(control$maxedges, Clist$nedges),
                     burnin=burnin,
                     interval=interval,
                     parallel=control$parallel,
                     packagenames=control$packagenames,
                     Clist.miss=ergm.design(nw, m, verbose=verbose))
  MCMCparams$samplesize <- nsim
  x <- list(nw=nw,m=m,Clist=Clist,MHproposal=MHproposal,MCMCparams=MCMCparams)
  class(x) <- c("networkcs")
  return(x)
}

## do.toggles <- function(ncs,toggle.edges) {
##   Clist <- ncs$Clist
##   MHproposal <- ncs$MHproposal
##   MCMCparams <- ncs$MCMCparams
##   maxedges <- MCMCparams$maxedges
##   verbose <- FALSE
##   edgelist <- as.edgelist(ncs$nw)
##   tails <- edgelist[,2]
##   heads <- edgelist[,1]
##   nedges <- nrow(edgelist)
##   toggletails <- toggle.edges[,2]
##   toggleheads <- toggle.edges[,1]
##   ntoggles <- nrow(toggle.edges)
##   stats <- rep(0,Clist$nterms)
##   z <- .C("do_toggles",
##           as.integer(nedges),as.integer(tails), as.integer(heads),
##           as.integer(ntoggles),as.integer(toggletails), as.integer(toggleheads),
##           as.integer(Clist$n),
##           as.integer(Clist$dir), as.integer(Clist$bipartite),
##           newnwtails = integer(MCMCparams$maxedges),
##           newnwheads = integer(MCMCparams$maxedges),
##           PACKAGE="networkstats")
## }

##' (description will go here)
##'
##' @title Compute the changescore for the given networkcs object and
##' the provided edge toggles.
##' @param ncs networkcs object
##' @param toggle.edges M x 2 matrix of edge toggles to perform
##' @return a vector of statistics (with an element for each term in the provided model.
##' @author Chris DuBois
get.changescore <- function(ncs,toggle.edges) {
  tmp <- 10
  Clist <- ncs$Clist
  MHproposal <- ncs$MHproposal
  MCMCparams <- ncs$MCMCparams
  maxedges <- MCMCparams$maxedges
  verbose <- FALSE
  edgelist <- as.edgelist(ncs$nw)
  tails <- edgelist[,2]
  heads <- edgelist[,1]
  nedges <- nrow(edgelist)
  toggletails <- toggle.edges[,2]
  toggleheads <- toggle.edges[,1]
  ntoggles <- nrow(toggle.edges)
  stats <- rep(0,Clist$nterms)
  # *** don't forget, tails is now passed in before heads.
  z <- .C("changescore",
          as.integer(length(nedges)),
          as.integer(nedges),as.integer(tails), as.integer(heads),
          as.integer(ntoggles),as.integer(toggletails), as.integer(toggleheads),
          as.integer(Clist$maxpossibleedges), as.integer(Clist$n),
          as.integer(Clist$dir), as.integer(Clist$bipartite),
          as.integer(Clist$nterms),
          as.character(Clist$fnamestring),
          as.character(Clist$snamestring),
          as.character(MHproposal$name), as.character(MHproposal$package),
          as.double(Clist$inputs),
          stats = as.double(stats),
          as.integer(MCMCparams$samplesize),
          statsmatrix = double(MCMCparams$samplesize * Clist$nstats),  # sample
          as.integer(MCMCparams$burnin),
          as.integer(MCMCparams$interval),
          newnwtails = integer(MCMCparams$maxedges),
          newnwheads = integer(MCMCparams$maxedges),
          as.integer(verbose),
          as.integer(MHproposal$bd$attribs),
          as.integer(MHproposal$bd$maxout),
          as.integer(MHproposal$bd$maxin),
          as.integer(MHproposal$bd$minout),
          as.integer(MHproposal$bd$minin),
          as.integer(MHproposal$bd$condAllDegExact),
          as.integer(length(MHproposal$bd$attribs)),
          as.integer(maxedges),
          PACKAGE="networkstats")

  nedges <- z$newnwtails[1]  # This tells how many new edges there are
  newedgelist <- cbind(z$newnwtails[2:(nedges+1)], z$newnwheads[2:(nedges+1)])

  return(z$stats)
}

##' (description will go here)
##'
##' @title Compute the changescore for the given networkcs object and
##' the provided edge toggles.
##' @param ncs networkcs object
##' @param toggle.edges M x 2 matrix of edge toggles to perform
##' @return a vector of statistics (with an element for each term in the provided model.
##' @author Chris DuBois

do.toggles <- function(ncs,edges) {
  if (nrow(edges)==1) {
    a <- ncs$nw[edges[1],edges[2]]
    if (a==1) ncs$nw[edges[1],edges[2]] <- 0
    else ncs$nw[edges[1],edges[2]] <- 1
  } else {
    ncs$nw[edges] <- 1 - ncs$nw[edges]
  }
  return(ncs)
}
  

## do.toggles <- function(ncs,toggle.edges) {
##   Clist <- ncs$Clist
##   MHproposal <- ncs$MHproposal
##   MCMCparams <- ncs$MCMCparams
##   maxedges <- MCMCparams$maxedges
##   verbose <- FALSE
##   edgelist <- as.edgelist(ncs$nw)
##   tails <- edgelist[,2]
##   heads <- edgelist[,1]
##   nedges <- nrow(edgelist)
##   toggletails <- toggle.edges[,2]
##   toggleheads <- toggle.edges[,1]
##   ntoggles <- nrow(toggle.edges)
##   stats <- rep(0,Clist$nterms)
##   newtails <- tails
##   newheads <- heads
##   # *** don't forget, tails is now passed in before heads.
##   z <- .C("dotoggles",
##           as.integer(length(nedges)),
##           as.integer(nedges),as.integer(tails), as.integer(heads),
##           as.integer(ntoggles),as.integer(toggletails), as.integer(toggleheads),
##           as.integer(Clist$maxpossibleedges),
##           as.integer(newtails),
##           as.integer(newheads),
##           as.integer(Clist$n),
##           as.integer(Clist$dir), as.integer(Clist$bipartite),
##           PACKAGE="networkstats")
##   return(invisible(NULL))
## }

get.changescore.network <- function(ncs,toggle.edges) {
  tmp <- 10
  Clist <- ncs$Clist
  MHproposal <- ncs$MHproposal
  MCMCparams <- ncs$MCMCparams
  maxedges <- MCMCparams$maxedges
  verbose <- FALSE
  edgelist <- as.edgelist(ncs$nw)
  tails <- edgelist[,2]
  heads <- edgelist[,1]
  nedges <- nrow(edgelist)
  toggletails <- toggle.edges[,2]
  toggleheads <- toggle.edges[,1]
  ntoggles <- nrow(toggle.edges)
  stats <- rep(0,Clist$nterms)
  # *** don't forget, tails is now passed in before heads.
  z <- .C("changescorenetwork",
          as.integer(length(nedges)),
          as.integer(nedges),as.integer(tails), as.integer(heads),
          as.integer(ntoggles),as.integer(toggletails), as.integer(toggleheads),
          as.integer(Clist$maxpossibleedges), as.integer(Clist$n),
          as.integer(Clist$dir), as.integer(Clist$bipartite),
          as.integer(Clist$nterms),
          as.character(Clist$fnamestring),
          as.character(Clist$snamestring),
          as.character(MHproposal$name), as.character(MHproposal$package),
          as.double(Clist$inputs),
          stats = as.double(stats),
          as.integer(MCMCparams$samplesize),
          statsmatrix = double(MCMCparams$samplesize * Clist$nstats),  # sample
          as.integer(MCMCparams$burnin),
          as.integer(MCMCparams$interval),
          newnwtails = integer(MCMCparams$maxedges),
          newnwheads = integer(MCMCparams$maxedges),
          as.integer(verbose),
          as.integer(MHproposal$bd$attribs),
          as.integer(MHproposal$bd$maxout),
          as.integer(MHproposal$bd$maxin),
          as.integer(MHproposal$bd$minout),
          as.integer(MHproposal$bd$minin),
          as.integer(MHproposal$bd$condAllDegExact),
          as.integer(length(MHproposal$bd$attribs)),
          as.integer(maxedges),
          PACKAGE="networkstats")

  nedges <- z$newnwtails[1]  # This tells how many new edges there are
  newedgelist <- cbind(z$newnwtails[2:(nedges+1)], z$newnwheads[2:(nedges+1)])

  return(z)
}

example.return.pointer <- function(b) {
    z <- .C("ExampleReturnPointer",as.integer(b), PACKAGE="networkstats")
    return(z)
}
