##' 
##'
##' ##' @title Create an object that allows for online updating of network statistics.
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
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Compute the changescore for the given networkcs object and
##' the provided edge toggles.
##' @param ncs networkcs object
##' @param toggle.edges M x 2 matrix of edge toggles to perform
##' @return a vector of statistics (with an element for each term in the provided model.
##' @author Chris DuBois
get.changescore <- function(ncs,toggle.edges) {
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
  as.double(Clist$inputs),
  stats = as.double(stats),
  as.integer(MCMCparams$samplesize),
  # The line below was changed as of version 2.2-3.  Now, the statsmatrix is 
  # initialized to zero instead of allowing the first row to be nonzero, then 
  # adding this first row to each row within MCMC_wrapper.
  # This is worth it:  There is no reason
  # that MCMCparams should include a huge matrix.
  statsmatrix = double(MCMCparams$samplesize * Clist$nstats),  # sample
  #  statsmatrix = as.double(t(MCMCparams$stats)), # By default, as.double goes bycol, not byrow; thus, we use the transpose here.
  as.integer(MCMCparams$burnin),
  as.integer(MCMCparams$interval),
  newnwtails = integer(MCMCparams$maxedges),
  newnwheads = integer(MCMCparams$maxedges),
  as.integer(verbose),
  as.integer(maxedges),
  PACKAGE="ergm")

  nedges <- z$newnwtails[1]  # This tells how many new edges there are
  newedgelist <- cbind(z$newnwtails[2:(nedges+1)], z$newnwheads[2:(nedges+1)])

  return(z$stats)
}

