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
