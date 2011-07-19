require(devtools)
require(network)
require(sna)
require(ergm)
load_all("~/Downloads/ergm")
nw <- as.network(rgraph(20,tprob=.3))
object <- nw ~ edges
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
debug(get.changescore)
eta0 <- rep(0,3);
z <- get.changescore(Clist, MHproposal, eta0, MCMCparams, verbose=verbose)

