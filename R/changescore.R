#  File ergm/R/ergm.getMCMCsample.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2011 the statnet development team
######################################################################
########################################################################################
# The <ergm.getMCMCsample> function samples networks using an MCMC algorithm via
# <MCMC_wrapper.C> and returns the stats matrix of the sampled networks and a single
# network as an edgelist. Note that the stats will be relative to the original network,
# i.e., the calling function must shift the statistics if required. The calling function
# must also attach column names to the statistics matrix if required.
#
# --PARAMETERS--
#   Clist     :  a list of parameters required by <MCMC_wrapper.C> and the result of
#                calling <ergm.Cprepare>
#   MHproposal:  a list of the parameters needed for Metropolis-Hastings proposals and
#                the result of calling <MHproposal>
#   eta0      :  the initial eta coefficients 
#   verbose   :  whether the C functions should be verbose; default=FALSE 
#   MCMCparams:  list of MCMC tuning parameters; those recognized include
#         maxedges      :  the maximum number of new edges that memory will be
#                          allocated for
#         samplesize    :  the number of networks to be sampled
#         interval      :  the number of proposals to ignore between sampled networks
#         burnin        :  the number of proposals to initially ignore for the burn-in
#                          period
#         Clist.miss    :  a corresponding 'Clist' for the network of missing edges,
#                          as returned by <ergm.design>
#         Clist.dt      :  yet another Clist, this one for fitting dynamic models 
#
#
# --RETURNED--
#   the sample as a list containing:
#     statsmatrix:  the stats matrix for the sampled networks, RELATIVE TO THE ORIGINAL
#                   NETWORK!
#     edgelist   :  the edgelist of the final?? sampled network
#
#########################################################################################

get.changescore <- function(Clist, MHproposal, eta0, MCMCparams, verbose=FALSE) {
  maxedges <- MCMCparams$maxedges
  nedges <- c(Clist$nedges,0,0)
  tails <- Clist$tails
  heads <- Clist$heads
  # *** don't forget, tails is now passed in before heads.
  z <- .C("MCMC_wrapper3",
  as.integer(length(nedges)), as.integer(nedges),
  as.integer(tails), as.integer(heads),
  as.integer(Clist$maxpossibleedges), as.integer(Clist$n),
  as.integer(Clist$dir), as.integer(Clist$bipartite),
  as.integer(Clist$nterms),
  as.character(Clist$fnamestring),
  as.character(Clist$snamestring),
  as.character(MHproposal$name), as.character(MHproposal$package),
  as.double(Clist$inputs),
  stats = as.double(eta0),
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
  as.integer(verbose), as.integer(MHproposal$bd$attribs),
  as.integer(MHproposal$bd$maxout), as.integer(MHproposal$bd$maxin),
  as.integer(MHproposal$bd$minout), as.integer(MHproposal$bd$minin),
  as.integer(MHproposal$bd$condAllDegExact), as.integer(length(MHproposal$bd$attribs)),
  as.integer(maxedges),
  PACKAGE="ergm")

  nedges <- z$newnwtails[1]  # This tells how many new edges there are
  newedgelist <- cbind(z$newnwtails[2:(nedges+1)], z$newnwheads[2:(nedges+1)])

  ## Post-processing of z$statsmatrix element: coerce to correct-sized matrix
  statsmatrix <- matrix(z$statsmatrix, nrow = MCMCparams$samplesize, byrow=TRUE)
  statsmatrix[is.na(statsmatrix)] <- 0

  ## outta here:  Return list with "statsmatrix" and "newedgelist"
  return(list(statsmatrix = statsmatrix, newedgelist = newedgelist))
}
