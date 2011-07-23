#Simulated annealing algorithm to find the maximum and minimum potential values
#for a given model.
extreme.pot<-function(formula,theta,iter=5000,temp.init=100,cool=(1/temp.init)^(2/iter),verbose=FALSE){
  require(ergm)
  netbase<-ergm.getnetwork(formula)
  statbase<-summary(formula)
  n<-network.size(netbase)
  netsm<-as.sociomatrix(netbase)
  netbase[,]<-0
  stat.max<-matrix(nr=iter,nc=length(statbase))
  pot.max<-rep(0,iter)
  stat.min<-matrix(nr=iter,nc=length(statbase))
  pot.min<-rep(0,iter)
  stat.max[1,]<-statbase
  stat.min[1,]<-statbase
  pot.max[1]<-pot.min[1]<-statbase%*%theta
  #First, get the upper limit
  temp<-temp.init
  sm<-netsm
  for(i in 2:iter){
    #Create the candidate network
    cand<-sm
    rc<-sample(1:n,2)
    if(is.directed(netbase)){
      cand[rc[1],rc[2]]<-1-cand[rc[1],rc[2]]
    }else{
      cand[rc[1],rc[2]]<-1-cand[rc[1],rc[2]]
      cand[rc[2],rc[1]]<-1-cand[rc[2],rc[1]]
    }
    nw<-netbase
    nw[,]<-cand
    #Update the formula and get new statistics
    formula<-ergm.update.formula(formula,nw ~ .)
    stat.max[i,]<-summary(formula)
    pot.max[i]<-stat.max[i,]%*%theta
    if(verbose)
      cat("Max iter",i,"temp=",temp,"pot=",pot.max[i],"\n")
    #Keep or reject
    if((pot.max[i]-pot.max[i-1])/temp<log(runif(1))){
      stat.max[i,]<-stat.max[i-1,]
      pot.max[i]<-pot.max[i-1]
    }else
      sm<-cand
    #Cool the system
    temp<-temp*cool
  }
  #Now, get the lower limit
  temp<-temp.init
  sm<-netsm
  for(i in 2:iter){
    #Create the candidate network
    cand<-sm
    rc<-sample(1:n,2)
    if(is.directed(netbase)){
      cand[rc[1],rc[2]]<-1-cand[rc[1],rc[2]]
    }else{
      cand[rc[1],rc[2]]<-1-cand[rc[1],rc[2]]
      cand[rc[2],rc[1]]<-1-cand[rc[2],rc[1]]
    }
    nw<-netbase
    nw[,]<-cand
    #Update the formula and get new statistics
    formula<-ergm.update.formula(formula,nw ~ .)
    stat.min[i,]<-summary(formula)
    pot.min[i]<-stat.min[i,]%*%theta
    if(verbose)
      cat("Min iter",i,"temp=",temp,"pot=",pot.min[i],"\n")
    #Keep or reject
    if((pot.min[i-1]-pot.min[i])/temp<log(runif(1))){
      stat.min[i,]<-stat.min[i-1,]
      pot.min[i]<-pot.min[i-1]
    }else
      sm<-cand
    #Cool the system
    temp<-temp*cool
  }
  #Return the results
  list(stat.max=stat.max,stat.min=stat.min,pot.max=pot.max,pot.min=pot.min)
}

#Log-likelihood estimation using an approximate Labesgue integrator (or, at
#least, that's what I like to call it).
estll.lebesgue<-function(mod,iter=5000,temp.init=100,cool=(1/temp.init)^(2/iter),subdiv=5e3,prior.k=1,method="BFGS",verbose=FALSE){
  require(ergm)
  require(sna)
  #Functions to compute negative log posterior for frequencies (Dirichlet 
  #prior, up to a constant) and associated gradient; assume par is a vector
  #of values proportional to the frequencies after exponentiation -- first
  #value is fixed at zero wlg.  Potentials are assumed given on log scale.
  nlp<-function(par,y,pot,prior,n){
    par<-c(0,par)
    p<-exp(par)
    p<-p/sum(p)
    lp<-log(p)
    -(y+prior-1)%*%lp+n*logSum(pot+lp)
  }
  dnlp<-function(par,y,pot,prior,n){
    par<-c(0,par)
    epar<-exp(par)
    -(y+prior-1+epar*((n-sum(y+prior-1))/sum(epar)-exp(log(n)+pot- logSum(pot+par))))[-1]
  }
  #Get the log support size
  lss<-log(2)*choose(network.size(mod$network),2)*(2- (!is.directed(mod$network)))
  #Find the range of the potential function over the support (and get some
  #lower limits on where graphs are)
  if(verbose)
    cat("Seeking upper and lower bounds on potential function\n")
  potlim<-extreme.pot(mod$formula,theta=mod$coef,iter=iter, temp.init=temp.init,cool=cool,verbose=verbose)
  potval<-unique(c(potlim$pot.max,potlim$pot.min))
  potrng<-range(potval)
  potinc<-seq(from=potrng[1],to=potrng[2],length=subdiv)  #Segment the potential
  potcnt<-rowSums(sapply(potval,"<=",potinc))             #Count unique cases
  potcnt<-c(potcnt[1],diff(potcnt))
  #Now, aggregate the MCMC sample data
  if(verbose)
    cat("Aggregating MCMC sample data\n")
  n<-NROW(mod$sample)
  samppot<-sweep(mod$sample,2,potlim$stat.max[1,],"+")%*%mod$coef
  y<-rowSums(sapply(samppot,"<=",potinc))
  y<-c(y[1],diff(y))
  print(y)
  #Estimate the frequency distribution of graphs across classes (post mode)
  if(verbose)
    cat("Finding posterior mode for frequency distribution over the potential\n")
  prior<-rep(prior.k/subdiv,subdiv)+potcnt
  potest<-optim(log(prior/sum(prior))[-1],nlp,gr=dnlp,method=method,y=y, pot=potinc+(potinc[2]-potinc[1])/2,prior=prior,n=n)
  print(potest$convergence)
  print(potest$message)
  potfreq<-exp(c(0,potest$par))
  potfreq<-potfreq/sum(potfreq)
  #Compute the estimated normalizing factor, and log-likelihood
  if(verbose)
    cat("Computing normalizing factor and log-likelihood\n")
  lnormfac<-logSum(lss+log(potfreq)+potinc+(potinc[2]-potinc[1])/2)
  llik<-potlim$pot.max[1]-lnormfac
  #Finally, return everything
  list(log.lik=llik,log.normfact=lnormfac,pot.obs=potlim$pot.max[1], pot.vals=potinc+(potinc[2]-potinc[1])/2,pot.freq=potfreq,pot.freqprior=prior, pot.freqdat=y,log.suppsize=lss)
}

#Log-likelihood estimation using two direct importance sampling estimators, w/
#bootstrap bias-correction.  None of these approaches seem to work well, alas,
#despite the fact tha they are guaranteed to be asymptotically consistent.
estll<-function(mod,reps=500){
  require(sna)
  require(ergm)
  #Formula for likelihood estimation is as follows:
  #exp(q*to) * sum_i exp(-q1*ti) / (SS sum_i exp((q-q1)*ti))
  #were SS is the support size, q are the canonical parameters,
  #to are the observed canonical statistics, q1 are the canonical
  #parameters for the importance sample, and t1 are the statistics for
  #the importance sample.
  lss<-log(2)*choose(network.size(mod$network),2)*(2-(!is.directed(mod$network)))
  eta<-ergm.eta(etamap=mod$etamap,theta=mod$coef)
  etasim<-ergm.eta(etamap=mod$etamap,theta=mod$MCMCtheta)
  sobs<-summary(mod$formula)
  ssim<-mod$sample  #Normalized at 0 for the observations
  ssim <- sweep(ssim,2,sobs,"+")
  #Define a function to return the estimates, given a selection vector
  calcit<-function(selpr){
    #ssimb <- sweep(ssim,1,selpr,"*")
    #ssimb <- ssimb[selpr>0,]
    x <- ssim%*%eta
    piinv <- exp(ssim%*%(eta-etasim))
    #hatmu <- sum(x*piinv)/sum(piinv)
    #E2 <- sum(x*x*piinv)/sum(piinv)
    hatmu <- sum(selpr*x*piinv)/sum(selpr*piinv)
    E2 <- sum(selpr*x*x*piinv)/sum(selpr*piinv)
    hats2 <- E2-hatmu*hatmu
    llik2 <- as.vector(sobs%*%eta-lss-hatmu+0.5*hats2)
    spispos <- selpr>0
    llik <- as.vector(sobs%*%eta+logSum((log(selpr)-ssim%*%etasim)[spispos])-lss- logSum((log(selpr)+ssim%*%(eta-etasim))[spispos]))
    c(llik,llik2)
  }
  #Obtain the base estimates
  m<-NROW(ssim)
  llobs<-calcit(1:m)
  #Now, draw a bootstrap sample
  llboot<-matrix(nr=reps,nc=2)
  meanselpr<-rep(0,m)              #Mean inclusion probs for sample (E+T 10.3)
  for(i in 1:reps){
    sel<-sample(1:m,m,replace=T)   #Draw sample
    selpr<-tabulate(sel,m)/m       #Inclusion counts, normalized
    llboot[i,]<-calcit(selpr*m)
    meanselpr<-meanselpr+selpr
  }
  meanselpr<-meanselpr/reps        #Mean inclusion prob, from actual samples
  #Get some bootstrap estimates
  llbootest<-calcit(meanselpr*m)
  llbias<-colMeans(llboot)-llobs
  llbiasimp<-colMeans(llboot)-llbootest
  llcorr<-llobs-llbias
  llcorrimp<-llobs-llbiasimp
  #Return the results
  cat("GHT approx Log-likelihood estimate is",llobs[1],"\n")
  cat("log-normal approx Log-likelihood estimate is",llobs[2],"\n")
  cat("Bootstrap means for both (GHT,log-norm) are:",colMeans(llboot),"\n")
  cat("Bootstrap bias estimates are:",llbias,"\n")
  cat("Bias corrected means are:",llcorr,"\n")
  cat("Improved bootstrap bias estimates are:",llbiasimp,"\n")
  cat("Improved bias corrected means are:",llcorrimp,"\n")
  c(llobs,llcorr,llcorrimp)
}
