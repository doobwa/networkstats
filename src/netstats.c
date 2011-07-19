/*
 *  File ergm/src/netstats.c
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2011 the statnet development team
 */
#include "netstats.h"
/*****************
 void network_stats_wrapper

 Wrapper for a call from R.  Return the change in the statistics when
 we go from an empty graph to the observed graph.  If the empty graph
 has true global values equal to zero for all statistics, then this
 change gives the true global values for the observed graph.
*****************/

/* *** don't forget tail-> head, so this fucntion now accepts tails before heads */

void network_stats_wrapper(int *tails, int *heads, int *dnedges, 
			   int *dn, int *dflag,  int *bipartite,
			   int *nterms, char **funnames,
			   char **sonames, double *inputs,  double *stats)
{
  int directed_flag, hammingterm;
  Vertex n_nodes;
  Edge n_edges, nddyads;
  Network nw[2];
  Model *m;
  ModelTerm *thisterm;
  Vertex bip;

/*	     Rprintf("prestart with setup\n"); */
  n_nodes = (Vertex)*dn; 
  n_edges = (Edge)*dnedges;     
  directed_flag = *dflag;
  bip = (Vertex)*bipartite;
  
  m=ModelInitialize(*funnames, *sonames, inputs, *nterms);
  nw[0]=NetworkInitialize(NULL, NULL, 0,
                          n_nodes, directed_flag, bip, 0);

  hammingterm=ModelTermHamming (*funnames, *nterms);
/*	     Rprintf("start with setup\n"); */
  if(hammingterm>0){
    thisterm = m->termarray + hammingterm - 1;
    nddyads = (Edge)(thisterm->inputparams[0]);
    /* Initialize discordance network to the reference network. */
    nw[1]=NetworkInitializeD(thisterm->inputparams+1, 
			     thisterm->inputparams+1+nddyads, nddyads,
			     n_nodes, directed_flag, bip, 0);
  }

  /* Compute the change statistics and copy them to stats for return to R. */
  SummStats(n_edges, tails, heads, nw, m,stats);
  
  ModelDestroy(m);
  NetworkDestroy(nw);
  if (hammingterm > 0)
    NetworkDestroy(&nw[1]);
}


/****************
 void SummStats Computes summary statistics for a network. Must be
 passed an empty network (and a possible discordance network) and 
 passed an empty network
*****************/

/* *** don't forget tail-> head, so this fucntion now accepts tails before heads */

void SummStats(Edge n_edges, Vertex *tails, Vertex *heads,
Network *nwp, Model *m, double *stats){
  
  GetRNGstate();  /* R function enabling uniform RNG */
  
  ShuffleEdges(tails,heads,n_edges); /* Shuffle edgelist. */
  
  for (unsigned int termi=0; termi < m->n_terms; termi++)
    m->termarray[termi].dstats = m->workspace;
  
  /* Doing this one toggle at a time saves a lot of toggles... */
  for(Edge e=0; e<n_edges; e++){
    ModelTerm *mtp = m->termarray;
    double *statspos=stats;
    
    for (unsigned int termi=0; termi < m->n_terms; termi++, mtp++){
      if(!mtp->s_func){
        (*(mtp->d_func))(1, tails+e, heads+e, 
        mtp, nwp);  /* Call d_??? function */
        for (unsigned int i=0; i < mtp->nstats; i++,statspos++)
          *statspos += mtp->dstats[i];
      }else statspos += mtp->nstats;
    }
    
    ToggleEdge(tails[e],heads[e],nwp);
  }
  
  ModelTerm *mtp = m->termarray;
  double *dstats = m->workspace;
  double *statspos=stats;
  for (unsigned int termi=0; termi < m->n_terms; termi++, dstats+=mtp->nstats, mtp++ ){
    if(mtp->s_func){
      (*(mtp->s_func))(mtp, nwp);  /* Call s_??? function */
      for (unsigned int i=0; i < mtp->nstats; i++,statspos++)
        *statspos += mtp->dstats[i];
    }else statspos += mtp->nstats;
  }
  
  PutRNGstate();
}

void changestat(int *dnumnets, int *nedges,
		   int *tails, int *heads,
                   int *maxpossibleedges,
                   int *dn, int *dflag, int *bipartite, 
                   int *nterms, char **funnames,
                   char **sonames, 
                   char **MHproposaltype, char **MHproposalpackage,
                   double *inputs, double *stats, int *samplesize, 
                   double *sample, int *burnin, int *interval,  
                   int *newnetworktails, 
                   int *newnetworkheads, 
                   int *fVerbose, 
                   int *attribs, int *maxout, int *maxin, int *minout,
                   int *minin, int *condAllDegExact, int *attriblength, 
                   int *maxedges){
  int directed_flag, hammingterm;
  Vertex n_nodes, nmax, bip, htail, hhead;
  Edge n_networks, nddyads, kedge;
  Network nw[3];
  DegreeBound *bd;
  Model *m;
  ModelTerm *thisterm;
  
  n_nodes = (Vertex)*dn; /* coerce double *dn to type Vertex */
  n_networks = (Edge)*dnumnets; /* coerce double *dnumnets to type Edge */
  nmax = (Edge)*maxedges; /* coerce double *maxedges to type Edge */
  bip = (Vertex)*bipartite; /* coerce double *bipartite to type Vertex */
  
  GetRNGstate();  /* R function enabling uniform RNG */
  
  directed_flag = *dflag;

  m=ModelInitialize(*funnames, *sonames, inputs, *nterms);

  /* Form the network */
  nw[0]=NetworkInitialize(tails, heads, nedges[0], n_nodes, directed_flag, bip, 0);

  hammingterm=ModelTermHamming (*funnames, *nterms);
  /*  ChangeStats(ntoggles, Vertex *toggletail, Vertex *togglehead,
      Network *nwp, Model *m){*/
/* Modifies newnetworktails and newnetworkheads in place?  */
  /* MCMCSample (*MHproposaltype, *MHproposalpackage, */
  /*             theta0, sample, (long int)*samplesize, */
  /*             (long int)*burnin, (long int)*interval, */
  /*             hammingterm, */
  /*             (int)*fVerbose, nw, m, bd);   */
  newnetworktails[1] = 5;

  Vertex a = 1;
  Vertex b = 2;
  Vertex *toggletail = &a;
  Vertex *togglehead = &b;
  int ntoggles = 1;
  ModelTerm *mtp = m->termarray;

  /* double *dstats = m->workspace; */
  /* for (unsigned int i=0; i < m->n_terms; i++){ */
  /*    mtp->dstats = dstats; */
  /*    (*(mtp->d_func))(ntoggles, toggletail, togglehead, mtp, nw); */
  /*    dstats += (mtp++)->nstats; */
  /* } */

  /* dstats[1]=(double)5.0; */
  /* sample[1]=(double)5.0; */
  /* sample[2] = *mtp->dstats; */

  /* for (unsigned int termi=0; termi < m->n_terms; termi++) */
  /*   m->termarray[termi].dstats = m->workspace; */
  
  /* /\* Doing this one toggle at a time saves a lot of toggles... *\/ */
  /* int n_edges=5; */
  /* for(Edge e=0; e<n_edges; e++){ */
  /*   ToggleEdge(tails[e],heads[e],nw); */
  /*   ModelTerm *mtp = m->termarray; */
  /*   double *statspos=stats; */
    
  /*   for (unsigned int termi=0; termi < m->n_terms; termi++, mtp++){ */
  /*      if(!mtp->s_func){ */
  /*       (*(mtp->d_func))(1, tails+e, heads+e, mtp, nw);  /\* Call d_??? function *\/ */
  /*       for (unsigned int i=0; i < mtp->nstats; i++,statspos++) */
  /*         *statspos += mtp->dstats[i]; */
  /*     }else statspos += mtp->nstats; */
  /*   } */
    
  /* } */

  /*  SummStats(nedges, tails, heads, nw, m,stats);*/

  /* ChangeStats(ntoggles,a,b,nw[0],m); /\*modifies model m in place*\/ */
  /* newnetworktails[0]=newnetworkheads[0]=EdgeTree2EdgeList(newnetworktails+1,newnetworkheads+1,nw,nmax-1); */
  ModelDestroy(m);
  NetworkDestroy(nw);
  PutRNGstate();  
}

void MCMC_wrapper3 (int *dnumnets, int *nedges,
		   int *tails, int *heads,
                   int *maxpossibleedges,
                   int *dn, int *dflag, int *bipartite, 
                   int *nterms, char **funnames,
                   char **sonames, 
                   char **MHproposaltype, char **MHproposalpackage,
                   double *inputs, double *stats, int *samplesize, 
                   double *sample, int *burnin, int *interval,  
                   int *newnetworktails, 
                   int *newnetworkheads, 
                   int *fVerbose, 
                   int *attribs, int *maxout, int *maxin, int *minout,
                   int *minin, int *condAllDegExact, int *attriblength, 
                   int *maxedges){
  int directed_flag, hammingterm;
  Vertex n_nodes, nmax, bip, htail, hhead;
  Edge n_networks, nddyads, kedge;
  Network nw[3];
  DegreeBound *bd;
  Model *m;
  ModelTerm *thisterm;
  
  n_nodes = (Vertex)*dn; /* coerce double *dn to type Vertex */
  n_networks = (Edge)*dnumnets; /* coerce double *dnumnets to type Edge */
  nmax = (Edge)*maxedges; /* coerce double *maxedges to type Edge */
  bip = (Vertex)*bipartite; /* coerce double *bipartite to type Vertex */
  
  GetRNGstate();  /* R function enabling uniform RNG */
  
  directed_flag = *dflag;

  m=ModelInitialize(*funnames, *sonames, inputs, *nterms);

  /* Form the network */
  nw[0]=NetworkInitialize(tails, heads, nedges[0], n_nodes, directed_flag, bip, 0);

  hammingterm=ModelTermHamming (*funnames, *nterms);

  int n_edges = 5;
  /*  SummStats(5, tails, heads, nw, m,stats);*/

  for (unsigned int termi=0; termi < m->n_terms; termi++)
    m->termarray[termi].dstats = m->workspace;
  
  /* Doing this one toggle at a time saves a lot of toggles... */
  for(Edge e=0; e<n_edges; e++){
    ModelTerm *mtp = m->termarray;
    double *statspos=stats;
    
    for (unsigned int termi=0; termi < m->n_terms; termi++, mtp++){
      if(!mtp->s_func){
        (*(mtp->d_func))(1, tails+e, heads+e, 
        mtp, nw);  /* Call d_??? function */
        for (unsigned int i=0; i < mtp->nstats; i++,statspos++)
          *statspos += mtp->dstats[i];
      }else statspos += mtp->nstats;
    }
    
    ToggleEdge(tails[e],heads[e],nw);
  }
  
  ModelTerm *mtp = m->termarray;
  double *dstats = m->workspace;
  double *statspos=stats;
  for (unsigned int termi=0; termi < m->n_terms; termi++, dstats+=mtp->nstats, mtp++ ){
    if(mtp->s_func){
      (*(mtp->s_func))(mtp, nw);  /* Call s_??? function */
      for (unsigned int i=0; i < mtp->nstats; i++,statspos++)
        *statspos += mtp->dstats[i];
    }else statspos += mtp->nstats;
  }

  ModelDestroy(m);
  NetworkDestroy(nw);
  PutRNGstate();  /* Disable RNG before returning */
  /*
void ChangeStats(unsigned int ntoggles, Vertex *toggletail, Vertex *togglehead,
				 Network *nwp, Model *m)
  */
}
