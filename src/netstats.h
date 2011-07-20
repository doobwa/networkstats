/*
 *  File ergm/src/netstats.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2011 the statnet development team
 */
#ifndef NETSTATS_H
#define NETSTATS_H

#include "edgetree.h"
#include "model.h"

/* *** don't forget tail -> head, so these functions accept tails first, not heads */

void network_stats_wrapper(int *tails, int *heads, int *dnedges, 
			   int *dn, int *dflag,  int *bipartite,
			   int *nterms, char **funnames,
			   char **sonames, double *inputs,  double *stats);
void SummStats(Edge n_edges, Vertex *tails, Vertex *heads,
	       Network *nwp, Model *m, double *stats);
void changescore(int *dnumnets, int *dnedges,
                   int *ntoggles,
		   int *toggletails, int *toggleheads,
		   int *tails, int *heads,
                   int *maxpossibleedges,
                   int *dn, int *dflag, int *bipartite, 
                   int *nterms, char **funnames,
                   char **sonames, 
                   char **MHproposaltype, char **MHproposalpackage,
                   double *inputs, double *theta0, int *samplesize, 
                   double *sample, int *burnin, int *interval,  
                   int *newnetworktails, 
                   int *newnetworkheads, 
                   int *fVerbose, 
                   int *attribs, int *maxout, int *maxin, int *minout,
                   int *minin, int *condAllDegExact, int *attriblength, 
                   int *maxedges);
#endif
