/*
 *  File ergm/src/MHproposals_bipartite.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2011 the statnet development team
 */
#ifndef MHproposals_bipartite_H
#define MHproposals_bipartite_H

#include "edgetree.h"
#include "changestat.h"
#include "model.h"
#include "MHproposal.h"

void MH_Bipartiterandomtoggle (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteConstantEdges (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteHammingConstantEdges (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteHammingTNT (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteCondDegreeDist (MHproposal *MHp, DegreeBound *bd, Network *nwp);
void MH_BipartiterandomtoggleNonObserved (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteCondDegHexadToggles (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteCondDegTetradToggles (MHproposal *MHp,  DegreeBound *bd, Network *nwp);
void MH_BipartiteCondDegree (MHproposal *MHp,  DegreeBound *bd, Network *nwp);

#endif 


