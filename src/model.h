/*
 *  File ergm/src/model.h
 *  Part of the statnet package, http://statnetproject.org
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) in
 *    http://statnetproject.org/attribution
 *
 *  Copyright 2011 the statnet development team
 */
#ifndef MODEL_H
#define MODEL_H

#include "edgetree.h"
#include "changestat.h"
#include "R_ext/Rdynload.h"

/* A Model object contains information about an entire ERGM, including the
   total numbers of terms, parameters, and statistics along with a pointer
   to an array of ModelTerm structures.  */
typedef struct Modelstruct {
  ModelTerm *termarray; /* array of size n_terms; see changestat.h
                           for ModelTerm definition */
  int n_terms;
  int n_stats;
  double *workspace; /* temporary workspace of size */
  double **dstatarray; /* array of size n_terms; the ith element in this
			  array is a pointer to an array of size
			  termarray[i].nstats                    */
} Model;

Model* ModelInitialize (char *fnames, char *sonames, double *inputs,
			int n_terms);

void ModelDestroy(Model *m);

/* A Model object contains information about an entire ERGM, including the
   total numbers of terms, parameters, and statistics along with a pointer
   to an array of ModelTerm structures.  */

int GetIndexForAttrValue(int value);
int ModelTermHamming (char *fnames, int n_terms);
int ModelTermFormation (char *fnames, int n_terms);
int ModelTermDissolve (char *fnames, int n_terms);


/* *** don't forget tail-> head, so this function accepts toggletail first, not togglehead  */

void ChangeStats(unsigned int ntoggles, Vertex *toggletail, Vertex *togglehead, Network *nwp, Model *m);

#endif

