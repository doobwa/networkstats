#include <Rdefines.h>
#include "changestat.h"
#include "changestats.h"
#include "model.h"
//#include "model.c"  // TODO: Is this legal to include a .c file?
#include "netstats.h"
#include "edgetree.h"
//#include "edgetree.c"

const int N_MAX=15;

static void _finalizer(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    Rprintf("finalizing\n");
    char *ptr = (char *) R_ExternalPtrAddr(ext);
    Free(ptr);
    R_ClearExternalPtr(ext);
}

SEXP createNetwork(SEXP Rnedges, SEXP Rtails, SEXP Rheads,
                   SEXP Rdn, SEXP Rdflag, SEXP Rbipartite)
/* SEXP createNetwork(int *nedges,	int *tails, int *heads, */
/*                    int *dn, int *dflag, int *bipartite)  */
{
  int directed_flag;
  Vertex n_nodes, nedges, bip;
  int *tails = INTEGER(Rtails);
  int *heads = INTEGER(Rheads);
  nedges = (int) Rnedges;
  n_nodes = (Vertex)Rdn; /* coerce double *dn to type Vertex */
  bip = (Vertex)Rbipartite; /* coerce double *bipartite to type Vertex */
  directed_flag = (int)Rdflag;
  Network nw;
  double *dn = REAL(Rdn);
  nw.nnodes = *dn;
  Network *nwp = &nw;
  double *r = REAL(Rdn);
  printf("Number of nodes is: %4.2f\n",*r);
  double b = nwp->nnodes;
  printf("Should also be: %4.2f\n",b);
  /* nw = NetworkInitialize(NULL,NULL,0,5,1,0,0); */
  /* Network *nwp = &nw; */
  /* nw = NetworkInitialize(tails, heads, nedges, n_nodes, directed_flag, bip, 0); */
  char *x = Calloc(N_MAX, char);
  snprintf(x, N_MAX, "my name is joe");
  SEXP ext = PROTECT(R_MakeExternalPtr(nwp, R_NilValue, Rnedges));
  R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
  UNPROTECT(1);
  return ext;
}
SEXP getNetworkEdges(SEXP ext)
{
  Network *nwp = (Network *) R_ExternalPtrAddr(ext);
  double b = nwp->nnodes;
  printf("number of nodes: %4.2f\n",b);
  /* int b = (int)(nw->nnodes); */
  /* printf("value is: %4.2f\n",b); */
  return R_NilValue;
}

SEXP create(SEXP info)
{
    char *x = Calloc(N_MAX, char);
    snprintf(x, N_MAX, "my name is joe");
    SEXP ext = PROTECT(R_MakeExternalPtr(x, R_NilValue, info));
    R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
    UNPROTECT(1);

    return ext;
}

SEXP get(SEXP ext)
{
    return mkString((char *) R_ExternalPtrAddr(ext));
}

SEXP set(SEXP ext, SEXP str)
{
    char *x = (char *) R_ExternalPtrAddr(ext);
    snprintf(x, N_MAX, CHAR(STRING_ELT(str, 0)));
    return ScalarLogical(TRUE);
}

