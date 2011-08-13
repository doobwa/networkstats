#include <Rdefines.h>

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

SEXP createNetwork(SEXP nedges, SEXP tails, SEXP heads,
                   SEXP dn, SEXP dflag, SEXP bipartite)
{
  int directed_flag, hammingterm;
  /* Vertex n_nodes, nmax, bip, htail, hhead; */
  /* Edge n_networks, nddyads, kedge; */
  /* Network nw[3]; */
  
  /* n_nodes = (Vertex)*dn; /\* coerce double *dn to type Vertex *\/ */
  /* n_networks = (Edge)*dnumnets; /\* coerce double *dnumnets to type Edge *\/ */
  /* bip = (Vertex)*bipartite; /\* coerce double *bipartite to type Vertex *\/ */
  /* directed_flag = *dflag; */
//  nw = NetworkInitialize(tails, heads, *nedges, n_nodes, directed_flag, bip, 0);
  char *x = Calloc(N_MAX, char);
  snprintf(x, N_MAX, "my name is joe");
  SEXP ext = PROTECT(R_MakeExternalPtr(x, R_NilValue, nedges));
  R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
  UNPROTECT(1);
  return ext;
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

