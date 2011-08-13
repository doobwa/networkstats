#include <Rdefines.h>

typedef struct Foostruct {
  int val;
} Foo;

const int N_MAX=15;

static void _finalizer(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    Rprintf("finalizing\n");
    //   char *ptr = (char *) R_ExternalPtrAddr(ext);
    Foo *ptr = (Foo *) R_ExternalPtrAddr(ext);
    Free(ptr);
    R_ClearExternalPtr(ext);
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
SEXP createFoo(SEXP info)
{
    Foo *foo;
    foo->val = 5;
    int b = foo->val;
    printf("Val: %d\n",b);

    SEXP ext = PROTECT(R_MakeExternalPtr(foo, R_NilValue, info));
    R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}
SEXP getFooVal(SEXP ext)
{
  Foo *foo = (Foo *) R_ExternalPtrAddr(ext);
  int b = foo->val;
  printf("Val: %d\n",b);
  return ScalarLogical(TRUE);//foo->nnodes;
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

