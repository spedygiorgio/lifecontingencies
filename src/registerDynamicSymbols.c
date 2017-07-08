#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP lifecontingencies_fAExnCpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lifecontingencies_fAxnCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lifecontingencies_fDAxnCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lifecontingencies_fExnCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP lifecontingencies_fIAxnCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lifecontingencies_mult2sum(SEXP, SEXP);
extern SEXP lifecontingencies_mult3sum(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"lifecontingencies_fAExnCpp", (DL_FUNC) &lifecontingencies_fAExnCpp, 5},
  {"lifecontingencies_fAxnCpp",  (DL_FUNC) &lifecontingencies_fAxnCpp,  6},
  {"lifecontingencies_fDAxnCpp", (DL_FUNC) &lifecontingencies_fDAxnCpp, 6},
  {"lifecontingencies_fExnCpp",  (DL_FUNC) &lifecontingencies_fExnCpp,  4},
  {"lifecontingencies_fIAxnCpp", (DL_FUNC) &lifecontingencies_fIAxnCpp, 6},
  {"lifecontingencies_mult2sum", (DL_FUNC) &lifecontingencies_mult2sum, 2},
  {"lifecontingencies_mult3sum", (DL_FUNC) &lifecontingencies_mult3sum, 3},
  {NULL, NULL, 0}
};

void R_init_lifecontingencies(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
