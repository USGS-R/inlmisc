/*
 * Function: DefinePolygons
 * ------------------------
 * Defines polygon rings within a single level
 *
 * Arguments:
 * a: integer; array of start-nodes for each segment
 * b: integer; array of end-nodes for each segment
 *
 * Returns:
 * ans: integer; polygon rings defined by nodes and ring index
 */

#include <R.h>
#include <Rinternals.h>

int FindNextSeg();
int StartNewSeg();

SEXP DefinePolygons(SEXP a, SEXP b) {
  int n = LENGTH(a);
  int idx = 0;
  int ply = 1;
  int i, node, *pa, *pb, *pc;

  SEXP c;

  a = PROTECT(a);
  b = PROTECT(b);
  c = PROTECT(allocVector(INTSXP, n * 2));

  pa = INTEGER(a);
  pb = INTEGER(b);
  pc = INTEGER(c);

  for(i = 0; i < n; i++) {
    node = pb[idx];
    pc[i] = node;
    pc[n + i] = ply;

    pa[idx] = -1; /* exclude used points */
    pb[idx] = -1;

    idx = FindNextSeg(pa, n, node);

    if(idx < 0) {
      idx = StartNewSeg(pa, n);
      ply += 1;
    }
  };

  UNPROTECT(3);
  return(c);
}

/* Find index for next segment by matching the end-node */
/* with start node of next segment */
int FindNextSeg(int array[], int nelements, int value) {
  int i;
  for (i = 0; i < nelements; i++) {
    if (array[i] == value) { /* end-node equals start-node */
      return(i);
    }
  }
  return(-1);
}

/* Find index for start of new polygon by finding first */
/* unused segment */
int StartNewSeg(int array[], int nelements) {
  int i;
  for (i = 0; i < nelements; i++) {
    if (array[i] > 0) { /* first unused point */
      return(i);
    }
  }
  return(-1);
}
