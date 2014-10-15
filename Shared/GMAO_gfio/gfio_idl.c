/* 
  IDL interface for GFIO.
*/

#include <stdio.h>
#include "gfio_c.h"

#define READ_ONLY 0

typedef struct { 
  int slen; /* Length of string */ 
  short stype; /* type of string:  (0) static, (!0) dynamic */ 
  char *s; /* Addr of string, invalid if slen == 0.  */ 
} IDL_STRING; 


int g5open(int argc, void *argv[]) {
  int fid, rc;
  char *filename;
  IDL_STRING *str;

  if ( argc != 1 ) {
    printf("g5open: invalid number of arguments = %d\n",argc);
    printf("Usage:\n     fid = g5open(filename)\n",argc);
    return -1;
  }
  str = (IDL_STRING *) argv[0];
  filename = str->s;
  printf("[] Opening GFIO file <%s> ... ",filename);
  Gfio_Open(filename,READ_ONLY,fid, rc);
  if ( rc ) fid = -1;
  printf("fid = %d\n",fid);
  return fid;
}

int g5close(int argc, void *argv[]) {
  int *ptr, fid, rc;
  if ( argc != 1 ) {
    printf("g5close: invalid number of arguments = %d\n",argc);
    printf("Usage:\n     rc = g5close(fid)\n",argc);
    return -1;
  }
  ptr = (int *) argv[0];
  fid = *ptr;
  Gfio_Close(fid,rc);
  if ( rc ) return (rc);
  return 0;
}

int g5dims(int argc, void *argv[]) {
  int fid,im,jm,km,lm;
  int *pfid,*pim,*pjm,*pkm,*plm;
  int na, nv, rc;
   if ( argc != 5 ) {
     printf("g5dims: invalid number of arguments = %d\n",argc);
     printf("Usage:\n    rc=g5dims(fid,im,jm,km,lm)\n",argc);
     return -1;
   }
   /* copy in */
   pfid = (int *) argv[0];
   fid = *pfid;

   Gfio_DimInquire(fid,im,jm,km,lm,nv,na,rc);

   /* copy out */
   pim = (int *) argv[1];
   pjm = (int *) argv[2];
   pkm = (int *) argv[3];
   plm = (int *) argv[4];
   *pim = im;
   *pjm = jm;
   *pkm = km;
   *plm = lm;
   return (rc);
}

int g5get(int argc, void *argv[]) {
  int fid,nymd,nhms,rc;
  int im, jm, km, lm, nv,na;
  int *pfid,*pnymd,*pnhms;
  int kbeg, kend;
  int *pkbeg, *pkend;
  char *vname;
  float **var;
  IDL_STRING *str;
  
  if ( argc != 7) {
    printf("g5getvar: invalid number of arguments = %d\n",argc);
    printf("Usage:\n  rc=g5get(fid,vname,nymd,nhms,kbeg,kend,var)\n",argc);
    return -1;
  }
  /* copy in */
  pfid = (int *) argv[0];  fid = *pfid;
  str = (IDL_STRING *) argv[1];  vname = str->s;
  pnymd = (int *) argv[2]; nymd = *pnymd;
  pnhms = (int *) argv[3]; nhms = *pnhms;
  pkbeg = (int *) argv[4]; kbeg = *pkbeg;
  pkend = (int *) argv[5]; kend = *pkend;

  var = (float *) argv[6];

  printf("[] Reading variable <%s> at %d %d ... ",vname,nymd,nhms);

  Gfio_DimInquire(fid,im,jm,km,lm,nv,na,rc);
  if (rc) return (rc);
  if ( kbeg < 1 || kbeg>=km ) {
     printf("g5dims: invalid kbeg = %d\n",kbeg);
     return (-1);
  }
  if ( kend<1 || kend>=km ) {
     printf("g5dims: invalid kend = %d\n",kend);
     return (-1);
  }

  Gfio_GetVar(fid,vname,nymd,nhms,im,jm,kbeg,kend,var,rc);
  printf("rc = %d\n",rc);
  
  return (rc);
}

