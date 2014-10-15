/**********************************************************************
Routine: testGfio -- Sample program to test the GFIO library.

Description:  This test program creates and writes a simple GFIO file.
              It then checks the file by reading information back.
              
Input Parameters:  None.
Output Parameters: None.

Revision History:

  1998.08.14  Lucchesi           Initial coding.

***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "gfio_c.h"

#define NVARS 3
#define MAXATT 50
#define ATTLEN 150
#define IM 72
#define JM 46
#define KM 3
#define FILL -999.9

void errorExit ( char *routine, int errcode ) {

   printf("Error %d in %s\n",errcode,routine);
   exit (-1);
}

main()
{

/* Variables for Gfio_Create. */

   char fname[129] = "Gfio_file_C.nc";
   char title[129] = "Test Dataset from Gfio C Interface";
   char source[129] = "NASA/GSFC Data Assimilation Office";
   char contact[129] = "lucchesi@dao.gsfc.nasa.gov";
   float amiss = FILL;
   int im = IM;
   int jm = JM;
   int km = KM;
   float *lon;
   float *lat;
   float *lev;
   char levunits[9] = "millibar";
   int beg_date = 19971014;
   int beg_time = 0;
   int timinc = 60000;
   int nvars = NVARS;
   char vname[NVARS][129] = {"SLP","HGHT","TMPU"};
   char vtitle[NVARS][40] = {"Sea level pressure","Geotpotential Heights","Temperature"};
   char vunits[NVARS][40] = {"Millibars","Meters","Kelvin"};
   int kmvar[NVARS] = {0, KM, KM};
   float valid_range[2][NVARS];
   float packing_range[2][NVARS];
   int fid = -9;
   int rc = -9;

/* Work variables */

   float lonInterval;
   float latInterval;
   float rtemp;
   int i,j,k,lm, ngatts;
   int *attArray;
   float attReal = -999.9;
   char attChar[20] = "hello";
   int *yyyymmdd, *hhmmss;
   int type, count;
   char attName[MAXATT][129];
   char  charAtt[ATTLEN];
   int   intAtt[ATTLEN];
   float floatAtt[ATTLEN];
   float forgrid_2d[JM][IM];
   float forgrid_3d[KM][JM][IM];
   float getgrid_3d[KM][JM][IM];

/* Allocate space. */

   lon = (float *) malloc (sizeof(float)*IM);
   lat = (float *) malloc (sizeof(float)*JM);
   lev = (float *) malloc (sizeof(float)*KM);
   attArray = (int *) malloc (sizeof(int)*10);

   valid_range[0][0] = amiss;
   valid_range[1][0] = amiss;
   valid_range[0][1] = amiss;
   valid_range[1][1] = amiss;
   valid_range[0][2] = amiss;
   valid_range[1][2] = amiss;

/* Packing Range array is loaded in Fortran memory layout */

   packing_range[0][0] = 0;
   packing_range[0][1] = 500.0;
   packing_range[0][2] = 0;
   packing_range[1][0] = 5;
   packing_range[1][1] = 0;
   packing_range[1][2] = 5;

   for (j=0; j < nvars; j++)
     for (i=0; i < 2; i++)
       printf ("packing_range(%d,%d)=%f\n",i,j,packing_range[i][j]);


/* Prepare grid definitions. */

    lonInterval = 360.0/im;
    rtemp = -180.0 + lonInterval*0.5;
    for (i=0; i<im; i++) {
      lon[i]=rtemp;
      rtemp=rtemp + lonInterval;
    }

    latInterval = 180.0/(jm-1);
    rtemp = -90.0;
    for (j=0; j<jm; j++) {
      lat[j] = rtemp;
      rtemp = rtemp + latInterval;
    }
    
    rtemp=0.0;
    for (k=0; k<km; k++) {
      lev[k] = rtemp;
      rtemp = rtemp + 1.0;
    }
    

/* Prepare fake data arrays. */

    for (i=0; i<im; i++) {
       for (j=0; j < jm; j++) {
          forgrid_2d[j][i]=j;
       }
    }


    for (i=0; i < im; i++) {
       for (j=0; j < jm; j++) {
          for (k=0; k < km; k++) {
             forgrid_3d[k][j][i]=k+1;
          }
       }
    }
 
    for (i=0; i < 9; i++) {
       attArray[i] = i*10;
    }
    
/* Define contents of the file we are going to write. */

   Gfio_Create (fname,title,source,contact,amiss,
                im,jm,km,lon,lat,lev,levunits,
                beg_date,beg_time,timinc,nvars,vname,
                vtitle,vunits,kmvar,valid_range,
                packing_range,0,fid,rc);
   if (rc < 0) errorExit ("Gfio_Create",rc);
   printf("Create finished\n");

/* Write out three variables for the first time. */

   Gfio_PutVar ( fid, vname[0], 19971014, 0, im, jm, 0, 1, forgrid_2d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);
   printf("wrote 2D field\n");
   Gfio_PutVar ( fid, vname[1], 19971014, 0, im, jm, 1, km, forgrid_3d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);
   printf("wrote 3D field\n");
   Gfio_PutVar ( fid, vname[2], 19971014, 0, im, jm, 1, km, forgrid_3d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);

/* Write out three variables for the second time. */

   Gfio_PutVar ( fid, vname[0], 19971014, 60000, im, jm, 0, 1, forgrid_2d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);
   Gfio_PutVar ( fid, vname[1], 19971014, 60000, im, jm, 1, km, forgrid_3d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);
   Gfio_PutVar ( fid, vname[2], 19971014, 60000, im, jm, 1, km, forgrid_3d, rc);
   if (rc < 0) errorExit ("Gfio_PutVar",rc);

/* Write user-defined attributes.  These become global attributes in the HDF file. */

   Gfio_PutIntAtt (fid, "int_attribute", 10, attArray, 0, rc);
   if (rc < 0) errorExit ("Gfio_PutIntAtt",rc);
   Gfio_PutRealAtt (fid, "real_attribute", 1, &attReal, 0, rc);
   if (rc < 0) errorExit ("Gfio_PutRealAtt",rc);
   Gfio_PutCharAtt (fid, "char_attribute", 5, attChar, rc);
   if (rc < 0) errorExit ("Gfio_PutCharAtt",rc);

/* Close file. */

   Gfio_Close (fid,rc);
   if (rc < 0) errorExit ("Gfio_Close",rc);

/* Re-open file. */

   Gfio_Open ( fname, 0, fid, rc );
   if (rc < 0) errorExit ("Gfio_Open",rc);

/* Get dimension information from the file. */

   Gfio_DimInquire (fid, im, jm, km, lm, nvars, ngatts, rc);
   if (rc < 0) errorExit ("Gfio_DimInquire",rc);

   printf("Results of DimInquire\n");
   printf("---------------------\n");
   printf("im=%d\n",im);
   printf("jm=%d\n",jm);
   printf("km=%d\n",km);
   printf("lm=%d\n",lm);
   printf("nvars=%d\n",nvars);
   printf("ngatts=%d\n",ngatts);

/* Get more detailed information. */

   yyyymmdd = (int *) malloc (sizeof(int)*lm);
   hhmmss   = (int *) malloc (sizeof(int)*lm);

   Gfio_Inquire (fid, im, jm, km, lm, nvars, title, source, contact, amiss,
                 lon, lat, lev, levunits, yyyymmdd, hhmmss, timinc, vname,
                 vtitle, vunits, kmvar, valid_range, packing_range, rc);
   if (rc < 0) errorExit ("Gfio_DimInquire",rc);

   printf("Results of Inquire\n");
   printf("------------------\n");
   printf("im=%d\n",im);
   printf("jm=%d\n",jm);
   printf("km=%d\n",km);
   printf("lm=%d\n",lm);
   printf("nvars=%d\n",nvars);
   printf("title=%s\n",title);
   printf("source=%s\n",source);
   printf("contact=%s\n",contact);
   printf("amiss=%f\n",amiss);
   printf("lon=|"); for (i=0; i<im; i++) printf("%f|",lon[i]); printf("\n");
   printf("lat=|"); for (i=0; i<jm; i++) printf("%f|",lat[i]); printf("\n");
   printf("lev=|"); for (i=0; i<km; i++) printf("%f|",lev[i]); printf("\n");
   printf("levunits=%s\n",levunits);
   printf("yyyymmdd=|"); for (i=0; i<lm; i++) printf("%d|",yyyymmdd[i]); printf("\n");
   printf("hhmmss=|"); for (i=0; i<lm; i++) printf("%d|",hhmmss[i]); printf("\n");
   printf("timinc=%d\n",timinc);
   printf("vnames=|"); for (i=0; i<nvars; i++) printf("%s|",vname[i]); printf("\n");
   printf("vtitle=|"); for (i=0; i<nvars; i++) printf("%s|",vtitle[i]); printf("\n");
   printf("vunits=|"); for (i=0; i<nvars; i++) printf("%s|",vunits[i]); printf("\n");
   printf("kmvar=|"); for (i=0; i<nvars; i++) printf("%d|",kmvar[i]); printf("\n");


/* Get one of the 3D variables and compare values to original data. */

   Gfio_GetVar (fid, vname[2], 19971014, 60000, im, jm, 1, km, getgrid_3d, rc);
   if (rc < 0) errorExit ("Gfio_GetVar",rc);
   for (i=0; i < IM*JM*KM; i++) {
      if (getgrid_3d[i] != forgrid_3d[i]) {
         printf ("%d %f %f\n",i,getgrid_3d[i],forgrid_3d[i]);
      }
   }

/* Read and print all of the global attributes. */

   if (ngatts < MAXATT) {
      Gfio_GetAttNames ( fid, ngatts, attName, rc );
      for (i=0; i < ngatts; i++) {
         Gfio_AttInquire (fid, attName[i], type, count, rc);
         if ( count < ATTLEN ) {
            switch (type) {
               case 0:
                  Gfio_GetIntAtt ( fid,attName[i],count,intAtt,rc);
                  printf("Attribute %d: integer: %s=|",i,attName[i]);
                  for (j=0; j < count; j++) printf("%d|",intAtt[j]); printf("\n");
                  break;
               case 1:
                  Gfio_GetRealAtt ( fid,attName[i],count,floatAtt,rc);
                  printf("Attribute %d: float: %s=|",i,attName[i]);
                  for (j=0; j < count; j++) printf("%f|",floatAtt[j]); printf("\n");
                  break;
               case 2:
                  Gfio_GetCharAtt ( fid,attName[i],count,charAtt,rc );
                  charAtt[count]='\0';
                  printf("Attribute %d: character: %s=%s\n",i,attName[i],charAtt);
                  break;
               default:
                  printf("Attribute %d: type %d unknown: %s\n",i,type,attName[i]);
                  break;
            }
         }
         else
            printf("Attribute %d: %s: count too long\n",i,attName[i]);
      }
   }
}
