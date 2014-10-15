/* file swapendian.c: source to swap binary grid data from IBM RS600 to
 *                    IBM pc and vice versa
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void usage( void );
void swapendian( char*, char* );

char *appname;

int main( int argc, char *argv[] )
{
 FILE *input_fid, *output_fid;
 char *buf, *ptr;
 struct stat *statbuf;
 register int i;
 size_t wrote, check;
 size_t fullsize;
 size_t mysize;
 size_t processed, ret, remainder;


 appname = strdup( argv[0] );

 if( argc != 3  ) {
	usage();
	exit(0);
 }

 printf("%s %s\n", "Swap Endian for File:", argv[1] );

 if( NULL == (input_fid = fopen( argv[1], "r" )) ) {
	fprintf( stderr, "\n");
	perror(appname);
	exit(1);
 }
 statbuf = (struct stat *) malloc( sizeof(struct stat) );
 stat( argv[1], statbuf );

 fullsize = (size_t)( statbuf->st_size );
 mysize = 256*BUFSIZ; /* 2Megabytes */
 mysize = (mysize < fullsize)? mysize: fullsize;

 if( NULL == ( buf = malloc(mysize ) ) ) {
	fprintf( stderr, "\n");
 	perror(appname);
	exit(1);
 }

 if( ( check = (statbuf->st_size)%4  ) != 0 ) { /*sorry, but I dunno it */
#ifdef DEBUG
   printf("%d", check );
#endif
   fprintf( stderr,"%s%s%s", "\nswapendian: infile \"",
	    argv[1],"\" wasn't multiple of 4 bytes long\n");
   exit(1);
 }

 if( NULL == (output_fid = fopen( argv[2], "w" )) ) {
   fprintf( stderr, "\n");
   perror(appname);
   exit(1);
 }

#ifdef DEBUG
 printf("%s %d %s\n", "allocated", statbuf->st_size, "bytes of mem" ); 
#endif

 processed = 0;
 remainder = fullsize;

 while (processed < fullsize) {

   ptr = buf;
   ret = fread( buf, 1, (size_t)( mysize ), input_fid );
   if (ret < mysize) {
     fprintf(stderr,"ERROR: sizes\n");
     exit(2);
   }

   for( i=0; i<(mysize)/4; i++)  {
     swapendian( ptr, ptr );
     ptr+=4;
   }
   wrote = fwrite( buf, 1, (size_t)( mysize ), output_fid );
   if (wrote < mysize) {
     fprintf(stderr,"%s\n","Error: fwrite");
     exit(2);
   }

   processed += mysize;
   remainder -= mysize;
   mysize = (mysize < remainder)? mysize: remainder;
 }


#ifdef DEBUG
 printf("%s %d %s\n", "succesfully wrote", wrote, "bytes");
#endif
 free(statbuf);
 free(buf);
 fclose( input_fid );
 fclose( output_fid );

 return 0;
}


void usage( void )
{
 fprintf( stderr,"%s %s %s","usage:", appname,
	 "in_file out_file\n");
}

void swapendian( char *ptr, char *cpy_site)
{
char tmp[4];

 tmp[3] =  *ptr;
 tmp[2] =  *(ptr+1);
 tmp[1] =  *(ptr+2);
 tmp[0] =  *(ptr+3);
 memcpy( cpy_site, tmp, 4 );
}
