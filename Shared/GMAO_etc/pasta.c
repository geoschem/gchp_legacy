/*

   This is a simple C program to return experiment id, year, month, day, hour
   given a file name and a GrADS-like template. It is better explained by 
   examples, see below. 

   This utility is used by the "pesto" script. It ain't pretty but
   seems to work.

   EXAMPLES:

   1)
   pasta         efake.prog.t19971231.18Z  %s.prog.t%y4%m2%d2.%h2Z
   produces:     1997 97 12 dec 31 18 efake 18:00Z31dec1997 00
                 year yr mo mon dy hr expid  grads_string   min

   2)
   pasta         efake.prog.t19971231.18Z  efake.%s.t%y4%m2%d2.%h2Z
   produces:     1997 97 12 dec 31 18 prog 18:00Z31dec1997 00
                 year yr mo mon dy hr prog  grads_string   min

   RESTRICTIONS:

   1) The token "%s" must be followed by a "."
   2) Supported tokens on input: 

          %s        experiment ID (a string followed by ".")
          %y4       year,  e.g., 1997
          %y2       year,  e.g., 97
          %m2       month, e.g., 12
          %d2       day,   e.g., 31
          %h2       hour,  e.g., 18
          %n2       minute, e.g., 30
          %c        single wildcard character, e.g. %c%c for nh
      Notice that there is no "%m3" allowed on input.

   REVISION HISTORY:

   26dec1999  da Silva  First version.
   27dec1999  da Silva  Changed name from pesto_atmos to pasta
   06jan2005  Owens     Added "%c" wildcard and some error trapping (PR 1260)
   23jan2007  Owens     Added support for %y4 output with %y2 input
 */

#include <stdio.h>
#include <string.h>

/* Pads with '-' space occupied by tokens (%y4, %m2, etc) */

void 
expand ( char *str1, char c1, char c2, int n, char *str2 ) 
{

  int i, j, k;

  if ( strlen(str1) < 3 ) return;

  j=0;
  i=0;
  while ( i < strlen(str1)-1 ) {
    if ( (str1[i] == '%') && (str1[i+1]==c1) && (str1[i+2]==c2) ) {
      for(k=j; k<j+n; k++) str2[k] = '-';
      j = j + n;
      i = i+3;
    } else if ((str1[i] == '%') && (str1[i+1]==c1) && (str1[i+1]=='c')) {
      for(k=j; k<j+n; k++) str2[k] = '-';
      j = j + n;
      i = i+2;
    } else {
      str2[j] = str1[i];
      i++; j++;
    }
  }
  k = i;
  for(i=k; i<strlen(str1); i++) {
      str2[j] = str1[i];
      j++;
  }
  str2[j]='\0';
  /* printf("Expansion of '%s' for %c%c is: %s\n", str1,c1,c2,str2); */
 
}

/* Checks whether a token (%y4, %m2, etc) exists inside str */

int
hasit ( char *str, char c1, char c2 ) {
  int i;
  for(i=0; i < strlen(str)-1; i++ ) {
     if ( (str[i] == '%') && (str[i+1]==c1) && (str[i+2]==c2) ) 
     return 1;
  }
  return 0;
}


/* Driver */

int 
main(int argc, char *argv[]) {
  char *string, *template, *chk;
  char b1[1024], b2[1024];
  char str[1024], y4[5], y2[3], m2[3], m3[4], d2[3], h2[3], n2[3];
  int  i, j, ls, m;
  char key[] = "1234567890";
  if ( argc != 3 ) {
       fprintf(stderr,"Usage:    pasta        string                     template\n");
       fprintf(stderr,"Example:  pasta  efake.prog.t19971231.18Z  %%s.prog.t%%y4%%m2%%d2.%%h2Z\n");
       exit(1);
  }

  string   = argv[1];
  template = argv[2];


  /* defaults */
  strcpy ( y4, "yryr" );
  strcpy ( y2, "yr" );
  strcpy ( m2, "mo" );
  strcpy ( m3, "mon" );
  strcpy ( d2, "dy" );
  strcpy ( h2, "hr" );
  strcpy ( n2, "00" );

  /* Start determining experiment ID */
  if ( hasit (template,'s', '.' ) ){

    /* expand all other tokens */
    expand(template,'y', '4', 4, b1);
    expand(b1,'y', '2', 2, b2);
    expand(b2,'m', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'h', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);

    /* find beg position of %s */
    i = -1;
    for(j=0; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&b1[j+1]=='s' ) {
            i = j;
            break;
      }
    }
    if ( i < 0 ) {
      fprintf(stderr,"pasta: internal error\n"); 
      exit(1);
    }
    for(j=i;j<strlen(string);j++) {
        
      if ( string[j] == '.' ) {	str[j-i] = '\0'; } 
     else                     { str[j-i] = string[j]; }

    }

  } else {

     str[0] = '\0';

  }


  ls = strlen(str)+1;
  /* Next, each date related token */

  /* y4 */
  if ( hasit (template,'y', '4' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '2', 2, b2);
    expand(b2,'m', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'h', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&(b1[j+1]=='y')&&(b1[j+2]=='4') ) {
            i = j; break;
      }
    }
    for(j=i;j<i+4; j++) y4[j-i] = string[j];
    y4[4]='\0';
    chk = strpbrk (y4, key);
    if ( ( chk == NULL ) ||  ( strlen(y4) > strlen(chk) ) )  {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }

  }
  /* y2 */
  if ( hasit (template,'y', '2' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '4', 4, b2);
    expand(b2,'m', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'h', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&(b1[j+1]=='y')&&(b1[j+2]=='2') ) {
            i = j; break;
      }
    }
    for(j=i;j<i+2; j++) y2[j-i] = string[j];
    y2[2]='\0';
    chk = strpbrk (y2, key);
    if ( ( chk == NULL ) ||  ( strlen(y2) > strlen(chk) ) )  {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }

  }
  /* m2 */
  if ( hasit (template,'m', '2' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '4', 4, b2);
    expand(b2,'y', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'h', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&(b1[j+1]=='m')&&(b1[j+2]=='2') ) {
            i = j; break;
      }
    }
    for(j=i;j<i+2; j++) m2[j-i] = string[j];
    m2[2]='\0';

    chk = strpbrk (m2, key);
     if ( ( chk == NULL ) ||  ( strlen(m2) > strlen(chk) ) )  {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }

    m = atoi ( m2 );
    if ( m == 1 )  strcpy ( m3, "jan" );
    if ( m == 2 )  strcpy ( m3, "feb" );
    if ( m == 3 )  strcpy ( m3, "mar" );
    if ( m == 4 )  strcpy ( m3, "apr" );
    if ( m == 5 )  strcpy ( m3, "may" );
    if ( m == 6 )  strcpy ( m3, "jun" );
    if ( m == 7 )  strcpy ( m3, "jul" );
    if ( m == 8 )  strcpy ( m3, "aug" );
    if ( m == 9 )  strcpy ( m3, "sep" );
    if ( m == 10 ) strcpy ( m3, "oct" );
    if ( m == 11 ) strcpy ( m3, "nov" );
    if ( m == 12 ) strcpy ( m3, "dec" );

  }
  /* d2 */
  if ( hasit (template,'d', '2' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '4', 4, b2);
    expand(b2,'y', '2', 2, b1);
    expand(b1,'m', '2', 2, b2);
    expand(b2,'h', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&(b1[j+1]=='d')&&(b1[j+2]=='2') ) {
            i = j; break;
      }
    }
    for(j=i;j<i+2; j++) d2[j-i] = string[j];
    d2[2]='\0';
    chk = strpbrk (d2, key);
    if ( ( chk == NULL ) ||  ( strlen(d2) > strlen(chk) ) )  {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }
  }
  /* h2 */
  if ( hasit (template,'h', '2' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '4', 4, b2);
    expand(b2,'y', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'m', '2', 2, b1);
    expand(b1,'n', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) {
      if ( (b1[j]=='%')&&(b1[j+1]=='h')&&(b1[j+2]=='2') ) {
            i = j; break;
      }
    }
    for(j=i;j<i+2; j++) h2[j-i] = string[j];
    h2[2]='\0';
    chk = strpbrk (h2, key); 
    if ( ( chk == NULL ) || (strlen(h2) > strlen(chk)) ) {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }

  }
  /* n2 */
  if ( hasit (template,'n', '2' ) ){
    expand(template,'s', '.', ls, b1);
    expand(b1,'y', '4', 4, b2);
    expand(b2,'y', '2', 2, b1);
    expand(b1,'d', '2', 2, b2);
    expand(b2,'m', '2', 2, b1);
    expand(b1,'h', '2', 2, b2);
    expand(b2,'c', '1', 1, b1);
    i = -1;
    for(j=1; j<strlen(b2); j++ ) { 
      if ( (b1[j]=='%')&&(b1[j+1]=='n')&&(b1[j+2]=='2') ) { 
            i = j; break; 
      } 
    }
    for(j=i;j<i+2; j++) n2[j-i] = string[j];
    n2[2]='\0';
    chk = strpbrk (n2, key); 
    if ( ( chk == NULL ) ||  ( strlen(n2) > strlen(chk) ) )  {
           fprintf(stderr,"pasta: error parsing token\n");
           exit(1);
    }

  }

  if ( str[0] == '\0' ) strcpy ( str, "expid" );

  if ( hasit (template,'y', '4' ))  strcpy (y2, &y4[2]);

  if ( hasit (template,'y', '2' )){
     if ( atoi(y2) > 40 ){ 
        strcpy( y4,"19");  
     }else{
        strcpy( y4,"20");  
     }
     strcat(y4,y2);
  }
  printf("%s %s %s %s %s %s %s %s:%sZ%s%s%s %s\n", y4, y2, m2, m3, d2, h2, str,
                                    h2, n2, d2, m3, y4, n2);
  
  exit(0);

} 
