/*
argopt: Parse options and arguments from argument list given to a command.

Usage: argopt  optstring  argument_list

argopt uses the C function getopt(3C) to parse options in "optstring" from 
the list of options and arguments in "argument_list".  All rules of 
parsing that apply to getopt apply to argopt.  One letter options are listed
in "optstring".  If the option letter is followed by a colon, the option is
expected to have an argument that may or may not be separated from it by white 
space.  "argument_list" is a list of zero or more blank-separated words.  It
contains the option letters (preceded by "-"), the option-arguments and 
operands.  For the list of  options specified in "optstring", argopt will write 
a blank separated list of words to standard output.  Each word is a one-to-one
match with an option letter in "optstring" and describes whether or not the
option letter was selected in "argument_list".  argopt writes a "0" if the 
option letter was not selected, a "1" if it was selected and does not require
an option-argument or the value of the option-argument if the option letter
was selected and the argument was provided.  The remaining list of operands 
in "argument_list" (i.e., anything getopt identifies as not being an option)
will be appended as blank separated words to the end of the option list on 
standard output.

The following example shows how a script would use argopt to parse the 
passed in argument list for 3 options and 2 operands.  The option letters 
are a, b and o, where option "o" requires an option-argument.  (Note that 
this is an example in a csh script.)

   set argv = `argopt abo: $*`
   
   if ( $status || $#argv != 5 ) then
      echo 'usage: myscript  [ -a ]  [ -b ]  [ -o o_value ]  arg1  arg2'
      exit 1
   endif
   
   set a_flag = $1
   shift
   set b_flag = $1
   shift
   set o_flag = $1
   if ( $o_flag == 0 ) set o_value = $default_value
   if ( $o_flag != 0 ) set o_value = $o_flag
   shift
   
   set arg1 = $1
   set arg2 = $2


Return codes and actions:

   0 - Successful.
   1 - Improper option letter or option-argument.  List written to standard
       output will be incomplete.

Version: 1.2

Modification log:

   DJL 940620 - Removed break from while loop if option found.  getopt must 
                read through all options to clear input buffer for next pass 
                through loop.
   DJL 940703 - Only returns on/off flag or value/off flag once if option 
                specified more than once in argument list.  

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>    /* for getopt */

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {

   int c;
   int opstr_len;
   int i;
   int status;
   char *opstring;

   extern char *optarg;
   extern int optind;
   extern int opterr;

/* start at 2nd argumet for parsing - 1st use as optstring */

   opstring=argv[1];
   opstr_len=strlen(opstring);

   for ( i=0 ; i <= opstr_len-1 ; i ++) {

      if ( *(opstring+i) != ':' ) {

         optind=2;
         status=0;

         while ((c = getopt(argc, argv, opstring)) != -1) {

            if ( status == 0 ) {

               if ( c == '?' ) {
                  status=-1;
                  break;
               }

               if ( *(opstring+i) == c ) {
            
                  if ( *(opstring+i+1) == ':' ) {
                     printf("%s ",optarg);
                     status=1;
                  } else {
                     printf("%c ",'1');
                     status=1;
                  }

               }

            }

         }

         if ( status == -1 ) break;
         if ( status ==  0 ) printf("%c ",'0');

      }

   }

/* Turn off getopt error message */

   opterr=0;

/* Positon "optind" to start of arguments and echo arguments */

   if ( status != -1 ) {

      optind=2;
      while ((c = getopt(argc, argv, opstring)) != -1);

      for ( ; optind < argc; optind++) {
         printf("%s ",argv[optind]);
      }

      printf("\n");
      return (0);

   } else {

      printf("\n");
      return (1);

   }

}
