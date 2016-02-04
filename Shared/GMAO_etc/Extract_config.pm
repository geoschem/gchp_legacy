package Extract_config;
#
# 20020114 TOwens added FVHOME to Run_Config search path
#

require Exporter;
require 5.000;
@ISA = qw(Exporter);
@EXPORT = qw(extract_config ex_array);

sub extract_config {

# This module contains the dirname() subroutine.

use File::Basename;

# If not specified, 4th argument is the same directory from which the calling
# script is invoked.

   if ( $#_ < 3 ) { $_[3] = dirname( $0 ); }

# If not specified, 5th argument is the default name of the run_config_file,
# "Run_Config".

   if ( $#_ < 4 ) { $_[4] = "Run_Config"; }

   my $requested_param = $_[0];
   my $run_config_file = $_[1];
   my $default_value   = $_[2];
   my $bin_dir         = $_[3];
   my $run_config_base = $_[4];

# If run_config_file given (i.e., not "DEFAULT"), use that if it
# exists.  Otherwise, find another one.

   if ( $run_config_file ne "DEFAULT" ) {

      if ( !( -e $run_config_file ) ) {
         print STDERR "(extract_config) Run_Config file '", $run_config_file, "' does not exist.\n";
         return( $default_value );
      }

      $run_config_file_use = $run_config_file;

   } else {

# See if a config file exists in $FVHOME/run if set otherwise check $HOME
      if ($ENV{'FVHOME'}){
          $run_config_file = join( '', $ENV{'FVHOME'}, "/", "run", "/", $run_config_base );
     } else {
        $run_config_file = join( '', $ENV{'HOME'}, "/", $run_config_base );
     }

     if ( -e $run_config_file ) {
        $run_config_file_use = $run_config_file;
      
# See if a config file exists in user's home directory in case $FVHOME/run came up dry.
     } else {
        $run_config_file = join( '', $ENV{'HOME'}, "/", $run_config_base );

        if ( -e $run_config_file ) {
           $run_config_file_use = $run_config_file;

# See if a config file exists in bin directory that this script resides in.

        } else {
           $run_config_file = join( '', $bin_dir, "/", $run_config_base );

           if ( -e $run_config_file ) {
              $run_config_file_use = $run_config_file;
# No config file found; the default value will be used for the parameter.

           } else {
              $run_config_file_use = "DEFAULT";
           }
        }
     } 
   }

   if ( $run_config_file_use eq "DEFAULT" ) {

      $param_value = $default_value;

   } else {

      unless ( open( CFILE, $run_config_file_use ) ) {
         print STDERR "(extract_config) Run_Config file '", $run_config_file_use, "' can not be opened.\n";
         return( $default_value );
      }

      $param_value = "";
      while ( <CFILE> ) {
         chop;
         if ( m/$requested_param/ ) { $param_value = ( split( /=/ ) )[1]; }
      }

      close( CFILE );

   }

   if ( not $param_value ) { 
         print STDERR "(extract_config) Did not find parameter '", $requested_param, "' in Run_Config file '", $run_config_file_use, "'\n";
         $param_value = $default_value;
   }

# Strip any leading or trailing blanks from the value.

   $param_value =~ s/^ *(.*) *$/$1/;

   return( $param_value );

}

#
# ex_array subroutine
# Extracts an array (wordlist) from a string
# by compressing imbedded whitespace and removing
# leading and trailing whitespace
#

sub ex_array {
     my ($string) = @_;
     my $array;

     ($string = $string) =~ s/^ *(.*) *$/$1/;
     ($string = $string) =~ s/\s+/ /g;

     @array = split(/ /, $string);
return @array;
}

1;
