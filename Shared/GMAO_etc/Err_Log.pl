#!@DASPERL

####################################################################
# This is a wrapper script for the error routine, which takes 
# following arguments as inputs:
#
# -P: Path(optional): path contains the Err_Log.pm
#
# -E: err_level(1, 2, 3, 4): indicate the error level.
# 
# -N: program_name: the program name which calls the Err_Log.pl
#
# -I: identifier:
#
# -X: experiment_id:  identifier and experiment_id combine
#     together to give a unique name for an experiment.
#
# -C: err_code: (optional 1, 2, ...) corresponding to error conditions.
#     The error codes are used to identify specific errors
#     and they may differ by application.  Note that multiple
#     error conditions (each with a unique error code) with
#     the same severity code may exist in the same application.
#
# -D: err_desc(optional):   description of error condition
#
# -L: log_name(optional):
#     If not specified, the error message will be written into the
#     standard error message log file. Otherwise, error messages
#     will be written into the 'log_name' file specified
#     in the log_name parameter. Note that 'log_name' could be a
#     combination of machine name, directory path and file name.
# 
# -J  
#     if J flag is set, the event and error logs will be combined
#     into a single log 
# 
# -R: Run_Config(optional): path and name of Run_Config file if not 
#     default location.
#
# EXAMPLE:
#
# Err_Log.pl -R /u/byin/ERR_RNT/Run_Config -P /u/byin/ERR_RNT/ -E 1 
#            -N your_program_name -I your_identifier -X your_exp_id
#            -C 4 -D "This is a description about the error"
#            -L yin@hera.gsfc.nasa.gov:/home/yin/test/LOG_FILE -J
# OR
#
# Err_Log.pl  -E 1 -N program_name -I identifier -X exp_id -C 4
#
# 20020405 Owens - added support for user specified error/event logs and
#                  joined logs.
# 20031118 Todling - set perl version via Makefile procedure
####################################################################

BEGIN {

# This module contains the getopts() subroutine.

   use Getopt::Std;

# Get options and arguments

   getopts('P:E:N:I:X:C:D:L:R:J');

# Path to directory containing Perl modules.

   if ( defined( $opt_P ) ) {
      $MODULE_PATH = $opt_P;
   } else {
      $MODULE_PATH = "DEFAULT";
   }

# Option in the err_level. 

   if ( defined( $opt_E ) ) {
      $err_level = $opt_E;
   } else {
      $err_level = 'x';
   }

# Option for program_name

   if ( defined( $opt_N ) ) {
      $program_name = $opt_N;
   } else {
      $program_name = 'M';
   }

# Option for identifier

   if ( defined( $opt_I ) ) {
      $identifier = $opt_I;
   } else {
      $identifier = 'M';
   }

# Option for experiment_id

   if ( defined( $opt_X ) ) {
      $experiment_id = $opt_X;
   } else {
      $experiment_id = 'M';
   }
# Option for err_code      

   if ( defined( $opt_C ) ) {
      $err_code = $opt_C;
   } else {
      $err_code = 'x';
   }

# Option for err_desc      

   if ( defined( $opt_D ) ) {
      $err_desc = $opt_D;
   } else {
      $err_desc = "DEFAULT";
   }

# Option for log_name

   if ( defined( $opt_L ) ) {
      $log_name = $opt_L;
   } else {
      $log_name = "DEFAULT";
   }

# Option for Run_Config

   if ( defined( $opt_R ) ) {
      $run_config = $opt_R;
   } else {
      $run_config = "DEFAULT";
   }

# Option for Joined logs

   if ( $opt_J ) {
      $join = 1;
   } else {
      $join = 0;
   }

# This module locates the full path name to the location of this file.
# $FindBin::Bin will contain that value.

   use FindBin;

# This module contains the dirname() subroutine.


   use File::Basename;
   use Cwd;

# If default MODULE_PATH, set path to the directory where this
# script resides.

   if ( $MODULE_PATH eq "DEFAULT" ) {
      $MODULE_PATH = $FindBin::Bin;
   }


      @SEARCH_PATH = ( $MODULE_PATH );
}       # End BEGIN


# Include the directories to be searched for required modules.
use lib ( @SEARCH_PATH );

# Set the path to be searched for required programs.

$ENV{'PATH'} = join( ':', @SEARCH_PATH, $ENV{'PATH'} );

use Err_Log;

 $rs = err_log($err_level, $program_name, $identifier, $experiment_id, $err_code, 
               {'join'       => $join, 
                'log_name'   => $log_name, 
                'err_desc'   => $err_desc,
                'run_config' => $run_config });

 if ( $rs = 0 ) {
     print "something wrong in calling err_log\n";
     exit 1;
}
exit 0;
