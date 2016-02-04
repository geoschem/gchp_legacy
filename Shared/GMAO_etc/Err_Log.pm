package Err_Log;
use Exporter;
@ISA = ('Exporter');
@EXPORT = (err_log);

#********** Archive error message  *******************************
#
# Return value: 1 -- writing the error message successfully
#               0 -- something wrong in writing the error message.
#
# Parameters passing into the subroutine
#
# INPUTS:
#
#   integer err_level: error severity level
#		0 -- success    
#		1 -- other error
#		2 -- warning
#		3 -- serious error
#		4 -- fatal error
#   program_name: The program name which calls the error routine.
#
#   identifier: should be use in combination with experiment_id to give
#		a unique identification of the experiment.
#   experiment_id:
# 		first or late look etc.
#   integer err_code: error code corresponding to error condition.
#               The error codes are used to identify specific errors
#               and they may differ by application.  Note that multiple
#               error conditions (each with a unique error code) with
#               the same severity code may exist in the same application.
#
#   err_desc(optional):   description of error condition
#
#   log_name(optional):
#               If not specified, the error message will be written into the
#               standard error message log file. Otherwise, error messages
#               will be written into the 'log_name' file specified
#               in the log_name parameter. Note that 'log_name' could be a
#               combination of machine name, directory path and file name.
#   join(optional - default = 0):
#               If 0 event log will be 'log_name'.O, error log = 'log_name'.E
#               If 1 event log and error log = 'log_name' 
#   run_config (optional - default = DEFAULT):
#               Location of Run_Config file to set remote shell
#   Example:
#
#	err_log( 2, 'ssmi_on29_ods.pl', 'GEOS2.5.4', 'llk', '1' )  
#       OR
#	err_log( 3, 'ssmi_on29_ods.pl', 'GEOS2.5.4', 'llk', '2',   
# 		{ 'err_desc'   => "missing data type",                           
#		  'log_name'   => "dao_ops@jimpf1:/dao_ops/GEOS_LOG",
#                 'join'       => "1",                               
#                 'run_config' => "/u/dao_ops/special/Run_Config" } )
#
# AUTHOR: BAOYU YIN
#
# HISTORY: INITIAL CODE 10/05/99
#
# 04/05/2002 Owens - added support for split or joined logs and Run_Config
#
#*****************************************************************

sub err_log {

require('timelocal.pl');
use Extract_config;

$remote = 0;
$MACHINE = "";
$REMSH = "";

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[$#_] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Process arguments.

   local ($err_level, $program_name, $identifier, $experiment_id, $err_code) = @_;

# Option to include error desc.

   if ( defined( %options ) && exists( $options{"err_desc"} ) && ($options{"err_desc"} ne "DEFAULT") ) {
      $err_desc = $options{"err_desc"};
   } else {
      $err_desc = "";
   }

# Option to join logs 

   if ( defined( %options ) && exists( $options{"join"} )  ) {
      $join = $options{"join"};
   } else {
      $join = 0;
   }

# Run-time configuration file.

   if ( defined( %options ) && exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Option for user-defined file_name_for_err.

   if ( defined( %options ) && exists( $options{"log_name"} ) && ($options{"log_name"} ne "DEFAULT") ) {
        
        $log_name = $options{"log_name"};
        if ( $log_name =~ /:/ ) {
             $remote = 1;
             ( $MACHINE, $log_name ) = split /:/, $log_name;
             ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "ssh" ) ) ne ""
               or die "(Err_log.pm) ERROR - can not set REMSH configuration value\n";
        }

      $error_log = $event_log = "$log_name";
      $error_log = "$log_name.E" unless ( $join );
      $event_log = "$log_name.O" unless ( $join );

   } else {

      $error_log = $event_log = "$ENV{'HOME'}/GEOS_EVENT_LOG";
      $error_log = "$ENV{'HOME'}/GEOS_ERROR_LOG" unless ( $join );
      $event_log = "$ENV{'HOME'}/GEOS_EVENT_LOG" unless ( $join );

   }

# zpad sub from Tommy
# Get GMT and add leading zeroes to single digit numbers
  ($sec,$min,$hour,$mday,$mon,$year,$null,$null,$null)=gmtime(time);
  $year= $year+1900;
  $mon= zpad($mon+1);
  $mday= zpad($mday);
  $hour= zpad($hour);
  $min= zpad($min);
  $sec= zpad($sec);

$yyyymmdd = "${year}${mon}${mday}";
$hhmmss = "${hour}${min}${sec}";

$err_message = sprintf"%s_%s\\|%s\\|%s\\|%s\\|%s\\|%s\\|%s",$yyyymmdd,$hhmmss,$program_name,$identifier,$experiment_id,$err_level,$err_code,$err_desc;

# Set log name
#--------------
  if ( $err_level == 0 ) {
      $LOG_FILE = $event_log;
  }else{
      $LOG_FILE = $error_log;
  }

# Set append command
#--------------------         
  if ( $remote ) { 
	 $append = "$REMSH $MACHINE \"echo $err_message >> $LOG_FILE\" ";
  }else{
	 $append = "echo $err_message >> $LOG_FILE";
  }

# Make system call
#-----------------
  $rs1 = system("$append");

# Reverse return code from system call
#-------------------------------------
  if ( $rs1 == 0 ) {
      return 1;
  }else{
      return 0;
  }

}


#zpad adds a leading zero to single digit integers
#
sub zpad{
  if ($_[0]<10){
      $out="0".$_[0];
     }
     else {$out=$_[0];
     }
return($out);
}#end sub zpad

