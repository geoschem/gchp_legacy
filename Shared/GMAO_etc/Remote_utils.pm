package Remote_utils;
#
# 20020114 Owens added  DEFAULT_RSH and DEFAULT_RCP values 
#
# 20020814 Owens added support for locally mounted file systems 
#
# 20040120 Owens added support for localhost dmget
#          Lucchesi changed default dmget location
#  
#
# 20050614 Lucchesi Added $remote_machine option to check_ftp()
#
# 20050801 Owens added local move in clean option to rput() 
#
# 20061203 Owens skip mkdir if dir exists in local move in rput()
#
# 20070301 Owens added dmput support to rdmget()
#
# 20080418 Owens changed return code handling on local mkdir to
#          check existence of new directory.
#
# 20090610 Kokron Use remote_host provided on command line when gethostbyname
#          returns empty string.  Workaround for pleiades

require 5.000;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(rput splitrfile is_remote_dir rm_remote_file mkdir_remote chmod_remote 
            ex_array rdmget rflist one_hop_put rget);

#===================== Default Programs =============================#

$DEFAULT_RSH = "/usr/bin/ssh";
$DEFAULT_RCP = "/usr/bin/scp";
$DEFAULT_WGET = "/usr/local/other/SLES11.3/wget/1.18/bin/wget --no-check-certificate";

#===================== Global Variables =============================#

my ($full_local_host_name, $full_remote_host_name);


#=================== Check for local host  ==========================#
sub check_local{

   use Sys::Hostname;

# Variables
 
   my ($LOCAL, $local_host_name, $local_domain_name, $remote_host_name, $remote_domain_name, $debug );

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }
# Debug option.

   if ( exists( $options{"debug"} ) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   }

# Arguments.

   my $remote_machine = $_[0];

# Get the name of this (local) host, and then get the fully qualified 
# local hostname (e.g., machine.gsfc.nasa.gov).

   $host = hostname();
   $full_local_host_name = ( gethostbyname( $host ) )[0];

# Get the domain name for the local host.

   ( $local_host_name, $local_domain_name ) = split( /\./, $full_local_host_name, 2 );

# Similarly get the name of the remote machine and its fully qualified name, and
# its domain name.

   $full_remote_host_name = ( gethostbyname( $remote_machine ) )[0];
   ( $remote_host_name, $remote_domain_name ) = split( /\./, $full_remote_host_name, 2 );
   $remote_host_name =~ s/-\w(tm)*$//;

   if ( $debug ) {
      print "(check_local):\n";
      print "~~~~~~~~~~~~~~\n";
      print " full_local_host_name = ";
      if ($full_local_host_name)  { print "$full_local_host_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "      local_host_name = ";
      if ($local_host_name)       { print "$local_host_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "    local_domain_name = ";
      if ($local_domain_name)     { print "$local_domain_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
      print "full_remote_host_name = ";
      if ($full_remote_host_name) { print "$full_remote_host_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "     remote_host_name = ";
      if ($remote_host_name)      { print "$remote_host_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "   remote_domain_name = ";
      if ($remote_domain_name)    { print "$remote_domain_name\n"; }
      else                        { print "NOT DEFINED\n"; }
      print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
   }

# Check for local transfer
   if ( $full_remote_host_name eq $full_local_host_name ) {
       print "(check_local): Local Transfer: No need to use network.\n" if $debug;
       $LOCAL = 1;
   }else{
       $LOCAL = 0;
   }

   return ( $LOCAL,
            $local_host_name,
            $local_domain_name,
            $remote_host_name,
            $remote_domain_name ); 
}

#=====================  Check for .netrc ==========================

sub check_ftp {

# Variables
my ( $use_ftp, $debug, $netrc_obj, %options ); 

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

# Debug option.

   if ( exists( $options{"debug"}) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   }

# Arguments.
   my $remote_id = $_[0];
   my $remote_machine = $_[1];
   my $home = eval { (getpwuid($>))[7] } || $ENV{HOME};
   my $user_netrc = $home . "/.netrc";

# Initialize use_ftp
      $use_ftp = 0;

# Check .netrc file for valid entry
   print "(check_ftp): \$user_netrc = $user_netrc\n" if ($debug);
   if ( ! -d $user_netrc && -e $user_netrc && -r $user_netrc && ! -z $user_netrc && ! -x $user_netrc ) {
      $netrc_obj = Net::Netrc->lookup( "$remote_machine", "$remote_id" );
      $in_netrc = defined( $netrc_obj );
      $use_ftp = 1 if ( $in_netrc );
   }
# Check for anonymous login
   if ( ( ! $use_ftp ) && (defined($remote_id) ) && ($remote_id eq "anonymous")) { $use_ftp = 1;}

# Report status
   if ($debug) {
     print "(check_ftp):\$remote_id  = $remote_id\n";
     print "(check_ftp):\$remote_machine  = $remote_machine\n";
     print "(check_ftp):\$use_ftp = $use_ftp\n";
   }     

return $use_ftp;
}

#=====================  Remote puts  ============================

sub rput {
    use Extract_config;
    use Sys::Hostname;
    use File::Basename; # fileparse(), basename(), dirname()

    my ($LOCAL, $local_host_name, $local_domain_name);
    my ($remote_host_name, $remote_domain_name, %isdir_options);
    my $dmftag = $ENV{DMFTAG};

   if ( $#_ < 1 ) {
      print STDERR <<'ENDOFHELP';

Usage: rput( local_file, remote_address:remote_file, options );

   local_file
         Name of file on this machine.

   remote_address:remote_file
         remote_address is the name of remote machine in the forms:
            remote_user_id@remote_machine or
            remote_machine
         In the 2nd case the same user id as on the local (this) 
         machine is assumed (see rcp).  

         remote_file is the name to be given to the file on the remote 
         machine.

         If the remote address is determined to be the same as the 
         local address, the transfer will be done with copy();
 
  options
         A reference to a hash array containing the options to use.  
         Accepted option names (given as keys) and possible values are

         debug => 0 or 1
            0 no debug messages - default
            1 print debug messages

         userid => string
            User ID on ATM "gateway" machines.  Default is user ID on 
            current machine.

         run_config => string
            Name of file (with path name, if necessary) to read to obtain the
            run-time (execution) configuration parameters.  rput needs the 
            name of the remote shell command to use on this machine (rsh or 
            remsh).

            If given, rput uses this file.  Otherwise, rput looks for a
            file named "Run_Config" in the user's home directory, then in the
            same directory as the program that invoked rput.

         direct => 0 or 1
            0 Attempt to use the ATM line if necessary - default.
            1 Don't use the ATM line - just initiate a one hop rcp.

         mode => string
            Sets the mode of the remote file after writing.  Set this to a
            string, like '0644', not a numeric value.  The default is '0644'.

ENDOFHELP
      exit 1;
   }

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Debug option.

   if ( exists( $options{"debug"} ) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   }

# clean option.

   if ( exists( $options{"clean"} ) ) {
      $clean = $options{"clean"};
   } else {
      $clean = 0;
   }


# User ID on gateway machines.

   if ( exists( $options{"userid"} ) ) {
      $USERID_ATM_GATEWAY = $options{"userid"};
   } else {
      $USERID_ATM_GATEWAY = getlogin || (getpwuid($<))[0] || die "(rput) ERROR - can not get login ID";
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Direct transfer request.

   if ( exists( $options{"direct"} ) ) {
#      $USE_ATM = ! $options{"direct"};
# Modified for ATM/NREN being discontinued 20011010
       $USE_ATM = 0;
   } else {
# Modified for ATM/NREN being discontinued 20011010 
#      $USE_ATM = 1;
      $USE_ATM = 0;
   }

# File permissions mode.

   if ( exists( $options{"mode"} ) ) {
      $mode = $options{"mode"};
   } else {
      $mode = '0644';
   }

# Get arguments.

# Set the name of the local file.

   $local_file = $_[0];

# Set the remote ID, machine name and file.

   ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[1] );

   if ( $debug ) {
      print "(rput):\n";
      print "dmftag = $dmftag\n";
      print "remote_id = $remote_id\n";
      print "remote_machine = $remote_machine\n";
      print "remote_file = $remote_file\n";
   }

# Get remote shell command to use on this machine (rsh or remsh).

   ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
      or die "(rput) ERROR - can not set REMSH configuration value\n";

# Check for local transfers
    ( $LOCAL,
      $local_host_name,
      $local_domain_name, 
      $remote_host_name,
      $remote_domain_name ) = check_local($remote_machine);

# If the remote file name is actually a directory, append the name of the
# local file to that. 

    %isdir_options = ( 'run_config' => $RUN_CONFIG_FILE, 'verbose' => $debug );

   if ($LOCAL) {
        $is_rdir = (-d $remote_file);
        $is_rdir = 0 if ( ! defined($is_rdir) );
   }elsif ( defined($full_remote_host_name) ) {
        $is_rdir = is_remote_dir( $remote_id, $full_remote_host_name, $remote_file, \%isdir_options );
   }else {
        $is_rdir = is_remote_dir( $remote_id, $remote_machine, $remote_file, \%isdir_options );
   }

   if ( $is_rdir == 1 ) {
      $local_file_name_only = ( fileparse( $local_file, "" ) )[0];
      $remote_file = join( '', $remote_file, "/", $local_file_name_only );
   } elsif ( $is_rdir < 0 ) {
      print "ERROR: Can not determine file status on remote machine $remote_machine, user $remote_id, file $remote_file\n";
      return(0);
   }

# Use the ATM line UNLESS:
#   The local and remote machines are in the same domain or
#   There is a request to not use it (USE_ATM==0).

   if ( $USE_ATM && ($local_domain_name ne $remote_domain_name) && ( ! $LOCAL ) ) {

# Create a temporary name for the file during transit

   $file_name_only = ( fileparse( $remote_file, "" ) )[0];
   $TEMP_FILE_NAME = join( '', $file_name_only, "_", $$ );

   if ( $debug ) {
      print "file_name_only = ", $file_name_only, "\n";
      print "TEMP_FILE_NAME = ", $TEMP_FILE_NAME, "\n";
   }
# Get ATM gateway info.

   ( $GATEWAY_GSFC = extract_config( "GATEWAY_GSFC", $RUN_CONFIG_FILE, "molotov1.gsfc.nasa.gov" ) ) ne ""
      or die "(rput) ERROR - can not set GATEWAY_GSFC configuration value\n";

   ( $ATM_GATEWAY_GSFC = extract_config( "ATM_GATEWAY_GSFC", $RUN_CONFIG_FILE, "molotov1-a.gsfc.nasa.gov" ) ) ne ""
      or die "(rput) ERROR - can not set ATM_GATEWAY_GSFC configuration value\n";

   ( $GATEWAY_ARC = extract_config( "GATEWAY_ARC", $RUN_CONFIG_FILE, "dixon0.nas.nasa.gov" ) ) ne ""
      or die "(rput) ERROR - can not set GATEWAY_ARC configuration value\n";

   ( $ATM_GATEWAY_ARC = extract_config( "ATM_GATEWAY_ARC", $RUN_CONFIG_FILE, "dixon0-atm.nas.nasa.gov" ) ) ne ""
      or die "(rput) ERROR - can not set ATM_GATEWAY_ARC configuration value\n";

   $USERID_GATEWAY_GSFC = $USERID_ATM_GATEWAY;
   $USERID_GATEWAY_ARC = $USERID_ATM_GATEWAY;

   ( $SCRATCH_GATEWAY_GSFC = extract_config( "SCRATCH_GATEWAY_GSFC", $RUN_CONFIG_FILE, "/raid1/$USERID_GATEWAY_GSFC" ) ) ne ""
      or die "(rput) ERROR - can not set SCRATCH_GATEWAY_GSFC configuration value\n";

   ( $GDAS_BIN_GATEWAY_GSFC = extract_config( "GDAS_BIN_GATEWAY_GSFC", $RUN_CONFIG_FILE, "/home/dao_ops/GEOS_DAS/bin" ) ) ne ""
      or die "(rput) ERROR - can not set GDAS_BIN_GATEWAY_GSFC configuration value\n";

   ( $SCRATCH_GATEWAY_ARC = extract_config( "SCRATCH_GATEWAY_ARC", $RUN_CONFIG_FILE, "/scratch/$USERID_GATEWAY_ARC" ) ) ne ""
      or die "(rput) ERROR - can not set SCRATCH_GATEWAY_ARC configuration value\n";

   ( $GDAS_BIN_GATEWAY_ARC = extract_config( "GDAS_BIN_GATEWAY_ARC", $RUN_CONFIG_FILE, "/u/dao_ops/GEOS_DAS/bin" ) ) ne ""
      or die "(rput) ERROR - can not set GDAS_BIN_GATEWAY_ARC configuration value\n";


   $gateway_arc_name = ( split( /\./, $GATEWAY_ARC, 2 ) )[0];
   $gateway_gsfc_name = ( split( /\./, $GATEWAY_GSFC, 2 ) )[0];

   if ( $debug ) {
      print "gateway_arc_name = ", $gateway_arc_name, "\n";
      print "gateway_gsfc_name = ", $gateway_gsfc_name, "\n";
   }
 

# Begin creating the paths to use. 

      SWITCH_PATHS: {

# CASE 1: At NAS, not on ARC gateway.

         if ( $local_domain_name eq "nas.nasa.gov" && $local_host_name ne $gateway_arc_name ) {

            if ( $debug ) {
               print "Case 1: At NAS, not on $GATEWAY_ARC.\n";
            }

#    Going only as far as GSFC gateway.

            if ( $remote_host_name eq $gateway_gsfc_name ) {

               @transfer_machines = ( $GATEWAY_ARC, $ATM_GATEWAY_GSFC );
               @transfer_ids      = ( $USERID_GATEWAY_ARC, $USERID_GATEWAY_GSFC );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_ARC, $TEMP_FILE_NAME ), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_ARC, "" );

#    Going beyond GSFC gateway.

            } else {

               @transfer_machines = ( $GATEWAY_ARC, $ATM_GATEWAY_GSFC, $full_remote_host_name );
               @transfer_ids      = ( $USERID_GATEWAY_ARC, $USERID_GATEWAY_GSFC, $remote_id );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_ARC, $TEMP_FILE_NAME ),
                                      join( '/', $SCRATCH_GATEWAY_GSFC, $TEMP_FILE_NAME ), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_ARC, $GDAS_BIN_GATEWAY_GSFC, "" );

            }

            last SWITCH_PATHS;

         }

# CASE 2: At NAS, on ARC gateway.

         if ( $local_domain_name eq "nas.nasa.gov" && $local_host_name eq $gateway_arc_name ) {

            if ( $debug ) {
               print "Case 2: At NAS, on $GATEWAY_ARC.\n";
            }

#    Going only as far as GSFC gateway.

            if ( $remote_host_name eq $gateway_gsfc_name ) {

               @transfer_machines = ( $ATM_GATEWAY_GSFC );
               @transfer_ids      = ( $USERID_GATEWAY_GSFC );
               @transfer_paths    = ( $remote_file );
               @geosdas_bin       = ( "" );

#    Going beyond GSFC gateway.

            } else {

               @transfer_machines = ( $ATM_GATEWAY_GSFC, $full_remote_host_name );
               @transfer_ids      = ( $USERID_GATEWAY_GSFC, $remote_id );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_GSFC, $TEMP_FILE_NAME), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_GSFC, "" );

            }

            last SWITCH_PATHS;

         }

# CASE 3: At GSFC, not on GSFC gateway.

         if ( $local_domain_name eq "gsfc.nasa.gov" && $local_host_name ne $gateway_gsfc_name ) {

            if ( $debug ) {
               print "Case 3: At GSFC, not on $GATEWAY_GSFC.\n";
            }

#    Going only as far as ARC gateway.

            if ( $remote_host_name eq $gateway_arc_name ) {

               @transfer_machines = ( $GATEWAY_GSFC, $ATM_GATEWAY_ARC );
               @transfer_ids      = ( $USERID_GATEWAY_GSFC, $USERID_GATEWAY_ARC );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_GSFC, $TEMP_FILE_NAME ), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_GSFC, "" );

#    Going beyond ARC gateway.

            } else {

               @transfer_machines = ( $GATEWAY_GSFC, $ATM_GATEWAY_ARC, $full_remote_host_name );
               @transfer_ids      = ( $USERID_GATEWAY_GSFC, $USERID_GATEWAY_ARC, $remote_id );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_GSFC, $TEMP_FILE_NAME ), join( '/', $SCRATCH_GATEWAY_ARC, $TEMP_FILE_NAME ), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_GSFC, $GDAS_BIN_GATEWAY_ARC, "" );

            }

            last SWITCH_PATHS;

         }

# CASE 4: At GSFC, on GSFC gateway.

         if ( $local_domain_name eq "gsfc.nasa.gov" && $local_host_name eq $gateway_gsfc_name ) {

            if ( $debug ) {
               print "Case 4: At GSFC, on $GATEWAY_GSFC.\n";
            }

#    Going only as far as ARC gateway.

            if ( $remote_host_name eq $gateway_arc_name ) {

               @transfer_machines = ( $ATM_GATEWAY_ARC );
               @transfer_ids      = ( $USERID_GATEWAY_ARC );
               @transfer_paths    = ( $remote_file );
               @geosdas_bin       = ( "" );

#    Going beyond ARC gateway.

            } else {

               @transfer_machines = ( $ATM_GATEWAY_ARC, $full_remote_host_name );
               @transfer_ids      = ( $USERID_GATEWAY_ARC, $remote_id );
               @transfer_paths    = ( join( '/', $SCRATCH_GATEWAY_ARC, $TEMP_FILE_NAME ), $remote_file );
               @geosdas_bin       = ( $GDAS_BIN_GATEWAY_ARC, "" );

            }

            last SWITCH_PATHS;

         }

# None of the above - don't use the ATM line after all.

         DEFAULT_PATHS: {

            if ( $debug ) {
               print "Default case: None of the above.\n";
            }

            @transfer_machines = ( $full_remote_host_name );
            @transfer_ids      = ( $remote_id );
            @transfer_paths    = ( $remote_file );
            @geosdas_bin       = ( "" );

         }
     }
# Don't use the ATM line.

   } else { 
          if ( $debug ) {
                 print "Not using ATM line at all.\n";
          }

         if ( defined($full_remote_host_name) ) {
            @transfer_machines = ( $full_remote_host_name );
         }else{
            @transfer_machines = ( $remote_machine );
         }
         @transfer_ids      = ( $remote_id );
         @transfer_paths    = ( $remote_file );
         @geosdas_bin       = ( "" );
   }

# Check for local copy

   if ( $LOCAL ){
        if ( $debug ) {
             print "(rput):\n";
             print "Local Transfer: Not using network at all.\n";
             print "host name   = $full_local_host_name\n";
             print "local file  = $local_file\n";
             print "remote file = $remote_file\n";
             print "clean = $clean\n";
        }
        if ( -l $local_file ) {
                $trans_retcode = copy( $local_file, $remote_file);
                $chmod_retcode = chmod( oct(${mode}), ${remote_file});
                $trans_retcode = ($trans_retcode && $chmod_retcode);
                unlink ${local_file} if ( $trans_retcode && $clean ); 
        }else{
             $trans_retcode = move( $local_file, $remote_file) if ( $clean );
             $trans_retcode = copy( $local_file, $remote_file) unless ( $clean );
             $chmod_retcode = chmod( oct(${mode}), ${remote_file});
             $trans_retcode = ($trans_retcode && $chmod_retcode);
        }
        if (-e $dmftag ) {
            my $cmd = "$dmftag -t 2 ";
            my $newretcode;
            $cmd = $cmd . "$remote_file";
            print "(rput) command = $cmd\n";
            $newretcode = `$cmd 2>&1`;
            if ($newretcode =~ /^dmtag failed!/) {
               system ("sleep 5");
               print "(rput) dmtag failed; trying again\n";
               print "newretcode = $newretcode and the file = $remote_file\n";
               $newretcode = `$cmd 2>&1`;
            }
            if ($newretcode =~ /^dmtag failed!/) {
               system ("sleep 10");
               print "(rput) dmtag failed; trying third time\n";
               print "newretcode = $newretcode and the file = $remote_file\n";
               $newretcode = `$cmd 2>&1`;
            }
            print "newretcode = $newretcode and the file = $remote_file\n";
            die "(rput) ERROR Permission denied on target file - $remote_file \n" if ($newretcode =~ /Permission denied/);
            die "(rput) ERROR Target file does not exist - $remote_file \n" if ($newretcode =~ /does not exist/);
        }
   } else {      

# Transfer the file using the specified pathways.

       if ( $debug ) {
            print "(rput):\n";
            print "local_file = ", $local_file, "\n";
            print "transfer_machines = ", "@transfer_machines", "\n";
            print "transfer_ids = ", "@transfer_ids", "\n";
            print "transfer_paths = ", "@transfer_paths", "\n";
            print "geosdas_bin = ", "@geosdas_bin", "\n";
            print "run_config = ", "$RUN_CONFIG_FILE", "\n";
       }
       %transf_options = ( 'run_config' => $RUN_CONFIG_FILE);
       $trans_retcode = transfile_put( $local_file, 
           \@transfer_machines, \@transfer_ids, \@transfer_paths, \@geosdas_bin, $mode, \%transf_options );
       $trans_retcode=unlink ${local_file} if ( $trans_retcode && $clean );
   }
   return ( $trans_retcode );

}


sub transfile_put {

# Get subroutine arguments.

   my $local_file        = $_[0];
   my @transfer_machines = @{ $_[1] };
   my @transfer_ids      = @{ $_[2] };
   my @transfer_paths    = @{ $_[3] };
   my @geosdas_bin       = @{ $_[4] };
   my $mode              = $_[5];

# Get options (from last argument, if it is a hash reference).

if ( ref( $_[-1] ) eq "HASH" ) {
     my %options = %{ pop( @_ ) };
   }

# run_config option.

   if ( exists( $options{"run_config"} ) ) {
      $run_config = $options{"run_config"};
   } else {
      $run_config = "DEFAULT";
   }

   $nhops = $#transfer_machines + 1;

   if ( $debug ) {
      print "(transfile_put):\n";
      print "nhops = ", $nhops, "\n";
      print "local_file = ", $local_file, "\n";
      print "transfer_machines = ", "@transfer_machines", "\n";
      print "transfer_ids = ", "@transfer_ids", "\n";
      print "transfer_paths = ", "@transfer_paths", "\n";
      print "geosdas_bin = ", "@geosdas_bin", "\n";
      print "mode = ", $mode, "\n";
      print "run_config = ", $run_config, "\n";
   }

# 
# Remove any "-a" or "-atm" suffixes in the remote machine names.  This is to get a set
# of names that can be used to do remote shell commands.

   $irtm = 0;
   foreach $rtm ( @transfer_machines ) {
      ( $remsh_machines[$irtm] = $rtm ) =~ s/-\w(tm)*\././;
      ++$irtm;
   }

# First hop.
   my %hop_options = ('run_config' => $run_config );
   $iret = one_hop_put( "", 
      "", "", $local_file, 
      $transfer_machines[0], $transfer_ids[0], $transfer_paths[0], $mode , \%hop_options );

# Subsequent hops.  As each hop completes, remove the file at the previous location.

   $ihop = 1;
   my %hops_options = ('run_config' => 'DEFAULT');

   while ( $iret && $ihop < $nhops ) {

      $iret = one_hop_put( $geosdas_bin[$ihop-1], 
         $transfer_machines[$ihop-1], $transfer_ids[$ihop-1], $transfer_paths[$ihop-1], 
         $transfer_machines[$ihop], $transfer_ids[$ihop], $transfer_paths[$ihop], $mode, \%hops_options );

      rm_remote_file( $transfer_ids[$ihop-1], $remsh_machines[$ihop-1], $transfer_paths[$ihop-1], \%hops_options );

      ++$ihop;

   }

# Send back the return code from the file transfers.

   return( $iret );

}


sub one_hop_put {

   my $geosdas_bin0           = $_[0];

   my $transfer_machines0     = $_[1];
   my $transfer_ids0          = $_[2];
   my $transfer_paths0        = $_[3];

   my $transfer_machines1     = $_[4];
   my $transfer_ids1          = $_[5];
   my $transfer_paths1        = $_[6];

   my $mode                   = $_[7];

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      my %options = %{ pop( @_ ) };
   }

# run_config option.

   if ( exists( $options{"run_config"} ) ) {
      $run_config = $options{"run_config"};
   } else {
      $run_config = "DEFAULT";
   }

# This module contains an interface to the .netrc file.

   use Net::Netrc;

   if ( $debug ) {
      print STDERR "(one_hop_put) Copying $transfer_ids0 @ $transfer_machines0 : $transfer_paths0 TO $transfer_ids1 @ $transfer_machines1 : $transfer_paths1 using run_config = $run_config", "\n";
      print STDERR "(one_hop_put) geosdas_bin0: $geosdas_bin0\n";
   }

# ---------------------------------------------------------------------------------
# If the first machine is passed in as a null, then one_hop_put should transfer a file
# on this machine of name $transfer_paths0 to the remote machine and location given
# in the last 4 arguments.
# ---------------------------------------------------------------------------------

   if ( $transfer_machines0 eq "" ) {

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.
       $use_ftp = check_ftp($transfer_ids1, $transfer_machines1);


# If there is login information, use ftp to transfer the file.

      if ( $use_ftp ) {

         if ( $debug ) {
            print STDERR "(one_hop_put) FTP attempt of $transfer_paths0 TO $transfer_ids1\@$transfer_machines1:$transfer_paths1\n";
         }

         $itry = 0;
         do {
            $ftp_ok = ftp_put( $transfer_paths0, $transfer_ids1, $transfer_machines1, $transfer_paths1 );
            ++ $itry;
         } until $ftp_ok || $itry > 3;

         if ( ! $ftp_ok ) {
            print STDERR "(one_hop_put) FAILED FTP attempt of $transfer_paths0 TO $transfer_ids1\@$transfer_machines1:$transfer_paths1\n";
         }

         $retcode = $ftp_ok;

# Else if there is no ftp login information, use rcp to transfer the file.  (Assumes 
# .rhosts is set properly on the remote machine.)

      } else {

         if ( $debug ) {
            print STDERR "(one_hop_put) RCP attempt of $transfer_paths0 TO $transfer_ids1\@$transfer_machines1:$transfer_paths1\n";
         }

         $itry = 0;
         do {
            %options = ( 'run_config' => $run_config );
            $rcp_ok = rcp_put( $transfer_paths0, $transfer_ids1, $transfer_machines1, $transfer_paths1, \%options );
            ++ $itry;
         } until $rcp_ok || $itry > 3;

         if ( ! $rcp_ok ) {
            print STDERR "(one_hop_put) FAILED RCP attempt of $transfer_paths0 TO $transfer_ids1\@$transfer_machines1:$transfer_paths1\n";
         }

         $retcode = $rcp_ok;

      }

# If the transfer was successful, chmod the remote file.

      if ( $retcode ) {
         chmod_remote( $transfer_ids1, $transfer_machines1, $transfer_paths1, $mode );
      }

# ---------------------------------------------------------------------------------
# In this case, send a remote shell command to the first machine to invoke the 
# one_hop_put command (a wrapper for this subroutine) on that machine.  It will push
# the file to the next machine.
# ---------------------------------------------------------------------------------

   } else {

# Strip "-a" or "-atm" from remote machine name.  This will be used to do a remote shell 
# to that machine.

      ( $remsh_machine0 = $transfer_machines0 ) =~ s/-\w(tm)*\././;

      $cmd = "$REMSH $remsh_machine0  -l $transfer_ids0 -n $geosdas_bin0/one_hop_put -m $mode $transfer_paths0 $transfer_ids1\@$transfer_machines1:$transfer_paths1";

      if ( $debug ) {
         print STDERR "(one_hop_put) Remote command issued: ", $cmd, "\n";
      }

      system( $cmd );
      $retcode = $? >> 8;

      if ( $debug ) {
         print STDERR "(one_hop_put) Remote command retcode = ", $retcode, "\n";
      }

# Change the return code so that 1 indicates success.  (Scripts and script wrappers
# return 0 on success, ala unix, perl subroutines return "true" on success.)

      $retcode = ! $retcode;

   }

# Print a message.

   if ( $debug ) {
      $tran_tag = "(one_hop_put) Transfer of $transfer_ids0\@$transfer_machines0:$transfer_paths0 TO $transfer_ids1\@$transfer_machines1:$transfer_paths1";
      if ( $retcode ) {
         print STDERR "$tran_tag - SUCCESSFUL\n";
      } else {
         print STDERR "tran_tag - FAILED\n";
      }
   }

   return( $retcode );

}


sub ftp_put {

# Get arguments.

   my $local_file             = $_[0];

   my $rem_loc;
   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $ftp_ok;


# This module contains the basename() subroutine.

   use File::Basename;

   use Sys::Hostname;
   $host = hostname();
   $full_local_host_name = ( gethostbyname( $host ) )[0];

   if ( $#_ == 1 ) {
      $rem_loc = $_[1];
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $rem_loc );
   } else {
      ( $remote_id, $remote_machine, $remote_file ) = @_[1..3];
   }

# See if the .netrc file contains the login information.  lookup will only return
# a defined value if it does.


# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

     $use_ftp = check_ftp($remote_id, $remote_machine);

     if( $use_ftp == 0 && $remote_id eq "anonymous" && $password eq "" ) {
#      --------------------------------------------------------------
#       If the remote_id and the password does not exist in the
#       .netrc file and the remote_id is "anonymous", then
#       extract the user name and the host name to create a password
#       for "anonymous ftp" session.
#      --------------------------------------------------------------
       use Sys::Hostname;

       my $user = eval { (getpwuid($>))[0] } || $ENV{HOME};
       my $host = hostname;

#    ------------------------------------------------------------
#      The following changes are made to get the full host name and
#      joined together to create a password for anonymous ftp session.
#    ------------------------------------------------------------

       $full_local_host_name = ( gethostbyname( $host ) )[0];
       $password = join("@",$user,$full_local_host_name);
       $password_alt = "${user}@";
   }

   $ftp_ok = 1;
   if ( "$full_local_host_name" ne "$remote_machine" ) {

#     This modules contains the ftp interface.

     use Net::FTP;

     $ftp_ok = 1;

     $ftp = Net::FTP->new( "$remote_machine", Debug => 1 ) or $ftp_ok = 0;
     if ( $use_ftp ) {
        $ftp->login() or $ftp_ok = 0;
     } else {
         if ( $password eq "" ) {
            return( ( "ERROR", "2", "Password not available" ) );
         } else {
           $ftp->login("$remote_id","$password") or $ftp_ok = 0;
            if ( ! ${ftp_ok} ) {
               $ftp_ok = 1;
               $ftp->login("$remote_id","$password_alt") or $ftp_ok = 0;
            }
         }
     }

        $ftp->binary();
        $ftp->put( $local_file, $remote_file ) or $ftp_ok = 0;
        $ftp->quit;
        return( $ftp_ok );
   } else {
         $cmd = "cp $local_file $remote_file";
          system( $cmd );
          return( $ftp_ok );
   }
}
1;



sub rcp_put {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Get arguments.

   my $local_file = $_[0];

   my $rem_loc;
   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $rcp_ok;
   my $newretcode;
   my $cmd;
   my $dmftag = $ENV{DMFTAG};

# This module contains the extract_config() subroutine.

   use Extract_config;

# This module contains the hostname() subroutine.

   use Sys::Hostname;

# This module contians the copy() subroutine.

   use File::Copy;

   $host = hostname();
   $full_local_host_name = ( gethostbyname( $host ) )[0];

   if ( $#_ == 1 ) {
      $rem_loc = $_[1];
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $rem_loc );
   } else {
      ( $remote_id, $remote_machine, $remote_file ) = @_[1..3];
   }

# If this is a remote transfer (between different machines), use the appropriate
# rcp command to do it.
my ( $LOCAL, $local_host_name, $local_domain_name,
            $remote_host_name, $remote_domain_name ) = check_local($remote_machine , \%options);

   if ( ! "$LOCAL" ) {

# Get remote copy command to use on this machine.  Use what's set, otherwise 
# try to get it from the run time configuration file.

      if ( ! defined( $RCP ) ) {
         ( $RCP = extract_config( "RCP", $RUN_CONFIG_FILE, "$DEFAULT_RCP" ) ) ne ""
            or die "ERROR - can not set RCP configuration value\n";
      }

# Create a command to do the remote copy.

      $cmd = "$RCP $local_file $remote_id\@$remote_machine:$remote_file";
      print "(rcp_put): cmnd= $cmd\n";
      system( $cmd );
      $retcode = $? >> 8;
      print "(rcp_put): retcode =$retcode\n";
      $rcp_ok = ! $retcode;
      print "(rcp_put): rcp_ok =$rcp_ok\n";
      if( (-e $dmftag) && $rcp_ok ) {
          # Tag the file for 2 tape copies
          $cmd = "ssh $remote_id\@$remote_machine $dmftag -t 2 $remote_file";
          $newretcode = `$cmd 2>&1`;
          print "(rcp_put): command= $cmd\n";
          print "newretcode = $newretcode and the file = $remote_file\n";
          die "(rcp_put) ERROR Permission denied on target file - $remote_file \n" if ($newretcode =~ /Permission denied/);
          die "(rcp_put) ERROR Target file does not exist - $remote_file \n" if ($newretcode =~ /does not exist/);
      }
# Else if this is a local transfer (on the same machine), use the copy function.

   } else {
      print "cmd= copy( $local_file, $remote_file )\n" if ($debug);
      $rcp_ok = copy( $local_file, $remote_file );
      if( (-e $dmftag) && $rcp_ok ) {
          $cmd = "$dmftag -t 2 ";
          $cmd = $cmd . "$remote_file";
          $newretcode = `$cmd 2>&1`;
          print "(rcp_put): command = $cmd\n";
          print "newretcode = $newretcode and the file = $remote_file\n";
          die "(rput) ERROR Permission denied on target file - $remote_file \n" if ($newretcode =~ /Permission denied/);
          die "(rput) ERROR Target file does not exist - $remote_file \n" if ($newretcode =~ /does not exist/);
      }

   }

   return( $rcp_ok );

}

#=================  Split up remote location  ====================

sub splitrfile {

# This module contains the hostname() subroutine.

  use Sys::Hostname;
  
  my $remote_location;
  my $remote_machine;
  my $remote_file;
  my $remote_id;
  my $opt_filename;

# $extra added for http paths which might come in with two :s

  if ( $_[0] =~ /:/ ){
     ( $remote_location, $remote_file, $opt_filename ) = split( /:/, $_[0] );
  }else{
       $remote_location = hostname();
       $remote_file = $_[0];
  }
  if ( $remote_location =~ /\@/ ) {
#      ( $remote_id, $remote_machine ) = split( /@/, $remote_location );
       ( $remote_id, $remote_machine ) = split( /@([^@]+)$/, $remote_location );
  }else{
       $remote_id = getlogin || (getpwuid($<))[0] || die "(splitrfile) ERROR - can not get login ID";
       $remote_machine = $remote_location;
  }

  return ( $remote_id, $remote_machine, $remote_file, $opt_filename );

}

#=================  Remote directory check  ======================

sub is_remote_dir {

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;
   
   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
     my %options = %{ pop( @_ ) };
   } 

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }


# This module contains an interface to the .netrc file.

   use Net::Netrc;

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

    $use_ftp = check_ftp($remote_id, $remote_machine);


# If there is login information, use ftp to transfer the file.

   if ( $use_ftp ) {
      $retcode = ftp_is_remote_dir( $remote_id, $remote_machine, $remote_file, \%options );

   } else {
      $retcode = rsh_is_remote_dir( $remote_id, $remote_machine, $remote_file, \%options );
   }

   return( $retcode );

}


sub ftp_is_remote_dir {
   0;
}


sub rsh_is_remote_dir {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
     my %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }
   if ( $debug ) {
      print "(is_remote_dir):\n";
      print "remote_id = ", $remote_id, "\n";
      print "remote_machine = ", $remote_machine, "\n";
      print "remote_file = ", "$remote_file", "\n";
      print "run_config = ", "$RUN_CONFIG_FILE", "\n";
   }

# This module contains the extract_config() subroutine.

   use Extract_config;

# Get remote shell command to use on this machine (rsh or remsh).  Use 
# what's set, otherwise try to get it from the run time configuration file.

   ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
    or die "(rsh_is_remote_dir) ERROR - can not set REMSH configuration value\n";

# Try using ksh on remote machine first 

   $cmd = "$REMSH $remote_machine  -l $remote_id -n 'ksh -c \"if [[ -d $remote_file ]] ; then echo 1 ; else echo 0 ; fi ;\"'";

   if ( $debug ) {
      print "(rsh_is_remote_dir): cmd = ", $cmd, "\n";
   }

   $is_rdir = `$cmd`;
   $retcode = $? >> 8;
   if ( $retcode != 0 ) { $is_rdir = -1 }

   if ( $debug ) {
      print "(rsh_is_remote_dir): retcode = ", $retcode, "\n";
      print "(rsh_is_remote_dir): is_rdir = ", $is_rdir, "\n";
   }

   return $is_rdir;

}

#=================  Remote file removal  =========================

sub rm_remote_file {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }

# This module contains an interface to the .netrc file.

   use Net::Netrc;

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

   $use_ftp = check_ftp($remote_id, $remote_machine);


# If there is login information, use ftp to transfer the file.

   if ( $use_ftp ) {
      $retcode = ftp_rm_remote_file( $remote_id, $remote_machine, $remote_file, \%options );

   } else {
      $retcode = rsh_rm_remote_file( $remote_id, $remote_machine, $remote_file, \%options );
   }

   return( $retcode );

}


sub ftp_rm_remote_file {
   0;
}


sub rsh_rm_remote_file {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }
   
# Debug status
  if ( exists( $options{"debug"} ) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   }


# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }

# This module contains the extract_config() subroutine.

   use Extract_config;

# Get remote shell command to use on this machine (rsh or remsh).  Use 
# what's set, otherwise try to get it from the run time configuration file.

      ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
         or die "(rsh_is_remote_dir) ERROR - can not set REMSH configuration value\n";

# Create a command to do a remove on the remote machine.
   
   $cmd = "$REMSH $remote_machine -l $remote_id -n /bin/rm -f $remote_file";

   if ( $debug ) {
      print "RM_REMOTE_FILE: cmd = ", $cmd, "\n";
   }

   `$cmd`;
# retcode is multiples of 256 -- the ">> 8" shifts the answer 8 bits.
   $retcode = $? >> 8;

   return( ! $retcode );

}

#=================  Make remote directory  ========================

sub mkdir_remote {
   use File::Path;
# Variables 
   my $mode;
   my $debug;
   my $mkdir;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

   if ( exists( $options{'mode'} ) ) {
      $mode = $options{'mode'};
   } else {
      $mode = '0755';
   }

   if ( exists( $options{'debug'} ) ) {
      $debug = $options{'debug'};
   } else {
      $debug = 0;
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_dir;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_dir ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_dir     = $_[2];
   }

# This module contains an interface to the .netrc file.

   use Net::Netrc;

# Check for local machine

   my ( $LOCAL, $local_host_name, $local_domain_name,
        $remote_host_name, $remote_domain_name ) = check_local($remote_machine);

   if ( ! $LOCAL ) {

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

        $use_ftp = check_ftp($remote_id, $remote_machine);
   
# If there is login information, use ftp to transfer the file.

       if ( $use_ftp ) {
            $retcode = ftp_mkdir_remote( $remote_id, $remote_machine, $remote_dir, \%options );

       } else {
            $retcode = rsh_mkdir_remote( $remote_id, $remote_machine, $remote_dir, \%options );
       }


  }else{
       if (! -d "$remote_dir") {
           $retcode = 0;
           $cmd = "mkdir -p -m $mode $remote_dir";
           print "$cmd \n" if ( $debug );
           $mkdirc=system("$cmd");
           $rc = $mkdirc >> 8;
           print "mkdir rc = $rc \n" if ( $debug );
           $retcode = 1 if ( -d "$remote_dir");  
           print "retcode = $retcode \n" if ( $debug );
       }else{
           $retcode = 1;
       }
  }
  return( $retcode );
}
sub ftp_mkdir_remote {
   0;
}


sub rsh_mkdir_remote {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_dir;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_dir ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_dir     = $_[2];
   }

# This module contains the extract_config() subroutine.

   use Extract_config;

# Get remote shell command to use on this machine (rsh or remsh).  Use 
# what's set, otherwise try to get it from the run time configuration file.

   ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
       or die "(mkdir_remote) ERROR - can not set REMSH configuration value\n";

# Create a command to do a mkdir on the remote machine.
   $cmd = "$REMSH $remote_machine  -l $remote_id mkdir -p $remote_dir";
   print "cmd = $cmd\n" if $debug;

# Execute command
   $rc=system( $cmd );
   $result = $?;
   $retcode = $result >> 8;
 
# Print result
   print "(mkdir_remote): rc= $rc result= $result retcode= $retcode\n" if ($debug);

# Kludge for dirac which returns -1 on success
#  return( ! $retcode );
   my %isdir_options = ( 'run_config' => ${RUN_CONFIG_FILE},
                            'verbose' => "$debug" );
   $is_rdir = is_remote_dir( $remote_id, $remote_machine, $remote_dir, \%isdir_options );
   
   return(  $is_rdir );

}

#=============  Change remote file permissions  ===================

sub chmod_remote {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

   if ( exists( $options{"debug"} ) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $mode;

   if ( $#_ == 1 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
      $mode = $_[1];
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
      $mode           = $_[3];
   }

# This module contains an interface to the .netrc file.

   use Net::Netrc;


# Check for local machine

   my ( $LOCAL, $local_host_name, $local_domain_name,
        $remote_host_name, $remote_domain_name ) = check_local($remote_machine);

   if ( ! $LOCAL ) {

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

        $use_ftp = check_ftp($remote_id, $remote_machine);
  
# If there is login information, use ftp to chmod the file.

        if ( $use_ftp ) {
           $retcode = ftp_chmod_remote( $remote_id, $remote_machine, $remote_file, $mode, \%options );

        } else {
           $retcode = rsh_chmod_remote( $remote_id, $remote_machine, $remote_file, $mode, \%options );
        }

        return( $retcode );

   } else {

        $cmd = "chmod $mode $remote_file";
        print "cmd = $cmd \n" if ( $debug );
        system( "$cmd" );
        $rc = $? >> 8;
        $retcode = ( ! $rc );

   }
 
return ($retcode);
}

sub ftp_chmod_remote {
   0;
}


sub rsh_chmod_remote {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Recursive option.  (Chmod each directory given.)

   if ( exists( $options{"recursive"} ) ) {
      $recursive = $options{"recursive"};
   } else {
      $recursive = 0;
   }
# Verbose option for debugging

   if ( exists( $options{"verbose"} ) ) {
      $verbose = $options{"verbose"};
   } else {
      $verbose = 0;
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $mode;

   if ( $#_ == 1 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
      $mode = $_[1];
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
      $mode           = $_[3];
   }

# This module contains the extract_config() subroutine.

   use Extract_config;

# Get remote shell command to use on this machine (rsh or remsh).  Use 
# what's set, otherwise try to get it from the run time configuration file.

      ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
         or die "(chmod_remote) ERROR - can not set REMSH configuration value\n";

# If the "recursive" option is set, split the path and chmod each directory if
# owned by the user.

   if ( $recursive ) {

      @all_dirs = split( '/', $remote_file );

      $ii = 0;
# If path begins with /, get rid of the first (blank) part_dir from split
      if ($remote_file=~ /^\//) {
           $part_dir = "/".$all_dirs[1];
           $ii ++;
      }else{
           $part_dir = $all_dirs[0];
      }

      do {
         $cmd = "$REMSH $remote_machine -l $remote_id  /bin/ls -do $part_dir";
         $ls_ret = ` $cmd `;
         @ls_array = &ex_array($ls_ret);
         $rem_owner = $ls_array[2];
         if ($rem_owner eq $remote_id){
             $cmd = "$REMSH $remote_machine -l $remote_id -n chmod $mode $part_dir";
             if ($verbose) {
                  print "Doing chmod $mode on $part_dir \n";
                  print "$cmd\n";
             }
              system( $cmd );
              $retcode = $? >> 8;
              if ($verbose) {
                   print "retcode = $retcode\n";
              }
         }
         ++ $ii;
         $part_dir = join( "/", $part_dir, $all_dirs[$ii] ) if $ii <= $#all_dirs;

      } until $ii > $#all_dirs;

# Otherwise, do a single chmod on the remote machine.

   } else {

      $cmd = "$REMSH $remote_machine  -l $remote_id -n chmod $mode $remote_file";
      if ($verbose) {
          print "Doing chmod $mode on $remote_file \n";
          print "$cmd\n";
      }

      system( $cmd );
      $retcode = $? >> 8;
      if ($verbose) {
          print "retcode = $retcode\n";
      }
   }

   return( ! $retcode );

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
 
#
#=============  dmget remote files  ================================
# Allows user to issue a remote dmget before retrieving files from
# dmf file systems.
#
sub rdmget {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Remote dmget binary

   if ( exists( $options{"dmget_bin"} ) ) {
      $DMGET_CMD = $options{"dmget_bin"};
      ( ( $dmget_id, $dmget_machine, $DMGET_CMD ) = splitrfile( $DMGET_CMD ) ) if ( $DMGET_CMD =~/:/ );
   } else {
      $DMGET_CMD = "/usr/bin/dmget";
   }

# Remote dmput binary

   if ( exists( $options{"dmput_bin"} ) ) {
      $DMPUT_CMD = $options{"dmput_bin"};
      ( ( $dmput, $dmput_machine, $DMPUT_CMD ) = splitrfile( $DMPUT_CMD ) ) if ( $DMPUT_CMD =~/:/ );
   } else {
     ($DMPUT_CMD = $DMGET_CMD)=~ s/get/put/;
   }

# Set put option

   if ( exists( $options{"do_dmput"} ) ) {
      $do_dmput = $options{"do_dmput"};
   } else {
      $do_dmput = 0;
   }

# Set verbose option

   if ( exists( $options{"verbose"} ) ) {
      $debug = $options{"verbose"};
   } else {
      $debug = 0;
   }

   my $remote_id;
   my $remote_machine;
   my $remote_files;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_files   = $_[2];
   }

   ( $remote_id = $dmget_id ) if defined($dmget_id);
   ($remote_machine = $dmget_machine ) if defined($dmget_machine);

# Check for local machine

   my ( $LOCAL, $local_host_name, $local_domain_name,
        $remote_host_name, $remote_domain_name ) = check_local($remote_machine);

# If command is to be run locally set cmd

   if ( $LOCAL ) {
        $cmd = "$DMGET_CMD  @${remote_files}";
        $cmd = "$DMPUT_CMD -r -n @${remote_files}" if ( $do_dmput );
   }else{

# This module contains the extract_config() subroutine.

        use Extract_config;

# Get remote shell command to use on this machine (rsh or remsh).  Use
# what's set, otherwise try to get it from the run time configuration file.
  
        if ( ! defined( $REMSH ) ) {
           ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
              or die "(rdmget) ERROR - can not set REMSH configuration value\n";
        }

# Create a command to do a dmget on the remote machine.

        $cmd = "$REMSH $remote_machine -l $remote_id $DMGET_CMD \' @${remote_files}\'";
        $cmd = "$REMSH $remote_machine -l $remote_id $DMPUT_CMD \' @${remote_files}\'" if ( $do_dmput );
   }
   if ($debug) {
        print "Doing dmget cmd = $cmd \n" if ( ! $do_dmput );
        print "Doing dmput cmd = $cmd \n" if ( $do_dmput );

   }
   system( "$cmd\&" );
   $retcode = $? >> 8;
   if ($debug) {
       print "dmget retcode = $retcode\n";
   }
   return( ! $retcode );

}

#=============  List remote files  ================================

sub rflist {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   print "$#_ $_[0] \n";
   $url = $_[0];
   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file, $extra ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }
#  print "0 remote_id = $remote_id\n"; 
#  print "0 remote_machine = $remote_machine\n";
#  print "0 remote_file = $remote_file\n";
#  print "0 extra = $extra\n";

# This module contains an interface to the .netrc file.

   use Net::Netrc;

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

   $use_ftp = check_ftp($remote_id, $remote_machine);

# If there is login information, use ftp to check the file.

   if ( $remote_machine eq "http" || $remote_machine eq "https" ) {
# Reconstruct URL without extra : characters.
      $url         = "http:${remote_file}${extra}";
      $dir         = dirname ("$url");
      $regex_raw   = basename ("$url");
      $regex_raw   =~s/\*/\.\*/g;
      $regex       = "\^${regex_raw}\$";
      print "Using regex=$regex\n";
      print "$DEFAULT_WGET -O - $dir\n";

      $string=`$DEFAULT_WGET -O - $dir | grep -o \'<a href=[\'\"\'\"\'\"][^\"\'\"\'\"\']*[\'\"\'\"\'\"]\' |  sed -e \'s/\^<a href=[\"\'\"\'\"\']//\' -e \'s/\[\"\'\"\'\"\'\]\$//\' | grep -v -e \'\?\' -e \'\/\' | grep -E $regex | uniq `;
      print "POPALI string = $string\n";
      @retlist = split(/\n/,$string);
   }
 
   elsif ( $use_ftp ) {
      @retlist = ftp_rflist( $remote_id, $remote_machine, $remote_file, \%options );

   } else {
      @retlist = rsh_rflist( $remote_id, $remote_machine, $remote_file, \%options );
   }

   return( @retlist );

}


sub ftp_rflist {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Size option.

   if ( exists( $options{"size"} ) ) {
      $size = $options{"size"};
   } else {
      $size = 0;
   }

# Password

   if ( exists( $options{"password"} ) ) {
      $password = $options{"password"};
   } else {
      $password = "";
   }

# Return full path or just file name(s).

   if ( exists( $options{"full_path"} ) ) {
      $full_path = $options{"full_path"};
   } else {
      $full_path = 0;
   }

# Check paasive ftp argument

   if ( exists( $options{"passive"} ) ) {
      $passive = $options{"passive"};
      print "Passive detected in options.  Passive=$passive\n";
   } else {
      $passive = 1;
      print "Passive not detected.  Default set to 1\n";
   }


# This modules contains the ftp interface.

   use Net::FTP;

# This module contains an interface to the .netrc file.

   use Net::Netrc;

# This module contains the basename() subroutine.

   use File::Basename;

# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }

# See if the .netrc file contains the login information.  lookup will only return
# a defined value if it does.

# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

   $use_ftp = check_ftp($remote_id, $remote_machine);

   if( $use_ftp == 0 && $remote_id eq "anonymous" && $password eq "" ) { 

#      --------------------------------------------------------------
#       If the remote_id and the password does not exist in the
#       .netrc file and the remote_id is "anonymous", then
#       extract the user name and the host name to create a password
#       for "anonymous ftp" session.
#      --------------------------------------------------------------
       use Sys::Hostname;

       my $user = eval { (getpwuid($>))[0] } || $ENV{HOME};
       my $host = hostname;

#    ------------------------------------------------------------
#      The following changes are made to get the full host name and
#      joined together to create a password for anonymous ftp session.
#    ------------------------------------------------------------

       $full_local_host_name = ( gethostbyname( $host ) )[0];
       $password = join("@",$user,$full_local_host_name);
       $password_alt = "${user}@";
   }



# Get the listing from the remote machine.

   $ftp = Net::FTP->new( "$remote_machine", Debug => 1,  Passive => $passive  ) 
      or return( ( "ERROR", "1", "Can not connect" ) );

   if ( $use_ftp) {
      $ftp->login();
   } else {
      if ( $password eq "" ) { 
         return( ( "ERROR", "2", "Password not available" ) );
      } else {
         $ftp->login("$remote_id","$password");
         if ( ! ${ftp_ok} ) {
            $ftp_ok = 1;
            $ftp->login("$remote_id","$password_alt") or $ftp_ok = 0;
         }
      }
   }

# Use a binary transfer of information, even for the ls, because the
# server may attempt to estimate the size of the file that would be
# the "ascii equivalent" to the binary file.  (ascii is the default
# mode.)  This takes a long time and give the wrong file size for
# binary files.

   $ftp->binary();

   $fname_ref = $ftp->ls( $remote_file );
   @file_name = @$fname_ref;

# Build up information to return.  Start with file name.

   @retlist = @file_name;

# If full path not wanted, only return file names.  (Note that @retlist
# is being modified in place here, and only contains the file name at 
# this point.)

   if ( ! $full_path ) {
      foreach ( @retlist ) {
         $_ = basename( $_, "" );
      }
   }

# If requested, add size of each file to the list.

   if ( $size ) {

      for ( $ii = 0 ; $ii <= $#retlist ; ++ $ii ) {
         $file_size = $ftp->size( $file_name[$ii] );
         $retlist[$ii] = join( " ", $retlist[$ii], $file_size );
      }

   }

# Log out from the server and return the information.

   $ftp->quit;

   return( @retlist );

}


sub rsh_rflist {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      $RUN_CONFIG_FILE = "DEFAULT";
   }

# Size option.

   if ( exists( $options{"size"} ) ) {
      $size = $options{"size"};
   } else {
      $size = 0;
   }

# Return full path or just file name(s).

   if ( exists( $options{"full_path"} ) ) {
      $full_path = $options{"full_path"};
   } else {
      $full_path = 0;
   }

# Return -l (long) output  (DEFAULT is short output)
   if ( exists( $options{"long"} ) ) {
        $long = $options{"long"};
   } else {
        $long = 0;
   }

# Return 1 if file exists or 0 if it does not ( -e )

   if ( exists( $options{"exist"} ) ) {
      $exist = $options{"exist"};
   } else {
      $exist = 0;
   }
# Debug status

   if ( exists( $options{"debug"} ) ) {
      $debug = $options{"debug"};
   } else {
      $debug = 0;
   } 
# Arguments.

   my $remote_id;
   my $remote_machine;
   my $remote_file;

   if ( $#_ == 0 ) {
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
   } else {
      $remote_id      = $_[0];
      $remote_machine = $_[1];
      $remote_file    = $_[2];
   }


# Check for local transfers
my ( $LOCAL, $local_host_name, $local_domain_name,
            $remote_host_name, $remote_domain_name ) = check_local($remote_machine);

  if ( "$LOCAL" ) {
      $rcmd = "";    # remote command

# Figure out remote, shell etc
# ----------------------------
  } else { 

   use Extract_config; # This module contains the extract_config() subroutine.

#  Get remote shell command to use on this machine (rsh or remsh).  Use 
#  what's set, otherwise try to get it from the run time configuration file.

      ( $REMSH = extract_config( "REMSH", $RUN_CONFIG_FILE, "$DEFAULT_RSH" ) ) ne ""
         or die "(rsh_rflist) ERROR - can not set REMSH configuration value\n";

    $rcmd = "$REMSH $remote_machine -l $remote_id -n "; # remote command
  }

# Create a command to do an 'ls' on the remote machine.
   if ($long || $size) {
        $cmd = "$rcmd /bin/ls -lL $remote_file";
   }else{
        $cmd = "$rcmd /bin/ls $remote_file";
   }

   if ( $debug ) {
      print "RSH_RFLIST: cmd = ", $cmd, "\n";
   }
   if ($size) {
        $retlist = `$cmd`;
        $rc = $?;
        if ( ! $rc ) {
             @remote_size = split(/\s+/, $retlist);
             @file_size   = ($remote_size[4]);
        }else{
             @file_size = (-1);
        }
   }else{
        @retlist = `$cmd`;
        $rc = $?;
   }
   if ($rc != 0) {
       $rc = 0;
   }else{
       $rc = 1;
   }
   if ($exist) {
       @retlist = ($rc);
   }
   if ($size) {
       @retlist = @file_size;
   }

return( @retlist );

}

#=====================  Remote gets  ============================

sub rget {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }
# Debug option.

   if ( exists( $options{'debug'} ) ) {
      $verbose = $options{'debug'};
   } else {
      $verbose = 0;
   }
 
# Arguments.

   my $local_file;
   my $remote_id;
   my $remote_machine;
   my $remote_file;

use File::Basename;

# Get arguments.

   print "Arguments: $#_\n";
   ARG_SWITCH: {
      if ( $#_ == 0 ) {
         print "0 Arguments: $_[0]\n";
         ( $remote_id, $remote_machine, $remote_file, $opt_filename ) = splitrfile( $_[0] );
         $local_file = basename( $remote_file, "" );
         last ARG_SWITCH;
      }
      if ( $#_ == 1 ) {
         print "1 Arguments: $_[0]\n";
         print "1 Arguments: $_[1]\n";
         ( $remote_id, $remote_machine, $remote_file, $opt_filename ) = splitrfile( $_[0] );

#        print "1 remote_id = $remote_id\n"; 
#        print "1 remote_machine = $remote_machine\n";
#        print "1 remote_file = $remote_file\n";
         $local_file = $_[1];
         last ARG_SWITCH;
      }
      if ( $#_ == 2 ) {
         ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
         $local_file = basename( $remote_file, "" );
         last ARG_SWITCH;
      }
      if ( $#_ == 3 ) {
         ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
         $local_file = $_[3];
         last ARG_SWITCH;
      }
      return 0;
   }
   
   if ( $opt_filename ) {
      print "opt_filename=$opt_filename\n";
   }


   if ( $debug ) {
      print "remote_id = ", $remote_id, "\n";
      print "remote_machine = ", $remote_machine, "\n";
      print "remote_file = ", $remote_file, "\n";
   }

# Check for local transfers
my ( $LOCAL, $local_host_name, $local_domain_name,
            $remote_host_name, $remote_domain_name ) = check_local($remote_machine, \%options);

# Check for FTP transfer
  if ( ! "$LOCAL" ) {
# This module contains an interface to the .netrc file.

        use Net::Netrc;
# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

        $use_ftp = check_ftp($remote_id, $remote_machine, \%options);

# If there is login information, use ftp to transfer the file.

        if ( $remote_machine eq "http" || $remote_machine eq "https" ) {
           # $opt_filename handles the case where the URL passed in has two ':' symbols, one after http
           # and the other between the machine name and the file path.
           if ( $opt_filename ) {
             $target = "${remote_machine}:${remote_file}/${opt_filename}";
           }
           else {
             $target = "${remote_machine}:${remote_file}";
           }
           print "$DEFAULT_WGET -O ${local_file} ${target}\n";
           `$DEFAULT_WGET -O ${local_file} ${target}`;
           if ( $? == 1 ) {
              $retcode = 0;
           }
           elsif ( $? == 0 ) {
              $retcode = 1;
           }
           else {
              $retcode = $?;
           }

           print "retcode=$? $retcode\n";
        }
        elsif ( $use_ftp ) {
           print "Using FTP\n";
           $retcode = ftp_get( $remote_id, $remote_machine, $remote_file,$local_file, \%options );

        } else {
           print "Using rcp\n";
           $retcode = rcp_get( $remote_id, $remote_machine, $remote_file, $local_file, \%options );
        }
  }else{
         print "Doing LOCAL COPY\n" if ( $verbose );
         $retcode = local_get( $remote_id, $remote_machine, $remote_file, $local_file, \%options );
  }
   return( $retcode );

}

sub local_get{

# This module contains the copy() subroutine.

   use File::Copy;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Get arguments
  
   my $local_file = $_[-1];
  if ( $#_ == 1 ) {
      $rem_loc = $_[0];
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $rem_loc );
   } else {
      ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
   }
   print "cmd = copy( $remote_file, $local_file )\n";
   $cp_ok = copy( $remote_file, $local_file );

   return( $cp_ok );

}
sub rcp_get {

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Run-time configuration file.

   if ( exists( $options{"run_config"} ) ) {
      my $RUN_CONFIG_FILE = $options{"run_config"};
   } else {
      my $RUN_CONFIG_FILE = "DEFAULT";
   }

# Get arguments.

   my $local_file = $_[-1];

   my $rem_loc;
   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $rcp_ok;

# This module contains the extract_config() subroutine.

   use Extract_config;


   if ( $#_ == 1 ) {
      $rem_loc = $_[0];
      ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $rem_loc );
   } else {
      ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
   }

# This is a remote transfer (between different machines), use the appropriate
# rcp command to do it.

# Get remote copy command to use on this machine.  Use what's set, otherwise
# try to get it from the run time configuration file.

      if ( ! defined( $RCP ) ) {
         ( $RCP = extract_config( "RCP", $RUN_CONFIG_FILE, "$DEFAULT_RCP" ) ) ne ""
            or die "ERROR - can not set RCP configuration value\n";
      }

# Create a command to do the remote copy.

      $cmd = "$RCP $remote_id\@$remote_machine:$remote_file $local_file";
      print "cmd =$cmd\n";
      system( $cmd );
      $retcode = $? >> 8;
      $rcp_ok = ! $retcode;


   return( $rcp_ok );

}

sub ftp_get {

   my $local_file;
   my $remote_id;
   my $remote_machine;
   my $remote_file;
   my $ftp_ok;

# This module contains the basename() subroutine.
   use File::Basename;

# This module contains the hostname() subroutine.
   use Sys::Hostname;

   $host = hostname();
   $full_local_host_name = ( gethostbyname( $host ) )[0];


# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[-1] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   } else {
      %options = ( noopt => "" );
   }

# Get options from hash
   if ( exists( $options{"passwd"} ) ) {
      $password = $options{"passwd"};
   }
   else {
      $password = "";
   }

# Get arguments.

   ARG_SWITCH: {
      if ( $#_ == 0 ) {
         ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
         $local_file = basename( $remote_file, "" );
         last ARG_SWITCH;
      }
      if ( $#_ == 1 ) {
         ( $remote_id, $remote_machine, $remote_file ) = splitrfile( $_[0] );
         $local_file = $_[1];
         last ARG_SWITCH;
      }
      if ( $#_ == 2 ) {
         ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
         $local_file = basename( $remote_file, "" );
         last ARG_SWITCH;
      }
      if ( $#_ == 3 ) {
         ( $remote_id, $remote_machine, $remote_file ) = @_[0..2];
         $local_file = $_[3];
         last ARG_SWITCH;
      }
      return 0;
   }

# See if the .netrc file contains the login information.  lookup will only return
# a defined value if it does.


# First see if there is a .netrc file containing ftp login information for
# the machine the transfer is going to.

   $use_netrc = check_ftp($remote_id, $remote_machine);

   if( $use_netrc == 0 && $remote_id eq "anonymous" && $password eq "" ) { 
            
#      --------------------------------------------------------------
#       If the remote_id and the password does not exist in the
#       .netrc file and the remote_id is "anonymous", then
#       extract the user name and the host name to create a password
#       for "anonymous ftp" session.
#      --------------------------------------------------------------
       use Sys::Hostname;

       my $user = eval { (getpwuid($>))[0] } || $ENV{HOME};
       my $host = hostname;

#    ------------------------------------------------------------
#      The following changes are made to get the full host name and
#      joined together to create a password for anonymous ftp session.
#    ------------------------------------------------------------

       $full_local_host_name = ( gethostbyname( $host ) )[0];
       $password = join("@",$user,$full_local_host_name);
       $password_alt = "${user}@";
               if ( $debug ) {
               print "NO netrc entry -- creating password for anonymous with id\n";
               print "\$host = $host\n";
               print "\$user = $user\n";
               print "\$password = $password\n";
          }
 

   }



   $ftp_ok = 1;

#    This modules contains the ftp interface.

      use Net::FTP;


      $ftp = Net::FTP->new( "$remote_machine", Debug => 1 )
       or return( ( "ERROR", "1", "Can not connect" ) );

       if ( $use_netrc ) {
          $ftp->login() or $ftp_ok = 0;
       } else {
         if ( $password eq "" ) {
             return( ( "ERROR", "2", "Password not available" ) );
         } else {
             $ftp->login("$remote_id","$password") or $ftp_ok = 0;
             if ( ! ${ftp_ok} ) {
               $ftp_ok = 1;
               $ftp->login("$remote_id","$password_alt") or $ftp_ok = 0;
             }
         }
       }
         $ftp->binary();
         $ftp->get( $remote_file, $local_file ) or $ftp_ok = 0;
         $ftp->quit;

         return( $ftp_ok );

}

1;
