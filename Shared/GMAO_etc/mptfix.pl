#!/usr/bin/env perl
# 
# mptfix.pl
#
#  11May2016 Thompson  Adaptations for using MPT
#  24Jun2016 Thompson  Changes that use the machinefile to calculate number of
#                      tasks per node. No longer requires that as an input 
#
#-----------------------------------------------------------------------------------------------------

use strict;
use warnings;

use Env;                       # make env vars readily available
use File::Basename;            # for basename(), dirname()
use File::Copy "cp";           # for cp()
use Getopt::Long;              # load module with GetOptions function
use FindBin;                   # so we can find where this script resides
use List::MoreUtils qw(uniq);  # to uniq the machinefile
use Data::Dumper;
$Data::Dumper::Sortkeys=1;

my $scriptname = basename($0);

my ($queue, $help,$rc,$debug);

$queue = "";
$rc = 1;

# Command line options

  GetOptions ( "q=s"    => \$queue,
               "debug"  => \$debug ,
               "h|help" => \$help );

  usage() if $help;

# Parse command line, etc

  main();

# All done

  if ($rc==0) {
     print "$0: sucessfully completed.\n\n";
     exit(0);
  } else {
     print "$0: failed to modify PBS job script\n\n";
     exit(1);
  }


#......................................................................

sub main {

   my ($jobfile, $machfile, $numtasks);
   my ($ntasks, $ntasks_per_node, $num_nodes);
   my (@mach_nodes, $num_cores, @unique_mach_nodes);
   my ($modulo, $nodelist);
   my (@joblines, $fragment, $command);
   my (%counts, $test_for_equal_nodes);

   if ( $#ARGV  <  2 ) {
     print STDERR " Missing arguments; see usage:\n";
     usage();
   } else {             # required command line args
     $jobfile           = $ARGV[0];
     $machfile          = $ARGV[1];
     $numtasks          = $ARGV[2];
   }

# FVROOT is where the binaries have been installed
# ------------------------------------------------

   
   if ( $queue eq "datamove" ) {
        $ntasks          = 1;
   } else {
        $ntasks          = $numtasks;
   }


# Parse the machine file and make a unique list
# ---------------------------------------------
   
   # Open the machinefile and put in an array
   unless (open MACH, "< ", $machfile ) {
      die "Cannot open $machfile";
   }

   chomp(@mach_nodes = <MACH>);
   close MACH;

   $counts{$_}++ for @mach_nodes;

   $num_cores = scalar @mach_nodes;

   if ( $num_cores != $numtasks ) {
      print "$0: ERROR: Number of cores found in machinefile ($num_cores) does not equal number of tasks passed in ($numtasks) \n\n";
      exit(1);
   }

   # Now just the unique members of that array
   @unique_mach_nodes = uniq @mach_nodes;

   $num_nodes = scalar @unique_mach_nodes;

   $modulo = $num_cores % $num_nodes;

   if ( $modulo == 0 ) {
      $ntasks_per_node = $num_cores/$num_nodes;
   } else {
      print "$0: ERROR: Number of cores found in machinefile ($num_cores) does not equally divide the number of nodes ($num_nodes) \n\n";
      exit(1);
   }

   $test_for_equal_nodes = scalar uniq values %counts;

   if ( $test_for_equal_nodes != 1) {
      print "$0: ERROR: The machinefile does note contain equal amounts of cores per node\n";
      print Dumper(\%counts);
      exit(1);
   }

   # Make a comma-delimited list of that unique array
   $nodelist = join( ',', @unique_mach_nodes);

   if ($debug) {
      print "machfile: $machfile\n";
      print "mach_nodes: @mach_nodes\n";
      print "unique_mach_nodes: @unique_mach_nodes\n";
      print "nodelist: $nodelist\n\n";
      print "ntasks: $ntasks\n";
      print "num_cores: $num_cores\n";
      print "num_nodes: $num_nodes\n";
      print "ntasks_per_node: $ntasks_per_node\n";
   }

# Now open the job file and substitute
# ------------------------------------

   # Open the machinefile and put in an array
   unless (open JOBINPUT, "< ", $jobfile ) {
      die "Cannot open $jobfile";
   }

   cp($jobfile, $jobfile . "_orig");
   @joblines = <JOBINPUT>;

   close JOBINPUT;

   # Open the machinefile and put in an array
   unless (open JOBOUTPUT, "> ", $jobfile ) {
      die "Cannot open $jobfile";
   }


   foreach $command (@joblines) {

      if ( $command =~ /mpirun/ ) {
         $fragment =  substr $command, index($command, 'mpirun'), 6, "mpiexec -machinefile";
      }

      if ( $command =~ /mpiexec_mpt/ ) {

         if ($debug) {
            print "command before: $command\n";
         }

         # This first substr finds the place where ntasks is and
         # substitutes it for ntasks_per_node calculated above.  The
         # mpirun use of MPT expects -np to be the processes per node
         # in the nodelist not the total number like a "usual" mpi run
         # command like mpiexec_mpt
          
         $fragment =  substr $command, index($command, "$ntasks"), length("$ntasks"), "$ntasks_per_node";


         # Now we replace mpiexec_mpt with mpirun and our comma-delimited nodelist
          
         $fragment =  substr $command, index($command, 'mpiexec_mpt'), 11, "mpirun $nodelist";

         if ($debug) {
            print "command after: $command\n";
         }
      }
      print JOBOUTPUT $command;
   }

   close JOBOUTPUT;

   $rc    = 0;
}

#......................................................................

sub usage {

   print <<"EOF";

NAME
     mptfix - Modify PBS job script for MPT
          
SYNOPSIS

     mptfix [...options...] jobfile
                            machfile
                            numtasks
          
DESCRIPTION

     The following parameters are required 

     jobfile            name of job script to be modified
     machfile           name of machinefile that contains nodes
     numtasks           number of tasks the code will run on

OPTIONS

     -debug       add debug prints
     -q           specify pbs queue (e.g., datamove when archiving)
     -h[elp]      prints this usage notice

AUTHOR

     Matthew Thompson (Matthew.Thompson\@nasa.gov), SSAI NASA/GSFC/GMAO
     Last modified: 23May2016      by: M. Thompson


EOF

  exit(1)

}
