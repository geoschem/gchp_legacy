#!/usr/bin/env perl
#--------------------------------------------------
#
# Purpose: create info files at given date/time
#          according to information in database. 
#
# Usage:
#
#  append_gsigcrc.pl [options] obsys.rc GSI_GridComp.rc
#
# !REVISION HISTORY:
#
#   28Sep2013 Todling  Initial code
#   06Nov2013 Todling  Skip all pre-qc files
#
#--------------------------------------------------

use Env;                 # make env vars readily available
use FindBin;             # so we can find where this script resides
use File::Basename;      # for basename(), dirname()
use Shell qw(rm);        # rm commands
use Getopt::Long;        # command line options

# look for perl packages in the following locations
#--------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );

GetOptions ( "debug",
             "h" );

# FVROOT is where the binaries have been installed
# ------------------------------------------------
$fvroot  = dirname($FindBin::Bin);
$fvroot  =~ s|/u/.realmounts/share|/share|;   # for portability across
                                              # NAS machines

# Initialize variables
# --------------------
  init();

# Execute this program
# --------------------
  run();

#....................................................................................
sub init {

   if ( $#ARGV  <  1 || $opt_h ) {
     print STDERR " Improper input parameters ; see usage:\n";
     usage();
   } else {              # required command line args
     $obsysrc = $ARGV[0];
     $gsigcrc = $ARGV[1];
   }

   die ">>>> ERROR <<< OBSCLASS env missing" unless $ENV{OBSCLASS};
   $obsclass = $ENV{OBSCLASS};

   if ( $ENV{PREPQC} ) {
       die ">>>> ERROR <<< PREPQC is on, therefore need EXPID env var to be set" unless $ENV{EXPID};
       $prepqcfile_handle = "gmao_prep_bufr => $EXPID.prepbufr.%y4%m2%d2.t%h2z.blk";
       $acftprofl_handle = "gmao_acftpfl_bufr => $EXPID.acft_profl.%y4%m2%d2.t%h2z.bfr"
   }

}

#....................................................................................
sub run {

# error check
$check = ( `grep observation_files $gsigcrc` );
if($check=~"observation_files") {
  die ">>> ERROR <<< cannot overwrite existing table";
}

# place name of table in file
open(MYJUNK,">>$gsigcrc") or
die ">>> ERROR <<< cannot write $gsigcrc";
print  MYJUNK <<"EOF";
observation_files::
EOF

@b = split(/,/, $obsclass);
foreach $oclass ( @b ) {
if ( "$oclass" =~ "pre-qc" ) {
  print "skipping pre-qc class $oclass \n";
} else {
 $row = ( `grep BEGIN $obsysrc | grep -v "#" | cut -c6- | grep $oclass` );
 $row =~ s/^\s+//; #remove leading spaces
 $row =~ s/\s+$//; #remove trailing spaces
print  MYJUNK <<"EOF";
$row
EOF
}
}
# care for when running prep-QC
if ($ENV{PREPQC} ) {
    $row = $prepqcfile_handle;
    $row2 = $acftprofl_handle;
print  MYJUNK <<"EOF";
$row
$row2
EOF
}
# place close mark of table in file
print  MYJUNK <<"EOF";
::
EOF
 close(MYJUNK);

}

#....................................................................................
sub usage {

   print <<"EOF";

NAME
     append_gsigcrc.pl - appends obs table to GSI_GridComp.rc

SYNOPSIS

     append_gsigcrc.pl [options] obsys.rc GSI_GridComp.rc

DESCRIPTION

     Based on the observation class and in the available classes
     in obsys.rc, this appends the GSI_GridComp.rc with the 
     pertinent obs-classes for the experiment in question

    Optinal Arguents:

    -h      help (echoes this usage)

    Required Env Vars:
      OBSCLASS  - set of obsclasses as defined in main ADAS, e.g.,
                  "ncep_prep_bufr,tcvitals"
    Optional Env Vars:
      PREPQC    - when set, ncep_prep_bufr class has special handle
                  (EXPID env var also required in this case)
      EXPID     - see PREPQC above

EOF

  exit(1);
}

