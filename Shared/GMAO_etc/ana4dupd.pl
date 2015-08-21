#!/usr/bin/env perl

#--------------------------------------------------
#
# Purpose: to rename DAS files during the run
#
# Usage:
#
#  ana4dupd.pl [options] EXPID NYMDB NHMSB
#                              NYMDE NHMSE DTMIN
#
# !REVISION HISTORY:
#
#   05Feb2014 Todling  Initial code
#
# !TO DO:
#
#   1) make sure works in lat-lon case
#
#--------------------------------------------------
use Env;                 # make env vars readily available
use FindBin;             # so we can find where this script resides
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Shell qw(cat rm);    # cat and rm commands
use Time::Local;         # time functions

# Look for perl packages in the following locations
# -------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );
use Manipulate_time "tick";

# Parse command line
# ------------------
  GetOptions ( "dir=s",
               "iter=s",
               "rcdir=s",
               "iau",
               "debug",
               "h" );

  usage() if $opt_h;

# FVROOT is where the binaries have been installed
# ------------------------------------------------
  $fvroot  = dirname($FindBin::Bin);
  $fvroot  =~ s|/u/.realmounts/share|/share|;   # for portability across
                                              # NAS machines
 
# Initialize variables
# --------------------
  init();

# Update backgrounds
# ------------------
  upd();

# Create IAU tendencies
# ---------------------
  iau();

# If made it here, exit with no error
# -----------------------------------
  exit(0);

#......................................................................
sub upd {

# Special-handle first file
# -------------------------
  $nymd = $nymdb;  $nhms = $nhmsb;
  $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`;  chomp($bfile);
      $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
      $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`;  chomp($afile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
      $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
      $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }

# Create ana for this time
  $cmd = $fvroot . "/bin/dynp.x $rcopt -s $bfile -p $ifile -a 1 -ainc -g5 -pncf -pureadd -realp -os $afile";
  print "Begin $cmd \n";
  if ( ! $opt_debug ) {
      $errcode = system($cmd);
      die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
      if ( ! -e "$afile" ) {
         die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
      }
  }

# Loop over time for all other files
# ----------------------------------
  while ( $nymd != $nymde || $nhms != $nhmse ) {
     ($nymd,$nhms) = tick($nymd,$nhms,$dtsec);
     $hh     = substr($nhms,0,2);
     if ( $rcdir ) {
         $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`;  chomp($bfile);
         $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
         $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`;  chomp($afile);
     } else {
         $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
         $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
         $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
     }
     $cmd = $fvroot . "/bin/dynp.x $rcopt -s $bfile -p $ifile -a 1 -ainc -g5 -pncf -pureadd -realp -os $afile";
     if ( $nymd == $nymde && $nhms == $nhmse ) { # special handle file at final time
         print "End $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "$afile" ) {
              die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
           }
         }
     } else {
         print "Others $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "$afile" ) {
              die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
           }
         }
     }
  }

} # <upd>

#......................................................................
sub iau {

  return unless ($getiau);

# Special-handle first file
# -------------------------
  $nymd = $nymdb;  $nhms = $nhmsb;
  $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`; chomp($bfile);
      $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`; chomp($afile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
      $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }

 ed_mkiau_rc ($bfile,$afile,$nymd,$nhms);

# Create iau tendency for this time
  $cmd = "$MPIRUN_IAU";
  print "Begin $cmd \n";
  if ( ! $opt_debug ) {
      $errcode = system($cmd);
      die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
      if ( ! -e "agcm_import_rst" ) {
         die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
      }
      $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
      rename("agcm_import_rst","$iaufname");
  }

# Loop over time for all other files
# ----------------------------------
  while ( $nymd != $nymde || $nhms != $nhmse ) {
     ($nymd,$nhms) = tick($nymd,$nhms,$dtsec);
     $hh     = substr($nhms,0,2);
     if ( $rcdir ) {
         $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`; chomp($bfile);
         $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`; chomp($afile);
     } else {
         $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
         $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
     }
     ed_mkiau_rc ($bfile,$afile,$nymd,$nhms);
     $cmd = "$MPIRUN_IAU";
     if ( $nymd == $nymde && $nhms == $nhmse ) { # special handle file at final time
         print "End $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "agcm_import_rst" ) {
              die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           }
           $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
           rename("agcm_import_rst","$iaufname");
         }
     } else {
         print "Others $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "agcm_import_rst" ) {
              die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           }
           $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
           rename("agcm_import_rst","$iaufname");
         }
     }
  }

} # <iau>

#......................................................................
sub ed_mkiau_rc {

  return unless ($getiau);

  my($bkgfile,$anafile,$anymd,$anhms) = @_;

  if( -e "$rcdir/mkiau.rc" ) { rm("$rcdir/mkiau.rc") };

  cp("$rcdir/mkiau.rc.tmpl","$rcdir/mkiau.rc.tmp");

  $ft   = "$rcdir/mkiau.rc";
  $frun = "$rcdir/mkiau.rc.tmp";

  open(LUN,"$frun") || die "Fail to open $frun: $!\n";
  open(LUN2,">$ft") || die "Fail to open   $ft: $!\n";

  while( defined($rcd = <LUN>) ) {
     chomp($rcd);
     if($rcd =~ /\>>>EXPID<<</)    {$rcd=~ s/\>>>EXPID<<</$expid/g; }
     if($rcd =~ /\>>>BKGFNAME<<</) {$rcd=~ s/\>>>BKGFNAME<<</$bkgfile/g; }
     if($rcd =~ /\>>>ANAFNAME<<</) {$rcd=~ s/\>>>ANAFNAME<<</$anafile/g; }
     if($rcd =~ /\>>>NCSUFFIX<<</) {$rcd=~ s/\>>>NCSUFFIX<<</$ncsuffix/g; }
     if($rcd =~ /\>>>ANADATE<<</)  {$rcd=~ s/\>>>ANADATE<<</$anymd/g; }
     if($rcd =~ /\>>>ANATIME<<</)  {$rcd=~ s/\>>>ANATIME<<</$anhms/g; }
     print(LUN2 "$rcd\n");
  }

  close(LUN);
  close(LUN2);
  rm("$rcdir/mkiau.rc.tmp");

}

#......................................................................
sub init {

   if ( $#ARGV  <  5 ) {
     print STDERR " Improper input parameters ; see usage:\n";
     usage();
   } else {              # required command line args
     $expid = $ARGV[0];
     $nymdb = $ARGV[1]; 
     $nhmsb = sprintf("%6.6d",$ARGV[2]); 
     $nymde = $ARGV[3]; 
     $nhmse = sprintf("%6.6d",$ARGV[4]); 
     $dtmin = $ARGV[5]; 
   }
   $dtsec = $dtmin * 60;
 
#  When passed, get positioned in this directory ...
   if ( $opt_dir ) {
      #chdir("$opt_dir");
   }

   $ncsuffix = "nc4";

   $iter = 0;
   if ( $opt_iter ) {
      $iter = $opt_iter;
   }
   die ">>> ERROR <<< cannot yet handle iter>0 " if ( $iter>0 );

   if ( $opt_rcdir ) {
       $rcdir = $opt_rcdir;
   }

   if ( $opt_iau ) {
      if ( $ENV{MPIRUN_IAU} ) {
         $getiau = 1;
      } else {
         die ">>> ERROR <<< need env(MPIRUN_IAU) to create IAU tendency";
      }
   }

   $btype = "bkg.eta";
   $itype = "xinc.eta";
   $atype = "ana.eta";
}
#......................................................................
sub usage {
   print <<"EOF";

NAME
     ana4dupd.pl - update background from 4d analysis
          
SYNOPSIS

     ana4dupd.pl  expid nymdb nhmsb nymde nhmse dtmin
     e.g.,
     ana4dupd.pl  d541  20090731 210000 20090731 210000 0
     ana4dupd.pl  d541  20090731 210000 20090801 030000 180
          
DESCRIPTION

     This script updates backgrounds given analysis increments 
     generated within the (typically 4d) variational loop.

     The following parameters are required 

     expid  experiment id
     nymdb  Starting date
     nhmsb  Starting time
     nymde  Ending date
     nhmse  Ending time
     dtmin  Time increment in minutes

    Require RC files:

     fvpsas.rc     - file holding filename templates for DAS-related files
     mkiau.rc.tmpl - file with intructions for how to run IAU (only when applicable)

    Optinal Arguents:

    -h             help (echoes this usage)   
    -dir  DIRNAME  work directory (bkg and xinc files should be here)
    -iter ITER     iteration number of outer loop to accommodate incremental approach
    -iau           creates IAU tendency from incremental difference
    -rcdir DIR     directory where to find relavant RC files
    -debug         simply check basic flow of this procedure not doing anything

    Optinal Environment Variables:
 
    MPIRUN_IAU     required only when -iau option specified,
                     e.g., setenv MPIRUN_IAU "mpirun -np 24 FULLPATH/mkiau.x"

EOF

  exit(1);
}
