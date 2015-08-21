#!/usr/bin/env perl

#-----------------------------------------------------------------------------
# !DESCRIPTION:
#
#    Uses L. Takacs utilities to convert NCEP (GFS) analysis to GEOS eta grid
#
# !REVISION HISTORY:
#
#  16Feb2014  Todling   Initial code
#
#-----------------------------------------------------------------------------

use Env;                 # make env vars readily available
use FindBin;             # so we can find where this script resides
use File::Basename;      # for basename(), dirname()
use Shell qw(rm);        # rm commands
use Getopt::Long;        # command line options

# look for perl packages in the following locations
#--------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );

# FVROOT is where the binaries have been installed
# ------------------------------------------------
$fvroot  = dirname($FindBin::Bin);
$fvroot  =~ s|/u/.realmounts/share|/share|;   # for portability across
                                              # NAS machines
# Command line options
# --------------------
GetOptions( "h", "prs", "o=s", "rc=s" );

usage() if $opt_h;

$user = getlogin();

$rc=init();

if (! $rc ) {
   n2g();
}
if (! $rc ) {
   g2p();
}

exit ($rc);

#=======================================================================
## name - init
##=======================================================================
sub init {


  if ( $#ARGV < 1 ) {
       print STDERR "missing nymd, nhms and/or expid; see usage";
       usage();
  } else {              # required command lile args
       $nymd    = $ARGV[0];
       $nhms    = sprintf("%6.6d",$ARGV[1]);
       $yyyy    = substr($nymd,0,4);
       $mm      = substr($nymd,4,2);
       $dd      = substr($nymd,6,2);
       $hh      = substr($nhms,0,2);
       $nymdhhz = "${nymd}_${hh}z";
  }

  if ( $ENV{NCSUFFIX} ) {
     $ncsuffix = $ENV{NCSUFFIX};
  } else {
     $ncsuffix = "nc4";
  }

  if( $opt_rc ) { # rc file required by odsstats
      $rcfile = $opt_rc;
  } else {
     if ( $ENV{FVHOME} ) {
         if ( -e "$FVHOME/run/blendrs.rc" ) {
             $rcfile = "$FVHOME/run/blendrs.rc";
         } else {
           if ( -e "$FVHOME/fcst/blendrs.rc" ) {
             $rcfile = "$FVHOME/fcst/blendrs.rc";
           } else {
             print "Error: User must specify location of blendrs.rc file \n";
             $rc = 1;
             exit ($rc);
           }
         }
     } else {
       print "Error: User must specify location of blendrs.rc file \n";
       $rc = 1;
       exit ($rc);
     }
  }

  if( $opt_o ) { # rc file required by odsstats
    $outetafile =  $opt_o;
  } else {
    $outetafile =  "gfs.ana.eta.${nymd}_${hh}z.$ncsuffix";
  }
  $outprstag = "gfs.inst3_3d_ana_Np"; # this is only the type (tag) for the filename

  # get name of file from NCEP
  $ncep_anal = `$fvroot/bin/echorc.x -template gfs $nymd $nhms -rc $rcfile ncep_anal_file`;
  chomp($ncep_anal);

  # determine resolution to go to from NCEP to GEOS
  @geos_resol = (`$fvroot/bin/echorc.x -rc $rcfile geos_resolution`);
  chomp($geos_resol); split($geos_resol);
  $im = $geos_resol[0]; chomp($im);
  $jm = $geos_resol[1]; chomp($jm);
 print "$im $jm \n";

  $rc = 0;

} # end init

#=======================================================================
sub n2g {

# Create NCEP Gridded data from GDAS Spectral Analysis at specified resolution
# ----------------------------------------------------------------------------
   Assignfn("$ncep_anal","ncep_anal");
   $cmd = "$fvroot/bin/ss2gg.x ncep_anal gg.data gg.ctl 0 $im $jm";
   print "$cmd \n";
   $rc = system($cmd);

   if ($rc) {return};

# Convert NCEP analysis to GMAO eta coordinate system
# ----------------------------------------------------
   $cmd = "$fvroot/bin/gg2eta.x -ncep gg.data -ctl gg.ctl";
   print "$cmd \n";
   $rc = system($cmd);
   if ( -e   "gg2eta.${nymd}_${hh}z.$ncsuffix" ) {
      rename("gg2eta.${nymd}_${hh}z.$ncsuffix","$outetafile");
   }

   $rc = 0;
   rm("gg.data");
   rm("gg.ctl");
   rm("ncep_anal");

} # end n2g
#=======================================================================
sub g2p {

  return 0 unless $opt_prs;

  $cmd = "$fvroot/bin/eta2prs.x -eta  $outetafile -levs 1000 925 850 700 500 400 300 250 200 150 100 50 20 10 -ana -noquad -tag  $outprstag";
   print "$cmd \n";
   $rc = system($cmd);

  $rc = 0;

} # end g2p
#=======================================================================
sub Assignfn {

# Assignfn - assigns fn to given file name fname.
# fname = old file
# fn = new file (links to old)
  my ( $fname, $fn ) = @_;
  unlink($fn) if ( -e $fn ) ;
  symlink("$fname","$fn");

}
#
#=======================================================================
# name - usage
#=======================================================================
sub usage {

   print <<"EOF";

NAME
     ncep2geos - Convert NCEP analysis files to GEOS eta grid
          
SYNOPSIS

     ncep2geos [...options...]  nymd nhms
          
DESCRIPTION

     The following parameter are required 

     nymd     Year-month-day, e.g., 19990901  for 01 Sept 1999 
     nhms     Hour-minutes-seconds, e.g., 120000

OPTIONS
 
 -h            prints this usage notice
 
 -o efname     sepecify name of output eta file 
                 (default: gfs.ana.eta.%y4%m2%d2_%h2z.nc4)

 -rc RCFILE    full path of RC file for odsstats
                 (default: FVHOME/run/blendrs.rc or FVHOME/fcst/blendrs.rc)

 -prs          convert eta file to and write fname
                 (default: do not convert to pressure)

TO DO 

 1. look for ncep-eta file in archive if not found when create the file anew

ENVIRONMENT

   FVHOME      needed to local rc file in case -rc not specified
   NCSUFFIX    specify SDF file type (default: nc4)

EXAMPLE RC FILE

  The following is an example of what this program expects to be in the rc file

ncep_anal_file:/discover/nobackup/dao_ops/intermediate/flk/stage/gfs/gfs.t%h2z.sanl.%y4%m2%d2
ncepeta_ana_file: gfs.ana.eta.%y4%m2%d2_%h2z.nc4
geos_resolution: 576 361

AUTHOR
      R. Todling (ricardo.todling\@nasa.gov), NASA/GSFC/GMAO
      Based on L. Takacs programs

EOF

  exit(1)

}
