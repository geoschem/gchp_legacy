#!/usr/bin/env perl
#--------------------------------------------------
#
# Purpose: verify that all entries in obs-sys-rc 
#          exist; flag data not found and, optionally,
#          touch to create a zero-length file.
#
# Usage:
#
#  verify_obsys.pl [options] obsys.rc
#
# !REVISION HISTORY:
#
#   18Nov2013 Todling  Initial code
#
#--------------------------------------------------
use Env;                 # make env vars readily available
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Shell qw(cat cut wc); # shell commands
use Time::Local;         # time functions
use FindBin;             # so we can find where this script resides
use lib ( "$FindBin::Bin" );
use POSIX qw(strftime);
use Manipulate_time;

#use strict;
#use warnings;

# Main ...
#{

my($oclass,@obsclass,$obsysrc);
my(%flags);

# FVROOT is where the binaries have been installed
# ------------------------------------------------
#$fvroot  = dirname($FindBin::Bin);
#$fvroot  =~ s|/u/.realmounts/share|/share|;   # for portability across
                                              # NAS machines

# Initialize variables
# --------------------
  init();

  foreach $oclass ( @obsclass ) {
     chomp($oclass);
     process_class ($oclass);
  }

#}
#.........................................................
sub init {

   GetOptions ( "debug",
                "touch",
                "h" );

   if ( $#ARGV  <  0 || $opt_h ) {
     print STDERR " Improper input parameters ; see usage:\n";
     usage();
   } else {              # required command line args
     $obsysrc = $ARGV[0];
   }

   if ($ENV{OBSCLASS} ) {
      $obsclass = $ENV{OBSCLASS};
      @obsclass = split(/,/, $obsclass);
   } else {
      @obsclass = (`grep BEGIN $obsysrc | grep -v "#" | cut -d" " -f2`);
   }

   if($opt_touch) {
      $do_touch = 1;
   } else {
      $do_touch = 0; 
   }

}
#.........................................................
sub process_class {

my($class) = @_;

my($nc,$nlines,$first);

$nlines = `wc -l < $obsysrc`;
open my $info, $obsysrc or die "Could not open $obsysrc: $!";

$nc = 0;
while( my $line = <$info>)  {   
    $first = substr($line, 0, 1); 
    if ( $first eq "#" ) { next };
    if ($nc > 0 || $line =~ $class) {
        # at this point we are dealing with a single table
        $nc++;
        if ($line !~ "BEGIN" & $line !~ "END" ) {
           handle_entry($line);
        }
        if ($line =~ "END") {$nc = 0}; 
    }
    last if $. == $nlines;
}
close $info;

}
#.......................................
sub handle_entry{
# this handles a single line within a class of a typical obsys.rc file
    my($entry) = @_;
    my(@tokens,$beg_date,$end_date,$extra,@aux,$freq);
    my($nymdb,$hhb,$nymde,$hhe,$freq_sc,$filetmpl);
    # get begin and end times of data availability
    @tokens = split(/-/, $entry); # separate first date string
    $beg_date = $tokens[0];
    $extra    = $tokens[1];
    @tokens = split(/\s+/, $extra); # separate second date string
    $end_date = $tokens[0];
    $extra    = $tokens[1];
    $nymdb = `echo $beg_date | cut -c1-8`; chomp($nymdb);
    $hhb   = `echo $beg_date | cut -c10-11`; chomp($hhb);
    $nymde = `echo $end_date | cut -c1-8`; chomp($nymde);
    $hhe   = `echo $end_date | cut -c10-11`; chomp($hhe);
    # get frequency of data in archive
    @tokens = split(/\s+/, $extra); # separate second date string
    $freq   = $tokens[0]; chomp($freq);
    $extra  = $tokens[1];
    if ($freq eq "120000") {
        $freq_sc = 12 * 3600;
    } else {
        $freq_sc =  6 * 3600;
    }
    # get file name (remove dirac or machine location from file path)
    @tokens = split(/\s+/, $entry);
    if ($tokens[3] =~ ":" ) {
        @aux = split(/:/, $tokens[3]);
        $filetmpl = $aux[1];
    } else {
        $filetmpl = $tokens[3];
    }
    # get today's date and make sure end-date is no larger than that
    my $today_date = strftime "%Y%m%d", localtime;
    if($nymde > $today_date) {$nymde = $today_date; $hhe = sprintf "%02d", 0;};

    # verify data in archive ...
    do_verify($nymdb,$hhb,$nymde,$hhe,$freq_sc,$filetmpl);
}
#.......................................
sub do_verify{
    my($nymdb,$hhb,$nymde,$hhe,$freq_sc,$filetmpl) = @_;
    my($filename,$nymd,$nhms);

    $nymd = $nymdb;
    $nhms = "${hhb}0000";

    $done=0;
    while ( ! $done ) {
       $filename = token_resolve($filetmpl,$nymd,$nhms);
       chomp($filename);
       if (! -e "$filename" ) {
          if ( $do_touch ) {
            print "touch $filename \n";
          } else {
            print "Missing File: $filename \n";
          }
       }
       ( $nymd, $nhms ) = tick ($nymd, $nhms, $freq_sc);
       if( $nymd > $nymde ) {$done = 1};
    }
}

#.......................................
sub usage {

   print <<"EOF";

NAME
     verify_obsys.pl - check files in obsys-rc and take action if requested

SYNOPSIS

     verify_obsys.pl [options] obsys.rc

DESCRIPTION

     Given observation classes defined in OBSCLASS env variable
     verify that data in that class do exist in the archive. Prints
     out data files when not found in archive and optionally it produces
     a list of touch commands for all missing files - user paste list
     a touch files if desired.

    Optinal Arguents:

    -h      help (echoes this usage)
    -touch  produces a list of touch commands for all missing files

    Optional Env Vars:
      OBSCLASS  - set of obsclasses as defined in main ADAS, e.g.,
                  "ncep_prep_bufr,tcvitals" (default: all classes in rc file)

EOF

  exit(1);
}

