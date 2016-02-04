#!/usr/bin/env perl
use strict;
use warnings;
#=======================================================================
# name - obsys_check.pl
# purpose - check an obsys rcfile to determine whether an observation
#           data set is available for a specified date/time range.
#
# Notes:
# 1. See usage() information for calling parameters and options.
# 2. The data must be available at the beginning date/time and through the
#    number of hours specified in order for a "true" value to be returned.
# 3. The script does not check whether the observation data files actually
#    exist but only whether the obsys.rc file says that they are available.
#
# return value (printed to STDOUT and returned as exit status)
# == 0 if data are not available for entire date/time range
# == 1 if data are available
#=======================================================================

# global variables
#-----------------
my ($rcfile, $class, $ymd1, $hr1, $ymd2, $hr2, $numhrs);

# main program
#-------------
{
    my ($labelfound, $found);
    init();
    open RC, "< $rcfile" or die "Error opening rcfile: $rcfile\n";
    while (<RC>) { last if $labelfound = /^BEGIN $class/ }
    warn "WARNING. obsclass = $class not found in $rcfile\n" unless $labelfound;

    $found = 0;
    while (<RC>) {
        next if /^\s*\#/;
        last if /^\s*END/ or /^\s*BEGIN/;
        last if ($found = search_date($_));
    }
    print "$found\n";
    exit $found;
}

#=======================================================================
# name - search_date
# purpose - parce entry from obsys.rc to see if datetime is included
#=======================================================================
sub search_date {
    my ($entry, $found, $begdatetime, $enddatetime);
    $entry = shift @_;
    $found = 0;
    if ($entry =~ m/\s+(\d{8})_(\d{2})z-(\d{8})_(\d{2})z/) {
        $begdatetime = "$1$2";
        $enddatetime = "$3$4";
        $found = 1 if "$ymd1$hr1" >= $begdatetime and "$ymd1$hr1" <= $enddatetime
            and       "$ymd2$hr2" >= $begdatetime and "$ymd2$hr2" <= $enddatetime;
    }
    return $found;
}

#=======================================================================
# name - init
# purpose - get runtime parameters and flagss
#=======================================================================
sub init {
    use FindBin qw($Bin);
    use lib ("$Bin");
    use Getopt::Long;
    use Manipulate_time ("num_days_in_month", "tick");
    my ($help, $year, $month, $day, $lastday, $hhmmss2);

    # get runtime options
    #--------------------
    GetOptions( "rc=s" => \$rcfile,
                "h"    => \$help );
    usage() if $help;
    $rcfile = "obsys.rc" unless $rcfile;
    die "Error. Cannot find file, $rcfile;" unless -e $rcfile;
    
    # get runtime parameters
    #-----------------------
    usage() unless scalar(@ARGV) == 4;
    ($class, $ymd1, $hr1, $numhrs) = @ARGV;

    # check inputs
    #-------------
    die "Error. Undecipherable date, $ymd1;" unless $ymd1 =~ m/\b\d{8}\b/;
    die "Error. Undecipherable hour, $hr1;"  unless $hr1  =~ m/\b\d{2}\b/
        or                                          $hr1  =~ m/\b\d{6}\b/;
    die "Error. Invalid numhrs, $numhrs;"    unless $numhrs  =~ m/\b\d+\b/;

    # check date/time values
    #-----------------------
    ($year, $month, $day) = ( $ymd1 =~ m/(\d{4})(\d{2})(\d{2})/ );
    $lastday = num_days_in_month($year, $month);
    $hr1 = substr($hr1, 0, 2) if $hr1 =~ m/\b\d{6}\b/;

    die "Error. Invalid month, $month;"       if $month < 1 or $month > 12;
    die "Error. Invalid day-of-month, $ymd1;" if $day   < 1 or $day   > $lastday;
    die "Error. Invalid hour, $hr1;"          if $hr1   < 0 or $hr1    > 24;

    # tick to end date/time
    #----------------------
    ($ymd2, $hhmmss2) = tick $ymd1, "${hr1}0000", 0, "${numhrs}0000";
    $hr2 = substr($hhmmss2, 0, 2);
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my $script = basename($0);
    print << "EOF";

usage: $script obsclass yyyymmdd hh numhrs [options]

where
  obsclass   label identifying obsclass to check
  yyyymmdd   beginning year-month-day to check, e.g. 19980101
  hh         beginning hour to check, e.g. 12
  numhrs     number of hours to check

options
  -rc        obsys rc file name (default = obsys.rc)
  -h         print usage information

description:     
EOF
}
