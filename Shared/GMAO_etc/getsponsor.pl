#!/usr/bin/env perl
#=======================================================================
#
# Name: getsponsor.pl
# Purpose: wrapper for get_spcode() in getsponsor.pm package
#
# return value
# => $groupID
#
# Notes:
# 1. See usage() for usage information.
#=======================================================================
use strict;
use warnings;
use FindBin qw($Bin);
use lib ("$Bin");
use getsponsor "get_spcode";

# global variables
#-----------------
my ($groupID, %flags, $outfile);

# main program
#-------------
{
    init();
    $groupID = get_spcode(%flags);

    if ($outfile) {
        open OUT, "> $outfile" or die ">> Error << opening file: $outfile: $!";
        print OUT "$groupID\n";
        close OUT;
    }
    else {
        print "$groupID\n";
    }
}

#=======================================================================
# name - init
# purpose - initialize variables, get runtime parameters
#=======================================================================
sub init {
    use Getopt::Long;
    my ($usefirst, $capture, $dflt, $grpID, $mapfile, $menu, $quiet, $help);

    # initializations
    #----------------
    $groupID = "";

    # runtime parameters
    #-------------------
    GetOptions("1"        => \$usefirst,
               "f=s"      => \$mapfile,
               "capture"  => \$capture,
               "dflt"     => \$dflt,
               "grpID=s"  => \$grpID,
               "menu"     => \$menu,
               "o=s"      => \$outfile,
               "q"        => \$quiet,
               "h"        => \$help);
    usage() if $help;

    # set flags hash for call to get_spcode()
    #----------------------------------------
    $flags{"grpID"}    = $grpID if $grpID;
    $flags{"mapfile"}  = $mapfile if $mapfile;
    $flags{"menu"}     = 1 if $menu;
    $flags{"dflt"}     = 1 if $dflt;
    $flags{"quiet"}    = 1 if $quiet;
    $flags{"stderr"}   = 1 if $capture;
    $flags{"usefirst"} = 1 if $usefirst;
}

#=======================================================================
# name - usage
# purpose - print usage information to standard output
#=======================================================================
sub usage {
    use File::Basename;
    my $name;

    $name = basename $0;
    print STDERR <<"EOF";

usage: $name [options]
   options:
     -1            (This is #1) Use first available groupID as the default.
     -f mapfile    File containing mapping between users and tasks
     -capture      Capture groupID from STDOUT (menu gets written to stderr)
     -dflt         Return default group ID without displaying the menu
     -grpID gid    Return gid if it is valid group ID; otherwise return default;
                   In either case, the menu is not displayed
     -menu         Display menu only
     -o outfile    Write groupID to outfile
     -q            Quiet mode; do not print some of the warning messages
     -h            Print this usage information

Note:
1. If either \$GID or \$gid is set as an environment variable, then the script
   will use this value as the default. \$GID takes precedence over \$gid if they
   are set to different values.
2. With the -grpID flag, if gid is not a valid group ID, then the script will
   look for a close match among the valid IDs before choosing the default.
3. If the environment variable \$GID or \$gid is not a valid group ID, then the
   script will look for a close match before going to the default.

EOF
exit;
}
