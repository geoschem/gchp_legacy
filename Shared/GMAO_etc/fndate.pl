#!@DASPERL -w
###########################################################################
#
#  Name: fndate.pl
#
#  Purpose - This script extracts the date and time values from a filename.
#
#  Notes:
#  The filename must contain a date/time value in one of the following
#  formats:
#  
#    - yyyymmdd_hh[Zz]
#    - yyyymmdd_hhmm[Zz]
#    - yyyymmdd_hhmmss[Zz]
#
#  REVISION HISTORY
#  03May2006   Stassi   Initial version of code
#
###########################################################################
use strict;
use File::Basename;

my ($fname,$ymd,$hms);
my $scriptname = basename($0);

unless (@ARGV == 1) { &usage() };
$fname = $ARGV[0];

if ( $fname =~ /.*(\d{8})_(\d{2})[Zz].*/ ) {
    $ymd = $1;
    $hms = $2."0000";
    print "$ymd $hms\n";

} elsif ( $fname =~ /.*(\d{8})_(\d{4})[Zz].*/ ) {
    $ymd = $1;
    $hms = $2."00";
    print "$ymd $hms\n";

} elsif ( $fname =~ /.*(\d{8})_(\d{6})[Zz].*/ ) {
    $ymd = $1;
    $hms = $2;
    print "$ymd $hms\n";

} else {
    die "filename does not contain properly formatted date: \"$fname\"";
}

#=======================================================================
# name - usage
# purpose - print usage information to standard output
#=======================================================================
sub usage {
    die << "EOF";

===============================================
usage: $scriptname <filename>

prints to standard output:

  yyyymmdd hh0000 ... if filename = \*yyyymmdd_hh[Zz]\*

  yyyymmdd hhmm00 ... if filename = \*yyyymmdd_hhmm[Zz]\*

  yyyymmdd hhmmss ... if filename = \*yyyymmdd_hhmmss[Zz]\*

===============================================


EOF

}
