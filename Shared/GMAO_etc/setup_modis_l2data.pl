#!/usr/bin/env perl
#=======================================================================
# name - setup_modis_l2_data
# purpose - organize modis mod04 and myd04 data into subdirectories
#           expected by the reading program
#
# runtime parameter
# => $workdir: directory location of modis data
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($workdir, %flags);

# main program
#-------------
{
    use File::Basename ("basename");
    use File::Copy ("move");
    use File::Path ("mkpath");

    my ($file, $base, $label, $Adate);
    my ($type, $year, $jdoy, $subdir);

    init();
    foreach $file (<$workdir/M?D04_L2.*.NRT.hdf>) {
        $base = basename $file;
        ($label, $Adate) = split /[\.]/, $base;

        $type = substr($label, 0, 5);
        $year = substr($Adate, 1, 4);
        $jdoy = substr($Adate, 5, 3);

        $subdir = "$workdir/$type/$year/$jdoy";
        unless (-d $subdir) {
            mkpath($subdir, \%flags) or die "Error making dir: $subdir;";
        }
        move($file, $subdir);
    }
}

#=======================================================================
# name - init
# purpose - get input parameters and flags
#=======================================================================
sub init {
    use Getopt::Long;
    my ($verbose);

    GetOptions( "v" => \$verbose );
    $workdir = shift @ARGV;

    $flags{"verbose"} = 1 if $verbose;
}
