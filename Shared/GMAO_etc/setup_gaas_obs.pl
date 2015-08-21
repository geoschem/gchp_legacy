#!/usr/bin/env perl
#=======================================================================
# name - setup_gaas_obs
# purpose - organize GAAS observation data into subdirectories expected
#           by the reading program
#
# runtime parameter
# => $workdir: directory location of modis data
#
# runtime flags (choose at least one)
# => $avhrr: organize avhrr patmosx data into subdirectories
# => $modis: organize modis Level2 HDF data into subdirectories
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($workdir, $modis, $avhrr, %flags, $verbose);

# main program
#-------------
{
    use File::Basename ("basename");
    use File::Copy ("move");
    use File::Path ("mkpath");

    my ($file, $base, $adflg);
    my ($label, $Adate, $time, $version);
    my ($type, $jdoy, $subdir);
    my ($year, $month, $day);

    init();
    if ($modis) {
        foreach $file (<$workdir/M?D04_L2.*.NRT.hdf>) {
            $base = basename $file;
            ($label, $Adate, $time, $version) = split /[\.]/, $base;

            $type = substr($label, 0, 5);
            $year = substr($Adate, 1, 4);
            $jdoy = substr($Adate, 5, 3);

            $subdir = "$workdir/$type/$version/$year/$jdoy";
            unless (-d $subdir) {
                mkpath($subdir, \%flags) or die "Error making dir: $subdir;";
                system("touch $subdir/.no_archiving");
            }
            print "mv $file $subdir\n" if $verbose;
            move($file, $subdir);
        }
    }

    if ($avhrr) {
        foreach $file (<$workdir/patmosx_v05r02.*.npz>) {
            $base = basename $file;
            ($label, $adflg, $Adate) = split /[\.]/, $base;

            $year  = substr($Adate, 0, 4);
            $month = substr($Adate, 4, 2);
            $day   = substr($Adate, 6, 2);

            $subdir = "$workdir/Y$year/M$month/D$day";
            unless (-d $subdir) {
                mkpath($subdir, \%flags) or die "Error making dir: $subdir;";
                system("touch $subdir/.no_archiving");
            }
            print "mv $file $subdir\n" if $verbose;
            move($file, $subdir);
        }
    }
            
}

#=======================================================================
# name - init
# purpose - get input parameters and flags
#=======================================================================
sub init {
    use Getopt::Long;

    GetOptions( "v"     => \$verbose,
                "modis" => \$modis,
                "avhrr" => \$avhrr );
    $workdir = shift @ARGV;

    $flags{"verbose"} = 1 if $verbose;
}
