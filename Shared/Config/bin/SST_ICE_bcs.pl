#!/usr/bin/env perl
#=======================================================================
# name - SST_ICE_bcs.pl
# purpose - create boundary condition files for SST and ice fractions
#
# revision history
# 02Jan2013  Akella  Initial version of code; written in csh
# 07Mar2013  Stassi  Convert to perl
#=======================================================================
use strict;
use warnings;
use FindBin;
use lib ("$FindBin::Bin");

# global variables
#-----------------
my ($date_start, $date_end, $outdir, $nlat, $nlon, $save, $verbose);
my ($reynoldsDFLT, $ostiaDFLT, $reynoldsDIR, $ostiaLABEL, $ostiaDIR);
my ($nsidcDFLT, $nsidcDIR, $northDIR, $southDIR);
my ($nlat_DFLT, $nlon_DFLT, $save_DFLT);
my ($OVERWRITE);

# defaults
#---------
$reynoldsDFLT = "/discover/nobackup/dao_ops/intermediate/flk/stage/reynolds_daily";
$ostiaDFLT = "/archive/input/dao_ops/obs/flk/ukmet_sst/netcdf/OSTIA";
$nsidcDFLT = "/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/NSIDC";
$save_DFLT = 0;

$ostiaLABEL = "-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc";

$nlat_DFLT = 1440;
$nlon_DFLT = 2880;

# main program
#-------------
{
    use Manipulate_time ("tick");
    use WriteLog qw(chdir_ unlink_ system1_ copy_ print_);

    my ($inputfile, $date, $date_next, $yyyy, $mm, $dd);
    my ($ostiaPATH, $ostia, $ostiaBZ2, $fileBZ2);
    my ($nsidc_NH, $nsidc_SH, $reynolds);

    init();
    print_("\nCreating boundary condition files for SST and Ice Fraction\n"
           . "==========================================================\n"
           . "start date: $date_start\n"
           . "end date:   $date_end\n"
           . "==========================================================\n");
    dmget_inputs();

    chdir_($outdir);
    $inputfile = "input_stuff.txt";

    $date = $date_start;
    while ($date < $date_end) {
        $date_next = tick($date);

        print_("\n[$date => $date_next]\n"
               . "~~~~~~~~~~~~~~~~~~~~~~\n");

        $yyyy = substr($date, 0, 4);
        $mm   = substr($date, 4, 2);
        $dd   = substr($date, 6, 2);

        $ostiaPATH = "$ostiaDIR/Y$yyyy/M$mm";
        $ostia = "$date$ostiaLABEL";
        $ostiaBZ2 = "$ostia.bz2";

        $nsidc_NH = "$northDIR/$yyyy/nt_$yyyy$mm${dd}_f17_nrt_n.bin";
        $nsidc_SH = "$southDIR/$yyyy/nt_$yyyy$mm${dd}_f17_nrt_s.bin";
        $reynolds = "$reynoldsDIR/avhrr-only-v2.$date.nc";


        unlink_($inputfile) if -e $inputfile;
        $fileBZ2 = "$ostiaPATH/$ostiaBZ2";
        if (-e $fileBZ2) {
            unless (overwrite_previous_output($date)) {
                print_("skipping\n");
                $date = $date_next;
                next;
            }
            copy_($fileBZ2, $ostiaBZ2);
            unlink_($ostia); system1_("bunzip2 $ostiaBZ2");
            write_inputfile($inputfile, $date, $date_next, $reynolds,
                            $nsidc_NH, $nsidc_SH, $ostia, $nlat, $nlon, $save);
            system1_("$FindBin::Bin/make_ostia_nsidc_bcs.x $inputfile");
            unlink_("$ostia");
            unlink_($inputfile);
        }
        $date = $date_next;
    }
}

#=======================================================================
# name - init
# purpose - get input parameters and flags
#=======================================================================
sub init {
    use Getopt::Long;
    use WriteLog qw(openLOG realpath_ mkpath_ query move_);
    my ($interactive, $help, $logfile);

    GetOptions( "start=s"    => \$date_start,
                "end=s"      => \$date_end,
                "outdir=s"   => \$outdir,
                "i"          => \$interactive,
                "nlat=s"     => \$nlat,
                "nlon=s"     => \$nlon,
                "reynolds=s" => \$reynoldsDIR,
                "ostia=s"    => \$ostiaDIR,
                "nsidc=s"    => \$nsidcDIR,
                "save"       => \$save,
                "v"          => \$verbose,
                "h|help"     => \$help );
    usage() if $help;
    $verbose = 0 unless $verbose;

    # get input parameters
    #---------------------
    ($date_start, $date_end, $outdir) = @ARGV;

    # check start and end dates
    #--------------------------
    $date_start = "" unless $date_start;
    $date_end   = "" unless $date_end;

    until ( $date_start =~ m/^\d{8}$/ ) {
        $date_start = query("Enter start date (yyyymmdd)");
    }
    until ( $date_end =~ m/^\d{8}$/ ) {
        $date_end = query("Enter end date (yyyymmdd)  ");
        if ($date_end < $date_start) {
            print "Error: end date must be greater than start date\n"
                . "   start date: $date_start\n"
                . "     end date: $date_end\n\n";
            $date_end = "";
        }
    }

    # check $outdir directory
    #------------------------
    until ($outdir) {
        $outdir = query("Enter OUTPUT directory for new restarts:", "");
    }
    $outdir = realpath_($outdir);
    mkpath_($outdir) unless -d $outdir;

    # open LOG
    #---------
    $logfile = "$outdir/SST_ICE_bcs.$date_start\_$date_end.LOG";
    move_($logfile, "$logfile~") if -e $logfile;
    openLOG($logfile);

    # check lat and lon values
    #-------------------------
    unless (defined($nlat)) {
        if ($interactive) { $nlat = query("Enter ocean grid nlat", $nlat_DFLT) }
        else              { $nlat = $nlat_DFLT }
    }    
    unless (defined($nlon)) {
        if ($interactive) { $nlon = query("Enter ocean grid nlon", $nlon_DFLT) }
        else              { $nlon = $nlon_DFLT }
    }    

    # check REYNOLDS input directory
    #-------------------------------
    {
        until ($reynoldsDIR) {
            if ($interactive) {
                $reynoldsDIR = query("Enter REYNOLDS input directory", $reynoldsDFLT);
            }
            else {
                $reynoldsDIR = $reynoldsDFLT;
            }
        }
        unless (-d $reynoldsDIR) {
            print "Cannot find directory: $reynoldsDIR\n";
            $reynoldsDIR = "";
            $interactive = 1;
            redo;
        }
    }
        
    # check OSTIA input directory
    #----------------------------
    {
        until ($ostiaDIR) {
            if ($interactive) {
                $ostiaDIR = query("Enter OSTIA input directory", $ostiaDFLT);
            }
            else {
                $ostiaDIR = $ostiaDFLT;
            }
        }
        unless (-d $ostiaDIR) {
            print "Cannot find directory: $ostiaDIR\n";
            $ostiaDIR = "";
            $interactive = 1;
            redo;
        }
    }
        
    # check NSIDC directory
    #----------------------
    {
        until ($nsidcDIR) {
            if ($interactive) {
                $nsidcDIR = query("Enter NSIDC input directory", $nsidcDFLT);
            } 
            else {
                $nsidcDIR = $nsidcDFLT;
            }
        }
        unless (-d $nsidcDIR) {
            print "Cannot find directory: $nsidcDIR\n";
            $nsidcDIR = "";
            $interactive = 1;
            redo;
        }

        $northDIR = "$nsidcDIR/NRT/north";
        unless (-d $northDIR) {
            print "Cannot find NSIDC/NRT/north directory: $northDIR\n";
            $nsidcDFLT = $nsidcDIR;
            $nsidcDIR = "";
            $interactive = 1;
            redo;
        }

        $southDIR = "$nsidcDIR/NRT/south";
        unless (-d $southDIR) {
            print "Cannot find NSIDC/NRT/south directory: $southDIR\n";
            $nsidcDFLT = $nsidcDIR;
            $nsidcDIR = "";
            $interactive = 1;
            redo;
        }
    }

    # save OSTIA-Reynolds Blend?
    #---------------------------
    unless (defined($save)) {
        if ($interactive) {
            $save = query("Save OSTIA-Reynolds Blend (y/n)?", "n");
            if (lc $save eq "y") { $save = 1         }
            else                 { $save = $save_DFLT }
        } else { $save = $save_DFLT }
    }

}

#=======================================================================
# name - dmget_inputs
# purpose - demigrate inputs as a group ahead of time rather than 
#           individually during processing as needed
#=======================================================================
sub dmget_inputs {
    use WriteLog qw(realpath_ system_);
    my ($date, $yyyy, $mm, $pid, @pidArr);
    my ($ostiaPATH, $ostiaBZ2, $fileBZ2, @ostiaInputs);

    # demigrate not needed unless inputs located on archive directories
    #------------------------------------------------------------------
    return unless realpath_($ostiaPATH) =~ m/archive/;

    $date = $date_start;
    while ($date < $date_end) {

        # parse date
        #-----------
        $yyyy = substr($date, 0, 4);
        $mm   = substr($date, 4, 2);

        $ostiaPATH = "$ostiaDIR/Y$yyyy/M$mm";
        $ostiaBZ2 = "$date$ostiaLABEL.bz2";
        $fileBZ2 = "$ostiaPATH/$ostiaBZ2";

        # make list of files to dmget
        #----------------------------
        if (-e $fileBZ2) {
            push @ostiaInputs, $fileBZ2;
            next unless scalar(@ostiaInputs) > 50;
        }

        # use forked job to dmget files
        #------------------------------
        defined($pid = fork) or die "Error during fork operation;";
        unless ($pid) {
            system_("dmget @ostiaInputs", $verbose);
            exit;
        }
        push @pidArr, $pid;
    }

    # in verbose mode, wait for dmget commands to complete
    #-----------------------------------------------------
    if ($verbose) {
        while (@pidArr) { $pid = shift(@pidArr); waitpid($pid,0) }
    }
}

#=======================================================================
# name - overwrite_previous_output
# purpose - if output already exists, then ask user whether it is okay
#           to overwrite it
#=======================================================================
sub overwrite_previous_output {
    use WriteLog ("query");
    my ($date, $sst, $fraci, $ans, $overwrite);

    $date = shift @_;

    $sst = "sst_$date.bin";
    $fraci = "fraci_$date.bin";
    return 1 unless -e $sst or -e $fraci;

    if (defined($OVERWRITE)) { $overwrite = $OVERWRITE }
    else {

        # show user pre-existing output
        #------------------------------
        print_("\nPre-existing output has been found for $date\n");
        if (-e $sst)   { print_("$sst (FOUND)\n")   }
        else           { print_("$sst (not found)\n")   }
        if (-e $fraci) { print_("$fraci (FOUND)\n") }
        else           { print_("$fraci (not found)\n") }

        # how does user want to handle pre-existing output
        #-------------------------------------------------
        $ans = 0;
        until ($ans == 1 or $ans == 2 or $ans == 3 or $ans ==4) {
            print_("\n1. overwrite output this day only\n"
                   . "2. overwrite all days with pre-existing output\n"
                   . "3. skip processing this day only\n"
                   . "4. skip all days with pre-existing output\n");
            $ans = query("Select:", 1)
        }

        $overwrite = 1 if $ans == 1 or $ans == 2;
        $overwrite = 0 if $ans == 3 or $ans == 4;
        $OVERWRITE = 1 if $ans == 2;
        $OVERWRITE = 0 if $ans == 4;
    }

    # remove pre-existing output
    #---------------------------
    if ($overwrite) {
        unlink_($sst)   if -e $sst;
        unlink_($fraci) if -e $fraci;
    }    

    return $overwrite;
}

#=======================================================================
# name - write_inputfile
# purpose - write inputs for make_ostia_nsidc_bcs.x to file
#=======================================================================
sub write_inputfile {
    use WriteLog qw(print_);
    my ($inputfile, $date, $date_next, $reynolds, $nsidc_NH, $nsidc_SH);
    my ($ostia, $nlat, $nlon, $save);

    $inputfile = shift @_;

    open INPUT, "> $inputfile" or die "Error opening input file: $inputfile;";
    print_("\nwriting $inputfile:\n");
    foreach (@_) { print INPUT "$_\n"; print_("$_\n") }
    print_("\n");
    close INPUT;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my $script = basename $0;

    print << "EOF";

usage: $script startdate enddate outdir [options]
where
   startdate     start date (yyyymmdd)
   enddate       end date (yyyymmdd)
   outdir        output directory

options
   -i               interactive mode; prompt for unsupplied options
   -nlat            latitude precision of ocean grid (default: $nlat_DFLT)
   -nlog            longitude precision of ocean grid (default: $nlon_DFLT)
   -reynolds dir    location of REYNOLDS data (default: $reynoldsDFLT)
   -ostia dir       location of OSTIA data (default: $ostiaDFLT)
   -nsidc dir       location of nsidc data (default: $nsidcDFLT)
   -save            save OSTIA-Reynolds Blend (default: no)
   -v               verbose mode; shows dmget commands (see note below)

note:
  In verbose mode, the script will wait for all the dmget commands to complete
  before processing any of the data.

EOF
exit;
}
