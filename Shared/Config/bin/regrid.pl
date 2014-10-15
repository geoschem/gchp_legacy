#!/usr/bin/env perl
#=======================================================================
# name - regrid_pl
# purpose - wrapper script for rs_hinterp.x and mk_Restarts programs
#
# revision history
# 24Nov2009  Stassi    initial version of code
# 09Sep2010  Stassi    compile rs_hinterp if needed; write rst.lcv
# 03Apr2012  Stassi    Added merra and cubed-sphere grid options
# 30Nov2012  Stassi    Updated to handle Ganymed-1_0_{M,D} tags
# 08May2013  Stassi    New 1-step regridding for all restarts except catch
#=======================================================================
use strict;
use warnings;
use FindBin qw($Bin);
use lib "$FindBin::Bin";

use WriteLog qw(openLOG closeLOG display query setprompt);
use WriteLog qw(chdir_ copy_ mkpath_ move_ print_ printLOG_ realpath_);
use WriteLog qw(symlink_ system_ unlink_ );

# global variables
#-----------------
my ($bcsHEAD, $bcsTagIN, $bcsTagOUT, $bkgFLG, $capture, $c2c, $c2cX);
my ($debug, $dbHash, $drymassFLG, $ESMABIN, $ESMATAG, $expid);
my ($gcmFLG, $g5modules, $getsponsorX, $grIN, $grINocean, $grOUT, $grOUTocean);
my ($hr, $interactive, $interp_restartsX, $landIceDT, $lblFLG, $lcvFLG);
my ($logfile, $merra, $mk_RestartsX, $mkdrstdateX, $month, $newid, $node);
my ($noprompt, $outdir, $regridj, $rs_hinterpX, $rsFLG, $rs_scaleX);
my ($rstdir, $rst_template, $rstIN_template, $scale_catchX, $surfFLG, $surflay);
my ($tagIN, $tagOUT, $upairFLG, $verbose, $workdir, $year, $ymd);
my (%IN, %iceIN, %OUT);
my (%CS, %im, %im4, %imo, %imo4, %jm, %jm4, %jm5, %jmo, %jmo4);
my (@SURFACE, @UPPERAIR_OPT, @UPPERAIR_REQ, @anafiles, @warnings);

# global tag variables
#---------------------
my $former_tag  = "Fortuna-2_1";       # default for input restarts
my $current_tag = "Ganymed-2_0";       # default for output restarts

my (@GCMtags, @DAStags);
my (@F14, @F20, @F21, @G10, @G10p, @G20, @G30, @G40);
my (@D214, @D540, @D561, @D580, @D591p, @D5A0, @D5B0, @D512);
my (%bcsTAG, %rank, %landIceVER, $landIceFLG);

# atmosphere lat/lon grids
#-------------------------
$im{"a"} =   "72"; $jm{"a"} =  "46";
$im{"b"} =  "144"; $jm{"b"} =  "91";
$im{"c"} =  "288"; $jm{"c"} = "181";
$im{"D"} =  "540"; $jm{"D"} = "361";
$im{"d"} =  "576"; $jm{"d"} = "361";
$im{"E"} = "1080"; $jm{"E"} = "721";
$im{"e"} = "1152"; $jm{"e"} = "721";

# ocean grids
#------------
$imo{"c"} =  "360"; $jmo{"c"} = "180";     # OSTIA
$imo{"e"} = "1440"; $jmo{"e"} = "720";     # MERRA-2
$imo{"f"} = "2880"; $jmo{"f"} = "1440";    # Reynolds

# atmosphere cubed-sphere grids
#------------------------------
foreach (qw/ 48 90 180 360 500 720 1000 1440 2000 2880 /) {
    $CS{"C$_"} = $_;
    $im{"C$_"} = $_;
    $jm{"C$_"} = 6*$_;
}

foreach (keys %im)  { $im4{$_}  = sprintf "%04i", $im{$_}  }
foreach (keys %jm)  { $jm4{$_}  = sprintf "%04i", $jm{$_}  }
foreach (keys %jm)  { $jm5{$_}  = sprintf "%05i", $jm{$_}  }
foreach (keys %imo) { $imo4{$_} = sprintf "%04i", $imo{$_} }
foreach (keys %jmo) { $jmo4{$_} = sprintf "%04i", $jmo{$_} }

# restart lists
#--------------
@UPPERAIR_REQ = ( "fvcore_internal_rst",
                  "moist_internal_rst" );

@UPPERAIR_OPT = ( "agcm_import_rst",
                  "agcm_internal_rst",
                  "gocart_internal_rst",
                  "pchem_internal_rst" );

@SURFACE      = ( "catch_internal_rst",
                  "landice_internal_rst",
                  "lake_internal_rst",
                  "saltwater_internal_rst" );

#=======================================================================
# main program
#=======================================================================
{
    init();

    check_inputs();
    check_programs();
    check_rst_files();
    create_workdir_and_logfile();
    
    getLandIceInput() if $landIceFLG;
    set_IN_OUT();
    confirm_inputs();
    dmget_input_restarts();

    if ($upairFLG) {
        printlabel("\nUpperair Restarts");

        if ($grIN eq $grOUT) {
            copy_upperair_rsts();
        }
        else {
            if ($CS{$grOUT}) { regrid_upperair_rsts_CS() }
            else             { regrid_upperair_rsts_LATLON() }
            rename_upperair_rsts();
        }
        if ($drymassFLG) {
            set_dry_mass();
        }
    }
    if ($surfFLG) {
        regrid_surface_rsts(\%IN, \%OUT);
        regrid_surface_rsts(\%iceIN, \%OUT) if $landIceDT;
    }
    get_anafiles()  if $bkgFLG;
    write_rst_lcv() if $lcvFLG;
    write_CMD_file();
    cleanup();
}

#=======================================================================
# name - init
# purpose - get runtime parameters, check environment, initialize global
#           variables
#=======================================================================
sub init {
    use File::Basename ("basename");
    use Getopt::Long;
    my ($help, $prompt, $ESMAETC);

    $| = 1;    # flush STDOUT buffer

    # capture command input
    #----------------------
    $interactive = 1 unless @ARGV;
    $capture = basename($0); foreach (@ARGV) { $capture .= " $_" }

    # identify platform
    #------------------
    chomp($node = `uname -n`);
    if ( $node =~ /^borg/ or $node =~ /^dirac/ or
         $node =~ /^dali/ or $node =~ /^discover/ ) { $node = "nccs" }
    die "Error. This script runs on NCCS machines only;\n" unless $node eq "nccs";

    # initialize tag arrays and hashes
    #---------------------------------
    init_tag_arrays_and_hashes();

    # runtime options
    #----------------
    GetOptions("ymd=i"           => \$ymd,
               "hr=i"            => \$hr,
               "grout|gridout=s" => \$grOUT,
               "outdir=s"        => \$outdir,
               "merra"           => \$merra,
               "d=s"             => \$rstdir,
               "expid=s"         => \$expid,
               "i"               => \$interactive,
               "oceanin=s"       => \$grINocean,
               "oceanout=s"      => \$grOUTocean,
               "esmabin=s"       => \$ESMABIN,
               "iceDT=s"         => \$landIceDT,
               "newid=s"         => \$newid,
               "tagin=s"         => \$tagIN,
               "tagout=s"        => \$tagOUT,
               "rs=i"            => \$rsFLG,
               "bkg!"            => \$bkgFLG,
               "lbl!"            => \$lblFLG,
               "lcv!"            => \$lcvFLG,
               "gcm"             => \$gcmFLG,
               "np"              => \$noprompt,
               "db|debug|nc"     => \$debug,
               "dbh"             => \$dbHash,
               "v"               => \$verbose,
               "h|help"          => \$help);

    usage() if $help;
    setprompt(0) if $noprompt;

    # check for inputs as runtime parameters
    #---------------------------------------
    ($ymd, $hr, $grOUT, $outdir) = @ARGV unless $ymd;

    # defaults
    #---------
    $dbHash  = 1 if $debug;
    $verbose = 0 unless $verbose;

    # turn-off alternate landIce processing
    #--------------------------------------
    if (defined($landIceDT) and $landIceDT eq "0") {
        $landIceFLG = 0;
        $landIceDT = "";
    }

    # get value for $ESMABIN and ESMATAG
    #-----------------------------------
    $ESMABIN = $Bin unless $ESMABIN;
    $ESMAETC = dirname($ESMABIN) . "/etc";
    if (-e "$ESMAETC/CVSTAG") { chomp($ESMATAG = `cat $ESMAETC/CVSTAG`) }
    $ESMATAG = "???" unless $ESMATAG;

    # GCM BCS head directory location
    #--------------------------------
    $bcsHEAD = "/discover/nobackup/ltakacs/bcs";
}

#=======================================================================
# name - check_inputs
# purpose - query user for runtime arguments and options if they were
#           not supplied (if interactive mode)
#=======================================================================
sub check_inputs {
    my ($fvrst, $ans, $prompt, $len, $msg, $warnFLG);
    my ($grINocean_dflt, $grOUTocean_dflt, $rstlcvIN);
    my ($newid_dflt, $bkg_dflt, $lcv_dflt, $lbl_dflt, $dflt);
    my ($landIceVERin, $landIceVERout);

    # location of input restarts
    #---------------------------
    until ($merra or ($rstdir and -d $rstdir)) {
        $prompt = "Do you want script to select MERRA input restarts for you";
        $ans = query("\n$prompt (y/n)?", "n");
        if (lc($ans) eq "y") {
            $merra = 1;
        } else {
            $rstdir = query("Enter INPUT restart directory:");
            print "\nCannot find restart dir: $rstdir\n" unless -d $rstdir;
        }
    }
    if ($rstdir) {
        $rstdir =~ s/\/*$//;    # remove trailing '/'s
        $rstdir = realpath_($rstdir);
    }

    # check $ymd and $hr
    #-------------------
    until ($ymd and $ymd =~ /^\d{8}$/) {
        $ymd = query("Enter ymd (yyyymmdd):");
    }

    $hr = "0".$hr if defined($hr) and length($hr) == 1;
    until ($hr and $hr =~ /^\d{2}$/) {
        $hr = query("Enter hour (hh):");
        $hr = "0.$hr" if length($hr) == 1;
    }        
    $year  = substr($ymd, 0, 4);
    $month = substr($ymd, 4, 2);

    # set values for MERRA restarts
    #------------------------------
    if ($merra) {
        $rstdir = "/archive/g_proj5/production/GEOSdas-2_1_4";
        $tagIN  = "Fortuna-1_4";
        $grIN      = "d";
        $grINocean = "c";

        if    ($year < 1979) { die "Error. MERRA data < 1979 not available\n" }
        elsif ($year < 1989) { $expid = "d5_merra_jan79" }
        elsif ($year < 1998) { $expid = "d5_merra_jan89" }
        else                 { $expid = "d5_merra_jan98" }
    }
    die "\nCannot find restart dir: $rstdir" unless -d $rstdir;
    $rstdir =~ s/\/*$//;    # remove trailing '/'s

    # get INPUT fvcore_internal_rst
    #------------------------------
    $fvrst = get_fvrst();

    # check atmosphere grids: $grIN and $grOUT
    #-----------------------------------------
    printlabel("\nAtmosphere Grids");
    print_("\nLat/Lon Grids       Cubed-Sphere Grids\n"
        .    "-------------       ------------------\n"
        .    "b = 2 deg           C48     C360     C1000\n"
        .    "c = 1 deg           C90     C500     C1440\n"
        .    "d = 1/2 deg         C180    C720     C2880\n"
        .    "e = 1/4 deg\n\n");

    print "FVCORE: $fvrst\n"
        . "Getting atmosphere grid resolution from INPUT fvcore file ... ";
    $grIN = determine_fvcore_grid($fvrst);
    print "DONE\nINPUT atmosphere grid: $grIN\n";

    if ($grOUT and $im{$grOUT}) {
        print "OUTPUT atmosphere grid: $grOUT\n";
    }
    else {
        until ($grOUT and $im{$grOUT}) {
            $grOUT = query("Enter OUTPUT atmosphere grid:");
        }
    }

    # check ocean grids: $grINocean and $grOUTocean
    #----------------------------------------------
    $grINocean_dflt  = "c";
    $grOUTocean_dflt = "c";

    unless ($grINocean  and $imo{$grINocean} and
            $grOUTocean and $imo{$grOUTocean}) {
        print "\nOcean Grids\n"
            .   "-----------\n"
            .   "c =  360x180   (Reynolds)\n"
            .   "e = 1440x720   (MERRA-2)\n"
            .   "f = 2880x1440  (OSTIA)\n\n";
    }
    print "INPUT ocean grid: $grINocean\n" if $grINocean and $imo{$grINocean};
    until ($grINocean and $imo{$grINocean}) {
        $grINocean = query("Enter INPUT ocean grid:", $grINocean_dflt);
    }
    print "OUTPUT ocean grid: $grOUTocean\n" if $grOUTocean and $imo{$grOUTocean};
    until ($grOUTocean and $imo{$grOUTocean}) {
        $grOUTocean = query("Enter OUTPUT ocean grid:", $grOUTocean_dflt);
    }

    # check tag info: $tagIN and $tagOUT
    #-----------------------------------
    $bcsTagIN  = resolve_bcsTAG($tagIN,  $grINocean,  "in")  if $tagIN;
    $bcsTagOUT = resolve_bcsTAG($tagOUT, $grOUTocean, "out") if $tagOUT;

    unless ($bcsTagIN and $bcsTagOUT) {
        print "\nSample GCM tags\n"
            .   "---------------\n";
        foreach (@GCMtags) {
            printf "%-5s: %s  ", $$_[0], $$_[1];
            $len = 20 - length($$_[1]);
            if ($$_[2]) { print "."x$len ."  $$_[-1]" }
            else        { print "."x$len ."  $$_[1]"  }
            print "\n";
        }
        print "\nSample DAS tags\n"
            .   "---------------\n";
        foreach (@DAStags) {
            printf "%-5s: %s  ", $$_[0], $$_[1];
            $len = 20 - length($$_[1]);
            if ($$_[2]) { print "."x$len ."  $$_[-1]" }
            else        { print "."x$len ."  $$_[1]"  }
            print "\n";
        }
    }
    until ($bcsTagIN) {
        print_("\nType 'bcs' to see BCS tags or\n");
        $tagIN = query("Enter GCM or DAS tag for inputs:", $former_tag);
        $bcsTagIN = resolve_bcsTAG($tagIN, $grINocean, "in");
        $tagIN = $bcsTagIN if $tagIN eq "bcs";
    }
    until ($bcsTagOUT) {
        print_("\nType 'bcs' to see BCS tags or\n");
        $tagOUT = query("Enter GCM or DAS tag for outputs:", $current_tag);
        $bcsTagOUT = resolve_bcsTAG($tagOUT, $grOUTocean, "out");
        $tagOUT = $bcsTagOUT if $tagOUT eq "bcs";
    }
    if ($rank{$bcsTagOUT} >= 12 ) { $surflay = 50; $drymassFLG = 1 }
    else                          { $surflay = 20 }

    # set $landIceFLG if landice_internal_rst requires special regrid
    #----------------------------------------------------------------
    unless (defined($landIceFLG)) {
        $landIceVERin  = $landIceVER{$tagIN};
        $landIceVERout = $landIceVER{$tagOUT};

        if ($landIceVERin and $landIceVERout) {
            $landIceFLG = 1 if $landIceVERin == 1 and $landIceVERout > 1;
        }
        else {
            if ($landIceDT) { $landIceFLG = 1 }
            else {
                $prompt = "Use landice restart from G10_ICE experiment";
                $ans = query("$prompt (y/n)?", "y");
                $landIceFLG = 1 unless lc($ans) eq "n";
                $landIceFLG = 0 unless $landIceFLG;
            }
        }
        $landIceDT = "" unless $landIceFLG;
    }

    # which restarts ?
    #-----------------
    $rsFLG = 0 unless $rsFLG;
    $rsFLG = 0 unless $rsFLG == 1 or $rsFLG == 2 or $rsFLG == 3;

    $upairFLG = 1;
    $surfFLG  = 1;

    unless ($rsFLG) {
        if ($interactive) {
            $ans = query("\nRegrid upperair restarts (y/n)?", "y");
            $rsFLG = 1 unless lc($ans) eq "n";

            $ans = query("Regrid surface restarts (y/n)?", "y");
            $rsFLG += 2 unless lc($ans) eq "n";
        }
        else { $rsFLG = 3 }
    }
    $upairFLG = 0 if $rsFLG == 2;
    $surfFLG  = 0 if $rsFLG == 1;

    # checks needed for cubed-sphere inputs
    #--------------------------------------
    $c2c = 0;
    if ($CS{$grIN}) {
        $c2c = 1;

        # cannot regrid cubed to higher resolution cubed
        #-----------------------------------------------
        if ($CS{$grOUT}) {
            die "Error. Cannot regrid cubed-sphere to higher resolution;"
                if $CS{$grOUT} > $CS{$grIN};
        }

        # cannot regrid upperair cubed to lat/lon
        #----------------------------------------
        elsif ($rsFLG == 1 or $rsFLG == 3) {
            print "\nWARNING. Cannot regrid cubed-sphere upperair to lat/lon\n";
            cleanup() if $rsFLG == 1;

            $ans = query("Continue with surface restarts (y/n)?", "n");
            cleanup() unless lc($ans) eq "y";

            print "WARNING. Upperair regrid turned off";
            $c2c = 0;
            $rsFLG = 2;
            $upairFLG = 0;
        }
    }

    # check $outdir
    #--------------
    print "\n";
    until ($outdir) {
        $outdir = query("Enter OUTPUT directory for new restarts:");
    }
    mkpath_($outdir) unless -d $outdir;
    $outdir = realpath_($outdir);
    chdir_($outdir, $verbose);

    # check $newid
    #-------------
    if ($expid) { $newid_dflt  = "$grOUT${grOUTocean}_$expid" }
    else        { $newid_dflt  = "$grOUT$grOUTocean"          }

    if ($interactive) {
        until ($newid) {
            $newid = $newid_dflt unless $newid;
            $newid = query("Enter Experiment ID for OUTPUT restarts", $newid);
        }
    } else {
        $newid = $newid_dflt unless $newid;
    }

    # check $bkgFLG, $lblFLG, and $lcvFLG
    #------------------------------------
    $bkgFLG = 0 if $bcsTagIN eq "Fortuna-1_4";

    if ($gcmFLG) {
        $bkgFLG = 0;
        $lblFLG = 1;
        $lcvFLG = 0;
    }

    # DAS mode defaults
    #------------------
    $rstlcvIN = rstname($expid, "rst.lcv", "$rstdir/$rstIN_template");
    if ($expid and -e $rstlcvIN) {
        $bkg_dflt = 1;
        $lbl_dflt = 0;
        $lcv_dflt = 1;
    }

    # probably gcm restarts if no $expid;
    # probably gcm restarts if no INPUT rst.lcv
    #------------------------------------------
    else {
        $bkg_dflt = 0;
        $lbl_dflt = 1;
        $lcv_dflt = 0;
    }

    if ($interactive) {
        until (defined($bkgFLG)) {
            if ($bkg_dflt) { $dflt = "y" }
            else           { $dflt = "n" }
            $ans = query("\nCopy bkg, satbang/satbias files (y/n)?", $dflt);
            $bkgFLG = 0 if lc($ans) eq "n";
            $bkgFLG = 1 if lc($ans) eq "y";
        }
        until (defined($lcvFLG)) {
            if ($lcv_dflt) { $dflt = "y" }
            else           { $dflt = "n" }
            $ans = query("Write rst.lcv restart (y/n)?", $dflt);
            $lcvFLG = 0 if lc($ans) eq "n";
            $lcvFLG = 1 if lc($ans) eq "y";
        }
        until (defined($lblFLG)) {
            if ($lbl_dflt) { $dflt = "y" }
            else           { $dflt = "n" }
            $prompt = "Append tag/grid label to names of final restarts (y/n)?";
            $ans = query($prompt, $dflt);
            $lblFLG = 0 if lc($ans) eq "n";
            $lblFLG = 1 if lc($ans) eq "y";
        }
    }
    $bkgFLG = $bkg_dflt unless defined($bkgFLG);
    $lblFLG = $lbl_dflt unless defined($lblFLG);
    $lcvFLG = $lcv_dflt unless defined($lcvFLG);

    # check for consistency between tags and ocean grids
    #---------------------------------------------------
    if ($grINocean eq "e" and $bcsTagIN !~ m/_MERRA-2$/) {
        die "\n" ."!"x63 ."\n"
            . "Error. Inconsistency between input BCS tag and input ocean grid\n"
            . "  tagIN     = $tagIN\n"
            . "  bcsTagIN  = $bcsTagIN\n"
            . "  grINocean = $grINocean\n"
            . "!"x63 ."\n\n";
    }
    if ($grOUTocean eq "e" and $bcsTagOUT !~ m/_MERRA-2$/) {
        die "\n" ."!"x65 ."\n"
            . "Error. Inconsistency between output BCS tag and output ocean grid\n"
            . "  tagOUT     = $tagOUT\n"
            . "  bcsTagOUT  = $bcsTagOUT\n"
            . "  grOUTocean = $grOUTocean\n"
            . "!"x65 ."\n\n";
    }
    if ($grINocean eq "f" and $bcsTagIN !~ m/_Ostia$/ and $bcsTagIN !~ m/_D$/) {
        die "\n" ."!"x63 ."\n"
            . "Error. Inconsistency between input BCS tag and input ocean grid\n"
            . "  tagIN     = $tagIN\n"
            . "  bcsTagIN  = $bcsTagIN\n"
            . "  grINocean = $grINocean\n"
            . "!"x63 ."\n\n";
    }
    if ($grOUTocean eq "f" and $bcsTagOUT !~ m/_Ostia$/ and $bcsTagOUT !~ m/_D$/) {
        die "\n" ."!"x65 ."\n"
            . "Error. Inconsistency between output BCS tag and output ocean grid\n"
            . "  tagOUT     = $tagOUT\n"
            . "  bcsTagOUT  = $bcsTagOUT\n"
            . "  grOUTocean = $grOUTocean\n"
            . "!"x65 ."\n\n";
    }

    # check for error conditions
    #---------------------------
    if ($rank{$bcsTagOUT} < $rank{"Ganymed-1_0"} and $CS{$grOUT}) {
        die "Error. Cannot create cubed-sphere restarts for Fortuna tags\n";
    }
    if (($expid eq $newid) and (realpath_($outdir) eq realpath_($rstdir))) {
        die "Error. expid equals newid (=$expid) "
            . "and outdir equals rstdir (=$rstdir);";
    }

    # need non-archive $outdir for CS restarts
    #-----------------------------------------
    if ($outdir =~ /archive/ and $CS{$grOUT} and $upairFLG) {
        warn "\n=======\nWARNING\n=======\n"
            ."> CS upper-air restarts are created with a PBS job.\n"
            ."> PBS jobs cannot write to the archive directories.\n"
            ."> Your OUTPUT directory appears to be in the archives.\n\n"
            ."outdir: $outdir\n\n";
        $ans = query("Quit (y/n)", "y");
        print "\n";
        exit unless lc($ans) eq "n";
        print "Continuing. Please check your output carefully!\n";
        $msg = "Cannot write regridded CS upper-air restarts".
            " to archive directory: $outdir";
        push @warnings, $msg;
    }

    # check grIN and grOUT values
    #----------------------------
    #--print("WARNING: grIN equals grOUT (=$grIN);\n") if $grIN eq $grOUT;
}

#=======================================================================
# name - get_fvrst
# purpose - find fvcore restart file in INPUT restart directory and
#           use it to determine naming template of restart files
#=======================================================================
sub get_fvrst {
    use File::Basename ("basename");
    my (@fvrsts, $pattern, $rstdir1, $fvrst, $fvrstname, $id);
    my (%IDfound, @idArr, $expid_dflt, $prompt);

    # check for fvcore restart
    #-------------------------
    @fvrsts = ();
    $pattern = "*fvcore_internal_rst*${ymd}_$hr*";
    @fvrsts = <$rstdir/$pattern>;

    unless (@fvrsts) {
        $pattern = "*fvcore_internal_rst*";
        @fvrsts = <$rstdir/$pattern>;
        @fvrsts = filter_datetimes(@fvrsts);
    }

    # if not found, then look in subdirectories
    #------------------------------------------
    unless (@fvrsts) {
        $pattern = "*fvcore_internal_rst*${ymd}_$hr*";
        $rstdir1 = "$rstdir/Y$year/M$month";
        @fvrsts = (<$rstdir1/$pattern>);

        unless (@fvrsts) {
            $rstdir1 = "$rstdir/rs/Y$year/M$month";
            @fvrsts = (<$rstdir1/$pattern>);
        }
        unless (@fvrsts) {
            if ($expid) {
                $rstdir1 = "$rstdir/$expid/rs/Y$year/M$month";
                @fvrsts = (<$rstdir1/$pattern>);
            }
        }
        $rstdir = $rstdir1 if @fvrsts;
    }
    die "\nError. No fvcore_internal_rst found in $rstdir for ${ymd}_$hr"
        unless @fvrsts;

    # attempt to id fvcore by expid
    #------------------------------
    unless (scalar(@fvrsts) == 1) {
        unless ($expid) {
            %IDfound = extract_expids(\@fvrsts);
            @idArr = sort keys %IDfound;
            if (scalar(@idArr) ==  1) {
                $expid = $idArr[0];
            }
            elsif (scalar(@idArr) > 1) {
                until ($expid and $IDfound{$expid}) {
                    print "\n--------------\n"
                        .   "Experiment IDs\n"
                        .   "--------------\n";
                    foreach (@idArr) { print "$_\n" }

                    $expid_dflt = $idArr[-1];
                    $prompt = "\nEnter Experiment ID of INPUT restarts:";
                    $expid = query($prompt, $expid_dflt);
                }
            }
            $expid = "" unless $expid;
        }
        $pattern = "$expid*fvcore_internal_rst*${ymd}_$hr*";
        @fvrsts = (<$rstdir/$pattern>);
    }

    unless (scalar(@fvrsts) == 1) {
        print "Error. Unable to identify unique fvcore_internal_rst file.\n";
        print "expid: $expid\n" if $expid;
        print "date/time: ${ymd}_$hr\n" if $ymd and defined($hr);
        print "===========\n"
            . "files found\n"
            . "===========\n";
        foreach (@fvrsts) { print "=> $_\n" }
        exit;
    }

    # check for experiment ID in fvcore restart name
    #-----------------------------------------------
    $fvrst = $fvrsts[0];
    $fvrstname = basename $fvrst;
    $id = $1 if $fvrstname =~ m/(\w+)\.fvcore_internal_rst/;
    $id = "" unless $id;

    if ($expid and $expid ne $id) {
        print "\nError. Mismatch between expid and fvcore ID\n"
            .   "fvcore rst file: $fvrst\n"
            .   "INPUT expid: $expid\n"
            .   "fvcore ID:   $id\n\n";
        exit;
    }
    $expid = $id unless $expid;

    # get input restart name template
    #--------------------------------
    if ($expid and $fvrstname =~ /^$expid\.fvcore_internal_rst\.(.+)$/) {
        $rstIN_template = "%s.%s.$1";
    }
    elsif ($expid and $fvrstname =~ /^$expid\.fvcore_internal_rst/) {
        $rstIN_template = "%s.%s";
    }
    elsif ($fvrstname =~ /^(.*)fvcore_internal_rst$/) {
        $rstIN_template = "%s%s";
    }
    elsif ($fvrstname =~ /(.*)fvcore_internal_rst\.(.+)$/) {
        $rstIN_template = "%s%s.$2";
    }
    else {
        die "\nError. Cannot find rstIN_template in $fvrstname;";
    }

    # output restart name template
    #-----------------------------
    $rst_template = "%s.%s.${ymd}_${hr}z.bin";

    return $fvrst;
}

#=======================================================================
# name - determine_fvcore_grid
# purpose - determine grid resolution of fvcore_internal_rst file
#
# input parameter
# => fvrst: fvcore_internal_rst file from which to extract grid resolution
#=======================================================================
sub determine_fvcore_grid {
    my ($fvrst, $grID);
    my ($fvrstX, $fvrstXout, $cmd, $im, $jm, $levs, $nymd, $nhms);

    $fvrst = shift @_;

    $fvrstX = "$ESMABIN/fvrst.x";
    die "Error. Program not found: $fvrstX;" unless -x $fvrstX;

    $cmd = "$fvrstX -h $fvrst";
    print_($cmd) if $debug;
    ($fvrstXout = `$cmd`) =~ s/^\s+//;
    ($im, $jm, $levs, $nymd, $nhms) = split /\s+/, $fvrstXout;

    foreach (keys %im) { $grID = $_ if $im{$_} eq $im and $jm{$_} eq $jm }
    unless ($grID) { die "Error\n\nError. Cannot determine grID of $fvrst;" }

    return $grID;
}

#=======================================================================
# name - resolve_bcsTAG
# purpose - determine the BCS tag based on the GCM or DAS tag and the
#           ocean grid resolution
#
# input parameters
# => $tagID: GCM or DAS tag
# => $oceanGRID: ocean grid resolution
#=======================================================================
sub resolve_bcsTAG {
    my ($tagID, $oceanGRID, $flag);
    my ($tagIDx, $bcsTAG);
    my (%reverse, $dflt);

    $tagID     = shift @_;
    $oceanGRID = shift @_;
    $flag      = shift @_;
    $bcsTAG    = undef;

    # check for and remove any label from the tag
    #--------------------------------------------
    $tagIDx = $tagID;
    foreach ( qw( UNSTABLE OPS retired )) { $tagIDx =~ s/_$_$// }

    # is user asking to input a BCS tag directly?
    #--------------------------------------------
    if ($tagID eq "bcs") {
        %reverse = reverse %rank;
        if ($flag eq "in") { $dflt = $bcsTAG{$former_tag}  }
        else               { $dflt = $bcsTAG{$current_tag} }

        print "\nBCS tags\n"
            .   "--------\n";
        foreach ( sort {$a<=>$b} keys %reverse ) {
            printf "%2s. %s\n", $_, $reverse{$_};
        }
        print "\n";
        until ($bcsTAG) {
            $bcsTAG = query("Enter BCS tag for ${flag}puts:", $dflt);
            $bcsTAG = $reverse{$bcsTAG} if $reverse{$bcsTAG};
            unless (-d "$bcsHEAD/$bcsTAG") {
                print "Error. Cannot find bcs directory: $bcsHEAD/$bcsTAG\n\n";
                $bcsTAG = "";
            }
        }
    }

    # get corresponding BCS tag
    #--------------------------
    unless ($bcsTAG) { $bcsTAG = $bcsTAG{$tagIDx}        }
    unless ($bcsTAG) { $bcsTAG = $tagID if $rank{$tagID} }
    unless ($bcsTAG) { $bcsTAG = ""                      }

    # is BCS tag consistent with ocean grid?
    #--------------------------------------
    if ($bcsTAG) {
        if ($oceanGRID eq "f" and $rank{$bcsTAG} < $rank{"Ganymed-1_0_m1"} ) {
            print "Error. Higher resolution ocean grid not available in tag.\n"
                . "        tag: $tagID";
            print " ($tagIDx)" if $tagID ne $tagIDx;
            print "\n"
              .   "     bcsTAG: $bcsTAG\n"
                . " ocean grid: $oceanGRID\n";
            die "Exiting;";
        }
    }

    # was BCS tag found?
    #------------------
    else {
        print "\nWARNING. Cannot recognize Tag ID: $tagIDx";
        print " ($tagID)" if $tagID ne $tagIDx;
        print "\n";
    }

    # change tag for high resolution ocean grid
    #------------------------------------------
    $bcsTAG =~ s/_Reynolds$/_MERRA-2/ if $oceanGRID eq "e";
    $bcsTAG =~ s/_Reynolds$/_Ostia/   if $oceanGRID eq "f";
    $bcsTAG =~ s/_M$/_D/              if $oceanGRID eq "f";

    return $bcsTAG;
}

#========================================================================
# name - init_tag_arrays_and_hashes
# purpose - initialize arrays and hashes which show the relationships
#           between the GCM and DAS tags and the BCS tags
#========================================================================
sub init_tag_arrays_and_hashes {

    # BCS Tag: Fortuna-1_4
    #---------------------
    @F14  = qw( F14                    Fortuna-1_4            Fortuna-1_4_p1 );
    @D214 = qw( 214                    GEOSdas-2_1_4          GEOSdas-2_1_4-m1
                GEOSdas-2_1_4-m2       GEOSdas-2_1_4-m3       GEOSdas-2_1_4-m4 );
    @D540 = qw( 540                    GEOSadas-5_4_0         GEOSadas-5_4_0_p1
                GEOSadas-5_4_0_p2      GEOSadas-5_4_0_p3      GEOSadas-5_4_0_p4
                GEOSadas-5_4_1         GEOSadas-5_4_1_p1      GEOSadas-5_4_2
                GEOSadas-5_4_3         GEOSadas-5_4_4         GEOSadas-5_5_0
                GEOSadas-5_5_1         GEOSadas-5_5_2         GEOSadas-5_5_3 );

    # BCS Tag: Fortuna-2_0
    #---------------------
    @F20  = qw( F20                    Fortuna-2_0 );

    # BCS Tag: Fortuna-2_1
    #---------------------
    @F21  = qw( F21                    Fortuna-2_1            Fortuna-2_1_p1
                Fortuna-2_1_p2         Fortuna-2_1_p3         Fortuna-2_2
                Fortuna-2_2_p1         Fortuna-2_2_p2         Fortuna-2_3
                Fortuna-2_3_p1         Fortuna-2_4            Fortuna-2_4_p1
                Fortuna-2_4_p2         Fortuna-2_5            Fortuna-2_5_BETA0
                Fortuna-2_5_p1         Fortuna-2_5_p2         Fortuna-2_5_p3
                Fortuna-2_5_p4         Fortuna-2_5_p5         Fortuna-2_5_p6
                Fortuna-2_5_pp2 );
    @D561 = qw( 561                    GEOSadas-5_6_1         GEOSadas-5_6_1_p1
                GEOSadas-5_6_1_p2      GEOSadas-5_6_1_p3      GEOSadas-5_6_1_p4
                GEOSadas-5_6_2         GEOSadas-5_6_2_p1      GEOSadas-5_6_2_p2
                GEOSadas-5_6_2_p3      GEOSadas-5_6_2_p4      GEOSadas-5_6_2_p5
                GEOSadas-5_6_2_p6      GEOSadas-5_7_1         GEOSadas-5_7_1_p1
                GEOSadas-5_7_1_p2      GEOSadas-5_7_2         GEOSadas-5_7_2_p1
                GEOSadas-5_7_2_p2      GEOSadas-5_7_2_p2_m1   GEOSadas-5_7_2_p3
                GEOSadas-5_7_2_p3_m1   GEOSadas-5_7_2_p3_m2   GEOSadas-5_7_2_p4
                GEOSadas-5_7_2_p5      GEOSadas-5_7_2_p5_m1   GEOSadas-5_7_3
                GEOSadas-5_7_3_p1      GEOSadas-5_7_3_p2      GEOSadas-5_7_3_p2 );

    # BCS Tag: Ganymed-1_0
    #---------------------
    @G10 =  qw( G10                    Ganymed-1_0            Ganymed-1_0_BETA
                Ganymed-1_0_BETA1      Ganymed-1_0_BETA2      Ganymed-1_0_BETA3
                Ganymed-1_0_BETA4 );

    @D580 = qw( 580                    GEOSadas-5_8_0         GEOSadas-5_9_0
                GEOSadas-5_9_1 );

    # BCS Tags: Ganymed-1_0_M and Ganymed-1_0_D
    #------------------------------------------
    @G10p = qw( G10p                   Ganymed-1_0_p1         Ganymed-1_0_p2
                Ganymed-1_0_p3         Ganymed-1_0_p4         Ganymed-1_0_p5
                Ganymed-1_0_p6 );

    @D591p= qw( 591p                   GEOSadas-5_9_1_p1      GEOSadas-5_9_1_p2
                GEOSadas-5_9_1_p3      GEOSadas-5_9_1_p4      GEOSadas-5_9_1_p5
                GEOSadas-5_9_1_p6      GEOSadas-5_9_1_p7      GEOSadas-5_9_1_p8
                GEOSadas-5_9_1_p9 );

    # BCS Tags: Ganymed-1_0_M and Ganymed-1_0_D w/ new landice rst
    #------------------------------------------------------------------------
    @G20  = qw( G20                    Ganymed-2_0            Ganymed-2_1
                Ganymed-2_1_p1         Ganymed-2_1_p2         Ganymed-2_1_p3
                Ganymed-2_1_p4         Ganymed-2_1_p5         Ganymed-2_1_p6 );
    @D5A0 = qw( 5A0                    GEOSadas-5_10_0        GEOSadas-5_10_0_p1 );

    # BCS Tags: Ganymed-1_0_Reynolds and Ganymed-1_0_Ostia
    #-----------------------------------------------------
    @G30  = qw( G30                    Ganymed-3_0            Ganymed-3_0_p1 );
    @D5B0 = qw( 5B0                    GEOSadas-5_10_0_p2     GEOSadas-5_11_0 );

    # BCS Tags: Ganymed-4_0_Reynolds, Ganymed-4_0_MERRA-2, and Ganymed-4_0_Ostia
    #---------------------------------------------------------------------------
    @G40  = qw( G40                    Ganymed-4_0 );
    @D512 = qw( 512                    GEOSadas-5_12_2 );

    foreach (@F14)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@F20)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_0" }
    foreach (@F21)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_1" }
    foreach (@G10)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0" }
    foreach (@G10p)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@G20)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@G30)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_Reynolds" }
    foreach (@G40)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-4_0_Reynolds" }

    foreach (@D214)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@D540)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@D561)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_1" }
    foreach (@D580)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0" }
    foreach (@D591p) { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@D5A0)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@D5B0)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_Reynolds" }
    foreach (@D512)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-4_0_Reynolds" }

    @GCMtags = (\@F14, \@F20, \@F21, \@G10, \@G10p, \@G20, \@G30, \@G40);
    @DAStags = (\@D214, \@D540, \@D561, \@D580, \@D591p, \@D5A0, \@D5B0, \@D512);

    # rank of BCS tags
    #-----------------
    %rank = ( "Ganymed-4_0_MERRA-2"  => 14,
              "Ganymed-4_0_Ostia"    => 13,
              "Ganymed-4_0_Reynolds" => 12,
              "Ganymed-1_0_Ostia"    => 11,
              "Ganymed-1_0_Reynolds" => 10,
              "Ganymed-1_0_D"        =>  9,
              "Ganymed-1_0_M"        =>  8,
              "Ganymed-1_0_m2"       =>  7,
              "Ganymed-1_0_m1"       =>  6,
              "Ganymed-1_0"          =>  5,
              "Fortuna-2_1"          =>  4,
              "Fortuna-2_0"          =>  3,
              "Fortuna-1_5"          =>  2,
              "Fortuna-1_4"          =>  1 );
}

#=======================================================================
# name - check_programs
# purpose - check for existence of regrid programs
#=======================================================================
sub check_programs {

    $rs_hinterpX = "$ESMABIN/rs_hinterp.x";
    die "Error. Program not found: $rs_hinterpX;" unless -x $rs_hinterpX;
    rs_hinterp_check();

    $mkdrstdateX = "$ESMABIN/mkdrstdate.x";
    if ($bkgFLG) {
        die "Error. Program not found: $mkdrstdateX;" unless -x $mkdrstdateX;
    }
    $mk_RestartsX = "$ESMABIN/mk_Restarts";
    die "Error. Program not found: $mk_RestartsX;" unless -x $mk_RestartsX;

    if ($surfFLG) {
        $scale_catchX = "$ESMABIN/Scale_Catch";
        die "Error. Program not found: $scale_catchX;" unless -x $scale_catchX;
    }
    if ($upairFLG) {
        $rs_scaleX = "$ESMABIN/rs_scale.x";
        die "Error. Program not found: $rs_scaleX;" unless -x $rs_scaleX;
    }
    if ($c2c) {
        $c2cX = "$ESMABIN/c2c.x";
        die "Error. Program not found: $c2cX;" unless -x $c2cX;
    }
    if ($CS{$grOUT}) {
        $interp_restartsX = "$ESMABIN/interp_restarts.x";
        $getsponsorX = "$ESMABIN/getsponsor.pl";
        $g5modules = "$ESMABIN/g5_modules";
        die "Error. $interp_restartsX not found;" unless -e $interp_restartsX;
        die "Error. $getsponsorX not found;" unless -e $getsponsorX;
        die "Error. $g5modules not found;" unless -e $g5modules;
    }
}

#=======================================================================
# name - rs_hinterp_check
# purpose - check for correct runtime flags in the rs_interp program
#           (earlier versions of the program used different runtime flags)
#=======================================================================
sub rs_hinterp_check {
    my (@runtime_flags, $rtFLG, $wordstring, $errors);

    # here are the rs_hinterp runtime flags the script is expecting
    #--------------------------------------------------------------
    @runtime_flags = qw( -dyn -moist -other -topo_old -topo_new -im -jm );
    chomp($wordstring = `$rs_hinterpX`);

    # the runtime flags have changed; be sure that user has the correct revision
    #---------------------------------------------------------------------------
    $errors = 0;
    foreach $rtFLG (@runtime_flags) {
        unless ($wordstring =~ /\s+$rtFLG\s+/) {
            print_("Error. Cannot find $rtFLG runflag in $rs_hinterpX\n");
            $errors++;
        }
    }

    # die if errors found
    #--------------------
    if ($errors) {
        die "Error. It appears you need another revision of rs_hinterp program;";
    }
}

#=======================================================================
# name - check_rst_files
# purpose - check for existence of restart files
#=======================================================================
sub check_rst_files {
    use Manipulate_time "tick";
    my ($notfound, $type, $file);
    my ($bkg, $cbkg, $num, $bymd, $bhms, $bhr, $name);
    my ($tymd, $thms, $thr, $altdir);

    # check for upper-air restarts
    #-----------------------------
    if ($upairFLG) {
        $notfound = 0;
        foreach $type ( @UPPERAIR_REQ ) {
            $file = rstname($expid, $type, "$rstdir/$rstIN_template");
            unless (-e $file) {
                print_("Cannot find $file\n");
                $notfound++;
            }
        }
        die "Exiting;" if $notfound;
    }

    # check for surface restarts
    #---------------------------
    if ($surfFLG) {
        $notfound = 0;
        foreach $type ( @SURFACE ) {
            $file = rstname($expid, $type, "$rstdir/$rstIN_template");
            unless (-e $file) {
                print_("Cannot find $file\n");
                $notfound++;
            }
        }
        die "Exiting;" if $notfound;
    }

    # check for ana files to copy and rename
    #---------------------------------------
    @anafiles = ();
    if ($bkgFLG) {
        my $SECS_PER_HOUR = 60 * 60;

        # look for bkg and cbkg files
        #----------------------------
        foreach $num ( "03", "06", "09" ) {
            ($bymd, $bhms) = tick($ymd, "${hr}0000", ($num-3)*$SECS_PER_HOUR);
            $bhr = substr($bhms, 0, 2);

            # bkg eta and sfc files
            #----------------------
            foreach $type ( "sfc", "eta" ) {
                $name = "$expid.bkg${num}_${type}_rst";
                $bkg = "$rstdir/$name.${bymd}_${bhr}z.nc4";

                unless (-e $bkg) {
                    $altdir = alt_rstdir($rstdir, $ymd, $bymd);
                    $bkg = "$altdir/$name.${bymd}_${bhr}z.nc4";
                }
                if (-e $bkg) { push @anafiles, $bkg }
                else {
                    warn "WARNING: Cannot find bkg file: $bkg\n";
                    push @warnings, "Did not find bkg file: $bkg";
                }
            }

            # cbkg eta file
            #--------------
            $name = "$expid.cbkg${num}_eta_rst";
            $cbkg = "$rstdir/$name.${bymd}_${bhr}z.nc4";

            unless (-e $cbkg) {
                $altdir = alt_rstdir($rstdir, $ymd, $bymd);
                $cbkg = "$altdir/$name.${bymd}_${bhr}z.nc4";
            }
            if (-e $cbkg) { push @anafiles, $cbkg }
            else {
                warn "WARNING: Cannot find cbkg file: $cbkg\n";
                push @warnings, "Did not find cbkg file: $cbkg";
            }
        }

        # look for satbang and satbias
        #-----------------------------
        foreach $type ( "ana_satbang_rst", "ana_satbias_rst" ) {
            $file = "$rstdir/$expid.$type.${ymd}_${hr}z.txt";
            if (-e $file) { push @anafiles, $file }
            else {
                warn "WARNING: Cannot find $file\n";
                push @warnings, "Did not find $file";
            }
        }

        # look for trak.GDA.rst file
        #---------------------------
        ($tymd, $thms) = tick( $ymd, "${hr}0000", -3*$SECS_PER_HOUR );
        $thr = substr($thms, 0, 2);
        $file = "$rstdir/$expid.trak.GDA.rst.${tymd}${thr}z.txt";
        unless (-e $file) {
            $altdir = alt_rstdir($rstdir, $ymd, $tymd);
            $file = "$altdir/$expid.trak.GDA.rst.${tymd}${thr}z.txt";
        }
        push @anafiles, $file if -e $file;
    }
}

#=======================================================================
# name - create_workdir_and_logfile
# purpose - create work directory and $logfile
#=======================================================================
sub create_workdir_and_logfile {
    my ($prompt, $ans);

    # create temporary work directory
    #--------------------------------
    $workdir = "$outdir/${ymd}_$$";
    unlink_($workdir, $verbose) if -d $workdir;
    mkpath_($workdir, $verbose);
    chdir_($workdir, $verbose);

    # CMD and LOG files
    #------------------
    $logfile = "$outdir/$newid.${ymd}_${hr}z.log";

    if (-e $logfile) {
        $prompt = "Overwrite " .display($logfile);
        $ans = query("\n$prompt (y/n)?", "y");
        unless (lc($ans) eq "y") {
            chdir_($outdir, $verbose);
            unlink_($workdir, $verbose);
            print("Exiting.\n");
            exit;
        }
        unlink_($logfile) if -e $logfile;
    }        
    openLOG($logfile);
    printLOG_("\$ESMABIN/$capture\n");
}

#=======================================================================
# name - getLandIceInput
# purpose - identify and retrieve the landice_internal_rst file from
#           Larry's G10_ICE experiment to match the restart date
#
# Note - The landice_internal_rst format changed between the Ganymed-1_0
#        and the Ganymed-2_0 tags, and the new format cannot be gotten
#        by simply regridding. Larry has created restarts in his G10_ICE
#        ("Ganymed_1_0 Ice") experiment which can be reformatted for the
#        Ganymed-2_0 tag.
#=======================================================================
sub getLandIceInput {
    use File::Basename ("basename");
    use List::Util ("max");
    my ($G10_ICE_dir, $bfile, %tarFiles, $tarfile);
    my ($delta_year, $delta_hour, $delta_day, $max, $diff, $signD, $signT);
    my ($delta_ymd, $delta_hr, $yyyymmdd, $hh0000, $hh);
    my ($landIceRST, @DT, $dateTime, $DT_dflt, $prompt);
    my ($dmls, %dmf_status);

    print_("\nSpecial regrid required for landice_internal_rst\n");

    # get list of tarfiles containing G10_ICE restarts
    #-------------------------------------------------
    $G10_ICE_dir = "/archive/u/ltakacs/GEOS5.0/G10_ICE/restarts";
    foreach (<$G10_ICE_dir/*/restarts*>) {
        $bfile = basename $_;
        $tarFiles{$1} = $_ if $bfile =~ m/restarts.e(\d{8}_\d{2})z.tar/;
    }

    # check user input for landice restart datetime (with -iceDT flag)
    #-----------------------------------------------------------------
    if ($landIceDT) {
        die "Error. landice datetime (-iceDT) needs format <yyyymmdd_hh>;"
            unless $landIceDT =~ m/\d{8}_\d{2}[z|Z]{0,1}/;
        $landIceDT =~ s/[z|Z]$//;  # remove trailing 'z'

        if ($tarFiles{$landIceDT}) {
            print_("User input date/time found: -iceDT $landIceDT\n");
        } else {
            print_("Did not find user input date/time: -iceDT $landIceDT\n\n");
        }

    } else { print_("\n") }

    $landIceDT = "${ymd}_$hr" unless $landIceDT;

  SEARCH: {

      # end search if user-supplied or data datetime is available
      #----------------------------------------------------------
      if ($tarFiles{$landIceDT}) {
          $tarfile = $tarFiles{$landIceDT};
          last SEARCH;
      }

      # look for a close date/time matches
      #-----------------------------------
      print_("Searching for landIce input restart to match [${ymd}_$hr]\n"
             . "search directory: $G10_ICE_dir\n\n");

      $diff = abs($year - 2000);
      $max = maxnum(10, $diff);

      foreach $delta_year ( 0..$max ) {
          foreach $delta_hour ( 0, 3, 6, 9, 12 ) {
              foreach $delta_day ( 0..14) {
                  foreach $signD ( 1, -1 ) {
                      foreach $signT ( 1, -1 ) {

                          $delta_ymd = sprintf "%04s00%02s", $delta_year, $delta_day;
                          $delta_hr  = sprintf "%02s0000", $delta_hour;

                          ($yyyymmdd, $hh0000) = tick( $ymd, "${hr}0000",
                                                       $signD * $delta_ymd,
                                                       $signT * $delta_hr );
                          $hh = substr($hh0000, 0, 2);
                          $dateTime = "${yyyymmdd}_" .$hh;

                          if ($tarFiles{$dateTime}) {
                              push @DT, $dateTime;
                              $DT_dflt = $dateTime unless $DT_dflt;
                          }
                      }
                  }
              }
          }
      }

      # no close matches found
      #-----------------------
      unless (@DT) {
          print_("Cannot find close date/time match for landIce regrid\n".
                   "See search directory for available date/times\n");
          $landIceDT = "";
          $prompt = "Enter date/time (format: yyyymmdd_hh):";
          until ( $tarFiles{$landIceDT} ) { $landIceDT = query("$prompt") }
      }

      # user chooses from close matches if exact match not found
      #---------------------------------------------------------
      unless ( $tarFiles{$landIceDT} ) {

          # get dmf status of each file
          #----------------------------
          foreach (@DT) {
              $dmls = `dmls -l $tarFiles{$_}`;
              $dmf_status{$_} = $1 if $dmls =~ m/(\(\w{3}\))/;
          }

          # list datetime options for landice
          #----------------------------------
          print_("LandIce matches found for [${ymd}_$hr]\n".
                 "=======================================\n");
          foreach (sort @DT) { print_("$_ $dmf_status{$_}\n") };
          print_("------------------------\n");

          # user choice
          #------------
          until ( $tarFiles{$landIceDT} ) {
              $landIceDT = query("Choose one", $DT_dflt);
          }
      }
      $tarfile = $tarFiles{$landIceDT};

    } # end SEARCH

    # print input landice restart date selection information
    #-------------------------------------------------------
    print_("Exact date/time match found.\n") if $landIceDT eq "${ymd}_$hr";
    printlabel("\nregrid date/time: ${ymd}_$hr\n"
               . "landIce input:    $landIceDT", 3);

    # demigrate the tarfile
    #----------------------
    system_("dmget $tarfile");

    # get name of landice restart
    #----------------------------
    chomp($landIceRST = `tar tf $tarfile | grep landice_internal_rst`);

    # store info in %iceIN hash
    #--------------------------
    $iceIN{"date"} = substr($landIceDT, 0, 8);
    $iceIN{"hour"} = substr($landIceDT, 9, 2);
    $iceIN{"tarfile"} = $tarfile;
    $iceIN{"landice_internal_rst"} = $landIceRST;
}

#=======================================================================
# name - set_IN_OUT
# purpose - set values in the %IN and %OUT hashes 
#=======================================================================
sub set_IN_OUT {
    my ($hh, $bcsTAG, $agrid, $ogrid, $bcsdir, $tfile, $val);
    my ($oceanID1, $oceanID2, $atmosID1, $atmosID2, $atmosID3, $atmosID4);
    my ($gridID, $im, $jm, $im4, $jm4, $jm5);

    # tag values
    #-----------
    $IN{"tag"}  = $tagIN;
    $OUT{"tag"} = $tagOUT;

    # BCS tag values
    #---------------
    $IN{"bcsTAG"}  = $bcsTagIN;
    $OUT{"bcsTAG"} = $bcsTagOUT;

    # atmosphere grid values
    #-----------------------
    $IN{"agrid"}  = $grIN;
    $OUT{"agrid"} = $grOUT;

    # experiment IDs
    #---------------
    $IN{"expid"}    = $expid;
    $OUT{"expid"}   = $newid;

    # ocean horizontal grid values
    #-----------------------------
    $IN{"ogrid"}  = $grINocean;
    $OUT{"ogrid"} = $grOUTocean;

    # location of restarts
    #---------------------
    $IN{"rstdir"}  = $rstdir;
    $OUT{"rstdir"} = $outdir;

    # restart name template
    #----------------------
    $IN{"template"}    = $rstIN_template;
    $OUT{"template"}   = $rst_template;

    # add label to final restarts
    #----------------------------
    if ($lblFLG) { $OUT{"label"} = 1 }
    else         { $OUT{"label"} = 0 }

    if ($surfFLG) {

        # check final vegdyn_internal_rst
        #--------------------------------
        $IN{"vegdyn_check"} = 1;

        # two loop processing for catchment
        #----------------------------------
        if ($rank{$bcsTagOUT} > $rank{"Fortuna-2_0"}) { $IN{"rescale"} = 1 }

        # special landice processing
        #---------------------------
        if ($landIceDT) {
            $iceIN{"bcsTAG"}   = "Ganymed-1_0_M";
            $iceIN{"agrid"}    = "C90";
            $iceIN{"expid"}    = "G10_ICE";
            $iceIN{"ogrid"}    = "c";
            $iceIN{"template"} = $rst_template;
            $iceIN{"landIce"}  = 1;
        }
    }

    # grid labels
    #------------
    foreach $hh ( \%IN, \%iceIN, \%OUT ) {
        next unless $$hh{"bcsTAG"};

        $bcsTAG = $$hh{"bcsTAG"};
        $agrid  = $$hh{"agrid"};
        $ogrid  = $$hh{"ogrid"};

        $oceanID1 = "$imo{$ogrid}x$jmo{$ogrid}";
        $oceanID2 = "DE$imo4{$ogrid}xPE$jmo4{$ogrid}";

        if ($CS{$agrid}) {
            $im  = $im{$agrid};
            $jm  = $jm{$agrid};
            $im4 = $im4{$agrid};
            $jm4 = $jm4{$agrid};
            $jm5 = $jm5{$agrid};

            $atmosID1 = "c$im";
            $atmosID2 = "${im}x${jm}";
            $atmosID3 = $atmosID2;
            $atmosID4 = "CF${im4}x6C";
            if ($bcsTAG eq "Ganymed-1_0")
            {      $gridID = "${atmosID4}${jm5}_$oceanID2" }
            else { $gridID = "${atmosID4}_$oceanID2" }
        }
        else {
            $im  = $im{$agrid};
            $jm  = $jm{$agrid};
            $im4 = $im4{$agrid};
            $jm4 = $jm4{$agrid};

            $atmosID1 = "${im}x${jm}";
            $atmosID2 = "${im}x${jm}_DC";
            $atmosID3 = $atmosID1;
            $atmosID4 = "${im4}x${jm4}";
            $gridID = "DC${im4}xPC${jm4}_$oceanID2";
        }

        # atmosphere and ocean horizontal grid labels
        #--------------------------------------------
        $$hh{"atmos" } = $atmosID1;
        $$hh{"atmos2"} = $atmosID2;
        $$hh{"atmos3"} = $atmosID3;
        $$hh{"atmos4"} = $atmosID4;
        $$hh{"ocean" } = $oceanID1;
        $$hh{"gridID"} = $gridID;

        # bcs directory
        #--------------
        if ( $rank{$bcsTAG} <= $rank{"Ganymed-1_0"} ) {
            $bcsdir = "$bcsHEAD/$bcsTAG/$atmosID1";
        }
        else {
            $bcsdir = "$bcsHEAD/$bcsTAG/$gridID";
        }
        if ( $rank{$bcsTAG} >= $rank{"Ganymed-4_0_Reynolds"} ) {
            $bcsdir = "$bcsHEAD/Ganymed-4_0/$bcsTAG/$gridID";
        }
        $$hh{"bcsdir"} = $bcsdir;

        # topological file
        #-----------------
        $$hh{"topo"} = "$bcsdir/topo_DYN_ave_$atmosID2.data";

        # tile file
        #----------
        if ( $rank{$bcsTAG} <= $rank{"Ganymed-1_0"} ) {
            if ( $CS{$agrid} ) { $tfile = "${gridID}_Pfafstetter.til"}
            else               { $tfile = "FV_${atmosID1}_DC_${oceanID1}_DE.til" }
            $$hh{"tile"} = "$bcsdir/$tfile";
        }
        else {
            $$hh{"tile"} = "$bcsdir/${gridID}-Pfafstetter.til";
        }
    }
    if ($dbHash) {
        foreach (sort keys %IN) { print "IN{$_} = $IN{$_}\n"  };
        print "\n";

        foreach (sort keys %iceIN) { print "iceIN{$_} = $iceIN{$_}\n"  };
        print "\n" if %iceIN;

        foreach (sort keys %OUT) { print "OUT{$_} = $OUT{$_}\n" };
        print "\n";
        pause();
    }

    # check values
    #-------------
    foreach $hh ( \%IN, \%iceIN, \%OUT ) {

        # check for existence of BCS directory
        #-------------------------------------
        if ( $val = $$hh{"bcsdir"} ) {
            die "\nError. Directory not found: $val;" unless -d $val;
        }

        # check for existence of topological and tile files
        #--------------------------------------------------
        foreach ( "topo", "tile" ) {
            if ( $val = $$hh{"$_"} ) {
                die "\nError. File not found: $val;" unless -f $val;
            }
        }
    }
}

#=======================================================================
# name - confirm_inputs
# purpose - display the user request;
#           then query user for confirmation
#=======================================================================
sub confirm_inputs {
    my ($ans, $rescale_catch);

    # print program parameters to standard output
    #--------------------------------------------
    printlabel("\nREGRID RESTARTS", 2);
    print_("ESMABIN: $ESMABIN\n");
    print_("ESMATAG: $ESMATAG\n") if $ESMATAG;
    print "logfile: $logfile\n";
    print_("\n");

    # display outputs
    #----------------
    print_("upper-air: ");
    if ($upairFLG) { print_("yes\n") } else { print_("no\n") }

    print_("surface:   ");
    if ($surfFLG)  { print_("yes\n") } else { print_("no\n") }

    print_("\nbkg/satb:  ");
    if ($bkgFLG)   { print_("yes\n") } else { print_("no\n") }

    print_("rst.lcv:   ");
    if ($lcvFLG)   { print_("yes\n") } else { print_("no\n") }

    print_("namelabel: ");
    if ($lblFLG)   { print_("yes\n") } else { print_("no\n") }

    # get rescale flags
    #------------------
    if ($IN{"rescale"}) { $rescale_catch = "yes" }
    else                { $rescale_catch = "no"  }

    # print info
    #-----------
    print_("\n# inputs\n"
           . "#-------\n"
           . ". CVS tag:     $IN{tag}\n");
    if ($IN{"expid"}) { print_(  ". expid:       $IN{expid}\n" ) }
    else              { print_(  ". expid:       (unlabeled)\n" ) }
    print_(  ". date:        $ymd\n"
           . ". hour:        $hr\n"
           . ". atmos grid:  $IN{atmos3} ($IN{agrid})\n"
           . ". ocean grid:  $IN{ocean} ($IN{ogrid})\n"
           . ". BCS tag:     $IN{bcsTAG}\n"
           . ". rstdir:      " .display($rstdir) ."\n");
    print_("\n# landice input\n"
           . "#--------------\n"
           . ". expid:       $iceIN{expid}\n"
           . ". date:        $iceIN{date}\n"
           . ". hour:        $iceIN{hour}\n"
           . ". atmos grid:  $iceIN{atmos3} ($IN{agrid})\n"
           . ". ocean grid:  $iceIN{ocean} ($IN{ogrid})\n"
           . ". BCS tag:     $iceIN{bcsTAG}\n"
           . ". tarfile:     $iceIN{tarfile}\n"
           . ". landice_rst: $iceIN{landice_internal_rst}\n") if $landIceDT;
    print_("\n# outputs\n"
           . "#--------\n"
           . ". CVS tag:     $OUT{tag}\n"
           . ". expid:       $OUT{expid}\n"
           . ". date:        $ymd\n"
           . ". hour:        $hr\n"
           . ". atmos grid:  $OUT{atmos3} ($OUT{agrid})\n"
           . ". ocean grid:  $OUT{ocean} ($OUT{ogrid})\n"
           . ". BCS tag:     $OUT{bcsTAG}\n"
           . ". surflay:     $surflay\n"
           . ". rescale:     $rescale_catch\n"
           . ". outdir:      " .display($outdir) ."\n"
           . ". workdir:     " .display($workdir) ."\n\n");

    confirm("y");
}

#=======================================================================
# name - dmget_input_restarts
# purpose - submit job to dmget the needed restarts
#
# input parameter
# => @restarts : (optional) list of restarts to dmget; if no restarts are
#                specified, a list of all possible restarts will be made
#=======================================================================
sub dmget_input_restarts {
    use Cwd ("realpath");
    my (@list, $type, $file, @restarts, $pid);

    return unless $node eq "nccs";

    @restarts = @_;
    unless (@restarts) {

        # make list of restart types to dmget
        #------------------------------------
        if ($upairFLG) {
            foreach ( @UPPERAIR_REQ, @UPPERAIR_OPT) { push @list, $_ };
        }
        if ($surfFLG) {
            foreach ( @SURFACE ) { push @list, $_ };
        }

        # get actual names of restart files to dmget
        #-------------------------------------------
        @restarts = ();
        foreach $type ( @list ) {
            $file = rstname($expid, $type, "$rstdir/$rstIN_template");
            push @restarts, $file if -e $file and realpath($file) =~ /archive/;
        }
        foreach $file ( @anafiles ) {
            push @restarts, $file if -e $file and realpath($file) =~ /archive/;
        }
    }

    # fork job to dmget restarts located under archive directory
    #-----------------------------------------------------------
    if (@restarts) {
        defined($pid = fork) or die "Cannot fork: $!";
        unless ($pid) { system_("dmget @restarts", $verbose); exit }
    }
}

#=======================================================================
# name - copy_upperair_rsts
# purpose - copy upperair restarts from input to output
#=======================================================================
sub copy_upperair_rsts {
    my ($type, $source, $dest);

    foreach $type ( @UPPERAIR_REQ ) {
        $source = rstname($expid, $type, "$rstdir/$rstIN_template");
        $dest = rstname($newid, $type, "$outdir/$rst_template");
        $dest .= ".$OUT{bcsTAG}.$OUT{gridID}" if $OUT{"label"};
        copy_($source, $dest, $verbose);
    }

    foreach $type ( @UPPERAIR_OPT ) {
        $source = rstname($expid, $type, "$rstdir/$rstIN_template");
        next unless -e $source;
        $dest = rstname($newid, $type, "$outdir/$rst_template");
        $dest .= ".$OUT{bcsTAG}.$OUT{gridID}" if $OUT{"label"};
        copy_($source, $dest, $verbose);
    }
}

#=======================================================================
# name - regrid_upperair_rsts_CS
# purpose - set up and submit PBS job to perform conversion to
#           cubed-sphere upper-air restarts
#=======================================================================
sub regrid_upperair_rsts_CS {
    my ($qsublog, $im, $NODES, $NPE, $QUEUE, $NCPUS, $MPIPROCS);
    my ($grpID, $grpIDflg, $type, $target, $FH);
    my ($DYN, $MOIST, $AGCM, $PCHEM, $GOCART);
    my ($moist, $newrst, $rst, $cmd, $status);

    $im = $im{$grOUT};
    if    ($im eq   "48") { $NODES =  1; $NPE =  12; $QUEUE = "general_small" }
    elsif ($im eq   "90") { $NODES =  1; $NPE =  12; $QUEUE = "general_small" }
    elsif ($im eq  "180") { $NODES =  2; $NPE =  24; $QUEUE = "general"       }
    elsif ($im eq  "360") { $NODES =  4; $NPE =  48; $QUEUE = "general"       }
    elsif ($im eq  "500") { $NODES =  4; $NPE =  48; $QUEUE = "general"       }
    elsif ($im eq  "720") { $NODES = 16; $NPE = 192; $QUEUE = "general"       }
    elsif ($im eq "1000") { $NODES = 32; $NPE = 384; $QUEUE = "general"       }
    elsif ($im eq "1440") { $NODES = 48; $NPE = 576; $QUEUE = "general"       }
    elsif ($im eq "2000") { $NODES = 64; $NPE = 768; $QUEUE = "general"       }
    elsif ($im eq "2880") { $NODES = 96; $NPE = 768; $QUEUE = "general_hi"    }
    else { die "Error; cannot recognize output grid: $grOUT;" }

    $NCPUS = 12;
    if ($im eq "2880") { $MPIPROCS =  8 }
    else               { $MPIPROCS = 12 }

    $regridj = "$workdir/regrid.j";
    $qsublog = "$outdir/$newid.upperair.${ymd}_${hr}z.log.o%j";
    unlink_($regridj) if -e $regridj; 

    chomp($grpID = `$getsponsorX -dflt`);
    if ($grpID eq "") { $grpIDflg = ""                         }
    else              { $grpIDflg = "PBS -W group_list=$grpID" }

    # copy the required restarts to work directory
    #---------------------------------------------
    foreach $type ( @UPPERAIR_REQ ) {
        $target = rstname($expid, $type, "$rstdir/$rstIN_template");
        copy_($target, $workdir, $verbose);
    }
    $DYN   = rstname($expid, "fvcore_internal_rst",  $rstIN_template);
    $MOIST = rstname($expid, "moist_internal_rst",   $rstIN_template);

    # copy the optional restarts to work directory if found
    #------------------------------------------------------
    foreach $type ( @UPPERAIR_OPT ) {
        $target = rstname($expid, $type, "$rstdir/$rstIN_template");
        next unless -e $target;
        copy_($target, $workdir, $verbose);
    }
    $AGCM   = rstname($expid, "agcm_import_rst",     $rstIN_template);
    $PCHEM  = rstname($expid, "pchem_internal_rst",  $rstIN_template);
    $GOCART = rstname($expid, "gocart_internal_rst", $rstIN_template);

    $AGCM   = "" unless -e $AGCM;
    $PCHEM  = "" unless -e $PCHEM;
    $GOCART = "" unless -e $GOCART;

    open REGRIDJ, "> $regridj" or die "Error. Unable to open $regridj; $!";
    $FH = select;

    select REGRIDJ;
    print <<"EOF";
#!/bin/csh -xf
#$grpIDflg
#PBS -l walltime=1:00:00
#PBS -l select=${NODES}:ncpus=${NCPUS}:mpiprocs=${MPIPROCS}
#PBS -N regrid
#PBS -q ${QUEUE}
#PBS -j oe

cd $workdir
source $g5modules
/bin/ln -s $DYN fvcore_internal_restart_in
/bin/ln -s $MOIST moist_internal_restart_in
/bin/ln -s $IN{"topo"} .
/bin/ln -s $OUT{"topo"} .
/bin/ln -s $OUT{"topo"} ./topo_dynave.data

/bin/touch input.nml

if(! $c2c) then
   if( ".$AGCM"   != . ) /bin/ln -s $AGCM   agcm_import_restart_in
   if( ".$PCHEM"  != . ) /bin/ln -s $PCHEM  pchem_internal_restart_in
   if( ".$GOCART" != . ) /bin/ln -s $GOCART gocart_internal_restart_in
endif

# The MERRA fvcore_internal_restarts don't include W or DZ, but we can add them by setting 
# HYDROSTATIC = 0 which means HYDROSTATIC = FALSE
set HYDROSTATIC = 0

set im = $im{$grOUT}

if (\$?I_MPI_ROOT) then
  setenv I_MPI_FABRICS "shm:ofa"
  setenv I_MPI_RDMA_SCALABLE_PROGRESS enable
  setenv I_MPI_USE_DYNAMIC_CONNECTIONS disable
  setenv I_MPI_JOB_STARTUP_TIMEOUT 10000
  setenv DAPL_ACK_RETRY 7
  setenv DAPL_ACK_TIMER 23
  setenv DAPL_RNR_RETRY 7
  setenv DAPL_RNR_TIMER 28
  setenv I_MPI_RDMA_RNDV_WRITE enable

  # intel scaling suggestions
  #--------------------------
  setenv DAPL_CM_ARP_TIMEOUT_MS 8000
  setenv DAPL_CM_ARP_RETRY_COUNT 25
  setenv DAPL_CM_ROUTE_TIMEOUT_MS 20000
  setenv DAPL_CM_ROUTE_RETRY_COUNT 15
  setenv DAPL_MAX_CM_RESPONSE_TIME 20
  setenv DAPL_MAX_CM_RETRIES 15
  #--setenv I_MPI_STATS 4

  $ESMABIN/esma_mpirun -perhost $MPIPROCS -np $NPE $interp_restartsX -999 \$im \$im 72 \$HYDROSTATIC

else

  if (\$?MVAPICH2) then
    setenv MV2_ENABLE_AFFINITY 0
    $ESMABIN/esma_mpirun -np $NPE $interp_restartsX -999 \$im \$im 72 \$HYDROSTATIC
  endif
endif

exit

EOF
close REGRIDJ;

    select $FH;
    chdir_($outdir, $verbose);
    print_("The CS regridding is MPI based; submitting job to PBS\n");
    system_("\nqsub -W block=true -o $qsublog $regridj");
    chdir_($workdir, $verbose);

    if ($c2c) {
        foreach $type (@UPPERAIR_OPT) {
            $rst = rstname($expid, $type, $rstIN_template);
            $newrst = rstnameI($workdir, $type);
            next unless -e $rst;

            $cmd = "$c2cX $rst $newrst $grOUT";
            $status = system_("\n$cmd");
            die "Error. $cmd;" if $status;
        }
    }
}

#=======================================================================
# name - regrid_upperair_rsts_LATLON
# purpose - run rs_hinterp program to create upper-air latlon regridded restarts
#=======================================================================
sub regrid_upperair_rsts_LATLON {
    my ($type, $target, @other, $file);
    my ($fvrst, $moist, $cmd);

    # link the required restarts to work directory
    #---------------------------------------------
    foreach $type ( @UPPERAIR_REQ ) {
        $target = rstname($expid, $type, "$rstdir/$rstIN_template");
        symlink_($target, $workdir, $verbose);
    }
    $fvrst = rstname($expid, "fvcore_internal_rst", $rstIN_template);
    $moist = rstname($expid, "moist_internal_rst", $rstIN_template);

    # copy the optional restarts to work directory if found
    #------------------------------------------------------
    @other = ();
    foreach $type ( @UPPERAIR_OPT ) {
        $file = rstname($expid, $type, $rstIN_template);
        $target = rstname($expid, $type, "$rstdir/$rstIN_template");
        next unless -e $target;
        symlink_($target, $workdir, $verbose);
        push @other, $file;
    }

    # run program
    #------------
    chdir_($workdir, $verbose);
    $cmd = "$rs_hinterpX"
        .  " -dyn $fvrst "
        .  " -moist $moist "
        .  " -other @other "
        .  " -topo_old " .$IN{"topo"}
        .  " -topo_new " .$OUT{"topo"}
        .  " -im $im{$grOUT} -jm $jm{$grOUT}";
    system_($cmd);
}

#=======================================================================
# name - rename_upperair_rsts
# purpose - rename the new upperair restarts
#=======================================================================
sub rename_upperair_rsts {
    my ($label, $tagID, $gridID);
    my ($arrAddr, $vFLG, $type, $newrst, $newname);

    $tagID  = $OUT{"bcsTAG"};
    $gridID = $OUT{"gridID"};

    # rename each new upperair restart
    #---------------------------------
    foreach $arrAddr ( \@UPPERAIR_REQ, \@UPPERAIR_OPT ) {
        $vFLG = $verbose;
        $vFLG = 1 if $arrAddr == \@UPPERAIR_REQ;
        foreach $type (@$arrAddr) { rename_upperair_restart($type, $vFLG) }
    }

    # rename job script, if job was qsub'ed (for cubed-sphere)
    #---------------------------------------------------------
    if (defined($regridj)) {
        $newname = "$outdir/$newid.upperair.${ymd}_${hr}z.$OUT{atmos3}.j";
        move_("\n$regridj", $newname);
    }
}

#=======================================================================
# name - rename_upperair_restart
# purpose - rename specified upperair restart
#
# input parameters
# => $type: upperair restart type
# => $vFLG: verbose flag
#
# return value
# => $newname: name of renamed restart
#=======================================================================
sub rename_upperair_restart {
    my ($type, $vFLG);
    my ($newrst, $newname);

    $type = shift @_;
    $vFLG = shift @_;

    if ($CS{$grOUT}) {
        $newrst = rstnameI($workdir, $type);
    } else {
        $newrst  = rstname($expid, $type, "$workdir/$rstIN_template");
        $newrst .= ".$OUT{atmos4}";
    }
    unless (-e $newrst) {
        print_("\nNOT FOUND(1): $newrst\n") if $vFLG;
        return;
    }
    $newname = rstname($newid, $type, "$outdir/$rst_template");
    $newname .= ".$OUT{bcsTAG}.$OUT{gridID}" if $OUT{"label"};
    move_("\n$newrst", $newname);

    return $newname;
}

#=======================================================================
# sub - set_dry_mass
# purpose - set dry mass of the atmosphere
#=======================================================================
sub set_dry_mass {
    my ($fvrst, $moist, $label);
    my ($sharedir, $AREA_bin, $ans, $cmd, $status);

    printlabel("\nSet Dry Mass of the Atmosphere");

    # check for AREA.bin file needed for dry mass
    #--------------------------------------------
    if ($CS{$grOUT}) {
        $sharedir = "$bcsHEAD/Ganymed-4_0/Shared";
        $AREA_bin = "$sharedir/$OUT{atmos4}.AREA.bin";

        unless (-e $AREA_bin) {
            print_ "Cannot find AREA.bin file: $AREA_bin\n";
            $ans = query("Continue without setting dry mass (y/n)?", "n");
            die "Quitting;" unless $ans eq "y";

            warn "WARNING. Skipping dry mass step.\n";
            return;
        }
    }
        
    # get names of fvcore and moist restarts
    #---------------------------------------
    $fvrst = rstname($newid, "fvcore_internal_rst", "$outdir/$rst_template");
    $moist = rstname($newid, "moist_internal_rst",  "$outdir/$rst_template");

    if ($OUT{"label"}) {
        $label = ".$OUT{bcsTAG}.$OUT{gridID}";
        $fvrst .= $label;
        $moist .= $label;
    }

    # run program to set dry mass
    #----------------------------
    $cmd  = "$rs_scaleX $fvrst $moist";
    $cmd .= " $AREA_bin" if $CS{$grOUT};

    $status = system_($cmd);
    die "Error. Setting dry mass of atmosphere;" if $status;

    # rename outputs
    #---------------
    unlink_("\n$fvrst");
    unlink_($moist);

    move_("\n$fvrst.scaled", $fvrst);
    move_("$moist.scaled", $moist);
}

#=======================================================================
# name - regrid_surface_rsts
# purpose - run mk_Restarts program to create regridded surface restarts
#
# input parameters
# => $hash1Addr: address to hash with input info
# => $hash2Addr: address to hash with output info
#=======================================================================
sub regrid_surface_rsts {
    my ($hash1Addr, $hash2Addr, %H1, %H2);
    my (@SFC, $flags, $InData_dir, $OutData_dir);
    my ($rstdir1, $expid1, $template1, $tile1, $type, $target);
    my ($rstdir2, $expid2, $template2, $tile2, $cmd, $status);
    my ($catchName, $catchIN, $catch, $catch_regrid, $catch_scaled);
    my ($label, $tagID, $gridID);

    # input and output values
    #------------------------
    ($hash1Addr, $hash2Addr) = @_;
    %H1 = %$hash1Addr;
    %H2 = %$hash2Addr;

    # which surface restarts?
    #------------------------
    if ($H1{"landIce"}) { printlabel("\nLandIce Restart")  }
    else                { printlabel("\nSurface Restarts") }

    foreach $type ( @SURFACE ) {
        if ($landIceDT) {
            if ($H1{"landIce"}) {
                push @SFC, $type if $type eq "landice_internal_rst";
            }
            else {
                push @SFC, $type if $type ne "landice_internal_rst";
            }
        }
        else { push @SFC, $type }
    }

    # set flags for $mk_RestartsX
    #----------------------------
    $flags = "";
    if ($landIceDT) {
        if ($H1{"landIce"}) { $flags = "-landice" }
        else                { $flags = "-saltwater -lake -catch" }
    }
    $flags .= " -surflay $surflay";

    # input and output directories
    #-----------------------------
    $InData_dir = "$workdir/InData";
    unlink_($InData_dir, $verbose) if -d $InData_dir;
    mkpath_($InData_dir, $verbose);

    $OutData_dir = "$workdir/OutData";
    unlink_($OutData_dir, $verbose) if -d $OutData_dir;
    mkpath_($OutData_dir, $verbose);

    if ($H1{"landIce"}) {
        chdir_($InData_dir, $verbose);
        system_("tar xf $H1{tarfile} $H1{landice_internal_rst}");
        print_("\n");
    }
    else {

        # link land-surface restarts to InData directory
        #-----------------------------------------------
        $rstdir1   = $H1{"rstdir"};
        $expid1    = $H1{"expid"};
        $template1 = $H1{"template"};

        foreach $type ( @SFC ) {
            $target = rstname($expid1, $type, "$rstdir1/$template1");

            if (-e $target) { symlink_($target, $InData_dir, $verbose) }
            else            { print_("$target not found.\n") }
        }
    }

    # link tile files into the InData and OutData directories
    #--------------------------------------------------------
    $tile1 = $H1{"tile"};
    $tile2 = $H2{"tile"};

    symlink_($tile1, $InData_dir);
    symlink_("\n$tile2", $OutData_dir);

    # run mk_Restarts program
    #------------------------
    chdir_($workdir, $verbose);
    system_("\n$mk_RestartsX $flags");

    # rename restarts
    #----------------
    rename_surface_rsts(\%H1, \%H2, \@SFC);

    #-------------------------------------------
    # second regrid iteration for catchment file
    #-------------------------------------------
    if ($H1{"rescale"}) {

        printlabel("\nCatch Restart: step 2");

        # output catchment from 1st step becomes input for 2nd step
        #----------------------------------------------------------
        $rstdir2   = $H2{"rstdir"};
        $expid2    = $H2{"expid"};
        $template2 = $H2{"template"};

        $label  = $H2{"label"};
        $tagID  = $H2{"bcsTAG"};
        $gridID = $H2{"gridID"};

        $catchName = rstname($expid2, "catch_internal_rst", $template2);
        $catchIN = "$InData_dir/$catchName";

        $catch = "$rstdir2/$catchName";
        $catch .= ".$tagID.$gridID" if $label;

        # new input directory for step 2
        #-------------------------------
        move_($InData_dir, "$InData_dir.step1", $verbose);
        mkpath_("\n$InData_dir", $verbose);
        symlink_($H2{tile}, $InData_dir);
        move_("\n$catch", "$catchIN", $verbose);

        # link clsm directory to output directory
        #----------------------------------------
        symlink_("\n$H2{bcsdir}/clsm", $OutData_dir);

        # run mk_Restarts program for catch only
        #---------------------------------------
        chdir_($workdir, $verbose);

        $flags = "-catch -surflay $surflay";
        @SFC = ();
        push @SFC, "catch_internal_rst";

        system_("\n$mk_RestartsX $flags");
        rename_surface_rsts(\%H2, \%H2, \@SFC);

        #------------------
        # rescale catchment
        #------------------
        printlabel("\nCatch Restart: rescale");

        $catch_regrid = $catch;
        $catch_scaled = "$catch.scaled";

        $cmd = "$scale_catchX $catchIN $catch_regrid $catch_scaled $surflay";
        print_("Scaling the CATCH_INTERNAL_RST file:\n");

        $status = system_($cmd);
        die "Error. Scaling CATCH restart;" if $status;
        unlink($catch);
        move_("\n$catch_scaled", $catch);
    }

    # check vegdyn_internal_rst
    #--------------------------
    check_vegdyn(\%H2, $OutData_dir) if $H1{"vegdyn_check"};
}

#=======================================================================
# name - rename_surface_rsts
# purpose - move and rename new surface restarts from $OutData_dir to $outdir
#
# input parameter
# => $hashAddr1: address of hash with input info
# => $hashAddr2: address of hash with output info
# => $arrAddr:   address of list with surface restart types
#=======================================================================
sub rename_surface_rsts {
    my ($hashAddr1, $hashAddr2, $arrAddr, %H1, %H2, @SFC);
    my ($vFLG, $expid1, $expid2, $template1, $template2);
    my ($label, $tagID, $gridID);
    my ($type, $newrst, $newname);

    $hashAddr1 = shift @_;
    $hashAddr2 = shift @_;
    $arrAddr   = shift @_;
    %H1 = %$hashAddr1;
    %H2 = %$hashAddr2;
    @SFC = @$arrAddr;

    # rename each new surface restart
    #--------------------------------
    $vFLG = $verbose;
    $expid1    = $H1{"expid"};
    $expid2    = $H2{"expid"};
    $template1 = $H1{"template"};
    $template2 = $H2{"template"};

    $label  = $H2{"label"};
    $tagID  = $H2{"bcsTAG"};
    $gridID = $H2{"gridID"};

    foreach $type ( @SFC ) {
        if ($H1{$type}) { $newrst = "$workdir/OutData/$H1{$type}" } # landice!
        else {
            $newrst = rstname($expid1, $type, "$workdir/OutData/$template1");
        }
        unless (-e $newrst) {
            print_("\nNOT FOUND(2): $newrst\n");
            next;
        }
        $newname = rstname($expid2, $type, "$outdir/$template2");
        $newname .= ".$tagID.$gridID" if $label;
        move_("\n$newrst", $newname);
    }
}

#=======================================================================
# name - check_vegdyn
# purpose - compare the vegdyn_internal_rst created during regridding to
#           the one found in the boundary condition (BC) directory.
#=======================================================================
sub check_vegdyn {
    my ($hashAddr, $OutData_dir);
    my (%H, $vegdyn_rst, $vegdyn_regrid, $vegdyn_bcs);
    my ($size, $label, $cmd, $status, $msg);

    $hashAddr    = shift @_;
    $OutData_dir = shift @_;
    %H = %$hashAddr;
    
    $vegdyn_rst = "vegdyn_internal_rst";
    $vegdyn_regrid = "$OutData_dir/$vegdyn_rst";
    $vegdyn_bcs = "$H{bcsdir}/vegdyn_$H{atmos2}.dat";

    $size = -s $vegdyn_regrid;
    $label = "$H{atmos2}.dat.$H{bcsTAG}.$H{gridID}";
    $cmd = "cmp -n $size $vegdyn_regrid $vegdyn_bcs";

    printlabel("\nVegdyn Check");
    print_("Comparing regrid vegdyn_internal_rst with BC file:\n");
    $status = system_($cmd);
    if ($status == 0) {
        printlabel("$vegdyn_rst is consistent with file in bcs directory", 4);
        print_("\n");
    }
    else {
        if ($status == 1) {
            $msg = "WARNING: "
                .  "$vegdyn_rst is NOT consistent with file in bcs directory\n"
                .  "regrid: ". display($vegdyn_regrid) ."\n"
                .  "bcsdir: ". display($vegdyn_bcs);
        }
        elsif ($status == 2) {
            $msg = "WARNING: "
                .  "vegdyn file was not found in bcs directory\n"
                .  "NOT FOUND: ". display($vegdyn_bcs)
        }
        printlabel($msg, 4);
        move_("\n$vegdyn_regrid", "$H{rstdir}/vegdyn_$label");
    }
}

#=======================================================================
# name - maxlength
# purpose - returns length of longest line in a string (presumably a 
#           multi-line string)
#=======================================================================
sub maxlength {
    my ($string, $max, $MAX, $len);

    $string = shift @_;
    $MAX = 150;
    $max = -1;
    {
        $len = index($string, "\n");
        $max = $len if $len > $max;
        last if $len < 0;

        $string = substr($string, ++$len);
        redo;
    }
    $len = length($string);
    $max = $len if $len > $max;
    $max = $MAX if $max > $MAX;

    return $max;
}

#=======================================================================
# name - get_anafiles
# purpose - copy and rename ana bkg, satbang, and satbias files
#=======================================================================
sub get_anafiles {
    use File::Basename;
    my ($newname);
    printlabel("\nAnalysis Files") if @anafiles;
    foreach ( @anafiles ) {
        ( my $newname = basename $_ ) =~ s/$expid/$newid/;
        copy_("\n$_", "$outdir/$newname");
    }
}

#=======================================================================
# name - write_rst_lcv
# purpose - write the rst.lcv date/time restart file
#=======================================================================
sub write_rst_lcv {
    my ($rstlcvOUT, $hhmmss);
    $rstlcvOUT = rstname($newid, "rst.lcv", "$outdir/$rst_template");
    $hhmmss = $hr."0000";
    system_("\n$mkdrstdateX $ymd $hhmmss $rstlcvOUT");
}

#=======================================================================
# name - write_CMD_file
# purpose - write file with the command used to regrid restarts
#=======================================================================
sub write_CMD_file {
    my ($cmdfile, $FH, $csh);

    # update captured command inputs
    #-------------------------------
    $capture =~ s/\s+\-i\b//;
    $capture .= " -ymd $ymd"                  if $capture !~ m/\s+\-ymd\b/;
    $capture .= " -hr $hr"                    if $capture !~ m/\s+\-hr\b/;
    $capture .= " -grout $grOUT"              if $capture !~ m/\s+\-grout\b/
        and                                      $capture !~ m/\s+\-gridout\b/;
    $capture .= " -outdir ". display($outdir) if $capture !~ m/\s+\-outdir\b/;

    if ($merra) {
        $capture .= " -merra"              if $capture !~ m/\s+\-merra\b/;
    }
    else {
        $capture .= " -d $rstdir"          if $capture !~ m/\s+\-d\b/;
        $capture .= " -expid $expid"       if $capture !~ m/\s+\-expid\b/ and $expid;
        $capture .= " -tagin $tagIN"       if $capture !~ m/\s+\-tagin\b/;
        $capture .= " -oceanin $grINocean" if $capture !~ m/\s+\-oceanin\b/;
    }

    if ($interactive) {
        $capture .= " -i" if $capture !~ m/\s+\-i\b/;
        unless ($gcmFLG) {
            if ($capture !~ m/\s+\-bkg\b/ and $capture !~ m/\s+\-nobkg\b/) {
                if ($bkgFLG) { $capture .= " -bkg"   }
                else         { $capture .= " -nobkg" }
            }
            if ($capture !~ m/\s+\-lbl\b/ and $capture !~ m/\s+\-nolbl\b/) {
                if ($lblFLG) { $capture .= " -lbl"   }
                else         { $capture .= " -nolbl" }
            }
            if ($capture !~ m/\s+\-lcv\b/ and $capture !~ m/\s+\-nolcv\b/) {
                if ($lcvFLG) { $capture .= " -lcv"   }
                else         { $capture .= " -nolcv" }
            }
        }
    }

    $capture .= " -tagout $tagOUT" if $capture !~ m/\s+\-tagout\b/;
    $capture .= " -rs $rsFLG"      if $capture !~ m/\s+\-rs\b/;

    $capture .= " -oceanout $grOUTocean" if $capture !~ m/\s+\-oceanout\b/;

    if ($landIceDT) {
        $capture .= " -iceDT $landIceDT" if $capture !~ m/\s+\-iceDT\b/;
    }
    elsif (defined($landIceFLG) and $landIceFLG == 0) {
        $capture .= " -iceDT 0" if $capture !~ m/\s+\-iceDT\b/;
    }

    # write file
    #-----------
    $cmdfile = "$outdir/$newid.${ymd}_${hr}z.CMD";
    print_("\nWriting $cmdfile\n");
    open CMD, "> $cmdfile" or die "Error. Opening $cmdfile; $!";
    $FH = select;
    select CMD;

    chomp($csh = `which csh`);
    print "#!$csh\n"
        . "#" ."="x79 ."\n"
        . "# The following command was used to create "
        . "the $newid.${ymd}_${hr}z restarts\n"
        . "#" ."="x79 ."\n"
        . "set ESMABIN = $ESMABIN\n"
        . "\$ESMABIN/" .$capture ."\n";
    close CMD;
    select $FH;
}

#=======================================================================
# name - cleanup
# purpose - remove the work directory, print warnings, and close LOG
#=======================================================================
sub cleanup {

    # remove work directory
    #----------------------
    unless ($debug) {
        if ($workdir and -e $workdir) {
            chdir_($outdir, $verbose);
            unlink_($workdir, $verbose);
        }
    }

    # print warnings before closing logfile
    #--------------------------------------
    foreach (@warnings) { print_("WARNING: $_\n") }
    closeLOG();
    exit;
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         UTILITY subroutines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#=======================================================================
# name - alt_rstdir
# purpose - take the restart directory for a given date ($rstdir1 and $ymd1), 
#           and use it to determine the restart directory for a different
#           date ($rstdir2 and $ymd2)
#
# input parameters
# => $rstdir1 : original restart directory
# => $ymd1    : original yyyymmdd
# => $ymd2    : new yyyymmdd
# => %replace : hash containing replacement strings where p2 is substituted
#               for p1 in the $rstdir if $replace{p1} = p2
#
# notes:
# 1. This is used to determine the restart directory for the bkg06 and
#    bkg09 files when the restarts fall on the last day of the month.
# 2. If Yyyyy amd Mmm values are not in the original restart directory
#    name then the new restart directory name will not be different,
#    regardless of whether the restarts are from the end of the month
#    or not.
#=======================================================================
sub alt_rstdir {
    my ($rstdir1, $ymd1, $ymd2, %replace);
    my ($year1, $month1, $year2, $month2, $rstdir2);

    # input parameters
    #-----------------
    $rstdir1 = shift @_;
    $ymd1    = shift @_;
    $ymd2    = shift @_;
    %replace = @_;
    
    # extract year and month values
    #------------------------------
    $year1  = substr($ymd1, 0, 4);
    $month1 = substr($ymd1, 4, 2);

    $year2  = substr($ymd2, 0, 4);
    $month2 = substr($ymd2, 4, 2);

    # determine rstdir for ymd2
    #--------------------------
    ($rstdir2 = $rstdir1) =~ s/Y$year1/Y$year2/;
    $rstdir2 =~ s/M$month1/M$month2/;

    # substitute replacement strings
    #-------------------------------
    foreach (keys %replace) { $rstdir2 =~ s/$_/$replace{$_}/ }

    return $rstdir2;
}

#=======================================================================
# name - confirm
# purpose - confirm whether to continue or not; if not then remove
#           work directory and logfile, and then exit
#
# input parameter
# => $dflt: default response to question of whether to continue or not
#=======================================================================
sub confirm {
    my ($dflt, $ans);

    $dflt = shift @_;
    $dflt = "y" unless $dflt;

    print_("===============\n");
    $ans = query("Continue (y/n)?", $dflt);
    print_("\n");

    unless (lc($ans) eq "y") {
        chdir_($outdir, $verbose);
        unlink_($workdir);
        closeLOG();
        unlink_($logfile);
        print("Exiting.\n");
        exit;
    }
}

#=======================================================================
# name - extract_expids
# purpose - extract experiment id labels from a list of restarts
#
# inputs
# => @rstArrAddr: address of array of restart file names
# => $rstType: (optional) type of restart; defaults to "fvcore_internal_rst"
#=======================================================================
sub extract_expids {
    use File::Basename;
    my ($rstArrAddr, $rstType);
    my (@rstArr, $base, %IDfound);

    $rstArrAddr = shift @_;
    @rstArr = @$rstArrAddr;

    $rstType = shift @_;
    $rstType = "fvcore_internal_rst" unless $rstType;

    foreach (@rstArr) {
        $base = basename $_;
        $IDfound{$1}++ if $base =~ m/^(\w+)\.$rstType/;
    }
    return %IDfound;
}

#=======================================================================
# name - filter_datetimes
# purpose - filter out fvcore rst names which do not match $ymd and $hr
#
# input parameter
# => @rstArr: array of fvcore_internal_rst files
#=======================================================================
sub filter_datetimes {
    use File::Basename ("basename");
    my ($rst, @filtered);

    foreach (@_) {
        $rst = basename($_);
        if ( $rst=~m/(\d{8}_\d{2})/ ) { push @filtered, $_ if $1 eq "${ymd}_$hr" }
        else                          { push @filtered, $_ }
    }
    return @filtered;
}

#=======================================================================
# name - maxnum
# purpose - return maximum value from list of numbers
#=======================================================================
sub maxnum {
    my ($maxval);
    $maxval = undef;
    foreach (@_) {
        if (defined($maxval)) { $maxval = $_ if $_ > $maxval }
        else                  { $maxval = $_ }
    }
    return $maxval;
} 

#=======================================================================
# name - pause
# purpose - pause the interactive session until user enters input
#=======================================================================
sub pause {
    unless ($noprompt) {
        print "Hit <cr> to continue ... ";
        my $dummy = <STDIN>;
    }
}

#=======================================================================
# name - printlabel
# purpose - print a label or msg sandwiched between upper and lower lines
#
# input parameters
# => $label: label to print
# => $flag: (optional) =1: ('-') use dash to form lines (default)
#                      =2: ('=') use equal sign
#                      =3: ('*') use asterisk
#                      =4: ('~') use tilde
#=======================================================================
sub printlabel {
    my ($label, $flag);
    my ($char, $len, $line);
    $label = shift @_;
    $flag = shift @_;
    $flag = 1 unless $flag;
    $label = strip_and_print_CRs($label);

    if    ($flag == 2) { $char = '=' }
    elsif ($flag == 3) { $char = '*' }
    elsif ($flag == 4) { $char = '~' }
    else               { $char = '-' }

    $len = maxlength($label);
    $line = ${char}x$len;
    print_("$line\n"
           ."$label\n"
           ."$line\n");
}

#=======================================================================
# name - rstname
# purpose - return name of restart file
#
# input parameters
# => $id: heading for file; can include path and/or expid
# => $type: type of restart (e.g. "fvcore_internal_rst")
# => template: file name template
#
# return value
# => name of restart file
#=======================================================================
sub rstname {
    my ($id, $type, $template, $name);
    ($id, $type, $template) = @_;
    $name = sprintf $template, $id, $type;
    return $name;
}    

#=======================================================================
# name - rstnameI
# purpose - return name of restart file created by $interp_restartX program
#           (used when interpolating to cubed-sphere grid)
#
# input parameters
# => dir: directory location of file
# => type: type of restart (e.g. "fvcore_internal_rst")
#
# return value
# => name of interpolated restart file
#=======================================================================
sub rstnameI {
    my ($type, $dir);
    $dir = shift @_;
    $type = shift @_;
    return "${dir}/${type}_c$im4{$grOUT}_072L";
}    

#=======================================================================
# name - strip_and_print_CRs
# purpose - strip and print leading "\n"'s from a string variable
#
# input parameters
# => $string: string variable
#=======================================================================
sub strip_and_print_CRs {
    my ($string, $cr);
    $string = shift @_;
    $cr = "";
    if ($string =~ s/^(\n*)//) { $cr .= $1 }
    print_($cr);
    return $string;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    my ($FH, $info);

    $FH = select;
    open(INFO, ">", \$info);

    select INFO;
    print <<"EOF";

NAME
    regrid.pl

PURPOSE
    Wrapper script for programs which regrid GCM restarts

SYNOPSIS

    regrid.pl yyyymmdd hr gridID outdir [options]

REQUIRED INPUTS
   -----------------------------
   as ordered runtime parameters
   -----------------------------
   yyyymmdd          8-digit date of the restarts being regridded
   hr                2-digit hour of the restarts being regridded
   grOUT             grid ID of the output restarts (see GRID IDENTIFIERS below)
   outdir            directory location for output restarts

   --------------------
   or as flagged values
   --------------------
   -ymd      yyyymmdd
   -hr       hr
   -grout    grOUT
   -outdir   outdir

REQUIRED OPTION FOR MERRA INPUTS
   -merra             get input restarts from OPS MERRA data archives

REQUIRED OPTIONS FOR NON-MERRA INPUTS
   -d        rstdir   location of input restart files
   -expid    expid    experiment ID of input restart files

INTERACTIVE OPTION
   -i                 prompt for inputs that are not supplied
                      (this is the default if no inputs are supplied)

OTHER OPTIONS
   -oceanin  oceanIN  ocean horizontal grid of inputs
                      =c : 1-deg (360x180); e.g. Reynolds
                      =e : 1/4-deg (1440x720); e.g. MERRA-2
                      =f : 1/8-deg (2880x1440); e.g. OSTIA
                      defaults to \'c\'
   -oceanout oceanOUT ocean horizontal grid of outputs (see -oceanIN)
   -esmabin  ESMABIN  location of build\'s scripts and programs; defaults to location
                      of regrid.pl script
   -iceDT     dtime   datetime for alternate landice rst input if regridding to
                      \'Ganymed-2_0\' from earlier tag; dtime should have the
                      following format: -iceDT yyyymmdd_hh
                      if dtime is not provided or if no restarts can be found to
                      match dtime, then script will prompt user from list of
                      available datetimes.
                      if dtime eq \'0\', then alternate landice rst will not be used
   -newid    newid    label to replace expid in output restart names;
                      defaults to \'n-expid\' where n is OUTPUT atmosphere grid ID
   -tagin    tagIN    GCM or DAS tag associated with inputs (see TAGS below);
                      defaults to $former_tag
   -tagout   tagOUT   GCM or DAS tag associated with outputs (see TAGS below);
                      defaults to $current_tag
   -rs       flag     flag indicating which restarts to regrid
                      =1 for upper-air restarts only
                      =2 for land-surface restarts only
                      =3 for both upper-air and land-surface restarts (default)

   -[no]bkg           copy and rename input bkg + satbang/bias files
   -[no]lbl           label final restarts with \'tagID.gridID\' extension
   -[no]lcv           create rst.lcv file for final restarts
   -gcm               gcm mode; equivalent to -nobkg, -lbl, and -nolcv flags

   -np                no prompt; take defaults for all prompts
   -db                (debug mode) Do not clean work directory after running programs;
   -dbh               (debug hash) Show contents of hashes: \%IN and \%OUT
   -v                 verbose mode
   -h[elp]            print usage message

GRID IDENTIFIERS

   Atmosphere Horizontal Grids
   ===========================
   latlon grids         im        jm
   ------------         --        --
   a = 4x5              72        46
   b = 2x2.5           144        91
   c = 1x1.25          288       181
   D = 0.5x0.666       540       361
   d = 0.5x0.625       572       361
   E = 0.25x0.3330    1080       721
   e = 0.25x0.3125    1152       721

   cubed-sphere grids
   ------------------
   Cn, where n = {48, 90, 180, 360, 500, 720, 1000, 1440, 2000, 2880}

   Ocean Horizontal Grids
   ======================
   c = 1-deg   (360x180);   e.g. Reynolds
   e = 1/4-deg (1440x720) ; e.g. MERRA-2
   f = 1/8-deg (2880x1440); e.g. OSTIA

TAGS
   Use GCM or DAS tag names with -tagin and -tagout flags

      Sample GCM tags
      ---------------
      F14  : $F14[1]  .........  $F14[-1]
      F21  : $F21[1]  .........  $F21[-1]
      G10  : $G10[1]  .........  $G10[-1]
      G10p : $G10p[1]  ......  $G10p[-1]
      G20  : $G20[1]  .........  $G20[-1]
      G30  : $G30[1]  .........  $G30[-1]
      G40  : $G40[1]  .........  $G40[-1]

      Sample DAS tags
      ---------------
      214  : $D214[1]  .......  $D214[-1]
      540  : $D540[1]  ......  $D540[-1]
      561  : $D561[1]  ......  $D561[-1]
      580  : $D580[1]  ......  $D580[-1]
      591p : $D591p[1]  ...  $D591p[-1]
      5A0  : $D5A0[1]  .....  $D5A0[-1]
      5B0  : $D5B0[1]  .....  $D5B0[-1]

AUTHOR
   Joe Stassi, SAIC (joe.stassi\@nasa.gov)

EOF
close INFO;

    select $FH;
    system("echo \"$info\" | more");
    exit;
}
