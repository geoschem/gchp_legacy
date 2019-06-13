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
use File::Basename qw(basename dirname);
use FindBin qw($Bin);
use lib "$Bin";

use WriteLog qw(openLOG closeLOG display query setprompt);
use WriteLog qw(chdir_ copy_ mkpath_ move_ print_ printLOG_ realpath_);
use WriteLog qw(symlink_ system_ unlink_ );

# global variables
#-----------------
my ($bcsHEAD, $bcsHEAD_ops, $bcsTagIN, $bcsTagOUT, $bkgFLG, $bkg_regrid_FLG);
my ($capture, $debug, $dbHash, $dyn2dynX, $c2cX, $drymassFLG);
my ($ESMABIN, $ESMATAG, $expid, $gcmFLG, $g5modules, $getinput, $getsponsorX);
my ($grIN, $grINocean, $grINocean_, $grOUT, $grOUTocean, $grpID, $grouplist);
my ($hr, $interactive, $interp_restartsX, $landIceDT, $lblFLG, $lcvFLG);
my ($levsIN, $levsOUT, $logfile, $merra, $mk_catch, $mk_catchcn, $mk_route);
my ($mk_RestartsX, $mkdrstdateX, $month, $newid, $node, $noprompt, $outdir);
my ($regridj, $qos, $rs_hinterpX, $rs_scaleX, $rsFLG, $rstdir, $rst_tarfile);
my ($rst_template, $rst_templateB, $rstIN_template, $rstIN_templateB);
my ($slurmjob, $scale_catchX, $scale_catchcnX, $surfFLG, $surflay, $surflayIN);
my ($tagIN, $tagOUT, $upairFLG, $verbose, $workdir, $year, $ymd, $zoom, $zoom_);
my (%IN, %iceIN, %OUT, %CS, %CSo, %atmLevs, %input_restarts, %hgrd);
my (%im, %im4, %imo, %imo4, %jm, %jm4, %jm5, %jmo, %jmo4);
my (%SURFACE, %UPPERAIR_OPT, %UPPERAIR_REQ, @anafiles, @warnings);

# global tag variables
#---------------------
my $former_tag  = "Ganymed-4_0";       # default for input restarts
my $current_tag = "Ganymed-4_0";       # default for output restarts

my (@GCMtags, @DAStags);
my (@F14, @F20, @F21, @G10, @G10p, @G20, @G30, @G40, @INL, @ICA);
my (@D214, @D540, @D561, @D580, @D591p, @D5A0, @D5B0, @D512, @D517);
my (%bcsTAG, %rank, %landIceVER, $landIceFLG);
my ($rank_1_catchcn, $rank_1_route, $rank_saltwater_split);

# atmosphere lat/lon grids
#-------------------------
$im{"a"} =   "72"; $jm{"a"} =  "46";
$im{"b"} =  "144"; $jm{"b"} =  "91";
$im{"c"} =  "288"; $jm{"c"} = "181";
$im{"D"} =  "540"; $jm{"D"} = "361";
$im{"d"} =  "576"; $jm{"d"} = "361";
$im{"E"} = "1080"; $jm{"E"} = "721";
$im{"e"} = "1152"; $jm{"e"} = "721";

# atmosphere levels
#------------------
$atmLevs{"72"}  = "072";
$atmLevs{"132"} = "132";
$atmLevs{"137"} = "137";
$atmLevs{"144"} = "144";

# ocean grids
#------------
$imo{"c"} =  "360"; $jmo{"c"} = "180";     # Reynolds
$imo{"e"} = "1440"; $jmo{"e"} = "720";     # MERRA-2
$imo{"f"} = "2880"; $jmo{"f"} = "1440";    # OSTIA
$imo{"CS"} = 1;                            # OSTIA cubed-sphere

foreach (qw/ 90 180 360 720 /) {
    $CSo{"C$_"} = $_;
    $imo{"C$_"} = $_;
    $jmo{"C$_"} = 6*$_;
}

# atmosphere cubed-sphere grids
#------------------------------
foreach (qw/ 12 24 48 90 180 360 500 720 1000 1440 2000 2880 5760 /) {
    $CS{"C$_"} = $_;
    $im{"C$_"} = $_;
    $jm{"C$_"} = 6*$_;
}

foreach (keys %im)  { $im4{$_}  = sprintf "%04i", $im{$_}  }
foreach (keys %jm)  { $jm4{$_}  = sprintf "%04i", $jm{$_}  }
foreach (keys %jm)  { $jm5{$_}  = sprintf "%05i", $jm{$_}  }
foreach (keys %imo) { $imo4{$_} = sprintf "%04i", $imo{$_} }
foreach (keys %jmo) { $jmo4{$_} = sprintf "%04i", $jmo{$_} }

# ana bkg_eta grid mapping from input atmosphere grid
#----------------------------------------------------
%hgrd = ( "a" => "a",
          "b" => "b",
          "c" => "c",
          "d" => "d",
          "e" => "e",
          "C12" => "a",
          "C24" => "a",
          "C48" => "b",
          "C90" => "c",
          "C180" => "d",
          "C360" => "d",
          "C500" => "d",
          "C720"  => "e",
          "C1000" => "e",
          "C1440" => "e",
          "C2000" => "e",
          "C2880" => "e",
          "C5760" => "e" );

# restart lists
#--------------
%UPPERAIR_REQ = ("fvcore_internal_rst"       => 1,
                 "moist_internal_rst"        => 1);

%UPPERAIR_OPT = ("agcm_import_rst"           => 1,
                 "agcm_internal_rst"         => 1,
                 "carma_internal_rst"        => 1,
                 "geosachem_internal_rst"    => 1,
                 "geoschemchem_internal_rst" => 1,
                 "gmichem_internal_rst"      => 1,
                 "gocart_internal_rst"       => 1,
                 "mam_internal_rst"          => 1,
                 "matrix_internal_rst"       => 1,
                 "pchem_internal_rst"        => 1,
                 "stratchem_internal_rst"    => 1,
                 "tr_internal_rst"           => 1);

%SURFACE      = ("catch_internal_rst"        => 1,
                 "catchcn_internal_rst"      => 1,
                 "lake_internal_rst"         => 1,
                 "landice_internal_rst"      => 1,
                 "openwater_internal_rst"    => 1,
                 "route_internal_rst"        => 1,
                 "saltwater_internal_rst"    => 1,
                 "seaicethermo_internal_rst" => 1);

#=======================================================================
# main program
#=======================================================================
{
    init();

    check_inputs();
    check_programs();
    check_rst_files();
    create_logfile();
    
    getLandIceInput() if $landIceFLG;
    set_IN_OUT();
    confirm_inputs();
    write_CMD_file();

    dmget_input_restarts();

    if ($upairFLG) {
        printlabel("\nUpperair Restarts");

        unless (copy_upperair_rsts()) {
            if ($CS{$grOUT}) { regrid_upperair_rsts_CS() }
            else             { regrid_upperair_rsts_LATLON();
                               set_dry_mass() if $drymassFLG }
            rename_upperair_rsts();
        }
    }
    if ($surfFLG) {
        regrid_surface_rsts(\%IN, \%OUT);
        regrid_surface_rsts(\%iceIN, \%OUT) if $landIceDT;
    }
    get_anafiles()  if $bkgFLG;
    write_rst_lcv() if $lcvFLG;
    cleanup();
}

#=======================================================================
# name - init
# purpose - get runtime parameters, check environment, initialize global
#           variables
#=======================================================================
sub init {
    use Getopt::Long;
    my ($help, $prompt, $merra1, $merra2, $ESMAETC);
    my ($bcsALT, $bcsHEAD_lt);

    $| = 1;    # flush STDOUT buffer

    # capture command input
    #----------------------
    $interactive = 1 unless @ARGV;
    $capture = basename($0); foreach (@ARGV) { $capture .= " $_" }

    # identify platform
    #------------------
    chomp($node = `uname -n`);
    if ($node =~ /^borg/ or $node =~ /^dirac/ or $node =~ /^warp/ or
        $node =~ /^dali/ or $node =~ /^discover/) { $node = "nccs" }
    die "Error. This script runs on NCCS machines only;\n" unless $node eq "nccs";

    $slurmjob = 1 if defined($ENV{"SLURM_JOBID"});

    # initialize tag arrays and hashes
    #---------------------------------
    init_tag_arrays_and_hashes();

    # runtime options
    #----------------
    GetOptions("ymd=i"           => \$ymd,
               "hr=i"            => \$hr,
               "grout|gridout=s" => \$grOUT,
               "outdir=s"        => \$outdir,
               "merra1"          => \$merra1,
               "merra|merra2"    => \$merra2,
               "d=s"             => \$rstdir,
               "expid=s"         => \$expid,
               "i"               => \$interactive,
               "np|noprompt"     => \$noprompt,
               "levsout=s"       => \$levsOUT,
               "oceanin=s"       => \$grINocean,
               "oceanout=s"      => \$grOUTocean,
               "esmabin=s"       => \$ESMABIN,
               "iceDT=s"         => \$landIceDT,
               "newid=s"         => \$newid,
               "tagin=s"         => \$tagIN,
               "tagout=s"        => \$tagOUT,
               "rs=i"            => \$rsFLG,
               "catchcn"         => \$mk_catchcn,
               "route"           => \$mk_route,
               "bkg!"            => \$bkgFLG,
               "lbl!"            => \$lblFLG,
               "lcv!"            => \$lcvFLG,
               "gcm"             => \$gcmFLG,
               "grpid=s"         => \$grpID,
               "qos=s"           => \$qos,
               "altbcs:s{,1}"    => \$bcsALT,
               "zoom=i"          => \$zoom,
               "db|debug|nc"     => \$debug,
               "dbh"             => \$dbHash,
               "v"               => \$verbose,
               "h|help"          => \$help);

    usage() if $help;
    setprompt(0) if $noprompt;

    $merra = 1 if $merra1;
    $merra = 2 if $merra2;
    $merra = 0 unless $merra;

    # check for input parameters; runtime options take precedence
    #------------------------------------------------------------
    my ($YMD, $HR, $GROUT, $OUTDIR) = @ARGV if scalar(@ARGV) == 4;

    $ymd    = $YMD    unless defined($ymd);
    $hr     = $HR     unless defined($hr);
    $grOUT  = $grOUT  unless defined($grOUT);
    $outdir = $OUTDIR unless defined($outdir);

    # defaults
    #---------
    $dbHash  = 1 if $debug;
    $verbose = 0 unless $verbose;
    $qos = 0 unless $qos;

    # save for CMD file if explicit user input
    #-----------------------------------------
    $zoom_ = $zoom if $zoom;

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

    # set the environment
    #--------------------
    if (-e "$Bin/g5_modules_perl_wrapper") { do "$Bin/g5_modules_perl_wrapper" }

    # GCM BCS head directory location
    #--------------------------------
    $bcsHEAD_lt = "/discover/nobackup/ltakacs/bcs";
    $bcsHEAD_ops = "/discover/nobackup/"
        .           "projects/gmao/share/gmao_ops/fvInput/g5gcm/bcs";

    if (defined($bcsALT)) {
        if ($bcsALT) { $bcsHEAD = $bcsALT }
        else         { $bcsHEAD = $bcsHEAD_lt }
    } else           { $bcsHEAD = $bcsHEAD_ops }
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
    @G40  = qw( G40                    Ganymed-4_0            Ganymed-4_0_p1
                Ganymed-4_1            Heracles-1_0           Heracles-1_1
                Heracles-2_0           Heracles-2_1           Heracles-3_0
                Heracles-4_0           Heracles-5_4_p3 );
    @D512 = qw( 512                    GEOSadas-5_12_2        GEOSadas-5_12_4
                GEOSadas-5_12_4_p1     GEOSadas-5_12_4_p2     GEOSadas-5_12_4_p3
                GEOSadas-5_12_5        GEOSadas-5_13_0_p1     GEOSadas-5_13_0_p2
                GEOSadas-5_13_1        GEOSadas-5_16_5 );

    # BCS Tags: Icarus-NL (New Land Parameters)
    #---------------------------------------------------------------------------
    @INL  = qw( INL Icarus-NL );

    # BCS Tags: Icarus (New Land Parameters, New Topography)
    #---------------------------------------------------------------------------
    @ICA  = qw( ICA                    Icarus                 Jason );
    @D517 = qw( GEOSadas-5_17_0        GEOSadas-5_17_1        GEOSadas-5_18_0
                GEOSadas-5_18_1        GEOSadas-5_18_2        GEOSadas-5_18_3
                GEOSadas-5_18_3_p1     GEOSadas-5_19_0        GEOSadas-5_20_0
                GEOSadas-5_20_0_p1 );

    foreach (@F14)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@F20)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_0" }
    foreach (@F21)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_1" }
    foreach (@G10)   { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0" }
    foreach (@G10p)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@G20)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@G30)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_Reynolds" }
    foreach (@G40)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-4_0_Reynolds" }
    foreach (@INL)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Icarus-NLv2_Reynolds" }
    foreach (@ICA)   { $landIceVER{$_} = 2; $bcsTAG{$_} = "Icarus_Reynolds" }

    foreach (@D214)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@D540)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-1_4" }
    foreach (@D561)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Fortuna-2_1" }
    foreach (@D580)  { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0" }
    foreach (@D591p) { $landIceVER{$_} = 1; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@D5A0)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_M" }
    foreach (@D5B0)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-1_0_Reynolds" }
    foreach (@D512)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Ganymed-4_0_Reynolds" }
    foreach (@D517)  { $landIceVER{$_} = 2; $bcsTAG{$_} = "Icarus_Reynolds" }

    @GCMtags = (\@F14, \@F20, \@F21, \@G10, \@G10p, \@G20, \@G30, \@G40, \@INL, \@ICA);
    @DAStags = (\@D214, \@D540, \@D561, \@D580, \@D591p, \@D5A0, \@D5B0, \@D512);

    # rank of BCS tags
    #-----------------
    %rank = ( "Icarus_Ostia"         => 20,
              "Icarus_MERRA-2"       => 19,
              "Icarus_Reynolds"      => 18,
              "Icarus-NLv2_Ostia"    => 17,
              "Icarus-NLv2_MERRA-2"  => 16,
              "Icarus-NLv2_Reynolds" => 15,
              "Ganymed-4_0_MERRA-2"  => 14,
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

    # minimum tag for catchcn and route options
    #------------------------------------------
    $rank_1_catchcn = $rank{"Icarus-NLv2_Reynolds"};
    $rank_1_route = $rank{"Icarus-NLv2_Reynolds"};
    $rank_saltwater_split = $rank{"Icarus-NL_Reynolds"};
}

#=======================================================================
# name - check_inputs
# purpose - query user for runtime arguments and options if they were
#           not supplied (if interactive mode)
#=======================================================================
sub check_inputs {
    my ($arcdir, $fvrst, $ans, $prompt, $len, $msg, $warnFLG);
    my ($grINocean_dflt, $grOUTocean_, $grOUTocean_dflt, $levsOUTdflt);
    my ($fname, $rstlcvIN, $newid_dflt, $bkg_dflt, $lcv_dflt, $lbl_dflt);
    my ($dflt, $landIceVERin, $landIceVERout);

    # use MERRA input restarts?
    #--------------------------
    unless ($merra or ($rstdir and -d $rstdir)) {
        $prompt = "Do you want script to select MERRA-2 input restarts for you";
        $ans = query("\n$prompt (y/n)?", "n");
        if (lc($ans) eq "y") {
            $merra = 2;
        }
        else {
            $prompt = "Do you want script to select MERRA-1 restarts for you";
            $ans = query("\n$prompt (y/n)?", "n");
            if (lc($ans) eq "y") {
                $merra = 1;
            }
        }
    }

    # location of input restarts
    #---------------------------
    until (($rstdir and -d $rstdir) or ($merra and ! $slurmjob)) {
        if ($slurmjob) {
            print "\n==================================================\n"
                .   "NOTE: You are running on SLURM interactive nodes. \n"
                .   "Inputs from archive directories are not available.\n"
                .   "==================================================\n";
        }
        $rstdir = query("Enter INPUT restart directory:");
        print "\nCannot find restart dir: $rstdir\n" unless -d $rstdir;
    }
    if ($rstdir) {
        $rstdir =~ s/\/*$//;    # remove trailing '/'s
        $rstdir = realpath_($rstdir);
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

    # create $workdir
    #----------------
    $workdir = "$outdir/${ymd}_$$";
    unlink_($workdir, $verbose) if -d $workdir;
    mkpath_($workdir, $verbose);

    # set values for MERRA restarts
    #------------------------------
    if ($merra == 1) {
        $rstdir = "/archive/g_proj5/production/GEOSdas-2_1_4" unless $slurmjob;
        $tagIN  = "Fortuna-1_4";
        $grIN      = "d";
        $grINocean = "c";

        if    ($year < 1979) { die "Error. MERRA data < 1979 not available\n" }
        elsif ($year < 1989) { $expid = "d5_merra_jan79" }
        elsif ($year < 1998) { $expid = "d5_merra_jan89" }
        else                 { $expid = "d5_merra_jan98" }
    }
    if ($merra == 2) {
        $arcdir = "/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4";
        $rstdir = $arcdir unless $slurmjob;
        $tagIN  = "Ganymed-4_0";
        $grIN      = "C180";
        $grINocean = "e";

        if ("$year$month" < 197901) {
            die "Error. MERRA-2 data < 1979 not available\n"
        }
        elsif ("$year$month" < 199201) { $expid = "d5124_m2_jan79" }
        elsif ("$year$month" < 200106) { $expid = "d5124_m2_jan91" }
        elsif ("$year$month" < 201101) { $expid = "d5124_m2_jan00" }
        else                           { $expid = "d5124_m2_jan10" }
    }
    die "\nError. Cannot find restart dir: $rstdir" unless -d $rstdir;
    $rstdir =~ s/\/*$//;    # remove trailing '/'s

    # copy upperair inputs if in archive; otherwise symlink
    #------------------------------------------------------
    if ($rstdir =~ /\/archive\//) { $getinput = \&copyinput }
    else                          { $getinput = \&symlinkinput }

    # get INPUT fvcore_internal_rst
    #------------------------------
    $fvrst = get_fvrst();

    # check atmosphere grids: $grIN and $grOUT
    #-----------------------------------------
    printlabel("\nAtmosphere Grids");
    print_("\nLat/Lon Grids       Cubed-Sphere Grids\n"
        .    "-------------       ------------------\n"
        .    "a = 4 deg           C12     C180     C1000\n"
        .    "b = 2 deg           C24     C360     C1440\n"
        .    "c = 1 deg           C48     C500     C2880\n"
        .    "d = 1/2 deg         C90     C720     C5760\n"
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

    # can always symlink upperair inputs when writing lat/lon outputs
    #----------------------------------------------------------------
    $getinput = \&symlinkinput unless $CS{$grOUT};

    # check output atmospheric levels
    #--------------------------------
    if ($CS{$grOUT}) {
        $levsOUT = -1 unless $levsOUT;
        until ($atmLevs{$levsOUT}) {
            print "\nAtmosphere levels\n"
                .   "-----------------\n";
            foreach (sort { $a <=> $b } keys %atmLevs) { print "$_\n" }

            print "\nINPUT atmosphere levels: $levsIN\n";
            $levsOUTdflt = $levsIN;
            $levsOUT = query("Enter OUTPUT atmosphere levels:", $levsOUTdflt);
        }
    }
    else { $levsOUT = $levsIN }

    # cannot regrid agcm_import_rst to different number of atmosphere levels
    #-----------------------------------------------------------------------
    if ($levsOUT != $levsIN) {
        $UPPERAIR_OPT{"agcm_import_rst"} = 0;
        $msg = "Cannot regrid agcm_import_rst from $levsIN "
            .  "to $levsOUT atmospheric levels.";
        push @warnings, $msg;
    }
        
    # input ocean grid: $grINocean
    #-----------------------------
    # Two variables are used to represent the input ocean grid
    #
    # 1. If input ocean grid is cubed-sphere ("CS"), then
    #   ---------------------
    #   * $grINocean = "CSi"
    #   ---------------------
    #     Indicates that input ocean grid agrees with input atmosphere cubed
    #     grid. This distinguishes input ocean grid from $grOUTocean = "CS"
    #     which indicates that output ocean grid agrees with output atmosphere
    #     cubed grid.
    #   ---------------------
    #   * $grINocean_ = "CS"
    #   ---------------------
    #     This value is used for display purposes and for writing the *.CMD file
    #
    # 2. If input ocean grid it not cubed sphere, then $grINocean_ = $grINocean
    #---------------------------------------------------------------------------
    $grINocean_dflt  = "c";
    $grOUTocean_dflt = "c";

    unless ($grINocean  and $imo{$grINocean} and
            $grOUTocean and $imo{$grOUTocean}) {
        print "\nOcean Grids\n"
            .   "-----------\n"
            .   "c  =  360x180   (Reynolds)\n"
            .   "e  = 1440x720   (MERRA-2)\n"
            .   "f  = 2880x1440  (OSTIA)\n"
            .   "CS = same as atmosphere (OSTIA cubed-sphere)\n\n";
    }
    until ($grINocean and $imo{$grINocean}) {
        $grINocean = query("Enter INPUT ocean grid:", $grINocean_dflt);
    }
    if (($grINocean eq "CS") or ($grINocean eq "CSi")) {
        unless ($CSo{$grIN}) {
            die "Error. Cannot have cubed ocean with atmosphere grid $grIN";
        }
        $grINocean = "CSi";
        $grINocean_ = "CS";
        $imo{"CSi"} = $im{$grIN};
        $jmo{"CSi"} = $jm{$grIN};
        $imo4{"CSi"} = $im4{$grIN};
        $jmo4{"CSi"} = $jm4{$grIN};
    }
    $grINocean_ = $grINocean unless $grINocean_;
    print "INPUT ocean grid: $grINocean_\n";

    # output ocean grid: $grOUTocean
    #-------------------------------
    until ($grOUTocean and $imo{$grOUTocean}) {
        $grOUTocean = query("Enter OUTPUT ocean grid:", $grOUTocean_dflt);
    }
    if ($grOUTocean eq "CS") {
        unless ($CSo{$grOUT}) {
            die "Error. Cannot have cubed ocean with atmosphere grid $grOUT;";
        }
        $imo{"CS"} = $im{$grOUT};
        $jmo{"CS"} = $jm{$grOUT};
        $imo4{"CS"} = $im4{$grOUT};
        $jmo4{"CS"} = $jm4{$grOUT};
    }
    print "OUTPUT ocean grid: $grOUTocean\n";

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
    if ($bcsTagIN) { print_("\nINPUT tag: $bcsTagIN\n\n") }
    else {
        until ($bcsTagIN) {
            print_("\nType 'bcs' to see BCS tags or\n");
            $tagIN = query("Enter GCM or DAS tag for inputs:", $former_tag);
            $bcsTagIN = resolve_bcsTAG($tagIN, $grINocean, "in");
            $tagIN = $bcsTagIN if $tagIN eq "bcs";
        }
    }
    if ($rank{$bcsTagIN} >= 12 ) { $surflayIN = 50 }
    else                         { $surflayIN = 20 }

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

    # for now, always create catch with surface restarts
    #---------------------------------------------------
    if ($surfFLG) { $mk_catch = 1 }
    else          { $mk_catch = 0; $mk_catchcn = 0; $mk_route = 0 }

    # no catchcn prior to $rank_1_catchcn
    #------------------------------------
    $mk_catchcn = 0 if $rank{$bcsTagOUT} < $rank_1_catchcn;
    $mk_route = 0 if $rank{$bcsTagOUT} < $rank_1_route;

    # get zoom value
    #---------------
    if ($surfFLG) {
        unless ($zoom) {
            $zoom = get_zoom($grIN);
            if ($interactive) {
                $prompt = "Zoom value to send to land regridding codes";
                $zoom = query("$prompt [1-8]?", $zoom);
            }
        }
    }

    # cannot regrid upperair cubed to lat/lon
    #----------------------------------------
    if ($CS{$grIN} and ($rsFLG == 1 or $rsFLG == 3)) {
        unless ($CS{$grOUT}) {
            print "\nWARNING. Cannot regrid cubed-sphere upperair to lat/lon\n";
            cleanup() if $rsFLG == 1;

            $ans = query("Continue with surface restarts (y/n)?", "n");
            cleanup() unless lc($ans) eq "y";

            print "WARNING. Upperair regrid turned off\n";
            $rsFLG = 2;
            $upairFLG = 0;
        }
    }
    # check $newid
    #-------------
    if ($grOUTocean eq "CS") { $grOUTocean_ = "-$grOUTocean" }
    else                     { $grOUTocean_ = $grOUTocean }

    if ($expid) { $newid_dflt  = "$grOUT${grOUTocean_}_$expid" }
    else        { $newid_dflt  = "$grOUT$grOUTocean_"          }

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
    $fname = rstname($expid, "rst.lcv", $rstIN_templateB);
    $rstlcvIN = findinput($fname);
    if ($expid and $rstlcvIN) {
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
            . "  grINocean = $grINocean_\n"
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
            . "  grINocean = $grINocean_\n"
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
}

#=======================================================================
# name - get_fvrst
# purpose - find fvcore restart file in INPUT restart directory and
#           use it to determine naming template of restart files
#=======================================================================
sub get_fvrst {
    my (@fvrsts, $pattern, $rstdir1, $fvrst, $fvrstname, $fname, $id);
    my (%IDfound, @idArr, $expid_dflt, $prompt, @temp_arr, $ext);

    # check for fvcore restart
    #-------------------------
    @fvrsts = ();
    $pattern = "*fvcore_internal_rst*";
    @fvrsts = <$rstdir/$pattern>;
    @fvrsts = filter_datetimes(@fvrsts);

    # check for rst tar files
    #------------------------
    unless (@fvrsts) {
        $pattern = "*\.rst\.*\.tar";
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

    # check for rst tar files
    #------------------------
    unless (@fvrsts) {
        $pattern = "*\.rst\.${ymd}_$hr*\.tar";
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
        if ($expid) {
            $pattern = "$expid*fvcore_internal_rst*${ymd}_$hr*";
            @fvrsts = (<$rstdir/$pattern>);
        }
        else {
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
            @fvrsts = ($IDfound{$expid});
            $expid = "" if $expid eq "NONE";
            $expid = "" unless $expid;
        }
    }

    unless (scalar(@fvrsts) == 1) {
        print "Error. Unable to identify unique fvcore_internal_rst file.\n";
        print "expid: $expid\n" if $expid;
        print "date/time: ${ymd}_$hr\n" if $ymd and defined($hr);
        if (@fvrsts) {
            print "===========\n"
                . "files found\n"
                . "===========\n";
        }
        foreach (@fvrsts) { print "=> $_\n" }
        exit;
    }

    # check for experiment ID in fvcore restart name
    #-----------------------------------------------
    $fvrst = $fvrsts[0];
    if ($fvrst =~ m/\.tar$/) {

        $rst_tarfile = $fvrst;
        print_("\nGetting inputs from tarfile: $rst_tarfile\n");
        system_("dmget $rst_tarfile", 0) if $rst_tarfile =~ /\/archive\//;

        chomp($fvrstname = `tar tf $rst_tarfile | grep fvcore_internal_rst`);
        if ($fvrstname !~ m/fvcore_internal_rst/) {
            die "Error. Cannot find fvcore in $rst_tarfile;";
        }            
        $fvrst = "$workdir/$fvrstname";
        $fname = &$getinput($fvrstname, $fvrst);
    } else {
        $fvrstname = basename $fvrst;
    }
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
        @temp_arr = split(/\./, $rstIN_template); pop(@temp_arr);
        $rstIN_templateB = join(".", @temp_arr, "bin");
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
    $ext = ftype($fvrst);
    die "Error. $fvrst not 'bin' or 'nc4';" if $ext ne "bin" and $ext ne "nc4";

    $rst_template = "%s.%s.${ymd}_${hr}z.$ext";
    $rst_templateB = "%s.%s.${ymd}_${hr}z.bin";

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
    ($im, $jm, $levsIN, $nymd, $nhms) = split /\s+/, $fvrstXout;

    foreach (keys %im) { $grID = $_ if $im{$_} eq $im and $jm{$_} eq $jm }
    unless ($grID) { die "Error\n\nError. Cannot determine grID of $fvrst;" }

    return $grID;
}

#=======================================================================
# name - get_zoom
# purpose - get zoom value to send to land regridding codes
#=======================================================================
sub get_zoom {
    my ($gridID, $id, $res, $zoom, %zoomVal);

    %zoomVal = ( "a" => 1, "b" => 1, "c" => 1,
                 "d" => 2, "D" => 2,
                 "e" => 4, "E" => 4 );

    $gridID = shift @_;
    $id = substr($gridID, 0, 1);

    if ($id eq "C") {
        $res = substr($gridID, 1);

        $zoom = sprintf("%.0f", $res/90.);
        $zoom = 1 if $zoom < 1;
        $zoom = 8 if $zoom > 8;
    }
    elsif ($zoomVal{$id}) {
        $zoom = $zoomVal{$id};
    }
    else {
        die "Error; Unknown Grid ID: $gridID;";
    }
    return $zoom;
}

#=======================================================================
# name - resolve_bcsTAG
# purpose - determine the BCS tag based on the GCM or DAS tag and the
#           ocean grid resolution
#
# input parameters
# => $tagID: GCM or DAS tag
# => $oceanGRID: ocean grid resolution
# => $flag: "in" or "out" flag indicating input or output tagID and grid
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
    foreach (qw(UNSTABLE OPS retired)) { $tagIDx =~ s/_$_$// }

    # is user asking to input a BCS tag directly?
    #--------------------------------------------
    if ($tagID eq "bcs") {
        %reverse = reverse %rank;
        if ($flag eq "in") { $dflt = $bcsTAG{$former_tag}  }
        else               { $dflt = $bcsTAG{$current_tag} }

        print "\nBCS tags\n"
            .   "--------\n";
        foreach (sort {$a<=>$b} keys %reverse) {
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
            print "\nError. Higher resolution ocean grid not available in tag.\n"
                . "        tag: $tagID";
            print " ($tagIDx)" if $tagID ne $tagIDx;
            print "\n"
              .   "     bcsTAG: $bcsTAG\n"
                . " ocean grid: $oceanGRID\n\n";
            die "Exiting;";
        }
        if ($oceanGRID eq "CS" or $oceanGRID eq "CSi") {
            if ($rank{$bcsTAG} < $rank{"Ganymed-4_0_Reynolds"} ) {
                print "\nError. Cubed-sphere ocean grid not available in tag.\n"
                    . "        tag: $tagID";
                print " ($tagIDx)" if $tagID ne $tagIDx;
                print "\n"
                    .   "     bcsTAG: $bcsTAG\n"
                    . " ocean grid: $oceanGRID\n\n";
                die "Exiting;";
            }
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
    $bcsTAG =~ s/_Reynolds$/_Ostia/   if $oceanGRID eq "CS";
    $bcsTAG =~ s/_Reynolds$/_Ostia/   if $oceanGRID eq "CSi";
    $bcsTAG =~ s/_M$/_D/              if $oceanGRID eq "f";

    return $bcsTAG;
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

    $dyn2dynX = "$ESMABIN/dyn2dyn.x";
    if ($bkgFLG and ($hgrd{$grIN} ne $hgrd{$grOUT})) {
        $bkg_regrid_FLG = 1;
        die "Error. Program not found: $dyn2dynX;" unless -x $dyn2dynX;
    } else { $bkg_regrid_FLG = 0 }

    if ($surfFLG and $mk_catchcn) {
        $scale_catchcnX = "$ESMABIN/Scale_CatchCN";
        die "Error. Program not found: $scale_catchcnX;" unless -x $scale_catchcnX;
    }
    if ($surfFLG) {
        $scale_catchX = "$ESMABIN/Scale_Catch";
        die "Error. Program not found: $scale_catchX;" unless -x $scale_catchX;
    }
    if ($upairFLG) {
        $rs_scaleX = "$ESMABIN/rs_scale.x";
        die "Error. Program not found: $rs_scaleX;" unless -x $rs_scaleX;
    }
    if ($CS{$grOUT}) {
        $interp_restartsX = "$ESMABIN/interp_restarts.x";
        $getsponsorX = "$ESMABIN/getsponsor.pl";
        $g5modules = "$ESMABIN/g5_modules";
        $c2cX = "$ESMABIN/c2c.x";
        die "Error. $interp_restartsX not found;" unless -e $interp_restartsX;
        die "Error. $getsponsorX not found;" unless -e $getsponsorX;
        die "Error. $g5modules not found;" unless -e $g5modules;
        die "Error. $c2cX not found;" unless -x $c2cX;
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
    @runtime_flags = qw(-dyn -moist -other -topo_old -topo_new -im -jm);
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
    my (%notfound, $fname, $file, $type, $num, $altdir);
    my ($bymd, $bhms, $bhr, $tymd, $thms, $thr);
    my ($bkg, $cbkg, $gaasbkg, $satb, $swFLG, $trak);

    %notfound = ();
    %input_restarts = ();

    # check for upper-air restarts
    #-----------------------------
    if ($upairFLG) {
        foreach $type (keys(%UPPERAIR_REQ), keys(%UPPERAIR_OPT)) {
            $fname = rstname($expid, $type, $rstIN_template);
            $file  = findinput($fname);

            if ($file) { $input_restarts{$type} = $file }
            else       { $notfound{$type} = 1 if $UPPERAIR_REQ{$type} }
        }
        if (%notfound) {
            foreach $type (sort keys %notfound) {
                print_("Error. Required restart not found: $type\n");
            }
            die "Exiting;";
        }
    }

    # check for surface restarts
    #---------------------------
    %notfound = ();
    if ($surfFLG) {
        foreach $type (sort keys %SURFACE) {
            if ($type eq "catchcn_internal_rst") { next unless $mk_catchcn }
            if ($type eq "route_internal_rst")   { next }

            $fname = rstname($expid, $type, $rstIN_template);
            $file  = findinput($fname);
            
            if ($file) { $input_restarts{$type} = $file }
            else       { $notfound{$type} = 1 }
        }
        foreach $type (keys %notfound) {
            $SURFACE{$type} = 0;
            next if $type eq "catchcn_internal_rst";
            $swFLG = 1;
            if ($type eq "saltwater_internal_rst") {
                $swFLG = 0 if $notfound{"openwater_internal_rst"};
                $swFLG = 0 if $notfound{"seaicethermo_internal_rst"}
            }
            elsif ($type eq "openwater_internal_rst") {
                $type = "saltwater_internal_rst";
                $swFLG = 0 if $notfound{$type};
            }
            elsif ($type eq "seaicethermo_internal_rst") {
                $type = "saltwater_internal_rst";
                $swFLG = 0 if $notfound{$type};
            }
            else {
                die("Exiting. $type not found;");
            }
            die("Exiting. $type not found;") unless $swFLG;
        }
    }

    # check for ana files to copy and rename
    #---------------------------------------
    @anafiles = ();
    if ($bkgFLG) {
        my $SECS_PER_HOUR = 60 * 60;

        # look for bkg and cbkg files
        #----------------------------
        foreach $num ("03", "04", "05", "06", "07", "08", "09") {
            ($bymd, $bhms) = tick($ymd, "${hr}0000", ($num-3)*$SECS_PER_HOUR);
            $bhr = substr($bhms, 0, 2);

            # bkg eta and sfc files
            #----------------------
            $altdir = alt_rstdir($rstdir, $ymd, $bymd);
            foreach $type ("sfc", "eta") {
                $fname = "$expid.bkg${num}_${type}_rst.${bymd}_${bhr}z.nc4";
                $bkg = findinput($fname, $altdir);

                if ($bkg) { push @anafiles, $bkg }
                else {
                    warn "WARNING: Cannot find bkg file: $fname\n";
                    push @warnings, "Did not find bkg file: $fname";
                }
            }

            # cbkg eta file
            #--------------
            $fname = "$expid.cbkg${num}_eta_rst.${bymd}_${bhr}z.nc4"; 
            $cbkg = findinput($fname, $altdir);

            if ($cbkg) { push @anafiles, $cbkg }
            else {
                warn "WARNING: Cannot find cbkg file: $fname\n";
                push @warnings, "Did not find cbkg file: $fname";
            }

            # gaas_bkg_sfc files
            #-------------------
            if ($num eq "06" or $num eq "09") {
                $fname = "$expid.gaas_bkg_sfc_rst.${bymd}_${bhr}z.nc4";
                $gaasbkg = findinput($fname, $altdir);

                if ($gaasbkg) { push @anafiles, $gaasbkg }
                else {
                    warn "WARNING: Cannot find gaasbkg file: $fname\n";
                    push @warnings, "Did not find gaasbkg file: $fname";
                }
            }
        }

        # look for satbang and satbias
        #-----------------------------
        foreach $type ("ana_satbang_rst", "ana_satbias_rst", "ana_satbiaspc_rst") {
            $fname = "$expid.$type.${ymd}_${hr}z.txt";
            $satb = findinput($fname);

            if ($satb) { push @anafiles, $satb }
            else {
                warn "WARNING: Cannot find $fname\n";
                push @warnings, "Did not find $fname";
            }
        }

        # look for trak.GDA.rst file
        #---------------------------
        ($tymd, $thms) = tick($ymd, "${hr}0000", -3*$SECS_PER_HOUR);
        $thr = substr($thms, 0, 2);
        $altdir = alt_rstdir($rstdir, $ymd, $tymd);

        $fname = "$expid.trak.GDA.rst.${tymd}${thr}z.txt";
        $trak = findinput($fname, $altdir);

        if ($trak) { push @anafiles, $trak }
    }
}

#=======================================================================
# name - create_logfile
# purpose - self-explanatory
#=======================================================================
sub create_logfile {
    my ($prompt, $ans);

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

      foreach $delta_year (0..$max) {
          foreach $delta_hour (0, 3, 6, 9, 12) {
              foreach $delta_day (0..14) {
                  foreach $signD (1, -1) {
                      foreach $signT (1, -1) {

                          $delta_ymd = sprintf "%04s00%02s", $delta_year, $delta_day;
                          $delta_hr  = sprintf "%02s0000", $delta_hour;

                          ($yyyymmdd, $hh0000) = tick($ymd, "${hr}0000",
                                                      $signD * $delta_ymd,
                                                      $signT * $delta_hr);
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
          until ($tarFiles{$landIceDT}) { $landIceDT = query("$prompt") }
      }

      # user chooses from close matches if exact match not found
      #---------------------------------------------------------
      unless ($tarFiles{$landIceDT}) {

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
          until ($tarFiles{$landIceDT}) {
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
    system_("dmget $tarfile") if realpath_($tarfile) =~ /\/archive\//;

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
    my ($HH, $bcsTAG, $agrid, $ogrid, $hgrid, $bcsdir, $tile, $topo, $val);
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
    $IN{"expid"}  = $expid;
    $OUT{"expid"} = $newid;

    $IN{"levs"}  = $levsIN;
    $OUT{"levs"} = $levsOUT;

    # ocean horizontal grid values
    #-----------------------------
    $IN{"ogrid"}  = $grINocean;
    $OUT{"ogrid"} = $grOUTocean;

    # bkg_eta grid value
    #-------------------
    if ($bkg_regrid_FLG) {
        $hgrid = $hgrd{$grOUT};
        $gridID = "$im{$hgrid}x$jm{$hgrid} ($hgrid)";
        $OUT{"bkg_hgrid"} = $hgrid;
        $OUT{"bkg_regrid"} = $gridID;
    }

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
    foreach $HH (\%IN, \%iceIN, \%OUT) {
        next unless $$HH{"bcsTAG"};

        $bcsTAG = $$HH{"bcsTAG"};
        $agrid  = $$HH{"agrid"};
        $ogrid  = $$HH{"ogrid"};

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

            if ($ogrid eq "CS" or $ogrid eq "CSi") {
                $oceanID1 = $atmosID3;
                $oceanID2 = $atmosID4;
            }

            if ($bcsTAG eq "Ganymed-1_0") {
                   $gridID = "${atmosID4}${jm5}_${oceanID2}" }
            else { $gridID = "${atmosID4}_${oceanID2}" }
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
            $gridID = "DC${im4}xPC${jm4}_${oceanID2}";
        }

        # atmosphere and ocean horizontal grid labels
        #--------------------------------------------
        $$HH{"atmos" } = $atmosID1;
        $$HH{"atmos2"} = $atmosID2;
        $$HH{"atmos3"} = $atmosID3;
        $$HH{"atmos4"} = $atmosID4;
        $$HH{"ocean" } = $oceanID1;
        $$HH{"gridID"} = $gridID;

        # bcs directory
        #--------------
        if ($atmosID2 eq "5760x34560") {
            $bcsdir = "/discover/nobackup/projects/gmao/osse2/stage/BCS_FILES/C5760";
        }
        elsif ($rank{$bcsTAG} >= $rank{"Icarus_Reynolds"}) {
            if ($bcsHEAD eq $bcsHEAD_ops) {
                $bcsdir = "$bcsHEAD/Icarus_Updated/$bcsTAG/$gridID";
            } else {
                $bcsdir = "$bcsHEAD/Icarus/$bcsTAG/$gridID";
            }
        }
        elsif ($rank{$bcsTAG} >= $rank{"Icarus-NLv2_Reynolds"}) {
            $bcsdir = "$bcsHEAD/Icarus-NLv2/$bcsTAG/$gridID";
        }
        elsif ($rank{$bcsTAG} >= $rank{"Ganymed-4_0_Reynolds"}) {
            $bcsdir = "$bcsHEAD/Ganymed-4_0/$bcsTAG/$gridID";
        }
        elsif ($rank{$bcsTAG} <= $rank{"Fortuna-2_0"}) {
            $bcsdir = "$bcsHEAD/$bcsTAG/$atmosID1";
        }
        elsif ($rank{$bcsTAG} <= $rank{"Ganymed-1_0"}) {
            $bcsdir = "$bcsHEAD/$bcsTAG/$atmosID1";
        }
        else {
            $bcsdir = "$bcsHEAD/$bcsTAG/$gridID";
        }
        die "Error; Cannot find bcs directory: $bcsdir;" unless -d $bcsdir;
        $$HH{"bcsdir"} = $bcsdir;

        # topological file
        #-----------------
        $topo = "$bcsdir/topo_DYN_ave_$atmosID2.data";
        die "Error. Cannot find topo file: $topo;" unless -f $topo;
        $$HH{"topo"} = $topo;

        # tile file
        #----------
        if ($rank{$bcsTAG} <= $rank{"Ganymed-1_0"}) {
            if ($CS{$agrid}) { $tile = "${gridID}_Pfafstetter.til"}
            else             { $tile = "FV_${atmosID1}_DC_${oceanID1}_DE.til" }
        }
        else {
            $tile = "${gridID}-Pfafstetter.til";
        }
        $tile = "$bcsdir/$tile";
        die "Error. Cannot find tile file: $tile" unless -f $tile;
        $$HH{"tile"} = "$tile";
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
    if ($surfFLG) { print_("yes\n") } else { print_("no\n") }
    if ($surfFLG) { print_("zoom:      $zoom\n") }

    if ($surfFLG and $rank{$bcsTagOUT} >= $rank_1_route) {
        print_("- route:   ");
        if ($mk_route) { print_("yes\n") } else { print_("no\n") }
    }

    if ($surfFLG and $rank{$bcsTagOUT} >= $rank_1_catchcn) {
        print_("- catch:   ");
        if ($mk_catch) { print_("yes\n") } else { print_("no\n") }

        print_("- catchCN: ");
        if ($mk_catchcn) { print_("yes\n") } else { print_("no\n") }
    }

    print_("\nbkg/satb:  ");
    if ($bkgFLG) { print_("yes\n") } else { print_("no\n") }
    if ($bkgFLG) {
        print_("bkg_eta:   ");
        if ($bkg_regrid_FLG) { print_("regrid\n") } else { print_("copy\n") }
    }
    print_("rst.lcv:   ");
    if ($lcvFLG) { print_("yes\n") } else { print_("no\n") }
    print_("namelabel: ");
    if ($lblFLG) { print_("yes\n") } else { print_("no\n") }

    # get rescale flags
    #------------------
    if ($IN{"rescale"}) { $rescale_catch = "yes" }
    else                { $rescale_catch = "no"  }

    # print info
    #-----------
    print_("\n# inputs\n"
           . "#-------\n"
           . ". CVS tag:      $IN{tag}\n");
    if ($IN{"expid"}) { print_(". expid:        $IN{expid}\n") }
    else              { print_(". expid:        (unlabeled)\n") }
    print_(  ". date:         $ymd\n"
           . ". hour:         $hr\n"
           . ". atmos grid:   $IN{atmos3} ($IN{agrid})\n"
           . ". atmos levs:   $IN{levs}\n"
           . ". ocean grid:   $IN{ocean} ($IN{ogrid})\n"
           . ". BCS tag:      $IN{bcsTAG}\n"
           . ". surflay:      $surflayIN\n"
           . ". rstdir:       " .display($rstdir) ."\n");
    print_(  ". rst tarfile:  " .display($rst_tarfile) ."\n") if $rst_tarfile;
    print_("\n# landice input\n"
           . "#--------------\n"
           . ". expid:        $iceIN{expid}\n"
           . ". date:         $iceIN{date}\n"
           . ". hour:         $iceIN{hour}\n"
           . ". atmos grid:   $iceIN{atmos3} ($IN{agrid})\n"
           . ". ocean grid:   $iceIN{ocean} ($IN{ogrid})\n"
           . ". BCS tag:      $iceIN{bcsTAG}\n"
           . ". tarfile:      $iceIN{tarfile}\n"
           . ". landice_rst:  $iceIN{landice_internal_rst}\n") if $landIceDT;
    print_("\n# outputs\n"
           . "#--------\n"
           . ". CVS tag:      $OUT{tag}\n"
           . ". expid:        $OUT{expid}\n"
           . ". date:         $ymd\n"
           . ". hour:         $hr\n"
           . ". atmos grid:   $OUT{atmos3} ($OUT{agrid})\n"
           . ". atmos levs:   $OUT{levs}\n"
           . ". ocean grid:   $OUT{ocean} ($OUT{ogrid})\n");
    print_(  ". bkg_eta grid: $OUT{bkg_regrid}\n") if $OUT{"bkg_regrid"};
    print_(  ". BCS tag:      $OUT{bcsTAG}\n"
           . ". surflay:      $surflay\n"
           . ". rescale:      $rescale_catch\n"
           . ". outdir:       " .display($outdir) ."\n"
           . ". workdir:      " .display($workdir) ."\n\n");

    confirm("y");
}

#=======================================================================
# name - dmget_input_restarts
# purpose - submit job to dmget the input restarts
#=======================================================================
sub dmget_input_restarts {
    my ($file, @list, @inputs, $pid);

    return unless $node eq "nccs";
    return if $rst_tarfile;

    @list = ();
    @inputs = sort keys %input_restarts;
    foreach $file (@inputs) { push @list, $file if $file =~ /\/archive\// }
    foreach $file (@anafiles) { push @list, $file if $file =~ /\/archive\// }

    if (@list) {
        defined($pid = fork) or die "Cannot fork: $!";
        unless ($pid) { system_("dmget @list", $verbose); exit }
    }
}

#=======================================================================
# name - copy_upperair_rsts
# purpose - copy upperair restarts from input to output if the input and
#           output restarts
#           1. are on the same grid
#           2. have the same number of levels
#           3. use identical topological files
#           
# return value -
# => $copyFLG
#    == 0 if upperair restarts were not copied
#    == 1 if upperair restarts were copied
#=======================================================================
sub copy_upperair_rsts {
    use File::Compare qw(compare);
    my ($topoIN, $topoOUT, $cmpFLG, $copyFLG);
    my ($type, $src, $dest);

    # check for topological files
    #----------------------------
    $topoIN = $IN{"topo"};
    $topoOUT = $OUT{"topo"};
    die "Error. Input topo file not found;"  unless -f $topoIN;
    die "Error. Output topo file not found;" unless -f $topoOUT;

    $cmpFLG = compare($topoIN, $topoOUT); # returns 0 if files are equal
    die "Error comparing topo files;" if $cmpFLG < 0;

    # set flag to indicate whether to copy or not
    #--------------------------------------------
    $copyFLG = 1;
    $copyFLG = 0 if $grIN ne $grOUT;
    $copyFLG = 0 if $levsIN ne $levsOUT;
    $copyFLG = 0 if $cmpFLG;

    # copy restarts
    #--------------
    if ($copyFLG) {
        chdir_($workdir, $verbose);
        foreach $type ((sort keys %UPPERAIR_REQ), (sort keys %UPPERAIR_OPT)) {
            unless ($input_restarts{$type}) {
                next if $UPPERAIR_OPT{$type};
                die "Error. Input $type restart not found;";
            }
            $src = $input_restarts{$type};
            $dest = rstname($newid, $type, "$outdir/$rst_template");
            $dest .= ".$OUT{bcsTAG}.$OUT{gridID}" if $OUT{"label"};
            &$getinput($src, $dest);
        }
    }

    # return flag indicating whether restarts were copied or not
    #-----------------------------------------------------------
    return $copyFLG;
}

#=======================================================================
# name - regrid_upperair_rsts_CS
# purpose - set up and submit PBS job to perform conversion to
#           cubed-sphere upper-air restarts
#=======================================================================
sub regrid_upperair_rsts_CS {
    my ($qsublog, $im, $NPE, $QOS, $QOSline, $MEMPERCPU, $nwrit);
    my ($type, $src, $dest, $input_nml, $FH);
    my ($DYN, $MOIST, $ACHEM, $CCHEM, $CARMA, $AGCM, $AGCMout, $GMICHEM, $GOCART);
    my ($MAM, $MATRIX, $PCHEM, $STRATCHEM, $TR);
    my ($moist, $newrst, $rst, $cmd, $status);

    $im = $im{$grOUT};
    if    ($im eq   "12") { $NPE =  12; $nwrit = 1 }
    elsif ($im eq   "24") { $NPE =  12; $nwrit = 1 }
    elsif ($im eq   "48") { $NPE =  12; $nwrit = 1 }
    elsif ($im eq   "90") { $NPE =  12; $nwrit = 1 }
    elsif ($im eq  "180") { $NPE =  24; $nwrit = 1 }
    elsif ($im eq  "360") { $NPE =  96; $nwrit = 1 }
    elsif ($im eq  "500") { $NPE =  96; $nwrit = 1 }
    elsif ($im eq  "720") { $NPE = 192; $nwrit = 2 }
    elsif ($im eq "1000") { $NPE = 384; $nwrit = 2 }
    elsif ($im eq "1440") { $NPE = 576; $nwrit = 2 }
    elsif ($im eq "2000") { $NPE = 768; $nwrit = 2 }
    elsif ($im eq "2880") { $NPE = 5400; $nwrit = 6 }
    elsif ($im eq "5760") { $NPE = 5400; $nwrit = 6 }
    else { die "Error; cannot recognize output grid: $grOUT;" }

    $QOS = $qos;
    $QOS = 0 if $QOS eq "debug" and $NPE > 532;
    $QOS = "high" if $im > 2000 and ! $QOS;
    
    if ($QOS) { $QOSline = "SBATCH --qos=$QOS" }
    else      { $QOSline = "" }        

    if ($im >= "2880") { $MEMPERCPU = "--mem-per-cpu=4G"}
    else               { $MEMPERCPU = ""                }

    # MAT Workaround C180 -> C720 cannot run on 192
    # ---------------------------------------------
    if (($im{$grIN} eq "180") and ($im eq "720")) { $NPE = 96 }

    # MAT Workaround C180 -> C1440 (found by Saulo)
    # ---------------------------------------------
    if (($im{$grIN} eq "180") and ($im eq "1440")) { $NPE = "600" }

    $regridj = "$workdir/regrid.j";
    $qsublog = "$outdir/$newid.upperair.${ymd}_${hr}z.log.o%j";
    unlink_($regridj) if -e $regridj; 

    # get group ID for batch job
    #---------------------------
    get_grouplist() unless $grouplist;

    # copy restarts to work directory
    #--------------------------------
    foreach $type ((sort keys %UPPERAIR_REQ), (sort keys %UPPERAIR_OPT)) {
        next unless $UPPERAIR_REQ{$type} or $UPPERAIR_OPT{$type};
        unless($input_restarts{$type}) {
            next if $UPPERAIR_OPT{$type};
            die "Error. Input $type restart not found;";
        }
        $src = $input_restarts{$type};
        $dest = "$workdir/" .basename($src);
        next if -e $dest;
        &$getinput($src, $dest);
    }
    $DYN       = rstname($expid, "fvcore_internal_rst",       $rstIN_template);
    $MOIST     = rstname($expid, "moist_internal_rst",        $rstIN_template);

    $ACHEM     = rstname($expid, "geosachem_internal_rst",    $rstIN_template);
    $CCHEM     = rstname($expid, "geoschemchem_internal_rst", $rstIN_template);
    $CARMA     = rstname($expid, "carma_internal_rst",        $rstIN_template);
    $AGCM      = rstname($expid, "agcm_import_rst",           $rstIN_template);
    $GMICHEM   = rstname($expid, "gmichem_internal_rst",      $rstIN_template);
    $GOCART    = rstname($expid, "gocart_internal_rst",       $rstIN_template);
    $MAM       = rstname($expid, "mam_internal_rst",          $rstIN_template);
    $MATRIX    = rstname($expid, "matrix_internal_rst",       $rstIN_template);
    $PCHEM     = rstname($expid, "pchem_internal_rst",        $rstIN_template);
    $STRATCHEM = rstname($expid, "stratchem_internal_rst",    $rstIN_template);
    $TR        = rstname($expid, "tr_internal_rst",           $rstIN_template);

    chdir_($workdir, $verbose);
    $ACHEM     = "" unless -e $ACHEM;
    $CCHEM     = "" unless -e $CCHEM;
    $CARMA     = "" unless -e $CARMA;
    $AGCM      = "" unless -e $AGCM;
    $GMICHEM   = "" unless -e $GMICHEM;
    $GOCART    = "" unless -e $GOCART;
    $MAM       = "" unless -e $MAM;
    $MATRIX    = "" unless -e $MATRIX;
    $PCHEM     = "" unless -e $PCHEM;
    $STRATCHEM = "" unless -e $STRATCHEM;
    $TR        = "" unless -e $TR;

    $AGCMout   = rstnameI(".", "agcm_import_rst");

    # write input.nml file
    #---------------------
    $input_nml = "$workdir/input.nml";
    open NML, "> $input_nml" or die "Error. Unable to open $input_nml; $!";
    $FH = select;
    select NML;
    print <<"EOF";
&fms_nml
      print_memory_usage=.false.
      domains_stack_size = 24000000
/
EOF
;
    close NML;
    select $FH;

    # write regrid.j file
    #--------------------
    open REGRIDJ, "> $regridj" or die "Error. Unable to open $regridj; $!";
    $FH = select;

    select REGRIDJ;
    print <<"EOF";
#!/bin/csh -xf
#$grouplist
#SBATCH --time=1:00:00
#SBATCH --ntasks=${NPE} ${MEMPERCPU}
#SBATCH --job-name=regrid
#SBATCH --constraint=hasw
#$QOSline

unlimit

cd $workdir
source $g5modules
/bin/ln -s $DYN fvcore_internal_restart_in
/bin/ln -s $MOIST moist_internal_restart_in
/bin/ln -s $IN{"topo"} .
/bin/ln -s $OUT{"topo"} .
/bin/ln -s $OUT{"topo"} ./topo_dynave.data

/bin/touch input.nml

if( ".$ACHEM"     != . ) /bin/ln -s $ACHEM  geosachem_internal_restart_in
if( ".$CCHEM"     != . ) /bin/ln -s $CCHEM  geoschemchem_internal_restart_in
if( ".$CARMA"     != . ) /bin/ln -s $CARMA  carma_internal_restart_in
if( ".$AGCM"      != . ) /bin/ln -s $AGCM   agcm_import_restart_in
if( ".$GMICHEM"   != . ) /bin/ln -s $GMICHEM gmichem_internal_restart_in
if( ".$GOCART"    != . ) /bin/ln -s $GOCART gocart_internal_restart_in
if( ".$MAM"       != . ) /bin/ln -s $MAM    mam_internal_restart_in
if( ".$MATRIX"    != . ) /bin/ln -s $MATRIX matrix_internal_restart_in
if( ".$PCHEM"     != . ) /bin/ln -s $PCHEM  pchem_internal_restart_in
if( ".$STRATCHEM" != . ) /bin/ln -s $STRATCHEM stratchem_internal_restart_in
if( ".$TR"        != . ) /bin/ln -s $TR tr_internal_restart_in

# The MERRA fvcore_internal_restarts don't include W or DZ, but we can add them by setting 
# HYDROSTATIC = 0 which means HYDROSTATIC = FALSE
set HYDROSTATIC = 0

set im = $im{$grOUT}

if (\$?I_MPI_ROOT) then

  # intel scaling suggestions
  #--------------------------
  
  setenv I_MPI_DAPL_UD on

  setenv DAPL_UCM_CQ_SIZE 4096
  setenv DAPL_UCM_QP_SIZE 4096

  setenv I_MPI_DAPL_UD_SEND_BUFFER_NUM 4096
  setenv I_MPI_DAPL_UD_RECV_BUFFER_NUM 4096
  setenv I_MPI_DAPL_UD_ACK_SEND_POOL_SIZE 4096
  setenv I_MPI_DAPL_UD_ACK_RECV_POOL_SIZE 4096
  setenv I_MPI_DAPL_UD_RNDV_EP_NUM 2
  setenv I_MPI_DAPL_UD_REQ_EVD_SIZE 2000

  setenv DAPL_UCM_REP_TIME 2000
  setenv DAPL_UCM_RTU_TIME 2000
  setenv DAPL_UCM_RETRY 7
  setenv DAPL_ACK_RETRY 7
  setenv DAPL_ACK_TIMER 20
  setenv DAPL_UCM_RETRY 10
  setenv DAPL_ACK_RETRY 10

else if (\$?MVAPICH2) then
  setenv MV2_ENABLE_AFFINITY 0

endif

set infiles = ()
set outfils = ()
foreach infile ( *_restart_in )
   if ( \$infile == fvcore_internal_restart_in ) continue
   if ( \$infile == moist_internal_restart_in  ) continue

   set infiles = ( \$infiles \$infile )
   set outfil = `echo \$infile | sed "s/restart_in/rst_out/"`
   set outfils = (\$outfils \$outfil)
end

if ( \$\#infiles ) then
    set ioflag = "-input_files \$infiles -output_files \$outfils"
else
    set ioflag = ""
endif

set drymassFLG = $drymassFLG
if (\$drymassFLG) then
    set dmflag = ""
else
    set dmflag = "-scalers F"
endif

$ESMABIN/esma_mpirun -np $NPE $interp_restartsX -im \$im -lm $levsOUT \\
   -do_hydro \$HYDROSTATIC \$ioflag \$dmflag -nwriter $nwrit

exit

EOF
;
    close REGRIDJ;
    select $FH;

    chdir_($outdir, $verbose);
    if ($slurmjob) {
        print_("\nThe CS regridding is MPI based; found SLURM_JOBID; "
               ."run in interactive batch environment\n");

        # Make regridj executable
        # -----------------------
        chmod 0755, $regridj;

        # Remove the .o%j from the qsublog file
        # -------------------------------------
        $qsublog =~ s/.o%j//g;

        system_("$regridj 1>$qsublog 2>&1");
    } else {
        print_("\nThe CS regridding is MPI based; submitting job to PBS\n");
        system_("qsub -W block=true -o $qsublog $regridj");
    }
    chdir_($workdir, $verbose);
}

#=======================================================================
# name - regrid_upperair_rsts_LATLON
# purpose - run rs_hinterp program to create upper-air latlon regridded restarts
#=======================================================================
sub regrid_upperair_rsts_LATLON {
    my ($type, $target, @other);
    my ($fvrst, $moist, $cmd);

    if ($rst_tarfile) {
        die "Error. Cannot regrid lat/lon restarts from restart tarfile;";
    }

    # link the required restarts to work directory
    #---------------------------------------------
    foreach $type (keys %UPPERAIR_REQ) {
        $target = $input_restarts{$type};
        symlinkinput($target, $workdir);
    }
    $fvrst = rstname($expid, "fvcore_internal_rst", $rstIN_template);
    $moist = rstname($expid, "moist_internal_rst", $rstIN_template);

    # copy the optional restarts to work directory if found
    #------------------------------------------------------
    @other = ();
    foreach $type (keys %UPPERAIR_OPT) {
        $target = $input_restarts{$type};
        next unless $target and -e $target;
        symlinkinput($target, $workdir);
        push @other, basename($target);
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
    foreach $type ((sort keys %UPPERAIR_REQ), (sort keys %UPPERAIR_OPT)) {
        $vFLG = $verbose;
        $vFLG = 1 if $UPPERAIR_REQ{$type};
        rename_upperair_restart($type, $vFLG);
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

    # cubed-sphere restarts
    #----------------------
    if ($CS{$grOUT}) {
        if ($type eq "fvcore_internal_rst" or $type eq "moist_internal_rst") {
            $newrst = rstnameI($workdir, $type);
        } else { $newrst = "${type}_out" }
    }

    # lat-lon restarts
    #-----------------
    else {
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
# name - set_dry_mass
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
    my ($rstdir1, $expid1, $template1, $tile1, $type, $src, $alt);
    my ($rstdir2, $expid2, $template2, $tile2, $cmd, $status);
    my ($catchName, $catchIN, $catch, $catch_regrid, $catch_scaled);
    my ($catchcnName, $catchcnIN, $catchcn, $catchcn_regrid, $catchcn_scaled);
    my ($label, $tagID, $gridID);
    my ($mk_catchcn_j, $mk_catchcn_log);

    # input and output values
    #------------------------
    ($hash1Addr, $hash2Addr) = @_;
    %H1 = %$hash1Addr;
    %H2 = %$hash2Addr;

    # which surface restarts?
    #------------------------
    if ($H1{"landIce"}) { printlabel("\nLandIce Restart")  }
    else                { printlabel("\nSurface Restarts") }

    foreach $type (sort keys %SURFACE) {
        next unless $SURFACE{$type};
        if ($type eq "catch_internal_rst")   { next unless $mk_catch }
        if ($type eq "catchcn_internal_rst") { next unless $mk_catchcn }
        if ($type eq "route_internal_rst")   { next unless $mk_route }

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
    if ($landIceDT) {
        if ($H1{"landIce"}) { $flags = "-landice" }
        else                { $flags = "-lake" }
    } else                  { $flags = "-lake -landice" }

    unless ($flags eq "-landice") {
        $flags .= " -saltwater" if $input_restarts{"saltwater_internal_rst"};
        $flags .= " -openwater" if $input_restarts{"openwater_internal_rst"};
        $flags .= " -seaice"    if $input_restarts{"seaicethermo_internal_rst"};

        $flags .= " -route"            if $mk_route;
        $flags .= " -catch"            if $mk_catch;
        $flags .= " -catchcn"          if $mk_catchcn;
        $flags .= " -surflay $surflay" if $mk_catchcn or $mk_catch;
        $flags .= " -grpID $grpID"     if $mk_catchcn and $grpID;
        $flags .= " -rsttime $ymd$hr"  if $mk_catchcn or $mk_route;
        $flags .= " -qos $qos"         if $mk_catchcn and $qos;
    }
    $flags .= " -zoom $zoom" if $zoom;

    # input and output directories
    #-----------------------------
    $InData_dir = "$workdir/InData";
    unlink_($InData_dir, $verbose) if -d $InData_dir;
    mkpath_($InData_dir, $verbose);

    $OutData_dir = "$workdir/OutData";
    unlink_($OutData_dir, $verbose) if -d $OutData_dir;
    mkpath_($OutData_dir, $verbose);

    # extract restart for landice processing
    #---------------------------------------
    if ($H1{"landIce"}) {
        chdir_($InData_dir, $verbose);
        system_("tar xf $H1{tarfile} $H1{landice_internal_rst}");
        print_("\n");
    }

    # copy land-surface restarts to InData directory
    #-----------------------------------------------
    else {
        $rstdir1   = $H1{"rstdir"};
        $expid1    = $H1{"expid"};
        $template1 = $H1{"template"};

        foreach $type (@SFC) {
            next if $type eq "route_internal_rst";
            $src = $input_restarts{$type};

            if ($src) {
                if ($type eq "catchcn_internal_rst") {
                    &$getinput($src, $InData_dir);
                } else {
                    symlinkinput($src, $InData_dir);
                }
            }
            else {
                print_("\n$type not found.\n");

                # use catch for catchcn, if needed
                #---------------------------------
                if ($type eq "catchcn_internal_rst") {
                    $alt = "catch_internal_rst";
                    $src = $input_restarts{$alt};
                    if ($src) {
                        $catchcn = rstname($expid1, $type, "$InData_dir/$template1");
                        &$getinput($src, $catchcn);
                    }
                    else {
                        print_("$alt not found.\n");
                    }
                }
            }
        }
    }

    # link or copy tile files into InData and OutData
    #------------------------------------------------
    $tile1 = $H1{"tile"};
    $tile2 = $H2{"tile"};

    if ($mk_catchcn) {
        copyinput($tile1, $InData_dir);
        copyinput($tile2, $OutData_dir);
    } else {
        symlinkinput($tile1, $InData_dir);
        symlinkinput($tile2, $OutData_dir);
    }

    # link rst directory to OutData
    #------------------------------
    symlinkinput("$H2{bcsdir}/rst", $OutData_dir) if $mk_route;
    symlinkinput("$H2{bcsdir}/til", $OutData_dir) if $mk_route;

    # run mk_Restarts program
    #------------------------
    chdir_($workdir, $verbose);
    system_("\n$mk_RestartsX $flags");

    # split saltwater into openwater and seaicethermo, if necessary
    #--------------------------------------------------------------
    split_saltwater() if $input_restarts{"saltwater_internal_rst"};

    # use mk_catchcn job and log file names from mk_Restarts script
    #--------------------------------------------------------------
    if ($mk_catchcn) {
        $mk_catchcn_j = "mk_catchcn.j";
        $mk_catchcn_log = "mk_catchcn.log";
        move_("$mk_catchcn_j", "$outdir/$mk_catchcn_j.1");
        move_("$mk_catchcn_log", "$outdir/$mk_catchcn_log.1");
    }
    rename_surface_rsts(\%H1, \%H2, \@SFC);

    #----------------------------------------------
    # second regrid iteration for catchment file(s)
    #----------------------------------------------
    if ($H1{"rescale"} and ($mk_catch or $mk_catchcn)) {

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

        $catchcnName = rstname($expid2, "catchcn_internal_rst", $template2);
        $catchcnIN = "$InData_dir/$catchcnName";

        $catchcn = "$rstdir2/$catchcnName";
        $catchcn .= ".$tagID.$gridID" if $label;

        # new input directory for step 2
        #-------------------------------
        move_($InData_dir, "$InData_dir.step1", $verbose);
        mkpath_("\n$InData_dir", $verbose);

        if ($mk_catchcn) { copyinput(   $tile2, $InData_dir) }
        else             { symlinkinput($tile2, $InData_dir) }

        move_("\n$catch", "$catchIN", $verbose)     if $mk_catch;
        move_("\n$catchcn", "$catchcnIN", $verbose) if $mk_catchcn;

        # link clsm directory to OutData
        #-------------------------------
        symlinkinput("$H2{bcsdir}/clsm", $OutData_dir);

        # catch and/or catchcn restart only during this pass
        #---------------------------------------------------
        $flags = "";
        $flags .= " -catch"             if $mk_catch;
        $flags .= " -catchcn"           if $mk_catchcn;
        $flags .= " -surflay $surflay";
        $flags .= " -grpID $grpID"      if $mk_catchcn and $grpID;
        $flags .= " -rsttime $ymd$hr"   if $mk_catchcn;
        $flags .= " -rescale"           if $mk_catchcn;
        $flags .= " -zoom $zoom"        if $zoom;
        #--$flags .= " -walltime 2:00:00"  if $mk_catchcn;
        #--$flags .= " -ntasks 112"        if $mk_catchcn;

        @SFC = ();
        push @SFC, "catch_internal_rst"   if $mk_catch;
        push @SFC, "catchcn_internal_rst" if $mk_catchcn;

        # run mk_Restarts program
        #------------------------
        chdir_($workdir, $verbose);
        system_("\n$mk_RestartsX $flags");
        rename_surface_rsts(\%H2, \%H2, \@SFC);

        if ($mk_catchcn) {
            move_("\n$mk_catchcn_j", "$outdir/$mk_catchcn_j.2");
            move_("$mk_catchcn_log", "$outdir/$mk_catchcn_log.2");
        }

        #------------------
        # rescale catchment
        #------------------
        if ($mk_catch) {
            printlabel("\nCatch Restart: rescale");

            $catch_regrid = $catch;
            $catch_scaled = "$catch.scaled";
            $cmd = "$scale_catchX $catchIN "
                .                "$catch_regrid "
                .                "$catch_scaled "
                .                "$surflay";
            print_("Scaling the CATCH_INTERNAL_RST file:\n");
            $status = system_($cmd);
            die "Error. Scaling CATCH_INTERNAL_RST file;" if $status;

            unlink($catch);
            move_("\n$catch_scaled", $catch);
        }
    }

    # check vegdyn_internal_rst
    #--------------------------
    check_vegdyn(\%H2, $OutData_dir) if $H1{"vegdyn_check"};
}

#=======================================================================
# name - split_saltwater
# purpose - check for conditions necessary to split the saltwater_internal_rst
#           into the openwater_internal_rst and seaicethermo_internal_rst, and
#           do so, if the conditions are met
#=======================================================================
sub split_saltwater {
    my ($expid, $template, $tile, $type, $saltwater, $cmd, $status);

    return if $rank{$bcsTagOUT} < $rank_saltwater_split;

    printlabel("\nSplitting Saltwater",3);
    print_("\n");

    $expid = $IN{"expid"};
    $template = $IN{"template"};
    $tile = $OUT{"tile"};

    $type = "saltwater_internal_rst";
    $saltwater = rstname($expid, $type, "$workdir/OutData/$template");

    $cmd = "$ESMABIN/SaltIntSplitter $tile $saltwater";
    $status = system_($cmd);

    if (!$status) {
       unlink_($saltwater, $verbose);
    }
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
    my ($type, @rstlist, $newrst, $newname);

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

    foreach $type (@SFC) {
        if ($H1{$type}) { $newrst = "$workdir/OutData/$H1{$type}" } # landice!
        else {
            $newrst = rstname($expid1, $type, "$workdir/OutData/$template1");
            unless (-e $newrst) {
                @rstlist = < $workdir/OutData/*$type* >;
                $newrst = $rstlist[0] if @rstlist;
            }
        }
        unless (-e $newrst) {
            print_("\nNOT FOUND(2): $newrst\n");
            next;
        }
        $newname = rstname($expid2, $type, "$outdir/$template2");
        $newname .= ".$tagID.$gridID" if $label;
        move_("\n$newrst", $newname);
    }

    # check for openwater and seaicethermo restarts created by saltwater split
    #-------------------------------------------------------------------------
    foreach $type ("openwater_internal_rst", "seaicethermo_internal_rst") {
        $newrst = "$workdir/OutData/$type";
        if (-e $newrst) {
            $newname = rstname($expid2, $type, "$outdir/$template2");
            $newname .= ".$tagID.$gridID" if $label;
            move_("\n$newrst", $newname);
        }
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
# purpose - copy ana files to the output directory and rename them
#=======================================================================
sub get_anafiles {
    my ($ana, $dbase, $dest, @ana_tar, @bkg_etas);

    printlabel("\nAnalysis Files") if @anafiles;
    foreach $ana (@anafiles) {

        $dbase = basename($ana);
        $dbase =~ s/$expid/$newid/ if $expid ne $newid;
        $dest = "$outdir/$dbase";
        copyinput($ana, $dest);
        push @bkg_etas, $dest if $dbase =~ m/\bbkg\d{2}_eta_rst\b/;

        # satbias file edit
        #------------------
        if ($dbase =~ m/satbias/) {
            if ($rank{$bcsTagOUT} >= $rank{"Ganymed-4_0_Reynolds"}) {
                satbias_edit($dest);
            }
        }
    }

    # copy other ana files found in $rst_tarfile
    #-------------------------------------------
    if ($rst_tarfile) {
        foreach (qw(bkg ana_satb)) {
            @ana_tar = (`tar tf $rst_tarfile | grep $_`);
            foreach $ana (@ana_tar) {
                chomp($ana);
                $dbase = basename($ana);
                $dbase =~ s/$expid/$newid/ if $expid ne $newid;
                $dest = "$outdir/$dbase";
                unless (-e $dest) {
                    copyinput($ana, $dest);
                    push @bkg_etas, $dest if $dbase =~ m/\bbkg\d{2}_eta_rst\b/;
                }
            }
        }
    }

    # regrid bkg_eta rst files
    #-------------------------
    if ($bkg_regrid_FLG) { foreach (@bkg_etas) { regrid_bkg_eta($_) } }
}

#=======================================================================
# name - satbias_edit
# purpose - make known needed edits to satbias file
#=======================================================================
sub satbias_edit {
    my ($satbias, $sbORIG, @biasNEW, %change);

    $satbias = shift @_;

    open SATBIAS, "< $satbias" or die "Error. Opening satbias: $satbias;";
    while (<SATBIAS>) {

        if (/airs281SUBSET_aqua/) {
            $change{"airs281_aqua"} = 1;
            s/airs281SUBSET_aqua/airs281_aqua      /;
        }
        push @biasNEW, $_;
    }
    close SATBIAS;

    if (%change) {
        $sbORIG = "$satbias.ORIG";
        move_ $satbias, $sbORIG;
        print_("new satbias: $satbias\n");

        open NEW, "> $satbias" or die "Error. Opening new satbias: $satbias;";
        foreach (@biasNEW) { print NEW $_ }
        close NEW;
        print_("( airs281SUBSET_aqua -> airs281_aqua )\n");
    }
}

#=======================================================================
# name - regrid_bkg_eta
# purpose - regrid a bkg_eta_rst restart
#=======================================================================
sub regrid_bkg_eta {
    my ($bkg_eta, $bkg_eta_orig, $bkgname, $flags, $cmd);
    $bkg_eta = shift @_;

    if (basename($bkg_eta) =~ m/(\bbkg\d{2}_eta_rst\b)/) { $bkgname = $1 }
    else { $bkgname = "bkg_rst_eta" }
        
    printlabel("\nregrid $bkgname", 2);

    # rename bkg_eta file
    #--------------------
    $bkg_eta_orig = "$bkg_eta.orig";
    move_($bkg_eta, $bkg_eta_orig);

    # regrid bkg_eta file
    #--------------------
    $flags = "-g5 -res $hgrd{$grOUT} -nlevs $levsOUT";
    $cmd = "$dyn2dynX $flags -o $bkg_eta $bkg_eta_orig";
    system_($cmd);
}

#=======================================================================
# name - write_rst_lcv
# purpose - write the rst.lcv date/time restart file
#=======================================================================
sub write_rst_lcv {
    my ($rstlcvOUT, $hhmmss);
    $rstlcvOUT = rstname($newid, "rst.lcv", "$outdir/$rst_templateB");
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
    $capture .= " -levsout $levsOUT"          if $capture !~ m/\s+\-levsout\b/
        and                                      $CS{$grOUT};
    $capture .= " -outdir ". display($outdir) if $capture !~ m/\s+\-outdir\b/;

    if ($merra == 1) {
        $capture .= " -merra1" if $capture !~ m/\s+\-merra1\b/;
    }
    if ($merra == 2) {
        $capture .= " -merra2" if $capture !~ m/\s+\-merra2\b/
            or                    $capture !~ m/\s+\-merra\b/;
    }
    else {
        $capture .= " -d $rstdir"           if $capture !~ m/\s+\-d\b/;
        $capture .= " -expid $expid"        if $capture !~ m/\s+\-expid\b/ and $expid;
        $capture .= " -tagin $tagIN"        if $capture !~ m/\s+\-tagin\b/;
        $capture .= " -oceanin $grINocean_" if $capture !~ m/\s+\-oceanin\b/;
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

    if ($mk_catchcn) {
        $capture .= " -catchcn"    if $capture !~ m/\s+\-catchcn\b/;
    }
    if ($landIceDT) {
        $capture .= " -iceDT $landIceDT" if $capture !~ m/\s+\-iceDT\b/;
    }
    elsif (defined($landIceFLG) and $landIceFLG == 0) {
        $capture .= " -iceDT 0" if $capture !~ m/\s+\-iceDT\b/;
    }

    $capture .= " -grpID $grpID" if $capture !~ m/\s+\-grpID\b/ and $grpID;
    $capture .= " -zoom $zoom"   if $capture !~ m/\s+\-zoom\b/ and $zoom_;

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
        . "setenv REGRIDBIN $ESMABIN\n"
        . "\$REGRIDBIN/" .$capture ."\n";
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
# name - ftype
# purpose - determine type of file
#
# input parameter
# => $fname: full file name
#
# output
# => $type: extension indicating the type of $fname
#          = "bin" for binary files
#          = "dir" for directory
#          = "nc4" for HDF files
#          = "txt" for text files
#=======================================================================
sub ftype {
    my ($fname, $description, $type);

    $fname = shift @_;
    die "Error. Cannot find file, $fname;" unless -e $fname;

    $description = `file $fname`;
    if    ($description =~ m/directory/)    { $type = "dir" }
    elsif ($description =~ m/ASCII/)        { $type = "txt" }
    elsif ($description =~ m/text/)         { $type = "txt" }
    elsif ($description =~ m/Hierarchical/) { $type = "nc4" }
    else                                    { $type = "bin" }
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
# name - copyinput
# purpose - rename and copy input to a specified directory
#=======================================================================
sub copyinput {
    my ($src, $dest, $dest_, $dbase, $ddir, $verboseFLG);
    $src = shift @_;
    $dest = shift @_;
    $verboseFLG = 1;

    if (-d $dest) {$dbase = basename($src);  $ddir = $dest}
    else          {$dbase = basename($dest); $ddir = dirname($dest)}

    $dest = "$ddir/$dbase";

    if (-e $src) {
        copy_("\n$src", $dest, $verboseFLG);
    }
    elsif ($rst_tarfile) {
        $dest_ = "$ddir/$src";
        print_("\nExtracting from tarfile: $src\n"
               . "  => " .display($dest_) ."\n");
        system("tar -C $ddir -f $rst_tarfile -x $src");
        move_($dest_, $dest) if $dest_ ne $dest;
    }
    else { die "Error. Cannot find file to copy: $src;" }
    return $dest;
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
    my ($rstArrAddr, $rstType);
    my (@rstArr, $base, %IDfound);

    $rstArrAddr = shift @_;
    @rstArr = @$rstArrAddr;

    $rstType = shift @_;
    $rstType = "fvcore_internal_rst" unless $rstType;

    %IDfound = ();
    foreach (@rstArr) {
        $base = basename $_;
        if ($base =~ m/^(\w+)\.$rstType/) { $IDfound{$1} = $_ }
        else                              { $IDfound{"NONE"} = $_ }
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
    my ($rst, @filtered);

    @filtered = ();
    foreach (@_) {
        $rst = basename($_);
        if ($rst=~m/(\d{8}_\d{2})/) { push @filtered, $_ if $1 eq "${ymd}_$hr" }
        else                        { push @filtered, $_ }
    }
    return @filtered;
}

#=======================================================================
# name - findinput
# purpose - determine whether an input file is available
#=======================================================================
sub findinput {
    my ($fname, $altdir);
    my ($found, $extract, $file);

    $fname = shift @_;
    $altdir = shift @_;
    $found = "";

    $file = "$rstdir/$fname";
    $file = "$altdir/$fname" if (! -e $file) and $altdir;
    $found = $file if -e $file;

    unless ($found) {
        if ($rst_tarfile) {
            chomp($extract = `tar -f $rst_tarfile -t $fname 2>&1`);
            if ($extract eq $fname) { $found = $fname }
            else { print_("NOT FOUND: $fname\n") }
        }
        else {
            print_("NOT FOUND: $file\n") unless $found;
        }
    }
    print_("- $found\n") if $found;
    return $found;
}

#=======================================================================
# name - get_grouplist
# purpose - get SBATCH directive for setting group ID
#=======================================================================
sub get_grouplist {
    use getsponsor "get_spcode";
    my (%flags, $dflt);

    # get default group ID
    #---------------------
    unless ($grpID) {
        %flags = ("dflt" => 1, "quiet" => 1);
        $grpID = get_spcode(%flags);

        if ($grpID and ! $noprompt) {
            print "\n--------\n";
            print   "Group ID\n";
            print   "--------\n";

            # display menu
            #-------------
            %flags = ("menu" => 1, "quiet" => 1);
            get_spcode(%flags);

            # get user choice
            #----------------
            $grpID = query("select group:", $grpID);
            print "\n";

            # check validity
            #---------------
            %flags = ("grpID" => $grpID, "quiet" => 1);
            $grpID = get_spcode(%flags);
        }
    }

    if ($grpID) { $grouplist = "SBATCH --account=$grpID" }
    else        { $grouplist = "" }
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
    return "${dir}/${type}_c$im4{$grOUT}_$atmLevs{$levsOUT}L";
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
# name - symlinkinput
# purpose - rename and copy input to a specified directory
# note
# - if input is contained in a rst tarfile, it will be copied, since it
#   cannot be linked 
#=======================================================================
sub symlinkinput {
    my ($target, $linkname, $linkname_, $lbase, $ldir, $verboseFLG);
    $target = shift @_;
    $linkname = shift @_;
    $verboseFLG = 1;

    if (-d $linkname) {$lbase = basename($target);   $ldir = $linkname}
    else              {$lbase = basename($linkname); $ldir = dirname($linkname)}

    $linkname = "$ldir/$lbase";

    if (-e $target) {
        symlink_("\n$target", $linkname, $verboseFLG)
    }
    elsif ($rst_tarfile) {
        $linkname_ = "$ldir/$target";
        print_("\nExtracting from tarfile: $target\n"
               . "  => " .display($linkname_) ."\n");
        system("tar -C $ldir -f $rst_tarfile -x $target");
        move_("\n$linkname_", $linkname) if $linkname_ ne $linkname;
    }
    else { die "Error. Cannot find file to link: $target;" }
    return $linkname;
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
   -merra             (same as -merra2)
   -merra1            get input restarts from OPS MERRA-1 data archives
   -merra2            get input restarts from OPS MERRA-2 data archives

REQUIRED OPTIONS FOR NON-MERRA INPUTS
   -d        rstdir   location of input restart files
   -expid    expid    experiment ID of input restart files

INTERACTIVE OPTION
   -i                 prompt for inputs that are not supplied
                      (this is the default if no inputs are supplied)
   -np                no prompt; take defaults for all prompts;
                      note: the -np flag takes precedence over the -i flag

OTHER OPTIONS
   -levsout  levsout  number of atmosphere levels in output
   -oceanin  oceanIN  ocean horizontal grid of inputs
                      =c  : 1-deg (360x180); e.g. Reynolds
                      =e  : 1/4-deg (1440x720); e.g. MERRA-2
                      =f  : 1/8-deg (2880x1440); e.g. OSTIA
                      =CS : OSTIA regridded to cubed-sphere
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
   -catchcn           Create a version of the catch restart which contains carbon values;
                      Not valid for tags prior to Heracles; Note: This option will add
                      10-20 minutes to the regrid process.
   -route             write the route_internal_rst

   -[no]bkg           copy and rename input bkg + satbang/bias files
   -[no]lbl           label final restarts with \'tagID.gridID\' extension
   -[no]lcv           create rst.lcv file for final restarts
   -gcm               gcm mode; equivalent to -nobkg, -lbl, and -nolcv flags
   -altbcs [bcsdir]   use boundary condition files found in bcsdir; if bcsdir
                      is not given, then use Larry\'s bcs directory
   -zoom n            zoom value to send to land regridding codes

   -grpid             group id; sponsor code to use for batch job charge
   -qos debug         use \"SBATCH --qos=debug directive\" for batch jobs,
                      if job meets --qos=debug requirements
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
   Cn, where n = {12, 24, 48, 90, 180, 360, 500, 720, 1000, 1440, 2000, 2880, 5760}

   Ocean Horizontal Grids
   ======================
   c  = 1-deg   (360x180);   e.g. Reynolds
   e  = 1/4-deg (1440x720) ; e.g. MERRA-2
   f  = 1/8-deg (2880x1440); e.g. OSTIA
   CS = OSTIA regridded to cubed-sphere grid

TAGS
   Use GCM or DAS tag names with -tagin and -tagout flags

      Sample GCM tags
      ---------------
      F14  : $F14[1]  ............  $F14[-1]
      F21  : $F21[1]  ............  $F21[-1]
      G10  : $G10[1]  ............  $G10[-1]
      G10p : $G10p[1]  .........  $G10p[-1]
      G20  : $G20[1]  ............  $G20[-1]
      G30  : $G30[1]  ............  $G30[-1]
      G40  : $G40[1]  ............  $G40[-1]
      INL  : $INL[1]  ..............  $INL[-1]
      ICA  : $ICA[1]  .................  $ICA[-1]

      Sample DAS tags
      ---------------
      214  : $D214[1]  ..........  $D214[-1]
      540  : $D540[1]  .........  $D540[-1]
      561  : $D561[1]  .........  $D561[-1]
      580  : $D580[1]  .........  $D580[-1]
      591p : $D591p[1]  ......  $D591p[-1]
      5A0  : $D5A0[1]  ........  $D5A0[-1]
      5B0  : $D5B0[1]  .....  $D5B0[-1]
      512  : $D512[1]  ........  $D512[-1]

AUTHOR
   Joe Stassi, SSAI (joe.stassi\@nasa.gov)

EOF
;
    close INFO;
    select $FH;

    system("echo \"$info\" | more");
    exit;
}
