#!@DASPERL -w
########################################################################
#
#  Name: cnv2prs.pl
#
#  Purpose - This script will set up and run lcv2prs.x jobs for various
#            types of output files.
#
#  Notes:
#  1. See usage (type "cnv2prs.pl -help") for flag information
#  2. $FVHOME, $FVHOME, and $NCPUS must be defined in the environment
#     prior to invoking this script.
#  3. The $FVROOT/bin/gethdftinc.x and $FVROOT/bin/lcv2prs.x must exist
#  4. This script expects the CVSTAG file to be present in the
#     $FVHOME/run directory.
#
#  REVISION HISTORY
#  03Mar2006   Stassi   Initial version of code
#  16May2006   Stassi   1. query for incr instead of using hard-coded values
#                       2. add conversion to prs for diag files
#  23Jun2006   Stassi   Corrected date extract for prog.eta files
#  02Aug2007   Kokron   Split "convert eta outputs" in two for parallel processing on discover
#  06Jun2008   Todling  Add asm files
#  22Jun2009   Kokron   mods to allow scripting to recognize pleiades and do the right thing 
#
########################################################################
use strict;

#*** global variable definitions
my ($FVHOME,$FVROOT,$GETHDFTINCX,$LCV2PRSX,$help);
my ($etaasmflg,$etaanaflg,$etabkgflg,$sfcflg,$prgflg,$diagflg);
my ($dsfcflg,$detaflg);
my (@files,@rcfiles);
my ($lcvflg,$subsetflg);
my (@pidArr,$pid);
my $MAXJOBS;

#*** rs files for converting to prs
#*** note: rc files for converting diag files to prs are hard-coded
my @sfc_rc           = qw( inst2d_met_x.rc );
my @eta_rc           = qw( inst3d_met_p.rc );

#*** rs files to subset diag_sfc, diag_eta, tend_eta, and diag_eta_edge
my @subset_dsfc_rc   = qw( tavg2d_met_x.rc );
my @subset_deta_rc   = qw( tavg3d_cld_v.rc
                           tavg3d_dyn_v.rc
                           tavg3d_prs_v.rc );
my @subset_teta_rc   = qw( tavg3d_mst_v.rc
                           tavg3d_tmp_v.rc
                           tavg3d_wnd_v.rc );
my @subset_detae_rc = qw( tavg3d_met_e.rc );

&init();

#*** convert surface outputs
if ($sfcflg) {
    @files = < *.xana.sfc.*.nc4 >;
    @rcfiles = @sfc_rc;
    $lcvflg = "";
    $subsetflg = "";
    &convert_files();

    @files = < *.xbkg.sfc.*_{00,06,12,18}z.nc4 >;
    @rcfiles = @sfc_rc;
    $lcvflg = "";
    $subsetflg = "";
    &convert_files();
}

#*** convert eta outputs
if ($etaanaflg) {
    @files = < *.ana.eta.*.nc4 >;
    @rcfiles = @eta_rc;
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();
}

if ($etaasmflg) {
    @files = < *.asm.eta.*.nc4 >;
    @rcfiles = @eta_rc;
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();
}

if ($etabkgflg) {
    @files = < *.bkg.eta.*_{00,06,12,18}z.nc4 >;
    @rcfiles = ( @eta_rc );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();
}

#*** convert prog forecast outputs
if ($prgflg) {
    @files = < *.prog.eta.*.nc4 >;
    @rcfiles = @eta_rc;
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();
}

#*** convert diagnostic outputs
if ($diagflg) {
    @files = < *tavg3d_cld_v.*.nc4 >;
    @rcfiles = ( "tavg3d_cld_p.rc" );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();

    @files = < *tavg3d_dyn_v.*.nc4 >;
    @rcfiles = ( "tavg3d_dyn_p.rc" );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();

    @files = < *tavg3d_mst_v.*.nc4 >;
    @rcfiles = ( "tavg3d_mst_p.rc" );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();

    @files = < *tavg3d_tmp_v.*.nc4 >;
    @rcfiles = ( "tavg3d_tmp_p.rc" );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();

    @files = < *tavg3d_wnd_v.*.nc4 >;
    @rcfiles = ( "tavg3d_wnd_p.rc" );
    $lcvflg = "-lcv";
    $subsetflg = "";
    &convert_files();
}

#*** subset diagnostic surface outputs
if ($dsfcflg) {
    @files = < *.diag_sfc.*.nc4 >;
    @rcfiles = @subset_dsfc_rc;
    $lcvflg = "";
    $subsetflg = "-doSubset";
    &convert_files();
}

#*** subset diagnostic eta outputs (
if ($detaflg) {
    @files = < *.diag_eta.*.nc4 >;
    @rcfiles = @subset_deta_rc;
    $lcvflg = "";
    $subsetflg = "-doSubset";
    &convert_files();

    @files = < *.tend_eta.*.nc4 >;
    @rcfiles = @subset_teta_rc;
    $lcvflg = "";
    $subsetflg = "-doSubset";
    &convert_files();

    @files = < *.diag_eta_edge.*.nc4 >;
    @rcfiles = @subset_detae_rc;
    $lcvflg = "";
    $subsetflg = "-doSubset";
    &convert_files();
}

#*** wait for remaining jobs to complete before exiting
while (@pidArr) {
    $pid = shift(@pidArr);
    waitpid($pid,0);
}

#=======================================================================
#  name - init
#  purpose - get environment variables and command-line flags,
#            and set global variable $MAXJOBS
#  NOTE: $MAXJOBS is the maximum number of jobs to run in background
#=======================================================================
sub init {
    use Getopt::Long;
    my $NCPUS;
    my $help;
    chomp(my $node = `uname -n`);

    #*** check for $FVHOME, $FVROOT, and $NCPUS
    die "ERROR::\$FVHOME variable not defined: $!"
        unless ($FVHOME = $ENV{"FVHOME"});

    die "ERROR::\$FVROOT variable not defined: $!"
        unless ($FVROOT = $ENV{"FVROOT"});

    die "ERROR::\$NCPUS is zero or undefined: $!"
        unless ($NCPUS = $ENV{"NCPUS"});

    #*** check for existence of gethdftinc.x program
    $GETHDFTINCX = $FVROOT . "/bin/gethdftinc.x";
    if (! -e $GETHDFTINCX) {
        die "ERROR::$GETHDFTINCX program not found: $!";
    }

    #*** check for existence of lcv2prs.x program
    $LCV2PRSX = $FVROOT . "/bin/lcv2prs.x";
    if (! -e $LCV2PRSX) {
        die "ERROR::$LCV2PRSX program not found: $!";
    }

    #*** set global variable, MAXJOBS
    if ($node =~ /discover/
        || $node =~ /borg/
        || $node =~ /^pfe\d*/
        || $node =~ /^r\d+i\d+n\d+/) {
        $MAXJOBS = 2;
    } else {
        $MAXJOBS = $NCPUS/3;
    }

    #*** get command-line flags
    GetOptions ("sfc"  => \$sfcflg,
                "etabkg"  => \$etabkgflg,
                "etaana"  => \$etaanaflg,
                "etaasm"  => \$etaasmflg,
                "prog" => \$prgflg,
                "diag" => \$diagflg,
                "dsfc" => \$dsfcflg,
                "deta" => \$detaflg,
                "help" => \$help);
    if ($help) { &usage() };

    #*** print usage message if no options selected
    unless (   ($sfcflg)
            or ($etaanaflg)
            or ($etaasmflg)
            or ($etabkgflg)
            or ($prgflg)
            or ($diagflg)
            or ($dsfcflg)
            or ($detaflg)) { &usage() };
}


#=======================================================================
# name - convert_files
# purpose - set the calling parameters for lcv2prs.x and call program
#=======================================================================
sub convert_files {
    my ($rcf,$fl);
    my ($vdate,$start,$class,$outfl);
    my (@prsflg,$inc);
    my $command;
    my $newpid;

    foreach $rcf ( @rcfiles ) {
        foreach $fl ( @files ) {

            #*** determine values needed for job execution
            ($vdate,$start) = &extr_date_time( $fl );
            ($class,$outfl) = &get_class_outfl( $fl, $rcf );
            next unless ($vdate and $class);

            @prsflg = (&pressure_file_info( $fl ));
            $inc = "`$GETHDFTINCX $fl`";

            #*** define command
            $command = "$LCV2PRSX ".
                "-cvs $FVHOME/run/CVSTAG ".
                "@prsflg ".
                "$lcvflg ".
                "$subsetflg ".
                "-nStep=1 ".
                "-date $vdate " .
                "-start $start ".
                "-inc $inc ".
                "-rc $rcf ".
                "-vars \@$class ".
                "-o $outfl ".
                "$fl";

            #*** fork to run job in background
            defined($newpid=fork)
                or warn "WARNING::unable to fork lcv2prs.x job: $!";
            unless ($newpid) {
                exec "echo;echo $command;echo;$command";
                die "Warning::unable to exec lcv2prs.x command: $!";
            }
            push(@pidArr,$newpid);


            #*** if number of jobs has reached limit, then wait
            if (@pidArr >= $MAXJOBS) {
                while (@pidArr) {
                    $pid = shift(@pidArr);
                    waitpid($pid,0);
                }
            }
        }
    }
}


#=======================================================================
# name - get_class_outfl
# purpose - get variable class and output filename from input filename
#=======================================================================
sub get_class_outfl {
    my $infile = $_[0];
    my $rcfile = $_[1];
    my ($class,$outfl);
    my ($type,$new);

    $class = undef;
    $outfl = undef;

    #*** extract variable class name from rcfile name
    if ($rcfile =~ /(.*)\.rc/) {
        $class = $1;
    } else {
        warn "Warning::cannot extract variable class name from \"$rcfile\": $!";
        exit 3;
    }

    #*** determine the output filename
    if ($infile =~ /.*\.(asm|ana|bkg|prog)\.eta\..*\.nc4/) {
        $type = $1;
        if ($rcfile eq "ana.prs.rc") {
            $new = "prs";
        } else {
            $new = $class;
        }
        $outfl = $infile;
        $outfl =~ s/$type.eta/$type.$new/;
    }
    if ($infile =~ /.*\.(xana|xbkg)\.sfc\..*\.nc4/) {
        $type = $1;
        $outfl = $infile;
        $outfl =~ s/$type.sfc/$type.$class/;
    }
    if ($infile =~ /(.*tavg3d.*)_v(.*\.nc4)/) {
        $outfl = $1."_p".$2;
    }
    if ($infile =~ /.*\.(diag_sfc|diag_eta|tend_eta|diag_eta_edge)\..*\.nc4/) {
        $type = $1;
        $outfl = $infile;
        $outfl =~ s/$type/$class/;
    }

    unless ($outfl) {
        warn "Warning::unable to determine output filename from " .
            "\"$infile\" and \"$rcfile\": $!";
    }

    ($class,$outfl);
}


#=======================================================================
# name - extr_date_time
# purpose - extract date and time from file name
#=======================================================================
sub extr_date_time {
    my ($filename,$date,$time);

    $date = undef;
    $time = undef;

    $filename = $_[0];
    if ($filename =~ /.*\+(\d{8})_(\d{2,4})z\.*/) {
        $date = $1;
        $time = $2 . "00";
        ($time =~ /\b\d{4}\b/) and ($time = $time . "00");
    } elsif ($filename =~ /.*\.(\d{8})_(\d{2,4})z\.*/) {
        $date = $1;
        $time = $2 . "00";
        ($time =~ /\b\d{4}\b/) and ($time = $time . "00");
    } else {
        warn "Warning::cannot extract date/time from \"$filename\": $!";
    }
    ($date,$time);
}


#=======================================================================
# name - pressure_file_info
# purpose - get prsf command flag if pressure info in separate file
#=======================================================================
sub pressure_file_info {
    my $infile = $_[0];
    my (@prsflg,$prsfile);

    @prsflg = ();
    $prsfile = undef;

    if ($infile =~ /(.*)tavg3d_cld_v(.*)/) {$prsfile = $1."tavg3d_prs_v".$2};
    if ($infile =~ /(.*)tavg3d_dyn_v(.*)/) {$prsfile = $1."tavg3d_prs_v".$2};
    if ($infile =~ /(.*)tavg3d_mst_v(.*)/) {$prsfile = $1."tavg3d_prs_v".$2};
    if ($infile =~ /(.*)tavg3d_tmp_v(.*)/) {$prsfile = $1."tavg3d_prs_v".$2};
    if ($infile =~ /(.*)tavg3d_wnd_v(.*)/) {$prsfile = $1."tavg3d_prs_v".$2};

    if ($prsfile) {@prsflg = ("-prsf", "$prsfile")};

    @prsflg;
}

#=======================================================================
# name - usage
# purpose - print usage information to standard output
#=======================================================================
sub usage {
    use File::Basename;
    my $scriptname = basename($0);

    print << "EOF";

NAME
    $scriptname

DESCRIPTION
    This script runs the lcv2prs.x program to convert selected output
    to pressure coordinates.

SYNOPSIS
    $scriptname [-eta] [-sfc] [-prog]
    $scriptname -help

OPTIONS
    -sfc:  convert sfc files (xana.sfc, xbkg.sfc) to pressure
    -eta:  convert eta files (asm.eta, ana.eta, bkg.eta) to pressure
    -prog: convert prog forecast files (prog.eta) to pressure
    -diag: convert diagnostic files to pressure
    -dsfc: subset diagnostic surface files (diag.sfc)
    -deta: subset diagnostic eta files (diag_eta, diag_eta_edge)
    -help: print this information

EOF

exit (1);
}
