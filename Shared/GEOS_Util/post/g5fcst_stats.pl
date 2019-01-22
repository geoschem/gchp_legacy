#!/usr/bin/env perl
#=======================================================================
# name - g5fcst_stats.pl
# purpose - script to submit jobs to calculate forecast statistics
#
# key global variables -
# => $storedir: directory where output files get copied
#=======================================================================
use warnings;
use strict;

use FindBin qw($Bin);
use lib "$Bin";

use File::Basename qw(basename dirname);
use File::Copy qw(copy);
use File::Path qw(mkpath rmtree);
use Manipulate_time qw(tick);
use WriteLog qw(chdir_ query setprompt);

# global variables
#-----------------
my ($anadir, $arcfile, $archiveFLG, $dasFLG, $dryrun, $etcdir);
my ($expid, $fhours, $fhours_dflt, $fs_tag);
my ($idate, $ihh, $jobdir, $localID, $ncsuffix, $ndays, $noprompt, $nver);
my ($offset_sec, $pesto, $progdir, $progtype, $secs_per_day, $secs_per_hour);
my ($statsX, $storedir, $tau_freq, $tau_fsec, $vanadir, $vanatype, $vexpid);
my (%opts, @acqIDs, @statsIDs);

$fhours_dflt = 120;   # using 123 will give same output
$localID = $$;
$ncsuffix = ".nc4";
$opts{"verbose"} = 1;

$secs_per_hour = 60 * 60;
$secs_per_day  = 24 * $secs_per_hour;

# main program
#-------------
{
    my ($afile, $aname, $climfilecnt, $cmd, $dd, $dd1, $fdir);
    my ($file, $filelist, $ffile, $ffile0, $fname, $fstatswork);
    my ($mm, $mm1, $ndate, $ndd, $ndy, $nmm, $ntime, $nv, $nyyyy);
    my ($pid, $vdate, $vdate0, $vdate1, $vdd, $vhh, $vhh0, $vhh1);
    my ($vmm, $vtime, $vyyyy, $yyyy, $yyyy1);
    my (%args, %fcsthash, %pesto_dirs, %vanahash);
    my (@ana_fnames, @climfiles, @fcst_fnames, @fcstlist);
    my (@pesto_dir_list, @rmlist, @vanalist);

    init();

    # key date and time variables
    #-----------------------------------------------
    # idate, ihh
    # ----------
    # > initial forecast date and time
    #
    # ndate, ntime
    # ------------
    # > current forecast date and time
    #   (when multiple days are being evaluated, i.e. ndays > 1)
    #
    # vdate, vtime, vhh
    # -----------------
    # > current verification date and time of current forecast
    #
    # vdate0, vhh0
    # ------------
    # > first verification date and time of first forecast;
    #   used for labeling the archive log file
    #
    # vdate1, vhh1
    # ------------
    # > first verification date and time of current forecast
    #   (when multiple days are being evaluated, i.e. ndays > 1)
    #   used for labeling outputs and log files
    #-----------------------------------------------

    # initial forecast date and time
    #-------------------------------
    $ndate = $idate;
    $ntime = "${ihh}0000";

    $vdate0 = 0;
    $vhh0 = 0;

    %pesto_dirs = ();

    write_g5fcst_stats_arc();
    verify_values();

    # loop over number of days to process
    #------------------------------------
    foreach $ndy (1..$ndays) {
        das_check($ndate, $ntime) if $dasFLG;

        # use a separate work directory for each day
        #-------------------------------------------
        $fstatswork = $ENV{"NOBACKUP"} ."/FSTATSWORK.$localID.$ndy";
        die "Work directory already exists;" if -e $fstatswork;
        mkpath($fstatswork, \%opts);

        # loop over verification hours
        #-----------------------------
        %fcsthash = ();
        %vanahash = ();

        # initial verification date and time for current forecast
        #--------------------------------------------------------
        ($vdate, $vtime) = tick($ndate, $ntime, $offset_sec);

        # save initial verification date/time from current forecast
        # for labeling output and log files
        #----------------------------------
        $vdate1 = $vdate;
        $vhh1 = extract_hh($vtime);

        # save initial verification date/time from first forecast
        # for labeling archive log
        #-------------------------
        $vdate0 = $vdate1 unless $vdate0;
        $vhh0 = $vhh1 unless $vhh0;

        ($nyyyy, $nmm, $ndd) = extract_yyyy_mm_dd($ndate);

        foreach $nv (1..$nver) {
            ($vyyyy, $vmm, $vdd) = extract_yyyy_mm_dd($vdate);
            $vhh = extract_hh($vtime);

            if ($ihh eq "00" and $nv == 1) {
                $fdir = "$vanadir/Y$nyyyy/M$nmm";
                $fname = "$vexpid.$vanatype.inst3d_met_p.${vdate}_${vhh}z";
                $ffile0 = "$fdir/$fname$ncsuffix";
            }
            else {

                # allows diffing with analysis (doing say EC-ana vs G5-ana)
                #----------------------------------------------------------
                if ($progtype eq "ana") {
                    $fdir  = "$anadir/Y$vyyyy/M$vmm";
                    $fname = "$expid.ana.inst3d_met_p.${vdate}_${vhh}z";
                }
                else {
                    $fdir  = "$progdir/Y$nyyyy/M$nmm/D$ndd/H$ihh";
                    $fname = "$expid.prog.$progtype.${ndate}_${ihh}z+${vdate}_${vhh}z";
                }
            }
            $ffile = "$fdir/$fname$ncsuffix";
            unless (-e $ffile) {
                rmtree($fstatswork) if -d $fstatswork;
                die "Error. Cannot find forecast : $ffile;";
            }
            $fcsthash{$ffile} = 1;

            if ($vexpid eq "gfs" or $vexpid eq "ecmwf") {
                $aname = "$vexpid.$vanatype.${vdate}_${vhh}00z" if $nv == 1;
            }
            else {
                $aname = "$vexpid.$vanatype.inst3d_met_p.${vdate}_${vhh}z";
            }
            $afile = "$vanadir/Y$vyyyy/M$vmm/$aname$ncsuffix";
            unless (-e $afile) {
                rmtree($fstatswork) if -d $fstatswork;
                die "Error. Cannot find ver analysis : $afile;";
            }
            $vanahash{$afile} = 1;

            # increment lead-time of forecast (verification time)
            #----------------------------------------------------
            ($vdate, $vtime) = tick($vdate, $vtime, $tau_fsec);
        }

        #--@climfiles = (<$ENV{SHARE}/dao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_??z.576x361.data.nc4>);
        @climfiles = (<$ENV{SHARE}/dao_ops/verification/stats/merrasc.197901-200812.clim_??z.144x91.data.nc>);
        $climfilecnt = scalar(@climfiles);
        if ($climfilecnt < 4) {
            rmtree($fstatswork) if -d $fstatswork;
            die "Error. Only $climfilecnt out of 4 Climatology Files found - exiting;"
        }

        # get fcst and ana filenames
        #---------------------------
        if ($vexpid eq "g5ncep" or $vexpid eq "gfs" or $vexpid eq "ecmwf") {
            if ($progtype eq "ana") {
                push @fcst_fnames, "$expid.ana.inst3d_met_p.*$ncsuffix";
                push @ana_fnames, "$vexpid.$vanatype.*$ncsuffix";
            }
            else {
                push @fcst_fnames, "$expid.prog.$progtype.${ndate}_${ihh}z+*$ncsuffix";
                push @ana_fnames, "$vexpid.$vanatype.*$ncsuffix";
            }
        }
        elsif ($vanatype eq "asm") {
            if ($progtype eq "ana") {
                push @fcst_fnames, $ffile0 if $ffile0;
                push @fcst_fnames, "$expid.ana.inst3d_met_p.*$ncsuffix";
                push @ana_fnames, "$vexpid.asm.inst3d_met_p.*$ncsuffix";
            }
            else {
                push @fcst_fnames, $ffile0 if $ffile0;
                push @fcst_fnames, "$expid.prog.$progtype.${ndate}_${ihh}z+*$ncsuffix";
                push @ana_fnames, "$vexpid.asm.inst3d_met_p.*$ncsuffix";
            }
        }
        else {
            push @fcst_fnames, $ffile0 if $ffile0;
            push @fcst_fnames, "$expid.prog.$progtype.${ndate}_${ihh}z+*$ncsuffix";
            push @ana_fnames, "$vexpid.asm.inst3d_met_p.*$ncsuffix";
            push @ana_fnames, "$vexpid.ana.inst3d_met_p.*$ncsuffix";
        }
        @fcstlist = sort keys %fcsthash;
        @vanalist = sort keys %vanahash;

        # dmget inputs
        #-------------
        foreach $filelist (\@fcstlist, \@vanalist) {
            $cmd = "dmget @$filelist";
            print "\n$cmd\n";
            defined ($pid = fork) or die "Error. fork failed: $!";
            unless ($pid) {
                exec $cmd;
                die "Error. exec dmget failed: $!";
            }
        }

        # fetch inputs
        #-------------
        print "\nCopying files to directory: $fstatswork\n";
        foreach $file (@fcstlist, @vanalist) {
            print "=> " .basename($file) ."\n";
            copy $file, $fstatswork or die "Error. copy failed: $file; $!";
        }

        # store calling arguments in a hash
        #----------------------------------
        $args{"ana_fnames_addr"}  = \@ana_fnames;
        $args{"climfiles_addr"}   = \@climfiles;
        $args{"fcst_fnames_addr"} = \@fcst_fnames;
        $args{"fcstlist_addr"}    = \@fcstlist;
        $args{"vanalist_addr"}    = \@vanalist;
        $args{"fstatswork"}       = $fstatswork;
        $args{"vdate0"}           = $vdate0;
        $args{"vdate1"}           = $vdate1;
        $args{"vhh0"}             = $vhh0;
        $args{"vhh1"}             = $vhh1;

        # submit job to calculate stats
        #------------------------------
        submit_calcjob(%args);

        # make list of search directories for archive job
        #------------------------------------------------
        ($yyyy1, $mm1, $dd1) = extract_yyyy_mm_dd($vdate1);
        $pesto_dirs{"prog/$fs_tag/Y$yyyy1/M$mm1"} = 1;
        $pesto_dirs{"etc/Y$yyyy1/M$mm1"} = 1;

        # increment to next day to be evaluated
        #--------------------------------------
        ($ndate, $ntime) = tick($ndate, $ntime, $secs_per_day);
    }

    # submit archive job
    #-------------------
    @pesto_dir_list = sort keys %pesto_dirs;
    $args{"pesto_dir_list_addr"} = \@pesto_dir_list;
    submit_archivejob(%args);
}

#=======================================================================
# name - init
# purpose - get runtime parameters and options
#=======================================================================
sub init {
    use Getopt::Long qw(GetOptions);
    my ($help, $offset, %opts);

    $ENV{"PATH"} = ".:$Bin:$ENV{PATH}";
    do "g5_modules_perl_wrapper";

    # flush buffer after each output operation
    #-----------------------------------------
    $| = 1;

    # get runtime options
    #--------------------
    GetOptions("ihh=i"     => \$ihh,
               "fhrs=i"    => \$fhours,

               "anadir=s"  => \$anadir,
               "progdir=s" => \$progdir,

               "vexpid=s"  => \$vexpid,
               "vanadir=s" => \$vanadir,

               "ptype=s"   => \$progtype,
               "vtype=s"   => \$vanatype,

               "storedir=s" => \$storedir,
               "archive!"   => \$archiveFLG,

               "das"      => \$dasFLG,
               "np"       => \$noprompt,

               "dryrun"   => \$dryrun,
               "h|help"   => \$help);
    usage() if $help;

    $noprompt = 1 if $dasFLG;
    setprompt(0) if $noprompt;

    # get runtime parameters
    #-----------------------
    $expid = shift @ARGV;
    $idate = shift @ARGV;
    $ndays = shift @ARGV;
               
    # initial fcst hour and offset
    #-----------------------------
    unless ($ihh) {
        if ($expid eq "a_flk_04") { $ihh =  0 }
        else                      { $ihh = 21 }
    }
    if ($ihh == 0 or $ihh == 6 or $ihh == 12 or $ihh == 18) { $offset = 0 }
    if ($ihh == 3 or $ihh == 9 or $ihh == 15 or $ihh == 21) { $offset = 3 }

    $ihh = sprintf("%02d", $ihh);
    $offset_sec = $offset * $secs_per_hour;

    # frequecy of verification (hours)
    #---------------------------------
    $tau_freq = 12;
    $tau_fsec = $tau_freq * $secs_per_hour;

    # length of fcst in hours
    #------------------------
    $fhours = $fhours_dflt unless $fhours;
    $nver = int($fhours/$tau_freq) + 1;

    # interactive runtime params
    #---------------------------
    until ($expid) {
        $expid = query("Forecast Experiment ID: ");
    }
    until ($idate and $idate =~ m/^\d{8}$/) {
        print "\nCannot decipher initial date: $idate\n" if $idate;
        $idate = query("initial date <yyyymmdd>: ");
    }
    until ($ndays and $ndays =~ m/^\d+$/) {
        print "\nCannot decipher ndays: $ndays\n" if $ndays;
        $ndays = query("number of days to process <n>: ");
    }
    usage() unless $idate and $ndays and $expid;

    # preliminary check for forecast and das hidden files
    #----------------------------------------------------
    das_check($idate, "${ihh}0000") if $dasFLG;

    # option defaults
    #----------------
    if ($dryrun) { $dryrun = "echo" }
    else         { $dryrun = "" }
    $archiveFLG = 1 unless defined($archiveFLG);

    # determine $storedir (the place where output gets copied prior to archival)
    #---------------------------------------------------------------------------
    $storedir = $ENV{"STOREDIR"} unless $storedir;
    unless ($storedir) { $storedir = dirname($ENV{"FVHOME"}) if $ENV{"FVHOME"} }
    unless ($storedir) { $storedir = $ENV{"NOBACKUP"} if $ENV{"NOBACKUP"} }
    die "Error. Storage directory not defined;" unless $storedir;

    mkpath($storedir, \%opts) unless -d $storedir;
    die "Error creating directory: $storedir;" unless -d $storedir;

    $jobdir = "$storedir/$expid/fstats";
    mkpath($jobdir, \%opts) unless -d $jobdir;

    $etcdir = "$storedir/$expid/etc";
    mkpath($etcdir, \%opts) unless -d $etcdir;

    # get $anadir and $progdir
    #-------------------------
    $anadir = "$ENV{ARCHIVE}/$expid/ana" unless $anadir;
    until (-d $anadir) {
        unless ($noprompt) {
            print "ANA directory does not exist: $anadir\n\n";
            $anadir = query("forecast ana directory:", $anadir);
        }
        else { die "forecast ana directory does not exist: $anadir;" }
    }
    $progdir = dirname($anadir) ."/prog" unless $progdir;
    until (-d $progdir) {
        unless ($noprompt) {
            print "forecase prog directory does not exist: $progdir\n\n";
            $progdir = query("forecast prog directory:", $progdir);
        }
        else { die "forecast prog directory does not exist: $progdir;" }
    }

    # get vexpid and vanadir
    #-----------------------
    unless ($vexpid) {
        $vexpid = basename(basename($vanadir)) if $vanadir;
        $vexpid = $expid unless $vexpid;
        $vexpid = query("verifying experiment ID:", $vexpid);
    }
    unless ($vanadir) {
        $vanadir = dirname(dirname($anadir))."/$vexpid/ana";
        $vanadir = $anadir unless -d $vanadir;
        $vanadir = query("verifying experiment ana directory:", $vanadir);
    }
    until (-d $vanadir) {
        print "Cannot find VANA verifying directory: $vanadir\n\n";
        $vanadir = query("verifying experiment ana directory:");
        $vexpid = basename($vanadir);
    }

    # get progtype and vanatype
    #--------------------------
    unless ($progtype) {
        $progtype = "inst3d_met_p";
        $progtype = query("forecast prog type: ", $progtype);
    }
    unless ($vanatype) {
        $vanatype = "asm";
        $vanatype = query("verification data type: ", $vanatype);
    }

    # determine fs_tag
    #--------------------
    $fs_tag = "fstats_$vanatype";

    if ($vanatype eq "inst3_3d_ana_Np") {
        $fs_tag = "fstats_ana"
    }

    if ($vexpid eq "dctl573" or $vexpid eq "dhyb573a") {
        $vanadir = "/archive/u/aelakkra/$vexpid/ana";
    }
    elsif ($vexpid eq "ecmwf") {
        $fs_tag = "fstats_ecmwf";
        if ($progtype eq "ana") {
            $fs_tag = "astats_ecmwf";
        }
    }
    elsif ($vexpid eq "gfs") {
        $fs_tag = "fstats_gfs";
        if ($progtype eq "ana") {
            $fs_tag = "astats_gfs";
        }
    }
    elsif ($vexpid eq "rsens3") {
        $vanadir = "/archive/u/dndaescu/$vexpid/ana"
    }
    elsif ($vexpid eq "d5124_m2_jan00") {
        $vanadir = "/home/dao_ops/$vexpid/run/.../archive/ana";
    }
    elsif ($expid eq "a_flk_04") {
        $vanatype = "ana";
        $progdir = "$ENV{ARCHIVE}/geos4/$vexpid/prog" unless $progdir;
        $vanadir = "$ENV{ARCHIVE}/geos4/$vexpid/ana";
        $fs_tag = "fstats_$vanatype"
    }

    if ($progtype eq "ana" and $expid eq $vexpid) {
        $fs_tag = "astats_self";
    }

    if ($vexpid eq "d591_rpit3_jan11") {
        $vanadir = "/discover/nobackup/rtodling/d591_rpit3_jan11/ana";
    }

    if ($vexpid eq "g5ncep") {
        $vanadir = "/archive/u/rtodling/g5ncep/$vexpid";
        $fs_tag = "fstats_ncana";
    }

    # find stats.x and pesto
    #-----------------------
    chomp($statsX = `which stats.x`);
    die "Error. Cannot find stats.x program;" unless -e $statsX;

    chomp($pesto = `which pesto`);
    die "Error. Cannot find pesto;" unless -e $pesto;
}

#=======================================================================
# name - das_check
# purpose - check for hidden files in $FVHOME/fcst and $FVHOME directories
#           left by DAS jobs to determine whether to calculate statistics
#
# input parameters
# => $ndate: forecast date; format, yyyymmdd
# => $ntime: forecast time; format, hhmmss
#=======================================================================
sub das_check {
    my ($ndate, $ntime);
    my ($calculate_stats, $fdatetime, $FVHOME, $dotSUBMITTED);
    my ($ddate, $dtime, $ddatetime, $dotDONE, %notfound);

    $ndate = shift @_;
    $ntime = shift @_;

    $calculate_stats = 1;

    # check that FVHOME variable is defined
    #--------------------------------------
    $FVHOME = $ENV{"FVHOME"};
    die "Error. FVHOME environment variable not defined;" unless $FVHOME;
    die "Error. Cannot find FVHOME directory: $FVHOME;" unless -d $FVHOME;

    # check for fcst SUBMITTED hidden file
    #-------------------------------------
    $fdatetime = "${ndate}_" .substr($ntime, 0, 2) ."z";
    $dotSUBMITTED = "$ENV{FVHOME}/fcst/.SUBMITTED.$fdatetime";
    unless (-e $dotSUBMITTED) {
        print "\nCannot find forecast hidden file:\n"
            . "- $dotSUBMITTED\n\n";
        $calculate_stats = 0;
    }

    # check for das hidden DONE files
    #--------------------------------
    ($ddate, $dtime) = tick($ndate, $ntime, $offset_sec);
    for (1..$nver) {
        $ddatetime = $ddate .substr($dtime, 0, 2);
        $dotDONE = "$FVHOME/.DONE_CENTRAL_ADAS.$ddatetime";
        $notfound{$dotDONE} = 1 unless -e $dotDONE;
        ($ddate, $dtime) = tick($ddate, $dtime, $tau_fsec);
    }
    if (%notfound) {
        print "Cannot find ana hidden files:\n";
        foreach (sort keys %notfound) { print "- $_\n" }
        $calculate_stats = 0;
    }

    # quit if hidden files not found
    #-------------------------------
    unless ($calculate_stats) {
        print "\nForecast statistics will not be calculated for $fdatetime\n\n";
        exit;
    }
}

#=======================================================================
# name - write_g5fcst_stats_arc
# purpose - write the g5fcst_stats.arc file
#=======================================================================
sub write_g5fcst_stats_arc {
    use File::Copy qw(move);
    my ($arcfile_tilde);

    $arcfile = "$jobdir/g5fcst.$fs_tag.arc";
    if (-e $arcfile) {
        $arcfile_tilde = "${arcfile}~";
        if (-e $arcfile_tilde) { unlink $arcfile }
        else                   { move $arcfile, $arcfile_tilde}
    }
    open ARC, "> $arcfile" or die "Error opening ARC file: $arcfile; $!";
    print ARC <<"EOF" or die "Error writing ARC file: $arcfile: $!";
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.%h2z.globl.b%y4%m2%d2_%h2z.e%y4%m2%d2_%h2z.ctl
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.%h2z.globl.b%y4%m2%d2_%h2z.e%y4%m2%d2_%h2z.data
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.%h2z.stats.b%y4%m2%d2_%h2z.e%y4%m2%d2_%h2z.ctl1
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.%h2z.stats.b%y4%m2%d2_%h2z.e%y4%m2%d2_%h2z.ctl2
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.%h2z.stats.b%y4%m2%d2_%h2z.e%y4%m2%d2_%h2z.data
\${PESTOROOT}%s/prog/$fs_tag/Y%y4/M%m2/%s.fstats.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.fstats_inputs.log.%y4%m2%d2_%h2z.txt
\${PESTOROOT}%s/etc/Y%y4/M%m2/%s.fstats_calc.log.%y4%m2%d2_%h2z.txt
EOF
;
    close ARC;
}

#=======================================================================
# name - submit_calcjob
# purpose - write and submit jobfile to run stats
#=======================================================================
sub submit_calcjob {
    my (%args, @ana_fnames, @climfiles, @fcst_fnames, @fcstlist, @vanalist);
    my ($fstatswork, $vdate1, $vhh1);
    my (@rmfilelist, $yyyy, $mm, $dd);
    my ($logdir, $logfile, $jobname, $jobdate, $jobfile);
    my ($cmd, $jobID, $jobIDline);

    # input arguments
    #----------------
    %args = @_;

    @ana_fnames  = @{$args{"ana_fnames_addr"}};
    @climfiles   = @{$args{"climfiles_addr"}};
    @fcst_fnames = @{$args{"fcst_fnames_addr"}};
    @fcstlist    = @{$args{"fcstlist_addr"}};
    @vanalist    = @{$args{"vanalist_addr"}};

    $fstatswork  = $args{"fstatswork"};
    $vdate1      = $args{"vdate1"};
    $vhh1        = $args{"vhh1"};

    foreach (@fcstlist, @vanalist) { push @rmfilelist, basename($_) };

    ($yyyy, $mm, $dd) = extract_yyyy_mm_dd($vdate1);
    $logdir = "$etcdir/Y$yyyy/M$mm";
    mkpath($logdir, \%opts) unless -d $logdir;

    $jobname = "fstats_calc";
    $jobdate = "${vdate1}_${vhh1}z";
    $jobfile = "$jobdir/$expid.$jobname.$jobdate.j";
    $logfile = "$logdir/$expid.$jobname.log.$jobdate.txt";

    print "\nwriting jobfile: $jobfile\n";
    open FH, "> $jobfile" or die "Error opening $jobfile; $!";
    print FH <<"EOF" or die "Error writing to $jobfile: $!";
#!/usr/bin/csh
#SBATCH --time=1:00:00
#SBATCH --job-name=$jobname.$jobdate
#SBATCH --output=$logfile.FAILED
#SBATCH --export=NONE
#SBATCH --constraint=hasw
##SBATCH --qos=debug   # iah

source $Bin/g5_modules
set echo
chdir $fstatswork

$dryrun mpiexec_mpt $statsX -fcst @fcst_fnames \\
                    -ana @ana_fnames \\
                    -cli @climfiles \\
                    -tag $expid.${ihh}z \\
                    -nfreq ${tau_freq}0000 \\
                    -levs 1000.0 850.0 700.0 500.0 400.0 \\
                           300.0 250.0 200.0 150.0 100.0 \\
                    -o $expid.fstats.log.$jobdate.txt \\
                    -verif gmao \\
                    -fcsrc gmao
@ calc_status = \$status

$pesto -arc $arcfile \\
       -expid $expid \\
       -d $fstatswork \\
       -r $storedir \\
       -l -v -clean
@ calc_status = \$calc_status + \$status

if (! \$calc_status) then
   \\rm @rmfilelist
   ls -l
   chdir ~
   \\rm -r $fstatswork
   qalter -o $logfile \$SLURM_JOBID
endif

EOF
;
    close FH;

    print "submitting jobfile: $jobfile\n";
    $cmd = "sbatch $jobfile";
    print "> $cmd\n";
    chomp($jobIDline = `$cmd`);

    $jobID = (split /\s+/, $jobIDline)[-1];
    print "jobID = $jobID\n\n";
    push @statsIDs, $jobID;
}

#=======================================================================
# name - submit_archivejob
# purpose - write and submit jobfile for archiving stats output files
#=======================================================================
sub submit_archivejob {
    my (%args, @pesto_dir_list, $vdate0, $vhh0);
    my ($yyyy, $mm, $dd, $logdir, $logfile, $pdir);
    my ($jobname, $jobdate, $jobfile_0, $jobfile);
    my ($cmd, $deps, $dependFLG, $jobIDline, $jobID);

    # input arguments
    #----------------
    %args = @_;
    @pesto_dir_list = @{$args{"pesto_dir_list_addr"}};
    $vdate0 = $args{"vdate0"};
    $vhh0   = $args{"vhh0"};

    ($yyyy, $mm, $dd) = extract_yyyy_mm_dd($vdate0);
    $logdir = "$etcdir/Y$yyyy/M$mm";
    mkpath($logdir, \%opts) unless -d $logdir;

    $jobname = "fstats_archive";
    $jobdate = "${vdate0}_${vhh0}z";
    $jobfile = "$jobdir/$expid.$jobname.${jobdate}_$ndays.j";
    $logfile = "$logdir/$expid.$jobname.log.${jobdate}_$ndays.txt";

    $jobfile_0 = "${jobfile}_0";
    open FH0, "> $jobfile_0" or die "Error opening $jobfile_0; $!";
    print FH0 <<"EOF" or die "Error writing to $jobfile_0: $!";
#!/usr/bin/csh
#SBATCH --time=1:00:00
#SBATCH --job-name=$jobname.$jobdate
#SBATCH --partition=datamove
#SBATCH --output=$logfile.FAILED
#SBATCH --export=NONE
#SBATCH --constraint=hasw

set echo
@ archive_status = 0
foreach dir ( \\
              )

    $pesto -arc $arcfile \\
           -expid $expid \\
           -d $storedir/$expid/\$dir \\
           -r \$ARCHIVE \\
           -l -v -clean

    @ archive_status = \$status + \$archive_status
end

if (! \$archive_status) then
   qalter -o $logfile \$SLURM_JOBID
endif
EOF
;
    close FH0;

    print "writing jobfile: $jobfile\n";
    open FH0, "< $jobfile_0" or die "Error opening $jobfile_0: $!;";
    open FH,  "> $jobfile"   or die "Error opening $jobfile: $!;";
    while (<FH0>) {
        print FH $_;
        if (m/foreach dir/) {
            foreach $pdir (@pesto_dir_list) {
                printf FH " " x 13;
                print FH "$pdir \\\n";
            }
        }
    }
    return unless $archiveFLG;
    close FH0;
    close FH;
    unlink($jobfile_0);

    $deps = "";
    foreach (@statsIDs) { $deps .= ":$_" if $_ }

    $dependFLG = "";
    $dependFLG = "--dependency afterany$deps" if $deps;

    print "submitting jobfile: $jobfile\n";
    $cmd = "sbatch $dependFLG $jobfile";
    print "> $cmd\n";
    chomp($jobIDline = `$cmd`);

    $jobID = (split /\s+/, $jobIDline)[-1];
    print "jobID = $jobID\n\n";
}

#=======================================================================
# name: extract_hh
# purpose: extract hh value from hhmmss string
#
# input parameter:
# => $hhmmss: input time string
#
# return value:
# => $hh: extracted hour value
#=======================================================================
sub extract_hh {
    my ($hhmmss, $hh);
    $hhmmss = shift @_;

    ($hh) = ($hhmmss =~ m/^(\d{2})\d{4}$/)
        or die "Error. Undecipherable time: $hhmmss;";
    return $hh;
}

#=======================================================================
# name: extract_yyyy_mm_dd
# purpose: extract yyyy, mm, and dd values from yyyymmdd string
#
# input parameter:
# => $yyyymmdd: input date string
#
# return values:
# => ($yyyy, $mm, $dd): extracted date values
#=======================================================================
sub extract_yyyy_mm_dd {
    my ($yyyymmdd, $yyyy, $mm, $dd);
    $yyyymmdd = shift @_;

    ($yyyy, $mm, $dd) = ($yyyymmdd =~ m/^(\d{4})(\d{2})(\d{2})$/)
        or die "Error. Undecipherable date: $yyyymmdd;";
    return ($yyyy, $mm, $dd);
}

#=======================================================================
# name - verify_values
# purpose - have user verify job values
#=======================================================================
sub verify_values {
    my ($ans);
    print "\nJob Inputs and Values\n";
    print   "---------------------\n";
    print   "localID: $localID\n";
    print   "---------------------\n";
    print "expid:       $expid\n";
    print "idate:       $idate\n";
    print "ndays:       $ndays\n\n";

    print "anadir:      $anadir\n";
    print "vanadir:     $vanadir\n";
    print "vanatype:    $vanatype\n";
    print "vexpid:      $vexpid\n\n";

    print "progdir:     $progdir\n";
    print "progtype:    $progtype\n";
    print "fs_tag:      $fs_tag\n\n";

    print "dryrun:      $dryrun\n\n" if $dryrun;

    $ans = query("Continue (y/n):", "y");
    if ($ans eq "n") {
        print "Exiting.\n";
        exit();
    }
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    my $script = basename($0);
    print <<"EOF";

NAME
    $script

SYNOPSIS
    $script \$expid \$idate \$ndays [OPTIONS]

PARAMETERS
     expid             forecast experiment ID
     idate             initial date of forecast; format: yyyymmdd
     ndays             number of days to process

OPTIONS [defaults in brackets]
    -ihh ihh           initial hour of forecast [21]
    -fhrs fhours       forecast length in hrs [$fhours_dflt]

    -anadir anadir     forecast ana directory [\$ARCHIVE/\$expid/ana]
    -progdir progdir   forecast prog directory [\$ARCHIVE/\$expid/prog]

    -vexpid vexpid     verifying experiment ID [basename(basename(\$vanadir)) or \$expid]
    -vanadir vanadir   verifying experiment ana directory [basename(\$anadir)/\$vexpid]

    -ptype progtype    forecast prog type [inst3d_met_p]
    -vtype vanatype    verification data type [asm]

    -storedir storedir location to move outputs after processing, prior to archiving
                       [dirname(\$FVHOME) or \$NOBACKUP]
    -noarchive         do not archive outputs [archives by default]

    -das               check for DAS hidden files before attempting to fetch files
                       and set no prompt; requires \$FVHOME environment variable;

    -np                no prompt; do not prompt for inputs
    -dryrun            dry run; show stats.x commands, but do not execute
    -h, -help          print usage information

EOF
exit();
}
