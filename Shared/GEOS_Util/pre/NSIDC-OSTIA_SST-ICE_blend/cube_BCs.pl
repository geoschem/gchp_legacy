#!/usr/bin/env perl
#=======================================================================
#
# This script converts a binary file of SST or Sea Ice Concentration on
# regular lat-lon grid to a binary file on cube sphere grid
#
# !REVISION HISTORY
# 09Jun2017 Auer   Initial version
# 09Jun2017 Akella General version, but preset resolutions 
#                   (input: 2880x1440; output: c360 or c720) for DAO OPS usage only
# 30Jun2017 Stassi Converted to perl
#=======================================================================
use strict;
use warnings;
use Cwd qw(abs_path cwd);
use File::Path qw(mkpath rmtree);

use FindBin qw($Bin);
use lib $Bin;

use getsponsor qw(get_spcode);

# global variables
#-----------------
my ($outdir, $gid, $workarea, $noprompt);
my ($yyyy, %oResVals, %varVals, $workdir);
my ($tiledir, $bcsdir, %variables, %resolutions);

$tiledir = "/discover/nobackup/ltakacs/bcs/Icarus/Shared";

$bcsdir = "/discover/nobackup/projects/gmao/share/dao_ops/fvInput/"
    .      "g5gcm/bcs/realtime/OSTIA_REYNOLDS/2880x1440";

%variables = ( "SST" => 1,
               "ICE" => 1,
               "ALL" => 1 );

%resolutions = ( "C90"  => 1,
                 "C180" => 1,
                 "C360" => 1,
                 "C720" => 1,
                 "C1440"=> 1,
                 "C2880"=> 1,
                 "ALL"  => 1 );

# main program
#-------------
{
    init();
    write_and_submit_jobfile();
}

#=======================================================================
# name - init
# purpose -
#=======================================================================
sub init {
    use Getopt::Long qw(GetOptions);
    my ($help, %flags, $ans);
    my ($oRes, @outRes, $res);
    my ($var, @vars, $vv);

    GetOptions( "d=s"   => \$outdir,
                "gid=s" => \$gid,
                "wa=s"  => \$workarea,
                "np"    => \$noprompt,
                "h"     => \$help);
    usage() if $help;

    # get input arguments
    #--------------------
    usage() if scalar(@ARGV) < 3;
    $yyyy = shift @ARGV;
    $var  = uc(shift @ARGV);
    $oRes = uc(shift @ARGV);

    $workarea = $ENV{"NOBACKUP"} unless $workarea;
    unless ($gid) {
        %flags = ();
        if ($noprompt) { $flags{"dflt"} = 1 }
        $gid = get_spcode(%flags);
    }

    # get oRes values
    #----------------
    @outRes = split /[,]/, $oRes;
    foreach $res (@outRes) {
        die "Error. Unknown resolution: $res;" unless $resolutions{$res};
        if ($res eq "ALL") {
            foreach (keys %resolutions) { $oResVals{$_} = 1 unless $_ eq "ALL" };
            last;
        } else { $oResVals{$res} = 1 }
    }

    # get var values
    #---------------
    @vars = split /[,]/, $var;
    foreach $vv (@vars) {
        die "Error. Unknown variable: $vv;" unless $variables{$vv};
        if ($vv eq "ALL") {
            foreach (keys %variables) { $varVals{$_} = 1 unless $_ eq "ALL" };
            last;
        } else { $varVals{$vv} = 1 unless $vv eq "ALL" }
    }

    # check inputs
    #-------------
    die "Error. Cannot decipher year: $yyyy;" unless $yyyy =~ m/^\d{4}$/;
    die "Error. Work area not found; $workarea;" unless -d $workarea;

    unless ($outdir) {
        $ans = "n";
        unless ($noprompt) {
            print "\nOutput to local directory (y/n)? [n]";
            chomp($ans = <STDIN>);
            $ans = "n" unless lc($ans) eq "y";
        }
        if (lc($ans) eq "n") {
            die "Error. Output directory location not given (use -d option);"
        }
        $outdir = cwd();
    }
    unless (-d $outdir) {
        $ans = "n";
        unless ($noprompt) {
            print "\nOutput directory: $outdir\n";
            print "Make Output Directory (y/n)? [n] ";
            chomp($ans = <STDIN>);
            $ans = "n" unless lc($ans) eq "y";
        }
        die "Error. Output directory not found: $outdir;" if lc($ans) eq "n";
        %flags = ();
        $flags{"verbose"} = 1;
        mkpath($outdir, \%flags);
    }
    $workarea = abs_path($workarea);
    $outdir = abs_path($outdir);
}

#=======================================================================
# name - write_and_submit_jobfile
# purpose -
#=======================================================================
sub write_and_submit_jobfile {
    my ($base, $jobfile, $logfile, $output);
    my (@oResList, @varList);
    my ($FH, $cmd);

    @oResList = (sort keys %oResVals);
    @varList = (sort keys %varVals);

    $base = "cube_BCs.$yyyy";
    $jobfile = "$outdir/$base.j";
    $logfile = "$outdir/$base.log.txt";
    unlink $jobfile if -e $jobfile;
    unlink $jobfile if -e $logfile;

    open JF, "> $jobfile" or die "Error opening jobfile: $jobfile; $!";
    $FH = select;
    select JF;

    print <<"EOF";
#!/bin/csh
#SBATCH --account=$gid
#SBATCH --export=NONE
#SBATCH --job-name=$base
#SBATCH --output=$logfile.RUNNING
#SBATCH --ntasks=24
#SBATCH --constraint=hasw
#SBATCH --time=1:00:00

set echo
set bcsdir = $bcsdir
set tiledir = $tiledir

set yyyy = $yyyy
set FVBIN = $Bin
set outdir = $outdir
set logfile = $outdir/$base.log.\$\$.txt

unset echo
source \$FVBIN/g5_modules

set echo
set workarea = $workarea
set workdir = \$workarea/bcswork.\$yyyy.\$\$

if (-d \$workdir) \\rm -r \$workdir
mkdir -p \$workdir
cd \$workdir

\@ errcnt = 0
set output_list = ()

foreach res (@oResList)

   set resX = `echo \$res | cut -c2-`
   \@ resY = \$resX * 6
   set resX3 = \$resX
   if (\$resX3 < 100) set resX3 = 0\$resX3
   set resX4 = 0\$resX3

   set tileIN = DE2880xPE1440_CF\${resX4}x6C.bin
   ln -s \$tiledir/\$tileIN \$workdir/\$tileIN

   set out_im = \$resX
   set out_gridname = PE\${resX}x\${resY}-CF
   set out_tiling_file = DE2880xPE1440_CF\${resX4}x6C.bin

   foreach var (@varList)
      set input_bcs  = dataoceanfile_OSTIA_REYNOLDS_\$var.2880x1440.\$yyyy.data
      set output     = dataoceanfile_OSTIA_REYNOLDS_\$var.\${resX}x\${resY}.\$yyyy.data

      if (! -e \$input_bcs) ln -s \$bcsdir/\$input_bcs .

      set rcfile = REGRID_FORCING_\${res}_\${var}.rc

      alias rcwrite "echo \\!* >> \$rcfile"
      rcwrite RUN_DT: 1800
      rcwrite
      rcwrite NX: 4
      rcwrite NY: 6
      rcwrite 
      rcwrite INPUT_GRIDNAME: PE2880x1440-DE
      rcwrite INPUT_IM: 2880
      rcwrite INPUT_JM: 1440
      rcwrite INPUT_LM: 1
      rcwrite 
      rcwrite OUTPUT_GRIDNAME: \$out_gridname
      rcwrite OUTPUT_LM: 1
      rcwrite OUTPUT_IM: \$out_im
      rcwrite 
      rcwrite OUTPUT_TILING_FILE: \$out_tiling_file
      rcwrite 
      rcwrite INPUT_FILE:  \$input_bcs
      rcwrite OUTPUT_FILE: \$output

      unset echo
      echo
      echo  Converting \$var for year \$yyyy to output resolution: \$resX \$resY
      echo "-------------------------------------------------------"
      set echo
      set output_list = ( \$output_list \$output )

      if (-e \$output) \\rm \$output
      if (-e REGRID_FORCING.rc) \\rm REGRID_FORCING.rc

      ln -s \$rcfile REGRID_FORCING.rc
      mpirun -np 24 \$FVBIN/regrid_forcing.x

      if (\$status) \@ errcnt++
      if (! -e \$output) \@ errcnt++
      if (-z \$output) \@ errcnt++
   end
end

if (\$errcnt) then
   set logfileF = \$logfile.FAILED
   qalter -o \$logfileF \$SLURM_JOBID
else
   qalter -o \$logfile \$SLURM_JOBID
   foreach file ( \$output_list )
      set outfile = \$outdir/\$file
      set previous = \$outfile.previous
      if (-e \$outfile) then
      if (-e \$previous) \\rm \$previous
         \\mv \$outfile \$previous
      endif
      \\mv \$file \$outdir
   end
   cd \$outdir
   \\rm -r \$workdir
endif
EOF
;
    close JF;
    select $FH;

    chdir($outdir);
    $cmd = "sbatch $jobfile";
    print "$cmd\n";
    system($cmd);        
}

#================================================================================
# name - usage
# purpose - 
#================================================================================
sub usage {
    use File::Basename qw(basename);
    my $script = basename $0;
    print <<"EOF";

usage: $script yyyy var oRes var [options]

input arguments (see Notes)
    yyyy            year
    var             variable to regrid; SST, ICE, or ALL
    oRes            output resolution: C90, C180, C360, C720, or ALL

options
    -d  outdir      directory for output log and data files; default="."
    -gid groupID    group account ID for job script
    -wa workarea    directory where the workdir will be created; default=\$NOBACKUP
    -np             no prompt; take defaults
    -h              print usage information

Notes:
1. Multiple var or oRes values can be given, separated by commas, with no spaces (see examples)
2. The var and oRes input arguments are case insensitive
3. The job script and logfile are written to the output directory
4. Job scripts are reusable without having to rerun the $script script

Examples:
1. Calculate 2017 SST cube BCs for the C180 grid; put output in the local directory
   > $script 2017 sst c180

2. Calculate 2016 SST cube BCs for the C180 and C360 grids; put output in the local directory
   > $script 2016 sst c180,c360

3. Calculate 2016 ICE and SST BCs for all grids; put output in the specified directory
   > $script 2016 ice,sst c90,c180,c360,c720 -d \$NOBACKUP/cube_BCs   
   or 
   > $script 2016 all all -d \$NOBACKUP/cube_BCs   

EOF
;
    exit;
}
