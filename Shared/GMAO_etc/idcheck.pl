#!/usr/bin/env perl
#=======================================================================
# name - checkid.pl
# purpose - this script checks whether an experiment ID exists in the SemperPy
#           databases or in the Intranet plot directory for geos5 modeling.
# Note:
# see usage() for input flag and parameter information and return status
#
# !Revision History
# -----------------
# 29Aug2013  Stassi    Initial version
#=======================================================================
use warnings;
use strict;

# global variables
#-----------------
my ($expid, $status, $rem_acct);
my ($noprompt, $quiet, $verbose, $DBcheck, $PLcheck);

# main program
#-------------
{
    init();
    check_semperpy_dbs() if $DBcheck;
    check_plots_dir()    if $PLcheck;
    exit $status;
}

#=======================================================================
# name - init
# purpose - get runtime options and parameter; set global check flags;
#           initialize return status
#=======================================================================
sub init {
    use Getopt::Long;
    my ($checkFLG, $help);

    # runtime options
    #----------------
    GetOptions( "check=i" => \$checkFLG,
                "rem=s"   => \$rem_acct,
                "np"      => \$noprompt,
                "q"       => \$quiet,
                "v"       => \$verbose,
                "h"       => \$help );
    usage() if $help;

    # runtime parameter
    #------------------
    $expid = shift @ARGV;
    usage() unless defined($expid);

    # set check flags
    #----------------
    $checkFLG = 3 unless $checkFLG;
    $DBcheck = 1 unless $checkFLG == 2;
    $PLcheck = 1 unless $checkFLG == 1;

    # initialize return status
    #-------------------------
    $status = 0;
}

#=======================================================================
# name - check_semperpy_dbs
# purpose - look for expid in SemperPy logical databases
#=======================================================================
sub check_semperpy_dbs {
    my ($SHARE, $modsDIR);
    my ($user, $pgpass, $PGP, $removeAfterCheck);
    my ($spy_sql, $bindir);
    my (@ldb_found, $which_pdb, $physical_db, $logical_db);
    my ($cmd, $psql_cmd, $select_results, $msg, $ess, $list);

    # load SemperPy module
    #---------------------
    $SHARE = "/discover/nobackup/projects/gmao/share";
    $modsDIR = "$SHARE/dasilva/lib/python/gmaopy/modules";

    unless (-e $modsDIR) {
        myprint("Warning: Unable to check SemperPy DB info");
        $status = 99 unless $status == 1;
        return;
    }
    $ENV{"MODULEPATH"} .= ":${modsDIR}";
    do "/usr/share/modules/init/perl";
    module ("load semperpy/1.90");

    # write .pgpass, if it does not already  exist
    #---------------------------------------------
    $user = "gmao_user";
    $pgpass = "$ENV{HOME}/.pgpass";
    unless (-e $pgpass) {
        myprint("Writing temporary password file: $pgpass");
        open $PGP, "> $pgpass" or die "Error. Unable to write $pgpass\n";
        print $PGP "dpdb:*:*:$user:hemperUI9#\n";
        close $PGP;
        system "chmod 600 $pgpass";
        $removeAfterCheck = 1;
    }

    # find SemperPy bin directory
    #----------------------------
    chomp($spy_sql = `which spy_sql`);
    chomp($bindir = `dirname $spy_sql`);

    # look for expid in each logical database
    #----------------------------------------
    @ldb_found = ();
    foreach $logical_db qw( fc_exp fc_ops im_exp im_ops ) {
        chomp($which_pdb = `python $bindir/whichdb.py $logical_db`);
        $physical_db = (split /\s+/, $which_pdb)[0];

        $cmd = "select distinct expver from $logical_db.stats";
        $psql_cmd = "psql -w -q -U $user -h dpdb $physical_db -c \"$cmd\"";
        print "$psql_cmd\n" if $verbose;
        $select_results = `$psql_cmd`;

        if ($select_results) {
            push @ldb_found, $logical_db if $select_results =~ m/\s$expid\s/;
        }
        else {
            $msg = "Warning: Unable to access SemperPy DB info"
                .  " (check password file contents: $ENV{HOME}/.pgpass)";
            myprint("\n$msg");
            $status = 99 unless $status == 1;
            return;
        }
    }

    if (@ldb_found) {
        if (scalar(@ldb_found) > 1) { $ess = "s" }
        else                        { $ess = ""  }

        $list = $ldb_found[0];
        for (1..$#ldb_found) { $list .= ", $ldb_found[$_]" }
        myprint("FOUND: Experiment ID \"$expid\" in SemperPy database$ess: $list");

        $status = 1;
    }
    else { myprint("Experiment ID \"$expid\" not found in SemperPy databases") }

    if ($removeAfterCheck) {
        myprint("Removing temporary password file: $pgpass");
        unlink $pgpass;
    }
}

#=======================================================================
# name - check_plots_dir
# purpose - look for expid subdirectory in the geos5 modeling Intranet
#           plot directory
#=======================================================================
sub check_plots_dir {
    my ($geos5dir, $cmd, $ssh_cmd, $check);

    remote_account_info();

    # check for access to Intranet plot directory
    #--------------------------------------------
    $geos5dir = "/gmao/intranet/research/modeling/agcm/geos5";
    $cmd = "/usr/bin/tcsh -c 'if (-e $geos5dir) echo yes'";
    $ssh_cmd = "ssh -q -oBatchMode=yes $rem_acct \"$cmd\"";
    print "$ssh_cmd\n" if $verbose;
    chomp($check = `$ssh_cmd`);

    unless ($check eq "yes") {
        myprint("Warning: Unable to access Intranet plot directory");
        $status = 99 unless $status == 1;
        return;
    }

    # look for $expid as subdirectory
    #--------------------------------
    $cmd = "/usr/bin/tcsh -c 'if (-e $geos5dir/$expid) echo yes'";
    $ssh_cmd = "ssh -q -oBatchMode=yes $rem_acct \"$cmd\"";
    print "$ssh_cmd\n" if $verbose;
    chomp($check = `$ssh_cmd`);

    if ($check eq "yes") {
        myprint("FOUND: Experiment ID \"$expid\" in Intranet plot directory: " .
                "$geos5dir/$expid/");
        $status = 1;
    }
    else {
        myprint("Experiment ID \"$expid\" not found in Intranet plot directory");
    }
}

#=======================================================================
# name - remote_account_info
# purpose - set the remote account information needed for accessing
#           the Intranet plot directory
#
# Note:
# 1. set to values to defaults if in no-prompt mode
#=======================================================================
sub remote_account_info {
    my ($remNODE, $remID);
    my ($dflt, $dflt_node, $dflt_user);

    # if $rem_acct was given, then extract $remID and $remNODE
    #---------------------------------------------------------
    if ($rem_acct) { ($remID, $remNODE) = split /\@/, $rem_acct }

    # default values
    #---------------
    $dflt_node = "train";
    $dflt_user = $ENV{"USER"};

    # query for remote node, if not given
    #------------------------------------
    $dflt = $dflt_node;
    until ($remNODE and ($remNODE eq "polar" or $remNODE eq "train")) {
        if ($noprompt) { $remNODE = $dflt; last }

        print "Remote machine for Intranet plots (train or polar) [$dflt]: ";
        chomp($remNODE = <STDIN>);
        $remNODE = $dflt unless $remNODE;
    }

    # query for remote ID, if not given
    #----------------------------------
    $dflt = $dflt_user;
    unless ($remID) {
        if ($noprompt) { $remID = $dflt unless $remID }
        else {
            print "Remote ID on $remNODE [$dflt]: ";
            chomp($remID = <STDIN>);
            $remID = $dflt unless $remID;
        }
    }
    $rem_acct = "$remID\@$remNODE";
}

#=======================================================================
# name - myprint
# purpose - print string to STDOUT unless in quiet mode
#=======================================================================
sub myprint {
    my ($str) = shift @_;
    print("$str\n") unless $quiet;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my ($name);
    $name = basename $0;
    print << "EOF";

description:
this script checks whether an experiment ID exists in the SemperPy databases
or in the geos5 modeling Intranet plot directory.

usage: $name [options] expid
where expid is the experiment ID to check

options
 -check flag     Flag indicating what to check
                   =1  => check SemperPy databases
                   =2  => check geos5 modeling Intranet plot directory
                   =3  => check both (default)
 -rem            Remote account to use for accessing Intranet plot directory
                 default: $ENV{"USER"}\@train
                 (remote machine must be either "polar" or "train")
 -np             Take default for \$rem_acct if not provided; Otherwise,
                 the script will prompt for the info
 -q              quiet mode; suppress warning and info messages
 -v              verbose mode; show psql and ssh commands
 -h              print usage information

return status
  =0     expid was not found
  =1     expid was found
  =99    expid was not found, but problems were encountered during query

EOF
exit;
}
