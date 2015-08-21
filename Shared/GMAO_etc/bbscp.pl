#!/usr/bin/env perl
########################################################################
#
# name: bbscp.pl
#
# purpose - This is a wrapper script for the bbscp command.
#
# Notes:
# 1. This script passes all relevant runtime arguments to the bbscp
#    command.
# 2. One exception to Note #1 above is that the -N flag is ignored if
#    it is present (see Note #3).
# 3. This script dynamically determines whether or not the -N flag
#    should be sent to the bbscp command based on the following:
#
#         -N is included ....... if target is a filename
#         -N not included ...... if target is a directory
#
# 4. The -z flag is hard-coded so that it is always sent to bbscp
#
# !Revision History
#
# 23Apr2007   Stassi    Initial version of code.
#
########################################################################
use strict;
use warnings;

# bbscp flags and bbftp options
#------------------------------
my ($bbftp_exec,$dryrun,$help,$keep,$xVerbose);
my ($rename,$version,$sndsize,$recsize,$suppress);
my ($range,$SERVcmd,$SSHcmd,$numstreams,$bbftprc);
my ($numtries,$timestamp,$verbose,$warnings);

# other global variables
#-----------------------
my ($target,@files2copy);
my ($target_is_remote,$target_is_a_file);
my ($bbscp_flags,$bbscp_cmd);

# local variable
#---------------
my $WD;

# main program
#-------------
{
    &get_runtime_flags;
    &is_target_remote;
    &is_target_a_file;
    &construct_bbscp_flags;
    &construct_bbscp_cmd;

    print "$bbscp_cmd\n";
    system "$bbscp_cmd";
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub get_runtime_flags {
    use Getopt::Long;
    Getopt::Long::Configure("no_ignore_case");
    my $arg;

    # cycle through all args looking for -D option
    #---------------------------------------------
    for (0..$#ARGV) {
	$arg = shift @ARGV;
	if ($arg =~ /^\-D.*/) {
	    $range = $arg;
	} else {
	    push @ARGV, $arg;
	}
    }

    # get other flags and options
    #----------------------------
    GetOptions(

	       # bbscp flags
	       #------------
	       "B=s" => \$bbftp_exec,
	       "d"   => \$dryrun,
	       "h"   => \$help,
	       "k"   => \$keep,
	       "l"   => \$xVerbose,
	       "N"   => \$rename,   #  *** this flag is ignored ***
	       "v"   => \$version,
	       "X=i" => \$sndsize,
	       "Y=i" => \$recsize,
	       "z"   => \$suppress, #  *** hard-coded below ***

	       # bbftp options
	       #--------------
	       "E=s" => \$SERVcmd,
	       "L=s" => \$SSHcmd,
	       "p=i" => \$numstreams,
	       "R=s" => \$bbftprc,
	       "r=i" => \$numtries,
	       "t"   => \$timestamp,
	       "V"   => \$verbose,
	       "w"   => \$warnings);

    # hard-code -z flag to be always on
    #----------------------------------
    $suppress = 1;
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub is_target_remote {
    my ($numargs);

    # check number of arguments
    #--------------------------
    $numargs = scalar(@ARGV);
    if ($numargs < 2) {	die "Error - no destination specified;" };

    # is target remote?
    #------------------
    $target = pop @ARGV;
    $target_is_remote = 0;
    if ($target =~ ":") { $target_is_remote = 1 };

    # the remaining arguments are the files to copy (perhaps just one)
    #-----------------------------------------------------------------
    @files2copy = @ARGV
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub is_target_a_file {
    my ($machine,$remote_target);
    my ($line,$pfield,@dummy);
    my $status;

    # default is target as a file
    #----------------------------
    $target_is_a_file = 1;

    if ($target_is_remote) {

	# check to see if target is a remote directory?
	#----------------------------------------------
	($machine,$remote_target) = split /:/, $target;
	$status = system "ssh $machine ls -ld $remote_target >& /dev/null";
	unless ($status) {
	    chomp($line = `ssh $machine ls -ld $remote_target`);
	    ($pfield,@dummy) = split /\s/, $line;
	    $target_is_a_file = 0 if ($pfield && $pfield =~ /^d.*/);
	}
    
    } else {

	# check to see if target a local directory?
	#------------------------------------------
	$target_is_a_file = 0 if -d $target;
    }

    # check for copying multiple files to a single file
    #--------------------------------------------------
    if ($target_is_a_file && ( scalar(@files2copy) > 1 ) ) {
	die "$0: Error - '$target' is not a directory;";
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub construct_bbscp_flags {
    my $flags = "";

    # the following flags are passed directly to bbscp
    #-------------------------------------------------
    if ( $bbftp_exec ) { $flags = $flags . "-B $bbftp_exec " };
    if ( $dryrun )     { $flags = $flags . "-d " };
    if ( $help )       { $flags = $flags . "-h " };
    if ( $keep )       { $flags = $flags . "-k " };
    if ( $xVerbose )   { $flags = $flags . "-l " };
    if ( $version )    { $flags = $flags . "-v " };
    if ( $sndsize )    { $flags = $flags . "-X $sndsize " };
    if ( $recsize )    { $flags = $flags . "-Y $recsize " };
    if ( $suppress )   { $flags = $flags . "-z " };
    if ( $range )      { $flags = $flags . "$range " };

    if ( $SERVcmd )    { $flags = $flags . "-E $SERVcmd " };
    if ( $SSHcmd )     { $flags = $flags . "-L $SSHcmd " };
    if ( $numstreams ) { $flags = $flags . "-p $numstreams " };
    if ( $bbftprc )    { $flags = $flags . "-R $bbftprc " };
    if ( $numtries )   { $flags = $flags . "-r $numtries " };
    if ( $timestamp )  { $flags = $flags . "-t " };
    if ( $verbose )    { $flags = $flags . "-V " };
    if ( $warnings )   { $flags = $flags . "-w " };

    # the -N flag depends on whether the target is a file or directory
    #-----------------------------------------------------------------
    if ( $target_is_a_file ) { $flags = $flags . "-N " };

    # set global variable
    #--------------------
    $bbscp_flags = $flags;
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub construct_bbscp_cmd {
    my ($BBSCP,$bbscp);

    $BBSCP = "bbscp";
    chomp( $bbscp = `which $BBSCP` );
    die "$0: Error - cannot find the $BBSCP command;" unless ( $bbscp );

    $bbscp_cmd = "$bbscp $bbscp_flags" . "@files2copy $target";
}
