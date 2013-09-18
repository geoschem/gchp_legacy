#!/usr/bin/perl
#
#                  GEOS DAS specific implementation.
#
# This utility asserts whether the system configuration is appropriate
# to build the system in question. For example, it can verify whether
# an appropiate set of modules is loaded. Since by design this script
# is meant to be very dependent on the model being built, the default
# provided in the Config/ directory is just a place holder.
# The recommended usage is:
#
# % gmake install ASSERT=my_own_assert_script
#
# where "my_own_assert_script" is the user implementation.
#
# The script should return one of the following exit codes:
#   = 0  if all is well
#   > 0  if Assert test fails
#   < 0  if Assert test cannot be performed
#
# If the test fails, then a detailed explanation of what the problem
# is should be printed to STDERR.
#
# !REVISION HISTORY
#  03Feb2006  da Silva  Design and prototype implementation.
#  01Jun2006  Stassi    check $BASEDIR and modules
#  06Jul2006  Stassi/RT updated baselibs to Mirvis version (v2_2r2.34)
#  30Aug2006  Stassi    Added verbose mode and ignore option
#  29Sep2006  Stassi    Corrected IGNORE_ASSERT option
#  12Oct2006  Stassi    Reformatted some of the output messages
#  24Oct2006  Stassi    Use Atanas's BASEDIR on columbia
#  09Jan2006  Kokron    Updated for discover.
#  16Mar2007  Kokron    Updated MPI version for discover; added
#                       suport for batch compilation
#-----------------------------------------------------------------------
use strict;

# global variables
#-----------------
my ($scriptname, $g5_modules);
my ($BASEDIR_local, $BASEDIR_expected);
my (@modules_local, @modules_expARR);
my (%found_expected);
my ($node, $errcnt);
my ($verbose);

# main program
#-------------
{
    &init();
    &get_local_env_values();
    &get_expected_values();
    &check_environment();
    &summary_and_exit();
}

#============================================================================
#  name - init
#  purpose - initialize values: $scriptname, $errcnt, and $node
#
#============================================================================
sub init {
    use File::Basename;   # for basename()
    use FindBin qw($Bin);
    use Getopt::Long;

    $scriptname = basename($0);
    $g5_modules = "$Bin/g5_modules";

    $errcnt = 0;
    chomp ( $node = `uname -n` );

    #*** verbose mode from either command arg or env variable
    GetOptions(-v => \$verbose);
    if ($ENV{"VERBOSE_ASSERT"}) { $verbose = 1 } ;

    #*** exit if IGNORE_ASSERT environment variable set
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #*** NOTE:  This option should only be consciously applied.
    #*** i.e., it is recommended that you not set this automatically.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ($ENV{"IGNORE_ASSERT"}) {
        &warn_and_exit("IGNORE_ASSERT environment variable set");
    }
}

#=========================================================================
#  name - get_local_env_values
#  purpose - get values from the calling environment
#=========================================================================
sub get_local_env_values {

    $BASEDIR_local = $ENV{"BASEDIR"};
    &warn_and_exit("Cannot find environment variable \$BASEDIR")
        unless $BASEDIR_local;

    @modules_local = split(":",$ENV{"LOADEDMODULES"}) if $ENV{"LOADEDMODULES"};
    &warn_and_exit("Cannot find environment variable \$LOADEDMODULES")
        unless @modules_local;
}

#=======================================================================
#  name - get_expected_values
#  purpose - determine which BASEDIR and modules are expected for node
#=======================================================================
sub get_expected_values {
    my ($mods);

    #*** get expected BASEDIR value from g5_modules
    chomp($BASEDIR_expected = `$g5_modules basedir`);
    &warn_and_exit("basedir not found in g5_modules for $node")
        unless $BASEDIR_expected;

    #*** get expected modules from g5_modules
    chomp($mods = `$g5_modules modules`);
    &warn_and_exit("No modules found in g5_modules for $node")
        unless $mods;
    @modules_expARR = split / /, $mods;

    #*** initialize found_expected hash to zero (i.e. not found yet)
    $found_expected{"BASEDIR"} = 0;
    foreach (@modules_expARR) {	$found_expected{$_} = 0 };
}

#=========================================================================
#  name - check_environment
#  purpose - get values from the calling environment
#=========================================================================
sub check_environment {
    my ($expVAL, $mod);

    if ($BASEDIR_local eq $BASEDIR_expected) { $found_expected{"BASEDIR"} = 1; }
    else                                     { $errcnt++ ;}

    foreach $mod (@modules_expARR) {
        if (&found($mod,@modules_local)) { $found_expected{$mod} = 1; }
        else                             { $errcnt++; }
    }
}

#=========================================================================
#  name - found
#  purpose - check whether $val is found in @arr
#=========================================================================
sub found {
    my ($val,@arr) = @_;
    my $found = 0;

    foreach (@arr) {
	if ($val eq $_) { 
	    $found = 1;
	    last;
	}
    }
    $found;
}


#=========================================================================
#  name - warn_and_exit
#  purpose - print a warning message, if in verbose mode, and exit
#=========================================================================
sub warn_and_exit {
    my $msg = shift @_;

    if ($verbose) {
        print STDERR "\n$scriptname: >>> WARNING <<< "
                .        "No environment check performed; $msg\n";
    }
    print "-1\n";
    exit -1;
}

#=========================================================================
#  name - summary_and_exit
#  purpose - print summary of what has and has not been found, and then exit
#=========================================================================
sub summary_and_exit {
    my $mod;

    if ($errcnt) {

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        # expected values not found #
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~#
	print STDERR "\n"."="x(41+length($scriptname)) ."\n";
	print STDERR "$scriptname: ERROR found in compilation environment\n";

	#*** print local environment values found
	print STDERR "="x(34+length($node)) ."\n";
	print STDERR "Values found in local $node environment\n";
	print STDERR "="x(34+length($node)) ."\n";
	print STDERR " > $BASEDIR_local\n";
	foreach $mod (@modules_local) {
	    print STDERR " > $mod\n";
	}

	#*** print expected environment values
	print STDERR "="x(21+length($g5_modules)) ."\n";
	print STDERR "Expected values from $g5_modules\n";
	print STDERR "="x(21+length($g5_modules)) ."\n";
	if ($found_expected{"BASEDIR"}) {
	    print STDERR " * (FOUND)     $BASEDIR_expected\n";
	} else {
            print STDERR " - (not found) $BASEDIR_expected\n";
	}
	foreach $mod (@modules_expARR) {
	    if ($found_expected{$mod}) {
		print STDERR " * (FOUND)     $mod\n";
	    } else {
		print STDERR " - (not found) $mod\n";
	    }
	}

    } else {

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        # Assert test was successful #
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        if ( $verbose ) {
            print STDERR "\n$scriptname: Environment check successful.\n";
        }
    }
    print "$errcnt\n";
    exit $errcnt;
}
