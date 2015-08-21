package Run_parallel;
#########################################################################
#
# name - Run_parallel
#
# purpose -
#   This package can be used to call a perl subroutine multiple times
#   in parallel.
#
# Notes:
#  1. The &init_parallel() sub must be called prior to calling the other
#     Run_parallel routines.
#  2. To call the &parallel() sub:
#     - Send the name of the perl subroutine you wish to run in
#       parallel as an input parameter along with an array of values
#       for which the subroutine will be called in parallel. The
#       subroutine name and the array must be sent as pointers. The
#       subroutine will be called multiple times, once for each value
#       in the array.
#     - Additional "non-parallel" parameters can be included in an
#       another array which can be sent directly, i.e. not as a pointer.
#       All of these parameters will be included in each parallel call.
#     - The subroutine must be able to accept and run with a single
#       parallel parameter value, along with the addition non-parallel
#       parameters.
#     - The subroutine calls will be made in parallel up to the
#       maximum number of allowable CPUs.
#     - See example below.
#  3. Within the subroutine, call the &halt() sub if a fatal error is detected.
#  4. Call the wrapup_parallel() sub after the final call to &parallel().
#
#  Example:
#    The following lines of code will call the perl subroutine, &subname(),
#    once for each value in the @vals array, and run the instances in parallel
#    over 4 processors.
#
#    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    $NCPUS = 4;
#    &init_parallel( $NCPUS, $workdir );
#    &parallel( \&subname, \@parallelParam, @nonparallelParams );
#    &wrapup_parallel();
#    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Notes on Load Balancing:
#  1. If load balancing is turned on, the parent process will
#     constantly monitor each child process to reallocate an idle
#     CPU as soon as the associated child process completes.
#  2. If load balancing is turned off, the parent process will only
#     check the oldest running child process and will not detect if
#     other children processes have completed.
#  3. Load balancing will yield the greatest benefit when there is a
#     high degree of variability in time used by the individual child
#     processes.
#  4. Load balancing uses more resources since the parent process
#     constantly monitors all of the children processes rather than
#     sitting idle waiting for the oldest child process to complete.
#     However, this is minimized by restricting the parent to check
#     each child only once per second.
#  5. If the number of children is less than MAX, then load balancing
#     will have no impact on time or resources.
#  6. By default, load balancing is turned on.  Subroutines are
#     available for turning load balancing off or on; see
#     load_balancing_off() and load_balancing_on()
#
# !Revision History
#
# 28Jul2006  Stassi    Package added to CVS repository.
# 07Dec2006  Owens/JS  Added "sleep 1" after each forked job to avoid conflicts
# 19Jan2006  Stassi    Added load balancing.
#
#########################################################################
use strict;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( init_parallel parallel halt wrapup_parallel );

my $bottle;
my $MAX;
my $balance_load = 1;

#-----------------------------------------------------------------------
#  name - init_parallel
#  purpose - 
#    This subroutine opens a file into which forked jobs can write error
#    messages to send back to the parent process.
#
#  input parameters
#    - $MAX : maximum number of child processes to run in parallel
#    - $workdir : (optional) name of directory where output can be written.
#                 defaults to local directory (i.e. the directory that is
#                 local when init_parallel() is called).
#
# --------------------------------------------------------------------
sub init_parallel {
    use Cwd qw(cwd);
    use FindBin qw($Script);
    my $workdir;
    my $subname = "Run_parallel::init_parallel()";

    # input parameters
    #-----------------
    $MAX = shift(@_);
    $workdir = shift(@_);

    if ($MAX < 1) {
        die ">>> ERROR <<< $subname; Max numbers of processes must be >= 1.";
    }
    if ($MAX == 1) {
        warn ">>> WARNING <<< $subname; Max number of processes equals 1."
            . "  No parallelization will be performed."
    }
    ($workdir) || ($workdir = &cwd);
    
    # open bottle file
    #-----------------
    $bottle = "$workdir/.parallel.$Script.forked_errmsgs.$$";
    die ">>> ERROR << $subname; file already exists: $bottle\n" if (-e $bottle);

    open BOTTLE, "> $bottle"
        or die ">>> ERROR <<< cannot open BOTTLE, $bottle: $!";
}

#-----------------------------------------------------------------------
#  name - parallel
#  purpose -
#    This subroutine receives a perl subroutine name and an array of values
#    (e.g. hours to be processed). It creates multiple instances of the call
#    to the subroutine, one for each value, and runs them simultaneously in
#    the background, up to the allowable number of CPUs.  It waits for all
#    jobs to complete before returning control back to the calling routine.
#
#  input parameters
#    - $subref:  pointer to the perl program subroutine to call
#    - $arrptr:  pointer to array of values
#    - @params:  other parameters to include with each subroutine call
#
#  Note:
#  * Only the parent process will return control back to the calling
#    routine.  Therefore, all the children processes must be cut loose
#    after they complete their task.
#-----------------------------------------------------------------------
sub parallel {
    use POSIX ":sys_wait_h";

    my ($subref,$fhr);
    my ($newpid,$pid);
    my ($status,$check_counter);
    my ($NCPUS);
    my ($arrptr,@params);
    my @values;
    my @pidArr;


    unless ($MAX) {
        die ">>> ERROR <<< the subroutine init_parallel() must be called ".
            "prior to calling the subroutine parallel().";
        }

    $subref = shift(@_);
    $arrptr = shift(@_);
    @values = @$arrptr;
    @params = @_;

    foreach $fhr ( @values ) {

        # check for maximum number of forked jobs
        #----------------------------------------
	if ($balance_load) {

	    # Load Balancing ON
	    #------------------
	    while (scalar(@pidArr) >= $MAX) {
		$check_counter = 0;

		while (1) {
		    $pid = shift(@pidArr);
		    $status = waitpid($pid,WNOHANG);
		    if ($status > 0) { last; }

		    push (@pidArr,$pid);
		    $check_counter++;

		    # take one second breather after checking each child
		    #---------------------------------------------------
		    if ($check_counter >= $MAX) {
			system ("sleep 1");
			$check_counter = 0;
		    }
		}
	    }
	} else {

	    # Load Balancing OFF
	    #-------------------
	    while (scalar(@pidArr) >= $MAX) {
		$pid = shift(@pidArr);
		waitpid($pid,0);
	    }
	}

        # fork job
        #---------
        defined($newpid=fork)
            or die ">>> ERROR <<< unable to fork: ${$subref} $fhr: $!";
        unless ($newpid) {

            #-------------------------------------#
            # child process makes subroutine call #
            #-------------------------------------#
                                                  #
            &$subref($fhr,@params);               #
            exit;                                 #
                                                  #
            #-------------------------------------#
            # child process task complete         #
            #-------------------------------------#
        }
        push (@pidArr,$newpid);

        # temporary pause to avoid conflicts between children
        #----------------------------------------------------
        system "sleep 1";
    }

    # wait for child processes to complete
    #-------------------------------------
    while (@pidArr) {
        $pid = shift(@pidArr);
        waitpid($pid,0);
    }

    # Check for error messages left by child processes
    #-------------------------------------------------
    &check_bottle;
}

#-----------------------------------------------------------------------
#  name - halt
#  purpose - This routine is called from a forked process when a fatal
#            error condition occurs.  An error message is left in the
#            bottle file for the parent process to find, and then the
#            child process is halted.
#
#  input parameters -
#    $errmsg: error message to print to standard output
#    $exitcode (optional):  non-zero exit code
#
#  Note:
#  * If no exit code is supplied, or if exit code is zero, then the
#    default exitcode=9 will be used.
#-----------------------------------------------------------------------
sub halt {
    my ($errmsg,$exitcode) = @_;
    print BOTTLE "$errmsg\n";
    ($exitcode) || ($exitcode = 9);
    exit($exitcode);
}

#-----------------------------------------------------------------------
#  name - wrapup_parallel
#  purpose - 
#    This subroutine closes and removes the file that was used by forked
#    jobs to send error messages back to the parent process.
# --------------------------------------------------------------------
sub wrapup_parallel {
    close BOTTLE;
    unlink $bottle;
}

#-----------------------------------------------------------------------
#  name - check_bottle
#  purpose - This subroutine checks to see whether any of the forked
#            processes sent an error message back to the parent process
#            (a message in a bottle, so to speak)
#
#  Notes:
#  1. All messages are assumed to be bad news (no news is good news).
#  2. If the bottle file is non-empty, then this subroutine will print
#     the contents to standard output and then exit.
#
#-----------------------------------------------------------------------
sub check_bottle {
    my $subname = "Run_parallel::check_bottle()";

    close BOTTLE;

    if ((-e $bottle) && (! -z $bottle)) {
        print "\n${subname}: Error messages found in $bottle:\n";
        open BOTTLE, "< $bottle";
        while ( <BOTTLE> ) { print $_ };
        close BOTTLE;
        die "Exiting";
    } else {

        # No messages found; re-open bottle for writing
        #-----------------------------------------------
        open BOTTLE, "> $bottle";
    }
}

#-----------------------------------------------------------------------
#  name - load_balancing_on
#  purpose - 
#    This subroutine will direct the parent process to constantly
#    monitor each child process to reallocate an idle CPU as soon as
#    the associated child process completes.
#
#  (see Notes in file prologue)
# --------------------------------------------------------------------
sub load_balancing_on {
    $balance_load = 1;
}

#-----------------------------------------------------------------------
#  name - load_balancing_off
#  purpose - 
#    This subroutine will turn off load balancing, i.e. the parent
#    process will only check the oldest running child process and will
#    not detect if other children processes have already completed.
#
#  (see Notes in file prologue)
# --------------------------------------------------------------------
sub load_balancing_off {
    $balance_load = 0;
}

1;
