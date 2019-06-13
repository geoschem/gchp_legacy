package GMAO_utils;
#########################################################################
#
# !Revision History
#
# 28Jul2006  Stassi    Package added to CVS repository.
# 09Aug2006  Stassi    Added option to suppress zeit output in System()
#
#########################################################################
use strict;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( Assignfn get_siteID pbs_env System );

#-----------------------------------------------------------------------
#  name - Assignfn
#  purpose - Assigns fn to given file name fname.
#
#  Notes:
#    fname = old file
#       fn = new file (links to old)
#-----------------------------------------------------------------------
sub Assignfn {
    my ( $fname, $fn ) = @_;
    unlink($fn) if ( -e $fn ) ;
    symlink("$fname","$fn");
}

#-----------------------------------------------------------------------
# name - get_siteID
# purpose - identify the processing platform
#
# input parameter
# => $flag:  =0  return all NAS nodes as "nas"
#            =1  return NAS nodes as either "cfe" or "pfe"
#-----------------------------------------------------------------------
sub get_siteID {
    my ($flag, $site, $siteID);
    $flag = shift @_;

    chomp($site = `uname -n`);

    # GMAO desktop
    # ------------
    $siteID = "gmao" if $site =~ /gsfc/;
    $siteID = "gmao" if $site =~ /^gs6101/;
    $siteID = "gmao" if $site =~ /.*gsfc\.nasa\.gov$/;
    $siteID = "gmao" if $site =~ /.*ndc\.nasa\.gov$/;

    # NAS sites
    #----------
    if ($site =~ /^cfe/) {
        if ($flag) { $siteID = "cfe" }
        else       { $siteID = "nas" }
    }
    if ($site =~ /^pfe/) {
        if ($flag) { $siteID = "pfe" }
        else       { $siteID = "nas" }
    }
    $siteID = "nas"  if $site =~ /^r\d+i\d+n\d+/;

    # NCCS sites
    #-----------
    $siteID = "nccs" if $site =~ /^discover/;
    $siteID = "nccs" if $site =~ /^borg/;
    $siteID = "nccs" if $site =~ /^dali/;

    # send back node name for all others
    #-----------------------------------
    $siteID = $site unless $siteID;

    return $siteID;
}
    
#-----------------------------------------------------------------------
# name - pbs_env
# purpose - get number of ranks (cpus) per node in local environment
#
# input
# => $flag :  if = "num_nodes", then return number of nodes in $PBS_NODEFILE
#             if = "cpus_per_node", then return number of cpus per node
#-----------------------------------------------------------------------
sub pbs_env {
    my ($flag, $siteID, $retval, $nodefile, $cpuinfo, %cnt);

    unless ($ENV{PBS_JOBID}) {
        warn "WARNING; not in PBS Environment;";
        return;
    }

    # check for correct input
    #------------------------
    $flag = shift @_;

    die "Error; no flag specified" unless $flag;
    unless (($flag eq "num_nodes") or ($flag eq "cpus_per_node")) {
        die "Error; incorrect flag: $flag;";
    }

    # num nodes in $PBS_NODEFILE
    #---------------------------------
    if ($flag eq "num_nodes") {
        $nodefile = $ENV{PBS_NODEFILE};
        die "Error; \$PBS_NODEFILE not defined;" unless $nodefile;
        die "Error; $nodefile not found" unless -e $nodefile;

        open NODEFILE, "< $nodefile" or die "Error opening $nodefile;";
        foreach (<NODEFILE>) { $cnt{$_}++ }
        close NODEfile;

        $retval = scalar(keys %cnt);
    }        

    # cpus per node
    #--------------
    elsif ($flag eq "cpus_per_node") {
        $siteID = get_siteID();

        if ($siteID eq "nccs") {
           $cpuinfo = "/proc/cpuinfo";
            last unless -e $cpuinfo;

            $retval = 0;
            open CPUINFO, "< $cpuinfo" or die "Error opening $cpuinfo;";
            foreach (<CPUINFO>) { $retval++ if /processor/ }
            close CPUINFO;
        }

        elsif ($siteID eq "nas") {
            $nodefile = $ENV{PBS_NODEFILE};
            die "Error; \$PBS_NODEFILE not defined;" unless $nodefile;
            die "Error; $nodefile not found" unless -e $nodefile;

            open NODEFILE, "< $nodefile" or die "Error opening $nodefile;";
            foreach (<NODEFILE>) { $cnt{$_}++ }
            close NODEfile;

            foreach (keys %cnt) { $retval = $cnt{$_}; last }
        }
        $retval = 0 unless $retval;
    }
    return $retval;
}

#-----------------------------------------------------------------------
#  name - System
#  purpose -
#
#-----------------------------------------------------------------------
sub System {
    use FindBin qw($Script);
    my ($rc,$rc1,$rc2,$exit_code);
    my ($error_code);
    my ($trace,$fvwork);
    my @zname;

    # calling parameters
    #-------------------
    my ( $cmd, $logfile, $xname, $quietzeit ) = @_;

    # set values
    #-----------
    $error_code = 6;
    $trace = $Script . "->GMAO_utils::System()";
    $fvwork = $ENV{"FVWORK"};
    unless ($fvwork) {
	warn "${trace}: >>> ERROR <<< \$FVWORK must be defined "
	    . "before calling this subroutine.\n";
	return $error_code;
    }

    # save STDOUT and STDERR before redirecting
    #------------------------------------------
    open SAVEOUT, ">&STDOUT";  # save stdout
    open SAVEERR, ">&STDERR";  # save stderr

    # DUMMY print statements to suppress WARNING messages
    #----------------------------------------------------
    print SAVEOUT "";
    print SAVEERR "";

    # redirect STDOUT and STDERR to $logfile
    #---------------------------------------
    unless (open STDOUT, ">>$logfile") {
        warn "${trace}: >>> ERROR <<< while redirecting STDOUT\n";
	return $error_code;
    }
    unless (open STDERR, ">>$logfile") {
        warn "${trace}: >>> ERROR <<< while redirecting STDERR\n";
	return $error_code;
    }

    select STDERR; $| = 1;     # make it unbuffered
    select STDOUT; $| = 1;     # make it unbuffered

    @zname = split(" ", $cmd);
    unless ($quietzeit) {
	if ( $xname ) {
	    $rc1 = system( "zeit_ci.x -r $fvwork/.zeit $xname");
	} else {
	    $rc1 = system( "zeit_ci.x -r $fvwork/.zeit $zname[0]");
	}
    }

    $rc = system ( $cmd );     # run the shell command

    unless ($quietzeit) {
	if ( $xname ) {
	    $rc2 = system( "zeit_co.x -r $fvwork/.zeit $xname");
	} else {
	    $rc2 = system( "zeit_co.x -r $fvwork/.zeit $zname[0]");
	}
    }

    # Bitwise shift returns actual UNIX return code
    #----------------------------------------------
    $exit_code = $rc >> 8;

    close STDOUT;
    close STDERR;

    open STDOUT, ">&SAVEOUT" ;  # restore stdout
    open STDERR, ">&SAVEERR" ;  # restore stderr

    return $exit_code;
}

1;
