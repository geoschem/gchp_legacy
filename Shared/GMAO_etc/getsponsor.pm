package getsponsor;
#=======================================================================
#
# Name: getsponsor.pm
# Purpose: perl package which exports the get_spcode() subroutine
#
# Note: See get_spcode() prologue for more information.
#=======================================================================
use strict;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( get_spcode );

# global variables
#-----------------
my (@GID, @lines, %flags);
my ($dfltFLG, $mapfile, $menu, $quiet, $stderr, $usefirst);
my ($groupID, $user);

#=======================================================================
# name - get_spcode
# purpose - get task groupID either from the getsponsor utility or from
#           a file which maps users to tasks or from the groups command
#
# input parameters
# => %flags: hash containing flags
#    where
#       $flag{"grpID"}    group ID to return if found 
#       $flag{"mapfile"}  name of file containing mapping between users and tasks
#       $flag{"menu"}     print menu only if =1 (true)
#       $flag{"dflt"}     return dflt value without printing menu if =1
#       $flag{"quiet"}    suppress some of the warning messages
#       $flag{"stderr"}   print to STDERR instead of STDOUT 
#       $flag{"usefirst"} use first available group ID as default
#
# return value
# => $groupID
#=======================================================================
sub get_spcode {

    %flags = @_;

    # extract group info into global variables: @GID and @lines
    #----------------------------------------------------------
    init();
    get_info_from_utility();
    get_info_from_groups() unless @GID;
    get_info_from_file()   unless @GID;;

    # if menuflg, then print menu
    #----------------------------
    if ($menu) { foreach (@lines) { print1("$_\n") } }

    # else if dfltFLG or groupID, then get requested value
    #-----------------------------------------------------
    elsif ($dfltFLG or $groupID) {
        $groupID = defaultID();
        warn ">> Warning << groupID has not been set;" unless $groupID;
    }

    # otherwise, prompt for user response
    #------------------------------------
    else {
        prompt_for_groupID();
        warn ">> Warning << groupID has not been set;" unless $groupID;
    }

    # return value
    #-------------
    return $groupID;
}

#=======================================================================
# name - init
# purpose - initialize variables, get runtime parameters
#=======================================================================
sub init {
    use Getopt::Long;
    my $grpID;

    # initializations
    #----------------
    $user = $ENV{"USER"};

    # extract flag values
    #--------------------
    if (%flags) {
        $dfltFLG  = $flags{"dflt"};
        $grpID    = $flags{"grpID"};
        $mapfile  = $flags{"mapfile"};
        $menu     = $flags{"menu"};
        $quiet    = $flags{"quiet"};
        $stderr   = $flags{"stderr"};
        $usefirst = $flags{"usefirst"};
    }

    # initialize $groupID
    #--------------------
    $groupID = "";
    $groupID = $grpID if $grpID;

    # check for mapfile existence
    #----------------------------
    if ($mapfile) {
        die ">> Error << mapfile not found: $mapfile;" unless -e $mapfile;
    }
    else {
        # this was once the default
        #--------------------------
        $mapfile = "/share/fvpsas/etc/MapGIDLoginIDTask.rc";
        $mapfile = "" unless -e $mapfile;
    }
}

#=======================================================================
# name - get_info_from_utility
# purpose - use groupID info from the getsponsor utility
#
# output 
# => values stored in global variables: @lines and @GID
#=======================================================================
sub get_info_from_utility {
    my ($getsponsor, $status, $last, $line, @items);

    # check availability of getsponsor utility
    #-----------------------------------------
    $getsponsor = "xxx";
    $status = system("which getsponsor >& /dev/null");
    chomp($getsponsor = `which getsponsor`) unless $status;

    unless (-x $getsponsor) {
        warn ">> Warning << getsponsor utility not found;" unless $quiet;
        return;
    }

    # get info from getsponsor
    #-------------------------
    chomp(@lines = `$getsponsor`);

    # remove trailing blanks and blank lines
    #---------------------------------------
    $last = scalar(@lines);
    foreach (1..$last) {
        $line = shift @lines;
        $line =~ s/\s*$//;
        push @lines, $line if $line;
    }
    unshift @lines, "";  # add one blank line to top

    foreach $line (@lines) {
        next unless $line;

        # assume first item is valid groupID if it matches correct pattern
        #-----------------------------------------------------------------
        @items = split /\s+/, $line;
        shift @items unless $items[0];
        if (@items) {
            push @GID, $items[0] if $items[0] =~ /\b\w{1,2}\d{3,5}\w{0,1}\b/;
        }
    }

    # warn if no valid tasks found
    #-----------------------------
    warn ">> Warning << no tasks found with getsponsor utility for <$user>;"
        unless @GID;
}

#=======================================================================
# name - get_info_from_groups
# purpose - use groupID info from the groups command
#
# output 
# => values stored in global variables: @lines and @GID
#=======================================================================
sub get_info_from_groups {
    my ($groups, $status, @groupinfo, $line, @items, $item);

    # check availability of groups command
    #-------------------------------------
    $groups = "xxx";
    $status = system("which groups >& /dev/null");
    chomp($groups = `which groups`) unless $status;

    unless (-x $groups) {
        warn ">> Warning << groups command not found;" unless $quiet;
        return;
    }

    # Load a label for first line when prompting user
    #------------------------------------------------
    push @lines, "\nSp Code";
    push @lines,     "-------";

    # get info from groups
    #---------------------
    chomp(@groupinfo = `$groups`);

    foreach $line (@groupinfo) {
        next unless $line;

        # remove trailing blanks
        #-----------------------
        $line =~ s/\s*$//;

        # extract valid groupIDs from each line
        #--------------------------------------
        @items = split /\s+/, $line;
        foreach $item (@items) {
            if ( $item =~ /\b\w{1,2}\d{3,5}\w{0,1}\b/ ) {
                push @GID, $item;
                push @lines, $item;
            }
        }
    }
    push @lines, "";

    # warn if no valid tasks found
    #-----------------------------
    warn ">> Warning << no tasks found with groups command for <$user>;"
        unless @GID;
}

#=======================================================================
# name - get_info_from_file
# purpose - get groupID info from a file which maps users to groupIDs
#
# output 
# => values stored in global variables: @lines and @GID
#=======================================================================
sub get_info_from_file {
    my (@all, $line, $title);
    my ($name, $gid, $desc);

    # return if map file does not exist
    #----------------------------------
    # NOTE: no warning here--see init()
    #----------------------------------
    return unless -e $mapfile;

    # get lines from map file
    #------------------------
    open MAP, $mapfile or die ">> Error << opening $mapfile: $!";
    chomp(@all = <MAP>);
    close MAP;
    @lines = grep { /$user/ } @all;

    # extract group IDs for each valid task
    #--------------------------------------
    foreach $line (@lines) {

        # remove leading/trailing blanks
        #-------------------------------
        $line =~ s/^\s+|\s+$//g;

        # valid groupID, if first item is user name
        #------------------------------------------
        ($name, $gid, $desc) = split /\s+/, $line;
        push @GID, $gid if $name eq $user;
    }

    # warn if no valid tasks found
    #-----------------------------
    warn ">> Warning << no tasks found in $mapfile for <$user>;"
        unless @GID or $quiet;

    # Add title to front of @lines and blank to end
    #----------------------------------------------
    $title = "\nAuthorized GroupIDs for <$user> from $mapfile\n"
        .      "-"x77;
    unshift @lines, $title;
    push @lines, "";
}

#=======================================================================
# name - prompt_for_groupID
# purpose - prompt user for which groupID to use
#=======================================================================
sub prompt_for_groupID {
    my ($default, $cnt, $MAXTRIES, $ans, $found);

    return unless @GID;

    $cnt = 1;
    $MAXTRIES = 3;

    # find default group ID
    #----------------------
    $default = defaultID();

    # query for group ID
    #-------------------
    while (1) {

        # print list of choices
        #----------------------
        foreach (@lines) { print1("$_\n") }

        # get user response
        #------------------
        print1("select group: ");
        print1("[$default] ") if $default;
        chomp($ans = <STDIN> );
        $ans =~ s/\s*//g;
        $groupID = $default unless $groupID = $ans;

        # check validity of response
        #---------------------------
        last if valid($groupID);

        # too many unsuccessful attempts
        #-------------------------------
        if (++$cnt > $MAXTRIES) {
            $groupID = "";
            warn ">> Warning << Invalid input;";
            last;
        }

        # try again
        #----------
        warn "\n\n!!! groupID $groupID not found in list."
            . " Please choose a groupID from list below;";
    }
}

#=======================================================================
# name - defaultID
# purpose - choose a value from @GID to be the default
#=======================================================================
sub defaultID {
    my (@myGIDs, $dflt, $myID, $myIDx, $xx, $gid);

    if (@GID and $usefirst) { $dflt = $GID[0] }
    else                    { $dflt = "" }

    # check supplied gid, then environment variables
    #-----------------------------------------------
    @myGIDs = ();
    if ($groupID)  { push @myGIDs, $groupID  }
    if ($ENV{GID}) { push @myGIDs, $ENV{GID} }
    if ($ENV{gid}) { push @myGIDs, $ENV{gid} }

  outer: foreach $myID (@myGIDs) {
      foreach $gid (@GID) {
          last outer if $dflt;

          # check for equivalency or close match
          #-------------------------------------
          if ($gid eq  $myID ) { $dflt = $gid; last outer }
          if ($gid =~ /$myID/) { $dflt = $gid; last outer }

          # try match after removing leading characters from $myID
          #-------------------------------------------------------
          for $xx (1..2) {
              $myIDx = substr($myID,$xx);
              next unless $myIDx;
              if ($gid =~ /$myIDx/) { $dflt = $gid; last outer }
          }
      }
  }

    # as last resort, pick last value in @GID as default
    #---------------------------------------------------
    unless ($dflt) {
        if (@GID) { $dflt = $GID[-1] }
    }
    return $dflt;
}

#=======================================================================
# name - print1
# purpose - print to STDERR if $stderr; otherwise print to STDOUT
#=======================================================================
sub print1 {
    my $str;

    $str = shift @_;
    if ($stderr) { print STDERR $str }
    else         { print $str }
}

#=======================================================================
# name - valid
# purpose - check whether supplied groupID is valid
#=======================================================================
sub valid {
    my ($grpID, $found);

    $grpID = shift @_;

    $found = 0;
    foreach (@GID) { $found = 1 if $grpID eq $_; }

    return $found;
}

1;
