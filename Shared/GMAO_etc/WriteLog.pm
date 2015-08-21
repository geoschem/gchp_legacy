package WriteLog;
#########################################################################
# name - WriteLog
#
# purpose -
#   This package provides perl command wrappers which will
#   - echo perl command to STDOUT
#   - echo perl command to LOG if logfile has been opened with openLOG()
#
# -----------------------
# subroutine descriptions
# ----------------------------------------
# (see individual prologues for more info)
# ----------------------------------------
#  1. openLOG(): open LOG
#  2. closeLOG(): close LOG
#  3. display(): inverse to realpath_(); gives cleaner pathname for display
#  4. query(): prompts user and returns response
#  5. setprompt(): can tell query() to always return default w/o prompting user
#  6. setverbose(): can turn echo off/on (i.e. quiet mode)
#
# -------------------
# wrapper subroutines
# --------------------------------------------------------------
# - These routine will echo to STDOUT and to LOG if it is open.
# - Most of these routines have an optional input parameter
#   which allows you to turn off the echo for this command.
# --------------------------------------------------------------
#  7. chdir_():
#  8. copy_():
#  9. mkpath_():
# 10. move_():
# 11. print_(): prints to STDOUT and also to LOG, if it is open
# 12. printf_(): formatted print to STDOUT and also to LOG, if it is open
# 13. printLOG_(): prints only to the LOG, if it is open; not to STDOUT
# 14. realpath_(): returns absolute path and saves original input for display
# 15. symlink_():
# 16. system_(): wrapper for system command; returns status
# 17. system1_(): wrapper for system command; dies if error status
# 18. unlink_(): works for either a file or a directory
#
# ------------------------------------
# adding <CR>'s to the front of output
# -------------------------------------------------------------------
# In the following subs, blank lines can be added to the front of
# the output by adding one or more "\n" to the first input parameter.
# -------------------------------------------------------------------
#  1. chdir_()
#  2. copy_()
#  3. mkpath_()
#  4. move_()
#  5. symlink_()
#  6. system_()
#  7. unlink_()
#
# Examples (with assumption, $verbose==1)
# --------
#  copy_($source, $dest) .......... prints a copy message to output
#  copy_("\n$source", $dest) ...... prints one blank line before message
#  copy_("\n\n$source", $dest) .... prints two blank lines before message
#
# !Revision History
# -----------------
# 21Feb2013  Stassi    Initial version of package
#########################################################################
use strict;
use File::Basename;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( openLOG
                     closeLOG
                     display
                     query
                     setprompt
                     setverbose
                     chdir_
                     copy_
                     mkpath_
                     move_
                     print_
                     printf_
                     printLOG_
                     realpath_
                     symlink_
                     system_
                     system1_
                     unlink_ );

# global variables
#-----------------
my ($LOGFILE, $noprompt, $verbose, %display);

# defaults
#---------
$LOGFILE = "";
$noprompt = 0;
$verbose = 1;

#=======================================================================
# name - openLOG
# purpose - open $logfile with file handle LOG;
#           flush LOG buffer after each write
#
# input parameter
# => $logfile: name of logfile to open
# => $new: (optional) == 1 : remove pre-existing file
#                     == 0 : append to pre-existing file (default)
#=======================================================================
sub openLOG {
    my $FH;
    my ($logfile, $new);

    $logfile = shift @_;
    $new = shift @_;

    die "Error; no filename specified for logfile;" unless defined($logfile);
    closeLOG() if $LOGFILE;

    if ($new) { unlink_($logfile, 0) if -e $logfile }

    open LOG, ">> $logfile" or die "Error. Unable to open logfile: $logfile;";
    $LOGFILE = $logfile;

    $FH = select;
    select LOG; $| = 1;
    select $FH;
}    

#=======================================================================
# name - closeLOG
# purpose - close LOG
#=======================================================================
sub closeLOG {
    if ($LOGFILE) {
        if (close LOG) { $LOGFILE = "" }
        else           { print_("Warning. Unable to close "
                                .display($LOGFILE) ."\n") }
    }
}    

#=======================================================================
# name - display
# purpose - substitute a "cleaner" looking pathname for the absolute pathname
#
# Notes:
# 1. This routine essentially is an inverse to the sub realpath_(),
#    but only if the values have already been given to realpath_().
# 2. The return value should be used for display only since perl functions
#    seem to work better with absolute pathnames than with links.
#=======================================================================
sub display {
    my $display_name = shift @_;
    foreach (keys %display) { $display_name =~ s/$_/$display{$_}/ }
    return $display_name;
}

#=======================================================================
# name - query
# purpose - query user for a response; return the response
#
# input parameters
# => $prompt: use this line to prompt for a response
# => $dflt: (optional) default value to use for <cr> response
#=======================================================================
sub query {
    my ($prompt, $dflt, $tflag, $ans);

    $prompt = shift @_;
    $dflt   = shift @_;

    $dflt = "" if blank($dflt);

    # prepare prompt
    #---------------
    $prompt .= " ";
    $prompt .= "[$dflt] " unless blank($dflt);
    print_($prompt);

    # get user response
    #------------------
    if ($noprompt) { $ans = $dflt; print_("$ans\n") }
    else           { chomp($ans = <STDIN>); $ans =~ s/^\s*|\s*$//g;
                     printLOG_("$ans\n") }

    $ans = "" unless defined($ans);
    $ans = $dflt if blank($ans);
    $ans = expand_EnvVars($ans);

    return $ans;
}

#=======================================================================
# name - setprompt
# purpose - sets global variable $noprompt which controls whether or not
#           the query() subroutine will prompt for input.
#
# input parameter
# => $flag (optional): == 0 : set $noprompt to 1, i.e. do not prompt
#                      == 1 : set $noprompt to 0, i.e. prompt (default)
#
# Note:
# 1. if prompt is turned off, then query() will automatically return the
#    default value, if available, rather than querying user for input.
#=======================================================================
sub setprompt {
    my $flag = shift @_;
    $flag = 1 unless defined($flag);

    if ($flag) { $noprompt = 0 }
    else       { $noprompt = 1 }
}

#=======================================================================
# name - setverbose
# purpose - sets global variable, $verbose
#
# input parameter
# => $flag (optional): == 0 : set $verbose = 0
#                      == 1 : set $verbose = 1 (default)
#=======================================================================
sub setverbose {
    my $flag = shift @_;
    $flag = 1 unless defined($flag);

    if ($flag) { $verbose = 1 }
    else       { $verbose = 0 }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          wrapper subroutines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#=======================================================================
# name - chdir_
# purpose - wrapper for perl chdir command
#
# input parameters
# => $dir: cd to this directory
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub chdir_ {
    use Cwd qw(chdir);  # override system chdir(); this keeps pwd up-to-date
    my ($dir, $vFLG);

    $dir  = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$dir, $vFLG);

    chdir $dir or die "Error. chdir $dir: $!";
    print_("cd " .display($dir) ."\n") if $vFLG;
}

#=======================================================================
# name - copy_
# purpose - wrapper for perl copy command
#
# input parameters
# => $source: move from this name
# => $dest: move to this name
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub copy_{
    use File::Basename qw(basename);
    use File::Copy qw(copy);
    my ($source, $dest, $vFLG);
    my ($prompt, $ans);

    $source = shift @_;
    $dest = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$source, $vFLG);
    
    # adjust $dest if it is a directory
    #----------------------------------
    if (-d $dest) { $dest .= "/" .basename $source }

    # overwrite pre-existing file?
    #-----------------------------
    if (-e $dest) {
        $prompt = "\nOverwrite " .display($dest) ." (y/n)?";
        $ans = query($prompt, "y");
        unless ($ans eq "y") { print_("Exiting\n"); exit }
    }        

    # copy file
    #----------
    copy $source, $dest or die "Error. Copying $source, $dest: $!";
    print_("copy " .display($source) ."\n".
           "  => " .display($dest)   ."\n") if $vFLG;
}

#=======================================================================
# name - mkpath_
# purpose - wrapper for perl mkpath command
#
# input parameters
# => $name: name of path to make
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub mkpath_ {
    use File::Path qw(mkpath);
    my ($name, $vFLG, %opts);

    $name = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$name, $vFLG);
    return if -d $name;

    # run mkpath command
    #-------------------
    %opts = ();
    $opts{"verbose"} = $vFLG;
    tee(\&mkpath, $name, \%opts) or die "Error. mkpath " .display($name) .";";
}

#=======================================================================
# name - move_
# purpose - wrapper for perl move command
#
# input parameters
# => $source: move from this name
# => $dest: move to this name
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub move_ {
    use File::Basename qw(basename);
    use File::Copy qw(move);
    my ($source, $dest, $vFLG);
    my ($prompt, $ans, $status);

    $source = shift @_;
    $dest = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$source, $vFLG);
    
    # adjust $dest if it is a directory
    #----------------------------------
    if (-d $dest) { $dest .= "/" .basename $source }

    # overwrite pre-existing file?
    #-----------------------------
    if (-e $dest) {
        $prompt = "\nOverwrite " .display($dest) ." (y/n)?";
        $ans = query($prompt, "y");
        unless ($ans eq "y") { print_("Exiting\n"); exit }
    }        

    # move file
    #----------
    if (-d $source) {
        $status = system("mv $source $dest");
        die "Error. Moving $source, $dest: $!" if $status;
    } else {
        move $source, $dest or die "Error. Moving $source, $dest: $!";
    }
    print_("move " .display($source) ."\n".
           "  => " .display($dest)   ."\n") if $vFLG;
}

#=======================================================================
# name - print_
# purpose - print to both STDOUT and to LOG
#=======================================================================
sub print_ {
    print @_;
    print LOG @_ if $LOGFILE;
    return;
}    

#=======================================================================
# name - printf_
# purpose - formatted print to both STDOUT and to LOG
#=======================================================================
sub printf_ {
    printf @_;
    printf LOG @_ if $LOGFILE;
    return;
}    

#=======================================================================
# name - printLOG_
# purpose - print only to LOG
#=======================================================================
sub printLOG_ {
    print LOG @_ if $LOGFILE;
    return;
}    

#=======================================================================
# name - realpath_
# purpose - wrapper for perl realpath command
#
# input parameter
# => $name: file or directory for which to find the absolute path
#
# Notes:
# 1. The realpath value is added to the global hash, %display, with the
#    input name as its key.
# 2. Defaults to local directory if $name is not sent
#=======================================================================
sub realpath_ {
    use Cwd qw(realpath);
    my ($name, $realpath);
    $name = shift @_;

    $realpath = realpath($name);
    unless ($display{$realpath}) {
        if (defined($name)) { $display{$realpath} = $name }
        else                { $display{$realpath} = "."   }
    }
    return $realpath;
}

#=======================================================================
# name - symlink_
# purpose - wrapper for perl symlink command
#
# input parameters
# => $target: the file being linked
# => $linkname: the link
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub symlink_ {
    use File::Basename qw(basename);
    my ($target, $linkname, $vFLG);

    $target   = shift @_;
    $linkname = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$target, $vFLG);

    # check to be sure $target exists
    #--------------------------------
    die "Error. $target not found;" unless -e $target;

    # adjust $linkname if it is a directory
    #--------------------------------------
    if (-d $linkname) { $linkname .= "/" .basename $target }
    unlink_($linkname, $vFLG) if -f $linkname or -l $linkname;

    # link target
    #------------
    symlink $target, $linkname or die
        "Error. symlink failed; $target -> $linkname: $!";
    print_("link " .display($target)   ."\n".
           "  => " .display($linkname) ."\n") if $vFLG;
}

#=======================================================================
# name - system_
# purpose - wrapper for perl system command
#
# input parameters
# => $cmd: command to send to system
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#
# Notes:
# 1. Use open3() to capture STDOUT, STDERR, and $status
# 2. This routine should not be used to call programs which require
#    interactive input (use perl system() instead).
# 3. If $cmd produces "too much" output, then this subroutine will
#    overload and freeze without giving any output. A reasonable
#    amount of output can be handled, but beware of diagnostic prints
#    which print everything and the kitchen sink.
#=======================================================================
sub system_ {
    use IPC::Open3 qw(open3);
    my ($cmd, $vFLG);
    my ($pid, $in, $status);

    $cmd  = shift @_;
    $vFLG = shift @_;
    $vFLG = verboseFLG($vFLG);

    strip_CRs(\$cmd, $vFLG);
    print_("$cmd\n") if $vFLG;

    $pid = open3($in, *OUT, *OUT, $cmd);
    waitpid($pid, 0);
    $status = $? >> 8;    

    if ($vFLG) { foreach (<OUT>) { print_($_) } }
    return $status;
}

#=======================================================================
# name - system1_
# purpose - wrapper for system_(); will check $status and quit if unsuccessful
#
# input parameters (same as for system_())
# => $cmd: command to send to system
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub system1_ {
    my ($cmd, $vFLG, $status);
    $status = system_(@_);

    # check status
    #-------------
    if ($status) {

        # print $cmd is not printed in system_()
        #---------------------------------------
        ($cmd, $vFLG) = @_;
        $vFLG = verboseFLG($vFLG);

        unless ($vFLG) {
            strip_CRs(\$cmd, $vFLG);
            print_("$cmd\n")
        }

        # print error status and exit
        #----------------------------
        print_("Error. status = $status\n");
        exit;
    }
}

#=======================================================================
# name - unlink_
# purpose - wrapper for perl unlink command
#
# input parameters
# => $name: name of file being unlinked
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#           == 0 : turn verbose off for this call
#           != 0 : turn verbose on  for this call
#=======================================================================
sub unlink_ {
    use File::Path qw(rmtree);
    my ($name, $vFLG, $cr, %opts);

    $name = shift @_;
    $vFLG = shift @_;

    $vFLG = verboseFLG($vFLG);
    strip_CRs(\$name, $vFLG);
    return unless -e $name;

    if (-d $name) {
        %opts = ();
        $opts{"verbose"} = $vFLG;
        tee(\&rmtree, $name, \%opts) or die "Error. rmtree " .display($name) .";";
    }
    else {
        if (unlink $name) { print_("rm " .display($name) ."\n") if $vFLG }
        else              { print_("Error unlinking $name\n") }
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     private UTILITY subroutines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#=======================================================================
# name - blank
# purpose - test whether input string is blank
#=======================================================================
sub blank {
    my $str;
    $str = shift @_;
    $str = "" unless defined($str);
    return 1 if $str =~ /^\s*$/;
}

#=======================================================================
# name - expand_EnvVars
# purpose - expand Environment Variables within a string
#=======================================================================
sub expand_EnvVars {
    my ($string, $cnt, $var, $name);

    $string = shift @_;
    $cnt = 0;

    # look for ${var} format
    #-----------------------
    while ($string =~ m/(\${(\w+)})/)   {
        $var = $1; $name = $2;
        $var =~ s/\$/\\\$/;
        $string =~ s/$var/$ENV{$name}/;
        die "Error. Infinite loop condition;" if ++$cnt > 100;
    }

    # look for $var format
    #---------------------
    while ($string =~ m/(\$\b(\w+)\b)/) {
        $var = $1; $name = $2;
        $var =~ s/\$/\\\$/;
        $string =~ s/$var/$ENV{$name}/;
        die "Error. Infinite loop condition;" if ++$cnt > 200;
    }
    return $string;
}

#=======================================================================
# name - strip_CRs
# purpose - strip and print leading "\n"'s from a string
#=======================================================================
sub strip_CRs {
    my ($strAddr, $vFLG, $cr);
    $strAddr = shift @_;
    $vFLG = shift @_;

    $cr = "";
    if ($$strAddr =~ m/^(\n*)(.*)$/) { $cr .= $1; $$strAddr = $2 }
    print_($cr) if $vFLG;
}

#=======================================================================
# name - tee
# purpose - run perl subroutines so that output goes to both STDOUT and LOG
#
# input parameters
# => $subref: address of system subroutine
# => @params: list parameters for system function
#
# Note:
# 1. Assumes that subroutine returns non-zero status for success
#=======================================================================
sub tee {
    my ($subref, @params);
    my ($outSTR, $FH, $status);

    $subref = shift @_;
    @params = @_;

    $outSTR = "";

    # capture STDOUT in variable, $outSTR
    #------------------------------------
    open STR, ">", \$outSTR or die "Error. open STR;";
    $FH = select;
    select STR;

    # run system function
    #--------------------
    $status = &$subref(@params);
    return 0 unless $status;

    # print output and close
    #-----------------------
    select $FH;
    print_($outSTR) if $outSTR;
    close STR;

    return $status;
}

#=======================================================================
# name - verboseFLG
# purpose - interpret $vFLG input to subroutine
#
# input parameter
# => $vFLG: (optional) verbose flag; defaults to global $verbose flag
#=======================================================================
sub verboseFLG {
    my $vFLG = shift @_;
    $vFLG = $verbose unless defined($vFLG);
    return $vFLG;
}

1;
