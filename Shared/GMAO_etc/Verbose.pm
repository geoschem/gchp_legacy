package Verbose;
#########################################################################
#
# name - Verbose
#
# purpose -
#   This package provides verbose wrappers to perl command and GMAO programs.
#
# !Revision History
#
# 09Jun2008  Stassi    Package added to CVS repository.
#
#########################################################################
use strict;
use File::Basename;
require Exporter;
our @ISA = "Exporter";
our @EXPORT_OK = qw( v_copy
                     v_dyn2dyn
                     v_forkcopy
                     v_link
                     v_mkdrstdate
                     v_rename
                     v_rst_date
                     v_setquiet
                     v_symlink
                     v_system
                     v_unlink
                     v_wait );

# global variables
#-----------------
my ($callscript, $cslen, $quiet);
my ($mkdrstdate, $rst_date, $dyn2dyn);

# initialize global variables
#----------------------------
$callscript = basename $0;
$cslen = length $callscript;
$quiet = 0;

#=======================================================================
# name - v_copy
# purpose - verbose copy
#
# input parameters
#   $source: source file
#   $destfile: destination file
#   $echoflag: if true, then only show directory when printing to stdout
#=======================================================================
sub v_copy {
    use File::Basename;
    use File::Copy;
    my ($source, $dest, $echoflag, $label);

    $source   = shift @_;
    $dest     = shift @_;
    $echoflag = shift @_;

    return unless &file_exists($source);
    if ($echoflag) { $label = dirname $dest; }
    else           { $label = $dest; }

    &v_unlink($dest);
    print "$callscript: copy $source\n"
        . " "x$cslen ."    to $label\n" unless $quiet;
    copy $source, $dest or warn
        ">> WARNING << $callscript: copy failed; $source to $dest: $!";
}

#=======================================================================
# name - v_dyn2dyn
# purpose - verbose call to dyn2dyn.x program
#
# input parameters
# - $opts:     string with run options and run flags
# - $forkflag: (optional) fork if non-zero (default = 0)
#=======================================================================
sub v_dyn2dyn {
    my ($opts, $forkflag, $pid);

    $opts     = shift @_;
    $forkflag = shift @_;

    $dyn2dyn = &locate_program("dyn2dyn.x") unless $dyn2dyn;

    if ($forkflag) {

        # come here to run as a forked process
        #-------------------------------------
        print "$callscript: dyn2dyn.x $opts (fork)\n" unless $quiet;

        defined($pid = fork) or die ">>> ERROR <<< cannot fork: $!";
        unless ($pid) {
            print "$callscript: *** (forked) dyn2dyn.x $opts\n" unless $quiet;
            system "$dyn2dyn $opts";
            exit; # exiting child process
        }
        return $pid;

    } else {

        # come here to run unforked
        #--------------------------
        print "$callscript: dyn2dyn.x $opts\n" unless $quiet;
        system "$dyn2dyn $opts";
        return 0;
    }
}

#=======================================================================
# name - v_forkcopy
# purpose - verbose copy using a forked process
#
# input parameters
#   $source: source file
#   $dest: destination file
#   $echoflag: if true, then only show directory when printing to stdout
#=======================================================================
sub v_forkcopy {
    use File::Basename;
    use File::Copy;
    my ($source, $dest, $echoflag, $label, $pid);

    $source    = shift @_;
    $dest      = shift @_;
    $echoflag  = shift @_;

    return 0 unless &file_exists($source);
    if ($echoflag) { $label = dirname $dest; }
    else           { $label = $dest; }

    &v_unlink($dest);
    print "$callscript: forkcopy $source\n"
        . " "x$cslen ."        to $label\n" unless $quiet;

    defined($pid = fork) or die ">>> ERROR <<< cannot fork: $!";
    unless ($pid) {
        print "$callscript: *** (forked) copy $source\n"
            . " "x$cslen ."                 to $label\n" unless $quiet;
        copy $source, $dest or warn
            ">> WARNING << $callscript: forkcopy failed; $source to $dest: $!";
        exit; # exiting child process
    }
    return $pid;
}    

#=======================================================================
# name - v_link
# purpose - verbose link
#
# input parameters
#   $target: file to link to
#   $linkname: link name
#   $echoflag: if true, then only show directory when printing to stdout
#=======================================================================
sub v_link {
    my ($target, $linkname, $echoflag, $label);

    $target   = shift @_;
    $linkname = shift @_;
    $echoflag = shift @_;

    return unless &file_exists($target);
    if ($echoflag) { $label = dirname $linkname; }
    else           { $label = $linkname; }

    &v_unlink($linkname);
    print "$callscript: link $target\n"
        . " "x$cslen ."    to $label\n" unless $quiet;
    link $target, $linkname or warn
        ">> WARNING << $callscript: link failed; $target <- $linkname: $!";
}

#=======================================================================
# name - v_setquiet
# purpose - set quiet flag
#
# input
#  $quietflag (optional)
#    =0: turn quiet flag off (i.e. verbose on)
#    =1: turn quiet flag on (i.e. verbose off) --- DEFAULT
#=======================================================================
sub v_setquiet {
    my $quietflag;

    $quietflag = shift @_;
    $quietflag = 1 unless defined($quietflag);

    # set global variable
    #--------------------
    $quiet = $quietflag;
}

#=======================================================================
# name - v_symlink
# purpose - verbose symlink
#
# input parameters
#   $target: file to symlink to
#   $linkname: link name
#   $echoflag: if true, then only show directory when printing to stdout
#=======================================================================
sub v_symlink {
    my ($target, $linkname, $echoflag, $label);

    $target   = shift @_;
    $linkname = shift @_;
    $echoflag = shift @_;

    return unless &file_exists($target);
    if ($echoflag) { $label = dirname $linkname; }
    else           { $label = $linkname; }

    &v_unlink($linkname);
    print "$callscript: symlink $target\n"
        . " "x$cslen ."       to $label\n" unless $quiet;
    symlink $target, $linkname or warn
        ">> WARNING << $callscript: symlink failed; $target <- $linkname: $!";
}

#=======================================================================
# name - v_mkdrstdate
# purpose - verbose call to mkdrstdate script
#
# input parameters
# - $nymd: yyyymmdd
# - $nhms: hhmmss
# - $outfile: output file
#=======================================================================
sub v_mkdrstdate {
    my ($nymd, $nhms, $outfile);

    $nymd    = shift @_;
    $nhms    = shift @_;
    $outfile = shift @_;

    $outfile = "d_rst" unless $outfile;
    $mkdrstdate = &locate_program("mkdrstdate.x") unless $mkdrstdate;

    &v_unlink($outfile);
    print "$callscript: mkdrstdate.x $nymd $nhms $outfile\n" unless $quiet;
    system "$mkdrstdate $nymd $nhms $outfile";
}

#=======================================================================
# name - v_rename
# purpose - verbose rename command
#
# input parameters
# - $oldname: old filename
# - $newname: new filename
#=======================================================================
sub v_rename {
    my ($oldname, $newname);

    $oldname = shift @_;
    $newname = shift @_;
    return unless &file_exists($oldname);

    print "$callscript: rename $oldname, $newname\n" unless $quiet;
    rename $oldname, $newname or warn
        ">> WARNING << $callscript: rename failed; $oldname to $newname: $!";
}

#=======================================================================
# name - v_rst_date
# purpose - verbose rst_date command
#
# input parameters
# - $infile:  input to rst_date
# - $outfile: output from rst_date
#=======================================================================
sub v_rst_date {
    my ($infile, $outfile, $outflag);

    $infile  = shift @_;
    $outfile = shift @_;
    return unless &file_exists($infile);

    $outflag = "";
    $outflag = "> $outfile" if $outfile;
    $rst_date = &locate_program("rst_date") unless $rst_date;

    &v_unlink($outfile);
    print "$callscript: rst_date $infile $outflag\n" unless $quiet;
    system "$rst_date $infile $outflag";
}

#=======================================================================
# name - v_unlink
# purpose - verbose unlink
#
# input parameters
# - $target: file to remove
#=======================================================================
sub v_unlink {
    my ($file);

    $file = shift @_;
    if ((-f $file) or (-l $file)) {
        print "$callscript: unlink $file\n" unless $quiet;
        unlink $file;
    }
}

#=======================================================================
# name - v_wait
# purpose - verbose wait for forked jobs to complete
#
# input parameter
# - @pidarr: array of pids
#=======================================================================
sub v_wait {
    my @pidarr;

    @pidarr = @_;
    return unless @pidarr;

    $| = 1;
    print "$callscript: waiting for forked jobs ... " unless $quiet;
    foreach (@pidarr) { waitpid($_, 0) };
    print "okay.\n" unless $quiet;
}

#=======================================================================
# name - v_system
# purpose - verbose system command
#
# input parameter
# - $cmd
#=======================================================================
sub v_system {
    my $cmd;

    $cmd = shift @_;
    print "$cmd\n" unless $quiet;
    system $cmd;
}

#=======================================================================
# name - file_exists
# purpose - determine whether a file exists
#
# input parameter
# - $file: file to check
#
# return value
# =1 if file exists
# =0 if file does not exist
#=======================================================================
sub file_exists {
    my ($file, $found);

    $file = shift @_;

    $found = (-e $file);
    print "$callscript: $file not found.\n" unless ($found or $quiet);
    return $found;
}

#=======================================================================
# name - locate_program
# purpose - find the directory location of a program
#
# input parameters
# - $progname: name of program to find
# - $program_ptr: pointer to variable holding program reference
#=======================================================================
sub locate_program {
    use FindBin;
    my ($progname, $prog, $program);
    my ($fvroot, $fvbin);
    my (@dirs, $dir);

    $progname = shift @_;

    # directory locations to search
    #------------------------------
    $fvroot = $ENV{"FVROOT"};
    $fvbin = "$fvroot/bin" if $fvroot;
    push @dirs, "$FindBin::Bin";
    push @dirs, $fvbin if ($fvbin and -d $fvbin);

    # locate program
    #---------------
    $program = "";
    foreach $dir (@dirs) {
        $prog = "$dir/$progname";
        $program = $prog if (-s $prog and -X $prog);
    }
    die ">>> Error <<< cannot find $progname: $!" unless $program;
    return $program;
}

1;
