#!/usr/bin/env perl
#=======================================================================
# name - cmpdir
# purpose - this utility allows users to quickly and easily see
#           differences between files in two experiment directories.
#
# Note:
# 1. See usage subroutine for usage information
# 2. This script uses the xxdiff utility
#
# !Revision History
# 29Mar2010  Stassi  Initial version.
#=======================================================================
use strict;
use warnings;

use Cwd ("abs_path");
use File::Basename qw(basename dirname);
use File::Copy qw(copy);
use File::Find qw(find);
use File::Path qw(mkpath rmtree);
use Getopt::Long;

use FindBin qw($Bin);
use lib "$FindBin::Bin";
use Query qw(query yes);

# global variables
#-----------------
my ($bindiff, $bwiFLG, $debug, $diffFLGs);
my ($dir1, $dir2, $dirA, $dirB, $dirL1, $dirL2);
my ($dmgetcmd, $ext, $first, $list, $listx);
my ($quiet, $recurse, $sortFLG, $subdir, $verbose);
my (%different, %diffsBIN, %diffsTXT, %dir_display);
my (%filesize, %found, %identical, %opts, %vopts);
my (@exclude, @fileIDs, @files, @files1, @files2);
my (@p1, @p2, @subdirs, @unmatched1, @unmatched2);

# main program
#-------------
{
    init();
    while (1) {
        cmp_files();
        show_results();
    }
}

#=======================================================================
# name - init
# purpose  - get input parameters and initialize global variables
#=======================================================================
sub init {
    my ($bwiFLG, $cvsX, $etcflg, $fcstflg, $help, %patterns, $rsflg, $runflg);
    my ($arrAddr, $subdirname, $val, @values, @vlist);

    # get runtime flags
    #------------------
    GetOptions("bwi"      => \$bwiFLG,
               "cvsX"     => \$cvsX,
               "db|debug" => \$debug,
               "ext=s"    => \$ext,
               "etc"      => \$etcflg,
               "fcst"     => \$fcstflg,
               "h|help"   => \$help,
               "id=s"     => \@fileIDs,
               "list"     => \$list,
               "listx"    => \$listx,
               "p=s"      => \%patterns,
               "p1=s"     => \@p1,
               "p2=s"     => \@p2,
               "q"        => \$quiet,
               "r"        => \$recurse,
               "rs"       => \$rsflg,
               "run"      => \$runflg,
               "subdir=s" => \$subdir,
               "v"        => \$verbose,
               "X=s"      => \@exclude );
    usage() if $help;
    $list = 1 if $listx;
    $verbose = 1 if $debug;

    # verbose option, only if verbose mode
    #-------------------------------------
    if ($verbose) { %opts = ("verbose" => 1) }
    else          { %opts = () }

    # verbose option, always
    #-----------------------
    %vopts = ("verbose" => 1);

    # extract comma-separated option values
    #--------------------------------------
    foreach $arrAddr (\@exclude, \@fileIDs, \@p1, \@p2) {
        @vlist = ();
        foreach (@$arrAddr) {
            @values = split ',', $_;
            foreach $val (@values) { push @vlist, $val if $val }
        }
        @$arrAddr = @vlist;
    }

    # exclude CVS directories
    #------------------------
    if ($cvsX) {
        push @exclude, "CVS";
        push @exclude, "Entries";
        push @exclude, "Root";
        push @exclude, "Tag";
    }

    # shortcuts for checking fcst, run, or rs directory 
    #--------------------------------------------------
    if ($etcflg)  { $subdir = "etc";  $recurse = 1 }
    if ($fcstflg) { $subdir = "fcst"; $recurse = 1 }
    if ($runflg)  { $subdir = "run";  $recurse = 1 }
    if ($rsflg)   { $subdir = "rs";   $recurse = 1 unless $ext }

    # runtime parameters
    #-------------------
    $dirA  = shift @ARGV;
    $dirB  = shift @ARGV;

    # check inputs
    #-------------
    usage() unless $dirA and $dirB;
    die ">> Error << Cannot find directory $dirA" unless -d $dirA;
    die ">> Error << Cannot find directory $dirB" unless -d $dirB;

    # remove final slash from directory name
    #---------------------------------------
    $dirA = cleanDirName($dirA, 1);
    $dirB = cleanDirName($dirB, 1);

    # add user inputted patterns to pattern arrays
    #---------------------------------------------
    foreach (keys %patterns) {
        push @p1, $_;
        push @p2, $patterns{$_};
    }

    # add directory basenames to pattern arrays
    #------------------------------------------
    push @p1, basename $dirA;
    push @p2, basename $dirB;

    # check for @p1/@p2 correspondence
    #---------------------------------
    die ">> Error << unequal number of patterns (-p1/-p2)"
        unless scalar(@p1) == scalar(@p2);

    # find common subdirectories
    #---------------------------
    foreach (<$dirA/*>) {
        next unless -d abs_path($_);
        $subdirname = basename $_;
        push @subdirs, $subdirname if -d abs_path("$dirB/$subdirname");
        next;
    }

    # check for requested subdirectory
    #---------------------------------
    if ($subdir) {
        die "not found: $dirA/$subdir\n" unless -d abs_path("$dirA/$subdir");
        die "not found: $dirB/$subdir\n" unless -d abs_path("$dirB/$subdir");
    }

    # initialize other variables
    #---------------------------
    $first = 1;
    if ($bwiFLG) { $diffFLGs = "-bwi" }
    else         { $diffFLGs = ""     }

    # select program to use for $bindiff
    #-----------------------------------
    if (-x "/home/jstassi/bin/cdo") {
        $bindiff = "/home/jstassi/bin/cdo diffn";
    }
    else  {
        chomp($bindiff = `which h5diff`);
        $bindiff = "" unless -x $bindiff;
    }

    # look for dmget command
    #-----------------------
    $dmgetcmd = "/x/x/x/";
    chomp($dmgetcmd = `which dmget`);
}

#=======================================================================
# name - cmp_files
# purpose - get list of files from both directories and compare files
#           to find those which are the same in both directories, those
#           which are not, and those which are in only one directory or
#           the other.
#=======================================================================
sub cmp_files {
    my ($dirID, $arrAddr, $file, $filext, @dmgetArr);
    my ($fID, $file1, $file2, $base1, $base2, $dirname1, $dirname2, $middir);
    my ($align, $maxK, $fmt1, $fmt2);
    my ($status, $indexB, $indexT, %Rfound);

    # get directories
    #----------------
    $dir1 = $dirA;
    $dir2 = $dirB;
    $dirL1 = basename $dir1;
    $dirL2 = basename $dir2;

    if ($subdir) {
        $dir1  .= "/$subdir";
        $dir2  .= "/$subdir";
        $dirL1 .= "/$subdir";
        $dirL2 .= "/$subdir";
    }

    # comparing lists of file names in directories only
    #--------------------------------------------------
    if ($list) { cmp_lists($dir1, $dir2); return }

    # get file lists
    #---------------
    if ($ext) {
        chomp( @files1 = (`find -L $dir1 -name \\*\.$ext`) );
        chomp( @files2 = (`find -L $dir2 -name \\*\.$ext`) );
    }
    elsif (@fileIDs) {
        @files1 = ();
        @files2 = ();
        foreach $fID (@fileIDs) {
            $fID =~ s/\./\\\./;
            chomp( @files1 = (@files1, `find -L $dir1 -name \\*$fID\\*`) );
            chomp( @files2 = (@files2, `find -L $dir2 -name \\*$fID\\*`) );
        }
    }
    else {
        if ($recurse) {
            chomp( @files1 = (`find -L $dir1 -name \\*`) );
            chomp( @files2 = (`find -L $dir2 -name \\*`) );
        }
        else {
            @files1 = ( <$dir1/*> );
            @files2 = ( <$dir2/*> );
        }
    }

    # remove directories
    #-------------------
    foreach $arrAddr (\@files1, \@files2) {

        foreach (0..$#$arrAddr) {
            $file = shift @$arrAddr;
            push @$arrAddr, $file unless -d abs_path($file);
        }
    }
    show_file_counts(1);
    find_common_label(\@files1, \@files2);

    # zero-out lists
    #---------------
    %diffsBIN = ();
    %diffsTXT = ();
    %different = ();
    %identical = ();
    @unmatched1 = ();
    @unmatched2 = ();
    %found = ();

    # which files are in both expdir1 and expdir2?
    #---------------------------------------------
    foreach $file1 (@files1) {

        $base1 = basename $file1;
        next if Xcluded($base1);

        $dirname1 = dirname $file1;
        $middir = "";
        $middir = $1 if ($dirname1 =~ m[^$dir1/(\S+)]);
        print "(1) checking $file1\n" if $verbose;

        if ($middir) { $file2 = namechange("$dir2/$middir/$base1") }
        else         { $file2 = namechange("$dir2/$base1")         }

        print "(2) checking $file2\n\n" if $verbose;

        if (-e $file2 or -l $file2) { $found{$file1} = $file2 }
        else                        { push @unmatched1, display($file1,1) }
    }

    # send job to dmget files (just in case)
    #---------------------------------------
    dmget(1, keys %found);
    dmget(2, values %found);

    # compare files found in both directories
    #----------------------------------------
    $maxK = 0;
    foreach (keys %found) {
        $align = length(basename($_));
        $maxK = $align if $align > $maxK;
    }
    $fmt1 = "checking %s\n";
    $fmt2 = "checking %-${maxK}s <=> %s\n";

    foreach $file1 (sort keys %found) {
        $file2 = $found{$file1};
        $base1 = basename($file1);
        $base2 = basename($file2);
        unless ($quiet) {
            if ($base1 eq $base2) { printf $fmt1, $base1         }
            else                  { printf $fmt2, $base1, $base2 }
        }

        # get file sizes
        #---------------
        $filesize{$file1} = -s abs_path($file1);
        $filesize{$file2} = -s abs_path($file2);

        $status = system_("diff $diffFLGs $file1 $file2 >& /dev/null");
        unless ($status) {
            $identical{$file1} = $file2;
            next;
        }

        # files are different
        #--------------------
        $different{$file1} = $file2;
        if ( text($file1, $file2) ) { $diffsTXT{++$indexT} = $file1 }
        else                        { $diffsBIN{++$indexB} = $file1 }
    }

    # which $dir2 files are not in $dir1?
    #------------------------------------
    %Rfound = reverse %found;
    foreach $file2 (@files2) {
        push @unmatched2, display($file2,2) unless $Rfound{$file2};
    }

    @unmatched1 = sort @unmatched1;
    @unmatched2 = sort @unmatched2;
}

#=======================================================================
# name - cmp_lists
# purpose - compare lists of files in $dir1 and $dir2
#=======================================================================
sub cmp_lists {
    my ($HOME, $list1, $list2);
    my (@filearr1, @filearr2, $cnt, $name, %fopts);
    my ($expid1, $expid2);

    # make list of files in $dir1
    #----------------------------
    print "\n(1) making list of files in $dir1\n";
    @files = ();

    %fopts = ( "wanted" => \&wanted, "follow" => 1 );
    find(\%fopts, $dir1);
    @filearr1 = sort @files;
    $expid1 = getexpid(@filearr1);

    # make list of files in $dir2
    #----------------------------
    print "(2) making list of files in $dir2\n";
    @files = ();
    find(\%fopts, $dir2);
    @filearr2 = sort @files;
    $expid2 = getexpid(@filearr2);

    # ignore $dir1/dir2 name differences
    #-----------------------------------
    if ($listx) {
        if ($dir1 =~ /$dir2/) {
            $cnt = scalar(@filearr2);
            for (1..$cnt) {
                $name = shift @filearr2;
                $name =~ s/$dir2/$dir1/;
                $name =~ s/\b$expid2\b/$expid1/g if $expid1 and $expid2;
                push @filearr2, $name;
            }
        }
        else {
            $cnt = scalar(@filearr1);
            for (1..$cnt) {
                $name = shift @filearr1;
                $name =~ s/$dir1/$dir2/;
                $name =~ s/\b$expid1\b/$expid2/g if $expid1 and $expid2;
                push @filearr1, $name;
            }
        }
    }

    # dump lists to files
    #--------------------
    $HOME = $ENV{"HOME"};
    $list1 = "$HOME/cmpdir_list1_" .basename($dir1);
    $list2 = "$HOME/cmpdir_list2_" .basename($dir2);

    listdump(\@filearr1, $list1, dirname($dir1)."/");
    print "\n(1) list written: $list1 (" .scalar(@filearr1) ." lines)\n";

    listdump(\@filearr2, $list2, dirname($dir2)."/");
    print "(2) list written: $list2 (" .scalar(@filearr2) ." lines)\n\n";
        
    # set up global arrays and hashes to show differences in list files
    #------------------------------------------------------------------
    @files1 = ( $list1 );
    @files2 = ( $list2 );
    $found{$list1} = $list2;
    $different{$list1} = $list2;
    $diffsTXT{"1"} = $list1;
    $diffFLGs = "-bwi";
    return;
}

#=======================================================================
# name - find_common_label
# purpose - check the file names to see if the files have a common label.
#           if so, then add labels to pattern arrays, if not already present.
#
# input parameters
# => $arr1ADDR: address of 1st array of filenames
# => $arr1ADDR: address of 2nd array of filenames
#=======================================================================
sub find_common_label {
    my ($arr1ADDR, $arr2ADDR, @arr1, @arr2);
    my ($base, @parts, %L1, %L2);
    my ($max, $labelone, $labeltwo);

    $arr1ADDR = shift @_;
    $arr2ADDR = shift @_;
    @arr1 = @$arr1ADDR;
    @arr2 = @$arr2ADDR;

    # get labels from files in each array
    #------------------------------------
    foreach (@arr1) {
        $base = basename $_;
        @parts = split /[.]/, $base;
        ++$L1{$parts[0]} if scalar(@parts) > 1;
    }
    foreach (@arr2) {
        $base = basename $_;
        @parts = split /[.]/, $base;
        ++$L2{$parts[0]} if scalar(@parts) > 1;
    }
    
    # find most common label in each array
    #-------------------------------------
    $max = 0; $labelone = "";
    foreach (keys %L1) {
        if ($L1{$_} > $max) { $labelone = $_; $max = $L1{$_} }
    }
    return unless $max*2 > @arr1;

    $max = 0; $labeltwo = "";
    foreach (keys %L2) {
        if ($L2{$_} > $max) { $labeltwo = $_; $max = $L2{$_} }
    }
    return unless $max*2 > @arr2;

    # add labels to pattern arrays if appropriate
    #--------------------------------------------
    if ($labelone ne $labeltwo) {
        unless ( in($labelone, @p1) and in($labeltwo, @p2) ) {
            push @p1, $labelone;
            push @p2, $labeltwo;
        }
    }
    if ($debug) {
        foreach (0..$#p1) { print "p1/p2 = $p1[$_]/$p2[$_]\n" };
        pause();
    }
    return;
}

#=======================================================================
# name - show_results
# purpose - give menu option for user to view results of comparison
#           between two directories
#=======================================================================
sub show_results {
    my ($opt, $dflt, $ans, $pid);

    $opt  = "";
    $dflt = 1;
    unless (@files1 and @files2) {
        if ($ext) { print "No .$ext files found.\n\n"; exit }
        else      { $dflt = 6 }
    }
    unless (%found) {
        if ($ext)  { print "No common .$ext files were found.\n\n" }
        else       { print "No common files were found.\n\n" }
        print "Continue? (y/n) [n] ";
        chomp($ans = <STDIN>);
        exit unless lc($ans) eq "y";
    }

    # put list diffs display in background by forking
    #------------------------------------------------
    if ($list) {
        defined($pid = fork) or die "Error while forking: $!";
        unless ($pid) {
            $diffsTXT{"wait"} = 1;
            display_text_diffs(1, %diffsTXT);
            delete $diffsTXT{"wait"};

            foreach (keys %different) {
                unlink $_;
                unlink $different{$_};
                print "\nunlink $_\n";
                print "unlink $different{$_}\n";
            }
        }
        exit;
    }

    while (1) {
        unless ($first and $dflt == 5) {
            underline("Make Selection",2);
            print " 1. differences\n"
                . " 2. identical\n"
                . " 3. not found\n\n"
                . " 4. file counts\n"
                . " 5. file lists\n";
            print " 6. choose (other) subdirectory\n" unless $ext;
            print "\n"
                . " 0. quit\n\n"
                . "choose option: [$dflt] ";

            chomp($opt = <STDIN>);
            $opt =~ s/\s//g;
        }
        $opt = $dflt if $opt eq "";
        exit unless $opt;

        $first = 0;
        if ($opt > 2) { $dflt = 0 }
        else          { $dflt = $opt + 1 }

        if ($opt eq "1") { show_differences(); next }
        if ($opt eq "2") { show_identical();   next }
        if ($opt eq "3") { show_unmatched();   next }
        if ($opt eq "4") { show_file_counts(); next }
        if ($opt eq "5") { list_files();       next }
        unless ($ext) {
            if ($opt eq "6") { choose_subdir();  return }
        }
        print "\n$opt: Invalid option; Try again.\n\n";
    }
}

#=======================================================================
# name - show differences
# purpose - print summary list of files which differ; give user option
#           to view differences in specific files using xxdiff utility
#=======================================================================
sub show_differences {
    unless (%diffsBIN or %diffsTXT) {
        print "\nNo differences found.\n";
        pause();
        return;
    }
    size_differences();
    show_binary_diffs() if %diffsBIN;
    show_text_diffs()   if %diffsTXT;
}

#=======================================================================
# name - size differences
# purpose - show size differences of common files
#=======================================================================
sub size_differences {
    my ($fmt, $file1, $file2, $label, $diffcnt);
    $fmt = "%s: %s (%d)\n";
    foreach $file1 (sort keys %filesize) {
        next unless $file2 = $found{$file1};
        if ($filesize{$file1} != $filesize{$file2}) {
            print "\n";
            print "These file sizes differ\n"
                . "-----------------------\n" unless $label;
            printf $fmt, "dir1", basename($file1), $filesize{$file1};
            printf $fmt, "dir2", basename($file2), $filesize{$file2};
            $diffcnt++;
            $label = 1;
        }
    }
    pause() if $diffcnt;
}

#=======================================================================
# name - show_binary_diffs
# purpose - 
#=======================================================================
sub show_binary_diffs {
    my (%diffs, $maxB, $align, $fmt1, $fmtB, $num, $show_menu, $ask);
    my ($file1, $file2, $base1, $base2, $ext1, $ext2, $dflt, $sel);

    if (@_) { %diffs = @_ }
    else    { %diffs = %diffsBIN }

    $maxB = 0;
    foreach (values %diffs) {
        $align = baselen($_);
        $maxB = $align if $align > $maxB;
    }
    $fmt1 = "%3s. %s\n";
    $fmtB = "%3s. %-${maxB}s <=> %-s\n";

    $num = 1;
    $show_menu = 1;
    while (1) {

        if ($show_menu) {

            # select file to do binary diff
            #------------------------------
            $num--;
            underline("These binary files differ") if %diffs;
            foreach (sort numeric keys %diffs) {
                $file1 = $diffs{$_};
                $base1 = mybase($file1, "1");
                $base2 = mybase($different{$file1}, "2");

                if ($base1 eq $base2) { printf $fmt1, $_, $base1 }
                else                  { printf $fmtB, $_, $base1, $base2 }
            }
        }

        unless ($bindiff) {
            pause();
            last;
        }

        print "\n";
        printf $fmt1, "0", "previous menu";
        printf $fmt1, "-1", "refresh menu\n";

        $show_menu = 0;
        $dflt = ++$num;

        while ($dflt) {
            unless ( $diffs{$dflt} ) { $dflt = 0; last }
            last if $diffs{$dflt} =~ /\.tar$/;

            if ($bindiff =~ m/cdo/) {
                last if $diffs{$dflt} =~ /\.hdf$/
                    or  $diffs{$dflt} =~ /\.nc4$/
                    or  $diffs{$dflt} =~ /\.ods$/;
            }
            else {
                last if $diffs{$dflt} =~ /\.nc4$/;
            }
            $dflt++;
                
        }

        print "Compare files: [$dflt] ";
        chomp( $sel = <STDIN> );
        $sel =~ s/\s//g;
        $sel = $dflt if $sel eq "";

        if ($sel ==  0) { return }
        if ($sel == -1) { $show_menu = 1; next }
            
        # show selected binary diff
        #--------------------------
        $num = $sel;
        unless ($diffs{$num}) {
            print "Selection not found: $num\n"
                . "Try again.\n";
            $num = --$dflt;
            next;
        }
        $file1 = $diffs{$num};
        $file2 = $different{$file1};

        $ext1 = (split /[.]/, $file1)[-1];
        $ext2 = (split /[.]/, $file2)[-1];

        # compare tarfile contents
        #-------------------------
        if ($ext1 eq "tar" and $ext2 eq "tar") {
            $ask = query("Compare tarfile contents?", "y");
            if (yes($ask)) {
                cmp_tarfiles($file1, $file2);

                $base1 = basename($file1);
                $base2 = basename($file2);

                print "\n Tarfile comparison complete.";
                print "\n [$num] $base1";
                print " <=> $base2" if $base1 ne $base2;
                print "\n";
                pause()
            }
            $num++;
            $show_menu = 1;
        }

        # compare binary files
        #---------------------
        else { cmp_binary_files($num, $file1, $file2) }
    }
}

#=======================================================================
# name - cmp_binary_files
# purpose - compare two binary files
#
# input parameters
# => $num: index of %diff
# => $file1: 1st file to compare
# => $file2: 2nd file to compare
#=======================================================================
sub cmp_binary_files {
    my ($num, $file1, $file2, $base1, $base2, $status);

    $num   = shift @_;
    $file1 = shift @_;
    $file2 = shift @_;

    $base1 = mybase($file1, "1");
    $base2 = mybase($file2, "2");

    if ($base1 eq $base2) {
        printf "$bindiff (%d) %s\n", $num, $base1;
    } else {
        printf "$bindiff (%d) %s <=> %s\n", $num, $base1, $base2;
    }

    $status = system_("$bindiff $file1 $file2");
    unless ($bindiff =~ m/cdo/) {
        if ($status ) { print "FILES DIFFER\n" }
        else          { print "FILES MATCH\n"  }
    }
}

#=======================================================================
# name - cmp_tarfiles
# purpose - recursively call cmpdir.pl to compare contents of tarfiles
#=======================================================================
sub cmp_tarfiles {
    my  (@tarfile, @tmpdir);

    $tarfile[1] = shift @_;
    $tarfile[2] = shift @_;

    # untar into temporary directories
    #---------------------------------
    foreach (1..2) {
        $tmpdir[$_] = "$ENV{TMPDIR}/tmpdir$_.$$";
        rmtree($tmpdir[$_], \%opts) if -d $tmpdir[$_];
        mkpath($tmpdir[$_], \%vopts);

        system_("tar xf $tarfile[$_] -C $tmpdir[$_]")
            && die "Error untarring $tmpdir[$_];";
    }

    # use cmpdir.pl to compare temporary directories
    #-----------------------------------------------
    system("$Bin/cmpdir.pl $tmpdir[1] $tmpdir[2]");

    # remove temporary directories when done
    #---------------------------------------
    rmtree($tmpdir[1], \%opts);
    rmtree($tmpdir[2], \%opts);
}

#=======================================================================
# name - show_text_diffs
# purpose - show text differences
#=======================================================================
sub show_text_diffs {
    my (%diffs, $maxT, $fmt0, $fmt1, $fmtT, $num);
    my ($file1, $base1, $base2, $dflt, $sel);

    if (@_) { %diffs = @_ }
    else    { %diffs = %diffsTXT }

    $maxT = 0;
    foreach (values %diffs) { $maxT = baselen($_) if baselen($_) > $maxT }

    $fmt0 = "%3s. %s";
    $fmt1 = "%3s. %s\n";
    $fmtT = "%3s. %-${maxT}s <=> %-s\n";

    return unless %diffs;
    $num = 0;
    while (1) {
        
        # select which file to show differences
        #--------------------------------------
        underline("These text files differ");
        foreach (sort numeric keys %diffs) {
            $file1 = $diffs{$_};
            $base1 = mybase($file1, "1");
            $base2 = mybase($different{$file1}, "2");

            if ($base1 eq $base2) { printf $fmt1, $_, $base1 }
            else                  { printf $fmtT, $_, $base1, $base2 }
        }
        $dflt = ++$num;
        $dflt = 0 unless $diffs{$dflt};

        print "\n";
        printf $fmt1, "0", "previous menu\n";
        if (keys %diffs > 1) {
            printf $fmt0, "a", "cycle thru all";
            if ($dflt) { print " (starting from $dflt)\n" } else { print "\n" }
        }
        if ($diffFLGs) { printf $fmt1, "b", "turn OFF -bwi diff flag" }
        else           { printf $fmt1, "b", "turn ON -bwi diff flag"  }

        if ($sortFLG) { printf $fmt1, "s", "turn OFF sorted diff\n" }
        else          { printf $fmt1, "s", "turn ON sorted diff\n"  }

        print "Make Selection: [$dflt] ";
        chomp( $sel = <STDIN> );
        $sel =~ s/\s//g;
        $sel = $dflt if $sel eq "";
        

        return if $sel eq "0";

        # show differences for all remaining files starting with current index
        #-----------------------------------------------------------------------
        # use $diffs{"wait"} to signal not to display all at once if $sel eq "a"
        # (unadvertised "A" option will display all at once)
        #-----------------------------------------------------------------------
        if ($sel eq "a" or $sel eq "A") {
            $diffs{"wait"} = 1 if $sel eq "a";
            $num = 1 unless $diffs{$num};
            while ($diffs{$num}) {
                display_text_diffs($num, %diffs);
                $num++;
            }
            delete $diffs{"wait"} if $diffs{"wait"};
            $num = -1; next;
        }

        # toggle diff -bwi flag
        #----------------------
        if ($sel eq "b") {
            $bwiFLG = ! $bwiFLG;
            if ($diffFLGs) { $diffFLGs = ""     }
            else           { $diffFLGs = "-bwi" }
            $num -= 2; $num = 0 if $num < 0; next;
        }

        # toggle $sortFLG
        #----------------
        if ($sel eq "s") {
            if ($sortFLG) { $sortFLG = 0 }
            else          { $sortFLG = 1 }
            $num -= 1; next;
        }

        # show selected difference
        #-------------------------
        $num = $sel;
        unless ($diffs{$num}) {
            print "Selection not found: $num\n"
                . "Try again.\n";
            $num = --$dflt;
            next;
        }
        display_text_diffs($num, %diffs);
    }
}

#=======================================================================
# name - display_text_diffs
# purpose - display the xxdiff of two text files
#
# input parameters
# => $num: index number of difference to display (starting at 1)
#=======================================================================
sub display_text_diffs {
    my ($num, %diffs, $amperflag);
    my ($file1, $file2, $base1, $base2);
    my ($tmpdir, $srt, $f1, $f2);

    $num = shift @_;
    %diffs = @_;

    $file1 = $diffs{$num};
    $file2 = $different{$file1};
    $base1 = basename $file1;
    $base2 = basename $file2;

    $tmpdir = "$ENV{TMPDIR}/tmpdir.$$";
    rmtree($tmpdir, \%opts) if -d $tmpdir;

    if ($sortFLG) {
        $srt = "sorted ";
        mkpath($tmpdir, \%opts);

        $f1 = "$tmpdir/$base1.sorted.1";
        $f2 = "$tmpdir/$base2.sorted.2";

        system_("sort $file1 > $f1") && die "Error: sort $file1 > $f1;";
        system_("sort $file2 > $f2") && die "Error: sort $file2 > $f2;";
    }
    else {
        $srt = "";
        $f1 = $file1;
        $f2 = $file2;
    }

    if ($base1 eq $base2) {
        printf "showing ${srt}diffs for (%d) %s\n", $num, $base1;
    } else {
        printf "showing ${srt}diffs for (%d) %s <=> %s\n", $num, $base1, $base2;
    }
    if ($diffs{"wait"} or $sortFLG) { $amperflag = ""  }
    else                            { $amperflag = "&" }

    system_("xxdiff --text $diffFLGs $f1 $f2 $amperflag");
    rmtree($tmpdir, \%opts) if -d $tmpdir;
}

#=======================================================================
# name - show_identical
# purpose - print summary list of files which are identical in both
#           directories
#=======================================================================
sub show_identical {
    my ($max, $num, $fmt1, $fmt2);
    my ($file1, $file2, $base1, $base2);

    $max = 0;
    foreach (keys %identical) {
        $max = length(mybase($_, "1")) if length(mybase($_, "1")) > $max;
    }
    $fmt1 = "%2d. %s\n";
    $fmt2 = "%2d. %-${max}s <=> %-s\n";

    if (%identical) {
        $num = 0;
        underline("These files are identical in the two directories");
        foreach (sort keys %identical) {
            $file1 = $_;
            $file2 = $identical{$file1};
            $base1 = mybase($file1, "1");
            $base2 = mybase($file2, "2");
            if ($base1 eq $base2) { printf $fmt1, ++$num, $base1         }
            else                  { printf $fmt2, ++$num, $base1, $base2 }
        }
    } else {
        print "\nNo identical files were found in the two directories.\n";
    }
    pause();
}

#=======================================================================
# name - show_unmatched
# purpose - print summary lists of files which exist in one directory
#           but not in the other
#=======================================================================
sub show_unmatched {
    my ($num, $ddir1, $ddir2, $ddirL1, $ddirL2);
    my ($ask, $f1, $f2, $file1, $file2);
    my (%diffsB, %diffsT, $indexB, $indexT);
    my (%RdiffsT, %RdiffsB, $key);
    my ($menu, $len, $len3, $len7);
    my ($dflt1, $dflt2, $cnt1, $cnt2, $cnt, $delta);

    $ddir1  = display($dir1,1);
    $ddir2  = display($dir2,2);
    $ddirL1 = display($dirL1,1);
    $ddirL2 = display($dirL2,2);

    # list files found in dir1 but not in dir2
    #-----------------------------------------
    if (@unmatched1) {
        $num = 0;
        underline("FOUND in (1) $ddirL1 but NOT FOUND in (2) $ddirL2");
        foreach ( @unmatched1 ) { printf "%2d. %s\n", ++$num, $_ }
    } else {
        if (@files1) { print "\nAll files in $ddir1 are also in $ddir2\n"   }
        else         { print "\nNo files found in dir1: $ddir1\n" }
    }
    pause();

    # list files found in dir2 but not in dir1
    #-----------------------------------------
    if (@unmatched2) {
        $num = 0;
        underline("FOUND in (2) $ddirL2 but NOT FOUND in (1) $ddirL1");
        foreach ( @unmatched2 ) { printf "%2d. %s\n", ++$num, $_ }
    } else {
        if (@files2) { print "\nAll files in $ddir2 are also in $ddir1\n"   }
        else         { print "\nNo files found in dir2: $ddir1\n" }
    }
    pause() unless @unmatched1 and @unmatched2;

    # does user want to compare unmatched files?
    #-------------------------------------------
    if (@unmatched1 and @unmatched2) {
        $menu = 1;
        $cnt1 = scalar @unmatched1;
        $cnt2 = scalar @unmatched2;
        if ($cnt1 > $cnt2) { $cnt = $cnt1 }
        else               { $cnt = $cnt2 }

        $len = maxLength(\@unmatched1, "basename");
        $len = 15 if $len < 15;
        $len3 = $len + 3;
        $len7 = $len + 7;
        $dflt1 = 1;
        $delta = 0;

        %diffsB = (); $indexB = 0;
        %diffsT = (); $indexT = 0;

        $ask = query("\nCompare unmatched files from the two directories?", "n");

        if (yes($ask) and $#unmatched1 == 0 and $#unmatched2 == 0) {
            $file1 = $unmatched1[0];
            $file2 = $unmatched2[0];
            $different{$file1} = $file2;

            if (text($file1, $file2)) { $diffsT{++$indexT} = $file1 }
            else                      { $diffsB{++$indexB} = $file1 }
            $ask = "n";
        }
        while (yes($ask)) {
            dmget(1, @unmatched1);
            dmget(2, @unmatched2);

            if ($menu) {
                print "\n     dir1: $ddirL1\n";
                print   "     dir2: $ddirL2\n\n";

                printf  "     %-${len7}s %s\n", "dir1", "dir2";
                printf  "     %-${len7}s %s\n", "-"x3, "-"x3;
                foreach (1..$cnt) {
                    if ($_ > $cnt1) {
                        printf " "x$len7 ."  %2d. %s\n",
                        $_, basename($unmatched2[$_-1]);
                    }
                    elsif ($_ > $cnt2) {
                        printf " %2d. %-${len3}s\n",
                        $_, basename($unmatched1[$_-1]);
                    }
                    else {
                        printf " %2d. %-${len3}s %2d. %s\n",
                        $_, basename($unmatched1[$_-1]),
                        $_, basename($unmatched2[$_-1]);
                    }
                }
            }
            $menu = 0;

            print "\n -1. Reprint menu\n";
            print   "  0. Exit menu\n";

            if ($f1) {
                print "\nchoose file from dir1 (1-$cnt1): $f1\n";
            }
            else {
                {
                    $f1 = query("\nchoose file from dir1 (1-$cnt1):", $dflt1++);
                    if (($f1 !~ /^-?\d+$/) or ($f1 < -1) or ($f1 > $cnt1)) {
                        $dflt1--;
                        redo;
                    }
                }
                if ($f1 == -1) { $menu = 1; $f1 = ""; $dflt1--; redo }
                if ($f1 ==  0) { last }
            }
            $dflt2 = $f1 - $delta;
            $dflt2 = $cnt2 if $dflt2 > $cnt2;

            {
                $f2 = query("choose file from dir2 (1-$cnt2):", $dflt2);
                redo if ($f2 !~ /^-?\d+$/) or ($f2 < -1) or ($f2 > $cnt2);
            }
            if ($f2 == -1) { $menu = 1; $f1 = ""; $dflt1--; redo }
            if ($f2 ==  0) { last }
            $delta = $f1 - $f2;

            $file1 = $unmatched1[$f1-1];
            $file2 = $unmatched2[$f2-1];

            # cannot compare $file1 to more than one $file2;
            # remove duplicate $file1 entries from %diffs{T,B}
            #-------------------------------------------------
            %RdiffsT = reverse %diffsT;
            if ($key = $RdiffsT{$file1}) {
                delete $diffsT{$key};
                %diffsT = reindex(\%diffsT);
                $indexT--;
            }
            %RdiffsB = reverse %diffsB;
            if ($key = $RdiffsB{$file1}) {
                delete $diffsB{$key};
                %diffsB = reindex(\%diffsB);
                $indexB--;
            }

            # add new entries
            #----------------
            $different{$file1} = $file2;
            if (text($file1, $file2)) { $diffsT{++$indexT} = $file1 }
            else                      { $diffsB{++$indexB} = $file1 }

            $dflt1 = $f1 + 1;
            $dflt1 = 0 if $dflt1 > $cnt1;

            $f1 = "";
            $f2 = "";
        }
        show_binary_diffs(%diffsB) if %diffsB;
        show_text_diffs(%diffsT) if %diffsT;
    }
}

#=======================================================================
# name - show_file_counts
# purpose - show number of files in each of the two directories being compared
#
# input parameter
# => $flag: flag indicating whether to pause afterwards (=0 for pause)
#=======================================================================
sub show_file_counts {
    my ($flag, $len1, $len2, $max, $fmt);

    $flag = shift @_;

    $len1 = length($dir1);
    $len2 = length($dir2);

    if ($len1 > $len2) { $max = $len1 }
    else               { $max = $len2 }
    $fmt = "%s: %-${max}s (%d files)\n";

    underline("Directory file counts");
    printf $fmt, "dir1", $dir1, scalar(@files1);
    printf $fmt, "dir2", $dir2, scalar(@files2);
    print "\n";

    pause() unless $flag;
}

#=======================================================================
# name - list_files
# purpose - display list of files in the two directories being compared
#=======================================================================
sub list_files {
    my ($fmt, $num, $base);

    $fmt = "%2d. %s\n";

    # print filenames in dir1
    #------------------------
    if (@files1) {
        underline("dir1: " .$dir1);
        $num = 0;
        foreach (sort @files1) {
            printf $fmt, ++$num, mybase($_, "1");
        }
    } else {
        print "\nNo files in dir1: $dir1\n";
    }
    pause();

    # print filenames in dir2
    #------------------------
    if (@files2) {
        underline("dir2: " .$dir2);
        $num = 0;
        foreach (sort @files2) {
            printf $fmt, ++$num, mybase($_, "2");
        }
    } else {
        print "\nNo files in dir2: $dir2\n";
    }
    pause();
}

#=======================================================================
# name - choose_subdir
# purpose - choose which subdirectory to compare
#=======================================================================
sub choose_subdir {
    my ($opt, $cnt, $dflt);

    # short-circuit if no common subdirectories
    #------------------------------------------
    unless (@subdirs) {
        print "\nNo common subdirectories found.\n";
        pause();
        return;
    }

    # choose subdirectory to compare
    #-------------------------------
    $dflt = 1;
    while (1) {
        underline("Directories");
        print "$dir1\n"
            . "$dir2\n";

        $cnt = 0;
        underline("Which subdirectory do you want to compare?",2);
        foreach (@subdirs) {
            printf "%2d. %s\n", ++$cnt, $_;
            $dflt = $cnt if $_ eq "run";
        }
        print "\n";
        print " 0. previous menu\n";
        print "\n";
        $opt = query("choose", $dflt);
        return if $opt eq "0";

        unless ($subdirs[$opt-1]) {
            print "\n$opt: Invalid option; Try again.\n\n";
            next;
        }
        $subdir = $subdirs[$opt-1];
        last;
    }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         UTILITY subroutines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#=======================================================================
# name - baselen
# purpose - get length of basename of a variable
#
# input parameters
# => $name: pathname of variable
#=======================================================================
sub baselen {
    my $name = shift @_;
    return length(mybase($name, "1"));
}    

#=======================================================================
# name - cleanDirName
# purpose - remove the final slash from a directory name and find
#           the absolute path
#
# input parameters
# => $dir:  name of directory
# => $flag: return abbreviated name if ==1
#=======================================================================
sub cleanDirName {
    my ($dir, $flag, $abspath);

    $dir  = shift @_;
    $flag = shift @_;
    while ($dir =~ m[^(.*[^/])/+$]) { $dir = $1 };
    
    $abspath = abs_path($dir);
    $dir_display{$abspath} = $dir if $flag;
    return $abspath;
}

#=======================================================================
# name - display
# purpose - display path as given rather as absolute path
#
# input parameters
# => $name: filename to display
# => $flag: flag indicating whether files come from $dir1 or $dir2
#=======================================================================
sub display {
    my $name = shift @_;
    my $flag = shift @_;

    if    ($flag == 1) { $name =~ s|$dirA|$dir_display{$dirA}| }
    elsif ($flag == 2) { $name =~ s|$dirB|$dir_display{$dirB}| }

    return $name;
}

#=======================================================================
# name - dmget
# purpose - send job to dmget files prior to comparing them
#
# input parameters
# => $flag: flag indicating whether files come from $dir1 or $dir2
# => @arr: array of files to dmget
#=======================================================================
sub dmget {
    my ($flag, @arr, $str, $name, $cnt, $max, $pid, $cmd);

    return unless -x $dmgetcmd;

    $flag = shift @_;
    @arr  = @_;
    
    $cnt = 0;
    $max = 150;

    # get list of files in archive directory
    #---------------------------------------
    $str = "";
    while (@arr) {
        $name = shift @arr;
        if (abs_path(dirname($name)) =~ /archive/) {
            $str .= " " .display($name, $flag);
            $cnt++;
        }

        # fork job to dmget files
        #------------------------
        if ($cnt > $max or (! @arr and $cnt)) {
            $cmd = "$dmgetcmd $str >& /dev/null";
            print "$cmd\n" if $verbose;
            defined($pid = fork) or die ">> Error << while forking: $!";
            unless ($pid) {
                exec $cmd;
                die ">> Error << $dmgetcmd command not executed: $!";
            }
            $str = "";
            $cnt = 0;
        }
    }
}

#=======================================================================
# name - getexpid
# purpose - extract a guess at the expid from list of file names
#
# input parameter
# => @arr: list of file names
#=======================================================================
sub getexpid {
    my (@arr, $expid, $max);
    my ($fullname, $name, @dummy, %count);

    @arr = @_;
    $expid = "";
    $max = -99;
    $expid = "";

    foreach $fullname (@arr) {
        ($name, @dummy) = split /[.]/, basename $fullname;
        $count{$name}++;
    }
    foreach $name (sort keys %count) {
        if ($count{$name} == $max) {
            $expid = "";
            next;
        }
        if ($count{$name} > $max) {
            $expid = $name;
            $max = $count{$name};
        }
    }
    return $expid;
}

#=======================================================================
# name - in
# purpose - determine whether value is in an array
#
# input parameters
# => $value: value to search for in an array
# => @array: array to search
#=======================================================================
sub in {
    my ($value, @array, $flag);

    $value = shift @_;
    @array = @_;

    $flag = 0;
    foreach (@array) {
        if ($_ eq $value) { $flag = 1; last }
    }
    return $flag;
}

#=======================================================================
# name - listdump
# purpose - dump sorted contents of array to file
#
# input parameters
# => $arrAddr: address of array to dump
# => $fname:   name of file where the contents get written
# => $exclude: string to exclude from the display
#=======================================================================
sub listdump {
    my ($arrAddr, @arr, $fname, $name, $exclude);
    $arrAddr = shift @_;
    $fname = shift @_;
    $exclude = shift @_;
    @arr = @$arrAddr;

    open OUTFL, "> $fname" or die "Error opening file, $fname;";
    foreach $name (sort @arr) {
        $name =~ s/$exclude//;
        print OUTFL "$name\n";
    }
    close OUTFL;
}

#=======================================================================
# name - maxLength
# purpose - find maximum length of strings in an array
#
# input parameters
# => $arrAddr: address of array containing strings
# => $flag: (optional) if eq "basename" then find maximum length of
#                      string basenames
#=======================================================================
sub maxLength {
    my ($arrAddr, $flag, @arr, $string);
    my ($length, $max);

    $arrAddr = shift @_;
    $flag = shift @_;
    @arr = @$arrAddr;

    $max = 0;
    foreach (@arr) {
        if ($flag eq "basename") { $string = basename $_ }
        else                     { $string = $_ }
        $length = length($string);
        $max = $length if $length > $max;
    }
    return $max;
}

#=======================================================================
# name - mybase
# purpose - return filename with specified directory name removed
#
# note: This is not the same as taking the basename, since the basename
#       function will remove all the directories preceding the last name,
#       where this function will only remove the directory which is being
#       compared.
#
# input parameters
# => $name: full name of file, including path
# => $flag: flag indicating which directory path to remove from $name
#             if $flag eq "1", then remove $dir1
#             if $flag eq "2", then remove $dir2
#=======================================================================
sub mybase {
    my ($name, $flag, $ddir, $base);

    $name = shift @_;
    $flag = shift @_;

    if ($flag eq "1") {
        $ddir = display($dir1,1);
        $ddir = "\'.\'" if $ddir eq ".";
        $name =~ s/$dir1\///;
        $name =~ s/$ddir\///;
    }
    elsif ($flag eq "2") {
        $ddir = display($dir2,2);
        $ddir = "\'.\'" if $ddir eq ".";
        $name =~ s/$dir2\///;
        $name =~ s/$ddir\///;
    }
    $name =~ s/^$ENV{"TMPDIR"}\///;
    return $name;
}

#=======================================================================
# name - namechange
# purpose - substitute patterns into a filename;
#           return the new filename, if the file exists
#
# input parameters
# => $name: name of file before name change
#=======================================================================
sub namechange {
    my ($name, $namesave, $flag);
    my ($dir, $base);

    $name = shift @_;
    $namesave = $name;

    # check each pattern substitution individually
    #---------------------------------------------
    foreach (0..$#p1) {
        last if -e $name;
        $dir = dirname $namesave;
        $base = basename $namesave;
        $base =~ s/\b$p1[$_]\b/$p2[$_]/g;
        $name = "$dir/$base";
    }

    # check cumulative pattern substitution
    #--------------------------------------
    unless (-e $name) {
        $name = $namesave;
        foreach (0..$#p1) {
            last if -e $name;
            $dir = dirname $name;
            $base = basename $name;
            $base =~ s/\b$p1[$_]\b/$p2[$_]/g;
            $name = "$dir/$base";
        }
    }

    return $name;
}

#=======================================================================
# name - numeric
# purpose - used with perl sort command to do a numeric sort
#=======================================================================
sub numeric {
    return  1 if $a > $b;
    return -1 if $a < $b;
}

#=======================================================================
# name - pause
# purpose - pause processing until user input is detected
#=======================================================================
sub pause {
    my $dummy;
    print "\nHit <CR> to continue ... ";
    $dummy = <STDIN>;
}

#=======================================================================
# name - reindex
# purpose - for a hash which uses an index number for the key, renumber
#           the keys (indices) if one has been removed
#
# input parameters
# => $hashAddr: the address of the hash to reindex
# => $first: first index of reindexed hash
#
# return value
# => %new: reindexed hash
#=======================================================================
sub reindex {
    my ($hashAddr, %myHash, $first, $next, %new);

    $hashAddr = shift @_;
    %myHash = %$hashAddr;

    $first = shift @_;
    $first = 1 unless $first;

    $next = $first;
    foreach (sort keys %myHash) { $new{$next++} = $myHash{$_}}

    return %new;
}

#=======================================================================
# name - system_
# purpose - call system command; print command to STDOUT if $debug
#
# input parameters
# => $cmd: command to send to system
# => $vflg: verbose flag; print $cmd to STDOUT if $vflg == 1
#=======================================================================
sub system_ {
    my ($cmd, $vflg, $status);
    $cmd = shift @_;
    $vflg = shift @_;
    print "$cmd\n" if $verbose or $vflg;
    ($status = system $cmd) /= 256;
    print "status = $status\n" if $debug;
    return $status;
}

#=======================================================================
# name - text
# purpose - determine whether both files are text (i.e. viewable)
#
# input parameters
# => $file1: 1st file
# => $file2: 2nd file
#=======================================================================
sub text {
    my ($file1, $file2, $type1, $type2, $txtflag);
    $file1 = shift @_;
    $file2 = shift @_;

    $txtflag = 0;
    $type1 = `file -L $file1`;
    $type2 = `file -L $file2`;

    $txtflag = 1 if ($type1=~/ASCII/ or $type1=~/text/ or $type1=~/source/)
        and         ($type2=~/ASCII/ or $type2=~/text/ or $type2=~/source/);

    return $txtflag;
}

#=======================================================================
# name - underline
# purpose - prints a string to stdout and underlines it
#
# input parameters
# => string: the string to underline
# => flag: (optional); defaults to =1
#           =1: underline only with '-'
#           =2: underline and overline with '='
#=======================================================================
sub underline {
    my ($string, $flag);
    my (%pattern, $cnt);

    $string = shift @_;
    $flag = shift @_;

    $pattern{1} = "-";
    $pattern{2} = "=";

    $flag = 1 unless $flag;
    $flag = 1 unless $flag == 2;

    $cnt = length($string);
    print "\n";
    print $pattern{$flag}x$cnt."\n" if $flag == 2;
    print $string."\n";
    print $pattern{$flag}x$cnt."\n";
}

#=======================================================================
# name - wanted
# purpose - collect file names in find function from File::Find
#=======================================================================
sub wanted {
    push @files, $File::Find::name;
}

#=======================================================================
# name - Xcluded
# purpose - identify files that are to be excluded
#=======================================================================
sub Xcluded {
    my ($name, $Xclude);

    $name = shift @_;

    $Xclude = 0;
    foreach (@exclude) {
        if ($name =~ /$_/) { $Xclude = 1; last }
    }
    return $Xclude;
}

#=======================================================================
# name - usage
# purpose - print script usage information
#=======================================================================
sub usage {    
    my $script = basename $0;
    print << "EOF";
Usage: $script dir1 dir2 [options]
where
  dir1 = first directory being compared
  dir2 = second directory being compared

options
  -bwi               ignore blanks, white space, and case when doing file diffs
  -cvsX              do not compare CVS files: Entries, Root, Tag
  -ext extension     compare all files with this extension (recursive)
  -etc               shortcut for "-subdir etc -r"
  -fcst              shortcut for "-subdir fcst -r"
  -h(elp)            print usage information
  -id fileID         compare all files with \"fileID\" as part of its filename
                     (see Note 1)
  -list              compare list of file names in dir1 and dir2 (rather than
                     comparing the actual files)
  -listx             same as -list, except ignore dir and expid name differences
  -q                 quiet mode
  -r                 recursively compare any subdirectories found
  -rs                shortcut for "-subdir rs -r"
  -run               shortcut for "-subdir run -r"
  -subdir name       start comparison in specified subdirectory
  -v                 verbose mode
  -X str             exclude filenames which include str (see Note 1)

pattern options
  -p1 pattern1       ignore these pattern differences in dir1/dir2 filenames
  -p2 pattern2       (see Notes 1-3)
or
  -p pattern1=pattern2

Notes:
1. Multiple values can be given for fileID (-id), pattern1 (-p1), pattern2 (-p2),
   and str (-X) by separating values with a comma (no space) or by multiple uses
   of the option flag.
2. Multiple pattern1=pattern2 values can be given by multiple uses of the -p flag
3. There must be a matching pattern2 for each pattern1, and vice versa.

EOF
exit;
}
