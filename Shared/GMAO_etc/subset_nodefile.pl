#!/usr/bin/env perl
#=======================================================================
# name - subset_nodefile.pl
# purpose - print a subset of nodes from a nodefile
#
# key global variables
# => $infil: input nodefile (defaults to $PBS_NODEFILE)
# => $outfil: output nodefile to which subsetted list is written
# => $index0: zero-based index into @nodeList (-index input option is one-based)
# => @nodeList: sorted list of nodes from input nodelist (each node listed once)
# => %cpusPerNode: number of CPUs per node on input nodelist; keys = @nodeList
#
# output (see -outflg options)
# => $cpusLEFT: number of remaining CPUs not included in subsetted list
# => $cpusLEFTall: number of all remaining CPUs
#
# Revision History
# 20110707  J.Stassi   Initial version of code
# 20150327  J.Stassi   Updated for SLURM which can have varying number of 
#                      processes per input node on $PBS_NODEFILE
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($infil, $outfil, $back, $index0, $ppnIN, $ppn, $outflg, $debug);
my (@nodeList, %cpusPerNode);
my ($cpusIN, $cpusOUT, $singleOUT);

# main program
#-------------
{
    init();
    read_infil();
    print_output();
}

#=======================================================================
# name - init
# purpose - get runtime flags and arguments
#=======================================================================
sub init {
    use Getopt::Long;
    my ($index1, $help);

    # get input options
    #------------------
    GetOptions( "f=s"      => \$infil,
                "o=s"      => \$outfil,
                "index=i"  => \$index1,
                "b|back"   => \$back,
                "ppnIN=i"  => \$ppnIN,
                "ppn=i"    => \$ppn,
                "single!"  => \$singleOUT,
                "outflg:s" => \$outflg,
                "db|debug" => \$debug,
                "h|help"   => \$help );
    usage() if $help;

    # get input parameter
    #--------------------
    $cpusOUT = shift @ARGV;

    # default values
    #---------------
    $infil = $ENV{PBS_NODEFILE} unless $infil;
    $outflg = 1 if defined($outflg) and $outflg eq "";
    $outflg = 0 unless $outflg;
    $index0 = $index1 - 1 if $index1;

    # check for error conditions
    #---------------------------
    die "Error. Input file not defined;" unless $infil;
    die "Error. Input file not found: $infil;" unless -e $infil;
    die "Error. Must supply # of output CPUs;" unless $cpusOUT;
    die "Error. Total CPUs ($cpusOUT) does not divide evenly by ppn ($ppn):"
        if $ppn and ($cpusOUT % $ppn != 0);
}

#=======================================================================
# name - read_infil
# purpose - read available nodes from input nodefile
#=======================================================================
sub read_infil {
    my ($singleIN, $node);

    # read input node list
    #---------------------
    %cpusPerNode = ();
    open INFIL, "< $infil" or die "Error. Opening input file; $infil;";
    foreach (<INFIL>) {
        chomp;
        next unless $_;
        $cpusPerNode{$_}++;
        $cpusIN++;
    }
    close INFIL;

    # determine format of input nodefile
    # i.e. nodes listed once for each process or just once
    #-----------------------------------------------------
    $singleIN = 0;
    if ($cpusIN == scalar(keys %cpusPerNode)) {
        die "Error. User must supply ppnIN if input NODEFILE is single entry;"
            unless $ppnIN;

        $singleIN = 1;
        $cpusIN *= $ppnIN;
        foreach (keys %cpusPerNode) { $cpusPerNode{$_} = $ppnIN }
    }

    # create node list
    #-----------------
    foreach $node (sort(keys %cpusPerNode)) {
        foreach (1..$cpusPerNode{$node}) { push @nodeList, $node }
    }
    $singleOUT = $singleIN unless defined($singleOUT);

    # check that requested processes are available
    #--------------------------------------------
    die "Error. Requesting more CPUs ($cpusOUT)"
        . " than are available ($cpusIN);" if $cpusOUT > $cpusIN;
}

#=======================================================================
# name - print_output
# purpose - print the subsetted nodes
#=======================================================================
sub print_output {
    my ($step, $cntALL, %cnt, $perNode, $ind, $index1);
    my ($node, @cpuList, %deja, $msg, $FH);
    my (%cpusLeftPerNode, %cpusLeftPerNodeALL, $cpusLEFT, $cpusLEFTall);

    # determine @nodeList step value and starting index 
    #---------------------------------------------------
    if ($back) { $step = -1; $index0 = $#nodeList unless defined($index0) }
    else       { $step =  1; $index0 = 0          unless defined($index0) }
    $index1 = $index0 + 1;

    # initialize counters and array index
    #------------------------------------
    $cntALL = 0;
    %cnt = ();
    %deja = ();

    %cpusLeftPerNode    = %cpusPerNode;
    %cpusLeftPerNodeALL = %cpusPerNode;

    $ind = $index0;

    while (1) {
        errorMSG() if $ind < 0 or $ind > $#nodeList;

        $node = $nodeList[$ind];
        $ind += $step;

        if ($ppn) {
            next if $cpusPerNode{$node} < $ppn;
            next if $cnt{$node} and $cnt{$node} >= $ppn;
        }
        push @cpuList, $node unless $singleOUT and $deja{$node};

        $cpusLeftPerNode{$node} = 0;
        $cpusLeftPerNodeALL{$node}--;

        $cnt{$node}++;
        $deja{$node}++;
        $cntALL++;

        last if $cntALL >= $cpusOUT;
    }
    errorMSG() if $cntALL < $cpusOUT;
    @cpuList = sort(@cpuList);

    # write @cpuList to output file
    #------------------------------
    if ($outfil) {
        open OUTFIL, "> $outfil" or die "Error. Opening output file, $outfil;";
        $FH =select;
        select OUTFIL;
    }
    foreach (@cpuList) { print "$_\n" }
    close OUTFIL if $outfil;
    select $FH;

    # print number of remaining CPUs, if requested
    #---------------------------------------------
    if ($outflg or $debug) {
        $cpusLEFT    = 0;
        $cpusLEFTall = 0;

        foreach $node (keys %cpusPerNode) {
            $cpusLEFT    += $cpusLeftPerNode{$node};
            $cpusLEFTall += $cpusLeftPerNodeALL{$node};
        }
        print "$cpusLEFT\n"    if $outflg == 1;
        print "$cpusLEFTall\n" if $outflg == 2;
        $| = 1;
    }

    # print debug info to STDERR
    #---------------------------
    if ($debug) {
        print STDERR "debug: $infil\n";
        print STDERR "debug: $outfil\n";
        print STDERR "debug: cpusIN      = $cpusIN\n";
        print STDERR "debug: cpusOUT     = $cpusOUT\n";
        print STDERR "debug: cpusLEFT    = $cpusLEFT\n";
        print STDERR "debug: cpusLEFTall = $cpusLEFTall\n";
        print STDERR "debug: first index = $index1\n";
        print STDERR "debug: step        = $step\n";
        print STDERR "debug: outflg      = $outflg\n\n";
    }
}
#=======================================================================
# name - errorMSG
# purpose - print error message and die if unable to allocate requested nodes
#=======================================================================
sub errorMSG {
    my ($msg, $index1);
    $index1 = $index0 + 1;
    $msg = "Error. Cannot allocate $cpusOUT cpus from $infil;";
    $msg .= " -index $index1";
    $msg .= " -back" if $back;
    $msg .= " -ppnIN $ppnIN" if $ppnIN;
    $msg .= " ppn = $ppn" if $ppn;
    $msg .= ";";
    die $msg;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename;
    my $name;
    $name = basename $0;

    print <<"EOF";

Description: print a subset of nodes from nodefile

Usage: $name cpusOUT [OPTIONS]
       $name [-h]

where
   cpusOUT is the number of CPUs to write to the output nodefile

OPTIONS
   -f nodeFileIN   Name of input nodefile; Defaults to environment variable,
                   \$PBS_NODEFILE, if it is defined

   -o nodeFileOUT  Name of output nodefile; Defaults to STDOUT

   -index index1   The start index (one-based) for subsetting nodes;
                   defaults to last index for -back flag;
                   otherwise, defaults to 1

   -back           Get nodes from the back of the input list;
                   default is to take nodes from front of input list

   -ppnIN ppn      Number of processes per node on input nodefile;
                   Only required if input nodefile lists each node only once;
                   Otherwise it is ignored and should not be given,
                   (value will be determined by input nodefile list)

   -ppn ppn        Number of processes per node for output nodefile;
                   This flag forces all nodes to have the same number of processes;
                   If this is not a requirement, then it should not be given

   -[no]single     Specify output print mode
                      -single   => print each node only once on output nodefile
                      -nosingle => print each node once for each process on that node
                   defaults to same mode as input if not specified

   -outflg [val]   if val==0, then do not print output
                   if val==1, then print number of remaining CPUs from non-subsetted nodes
                   if val==2, then print number of remaining CPUs from all nodes

                   if -outflg is excluded, then val defaults to 0
                   if val is excluded,     then val defaults to 1

   -h              Print usage information

OUTPUT
   prints the number of remaining CPUs (see -left option)

EOF
exit;
}
