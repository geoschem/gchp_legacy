#!/usr/bin/env perl
#=======================================================================
# name - subset_nodefile.pl
# purpose - print a subset of nodes from a nodefile
#
# key global variables
# => $infil: input nodefile (defaults to $PBS_NODEFILE)
# => $outfil: output nodefile to which subsetted list is written
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
my ($infil, $outfil, $back, $index, $ppnIN, $ppn, $outflg, $debug);
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
    my ($front, $help);

    # get input options
    #------------------
    GetOptions( "f=s"      => \$infil,
                "o=s"      => \$outfil,
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

    # check for error conditions
    #---------------------------
    die "Error. Input file not defined;" unless $infil;
    die "Error. Input file not found: $infil;" unless -e $infil;
    die "Error. Must supply # of CPUs with -n flag;" unless $cpusOUT;
    die "Error. Total CPUs ($cpusOUT) does not divide evenly by ppn ($ppn):"
        if $ppn and ($cpusOUT % $ppn != 0);
}

#=======================================================================
# name - read_infil
# purpose - read available nodes from input nodefile
#=======================================================================
sub read_infil {
    my ($singleIN);

    # read input node list
    #---------------------
    %cpusPerNode = ();
    open INFIL, "< $infil" or die "Error. Opening input file; $infil;";
    foreach (<INFIL>) {
        chomp;
        next unless $_;
        push @nodeList, $_ unless $cpusPerNode{$_};
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
    @nodeList = sort @nodeList;
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
    my ($step, $cnt, $perNode, $ind);
    my ($node, @cpuList, %deja, $msg, $FH);
    my (%cpusLeftPerNode, %cpusLeftPerNodeALL, $cpusLEFT, $cpusLEFTall);

    # determine @nodeList step value and starting index 
    #---------------------------------------------------
    if ($back) { $step = -1; $index = $#nodeList unless defined($index) }
    else       { $step =  1; $index = 0          unless defined($index) }

    # initialize counters and array index
    #------------------------------------
    $cnt = 0;
    $perNode = 0;
    $ind = $index;

    %cpusLeftPerNode    = %cpusPerNode;
    %cpusLeftPerNodeALL = %cpusPerNode;

    while (1) {
        $node = $nodeList[$ind];

        # skip node if it does not have enough CPUs per node
        #---------------------------------------------------
        if ($ppn and $cpusPerNode{$node} < $ppn) {
            $ind += $step;
            next;
        }

        # add node to @cpuList
        #---------------------
        if ($back) { unshift @cpuList, $node unless $singleOUT and $deja{$node} }
        else       { push    @cpuList, $node unless $singleOUT and $deja{$node} }

        $cpusLeftPerNode{$node} = 0;
        $cpusLeftPerNodeALL{$node}--;

        # increment counters
        #-------------------
        $cnt++;
        $perNode++;
        $deja{$node}++;

        # we are done if we have reached the number of requested CPUs
        #------------------------------------------------------------
        last if $cnt >= $cpusOUT;

        # move to next node, if at @cpusPerNode or $ppn limit
        #----------------------------------------------------
        if ($perNode >= $cpusPerNode{$node} or ($ppn and ($perNode >= $ppn))) {
            $ind += $step;
            $perNode = 0;
        }

        # die if @nodeList index is out of bounds
        #----------------------------------------
        if (($ind < 0) or ($ind > $#nodeList)) {
            $msg = "Error. Cannot allocate $cpusOUT cpus from $infil;";
            $msg .= " ppn = $ppn;" if $ppn;
            $msg .= " back = $back;" if $back;
            die $msg;
        }
    }

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

        foreach $node (@nodeList) {
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
        print STDERR "debug: outflg      = $outflg\n\n";
    }
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
