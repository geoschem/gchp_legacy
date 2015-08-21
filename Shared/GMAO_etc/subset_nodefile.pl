#!/usr/bin/env perl
#=======================================================================
# name - subset_nodefile.pl
# purpose - print a subset of nodes from a node file
#
# Revision History
# 20110707  J.Stassi   Initial version of code
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($infil, $outfl, $ncpus, $ppn);
my (@nodeARR, $index, $single);

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
    my ($front, $back, $help);

    GetOptions( "f=s"     => \$infil,
                "o=s"     => \$outfl,
                "ncpus=i" => \$ncpus,
                "ppn=i"   => \$ppn,
                "front"   => \$front,
                "back"    => \$back,
                "index=i" => \$index,
                "single!" => \$single,
                "h|help"  => \$help );
    usage() if $help;

    $infil = $ENV{PBS_NODEFILE} unless $infil;

    # determine index into the node array
    #------------------------------------
    unless (defined($index)) {
        $index = -1 if $back;
        $index =  0 if $front;
        $index =  0 unless defined($index);
    }

    # check inputs
    #-------------
    die "Error. Input file not defined;" unless $infil;
    die "Error. Input file not found: $infil;" unless -e $infil;

    die "Error. Must supply # of CPUs with -n flag;" unless $ncpus;
    die "Error. Must supply # of processes per node with -ppn flag;" unless $ppn;
}

#=======================================================================
# name - read_infil
# purpose - read available nodes from input node file
#=======================================================================
sub read_infil {
    my ($node, %found, $max);

    @nodeARR = ();
    %found = ();

    open INFIL, "< $infil" or die "Error. Opening input file; $infil;";
    foreach $node (<INFIL>) { push @nodeARR, $node unless $found{$node}++ }
    close INFIL;

    $max = 1;
    foreach (keys %found) { $max = $found{$_} if $max < $found{$_} }

    unless (defined($single)) {
        if ($max == 1) { $single = 1 }
        else           { $single = 0 }
    }
    warn "WARNING. Requesting ppn=$ppn, but nodes listed in input"
        . " node file only $max times;" if $max > 1 and $max < $ppn;
}

#=======================================================================
# name - print_output
# purpose - print the subsetted nodes
#=======================================================================
sub print_output {
    my ($available, $first, $last, $num, $cnt);

    # check that requested processes are avaiable
    #--------------------------------------------
    $available = scalar(@nodeARR) * $ppn;
    die "Error. Requesting more CPUs ($ncpus)"
        . " than are available ($available);" if $ncpus > $available;

    # get first and last index into @nodeARR
    #---------------------------------------
    if ($index == -1) { $first = scalar(@nodeARR) - ($ncpus / $ppn) }
    else              { $first = $index }
    $last = $first - 1 + ($ncpus / $ppn);

    $first = int($first);
    $last = int($last + 1) unless $last == int($last);

    # check for reasonable indices
    #-----------------------------
    if (($first < 0) or ($last > $#nodeARR)) {
        die   "Error. Available node indices = [0, $#nodeARR]\n"
            . "Requested node indices: first = $first, last = $last\n"
            . "Node Indexing out of range;";
    }

    # print output nodes
    #-------------------
    if ($outfl) {
        open OUTFL, "> $outfl" or die "Error. Opening output file, $outfl;";
        select OUTFL;
    }

    if ($single) { $num = 1    }
    else         { $num = $ppn }

    $cnt = 1;
    foreach $index ($first..$last) {
        foreach (1..$num) {
            last if $cnt++ > $ncpus;
            print $nodeARR[$index];
        }
    }
    close OUTFL if $outfl;
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

Description: print a subset of nodes from node file

Usage: $name -ncpus ncpus -ppn ppn [OPTIONS]
       $name [-h]

REQUIRED FLAGS
   -ncpus ncpus    number of CPUs for output node file
   -ppn ppn        number of processes per node for output node file

I/O OPTIONS
   -f filename     name of input node file; Defaults to environment
                   variable, \$PBS_NODEFILE, if variable is defined
   -o filename     name of output node file; Defaults to STDOUT
   -[no]single     specify output print mode
                      -single   => print each node as single entry in output
                      -nosingle => print each node ppn times in output
                   Defaults to same mode as input if not specified

NODE SELECTION OPTIONS
   -front          get nodes from the beginning of the input list (Default)
   -back           get nodes from the end of the input list
   -index n        start subsetting at n\'th node (first index is 0)

  The "index n" flag supercedes "-front" which supercedes "-back".

OTHER OPTIONS
   -h               print usage information

EOF
exit;
}
