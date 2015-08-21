#!/usr/bin/env perl
#=======================================================================
# name - subst_path.pl
# purpose - This script substitutes a string into a hard-coded path
#           for one or more variable definitions within a file.
#
# revision history
# 06Dec2013  Stassi    initial version of code
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($infile, $outfile, $overwrite, $label, $string, @varARR);

# main program
#-------------
{
    use File::Copy ("move");
    my ($newline, $var, $front, $back, @outlines, $FH);

    init();
    open INFILE, "< $infile" or die "Error opening file: $infile;";

    # read input
    #-----------
    foreach (<INFILE>) {
        $newline = $_;

        # modify variable definitions
        #----------------------------
        foreach $var (@varARR) {
            if (/(^\s*$var\s*[:|:=|=]\s*)\S*$label(.*$)/) {
                $front = $1;
                $back = $2;
                $string =~ s/\/*$// if $back =~ /^\//;
                $newline = "$front$string$back\n";
            }
        }
        push @outlines, $newline;
    }
    close INFILE;

    # write output
    #-------------
    if ($outfile) {
        if ($overwrite) { move $infile, "${infile}~" }
        open OUTFL, "> $outfile" or die "Error opening file: $outfile;";
        foreach (@outlines) { print OUTFL $_ };
        close OUTFL;
    }
    else { foreach (@outlines) { print $_ } }
}

#=======================================================================
# name - init
# purpose - get input parameters
#=======================================================================
sub init {
    use Getopt::Long;
    my ($help);

    GetOptions( "h"   => \$help,
                "i"   => \$overwrite,
                "o=s" => \$outfile );
    usage() if $help;

    # get runtime parameters
    #-----------------------
    usage() if scalar(@ARGV) < 4;
    $infile = shift @ARGV;
    $label = shift @ARGV;
    $string = shift @ARGV;
    @varARR = @ARGV;

    unless ($outfile) { $outfile = $infile if $overwrite }
    die "File not found: $infile;" unless -e $infile;
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {
    use File::Basename ("basename");
    my $script = basename $0;

    print <<"EOF";
purpose: substitute string into hard-coded path for one or more variable
         definitions within a file
usage: $script infile label string var1 [ var2 ... ] [ options ]
where
 infile: input file containing variable definition(s)
  label: label indicating last portion of path definition to substitute out
 string: string to substitute into path definition
   var?: variable(s) containing path definitions

options
   -h         print usage information
   -i         overwrite infile
   -o outfl   output file (defaults to STDOUT)

Example:
if file, CARMchem_Registry.rc, contains the following lines:

  DU_OPTICS:  /discover/nobackup/pcolarco/fvInput/AeroCom/x/carma_optics_DU.v5
  SS_OPTICS:  /discover/nobackup/pcolarco/fvInput/AeroCom/x/carma_optics_SS.v3

then the following command

> $script CARMAchem_Registry.rc fvInput /home/jstassi/e512/fvInput DU_OPTICS SS_OPTICS

will change the lines to the following:

  DU_OPTICS:  /home/jstassi/e512/fvInput/AeroCom/x/carma_optics_DU.v5
  SS_OPTICS:  /home/jstassi/e512/fvInput/AeroCom/x/carma_optics_SS.v3

Note: The script recognizes the following delimiters between a variable name
      and it's definition: :, := , =

EOF
    exit;
}
