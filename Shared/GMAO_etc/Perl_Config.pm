package Perl_Config;
#=======================================================================
# name - Perl_Config
# purpose - This package provides a subroutine to set environment variables
#           defined in a configuration file (which are normally set in the
#           csh environment by sourcing the file).
#
# note: The motivation for this package is to allow perl programs to set
#       the environment variables defined in the FVDAS_Run_Config file in
#       the experiment run directory.
#=======================================================================
use strict;
use warnings;

require 5.000;
require Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(perl_config);

#=======================================================================
# name - perl_config
# purpose - set environment variables defined in a configuration file,
#           e.g. the experiment FVDAS_Run_Config file
#
# input parameters
# => %options: hash containing run options
#     where
#       $options{"config_file"} = configuration file to "source"
#       $options{"debug"} = 1 (turns on debug feature)
#
# notes:
# 1. The config_file defaults to "FVDAS_Run_Config" if not provided in the
#    %options hash.   
#=======================================================================
sub perl_config {
    my (%options, $config_file, $debug);
    my ($var, $val, @var0, @var1);

    # get options
    #------------
    %options = @_;

    $config_file = $options{"config_file"};
    $config_file = "FVDAS_Run_Config" unless $config_file;
    $debug = $options{"debug"};

    # open config file
    #-----------------
    open CF, "< $config_file" or die "Error opening $config_file:$!;";
    while (<CF>) {

        # parse setenv statements
        #------------------------
        ($var, $val) = /^\s*setenv\s+(\S+)\s+(.+)\s*$/;
        next unless defined($var) and defined($val);

        #--------------------------------------
        # - expand variables
        # - remove quotes around strings
        # - evaluate back-ticked expressions
        #--------------------------------------
        $val = expand($val);
        $val =~ s/^\"|\"$//g;
        $val =~ s/^\'|\'$//g;
        chomp($val = eval $val) if $val =~ /`/;

        # set environment variable
        #-------------------------
        $ENV{$var} = $val;
    }
    close CF;
    debug_check() if $debug;
}

#=======================================================================
# name - expand
# purpose - expand variables within a text string
#
# input argument
# => $strIN: string to expand
#
# return value
# => $strOUT: expanded string
#=======================================================================
sub expand {
    my ($strIN, $strOUT);
    my ($nn, @vars, $var, $varname, $val_ENV, $val_eval);

    $strIN = shift @_;
    $strOUT = $strIN;

    # make list of variables embedded in $strIN
    #------------------------------------------
    for $nn (0..length($strIN)-1) {
        if (substr($strIN,$nn) =~ /^(\$\w+)/)    { push @vars, $1 }
        if (substr($strIN,$nn) =~ /^(\$\{\w+})/) { push @vars, $1 }
    }

    # if variables have values, then substitute
    #------------------------------------------
    foreach $var (@vars) {

        ($varname = $var) =~ s/[\$\{\}]//g;
        $val_ENV  = $ENV{$varname};
        $val_eval = eval($var);

        $var =~ s/([\$\{])/\\$1/;    # change '$' => '\$' and '{' => '\{'

        $strOUT =~ s/$var/$val_ENV/  if defined($val_ENV);
        $strOUT =~ s/$var/$val_eval/ if defined($val_eval);
    }
    return $strOUT;
}

#=======================================================================
# name - debug_check
# purpose -
#=======================================================================
sub debug_check {
    my ($var);

    while (1) {
        print "variable ('0' to exit debug): ";
        chomp($var = <STDIN>);
        last if $var eq "0";
        print "$var = #$ENV{$var}#\n\n";
    }
}
1;
