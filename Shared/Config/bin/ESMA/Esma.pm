package Esma;
#
#  This module implements a base class for creating driving scripts for
#  ESMA applications.
#
#---
#
#  This program is free software; you can redistribute it and/or modify it 
#  under the same terms as Perl itself.
#
#  THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
#  WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
#  OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE
#
#  REVISION HISTORY:
#
#  28Oct2006  da Silva  Initial implementation loosely based on the 
#                       functionality of GEOS-4 "fvpsas" script, but
#                       adopting an OO approach.
#
#--------------------------------------------------------------------------

require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(new setup}

#.........................................................................

 use FileHandle;
 use Carp;
 use Env;                 # Environment variables

 $VERSION = 1.00;

# The public ESMA object
# ----------------------
my %esma = {

         ExpID => undef,  # Experiment ID
      ExpDescr => undef,  # Experiment description 

         ExpCF => {},     # User supplied configuration parameters

    ExpHomeDir => undef,  # Where the experiment scripts, recycles reside
                          # (same as FVHOME in GEOS-4)
    ExpExecDir => undef,  # Where the job actually runs (FVWORK)
    ExpParkDir => undef,  # Truck stop before mass storage destination 
    ExpArchDir => undef,  # Mass storage destination 

       EsmaDir => undef,  # Where system binaries have been installed 

      Restarts => [],     # list of restarts to be brought over
      Resources => [],    # list of resources to be brought over
      Children => [],     # children components

         Debug => undef,  # whether to print out debug info 

};

#.........................................................................

sub new {                                                 # Constructor

    my $that = shift;
    my $class = ref($that) || $that;

    my ( $argv ) = @_;

#   Record input args
#   -----------------
    $esma{ExpID}      = $argv->{ExpID};
    $esma{ExpDescr}   = $argv->{ExpDescr};
    $esma{ExpCF}      = $argv->{ExpCF};

    $esma{ExpHomeDir} = $argv->{ExpHomeDir};
    $esma{ExpExecDir} = $argv->{ExpExecDir};
    $esma{ExpParkDir} = $argv->{ExpParkDir};
    $esma{ExpArchDir} = $argv->{ExpArchDir};
    $esma{EsmaDir}    = $argv->{EsmaDir};

    $esma{Restarts}   = $argv->{Restarts};
    $esma{Resources}  = $argv->{Resources};
    $esma{Children}   = $argv->{Children};

    $esma{Debug}      = $argv->{Debug};

#   Bless it
#   --------
    $self = { %ema, };
    bless $self, $class;

    return $self;

}

#..........................................................................

sub setup {                          # Prepares the run environment
   my $self = shift;

}

#..........................................................................

sub acquire {                        # Acquires necessary data
   my $self = shift;

}

#..........................................................................

sub run {                            # Executes the poor component
   my $self = shift;

}

#..........................................................................

sub deliver {                        # Archives/disposes of run output
   my $self = shift;

}

#..........................................................................

sub DESTROY {                                               # Destructor

   my $self = shift;

   print "Destroyed Esma object associated with $self->{file}\n" 
         if ( $self->{debug} );

   $self->{ExpID}  = undef;
   
}

#..........................................................................
