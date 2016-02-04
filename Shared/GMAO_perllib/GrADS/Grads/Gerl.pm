package Grads::Gerl;
#
#  Extends Perl with GrADS like commands. Requires GrADS.pm.
#
#  (c) 2006-2008 Arlindo da Silva <dasilva@opengrads.org>
#
#  This program is free software; you can redistribute it and/or modify it 
#  under the same terms as Perl itself.
#
#  THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
#  WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
#  OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE
#
#  !REVISION HISTORY:
#
#  07Mar06  da Silva  First prototype API, only open_ has useful return 
#                     values yet.
#  06Sep2007  da Silva  Documentation, prototype PDL interface
#   
#--------------------------------------------------------------------------

require 5.000;
require Exporter;

@ISA = qw(Exporter);

@EXPORT = qw( Open c clear d display Display  define disable draw
              Exp enable ga_ gaprint gxyat
              grads gradsc gradsnc gradsnc4 gradshdf gradsdods gradsdap
              Imp printim rword rline reinit run sdfopen xdfopen Set set
              Query Q query q quit );

@EXPORT_OK = qw(Ga);

#.........................................................................

BEGIN {
 $VERSION = "0.6.0"; # It will be Version 1 when Query is fully
                     # implemented
 use Carp;
 use Grads;
 use Data::Dumper;

 $Ga = undef;   # the GrADS object

# If under PDL shell, setup prompt and command line filter
# --------------------------------------------------------
  if ( defined($PERLDL::ESCAPE) && "$PERLDL::ESCAPE" ne "" ) {
     eval {
       eval 'require Term::ReadLine';
       if ( $@ eq "" ) { $PERLDL::PROMPT = 'gadl-> '; }
       else            { $PERLDL::PROMPT = 'gadl> ' ; };
       my $filter = sub {
          my $str = shift;
          $str =~ s/^\s+//;  # Strip leading space
          if ($str =~ /^\. /             || 
              $str =~ /^display /        || 
              $str =~ /^define /         || 
              $str =~ /^disable/         || 
              $str =~ /^draw /           || 
              $str =~ /^d /              || 
              $str =~ /^Exp /            || 
              $str =~ /^ga_ /            || 
              $str =~ /^gaprint /        || 
              $str =~ /^gxyat /          || 
              $str =~ /^Open /           || 
              $str =~ /^o /              || 
              $str =~ /^pr /             ||
              $str =~ /^pi /             ||
              $str =~ /^printim /        ||
              $str =~ /^q /              ||
              $str =~ /^Q /              ||
              $str =~ /^Query /          ||
              $str =~ /^run /            ||
              $str =~ /^r /              ||
              $str =~ /^s /              ||
              $str =~ /^Set /            ||
              $str =~ /^enable /         ) {
              my ($command, $args) = split(/\s+/,$str, 2);
              $command = "Open"  if ( $str =~ /^o / );
              $command = "gaprint" if ( $str =~ /^pr / );
              $command = "printim" if ( $str =~ /^pi / );
              $command = "query" if ( $str =~ /^q / );
              $command = "run"   if ( $str =~ /^r / );
              $command = "ga_"   if ( $str =~ /^\. / );
	      if ( $str =~ /^s / ) { # set is taken
		$command = "ga_";
		$args  = "set " . "$args";
              }
              $str = "$command qq($args)"
                    if (defined $args && $args !~ /^\"/);
              print "Gerl translation: $str" if ( $report );
          }; # if Gerl command
          # Return the input string, modified as required
          return $str;
       }; # filtering sub
       my $gafilter = main::preproc_add $filter; # register our filter
      }; # eval PDL customization
     }; # if under PDL shell

# Start GrADS automatically if so requested
# -----------------------------------------
  if ( $ENV{GERL_AUTO} ) {
    $Ga = new Grads;      # starts the grads process.
    confess "cannot start GrADS process" unless ( $Ga );
  }

} # end of BEGIN

#.........................................................................

sub grads {
    my ( $argv ) = @_;
    $Ga = new Grads $argv;      # starts the grads process.
    return 1 unless ( $Ga );
    return 0;
}

sub quit { $Ga = undef;}        # calls the destructor: quits, close pipes

sub ga_ {                       # executes a grads command
    my ( $cmds ) = @_;
    @cmd = split('\n',$cmds);
    foreach $cmd ( @cmd ) {
       $rc = $Ga->cmd($cmd);
       return $rc if ( $rc );
    };
    return 0; 
}

sub define {
    my ( $varn, @expr ) = @_;
    return $Ga->cmd("define $varn = @expr");
}

sub Display { # Displays PDL or GrADS expressions
    my $Expr = shift;
    my $Grid = shift;
    return $Ga->Display($Expr,$Grid);
}

sub Exp {
    my $what = shift;
    my ($var, $grid) = $Ga->Exp($what);
    return ( wantarray ? ($var, $grid) : $var );
}

sub Imp {
    my $name = shift;
    my $var  = shift;
    my $grid = shift;
    my $rc = $Ga->Imp($name,$var,$grid);
    return $rc;
}

sub Open {
    my $fname = shift;
    my $ftype = shift;
    my $fh = $Ga->Open($fname,$ftype);
    return $fh;
}

sub Q {
    my $what = shift;
    my $qh = $Ga->Query($what);
    return $qh;
}

sub Query {
    my $what = shift;
    my $qh = $Ga->Query($what);
    return $qh;
}

sub Set {
    my $what = shift;
    return $Ga->Set($what);
}

sub rline {
    my $m = shift;
    my $line = $Ga->rline($m); 
    return $line;
}

sub rword {
    my $m = shift;
    my $n = shift;
    my $word = $Ga->rword($m,$n); 
    return $word;
}

sub gaprint {
    return $Ga->cmd("print @_");
}

sub AUTOLOAD {
    $this = $AUTOLOAD;
    $this =~ s/.*:://; # trim package name 
    if ( "$this" =~ /^grads.*\b/ ) {
          my ( $argv ) = @_;
          $argv{Bin} = "$this" unless ( $argv{Bin} );
          $Ga = new Grads $argv;    # starts the grads process.
          return 1 unless ( $Ga );
	} 
    else {
      my ( @args ) = @_;
      $cmd = sprintf("$this @args");
      return $Ga->cmd($cmd); 
    }
}

__END__

sub AUTOLOAD {
    my ( @args ) = @_;
    $this = $AUTOLOAD;
    $this =~ s/.*:://; # trim package name 
    $cmd = sprintf("$this @args");
    return $Ga->cmd($cmd);
}



1;

=head1 NAME

Grads::Gerl - Procedural Interface to the GrADS Client Class

=head1 SYNOPSIS

=head3 Main Functions:

=over 4

=item

I<use> B<Grads::Gerl>

=item 

$I<rc> = B<grads> ( [I<OPTIONS>] );

=item 

$I<rc> = B<ga_> ( I<COMMAND> );

=item 

$I<rc> = B<define> ( I<VARNAME>, I<EXPR> );

=item 

$I<rc> = B<Display> ( I<GA_VAR> , I<PERL_VAR>, [I<GRID>] );

=item 

$I<pv> = B<Exp> ( I<EXPR> );

=item 

($I<x>,$I<grid>) = B<Exp> ( I<EXPR> );

=item 

$I<rc> = B<Imp> ( I<GA_VAR> , I<PERL_VAR>, [I<GRID>] );

=item 

$I<rc> = B<gaprint> ( [I<FILENAME>] );

=item 

$I<fh> = B<Open> ( I<FILENAME> [, I<FILETYPE>] );

=item 

$I<qh> = B<Query> ( I<PROPERTY> );

=item 

$I<rc> = B<Set> ( I<PROPERTIES> );

=item 

$I<ln> = B<rline>(I<i>);

=item 

$I<wd> = B<rword>(I<i>,I<j>);

=back

=head3 Autoloaded functions:

=over 4

=item

$I<rc> = B<gradsc> ( [I<OPTIONS>] );

=item

$I<rc> = B<gradsnc> ( [I<OPTIONS>] );

=item

$I<rc> = B<gradsnc4> ( [I<OPTIONS>] );

=item

$I<rc> = B<gradshdf> ( [I<OPTIONS>] );

=item

$I<rc> = B<gradsdap> ( [I<OPTIONS>] );

=item

$I<rc> = B<gradsdods> ( [I<OPTIONS>] );

=item

$I<rc> = B<c>;

=item

$I<rc> = B<clear>;

=item

$I<rc> = B<d> ( [I<EXPR>] );

=item

$I<rc> = B<display> ( [I<EXPR>] );

=item

$I<rc> = B<disable> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<draw> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<enable> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<printim> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<run> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<sdfopen> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<set> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<query> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<q> ( [I<ARGUMENTS>] );

=item

$I<rc> = B<quit>;

=item

$I<rc> = B<xdfopen> ( [I<ARGUMENTS>] );

=back

=head1 DESCRIPTION

C<Grads::Gerl> implements a I<procedural> interface to the Grid Analysis and
Display System (GrADS) by means of bi-directonal pipes.  It starts
GrADS, sends commands to it, parses its output and return codes, and
provides high level interfaces to query GrADS properties and dimension
environment. 

In the remainder of this document we will use C<Grads> and C<Grads::Gerl> to
denote the Perl modules of same name, and GrADS to refer to the GrADS
application that these modules interface to.

This module is based on C<Grads>, an object oriented interface to
GrADS: L<http://opengrads.org/doc/perl/Grads/Grads.html> 
simply instantiates a single C<Grads> object, and
provides several functions that attempt to emulate the look and feel
of the traditional GrADS application. Here is a simple example
comparing the two approaches. Consider a simple script that starts
GrADS, opens a file and displays a variable using C<Gerl>:

   use Grads::Gerl;
   grads { Bin=>"gradsnc", Window=>1 };
   Open "model.nc";
   display "ua;va";
   quit;

Using the OO interface implemented in C<Grads> this example would look
like:

   use Grads;
   $ga = new Grads { Bin=>"gradsnc", Window=>1 };
   $ga->Open "model.nc";
   $ga->cmd "display "ua;va";
   $ga = undef;

These are not that different, but as the TUTORIAL secion will
illustrate, C<Gerl>'s syntax is closer to the traditional GrADS
command line, and provides a nice facility to evaluate a batch of
GrADS commands with a convenient way of catching exceptions.

The wrapper script C<gadl> goes a step further: it customizes the Perl
Data Language (PDL) shell (C<perldl>), automatically loading C<Gerl>
and providing a command line interface to GrADS.  This combination
provides a powerful and convenient environment for advanced
geophysical data analysis and visualization. PDL complements GrADS
quite nicely with a wealth of numerical methods and visualization
tools.  Here is how one would write the example above using
C<gadl>. From your OS command line you start C<gadl>:

   % gadl -nc

Then at the C<< gadl-> >> command line prompt enter

   o model.nc
   d ua;va
   q

No semi-collons, no double quotes, and even additional shortcuts such
as C<o> for "Open". See the COMMAND LINE FILTER FOR PDL section below
for a description of all the shorthands provided by C<Gerl> when
running under C<perldl>.

=head1 REQUIREMENTS

You must have Perl 5 and GrADS installed on your system. This module
requires a version of GrADS supporting inter-process communication
through the "-u" command line argument. This is available with GrADS
Version 1.9.0 and later, not including the 1.9 beta versions. Any of
the OpenGrADS variants also meet this requirement. Functions I<Imp>
and I<Exp>, which allows GrADS to exchange data with the Perl Data
Language (L<http://pdl.perl.org>), obviously requires packages C<PDL> and
C<PDL::IO::FlexRaw>, in addition to the GrADS user defined extension
C<libipc>. No need to install these modules if you do not intent to
use these methods.

=head1 TUTORIAL

=head2 Getting started

Since C<Gerl> is based on the C<Grads> module you are strongly
encouraged to read the documentation for that module first, in
particular the tutorial.

For running this tutorial you will need a sample GrADS dataset. Please
download F<model.nc> from L<http://opengrads.org/sample_data>. If you
are new to GrADS you may want to read the Tutorial on
L<http://grads.iges.org/grads/gadoc/tutorial.html>. This document is
not a GrADS tutorial but rather a tutorial of the Perl interface to
GrADS.

In this example we will use the L<Data::Dumper> module to examine the
contents of hashes returned by some of the methods. So, we start by
using these 2 modules:

   use  Grads::Gerl;
   use  Data::Dumper;

To start the GrADS process we use the C<grads> function:

   $rc = grads { Bin=>"gradsnc", Window=>1 };

For this and subsequent function calls, the return code variable
C<$rc> will be zero if the command completed sucessfully and non-zero
if an error occurred. Usually you would follow the command above with
something like this:

   die "cannot start grads" if ( $rc );

The function I<ga_>() is used to send generic commands to GrADS,
e.g.,

   $rc = $ga "q config";

=head2 Opening files

The I<Open>() function opens a GrADS dataset in any of the supported
formats:

  $fh = Open "wrong_file.nc" or
        warn ">>> Cannot open wrong_file.nc as expected, proceeding.\n";

In this particular case we fed it a bogus file name to force an error
condition. Let's try again, this time with the F<model.nc> file that
you just downloaded:

  $fh = Open "model.nc" or
    die "cannot open model.nc but it is needed for testing, aborting ";

I<Open>() returns a file handle C<$fh> with useful metadata about the
file just read. You can use I<Dumper> for examining the contents of C<$fh>:

  print "\n>>> File opened: " . Dumper($fh) . "\n"; 

A slightly reformatted output follows:

   $fh = {
          'fid'      => '1',
          'bin'      => 'model.nc',
          'desc'     => 'model.nc',
          'title'    => '',
          'type'     => 'Gridded'
          'nvars'    => 8,
          'vars'     => [ 'ps','ts','pr','ua','va','zg','ta','hus' ],
          'var_levs' => [ '0',  '0', '0', '7', '7','7', '7', '7'   ],
          'nx'       => '72',
          'ny'       => '46',
          'nz'       => '7',
          'nt'       => '5',
        };


=head2 Basic syntax

Next we show examples of some simple grads commands. Notice
how the word C<print> is always escaped to avoid conflict with Perl's
C<print> command:

   enable 'print gerl-test.gx';
   set lev, 500;
   set x, 1;
   set gxout, shaded; 

The use of commas in the previous block will come naturally to perl
programmers, but may be confusing for someone coming from the classic
Grads command line with no exposure to Perl. One of Perl's mottos is
I<There is more than one way to do it> (TIMTOWTDI, usually pronounced
I<Tim Toady>), and the first C<set> command above could be written in
a number of equivalent ways:

   set("lev","500');
   set(lev,500);     # this will not work is lev is defined somewhere
                     # a function name
   set("lev 500");
   set lev, 500;
   set "lev 500";
   ga_ "set lev 500";

However, the syntax

   set lev 500

does not work because the arguments of C<set> must be specified as a
list. The C<gadl> front-end to C<perldl> removes this restriction as
it provides a command line filter to special handle GrADS commands.

Like any Perl script, flow control and variable interpolation works as
usual:

   foreach $var ( ps, ta ) {
       foreach $t (1..5) {
           set t, $t;
	   define tz, "ave($var,lon=0,lon=360)";
	   display tz;
           draw title, "Zonal Means: $var at t=$t";
	   printim "gerl-$var-$t.png";
           gaprint;  # print is already taken, so use gaprint instead
           clear;
       }
   }

=head2 Executing native GrADS commands in a I<here> document

A unique feature of C<Gerl> compared to its sibiling C<Grads> is that
one can enter a batch of GrADS commands in the form of a I<here>
document as in this code fragment:

   $rc = ga_ <<"EOF";
      set lev 700
      set lon 0 360
      set t 2
      d ua;va;sqrt(ua*ua+va*va)
      draw title Winds at 700 hPa
      printim gerl-winds.png
   EOF

=head2 Querying the GrADS state

These commands are sent to GrADS, one at time, and in case of a
non-zero error return C<ga_>() stops execution and returns the first
non-zero error code it encounters. This feature is particularly useful
to embed existing GrADS code without the need to convert to Perl
syntax. (A really useful feature would be a function that
evaluates code fragments from the GrADS built in scripting language.)

Like the C<Open>() function, I<Query>() returns a query handle with
information about the particular GrADS property. Here are a few
examples:

   $qh = Query "time";
   print "\n>>> Time handle: ";   print Dumper($qh); 

   $qh = Query "file";
   print "\n>>> Time handle: ";   print Dumper($qh); 

   $dh = Query "dims";
   print "\n>>> Dim handle: ";    print Dumper($dh); 

As of this writing only a handfull of GrADS C<query> properties are
implemented by I<Query>(), but this list is growing with each
release. Be sure to contribute any extension you add. In the meantime,
you can use the I<rword> and I<rline> functions to parse the output of
native C<query> command.  Read on.

=head2 Parsing GrADS output, the traditional way

Traditionally, the built in GrADS scripting language (F<gs>) includes
functions C<sublin> and C<subwrd> to parse the lines and words within
line of each GrADS command issued. To aid the conversion of I<gs>
scripts to Perl, we have included functions I<rword> and I<rline>
which give access to words and lines in the GrADS output stream. The
I<rword>(i,j) function returns the I<j>-th word in the I<i>-th line of
the GrADS command just issued, viz.

  ga_ "q config");
  for $i ( 1...$Grads::Gerl::Ga->{nLines} ) {
      printf("RWORD %3d: ", $i);
      $j=1;  # starts from 1
      while ( $word=rword($i,$j) ) {
	  print $word . " ";
	  $j++;
      }
      print "\n";
      $i++;
  }

To obtain a given output line, use the I<rline> function:

  print "RLINE  *3: " . rline(3) . "\n\n";

=head2 The Set command

The I<Set>() function is used to issue a batch of GrADS C<set> commands,
stored in an array. This is particularly useful to define a graphics
context that can be reused prior to issuing each C<display>
command. Here is an example:

  my @gc;
  $Grads::Gerl::Ga->{Echo} = 1;
  push @gc, "grads off";
  push @gc, "gxout shaded";
  $rc = Set \@gc;

=head2 Interfacing to the Perl Data Language (PDL)

Function I<Exp> allows you to export a GrAS variable into a Perl Data
Language (PDL) object, commonly refered to as I<piddles>:

   $ps = Exp ps;
   print "ps = $ps";

The output piddle C<ps> is sometimes refered to as a I<GrADS field> as
it register a grid,

   $grid = $ps->gethdr()

which contains information about the coordinate variables (longitude,
latitude, vertical level and time), in addition to low level metadata
for exchanging data with GrADS. (This is the same concept of I<field>
introduced by Earth-system Modeling Framework, ESMF). Alternatively,
you can explicitly grab the C<$grid> during export,

   ($ps,$grid) = Exp ps; 

You can also import a piddle with the I<Imp> function, provided one
specifies the necessary C<grid> metadata: 

   $logps = log($ps); 
   Imp logps, $logps, $grid; 
   display logps;

Of course, there is more than one way of doing this. Having a C<$grid>
you can use the piddle's C<sethdr()> function to regsiter it:

   $logps->sethdr($grid);

and then there is no need to explicitly pass C<$grid> to function
C<Imp>: 

   $Imp logps, $logps;
   Display logps;

Given the appropriate metadata, one can also display a piddle in
GrADS. If all one wants to do is to display a variable there is no
need to import it first as we did above. In the previous example, you
could display C<$logps> directly:

   Display $logps;

The current implementation requires that both C<x> and C<y> dimensions
be varying. In addition, both C<z> and C<t> dimensions can be varying,
individually or at the same time.  Just like GrADS itself, C<Display>
cannot handle both C<z> and C<t> varying at the same time. Here is an
example exporting a 4D variable:

  ga_ "set t 1 5";
  ga_ "set z 1 7";
  $ua = Exp ua;
  print $ua->dims;

=head2 Terminating your GrADS session

To terminate your GrADS session just enter:

  quit;

This will cause a C<quit> to be sent to GrADS which in turn will end
the connection. 

=head1 MAIN FUNCTIONS

Unless otherwise stated all functions returning the error code $I<rc>
behave much in the same way as a shell command. A zero $I<rc> value
means that the function completed sucessfully, while a non-zero value
indicates an error condition.  When a function states that it returns a
reference or a list, failure will be returned as I<undef> or an empty
list. 

=head2 $I<rc> = B<grads> ( [I<OPTIONS>] )

=over 4

This function starts the GrADS application with a bi-directional pipe
so that it can send commands to the GrADS C<stdin> and read the output
from the GrADS C<stdout>. The I<OPTIONS> are passed in a hash like
fashion, using key and value pairs. Possible options are:

=over 8

=item I<Bin> 

The name of the GrADS binary to execute; the default is
I<gradshdf>. Enter the full path name if you need to. 

=item I<Verb> 

Whether or not to print the return code for each GrADS
command issued. Set I<Verb=>1 to only print commands with non-zero
return codes. Set I<Verb=>2 to print the return for all commands. The
default is really quiet: it does not print any return code. Along with
I<Echo>, this feature is very useful when debugging your scripts.

=item I<Echo>

Whether or not to echo the GrADS C<stdout>.

=item I<Window>

Set to 1 to enable the GrADS graphics window; the default
is to run GrADS in batch mode.

=item I<Port> 

Set to 1 to start GrADS in portrait mode; the default is to
start GrADS in landscape mode.

=item I<Opts> 

Other options you would like to pass to the GrADS
application on the command line, e.g., C<<Opts=>'-g 800x600'>>.

=back

=back

=head2 $I<rc> = B<ga_> ( I<COMMAND> )

=over 4

Executes a generic GrADS command as one would enter on the command
line. Use the I<rword> and I<rline> functions to parse the C<stdout>
generated by this command. Example:

   $rc = $ga_ "enable print example.gm";
   die "cannot enable print" if ( $rc );

You can also add a batch of commands in a I<here> document:

   $rc = ga_ <<EOF
      set lev 700
      set lon 0 360
      set t 2
      d ua;va;sqrt(ua*ua+va*va)
      draw title Winds at 700 hPa
      printim gerl-winds.png
   EOF

Each command will be executed one at a time, and the return code of
each examined. In case of a non-zero error condition the ofending
C<$rc> is returned and subsequent commands are not executed.

=back


=head2 $I<rc> = B<define> ( I<VARNAME>, I<EXPR> )

=over 4

This function issues a GrADS define command. These two commands are
equivalent: 

   define speed, 'sqrt(u*v+v*v)';
   ga_   'speed = sqrt(u*v+v*v)';

where C<speed> is the variable name to be defined, and
C<sqrt(u*v+v*v)> is the expression defining it.

=back 



=head2 $I<rc> = B<Display> ( I<GA_EXPR> | $I<PERL_EXPR>[, I<GRID> )

=over 4

This function displays either a regular GrADS expression or a piddle
expression. 

For GrADS expressions, it detects such cases with varying time
dimension and adds an additional carriage return in response to the
anticipated user input at the end of an animation sequence.

For piddle expressions, one can specify additional metadata; see
C<Exp> below for more more information on this metadata. In fact,
C<Display> of piddle expressions is simply syntatic sugar for
C<< Imp(<display>,EXPR,GRID). >> These 2 statements are equivalent:

   Display $ts;
   Imp '<display>', $ts;

=back

=head2 $I<x> = B<Exp> ( I<EXPR> )

=head2 ($I<x>,$I<grid>) = B<Exp> ( I<EXPR> )

=over 4

This is the I<Export> function which evaluates the GrADS expression
I<EXPR> and return an array of floating numbers $I<x> as a Perl Data
Language (L<http://pdl.perl.org>) I<piddle> object. This function requires the
C<PDL::IO::FlexRaw> package. In list context, the optional metadata
$I<grid> is also returned. You will need this metadata to import back
into GrADS derived piddles which does not have this information stored
inside. In scalar context one can retrieve C<$grid> with the PDL 
C<gethdr> function:

   $grid = $x->gethdr()

C<$grid> contains information about the coordinate variables, as well
as relevant metadada needed to exchange data with GrADS. Consult the
documentation for the module C<Grads.pm> 
at L<http://opengrads.org/doc/perl/Grads/Grads.html> 
for addtional information on
C<$grid>.

The current implementation requires that both <x> and <y> dimensions
be varying. In addition both <z> and <t> can vary as well, resulting
in piddles that are 2D, 3D or 4D.  For consistency with current PDL
practice, the dimensions of the 3D/4D arrays are kind of strange:
C<(nt,nz,nx,ny)> instead of C<(nt,nz,ny,nx)>. This is done so that
each of the horizontal (nx,ny) slices can be easily displayed in PDL
with functions such as C<imag>.

Consult the documentation for the method C<Exp> in module C<Grads.pm>
at L<http://opengrads.org/doc/perl/Grads/> for addtional information on this function.


=back

=head2 $I<rc> = B<Imp> ( I<GA_VAR>, I<PERL_VAR>, [I<GRID>] ) 

=over 4

This is the I<Import> function which takes an array of floating numbers
I<PERL_VAR>, implemented as a Perl Data Language (L<http://pdl.perl.org>) I<piddle>
object, sends it to GrADS, defining it as a GrADS variable I<GA_VAR>.
This function requires the C<PDL::IO::FlexRaw> package.

In the current implementation, the input piddle is required to have
registered a specific I<header> with the piddle function C<sethdr>, or
you have to provide the same information through the optional
parameter I<GRID>. See function C<Exp> above of a descriptions of the
contents of I<GRID>. This information is returned when a variable is
exported from GrADS with function I<Exp>, and this metadata can be
reused during an import operation. See B<Exp>.

=back

=head2 $I<rc> = B<gaprint> ( [I<FILENAME>] )

=over 4

Issues a GrADS C<print> command. To avoid conflict with Perl's print
command we have renamed this function C<gaprint>.

=back

=head2 $I<fh> = B<Open> ( I<FILENAME> [, I<FILETYPE>] )

=over 4

Opens a GrADS dataset in one of the supported formats, issuing a GrADS
C<open>, C<sdfopen> or C<xdfopen> command depending on the file
type. By default, an heuristic methodxs based on the file name is used
to determine the file type. Files with extension C<hdf> and C<nc>, or
starting with C<http://> are assumed to be opened with C<sdfopen>;
files with extension C<ddf> are assumed to be opened with C<xdfopen>;
all else is opened with the GrADS command C<open>. The file name
extension matching is I<case insensitive>. The optional parameter
I<FILETYPE>, which can take the values C<sdf>, C<xdf> or C<ctl>, can
be used for those cases when the heuristic method fails.

This function returns $I<fh>, a reference to hash containing the file
metadata. This is the same information as returned by
I<Query>("file"); see below for description of this hash.

=back

=head2 $I<qh> = B<Query> ( I<PROPERTY> )

=over 4

This function issues the GrADS command C<< query I<PROPERTY> >>, and
returns the output of this command in a reference to a hash. Consult
the documentation for the module C<Grads.pm> 
at L<http://opengrads.org/doc/perl/Grads/Grads.html> 
for details of the
properties that are currently implemented.

=head2 $I<rc> = B<Set> ( I<PROPERTIES> ) 

=over 4

This functions takes a series of arguments for the GrADS C<set> command
and execute them in sequence. On input, I<PROPERTIES> is an array
reference containing the properties to be set. Example:

  push @gc, "grads off";
  push @gc, "gxout shaded";
  $rc = $ga->Set(\@gc);

=back

=head2 $I<line> = B<rline>(I<i>)

=over 4

This function returns the I<i>-th line of the C<stdout> generated by the
last GrADS command. The number of lines available can be retrieved
from the object with key C<nLines>, e.g., C<$ga->{nLines}>.

=back

=head2 $I<word> = B<rword>(I<i>,I<j>)

=over 4

This function returns the I<j>-th word within the I<i>-th line of the
C<stdout> generated by the last GrADS command. The number of lines
available can be retrieved from the object with key C<nLines>, e.g.,
C<< $ga->{nLines} >>.

=back

=head1 AUTOLOADED FUNCTIONS

Perl Autoload facility allows one to define proxy functions when one
is not explicitly provided; you can find more information in here
L<http://perl.active-venture.com/pod/perltoot-autoload.html>. This is
very useful to make intrinsic GrADS commands appear as Perl
functions. The following GrADS functions are defined by this device.

=head2 Startup functions:

These functions are all variants of the function B<grads>, the only
difference is that the default GrADS executable is the same as the
name of the function.

=head3 $I<rc> = B<gradsc> ( [I<OPTIONS>] )

=head3 $I<rc> = B<gradsnc> ( [I<OPTIONS>] )

=head3 $I<rc> = B<gradsnc4> ( [I<OPTIONS>] )

=head3 $I<rc> = B<gradshdf> ( [I<OPTIONS>] )

=head3 $I<rc> = B<gradsdap> ( [I<OPTIONS>] )

=head3 $I<rc> = B<gradsdods> ( [I<OPTIONS>] )

=head2 GrADS commands:

The following GrADS commands are exported by C<Gerl>. Consult the
GrADS documentation at L<http://grads.iges.org/grads/gadoc/index.html>
for a detailed description of what these commands do.

=head3 $I<rc> = B<c> 

=over 4

Same as B<clear>.

=back

=head3 $I<rc> = B<clear>

=over 4

Clears the screen.

=back

=head3 $I<rc> = B<d> ( [I<EXPR>] )

=over 4

Same as B<display>.

=back

=head3 $I<rc> = B<display> ( [I<EXPR>] )

=over 4

Display a GrADS expression.

=back

=head3 $I<rc> = B<disable> ( [I<ARGUMENTS>] )

=over 4

Disables GrADS features such as metafile printing.

=back

=head3 $I<rc> = B<draw> ( [I<ARGUMENTS>] )

=over 4

Draw titles, buttons and other graphic objects.

=back

=head3 $I<rc> = B<enable> ( [I<ARGUMENTS>] )

=over 4

Enables GrADS features such as metafile printing.

=back

=head3 $I<rc> = B<printim> ( [I<ARGUMENTS>] )

=over 4

Prints images.

=back

=head3 $I<rc> = B<run> ( [I<ARGUMENTS>] )

=over 4

Runs a native GrADS script.

=back

=head3 $I<rc> = B<set> ( [I<ARGUMENTS>] )

=over 4

Sets all kinds of GrADS parameters.

=back

=head3 $I<rc> = B<sdfopen> ( [I<ARGUMENTS>] )

=over 4

Opens a self-describing file (SDF); these are usually NetCDF or HDF files.

=back

=head3 $I<rc> = B<query> ( [I<ARGUMENTS>] )

=over 4

Queries the GrADS internal state.

=back

=head3 $I<rc> = B<q> ( [I<ARGUMENTS>] )

=over 4

Same as B<query>.

=back

=head3 $I<rc> = B<quit>

=over 4

Terminates GrADS, closing files, graphical window, etc.

=back

=head3 $I<rc> = B<xdfopen> ( [I<ARGUMENTS>] )

=over 4

Alternative version of B<sdfopen> where a data descriptor file is used
supplement or overwrite metadata in a self-describing file.

=back

=head1 COMMAND LINE FILTER FOR PDL SHELL

The program B<perldl> is a simple shell (written in Perl) for
interactive use of the Perl Data Language (PDL). Perl/PDL commands can
simply be typed in, and it can support command line editing with a
history mechanism. The B<perldl> shell also allows user defined
command line filters where user input can be pre-processed and
properly I<escaped> as needed. We explore this feature to give GrADS
users familiar with the classic C<< ga-> >> command line a familiar
look-and-feel when using B<perldl>.

When used under B<perldl> this module automatically defines a command
line filter that enables the following shortcuts:

=over 4

=item

B<o> - replaced with B<Open>

=item

B<pi> - replaced with B<printim>

=item

B<pr> - replaced with B<gaprint>

=item

B<r> - replaced with B<run>

=item

B<s> - replaced with B<ga_ set>

=item

B<.> - replaced with B<ga_> 

=back

In addition, it detects GrADS commands and properly escape then so
that you can type

   gadl> s lev 500

without the need for commas and double quotes. The wrapper script
B<gadl> starts B<perldl>, automatically loading C<Gerl> and this
command line filter.

=head2 Example:

   gadl-> o model
   gadl-> s gxout shaded
   gadl-> d ua;va
   gadl-> draw title Hello, World!
   gadl-> printim

Notice that the default prompt B<< perldl> >> has been changed to
B<< gadl-> >>.

=head1 TO DO

=over 4

=item 

Complete implementation of the I<Query> function.

=item

Implement in Perl a native GrADS script (I<gs>) interpreter, or
perhaps a just a translator.

=back

=head1 BUGS

=over 4

=item 

On some systems it hangs when the name of an invalid GrADS
executable is provided.

=item 

Under PerlDL, any attempt to read from stdin from within GrADS
will fail, including the I<pull> function in C<gs> scripts and 
the request for I<CR> after an animation sequence is displayed.

=back

=head1 SEE ALSO

=over 4

=item 

L<http://opengrads.org/doc/perl/Grads/Grads.html> - an object oriented interface to GrADS

=item 

L<http://opengrads.org/doc/perl/gadl/gadl.html> - gadl, a wrapper script to start the PDL shell loading Grads::Gerl.

=item 

L<http://pdl.perl.org> - the Perl Data Language (PDL) official website.

=item 

L<http://grads.iges.org/grads> - the official GrADS website.

=item 

L<http://opengrads.org> - the OpenGrADS website hosting a number
of GrADS extensions.

=item 

L<http://opengrads.org/test_data> - sample datasets for running
tutorials and testing GrADS.

=back

=head1 AUTHOR

Arlindo da Silva <dasilva@opengrads.org>

=head1 COPYRIGHT

Copyright (c) 2006-2007 Arlindo da Silva. All rights reserved.
This program is free software; you can redistribute it and/or modify it 
under the same terms as Perl itself.

=cut




