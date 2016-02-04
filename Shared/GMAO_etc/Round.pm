package Round;

use strict;
use POSIX;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

require Exporter;

@ISA = qw(Exporter AutoLoader);
@EXPORT = qw(round nearest);
@EXPORT_OK = qw(round nearest round_even round_odd round_rand
   nearest_ceil nearest_floor nearest_rand
   nlowmult nhimult );
$VERSION = '0.05';

%EXPORT_TAGS = ( all => [ @EXPORT_OK ] );

#--- Determine what value to use for "one-half".  Because of the
#--- perversities of floating-point hardware, we must use a value
#--- slightly larger than 1/2.  We accomplish this by determining
#--- the bit value of 0.5 and increasing it by a small amount in a
#--- lower-order byte.  Since the lowest-order bits are still zero,
#--- the number is mathematically exact.

my $halfhex = unpack('H*', pack('d', 0.5));
if (substr($halfhex,0,2) ne '00' && substr($halfhex, -2) eq '00') {
   #--- It's big-endian.
   substr($halfhex, -4) = '1000';
} else {
   #--- It's little-endian.
   substr($halfhex, 0,4) = '0010';
}

my $half = unpack('d',pack('H*', $halfhex));

sub round {
 my $x;
 my @res = ();
 foreach $x (@_) {
   if ($x >= 0) {
      push @res, POSIX::floor($x + $half);
   } else {
      push @res, POSIX::ceil($x - $half);
   }
 }
 return (wantarray) ? @res : $res[0];
}

sub round_even {
 my $x;
 my @res = ();
 foreach $x (@_) {
   my ($sign, $in, $fr) = _sepnum($x);
   if ($fr == 0.5) {
      push @res, $sign * (($in % 2 == 0) ? $in : $in + 1);
   } else {
      push @res, $sign * POSIX::floor(abs($x) + $half);
   }
 }
 return (wantarray) ? @res : $res[0];
}

sub round_odd {
 my $x;
 my @res = ();
 foreach $x (@_) {
   my ($sign, $in, $fr) = _sepnum($x);
   if ($fr == 0.5) {
      push @res, $sign * (($in % 2 == 1) ? $in : $in + 1);
   } else {
      push @res, $sign * POSIX::floor(abs($x) + $half);
   }
 }
 return (wantarray) ? @res : $res[0];
}

sub round_rand {
 my $x;
 my @res = ();
 foreach $x (@_) {
   my ($sign, $in, $fr) = _sepnum($x);
   if ($fr == 0.5) {
      push @res, $sign * ((rand(4096) < 2048) ? $in : $in + 1);
   } else {
      push @res, $sign * POSIX::floor(abs($x) + $half);
   }
 }
 return (wantarray) ? @res : $res[0];
}

#--- Separate a number into sign, integer, and fractional parts.
#--- Return as a list.
sub _sepnum {
 my $x = shift;
 my ($sign, $i);
 $sign = ($x >= 0) ? 1 : -1;
 $x = abs($x);
 $i = int($x);
 return ($sign, $i, $x - $i);
}

#------ "Nearest" routines (round to a multiple of any number)

sub nearest {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
   if ($x >= 0) {
      push @res, $targ * int(($x + $half * $targ) / $targ);
   } else {
      push @res, $targ * POSIX::ceil(($x - $half * $targ) / $targ);
   }
 }
 return (wantarray) ? @res : $res[0];
}

# In the next two functions, the code for positive and negative numbers
# turns out to be the same.  For negative numbers, the technique is not
# exactly obvious; instead of floor(x+0.5), we are in effect taking
# ceiling(x-0.5).

sub nearest_ceil {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
    push @res, $targ * POSIX::floor(($x + $half * $targ) / $targ);
 }
 return (wantarray) ? @res : $res[0];
}

sub nearest_floor {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
    push @res, $targ * POSIX::ceil(($x - $half * $targ) / $targ);
 }
 return (wantarray) ? @res : $res[0];
}

sub nearest_rand {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
   my ($sign, $in, $fr) = _sepnear($x, $targ);
   if ($fr == 0.5 * $targ) {
      push @res, $sign * $targ * ((rand(4096) < 2048) ? $in : $in + 1);
   } else {
      push @res, $sign * $targ * int((abs($x) + $half * $targ) / $targ);
   }
 }
 return (wantarray) ? @res : $res[0];
}

#--- Next lower multiple
sub nlowmult {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
    push @res, $targ * POSIX::floor($x / $targ);
 }
 return (wantarray) ? @res : $res[0];
}

#--- Next higher multiple
sub nhimult {
 my ($targ, @inputs) = @_;
 my @res = ();
 my $x;

 $targ = abs($targ) if $targ < 0;
 foreach $x (@inputs) {
    push @res, $targ * POSIX::ceil($x / $targ);
 }
 return (wantarray) ? @res : $res[0];
}

#--- Separate a number into sign, "integer", and "fractional" parts
#--- for the 'nearest' calculation.  Return as a list.
sub _sepnear {
 my ($x, $targ) = @_;
 my ($sign, $i);
 $sign = ($x >= 0) ? 1 : -1;
 $x = abs($x);
 $i = int($x / $targ);
 return ($sign, $i, $x - $i*$targ);
}

1;

