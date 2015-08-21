package Grads;
#
#  This Class implements a Perl interface to GrADS by means of bi-directional 
#  pipes. It requires a small patch to GrADS to unbuffer stdin/stdout and 
#  to allow capture of error codes.
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
#  REVISION HISTORY:
#
#  03Mar2006  da Silva  First crack.
#  04Jul2007  da Silva  Further development: added open(), query(),
#                       fixed "j" index for rword.
#  06Sep2007  da Silva  Documentation, prototype PDL interface
#  25May2009  esok      Added long variable names as "var_names"
#  02Sep2009  esok      Fixed variable qualifiers, added "use strict" and
#                       "use warnings"
#  22Feb2010  dnadeau   Change from IPC::Open2 to IPC::Run
#                       Change \.NC$ to \.NC.*$ in order to handle .nc4 files.
#
#--------------------------------------------------------------------------

use strict;
use warnings;
require 5.000;
require Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(new);

#.........................................................................

BEGIN {

 use FileHandle;
# use IPC::Open2;
 use IPC::Run qw( start pump finish  );

 use Carp;
 use Data::Dumper;

 my $VERSION = "1.00"; 

 my %ga = (

    Pid    => undef,  # the GrADS pid
    Echo   => undef,  # Whether or not we echo the GrADS stdout
    Verb   => undef,  # Whether or not to print rc for each GrADS cmd
                      #  Verb=1 only prints rc=1 cmds; Verb=2 all cmds;
                      #  default is really quiet.

    Lines  => undef,  # the junk GrADS writes out in response to a cmd
    Words  => undef,  # the junk GrADS writes out in response to a cmd
    nLines => 0,      # number of lines in Words

    Reader => undef,  # pipe: GrADS stdout
    Writer => undef,  # pipe: GrADS stdin

    expTransferfile => undef, # Transfer file from GrADS to PDL
    impTransferfile => undef, # Transfer file from PDL to GrADS

 );

 my $MMM2MM = { JAN=>'01', FEB=>'02', MAR=>'03', APR=>'04', MAY=>'05', JUN=>'06',
             JUL=>'07', AUG=>'08', SEP=>'09', OCT=>'10', NOV=>'11', DEC=>'12'};


 eval 'use PDL';
 eval 'use PDL::NiceSlice';
 eval 'use PDL::IO::FlexRaw';
 my ( $PDL, $EXPTRANSFERFILE, $IMPTRANSFERFILE );
 if ( $@ eq "" ) {
      $PDL = 1;
       $EXPTRANSFERFILE = "-";            # default transfer file
       $IMPTRANSFERFILE = "-";            # default transfer file
 } else {
      $EXPTRANSFERFILE = "ga_save.dat~"; # default transfer file
      $IMPTRANSFERFILE = "ga_save.dat~"; # default transfer file
      $PDL = 0;
      carp "Grads: PDL features not enabled; this is OK but I will not be able to exchange data with PDL  ... "; 
 }


#.........................................................................
sub new {                                                   # Constructor

    my $that = shift;
    my $class = ref($that) || $that;

    my ( $argv ) = @_;

    my ($Bin, $Echo, $Verb, $Port, $Batch, $Opts, $Land, $Window, 
        $TransferFile, $expTransferFile, $impTransferFile);

    local *Reader;
    local *Writer;

#   Complement argv with env variables
#   ----------------------------------
    for my $att qw ( Bin Echo Verb Port Window Opts 
                  TransferFile expTransferFile impTransferFile ) {
        my $evar = "GA_" . uc $att;
 #      print qq(env var = $evar = <$ENV{$evar}>\n);
        $argv->{$att} = "$ENV{$evar}" 
            if ( defined($ENV{$evar}) && 
                 ("$argv->{$att}" eq "" && "$ENV{$evar}" ne "" ));
    }

#   Parse user specified options
#   ----------------------------
    $Bin   = "gradshdf" unless ( $Bin    = $argv->{Bin}   );
    $Echo  = 0          unless ( $Echo   = $argv->{Echo}  );
    $Verb  = 0          unless ( $Verb   = $argv->{Verb}  );
    $Port  = 0          unless ( $Port   = $argv->{Port}  );
    $Window = 0         unless ( $Window = $argv->{Window} );
    $Opts  = ""         unless ( $Opts   = $argv->{Opts}  );

    if ( defined($argv->{TransferFile}) &&
         "$argv->{TransferFile}" ne "" ) {
         $EXPTRANSFERFILE = "$argv->{TransferFile}";
         $IMPTRANSFERFILE = "$argv->{TransferFile}";
    }

    $expTransferFile = "$EXPTRANSFERFILE" 
                        unless ( defined($argv->{expTransferFile}) &&
                                 ($expTransferFile 
                                         = "$argv->{expTransferFile}") );
    $impTransferFile = "$IMPTRANSFERFILE" 
                        unless ( defined($argv->{impTransferFile}) &&
                                 ($impTransferFile 
                                         = "$argv->{impTransferFile}") );

    $Batch = 1-$Window; 
    $Land  = 1-$Port;

#   Build GrADS startup command line
#   --------------------------------
    my $opts = "-u";                      # force unbuffered mode (essential) 
    $opts = "$opts -p"        if ( $Port  );
    $opts = "$opts -l"        if ( $Land  );
    $opts = "$opts -b"        if ( $Batch );
    $opts = "$opts $Opts"     if ( $Opts  );
    my $cmd  = "$Bin $opts";  
    my @cmd = split ' ', $opts;
    unshift @cmd,"$Bin";

#   Spawn GrADS process with bi-directional pipes
#   ---------------------------------------------

    my $READER;
    my $WRITER;
    my $ERR;

    my $h = start 
	\@cmd,
	\$WRITER,
	\$READER, 
	\$ERR,
	or die "cat returned $?";


    my $Pid = ${$h->{'KIDS'}}[0]->{'PID'};

#   Record state
#   ------------
    $ga{Pid}    = $Pid;  # the process ID
    $ga{Verb}   = $Verb; # prints rc for each GrADS command issued
    $ga{Echo}   = $Echo; # echo's GrADS output
    $ga{Reader} = \$READER;
    $ga{Writer} = \$WRITER;
    $ga{Handle} = $h;
    $ga{expTransferFile} = $expTransferFile;
    $ga{impTransferFile} = $impTransferFile;

#   Bless it
#   --------
    my $self = { %ga, };
    bless $self, $class;

    my $rc = $self->_parseReader();

    if ( $rc ) {
    carp "could not parse initial reader ";
    return undef;
    } else {
    print "Started <$cmd> with pid = $Pid, rc = $rc\n" if ( $Verb );
    return $self;
    }

}

#..................................................................
sub DESTROY {                                                   # Destructor

   my $self = shift;

   my $Verb = $self->{Verb};
   my $Pid  = $self->{Pid};

#   $self->cmd("quit");

   print "Destroyed GrADS with pid = $Pid\n" if ( $Verb );

   $self->{Pid}    = undef;
   $self->{Echo}   = undef; 
   $self->{Verb}   = undef; 
   $self->{Lines}  = undef; 
   $self->{nLines} = 0;
   $self->{Words}  = undef; 
   $self->{Reader} = undef;  
   $self->{Writer} = undef; 

   
}

#..................................................................
sub cmd {

    my $self = shift;
    my ( $cmd ) = @_;

    my $Verb   = $self->{Verb};
    my $Echo   = $self->{Echo};
    my $Reader = $self->{Reader};
    my $Writer = $self->{Writer};

    my $h      = $self->{Handle};


#   Clear Output
#   --------------
    $$Reader = '';

#   Send the command
#   ----------------
    $$Writer = $cmd."\n";
    pump $h  while length $$Writer; 

#   Parse the output
#   ---------------
    my $rc = $self->_parseReader();

    if ( $rc > 0 ) {
         print "rc = $rc for <$cmd> \n" if ( $Verb );
     } else {
         print "rc = $rc for <$cmd> \n" if ( "$Verb" > 1 );
     }

    return $rc;

}

#..................................................................
sub Open {                                    # generic file opener

    my $self = shift;
    my $fname = shift;
    my $ftype = shift;

    $ftype = "DEFAULT" unless ( $ftype );

#   Determine the file opener
#   -------------------------
    my $FTYPE = uc $ftype; 
    my $opener = "open";
    if ( $FTYPE eq "DEFAULT" ) {
        $_ = uc $fname;
        $opener = "sdfopen" if ( ?^HTTP\://? || /\.HDF$/ || /\.NC.*$/ );
        $opener = "xdfopen" if ( /\.DDF$/ );
    }
    elsif ( "$FTYPE" eq "SDF" ) {
        $opener = "sdfopen";
    }
    elsif ( "$FTYPE" eq "XDF" ) {
        $opener = "xdfopen";
    }

#  Open the file
#  -------------
   my $rc = $self->cmd("$opener $fname");
   return undef if ( $rc );

#  Return file handle
#  ------------------
   my $fh = $self->Query("file");
   return $fh;

}

#..................................................................
sub Query {    # runs query command and collect output in convenient hash

    my $self = shift;
    my ( $what ) = @_;

    my %qh; my $rc; my $i; my @token;

#   Execute query
#   -------------
    $rc = $self->cmd("query $what");
    return undef if ( $rc );

#   Parse results
#   -------------
    if ( lc "$what" =~ /^file\b/  ) {                    # query file
        my $title;
        my @title; 
        @title = @{$self->{Words}[1]};
        $title = "@title[3..@title-1]";
        $title =~ s/\"//g;
        $qh{fid} = $self->rword(1,2);
        $qh{title} = "$title";
        $qh{desc} = $self->rword(2,2);
        $qh{bin} = $self->rword(3,2);
        $qh{type} = $self->rword(4,3);
        $qh{nx} = $self->rword(5,3);
        $qh{ny} = $self->rword(5,6);
        $qh{nz} = $self->rword(5,9);
        $qh{nt} = $self->rword(5,12);
        $qh{nvars} = $self->rword(6,5);
        my @vars;
        my @levs;
        my @var_names;
        for $i (7..6+$qh{nvars}) {
            push @vars, $self->rword($i,1); 
            push @levs, $self->rword($i,2);
            my $var_name = $self->rword($i,4);
            my $j = 5;
            while (my $word = $self->rword($i,$j)) { 
              $var_name .= " " . $word;
              $j++;
            }
            push @var_names, $var_name;
        } 
        $qh{vars}     = \@vars;
        $qh{var_levs} = \@levs;
        $qh{var_names}= \@var_names;
    }

    if ( lc "$what" =~ /^ctlinfo\b/  ) {                    # query ctlinfo
	my $nbWords =  $#{$self->{Words}};
	my $value = '';
	my $savedkey = '';
	for( my $i = 1; $i < $nbWords; $i++ ) {
	    my $key = shift(@{$self->{Words}[$i]});

#           Concatenate value line to last key.
#           ------------------------------------
	    if( $key !~ /[a-zA-Z]/) {
		$value = $value.' '.$key.' '.join(" ", @{$self->{Words}[$i]});
		$qh{$savedkey} = $value;
		next;
	    }

	    if( $key =~ 'endvars' ) {next};

	    $value = join(" ", @{$self->{Words}[$i]});
	    $qh{$key} = $value;
	    $savedkey=$key;
	 }
    }

    elsif (  lc "$what" eq "dims" ) {                    # query dims
	$qh{dfile} = $self->rword(1,5);
	$qh{x_state} = $self->rword(2,3);
	if ( $qh{x_state} eq 'fixed' ) {
	     $qh{lon} = [ $self->rword(2,6), $self->rword(2,6)];
	     $qh{x}   = [ $self->rword(2,9), $self->rword(2,9)];
	}
	else {
	     $qh{lon} = [ $self->rword(2,6), $self->rword(2,8)];
	     $qh{x}   = [ $self->rword(2,11), $self->rword(2,13)];
	}
             $qh{nx} = $qh{x}[1] - $qh{x}[0] + 1; 
    $qh{y_state} = $self->rword(3,3);
    if ( $qh{y_state} eq 'fixed' ) {
         $qh{lat} = [ $self->rword(3,6), $self->rword(3,6)];
         $qh{y}   = [ $self->rword(3,9), $self->rword(3,9)];
        }   
    else {
             $qh{lat} = [ $self->rword(3,6), $self->rword(3,8)];
         $qh{y}   = [ $self->rword(3,11), $self->rword(3,13)];
    }
             $qh{ny} = $qh{y}[1] - $qh{y}[0] + 1; 
    $qh{z_state} = $self->rword(4,3);
    if ( $qh{z_state} eq 'fixed' ) {
        $qh{lev} = [ $self->rword(4,6), $self->rword(4,6)];
        $qh{z}   = [ $self->rword(4,9), $self->rword(4,9)];
        }
    else {
        $qh{lev} = [ $self->rword(4,6), $self->rword(4,8)];
        $qh{z}   = [ $self->rword(4,11), $self->rword(4,13)];
    }
            $qh{nz} = $qh{z}[1] - $qh{z}[0] + 1; 
    $qh{t_state} = $self->rword(5,3);
    if ( $qh{t_state} eq 'fixed' ) {
        $qh{time} = [ $self->rword(5,6), $self->rword(5,6)];
        $qh{t}    = [ $self->rword(5,9), $self->rword(5,9)];
    }
    else {
        $qh{time} = [ $self->rword(5,6),  $self->rword(5,8)];
        $qh{t}    = [ $self->rword(5,11), $self->rword(5,13)];
    }
            $qh{nt} = $qh{t}[1] - $qh{t}[0] + 1; 
    }

    elsif ( lc "$what" eq "time" ) {                    # query time
    my @time; my @day; my @token; my @dd; 
        my @mmm; my @mm; my @yyyy;
        my @hh; my @nn;
    $time[0] = $self->rword(1,3);
    $time[1] = $self->rword(1,5);
    $day[0] = $self->rword(1,6);
    $day[1] = $self->rword(1,8);
    for $i ( 0..1 ) {
        @token = split "Z", $time[$i];
        $dd[$i]  = substr($token[1],0,2);       
        $mmm[$i] = substr($token[1],2,3);       
        $mm[$i] = $MMM2MM->{$mmm[$i]};
        $yyyy[$i] = substr($token[1],5,4);       
        @token = split ":", $token[0];
            $hh[$i] = $token[0];
            $nn[$i] = $token[1];
    }
        $qh{time} = \@time;
        $qh{dd}   = \@dd;
        $qh{day}  = \@day;
        $qh{hh}   = \@hh;
        $qh{nn}   = \@nn;
        $qh{mmm}  = \@mmm;
        $qh{mm}   = \@mm;
        $qh{yyyy} = \@yyyy;
    }

    elsif ( lc "$what" =~ /^gr2w/ ) { # Converts gr screen to world coordinates
        $qh{Lon} = $self->rword(1,3);
        $qh{Lat} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^xy2w/ ) { # Converts XY screen to world coordinates
        $qh{Lon} = $self->rword(1,3);
        $qh{Lat} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^pp2xy/ ) { # Converts virtual page XY 
                                      # to real page XY coordinates
        $qh{X} = $self->rword(1,3);
        $qh{Y} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^gr2xy/ ) { # Converts grid to XY screen coordinates
        $qh{X} = $self->rword(1,3);
        $qh{Y} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^w2xy/ ) { # Converts world to XY screen coordinates
        $qh{X} = $self->rword(1,3);
        $qh{Y} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^xy2gr/ ) { # Converts XY screen to grid coordinates
        $qh{I} = $self->rword(1,3);
        $qh{J} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^w2gr/ ) { # Converts world to grid coordinates
        $qh{I} = $self->rword(1,3);
        $qh{J} = $self->rword(1,6);
    }

    elsif ( lc "$what" =~ /^attr/ ) { # query attributes (for now, only units)
      my $nLines = $self->{nLines};
      for $i (1..$nLines) {
	my $thirdWord = $self->rword($i,3); 
	if ( lc "$thirdWord" =~ "units" ) {
	  my $var = $self->rword($i,1);
	  my $unit = $self->rword($i,4);
	  $qh{$var} = $unit;
	}
      }
    }

    else {

    $qh{placeholder} = "query handle not implemented yet";

    }

    my $qh = \%qh;
    return $qh;
}

#.....................................................................

sub Values {  # Get explicit information about dimensions

    my $self = shift;
    my %qh;
	my %tdef;
	my %xdef;
	my %ydef;
    my @zdef;

    my $rc = $self->cmd("query ctlinfo");
    return undef if ( $rc );

    foreach my $i (0 .. $self->{nLines}) {
        # print "Line: $i  word:  " . $self->rword($i, 1) . "\n";
        my $keyword = $self->rword($i, 1);
        if ($keyword eq "zdef") {
            # print "Found zdef\n";
            if ($self->rword($i, 3) eq "levels") {
                my $j = 4;
                my $count = $self->rword($i, 2);
                # print "Found $count levels\n";
                while (scalar(@zdef) < $count) {
                    my $word;
                    while (not $word = $self->rword($i, $j) and $word ne "0") {
                        # print "a $i $j $word\n";
                        ++$i;
                        $j = 1;
                        $word = $self->rword($i, $j);
                    }
                    # print "b $i $j $word\n";
                    # print "  " . $self->rword($i, $j) . "\n";
                    push @zdef, ($word);
                    ++$j;
                }
            }
    		$qh{'zdef'} = join ' ', @zdef;
        }
        elsif ($keyword eq "tdef") {
            # print "Found $keyword\n";
            if ($self->rword($i, 3) eq "linear") {
				$tdef{'ntimes'} = $self->rword($i, 2);
				$tdef{'enum'} = $self->rword($i, 3);
				$tdef{'time0'} = $self->rword($i, 4);
				$tdef{'period'} = $self->rword($i, 5);
				$qh{$keyword} = \%tdef;
			}
			else
			{
				print STDERR "Grads::Values: Unknown $keyword '$self->rword($i, 3)'\n";
			}
        }
        elsif ($keyword eq "xdef") {
            # print "Found $keyword\n";
            if ($self->rword($i, 3) eq "linear") {
				$xdef{'nx'} = $self->rword($i, 2);
				$xdef{'enum'} = $self->rword($i, 3);
				$xdef{'x0'} = $self->rword($i, 4);
				$xdef{'xstep'} = $self->rword($i, 5);
				$qh{$keyword} = \%xdef;
			}
			else
			{
				print STDERR "Grads::Values: Unknown $keyword '$self->rword($i, 3)'\n";
			}
        }
        elsif ($keyword eq "ydef") {
            # print "Found $keyword\n";
            if ($self->rword($i, 3) eq "linear") {
				$ydef{'ny'} = $self->rword($i, 2);
				$ydef{'enum'} = $self->rword($i, 3);
				$ydef{'y0'} = $self->rword($i, 4);
				$ydef{'ystep'} = $self->rword($i, 5);
				$qh{$keyword} = \%ydef;
			}
			else
			{
				print STDERR "Grads::Values: Unknown $keyword '$self->rword($i, 3)'\n";
			}
        }
    }

    my $qh = \%qh;
    return $qh;
}


#.....................................................................

sub Exp {            # evaluate GrADS expression and EXPORT as piddle
    my $self = shift;
    my $expr = shift;  # GrADS expression to evaluate

    unless ( $PDL ) {
       carp 'Grads "exp" method requires PDL but PDL is not available';
       return undef;
    }

#   Retrieve dimension environment
#   ------------------------------
    my $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    my $dh = $self->Query("dims"); 
    $self->{Echo} = $Echo;                       # restore user Echo level
    croak "cannot query dim" unless ($dh);
    my ( $t1, $t2 ) = @{$dh->{t}};
    my ( $z1, $z2 ) = @{$dh->{z}};
    my ( $nx, $ny, $nz, $nt ) =  ( $dh->{nx}, $dh->{ny}, $dh->{nz}, $dh->{nt});

#   Initial implementation: require x,y to vary
#   Note: removing this restriction is not very hard, but requires
#         special handling the different dimension permutations
#         given the way GrADS invokes functions for XZ, YZ, ZT, etc
#   ---------------------------------------------------------------
    croak "Exp: lon/lat must be varying but got (nx,ny)=($nx,$ny)"
        if ( $nx==1 || $ny==1 );

#   Shortcut for 1 time/vertical level
#   ----------------------------------
    if ( $nz == 1 && $nt==1 ) { 
      my ($var, $grid) = $self->_Exp2D("$expr"); 
      return ( wantarray ? ($var, $grid) : $var );
    }

#   Loop over time/z, get a GrADS 2D slice at a time/z
#   --------------------------------------------------
    my ($var, $grid);
    my $Var = zeroes($nt,$nz,$nx,$ny); # space for sampled expr
    my $Meta = zeroes($nt,$nz); # grads exchange metadata
    my $lev = zeroes($nz); # grads exchange metadata
    my @time = ();
    my $rc = 0;
    $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    my $l = 0;   # time index 
    for my $t ( $t1..$t2 ) {
      $rc = $self->cmd("set t $t");   # One 2D "slice" at a time
      goto cleanup if ( $rc );
      my $k = 0;   # z index
      for my $z ( $z1..$z2 ) {
         $rc = $self->cmd("set z $z");   # One 2D "slice" at a time
         goto cleanup if ( $rc );
         ($var, $grid) = $self->_Exp2D("$expr");
         unless ( ref($var) eq "PDL" ) {
            $rc = 1;
            carp "Exp: cannot export <$expr> for t=$t, z=$z";
            goto cleanup;
     }
         $Var->slice("$l,$k,:,:") .= $var;
         $Meta->slice("$l,$k") .= $grid->{meta};
         $lev->slice("$k") .= $grid->{lev};
         $k++;
      } # foreach z
      push @time, $grid->{time}[0];
      $l++;
    } # foreach t

#   Remove dimensions with size 1
#   -----------------------------
    if ($nz==1 || $nt==1)  {
      $Var  =  $Var->clump(2);
      $Meta = $Meta->clump(2);
    }

#   Dimension names
#   ---------------
    my $dims;
    if    ( $nz==1 ) { $dims = [        'time', 'lon', 'lat' ]; }
    elsif ( $nt==1 ) { $dims = [         'lev', 'lon', 'lat' ]; }
    else             { $dims = [ 'time', 'lev', 'lon', 'lat' ]; }

#   Holds coordinate variables and dimension environment
#   ----------------------------------------------------
    my %Grid = (  
        name => $expr, 
        meta => $Meta, 
        denv => $dh,
        dims => $dims,
        time => \@time,
        lev  => $lev,
        lat  => $grid->{lat},
        lon  => $grid->{lon},
           );

    $Var->sethdr( \%Grid );
         
cleanup:

    $Var = undef if ( $rc );

#   Restore dimension environment
#   -----------------------------
    $rc = $self->_restoreDimEnv($dh);
    $Var = undef if ( $rc );

    $self->{Echo} = $Echo;                       # restore user Echo level

#   return piddle with data/metadata
#   --------------------------------
    return ( wantarray ? ($Var, \%Grid) : $Var );

  } # Exp

#.....................................................................
#
#       This is an internal version handling 2D slices, not user callable.
#
sub _Exp2D {            # evaluate GrADS expression and EXPORT as piddle
    my $self = shift;
    my $expr = shift;  # GrADS expression to evaluate

    local *XFILE;

    unless ( $PDL ) {
       carp 'Grads "exp" method requires PDL but PDL is not available';
       return undef;
    }

#   Whether using GrADS stdout for exchanging data
#   ----------------------------------------------
    my $pipe = 0;
       $pipe = 1  if ( "$self->{expTransferFile}" eq "-" );

    my $cmd = "ipc_define void = ipc_save($expr,$self->{expTransferFile})";

#   Open transfer file
#   ------------------
    if ( $pipe ) {
         *XFILE = $self->{Reader}; # this is GrADS stdout
         # I am not sure what todo about binmode in this case
    } else {
      open(XFILE, "<$self->{expTransferFile}") or
    croak "cannot open transfer file <$self->{expTransferFile}>";
    }
    binmode(XFILE); # may be a problem on Win32 with pipes....

#   Retrieve dimension environment for consistency checks
#   -----------------------------------------------------
    my $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    my $dh = $self->Query("dims"); 
    $self->{Echo} = $Echo;                       # restore user Echo level
       croak "Exp: cannot query dim" unless ($dh);
    my ( $nx, $ny, $nt ) =  ( $dh->{nx}, $dh->{ny}, $dh->{nt} );
       croak "Exp: got nt=$nt, expecting nt=1" if ( $nt>1 );

#   Save expression to a disk file or pipe
#   --------------------------------------
    my $rc;
    $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    if ( $pipe ) {
      local *Writer = $self->{Writer};
      print Writer "$cmd\n"; # asynchronously
      $rc = 0 # will check rc towards the end
    } else {
      $rc = $self->cmd("$cmd");
    }
    $self->{Echo} = $Echo;                       # restore user Echo level
    return undef if ( $rc );

#   Position stream pointer after <EXP> marker
#   ------------------------------------------
    if ( $pipe ) {
      $_ = "";
      while ( ! /^<EXP>/ ) {
#   print $_;
    chomp($_=<XFILE>);
      }
    }

#   Read transfer file into piddle
#   ------------------------------
    my $header = [
       {Type => 'float', NDims => 1, Dims => 20  },
    ];

    my ( $meta ) = readflex(\*XFILE,$header);

#   Notice that we have at most rank 2
#   ----------------------------------
    my $amiss = $meta->slice(0);
    my $ix = $meta->slice(1);
    my $iy = $meta->slice(2);
    my $mx = $meta->slice(3);
    my $my = $meta->slice(4);
    my $xt = $meta->slice(5);
    my $yt = $meta->slice(6);
    my $x0 = $meta->slice(7);
    my $dx = $meta->slice(8);
    my $y0 = $meta->slice(9);
    my $dy = $meta->slice(10);

#   Verify metadata
#   ---------------
    if ( $ix != 0 || $iy != 1 ) {
      carp "Exp: ix/iy mismatch; got ($ix,$iy), expected (0,1)";
      carp "Exp: make sure <$expr> is a valid expression";
      close XFILE unless $pipe;
      return undef;
    }
    if ( $nx != $mx || $ny != $my ) {
      carp "Exp: nx/ny mismatch; got ($mx,$my), but environment has ($nx,$ny)";
#      carp "Exp: will continue, but keep our fingers crossed";
#      close XFILE unless $pipe;
#      return undef;
      $nx = $mx;
      $ny = $my;
    }

#   Read the field
#   --------------
    $header = [
       {Type => 'float', NDims => 2, Dims => [ $nx, $ny ] },
       {Type => 'float', NDims => 1, Dims => [$nx] },
       {Type => 'float', NDims => 1, Dims => [$ny] }
    ];
    my ( $var, $lon, $lat ) = readflex(\*XFILE,$header);

#   When using pipe check $rc from asynchronous ipc_save, 
#   otherwise just close the disk file
#   -----------------------------------------------------
    if ( $pipe ) {
          my $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
          $rc = $self->_parseReader();
          $self->{Echo} = $Echo;                       # restore user Echo level
          if ( $rc ) {
        carp "problems exporting $expr";
            return undef; # only now can check rc from ipd_save
          }
    } else {
      close(XFILE); 
    }

#   Holds coordinate variables and dimension environment
#   ----------------------------------------------------
    my %grid = (  
        name => $expr, 
        meta => $meta, 
        denv => $dh,
        dims => ['lon','lat'], 
        time => [ $dh->{time}[0] ],
        lev  => pdl(1.) * $dh->{lev}[0],
        lat  => $lat,
        lon  => $lon,
           );

    $var->sethdr( \%grid ); # register grid

    return ( wantarray ? ($var, \%grid) : $var );

} # _Exp2D

#.....................................................................
sub _restoreDimEnv {
  my $self = shift;
  my $dh = shift;
  my $rc = 0;
  if ( $dh ) {
    $rc = $self->cmd("set x @{$dh->{x}}"); return $rc if $rc;
    $rc = $self->cmd("set y @{$dh->{y}}"); return $rc if $rc;
    $rc = $self->cmd("set z @{$dh->{z}}"); return $rc if $rc;
    $rc = $self->cmd("set t @{$dh->{t}}"); return $rc if $rc;
  } else {
    $rc = 1;
  }
  return $rc;
}

#..................................................................
sub Display { # Displays (in GrADS) PDL or GrADS expression

    my $self = shift;
    my $Expr  = shift;  # PDL or GrADS expression
    my $Grid = shift;   # optional metadata

    my $rc = 0;
    if ( ref($Expr) eq "PDL" ) {
      $rc = $self->Imp('<display>', $Expr, $Grid );
    } else {
      my $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
      my $dh = $self->Query("dims"); 
      $self->{Echo} = $Echo;                       # restore user Echo level
      croak "cannot query dim" unless ($dh);
      my $cr = "\n" if ( $dh->{nt} > 1 );
      $rc = $self->cmd("display $Expr $cr");
      $self->_parseReader() if ( $rc && $dh->{nt} > 1 ); # extra cr was
                                                            # not needed 
    }

    return $rc;

}

#.....................................................................

sub Imp {             # import piddle into GrADS by defining variable

    my $self = shift;
    my $name = shift;  # name of variable in GrADS namespace
    my $Var  = shift;  # PDL variable
    my $Grid = shift;  # optional metadata

    local *IFILE;

    unless ( $PDL ) {
       carp 'Grads "imp" method requires PDL but PDL is not available';
       return undef;
    }

#   Whether using GrADS stdout for exchanging data
#   ----------------------------------------------
    my $pipe = 0;
       $pipe = 1  if ( "$self->{impTransferFile}" eq "-" );

#   Retrieve dimension environment
#   ------------------------------
    my $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    my $dh = $self->Query("dims"); 
    $self->{Echo} = $Echo;                       # restore user Echo level
    croak "cannot query dim" unless ($dh);
    my ( $nx, $ny, $nz, $nt ) =  ( $dh->{nx}, $dh->{ny}, $dh->{nz}, $dh->{nt});
    my $nl = $nz * $nt;

    unless ( ref($Grid) eq "HASH" ) { 
      if ( ref($Var) eq "PDL" ) { $Grid = $Var->gethdr() };
      croak "metadata missing" unless ( $Grid );
    }

#   Initial implementation: require x,y to vary
#   Note: remove this restriction is not very hard, but requires
#         special handling the different dimension permutatio  ns separately
#         given the way GrADS invokes functions for XZ, YZ, ZT, etc
#   ----------------------------------------------------------------------
    croak "Imp: lon/lat must be varying but got (nx,ny)=($nx,$ny)"
        if ( $nx==1 || $ny==1 );

#   Determine the actual load command
#   ---------------------------------
    my $cmd; 
    if ( "$name" eq "<display>" ) { 
     $cmd = "display ipc_load()";        
      croak "Imp: for display only one of z/t can vary but got (nz,nt)=($nz,$nt)"
    if ( $nz!=1 && $nt!=1 );
    } else {
      $cmd = "ipc_define $name = ipc_load()";
    }

#   Save piddle to transfer stream; if pipe tell GrADS right away
#   to start looking for data on stdin
#   -------------------------------------------------------------
    if ( $pipe ) {
      my $rc = $self->cmd("ipc_open - r");
        croak "<ipc_open - r> failed" if ( $rc );
      *IFILE = $self->{Writer}; # this is GrADS stdin
      print IFILE "$cmd\n";
    } else {
      open(IFILE, ">$self->{impTransferFile}") or
    croak "cannot open transfer file <$self->{impTransferFile}>";
    }
    binmode(IFILE);

    my $Meta;
    if ( $nz>1 && $nt>1 ) {
      $Var  =  $Var->mv(0,1)->clump(2); 
      $Meta = $Grid->{meta}->mv(0,1)->clump(2); 
    } else {
      $Meta = $Grid->{meta};
    }

    for my $n (0..$nl-1) {

      my ($var, $grid);

      my $meta;
      if ( $nl==1 ) {  # special case
        $var = $Var;
    $meta = $Meta;
      } else {
    $var = $Var->slice("$n,:,:");
    $meta = $Meta->slice("$n,:");
      }

#     Extract metadata
#     ----------------
      my $lon = $Grid->{lon};
      my $lat = $Grid->{lat};

#     Verify metadata
#     ---------------
      unless ( $meta->nelem == 20 ) {
    carp 'Imp: invalid metadata, meta size is not 20';
        close IFILE unless $pipe;
    return undef;
      }
      my $mx = $meta->slice(3);
      my $my = $meta->slice(4);
      if ( $nx != $mx || $ny != $my ) {
    carp "Imp: nx/ny mismatch; got ($mx,$my), but environment has ($nx,$ny)";
    $nx = $mx;
    $ny = $my;
#   carp "Imp: nx/ny mismatch; got ($mx,$my), expected ($nx,$ny)";
#       close IFILE unless $pipe;
#   return undef;
      }

#     Write to file
#     -------------
      my $hdr = writeflex ( \*IFILE, $meta, $var->float, $lon, $lat ) or
    croak "cannot write to transfer file";

    }

    if ( "$name" eq "<display>" ) { 
         print IFILE "\n" if $nt > 1; # handle animation prompt
    }

#   Load disk transfer file into GrADS; in case of pipes just
#   check error condition on ipc_load 
#   ---------------------------------------------------------
    my $RC = 0;
    $Echo = $self->{Echo}; $self->{Echo} = 0; # be discrete about this
    if ( $pipe ) {
      $RC = $self->_parseReader() if $pipe ; # consume simulated makers
    } else {
      close IFILE;
      my $rc = $self->cmd("ipc_open $self->{impTransferFile} r");  
      $RC = $rc if $rc;
      $rc = $self->cmd($cmd);
      $RC = $rc if $rc;
    }      

    $self->cmd("ipc_close"); 
    $self->{Echo} = $Echo;    # restore user Echo level

    return $RC;

} # Imp

#..................................................................
sub rline {                   # return (i)-line from Result strings

    my $self = shift;
    my    $i = shift;

    my $word = "$self->{Lines}->[$i]";

    return $word;

}

#..................................................................
sub rword {                   # return (i,j)-word from Result strings

#
#  For consistency with GrADS gs scripts, the colum index "j"
#  on input starts from 1; 
#

    my $self = shift;
    my ( $i, $j ) = @_;

    return "" if (!defined($self->{Words}->[$i][$j-1]));
    my $word = "$self->{Words}->[$i][$j-1]";

    return $word;

}

#..................................................................
sub Set {         # run several GrADS "set" according to input array

    my $self = shift;
    my   $gc = shift;

    my $opt;

    for $opt ( @{$gc} ) {
        my $rc = $self->cmd("set $opt");
        return $rc if ( $rc );
    }
    return 0;

}

#..................................................................
sub _parseReader {

    my $self = shift;

    my ($got, $token);
    my $nLines = 0;
    my $rc = 0;

#    local *Reader = $self->{Reader};
    my $READER = $self->{Reader};
    my $Echo   = $self->{Echo};
    my $h      = $self->{Handle};


    $token = "" unless defined($token);
    my $line;
    my @tokens;

    pump $h until $$READER =~ /<\/IPC>/mgc ;
    my @Page = split '\n', $$READER;

    while ( "$token" ne "<IPC>" ) {
	chomp($got = shift @Page);
	$line = $got; 
	@tokens = split(' ',$got); 
	$token = $tokens[0] if defined($tokens[0]);
    }

#   Record the grads command issued
#   -------------------------------
    my @lines = ();    
    my @words = ();
    push @lines, $line;
    push @words, [ @tokens ];
#   print "$got\n" if ( $Echo );

    chomp($got = shift @Page);

    $line = $got; @tokens = split(' ',$got); 
    $token = $tokens[0] if defined $tokens[0];
    while ( "$token" ne "</IPC>" ) {
	if ( "$token" eq "<RC>" ) { 
	    $rc = $tokens[1];
	} else { 
	    push @lines, $line;
	    push @words, [ @tokens ];
	    $nLines++;
	}
	chomp($got = shift @Page);
	print "$got\n" if ( $Echo );
	$line = $got; @tokens = split(' ',$got); 
	$token = $tokens[0] if defined $tokens[0];
    }

    $self->{Lines} = \@lines;  # save line output for later parsing
    $self->{Words} = \@words;  # save word output for later parsing
    $self->{nLines} = $nLines;

    return $rc;

}

} # End of outermost BEGIN block, used to contain local variables

1;

__END__

=head1 NAME

Grads - GrADS Client Class

=head1 SYNOPSIS

=over 4

=item

I<use> B<Gerl>;

=item 

$I<ga> = B<new> I<Grads> ( [I<OPTIONS>] );

=item 

$I<rc> = $I<ga>->B<cmd> ( I<COMMAND> );

=item

$I<rc> = B<Display> ( I<GA_EXPR> | $I<PERL_EXPR>[, I<GRID> )

=item 

$I<x> = $I<ga>->B<Exp> ( I<EXPR> );

=item

($I<x>,$I<grid>) = $I<ga>->B<Exp> ( I<EXPR> );

=item 

$I<rc> = $I<ga>->B<Imp> ( I<GA_VAR>, I<PERL_VAR>, [I<GRID>] ) 

=item 

$I<fh> = $I<ga>->B<Open> ( I<FILENAME> [, I<FILETYPE>] );

=item 

$I<qh> = $I<ga>->B<Query> ( I<PROPERTY> );

=item 

$I<rc> = $I<ga>->B<Set> ( I<PROPERTIES> );

=item 

$I<ln> = $I<ga>->B<rline>(I<i>);

=item 

$I<wd> = $I<ga>->B<rword>(I<i>,I<j>);

=back

=head1 DESCRIPTION

C<Grads> is a class implementing an interface to the Grid Analysis and
Display System (GrADS) by means of bi-directonal pipes.  It starts
GrADS, sends commands to it, parses its output and return codes, and
provides high level interfaces to query GrADS properties and dimension
environment. The companion module L<http://opengrads.org/doc/perl/Gerl/> provides a procedural
interface to this module, with the look and feel of a traditional GrADS
script.

In the remainder of this document we will use C<Grads> to mean this
Perl class, and GrADS to refer to the GrADS application that this
class interfaces to.

=head1 REQUIREMENTS

You must have Perl 5 and GrADS installed on your system. This module
requires a version of GrADS supporting inter-process communication
through the "-u" command line argument. This is available with GrADS
Version 1.9.0 and later, not including the 1.9 beta versions. Any of
the OpenGrADS variants also meet this requirement. Methods I<Imp> and
I<Exp>, which allows GrADS to exchange data with the Perl Data
Language (L<http://pdl.perl.org>), obviously requires packages C<PDL> and
C<PDL::IO::FlexRaw>, in addition to the GrADS user defined extension
C<libipc>. No need to install these modules if you do not intent to
use these methods.

=head1 TUTORIAL

=head2 Getting started

For running this tutorial you will need a sample GrADS dataset. Please
download F<model.nc> from L<http://opengrads.org/sample_data>. If you
are new to GrADS you may want to read the Tutorial on
L<http://grads.iges.org/grads/gadoc/tutorial.html>. This document is not a
GrADS tutorial but rather a tutorial of the Perl interface to GrADS.

In this example we will use the L<Data::Dumper> module to examine the
contents of hashes returned by some of the methods. So, we start by
using these 2 modules:

   use  Grads;
   use  Data::Dumper;

Let's create a C<Grads> object by starting the I<gradsnc> binary in
landscape mode, with an active graphics window:

   $ga = new Grads { Bin=>"gradsnc", Port=>0, Window=>1, };

The I<cmd> method is used to send generic commands to GrADS, e.g.,

   $rc = $ga->cmd("q config");

The return code C<$rc> will be C<0> if all went well, and non-zero if
the particular GrADS command exited with an error. 

=head2 Opening files

The I<Open> method opens a GrADS dataset in any of the supported
formats:

  $fh = $ga->Open("wrong_file.nc") or
        warn ">>> Cannot open wrong_file.nc as expected, proceeding.\n";

In this particular case we fed it a bogus file name to force an error
condition. Let's try again, this time with the F<model.nc> file that
you just downloaded:

  $fh = $ga->Open("model.nc") or
    die "cannot open model.nc but it is needed for testing, aborting ";

I<Open> returns a file handle C<$fh> with useful metadata about the
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
          'var_names'=> [ 'Variable Title 1','Variable Title 2',...],
          'nx'       => '72',
          'ny'       => '46',
          'nz'       => '7',
          'nt'       => '5',
        };

=head2 Querying the GrADS state

Similarly, the I<Query> method returns a query handle with information
about the particular GrADS property. Here are a few examples:

  $qh = $ga->Query("time");
  print "\n>>> Time handle: ";   print Dumper($qh); 

  $fh = $ga->Query("file");
  print "\n>>> File handle: ";   print Dumper($fh); 

  $dh = $ga->Query("dims");
  print "\n>>> Dim handle: ";    print Dumper($dh); 

As of this writing only a handfull of GrADS C<query> properties are
implemented by the I<Query> method, but this list is growing with each
release. Be sure to contribute any extension you add. In the meantime,
you can use the I<rword> and I<rline> methods to parse the output of
native C<query> command.  Read on.

=head2 Parsing GrADS output, the traditional way

Traditionally, the built in GrADS scripting language (F<gs>) includes
functions C<sublin> and C<subwrd> to parse the lines and words within
line of each GrADS command issued. To aid the conversion of I<gs>
scripts to Perl, we have included methods I<rword> and I<rline> which
give access to words and lines in the GrADS output stream. The
I<rword>(i,j) method returns the I<j>-th word in the I<i>-th line of the
GrADS command just issued, viz.

  $ga->cmd("q config");
  for $i ( 1...$ga->{nLines} ) {
      printf("RWORD %3d: ", $i);
      $j=1;  # starts from 1
      while ( $word=$ga->rword($i,$j) ) {
      print $word . " ";
      $j++;
      }
      print "\n";
      $i++;
  }

To obtain a given output line, use the I<rline> method:

  print "RLINE  *3: " . $ga->rline(3) . "\n\n";

=head2 The Set method

The I<Set> method is used to issue a batch of GrADS C<set> commands,
stored in an array. This is particularly useful to define a graphics
context that can be reused prior to issuing each C<display>
command. Here is an example:

  my @gc;
  $ga->{Echo} = 1;
  push @gc, "grads off";
  push @gc, "gxout shaded";
  $rc = $ga->Set(\@gc);

=head2 Interfacing to the Perl Data Language (PDL)

Method I<Exp> allows you to export a GrAS variable into a Perl Data
Language (PDL) object, commonly refered to as I<piddles>:

   $ps = $ga->Exp ps;
   print "ps = $ps";

The output piddle C<ps> is sometimes refered to as a I<GrADS field> as
it register a grid,

   $grid = $ps->gethdr()

which contains information about the coordinate variables (longitude,
latitude, vertical level and time), in addition to low level metadata
for exchanging data with GrADS. (This is the same concept of I<field>
introduced by Earth-system Modeling Framework, ESMF). Alternatively,
you can explicitly grab the C<$grid> during export,

   ($ps,$grid) = $ga->Exp ps; 

You can also import a piddle with the I<Imp> method, provided one
specifies the necessary C<grid> metadata: 

   $logps = log($ps); 
   $rc = $ga->Imp logps, $logps, $grid; 
   $rc = $ga->display logps;

Of course, there is more than one way of doing this. Having a C<$grid>
you can use the piddle's C<sethdr()> method to regsiter it:

   $logps->sethdr($grid);

and then there is no need to explicitly pass C<$grid> to method
C<Imp>: 

   $rc = $ga->Imp logps, $logps;
   $rc = $ga->Display logps;

Given the appropriate metadata, one can also display a piddle in
GrADS. If all one wants to do is to display a variable there is no
need to import it first as we did above. In the previous example, you
could display C<$logps> directly:

   $rc = $ga->Display $logps;

The current implementation requires that both C<x> and C<y> dimensions
be varying. In addition, both C<z> and C<t> dimensions can be varying,
individually or at the same time.  Just like GrADS itself, C<Display>
cannot handle both C<z> and C<t> varying at the same time. Here is an
example exporting a 4D variable:

  $ga->cmd "set t 1 5";
  $ga->cmd "set z 1 7";
  $ua = Exp ua;
  print $ua->dims;


=head2 Terminating your GrADS session

As usual in perl, the C<Grads> destructor will be invoked when the object
gets out of scope of when it is explitly undefined like this:

  $ga = undef;

This will cause a C<quit> to be sent to GrADS which in turn will end
the connection. 


=head1 CONSTRUCTOR

=head2 B<new> C<Grads> ( [I<OPTIONS>] )

=over 4

This is the constructor for a new C<Grads> object. It starts the GrADS
application with a bi-directional pipe so that it can send commands to
the GrADS C<stdin> and read the output from the GrADS C<stdout>. The
I<OPTIONS> are passed in a hash like fashion, using key and value
pairs. Possible options are:

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

Set to 1 to enable the GrADS graphics window; the
default is to run GrADS in batch mode.

=item I<Port> 

Set to 1 to start GrADS in portrait mode; the default
is to start GrADS in landscape mode.

=item I<Opts> 

Other options you would like to pass to the GrADS
application on the command line, e.g., C<<Opts=>'-g 800x600'>>.

=back

=back

=head1 METHODS

Unless otherwise stated all methods returning the error code $I<rc>
behave much in the same way as a shell command. A zero $I<rc> value
means that the method completed sucessfully, while a non-zero value
indicates an error condition.  When a method states that it returns a
reference or a list, failure will be returned as I<undef> or an empty
list. In describing the methods below we sometimes omit the object
reference for notational simplicity.

=head2 $I<rc> = B<cmd> ( I<COMMAND> )

=over 4

Executes a generic GrADS command as one would enter on the command
line. Use the I<rword> and I<rline> methods to parse the C<stdout>
generated by this command. Example:

   $rc = $ga->cmd "enable print example.gm";
   die "cannot enable print" if ( $rc );

=back

=head2 $I<rc> = B<Display> ( I<GA_EXPR> | $I<PERL_EXPR>[, I<GRID> )

=over 4

This method displays either a regular GrADS expression or a piddle
expression. 

For GrADS expressions, it detects such cases with varying time
dimension and adds an additional carriage return in response to the
anticipated user input at the end of an animation sequence.

For piddle expressions, one can specify additional metadata; see
C<Exp> below for more more information on this metadata. In fact,
C<Display> of piddle expressions is simply syntatic sugar for
C<< Imp(<display>,EXPR,GRID). >> These 2 statements are equivalent:

   $ga->Display $ts;
   $ga->Imp '<display>', $ts;

=back

=head2 $I<x> = B<Exp> ( I<EXPR> )

=head2 ($I<x>,$I<grid>) = B<Exp> ( I<EXPR> )

=over 4

This is the I<Export> method which evaluates the GrADS expression
I<EXPR> and return an array of floating numbers $I<x> as a Perl Data
Language (L<http://pdl.perl.org>) I<piddle> object. This method requires the
C<PDL::IO::FlexRaw> package. In list context, the optional metadata
$I<grid> is also returned. You will need this metadata to import back
into GrADS derived piddles which does not have this information stored
inside. In scalar context one can retrieve C<$grid> with the PDL 
C<gethdr> method:

   $grid = $x->gethdr()

C<$grid> contains information about the coordinate variables,
as well as relevant metadada needed to exchange data with GrADS. The
$I<grid> is returned as a reference to a hash defining the following keys:

=over 8

=item I<name> 

Usually the same as the GrADS expression being
evaluated

=item I<meta> 

Metadata used during data exchange with GrADS as described below; the
dimension of this piddle is related to the dimension of the main data
piddle C<$x>.

=item I<denv> 

A hash with the current dimension environment as
returned by C<Query("dims")>

=item I<dims> 

A perl array with the name of the piddle dimensions, e.g., ['time',
'lev', 'lon', 'lat']. The size of this array is the same as the rank
of C<$x>.

=item I<time> 

Perl array containing time strings such as C<12Z01jan1987>

=item I<lev> 

1D piddle with a vertical levels 

=item I<lat> 

1D piddle with latitudes 

=item I<lon> 

1D piddle with longitudes

=back 

The current implementation requires that both <x> and <y> dimensions
be varying. In addition both <z> and <t> can vary as well, resulting
in piddles that are 2D, 3D or 4D. If (nt,nz,nx,ny) are the sizes of
the (time,vertical,longitude,latitude) dimensions associated with
I<EXPR> then we have:

=over 4

=item *

When the C<z> and C<t> dimensions are B<fixed>, C<Exp> returns a 2D
piddle of dimensions (nx,ny), and 

   $meta = $grid->{meta}

is a 1D piddle of size 20 describing the GrADS exchange metadata
associated with this horizontal slice.

=item *

When one of the C<z>/C<t> dimensions is varying while the other is
fixed, C<Exp> returns a 3D piddle of dimensions C<(nt,nx,ny)> or
C<(nz,nx,ny)> as the case may be. The corresponding exchange metadata
C<$meta} > has dimensions C<(nt,20)> or C<(nz,20)>.

=item *

When both C<z> and C<t> dimensions are varying C<Exp> returns a 4D
piddle of dimensions C<(nt,nz,nx,ny)>. The corresponding exchange metadata
C<$meta> has dimensions C<(nt,nz,20)>.

=back

You may have noticed that for consistency with current PDL practice,
the dimensions of the 4D arrays are kind of strange: C<(nt,nz,nx,ny)>
instead of C<(nt,nz,ny,nx)>. This is done so that each of the
horizontal (nx,ny) slices can be easily displayed in PDL with
functions such as C<imag>.

=head3 Exchange metadata C<$meta>

The size of the last dimension of the exchange metadata
C<$meta> is 20, corresponding to the following float numbers:
       
       1:  Undefined value for the grid
       2:  i dimension (idim).  Dimensions are:
           -1 - None
            0 - X dimension (lon)
            1 - Y dimension (lat)
            2 - Z dimension (lev)
            3 - T dimension (time)
       3:  j dimension (jdim).  Note:  if idim and
           jdim are -1, the grid is a single value.
           If jdim is -1, the grid is a 1-D grid.
       4:  number of elements in the i direction (isiz)
       5:  number of elements in the j direction (jsiz)
           Array is dimensioned (isiz,jsiz).
       6:  i direction linear flag.  If 0, the
           i dimension has non-linear scaling.
       7:  j dimension linear flag.
       8:  istrt.  This is the world coordinate value
           of the first i dimension, ONLY if the i dimension
           has linear scaling and the i dimension is not
           time.
       9:  iincr.  Increment in the i dimension of the
           world coordinate.  ONLY if the i dimension has
           linear scaling.
       10: jstrt.  World coordinate of the first j
           dimension, only if the j dimension has linear
           scaling, and the j dimension is not time.
       11: jincr.  World coordinate increment for j
           dimension.
       12: If one of the dimensions is time, values
           12 to 16 are defined as the start time
           12 is the start year.
       13: start month
       14: start day
       15: start hour
       16: start minute
       17: Values 17 and 18 contain the time increment
           for the time dimension.  17 contains the
           increment in minutes.
       18: increment in months.  (GrADS handles all
           increments in terms of minutes and months).
       19,20: reserved

Additional information about this metadata can be found on the
documentation fot C<libipc>, a library of GrADS extensions used to
implement the data exchange with GrADS.

=back

=head2 $I<rc> = B<Imp> ( I<GA_VAR>, I<PERL_VAR>, [I<GRID>] ) 

=over 4

This is the I<Import> method which takes an array of floating numbers
I<PERL_VAR>, implemented as a Perl Data Language (L<http://pdl.perl.org>) I<piddle>
object, sends it to GrADS, defining it as a GrADS variable I<GA_VAR>.
This method requires the C<PDL::IO::FlexRaw> package.

In the current implementation, the input piddle is required to have
registered a specific I<header> with the piddle method C<sethdr>, or
you have to provide the same information through the optional
parameter I<GRID>. See method C<Exp> above of a descriptions of the
contents of I<GRID>. This information is returned when a variable is
exported from GrADS with method I<Exp>, and this metadata can be
reused during an import operation. See B<Exp>.

=back

=head2 $I<fh> = B<Open> ( I<FILENAME> [, I<FILETYPE>] )

=over 4

Opens a GrADS dataset in one of the supported formats, issuing a GrADS
C<open>, C<sdfopen> or C<xdfopen> command depending on the file
type. By default, an heuristic method based on the file name is used
to determine the file type. Files with extension C<hdf> and C<nc>, or
starting with C<http://> are assumed to be opened with C<sdfopen>;
files with extension C<ddf> are assumed to be opened with C<xdfopen>;
all else is opened with the GrADS command C<open>. The file name
extension matching is I<case insensitive>. The optional parameter
I<FILETYPE>, which can take the values C<sdf>, C<xdf> or C<ctl>, can
be used for those cases when the heuristic method fails.

This method returns $I<fh>, a reference to a hash containing the file
metadata. This is the same information as returned by
I<Query>("file"); see below for description of this hash.

=back

=head2 $I<qh> = B<Query> ( I<PROPERTY> )

=over 4

This method issues the GrADS command C<< query I<PROPERTY> >>, and
returns the output of this command in a reference to a hash. The
following properties are currently implemented:

=back 

=head3 $I<qh> = B<Query> ( C<file #> ) 

=over 4

Returns information about the file number
C<#>. If the file number is ommitted the default file is used. The
following keys are defined in the output hash:

=over 8

=item I<fid> 

The file Id number

=item I<title> 

The title of the dataset

=item I<desc> 

The description of the dataset

=item I<bin> 

The binary file name

=item I<type> 

File type

=item I<nx> 

Number of longitudinal points

=item I<ny> 

Number of latitudinal points

=item I<nz> 

Number of vertical levels

=item I<nt> 

Number of times on file

=item I<nvars> 

Number of variables on file

=item I<vars> 

Array with variable list

=item I<var_levs> 

Array with number of levels for each variable

=item I<var_names>

Array with long name for each variable

=back

=back

=head3 $I<qh> = B<Query> ( C<dims> )

=over 4

Returns information about the dimension environment. The
following keys are defined in the output hash:

=over 8

=item I<dfile> 

Default file number

=item I<x_state> 

x-coordinate state: C<fixed> or C<varying>

=item I<lon> 

longitudinal range

=item I<x> 

x-index range

=item I<y_state> 

y-coordinate state: C<fixed> or C<varying>

=item I<lat> 

latitudinal range

=item I<y> 

y-index range

=item I<z_state> 

z-coordinate state: C<fixed> or C<varying>

=item I<lev> 

level range

=item I<z> 

z-index range

=item I<t_state> 

time-coordinate state: C<fixed> or C<varying>

=item I<time> 

time range

=item I<t> 

t-index range

=back

=back

=head3 $I<qh> = B<Query> ( C<time> ) 

=over 4

Returns information on the current time
coordinate. The following keys are defined in the output hash:

=over 8

=item I<time> 

time string 

=item I<dd> 

2 digit day, e.g., C<12> 

=item I<day> 

day of the week, e.g., C<Tue>

=item I<hh> 

2 digit hour in the range 0...23

=item I<nn> 

2 digit seconds

=item I<mmm> 

3 letter month, e.g., C<Jan>

=item I<mm> 

2 digit month, e.g., C<01>

=item I<yyyy> 

4 digit year, e.g., 2007

=back

=back


=head3 $I<qh> = B<Query> ( C<gr2w I J> ) 

=over 4

Converts grid to world coordinates. The following keys are defined
in the output hash:

=over 8

=item I<Lon>

Longitude

=item I<Lat>

Latitude

=back

=back


=head3 $I<qh> = B<Query> ( C<xy2w X Y> ) 

=over 4

Converts screen to world coordinates. The following keys are defined in the
output hash:

=over 8

=item I<Lon>

Longitude

=item I<Lat>

Latitude

=back

=back

=head3 $I<qh> = B<Query> ( C<pp2xy X Y> ) 

=over 4

Converts virtual page to real page coordinates. The following keys are
defined in the output hash:

=over 8

=item X

x-coordinate

=item Y

y-coordinate

=back

=back


=head3 $I<qh> = B<Query> ( C<gr2xy I J> ) 

=over 4

Converts grid to real page coordinates. The following keys are defined
in the output hash:

=over 8

=item X

x-coordinate

=item Y

y-coordinate

=back

=back


=head3 $I<qh> = B<Query> ( C<w2xy LON LAT> ) 

=over 4

Converts world to real page coordinates. The following keys are
defined in the output hash:

=over 8

=item X

x-coordinate

=item Y

y-coordinate

=back

=back



=head3 $I<qh> = B<Query> ( C<xy2gr X Y> ) 

=over 4

Converts real page to grid coordinates. The following keys are defined
in the output hash:

=over 8

=item I<I>

x-coordinate in index space; noice that I<I> is a float point number

=item I<J>

y-coordinate in index space; noice that I<J> is a float point number

=back

=back


=head3 $I<qh> = B<Query> ( C<w2gr LON LAT> ) 

=over 4

Converts world to grid coordinates. The following keys are defined in
the output hash:

=over 8

=item I<I>

x-coordinate in index space; noice that I<I> is a float point number

=item I<J>

y-coordinate in index space; noice that I<J> is a float point number

=back

=back


=head3 B<Note:>

=over 4

Eventually all the query properties will be implemented. 

=back

=head2 $I<rc> = B<Set> ( I<PROPERTIES> ) 

=over 4

This methods takes a series of arguments for the GrADS C<set> command
and execute them in sequence. On input, I<PROPERTIES> is an array
reference containing the properties to be set. Example:

  push @gc, "grads off";
  push @gc, "gxout shaded";
  $rc = $ga->Set(\@gc);

=back

=head2 $I<line> = B<rline>(I<i>)

=over 4

This method returns the I<i>-th line of the C<stdout> generated by the
last GrADS command. The number of lines available can be retrieved
from the object with key C<nLines>, e.g., C<$ga->{nLines}>.

=back

=head2 $I<word> = B<rword>(I<i>,I<j>)

=over 4

This method returns the I<j>-th word within the I<i>-th line of the
C<stdout> generated by the last GrADS command. The number of lines
available can be retrieved from the object with key C<nLines>, e.g.,
C<< $ga->{nLines} >>.

=back

=head1 TO DO

=over 4

=item 

Complete implementation of the I<Query> method.

=item 

Remove requirement of varying x/y dimensions during C<Imp/Exp>.

=back

=head1 BUGS

=over 4

=item 

On some systems it hangs when the name of an invalid GrADS
executable is provided.

=item 

Under PerlDL, any attempt to read from stdin from within GrADS will
fail, including the I<pull> function in C<gs> scripts and the request
for I<CR> after an animation sequence is displayed.  For animation,
the workaround is to use method C<Display> which automatially includes
an extra carriage return to avoid locking up.

=item

Sometimes the output buffer may get messed up, particularly when an
error occurs during data exchange with PDL.

=back

=head1 SEE ALSO

=over 4

=item 

L<http://opengrads.org/doc/perl/Gerl/Gerl.html> - Gerl, a procedural interface to GrADS

=item 

L<http://opengrads.org/doc/perl/gadl/gadl.html> - gadl, a wrapper script to
start the PDL shell loading Gerl.

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

=item 

L<http://www.esmf.ucar.edu> - Earth-system Modeling Framework (ESMF)

=back

=head1 AUTHOR

Arlindo da Silva <dasilva@opengrads.org>

=head1 COPYRIGHT

Copyright (c) 2006-2008 Arlindo da Silva. All rights reserved.
This program is free software; you can redistribute it and/or modify it 
under the same terms as Perl itself.

=cut




