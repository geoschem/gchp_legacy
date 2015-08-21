package Rc;
#
#  This Class implements a Perl interface to Resource files. It has 
#  methods for reading/parsing and writing resource files.
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
#  09Oct2006  da Silva  Initial implementation based on code fragments
#                       from the original "red" script.
#
#--------------------------------------------------------------------------

require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(new dump write);

#.........................................................................

 use FileHandle;
 use Carp;
 use Env;                 # Environment variables

 $VERSION = 1.00;

# The public Rc object
# --------------------
  my %rc = {

    file   => undef,  # file name of the associated resource file
    name   => undef,  # name of the resource file
    title  => undef,  # title of the resource file
    Line   => undef,  # each line of the resource file: as is
    Help   => undef,  # parsed help information
    Hist   => undef,  # parsed revision information
    Ui     => undef,  # parsed user interface 
    debug  => undef,  # print things as it goes along; mostly for debugging
    critique => undef, # callback for critique of user input
  };

#.........................................................................

sub new {                                                 # Constructor

    my $that = shift;
    my $class = ref($that) || $that;

    my ( $argv ) = @_;

    my $file  = $argv->{file};
    my $debug = $argv->{debug};

#   Record input args
#   -----------------
    $rc{file}   = $file;
    $rc{debug}  = $debug;

#   Bless it
#   --------
    $self = { %rc, };
    bless $self, $class;

    $self->_parseFile();  # _parse is kind of private

    return $self;

}

#..........................................................................

sub DESTROY {                                               # Destructor

   my $self = shift;

   print "Destroyed Rc associated with $self->{file}\n" 
         if ( $self->{verb} );

   $self->{file}  = undef;
   $self->{name}  = undef;
   $self->{title} = undef;
   $self->{Line}  = undef;
   $self->{Help}  = undef;
   $self->{Hist}  = undef;
   $self->{Ui}    = undef;
   $self->{Dict}  = undef;
   $self->{debug} = undef;
   
}

#..........................................................................

sub _parseFile {                     # reads and parses a resource file

    my $self = shift;

#   Local short hands
#   -----------------
    my $RCname  = undef;	# Resource File name 
    my $RCtitle = undef;	# Resource title 
    my @Line    = ();		# Actual RC lines 
    my @UI      = ();           # parsed user interface tokens
    my %Dict    = {};

    $i = -1;  # line index
    $m = -1;  # GUI index (actual questions)

    $file = $self->{file};
    $debug = $self->{debug};
    
#   Open the file
#   -------------
    open (FILE, $file) or confess "cannot open file $file";

# Main loop over lines on file
# ----------------------------
LINE:
  while (<FILE>) {

    chop;     # strip record separator

    $line = $_;

    $i++;
    push @Line, $line; # store all lines for later output

    @Fld  = split(' ', $_, 9999);

    if ( $Fld[0] eq '#BOP' ) {
         $doing_help = 0;         
         $doing_hist = 0;         
         $doing_prol = 1;         
         next LINE;
     }

    if ( $Fld[1] eq '!RESOURCE:' ) {
         $RCname = $Fld[2];
         shift @Fld;
         shift @Fld;
         shift @Fld;
         shift @Fld;
	 $RCtitle = join(' ', @Fld );
         next LINE;
    }

    if ( $Fld[1] eq '!HELP:' ) {
         $doing_help = 1;         
         next LINE;
     }

     if ( $Fld[1] eq '!REVISION' ) {
         $doing_help = 0;         
         $doing_hist = 1;         
         next LINE;
     }

     if ( $Fld[0] eq '#EOP' ) {
         $doing_help = 0;         
         $doing_hist = 0;         
         $doing_prol = 0;         
         next LINE;
     }

    if ( $doing_help ) {
        $help = $line;
        $help =~ s/^\#/ /;
        push @Help, $help;
    }

    if ( $doing_hist ) {
        $hist = $line;
        $hist =~ s/^\#/ /;
        # print "hist = $hist\n";
        push @Hist, $hist;
    }

    next LINE if ( $doing_prol );

    ( $tmp, $cmd ) = split('#', $_,   2);  # rcv = resource value
    ( $rcn, $rcv ) = split(':', $tmp, 2);  # rcn = resource name
    $rcn =~ s/[ ]*\?[ ]*/\?/;
    $cmd =~ s/^\#[ ]*//;                   # cmd = GUI directive

    $rcn = $rcn . ":" if $rcv;       # put ":" back in label

#   Continue if a UI command is found
#   ---------------------------------
###    next LINE unless ( $rcv =~ /[ ]*\?/ || $cmd =~ /[ ]*&/ );

#   Is this an UI line?
#   -------------------
###    if ( $rcv =~ /[ ]*\?/ || $rcv eq "" ) {
    if ( $rcv =~ /[ ]*\?/ || $cmd =~ /[ ]*&/ ) {

#        Parse widget and args
#        ---------------------
         $m++ if ( $rcv );                # keep track of questions
         $rcv =~ s/[ ]*\?[ ]*//;          # strip out "?"

	 if ( $cmd =~ /[ ]*&/ ) {
	   ( $rcw, $rca ) = split(' ', $cmd, 2);  
	 } 
	 else {
	   $cmd =~ s/[ ]*//;
	   $rcw = "&Str";   # default widget name
	   $rca = $cmd; 
	 } 

#        Record user interface request
#        -----------------------------
         $tokens = {
                 index  => $i,   # line index in original rc file
                 name   => $rcn, # resource name
                 value  => $rcv, # actually, the default resource value
                 widget => $rcw, # the widget name
                 args   => $rca, # the arguments for the widget  
                 set    => 1,    # always UI as been set 
         };

         push @UI, $tokens;       # sequential array with UI tokens 

     } else {

         $tokens = {
                 index  => $i,    # line index in original rc file
                 name   => $rcn,  # resource name
                 value  => $rcv,  # actually, the default resource value
                 widget => $cmd,  # not really the widget name
                 args   => undef, # the arguments for the widget   
                 set    => 0,     # whether it has been modified by a set()
         };
     }

     $Dict{$rcn} = $tokens;   # hash with parsed line indexed by resource name


  }  # end LINE

# All done with file
# ------------------
  close(FILE);

# Record results into object
# --------------------------
  $self->{name}  = $RCname;
  $self->{title} = $RCtitle;
  $self->{Line}  = \@Line;
  $self->{Help}  = \@Help;
  $self->{Hist}  = \@Hist;
  $self->{Ui}    = \@UI; 
  $self->{Dict}  = \%Dict; 

# Debugging output
# ----------------
  $self->dump() if ( $debug );

}

#..........................................................................
sub _parseLine {                 # parse a resource line
    my $self = shift;
    my $line = shift;

}

#..........................................................................
sub dump {                                   # output, mostly for debugging

  my $self = shift;

# Convenience counters
# --------------------
  my $nLine = @{$self->{Line}};
  my $nHelp = @{$self->{Help}};
  my $nHist = @{$self->{Hist}};
  my $nUi   = @{$self->{Ui}};

# Debugging information, if requested
# -----------------------------------
  print "\n" ;
  print "  NAME: ", $self->{name}, "\n";
  print " TITLE: ", $self->{title},"\n";
  for ( $i=0; $i < $nHelp; $i++ ) { 
      print "  HELP: ",$self->{Help}[$i],"\n"; }
  for ( $i=0; $i < $nHist; $i++ ) { 
      print "  HIST: ",$self->{Hist}[$i],"\n"; }
  for ( $i=0; $i < $nUi;   $i++ ) {
      $ui = $self->{Ui}->[$i]; 
      print "\n";
      print " LABEL: |$ui->{name}| ($ui->{index})\n";
      print " VALUE: |$ui->{value}|\n";
      print "WIDGET: |$ui->{widget}|\n";
      print "  ARGS: |$ui->{args}|\n"; 
  }
  
}

#..........................................................................

sub write {                         # writes out resource file

    my $self = shift;
    my $argv = shift;

    my $file  = $argv->{file}; # outputfile name
    my $xenv  = $argv->{xenv}; # expand environment variables

    my $rLine = $self->{Line};
    my $nLine = @{$rLine};

    $file = '-' unless($file);

# Commit changes unless told not to
# ---------------------------------
  $self->commit() unless ( $argv->{NoCommit} );

# Open
# ----
  if ( $file eq "-" ) {
       open(FILE,">&STDOUT");
  } else {
       open(FILE,">$file") or confess "cannot open $file";
  }

# Write
# -----
  if ( $xenv ) { 
      for ( my $i=0; $i < $nLine; $i++ ) { 
          $evalline = $rLine->[$i];
	  eval "\$eLine = \"$evalline\"" ;   # expand environmnet
	  printf FILE "%s\n", "$eLine";  }  
  } else {
    for ( my $i=0; $i < $nLine; $i++ ) { 
    	printf FILE "%s\n", $rLine->[$i];  
    }
  }

# Close
# -----
  close(FILE);

}

#.........................................................................
sub commit {    # commit changes made to UI resources

    my $self = shift;
    my $argv = shift;

    my $Line = $self->{Line};
    my $Dict = $self->{Dict};

    foreach $key ( keys %{$Dict} ) {
        if ( $Dict->{$key}->{'set'} ) {
	    $j       = $Dict->{$key}->{'index' };
	    $name    = $Dict->{$key}->{'name'  };
	    $value   = $Dict->{$key}->{'value' };
	    $widget  = $Dict->{$key}->{'widget'};
	    $args    = $Dict->{$key}->{'args'  };
	    $Line->[$j] = $name . " " . $value . "   # " . 
                          $widget . " " . $args;
#           print $self->{'file'} . " <$key> <$value> NEW LINE: " . $Line->[$j] . "\n";
	}
    }

}

#.........................................................................
sub set {    # update value of an UI resource
    my $self = shift;
    my $name  = shift;
    my $value = shift;
    my $token;
    return undef unless ( $token = $self->{Dict}->{$name} );
    $token->{'set'} = 1;  # so that we now to commit it
    return $token->{'value'} = $value;
}

#.........................................................................
sub get {    # update value of an UI resource
    my $self = shift;
    my $name  = shift;
    $value = $self->{Dict}->{$name}->{value};
    return $value;
}

1;




