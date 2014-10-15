#!/usr/bin/perl
#
# Reads a Fortran source code with calls to 
#
#  MAPL_StateAddImportSpec
#  MAPL_StateAddExportSpec
# 
#  GEOS_StateAddImportSpec
#  GEOS_StateAddExportSpec
# 
# And creates a simple ASCII table with the variables defined.
#
# REVISION HISTORY:
# 03Oct1996  da Silva  First crack.
#
#........................................................................

use File::Basename;
use Getopt::Std;         # command line options
use Data::Dumper;

$Iam = "mapl_vlist";

# Parse command line
# ------------------
  getopts('hwslcfXIPF');
  usage() if $opt_h;
  if ( $opt_f ) {
    $opt_s = 1;
    $opt_l = 1;
    $opt_c = 1;
  }
  if ( $opt_F ) {
    $opt_X = 1;
    $opt_I = 1;
    $opt_P = 1;
  }
  $opt_s = 1 unless ( $opt_l || $opt_c );  # default
  $opt_X = 1 unless ( $opt_I || $opt_P );  # default

# Main loop over file
# -------------------
$pFlag = 0; # internal state (private)
$iFlag = 0; # import state
$xFlag = 0; # export state
$inBlock = 0; # whether inside a block

$nTable = 1; # Table number for wiki output

LINE: while (<>) {

    next LINE if (/^\#/ || /^\!/ );  # skip comment lines

    $Line    = $_;
    @tokens  = split;
    $KEYWORD = uc $tokens[0];

#   Look for Grid Component Name
#   ----------------------------
    if ( "$KEYWORD" =~ /^MODULE/ ) {
	$gc = trimit($tokens[1]);
        $gc =~ s/GridComp.*$//i;           # keep it simple
        $gc =~ s/^MAPL_//i;
        $gc =~ s/^GEOS_//i;
        $gc =~ s/_$//;

        next LINE;
    }

#   Start of Import/Export/Internal block
#   -------------------------------------
    if ( "$KEYWORD" =~ /^CALL/ ) {
	$_ = uc $tokens[1]; # subroutine name
	if    ( /^MAPL_ADDIMPORTSPEC/ )     { $iFlag = 1; }
        elsif ( /^MAPL_ADDEXPORTSPEC/ )     { $xFlag = 1; }
        elsif ( /^MAPL_ADDINTERNALSPEC/ )   { $pFlag = 1; }
	elsif ( /^GEOS_STATEADDIMPORTSPEC/ )     { $iFlag = 1; }
        elsif ( /^GEOS_STATEADDEXPORTSPEC/ )     { $xFlag = 1; }
        elsif ( /^GEOS_STATEADDINTERNALSPEC/ )   { $pFlag = 1; }
        $inBlock = $iFlag + $xFlag + $pFlag;
        next LINE;
    }

    next LINE unless ( $inBlock );

#   End of Block?
#   -------------
    $lword = pop @tokens;
    if ( "$lword" =~ /\)$/ ) { # Line must end with ")"

        $Long  = "$long"  . "gc";
        $Short = "$short" . "gc";

        $Gc = "$gc" . "$short";

#       Save variable info
#       ------------------
        my $vinfo = [ $gc, $short, $long, $units, $dims, $vloc ];
###     print qq( $gc | $short | $long | $units | $dims | $vloc ) . "\n";
	if ( $xFlag ) {
	    $xLONG{$Long}   = $vinfo;
	    $xSHORT{$Short} = $vinfo;
	    $xGC{$Gc}       = $vinfo;
	    $xFlag = 0;
        } elsif ( $iFlag ) {
	    $iLONG{$Long}  = $vinfo;
	    $iSHORT{$Short} = $vinfo;
	    $iFlag = 0;
	    $iGC{$Gc}       = $vinfo;
        } elsif ( $pFlag ) {
	    $pLONG{$Long}  = $vinfo;
	    $pSHORT{$Short} = $vinfo;
	    $pGC{$Gc}       = $vinfo;
	    $pFlag = 0;
        }

	$pFlag = 0; # internal state (private)
	$iFlag = 0; # import state
	$xFlag = 0; # export state
	$inBlock = 0; # whether inside a block

        next LINE;

    } # end of block

#   We are in block, so try to save attributes
#   ------------------------------------------
    @tokens = split '=', $Line;    
    $key = uc $tokens[0];
    $key = trimit($key);
    $val = trimit($tokens[1]);

#   print "############# <$key> <$val>\n";
    
    $long  = "$val" if ( $key =~ /^LONG_NAME/ );
    $short = "$val" if ( $key =~ /^SHORT_NAME/ );
    $units = "$val" if ( $key =~ /^UNITS/ );
    $dims  = "$val" if ( $key =~ /^DIMS/ );
    $vloc  = "$val" if ( $key =~ /^VLOCATION/ );


} # main loop

if ( $opt_X ) {

  $State = Export; $STATE = uc $State;
  if ( $opt_w ) { print "\n== $State State ==\n"; }
  else          { print "\n\n                                       $STATE State ";       }

  write_vinfo("Sorted by SHORT variable name",\%xSHORT)
      if $opt_s;
  write_vinfo("Sorted by LONG variable name",\%xLONG)
      if $opt_l;
  write_vinfo("Sorted by COMPONENT name",\%xGC)
      if $opt_c;
}

if ( $opt_P ) {

  $State = Internal; $STATE = uc $State;
  if ( $opt_w ) { print "\n== $State State ==\n"; }
  else          { print "\n\n                                       $STATE State ";       }

  write_vinfo("Sorted by SHORT variable name",\%pSHORT)
      if $opt_s;
  write_vinfo("Sorted by LONG variable name)",\%pLONG)
      if $opt_l;
  write_vinfo("Sorted by COMPONENT name)",\%pGC)
      if $opt_c;

}

if ( $opt_I ) {

  $State = Import; $STATE = uc $State;
  if ( $opt_w ) { print "\n== $State State ==\n"; }
  else          { print "\n\n                                      $STATE State ";       }

  write_vinfo("Sorted by SHORT variable name",\%iSHORT)
      if $opt_s;
  write_vinfo("Sorted by LONG variable name",\%iLONG) 
      if $opt_l;
  write_vinfo("Sorted by COMPONENT name",\%iGC)
      if $opt_c;

}

   exit 0;

#.........................................................................
sub trimit {
    my $str = shift;
    $str =~ s/,.*$//g;
    $str =~ s/^\s*//g;
    $str =~ s/\s*$//g;
    $str =~ s/^\'//;
    $str =~ s/\'$//;
    return $str;
}

#.........................................................................
sub write_vinfo {

    if ( $opt_w ) { write_wiki_vinfo(@_); }
    else          { write_text_vinfo(@_); }

}

#.........................................................................
sub write_text_vinfo {   # ASCII version

    my $Title = shift;
    my $Info  = shift;

print "\n\n";
print "$STATE State ($Title)\n\n";

print "-----------|------------|------------|------| ---------------------------------------------------\n";   
print "  Name     | Component  |    Units   | Dim  |                     Long Name\n";
print "-----------|------------|------------|------| ---------------------------------------------------\n";   

    for $key ( sort keys %{$Info} ) {

        my $vinfo = $Info->{$key};
 
        my ( $gc, $short, $long, $units, $dims, $vloc ) = @{$vinfo};

        $gc = substr($gc,0,10);
 
        if (length($units) > 10) {
            $long = "$long" . " [$units]";    
	    $units = "   --->   ";
        }

        $dims =~ s/^MAPL_Dims//i;
        $dims =~ s/^GEOS_Dims//i;

        $dims = "xy  " if ( "$dims" =~ /HorzOnly/ );
        $dims = "  z " if ( "$dims" =~ /VertOnly/ );
        $dims = "xyz " if ( "$dims" =~ /HorzVert/ );
        $dims = "tile" if ( "$dims" =~ /TileOnly/ );

        $long =~ s/_/ /g;
        $long = ucfirst $long;


#        print "KEY = <$key>\n";
   
#	print qq( $gc | $short | $units | \n );
 
        if ( length($short) > 10 ) {   
        printf "%s \n", 
  	        $short;
        printf "%-10s | %-10s | %-10s | %4s | %s\n", 
  	        " ", $gc, $units, $dims, $long;
        } else {
        printf "%-10s | %-10s | %-10s | %4s | %s\n", 
  	        $short, $gc, $units, $dims, $long;
        }

    }

print "-----------|------------|------------|------| ---------------------------------------------------\n";   

}

#.........................................................................
sub write_wiki_vinfo {   # MediaWiki version

    my $Title = shift;
    my $Info  = shift;

    my $title = lcfirst $Title;

    print <<EOF;

=== $Title ===

<center>

{| border="1"

|+ ''Table $nTable.'' List of GEOS-5 $State State variables $title

!  Name !! Component !! Units !! Dim  !! Long Name
EOF

    for $key ( sort keys %{$Info} ) {

        my $vinfo = $Info->{$key};
 
        my ( $gc, $short, $long, $units, $dims, $vloc ) = @{$vinfo};

        $gc = substr($gc,0,10);
 
#        if (length($units) > 10) {
#            $long = "$long" . " [$units]";    
#	    $units = "   --->   ";
#        }

        $dims =~ s/^MAPL_Dims//i;
        $dims =~ s/^GEOS_Dims//i;

        $dims = "xy  " if ( "$dims" =~ /HorzOnly/ );
        $dims = "  z " if ( "$dims" =~ /VertOnly/ );
        $dims = "xyz " if ( "$dims" =~ /HorzVert/ );
        $dims = "tile" if ( "$dims" =~ /TileOnly/ );

        $long =~ s/_/ /g;
        $long = ucfirst $long;


        print <<EOF;
\|-
\| $short \|\| $gc \|\| $units \|\| $dims \|\| $long
EOF

    }

        print <<EOF;
\|}
</center>

EOF

    $nTable++;

}


#.........................................................................

sub usage {

    $GetPointer = "$MAPL"."_GetPointer";
   print <<"EOF";

NAME
     mapl_vlist - lists Import/Export/Internal states from MAPL components
          
SYNOPSIS

     mapl_vlist.pl [OPTIONS]  Fortran_filenames
          
DESCRIPTION
     This utility reads a list of Fortran files defining MAPL components
     and lists the variables comprising the Import, Export and Internal
     States. Several lists can be produced, sorting the output by short
     names, long names, component namesm etc.

     The GEOS-5 coding style is implicitly assumed. Here is an example
     of a typical "add spec" code fragment:

     call MAPL_StateAddImportSpec(GC,                                  &
          LONG_NAME  = 'mass_fraction_of_cloud_ice_in_air',            &
          UNITS      = '1',                                            &
          SHORT_NAME = 'QI',                                           &
          DIMS       = MAPL_DimsHorzVert,                              &
          VLOCATION  = MAPL_VLocationCenter,                           &
          AVERAGING_INTERVAL = ACCUMINT,                               &
          REFRESH_INTERVAL   = MY_STEP,                                &
                                                            RC=STATUS  )
     
     Notice that arguments are entered one per continuation line. 
     The module name is derived from the Fortran "Module" keyword in
     the file, trimming out any trailing "_GridComp*" and leading
     "GEOS_". For example, the stament

           Module GEOS_SolarGridCompMod

     would be interpreted as the having "Solar" as the component name.
     Notice this is related but not exactly the Component name entering
     a MAPL_History resource file. 



EXAMPLE

     % cd ~/GEOSgcm/src
     % mapl_vlist.pl `find . -name '*.[PFf]*'`

OPTIONS

 -h   Print this page
 -w   Produce MediaWiki output; default is ASCII text

 -s   Sort by SHORT variable name (default)
 -l   Sort by LONG variable name 
 -c   Sort by COMPONENT name 
 -f   Full mode, writes all sorting options to stdout 

 -X   Show EXPORT   state only (default) 
 -I   SHow IMPORT   state only
 -P   Show INTERNAL state only
 -F   Show IMPORT/EXPORT and INTERNAL states

TO DO

  Produce Wiki/HTML/Latex friendly output.

SEE ALSO
     The MAPL User's Guide.

AUTHOR
     Arlindo da Silva, NASA/GSFC.

EOF

  exit(1)

 }
