#!/usr/bin/env perl

#--------------------------------------------------
#
# Purpose: to summarize procedural timings in DAS
#
# Usage:
#
#  zeit_summary.pl [options] zeit.log
#
# !REVISION HISTORY:
#
#   24Feb2017 Todling  Initial code
#
# !TO DO:
#
#   1) fix time diff calculation for when date changes within
#      procedure under consideration, e.g., GSI starts at 23:45
#      and ends 0:15 of next day.
#   2) ability to summarize results of multiple zeit files (accummulation)
#   3) pair-up entries - to allow for handling of non-sequential zeit calls
#--------------------------------------------------
use Env;                 # make env vars readily available
use FindBin;             # so we can find where this script resides
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Shell qw(cat rm);    # cat and rm commands
use Time::Local;         # time functions

use GD::Graph::bars;
use GD::Graph::Data;
use GD::Text;

use Data::Dumper;

GetOptions ( "o=s"  => \$figname,
             "fzeit",
             "t");

 my @procs;  # procedures
 my @times;  # timings (min) 
 my @percst; # percentange of total times

 my @all_data;

# Defaults and command line parse
  init();

# Read in data
  @all_data  = [];
  $ii = 0;
  foreach $fn ( @fnames ) {
     print "handling $fn \n";
     if ($fzeit) {
        getdata("$fn");
        fortran_zeit_table();
     } else {
        script_zeit_table("$fn");
     }

# Determine type of statistic to produce
     if ( $this_type ) {
        @data = @percst;
        $ylab = 'Time (%)';
     } else {
        @data = @times;
        $ylab = 'Time (min)';
     }

     $all_data[$ii] = [ @data ]; $ii = $ii + 1;

  }

# Make plot
  makeplot();

# Wrap up
  if ($rc==0) {
     print "$0: sucessfully completed.\n\n";
     exit(0);
  } else {
     print "$0: failed to collect diag files\n\n";
     exit(1);
  }

#...................................................
sub init{

   @fnames = ();

   if ( $#ARGV  <  0 ) {
     print STDERR " Missing arguments; see usage: \n";
     usage();
   } else {              # required command line args
     $ic=0;
     while ( $ic <= $#ARGV ) {
        $fn = $ARGV[$ic];
        push  (@fnames , $fn );
        $ic = $ic + 1;
     }
   }
   $rc = 0;

   $figname = 'Zeit.png' unless defined $figname; 
  
   $this_type = 1;
   if ($opt_t) {
      $this_type = 0;
   }

   $fzeit = 0;
   if ( $opt_fzeit ) {$fzeit = 1};

}
#...................................................
sub getdata{

my ($fname) = @_;
if ( ! -e "$fname" ) {die "Cannot find file: $fname "};

@lines = (`grep -n ' .zeit.  ' $fname`);
my @words = split /:/, $lines[0];
$lb = "@words[0]\n" ;
$lb = $lb + 1;
@lines = (`grep -n ' GSIsa_Write ' $fname`);
my @words = split /:/, $lines[0];
$le = "@words[0]\n" ;

if ( -e ".zeit.tmp.file" ) {rm(".zeit.tmp.file")};
$cmd = "awk 'NR==$lb, NR==$le' $fname > .zeit.tmp.file";
system($cmd);

} # init

#...................................................
sub fortran_zeit_table {

    my $file = ".zeit.tmp.file";
    open(FILE, $file) or die "Cannot open file: $!";

    $nl=0; $total=0;
    @procs = (); @times = ();
    while (my $line = <FILE>) {

#       unless ($line =~ m/^\d+\s+/) {next;}   
        chomp  $line;
#           null    counts period  NET    m:s      %   scope   m:s   %      [procedure]
        my ($none , $nc  , $per  , $net,  $ms, $perc, $scope, $ms2 , $perc2, $proc) = split(/\s+/, $line);
        ($var1, $var2) = split(/:/,$ms);
        $time =  $var1 + $var2/60; # time in min
        $total = $total + $time;
        push  (@procs , $proc );
        push  (@times , $time );
        
        $nl = $nl + 1;
    }

    close(FILE);

#   calc percentage time
    @percst = ();
    foreach $t (@times) {
       $tt = 100 * $t / $total;
       push  (@percst , $tt );
    }

}
#...................................................
sub script_zeit_table {

    my ($file) = @_;

    open(FILE, $file) or die "Cannot open file: $!";

    @procs = (); @times = (); @output = ();
    $dominant = 1;
    while (my $line = <FILE>) {

#       unless ($line =~ m/^\d+\s+/) {next;}   
        chomp  $line;
#           zeit    proc     Date      date    Time   hh   min   sec    rest
        my ($zeit , $proc  , $name  , $date,  $name, $hh, $min, $sec , $rest) = split(/\s+/, $line);
        if ( $dominant ) {
           if ( $proc =~ "GSIsa" | $proc =~ "gsidiags" | $proc =~ "AnaToGcm" | $proc =~ "gcm" | $proc =~ "frepack" | $proc =~ "cnv2prs" ) {
              $time =  60*$hh + $min + $sec/60; # time in min
              push  (@procs , $proc );
              push  (@times , $time );
           }
        } else {
           if ( $proc ne "RunGcm" & $proc ne "PreAnaQC" & $proc ne "AnalysisRun" & $proc ne "SplitExecPost" & $proc ne "TagAndRecycle" & $proc ne "TagAndCopy" & $proc ne "gsiinfo" ) {
              $time =  60*$hh + $min + $sec/60; # time in min
              push  (@procs , $proc );
              push  (@times , $time );
           }
        }

    }

#   redefine times as unique values
    @btime = (); @etime = ();
    for (my $i=0; $i<=$#times+1; $i+=2) { push @btime, $times[$i] };
    for (my $i=1; $i<=$#times+1; $i+=2) { push @etime, $times[$i] };
    for (my $i=0; $i<=$#times;   $i+=2) { push @output,$procs[$i] }; @procs = @output;
    $nf = 0;
    @times = (); $total = 0;
    while ( $nf < $#btime) {
      $tt = $etime[$nf] - $btime[$nf];
      $total = $total + $tt;
      push  (@times , $tt );
      $nf = $nf + 1;
    }
    print "$fn  total time: $total \n";

#   calc percentage time
    @percst = ();
    foreach $t (@times) {
       $tt = 100 * $t / $total;
       push  (@percst , $tt );
    }

    close(FILE);
}
#...................................................
sub makeplot{

   print Dumper(\@all_data);
#  Plot data now
   my $data = GD::Graph::Data->new([ 
                          [ @procs ] ,
                            @all_data ,
               ] ) or die GD::Graph::Data->error;

   my $graph = GD::Graph::bars->new(800,600);

# $graph->set_legend(@cases);

#   $graph->set_x_label_font(gdMediumBoldFont, 24);
   my $font_dir  = '/usr/share/fonts/truetype';
#my $font_file = "$font_dir/bullpeni.ttf";
#my $font_file = "$font_dir/berylium.ttf";
#my $font_file = "$font_dir/nasaliza.ttf";
   my $font_file = "$font_dir/VeraMoBd.ttf";
 
   $graph->set_title_font($font_file, 20);
   $graph->set_x_label_font($font_file, 16);
   $graph->set_y_label_font($font_file, 16);
   $graph->set_x_axis_font($font_file, 11);
   $graph->set_y_axis_font($font_file, 11);
   $graph->set_legend_font($font_file, 9);

   $graph->set( 
       x_label         => 'Procedures',
       y_label         => $ylab,
       title           => 'GSI Timings',

       x_labels_vertical => 1,
       x_label_position  => 0.5,   # center x-label

#      dclrs             => ['#42dff4'],
 
       transparent     => 0,

       bar_spacing      => 1,
       bargroup_spacing => 10,
   ) or die $graph->error;


   $graph->plot($data) or die $graph->error;
 
   open(my $out, '>', $figname) or die "Cannot open '$figname' for write: $!";
   binmode $out;
   print $out $graph->gd->png;
   close $out;
}

#...................................................
sub usage {

   print <<"EOF";

NAME
     zeit_summary.pl - Display summary of timings from zeit programs
          
SYNOPSIS

     zeit_summary.pl [...options...] 
          
DESCRIPTION

OPTIONS

     -fzeit        handle zeit output from FORTRAN log files
                     (DEFAULT: handle zeit output from shell scripts)
     -o            output figure filename
     -t            type of output, percentage or time
                     (DEFAULT: percentage)

NECESSARY ENVIRONMENT

OPTIONAL ENVIRONMENT

AUTHOR

     Ricardo Todling (Ricardo.Todling\@nasa.gov), NASA/GSFC/GMAO
     Last modified: 24Feb2017      by: R. Todling


EOF

  exit(1)

}
