package Manipulate_time;
require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(z_time inc_time dec_time date8 year4 date6 mmm_yr now token_resolve
             jdoy zpad cpad tick num_days_in_month get_hours);

use FindBin;
use lib "$FindBin::Bin";

# global variables
#-----------------
my $SECS_PER_MIN  = 60;
my $MINS_PER_HOUR = 60;
my $HOURS_PER_DAY = 24;
my $MONTHS_PER_YR = 12;

my $SECS_PER_HOUR = $SECS_PER_MIN  * $MINS_PER_HOUR;
my $SECS_PER_DAY  = $SECS_PER_HOUR * $HOURS_PER_DAY;
my $MINS_PER_DAY  = $MINS_PER_HOUR * $HOURS_PER_DAY;

# generic MONTH and YEAR definitions
#-----------------------------------
my $DAYS_PER_MONTH = 30;
my $DAYS_PER_YEAR = 365;

#.........................................
sub z_time {

# z_time takes no arguments.

# Local variables

   my $sec = 0;
   my $min = 0;
   my $hour = 0;
   my $mday = 0;
   my $mon = 0;
   my $year = 0;
   my $wday = 0;
   my $yday = 0;
   my $isdst = 0;
   my $cur_date = 0;
   my $cur_time = 0;

# Get the GMT time list.

# All values that come out of gtime are numeric.
# $sec = current seconds. (Range = [0..59])
# $min = current minutes. (Range = [0..59])
# $hour = current GMT hour. (Range = [0..23])
# $mday = current day of month. (Range = [1..31] of course it depends.)
#         Note that $mday starts at 1, not 0!
# $mon = current month. (Range = [0..11])  But $mon does start at 0!
# $year = Number of years since 1900, e.g., 97 is 1997; 101 is 2001. (Range: >= 0.)
# $wday = Day of week with Sunday as 0.  (Range = [0..6])
# $yday = Day of year.  (Range = [0..355] or 356, depending)
# isdst = "Is daylight saving time".  1 if it is.

   ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time);

# Set date in YYYYMMDD format and time in HHMMSS format.

   $cur_date = ($year+1900)*10000 + ($mon+1)*100 + $mday;
   $cur_time = $hour*10000 + $min*100 + $sec;

   return ( $cur_date, $cur_time );

}


#.........................................
sub inc_time {

# Time calculations are best done in integer.

   use integer;

# Contains the julian_day() and inverse_julian_day() functions.

   use Time::JulianDay;

# This subroutine requires 4 arguments.

   if ( $#_ + 1 < 4 ) {
      print "(inc_time) usage: inc_time( orig_date, orig_time, inc_date, inc_time ) \n";
      return, ( 0, 0 );
   }

# Get arguments.

   my $orig_date = $_[0];
   my $orig_time = $_[1];
   my $inc_date  = $_[2];
   my $inc_time  = $_[3];

# Get original year, month and day.

   $yyyy_orig = $orig_date/10000;
   $mm_orig = ($orig_date/100) % 100;
   $dd_orig = $orig_date % 100;

# Get original hour, minute and second.

   $hh_orig = $orig_time/10000;
   $uu_orig = ($orig_time/100) % 100;
   $ss_orig = $orig_time % 100;

# Ensure year is a 4 digit year.  (If 2 digit, then assumes that
# if yy is <= 49 then year is 20yy, else year is 19yy.  Date returned
# will use the 2 digit year.)

   $using_Y2 = 0;

   if ( $yyyy_orig <= 99 ) {
      $yyyy_orig = year4( $yyyy_orig );
      $using_Y2 = 1;
   }

# Get number of years, months and days to increment.

   $yy_inc = $inc_date/10000;
   $mm_inc = ($inc_date/100) % 100;
   $dd_inc = $inc_date % 100;

# Get number of hours, minutes and seconds to increment.

   $hh_inc = $inc_time/10000;
   $uu_inc = ($inc_time/100) % 100;
   $ss_inc = $inc_time % 100;

# Get day number of original date since 1 Jan 1970.

   $jdayn = julian_day( $yyyy_orig, $mm_orig, $dd_orig );

# Add times

   $ss_new=$ss_orig+$ss_inc;
   if ( $ss_new >= 60 ) {
      $uu_add=$ss_new/60;
      $ss_new=$ss_new % 60;
   } else {
      $uu_add=0;
   }

   $uu_new=$uu_orig+$uu_inc+$uu_add;
   if ( $uu_new >= 60 ) {
      $hh_add=$uu_new/60;
      $uu_new=$uu_new % 60;
   } else {
      $hh_add=0;
   }

   $hh_new=$hh_orig+$hh_inc+$hh_add;
   if ( $hh_new >= 24 ) {
      $dd_add=$hh_new/24;
      $hh_new=$hh_new % 24;
   } else {
      $dd_add=0;
   }

# Add number of incremented days and those to be added
# because of the time addition.

   $jdayn_new=$jdayn+$dd_inc+$dd_add;

# Find new calendar date

   ($yyyy_new, $mm_new, $dd_new) = inverse_julian_day($jdayn_new);

# Add in month and year increments.

   $mm_new=$mm_new+$mm_inc;

   $yy_add=0;
   while ( $mm_new > 12 ) {
      $mm_new=$mm_new-12;
      $yy_add=$yy_add+1;
   }

   $yyyy_new=$yyyy_new+$yy_inc+$yy_add;

# Create new ymd and hms.

   $ymd_new=$yyyy_new*10000+$mm_new*100+$dd_new;
   $hms_new=$hh_new*10000+$uu_new*100+$ss_new;

   if ( $using_Y2 ) { $ymd_new = date6( $ymd_new ); }

   return ( $ymd_new, $hms_new );

}

#.........................................
sub dec_time {

# Time calculations are best done in integer.

   use integer;

# Contains the julian_day() and inverse_julian_day() functions.

   use Time::JulianDay;

# This subroutine requires 4 arguments.

   if ( $#_ + 1 < 4 ) {
      print "(dec_time) usage: dec_time( orig_date, orig_time, dec_date, dec_time ) \n";
      return, ( 0, 0 );
   }

# Get arguments.

   my $orig_date = $_[0];
   my $orig_time = $_[1];
   my $dec_date  = $_[2];
   my $dec_time  = $_[3];

# Get original year, month and day.

   $yyyy_orig = $orig_date/10000;
   $mm_orig = ($orig_date/100) % 100;
   $dd_orig = $orig_date % 100;

# Get original hour, minute and second.

   $hh_orig = $orig_time/10000;
   $uu_orig = ($orig_time/100) % 100;
   $ss_orig = $orig_time % 100;

# Ensure year is a 4 digit year.  (If 2 digit, then assumes that
# if yy is <= 49 then year is 20yy, else year is 19yy.  Date returned
# will use the 2 digit year.)

   $using_Y2 = 0;

   if ( $yyyy_orig <= 99 ) {
      $yyyy_orig = year4( $yyyy_orig );
      $using_Y2 = 1;
   }

# Get number of years, months and days to decrement.

   $yy_dec = -( $dec_date/10000 );
   $mm_dec = -( ($dec_date/100) % 100 );
   $dd_dec = -( $dec_date % 100 );

# Get number of hours, minutes and seconds to decrement.

   $hh_dec = -( $dec_time/10000 );
   $uu_dec = -( ($dec_time/100) % 100 );
   $ss_dec = -( $dec_time % 100 );

# Get day number of original date since 1 Jan 1970.

   $jdayn = julian_day( $yyyy_orig, $mm_orig, $dd_orig );

# Subtract times

   $ss_new=$ss_orig+$ss_dec;
   if ( $ss_new < 0 ) {
      $uu_sub=($ss_new+1)/60-1;
      $ss_new=$ss_new+60*abs($uu_sub);
   } else {
      $uu_sub=0;
   }

   $uu_new=$uu_orig+$uu_dec+$uu_sub;
   if ( $uu_new < 0 ) {
      $hh_sub=($uu_new+1)/60-1;
      $uu_new=$uu_new+60*abs($hh_sub);
   } else {
      $hh_sub=0;
   }

   $hh_new=$hh_orig+$hh_dec+$hh_sub;
   if ( $hh_new < 0 ) {
      $dd_sub=($hh_new+1)/24-1;
      $hh_new=$hh_new+24*abs($dd_sub);
   } else {
      $dd_sub=0;
   }

# Subtract number of decremented days and those to be subtracted
# off because of the time subtraction.

   $jdayn_new=$jdayn+$dd_dec+$dd_sub;

# Find new calendar date

   ($yyyy_new, $mm_new, $dd_new) = inverse_julian_day($jdayn_new);

# Add in month and year increments.

   $mm_new=$mm_new+$mm_dec;

   $yy_sub=0;
   while ( $mm_new < 0 ) {
      $mm_new=$mm_new+12;
      $yy_sub=$yy_sub-1;
   }

   $yyyy_new=$yyyy_new+$yy_dec+$yy_sub;

# Create new ymd and hms.

   $ymd_new=$yyyy_new*10000+$mm_new*100+$dd_new;
   $hms_new=$hh_new*10000+$uu_new*100+$ss_new;

   if ( $using_Y2 ) { $ymd_new = date6( $ymd_new ); }

   return ( $ymd_new, $hms_new );

}

#.........................................
sub date8 {

# Time calculations are best done in integer.

   use integer;

# Takes one argument, which is taken to be a date in YYMMDD
# format (2 digit year), and returns the date in YYYYMMDD format.
# The 2 digit year is assumed to be a year between 1950 and 2049.

   my $yymd = $_[0];

   $yy = $yymd/10000;

   if ( $yy >= 0 && $yy <= 99 ) {
      $md = $yymd % 10000;
      $yyyymd = year4($yy)*10000+$md;
   } else {
      $yyyymd = $yymd;
   }

   return $yyyymd;

}

#.........................................
sub year4 {

# Time calculations are best done in integer.

   use integer;

# Takes one argument, which if it is a value between 0 and 99, assumes
# it to be a 2 digit year between 1950 and 2049.  Returns the actual year.

# Thus if yy is in the range [50..99], year4 returns [1950..1999].
# If yy is in the range [00..49], year 4 returns [2000..2049].

   my $yy = $_[0];

   if ( $yy >= 0 && $yy <= 49 ) {
      return 2000+$yy;
   } elsif ( $yy >= 50 && $yy <= 99 ) {
      return 1900+$yy;
   } else {
      return $yy;
   }

}

#.........................................
sub date6 {

# Time calculations are best done in integer.

   use integer;

# Takes one argument, which is taken to be a date in YYYYMMDD
# format (4 digit year), and returns the date in YYMMDD format.
# If the year is between 1950 and 1999, then 50-99 is returned
# as the year.  If the year is between 2000 and 2049, then 00-49
# is returned as the year.  Any 4 digit years outside this range
# returned a value of -1.

   my $yyyymd = $_[0];

   $yyyy = $yyyymd/10000;

   if ( $yyyy >= 1950 && $yyyy <= 2049 ) {
      $yymd = $yyyymd % 1000000;
   } else {
      $yymd = -1;
   }

   return $yymd;

}

#.........................................
sub mmm_yr {

# Time calculations are best done in integer.

   use integer;

# Takes one or two arguments.

# If two arguments:
#
#    $tag = mmm_yr( $yy, $mm );
#
# where
#
#    $yy - the year as an integer (either 2 digit or 4 digit).
#    $mm - the month as an integer (1-12).
#
# If one argument:
#
#    $tag = mmm_yr( $yymm );
#
# where
#
#    $yymm = the year and month together, with the year as
#            a 2 or 4 digit integer and the month as a 2 digit
#            integer.  (E.g., 9801 or 199801)
#
# mmm_yr returns a string wich is the 3 character name of the month
# prefixing the year (e.g., "jan98" or "jan1998")

   my $mm = 0;
   my $yy = 0;

   if ( $#_ == 0 ) {
      $yy = $_[0]/100;
      $mm = $_[0] % 100;
   } elsif ( $#_ == 1 ) {
      $yy = $_[0];
      $mm = $_[1];
   } else {
      print "(mmm_yr) usage: mmm_yr( yy, mm ) or mmm_yr( yymm )\n";
      return( "" );
   }

   @mnth_list = qw(jan feb mar apr may jun jul aug sep oct nov dec);

   return( "$mnth_list[$mm-1]$yy" );

}

#.........................................
#
# token_resolve takes a tokenized filename, and optionally an eight digit date and a
# six digit time and replaces the tokens with the correct values. If no date and time
# are supplied, the replacement is done with Unix wildcards. If only a time is needed
# you must supply a zero for the date as a placeholder.
#
# Legal tokens are based on the GrADS tokens:
#
#     %y2   2 digit year
#     %y4   4 digit year
#     %m2   2 digit month (leading zero if needed)
#     %mc   3 character month abbreviation (jan, feb etc.)
#     %MC   3 character month abbreviation (JAN, FEB etc.)
#     %d2   2 digit day (leading zero if needed)
#     %j3   3 digit Julian day of year (001-366)
#     %h2   2 digit hour
#     %n2   2 digit minute (leading zero if needed)
#     %s2   2 digit second
#     %s    any string - returns "*"
#     %c    any single character - returns "?"
#     %n    any single digit - returns "[0-9]"
#

sub token_resolve {
    undef $time;
    my ( $file, $date, $time ) = @_;
    my @mnth_list = qw(jan feb mar apr may jun jul aug sep oct nov dec);
    my @Mnth_list = qw(JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC);
    my     $yyyy  = "[0-9][0-9][0-9][0-9]";
    my     $yy    = "[0-9][0-9]";
    my     $mm    = "[0-1][0-9]";
    my     $dd    = "[0-3][0-9]";
    my     $jd    = "[0-3][0-9][0-9]";
    my     $hh    = "[0-2][0-9]";
    my     $MM    = "[0-5][0-9]";
    my     $SS    = "[0-5][0-9]";
    my     $mc    = "???";
    my     $MC    = "???";

    if ( defined($date) ){
         my @ddigits = split //, $date;
         $yyyy  = substr($date,0,4) if ( defined($ddigits[3]) );
           $yy  = substr($date,2,2) if ( defined($ddigits[3]) );
           $mm  = substr($date,4,2) if ( defined($ddigits[5]) );
           $dd  = substr($date,6,2) if ( defined($ddigits[7]) );
           $mc  = $mnth_list[$mm-1] if ( defined($ddigits[5]) );
           $MC  = $Mnth_list[$mm-1] if ( defined($ddigits[5]) );
           $jd  = jdoy($yyyy,$mm,$dd) if ( defined($ddigits[7]) );
    }
    if ( defined($time) ){
       $time = "000000" if ( $time eq "0" );
       my @tdigits = split //, $time;
       $hh = substr($time,0,2) if ( defined($tdigits[1]) );
       $MM = substr($time,2,2) if ( defined($tdigits[3]) );
       $SS = substr($time,4,2) if ( defined($tdigits[5]) );
    }
    ($file = $file) =~ s/%h2/$hh/g;
    ($file = $file) =~ s/%n2/$MM/g;
    ($file = $file) =~ s/%s2/$SS/g;

    ($file = $file) =~ s/%y4/$yyyy/g;
    ($file = $file) =~ s/%y2/$yy/g;
    ($file = $file) =~ s/%m2/$mm/g;
    ($file = $file) =~ s/%mc/$mc/g;
    ($file = $file) =~ s/%MC/$MC/g;
    ($file = $file) =~ s/%d2/$dd/g;

    ($file = $file) =~ s/%j3/$jd/g;
    ($file = $file) =~ s/%c/?/g;
    ($file = $file) =~ s/%n/[0-9]/g;
    ($file = $file) =~ s/%s/*/g;
    return ($file);
}

#.........................................
#
# jdoy takes either a 4-digit year, 2-digit month and 2-digit day or an
# 8-digit date and returns a 3-digit julian day of year
#
sub jdoy {
        my($year,$mon,$day)=@_;
        if ((! defined($mon) ) && (! defined($day) )) {
           $mon   = substr($year,4,2);
           $day   = substr($year,6,2);
           $year  = substr($year,0,4);
        }
        @jday=(0,0,31,59,90,120,151,181,212,243,273,304,334);
        $julian = $jday[$mon] + $day + &leap_check($year,$mon,$day);
return(cpad ($julian));

}

#.........................................
# leap_check

sub leap_check {
    my($year,$month,$day) = @_;

    if ($year % 4) {
          return(0);
    }
    if (!($year % 100)) {
          if ($year % 400) {
              return(0);
          }
    }
    if ($month < 3) {
         return(0);
    } else {
         return(1);
    }
}

#.........................................
# now subroutine
# returns unique 14 digit numeric string based on the current date and time
# it creates a lockfile in $ENV{HOME} to prevent two 'now' processes from running
# simultaneously and sleeps a random number of seconds before retrying if a lock
# is encountered
#

sub now{
  use Env;
  srand($$);
  $counter = 0;
  $timer = int(rand(60));
  sleep($timer);
  $LOCK = "$ENV{HOME}/now.LOCK";
  while ( ( -e "$LOCK" ) && ( $counter < 5 ) ) {
       print "waiting for $LOCK to be deleted\n";
       $timer = int(rand(20));
       sleep($timer);
       $counter ++;
  }
  if ( $counter >= 5 ){
         $now = -999;
  }else{
         $return=system("touch $LOCK");
         $status = $? >> 8;
         if ( $status ) {
              print "\$status = $status\n";
              print "\$return = $return\n";
              $now = -999;
         }else{
              ($sec,$min,$hour,$mday,$mon,$year,$null,$null,$null)=gmtime(time);
              unlink "$LOCK";
              $year= $year+1900;
              $mon=  zpad($mon+1);
              $mday= zpad($mday);
              $hour= zpad($hour);
              $min=  zpad($min);
              $sec=  zpad($sec);
              $now=  join '',$year,$mon,$mday,$hour,$min,$sec;
         }
  }
  return($now);
}

#.........................................
#
# zpad subroutines
# add 1 leading zero to single digit integers
#

sub zpad{
  my $out;
  if ($_[0]<10){
      $out="0".$_[0];
     }
     else {$out=$_[0];
     }
  return($out);

}#end sub zpad

#.........................................
#
# cpad subroutine
# add 2 leading zeroes to single digit integers
#
sub cpad{
  my $out;
  if ($_[0]<100){
     $znum = $_[0];
     if ($_[0]<10){
         $znum = "0".$_[0];
     }
     $out="0"."$znum";
  }else{
     $out=$_[0];
  }
  return($out);

}#end sub cpad

#........................................................................
#
# Simple perl routine for ticking the clock.
#
# !Revision History
# 2000Apr04  daSilva  Initial version
# 2010Sep17  Stassi   Replaced Time::Local routines with sub add_seconds()
#                     so that script will work beyond 19Jan2038
# 2011Jan14  Stassi   Moved tick to package from stand-alone script
#........................................................................
sub tick {
    my ($numargs, $nymd, $nhms, $isecs);
    my ($iymd, $ihms, $hh, $mm, $ss);
    my ($yr, $mnth, $dy, $idays);

    $numargs = scalar @_;
    if ($numargs < 1 or $numargs > 4) {
        die ">> Error << incorrect number of calling arguments to tick;";
    }
    $nymd = shift @_;
    $nhms = shift @_;

    if ($numargs == 3) {
        $isecs = shift @_;
    }
    elsif ($numargs == 4) {
        $iymd = shift @_;
        $ihms = shift @_;

        ($hh, $mm, $ss) = parseIntTime($ihms);
        ($yr, $mnth, $dy) = parseIntTime($iymd);
        $idays = $dy + $mnth*$DAYS_PER_MONTH + $yr*$DAYS_PER_YEAR;

        $isecs = $ss;
        $isecs += $SECS_PER_MIN  * $mm;
        $isecs += $SECS_PER_HOUR * $hh;
        $isecs += $SECS_PER_DAY  * $idays;
    }
    $isecs = 86400 unless defined($isecs);
    $nhms = 0 unless $nhms;

    ($nymd, $nhms) = add_seconds($nymd, $nhms, $isecs);

    if ( $numargs == 1 ) { return $nymd          }
    else                 { return ($nymd, $nhms) }
}

#...................................................................
sub add_seconds {
    my ($nymd, $nhms, $isecs);
    my ($yr, $mnth, $dy, $hh, $mm, $ss);

    $nymd  = shift @_;
    $nhms  = shift @_;
    $isecs = shift @_;

    ($yr, $mnth, $dy) = parseIntTime($nymd);
    ($hh, $mm, $ss) = parseIntTime($nhms);

    # check validity of initial date/time
    #------------------------------------
    die "Error; invalid month ($mnth) in initial date: $nymd;"
        if $mnth < 1 or $mnth > 12;

    die "Error; invalid day ($dy) in initial date: $nymd;"
        if $dy < 1 or $dy > num_days_in_month($yr, $mnth);

    die "Error; invalid hour ($hh) in initial time: $nhms;"
        if $hh < 0 or $hh >= $HOURS_PER_DAY;

    die "Error; invalid minute ($mm) in initial time: $nhms;"
        if $mm < 0 or $mm >= $MINS_PER_HOUR;

    die "Error; invalid seconds ($ss) in initial time: $nhms;"
        if $ss < 0 or $ss >= $SECS_PER_MIN;

    # adjust seconds
    #---------------
    return ($nymd, $nhms) unless $isecs;
    $ss += $isecs;

    while ($ss <= -1*$SECS_PER_DAY)  { $ss += $SECS_PER_DAY;  $dy-- }
    while ($ss <= -1*$SECS_PER_HOUR) { $ss += $SECS_PER_HOUR; $hh-- }
    while ($ss < 0 )                 { $ss += $SECS_PER_MIN;  $mm-- }

    while ($ss >= $SECS_PER_DAY)     { $ss -= $SECS_PER_DAY;  $dy++ }
    while ($ss >= $SECS_PER_HOUR)    { $ss -= $SECS_PER_HOUR; $hh++ }
    while ($ss >= $SECS_PER_MIN)     { $ss -= $SECS_PER_MIN;  $mm++ }

    # adjust minutes
    #---------------
    while ($mm < 0)                  { $mm += $MINS_PER_HOUR; $hh-- }
    while ($mm >= $MINS_PER_HOUR)    { $mm -= $MINS_PER_HOUR; $hh++ }

    # adjust hours
    #-------------
    while ($hh < 0)                  { $hh += $HOURS_PER_DAY; $dy-- }
    while ($hh >= $HOURS_PER_DAY)    { $hh -= $HOURS_PER_DAY; $dy++ }

    # adjust days, months, and years
    #-------------------------------
    while ($dy < 1) {
        $mnth--;
        while ($mnth < 1) { $mnth += $MONTHS_PER_YR; $yr-- }
        $dy += num_days_in_month($yr, $mnth);
    }

    while ($dy > num_days_in_month($yr, $mnth)) {
        $dy -= num_days_in_month($yr, $mnth);
        $mnth++;
        while ($mnth > $MONTHS_PER_YR) { $mnth -= $MONTHS_PER_YR; $yr++ }
    }

    # zero-fill values where necessary for 2-digit and 4-digit representation
    #------------------------------------------------------------------------
    foreach (\$ss, \$mm, \$hh, \$dy, \$mnth) { $$_ = sprintf "%02i", $$_ }
    $yr = sprintf "%04i", $yr;

    return "$yr$mnth$dy", "$hh$mm$ss";
}

#...................................................................
sub parseIntTime {
    my ($intTime, $sign, $front, $next2, $last2);

    $intTime = shift @_;
    $intTime =~ s/^\s*|\s*$//g;   # remove leading/trailing blanks


    if ($intTime < 0) { $sign = -1; $intTime *= -1 } else { $sign = 1 }
    $intTime = sprintf "%06i", $intTime;

    $front = $sign * substr($intTime, 0, -4);
    $next2 = $sign * substr($intTime, -4, 2);
    $last2 = $sign * substr($intTime, -2, 2);

    return ($front, $next2, $last2);
}

#...................................................................
sub num_days_in_month {
    my ($yr, $mnth);
    my %lastday = ( "01" => 31, "02" => 28, "03" => 31, "04" => 30,
                    "05" => 31, "06" => 30, "07" => 31, "08" => 31,
                    "09" => 30, "10" => 31, "11" => 30, "12" => 31 );

    # input parameters
    #-----------------
    $yr   = shift @_;
    $mnth = shift @_;

    $yr   = sprintf "%04i", $yr;
    $mnth = sprintf "%02i", $mnth;

    # check input for correct format
    #-------------------------------
    die "Error. Incorrect year value: $yr;"    unless $yr   =~ /^\d{4}$/;
    die "Error. Incorrect month value: $mnth;" unless $mnth =~ /^\d{2}$/;
    die "Error. Incorrect month value: $mnth;" if $mnth < 1 or $mnth > 12;

    # special handling for month of February
    #---------------------------------------
    if ($mnth eq "02") {
        $lastday{"02"} = 29 if $yr%4==0 and ($yr%100!=0 or $yr%400==0);
    }

    return $lastday{$mnth};
}

#.........................................
#
# get_hours  - return file time list
#
#
sub get_hours{

    my ($type, $finalize, $syntime, $hm) = @_;
    my @hlist=();
    my $mm = "";

    if ($type eq "daily") { @hlist = "0000" }
    elsif ((( $type eq "inst3d") || ( $type eq "tavg3d")||( $type eq "tsyn3d") || ($type eq "inst6"))  && (! $finalize )){
        if ( "$hm" eq "m" ){ $mm = "00";}
        if ( "$syntime" eq "all" ) { @hlist = ( "00$mm","06$mm","12$mm","18$mm" ) }
        else                       { @hlist = ( "$syntime$mm" ) }
    }
    elsif ($type eq "inst2d") {
        if ( "$hm" eq "m" ){ $mm = "00" }
        if ( $finalize ) { @hlist = ( "21"."$mm") }
        else{
            if ("$syntime" eq "all")  { @hlist = ( "00$mm","03$mm","06$mm","09$mm",
                                                   "12$mm","15$mm","18$mm","21$mm" ) }
            if ("$syntime" eq "00" )  { @hlist = ( "00$mm" ) }
            if ("$syntime" eq "06" )  { @hlist = ( "03$mm","06$mm" ) }
            if ("$syntime" eq "12" )  { @hlist = ( "09$mm","12$mm" ) }
            if ("$syntime" eq "18" )  { @hlist = ( "15$mm","18$mm" ) }
        }
    }
    elsif ($type eq "inst3") {
        if ( "$hm" eq "m" ){ $mm = "00" }
        if ( $finalize ) { @hlist = ()  }
        else {
            if ("$syntime" eq "all")  { @hlist = ( "00$mm","03$mm","06$mm","09$mm",
                                                   "12$mm","15$mm","18$mm","21$mm" ) }
            if ("$syntime" eq "00" )  { @hlist = ( "00$mm","03$mm" ) }
            if ("$syntime" eq "06" )  { @hlist = ( "06$mm","09$mm" ) }
            if ("$syntime" eq "12" )  { @hlist = ( "12$mm","15$mm" ) }
            if ("$syntime" eq "18" )  { @hlist = ( "18$mm","21$mm" ) }
        }
    }
    elsif (($type eq "tavg2d")||($type eq "tavg3")) {
        if ( "$hm" eq "m" ){ $mm = "30" }
        if ( $finalize ) { @hlist = ( "22$mm" ) }
        else{
            if ( "$syntime" eq "all" ) { @hlist = ( "01$mm","04$mm","07$mm","10$mm",
                                                    "13$mm","16$mm","19$mm","22$mm" ) }
            if ( "$syntime" eq "00" )  { @hlist = ( "01$mm" ) }
            if ( "$syntime" eq "06" )  { @hlist = ( "04$mm","07$mm" ) }
            if ( "$syntime" eq "12" )  { @hlist = ( "10$mm","13$mm" ) }
            if ( "$syntime" eq "18" )  { @hlist = ( "16$mm","19$mm" ) }
        }
    }
    elsif ($type eq "tavg1"){
        if ("$hm" eq "m" ){ $mm = "30";}

        if ( $finalize ) { @hlist = ( "21$mm","22$mm","23$mm" ) }
        else{
            if ( "$syntime" eq "all" ) { @hlist = ( "00$mm","01$mm","02$mm","03$mm","04$mm","05$mm",
                                                    "06$mm","07$mm","08$mm","09$mm","10$mm","11$mm",
                                                    "12$mm","13$mm","14$mm","15$mm","16$mm","17$mm",
                                                    "18$mm","19$mm","20$mm","21$mm","22$mm","23$mm" ) }
            if ( "$syntime" eq "00" )  { @hlist = ( "00$mm","01$mm","02$mm" ) }
            if ( "$syntime" eq "06" )  { @hlist = ( "03$mm","04$mm","05$mm","06$mm","07$mm","08$mm" ) }
            if ( "$syntime" eq "12" )  { @hlist = ( "09$mm","10$mm","11$mm","12$mm","13$mm","14$mm" ) }
            if ( "$syntime" eq "18" )  { @hlist = ( "15$mm","16$mm","17$mm","18$mm","19$mm","20$mm" ) }
        }
    }
    elsif ($type eq "inst1"){
        if ("$hm" eq "m" ){ $mm = "00" }

        if ( $finalize ) { @hlist = ( "22$mm","23$mm" ) }
        else{
            if ( "$syntime" eq "all" ) { @hlist = ( "00$mm","01$mm","02$mm","03$mm","04$mm","05$mm",
                                                    "06$mm","07$mm","08$mm","09$mm","10$mm","11$mm",
                                                    "12$mm","13$mm","14$mm","15$mm","16$mm","17$mm",
                                                    "18$mm","19$mm","20$mm","21$mm","22$mm","23$mm" ) }
            if ( "$syntime" eq "00" )  { @hlist = ( "00$mm","01$mm","02$mm","03$mm" ) }
            if ( "$syntime" eq "06" )  { @hlist = ( "04$mm","05$mm","06$mm","07$mm","08$mm","09$mm" ) }
            if ( "$syntime" eq "12" )  { @hlist = ( "10$mm","11$mm","12$mm","13$mm","14$mm","15$mm" ) }
            if ( "$syntime" eq "18" )  { @hlist = ( "16$mm","17$mm","18$mm","19$mm","20$mm","21$mm" ) }
        }
    }
    return( @hlist );
}
1;
