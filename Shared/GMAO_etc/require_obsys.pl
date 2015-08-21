#!/usr/bin/env perl
#
# ./require_obsys.pl 19990801 000000 240000 5 ssmi_wentz_tpw
# Script to check the existence of observation files for a given
#   date range and observation file class from a resource file
# See usage() of this script for more information.
#
# Key Variables:
# $classSTRING: a runtime parameter, contains a string of classes
# @allclass: an array of classes extracted from the rcfile
#
# !REVISION HISTORY:
#
# 18Sep2007 Stassi   Reformatted required_obsys script
# 06Nov2007 Stassi   Added -stem option
# 29Jul2009 Stassi   Add code to filter out obsolete data sets
# 30Sep2013 Todling  Remove most of the obslete classes from obsys.rc
#=========================================================================
use strict;

# global variables
#-----------------
my @mday=(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
my ($rcfile, $show_all, $class_desc, $hard_check, $lookahead);
my ($quiet, $remote, $butstem, $stem, $verbose);
my ($notfound, $yfound, $today, $host);
my ($bymd, $bhms, $ihms, $nstep, $classSTRING, @allclass);

$ENV{PATH} = "./:" . $ENV{PATH};

my @obsolete_data = qw ( merra_sbuv_prep_bufr
                         ncep_osbuv_bufr
                         upabufr_llk
                         upabufr_val );


# main program
#-------------
{
    my(@obsclass, $oclass, @ymdh);

    # initialization
    #---------------
    &init();
    &classDesc() if $class_desc;

    # process observation class as "all" condition
    #---------------------------------------------
    if(lc($classSTRING) eq "all") { &do_All(); }

    # generate YYYYMMDDHH array @ymdh of all possible times
    #------------------------------------------------------
    @ymdh = &DateList();
    &msg("I","ymdh=@ymdh") if($verbose);

    # get records from $rcfile and then validate file existence
    #----------------------------------------------------------
    &msg("I",">> Processing require...");
    @obsclass = split(/,/, $classSTRING);
    foreach $oclass (@obsclass) {
        &msg("I","> obclass: $oclass");
        &get_rcd($oclass, @ymdh);
    }  

    # all done
    #---------
    close(LUN) || die "Couldn't close file: $rcfile $!\n";
    &msg("I","\nJob finished.");
    exit(0);   #0 as a success exit
}

#=========================================================================
#
# init: sets optional and required command line arguments and check
#       database resource file.
#
#=========================================================================
sub init {
    use Getopt::Long;

    my $rcfile_DFLT = "obsys.rc";    # default name for dataBase table
    my ($rc, $yyyy, $mm, $dd);
    my ($help, $lastday);

    # get options and command-line arguments
    #---------------------------------------
    $rc = GetOptions( "drc=s"      => \$rcfile,
                      "show_all"   => \$show_all,
                      "class_desc" => \$class_desc,
                      "hard_check" => \$hard_check,
                      "la=s"       => \$lookahead,
                      "rid=s"      => \$remote,
                      "butstem"    => \$butstem,
                      "stem=s"     => \$stem,
                      "h"          => \$help,
                      "v"          => \$verbose,
                      "q"          => \$quiet );
    usage() if ( $help || !$rc );

    ($verbose = 0) if ($quiet);  #quiet trumps verbose
    ($rcfile) or ($rcfile = $rcfile_DFLT);
    ($stem) or ($stem = "");
    ($butstem) or ($butstem = "");

    # set a couple of global variables
    #---------------------------------
    chomp($today = `date '+%Y%m%d'`);  #current date YYYYMMDD
    chomp($host = `hostname`);         #local hostName
    &msg("I","local host = $host") if($verbose);

    # open database (obsys) resource file
    #------------------------------------
    &msg("I","Database resource file = $rcfile") if ($verbose);
    open(LUN,"$rcfile") || die "Fail to open file: $rcfile $!\n";

    # check obclass first
    #--------------------
    @allclass = &get_classes_from_rcfile();
    @allclass = &remove_obsolete_data(@allclass) unless $show_all or $stem;

    # short-circuit initialization if only looking for class descriptions
    #--------------------------------------------------------------------
    return if $class_desc;

    # get runtime parameters
    #-----------------------
    usage() if ($#ARGV != 4);
    ($bymd, $bhms, $ihms, $nstep, $classSTRING) = @ARGV;

    # extract year (yyyy), month (mm), and day (dd)
    #--------------------------------------------
    if(length($bymd) != 8) { &msg("E","Beginning YYYYMMDD '$bymd' wrong") };
    $yyyy = substr($bymd,0,4);
    $mm   = substr($bymd,4,2);
    $dd   = substr($bymd,6,2);

    &set_mday($yyyy);
    $lastday = $mday[$mm-1];

    # validate $bymd
    #---------------
    if ($mm>12 || $mm<1)        {&msg("E","Beginning YYYYMMDD '$bymd' wrong")};
    if ($dd<1  || $dd>$lastday) {&msg("E","Beginning YYYYMMDD '$bymd' wrong")};

    # validate $bhms
    #---------------
    if(length($bhms) != 6 || $bhms >= 240000) {
        &msg("E","Beginning HHMMSS '$bhms' wrong") };

    # validate time increment $ihms and time step $nstep
    #---------------------------------------------------
    if(length($ihms) != 6 || $ihms <=0) {
        &msg("E","time step increment HHMMSS '$ihms' wrong") };

    if($nstep <= 0) {&msg("E","Number of timesteps '$nstep' wrong");}
    return;
}

#=========================================================================
# remove_obsolete_data: 
#=========================================================================
sub remove_obsolete_data {
    my(@classArr, @filtered, $class);
    @classArr = @_;

    while (@classArr) {
        $class = shift @classArr;
        push @filtered, $class unless is_obsolete($class);
    }
    return @filtered;
}

#=========================================================================
# is_obsolete
#=========================================================================
sub is_obsolete {
    my ($class, $flag, $obsolete_class);

    $class = shift @_;
    $flag = 0;

    foreach $obsolete_class (@obsolete_data) {
        if ($class eq $obsolete_class) {
            $flag = 1;
            last;
        }
    }
    return $flag;
}    

#=========================================================================
#
# do_All: looping through database resource file to hard check the existence
#         of each file.
#
#=========================================================================
sub do_All {
    my(@allclass) = @_;
    my($size,$oclass,$nrcd,$rcd,@arr,@field,$hinc,$dstring);
    my($startdate,$enddate);
    my($message);

    $size = scalar(@allclass);

    # check resource file for matched conditions
    #-------------------------------------------
    foreach $oclass (@allclass) {
        &msg("I",">>class = $oclass");
        $nrcd = 0;

      LOOP: while( $rcd = <LUN> ) {
          $rcd = &clean_whitespace($rcd);
          @arr = split(/\s/, $rcd);
          if ( length($rcd)<=2  || $rcd=~/^\#/ || scalar(@arr)<2 ) { next };

          # look for beginning of $oclass
          #------------------------------
          if( ($arr[0] =~ /^BEGIN/i) && ($arr[1] eq $oclass) ) {

              while( $rcd = <LUN> ) {
                  $rcd = &clean_whitespace($rcd);
                  if(length($rcd) <= 2  || ($rcd =~ /^\#/)) { next };

                  if($rcd =~ /^END/i) {
                      if ($nrcd <= 0) {
                          $message = "No valid record found on $rcfile ".
                              "for class '$oclass'.";
                          &msg("E", $message);

                      }
                      last LOOP;
                  }
                  @field = split(/\s/, $rcd);
                  if(scalar(@field) == 3 ) {
                      $nrcd++;
                      ($startdate, $enddate) = &extract_dates($field[0]);
                      $hinc = substr($field[1], 0, 2);
                      $dstring = &DateString($startdate, $enddate, $hinc);
                      &get_ls($field[2], $dstring);
                  }
              }
          }
      }  # end LOOP
    }
    close(LUN) || die "Couldn't close file: $rcfile $!\n";
    &msg("I","\nJob finished.");
    exit(0);      #exit with a success
}

#=========================================================================
#
# classDesc: To get the data description for each obclass defined and quit.
#
#=========================================================================
sub classDesc {
    my($oclass, %desc, $rcd, $x);

    print "\n";
  LOOP: foreach $oclass (@allclass) {
      $desc{$oclass} = "";

      # read records looking for beginning of descriptions
      #---------------------------------------------------
      while( $rcd = <LUN> ) {
          $rcd = &clean_whitespace($rcd);

          # descriptions found
          #-------------------
          if( ($rcd=~/^\#\+/) && ($rcd=~/BEGIN/)) {

              # read records looking for specific class description
              #----------------------------------------------------
              while( $rcd = <LUN> ) {
                  $rcd = &clean_whitespace($rcd);

                  if( $rcd =~ /$oclass\s*\:/ ) {
                      ($x, $desc{$oclass}) = split(":", $rcd);
                      seek(LUN, 0, 0);
                      next LOOP;
                  }
                  if( ($rcd=~/^\#\+/) && ($rcd=~/END/) ) { #found END
                      seek(LUN, 0, 0);
                      next LOOP;
                  }
              }
          }
      }
  }  # end while
    print ("     obclass               Data Description\n");
    print ("     ----------------      -------------------------\n");
    foreach $oclass (@allclass) {
        printf ("     %-20s %-45s\n", $oclass, $desc{$oclass});
    }
    print ("\n");
    exit(0);            #0 as a success exit
}

#=========================================================================
#
# DateList: Returns with array of unique records of YYYYMMDDHH.
#
#=========================================================================
sub DateList {
    my(@ymdh);
    my($ymdhval,$hh,$ymd,$hms);
    my($numsteps);

    # clear output array before starting
    #-----------------------------------
    undef(@ymdh);

    # assemble beginning date/hour and push to array
    #-----------------------------------------------
    $hh = sprintf("%02d",$bhms/10000);
    $ymdhval = $bymd . $hh;
    push @ymdh, $ymdhval;

    # initialize time variables with beginning year, month, day, and hour
    #--------------------------------------------------------------------
    $ymd = $bymd;
    $hms  = $bhms;

    # increment number of steps for look ahead option
    #------------------------------------------------
    $numsteps = $nstep - 1;
    if($lookahead) { $numsteps += $lookahead };

    my $i = 1;
    foreach (1..$numsteps) {

        # add increment to date/time
        #---------------------------
        ($ymd, $hms) = &add_time_increment($ymd,$hms,$ihms);

        $hh = sprintf("%02d",$hms/10000);
        $ymdhval = $ymd . $hh;

        # push onto array
        #----------------
        push @ymdh, $ymdhval;
        $i++;
    }
    return @ymdh;
}

#=========================================================================
#
# get_rcd: get valid records @data from database template file $rcfile,
#          list files if $hard_check used to validate the existence of files
#
# Inputs
#   - $oclass: class name to check whether records are within date range 
#   - @ymdh: list of all dates in the date range
#
# Note: values in @ymdh are in YYYYMMDDHH format
#=========================================================================
sub get_rcd {
    my($oclass, @ymdh);
    my($rcd,$j, @arr, $nrcd, @field, @data);
    my($found,%ymdhstr,$startdate,$enddate,$hhinc);
    my($dstring,$ymdh1,$ymdh2);
    my(@valid,@invalid,$ymdhval,$vdate);
    my ($daterangestring,$timeinc,$fntemplate);
    my($message);

    # input parameters
    #-----------------
    $oclass = shift @_;
    @ymdh = @_;

    $yfound = 0;
    $notfound = 0;
    $nrcd = 0;

    # extract first and last dates from array
    #----------------------------------------
    $ymdh1 = substr($ymdh[0],0,8);
    $ymdh2 = substr($ymdh[-1],0,8);

    # check resource file for matched conditions
    #-------------------------------------------
  LOOP:  while( $rcd = <LUN> ) {
      $rcd = &clean_whitespace($rcd);

      @arr = split(/\s/, $rcd);
      if(length($rcd)<=2  || $rcd=~/\#/ || scalar(@arr)<2) {next};

      # looking for beginning of oclass records
      #----------------------------------------
      if( ($rcd =~ /^BEGIN/i) && ($arr[1] eq $oclass) ) {

          # read oclass records
          #--------------------
          while( $rcd = <LUN> ) {
              $rcd = &clean_whitespace($rcd);

              # skip comments
              #--------------
              if(length($rcd)<=2  || ($rcd=~/\#/)) {next};

              # end of oclass
              #--------------
              if($rcd=~/END/i) {
                  if ($nrcd == 0) {
                      $message = "No valid record found on $rcfile ".
                          "for class '$oclass' from $ymdh1 to $ymdh2.";
                      &msg("E", $message);
                  } else {
                      last LOOP;
                  }
              }

              # process oclass record
              #----------------------
              @field = split(/\s/, $rcd);
              if(scalar(@field) == 3 ) {
                  $daterangestring  = $field[0];
                  $timeinc          = $field[1];
                  $fntemplate       = $field[2];
                  if (length($fntemplate)<10) { next };

                  ($startdate, $enddate) = &extract_dates($daterangestring);
                  $hhinc = substr($timeinc, 0, 2);
                  $dstring = &DateString($startdate, $enddate, $hhinc);   
                  $found = 0;

                  # ymdhstr is hash of strings of valid dates for each fntemplate
                  #--------------------------------------------------------------
                  foreach $ymdhval (@ymdh) {
                      if( $dstring=~/$ymdhval/ ) {
                          $data[$nrcd] = $fntemplate;
                          push(@valid, $ymdhval);
                          $nrcd++;

                          if( $ymdhstr{$fntemplate} ) {
                              $ymdhstr{$fntemplate} .= ",$ymdhval";
                          } else {
                              $ymdhstr{$fntemplate} .= "$ymdhval";
                          }
                      }
                  }
              }
          }
      }
  }
    if($nrcd == 0) {&msg("E","ClassName '$oclass' not found on $rcfile."); }

    if($verbose) {
        foreach $fntemplate (sort keys %ymdhstr) {
            &msg("I","$fntemplate: $ymdhstr{$fntemplate}");
        }
    }

    @valid = &sort_and_remove_duplicates(@valid);
    &msg("I","\nTimes found <<@valid>>") if ($verbose); #YYYYMMDDHH

    # any date not in @valid goes to @invalid
    #----------------------------------------
    if (scalar(@ymdh) > scalar(@valid)) {
        foreach $ymdhval (@ymdh) {
            $found = 0;
            foreach $vdate (@valid) {
                if ($ymdhval == $vdate) {
                    $found = 1;
                    last;
                }
            }
            push @invalid, $ymdhval unless ($found);
        }
        $message = "File name template not found on $rcfile ".
            "for obclass '$oclass' for the following times:  >>@invalid<<";
        &msg("W",$message);
    }

    @data = &sort_and_remove_duplicates(@data);
    &msg("I","\nFile name template(s) found on $rcfile for obclass '$oclass':");

    # check for valid data file name templates in @data
    # (I am not sure the significance of length > 10 -- jcs)
    #-------------------------------------------------------
    $j = 0;
    foreach $fntemplate (@data) {
        $j++ if (length($fntemplate) > 10);
        &msg("I","$fntemplate");
    }

    # if nothing in @data, then no valid records for input $oclass
    #-------------------------------------------------------------
    if($j <= 0) {
        $message = "No matched records found on $rcfile ".
            "for obclass '$oclass' from $ymdh1 to $ymdh2.";
        &msg("E",$message);
    }

    # validate existence of file (hard check)
    #----------------------------------------
    if($hard_check) {   #for file list
        &msg("I","\n>> Checking the existence of files...");

        foreach $fntemplate (keys %ymdhstr) {
            &get_ls($fntemplate, $ymdhstr{$fntemplate});
        }
        &msg("I","\nA total of $yfound files valid.");
        &msg("I","A total of $notfound files not valid.");
    }
    seek(LUN, 0, 0);
    return();
}

#=========================================================================
#
# get_classes_from_rcfile: 
#     1. Extract all classes from the rcfile.
#     2. Check for the following:
#        - error if BEGIN/END statements are not paired
#        - error if BEGIN statement not followed by class name
#        - error if duplicate class names found
#        - warn if BEGIN/END statements are not capitalized
#        - warn if class names differ only by capitalization
#        - warn if extraneous lines found
#
#=========================================================================
sub get_classes_from_rcfile {
    my($rcd,@arr);
    my($i,$j,$k,$mark,$numrecs);
    my($nclass,@allclass,@sortclass,$oclass);
    my($current,$size);
    my($message);

    # initialize counters
    #--------------------
    $k = 0;         # line counter
    $mark = 0;      # counter used to match BEGIN/END statement pairs
    $numrecs = 0;   # number of non-comment lines in each class

    while( $rcd=<LUN> ) {  # <== to read a rcd
        $rcd = &clean_whitespace($rcd);
        $k++;

        if(length($rcd)<=2 || ($rcd=~/^\#/)) { next; };

        if($rcd=~/^BEGIN/i ) {   # checking BEGIN statements
            $mark++;

            # check for error conditions
            #---------------------------
            if( !($rcd=~/BEGIN/)) {
                &msg("W", "(line.$k) 'BEGIN' not all caps: $rcd.");
            }
            if($mark != 1) {
                &msg("E", "(line.$k) BEGIN\/END not in pair: $rcd.");
            }
            @arr = split(/\s/, $rcd);
            if(scalar(@arr) < 2 ) {
                &msg("E", "(line.$k) Format Error is BEGIN obclassName: $rcd") };

            # check stem, if provided
            #------------------------
            $current = $arr[1];
            if ( $butstem ne "" ) {
              unless ($stem && ($current =~ /^$stem/)) {

                  # remove class name to @allclass array
                  #-------------------------------------
                  $numrecs = 0;
                  push @allclass, $current;
              }
            } else {
              unless ($stem && ($current !~ /^$stem/)) {

                  # add class name to @allclass array
                  #-------------------------------
                  $numrecs = 0;
                  push @allclass, $current;
              }
            }

        } elsif($rcd=~/^END/i ) {   # checking END statements
            $mark--;

            # check for error conditions
            #---------------------------
            unless ($numrecs) {
                #--&msg("W", "(line.$k) No records found for class, $current.");
                #--pop @allclass;
            } else {
                $numrecs = 0;
            }
            if( !($rcd=~/END/)) {
                &msg("W", "(line.$k) 'END' not all caps; class: $current.");
            }
            if($mark != 0) {
                &msg("E", "(line.$k) BEGIN/END not in pair for class: $current.");
            }

        } elsif($mark == 0) {  # There should not be lines between entries
            &msg("W", "(line.$k) Illegal line: $rcd.");

        } elsif ( !($rcd=~/^\#/) ) {
            $numrecs++;
        }
    }

    # check for duplicate obclasses entries
    #--------------------------------------
    @sortclass = sort(@allclass);   #sorted @allclass
    $size = scalar(@sortclass);

    for ($i=0; $i<$size-1; $i++) {
        for ($j=$i+1; $j<$size; $j++) {

            if ( lc($sortclass[$i]) eq lc($sortclass[$j]) ) {
                if ( $sortclass[$i] eq $sortclass[$j] ) {
                    &msg("E", "Identical obclass '$sortclass[$j]' found.");
                } else {
                    &msg("W", "obclass $sortclass[$j] == $sortclass[$i]?");
                }
            }
        }
    }

    if($verbose) {
        &msg("I","\n>> Class list:");
        $i = 1;
        foreach $oclass (@allclass) {
            $message = sprintf "%03d: %-35s", $i++, $oclass;
            &msg("I",$message);
        }
    }

    # rewind resource file back to beginning
    #---------------------------------------
    seek(LUN, 0, 0);
    return(@allclass);
}

#=========================================================================
#
# DateString: return a string of YYYYMMDDHH,... from beginning ymdh1
#             to ending ymdh2 with hh as increment
#
#=========================================================================
sub DateString {
    my($ymdh1, $ymdh2, $hhinc);
    my($ymdhval, $string);
    my($ymd, $hms, $ihms, $hh);

    # input parameters
    #-----------------
    $ymdh1 = shift @_;
    $ymdh2 = shift @_;
    $hhinc = shift @_;

    # return if beginning time is greater than end time
    #--------------------------------------------------
    if ( $ymdh1 > $ymdh2 ) {
        return "";
    }

    # initialize date/hour string with beginning yyyymmddhh
    #------------------------------------------------------
    $string = $ymdh1;

    # initialize initial time and time increment
    #-------------------------------------------
    $ymd  = substr($ymdh1,0,8);
    $hms  = substr($ymdh1,8,2) * 10000;
    $ihms = $hhinc * 10000;

    while ( 1 ) {

        # add increment to date/time
        #---------------------------
        ($ymd, $hms) = &add_time_increment($ymd, $hms, $ihms);

        $hh = sprintf("%02d",$hms/10000);
        $ymdhval = $ymd . $hh;

        # add to string if within date/hour range, or exit loop if not
        #-------------------------------------------------------------
        if ( $ymdhval <= $ymdh2 ) {
            $string .= ",$ymdhval";
        } else {
            last;
        }
    }
    return $string;
}

#=========================================================================
#
# extract_dates:
#   - take input in format: YYYYMMDD_HHz-YYYYMMDD_HHz(present)
#   - return with start and end dates in format: YYYYMMDDHH
#
#=========================================================================
sub extract_dates {
    my @field;
    my ($start,$end);
    my ($startdate,$enddate);
    my $inline = shift @_;

    # extract start and end dates
    @field = split /-/, $inline;
    if (scalar(@field) != 2) {
        &msg("E","Wrong format for start & end dates: $inline");
    }
    $start = $field[0];
    $end   = $field[1];

    # reformat start date
    #--------------------
    $startdate = &yyyymmddhh($start);
    unless ($startdate) { &msg("E","Invalid start date/time: $start") };

    # reformat end date
    #------------------
    if ($end =~ /^present$/) {
        $enddate = $today . "18";
    } else {
        $enddate = &yyyymmddhh($end);
    }
    unless ($enddate) { &msg("E","Invalid end date/time: $end") };

    return $startdate, $enddate;
}

#=========================================================================
#
# yyyymmddhh:
#    - take date/time as input in format: yyyymmdd_hhz
#    - check validity of date and time
#    - return datetime in format: yyyymmddhh
#
#=========================================================================
sub yyyymmddhh {
    my($input,$datetime);
    my($yyyy,$mm,$dd,$hh);

    $input = shift @_;
    {
        if (length($input) != 12)              { $datetime = 0; last };

        $yyyy = substr($input, 0, 4);
        $mm   = substr($input, 4, 2);
        $dd   = substr($input, 6, 2);
        $hh   = substr($input, 9, 2);
        &set_mday($yyyy);

        # check year value for gross errors
        #----------------------------------
        if ( $yyyy < 1900  ||  $yyyy > 2200 )  { $datetime = 0; last };

        # check month value reasonableness
        #---------------------------------
        if ( $mm < 1  ||  $mm > 12 )           { $datetime = 0; last };

        # check day and hour values
        #--------------------------
        if ( $dd < 1  ||  $dd > $mday[$mm-1] ) { $datetime = 0; last };
        if ( $hh < 0  ||  $hh > 24)            { $datetime = 0; last };

        # set date time in proper format
        #-------------------------------
        $datetime = $yyyy . $mm . $dd . $hh;
    }
    return $datetime;
}


#=========================================================================
#
# get_ls: to validate (hard check) if the given file do exist
#
#=========================================================================
sub get_ls {
    my($fntemplate, $dstring) = @_;
    my(@ymdh,$ymdhval,$name,$new);
    my($yyyy,$yy,$mm,$dd,$hh);
    my($mass,$local,$opt,$rsh);
    my(@arr,$arrsize,@list,$file);
    my($message);

    $rsh = "ssh";
    @ymdh = split(/,/, $dstring);   # $dstring in the form of YYYYMMDDHH
    $mass = $ENV{MHOST};
    $opt = "-lL";

    foreach $ymdhval (@ymdh) {
        $new = $fntemplate; 
        $yyyy = substr($ymdhval, 0, 4);
        $yy   = substr($ymdhval, 2, 2);
        $mm   = substr($ymdhval, 4, 2);
        $dd   = substr($ymdhval, 6, 2);
        $hh   = substr($ymdhval, 8, 2);
        $new =~ s/%y4/$yyyy/g;
        $new =~ s/%y2/$yy/g;
        $new =~ s/%m2/$mm/g;
        $new =~ s/%d2/$dd/g;
        $new =~ s/%h2/$hh/g;

        $new =~ s/%c/?/g;       #Warning: this may generate more than one file
        $new =~ s/%n/[0-9]/g;   #Warning: this may generate more than one file

        if( ($new =~ /MHOST/) && !$mass ) {
            &msg("E", "MHOST not defined: $new.");
            $notfound++;
            next;        #skip this record
        }
        $new =~ s/\${MHOST}/$mass/g;
        @arr = split(/:/, $new);         
        $arrsize = scalar(@arr);
        
        if( $host =~ /$arr[0]/ || $arr[0]=~ /$host/ || $arrsize == 1) {
            $local = 0;        #local server
            if( $arrsize == 1 ) { 
                @list = `ls $opt $arr[0]`; #no hostName
            } else {
                @list = `ls $opt $arr[1]`;
            }
        } else { 
            if($remote) {
                $arr[0] = $remote ."\@" .$arr[0];
            }
            @list = `$rsh $arr[0] ls $opt $arr[1]`;
        }

        unless (@list) {    # exit after first Error encountered
            $notfound++;
            if ($arrsize == 1) {
                $message = "No files found: $arr[0]";
            } else {
                $message = "Server $arr[0] not accessible" .
                    " or no files found: $arr[1]";
            }
            &msg("E", $message);

        } else {        #file exists
            if($list[0] =~ /password/) { &msg("E", "$arr[0] need new password."); }
            foreach $file (@list) {
                $file = &clean_whitespace($file);
                @arr = split(/\s/, $file);
                foreach (1..3) { shift(@arr) };
                &msg("I","Valid:@arr");
                $yfound++;
            }
        }
    }
}

#=========================================================================
#
# clean_whitespace: 
#     1. chomp
#     2. replace tab characters with blanks
#     3. replace multiple consecutive blanks spaces with a single blank
#     4. remove leading and trailing blanks
#
#=========================================================================
sub clean_whitespace {
    my $line = shift @_;

    chomp($line);
    $line =~ tr/\t/ /s;     # replace tabs with blanks
    $line =~ tr/  / /s;     # replace multiple blanks with single blank
    $line =~ s/^ | $//g;    # remove leading and trailing blanks

    return $line;
  }

#=========================================================================
#
# add_time_increment: add time increment to ymd and hms
#
#=========================================================================
sub add_time_increment {
    my($ymd, $hms, $ihms);
    my($yyyy, $mm, $dd);
    my($hh, $mn, $ss);
    my($ihh, $imn, $iss);

    # input parameters
    #-----------------
    $ymd  = shift @_;
    $hms  = shift @_;
    $ihms = shift @_;

    # break down date and time to $yyyy, $mm, $dd, $hh, $mn, $ss
    #-----------------------------------------------------------
    $yyyy = substr($ymd,0,4);
    $mm   = substr($ymd,4,2);
    $dd   = substr($ymd,6,2);

    $hh = $hms/10000;
    $mn = ($hms/100)%100;
    $ss = $hms%100;

    # break down time increment to $ihh, $imn, $iss
    #----------------------------------------------
    $ihh = $ihms/10000;
    $imn = ($ihms/100)%100;
    $iss = $ihms%100;

    # add increments
    #---------------
    if ($iss) { $ss += $iss };
    if ($imn) { $mn += $imn };
    if ($ihh) { $hh += $ihh };

    # adjust time values as required
    #-------------------------------
    while ($ss >= 60) {
        $ss -= 60;
        $hh++;
    }
    while ($mn >=60) {
        $mn -= 60;
        $hh++;
    }
    while ($hh >=24) {
        $hh -= 24;
        $dd++;
    }
    &set_mday($yyyy);
    while ($dd > $mday[$mm-1]) {
        $dd -= $mday[$mm-1];
        $mm++;

        while ($mm > 12) {
            $mm -= 12;
            $yyyy++;
        }
    }

    # construct values to return
    #---------------------------
    if ($ss < 10) { $ss = sprintf "%02d", $ss };
    if ($mn < 10) { $mn = sprintf "%02d", $mn };
    if ($hh < 10) { $hh = sprintf "%02d", $hh };
    if ($dd < 10) { $dd = sprintf "%02d", $dd };
    if ($mm < 10) { $mm = sprintf "%02d", $mm };
    $ymd = $yyyy.$mm.$dd;
    $hms = $hh.$mn.$ss;

    return $ymd, $hms;
}

#=========================================================================
#
# sort_and_remove_duplicates: from an array
#
#=========================================================================
sub sort_and_remove_duplicates {
    my (@list, $val, $size);

    @list = @_;
    @list = sort @list;
    $size = scalar(@list);

    foreach (1..$size) {
        $val = shift @list;
        if (@list) {
            unless ($val eq $list[0]) { push @list, $val };
        } else {
            push @list, $val;
        }
    }
    return @list;
}

#=========================================================================
#
# set_mday: For inputted year value, set correct number of days in February
#           in global array, @mday.
#
#=========================================================================
sub set_mday {
    my $year = shift @_;

    $mday[1] = 28;
    if(($year%4 == 0 && $year%100 != 0) || $year%400==0 ) { $mday[1] = 29 };
}

#=========================================================================
#
# msg: subroutine to print out informative(I), warning(W), error(E),
#      or fatal(F) message.
#
# NOTES:
# 1. Informative messsages are not printed in quiet mode
# 2. Exit software after Error message.
#=========================================================================
sub msg {
    my ($type, $message) = @_;

    if ($type eq "I") {
        print ("$message\n") unless ($quiet);
    } else {
        print ("-$type: $message\n");
    }

    if($type eq "E") { exit(1) };  # Exit for error
}

#=========================================================================
sub usage {

   print <<"EOF";

NAME
     require_obsys.pl - Procedure to handle filtering of observation 
                        classes created in fvsetup.

SYNOPSIS

     require_obsy.pl [...options...]  bymd bhms ihms nstep obclass

DESCRIPTION

     Require_obsys.pl is a utility to ... 

OPTIONS

     -h              prints this page
     -show_all        show all available data sets (including obsolete data)
     -class_desc     to get the description for each obclass and quit
     -drc fullName   user specified database resource file with file path
                      (default is obsys.rc)
     -hard_check     actually check if the specified file do exist in the mass storage
                      (default is soft_check that is the input conditions will be
                      verified from the observation database resource file)
     -la lstep       Look ahead lstep time steps, that is, checking the existence of
                      future data files (default: lstep=0)
     -rid user       user ID used in remote host machine
                      (default is ENV{USER} from local machine)
     -butstem        allow filtering out all files with specified stem pattern
     -stem pattern   restrict observation class files to those whose name begins
                     with pattern
     -v              verbose mode (default is quiet)
     -q              very quiet (if success, nothing will be printed; if failure, 

RESOURCE FILES


SEE ALSO

    acquire_obsys - Retrives files from mass storage with look-ahead capability

AUTHORS

     Joe Stassi (joe.stassi\@nasa.gov)

EOF

  exit(1)
}
