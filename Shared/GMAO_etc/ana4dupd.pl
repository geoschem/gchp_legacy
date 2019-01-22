#!/usr/bin/env perl

#--------------------------------------------------
#
# Purpose: to rename DAS files during the run
#
# Usage:
#
#  ana4dupd.pl [options] EXPID NYMDB NHMSB
#                              NYMDE NHMSE DTMIN
#
# !REVISION HISTORY:
#
#   05Feb2014 Todling  Initial code
#   21Sep2015 Todling  Add a version of an incremental update machinery
#   26Apr2016 Todling  Add option to update initial time restarts (4d-only)
#   20Feb2017 Todling  Parallel upd; opt to calc only initial tendency
#
# !TO DO:
#
#   1) make sure works in lat-lon case
#
#--------------------------------------------------
use Env;                 # make env vars readily available
use FindBin;             # so we can find where this script resides
use File::Basename;      # for basename(), dirname()
use File::Copy "cp";     # for cp()
use Getopt::Long;        # load module with GetOptions function
use Shell qw(cat rm);    # cat and rm commands
use Time::Local;         # time functions

# Look for perl packages in the following locations
# -------------------------------------------------
use lib ( "$FindBin::Bin", "$FVROOT/bin", "$ESMADIR/$ARCH/bin" );
use Manipulate_time "tick";

# Parse command line
# ------------------
  GetOptions ( "dir=s",
               "iter=s",
               "rcdir=s",
               "updrst0",
               "ncpus=i",
               "iau",
               "debug",
               "h" );

  usage() if $opt_h;

# FVROOT is where the binaries have been installed
# ------------------------------------------------
  $fvroot  = dirname($FindBin::Bin);
  $fvroot  =~ s|/u/.realmounts/share|/share|;   # for portability across
                                              # NAS machines
 
# Initialize variables
# --------------------
  init();

# Increment update
# ----------------
  iupd();

# Analysis update
# ---------------
  aupd();

# Create IAU tendencies
# ---------------------
  iau();

# Update initial conditions
# -------------------------
  rst0upd();

# Handle initial background (since GCM does not provide update to this)
# ---------------------------------------------------------------------
######  updt0();  #_RT: leave this up to main script (GEOSdas.csm)

# If made it here, exit with no error
# -----------------------------------
  exit(0);

#......................................................................
sub iupd {
  return 0 unless ($do_iupd);
  return 0 unless ($iter > 0);

# Special-handle first file
# -------------------------
  my $nymd = $nymdb;  $nhms = $nhmsb;
  my $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }
  my ($fname, $ext) = &fname_fext ($ifile); 
  $Ifile = "${fname}.iter${iter}.$ext";
  if ( ! -e $Ifile ) {
       $errcode = 99;
       die ">>> ERROR <<< cannot find file $Ifile " if ( $errcode );
  }
  $cmd = $fvroot . "/bin/dyn_iupd.x $Ifile $ifile";
  print "On ITER $iter: Begin $cmd \n";
  if ( ! $opt_debug ) {
      $errcode = system($cmd);
      die ">>> ERROR <<< cannot update increment at time ($nymd,$nhms) " if ( $errcode );
      if ( ! -e "$afile" ) {
         die ">>> ERROR <<< did not increment update at time ($nymd,$nhms) " if ( $errcode );
      }
  }

# Loop over time for all other files
# ----------------------------------
  while ( $nymd != $nymde || $nhms != $nhmse ) {
     ($nymd,$nhms) = tick($nymd,$nhms,$dtsec);
     $hh     = substr($nhms,0,2);
     if ( $rcdir ) {
         $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
         my $rcopt = "-rc $rcdir/fvpsas.rc";
     } else {
         $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
         my $rcopt = "";
     }
     my ($fname, $ext) = &fname_fext ($ifile); 
     $Ifile = "${fname}.iter${iter}.$ext";
     if ( ! -e $Ifile ) {
          $errcode = 99;
          die ">>> ERROR <<< cannot find file $Ifile " if ( $errcode );
     }
     $cmd = $fvroot . "/bin/dyn_iupd.x $Ifile $ifile";
     print "On ITER $iter: Begin $cmd \n";
     if ( ! $opt_debug ) {
         $errcode = system($cmd);
         die ">>> ERROR <<< cannot update increment at time ($nymd,$nhms) " if ( $errcode );
         if ( ! -e "$afile" ) {
            die ">>> ERROR <<< did not increment update at time ($nymd,$nhms) " if ( $errcode );
         }
     }
  }
}
#......................................................................
sub aupd {

# Special-handle first file
# -------------------------
  $nymd = $nymdb;  $nhms = $nhmsb;
  $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`;  chomp($bfile);
      $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
      $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`;  chomp($afile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
      $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
      $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }
# In case of multiple outer loops, the analysis is formed by adding 
# the total increment at interation ITER to the original background
# at iteration 0
  if ( $do_iupd && $iter > 0 ) {
     my ($bname, $ext) = &fname_fext ($bfile); 
     $bfile = "${bname}.iter0.$ext";
     if ( ! -e $bfile ) {
          $errcode = 99;
          die ">>> ERROR <<< cannot find file $bfile " if ( $errcode );
     }
  }

# Create ana for this time
  $cmd = $fvroot . "/bin/dynp.x $rcopt -s $bfile -p $ifile -a 1 -ainc -g5 -pncf -pureadd -realp -os $afile";
  print "On ITER $iter: Begin $cmd \n";
  if ( ! $opt_debug ) {
      $errcode = system($cmd);
      die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
      if ( ! -e "$afile" ) {
         die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
      }
  }

# Loop over time for all other files
# ----------------------------------
  my ($newpid, $pid, @pidARR);
  while ( $nymd != $nymde || $nhms != $nhmse ) {
     ($nymd,$nhms) = tick($nymd,$nhms,$dtsec);

     @pidARR = load_balance($MAX, @pidARR); # do not fork more than $MAX jobs
     defined($newpid=fork) or die ">> ERROR << Cannot fork: $!";
     unless ($newpid) {

         #---------------#
         # child process #
         #---------------#
         aupd_child($nymd,$nhms);
         exit;
     }

     #----------------#
     # parent process #
     #----------------#
     push @pidARR, $newpid;
  }

# wait for forked jobs to complete
# --------------------------------
  while (@pidARR) {
      $pid = shift @pidARR;
      waitpid($pid,0);
  }

} # <upd>

#......................................................................
sub aupd_child{
     my($nymd,$nhms) = @_;
     my($bfile,$ifile,$afile);
     my($hh,$cmd,$errcode);

     $hh     = substr($nhms,0,2);
     if ( $rcdir ) {
         $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`;  chomp($bfile);
         $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
         $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`;  chomp($afile);
     } else {
         $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
         $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
         $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
     }
     if ( $do_iupd && $iter > 0 ) {
        my ($bname, $ext) = &fname_fext ($bfile); 
        $bfile = "${bname}.iter0.$ext";
        if ( ! -e $bfile ) {
             $errcode = 99;
             die ">>> ERROR <<< cannot find file $bfile " if ( $errcode );
        }
     }
     $cmd = $fvroot . "/bin/dynp.x $rcopt -s $bfile -p $ifile -a 1 -ainc -g5 -pncf -pureadd -realp -os $afile";
     if ( $nymd == $nymde && $nhms == $nhmse ) { # special handle file at final time
         print "On ITER $iter: End $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "$afile" ) {
              die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
           }
         }
     } else {
         print "On ITER $iter: Others $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot update at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "$afile" ) {
              die ">>> ERROR <<< did not update at time ($nymd,$nhms) " if ( $errcode );
           }
         }
     }
}
#......................................................................
sub updt0 {

  return 0 if ( $do_iupd ); # in this case, initial bkg already updated with increment

# In 4d case, overwrite background with analysis
# ----------------------------------------------
  return 0 if ( $nymdb == $nymde && $nhmsb == $nhmse );

# Special-handle first file
# -------------------------
  $nymd = $nymdb;  $nhms = $nhmsb;
  $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`;  chomp($bfile);
      $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`;  chomp($afile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
      $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }

  my ($bname, $ext) = &fname_fext ($bfile); 
  $bfnew = "${bname}.iter$iter.$ext";
  print "On ITER $iter: copying $bfile  to  $bfnew \n" ;
  cp("$bfile","$bfnew");
  print "On ITER $iter: overwriting $bfile  with  $afile \n" ;
  cp("$afile","$bfile");   

}
#......................................................................
sub iau {

  return unless ($getiau);

# Special-handle first file
# -------------------------
  $nymd = $nymdb;  $nhms = $nhmsb;
  $hh   = substr($nhms,0,2);
  if ( $rcdir ) {
      $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`; chomp($bfile);
      $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`; chomp($afile);
      $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
      my $rcopt = "-rc $rcdir/fvpsas.rc";
  } else {
      $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
      $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
      $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
      my $rcopt = "";
  }
  if ( $do_iupd && $iter > 0 ) {
     my ($bname, $ext) = &fname_fext ($bfile); 
     $bfile = "${bname}.iter0.$ext";
     if ( ! -e $bfile ) {
          $errcode = 99;
          die ">>> ERROR <<< cannot find file $bfile " if ( $errcode );
     }
  }

  ed_mkiau_rc ($bfile,$afile,$ifile,$nymd,$nhms);

# Create iau tendency for this time
  $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
  $agcm4gcm = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms noexp_iau_tendency_filename`; chomp($agcm4gcm);
  if ( -e "agcm_import_rst" ) { rm("agcm_import_rst") };
  if ( -e "$iaufname" )       { rm("$iaufname") };
  if ( -e "$agcm4gcm" )       { rm("$agcm4gcm") };
  $cmd = "$MPIRUN_IAU";
  print "On ITER $iter: Begin $cmd \n";
  if ( ! $opt_debug ) {
      $errcode = system($cmd);
      die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
      if ( ! -e "agcm_import_rst" ) {
         die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
      }
      print "On ITER $iter: moving agcm_import_rst to $iaufname ...\n";
      rename("agcm_import_rst","$iaufname");
      print "On ITER $iter: linking $iaufname to $agcm4gcm ...\n";
      Assignfn("$iaufname","$agcm4gcm");
  }

# Loop over time for all other files
# ----------------------------------
  return if ( $iau0only == 1 );
  while ( $nymd != $nymde || $nhms != $nhmse ) {
     ($nymd,$nhms) = tick($nymd,$nhms,$dtsec);
     $hh     = substr($nhms,0,2);
     if ( $rcdir ) {
         $bfile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_bkg_filename`; chomp($bfile);
         $afile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_ana_filename`; chomp($afile);
         $ifile = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms upper-air_xinc_filename`; chomp($ifile);
     } else {
         $bfile = "$expid.$btype.${nymd}_${hh}z.$ncsuffix";
         $afile = "$expid.$atype.${nymd}_${hh}z.$ncsuffix";
         $ifile = "$expid.$itype.${nymd}_${hh}z.$ncsuffix";
     }  
     if ( $do_iupd && $iter > 0 ) {
        my ($bname, $ext) = &fname_fext ($bfile); 
        $bfile = "${bname}.iter0.$ext";
        if ( ! -e $bfile ) {
             $errcode = 99;
             die ">>> ERROR <<< cannot find file $bfile " if ( $errcode );
        }
     }
     ed_mkiau_rc ($bfile,$afile,$ifile,$nymd,$nhms);
     $cmd = "$MPIRUN_IAU";
     if ( $nymd == $nymde && $nhms == $nhmse ) { # special handle file at final time
         print "On ITER $iter: End $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "agcm_import_rst" ) {
              die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           }
           $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
           $agcm4gcm = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms noexp_iau_tendency_filename`; chomp($agcm4gcm);
           print "On ITER $iter: moving agcm_import_rst to $iaufname ...\n";
           rename("agcm_import_rst","$iaufname");
           print "On ITER $iter: linking $iaufname to $agcm4gcm ...\n";
           Assignfn("$iaufname","$agcm4gcm");
         }
     } else {
         print "On ITER $iter: Others $cmd \n";
         if ( ! $opt_debug ) {
           $errcode = system($cmd);
           die ">>> ERROR <<< cannot create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           if ( ! -e "agcm_import_rst" ) {
              die ">>> ERROR <<< did not create IAU tendency at time ($nymd,$nhms) " if ( $errcode );
           }
           $iaufname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($iaufname);
           $agcm4gcm = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms noexp_iau_tendency_filename`; chomp($agcm4gcm);
           print "On ITER $iter: moving agcm_import_rst to $iaufname ...\n";
           rename("agcm_import_rst","$iaufname");
           print "On ITER $iter: linking $iaufname to $agcm4gcm ...\n";
           Assignfn("$iaufname","$agcm4gcm");
         }
     }
  }

} # <iau>

#......................................................................
sub ed_mkiau_rc {

  return unless ($getiau);

  my($bkgfile,$anafile,$incfile,$anymd,$anhms) = @_;

  if( -e "$rcdir/mkiau.rc" ) { rm("$rcdir/mkiau.rc") };

  cp("$rcdir/mkiau.rc.tmpl","$rcdir/mkiau.rc.tmp");

  $ft   = "$rcdir/mkiau.rc";
  $frun = "$rcdir/mkiau.rc.tmp";

  open(LUN,"$frun") || die "Fail to open $frun: $!\n";
  open(LUN2,">$ft") || die "Fail to open   $ft: $!\n";

  while( defined($rcd = <LUN>) ) {
     chomp($rcd);
     if($rcd =~ /\>>>EXPID<<</)    {$rcd=~ s/\>>>EXPID<<</$expid/g; }
     if($rcd =~ /\>>>BKGFNAME<<</) {$rcd=~ s/\>>>BKGFNAME<<</$bkgfile/g; }
     if($rcd =~ /\>>>ANAFNAME<<</) {$rcd=~ s/\>>>ANAFNAME<<</$anafile/g; }
     if($rcd =~ /\>>>INCFNAME<<</) {$rcd=~ s/\>>>INCFNAME<<</$incfile/g; }
     if($rcd =~ /\>>>NCSUFFIX<<</) {$rcd=~ s/\>>>NCSUFFIX<<</$ncsuffix/g; }
     if($rcd =~ /\>>>ANADATE<<</)  {$rcd=~ s/\>>>ANADATE<<</$anymd/g; }
     if($rcd =~ /\>>>ANATIME<<</)  {$rcd=~ s/\>>>ANATIME<<</$anhms/g; }
     print(LUN2 "$rcd\n");
  }

  close(LUN);
  close(LUN2);
  rm("$rcdir/mkiau.rc.tmp");

}

#......................................................................
sub rst0upd {

  return unless ($upd0);

# Special-handle first file
# -------------------------
  my $nymd = $nymdb;  $nhms = $nhmsb;
  my $hh   = substr($nhms,0,2);
  my $timetagz = "${nymd}_${hh}z";

  ed_rstupd_rc ($nymd,$nhms);

# Get relevant filenames
# ----------------------
# input:
  $fvdynrst = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms DYN_INTERNAL_RESTART_FILE`; chomp($fvdynrst);
  $moistrst = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms MOIST_INTERNAL_RESTART_FILE`; chomp($moistrst);
  $pchemrst = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms PCHEM_INTERNAL_RESTART_FILE`; chomp($pchemrst);
  $ainc0rst = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms AINC_INTERNAL_RESTART_FILE`; chomp($ainc0rst);
  $species  = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms pchem_clim`; chomp($species);
  $aincname = `$fvroot/bin/echorc.x -rc $rcdir/fvpsas.rc -template $expid $nymd $nhms iau_tendency_filename`; chomp($aincname);
# output:
  $fvdynupd = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms DYN_INTERNAL_CHECKPOINT_FILE`; chomp($fvdynupd);
  $moistupd = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms MOIST_INTERNAL_CHECKPOINT_FILE`; chomp($moistupd);
  $pchemupd = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms PCHEM_INTERNAL_CHECKPOINT_FILE`; chomp($pchemupd);

# settings:
  $domoist = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms ALLOW_MOIST_AINC_UPDATE`; chomp($domoist);
  $dopchem = `$fvroot/bin/echorc.x -rc $rcdir/rstupd.rc -template $expid $nymd $nhms ALLOW_PCHEM_AINC_UPDATE`; chomp($dopchem);

# Make sure PCHEM-related files are present
  if ( ! -e "Chem_Registry.rc" ) {
     die ">>> ERROR <<< cannot find Chem_Registry.rc needed to handle PCHEM update" if ( $errcode );
  }

  if ( ! -e "$species" ) {
     $errcode = 999;
     die ">>> ERROR <<< cannot find file $species ; likely lnbcs has not been executed" if ( $errcode );
  }

  my $adynsfx  = "bin";
  my $moistsfx = "bin";
  my $pchemsfx = "bin";

# if ( $do_iupd && $iter > 0 ) {
#    $gcmfile = "$expid.fvcore_internal_rst.$timetagz.iter0.$dynsfx";
#    if ( -e $gcmfile ) {
#       print "On ITER $iter: Linking $gcmfile to $fvdynrst to update initial restart\n";
#       Assignfn("$gcmfile","$fvdynrst");
#    } else {
#         $errcode = 99;
#         die ">>> ERROR <<< cannot find file $gcmfile " if ( $errcode );
#    }
#    $gcmfile = "$expid.moist_internal_rst.$timetagz.iter0.$moistsfx";
#    if ( -e $gcmfile ) {
#       print "On ITER $iter: Linking $gcmfile to $moistrst to update initial restart\n";
#       Assignfn("$gcmfile","$moistrst");
#    } else {
#         $errcode = 99;
#         die ">>> ERROR <<< cannot find file $gcmfile " if ( $errcode );
#    }
#    $gcmfile = "$expid.pchem_internal_rst.$timetagz.iter0.$pchemsfx";
#    if ( -e $gcmfile ) {
#       print "On ITER $iter: Linking $gcmfile to $pchemrst to update initial restart\n";
#       Assignfn("$gcmfile","$pchemrst");
#    } else {
#         $errcode = 99;
#         die ">>> ERROR <<< cannot find file $gcmfile " if ( $errcode );
#    }
# }

# Update initial conditions at initial time
  if ( -e "$aincname" ) {
     print "On ITER $iter: Linking $aincname to $ainc0rst to update initial restart\n";
     Assignfn("$aincname","$ainc0rst");
     $cmd = "$MPIRUN_UPRST";
     print "Begin $cmd \n";
     if ( ! $opt_debug ) {
        $errcode = system($cmd);
        die ">>> ERROR <<< cannot update initial condition at time ($nymd,$nhms) " if ( $errcode );
        # fvcore ...
        if ( ! -e "$fvdynupd" ) {
           die ">>> ERROR <<< did not update initial FVCORE file at time ($nymd,$nhms) " if ( $errcode );
        }
#       wire filename until MAPL Generic made to resolve filename template
        $fvcwired = "$expid.$fvdynrst.${nymd}_${hh}z.$adynsfx";
        rename("$fvdynupd","$fvcwired");
        print "On ITER $iter: Updated $fvcwired \n";
#       Assignfn("$fvcwired","$fvdynrst");
#       print "On ITER $iter: Linked $fvcwired to $fvdynrst \n";
#       Assignfn("$fvdynupd","$fvdynrst");
#       print "On ITER $iter: Linked $fvdynupd to $fvdynrst \n";
        # moist ...
        if ( ! -e "$moistupd" ) {
           die ">>> ERROR <<< did not update initial MOIST  file at time ($nymd,$nhms) " if ( $errcode );
        }
#       wire filename until MAPL Generic made to resolve filename template
        $mstwired="$expid.$moistrst.${nymd}_${hh}z.$moistsfx";
        rename("$moistupd","$mstwired");
        print "On ITER $iter: Updated $mstwired \n";
#       Assignfn("$mstwired","$moistrst");
#       print "On ITER $iter: Linked $mstwired to $moistrst \n";
#       Assignfn("$moistupd","$moistrst");
#       print "On ITER $iter: Linked $moistupd to $moistrst \n";
        # pchem ...
        if ( ! -e "$pchemupd" ) {
           die ">>> ERROR <<< did not update initial PCHEM  file at time ($nymd,$nhms) " if ( $errcode );
        }
#       wire filename until MAPL Generic made to resolve filename template
        $pchwired="$expid.$pchemrst.${nymd}_${hh}z.$pchemsfx";
        rename("$pchemupd","$pchwired");
        print "On ITER $iter: Updated $pchwired \n";
#       Assignfn("$pchwired","$pchemrst");
#       print "On ITER $iter: Linked $pchwired to $pchemrst \n";
#       Assignfn("$pchemupd","$pchemrst");
#       print "On ITER $iter: Linked $pchemupd to $pchemrst \n";
     }
  } else {
     $errcode = 99;
     die ">>> ERROR <<< cannot find file $aincname " if ( $errcode );
  }

} # <rst0upd>

#......................................................................
sub ed_rstupd_rc {

  return unless ($upd0);

  my($anymd,$anhms) = @_;

  if( -e "$rcdir/rstupd.rc" ) { rm("$rcdir/rstupd.rc") };

  cp("$rcdir/rstupd.rc.tmpl","$rcdir/rstupd.rc.tmp");

  $ft   = "$rcdir/rstupd.rc";
  $frun = "$rcdir/rstupd.rc.tmp";

  open(LUN,"$frun") || die "Fail to open $frun: $!\n";
  open(LUN2,">$ft") || die "Fail to open   $ft: $!\n";

  while( defined($rcd = <LUN>) ) {
     chomp($rcd);
     if($rcd =~ /\>>>ANADATE<<</)  {$rcd=~ s/\>>>ANADATE<<</$anymd/g; }
     if($rcd =~ /\>>>ANATIME<<</)  {$rcd=~ s/\>>>ANATIME<<</$anhms/g; }
     print(LUN2 "$rcd\n");
  }

  close(LUN);
  close(LUN2);
  rm("$rcdir/rstupd.rc.tmp");

}

#......................................................................
sub init {

   if ( $#ARGV  <  5 ) {
     print STDERR " Improper input parameters ; see usage:\n";
     usage();
   } else {              # required command line args
     $expid = $ARGV[0];
     $nymdb = $ARGV[1]; 
     $nhmsb = sprintf("%6.6d",$ARGV[2]); 
     $nymde = $ARGV[3]; 
     $nhmse = sprintf("%6.6d",$ARGV[4]); 
     $dtmin = $ARGV[5]; 
   }
   $dtsec = $dtmin * 60;
 
#  When passed, get positioned in this directory ...
   if ( $opt_dir ) {
      #chdir("$opt_dir");
   }

   $ncsuffix = "nc4";
   $do_iupd = 1;

   $iter = 0;
   if ( $opt_iter ) {
      $iter = $opt_iter;
   }

   if ( $opt_rcdir ) {
       $rcdir = $opt_rcdir;
   }

   if ( $opt_iau ) {
      if ( $ENV{MPIRUN_IAU} ) {
         $getiau = 1;
      } else {
         die ">>> ERROR <<< need env(MPIRUN_IAU) to create IAU tendency";
      }
   }
   $upd0 = 0;
   if ( $opt_updrst0 ) {
      $upd0 = 1;
   }

   $btype = "bkg.eta";
   $itype = "xinc.eta";
   $atype = "ana.eta";

   $iau0only = 1;
   $iau0only = $ENV{ANA4DUPD_IAU0_ONLY} if(defined $ENV{'ANA4DUPD_IAU0_ONLY'});

  $MAX = 1;
  if ( defined($opt_ncpus) ) {
      $MAX = $opt_ncpus;
      if ($MAX < 1) { $MAX = 1; }
      else {
          $NCPUS = $ENV{"NCPUS"};
          if ($NCPUS) {
            if($MAX > $NCPUS) {
               $MAX = $NCPUS;
               print "Redefine number of processes used to NCPUS: $MAX \n";
            }
          }
      }
  }

}
#......................................................................

#=======================================================================
# name: load_balance
# purpose: If the number of child processes is at MAX or above, then
#          wait here until enough child processes complete to get the
#          total number under the limit.
#=======================================================================
sub load_balance {

    my ($MAX, @pidARR);
    my ($check_counter, $pid, $status);

    # get input parameters
    #---------------------
    $MAX = shift @_;
    @pidARR = @_;

    while (scalar(@pidARR) >= $MAX) {

        # loop through child processes
        #-----------------------------
        $check_counter = 0;
        while (1) {

            # check child process
            #---------------------------------
            # status equals 0   if still alive
            # status equals pid if complete
            # status equals -1  if not found
            #---------------------------------
            $pid = shift @pidARR;
            $status = waitpid($pid, WNOHANG);
            last if $status;

            # child process not complete
            #---------------------------
            push @pidARR, $pid;
            $check_counter++;

            # take one second breather before looping through again
            #------------------------------------------------------
            if ($check_counter >= $MAX) {
                sleep 1;
                $check_counter = 0;
            }
        }
    }
    return @pidARR if @pidARR;
}
#......................................................................
sub fname_fext {
    local $_ = reverse $_[0] or 
        die "no filename supplied to fname_fext!\n";
    #no period or ext found, return original arg
    return $_[0] unless /(.*?)\.(.+)/; 
    my $fname = reverse$2;
    my $ext= reverse $1;
    return ($fname, $ext);    
}
#......................................................................
sub Assignfn {

# Assignfn - assigns fn to given file name fname.
# fname = old file
# fn = new file (links to old)
  my ( $fname, $fn ) = @_;
  unlink($fn) if ( -e $fn ) ;
  symlink("$fname","$fn");
}
#......................................................................
sub usage {
   print <<"EOF";

NAME
     ana4dupd.pl - update background from 4d analysis
          
SYNOPSIS

     ana4dupd.pl  expid nymdb nhmsb nymde nhmse dtmin
     e.g.,
     ana4dupd.pl  d541  20090731 210000 20090731 210000 0
     ana4dupd.pl  d541  20090731 210000 20090801 030000 180
          
DESCRIPTION

     This script updates backgrounds given analysis increments 
     generated within the (typically 4d) variational loop.

     The following parameters are required 

     expid  experiment id
     nymdb  Starting date
     nhmsb  Starting time
     nymde  Ending date
     nhmse  Ending time
     dtmin  Time increment in minutes

    Require RC files:

     fvpsas.rc      - file holding filename templates for DAS-related files
     mkiau.rc.tmpl  - file with intructions for how to run IAU (only when applicable)
     rstupd.rc.tmpl - file with intructions for how to run RSTUPD (only when applicable)

    Optinal Arguents:

    -h             help (echoes this usage)   
    -dir  DIRNAME  work directory (bkg and xinc files should be here)
    -iter ITER     iteration number of outer loop to accommodate incremental approach
    -iau           creates IAU tendency from incremental difference
    -ncpus         defines number of CPUS to use in forked work load
    -updrst0       update initial time restarts
    -rcdir DIR     directory where to find relavant RC files
    -debug         simply check basic flow of this procedure not doing anything

    Optional Environment Variables:
 
    MPIRUN_IAU     required only when -iau option specified,
                     e.g., setenv MPIRUN_IAU "mpirun -np 24 FULLPATH/mkiau.x"
    MPIRUN_UPRST   required only when -updrst0 option is used,
                     e.g., setenv MPIRUN_UPRST "mpirun -np 24 FULLPATH/rstupd.x"

    ANA4DUPD_IAU0_ONLY  when specified to 1 will calc only initial-time IAU tendency
                           (default: 1)

EOF

  exit(1);
}
