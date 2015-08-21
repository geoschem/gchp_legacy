package Conv_utils;
require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(repack_stage conv_archive raw_archive);

#=================  Stage the repack data  ======================

sub repack_stage {

# This module contains the copy() subroutine.

   use File::Copy;

# This module contains the mkpath() subroutine.

   use File::Path;

# This module contains the rput(), chmod_remote() and mkdir_remote() subroutines.

   use Remote_utils;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[$#_] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Directory in which the REPACK files to be staged will be found).

   if ( defined( %options ) && exists( $options{"from_dir"} ) ) {
      $from_dir = $options{"from_dir"};
   } else {
      $from_dir = ".";
   }

# Option indicating that prep ID is not included on the file names.

   if ( defined( %options ) && exists( $options{"no_prep_id"} ) ) {
      $no_prep_id = $options{"no_prep_id"};
   } else {
      $no_prep_id = 0;
   }

# Arguments

   my $prep_ID    = $_[0];
   my $repack_tag = $_[1];
   my $stage_loc  = $_[2];
   my $stage_date;

   if ( $#_ >= 3 ) {
      $stage_date = $_[3];
   } else {
      $stage_date = 0;
   }

# If the staging location does have a "@", but does not have a ":",
# then assume it's a remote location of the form "userid@machine" and 
# just put a ":" at the end.  The subdirectory will be appended, and the path 
# will be relative to the user's home directory on that machine.

   if ( $stage_loc =~ /@/ && $stage_loc !~ /:/ ) { $stage_loc = "$stage_loc:"; }

# If the archive location does not end with a ":" or a "/", then assume some path 
# has been given, and put a "/" there to allow later appending of the subdirectory.

   if ( $stage_loc !~ m'[:/]$' ) { $stage_loc = "$stage_loc/"; }

   $files_trans = 0;

# Get list of REPACK files to be staged, if no particular date has
# been indicated, otherwise get that specific date.

   if ( $stage_date == 0 ) {

      opendir( DIR, $from_dir ) || die "can't opendir $from_dir: $!";

      if ( $no_prep_id ) {
         @staged_files = grep { /^$repack_tag/ } readdir( DIR );
      } else {
         @staged_files = grep { /^$repack_tag\.$prep_ID/ } readdir( DIR );
      }

      closedir( DIR );

   } else {

      if ( $no_prep_id ) {
         @staged_files = ( "$repack_tag.t$stage_date" );
      } else {
         @staged_files = ( "$repack_tag.$prep_ID.t$stage_date" );
      }

   }

# Append preprocessing ID and file type to get the complete location.

   $full_stage_loc = "$stage_loc$prep_ID/repack";

# Process each file.

   foreach $sfile ( @staged_files ) {

# Check if the location to stage to is a remote location.  If so, use the
# remote put function.  Otherwise, just do a copy.  Make the directory first.
# File and directory permission modes are also explicitly set.  Note that mkpath
# interacts with the umask, so even though it defaults to 0777, it must be reset
# to what we want.

      $err_trans = 0;

      if ( $stage_loc =~ /:/ ) {
         mkdir_remote( $full_stage_loc ) or $err_trans = 1;
         chmod_remote( $full_stage_loc, '0755', { 'recursive' => 1 } ) or $err_trans = 1;
         rput( "$from_dir/$sfile", $full_stage_loc ) or $err_trans = 1;
      } else {
         if ( ! -d $full_stage_loc ) {
            mkpath( $full_stage_loc ) or $err_trans = 1;
            chmod( 0755, $full_stage_loc ) or $err_trans = 1;
         }
         copy( "$from_dir/$sfile", $full_stage_loc ) or $err_trans = 1;
         chmod( 0644, "$full_stage_loc/$sfile" ) or $err_trans = 1;
      }

      if ( $err_trans ) {
         print STDERR "(repack_stage) ERROR - Could not transfer $from_dir/$sfile to $full_stage_loc","\n";
      } else {
         ++ $files_trans;
      }

   }

   return( $files_trans );

}

#============  Archive the conventional data ====================

sub conv_archive {

# This module contains the copy() subroutine.

   use File::Copy;

# This module contains the mkpath() subroutine.

   use File::Path;

# Any arithmetic done in this subroutine is done as integer.

   use integer;

# This module contains the rput(), chmod_remote() and mkdir_remote() subroutines.

   use Remote_utils;

# This module contains the mmm_yr() subroutine.

   use Manipulate_time;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[$#_] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Working directory for the preprocessing.  The UNPACK files will be 
# archived from a "unpack_storage" subdirectory in this directory, and
# the REPACK files will be archived from the "repack_storage" subdirectory.

   if ( defined( %options ) && exists( $options{"conv_work_dir"} ) ) {
      $conv_work_dir = $options{"conv_work_dir"};
   } else {
      $conv_work_dir = ".";
   }

# Option indicating that prep ID is not included on the file names.

   if ( defined( %options ) && exists( $options{"no_prep_id"} ) ) {
      $no_prep_id = $options{"no_prep_id"};
   } else {
      $no_prep_id = 0;
   }

# Arguments

   my $prep_ID     = $_[0];
   my $repack_tag  = $_[1];
   my $archive_loc = $_[2];
   my $archive_date;

   if ( $#_ >= 3 ) {
      $archive_date = $_[3];
   } else {
      $archive_date = 0;
   }

# If the archive location does have a "@", but does not have a ":",
# then assume it's a remote location of the form "userid@machine" and 
# just put a ":" at the end.  The subdirectory will be appended, and the path 
# will be relative to the user's home directory on that machine.

   if ( $archive_loc =~ /@/ && $archive_loc !~ /:/ ) { $archive_loc = "$archive_loc:"; }

# If the archive location does not end with a ":" or a "/", then assume some path 
# has been given, and put a "/" there to allow later appending of the subdirectory.

   if ( $archive_loc !~ m'[:/]$' ) { $archive_loc = "$archive_loc/"; }

   $files_trans = 0;

   $unpack_dir = "$conv_work_dir/unpack_storage";
   $repack_dir = "$conv_work_dir/repack_storage";

# Get list of unpack files to be staged, if no particular date has
# been indicated, otherwise get that specific date.

   if ( $archive_date == 0 ) {

      opendir( DIR, $unpack_dir ) || die "can't opendir $unpack_dir: $!";

      if ( $no_prep_id ) {
         @archived_files = grep { /^[^\.]+\.t/ } readdir( DIR );
      } else {
         @archived_files = grep { /^[^\.]+\.$prep_ID/ } readdir( DIR );
      }

      closedir( DIR );

   } else {

      opendir( DIR, $unpack_dir ) || die "can't opendir $unpack_dir: $!";

      if ( $no_prep_id ) {
         @archived_files = grep { /^[^\.]+\.t$archive_date/ } readdir( DIR );
      } else {
         @archived_files = grep { /^[^\.]+\.$prep_ID\.t$archive_date/ } readdir( DIR );
      }

      closedir( DIR );

   }

# Process each file.

   foreach $sfile ( @archived_files ) {

# Find the directory this file should go in.

      @nodes = split( /\./, $sfile );

      $type_tag = $nodes[0];
      ( $time_tag = $nodes[$#nodes] ) =~ s/^t//;

      $subdir = join( "/", $prep_ID, "unpack", $type_tag, mmm_yr( $time_tag/100 ) );

# Check if the location to archive to is a remote location.  If so, use the
# remote put function.  Otherwise, just do a copy.  Make the directory first.
# File and directory permission modes are also explicitly set.  Note that mkpath
# interacts with the umask, so even though it defaults to 0777, it must be reset
# to what we want.

      $err_trans = 0;

      if ( $archive_loc =~ /:/ ) {
         mkdir_remote( "$archive_loc$subdir" ) or $err_trans = 1;
         chmod_remote( "$archive_loc$subdir", '0755', { 'recursive' => 1 } ) or $err_trans = 1;
         rput( "$unpack_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
      } else {
         if ( ! -d "$archive_loc$subdir" ) {
            mkpath( "$archive_loc$subdir" ) or $err_trans = 1;
            chmod( 0755, "$archive_loc$subdir" ) or $err_trans = 1;
         }
         copy( "$unpack_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
         chmod( 0644, "$archive_loc$subdir/$sfile" ) or $err_trans = 1;
      }

      if ( $err_trans ) {
         print STDERR "(conv_archive) ERROR - Could not transfer $unpack_dir/$sfile to $archive_loc$subdir","\n";
      } else {
         ++ $files_trans;
         unlink "$unpack_dir/$sfile";
      }

   }

# Get list of repack files to be staged, if no particular date has
# been indicated, otherwise get that specific date.

   if ( $archive_date == 0 ) {

      opendir( DIR, $repack_dir ) || die "can't opendir $repack_dir: $!";

      if ( $no_prep_id ) {
         @archived_files = grep { /^$repack_tag/ } readdir( DIR );
      } else {
         @archived_files = grep { /^$repack_tag\.$prep_ID/ } readdir( DIR );
      }

      closedir( DIR );

   } else {

      if ( $no_prep_id ) {
         @archived_files = ( "$repack_tag.t$archive_date" );
      } else {
         @archived_files = ( "$repack_tag.$prep_ID.t$archive_date" );
      }

   }

# Process each file.

   foreach $sfile ( @archived_files ) {

# Find the directory this file should go in.

      @nodes = split( /\./, $sfile );

      $type_tag = $nodes[0];
      ( $time_tag = $nodes[$#nodes] ) =~ s/^t//;

      $subdir = join( "/", $prep_ID, "repack", $type_tag, mmm_yr( $time_tag/100 ) );

# Check if the location to archive to is a remote location.  If so, use the
# remote put function.  Otherwise, just do a copy.  Make the directory first.
# File and directory permission modes are also explicitly set.  Note that mkpath
# interacts with the umask, so even though it defaults to 0777, it must be reset
# to what we want.

      $err_trans = 0;

      if ( $archive_loc =~ /:/ ) {
         mkdir_remote( "$archive_loc$subdir" ) or $err_trans = 1;
         chmod_remote( "$archive_loc$subdir", '0755', { 'recursive' => 1 } ) or $err_trans = 1;
         rput( "$repack_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
      } else {
         if ( ! -d "$archive_loc$subdir" ) {
            mkpath( "$archive_loc$subdir" ) or $err_trans = 1;
            chmod( 0755, "$archive_loc$subdir" ) or $err_trans = 1;
         }
         copy( "$repack_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
         chmod( 0644, "$archive_loc$subdir/$sfile" ) or $err_trans = 1;
      }

      if ( $err_trans ) {
         print STDERR "(conv_archive) ERROR - Could not transfer $repack_dir/$sfile to $archive_loc$subdir","\n";
      } else {
         ++ $files_trans;
         unlink "$repack_dir/$sfile";
      }

   }

   return( $files_trans );

}

#============  Archive the "raw" data sets ======================

sub raw_archive {

# This module contains the copy() subroutine.

   use File::Copy;

# This module contains the mkpath() subroutine.

   use File::Path;

# Any arithmetic done in this subroutine is done as integer.

   use integer;

# This module contains the rput(), chmod_remote() and mkdir_remote() subroutines.

   use Remote_utils;

# This module contains the mmm_yr() subroutine.

   use Manipulate_time;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[$#_] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Working directory for the preprocessing.  The raw files will be 
# archived from a "raw_storage" subdirectory in this directory.

   if ( defined( %options ) && exists( $options{"conv_work_dir"} ) ) {
      $conv_work_dir = $options{"conv_work_dir"};
   } else {
      $conv_work_dir = ".";
   }

# Option indicating that prep ID is not included on the file names.

   if ( defined( %options ) && exists( $options{"no_prep_id"} ) ) {
      $no_prep_id = $options{"no_prep_id"};
   } else {
      $no_prep_id = 0;
   }

# Arguments

   my $prep_ID     = $_[0];
   my $archive_loc = $_[1];
   my $archive_date;

   if ( $#_ >= 3 ) {
      $archive_date = $_[3];
   } else {
      $archive_date = 0;
   }

# If the archive location does have a "@", but does not have a ":",
# then assume it's a remote location of the form "userid@machine" and 
# just put a ":" at the end.  The subdirectory will be appended, and the path 
# will be relative to the user's home directory on that machine.

   if ( $archive_loc =~ /@/ && $archive_loc !~ /:/ ) { $archive_loc = "$archive_loc:"; }

# If the archive location does not end with a ":" or a "/", then assume some path 
# has been given, and put a "/" there to allow later appending of the subdirectory.

   if ( $archive_loc !~ m'[:/]$' ) { $archive_loc = "$archive_loc/"; }

   $files_trans = 0;

   $raw_dir = "$conv_work_dir/raw_storage";

# Get list of raw files to be staged, if no particular date has
# been indicated, otherwise get that specific date.

   if ( $archive_date == 0 ) {

      opendir( DIR, $raw_dir ) || die "can't opendir $raw_dir: $!";

      if ( $no_prep_id ) {
         @archived_files = grep { /^[^\.]+\.t/ } readdir( DIR );
      } else {
         @archived_files = grep { /^[^\.]+\.$prep_ID/ } readdir( DIR );
      }

      closedir( DIR );

   } else {

      opendir( DIR, $raw_dir ) || die "can't opendir $raw_dir: $!";

      if ( $no_prep_id ) {
         @archived_files = grep { /^[^\.]+\.t$archive_date/ } readdir( DIR );
      } else {
         @archived_files = grep { /^[^\.]+\.$prep_ID\.t$archive_date/ } readdir( DIR );
      }

      closedir( DIR );

   }

# Process each file.

   foreach $sfile ( @archived_files ) {

# Find the directory this file should go in.

      @nodes = split( /\./, $sfile );

      $type_tag = $nodes[0];

      if ( $type_tag eq "on29" ) {

         ( $time_tag = $nodes[$#nodes-1] ) =~ s/^d//;
         $yr = sprintf( "%02d", $time_tag/10000 );
         $mm = sprintf( "%02d", ( $time_tag/100 ) % 100 );
         $subdir = join( "/", $prep_ID, "on29_obs", "Y" . $yr, "M" . $mm );

      } elsif ( $type_tag eq "tovs" || $type_tag eq "rtovs" ) {

         ( $time_tag = $nodes[$#nodes] ) =~ s/^d//;
         $yr = sprintf( "%02d", $time_tag/10000 );
         $mm = sprintf( "%02d", ( $time_tag/100 ) % 100 );
         $subdir = join( "/", $prep_ID, "bufr_nesdis_rets", "Y" . $yr, "M" . $mm );

      } else {

         print STDERR "(raw_archive) $sfile not archivable\n";
         next;

      }

# Check if the location to archive to is a remote location.  If so, use the
# remote put function.  Otherwise, just do a copy.  Make the directory first.
# File and directory permission modes are also explicitly set.  Note that mkpath
# interacts with the umask, so even though it defaults to 0777, it must be reset
# to what we want.

      $err_trans = 0;

      if ( $archive_loc =~ /:/ ) {
         mkdir_remote( "$archive_loc$subdir" ) or $err_trans = 1;
         chmod_remote( "$archive_loc$subdir", '0755', { 'recursive' => 1 } ) or $err_trans = 1;
         rput( "$raw_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
      } else {
         if ( ! -d "$archive_loc$subdir" ) {
            mkpath( "$archive_loc$subdir" ) or $err_trans = 1;
            chmod( 0755, "$archive_loc$subdir" ) or $err_trans = 1;
         }
         copy( "$raw_dir/$sfile", "$archive_loc$subdir" ) or $err_trans = 1;
         chmod( 0644, "$archive_loc$subdir/$sfile" ) or $err_trans = 1;
      }

      if ( $err_trans ) {
         print STDERR "(conv_archive) ERROR - Could not transfer $raw_dir/$sfile to $archive_loc$subdir","\n";
      } else {
         ++ $files_trans;
         unlink "$raw_dir/$sfile";
      }

   }

   return( $files_trans );

}

1;
