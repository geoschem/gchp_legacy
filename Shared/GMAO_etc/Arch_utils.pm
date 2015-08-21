package Arch_utils;
use Exporter;

@ISA = ('Exporter');
@EXPORT = (gen_archive);

#========== Archive data to local or remote machine =============
#
#  gen_archive - General data archiver.  This script was built 
#    from the 2.5.4 subroutine "conv_archive" (Conv_utils.pm) 
#    which archived the unpack and repack files to mass storage 
#    at the end of the conventional data processing.  While
#    "conv_archive" was tailored to work only with conventional
#    processing, this subroutine is general and can handle any
#    type of data.  It will be archived to the standard directory
#    hierarchy used by the DAO for observational data. (CR #226).
#    That hierarchy is:
#
#    /base/<job-type>/<source>/<format>/[subtype]/year/month/file
#    i.e., /u2/dao_ops/flk/wssmi_wind/ods/Y1999/M06/... 
#               [ <subtype> is optional ]
#
#    INPUTS:
#              environment (ops,val,test)
#              prep-id (flk, llk)
#              observation-type  (wssmi_wind, nssmi_tpw, conv)
#              format of data to archive (ODS, native, ON29)
#              valid date of data (YYYYMMDD)
#              archive_location & base dir (dao_ops@gatun:/u2/dao_ops)
#              filename(s) (tpw.t19990602,/scratch1/lucchesi/ssmi.t19990704)
#    
#    OPTIONAL INPUT:  
#              HASH table options argument:
#                 delete => 0  (Default) Do not delete local files
#                 delete => 1  Delete local files after archiving
#                 direct => 1  (Default) Direct transfers - No ATM/NREN hopping
#                 direct => 0  Use ATM/NREN when available
#                 remdel => 0  (Default) Do not delete remote file before transfer
#                 remdel => 1  Delete remote file before transfer (avoid dmget on overwrites)
#                 remote_name => "string" remote name (default is local name).
#                 subtype => "string" For file formats with a "subtype".
#                 verbose => 1  Print diagnostic information
#                 y4only  => 0 (Default) Date must be 8 characters long
#                 y4only  => 1 date for subdir contains only year - no month 
#                 run_config => "string" Run_config file path and name (default = DEFAULT)
#
#    RETURNS:
#              number of files transferred
#
#    NOTES:    Currently, filename(s) must either be fully-qualified
#              or the gen_archive working directory must be the same 
#              directory that contains the file(s) to archive.  Thus, a 
#              PERL program calling gen_archive with relative pathnames
#              should CD to the directory with the data files first.
#
#    USES:
#          File:Copy File:Path modules (standard)
#          Remote_utils module (DAO)           
#
#    EXAMPLE CALL:
#
#    gen_archive ('ops','llk','wentz_ssmi','ods','19990701',
#                 'lucchesi@gatun:/silo1/stage2/lucchesi',
#                 file1,file2,file3,{ 'subtype' => "wind",
#                 'verbose' => 1 } );
#
#    AUTHOR/REVISIONS:  Rob Lucchesi  
#         Initial                                    06/14/99 RL
#         Added 'delete' option                      06/15/99 RL
#         Added 'subtype' option, new dir structure  06/17/99 RL
#         Added 'environment' parameter, verbose     07/20/99 RL
#         Added 'direct','remdel','remote_name' options 02/27/01 TO
#         Added 'run_config' option                  04/03/01 TO
#         Added 'exp_path' option                    04/20/07 TO
#
#    HISTORY:
#         Evolved from Dave Lamich's "conv_archive" subroutine.
#
#=======================================================================

sub gen_archive {

# This module contains the copy() subroutine.

   use File::Copy;

# This module contains the basename(), dirname()

   use File::Basename; 

# This module contains the mkpath() subroutine.

   use File::Path;

# Any arithmetic done in this subroutine is done as integer.

   use integer;

# Contains rput(), chmod_remote() and mkdir_remote() subroutines.

   use Remote_utils;

# Get options (from last argument, if it is a hash reference).

   if ( ref( $_[$#_] ) eq "HASH" ) {
      %options = %{ pop( @_ ) };
   }

# Load arguments to local variables.

   my $environment = shift;      # i.e., ops
   my $prep_id     = shift;      # i.e., llk
   my $obs_type    = shift;      # i.e., wentz_ssmi
   my $format      = shift;      # i.e., ods
   my $date        = shift;      # i.e., 19990615 or 1999061512 or 1999
   my $archive_loc = shift;      # i.e., dao_ops@gatun:/silo1/dao_ops
   @archived_files = @_;         # filenames to archive


# PROCESS THE OPTIONS FROM THE HASH TABLE
# Option to specify a Run_Config file location

   if ( defined( %options ) && exists( $options{'run_config'} ) ) {
      $run_config = $options{'run_config'};
   }
   else {
      $run_config = DEFAULT;
   }


# Option indicating that prep ID is not included on the file names.

   if ( defined( %options ) && exists( $options{"no_prep_id"} ) ) {
      $no_prep_id = $options{"no_prep_id"};
   }
   else {
      $no_prep_id = 0;
   }

#  Option to delete local file(s) after archive step.

   if ( defined( %options ) && exists( $options{"delete"} ) ) {
      $delete = $options{"delete"};
   }
   else {
      $delete = 0;
   }

#  Verbose option

   if ( defined( %options ) && exists( $options{"verbose"} ) ) {
      $verbose = $options{"verbose"};
   }
   else {
      $verbose = 0;
   }

#  Option to include subtype in the file path.

   if ( defined( %options ) && exists( $options{"subtype"} ) ) {
      $subtype = $options{"subtype"};
   }
   else {
      $subtype = $obs_type;
   }

#  Option to specify different remote file name
   if ( defined( %options ) && exists( $options{"remote_name"} ) ) {
      if (! $#archived_files ) {
          $remote_name = $options{"remote_name"};
          $use_remote  = 1;
      }
      else{
          warn "Can't specify remote name for multiple file transfers.\n";
          $use_remote  = 0;
      }
   } 
   else {
      $use_remote  = 0;
   }

#  Option to specify deletion of remote file before transfer
   if ( defined( %options ) && exists( $options{"remdel"} ) ) {
       $remote_delete = $options{"remdel"};
   }
   else {
       $remote_delete = 0;
   }

# Option to specify file permissions

   if ( defined( %options ) && exists( $options{"filemode"} ) ) {
       $filemode = $options{"filemode"};
   }
   else {
       $filemode = '0644';
   }

# Option to specify directory permissions 

   if ( defined( %options ) && exists( $options{"dirmode"} ) ) {
       $dirmode = $options{"dirmode"};
   }
   else {
       $dirmode = '0755';
   }


# Option to specify y4only date

   if ( defined( %options ) && exists( $options{"y4only"} ) ) {
       $y4only = $options{"y4only"};
   }
   else {
       $y4only = 0;
   }

# Option to specify no ATM/NREN hopping
   if ( defined( %options ) && exists( $options{"direct"} ) ) {
       $direct = $options{"direct"};
   }
   else {
       $direct = 1;
   }

# Option to specify explicit archive path
   if ( defined( %options ) && exists( $options{"exp_path"} ) ) {
       $explicit_path = $options{"exp_path"};
   }
   else {
       $explicit_path = 0;
   }

 
# DETERMINE ARCHIVE LOCATION

   if ( $archive_loc =~ /@/ && $archive_loc !~ /:/ ) { 
      $archive_loc = "$archive_loc:"; 
   }

# If the archive location does not end with a ":" or a "/", then assume 
# some path has been given, and put a "/" there to allow later appending 
# of the subdirectory.

   if ( $archive_loc !~ m'[:/]$' ) { 
      $archive_loc = "$archive_loc/"; 
   }

   $files_trans = 0;

# Parse date information.  
 
   if ((length($date) != 8)&&( ! $y4only )) {
      print "gen_archive: Date must be 8 characters (YYYYMMDD)\n";
      return -1;
   }
   $year  = substr ($date, 0, 4);
   if (! $y4only) {
       $month = substr ($date, 4, 2);
   } 
# Process each file.

   foreach $sfile ( @archived_files ) {
      if ($y4only) {
           $subdir = join("/",$environment,$prep_id,$obs_type,$format,
                          $subtype,"Y$year");
      }else{
           $subdir = join("/",$environment,$prep_id,$obs_type,$format,
                          $subtype,"Y$year","M$month");
      }
      if ($explicit_path){
          $subdir ="";
      }
      if (! $use_remote ) { 
          $remote_name = basename($sfile);
      }
      if ($verbose) {
        print "Will put $sfile as $archive_loc$subdir\/$remote_name\n";
      }


# Check if the location to archive to is a remote location.  If so, use the
# remote put function.  Otherwise, just do a copy.  Make the directory first.
# File and directory permission modes are also explicitly set.  Note that mkpath
# interacts with the umask, so even though it defaults to 0777, it must be reset
# to what we want.

      $err_trans = 0;
      if ( $archive_loc =~ /:/ ) {
         if ($remote_delete) {
             %options = ( 'exist'      => 1,
                          'debug'      => $verbose,
                          'run_config' => $run_config );
             @rc=rflist("$archive_loc$subdir/$remote_name",\%options);
             if ($rc[0]) {
                       %options = ( 'debug'      => $verbose,
                                    'run_config' => $run_config );
                       $rc=rm_remote_file("$archive_loc$subdir/$remote_name", \%options);
                       if ($verbose) {
                           print "rm_remote_file returned $rc\n";
                       }
             }
         }
         %options = ( 'exist'      => 1,
                      'debug'      => $verbose,
                      'run_config' => $run_config );
         @rc=rflist("$archive_loc$subdir",\%options);
         if (! $rc[0]) {
              print "Creating remote directory: $archive_loc$subdir .\n";

              %options = ( 'debug'      => $verbose,
                           'run_config' => $run_config );
              mkdir_remote( "$archive_loc$subdir", \%options ) or $err_trans = 1;
              print "err_trans = $err_trans\n"; 
              %options = ( 'recursive'  => 1,
                           'verbose'    => $verbose,
                           'run_config' => $run_config );
              chmod_remote( "$archive_loc$subdir", $dirmode, \%options) or $err_trans = 2;
              print "err_trans = $err_trans\n";
         }
         print "run_config = $run_config\n";

         %options = ( 'direct'     => $direct,
                      'mode'       => $filemode,
                      'run_config' => $run_config,
                      'debug'      => $verbose ); 
         rput("$sfile", "$archive_loc$subdir/$remote_name",  \%options) or $err_trans = 3;
         print "err_trans = $err_trans\n";
      } 
      else {
         if ( ! -d "$archive_loc$subdir" ) {
            mkpath( "$archive_loc$subdir" ) or $err_trans = 4;
            chmod( oct($dirmode), "$archive_loc$subdir" ) or $err_trans = 5;
         }
         if ( ($remote_delete) && ( -e "$archive_loc$subdir/$remote_name") ){
              unlink ("$archive_loc$subdir/$remote_name") or $err_trans = 6;
         }
         copy( "$sfile", "$archive_loc$subdir/$remote_name" ) or $err_trans = 7;
         chmod( oct($filemode), "$archive_loc$subdir/$remote_name" ) or $err_trans = 8;
      }
      if ( $err_trans ) {
         print STDERR "(gen_archive) ERROR $err_trans - Could not transfer $sfile to $archive_loc$subdir/$remote_name","\n";
      } 
      else {
         ++ $files_trans;
         if ($delete) {
           unlink "$sfile";
         }
      }
   }
   return( $files_trans );
}

#=================  Stage the repack data  ======================

sub gen_stage {

# This module contains the copy() subroutine.

   use File::Copy;

# This module contains the mkpath() subroutine.

   use File::Path;

# This module contains the rput(), chmod_remote() and mkdir_remote() subroutines.

   use Remote_utils;

}
