   Program fv2prs

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  fv2prs --- converting eta (lcv) files to pressure for FVDAS
!
!
! !USAGE: see the routine usage() below
!
! !USES:
!
   use m_set_eta, only: set_eta
   use m_die
   use m_inpak90
   use m_StrTemplate
   use m_interp
   use m_chars
   use m_const, only: undef,cpm,grav,kappa,zvir

   Implicit NONE

! !DESCRIPTION: converting eta (lcv) files to pressure for FVDAS         
!
! !REVISION HISTORY:
!
!  May2001  da Silva  Initial design and prototyping.
!  Jun2001  Baoyu Yin Implementation
!  Jul2001  Baoyu Yin Modification to remove units from long names in the
!                     metadata.
!  Aug2001  Baoyu Yin Modified to accept -psf and -date option
!  Nov2001  Baoyu Yin Variables are case-sensitive.
!  Jan2002  Baoyu Yin "lcv" or "eta" options are not longer needed. If no
!                     options specified from command lines, all variables
!                     in the input file will be convert.  
!  Apr2002  Baoyu Yin Added vertical levels checking and modified km assignments.
!  Apr2002  Baoyu Yin Added -cvs option for CVS version.
!  May2002  Baoyu Yin Added handling for a new table for variable description 
!                     (long names) from resource files.
!  Jun2002  Baoyu Yin Added handling for vertical orientation.
!  Jun2002  Baoyu Yin Modified starting time for time interpolation. The starting
!                     time will be the first time step from input file for that day.
!                     Added date increment if the date goes to next day.
!                     Modified time index in GFIO_GetVarT for "delp"
!  Jul2002  Baoyu Yin Fixed bugs about vwnd and -levels option for HDF EOS
!  Aug2002  Baoyu Yin Added a new column to the .rc files to indicate that
!                     a variable should be non-negative or not (1 or 0).
!  Jan2003  Baoyu Yin Added '-help' option and modified Usage() message.
!  Jan2003  Baoyu Yin Modified back to case-insensitive for variables
!  Jan2003  Baoyu Yin Re-arranged the formula to compute RH.
!  Feb2003  Baoyu Yin Added new option 'alt' to convert eta coordinate to
!                     height coordinate.
!  APR2003  Baoyu Yin Modified to reduce the number of "false errors"
!  MAY2003  Baoyu Yin Added -f option to overwrite output file. The -o option will
!                     no longer overwrite output file. It will stop if there is an
!                     output file existed. Modified some error messages to be more
!                     specific.
!  JUN2003  Baoyu Yin -bkg option was added in the command line for Ts time interpolation
!                     In the .rc files, a new column needs to add to indicate whether
!                     a variable uses the new interpolation method. 
!  JUL2003  Baoyu Yin Increased file name length for psf. Added ps file name template
!  Sep2003  Baoyu Yin Added -start option
!  Oct2003  Baoyu Yin Added support for lwgdown and lwgup
!  May2004  Baoyu Yin Fixed memory leak.
!  Sep2004  Baoyu Yin Modified the main program to make it concise.
!02Nov2004  Todling   Removed dependency on FVGCM; getting set_eta from m_mapz
!  Oct2011  Lucchesi  Added -lonshift option which should be used if input data is 
!                     [-180 to 180], because the "Interp_Field" routine returns data
!                     at [0 360].  This was not a problem with GEOS-4 data.
!27Jan2014 Todling    Some sync with m_const/MAPL_const
!--------------------------------------------------------------------------------
!EOP


   character(len=*), parameter :: myname = 'fv2prs'

  
!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------

      integer, parameter :: mFiles = 256       ! Max. number of input files
      integer, parameter :: mVars  = 256       ! Max. number of variables
      integer, parameter :: mLevs  = 256       ! Max. number of levels    
      integer, parameter :: mPsfs  = 256       ! Max. number of surface files for 3D mix_P    
      integer, parameter :: mDer3dVars  = 6    ! Max. number of dervied 3D variables
      integer, parameter :: mDer2dVars  = 6    ! Max. number of dervied 2D variables
      integer, parameter :: km_std = 36        ! Vertical number of levels if no variables
                                               ! are specified 

!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


      integer            :: nFiles             ! Actual number of input files
      character(len=255) :: inFiles(mFiles)    ! Input file names
      character(len=255) :: inFile1            ! Input file names
      character(len=255) :: inFile2            ! Input file names
      character(len=255) :: outFile            ! Output file name 
      character(len=255) :: outFile1           ! Output file name 
      character(len=255) :: cvsFile            ! CVS file name containing CVS version
      character(len=255) :: cvsV               ! CVS version

      integer  :: cvsV_yes                     ! have a cvs version from command line
      integer  :: date                         ! yyyymmdd -- user specified from command line
      integer  :: date1                        ! yyyymmdd -- user specified from command line
      integer  :: begTime                      ! yyyymmdd -- user specified from command line
      integer  :: startTime                    ! yyyymmdd -- user specified from command line
      integer  :: inc_hhmmss                   ! increment hours specified from command line
      integer  :: curTime                      ! temp variable for time increment
      integer  :: im                           ! zonal dimension
      integer  :: jm                           ! meridional dimension
      integer  :: km                           ! vertical dimension

      real, pointer     :: lon(:)              ! longitudes in deg (im)
      real, pointer     :: lat(:)              ! latitudes in deg (jm)
      real, pointer     :: lev(:)              ! levels in hPa (km)
      real              :: lev_std(36) = (/1000.,975.,950.,925.,900.,875.,850.,825.,800.,750., &
                           700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,   &
                           50.,40.,30.,20.,10.,7.,5.,3.,2.,1.,0.4,0.2 /) 
!                                              ! levels in hPa (km)
 
      integer           :: nLevs = 0           ! total number of levels
      real, pointer     :: Levs(:)             ! vertical levels
      character(len=256) :: cLevs(mLevs)        ! Character reprsentation of levels

      integer           :: nPsfs = 0           ! total number of input psf files
      character(len=255):: srfFiles(mPsfs)     ! input psf files

      integer           :: nbkg = 0            ! total number of input psf files
      character(len=255):: bkgFiles(mPsfs)     ! input psf files

      integer           :: nVars               ! Actual number of variables
      character(len=64) :: inVars(mVars)       ! Input  variable names (nVars)
      character(len=64) :: outVars(mVars)      ! output variable names (nVars)
      character(len=64) :: outUnits(mVars)     ! Units of output variables (nVars)
    
                                               ! Unit conversion factors:
                                               ! OUT = scaleFactor * IN + addOffset
      real :: scaleFactor(mVars)               ! scaling (nVars) 
      real :: addOffset(mVars)                 ! offset (nVars)
      integer :: isPos(mVars)                  ! should be positive?
      integer :: doInterp(mVars)               ! should be positive?

      character(len=3) :: inType               ! Input file type: 'eta' or 'lcv'
      character(len=3) :: outType              ! Output file type: 'prs' or 'alt'

      integer          :: outPrec              ! Output file precision:
                                               ! 0 = 32 bits,  1 = 64bits


!                              -----------------------
!                                Variable Work Space
!                              -----------------------

      real, pointer ::  inField(:,:,:)         ! Input variable
      real, allocatable ::  inFieldtmp(:,:,:)  ! Input variable
      real, pointer ::  inFieldtmpv(:,:,:)     ! Input variable
      real, pointer ::  inFieldw(:,:,:)        ! Input variable for V-wind
      real, pointer ::  write_out(:,:,:)       ! Input variable
      real, pointer ::  write_outw(:,:,:)      ! Input variable for V-wind
      real, pointer :: outField(:,:,:)         ! Onput variable
      real, pointer :: alt(:,:,:)              ! Onput variable
      real, pointer :: alt1(:,:,:)              ! Onput variable
      real, pointer :: outFieldw(:,:,:)        ! Onput variable for V-wind
      real,  pointer ::  ps(:,:)               ! sfc pressure
      real,  pointer ::  phis(:,:)
      real,  pointer ::  wz(:,:,:)
      real,  pointer ::  slp(:,:)
      real,  pointer ::  delp(:,:,:)           ! pressure thickness
      real,  pointer ::  work(:,:,:)           ! working arrary for vertical flip
      real,  pointer ::  pt(:,:,:)             ! temperature
      real,  pointer ::  sphu(:,:,:)           ! 
      real,  pointer ::  pk(:,:,:)             ! 
      real,  pointer ::  pkz(:,:,:)            ! 
      real,  pointer ::  peln(:,:,:)           ! 
      real, allocatable :: pmk(:,:)            ! working array for pm
      real, allocatable :: tfield(:,:)         ! working array for tmpu
      real, allocatable :: qsfield(:,:)        ! working array for RH
      real, allocatable :: arg1(:,:,:)         ! working array for preacc, osr, osrolr
      real, allocatable :: arg2(:,:,:)         ! working array for preacc, osr, osrolr
      real, allocatable :: arg3(:,:,:)         ! working array for lwgup
      type(int_grid) :: grid                   ! Output grid variable
      type(int_grid) :: grid1                  ! Output grid variable
      integer        :: nobs                   ! im*jm*km
      real, pointer  :: lon3d(:)               ! longitudes in deg (im*jm*km)
      real, pointer  :: lat3d(:)               ! latitudes in deg (im*jm*km)
      real, pointer  :: lev3d(:)               ! levels in hPa (im*jm*km)
      real, pointer  :: lon3d_e(:)             ! longitudes in deg (im*jm*km)
      real, pointer  :: lat3d_e(:)             ! latitudes in deg (im*jm*km)
      real, pointer  :: lev3d_e(:)             ! levels in hPa (im*jm*km)
      real, pointer  :: conf(:)                ! Confidence level (im*jm*km)
      character(len=257)  :: var_name           ! Input variable name
      character(len=257)  :: var_name_out       ! Onput variable name
      character(len=257)  :: v_name             ! Input variable name for V-wind
      character(len=257)  :: v_name_out         ! Onput variable name for V-wind
      character(len=64) :: inVarsg(mVars)      ! Input variable names for GFIO (tNames)
      character(len=64) :: outVarsg(mVars)     ! Output variable names for GFIO (tNames)
      character(len=64) :: outVarsgc(mVars)    ! Output variable names from rc.name table 
      character(len=64) :: outUnitsg(mVars)    ! Units of output variables for GFIO 
      character(len=6)  :: der3dVars(mDer3dVars)  = (/"HGHT  ","ALT   ","DENS  ","RH    ", "TMPU  ","DTRAIN"/) 
                                               ! Dervied 3D variable names
      character(len=7) :: der2dVars(mDer2dVars)  = (/"PREACC ", "SLP    ", "OSR    ", "OSRCLR ", "LWGDOWN", "LWGUP  "/) 
                                               ! Dervied 2D variable names
      integer           :: tNames              ! Total number of the GFIO variables
      real, pointer     :: ak(:)               ! model eta level parameter a
      real, pointer     :: bk(:)               ! model eta level parameter b
      integer           :: ks                  ! interface level (not needed)
      real              :: ptop                ! top pressure level (not needed)
      real              :: pint                ! interface pressure level (not needed)
      integer           :: nlist               ! number of input variables for 
                                               ! computing hght, tmpu, slp and rh.
      character(len=257) :: list(5)             ! array contains the above variables
      character(len=257) :: name_tmp = ' '
      character(len=257) :: pt_name
      character(len=257) :: ze_name
      character(len=257) :: sphu_name
      character(len=257) :: ts_name
      character(len=257) :: ps_name
      character(len=257) :: delp_name
      character(len=257) :: phis_name
      character(len=257) :: tmp_name1, tmp_name2, op, op3
      character(len=257) :: tmp_lwgup1, tmp_lwgup2, tmp_lwgup3
      character(len=257) :: tmp_lwgdown1, tmp_lwgdown2
      character(len=257) :: tmp_osr1, tmp_osr2, op1
      character(len=257) :: tmp_osrclr1, tmp_osrclr2, op2
      integer           :: imPs, jmPs, kmPs, lmPs, nvarsPs, ngattsPs, ips
      character(len=255) :: titlePs            ! meta data title
      character(len=255) :: sourcePs           ! data source
      character(len=255) :: contactPs          ! contact org.   
      character(len=255) :: levunitsPs         ! Vertical levels
      character(len=255) :: vNamePs(mVars)       ! output variable names (nVars)
      character(len=255) :: vtitlePs(mVars)      ! output title
      character(len=255) :: vunitsPs(mVars)      ! output vertical level unit
      integer, pointer :: yyyymmddPs(:)          ! Date
      integer, pointer :: hhmmssPs(:)            !
      integer          :: timincPs             ! Time increment
      real              :: amissPs             ! Missing value
      real, pointer     :: lonPs(:)            ! longitudes in deg (im)
      real, pointer     :: latPs(:)            ! latitudes in deg (jm)
      real, pointer     :: levPs(:)            ! levels in hPa (km)
      integer, pointer  :: kmVarPs(:)          ! Number of vertical levels for variables
      logical           :: found_ps
      logical           :: ex = .false.
      logical           :: isForced = .false.  ! Force to overwrite output file
      logical           :: lonshift = .false.
      character(len=8)  :: char_date
      character(len=6)  :: char_time
      real              :: valid_rangePs(2, mVars)
      real              :: packing_rangePs(2, mVars)

!                                  Local Work Space
!                              -----------------------

      integer ibkg,ipsf, iff, it, iv, id, lm, num_levs, out_lev, itest, nnames, ii, i, j, k
      integer        :: name_len, t_len 
      integer        :: name_pos 
      logical        :: wind_field = .false.
      logical        :: compu_hght = .false.
      logical        :: compu_alt = .false.
      logical        :: compu_dens = .false.
      logical        :: compu_rh = .false.
      logical        :: compu_tmpu = .false.
      logical        :: compu_slp = .false.
      logical        :: eta_file = .false.
      logical        :: allVars = .false.
      logical        :: doRev = .false.
      integer  ymdTmp, ymdTmp1, hmsTmp1, ymdTmp2, hmsTmp2
      logical        :: templt = .false.
      character(len=255):: srfFileTmp1
      character(len=255):: srfFileTmp2
      real           ::  buf(56)

!                              -----------------------
!                                  Output Meta Data
!                              -----------------------

      character(len=255) :: title              ! meta data title
      character(len=255) :: source             ! data source
      character(len=255) :: title_cvs          ! meta data title
      character(len=255) :: source_cvs         ! data source
      character(len=255) :: contact            ! contact org.   
      character(len=255) :: levunits           ! Vertical levels
      real               :: missing_val

      integer          :: in_fmode = 1         ! non-zero for READ-ONLY
      integer          :: out_fmode = 0        ! 0 for READ-WRITE 
      integer          :: fid                  ! input file ID
      integer          :: fid2                 ! input file ID for second file
      integer          :: fidb                 ! tmp input file ID 
      integer          :: fidbp                ! input file ID for previous bkg file
      integer          :: fidbc                ! input file ID for current bkg file
      integer          :: fidbn                ! input file ID for next bkg file
      integer          :: fid_srf1             ! input file ID for srfFile
      integer          :: fid_srf2             ! input file ID for srfFile
      integer          :: out_fid              ! output file ID
      integer          :: rc, rc1              ! return error code

      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            ! 
      integer, pointer :: yyyymmdd2(:)          ! Date
      integer, pointer :: hhmmss2(:)            ! 
      integer          :: timinc2              ! Time increment
      integer          :: lm2                  ! num. of time steps in fid2
      integer, pointer :: hhmmss_bkg(:)        ! 
      integer          :: timinc               ! Time increment
      integer          :: timinc_save          ! Time increment
      integer          :: time_save            ! Current Time 

      character(len=257) :: vName(mVars)       ! output variable names (nVars)
      character(len=257) :: vtitle(mVars)      ! output title
      character(len=257) :: vunits(mVars)      ! output vertical level unit
      integer            :: outKm(mVars)       ! number of levels for variables;
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)


!                              -----------------------
!                                  eta information 
!                              -----------------------

      integer           :: im_e                ! input zonal dimension       
      integer           :: jm_e                ! input meridional dimension       
      integer           :: km_e                ! input vertical dimension    
      integer           :: lm_e                ! input time dimension    
      integer           :: nVars_e             ! input number of variables   
      real              :: amiss               ! Missing value
      real, pointer     :: lon_e(:)            ! longitudes in deg (im)
      real, pointer     :: lat_e(:)            ! latitudes in deg (jm)
      real, pointer     :: lev_e(:)            ! levels in hPa (km)
      integer, pointer  :: kmVar_e(:)          ! Number of vertical levels for variables

      character(len=257) :: vtitle_in(mVars)   ! output title
      character(len=257) :: longName(mVars)    ! output title
      real              :: valid_range(2, mVars)
      real              :: packing_range(2, mVars)
      integer           :: ngatts              ! Number of attributes for GFIO
!.................................................................................

!  Get user input
!  --------------
   call  Init_ ( mFiles, nFiles, inFiles, outFile, cvsFile,&
                 im, jm, km, lon, lat, lev, nLevs, Levs,   &
                 mVars, nVars, inVars, outVars, outUnits,  &
                 scaleFactor, addOffset, isPos, inType,    &
                 outType, outPrec,nPsfs, srfFiles, nbkg,   &
                 bkgFiles, doInterp, date,                 &
                 inc_hhmmss, startTime, outVarsgc,         &
                 longName, isForced, lonshift )

   call getCvsVersion()

!  Loop over input files ...
!  -------------------------
   do iff = 1, nFiles

!     Open GFIO file
!     --------------
      call openFile()

!     Determine on file
!     ------------------
      call getMetaData()

!     Construct GFIO output variable names for GFIO_Create
      call getOutputVarName()

!     Construct GFIO input variable names
      call getInputVarName()

!     GFIO output file is created only once.  
      if ( iff == 1 ) then
         call createFile()
      end if

!     Set start time from command line option or file
      if ( date /= 99999999 .and. iff .eq. 1 ) then
         curTime = begTime
      elseif (  iff .eq. 1 ) then
         curTime = hhmmss(1)
      end if
         
!     Loop over times on file
!     -----------------------
      do it = 1, lm_e
 
         if ( date /= 99999999 ) then
            call checkEndDateTime()
            if (yyyymmdd2(lm2) .lt. date .or. (yyyymmdd2(lm2) .eq. date  &
               .and. hhmmss2(lm2) .lt. curTime) ) cycle
         end if 

!        Check time for time interpolation.
         if ( date .eq. 99999999 ) then 
            print *, 'Run in normal mode'
         else
            if ( date .lt. yyyymmdd(it) .or. date1 .gt. date ) cycle
         end if
         time_save = hhmmss(it)
         if ( startTime /= 99999999 .and. date .eq. yyyymmdd(lm_e) .and. &
             curTime .gt. (timinc_save+hhmmss(lm_e))) then
            exit
         endif

!        This do loop is for time increment
!        ----------------------------------
         do
            if ( date /= 99999999 ) then
               if (yyyymmdd2(lm2) .lt. date .or. (yyyymmdd2(lm2) .eq. date  &
                  .and. hhmmss2(lm2) .lt. curTime) ) exit
            end if

            call initializeGrid()

!           Loop over variables
!           -------------------
            do iv = 1, nVars 
               call readVar()

!              do interpolation
!              ----------------
               call Interpolation()

!              Fix units
!              ---------
               call fixUnits()

!              Write interpolate variable to output file
!              -----------------------------------------           
               call writeOut()

               call deallocateMemory()
            end do  ! variables

!           memory clean up 
            call Interp_Clean( grid )
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') call Interp_Clean( grid1 )

!           check time interpolation
!           ------------------------
            curTime = curTime + timinc    
            if ( curTime .ge. 240000 ) then
               curTime = curTime -240000
               date1 = INCYMD(date, 1)
               exit
            end if
            if ( curTime .ge. (time_save + timinc_save) ) exit
            if ( it .eq. lm_e .and. timinc_save .eq. timinc) exit
            if ( curTime .ge. hhmmss(it+1) .and.               &
                 yyyymmdd(it) .eq. yyyymmdd(it+1) .and.        &
                 it .lt. lm_e ) exit
     
         end do ! do loop
      end do ! times loop

      deallocate ( ps )
      deallocate ( delp, work )
      deallocate ( ak, bk, lon_e, lat_e, lev_e, kmVar_e )
      deallocate (yyyymmdd, hhmmss)

!     Close input file
!     ----------------
      call GFIO_Close ( fid, rc )
      if ( iff < nFiles ) then
         call GFIO_Close ( fid2, rc )
         if (nbkg > 0) then
            call GFIO_Close(fidbc, rc)
            if (fidbp /= fidbc) call GFIO_Close(fidbp, rc)
            if (fidbn /= fidbc) call GFIO_Close(fidbn, rc)
         end if
      end if 

   end do ! input files

!  Close output file
!  ----------------
   call GFIO_Close ( out_fid, rc )


!  All done
!  --------
   call exit(0)

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_ --- Parses command line and loads resource file
! 
! !INTERFACE:
!
   subroutine Init_ ( mFiles, nFiles, inFiles, outFile, cvsFile,&
                      im, jm, km, lon, lat, lev, nLevs, Levs,   &
                      mVars, nVars, inVars, outVars, outUnits,  &
                      scaleFactor, addOffset, isPos, inType,    &
                      outType, outPrec,nPsfs, srfFiles, nbkg,   &
                      bkgFiles, doInterp, date,                 &
		      inc_hhmmss, startTime, outVarsgc,         &
                      longName, isForced, lonshift )

!
! !USES:
!
   Implicit NONE

!
! !INPUT PARAMETERS: 
!

      integer, intent(in)  :: mFiles           !  Max. number of input files as
                                               !   declared in calling program
      integer, intent(in)  :: mVars            !  Max. number of variables


!
! !OUTPUT PARAMETERS:
!

      integer, intent(out)          :: nFiles       !  Actual number of input files
      character(len=*), intent(out) :: inFiles(:)   !  Input file names
      character(len=*), intent(out) :: outFile      !  Output file name 
      character(len=*), intent(out) :: cvsFile      !  Output file name 


      integer, intent(out)  :: im              !  zonal dimension
      integer, intent(out)  :: jm              !  meridional dimension
      integer, intent(out)  :: km              !  vertical dimension

      real, pointer         :: lon(:)          ! longitudes in deg (im)
      real, pointer         :: lat(:)          ! latitudes in deg (jm)
      real, pointer         :: lev(:)          ! levels in hPa (km)
      real, pointer         :: Levs(:)         ! actual levels
      integer, intent(out)  :: nLevs           ! actual number of levels
      

      integer,          intent(out) :: nVars        ! Actual number of variables
      character(len=*), intent(out) :: inVars(:)    ! Input  variable names (nVars)
      character(len=*), intent(out) :: outVars(:)   ! output variable names (nVars)
      character(len=*), intent(out) :: outUnits(:)  ! Units of output variables (nVars)
                                               ! Unit conversion factors:
                                               !   OUT = scaleFactor * IN + addOffset
    
      real, intent(out) :: scaleFactor(:)      ! scaling (nVars) 
      real, intent(out) :: addOffset(:)        ! offset (nVars)
      integer, intent(out) :: isPos(:)         ! Do positive check
      integer, intent(out) :: doInterp(:)      ! Do positive check
      logical, intent(out) :: isForced         ! force to overwrite output file
      logical, intent(out) :: lonshift         ! shift longitudes 180 when writing

      character(len=*), intent(out) :: outVarsgc(:) ! output variable names (nVars)
      character(len=*), intent(out) :: longName(:)  ! output variable names (nVars)

      character(len=3), intent(out) :: inType  ! Input file type: 'eta' or 'lcv'
      character(len=3), intent(out) :: outType ! Output file type: 'prs' or 'alt'

      integer, intent(out)          :: outPrec ! Output file precision:
                                               ! 0 = 32 bits,  1 = 6 4bits
      integer, intent(out)          :: nPsfs   ! number of input ps files
      character(len=*), intent(out) :: srfFiles(:)  !  Output file name for ps or surfp

      integer, intent(out)          :: nbkg    ! number of input ps files
      character(len=*), intent(out) :: bkgFiles(:)  !  Output file name for ps or surfp

      integer, intent(out)          :: date        ! User specified date   
      integer, intent(out)          :: inc_hhmmss  !User specified increment hours(hhmmss)   
      integer, intent(out)          :: startTime   !User specified increment hours(hhmmss)   

! !DESCRIPTION: This routine initializes {\tt fv2prs}. It parses the command 
!               and loads necessary information from the resource file.
!
! !REVISION HISTORY: 
!
! 01May2001  da Silva  Initial design and prologue.
! 12May2001  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

   integer             iarg, argc
   integer :: iargc
   character(len=4096)  argv, srfFile, bkgFile

   character(len=255)   rcfile, label, var, Vars(mVars), tmp, tmp1

   integer, parameter :: mKm = 256  ! max no of levels
   real levels(mKm)

   integer i, j, n, nVars0, rc, ios
   integer name_pos, name_len, iv   
   real    xWest, p
   logical :: debug = .false.

   argc = iargc()
   if ( argc < 1 ) call usage_()

!  Defaults
!  --------
   nFiles = 0
   nVars = 0
   outFile = 'fv2prs.prs.nc4'
   rcfile = 'fv2prs.rc'
   srfFile = 'fv2prs.ps.input'
   bkgFile = 'fv2prs.bkg.input'
   outPrec = 0
   outType = 'prs'
   date = 99999999
   inc_hhmmss = 99999999
   startTime = 99999999
   im = -1
   jm = -1
   km = -1
   xWest = undef
   inType = 'eta'

   iarg = 0
   do i = 1, 32767
      iarg = iarg + 1
      if ( iarg .gt. argc ) exit
      call GetArg ( iArg, argv )
      if(index(argv,'-o') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, outFile )
      else if(index(argv,'-f') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, outFile )
         isForced = .true.
      else if(index(argv,'-cvs') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, cvsFile )
      else if(index(argv,'-psf') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, srfFile )
         if (len(trim(srfFile)) .ge. 4096) call die (myname, '-psf files are too long.')
         call split_ ( ',', srfFile, mPsfs, srfFiles, nPsfs )
      else if(index(argv,'-bkgf') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, bkgFile )
         call split_ ( ',', bkgFile, mPsfs, bkgFiles, nbkg )
      else if(index(argv,'-date') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) date
      else if(index(argv,'-inc') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) inc_hhmmss
!         if ( date .eq. 99999999 ) call usage_()
      else if(index(argv,'-start') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) startTime 
      else if(index(argv,'-rc') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, rcfile )
      else if(index(argv,'-im') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) im
      else if(index(argv,'-west') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) xWest
      else if(index(argv,'-jm') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) jm
      else if(index(argv,'-vars') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mVars, Vars, nVars )
      else if(index(argv,'-levels') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mLevs, cLevs, nLevs )
         allocate( Levs(nLevs), stat = rc)
         if ( rc /= 0 )  call die (myname, 'wrong in allocating nLevs')

         do k = 1, nLevs
            read(cLevs(k),*) Levs(k)
         enddo
      else if(index(argv,'-prec') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) outPrec
      else if(index(argv,'-eta') .gt. 0 ) then
         inType = 'eta'
     else if(index(argv,'-lcv') .gt. 0 ) then
         inType = 'lcv'
     else if(index(argv,'-alt') .gt. 0 ) then
         outType = 'alt'
     else if(index(argv,'-help') .gt. 0 ) then
         call help_();
      else if(index(argv,'-d') .gt. 0 ) then
         debug = .true.
      else if(index(argv,'-lonshift') .gt. 0 ) then
         lonshift = .true.
      else
         nFiles = nFiles + 1
         inFiles(nFiles) = argv
      end if

   end do

print *
print *, "-----------------------------------------------------------"
   if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
print *, "fv2PRS - Convert fvDAS Output Files to Height Coordinates"
   else
print *, "fv2PRS - Convert fvDAS Output Files to Pressure Coordinates"
   end if
print *, "-----------------------------------------------------------"
print *

   if ( outPrec .eq. 32 ) outPrec = 0   
   if ( outPrec .eq. 64 ) outPrec = 1   

    if ( nFiles .eq. 0 ) call die (myname, 'no files specified')

!  Figure out output file name if not specified
!  --------------------------------------------
   if ( trim(outFile) .eq. 'fv2prs.prs.nc4' ) then
      i = index ( inFiles(1), '.eta.' )
      if ( i .lt. 1 ) i = index ( inFiles(1), '.lcv.' )
      if ( i .gt. 1 ) then
         outFile = inFiles(1)(1:i-1) // '.prs.' // inFiles(1)(i+5:)
      end if
   end if

!                  -------------------
!                  Parse Resource File
!                  -------------------

!  Load resource file
!  ------------------
   call i90_loadf ( rcfile, rc )
   if ( rc .ne. 0 ) then 
       if ( im > 0 ) then 
         allocate ( lon(im), stat=ios )
         if ( xWest .eq. undef ) xWest = -180.
         do i = 1, im
            lon(i) = xWest + (i-1) * 360. / im
         end do
       end if
       if ( jm > 0 ) then 
         allocate ( lat(jm), stat=ios )
         do j = 1, jm
            lat(j) = -90.0 + (j-1) * 180. / ( jm - 1 )
          end do
       end if

       return
   end if

!  Get PS files
   if ( nPsfs .eq. 0 ) then
      call i90_label ( 'ps_files:', rc )
      call i90_Gtoken ( srfFile, rc)
      if ( rc .eq. 0) then  
         call split_ ( ',', srfFile, mPsfs, srfFiles, nPsfs )
      end if
   end if
      
!  Get coordinates
!  ---------------
   if ( im < 0 ) then
      call i90_label ( 'zonal_resolution:', rc )
      im = i90_gint(rc)
      if ( rc .ne. 0 ) call die(myname,'cannot load im')
   end if
   if ( xWest .eq. undef ) then
      call i90_label ( 'west_most_longitude_in_deg:', rc )
      xWest = i90_gfloat(rc)
      if ( rc .ne. 0 ) call die(myname,'cannot load xWest')
   end if
   if ( jm < 0 ) then
      call i90_label ( 'meridional_resolution:', rc )
      jm = i90_gint(rc)
      if ( rc .ne. 0 ) call die(myname,'cannot load jm')
   end if
   if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
      call i90_label ( 'vertical_levels_in_meter:', rc )
      if ( rc .ne. 0 ) call die(myname,'cannot load vertical levels in meter')
      km = 0
      do i = 1, mKm
         p = i90_gfloat(rc)
         if ( rc .ne. 0 ) exit
         km = km + 1
         levels(km) = p
      end do
   else
      call i90_label ( 'vertical_levels_in_hPa:', rc )
      if ( rc .ne. 0 ) call die(myname,'cannot load vertical levels')
      km = 0
      do i = 1, mKm
         p = i90_gfloat(rc)
         if ( rc .ne. 0 ) exit
         km = km + 1
         levels(km) = p
      end do
   end if 
   if ( im .le. 0 ) call die(myname, 'invalid im<0')
   if ( jm .le. 0 ) call die(myname, 'invalid jm<0')
   if ( km .eq. 0 ) call die(myname, 'invalid km=0')

!  Generate coordinate variables
!  -----------------------------
   allocate ( lon(im), lat(jm), lev(km), stat=ios )
   if ( ios .ne. 0 ) call die(myname,'cannot allocate memory for lat/lon/lev')
   do i = 1, im
      lon(i) = xWest + (i-1) * 360. / im
   end do
   do j = 1, jm
      lat(j) = -90.0 + (j-1) * 180. / ( jm - 1 )
   end do
   lev(1:km) = levels(1:km)

   if ( nVars .eq. 0 ) return

!  Resolve variable classes
!  ------------------------
   nVars0 = nVars  ! number before expanding var classes
   nVars = 0       ! final number with var classes expanded
   do n = 1, nVars0

!     Simple variable name
!     --------------------
      if ( Vars(n)(1:1) .ne. '@' ) then
           nVars = nVars + 1
           outVars(nVars) = Vars(n)

!     Variable class
!     --------------
      else

          Vars(n) = uppercase ( Vars(n) )
          label = 'VarClass*'//trim(Vars(n)(2:))//':'
          call i90_label(trim(label),rc)
          if ( rc .ne. 0 ) then
             Vars(n) = lowercase(Vars(n))
             label = 'VarClass*'//trim(Vars(n)(2:))//':'
             call i90_label(trim(label),rc)
             if ( rc .ne. 0 )                            &
                call die(myname,'cannot find variable class '//trim(vars(n)))
          end if

          j = nVars + 1
          do i = j, mVars
             call i90_gtoken ( var, rc )
             if ( rc .ne. 0 ) exit
             nVars = nVars + 1
             outVars(nVars) = var
          end do

       end if

    end do

!  Load Variable translation table
!  -------------------------------
   do n = 1, nVars

!     Defaults
!     --------
      inVars(n) = outVars(n)
      outUnits(n) = 'UNKNOWN'
      scaleFactor(n) = 1.0
      AddOffset(n) = 0.0

!     Change defaults if entry exists on translation table
!     ----------------------------------------------------
      call i90_label ( uppercase(trim(outVars(n))), rc )
      if (rc .eq. 0) then
         outVars(n) = uppercase(outVars(n))
         call i90_gtoken ( outUnits(n), rc )
         if ( rc .ne. 0 ) call die(myname,'cannot get units' )
         if (trim(outUnits(n)) .eq. ":") then
            call i90_label ( lowercase(trim(outVars(n))), rc )
            if (rc .eq. 0) then
		outVars(n) = lowercase(outVars(n))
                call i90_gtoken ( outUnits(n), rc )
                if ( rc .ne. 0 ) call die(myname,'cannot get units' )
                call i90_gtoken ( inVars(n), rc )
                if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
                scaleFactor(n) = i90_gfloat(rc)
                if ( rc .ne. 0 ) call die(myname,'cannot get scale factor' )
                addOffset(n) = i90_gfloat(rc)
                if ( rc .ne. 0 ) call die(myname,'cannot get add offset' )
                isPos(n) = i90_gint(rc)
                if ( rc .ne. 0 ) isPos(n) = 0 
                doInterp(n) = i90_gint(rc)
                if ( rc .ne. 0 ) doInterp(n) = 0 
            end if
         else
            call i90_gtoken ( inVars(n), rc )
            if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
            scaleFactor(n) = i90_gfloat(rc)
            if ( rc .ne. 0 ) call die(myname,'cannot get scale factor' )
            addOffset(n) = i90_gfloat(rc)
            if ( rc .ne. 0 ) call die(myname,'cannot get add offset' )
            isPos(n) = i90_gint(rc)
            if ( rc .ne. 0 ) isPos(n) = 0 
            doInterp(n) = i90_gint(rc)
            if ( rc .ne. 0 ) doInterp(n) = 0 
         end if 
      else
         call i90_label ( lowercase(trim(outVars(n))), rc )
         if (rc .eq. 0) then
            outVars(n) = lowercase(outVars(n))
            call i90_gtoken ( outUnits(n), rc )
            if ( rc .ne. 0 ) call die(myname,'cannot get units' )
            call i90_gtoken ( inVars(n), rc )
            if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
            scaleFactor(n) = i90_gfloat(rc)
            if ( rc .ne. 0 ) call die(myname,'cannot get scale factor' )
            addOffset(n) = i90_gfloat(rc)
            if ( rc .ne. 0 ) call die(myname,'cannot get add offset' )
            isPos(n) = i90_gint(rc)
            if ( rc .ne. 0 ) isPos(n) = 0 
            doInterp(n) = i90_gint(rc)
            if ( rc .ne. 0 ) doInterp(n) = 0 
         else
            call i90_label ( trim(outVars(n)), rc )
            if (rc .eq. 0) then
               call i90_gtoken ( outUnits(n), rc )
               if ( rc .ne. 0 ) call die(myname,'cannot get units' )
               call i90_gtoken ( inVars(n), rc )
               if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
               scaleFactor(n) = i90_gfloat(rc)
               if ( rc .ne. 0 ) call die(myname,'cannot get scale factor' )
               addOffset(n) = i90_gfloat(rc)
               if ( rc .ne. 0 ) call die(myname,'cannot get add offset' )
               isPos(n) = i90_gint(rc)
               if ( rc .ne. 0 ) isPos(n) = 0
               doInterp(n) = i90_gint(rc)
               if ( rc .ne. 0 ) doInterp(n) = 0 
            end if
         end if 
      end if

   end do
     
   iv = 1
   do i = 1, nVars
      if ( index(trim(outVars(i)), ';' ) .gt. 0 ) then
         name_pos = index(trim(outVars(i)), ';')
         name_len = len(trim(outVars(i)))
         outVarsgc(iv) = trim(outVars(i)(1:(name_pos-1)))
         outVarsgc(iv+1) = trim(outVars(i)((name_pos+1):name_len))
         iv = iv +2
      else
         outVarsgc(iv) = outVars(i)
         iv = iv + 1
      end if
   end do

   do i = 1, iv
      call i90_label ( uppercase(trim(outVarsgc(i))) // ':', rc )
      if ( rc .ne. 0 ) call i90_label ( lowercase(trim(outVarsgc(i))) // ':', rc )
      if ( rc .ne. 0 ) cycle
      call i90_gtoken ( tmp, rc )
      if ( rc .ne. 0 ) cycle 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            longName(i) = trim(tmp1)
            exit
         end if
         longName(i) = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = longName(i)
      end do 
   end do

!......................................................................


   print *, 'Input   Files: ', nFiles, ' ('//inType//')'
   do i = 1, nFiles
      print *, "               ", trim(inFiles(i))
   end do
   print *
   print *, 'Output   File: ', trim(outFile), ', prec = ', outPrec
   print *, 'Resource File: ', trim(rcfile)
   print *

   if ( debug ) then
    write(*,'(a,i3,/(10x,6f10.2)))') '    Longitudes: ', im,lon(1:im)
    write(*,'(a,i3,/(10x,6f10.2)))') '     Latitudes: ', jm,lat(1:jm)
   end if
   if ( nLevs .gt. 0 ) then
      write(*,'(a,i3,/(10x,6f10.2)))') '        Levels: ', nLevs,Levs
   else
      write(*,'(a,i3,/(10x,6f10.2)))') '        Levels: ', km,lev(1:km) 
   end if 
   print *
   write(*,'(a,i3,/(10x,6a10)))') '     Variables: ', nVars !,outVars(1:nVars)

   print *, &
'    OutVar      Units       inVar         Scale Factor   Add Offset   Positive Check', &
     '  doInterp_new_mthd'
   print *, &
'    ------      -----       -----         ------------   ----------   -------------', &
     '   -----------------'
   do n = 1, nVars
      write(*,'(5x,3a12,2f12.5,i12, I12)') outVars(n), outUnits(n), inVars(n), &        
                                scaleFactor(n), addOffset(n), isPos(n), doInterp(n)
   end do
   print *

   end subroutine Init_


   subroutine getCvsVersion()

   open (11, file=cvsFile, iostat=rc) 
   if ( rc .eq. 0 )  then
      read(11, '(a)') cvsV
      cvsV_yes = 1
   end if
 
   end subroutine getCvsVersion


!  This routine gets dimensions and coordinate information.

   subroutine getMetaData()

     call GFIO_DimInquire ( fid, im_e, jm_e, km_e, lm_e, nvars_e, ngatts, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')

!    Allocate memory for meta data
     allocate ( yyyymmdd(lm_e),hhmmss(lm_e),lon_e(im_e),lat_e(jm_e),lev_e(km_e), &
                kmVar_e(mVars), ak(km_e+1), bk(km_e+1), hhmmss_bkg(lm_e), stat = rc )
     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon,lat,lev')
    
     if ( nVars .eq. 0 ) then
        allVars = .true.
     end if 

!    Set ptop, pint, ak and bk
     call set_eta(km_e,ks,ptop,pint,ak,bk)

!    Allocated memory for ps or delp 
     allocate ( delp(im_e,jm_e,km_e), work(im_e,jm_e,km_e), ps(im_e,jm_e), stat = rc ) 
     if ( rc /= 0 )  call die (myname, 'can not allocate delp, work and ps')

!    Get meta data
     print *, 'calling inquire'
     call GFIO_Inquire ( fid, im_e, jm_e, km_e, lm_e, nVars_e,  &
                               title, source, contact, amiss,   &
                               lon_e, lat_e, lev_e, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_e,  &
                               valid_range , packing_range, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire '//inFile1)
     source = "Global Modeling and Assimilation Office"
     contact = "data@gmao.gsfc.nasa.gov"

     if (outType .eq. 'alt' .or. outType .eq. 'ALT')  levunits = 'meter'
     if (  km_e .gt. 1 .and. lev_e(1) .gt. lev_e(2) ) then
        doRev = .true.
     end if

     do iv = 1, lm_e
        hhmmss_bkg(iv) = hhmmss(iv)
     end do

     timinc_save = timinc
     if ( inc_hhmmss .ne. 99999999 ) then
         timinc = inc_hhmmss
     end if

!     call GFIO_GetVarT ( fid, 'delp', yyyymmdd(1), hhmmss(1), &
!         im_e, jm_e, 1, km_e, delp, rc, fid2 )
     rc = 1
     do iv = 1, nvars_e
        if (vname(iv) .eq. 'delp' .or. vname(iv) .eq. 'DELP') then
           rc = 0
           exit
        end if
     end do 

     iv = 1
     if ( rc .eq. 0 ) then
        call GFIO_GetRealAtt ( fid, "ptop", iv, buf, rc1 ) 
     end if
     if ( rc .eq. 0 .and. rc1 .eq. 0)  then 
        inType = 'lcv'
     else
        inType = 'eta'
     end if

     if ( allVars ) then
        ii = 0
        do iv = 1, nvars_e
           if ( inType .eq. 'lcv' .and.                         &
                (vname(iv) .eq. 'delp' .or.                     &
                 vname(iv) .eq. 'DELP') )  cycle
           ii = ii + 1
           inVars(ii) = vname(iv)
           outVars(ii) = vname(iv)
           outUnits(ii) = vunits(iv)
           outKm(ii) = kmVar_e(iv)
           if (kmVar_e(iv) .gt. 0 ) outKm(ii) = km
           if (kmVar_e(iv) .gt. 0 .and. nLevs .gt. 0 ) outKm(ii) = nLevs
           scaleFactor(ii) = 1.0
           addOffset(ii) = 0.0
           isPos(ii) = 0
           doInterp(ii) = 0
        end do         
        nVars = ii 
     end if 

     if ( im < 0 ) then 
        allocate ( lon(im_e), stat = rc )
        lon = lon_e
        im = im_e
     end if
     if ( jm < 0 ) then 
        allocate ( lat(jm_e), stat = rc )
        lat = lat_e
        jm = jm_e
     end if
     if ( km < 0 ) then 
        allocate ( lev(km_e), stat = rc )
        lev = lev_std
        km = km_std
     end if

   end subroutine getMetaData

!  This subroutine gets output variable names and variable metadata.
!  If the output variables are computed from other variables, get
!  the variable names needed to computed the output variables. 
!  The total number of output variables will accumulated.

   subroutine getOutputVarName()

     nnames = 1

     do iv = 1, nVars
        if ( index(trim(outVars(iv)), ';' ) .gt. 0 ) then
           name_pos = index(trim(outVars(iv)), ';')
           name_len = len(trim(outVars(iv)))
           outVarsg(nnames) = outVars(iv)(1:(name_pos-1))
           outVarsg(nnames+1) = outVars(iv)((name_pos+1):name_len)
           outUnitsg(nnames) = outUnits(iv)
           outUnitsg(nnames+1) = outUnits(iv)
           nnames = nnames + 2
         else
           outVarsg(nnames) = outVars(iv)
           outUnitsg(nnames) = outUnits(iv)

           select case ( trim(outVars(iv)) )
              case ("TMPU", "tmpu")
                 vtitle_in(nnames) = 'temperature'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("RH", "rh")
                 vtitle_in(nnames) = 'relative humidity'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("DTRAIN", "dtrain")
                 vtitle_in(nnames) = 'Detrainment Cloud Mass Flux'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("HGHT", "hght")
                 vtitle_in(nnames) = 'geopotential height'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("ALT", "alt")
                 vtitle_in(nnames) = 'altimetric height'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("DENS", "dens")
                 vtitle_in(nnames) = 'density'
                 outKm(nnames) = km
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("SLP", "slp")
                 vtitle_in(nnames) = 'sea level pressure'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("PREACC", "preacc")
                 vtitle_in(nnames) = 'total precipitation'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("LWGDOWN", "lwgdown")
                 vtitle_in(nnames) = 'Downward longwave radiation at the ground'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("LWGUP", "lwgup")
                 vtitle_in(nnames) = 'Upward longwave radiation at the ground'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("OSR", "osr")
                 vtitle_in(nnames) = 'outgoing shortwave radiation'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
              case ("OSRCLR", "osrclr")
                 vtitle_in(nnames) = 'clear sky outgoing shortwave radiation'
                 outKm(nnames) = 0
                 valid_range_prs(1,nnames) = undef
                 packing_range_prs(1,nnames) = undef
                 valid_range_prs(2,nnames) = undef
                 packing_range_prs(2,nnames) = undef
           end select

           nnames = nnames + 1
         end if
!        Handle TMPU, HGHT, SLP and RH
         if ( index(inVars(iv), ',' ) .gt. 0 ) then

           select case ( trim(outVars(iv)) )
            case ("TMPU", "tmpu")
               call split_ (',', inVars(iv), 3, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               delp_name = list(2)
               sphu_name = list(3)
               name_pos = index(trim(sphu_name), ')')
               sphu_name = sphu_name(1:(name_pos-1))
            case ("ALT", "alt") 
               call split_ (',', inVars(iv), 2, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               ze_name = pt_name
               phis_name = list(2)
               name_pos = index(trim(phis_name), ')')
               phis_name = phis_name(1:(name_pos-1))
            case ("HGHT", "hght") 
               call split_ (',', inVars(iv), 2, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               ze_name = pt_name
               phis_name = list(2)
               name_pos = index(trim(phis_name), ')')
               phis_name = phis_name(1:(name_pos-1))
            case ("DENS", "dens") 
               call split_ (',', inVars(iv), 2, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               ze_name = pt_name
               phis_name = list(2)
               name_pos = index(trim(phis_name), ')')
               phis_name = phis_name(1:(name_pos-1))
            case ("RH", "rh") 
               call split_ (',', inVars(iv), 2, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               sphu_name = list(2)
               name_pos = index(trim(sphu_name), ')')
               sphu_name = sphu_name(1:(name_pos-1))
            case ("DTRAIN","dtrain")
               call split_ (',', inVars(iv), 3, list, nlist)
               tmp_name1 = list(1)
               name_len = len(trim(tmp_name1))
               name_pos = index(trim(tmp_name1), '(')
               tmp_name1 = tmp_name1((name_pos+1):name_len)
               op = list(2)
               tmp_name2 = list(3)
               name_pos = index(trim(tmp_name2), ')')
               tmp_name2 = tmp_name2(1:(name_pos-1))
            case ("SLP", "slp")
               call split_ (',', inVars(iv), 3, list, nlist)
               pt_name = list(1)
               name_len = len(trim(pt_name))
               name_pos = index(trim(pt_name), '(')
               pt_name = pt_name((name_pos+1):name_len)
               phis_name = list(2)
               delp_name = list(3)
               name_pos = index(trim(delp_name), ')')
               delp_name = delp_name(1:(name_pos-1))
            case ("PREACC", "preacc")
               call split_ (',', inVars(iv), 3, list, nlist)
               tmp_name1 = list(1)
               name_len = len(trim(tmp_name1))
               name_pos = index(trim(tmp_name1), '(')
               tmp_name1 = tmp_name1((name_pos+1):name_len)
               op = list(2)
               tmp_name2 = list(3)
               name_pos = index(trim(tmp_name2), ')')
               tmp_name2 = tmp_name2(1:(name_pos-1))
            case ("LWGDOWN", "lwgdown")
               call split_ (',', inVars(iv), 3, list, nlist)
               tmp_lwgdown1 = list(1)
               name_len = len(trim(tmp_lwgdown1))
               name_pos = index(trim(tmp_lwgdown1), '(')
               tmp_lwgdown1 = tmp_lwgdown1((name_pos+1):name_len)
               op3 = list(2)
               tmp_lwgdown2 = list(3)
               name_pos = index(trim(tmp_lwgdown2), ')')
               tmp_lwgdown2 = tmp_lwgdown2(1:(name_pos-1))
            case ("LWGUP", "lwgup")
               call split_ (',', inVars(iv), 5, list, nlist)
               tmp_lwgup1 = list(1)
               name_len = len(trim(tmp_lwgup1))
               name_pos = index(trim(tmp_lwgup1), '(')
               tmp_lwgup1 = tmp_lwgup1((name_pos+1):name_len)
               tmp_lwgup2 = list(3)
               tmp_lwgup3 = list(5)
               name_pos = index(trim(tmp_lwgup3), ')')
               tmp_lwgup3 = tmp_lwgup3(1:(name_pos-1))
            case ("OSRCLR", "osrclr")
               call split_ (',', inVars(iv), 3, list, nlist)
               tmp_osrclr1 = list(1)
               name_len = len(trim(tmp_osrclr1))
               name_pos = index(trim(tmp_osrclr1), '(')
               tmp_osrclr1 = tmp_osrclr1((name_pos+1):name_len)
               op2 = list(2)
               tmp_osrclr2 = list(3)
               name_pos = index(trim(tmp_osrclr2), ')')
               tmp_osrclr2 = tmp_osrclr2(1:(name_pos-1))
            case ("OSR", "osr")
               call split_ (',', inVars(iv), 3, list, nlist)
               tmp_osr1 = list(1)
               name_len = len(trim(tmp_osr1))
               name_pos = index(trim(tmp_osr1), '(')
               tmp_osr1 = tmp_osr1((name_pos+1):name_len)
               op1 = list(2)
               tmp_osr2 = list(3)
               name_pos = index(trim(tmp_osr2), ')')
               tmp_osr2 = tmp_osr2(1:(name_pos-1))
           end select 

         end if 
      end do

   end subroutine getOutputVarName

!  This routine selects input variables and their metadata from input files
!  based on output variable names.

   subroutine getInputVarName()

     nnames = 1
     do iv = 1, nVars
        if ( index(trim(inVars(iv)), ';' ) .gt. 0 ) then
           name_pos = index(trim(inVars(iv)), ';')
           name_len = len(trim(inVars(iv)))
           inVarsg(nnames) = inVars(iv)(1:(name_pos-1))
           inVarsg(nnames+1) = inVars(iv)((name_pos+1):name_len)
           nnames = nnames + 2
         else
           inVarsg(nnames) = inVars(iv)
           nnames = nnames + 1
         end if
     end do

!    tNames will be the total number of GFIO variables.
     tNames = nnames - 1

!    Find long names and number of levels for meta data
!    -------------------------------
     do iv = 1, tNames 
        do itest = 1, nVars_e
            if ( (uppercase(trim(inVarsg(iv)))) .eq. uppercase(trim(vName(itest))) )  then 
               if ( index(vtitle(itest), '[' ) .gt. 0 ) then
                  name_pos = index(vtitle(itest), '[' )
                  vtitle_in(iv) = vtitle(itest)(1:(name_pos-1))
               else
                  vtitle_in(iv) = vtitle(itest)
               end if
               outKm(iv) = kmVar_e(itest)
               if (kmVar_e(itest) .gt. 0) outKm(iv) = km
               if (kmVar_e(itest) .gt. 0 .and. nLevs .gt. 0) outKm(iv) = nLevs
               valid_range_prs(1,iv) = undef
               packing_range_prs(1,iv) = undef
               valid_range_prs(2,iv) = undef
               packing_range_prs(2,iv) = undef
!               valid_range_prs(1,iv) = valid_range(1,itest)
!               packing_range_prs(1,iv) = packing_range(1,itest)
!               valid_range_prs(2,iv) = valid_range(2,itest)
!               packing_range_prs(2,iv) = packing_range(2,itest)
            end if
        end do
     end do
   end subroutine getInputVarName

!  This routine prepares some metadata for output files, and
!  creates the output files.

   subroutine createFile()

      if ( nLevs .le. 0 )  then
         nLevs = km
         allocate(Levs(km), stat = rc)
         if ( rc /= 0 )  call die (myname, 'cannot allocate Levs')
         Levs = lev
      end if
         
      amiss = undef

      t_len = 254
      do  ii= 254, 1, -1
         if (title(ii:ii) .eq. ' ' .or. ichar(title(ii:ii)) .le. 0) then
            t_len = t_len - 1
         else 
            exit
         end if
      end do
      if ( cvsV_yes .eq. 1 ) then
         title_cvs = title(1:t_len) //'. '// cvsV(1:len(cvsV))
      else
         title_cvs = title(1:t_len)
      end if

      t_len = 254
      do ii = 254, 1, -1
         if (source(ii:ii) .eq. ' ' .or. ichar(source(ii:ii)) .le. 0) then
            t_len = t_len - 1
         else 
            exit
         end if
      end do
      if ( cvsV_yes .eq. 1 ) then
         source_cvs = source(1:t_len) //'. '// cvsV(1:len(cvsV))
      else
         source_cvs = source(1:t_len)
      end if

      do i = 1, tNames
        do ii = 1, tNames
           if ( outVarsg(i) .eq. outVarsgc(ii) ) then
              if ( len(trim(longName(ii))) .ge. 1 .and.                   &
                   len(trim(longName(ii))) .lt. 255 ) then
                 vtitle_in(i) = trim(longName(ii))
              end if
           end if
        end do
      end do

      inquire(file=outFile1, EXIST=ex)

      if (ex .and. (.not. isForced) )  &
         call die (myname,'please use -f option instead of -o option to overwrite output file -- ' //outFile1)
      if ( date .eq. 99999999 ) then
         call GFIO_Create ( outFile1, title_cvs, source_cvs, contact, amiss,&
                            im, jm, nLevs, lon, lat, Levs, levunits,       &
                            yyyymmdd(1), hhmmss(1), timinc,                &
                            tNames, outVarsg, vtitle_in, outUnitsg, outKm, &
                            valid_range_prs, packing_range_prs, outPrec,   &
                            out_fid, rc )
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')
      else
         begTime = 0
         do ii = 1, lm_e
            if (yyyymmdd(ii) .eq. date ) then
               begTime = hhmmss(ii)
               exit
            end if
         end do
         if ( startTime /= 99999999 ) then
            begTime = startTime
         end if 
         call GFIO_Create ( outFile1, title_cvs, source_cvs, contact, amiss,&
                            im, jm, nLevs, lon, lat, Levs, levunits,       &
                            date, begTime, timinc,                         &
                            tNames, outVarsg, vtitle_in, outUnitsg, outKm, &
                            valid_range_prs, packing_range_prs, outPrec,   &
                            out_fid, rc )
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')
      end if 
   end subroutine createFile

   subroutine checkEndDateTime()

     integer ::  im2, jm2, km2, nvars2, ngatts2

     call GFIO_DimInquire (fid2, im2, jm2, km2, lm2, nvars2, ngatts2, rc)
     allocate(yyyymmdd2(lm2), hhmmss2(lm2))

     call GFIO_Inquire ( fid2, im_e, jm_e, km_e, lm_e, nVars_e,  &
                               title, source, contact, amiss,   &
                               lon_e, lat_e, lev_e, levunits,   &
                               yyyymmdd2, hhmmss2, timinc2,     &
                               vname, vtitle, vunits, kmVar_e,  &
                               valid_range , packing_range, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire '//inFile1)
   end subroutine checkEndDateTime
!  This routine reads all the coordinate variables and sets up eta/lcv 
!  coordinates for interpolation. 

   subroutine initializeGrid()
   
      hhmmss(it) = curTime

      if (nbkg > 0) then
         fidbp = -1
         fidbc = -1
         fidbn = -1
         do ibkg = 1, nbkg
            call GFIO_Open ( bkgFiles(ibkg), in_fmode, fidb, rc ) 

            call GFIO_DimInquire(fidb,imPs,jmPs,kmPs,lmPs,nvarsPs,ngattsPs,rc)
            if ( rc /= 0 )  call die(myname,'can not do GFIO_DimInquire from '//srfFiles(ipsf))
            allocate ( yyyymmddPs(lmPs),hhmmssPs(lmPs),lonPs(imPs),latPs(jmPs), &
                       levPs(kmPs), kmVarPs(mVars), stat = rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmddPs')
            call GFIO_Inquire (fidb,imPs,jmPs,kmPs,lmPs,nVarsPs,  &
                   titlePs, sourcePs, contactPs, amissPs,   &
                   lonPs, latPs, levPs, levunitsPs, &
                   yyyymmddPs, hhmmssPs, timincPs,  &
                   vnamePs, vtitlePs, vunitsPs, kmVarPs,  &
                   valid_rangePs , packing_rangePs, rc)
            if (rc /= 0) call die (myname, 'can not do GFIO_Inquire for BKG file '//bkgFiles(ibkg))

            do ips = 1, lmPs
               if (yyyymmdd(it) ==  yyyymmddPs(ips) .and. &
                   hhmmss(it) == hhmmssPs(ips) ) then
                   call GFIO_Open ( bkgFiles(ibkg), in_fmode, fidbc, rc )
               end if
               if (yyyymmdd(it) ==  yyyymmddPs(ips) .and. &
                   hhmmss(it)-timinc == hhmmssPs(ips) ) then
                   call GFIO_Open ( bkgFiles(ibkg), in_fmode, fidbp, rc )
               end if
               if ( (yyyymmdd(it) ==  yyyymmddPs(ips) .and. &
                   hhmmss(it)+timinc == hhmmssPs(ips) ) .or.&
                    (INCYMD(yyyymmdd(it), 1) .eq. yyyymmddPs(ips) &
                     .and. (hhmmss(it)+timinc-240000) .eq. hhmmssPs(ips))  ) then
                   call GFIO_Open ( bkgFiles(ibkg), in_fmode, fidbn, rc )
               end if
            enddo

            if (fidbp .eq. -1) fidbp = fidbc
            if (fidbn .eq. -1) fidbn = fidbc

            call GFIO_Close(fidb, rc)
         enddo
      end if

      if ( inType .eq. 'eta' ) then                    
         if (nPsfs .eq. 2) then
             call GFIO_Open ( srfFiles(1), in_fmode, fid_srf1, rc )

             call GFIO_DimInquire(fid_srf1,imPs,jmPs,kmPs,lmPs,nvarsPs,ngattsPs,rc)
             if ( rc /= 0 )  call die(myname,'can not do GFIO_DimInquire from template '//srfFiles(1))
             allocate ( yyyymmddPs(lmPs),hhmmssPs(lmPs),lonPs(imPs),latPs(jmPs), &
                        levPs(kmPs), kmVarPs(mVars), stat = rc )
             if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmddPs')
             call GFIO_Inquire (fid_srf1,imPs,jmPs,kmPs,lmPs,nVarsPs,  &
                    titlePs, sourcePs, contactPs, amissPs,   &
                    lonPs, latPs, levPs, levunitsPs, &
                    yyyymmddPs, hhmmssPs, timincPs,  &
                    vnamePs, vtitlePs, vunitsPs, kmVarPs,  &
                    valid_rangePs , packing_rangePs, rc)
             if (rc /= 0) call die (myname, 'can not do GFIO_Inquire for PS file '//srfFiles(ipsf))

             if (hhmmssPs(lmPs) + timincPs .ge. 240000) then
                call strTemplate(srfFileTmp2, srfFiles(nPsfs), nymd = incymd(yyyymmddPs(lmPs),1),  &
                             nhms = hhmmssPs(lmPs) + timincPs - 240000 ) 
             else
                call strTemplate(srfFileTmp2, srfFiles(nPsfs), nymd = yyyymmddPs(lmPs),  &
                             nhms = hhmmssPs(lmPs) + timincPs )  
             end if
             if (trim(srfFileTmp2) .eq. trim(srfFiles(nPsfs)) ) then
                templt = .false.
             else
                templt = .true.
             end if
             call GFIO_Close(fid_srf1, rc)
             if (templt) then
                ymdTmp1 = yyyymmddPs(lmPs)
                hmsTmp1 = hhmmssPs(lmPs)
                do while (yyyymmdd(it) .gt. ymdTmp1 .or. (yyyymmdd(it) .eq. ymdTmp1 .and. &
                          hhmmss(it) .gt. hmsTmp1) )
                   ymdTmp = hmsTmp1
                   hmsTmp1 = hmsTmp1 + timincPs
                   if (hmsTmp1 .ge. 240000) then
                      hmsTmp1 = hmsTmp1 - 240000
                      ymdTmp1 = incymd(ymdTmp1, 1)
                   end if
                end do
                hmsTmp2 = hmsTmp1 - timincPs
                ymdTmp2 = ymdTmp1
                if (hmsTmp2 .lt. 0) then
                   hmsTmp2 = hmsTmp2 + 240000
                   ymdTmp2 = ymdTmp
                end if
                call strTemplate(srfFileTmp1, srfFiles(nPsfs), nymd=ymdTmp1, nhms=hmsTmp1) 
                call strTemplate(srfFileTmp2, srfFiles(nPsfs), nymd=ymdTmp2, nhms=hmsTmp2) 
                call GFIO_Open ( srfFileTmp1, in_fmode, fid_srf1, rc )
                call GFIO_Open ( srfFileTmp2, in_fmode, fid_srf2, rc )
                   
                if (ymdTmp1 .eq. yyyymmdd(it) .and. hmsTmp1 .eq. hhmmss(it)) found_ps = .true.

                name_tmp = 'surfp'
                call check_name(name_tmp, nVarsPs, vNamePs, rc)
                if (rc .eq. 0) then
                   if (found_ps) then
                   call GFIO_GetVarT ( fid_srf1, name_tmp, yyyymmdd(it), hhmmss(it), &
                                    im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                   else
                   call GFIO_GetVarT ( fid_srf2, name_tmp, yyyymmdd(it), hhmmss(it), &
                                    im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                   print *, "Ignore the GFIO_GetVar: error reading variable"
                   endif
                   if (rc .eq. 0) then
                      call GFIO_Close ( fid_srf1, rc1 )
                      if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                   end if
                else
                   name_tmp = "ps"
                   call check_name(name_tmp, nVarsPs, vNamePs, rc)
                   call GFIO_GetVarT ( fid_srf2, name_tmp, yyyymmdd(it), hhmmss(it),  &
                     im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                   if (rc .eq. 0) then
                      call GFIO_Close ( fid_srf1, rc1 )
                      if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                   end if
                end if
                if (rc /= 0) call die(myname, 'can not get surface files with templates')
             end if
         end if

         if ( .not. templt ) then
          if ( nPsfs > 0 ) then
             do ipsf = 1, nPsfs
                call GFIO_Open ( srfFiles(ipsf), in_fmode, fid_srf1, rc )
                if ( ipsf .ge. 2) then
                   call GFIO_Open ( srfFiles(ipsf-1), in_fmode, fid_srf2, rc )
                else
                   fid_srf2 = fid_srf1
                end if

                call GFIO_DimInquire(fid_srf1,imPs,jmPs,kmPs,lmPs,nvarsPs,ngattsPs,rc)
                if ( rc /= 0 )  call die(myname,'can not do GFIO_DimInquire from '//srfFiles(ipsf))
                allocate ( yyyymmddPs(lmPs),hhmmssPs(lmPs),lonPs(imPs),latPs(jmPs), &
                           levPs(kmPs), kmVarPs(mVars), stat = rc )
                if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmddPs')
                call GFIO_Inquire (fid_srf1,imPs,jmPs,kmPs,lmPs,nVarsPs,  &
                       titlePs, sourcePs, contactPs, amissPs,   &
                       lonPs, latPs, levPs, levunitsPs, &
                       yyyymmddPs, hhmmssPs, timincPs,  &
                       vnamePs, vtitlePs, vunitsPs, kmVarPs,  &
                       valid_rangePs , packing_rangePs, rc)
                if (rc /= 0) call die (myname, 'can not do GFIO_Inquire for PS file '//srfFiles(ipsf))

                found_ps = .false.
                do ips = 1, lmPs
                   if (yyyymmdd(it) ==  yyyymmddPs(ips) .and. &
                       hhmmss(it) == hhmmssPs(ips) ) then 
                       found_ps = .true.
                   end if
                enddo

!                if ( found_ps ) then
                if ( found_ps .or. (yyyymmddPs(lmPs) .ge. yyyymmdd(it)  &
                     .and. hhmmssPs(lmPs) .ge. hhmmss(it)) ) then
                   name_tmp = 'surfp'
                   call check_name(name_tmp, nVarsPs, vNamePs, rc)
                   if (rc .eq. 0) then
                      if (found_ps) then
                      call GFIO_GetVarT ( fid_srf1, name_tmp, yyyymmdd(it), hhmmss(it), &
                                       im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                      else 
                      call GFIO_GetVarT ( fid_srf2, name_tmp, yyyymmdd(it), hhmmss(it), &
                                       im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                      print *, "Ignore the GFIO_GetVar: error reading variable"
                      endif
                      if (rc .eq. 0) then
                         call GFIO_Close ( fid_srf1, rc1 )
                         if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                         exit
                      end if
                   else 
                      name_tmp = "ps"
                      call check_name(name_tmp, nVarsPs, vNamePs, rc)
                      call GFIO_GetVarT ( fid_srf2, name_tmp, yyyymmdd(it), hhmmss(it),  &
                        im_e, jm_e, 0, 1, ps, rc, fid_srf1 )
                      if (rc .eq. 0) then
                         call GFIO_Close ( fid_srf1, rc1 )
                         if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                         exit
                      end if
                   end if
                   call GFIO_Close ( fid_srf1, rc1 )
                   if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                   if ( rc .eq. 0 ) exit
                else
                   call GFIO_Close ( fid_srf1, rc1 )
                   if ( fid_srf1 /= fid_srf2 ) call GFIO_Close ( fid_srf2, rc1 )
                end if
             end do
          else 
            name_tmp = "surfp"
            call check_name(name_tmp, nVars_e, vName, rc)
            if (rc .eq. 0) then
               if (date .ne. 99999999) then
                  call GFIO_GetVarT(fid, name_tmp, date, hhmmss(it), &
                        im_e, jm_e, 0, 1, ps, rc, fid2 )
               else
                  call GFIO_GetVarT(fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                        im_e, jm_e, 0, 1, ps, rc, fid2 )
               end if
            else
               name_tmp = "ps"
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it),    &
                  im_e, jm_e, 0, 1, ps, rc, fid2 )
            end if
          end if 
         end if 

         write(char_date,'(i8)') yyyymmdd(it)
         write(char_time,'(i6)') hhmmss(it)
         if ( rc /= 0 ) call die (myname, 'can not get ps from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z' )
      end if

      if ( inType .eq. 'eta' ) then
          call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, ps, ak, bk)
          if ( rc /= 0 )  call die (myname, 'something wrong in Interp_Init')
      end if
     
      if ( inType .eq. 'lcv' ) then
        if ( doRev ) then
           name_tmp = "delp"
           call check_name(name_tmp, nVars_e, vName, rc)
           call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it),      &
              im_e, jm_e, 1, km_e, work, rc, fid2 )
           if ( rc /= 0 )                                                 &
              call die (myname, 'can not get delp from ' //trim(inFile1) & 
                        //' at ' //char_date //' ' //char_time //'z')
           do k = 1, km_e
             do j = 1, jm_e
               do i = 1, im_e
                  delp(i,j,k) = work(i,j,km_e+1-k)
               end do 
             end do
           end do
        else
           name_tmp = "delp"
           call check_name(name_tmp, nVars_e, vName, rc)
           call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
               im_e, jm_e, 1, km_e, delp, rc, fid2 )
        end if

        if (outType .eq. 'alt' .or. outType .eq. 'ALT' ) then 
           call Interp_Init(im_e, jm_e, km_e, ptop, grid1, rc, delp = delp)
        else
           call Interp_Init(im_e, jm_e, km_e, ptop, grid1, rc, delp = delp)
           call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, delp = delp)
        end if
        if ( rc /= 0 )  call die (myname, 'something wrong in Interp_Init')

        if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
            allocate ( alt(im_e,jm_e,km_e), alt1(im_e,jm_e,km_e), stat=rc )
            allocate ( pt(im_e,jm_e,km_e), phis(im_e,jm_e), slp(im_e,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pt and phis')
            name_tmp = "theta"
            call check_name(name_tmp, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                   im_e, jm_e, 1, km_e, pt, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' &
                   //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            name_tmp = "phis"
            call check_name(name_tmp, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it),  &
                  im_e, jm_e, 0, 1, phis, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 2D phis from ' &
                  //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')

            allocate ( wz(im_e,km_e + 1,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate wz')
            call comp_hght(ptop,im_e,jm_e,km_e,kappa,cpm,grav,delp, phis, pt, wz)
            wz = wz / grav
            do k = 1, km_e
               do j = 1, jm_e
                  do i = 1, im_e
                     alt1(i,j,km_e-k+1) = wz(i,k,j)
                  end do
               end do
            end do
            allocate (lon3d_e(im_e*jm_e*km_e), lat3d_e(im_e*jm_e*km_e),  &
                      lev3d_e(im_e*jm_e*km_e), stat=rc )
            call get_coords(im_e,jm_e,km_e,lon_e,lat_e,lev_e,lon3d_e,lat3d_e,lev3d_e)
            call geopot2alt(im_e*jm_e*km_e,alt1,grav,lat3d_e,alt)
             
            call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, pm=alt)
            if ( rc /= 0 )  call die (myname, 'can not initialize height grid')
            deallocate(lon3d_e,lat3d_e,lev3d_e)
            deallocate(alt, alt1)
            deallocate(pt, phis, wz, slp)
        end if
      elseif (outType .eq. 'alt' .or. outType .eq. 'ALT') then
            allocate ( alt(im_e,jm_e,km_e), alt1(im_e,jm_e,km_e),stat=rc )
            name_tmp = "h"
            call check_name(name_tmp, nVars_e, vName, rc)
            if (rc .eq. 0) then
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                                   im_e, jm_e, 1, km_e, alt, rc, fid2 )
            else
               name_tmp = "hght"
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                                   im_e, jm_e, 1, km_e, alt, rc, fid2 )
            end if
            if (rc .ne. 0) then
               name_tmp = "ze"
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                         im_e, jm_e, 1, km_e, alt, rc, fid2 )
            end if
            if ( rc /= 0 )  call die (myname, 'something wrong in reading HGHT')
            do k = 1, km_e
               do j = 1, jm_e
                  do i = 1, im_e
                     alt1(i,j,km_e-k+1) = alt(i,j,k)
                  end do
               end do
            end do
            allocate (lon3d_e(im_e*jm_e*km_e), lat3d_e(im_e*jm_e*km_e),  &
                      lev3d_e(im_e*jm_e*km_e), stat=rc )
            call get_coords(im_e,jm_e,km_e,lon_e,lat_e,lev_e,lon3d_e,lat3d_e,lev3d_e)
            call geopot2alt(im_e*jm_e*km_e,alt1,grav,lat3d_e,alt)
            deallocate(lon3d_e,lat3d_e,lev3d_e)
            call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, pm=alt)
            if ( rc /= 0 )  call die (myname, 'can not initialize height grid')
            if (rc .eq. 0)  deallocate(alt,alt1)
      end if

   end subroutine initializeGrid

!  This routine reads/computes input variables.
 
   subroutine readVar()

      wind_field = .false.
   
      if (date .ne. 99999999) yyyymmdd(it) = date

!     Expand variable names for u;v
      if ( index(trim(inVars(iv)), ';' ) .gt. 0 ) then
          name_pos = index(trim(inVars(iv)), ';')
          name_len = len(trim(inVars(iv)))
          v_name  = inVars(iv)((name_pos+1):name_len)
          var_name = inVars(iv)(1:(name_pos-1))
          wind_field = .true.
      else
          var_name = inVars(iv)
      end if

      if ( index(trim(outVars(iv)), ';' ) .gt. 0 ) then
          name_pos = index(trim(outVars(iv)), ';')
          name_len = len(trim(outVars(iv)))
          v_name_out  = outVars(iv)((name_pos+1):name_len)
          var_name_out = outVars(iv)(1:(name_pos-1))
      else
          var_name_out = outVars(iv)
      end if

      call check_name(var_name, nVars_e, vName, rc)
      call check_name(v_name, nVars_e, vName, rc)

!      do itest = 1, nVars_e
!         if (uppercase(trim(var_name)) .eq. uppercase(trim(vName(itest))) ) then
!            var_name = vName(itest)
!         end if
!         if (uppercase(trim(v_name)) .eq. uppercase(trim(vName(itest))) ) then
!            v_name = vName(itest)
!         end if
!      end do

!     find the index for the current variable from vName array
      do itest = 1, nVars_e
         if ( (index(uppercase(trim(inVars(iv))), uppercase(trim(vName(itest))) ) .gt. 0   &
            .and. index(trim(inVars(iv)), ';') .gt. 0 )      &
            .or. uppercase(trim(inVars(iv))) .eq. uppercase(trim(vName(itest)))  & 
            .or. lowercase(trim(inVars(iv))) .eq. lowercase(trim(vName(itest))))  exit 
      end do

      num_levs = kmVar_e(itest)

      do id = 1, mDer3dVars     
         if ( trim(outVars(iv)) .eq. trim(der3dVars(id)) .or. trim(outVars(iv)) .eq. lowercase(der3dVars(id)) )   then
             num_levs = km_e
         end if 
      end do

      do id = 1, mDer2dVars     
         if ( trim(outVars(iv)) .eq. trim(der2dVars(id)) .or. trim(outVars(iv)) .eq. lowercase(der2dVars(id)))  then
             num_levs = 0
         end if
      end do

!     Read variable from GFIO file
!     ----------------------------
      if ( num_levs > 0 ) then       ! 3D file
!     Allocated memory 
         nobs = im * jm * km
         allocate ( conf(nobs), lon3d(nobs), lat3d(nobs), lev3d(nobs),     &
              inField(im_e,jm_e,km_e), outField(im,jm,km),     &
              inFieldtmp(im_e,jm_e,km_e), stat=rc ) 
         if ( rc /= 0 )  call die (myname, 'can not allocate lon3d ')
         inField = 0

         print *,' Will read ',TRIM(var_name)
         if ( doRev ) then
            if (nbkg .lt. 1 .or.  doInterp(iv) .lt. 1) then
               call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, work, rc, fid2 )
            else
               call InterpT(fid,fidbc,var_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,km_e,lm_e,work,rc,amiss,fid2,fidbp,fidbn )
            end if
            do k = 1, km_e
              do j = 1, jm_e
                do i = 1, im_e
                  inField(i,j,k) = work(i,j,km_e+1-k)
                  if (inField(i,j,k) .gt. 0.01*undef) inField(i,j,k) = undef
                end do
              end do
            end do
         else
            print *, 'No dorev'
            if (index(var_name, ',') .gt. 0) then 
               rc = 1
            else
               if (nbkg .lt. 1 .or.  doInterp(iv) .lt. 1) then 
            call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, inField, rc, fid2 )
               else
            call InterpT(fid,fidbc,var_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,km_e,lm_e,inField,rc,amiss,fid2,fidbp,fidbn )
               end if
            end if
         end if
         if ( rc /= 0 )  then
            select case ( trim(outVars(iv)) )

!
            case ("HGHT", "hght")
               if ( trim(ze_name) .eq. 'ZE' .or. trim(ze_name) .eq. 'ze' ) then

            allocate (slp(im_e,jm_e), phis(im_e,jm_e), wz(im_e,km_e + 1,jm_e), stat = rc)
            if ( rc /= 0 )  call die (myname, 'cannot allocate SLP, PHIS, WZ')
            name_tmp = 'slp'
            call check_name(name_tmp, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, slp, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading SLP from ' &
                  //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            slp = slp * 0.01
            call check_name(phis_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, phis, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading PHIS '&
                 //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            call check_name(ze_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, ze_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, inField, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading ZE ' & 
                 //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            do i = 1, im_e
               do j = 1, jm_e
                  wz(i,km_e + 1,j) = phis(i,j) / grav
                  do k = 1, km_e
               wz(i,k,j) = inFIeld(i,j,k) /grav
                  end do
               end do
            end do

               else 

            allocate ( pt(im_e,jm_e,km_e), phis(im_e,jm_e),   &
                 slp(im_e,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pt and phis')
            call check_name(pt_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                   im_e, jm_e, 1, km_e, pt, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' & 
                 //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            call check_name(phis_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it),  &
                  im_e, jm_e, 0, 1, phis, rc, fid2 )
            if (rc /= 0)  call die (myname, 'can not get 2D phis ' //trim(inFile1) &
                  //' at ' //char_date //' ' //char_time //'z')
            allocate (pk(im_e,jm_e,km_e+1),pkz(im_e,jm_e,km_e),     &
               peln(im_e,km_e+1,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pk from ' &
                   //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            if ( inType .eq. 'lcv' ) then
               call geopm(ptop,pk,delp,pt,im_e,jm_e,km_e,1,jm_e,cpm,kappa,1)
               call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                   grid1%pe, pk, kappa, ks, peln, pkz, .false.)
            else
               call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                   grid1%pe, pk, kappa, ks, peln, pkz, .true. )
            end if
            call comp_slp(im_e,jm_e,km_e,pt,phis,delp,      &
                     grid1%pe,grid%pm,kappa,grav,slp)

            allocate ( wz(im_e,km_e + 1,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate wz')
            call comp_hght(ptop,im_e,jm_e,km_e,kappa,cpm,grav,delp, phis, pt, wz)
            wz = wz / grav
            compu_hght = .true.

               end if

!
            case ("ALT", "alt")
               if ( trim(ze_name) .eq. 'ZE' .or. trim(ze_name) .eq. 'ze' ) then

            allocate (slp(im_e,jm_e), phis(im_e,jm_e), wz(im_e,km_e + 1,jm_e), stat = rc)
            if ( rc /= 0 )  call die (myname, 'cannot allocate SLP, PHIS, WZ')
            name_tmp = 'slp'
            call check_name(name_tmp, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, slp, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading SLP from ' &
                   //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            slp = slp * 0.01
            call check_name(phis_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, phis, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading PHIS from '&
                    //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            call check_name(ze_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, ze_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, inField, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'something wrong in reading ZE from ' &
                //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            do i = 1, im_e
               do j = 1, jm_e
                  wz(i,km_e + 1,j) = phis(i,j) / grav
                  do k = 1, km_e
               wz(i,k,j) = inFIeld(i,j,k) /grav
                  end do
               end do
            end do

               else 

            allocate ( pt(im_e,jm_e,km_e), phis(im_e,jm_e),   &
                 slp(im_e,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pt and phis')
            call check_name(pt_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                   im_e, jm_e, 1, km_e, pt, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' &
               //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            call check_name(phis_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it),  &
                  im_e, jm_e, 0, 1, phis, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 2D phis from ' &
               //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            allocate (pk(im_e,jm_e,km_e+1),pkz(im_e,jm_e,km_e),     &
               peln(im_e,km_e+1,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pk')
            if ( inType .eq. 'lcv' ) then
               call geopm(ptop,pk,delp,pt,im_e,jm_e,km_e,1,jm_e,cpm,kappa,1)
               call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                   grid1%pe, pk, kappa, ks, peln, pkz, .false.)
            else
               call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                   grid1%pe, pk, kappa, ks, peln, pkz, .true. )
            end if
            call comp_slp(im_e,jm_e,km_e,pt,phis,delp,      &
                     grid1%pe,grid%pm,kappa,grav,slp)

            allocate ( wz(im_e,km_e + 1,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate wz')
            call comp_hght(ptop,im_e,jm_e,km_e,kappa,cpm,grav,delp, phis, pt, wz)
            wz = wz / grav
            compu_alt = .true.

               end if

!
            case ("DENS", "dens")
            if ( inType .ne. 'lcv' ) call die (myname, 'Can not compute density')
            allocate ( pt(im_e,jm_e,km_e), phis(im_e,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate pt and phis')

            call check_name(pt_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                   im_e, jm_e, 1, km_e, pt, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' &
               //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            call check_name(phis_name, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it),  &
                  im_e, jm_e, 0, 1, phis, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 2D phis from ' &
               //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')

            call comp_density(ptop,im_e,jm_e,km_e,kappa,cpm,grav,delp, phis, pt, inField)
            compu_dens = .true.

!
            case ("RH", "rh")
               allocate ( pt(im_e,jm_e,km_e), sphu(im_e,jm_e,km_e),   &
                    stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pt and sphu')
               call check_name(pt_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, pt, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' &
             //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               call check_name(sphu_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, sphu_name, yyyymmdd(it), hhmmss(it),  &
               im_e, jm_e, 1, km_e, sphu, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D sphu from ' &
             //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               
               allocate (pk(im_e,jm_e,km_e+1),pkz(im_e,jm_e,km_e),     &
                   peln(im_e,km_e+1,jm_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pk')
               if ( inType .eq. 'lcv' ) then
            call geopm(ptop,pk,delp,pt,im_e,jm_e,km_e,1,jm_e,cpm,kappa,1)
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .false.)
               else
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .true. )
               end if
               pt = pt*pkz / (1. + zvir*sphu)

               allocate (pmk(im_e,jm_e),tfield(im_e,jm_e),qsfield(im_e,jm_e),stat=rc)
               if ( rc /= 0 )  call die (myname, 'can not allocate pmk')

               call gestbl()

               do k = 1, km_e
            do j = 1, jm_e
               do i = 1, im_e
                  pmk(i,j) = 100*grid1%pm(i,k,j)
                  tfield(i,j) = pt(i,j,k)
               end do
            end do

            call vqsat ( tfield, pmk, qsfield, im_e*jm_e, undef )

            do j = 1, jm_e
               do i = 1, im_e
                  inField(i,j,k) = qsfield(i,j)
               end do
            end do

               end do

               inField = 100 * sphu*(1.-inField) / (inField*(1.-sphu)) 
               compu_rh = .true.

!
            case ("TMPU", "tmpu")
               allocate ( pt(im_e,jm_e,km_e), sphu(im_e,jm_e,km_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pt and sphu')
               call check_name(pt_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, pt, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D pt from ' &
            //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               call check_name(sphu_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, sphu_name, yyyymmdd(it), hhmmss(it),  &
               im_e, jm_e, 1, km_e, sphu, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D sphu from ' &
            //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               allocate (pk(im_e,jm_e,km_e+1),pkz(im_e,jm_e,km_e),     &
                   peln(im_e,km_e+1,jm_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pk')
               if ( inType .eq. 'lcv' ) then
            call geopm(ptop,pk,delp,pt,im_e,jm_e,km_e,1,jm_e,cpm,kappa,1)
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .false.)
               else
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .true. )
               end if
               inField = pt*pkz / (1. + zvir*sphu)
               compu_tmpu = .true.

!
            case ("DTRAIN", "dtrain")
               allocate ( arg1(im_e,jm_e,km_e), arg2(im_e,jm_e,km_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate arg1 and arg2')

               name_tmp = tmp_name1
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, arg1, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D tmp_name1 from ' &
             //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               name_tmp = tmp_name2
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, arg2, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D tmp_name2 from ' &
            //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')

               if ( index(op, '+' ) .gt. 0 ) then
            inField = arg1 + arg2
               else
            inField = arg1 -arg2
               end if

!
            case default
            print *, 'DEFAULT.', var_name
               if ( doRev ) then
            call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, work, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D VAR ' //trim(var_name) &
              // ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            do k = 1, km_e
             do j = 1, jm_e
               do i = 1, im_e
                  inField(i,j,k) = work(i,j,km_e+1-k)
                  if (inField(i,j,k) .gt. 0.01*undef) inField(i,j,k) = undef
               end do
             end do
            end do
               else
            call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, inField, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 3D VAR ' //trim(var_name) &
             // ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               print *, 'GFIO_GetVarT ',var_name
               end if

            end select
         end if

         if ( wind_field ) then
            print *, 'WIND_FIELD'
            allocate (inFieldw(im_e,jm_e,km_e),outFieldw(im,jm,km), stat = rc )
            if (rc /= 0)  call die (myname,'can not allocate inFieldw, outFieldw')
            if ( doRev ) then
               if (nbkg .lt. 1 .or.  doInterp(iv) .lt. 1) then 
            call GFIO_GetVarT ( fid, v_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, work, rc, fid2 )
               else
            call InterpT(fid,fidbc,v_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,km_e,lm_e,work,rc,amiss,fid2,fidbp,fidbn )
               end if
               if ( rc /= 0 )  call die (myname, 'can not get 3D VAR ' //trim(var_name) //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               do k = 1, km_e
          do j = 1, jm_e
            do i = 1, im_e
               inFieldw(i,j,k) = work(i,j,km_e+1-k)
            end do
          end do
               end do
            else
               if (nbkg .lt. 1 .or.  doInterp(iv) .lt. 1) then 
            call GFIO_GetVarT ( fid, v_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, inFieldw, rc, fid2 )
               else
            call InterpT(fid,fidbc,v_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,km_e,lm_e,inFieldw,rc,amiss,fid2,fidbp,fidbn )
               end if
               if ( rc /= 0 )  call die (myname, 'can not get 3D VAR ' //trim(var_name) //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z' )
            end if
         end if
      else                     ! 2D file
         nobs = im * jm 
         allocate ( conf(nobs), lon3d(nobs), lat3d(nobs), lev3d(nobs),     &
              inField(im_e, jm_e, 1), outField(im, jm, 1),stat = rc )
         if ( rc /= 0 )  call die (myname, 'can not allocate conf, lon3d')
        
         if (trim(outVars(iv)) .eq. 'SLP' .or. trim(outVars(iv)) .eq. 'slp' ) then
            name_tmp = 'slp'
            call check_name(name_tmp, nVars_e, vName, rc)
            if (rc .eq. 0) then
               call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it),  &
             im_e, jm_e, 0, 1,  inField, rc, fid2 )
            end if
            if ( rc /= 0 )  then
               allocate ( pt(im_e,jm_e,km_e), phis(im_e,jm_e),   &
                    slp(im_e,jm_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pt and phis')
               call check_name(pt_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, pt_name, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 1, km_e, pt, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D pt ' //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               call check_name(phis_name, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, phis_name, yyyymmdd(it), hhmmss(it),  &
               im_e, jm_e, 0, 1, phis, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 3D phis ' //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               allocate (pk(im_e,jm_e,km_e+1),pkz(im_e,jm_e,km_e),     &
                  peln(im_e,km_e+1,jm_e), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate pk')
               if ( inType .eq. 'lcv' ) then
            call geopm(ptop,pk,delp,pt,im_e,jm_e,km_e,1,jm_e,cpm,kappa,1)
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .false.)
               else
            call pkez(im_e,jm_e,km_e,1, jm_e, ptop,      &
                grid1%pe, pk, kappa, ks, peln, pkz, .true. )
               end if
               call comp_slp(im_e,jm_e,km_e,pt,phis,delp,      &
                  grid1%pe,grid%pm,kappa,grav,inField) 
               compu_slp = .true.
            end if
         else if ( trim(outVars(iv)) .eq. 'PREACC' .or. trim(outVars(iv)) .eq. 'OSR' &
             .or. trim(outVars(iv)) .eq. 'OSRCLR' .or. trim(outVars(iv)) .eq. 'LWGDOWN' & 
             .or. trim(outVars(iv)) .eq. 'LWGUP' .or.              &
             trim(outVars(iv)) .eq. 'preacc' .or. trim(outVars(iv)) .eq. 'osr' &
             .or. trim(outVars(iv)) .eq. 'osrclr' .or. trim(outVars(iv)) .eq. 'lwgdown' & 
             .or. trim(outVars(iv)) .eq. 'lwgup' ) then
            if (index(var_name, ',') .gt. 0 ) then
               rc = 1
            else
               call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it),       &
             im_e, jm_e, 0, 1,  inField, rc, fid2 )
            end if
            if ( rc /= 0 )  then
               allocate ( arg1(im_e,jm_e,1), arg2(im_e,jm_e,1), stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate arg1 and arg2')
               if ( trim(outVars(iv)) .eq. 'OSRCLR' .or. trim(outVars(iv)) .eq. 'osrclr') then
            tmp_name1 = tmp_osrclr1
            tmp_name2 = tmp_osrclr2
            op = op2
               end if
               if ( trim(outVars(iv)) .eq. 'OSR' .or. trim(outVars(iv)) .eq. 'osr' ) then
            tmp_name1 = tmp_osr1
            tmp_name2 = tmp_osr2
            op = op1
               end if
               if ( trim(outVars(iv)) .eq. 'LWGDOWN' .or. trim(outVars(iv)) .eq. 'lwgdown' ) then
            tmp_name1 = tmp_lwgdown1
            tmp_name2 = tmp_lwgdown2
            op = op3
               end if
               if ( trim(outVars(iv)) .eq. 'LWGUP' .or. trim(outVars(iv)) .eq. 'lwgup' ) then
            allocate ( arg3(im_e,jm_e,1), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate arg3')
            tmp_name1 = tmp_lwgup1
            tmp_name2 = tmp_lwgup2
               end if
               name_tmp = tmp_name1
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, arg1, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 2D tmp_name1 ' //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               name_tmp = tmp_name2
               call check_name(name_tmp, nVars_e, vName, rc)
               call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                im_e, jm_e, 0, 1, arg2, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 2D tmp_name2 ' //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               if ( trim(outVars(iv)) .eq. 'LWGUP' .or. trim(outVars(iv)) .eq. 'lwgup' ) then
            name_tmp = tmp_lwgup3
            call check_name(name_tmp, nVars_e, vName, rc)
            call GFIO_GetVarT ( fid, name_tmp, yyyymmdd(it), hhmmss(it), &
                   im_e, jm_e, 0, 1, arg3, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 2D tmp_name3 ' //&
              ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            inField = arg1 + arg2 - arg3
               else
            if ( index(op, '+' ) .gt. 0 ) then
               inField = arg1 + arg2 
            else
               inField = arg1 -arg2
            end if
               end if
            end if
         else if ( wind_field ) then
            allocate ( inFieldw(im_e, jm_e, 1),outFieldw(im,jm,1), stat = rc ) 
            if ( rc /= 0 )  call die (myname, 'can not allocate')
            if ( doInterp(iv) < 1 .or. nbkg < 1) then
               call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it),      &
             im_e, jm_e, 0, 1,  inField, rc, fid2 )
               if (rc /= 0)  call die (myname, 'can not get 2D VAR ' //trim(var_name) //&
              ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            else
               call InterpT(fid,fidbc,var_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,1,lm_e,inField,rc,amiss,fid2,fidbp,fidbn )
               if (rc /= 0)  call die (myname, 'can not get 2D VAR ' //trim(var_name) //&
             ' from ' //trim(bkgFiles(iff)) //' at ' //char_date //' ' //char_time //'z')
            endif
         else  
            if ( index(var_name, ',') .gt. 0 ) then
               rc = 1
            else
               if (doInterp(iv) < 1 .or. nbkg < 1 ) then
            call GFIO_GetVarT ( fid, var_name, yyyymmdd(it), hhmmss(it),       &
                im_e, jm_e, 0, 1,  inField, rc, fid2 )
            if ( rc /= 0 )  call die (myname, 'can not get 2D VAR ' //trim(var_name) //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
               else
            call InterpT(fid,fidbc,var_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,1,lm_e,inField,rc,amiss,fid2,fidbp,fidbn )
            if (rc /= 0)  call die (myname, 'can not get 2D VAR ' //trim(var_name) //&
             ' from ' //trim(bkgFiles(iff)) //' at ' //char_date //' ' //char_time //'z')
               end if
            end if
         end if

         if ( wind_field ) then
            if (doInterp(iv) < 1 .or. nbkg < 1 ) then
               call GFIO_GetVarT ( fid, v_name, yyyymmdd(it), hhmmss(it),      &
             im_e, jm_e, 0, 1,  inFieldw, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'can not get 2D VAR ' //(v_name) //&
           ' from ' //trim(inFile1) //' at ' //char_date //' ' //char_time //'z')
            else
               call InterpT(fid,fidbc,v_name,yyyymmdd(it),hhmmss(it),yyyymmdd,hhmmss_bkg, &
                timinc,im_e,jm_e,1,lm_e,inFieldw,rc,amiss,fid2,fidbp,fidbn )
            if (rc /= 0)  call die (myname, 'can not get 2D VAR ' //trim(var_name) //&
             ' from ' //trim(bkgFiles(iff)) //' at ' //char_date //' ' //char_time //'z')
            end if

         end if
      end if
   end subroutine readVar


!  This routine calls interpolation routines to do interpolation.

   subroutine Interpolation

!      inField = inField * scaleFactor(iv) + addOffset(iv) 
!      if ( trim(outVars(iv)) .eq. 'ALT' .or. trim(outVars(iv)) .eq. 'HGHT' .or. &
!           trim(outVars(iv)) .eq. 'alt' .or. trim(outVars(iv)) .eq. 'hght' ) &
!           wz = wz * scaleFactor(iv) + addOffset(iv)
      if (isPos(iv) .eq. 1) then
         if (num_levs .gt. 0 ) then
            do k = 1, km
              do j= 1, jm
		      do i = 1, im
			 if (inField(i,j,k) < 0) inField(i,j,k) = 0
          end do
              end do
            end do
         else   
            do j= 1, jm
		    do i = 1, im
		       if (inField(i,j,1) < 0) inField(i,j,1) = 0
              end do
            end do
         end if
      end if
        
!      if  ( wind_field ) inFieldw= inFieldw* scaleFactor(iv) + addOffset(iv)

!     Interpolate from Input to Output grid
!     -------------------------------------
!      if ( inType .eq. 'lcv' ) then
!         call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, delp = delp)
!      end if

!      if ( inType .eq. 'eta' ) then
!         call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, ps, ak, bk)
!      end if
        
      if ( num_levs > 0 ) then 
         call get_coords(im,jm,km,lon,lat,lev,lon3d,lat3d,lev3d)
         if ( wind_field ) then
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
               allocate(inFieldtmpv(im_e,jm_e,km_e),stat=rc )
               do k = 1, km_e
           do j = 1, jm_e
             do i = 1, im_e
                inFieldtmp(i,j,k) = inField(i,j,km_e+1-k)
                inFieldtmpv(i,j,k) = inFieldw(i,j,km_e+1-k)
             end do
           end do
               end do

               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                 im_e, jm_e, km_e, inFieldtmp, 1,        &
                 outField, conf, rc, inFieldtmpv, outFieldw, opt=1)
            else
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                 im_e, jm_e, km_e, inField, 1,        &
                 outField, conf, rc, inFieldw, outFieldw)
            end if
            if ( rc /= 0 )  call die (myname, 'something wrong in interpolation')
         else if ((trim(outVars(iv)) .eq. 'HGHT' .or. trim(outVars(iv)) .eq. 'ALT' .or. &
           (trim(outVars(iv)) .eq. 'hght' .or. trim(outVars(iv)) .eq. 'alt')) .and. &
            (compu_hght .or. compu_alt .or. ze_name .eq. 'ze' .or. ze_name .eq. 'ZE') ) then
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
 
               do k = 1, km_e
           do j = 1, jm_e
             do i = 1, im_e
                inFieldtmp(i,j,k) = wz(i,km_e+1-k,j)
             end do
           end do
               end do

               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, km_e, inFieldtmp, 0,     &
                    outField, conf, rc, opt = 1 )
            else
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, km_e, inField, 3,     &
                    outField, conf, rc, wz = wz, slp = slp )
            end if
            if ( rc /= 0 )  call die (myname, 'something wrong in interpolation')
            if (trim(outVars(iv)) .eq. 'ALT' .or. trim(outVars(iv)) .eq. 'alt') then 
               allocate ( alt(im,jm,km), stat=rc )
               call geopot2alt(nobs,outField,grav,lat3d,alt)
               outField = alt
            end if
         else
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
 
               do k = 1, km_e
           do j = 1, jm_e
             do i = 1, im_e
                inFieldtmp(i,j,k) = inField(i,j,km_e+1-k)
             end do
           end do
               end do

               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, km_e, inFieldtmp, 0,     &
                    outField, conf, rc, opt = 1 )
            else
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, km_e, inField, 0,     &
                    outField, conf, rc )
		  end if
            if ( rc /= 0 )  call die (myname, 'something wrong in interpolation')
         end if
      else 
         call get_coords(im,jm,1,lon,lat,lev,lon3d,lat3d,lev3d)
         if ( wind_field ) then
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                 im_e, jm_e, 1, inField, 1,           &
                 outField, conf, rc, inFieldw, outFieldw, opt=1 )
            else
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                 im_e, jm_e, 1, inField, 1,           &
                 outField, conf, rc, inFieldw, outFieldw )
            end if
            if ( rc /= 0 )  call die (myname, 'something wrong in interpolation')
         else
            if (outType .eq. 'alt' .or. outType .eq. 'ALT') then
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, 1, inField, 0,        &
                    outField, conf, rc, opt=1 )
            else
               call Interp_Field ( grid, lon3d, lat3d, lev3d, nobs,  &
                    im_e, jm_e, 1, inField, 0,        &
                    outField, conf, rc )
		  end if
            if ( rc /= 0 )  call die (myname, 'something wrong in interpolation')
         end if
      end if 
   end subroutine Interpolation
!  This routine does unit transfers.

   subroutine fixUnits()

      if (num_levs > 0 ) then 
         out_lev = km
      else
         out_lev = 1
      end if
      do k = 1, out_lev
        do j = 1, jm
          do i = 1, im
             if ( outField(i,j,k) < 0.1*undef ) then
                outField(i,j,k) = outField(i,j,k) * scaleFactor(iv) + addOffset(iv)
             else
                outField(i,j,k) = undef
             end if
             if ( wind_field ) then
                if (outFieldw(i,j,k) < 0.1*undef) then
                   outFieldw(i,j,k) = outFieldw(i,j,k) * scaleFactor(iv) &
                                                  + addOffset(iv)
                else
                    outFieldw(i,j,k) = undef
                end if
             end if
          end do
        end do
      end do
   end subroutine fixUnits


!  This routine writes out variables according to the rc file.

   subroutine writeOut()
      if ( num_levs > 0 ) then
         allocate ( write_out(im,jm,nLevs), stat = rc)
         if ( rc /= 0 )  call die (myname, 'cannot allocate write_out')
         if ( wind_field ) then 
            allocate ( write_outw(im,jm,nLevs), stat = rc)
            if ( rc /= 0 )  call die (myname, 'cannot allocate write_outw')
         end if

         if ( nLevs .ne. km ) then
            do j =1, jm
               do i = 1, im
            do k = 1, nlevs
               ii = 1
               do while ( Levs(k) .ne. lev(ii) ) 
                  ii = ii + 1
               end do
               write_out(i,j,k) = outField(i,j,ii)
               if ( wind_field ) then 
                  write_outw(i,j,k) = outFieldw(i,j,ii)
               end if
            enddo
               enddo
            enddo
         else
            write_out = outField
            if ( wind_field )  write_outw  = outFieldw
         end if
         if (lonshift) then
            print *, "Shifting ..."
            call lon_shift (write_out, im, jm, nLevs)   
            if ( wind_field ) then
               call lon_shift (write_outw, im, jm, nLevs)
            endif
         endif

         if ( wind_field ) then
            call GFIO_PutVar (out_fid,var_name_out,yyyymmdd(it),hhmmss(it),  &
                         im, jm, 1, nLevs, write_out, rc )
            if ( rc /= 0 )  call die (myname, 'can not write ' //var_name_out)
            call GFIO_PutVar (out_fid,v_name_out,yyyymmdd(it),hhmmss(it),    &
                         im, jm, 1, nLevs, write_outw, rc )
            if ( rc /= 0 )  call die (myname, 'can not write ' //v_name_out)
         else
            call GFIO_PutVar (out_fid,var_name_out,yyyymmdd(it),hhmmss(it),  &
                         im, jm, 1, nLevs, write_out, rc )
            if ( rc /= 0 )  call die (myname, 'can not write ' //var_name_out)
         end if 
      else

         if (lonshift) then
            print *, "Shifting ..."
            call lon_shift (write_out, im, jm, 1)
            if ( wind_field ) then
               call lon_shift (write_outw, im, jm, 1)
            endif
         endif

         if ( wind_field ) then
            call GFIO_PutVar (out_fid,var_name_out,yyyymmdd(it),hhmmss(it),  &
                         im, jm, 0, 1, outField, rc )
            if ( rc /= 0 )  call die (myname, 'can not write ' //var_name_out)
            call GFIO_PutVar (out_fid,v_name_out,yyyymmdd(it),hhmmss(it),    &
                         im, jm, 0, 1, outFieldw, rc )
            if ( rc /= 0 )  call die (myname, 'can not write ' //var_name_out)
         else
            call GFIO_PutVar (out_fid,var_name_out,yyyymmdd(it),hhmmss(it),  &
                         im, jm, 0, 1, outField, rc )
            if ( rc /= 0 )  call die(myname, 'can not write 2D file ' //var_name_out)
         end if 
      end if
   end subroutine writeOut

   subroutine deallocateMemory()

      deallocate( inField, outField )
      if ((trim(outVars(iv)) .eq. 'TMPU' .or. trim(outVars(iv)) .eq. 'tmpu')  &
         .and. compu_tmpu ) deallocate(pt,sphu,peln,pk,pkz)
      if ((trim(outVars(iv)) .eq. 'RH' .or. trim(outVars(iv)) .eq. 'rh')      &
         .and. compu_rh ) deallocate(pt,sphu,peln,pk,pkz)
      if ((trim(outVars(iv)) .eq. 'HGHT' .or. trim(outVars(iv)) .eq. 'hght')  &
         .and. compu_hght )  deallocate(wz,pt,phis,slp,peln,pk,pkz)
      if ((trim(outVars(iv)) .eq. 'HGHT' .or. trim(outVars(iv)) .eq. 'hght')  &
         .and. (pt_name .eq. 'ZE' .or. pt_name .eq. 'ze') ) &
         deallocate(wz,phis,slp)
      if ((trim(outVars(iv)) .eq. 'DENS' .or. trim(outVars(iv)) .eq. 'dens')  &
         .and. compu_dens )  deallocate(pt,phis)
      if ((trim(outVars(iv)) .eq. 'ALT' .or. trim(outVars(iv)) .eq. 'alt')  &
         .and. compu_alt )  deallocate(pt,phis)
      if ((trim(outVars(iv)) .eq. 'SLP' .or. trim(outVars(iv)) .eq. 'slp')    &
         .and. compu_slp )  deallocate(pt,phis,slp,peln,pk,pkz)
      if ( num_levs .gt. 0 ) deallocate( write_out )
      if ( num_levs .gt. 0 .and. wind_field ) deallocate( write_outw )
      if ( wind_field ) deallocate( inFieldw, outFieldw )
      if ( allocated (inFieldtmp) ) deallocate( inFieldtmp )
      if ((outType .eq. 'alt' .or. outType .eq. 'ALT') .and. wind_field )    &
         deallocate( inFieldtmpv )
      deallocate( lon3d, lat3d, lev3d, conf )
      if ( allocated (arg1) ) deallocate( arg1 )
      if ( allocated (arg2) ) deallocate( arg2 )
      if ( allocated (arg3) ) deallocate( arg3 )
      if ( allocated (pmk) ) deallocate( pmk )
      if ( allocated (tfield) ) deallocate( tfield )
      if ( allocated (qsfield) ) deallocate( qsfield )
   end subroutine deallocateMemory

!  This routine opens two files for time interpolation.

   subroutine openFile()
!    resolve template
     call strTemplate(inFile1, inFiles(iff), nymd = date )
     call strTemplate(outFile1, outFile, nymd = date )
!     call strTemplate(inFiles(iff), inFiles(iff), nymd = date )
      
!    Open GFIO file
!    --------------
     call GFIO_Open ( inFile1, in_fmode, fid, rc )
!     call GFIO_Open ( inFiles(iff), in_fmode, fid, rc )
     if ( rc /= 0 )  call die (myname, 'can not open input files '//inFile1 )
      
     if ( iff < nFiles ) then
        call GFIO_Open ( inFiles(iff+1), in_fmode, fid2, rc )
        if ( rc /= 0 )  call die (myname, 'can not open second input file '//inFiles(iff+1) )
     elseif ( inc_hhmmss .ne. 99999999 ) then
        call strTemplate(inFile2, inFiles(iff), nymd = INCYMD(date, 1) )
        call GFIO_Open ( inFile2, in_fmode, fid2, rc )
        if ( rc /= 0 )  call die (myname, 'can not open input files '//inFile2)
     else
        fid2 = fid
     end if
      
   end subroutine openFile

!............................................................................

    subroutine split_ ( tok, str, mList, List, nList )
    implicit NONE
    character(len=1), intent(in)  :: tok  ! delimitter
    character(len=*), intent(in)  :: str  ! string
    integer,          intent(in)  :: mList
    character(len=*), intent(out) :: List(mList) 
    integer,          intent(out) :: nList

    integer i, l, n, j

    i = 1
    l = len_trim(str)
    nList = 0
    do n = 1, mList
       j = index(trim(str(i:)),tok) - 1
       if ( j < 1 ) then
            nList = nList + 1
            List(nList) = str(i:)
            exit
       end if
       nList = nList + 1
       List(nList) = str(i:i+j-1) 
       i = i + j + 1
       if ( i > l ) exit
    end do

    end subroutine split_

    subroutine Usage_()
   
!
! Prints usage notice.
!

print *, "NAME"
print *, "   fv2prs  converts ETA or LCV files to Pressure Coordinates"
print *
print *, "SYNOPSIS"
print *, "   fv2prs  [options] input_fname(s)"
print *
print *, "OPTIONS"
print *
print *, "  -o fname    output file name. If the -o option doesn't exist, "
print *, "              The output file name will be 'fv2prs.prs.nc4' if 'eta' or 'lcv' "
print *, "              doesn't exist in the input files; The output file name will be "
print *, "              the same as the first input file except that 'eta' or 'lcv' in "
print *, "              the input files is replaced with 'prs'."
print *, "  -f fname    output file name. Same as -o option except that it will overwrite "
print *, "              output file if the output file exists.  "
print *, "  -rc rcfile  resource file name"
print *, "        Note: There are several resource files. fv2prs.rc (default) and"
print *, "              fv2prs.lcv.rc are the mostly used. fv2prs.lcv.rc should be"
print *, "              used for bkg.eta, prog.eta, ana.eta and ana.sfc input files."
print *, "              fv2prs.rc should be used for other input files."
print *, "              The standard location of the .rc files is in the 'etc' "
print *, "              directory of your build. The directory of resource files "
print *, "              for operations is /u/dao_ops/GEOS-4.0.1/etc/. "
print *, "  -date yyyymmdd  user specified date (one day chunk). The output will "
print *, "              contains data for that day only. "
print *, "  -inc  hhmmss    user specified time increment (e.g. -inc 30000) "
print *, "                  to do time interpolation."
print *, "  -start hhmmss   user specified output starting time. (e.g. -start 30000) "
print *, "  -cvs  A file name containing CVS version"
print *, "  -psf  fnames user specified file names for PS or SURFP."
print *, "               Using comma between files."
print *, "               File template can been used. For Example,  "
print *, "               -psf c402_flk_01.diag.sfc.20030715_1330z.nc4,c402_flk_01.diag.sfc.%y4%m2%d2_%h2%n2z.nc4 "
print *, "        Note: Surface pressure (PS or SURFP) is needed to compute"
print *, "              vertical coordinate for eta files (e.g. diag)."
print *, "              There will be an error message like 'can not get 2D VARPS' "
print *, "              if PS or SURFP could not be found from the input files." 
print *, "              In this case, you need provide PS file(s) (diag.sfc)." 
print *, "  -prec n     precision: "
print *, "                    n=0 for 32 bits (default) "
print *, "                    n=1 for 64 bits"
print *, "  -alt        converting eta files to height coordinate. Please remember to add "
print *, "              vertical_levels_in_meter: 1000 5000 ... in the .rc files. "
print *, "  -vars varn  actual variable names/classes, e.g.,"
print *, "                -vars HGHT,SLP,TMPU,@tavg2d_eng_x"
print *, "        OR      -vars UWND\;VWND,TMPU,SLP"
print *, "        Note: WIND related variables (UWND, VWND, U2M, V2M, U10M, V10M)"
print *, "              must appear in pairs (UWND;VWND). A '\' may be needed"
print *, "              before ';' if you select variables from the command line."
print *, "  -levels vertical levels, e.g.,                  "
print *, "                -levels 1000,500,100,0.2          "
print *, "  -im imOut     number of grid points output in longitude"
print *, "  -jm jmOut     number of grid points output in latitude"
print *, "  -west xwest   output starting longitude (degree)" 
print *, "  -lonshift     flag to shift longitudes 180 degrees before writing.  This should be used"
print *, "                with input data formatted as [-180 180]"
print *, "  -help         print User's Manual" 
print *
print *, "DESCRIPTION"
print *, "  Converts fvDAS files in either ETA or Lagrangian Control"
print *, "  Volume (LCV) coordinates to constant pressure coordinates"
print *, "  (PRS). If no option specified, all variables from input"
print *, "  files will be converted to 36 standard pressure levels."
print *
print *
print *, ">>>> PLEASE TYPE 'fv2prs.x -help' FOR MORE INFORMATION <<<<"
print *

    call die ( myname, 'exiting' )

    end subroutine Usage_

      
! .........................................................................

      subroutine pkez(im, jm, km, jfirst, jlast, ptop,  &
                     pe, pk, akap,  ks, peln, pkz, eta)
!
! eta: true on eta coordinate; pk needs to be updated

! true:
! Input:  pe
! Output: pk, pkz, peln

! false:
! Input:  pk, pe
! Output: pkz

! WS 99.05.19 : Added im, jm, km as arguments
! WS 99.07.27 : Limited region to jfirst:jlast

      implicit none

! WS 99.05.19 : Removed fvcore.h

      integer im, jm, km, jfirst, jlast
      real  pe(im, km+1, jm)
      real  pk(im, jm, km+1)
      real  pkz(im, jm, km)
      real peln(im, km+1, jm)
      real ptop

      integer ks
      logical eta

      real akap

! Local
      real  pk2(im, km+1)
      real pek
      real lnp

      integer i, j, k, j1, jmm0

      j1   = max(1,jfirst)
      jmm0 = min(jm,jlast)

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,pek,lnp,pk2)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,pek,lnp,pk2)
#endif

! WS 99.07.27 : Limited region to jfirst:jlast

!!!   do 1000 j=1, jm
      do 1000 j=j1, jmm0

      if ( eta ) then

! <<<<<<<<<<< Eta cordinate Coordinate  >>>>>>>>>>>>>>>>>>>

      pek =   ptop ** akap
      lnp = log(pe(1,1,j))

      do i=1,im
          pk2(i,1)   = pek
         peln(i,1,j) = lnp
      enddo

      if(ks .ne. 0) then
      do k=2, ks+1
             pek = pe(1,k,j)**akap
             lnp = log(pe(1,k,j))
         do i=1,im
             pk2(i,k)   = pek
            peln(i,k,j) =  lnp
         enddo
      enddo

      do k=1, ks
           pek = (       pk2(1,k+1)   - pk2(1,k))   /   &
                (akap*(peln(1,k+1,j) - peln(1,k,j)) )
           do i=1,im
              pkz(i,j,k) = pek
           enddo
      enddo

      endif

      do k=ks+2,km
         do i=1,im
            pk2(i,k) = pe(i,k,j)**akap
         enddo
      enddo

      do i=1,im
         pk2(i,km+1) = pk(i,j,km+1)
      enddo

      do k=ks+2,km+1
         do i=1,im
            peln(i,k,j) =  log(pe(i,k,j))
         enddo
      enddo

      do k=ks+1,km
         do i=1,im
            pkz(i,j,k) = (pk2(i,k+1) - pk2(i,k)) /     &
                        (akap*(peln(i,k+1,j) - peln(i,k,j)) )
         enddo
      enddo

      do k=2,km
         do i=1,im
            pk(i,j,k) = pk2(i,k)
         enddo
      enddo

      else

! <<<<<<<<<<< General Coordinate  >>>>>>>>>>>>>>>>>>>

      pek =   ptop ** akap
      lnp = log(pe(1,1,j))

      do i=1,im
          pk2(i,1) = pek
         peln(i,1,j) = lnp
      enddo

      do k=2,km+1
         do i=1,im
            peln(i,k,j) =  log(pe(i,k,j))
             pk2(i,k) =  pk(i,j,k)
         enddo
      enddo

        do k=1,km
           do i=1,im
              pkz(i,j,k) = (       pk2(i,k+1) - pk2(i,k) )  /  &
                          (akap*(peln(i,k+1,j) - peln(i,k,j)) )
           enddo
        enddo

      endif
1000  continue

      return
      end subroutine pkez

      subroutine  comp_slp(im,jm,km,pt,phis,delp,pe,pm,cappa,grav,inField)
      integer,        intent(in)  :: im
      integer,        intent(in)  :: jm
      integer,        intent(in)  :: km
      real,           intent(in)  :: pt(im,jm,km)
      real,           intent(in)  :: phis(im,jm)
      real,           intent(in)  :: delp(im,jm,km)
      real,           intent(in)  :: pe(im,km+1,jm)
      real,           intent(in)  :: pm(im,km,jm)
      real,           intent(in)  :: cappa        
      real,           intent(in)  :: grav         
      real,           intent(out) :: inField(im,jm,1)

      real Hmax, p_offset, p_bot
      integer k_bot, k1, k2
      real t_ref(im,jm), p_ref(im,jm)

      Hmax = 8.e3
      p_offset = 150. ! 150 hPa above surface

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p_bot,k_bot,k1,k2)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,p_bot,k_bot,k1,k2)
#endif

! Compute SLP and the  confidence level

! Find reference temperature by averaging Tv in a couple of
!  layers above the PBL.
      do j=1,jm
         do i = 1, im
            p_bot = pm(i,km,j) - p_offset
            k_bot = -1
            do k = km, 1, -1
               k_bot = k
               if ( pe(i,k+1,j) .lt. p_bot ) exit
            end do
            if ( k_bot .lt. 2 ) then
               print *, ': got  k_bot<2   while computing T_ref'
               call exit(1)
            else
               k1 = k_bot - 1
               k2 = k_bot
               t_ref(i,j) = ( pt(i,j,k1) * pkz(i,j,k1) * delp(i,j,k1) +  &
                             pt(i,j,k2) * pkz(i,j,k2) * delp(i,j,k2) )   &
                         / ( delp(i,j,k1) + delp(i,j,k2) )
               p_ref(i,j) = ( pe(i,k_bot+1,j) + pe(i,k_bot-1,j) ) / 2.
            end if
         end do

      end do


! calculated SLP and extrapolated surface temperature
         call slp_ukmo ( im, jm, T_ref, p_ref, pe(1:im,km+1,1:jm),   &
                        phis(1:im,1:jm)/grav, inField(1:im,1:jm,1) )

      end subroutine comp_slp 



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  slp_ukmo --- Computes sea-level pressure ("UKMO" algorithm)
!
! !INTERFACE:
!
      subroutine slp_ukmo ( im, jm, T_ref, p_ref, ps, zs, slp )

! !USES:
!
      USE m_const, only: grav
      USE m_const, only: rgas
      USE m_const, only: gamma
      Implicit NONE


! !INPUT PARAMETERS:

      integer, intent(in)   :: im, jm           ! grid dimensions

      real,    intent(in)   :: T_ref(im,jm)     ! Reference virtual temperature (K)
      real,    intent(in)   :: p_ref(im,jm)     ! Reference pressure level (hPa)
      real,    intent(in)   ::    ps(im,jm)     ! surface pressure (hPa)
      real,    intent(in)   ::    zs(im,jm)     ! topographic height (m)

! !OUTPUT PARAMETERS:

      real,    intent(out)  ::   slp(im,jm)     ! sea-level pressure (hPa)
                                                !   valid at the surface


! !DESCRIPTION: Let's assume that the virtual temperature at the {\em fictious}
!               layer under the ground is given by
!  \be
!        T(z) = T_* + \gamma z, \qquad \gamma \equiv = 6.5 K/Km
!  \ee
!  where $T_*$ is a temperature assumed to be valid at the surface, but different
!  from model's ground temperature (which is heavily affected by the diurnal cycle.)
!  Integrating the hydrostatic equation with this particular profile of $T(z)$
!  we obtain
!  \be
!       p_0 = p_s \( {T_* + \gamma z_s \over T_*} \)^{g/(R\gamma)}
!  \ee
!  where $z_s$ is the model's topographic height, and $p_0$ is the desired sea-level
!  pressure.
!
!  In order to avoid spurious diurnal cycle in the sea-level pressure, we do not
!  use the model's ground temperature for $T_*$. Instead, we extrapolated the
!  virtual temperature from a reference level just above the PBL (say, the fifth
!  eta layer, $p ~700$ hPa) using the constant moist adiabatic lapse rate $\gamma$,
!  viz.
!  \be
!          T_* = T_{ref} \( p_s / p_{ref} \) ^{g/(R\gamma)}
!  \ee
!  where $T_{ref}$ is a reference temperature valid at level $p_{ref}$.
!  For additional information consult Woodage (1985, Meteor. Mag., 114, 1-13)
!  and Ingleby (1995, Wea. Forecasting, 10, 172-182).
!
! !REVISION HISTORY:
!
!  20Mar2000  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

      integer i, j
      real factor, yfactor
      real T_star(im,jm)

      factor  = grav / ( Rgas * gamma )
      yfactor = ( Rgas * gamma ) / grav
      do j = 1, jm
         do i = 1, im
            T_star(i,j) = T_ref(i,j)                     &
                       * ( ps(i,j) / p_ref(i,j) ) ** yfactor
               slp(i,j) = ps(i,j)                        &
                       * (1.0 + gamma*zs(i,j)/T_star(i,j) ) ** factor
         end do
      end do

      end subroutine slp_ukmo

      subroutine get_coords(im,jm,km,lon,lat,lev,lon3d,lat3d,lev3d)

      integer,        intent(in)  :: im
      integer,        intent(in)  :: jm
      integer,        intent(in)  :: km
      real,           intent(in)  :: lon(im)
      real,           intent(in)  :: lat(jm)
      real,           intent(in)  :: lev(km)
      real,           intent(out)  :: lon3d(im*jm*km)
      real,           intent(out)  :: lat3d(im*jm*km)
      real,           intent(out)  :: lev3d(im*jm*km)
!
!     Generates lat-lon-lev coordinates of gridded fields for
!     interfacing with m_interp.
!
      character(len=*), parameter :: myname = 'get_coords'

      integer i, j, k, m

      m = 0

      do k = 1, km
         do j = 1, jm
            do i = 1, im
               m = m + 1
               lon3d(m) = lon(i)
               lat3d(m) = lat(j)
               lev3d(m) = lev(k)
            end do
         end do
      end do

    end subroutine get_coords


      subroutine geopm(ptop,pk,delp,pt,im,jm, km,jfirst, jlast, cp,akap,id)

      implicit none

! !INPUT PARAMETERS:

      integer,     intent(in)  ::  im
      integer,     intent(in)  ::  jm
      integer,     intent(in)  ::  km
      integer,     intent(in)  ::  jfirst
      integer,     intent(in)  ::  jlast
      integer,     intent(in)  ::  id
      real,        intent(in)  ::  akap
      real,        intent(in)  ::  cp
      real,        intent(in)  ::  ptop
      real,        intent(in)  ::  delp(im,jm,km)
      real,        intent(in)  ::  pt(im,jm,km)

! !OUTPUT PARAMETERS
      real,        intent(out) ::  pk(im,jm,km+1) 

! Local:
      real pk2(im,km+1)
      integer i, j, k
      real p2d(im,km+1)
      real ptk

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p2d,ptk, pk2)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,p2d,ptk, pk2)
#endif

      do 1000 j=jfirst,jlast

        ptk  = ptop ** akap

        do i=1,im
          p2d(i,1) = ptop
          pk2(i,1) = ptk
        enddo

! Top down
        do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
            pk2(i,k) = p2d(i,k) ** akap
          enddo
        enddo

! Bottom up

        if(id .ne. 0) then
          do k=1,km+1
            do i=1,im
              pk(i,j,k) = pk2(i,k)
            enddo
          enddo
        endif

1000  continue

      return
      end subroutine geopm

      subroutine comp_hght(ptop,im,jm,km,akap,cpm,grav,delp,phis,pt,wz)


! !INPUT PARAMETERS:
      real,        intent(in)  :: ptop
      integer,     intent(in)  :: im
      integer,     intent(in)  :: jm
      integer,     intent(in)  :: km
      real,        intent(in)  :: akap 
      real,        intent(in)  :: cpm
      real,        intent(in)  :: grav
      real,        intent(in)  :: delp(im, jm, km)
      real,        intent(in)  :: phis(im, jm)
      real,        intent(in)  :: pt(im, jm, km)

! !OUTPUT PARAMETERS
      real,        intent(out) :: wz(im, km+1, jm)

! !DESCRIPTION:
!     Compute HGHT

! !LOCAL:
      real pk2(im,km+1)
      integer i, j, k
      real p2d(im,km+1)
      real ptk

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p2d,ptk, pk2)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,p2d,ptk, pk2)
#endif

      do j = 1, jm
         ptk  = ptop ** akap

         do i=1,im
            p2d(i,1) = ptop
            pk2(i,1) = ptk
            wz(i,km+1,j) = phis(i,j) 
         end do

         do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
            pk2(i,k) = p2d(i,k) ** akap
          enddo
         enddo


         do k=km,1,-1
            do i=1,im
               wz(i,k,j) = wz(i,k+1,j) + cpm*pt(i,j,k)*(pk2(i,k+1)-pk2(i,k))
            end do
         end do

       end do

       end subroutine comp_hght


      subroutine comp_density(ptop,im,jm,km,akap,cpm,grav,delp,phis,pt,rho)


! !INPUT PARAMETERS:
      real,        intent(in)  :: ptop
      integer,     intent(in)  :: im
      integer,     intent(in)  :: jm
      integer,     intent(in)  :: km
      real,        intent(in)  :: akap 
      real,        intent(in)  :: cpm
      real,        intent(in)  :: grav
      real,        intent(in)  :: delp(im, jm, km)
      real,        intent(in)  :: phis(im, jm)
      real,        intent(in)  :: pt(im, jm, km)

! !OUTPUT PARAMETERS
      real,        intent(out) :: rho(im,jm,km)

! !DESCRIPTION:
!     Compute HGHT

! !LOCAL:
      real wz(im, km+1) ! height
      real pk2(im,km+1)
      integer i, j, k
      real p2d(im,km+1)
      real ptk, delh

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p2d,ptk, pk2)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,p2d,ptk, pk2)
#endif

      do j = 1, jm
         ptk  = ptop ** akap

         do i=1,im
            p2d(i,1) = ptop
            pk2(i,1) = ptk
         end do

         do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
            pk2(i,k) = p2d(i,k) ** akap
          enddo
         enddo


         do k = 1, km
            do i=1,im
               rho(i,j,k) = delp(i,j,k) &
                          / ( cpm*pt(i,j,k)*(pk2(i,k+1)-pk2(i,k)) )
            end do
         end do

       end do

       end subroutine comp_density


      SUBROUTINE geopot2alt(nvals,geopot,g0,lat,alt)

      IMPLICIT NONE

      integer, intent (in)                   :: nvals
      real, intent (in), dimension (1:nvals) :: geopot,lat
      real, intent (in)                      :: g0
      real, intent (out),dimension (1:nvals) :: alt

! SUBROUTINE geopot2alt
!
! calculates altimetric heights from geopotential heights
!
! INPUT
!    nvals: number of points in the arrays [geopot,lat,alt]
!    geopot: geopotential height [IN MGP]
!    g0: standard acceralation of gravity in which geopot is computed [IN M/S**2]
!    lat: latitude [IN DEGREES]
! OUPUT
!    alt: altimetric height [IN METERS]
! examples:
!    g0 is 9.80665 if you want to convert from mgp given by Radiosondes,
!    g0 is 9.80616 if you want to convert from mgp as expressed in the FV model
!    g0 is 9.8     if you want to check this routine versus Table 51 of the
!                  Smithsonian Meterological Tables, pages 222-223
!
!  REFERENCES : 
!         HANDBOOK OF GEOPHYSICS AND THE SPACE ENVIRONMENT
!              AIR FORCE GEOPHYSICS LABORATORY, USAF, 1985
!         SMITHSONIAN METEOROLOGICAL TABLES, List, 1968
!
! 12Feb2003	P.Poli	original code (derived from routines written
!			for GPS radio occultation)
!

      real, dimension (:), allocatable :: zg, zearthradius, &
                           zlat_rad, zsinlat, zsin2lat, ratiogs
      real, parameter :: pi=3.14159265358979323846264338328
      real :: deg2rad=pi/180.

      allocate(zg(nvals),zearthradius(nvals),zlat_rad(nvals),&
               zsinlat(nvals),zsin2lat(nvals),ratiogs(nvals))

      ! (1) g
      zlat_rad(:)=lat(:)*deg2rad
      zsinlat(:)=sin(zlat_rad)
      zsin2lat(:)=sin(2.*zlat_rad)
      zg = 9.780356 * (1.+ &
                &0.0052885*zsinlat*zsinlat- &
                &0.0000059*zsin2lat*zsin2lat)
      ratiogs(:)=zg(:)/g0
      ! (2) effective Earth radius
      zearthradius = 2.*zg/ (3.085462E-6 + &
       &2.27E-9*cos(2.*zlat_rad) - 2.E-12*cos(4.*zlat_rad))
!print *,'zearthradius,zg',zearthradius,zg

      alt (:) = zearthradius(:)*geopot(:)/&
           &(zearthradius(:)*ratiogs(:) - geopot(:))
      deallocate(zg,zearthradius,zlat_rad,zsinlat,zsin2lat,ratiogs)

      END SUBROUTINE geopot2alt


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  vqsat --- Compute saturation specific humidity
!                      (consistent with fvccm3-1.2.0)
!
! !INTERFACE:
!
      subroutine vqsat ( t, p, qs, len, undef )

! !USES:

      Implicit NONE

! !INPUT PARAMETERS:

      integer, intent(in)     :: len         ! vector length
      real, intent(in)        :: t(len)      ! temperature [K]
      real, intent(in)        :: p(len)      ! pressure [Pa]
      real, intent(in)        :: undef       ! undefined value

! !OUTPUT PARAMETERS:

      real, intent(out)       :: qs(len)     ! Saturation specific humidity [Pa]

! !DESCRIPTION: This routine computes saturation vapor pressure [Pa] and
!             saturation specific humidity [g/g] as a function of
!             temperature [K] and pressure [Pa].
!
!----------------------------Code History-------------------------------
! Original version:  J. Hack
! Standardized:      J. Rosinski, June 1992
!                    T. Acker, March 1996
! Reviewed:          J. Hack, August 1992
! Modified:          To handle pressure level data which can have undefined
!                    values.  This code was also stripped of the use of the
!                    land index array location.
!-----------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!  17nov2000  Dee    Obtained from Sharon Nebuda, taken from fvccm3-1.2.0
!                    Added DAO prologue; worried about name of developer.
!                    Modification: Allow array of pressures.
!
!EOP
!-------------------------------------------------------------------------

!
!--------------------------Local Variables------------------------------
!
      real omeps               ! 1 - 0.622
      real es(len)             ! Saturation vapor pressure [g/g]
      integer i,ii             ! Local vector indices
!
!-----------------------------------------------------------------------
!
! $Id: fv2prs.F90,v 1.6 2014-02-04 18:08:15 rtodling Exp $
! $Author: rtodling $
!
!
! Common block and statement functions for saturation vapor pressure
! look-up procedure, J. J. Hack, February 1990
!
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
!
! Table of saturation vapor pressure values es from tmin degrees
! to tmax+1 degrees k in one degree increments.  ttrice defines the
! transition region where es is a combination of ice & water values
!
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,  &
                 epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,   &
                 icephs

      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
!
! Dummy variables for statement functions
!
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up

! Statement functions used in saturation vapor pressure table lookup
! there are two ways to use these three statement functions.
! For compilers that do a simple in-line expansion:
! => ttemp = tlim(t)
!    es    = estbl4(ttemp)
!
! For compilers that provide real optimization:
! => es    = estblf(t)
!
      tlim(td) = max(min(td,tmax),tmin)
!
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)  &
                 *estbl(int(tlim(td)-tmin)+1)                     &
                 -(tmin + int(tlim(td)-tmin) - tlim(td)      )    &
                 *estbl(int(tlim(td)-tmin)+2)
!
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)  &
                 + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
!

!-----------------------------------------------------------------------
      omeps = 1.0 - epsqs
!     write(6,*) ' epsqs = ',epsqs
!DIR$ IVDEP
      do i=1,len
        if (t(i) .ne. undef) then
         es(i) = estblf(t(i))
!
! Saturation specific humidity
!
         qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))

!
! The following check is to avoid the generation of negative values
! that can occur in the upper stratosphere and mesosphere
!
         qs(i) = min(1.0,qs(i))
!
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p(i)
         end if
        else
          qs(i) = undef
          es(i) = undef
        endif
      end do

      return
!
      end subroutine vqsat

      subroutine gestbl
      use m_const, only: eps,cpm,rvap
!-----------------------------------------------------------------------
!
! Builds saturation vapor pressure table for later lookup procedure.
! Uses Goff & Gratch (1946) relationships to generate the table
! according to a set of free parameters defined below.  Auxiliary
! routines are also included for making rapid estimates (well with 1%)
! of both es and d(es)/dt for the particular table configuration.
!
!---------------------------Code history--------------------------------
!
! Original version:  J. Hack
! Standardized:      L. Buja, Jun 1992,  Feb 1996
! Reviewed:          J. Hack, G. Taylor, Aug 1992
!                    J. Hack, Aug 1992
!
!-----------------------------------------------------------------------
!
! $Id: fv2prs.F90,v 1.6 2014-02-04 18:08:15 rtodling Exp $
! $Author: rtodling $
!
!-----------------------------------------------------------------------
!------------------------------Arguments--------------------------------
!
! Input arguments
!
      real tmn           ! Minimum temperature entry in es lookup table
      real tmx           ! Maximum temperature entry in es lookup table
      real epsil         ! Ratio of h2o to dry air molecular weights
      real trice         ! Transition range from es over range to es over ice
      real latvap        ! Latent heat of vaporization
      real latice        ! Latent heat of fusion
      real rh2o          ! Gas constant for water vapor
      real cpair         ! Specific heat of dry air
!
!---------------------------Local variables-----------------------------
!
      real t             ! Temperature
      integer n          ! Increment counter
      integer lentbl     ! Calculated length of lookup table
      integer itype      ! Ice phase: 0 -> no ice phase
                         !            1 -> ice phase, no transition
                         !           -x -> ice phase, x degree transition
      logical ip         ! Ice phase logical flag
!
!---------------------------Statement function--------------------------
!
!
!
! $Id: fv2prs.F90,v 1.6 2014-02-04 18:08:15 rtodling Exp $
! $Author: rtodling $
!
!
! Common block and statement functions for saturation vapor pressure
! look-up procedure, J. J. Hack, February 1990
!
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
!
! Table of saturation vapor pressure values es from tmin degrees
! to tmax+1 degrees k in one degree increments.  ttrice defines the
! transition region where es is a combination of ice & water values
!
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,  &
                  epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,   &
                  icephs
!
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
!
! Dummy variables for statement functions
!
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
!
! Statement functions used in saturation vapor pressure table lookup
! there are two ways to use these three statement functions.
! For compilers that do a simple in-line expansion:
! => ttemp = tlim(t)
!    es    = estbl4(ttemp)
!
! For compilers that provide real optimization:
! => es    = estblf(t)
!
      tlim(td) = max(min(td,tmax),tmin)
!
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)  &
                 *estbl(int(tlim(td)-tmin)+1)                     &
                 -(tmin + int(tlim(td)-tmin) - tlim(td)      )    &
                 *estbl(int(tlim(td)-tmin)+2)
!
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)  &
                 + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
!

!-----------------------------------------------------------------------
!
! Set es table parameters
!
      tmin   = 173.16       ! Minimum temperature entry in table
      tmax   = 375.16       ! Maximum temperature entry in table
      ttrice = 20.          ! Trans. range from es over h2o to es over ice
      icephs = .true.       ! Ice phase (true or false)
!
! Set physical constants required for es calculation
!
!     epsqs  = 0.622
      epsqs  = eps
      hlatv  = 2.5104e6
      hlatf  = 3.336e5
!     rgasv  = 4.61e2
      rgasv  = rvap
!     cp     = 1004.64
      cp     = cpm
!
      lentbl = int(tmax-tmin+2.000001)
      if (lentbl .gt. plenest) then
         write(6,9000) tmax, tmin, plenest
         stop
      end if
!
! Begin building es table.
! Check whether ice phase requested.
! If so, set appropriate transition range for temperature
!
      if (icephs) then
         if(ttrice.ne.0.0) then
            itype = -ttrice
         else
            itype = 1
         end if
      else
         itype = 0
      end if
!
      t = tmin - 1.0
      do n=1,lentbl
         t = t + 1.0
         call gffgch(t,estbl(n),itype)
      end do
!
      do n=lentbl+1,plenest
         estbl(n) = -99999.0
      end do
!
! Table complete -- Set coefficients for polynomial approximation of
! difference between saturation vapor press over water and saturation
! pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
! is valid in the range -40 < t < 0 (degrees C).
!
!                  --- Degree 5 approximation ---
!
      pcf(1) =  5.04469588506e-01
      pcf(2) = -5.47288442819e+00
      pcf(3) = -3.67471858735e-01
      pcf(4) = -8.95963532403e-03
      pcf(5) = -7.78053686625e-05
 9000 format('GESTBL: FATAL ERROR *********************************',/, &
          ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH',    &
          ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/,   &
          ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
!
      end subroutine gestbl

      subroutine gffgch(t       ,es      ,itype   )
!-----------------------------------------------------------------------
!
! Computes saturation vapor pressure over water and/or over ice using
! Goff & Gratch (1946) relationships.  T (temperature), and itype are
! input parameters, while es (saturation vapor pressure) is an output
! parameter.  The input parameter itype serves two purposes: a value of
! zero indicates that saturation vapor pressures over water are to be
! returned (regardless of temperature), while a value of one indicates
! that saturation vapor pressures over ice should be returned when t is
! less than 273.16 degrees k.  If itype is negative, its absolute value
! is interpreted to define a temperature transition region below 273.16
! degrees k in which the returned saturation vapor pressure is a
! weighted average of the respective ice and water value.  That is, in
! the temperature range 0 => -itype degrees c, the saturation vapor
! pressures are assumed to be a weighted average of the vapor pressure
! over supercooled water and ice (all water at 0 c; all ice at -itype
! c).  Maximum transition range => 40 c
!
!---------------------------Code history--------------------------------
!
! Original version:  J. Hack
! Standardized:      L. Buja, Jun 1992,  Feb 1996
! Reviewed:          J. Hack, G. Taylor, Aug 1992
!                    J. Hack, Feb 1996
!
!-----------------------------------------------------------------------
!
! $Id: fv2prs.F90,v 1.6 2014-02-04 18:08:15 rtodling Exp $
! $Author: rtodling $
!
!-----------------------------------------------------------------------
!------------------------------Arguments--------------------------------
!
! Input arguments
!
      real t          ! Temperature
      integer itype   ! Flag for ice phase and associated transition
!
! Output arguments
!
      real es         ! Saturation vapor pressure
!
!---------------------------Local variables-----------------------------
!
      real e1         ! Intermediate scratch variable for es over water
      real e2         ! Intermediate scratch variable for es over water
      real eswtr      ! Saturation vapor pressure over water
      real f          ! Intermediate scratch variable for es over water
      real f1         ! Intermediate scratch variable for es over water
      real f2         ! Intermediate scratch variable for es over water
      real f3         ! Intermediate scratch variable for es over water
      real f4         ! Intermediate scratch variable for es over water
      real f5         ! Intermediate scratch variable for es over water
      real ps         ! Reference pressure (mb)
      real t0         ! Reference temperature (freezing point of water)
      real term1      ! Intermediate scratch variable for es over ice
      real term2      ! Intermediate scratch variable for es over ice
      real term3      ! Intermediate scratch variable for es over ice
      real tr         ! Transition range for es over water to es over ice
      real ts         ! Reference temperature (boiling point of water)
      real weight     ! Intermediate scratch variable for es transition
      integer itypo   ! Intermediate scratch variable for holding itype
!
!-----------------------------------------------------------------------
!
! Check on whether there is to be a transition region for es
!
      if (itype.lt.0) then
        tr    = abs(float(itype))
        itypo = itype
        itype = 1
      else
        tr    = 0.0
        itypo = itype
      end if
      if (tr .gt. 40.0) then
        write(6,900) tr
        stop
      end if
!
      if(t .lt. (273.16 - tr) .and. itype.eq.1) go to 10
!
! Water
!
      ps = 1013.246
      ts = 373.16
      e1 = 11.344*(1.0 - t/ts)
      e2 = -3.49149*(ts/t - 1.0)
      f1 = -7.90298*(ts/t - 1.0)
      f2 = 5.02808*log10(ts/t)
      f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
      f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
      f5 = log10(ps)
      f  = f1 + f2 + f3 + f4 + f5
      es = (10.0**f)*100.0
      eswtr = es
!
      if(t.ge.273.16 .or. itype.eq.0) go to 20
!
! Ice
!
   10 continue
      t0    = 273.16
      term1 = 2.01889049/(t0/t)
      term2 = 3.56654*log(t0/t)
      term3 = 20.947031*(t0/t)
      es    = 575.185606e10*exp(-(term1 + term2 + term3))
!
      if (t.lt.(273.16 - tr)) go to 20
!
! Weighted transition between water and ice
!
      weight = min((273.16 - t)/tr,1.0)
      es = weight*es + (1.0 - weight)*eswtr
!
   20 continue
      itype = itypo
      return
!
  900 format('GFFGCH: FATAL ERROR ******************************',/,  &
            'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR',     &
            ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF',      &
            ' 40.0 DEGREES C',/, ' TR = ',f7.2)
!
      end subroutine gffgch

      function incymd (nymd,m)
      implicit none
      integer nymd,m
      integer ny,nm,nd,incymd,ny00
      integer ndpm(12)
      data    ndpm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      logical leap
      data    ny00     / 1900 /
      leap(ny) = mod(ny,4).eq.0 .and. (ny.ne.0 .or. mod(ny00,400).eq.0)
!
      ny = nymd / 10000
      nm = mod(nymd,10000) / 100
      nd = mod(nymd,100) + m
!
      if (nd.eq.0) then
      nm = nm - 1
      if (nm.eq.0) then
          nm = 12
          ny = ny - 1
      endif
      nd = ndpm(nm)
      if (nm.eq.2 .and. leap(ny))  nd = 29
      endif

      if (nd.eq.29 .and. nm.eq.2 .and. leap(ny))  go to 20

      if (nd.gt.ndpm(nm)) then
      nd = 1
      nm = nm + 1
      if (nm.gt.12) then
          nm = 1
          ny = ny + 1
      endif
      endif
!
   20 continue
      incymd = ny*10000 + nm*100 + nd
      return
      end function incymd

    subroutine help_()
   
!
! Prints help notice.
!

write(6,6000) 
6000 format ( "fv2prs.x is a tool to convert ETA or LCV files to pressure coordinates. ",& 
 
//"PLEASE NOTE: fv2prs.x has several versions. Not all versions support ",&
/"all the options. Try to use the latest version, which can be found in the ",&
/"operational build of fvDAS, e.g., /u/dao_ops/GEOS-4.0.1/bin/. " &
  
//"The SYNOPSIS is  ",&
  
//">fv2prs.x [options] input_fname(s)  ",&
  
//"1) Running fv2prs.x without options  ",&
  
//">fv2prs.x input_fname(s)  ",&
  
//"fv2prs.x will convert all variables from eta to pressure coordinates.  ",&
/"The output pressure coordinates will be on 36 standard pressure  ",&
/"levels (1000 975 950 925 900 875 850 825 800 750 700 650 600 550 500",&  
/"450 400 350 300 250 200 150 100 70 50 40 30 20 10 7 5 3 2 1 0.4 0.2).  ",&
/"The lat lon and other meta data will be the same as those in the input  ",&
/"files. The output file name could be fv2prs.prs.nc4 if 'eta' or 'lcv'  ",&
/"doesn't exist in the input files. The output file name could be the  ",&
/"same as the first input file except that 'eta' or 'lcv' in the input  ",&
/"files is replaced with 'prs'. The output file will include all the  ",&
/"time steps existed in the input files.  ",&
  
//"2) Running fv2prs.x with options  ",&
  
//"> fv2prs.x options input_fname(s)  ",&
  
//"The options are  ",&
//"  -o fname    output file name. If the -o option doesn't exist, " &
/"              The output file name will be 'fv2prs.prs.nc4' if 'eta' or 'lcv' " &
/"              doesn't exist in the input files; The output file name will be " &
/"              the same as the first input file except that 'eta' or 'lcv' in " &
/"              the input files is replaced with 'prs'. If template is used in  ",&
/"              input/output file names, the -date option should be present.  ",&
/"  -f fname    output file name. Same as -o option except that it will overwrite ", &
/"              output file if the output file exists.  ", &
/"  -rc rcfile  resource file name. fv2prs.rc and fv2prs.lcv.rc are the  ",&
/"              two resource files that are used most. fv2prs.rc is for  ",&
/"              diag (diag.eta and diag.sfc) files, and fv2prs.lcv.rc is for ana(lcv) ",&
/"              (ana.eta and ana.sfc), bkg.eta and prog.eta files. Please use the  ",&
/"              correct resource file, otherwise fv2prs will fail because the ",& 
/"              resource file and the input files do not match. The  ",&
/"              resource files can be modified to meet your need.  ",&
//"              The variable cases in output files are determined by the output " &
/"              variables in the Variable_translation_table in resource files." &
//"              The variables in the Variable_translation_table should be " &
/"              in special order. For example, " &
/"              OLR        W/m2       FLNT                   1.0        0.0 " &
/"              OLRCLR     W/m2       FLNTC                  1.0        0.0 " &
/"              is the correct order. Switch between these two lines will have an error " &
/"              like 'fv2prs: cannot get scale factor'. This is a 'feature' " &
/"              i90_loadf function has. " &
/"              fv2prs.rc is the DEFAULT resource file. If no resource file is "&
/"              specified in the command line, it will try to find fv2prs.rc in your "&
/"              current directory. If no resource file is found, it will fail with a "&
/"              message like 'i90_loadf: opntext() error, ios =    2'."&
/"  -date yyyymmdd  ",&
/"              user specified date. This option is mainly for operation.  ",&
/"              Sometimes, two or more input files are needed to produce  ",&
/"              the required output. The output file will only contains  ",&
/"              time steps for the specified date no matter how many days  ",&
/"              the input file covers. Note: if -date option is used,  ",&
/"              the starting time will be the first time step from input files  ",&
/"              for that day.  ",&
/"  -inc hhmmss user specified time increment (e.g. -inc 30000). This  ",&
/"              option is for doing time interpolation. The default time  ",&
/"              increment is from the input file time increment. This  ",&
/"              option should be used with -date option together.  ",&
/"  -start      user specified output starting time. It should be used with ",&
/"              -date and -inc options together. ",&
/"  -psf fnames user specified file names for PS or SURFP. This option",&  
/"              is for diag.eta files. To do the conversion, PS or SURFP ",& 
/"              is needed to create vertical coordinate for diag.eta files. ",& 
/"              Usually diag.sfc contains PS variable. diag.sfc can be used  ",&
/"              as surface pressure files. If more than one file is needed,  ",&
/"              use comma between files (e.g. -psf psf1,psf2,psf3) and  ",&
/"              these files should be sequential in time.  ",&
/"  -vars varn  actual variable names/classes, e.g.,  ",&
/"                -vars HGHT,SLP,TMPU,@tavg2d_eng_x  ",&
/"        OR      -vars UWND\;VWND,TMPU,SLP  ",&
/"        Note: WIND related variables (UWND, VWND, U2M, V2M, U10M, V10M)  ",&
/"              must appear in pairs (UWND;VWND). A '\' may be needed  ",&
/"              before ';' if you select variables from the command line. ",& 
/"              The '\' is not needed if they are in the variable classes.  ",&
/"  -cvs fname  file name containing description about CVS version  ",&
/"  -prec n     precision: n=0 for 32 bits (default)   n=1 for 64 bits ",& 
/"  -alt        converting eta files to height coordinate. Please remember to add " &
/"              vertical_levels_in_meter: 1000 5000 ... in the .rc files. " &
/"  -levels vertical levels  ",&
/"              (e.g. -levels 1000,500,100,0.2) Default is 36 standard levels.  ",&
/"  -im imOut   number of grid points output in longitude  ",&
/"  -jm jmOut   number of grid points output in latitude  ",&
/"  -west xwest output starting longitude (degree)  " &
)

    call die ( myname, 'exiting' )

    end subroutine help_

    subroutine check_name(vname, nVars, var_arr, rc)

    Implicit NONE

! !INPUT PARAMETERS:

    integer, intent(in)  :: nVars           
    character(len=255), intent(in)  :: var_arr(mVars)      

! !OUTPUT PARAMETERS:

    integer, intent(out)  :: rc              

! !IN/OUTPUT PARAMETERS:

    character(len=257), intent(inout)  :: vname           

    integer i

    rc = 1
    do i = 1, nVars
      if (uppercase(trim(vname)) .eq. uppercase(trim(var_arr(i))) ) then
         vname = var_arr(i)
         rc = 0
      end if
    end do
   
    end subroutine check_name

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!
! !ROUTINE:  InterpT -- Get a variable from ana and bkg files with interpolation
!
! !DESCRIPTION: This routine will read the request variable from ana if
!               the variable exist. If not, it will try to read previous
!               time step and next time step from ana and bkg files, and 
!               do interpolation.
!
! !INTERFACE:
!
      subroutine InterpT(fida1, fidbc, var_name, date, time, yyyymmdd, hhmmss,  &
             timinc, im, jm, km, lm, inField, rc, missing, fida2, fidbp,fidbn )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer, intent(in) ::     fida1            ! File handle for ana.eta
      integer, intent(in) ::     fidbc            ! File handle for bkg.eta
      character(len=*), intent(in) :: var_name    ! Variable name
      integer, intent(in) :: date             ! Year-month-day, e.g., 19971003
      integer, intent(in) :: time             ! Hour-minute-second, e.g., 120000
      integer, intent(in) :: yyyymmdd(:)     
      integer, intent(in) :: hhmmss(:)      
      integer, intent(in) :: timinc           ! time increment
      integer, intent(in) :: im               ! size of longitudinal dimension
      integer, intent(in) :: jm               ! size of latitudinal  dimension
      integer, intent(in) :: km               ! number of levels to read
      integer, intent(in) :: lm               ! number of time steps
      real, intent(in)    :: missing          !FILL value in the input files
      integer, intent(in) :: fida2   !file id for ana.sfc
      integer, intent(in) :: fidbp   !file id for bkg.sfc for previous time step
      integer, intent(in) :: fidbn   !file id for bkg.sfc for next time step

!
! !OUTPUT PARAMETERS:
!
      real, intent(out) :: InField(im,jm,km)  ! Gridded data read for this time
      integer, intent(out) ::  rc             ! Error return code:
                                              !  rc  = 0   all is well
                                              !  rc \= 0   abnormal exit 

! !REVISION HISTORY:
!
!  2003.06.11 B. Yin      Initial code.
!
!-------------------------------------------------------------------------
   real  tsa1(im,jm,km), tsa2(im,jm,km)   
   real tsb(im,jm,km), tsb1(im,jm,km), tsb2(im,jm,km)
   real deltp(im,jm,km), deltn(im,jm,km)
   integer il, ip, in, nymd, pstep, nstep
   logical doRead

   doRead = .false.
   rc = 0

!  If date and time can been found in yyyymmdd and hhmmss, read the requested variable
!  other than do interpolation.
   do il = 1, lm
      if (date .eq. yyyymmdd(il) .and. time .eq. hhmmss(il)) doRead = .true.
   end do

   if ( doRead ) then
     if (km >1) then
        call GFIO_GetVar (fida1, var_name, date, time, &
                          im, jm, 1, km, inField, rc )
     else
        call GFIO_GetVar (fida1, var_name, date, time, &
                          im, jm, 0, 1, inField, rc )
     endif
   else
     nymd = date
     pstep = time - timinc
     nstep = time + timinc
     if ( nstep .ge. 240000 ) then
        nymd = INCYMD(date, 1)
        nstep = nstep - 240000
     end if


     if (km >1) then            ! 3D
!       read a variable at current time step from a bkg file
        call GFIO_GetVar (fidbc, var_name, date, time,               &
                          im, jm, 1, km, tsb, rc )
!       read a variable at previous time step from a ana file
        call GFIO_GetVar (fida1, var_name, date, pstep, &
                          im, jm, 1, km, tsa1, rc )
!       read a variable at nest time step from a ana file
        call GFIO_GetVar (fida1, var_name, nymd, nstep, &
                          im, jm, 1, km, tsa2, rc )
        if ( rc /= 0 )  call GFIO_GetVar (fida2, var_name, nymd, nstep, &
                          im, jm, 1, km, tsa2, rc )
!       read a variable at previous time step from a bkg file
        call GFIO_GetVar (fidbp, var_name, date, pstep, &
                          im, jm, 1, km, tsb1, rc )
!       read a variable at next time step from a bkg file
        call GFIO_GetVar (fidbn, var_name, nymd, nstep, &
                          im, jm, 1, km, tsb2, rc )
     else                      ! 2D
!       read a variable at current time step from a bkg file
        call GFIO_GetVar (fidbc, var_name, date, time,               &
                          im, jm, 0, 1, tsb, rc )
!       read a variable at previous time step from a ana file
        call GFIO_GetVar (fida1, var_name, date, pstep, &
                          im, jm, 0, 1, tsa1, rc )
!       read a variable at nest time step from a ana file
        call GFIO_GetVar (fida1, var_name, nymd, nstep, &
                          im, jm, 0, 1, tsa2, rc )
        if ( rc /= 0 )  call GFIO_GetVar (fida2, var_name, nymd, nstep, &
                          im, jm, 0, 1, tsa2, rc )
!       read a variable at previous time step from a bkg file
        call GFIO_GetVar (fidbp, var_name, date, pstep, &
                          im, jm, 0, 1, tsb1, rc )
!       read a variable at next time step from a bkg file
        call GFIO_GetVar (fidbn, var_name, nymd, nstep, &
                          im, jm, 0, 1, tsb2, rc )
     end if

!    do interpolation

     do k = 1, km
       do j = 1, jm
         do i = 1, im
           if (tsa1(i,j,k) .gt. 0.99*missing .or.  &
               tsa2(i,j,k) .gt. 0.99*missing .or.  &
               tsb1(i,j,k) .gt. 0.99*missing .or.  &
               tsb2(i,j,k) .gt. 0.99*missing ) then
               
               inField(i,j,k) = missing
           else
              deltp(i,j,k) = tsa1(i,j,k) - tsb1(i,j,k)
              deltn(i,j,k) = tsa2(i,j,k) - tsb2(i,j,k)
              deltp(i,j,k) = 0.5*(deltp(i,j,k) + deltn(i,j,k))
              inField(i,j,k) = tsb(i,j,k) + deltp(i,j,k)
           endif
         enddo
       enddo
     enddo

   end if

   return
   end subroutine

   subroutine lon_shift(field, im, jm, km)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer, intent(in) :: im
      integer, intent(in) :: jm
      integer, intent(in) :: km
!
! !INPUT/OUTPUT PARAMETERS:
!
      real, intent(inout) ::     field (im, jm, km)
      integer i, j, k
      real tmp

      do k = 1, km
         do j = 1, jm
            do i = 1, im/2
               tmp = field(i,j,k)
               field(i,j,k) = field(i+im/2,j,k)
               field(i+im/2,j,k) = tmp
            enddo
         enddo
      enddo

      end subroutine lon_shift

end Program fv2prs



