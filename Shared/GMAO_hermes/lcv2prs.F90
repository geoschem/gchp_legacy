   Program lcv2prs

!-------------------------------------------------------------------------
!         NASA/GSFC, Global Modeling and Assimilation Office (GMAO)      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  lcv2prs --- converting eta to pressure level/subsetting eta
!                        files. 
!
! !USAGE: see the routine usage() below
!
! !USES:
!
   use m_set_eta, only: set_eta
   use m_set_eta, only: set_ncep72
   use m_set_eta, only: unset_ncep72
   use m_die
   use m_inpak90
   use m_interp
   use m_chars

   use ESMF_CFIOMod, only: &
        ESMF_CFIOFileClose,    &
        ESMF_CFIOFileCreate,   &
        ESMF_CFIOFileOpen,     &
        ESMF_CFIOVarReadT2,    &
#if defined(HDFEOS)
        ESMF_CFIODownBit,      &
#endif
        ESMF_CFIOVarWrite      

   use ESMF_CFIOFileMod, only: &
        ESMF_CFIO,                 &
        ESMF_CFIOCreate,           &
        ESMF_CFIODestroy,          &
        ESMF_CFIOGet,              &
        ESMF_CFIOSet

   use ESMF_CFIOGridMod, only: &
        ESMF_CFIOGrid,             &
        ESMF_CFIOGridCreate,       &
        ESMF_CFIOGridGet,          &
        ESMF_CFIOGridSet

   use ESMF_CFIOVarInfoMod, only: &
        ESMF_CFIOVarInfo,             &
        ESMF_CFIOVarInfoCreate,       &
        ESMF_CFIOVarInfoDestroy,      &
        ESMF_CFIOVarInfoGet,          &
        ESMF_CFIOVarInfoSet

   use ESMF_CFIOUTILMOD, only: getdate, CFIO_parseIntTime
   use m_StrTemplate

   use m_const, only: cpm,grav,kappa,zvir
   use MAPL_ConstantsMod
   use GEOS_UtilsMod, only: GEOS_Qsat

   Implicit NONE

! !DESCRIPTION: converting eta to pressure level/subsetting eta files.
!
! !REVISION HISTORY:
!
!  ??Dec2004  Baoyu Yin Initial design and implementation.
!  ??Jan2005  Baoyu Yin Added time selection.
!  ??Feb2005  Baoyu Yin Added hght, rh, slp and tmpu computation.
!  ??Oct2005  Baoyu Yin Added variable name case checking and added longName
!                     if users don't provide them.
!  ??Mar2006  Baoyu Yin Added variable name "H" for HGHT
!  ??Mar2006  Baoyu Yin Added km_e checking for 2D files.
!  21Mar2006  Todling   Declared explicit used internal procedures of ESMF_CFIO
!  ??May2006  Baoyu Yin Added -prsf option for converting diag.eta to pressre   
!  ??May2006  Baoyu Yin Fixed problems in converting 3D files to output with one level
!  ??Jun2006  Baoyu Yin Added conditional compilation for HDFEOS support
!  ??Jul2006  Baoyu Yin Added compQ option to compute specific humidity on pressure
!                             coordinate.
!  ??Aug2006  Baoyu Yin Modified RH defination.
!  ??Nov2006  Baoyu Yin Added options for history, institution, comments...
!                       in command lines to override whatever in input files
!                       or rc files.
!  ??Nov2006  Baoyu Yin Added StandardName: in the rc files to create CFIO
!                       compliant standard variable names.
!  ??Nov2006  Baoyu Yin Notes about RH and Q computation: RH is defined as 
!                       q / qs.
!                       RH is interpolated from eta to pressure.
!                       Q is interpolated from eta to pressure directly without -compQ option.
!                       With -compQ option, Q is interpolated from eta to pressure 
!                       directly above 100mb. For 1000mb -> 100mb, Q is computed on 
!                       pressure using RH in prs.
!  ??Nov2006  Baoyu Yin Added QC and TQC support.
!  ??Dec2006  Baoyu Yin fixed vwnd lon shift problem in prog.eta files.
!  ??Dec2006  Baoyu Yin Added support for IAU files.
!  ??Dec2006  Baoyu Yin Added support for converting tavg_v files to prs.
!  ??Mar2007  Baoyu Yin Added support for converting D grid to A grid for wind.
!  ??Mar2007  Baoyu Yin Added support for subsetting for ana.eta file.
!  ??Mar2007  Baoyu Yin Added support for shaving and compression. 
!  ??Apr2007  Baoyu Yin Fixed some memory leak.
!  19Oct2007  D Kokron  d2a was called with actual arguments (inField and vField)
!                       that were 3D real pointers, but the subroutine expects
!                       a 2D real. This violated the FORTRAN standard, and the
!                       Intel V10 compiler refused to compile.
!  04Dec2009  Todling   Fix dims of wz (luckily did not affect calculation!)
!-------------------------------------------------------------------------
!EOP
   character(len=*), parameter :: myNewName = 'lcv2prs'
  
!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------

      integer, parameter :: mFiles = 256       ! Max. number of input files
      integer, parameter :: mVars  = 256       ! Max. number of variables
      integer, parameter :: mLevs  = 256       ! Max. number of levels    


!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


      integer            :: nFiles             ! Actual number of input files
      character(len=256) :: inFiles(mFiles)    ! Input file names
      character(len=256) :: outFile            ! Output file name

      integer           :: km                  ! vertical dimension
      integer, pointer  :: levNums(:)          ! vertical level numbers
      integer           :: nLevs = 0           ! total number of levels
      real, pointer     :: Levs(:)             ! vertical levels
      character(len=25) :: cLevs(mLevs)        ! Character reprsentation of levels

      integer           :: nVars               ! Actual number of variables
      integer           :: nPsfs = 0           ! total number of input prs files
      character(len=255):: prsFiles(mFiles)    ! input prsf files
      character(len=64) :: outVars(mVars)      ! output variable names (nVars)
      character(len=64) :: inVars(mVars)       ! input variable names (nVars)
      character(len=64) :: outUnits(mVars)     ! Units of output variables (nVars)
      character(len=64) :: outUnitsg(mVars)    ! Units of output variables (nVars)
      character(len=64) :: uWind               ! U wind name for D grid
      character(len=64) :: vWind               ! V wind name for D grid
      integer           :: outPrec             ! Output file precision:
                                               ! 0 = 32 bits,  1 = 64bits
      real :: offSet(mVars)                    ! User defined addOffSet
      real :: scaleFactor(mVars)               ! User defined scale factor
      logical           :: onEdges = .false.   ! for diag_eta_edge files
      logical           :: doSubset = .false.  ! for 3D diag.eta files
      logical           :: all2D = .true.      ! for 2D files
      logical           :: oneStep = .false.   ! only one time step in output
      logical           :: eta = .true.        ! eta files (Not ana.eta)
      logical           :: doComputing = .false. ! computation of rh, slp..
      logical           :: doComp      = .false. ! do szip compression
      logical           :: doChunk     = .false. ! do szip compression
      logical           :: compQ       = .false. ! do Q computation on pressure 
      integer           :: isPos(mVars)
      character(len=256) :: cfioStandardName(mVars)    ! output var names

!                              -----------------------
!                                Variable Work Space
!                              -----------------------

      real, pointer ::  inField(:,:,:)         ! Input variable
      real, pointer ::  outField(:,:,:)        ! Output variable
      real, pointer ::  vField(:,:,:)          ! V wind Input
      real, pointer ::  vOutField(:,:,:)       ! V wind Output
      real, pointer ::  pl(:,:,:)              ! Input mid-eta level
      real, pointer ::  ple(:,:,:)             ! Input edge eta level
      real, pointer ::  delp(:,:,:)            ! Input delp 
      real, pointer ::  wz(:,:,:)              ! Input hght 
      real, pointer ::  tmpu(:,:,:)            ! Input hght 
      real, pointer ::  rh(:,:,:)              ! Input hght 
      real, pointer ::  tmpuP(:,:,:)           ! temperature on prs levels
      real, pointer ::  rhP(:,:,:)             ! rh on prs levels
      real, pointer ::  qsfieldP(:,:,:)        ! Qs on prs levels
      real, pointer ::  slp(:,:)               ! Input slp  
      real*4, pointer  :: pmk(:,:)
      real*4, pointer :: tfield(:,:)
      real, pointer :: qsfield(:,:)
      real, pointer ::  tqi(:,:,:)             ! Input TQI for TQC
      real, pointer ::  tql(:,:,:)             ! Input TQL for TQC

      integer       :: begDate      ! starting date of output
      integer       :: begDate_keep ! starting date of output
      integer       :: curDate      ! current date
      integer       :: begTime      ! starting time of output
      integer       :: begTime_keep ! starting time of output
      integer       :: incTime      ! time increment of output
      integer       :: curTime      ! current time
      integer       :: preTime      ! previous time
      logical       :: timeInterp

      real, pointer     :: ak(:)    ! model eta level parameter a
      real, pointer     :: bk(:)    ! model eta level parameter b
      integer           :: ks       ! interface level (not needed)
      real              :: ptop=0.01! top pressure level (not needed)
      real              :: pint     ! interface pressure level (not needed)

      integer iff                   ! file counter
      integer nnames                ! variable counter
      integer name_len              ! variable counter
      integer name_pos              ! variable counter
      integer tNames                ! variable counter
      integer it                    ! time counter
      integer iv                    ! variable counter
      integer itest, ii, i, j, k               
      integer          :: in_fmode = 1         ! non-zero for READ-ONLY
      integer          :: out_fmode = 0        ! 0 for READ-WRITE 
      integer          :: rc                   ! return error code
      integer          :: rtcode               ! return error code
      integer          :: gridIdx              ! grid index for the biggest km_e
      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            !
      integer          :: timinc               ! Time increment
      logical          :: ex = .false.         ! output file exist or not
      logical          :: twoDimVar            ! 2D variable 
      character(len=8) :: format
      logical          :: ncep72               ! use NCEP-like levels but augmented to 72


!                              -----------------------
!                                  Output Meta Data
!                              -----------------------

      integer :: im                            ! output zonal dimension
      integer :: jm                            ! output meridional dimension
      character(len=256) :: title              ! meta data title
      character(len=256) :: title_cvs          ! meta data title with a CVS tag
      character(len=256) :: source             ! data source
      character(len=256) :: source_cvs         ! data source with a CVS tag
      character(len=256) :: contact            ! contact org.   
      character(len=128) :: levunits           ! Vertical levels
      character(len=128) :: coordinate         ! pressure, eta or sigma?
      character(len=128) :: standard_name      ! standard name for coordinate
      character(len=512) :: history            ! file history
      character(len=512) :: convention         ! CFIO or COARDS
      character(len=512) :: institution
      character(len=512) :: references
      character(len=512) :: comment
      character(len=256) :: cvsFile            ! CVS file name containing CVS version
      character(len=256) :: cvsV               ! CVS version
      integer            :: cvsV_yes=0
      real               :: missing_val
      character(len=128) :: vtitle(mVars)      ! output title
      character(len=128) :: stitle(mVars)      ! variable standard name
      character(len=128) :: vunits(mVars)      ! output title
      character(len=256) :: vName(mVars)       ! output variable names (nVars)
      integer            :: outKm(mVars)       ! number of levels for variables
      integer            :: inKm(mVars)        ! number of levels for input variables
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)
      real, pointer     :: lon_o(:)            ! output longitudes in deg (im)
      real, pointer     :: lat_o(:)            ! latitudes in deg (jm)
      real, pointer     :: lev_o(:)            ! levels in hPa (km)
      real, pointer     :: lon3d(:)            ! output longitudes in deg (im)
      real, pointer     :: lat3d(:)            ! latitudes in deg (jm)
      real, pointer     :: lev3d(:)            ! levels in hPa (km)
      real, pointer     :: conf(:)   


!                              -----------------------
!                                  eta information 
!                              -----------------------

      integer           :: im_e                ! input zonal dimension       
      integer           :: jm_e                ! input meridional dimension       
      integer           :: km_e                ! input vertical dimension    
      integer           :: lm_e                ! input time dimension    
      integer           :: nVars_e             ! input number of variables   
      integer           :: nGrids              ! number of grids in input files
      real              :: undef               ! Missing value
      real, pointer     :: lon_e(:)            ! longitudes in deg (im)
      real, pointer     :: lat_e(:)            ! latitudes in deg (jm)
      real, pointer     :: lev_e(:)            ! levels in hPa (km)
      real*4, pointer     :: lev_e4(:)            ! levels in hPa (km)
      integer           :: kmVar_e(mVars)      ! Number of vertical levels for variables

      character(len=128) :: longName(mVars)    ! output var names
      character(len=128) :: standardName(mVars)    ! output var names
      character(len=64) :: outVarsgc(mVars)    ! Output variable names from rc file
      character(len=64) :: outVarsg(mVars)     ! Output variable names from rc file
      character(len=64) :: inVarsg(mVars)      ! Input variable names for CFIO 

      real              :: valid_range(2, mVars)
      real              :: packing_range(2, mVars)
      integer           :: ngatts              ! Number of attributes for GFIO

      real              :: xWest               ! starting point for lon   
      real              :: xSouth              ! starting point for lat   
      real              :: deltaPhi            ! diffeence of two grid point in lat
      type(int_grid)    :: grid                ! Output grid variable

! define ESMF_CFIO, ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects

      type(ESMF_CFIO) :: cfio_in               ! CFIO input file object
      type(ESMF_CFIO) :: cfio_in2              ! CFIO input file object
      type(ESMF_CFIO) :: cfio_out              ! CFIO output file object
      type(ESMF_CFIOVarInfo), pointer :: vars(:) ! CFIO input variable object
      type(ESMF_CFIOVarInfo), pointer :: cfioOutVars(:) ! CFIO output var obj.
      type(ESMF_CFIOGrid), pointer  :: grid_in(:)       ! input file grid object
      type(ESMF_CFIOGrid) :: grid_out          ! output file grid object

      real, pointer ::  ua(:,:), va(:,:)
      real, pointer ::  sinlon(:), coslon(:)
      real dl
      real ptopp(1)
      integer ksp(1)
      integer nbits
      integer nbit
      integer tSteps
      integer mykm
      integer :: hour, minute, sec, incSecs
      real, pointer ::  x(:,:)
      real*4, pointer ::  xr(:,:)
      real, allocatable :: tmp(:,:), tmp2(:,:)

!.................................................................................

!  Get user input
!  --------------
   call Init_ ( mFiles, nFiles, inFiles, outFile, cvsFile,       &
                im, jm, km, lon_o, lat_o, lev_o, nLevs, Levs,    &
                levNums, mVars, nVars, inVars, outVars, outUnits,&
                scaleFactor, offSet, isPos, eta, compQ,          &
                outPrec, begDate, incTime, begTime, outVarsgc,   &
                longName, cfioStandardName, onEdges, doSubset,   &
                oneStep, doComp, doChunk, nPsfs, prsFiles, nbits, &
                tSteps, format, ncep72 )

! Get CVS version if -cvs option is given.
! ----------------------------------------
   if (len(trim(cvsFile)) .ge. 1 ) then
      open (11, file=cvsFile, iostat=rc)
      if ( rc .eq. 0 )  then
         read(11, '(a)') cvsV
         cvsV_yes = 1
      end if
   end if

   begTime_keep = begTime
   begDate_keep = begDate
   curTime = begTime
   curDate = begDate

!  Loop over input files ...
!  -------------------------
   loop_file : do iff = 1, nFiles

!    Create input Grid and CFIO objects and Open GFIO file
!    --------------
     cfio_in =  ESMF_CFIOCreate(cfioObjName='input')
     call ESMF_CFIOSet(cfio_in, fName=inFiles(iff))

     cfio_in2 =  ESMF_CFIOCreate(cfioObjName='input2')
     if ( iff .lt. nFiles) then
        call ESMF_CFIOSet(cfio_in2, fName=inFiles(iff+1))
     else
        call ESMF_CFIOSet(cfio_in2, fName=inFiles(iff))
     end if
     call ESMF_CFIOFileOpen(cfio_in, in_fmode, rc=rc)
     if (rc .eq. -39) call die (myNewName, 'Can not open input file.')
     call ESMF_CFIOFileOpen(cfio_in2, in_fmode)

!    Determine on file
!    ------------------

!    Get Global metadata
!    -------------------
     call ESMF_CFIOGet(cfio_in, nVars=nVars_e, date=begDate, begTime=begTime, &
                timeInc=timinc, nSteps=lm_e, nGrids=nGrids)
!     call ESMF_CFIOGet(cfio_in, source=source,&
!               title=title, contact=contact, comment=comment,   &
!               history=history, convention=convention,          &
!               institution=institution, references=references )
!     if ( nGrids /= 1 ) call die (myNewName, 'Input file has more than one grid')

     allocate ( yyyymmdd(lm_e),hhmmss(lm_e), stat = rc )
!    Get yyyymmdd and hhmmss 
     do i = 1, lm_e
           call CFIO_parseIntTime ( timInc, hour, minute, sec )
           incSecs = hour*3600 + minute*60 + sec
           call GetDate(begDate, begTime, (i-1)*incSecs, &
                yyyymmdd(i), hhmmss(i), rc)
     end do

     if ( iff .eq. 1 ) then
     allocate(vars(nVars_e))
     do i = 1, nVars_e
       vars(i) = ESMF_CFIOVarInfoCreate()
     enddo
!    Get variable objects and variable metadata
!    ------------------------------------------
     call ESMF_CFIOGet(cfio_in, varObjs=vars)
     allocate(grid_in(nVars_e))
     do i = 1, nVars_e
        grid_in(i) = ESMF_CFIOGridCreate(gName='eta_grid')
        call ESMF_CFIOVarInfoGet(vars(i), vName=vName(i),  grid=grid_in(i),   &
                 twoDimVar=twoDimVar, vTitle=vtitle(i), vUnits=vunits(i), &
!                 addOffSet=offSet(i), scaleFactor=scaleFactor(i),            &
                 validRange=valid_range(:,i), packingRange=packing_range(:,i), &
                 amiss=undef, standardName=stitle(i) )
!       Get Grid information 
!       --------------------
        call ESMF_CFIOGridGet(grid_in(i), im=im_e, jm=jm_e, km=km_e)
        if ( twoDimVar ) then
           kmVar_e(i) = 0
        else
           kmVar_e(i) = km_e 
           all2D = .false.
        end if
     enddo

     km_e = kmVar_e(1)
     gridIdx = 1
     do i = 1, nVars_e-1
        if (kmVar_e(i+1) .gt. kmVar_e(i)) then
           km_e = kmVar_e(i+1)
           gridIdx = i+1
        end if
     end do

     end if
     
!    Allocate memory for meta data
     if ( km_e .eq. 0 ) km_e = 1
     if (iff .eq. 1) then
        allocate ( lon_e(im_e),lat_e(jm_e),lev_e(km_e), lev_e4(km_e))   
     end if
!    Get Input Grid Information
     if (iff .eq. 1) then
     if ( all2D ) then
        call ESMF_CFIOGridGet(grid_in(1), lon=lon_e, lat=lat_e)
     else
        call ESMF_CFIOGridGet(grid_in(gridIdx), lon=lon_e, lat=lat_e, lev=lev_e, &
                           coordinate=coordinate, levUnit=levunits, &
                           standardName=standard_name)
        lev_e4 = lev_e
      end if

     if ( all2D ) km = 1
!    print *,"DEBUG: nlevs=",nLevs
     if (nLevs .le. 0 ) then ! choose all levels
        nLevs = km
        allocate(Levs(nLevs), stat=rc)
        if ( doSubset ) then
           do k = 1, km
              Levs(k) = lev_e4(levNums(k))*10/10.
           end do
        else 
            Levs = lev_o
        end if
     end if

!    Set Output Grid Information
     grid_out = ESMF_CFIOGridCreate(gName='prs_grid')
!     if ( all2D ) then
!        call ESMF_CFIOGridSet(grid_out, im=im, jm=jm)
!        call ESMF_CFIOGridSet(grid_out, lon=lon_o, lat=lat_o)
!     else
        call ESMF_CFIOGridSet(grid_out, im=im, jm=jm, km=km)
        call ESMF_CFIOGridSet(grid_out, lon=lon_o, lat=lat_o, lev=Levs)
!     end if
     if ( doSubset ) then
!        call ESMF_CFIOGridSet(grid_out, coordinate=coordinate, levUnit=levunits, &
!                           standardName=standard_name)
!        print *, "coordinate, levunits, standard_name", coordinate, levunits, standard_name
!        if (index(coordinate, 'unknown') .gt. 0 .or. len(trim(coordinate)) .le. 0) &
!        ana.eta file doesn't have coordinate and standard_name metadata. Add them.
         if (.not. eta)  coordinate = 'eta' 
!        if (index(standard_name, 'unknown') .gt. 0 .or. len(trim(standard_name)) .le. 0) &
         if (.not. eta)  standard_name = 'model_layers'
        call ESMF_CFIOGridSet(grid_out, coordinate=coordinate, levUnit=levunits, &
                           standardName=standard_name)
!        call ESMF_CFIOGridSet(grid_out, coordinate='eta', levUnit=levunits, &
!                           formulaTerm ="PS + PL",                         &
!                           standardName="atmosphere_hybrid_sigma_pressure_coordinate")
     else
!        if ( .NOT. all2D ) then    
           call ESMF_CFIOGridSet(grid_out, coordinate='pressure', levUnit='hPa', &
                      standardName='pressure')
!        end if
     end if

!    Do time interpolation and selection.
     if (curDate .eq. -999) curDate = begDate
     if (curTime .eq. -999) curTime = begTime
!     if (begDate .ge. yyyymmdd(lm_e) .and. curTime .gt. hhmmss(lm_e) ) cycle
     if (begDate .ge. yyyymmdd(lm_e) .and. curTime .gt. hhmmss(lm_e) +timinc) cycle
     outKm = nLevs
     inKm = nLevs

     valid_range_prs(1,:) = valid_range(1,1)
     valid_range_prs(2,:) = valid_range(2,1)

     standardName = ''
!    Find long names, number of levels and other meta data
!    -------------------------------
     if (nVars .le. 0) then   ! No variables selected -- all vars included
        nVars = nVars_e
        do i = 1, nVars
           outVars(i) = vName(i)
           inVars(i) = vName(i)
           inVarsg(i) = vName(i)
           outVarsg(i) = vName(i)
           outVarsgc(i) = vName(i)
           longName(i) = vtitle(i)
           standardName(i) = stitle(i)
           outKm(i) = kmVar_e(i)
           inKm(i) = kmVar_e(i)
           if (outKm(i) .gt. 0 .and. nLevs .gt. 0) outKm(i) = nLevs
           if (inKm(i) .gt. 0 .and. nLevs .gt. 0) inKm(i) = nLevs
           valid_range_prs(:,i) = valid_range(:,i)
           packing_range_prs(:,i) = packing_range(:,i)      
           outUnits(i) = vunits(i)
           scaleFactor(i) = 1.0
           offSet(i) = 0.0
        end do
     end if

!    Construct GFIO output variable names for GFIO_Create
!    If u and v wind are presented, seperate them.
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
           nnames = nnames + 1
         end if
      end do

!    Select variables from input file

     nnames = 1
     do iv = 1, nVars
        if ( index(trim(inVars(iv)), ';' ) .gt. 0 ) then
           name_pos = index(trim(inVars(iv)), ';')
           name_len = len(trim(inVars(iv)))
           uWind = inVars(iv)(1:(name_pos-1))
           vWind  = inVars(iv)((name_pos+1):name_len)
           inVarsg(nnames) = uWind
           inVarsg(nnames+1) = vWind
           nnames = nnames + 2
         else
           inVarsg(nnames) = inVars(iv)
           nnames = nnames + 1
         end if
     end do
     tNames = nnames - 1

     do iv = 1, tNames
        do itest = 1, nVars_e
           if ( uppercase(inVarsg(iv)) .eq. uppercase(vName(itest)) ) then
              if ( outUnits(iv) .eq. "UNKNOWN" ) then
                 print *, "Units for ", trim(vName(itest))," not in rc file.  Will use ",trim(vunits(itest))
                 outUnitsg(iv)=trim(vunits(itest))
              endif
              outKm(iv) = kmVar_e(itest)
              inKm(iv) = kmVar_e(itest)
              if (outKm(iv) .gt. 0 .and. nLevs .gt. 0) outKm(iv) = nLevs
              if (outKm(iv) .eq. 1) outKm(iv) = 0
              if (len(trim(longName(iv))) .le. 0) longName(iv)=vtitle(itest)
!              if (trim(standardName(iv)) .ne. 'unknown') standardName(iv)=stitle(itest)
              if ( len(trim(stitle(itest))) .gt. 0 ) then
                 standardName(iv)=stitle(itest)
              else
                 standardName(iv)=longName(iv)    
              end if
              valid_range_prs(1,iv) = valid_range(1,itest)
              valid_range_prs(2,iv) = valid_range(2,itest)
              packing_range_prs(1,iv) = packing_range(1,itest)
              packing_range_prs(2,iv) = packing_range(2,itest)
           end if
        end do
     end do

     end if

!    Checking longName  and input varibale names
     ii = 0
     do iv = 1, tNames
        ii = ii + 1
        do itest = 1, nVars_e
!          print *, "DEBUG: inVars=",trim(inVars(iv)),"  vName=",trim(vName(itest))
           if ( uppercase(inVars(iv)) .eq. uppercase(vName(itest)) ) then
              if (len(trim(longName(ii))) .le. 0) longName(ii)=vtitle(itest)
!             print *, "DEBUG: inVars=",trim(inVars(iv)),"  vName=",trim(vName(itest))
              if ( uppercase(inVars(iv)) .eq. vName(itest) ) then 
                 inVars(iv) = uppercase(inVars(iv))
              elseif ( inVars(iv) .eq. lowercase(inVars(iv)) ) then
                 inVars(iv) = lowercase(inVars(iv))
              end if
           else
              if ( index(trim(inVars(iv)), ';' ) .gt. 0 ) then
                 name_pos = index(trim(inVars(iv)), ';')
                 name_len = len(trim(inVars(iv)))
                 uWind = inVars(iv)(1:(name_pos-1))
                 vWind  = inVars(iv)((name_pos+1):name_len)
                 if ( uppercase(uWind) .eq. uppercase(vName(itest)) ) then
                    if (len(trim(longName(ii))) .le. 0) longName(ii)=vtitle(itest)
                    if ( uppercase(uWind) .eq. vName(itest) ) then 
                       inVars(iv) = uppercase(inVars(iv))
                    else 
                       inVars(iv) = lowercase(inVars(iv))
                    end if
                 end if
                 if ( uppercase(vWind) .eq. uppercase(vName(itest)) ) then
                    ii = ii + 1
                    if (len(trim(longName(ii))) .le. 0) longName(ii)=vtitle(itest)
                 end if
              end if

           end if
        end do
!       print *, "DEBUG: inVars=",trim(inVars(iv))
     end do


!    GFIO output file is created only once.  
      if ( iff == 1 ) then
!        Set ptop, pint, ak and bk
         allocate(ak(km_e+1), bk(km_e+1))
         if(ncep72) call set_ncep72
         call set_eta(km_e,ks,ptop,pint,ak,bk)
         if(ncep72) call unset_ncep72

!        Create output GFIO file
!        -----------------------
         if (begDate < 0 .or. begTime < 0 .or. incTime < 0) then ! no time interpolation
            begDate = yyyymmdd(1)
            begTime = hhmmss(1)
            incTime = timinc 
            timeInterp = .false.
         else
            timeInterp = .true.
         end if

!        Append CVS tag to title and source
         call getTag(cvsV, title, source, cvsV_yes, title_cvs, source_cvs)

!  Create output file with all the required metadata
!  -------------------------------------------------

!        Set Variable metadata
         if (iff .eq. 1) then
            allocate(cfioOutVars(tNames))
         end if
         do iv = 1, tNames
             if (trim(outVarsgc(iv)) .eq. "SLP" .or. trim(outVarsgc(iv)) .eq. "slp" &
                 .or. trim(outVarsgc(iv)) .eq. "PS" .or.                            &
                 trim(outVarsgc(iv)) .eq. "ps"                                      &
                 .or. trim(outVarsgc(iv)) .eq. "TQC" .or.                           &
                 trim(outVarsgc(iv)) .eq. "tqc" )  then
                 outKm(iv) = 0
                 inKm(iv) = 0
             end if
             if ( outKm(iv) .gt. 0 ) then
                twoDimVar = .false.
             else
                twoDimVar = .true.
             end if
             cfioOutVars(iv) = ESMF_CFIOVarInfoCreate(vName=outVarsgc(iv))
             if (valid_range_prs(1,iv) .eq. valid_range_prs(2,iv) ) then
                valid_range_prs(1,iv) = -1.e30
                valid_range_prs(2,iv) = 1.e30
             end if
             if (len(trim(standardName(iv))) .lt. 1 .or.                      &
                 trim(standardName(iv)) .eq. 'unknown' ) standardName(iv) = outVarsgc(iv)
!             standardName(iv) = outVarsgc(iv)
             if ( len(trim(cfioStandardName(iv))) .ge. 1 ) standardName(iv) = cfioStandardName(iv)
             call ESMF_CFIOVarInfoSet(cfioOutVars(iv), vName=outVarsgc(iv),   &
                       vTitle=trim(longName(iv)), grid=grid_out, standardName=  &
                       trim(standardName(iv)), twoDimVar=twoDimVar, validRange= &
                       valid_range_prs(:,iv), vUnits=outUnitsg(iv),         &
                       amiss=undef, scaleFactor=1., addOffSet=0.)
         end do
            
!        Set CFIO metadata
         cfio_out =  ESMF_CFIOCreate(cfioObjName='cfio_out')
         call ESMF_CFIOSet(cfio_out, fName=outFile, varObjs=cfioOutVars, &
                 grid=grid_out, date=curDate, BegTime=curTime,           &
                 timeInc=incTime, prec=outPrec)
         call ESMF_CFIOSet(cfio_out, source=source_cvs,                  &
                   title=title_cvs, contact=contact, comment=comment,    &
                   history=history, convention=convention,           &
                   institution=institution, references=references )
#if defined(HDFEOS)
         if (tSteps .gt. 0) call ESMF_CFIOSet(cfio_out, nSteps=tSteps)
#endif
         if (.not. eta .and. doSubset) then
            ptopp = ptop
            call ESMF_CFIOSet(cfio_out, attRealName='ptop', attReal=ptopp )

            ptopp = pint
            call ESMF_CFIOSet(cfio_out, attRealName='pint', attReal=ptopp )
            ksp = ks
            call ESMF_CFIOSet(cfio_out, attIntName='ks', attInt=ksp )

            call ESMF_CFIOSet(cfio_out, attRealName='ak', attReal=ak )
            call ESMF_CFIOSet(cfio_out, attRealName='bk', attReal=bk )

            ksp = 15760
            call ESMF_CFIOSet(cfio_out, attIntName='nstep', attInt=ksp )
         end if

!         Eta2prs may run twice for variables written at mid-layer and edges.
          inquire(file=outFile, EXIST=ex)  
          if ( ex ) then
             call ESMF_CFIOFileOpen(cfio_out, out_fmode)
          else
             call ESMF_CFIOFileCreate(cfio_out, format = format)
          end if
      end if

      if (tNames .ne. nVars) then
         do i = 1, nVars
            if (index(inVars(i),";") .gt. 0)  then
               do ii = i+1, nVars
                  outKm(ii) = outKm(ii+1)
                  inKm(ii) = inKm(ii+1)
               end do 
             end if
             if (outKm(i) .eq. 1) outKm(i) =0
          end do
      end if
!     Loop over times on file
!     -----------------------
      loop_time : do it = 1, lm_e
        
        
!      do time interpolation if needed.
       loop_do : do

         if ( oneStep .and. (begDate_keep /=curDate .or. curTime /= begTime_keep)) then
             curTime = curTime + incTime
             if ( curTime .ge. 240000 ) then
                curTime = curTime - 240000
                curDate = incymd(curDate, 1)
             end if
             exit 
!             cycle loop_time
         end if
         if (.not. timeInterp .and. curDate .gt. yyyymmdd(it)) then
              exit
         endif
!         if (timeInterp .and. curDate .gt. begDate) exit   
     
         if ( begDate_keep /= -999 .and. curDate /= begDate_keep) then
              exit
         endif
!         if (begDate_keep .ge. yyyymmdd(lm_e) .and. curTime .gt. hhmmss(lm_e) )  &
!             cycle loop_file
!        Get DELP, and call Interp_Init for 3D variables
         if ( .NOT. doSubset .and. .NOT. all2D .and. .NOT. onEdges ) then
               if (.not. associated(delp)) allocate ( delp(im_e,jm_e,km_e),  stat=rc )
               if ( rc /= 0 )  call die (myNewName, 'can not allocate delp')
               if ( nPsfs .eq. 0 ) then
                  call checkStrictVar('delp', vName, nVars_e, rtcode)
                  if ( rtcode .eq. 0) then
                      call ESMF_CFIOVarReadT2(cfio_in, 'delp', curDate, curTime, delp, rc= rc, cfio2=cfio_in2)
                  else
                      call ESMF_CFIOVarReadT2(cfio_in, 'DELP', curDate, curTime, delp, rc= rc, cfio2=cfio_in2)
                  end if
                  if ( rc /= 0 )  call die (myNewName, 'can not read DELP')
               else
                  call checkStrictVar('delp', vName, nVars_e, rtcode)
                  if ( rtcode .eq. 0) then 
                     call readPrs(curDate, curTime, 'delp', im_e, jm_e, km_e, delp, rc)
                  else 
                     call readPrs(curDate, curTime, 'DELP', im_e, jm_e, km_e, delp, rc)
                  end if
                  if ( rc /= 0 )  call die (myNewName, 'can not read DELP')
               end if
               if (lon_e(1) < 0 .and. .not. doSubset ) call lon_shift(delp, im_e, jm_e, km_e)
               call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, delp=delp)
         end if
         if  ( onEdges ) then
            allocate(ple(im_e,jm_e,km_e), stat=rc )
            if ( rc /= 0 )  call die (myNewName, 'can not allocate ple')
            call checkStrictVar('PLE', vName, nVars_e, rtcode)
            if ( rtcode .eq. 0) then
               call ESMF_CFIOVarReadT2(cfio_in, 'PLE', curDate, curTime, ple, rc= rc, cfio2=cfio_in2)
            else
               call ESMF_CFIOVarReadT2(cfio_in, 'ple', curDate, curTime, ple, rc= rc, cfio2=cfio_in2)
            end if
            if (lon_e(1) < 0 .and. .not. doSubset ) call lon_shift(ple, im_e, jm_e, km_e)
            call Interp_Init(im_e, jm_e, km_e-1, ptop, grid, rc, pe=ple*0.01)
         end if

         if (all2D) call Interp_Init(im_e, jm_e, 1, ptop, grid, rc)
   
         doComputing = .false.

!        Loop over variables
!        -------------------
         loop_var : do iv = 1, nVars 
      
!           Check a varaiable to see whether it exist
            call checkVar(inVars(iv), vName, nVars_e, rc)
            if (rc .ne. 0 .and. trim(outVars(iv)) .eq. "TQC") eta = .false.
            if (rc .ne. 0 .and. (trim(outVars(iv)) .ne. "HGHT" .or. &
                trim(outVars(iv)) .ne. "H" .or.                     &
                trim(outVars(iv)) .ne. "RH" .or.                    &
                trim(outVars(iv)) .ne. "TMPU" .or.                  &
                trim(outVars(iv)) .ne. "T" .or. &
                trim(outVars(iv)) .ne. "QC" .or. &
                trim(outVars(iv)) .ne. "SLP") .and. eta) &
                 cycle loop_var
!            if (rc .ne. 0 .and. eta ) cycle loop_var
            if (rc .ne. 0 .and. (trim(outVars(iv)) == "HGHT" .or.   &
                  trim(outVars(iv)) == "H" .or.                     &
                  trim(outVars(iv)) == "RH" .or.                    &
                  trim(outVars(iv)) == "TMPU" .or.                  &
                  trim(outVars(iv)) == "T" .or.                     &
                  trim(outVars(iv)) == "SLP") .and.                 &
                  ( .NOT. doComputing) )  then
                   if (.not. associated(wz) ) allocate( wz(im_e,km_e+1,jm_e) )
                   if (.not. associated(slp) ) allocate( slp(im_e,jm_e) )
                   if (.not. associated(tmpu) ) allocate( tmpu(im_e,jm_e,km_e) )
                   if (.not. associated(rh) ) allocate( rh(im_e,jm_e,km_e) )
                   if (.not. associated(delp) ) allocate( delp(im_e,jm_e,km_e) )
               if ( doSubset ) then
               call ESMF_CFIOVarReadT2(cfio_in, 'delp', curDate, curTime, delp, rc= rc, cfio2=cfio_in2)
               call Interp_Init(im_e, jm_e, km_e, ptop, grid, rc, delp=delp)
               end if
               call compHght(cfio_in,curDate,curTime,delp,im_e,jm_e,km_e,lon_e(1),wz,slp,tmpu,rh,rc,cfio_in2)
               doComputing = .true.
            end if

            if ( compQ .and. ( .NOT. doComputing) ) then
               if (.not. associated(wz) ) allocate( wz(im_e,km_e+1,jm_e) )
               if (.not. associated(slp) ) allocate( slp(im_e,jm_e) )
               if (.not. associated(tmpu) ) allocate( tmpu(im_e,jm_e,km_e) )
               if (.not. associated(rh) ) allocate( rh(im_e,jm_e,km_e) )
               call compHght(cfio_in,curDate,curTime,delp,im_e,jm_e,km_e,lon_e(1),wz,slp,tmpu,rh,rc,cfio_in2)
               doComputing = .true.
            end if

!           Read variable from GFIO file
!           ----------------------------

!           read 3D variable
            if (inKm(iv) .gt. 0) then
               allocate ( inField(im_e,jm_e,km_e), stat=rc )
               if (rc .ne. 0) print *, "Can't allocate inField"
               call checkVar(inVars(iv), vName, nVars_e, rc)
               if ( doComputing .and. rc /= 0 ) then
                  if ( trim(outVars(iv)) .eq. "T" .or. trim(outVars(iv)) .eq. "TMPU") &
                      inField = tmpu
                  if ( trim(outVars(iv)) .eq. "RH" ) inField = rh
                  if ( rc /= 0 .and. (trim(outVars(iv)) .eq. 'QC' .or. trim(outVars(iv)) .eq. 'qc') ) then
                     allocate ( tql(im_e,jm_e,km_e), stat=rc )
                     allocate ( tqi(im_e,jm_e,km_e), stat=rc )
                     call ESMF_CFIOVarReadT2(cfio_in, 'qltot', curDate, curTime, tql, rc=rc, cfio2=cfio_in2)
                     if ( rc /= 0 )  call die (myNewName, 'can not get qltot from input files.')
                     call ESMF_CFIOVarReadT2(cfio_in, 'qitot', curDate, curTime, tqi, rc=rc, cfio2=cfio_in2)
                     if ( rc /= 0 )  call die (myNewName, 'can not get qitot from input files.')
                     inField = tqi + tql
                     deallocate(tqi, tql)
                  end if
               else
                  if (index(inVars(iv),";") .gt. 0) then
                     name_pos = index(trim(inVars(iv)), ';')
                     name_len = len(trim(inVars(iv)))
                     uWind = inVars(iv)(1:(name_pos-1))
                     vWind  = inVars(iv)((name_pos+1):name_len)
                     call ESMF_CFIOVarReadT2(cfio_in, uWind, curDate, curTime, inField, rc=rc, cfio2=cfio_in2)
                     allocate ( vField(im_e,jm_e,km_e), stat=rc )
                     call ESMF_CFIOVarReadT2(cfio_in, vWind, curDate, curTime, vField, rc=rc, cfio2=cfio_in2)
                     if ( lon_e(1) < 0 .and. .not. doSubset ) call lon_shift(vField, im_e, jm_e, km_e)
                  else   
                     if ( rc /= 0 .and. (trim(outVars(iv)) .eq. 'QC' .or. trim(outVars(iv)) .eq. 'qc') ) then
                        allocate ( tql(im_e,jm_e,km_e), stat=rc )
                        allocate ( tqi(im_e,jm_e,km_e), stat=rc )
                        call ESMF_CFIOVarReadT2(cfio_in, 'qltot', curDate, curTime, tql, rc=rc, cfio2=cfio_in2)
                        if ( rc /= 0 )  call die (myNewName, 'can not get qltot from input files.')
                        call ESMF_CFIOVarReadT2(cfio_in, 'qitot', curDate, curTime, tqi, rc=rc, cfio2=cfio_in2)
                        if ( rc /= 0 )  call die (myNewName, 'can not get qitot from input files.')
                        inField = tqi + tql
                        deallocate(tqi, tql)
                     else
!                       print *, "DEBUG 1: reading ", trim( inVars(iv) )
                        call ESMF_CFIOVarReadT2(cfio_in, inVars(iv), curDate, curTime, inField, rc=rc, cfio2=cfio_in2)
!                       print *, "DEBUG 1: rc=", rc
                     end if
                  end if
               end if
!               if ( rc /= 0 )  call die (myNewName, 'something wrong in GFIO_GetVarT for 3D file')

               if (lon_e(1) < 0 .and. .not. doSubset) then
                  call checkVar(inVars(iv), vName, nVars_e, rc)
                  if ( rc .eq. 0 ) call lon_shift(inField, im_e, jm_e, km_e)
               end if

            else
!           read 2D variable
               allocate ( inField(im_e,jm_e,1), stat=rc )
               if (rc /= 0) call  die (myNewName, 'cannot allocate inField')
               call checkVar(inVars(iv), vName, nVars_e, rc)
               if ( doComputing .and. rc /= 0 ) then
                  if ( trim(outVars(iv)) .eq. "SLP" ) inField(:,:,1) = slp
               else
                  if (index(inVars(iv),";") .gt. 0) then
                     name_pos = index(trim(inVars(iv)), ';')
                     name_len = len(trim(inVars(iv)))
                     uWind = inVars(iv)(1:(name_pos-1))
                     vWind  = inVars(iv)((name_pos+1):name_len)
                     call ESMF_CFIOVarReadT2(cfio_in, uWind, curDate, curTime, inField, rc=rc, cfio2=cfio_in2)
                     allocate ( vField(im_e,jm_e,1), stat=rc )
                     call ESMF_CFIOVarReadT2(cfio_in, vWind, curDate, curTime, vField, rc=rc, cfio2=cfio_in2)
                     if ( lon_e(1) < 0 .and. .not. doSubset ) call lon_shift(vField, im_e, jm_e, 1)
                  else
                     if ( rc /= 0 .and. (trim(outVars(iv)) .eq. 'TQC' .or. trim(outVars(iv)) .eq. 'tqc') ) then
                        allocate ( tql(im_e,jm_e,1), stat=rc )
                        allocate ( tqi(im_e,jm_e,1), stat=rc )
                        call ESMF_CFIOVarReadT2(cfio_in, 'TQL', curDate, curTime, tql, rc=rc, cfio2=cfio_in2)
                        if ( rc /= 0 )  call die (myNewName, 'can not get TQL from input files.')
                        call ESMF_CFIOVarReadT2(cfio_in, 'TQI', curDate, curTime, tqi, rc=rc, cfio2=cfio_in2)
                        if ( rc /= 0 )  call die (myNewName, 'can not get TQI from input files.')
                        inField = tqi + tql
                        deallocate(tqi, tql)
                     else
!                       print *, "DEBUG 2: reading ", trim( inVars(iv) )
                        call ESMF_CFIOVarReadT2(cfio_in, inVars(iv), curDate, curTime, inField, rc=rc, cfio2=cfio_in2)
                     end if
                  end if
                  if ( rc /= 0 )  call die (myNewName, 'something wrong in GFIO_GetVarT for 2D file')
               end if
               if (lon_e(1) < 0 .and. .not. doSubset) then
                  call checkVar(inVars(iv), vName, nVars_e, rc)
                  if ( rc .eq. 0 ) call lon_shift(inField, im_e, jm_e, 1)
               end if
!               if (lon_e(1) < 0 .and. .not. doSubset .and. trim(outVars(iv)) .ne. "SLP") &
!                   call lon_shift(inField, im_e, jm_e, 1)
!               if ( lon_e(1) < 0 .and. .not. doSubset) call lon_shift(inField, im_e, jm_e, 1)
            end if
              
!           Interpolate from Input to Output grid
!           -------------------------------------
!           print *, "DEBUG: Interpolate."
            if (inKm(iv) .gt. 0) then
               allocate(outField(im,jm,nLevs), stat=rc) 
!              print *, "DEBUG: allocated outField: im=",im," jm=",jm," nLevs=",nLevs
               if (rc /= 0) call die (myNewName, 'cannot allocate outField')
               if ( .Not. doSubset ) then
                 allocate(lon3d(im*jm*km),lat3d(im*jm*km),lev3d(im*jm*km),conf(im*jm*km), &
                          stat=rc)
                 if (rc /= 0) call die (myNewName, 'cannot allocate lon3d, conf')
                 call get_coords(im,jm,km,lon_o,lat_o,Levs,lon3d,lat3d,lev3d)
                 call checkVar(inVars(iv), vName, nVars_e, rc)
                 if ( (trim(outVars(iv)) .eq. "HGHT" .or.                     &
                       trim(outVars(iv)) .eq. "H") .and. rc /= 0) then
                     call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km, &
                                im_e, jm_e, km_e, inField, 3,           &
                                outField, conf, rc, wz = wz, slp = slp )
                 else
                    if (index(inVars(iv),";") .gt. 0 ) then
                       allocate(vOutField(im,jm,nLevs), stat=rc)
                       call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                                        im_e, jm_e, km_e, inField, 1, outField,  &
                                        conf, rc, vField, vOutField, amiss=undef )
                       deallocate(vField)

                    else
                       if ( compQ .and. (trim(outVars(iv)) .eq. "SPHU" .or.        &
                                       trim(outVars(iv)) .eq. "sphu" .or.         &
                                       trim(outVars(iv)) .eq. "QV" .or.           &
                                       trim(outVars(iv)) .eq. "qv" ) ) then

                          allocate(rhP(im,jm,km), tmpuP(im,jm,km), qsfieldP(im,jm,km))
                          call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                          im_e, jm_e, km_e, rh, 0, rhP, conf, rc, amiss=undef )
                          call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                          im_e, jm_e, km_e, tmpu, 0, tmpuP, conf, rc, amiss=undef )

                          allocate(tfield(im,jm), pmk(im,jm),qsfield(im,jm))
                          do k = 1, km
                             do j = 1, jm
                             do i = 1, im
                                tfield(i,j) = tmpuP(i,j,k)
                                pmk(i,j) = 100*Levs(k)
                             end do
                             end do
                             qsfield = GEOS_Qsat(tfield, pmk, PASCALS=.true.)
                             do j = 1, jm
                             do i = 1, im
                                   qsfieldP(i,j,k) = qsfield(i,j)
                             end do
                             end do
                          end do

! Interpolate Q from eta to pressure
                          call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                          im_e, jm_e, km_e, inField, 0, outField, conf, rc, amiss=undef )

                          do k = 1, km

! Select Q in pressure from outField and rh*Qs 
                           if ( Levs(k) .ge. 100 ) then 
                             do j = 1, jm
                             do i = 1, im
                                if (rhP(i,j,k) .gt. 0.1*undef .or. qsfieldP(i,j,k) .gt.  &
                                               0.1*undef) then
                                   outField(i,j,k) = undef
                                else
                                   outField(i,j,k) = rhP(i,j,k)/100.*qsfieldP(i,j,k)
                                end if
                             end do
                             end do
                           end if

                          end do
                          deallocate(tfield, pmk, qsfield)
                          deallocate(rhP, tmpuP, qsfieldP)
                       else  
                          if ( .NOT. onEdges ) then
                             call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                             im_e, jm_e, km_e, inField, 0, outField, conf, rc, amiss=undef )
                          else
                             call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*km,  &
                             im_e, jm_e, km_e, inField, 0, outField, conf, rc,         &
                             edges=.true., amiss=undef )
                          end if
                       end if
                    end if
                 end if
               else
                  if ( ((lon_o(1) .ge. 0 .and. lon_e(1) .lt. 0) .or.   &
                       (lon_o(1) .lt. 0 .and. lon_e(1) .ge. 0) ) .and. &
                       (index(inVars(iv),";") .le. 0) )                &
                      call lon_shift(inField, im_e, jm_e, km_e)
                  call doSubseting(inField, im_e, jm_e, km_e, outField, nLevs, levNums)
               end if
            else
               if (.not. associated(outField)) then
                  allocate(outField(im,jm,1),stat=rc)
                  if (rc /= 0) call die (myNewName, 'cannot allocate outField')
               end if
               if ( .Not. doSubset ) then
                  allocate(lon3d(im*jm), lat3d(im*jm), lev3d(im*jm), conf(im*jm), &
                           stat=rc)
                  if (rc /= 0) call die (myNewName, 'cannot allocate 2D lon3d, conf')
                  call get_coords(im,jm,1,lon_o,lat_o,Levs,lon3d,lat3d,lev3d)
                  if (index(inVars(iv),";") .gt. 0 ) then
                     allocate(vOutField(im,jm,1), stat=rc)
                     call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm*1 ,  &
                                     im_e, jm_e, 1, inField, 1, outField,  &
                                     conf, rc, vField, vOutField, amiss=undef )
                     deallocate(vField)
                  else
                     call Interp_Field ( grid, lon3d, lat3d, lev3d, im*jm,  &
                        im_e, jm_e, 1, inField, 0, outField, conf, rc, amiss=undef )
                  end if
               else
                  if ((lon_o(1) .ge. 0 .and. lon_e(1) .lt. 0)  .or.   &
                      (lon_o(1) .lt. 0 .and. lon_e(1) .ge. 0) )      &
                      call lon_shift(inField, im_e, jm_e, 1)
                  outField = inField
               end if
            end if

!           Unit conversion.
!            if ( .NOT. doSubset ) then
!            print *, "DEBUG: unit conversion."
              if (outKm(iv) .gt. 0) then
                 if (index(inVars(iv), ";") .gt. 0) then
                    call unitConvert(im,jm,km,scaleFactor(iv),offSet(iv),undef,outField)
                    if (.not. eta .and. doSubset) then
                       call unitConvert(im,jm,km,scaleFactor(iv),offSet(iv),undef,vField)
                    else
                       call unitConvert(im,jm,km,scaleFactor(iv),offSet(iv),undef,vOutField)
                    end if
                 else
                    call unitConvert(im,jm,km,scaleFactor(iv),offSet(iv),undef,outField)
                 end if 
              else
                 if (index(inVars(iv), ";") .gt. 0) then
                    call unitConvert(im,jm,1,scaleFactor(iv),offSet(iv),undef,outField)
                    call unitConvert(im,jm,1,scaleFactor(iv),offSet(iv),undef,vOutField)
                 else
                    call unitConvert(im,jm,1,scaleFactor(iv),offSet(iv),undef,outField)
                 end if
              end if
!            end if

!           Write interpolate variable to output file
!           -----------------------------------------           
!           print *, "DEBUG: write"
            if (index(outVars(iv), ";") .gt. 0) then
               name_pos = index(trim(outVars(iv)), ';')
               name_len = len(trim(outVars(iv)))
               uWind = outVars(iv)(1:(name_pos-1))
               vWind  = outVars(iv)((name_pos+1):name_len)

               if (.not. eta .and. doSubset) then

                  allocate(vOutField(im,jm,nLevs), stat=rc) 
                  if (rc /= 0) call die (myNewName, 'cannot allocate vOutField')

                  dl = 8.*atan(1.0) / float(im)
                  allocate(coslon(im), sinlon(im))
                  allocate(ua(im,jm), va(im,jm))
                  allocate(tmp(im_e,jm_e), tmp2(im_e,jm_e))
                  do i=1,im/2
                     coslon(i)      = -cos((i-1)*dl)
                     coslon(i+im/2) = -coslon(i)
                     sinlon(i)      = -sin((i-1)*dl)
                     sinlon(i+im/2) = -sinlon(i)
                  enddo
                  do k=1,km
!                 map D-grid wind to A-grid
                  tmp = inField(:,:,k)
                  tmp2 = vField(:,:,k)
                  call d2a(tmp,tmp2, ua ,va,  &
                    im, jm, 1, jm, coslon, sinlon)

                  do j=1,jm
                  do i=1,im
                     outField(i,j,k) = ua(i,j)
                     vOutField(i,j,k) = va(i,j)
                  enddo
                  enddo
                  enddo
                  deallocate(coslon, sinlon)
                  deallocate(ua, va)
                  deallocate(tmp, tmp2)

                  if ((lon_o(1) .ge. 0 .and. lon_e(1) .lt. 0)  .or.   &
                      (lon_o(1) .lt. 0 .and. lon_e(1) .ge. 0) )  then 
                     call lon_shift(outField, im, jm, km)
                     call lon_shift(vOutField, im, jm, km)
                  end if
               end if


               if ( nbits .gt. 0 ) then
                  do k = 1, max(1,outKm(iv))
                  allocate(x(im,jm), xr(im,jm))
                  do j=1,jm
                  do i=1,im
                     x(i,j) = outField(i,j,k)
                  enddo 
                  enddo 
#if defined(HDFEOS)
!                 print *, "DEBUG: About to call downbit."
                  call ESMF_CFIODownBit(x,xr,nbits,undef=undef,rc=rc)
#endif
                  do j=1,jm
                  do i=1,im
                     outField(i,j,k) = xr(i,j)
                  enddo 
                  enddo 
                  do j=1,jm
                  do i=1,im
                     x(i,j) = vOutField(i,j,k)
                  enddo 
                  enddo 
#if defined(HDFEOS)
                     call ESMF_CFIODownBit(x,xr,nbits,undef=undef, rc=rc)
#endif
                  do j=1,jm
                  do i=1,im
                  vOutField(i,j,k)  = xr(i,j)
                  enddo 
                  enddo 
                  enddo 
                  deallocate(x, xr)
               end if
#if defined(HDFEOS)
               call ESMF_CFIOVarWrite(cfio_out,uWind,outField,curDate,curTime,  &
                                      doComp=doComp,doChunk=doChunk,rc=rc)
               call ESMF_CFIOVarWrite(cfio_out,vWind,vOutField,curDate,curTime, &
                                      doComp=doComp,doChunk=doChunk,rc=rc)
#else 
               call ESMF_CFIOVarWrite(cfio_out,uWind,outField,curDate,curTime, rc=rc)
               call ESMF_CFIOVarWrite(cfio_out,vWind,vOutField,curDate,curTime, rc=rc)
#endif
!              print *, "DEBUG: Write ", uWind, " rc=",rc
!              print *, "DEBUG: Write ", vWind, " rc=",rc
               deallocate(vOutField)
            else
!              print *, "DEBUG: else, not wind vector."
               if (isPos(iv) .eq. 1) then
                  if ( minval (outField) .lt. 0.0 ) then
                     print *, "WARNING: Zeroing out negative data. ",curDate, " ", curTime, " ",TRIM(outVars(iv))," min value=", minval(outField)
                     outField = max(0.0, outField)
                  end if
               end if
            if ( nbits .gt. 0 ) then
!              print *, "DEBUG: Allocating space: im=",im," jm=",jm," km=",max(1,outKm(iv))
               allocate(x(im,jm), xr(im,jm))
               do k = 1, max(1,outKm(iv))
!              print *, "DEBUG: k=",k
               do j=1,jm
               do i=1,im
                  x(i,j) = outField(i,j,k)
               enddo
               enddo
#if defined(HDFEOS)
!                 print *, "DEBUG: About to call downbit."
                  call ESMF_CFIODownBit(x,xr,nbits,undef=undef, rc=rc)
!                 print *, "DEBUG: rc =",rc               
#endif
               do j=1,jm
               do i=1,im
                  outField(i,j,k) = xr(i,j)
               enddo
               enddo
               enddo
               deallocate(x, xr)
             end if
!            print *, "DEBUG: About to write ", trim( outVars(iv) )
#if defined(HDFEOS)
             call ESMF_CFIOVarWrite(cfio_out,outVars(iv),outField,curDate,curTime, &
                                      doComp=doComp,doChunk=doChunk,rc=rc)
#else  
             call ESMF_CFIOVarWrite(cfio_out,outVars(iv),outField,curDate,curTime, rc=rc)
#endif
!            print *, "DEBUG: Write ", outVars(iv), " rc=",rc
            end if
            if ( rc /= 0 )  call die (myNewName, 'can not write var')

            if (.NOT. doSubset) then
               deallocate( inField, outField, lon3d, lat3d, lev3d, conf )
            else
               deallocate( inField, outField )
            end if
            
         end do loop_var  ! variables

        if (doComputing) deallocate(wz,slp,tmpu,rh)
!        if ( onEdges .and. .NOT. doSubset ) then
!           deallocate(ple)
!        end if
!        if ( .NOT. onEdges .and. .NOT. all2D .and. .NOT. doSubset .and. eta) then
!           deallocate(pl)
!        end if
        preTime = curTime
        curTime = curTime + incTime

        if ( curTime .ge. 240000 ) then
           curTime = curTime - 240000
           curDate = incymd(curDate, 1)
           exit 
        end if
        if ( curTime .gt. mod(hhmmss(it)+timinc, 240000) ) then
             exit
        endif
        if ( .not. timeInterp .and. preTime .eq. hhmmss(it)) then
             exit
        endif
        if ( preTime .eq. hhmmss(it) .and. timinc .le. incTime) then
             exit
        endif
       end do loop_do ! end "do" while loop
      end do loop_time! times


!     Close input file
!     ----------------
!     (VarInfo is now destroyed within the ESMF_CFIODestroy() routine)
!---------------------------------------------------------------------
!---       do i = 1, nVars_e
!---          call ESMF_CFIOVarInfoDestroy(vars(i))
!---       end do
!--------------------------------------------------------------------
!-       call ESMF_CFIODestroy(cfio_in)
!-       call ESMF_CFIODestroy(cfio_in2)
   end do loop_file ! input files

!  Close output file
!  ----------------
   call ESMF_CFIOFileClose(cfio_out,rc=rc)
!   deallocate(grid_in)


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
                      levNums, mVars, nVars, inVars, outVars, outUnits,  &
                      scaleFactor, addOffset, isPos, eta, compQ,    &
                      outPrec, date, inc_hhmmss, startTime, outVarsgc,   &
		      longName, cfioStandardName, onEdges, doSubset,     &
                      oneStep, doComp, doChunk, nPsfs, prsFiles, nbit,   &
                      tSteps, format, ncep72 ) 

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
      integer, pointer      :: levNums(:)        ! vertical level numbers

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
      logical, intent(out) :: eta              ! eta or lcv file
      logical, intent(out) :: onEdges          ! Variables are on edges
      logical, intent(out) :: doSubset         ! Eta --> eta
      logical, intent(out) :: oneStep            ! only one time step in output file
      logical, intent(out) :: doComp           ! do szip compression
      logical, intent(out) :: doChunk          ! do szip chunk compression
      logical, intent(out) :: compQ            ! compute SPHU on pressure levels

      character(len=*), intent(out) :: outVarsgc(:) ! output variable names (nVars)
      character(len=*), intent(out) :: longName(:)  ! output variable names (nVars)
      character(len=*), intent(out) :: cfioStandardName(:)  ! output cfio standard  names 

      integer, intent(out)          :: outPrec ! Output file precision:
                                               ! 0 = 32 bits,  1 = 6 4bits
      integer, intent(out)          :: date        ! User specified date   
      integer, intent(out)          :: inc_hhmmss  !User specified increment hours(hhmmss)   
      integer, intent(out)          :: startTime   !User specified increment hours(hhmmss)   

      integer, intent(out)          :: nPsfs       !number of pre files
      character(len=*), intent(out) :: prsFiles(mFiles) ! output variable names (nVars)
      integer, intent(out)  :: nbit
      integer, intent(out)  :: tSteps
      character(len=*), intent(out) :: format           ! output format (GrADS/HDF/HDFEOS)
      logical,intent(out)           :: ncep72      ! use NCEP-like levels (but augmented to 72)

! !DESCRIPTION: This routine initializes {\tt lcv2prs}. It parses the command 
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
   character(len=4096)  argv, prsFile

   character(len=255)   rcfile, var, Vars(mVars), tmp, tmp1
   character(len=4096) label
   character(len=64)   inType
   character(len=256)  usrHistory, usrConvention, usrInstitution 
   character(len=256)  usrReferences, usrComment, usrSource
   character(len=256)  usrTitle, usrContact

   integer, parameter :: mKm = 256  ! max no of levels
   real levels(mKm)

   integer i, j, n, nVars0, rc, ios
   integer name_pos, name_len, iv   
   real    xWest, xSouth, deltaPhi, deltaj, p
   logical :: debug = .false.

   argc = iargc()
   if ( argc < 1 ) call usage_()

!  Defaults
!  --------
   nFiles = 0
   nVars = 0
   usrHistory = ''
   usrConvention = ''
   usrInstitution  = ''
   usrReferences = ''
   usrComment = ''
   usrSource = ''
   usrTitle = ''
   usrContact  = ''
   outFile = 'lcv2prs.prs.nc4'
   rcfile = 'lcv2prs.rc'
   outPrec = 0
   date = -999
   inc_hhmmss = -999
   startTime = -999
   im = -1
   jm = -1
   km = -1
   nbit = 0
   tSteps = 0
   xWest = -999
   xSouth = -999
   deltaPhi = -999
   inType = 'eta'
   eta = .true.
   doComp = .false.
   doChunk = .false.
   format = 'SDF'
   ncep72 = .false.

   iarg = 0
   do i = 1, 32767
      iarg = iarg + 1
      if ( iarg .gt. argc ) then
           exit
      endif
      call GetArg ( iArg, argv )
      if(index(argv,'-o') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, outFile )
      else if(index(argv,'-edge') .gt. 0 ) then
         onEdges = .true.
      else if(index(argv,'-lcv') .gt. 0 ) then
         eta = .false.
      else if(index(argv,'-compQ') .gt. 0 ) then
         compQ = .true.
      else if(index(argv,'-nStep=1') .gt. 0 ) then
         oneStep = .true.
      else if(index(argv,'-doComp') .gt. 0 ) then
         doComp = .true.
      else if(index(argv,'-doChunk') .gt. 0 ) then
         doChunk = .true.
      else if(index(argv,'-doSubset') .gt. 0 ) then
         doSubset = .true.
      else if(index(argv,'-cvs') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, cvsFile )
      else if(index(argv,'-format') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, format )
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
      else if(index(argv,'-start') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) startTime 
      else if(index(argv,'-rc') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, rcfile )
      else if(index(argv,'-hist') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrHistory )
      else if(index(argv,'-convention') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrConvention )
      else if(index(argv,'-inst') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrInstitution )
      else if(index(argv,'-ref') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrReferences )
      else if(index(argv,'-comment') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrComment )
      else if(index(argv,'-src') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrSource )
      else if(index(argv,'-title') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrTitle )
      else if(index(argv,'-contact') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, usrContact )
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
      else if(index(argv,'-south') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) xSouth
      else if(index(argv,'-jm') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) jm
      else if(index(argv,'-nbits') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) nbit
         print *, "nbit: ", nbit
      else if(index(argv,'-tSteps') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) tSteps
      else if(index(argv,'-vars') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mVars, Vars, nVars )
      else if(index(argv,'-prsf') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, prsFile )
         if (len(trim(prsFile)) .ge. 4096) call die (myNewName, '-prsf files are too long.')
         call split_ ( ',', prsFile, mFiles, prsFiles, nPsfs )
      else if(index(argv,'-levels') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mLevs, cLevs, nLevs )
         allocate( Levs(nLevs), stat = rc)
         if ( rc /= 0 )  call die (myNewName, 'wrong in allocating nLevs')

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
      else if(index(argv,'-ncep72') .gt. 0 ) then
         ncep72 = .true.
     else if(index(argv,'-help') .gt. 0 ) then
!         call help_();
      else if(index(argv,'-d') .gt. 0 ) then
         debug = .true.
      else
         nFiles = nFiles + 1
         inFiles(nFiles) = argv
      end if

   end do

print *
print *, "-----------------------------------------------------------"
print *, "lcv2prs - Convert fvDAS Output Files to Pressure Coordinates"
print *, "-----------------------------------------------------------"
print *

   if ( outPrec .eq. 32 ) outPrec = 0   
   if ( outPrec .eq. 64 ) outPrec = 1   

    if ( nFiles .eq. 0 ) call die (myNewName, 'no files specified')

!  Figure out output file name if not specified
!  --------------------------------------------
   if ( trim(outFile) .eq. 'lcv2prs.prs.nc4' ) then
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
         if ( xWest .eq. -999 ) xWest = -180.
         do i = 1, im
            lon(i) = xWest + (i-1) * 360. / im
         end do
       end if
       if ( jm > 0 ) then 
         allocate ( lat(jm), stat=ios )
         if ( xSouth .eq. -999 ) xSouth = -90.
         if ( deltaPhi .eq. -999 ) then
            deltaj = 180. / ( jm - 1 )
         else
            deltaj = deltaPhi
         end if
         do j = 1, jm
            lat(j) = xSouth + (j-1) * deltaj
          end do
       end if

       return
   end if

!  Get coordinates
!  ---------------
   if ( im < 0 ) then
      call i90_label ( 'zonal_resolution:', rc )
      im = i90_gint(rc)
      if ( rc .ne. 0 ) call die(myNewName,'cannot load im')
   end if
   if ( xWest .eq. -999 ) then
      call i90_label ( 'west_most_longitude_in_deg:', rc )
      xWest = i90_gfloat(rc)
      if ( rc .ne. 0 ) call die(myNewName,'cannot load xWest')
   end if
   if ( jm < 0 ) then
      call i90_label ( 'meridional_resolution:', rc )
      jm = i90_gint(rc)
      if ( rc .ne. 0 ) call die(myNewName,'cannot load jm')
   end if
   if ( xSouth .eq. -999 ) then
      call i90_label ( 'south_most_latitude_in_deg:', rc )
      xSouth = i90_gfloat(rc)
      if ( rc .ne. 0 ) xSouth = -90.
   end if
   if ( deltaPhi .eq. -999 ) then
      call i90_label ( 'delta_phi:', rc )
      deltaPhi = i90_gfloat(rc)
      if ( rc .ne. 0 ) then 
         deltaj = 180. / ( jm - 1 )
      else
         deltaj = deltaPhi
      end if
   end if

   if ( .NOT. doSubset ) then
      call i90_label ( 'vertical_levels_in_hPa:', rc )
      if ( rc .ne. 0 ) call die(myNewName,'cannot load vertical levels')
      km = 0
      do i = 1, mKm
         p = i90_gfloat(rc)
         if ( rc .ne. 0 ) then
              exit
         endif
         km = km + 1
         levels(km) = p
      end do
   end if
          
   if ( im .le. 0 ) call die(myNewName, 'invalid im<0')
   if ( jm .le. 0 ) call die(myNewName, 'invalid jm<0')

   if ( doSubset ) call i90_label ( 'vertical_level_numbers:', rc )
   if ( rc .eq. 0 ) then
      km = 0
      do i = 1, mKm
         p = i90_gint(rc)
         if ( rc .ne. 0 ) then
               exit
         endif
         km = km + 1
         levels(km) = p
      end do
      allocate(levNums(km), stat = rc)
      do i = 1, km
         levNums(i) = levels(i)
      end do
   end if
   if ( km .eq. 0 ) call die(myNewName, 'invalid km=0')

!  Generate coordinate variables
!  -----------------------------
   allocate ( lon(im), lat(jm), lev(km), stat=ios )
   if ( ios .ne. 0 ) call die(myNewName,'cannot allocate memory for lat/lon/lev')
   do i = 1, im
      lon(i) = xWest + (i-1) * 360. / im
   end do
   do j = 1, jm
      lat(j) = xSouth + (j-1) * deltaj
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
           call getClass_('VarClass*'//trim(Vars(n)(2:))//':',      &
               outVars,nVars)
      end if

    end do

!   call i90_loadf ( rcfile, rc )

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
      call i90_label("Variable_translation_table::",rc)
      if ( rc .ne. 0 ) call die(myNewName,'cannot get Variable_translation_table')
      call i90_gLine(rc)
      do while(rc==0)
         call i90_gtoken ( tmp, rc )
         if(rc==0) then
            do i=1,mVars
               if(uppercase(trim(tmp))==uppercase(trim(outVars(n)))) then
               exit
               endif
             end do
             if ( i .ge. mVars ) then
                call i90_gLine(rc)
                cycle
             end if
             call i90_gtoken ( outUnits(n), rc )
                                                                                       
             if(rc==0) then
                call i90_gtoken ( inVars(n), rc )
                if(rc==0) scaleFactor(n) = i90_gfloat(rc)
                if(rc==0) addOffset(n) = i90_gfloat(rc)
                if(rc==0) isPos(n) = i90_gint(rc)
                exit
             endif
          endif
!          call i90_gLine(rc)
        end do

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

   longName = ''
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

   cfioStandardName = ''
   do i = 1, iv
      call i90_label ( 'StandardName:' // uppercase(trim(outVarsgc(i))) // ':', rc )
      if ( rc .ne. 0 ) call i90_label ( 'StandardName:' // lowercase(trim(outVarsgc(i))) // ':', rc )
      if ( rc .ne. 0 ) cycle
      call i90_gtoken ( tmp, rc )
      if ( rc .ne. 0 ) cycle 
      tmp1 = trim(tmp)
      do j = 1, 128
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            cfioStandardName(i) = trim(tmp1)
            exit
         end if
         cfioStandardName(i) = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = cfioStandardName(i)
      end do 
   end do

   call i90_label ('history:', rc)
   if ( rc .ne. 0 ) then
      history = "File written by CFIO"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            history = trim(tmp1)
            exit
         end if
         history = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = history
      end do
   end if
   if ( len(trim(usrHistory)) .ge. 1 ) history = usrHistory

   call i90_label ('convention:', rc)
   if ( rc .ne. 0 ) then
      convention = "CF-1.0"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            convention = trim(tmp1)
            exit
         end if
         convention = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = convention
      end do
   end if
   if ( len(trim(usrConvention)) .ge. 1 ) convention = usrConvention

   call i90_label ('institution:', rc)
   if ( rc .ne. 0 ) then
      institution = "Global Modeling and Assimilation Office, NASA Goddard Space Flight Center, Greenbelt, MD 20771"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            institution = trim(tmp1)
            exit
         end if
         institution = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = institution
      end do
   end if
   if ( len(trim(usrInstitution)) .ge. 1 ) institution = usrInstitution

   call i90_label ('references:', rc)
   if ( rc .ne. 0 ) then
      references = "http://gmao.gsfc.nasa.gov"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            references = trim(tmp1)
            exit
         end if
         references = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = references
      end do
   end if
   if ( len(trim(usrReferences)) .ge. 1 ) references = usrReferences  

   call i90_label ('comment:', rc)
   if ( rc .ne. 0 ) then
      comment = "First CFIO GEOS version"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            comment = trim(tmp1)
            exit
         end if
         comment = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = comment
      end do
   end if
   if ( len(trim(usrComment)) .ge. 1 ) comment = usrComment

   call i90_label ('source:', rc)
   if ( rc .ne. 0 ) then
      source = "Global Modeling and Assimilation Office"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            source = trim(tmp1)
            exit
         end if
         source = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = source
      end do
   end if
   if ( len(trim(usrSource)) .ge. 1 ) source = usrSource

   call i90_label ('title:', rc)
   if ( rc .ne. 0 ) then
      title = "File written by CFIO"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            title = trim(tmp1)
            exit
         end if
         title = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = title
      end do
   end if
   if ( len(trim(usrTitle)) .ge. 1 ) title = usrTitle   

   call i90_label ('contact:', rc)
   if ( rc .ne. 0 ) then
      contact = "data@gmao.gsfc.nasa.gov"
   else
      call i90_gtoken ( tmp, rc ) 
      tmp1 = trim(tmp)
      do j = 1, 256
         call i90_gtoken ( tmp, rc )
         if ( rc .ne. 0 ) then
            contact = trim(tmp1)
            exit
         end if
         contact = trim(tmp1) // ' ' // trim(tmp)
         tmp1 = contact
      end do
   end if
   if ( len(trim(usrContact)) .ge. 1 ) contact = usrContact 

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
    write(*,'(a,i3,/(10x,6f10.2))') '    Longitudes: ', im,lon(1:im)
    write(*,'(a,i3,/(10x,6f10.2))') '     Latitudes: ', jm,lat(1:jm)
   end if
   if ( nLevs .gt. 0 ) then
      write(*,'(a,i3,/(10x,6f10.2))') '        Levels: ', nLevs,Levs
   else
      write(*,'(a,i3,/(10x,6f10.2))') '        Levels: ', km,lev(1:km) 
   end if 
   print *
   write(*,'(a,i3,/(10x,6a10))') '     Variables: ', nVars !,outVars(1:nVars)

   print *, &
'    OutVar      Units       inVar         Scale Factor   Add Offset   Positive Check'
   print *, &
'    ------      -----       -----         ------------   ----------   -------------'
   do n = 1, nVars
      write(*,'(5x,3a12,2f12.5,i12, I12)') outVars(n), outUnits(n), inVars(n), &        
                                scaleFactor(n), addOffset(n), isPos(n)
   end do
   print *

   end subroutine Init_

    subroutine Usage_()
   
print *, "NAME"
print *, "   lcv2prs  converting/subsetting lcv files."
print *
print *, "SYNOPYSIS"
print *, "   lcv2prs  [options] input_fname(s)"
print *
print *, "OPTIONS"
print *
print *, "  -o fname    output file name "
print *, "  -rc rcFile  resource file"
print *, "  -cvs cvsFile file with CVS tag"
print *, "  -im  imOut     number of grid points output in longitude"
print *, "  -jm  jmOut     number of grid points output in latitude"
print *, "  -west lon    lon starting point in output file"
print *, "  -south lat   latitude starting point in output file"
print *, "  -prec n     precision: "
print *, "                    n=0 for 32 bits (default) "
print *, "                    n=1 for 64 bits"
print *, "  -vars varn  actual variable names, e.g., -vars hght,uwnd;vwnd"
print *, "              or variable class, e.g., -vars @tavg3d_met_e"
print *, "  -doSubset   Eta to Eta subsetting"
print *, "  -prsf files Pressure files for converting to prs. "
print *, "              If more than one file is needed, use comma to seperate files."
print *, "              Grads format template can be used. "
print *, "  -edge       Input variables were written on Edges"
print *, "  -date  yyyymmdd   Starting date"
print *, "  -start hhmmss     Starting time"
print *, "  -inc   hhmmss     Time increment"
print *, "  -nStep=1          Output file has only one time step"
print *, "  -lcv              Input files are ana.eta. HGHT and other variables need to be computed"
print *, "  -doComp           Will do szip compression."
print *, "  -doChunk          Will do chunking for compression."
print *, "  -hist history               Global attribute for history."
print *, "  -convention convention      Global attribute for convention."
print *, "  -inst institution           Global attribute for institution."
print *, "  -ref references             Global attribute for references."
print *, "  -comment comment            Global attribute for comment."
print *, "  -src source                 Global attribute for source."
print *, "  -title title                Global attribute for title."
print *, "  -contact contact            Global attribute for contact."
print *, "  -compQ            compute specific humidity on pressure coordinate for ana.eta files"
print *
print *, "DESCRIPTION"
print *, "  converting/subsetting lcv files."
print *

    call die ( myNewName, 'exiting' )

    end subroutine Usage_

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
       if ( i > l ) then
            exit
       endif
    end do

    end subroutine split_


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

      subroutine getTag(cvsV,title,source,cvsV_yes,title_cvs,source_cvs)
         character(len=256), intent(in) :: cvsV         ! CVS version
         character(len=256), intent(in) :: title        ! meta data title
         character(len=256), intent(in) :: source       ! data source
         integer, intent(in)            :: cvsV_yes     ! data source
         character(len=256), intent(out) :: title_cvs   ! meta data title
         character(len=256), intent(out) :: source_cvs  ! data source
       
         integer :: t_len, ii

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

      end subroutine getTag
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
      character(len=*), parameter :: myNewName = 'get_coords'
                                                                                                                            
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

      subroutine checkVar(var1, var2, n2, rc)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      character(len=*) :: var1
      integer :: n2
      character(len=*) :: var2(n2)

! !OUTPUT PARAMETERS:
      integer, intent(out) :: rc
      
      
! Work vars
      integer :: j

      rc = -1
      do j = 1, n2
         if(trim(uppercase(var1)) .eq. trim(uppercase(var2(j)))) then 
            rc = 0
            return
         end if
      end do

      do j = 1, n2
         if ( (index(var1,";") .gt. 0) .and.     &
           index(uppercase(var1),trim(uppercase(var2(j)))) .gt. 0) then 
            rc = 0
            return
         end if
      end do

      return
      end subroutine checkVar

      subroutine checkStrictVar(var1, var2, n2, rc)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      character(len=*) :: var1
      integer :: n2
      character(len=*) :: var2(n2)

! !OUTPUT PARAMETERS:
      integer, intent(out) :: rc
      
      
! Work vars
      integer :: j

      rc = -1
      do j = 1, n2
         if(trim(var1) .eq. trim(var2(j))) then 
            rc = 0
            return
         end if
      end do

      do j = 1, n2
         if ( (index(var1,";") .gt. 0) .and.     &
           index(var1,trim(var2(j))) .gt. 0) then 
            rc = 0
            return
         end if
      end do

      return
      end subroutine checkStrictVar

      subroutine doSubseting(inField, im_e, jm_e, km_e, outField, km, levNums)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer :: im_e
      integer :: jm_e
      integer :: km_e
      integer :: km
      integer, pointer :: levNums(:)
      real, pointer :: inField(:,:,:)
                                                                                                      
! !OUTPUT PARAMETERS:
      real, pointer :: outField(:,:,:)

      integer :: i, j, k

      do k = 1, km
        do j = 1, jm_e
           do i = 1, im_e
              outField(i,j,k) = inField(i,j,levNums(k))
           enddo
        enddo
      enddo
      return
      end subroutine doSubseting

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: getClass_ - get all tokens of this class
!
! !DESCRIPTION:
!
! !INTERFACE:
                                                                                           
    subroutine getClass_(label,outVars,nVars,stat)
      use m_die,only : die,perr
      use m_inpak90,only : i90_label,i90_gLine
      implicit none
      character(len=*),intent(in) :: label
      character(len=*),dimension(:),intent(out) :: outVars
      integer,intent(out) :: nVars
      integer,optional,intent(out) :: stat
                                                                                           
! !REVISION HISTORY:
!       24Jan05 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________
                                                                                           
  character(len=*),parameter :: myNewName_=myNewName//'::getClass_'
  integer :: n
  integer :: rc,ier     ! rc is returned, but not used for now
                                                                                           
  if(present(stat)) stat=0
  nVars=0
  outVars=''
                                                                                           
        ! locate the label
                                                                                           
  call i90_label(label,rc)
  if(rc/=0) then
    call perr(myNewName_,'not found, "'//trim(label)//'"')
    if(.not.present(stat)) call die(myNewName_)
    stat=-1     ! -1 for less than expected
    return
  endif
                                                                                           
        ! if the label is located, get tokens one at a time
                                                                                           
  call getaRow_(outVars(nVars+1:),n,ier,rc)
  do while(ier==1)      ! expecting more takens in the next line
    nVars=nVars+n
    call i90_gLine(rc)
    if(rc/=0) then      ! this is the end of resource data
      ier=0
      exit
    endif
    call getaRow_(outVars(nVars+1:),n,ier,rc)
  end do
  nVars=nVars+n
!________________________________________
        ! ier== 0 for normal ending and
        !    ==-1 for buffer (i.e. outVars(:)) overflow.
                                                                                           
  select case(ier)
  case(0)
        ! this is where the label is found and a line of input buffer is
        ! processed with either 0 or more tokens.  Everything should be
        ! fine.
                                                                                           
  case(-1)
        ! this is where some extra token presents, but the buffer is
        ! not sufficient (size(outVars)).  A warning message has been
        ! sent, one may choose to add additional message here.
                                                                                           
    call perr(myNewName_,'insufficient buffer for "'//trim(label)//'"')
    if(.not.present(stat)) call die(myNewName_)
    stat=+1     ! +1 for extra
                                                                                           
  case default
        ! this is an unexpected error.  Something is wrong in the logic.
                                                                                           
    call die(myNewName_,'unexpected ier value',ier)
  endselect
                                                                                           
end subroutine getClass_
                                                                                           
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: getaRow_ - get a line of tokens
!
! !DESCRIPTION:
!
! !INTERFACE:
                                                                                           
    subroutine getaRow_(outvars,count,ier,rc)
      use m_inpak90,only : i90_gtoken
      use m_die,only : perr
      implicit none
      character(len=*), intent(out) :: outVars(:)
      integer,intent(out) :: count
                ! Count is the token count in a single line
      integer,intent(out) :: ier        ! logical error code
                                ! ier== 0; fine
                                !    ==+1; more line(s) to get
                                !    ==-1; buffer overflow (outVars(:))
      integer,intent(out) :: rc         ! inpak90 processing code
                                                                                           
! !REVISION HISTORY:
!       ddMmm04 - Baoyu Yin <yin@gmao.gsfc.nasa.gov>
!               - original single input line version
!       24Jan05 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________
                                                                                           
  character(len=*),parameter :: myNewName_=myNewName//'::getaRow_'
                                                                                           
  integer :: nV,mV
  integer :: mrc
  character(len=len(outVars)) :: var
                                                                                           
! Original comments for historical information:
                                                                                           
! here  Vars(n) = @tavg2d_slv_x
! mVars = 256, nVars = 0
! character(len=255) var
! character(len=*), intent(out) :: outVars(:)
!   label = 'VarClass*'//trim(Vars(n)(2:))//':'
                                                                                           
  ier=0
  count=0
                                                                                           
        ! get tokens until there is no token left (mrc/=0) or the output
        ! buffer is all filled (nV>mV).
                                                                                           
  mV=size(outVars)
  nV=0
  do while(nV<mV)
    call i90_gtoken ( outVars(nV+1), mrc )
    if ( mrc .ne. 0 ) then
         exit
    endif
    nV = nV + 1
  end do
                                                                                           
  if(nV>0) then
                ! something has been read
    if(outVars(nV)=='\' .or. outVars(nV)=='&') then
                        ! the last token is a sign of additional line(s)
      nV=nV-1           ! remove the last token from outVars(:).
      ier=1             ! signal the parent.
    endif
  endif
                                                                                           
  count=nV              ! the actual token count
                                                                                           
        ! Check for insufficient buffer condition, if there is still
        ! something left in the input line.
  call i90_gtoken ( var, rc )
  if(rc==0) then
    call perr(myNewName_, 'insufficient buffer at "'//trim(var)//'".',rc)
    ier=-1      ! signal an insufficient buffer condition
    return
  endif
end subroutine getaRow_
                                                                                           
subroutine compHght(cfio_in, curDate, curTime, delp, im, jm, km, lon_min, wz, slp, tmpu, rh, rc,cfio_in2)

! HISTORY:
!  04Dec2009 Todling  fix dims on wz and peln
!
     character(len=*), parameter :: myname_ = 'compHght'

     type(ESMF_CFIO), intent(inOut) :: cfio_in
     type(ESMF_CFIO), intent(inOut), optional :: cfio_in2
     integer, intent(in) :: curDate
     integer, intent(in) :: curTime
     integer, intent(in) :: im
     integer, intent(in) :: jm
     integer, intent(in) :: km
     real, intent(in)    :: lon_min
     real, intent(in)    :: delp(im,jm,km)
 
     real, intent(out) :: wz(im,km+1,jm)
     real, intent(out) :: slp(im,jm)
     real, intent(out) :: rh(im,jm,km)
     real, intent(out) :: tmpu(im,jm,km)
     integer, intent(out) :: rc      

     real, pointer :: phis(:,:)
     real, pointer :: phis3d(:,:,:)
     real, pointer :: theta(:,:,:)
     real, pointer :: sphu(:,:,:)
     real :: pk(im,jm,km+1)
     real :: pkz(im,jm,km+1)
     real :: peln(im,km+1,jm)
     real*4 :: pmk(im,jm)
     real*4 :: tfield(im,jm)
     real :: qsfield(im,jm)

     logical, save :: have_tv = .false.

     allocate(phis(im,jm), phis3d(im,jm,1), theta(im,jm,km), sphu(im,jm,km))
     call ESMF_CFIOVarReadT2(cfio_in, 'phis', curDate, curTime, phis3d, rc=rc,cfio2=cfio_in2)
     if (lon_min < 0 .and. .not. doSubset ) call lon_shift(phis3d, im, jm, 1)
     phis = phis3d(:,:,1)
     deallocate(phis3d)
     call ESMF_CFIOVarReadT2(cfio_in, 'theta', curDate, curTime, theta, rc=rc,cfio2=cfio_in2)
     if (rc/=0) then
         print*,'cannot find THETA ...'
         call ESMF_CFIOVarReadT2(cfio_in, 'tv', curDate, curTime, theta, rc=rc,cfio2=cfio_in2)
         if(rc/=0) call die(myname_,'cannot find neither theta nor tv in file')
         have_tv=.true.  ! theta array has tv
         print*,'... found TV instead'
     endif
     call ESMF_CFIOVarReadT2(cfio_in, 'sphu', curDate, curTime, sphu, rc=rc,cfio2=cfio_in2)
     if (lon_min < 0 .and. .not. doSubset ) then
        call lon_shift(theta, im, jm, km)
        call lon_shift(sphu, im, jm, km)
     end if

     call geopm(ptop,pk,delp,im,jm,km,1,jm,cpm,kappa,1)
     call pkez(im,jm,km,1,jm,ptop,grid%pe,pk,kappa,ks,peln,pkz,.false.)
     if (have_tv) then
         tmpu  = theta / (1. + zvir*sphu)
         theta = theta / pkz
         have_tv = .false.    ! now theta array has theta
     else
         tmpu = theta*pkz / (1. + zvir*sphu)
     endif
                          
      do k = 1, km
         do j = 1, jm
            do i = 1, im
               pmk(i,j) = 100*grid%pm(i,k,j)
               tfield(i,j) = tmpu(i,j,k)
            end do
         end do
                          
!         call vqsat ( tfield, pmk, qsfield, im*jm, undef )
          qsfield = GEOS_Qsat(tfield, pmk, PASCALS=.true.)
                          
         do j = 1, jm
            do i = 1, im
               rh(i,j,k) = qsfield(i,j)
            end do
         end do
     end do
                          
     rh = 100 * sphu / rh
!     rh = 100 * sphu*(1.-rh) / (rh*(1.-sphu))
     call comp_slp(im,jm,km,theta,phis,delp,            &
                  grid%pe,grid%pm,kappa,grav,pkz,slp)
     call comp_hght(ptop,im,jm,km,kappa,cpm,grav,delp, phis, theta, wz)
     wz = wz / grav
     deallocate(phis, theta, sphu) 
end subroutine compHght

      subroutine geopm(ptop,pk,delp,im,jm, km,jfirst, jlast, cp,akap,id)
                                                                                                     
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
                                                                                                     
! !OUTPUT PARAMETERS
      real,        intent(out) ::  pk(im,jm,km+1)
                  
! !REVISION HISTORY:
!
!   25Apr2007 Todling  Removed pt(theta) from arg list: not used
!
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
                                                                                                     
      subroutine  comp_slp(im,jm,km,pt,phis,delp,pe,pm,cappa,grav,pkz,inField)
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
      real,           intent(in)  :: pkz(im,jm,km+1)
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
               if ( pe(i,k+1,j) .lt. p_bot ) then
                    exit
               endif
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
                                                                                                     
subroutine unitConvert(im,jm,km,scaleFactor,offSet,undef,outField)
    integer, intent(in)  ::  im
    integer, intent(in)  ::  jm
    integer, intent(in)  ::  km
    real, intent(in)  ::  scaleFactor
    real, intent(in)  ::  offSet
    real, intent(in)  ::  undef 
    real, intent(inOut)  ::  outField(im,jm,km)

    do k = 1, km
       do j = 1, jm
          do i = 1, im
             if ( abs(outField(i,j,k) - undef) .gt. 0.01 )   &
                outField(i,j,k) = outField(i,j,k)*scaleFactor+offSet
          enddo
       enddo
    enddo

end subroutine unitConvert

subroutine readPrs (curDate, curTime, vName, im, jm, km, prs, rc)
     integer, intent(in)  :: curDate
     integer, intent(in)  :: curTime
     character(len=*),intent(in) :: vName
     integer, intent(in)  :: im, jm, km 
     real, pointer  :: prs(:,:,:)
     integer, intent(out) :: rc
     
     type(ESMF_CFIO) :: cfio_prs, cfio_prs2
     integer :: ipsf
     integer :: i
     integer :: prsDate, prsTime, prsInc, prsLm
     integer :: incSecs, hour, minute, sec
     integer, pointer    :: yyyymmddPrs(:), hhmmssPrs(:)
     logical :: found_prs
     character(len=255):: prsFileTmp

!     allocate (prs(im,jm,km), stat=rc)

     do ipsf = 1, nPsfs
        call strTemplate(prsFileTmp, prsFiles(ipsf), nymd=begDate, nhms=begTime, stat=rc)

        cfio_prs = ESMF_CFIOCreate(cfioObjName='prs_file')
        call ESMF_CFIOSet(cfio_prs, fName=prsFileTmp)
        call ESMF_CFIOFileOpen(cfio_prs, in_fmode)

        cfio_prs2 = ESMF_CFIOCreate(cfioObjName='prs_file2')
        if ( ipsf .lt. nPsfs) then
           call ESMF_CFIOSet(cfio_prs2, fName=prsFiles(ipsf+1))
        else
           call ESMF_CFIOSet(cfio_prs2, fName=prsFileTmp)
        end if
        call ESMF_CFIOFileOpen(cfio_prs2, in_fmode)

        call ESMF_CFIOGet(cfio_prs, date=prsDate, begTime=prsTime, &
                timeInc=prsInc, nSteps=prsLm)

        allocate ( yyyymmddPrs(prsLm),hhmmssPrs(prsLm), stat = rc )
        do i = 1, prsLm
           call CFIO_parseIntTime (   prsTime, hour, minute, sec )
           incSecs = hour*3600 + minute*60 + sec
           call GetDate(prsDate, prsTime, (i-1)*incSecs,  &
                        yyyymmddPrs(i), hhmmssPrs(i), rc)
        end do

        found_prs = .false.
        do i = 1, prsLm
           if (begDate ==  yyyymmddPrs(i) .and. begTime == hhmmssPrs(i) ) then
              found_prs = .true.
           end if
        enddo

        rc = 1
        if ( found_prs .or. (yyyymmddPrs(prsLm) .ge. begDate  &
                     .and. hhmmssPrs(prsLm) .ge. begTime) ) then
           call ESMF_CFIOVarReadT2(cfio_prs, vName, curDate, curTime, prs, rc=rc, cfio2=cfio_prs2)
           if (rc .eq. 0) then
              call ESMF_CFIOFileClose(cfio_prs)
              call ESMF_CFIOFileClose(cfio_prs2)
              exit
           end if
        end if

        call ESMF_CFIOFileClose(cfio_prs)
        call ESMF_CFIOFileClose(cfio_prs2)

     end do

end subroutine readPrs

      subroutine d2a(u,v,ua,va,im,jm,jfirst,jlast,coslon,sinlon)
! This is primarily for turbulence package designed for A-grid.
! Also used for output to A-grid.
! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1; removed fvcore.h
! WS 99.07.26 : Added jfirst and jlast as arguments

      implicit none
      integer im, jm, jfirst, jlast
! WS 99.07.26 : u must be ghosted N2S1
      real u(im,jm),v(im,jm),ua(im,jm),va(im,jm),coslon(im),sinlon(im)

      integer   imh
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1
      real un, vn, us, vs

! Convert D-grid winds to A-grid
! u --> ua, v --> va

      real utmp(im,jm),vtmp(im,jm)

      imh = im/2

      js = 3
      jn = jm - js + 1
      im1 = im-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 45 j=jn+1,jm-1
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jm-1
50    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,im-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) - &
                      (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) -      &
                      (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) -    &
                      (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) -  &
                      (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) - &
                      (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
! Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i)  &
             + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i)  &
             + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      us = us/im
      vs = vs/im

      do i=1,imh
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN
! Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i)  &
             + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i)  &
             + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      un = un/im
      vn = vn/im

      do i=1,imh
      ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
      va(i,jm) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jm) = -ua(i,jm)
      va(i+imh,jm) = -va(i,jm)
      enddo

      ENDIF

      do 100 j=2,jm-1
      do 100 i=1,im
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)
      return
      end subroutine d2a

end Program lcv2prs
