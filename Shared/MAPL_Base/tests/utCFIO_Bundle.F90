!
! Simple unit test for CFIO Read/Write Bundle
!

#include "MAPL_Generic.h"

   Program utCFIO

   use ESMF_Mod
   use MAPL_Mod

   implicit NONE

   type(ESMF_Grid)     :: grid
   type (ESMF_VM)      :: VM
   type(ESMF_DELayout) :: layout

   integer             :: nymd, nhms
   type(ESMF_Time)     :: fTime, dTime
   type(ESMF_TimeInterval)  :: fTimeStep, dTimeStep
   type(ESMF_Clock)    :: fClock, dClock

   type(ESMF_Bundle)   :: fBundle, dBundle

   type(MAPL_CFIO) :: cfio

   integer :: IM_WORLD = 72, JM_WORLD = 46, KM_WORLD = 26   
   integer :: i, j, k, im, jm, km                                      ! local

   character(len=*), parameter :: &
       dirname = '.',             &
     fFilename = dirname // '/sample.prs.nc'

   integer :: status, rc
   logical :: IamRoot
   integer, pointer :: resolution(:)
   real,    pointer ::levels(:)

   character(len=*), parameter :: Iam = 'utCFIO'

!                             -----
    
    call test_main()

CONTAINS

    subroutine test_main()

!   Initialize framework
!   --------------------
    call ESMF_Initialize (vm=vm, rc=status)
    VERIFY_(status)

    IamRoot = MAPL_am_I_root()

!   Get the global vm
!   -----------------
    call ESMF_VMGetGlobal(vm, rc=status)
    VERIFY_(status)

!   Create a grid
!   -------------
    grid = MyGridCreate_ ( vm, rc=status )
    VERIFY_(status)

!   Create empty bundles
!   --------------------
    fBundle = ESMF_BundleCreate ( name='Francesca', grid=grid, rc=status )
    VERIFY_(status)
    dBundle = ESMF_BundleCreate ( name='Denise',    grid=grid, rc=status )
    VERIFY_(status)

!   Set the time as the one on the hardwired file name
!   --------------------------------------------------
    call ESMF_CalendarSetDefault ( ESMF_CAL_GREGORIAN, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet( fTime, yy=2006, mm=8, dd=9, h=0, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet( dTime, yy=2006, mm=8, dd=9,  h=6, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeIntervalSet( fTimeStep, h=6, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    fClock = ESMF_ClockCreate ( name="Clovis", timeStep=fTimeStep, &
                                startTime=fTime, rc=status )
    VERIFY_(STATUS)

!   Read Bundle from file on a clean slate
!   --------------------------------------
    if ( IamRoot ) print *, 'Reading ' // fFilename
! this the equivalent of ESMF_ioRead
    call MAPL_cfioRead  ( fFilename, fTime, fBundle, rc=status, &
                        verbose=.true., force_regrid=.true.   )
    VERIFY_(status)

!   Next, create a bundle with same variables as the first one, and use
!    that to determine which variables to read from the second file
!   -------------------------------------------------------------------
    if ( IamRoot ) print *, 'Scaning ' // fFilename
    call MAPL_cfioRead  ( fFilename, fTime, dBundle, rc=status, &
                        verbose=.true., noRead = .true.,      &
                        only_vars = 'phis,qv' )

    VERIFY_(status)
    if ( IamRoot ) print *, 'Reading ' // fFilename
    call MAPL_cfioRead  ( fFilename, dTime, dBundle, rc=status, &
                        verbose=.true., force_regrid=.true. )
    VERIFY_(status)

!   Setup data types need for write
!   -------------------------------
    allocate ( resolution(2), levels(KM_WORLD), stat=status )
    VERIFY_(status)
    resolution = (/ IM_WORLD/2, JM_WORLD/2 /)
    levels     = (/ (k, k=1,KM_WORLD) /)

!   Write the same bundle to a differfent file
!   ------------------------------------------
    call MAPL_cfioCreate ( cfio, 'Cindy', fClock, fBundle, fTimeStep, &
         resolution, levels, descr='Bundle Write Test', rc=status )
    VERIFY_(status)

    call MAPL_cfioWrite ( cfio, fClock, fBundle, rc=status, &
                          verbose = .true. )
    VERIFY_(status)

    call MAPL_cfioDestroy ( cfio )

!   All done
!   --------
    call ESMF_Finalize ( rc=status )
    VERIFY_(STATUS)
    
  end subroutine test_main

!........................................................................

  function MyGridCreate_ ( vm, rc) result(grid)

    type (ESMF_VM),    intent(IN   ) :: VM
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

! Local vars
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MyGridCreate'

    integer                         :: LM
    integer                         :: L
    integer                         :: NX, NY
    integer, allocatable            :: IMXY(:), JMXY(:)
    character(len=ESMF_MAXSTR)      :: gridname
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY, deltaZ
    real                            :: LON0, LAT0

    real :: pi, d2r

! grid create

    lm = KM_WORLD   ! no. vertical layers
    nx = 2
    ny = 2

     pi  = 4.0 * atan ( 1.0 ) 
    d2r  = pi / 180.
    LON0 = -180  * d2r
    LAT0 = -90.0 * d2r

! Get the IMXY vector
! -------------------
    allocate( imxy(0:nx-1) )  
    call MAPL_DecomposeDim ( IM_WORLD, imxy, nx )

! Get the JMXY vector
! -------------------
    allocate( jmxy(0:ny-1) )  
    call MAPL_DecomposeDim ( JM_WORLD, jmxy, ny )

    deltaX = 2.0*pi/IM_WORLD
    deltaY = pi/(JM_WORLD-1)
    deltaZ = 1.0

    if ( MAPL_Am_I_Root() ) then
       print *, 'nx : imxy = ', nx, ' : ', imxy
       print *, 'ny : jmxy = ', ny, ' : ', jmxy
    endif

! Define South-West Corner of First Grid-Box
! ------------------------------------------
    minCoord(1) = LON0 - deltaX/2 
    minCoord(2) = LAT0 - deltaY/2
    minCoord(3) = deltaZ/2.

    layout = ESMF_DELayoutCreate(vm, deCountList=(/NX, NY/), rc=status)
    VERIFY_(STATUS)

    grid = ESMF_GridCreateHorzLatLonUni(         &
         counts = (/IM_WORLD, JM_WORLD/),        &
         minGlobalCoordPerDim=minCoord(1:2),     &
         deltaPerDim=(/deltaX, deltaY /),        &
         horzStagger=ESMF_Grid_Horz_Stagger_A,   &
         periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
         name='Beatrice', rc=status)
    VERIFY_(STATUS)

    call ESMF_GridAddVertHeight(grid,            &
         delta=(/(deltaZ, L=1,LM) /),            &
         rc=status)
    VERIFY_(STATUS)

    call ESMF_GridDistribute(grid,               &
         deLayout=layout,                        &
         countsPerDEDim1=imxy,                   &
         countsPerDEDim2=jmxy,                   &
         rc=status)
    VERIFY_(STATUS)

    deallocate(imxy)
    deallocate(jmxy)

    RETURN_(STATUS)

  end function MyGridCreate_

end Program utCFIO



