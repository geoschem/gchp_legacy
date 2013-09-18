#if defined( ESMF_ )
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !PROGRAM: GIGC (Grid-Independent GEOS-Chem)
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
#include "MAPL_Generic.h"

PROGRAM GIGC
!
! !USES: 
!
  USE ESMF_Mod                                     ! ESMF framework
  USE MAPL_Mod                                     ! MAPL framework
!  USE GC_Value_Mod                                 ! GEOS-Chem input values
  USE GIGC_GridCompMod,            ONLY: SetServices  ! To set IRF methods

  IMPLICIT NONE
!
! !REMARKS:
!  ROOT PROGRAM FOR EXECUTING THE GRID-INDEPENDENT GEOS-Chem
!  Adapted from the ut_GEOSCHEM.F90 Sandbox written by
!  R. Yantosca dn A. da Silva
!
! !REVISION HISTORY: 
!  01 Dec 2009 - A. Da Silva - Initial version  
!  02 Apr 2010 - R. Yantosca - Modified for GEOS-Chem column code
!  02 Apr 2010 - R. Yantosca - Added ProTex Headers, other cosmetic changes
!  07 Apr 2010 - R. Yantosca - Now populate all import/internal state fields
!  08 Apr 2010 - R. Yantosca - Now reference GEOS-Chem column input values from
!                              module "bmy_GC_Value_Mod.F90".  The header file
!                              "Column_Values_Saved_From_GEOS-Chem.h" is 
!                              now obsolete.
!  01 Jun 2010 - R. Yantosca - Increased output format to preserve precision
!  01 Jul 2010 - R. Yantosca - Now zero D_OH_MASS and D_AIR_MASS
!  11 Oct 2012 - R. Yantosca - Updated to be consistent w/ v9-01-03 and
!                              the new species/tracers in the Registry file
!  18 Oct 2012 - R. Yantosca - Fixed a few issues w/ the ASCII file input
!  17 Jun 2013 - M. Long     - Readapted as GEOSChem.F90 front-end application
!                              driver for the GIGC program
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
  !==========================================================================
  ! Basic ESMF objects being used in this example
  !==========================================================================
  TYPE(ESMF_Grid)         :: Grid            ! Grid
  TYPE(ESMF_VM)           :: VM              ! ESMF Virtual Machine
  TYPE(ESMF_Time)         :: startTime       ! ESMF Time object (start time)
  TYPE(ESMF_Time)         :: stopTime        ! ESMF Time object (stop time)
  TYPE(ESMF_TimeInterval) :: TimeStep        ! ESMF TimeStep object 

  !==========================================================================
  ! Grid component objects
  !==========================================================================
  TYPE(ESMF_GridComp)     :: GrComp          ! ESMF Gridded component
  TYPE(ESMF_State)        :: Import          ! ESMF Import state
  TYPE(ESMF_State)        :: Export
  TYPE(ESMF_Clock)        :: Clock

  TYPE (MAPL_MetaComp), pointer :: MAPL

  !==========================================================================
  ! Basic information about the parallel environment
  ! PET = Persistent Execution Threads
  ! In the current implementation, a PET is equivalent to an MPI process
  !==========================================================================
  INTEGER                 :: myPET           ! The local PET #
  INTEGER                 :: nPET            ! Total # of PETs we are using
  INTEGER                 :: STATUS          ! Status variable
  INTEGER                 :: RC              ! Status variable
  INTEGER                 :: I,  J, N        ! Loop indices 
  INTEGER                 :: IM, JM          ! Loop indices
  INTEGER                 :: NX              ! Layout: # of PETs in longitude
  INTEGER                 :: NY              ! Layout: # of PETs in latitude
  INTEGER                 :: IM_WORLD        ! # of longitudes in global grid
  INTEGER                 :: JM_WORLD        ! # of latitudes in global grid
  INTEGER                 :: LM_WORLD        ! # of levels in global grid
  LOGICAL                 :: MASTERPROC
  !==========================================================================
  ! Character variables
  !==========================================================================
  CHARACTER(LEN=ESMF_MAXSTR)  :: name
  CHARACTER(LEN=*), PARAMETER :: Iam = 'GIGC'

  REAL :: start, finish

  !==========================================================================
  ! Call the Main program
  !==========================================================================
  CALL CPU_TIME(start)
  CALL Main()
  CALL CPU_TIME(finish)
  write(*,*) 'RUNTIME: ',finish-start

CONTAINS
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Main
!
! !DESCRIPTION: The Main subroutine does the following:
!
! \begin{enumerate}
! \item Initializes the ESMF and MAPL frameworks
! \item Creates an ESMF Grid object
! \item Creates an ESMF Clock object
! \item Creates and initializes the ESMF Import and Export States
! \item Creates the GEOS-Chem gridded component
! \item Initializes the internal state of the GEOS-Chem gridded component
! \item Runs the GEOS-Chem gridded component
! \item Finalizes the GEOS-Chem gridded component
! \item Finalizes ESMF and MAPL frameworks
! \end{enumerate}
!
! !INTERFACE:
!
  SUBROUTINE Main()
!
! !REMARKS:
!
! !REVISION HISTORY: 
!  01 Dec 2009 - A. Da Silva - Initial version  
!  02 Apr 2010 - R. Yantosca - Modified for GEOS-Chem column code
!  02 Apr 2010 - R. Yantosca - Added ProTex Headers, other cosmetic changes
!  09 Apr 2010 - R. Yantosca - Add PET # to debug error messages
!  12 May 2010 - R. Yantosca - Now read start/stop time info from *.rc files
!  11 Oct 2012 - R. Yantosca - Updated for GEOS-Chem v9-01-03
!  17 Jun 2013 - M. Long     - Modified for Chemistry-only GIGC run.
!                              This is the GIGC stand-alone first-take
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Config)  :: MaplCF          
    TYPE(ESMF_Config)  :: GeosCF          
    INTEGER            :: runDt,         count
    INTEGER            :: utcStartDate,  utcStartTime
    INTEGER            :: utcEndDate,    utcEndTime
    INTEGER            :: yy0, mm0, dd0, h0, m0, s0
    INTEGER            :: yy1, mm1, dd1, h1, m1, s1
    CHARACTER(LEN=255) :: fileName

    !========================================================================
    ! Initialize
    !========================================================================

    ! Initialize ESMF framework.  For performance reasons, it is
    ! important to turn OFF ESMF's automatic logging feature
    CALL ESMF_Initialize( DefaultLogType=ESMF_LOG_NONE, VM=VM, __RC__ )
    
    ! Get the total # of PETs and local PET index from the ESMF VM 
    CALL ESMF_VMGet( VM, localPET=myPET, PETcount=nPET )  

    !-----------------------------------
    ! Read start/stop time info
    !-----------------------------------

    ! Create config objects
    MaplCF = ESMF_ConfigCreate( __RC__ )
    GeosCF = ESMF_ConfigCreate( __RC__ )

    ! Load config objects   
    CALL ESMF_ConfigLoadFile( MaplCF, 'MAPL.rc',          __RC__ )
    CALL ESMF_ConfigLoadFile( GeosCF, 'GIGC_GridComp.rc', __RC__ )

    IF (myPET .eq. 0) MASTERPROC=.true.

    ! Echo info if we are on the root PET
    IF ( MAPL_AM_I_ROOT() ) THEN
       WRITE( 6, '(a)' ) REPEAT( '=', 79 )
       WRITE( 6, 100   ) Iam, nPET
100    FORMAT( 'Starting ', a, ' with ', i3, ' PETs ...' )
       WRITE( 6, '(a)' ) REPEAT( '=', 79 )
    ENDIF

    !-----------------------------------
    ! Get grid information
    !-----------------------------------
 
   call ESMF_ConfigGetAttribute(GeosCF, Nx, Label='NX:', __RC__)
   call ESMF_ConfigGetAttribute(GeosCF, Ny, Label='NY:', __RC__)
   call ESMF_ConfigGetAttribute(GeosCF, IM_WORLD, Label='IM_WORLD:', __RC__)
   call ESMF_ConfigGetAttribute(GeosCF, JM_WORLD, Label='JM_WORLD:', __RC__)
   call ESMF_ConfigGetAttribute(GeosCF, LM_WORLD, Label='LM_WORLD:', __RC__)

    !----------------------------------- 
    ! Create the ESMF Grid object
    !-----------------------------------

   ! TEMPORARY DECOMPOSITION METHODS
   IF (mod(sqrt(real(nPet)),1.) .eq. 0) THEN
      NX = int(sqrt(real(nPet)))
      NY = NX
   ELSEIF (mod(nPet/2.,1.).eq.0) THEN
      NX = nPet/2
      NY = 2
   ELSEIF (mod(nPet/3.,1.).eq.0) THEN
      NX = nPet/3
      NY = 3
   END IF

    ! Create a global 2D Lat-Lon grid on a 2x1 layout
    Grid = MAPL_LatLonGridCreate( name     = 'GEOS-Chem Grid', &
                                  Nx       = NX,               &
                                  Ny       = NY,               &
                                  IM_WORLD = IM_WORLD,         &
                                  JM_WORLD = JM_WORLD,         &
                                  LM_WORLD = LM_WORLD,         &
                                  __RC__ )

    IF (MAPL_AM_I_ROOT()) write(*,*) 'Nx, Ny: ',Nx, Ny

!    call MAPL_GetResource(MAPL, NX, LABEL="NX:", RC=status)
!    VERIFY_(STATUS)
!    call MAPL_GetResource(MAPL, NY, LABEL="NY:", RC=status)
!    VERIFY_(STATUS)

    ! Create gridded component 
    GrComp = ESMF_GridCompCreate( name         = 'GIGC', &
                                  Grid         = Grid,           &
                                  GridCompType = ESMF_ATM,       &
                                  ConfigFile   = 'MAPL.rc',      &
                                  __RC__  )

    ! Validate the grid
    CALL ESMF_GridValidate( Grid, __RC__ )

    ! Timestep
    CALL ESMF_ConfigGetAttribute( MaplCF, runDt,               &
                                  Label   = "RUN_DT:",         &
                                  Default = 1800,              &
                                  __RC__ )

    ! Start date
    CALL ESMF_ConfigGetAttribute( GeosCF, utcStartDate,        &
                                  Label   = "UTC_START_DATE:", &
                                  Default = 20080101,          &
                                   __RC__ )

    ! Start time
    CALL ESMF_ConfigGetAttribute( GeosCF, utcStartTime,        &
                                  LABEL   = "UTC_START_TIME:", &
                                  Default = 000000,            &
                                   __RC__ )

    ! End date
    CALL ESMF_ConfigGetAttribute( GeosCF, utcEndDate,          &
                                  Label   = "UTC_END_DATE:",   &
                                  Default = 20080101,          &
                                   __RC__ )

    ! End time
    CALL ESMF_ConfigGetAttribute( GeosCF, utcEndTime,          &
                                  LABEL   = "UTC_END_TIME:",   &
                                  Default = 010000,            &
                                   __RC__ )

    ! Split time variables for defining the clock 
    CALL MAPL_UnpackTime( utcStartDate, yy0, mm0, dd0 )
    CALL MAPL_UnpackTime( utcStartTime, h0,  m0,  s0  )
    CALL MAPL_UnpackTime( utcEndDate,   yy1, mm1, dd1 )
    CALL MAPL_UnpackTime( utcEndTime,   h1,  m1,  s1  )

    ! Free the config objects
    CALL ESMF_ConfigDestroy( MaplCF, __RC__ )
    CALL ESMF_ConfigDestroy( GeosCF, __RC__ )

    ! Convert MAPL timestep from seconds to mins
    runDt = runDt / 60

    !-----------------------------------
    ! Create the ESMF Clock object
    !-----------------------------------

    ! Set the calendar type (e.g. "Gregorian")
    CALL ESMF_CalendarSetDefault( ESMF_CAL_GREGORIAN )

    ! Set the starting time (Jul 1, 2008)
    CALL ESMF_TimeSet( startTime, yy=yy0, mm=mm0, dd=dd0, h=h0, m=m0, s=s0 )
    CALL ESMF_TimeSet( stopTime , yy=yy1, mm=mm1, dd=dd1, h=h1, m=m1, s=s1 )

    ! Set the time step (30 mins)
    CALL ESMF_TimeIntervalSet( TimeStep, h=0, m=runDt, s=0, __RC__ )

    ! Create the ESMF clock
    Clock = ESMF_ClockCreate( "Clock",               &
                              timeStep  = TimeStep,  & 
                              startTime = startTime, &
                              stopTime  = stopTime,  &
                               __RC__ )

    !-----------------------------------                                                                                                                                   ! States                                                                                                                                                               !-----------------------------------                                                                                                                                

    ! Create import state                                                                                                                                               
    Import = ESMF_StateCreate( stateName='impGIGC', &
                               stateType=ESMF_STATE_IMPORT, __RC__ )

    ! Create export state                                                                                                                                               
    Export = ESMF_StateCreate( stateName='expGIGC', &
                               stateType=ESMF_STATE_EXPORT, __RC__ )

    !-----------------------------------
    ! Gridded component
    !-----------------------------------

    ! Set the component services.  This identifies the Initialize,
    ! Run, and Finalize methods to the ESMF and MAPL frameworks.
    CALL ESMF_GridCompSetServices( GrComp, SetServices, __RC__ )

    ! Initialize the gridded component
    CALL ESMF_GridCompInitialize( GrComp, Import, Export, Clock, __RC__ )

    !========================================================================
    ! Run
    !========================================================================

    ! timestep counter
    count = 0

    ! Timestepping loop
    DO WHILE ( .not. ESMF_ClockIsStopTime( Clock, RC ) )

       ! Call the Run method of the GEOS-Chem gridded component
       CALL ESMF_GridCompRun( GrComp, Import, Export, Clock, __RC__ )

       ! Advance the clock by one timestep
       CALL ESMF_ClockAdvance( Clock, __RC__ )

       ! Counter
       count = count + 1
    ENDDO

    !========================================================================
    ! Finalize
    !========================================================================

    ! Call the Finalize method of the GEOS-Chem gridded component
    CALL ESMF_GridCompFinalize( GrComp, Import, Export, Clock, __RC__ )

    ! Finalize ESMF
    CALL ESMF_Finalize( __RC__ )

  END SUBROUTINE Main
!EOC
END PROGRAM GIGC
#endif
