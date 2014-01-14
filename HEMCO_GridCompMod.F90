#include "MAPL_Generic.h"

module HEMCO_GridCompMod

  USE ESMF_Mod
  USE ESMF_DistGridMod
  USE MAPL_Mod

  USE GIGC_MPI_Wrap,        ONLY : mpiComm

  ! HEMCO routines/variables
  USE HCO_ERROR_MOD
  USE HCO_STATE_MOD,        ONLY : HCO_State
  USE HCOX_EXTOPT_MOD,      ONLY : OptExt

  ! temporary
  USE CharPak_Mod

  implicit none
  PRIVATE

  public SetServices
 
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE                          :: Initialize_     ! Init method
  PRIVATE                          :: Run_            ! Run method  
  PRIVATE                          :: Finalize_       ! Finalize method
  PRIVATE                          :: Extract_        ! Get values from ESMF
!
! !PRIVATE TYPES:
!
  ! Legacy state
  TYPE EMIS_State
     PRIVATE
     TYPE(ESMF_Config)             :: myCF            ! Private ESMF Config obj
  END TYPE EMIS_State

  ! Hook for the ESMF
  TYPE EMIS_Wrap
     TYPE(EMIS_State), POINTER :: PTR => null()       ! Ptr to EMIS_State
  END TYPE EMIS_Wrap

  INTEGER                          :: logLun          ! LUN for stdout logfile
  CHARACTER(LEN=ESMF_MAXSTR)       :: logFile         ! File for stdout redirect
  CHARACTER(LEN=ESMF_MAXSTR)       :: StdOutFile      ! StdOutFile

  ! HEMCO objects
  TYPE(HCO_State), POINTER      :: HcoState => NULL() ! HEMCO state
  TYPE(OptExt),    POINTER      :: ExtOpt   => NULL() ! Extension options bundle 

  ! HEMCO configuration file:
  CHARACTER(LEN=ESMF_MAXSTR), PARAMETER :: ConfigFile = &
     '/home/ckeller/GIGC/HEMCO_input/HEMCO_Config.GIGCtest'

  ! Extension toggle (for testing purposes)
  LOGICAL, PARAMETER :: DoExt = .FALSE.

contains

!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetServices 
!
! !DESCRIPTION: SetServices routine 


    subroutine SetServices ( GC, RC )

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional  , intent(  OUT) :: RC  ! return code

! !DESCRIPTION:  The SetServices for the emissions GC needs to register its
!   Initialize and Run.  It uses the MAPL_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs and runs their respective SetServices.
!
    type(esmf_vm) :: vm
    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    INTEGER    :: I
    INTEGER    :: N, ID
    INTEGER    :: NPES, MYID, NX, NY
    TYPE (MAPL_METACOMP), POINTER :: MAPL
    TYPE(EMIS_State), POINTER     :: myState        ! Legacy state
    TYPE(EMIS_Wrap)               :: wrap           ! Wrapper for myState
    CHARACTER(LEN=ESMF_MAXSTR)    :: compName       ! Gridded Component name
    LOGICAL                       :: am_I_Root

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // Iam

! nothing to register for this component
! --------------------------------------

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VmGet(VM, localPet=MYID, petCount=npes, rc=status)
    VERIFY_(STATUS)

   ! Set the Initialize, Run and Finalize entry points
   !--------------------------------------------------
   
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT, Initialize_, RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETRUN, Run_, RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETFINAL, Finalize_, RC=STATUS )
   VERIFY_(STATUS)

   !=======================================================================
   ! Wrap internal state for storing in this gridded component
   ! Rename this to a "legacy state"
   !=======================================================================
   ALLOCATE( myState, stat=STATUS )
   VERIFY_(STATUS)
   wrap%ptr => myState

   myState%myCF = ESMF_ConfigCreate(__RC__)
   call ESMF_ConfigLoadFile( myState%myCF, 'GIGC_GridComp.rc', __RC__)
   
   ! Store internal state with Config object in the gridded component
   CALL ESMF_UserCompSetInternalState( GC, 'EMIS_State', wrap, STATUS )
   VERIFY_(STATUS)
   
    !=======================================================================
    !                    %%% MAPL Data Services %%%
    !=======================================================================
!EOC
!BOP
!
! !IMPORT STATE:
!
    ! HEMCO data arrays
    am_I_Root   = MAPL_Am_I_Root()
    CALL HCO_SetServices( am_I_Root, GC, TRIM(ConfigFile), __RC__ )

    ! Other imports (from other components):
!#include "HEMCO_ImportSpec___.h"

     CALL MAPL_AddImportSpec( GC, &
          SHORT_NAME          = 'U10M',               &
          LONG_NAME           = 'sfc_ew_10m_wind',    &
          UNITS               = '1',                  &
          DIMS                = MAPL_DimsHorzOnly,    &
          VLOCATION           = MAPL_VLocationNone,   &
          __RC__ )

     CALL MAPL_AddImportSpec( GC, &
          SHORT_NAME          = 'V10M',               &
          LONG_NAME           = 'sfc_ns_10m_wind',    &
          UNITS               = '1',                  &
          DIMS                = MAPL_DimsHorzOnly,    &
          VLOCATION           = MAPL_VLocationNone,   &
          __RC__ )

    ! Import from GIGC 
!    call MAPL_AddImportSpec(GC,                                   &
!        SHORT_NAME         = 'TRACERS',                           &
!        LONG_NAME          = 'tracer_volume_mixing_ratios',       &
!        UNITS              = 'mol/mol',                           &
!        DIMS               = MAPL_DimsHorzVert,                   &
!        VLOCATION          = MAPL_VLocationCenter,                &
!        DATATYPE           = MAPL_BundleItem,                     &
!                                                          __RC__ )

!
! !INTERNAL STATE:
!
!#include "HEMCO_InternalSpec___.h"

!
! !EXTERNAL STATE:
!
!#include "HEMCO_ExportSpec___.h"
!

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'EMISSIONS',                         &
        LONG_NAME          = 'tracer_surface_emissions',          &
        UNITS              = 'kg/m2/s',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                          __RC__ )
!    call MAPL_AddExportSpec(GC,                                   &
!        SHORT_NAME         = 'DRYDEP',                            &
!        LONG_NAME          = 'tracer_surface_drydep_rates',       &
!        UNITS              = 'm/s',                               &
!        DIMS               = MAPL_DimsHorzOnly,                   &
!        VLOCATION          = MAPL_VLocationNone,                  &
!        DATATYPE           = MAPL_BundleItem,                     &
!                                                       RC=STATUS  )

!EOP

!BOC

! Set services now
! ----------------
   call MAPL_GenericSetServices  ( GC, RC=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize_ 
!
! !DESCRIPTION: Initialize_ is the initialize method of the HEMCO 
!  gridded component. This is a simple ESMF/MAPL wrapper which calls down
!  to the Initialize method of the HEMCO code. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Initialize_( GC, Import, Export, Clock, RC )
!
! !USES:
!
    USE HCO_STATE_MOD,    ONLY : HcoState_Init
    USE HCO_CONFIG_MOD,   ONLY : Config_GetnSpecies 
    USE HCO_CONFIG_MOD,   ONLY : Config_GetSpecNames
    USE HCO_MAIN_MOD,     ONLY : HCO_INIT
    USE HCOX_DRIVER_MOD,  ONLY : HCOX_INIT
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC      ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import  ! Import state
    TYPE(ESMF_State),    INTENT(INOUT)         :: Export  ! Export state
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock   ! ESMF clock object
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Dec 2013 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Objects
    TYPE(ESMF_Grid)              :: Grid        ! ESMF Grid object
    TYPE(ESMF_Config)            :: MaplCF      ! ESMF Config obj (MAPL.rc)
    TYPE(ESMF_Config)            :: GeosCF      ! ESMF Config obj (GIGC*.rc) 
    TYPE(MAPL_METACOMP), POINTER :: MAPL    
                                   
    ! Scalars                                   
    LOGICAL                     :: am_I_Root   ! Are we on the root CPU?
    INTEGER                     :: error       ! HEMCO error code
    INTEGER                     :: myPet       ! # of the CPU we are on 
    INTEGER                     :: IM          ! # of longitudes on this CPU
    INTEGER                     :: JM          ! # of latitudes  on this CPU
    INTEGER                     :: LM          ! # of levels     on this CPU
    REAL                        :: tsChem      ! Chemistry timestep [s]
    REAL                        :: tsDyn       ! Dynamic timestep [s]
    INTEGER                     :: nPets       ! Total # of CPUs 
    CHARACTER(LEN=5)            :: petStr      ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
     
    ! Pointer arrays
    REAL(ESMF_KIND_R4), POINTER :: lonCtr(:,:) ! Lon centers on this CPU [rad]
    REAL(ESMF_KIND_R4), POINTER :: latCtr(:,:) ! Lat centers on this CPU [rad]
    REAL,               POINTER :: u10m  (:,:) ! 

    ! TRACERS bundle (import)
    TYPE(ESMF_FieldBundle)       :: trcBUNDLE  ! Tracer field bundle
    TYPE(ESMF_Field      )       :: trcFIELD   ! Tracer field

    ! HEMCO bundle (internal)
    TYPE(ESMF_FieldBundle)       :: hcoBUNDLE
    TYPE(ESMF_Field)             :: hcoFIELD

    ! Working variables
    CHARACTER(LEN=ESMF_MAXSTR)   :: trcNAME
    CHARACTER(LEN=ESMF_MAXSTR)   :: hcoNAME
    INTEGER                      :: I, N    
    REAL                         :: TCVV
    REAL, POINTER                :: Arr3D(:,:,:) => NULL()

    !=======================================================================
    ! Initialize_ begins here!
    !=======================================================================

    ! Error handling
    __Iam__('Initialize_')

    ! Assume success
    ERROR = HCO_SUCCESS

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    VERIFY_(STATUS)
    Iam = trim( compName ) // '::' // trim( Iam )

!    ! Get my MAPL_Generic state
!    !--------------------------
!    call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
!    VERIFY_(STATUS)

    ! Initialize MAPL Generic
    !--------------------------
    CALL MAPL_GenericInitialize( GC, Import, Export, Clock, __RC__ )
    VERIFY_(STATUS)

    ! Test if we are on the root CPU
    am_I_Root = MAPL_Am_I_Root()

    !=======================================================================
    ! Get various parameters from the ESMF/MAPL framework
    !=======================================================================

    CALL Extract_( GC,                   &  ! Ref to this Gridded Component
                   Clock,                &  ! ESMF Clock object
                   Grid      = Grid,     &  ! ESMF Grid object
                   MaplCf    = MaplCF,   &  ! ESMF Config obj (MAPL*.rc) 
                   GeosCf    = GeosCF,   &  ! ESMF Config obj (GIGC*.rc)
                   tsDyn     = tsDyn,    &  ! Dynamic timestep [sec]
                   tsChem    = tsChem,   &  ! Chemistry timestep [sec]
                   IM        = IM,       &  ! # of longitudes on this PET
                   JM        = JM,       &  ! # of latitudes  on this PET
                   LM        = LM,       &  ! # of levels     on this pET
                   lonCtr    = lonCtr,   &  ! sfc. lon ctrs [rad]
                   latCtr    = latCtr,   &  ! sfc. lat ctrs [rad]
                   localpet  = myPet,    &  ! # of the CPU we are on now
                   petCount  = nPets,    &  ! Total # of CPUs
                   __RC__ )

    !=======================================================================
    ! Open a log file on each PET where stdout will be redirected
    !=======================================================================

    ! Name of logfile for stdout redirect
!    CALL ESMF_ConfigGetAttribute( GeosCF, StdOutFile,          &
!                                  Label   = "STDOUT_LOGFILE:", &
!                                  Default = "PET%%%%%.init",   &
!                                   __RC__ )

!    ! Name of log LUN # for stdout redirect
!    CALL ESMF_ConfigGetAttribute( GeosCF, logLun,              &
!                                  Label   = "STDOUT_LOGLUN:",  &
!                                  Default = 700,               &
!                                   __RC__ )

    StdOutFile = 'HEMCO.log'
    logLun     = 800

    ! Replace tokens w/ PET # in the filename
    IF ( am_I_Root ) THEN
       logFile = StdOutFile
       WRITE( petStr, '(i5.5)' ) myPet
       CALL StrRepl( logFile, '%%%%%', petStr )
    ENDIF

    ! Open file for stdout redirect
    IF ( am_I_Root )  THEN
       OPEN ( UNIT=logLun, FILE=TRIM(logFile), STATUS='UNKNOWN' )
    ENDIF

    ! Add descriptive header text
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(a)'   ) REPEAT( '#', 79 )
       WRITE( logLun, 100     ) TRIM( logFile ), TRIM( Iam ), myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    !=======================================================================
    ! Initialize HEMCO state object 
    !=======================================================================

    ! Get number of species registered from configuration file
    N = Config_GetnSpecies () 

    ! Initialize HEMCO state
    IF (am_I_Root) WRITE( logLun, * ) 'Do HcoState_Init'
    CALL HcoState_Init ( am_I_Root, HcoState, N, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF
    IF (am_I_Root) WRITE( logLun, * ) 'HcoState_Init done'

    !=======================================================================
    ! Set variables in HcoState 
    !=======================================================================

    ! ----------------------------------------------------------------------
    ! General
    HcoState%isESMF     =  .TRUE. 
    HcoState%IMPORT     => IMPORT
    HcoState%ConfigFile =  ConfigFile

    ! ----------------------------------------------------------------------
    ! Set species information 

    ! Species names
    CALL Config_GetSpecNames( HcoState%SpcName, N, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF

    ! --> All other species properties are not of relevance 
    !     in the ESMF environment, hence stick with the default
    !     values! 

    ! ----------------------------------------------------------------------
    ! TODO: Remove from here...
    ! Set size bins of coarse and accumulation mode aerosols
    ! NOTE: primarily used for aerosol extensions
    HcoState%SALA_REDGE_um = 0d0
    HcoState%SALC_REDGE_um = 0d0

    ! ----------------------------------------------------------------------
    ! Define grid on this CPU

    ! Grid dimensions
    HcoState%NX = IM
    HcoState%NY = JM
    HcoState%NZ = LM

    ! Set grid midpoints (convert from rad to deg)
    ALLOCATE ( HcoState%XMID(IM,JM,1), STAT=RC )
    IF ( RC/= ESMF_SUCCESS ) RETURN
    ALLOCATE ( HcoState%YMID(IM,JM,1), STAT=RC )
    IF ( RC/= ESMF_SUCCESS ) RETURN
    HcoState%XMID(:,:,1) = lonCtr(:,:) * 180d0 / HcoState%PI
    HcoState%YMID(:,:,1) = latCtr(:,:) * 180d0 / HcoState%PI
  
    ! grid sizes (area and height).
    ! NOTE: these variables are used by some of the extensions. No need
    ! to define them if no extensions are used. 
    HcoState%AREA_M2    => NULL() !AREA_M2
    HcoState%BXHEIGHT_M => NULL() !State_Met%BXHEIGHT
 
    ! grid edge information is only used for HEMCO internal regridding
    ! routines, which are never called in an ESMF environment. Hence
    ! leave pointers nullified.
    HcoState%XEDGE => NULL() 
    HcoState%YEDGE => NULL() 
    HcoState%YSIN  => NULL() 

    ! ----------------------------------------------------------------------
    ! Emission and dynamics timestep in seconds
    ! --> For now, assume that emission timestep = chemistry timestep!!
    HcoState%TS_EMIS = tsChem
    HcoState%TS_DYN  = tsDyn

    ! ----------------------------------------------------------------------
    ! Current timestamps 
    ! ----------------------------------------------------------------------
    CALL HcoState_SetTime ( Clock, HcoState, __RC__ )

    !=======================================================================
    ! Initialize HEMCO internal lists and variables 
    !=======================================================================
 
    CALL HCO_INIT ( am_I_Root, HcoState, ERROR ) 
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF

    !=======================================================================
    ! Initialize HEMCO extensions 
    !=======================================================================

    ! temp. toggle
    if ( DoExt ) then 

    ! Set current datetime in HcoState first (needed to read some 
    ! restart files, e.g. for soil NOx)
    ! TODO: set species information properly. Probably best to 
    ! explicitly set the molecular weights (?)
    CALL HCOX_INIT ( am_I_Root, HcoState, ExtOpt, ERROR )

    ! Error trap
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF

    endif ! temp. toggle

    !=======================================================================
    ! Define HEMCO data bundle
    ! Each field of the bundle corresponds to an array of HcoState and will
    ! be populated with the emissions calculated by HEMCO.
    ! Additional properties will be added in the chemistry component! 
    !=======================================================================

    call ESMF_StateGet(Export, 'EMISSIONS', HcoBUNDLE, __RC__ )

    ! Empty data array to be copied to each field bundle
    ALLOCATE(Arr3D(IM,JM,LM),STAT=STATUS)
    VERIFY_(STATUS)
    Arr3D = 0d0

    ! Add fields to bundle
    DO I = 1, N

       ! Extract species name
       HcoNAME = HcoState%SpcName(I)

       ! Create field. 
       HcoFIELD = ESMF_FieldCreate ( Grid,                      &
                                     Arr3D,                     &
                                     copyflag = ESMF_DATA_COPY, &
                                     name     = HcoNAME,        &
                                                        __RC__ )
 
!       call ESMF_AttributeSet (HcoFIELD,    &
!            NAME  = 'ModSpcID',             &
!            VALUE = -1,                     &
!                                    __RC__ )       
       
!       call ESMF_AttributeSet (HcoFIELD,    &
!            NAME  = 'MolWeight_gmol-1',     &
!            VALUE = HcoState%SpecMW(I),     &
!                                    __RC__ )       
       
       call ESMF_FieldBundleAdd ( HcoBUNDLE, HcoFIELD, __RC__ )
    ENDDO

   ! Set bundle attributes
    call ESMF_AttributeSet (HcoBUNDLE,    &
         NAME  = 'N_SPECIES',             &
         VALUE = N,                       &
                               __RC__    )

    ! Don't need temporary array anymore
    IF ( ASSOCIATED(Arr3D) ) DEALLOCATE(Arr3D)
       
    ! Verbose mode
    if (AM_I_ROOT) then !.and. HcoState%verbose) then
       print *, trim(Iam)//': EMISSIONS bundle during Initialization:' 
       call ESMF_FieldBundlePrint ( HcoBUNDLE )
    end if

    IF (am_I_Root) WRITE( logLun, * ) 'Bundle defined'

    !=======================================================================
    ! All done
    !=======================================================================

    ! Write a header before the timestepping begins
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(/,a)' ) REPEAT( '#', 79 )
       WRITE( logLun, 200     ) TRIM( compName ) // '::Init_', myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    ! Close the file for stdout redirect.  Reopen when executing the run method.
    IF ( am_I_Root )  CLOSE ( UNIT=logLun )

    ! Successful return
    RETURN_(ESMF_SUCCESS)

    ! Formats
100 FORMAT( '### ',                                           / &
            '### ', a ,                                       / &
            '### ', a, '  |  Initialization on PET # ', i5.5, / &
            '### ' )
200 FORMAT( '### ',                                           / &
            '### ', a, '  |  Execution on PET # ',      i5.5, / &
            '###' )

  END SUBROUTINE Initialize_

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run_
!
! !DESCRIPTION: Run_ is the run method of the GEOSCHEMchem gridded component.  
!  GC is a simple ESMF/MAPL wrapper which calls down to the Run method of 
!  the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run_( GC, Import, Export, Clock, RC )
!
! !USES:
!
    USE HCO_ARRAY_MOD,      ONLY : HCO_ArrReset 
    USE HCO_MAIN_MOD,       ONLY : HCO_RUN
    USE HCOX_DRIVER_MOD,    ONLY : HCOX_RUN

!#   include "GIGCemis_DeclarePointer___.h"        ! Ptr decls to states
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import      ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export      ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock       ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now uses the updated Extract_ method
!  09 Apr 2010 - R. Yantosca - Initialize Timing, GeoLoc objects
!  16 Apr 2010 - R. Yantosca - Now move the array assignments before & after
!                              the call to GC_CHUNK_RUN into separate
!                              include files, for clarity
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now use IDENT%VERBOSE to trigger debug output
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root CPU
!  16 Oct 2012 - R. Yantosca - Now Include freeform files Includes_Before_Run.H
!                              and Includes_After_Run.H
!  13 Feb 2013 - R. Yantosca - Now call MAPL_Get_SunInsolation to return
!                              solar zenith angle and related properties
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!  
    ! Objects
    TYPE(ESMF_Grid)              :: Grid          ! ESMF Grid object
    TYPE(ESMF_Config)            :: MaplCF        ! Config (MAPL.rc)
    TYPE(ESMF_Config)            :: GeosCF        ! Config (GIGC*.rc)
                                                  
    ! Scalars                                     
    LOGICAL                      :: am_I_Root     ! Are we on the root CPU?
    INTEGER                      :: error         ! G-C error return code
    CHARACTER(LEN=ESMF_MAXSTR)   :: compName      ! Gridded Component name

    ! HEMCO bundle
    TYPE(ESMF_FieldBundle)       :: hcoBUNDLE
    REAL, POINTER                :: fPtrArray(:,:,:) => NULL()
    INTEGER                      :: I, N

    ! Pointers for IMPORT
    REAL, POINTER, DIMENSION(:,:)   :: U10M
    REAL, POINTER, DIMENSION(:,:)   :: V10M

    !=======================================================================
    ! Run_ begins here!
    !=======================================================================
    
    __Iam__('Run_')

    ! Assume success
    error = HCO_SUCCESS

    ! Are we on the root CPU?
    am_I_Root = MAPL_Am_I_Root()

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    Iam = TRIM( compName ) // '::' // TRIM( Iam )

    ! Get pointers to fields in import, internal, and export states
!#   include "Emis_GetPointer___.h"

    ! Re-open file for stdout redirect
    IF ( am_I_Root )  THEN
     OPEN ( UNIT=logLun, FILE=TRIM(logFile), STATUS='OLD' , ACTION='WRITE', ACCESS='APPEND' )
    END IF

    !=======================================================================
    ! pre-Run method array assignments
    !=======================================================================

!#   include "Includes_Before_Emis.H"

    ! Connect HEMCO state object with HEMCO bundle
    call ESMF_StateGet(Export, 'EMISSIONS', hcoBUNDLE, __RC__ )
    call ESMF_FieldBundleGet( hcoBUNDLE, fieldCount=N, __RC__ )
    DO I = 1, N
       call ESMFL_BundleGetPointerToData( hcoBUNDLE, I, fPtrArray, __RC__ )  
       HcoState%Emsr3D(I)%Arr3D => fPtrArray 
       fPtrArray => NULL() 
    ENDDO

    ! Set pointers to Met fields
    call MAPL_GetPointer ( IMPORT, U10M,  'U10M', __RC__ )
    call MAPL_GetPointer ( IMPORT, V10M,  'V10M', __RC__ )

    ! temp. toggle
    if ( DoExt ) then 

    ! SeaFlux
    IF ( ASSOCIATED ( ExtOpt%SeaFlxOpt ) ) THEN
       ExtOpt%SeaFlxOpt%U10M    => U10M
       ExtOpt%SeaFlxOpt%V10M    => V10M
       ! etc.
    ENDIF

    endif

    ! etc.
!    ! don't forget pointer to boxheight!!
!    HcoState%BXHEIGHT_M => State_Met%BXHEIGHT

    !=======================================================================
    ! Reset all emission and deposition values in HcoState
    !=======================================================================
    CALL HCO_ArrReset ( HcoState, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.)
    ENDIF
   
    !=======================================================================
    ! Set HEMCO calculation options 
    !=======================================================================

    ! Range of tracers and emission categories.
    ! Set Extension number ExtNr to 0, indicating that the core
    ! module shall be executed. 
    HcoState%SpcMin =  1
    HcoState%SpcMax = -1 
    HcoState%CatMin =  1
    HcoState%CatMax = -1
    HcoState%ExtNr  =  0

    ! Set current datetime
    CALL HcoState_SetTime ( Clock, HcoState, __RC__ )

    ! Use temporary array?
    HcoState%FillTemp3D = .FALSE.
 
    !=======================================================================
    ! Run HEMCO and write emissions into HcoState 
    !=======================================================================
    CALL HCO_RUN ( am_I_Root, HcoState, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.)
    ENDIF

    !=======================================================================
    ! Run HEMCO extensions. Emissions are added to HcoState 
    !=======================================================================

    ! temp. toggle
    if ( DoExt ) then

    CALL HCOX_RUN ( am_I_Root, HcoState, ExtOpt, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.)
    ENDIF

    endif ! temp. toggle

    !=======================================================================
    ! post-Run method array assignments
    !=======================================================================

!#   include "Includes_After_Emis.H"

    ! temp. toggle
    if ( DoExt ) then

    ! SeaFlux
    IF ( ASSOCIATED ( ExtOpt%SeaFlxOpt ) ) THEN
       ExtOpt%SeaFlxOpt%U10M    => NULL() 
       ExtOpt%SeaFlxOpt%V10M    => NULL()
       ! etc.
    ENDIF

    endif

    ! Disconnect HEMCO state object with HEMCO bundle
    DO I = 1, N
       HcoState%Emsr3D(I)%Arr3D => NULL() 
    ENDDO

    !=======================================================================
    ! All done
    !=======================================================================

    ! Close the file for stdout redirect.
    IF ( am_I_Root )  CLOSE ( UNIT=logLun )

    ! Successful return
    RETURN_(ESMF_SUCCESS)

    ! Formats
100 FORMAT( '---> DATE: ', i4.4, '/', i2.2, '/', i2.2,      &
            '  GMT: ', i2.2, ':', i2.2, '  X-HRS: ', f11.3 )
110 FORMAT( 'Box (',i3,',',i3,') on PET ', i3, ' has coords: ', 2f7.2, &
               ' LocT = ', f9.4 )

  end subroutine Run_

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finalize_
!
! !DESCRIPTION: Finalize_ is the finalize method of the HEMCO gridded
! component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Finalize_ 
!
! !USES:
!
  USE HCO_MAIN_MOD,      ONLY : HCO_FINAL
  USE HCOX_DRIVER_MOD,   ONLY : HCOX_FINAL
  USE HCO_STATE_MOD,     ONLY : HcoState_Final
!
! !REVISION HISTORY:
!  02 Jan 2014 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC

    !=======================================================================
    ! FINALIZE_ begins here!
    !=======================================================================

    ! temp. toggle (testing only)
    if ( DoExt ) then
    ! Cleanup extensions and ExtOpt object 
    CALL HCOX_FINAL ( HcoState, ExtOpt )
    endif

    ! Cleanup HCO core
!    CALL HCO_FINAL

    ! Cleanup HcoState object
!    CALL HcoState_Final ( HcoState ) 

  end subroutine Finalize_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Extract_
!
! !DESCRIPTION: GC routine extracts several common quantities from the 
!  ESMF/MAPL environment so that they can be later passed down to the 
!  grid-independent GEOS-Chem code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Extract_( GC,       Clock,    Grid,    MaplCF, GeosCF,    &
                       localPet, petCount, I_LO,    J_LO,   I_HI,      &
                       nymdB,    nymdE,    nhmsB,   nhmsE,             &
                       J_HI,     IM,       JM,      LM,     IM_WORLD,  &
                       JM_WORLD, LM_WORLD, lonCtr,  latCtr,            &
                       tsChem,   tsDyn,    mpiComm, ZTH,   SLR,        &
                       RC )

!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!                                                             
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC          ! GC grid comp
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    !----------------------------------
    ! ESMF and/or MAPL quantities
    !----------------------------------
    TYPE(ESMF_Grid),     INTENT(OUT), OPTIONAL :: Grid        ! ESMF Grid obj
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: MaplCF      ! MAPL.rc
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: GeosCF      ! GIGC*.rc
    INTEGER,             INTENT(OUT), OPTIONAL :: localPet    ! This PET
    INTEGER,             INTENT(OUT), OPTIONAL :: petCount    ! Total # of PETs
    INTEGER,             INTENT(OUT), OPTIONAL :: mpiComm     ! MPI Communicator Handle

    !----------------------------------
    ! Local grid coordinates 
    ! (defined on the current CPU)
    !----------------------------------
    INTEGER,             INTENT(OUT), OPTIONAL :: I_LO        ! Min lon index
    INTEGER,             INTENT(OUT), OPTIONAL :: J_LO        ! Min lat index
    INTEGER,             INTENT(OUT), OPTIONAL :: I_HI        ! Max lon index
    INTEGER,             INTENT(OUT), OPTIONAL :: J_HI        ! Max lat index
    INTEGER,             INTENT(OUT), OPTIONAL :: IM          ! Total # lons
    INTEGER,             INTENT(OUT), OPTIONAL :: JM          ! Total # lats
    INTEGER,             INTENT(OUT), OPTIONAL :: LM          ! Total # levs
    REAL(ESMF_KIND_R4),  POINTER,     OPTIONAL :: lonCtr(:,:) ! Lon ctrs [rad]
    REAL(ESMF_KIND_R4),  POINTER,     OPTIONAL :: latCtr(:,:) ! Lat ctrs [rad]

    !----------------------------------
    ! Global grid coordinates
    !----------------------------------
    INTEGER,             INTENT(OUT), OPTIONAL :: IM_WORLD    ! Global # lons
    INTEGER,             INTENT(OUT), OPTIONAL :: JM_WORLD    ! Global # lats
    INTEGER,             INTENT(OUT), OPTIONAL :: LM_WORLD    ! Global # levs

    !-----------------------------------                     
    ! Time information 
    !-----------------------------------                     
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdB       ! YYYYMMDD @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdE       ! YYYYMMDD @ end
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsB       ! hhmmss @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsE       ! hhmmss @ end

    !-----------------------------------                     
    ! Timestep variables [seconds]          
    !-----------------------------------                     
    REAL,                INTENT(OUT), OPTIONAL :: tsChem      ! Chemistry
    REAL,                INTENT(OUT), OPTIONAL :: tsDyn       ! Dynamics

    !-----------------------------------                     
    ! Solar parameters
    !-----------------------------------                     
    REAL,                INTENT(OUT), OPTIONAL :: ZTH(:,:)    ! Solar zth angle
    REAL,                INTENT(OUT), OPTIONAL :: SLR(:,:)    ! Insolation

    !-----------------------------------                        
    ! Return code 
    !-----------------------------------                     
    INTEGER,             INTENT(OUT), OPTIONAL :: RC          ! 0 = all is well
!
! !REMARKS:
!  If you need to obtain a quantity not returned by this routine, you can
!  manually extract it from the MaplCF or GeosCF configuration objects.
!
! !REVISION HISTORY:
!  01 Dec 2009 - A. Da Silva - Initial version
!  07 Apr 2010 - R. Yantosca - Added ProTeX headers
!  08 Apr 2010 - R. Yantosca - Make all outputs optional
!  08 Apr 2010 - R. Yantosca - Added outputs for localPet, petCount
!  08 Apr 2010 - R. Yantosca - Added outputs for individual time values
!                              as well as elapsed time (hours)
!  13 Apr 2010 - R. Yantosca - Now take tsDyn from the MAPL "RUN_DT:" setting
!  30 Nov 2012 - R. Yantosca - Now return IM_WORLD, JM_WORLD, LM_WORLD
!  30 Nov 2012 - R. Yantosca - Now return local indices I_LO, J_LO, I_HI, J_HI
!  05 Dec 2012 - R. Yantosca - Removed latEdg argument; cosmetic changes
!  13 Feb 2013 - E. Nielsen  - Restart file inquiry for GEOS-5
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    ! Objects
    TYPE(ESMF_Time)               :: startTime      ! ESMF start time obj
    TYPE(ESMF_Time)               :: currTime       ! ESMF current time obj
    TYPE(ESMF_TimeInterval)       :: elapsedTime    ! ESMF elapsed time obj
    TYPE(ESMF_VM)                 :: VM             ! ESMF VM object
    TYPE(EMIS_State), POINTER     :: myState        ! Legacy state
    TYPE(EMIS_Wrap)               :: wrap           ! Wrapper for myState
    TYPE(MAPL_MetaComp),  POINTER :: metaComp       ! MAPL MetaComp object
    TYPE(MAPL_SunOrbit)           :: sunOrbit

    ! Scalars
    CHARACTER(len=ESMF_MAXSTR)    :: compName       ! Gridded component name
    INTEGER(ESMF_KIND_I8)         :: count          ! # of clock advances
    INTEGER                       :: locDims(3)     ! Array for local dims
    INTEGER                       :: globDims(3)    ! Array for global dims
    INTEGER                       :: IL,   IU       ! Min/max local lon indices
    INTEGER                       :: JL,   JU       ! Min/max local lat indices

    !=======================================================================
    ! Initialization
    !=======================================================================

    __Iam__('Extract_')

    ! Get my name and set-up traceback handle
    CALL ESMF_GridCompGet( GC, name=compName, vm=VM, __RC__ )
    Iam = TRIM( compName ) // '::' // TRIM( Iam )

    ! Get the internal state which holds the private Config object
    CALL ESMF_UserCompGetInternalState( GC, 'EMIS_State', wrap, STATUS )
    VERIFY_(STATUS)
    myState => wrap%ptr

    ! Assume successful return
    IF ( PRESENT( RC ) ) RC = ESMF_SUCCESS

    ! Zero variables
    locDims  = 0
    globDims = 0
    IL       = 0
    JL       = 0
    IU       = 0
    JU       = 0

    !=======================================================================
    ! Extract information from ESMF VM object
    !=======================================================================

    ! Index of the PET we are on now
    IF ( PRESENT( localPet ) ) THEN
       CALL ESMF_VmGet( VM, localPet=localPet, __RC__ )
    ENDIF

    ! Total # of PETs used by this gridded component
    IF ( PRESENT( petCount ) ) THEN
       CALL ESMF_VmGet( VM, petCount=petCount, __RC__ )
    ENDIF

    ! Global MPI Communicator Handle
    IF ( PRESENT( mpiComm ) ) THEN
       CALL ESMF_VmGet( VM, mpicommunicator=mpiComm, __RC__ )
    ENDIF

    !=======================================================================
    ! Extract information from ESMF Config objects
    !=======================================================================

    ! Get the Config object based on "MAPL.rc"
    CALL ESMF_GridCompGet( GC, Config=MaplCF, __RC__ )
    
    ! Get the Config object based on "GIGC_GridComp.rc"
    GeosCF = myState%myCF

    ! Dynamic timestep (convert to minutes)
    IF ( PRESENT( tsDyn ) ) THEN
       CALL ESMF_ConfigGetAttribute( MaplCF, tsDyn,                       &
                                     Label="RUN_DT:",             __RC__ )
    ENDIF

    ! Chemistry timestep (convert to minutes)
    IF ( PRESENT( tsChem ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, tsChem,                      &
                                     Label="CHEMISTRY_TIMESTEP:", __RC__ )
    ENDIF

    ! Start date
    IF ( PRESENT( nymdb ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nymdB,                       &
                                     Label   = "UTC_START_DATE:", __RC__ )
    ENDIF

    ! Start time
    IF ( PRESENT( nhmsB ) ) then
       CALL ESMF_ConfigGetAttribute( GeosCF, nhmsB,                       &
                                     LABEL   = "UTC_START_TIME:", __RC__ )
    ENDIF

    ! End date
    IF ( PRESENT( nymdE ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nymdE,                       &
                                     Label   = "UTC_END_DATE:",   __RC__ )
    ENDIF

    ! End time
    IF ( PRESENT( nhmsE ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nhmsE,                       &
                                     LABEL   = "UTC_END_TIME:",  __RC__ )
    ENDIF

    !=======================================================================
    ! Extract grid information
    !=======================================================================
    IF ( PRESENT( Grid ) ) THEN
    
       ! Get the ESMF grid attached to this gridded component
       CALL ESMF_GridCompGet( GC, grid=Grid, __RC__ )

       ! Get # of dimensions on this pet, and globally
       CALL MAPL_GridGet( Grid,                                        &
                          localCellCountPerDim  = locDims,             &
                          globalCellCountPerDim = globDims,            &
                          __RC__ )
          
       ! Get the upper and lower bounds of on each PET
       CALL GridGetInterior( Grid, IL, IU, JL, JU, __RC__  )

    ENDIF

    ! Save fields for return
    IF ( PRESENT( I_LO     ) ) I_LO     = IL
    IF ( PRESENT( J_LO     ) ) J_LO     = JL
    IF ( PRESENT( I_HI     ) ) I_HI     = IU
    IF ( PRESENT( J_HI     ) ) J_HI     = JU
    IF ( PRESENT( IM       ) ) IM       = locDims(1)
    IF ( PRESENT( JM       ) ) JM       = locDims(2)
    IF ( PRESENT( LM       ) ) LM       = locDims(3)
    IF ( PRESENT( IM_WORLD ) ) IM_WORLD = globDims(1)
    IF ( PRESENT( JM_WORLD ) ) JM_WORLD = globDims(2)
    IF ( PRESENT( LM_WORLD ) ) LM_WORLD = globDims(3)

    ! Get horizontal coordinate variables
    CALL MAPL_GetObjectFromGC( GC, metaComp, __RC__ )

    ! Longitude values on this PET
    IF ( PRESENT( lonCtr ) ) THEN
       CALL MAPL_Get( metaComp, lons=lonCtr, __RC__ )
    ENDIF

    ! Latitude values on this PET
    IF ( PRESENT( latCtr ) ) THEN
       CALL MAPL_Get( metaComp, lats=latCtr, __RC__ )
    ENDIF

    !=======================================================================
    ! Get solar zenith angle enformation
    !=======================================================================
    IF ( PRESENT( ZTH    ) .and. PRESENT( SLR    )  .and. &
         PRESENT( lonCtr ) .and. PRESENT( latCtr ) ) THEN
         
       ! Get the Orbit object (of type MAPL_SunOrbit),
       ! which is used in the call to MAPL_SunGetInsolation
       CALL MAPL_Get( metaComp,                       &
                      LONS      = lonCtr,             &
                      LATS      = latCtr,             &
                      ORBIT     = sunOrbit,           &
                      __RC__                         )

       ! Get the solar zenith angle and solar insolation
       ! NOTE: ZTH, SLR are allocated outside of this routine
       CALL MAPL_SunGetInsolation( LONS  = lonCtr,    &
                                   LATS  = latCtr,    &
                                   ORBIT = sunOrbit,  &
                                   ZTH   = ZTH,       &
                                   SLR   = SLR,       &
                                   CLOCK = Clock,     &
                                   __RC__            )

    ENDIF

    !=======================================================================
    ! All done
    !=======================================================================
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Extract_
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gridGetInterior
!
! !DESCRIPTION: Given an ESMF grid, returns the lower and upper longitude
!  and latitude indices on a given PET.
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE GridGetInterior( Grid, I1, IN, J1, JN, RC )
!
! !INPUT PARAMETERS: 
!
    TYPE(ESMF_Grid), INTENT(IN)  :: Grid   ! ESMF Grid object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(OUT) :: I1     ! Lower lon index on this PET
    INTEGER,         INTENT(OUT) :: IN     ! Upper lon index on this PET
    INTEGER,         INTENT(OUT) :: J1     ! Lower lat index on this PET
    INTEGER,         INTENT(OUT) :: JN     ! Upper lat index on this PET
    INTEGER,         INTENT(OUT) :: RC     ! Success/failure
!
! !REMARKS:
!  This was a PRIVATE routine named MAPL_GridGetInterior within MAPL_Base.F90.
!  I have pulled the source code from there.
! 
! !REVISION HISTORY: 
!  30 Nov 2012 - R. Yantosca - Initial version, based on MAPL_Base
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                               :: status
    CHARACTER(LEN=ESMF_MAXSTR)            :: IAm='MAPL_GridGetInterior'

    TYPE(ESMF_DistGrid)                   :: distGrid
    TYPE(ESMF_DELayout)                   :: LAYOUT
    INTEGER,               ALLOCATABLE    :: AL(:,:)
    INTEGER,               ALLOCATABLE    :: AU(:,:)
    INTEGER                               :: nDEs
    INTEGER                               :: deId
    INTEGER                               :: gridRank
    INTEGER                               :: deList(1)

    ! Get ESMF DistGrid object
    CALL ESMF_GridGet    ( GRID,                           &
                           dimCount        = gridRank,     &
                           distGrid        = distGrid,     &
                           __RC__ )                    
 
    ! Get ESMF DELayout object
    CALL ESMF_DistGridGet( distGRID,                       &
                           delayout        =layout,        &
                           __RC__ )                    
                                                       
                
    ! Get the # of DE's and the list of DE's
    CALL ESMF_DELayoutGet( layout,                         &
                           deCount         = nDEs,         &
                           localDeList     = deList,       &
                           __RC__ )

    deId = deList(1)

    ! Allocate memory
    ALLOCATE( AL( gridRank, 0:nDEs-1 ), stat=status )
    ALLOCATE( AU( gridRank, 0:nDEs-1 ), stat=status )

    ! Get the min/max lon/lat values on each PET
    CALL ESMF_DistGridGet( distgrid,                       &
                           minIndexPDimPDe = AL,           &
                           maxIndexPDimPDe = AU,           &
                           __RC__ )

    ! Local Lon indices
    I1 = AL( 1, deId )   ! Lower
    IN = AU( 1, deId )   ! Upper

    ! Local lat indices
    J1 = AL( 2, deId )   ! Lower
    JN = AU( 2, deId )   ! Upper
 
    ! Free memory
    DEALLOCATE(AU, AL)
   
    ! Return successfully
    RC = HCO_SUCCESS

  END SUBROUTINE GridGetInterior
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_SetServices
!
! !DESCRIPTION: Subroutine HCO\_SetServices registers all required HEMCO 
! data so that it can be imported through the ESMF import state. 
! This routine is called at the beginning of a simulation - even ahead of 
! the initialization routines. Since this routine is called from outside of 
! the HEMCO environment, use the MAPL specific error codes! 
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_SetServices( am_I_Root, GC, ConfigFile, RC ) 
!
! !USES:
!
      USE HCO_DATACONT_MOD, ONLY : ListCont
      USE HCO_CONFIG_MOD,   ONLY : Config_ReadFile, GetNextCont
      USE HCO_CONFIG_MOD,   ONLY : Config_ScalIDinUse
!
! !ARGUMENTS:
!
      LOGICAL,             INTENT(IN   )   :: am_I_Root
      TYPE(ESMF_GridComp), INTENT(INOUT)   :: GC
      CHARACTER(LEN=*),    INTENT(IN   )   :: ConfigFile
      INTEGER,             INTENT(  OUT)   :: RC
!
! !REVISION HISTORY:
!  29 Aug 2013 - C. Keller - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER                    :: FLAG
      TYPE(ListCont), POINTER    :: CurrCont => NULL()

      ! ================================================================
      ! HCO_SetServices begins here
      ! ================================================================

      ! For MAPL/ESMF error handling (defined Iam and STATUS)
      __Iam__('HCO_SetServices (HEMCO_GridCompMod.F90)') 

      ! ---------------------------------------------------------------------
      ! Read file into buffer
      ! ---------------------------------------------------------------------

      CALL Config_ReadFile( am_I_Root, TRIM(ConfigFile), STATUS )
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ASSERT_(.FALSE.)
      ENDIF

      ! Loop over all lines and set services according to input file content
      CALL GetNextCont ( CurrCont, FLAG )
      DO WHILE ( FLAG == HCO_SUCCESS ) 

         ! Skip containers that are not defined
         IF ( .NOT. ASSOCIATED(CurrCont%Dta) ) THEN
            CALL GetNextCont ( CurrCont, FLAG )
            CYCLE
         ENDIF

         ! For scale factor fields, check if this scale factor is used
         ! by any of the base fields
         IF ( CurrCont%Dta%DataType > 1 ) THEN
            IF ( .NOT. Config_ScalIDinUse( CurrCont%Dta%ScalID ) ) THEN
               CALL GetNextCont ( CurrCont, FLAG )
               CYCLE
            ENDIF
         ENDIF         

         ! Ignore containers with ncRead flag disabled. These are typically
         ! scalar fields directly read from the configuration file. 
         IF ( .NOT. CurrCont%Dta%ncRead ) THEN 
            ! don't do anything

         ! 2D data
         ELSEIF ( CurrCont%Dta%SpaceDim == 2 ) THEN

            CALL MAPL_AddImportSpec(GC,                  &
               SHORT_NAME = TRIM(CurrCont%Dta%cName),    &
               LONG_NAME  = TRIM(CurrCont%Dta%cName),    &
               UNITS      = TRIM(CurrCont%Dta%OrigUnit), &
               DIMS       = MAPL_DimsHorzOnly,           &
               VLOCATION  = MAPL_VLocationNone,          &
               RC         = STATUS                        )
            VERIFY_(STATUS)

         ! 3D data: Assume central location in vertical dimension!
         ELSEIF ( CurrCont%Dta%SpaceDim == 3 ) THEN
 
            CALL MAPL_AddImportSpec(GC,                  &
               SHORT_NAME = TRIM(CurrCont%Dta%cName),    &
               LONG_NAME  = TRIM(CurrCont%Dta%cName),    &
               UNITS      = TRIM(CurrCont%Dta%OrigUnit), &
               DIMS       = MAPL_DimsHorzVert,           &
               VLOCATION  = MAPL_VLocationCenter,        &
               RC         = STATUS                        )
            VERIFY_(STATUS)

         ! Return w/ error if not 2D or 3D data 
         ELSE
            ASSERT_(.FALSE.) 
         ENDIF

         ! Advance to next container
         CALL GetNextCont ( CurrCont, FLAG ) 

      ENDDO

      ! Free pointer
      CurrCont => NULL()

      ! Return success
      RETURN_(ESMF_SUCCESS)

      END SUBROUTINE HCO_SetServices
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HcoState_SetTime 
!
! !DESCRIPTION: Subroutine HcoState\_SetTime sets all time stamps in HEMCO
! state using time information extracted from the ESMF clock object. 
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE HcoState_SetTime( Clock, HcoState, RC ) 
!
! !USES:
!
    USE HCO_TIME_MOD,     ONLY : HCO_GetWeekday
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock       ! ESMF clock obj 
    TYPE(HCO_State),     POINTER               :: HcoState    ! HEMCO obj
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(  OUT)         :: RC          ! Error code
!
! !REVISION HISTORY:
!  29 Aug 2013 - C. Keller - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Objects
    TYPE(ESMF_Time)                   :: startTime    ! ESMF start time obj
    TYPE(ESMF_Time)                   :: currTime     ! ESMF current time obj

    ! Scalars
    INTEGER(ESMF_KIND_I8)             :: ccount       ! # of clock advances
    INTEGER                           :: doy          ! Day of year (0-365/366)
    INTEGER                           :: weekday      ! 0=Sun,...,6=Sat 
    INTEGER                           :: yyyy, mm, dd ! Year, month, day
    INTEGER                           :: h,    m,  s  ! Hour, minute, seconds
    REAL                              :: utc          ! UTC time

    ! ================================================================
    ! HcoState_SetTime begins here
    ! ================================================================

    ! For MAPL/ESMF error handling (defined Iam and STATUS)
    __Iam__('HcoState_SetTime (HEMCO_GridCompMod.F90)') 

    !=======================================================================
    ! Extract time/date information
    !=======================================================================
    
    ! Get the ESMF time object
    CALL ESMF_ClockGet( Clock,                    &
                        startTime    = startTime, &
                        currTime     = currTime,  &
                        advanceCount = ccount,    &
                         __RC__ )

    ! Get individual fields from the time object
    CALL ESMF_TimeGet( currTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )
    utc     = ( DBLE(h) ) + ( DBLE(m)/60d0 ) + ( DBLE(s)/3600d0 ) 
    weekday = HCO_GetWeekday ( yyyy, mm, dd, utc )

    ! Pass to HEMCO state 
    HcoState%sYear       = yyyy
    HcoState%sMonth      = mm
    HcoState%sDay        = dd
    HcoState%sHour       = h
    HcoState%sMin        = m
    HcoState%sSec        = s
    HcoState%sDayOfYear  = doy
    HcoState%sWeekday    = weekday

    ! Return success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE HcoState_SetTime
!EOC
end module HEMCO_GridCompMod
