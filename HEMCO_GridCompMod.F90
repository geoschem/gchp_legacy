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
  PRIVATE                          :: Initialize_      ! Init method
  PRIVATE                          :: Run_             ! Run method  
  PRIVATE                          :: Finalize_        ! Finalize method
  PRIVATE                          :: HcoState_SetTime ! 
  PRIVATE                          :: HCO_SetServices  ! 
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

  ! HEMCO objects
  TYPE(HCO_State), POINTER      :: HcoState => NULL() ! HEMCO state
  TYPE(OptExt),    POINTER      :: ExtOpt   => NULL() ! Extension options bundle 

  ! HEMCO configuration file:
! CHARACTER(LEN=ESMF_MAXSTR), PARAMETER :: ConfigFile = &
!    '/home/ckeller/GIGC/HEMCO_input/HEMCO_Config.GIGCtest'
  CHARACTER(LEN=ESMF_MAXSTR)    :: ConfigFile     ! HEMCO configuration file 

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

! !DESCRIPTION:  The SetServices for the emissions needs to register its
!   Initialize and Run.
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
   call ESMF_ConfigLoadFile( myState%myCF, 'HEMCO_GridComp.rc', __RC__)
   
   ! Store internal state with Config object in the gridded component
   CALL ESMF_UserCompSetInternalState( GC, 'HEMCO_State', wrap, STATUS )
   VERIFY_(STATUS)
   
    !=======================================================================
    !                    %%% MAPL Data Services %%%
    !=======================================================================
!EOC
!BOP
!
! !IMPORT STATE:
!
    ! Extract HEMCO configuration file path&name (from HEMCO_GridComp.rc)
    CALL ESMF_ConfigGetAttribute( myState%myCF, ConfigFile,     &
                                  Label="CONFIG_FILE:", __RC__ )

    ! Set HEMCO services (reads the HEMCO configuration file and registers
    ! all required emission fields) 
    am_I_Root   = MAPL_Am_I_Root()
    CALL HCO_SetServices( am_I_Root, GC, TRIM(ConfigFile), __RC__ )

    ! Other imports (from other components):
#include "HEMCO_ImportSpec___.h"

!     CALL MAPL_AddImportSpec( GC, &
!          SHORT_NAME          = 'U10M',               &
!          LONG_NAME           = 'sfc_ew_10m_wind',    &
!          UNITS               = '1',                  &
!          DIMS                = MAPL_DimsHorzOnly,    &
!          VLOCATION           = MAPL_VLocationNone,   &
!          __RC__ )
!
!     CALL MAPL_AddImportSpec( GC, &
!          SHORT_NAME          = 'V10M',               &
!          LONG_NAME           = 'sfc_ns_10m_wind',    &
!          UNITS               = '1',                  &
!          DIMS                = MAPL_DimsHorzOnly,    &
!          VLOCATION           = MAPL_VLocationNone,   &
!          __RC__ )

!     ! grid cell area (imported from GIGCchem)
!     CALL MAPL_AddImportSpec( GC, &
!          SHORT_NAME          = 'AREA',               &
!          LONG_NAME           = 'grid_cell_area',     &
!          UNITS               = 'm^2',                &
!          DIMS                = MAPL_DimsHorzOnly,    &
!          VLOCATION           = MAPL_VLocationNone,   &
!          __RC__ )

    ! Import from GIGC 
    call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'TRACERS',                           &
        LONG_NAME          = 'tracer_volume_mixing_ratios',       &
        UNITS              = 'mol/mol',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                          __RC__ )

!    ! pressure edges (from dynamics) 
!    call MAPL_AddImportSpec(GC,                                   &
!        SHORT_NAME         = 'PEDGE',                             &
!        LONG_NAME          = 'pressure_edge',                     &
!        UNITS              = 'hPa',                               &
!        DIMS               = MAPL_DimsHorzVert,                   &
!        VLOCATION          = MAPL_VLocationCenter,                &
!                                                       __RC__     )
!
!    ! pressure center (from dynamics) 
!    call MAPL_AddImportSpec(GC,                                   &
!        SHORT_NAME         = 'PCENTER',                           &
!        LONG_NAME          = 'pressure_center',                   &
!        UNITS              = 'hPa',                               &
!        DIMS               = MAPL_DimsHorzVert,                   &
!        VLOCATION          = MAPL_VLocationCenter,                &
!                                                       __RC__     )
!
!    ! box heights (from dynamics) 
!    call MAPL_AddImportSpec(GC,                                   &
!        SHORT_NAME         = 'BOXHEIGHT',                         &
!        LONG_NAME          = 'boxheight',                         &
!        UNITS              = 'm',                                 &
!        DIMS               = MAPL_DimsHorzVert,                   &
!        VLOCATION          = MAPL_VLocationCenter,                &
!                                                       __RC__     )
!
! !INTERNAL STATE:
!
!uncomment the following line if HEMCO internal state is not empty
!#include "HEMCO_InternalSpec___.h"

!
! !EXTERNAL STATE:
!
!uncomment the following line if HEMCO export state is not empty
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
    USE HCO_TOOLS_MOD,    ONLY : HCO_CharMatch
    USE HCO_DRIVER_MOD,   ONLY : HCO_INIT
    USE HCOX_DRIVER_MOD,  ONLY : HCOX_INIT

    ! testing only
    USE HCO_ARR_MOD,      ONLY : HCO_ValInit

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
!    TYPE(ESMF_Config)            :: ChemCF      ! ESMF Config obj (GIGC*.rc)
    TYPE(ESMF_Config)            :: EmisCF      ! ESMF Config obj (HEMCO*.rc) 
    TYPE(MAPL_METACOMP), POINTER :: MAPL    

    TYPE(ESMF_VM)                 :: VM             ! ESMF VM object
    TYPE(EMIS_State), POINTER     :: myState        ! Legacy state
    TYPE(EMIS_Wrap)               :: wrap           ! Wrapper for myState
                                   
    ! Scalars                                   
    LOGICAL                     :: am_I_Root   ! Are we on the root CPU?
    INTEGER                     :: error       ! HEMCO error code
    INTEGER                     :: myPet       ! # of the CPU we are on 
    INTEGER                     :: nPets       ! Total # of CPUs 
    CHARACTER(LEN=5)            :: petStr      ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
    CHARACTER(LEN=ESMF_MAXSTR)  :: StdOutFile  ! output log file 

    ! TRACERS bundle (import)
    TYPE(ESMF_FieldBundle)       :: trcBUNDLE  ! Tracer field bundle
    TYPE(ESMF_Field      )       :: trcFIELD   ! Tracer field

    ! HEMCO bundle (internal)
    TYPE(ESMF_FieldBundle)       :: hcoBUNDLE
    TYPE(ESMF_Field)             :: hcoFIELD

    ! Grid information
    TYPE(MAPL_MetaComp),  POINTER   :: metaComp    ! MAPL MetaComp object
    INTEGER                         :: locDims(3)  ! Array for local dims
    REAL,                 POINTER   :: lonCtr(:,:)   ! Lon centers on this CPU [rad]
    REAL,                 POINTER   :: latCtr(:,:)   ! Lat centers on this CPU [rad]
    INTEGER                         :: lDE
    INTEGER                         :: localDECount

    ! Working variables
    CHARACTER(LEN=ESMF_MAXSTR)      :: trcNAME
    CHARACTER(LEN=ESMF_MAXSTR)      :: hcoNAME
    CHARACTER(LEN=255)              :: MSG 
    CHARACTER(LEN=31), ALLOCATABLE  :: HcoSpecNames(:), TrcNames(:) 
    INTEGER, ALLOCATABLE            :: matchidx(:)
    INTEGER                         :: nMatch, cnt, AS
    INTEGER                         :: I, J, N, IDX, nTrc, nHco 
    REAL                            :: TCVV
    REAL, POINTER                   :: Arr3D(:,:,:) => NULL()
    LOGICAL                         :: verb

    !=======================================================================
    ! Initialize_ begins here!
    !=======================================================================

    ! Error handling
    __Iam__('Initialize_')

    ! Assume success
    ERROR = HCO_SUCCESS

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, vm=VM, __RC__ )
    Iam = trim( compName ) // '::' // trim( Iam )

    ! Initialize MAPL Generic
    CALL MAPL_GenericInitialize( GC, Import, Export, Clock, __RC__ )

    ! Test if we are on the root CPU
    am_I_Root = MAPL_Am_I_Root()

    ! Get the internal state which holds the private Config object
    CALL ESMF_UserCompGetInternalState( GC, 'HEMCO_State', wrap, STATUS )
    VERIFY_(STATUS)
    myState => wrap%ptr
    EmisCF  =  myState%myCF

    !=======================================================================
    ! Extract model tracer names. These are the tracers used by 
    ! dynamics & chemistry. The HEMCO tracers will be matched against them.
    ! NOTE: The TRACERS bundle becomes initialized in the chemistry comp. 
    !=======================================================================
    call ESMF_StateGet(IMPORT, 'TRACERS', trcBUNDLE, __RC__ )
    call ESMF_FieldBundleGet(trcBUNDLE, fieldCount=nTrc, __RC__ )

    allocate(trcNames(nTrc))
    trcNames(:) = ''
    DO N=1, nTrc
       call ESMF_FieldBundleGet(trcBUNDLE, N, trcFIELD, __RC__ )
       call ESMF_FieldGet( trcFIELD, NAME=trcNAME, __RC__)
       trcNames(N) = TRIM(trcNAME)
    ENDDO

    !=======================================================================
    ! HEMCO initialization calls follow below
    ! Note: the configuration file was already read in SetServices, so no
    ! need to do this again!
    !=======================================================================

    ! Open HEMCO logfile (as specified in config file) 
    IF ( am_I_Root ) THEN
       CALL HCO_LOGFILE_OPEN ( RC=ERROR )
       IF ( ERROR/=HCO_SUCCESS ) THEN
          ASSERT_(.FALSE.)
       ENDIF
    ELSE
       CALL HCO_VERBOSE_SET ( .FALSE. )
    ENDIF
    verb = HCO_VERBOSE_CHECK() .AND. am_I_Root 

    !=======================================================================
    ! Define HEMCO species
    !=======================================================================
    ! Get number of species registered from configuration file
    nHco = Config_GetnSpecies () 

    ! Species names
    ALLOCATE(HcoSpecNames(nHco),STAT=AS)
    ASSERT_(AS==0)
    CALL Config_GetSpecNames( HcoSpecNames, nHco, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF

    ! See how many species are also used in GEOS-Chem
    ALLOCATE(matchIDx(N),STAT=AS)
    ASSERT_(AS==0)
    matchIDx(:) = -1
    CALL HCO_CharMatch( HcoSpecNames, nHco, TrcNames, nTrc, matchIDx, nMatch )
    IF ( nMatch == 0 ) THEN
       PRINT*, 'HcoSpecNames: ', HcoSpecNames
       PRINT*, 'TrcNames    : ', TrcNames
       CALL HCO_ERROR ('No matching species!', ERROR, THISLOC='HCO_INIT')
       ASSERT_(.FALSE.)
    ENDIF

    !=======================================================================
    ! Initialize HEMCO state object 
    !=======================================================================
    CALL HcoState_Init ( am_I_Root, HcoState, nMatch, ERROR )
    IF ( ERROR /= HCO_SUCCESS ) THEN
       ASSERT_(.FALSE.) 
    ENDIF

    !=======================================================================
    ! Set variables in HcoState 
    !=======================================================================

    ! General
    HcoState%isESMF     =  .TRUE. 
    HcoState%IMPORT     => IMPORT
    HcoState%ConfigFile =  ConfigFile

    ! ----------------------------------------------------------------------
    ! Grid
 
    ! Get the ESMF grid attached to this gridded component
    CALL ESMF_GridCompGet( GC, grid=Grid, __RC__ )
    CALL MAPL_GridGet( Grid, localCellCountPerDim = locDims, __RC__ )      

    ! Pass local grid dimensions
    HcoState%NX = locDims(1)
    HcoState%NY = locDims(2)
    HcoState%NZ = locDims(3)

    ! Prepare HcoState grid arrays 
    ALLOCATE ( HcoState%Grid%XMID(HcoState%NX,HcoState%NY), STAT=AS )
    ASSERT_(AS==0)
    ALLOCATE ( HcoState%Grid%YMID(HcoState%NX,HcoState%NY), STAT=AS )
    ASSERT_(AS==0)

    ! Get horizontal coordinate variables and pass to HcoState.
    CALL MAPL_GetObjectFromGC( GC, metaComp,    __RC__ )
    CALL MAPL_Get( metaComp, lons=lonCtr,       __RC__ )
    CALL MAPL_Get( metaComp, lats=latCtr,       __RC__ )

    ! Convert from rad to deg and pass to HcoState
    HcoState%Grid%XMID(:,:) = lonCtr(:,:) * 180d0 / HcoState%Phys%PI
    HcoState%Grid%YMID(:,:) = latCtr(:,:) * 180d0 / HcoState%Phys%PI

    ! NOTE: the grid box area is obtained from GIGCchem through the
    ! IMPORT state. This pointer is set in the run_ subroutine.
    HcoState%Grid%BXHEIGHT_M => NULL()
!    ALLOCATE ( HcoState%Grid%BXHEIGHT_M(HcoState%NX,HcoState%NY,HcoState%NZ), STAT=AS )
!    ASSERT_(AS==0)
!    HcoState%Grid%BXHEIGHT_M(:,:,:) = 0.0d0
 
    ! Grid edge information is only used for HEMCO internal regridding
    ! routines, which are never called in an ESMF environment. Hence
    ! leave pointers nullified.
    HcoState%Grid%XEDGE => NULL() 
    HcoState%Grid%YEDGE => NULL() 
    HcoState%Grid%YSIN  => NULL() 

    ! ----------------------------------------------------------------------
    ! Timesteps
 
    ! Dynamic timestep (get from MAPL.rc)
    CALL ESMF_GridCompGet( GC, Config=MaplCF, __RC__ )
    CALL ESMF_ConfigGetAttribute( MaplCF, HcoState%TS_DYN, &
                                  Label="RUN_DT:", __RC__ )

!    ! Dynamics timestep (get from Chem_GridComp.rc)
!    CALL ESMF_ConfigGetAttribute( ChemCF, tsChem,                      &
!                                     Label="DYNAMICS_TIMESTEP:", __RC__ )

!    ! Chemistry timestep (get from Chem_GridComp.rc)
!    CALL ESMF_ConfigGetAttribute( ChemCF, tsChem,                      &
!                                     Label="CHEMISTRY_TIMESTEP:", __RC__ )

    ! Emission timestep (get from HEMCO_GridComp.rc)
    CALL ESMF_ConfigGetAttribute( EmisCF, HcoState%TS_EMIS,           &
                                  Label="EMISSION_TIMESTEP:", __RC__ )

    ! For now, assume chemistry timestep = emission timestep
    HcoState%TS_CHEM = HcoState%TS_EMIS

    !=======================================================================
    ! Set species information and define HEMCO data bundle
    ! Each field of the bundle corresponds to an array of HcoState and will
    ! be populated with the emissions calculated by HEMCO.
    ! Additional properties will be added in the chemistry component! 
    !=======================================================================

    call ESMF_StateGet(Export, 'EMISSIONS', HcoBUNDLE, __RC__ )

    ! Empty data array to be copied to each field bundle
    ALLOCATE(Arr3D(HcoState%NX,HcoState%NY,HcoState%NZ),STAT=AS)
    ASSERT_(AS==0)
    Arr3D = 0d0

    ! Logfile I/O
    IF ( am_I_Root ) THEN
       MSG = 'HEMCO species:'
       CALL HCO_MSG(MSG)
    ENDIF

    ! Add all used HEMCO species to bundle
    cnt = 0
    DO I = 1, nHco

       ! Skip if this HEMCO species is not used
       IF ( MatchIDx(I) <= 0 ) CYCLE

       ! Increase counter: this is the index in HcoState%Spc!
       cnt = cnt + 1

       ! Set species names in HcoState 
       HcoState%Spc(cnt)%SpcName = HcoSpecNames(I)
       HcoNAME = HcoSpecNames(I)
       N = MatchIDx(I)

       ! Get additional information from TRACERS bundle
       ! and pass to HEMCO state object.
       call ESMF_FieldBundleGet(trcBUNDLE, N, trcFIELD, __RC__ )

       ! tracer ID
       call ESMF_AttributeGet (trcFIELD,     &
            NAME  = 'TRAC_ID',               &
            VALUE = HcoState%Spc(cnt)%ModID, &
                                    __RC__ )       

       ! molecular weight
       call ESMF_AttributeGet (trcFIELD,     &
            NAME  = 'MW_g',                  &
            VALUE = HcoState%Spc(cnt)%MW_g,  &
                                    __RC__ )

       ! --> For now, set emitted MW to species MW! 
       HcoState%Spc(cnt)%EmMW_g = HcoState%Spc(cnt)%MW_g

       ! emission ratio
       call ESMF_AttributeGet (trcFIELD,          &
            NAME  = 'MolecRatio',                 &
            VALUE = HcoState%Spc(cnt)%MolecRatio, &
                                    __RC__ )

       ! TODO: Also fetch Henry law's constants once the new species structure is in place

       ! Now create field in bundle. Pass name and (chemistry) tracer ID. 
       HcoFIELD = ESMF_FieldCreate ( Grid,                      &
                                     Arr3D,                     &
                                     copyflag = ESMF_DATA_COPY, &
                                     name     = HcoNAME,        &
                                                        __RC__ )
       call ESMF_AttributeSet (HcoFIELD,      &
            NAME  = 'TRAC_ID',                &
            VALUE = HcoState%Spc(cnt)%ModID,  &
                                    __RC__ )       
       call ESMF_FieldBundleAdd ( HcoBUNDLE, HcoFIELD, __RC__ )

       ! Logfile I/O 
       IF ( am_I_Root ) THEN
          MSG = 'Species ' // TRIM(HcoState%Spc(cnt)%SpcName)
          CALL HCO_MSG(MSG)
          IF ( verb ) THEN
             write(MSG,*) '--> HcoID         : ', HcoState%Spc(cnt)%HcoID
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> ModID         : ', HcoState%Spc(cnt)%ModID
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> MW (g/mol)    : ', HcoState%Spc(cnt)%MW_g
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> emitted MW    : ', HcoState%Spc(cnt)%EmMW_g
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> Molecule ratio: ', HcoState%Spc(cnt)%MolecRatio
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> Henry constant: ', HcoState%Spc(cnt)%HenryK0
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> Henry temp.   : ', HcoState%Spc(cnt)%HenryCR
             CALL HCO_MSG(MSG)
             write(MSG,*) '--> Henry pKA     : ', HcoState%Spc(cnt)%HenryPKA
             CALL HCO_MSG(MSG)
          ENDIF
       ENDIF
    ENDDO
    IF ( am_I_Root ) CALL HCO_MSG(SEP1='-')

   ! Set bundle attributes
    call ESMF_AttributeSet (HcoBUNDLE,    &
         NAME  = 'N_SPECIES',             &
         VALUE = cnt,                     &
                               __RC__    )

    ! Don't need temporary array anymore
    IF ( ASSOCIATED(Arr3D)        ) DEALLOCATE(Arr3D)
    IF ( ALLOCATED( HcoSpecNames) ) DEALLOCATE( HcoSpecNames )
    IF ( ALLOCATED( TrcNames    ) ) DEALLOCATE( TrcNames     )
    IF ( ALLOCATED( MatchIDx    ) ) DEALLOCATE( MatchIDx     )
       
    ! Verbose mode
    if (verb) then 
       print *, trim(Iam)//': EMISSIONS bundle during Initialization:' 
       call ESMF_FieldBundlePrint ( HcoBUNDLE )
    end if

    !=======================================================================
    ! Initialize HEMCO internal lists and variables 
    !=======================================================================
 
    CALL HCO_INIT ( am_I_Root, HcoState, ERROR ) 
    ASSERT_(ERROR==HCO_SUCCESS)

    ! ----------------------------------------------------------------------
    ! Pass current timestamps to HcoState 
    CALL HcoState_SetTime ( am_I_Root, Clock, HcoState, __RC__ )

    !=======================================================================
    ! Initialize HEMCO extensions 
    !=======================================================================

    ! temp. toggle
    CALL HCOX_INIT ( am_I_Root, HcoState, ExtOpt, ERROR )
    ASSERT_(ERROR==HCO_SUCCESS)

    !=======================================================================
    ! All done
    !=======================================================================

    ! Close HEMCO logfile before leaving
    IF ( am_I_Root ) THEN
       CALL HCO_LOGFILE_CLOSE 
    ENDIF    

    ! Successful return
    RETURN_(ESMF_SUCCESS)

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
    USE HCO_FLUXARR_MOD,    ONLY : HCO_FluxarrReset
    USE HCO_DRIVER_MOD,     ONLY : HCO_RUN
    USE HCOX_DRIVER_MOD,    ONLY : HCOX_RUN

#   include "HEMCO_DeclarePointer___.h"            ! Ptr decls to states
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
    ! Scalars                                     
    LOGICAL                      :: am_I_Root     ! Are we on the root CPU?
    INTEGER                      :: error         ! G-C error return code
    CHARACTER(LEN=ESMF_MAXSTR)   :: compName      ! Gridded Component name

    ! HEMCO bundle
    TYPE(ESMF_FieldBundle)       :: hcoBUNDLE
    REAL, POINTER                :: fPtrArray(:,:,:) => NULL()
    INTEGER                      :: I, L, LE, N

    !=======================================================================
    ! Run_ begins here!
    !=======================================================================
    
    __Iam__('Run_')

    ! Assume success
    error = HCO_SUCCESS

    ! Are we on the root CPU?
    am_I_Root = MAPL_Am_I_Root()

    ! Open logfile (as specified in config file) 
    IF ( am_I_Root ) THEN
       CALL HCO_LOGFILE_OPEN ( RC=ERROR )
       ASSERT_(ERROR==HCO_SUCCESS)
    ENDIF

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    Iam = TRIM( compName ) // '::' // TRIM( Iam )

    !=======================================================================
    ! pre-Run method array assignments
    !=======================================================================

    ! # of vertical levels
    L  = HcoState%NZ   ! center
    LE = L+1           ! edges

    ! Connect HEMCO state object with HEMCO bundle
    call ESMF_StateGet(Export, 'EMISSIONS', hcoBUNDLE, __RC__ )
    call ESMF_FieldBundleGet( hcoBUNDLE, fieldCount=N, __RC__ )
    DO I = 1, N
       call ESMFL_BundleGetPointerToData( hcoBUNDLE, I, fPtrArray, __RC__ )  
       HcoState%Spc(I)%Emis%Val => fPtrArray(:,:,L:1:-1) 
       fPtrArray => NULL() 
    ENDDO

    ! Get pointers to fields in import, internal, and export states
    ! This sets the pointers to all met fields variables needed by HEMCO
#   include "HEMCO_GetPointer___.h"

    ! Set pointers in ExtOpt 
#   include "HEMCO_Includes_BeforeRun.H"

    !=======================================================================
    ! Reset all emission and deposition values in HcoState
    !=======================================================================
    CALL HCO_FluxarrReset ( HcoState, ERROR )
    ASSERT_(ERROR==HCO_SUCCESS)
   
    !=======================================================================
    ! Set HEMCO calculation options 
    !=======================================================================

    ! Range of tracers and emission categories.
    ! Set Extension number ExtNr to 0, indicating that the core
    ! module shall be executed. 
    HcoState%Options%SpcMin =  1
    HcoState%Options%SpcMax = -1 
    HcoState%Options%CatMin =  1
    HcoState%Options%CatMax = -1
    HcoState%Options%ExtNr  =  0

    ! Use temporary array?
    HcoState%Options%FillBuffer = .FALSE.

    ! Set current datetime
    CALL HcoState_SetTime ( am_I_Root, Clock, HcoState, __RC__ )

    !=======================================================================
    ! Run HEMCO and write emissions into HcoState 
    !=======================================================================
    CALL HCO_RUN ( am_I_Root, HcoState, ERROR )
    ASSERT_(ERROR==HCO_SUCCESS)

    !=======================================================================
    ! Run HEMCO extensions. Emissions are added to HcoState 
    !=======================================================================
    CALL HCOX_RUN ( am_I_Root, HcoState, ExtOpt, ERROR )
    ASSERT_(ERROR==HCO_SUCCESS)

    !=======================================================================
    ! post-Run method array assignments
    !=======================================================================

#   include "HEMCO_Includes_AfterRun.H"

    ! Disconnect HEMCO state object with HEMCO bundle
    DO I = 1, N
       HcoState%Spc(I)%Emis%Val => NULL() 
    ENDDO
 
    !=======================================================================
    ! All done
    !=======================================================================

    ! Close HEMCO logfile before leaving
    IF ( am_I_Root ) THEN
       CALL HCO_LOGFILE_CLOSE
    ENDIF    

    ! Successful return
    RETURN_(ESMF_SUCCESS)

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
  USE HCO_DRIVER_MOD,    ONLY : HCO_FINAL
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
    ! Cleanup extensions and ExtOpt object 
    CALL HCOX_FINAL ( HcoState, ExtOpt )

    ! Cleanup HCO core
    CALL HCO_FINAL

    ! Make sure HcoState variables pointing to shared data are 
    ! nullified (otherwise, the target arrays become deallocated)
    HcoState%Grid%AREA_M2    => NULL()
    HcoState%Grid%BXHEIGHT_M => NULL()

    ! Cleanup HcoState object
    CALL HcoState_Final ( HcoState ) 

  end subroutine Finalize_
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
! This routine is called at the beginning of a simulation - ahead of 
! the initialization routines. Since this routine is called from outside of 
! the HEMCO environment, use the MAPL specific error codes!
! This routine determines all required HEMCO input fields from the HEMCO 
! configuration file. Note that each file needs an equivalent ESMF-style
! entry in the registry file (typically ExtData.rc). Otherwise, ESMF won't 
! read these files and HEMCO will fail when attempting to get pointers to 
! the data arrays.
! It is important to note that the field names provided in ExtData.rc must
! match the names in the HEMCO configuration file! Also, all time settings
! (average and update interval) and data units need to be properly specified
! in ExtData.rc.
!
! !TODO: For now, ExtData.rc and HEMCO configuration file have to be 
! synchronized manually. Need to write a script to automate this process!
!
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
      ASSERT_(STATUS==HCO_SUCCESS)

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

         ! Add arrays to import spec. Distinguish between 2D and 3D arrays.
         ! Note that we can ignore the time reading interval here, as this
         ! is automatically determined by ESMF based upon the registry file
         ! content!.

         ! Import 2D data
         ELSEIF ( CurrCont%Dta%SpaceDim == 2 ) THEN

            CALL MAPL_AddImportSpec(GC,                  &
               SHORT_NAME = TRIM(CurrCont%Dta%cName),    &
               LONG_NAME  = TRIM(CurrCont%Dta%cName),    &
               UNITS      = TRIM(CurrCont%Dta%OrigUnit), &
               DIMS       = MAPL_DimsHorzOnly,           &
               VLOCATION  = MAPL_VLocationNone,          &
               RC         = STATUS                        )
            VERIFY_(STATUS)

         ! Import 3D data: Assume central location in vertical dimension!
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
    SUBROUTINE HcoState_SetTime( am_I_Root, Clock, HcoState, RC ) 
!
! !USES:
!
    USE HCO_TIME_MOD,     ONLY : HcoClock_Set
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
    LOGICAL,             INTENT(IN   )         :: am_I_Root   ! Root CPU?
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
    INTEGER                           :: error

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

    ! Set time (HEMCO clock object) 
    CALL HcoClock_Set( am_I_Root, HcoState, yyyy, mm, dd, h, m, s, doy, error )
    ASSERT_(ERROR==HCO_SUCCESS)

    ! Return success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE HcoState_SetTime
!EOC
end module HEMCO_GridCompMod
