#include "MAPL_Generic.h"

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: Chem_GridCompMod
!
! !DESCRIPTION: Chem_GridComp is an ESMF gridded component 
! implementing the GEOS-Chem chemistry and related processes, including
! dry deposition, emissions, and wet deposition. In addition, the
! parameterizations for PBL mixing and convection as used in GEOS-Chem
! can be invoked by enabling the corresponding option in the GEOS-Chem
! input file (input.geos.RC). In this case, the corresponding GEOS-5
! process must NOT be applied to the GC tracers, i.e. the tracers must 
! not be friendly to turbulence (if PBL mixing is used) and/or moist
! (for convection).
!\\
!\\
! This gridded component contains three run phases: 
!
!  -1: Phase -1 is the standard setting in GCHP. It executes all components. 
!      Phase is -1 if number of phases is set to 1 in config file GCHP.rc.
!
!   1: Phase 1 is used in GEOS-5. It executes convection, dry deposition,
!      and emissions and should be called before surface processes/turbulence. 
! 
!   2: Phase 2 is used in GEOS-5. It performs chemistry, and wet deposition, 
!      and should be called after turbulence.
!\\
!\\
! !INTERFACE:
!
MODULE Chem_GridCompMod
!
! !USES:
!
  USE CMN_Size_Mod
  USE ESMF                                           ! ESMF library
  USE MAPL_Mod                                       ! MAPL library
  USE Charpak_Mod                                    ! String functions
  USE Hco_Types_Mod, ONLY : ConfigObj
  USE Input_Opt_Mod                                  ! Input Options obj
  USE GIGC_Chunk_Mod                                 ! GIGC IRF methods
  USE GIGC_HistoryExports_Mod
  USE GIGC_ProviderServices_Mod
  USE ErrCode_Mod                                    ! Error numbers
  USE State_Chm_Mod                                  ! Chemistry State obj
  USE State_Diag_Mod                                 ! Diagnostics State obj
  USE State_Met_Mod                                  ! Meteorology State obj
  USE Species_Mod,   ONLY : Species
  USE Time_Mod,      ONLY : ITS_A_NEW_DAY, ITS_A_NEW_MONTH

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC   :: SetServices    ! Sets ESMF entry points
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE  :: Initialize_    ! Init method
  PRIVATE  :: Run1           ! Run wrapper phase 1
  PRIVATE  :: Run2           ! Run wrapper phase 2 or -1
  PRIVATE  :: Run_           ! Run method
  PRIVATE  :: Finalize_      ! Finalize method
  PRIVATE  :: Extract_       ! Get values from ESMF
!
! !PRIVATE TYPES:
!
  ! Legacy state
  TYPE GEOSCHEM_State
     PRIVATE
     TYPE(ESMF_Config)             :: myCF           ! Private ESMF Config obj
  END TYPE GEOSCHEM_State

  ! Hook for the ESMF
  TYPE GEOSCHEM_Wrap
     TYPE(GEOSCHEM_State), POINTER :: PTR => null()  ! Ptr to GEOSCHEM_State
  END TYPE GEOSCHEM_Wrap

  ! For passing from internal state to Chm_State and vice versa
  ! SDE 2016-03-28: Assume that Int2Chm is held as v/v dry
  TYPE Int2ChmMap
     CHARACTER(LEN=255)            :: TrcName
     INTEGER                       :: TrcID
     REAL                          :: TCVV
     REAL(ESMF_KIND_R8), POINTER   :: Internal(:,:,:) => NULL()
  END TYPE Int2ChmMap

  ! For mapping internal state
  TYPE(Int2ChmMap), POINTER        :: Int2Chm(:) => NULL()
  ! Objects for GEOS-Chem
  TYPE(OptInput)                   :: Input_Opt      ! Input Options
  TYPE(MetState)                   :: State_Met      ! Meteorology state
  TYPE(ChmState)                   :: State_Chm      ! Chemistry state
  TYPE(DgnState)                   :: State_Diag     ! Diagnostics state
  TYPE(Species),          POINTER  :: ThisSpc => NULL()
  TYPE(ConfigObj),        POINTER  :: HcoConfig
  TYPE(HistoryConfigObj), POINTER  :: HistoryConfig

  ! Scalars
  INTEGER                          :: logLun     ! LUN for stdout logfile
  CHARACTER(LEN=ESMF_MAXSTR)       :: logFile    ! File for stdout redirect
  LOGICAL                          :: isProvider ! provider to AERO, RATS, ANOX?
  LOGICAL                          :: calcOzone  ! if PTR_GCCTO3 is associated

  ! Is this being run as a CTM?
  INTEGER                          :: IsCTM

  ! Number of run phases, 1 or 2. Set in the rc file; else default is 2.
  INTEGER                          :: NPHASE

  ! Pointers to import, export and internal state data. Declare them as 
  ! module variables so that we have to assign them only on first call.
  !
  ! NOTE: Any provider-related exports (e.g. H2O_TEND) are now handled within
  ! gigc_providerservices_mod.F90. Pointers are manually declared there and
  ! those declared in the .h file included below are not used. (ewl, 11/3/2017)
# include "GIGCchem_DeclarePointer___.h"

  ! Use archived convection fields?
  ! If the attribute 'ARCHIVED_CONV' in the GEOS-Chem configuration file is set
  ! to '1', GEOS-Chem will use archived convection fields, imported through
  ! ExtData (ExtData must contain an entry for each of the pointers defined
  ! below). These data fields are then passed to the GEOS-Chem meteorlogical 
  ! state instead of the (instantaneous) fields imported from MOIST. The 
  ! fields imported through ExtData must be named 'ARCHIVED_PFI_CN', 
  ! 'ARCHIVED_PFL_CN', etc.
  LOGICAL           :: ArchivedConv
  REAL, POINTER     :: PTR_ARCHIVED_PFI_CN (:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_PFL_CN (:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_CNV_MFC(:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_CNV_MFD(:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_CNV_CVW(:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_DQRC   (:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_REV_CN (:,:,:) => NULL()
  REAL, POINTER     :: PTR_ARCHIVED_T      (:,:,:) => NULL()

  ! MPI communicator
  INTEGER, SAVE     :: mpiCOMM
!
! !REMARKS:
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!                                                                             .
!  NOTE: The abbreviation "PET" stands for "Persistent Execution Thread".
!  It is a synomym for CPU.
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  10 Oct 2012 - R. Yantosca - Now references GC_Utils.F90
!  10 Oct 2012 - R. Yantosca - Updated for GEOS-Chem v9-01-03
!  16 Oct 2012 - R. Yantosca - Rename GC_MET object to State_Met
!  16 Oct 2012 - R. Yantosca - Rename GC_STATE object to State_Chm
!  17 Oct 2012 - R. Yantosca - Removed some old "column code" stuff
!  22 Oct 2012 - R. Yantosca - Now references renamed gigc_* modules
!  01 Nov 2012 - R. Yantosca - Now references gigc_input_opt_mod.F90
!  07 Nov 2012 - R. Yantosca - Removed Setup_GeoLoc_ routine
!  07 Nov 2012 - R. Yantosca - Now read placeholder values for input.geos
!  08 Nov 2012 - R. Yantosca - Now initialize Input_Opt%MAX_DIAG field
!  15 Mar 2013 - R. Yantosca - Remove IDENT object and Error_Trap routine
!  25 Mar 2014 - E. Nielsen  - ESMF-5
!  22 Sep 2014 - C. Keller   - Added two run phases
!  17 Oct 2014 - C. Keller   - Various updates to fill provider fields.
!  26 Nov 2014 - C. Keller   - Added H2O_HIST and O3_HIST. 
!  22 Feb 2015 - C. Keller   - Now check if geoschemchem_import_rst exist
!  06 Jun 2016 - M. Yannetti - Added Get_Transport.
!  19 Dec 2016 - M. Long     - Update for v11-01k
!  01 Sep 2017 - E. Lundgren - Enable automation of GCHP diagnostics
!  19 Sep 2017 - E. Lundgren - Remove Get_Transport
!  02 Nov 2017 - E. Lundgren - Remove unused private functions roundoff, 
!                              globalsum, and print_mean_oh
!  06 Nov 2017 - E. Lundgren - Abstract provider services to new module
!                              gigc_providerservices_mod.F90
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices
!
! !DESCRIPTION: The SetServices routine does the following:
!
! \begin{itemize}
! \item Defines the Initialize method for the GEOSCHEMchem gridded component
! \item Defines the Run methods for the GEOSCHEMchem gridded component
! \item Defines the Finalize method for the GEOSCHEMchem gridded component
! \item Attaches an internal state (which holds a private ESMF Config object)
!       to the GEOSCHEMchem gridded component.
! \end{itemize}
!
! !INTERFACE:
!
  SUBROUTINE SetServices( GC, RC )
!
! !USES:
!
    USE HCOI_ESMF_MOD,   ONLY : HCO_SetServices
    USE GCKPP_Model
    USE CHARPAK_MOD,     ONLY : STRSPLIT
    USE INQUIREMOD,      ONLY : findFreeLUN
    USE FILE_MOD,        ONLY : IOERROR
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure
!
! !REMARKS:
!  ESMF can only attach one Config object per Gridded Component.  The 
!  Config object that is defined from the "MAPL.rc" resource file is 
!  directly attached to the GEOSCHEMchem gridded component.  
!                                                                             .
!  To attach the Config object defined from the "GEOSCHEMchem_GridComp.rc" 
!  resource file, we must first create a derived type with a pointer to
!  the Config object, then attach that to the gridded component as an
!  "internal state" (also called "legacy state").
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  07 Apr 2010 - R. Yantosca - Updated comments, cosmetic changes 
!  22 Sep 2014 - C. Keller   - Added two run phases
!  07 Aug 2017 - E. Lundgren - Add Olson and CHRL imports
!  14 Jul 2017 - E. Lundgren - Read simulation type to determine whether to
!                              add KPP species to the internal state
!  01 Sep 2017 - E. Lundgren - Call new subroutine HistoryExports_SetServices
!                              for GEOS-Chem state object diagnostics
!  12 Sep 2017 - E. Lundgren - Use species prefix "SPFX" from gigc_types_mod.F90
!  06 Nov 2017 - E. Lundgren - Abstract provider services to new module
!                              gigc_providerservices_mod.F90
!  08 Mar 2018 - E. Lundgren - "SPFX" now retrieved from gigc_historyexports_mod
!EOP
!------------------------------------------------------------------------------
!BOC
! 
! !LOCAL VARIABLES:
!
    TYPE(ESMF_CONFIG)             :: CF
    TYPE(MAPL_MetaComp),  POINTER :: STATE => NULL()
    TYPE(GEOSCHEM_State), POINTER :: myState       ! Legacy state
    TYPE(GEOSCHEM_Wrap)           :: wrap          ! Wrapper for myState
    CHARACTER(LEN=ESMF_MAXSTR)    :: compName      ! Gridded Component name
    CHARACTER(LEN=ESMF_MAXSTR)    :: COMP_NAME     ! This syntax for mapl_acg.pl
    CHARACTER(LEN=ESMF_MAXSTR)    :: HcoConfigFile ! HEMCO configuration file
    CHARACTER(LEN=ESMF_MAXSTR)    :: HistoryConfigFile ! HISTORY config file
    CHARACTER(LEN=ESMF_MAXSTR)    :: SpcName       ! Registered species name
    CHARACTER(LEN=40)             :: AdvSpc(500)
    CHARACTER(LEN=255)            :: LINE, MSG, SUBSTRS(500)
    INTEGER                       :: N, I, J, T, IU_GEOS, IOS
    INTEGER                       :: NAdv, SimType, landTypeInt
    LOGICAL                       :: FOUND = .false.
    LOGICAL                       :: EOF
    CHARACTER(LEN=60)             :: rstFile, landTypeStr, importName
    INTEGER                       :: restartAttr

    __Iam__('SetServices')

    !=======================================================================
    ! Set services begins here 
    !=======================================================================

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! NOTE: We need to use COMP_NAME for mapl_acg.pl script
    COMP_NAME = TRIM( compName )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::SetServices'
    
    !=======================================================================
    ! Wrap internal state for storing in this gridded component
    ! Rename this to a "legacy state"
    !=======================================================================
    ALLOCATE( myState, stat=STATUS )
    VERIFY_(STATUS)
    wrap%ptr => myState

    !=======================================================================
    ! Define an ESMF Config object from the Resource file and set it 
    ! as an "internal state" of the GEOSCHEMchem gridded component
    !=======================================================================
    myState%myCF = ESMF_ConfigCreate(__RC__)
    call ESMF_ConfigLoadFile( myState%myCF, 'GCHP.rc', __RC__)

    ! Get generic state object
    CALL MAPL_GetObjectFromGC( GC, STATE, __RC__ )
    call MAPL_GetResource( STATE, IsCTM, label='GEOSChem_CTM:', & 
                           default=1, rc=status )
    VERIFY_(STATUS)

    !=======================================================================
    !                 %%% ESMF Functional Services %%%
    !=======================================================================

    ! Set the Initialize, Run, Finalize entry points
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_INITIALIZE,  &
                                     Initialize_, __RC__ )
    !IF (.not. IsCTM) &
!    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_RUN, Run1, __RC__ )
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_RUN, Run2, __RC__ )
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_FINALIZE,  &
                                     Finalize_, __RC__ )
        
    ! Store internal state with Config object in the gridded component
    CALL ESMF_UserCompSetInternalState( GC, 'GEOSCHEM_State', wrap, STATUS )
    VERIFY_(STATUS)

    !=======================================================================
    !                    %%% MAPL Data Services %%%
    !=======================================================================
!EOC
!BOP
!
! !IMPORT STATE:
!
#   include "GIGCchem_ImportSpec___.h"

    call MAPL_AddImportSpec(GC, &
       SHORT_NAME         = 'PLE',  &
       LONG_NAME          = 'pressure_level_edges',  &
       UNITS              = 'Pa', &
       PRECISION          = ESMF_KIND_R8, &
       DIMS               = MAPL_DimsHorzVert,    &
       VLOCATION          = MAPL_VLocationEdge,    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC, &
       SHORT_NAME         = 'DryPLE',  &
       LONG_NAME          = 'dry_pressure_level_edges',  &
       UNITS              = 'Pa', &
       PRECISION          = ESMF_KIND_R8, &
       DIMS               = MAPL_DimsHorzVert,    &
       VLOCATION          = MAPL_VLocationEdge,    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

!
! !INTERNAL STATE:
!
#   include "GIGCchem_InternalSpec___.h"

    ! Determine if using a restart file for the internal state. Setting
    ! the GIGCchem_INTERNAL_RESTART_FILE to +none in GCHP.rc indicates
    ! skipping the restart file. Species concentrations will be retrieved
    ! from the species database, overwriting MAPL-assigned default values.
    CALL ESMF_ConfigGetAttribute( myState%myCF, rstFile, &
                                  Label = "GIGCchem_INTERNAL_RESTART_FILE:",&
                                  __RC__ ) 
    IF ( TRIM(rstFile) == '+none' ) THEN
       restartAttr = MAPL_RestartSkipInitial ! file does not exist;
                                             ! use background values
    ELSE
       restartAttr = MAPL_RestartOptional    ! try to read species from file;
                                             ! use background vals if not found
    ENDIF

!-- Read in species from input.geos and set FRIENDLYTO
    ! ewl TODO: This works but is not ideal. Look into how to remove it.

    ! Open input.geos and read a lines until hit advected species menu
    IU_GEOS = findFreeLun()
    OPEN( IU_GEOS, FILE='input.geos', STATUS='OLD', IOSTAT=IOS )
    IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, 'READ_SPECIES_FROM_FILE:1' )
    DO
      READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE
      IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, 'READ_SPECIES_FROM_FILE:2' )
      IF ( INDEX( LINE, 'ADVECTED SPECIES MENU' ) > 0 ) EXIT
    ENDDO

    ! Read in all advected species names and add them to internal state
    NADV=0
    DO WHILE( INDEX( LINE, 'TRANSPORT MENU' ) .le. 0) 
       READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE
       EOF = IOS < 0
       IF ( EOF ) RETURN
       CALL STRSPLIT( LINE(26:), ' ', SUBSTRS, N )
       IF ( INDEX( LINE, 'Species name' ) > 0 ) THEN
          call MAPL_AddInternalSpec(GC, &
              SHORT_NAME         = TRIM(SPFX) // TRIM(SUBSTRS(1)),  &
              LONG_NAME          = TRIM(SUBSTRS(1)),  &
              UNITS              = 'mol mol-1', &
              DIMS               = MAPL_DimsHorzVert,    &
              VLOCATION          = MAPL_VLocationCenter,    &
              PRECISION          = ESMF_KIND_R8, &
              FRIENDLYTO         = 'DYNAMICS:TURBULENCE:MOIST',  &
              RESTART            = restartAttr, &
              RC                 = RC  )
         NADV = NADV+1
         AdvSpc(NADV) = TRIM(SUBSTRS(1))
       ELSEIF ( INDEX( LINE, 'Type of simulation' ) > 0 ) THEN
          ! Read and store simulation type
          READ( SUBSTRS(1:N), * ) SimType
       ENDIF
    ENDDO
    CLOSE( IU_GEOS )
!
!    CALL READ_SPECIES_FROM_FILE( GC, MAPL_am_I_Root(), restartAttr, &
!                                 AdvSpc, Nadv, SimType, RC )

!-- Add all additional species in KPP (careful not to add dummy species)
!-- only if fullchem or aerosol simulations
    IF ( SimType == 3 .OR. SimType == 10 ) THEN
       DO I=1,NSPEC
          FOUND = .false.
       
          ! Skip dummy RR species for prod/loss diagnostic (mps, 8/23/16)
          SpcName = ADJUSTL( Spc_Names(I) )
          IF ( SpcName(1:2) == 'RR' ) CYCLE
       
          DO J=1,Nadv !Size of AdvSpc
             IF (trim(AdvSpc(J)) .eq. trim(SpcName)) FOUND = .true.
          ENDDO
          
          IF (Found .neqv. .true.) Then
          call MAPL_AddInternalSpec(GC, &
               SHORT_NAME         = TRIM(SPFX) // SpcName,  &
               LONG_NAME          = SpcName,  &
               UNITS              = 'mol mol-1', &
               PRECISION          = ESMF_KIND_R8, &
               DIMS               = MAPL_DimsHorzVert,    &
               VLOCATION          = MAPL_VLocationCenter,    &
               RESTART            = restartAttr,    &
               RC                 = STATUS  )
          Endif
       ENDDO
    ENDIF


!
! !EXTERNAL STATE:
!
#   include "GIGCchem_ExportSpec___.h"

    ! Read HISTORY config file and add exports for unique items
    CALL ESMF_ConfigGetAttribute( myState%myCF, HistoryConfigFile, &
                                  Label="HISTORY_CONFIG:",         &
                                  Default="HISTORY.rc", __RC__ )
    CALL HistoryExports_SetServices( MAPL_am_I_Root(), HistoryConfigFile, &
                                     GC, HistoryConfig, RC=STATUS )
    VERIFY_(STATUS)

    ! Is this needed anymore?
    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'TRACERS',                           &
        LONG_NAME          = 'tracer_volume_mixing_ratios',       &
        UNITS              = 'mol/mol',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                       RC=STATUS  )
!EOP
!BOC

    !=======================================================================
    ! Add provider services, if any (AERO, RATS, Analysis Ox)
    !=======================================================================
    CALL Provider_SetServices( MAPL_am_I_Root(), GC, isProvider, __RC__ )

    !=======================================================================
    !              %%% Test for archived convection fields %%%
    !=======================================================================
    CALL ESMF_ConfigGetAttribute( myState%myCF, I, &
            Label="ARCHIVED_CONV:", Default=0, __RC__ )
    ArchivedConv = ( I == 1 )

    ! Need to add archived convection fields to import state
    IF ( ArchivedConv ) THEN
       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_PFI_CN',                   &
          LONG_NAME          = 'archived_PFI_CN',                   &
          UNITS              = 'kg m-2 s-1',                        &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationEdge,                  &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_PFL_CN',                   &
          LONG_NAME          = 'archived_PFL_CN',                   &
          UNITS              = 'kg m-2 s-1',                        &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationEdge,                  &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_CNV_MFC',                  &
          LONG_NAME          = 'archived_CNV_MFC',                  &
          UNITS              = 'kg m-2 s-1',                        &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationEdge,                  &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_CNV_MFD',                  &
          LONG_NAME          = 'archived_CNV_MFD',                  &
          UNITS              = 'kg m-2 s-1',                        &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_CNV_CVW',                  &
          LONG_NAME          = 'archived_CNV_CVW',                  &
          UNITS              = 'hPa s-1',                           &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_DQRC',                     &
          LONG_NAME          = 'archived_DQRC',                     &
          UNITS              = 'kg kg-1 s-1',                       &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_REV_CN',                   &
          LONG_NAME          = 'archived_REV_CN',                   &
          UNITS              = 'kg kg-1 s-1',                       &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )

       call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'ARCHIVED_T',                        &
          LONG_NAME          = 'archived_T',                        &
          UNITS              = 'K',                                 &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
    ENDIF ! ArchivedConv 

    ! OLSON
    DO T = 1, NSURFTYPE
       landTypeInt = T-1
       WRITE ( landTypeStr, '(I2.2)' ) landTypeInt
       importName = 'OLSON' // TRIM(landTypeStr)
       CALL MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = importName,                          &
          LONG_NAME          = 'OLSON_land_by_type',                &
          UNITS              = 'unitless',                          &
          DIMS               = MAPL_DimsHorzOnly,                   &
                                                            __RC__ )
    ENDDO

    ! LAI
    CALL MAPL_AddImportSpec(GC,                                  &
       SHORT_NAME         = 'XLAIMULTI',                         &
       LONG_NAME          = 'LAI_by_type',                       &
       UNITS              = 'cm2 cm-2',                          &
       DIMS               = MAPL_DimsHorzVert,                   &
       VLOCATION          = MAPL_VLocationEdge,                  &
                                                            __RC__ )

    ! CHLR (chlorophyll-a, used in marine POA simulation only)
    !CALL MAPL_AddImportSpec(GC,                                  &
    !   SHORT_NAME         = 'XCHLRMULTI',                        &
    !   LONG_NAME          = 'CHLR_by_type',                      &
    !   UNITS              = 'mg m-3',                            &
    !   DIMS               = MAPL_DimsHorzVert,                   &
    !   VLOCATION          = MAPL_VLocationEdge,                  &
    !                                                        __RC__ )

    ! Set HEMCO services
    ! --------------------
    CALL ESMF_ConfigGetAttribute( myState%myCF, HcoConfigFile, &
                                  Label="HEMCO_CONFIG:", &
                                  Default="HEMCO_Config.rc", __RC__ )
    CALL HCO_SetServices( MAPL_am_I_Root(), GC, HcoConfig,  &
                          TRIM(HcoConfigFile), __RC__ )    

    ! Set the Profiling timers
    ! ------------------------
    CALL MAPL_TimerAdd(GC, NAME="INITIALIZE", RC=status)
    VERIFY_(status)
    CALL MAPL_TimerAdd(GC, NAME="RUN", RC=status)
    VERIFY_(status)
    CALL MAPL_TimerAdd(GC, NAME="FINALIZE", RC=status)
    VERIFY_(status)

    CALL MAPL_TimerAdd(GC, NAME="DO_CHEM", RC=status)
    VERIFY_(status)
    CALL MAPL_TimerAdd(GC, NAME="CP_BFRE", RC=status)
    VERIFY_(status)
    CALL MAPL_TimerAdd(GC, NAME="CP_AFTR", RC=status)
    VERIFY_(status)

    ! More timers to be called in gigc_chunk_run 
    CALL MAPL_TimerAdd(GC, NAME="GC_CONV"  , __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_EMIS"  , __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_DRYDEP", __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_FLUXES", __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_TURB"  , __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_CHEM"  , __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_WETDEP", __RC__)
    CALL MAPL_TimerAdd(GC, NAME="GC_DIAGN" , __RC__)

    ! Generic Set Services
    ! --------------------
    CALL MAPL_GenericSetServices( GC, RC=status )
    VERIFY_(status)

    NULLIFY(STATE)

    !=======================================================================
    ! All done
    !=======================================================================
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE SetServices
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize_ 
!
! !DESCRIPTION: Initialize_ is the initialize method of the GEOSCHEMchem
!  gridded component.  This is a simple ESMF/MAPL wrapper which calls down
!  to the Initialize method of the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Initialize_( GC, Import, Export, Clock, RC )
!
! !USES:
!
    USE TIME_MOD,  ONLY : GET_TS_CHEM, GET_TS_EMIS
    USE TIME_MOD,  ONLY : GET_TS_DYN,  GET_TS_CONV
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC       ! Ref to GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import   ! Import State object
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export   ! Export State object
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock    ! ESMF clock object
!                                                  
! !OUTPUT PARAMETERS:                              
!                                                  
    INTEGER,             INTENT(OUT)           :: RC       ! Success or failure?
!
! !REMARKS:
!  We call routine Extract_ to return various values (i.e. grid parameters,
!  start & end dates, PET information, etc.) from the ESMF/MAPL environment.  
!  We then pass those to GEOS-Chem via routine GIGC_CHUNK_INIT, which is
!  located in GEOS-Chem module ./GEOS-Chem/ESMF/gigc_chunk_mod.F90.
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now uses the updated Extract_ method.
!  14 Apr 2010 - R. Yantosca - Activated call to GC_CHUNK_INIT
!  15 Apr 2010 - R. Yantosca - Add extra error checks for dimensions
!  23 Apr 2010 - R. Yantosca - Now pass IDENT obj to GC_CHUNK_INIT routine
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now set Ident%VERBOSE to FALSE
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root PET
!  08 Nov 2012 - R. Yantosca - Now pass options to G-C via Input_Opt object
!  04 Dec 2012 - R. Yantosca - Now get local PET grid indices as well as the
!                              global grid indices from the Extract_ function
!  26 Feb 2013 - R. Yantosca - Now read Input_Opt%MAX_DEP from the rc file
!  08 Mar 2013 - R. Yantosca - Now save the PET # (aka PET #) in Input_Opt
!  15 Mar 2013 - R. Yantosca - Remove IDENT object, which was a holdover from
!                              the GEOS-Chem column code
!  13 Oct 2014 - C. Keller   - Updated for HEMCO
!  24 Oct 2014 - C. Keller   - Updated for RATS/AERO/Analysis OX provider
!  23 Feb 2015 - C. Keller   - Now use local variable haveImpRst
!  06 Nov 2017 - E. Lundgren - Abstract provider services to new module
!                              gigc_providerservices_mod.F90
!  06 Mar 2018 - E. Lundgren - Remove obsolete variables; update usage of GC
!                              timesteps since now in seconds not minutes
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Objects
    TYPE(ESMF_Grid)             :: Grid        ! ESMF Grid object
    TYPE(ESMF_Config)           :: MaplCF      ! ESMF Config obj (MAPL.rc)
    TYPE(ESMF_Config)           :: GeosCF      ! ESMF Config obj (GEOSCHEM*.rc) 
                                       
    ! Scalars                                   
    LOGICAL                     :: am_I_Root   ! Are we on the root PET?
    INTEGER                     :: N, trcID
    INTEGER                     :: myPet       ! # of the PET we are on 
    INTEGER                     :: NPES        ! # of total PETs in MPI world
    INTEGER                     :: nymdB       ! GMT date @ start of simulation
    INTEGER                     :: nymdE       ! GMT date @ end of simulation
    INTEGER                     :: nymd        ! GMT date @ current time
    INTEGER                     :: nhmsB       ! GMT time @ start of simulation
    INTEGER                     :: nhmsE       ! GMT time @ end of simulation
    INTEGER                     :: nhms        ! GMT time @ current time
    INTEGER                     :: I_LO        ! Min lon index on this PET
    INTEGER                     :: J_LO        ! Min lat index on this PET
    INTEGER                     :: I_HI        ! Max lon index on this PET
    INTEGER                     :: J_HI        ! Max lat index on this PET
    INTEGER                     :: IM          ! # of longitudes on this PET
    INTEGER                     :: JM          ! # of latitudes  on this PET
    INTEGER                     :: LM          ! # of levels     on this PET
    INTEGER                     :: IM_WORLD    ! # of longitudes in global grid
    INTEGER                     :: JM_WORLD    ! # of latitudes  in global grid
    INTEGER                     :: LM_WORLD    ! # of levels     in global grid
    REAL                        :: tsChem      ! Chemistry timestep [s]
    REAL                        :: tsDyn       ! Dynamic timestep [s]
    CHARACTER(LEN=5)            :: petStr      ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
    
    ! time step error checks 
    REAL                         :: ChemTS, EmisTS
    type(ESMF_Time)              :: CurrTime    ! Current time of the ESMF clock
 
    ! Pointer arrays
    REAL(ESMF_KIND_R4),  POINTER :: lonCtr(:,:) ! Lon centers on this PET [rad]
    REAL(ESMF_KIND_R4),  POINTER :: latCtr(:,:) ! Lat centers on this PET [rad]
    TYPE(MAPL_MetaComp), POINTER :: STATE => NULL()
    REAL(ESMF_KIND_R8), POINTER  :: Ptr3D_R8(:,:,:) => NULL()
    INTEGER                      :: I, nFlds
    TYPE(ESMF_STATE)             :: INTSTATE 
    TYPE(ESMF_Field)             :: GcFld

    ! Does GEOS-Chem restart file exist?
    ! Before broadcasting, we check if there is an import restart file for
    ! GEOS-Chem. This variable is then passed to Input_Opt after 
    ! initialization (and CPU broadcasting) of all GEOS-Chem variables.
    LOGICAL                      :: haveImpRst

    __Iam__('Initialize_')

    !=======================================================================
    ! Initialization
    !=======================================================================

    ! Get my name and set-up traceback handle
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Initialize_'
    
    ! Get my MAPL_Generic state
    ! -------------------------
    CALL MAPL_GetObjectFromGC(GC, STATE, RC=STATUS)
    VERIFY_(STATUS)

    !  Start timers
    !  ------------
    CALL MAPL_TimerOn( STATE, "TOTAL")
    CALL MAPL_TimerOn( STATE, "INITIALIZE")

    ! Initialize MAPL Generic
    CALL MAPL_GenericInitialize( GC, Import, Export, Clock, __RC__ )

    ! Get Internal state.
    CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ ) 

    ! Test if we are on the root PET
    am_I_Root = MAPL_Am_I_Root()

    ! Get various parameters from the ESMF/MAPL framework
    ! Note: variable haveImpRst must not yet be written into Input_Opt yet 
    ! since the variables of Input_Opt may be 'erased' during initialization
    ! of GEOS-Chem.
    CALL Extract_( GC,                        &  ! Ref to this Gridded Comp 
                   Clock,                     &  ! ESMF Clock object
                   Grid        = Grid,        &  ! ESMF Grid object
                   MaplCF      = MaplCF,      &  ! MAPL.rc Configuration object
                   GeosCF      = GeosCF,      &  ! GEOSCHEM*.rc Config object
                   I_LO        = I_LO,        &  ! Min lon index on this PET
                   J_LO        = J_LO,        &  ! Min lat  index on this PET
                   I_HI        = I_HI,        &  ! Max lon index on this PET
                   J_HI        = J_HI,        &  ! Max lat  index on this PET
                   IM          = IM,          &  ! # of longitudes on this PET
                   JM          = JM,          &  ! # of latitudes  on this PET
                   LM          = LM,          &  ! # of levels     on this PET
                   IM_WORLD    = IM_WORLD,    &  ! # of lons in global grid
                   JM_WORLD    = JM_WORLD,    &  ! # of lats  in global grid
                   LM_WORLD    = LM_WORLD,    &  ! # of levels in global grid
                   nymdB       = nymdB,       &  ! YYYYMMDD @ start of sim
                   nhmsB       = nhmsB,       &  ! hhmmss   @ end   of sim
                   nymdE       = nymdE,       &  ! YYYMMDD  @ start of sim
                   nhmsE       = nhmsE,       &  ! hhmmss   @ end   of sim
                   tsChem      = tsChem,      &  ! Chemistry timestep [seconds]
                   tsDyn       = tsDyn,       &  ! Dynamics timestep  [seconds]
                   localPet    = myPet,       &  ! PET # that we are on now
                   petCount    = NPES,        &  ! Number of PETs in MPI World
                   mpiComm     = mpiComm,     &  ! MPI Communicator Handle
                   lonCtr      = lonCtr,      &  ! This PET's lon ctrs [radians]
                   latCtr      = latCtr,      &  ! This PET's lat ctrs [radians]
		   haveImpRst  = haveImpRst,  &  ! Does import restart exist? 
                   __RC__                      )

    Input_Opt%myCpu   = myPet
    Input_Opt%mpiCOMM = mpiComm

    ! MSL - shift from 0 - 360 to -180 - 180 degree grid
    where (lonCtr .gt. MAPL_PI ) lonCtr = lonCtr - 2*MAPL_PI

    ! Name of logfile for stdout redirect
    CALL ESMF_ConfigGetAttribute( GeosCF, logFile,              &
                                  Label   = "STDOUT_LOGFILE:",  &
                                  Default = "PET%%%%%.init",    &
                                  __RC__                       )

    ! Name of log LUN # for stdout redirect
    CALL ESMF_ConfigGetAttribute( GeosCF, logLun,               &
                                  Label   = "STDOUT_LOGLUN:",   &
                                  Default = 700,                &
                                  __RC__                       )

    !=======================================================================
    ! Open a log file on each PET where stdout will be redirected
    !=======================================================================

    ! Check that the choice of LUN is valid
    IF ((logLun.eq.5).or.(logLun.eq.6)) Then
       WRITE(*,'(a,I5,a)') 'Invalid LUN (',logLun,') chosen for log output'
       WRITE(*,'(a)'     ) 'An LUN other than 5 or 6 must be used'
       ASSERT_(.FALSE.)
    ENDIF

    ! Replace tokens w/ PET # in the filename
    IF ( am_I_Root ) THEN
       WRITE( petStr, '(i5.5)' ) myPet
       CALL StrRepl( logFile, '%%%%%', petStr )
    ENDIF

    ! Open file for stdout redirect
    IF ( am_I_Root )  THEN
       OPEN ( logLun, FILE=LOGFILE, STATUS='UNKNOWN' )
    ENDIF

    ! Add descriptive header text
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(a)'   ) REPEAT( '#', 79 )
       WRITE( logLun, 100     ) TRIM( logFile ), TRIM( Iam ), myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    !=======================================================================
    ! Save values from the ESMF resource file into the Input_Opt object
    ! so that we can pass those into GeosCore/input_mod.F of GEOS-Chem
    !=======================================================================

    ! ewl note: might be able to remove some of these...

    ! Max # of diagnostics
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_BPCH_DIAG,  &
                                  Default = 80,                     &
                                  Label   = "MAX_DIAG:",            &
                                  __RC__                           ) 

    ! Max # of families per ND65 family tracer (not used for ESMF)
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_Families,   &
                                  Default = 20,                     &
                                  Label   = "MAX_FAM:",             & 
                                  __RC__                           )

    ! # of levels in LINOZ climatology
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%LINOZ_NLEVELS,  &
                                  Default = 25,                     &
                                  Label   = "LINOZ_NLEVELS:",       &
                                  __RC__                           )    

    ! # of latitudes in LINOZ climatology
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%LINOZ_NLAT,     &
                                  Default = 18,                     &
                                  Label   = "LINOZ_NLAT:",          & 
                                  __RC__                           )

    ! # of months in LINOZ climatology
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%LINOZ_NMONTHS,  &
                                  Default = 12,                     &
                                  Label   = "LINOZ_NMONTHS:",       &
                                  __RC__                           )

    ! # of fields in LINOZ climatology
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%LINOZ_NFIELDS,  &
                                  Default = 7,                      &
                                  Label   = "LINOZ_NFIELDS:",       &
                                  __RC__                           ) 

    ! # of run phases
    CALL ESMF_ConfigGetAttribute( GeosCF, NPHASE,                   & 
                                  Default = 2,                      &
                                  Label   = "RUN_PHASES:",          &
                                  __RC__                           )
    ASSERT_(NPHASE==1.OR.NPHASE==2) 

    !! THIS NEEDS DEALING WITH AT SOME POINT IN THE NEAR FUTURE
    !! -- what this fix meant to address is now handled by setting
    !! -- GIGCchem_REFERENCE_TIME in GCHP.rc. This is not an ideal
    !! -- solution since it presents another user-based change needed
    !! -- to alter the runtime parameters of GCHP. MSL
    !!=======================================================================
    !! Set the Chem Alarm Ringtime
    !!=======================================================================
    !! Query the chemistry alarm.
    !! ----------------------------------------------------------------------
    !CALL MAPL_Get(STATE, RUNALARM=ALARM, __RC__)
    !
    !! Query the current time (assuming this is beginning of run)
    !! ----------------------------------------------------------------------
    !call ESMF_ClockGet(currTime=currTime, __RC__)
    !
    !! Make sure that Chem is called on 1st timestep of the run
    !call ESMF_AlarmSet(ALARM, ringTime=currTime, __RC__)
    
    !=======================================================================
    ! Initialize GEOS-Chem (will also initialize HEMCO)
    !=======================================================================
 
    ! Call the GIGC initialize routine
    CALL GIGC_Chunk_Init( am_I_Root = am_I_Root,  & ! Are we on the root PET?
                          value_I_LO= I_LO,       & ! Min lon index on this PET
                          value_J_LO= J_LO,       & ! Min lat index on this PET
                          value_I_HI= I_HI,       & ! Max lon index on this PET
                          value_J_HI= J_HI,       & ! Max lat index on this PET
                          value_IM  = IM,         & ! # lons   on this PET
                          value_JM  = JM,         & ! # lats   on this PET
                          value_LM  = LM,         & ! # levels on this PET
                          value_IM_WORLD=IM_WORLD,& ! # lons   in global grid
                          value_JM_WORLD=JM_WORLD,& ! # lats   in global grid
                          value_LM_WORLD=LM_WORLD,& ! # levels in global grid
                          nymdB     = nymdB,      & ! YYYYMMDD @ start of run
                          nhmsB     = nhmsB,      & ! hhmmss   @ start of run
                          nymdE     = nymdE,      & ! YYYYMMDD @ end of run
                          nhmsE     = nhmsE,      & ! hhmmss   @ end of run
                          tsChem    = tsChem,     & ! Chemical timestep [s]
                          tsDyn     = tsDyn,      & ! Dynamic  timestep [s]
                          lonCtr    = lonCtr,     & ! Lon centers [radians]
                          latCtr    = latCtr,     & ! Lat centers [radians]
                          myPET     = myPET,      & ! Local PET
                          GC        = GC,         & ! Ref to this gridded comp
                          EXPORT    = EXPORT,     & ! Export state object
                          Input_Opt = Input_Opt,  & ! Input Options obj
                          State_Chm = State_Chm,  & ! Chemistry State obj
                          State_Diag= State_Diag, & ! Diagnostics State obj
                          State_Met = State_Met,  & ! Meteorology State obj
                          HcoConfig = HcoConfig,  & ! HEMCO Configuration Object
                          HistoryConfig = HistoryConfig, & ! History Config Obj
                          __RC__                 )

    ! Also save the MPI & PET specs to Input_Opt
    Input_Opt%myCpu   = myPet
    Input_Opt%MPICOMM = MPICOMM
    Input_Opt%NPES    = NPES
    Input_Opt%HPC     = .true. ! Yes, this is an HPC (ESMF) sim.
    if ( am_I_Root ) Input_Opt%RootCPU = .true.

    ! It's now safe to store haveImpRst flag in Input_Opt
    ! SDE DEBUG 2017-01-01: Overwrite this? The Extract_ routine does not seem
    ! to actually set this, and it being false results in everything being
    ! thrown off by 1 timestep
    haveImpRst = .True.
    Input_Opt%haveImpRst = haveImpRst

    IF ( isProvider ) THEN
       CALL Provider_Initialize( am_I_Root, IM,       JM,     LM,  &
                                 State_Chm, INTSTATE, EXPORT, __RC__ )
    ENDIF

    !=======================================================================
    ! Initialize the Int2Chm object. This is used to copy the tracer arrays
    ! from the internal state to State_Chm, and vice versa.
    ! In this step, we also check for the friendlieness of the tracers. If
    ! the GEOS-Chem internal convection/turbulence schemes shall be used
    ! (as specified in input.geos.rc), the tracers must not be friendly to
    ! the GEOS-5 moist / turbulence components!
    !=======================================================================
    nFlds = State_Chm%nAdvect
    ALLOCATE( Int2Chm(nFlds), STAT=STATUS )
    ASSERT_(STATUS==0)

    ! Do for every tracer in State_Chm
    DO I = 1, nFlds

       ! Get info about this species from the species database
       N = State_Chm%Map_Advect(I)
       ThisSpc => State_Chm%SpcData(N)%Info

       ! Pass tracer name
       Int2Chm(I)%TrcName = TRIM(ThisSpc%Name)

       ! Get tracer ID
       Int2Chm(I)%TrcID = IND_( TRIM(Int2Chm(I)%TrcName) )

       ! If tracer ID is not valid, make sure all vars are at least defined.
       IF ( Int2Chm(I)%TrcID <= 0 ) THEN
          Int2Chm(I)%Internal => NULL()
          CYCLE
       ENDIF

       ! Get internal state field
       CALL ESMF_StateGet( INTSTATE, TRIM(SPFX) // TRIM(Int2Chm(I)%TrcName), &
                           GcFld, RC=STATUS )
       IF ( STATUS /= ESMF_SUCCESS ) THEN
          WRITE(*,*) 'Cannot find in internal state: ' // TRIM(SPFX) // &
                     TRIM(Int2Chm(I)%TrcName)
          ASSERT_(.FALSE.)
       ENDIF

       ! Check friendliness of field: the field must not be friendly to 
       ! moist and/or turbulence if the corresponding GEOS-Chem switches 
       ! are turned on!
       ! Check for friendlieness to convection: only if GEOS-Chem convection 
       ! is enabled
       ! -- THIS IS CURRENTLY NOT RELEVANT TO THE GCHP, BECAUSE THERE'S ONLY 
       !    ONE CONVECTION ROUTINE. This code is still here, though, to 
       !    permit the inclusion of an external convection component. MSL
       !IF ( Input_Opt%LCONV ) THEN
       !   FRIENDLY=.FALSE.
       !   CALL ESMF_AttributeGet( GcFld, NAME="FriendlyToMOIST", &
       !                           VALUE=FRIENDLY, RC=STATUS )
       !   IF ( STATUS==ESMF_SUCCESS .AND. FRIENDLY ) THEN
       !      IF ( am_I_Root ) THEN
       !         WRITE(*,*) ' '
       !         WRITE(*,*) &
       !           'GEOS-Chem convection is turned on, but tracer is also'
       !         WRITE(*,*) &
       !           'friendly to MOIST. Cannot do both: ', &
       !           TRIM(Int2Chm(I)%TrcName)
       !         WRITE(*,*) ' '
       !      ENDIF
       !      ASSERT_(.FALSE.)
       !   ENDIF
       !ENDIF

       ! Check for friendlieness to turbulence: only if GEOS-Chem turbulence 
       ! is enabled
       ! -- THIS IS CURRENTLY NOT RELEVANT TO THE GCHP, BECAUSE THERE'S ONLY 
       !    ONE TURBULENCE ROUTINE. This code is still here, though, to 
       !    permit the inclusion of an external turbulence/mixing component. 
       !    MSL
       !IF ( Input_Opt%LTURB ) THEN
       !   FRIENDLY=.FALSE.
       !   CALL ESMF_AttributeGet( GcFld, NAME="FriendlyToTURBULENCE", &
       !                           VALUE=FRIENDLY, RC=STATUS )
       !   IF ( STATUS==ESMF_SUCCESS .AND. FRIENDLY ) THEN
       !      IF ( am_I_Root ) THEN
       !         WRITE(*,*) ' '
       !         WRITE(*,*) &
       !           'GEOS-Chem turbulence is turned on, but tracer is also'
       !         WRITE(*,*) &
       !           'friendly to TURBULENCE. Cannot do both: ', &
       !           TRIM(Int2Chm(I)%TrcName)
       !         WRITE(*,*) ' '
       !      ENDIF
       !      ASSERT_(.FALSE.)
       !   ENDIF
       !ENDIF

       ! Get pointer to field
       CALL ESMF_FieldGet( GcFld, 0, Ptr3D_R8, __RC__ )

       Int2Chm(I)%Internal => Ptr3D_R8
      
       ! Free pointers
       Ptr3D_R8 => NULL()
       ThisSpc  => NULL()

    ENDDO

    !=======================================================================
    ! Error trap: make sure that chemistry / emission time step are same and
    ! correspond to the chemistry step set in GEOSCHEMchem_GridComp.rc.
    !=======================================================================
    ChemTS = GET_TS_CHEM() 
    EmisTS = GET_TS_EMIS()
    IF ( ChemTS /= tsChem .OR. EmisTS /= tsChem ) THEN
       IF( am_I_Root ) THEN
          WRITE(*,*) 'GEOS-Chem chemistry and/or emission time step do not'
          WRITE(*,*) 'agree with time step set in GEOSCHEMchem_GridComp.rc'
          WRITE(*,*) 'GEOS-Chem chemistry time step                 : ', ChemTS
          WRITE(*,*) 'GEOS-Chem emission  time step                 : ', EmisTS
          WRITE(*,*) 'CHEMISTRY_TIMESTEP in GEOSCHEMchem_GridComp.rc: ', tsChem
       ENDIF
       ASSERT_(.FALSE.)
    ENDIF

    ! Also check for convection and dynamics time step.
    ChemTS = GET_TS_CONV()
    EmisTS = GET_TS_DYN()
    IF ( ChemTS /= tsDyn .OR. EmisTS /= tsDyn ) THEN
       IF( am_I_Root ) THEN
          WRITE(*,*) 'GEOS-Chem transport and/or convection time step do not'
          WRITE(*,*) 'agree with time step set in GEOSCHEMchem_GridComp.rc'
          WRITE(*,*) 'GEOS-Chem convection time step                : ', ChemTS
          WRITE(*,*) 'GEOS-Chem dynamics   time step                : ', EmisTS
          WRITE(*,*) 'RUN_DT in CAP.rc                              : ', tsDyn
       ENDIF
       ASSERT_(.FALSE.)
    ENDIF

    IF ( ArchivedConv .AND. am_I_Root ) THEN
       WRITE(*,*) ' '
       WRITE(*,*) ' --------------------------------------------------- '
       WRITE(*,*) ' GEOS-Chem will be using archived convection fields! '
       WRITE(*,*) ' --------------------------------------------------- '
       WRITE(*,*) ' '
    ENDIF

    ! Write a header before the timestepping begins
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(/,a)' ) REPEAT( '#', 79 )
       WRITE( logLun, 200     ) TRIM( compName ) // '::Run_', myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    ! Close the file for stdout redirect. Reopen when executing the run method.
    IF ( am_I_Root ) THEN
       CLOSE ( UNIT=logLun )
    ENDIF

    ! Stop timers
    ! -----------
    CALL MAPL_TimerOff( STATE, "INITIALIZE")

    CALL MAPL_TimerOff( STATE, "TOTAL")

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

!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run1
!
! !DESCRIPTION: Run1 is a wrapper method for the phase 1 run phase of the 
!  GEOSCHEMchem gridded component. It calls down to the Run method of the 
!  GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run1 ( GC, Import, Export, Clock, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock    ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Error return code
!
! !REMARKS:
!
! !REVISION HISTORY:
!  22 Sep 2014 - C. Keller   - Initial version.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
    CHARACTER(LEN=ESMF_MAXSTR)  :: Iam
    INTEGER                     :: STATUS
    INTEGER                     :: PHASE

    !=======================================================================
    ! Run1 starts here 
    !=======================================================================

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Run1'

    ! Call run routine stage 1 if more than one phase. If not 2 phases, 
    ! such as in GCHP, then we do all chemistry related processes from 
    ! Run2 instead.
    IF ( NPHASE == 2 ) THEN
       PHASE = 1
       CALL Run_ ( GC, IMPORT, EXPORT, CLOCK, PHASE, __RC__ )
    ENDIF

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Run1 
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run2
!
! !DESCRIPTION: Run2 is a wrapper method for the phase 2 or -1 run phase 
!  of the GEOSCHEMchem gridded component. It calls down to the Run method 
!  of the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run2 ( GC, Import, Export, Clock, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock    ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Error return code
!
! !REMARKS:
!
! !REVISION HISTORY:
!  22 Sep 2014 - C. Keller   - Initial version.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
    CHARACTER(LEN=ESMF_MAXSTR)  :: Iam
    INTEGER                     :: PHASE
    INTEGER                     :: STATUS

    !=======================================================================
    ! Run2 starts here 
    !=======================================================================

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Run2'

    ! Set phase number: this is 2 for multi-phase runs (e.g. GEOS-5), and
    ! is -1 for single-phase runs (e.g. GCHP). If set to -1, all processes 
    ! are called (drydep, emissions, chemistry, etc.)
    IF ( NPHASE == 1 ) THEN
       PHASE = -1
    ELSE
       PHASE = 2
    ENDIF

    ! Call run routine
    CALL Run_ ( GC, IMPORT, EXPORT, CLOCK, PHASE, __RC__ )

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Run2 
!EOC
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
!  Note: this routine currently skips the call down to GEOS-Chem on the very 
!  first time it is invoked. The reason is that a number of met-variables seem 
!  to be undefined still (e.g. BXHEIGHT, T, etc), yielding to seg-faults and/or 
!  crazy results. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run_( GC, Import, Export, Clock, Phase, RC )
!
! !USES:
!
    USE HCO_INTERFACE_MOD,       ONLY : HcoState
    USE Olson_Landmap_Mod,       ONLY : Compute_Olson_Landmap
    USE Precision_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC     ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import ! Import State
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock  ! ESMF Clock object
    INTEGER,             INTENT(IN   )         :: Phase  ! Run phase (-1/1/2)
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(  OUT)         :: RC     ! Error return code
!
! !REMARKS:
!  We call routine Extract_ to return various values (i.e. grid parameters,
!  start & end dates, PET information, etc.) from the ESMF/MAPL environment.  
!  We then pass those to GEOS-Chem via routine GIGC_CHUNK_RUN, which is
!  located in GEOS-Chem module ./GEOS-Chem/ESMF/gigc_chunk_mod.F90.

! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now uses the updated Extract_ method
!  09 Apr 2010 - R. Yantosca - Initialize Timing, GeoLoc objects
!  16 Apr 2010 - R. Yantosca - Now move the array assignments before & after
!                              the call to GC_CHUNK_RUN into separate
!                              include files, for clarity
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now use IDENT%VERBOSE to trigger debug output
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root PET
!  16 Oct 2012 - R. Yantosca - Now Include freeform files Includes_Before_Run.H
!                              and Includes_After_Run.H
!  13 Feb 2013 - R. Yantosca - Now call MAPL_Get_SunInsolation to return
!                              solar zenith angle and related properties
!  08 Mar 2013 - R. Yantosca - Now save the PET # (aka PET #) in Input_Opt 
!  15 Mar 2013 - R. Yantosca - Remove IDENT object, which was a holdover from
!                              the GEOS-Chem column code
!  22 Sep 2014 - C. Keller   - Added Phase argument
!  24 Oct 2014 - C. Keller   - Now derive all O3 export quantities from Tracers
!                              instead of Species (Species are zero in the 
!                              stratosphere!). Removed species from internal
!                              state as no physics was applied to them anyways.
!  29 Nov 2016 - E. Lundgren - Initialize Olson fractional land type, MODIS 
!                              LAI, and MODIS CHLR from ExtData imports
!  01 Sep 2017 - E. Lundgren - Enable automation of GCHP diagnostics by 
!                              setting data pointers and copying GC state values
!                              using the new HistoryConfig object
!  26 Jul 2017 - S. Eastham  - Read LAI from a single variable in file
!  06 Nov 2017 - E. Lundgren - Abstract provider services to new module
!                              gigc_providerservices_mod.F90
!EOP
!------------------------------------------------------------------------------
!BOC

!
! LOCAL VARIABLES:
!  
    ! Objects
    TYPE(ESMF_Grid)              :: Grid          ! ESMF Grid object
    TYPE(ESMF_Config)            :: MaplCF        ! Config (MAPL.rc)
    TYPE(ESMF_Config)            :: GeosCF        ! Config (GEOSCHEM*.rc)
    TYPE(ESMF_Alarm)             :: ALARM
                                                  
    ! Scalars                                     
    LOGICAL                      :: am_I_Root     ! Are we on the root PET?
    LOGICAL                      :: IsChemTime    ! Chemistry alarm proxy
    LOGICAL                      :: IsRunTime     ! Time to call GEOS-Chem
    LOGICAL                      :: IsTendTime    ! Time to calculate tendencies
    INTEGER                      :: IND           ! Species or tracer index
    INTEGER                      :: IM            ! # of lons   on this PET
    INTEGER                      :: JM            ! # of lats   on this PET
    INTEGER                      :: LM            ! # of levels on this PET
    INTEGER                      :: error         ! G-C error return code
    INTEGER(ESMF_KIND_I8)        :: advCount      ! # of clock advances
    INTEGER                      :: nymd          ! YYYY/MM/DD date
    INTEGER                      :: nhms          ! hh:mm:ss time
    INTEGER                      :: myPet         ! PET # we are on now
    INTEGER                      :: nPets         ! Total # of PETs
    INTEGER                      :: I, J, L       ! Loop indices
    INTEGER                      :: LR, N         ! Loop indices
    INTEGER                      :: N_TRC         ! Shadow var: # of tracers
    INTEGER                      :: year          ! Current year    
    INTEGER                      :: month         ! Current month
    INTEGER                      :: day           ! Current day
    INTEGER                      :: dayOfYr       ! Current day of year
    INTEGER                      :: hour          ! Current hour
    INTEGER                      :: minute        ! Current minute
    INTEGER                      :: second        ! Current second
    REAL                         :: UTC           ! Universal time
    REAL                         :: tsChem        ! Chem timestep [sec]
    REAL                         :: tsDyn         ! Dynamic timestep [sec]
    REAL                         :: hElapsed      ! Elapsed time [hours]
    REAL*8                       :: lonDeg        ! Longitude [degrees]
    REAL*8                       :: latDeg        ! Latitude [degrees]
    REAL                         :: locTime       ! Local time [hours]
    REAL*8                       :: P1, P2        ! Pressure variables
    CHARACTER(LEN=4)             :: petStr        ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)   :: compName      ! Gridded Component name
    
    ! Allocatable local arrays
    REAL,  ALLOCATABLE, TARGET   :: zenith(:,:)   ! Solar zenith angle
    REAL,  ALLOCATABLE, TARGET   :: solar(:,:)    ! Solar insolation
 
    ! Pointer arrays needed to initialize from imports
    REAL, POINTER                :: Ptr2d(:,:) => NULL()
    REAL, POINTER                :: Ptr3d   (:,:,:) => NULL()
    REAL(ESMF_KIND_R8), POINTER  :: Ptr3d_R8(:,:,:) => NULL()

    ! Other pointer arrays
    REAL(ESMF_KIND_R4),  POINTER :: lonCtr  (:,:) ! Lon centers, this PET [rad]
    REAL(ESMF_KIND_R4),  POINTER :: latCtr  (:,:) ! Lat centers, this PET [rad
    TYPE(MAPL_MetaComp), POINTER :: STATE

    ! For CTM Mode
    REAL(ESMF_KIND_R8),  POINTER :: PLE(:,:,:)     => NULL() ! INTERNAL: PEDGE
    REAL,                POINTER :: AIRDENS(:,:,:) => NULL() ! INTERNAL: PEDGE

    ! For aero bundle
    INTEGER                      :: nAero
    TYPE(ESMF_STATE)             :: INTSTATE
    TYPE(ESMF_FieldBundle)       :: AeroBdl 
    TYPE(ESMF_Field)             :: AeroFld
    REAL, POINTER                :: GcPtr3d  (:,:,:) => NULL()
    REAL, POINTER                :: AeroPtr3d(:,:,:) => NULL()
    CHARACTER(LEN=ESMF_MAXSTR)   :: GcName
    REAL                         :: GCMW, FRAC

    ! Tracer & HEMCO bundle
    INTEGER :: trcID, RST
    REAL    :: COEFF
    CHARACTER(LEN=ESMF_MAXSTR)   :: trcNAME,hcoNAME
    TYPE(ESMF_Field      )       :: trcFIELD
    TYPE(ESMF_FieldBundle)       :: trcBUNDLE
    REAL              , POINTER  :: fPtrArray(:,:,:)
    REAL(ESMF_KIND_R8), POINTER  :: fPtrVal, fPtr1D(:)

    ! Initialize everything to zero (from registry file)?
    !INTEGER                      :: InitZero 
 
    ! Initialize from GC Classic restart file (see registry file)?
    !INTEGER                      :: InitGCC

    ! Initialize variables used for reading Olson and MODIS LAI imports,
    ! and setting land/water/ice index (State_Met%LWI)
    INTEGER            :: T, V, landTypeInt
    INTEGER            :: IMAXLOC(1)
    CHARACTER(len=64)  :: landTypeStr, varName, importName

    ! First call?
    LOGICAL, SAVE      :: FIRST    = .TRUE.

    __Iam__('Run_')

    !=======================================================================
    ! Run starts here
    !=======================================================================

    ! Are we on the root PET?
    am_I_Root = MAPL_Am_I_Root()

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Run_'

    ! Get my MAPL_Generic state
    ! -------------------------
    CALL MAPL_GetObjectFromGC(GC, STATE, __RC__)

    ! Query the chemistry alarm.
    ! This checks if it's time to do chemistry, based on the time step
    ! set in AGCM.rc (GIGCchem_DT:). If the GEOS-Chem time step is not
    ! specified in AGCM.rc, the heartbeat will be taken (set in MAPL.rc).
    ! ----------------------------------------------------------------------
    CALL MAPL_Get(STATE, RUNALARM=ALARM, __RC__)
    IsChemTime = ESMF_AlarmIsRinging(ALARM, __RC__)

    ! Turn off alarm: only if it was on and this is not phase 1 (don't turn off
    ! after phase 1 since this would prevent phase 2 from being executed).
    IF ( IsChemTime .AND. PHASE /= 1 ) THEN
       CALL ESMF_AlarmRingerOff(ALARM, __RC__ )
    ENDIF

    ! Get Internal state
    CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ )

    ! ----------------------------------------------------------------------
    ! Check if we need to call the GEOS-Chem driver. The GEOS-Chem driver 
    ! contains routines for the following processes:
    !
    ! Phase 1:
    ! (1) Convection:     --> Dynamics time step  (optional)
    ! (2) Dry deposition  --> Chemistry time step
    ! (3) Emissions       --> Chemistry time step
    !
    ! Phase 2:
    ! (4) Turbulence      --> Dynamics time step  (optional)
    ! (5) Chemistry       --> Chemistry time step
    ! (6) Wet deposition  --> Dynamics time step
    ! 
    ! Phase -1:
    ! Includes all of the above
    !
    ! Convection and turbulence are only called if the corresponding 
    ! switches are turned on in the GEOS-Chem input file (input.geos.rc).
    !
    ! To avoid unnecessary calls to the GEOS-Chem driver routine, we 
    ! check here if it's time for any of the processes listed above.
    ! The IsChemTime variable will be passed down to the GEOS-Chem driver
    ! to ensure that chemistry is only executed if it's time to do so.
    !
    ! The O3 and H2O tendencies will only be calculated when doing chemistry 
    ! (set to zero otherwise). All other export variables are updated every 
    ! time GEOS-Chem is called.
    ! ----------------------------------------------------------------------
    IsRunTime = IsChemTime
    IF ( Input_Opt%LCONV .AND. Phase /= 2 ) IsRunTime = .TRUE.
    IF ( Input_Opt%LTURB .AND. Phase /= 1 ) IsRunTime = .TRUE.
    IF ( Input_Opt%LWETD .AND. Phase /= 1 ) IsRunTime = .TRUE.

    ! Is it time to update tendencies?
    ! Tendencies shall only be updated when chemistry is done, which is 
    ! Phase -1 or 2.
    IsTendTime = ( IsChemTime .AND. Phase /= 1 )

    ! Start timers
    ! ------------
    CALL MAPL_TimerOn(STATE, "TOTAL")

    ! Get pointers to fields in import, internal, and export states defined
    ! in the registry file. This has to be done on the first call only.
    IF ( FIRST ) THEN
#      include "GIGCchem_GetPointer___.h"

       !IF ( IsCTM ) THEN
       call MAPL_GetPointer ( IMPORT, PLE,      'PLE',     __RC__ )
       call MAPL_GetPointer ( IMPORT, AIRDENS,  'AIRDENS', __RC__ )
       !ENDIF

       ! Set up pointers if GEOS-Chem is a provider
       !IF ( isProvider ) THEN
       CALL Provider_SetPointers( am_I_Root, EXPORT, calcOzone, __RC__ )
       !ENDIF

       ! Pass IMPORT/EXPORT object to HEMCO state object
       !CALL GetHcoState( HcoState )
       ASSERT_(ASSOCIATED(HcoState))
       HcoState%GRIDCOMP => GC
       HcoState%IMPORT   => IMPORT
       HcoState%EXPORT   => EXPORT
!       HcoState => NULL()

       ! To use archived convection fields
       IF ( ArchivedConv ) THEN
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_PFI_CN ,           &
                                 'ARCHIVED_PFI_CN'  , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_PFL_CN ,           &
                                 'ARCHIVED_PFL_CN'  , notFoundOK=.TRUE., &
                                  __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_MFC,           &
                                 'ARCHIVED_CNV_MFC' , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_MFD,           &
                                 'ARCHIVED_CNV_MFD' , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_CVW,           &
                                 'ARCHIVED_CNV_CVW' , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_DQRC   ,           &
                                 'ARCHIVED_DQRC'    , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_REV_CN ,           &
                                 'ARCHIVED_PFI_CN'  , notFoundOK=.TRUE., &
                                 __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_T      ,           &  
                                 'ARCHIVED_T'       , notFoundOK=.TRUE., &
                                  __RC__ )
       ENDIF
    ENDIF

    ! Run when it's time to do so
    ! Always run on first call to make sure that all variables become
    ! properly specified and initialized.
    ! ------------------------------------------------------------------
    RunningGEOSChem: IF(IsRunTime .OR. FIRST) THEN

       CALL MAPL_TimerOn(STATE, "RUN"  )
       
       ! Re-open file for stdout redirect
       IF ( am_I_Root )  THEN
          OPEN ( UNIT=logLun, FILE   = TRIM( logFile ), STATUS = 'OLD' ,   &
                              ACTION ='WRITE',          ACCESS = 'APPEND' )
       ENDIF
       
       ! Get various parameters from the ESMF/MAPL framework
       CALL Extract_( GC,                   &  ! Ref to this Gridded Component
                      Clock,                &  ! ESMF Clock object
                      Grid      = Grid,     &  ! ESMF Grid object
                      MaplCf    = MaplCF,   &  ! ESMF Config obj (MAPL*.rc) 
                      GeosCf    = GeosCF,   &  ! ESMF Config obj (GEOSCHEM*.rc)
                      tsChem    = tsChem,   &  ! Chemistry timestep [min]
                      tsDyn     = tsDyn,    &  ! Dynamic timestep [min]
                      nymd      = nymd,     &  ! Current YYYY/MM/DD date
                      nhms      = nhms,     &  ! Current hh:mm:ss time
                      year      = year,     &  ! Current year
                      month     = month,    &  ! Current month
                      day       = day,      &  ! Current day
                      dayOfYr   = dayOfYr,  &  ! Current day of year
                      hour      = hour,     &  ! Current hour
                      minute    = minute,   &  ! Current minute
                      helapsed  = hElapsed, &  ! Elapsed hours
                      advCount  = advCount, &  ! # of times clock has advanced
                      IM        = IM,       &  ! # of longitudes on this PET
                      JM        = JM,       &  ! # of latitudes  on this PET
                      LM        = LM,       &  ! # of levels     on this pET
                      utc       = utc,      &  ! Universal time [hours]
                      localpet  = myPet,    &  ! # of the PET we are on now
                      petCount  = nPets,    &  ! Total # of PETs
                      __RC__ )
       
       ! Allocate GMAO_ZTH (declared at top of module)
       IF ( .not. ALLOCATED( zenith ) ) THEN
          ALLOCATE( zenith( IM, JM ), STAT=STATUS)
          VERIFY_(STATUS)
       ENDIF
       
       ! Allocate GMAO_SLR (declared @ top of module)
       IF ( .not. ALLOCATED( solar ) ) THEN
          ALLOCATE( solar( IM, JM ), STAT=STATUS)
          VERIFY_(STATUS)
       ENDIF
       
       ! Call EXTRACT a second time to get the solar zenith
       ! angle and solar insolation fields
       CALL Extract_( GC,                   &  ! Ref to this Gridded Component
                      Clock,                &  ! ESMF Clock object
                      Grid      = Grid,     &  ! ESMF Grid object
                      MaplCf    = MaplCF,   &  ! ESMF Config obj (MAPL*.rc) 
                      GeosCf    = GeosCF,   &  ! ESMF Config obj (GEOSCHEM*.rc)
                      lonCtr    = lonCtr,   &  ! Lon centers on this PET [rad]
                      latCtr    = latCtr,   &  ! Lat centers on this PET [rad]
                      ZTH       = zenith,   &  ! Solar zenith angle
                      SLR       = solar,    &  ! Solar insolation
                      __RC__ )
       
       ! MSL - shift from 0 - 360 to -180 - 180 degree grid
       where (lonCtr .gt. MAPL_PI ) lonCtr = lonCtr - 2*MAPL_PI
       
       !=======================================================================
       ! Print timing etc. info to the log file outside of the (I,J) loop
       !=======================================================================
       IF ( am_I_Root ) THEN
          WRITE( logLun, 100 ) year, month, day, hour, minute, hElapsed
       ENDIF
       
       !=======================================================================
       ! Prevent use of (occasional) MAPL_UNDEF tropopause pressures
       !=======================================================================
       
       ! GCCTROPP contains the last valid tropopause pressure
       WHERE ( TROPP /= MAPL_UNDEF ) GCCTROPP = TROPP
       
       ! If any values in GCCTROPP are undefined, stop the run
       IF ( ANY( GCCTROPP == MAPL_UNDEF ) ) THEN
          PRINT *,TRIM(Iam)//": At least one invalid tropopause pressure."
          STATUS = GC_FAILURE
          VERIFY_(STATUS)
       ENDIF
       
       !=======================================================================
       ! pre-Run method array assignments. This passes the tracer arrays from
       ! the internal state to State_Chm. On the first call, it also fills the
       ! internal species arrays in State_Chm with the values read from the
       ! restart file (and stored in the internal state).
       !=======================================================================
       
       ! SDE: This will overwrite and State_Chm%Species with
       !      data in units of v/v dry (2016-03-28)
       ! EWL: Includes_Before_Run.H is now limited to setting State_Met 
       !      variables from IMPORT state
       CALL MAPL_TimerOn(STATE, "CP_BFRE")
#      include "Includes_Before_Run.H"
       CALL MAPL_TimerOff(STATE, "CP_BFRE")

       !=======================================================================
       ! Pass advected tracers from internal state to GEOS-Chem tracers array
       !=======================================================================
       DO I = 1, SIZE(Int2Chm,1)
          IF ( Int2Chm(I)%TrcID <= 0 ) CYCLE
          State_Chm%Species(:,:,:,Int2Chm(I)%TrcID) = Int2Chm(I)%Internal
       ENDDO
       
       ! Flip in the vertical
       State_Chm%Species   = State_Chm%Species( :, :, LM:1:-1, : )
       
       !=======================================================================
       ! On first call, also need to initialize the species from restart file.
       ! Only need to do this for species that are not advected, i.e. species
       ! that are not tracers (all other species arrays will be filled with
       ! tracer values anyways!).
       ! We only need to do this on the first call because afterwards, species
       ! array already contains values from previous chemistry time step
       ! (advected species will be updated with tracers)
       ! ckeller, 10/27/2014
       !=======================================================================
       IF ( FIRST ) THEN
       
          ! Get Generic State
          call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
          VERIFY_(STATUS)
          ! Get Internal state
          CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTERNAL, __RC__ ) 
       
          ! Loop over all species and get info from spc db
          DO N = 1, State_Chm%nSpecies
             ThisSpc => State_Chm%SpcData(N)%Info
             IF ( TRIM(ThisSpc%Name) == '' ) CYCLE
             IND = IND_( TRIM(ThisSpc%Name ) )
             IF ( IND < 0 ) CYCLE
       
             ! Get data from internal state and copy to species array
             CALL MAPL_GetPointer( INTERNAL, Ptr3D_R8, TRIM(SPFX) //          &
                                   TRIM(ThisSpc%Name), notFoundOK=.TRUE.,     &
                                   __RC__ )
             IF ( .NOT. ASSOCIATED(Ptr3D_R8) ) THEN
                IF ( MAPL_am_I_Root()) WRITE(*,*)                             &
                   'Could not find species in INTERNAL state - will be ' //   &
                   'initialized to zero: ', TRIM(SPFX), TRIM(ThisSpc%Name)
                State_Chm%Species(:,:,:,IND) = 1d-26
                CYCLE
             ENDIF
             State_Chm%Species(:,:,:,IND) = Ptr3D_R8(:,:,LM:1:-1)
             if ( MAPL_am_I_Root()) WRITE(*,*)                                &
             'Initialized species from INTERNAL state: ', TRIM(ThisSpc%Name)

             ! Determine if species in restart file
             CALL ESMF_StateGet( INTERNAL, TRIM(SPFX) // TRIM(ThisSpc%Name),  &
                  trcFIELD, RC=RC )
             CALL ESMF_AttributeGet( trcFIELD, NAME="RESTART",                &
                  VALUE=RST, RC=STATUS )
       
             ! Set spc conc to background value if rst skipped or var not there
             IF ( RC /= ESMF_SUCCESS .OR. RST == MAPL_RestartBootstrap .OR.   &
                      RST == MAPL_RestartSkipInitial ) THEN
                DO L = 1, LLPAR 
                DO J = 1, JJPAR
                DO I = 1, IIPAR
                   ! Special handling for MOH (mimicking GEOS-Chem Classic)
                   IF ( TRIM( ThisSpc%Name ) == 'MOH' ) THEN
                      ! Test for altitude (L < 9 is always in the trop)
                      IF ( L <= 9 ) THEN
                         ! Test for ocean/land boxes
                         IF ( State_Met%FRCLND(I,J) >= 0.5 ) THEN
                            ! Continental boundary layer: 2 ppbv MOH
                            State_Chm%Species(I,J,L,IND) = 2.000e-9_fp
                         ELSE
                            ! Marine boundary layer: 0.9 ppbv MOH
                            State_Chm%Species(I,J,L,IND) = 0.900e-9_fp
                         ENDIF
                      ELSE
                         ! Test for troposphere
                         IF ( State_Met%InTroposphere(I,J,L) ) THEN
                            ! Free troposphere: 0.6 ppbv MOH
                            State_Chm%Species(I,J,L,IND) = 0.600e-9_fp 
                         ELSE
                            ! Strat/mesosphere:
                            State_Chm%Species(I,J,L,IND) = 1.0E-30_FP 
                         ENDIF
                      ENDIF
                   ELSEIF ( L > LLCHEM .AND. &
                            ( .NOT. ThisSpc%Is_Advected ) ) THEN
                      ! For non-advected spc at L > LLCHEM, use small number
                      State_Chm%Species(I,J,L,IND) = 1.0E-30_FP           
                   ELSE
                      ! For all other cases, use the background value in spc db
                      State_Chm%Species(I,J,L,IND) = ThisSpc%BackgroundVV 
                   ENDIF
                ENDDO
                ENDDO
                ENDDO
                Ptr3D_R8(:,:,:) = State_Chm%Species(:,:,LM:1:-1,IND)
                IF ( MAPL_am_I_Root()) THEN
                   WRITE(*,*)  &
                   '   WARNING: using background values from species database'
                ENDIF
             ENDIF
             ThisSpc => NULL()
          ENDDO
       ENDIF

       !=======================================================================
       ! On first call, initialize certain other State_Chm arrays from 
       ! imports if they are found (ewl, 12/13/18)
       !=======================================================================
       IF ( FIRST ) THEN
          CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'H2O2AfterChem',  &
                                notFoundOK=.TRUE., __RC__ )
          IF ( ASSOCIATED(Ptr3d) .AND. &
               ASSOCIATED(State_Chm%H2O2AfterChem) ) THEN
             State_Chm%H2O2AfterChem = Ptr3d(:,:,LM:1:-1)
          ENDIF
          Ptr3d => NULL()
          
          CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'SO2AfterChem',   &
                                notFoundOK=.TRUE., __RC__ )
          IF ( ASSOCIATED(Ptr3d) .AND. &
               ASSOCIATED(State_Chm%SO2AfterChem) ) THEN
             State_Chm%SO2AfterChem = Ptr3d(:,:,LM:1:-1)
          ENDIF
          Ptr3d => NULL()
          
          CALL MAPL_GetPointer( INTSTATE, Ptr2d, 'DryDepNitrogen', &
                                notFoundOK=.TRUE., __RC__ )
          IF ( ASSOCIATED(Ptr2d) .AND. &
               ASSOCIATED(State_Chm%DryDepNitrogen) ) THEN
             State_Chm%DryDepNitrogen = Ptr2d
          ENDIF
          Ptr2d => NULL()
          
          CALL MAPL_GetPointer( INTSTATE, Ptr2d, 'WetDepNitrogen', &
                                notFoundOK=.TRUE., __RC__ )
          IF ( ASSOCIATED(Ptr2d) .AND. &
               ASSOCIATED(State_Chm%WetDepNitrogen) ) THEN
             State_Chm%WetDepNitrogen = Ptr2d
          ENDIF
          Ptr2d => NULL()
          
          CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'KPPHvalue' ,     &
                                notFoundOK=.TRUE., __RC__ )
          IF ( ASSOCIATED(Ptr3d) .AND. &
               ASSOCIATED(State_Chm%KPPHvalue) ) THEN
             State_Chm%KPPHvalue(:,:,1:LLCHEM) =  &
                 REAL(Ptr3d(:,:,LM:LM-LLCHEM+1:-1),KIND=ESMF_KIND_R8)
          ENDIF
          Ptr3d => NULL()
       ENDIF

       !=======================================================================
       ! Set Olson land map types from import of Olson file. 
       !=======================================================================
       ! We are currently using land type fractions derived from the 2001
       ! Olson land map instead of GEOS5 vegetation type fractions. Fractions
       ! are calculated by ExtData using conservate fractional regridding of
       ! the native 0.25x0.25 resolution file. (ewl, 11/29/16)
       !
       ! Previous:
       ! Set land types in State_Met from GEOS5 vegetation type fractions or
       ! OLSON land type fractions. For now, the land types are treated as 
       ! static and obtained from offline fields. The routine below thus needs
       ! to be called only once.
       ! Once the GEOS-5 land types are dynamic, we should import those from
       ! the surface component (field ITY, or better: vegetation type fractions
       ! per grid box).                                   (ckeller, 01/06/2015)
       !
       !=======================================================================
       IF ( FIRST ) THEN
       
          ! Set Olson fractional land type from import (ewl)
          If (am_I_Root) Write(6,'(a)') 'Initializing land type ' //         &
                           'fractions from Olson imports'
          Ptr2d => NULL()
          DO T = 1, NSURFTYPE
       
             ! Create two-char string for land type
             landTypeInt = T-1
             WRITE ( landTypeStr, '(I2.2)' ) landTypeInt
             importName = 'OLSON' // TRIM(landTypeStr)
       
             ! Get pointer and set populate State_Met variable
             CALL MAPL_GetPointer ( IMPORT, Ptr2D, TRIM(importName),         &
                                    notFoundOK=.TRUE., __RC__ )
             If ( Associated(Ptr2D) ) Then
                If (am_I_Root) Write(6,*)                                    &
                     ' ### Reading ' // TRIM(importName) // ' from imports'
                State_Met%LandTypeFrac(:,:,T) = Ptr2D(:,:)
             ELSE
                WRITE(6,*) TRIM(importName) // ' pointer is not associated'
             ENDIF
       
             ! Nullify pointer
             Ptr2D => NULL()
          ENDDO

          ! Add an error check to stop the run if the Olson land
          ! map data comes back as all zeroes.  This issue is known
          ! to happen in gfortran but not with ifort. (bmy, 1/10/19)
          IF ( MINVAL( State_Met%LandTypeFrac ) < 1e-32  .and.               &
               MAXVAL( State_Met%LandTypeFrac ) < 1e-32 ) THEN
             WRITE( 6, '(a)' )                                               &
                'ERROR: State_Met%LandTypeFrac contains all zeroes! '     // & 
                'This error is a known issue in MAPL with gfortran. '     // &
                'You should not get this error if you compiled with '     // &
                'ifort.'
             RC = GC_FAILURE
             ASSERT_(RC==GC_SUCCESS)
          ENDIF
  
          ! Compute State_Met variables IREG, ILAND, IUSE, and FRCLND
          CALL Compute_Olson_Landmap( am_I_Root, Input_Opt, State_Met, RC )
       ENDIF
       
       !=======================================================================
       ! Read MODIS leaf area index (LAI) from imports of post-processed MODIS 
       ! files. Monthly files are interpolated once per day. Read Chlorophyll-a
       ! data is using the marine POA simulation.
       !
       ! DESCRIPTION OF LAI DATA READ BY EXTDATA:
       ! Each monthly file contains 73 variables of format XLAIxx where xx is
       ! land type (e.g. XLAI00 to XLAI72). Grid cells with land type xx have
       ! native LAI values; all other grid cells have zero values. ExtData 
       ! regrids the native resolution files to yield average values per GC 
       ! grid cell (not area-weighted). These are used with the similarly
       ! regridded Olson fractional land types to construct area-weighted LAI 
       ! per land type per grid cell, recreating the GEOS-Chem classic
       ! online regridding in modis_lai_mod. (ewl, 11/29/16)
       !=======================================================================
       IF ( FIRST .OR. ITS_A_NEW_DAY() ) THEN
          If (am_I_Root) Write(6,'(a)') 'Initializing leaf area index ' // &
                           'variable from imports'
       
          ! Try getting the MULTI import first
          Ptr3D => NULL()
          importName = 'XLAIMULTI'
          Call MAPL_GetPointer ( IMPORT, Ptr3D, Trim(importName), &
             notFoundOK = .TRUE., __RC__ )
          If ( Associated(Ptr3D) ) Then
             If (am_I_Root) Write(6,'(a)') ' ### Reading XLAI from multi-import'
             State_Met%XLAI_NATIVE(:,:,:) = Ptr3D(:,:,:)
          Else
             Ptr2d => NULL()
             DO T = 1, NSURFTYPE
             
                ! Create two-char string for land type
                landTypeInt = T-1
                IF ( landTypeInt < 10 ) THEN
                   WRITE ( landTypeStr, "(A1,I1)" ) '0', landTypeInt
                ELSE
                   WRITE ( landTypeStr, "(I2)" ) landTypeInt  
                ENDIF
             
                ! Get pointer and populate State_Met variable for XLAI_NATIVE
                importName = 'XLAI' // TRIM(landTypeStr)
             
                CALL MAPL_GetPointer ( IMPORT, Ptr2D, TRIM(importName),  &
                                       notFoundOK=.TRUE., __RC__ )
                If ( Associated(Ptr2D) ) Then
                   If (am_I_Root) Write(6,*)                                &
                        ' ### Reading ' // TRIM(importName) // ' from imports'
                   State_Met%XLAI_NATIVE(:,:,T) = Ptr2D(:,:)
                ELSE
                   WRITE(6,*) TRIM(importName) // ' pointer is not associated'
                ENDIF
                Ptr2D => NULL()
             ENDDO
          ENDIF
          Ptr3D => NULL()
       ENDIF
       
       !=======================================================================
       ! Get UV albedo for photolysis if first timestep or its a new month
       !=======================================================================
       IF ( FIRST .OR. ITS_A_NEW_MONTH() ) THEN
          Ptr2d => NULL()
          CALL MAPL_GetPointer ( IMPORT, Ptr2D, 'UV_ALBEDO',  &
                                 notFoundOK=.TRUE., __RC__ )
          If ( Associated(Ptr2D) ) Then
             If (am_I_Root) Write(6,*)                                &
                  ' ### Reading UV_ALBEDO from imports'
             State_Met%UVALBEDO(:,:) = Ptr2D(:,:)
          ELSE
             IF ( am_I_Root ) Write(6,*) 'UV_ALBEDO pointer is not associated'
          ENDIF
          Ptr2D => NULL()
       ENDIF
       
       !=======================================================================
       ! Get total ozone column from GEOS-Chem export variable.
       ! Need to calculate from restart variables on first call!
       !=======================================================================
       IF ( calcOzone ) THEN
          IF ( FIRST ) THEN
             CALL CalcTotalOzone( am_I_Root, PLE, GCCTROPP, __RC__ )
          ENDIF
          CALL SetStateMetTO3( am_I_Root, State_Met, __RC__ )
       ENDIF

       !=======================================================================
       ! Execute GEOS-Chem on multiple PETs
       !=======================================================================
       
       ! Fix negatives!
       ! These can be brought in as an artifact of convection.
       WHERE ( State_Chm%Species < 0.0e0 )
          State_Chm%Species = 1.0e-36
       END WHERE 
       
       ! Execute GEOS-Chem if it's time to run it
       IF ( IsRunTime ) THEN
       
          ! This is mostly for testing
          IF ( FIRST .AND. Input_Opt%haveImpRst ) THEN
             IF ( am_I_Root ) THEN
                WRITE(*,*) ''
                WRITE(*,*) 'Doing warm GEOS-Chem restart'
                WRITE(*,*) ''
             ENDIF
          ENDIF
       
          ! Only if restart file exists...
          IF ( Input_Opt%haveImpRst ) THEN
       
             CALL MAPL_TimerOn(STATE, "DO_CHEM")
       
             ! NOTE: Second was not extracted previously; set to 0 for now
             second = 0
             ! Run the GEOS-Chem column chemistry code for the given phase
             CALL GIGC_Chunk_Run( am_I_Root  = am_I_Root,  & ! Is this root PET?
                                  GC         = GC,         & ! Grid comp ref. 
                                  IM         = IM,         & ! # lons on PET
                                  JM         = JM,         & ! # lats on PET
                                  LM         = LM,         & ! # levs on PET
                                  nymd       = nymd,       & ! Current YYYYMMDD
                                  nhms       = nhms,       & ! Current hhmmss
                                  year       = year,       & ! Current year
                                  month      = month,      & ! Current month
                                  day        = day,        & ! Current day
                                  dayOfYr    = dayOfYr,    & ! Current doy
                                  hour       = hour,       & ! Current hour
                                  minute     = minute,     & ! Current minute
                                  second     = second,     & ! Current second
                                  utc        = utc,        & ! Current UTC [hrs]
                                  hElapsed   = hElapsed,   & ! Elapsed hours
                                  Input_Opt  = Input_Opt,  & ! Input Options
                                  State_Chm  = State_Chm,  & ! Chemistry State
                                  State_Met  = State_Met,  & ! Meteorology State
                                  State_Diag = State_Diag, & ! Diagnostics State
                                  Phase      = Phase,      & ! Run phase
                                  IsChemTime = IsChemTime, & ! Time for chem?
                                  __RC__                  )  ! Success or fail?
       
             CALL MAPL_TimerOff(STATE, "DO_CHEM")
       
          ! Restart file does not exist:
          ELSE
             IF ( am_I_Root ) THEN
                WRITE(*,*) ''
                WRITE(*,*) ' SKIP GEOS-CHEM PHASE ', Phase, &
                           ' BECAUSE IMPORT RESTART FILE IS MISSING'
                WRITE(*,*) ''
             ENDIF
             where( State_Met%HFLUX .eq. 0.) State_Met%HFLUX = 1e-5
          ENDIF 
       ENDIF !IsRunTime
       
       !=======================================================================
       ! post-Run method array assignments. This copies the values back from
       ! the State_Chm tracer arrays to the internal state, so that they can
       ! be seen by other components (moist, turbulence, ...)
       !=======================================================================
       
       CALL MAPL_TimerOn(STATE, "CP_AFTR")
       State_Chm%Species = State_Chm%Species( :, :, LM:1:-1, : )
       
       DO I = 1, SIZE(Int2Chm,1)
          IF ( Int2Chm(I)%TrcID <= 0 ) CYCLE
          Int2Chm(I)%Internal = State_Chm%Species(:,:,:,Int2Chm(I)%TrcID)
       ENDDO
       
       CALL MAPL_TimerOff(STATE, "CP_AFTR")
       
       ! Stop timer
       ! ----------
       CALL MAPL_TimerOff(STATE, "RUN"  )
       
       ! Fill bundles only on chemistry time steps and if phase -1 or 2 
       ! --------------------------------------------------------------
       IF ( IsTendTime ) THEN

          IF ( isProvider ) THEN
             CALL Provider_FillBundles( am_I_Root, tsChem,    PLE, GCCTROPP, &
                                        STATE,     Input_Opt, GC,  EXPORT,   &
                                        __RC__ )
          ENDIF

          IF ( calcOzone ) THEN
             !================================================================
             ! Total ozone and total tropospheric ozone for export [dobsons].
             ! 2.69E+20 per dobson.
             !================================================================
             CALL CalcTotalOzone( am_I_Root, PLE, GCCTROPP, __RC__ )
          ENDIF

       ENDIF ! IsTendTime

    ENDIF RunningGEOSChem

    !=======================================================================
    ! If we were not doing chemistry, make sure that all tendencies are
    ! zero. We ignore the tendencies that may arise due to physical
    ! processes covered by GEOS-Chem (e.g. convection).
    !=======================================================================
    IF ( .NOT. IsTendTime ) THEN
       CALL Provider_ZeroTendencies( am_I_Root, __RC__ )
    ENDIF

    !=======================================================================
    ! Copy HISTORY.rc diagnostic data to exports. Includes HEMCO emissions 
    ! diagnostics but excludes internal state and exports created explicitly
    ! in Chem_GridCompMod. NOTE: Exports created explicitly in Chem_GridCompMod
    ! will eventually be moved elsewhere as diagnostics for use with GEOS-5.
    ! (ewl, 11/2/17)
    !=======================================================================
    IF ( FIRST ) THEN
       CALL HistoryExports_SetDataPointers( am_I_Root,     EXPORT,        &
                                            HistoryConfig, State_Chm,     &
                                            State_Diag,    State_Met,   STATUS )
       VERIFY_(STATUS)
    ENDIF
    CALL CopyGCStates2Exports( am_I_Root, Input_Opt, HistoryConfig, STATUS )
    VERIFY_(STATUS)

    !=======================================================================
    ! All done
    !=======================================================================

    IF ( ALLOCATED( zenith ) ) DEALLOCATE( zenith )
    IF ( ALLOCATED( solar  ) ) DEALLOCATE( solar  )

    ! Close the file for stdout redirect.
    IF ( am_I_Root ) THEN
       CLOSE ( UNIT=logLun )
    ENDIF
       
    ! Stop timer
    ! ----------
    CALL MAPL_TimerOff(STATE, "TOTAL")
  
    ! The restart file should exist at least after the first full cycle,
    ! e.g. after phase 1 and phase 2 has been called once.
    IF ( FIRST .AND. Phase == 1 ) THEN
       Input_Opt%haveImpRst = Input_Opt%haveImpRst
    ELSE
       Input_Opt%haveImpRst = .TRUE.
    ENDIF

    ! Update first flags
    FIRST = .FALSE.

    ! Successful return
    RETURN_(ESMF_SUCCESS)

    ! Formats
100 FORMAT( '---> DATE: ', i4.4, '/', i2.2, '/', i2.2,      &
            '  GMT: ', i2.2, ':', i2.2, '  X-HRS: ', f11.3 )
110 FORMAT( 'Box (',i3,',',i3,') on PET ', i3, ' has coords: ', 2f7.2, &
               ' LocT = ', f9.4 )
200 FORMAT( '### ',                                           / &
            '### ', a, '  |  Execution on PET # ',      i5.5, / &
            '###' )

  END SUBROUTINE Run_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finalize_
!
! !DESCRIPTION: Finalize_ is the finalize method of the GEOSCHEMchem gridded 
!  component.  GC is a simple ESMF/MAPL wrapper which calls down to the
!  Finalize method of the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Finalize_( GC, Import, Export, Clock, RC )
!
! !USES:
!
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock    ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure?
!
! !REMARKS:
!  We call routine Extract_ to return various values (i.e. grid parameters,
!  start & end dates, PET information, etc.) from the ESMF/MAPL environment.  
!  We then pass those to GEOS-Chem via routine GIGC_CHUNK_FINAL, which is
!  located in GEOS-Chem module ./GEOS-Chem/ESMF/gigc_chunk_mod.F90.
!
! !REVISION HISTORY:
!  01 Dec 2009 - A. Da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now finalize myState%CF and myState
!  15 Apr 2010 - R. Yantosca - Activate call to GC_CHUNK_FINAL
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now set Ident%VERBOSE to FALSE
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root PET
!  08 Mar 2013 - R. Yantosca - Now save the PET # (aka PET #) in Input_Opt
!  15 Mar 2013 - R. Yantosca - Remove IDENT object, which was a holdover from
!                              the GEOS-Chem column code
!  27 Oct 2014 - C. Keller   - Now save species that are not advected into
!                              internal state to ensure they are written into
!                              the restart file.
!  07 Aug 2017 - E. Lundgren - Use species database instead of State_Chm vars 
!                              Spec_Name and Spec_ID
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Objects
    TYPE(ESMF_Grid)            :: Grid        ! ESMF Grid object
    TYPE(ESMF_Config)          :: MaplCF      ! Config (MAPL.rc)
    TYPE(ESMF_Config)          :: GeosCF      ! Config (GEOSCHEM*.rc)
 
    ! Scalars
    LOGICAL                    :: am_I_Root   ! Are we on the root PET?
    CHARACTER(LEN=ESMF_MAXSTR) :: compName    ! Gridded component name
    INTEGER                    :: error       ! GEOS-Chem error code
    INTEGER                    :: myPet       ! # of PET we are on now
    INTEGER                    :: I,  J,  L   ! Loop indices
    INTEGER                    :: IM          ! # of longitudes on this PET
    INTEGER                    :: JM          ! # of latitudes on this PET
    INTEGER                    :: LM          ! # of levels on this PET
    REAL                       :: UTC         ! UTC time [hours]
    
    ! Pointers
    TYPE(MAPL_MetaComp), POINTER :: STATE
    TYPE(Species),       POINTER :: ThisSpc

    ! For species copying
    INTEGER                     :: IND
    TYPE(ESMF_STATE)            :: INTSTATE
    REAL, POINTER               :: Ptr2d(:,:)   => NULL()
    REAL, POINTER               :: Ptr3d(:,:,:) => NULL()
    REAL(ESMF_KIND_R8), POINTER :: Ptr3d_R8(:,:,:) => NULL()

    __Iam__('Finalize_')

    !=======================================================================
    ! Initialization
    !=======================================================================

    ! Are we on the root PET
    am_I_Root = MAPL_Am_I_Root()

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Finalize_'

    ! Get my MAPL_Generic state
    ! -------------------------
    CALL MAPL_GetObjectFromGC(GC, STATE, RC=STATUS)
    VERIFY_(STATUS)

    ! Start timers
    ! ------------
!    CALL MAPL_TimerOn(STATE, "TOTAL")
!    CALL MAPL_TimerOn(STATE, "FINALIZE")

    ! Get pointers to fields in import, internal, and export states
!#   include "GIGCchem_GetPointer___.h"

    ! Re-open file for stdout redirect
    IF ( am_I_Root )  THEN
       OPEN ( UNIT = logLun, FILE   = TRIM( logFile ), STATUS = 'OLD' ,   &
                             ACTION = 'WRITE',         ACCESS = 'APPEND' )
    ENDIF

    ! Get various parameters from the ESMF/MAPL framework
    CALL Extract_( GC,                 &    ! Ref to this Gridded Component
                   Clock,              &    ! ESMF Clock object
                   Grid     = Grid,    &    ! ESMF Grid object
                   MaplCF   = MaplCF,  &    ! ESMF Config obj (MAPL.rc)
                   GeosCF   = GeosCF,  &    ! ESMF Config obj (GEOSCHEM*.rc)
                   IM       = IM,      &    ! # of longitudes on this PET
                   JM       = JM,      &    ! # of latitudes  on this PET
                   LM       = LM,      &    ! # of levels     on this PET
                   utc      = utc,     &    ! Universal time [hours]
                   localPET = myPet,   &    ! PET # we are on now 
                   __RC__ )

    !=========================================================================
    ! Archive species in internal state. Do this only for species that are
    ! not advected, since those need to be included in the restart file.
    ! ckeller, 10/27/2014
    ! Also archive certain State_Chm arrays in internal state (ewl, 12/13/18)
    !=========================================================================

    ! Get Internal state
    CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ ) 

    ! Loop over all species
    DO I = 1, State_Chm%nSpecies
 
       ! Get info about this species from the species database
       ThisSpc => State_Chm%SpcData(I)%Info

       ! Skip if empty
       IF ( TRIM(ThisSpc%Name) == '' ) CYCLE

       ! Is this a tracer?
       IND = IND_( TRIM(ThisSpc%Name) )
       IF ( IND >= 0 ) CYCLE

       ! Get data from internal state and copy to species array
       CALL MAPL_GetPointer( INTSTATE, Ptr3D_R8, TRIM(ThisSpc%Name), &
                             notFoundOK=.TRUE., __RC__ )
       IF ( .NOT. ASSOCIATED(Ptr3D_R8) ) CYCLE
       Ptr3D_R8 = State_Chm%Species(:,:,LM:1:-1,IND)
       Ptr3D_R8 => NULL()

       ! Verbose 
       if ( MAPL_am_I_Root()) write(*,*)                &
                'Species written to INTERNAL state: ',  &
                TRIM(ThisSpc%Name)
    ENDDO

    CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'H2O2AfterChem',  &
                           notFoundOK=.TRUE., __RC__ ) 
    IF ( ASSOCIATED(Ptr3d) .AND. &
         ASSOCIATED(State_Chm%H2O2AfterChem) ) THEN
       Ptr3d(:,:,LM:1:-1) = State_Chm%H2O2AfterChem(:,:,LM:1:-1)
    ENDIF
    Ptr3d => NULL()
    
    CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'SO2AfterChem',   &
                          notFoundOK=.TRUE., __RC__ ) 
    IF ( ASSOCIATED(Ptr3d) .AND. &
         ASSOCIATED(State_Chm%SO2AfterChem) ) THEN
       Ptr3d(:,:,LM:1:-1) = State_Chm%SO2AfterChem
    ENDIF
    Ptr3d => NULL()
    
    CALL MAPL_GetPointer( INTSTATE, Ptr2d, 'DryDepNitrogen', &
                          notFoundOK=.TRUE., __RC__ ) 
    IF ( ASSOCIATED(Ptr2d) .AND. &
         ASSOCIATED(State_Chm%DryDepNitrogen) ) THEN
       Ptr2d = State_Chm%DryDepNitrogen
    ENDIF
    Ptr2d => NULL()
    
    CALL MAPL_GetPointer( INTSTATE, Ptr2d, 'WetDepNitrogen', &
                          notFoundOK=.TRUE., __RC__ ) 
    IF ( ASSOCIATED(Ptr2d) .AND. &
         ASSOCIATED(State_Chm%WetDepNitrogen) ) THEN
       Ptr2d = State_Chm%WetDepNitrogen
    ENDIF
    Ptr2d => NULL()
    
    CALL MAPL_GetPointer( INTSTATE, Ptr3d, 'KPPHvalue' ,     &
                          notFoundOK=.TRUE., __RC__ ) 
    IF ( ASSOCIATED(Ptr3d) .AND. &
         ASSOCIATED(State_Chm%KPPHvalue) ) THEN
       Ptr3d(:,:,1:LM-LLCHEM) = 0.0
       Ptr3d(:,:,LM:LM-LLCHEM+1:-1) = &
          REAL(State_Chm%KPPHvalue(:,:,1:LLCHEM),KIND=ESMF_KIND_R4)
    ENDIF

    !=======================================================================
    ! Print end-of-simulation output
    !=======================================================================

    ! Print the mean OH lifetime
!    CALL Print_Mean_OH( GC, logLun, D_AIR_MASS, D_OH_MASS, __RC__ )

    !=======================================================================
    ! Finalize the Gridded Component
    !=======================================================================

    ! Save the PET # (aka CPU #) in Input_Opt
    Input_Opt%myCpu = myPet

    ! Call the FINALIZE method of the GEOS-Chem column chemistry code
    CALL GIGC_Chunk_Final( am_I_Root  = am_I_Root,   &   ! Is this the root PET?
                           Input_Opt  = Input_Opt,   &   ! Input Options
                           State_Chm  = State_Chm,   &   ! Chemistry State
                           State_Met  = State_Met,   &   ! Meteorology State
                           State_Diag = State_Diag,  &   ! Diagnostics State
                           __RC__                   )

    ! Free Int2Chm pointer
    IF ( ASSOCIATED(Int2Chm) ) THEN
       DO I=1,SIZE(Int2Chm,1)
          Int2Chm(I)%Internal => NULL()
       ENDDO
       DEALLOCATE(Int2Chm)
    ENDIF

    ! Deallocate the history interface between GC States and ESMF Exports
    CALL Destroy_HistoryConfig( am_I_Root, HistoryConfig, RC )

    ! Deallocate provide pointers and arrays
    CALL Provider_Finalize( am_I_Root, __RC__ )

    PTR_ARCHIVED_PFI_CN  => NULL()
    PTR_ARCHIVED_PFL_CN  => NULL()
    PTR_ARCHIVED_CNV_MFC => NULL()
    PTR_ARCHIVED_CNV_MFD => NULL()
    PTR_ARCHIVED_CNV_CVW => NULL()
    PTR_ARCHIVED_DQRC    => NULL()
    PTR_ARCHIVED_REV_CN  => NULL()
    PTR_ARCHIVED_T       => NULL()

    ! Finalize MAPL Generic
    CALL MAPL_GenericFinalize( GC, Import, Export, Clock, __RC__ )

    !=======================================================================
    ! All done
    !=======================================================================

    ! Add descriptive footer text
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(/,a)' ) REPEAT( '#', 79 )
       WRITE( logLun, 100     ) TRIM( Iam ), myPet
       WRITE( logLun, '(a)'   ) REPEAT( '#', 79 )
    ENDIF
       
    ! Formats
100 FORMAT( '###',                                     /, &
            '### ', a, '  |  Cleanup on PET # ', i5.5, /  &
            '###' )

    ! Close file
    CLOSE( logLun )

    ! Stop timers
    ! -----------
!    CALL MAPL_TimerOff(STATE, "FINALIZE")
!    CALL MAPL_TimerOff(STATE, "TOTAL")

    ! Successful return
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Finalize_
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
  SUBROUTINE Extract_( GC,         Clock,    Grid,    MaplCF, GeosCF,    &
                       localPet,   petCount, I_LO,    J_LO,   I_HI,      &
                       J_HI,       IM,       JM,      LM,     IM_WORLD,  &
                       JM_WORLD,   LM_WORLD, lonCtr,  latCtr, advCount,  &
                       nymdB,      nymdE,    nymd,    nhmsB,  nhmsE,     &
                       nhms,       year,     month,   day,    dayOfYr,   &
                       hour,       minute,   second,  utc,    hElapsed,  &
                       tsChem,     tsDyn,    mpiComm, ZTH,   SLR,        &
                       haveImpRst, RC )
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
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: MaplCF      ! AGCM.rc
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: GeosCF      ! GEOSCHEM*.rc
    INTEGER,             INTENT(OUT), OPTIONAL :: localPet    ! This PET
    INTEGER,             INTENT(OUT), OPTIONAL :: petCount    ! Total # of PETs
    INTEGER,             INTENT(OUT), OPTIONAL :: mpiComm     ! MPI Comm Handle

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
                                                              
    !----------------------------------
    ! Date and time variables
    !----------------------------------
   INTEGER(ESMF_KIND_I8),INTENT(OUT), OPTIONAL :: advCount    ! # of clock advs
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdB       ! YYYYMMDD @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdE       ! YYYYMMDD @ end
    INTEGER,             INTENT(OUT), OPTIONAL :: nymd        ! YYYYMMDD now
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsB       ! hhmmss @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsE       ! hhmmss @ end
    INTEGER,             INTENT(OUT), OPTIONAL :: nhms        ! hhmmss now
    INTEGER,             INTENT(OUT), OPTIONAL :: year        ! UTC year 
    INTEGER,             INTENT(OUT), OPTIONAL :: month       ! UTC month
    INTEGER,             INTENT(OUT), OPTIONAL :: day         ! UTC day
    INTEGER,             INTENT(OUT), OPTIONAL :: dayOfYr     ! UTC day of year
    INTEGER,             INTENT(OUT), OPTIONAL :: hour        ! UTC hour
    INTEGER,             INTENT(OUT), OPTIONAL :: minute      ! UTC minute
    INTEGER,             INTENT(OUT), OPTIONAL :: second      ! UTC second
    REAL,                INTENT(OUT), OPTIONAL :: utc         ! UTC time [hrs]
    REAL,                INTENT(OUT), OPTIONAL :: hElapsed    ! Elapsed hours

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

    !-----------------------------------------------
    ! Optional import restart file existence inquiry
    !-----------------------------------------------
    LOGICAL,             INTENT(OUT), OPTIONAL :: haveImpRst ! Import rst exist? 

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
!  05 Jan 2016 - S. D. Eastham - Fixed order of time calls
!  02 Nov 2017 - E. Lundgren - Replace use of local GridGetInterior with 
!                              call to MAPL_GridGetInterior (now public)
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    ! Objects
    TYPE(ESMF_Time)               :: startTime      ! ESMF start time obj
    TYPE(ESMF_Time)               :: stopTime       ! ESMF stop time obj
    TYPE(ESMF_Time)               :: currTime       ! ESMF current time obj
    TYPE(ESMF_TimeInterval)       :: elapsedTime    ! ESMF elapsed time obj
    TYPE(ESMF_TimeInterval)       :: chemInterval   ! chemistry interval
    TYPE(ESMF_ALARM)              :: ALARM          ! Run alarm 
    TYPE(ESMF_VM)                 :: VM             ! ESMF VM object
    TYPE(GEOSCHEM_State), POINTER :: myState        ! Legacy state
    TYPE(GEOSCHEM_Wrap)           :: wrap           ! Wrapper for myState
    TYPE(MAPL_MetaComp),  POINTER :: STATE          ! MAPL MetaComp object
    TYPE(MAPL_SunOrbit)           :: sunOrbit

    ! Scalars
    CHARACTER(len=ESMF_MAXSTR)    :: compName       ! Gridded component name
    CHARACTER(len=ESMF_MAXSTR)    :: importRstFN    ! Import restart file name
    INTEGER(ESMF_KIND_I8)         :: count          ! # of clock advances
    INTEGER                       :: locDims(3)     ! Array for local dims
    INTEGER                       :: globDims(3)    ! Array for global dims
    INTEGER                       :: doy            ! Day of year (0-365/366)
    INTEGER                       :: yyyy, mm, dd   ! Year, month, day
    INTEGER                       :: h,    m,  s    ! Hour, minute, seconds
    INTEGER                       :: IL,   IU       ! Min/max local lon indices
    INTEGER                       :: JL,   JU       ! Min/max local lat indices
    REAL                          :: elapsedHours   ! Elapsed hours of run
    REAL(ESMF_KIND_R8)            :: dt_r8          ! chemistry timestep

    __Iam__('Extract_')

    !=======================================================================
    ! Initialization
    !=======================================================================

    ! Get my name and set-up traceback handle
    CALL ESMF_GridCompGet( GC, name=compName, vm=VM, __RC__ )
    Iam = TRIM(compName)//'::Extract_'

    ! Get the internal state which holds the private Config object
    CALL ESMF_UserCompGetInternalState( GC, 'GEOSCHEM_State', wrap, STATUS )
    VERIFY_(STATUS)
    myState => wrap%ptr

    ! Get generic state object
    CALL MAPL_GetObjectFromGC( GC, STATE, __RC__ )

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

    ! Get the Config object 
    CALL ESMF_GridCompGet( GC, Config=MaplCF, __RC__ )
    
    ! Get the Config object based on "GEOSCHEMchem_GridComp.rc"
    GeosCF = myState%myCF

    ! Dynamic timestep (in seconds)
    IF ( PRESENT( tsDyn ) ) THEN
       CALL ESMF_ConfigGetAttribute( MaplCF, tsDyn, Default=1800.,        &
                                     Label="RUN_DT:",             __RC__ )
    ENDIF

    ! Chemistry timestep (in seconds)
    IF ( PRESENT( tsChem ) ) THEN
        !CALL ESMF_ConfigGetAttribute ( MaplCF, tsChem, &
        !                   LABEL="GIGCchem_DT:", DEFAULT=tsDyn, __RC__ )
        CALL MAPL_Get( STATE, RUNALARM=ALARM, __RC__ ) 
        CALL ESMF_AlarmGet( ALARM, RingInterval=chemInterval, __RC__ )
        CALL ESMF_TimeIntervalGet( chemInterval, s_r8=dt_r8, __RC__ )
        tsChem = real(dt_r8)

        IF(tsChem < tsDyn) THEN
         IF(MAPL_AM_I_ROOT()) Write(6,*) 'Chem_DT cannot be less than RUN_DT'
         STATUS = 1
         VERIFY_(STATUS)
        ENDIF
    ENDIF

    !=======================================================================
    ! Extract time/date information
    !=======================================================================
    
    ! Get the ESMF time object
    CALL ESMF_ClockGet( Clock,                    &
                        startTime    = startTime, &
                        stopTime     = stopTime,  &
                        currTime     = currTime,  &
                        advanceCount = count,     &
                         __RC__ )

    ! Get starting-time fields from the time object
    CALL ESMF_TimeGet( startTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save packed fields for return
    IF ( PRESENT( nymdB    ) ) CALL MAPL_PackTime( nymdB, yyyy, mm, dd )
    IF ( PRESENT( nhmsB    ) ) CALL MAPL_PackTime( nhmsB, h,    m,  s  )

    ! Get ending-time fields from the time object
    CALL ESMF_TimeGet( stopTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save packed fields for return
    IF ( PRESENT( nymdE    ) ) CALL MAPL_PackTime( nymdE, yyyy, mm, dd )
    IF ( PRESENT( nhmsE    ) ) CALL MAPL_PackTime( nhmsE, h,    m,  s  )

    IF ( PRESENT( advCount ) ) advCount = count
 
    !=======================================================================
    ! SDE 2017-01-05: The following calls must be kept as a single block,
    ! or the wrong date/time elements will be returned (the yyyy/mm/dd    
    ! etc variables are re-used). Specifically, the output variables must
    ! be set now, before the variables are re-used.
    !=======================================================================
    ! Start of current-time block
    !=======================================================================
    ! Get current-time fields from the time object
    CALL ESMF_TimeGet( currTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save packed fields for return
    IF ( PRESENT( nymd     ) ) CALL MAPL_PackTime( nymd, yyyy, mm, dd )
    IF ( PRESENT( nhms     ) ) CALL MAPL_PackTime( nhms, h,    m,  s  )

    ! Save the various extacted current-time fields for return
    IF ( PRESENT( year     ) ) year     = yyyy
    IF ( PRESENT( month    ) ) month    = mm
    IF ( PRESENT( day      ) ) day      = dd
    IF ( PRESENT( dayOfYr  ) ) dayOfYr  = doy
    IF ( PRESENT( hour     ) ) hour     = h
    IF ( PRESENT( minute   ) ) minute   = m
    IF ( PRESENT( second   ) ) second   = s
    IF ( PRESENT( utc      ) ) utc      = ( DBLE( h )        ) + & 
                                          ( DBLE( m )/60d0   ) + &
                                          ( DBLE( s )/3600d0 )
    !=======================================================================
    ! End of current-time block
    !=======================================================================

    CALL ESMF_TimeGet( startTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save fields for return
    IF ( PRESENT( nymdB    ) ) CALL MAPL_PackTime( nymdB, yyyy, mm, dd )
    IF ( PRESENT( nhmsB    ) ) CALL MAPL_PackTime( nhmsB, h,    m,  s  )

    CALL ESMF_TimeGet( stopTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save fields for return
    IF ( PRESENT( nymdE    ) ) CALL MAPL_PackTime( nymdE, yyyy, mm, dd )
    IF ( PRESENT( nhmsE    ) ) CALL MAPL_PackTime( nhmsE, h,    m,  s  )

    IF ( PRESENT( advCount ) ) advCount = count

    ! Compute elapsed time since start of simulation
    elapsedTime = currTime - startTime

    ! Get time fields from the elapsedTime object
    CALL ESMF_TimeIntervalGet( elapsedTime, h=h, m=m, s=s, __RC__ )

    ! Convert to decimal hours
    elapsedHours = DBLE( h ) + ( DBLE( m )/60d0 ) + ( DBLE( s )/3600d0 )
    
    ! Save fields for return
    IF ( PRESENT( hElapsed ) ) hElapsed = elapsedHours

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
       CALL MAPL_GridGetInterior( Grid, IL, IU, JL, JU )

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

    ! Longitude values on this PET
    IF ( PRESENT( lonCtr ) ) THEN
       CALL MAPL_Get( STATE, lons=lonCtr, __RC__ )
    ENDIF

    ! Latitude values on this PET
    IF ( PRESENT( latCtr ) ) THEN
       CALL MAPL_Get( STATE, lats=latCtr, __RC__ )
    ENDIF

    !=======================================================================
    ! Get solar zenith angle enformation
    !=======================================================================
    IF ( PRESENT( ZTH    ) .and. PRESENT( SLR    )  .and. &
         PRESENT( lonCtr ) .and. PRESENT( latCtr ) ) THEN
         
       ! Get the Orbit object (of type MAPL_SunOrbit),
       ! which is used in the call to MAPL_SunGetInsolation
       CALL MAPL_Get( STATE,                       &
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
END MODULE Chem_GridCompMod
 
