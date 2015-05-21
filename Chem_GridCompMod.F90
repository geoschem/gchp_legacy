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
! This gridded component contains two run phases: Phase 1 executes
! dry deposition and emissions and should be called before surface 
! processes/turbulence. Phase 2 performs chemistry and wet deposition,
! and should be called after turbulence. Convection and PBL mixing are
! done in phase 1 and phase 2, respectively (if selected).
!\\
!\\
! GEOS-Chem includes species CH4, N2O, CFC-11, CFC-12, and HCFC-22
! and thus can serve as the provider of the radiatively active tracers 
! (RATs) needed by the radiation component. If GEOS-Chem is set as the
! RATS provider, all required quantities (i.e. the five species listed 
! above plus H2O_TEND) become automatically added to the export state.
! Similarly, the quantities O3, OX, O3PPMV, and OX_TEND become calcu-
! lated if GEOS-Chem is the analysis OX provider, and an AERO and 
! AERO_DP bundle is created and filled if GEOS-Chem is the AERO 
! provider. The providers are specified in the model configuration file 
! (AGCM.rc).
!\\
!\\
! The AERO bundle is filled with the four GEOS-Chem dust tracers (DST1
! to DST4), accumulation and coarse sea salt aerosol (SALA, SALC), SO4, 
! as well as hydrophilic and hydrophobic organic and black carbon (BCPI,
! BCPO, OCPI, OCPO). The corresponding names assigned to the AERO bundle 
! are defined below.
! Currently, the AERO_DP bundle is created but not filled.
!\\
!\\
! All provider quantities are added to the export state and updated
! after every run call. If GEOS-Chem is the analysis OX provider, the
! GEOS-Chem OX export is added to the TRANA bundle in GEOS_ChemGridComp.F90.
!\\
!\\
! !INTERFACE:
!
MODULE Chem_GridCompMod
!
! !USES:
!
  USE ESMF                                           ! ESMF library
  USE MAPL_Mod                                       ! MAPL library
  USE Charpak_Mod                                    ! String functions
  USE GIGC_Type_Mod                                  ! Derived type defs
  USE GIGC_MPI_Wrap, ONLY : mpiComm
  USE GIGC_Chem_Utils                                ! Functions
  USE GIGC_Chunk_Mod                                 ! GIGC IRF methods
  USE GIGC_ErrCode_Mod                               ! Error numbers
  USE GIGC_Input_Opt_Mod                             ! Input Options obj
  USE GIGC_State_Chm_Mod                             ! Chemistry State obj
  USE GIGC_State_Met_Mod                             ! Meteorology State obj

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC                           :: SetServices    ! Sets ESMF entry points
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE                          :: Initialize_    ! Init method
  PRIVATE                          :: Run1           ! Run wrapper phase 1
  PRIVATE                          :: Run2           ! Run wrapper phase 2
  PRIVATE                          :: Run_           ! Run method
  PRIVATE                          :: Finalize_      ! Finalize method
  PRIVATE                          :: Extract_       ! Get values from ESMF
  PRIVATE                          :: Roundoff       ! Truncates a number
  PRIVATE                          :: Print_Mean_OH  ! Mean OH lifetime
  PRIVATE                          :: GlobalSum      ! Sums across PETs
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
  TYPE Int2ChmMap
     CHARACTER(LEN=255)            :: TrcName
     INTEGER                       :: TrcID
     REAL, POINTER                 :: Internal(:,:,:) => NULL()
  END TYPE Int2ChmMap

  ! For mapping internal state
  TYPE(Int2ChmMap), POINTER        :: Int2Chm(:) => NULL()

  ! Objects for GEOS-Chem
  TYPE(Spec_2_Trac)                :: Coef           ! Species <-> tracer map  
  TYPE(OptInput)                   :: Input_Opt      ! Input Options
  TYPE(MetState)                   :: State_Met      ! Meteorology state
  TYPE(ChmState)                   :: State_Chm      ! Chemistry state

  ! Scalars
  INTEGER                          :: logLun         ! LUN for stdout logfile
  CHARACTER(LEN=ESMF_MAXSTR)       :: logFile        ! File for stdout redirect

  ! Is GEOS-Chem the provider for AERO, RATS, and/or Analysis OX? 
  LOGICAL                          :: DoAERO
  LOGICAL                          :: DoRATS
  LOGICAL                          :: DoANOX

  ! Is this being run as a CTM?
  INTEGER                          :: IsCTM

  ! Number of run phases. Can be set in the resource file (default is 2).
  INTEGER                          :: NPHASE

  ! List here GEOS-Chem tracer names and corresponding names to be assigned
  ! to the AERO bundle (if GC is the AERO provider). The names in the AERO
  ! bundle must be the names that are expected by the irradiation component:
  ! - OCphobic, OCphilic, BCphobic, and BCphilic for hydrophobic and hydrophilic
  !   organic and black carbon, respectively
  ! - SO4 for SO4
  ! - du001 - du005 for the following five dust bins (see DU_GridComp.rc in
  !   GOCART):
  !   radius_lower: 0.1 1.0 1.8 3.0 6.0
  !   radius_upper: 1.0 1.8 3.0 6.0 10.0
  !
  !   The GEOS-Chem dust bins are: 
  !   Reff: 0.7 1.4 2.4 4.5
  !   Those become simply mapped onto the GOCART dust bins 1-4 (du001 ... du004).
  !
  ! - ss001-ss005 for the following five sea salt aerosol bins (see SS_GridComp.rc
  !   in GOCART):
  !   radius_lower: 0.03 0.1 0.5 1.5 5.0
  !   radius_upper: 0.1  0.5 1.5 5.0 10.0
  !
  !   The GEOS-Chem sea salt aerosols are (SALA and SALC):
  !   radius_lower: 0.01 0.5
  !   radius_upper: 0.5  8.0
  !   SALA becomes mapped onto ss001 and ss002, and SALC onto ss003, ss004, ss005. 
  !   For now, we assume uniform size distribution within the GEOS-Chem bins, i.e.
  !   the GEOS-Chem size bins are evenly split into the GOCART bins. The fractions can
  !   be specified below.
  !   At some point, we may revisit these fractions (at least take into account the
  !   log-normal behavior of the aerosol distribution)
  INTEGER, PARAMETER           :: NumAERO = 11
  CHARACTER(LEN=ESMF_MAXSTR)   :: GcNames(NumAero) = &
                                  (/ 'DST1',     'DST2',     'DST3',     'DST4',     &
                                     'SALA',     'SALC',     'BCPO',     'BCPI',     &
                                     'OCPO',     'OCPI',     'SO4 '                   /)

  CHARACTER(LEN=ESMF_MAXSTR)   :: AeroNames(NumAero) = &
                                  (/ 'du001   ', 'du002   ', 'du003   ', 'du004   ', &
                                     'ss001   ', 'ss003   ', 'BCphobic', 'BCphilic', &
                                     'OCphobic', 'OCphilic', 'SO4     '               /)

  ! Fraction of SALA in ss001 and ss002, respectively
  CHARACTER(LEN=ESMF_MAXSTR)   :: SALAnames(2) = (/ 'ss001', 'ss002' /)
  REAL, PARAMETER              :: SALAsplit(2) = (/  0.2,     0.8    /)
  ! Fraction of SALC in ss003, ss004, and ss005.
  CHARACTER(LEN=ESMF_MAXSTR)   :: SALCnames(3) = (/ 'ss003', 'ss004' , 'ss005' /)
  REAL, PARAMETER              :: SALCsplit(3) = (/  0.13,    0.47,     0.4    /) 

  ! Prefix of the tracer and species names in the internal state. Those have to match
  ! the prefixes given in GEOSCHEMchem_Registry.rc. 
  CHARACTER(LEN=4), PARAMETER  :: TPFX = 'TRC_'
  CHARACTER(LEN=4), PARAMETER  :: SPFX = 'SPC_'
 
  ! Pointers to import, export and internal state data. Declare them as 
  ! module variables so that we have to assign them only on first call.
# include "GIGCchem_DeclarePointer___.h"

  ! Pointers for RATS and analysis OX. Those are not included in the GEOS-Chem
  ! registry and only filled if GEOS-Chem is the RATS and/or analysis OX provider.
  ! The history arrays (*_HIST) are used to archive the O3 and H2O fields from the
  ! previous chemistry time step.

  ! -Analysis OX:
  REAL, POINTER     :: O3      (:,:,:) => NULL()
  REAL, POINTER     :: O3PPMV  (:,:,:) => NULL()
  REAL, POINTER     :: OX      (:,:,:) => NULL()
  REAL, POINTER     :: OX_TEND (:,:,:) => NULL()
  REAL, POINTER     :: O3_HIST (:,:,:) => NULL()

  ! -RATS:
  REAL, POINTER     :: CH4     (:,:,:) => NULL()
  REAL, POINTER     :: N2O     (:,:,:) => NULL()
  REAL, POINTER     :: CFC11   (:,:,:) => NULL()
  REAL, POINTER     :: CFC12   (:,:,:) => NULL()
  REAL, POINTER     :: HCFC22  (:,:,:) => NULL()
  REAL, POINTER     :: H2O_TEND(:,:,:) => NULL()
  REAL, POINTER     :: H2O_HIST(:,:,:) => NULL()

  ! -Corresponding pointers to internal state. We now use these variables instead
  !  of the auto-generated pointers (GIGCchem_DeclarePointer___.h) to avoid 
  !  compilation errors if these species are not defined in GEOS-Chem (e.g. for
  !  specialty sims). 
  REAL, POINTER     :: PTR_O3      (:,:,:) => NULL()
  REAL, POINTER     :: PTR_CH4     (:,:,:) => NULL()
  REAL, POINTER     :: PTR_N2O     (:,:,:) => NULL()
  REAL, POINTER     :: PTR_CFC11   (:,:,:) => NULL()
  REAL, POINTER     :: PTR_CFC12   (:,:,:) => NULL()
  REAL, POINTER     :: PTR_HCFC22  (:,:,:) => NULL()
  REAL, POINTER     :: PTR_H2O     (:,:,:) => NULL()

  ! GCCTO3 and GCCTTO3 are the pointers to the corresponding export state fields
  REAL, POINTER     :: PTR_GCCTO3 (:,:) => NULL()
  REAL, POINTER     :: PTR_GCCTTO3(:,:) => NULL()

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

!
! !DEFINED PARAMETERS:
!
  ! Scale factor to prevent underflow in mean OH diagnostic
  REAL*8, PARAMETER                :: OH_SCALE = 1d20
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
!       (phase 1 and phase 2).
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
    USE HCOI_ESMF_MOD, ONLY : HCO_SetServices
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp),  INTENT(INOUT) :: GC       ! Ref to this GridComp
!
! !OUTPUT PARAMETERS:
!
    INTEGER,              INTENT(OUT)   :: RC       ! Success or failure
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
    CHARACTER(LEN=ESMF_MAXSTR)    :: ProviderName  ! Provider name
    CHARACTER(LEN=ESMF_MAXSTR)    :: compName      ! Gridded Component name
    CHARACTER(LEN=ESMF_MAXSTR)    :: COMP_NAME     ! This syntax for mapl_acg.pl
    CHARACTER(LEN=ESMF_MAXSTR)    :: HcoConfigFile ! HEMCO configuration file
    INTEGER                       :: I

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
    call ESMF_ConfigLoadFile( myState%myCF, 'GIGC_GridComp.rc', __RC__)

    ! Get generic state object
    CALL MAPL_GetObjectFromGC( GC, STATE, __RC__ )
    call MAPL_GetResource( STATE, IsCTM, label='GEOSChem_CTM:', default=1, rc=status )
    VERIFY_(STATUS)

    !=======================================================================
    !                 %%% ESMF Functional Services %%%
    !=======================================================================

    ! Set the Initialize, Run, Finalize entry points
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_INITIALIZE,  Initialize_, __RC__ )
    !IF (.not. IsCTM) &
!    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_RUN,    Run1,        __RC__ )
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_RUN,         Run2,        __RC__ )
    CALL MAPL_GridCompSetEntryPoint( GC, ESMF_METHOD_FINALIZE,    Finalize_,   __RC__ )
        
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

!
! !INTERNAL STATE:
!
#   include "GIGCchem_InternalSpec___.h"
!
! !EXTERNAL STATE:
!
#   include "GIGCchem_ExportSpec___.h"

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
    !                    %%% Add provider services %%%
    !=======================================================================

    ! Check if GEOS-Chem is set as the AERO and/or RATS provider 
    ! ----------------------------------------------------------

    ! Get configuration
    CALL ESMF_GridCompGet( GC, CONFIG = CF, __RC__ )

    ! See if GC is the AERO provider
    DoAERO = .FALSE.
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,       &
                                  Label="AERO_PROVIDER:", &
                                  Default="PCHEM",        &
                                  __RC__                   )
    IF ( ProviderName == "GIGCchem" ) DoAERO = .TRUE.
 
    ! See if GC is the RATS provider
    DoRATS = .FALSE.
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,       &
                                  Label="RATS_PROVIDER:", &
                                  Default="PCHEM",        &
                                  __RC__                   )
    IF ( ProviderName == "GIGCchem" ) DoRATS = .TRUE.

    ! See if GC is the Analysis OX provider
    DoANOX = .FALSE.
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,              &
                                  Label="ANALYSIS_OX_PROVIDER:", &
                                  Default="PCHEM",               &
                                  __RC__                          )
    IF ( ProviderName == "GIGCchem" ) DoANOX = .TRUE.

    ! Add AERO and AERO_DP bundles to export state if GEOS-Chem is the 
    ! AERO provider
    ! ----------------------------------------------------------------
    IF ( DoAERO ) THEN
      
       ! The AERO bundle contains DUST, SALT, SO4, BC, and OC.
       ! These quantities will be obtained from the respective
       ! GEOS-Chem internal state quantities. 
       ! Fields are added to bundle in the initialize routine.
       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME         = 'AERO',                              &
          LONG_NAME          = 'aerosol_mass_mixing_ratios',        &
          UNITS              = 'kg kg-1',                           &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
          DATATYPE           = MAPL_BundleItem,                     &
                                                            __RC__ )
       ! This bundle is needed by surface for snow albedo modification.
       ! At the moment, it is not filled by GEOS-Chem.
       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME         = 'AERO_DP',                           &
          LONG_NAME          = 'aerosol_deposition',                &
          UNITS              = 'kg m-2 s-1',                        &
          DIMS               = MAPL_DimsHorzOnly,                   &
          DATATYPE           = MAPL_BundleItem,                     &
                                                            __RC__ )
      
       ! Fields of AERO_DP bundle:
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUDP_DST1',                &
          LONG_NAME          = 'dust1_dry_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUDP_DST2',                &
          LONG_NAME          = 'dust2_dry_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUDP_DST3',                &
          LONG_NAME          = 'dust3_dry_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUDP_DST4',                &
          LONG_NAME          = 'dust4_dry_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUWT_DST1',                &
          LONG_NAME          = 'dust1_wet_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUWT_DST2',                &
          LONG_NAME          = 'dust2_wet_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUWT_DST3',                &
          LONG_NAME          = 'dust3_wet_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'DUWT_DST4',                &
          LONG_NAME          = 'dust4_wet_depostion',      &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'BCDP_BCPI',                &
          LONG_NAME          = 'BCPI_dry_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'BCDP_BCPO',                &
          LONG_NAME          = 'BCPO_dry_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'BCWT_BCPI',                &
          LONG_NAME          = 'BCPI_wet_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'BCWT_BCPO',                &
          LONG_NAME          = 'BCPO_wet_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
 
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OCDP_OCPI',                &
          LONG_NAME          = 'OCPI_dry_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OCDP_OCPO',                &
          LONG_NAME          = 'OCPO_dry_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OCWT_OCPI',                &
          LONG_NAME          = 'OCPI_wet_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )

       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OCWT_OCPO',                &
          LONG_NAME          = 'OCPO_wet_depostion',       &
          UNITS              = 'kg m-2 s-1',               &
          DIMS               = MAPL_DimsHorzOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
                                                   __RC__ )

!!! to diagnose fields in AERO bundle
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_OCphobic',            &
          LONG_NAME          = 'AERO_OCphobic',            &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_OCphilic',            &
          LONG_NAME          = 'AERO_OCphilic',            &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_BCphobic',            &
          LONG_NAME          = 'AERO_BCphobic',            &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_BCphilic',            &
          LONG_NAME          = 'AERO_BCphilic',            &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_SO4',                 &
          LONG_NAME          = 'AERO_SO4',                 &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_du001',               &
          LONG_NAME          = 'AERO_du001',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_du002',               &
          LONG_NAME          = 'AERO_du002',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_du003',               &
          LONG_NAME          = 'AERO_du003',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_du004',               &
          LONG_NAME          = 'AERO_du004',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
        
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_du005',               &
          LONG_NAME          = 'AERO_du005',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_ss001',               &
          LONG_NAME          = 'AERO_ss001',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_ss002',               &
          LONG_NAME          = 'AERO_ss002',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_ss003',               &
          LONG_NAME          = 'AERO_ss003',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_ss004',               &
          LONG_NAME          = 'AERO_ss004',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'AERO_ss005',               &
          LONG_NAME          = 'AERO_ss005',               &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
!!!

    ENDIF ! DoAERO

    ! If GEOS-Chem is the Analysis OX provider, we need to make sure that
    ! all required quantities are exported.
    ! 
    ! Important: the OX field is expected to be part of the TRANA bundle,
    ! defined in GEOS_ChemGridComp.F90. Most chemistry children keep the
    ! OX field in the internal state and make it frienly to ANALYSIS.
    ! Here, we define it as export quantity. It will be added to the
    ! TRANA bundle in GEOS_ChemGridComp.F90.
    ! ------------------------------------------------------------------
    IF ( DoANOX ) THEN
      
       ! Add to export state
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OX',                       &
          LONG_NAME          = 'ozone_volume_mixing_ratio',&
          UNITS              = 'mol mol-1',                &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'O3',                       &
          LONG_NAME          = 'ozone_mass_mixing_ratio',  &
          UNITS              = 'kg kg-1',                  &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &  
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'O3PPMV',                   &
          LONG_NAME          = 'ozone_volume_mixing_ratio_in_ppm',  &
          UNITS              = 'ppmv',                     &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )
  
       call MAPL_AddExportSpec(GC,                         &
          SHORT_NAME         = 'OX_TEND',                  &
          LONG_NAME          = 'tendency_of_odd_oxygen_mixing_ratio_due_to_chemistry',  &
          UNITS              = 'mol mol-1 s-1',            &
          DIMS               = MAPL_DimsHorzVert,          &
          VLOCATION          = MAPL_VLocationCenter,       &
                                                   __RC__ )

    ENDIF !AnOx

    ! If GEOS-Chem is the RATS provider, we need to make sure that all 
    ! RATS quantities are available to irradiation. We will get these 
    ! quantities directly from the GEOS-Chem internal state, except for 
    ! H2O_TEND that is calculated explicitly.
    ! Since those fields are just copies of the GEOS-Chem internal
    ! species, we add them as export specs, i.e. no physics is applied
    ! to those fields.
    ! ----------------------------------------------------------------
    IF ( DoRATS ) THEN

       call MAPL_AddExportSpec(GC,                                &
          SHORT_NAME         = 'N2O',                               &
          LONG_NAME          = 'nitrous_oxide_volume_mixing_ratio', &
          UNITS              = 'mol mol-1',                         &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
  
       call MAPL_AddExportSpec(GC,                                &
          SHORT_NAME         = 'CFC11',                             &
          LONG_NAME          = 'CFC11_(CCl3F)_volume_mixing_ratio', &
          UNITS              = 'mol mol-1',                         &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
       
       call MAPL_AddExportSpec(GC,                                &
          SHORT_NAME         = 'CFC12',                             &
          LONG_NAME          = 'CFC12_(CCl2F2)_volume_mixing_ratio',&
          UNITS              = 'mol mol-1',                         &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
  
       call MAPL_AddExportSpec(GC,                                &
          SHORT_NAME         = 'HCFC22',                            &
          LONG_NAME          = 'HCFC22_(CHClF2)_volume_mixing_ratio', &
          UNITS              = 'mol mol-1',                         &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
    
       call MAPL_AddExportSpec(GC,                                &
          SHORT_NAME         = 'CH4',                               &
          LONG_NAME          = 'methane_volume_mixing_ratio',       &
          UNITS              = 'mol mol-1',                         &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
  
       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME         = 'H2O_TEND',                          &
          LONG_NAME          = 'tendency_of_water_vapor_mixing_ratio_due_to_chemistry',  &
          UNITS              = 'kg kg-1 s-1',                       &
          DIMS               = MAPL_DimsHorzVert,                   &
          VLOCATION          = MAPL_VLocationCenter,                &
                                                            __RC__ )
    ENDIF ! DoRATS

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

    ! Set HEMCO services
    ! --------------------
    CALL ESMF_ConfigGetAttribute( myState%myCF, HcoConfigFile, Label="HEMCO_CONFIG:", &
                                  Default="HEMCO_Config.rc", __RC__ )
    CALL HCO_SetServices( MAPL_am_I_Root(), GC, TRIM(HcoConfigFile), __RC__ )    

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
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC       ! Ref. to this GridComp
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
    INTEGER                     :: myPet       ! # of the PET we are on 
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
 
    ! Pointer arrays
    REAL(ESMF_KIND_R4),  POINTER :: lonCtr(:,:) ! Lon centers on this PET [rad]
    REAL(ESMF_KIND_R4),  POINTER :: latCtr(:,:) ! Lat centers on this PET [rad]
    TYPE(MAPL_MetaComp), POINTER :: STATE => NULL()

    ! For AERO
    INTEGER                      :: I, J, NFLDS, GCID
    LOGICAL                      :: FRIENDLY
    REAL                         :: GCMW, FRAC
    REAL, POINTER                :: Ptr3D(:,:,:) => NULL()
    TYPE(ESMF_STATE)             :: INTSTATE 
    TYPE(ESMF_FieldBundle)       :: AeroBdl 
    TYPE(ESMF_Field)             :: AeroFld, GcFld
    CHARACTER(LEN=ESMF_MAXSTR)   :: GCName, AeroName

    ! Working variables
    TYPE(ESMF_Field)            :: field
    TYPE(ESMF_FieldBundle)      :: bundle
    INTEGER                     :: N, trcID, ID_EMIT

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
    CALL Extract_( GC,                        &  ! Reference to this Gridded Comp 
                   Clock,                     &  ! ESMF Clock object
                   Grid        = Grid,        &  ! ESMF Grid object
                   MaplCF      = MaplCF,      &  ! MAPL.rc Configuration object
                   GeosCF      = GeosCF,      &  ! GEOSCHEM*.rc Config object
                   I_LO        = I_LO,        &  ! Min longitude index on this PET
                   J_LO        = J_LO,        &  ! Min latitude  index on this PET
                   I_HI        = I_HI,        &  ! Max longitude index on this PET
                   J_HI        = J_HI,        &  ! Max latitude  index on this PET
                   IM          = IM,          &  ! # of longitudes on this PET
                   JM          = JM,          &  ! # of latitudes  on this PET
                   LM          = LM,          &  ! # of levels     on this PET
                   IM_WORLD    = IM_WORLD,    &  ! # of longitudes in global grid
                   JM_WORLD    = JM_WORLD,    &  ! # of latitudes  in global grid
                   LM_WORLD    = LM_WORLD,    &  ! # of levels     in global grid
                   nymdB       = nymdB,       &  ! YYYYMMDD @ start of simulation
                   nhmsB       = nhmsB,       &  ! hhmmss   @ end   of simulation
                   nymdE       = nymdE,       &  ! YYYMMDD  @ start of simulation
                   nhmsE       = nhmsE,       &  ! hhmmss   @ end   of simulation
                   tsChem      = tsChem,      &  ! Chemistry timestep [seconds]
                   tsDyn       = tsDyn,       &  ! Dynamics timestep  [seconds]
                   localPet    = myPet,       &  ! PET # that we are on now
                   mpiComm     = mpiComm,     &  ! MPI Communicator Handle
                   lonCtr      = lonCtr,      &  ! Lon centers on this PET [radians]
                   latCtr      = latCtr,      &  ! Lat centers on this PET [radians]
		   haveImpRst  = haveImpRst,  &  ! Does import restart exist? 
                   __RC__                      )

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

    ! Max # of diagnostics
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_DIAG,       &
                                  Default = 70,                     &
                                  Label   = "MAX_DIAG:",            &
                                  __RC__                           )    

    ! Max # of tracers
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_TRCS,       &
                                  Default = 100,                    &
                                  Label   = "MAX_TRCS:",            &
                                 __RC__                            )    

    ! Max # of species / family tracer
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_MEMB,       &
                                  Default = 15,                     &
                                  Label   = "MAX_MEMB:",            &
                                  __RC__                           )

    ! Max # of families per ND65 family tracer (not used for ESMF)
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_FAMS,       &
                                  Default = 20,                     &
                                  Label   = "MAX_FAMS:",            & 
                                  __RC__                           )

    ! Max # of families per ND65 family tracer (not used for ESMF)
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_DEP,        &
                                  Default = 100,                    &
                                  Label   = "MAX_DEP:",             &
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

    ! Also save the PET # (aka CPU #) in the Input_Opt object
    Input_Opt%myCpu = myPet

    !=======================================================================
    ! Initialize GEOS-Chem (will also initialize HEMCO)
    !=======================================================================
 
    ! Call the GIGC initialize routine
    CALL GIGC_Chunk_Init( am_I_Root = am_I_Root,  & ! Are we on the root PET?   
                          I_LO      = I_LO,       & ! Min lon index on this PET
                          J_LO      = J_LO,       & ! Min lat index on this PET
                          I_HI      = I_HI,       & ! Max lon index on this PET
                          J_HI      = J_HI,       & ! Max lat index on this PET
                          IM        = IM,         & ! # lons   on this PET
                          JM        = JM,         & ! # lats   on this PET
                          LM        = LM,         & ! # levels on this PET
                          IM_WORLD  = IM_WORLD,   & ! # lons   in global grid
                          JM_WORLD  = JM_WORLD,   & ! # lats   in global grid
                          LM_WORLD  = LM_WORLD,   & ! # levels in global grid
                          nymdB     = nymdB,      & ! YYYYMMDD @ start of run
                          nhmsB     = nhmsB,      & ! hhmmss   @ start of run
                          nymdE     = nymdE,      & ! YYYYMMDD @ end of run
                          nhmsE     = nhmsE,      & ! hhmmss   @ end of run
                          tsChem    = tsChem,     & ! Chemical timestep [s]
                          tsDyn     = tsDyn,      & ! Dynamic  timestep [s]
                          lonCtr    = lonCtr,     & ! Lon centers [radians]
                          latCtr    = latCtr,     & ! Lat centers [radians]
                          Input_Opt = Input_Opt,  & ! Input Options obj
                          State_Met = State_Met,  & ! Meteorology State obj
                          State_Chm = State_Chm,  & ! Chemistry State obj
                          __RC__                 )

    ! It's now save to store haveImpRst flag in Input_Opt
    Input_Opt%haveImpRst = haveImpRst

    !=======================================================================
    ! If GEOS-Chem is the AERO provider, initialize the AERO bundle here.
    ! All GEOS-Chem tracers possibly being added to the AERO bundle are
    ! listed at the beginning of the module. Here, we see which ones of 
    ! those are effectively defined and create a field in the bundle for
    ! them. The AERO names are given the names listed at the beginning of
    ! the module.
    ! GEOS-Chem tracers are in mol/mol, whereas the AERO bundle holds
    ! data in kg/kg. We therefore need to copy the data so that we can 
    ! change units independently.
    !=======================================================================
    IF ( DoAERO ) THEN

       ! Get AERO bundle
       CALL ESMF_StateGet( EXPORT, 'AERO', AeroBdl, __RC__ )

       ! Loop over all GC tracers that we may want to add to the AERO
       ! bundle
       DO I = 1, NumAERO

          ! Get GEOS-Chem tracer ID
          GCID = Get_Indx( TRIM(GcNames(I)), State_Chm%Trac_Id, State_Chm%Trac_Name )

          ! If species is defined, copy field and add to AERO bundle
          IF ( GCID > 0 ) THEN

             ! This is the name in the internal state
             GCName = TRIM(TPFX) // TRIM(GcNames(I))

             ! Get field from internal state
             CALL ESMF_StateGet( INTSTATE, TRIM(GCName), GcFld, RC=RC )
             IF ( RC /= ESMF_SUCCESS ) THEN
                WRITE(*,*) 'Cannot fill AERO bundle - field not found in ' // &
                           'internal state: ' // TRIM(GCName)
                ASSERT_(.FALSE.)
             ENDIF
  
             ! Set number of fields to be created. This is only different from
             ! 1 for sea salt aerosols, which are mapped onto multiple AERO
             ! fields.
             NFLDS = 1
             IF ( TRIM(GcNames(I)) == 'SALA' ) NFLDS = 2
             IF ( TRIM(GcNames(I)) == 'SALC' ) NFLDS = 3

             ! Now create all fields
             DO J = 1, NFLDS
 
                ! AERO field name
                AeroName = TRIM(AeroNames(I))
                IF ( NFLDS == 2 ) AeroName = SALAnames(J)
                IF ( NFLDS == 3 ) AeroName = SALCnames(J)
 
                ! Create new field
                AeroFld = MAPL_FieldCreate( GcFld, name=AeroName, &
                                            DoCopy=.TRUE., __RC__  )
      
                ! Get molecular weight (g/mol)
                GCMW = Input_Opt%Tracer_MW_G(GCID)
      
                ! Fraction of the GC field to be used in the AERO field
                FRAC = 1.0
                IF ( NFLDS == 2 ) FRAC = SALAsplit(J)
                IF ( NFLDS == 3 ) FRAC = SALCsplit(J)

                ! Pass GEOS-Chem field name, molecular weight and fraction to be
                ! used to bundle for easier handling lateron
                CALL ESMF_AttributeSet ( AeroFld, NAME='GCNAME', VALUE=GCName, __RC__ ) 
                CALL ESMF_AttributeSet ( AeroFld, NAME='GCMW',   VALUE=GCMW,   __RC__ ) 
                CALL ESMF_AttributeSet ( AeroFld, NAME='FRAC',   VALUE=FRAC,   __RC__ ) 
      
                ! Before adding to the bundle, convert data from mol/mol to kg/kg
                CALL ESMF_FieldGet( AeroFld, farrayPtr=Ptr3D, __RC__ )
                Ptr3D = Ptr3D * GCMW / MAPL_AIRMW * FRAC
                Ptr3D => NULL()
   
                ! Add to bundle
                CALL MAPL_FieldBundleAdd ( AeroBdl, AeroFld, __RC__ )
             ENDDO !J
          ENDIF
       ENDDO

       ! ---------------------------------------------------------------------
       ! Initialize the AERO_DP bundle
       ! ---------------------------------------------------------------------
       CALL ESMF_StateGet( EXPORT, 'AERO_DP', AeroBdl, __RC__ )

       ! Dust dry and wet deposition 
       CALL ESMF_StateGet( EXPORT, 'DUDP_DST1', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUDP_DST2', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUDP_DST3', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUDP_DST4', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( EXPORT, 'DUWT_DST1', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUWT_DST2', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUWT_DST3', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'DUWT_DST4', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       ! Black carbon dry and wet depostion 
       CALL ESMF_StateGet( EXPORT, 'BCDP_BCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( EXPORT, 'BCDP_BCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( EXPORT, 'BCWT_BCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'BCWT_BCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       ! Organic carbon dry and wet depostion 
       CALL ESMF_StateGet( EXPORT, 'OCDP_OCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( EXPORT, 'OCDP_OCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( EXPORT, 'OCWT_OCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( EXPORT, 'OCWT_OCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

    ENDIF ! DoAERO

    IF ( DoANOX ) THEN
       CALL MAPL_GetPointer( INTSTATE, PTR_O3, TRIM(TPFX)//'O3', __RC__ )

       ! O3_HIST is needed to store O3 field from previous chemistry time step
       ALLOCATE( O3_HIST(IM,JM,LM), STAT=STATUS )
       ASSERT_(STATUS==0)
    ENDIF

    IF ( DoRATS ) THEN
       CALL MAPL_GetPointer( INTSTATE,    PTR_CH4, TRIM(TPFX)//'CH4',    __RC__ )
       CALL MAPL_GetPointer( INTSTATE,    PTR_N2O, TRIM(TPFX)//'N2O',    __RC__ )
       CALL MAPL_GetPointer( INTSTATE,  PTR_CFC11, TRIM(TPFX)//'CFC11',  __RC__ )
       CALL MAPL_GetPointer( INTSTATE,  PTR_CFC12, TRIM(TPFX)//'CFC12',  __RC__ )
       CALL MAPL_GetPointer( INTSTATE, PTR_HCFC22, TRIM(TPFX)//'HCFC22', __RC__ )
       CALL MAPL_GetPointer( INTSTATE,    PTR_H2O, TRIM(TPFX)//'H2O',    __RC__ )

       ! H2O_HIST is needed to store H2O field from previous chemistry time step
       ALLOCATE( H2O_HIST(IM,JM,LM), STAT=STATUS )
       ASSERT_(STATUS==0)
    ENDIF

    !=======================================================================
    ! Initialize the Int2Chm object. This is used to copy the tracer arrays
    ! from the internal state to State_Chm, and vice versa.
    ! In this step, we also check for the friendlieness of the tracers. If
    ! the GEOS-Chem internal convection/turbulence schemes shall be used
    ! (as specified in input.geos.rc), the tracers must not be friendly to
    ! the GEOS-5 moist / turbulence components!
    !=======================================================================
    nFlds = SIZE(State_Chm%Trac_Name,1)
    ALLOCATE( Int2Chm(nFlds), STAT=STATUS )
    ASSERT_(STATUS==0)

    ! Do for every tracer in State_Chm
!<<>>    DO I = 1, nFlds
!<<>>
!<<>>       ! Pass tracer name
!<<>>       Int2Chm(I)%TrcName = TRIM(State_Chm%Trac_Name(I))
!<<>>
!<<>>       ! Get tracer ID
!<<>>       Int2Chm(I)%TrcID = Get_Indx( TRIM(Int2Chm(I)%TrcName), &
!<<>>                                    State_Chm%Trac_Id, State_Chm%Trac_Name )
!<<>>
!<<>>       ! If tracer ID is not valid, make sure all variables are at least defined.
!<<>>       IF ( Int2Chm(I)%TrcID <= 0 ) THEN
!<<>>          Int2Chm(I)%Internal => NULL()
!<<>>          CYCLE
!<<>>       ENDIF
!<<>>
!<<>>       ! Get internal state field
!<<>>       CALL ESMF_StateGet( INTSTATE, TRIM(TPFX)//TRIM(Int2Chm(I)%TrcName), GcFld, RC=STATUS )
!<<>>       IF ( STATUS /= ESMF_SUCCESS ) THEN
!<<>>          WRITE(*,*) 'Cannot find in internal state: '//TRIM(TPFX)//TRIM(Int2Chm(I)%TrcName)
!<<>>          ASSERT_(.FALSE.)
!<<>>       ENDIF
!<<>>
!<<>>       ! Check friendlieness of field: the field must not be friendly to moist and/or 
!<<>>       ! turbulence if the corresponding GEOS-Chem switches are turned on!
!<<>>
!<<>>       ! Check for friendlieness to convection: only if GEOS-Chem convection is enabled
!<<>>       IF ( Input_Opt%LCONV ) THEN
!<<>>          FRIENDLY=.FALSE.
!<<>>          CALL ESMF_AttributeGet( GcFld, NAME="FriendlyToMOIST", &
!<<>>                                  VALUE=FRIENDLY, RC=STATUS )
!<<>>          IF ( STATUS==ESMF_SUCCESS .AND. FRIENDLY ) THEN
!<<>>             IF ( am_I_Root ) THEN
!<<>>                WRITE(*,*) ' '
!<<>>                WRITE(*,*) 'GEOS-Chem convection is turned on, but tracer is also'
!<<>>                WRITE(*,*) 'friendly to MOIST. Cannot do both: ', TRIM(Int2Chm(I)%TrcName)
!<<>>                WRITE(*,*) ' '
!<<>>             ENDIF
!<<>>             ASSERT_(.FALSE.)
!<<>>          ENDIF
!<<>>       ENDIF
!<<>>
!<<>>       ! Check for friendlieness to turbulence: only if GEOS-Chem turbulence is enabled
!<<>>       IF ( Input_Opt%LTURB ) THEN
!<<>>          FRIENDLY=.FALSE.
!<<>>          CALL ESMF_AttributeGet( GcFld, NAME="FriendlyToTURBULENCE", &
!<<>>                                  VALUE=FRIENDLY, RC=STATUS )
!<<>>          IF ( STATUS==ESMF_SUCCESS .AND. FRIENDLY ) THEN
!<<>>             IF ( am_I_Root ) THEN
!<<>>                WRITE(*,*) ' '
!<<>>                WRITE(*,*) 'GEOS-Chem turbulence is turned on, but tracer is also'
!<<>>                WRITE(*,*) 'friendly to TURBULENCE. Cannot do both: ', TRIM(Int2Chm(I)%TrcName)
!<<>>                WRITE(*,*) ' '
!<<>>             ENDIF
!<<>>             ASSERT_(.FALSE.)
!<<>>          ENDIF
!<<>>       ENDIF
!<<>>
!<<>>       ! Get pointer to field
!<<>>       CALL ESMF_FieldGet( GcFld, 0, Ptr3D, __RC__ )
!<<>>
!<<>>       ! Get pointer to internal state and link it to Int2Chm object.
!<<>>!       CALL MAPL_GetPointer( INTSTATE, Ptr3D, TRIM(TPFX)//TRIM(Int2Chm(I)%TrcName), __RC__ )
!<<>>       Int2Chm(I)%Internal => Ptr3D
!<<>>      
!<<>>       ! Free pointers
!<<>>       Ptr3D => NULL()
!<<>>
!<<>>    ENDDO

    !=======================================================================
    ! Error trap: make sure that chemistry / emission time step are same and
    ! correspond to the chemistry step set in GEOSCHEMchem_GridComp.rc.
    !=======================================================================
    ChemTS = GET_TS_CHEM() * 60d0 
    EmisTS = GET_TS_EMIS() * 60d0
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
    ChemTS = GET_TS_CONV() * 60d0 
    EmisTS = GET_TS_DYN()  * 60d0
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

    !=======================================================================
    ! Create TRACERS bundle 
    !=======================================================================

    call ESMF_StateGet(Export, 'TRACERS', bundle, __RC__ )
    call MAPL_Get ( State, INTERNAL_ESMF_STATE=INTERNAL, RC=STATUS )
    VERIFY_(STATUS)

    DO N = 1, Input_Opt%N_TRACERS

       call ESMF_StateGet ( Internal,       &
            trim(tpfx)//trim(State_Chm%Trac_Name(N)), &
            FIELD, RC=RC )
       IF ( RC /= ESMF_SUCCESS ) THEN
          WRITE(*,*) 'Cannot fill TRACER bundle - field not found in ' // &
               'internal state: ' //trim(tpfx)//trim(State_Chm%Trac_Name(N))
          ASSERT_(.FALSE.)
       ENDIF

       trcID = State_Chm%Trac_ID(N)

       call ESMF_AttributeSet (field,      &
            NAME  = 'TCVV',                &
            VALUE = real(Input_Opt%TCVV(trcID),4), &
                    __RC__ )       

       call ESMF_AttributeSet (field,     &
            NAME  = 'TRAC_ID',            &
            VALUE = trcID,                &
                    __RC__ )       

       call MAPL_FieldBundleAdd ( BUNDLE, FIELD, __RC__ )
       IF ( RC .eq. ESMF_SUCCESS ) THEN
          IF ( am_I_Root ) write(*,*) 'Added '//trim(tpfx)//trim(State_Chm%Trac_Name(N)) &
               //' to TRACER bundle'
       ENDIF
    ENDDO

    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LTRAN',              &
         VALUE = Input_Opt%LTRAN,      &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'N_TRACERS',          &
         VALUE = Input_Opt%N_TRACERS,  &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LFILL',              &
         VALUE = Input_Opt%LFILL,      &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LPRT',               &
         VALUE = Input_Opt%LPRT,       &
         RC = STATUS )       
       
   if (AM_I_ROOT .and. Input_Opt%LPRT) then
       print *, trim(Iam)//': TRACERS Bundle during Initialize():' 
       call ESMF_FieldBundlePrint ( bundle )
   end if

    !=======================================================================
    ! All done
    !=======================================================================

    ! Write a header before the timestepping begins
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(/,a)' ) REPEAT( '#', 79 )
       WRITE( logLun, 200     ) TRIM( compName ) // '::Run_', myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    ! Close the file for stdout redirect.  Reopen when executing the run method.
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

    !=======================================================================
    ! Run1 starts here 
    !=======================================================================

    ! Set up traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )

    ! Identify this routine to MAPL
    Iam = TRIM(compName)//'::Run1'

    ! Call run routine stage 1
    ! Skip this step if only one phase is defined. In this case, we do all 
    ! chemistry related processes in phase 2.
    IF ( NPHASE == 2 ) THEN
       CALL Run_ ( GC, IMPORT, EXPORT, CLOCK, 1, __RC__ )
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
! !DESCRIPTION: Run2 is a wrapper method for the phase 2 run phase of the 
!  GEOSCHEMchem gridded component. It calls down to the Run method of the 
!  GEOS-Chem column chemistry code.
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

    ! Set phase number: this is 2 for multi-phase runs, -1 otherwise. If
    ! set to -1, all processes are called (drydep, emissions, chemistry, etc.)
    IF ( NPHASE == 1 ) THEN
       PHASE = -1
    ELSE
       PHASE = 2
    ENDIF

    ! Call run routine stage 2
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
    USE HCO_STATE_MOD,           ONLY : HCO_STATE
    USE HCOI_GC_MAIN_MOD,        ONLY : GetHcoState
    USE GC_LAND_INTERFACE,       ONLY : LANDTYPE_REMAP
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC       ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock    ! ESMF Clock object
    INTEGER,             INTENT(IN   )         :: Phase    ! Run phase (1 or 2)
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(  OUT)         :: RC       ! Error return code
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

    ! Pointer arrays
    REAL(ESMF_KIND_R4),  POINTER :: lonCtr(:,:)   ! Lon centers, this PET [rad]
    REAL(ESMF_KIND_R4),  POINTER :: latCtr(:,:)   ! Lat centers, this PET [rad]
    REAL, POINTER                :: Ptr3d (:,:,:) => NULL() ! Needed by Include_Before_Run.H
    TYPE(MAPL_MetaComp), POINTER :: STATE

    ! For CTM Mode
    REAL,                POINTER :: PLE(:,:,:)     => NULL() ! INTERNAL: PEDGE
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
    INTEGER :: trcID
    REAL    :: COEFF
    CHARACTER(LEN=ESMF_MAXSTR)   :: trcNAME,hcoNAME
    TYPE(ESMF_Field      )       :: trcFIELD
    TYPE(ESMF_FieldBundle)       :: trcBUNDLE
    REAL              , POINTER  :: fPtrArray(:,:,:)
    REAL(ESMF_KIND_R8), POINTER  :: fPtrVal, fPtr1D(:)

    ! Initialize everything to zero (from registry file)?
    INTEGER                      :: InitZero 
 
    ! For HEMCO
    TYPE(HCO_STATE),     POINTER :: HcoState => NULL()

    ! First call?
    LOGICAL, SAVE                :: FIRST    = .TRUE.

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

    ! Turn off alarm: only if it was on and this is phase 2 (don't turn off
    ! after phase 1 since this would prevent phase 2 from being executed).
    IF ( IsChemTime .AND. PHASE /= 1 ) THEN
       CALL ESMF_AlarmRingerOff(ALARM, __RC__ )
    ENDIF

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
    ! Tendencies shall only be updated when chemistry is done.
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

       ! Get pointers to analysis OX exports
       IF ( DoANOX ) THEN
          CALL MAPL_GetPointer ( EXPORT, OX_TEND, 'OX_TEND' , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,      OX, 'OX'      , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,      O3, 'O3'      , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,  O3PPMV, 'O3PPMV'  , __RC__ )

          ! Update 'historical' O3. This is the O3 from the previous
          ! chemistry time step.
          IF ( ASSOCIATED(OX_TEND) ) O3_HIST = PTR_O3
       ENDIF

       ! Get pointers to RATS exports
       IF ( DoRATS) THEN
          CALL MAPL_GetPointer ( EXPORT, H2O_TEND, 'H2O_TEND' , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,      CH4, 'CH4'      , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,      N2O, 'N2O'      , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,    CFC11, 'CFC11'    , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,    CFC12, 'CFC12'    , __RC__ )
          CALL MAPL_GetPointer ( EXPORT,   HCFC22, 'HCFC22'   , __RC__ )

          ! Update 'historical' H2O. This is the H2O from the previous
          ! chemistry time step.
          IF ( ASSOCIATED(H2O_TEND) ) H2O_HIST = PTR_H2O
       ENDIF

       ! Eventually get pointers to GCCTO3 and GCCTTO3. Those fields are optional
       ! and are only filled if defined and required.
       CALL MAPL_GetPointer ( EXPORT, PTR_GCCTO3,   'GCCTO3', notFoundOK=.TRUE., __RC__ )
       CALL MAPL_GetPointer ( EXPORT, PTR_GCCTTO3, 'GCCTTO3', notFoundOK=.TRUE., __RC__ )

       ! Pass IMPORT/EXPORT object to HEMCO state object
       CALL GetHcoState( HcoState )
       ASSERT_(ASSOCIATED(HcoState))
       HcoState%GRIDCOMP => GC
       HcoState%IMPORT   => IMPORT
       HcoState%EXPORT   => EXPORT
       HcoState => NULL()

       ! To use archived convection fields
       IF ( ArchivedConv ) THEN
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_PFI_CN , &
                                 'ARCHIVED_PFI_CN'  , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_PFL_CN , &
                                 'ARCHIVED_PFL_CN'  , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_MFC, &
                                 'ARCHIVED_CNV_MFC' , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_MFD, &
                                 'ARCHIVED_CNV_MFD' , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_CNV_CVW, &
                                 'ARCHIVED_CNV_CVW' , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_DQRC   , &
                                 'ARCHIVED_DQRC'    , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_REV_CN , &
                                 'ARCHIVED_PFI_CN'  , notFoundOK=.TRUE., __RC__ )
          CALL MAPL_GetPointer ( IMPORT, PTR_ARCHIVED_T      , & 
                                 'ARCHIVED_T'       , notFoundOK=.TRUE., __RC__ )
       ENDIF
    ENDIF

    !=======================================================================
    ! Set land types in State_Met from GEOS5 vegetation type fractions or
    ! OLSON land type fractions. For now, the land types are treated as 
    ! static and obtained from offline fields. The routine below thus needs
    ! to be called only once.
    ! Once the GEOS-5 land types are dynamic, we should import those from
    ! the surface component (field ITY, or better: vegetation type fractions
    ! per grid box).                                   (ckeller, 01/06/2015)
    !=======================================================================
    IF ( FIRST ) THEN
       CALL LANDTYPE_REMAP ( am_I_Root, IMPORT, State_Met, __RC__ )
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
                   lonCtr    = lonCtr,   &  ! Lon centers on this PET [radians]
                   latCtr    = latCtr,   &  ! Lat centers on this PET [radians]
                   ZTH       = zenith,   &  ! Solar zenith angle
                   SLR       = solar,    &  ! Solar insolation
                   __RC__ )

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
       STATUS = GIGC_FAILURE
       VERIFY_(STATUS)
    ENDIF

    !=======================================================================
    ! Populate tracer array in State_Chm 
    ! ==> Get arrays from Chemisty Export State via trcBUNDLE
    !=======================================================================

    call ESMF_StateGet(EXPORT, 'TRACERS', trcBUNDLE,  __RC__ )    
    call ESMF_FieldBundleGet(trcBUNDLE, fieldCount=N_TRC,   __RC__ ) 
!    IF (.not. FIRST) THEN
       DO IND=1, N_TRC 
          call ESMFL_BundleGetPointerToData( trcBUNDLE, IND, fPtrArray, __RC__)
          call ESMF_FieldBundleGet(trcBUNDLE, IND, trcFIELD, __RC__ ) 
          !       call ESMF_FieldGet( trcFIELD, NAME=trcNAME, __RC__)
          
          ! Extract species ID
          call ESMF_AttributeGet (trcFIELD,    &
               NAME  = 'TRAC_ID',              &
               VALUE = trcID,            __RC__ )
          
          ! Pass to State_Chm. 
          State_Chm%Tracers(:,:,:,trcID) = fPtrArray(:,:,LM:1:-1)
          !       nullify(fPtrArray)
       END DO
!    ENDIF

    !=======================================================================
    ! pre-Run method array assignments. This passes the tracer arrays from
    ! the internal state to State_Chm. On the first call, it also fills the
    ! internal species arrays in State_Chm with the values read from the
    ! restart file (and stored in the internal state).
    !=======================================================================

    CALL MAPL_TimerOn(STATE, "CP_BFRE")
#   include "Includes_Before_Run.H"
    CALL MAPL_TimerOff(STATE, "CP_BFRE")

    !=======================================================================
    ! Check if zero initialization option is selected. If so, make sure all
    ! concentrations are initialized to zero! 
    !=======================================================================
    IF ( FIRST ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, InitZero, Default=0, &
                                     Label = "INIT_ZERO:", __RC__ ) 
       IF ( InitZero == 1 ) THEN
          State_Chm%Tracers = 1.0d-26
          State_Chm%Species = 1.0d-26
          IF ( am_I_Root ) THEN
             write(*,*) ' '
             write(*,*) ' '
             write(*,*) '### ALL GEOS-CHEM CONCENTRATIONS INITIALIZED TO ZERO (1d-26)!!! ###'
             write(*,*) ' '
             write(*,*) ' '
          ENDIF
       ENDIF
    ENDIF

    !=======================================================================
    ! Get total ozone column from GEOS-Chem export variable.
    ! Need to calculate from restart variables on first call!
    !=======================================================================
    IF ( FIRST ) CALL CalcTotOzone_( am_I_Root,  PTR_O3, PLE, GCCTROPP, &
                                     PTR_GCCTO3, PTR_GCCTTO3, __RC__ )
    IF ( ASSOCIATED(PTR_GCCTO3) ) State_Met%TO3 = PTR_GCCTO3


    !=======================================================================
    ! Execute GEOS-Chem on multiple PETs
    !=======================================================================
    
    ! Fix negatives!
    ! These can be brought in as an artifact of convection.
    WHERE ( State_Chm%Tracers < 0.0e0 )
       State_Chm%Tracers = 1.0e-36
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
   
          ! Save the PET # (aka PET #) in Input_Opt
          Input_Opt%myCpu = myPet
    
          ! Run the GEOS-Chem column chemistry code for the given phase
          CALL GIGC_Chunk_Run( am_I_Root  = am_I_Root,  & ! Is this the root PET?
                               GC         = GC,         & ! Gridded component ref. 
                               IM         = IM,         & ! # of lons on this PET
                               JM         = JM,         & ! # of lats on this PET
                               LM         = LM,         & ! # of levs on this PET
                               nymd       = nymd,       & ! Current YYYYMMDD date
                               nhms       = nhms,       & ! Current hhmmss time
                               year       = year,       & ! Current year
                               month      = month,      & ! Current month
                               day        = day,        & ! Current day
                               dayOfYr    = dayOfYr,    & ! Current day of year
                               hour       = hour,       & ! Current hour
                               minute     = minute,     & ! Current minute
                               second     = second,     & ! Current second
                               utc        = utc,        & ! Current UTC time [hrs]
                               hElapsed   = hElapsed,   & ! Elapsed hours
                               Input_Opt  = Input_Opt,  & ! Input Options
                               State_Chm  = State_Chm,  & ! Chemistry State
                               State_Met  = State_Met,  & ! Meteorology State
                               Phase      = Phase,      & ! Run phase
                               IsChemTime = IsChemTime, & ! Is it time for chemistry?
                               __RC__                  )  ! Success or failure?
   
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
#   include "Includes_After_Run.H"
    CALL MAPL_TimerOff(STATE, "CP_AFTR")

    !=======================================================================
    ! Copy tracer arrays back to the bundle
    !=======================================================================

!    call ESMF_StateGet(EXPORT, 'TRACERS', trcBUNDLE,  __RC__ )    
    DO IND=1, N_TRC
       call ESMFL_BundleGetPointerToData( trcBUNDLE, IND, fPtrArray, __RC__)
       call ESMF_FieldBundleGet(trcBUNDLE, IND, trcFIELD, __RC__ ) 
!       call ESMF_FieldGet( trcFIELD, NAME=trcNAME, __RC__)
     
       ! Extract species ID
       call ESMF_AttributeGet (trcFIELD,    &
            NAME  = 'TRAC_ID',              &
            VALUE = trcID,            __RC__ )

       ! Pass Tracers back to Internal State
       fPtrArray = State_Chm%Tracers(:,:,LM:1:-1,trcID)
!       nullify(fPtrArray)
    END DO

    ! Stop timer
    ! ----------
    CALL MAPL_TimerOff(STATE, "RUN"  )

    ! Fill bundles only on chemistry time steps and after phase 2 
    ! -----------------------------------------------------------
    IF ( IsTendTime ) THEN

       !=======================================================================
       ! Fill ozone export states if GC is the analysis OX provider:
       !      OX: volume mixing ratio
       !      O3: mass mixing ratio
       !  O3PPMV: volume mixing ratio in ppm
       ! OX_TEND: mol mol-1 s-1
       !
       ! GEOS-Chem tracer:
       ! PTR_O3: mol mol-1
       !=======================================================================
       IF ( DoANOX ) THEN
          IF ( ASSOCIATED(OX     ) ) OX      = PTR_O3
          IF ( ASSOCIATED(O3     ) ) O3      = PTR_O3 * MAPL_O3MW / MAPL_AIRMW
          IF ( ASSOCIATED(O3PPMV ) ) O3PPMV  = PTR_O3 * 1.00E+06
   
          ! Get tendencies. Also store current O3 field in O3_HIST for use in 
          ! next chemistry time step.
          IF ( ASSOCIATED(OX_TEND) ) THEN
             OX_TEND = ( PTR_O3 - O3_HIST ) / tsChem
             O3_HIST = PTR_O3
          ENDIF
       ENDIF
   
       !=======================================================================
       ! Total ozone and total tropospheric ozone for export [dobsons].
       ! 2.69E+20 per dobson.
       !=======================================================================
       CALL CalcTotOzone_( am_I_Root,  PTR_O3, PLE, GCCTROPP, &
                           PTR_GCCTO3, PTR_GCCTTO3, __RC__ )
   
       !=======================================================================
       ! Fill RATS export states if GC is the RATS provider
       ! The tracer concentrations of the RATS export states are in mol mol-1,
       ! exactly the same as the GC internal values. 
       ! PTR_H2O is in mol mol-1. Convert to kg here.
       !=======================================================================
       IF ( DoRATS ) THEN
          IF ( ASSOCIATED(CH4   ) )    CH4 = PTR_CH4
          IF ( ASSOCIATED(N2O   ) )    N2O = PTR_N2O
          IF ( ASSOCIATED(CFC11 ) )  CFC11 = PTR_CFC11
          IF ( ASSOCIATED(CFC12 ) )  CFC12 = PTR_CFC12
          IF ( ASSOCIATED(HCFC22) ) HCFC22 = PTR_HCFC22
   
          ! Get tendencies only on chemistry time step. Also store current H2O 
          ! field in H2O_HIST for use in next chemistry time step.
          IF ( ASSOCIATED(H2O_TEND) ) THEN
             H2O_TEND = ( PTR_H2O - H2O_HIST ) * MAPL_H2OMW / MAPL_AIRMW / tsChem
             H2O_HIST = PTR_H2O
          ENDIF
       ENDIF
   
       !=======================================================================
       ! Fill AERO bundle if GEOS-Chem is the AERO provider.
       ! For every field of the AERO bundle, we will copy the corresponding
       ! GEOS-Chem tracer field, converting units from mol mol-1 to kg kg-1.
       !=======================================================================
       IF ( DoAERO ) THEN
   
          ! Get Internal state
          CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ ) 
   
          ! Get AERO bundle
          CALL ESMF_StateGet( EXPORT, 'AERO', AeroBdl, __RC__ )
   
          ! Number of fields in the AERO Bundle
          CALL ESMF_FieldBundleGet ( AeroBdl, FieldCount=nAero, __RC__ )
   
          ! Update every field
          DO N = 1, nAero
   
             ! Get field
             CALL ESMF_FieldBundleGet( AeroBdl, N, AeroFld, __RC__ )
   
             ! Extract GC tracer name, molecular weight and fraction to be used
             CALL ESMF_AttributeGet( AeroFld, NAME='GCNAME', VALUE=GcName, __RC__ )
             CALL ESMF_AttributeGet( AeroFld, NAME='GCMW'  , VALUE=GCMW,   __RC__ )
             CALL ESMF_AttributeGet( AeroFld, NAME='FRAC',   VALUE=FRAC,   __RC__ ) 
   
             ! Get pointer to Aero data
             CALL ESMF_FieldGet( AeroFld, farrayPtr=AeroPtr3D, __RC__ )
   
             ! Get pointer to GC data
             CALL MAPL_GetPointer ( INTSTATE, GcPtr3D, TRIM(GcName), __RC__ )
   
             ! Pass GC to AERO. Convert from mol/mol to kg/kg. Only use the 
             ! fraction specified during initialization (different from 1 for
             ! sea salt aerosols only)
             AeroPtr3D = GcPtr3D * FRAC * GCMW / MAPL_AIRMW
 
   !!! writing to diagnostics
             GcPtr3D   => NULL()
             CALL ESMF_FieldGet( AeroFld, NAME=GcName, __RC__ )
             CALL MAPL_GetPointer ( EXPORT, GcPtr3D, 'AERO_'//TRIM(GcName), &
                                    NotFoundOk=.TRUE., __RC__ )
             IF ( ASSOCIATED(GcPtr3D) ) GcPtr3D = AeroPtr3D
   !!!
   
             ! Free pointers
             GcPtr3D   => NULL()
             AeroPtr3D => NULL()
          ENDDO
  
          ! Fill AERO_DP bundle
          CALL FillAeroDP ( am_I_Root, GC, EXPORT, __RC__ )
 
       ENDIF ! DoAero
    ENDIF ! IsTendTime

    END IF RunningGEOSChem

    !=======================================================================
    ! If we were not doing chemistry, make sure that all tendencies are
    ! zero. We ignore the tendencies that may arise due to physical
    ! processes covered by GEOS-Chem (e.g. convection).
    !=======================================================================
    IF ( .NOT. IsTendTime ) THEN
       IF ( DoANOX ) THEN
          IF ( ASSOCIATED(OX_TEND) ) OX_TEND = 0.0 
       ENDIF
       IF ( DoRATS ) THEN
          IF ( ASSOCIATED(H2O_TEND) ) H2O_TEND  = 0.0 
       ENDIF
    ENDIF


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
  
    ! Update first flags
    FIRST = .FALSE.

    ! The restart file should exist at least after the first full cycle,
    ! e.g. after phase 1 and phase 2 has been called once.
    IF ( FIRST .AND. Phase == 1 ) THEN
       Input_Opt%haveImpRst = Input_Opt%haveImpRst
    ELSE
       Input_Opt%haveImpRst = .TRUE.
    ENDIF

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
!#   include "GIGCchem_DeclarePointer___.h"     ! Ptr decls to states
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
    TYPE(GC_GeoLoc)            :: GeoLoc      ! G-C obj for location
 
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

    ! For species copying
    INTEGER                   :: IND
    TYPE(ESMF_STATE)          :: INTSTATE
    REAL, POINTER             :: Ptr3D(:,:,:) => NULL()

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
    !=========================================================================

    ! Get Internal state
    CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ ) 

    ! Loop over all species
    DO I = 1, SIZE(State_Chm%Spec_ID,1)

       ! Skip if empty
       IF ( TRIM(State_Chm%Spec_Name(I)) == '' ) CYCLE

       ! Is this a tracer?
       IND = Get_Indx( TRIM(State_Chm%Spec_Name(I)), State_Chm%Trac_Id, State_Chm%Trac_Name )
       IF ( IND > 0 ) CYCLE

       ! Get data from internal state and copy to species array
       CALL MAPL_GetPointer( INTSTATE, Ptr3D, TRIM(SPFX)//TRIM(State_Chm%Spec_Name(I)), &
                             notFoundOK=.TRUE., __RC__ )
       IF ( .NOT. ASSOCIATED(Ptr3D) ) CYCLE
       Ptr3D = State_Chm%Species(:,:,LM:1:-1,State_Chm%Spec_ID(I))

       ! Verbose 
       if ( MAPL_am_I_Root()) write(*,*) 'Species written to INTERNAL state: ', TRIM(State_Chm%Spec_Name(I))
    ENDDO

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
    CALL GIGC_Chunk_Final( am_I_Root = am_I_Root,  &   ! Is this the root PET?
                           Input_Opt = Input_Opt,  &   ! Input Options
                           State_Chm = State_Chm,  &   ! Chemistry State
                           State_Met = State_Met,  &   ! Meteorology State
                           __RC__                 )


    ! Free Int2Chm pointer
    IF ( ASSOCIATED(Int2Chm) ) THEN
       DO I=1,SIZE(Int2Chm,1)
          Int2Chm(I)%Internal => NULL()
       ENDDO
       DEALLOCATE(Int2Chm)
    ENDIF

    ! Free local pointers
    O3               => NULL()
    O3PPMV           => NULL()
    OX               => NULL()
    OX_TEND          => NULL()
    CH4              => NULL()
    N2O              => NULL()
    CFC11            => NULL()
    CFC12            => NULL()
    HCFC22           => NULL()
    H2O_TEND         => NULL()
    PTR_O3           => NULL()
    PTR_CH4          => NULL()
    PTR_N2O          => NULL()
    PTR_CFC11        => NULL()
    PTR_CFC12        => NULL()
    PTR_HCFC22       => NULL()
    PTR_H2O          => NULL()
    PTR_GCCTO3       => NULL()
    PTR_GCCTTO3      => NULL()

    PTR_ARCHIVED_PFI_CN  => NULL()
    PTR_ARCHIVED_PFL_CN  => NULL()
    PTR_ARCHIVED_CNV_MFC => NULL()
    PTR_ARCHIVED_CNV_MFD => NULL()
    PTR_ARCHIVED_CNV_CVW => NULL()
    PTR_ARCHIVED_DQRC    => NULL()
    PTR_ARCHIVED_REV_CN  => NULL()
    PTR_ARCHIVED_T       => NULL()

    ! Deallocate arrays
    IF ( ASSOCIATED( O3_HIST) ) DEALLOCATE( O3_HIST)
    IF ( ASSOCIATED(H2O_HIST) ) DEALLOCATE(H2O_HIST)

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
! !IROUTINE: CalcTotOzone_ 
!
! !DESCRIPTION: CalcTotOzone_ calculates total ozone for the entire
!  atmosphere and troposphere only (in dobsons) and writes them into
!  the export variables GCCTO3 and GCCTTO3, respectively.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE CalcTotOzone_ ( am_I_Root, O3, PLE, TROPP, TO3, TTO3, RC )
!
! !INPUT PARAMETERS:
!
    LOGICAL                        :: am_I_Root
    REAL,   POINTER                :: O3   (:,:,:)
    REAL,   POINTER                :: PLE  (:,:,:)
    REAL,   POINTER                :: TROPP(:,:  )
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                         
!              
    REAL,   POINTER                :: TO3 (:,:)
    REAL,   POINTER                :: TTO3(:,:)
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER, INTENT(OUT), OPTIONAL :: RC
!
! !REVISION HISTORY:
!  25 Oct 2014 - C. Keller   - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    REAL,  ALLOCATABLE           :: DUsLayerL(:,:)! Dobsons in a layer, 
                                                  !  for total ozone
    REAL,  ALLOCATABLE           :: wgt(:,:)      ! Layer thickness weighting
                                                  !  for total ozone
    INTEGER                      :: IM, JM, LM, L, STATUS
    CHARACTER(LEN=ESMF_MAXSTR)   :: Iam

    !=======================================================================
    ! CalcTotOzone_ begins here
    !=======================================================================

    ! Traceback handle
    Iam = 'CalcTotOzone_'

    ! Nothing to do if neither of the arrays is associated
    IF ( .NOT. ASSOCIATED(TO3) .AND. .NOT. ASSOCIATED(TTO3) ) THEN
       RC = ESMF_SUCCESS
       RETURN
    ENDIF

    ! Grid size
    IM = SIZE(O3,1)
    JM = SIZE(O3,2)
    LM = SIZE(O3,3)

    ! Allocate local variables
    ALLOCATE(DUsLayerL(IM,JM), STAT=STATUS)
    VERIFY_(STATUS)
    ALLOCATE(wgt(IM,JM), STAT=STATUS)
    VERIFY_(STATUS)
 
    ! Calculate total ozone
    DO L = 1,LM
 
       DUsLayerL(:,:) = O3(:,:,L)*(PLE(:,:,L)-PLE(:,:,L-1))*(MAPL_AVOGAD/2.69E+20)/(MAPL_AIRMW*MAPL_GRAV)

       IF ( ASSOCIATED(TO3) ) TO3 = TO3+DUsLayerL
       IF ( ASSOCIATED(TTO3) ) THEN
          wgt  = MAX(0.0,MIN(1.0,(PLE(:,:,L)-TROPP(:,:))/(PLE(:,:,L)-PLE(:,:,L-1))))
          TTO3 = TTO3+DUsLayerL*wgt
       END IF
    END DO
 
    ! Cleanup
    DEALLOCATE(DUsLayerL, STAT=STATUS)
    VERIFY_(STATUS)
    DEALLOCATE(wgt, STAT=STATUS)
    VERIFY_(STATUS)

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE CalcTotOzone_
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
    LOGICAL,             INTENT(OUT), OPTIONAL :: haveImpRst  ! Does import restart exist? 

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
!        CALL ESMF_ConfigGetAttribute ( MaplCF, tsChem, LABEL="GIGCchem_DT:", DEFAULT=tsDyn, __RC__ )
        CALL MAPL_Get( STATE, RUNALARM=ALARM, __RC__ ) 
        CALL ESMF_AlarmGet( ALARM, RingInterval=chemInterval, __RC__ )
        CALL ESMF_TimeIntervalGet( chemInterval, s_r8=dt_r8, __RC__ )
        tsChem = real(dt_r8)

        IF(tsChem < tsDyn) THEN
         IF(MAPL_AM_I_ROOT()) PRINT *,"Chem_DT cannot be less than RUN_DT"
         STATUS = 1
         VERIFY_(STATUS)
        END IF
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
    ! Does the import restart file exist?
    !=======================================================================
    
    ! Import restart file name
    CALL ESMF_ConfigGetAttribute( GeosCF, importRstFN,                    &
                                  DEFAULT = "geoschemchem_import_rst",    &
                                  LABEL   = "importRestartFileName:",     &
                                  __RC__ )

   
    ! Test if it exists
    IF ( PRESENT( haveImpRst ) ) THEN
       INQUIRE( FILE=TRIM( importRstFN ), EXIST=haveImpRst )
       IF( MAPL_AM_I_ROOT() ) THEN
          PRINT *," ",TRIM( importRstFN )," exists: ", haveImpRst
          PRINT *," "
       END IF
    END IF

    !=======================================================================
    ! Extract time/date information
    !=======================================================================
    
    ! Get the ESMF time object
    CALL ESMF_ClockGet( Clock,                    &
                        startTime    = startTime, &
                        currTime     = currTime,  &
                        advanceCount = count,     &
                         __RC__ )

    ! Get individual fields from the time object
    CALL ESMF_TimeGet( currTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save fields for return
    IF ( PRESENT( nymd     ) ) CALL MAPL_PackTime( nymd, yyyy, mm, dd )
    IF ( PRESENT( nhms     ) ) CALL MAPL_PackTime( nhms, h,    m,  s  )
    IF ( PRESENT( advCount ) ) advCount = count
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
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: FillAeroDP
!
! !DESCRIPTION: FillAeroDP fills the AERO_DP bundle
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE FillAeroDP ( am_I_Root, GC, EXPORT, RC ) 
!
! !USES:
!
    USE HCO_ERROR_MOD
    USE HCO_DIAGN_MOD,   ONLY : DiagnCont, Diagn_Get
!
! !INPUT PARAMETERS:
!
    LOGICAL                            :: am_I_Root
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                         
!              
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER, INTENT(OUT), OPTIONAL     :: RC
!
! !REVISION HISTORY:
!  30 Mar 2015 - C. Keller   - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!

    REAL, POINTER                :: Ptr2d(:,:) => NULL()
    INTEGER                      :: I, J, N, TrcID
    CHARACTER(LEN= 2)            :: Prfx 
    CHARACTER(LEN=15)            :: TrcName 
    CHARACTER(LEN=ESMF_MAXSTR)   :: ExpName 

    ! Hemco diagnostics
    INTEGER                      :: DgnID
    INTEGER                      :: FLAG, ERR
    TYPE(DiagnCont), POINTER     :: DgnCont => NULL()

    ! Error handling
    INTEGER                      :: STATUS
    CHARACTER(LEN=ESMF_MAXSTR)   :: Iam

    !=======================================================================
    ! FillAeroDP begins here
    !=======================================================================

    ! Traceback handle
    Iam = 'FillAeroDP'

    ! There are 8 species in total
    DO N = 1, 8
 
       ! Get species ID
       SELECT CASE ( N )
          CASE ( 1 )
             TrcName = 'DST1'
             Prfx    = 'DU'
          CASE ( 2 )
             TrcName = 'DST2'
             Prfx    = 'DU'
          CASE ( 3 )
             TrcName = 'DST3'
             Prfx    = 'DU'
          CASE ( 4 )
             TrcName = 'DST4'
             Prfx    = 'DU'
          CASE ( 5 )
             TrcName = 'BCPI'
             Prfx    = 'BC'
          CASE ( 6 )
             TrcName = 'BCPO'
             Prfx    = 'BC'
          CASE ( 7 )
             TrcName = 'OCPI'
             Prfx    = 'OC'
          CASE ( 8 )
             TrcName = 'OCPO'
             Prfx    = 'OC'
          CASE DEFAULT
             TrcName = 'YeahYeahYeah'
       END SELECT

       ! Get GEOS-Chem tracer ID
       TrcID = Get_Indx( TRIM(TrcName), State_Chm%Trac_ID, State_Chm%Trac_Name )

       ! Only if tracer is defined...
       IF ( TrcID <= 0 ) CYCLE 

       ! Dry dep and wet dep
       DO I = 1, 2
   
          IF ( I == 1 ) THEN
             ExpName = TRIM(Prfx)//'DP_'//TRIM(TrcName)
          ELSEIF ( I == 2 ) THEN 
             ExpName = TRIM(Prfx)//'WT_'//TRIM(TrcName)
          ENDIF

          ! Get pointer
          CALL MAPL_GetPointer( EXPORT, Ptr2D, TRIM(ExpName), notFoundOk=.TRUE., __RC__ )

          ! Skip if not defined
          IF ( .NOT. ASSOCIATED(Ptr2D) ) CYCLE             

          ! Reset
          Ptr2D = 0.0
       
          ! For deposition arrays ...
          IF ( I == 1 ) THEN
            
             ! Get diagnostics 
             DgnID = 44500 + TrcID
             CALL Diagn_Get( am_I_Root, .FALSE., DgnCont, FLAG, ERR, &
                    cID=DgnID, AutoFill=-1, InclManual=.TRUE., &
                    COL=Input_Opt%DIAG_COLLECTION ) 

             ! Error check 
             ASSERT_( ERR == HCO_SUCCESS )

             ! Add to array if diagnostics is defined
             ! GEOS-Chem diagnostics is in kg m-2 s-1.
             IF ( FLAG == HCO_SUCCESS ) THEN
                IF ( ASSOCIATED(DgnCont%Arr2D%Val) ) THEN
                   Ptr2D = Ptr2D + DgnCont%Arr2D%Val
   
                   ! testing only
                   if(am_I_Root) then
                      write(*,*) TRIM(DgnCont%cName), ' added to ', TRIM(ExpName)
                   endif

                ENDIF
             ENDIF
        
          ! For wet depostion arrays ... 
          ELSEIF ( I == 2 ) THEN

             ! Convective and wet scavenging
             DO J = 1, 2

                SELECT CASE ( J ) 
                   ! Convection:
                   CASE ( 1 ) 
                      DgnID = 38000 + TrcID
                   ! Wet deposition
                   CASE ( 2 ) 
                      DgnID = 39000 + TrcID
                   CASE DEFAULT
                      DgnID = -1
                END SELECT

                ! Get diagnostics 
                CALL Diagn_Get( am_I_Root, .FALSE., DgnCont, FLAG, ERR, &
                       cID=DgnID, AutoFill=-1, InclManual=.TRUE., &
                       COL=Input_Opt%DIAG_COLLECTION ) 

                ! Error check 
                ASSERT_( ERR == HCO_SUCCESS )

                ! Add to array if diagnostics is defined. GEOS-Chem diagnostics
                ! is already in kg m-2 s-1.
                IF ( FLAG == HCO_SUCCESS ) THEN
                   IF ( ASSOCIATED(DgnCont%Arr2D%Val) ) THEN
                      WHERE(DgnCont%Arr2D%Val>0.0_sp)
                         Ptr2D = Ptr2D + DgnCont%Arr2D%Val
                      ENDWHERE

                      ! testing only
                      if(am_I_Root) then
                         write(*,*) TRIM(DgnCont%cName), ' added to ', TRIM(ExpName)
                      endif

                   ELSEIF ( ASSOCIATED(DgnCont%Arr3D%Val) ) THEN
                      Ptr2D = Ptr2D + SUM(DgnCont%Arr3D%Val,DIM=3)
                   ENDIF
                ENDIF
             ENDDO !J 
          ENDIF

       ENDDO !I
    ENDDO !N

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE FillAeroDP 
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: RoundOff
!
! !DESCRIPTION: Rounds a number X to N decimal places of precision.
!\\
!\\
! !INTERFACE:
!
  FUNCTION RoundOff( X, N ) RESULT( Y )
!
! !INPUT PARAMETERS:
! 
    REAL*8,  INTENT(IN) :: X   ! Number to be rounded
    INTEGER, INTENT(IN) :: N   ! Number of decimal places to keep
!
! !RETURN VALUE:
!
    REAL*8              :: Y   ! Number rounded to N decimal places
!
! !REMARKS:
!  The algorithm to round X to N decimal places is as follows:
!  (1) Multiply X by 10**(N+1)
!  (2) If X < 0, then add -5 to X; otherwise add 5 to X
!  (3) Round X to nearest integer
!  (4) Divide X by 10**(N+1)
!  (5) Truncate X to N decimal places: INT( X * 10**N ) / 10**N
!                                                                             .
!  Rounding algorithm from: Hultquist, P.F, "Numerical Methods for Engineers 
!   and Computer Scientists", Benjamin/Cummings, Menlo Park CA, 1988, p. 20.
!                                                                             .
!  Truncation algorithm from: http://en.wikipedia.org/wiki/Truncation
!                                                                             .
!  The two algorithms have been merged together for efficiency.
!
! !REVISION HISTORY:
!  14 Jul 2010 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES
!
    ! Round and truncate X to N decimal places
    Y = INT( NINT( X*(10d0**(N+1)) + SIGN( 5d0, X ) ) / 10d0 ) / (10d0**N)

  END FUNCTION RoundOff
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: print_mean_oh
!
! !DESCRIPTION: Subroutine Print\_Mean\_OH prints the average mass-weighted OH 
!  concentration at the end of a simulation.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Print_Mean_OH( GC, logLun, airMass, ohMass, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC             ! Ref to this GridComp

    INTEGER,             INTENT(IN)    :: logLun         ! LUN for stdout print
    REAL,     POINTER,   INTENT(IN)    :: airMass(:,:,:) ! Air mass [molec air]
    REAL,     POINTER,   INTENT(IN)    :: ohMass (:,:,:) ! Mass-weighted OH
                                                         !  [molec OH/cm3 * 
                                                         !   molec air]
!
! !INPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC             ! Return code
!
! !REMARKS:
!  The AIRMASS and OHMASS variables are used to pass the data values from the 
!  ESMF state, which is REAL*4.  We need to make sure that we do not store into
!  the ESMF state any values that exceed 1e38, which is the maximum allowable 
!  value.  Therefore, AIRMASS and OHMASS will store the values divided by the
!  scale factor OH_SCALE = 1d20 in order to prevent this overflow situation.
!
! !REVISION HISTORY: 
!  01 Jul 2010 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES
!
    CHARACTER(LEN=ESMF_MAXSTR) :: compName      ! Name of gridded component
    REAL*8                     :: SUM_OHMASS   
    REAL*8                     :: SUM_MASS   
    REAL*8                     :: OHCONC
 
    __Iam__('Print_Mean_OH')

    !=======================================================================
    ! Initialization
    !=======================================================================

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    Iam = TRIM(compName)//'::Print_Mean_OH'
 
    !=======================================================================
    ! Print mean OH values
    !=======================================================================
    
    ! Total Mass-weighted OH [molec OH/cm3] * [molec air]
    SUM_OHMASS = GlobalSum( GC, ohMass,  __RC__ )

    ! Atmospheric air mass [molec air]
    SUM_MASS   = GlobalSum( GC, airMass, __RC__ )

    ! Restore proper values by applying the OH scale factor
    ! (This is necessary in order avoid overflow)
    SUM_OHMASS = SUM_OHMASS * OH_SCALE
    SUM_MASS   = SUM_MASS   * OH_SCALE

    ! Avoid divide-by-zero errors 
    IF ( SUM_MASS > 0d0 ) THEN 
            
       ! Divide OH by [molec air] and report as [1e5 molec/cm3]
       OHCONC = ( SUM_OHMASS / SUM_MASS ) / 1d5
         
       ! Write value to log file
       WRITE( logLun, '(/,a)' ) REPEAT( '=', 79 ) 
       WRITE( logLun, *       ) 'Mass-Weighted OH Concentration'
       WRITE( logLun, *       ) 'Mean OH = ', OHCONC, ' [1e5 molec/cm3]' 
       WRITE( logLun, '(  a)' ) REPEAT( '=', 79 ) 

    ELSE

       ! Write error msg if SUM_MASS is zero
       WRITE( logLun, '(/,a)' ) REPEAT( '=', 79 ) 
       WRITE( logLun, '(  a)' ) 'Could not print mass-weighted OH!'
       WRITE( logLun, '(  a)' ) 'Atmospheric air mass is zero!'
       WRITE( logLun, '(  a)' ) REPEAT( '=', 79 ) 
       
    ENDIF

  END SUBROUTINE Print_Mean_OH
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GlobalSum
!
! !DESCRIPTION: Subroutine GlobalSum prints the sum (or minimum, or maximum)
!  of an array across all PETs.  Calls the ESMF_VMAllFullReduce function
!  to do the array reduction.  The default is to compute the array sum
!  unless MINIMUM=.TRUE. or MAXIMUM=.TRUE.
!\\
!\\
! !INTERFACE:
!
  FUNCTION GlobalSum( GC, dataPtr, minimum, maximum, RC ) RESULT( value )
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC              ! Gridcomp name
    REAL,    POINTER,    INTENT(IN)    :: dataPtr(:,:,:)  ! Data array
    LOGICAL, OPTIONAL,   INTENT(IN)    :: minimum         ! Compute minimum?
    LOGICAL, OPTIONAL,   INTENT(IN)    :: maximum         ! Compute maximum?
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC              ! Return code
!
! !RETURN VALUE:
!
    REAL                               :: value           ! Sum, max, or min
! 
! !REVISION HISTORY: 
!  01 Jul 2010 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES
!
    ! Objects
    TYPE(ESMF_VM)              :: VM            ! ESMF VM object

    ! Scalars
    INTEGER                    :: nSize         ! Size of 3-D array
    CHARACTER(LEN=ESMF_MAXSTR) :: compName      ! Gridded component name
    __Iam__('GlobalSum')

    ! Arrays 
    REAL, ALLOCATABLE          :: data1d(:)     ! 1-D array for reduction

    !=======================================================================
    ! Initialization
    !=======================================================================

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, vm=VM, __RC__ )
    Iam = TRIM(compName)//'::GlobalSum'

    !=======================================================================
    ! Do the reduction operation across all CPU's: sum, max, or min
    !=======================================================================

    ! Create a 1-D vector
    nSize = SIZE( dataPtr )
    ALLOCATE( data1d( nSize ), STAT=STATUS )
    VERIFY_(STATUS)

    ! Rearrange into a 1-D array
    data1d = RESHAPE( dataPtr, (/1/) )
    
    ! Compute the sum over all PETS
    IF ( PRESENT( maximum ) ) THEN
       CALL ESMF_VMAllFullReduce( VM, data1d, value, nSize, ESMF_REDUCE_MAX, __RC__ )
    ELSE IF ( PRESENT( minimum ) ) THEN
       CALL ESMF_VMAllFullReduce( VM, data1d, value, nSize, ESMF_REDUCE_MIN, __RC__ )
    ELSE 
       CALL ESMF_VMAllFullReduce( VM, data1d, value, nSize, ESMF_REDUCE_SUM, __RC__ )
    ENDIF

    ! Deallocate temporary array
    IF( ALLOCATED( data1D ) ) DEALLOCATE( data1d )

  END FUNCTION GlobalSum
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
    TYPE(ESMF_DistGrid)             :: distGrid
    TYPE(ESMF_DELayout)             :: LAYOUT
    INTEGER,            ALLOCATABLE :: AL(:,:)
    INTEGER,            ALLOCATABLE :: AU(:,:)
    INTEGER                         :: nDEs
    INTEGER                         :: deId
    INTEGER                         :: gridRank
    INTEGER                         :: deList(1)

    ! Identify this routine to MAPL
    __Iam__('GridGetInterior')

    ! Get ESMF DistGrid object
    CALL ESMF_GridGet    ( GRID,                           &
                           dimCount        = gridRank,     &
                           distGrid        = distGrid,     &
                           __RC__ )                    
 
    ! Get ESMF DELayout object
    CALL ESMF_DistGridGet( distGrid,                       &
                           delayout        = layout,       &
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
    CALL ESMF_DistGridGet( distGrid,                       &
                           minIndexPDe = AL,               &
                           maxIndexPDe = AU,               &
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
    RC = GIGC_SUCCESS

  END SUBROUTINE GridGetInterior
!EOC
END MODULE Chem_GridCompMod
 
