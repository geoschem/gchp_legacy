#include "MAPL_Generic.h"

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_providerservices_mod
!
! !DESCRIPTION: GEOS-Chem includes species CH4, N2O, CFC-11, CFC-12, and 
! HCFC-22 and thus can serve as the provider of the radiatively active tracers 
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
MODULE gigc_providerservices_mod
!
! !USES:
!
  USE ESMF     
  USE MAPL_Mod 
  USE Chem_Mod                            ! Chemistry Base Class

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC   :: Provider_SetServices
  PUBLIC   :: Provider_Initialize
  PUBLIC   :: Provider_SetPointers
  PUBLIC   :: Provider_FillBundles
  PUBLIC   :: Provider_ZeroTendencies
  PUBLIC   :: Provider_Finalize
  PUBLIC   :: CalcTotalOzone
  PUBLIC   :: SetStateMetTO3
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE  :: FillAeroDP
!
! !PRIVATE TYPES:
!
  ! Is GEOS-Chem the provider for AERO, RATS, and/or Analysis OX? 
  LOGICAL  :: DoAERO
  LOGICAL  :: DoRATS
  LOGICAL  :: DoANOX

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
  !   Those become simply mapped onto the GOCART dust bins 1-4 
  !   (du001 ... du004).
  !
  ! - ss001-ss005 for the following five sea salt aerosol bins 
  !   (see SS_GridComp.rc
  !   in GOCART):
  !   radius_lower: 0.03 0.1 0.5 1.5 5.0
  !   radius_upper: 0.1  0.5 1.5 5.0 10.0
  !
  !   The GEOS-Chem sea salt aerosols are (SALA and SALC):
  !   radius_lower: 0.01 0.5
  !   radius_upper: 0.5  8.0
  !   SALA becomes mapped onto ss001 and ss002, and SALC onto ss003, ss004, 
  !   s005. For now, we assume uniform size distribution within the 
  !   GEOS-Chem bins, i.e. the GEOS-Chem size bins are evenly split into the 
  !   GOCART bins. The fractions can be specified below.
  !   At some point, we may revisit these fractions (at least take into 
  !   account the log-normal behavior of the aerosol distribution)
  INTEGER, PARAMETER           :: NumAERO = 11

  CHARACTER(LEN=ESMF_MAXSTR)   :: GcNames(NumAero) = &
                                  (/ 'DST1',  'DST2',  'DST3',  'DST4',     &
                                     'SALA',  'SALC',  'BCPO',  'BCPI',     &
                                     'OCPO',  'OCPI',  'SO4 '                /)

  CHARACTER(LEN=ESMF_MAXSTR)   :: AeroNames(NumAero) = &
                         (/ 'du001   ', 'du002   ', 'du003   ', 'du004   ', &
                            'ss001   ', 'ss003   ', 'BCphobic', 'BCphilic', &
                            'OCphobic', 'OCphilic', 'SO4     '               /)

  ! Fraction of SALA in ss001 and ss002, respectively
  CHARACTER(LEN=ESMF_MAXSTR) :: SALAnames(2) = (/ 'ss001', 'ss002' /)
  REAL, PARAMETER            :: SALAsplit(2) = (/  0.2,     0.8    /)

  ! Fraction of SALC in ss003, ss004, and ss005.
  CHARACTER(LEN=ESMF_MAXSTR) :: SALCnames(3) = (/ 'ss003', 'ss004', 'ss005' /)
  REAL, PARAMETER            :: SALCsplit(3) = (/  0.13,    0.47,    0.4    /) 

  CHARACTER(LEN=ESMF_MAXSTR)   :: DST4names(2) = (/ 'du004', 'du005' /)
  REAL, PARAMETER              :: DST4split(2) = (/  1.00,    0.0    /) 

  ! Molecular weights (g/mol) used by GOCART
  REAL                         :: GocartMW(NumAero) = &
                                  (/ 100.0,  100.0,  100.0,  100.0 ,     &
                                      58.0,   58.0,  180.0,  180.0 ,     &
                                     180.0,  180.0,  132.0                /)

  CHARACTER(LEN=15), PARAMETER :: COLLIST(8) = (/ 'NO2', 'O3',   'CH4', 'CO', &
                                                  'BrO', 'CH2O', 'SO2', 'IO'  /)

  ! Pointers for RATS and analysis OX. Those are not included in the GEOS-Chem
  ! registry and only filled if GEOS-Chem is the RATS and/or analysis OX 
  ! provider. The history arrays (*_HIST) are used to archive the O3 and 
  ! H2O fields from the previous chemistry time step.

! GCHP only:
!  ! -Analysis OX:
!  REAL, POINTER     :: O3      (:,:,:) => NULL()
!  REAL, POINTER     :: O3PPMV  (:,:,:) => NULL() ! was commented out (ewl)
!  REAL, POINTER     :: OX      (:,:,:) => NULL()
!  REAL, POINTER     :: OX_TEND (:,:,:) => NULL() ! was commented out (ewl)

!NOTE FROM LIZZIE: O3_HIST and H2O_HIST seem to have been deleted.
! I will remove them if in GCHP only. Keep H2O_TEND.
!---

  ! -RATS:
  REAL, POINTER     :: CH4     (:,:,:) => NULL()
  REAL, POINTER     :: N2O     (:,:,:) => NULL()
  REAL, POINTER     :: CFC11   (:,:,:) => NULL()
  REAL, POINTER     :: CFC12   (:,:,:) => NULL()
  REAL, POINTER     :: HCFC22  (:,:,:) => NULL()

! GCHP only:
!  REAL, POINTER     :: H2O_TEND(:,:,:) => NULL() ! was commented out (ewl)
!---

  ! -Corresponding pointers to internal state. We now use these variables 
  !  instead of the auto-generated pointers (GIGCchem_DeclarePointer___.h) 
  !  to avoid compilation errors if these species are not defined in 
  !  GEOS-Chem (e.g. for specialty sims). 
  REAL(ESMF_KIND_R8), POINTER :: PTR_O3      (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_CH4     (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_N2O     (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_CFC11   (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_CFC12   (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_HCFC22  (:,:,:) => NULL()
  REAL(ESMF_KIND_R8), POINTER :: PTR_H2O     (:,:,:) => NULL()

  ! GCCTO3 and GCCTTO3 are the pointers to the export state fields which
  ! are automatically declared if GCCTO3 and GCCTTO3 are in Chem_Registry.rc
  REAL, POINTER     :: PTR_GCCTO3 (:,:) => NULL()
  REAL, POINTER     :: PTR_GCCTTO3(:,:) => NULL()

  ! Perturb ozone?
  LOGICAL           :: PerturbO3
  LOGICAL           :: PerturbCO
  REAL              :: FIXPERT       ! Fixed perturbation 
  REAL, PARAMETER   :: MAXPERT = 0.1 ! Maximum perturbation

  ! For NOx diagnostics
  INTEGER           :: id_rc_no  = -1
  INTEGER           :: id_rc_no2 = -1
  INTEGER           :: id_jno2   = -1

  ! Mie table
  TYPE(Chem_Mie)     :: geoschemMieTable(2)
  INTEGER, PARAMETER :: instanceComputational = 1
  INTEGER, PARAMETER :: instanceData          = 2
!
! !REMARKS:
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!                                                                             .
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)
!  07 Jun 2018 - E. Lundgren - Updated for C. Keller's v11-02e in GEOS-5
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
! !IROUTINE: Provider_SetServices
!
! !DESCRIPTION:
!
!
! !INTERFACE:
!
  SUBROUTINE Provider_SetServices( am_I_Root, GC, isProvider, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,              INTENT(IN)    :: am_I_Root ! Root PET?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp),  INTENT(INOUT) :: GC        ! Ref to this GridComp
!
! !OUTPUT PARAMETERS:
!
    LOGICAL, INTENT(OUT)   :: isProvider ! Provider to AERO, RATS, or ANOX?
    INTEGER, INTENT(OUT)   :: RC        ! Success or failure
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)
!  07 Jun 2018 - E. Lundgren - Updated for C. Keller's v11-02e in GEOS-5
!EOP
!------------------------------------------------------------------------------
!BOC
! 
! !LOCAL VARIABLES:
!
    TYPE(ESMF_CONFIG)             :: CF
    CHARACTER(LEN=ESMF_MAXSTR)    :: ProviderName

    !=======================================================================
    ! Provider_SetServices begins here 
    !=======================================================================

    __Iam__('provider_SetServices')

    ! Initialize
    DoAERO = .FALSE.
    DoRATS = .FALSE.
    DoANOX = .FALSE.

    ! Get configuration
    CALL ESMF_GridCompGet( GC, CONFIG = CF, __RC__ )

    !================================
    ! Is GC the AERO provider?
    !================================
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,       &
                                  Label="AERO_PROVIDER:", &
                                  Default="PCHEM",        &
                                  __RC__                   )
    IF ( ProviderName == "GEOSCHEMCHEM" ) DoAERO = .TRUE.

    !================================
    ! Is GC the RATS provider?
    !================================
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,       &
                                  Label="RATS_PROVIDER:", &
                                  Default="PCHEM",        &
                                  __RC__                   )
    IF ( ProviderName == "GEOSCHEMCHEM" ) DoRATS = .TRUE.

    !================================
    ! Is GC the Analysis OX provider?
    !================================
    CALL ESMF_ConfigGetAttribute( CF, ProviderName,              &
                                  Label="ANALYSIS_OX_PROVIDER:", &
                                  Default="PCHEM",               &
                                  __RC__                          )
    IF ( ProviderName == "GEOSCHEMCHEM" ) DoANOX = .TRUE.

    !================================
    ! If AERO provider
    !================================
    ! Add AERO and AERO_DP bundles to export state
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

    ENDIF ! DoAERO

    !================================
    ! If RATS provider
    !================================
    ! If GEOS-Chem is the RATS provider, we need to make sure that all 
    ! RATS quantities are available to irradiation. We will get these 
    ! quantities directly from the GEOS-Chem internal state, except for 
    ! H2O_TEND that is calculated explicitly.
    ! Since those fields are just copies of the GEOS-Chem internal
    ! species, we add them as export specs, i.e. no physics is applied
    ! to those fields.
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
    ENDIF ! DoRATS

    ! Determine if GC is a provider
    IF ( DoAERO .OR. DoRATS .OR. DoANOX ) isProvider = .TRUE.

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_SetServices
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Provider_Initialize
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Provider_Initialize( am_I_Root, IM,       JM,     LM, &
                                  State_Chm, IntState, Export, RC )
!
! !USES:
!
    USE State_Chm_Mod, ONLY: ChmState, IND_
    USE GIGC_HistoryExports_Mod, ONLY: SPFX
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN)    :: am_I_Root ! Root PET?
    INTEGER,          INTENT(IN)    :: IM        ! # of longitudes on this PET
    INTEGER,          INTENT(IN)    :: JM        ! # of latitudes  on this PET
    INTEGER,          INTENT(IN)    :: LM        ! # of levels     on this PET
    TYPE(ChmState),   INTENT(IN)    :: State_Chm ! Chemistry State
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_State), INTENT(INOUT), TARGET :: IntState  ! Internal State
    TYPE(ESMF_State), INTENT(INOUT), TARGET :: Export    ! Export State
!                                                  
! !OUTPUT PARAMETERS:                              
!                                                  
    INTEGER,          INTENT(OUT)           :: RC        ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)
!  07 Jun 2018 - E. Lundgren - Updated for C. Keller's v11-02e in GEOS-5
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Scalars
    CHARACTER(LEN=ESMF_MAXSTR)   :: GCName, AeroName, Name
    INTEGER                      :: I, J, NFLDS, GCID, STAT, instance
    REAL                         :: GCMW, FRAC

    ! Derived types
    TYPE(ESMF_FieldBundle)       :: bundle, AeroBdl 
    TYPE(ESMF_Field)             :: field, AeroFld, GcFld

    ! Pointers
    REAL, POINTER                :: Ptr3D(:,:,:) => NULL()
    REAL(ESMF_KIND_R8), POINTER  :: Ptr3D_R8(:,:,:) => NULL()

    TYPE(ESMF_STATE)             :: Aero

    !=======================================================================
    ! Provider_Initialize begins here
    !=======================================================================

    __Iam__('Provider_Initialize')

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
       CALL ESMF_StateGet( Export, 'AERO', AeroBdl, __RC__ )

       ! This attribute indicates if the aerosol optics method is implemented 
       ! or not. Radiation will not call the aerosol optics method unless 
       ! this attribute is explicitly set to true.
       call ESMF_AttributeSet(aero, name='implements_aerosol_optics_method', &
                              value=.true., __RC__)
       aeroBdl = ESMF_FieldBundleCreate(name='AEROSOLS', __RC__)
       call MAPL_StateAdd(Aero, aeroBdl, __RC__)

       ! Loop over all GC tracers that we may want to add to the AERO
       ! bundle
       DO I = 1, NumAERO

          ! Get GEOS-Chem tracer ID
          GCID = IND_(TRIM(GcNames(I)))

          ! If species is defined, copy field and add to AERO bundle
          IF ( GCID > 0 ) THEN

             ! This is the name in the internal state
             GCName = TRIM(SPFX) // TRIM(GcNames(I))

             ! Get field from internal state
             CALL ESMF_StateGet( IntState, TRIM(GCName), GcFld, RC=RC )
             ! Try TRC_<NAME> if SPC_<NAME> not found
             IF ( RC /= ESMF_SUCCESS ) THEN
                GCName = 'TRC_'//TRIM(GcNames(I))
                CALL ESMF_StateGet( INTSTATE, TRIM(GCName), GcFld, RC=RC )
             ENDIF

             ! Error if none of the above found
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
             IF ( TRIM(GcNames(I)) == 'DST4' ) NFLDS = 2

             ! Now create all fields
             DO J = 1, NFLDS
 
                ! AERO field name
                AeroName = TRIM(AeroNames(I))
                IF ( TRIM(GcNames(I)) == 'SALA' ) AeroName = SALAnames(J)
                IF ( TRIM(GcNames(I)) == 'SALC' ) AeroName = SALCnames(J)
                IF ( TRIM(GcNames(I)) == 'DST4' ) AeroName = DST4names(J)

                ! Create new field
                AeroFld = MAPL_FieldCreate( GcFld, name=AeroName, &
                                            DoCopy=.TRUE., __RC__  )
      
                ! Get molecular weight (g/mol)
                !GCMW = Input_Opt%Tracer_MW_G(GCID)
                GCMW = GocartMW(I)  

                ! Fraction of the GC field to be used in the AERO field
                FRAC = 1.0
                IF ( TRIM(GcNames(I)) == 'SALA' ) FRAC = SALAsplit(J)
                IF ( TRIM(GcNames(I)) == 'SALC' ) FRAC = SALCsplit(J)
                IF ( TRIM(GcNames(I)) == 'DST4' ) FRAC = DST4split(J)

                ! Pass GEOS-Chem field name, molecular weight and fraction 
                ! to be used to bundle for easier handling lateron
                CALL ESMF_AttributeSet ( AeroFld, NAME='GCNAME', &
                                         VALUE=GCName, __RC__ ) 
                CALL ESMF_AttributeSet ( AeroFld, NAME='GCMW',   &
                                         VALUE=GCMW,   __RC__ ) 
                CALL ESMF_AttributeSet ( AeroFld, NAME='FRAC',   &
                                         VALUE=FRAC,   __RC__ ) 
      
                ! Before adding to the bundle, convert data from mol/mol 
                ! to kg/kg
                ! Data is now stored in kg/kg total. (ckeller, 3/7/17)
                CALL ESMF_FieldGet( AeroFld, farrayPtr=Ptr3D, __RC__ )
                !Ptr3D = Ptr3D * GCMW / MAPL_AIRMW * FRAC
                Ptr3D = Ptr3D * FRAC
                Ptr3D => NULL()
   
                ! Add to bundle
                CALL MAPL_FieldBundleAdd ( AeroBdl, AeroFld, __RC__ )
             ENDDO !J
          ENDIF
       ENDDO

! EWL: Comment out this new code for now. It has some MAPL dependencies
! that need to be sorted out.
!       ! Mie table
!       instance = instanceComputational
!       geoschemMieTable(instance) = Chem_MieCreate(MaplCF, __RC__)
!       name = 'mie_table_instance'
!       CALL ESMF_AttributeSet(aero, name=name, value=instance, __RC__)
!
!       ! state of the atmosphere
!       name = 'air_pressure_for_aerosol_optics'
!       CALL ESMF_AttributeSet(aero, name=name, value='PLE', __RC__)
!       name = 'relative_humidity_for_aerosol_optics'
!       CALL ESMF_AttributeSet(aero, name=name, value='RH',  __RC__)
!       name = 'cloud_area_fraction_for_aerosol_optics'
!       CALL ESMF_AttributeSet(aero, name=name, value='',    __RC__) 
!       ! 'cloud_area_fraction_in_atmosphere_layer_for_aerosol_optics'
!
!       ! aerosol optics
!       name = 'band_for_aerosol_optics'
!       CALL ESMF_AttributeSet(aero, name=name, value=0,     __RC__)
!       name = 'extinction_in_air_due_to_ambient_aerosol'
!       CALL ESMF_AttributeSet(aero, name=name, value='EXT', __RC__)
!       name = 'single_scattering_albedo_of_ambient_aerosol'
!       CALL ESMF_AttributeSet(aero, name=name, value='SSA', __RC__)
!       name = 'asymmetry_parameter_of_ambient_aerosol'
!       CALL ESMF_AttributeSet(aero, name=name, value='ASY', __RC__)
!
!       ! add PLE to Aero state
!       name = 'air_pressure_for_aerosol_optics'
!       CALL ESMF_AttributeGet(aero, name=name, value=fieldName, __RC__)
!       if (fieldName /= '') THEN
!          aeroFld = MAPL_FieldCreateEmpty(TRIM(fieldName), grid, __RC__)
!          CALL MAPL_FieldAllocCommit(aeroFld,                           &
!                                     dims=MAPL_DimsHorzVert,            &
!                                     location=MAPL_VLocationEdge,       &
!                                     typekind=MAPL_R4,                  &
!                                     hw=0,                              &
!                                      __RC__)
!          CALL MAPL_StateAdd(aero, aeroFld, __RC__)
!       end if
!
!       ! add RH to Aero state
!       name = 'relative_humidity_for_aerosol_optics'
!       CALL ESMF_AttributeGet(aero, name=name, value=fieldName, __RC__)
!       if (fieldName /= '') THEN
!          aeroFld = MAPL_FieldCreateEmpty(TRIM(fieldName), grid, __RC__)
!          CALL MAPL_FieldAllocCommit(aeroFld,                           &
!                                     dims=MAPL_DimsHorzVert,            &
!                                     location=MAPL_VLocationCenter,      &
!                                     typekind=MAPL_R4,                  &
!                                     hw=0,                              &
!                                     __RC__)
!          CALL MAPL_StateAdd(aero, aeroFld, __RC__)
!       end if
!
!       ! add EXT to Aero state
!       name = 'extinction_in_air_due_to_ambient_aerosol'
!       CALL ESMF_AttributeGet(aero, name=name, value=fieldName, __RC__)
!       if (fieldName /= '') THEN
!          aeroFld = MAPL_FieldCreateEmpty(TRIM(fieldName), grid, __RC__)
!          CALL MAPL_FieldAllocCommit(aeroFld,                           &
!                                     dims=MAPL_DimsHorzVert,            &
!                                     location=MAPL_VLocationCenter,     &
!                                     typekind=MAPL_R4,                  &
!                                     hw=0,                              &
!                                     __RC__)
!          CALL MAPL_StateAdd(aero, aeroFld, __RC__)
!       end if
!
!       ! add SSA to aero state
!       name = 'single_scattering_albedo_of_ambient_aerosol'
!       CALL ESMF_AttributeGet(aero, name=name, value=fieldName, __RC__)
!       if (fieldName /= '') THEN
!          aeroFld = MAPL_FieldCreateEmpty(TRIM(fieldName), grid, __RC__)
!          CALL MAPL_FieldAllocCommit(aeroFld,                           &
!                                     dims=MAPL_DimsHorzVert,            &
!                                     location=MAPL_VLocationCenter,     &
!                                     typekind=MAPL_R4,                  &
!                                     hw=0,                              &
!                                     __RC__)
!          CALL MAPL_StateAdd(aero, aeroFld, __RC__)
!       end if
!
!       ! add ASY to aero state
!       name = 'asymmetry_parameter_of_ambient_aerosol'
!       CALL ESMF_AttributeGet(aero, name=name, value=fieldName, __RC__)
!       IF (fieldName /= '') THEN
!          aeroFld = MAPL_FieldCreateEmpty(TRIM(fieldName), grid, __RC__)
!          CALL MAPL_FieldAllocCommit(aeroFld,                           &
!                                     dims=MAPL_DimsHorzVert,            &
!                                     location=MAPL_VLocationCenter,     &
!                                     typekind=MAPL_R4,                  &
!                                     hw=0,                              &
!                                     __RC__)
!          CALL MAPL_StateAdd(aero, aeroFld, __RC__)
!       ENDIF
!
!       ! attach the aerosol optics method
!       CALL ESMF_MethodAdd(aero, label='aerosol_optics',                &
!                           userRoutine=aerosol_optics, __RC__)

       ! ---------------------------------------------------------------------
       ! Initialize the AERO_DP bundle
       ! ---------------------------------------------------------------------
       CALL ESMF_StateGet( Export, 'AERO_DP', AeroBdl, __RC__ )

       ! Dust dry and wet deposition 
       CALL ESMF_StateGet( Export, 'DUDP_DST1', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUDP_DST2', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUDP_DST3', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUDP_DST4', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( Export, 'DUWT_DST1', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUWT_DST2', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUWT_DST3', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'DUWT_DST4', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       ! Black carbon dry and wet depostion 
       CALL ESMF_StateGet( Export, 'BCDP_BCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( Export, 'BCDP_BCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( Export, 'BCWT_BCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'BCWT_BCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       ! Organic carbon dry and wet depostion 
       CALL ESMF_StateGet( Export, 'OCDP_OCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( Export, 'OCDP_OCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )
      
       CALL ESMF_StateGet( Export, 'OCWT_OCPI', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

       CALL ESMF_StateGet( Export, 'OCWT_OCPO', AeroFld, __RC__ )
       CALL MAPL_FieldBundleAdd( AeroBdl, AeroFld, __RC__ )

    ENDIF ! DoAERO

    CALL MAPL_GetPointer( INTSTATE, PTR_O3, 'TRC_O3', NotFoundOk=.TRUE., &
                           __RC__ )

    IF ( DoRATS ) THEN
       CALL MAPL_GetPointer( INTSTATE,    PTR_CH4, 'TRC_CH4',    __RC__ )
       CALL MAPL_GetPointer( INTSTATE,    PTR_N2O, 'TRC_N2O',    __RC__ )
       CALL MAPL_GetPointer( INTSTATE,  PTR_CFC11, 'TRC_CFC11',  __RC__ )
       CALL MAPL_GetPointer( INTSTATE,  PTR_CFC12, 'TRC_CFC12',  __RC__ )
       CALL MAPL_GetPointer( INTSTATE, PTR_HCFC22, 'TRC_HCFC22', __RC__ )
       CALL MAPL_GetPointer( INTSTATE,    PTR_H2O, 'TRC_H2O',    __RC__ )
    ENDIF

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_Initialize
!EOC

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Provider_SetPointers
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Provider_SetPointers( am_I_Root, Export, calcOzone, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN)         :: am_I_Root ! Root PET?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_State), INTENT(INOUT), TARGET :: Export    ! Export State
!
! !OUTPUT PARAMETERS:
!
    LOGICAL,          INTENT(OUT)        :: calcOzone ! PTR_GCCTO3 assoc?
    INTEGER,          INTENT(OUT)        :: RC        ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)
!  07 Jun 2018 - E. Lundgren - Updated for C. Keller's v11-02e in GEOS-5
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    !=======================================================================
    ! Provider_SetPointers begins here
    !=======================================================================

    __Iam__('Provider_SetPointers')

    ! Get pointers to fields in export states. This has to be done on the 
    ! first call only.

! EWL: not sure if this is still used so comment out for now
!    ! Get pointers to analysis OX exports
!    IF ( DoANOX ) THEN
!       CALL MAPL_GetPointer ( Export, OX_TEND, 'OX_TEND' , __RC__ )
!       CALL MAPL_GetPointer ( Export,      OX, 'OX'      , __RC__ )
!       CALL MAPL_GetPointer ( Export,      O3, 'O3'      , __RC__ )
!       CALL MAPL_GetPointer ( Export,  O3PPMV, 'O3PPMV'  , __RC__ )
!    ENDIF
!
!    ! Get pointers to RATS exports
!    IF ( DoRATS) THEN
!       CALL MAPL_GetPointer ( Export, H2O_TEND, 'H2O_TEND' , __RC__ )
!       CALL MAPL_GetPointer ( Export,      CH4, 'CH4'      , __RC__ )
!       CALL MAPL_GetPointer ( Export,      N2O, 'N2O'      , __RC__ )
!       CALL MAPL_GetPointer ( Export,    CFC11, 'CFC11'    , __RC__ )
!       CALL MAPL_GetPointer ( Export,    CFC12, 'CFC12'    , __RC__ )
!       CALL MAPL_GetPointer ( Export,   HCFC22, 'HCFC22'   , __RC__ )
!    ENDIF

    ! Eventually get pointers to GCCTO3 and GCCTTO3. Those fields are 
    ! optional and are only filled if defined and required.
    CALL MAPL_GetPointer ( Export, PTR_GCCTO3,   'GCCTO3', &
                           notFoundOK=.TRUE., __RC__ )
    CALL MAPL_GetPointer ( Export, PTR_GCCTTO3, 'GCCTTO3', &
                           notFoundOK=.TRUE., __RC__ )

! GCHP only (but use might be necessary now so just set to false):
    calcOzone = .FALSE.
!    IF ( ASSOCIATED( PTR_GCCTO3 ) ) THEN
!       calcOzone = .TRUE.
!    ENDIF
!---

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_SetPointers
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Provider_FillBundles
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Provider_FillBundles( am_I_Root, tsChem,    PLE, GCCTROPP, &
                                   STATE,     Input_Opt, GC,  Export,  RC )
!
! !USES:
!
    USE Input_Opt_Mod, ONLY : OptInput
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN) :: am_I_Root       ! Root PET?
    REAL,                INTENT(IN) :: tsChem          ! Chemistry timestep
    REAL(ESMF_KIND_R8),  POINTER    :: PLE  (:,:,:)    ! Pressure level edges
    REAL,                POINTER    :: GCCTROPP(:,:  ) ! Tropopause pressures
    TYPE(MAPL_MetaComp), POINTER    :: STATE
    TYPE(OptInput),      INTENT(IN) :: Input_Opt       ! Input Options
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC        ! Ref to GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export    ! Export State
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)           :: RC        ! Success or fail?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)

!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Scalars
    INTEGER                      :: N, nAero
    CHARACTER(LEN=ESMF_MAXSTR)   :: GcName
    REAL                         :: GCMW, FRAC

    ! Pointers
    REAL, POINTER                :: GcPtr3d  (:,:,:) => NULL()
    REAL, POINTER                :: AeroPtr3d(:,:,:) => NULL()

    ! Derived types
    TYPE(ESMF_STATE)             :: IntState, Aero
    TYPE(ESMF_FieldBundle)       :: AeroBdl 
    TYPE(ESMF_Field)             :: AeroFld

    !=======================================================================
    ! Provider_FillBundles begins here
    !=======================================================================

    __Iam__('Provider_FillBundles')

    !====================================================================
    ! Fill ozone export states if GC is the analysis OX provider:
    !      OX: volume mixing ratio
    !      O3: mass mixing ratio
    !  O3PPMV: volume mixing ratio in ppm
    ! OX_TEND: mol mol-1 s-1
    !
    ! GEOS-Chem tracer:
! GEOS-5:
    ! PTR_O3: kg kg-1 total air
! GCHP has different units:
    ! PTR_O3: mol mol-1
!---
    !====================================================================
! GCHP only:
!    IF ( DoANOX ) THEN
!---

! GEOS-5:
! EWL: this doesn't quite work yet so comment out for now:
!    IF ( ASSOCIATED(O3     ) ) O3      = PTR_O3 
!    IF ( ASSOCIATED(OX     ) ) OX      = PTR_O3 * MAPL_AIRMW / MAPL_O3MW
!    IF ( ASSOCIATED(O3PPMV ) ) O3PPMV  = PTR_O3 * MAPL_AIRMW / MAPL_O3MW * 1.00E+06
! GCHP is slightly different:
!       IF ( ASSOCIATED(OX     ) ) OX      = PTR_O3
!       IF ( ASSOCIATED(O3     ) ) O3      = PTR_O3 * MAPL_O3MW / MAPL_AIRMW
!       IF ( ASSOCIATED(O3PPMV ) ) O3PPMV  = PTR_O3 * 1.00E+06
!---


! GCHP only:    
!       ! Get tendencies. Also store current O3 field in O3_HIST for use in 
!       ! next chemistry time step.
!       IF ( ASSOCIATED(OX_TEND) ) THEN
!          OX_TEND = ( PTR_O3 - O3_HIST ) / tsChem
!          O3_HIST = PTR_O3
!       ENDIF
!    ENDIF
!---
    
    !====================================================================
    ! Fill RATS export states if GC is the RATS provider
    ! The tracer concentrations of the RATS export states are in mol mol-1,
! GCHP only: 
!    ! exactly the same as the GC internal values. 
!    ! PTR_H2O is in mol mol-1. Convert to kg here.
!---
    !====================================================================
! GCHP only:
!    IF ( DoRATS ) THEN
!---

! GEOS-5:
    IF ( ASSOCIATED(CH4   ) )    CH4 = PTR_CH4    * MAPL_AIRMW /  16.00
    IF ( ASSOCIATED(N2O   ) )    N2O = PTR_N2O    * MAPL_AIRMW /  44.00
    IF ( ASSOCIATED(CFC11 ) )  CFC11 = PTR_CFC11  * MAPL_AIRMW / 137.37
    IF ( ASSOCIATED(CFC12 ) )  CFC12 = PTR_CFC12  * MAPL_AIRMW / 120.91
    IF ( ASSOCIATED(HCFC22) ) HCFC22 = PTR_HCFC22 * MAPL_AIRMW /  86.47
! GCHP uses different equations:
!       IF ( ASSOCIATED(CH4   ) )    CH4 = PTR_CH4
!       IF ( ASSOCIATED(N2O   ) )    N2O = PTR_N2O
!       IF ( ASSOCIATED(CFC11 ) )  CFC11 = PTR_CFC11
!       IF ( ASSOCIATED(CFC12 ) )  CFC12 = PTR_CFC12
!       IF ( ASSOCIATED(HCFC22) ) HCFC22 = PTR_HCFC22
!---  
  
! GCHP only:
!       ! Get tendencies only on chemistry time step. Also store current H2O 
!       ! field in H2O_HIST for use in next chemistry time step.
!       IF ( ASSOCIATED(H2O_TEND) ) THEN
!          H2O_TEND = ( PTR_H2O - H2O_HIST ) * MAPL_H2OMW / MAPL_AIRMW   &
!                      / tsChem
!          H2O_HIST = PTR_H2O
!       ENDIF
!    ENDIF
!---
    
    !====================================================================
    ! Fill AERO bundle if GEOS-Chem is the AERO provider.
    ! For every field of the AERO bundle, we will copy the corresponding
    ! GEOS-Chem tracer field, converting units from mol mol-1 to kg kg-1.
    !====================================================================
    IF ( DoAERO ) THEN

       ! Get Internal state
       CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTSTATE, __RC__ ) 

       ! Get AERO bundle
       CALL ESMF_StateGet( EXPORT, 'AERO',     Aero,    __RC__ )
       CALL ESMF_StateGet( Aero,   'AEROSOLS', AeroBdl, __RC__ )

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
          !AeroPtr3D = GcPtr3D * FRAC * GCMW / MAPL_AIRMW
          AeroPtr3D = GcPtr3D * FRAC

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
       CALL FillAeroDP ( am_I_Root, Input_Opt, GC, EXPORT, __RC__ )
    ENDIF ! DoAero

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_FillBundles
!EOC

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Provider_ZeroTendencies
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Provider_ZeroTendencies( am_I_Root, RC )
!
! !INPUT PARAMETERS:
!
    LOGICAL, INTENT(IN)    :: am_I_Root ! Root PET?
!
! !OUTPUT PARAMETERS:
!
    INTEGER, INTENT(OUT)   :: RC        ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - initial version (refactored Chem_GridCompMod)
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    !=======================================================================
    ! Provider_ZeroTendencies begins here
    !=======================================================================

    __Iam__('Provider_ZeroTendencies')

! EWL: not sure if this is relevant anymore so comment out for now
!    IF ( DoANOX ) THEN
!       IF ( ASSOCIATED(OX_TEND) ) OX_TEND = 0.0 
!    ENDIF
!    IF ( DoRATS ) THEN
!       IF ( ASSOCIATED(H2O_TEND) ) H2O_TEND  = 0.0 
!    ENDIF

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_ZeroTendencies
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: CalcTotalOzone 
!
! !DESCRIPTION: CalcTotalOzone calculates total ozone for the entire
!  atmosphere and troposphere only (in dobsons) and writes them into
!  the export variables GCCTO3 and GCCPTR_GCCTTO3, respectively.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE CalcTotalOzone ( am_I_Root, PLE, GCCTROPP, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,            INTENT(IN) :: am_I_Root       ! Root PET?
    REAL(ESMF_KIND_R8), POINTER    :: PLE  (:,:,:)    ! Pressure level edges
    REAL,               POINTER    :: GCCTROPP(:,:  ) ! Tropopause pressures
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER, INTENT(OUT), OPTIONAL :: RC             ! Success or failure?
!
! !REVISION HISTORY:
!  25 Oct 2014 - C. Keller   - Initial version
!  02 Nov 2017 - E. Lundgren - Move to this module with structural edits
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    INTEGER            :: L, IM, JM, LM, STAT
    REAL,  ALLOCATABLE :: DUsLayerL(:,:)       ! Dobsons in a layer
    REAL,  ALLOCATABLE :: wgt(:,:)             ! Layer thickness weighting

    !=======================================================================
    ! CalcTotalOzone begins here
    !=======================================================================

    __Iam__('CalcTotalOzone')

    ! Nothing to do if neither of the arrays is associated
    IF ( .NOT. ASSOCIATED(PTR_GCCTO3) .AND.   &
         .NOT. ASSOCIATED(PTR_GCCTTO3) ) THEN
       RC = ESMF_SUCCESS
       RETURN
    ENDIF

    ! Grid size
    IM = SIZE(PTR_O3,1)
    JM = SIZE(PTR_O3,2)
    LM = SIZE(PTR_O3,3)

    ! Allocate local variables
    ALLOCATE( DUsLayerL(IM,JM), STAT=STAT )
    VERIFY_(STAT)
    ALLOCATE( wgt(IM,JM), STAT=STAT )
    VERIFY_(STAT)
 
    ! Calculate total ozone
    DO L = 1,LM 
       DUsLayerL(:,:) = PTR_O3(:,:,L) * ( PLE(:,:,L) - PLE(:,:,L-1) )   &
                        * ( MAPL_AVOGAD / 2.69E+20 )                    & 
                        / ( MAPL_AIRMW * MAPL_GRAV )
       IF ( ASSOCIATED(PTR_GCCTO3) ) PTR_GCCTO3 = PTR_GCCTO3 + DUsLayerL
       IF ( ASSOCIATED(PTR_GCCTTO3) ) THEN
          wgt  = MAX( 0.0, MIN( 1.0, ( PLE(:,:,L) - GCCTROPP(:,:) )    &
                 / ( PLE(:,:,L) - PLE(:,:,L-1) ) ) )
          PTR_GCCTTO3 = PTR_GCCTTO3 + DUsLayerL * wgt
       ENDIF
    ENDDO
 
    ! Cleanup
    DEALLOCATE(DUsLayerL, STAT=STAT)
    VERIFY_(STAT)
    DEALLOCATE(wgt, STAT=STAT)
    VERIFY_(STAT)

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE CalcTotalOzone
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetStateMetTO3
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE SetStateMetTO3 ( am_I_Root, State_Met, RC )
!
! !USES
!
  USE State_Met_Mod
!
! !INPUT PARAMETERS:
!
    LOGICAL,         INTENT(IN)    :: am_I_Root
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                         
!              
    TYPE(MetState),  INTENT(INOUT) :: State_Met
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER, INTENT(OUT), OPTIONAL :: RC
!
! !REVISION HISTORY:
!  25 Oct 2014 - C. Keller   - Initial version
!  02 Nov 2017 - E. Lundgren - Move to this module with structural edits
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    !=======================================================================
    ! SetStateMetTO3 begins here
    !=======================================================================

    __Iam__('SetStateMetTO3')

    ! Only set State_Met%TO3 if PTR_GCCTO3 is associated
    IF ( ASSOCIATED(PTR_GCCTO3) ) THEN
       State_Met%TO3 = PTR_GCCTO3
    ENDIF

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE SetStateMetTO3
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Provider_Finalize
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Provider_Finalize( am_I_Root, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL, INTENT(IN)  :: am_I_Root ! root PET?
!
! !OUTPUT PARAMETERS:
!
    INTEGER, INTENT(OUT) :: RC        ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  02 Nov 2017 - E. Lundgren - Move to this module with structural edits
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    !=======================================================================
    ! Provider_Finalize begins here
    !=======================================================================

    __Iam__('Provider_Finalize')

    ! Free local pointers
! EWL: comment out these for now pending more thorough comparison of old vs new
!    O3          => NULL()
!    O3PPMV      => NULL()
!    OX          => NULL()
!    OX_TEND     => NULL()
    CH4         => NULL()
    N2O         => NULL()
    CFC11       => NULL()
    CFC12       => NULL()
    HCFC22      => NULL()
!    H2O_TEND    => NULL()
    PTR_O3      => NULL()
    PTR_CH4     => NULL()
    PTR_N2O     => NULL()
    PTR_CFC11   => NULL()
    PTR_CFC12   => NULL()
    PTR_HCFC22  => NULL()
    PTR_H2O     => NULL()
    PTR_GCCTO3  => NULL()
    PTR_GCCTTO3 => NULL()

!    ! Deallocate arrays
!    IF ( ASSOCIATED(  O3_HIST ) ) DEALLOCATE( O3_HIST )
!    IF ( ASSOCIATED( H2O_HIST ) ) DEALLOCATE( H2O_HIST )

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE Provider_Finalize
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
  SUBROUTINE FillAeroDP ( am_I_Root, Input_Opt, GC, Export, RC ) 
!
! !USES:
!
    USE HCO_ERROR_MOD
    USE HCO_DIAGN_MOD,     ONLY : Diagn_Get
    USE HCO_INTERFACE_MOD, ONLY : HcoState
    USE HCO_TYPES_MOD,     ONLY : DiagnCont
    USE Input_Opt_Mod,     ONLY : OptInput
    USE State_Chm_Mod,     ONLY : IND_
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN)    :: am_I_Root ! Root PET?
    TYPE(OptInput),      INTENT(IN)    :: Input_Opt
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                         
!              
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC        ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Export    ! Export State
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER, INTENT(OUT), OPTIONAL     :: RC        ! Success or failure?
!
! !REVISION HISTORY:
!  30 Mar 2015 - C. Keller   - Initial version
!  02 Nov 2017 - E. Lundgren - Move to this module with structural edits
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Scalaras
    INTEGER                      :: I, J, N, TrcID
    CHARACTER(LEN= 2)            :: Prfx 
    CHARACTER(LEN=15)            :: TrcName 
    CHARACTER(LEN=ESMF_MAXSTR)   :: ExpName 

    ! Pointers
    REAL, POINTER                :: Ptr2d(:,:) => NULL()

    ! Hemco diagnostics
    INTEGER                      :: DgnID
    INTEGER                      :: FLAG, ERR
    TYPE(DiagnCont), POINTER     :: DgnCont => NULL()

    !=======================================================================
    ! FillAeroDP begins here
    !=======================================================================

    __Iam__('FillAeroDP')

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
       TrcID = IND_( TRIM(TrcName) )

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
          CALL MAPL_GetPointer( Export, Ptr2D, TRIM(ExpName),  &
                                notFoundOk=.TRUE., __RC__ )

          ! Skip if not defined
          IF ( .NOT. ASSOCIATED(Ptr2D) ) CYCLE             

          ! Reset
          Ptr2D = 0.0

          ! ------------------
          ! Dry deposition
          ! ------------------
          IF ( I == 1 ) THEN
            
             ! Get diagnostics 
             DgnID = 44500 + TrcID
             CALL Diagn_Get( am_I_Root, HcoState, .FALSE., DgnCont,  &
                             FLAG, ERR, cID=DgnID, AutoFill=-1,      &
                             COL=Input_Opt%DIAG_COLLECTION ) 

             ! Error check 
             ASSERT_( ERR == HCO_SUCCESS )

             ! Add to array if diagnostics is defined
             ! GEOS-Chem diagnostics is in kg m-2 s-1.
             IF ( FLAG == HCO_SUCCESS ) THEN
                IF ( ASSOCIATED(DgnCont%Arr2D%Val) ) THEN
                   Ptr2D = Ptr2D + DgnCont%Arr2D%Val

                ENDIF
             ENDIF

          ! ------------------
          ! Wet depostion 
          ! ------------------
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
                CALL Diagn_Get( am_I_Root, HcoState, .FALSE., DgnCont, &
                                FLAG, ERR, cID=DgnID, AutoFill=-1,     &
                                COL=Input_Opt%DIAG_COLLECTION ) 

                ! Error check 
                ASSERT_( ERR == HCO_SUCCESS )

                ! Add to array if diagnostics is defined. GEOS-Chem diagnostics
                ! is already in kg m-2 s-1.
                IF ( FLAG == HCO_SUCCESS ) THEN
                   IF ( ASSOCIATED(DgnCont%Arr2D%Val) ) THEN
                      Ptr2D = Ptr2D + DgnCont%Arr2D%Val
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
END MODULE gigc_providerservices_mod
