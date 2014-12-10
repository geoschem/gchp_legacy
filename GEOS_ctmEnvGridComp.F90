#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!         NASA/GSFC, Software Systems Support Office, Code 610.3         !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: GEOS_ctmEnvGridComp -- Prepares derived variables for GEOSctm
!
! !INTERFACE:
!
      module GEOS_ctmEnvGridComp
!
! !USES:
      use ESMF
      use MAPL_Mod
      use FV_StateMod, only : calcCourantNumberMassFlux => fv_computeMassFluxes
      use m_set_eta,  only : set_eta

      implicit none
      private

! !PUBLIC MEMBER FUNCTIONS:

      public SetServices
      public compAreaWeightedAverage

      interface compAreaWeightedAverage
         module procedure compAreaWeightedAverage_2d
         module procedure compAreaWeightedAverage_3d
      end interface
!
! !DESCRIPTION:
! This GC is used to derive variables needed by the CTM GC children.
!
! !AUTHORS:
! Jules.Kouatchou-1@nasa.gov
! Michael Long (mlong@seas.harvard.edu)
!
! !REVISION HISTORY:
!  08 Sep 2014 - M. Long - Modification to calculate pressure at vertical
!                          grid edges from hybrid coordinates
!
!EOP
!-------------------------------------------------------------------------
      integer,  parameter :: r8     = 8
      integer,  parameter :: r4     = 4

      INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,30)
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14,300)
      INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(18,400)

      real(r8), parameter :: RADIUS = MAPL_RADIUS
      real(r8), parameter :: PI     = MAPL_PI_R8
      real(r8), parameter :: D0_0   = 0.0_r8
      real(r8), parameter :: D0_5   = 0.5_r8
      real(r8), parameter :: D1_0   = 1.0_r8
      real(r8), parameter :: GPKG   = 1000.0d0
      real(r8), parameter :: MWTAIR =   28.96d0

!-------------------------------------------------------------------------
      CONTAINS
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices -- Sets ESMF services for this component
!
! !INTERFACE:
!
      subroutine SetServices ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
      integer, intent(OUT)               :: RC  ! return code
!
! !LOCAL VARIABLES:
      type (ESMF_State)                  :: INTERNAL
!
! !DESCRIPTION:  
!   The SetServices for the CTM Der GC needs to register its
!   Initialize and Run.  It uses the MAPL\_Generic construct for defining 
!   state specs. 
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer                    :: STATUS
      integer                    :: I
      type (ESMF_Config)         :: CF
      character(len=ESMF_MAXSTR) :: COMP_NAME
      character(len=ESMF_MAXSTR) :: IAm = 'SetServices'

     ! Get my name and set-up traceback handle
     ! ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

     ! Register services for this component
     ! ------------------------------------
      call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, __RC__ )
      call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run,        __RC__ )

! !IMPORT STATE:

      call MAPL_AddImportSpec(GC,                              &
           SHORT_NAME        = 'AREA',                         &
           LONG_NAME         = 'agrid_cell_area',              &
           UNITS             = 'm+2',                          &
           DIMS              = MAPL_DimsHorzOnly,              &
           VLOCATION         = MAPL_VLocationNone,    RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'PS0',                                       &
           LONG_NAME  = 'pressure_at_surface_before_advection',      &
           UNITS      = 'hPa',                                       &
           DIMS       = MAPL_DimsHorzOnly,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'PS1',                                       &
           LONG_NAME  = 'pressure_at_surfaces_after_advection',      &
           UNITS      = 'hPa',                                       &
           DIMS       = MAPL_DimsHorzOnly,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)

!      call MAPL_AddImportSpec(GC,                                    &
!           SHORT_NAME = 'TH',                                        &
!           LONG_NAME  = 'potential_temperature',                     &
!           UNITS      = 'K',                                         &
!           DIMS       =  MAPL_DimsHorzVert,                          &
!           VLOCATION  =  MAPL_VLocationCenter,            RC=STATUS  )
!      VERIFY_(STATUS)
!
!      call MAPL_AddImportSpec(GC,                                    &
!           SHORT_NAME = 'Q',                                         &
!           LONG_NAME  = 'specific_humidity',                         &
!           UNITS      = 'kg kg-1',                                   &
!          DIMS       = MAPL_DimsHorzVert,                            &
!          VLOCATION  = MAPL_VLocationCenter,              RC=STATUS  )
!      VERIFY_(STATUS)
!
!     call MAPL_AddImportSpec(GC,                                    &
!          SHORT_NAME         = 'ZLE',                               &
!          LONG_NAME          = 'geopotential_height',               &
!          UNITS              = 'm',                                 &
!          DIMS               = MAPL_DimsHorzVert,                   &
!          VLOCATION          = MAPL_VLocationEdge,       RC=STATUS  )
!      VERIFY_(STATUS)
!
!      call MAPL_AddImportSpec ( gc,                                  &
!           SHORT_NAME = 'DELP',                                      &
!           LONG_NAME  = 'pressure_thickness',                        &
!           UNITS      = 'Pa',                                        &
!           DIMS       = MAPL_DimsHorzVert,                           &
!           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
!      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'UC0',                                       &
           LONG_NAME  = 'eastward_wind_on_C-Grid_before_advection',  &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             & 
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'UC1',                                       &
           LONG_NAME  = 'eastward_wind_on_C-Grid_after_advection',   &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'VC0',                                       &
           LONG_NAME  = 'northward_wind_on_C-Grid_before_advection', &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'VC1',                                       &
           LONG_NAME  = 'northward_wind_on_C-Grid_after_advection',  &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

! Export State
      call MAPL_AddExportSpec(GC,                            &
        SHORT_NAME         = 'AIRDENS',                      &
        LONG_NAME          = 'air_density',                  &
        UNITS              = 'kg m-3',                       &
        DIMS               = MAPL_DimsHorzVert,              &
        VLOCATION          = MAPL_VLocationCenter,  RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddExportSpec(GC,                            &
        SHORT_NAME         = 'MASS',                         &
        LONG_NAME          = 'total_mass',                   &
        UNITS              = 'kg',                           &
        DIMS               = MAPL_DimsHorzVert,              &
        VLOCATION          = MAPL_VLocationCenter,  RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'CXr8',                                      &
           LONG_NAME  = 'eastward_accumulated_courant_number',       &
           UNITS      = '',                                          &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'CYr8',                                      &
           LONG_NAME  = 'northward_accumulated_courant_number',      &
           UNITS      = '',                                          &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'MFXr8',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_eastward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'MFYr8',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_northward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

!---------------------------------------------------------------------
      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'CX',                                      &
           LONG_NAME  = 'eastward_accumulated_courant_number',       &
           UNITS      = '',                                          &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'CY',                                      &
           LONG_NAME  = 'northward_accumulated_courant_number',      &
           UNITS      = '',                                          &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)
      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'MFX',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_eastward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'MFY',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_northward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
      VERIFY_(STATUS)
!---------------------------------------------------------------------

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'PLE1r8',                                    &
           LONG_NAME  = 'pressure_at_layer_edges_after_advection',   &
           UNITS      = 'Pa',                                        &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'PLE0r8',                                    &
           LONG_NAME  = 'pressure_at_layer_edges_before_advection',  &
           UNITS      = 'Pa',                                        &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)

      call MAPL_AddExportSpec ( gc,                                  &
           SHORT_NAME = 'PLE',                                       &
           LONG_NAME  = 'pressure_at_layer_edges',                   &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)

      ! Internal State - MSL
      !-------------------------
      ! Store internal state with Config object in the gridded component
      CALL ESMF_UserCompSetInternalState( GC, 'ctmEnv_State', INTERNAL, STATUS )
      VERIFY_(STATUS)
      call MAPL_AddInternalSpec ( gc,                                &
           SHORT_NAME = 'PLE0',                                      &
           LONG_NAME  = 'pressure_at_layer_edges_before_advection',  &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)
      call MAPL_AddInternalSpec ( gc,                               &
           SHORT_NAME = 'PLE1',                                      &
           LONG_NAME  = 'pressure_at_layer_edges_after_advection',   &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      VERIFY_(STATUS)


      ! Set the Profiling timers
      !-------------------------
      call MAPL_TimerAdd(GC,    name="INITIALIZE"  ,RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_TimerAdd(GC,    name="RUN"         ,RC=STATUS)
      VERIFY_(STATUS)

      ! Create children's gridded components and invoke their SetServices
      ! -----------------------------------------------------------------
      call MAPL_GenericSetServices    ( GC, RC=STATUS )
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
  
      end subroutine SetServices
!
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize -- Initialized method for composite the CTMder
!
! !INTERFACE:
!
      subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      type(ESMF_State),    intent(inout) :: IMPORT ! Import state
      type(ESMF_State),    intent(inout) :: EXPORT ! Export state
      type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
!
! !OUTPUT VARIABLES:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
!  The Initialize method of the CTM Derived Gridded Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      __Iam__('Initialize')
      character(len=ESMF_MAXSTR)    :: COMP_NAME
      REAL, POINTER, DIMENSION(:,:) :: cellArea
      type(ESMF_Grid)               :: esmfGrid
      type (ESMF_VM)                :: VM
      integer                       :: im, jm, km, i
      type(MAPL_MetaComp), pointer  :: ggState      ! GEOS Generic State
      type (ESMF_Config)            :: CF
      integer                       :: dims(3)
      integer :: comm

      !  Get my name and set-up traceback handle
      !  ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, VM=VM, RC=STATUS )
      VERIFY_(STATUS)
      Iam = TRIM(COMP_NAME)//"::Initialize"

      !  Initialize GEOS Generic
      !  ------------------------
      call MAPL_GenericInitialize ( gc, IMPORT, EXPORT, clock,  RC=STATUS )
      VERIFY_(STATUS)

      !  Get my internal MAPL_Generic state
      !  -----------------------------------
      call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(ggSTATE,"TOTAL")
      call MAPL_TimerOn(ggSTATE,"INITIALIZE")

      ! Get the grid related information
      !---------------------------------
      call ESMF_GridCompGet ( GC, GRID=esmfGrid, rc=STATUS)
      VERIFY_(STATUS)

      call MAPL_GridGet ( esmfGrid, globalCellCountPerDim=dims, RC=STATUS)
      VERIFY_(STATUS)

      im = dims(1)
      jm = dims(2)
      km = dims(3)
    
      ! Get the time-step
      ! -----------------------
      !call MAPL_GetResource( ggState, ndt, 'RUN_DT:', default=0, RC=STATUS )
      !VERIFY_(STATUS)
      !dt = ndt

      call MAPL_TimerOff(ggSTATE,"INITIALIZE")
      call MAPL_TimerOff(ggSTATE,"TOTAL")

      RETURN_(ESMF_SUCCESS)

      end subroutine Initialize
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run -- Run method
!
! !INTERFACE:
!
      subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      type(ESMF_State),    intent(inout) :: IMPORT ! Import state
      type(ESMF_State),    intent(inout) :: EXPORT ! Export state
      type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
! The Run method of the derived variables CTM Gridded Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC 
!
! !LOCAL VARIABLES:
      character(len=ESMF_MAXSTR)      :: IAm = "Run"
      integer                         :: STATUS
      character(len=ESMF_MAXSTR)      :: COMP_NAME
      type (MAPL_MetaComp), pointer   :: ggState
      type (ESMF_Grid)                :: esmfGrid
      type (ESMF_State)               :: INTERNAL

      ! Imports
      !--------
      real, pointer, dimension(:,:)   ::       PS1 => null()
      real, pointer, dimension(:,:)   ::       PS0 => null()
      real, pointer, dimension(:,:,:) ::       UC0 => null()
      real, pointer, dimension(:,:,:) ::       UC1 => null()
      real, pointer, dimension(:,:,:) ::       VC0 => null()
      real, pointer, dimension(:,:,:) ::       VC1 => null()
      real, pointer, dimension(:,:,:) ::        th => null()
      real, pointer, dimension(:,:,:) ::         q => null()
      real, pointer, dimension(:,:,:) ::       zle => null()
      real, pointer, dimension(:,:,:) ::      DELP => null()
      real, pointer, dimension(:,:)   ::  cellArea => null()

      ! Exports
      !--------
      real,     pointer, dimension(:,:,:) ::       PLE => null()
      real,     pointer, dimension(:,:,:) ::       rho => null()
      real,     pointer, dimension(:,:,:) ::      mass => null()
      real(r8), pointer, dimension(:,:,:) ::      CXr8 => null()
      real(r8), pointer, dimension(:,:,:) ::      CYr8 => null()
      real(r8), pointer, dimension(:,:,:) ::    PLE1r8 => null()
      real(r8), pointer, dimension(:,:,:) ::    PLE0r8 => null()
      real(r8), pointer, dimension(:,:,:) ::     MFXr8 => null()
      real(r8), pointer, dimension(:,:,:) ::     MFYr8 => null()

!-MSL
      real, pointer, dimension(:,:,:) ::     MFX => null()
      real, pointer, dimension(:,:,:) ::     MFY => null()
      real, pointer, dimension(:,:,:) ::     CX => null()
      real, pointer, dimension(:,:,:) ::     CY => null()
!--
      real(r8), pointer, dimension(:,:,:) ::      UCr8 => null()
      real(r8), pointer, dimension(:,:,:) ::      VCr8 => null()
      real(r8), pointer, dimension(:,:,:) ::     PLEr8 => null()

      real,     pointer, dimension(:,:,:) ::      PLE0 => null()
      real,     pointer, dimension(:,:,:) ::      PLE1 => null()

      integer               :: km, k, is, ie, js, je, lm, l, ik
      integer               :: ndt, isd, ied, jsd, jed
      real(r8), allocatable :: AP(:), BP(:)
      real(r8)              :: dt

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------
      call ESMF_GridCompGet ( GC, name=COMP_NAME, Grid=esmfGrid, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

      ! Get my internal MAPL_Generic state
      !-----------------------------------
      call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS )
      VERIFY_(STATUS)

      call MAPL_TimerOn(ggState,"TOTAL")
      call MAPL_TimerOn(ggState,"RUN")

      ! Get the time-step
      ! -----------------------
      call MAPL_GetResource( ggState, ndt, 'RUN_DT:', default=0, RC=STATUS )
      VERIFY_(STATUS)
      dt = ndt

      ! Get to the imports...
      ! ---------------------
      call MAPL_GetPointer ( IMPORT,      PS0,     'PS0', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( IMPORT,      PS1,     'PS1', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( IMPORT,      UC0,     'UC0', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( IMPORT,      UC1,     'UC1', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( IMPORT,      VC0,     'VC0', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( IMPORT,      VC1,     'VC1', RC=STATUS )
      VERIFY_(STATUS)

      ! Get to the internal pointers...
      ! ---------------------
      ! Get the internal state which holds the private Config object
!      CALL ESMF_UserCompGetInternalState( GC, 'ctmEnv_State', INTERNAL, STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( INTERNAL,     PLE0,     'PLE0', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( INTERNAL,     PLE1,     'PLE1', RC=STATUS )
!      VERIFY_(STATUS)

      

      !call MAPL_GetPointer ( IMPORT, cellArea,    'AREA', RC=STATUS )
      !VERIFY_(STATUS)
!      call MAPL_GetPointer ( IMPORT,       th,      'TH', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( IMPORT,       q,        'Q', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( IMPORT,     zle,      'ZLE', RC=STATUS )
!      VERIFY_(STATUS)

      ! Get local dimensions
      is = lbound(UC1,1); ie = ubound(UC1,1)
      js = lbound(UC1,2); je = ubound(UC1,2)
      lm = size  (UC1,3)

      ! Calcaulate PLE0/1 - M.Long
      ! ---------------------
#include "GEOS_HyCoords.H"
      
      ALLOCATE( PLE0(is:ie,js:je,lm+1),   STAT=STATUS); VERIFY_(STATUS)
      ALLOCATE( PLE1(is:ie,js:je,lm+1),   STAT=STATUS); VERIFY_(STATUS)

      ! Reverse PLE0/1 because the 
      ! daggum models flip the atmosphere.
      DO L=1,LM+1
         PLE0(:,:,L) = 100.d0*(AP(L) + ( BP(L) * PS0(:,:) ))
         PLE1(:,:,L) = 100.d0*(AP(L) + ( BP(L) * PS1(:,:) ))
      END DO
      IF (.true.) THEN ! Flip vertical
         PLE0(:,:,:) = PLE0(:,:,LM+1:1:-1)
         PLE1(:,:,:) = PLE1(:,:,LM+1:1:-1)
      ELSE ! Don't flip vertical
         !PLE0(:,:,:) = PLE0(:,:,1:LM+1) ! <<>> This is done in Includes_Before_Run.H
         !PLE1(:,:,:) = PLE1(:,:,1:LM+1)
      ENDIF
       UC0(:,:,:) =  UC0(:,:,LM:1:-1)
       VC0(:,:,:) =  VC0(:,:,LM:1:-1)
       UC1(:,:,:) =  UC1(:,:,LM:1:-1)
       VC1(:,:,:) =  VC1(:,:,LM:1:-1)

      DEALLOCATE( AP, BP )

      ! Get to the exports...
      ! ---------------------
 
      call MAPL_GetPointer ( EXPORT, PLE, 'PLE',  RC=STATUS )
      VERIFY_(STATUS)
      PLE = PLE0

      call MAPL_GetPointer ( EXPORT, PLE0r8, 'PLE0r8',  RC=STATUS )
      VERIFY_(STATUS)
      PLE0r8 = real(PLE0,8)

      call MAPL_GetPointer ( EXPORT, PLE1r8, 'PLE1r8',  RC=STATUS )
      VERIFY_(STATUS)
      PLE1r8 = real(PLE1,8)

      call MAPL_GetPointer ( EXPORT, MFXr8, 'MFXr8', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( EXPORT, MFYr8, 'MFYr8', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( EXPORT,  CXr8,  'CXr8', RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetPointer ( EXPORT,  CYr8,  'CYr8', RC=STATUS )
      VERIFY_(STATUS)

!--MSL
!      call MAPL_GetPointer ( EXPORT, MFX, 'MFX', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( EXPORT, MFY, 'MFY', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( EXPORT,  CX,  'CX', RC=STATUS )
!      VERIFY_(STATUS)
!      call MAPL_GetPointer ( EXPORT,  CY,  'CY', RC=STATUS )
!      VERIFY_(STATUS)
!--

      !IF (MAPL_AM_I_ROOT())

      ! Compute the courant numbers and mass fluxes
      !--------------------------------------------
      ALLOCATE( UCr8(is:ie,js:je,lm),   STAT=STATUS); VERIFY_(STATUS)
      ALLOCATE( VCr8(is:ie,js:je,lm),   STAT=STATUS); VERIFY_(STATUS)
      ALLOCATE(PLEr8(is:ie,js:je,lm+1), STAT=STATUS); VERIFY_(STATUS)

      UCr8  = 0.50d0*(UC1  + UC0)
      VCr8  = 0.50d0*(VC1  + VC0)
      PLEr8 = 0.50d0*(PLE1 + PLE0)

      call calcCourantNumberMassFlux(UCr8, VCr8, PLEr8, &
                                MFXr8, MFYr8, CXr8, CYr8, dt)

!      MFX = real(MFXr8,4); MFY = real(MFYr8,4)
!      CX = real(CXr8,4); CY = real(CYr8,4)
 
!      MFX = 0.; MFY = 0.
!      CX = 0.; CY=0.
   
!      write(*,*) 'dt<>: ', dt
!      write(*,*) 'UCr8<>:',maxval(UCr8),minval(UCr8) 
!      write(*,*) 'PLEr8<>:',maxval(PLEr8),minval(PLEr8) 
!      write(*,*) 'ENV CX<>:',maxval(CXr8),minval(CXr8) 
!      write(*,*) 'ENV MFX<>:',maxval(MFXr8),minval(MFXr8) 

!      DEALLOCATE( PLE0, PLE1 )
      DEALLOCATE( UCr8, VCr8 )

      ! Get to the exports...
      ! ---------------------
     !call MAPL_GetPointer ( EXPORT,  rho, 'AIRDENS', ALLOC=.TRUE., RC=STATUS )
     !VERIFY_(STATUS)
     !call MAPL_GetPointer ( EXPORT, mass,    'MASS', ALLOC=.TRUE., RC=STATUS )
     !VERIFY_(STATUS)

     !if ( associated(rho) .and. associated(mass)) then
     !   ! Compute air density
     !   ! -------------------
     !   call airdens_ ( rho, PLE, th, q )
!
     !   ! Compute the total mass
     !   !-----------------------
     !   km = size(mass,3)
     !   DO k = 1, km
     !      mass(:,:,k) = rho(:,:,k)*cellArea(:,:)*(zle(:,:,k-1)-zle(:,:,k))
     !   END DO
     !end if

      call MAPL_TimerOff(ggState,"RUN")
      call MAPL_TimerOff(ggState,"TOTAL")

      ! All Done
      ! --------
      RETURN_(ESMF_SUCCESS)

      end subroutine Run
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeEdgePressure(PLE, PS, AK, BK, km)
!
! !INPUT PARAMETERS:
      INTEGER,  intent(in) :: km      ! number of vertical levels
      REAL(r4), intent(in) :: PS(:,:) ! Surface pressure (Pa)
      REAL(r8), intent(in) :: ak(km+1), bk(km+1)
!
! !OUTPUT PARAMETERS:
      REAL(r4), intent(out) :: PLE(:,:,:)  ! Edge pressure (Pa)
!EOP
!------------------------------------------------------------------------------
!BOC
      INTEGER  :: L
      
      DO L = 1, km
         PLE(:,:,L) = ak(L) + bk(L)*PS(:,:)
      END DO

      RETURN

      end subroutine computeEdgePressure
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeTotalPrecip(TPREC, PRECANV, PRECCON, PRECLSC)
!
! !INPUT PARAMETERS:
     REAL(r4), intent(in) :: PRECANV(:,:) ! Surface precipitation flux from anvils (kg/m2/s)
     REAL(r4), intent(in) :: PRECCON(:,:) ! Surface precipitation flux from convection (kg/m2/s)
     REAL(r4), intent(in) :: PRECLSC(:,:) ! Surface precipitation flux from large-scale (kg/m2/s)
!
! !OUTPUT PARAMETERS:
     REAL(r4), intent(out) :: TPREC(:,:)  ! Total precipitation (kg/m2/s)
!EOP
!------------------------------------------------------------------------------
!BOC
      TPREC = PRECANV + PRECCON + PRECLSC

      RETURN

      end subroutine computeTotalPrecip
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeLWI(LWI, TSKIN, FRLAKE, FROCEAN, FRSEAICE)
!
! !INPUT PARAMETERS:
     REAL(r4), intent(in) :: TSKIN(:,:)    ! Surface skin temperature (K)
     REAL(r4), intent(in) :: FRLAKE(:,:)   ! Fraction of lake type in grid box (1)
     REAL(r4), intent(in) :: FROCEAN(:,:)  ! Fraction of ocean in grid box (1)
     REAL(r4), intent(in) :: FRSEAICE(:,:) ! Ice covered fraction of tile (1)
!
! !OUTPUT PARAMETERS:
     REAL(r4), intent(out) :: LWI(:,:) ! Land water ice flag (1)
!
!EOP
!------------------------------------------------------------------------------
!BOC

                                          LWI = 1.0  ! Land
      where ( FROCEAN+FRLAKE >= 0.6     ) LWI = 0.0  ! Water
      where ( LWI==0 .and. FRSEAICE>0.5 ) LWI = 2.0  ! Ice
      where ( LWI==0 .and. TSKIN<271.40 ) LWI = 2.0  ! Ice

      RETURN

      end subroutine computeLWI
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeRelativeHumidity(RH2, PRESS3D, T, QV)

!
! !INPUT PARAMETERS:
      REAL, intent(in) :: PRESS3D(:,:,:)  ! Pressure (Pa)
      REAL, intent(in) :: T      (:,:,:)  ! Air temperature (K)
      REAL, intent(in) :: QV     (:,:,:)  ! Specific humidity (kg/kg)
!
! !OUTPUT PARAMETERS:
      REAL, intent(out) :: RH2(:,:,:) ! Relative humidity (1)
!
!EOP
!------------------------------------------------------------------------------
!BOC

      ! -----------------------------------------------------------------
      ! First calculate relative humidity from Seinfeld (1986) p. 181.
      ! The first  RH2 is the temperature dependent parameter a.
      ! The second RH2 is the saturation vapor pressure of water.
      ! The third  RH2 is the actual relative humidity as a fraction.
      ! Then make sure RH2 is between 0 and 0.95.
      !-----------------------------------------------------------------

      RH2(:,:,:) = 1.0d0 - (373.15d0 / T(:,:,:))

      RH2(:,:,:) =  &
             1013.25d0 * Exp (13.3185d0 * RH2(:,:,:)    -  &
                               1.9760d0 * RH2(:,:,:)**2 -  &
                               0.6445d0 * RH2(:,:,:)**3 -  &
                               0.1299d0 * RH2(:,:,:)**4)

      RH2(:,:,:) = QV(:,:,:) * MWTAIR / 18.0d0 /  &
                      GPKG * PRESS3D(:,:,:) / RH2(:,:,:)

      RH2(:,:,:) = Max (Min (RH2(:,:,:), 0.95d0), 0.0d0)

      RETURN 

      end subroutine computeRelativeHumidity
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINES: airdens
!
! !INTERFACE:

      subroutine airdens_ ( rho, pe, th, q )
!
! !INPUT PARAMETERS:
      real,    intent(in) :: pe(:,:,:)      ! pressure edges
      real,    intent(in) :: th(:,:,:)      ! (dry) potential temperature
      real,    intent(in) :: q(:,:,:)       ! apecific humidity
!
! !OUTPUT PARAMETERS:
      real,    intent(out) :: rho(:,:,:)    ! air density [kg/m3]
!
! !DESCRIPTION:
! Computes the air density that might be needed when GEOSchem is not
! exercised.
!
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer :: k, iml, jml, nl     ! dimensions
      real :: eps
      integer :: STATUS, RC
      character(len=ESMF_MAXSTR)      :: IAm = "airdens_"
      real, allocatable :: npk(:,:,:) ! normalized pk = (pe/p0)^kappa

      iml = size(q,1);  jml = size(q,2);  nl = size(q,3)

      allocate(npk(iml,jml,nl+1),stat=STATUS) ! work space
      VERIFY_(STATUS)

      eps = MAPL_RVAP / MAPL_RGAS - 1.0

      ! Compute normalized pe**Kappa
      ! ----------------------------
      npk = (pe/MAPL_P00)**MAPL_KAPPA

      ! Compute rho from hydrostatic equation
      ! -------------------------------------
      do k = 1, nl
         rho(:,:,k) =       ( pe(:,:,k+1) - pe(:,:,k) ) /      &
                      ( MAPL_CP * ( th(:,:,k)*(1. + eps*q(:,:,k) ) ) &
                              * ( npk(:,:,k+1) - npk(:,:,k) ) )
      end do

      deallocate(npk)

      end subroutine airdens_
!EOC
!-----------------------------------------------------------------------
!BOP
      function compAreaWeightedAverage_2d (var2D, vm, cellArea) result(wAverage)
!
! !INPUT PARAMETER:
      real            :: var2D(:,:)
      real            :: cellArea(:,:)
      type (ESMF_VM)  :: VM
!
! RETURNED VALUE:
      real  :: wAverage
!
! DESCRIPTION:
! Computes the area weighted average of a 2d variable.
!
!EOP
!-----------------------------------------------------------------------
!BOC
      logical, save :: first = .true.
      real(r8) , save :: sumArea
      real(r8) :: sumWeight
      integer :: ik, im, jm, STATUS, RC
      real(r8), pointer :: weightVals(:,:)
      real(r8) :: sumWeight_loc, sumArea_loc
      character(len=ESMF_MAXSTR) :: IAm = 'compAreaWeightedAverage_2d'

      ! Determine the earth surface area
      if (first) then
         sumArea_loc   = SUM( cellArea  (:,:)  )
         call MAPL_CommsAllReduceSum(vm, sendbuf= sumArea_loc, &
                                         recvbuf= sumArea, &
                                         cnt=1, RC=status)
         VERIFY_(STATUS)

         first = .false.
      end if

      im = size(cellArea,1)
      jm = size(cellArea,2)

      allocate(weightVals(im,jm))
      weightVals(:,:) = cellArea(:,:)*var2D(:,:)

      sumWeight_loc = SUM( weightVals(:,:) )

      call MAPL_CommsAllReduceSum(vm, sendbuf= sumWeight_loc, recvbuf= sumWeight, &
         cnt=1, RC=status)
      VERIFY_(STATUS)

      wAverage = sumWeight/sumArea

      deallocate(weightVals)

      return

      end function compAreaWeightedAverage_2d
!EOC
!-----------------------------------------------------------------------
!BOP
      function compAreaWeightedAverage_3d (var3D, vm, cellArea) result(wAverage)
!
! !INPUT PARAMETER:
      real            :: var3D(:,:,:)
      real            :: cellArea(:,:)
      type (ESMF_VM)  :: VM
!
! RETURNED VALUE:
      real  :: wAverage
!
! DESCRIPTION:
! Computes the area weighted average of a 3d variable.
!
!EOP
!-----------------------------------------------------------------------
!BOC
      logical, save :: first = .true.
      real(r8) , save :: sumArea
      real(r8) :: sumWeight
      integer :: ik, im, jm, STATUS, RC
      real(r8), pointer :: weightVals(:,:)
      real(r8) :: sumWeight_loc, sumArea_loc
      character(len=ESMF_MAXSTR) :: IAm = 'compAreaWeightedAverage_3d'

      ! Determine the earth surface area
      if (first) then
         sumArea_loc   = SUM( cellArea  (:,:)  )
         call MAPL_CommsAllReduceSum(vm, sendbuf= sumArea_loc, &
                                         recvbuf= sumArea, &
                                         cnt=1, RC=status)
         VERIFY_(STATUS)

         first = .false.
      end if

      im = size(cellArea,1)
      jm = size(cellArea,2)

      allocate(weightVals(im,jm))
      weightVals(:,:) = 0.0d0
      DO ik = lbound(var3D,3), ubound(var3D,3)
         weightVals(:,:) = weightVals(:,:) + cellArea(:,:)*var3D(:,:,ik)
      END DO

      sumWeight_loc = SUM( weightVals(:,:) )

      call MAPL_CommsAllReduceSum(vm, sendbuf= sumWeight_loc, recvbuf= sumWeight, &
         cnt=1, RC=status)
      VERIFY_(STATUS)

      wAverage = sumWeight/sumArea

      deallocate(weightVals)

      return

      end function compAreaWeightedAverage_3d
!EOC
!-----------------------------------------------------------------------
      end module GEOS_ctmEnvGridComp
