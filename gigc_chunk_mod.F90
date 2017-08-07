#if defined (ESMF_)
#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_chunk_mod
!
! !DESCRIPTION: Module GC\_CHUNK\_MOD is the module that contains the init,
!  run, and finalize methods for the ESMF interface to the Grid-Independent
!  GEOS-Chem (aka "GIGC").
!\\
!\\
! !INTERFACE: 
!      
MODULE GIGC_Chunk_Mod
!
! !USES:
!      
  USE MAPL_MOD
  USE ESMF
  USE GCHP_Utils

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: GIGC_Chunk_Init
  PUBLIC :: GIGC_Chunk_Run
  PUBLIC :: GIGC_Chunk_Final
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: GIGC_Cap_Tropopause_Prs
  PRIVATE :: GIGC_Revert_Units
  PRIVATE :: GIGC_Assert_Units
!
! !REMARKS:
!  The routines in this module execute only when GEOS-Chem is connected
!  to the GEOS-5 GCM via the ESMF/MAPL interface.
!
! !REVISION HISTORY:
!  22 Jun 2009 - R. Yantosca & P. Le Sager - Chunkized & cleaned up.
!  09 Oct 2012 - R. Yantosca - Now pass am_I_Root to all routines
!  09 Oct 2012 - R. Yantosca - Added comments, cosmetic changes
!  16 Oct 2012 - R. Yantosca - Renamed GC_STATE argument to State_Chm
!  22 Oct 2012 - R. Yantosca - Renamed to gigc_chunk_mod.F90
!  01 Nov 2012 - R. Yantosca - Now pass Input Options object to routines
!  15 Mar 2013 - R. Yantosca - Add routine GIGC_Cap_Tropopause_Prs
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE TYPES:
!
  ! Derived type for chunk diagnostic output (for code validation)
  TYPE GC_DIAG
     LOGICAL                    :: DO_PRINT     ! Should we print out?
     INTEGER                    :: N_DIAG       ! # of diag quantities
     INTEGER                    :: COUNT        ! Counter for averaging
     CHARACTER(LEN=10), POINTER :: NAME(:)      ! Tracer names
     REAL*8,            POINTER :: TRACER(:,:)  ! Tracer concentrations
     CHARACTER(LEN=40)          :: FILENAME     ! File name for output
     INTEGER                    :: LUN          ! File unit # for output
  END TYPE GC_DIAG

  ! Derived type object for saving concentration diagnostics
  TYPE(GC_DIAG)                 :: DIAG_COL

CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_chunk_init
!
! !DESCRIPTION: Subroutine GIGC\_CHUNK\_INIT is the ESMF init method for
!  the Grid-Independent GEOS-Chem (aka "GIGC").  This routine calls lower-
!  level routines to allocate arrays and read input files.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Chunk_Init( am_I_Root, I_LO,      J_LO,       I_HI,      &
                              J_HI,      IM,        JM,         LM,        &
                              IM_WORLD,  JM_WORLD,  LM_WORLD,   nymdB,     &
                              nhmsB,     nymdE,     nhmsE,      tsChem,    &
                              tsDyn,     lonCtr,    latCtr,     myPET,     &
                              Input_Opt, State_Chm, State_Diag, State_Met, &
                              HcoConfig, RC      )
!
! !USES:
!
    USE ESMF,                    ONLY : ESMF_KIND_R4
    USE GIGC_Initialization_Mod, ONLY : GIGC_Init_Simulation
    USE ErrCode_Mod
    USE Input_Opt_Mod,           ONLY : OptInput
    USE State_Chm_Mod,           ONLY : ChmState
    USE State_Diag_Mod,          ONLY : DgnState
    USE State_Met_Mod,           ONLY : MetState
    USE Diagnostics_Mod,         ONLY : Diagnostics_Init
    USE EMISSIONS_MOD,           ONLY : EMISSIONS_INIT
    USE HCO_TYPES_MOD,           ONLY : ConfigObj
    USE UCX_MOD,                 ONLY : INIT_UCX, SET_INITIAL_MIXRATIOS
    USE UnitConv_Mod
!
! !INPUT PARAMETERS:
!
    LOGICAL,            INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
    INTEGER,            INTENT(IN)    :: I_LO        ! Min lon index, this CPU
    INTEGER,            INTENT(IN)    :: J_LO        ! Min lat index, this CPU
    INTEGER,            INTENT(IN)    :: I_HI        ! Max lon index, this CPU
    INTEGER,            INTENT(IN)    :: J_HI        ! Max lat index, this CPU
    INTEGER,            INTENT(IN)    :: IM          ! # lons, this CPU
    INTEGER,            INTENT(IN)    :: JM          ! # lats, this CPU
    INTEGER,            INTENT(IN)    :: LM          ! # levs, this CPU
    INTEGER,            INTENT(IN)    :: IM_WORLD    ! # lons, global grid
    INTEGER,            INTENT(IN)    :: JM_WORLD    ! # lats, global grid
    INTEGER,            INTENT(IN)    :: LM_WORLD    ! # levs, global grid
    INTEGER,            INTENT(IN)    :: myPET       ! Local PET
    INTEGER,            INTENT(IN)    :: nymdB       ! YYYYMMDD @ start of run
    INTEGER,            INTENT(IN)    :: nhmsB       ! hhmmss   @ start of run
    INTEGER,            INTENT(IN)    :: nymdE       ! YYYYMMDD @ end of run
    INTEGER,            INTENT(IN)    :: nhmsE       ! hhmmss   @ end of run
    REAL,               INTENT(IN)    :: tsChem      ! Chemistry timestep [s]
    REAL,               INTENT(IN)    :: tsDyn       ! Chemistry timestep [s]
    REAL(ESMF_KIND_R4), INTENT(IN)    :: lonCtr(:,:) ! Lon centers [radians]
    REAL(ESMF_KIND_R4), INTENT(IN)    :: latCtr(:,:) ! Lat centers [radians]
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput),     INTENT(INOUT) :: Input_Opt   ! Input Options object
    TYPE(ChmState),     INTENT(INOUT) :: State_Chm   ! Chemistry State object
    TYPE(DgnState),     INTENT(INOUT) :: State_Diag  ! Diagnostics State object
    TYPE(MetState),     INTENT(INOUT) :: State_Met   ! Meteorology State object
    TYPE(ConfigObj),    POINTER       :: HcoConfig   ! HEMCO config obj 
!
! !OUTPUT PARAMETERS:
!
    INTEGER,            INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!  Need to add better error checking
!
! !REVISION HISTORY: 
!  18 Jul 2011 - M. Long     - Initial Version
!  28 Mar 2012 - M. Long     - Rewrite per structure of BCC init interface
!  09 Oct 2012 - R. Yantosca - Added comments, cosmetic changes
!  16 Oct 2012 - R. Yantosca - Renamed GC_STATE argument to State_Chm
!  16 Oct 2012 - R. Yantosca - Renamed GC_MET argument to State_Met
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_chm_mod.F90
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_met_mod.F90
!  22 Oct 2012 - R. Yantosca - Renamed to GIGC_Chunk_Init
!  01 Nov 2012 - R. Yantosca - Now reference gigc_input_opt_mod.F90
!  01 Nov 2012 - R. Yantosca - Reordered arguments for clarit
!  28 Nov 2012 - M. Long     - Now pass lonCtr, latCtr, latEdg as arguments
!                              to routine GIGC_Init_Simulation
!  03 Dec 2012 - R. Yantosca - Now call Init_CMN_SIZE (in CMN_SIZE_mod.F)
!                              instead of GIGC_Init_Dimensions to initialize
!                              the size parameters.
!  03 Dec 2012 - R. Yantosca - Rename NI, NJ, NL to IM, JM, LM for clarity
!  03 Dec 2012 - R. Yantosca - Now pass I_LO, J_LO, I_HI, J_HI, IM_WORLD, 
!                              JM_WORLD, LM_WORLD via the arg list
!  05 Dec 2012 - R. Yantosca - Remove latEdg argument
!  06 Dec 2012 - R. Yantosca - Add nymdB, nhmsB, nymdB, nhmsB arguments,
!                              and remove nymd, nhms
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: STATUS
    CHARACTER(LEN=ESMF_MAXSTR)     :: Iam

    !=======================================================================
    ! GIGC_CHUNK_INIT begins here 
    !=======================================================================

    ! Error trap
    Iam = 'GIGC_CHUNK_INIT (gigc_chunk_mod.F90)'

    ! Assume success
    RC = GC_SUCCESS

    !======================================================================
    ! Initialize the G-C simulation and chemistry mechanism
    !=======================================================================
    CALL GIGC_Init_Simulation( am_I_Root      = am_I_Root,  & ! Root CPU?
                               nymdB          = nymdB,      & ! Date @ start
                               nhmsB          = nhmsB,      & ! Time @ start
                               nymdE          = nymdE,      & ! Date @ end
                               nhmsE          = nhmsE,      & ! Time @ end
                               tsChem         = tsChem,     & ! Chem step [s]
                               tsDyn          = tsDyn,      & ! Dyn  step [s]
                               value_I_LO     = I_LO,       & ! Local min lon
                               value_J_LO     = J_LO,       & ! Local min lat
                               value_I_HI     = I_HI,       & ! Local max lon 
                               value_J_HI     = J_HI,       & ! Local max lat
                               value_IM       = IM,         & ! Local # lons
                               value_JM       = JM,         & ! Local # lats
                               value_LM       = LM,         & ! Local # levs
                               value_IM_WORLD = IM_WORLD,   & ! Global # lons
                               value_JM_WORLD = JM_WORLD,   & ! Global # lats
                               value_LM_WORLD = LM_WORLD,   & ! Global # levs
                               lonCtr         = lonCtr,     & ! Lon ctrs [rad]
                               latCtr         = latCtr,     & ! Lat ctrs [rad]
                               myPET          = myPET,      & ! Local PET
                               Input_Opt      = Input_Opt,  & ! Input Options
                               State_Chm      = State_Chm,  & ! Chemistry State
                               State_Diag     = State_Diag, & ! Diagnostic State
                               State_Met      = State_Met,  & ! Met State
                               RC             = RC         )  ! Success?
    ASSERT_(RC==GC_SUCCESS)

    !=======================================================================
    ! Initialize HEMCO
    !=======================================================================
    CALL EMISSIONS_INIT ( am_I_Root, Input_Opt, State_Met, State_Chm, RC, &
                          HcoConfig=HcoConfig )
    ASSERT_(RC==GC_SUCCESS)

    !-------------------------------------------------------------------------
    ! Stratosphere - can't be initialized without HEMCO because of STATE_PSC
    !-------------------------------------------------------------------------
    IF ( Input_Opt%LUCX ) THEN

       ! Initialize stratospheric routines
       CALL INIT_UCX( am_I_Root, Input_Opt, State_Chm )

       ! Set simple initial tracer conditions
       CALL SET_INITIAL_MIXRATIOS( am_I_Root, Input_Opt, State_Met, State_Chm )
    ENDIF

    ! Initialize diagnostics.
    CALL Diagnostics_Init( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
    ASSERT_(RC==GC_SUCCESS)

    !=======================================================================
    ! Make sure options are sane
    !=======================================================================

    ! In GEOS-5, we typically use the GEOS-5 online transport, convection 
    ! and turbulence scheme to vertically distribute tracer concentrations.
    ! In this case, emissions and dry deposition should only be applied to
    ! the surface layer. This is similiar to the non-local PBL mixing scheme
    ! used in GEOS-Chem. Since there are a lot of checks for the non-local
    ! mixing scheme all over the GEOS-Chem code, we force it here to .TRUE.
    ! if we are not doing turbulence within GEOS-Chem. This will make sure
    ! that the dry deposition quantities are calculated w/r/t the surface
    ! layer only (ckeller, 11/04/14).
!    IF ( .NOT. Input_Opt%LTURB .AND. .NOT. Input_Opt%LNLPBL ) THEN
!       Input_Opt%LNLPBL = .TRUE.
!       IF ( am_I_Root ) THEN
!          write(*,*) ' '
!          write(*,*) ' SET GEOS-CHEM NON-LOCAL PBL MIXING TO TRUE '
!          write(*,*) ' '
!       ENDIF
!    ENDIF

    ! GCHP expects units of v/v dry...
    CALL ConvertSpc_KgKgDry_to_VVDry( am_I_Root, State_Chm, RC )
    ASSERT_(RC==GC_SUCCESS)

  END SUBROUTINE GIGC_Chunk_Init
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_chunk_run
!
! !DESCRIPTION: Subroutine GIGC\_CHUNK\_RUN is the ESMF run method for
!  the Grid-Independent GEOS-Chem (aka "GIGC").  This routine is the driver
!  for the following operations:
!
! \begin{itemize}
! \item Dry deposition
! \item Chemistry
! \end{itemize}
!
! !INTERFACE:
!
  SUBROUTINE GIGC_Chunk_Run( am_I_Root, GC, IM,    JM,        LM,         &
                             nymd,      nhms,      year,      month,      &
                             day,       dayOfYr,   hour,      minute,     &
                             second,    utc,       hElapsed,  Input_Opt,  &
                             State_Chm, State_Met, Phase,     IsChemTime, &
                             RC                                            )
!
! !USES:
!
    USE Chemistry_Mod,      ONLY : Do_Chemistry
    USE Dao_Mod,            ONLY : AirQnt,  SET_DRY_SURFACE_PRESSURE
    USE DryDep_Mod,         ONLY : Do_DryDep
    USE GC_Land_Interface
    USE ErrCode_Mod
    USE Input_Opt_Mod,      ONLY : OptInput
    USE State_Chm_Mod,      ONLY : ChmState, IND_
    USE State_Met_Mod,      ONLY : MetState
    USE PBL_MIX_MOD,        ONLY : DO_PBL_MIX, COMPUTE_PBL_HEIGHT
    USE VDIFF_MOD,          ONLY : DO_PBL_MIX_2
    USE Pressure_Mod,       ONLY : Accept_External_Pedge, Set_Floating_Pressures
    USE Time_Mod,           ONLY : Accept_External_Date_Time
    USE Time_Mod,           ONLY : ITS_TIME_FOR_CHEM
    USE TIME_MOD,           ONLY : GET_TS_CHEM, GET_TS_DYN
    USE WETSCAV_MOD,        ONLY : SETUP_WETSCAV, DO_WETDEP
    USE DRYDEP_MOD,         ONLY : DEPSAV, NUMDEP, NTRAIND
    USE CONVECTION_MOD,     ONLY : DO_CONVECTION
    USE TOMS_MOD,           ONLY : COMPUTE_OVERHEAD_O3
    USE GC_GRID_MOD,        ONLY : AREA_M2
    USE Diagnostics_Mod,    ONLY : Diagnostics_Write
    USE EMISSIONS_MOD,      ONLY : EMISSIONS_RUN
    USE STRAT_CHEM_MOD,     ONLY : INIT_STRAT_CHEM, Minit_is_set
    USE MODIS_LAI_Mod,      ONLY : Compute_XLAI_GCHP

!    ! HEMCO update
    USE HCO_ERROR_MOD
    USE HCO_STATE_MOD,      ONLY : Hco_GetHcoID
    USE HCO_INTERFACE_MOD,  ONLY : HcoState, SetHcoTime
    USE HCO_INTERFACE_MOD,  ONLY : GetHcoVal 

    ! Apply tracer tendencies
    USE MIXING_MOD,         ONLY : DO_TEND, DO_MIXING

    ! UCX
    USE UCX_MOD,            ONLY : SET_H2O_TRAC

    ! Unit conversion (SE 2016-03-27)
    Use UnitConv_Mod

    ! ewl debugging
    USE Time_Mod,           ONLY : ITS_A_NEW_MONTH
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on root CPU?
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
    INTEGER,        INTENT(IN)    :: IM          ! # of lons on this CPU
    INTEGER,        INTENT(IN)    :: JM          ! # of lats on this CPU
    INTEGER,        INTENT(IN)    :: LM          ! # of levs on this CPU
    INTEGER,        INTENT(IN)    :: nymd        ! YYYY/MM/DD @ current time
    INTEGER,        INTENT(IN)    :: nhms        ! hh:mm:ss   @ current time
    INTEGER,        INTENT(IN)    :: year        ! UTC year 
    INTEGER,        INTENT(IN)    :: month       ! UTC month
    INTEGER,        INTENT(IN)    :: day         ! UTC day
    INTEGER,        INTENT(IN)    :: dayOfYr     ! UTC day of year
    INTEGER,        INTENT(IN)    :: hour        ! UTC hour
    INTEGER,        INTENT(IN)    :: minute      ! UTC minute
    INTEGER,        INTENT(IN)    :: second      ! UTC second
    REAL*4,         INTENT(IN)    :: utc         ! UTC time [hrs]
    REAL*4,         INTENT(IN)    :: hElapsed    ! Elapsed hours
    INTEGER,        INTENT(IN)    :: Phase       ! Run phase (1 or 2)
    LOGICAL,        INTENT(IN)    :: IsChemTime  ! Time for chemistry? 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
    TYPE(ChmState), INTENT(INOUT) :: State_Chm   ! Chemistry State object
    TYPE(MetState), INTENT(INOUT) :: State_Met   ! Meteorology State object
!
! !OUTPUT PARAMETERS G:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Return code
!
! !REMARKS:
!  Met field inputs from the State_Met object have SI units.  Some GEOS-Chem 
!  lower-level routines require nonstandard units.  Units are converted and 
!  stored in local variables within this module.
!                                                                             .
!  Need to add better error-handling.
!
! !REVISION HISTORY:
!  18 Jul 2011 - M. Long     - Initial Version
!  09 Oct 2012 - R. Yantosca - Added extra comments & cosmetic changes
!  16 Oct 2012 - R. Yantosca - Renamed GC_STATE argument to State_Chm
!  16 Oct 2012 - R. Yantosca - Renamed GC_MET argument to State_Met
!  17 Oct 2012 - R. Yantosca - Need to call AIRQNT before chemistry
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_chm_mod.F90
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_met_mod.F90
!  22 Oct 2012 - R. Yantosca - Renamed to GIGC_Chunk_Run
!  25 Oct 2012 - R. Yantosca - Now pass RC to GIGC_DO_CHEM
!  01 Nov 2012 - R. Yantosca - Now reference gigc_input_opt_mod.F90
!  08 Nov 2012 - R. Yantosca - Now pass Input_Opt to GIGC_Do_Chem
!  13 Nov 2012 - M. Long     - Added Dry Deposition method
!  29 Nov 2012 - R. Yantosca - Now block off calls to GIGC_DO_DRYDEP and
!                              GIGC_DO_CHEM w/ the appropriate logical flags
!  04 Dec 2012 - R. Yantosca - Now convert units of State_Chm%TRACERS here
!                              instead of in lower-level routines
!  07 Dec 2012 - R. Yantosca - Now call Accept_Date_Time_From_ESMF to pass the
!                              date & time from ESMF to GeosUtil/time_mod.F
!  07 Dec 2012 - R. Yantosca - Now pass UTC via Accept_Date_Time_From_ESMF;
!                              this ensures proper localtime computation
!  11 Dec 2012 - R. Yantosca - Now call DO_DRYDEP directly; no longer call
!                              GIGC_DO_DRYDEP, this is moved to obsolete dir.
!  11 Dec 2012 - R. Yantosca - Now call routine ACCEPT_EXTERNAL_PEDGE to pass
!                              the pressure edges from ESMF to GEOS-Chem
!  15 Mar 2013 - R. Yantosca - Now call GIGC_CAP_TROPOPAUSE_PRS to cap the
!                              State_Met%TROPP field to 200 hPa polewards
!                              of 60S and 60N.  We do this in the std G-C.
!  05 Jun 2013 - R. Yantosca - Remove obsolete code
!  22 Sep 2014 - C. Keller   - Added run phase argument
!  14 Oct 2014 - C. Keller   - Various updates to include drydep and emissions
!                              to tracer arrays, etc.
!  26 Nov 2014 - C. Keller   - Added IsChemTime variable.
!  19 Oct 2016 - R. Yantosca - Now call Set_Init_Cond_Strat_Chem after the
!                              1st call to AIRQNT to save initial conditions
!  01 Dec 2016 - E. Lundgren - Calculate LAI using new routine for GCHP
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MAPL_MetaComp), POINTER   :: STATE
    REAL*8                         :: DT
    INTEGER                        :: ERROR
!    REAL*8                         :: FLX, DEP, FRAC
!    REAL*8                         :: Emis8, Dep8
!    INTEGER                        :: I, J, L, T 
!    INTEGER                        :: N1, N2
!    INTEGER                        :: HcoID
    INTEGER                        :: STATUS
    CHARACTER(LEN=ESMF_MAXSTR)     :: Iam
!    LOGICAL                        :: FND

    ! Local logicals to turn on/off individual components
    ! The parts to be executed are based on the input options,
    ! the time step and the phase.
    LOGICAL                        :: DoConv 
    LOGICAL                        :: DoDryDep
    LOGICAL                        :: DoEmis
    LOGICAL                        :: DoTend 
    LOGICAL                        :: DoTurb 
    LOGICAL                        :: DoChem
    LOGICAL                        :: DoWetDep

    ! First call?
    LOGICAL, SAVE                  :: FIRST = .TRUE.

    ! Update?
    LOGICAL                        :: pUpdate

    ! # of times this routine has been called. Only temporary for printing 
    ! processes on the first 10 calls.
    INTEGER, SAVE                  :: NCALLS = 0

    ! UV month
    INTEGER, SAVE                  :: UVmonth = -999

    !=======================================================================
    ! GIGC_CHUNK_RUN begins here 
    !=======================================================================

    ! Error trap
    Iam = 'GIGC_CHUNK_RUN (gigc_chunk_mod.F90)'

    ! Assume success
    RC = GC_SUCCESS

    ! Get state object (needed for timers)
    CALL MAPL_GetObjectFromGC(GC, STATE, __RC__)

    ! Populate grid box areas from gc_grid_mod.F on first call. We have to do 
    ! this in the run phase and not in initialize because it seems like the
    ! AREA pointer (imported from superdynamics) is only properly filled in
    ! run.
    IF ( FIRST ) THEN
       AREA_M2 = State_Met%AREA_M2
    ENDIF

    !=======================================================================
    ! Define processes to be covered in this phase
    !
    ! In the standard GEOS-Chem, the following operator sequence is used:
    ! 1. DryDep (kg)
    ! 2. Emissions (kg)
    ! 3. Turbulence (v/v)
    ! 4. Convection (v/v)
    ! 5. Chemistry (kg)
    ! 6. Wetdep (kg)
    !
    ! The GEOS-5 operator sequence is:
    ! 1. Gravity wave drag
    ! 2. Moist (convection)
    ! 3. Chemistry 1 (drydep and emissions)
    ! 4. Surface 1
    ! 5. Turbulence 1
    ! 6. Surface 2
    ! 7. Turbulence 2
    ! 8. Chemistry 2 (chemistry and wet deposition)
    ! 9. Radiation 
    !
    ! Here, we use the following operator sequence:
    ! 
    ! 1.  Convection (v/v) --> Phase 1
    ! 2.  DryDep (kg)      --> Phase 1
    ! 3.  Emissions (kg)   --> Phase 1
    ! 4a. Tendencies (v/v) --> Phase 1
    ! -------------------------------
    ! 4b. Turbulence (v/v) --> Phase 2 
    ! 5.  Chemistry (kg)   --> Phase 2
    ! 6.  WetDep (kg)      --> Phase 2     
    ! 
    ! Any of the listed processes is only executed if the corresponding switch
    ! in the input.geos file is enabled. If the physics component already
    ! covers convection or turbulence, they should not be applied here!
    ! The tendencies are only applied if turbulence is not done within
    ! GEOS-Chem (ckeller, 10/14/14).
    !=======================================================================

    ! By default, do processes as defined in input.geos. DoTend is defined
    ! below. 
    DoConv   = Input_Opt%LCONV                    ! dynamic time step
    DoDryDep = Input_Opt%LDRYD .AND. IsChemTime   ! chemistry time step
    DoEmis   = Input_Opt%LEMIS .AND. IsChemTime   ! chemistry time step
    DoTurb   = Input_Opt%LTURB                    ! dynamic time step
    DoChem   = Input_Opt%LCHEM .AND. IsChemTime   ! chemistry time step
    DoWetDep = Input_Opt%LWETD                    ! dynamic time step 

    ! Only do selected processes for given phases: 
    ! Phase 1: disable turbulence, chemistry and wet deposition. 
    IF ( Phase == 1 ) THEN
       DoTurb   = .FALSE.
       DoChem   = .FALSE.
       DoWetDep = .FALSE.

    ! Phase 2: disable convection, drydep and emissions. 
    ELSEIF ( Phase == 2 ) THEN
       DoConv   = .FALSE.
       DoDryDep = .FALSE.
       DoEmis   = .FALSE. 
    ENDIF

    ! Check if tendencies need be applied. The drydep and emission calls
    ! only calculates the emission / drydep rates, but do not apply the
    ! tendencies to the tracer array yet. If turbulence is done as part of
    ! GEOS-5, we need to make sure that these tendencies are applied to the
    ! tracer array. If turbulence is explicitly covered by GEOS-Chem,
    ! however, the tendencies become automatically applied within the PBL
    ! mixing routines (DO_MIXING), so we should never apply the tendencies
    ! in this case.
    DoTend = ( DoEmis .OR. DoDryDep ) .AND. .NOT. Input_Opt%LTURB

    ! testing only
    IF ( am_I_Root .and. NCALLS < 10 ) THEN 
       write(*,*) 'GEOS-Chem phase ', Phase, ':'
       write(*,*) 'DoConv   : ', DoConv
       write(*,*) 'DoDryDep : ', DoDryDep
       write(*,*) 'DoEmis   : ', DoEmis
       write(*,*) 'DoTend   : ', DoTend
       write(*,*) 'DoTurb   : ', DoTurb
       write(*,*) 'DoChem   : ', DoChem
       write(*,*) 'DoWetDep : ', DoWetDep
       write(*,*) ' '
    ENDIF

    !-------------------------------------------------------------------------
    ! Pre-Run assignments
    !-------------------------------------------------------------------------

    !! Eventually initialize/reset wetdep
    IF ( DoConv .OR. DoChem .OR. DoWetDep ) THEN
       CALL SETUP_WETSCAV( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
    ENDIF

    ! Pass time values obtained from the ESMF environment to GEOS-Chem
    CALL Accept_External_Date_Time( am_I_Root      = am_I_Root,  &
                                    value_NYMD     = nymd,       &  
                                    value_NHMS     = nhms,       &  
                                    value_YEAR     = year,       &  
                                    value_MONTH    = month,      &  
                                    value_DAY      = day,        &  
                                    value_DAYOFYR  = dayOfYr,    &  
                                    value_HOUR     = hour,       &  
                                    value_MINUTE   = minute,     &  
                                    value_HELAPSED = hElapsed,   & 
                                    value_UTC      = utc,        &
                                    RC             = RC         )

    ! Set HEMCO time
    CALL SetHcoTime ( am_I_Root, DoEmis, RC )

    ! Calculate MODIS leaf area indexes needed for dry deposition
    ! (passing false as 2nd arg calculates chlorophyll-a instead)
    CALL Compute_XLAI_GCHP( am_I_Root, State_Met, RC )

    ! Set the pressure at level edges [hPa] from the ESMF environment
    CALL Accept_External_Pedge    ( am_I_Root      = am_I_Root,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

    ! Set dry surface pressure (PS1_DRY) from State_Met%PS1_WET
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 1 )

    ! Set dry surface pressure (PS2_DRY) from State_Met%PS2_WET
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 2 )

    ! Initialize surface pressures to match the post-advection pressures
    State_Met%PSC2_WET = State_Met%PS2_WET
    State_Met%PSC2_DRY = State_Met%PS2_DRY
    CALL SET_FLOATING_PRESSURES( am_I_Root, State_Met, RC )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Define airmass and related quantities
    ! If transport is ON, this module should be "insensitive" to changes in
    ! pressure. If transport is OFF, we should perform this update - but only
    ! after the first timestep (consistency with GCC)
    pUpdate = ((.not.FIRST).and.(.not.Input_Opt%LTRAN))
    CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, pUpdate )

    ! Save the initial tracer concentrations in the MINIT variable of
    ! GeosCore/strat_chem_mod.F90.  This has to be done here, after the
    ! very first call to AIRQNT, because we need State_Chm%AD to have been
    ! populated with non-zero values.  Otherwise the unit conversions will
    ! blow up and cause GCHP to crash. (bmy, 10/19/16)
    IF ( FIRST ) THEN
       IF ( Input_Opt%LSCHEM ) THEN
          ! Note: Init_Strat_Chem expects units of kg/kg dry
          CALL INIT_STRAT_CHEM( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
          Minit_is_set = .true.
       ENDIF
    ENDIF

    ! Cap the polar tropopause pressures at 200 hPa, in order to avoid
    ! tropospheric chemistry from happening too high up (cf. J. Logan)
    CALL GIGC_Cap_Tropopause_Prs  ( am_I_Root      = am_I_Root,  &
                                    IM             = IM,         &
                                    JM             = JM,         &
                                    Input_Opt      = Input_Opt,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

    ! Call PBL quantities. Those are always needed
    CALL COMPUTE_PBL_HEIGHT( State_Met )

    ! Force units to standard (kg/kg dry)
    If (.not.GIGC_Assert_Units(am_I_Root, State_Chm)) Then
       Call GIGC_Revert_Units( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
       ASSERT_(RC==GC_SUCCESS)
    End If
    
    ! SDE 05/28/13: Set H2O to STT if relevant
    IF ( IND_('H2O','A') > 0 ) THEN
       CALL SET_H2O_TRAC( am_I_Root, ((.NOT. Input_Opt%LUCX) .OR. Input_Opt%LSETH2O ), &
                          Input_Opt, State_Met, State_Chm, RC )
       ! Only force strat once if using UCX
       IF (Input_Opt%LSETH2O) Input_Opt%LSETH2O = .FALSE.
    ENDIF

    !=======================================================================
    ! EMISSIONS phase 1. Should be called every time to make sure that the
    ! HEMCO clock and the HEMCO data list are up to date.
    !=======================================================================
    CALL EMISSIONS_RUN( am_I_Root, Input_Opt, State_Met, &
                        State_Chm, DoEmis,    1, RC       )

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!!                                PHASE 1                                 !!!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    !=======================================================================
    ! 1. Convection (in v/v)
    ! 
    ! Call GEOS-Chem internal convection routines if convection is enabled
    ! in input.geos. This should only be done if convection is not covered
    ! by another gridded component and/or the GC species are not made
    ! friendly to this component!!
    !=======================================================================
    IF ( DoConv ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_CONV' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do convection now' 

       CALL DO_CONVECTION ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)
 
       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_CONV' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Convection done!'
    ENDIF   

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    !=======================================================================
    ! 2. Dry deposition.
    !
    ! This calculates the deposition rates in [s-1].
    !=======================================================================
    IF ( DoDryDep ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_DRYDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) THEN
          write(*,*) ' --- Do drydep now'
          write(*,*) '     Use FULL PBL: ', Input_Opt%PBL_DRYDEP
       endif

       ! Make sure tracers are in kg
       CALL ConvertSpc_KgKgDry_to_Kg( am_I_Root, State_Met, State_Chm, RC )

       ! Calculate drydep rates 
       CALL Do_DryDep   ( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?
       ASSERT_(RC==GC_SUCCESS)

       ! Revert units
       Call GIGC_Revert_Units( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_DRYDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Drydep done!'

    ENDIF ! Do drydep

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    !=======================================================================
    ! 3. Emissions (HEMCO). HEMCO must be called on first time step to make
    ! sure that the HEMCO data lists are all properly set up. 
    !=======================================================================
    IF ( DoEmis ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_EMIS' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do emissions now'

       ! Call HEMCO run interface 
       CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, DoEmis, Phase, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_EMIS' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Emissions done!'
    ENDIF

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    !=======================================================================
    ! If physics covers turbulence, simply add the emission and dry 
    ! deposition fluxes calculated above to the tracer array, without caring
    ! about the vertical distribution. The tracer tendencies are only added
    ! to the tracers array after emissions, drydep. So we need to use the
    ! emissions time step here.
    ! Subroutine DO_TEND operates in mass units, e.g. the tracers must be 
    ! in [kg].
    ! SDE 2016-04-05: Input units should now be v/v dry. 
    !=======================================================================
    IF ( DoTend ) THEN 
  
       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_FLUXES' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                           ' --- Add emissions and drydep to tracers'
 
       ! Make sure tracers are in v/v
       CALL ConvertSpc_KgKgDry_to_VVDry( am_I_Root, State_Chm, RC )

       ! Get emission time step [s]. 
       ASSERT_(ASSOCIATED(HcoState))
       DT = HcoState%TS_EMIS 

       ! Apply tendencies over entire PBL. Use emission time step.
       CALL DO_TEND ( am_I_Root, Input_Opt, State_Met, State_Chm, .FALSE., RC, DT=DT )
       ASSERT_(RC==GC_SUCCESS)

       ! Revert units
       Call GIGC_Revert_Units( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                                 '     Tendency time step [s]: ', DT 
 
       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                                 ' --- Fluxes applied to tracers!' 
 
       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_FLUXES' )

    ENDIF ! Tendencies 

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!!                              PHASE 2                                !!!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    !=======================================================================
    ! 4. Turbulence (v/v)
    !
    ! Call GEOS-Chem internal turbulence routines if turbulence is enabled
    ! in input.geos. This should only be done if turbulence is not covered
    ! by another gridded component and/or the GC species are not made
    ! friendly to this component!!
    ! Subroutine DO_MIXING operates in mixing ratios, e.g. the tracers must 
    ! be in [v/v]. 
    !=======================================================================
    IF ( DoTurb ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_TURB' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do turbulence now'

       ! Do mixing and apply tendencies. This will use the dynamic time step, which
       ! is fine since this call will be executed on every time step. 
       CALL DO_MIXING ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)

       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Turbulence done!'

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_TURB' )

    ENDIF

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    !=======================================================================
    ! 5. Chemistry
    !=======================================================================
    IF ( DoChem ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_CHEM' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do chemistry now'

       ! Calculate TOMS O3 overhead. For now, always use it from the
       ! Met field. State_Met%TO3 is imported from PCHEM.
       ! (ckeller, 10/21/2014).
       ! EWL: this is default in GCC. Need it offline as an option?
       !   (when we can, change .TRUE. to Input_Opt%USE_O3_FROM_MET)
       CALL COMPUTE_OVERHEAD_O3( am_I_Root, DAY, .TRUE., State_Met%TO3 )

       ! Set H2O to STT if relevant
       IF ( IND_('H2O','A') > 0 ) THEN
          CALL SET_H2O_TRAC( am_I_Root, (.not. Input_Opt%LUCX), Input_Opt, &
                             State_Met, State_Chm, RC )
       ENDIF

       ! Do chemistry
       CALL Do_Chemistry( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_CHEM' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Chemistry done!'
    ENDIF

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    !=======================================================================
    ! 6. Wet deposition
    !=======================================================================
    IF ( DoWetDep ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_WETDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do wetdep now'

       ! Do wet deposition
       CALL DO_WETDEP( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_WETDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Wetdep done!'
    ENDIF

    ! Check that units are correct
    ASSERT_(GIGC_Assert_Units(am_I_Root, State_Chm))

    ! SDE 2017-01-06: Archive the specific humidity to allow tracers to be
    ! modified currectly after SPHU is updated
    State_Met%SPHU_prev = State_Met%SPHU

    !=======================================================================
    ! Diagnostics 
    ! In an ESMF environment, all diagnostics are passed to the MAPL 
    ! HISTORY component every time step. Thus, we can call the diagnostics 
    ! at the end of the call sequence even though we don't know yet what the
    ! next step will be (e.g. we do not know yet if this is the last time 
    ! step of this month, etc.).
    !=======================================================================
    CALL MAPL_TimerOn( STATE, 'GC_DIAGN' )
    CALL Diagnostics_Write ( am_I_Root, Input_Opt, State_Chm, .FALSE., RC ) 
    CALL MAPL_TimerOff( STATE, 'GC_DIAGN' )

    !=======================================================================
    ! Clean up
    !=======================================================================

    ! Make sure tracers leave routine in v/v dry
    CALL ConvertSpc_KgKgDry_to_VVDry( am_I_Root, State_Chm, RC )

    ! testing only
    IF ( PHASE /= 1 .AND. NCALLS < 10 ) NCALLS = NCALLS + 1 

    ! This is not the first call any more
    FIRST = .FALSE.

    ! Return w/ success
    RC = GC_SUCCESS

  END SUBROUTINE GIGC_Chunk_Run
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_chunk_final
!
! !DESCRIPTION: Subroutine GC\_CHUNK\_FINAL deallocates pointers and
!  arrays used in the chemistry. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Chunk_Final( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE GIGC_Finalization_Mod
    USE Input_Opt_Mod,         ONLY : OptInput
    USE State_Chm_Mod,         ONLY : ChmState
    USE State_Met_Mod,         ONLY : MetState
    USE HCOI_GC_MAIN_MOD,      ONLY : HCOI_GC_FINAL
    USE Diagnostics_Mod,       ONLY : Diagnostics_Write
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt     ! Input Options object
    TYPE(ChmState), INTENT(INOUT) :: State_Chm     ! Chemistry State object
    TYPE(MetState), INTENT(INOUT) :: State_Met     ! Meteorology State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC            ! Success or failure
!
! !REVISION HISTORY: 
!  18 Jul 2011 - M. Long     - Initial Version
!  09 Oct 2012 - R. Yantosca - Added comments & cosmetic changes
!  16 Oct 2012 - R. Yantosca - Renamed GC_STATE argument to State_Chm
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_chm_mod.F90
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_met_mod.F90
!  22 Oct 2012 - R. Yantosca - Renamed to GIGC_Chunk_Final
!  01 Nov 2012 - R. Yantosca - Now reference gigc_input_opt_mod.F90
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Assume succes
    RC = GC_SUCCESS

    ! Diagnostics 
    CALL Diagnostics_Write( am_I_Root, Input_Opt, State_Chm, .TRUE., RC )

    ! Finalize HEMCO
    CALL HCOI_GC_FINAL( am_I_Root, .FALSE. )

    ! Finalize GEOS-Chem
    CALL GIGC_Finalize( am_I_Root = am_I_Root,  &  ! Are we on the root CPU?
                        Input_Opt = Input_Opt,  &  ! Input Options
                        State_Chm = State_Chm,  &  ! Chemistry State
                        State_Met = State_Met,  &  ! Meteorology State
                        RC        = RC         )   ! Success or failure?

  END SUBROUTINE GIGC_Chunk_Final
!EOC
END MODULE GIGC_Chunk_Mod
#endif
