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
  use ESMF
!  USE Mapping_Mod, ONLY : MapWeight

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
  PRIVATE :: SET_CH4
  PRIVATE :: SET_OZONOPAUSE
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

  ! Derived type objects
!  TYPE(MapWeight),      POINTER :: mapping(:,:) => NULL()

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
  SUBROUTINE GIGC_Chunk_Init( am_I_Root, I_LO,      J_LO,      I_HI,      &
                              J_HI,      IM,        JM,        LM,        &
                              IM_WORLD,  JM_WORLD,  LM_WORLD,  nymdB,     &
                              nhmsB,     nymdE,     nhmsE,     tsChem,    &
                              tsDyn,     lonCtr,    latCtr,    myPET,     &
                              Input_Opt, State_Chm, State_Met, HcoConfig, RC )
!
! !USES:
!
    USE ESMF,                    ONLY : ESMF_KIND_R4
    USE GIGC_Initialization_Mod, ONLY : GIGC_Init_Simulation
    USE ErrCode_Mod
    USE Input_Opt_Mod,           ONLY : OptInput
    USE State_Chm_Mod,           ONLY : ChmState
    USE State_Met_Mod,           ONLY : MetState
    USE Diagnostics_Mod,         ONLY : Diagnostics_Init
    USE EMISSIONS_MOD,           ONLY : EMISSIONS_INIT
    USE HCO_TYPES_MOD,           ONLY : ConfigObj
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
                               Input_Opt      = Input_Opt,  & ! Input Options
                               State_Chm      = State_Chm,  & ! Chemistry State
                               State_Met      = State_Met,  & ! Met State
                               myPET          = myPET,      & ! Local PET
!                               mapping        = mapping,    & ! Olson map wts
                               RC             = RC         )  ! Success?
    ASSERT_(RC==GC_SUCCESS)

    !=======================================================================
    ! Initialize HEMCO
    !=======================================================================
    CALL EMISSIONS_INIT ( am_I_Root, Input_Opt, State_Met, State_Chm, RC, &
                          HcoConfig=HcoConfig )
    ASSERT_(RC==GC_SUCCESS)


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
!    USE COMODE_MOD,         ONLY : CSPEC_FULL
!    USE COMODE_LOOP_MOD
!    USE comode_mod
    USE PRECISION_MOD
    USE PHYSCONSTANTS,      ONLY : AVO, AIRMW
    USE Chemistry_Mod,      ONLY : Do_Chemistry
    USE Dao_Mod,            ONLY : AirQnt, SET_DRY_SURFACE_PRESSURE, AVGPOLE
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
!    USE TRACERID_MOD
    USE WETSCAV_MOD,        ONLY : SETUP_WETSCAV, DO_WETDEP
    USE DRYDEP_MOD,         ONLY : DEPSAV, NUMDEP, NTRAIND
    USE CONVECTION_MOD,     ONLY : DO_CONVECTION
    USE TOMS_MOD,           ONLY : COMPUTE_OVERHEAD_O3
    USE GRID_MOD,           ONLY : AREA_M2
    USE Diagnostics_Mod,    ONLY : Diagnostics_Write
    USE EMISSIONS_MOD,      ONLY : EMISSIONS_RUN
    USE UVALBEDO_MOD,       ONLY : GET_UVALBEDO
!    USE Strat_Chem_Mod,     ONLY : Set_Init_Conc_Strat_Chem
    USE STRAT_CHEM_MOD,       ONLY : INIT_STRAT_CHEM, Minit_is_set

!    ! HEMCO update
    USE HCO_ERROR_MOD
    USE HCO_STATE_MOD,      ONLY : HCO_GetHcoID
    USE HCO_INTERFACE_MOD,  ONLY : HcoState, SetHcoTime
    USE HCO_INTERFACE_MOD,  ONLY : GetHcoVal 

    ! Apply tracer tendencies
    USE MIXING_MOD,         ONLY : DO_TEND, DO_MIXING

    ! UCX
    USE UCX_MOD,            ONLY : SET_H2O_TRAC

    ! Unit conversion (SE 2016-03-27)
    Use UnitConv_Mod
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
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MAPL_MetaComp), POINTER   :: STATE
    REAL*8                         :: DT
    INTEGER                        :: ERROR
!    REAL*8                         :: FLX, DEP, FRAC
!    REAL*8                         :: Emis8, Dep8
    INTEGER                        :: N
!    INTEGER                        :: N1, N2
    INTEGER                        :: SpcID, HcoID
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
    LOGICAL                        :: HEMCOhasCH4

!    ! Are tracers in mass or mixing ratio?
!    Integer                        :: CellUnit
!    Integer, Parameter             :: VVDry_Type=0
!    Integer, Parameter             :: KgKgDry_Type=1
!    Integer, Parameter             :: Kg_Type=2

    ! To convert molec cm-3 to kg/kg
    REAL(fp)                       :: MolecRatio, MW_g 
    REAL(hp)                       :: tmp 

    ! First call?
    LOGICAL, SAVE                  :: FIRST = .TRUE.

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

    ! Populate grid box areas from grid_mod.F on first call. We have to do 
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

    ! Set the pressure at level edges [hPa] from the ESMF environment
    CALL Accept_External_Pedge    ( am_I_Root      = am_I_Root,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

    ! Eventually set tropopause pressure according to ozone values
    ! (use ozonopause) 
    CALL SET_OZONOPAUSE ( am_I_Root, Input_Opt,  State_Met, &
                          State_Chm, IM, JM, LM, RC )
    IF ( RC /= GC_SUCCESS ) RETURN 

    ! Set dry surface pressure (PS1_DRY) from State_Met%PS1_WET
    ! and compute avg surface pressures near polar caps
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 1 )
!!!    CALL AVGPOLE( State_Met%PS1_DRY )

    ! Set dry surface pressure (PS2_DRY) from State_Met%PS2_WET
    ! and average as polar caps
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 2 )
!!!    CALL AVGPOLE( State_Met%PS2_DRY )

!!!    ! Compute avg moist pressure at polar caps 
!!!    CALL AVGPOLE( State_Met%PS2_WET )

!!!#if !defined( EXTERNAL_FORCING)
    ! Initialize surface pressures prior to interpolation
    ! to allow initialization of floating pressures
    State_Met%PSC2_WET = State_Met%PS1_WET
    State_Met%PSC2_DRY = State_Met%PS1_DRY
    CALL SET_FLOATING_PRESSURES( am_I_Root, State_Met, RC )
    IF ( RC /= GC_SUCCESS ) RETURN
!!!#endif 

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '0 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF

    ! Define airmass and related quantities
    !CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, (.not.FIRST) )
    CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, .FALSE. )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '1 negatives for species ',N
       ENDIF
    ENDDO
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

    ! Check what unit the tracers are in - need kg/kg dry for emissions
!    Select Case (TRIM(State_Chm%Spc_Units))
!        Case ('kg/kg dry')
!            ! Do nothing
!        Case ('kg')
!            CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Met, State_Chm, RC )
!        Case ('v/v dry')
!            CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Chm, RC )
!        Case Default
!            Write(6,'(a,a,a)') 'Tracer units (', State_Chm%Spc_Units, ') not recognized'
!            RC = GC_FAILURE
!            ASSERT_(RC==GC_SUCCESS)
!    End Select
    !!!CALL ConvertSpc_VVTotal_to_VVDry ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '2 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF

    ! vvtotal to vvdry
    DO N=1,State_Chm%nSpecies
       State_Chm%Species(:,:,:,N) = State_Chm%Species(:,:,:,N) &
                                  * State_Met%PMID(:,:,:) / State_Met%PMID_DRY(:,:,:)
    ENDDO

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '3 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF


!    CALL ConvertSpc_VVDry_to_KgKgDry ( am_I_Root, State_Chm, RC )
!    CellUnit = KgKgDry_Type
  
    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '4 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF
 
    ! Convert to kg/kg dry
    DO N=1,State_Chm%nSpecies

       ! Molecular weight of species
       MW_g = State_Chm%SpcData(N)%Info%EmMW_g

       ! Error trap
       IF ( MW_g < 0.0 ) THEN
          !IF ( am_I_Root ) write(*,*) 'Negative MW - set to 29: ', TRIM(State_Chm%SpcData(N)%Info%Name)
          MW_g = AIRMW * -1.0
!          IF ( First ) THEN
!             State_Chm%Species(:,:,:,N) = 1.0e-20
!          ENDIF
       ENDIF
 
       ! eventually convert molec/cm3
       IF ( MAXVAL(State_Chm%Species(:,:,:,N)) > 10.0 ) THEN

          !!! testing only
          write(*,*) 'Some values > 10.0!!! ',N
          State_Chm%Species(:,:,:,N) = 1.0e-20

!          ! Moles C / moles species
!          MolecRatio = State_Chm%SpcData(N)%Info%MolecRatio
!
!          ! Error trap
!          IF ( MolecRatio < 0.0 ) THEN
!             IF ( am_I_Root .and. .false. ) write(*,*) 'Negative MolecRatio for ', TRIM(State_Chm%SpcData(N)%Info%Name)
!             MolecRatio = 1.0
!          ENDIF
!
!          ! Do the unit conversion
!          State_Chm%Species(:,:,:,N) = State_Chm%Species(:,:,:,N)           &
!                                     * ( 1e+6_fp * MolecRatio )             &
!                                     / ( AVO / ( MW_g * 1.e-3_fp ) )        &
!                                     / State_Met%AIRDEN(:,:,:)
      
       ! else assume it's v/v dry
       ELSE
          ! v/v dry to kg/kg dry
          State_Chm%Species(:,:,:,N) = State_Chm%Species(:,:,:,N) &
                                     * ( MW_g / AIRMW )
       ENDIF
    ENDDO

    ! Update species units
    State_Chm%Spc_Units = 'kg/kg dry'

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '5 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF
 
    ! SDE 05/28/13: Set H2O to STT if relevant
    IF ( IND_('H2O','T') > 0 ) THEN
       CALL SET_H2O_TRAC( am_I_Root, ((.NOT. Input_Opt%LUCX) .OR. Input_Opt%LSETH2O ), &
                          Input_Opt, State_Met, State_Chm, RC )
       ! Only force strat once if using UCX
       IF (Input_Opt%LSETH2O) Input_Opt%LSETH2O = .FALSE.
    ENDIF

    ! check for negatives
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) '6 negatives for species ',N
       ENDIF
    ENDDO
    ENDIF

    ! Save the initial tracer concentrations in the MINIT variable of
    ! GeosCore/strat_chem_mod.F90.  This has to be done here, after the
    ! very first call to AIRQNT, because we need State_Chm%AD to have been
    ! populated with non-zero values.  Otherwise the unit conversions will
    ! blow up and cause GCHP to crash. (bmy, 10/19/16)
    IF ( FIRST ) THEN
       IF ( Input_Opt%LSCHEM ) THEN
          ! Note: Init_Strat_Chem expects units of kg/kg dry
          CALL INIT_STRAT_CHEM( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
!<<GONE>>          CALL Set_Init_Conc_Strat_Chem( am_I_Root, Input_Opt,     &
!<<GONE>>                                         State_Met, State_Chm, RC )
          write(*,*) '<><><> INIT_STRAT_CHEM complete'
          Minit_is_set = .true.
       ENDIF
    ENDIF

    ! testing only 
    IF ( am_I_Root.and..false.) THEN
    DO N=1,State_Chm%nSpecies
       write(*,*) 'A species ',N,': ',MINVAL(State_Chm%Species(:,:,:,N)),MAXVAL(State_Chm%Species(:,:,:,N))
    ENDDO
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
  
!       ! Make sure tracers are in kg/kg
!       IF ( CellUnit.ne.KgKgDry_Type ) Then
!          If ( CellUnit .eq. Kg_Type ) Then
!             CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Met, State_Chm, RC )
!          ElseIf ( CellUnit .eq. VVDry_Type) Then
!             CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Chm, RC )
!          EndIf
!          CellUnit = KgKgDry_Type
!       ENDIF

       CALL DO_CONVECTION ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)
 
       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_CONV' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Convection done!'
    ENDIF   

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

!       ! Make sure tracers are in kg
!       IF ( CellUnit.ne.Kg_Type ) Then
!          If ( CellUnit .eq. KgKgDry_Type ) Then
!             CALL Convert_KgKgDry_to_Kg( am_I_Root, Input_Opt,&
!                                         State_Met, State_Chm, RC )
!          ElseIf ( CellUnit .eq. VVDry_Type) Then
!             CALL Convert_VVDry_to_Kg( am_I_Root, Input_Opt,&
!                                            State_Met, State_Chm, RC )
!          EndIf
!          CellUnit = Kg_Type
!       ENDIF

       ! Update & Remap Land-type arrays from Surface Grid-component
       !CALL GEOS5_TO_OLSON_LANDTYPE_REMAP( State_Met, RC )    
    
       ! Calculate drydep rates 
       CALL Do_DryDep   ( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_DRYDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Drydep done!'

    ENDIF ! Do drydep

    !=======================================================================
    ! 3. Emissions (HEMCO). HEMCO must be called on first time step to make
    ! sure that the HEMCO data lists are all properly set up. 
    !=======================================================================
    IF ( DoEmis ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_EMIS' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do emissions now'

!       ! Make sure tracers are in kg/kg dry
!       IF ( CellUnit.ne.KgKgDry_Type ) Then
!          If ( CellUnit .eq. Kg_Type ) Then
!             CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Met, State_Chm, RC )
!          ElseIf ( CellUnit .eq. VVDry_Type) Then
!             CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Chm, RC )
!          EndIf
!          CellUnit = KgKgDry_Type
!       ENDIF

       ! Call HEMCO run interface - Phase 2 
       !CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, DoEmis, Phase, RC )
       CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, IsChemTime, 2, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_EMIS' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Emissions done!'
    ENDIF

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
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Add emissions and drydep to tracers'
 
       ! Make sure tracers are in v/v
       IF ( TRIM(State_Chm%Spc_Units) == 'kg/kg dry' ) THEN
          CALL ConvertSpc_KgKgDry_to_VVDry( am_I_Root, State_Chm, RC ) 
          IF ( RC /= GC_SUCCESS ) RETURN 
       ENDIF

       ! Get emission time step [s]. 
       ASSERT_(ASSOCIATED(HcoState))
       DT = HcoState%TS_EMIS 
!       HcoState => NULL()

       ! Apply tendencies over entire PBL. Use emission time step.
       CALL DO_TEND ( am_I_Root, Input_Opt, State_Met, State_Chm, .FALSE., RC, DT=DT )
       ASSERT_(RC==GC_SUCCESS)

       ! Convert species conc back to [kg/kg dry air] after mixing (ewl, 8/12/15)
       CALL ConvertSpc_VVDry_to_KgKgDry( am_I_Root, State_Chm, RC ) 
       IF ( RC /= GC_SUCCESS ) RETURN 

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) '     Tendency time step [s]: ', DT 
 
       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Fluxes applied to tracers!' 
 
       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_FLUXES' )

    ENDIF ! Tendencies 

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

!       ! Make sure tracers are in kg/kg
!       IF ( CellUnit.ne.KgKgDry_Type ) Then
!          If ( CellUnit .eq. VVDry_Type ) Then
!             CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Chm, RC )
!          ElseIf ( CellUnit .eq. Kg_Type) Then
!             CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Met, State_Chm, RC )
!          EndIf
!          CellUnit = KgKgDry_Type
!       ENDIF

       ! Do mixing and apply tendencies. This will use the dynamic time step, which
       ! is fine since this call will be executed on every time step. 
       CALL DO_MIXING ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)

       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Turbulence done!'

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_TURB' )

    ENDIF

    ! Set tropospheric CH4 concentrations and fill species array with
    ! current values. 
    IF ( DoTurb .OR. DoTend ) THEN

       ! Check if CH4 emissions are defined through HEMCO
       SpcID       = ind_('CH4')
       HEMCOhasCH4 = .FALSE.
       IF ( SpcID > 0 .AND. ASSOCIATED(HcoState) ) THEN
          HcoID = HCO_GetHcoID( 'CH4', HcoState )
          IF ( HcoID > 0 ) THEN 
             CALL GetHcoVal ( HcoID, 1, 1, 1, HEMCOhasCH4, Emis=tmp )
          ENDIF
       ENDIF

       ! Set fixed trop. CH4 if it is not an emission species.
       IF ( .NOT. HEMCOhasCH4 ) THEN
          CALL SET_CH4 ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                         IM, JM, Year, RC )
       ENDIF
    ENDIF


    !=======================================================================
    ! 5. Chemistry
    !=======================================================================
    IF ( DoChem ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_CHEM' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do chemistry now'

       ! Make sure tracers are in kg
!       IF ( CellUnit.ne.KgKgDry_Type ) Then
!          If ( CellUnit .eq. VVDry_Type ) Then
!             CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Chm, RC )
!          ElseIf ( CellUnit .eq. Kg_Type) Then
!             CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Met, State_Chm, RC )
!          EndIf
!          CellUnit = KgKgDry_Type
!       ENDIF
!
!<<GONE>>       ! Write JLOP_PREVIOUS into JLOP to make sure that JLOP contains 
!<<GONE>>       ! the current values of JLOP_PREVIOUS. In chemdr.F, JLOP_PREVIOUS is filled 
!<<GONE>>       ! with JLOP before resetting JLOP to current values and we simply want to 
!<<GONE>>       ! make sure that JLOP_PREVIOUS is not set to zero everywhere on the first
!<<GONE>>       ! call (when JLOP is still all zero).
!<<GONE>>       JLOP = JLOP_PREVIOUS
!<<GONE>>
!<<GONE>>       ! Zero Rate arrays  
!<<GONE>>       RRATE = 0.E0
!<<GONE>>       TRATE = 0.E0

       ! Calculate TOMS O3 overhead. For now, always use it from the
       ! Met field. State_Met%TO3 is imported from PCHEM.
       ! (ckeller, 10/21/2014).
       CALL COMPUTE_OVERHEAD_O3( am_I_Root, DAY, .TRUE., State_Met%TO3 )

       ! Set H2O to STT if relevant
       IF ( IND_('H2O','T') > 0 ) THEN
          CALL SET_H2O_TRAC( am_I_Root, (.not. Input_Opt%LUCX), Input_Opt, &
                             State_Met, State_Chm, RC )
       ENDIF

       ! For Fast-JX, update UV albedo data. These values are currently
       ! read from a climatology (via HEMCO). We may eventually get them
       ! from GEOS-5, but I don't know which field to use (ckeller, 3/9/15).
       ! Note: now use ALBVF from GEOS-5.
       IF ( UVmonth /= month ) THEN
          CALL GET_UVALBEDO( am_I_Root, Input_Opt, State_Met, RC )
          ASSERT_(RC==GC_SUCCESS)
          UVmonth = month
       ENDIF 

       ! testing only 
       IF ( am_I_Root.and..false.) THEN
       write(*,*) 'Now calling do_chemistry'
       DO N=1,State_Chm%nSpecies
          write(*,*) 'AA species ',N,': ',MINVAL(State_Chm%Species(:,:,:,N)),MAXVAL(State_Chm%Species(:,:,:,N))
       ENDDO
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
       IF ( am_I_Root.and..false.) THEN
       DO N=1,State_Chm%nSpecies
          write(*,*) 'B species ',N,': ',MINVAL(State_Chm%Species(:,:,:,N)),MAXVAL(State_Chm%Species(:,:,:,N))
       ENDDO
       ENDIF

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Chemistry done!'
    ENDIF

    !=======================================================================
    ! 6. Wet deposition
    !=======================================================================
    IF ( DoWetDep ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_WETDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do wetdep now'

!       ! Make sure tracers are in kg/kg dry
!       IF ( CellUnit.ne.KgKgDry_Type ) Then
!          If ( CellUnit .eq. VVDry_Type ) Then
!             CALL Convert_VVDry_to_KgKgDry( am_I_Root, Input_Opt,&
!                                         State_Chm, RC )
!          ElseIf ( CellUnit .eq. Kg_Type) Then
!             CALL Convert_Kg_to_KgKgDry( am_I_Root, Input_Opt,&
!                                            State_Met, State_Chm, RC )
!          EndIf
!          CellUnit = KgKgDry_Type
!       ENDIF


       ! Do wet deposition
       CALL DO_WETDEP( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_WETDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Wetdep done!'
    ENDIF

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

!    ! Make sure tracers leave routine in v/v moist
!    CALL ConvertSpc_KgKgDry_to_VVDry ( am_I_Root, State_Chm, RC )

    ! Convert to v/v dry
    DO N=1,State_Chm%nSpecies

       ! Molecular weight of species
       MW_g = State_Chm%SpcData(N)%Info%EmMW_g
       IF ( MW_g <= 0.0 ) THEN
          !IF ( am_I_Root ) write(*,*) 'Negative MW - set to 29: ', TRIM(State_Chm%SpcData(N)%Info%Name)
          MW_g = AIRMW * -1.0
       ENDIF

       ! v/v dry to kg/kg dry
       State_Chm%Species(:,:,:,N) = State_Chm%Species(:,:,:,N) &
                                  / ( MW_g / AIRMW )
    ENDDO

    ! vvdry to vvmoist
    DO N=1,State_Chm%nSpecies
       State_Chm%Species(:,:,:,N) = State_Chm%Species(:,:,:,N) &
                                  / State_Met%PMID(:,:,:) * State_Met%PMID_DRY(:,:,:)
    ENDDO

    ! testing only 
    IF ( am_I_Root.and..false.) THEN
       DO N=1,State_Chm%nSpecies
          write(*,*) 'C species ',N,': ',MINVAL(State_Chm%Species(:,:,:,N)),MAXVAL(State_Chm%Species(:,:,:,N))
       ENDDO
    ENDIF

    ! check for negatives
    IF ( am_I_Root .and. .false.) THEN
    DO N=1,State_Chm%nSpecies
       IF ( ANY(State_Chm%Species(:,:,:,N)<0.0) ) THEN
        write(*,*) 'B negatives for species ',N
       ENDIF
    ENDDO
    ENDIF

!    CALL Convert_VVDry_to_VVTotal ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )

!    ! Make sure tracers leave routine in v/v
!    IF ( CellUnit.ne.VVDry_Type ) Then
!       If ( CellUnit .eq. KgKgDry_Type ) Then
!          CALL Convert_KgKgDry_to_VVDry( am_I_Root, Input_Opt,&
!                                      State_Chm, RC )
!       ElseIf ( CellUnit .eq. Kg_Type) Then
!          CALL Convert_Kg_to_VVDry( am_I_Root, Input_Opt,&
!                                         State_Met, State_Chm, RC )
!       EndIf
!       CellUnit = VVDry_Type
!    ENDIF

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
    CALL Diagnostics_Write ( am_I_Root, Input_Opt, State_Chm, .TRUE., RC ) 

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
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_cap_tropopause_prs
!
! !DESCRIPTION: Subroutine GIGC\_CAP\_TROPOPAUSE\_PRS caps the tropopause
!  pressure in polar latitudes to 200 hPa, so that we don't end up doing
!  tropopsheric chemistry too high over the poles.  This is done in the
!  standalone GEOS-Chem, and we also need to apply this when running
!  GEOS-Chem within the GEOS-5 GCM.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Cap_Tropopause_Prs( am_I_Root, IM,        JM,  &
                                      Input_Opt, State_Met, RC  )
!
! !USES:
!
    USE ErrCode_Mod
    USE Input_Opt_Mod,      ONLY : OptInput
    USE State_Met_Mod,      ONLY : MetState
    USE Grid_Mod,           ONLY : Get_XEdge
    USE Grid_Mod,           ONLY : Get_YEdge
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
    INTEGER,        INTENT(IN)    :: IM            ! # of lons on this CPU
    INTEGER,        INTENT(IN)    :: JM            ! # of lats on this CPU
    TYPE(OptInput), INTENT(IN)    :: Input_Opt     ! Input Options object
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(MetState), INTENT(INOUT) :: State_Met     ! Meteorology State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC            ! Success or failure
!
! !REMARKS:
!  Jennifer Logan (see correspondence below) suggested that we should cap the 
!  variable tropopause at 200hPa in near-polar regions (90-60S and 60-90N), 
!  to avoid the problem with anomalously high tropopause heights at high 
!  latitudes. This fix was standardized in GEOS-Chem v7-04-13.
!                                                                             .
!  Jennifer Logan wrote:
!     I think we should restrict the tropopause at latitudes > 60 deg. to 
!     pressures greater than 200 mb (about 11 km). From Fig. 3 in Seidel and 
!     Randel, there are tropopause (TP) heights as high as 13.5 km in the 
!     Antarctic (median height is ~9.8 km, 250 mb), but I don't think we want 
!     to be doing trop. chem there. The median TP pressure at ~80 N is ~300 mb, 
!     compared to ~250 mb at 70-85 S. The extratropical TP heights are higher
!     (lower pressure) in the SH than in the NH according to Fig. 3. 
!     This approach is also very easy to explain in a paper. 
! 
! !REVISION HISTORY: 
!  14 Mar 2013 - R. Yantosca - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: I,       J
    REAL*8  :: YSOUTH,  YNORTH

    ! Assume success
    RC = GC_SUCCESS

    ! Return if option not set
    IF ( .NOT. Input_Opt%LCAPTROP ) RETURN

    ! Loop over grid boxes on this PET
    DO J = 1, JM
    DO I = 1, IM

       ! North & south edges of box
       YSOUTH = GET_YEDGE( I, J,   1 )
       YNORTH = GET_YEDGE( I, J+1, 1 )

       ! Cap tropopause height at 200 hPa polewards of 60N and 60S
       IF ( YSOUTH >= 60d0 .or. YNORTH <= -60d0 ) THEN
          State_Met%TROPP(I,J) = MAX( State_Met%TROPP(I,J), 200d0 )
       ENDIF

    ENDDO
    ENDDO

  END SUBROUTINE GIGC_Cap_Tropopause_Prs
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: set_ch4 
!
! !DESCRIPTION: Subroutine SET\_CH4 updates tropospheric CH4 concentrations in 
!  the tracer array using prescribed latitutinal values. It then also updates
!  the CH4 species array accordingly. State_Chm%Tracers is expected to enter
!  this routine in units of kg/kg dry. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE SET_CH4 ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                       IM, JM, Year, RC )
                                      
!
! !USES:
!
    USE Precision_Mod 
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Met_Mod, ONLY : MetState
    USE State_Chm_Mod, ONLY : ChmState, Ind_
    USE Grid_Mod,      ONLY : GET_YMID
    USE PHYSCONSTANTS, ONLY : AIRMW

    USE CHEMGRID_MOD,  ONLY : GET_CHEMGRID_LEVEL
 
!    ! Testing only
!    USE COMODE_LOOP_MOD,    ONLY : C3090S, C0030S, C0030N, C3090N
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
    TYPE(OptInput), INTENT(IN)    :: Input_Opt     ! Input Options object
    TYPE(MetState), INTENT(IN)    :: State_Met     ! Meteorology State object
    INTEGER,        INTENT(IN)    :: IM            ! # of lons on this CPU
    INTEGER,        INTENT(IN)    :: JM            ! # of lats on this CPU
    INTEGER,        INTENT(IN)    :: Year          ! Current year 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ChmState), INTENT(INOUT) :: State_Chm     ! Chem state object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC            ! Success or failure
!
! !REMARKS:
! 
! !REVISION HISTORY: 
!  10 Nov 2015 - C. Keller   - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER  :: I, J, L
    INTEGER  :: SpcID
    REAL*8   :: YMID 
    REAL(fp) :: THISCH4
    REAL(fp) :: MWg
    REAL(fp) :: C3090S, C0030S, C0030N, C3090N

    LOGICAL, PARAMETER :: VARIABLE_CH4 = .TRUE.

    ! Assume success
    RC = GC_SUCCESS

    ! CH4 species index
    SpcID = ind_('CH4')

    ! Nothing to do if this is not a species or if UCX is on
    IF ( SpcID <= 0 .OR. Input_Opt%LUCX ) RETURN

    ! Set zonal averaged CH4 concentrations (ppbv)
    CALL GET_GLOBAL_CH4( Year, VARIABLE_CH4, &
                         C3090S, C0030S, C0030N, C3090N, &
                         am_I_Root, Input_Opt )

    ! Molecular weight
    MWg = State_Chm%SpcData(SpcID)%Info%emMW_g
    IF ( MWg < 0 ) THEN
       MWg = 16.0
    ENDIF

    ! Loop over grid boxes on this PET
    DO J = 1, JM
    DO I = 1, IM

       ! Lat midpoint of box
       YMID = GET_YMID( I, J, 1 )
   
       ! Set CH4 (v/v)
       THISCH4 = 0.0_fp
       IF ( YMID < -30.0_fp ) THEN
          THISCH4 = C3090S * 1.0e-9_fp
       ELSEIF ( YMID >= -30.0_fp .AND. YMID < 0.0_fp ) THEN
          THISCH4 = C0030S * 1.0e-9_fp
       ELSEIF ( YMID >= 0.0_fp .AND. YMID < 30.0_fp ) THEN
          THISCH4 = C0030N * 1.0e-9_fp
       ELSEIF ( YMID >= 30.0_fp ) THEN
          THISCH4 = C3090N * 1.0e-9_fp
       ENDIF
  
       ! Distribute over entire troposphere [kg/kg dry]
       ! L = GET_CHEMGRID_LEVEL( I, J, State_Met )

       ! Add to surface layer
       L = 1
       State_Chm%Species(I,J,1:L,SpcID) = THISCH4 * MWg / AIRMW
       !!!State_Chm%Tracers(I,J,1:L,SpcID) = THISCH4 * State_Met%AD(I,J,1:L) / Input_Opt%TCVV(IDTCH4)
    ENDDO
    ENDDO

    ! Verbose
    IF ( am_I_Root ) THEN
       WRITE(*,*) 'GEOS-Chem: use prescribed CH4 surface concentrations'
    ENDIF

  END SUBROUTINE SET_CH4 
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: set_ozonopause
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE SET_OZONOPAUSE ( am_I_Root, Input_Opt,  State_Met, &
                              State_Chm, IM, JM, LM, RC )
                                      
!
! !USES:
!
    USE ErrCode_Mod
    USE Precision_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Met_Mod, ONLY : MetState
    USE State_Chm_Mod, ONLY : ChmState, Ind_
!    USE TRACERID_MOD,       ONLY : IDTO3
    USE PRESSURE_MOD,       ONLY : GET_PCENTER
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
    TYPE(OptInput), INTENT(IN)    :: Input_Opt     ! Input Options object
    INTEGER,        INTENT(IN)    :: IM            ! # of lons on this CPU
    INTEGER,        INTENT(IN)    :: JM            ! # of lats on this CPU
    INTEGER,        INTENT(IN)    :: LM            ! # of levels 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(MetState), INTENT(INOUT) :: State_Met     ! Meteorology State object
    TYPE(ChmState), INTENT(INOUT) :: State_Chm     ! Chem state object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC            ! Success or failure
!
! !REMARKS:
! 
! !REVISION HISTORY: 
!  10 Nov 2015 - C. Keller   - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER            :: I, J, L, N, IDTO3
    LOGICAL            :: IsInStrat
    REAL(fp)           :: O3val
    INTEGER, PARAMETER :: NLEVEL = 3

    ! Assume success
    RC = GC_SUCCESS

    ! Species index
    IDTO3 = ind_('O3')

    ! Return here if ozonopause parameter has invalid value
    IF ( Input_Opt%OZONOPAUSE <= 0.0_fp .OR. IDTO3 < 0 ) RETURN

    ! Reset tropopause pressures
    State_Met%TROPP(:,:) = 0.0_fp

    ! ozonopause value (ppb --> v/v) 
    O3val = Input_Opt%OZONOPAUSE * 1.0e-9_fp

    ! Loop over grid boxes on this PET
    DO J = 1, JM
    DO I = 1, IM

       IsInStrat = .FALSE.

       ! Find first level where ozone concentration exceeds threshold 
       DO L = 1, LM
          IF ( State_Chm%Species(I,J,L,IDTO3) >= O3val ) THEN
             ! Sanity check: level above should have higher ozone and  
             ! pressure should be above 500hPa
             IF ( L > ( LM-NLEVEL+1 ) ) THEN
                IsInStrat = .TRUE.
             ELSEIF ( GET_PCENTER(I,J,L) >= 500.0_fp ) THEN
                IsInStrat = .FALSE.
             ELSE
                IsInStrat = .TRUE.
                DO N = 1, NLEVEL
                   IF ( State_Chm%Species(I,J,L+N,IDTO3) < State_Chm%Species(I,J,L,IDTO3) ) THEN
                      IsInStrat = .FALSE.
                      EXIT
                   ENDIF
                ENDDO
             ENDIF
          ENDIF

          ! Set TROPP to value in middle of this grid box
          IF ( IsInStrat ) THEN
                State_Met%TROPP(I,J) = GET_PCENTER(I,J,L) 
             EXIT ! End L loop
          ENDIF
       ENDDO
    ENDDO
    ENDDO

  END SUBROUTINE SET_OZONOPAUSE
!EOC
END MODULE GIGC_Chunk_Mod
#endif
