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
                              Input_Opt, State_Chm, State_Met, RC      )
!
! !USES:
!
    USE ESMF,                    ONLY : ESMF_KIND_R4
    USE GIGC_Initialization_Mod, ONLY : GIGC_Init_Simulation
    USE GIGC_ErrCode_Mod
    USE GIGC_Input_Opt_Mod,      ONLY : OptInput
    USE GIGC_State_Chm_Mod,      ONLY : ChmState
    USE GIGC_State_Met_Mod,      ONLY : MetState
    USE Diagnostics_Mod,         ONLY : Diagnostics_Init
    USE EMISSIONS_MOD,           ONLY : EMISSIONS_INIT
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
    RC = GIGC_SUCCESS

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
    ASSERT_(RC==GIGC_SUCCESS)

    ! Initialize diagnostics.
    CALL Diagnostics_Init( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
    ASSERT_(RC==GIGC_SUCCESS)
 
    !=======================================================================
    ! Initialize HEMCO
    !=======================================================================
    CALL EMISSIONS_INIT ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
    ASSERT_(RC==GIGC_SUCCESS)

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
    USE COMODE_LOOP_MOD
    USE comode_mod
    USE Chemistry_Mod,      ONLY : Do_Chemistry
    USE Dao_Mod,            ONLY : Convert_Units, AirQnt
    USE DryDep_Mod,         ONLY : Do_DryDep
    USE GC_Land_Interface
    USE GIGC_ErrCode_Mod
    USE GIGC_Input_Opt_Mod, ONLY : OptInput
    USE GIGC_State_Chm_Mod, ONLY : ChmState
    USE GIGC_State_Met_Mod, ONLY : MetState
    USE PBL_MIX_MOD,        ONLY : DO_PBL_MIX, COMPUTE_PBL_HEIGHT
    USE VDIFF_MOD,          ONLY : DO_PBL_MIX_2
    USE Pressure_Mod,       ONLY : Accept_External_Pedge, Set_Floating_Pressure
    USE Time_Mod,           ONLY : Accept_External_Date_Time
    USE Time_Mod,           ONLY : ITS_TIME_FOR_CHEM
    USE TIME_MOD,           ONLY : GET_TS_CHEM, GET_TS_DYN
    USE TRACERID_MOD
    USE WETSCAV_MOD,        ONLY : INIT_WETSCAV, DO_WETDEP
    USE DRYDEP_MOD,         ONLY : DEPSAV, NUMDEP, NTRAIND
    USE CONVECTION_MOD,     ONLY : DO_CONVECTION
    USE TOMS_MOD,           ONLY : COMPUTE_OVERHEAD_O3
    USE GRID_MOD,           ONLY : AREA_M2
    USE Diagnostics_Mod,    ONLY : Diagnostics_Write
    USE EMISSIONS_MOD,      ONLY : EMISSIONS_RUN
    USE UVALBEDO_MOD,       ONLY : GET_UVALBEDO

!    ! HEMCO update
    USE HCO_ERROR_MOD
    USE HCO_STATE_MOD,      ONLY : HCO_STATE
    USE HCOI_GC_MAIN_MOD,   ONLY : GetHcoState, SetHcoTime
!    USE HCOI_GC_MAIN_MOD,   ONLY : GetHcoVal,   GetHcoID

    ! Apply tracer tendencies
    USE MIXING_MOD,         ONLY : DO_TEND, DO_MIXING

    ! UCX
    USE UCX_MOD,            ONLY : SET_H2O_TRAC
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
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MAPL_MetaComp), POINTER   :: STATE
    TYPE(HCO_STATE), POINTER       :: HcoState => NULL() 
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

    ! Are tracers in mass or mixing ratio?
    LOGICAL                        :: isMass

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
    RC = GIGC_SUCCESS

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

    ! Eventually initialize/reset wetdep
    IF ( DoConv .OR. DoChem .OR. DoWetDep ) THEN
       CALL INIT_WETSCAV( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
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

#if !defined( EXTERNAL_FORCING)
    CALL Set_Floating_Pressure( State_Met%PS1 )
#endif 

    CALL AirQnt( am_I_Root, State_Met, RC )

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

    ! Tracers enter in v/v. Convert to kg. 
    CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                          N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                          TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                          AD        = State_Met%AD,         & ! Air mass [kg]
                          STT       = State_Chm%Tracers    )  ! Tracer array
    IsMass = .TRUE.
    
    ! SDE 05/28/13: Set H2O to STT if relevant
    IF ( IDTH2O > 0 ) THEN
       CALL SET_H2O_TRAC( am_I_Root, ((.NOT. Input_Opt%LUCX) .OR. Input_Opt%LSETH2O ), &
                          Input_Opt, State_Met, State_Chm, RC )
       ! Only force strat once if using UCX
       IF (Input_Opt%LSETH2O) Input_Opt%LSETH2O = .FALSE.
    ENDIF

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
  
       ! Make sure tracers are in v/v
       IF ( isMass ) THEN 
          CALL Convert_Units  ( IFLAG     = 1,                    & ! [kg] --> [v/v]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .FALSE.
       ENDIF

       CALL DO_CONVECTION ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GIGC_SUCCESS)
 
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

       ! Make sure tracers are in kg
       IF ( .NOT. IsMass ) THEN
          CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .TRUE.
       ENDIF

       ! Update & Remap Land-type arrays from Surface Grid-component
       !CALL GEOS5_TO_OLSON_LANDTYPE_REMAP( State_Met, RC )    
    
       ! Calculate drydep rates 
       CALL Do_DryDep   ( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?
       ASSERT_(RC==GIGC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_DRYDEP' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Drydep done!'

    ENDIF ! Do drydep

    !=======================================================================
    ! 3. Emissions (HEMCO). HEMCO must be called on first time step to make
    ! sure that the HEMCO data lists are all properly set up. 
    !=======================================================================
    IF ( DoEmis .OR. FIRST ) THEN

       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_EMIS' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do emissions now'

       ! Make sure tracers are in kg
       IF ( .NOT. IsMass ) THEN
          CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .TRUE.
       ENDIF

       ! Call HEMCO run interface 
       CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, DoEmis, Phase, RC )
       ASSERT_(RC==GIGC_SUCCESS)

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
    !=======================================================================
    IF ( DoTend ) THEN 
  
       ! Timer on
       CALL MAPL_TimerOn( STATE, 'GC_FLUXES' )

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Add emissions and drydep to tracers'
 
       ! Make sure tracers are in kg
       IF ( .NOT. IsMass ) THEN
          CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .TRUE.
       ENDIF

       ! Get emission time step [s]. 
       CALL GetHcoState( HcoState )
       ASSERT_(ASSOCIATED(HcoState))
       DT = HcoState%TS_EMIS 
       HcoState => NULL()

       ! Apply tendencies over entire PBL. Use emission time step.
       CALL DO_TEND ( am_I_Root, Input_Opt, State_Met, State_Chm, .FALSE., RC, DT=DT )
       ASSERT_(RC==GIGC_SUCCESS)

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

       ! Make sure tracers are in v/v
       IF ( isMass ) THEN 
          CALL Convert_Units  ( IFLAG     = 1,                    & ! [kg] --> [v/v]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .FALSE.
       ENDIF

       ! Do mixing and apply tendencies. This will use the dynamic time step, which
       ! is fine since this call will be executed on every time step. 
       CALL DO_MIXING ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GIGC_SUCCESS)

       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Turbulence done!'

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_TURB' )

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
       IF ( .NOT. IsMass ) THEN
          CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .TRUE.
       ENDIF

       ! Write JLOP_PREVIOUS into JLOP to make sure that JLOP contains 
       ! the current values of JLOP_PREVIOUS. In chemdr.F, JLOP_PREVIOUS is filled 
       ! with JLOP before resetting JLOP to current values and we simply want to 
       ! make sure that JLOP_PREVIOUS is not set to zero everywhere on the first
       ! call (when JLOP is still all zero).
       JLOP = JLOP_PREVIOUS

       ! Zero Rate arrays  
       RRATE = 0.E0
       TRATE = 0.E0

       ! Calculate TOMS O3 overhead. For now, always use it from the
       ! Met field. State_Met%TO3 is imported from PCHEM.
       ! (ckeller, 10/21/2014).
       CALL COMPUTE_OVERHEAD_O3( am_I_Root, DAY, .TRUE., State_Met%TO3 )

       ! Set H2O to STT if relevant
       IF ( IDTH2O > 0 ) THEN
          CALL SET_H2O_TRAC( am_I_Root, (.not. Input_Opt%LUCX), Input_Opt, &
                             State_Met, State_Chm, RC )
       ENDIF

       ! For Fast-JX, update UV albedo data. These values are currently
       ! read from a climatology (via HEMCO). We may eventually get them
       ! from GEOS-5, but I don't know which field to use (ckeller, 3/9/15).
       ! Note: now use ALBVF from GEOS-5.
       IF ( UVmonth /= month ) THEN
          CALL GET_UVALBEDO( am_I_Root, Input_Opt, State_Met, RC )
          ASSERT_(RC==GIGC_SUCCESS)
          UVmonth = month
       ENDIF 

       ! Do chemistry
       CALL Do_Chemistry( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?
       ASSERT_(RC==GIGC_SUCCESS)

       ! Timer off
       CALL MAPL_TimerOff( STATE, 'GC_CHEM' )

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

       ! Make sure tracers are in kg
       IF ( .NOT. IsMass ) THEN
          CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .TRUE.
       ENDIF

       ! Do wet deposition
       CALL DO_WETDEP( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
       ASSERT_(RC==GIGC_SUCCESS)

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
    CALL Diagnostics_Write( am_I_Root, Input_Opt, .FALSE., RC )
    CALL MAPL_TimerOff( STATE, 'GC_DIAGN' )

    !=======================================================================
    ! Clean up
    !=======================================================================

    ! Make sure tracers leave routine in v/v
    IF ( IsMass ) THEN
       CALL Convert_Units  ( IFLAG     = 1,                    & ! [kg] -> [v/v]
                             N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                             TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                             AD        = State_Met%AD,         & ! Air mass [kg]
                             STT       = State_Chm%Tracers    )  ! Tracer array
       IsMass = .FALSE.
    ENDIF

    ! testing only
    IF ( PHASE /= 1 .AND. NCALLS < 10 ) NCALLS = NCALLS + 1 

    ! This is not the first call any more
    FIRST = .FALSE.

    ! Return w/ success
    RC = GIGC_SUCCESS

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
    USE GIGC_ErrCode_Mod
    USE GIGC_Finalization_Mod
    USE GIGC_Input_Opt_Mod,    ONLY : OptInput
    USE GIGC_State_Chm_Mod,    ONLY : ChmState
    USE GIGC_State_Met_Mod,    ONLY : MetState
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
    RC = GIGC_SUCCESS

    ! Diagnostics 
    CALL Diagnostics_Write( am_I_Root, Input_Opt, .TRUE., RC )

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
    USE GIGC_ErrCode_Mod
    USE GIGC_Input_Opt_Mod, ONLY : OptInput
    USE GIGC_State_Met_Mod, ONLY : MetState
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
    RC = GIGC_SUCCESS

    ! Loop over grid boxes on this PET
    DO J = 1, JM
    DO I = 1, IM

       ! North & south edges of box
       YSOUTH = GET_YEDGE( I, J,   1 )
       YNORTH = GET_YEDGE( I, J+1, 1 )

       ! Cap tropopause height at 200 hPa polewards of 60N and 60S
!       IF ( YSOUTH >= 60d0 .or. YNORTH <= -60d0 ) THEN
!          State_Met%TROPP(I,J) = MAX( State_Met%TROPP(I,J), 200d0 )
!       ENDIF

    ENDDO
    ENDDO

  END SUBROUTINE GIGC_Cap_Tropopause_Prs
!EOC
END MODULE GIGC_Chunk_Mod
#endif
