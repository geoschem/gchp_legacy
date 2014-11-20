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
  USE Mapping_Mod, ONLY : MapWeight

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
  TYPE(MapWeight),      POINTER :: mapping(:,:) => NULL()

  ! For chemistry
  INTEGER, POINTER              :: JLOP_PREV_loc(:,:,:)

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
                              tsDyn,     lonCtr,    latCtr,    Input_Opt, &
                              State_Chm, State_Met, RC      )
!
! !USES:
!
    USE ESMF,                    ONLY : ESMF_KIND_R4
    USE GIGC_Initialization_Mod, ONLY : GIGC_Init_Simulation
    USE GIGC_ErrCode_Mod
    USE GIGC_Input_Opt_Mod,      ONLY : OptInput
    USE GIGC_State_Chm_Mod,      ONLY : ChmState
    USE GIGC_State_Met_Mod,      ONLY : MetState

    ! HEMCO update
    USE HCO_ERROR_MOD
    USE HCOI_GC_MAIN_MOD,        ONLY : HCOI_GC_INIT
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
    INTEGER,            INTENT(IN)    :: nymdB       ! YYYYMMDD @ start of run
    INTEGER,            INTENT(IN)    :: nhmsB       ! hhmmss   @ start of run
    INTEGER,            INTENT(IN)    :: nymdE       ! YYYYMMDD @ end of run
    INTEGER,            INTENT(IN)    :: nhmsE       ! hhmmss   @ end of run
    REAL,               INTENT(IN)    :: tsChem      ! Chemistry timestep
    REAL,               INTENT(IN)    :: tsDyn       ! Chemistry timestep
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
                               tsChem         = tsChem,     & ! Chem step [min]
                               tsDyn          = tsDyn,      & ! Dyn  step [min]
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
                               mapping        = mapping,    & ! Olson map wts
                               RC             = RC         )  ! Success?
    IF ( RC /= GIGC_SUCCESS ) RETURN

    !=======================================================================
    ! Initialize HEMCO
    !=======================================================================
    CALL HCOI_GC_Init ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       RC = GIGC_SUCCESS+1
    ELSE
       RC = GIGC_SUCCESS
    ENDIF

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
  SUBROUTINE GIGC_Chunk_Run( am_I_Root, IM,        JM,        LM,         &
                             nymd,      nhms,      year,      month,      &
                             day,       dayOfYr,   hour,      minute,     &
                             second,    utc,       hElapsed,  Input_Opt,  &
                             State_Chm, State_Met, Phase,     RC           )
!
! !USES:
!
!    USE COMODE_MOD,         ONLY : CSPEC_FULL
    USE COMODE_LOOP_MOD
    USE comode_mod
    USE Chemistry_Mod,      ONLY : Do_Chemistry
    USE Dao_Mod,            ONLY : Convert_Units
    USE DryDep_Mod,         ONLY : Do_DryDep
    USE GC_Land_Interface
    USE GIGC_ErrCode_Mod
    USE GIGC_Input_Opt_Mod, ONLY : OptInput
    USE GIGC_State_Chm_Mod, ONLY : ChmState
    USE GIGC_State_Met_Mod, ONLY : MetState
    USE GRID_MOD,           ONLY : AREA_M2
    USE PBL_MIX_MOD,        ONLY : DO_PBL_MIX, COMPUTE_PBL_HEIGHT
    USE VDIFF_MOD,          ONLY : DO_PBL_MIX_2
    USE Pressure_Mod,       ONLY : Accept_External_Pedge
    USE Time_Mod,           ONLY : Accept_External_Date_Time
    USE Time_Mod,           ONLY : ITS_TIME_FOR_CHEM
    USE TRACERID_MOD
    USE WETSCAV_MOD,        ONLY : INIT_WETSCAV, DO_WETDEP
    USE DRYDEP_MOD,         ONLY : DEPSAV, NUMDEP, NTRAIND
    USE CONVECTION_MOD,     ONLY : DO_CONVECTION
    USE TOMS_MOD,           ONLY : COMPUTE_OVERHEAD_O3

    ! HEMCO update
    USE HCO_ERROR_MOD
    USE HCO_STATE_MOD,      ONLY : HCO_STATE
    USE HCOI_GC_MAIN_MOD,   ONLY : HCOI_GC_RUN, GetHcoState
    USE HCOI_GC_MAIN_MOD,   ONLY : GetHcoVal,   GetHcoID
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on root CPU?
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
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(HCO_STATE), POINTER       :: HcoState => NULL() 
    REAL*8                         :: DT
    REAL*8                         :: FLX, DEP
    REAL*8                         :: Emis8, Dep8
    INTEGER                        :: I, J, L, T 
    INTEGER                        :: N1, N2
    INTEGER                        :: ERROR
    INTEGER                        :: HcoID
    CHARACTER(LEN=ESMF_MAXSTR)     :: Iam
    LOGICAL                        :: FND

    ! Local logicals from input_opt
    LOGICAL                        :: LCONV, LDRYD, LEMIS
    LOGICAL                        :: LWETD, LCHEM, LTURB

    ! Are tracers in mass or mixing ratio?
    LOGICAL                        :: isMass

    ! Kludge to skip first phase one:
    LOGICAL, SAVE                  :: FIRST = .TRUE.

    !=======================================================================
    ! GIGC_CHUNK_RUN begins here 
    !=======================================================================

    ! Error trap
    Iam = 'GIGC_CHUNK_RUN (gigc_chunk_mod.F90)'

    ! Assume success
    RC = GIGC_SUCCESS

    ! Kludge: Don't do (phase one) on first call because a number of 
    ! variables are empty / invalid. Eventually need to bootstrap those
    ! values...
    IF ( FIRST ) THEN
       FIRST = .FALSE.
       RETURN
    ENDIF

    !=======================================================================
    ! Define processes to be covered in this phase
    !
    ! In the standard GEOS-Chem, the following operator sequence is used:
    ! FULL mixing               Non-local PBL mixing
    ! 1. Turbulence (v/v)       1. DryDep (kg)
    ! 2. Convection (v/v)       2. Emissions (kg)
    ! 3. DryDep (kg)            3. Turbulence (v/v)
    ! 4. Emissions (kg)         4. Convection (v/v)
    ! 5. Chemistry (kg)         5. Chemistry (kg)
    ! 6. WetDep (kg)            6. Wetdep (kg)
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
    !
    ! Here, we use the following operator sequence:
    ! 
    ! 1. Convection (v/v) --> Phase 1
    ! 2. DryDep (kg)      --> Phase 1
    ! 3. Emissions (kg)   --> Phase 1
    ! -------------------------------
    ! 4. Turbulence (v/v) --> Phase 2 
    ! 5. Chemistry (kg)   --> Phase 2
    ! 6. WetDep (kg)      --> Phase 2     
    ! 
    ! Any of the listed processes is only executed if the corresponding switch
    ! in the input.geos file is enabled. If the physics component already
    ! covers convection or turbulence, they should not be applied here!
    !
    ! (ckeller, 10/14/14)
    !=======================================================================

    ! Mirror logical switches
    LCONV  = Input_Opt%LCONV
    LTURB  = Input_Opt%LTURB
    LDRYD  = Input_Opt%LDRYD
    LEMIS  = Input_Opt%LEMIS
    LCHEM  = Input_Opt%LCHEM
    LWETD  = Input_Opt%LWETD

    ! Define processes to be covered
    IF ( Phase == 1 ) THEN
       Input_Opt%LCONV  = LCONV 
       Input_Opt%LDRYD  = LDRYD 
       Input_Opt%LEMIS  = LEMIS
       Input_Opt%LTURB  = .FALSE.
       Input_Opt%LCHEM  = .FALSE.
       Input_Opt%LWETD  = .FALSE.

    ELSEIF ( Phase == 2 ) THEN
       Input_Opt%LCONV  = .FALSE.
       Input_Opt%LDRYD  = .FALSE. 
       Input_Opt%LEMIS  = .FALSE.
       Input_Opt%LTURB  = LTURB
       Input_Opt%LCHEM  = LCHEM
       Input_Opt%LWETD  = LWETD
    ENDIF

    !-------------------------------------------------------------------------
    ! Pre-Run assignments
    !-------------------------------------------------------------------------

    ! Eventually initialize/reset wetdep
    IF ( Input_Opt%LCONV .OR. Input_Opt%LWETD ) THEN
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

    ! If it is not a multiple of the chemistry timestep, return
    !IF ( .not. ITS_TIME_FOR_CHEM() ) RETURN
    
    ! Set the pressure at level edges [hPa] from the ESMF environment
    CALL Accept_External_Pedge    ( am_I_Root      = am_I_Root,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

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

    ! Data enters in v/v
    isMass = .FALSE.

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
    IF ( Input_Opt%LCONV ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do convection now'
  
       CALL DO_CONVECTION ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
 
       ! testing only
       if(am_I_Root) write(*,*) ' --- Convection done!'
    ENDIF   

    !---------------------------------
    ! Unit conversion [v/v] --> [kg]
    !---------------------------------
    IF ( Input_Opt%LDRYD .OR. Input_Opt%LEMIS ) THEN
       CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                             N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                             TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                             AD        = State_Met%AD,         & ! Air mass [kg]
                             STT       = State_Chm%Tracers    )  ! Tracer array
       IsMass = .TRUE.
    ENDIF

    !=======================================================================
    ! 2. Dry deposition.
    !
    ! This calculates the deposition rates in [s-1].
    !=======================================================================
    IF ( Input_Opt%LDRYD ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do drydep now'

       ! Update & Remap Land-type arrays from Surface Grid-component
       CALL GEOS5_TO_OLSON_LANDTYPE_REMAP( State_Met, RC )    
    
       ! Execute Dry deposition
       CALL Do_DryDep   ( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?

       ! testing only
       if(am_I_Root) write(*,*) ' --- Drydep done!'

    ENDIF ! Do drydep

    !=======================================================================
    ! 3. Emissions (HEMCO).
    !=======================================================================
    IF ( Input_Opt%LEMIS ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do emissions now'

       CALL HCOI_GC_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, ERROR )
       ASSERT_(ERROR==HCO_SUCCESS)

       ! testing only
       if(am_I_Root) write(*,*) ' --- Emissions done!'
    ENDIF

    !=======================================================================
    ! If physics covers turbulence, simply add the emisison and dry 
    ! deposition fluxes calculated above to the tracer array, without caring
    ! about the vertical distribution.
    ! Set the emission and deposition values to zero to make sure that they
    ! are not used again in chemistry (even if the non-local PBL scheme is 
    ! used, emissions above the PBL are still added to the chemical solver).
    !=======================================================================
    IF ( .NOT. Input_Opt%LTURB .AND. (Input_Opt%LEMIS .OR. Input_Opt%LDRYD) ) THEN
  
       ! testing only
       if(am_I_Root) write(*,*) ' --- Add emissions and drydep to tracers'
 
       ! Get pointer to HEMCO state
       CALL GetHcoState ( HcoState )
       ASSERT_( ASSOCIATED(HcoState) )

       ! Emission time step in seconds
       DT = HcoState%TS_EMIS
 
       ! Loop over all grid boxes 
       DO L = 1, HcoState%NZ
       DO J = 1, HcoState%NY
       DO I = 1, HcoState%NX

          ! Do for all species
          DO T = 1, Input_Opt%N_TRACERS
 
             ! Reset
             FLX = 0.0d0
             DEP = 0.0d0
 
             ! testing only
             IF ( am_I_Root .and. i==1 .and. j==1 .and. l==1 ) THEN
                ! Get HEMCO ID corresponding to this GC tracer
                HcoID = GetHcoID( TrcID = T )
                IF ( HcoID > 0 ) THEN
                IF ( ASSOCIATED(HcoState%Spc(HcoID)%Emis%Val) ) THEN
                   write(*,*) '     Emission range for species: ', TRIM(Input_Opt%TRACER_NAME(T)), &
               MINVAL(HcoState%Spc(HcoID)%Emis%Val), MAXVAL(HcoState%Spc(HcoID)%Emis%Val)
                ENDIF
                ENDIF
             ENDIF
     
             ! Get emissions from HEMCO (kg/m2/s)
             CALL GetHcoVal( T, I, J, L, FND, Emis8=Emis8 )
             IF ( FND ) THEN
                ! kg/m2/s -> kg
                FLX = FLX + ( Emis8 * HcoState%Grid%AREA_M2%Val(I,J) * DT )
             ENDIF      
 
             ! Deposition (surface layer only)
             IF ( L == 1 ) THEN

                ! Get deposition rate from HEMCO (1/s)
                CALL GetHcoVal( T, I, J, L, FND, Dep8 = Dep8 )
                IF ( FND ) DEP = DEP + Dep8

                ! Also add deposition from drydep_mod.F [1/s].
                ! Need to find drydep index N2 for current GC tracer.
                N2 = 0
                DO N1 = 1, NUMDEP
                   IF ( NTRAIND(N1) == T ) THEN
                      N2 = N1
                      EXIT
                   ENDIF
                ENDDO
                IF ( N2 > 0 ) THEN 
                   DEP = DEP + DEPSAV(I,J,N2)
                ENDIF

                ! Deposition in kg (1/s --> kg)
                DEP = DEP * State_Chm%Tracers(I,J,L,T) * DT
             ENDIF

             ! Add to box. Make sure concentration is not negative.
             State_Chm%Tracers(I,J,L,T) = State_Chm%Tracers(I,J,L,T) + FLX - DEP
             IF ( State_Chm%Tracers(I,J,L,T) < 0.0d0 ) State_Chm%Tracers(I,J,L,T) = 0.0d0

          ENDDO !T
       ENDDO !I
       ENDDO !J
       ENDDO !L

       ! Reset all arrays
       DepSav              = 0.0d0
  
       ! Clean up
       HcoState => NULL()

       ! testing only
       if(am_I_Root) write(*,*) ' --- Fluxes applied to tracers!' 
 
    ENDIF ! Turbulence

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
    !=======================================================================
    IF ( Input_Opt%LTURB ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do turbulence now'

       ! Make sure tracers are in v/v
       IF ( isMass ) THEN 
          CALL Convert_Units  ( IFLAG     = 1,                    & ! [kg] --> [v/v]
                                N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                                TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                                AD        = State_Met%AD,         & ! Air mass [kg]
                                STT       = State_Chm%Tracers    )  ! Tracer array
          IsMass = .FALSE.
       ENDIF

       IF ( Input_Opt%LNLPBL ) THEN
          ! testing only
          if(am_I_Root) write(*,*) '     --> Use non-local PBL scheme'
          CALL DO_PBL_MIX_2( am_I_Root, Input_Opt%LTURB, Input_Opt, &
                             State_Met, State_Chm,       RC          )
       ELSE
          ! testing only
          if(am_I_Root) write(*,*) '     --> Use full mixing scheme'
          CALL DO_PBL_MIX( Input_Opt%LTURB, Input_Opt, State_Met, State_Chm )
       ENDIF

       ! testing only
       if(am_I_Root) write(*,*) ' --- Turbulence done!'

    ENDIF

    ! Chemistry and Wetdep need tracers in mass
    IF ( .NOT. IsMass .AND. ( Input_Opt%LCHEM .OR. Input_Opt%LWETD ) ) THEN
       CALL Convert_Units  ( IFLAG     = 2,                    & ! [v/v] -> [kg]
                             N_TRACERS = Input_Opt%N_TRACERS,  & ! # of tracers
                             TCVV      = Input_Opt%TCVV,       & ! Molec / kg
                             AD        = State_Met%AD,         & ! Air mass [kg]
                             STT       = State_Chm%Tracers    )  ! Tracer array
       IsMass = .TRUE.
    ENDIF

    !=======================================================================
    ! 5. Chemistry
    !=======================================================================
    IF ( Input_Opt%LCHEM ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do chemistry now'

       IF (.NOT. ASSOCIATED(JLOP_PREV_loc)) THEN
          ALLOCATE(JLOP_PREV_loc(ILONG,ILAT,IPVERT),STAT=RC)
          ASSERT_(RC==0)
          JLOP_PREV_loc = 0
       ENDIF
   
       !vartrop fix (dkh, 05/08/11)
       IF (ALLOCATED( JLOP          ) ) DEALLOCATE( JLOP          )
       IF (ALLOCATED( JLOP_PREVIOUS ) ) DEALLOCATE( JLOP_PREVIOUS )
       ALLOCATE( JLOP( ILONG, ILAT, IPVERT ), STAT=RC )
       ASSERT_(RC==0)
       ALLOCATE( JLOP_PREVIOUS( ILONG, ILAT, IPVERT ), STAT=RC )
       ASSERT_(RC==0)
   
       JLOP          = JLOP_PREV_loc
       JLOP_PREVIOUS = JLOP_PREV_loc
       AREA_M2 = State_Met%AREA_M2

       ! Zero Rate arrays  
       RRATE = 0.E0
       TRATE = 0.E0

       ! Calculate TOMS O3 overhead. For now, always use it from the
       ! Met field. State_Met%TO3 is imported from PCHEM.
       ! (ckeller, 10/21/2014).
       CALL COMPUTE_OVERHEAD_O3( DAY, .TRUE., State_Met%TO3 )

       ! Do chemistry
       CALL Do_Chemistry( am_I_Root = am_I_Root,            & ! Root CPU?
                          Input_Opt = Input_Opt,            & ! Input Options
                          State_Chm = State_Chm,            & ! Chemistry State
                          State_Met = State_Met,            & ! Met State
                          RC        = RC                   )  ! Success?

       ! Store in internal variables
       JLOP_PREV_loc = JLOP

       ! testing only
       if(am_I_Root) write(*,*) ' --- Chemistry done!'
    ENDIF

    !=======================================================================
    ! 6. Wet deposition
    !=======================================================================
    IF ( Input_Opt%LWETD ) THEN

       ! testing only
       if(am_I_Root) write(*,*) ' --- Do wetdep now'

       ! Do wet deposition
       CALL DO_WETDEP( am_I_Root, Input_Opt, State_Met, State_Chm, RC )

       ! testing only
       if(am_I_Root) write(*,*) ' --- Wetdep done!'
    ENDIF

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

    ! Reset switches to orig. values
    Input_Opt%LCONV  = LCONV
    Input_Opt%LTURB  = LTURB
    Input_Opt%LDRYD  = LDRYD
    Input_Opt%LEMIS  = LEMIS
    Input_Opt%LCHEM  = LCHEM
    Input_Opt%LWETD  = LWETD

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

    ! Finalize HEMCO
    CALL HCOI_GC_FINAL( am_I_Root )

    ! Finalize GEOS-Chem
    CALL GIGC_Finalize( am_I_Root = am_I_Root,  &  ! Are we on the root CPU?
                        Input_Opt = Input_Opt,  &  ! Input Options
                        State_Chm = State_Chm,  &  ! Chemistry State
                        State_Met = State_Met,  &  ! Meteorology State
                        RC        = RC         )   ! Success or failure?

    ! Deallocate module arrays
    IF ( Associated(JLOP_PREV_loc) ) DEALLOCATE(JLOP_PREV_loc) 

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
