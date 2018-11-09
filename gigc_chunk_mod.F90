#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_chunk_mod
!
! !DESCRIPTION: Module GC\_CHUNK\_MOD is the module that contains the init,
!  run, and finalize methods for the ESMF interface to GEOS-Chem.
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
  USE ErrCode_Mod

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
  PRIVATE :: SET_OZONOPAUSE   ! GEOS-5 only
!  PRIVATE :: StateDiag2Export ! GEOS-5 only
!
! !REVISION HISTORY:
!  22 Jun 2009 - R. Yantosca & P. Le Sager - Chunkized & cleaned up.
!  09 Oct 2012 - R. Yantosca - Now pass am_I_Root to all routines
!  09 Oct 2012 - R. Yantosca - Added comments, cosmetic changes
!  16 Oct 2012 - R. Yantosca - Renamed GC_STATE argument to State_Chm
!  22 Oct 2012 - R. Yantosca - Renamed to gigc_chunk_mod.F90
!  01 Nov 2012 - R. Yantosca - Now pass Input Options object to routines
!  15 Mar 2013 - R. Yantosca - Add routine GIGC_Cap_Tropopause_Prs
!  08 Mar 2018 - E. Lundgren - Move gigc_initialization_mod contents to 
!                              gigc_chunk_init now that LOC is much reduced
!EOP
!------------------------------------------------------------------------------
!BOC

! GEOS-5 only:
!
! !PRIVATE TYPES:
!
!  ! Derived type for chunk diagnostic output (for code validation)
!  TYPE GC_DIAG
!     LOGICAL                    :: DO_PRINT     ! Should we print out?
!     INTEGER                    :: N_DIAG       ! # of diag quantities
!     INTEGER                    :: COUNT        ! Counter for averaging
!     CHARACTER(LEN=10), POINTER :: NAME(:)      ! Tracer names
!     REAL*8,            POINTER :: TRACER(:,:)  ! Tracer concentrations
!     CHARACTER(LEN=40)          :: FILENAME     ! File name for output
!     INTEGER                    :: LUN          ! File unit # for output
!  END TYPE GC_DIAG
!
!  ! Derived type object for saving concentration diagnostics
!  TYPE(GC_DIAG)                 :: DIAG_COL
!---

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
!  GEOS-Chem.  This routine calls routines within core GEOS-Chem to allocate 
!  arrays and read input files.
!\\
!\\
! !INTERFACE:
!
! GEOS-5 only:
  SUBROUTINE GIGC_Chunk_Init( am_I_Root,  value_I_LO,      value_J_LO,      &
                              value_I_HI, value_J_HI,      value_IM,        &
                              value_JM,   value_LM,        value_IM_WORLD,  &
                              value_JM_WORLD,              value_LM_WORLD,  &
                              value_LLSTRAT,                                &
                              nymdB,      nhmsB,           nymdE,           &
                              nhmsE,      tsChem,          tsDyn,           &
                              lonCtr,     latCtr,          myPET,           &
                                                           Input_Opt,       &
                              State_Chm,  State_Diag,      State_Met,       &
                              HcoConfig,  HistoryConfig,   RC )
! GCHP instead:
!  SUBROUTINE GIGC_Chunk_Init( am_I_Root,  value_I_LO,    value_J_LO,     & 
!                              value_I_HI, value_J_HI,    value_IM,       &
!                              value_JM,   value_LM,      value_IM_WORLD, &
!                              value_JM_WORLD,            value_LM_WORLD, &
!                              nymdB,      nhmsB,         nymdE,          &
!                              nhmsE,      tsChem,        tsDyn,          &
!                              lonCtr,     latCtr,        myPET,          &
!                              GC,         EXPORT,        Input_Opt,      &
!                              State_Chm,  State_Diag,    State_Met,      &
!                              HcoConfig,  HistoryConfig, RC      )
!---
!
! !USES:
!
    USE Chemistry_Mod,           ONLY : Init_Chemistry
    USE CMN_Size_Mod,            ONLY : IIPAR, JJPAR, LLPAR, dLon, dLat
    USE DiagList_Mod
    USE Emissions_Mod,           ONLY : Emissions_Init
    USE ESMF,                    ONLY : ESMF_KIND_R4
    USE Fast_JX_Mod,             ONLY : Init_FJX
    USE GC_Environment_Mod
    USE GC_Grid_Mod,             ONLY : SetGridFromCtr
    USE GC_Grid_Mod,             ONLY : Set_xOffSet, Set_yOffSet ! GEOS-5 only
    USE GIGC_HistoryExports_Mod, ONLY : HistoryConfigObj
    USE HCO_Types_Mod,           ONLY : ConfigObj
    USE Input_Mod,               ONLY : Read_Input_File
    USE Input_Opt_Mod,           ONLY : OptInput, Set_Input_Opt
    USE Linoz_Mod,               ONLY : Linoz_Read
    USE PBL_Mix_Mod,             ONLY : Init_PBL_Mix
    USE PhysConstants,           ONLY : PI_180
    USE Pressure_Mod,            ONLY : Init_Pressure
    USE Roundoff_Mod,            ONLY : RoundOff
    USE State_Chm_Mod,           ONLY : ChmState
    USE State_Diag_Mod,          ONLY : DgnState
    USE State_Met_Mod,           ONLY : MetState
    USE Strat_Chem_Mod,          ONLY : Init_Strat_Chem          ! GEOS-5 only
    USE Tendencies_Mod,          ONLY : TEND_INIT                ! GEOS-5 only
    USE Time_Mod,                ONLY : Set_Timesteps
    USE UCX_MOD,                 ONLY : INIT_UCX
    USE UnitConv_Mod,            ONLY : Convert_Spc_Units

!
! !INPUT PARAMETERS:
!
    LOGICAL,            INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
    INTEGER,            INTENT(IN)    :: value_I_LO    ! Min lon index, this CPU
    INTEGER,            INTENT(IN)    :: value_J_LO    ! Min lat index, this CPU
    INTEGER,            INTENT(IN)    :: value_I_HI    ! Max lon index, this CPU
    INTEGER,            INTENT(IN)    :: value_J_HI    ! Max lat index, this CPU
    INTEGER,            INTENT(IN)    :: value_IM      ! # lons, this CPU
    INTEGER,            INTENT(IN)    :: value_JM      ! # lats, this CPU
    INTEGER,            INTENT(IN)    :: value_LM      ! # levs, this CPU
    INTEGER,            INTENT(IN)    :: value_IM_WORLD! # lons, global grid
    INTEGER,            INTENT(IN)    :: value_JM_WORLD! # lats, global grid
    INTEGER,            INTENT(IN)    :: value_LM_WORLD! # levs, global grid
    INTEGER,            INTENT(IN)    :: value_LLSTRAT ! # strat. levs
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
! GCHP only:
!    TYPE(ESMF_State), INTENT(INOUT), TARGET :: EXPORT ! Export state object
!    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
!---
    TYPE(OptInput),     INTENT(INOUT) :: Input_Opt   ! Input Options object
    TYPE(ChmState),     INTENT(INOUT) :: State_Chm   ! Chemistry State object 
    TYPE(DgnState),     INTENT(INOUT) :: State_Diag  ! Diagnostics State object
    TYPE(MetState),     INTENT(INOUT) :: State_Met   ! Meteorology State object
    TYPE(ConfigObj),    POINTER       :: HcoConfig   ! HEMCO config obj 
    TYPE(HistoryConfigObj), POINTER   :: HistoryConfig ! History config obj 
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
!  06 Mar 2018 - E. Lundgren - Remove Set_Initial_MixRatios
!  08 Mar 2018 - E. Lundgren - Move gigc_initialized_mod code here and move
!                              dlat/dlon calculation to gc_init_grid;
!                              GC timesteps are now seconds;
!                              Call set_input_opt to initialize input_opt vars;
!                              Add error handling using MAPL Assert_;
!                              Rename Initialize_Geos_Grid to GC_Init_Grid;
!                              Now call GC_Allocate_All after input.geos read;
!                              Restructure grid init based on gcbe v11-02e;
!                              Remove all unused code and simplify comments
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: I, J, L, STATUS
    CHARACTER(LEN=ESMF_MAXSTR)     :: Iam

    !=======================================================================
    ! GIGC_CHUNK_INIT begins here 
    !=======================================================================

    ! Error trap
    Iam = 'GIGC_CHUNK_INIT (gigc_chunk_mod.F90)'

    ! Assume success
    RC = GC_SUCCESS

! GEOS-5 only:
    ! ckeller, 01/16/17
    Input_Opt%MAX_DIAG      = 1 
    Input_Opt%MAX_FAM       = 250
    Input_Opt%LINOZ_NLAT    = 18
    Input_Opt%LINOZ_NMONTHS = 12
    Input_Opt%LINOZ_NFIELDS = 7
    Input_Opt%RootCPU       = am_I_Root
!-----

    ! Initialize Input_Opt fields to zeros or equivalent
    CALL Set_Input_Opt( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

! GCHP has this code here, but it needs to go after reading input.geos
! for GEOS-5 since the timesteps in input.geos are not used:
!    ! Update Input_Opt with timing fields
!    Input_Opt%NYMDb   = nymdB
!    Input_Opt%NHMSb   = nhmsB
!    Input_Opt%NYMDe   = nymdE
!    Input_Opt%NHMSe   = nhmsE
!    Input_Opt%TS_CHEM = INT( tsChem )   ! Chemistry timestep [sec]
!    Input_Opt%TS_EMIS = INT( tsChem )   ! Chemistry timestep [sec]
!    Input_Opt%TS_DYN  = INT( tsDyn  )   ! Dynamic   timestep [sec]
!    Input_Opt%TS_CONV = INT( tsDyn  )   ! Dynamic   timestep [sec]
!    Input_Opt%myCPU   = myPET
!---

! GEOS-5 vs GCHP NOTE: In GEOS-5 input.geos is read by every thread so 
! values are not broadcasted. Do this in a future version of GCHP since 
! little overhead to read file. Beware init order changes in v11-02f.

    ! Read GEOS-Chem input file at very beginning of simulation
    CALL Read_Input_File( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

! GCHP only (commented out in GEOS-5):
!    ! In the ESMF/MPI environment, we can get the total overhead ozone
!    ! either from the met fields (GIGCsa) or from the Import State (GEOS-5)
!    Input_Opt%USE_O3_FROM_MET = .TRUE.
!
!    ! Read LINOZ climatology
!    IF ( Input_Opt%LLINOZ ) THEN
!       CALL Linoz_Read( am_I_Root, Input_Opt, RC ) 
!       ASSERT_(RC==GC_SUCCESS)
!    ENDIF
!---

    ! Allocate all lat/lon arrays including CMN_Size_Mod parameters
    CALL GC_Allocate_All( am_I_Root      = am_I_Root,                     &
                          Input_Opt      = Input_Opt,                     &
                          value_I_LO     = value_I_LO,                    &
                          value_J_LO     = value_J_LO,                    &
                          value_I_HI     = value_I_HI,                    &
                          value_J_HI     = value_J_HI,                    &
                          value_IM       = value_IM,                      &
                          value_JM       = value_JM,                      &
                          value_LM       = value_LM,                      &
                          value_IM_WORLD = value_IM_WORLD,                &
                          value_JM_WORLD = value_JM_WORLD,                &
                          value_LM_WORLD = value_LM_WORLD,                &
                          value_LLSTRAT  = value_LLSTRAT,                 &
                          RC             = RC              )            
    ASSERT_(RC==GC_SUCCESS)

! GCHP only (commented out in GEOS-5):
!    ! ewl TODO: Is this calculation of dlon/dlat needed for GCHP or GEOS-5?
!    !========================================================================
!    ! Compute the DLON and DLAT values.  NOTE, this is a kludge since to do
!    ! this truly rigorously, we should take the differences between the grid
!    ! box edges.  But because I can't seem to find a way to get the grid
!    ! box edge information, the next best thing is to take the differences
!    ! between the grid box centers.  They should all be the same given that
!    ! the GEOS-Chem grid is regular. (bmy, 12/7/12)
!    !========================================================================
!    DO L = 1, LLPAR
!    DO J = 1, JJPAR
!    DO I = 1, IIPAR
!    
!       ! Compute Delta-Longitudes [degrees]
!       IF ( I == IIPAR ) THEN
!          dLon(I,J,L) = RoundOff( ( DBLE( lonCtr(IIPAR,  J) ) / PI_180 ), 4 ) &
!                      - RoundOff( ( DBLE( lonCtr(IIPAR-1,J) ) / PI_180 ), 4 )
!       ELSE
!          dLon(I,J,L) = RoundOff( ( DBLE( lonCtr(I+1,    J) ) / PI_180 ), 4 ) &
!                      - RoundOff( ( DBLE( lonCtr(I,      J) ) / PI_180 ), 4 )
!       ENDIF
!    
!       ! Compute Delta-Latitudes [degrees]
!       IF ( J == JJPAR ) THEN
!          dLat(I,J,L) = RoundOff( ( DBLE( latCtr(I,JJPAR  ) ) / PI_180 ), 4 ) &
!                      - RoundOff( ( DBLE( latCtr(I,JJPAR-1) ) / PI_180 ), 4 )
!       ELSE
!          dLat(I,J,L) = RoundOff( ( DBLE( latCtr(I,J+1    ) ) / PI_180 ), 4 ) &
!                      - RoundOff( ( DBLE( latCtr(I,J      ) ) / PI_180 ), 4 )
!       ENDIF
!    
!    ENDDO
!    ENDDO
!    ENDDO
!----- 

    ! Initialize horizontal grid parameters
    CALL GC_Init_Grid( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Set grid based on passed mid-points
    CALL SetGridFromCtr( am_I_Root, value_IM,    value_JM, &
                         lonCtr,    latCtr,      RC      )
    ASSERT_(RC==GC_SUCCESS)

! GEOS-5: This code is in GCHP but at an earlier stage. However, it must
! be set right before setting timesteps so that input.geos values are
! not used:
    ! Update Input_Opt with timing fields
    Input_Opt%NYMDb   = nymdB
    Input_Opt%NHMSb   = nhmsB
    Input_Opt%NYMDe   = nymdE
    Input_Opt%NHMSe   = nhmsE
    Input_Opt%TS_CHEM = INT( tsChem )   ! Chemistry timestep [sec]
    Input_Opt%TS_EMIS = INT( tsChem )   ! Chemistry timestep [sec]
    Input_Opt%TS_DYN  = INT( tsDyn  )   ! Dynamic   timestep [sec]
    Input_Opt%TS_CONV = INT( tsDyn  )   ! Dynamic   timestep [sec]
    Input_Opt%myCPU   = myPET
!---

    ! Set GEOS-Chem timesteps on all CPUs
    CALL SET_TIMESTEPS( am_I_Root  = am_I_Root,                          &
                        Chemistry  = Input_Opt%TS_CHEM,                  &
                        Convection = Input_Opt%TS_CONV,                  &
                        Dynamics   = Input_Opt%TS_DYN,                   &
                        Emission   = Input_Opt%TS_EMIS,                  &
                        Radiation  = Input_Opt%TS_RAD,                   &
                        Unit_Conv  = MAX( Input_Opt%TS_DYN,              &
                                          Input_Opt%TS_CONV ),           &
                        Diagnos    = Input_Opt%TS_DIAG         )

    CALL GC_Init_StateObj( am_I_Root, HistoryConfig%DiagList, Input_Opt, &
                           State_Chm, State_Diag, State_Met, RC )
    ASSERT_(RC==GC_SUCCESS)

! GEOS-5 only (I deleted from GCHP. Needed for GEOS-5?):
    ! Make sure to reset I0 and J0 in grid_mod.F90 with
    ! the values carried in the Input Options object
    CALL Set_xOffSet( Input_Opt%NESTED_I0 )
    CALL Set_yOffSet( Input_Opt%NESTED_J0 )
!---

    ! Initialize other GEOS-Chem modules
    CALL GC_Init_Extra( am_I_Root, HistoryConfig%DiagList, Input_Opt,    &
                        State_Chm, State_Diag, RC ) 
    ASSERT_(RC==GC_SUCCESS)

! GCHP only:
!    ! Initialize photolysis
!    IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR.                     &
!         Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
!       CALL Init_FJX( am_I_Root, Input_Opt, State_Chm, State_Diag, RC ) 
!       ASSERT_(RC==GC_SUCCESS)
!    ENDIF
!---

! General note for both GCHP and GEOS-5. Setting the units here is somewhere
! dangerous. The species values MUST be these units for conversion routines
! to work. GEOS-5 units at this point are kg/kg total, but are later converted
! not using those routines. Setting kg/kg dry here is misleading.
    ! Set State_Chm units
# if defined( MODEL_GEOS )
    State_Chm%Spc_Units = 'kg/kg total'
#else
    State_Chm%Spc_Units = 'kg/kg dry'
#endif

    ! Initialize the GEOS-Chem pressure module (set Ap & Bp)
    CALL Init_Pressure( am_I_Root )

    ! Initialize the PBL mixing module
    CALL INIT_PBL_MIX( am_I_Root, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Initialize chemistry mechanism
    IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR. Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
       CALL INIT_CHEMISTRY ( am_I_Root, Input_Opt, State_Chm, State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)
    ENDIF

! GCHP only:
!    ! Allocate array of overhead O3 columns for TOMS if chemistry is on
!    IF ( Input_Opt%LCHEM ) THEN
!       CALL Init_TOMS( am_I_Root, Input_Opt, RC )
!       ASSERT_(RC==GC_SUCCESS)
!    ENDIF
!---

! GCHP vs GEOS-5 note: init_fjx is called within GCHP after init_chemistry.
! This should be removed since it is called within init_chemistry

    ! Initialize HEMCO
    CALL EMISSIONS_INIT ( am_I_Root, Input_Opt, State_Met, State_Chm, RC, &
                          HcoConfig=HcoConfig )
    ASSERT_(RC==GC_SUCCESS)

    ! Stratosphere - can't be initialized without HEMCO because of STATE_PSC
    IF ( Input_Opt%LUCX ) THEN

       ! Initialize stratospheric routines
       CALL INIT_UCX( am_I_Root, Input_Opt, State_Chm, State_Diag )

    ENDIF

! GEOS-5 only (this is done later in GCHP, within gigc_chunk_run,
! and does not have the LLSTRAT threshold applied (should it?):
    !IF ( Input_Opt%LSCHEM .AND. Input_Opt%LLSTRAT < value_LM ) THEN
     IF ( Input_Opt%LSCHEM ) THEN
       CALL INIT_STRAT_CHEM( am_I_Root, Input_Opt, State_Chm, & 
                             State_Met, RC )
       IF (RC /= GC_SUCCESS) RETURN
    ENDIF
!----

! GEOS-5 only (tend_init is not called in GCHP):
    !-------------------------------------------------------------------------
    ! Diagnostics and tendencies 
    !-------------------------------------------------------------------------

    ! The GEOS-Chem diagnostics list, stored in HistoryConfig, is initialized 
    ! during GIGC_INIT_SIMULATION, and corresponding arrays in State_Diag are 
    ! allocated accordingly when initializing State_Diag. Here, we thus 
    ! only need to initialize the tendencies, which have not been initialized
    ! yet (ckeller, 11/29/17). 
    CALL Tend_Init ( am_I_Root, Input_Opt, State_Met, State_Chm, RC ) 
    ASSERT_(RC==GC_SUCCESS)
!----

    ! ewl TODO: Is this block needed for GEOS-5? If not, remove.
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

! GCHP only (units remain kg/kg dry in GEOS-5?):
!    ! Convert species units to internal state units (v/v dry)
!    CALL Convert_Spc_Units( am_I_Root, Input_Opt, State_Met, &
!                            State_Chm, 'v/v dry', RC )
!    ASSERT_(RC==GC_SUCCESS)
!----

    ! Return success
    RC = GC_Success

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
! GEOS-5 vs GCHP note: GEOS-5 gets passed FirstRewind
  SUBROUTINE GIGC_Chunk_Run( am_I_Root,  GC,   IM,   JM,         LM,         &
                             nymd,       nhms,       year,       month,      &
                             day,        dayOfYr,    hour,       minute,     &
                             second,     utc,        hElapsed,   Input_Opt,  &
                             State_Chm,  State_Met,  State_Diag, Phase,      &
                             IsChemTime, FrstRewind, RC )
!
! !USES:
!
    ! GEOS-Chem state objects 
    USE HCO_Interface_Mod,  ONLY : HcoState
    USE Input_Opt_Mod,      ONLY : OptInput
    USE State_Chm_Mod,      ONLY : ChmState
    USE State_Diag_Mod
    USE State_Met_Mod,      ONLY : MetState

    ! GEOS-Chem components
    USE Chemistry_Mod,      ONLY : Do_Chemistry, Recompute_OD
    USE Convection_Mod,     ONLY : Do_Convection
    USE DryDep_Mod,         ONLY : Do_DryDep
    USE Emissions_Mod,      ONLY : Emissions_Run
    USE Mixing_Mod,         ONLY : Do_Tend, Do_Mixing
    USE Strat_Chem_Mod,     ONLY : Init_Strat_Chem, Minit_is_Set
    USE WetScav_Mod,        ONLY : Setup_WetScav, Do_WetDep

    ! Specialized subroutines
    USE Dao_Mod,            ONLY : AirQnt, Set_Dry_Surface_Pressure
    USE Dao_Mod,            ONLY : GIGC_Cap_Tropopause_Prs
    USE Set_Global_CH4_Mod, ONLY : Set_CH4
    USE MODIS_LAI_Mod,      ONLY : Compute_XLAI_GCHP
    USE PBL_Mix_Mod,        ONLY : Compute_PBL_Height
    USE Pressure_Mod,       ONLY : Set_Floating_Pressures
    USE TOMS_Mod,           ONLY : Compute_Overhead_O3
    USE UCX_Mod,            ONLY : Set_H2O_Trac

    ! Utilities
    USE ErrCode_Mod
    USE GC_Grid_Mod,        ONLY : AREA_M2
    USE HCO_Error_Mod
    USE HCO_Interface_Mod,  ONLY : SetHcoTime
    USE Pressure_Mod,       ONLY : Accept_External_Pedge
    USE State_Chm_Mod,      ONLY : IND_
    USE Time_Mod,           ONLY : Accept_External_Date_Time
#if defined( MODEL_GEOS )
    USE UnitConv_Mod,       ONLY : Convert_Spc_Units
#endif

    ! Diagnostics
    USE Diagnostics_Mod,    ONLY : Set_Diagnostics_EndofTimestep

! GEOS-5 only:
    USE CMN_SIZE_MOD,       ONLY : IIPAR, JJPAR, LLPAR
    USE PRECISION_MOD
    USE DAO_MOD,            ONLY : GET_COSINE_SZA
    USE DiagList_Mod,       ONLY : DgnList 
    USE DIAG_MOD,           ONLY : AD21
    USE HCOI_GC_MAIN_MOD,   ONLY : HCOI_GC_WriteDiagn
!---
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
    LOGICAL,        INTENT(IN)    :: IsChemTime  ! Time for chemistry? 
    LOGICAL,        INTENT(IN)    :: FrstRewind  ! Is it the first rewind? 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(OptInput),      INTENT(INOUT) :: Input_Opt   ! Input Options obj
    TYPE(ChmState),      INTENT(INOUT) :: State_Chm   ! Chemistry State obj
    TYPE(MetState),      INTENT(INOUT) :: State_Met   ! Meteorology State obj
    TYPE(DgnState),      INTENT(INOUT) :: State_Diag  ! Diagnostics State obj
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Return code
!
! !REMARKS:
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
!  13 Feb 2018 - E. Lundgren - Call Recompute_OD at end of chem dt for aer diags
!EOP
!------------------------------------------------------------------------------
!BOC
    TYPE(MAPL_MetaComp), POINTER   :: STATE
    REAL*8                         :: DT
    CHARACTER(LEN=ESMF_MAXSTR)     :: Iam
    INTEGER                        :: STATUS
#if defined( MODEL_GEOS )
    CHARACTER(LEN=ESMF_MAXSTR)     :: OrigUnit
    INTEGER                        :: N, I, J, L
#endif

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

    ! # of times this routine has been called. Only temporary for printing 
    ! processes on the first 10 calls.
    INTEGER, SAVE                  :: NCALLS = 0

    ! UV month
    INTEGER, SAVE                  :: UVmonth = -999

    ! Strat. H2O settings 
    LOGICAL                        :: SetStratH2O 
    LOGICAL, SAVE                  :: LSETH2O_orig
!----

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
    ! AREA pointer (imported from superdynamics) is only properly filled in run.
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

    ! By default, do processes as defined in input.geos. DoTend defined below. 
    DoConv   = Input_Opt%LCONV                    ! dynamic time step
    DoDryDep = Input_Opt%LDRYD .AND. IsChemTime   ! chemistry time step
    DoEmis   = Input_Opt%LEMIS .AND. IsChemTime   ! chemistry time step
! GEOS-5:
    DoTurb   = Input_Opt%LTURB .AND. IsChemTime   ! dynamic time step
! GCHP uses instead:
!    DoTurb   = Input_Opt%LTURB                    ! dynamic time step
!---
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
    CALL Compute_XLAI_GCHP( am_I_Root, State_Met, RC )

    ! Set the pressure at level edges [hPa] from the ESMF environment
    CALL Accept_External_Pedge    ( am_I_Root      = am_I_Root,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

! GEOS-5 only:
    ! Eventually set tropopause pressure according to ozone values
    ! (use ozonopause) 
    CALL SET_OZONOPAUSE ( am_I_Root, Input_Opt,  State_Met, &
                          State_Chm, IM, JM, LM, RC )
    IF ( RC /= GC_SUCCESS ) RETURN 
!---

    ! Set dry surface pressure (PS1_DRY) from State_Met%PS1_WET
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 1 )

    ! Set dry surface pressure (PS2_DRY) from State_Met%PS2_WET
    CALL SET_DRY_SURFACE_PRESSURE( State_Met, 2 )

    ! Initialize surface pressures prior to interpolation
    ! to allow initialization of floating pressures
    State_Met%PSC2_WET = State_Met%PS1_WET
    State_Met%PSC2_DRY = State_Met%PS1_DRY
    CALL SET_FLOATING_PRESSURES( am_I_Root, State_Met, RC )
    IF ( RC /= GC_SUCCESS ) RETURN

! GEOS-5:
    ! Define airmass and related quantities
    !CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, (.not.FIRST) )
    CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, .FALSE. )
    IF ( RC /= GC_SUCCESS ) RETURN
! GCHP uses instead:
!    ! Define airmass and related quantities, and determine whether to scale
!    ! species mixing ratios to account for mass conservation across 
!    ! pressure changes. Only scale mixing ratios if transport is OFF and
!    ! it is after the first timestep. If transport is ON then species
!    ! should be "insensitive" to changes in pressure. If it is the first 
!    ! timestep then pressure history is not available for the scaling.
!    pUpdate = ((.not.FIRST).and.(.not.Input_Opt%LTRAN))
!    CALL AirQnt( am_I_Root, Input_opt, State_Met, State_Chm, RC, pUpdate )
!---

    ! Save the initial tracer concentrations in the MINIT variable of
    ! GeosCore/strat_chem_mod.F90.  This has to be done here, after the
    ! very first call to AIRQNT, because we need State_Chm%AD to have been
    ! populated with non-zero values.  Otherwise the unit conversions will
    ! blow up and cause GCHP to crash. (bmy, 10/19/16)
! GEOS-5 only (not commented out in GCHP):
!    IF ( FIRST .and. Input_Opt%LSCHEM ) THEN
!       CALL INIT_STRAT_CHEM( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
!       Minit_is_set = .true.
!    ENDIF
!---

    ! Cap the polar tropopause pressures at 200 hPa, in order to avoid
    ! tropospheric chemistry from happening too high up (cf. J. Logan)
    CALL GIGC_Cap_Tropopause_Prs  ( am_I_Root      = am_I_Root,  &
                                    IM             = IM,         &
                                    JM             = JM,         &
                                    Input_Opt      = Input_Opt,  &
                                    State_Met      = State_Met,  &
                                    RC             = RC         )

    ! Call PBL quantities. Those are always needed
    CALL COMPUTE_PBL_HEIGHT( am_I_Root, State_Met, RC )

#if defined( MODEL_GEOS )
    !=======================================================================
    ! Species arrive in kg/kg total. Convert to kg/kg dry 
    !=======================================================================

    ! Adjust total mixing ratio to account for change in specific
    ! humidity and dry air mass since end of last timestep. Always skip
    ! first step and do not apply if transport is turned on. (ewl, 11/8/18)
    IF ( (.NOT. FIRST) .AND. ( .NOT. Input_Opt%LTRAN ) ) THEN
       DO N = 1, State_Chm%nSpecies
       DO L = 1, LLPAR
       DO J = 1, JJPAR
       DO I = 1, IIPAR
         State_Chm%Species(I,J,L,N) = State_Chm%Species(I,J,L,N)          &
                 / ( 1e0_fp - ( State_Met%SPHU_PREV(I,J,L) * 1e-3_fp ) )  &
                 * ( 1e0_fp - ( State_Met%SPHU(I,J,L) * 1e-3_fp ) )       &
                 * State_Met%DP_DRY_PREV(I,J,L) / State_Met%DELP_DRY(I,J,L)    
       ENDDO
       ENDDO
       ENDDO
       ENDDO
    ENDIF
    
    ! Convert total mixing ratio to dry mixing ratio
    CALL Convert_Spc_Units ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                            'kg/kg dry', RC )
#endif

    ! SDE 05/28/13: Set H2O to STT if relevant
! GEOS-5 only:
    ! Tropospheric H2O is always prescribed (using GEOS Q). For strat H2O
    ! there are three options, controlled by toggles 'set initial global MR'
    ! in input.geos and 'Prescribe_strat_H2O' in GEOSCHEMchem_GridComp.rc: 
    ! (A) never prescribe strat H2O --> both toggles off
    ! (B) prescribe strat H2O on initial time step --> toggle in input.goes on
    ! (C) always prescribe strat H2O --> toggle in GEOSCHEMchem_GridComp.rc on
    !=======================================================================
    IF ( FIRST ) LSETH2O_orig = Input_Opt%LSETH2O
    IF ( IND_('H2O') > 0 ) THEN
       IF ( FIRST .OR. FrstRewind ) Input_Opt%LSETH2O = LSETH2O_orig
       SetStratH2O = .FALSE.
       IF ( Input_Opt%LSETH2O      ) SetStratH2O = .TRUE.
       IF ( .NOT. Input_Opt%LUCX   ) SetStratH2O = .TRUE.
       IF ( Input_Opt%AlwaysSetH2O ) SetStratH2O = .TRUE.
       CALL SET_H2O_TRAC( am_I_Root, SetStratH2O, & 
                          Input_Opt, State_Met, State_Chm, RC )
      ! Only force strat once if using UCX
       IF (Input_Opt%LSETH2O) Input_Opt%LSETH2O = .FALSE.
    ENDIF
! GCHP is less complicated:
!    IF ( IND_('H2O','A') > 0 ) THEN
!       CALL SET_H2O_TRAC( am_I_Root, ((.NOT. Input_Opt%LUCX) .OR.    &
!                          Input_Opt%LSETH2O ), Input_Opt, State_Met, &
!                          State_Chm, RC )
!       ! Only force strat once if using UCX
!       IF (Input_Opt%LSETH2O) Input_Opt%LSETH2O = .FALSE.
!    ENDIF
!---

! GEOS-5 only:
    ! Compute the cosine of the solar zenith angle array
    ! State_Met%SUNCOS     = at the current time
    ! State_Met%SUNCOSmid  = at the midpt of the chem timestep
    ! State_Met%SUNCOSmid5 = at the midpt of the chem timestep 5hrs ago
    CALL GET_COSINE_SZA( am_I_Root, Input_Opt, State_Met, RC )
!---

    !=======================================================================
    ! EMISSIONS phase 1. Should be called every time to make sure that the
    ! HEMCO clock and the HEMCO data list are up to date.
    !=======================================================================
    CALL EMISSIONS_RUN( am_I_Root, Input_Opt,  State_Met, &
                        State_Chm, DoEmis,     1, RC       )
! GEOS-5 only (should add to GCHP):
    ASSERT_(RC==GC_SUCCESS)
!---

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
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do convection now'
       CALL MAPL_TimerOn( STATE, 'GC_CONV' )

       CALL DO_CONVECTION ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                            State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)
 
       CALL MAPL_TimerOff( STATE, 'GC_CONV' )
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Convection done!'
    ENDIF   

    !=======================================================================
    ! 2. Dry deposition
    !
    ! Calculates the deposition rates in [s-1].
    !=======================================================================
    IF ( DoDryDep ) THEN
       if(am_I_Root.and.NCALLS<10) THEN
          write(*,*) ' --- Do drydep now'
          write(*,*) '     Use FULL PBL: ', Input_Opt%PBL_DRYDEP
       endif
       CALL MAPL_TimerOn( STATE, 'GC_DRYDEP' )
    
       ! Do dry deposition
! GEOS-5:
       CALL Do_DryDep ( am_I_Root, Input_Opt, State_Met, &
                        State_Chm, State_Diag, RC ) 
! GCHP passes assignments instead:
!       CALL Do_DryDep( am_I_Root, Input_Opt=Input_Opt, State_Chm=State_Chm, &
!                       State_Met=State_Met, State_Diag=State_Diag, RC=RC )
!---
       ASSERT_(RC==GC_SUCCESS)

       CALL MAPL_TimerOff( STATE, 'GC_DRYDEP' )
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Drydep done!'
    ENDIF

    !=======================================================================
    ! 3. Emissions (HEMCO)
    !
    ! HEMCO must be called on first time step to make sure that the HEMCO
    ! data lists are all properly set up. 
    !=======================================================================
    IF ( DoEmis ) THEN
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do emissions now'
       CALL MAPL_TimerOn( STATE, 'GC_EMIS' )

! GEOS-5 only:
       ! Call HEMCO run interface - Phase 2 
       CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, &
                            State_Chm, IsChemTime, 2, RC )
! GCHP passes Phase instead:
!       CALL EMISSIONS_RUN ( am_I_Root, Input_Opt, State_Met, State_Chm, &
!                            DoEmis, Phase, RC )
!---
       ASSERT_(RC==GC_SUCCESS)

       CALL MAPL_TimerOff( STATE, 'GC_EMIS' )
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
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                           ' --- Add emissions and drydep to tracers'
       CALL MAPL_TimerOn( STATE, 'GC_FLUXES' )

! GEOS-5 only (???): 
       ! Make sure tracers are in v/v
       CALL Convert_Spc_Units ( am_I_Root, Input_Opt, State_Met, &
                                State_Chm, 'v/v dry', RC )
       IF ( RC /= GC_SUCCESS ) RETURN 
!---

       ! Get emission time step [s]. 
       ASSERT_(ASSOCIATED(HcoState))
       DT = HcoState%TS_EMIS 

! GEOS-5 only:
!       ! Reset diagnostics if needed
!       IF ( ASSOCIATED(State_Diag%DryDep) ) State_Diag%DryDep = 0.0
!---

       ! Apply tendencies over entire PBL. Use emission time step.
       CALL DO_TEND ( am_I_Root, Input_Opt, State_Met, State_Chm,  &
                      State_Diag, .FALSE., RC, DT=DT )
       ASSERT_(RC==GC_SUCCESS)

! GEOS-5 only (this unit conversion should not be necessary):
       ! Convert species conc back to [kg/kg dry air] after mixing (ewl, 8/12/15)
       CALL Convert_Spc_Units ( am_I_Root, Input_Opt, State_Met, &
                                State_Chm, 'kg/kg dry', RC )
       IF ( RC /= GC_SUCCESS ) RETURN 

       ! testing only
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                                 '     Tendency time step [s]: ', DT 
!---

       CALL MAPL_TimerOff( STATE, 'GC_FLUXES' )
       if(am_I_Root.and.NCALLS<10) write(*,*)   &
                                 ' --- Fluxes applied to tracers!' 
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
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do turbulence now'
       CALL MAPL_TimerOn( STATE, 'GC_TURB' )

       ! Do mixing and apply tendencies. This will use the dynamic time step,
       ! which is fine since this call will be executed on every time step. 
       CALL DO_MIXING ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                        State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)

       CALL MAPL_TimerOff( STATE, 'GC_TURB' )
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Turbulence done!'
    ENDIF

! GEOS-5 sets CH4 values only if phase is not 2, and routine expects state_diag:
    ! Set tropospheric CH4 concentrations and fill species array with
    ! current values. 
!    IF ( DoTurb .OR. DoTend ) THEN
!      IF ( Input_Opt%LCH4SBC ) THEN
          IF ( Phase /= 2 ) THEN
            CALL SET_CH4 ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                           State_Diag, RC )
            ASSERT_(RC==GC_SUCCESS)
          ENDIF
!      ENDIF
!    ENDIF
! GCHP also checks if a full chem simulation and CH4 is a species. Note that
! it does not yet accept State_Diag:
!    ! Set tropospheric CH4 concentrations and fill species array with
!    ! current values. 
!    IF ( Phase /= 2 .AND. Input_Opt%ITS_A_FULLCHEM_SIM  &
!         .AND. IND_('CH4','A') > 0 ) THEN
!       CALL SET_CH4 ( am_I_Root, Input_Opt, State_Met, State_Chm, RC )
!    ENDIF
!---

    !=======================================================================
    ! 5. Chemistry
    !=======================================================================
    IF ( DoChem ) THEN
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do chemistry now'
       CALL MAPL_TimerOn( STATE, 'GC_CHEM' )

       ! Calculate TOMS O3 overhead. For now, always use it from the
       ! Met field. State_Met%TO3 is imported from PCHEM (ckeller, 10/21/2014).
       CALL COMPUTE_OVERHEAD_O3( am_I_Root, DAY, .TRUE., State_Met%TO3 )

! GEOS-5 (commented out):
!       ! Set H2O to STT if relevant
!       IF ( IND_('H2O') > 0 ) THEN
!          CALL SET_H2O_TRAC( am_I_Root, .TRUE., Input_Opt, &
!                             State_Met, State_Chm, RC )
!       ENDIF
! GCHP does this instead:
!       ! Set H2O to species value if H2O is advected
!       IF ( IND_('H2O','A') > 0 ) THEN
!          CALL SET_H2O_TRAC( am_I_Root, (.not. Input_Opt%LUCX), Input_Opt, &
!                             State_Met, State_Chm, RC )
!       ENDIF
!---

! GEOS-5:
       ! Do chemistry
       CALL Do_Chemistry( am_I_Root, Input_Opt, State_Met, &
                          State_Chm, State_Diag, RC ) 

! GCHP passes assignments:
!       CALL Do_Chemistry( am_I_Root  = am_I_Root,            & ! Root CPU?
!                          Input_Opt  = Input_Opt,            & ! Input Options
!                          State_Chm  = State_Chm,            & ! Chemistry State
!                          State_Met  = State_Met,            & ! Met State
!                          State_Diag = State_Diag,           & ! Diagn State
!                          RC         = RC                   )  ! Success?
!
       ASSERT_(RC==GC_SUCCESS)

       CALL MAPL_TimerOff( STATE, 'GC_CHEM' )
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Chemistry done!'
    ENDIF

    !=======================================================================
    ! 6. Wet deposition
    !=======================================================================
    IF ( DoWetDep ) THEN
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Do wetdep now'
       CALL MAPL_TimerOn( STATE, 'GC_WETDEP' )

       ! Do wet deposition
       CALL DO_WETDEP( am_I_Root, Input_Opt, State_Met, State_Chm,  &
                       State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)

       CALL MAPL_TimerOff( STATE, 'GC_WETDEP' )
       if(am_I_Root.and.NCALLS<10) write(*,*) ' --- Wetdep done!'
    ENDIF

    !=======================================================================
    ! Diagnostics 
    !=======================================================================

! GEOS-5 only:
    !==============================================================
    !      ***** U P D A T E  O P T I C A L  D E P T H *****          
    !==============================================================
    ! Recalculate the optical depth at the wavelength(s) specified
    ! in the Radiation Menu. This must be done before the call to any
    ! diagnostic and only on a chemistry timestep.
    ! (skim, 02/05/11)
    ! RECOMPUTE_OD also contains a call to AEROSOL_CONC, which updates
    ! the PM25 diagnostics.
    !IF ( DoChem .AND. ND21 > 0 ) THEN
    !IF ( DoChem ) THEN
       ! Recompute_OD populates the AD21 diagnostics. Make sure to flush
       ! them first (ckeller, 8/9/17)
       IF ( Input_Opt%ND21 > 0 ) AD21(:,:,:,:) = 0.0
       CALL RECOMPUTE_OD ( am_I_Root, Input_Opt,  State_Met, &
                           State_Chm, State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)
    !ENDIF
! GCHP does not used AD21 and only recomputes OD if DoChem
!    IF ( DoChem ) THEN
!       ! Recalculate the optical depth at the wavelength(s) specified
!       ! in the Radiation Menu. This must be done before the call to any
!       ! diagnostic.
!       CALL Recompute_OD( am_I_Root, Input_Opt,  State_Met,  &
!                          State_Chm, State_Diag, RC         )
!       ASSERT_(RC==GC_SUCCESS)
!    ENDIF
!---

! GEOS-5 only:
    CALL MAPL_TimerOn( STATE, 'GC_DIAGN' )
!---

    ! Set certain diagnostics dependent on state at end of step. This
    ! includes species concentration and dry deposition flux.
    CALL Set_Diagnostics_EndofTimestep( am_I_Root, Input_Opt,  State_Met, &
                                        State_Chm, State_Diag, RC )
    ASSERT_(RC==GC_SUCCESS)

! GEOS-5 only (diagnostics):
    ! Write HEMCO diagnostics 
    CALL HCOI_GC_WriteDiagn( am_I_Root, Input_Opt, .FALSE., RC )
    ASSERT_(RC==GC_SUCCESS)

    CALL MAPL_TimerOff( STATE, 'GC_DIAGN' )
!---


#if defined( MODEL_GEOS )
    !=======================================================================
    ! Convert species back to kg/kg total
    !=======================================================================

    ! Convert dry mixing ratio to total mixing ratio
    CALL Convert_Spc_Units ( am_I_Root, Input_Opt, State_Met, State_Chm, &
                            'kg/kg total', RC )

    ! Save specific humidity and dry air mass for total mixing ratio 
    ! adjustment in next timestep, if needed (ewl, 11/8/18)
    State_Met%SPHU_PREV = State_Met%SPHU
#endif

    !=======================================================================
    ! Clean up
    !=======================================================================

    ! testing only
    IF ( PHASE /= 1 .AND. NCALLS < 10 ) NCALLS = NCALLS + 1 

    ! First call is done
    FIRST = .FALSE.

! GCHP only:
!    ! Convert units to units of the internal state
!    CALL Convert_Spc_Units( am_I_Root, Input_Opt, State_Met, State_Chm, &
!                            'v/v dry', RC )
!---

    ! Return success
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
! !DESCRIPTION: Subroutine GIGC\_CHUNK\_FINAL is the ESMF finalize method for
!  GEOS-Chem.  This routine deallocates pointers and arrays.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Chunk_Final( am_I_Root, Input_Opt, State_Chm, State_Met, &
                               State_Diag, RC )
!
! !USES:
!
    USE Input_Opt_Mod,         ONLY : OptInput!, Cleanup_Input_Opt
    USE State_Chm_Mod,         ONLY : ChmState, Cleanup_State_Chm
    USE State_Met_Mod,         ONLY : MetState, Cleanup_State_Met
    USE HCOI_GC_MAIN_MOD,      ONLY : HCOI_GC_FINAL
! GEOS-5 only:
    USE State_Diag_Mod,        ONLY : DgnState, Cleanup_State_Diag
    USE DiagList_Mod,          ONLY : DgnList, Cleanup_DiagList
    USE Diagnostics_Mod
!---
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
    TYPE(DgnState), INTENT(INOUT) :: State_Diag    ! Diagnostics State object
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
!  19 Sep 2017 - E. Lundgren - Move gigc_finalize content to within this routine
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Assume succes
    RC = GC_SUCCESS

    ! Finalize HEMCO
    CALL HCOI_GC_FINAL( am_I_Root, .FALSE., RC )
    IF ( am_I_Root ) THEN
       IF ( RC == GC_SUCCESS ) THEN
          write(*,'(a)') 'HEMCO::Finalize... OK.'
       ELSE
          write(*,'(a)') 'HEMCO::Finalize... FAILURE.'
       ENDIF
    ENDIF

    ! Deallocate fields of the Input Options object
! GEOS-5 only:
    ! The call to Cleanup_Input_Opt causes a memory leak error. Comment
    ! for now (ckeller, 11/29/16).
    ! CALL Cleanup_Input_Opt( am_I_Root, Input_Opt, RC )
    !IF ( am_I_Root ) THEN
    !   IF ( RC == GC_SUCCESS ) THEN
    !      write(*,'(a)') 'Chem::Input_Opt Finalize... OK.'
    !   ELSE
    !      write(*,'(a)') 'Chem::Input_Opt Finalize... FAILURE.'
    !   ENDIF
    !ENDIF
!---

    ! Deallocate fields of the Chemistry State object
    CALL Cleanup_State_Chm( am_I_Root, State_Chm, RC )
    IF ( am_I_Root ) THEN
       IF ( RC == GC_SUCCESS ) THEN
          write(*,'(a)') 'Chem::State_Chm Finalize... OK.'
       ELSE
          write(*,'(a)') 'Chem::State_Chm Finalize... FAILURE.'
       ENDIF
    ENDIF

    ! Deallocate fields of the Meteorology State object
    CALL Cleanup_State_Met( am_I_Root, State_Met, RC )
    IF ( am_I_Root ) THEN
       IF ( RC == GC_SUCCESS ) THEN
          write(*,'(a)') 'Chem::State_Met Finalize... OK.'
       ELSE
          write(*,'(a)') 'Chem::State_Met Finalize... FAILURE.'
       ENDIF
    ENDIF

! GEOS-5 only (this is an omission in GCHP - I remember putting it in
! but somehow it never got pushed - add to v11-02):
    ! Deallocate fields of the Chemistry State object
    CALL Cleanup_State_Diag( am_I_Root, State_Diag, RC )
    IF ( am_I_Root ) THEN
       IF ( RC == GC_SUCCESS ) THEN
          write(*,'(a)') 'Chem::State_Diag Finalize... OK.'
       ELSE
          write(*,'(a)') 'Chem::State_Diag Finalize... FAILURE.'
       ENDIF
    ENDIF

  END SUBROUTINE GIGC_Chunk_Final
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
    IDTO3 = Ind_('O3')

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
