#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_initialization_mod
!
! !DESCRIPTION: Module GIGC\_INITIALIZATION\_MOD contains the initialize 
!  methods for the ESMF interface of High Performance GEOS-Chem.
!\\
!\\
! !INTERFACE: 
!      
MODULE GIGC_Initialization_Mod
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
  PUBLIC  :: GIGC_Init_Simulation
!
! !PRIVATE MEMBER FUNCTIONS:
!
!
! !REVISION HISTORY: 
!  16 Oct 2012 - M. Long     - Initial version
!  16 Oct 2012 - R. Yantosca - Added ProTeX headers
!  22 Oct 2012 - R. Yantosca - Renamed to gigc_initialization_mod.F90
!  22 Oct 2012 - R. Yantosca - Renamed several routines for better consistency
!  25 Oct 2012 - R. Yantosca - Remove routine GIGC_SetEnv
!  03 Dec 2012 - R. Yantosca - Now pass extra arguments to GIGC_Init_Dimensions
!  06 Dec 2012 - R. Yantosca - Now remove routine GIGC_Init_TimeInterface; this
!                              is now superseded by Accept_Date_Time_From_ESMF
!  08 Mar 2018 - E. Lundgren - Remove GIGC_Get_Options; merge its functionality
!                              into GGIC_Init_Simulation for simplicity;
!                              overhaul gigc_init_simulation for v11-02e
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_init_simulation
!
! !DESCRIPTION: Routine GIGC\_INIT\_SIMULATION is the Initialize method for 
!  the ESMF interface that connects GCHP to the GEOS-5 GCM.  Calls to the 
!  various GEOS-Chem init routines are made from here.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Init_Simulation( am_I_Root,                        &
                                   nymdB,           nhmsB,           &
                                   nymdE,           nhmsE,           &
                                   tsChem,          tsDyn,           &
                                   lonCtr,          latCtr,          &     
                                   value_I_LO,      value_J_LO,      &
                                   value_I_HI,      value_J_HI,      &
                                   value_IM,        value_JM,        &
                                   value_LM,        value_IM_WORLD,  &
                                   value_JM_WORLD,  value_LM_WORLD,  &
                                   myPET,           GC,              &
                                   EXPORT,          Input_Opt,       &
                                   State_Chm,       State_Diag,      &
                                   State_Met,       HistoryConfig,   &
                                   RC         )      
!
! !USES:
!
    USE Chemistry_Mod,           ONLY : INIT_CHEMISTRY
    USE CMN_SIZE_Mod
    USE GC_Environment_Mod
    USE Input_Mod,               ONLY : Read_Input_File
    USE Input_Opt_Mod,           ONLY : OptInput, Set_Input_Opt
    USE FAST_JX_Mod,             ONLY : Init_FJX
    USE GC_Grid_Mod,             ONLY : SetGridFromCtr
    USE GIGC_HistoryExports_Mod, ONLY : HistoryConfigObj
    USE GIGC_MPI_Wrap,           ONLY : GIGC_Input_Bcast
    USE Linoz_Mod,               ONLY : Linoz_Read
    USE PBL_Mix_Mod,             ONLY : Init_PBL_Mix
    USE PhysConstants,           ONLY : PI_180
    USE Pressure_Mod,            ONLY : Init_Pressure
    USE Roundoff_Mod,            ONLY : RoundOff
    USE State_Chm_Mod,           ONLY : ChmState
    USE State_Diag_Mod,          ONLY : DgnState
    USE State_Met_Mod,           ONLY : MetState
    USE TOMS_Mod,                ONLY : Init_TOMS
    USE Time_Mod,                ONLY : Set_Timesteps
!
! !INPUT PARAMETERS: 
!
    LOGICAL,         INTENT(IN)    :: am_I_Root       ! Is this the root CPU?
    INTEGER,         INTENT(IN)    :: myPET           ! Local PET
    INTEGER,         INTENT(IN)    :: nymdB           ! GMT date (YYYY/MM/DD)
    INTEGER,         INTENT(IN)    :: nhmsB           ! GMT time (hh:mm:ss)
    INTEGER,         INTENT(IN)    :: nymdE           ! GMT date (YYYY/MM/DD)
    INTEGER,         INTENT(IN)    :: nhmsE           ! GMT time (hh:mm:ss)
    REAL,            INTENT(IN)    :: tsChem          ! Chem timestep [seconds]
    REAL,            INTENT(IN)    :: tsDyn           ! Dyn  timestep [seconds]
    REAL*4,  TARGET, INTENT(IN)    :: lonCtr(:,:)     ! Lon centers [radians]
    REAL*4,  TARGET, INTENT(IN)    :: latCtr(:,:)     ! Lat centers [radians]
    INTEGER,         INTENT(IN)    :: value_I_LO      ! Min local lon index
    INTEGER,         INTENT(IN)    :: value_J_LO      ! Min local lat index
    INTEGER,         INTENT(IN)    :: value_I_HI      ! Max local lon index
    INTEGER,         INTENT(IN)    :: value_J_HI      ! Max local lat index
    INTEGER,         INTENT(IN)    :: value_IM        ! # lons on this CPU
    INTEGER,         INTENT(IN)    :: value_JM        ! # lats on this CPU
    INTEGER,         INTENT(IN)    :: value_LM        ! # levs on this CPU
    INTEGER,         INTENT(IN)    :: value_IM_WORLD  ! # lons in whole globe
    INTEGER,         INTENT(IN)    :: value_JM_WORLD  ! # lats in whole globe
    INTEGER,         INTENT(IN)    :: value_LM_WORLD  ! # levs in whole globe
!
! !INPUT/OUTPUT PARAMETERS:
    TYPE(ESMF_State), INTENT(INOUT), TARGET :: EXPORT ! Export state object
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(OptInput),      INTENT(INOUT) :: Input_Opt   ! Input Options
    TYPE(ChmState),      INTENT(INOUT) :: State_Chm   ! Chemistry State
    TYPE(DgnState),      INTENT(INOUT) :: State_Diag  ! Diagnostics State
    TYPE(MetState),      INTENT(INOUT) :: State_Met   ! Meteorology State
    TYPE(HistoryConfigObj), POINTER    :: HistoryConfig ! History config obj 
!
!
! !OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(OUT)   :: RC              ! Success or failure?  
!
! !REMARKS
!  Add other calls to GEOS-Chem init routines as necessary.
!
! !REVISION HISTORY: 
!  15 Oct 2012 - M. Long     - Initial version
!  15 Oct 2012 - R. Yantosca - Added ProTeX Headers, use F90 format/indents
!  17 Oct 2012 - R. Yantosca - Now initialize the chemistry mechanism
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_chm_mod.F90
!  19 Oct 2012 - R. Yantosca - Now reference gigc_state_met_mod.F90
!  01 Nov 2012 - R. Yantosca - Now reference gigc_input_opt_mod.F90
!  09 Nov 2012 - R. Yantosca - Now use fields from Input Options object
!  13 Nov 2012 - R. Yantosca - Pass Input Options object to routines 
!                              SETEMDEP, INIT_COMODE
!  28 Nov 2012 - R. Yantosca - Remove reference to INIT_DAO, since there are
!                              no more module arrays anymore in dao_mod.F
!  29 Nov 2012 - R. Yantosca - Add lonCtr, latCtr, latEdg as arguments
!  29 Nov 2012 - R. Yantosca - Now pass am_I_Root to Olson landmap routines
!  03 Dec 2012 - R. Yantosca - Now pass value_* arguments to pass dimension
!                              info from ESMF down to lower-level routines
!  06 Dec 2012 - R. Yantosca - Now accept start & end dates & times via 
!                              the nymdB, nymdE, nhmsB, nhmsE arguments
!  06 Dec 2012 - R. Yantosca - Remove nymd, nhms arguments, these will be
!                              the same as nymdB, nhmsB (start of run)
!  26 Feb 2013 - M. Long     - Now read ASCII input files on root CPU and 
!                              broadcast to other CPUs.
!  26 Feb 2013 - R. Yantosca - Cosmetic changes
!  01 Mar 2013 - R. Yantosca - Need to move the definition of prtDebug higher
!  04 Mar 2013 - R. Yantosca - Now call GIGC_Init_Extra, which moves some init
!                              calls out of the run stage.  This has to be done
!                              after we broadcast Input_Opt to non-root CPUs.
!  07 Mar 2013 - R. Yantosca - Call READER on all CPUs until further notice
!  07 Mar 2013 - R. Yantosca - Now use keyword arguments for clarity
!  02 Jan 2014 - C. Keller   - Now call SetGridFromCtr to make sure that 
!                              grid_mod.F90 stored the correct edges/mid-points.
!  01 Dec 2016 - E. Lundgren - Remove GC classic Olson routine calls
!  06 Mar 2018 - E. Lundgren - GC timesteps are now seconds;
!                              Call set_input_opt to initialize input_opt vars;
!                              Add error handling using MAPL Assert_;
!                              Rename Initialize_Geos_Grid to GC_Init_Grid;
!                              Now call GC_Allocate_All after input.geos read;
!                              Move code from gigc_Get_Options subrtn here;
!                              Restructure grid init based on gcbe v11-02e;
!                              Remove all unused code and simplify comments
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER            :: I, J, L
    CHARACTER(LEN=255) :: Iam

    !=======================================================================
    ! GIGC_Init_Simulation starts here
    !=======================================================================

    ! Initialize
    RC  = GC_SUCCESS
    Iam = 'GIGC_Init_Simulation (gigc_initialization_mod.F90)'

    ! Initialize Input_Opt fields to zeros or equivalent
    CALL Set_Input_Opt( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

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

    ! ewl TODO: Do we need this updated? Do we use dlon/dlat? If not,
    ! we can remove this. If we do need this, it might be better in 
    ! gc_bleeding_edge. Should certainly 
    !========================================================================
    ! Compute the DLON and DLAT values.  NOTE, this is a kludge since to do
    ! this truly rigorously, we should take the differences between the grid
    ! box edges.  But because I can't seem to find a way to get the grid
    ! box edge information, the next best thing is to take the differences
    ! between the grid box centers.  They should all be the same given that
    ! the GEOS-Chem grid is regular. (bmy, 12/7/12)
    !========================================================================
    DO L = 1, LLPAR
    DO J = 1, JJPAR
    DO I = 1, IIPAR

       ! Compute Delta-Longitudes [degrees]
       IF ( I == IIPAR ) THEN
          dLon(I,J,L) = RoundOff( ( DBLE( lonCtr(IIPAR,  J) ) / PI_180 ), 4 ) &
                      - RoundOff( ( DBLE( lonCtr(IIPAR-1,J) ) / PI_180 ), 4 )
       ELSE
          dLon(I,J,L) = RoundOff( ( DBLE( lonCtr(I+1,    J) ) / PI_180 ), 4 ) &
                      - RoundOff( ( DBLE( lonCtr(I,      J) ) / PI_180 ), 4 )
       ENDIF

       ! Compute Delta-Latitudes [degrees]
       IF ( J == JJPAR ) THEN
          dLat(I,J,L) = RoundOff( ( DBLE( latCtr(I,JJPAR  ) ) / PI_180 ), 4 ) &
                      - RoundOff( ( DBLE( latCtr(I,JJPAR-1) ) / PI_180 ), 4 )
       ELSE
          dLat(I,J,L) = RoundOff( ( DBLE( latCtr(I,J+1    ) ) / PI_180 ), 4 ) &
                      - RoundOff( ( DBLE( latCtr(I,J      ) ) / PI_180 ), 4 )
       ENDIF

    ENDDO
    ENDDO
    ENDDO

    ! Root CPU only
    IF ( am_I_Root ) THEN

       ! Read input.geos
       CALL Read_Input_File( am_I_Root, Input_Opt, RC )
       ASSERT_(RC==GC_SUCCESS)

       ! In the ESMF/MPI environment, we can get the total overhead ozone
       ! either from the met fields (GIGCsa) or from the Import State (GEOS-5)
       Input_Opt%USE_O3_FROM_MET = .TRUE.

       ! Read LINOZ climatology
       IF ( Input_Opt%LLINOZ ) THEN
          CALL Linoz_Read( am_I_Root, Input_Opt, RC ) 
          ASSERT_(RC==GC_SUCCESS)
       ENDIF
    ENDIF

    ! Allocate all lat/lon arrays
    CALL GC_Allocate_All( am_I_Root,      Input_Opt,      RC,               &
                          value_I_LO,     value_J_LO,     value_I_HI,       &
                          value_J_HI,     value_IM,       value_JM,         &
                          value_LM,       value_IM_WORLD, value_JM_WORLD,   &
                          value_LM_WORLD  )            
    ASSERT_(RC==GC_SUCCESS)

    ! Broadcast Input_Opt from root to all other CPUs
    CALL GIGC_Input_Bcast( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Initialize horizontal grid parameters
    CALL GC_Init_Grid( am_I_Root, Input_Opt, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Set grid based on passed mid-points
    CALL SetGridFromCtr( am_I_Root, value_IM, value_JM, lonCtr, latCtr, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Set timesteps
    CALL Set_Timesteps( am_I_Root  = am_I_Root,                          &
                        Chemistry  = Input_Opt%TS_CHEM,                  &
                        Convection = Input_Opt%TS_CONV,                  &
                        Dynamics   = Input_Opt%TS_DYN,                   &
                        Emission   = Input_Opt%TS_EMIS,                  &
                        Radiation  = Input_Opt%TS_RAD,                   &
                        Unit_Conv  = MAX( Input_Opt%TS_DYN,              &
                                          Input_Opt%TS_CONV ),           &
                        Diagnos    = Input_Opt%TS_DIAG         )

    ! Initialize derived-type objects for met, chem, and diag
    CALL GC_Init_StateObj( am_I_Root, HistoryConfig%DiagList, Input_Opt, &
                           State_Chm, State_Diag, State_Met, RC )
    ASSERT_(RC==GC_SUCCESS)

    ! Initialize other GEOS-Chem modules
    CALL GC_Init_Extra( am_I_Root, HistoryConfig%DiagList, Input_Opt,    &
                        State_Chm, State_Diag, RC ) 
    ASSERT_(RC==GC_SUCCESS)

    ! Initialize photolysis
    IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR.                     &
         Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
       CALL Init_FJX( am_I_Root, Input_Opt, State_Chm, State_Diag, RC ) 
       ASSERT_(RC==GC_SUCCESS)
    ENDIF
      
    ! Set State_Chm units
    State_Chm%Spc_Units = 'kg/kg dry'

    ! Initialize pressure module (set Ap & Bp)
    CALL Init_Pressure( am_I_Root )

    ! Initialize PBL mixing module
    CALL Init_PBL_Mix( am_I_Root, RC )
    ASSERT_(RC==GC_SUCCESS)
    
    ! Initialize chemistry mechanism
    IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR. Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
       CALL Init_Chemistry( am_I_Root, Input_Opt, State_Chm, State_Diag, RC )
       ASSERT_(RC==GC_SUCCESS)
    ENDIF

    ! Allocate array of overhead O3 columns for TOMS if chemistry is on
    IF ( Input_Opt%LCHEM ) THEN
       CALL Init_TOMS( am_I_Root, Input_Opt, RC )
       ASSERT_(RC==GC_SUCCESS)
    ENDIF

    ! Return success
    RC = GC_Success

  END SUBROUTINE GIGC_Init_Simulation
!EOC
END MODULE GIGC_Initialization_Mod
