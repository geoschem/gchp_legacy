#if defined( ESMF_ )
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_initialization_mod
!
! !DESCRIPTION: Module GIGC\_INITIALIZATION\_MOD is the module that
!  the initialize methods for the ESMF interface to the Grid-Independent
!  GEOS-Chem (aka "GIGC").
!\\
!\\
! !INTERFACE: 
!      
MODULE GIGC_Initialization_Mod
!
! !USES:
!      
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!      
  PUBLIC  :: GIGC_Get_Options
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
! !IROUTINE: gigc_get_options
!
! !DESCRIPTION: Routine GIGC\_GET\_OPTIONS reads options for a GEOS-Chem 
!  simulation from the input.geos\_\_\_.rc input file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Get_Options( am_I_Root, lonCtr,    latCtr,  &
                               Input_Opt, State_Chm, RC      )
!
! !USES:
!
    USE PhysConstants       
    USE CMN_SIZE_Mod
    USE Roundoff_Mod,       ONLY : RoundOff
    USE Error_Mod,          ONLY : Debug_Msg
    USE ErrCode_Mod
    USE Input_Opt_Mod,      ONLY : OptInput
    USE State_Chm_Mod,      ONLY : ChmState
    USE Input_Mod,          ONLY : Read_Input_File
    USE Linoz_Mod,          ONLY : Linoz_Read
!
! !INPUT PARAMETERS: 
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
    REAL*4,         INTENT(IN)    :: lonCtr(:,:)   ! Lon ctrs [deg] from ESMF
    REAL*4,         INTENT(IN)    :: latCtr(:,:)   ! Lat ctrs [deg] from ESMF
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
    TYPE(ChmState), INTENT(INOUT) :: State_Chm
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC            ! Success or failure
! 
! !REMARKS:
!  NOTE: For now assume that GEOS_Chem will always accept a regular 
!  Cartesian grid.  This is more or less dictated by the input data.
!  The GEOS-5 data can be regridded via ESMF from whatever grid it uses.
!  (bmy, 11/30/12)
!
! !REVISION HISTORY: 
!  15 Oct 2012 - M. Long     - Initial version
!  15 Oct 2012 - R. Yantosca - Added ProTeX Headers, use F90 format/indents
!  22 Oct 2012 - R. Yantosca - Renamed to GIGC_Get_Options
!  22 Oct 2012 - R. Yantosca - Added RC output argument
!  01 Nov 2012 - R. Yantosca - Now pass the Input Options object via arg list
!  03 Dec 2012 - R. Yantosca - Reorder subroutines for clarity
!  07 Dec 2012 - R. Yantosca - Compute DLON, DLAT more rigorously
!  26 Feb 2013 - M. Long     - Now pass State_Chm as an argument
!  26 Feb 2013 - M. Long     - Read "input.geos" on root CPU only
!  06 Mar 2013 - R. Yantosca - Now move non-root CPU setup out of this routine
!  18 Mar 2013 - R. Yantosca - Now call LINOZ_READ on the root CPU
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Scalars
    INTEGER :: I, J, L
  
    ! Assume success
    RC = GC_SUCCESS

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

    !========================================================================
    ! Root CPU setup
    !========================================================================
    IF ( am_I_Root ) THEN

       ! Read the GEOS-Chem input file here.  For now only read on the root
       ! CPU so that we can broadcast to other CPUs in GIGC_Init_Simulation
       ! (mlong, bmy, 2/26/13)
       CALL Read_Input_File( am_I_Root, Input_Opt, RC )
       IF ( RC /= GC_SUCCESS ) RETURN

       ! In the ESMF/MPI environment, we can get the total overhead ozone
       ! either from the met fields (GIGCsa) or from the Import State (GEOS-5)
       Input_Opt%USE_O3_FROM_MET = .TRUE.

       ! Echo info
       IF ( Input_Opt%LPRT ) THEN
          CALL Debug_Msg( '### Root CPU, after READ_INPUT_FILE' )
       ENDIF

       ! Read the LINOZ climatology file on the root CPU, so that we can
       ! MPI broadcast the data to the other CPUs in GIGC_Init_Simulation
       ! (bmy, 3/18/13)
       IF ( Input_Opt%LLINOZ ) THEN
          CALL Linoz_Read( am_I_Root, Input_Opt, RC ) 
          IF ( RC /= GC_SUCCESS ) RETURN

          ! Echo info
          IF ( Input_Opt%LPRT ) THEN
             CALL Debug_Msg( '### Root CPU, after LINOZ_READ' )
          ENDIF
       ENDIF
    ENDIF

  END SUBROUTINE GIGC_Get_Options
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_init_simulation
!
! !DESCRIPTION: Routine GIGC\_INIT\_SIMULATION is the Initialize method for 
!  the ESMF interface that connects the Grid-Independent GEOS-Chem (aka "GIGC")
!  to the GEOS-5 GCM.  Calls to the various GEOS-Chem init routines (which 
!  allocate arrays, etc.) are made from here.
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
                                   myPET,           Input_Opt,       &
                                   State_Chm,       State_Diag,      &
                                   State_Met,       RC         )      
!
! !USES:
!
    USE GC_Environment_Mod
    USE ErrCode_Mod  
    USE Input_Opt_Mod
    USE State_Chm_Mod
    USE State_Diag_Mod
    USE State_Met_Mod
    USE PhysConstants
    USE CMN_SIZE_MOD
!    USE COMODE_MOD
!    USE COMODE_LOOP_MOD       
!    USE GCKPP_COMODE_MOD,     ONLY : Init_GCKPP_Comode
    USE ERROR_MOD,            ONLY : Debug_Msg
    USE FAST_JX_MOD,          ONLY : Init_FJX
    USE GC_Grid_Mod,          ONLY : Init_Grid
    USE GC_Grid_Mod,          ONLY : Set_xOffSet
    USE GC_Grid_Mod,          ONLY : Set_yOffSet
    USE GC_Grid_Mod,          ONLY : SetGridFromCtr
    USE Input_Mod,            ONLY : GC_Init_Extra
    USE Input_Mod,            ONLY : Initialize_Geos_Grid
    USE Mapping_Mod,          ONLY : MapWeight
    USE Mapping_Mod,          ONLY : Init_Mapping
    USE PBL_MIX_MOD,          ONLY : INIT_PBL_MIX
    USE PRESSURE_MOD,         ONLY : INIT_PRESSURE
#if defined( APM )
    USE TRACER_MOD,           ONLY : INIT_TRACER
#endif
    USE WETSCAV_MOD,          ONLY : INIT_WETSCAV
!    USE WETSCAV_MOD,          ONLY : Get_WetDep_IDWetD
    USE DRYDEP_MOD,           ONLY : INIT_WEIGHTSS, INIT_DRYDEP
    USE DUST_MOD,             ONLY : INIT_DUST
    USE GIGC_MPI_WRAP
    USE TIME_MOD,             ONLY : SET_TIMESTEPS
    USE SEASALT_MOD,          ONLY : INIT_SEASALT
    USE TOMS_MOD,             ONLY : INIT_TOMS
    
    ! Stratosphere 
    USE STRAT_CHEM_MOD,       ONLY : INIT_STRAT_CHEM

    USE MIXING_MOD,           ONLY : INIT_MIXING
    USE CHEMISTRY_MOD,        ONLY : INIT_CHEMISTRY
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
!
    TYPE(OptInput),  INTENT(INOUT) :: Input_Opt       ! Input Options
    TYPE(ChmState),  INTENT(INOUT) :: State_Chm       ! Chemistry State
    TYPE(DgnState),  INTENT(INOUT) :: State_Diag      ! Diagnostics State
    TYPE(MetState),  INTENT(INOUT) :: State_Met       ! Meteorology State
!
!
! !OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(OUT)   :: RC              ! Success or failure?  
!
! !REMARKS
!  Add other calls to G EOS-Chem init routines as necessary.
!  NOTE: Later on maybe split these init calls among other routines.
!  Also need to add better error trapping
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
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL            :: prtDebug
    INTEGER            :: DTIME, K, AS, N, YEAR, I, J, L, TMP, STAT
    CHARACTER(LEN=255) :: NAME

    !=======================================================================
    ! Initialize key GEOS-Chem sections
    !=======================================================================

    ! Initialize
    RC       = GC_SUCCESS
    DTIME    = tsChem

    ! Determine if we have to print debug output
    prtDebug = ( Input_Opt%LPRT .and. am_I_Root )

    ! Allocate GEOS-Chem module arrays
    CALL GC_Allocate_All  ( am_I_Root      = am_I_Root,                     &
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
                            RC             = RC              )            
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Allocate GEOS-Chem module arrays

    ! Save timing fields in Input_Opt for passing down to module
    ! GeosCore/input_mod.F via routine GIGC_Get_Options (bmy, 12/6/12)
    Input_Opt%NYMDb   = nymdB
    Input_Opt%NHMSb   = nhmsB
    Input_Opt%NYMDe   = nymdE
    Input_Opt%NHMSe   = nhmsE
    Input_Opt%TS_CHEM = INT( tsChem ) / 60   ! Chemistry timestep [min]
    Input_Opt%TS_EMIS = INT( tsChem ) / 60   ! Chemistry timestep [min]
    Input_Opt%TS_DYN  = INT( tsDyn  ) / 60   ! Dynamic   timestep [min]
    Input_Opt%TS_CONV = INT( tsDyn  ) / 60   ! Dynamic   timestep [min]

    Input_Opt%myCPU = myPET

    !-----------------------------------------------------------------------
    ! Read info from the "input.geos" file into the Input_Opt object
    ! on the root CPU.  MPI broadcast Input_Opt to non-root CPUs.
    ! Continue with non-root CPU setup.
    !-----------------------------------------------------------------------

    ! Read options from the "input.geos" file into Input_Opt
    CALL GIGC_Get_Options( am_I_Root = am_I_Root,                           &
                           lonCtr    = lonCtr,                              &
                           latCtr    = latCtr,                              &
                           Input_Opt = Input_Opt,                           &
                           State_Chm = State_Chm,                           &
                           RC        = RC           )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Broadcast fields of Input_Opt from root to all other CPUs
    CALL GIGC_Input_Bcast( am_I_Root = am_I_Root,                           &
                           Input_Opt = Input_Opt,                           &
                           RC        = RC           )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Broadcast IDTxxx etc. tracer flags from root to all other CPUs
    CALL GIGC_IDT_Bcast  ( am_I_Root = am_I_Root,                           &  
                           Input_Opt = Input_Opt,                           &  
                           RC        = RC           )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Complete initialization ops on all threads
    !IF ( .NOT. am_I_Root ) THEN 

    ! Make sure to reset I0 and J0 in grid_mod.F90 with
    ! the values carried in the Input Options object
    CALL Set_xOffSet( Input_Opt%NESTED_I0 )
    CALL Set_yOffSet( Input_Opt%NESTED_J0 )

    ! We still need to call Initialize_Geos_Grid on all CPUs though.
    ! without having to read the "input.geos" file. (mlong, bmy, 2/26/13)
    IF (.not. am_I_Root) then
      CALL Initialize_Geos_Grid( am_I_Root = am_I_Root,                    &
           Input_Opt = Input_Opt,                    &
           RC        =  RC )
      IF ( RC /= GC_SUCCESS ) RETURN

      CALL SetGridFromCtr( am_I_Root, value_IM, value_JM, lonCtr, latCtr, RC )
      IF ( RC /= GC_SUCCESS ) RETURN
    End If

    ! This call should be made on all CPUs
    CALL SetGridFromCtr( am_I_Root, value_IM, value_JM, lonCtr, latCtr, RC )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Initialize dry deposition (in GeosCore/drydep_mod.F)
    ! SDE 2016-03-27: This is now performed in GIGC_Init_Extra (as of v11-01)
    !IF ( Input_Opt%LDRYD .and. (.not. am_I_Root) ) THEN
    !   CALL Init_Drydep( am_I_Root = am_I_Root,                          &
    !        Input_Opt = Input_Opt,                          &
    !        State_Chm = State_Chm,                          &
    !        RC        = RC         )
    !   IF ( RC /= GC_SUCCESS ) RETURN
    !ENDIF
    !ENDIF ! am_I_Root

    ! Initialize tracer quantities (in GeosCore/tracer_mod.F)
#if defined( APM )
    CALL Init_Tracer( am_I_Root = am_I_Root,                             &
         Input_Opt = Input_Opt,                             &
         RC        = RC           )
    IF ( RC /= GC_SUCCESS ) RETURN
#endif

    ! Initialize wet deposition tracer IDs - now obsolete
    !IF ( Input_Opt%LWETD .OR. Input_Opt%LCONV ) THEN
    !   CALL WETDEPID( am_I_Root, Input_Opt, RC )
    !   IF ( RC /= GC_SUCCESS ) RETURN
    !ENDIF

    !ENDIF ! Not root

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

    ! Initialize derived-type objects for meteorology & chemistry states
    CALL GC_Init_All( am_I_Root  = am_I_Root,                              &
                      Input_Opt  = Input_Opt,                              &
                      State_Chm  = State_Chm,                              &
                      State_Diag = State_Diag,                             & 
                      State_Met  = State_Met,                              &
                      RC         = RC         )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! After broadcasting Input_Opt to other CPUs, call GIGC_Init_Extra
    ! to initialize other modules (e.g. carbon_mod.F, dust_mod.F, 
    ! seasalt_mod.F,  sulfate_mod.F).  We needed to move these init 
    ! calls out of the run stage and into the init stage. (bmy, 3/4/13)
    CALL GC_Init_Extra( am_I_Root, Input_Opt, State_Chm, RC ) 
    IF ( RC /= GC_SUCCESS ) RETURN

    !-----------------------------------------------------------------------
    ! Read other ASCII files on the root CPU and broadcast to other CPUs
    !-----------------------------------------------------------------------

!------------------------------------------------------------------------------
! Prior to 3/7/13:
! For now, just call READER on all CPUs.  It may be difficult to try to MPI
! broadcast all of the fields that READER touches (bmy, 3/7/13)
!    ! Read "mglob.dat"
!    IF ( am_I_Root ) THEN
!------------------------------------------------------------------------------
       ! Read from data file mglob.dat
!       CALL READER( .TRUE.,  am_I_Root, Input_Opt )

       !### Debug
       IF ( prtDebug ) THEN
          CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: after READER' )
       ENDIF

!------------------------------------------------------------------------------
! Prior to 3/7/13:
! For now, just call READER on all CPUs.  It may be difficult to try to MPI
! broadcast all of the fields that READER touches (bmy, 3/7/13)
!    ENDIF
!
!    ! Broadcast "mglob.dat"
!       CALL GIGC_Reader_Bcast( RC )
!    CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: after GIGC_Bcast_READER' )
!------------------------------------------------------------------------------

    ! Read "globchem.dat" chemistry mechanism
!------------------------------------------------------------------------------
! Prior to 3/7/13:
! NOTE: for now, just call READCHEM on all CPUs.  Try to figure out how
! to MPI broadcast later.  This could be very difficult. (bmy, mlong, 3/7/13)
!    IF ( am_I_Root ) THEN
!------------------------------------------------------------------------------
!       CALL READCHEM( am_I_Root, Input_Opt, RC )
!       IF ( RC /= GC_SUCCESS ) RETURN

       !### Debug
       IF ( prtDebug ) THEN
          CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: after READCHEM' )        
       ENDIF
!------------------------------------------------------------------------------
! Prior to 3/7/13:
! NOTE: for now, just call READCHEM on all CPUs.  Try to figure out how
! to MPI broadcast later.  This could be very difficult. (bmy, mlong, 3/7/13)
!    ENDIF
!
!    ! Broadcast "globchem.dat" to other CPUs
!    CALL GIGC_ReadChem_Bcast( RC )
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Prior to 3/7/13:
! NOTE: for now, just call INPHOT on all CPUs.  Try to figure out how
! to MPI broadcast later.  This could be very difficult. (bmy, mlong, 3/7/13)
!    IF ( am_I_Root ) THEN
!------------------------------------------------------------------------------
       IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR.                     &
            Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
          CALL INIT_FJX( am_I_Root, Input_Opt, RC )  ! Are we on the root CPU?
          IF ( RC /= GC_SUCCESS ) RETURN
          
          !### Debug
          IF ( prtDebug ) THEN
             CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: after INIT_FJX' )        
          ENDIF
       ENDIF
!------------------------------------------------------------------------------
! Prior to 3/7/13:
! NOTE: for now, just call INPHOT on all CPUs.  Try to figure out how
! to MPI broadcast later.  This could be very difficult. (bmy, mlong, 3/7/13)
!    ENDIF
!
!    Broadcast FAST-J inputs to other CPUs
!    CALL GIGC_Inphot_Bcast(  Input_Opt, RC )
!------------------------------------------------------------------------------

    !-----------------------------------------------------------------------
    ! Continue with GEOS-Chem setup
    !-----------------------------------------------------------------------

    ! Zero diagnostic arrays
    CALL Initialize( am_I_Root, Input_Opt, 2, RC )
    IF ( RC /= GC_SUCCESS ) RETURN

    ! Zero diagnostic counters
    CALL Initialize( am_I_Root, Input_Opt, 3, RC )
    IF ( RC /= GC_SUCCESS ) RETURN
      
    ! Set State_Chm units
    State_Chm%Spc_Units = 'kg/kg dry'

    ! Initialize the GEOS-Chem pressure module (set Ap & Bp)
    CALL Init_Pressure( am_I_Root )

    ! Initialize the PBL mixing module
    CALL Init_PBL_Mix()

    !=======================================================================
    ! Initialize dry deposition 
    !=======================================================================
    IF ( Input_Opt%LDRYD )  THEN
       ! Placeholder

       !### Debug
       IF ( prtDebug ) THEN
          CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: initialize drydep' )
       ENDIF
    ENDIF


    ! Initialize PBL quantities but do not do mixing
    ! Add option for non-local PBL (Lin, 03/31/09) 
    !CALL INIT_MIXING ( am_I_Root, Input_Opt, &
    !                   State_Met, State_Chm, RC ) 
    
    !=======================================================================
    ! Initialize chemistry mechanism
    !=======================================================================

    ! Moved here (from chemistry_mod.F and chemdr.F) because some
    ! of the variables are used for non-local PBL mixing BEFORE 
    ! the first call of the chemistry routines (ckeller, 05/19/14).
    IF ( Input_Opt%ITS_A_FULLCHEM_SIM .OR. Input_Opt%ITS_AN_AEROSOL_SIM ) THEN
       CALL INIT_CHEMISTRY ( am_I_Root, Input_Opt, State_Chm, RC )
    ENDIF

    ! If we are doing chemistry ...
    IF ( Input_Opt%LCHEM ) THEN
       
       ! Allocate array of overhead O3 columns for TOMS
       CALL INIT_TOMS( am_I_Root, Input_Opt, RC )

       !### Debug
       IF ( prtDebug ) THEN
          CALL DEBUG_MSG( '### GIGC_INIT_SIMULATION: after INIT_TOMS' )
       ENDIF

    ENDIF

!    ! Note: Init_Strat_Chem expects units of kg/kg dry
!    IF ( Input_Opt%LSCHEM ) THEN
!       CALL INIT_STRAT_CHEM( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
!    ENDIF

    ! Return w/ success
    RC = GC_Success

  END SUBROUTINE GIGC_Init_Simulation
!EOC
END MODULE GIGC_Initialization_Mod
#endif
