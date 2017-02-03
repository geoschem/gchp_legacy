!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: GCHP_Utils
!
! !DESCRIPTION: Utility module for the ESMF interface to GEOS-Chem.
!\\
!\\
! !INTERFACE:
!
MODULE GCHP_Utils
!
! !USES:
!
  IMPLICIT NONE
  INTEGER            :: IU_GEOS, IOS
  INTEGER, PARAMETER :: FIRSTCOL = 26
  INTEGER, PARAMETER :: MAXDIM   = 500

  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: GET_SPC_INDX
  PUBLIC :: GET_PBLLEV
  PUBLIC :: READ_SPECIES_FROM_FILE
  PUBLIC :: GIGC_Assert_Units
  PUBLIC :: GIGC_Revert_Units
  PUBLIC :: GIGC_Cap_Tropopause_Prs
!
! !REVISION HISTORY:
!  09 Oct 2012 - M. Long     - Initial version
!  09 Oct 2012 - R. Yantosca - Added ProTeX headers
!  09 Oct 2012 - R. Yantosca - Use F90 free-format indenting (Emacs F90 mode)
!  22 Oct 2012 - R. Yantosca - Renamed to gigc_chem_utils.F90

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
! !IROUTINE: get_spc_indx
!
! !DESCRIPTION: Returns the index of a chemical species given its name.
!\\
!\\
! !INTERFACE:
!
  FUNCTION Get_Spc_Indx( SPC_NAME, GC_SPC_IDS, GC_SPC_NAMES ) RESULT( SPC_INDX )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: SPC_NAME          ! Species name
    CHARACTER(LEN=*), INTENT(IN) :: GC_SPC_NAMES(:)   ! Names of all species
    INTEGER,          INTENT(IN) :: GC_SPC_IDS(:)     ! ID's  of all species
!
! !RETURN VALUE:
!
    INTEGER                      :: SPC_INDX          ! Index of this species 
!
! !REVISION HISTORY: 
!  09 Oct 2012 - M. Long     - Initial version
!  09 Oct 2012 - R. Yantosca - Added ProTeX headers
!  09 Oct 2012 - R. Yantosca - Declare INDX as integer return value
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: M

    ! Initialize
    SPC_INDX = -1

    ! Loop over all species names
    DO M = 1, SIZE( GC_SPC_NAMES )

       ! Return the index of the sought-for species
       IF( TRIM( SPC_NAME ) == TRIM( GC_SPC_NAMES(M) ) ) THEN
          SPC_INDX = GC_SPC_IDS(M)
          EXIT
       ENDIF

    ENDDO

  END FUNCTION Get_Spc_Indx
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_pbllev
!
! !DESCRIPTION: Get the level of the PBL
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE get_pbllev!(pblht,zmid,ncol,pbllev)
!
! !USES:
!
    !USE ppgrid
!
! !INPUT PARAMETERS:
!
    ! Comment these out for now
    !integer,  intent(in)   :: ncol ! Columns per chunk.
    !
    ! Calculate pbllev_cam across ncol
    !real, intent(in)   :: pblht(pcols)
    !real, intent(in)   :: zmid(pcols, pver)
    !real, intent(out)  :: pbllev(pcols)

!
! !REMARKS:
!  Comment out until further notice.
!
! !REVISION HISTORY: 
!  09 Oct 2012 - M. Long     - Initial version
!  09 Oct 2012 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!

!    integer                :: i, z
!
!    pbllev = 0
!
!    DO i=1, ncol
!       DO z=1, pver-1
!          IF (pblht(i) <= zmid(i,pver-(z))) THEN
!             IF (pblht(i) > zmid(i,pver-(z-1))) THEN
!                pbllev(i) = z
!             ENDIF
!          ENDIF
!       ENDDO
!    ENDDO
  END SUBROUTINE get_pbllev
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_pbllev
!
! !DESCRIPTION: Get the level of the PBL
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE READ_SPECIES_FROM_FILE( GC, am_I_Root, AdvSpc, Nadv, RC )
!
! !USES:
!
    USE ESMF
    USE MAPL_Mod
    USE INQUIREMOD, ONLY : findFreeLUN
    USE FILE_MOD,   ONLY : IOERROR
!
! !INPUT PARAMETERS:
!
    CHARACTER(40),       INTENT(OUT)   :: AdvSpc(:)
    INTEGER,             INTENT(OUT)   :: RC, Nadv
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref to this GridComp
    LOGICAL                            :: am_I_Root
!
! !REMARKS:
!  !
! !REVISION HISTORY: 
!  04 Jan 2016 - M. Long     - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: N
    CHARACTER(LEN=MAXDIM) :: LINE, MSG
    CHARACTER(LEN=255)    :: SUBSTRS(500)

    IU_GEOS = findFreeLun()

    ! Open file
    OPEN( IU_GEOS, FILE='input.geos', STATUS='OLD', IOSTAT=IOS )
    IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, 'READ_SPECIES_FROM_FILE()' )

    DO
      ! Read a line from the file
      READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE
      IF ( INDEX( LINE, 'ADVECTED SPECIES MENU' ) > 0 ) EXIT
    ENDDO

    NADV=0
    DO WHILE( INDEX( LINE, 'TRANSPORT MENU' ) .le. 0) 
       ! Read next and Split line into substrings
      CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_adv_spec_menu:4', LINE )
      IF ( INDEX( LINE, 'Species #' ) > 0 ) THEN
         ! Save advected species name
         call MAPL_AddInternalSpec(GC, &
              SHORT_NAME         = 'SPC_'//TRIM(SUBSTRS(1)),  &
              LONG_NAME          = TRIM(SUBSTRS(1)),  &
              UNITS              = 'mol mol-1', &
              DIMS               = MAPL_DimsHorzVert,    &
              VLOCATION          = MAPL_VLocationCenter,    &
              FRIENDLYTO         = 'DYNAMICS:TURBULENCE:MOIST',    &
              RC                 = RC  )
         NADV = NADV+1
         AdvSpc(NADV) = TRIM(SUBSTRS(1))
      ENDIF
    ENDDO

    ! Close input file
    CLOSE( IU_GEOS )
    
  END SUBROUTINE READ_SPECIES_FROM_FILE
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: split_one_line
!
! !DESCRIPTION: Subroutine SPLIT\_ONE\_LINE reads a line from the input file 
!  (via routine READ\_ONE\_LINE), and separates it into substrings.
!\\
!\\
!  SPLIT\_ONE\_LINE also checks to see if the number of substrings found is 
!  equal to the number of substrings that we expected to find.  However, if
!  you don't know a-priori how many substrings to expect a-priori, 
!  you can skip the error check.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE SPLIT_ONE_LINE( SUBSTRS, N_SUBSTRS, N_EXP, LOCATION, LINE ) 
!
! !USES:
!
      USE CHARPAK_MOD, ONLY: STRSPLIT
!
! !INPUT PARAMETERS: 
!
      ! Number of substrings we expect to find
      INTEGER,            INTENT(IN)  :: N_EXP

      ! Name of routine that called SPLIT_ONE_LINE
      CHARACTER(LEN=*),   INTENT(IN)  :: LOCATION 
!
! !OUTPUT PARAMETERS:
!
      ! Array of substrings (separated by " ")
      CHARACTER(LEN=255),    INTENT(OUT) :: SUBSTRS(MAXDIM)
      CHARACTER(LEN=MAXDIM), INTENT(OUT) :: LINE

      ! Number of substrings actually found
      INTEGER,            INTENT(OUT) :: N_SUBSTRS
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!  17 Sep 2013 - R. Yantosca - Extend LINE to 500 chars to allow more tracers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL                         :: EOF
      CHARACTER(LEN=255)              :: MSG

      !=================================================================
      ! SPLIT_ONE_LINE begins here!
      !=================================================================      

      ! Create error msg
      MSG = 'SPLIT_ONE_LINE: error at ' // TRIM( LOCATION )

      !=================================================================
      ! Read a line from disk
      !=================================================================
      ! Read a line from the file
      READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE

      ! STOP on End-of-File w/ error msg
      IF ( EOF ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, '(a)' ) 'End of file encountered!' 
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
      ENDIF

      !=================================================================
      ! Split the lines between spaces -- start at column FIRSTCOL
      !=================================================================
      CALL STRSPLIT( LINE(FIRSTCOL:), ' ', SUBSTRS, N_SUBSTRS )

      ! Sometimes we don't know how many substrings to expect,
      ! if N_EXP is greater than MAXDIM, then skip the error check
      IF ( N_EXP < 0 ) RETURN

      ! Stop if we found the wrong 
      IF ( N_EXP /= N_SUBSTRS ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, 100   ) N_EXP, N_SUBSTRS
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
 100     FORMAT( 'Expected ',i2, ' substrs but found ',i3 )
      ENDIF
       
      END SUBROUTINE SPLIT_ONE_LINE
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
    USE GC_Grid_Mod,        ONLY : Get_XEdge
    USE GC_Grid_Mod,        ONLY : Get_YEdge
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
! !IROUTINE: gigc_revert_units
!
! !DESCRIPTION: Subroutine GIGC\_REVERT\_UNITS forces the units back to kg/kg dry
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Revert_Units( am_I_Root, Input_Opt, State_Chm, State_Met, RC )
!
! !USES:
!
    USE ErrCode_Mod
    USE Input_Opt_Mod,    ONLY : OptInput
    USE State_Chm_Mod,    ONLY : ChmState
    USE State_Met_Mod,    ONLY : MetState
    Use UnitConv_Mod
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
!  21 Dec 2016 - S. D. Eastham - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!

    ! Are tracers in mass or mixing ratio?
    ! Are tracers in mass or mixing ratio?
    Logical                        :: LPrt, LConvert
    Character(Len=20)              :: oldUnits

    ! Assume succes
    RC = GC_SUCCESS

    LPrt = (am_I_Root .and. (Input_Opt%LPrt) )
    oldUnits = Trim(State_Chm%Spc_Units)
    LConvert = .False.

    ! Check what unit the tracers are in - hold as kg/kg dry throughout
    Select Case (Trim(State_Chm%Spc_Units))
        Case ('kg/kg dry')
            ! Do nothing
        Case ('kg')
            CALL ConvertSpc_Kg_to_KgKgDry( am_I_Root, State_Met, State_Chm, RC )
        Case ('v/v dry')
            CALL ConvertSpc_VVDry_to_KgKgDry( am_I_Root, State_Chm, RC )
        Case Default
            Write(6,'(a,a,a)') 'Species units (', State_Chm%Spc_Units, ') not recognized'
            RC = GC_FAILURE
    End Select

    ! Debug information
    If (LConvert.and.LPrt) Then
       Write(6,'(a,a,a)') ' GIGC: Species units reverted from ', oldUnits, ' to kg/kg dry'
    End If

  END SUBROUTINE GIGC_Revert_Units
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_assert_units
!
! !DESCRIPTION: Function ASSERT\_UNITS checks to make sure the units are
! correct
!\\
!\\
! !INTERFACE:
!
  FUNCTION GIGC_Assert_Units( am_I_Root, State_Chm ) RESULT( isOK )
!
! !USES:
!
    USE State_Chm_Mod,    ONLY : ChmState
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root     ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ChmState), INTENT(INOUT) :: State_Chm     ! Chemistry State object
!
! !OUTPUT PARAMETERS:
!
    LOGICAL                       :: isOK          ! True if correct unit
!
! !REVISION HISTORY: 
!  21 Dec 2016 - S. D. Eastham - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Check what unit the tracers are in - hold as kg/kg dry throughout
    Select Case (Trim(State_Chm%Spc_Units))
        Case ('kg/kg dry')
            isOK = .True.
        Case Default
            isOK = .False.
    End Select

  END FUNCTION GIGC_Assert_Units
!EOC

END MODULE GCHP_Utils
