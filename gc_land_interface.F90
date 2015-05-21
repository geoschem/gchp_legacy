#if defined( ESMF_ )
MODULE GC_LAND_INTERFACE
!
! !USES:
!      

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: LANDTYPE_REMAP
  PUBLIC :: GEOS5_TO_OLSON_LANDTYPE_REMAP
!
! !PRIVATE MEMBER FUNCTIONS:
!
  ! Use GEOS-5 vegetation types? If set to FALSE, the GEOS-Chem
  ! dry-dep types (created from the OLSON land map), will be used instead.
  LOGICAL, PARAMETER  :: UseGEOS5 = .TRUE.
!
! !REMARKS:
!  The routines in this module execute only when GEOS-Chem is connected
!  to the GEOS-5 GCM via the ESMF/MAPL interface.
!
!  Routines are used for interfacing online surface data in GEOS5 with
!  GEOS-Chem
!
! !REVISION HISTORY:
!  08 Jan 2014 - M. Long     - Initial version.
!  06 Jan 2015 - C. Keller   - Update to use grid box fractions (GEOS-5
!                              vegetation or GEOS-Chem dry deposition types).
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE TYPES:
!

CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: LANDTYPE_REMAP 
!
! !DESCRIPTION: Subroutine LANDTYPE\_REMAP is a method for
!  the ESM-coupled Grid-Independent GEOS-Chem (aka "GIGC") to remap GEOS-5's
!  land surface grid types or the GEOS-Chem dry deposition type fractions 
!  to the Olson land types for use by dry deposition.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE LANDTYPE_REMAP( am_I_Root, IMPORT, State_Met, RC )
!
! !USES:
!
    USE ESMF
    USE MAPL_MOD
    USE GIGC_ErrCode_Mod
    USE GIGC_State_Met_Mod, ONLY : MetState

# include "MAPL_Generic.h"
!
! !INPUT/OUTPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   ) :: am_I_Root     ! Root CPU?
    TYPE(ESMF_State), INTENT(INOUT) :: Import        ! Import State
    TYPE(MetState),   INTENT(INOUT) :: State_Met     ! Meteorology State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)     :: RC            ! Success or failure
!
! !REMARKS:
!
! !REVISION HISTORY:
!  06 Jan 2015 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                    :: I, NTYP
    INTEGER                    :: STATUS
    INTEGER, ALLOCATABLE       :: ITYPES(:)
    REAL, POINTER              :: Ptr2D(:,:) => NULL()
    CHARACTER(LEN=2)           :: CHAR2
    CHARACTER(LEN=1)           :: CHAR1
    CHARACTER(LEN=63)          :: PtrName
    CHARACTER(LEN=ESMF_MAXSTR) :: Iam

    !===============================================================
    ! LANDTYPE_REMAP begins here
    !===============================================================

    ! For error handling
    Iam = 'LANDTYPE_REMAP (gc_land_interface.F90)'

    ! Make sure all required variables are associated
    ASSERT_(ASSOCIATED(State_Met%IREG))
    ASSERT_(ASSOCIATED(State_Met%IUSE))
    ASSERT_(ASSOCIATED(State_Met%ILAND))

    IF ( .NOT. UseGEOS5 ) THEN
    !------------------------------------------------------------------
    ! GEOS-CHEM DRYDEP TYPE FRACTIONS
    ! If this option is enabled, it is assumed that the grid box 
    ! fractions of the 11 dry deposition land types used by GEOS-Chem
    ! are provided in ExtData via fields 'IDEP_FRAC_XX', where XX = 
    ! 01..11.
    ! 
    ! The following section reads these fractions (which are between
    ! 0.0 and 1.0 per grid box), and fills the State_Met object 
    ! accordingly. The dry dep types are mapped back onto one of the
    ! corresponding OLSON land types, simply to be consistent with the
    ! standard GEOS-Chem. The OLSON land types will be remapped onto
    ! the dry-dep types in drydep_mod.F
    !------------------------------------------------------------------

       ! There are 11 dry deposition types
       NTYP = 11

       ! Assign an OLSON land type to every drydep type.
       ALLOCATE(ITYPES(NTYP))    
       ITYPES(1 ) = 12
       ITYPES(2 ) = 5 
       ITYPES(3 ) = 3
       ITYPES(4 ) = 30
       ITYPES(5 ) = 7
       ITYPES(6 ) = 20
       ITYPES(7 ) = 9 
       ITYPES(8 ) = 8
       ITYPES(9 ) = 13
       ITYPES(10) = 1
       ITYPES(11) = 0

    ELSE
    !------------------------------------------------------------------
    ! GEOS-5 VEGETATION TYPE FRACTIONS
    ! Similar to the dry dep fractions above, but will use the GEOS-5
    ! vegetation types instead. The grid fractions are imported from
    ! ExtData (field name GEOS5_VEGTYPE_FRAC_XX). The vegetation
    ! fractions will be added to a world initialized with 100% water
    ! everywhere, i.e. the vegetation fractions will be subtracted
    ! from the corresponding water fractions.
    ! 
    ! Since the GEOS-5 vegetation types are currently static, this
    ! approach is better than importing the dominant vegetation type 
    ! of each grid box from the surface component (field ITY).
    !
    ! The dry deposition types obtained from the mapping listed below
    ! seem to differ from the ones obtained from the 'classic'
    ! GEOS-Chem approach based on the Olson land types. I don't know
    ! if that's caused by the mapping or differences in the vegetation
    ! distribution.
    !-------------------------------------------------------------------
    ! GEOS-5     Description of vegetation type             MAP-to-Olson
    ! Index                                                 Index
    ! 1:         BROADLEAF EVERGREEN TREES                  6
    ! 2:         BROADLEAF DECIDUOUS TREES                  25
    ! 3:         NEEDLELEAF TREES                           60
    ! 4:         GROUND COVER                               23
    ! 5:         BROADLEAF SHRUBS                           2
    ! 6:         DWARF TREES (TUNDRA)                       28
    ! 7:         BARE SOIL --- this entry is emtpy !!! ---  0 
    ! 8:         DESERT    --- this entry is empty !!! ---  8
    !
    ! I don't have that in my vegetation file:
    ! 13:        Ice/Glacier                                   13
    !
    ! I cannot find a matching Olson land type for bare soil, so it 
    ! will be set to 0 (water). This entry is empty and will not be
    ! considered anyways.                        (ckeller, 06 Jan 2015)
    !-------------------------------------------------------------------

       ! There are 8 types + 1 for water (default)
       NTYP = 9

       ! Assign an OLSON land type to every drydep type.
       ALLOCATE(ITYPES(NTYP))    
       ITYPES(1 ) = 6 
       ITYPES(2 ) = 25
       ITYPES(3 ) = 60
       ITYPES(4 ) = 23
       ITYPES(5 ) = 2
       ITYPES(6 ) = 28
       ITYPES(7 ) = 0 
       ITYPES(8 ) = 8
       ITYPES(9 ) = -1 ! will be filled manually

    ENDIF

    ! Use all types in all grid boxes (fractions will be zero in 
    ! most places)
    State_Met%IREG(:,:) = NTYP

    ! For GEOS-5 land types, we need to fill up water everywhere, then 
    ! subtract the vegetation from that.
    IF ( UseGEOS5 ) THEN
       State_Met%ILAND(:,:,9) = 0
       State_Met%IUSE (:,:,9) = 1000 ! 100 % water to start with
    ENDIF
 
    ! Do for every dry deposition type
    DO I = 1, NTYP

       ! Skip invalid entries
       IF ( ITYPES(I) < 0 ) CYCLE

       ! Set ILAND to the OLSON land type specified above 
       State_Met%ILAND(:,:,I) = ITYPES(I)

       ! Get pointer name
       IF ( .NOT. UseGEOS5 ) THEN
          WRITE(CHAR2,'(i2.2)') I
          PtrName = 'IDEP_FRAC_' // CHAR2
       ELSE
          WRITE(CHAR1,'(i1.1)') I
          PtrName = 'VEGFRAC_' // CHAR1
       ENDIF

       ! Import dry-dep grid box fractions for this land type 
       CALL MAPL_GetPointer( IMPORT, Ptr2D, TRIM(PtrName), __RC__ )

       ! Pass Add to State_Met array. Convert to per mil (integer)
       State_Met%IUSE(:,:,I) = NINT(Ptr2D * 1000.0)

       ! For GEOS-5 land types, subtract vegetation fractions from water
       IF ( UseGEOS5 ) THEN
          State_Met%IUSE(:,:,9) = State_Met%IUSE(:,:,9) - State_Met%IUSE(:,:,I)
       ENDIF

    ENDDO !I

    ! Return w/ success
    RC = GIGC_SUCCESS
    
  END SUBROUTINE LANDTYPE_REMAP 
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GEOS5_TO_OLSON_LANDTYPE_REMAP
!
! !DESCRIPTION: Subroutine GEOS5_TO_OLSON_LANDTYPE_REMAP is a method for
!  the ESM-coupled Grid-Independent GEOS-Chem (aka "GIGC") to remap GEOS-5's
!  land surface grid and types to the Olson land types for use by dry deposition.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GEOS5_TO_OLSON_LANDTYPE_REMAP( STATE_MET, RC )
!
! !USES:
!
    USE GIGC_ErrCode_Mod
    USE GIGC_State_Met_Mod, ONLY : MetState
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
!
! !REVISION HISTORY:
!  08 Jan 2014 - M. Long     - Initial version.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Assume success
    RC = GIGC_FAILURE

    ! Note: ILAND is initialized to 100% water earth
    !       The remapping below thus acts as a mask atop
    !       a global ocean.
    ! Note2: ITY does not seem to be updated after its initialization
    !        (set to 1d0 by default. So all ITY values are 1d0 for the
    !        moment... (ckeller, 10/11/2014)
      if( associated(State_Met%ILAND) ) then
         where ( State_Met%ITY==1d0 ) State_Met%ILAND(:,:,6 )=6
         where ( State_Met%ITY==1d0 ) State_Met%ILAND(:,:,1 )=6
         where ( State_Met%ITY==2d0 ) State_Met%ILAND(:,:,25)=25
         where ( State_Met%ITY==2d0 ) State_Met%ILAND(:,:,1 )=25
         where ( State_Met%ITY==3d0 ) State_Met%ILAND(:,:,20)=20
         where ( State_Met%ITY==3d0 ) State_Met%ILAND(:,:,1 )=20
         where ( State_Met%ITY==4d0 ) State_Met%ILAND(:,:,23)=23
         where ( State_Met%ITY==4d0 ) State_Met%ILAND(:,:,1 )=23
         where ( State_Met%ITY==5d0 ) State_Met%ILAND(:,:,2 )=2
         where ( State_Met%ITY==5d0 ) State_Met%ILAND(:,:,1 )=2
         where ( State_Met%ITY==6d0 ) State_Met%ILAND(:,:,28)=28
         where ( State_Met%ITY==6d0 ) State_Met%ILAND(:,:,1 )=28
!        where ( State_Met%ITY==7d0 ) State_Met%ILAND(:,:,53)=53
         where ( State_Met%ITY==13d0) State_Met%ILAND(:,:,3 )=3 ! 3 has equivalent properties of 70 in GEOS-Chem's Olson params
         where ( State_Met%ITY==13d0) State_Met%ILAND(:,:,1 )=3
      endif
      if( associated(State_Met%IREG) ) State_Met%IREG = 1
      if( associated(State_Met%IUSE) ) State_Met%IUSE = 1000

!------------------------------------------------------------------------
! GEOS-5     Description of vegetation type                MAP-to-Olson
! Index                                                    Index
! 1:         BROADLEAF EVERGREEN TREES                     6
! 2:         BROADLEAF DECIDUOUS TREES                     25
! 3:         NEEDLELEAF TREES                              60
! 4:         GROUND COVER                                  23
! 5:         BROADLEAF SHRUBS                              2
! 6:         DWARF TREES (TUNDRA)                          28
! 7:         Not Used in GEOS-5.
! 13:        Ice/Glacier                                   13

    RC = GIGC_SUCCESS

  END SUBROUTINE GEOS5_TO_OLSON_LANDTYPE_REMAP
!EOC
END MODULE GC_LAND_INTERFACE
#endif
