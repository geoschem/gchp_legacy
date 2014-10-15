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
  PUBLIC :: GEOS5_TO_OLSON_LANDTYPE_REMAP
!
! !PRIVATE MEMBER FUNCTIONS:
!

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

END MODULE GC_LAND_INTERFACE
#endif




