!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_types_mod.F90
!
! !DESCRIPTION: Module GIGC\_Types\_Mod contains derived type definitions
!  and global parameters used in GEOS-Chem with the High Performance option. 
!\\
!\\
! !INTERFACE: 
!
MODULE GIGC_TYPES_MOD
!
! USES:
!
  !USE HCO_Error_Mod
  !USE HCO_Arr_Mod

  IMPLICIT NONE
  PUBLIC 
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! !PRIVATE MEMBER FUNCTIONS:
!
!
! !PUBLIC PARAMETERS:
!
  ! Maximum length of option name
  INTEGER, PARAMETER       :: OPTLEN = 1023
!
! !PUBLIC TYPES:
!
  !-------------------------------------------------------------------------
  ! DataCont: Derived type definition for generic data container (not used)
  !-------------------------------------------------------------------------
  TYPE :: DataCont

     ! Container information 
     CHARACTER(LEN= 63)          :: cName          ! Cont. name
     INTEGER                     :: cID            ! Cont. ID
     INTEGER                     :: targetID       ! target ID
     INTEGER                     :: DctType        ! Data type
     !TYPE(FileData),     POINTER :: Dta            ! data information
     INTEGER                     :: DtaHome        ! Home cont for Dta?
     CHARACTER(LEN= 31)          :: SpcName        ! Species Name 
     INTEGER                     :: HcoID          ! HEMCO species ID
     INTEGER                     :: ExtNr          ! Extension #
     INTEGER                     :: Cat            ! Category
     INTEGER                     :: Hier           ! Hierarchy
     INTEGER                     :: ScalID         ! Scale factor ID
     INTEGER                     :: Oper           ! Operator
     INTEGER                     :: nScalID        ! # of scale factor IDs 
     INTEGER,            POINTER :: Scal_cID(:)    ! assoc. scalefactor IDs
     LOGICAL                     :: Scal_cID_set   ! cIDs or scalIDs 
  END TYPE DataCont

  !-------------------------------------------------------------------------
  ! ListCont: Derived type definition for generic list object (not used)
  !-------------------------------------------------------------------------
  TYPE :: ListCont
     TYPE(DataCont),     POINTER :: Dct
     TYPE(ListCont),     POINTER :: NextCont
  END TYPE ListCont

  !-------------------------------------------------------------------------
  ! cIdListPnt: Derived type definition for pointing to list containers
  !-------------------------------------------------------------------------
  TYPE :: cIDListPnt
     TYPE(DataCont),     POINTER :: PNT ! Pointer to list container
  END TYPE cIDListPnt

  !-------------------------------------------------------------------------
  ! Variables to store (unique) species names
  !-------------------------------------------------------------------------
  TYPE :: SpecNameCont
     CHARACTER(LEN=31)           :: SpecName
     TYPE(SpecNameCont), POINTER :: NEXT
  END TYPE SpecNameCont

  !-----------------------------------------------------------------
  ! Geographic location
  !-----------------------------------------------------------------
  TYPE :: GC_GEOLOC
     REAL*8           :: LON          ! Longitude [degrees]
     REAL*8           :: LAT          ! Latitude  [degrees]
     REAL*8           :: LOCALTIME    ! Local solar time [hrs]

  END TYPE GC_GEOLOC
!                                                                             
! !REVISION HISTORY:
!  30 Aug 2017 - E. Lundgren - Initial version. Replace gigc_type_mod.F90
!                              which is mostly outdated. Only GC_GEOLOC
!                              is kept from that file.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE TYPES/ARGUMENTS:
!

  !----------------------------------------------------------------
  ! MODULE ROUTINES follow below
  !----------------------------------------------------------------

CONTAINS

!EOC
END MODULE GIGC_TYPES_MOD
