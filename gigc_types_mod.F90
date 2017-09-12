!------------------------------------------------------------------------------
!
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
  ! Prefix of the species names in the internal state. This must match 
  ! the prefix given in GEOSCHEMchem_Registry.rc. 
  CHARACTER(LEN=4), PARAMETER  :: SPFX = 'SPC_'
!
! !PUBLIC TYPES:
!
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
!  30 Aug 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!EOC
END MODULE GIGC_TYPES_MOD
