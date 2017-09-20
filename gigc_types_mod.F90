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
  ! Prefix of the species names in the internal state and HISTORY.rc
  CHARACTER(LEN=4), PARAMETER  :: SPFX = 'SPC_'
!
! !PUBLIC TYPES:
!
!                                                                             
! !REVISION HISTORY:
!  30 Aug 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!EOC
END MODULE GIGC_TYPES_MOD
