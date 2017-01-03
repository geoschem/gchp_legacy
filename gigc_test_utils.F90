!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_test_utils
!
! !DESCRIPTION: Module GIGC\_Test\_Utils contains debugging code for the
!  ESMF interface to the Grid-Independent GEOS-Chem (aka "GIGC").
!\\
!\\
! !INTERFACE:
!
MODULE GIGC_Test_Utils
!
! !USES:
!
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: GIGC_Dump_Config
!
! !REMARKS:
!  Additional debugging routines can be added here as necessary
!
! !REVISION HISTORY: 
!  18 Oct 2012 - M. Long     - Initial version
!  18 Oct 2012 - R. Yantosca - Added ProTeX headers
!  22 Oct 2012 - R. Yantosca - Renamed to gigc_test_utils.F90
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
! !IROUTINE: gigc_dump_config
!
! !DESCRIPTION: Prints out information about GEOS-Chem options.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Dump_Config( am_I_Root )
!
! !USES:
!
!    USE LOGICAL_MOD
!
! !INPUT PARAMETERS: 
!
    LOGICAL, INTENT(IN) :: am_I_root
! 
! !REVISION HISTORY: 
!  18 Oct 2012 - M. Long     - Initial version
!  18 Oct 2012 - R. Yantosca - Added ProTeX headers
!  22 Oct 2012 - R. Yantosca - Renamed to GIGC_Dump_Config
!EOP
!------------------------------------------------------------------------------
!BOC
   
    IF ( am_I_Root ) THEN 

    ENDIF

  END SUBROUTINE GIGC_Dump_Config
!EOC
END MODULE GIGC_Test_Utils
