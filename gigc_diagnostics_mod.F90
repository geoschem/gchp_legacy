#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_diagnostics_mod
!
! !DESCRIPTION: Module GIGC\_DIAGNOSTICS\_MOD contains the 
!  methods to register the standard diagnostics enabled by users
!  within input.geos directly as HISTORY imports and automatically
!  write them into the history files.
!  
!  The procedures here define a collection within MAPL_History
!  and assign to it the identical properties - to the extent
!  possible - of those written to standard GEOS-Chem output
!  files. As of writing, GEOS-Chem still primarily uses bpch
!  binary file formats.
!\\
!\\
! !INTERFACE: 
!
MODULE GIGC_Diagnostics_Mod
!
! USES:
!
  USE ESMF, ONLY : ESMF_MAXSTR, ESMF_GridComp, ESMF_State
  USE MAPL_Mod

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! !PUBLIC DATA MEMBERS:
!

  ! Error success/failure definitions
  INTEGER, PARAMETER, PUBLIC  :: DIAG_SUCCESS = 0
  INTEGER, PARAMETER, PUBLIC  :: DIAG_FAILURE = -999

!
! !PRIVATE DATA MEMBERS:
!

!
! !REMARKS:
!  -----------------------------------------------------------------------
!                                                                             
! !REVISION HISTORY:
!  18 Dec 2014 - M. Long     - Initial Version
!  12 Aug 2015 - M. Long     - Initial modification to link C. Keller's
!                              diagnostics routines, input.geos settings,
!                              and MAPL_History
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
  SUBROUTINE GIGC_DefineHistoryContainer
  END SUBROUTINE GIGC_DefineHistoryContainer
  SUBROUTINE GIGC_RegisterGCDiagnostics
  END SUBROUTINE GIGC_RegisterGCDiagnostics
!EOC
END MODULE GIGC_Diagnostics_Mod
