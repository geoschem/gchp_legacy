!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !INCLUDE: Includes_Before_HEMCO.H
!
! !DESCRIPTION: This include file contains the array assignments that need
!  to be made BEFORE the call to the HEMCO run method of the 
!  HEMCO\_GridCompMod.F90 code.  These array assignments take data out 
!  of the import and internal states and saves them into derived-type objects
!  that are used to pass information between HEMCO and the ESMF interface.
!\\
!\\
!  These assignments were placed into this separate include file to avoid 
!  bogging down the HEMCO\_GridCompMod.F90 module.
!\\
!\\
! !REVISION HISTORY: 
!  04 Apr 2014 - C. Keller   - Initial version for HEMCO
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Two-dimensional fields
  !=========================================================================

  IF ( ExtOpt%ALBD%DoUse ) THEN
     ExtOpt%ALBD%Arr%Val => ALBD
  ENDIF
  IF ( ExtOpt%WLI%DoUse ) THEN
     ExtOpt%WLI%Arr%Val => LWI
  ENDIF
  IF ( ExtOpt%TSKIN%DoUse ) THEN
     ExtOpt%TSKIN%Arr%Val => LWI
  ENDIF
  IF ( ExtOpt%U10M%DoUse ) THEN
     ExtOpt%U10M%Arr%Val => U10M
  ENDIF
  IF ( ExtOpt%V10M%DoUse ) THEN
     ExtOpt%V10M%Arr%Val => V10M
  ENDIF

  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================

  !=========================================================================
  ! Three-dimensional fields on level centers
  !=========================================================================



