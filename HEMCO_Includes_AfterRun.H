!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !INCLUDE: Includes_After_HEMCO.H
!
! !DESCRIPTION: This include file contains the array assignments that need
!  to be made AFTER the call to the HEMCO run methods of the 
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

  HcoState%Grid%AREA_M2    => NULL()

  ExtOpt%ALBD%Arr%Val   => NULL() 
  ExtOpt%WLI%Arr%Val    => NULL() 
  ExtOpt%TSKIN%Arr%Val  => NULL() 
  ExtOpt%U10M%Arr%Val   => NULL() 
  ExtOpt%V10M%Arr%Val   => NULL() 

  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================

  !=========================================================================
  ! Three-dimensional fields on level centers
  !=========================================================================

  HcoState%Grid%BXHEIGHT_M => NULL()

  ExtOpt%PEDGE%Arr%Val     => NULL() 
  ExtOpt%PCENTER%Arr%Val   => NULL() 


