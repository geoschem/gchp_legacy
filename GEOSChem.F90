! 

! *********************************************************************
! *****                      Main Program                          ****
! *****                                                            ****
! *****                                                            ****
! *********************************************************************

#define I_AM_MAIN

#include "MAPL_Generic.h"

Program GIGC_Main


   use MAPL_Mod
   use GIGC_GridCompMod, only:  ROOT_SetServices => SetServices

   implicit none

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="GIGC_Main"

   logical           :: AmIRoot

   call MAPL_CAP(ROOT_SetServices, AmIRoot=AmIRoot, rc=STATUS)
   VERIFY_(STATUS)

   if( STATUS.eq.0 ) then
      if (AmIRoot) then
         close(99)
         open (99,file='EGRESS',form='formatted')
         close(99)
      end if
   endif

   call exit(0)

 end Program GIGC_Main

!EOC
