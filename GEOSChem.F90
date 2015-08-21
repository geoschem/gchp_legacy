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

   pause(0.05) ! With MVAPICH, the MPI procs were getting ahead of themselves.
               ! This pause siezes operations just enough to let the prog. spin up.
               ! It may have been lib or machine specific, but it wasn't
               ! investigated further. M.Long - 8/21/15
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
