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

   include "mpif.h"

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="GIGC_Main"

   logical           :: AmIRoot

   ! For unknown reasons with MVAPICH, the MPI procs were getting ahead of themselves.
   ! Causing the mpi system to fail. So, the mpi_init() call was moved here
   ! instead of in MAPL_Cap.F90 to ensure that the init was the 1st thing to
   ! happen. MLong. 8/21/15
   call mpi_init(status)
   VERIFY_(STATUS) 
   call MAPL_CAP(ROOT_SetServices, AmIRoot=AmIRoot, CommIn=MPI_COMM_WORLD, rc=STATUS)
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
