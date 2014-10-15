#define I_AM_MAIN

#include "MAPL_Generic.h"

Program StandAlone_DynAdvCore


  use MAPL_Mod
  use StandAlone_DynAdvCore_GridCompMod,      only: SetServices


   implicit none

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="StandAlone_DynAdvCore"

   logical           :: AmIRoot

   call MAPL_CAP(SetServices, AmIRoot=AmIRoot, rc=STATUS)
   VERIFY_(STATUS)

   call exit(0)

 end Program StandAlone_DynAdvCore

!EOC

