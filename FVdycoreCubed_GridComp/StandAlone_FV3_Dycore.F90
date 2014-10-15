#define I_AM_MAIN

#include "MAPL_Generic.h"

Program StandAlone_FV3_Dycore


  use MAPL_Mod
  use FVdycoreCubed_GridComp,      only: SetServices


   implicit none

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="StandAlone_FV3_Dycore"

   logical           :: AmIRoot

   call MAPL_CAP(SetServices, AmIRoot=AmIRoot, rc=STATUS)
   VERIFY_(STATUS)

   call exit(0)

 end Program StandAlone_FV3_Dycore

!EOC

