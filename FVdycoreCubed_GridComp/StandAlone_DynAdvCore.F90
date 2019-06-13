#define I_AM_MAIN

#include "MAPL_Generic.h"
#define _RC rc=status); _VERIFY(status

program StandAlone_DynAdvCore
   use MAPL_Mod
  use StandAlone_DynAdvCore_GridCompMod, only: SetServices
   use MPI
   use FLAP

   implicit none

!EOP

!EOC

   character(*), parameter :: IAM = __FILE__

   type (MAPL_Cap) :: cap
   type (command_line_interface) :: options
   integer :: status

   call options%init( &
        description = 'FV Standalone DyAdvCore', &
        authors     = 'S.J. Lin, R. Rood, W. Putman')

   cap = MAPL_Cap('Standalone FV3 DynAdvCore', SetServices)
   call cap%add_command_line_options(options, _RC)
   call cap%run(options, _RC)

 end Program StandAlone_DynAdvCore

!EOC




