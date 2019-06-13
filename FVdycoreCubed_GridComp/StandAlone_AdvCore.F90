#define I_AM_MAIN

#include "MAPL_Generic.h"
#define _RC rc=status); _VERIFY(status

program StandAlone_AdvCore
   use MAPL_Mod
   use AdvCore_GridCompMod, only: SetServices
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
        description = 'FV Standalone dvCore', &
        authors     = 'S.J. Lin, R. Rood, W. Putman')

   cap = MAPL_Cap('Standalone FV3 AdvCore', SetServices)
   call cap%add_command_line_options(options, _RC)
   call cap%run(options, _RC)

end program StandAlone_AdvCore

!EOC


