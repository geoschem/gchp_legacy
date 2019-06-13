!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_realkinds - real KIND definitions
!
! !DESCRIPTION:
!
! !INTERFACE:

module m_realkinds
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
      implicit none
      private	! except

      public :: kind_r4		! real*4
      public :: kind_r8		! real*8
      public :: kind_r		! default real
      public :: SP		! default REAL
      public :: DP		! default DOUBLE_PRECISION

      integer,parameter :: SP = REAL32
      integer,parameter :: DP = REAL64

      integer,parameter :: kind_r4=SP
      integer,parameter :: kind_r8=DP
      integer,parameter :: kind_r =kind(1.)

! !REVISION HISTORY:
! 	19Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_realkinds'

end module m_realkinds
