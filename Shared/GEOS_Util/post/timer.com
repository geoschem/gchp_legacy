
C **********************************************************************
C *****                   GEOS Timing Parameters                   *****
C **********************************************************************

      integer          maxtask
      parameter       (maxtask=50)

      character*20     tasks
      integer          ntasks
      integer*8        ntot,   nins
      real             cputot, cpuins
      integer          pad

      common /time_parm/ pad, ntasks
      common /time_parm/  tasks(maxtask) 
      common /time_parm/ cputot(maxtask), ntot(maxtask) 
      common /time_parm/ cpuins(maxtask), nins(maxtask) 
