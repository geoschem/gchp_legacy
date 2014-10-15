      subroutine timebeg (task)
C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Utility to Begin Timing of Task
C
C  Argument Description                                                         
C  --------------------                                                          
C     task ...... Character String (<=*10) for Timed Process
C
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      implicit none
      character*(*) task
      include 'timer.com'

      integer n
      real    wclk
      logical first,  set
      data    first /.true./

      if(first) then

c Initialize Task Timings to Zero
c -------------------------------
      do n=1,maxtask
      cputot(n) = 0.0   ! Total Accumulated Time for Task
      cpuins(n) = 0.0   !       Accumulated Time for Task since last Print
        ntot(n) = 0     ! Total Number of  Calls for Task
        nins(n) = 0     !       Number of  Calls for Task since last Print
      enddo

c Set First Task
c --------------
          call clocks ( wclk )
          ntasks   = 1
          tasks(1) = task
         cputot(1) = -wclk
         cpuins(1) = -wclk
      else

c Set Subsequent Tasks
c --------------------
      n = 1
      do while (task.ne.tasks(n) .and. n.le.ntasks)
      n = n+1
      enddo
      if( n.gt.ntasks) then
            ntasks = ntasks+1
         if(ntasks.gt.maxtask ) then
         print *, 'Too many Timing Tasks are Set!!'
         print *, 'Maximum Number of Tasks = ',maxtask
#if mpi
         call mpi_finalize (n)
#endif
         call exit (101)
         endif
         call clocks ( wclk )
          tasks(ntasks) = task
         cputot(ntasks) = cputot(ntasks)-wclk
         cpuins(ntasks) = cpuins(ntasks)-wclk
      else
         call clocks ( wclk )
         cputot(n) = cputot(n)-wclk
         cpuins(n) = cpuins(n)-wclk
      endif
   
      endif

      first = .false.
      return
      end

      subroutine timeend (task)
C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Utility to End Timing of Task
C
C  Argument Description                                                         
C  --------------------                                                          
C     task ...... Character String (<=*10) for Timed Process
C
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      implicit none
      character*(*) task
      include 'timer.com'

      integer n
      real    wclk

      n = 1
      do while (task.ne.tasks(n) .and. n.le.ntasks)
      n = n+1
      enddo
      if( n.gt.ntasks) then
         print *, 'No Begining Timing Task found for: ',task
#if mpi
         call mpi_finalize (n)
#endif
         call exit (101)
      endif
   
      call clocks ( wclk )
      cputot(n) = cputot(n)+wclk
      cpuins(n) = cpuins(n)+wclk
        ntot(n) =   ntot(n)+1
        nins(n) =   nins(n)+1

      return
      end

      subroutine timepri (ku)
C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Utility to Print Taks Timings
C
C  Argument Description                                                         
C  --------------------                                                          
C     ku ........ Output Unit Number
C
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      implicit none
      include 'timer.com'

      integer n,ku
      real    cpuinscall,cputotcall,ratins,rattot

      write(ku,1000)
      do n=1,ntasks
                         cpuinscall = 0.0
                         cputotcall = 0.0
      if( nins(n).ne.0 ) cpuinscall = cpuins(n)/nins(n)
      if( ntot(n).ne.0 ) cputotcall = cputot(n)/ntot(n)
                             ratins = cpuins(n)/cpuins(1)*100
                             rattot = cputot(n)/cputot(1)*100
      write(ku,2000) tasks(n),cpuins(n),ratins,nins(n),cpuinscall,
     .                        cputot(n),rattot,ntot(n),cputotcall
      enddo
      write(ku,3000)

      do n=1,ntasks
      cpuins(n) = 0.0
        nins(n) = 0
      enddo

 1000 format(/,' *************************************************************************************************************',
     .       /,' ******************                    Timings on the Root Processor                        ******************',
     .       /,' *************************************************************************************************************',
     .       /,'   NAME                  CPU (sec)  %Main      Calls    CPU/Call    Total CPU   %Main      Calls  Tot CPU/Call',
     .       /,' -------------------------------------------------------------------------------------------------------------',/)
 2000 format(2x,a20,2x,f9.2,3x,f6.2,2x,i8,3x,f9.4,4x,f9.2,3x,f6.2,2x,i8,3x,f9.4)
 3000 format(/,' *************************************************************************************************************',/)

      return
      end

      subroutine clocks ( WCLK )
      real*4   wclk
      integer(4) :: ic4,crate4,cmax4
         call system_clock(count=ic4,count_rate=crate4,count_max=cmax4)
         wclk   = ic4*0.0001
      RETURN
      END
