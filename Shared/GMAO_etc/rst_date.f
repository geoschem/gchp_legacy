
       implicit none

       integer, parameter :: nargs = 1 
       character(len=255) :: arg(nargs), drst
       integer            :: ndummy, nymd, nhms, n

       do n=1,nargs
          call getarg(n,arg(n))
       enddo
       drst = trim(arg(1))

       open (10,file=drst,form='unformatted')
       read(10) ndummy, nymd, nhms
       close(10)
       write(6,'(i8.8,1x,i6.6)') nymd, nhms

       end
