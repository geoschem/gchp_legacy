
       implicit none

       integer, parameter :: nargs = 3 
       integer, parameter :: ndummy = 1000
       character(len=255) :: arg(nargs)
       character(len=255) :: fname
       integer            :: nymd, nhms, n, argc, iargc

       argc =  iargc()
       if ( argc .lt. 1 ) then
            print*, 'GEOS-5 temporary and specific program '
            print*, 'to create d_rst file with date and time'
            print*, '       Usage:  mkdrstdate.x nymd nhms'
            call exit(0)
       endif
       do n=1,argc
          call getarg(n,arg(n))
       enddo
       read(arg(1), * ) nymd
       read(arg(2), * ) nhms

       if (argc==3) then
           fname = arg(3)
           open (10,file=trim(fname),form='unformatted')
       else
           open (10,file='d_rst',form='unformatted')
       endif
       write(10) ndummy, nymd, nhms
       close(10)

       end
