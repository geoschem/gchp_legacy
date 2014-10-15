!
!  Simple testcode for the cyclic option of GetVarT.
!

 program ut

 integer, parameter :: imx = 288, jmx = 181
 real :: grid(imx,jmx)
 integer :: im = imx, jm = jmx

 character(len=255) :: &
 fname = '/share/dasilva/fvInput/fvchem/c/martin.biomass_src.sfc.1971.hdf', &
 vname = 'biomass'

 integer :: READONLY = 1
 integer :: fid, rc, kbeg = 0, kount = 1, loop
 logical :: cyclic = .true.
 real :: fac

 print *, 'Enter 1 for cyclic mode'
 read *, icyc
 if ( icyc .eq. 1 ) then
      cyclic = .true.
 else
      cyclic = .false.
 end if

 fac = 1000000000.
 i = 25
 j = 101 ! africa
 do loop = 1, 100

    print *
    print *, 'Reading '//trim(vname)//' from '//trim(fname)
    print *, 'Enter nymd, nhms'
    read  *, nymd, nhms
    
    call  GFIO_Open ( fname, READONLY, fid, rc )
    !    print *, 'Open, rc = ', rc
       
    if ( nymd < 0 ) then 
         call exit(0)
    else if ( nymd == 0 ) then

         do m = 100, 1200, 100
            do n = 1, 28
            nymd = 19710000 + m + n
            call GFIO_GetVarT1 ( fid, vname, nymd, nhms, &
                 im, jm, kbeg, kount, grid, rc, &
                 cyclic, fid )
            print *, nymd, nhms,  grid(i,j)*fac
         end do
         end do

    else      

       call GFIO_GetVarT1 ( fid, vname, nymd, nhms, &
            im, jm, kbeg, kount, grid, rc, &
            cyclic, fid )
       !    print *, 'GetVarT1, rc = ', rc
       
       print *, 'min/max = ', minval(grid*fac), maxval(grid*fac)

    end if
   
       call GFIO_Close ( fid, rc )
       print *, 'Close, rc = ', rc

 end do

end program ut
