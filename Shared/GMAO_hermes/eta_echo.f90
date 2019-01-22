program eta_echo
use m_set_eta, only: set_eta
use m_set_eta, only: get_ref_plevs
use m_spline, only: spline
implicit none

integer :: nlevs,mlevs
integer :: k,km,ks,ii
logical :: bot2top, lsingle, lplevs
real(8) :: ptop8, pint8
real(8),allocatable :: ak8(:),bk8(:)
real(4),allocatable :: hloc(:),zloc(:),betac(:),betae(:)
real(4) :: ak4,bk4
real(4) :: pbottom
character(len=255) :: locfname

call init_

allocate(ak8(nlevs+1),bk8(nlevs+1))

call set_eta ( nlevs, ks, ptop8, pint8, ak8, bk8 )

if (trim(locfname) .ne. "NULL" ) then
   call get_locs_()
endif

if (lplevs) then
    call write_plevs_()
endif

call write_akbk_()

deallocate(ak8,bk8)

contains

subroutine init_
  integer argc, iarg
  integer iargc
  character(len=255) :: argv
  argc = iargc()
  if ( argc < 1 ) then
     print *, "Usage: eta_echo.x [options] nlevs"
     print *, "   "
     print *, " Echo ak/bks defining levels" 
     print *, "   "
     print *, " Options  "
     print *, "   "
     print *, " -bot2top    - echoes from bottom to top"
     print *, " -loc FILE   - reads localization file from GSI"
     print *, " -p0  PBOT   - bottom pressure level (hPa)"
     print *, " -mlevs LEVS - interpolate to this level setting"
     print *, " -plevs      - echoes pressure levels"
     print *, " -r4         - single precision"
     print *, "   "
     stop
  end if
  bot2top = .false.
  lsingle = .false.
  lplevs = .false.
  locfname = "NULL"
  pbottom = -999.
  mlevs=0
  iarg = 1
  do while (iarg<=argc)
     call GetArg ( iarg, argv )
     select case (argv)
     case ("-bot2top")
         bot2top = .true.
     case ("-p0")
         iarg = iarg + 1
         call GetArg ( iarg, argv )
         read(argv,*) pbottom
         pbottom=pbottom*100.
     case ("-mlevs")
         iarg = iarg + 1
         call GetArg ( iarg, argv )
         read(argv,*) mlevs
     case ("-plevs")
         lplevs = .true.
     case ("-r4")
         lsingle = .true.
     case ("-loc")
         iarg = iarg + 1
         call GetArg ( iarg, argv )
         locfname = trim(argv)
     case default
        read(argv,*) nlevs
     end select
     iarg = iarg+1
  end do

end subroutine init_

subroutine get_locs_
   bot2top=.true.
   allocate(hloc(nlevs),zloc(nlevs),betac(nlevs),betae(nlevs))
   open(10,file=trim(locfname))
   read(10,*) km
   if(km/=nlevs) then
      print *, 'Inconsistent levels: km/nlevs', km, nlevs
      print *, 'Aborting ...'
      call exit(1)
   else
      do k=1,nlevs
         read(10,*) hloc(k), zloc(k), betac(k), betae(k)  
      enddo
   endif
end subroutine get_locs_

subroutine write_plevs_
    real(8),allocatable :: levn(:)
    real(8),allocatable :: levm(:)
    real(8),allocatable :: xak(:),xbk(:)
    real(4),allocatable :: hlocm(:), zlocm(:), betacm(:), betaem(:)
    integer :: ksm
    print*
    print*, "Ref. pressure levels"
    print*, "===================="
    allocate(levn(nlevs))
    if(pbottom>0.) then
       call get_ref_plevs ( ak8, bk8, ptop8, levn, p0=real(pbottom,8) )
    else
       call get_ref_plevs ( ak8, bk8, ptop8, levn )
    endif
    if (trim(locfname) .eq. "NULL" ) then  
       if (bot2top) then
          do k = 1, nlevs
             ii=nlevs-k+1
             write(6,'(i5,2x,f20.10)') k,levn(ii)
          enddo
       else
          do k = 1, nlevs
             write(6,'(i5,2x,f20.10)') k,levn(k)
          enddo
       endif
    else
       levn=levn(nlevs:1:-1)
       do k = 1, nlevs
          write(6,'(i5,2x,f20.10,2x,f7.1,3x,f4.1,2(3x,f7.4))') k,levn(k),hloc(k), zloc(k), betac(k), betae(k)
       enddo
       call loc2grads_(real(levn,4),hloc, zloc, betac, betae)

       if (mlevs>0) then
          allocate(levm(mlevs))
          allocate(xak(mlevs+1),xbk(mlevs+1))
          call set_eta ( mlevs, ksm, ptop8, pint8, xak, xbk )
          if(pbottom>0.) then
             call get_ref_plevs ( xak, xbk, ptop8, levm, p0=real(pbottom,8) )
          else
             call get_ref_plevs ( xak, xbk, ptop8, levm )
          endif
          deallocate(xak,xbk)
          allocate(hlocm(mlevs), zlocm(mlevs), betacm(mlevs), betaem(mlevs))
          levm=levm(mlevs:1:-1)
          call spline ( real(levn,4), real(levm,4), hloc, hlocm )
          call spline ( real(levn,4), real(levm,4), zloc, zlocm )
          call spline ( real(levn,4), real(levm,4), betac, betacm )
          call spline ( real(levn,4), real(levm,4), betae, betaem )
          call loc2grads_(real(levm,4),hlocm, zlocm, betacm, betaem)
          write(6,*)
          write(6,*)
          do k = 1, mlevs
             write(6,'(i5,2x,f20.10,2x,f7.1,3x,f4.1,2(3x,f7.4))') k,levm(k),hlocm(k), zlocm(k), betacm(k), betaem(k)
          enddo
          deallocate(hlocm, zlocm, betacm, betaem)
          deallocate(levm)
       endif

       deallocate(hloc, zloc, betac, betae)
    endif
    deallocate(levn)
end subroutine write_plevs_

subroutine write_akbk_

print*
print*, "AK and BK"
print*, "========="
if (bot2top) then
   do ii=nlevs+1,1,-1
      if(lsingle) then
         ak4=ak8(ii); bk4=bk8(ii)
         write(6,104) nlevs-ii+2,ak4,bk4
      else
         write(6,108) nlevs-ii+2,ak8(ii),bk8(ii)
      endif
   enddo
else
   do ii=1,nlevs+1
      if(lsingle) then
         ak4=ak8(ii); bk4=bk8(ii)
         write(6,104) ii,ak4,bk4
      else
         write(6,108) ii,ak8(ii),bk8(ii)
      endif
   enddo
endif
104 format(4x,i6,2x,2(f13.6,2x))
108 format(4x,i6,2x,1p,2(e21.14,2x))
end subroutine write_akbk_

subroutine loc2grads_(lev,hloc, zloc, betac, betae)
 use m_ioutil, only : luavail
 implicit none
 real(4),intent(in) :: lev(:),hloc(:),zloc(:),betac(:),betae(:)
 integer lo,km
 character(len=80) :: fctl,fgrd
 lo=luavail()
 km=size(lev)
 write(fgrd,'(a,i3.3,a)') 'hybens_info_', km, '.grd'
 open(lo,file=trim(fgrd),form='unformatted',convert='little_endian')
 do k=1,km
    write(lo) hloc(k)
 enddo
 do k=1,km
    write(lo) zloc(k)
 enddo
 do k=1,km
    write(lo) betac(k)
 enddo
 do k=1,km
    write(lo) betae(k)
 enddo
 close(lo)
 write(fctl,'(a,i3.3,a)') 'hybens_info_', km, '.ctl'
 open(lo,file=trim(fctl),form='formatted')
 write(lo,'(3a)') 'DSET ', '^', trim(fgrd)
 write(lo,'(a)') 'OPTIONS sequential'
 write(lo,'(a)') 'TITLE betas_locs'
 write(lo,'(a,1p,e11.4)') 'UNDEF ', 1.e15
 write(lo,'(a)') 'XDEF 1 LINEAR 1 1'
 write(lo,'(a)') 'YDEF 1 LINEAR 1 1'
 write(lo,'(a,i6,a)') 'ZDEF ', km, ' levels'
 write(lo,'(5x,4f12.6)') lev
 write(lo,'(a)') 'TDEF 1 LINEAR 12:00Z04Jul1776 6hr'
 write(lo,'(a,i6)') 'VARS ', 4
 write(lo,'(a,i6,a)') 'hloc ', km, ' 0 hloc'
 write(lo,'(a,i6,a)') 'zloc ', km, ' 0 zloc'
 write(lo,'(a,i6,a)') 'bc   ', km, ' 0 bc'
 write(lo,'(a,i6,a)') 'be   ', km, ' 0 be'
 write(lo,'(a)') 'ENDVARS'
 close(lo)
end subroutine loc2grads_

end program eta_echo
