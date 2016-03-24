program dyn_iupd

use m_dyn, only: dyn_vect
use m_dyn, only: dyn_init
use m_dyn, only: dyn_clean
use m_dyn, only: dyn_get
use m_dyn, only: dyn_getdim
use m_dyn, only: dyn_put
use m_maph_pert, only: h_map_pert
use m_die, only: die

implicit none

character(len=*), parameter :: myname="dyn_iupd"

character(len=256)  ifile
character(len=256)  iofile
character(len=256)  argv
integer, parameter :: dyntype=5
integer :: nymd, nhms, freq, rc
integer :: im1,jm1,km1,lm1
integer :: im2,jm2,km2,lm2
integer :: iargc,iarg, argc, k, ks, lm
real :: ptop,pint
real,allocatable :: ak(:),bk(:)
type(dyn_vect) :: xi
type(dyn_vect) :: yi
type(dyn_vect) :: zi
logical, parameter :: pncf=.true.

argc = iargc()
if ( argc < 2 ) then
   print *, "Usage: reset_time.x fname yyyymmdd hhmmss time_inc"
   print *, "   "

   print *, "Reset begin_date, begin_time and time_increment."
   print *, "Use negative number to skip. For example, "
   print *, "   "
   print *, "reset_time.x file_name -9 60000 -9 "
   print *, "will only modify begin_time"
   print *, "Please note: for HDF-EOS format, TIME:EOSGRID will"
   print *, "             be modified, but Time will be NOT."
   stop
end if

iarg = 1
call GetArg ( iarg, ifile )
print * , "input increment: ", trim(ifile)
iarg = iarg + 1
call GetArg ( iarg, iofile )
print * , "increment to update: ", trim(iofile)

call dyn_getdim ( trim(ifile) , im1, jm1, km1, lm1, rc )
call dyn_getdim ( trim(iofile), im2, jm2, km2, lm2, rc )
print *, im1, jm1, km1, lm1
print *, im2, jm2, km2, lm2

! Number of levels and tracers must equal for now
! -----------------------------------------------
if ( km1 /= km2 ) then
   call die (myname,'lev tracers not consistent (error), aborting ... ')
endif
if ( lm1 /= lm2  ) then
   print *, trim(myname),' number of tracers not consistent (warning), aborting ... '
endif

! Read latest increment
! ---------------------
call dyn_get ( trim(iofile),  nymd, nhms, yi, rc, freq=freq, vectype=dyntype, pncf=pncf )

! Handle horizontal resolution difference ...
! will interpolate fields of old increment (first file)
! to the resulution of latest increments (second file)
! ----------------------------------------------------
if ( im1 /= im2 .or. &  ! when horizontal res not consistent, 
     jm1 /= jm2  ) then ! interpolate to hi-res

!    Read previous increment
!    ----------------------
     call dyn_get ( trim(ifile), nymd, nhms, zi, rc, freq=freq, vectype=dyntype, pncf=pncf )

!    Initialize dimension of output (interpolated) vector
!    ----------------------------------------------------
     call dyn_init ( im2, jm2, km2, lm2, xi, rc, &
                     zi%grid%ptop, zi%grid%ks, zi%grid%ak, zi%grid%bk, vectype=dyntype )
          if ( rc/=0 ) then
               call die (myname, ': Error initializing dyn vector(x_e)')
          endif
     print *, 'debug ',xi%grid%lm,yi%grid%lm,zi%grid%lm

!    Interpolate to required resolution
!    ----------------------------------
     lm1=min(zi%grid%lm,xi%grid%lm)
     xi%grid%lm=lm1
     zi%grid%lm=lm1
     call h_map_pert ( zi, xi, rc )
          if ( rc/=0 ) then
               call dyn_clean ( zi )
               call dyn_clean ( xi )
               print *, 'h_map error code = ', rc
               call die(myname,' failed in h_map')
          else
               call dyn_clean ( zi )
          endif

else
!    Read previous increment
!    -----------------------
     call dyn_get ( trim(ifile), nymd, nhms, xi, rc, freq=freq, vectype=dyntype, pncf=pncf )
     lm1=min(yi%grid%lm,xi%grid%lm)
endif

! update
yi%u = yi%u + xi%u
yi%v = yi%v + xi%v
yi%pt= yi%pt+ xi%pt
yi%q(:,:,:,1:lm1) = yi%q(:,:,:,1:lm1) + xi%q(:,:,:,1:lm1)
yi%ps= yi%ps+ xi%ps
yi%ts= yi%ts+ xi%ts

! reconstruct delp increment from ps increment
allocate(ak(km2+1),bk(km2+1))
do k=1,km2
   yi%delp(:,:,k)= (yi%grid%bk(k+1) - yi%grid%bk(k))*yi%ps(:,:)
enddo
deallocate(ak,bk)

! write out updated incremet - will overwrite original file
call dyn_put ( trim(iofile), nymd, nhms, 0, yi, rc, new=.true., freq=freq, vectype=dyntype )

! clean up
call dyn_clean ( yi )
call dyn_clean ( xi )

end program dyn_iupd
