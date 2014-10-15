program dyn2real_eta

use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_put
use m_dyn, only: dyn_clean

use m_topo_remap, only: dyn_real_eta

use m_die, only: die
implicit none

integer  im, jm, km, ifile
integer  freq, dyntype, rc
integer  nymd, nhms
type(dyn_vect) x_b

character(len=*), parameter :: myname = 'dyn_check'
character(len=256) :: files(2)

call init_

dyntype = 5

! Get first dyn-vector (to tap on target pressure)
! ------------------------------------------------
  call dyn_get ( files(1), nymd, nhms, x_b, rc, timidx=1, freq=freq, vectype=dyntype )
  if ( rc .ne. 0 ) then
     call die(myname,'cannot read target file')
  end if

! Get dims ...
! ------------
  im = x_b%grid%im
  jm = x_b%grid%jm
  km = x_b%grid%km

! First check ps-delp consistency ...
! -----------------------------------
  call check_ps_consistency_(x_b%grid%ptop,x_b%ps,x_b%delp,files(1))

! Remap ...
! ---------
  call dyn_real_eta ( x_b, dyntype )

! Finally check ps-delp consistency on remapped fields
! ----------------------------------------------------
  call check_ps_consistency_(x_b%grid%ptop,x_b%ps,x_b%delp,files(2))

! Write out remapped fields
! -------------------------
  call dyn_put ( trim(files(2)), nymd, nhms, 0, x_b, rc, freq=freq, vectype=dyntype)

! Clean up
! --------
  call dyn_clean (x_b)

contains

subroutine init_

character(len=256)  argv
integer :: argc,iarg,iargc

argc = iargc()
if ( argc < 1 ) then
     print *
     print *, "Usage: dyn_realeta.x dyn_input dyn_output"
     print *
     stop
endif

iarg = 1
call GetArg ( iarg, files(1) )
iarg = iarg + 1
call GetArg ( iarg, files(2) )

end subroutine init_

subroutine check_ps_consistency_(ptop,ps,delp,fname)

character(len=*),intent(in) :: fname
real,intent(in) :: ptop
real,intent(in) :: delp(:,:,:)
real,intent(in) :: ps(:,:)
integer i,j,k
real    ps_diff
real,allocatable,dimension(:,:) :: xps

allocate(xps(im,jm))

! Calculate ps based on delp and ptop
xps = ptop ! x_b%grid%ak(1)
do k = 1,km
do j = 1,jm
do i = 1,im
   xps(i,j) = xps(i,j) + delp(i,j,k)
end do
end do
end do

! Compare w/ dyn-vector ps
ps_diff =  sum(abs(xps-ps))

print *, 'PS diff: ', ps_diff, ' for file: ', trim(fname)

deallocate(xps)

end subroutine check_ps_consistency_

end program dyn2real_eta
