module m_mapz_pert
use m_set_eta, only: set_eta
use m_set_eta, only: get_ref_plevs
use m_dyn, only: dyn_vect
use m_spline, only: spline
implicit none
private
public :: mapz_pert_set
public :: mapz_pert_interp
interface mapz_pert_set
   module procedure set_
end interface
interface mapz_pert_interp
   module procedure mapz_pert_
   module procedure vinterp_
end interface

contains

subroutine mapz_pert_ ( plevi, plevo, xpi, xpo, rc )
real(8),intent(in) :: plevi(:)
real(8),intent(in) :: plevo(:)
type(dyn_vect) :: xpi
type(dyn_vect) :: xpo
integer, intent(out) :: rc

integer ll

rc=0

! interpolate vertically
call mapz_pert_interp ( plevi, plevo, xpi%u , xpo%u , rc)
if (rc/=0) then
   print *,  'main: Error from mapz_pert_interp(u), rc=', rc
   return
endif
call mapz_pert_interp ( plevi, plevo, xpi%v , xpo%v , rc)
if (rc/=0) then
   print *,  'main: Error from mapz_pert_interp(v), rc=', rc
   return
endif
call mapz_pert_interp ( plevi, plevo, xpi%pt, xpo%pt, rc)
if (rc/=0) then
   print *,  'main: Error from mapz_pert_interp(pt), rc=', rc
   return
endif
do ll=1,xpi%grid%lm
  call mapz_pert_interp ( plevi, plevo, xpi%q(:,:,:,ll), xpo%q(:,:,:,ll), rc)
  if (rc/=0) then
     print *,  'main: Error from mapz_pert_interp(q), ll,rc=', ll,rc
     return
  endif
enddo
call mapz_pert_interp ( plevi, plevo, xpi%delp, xpo%delp, rc)
if (rc/=0) then
   print *,  'main: Error from mapz_pert_interp(delp), rc=', rc
   return
endif

! now copy 2d fields over
xpo%phis = xpi%phis
xpo%hs_stdv = xpi%hs_stdv
xpo%ts = xpi%ts
xpo%frland = xpi%frland
xpo%frlandice = xpi%frlandice
xpo%frlake = xpi%frlake
xpo%frocean = xpi%frocean
xpo%frseaice = xpi%frseaice
xpo%ps = xpi%ps

end subroutine mapz_pert_
subroutine set_ (nlevs,plevs)
integer,intent(in) :: nlevs
real(8),intent(inout) :: plevs(nlevs)

real(8), allocatable:: ak(:),bk(:)
real(8):: ptop, pint
integer :: ks

allocate(ak(nlevs+1),bk(nlevs+1))
call set_eta ( nlevs, ks, ptop, pint, ak, bk )
call get_ref_plevs ( ak, bk, ptop, plevs )
deallocate(ak,bk)

end subroutine set_

subroutine vinterp_ ( plevi, plevo, yi, yo, rc)
real(8),intent(in)   :: plevi(:)
real(8),intent(in)   :: plevo(:)
real(8),intent(in)   :: yi(:,:,:)
real(8),intent(inout):: yo(:,:,:)
integer,intent(out)  :: rc

integer imi,jmi,kmi
integer imo,jmo,kmo
integer ii,jj

imi=size(yi,1); imo=size(yo,1)
jmi=size(yi,2); jmo=size(yo,2)
kmi=size(yi,3); kmo=size(yo,3)

rc=0
if (size(plevi)/=kmi .or. size(plevo)/=kmo .or. &
    imi/=imo         .or. jmi/=jmo         ) then
   rc=1
   return
endif

! This is nuts w/o mpi or co-arrays ...
! -------------------------------------
do jj=1,jmi
do ii=1,imi
   call spline( plevi, plevo, yi(ii,jj,:), yo(ii,jj,:) ) 
enddo
enddo
end subroutine vinterp_

end module m_mapz_pert
