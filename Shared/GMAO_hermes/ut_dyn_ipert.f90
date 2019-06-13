program ut_dyn_ipert
use m_dyn, only: dyn_init
use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_put
use m_dyn, only: dyn_clean
use m_set_eta, only: set_eta
use m_mapz_pert, only: mapz_pert_set
use m_mapz_pert, only: mapz_pert_interp
implicit none

type(dyn_vect) :: xpi  ! input vector
type(dyn_vect) :: xpo  ! output vector

integer ll,kmi,kmo
integer nymd, nhms, ks, freq, rc
real ptop,pint
integer :: dyntype=5
character(len=255) :: ipfname
character(len=255) :: opfname

real,allocatable,dimension(:) :: ak,bk
real,allocatable,dimension(:) :: plevi,plevo

kmo = 132

ipfname = 'old.nc4'
opfname = 'new.nc4'
call dyn_get ( ipfname, nymd, nhms, xpi, rc, timidx=1, freq=freq, vectype=dyntype, pncf=.true. )

kmi=xpi%grid%km

allocate(ak(kmo+1),bk(kmo+1))
call set_eta ( kmo,ks,ptop,pint,ak,bk )
call dyn_init ( xpi%grid%im, xpi%grid%jm, kmo, xpi%grid%lm, xpo, rc,  &
                vectype=dyntype, ptop=ptop, ks=ks, ak=ak, bk=bk )
     if (rc/=0) then
        print *,  'main: Error initializing dyn vector(xpo), rc=', rc
        call exit(1)
    endif
deallocate(ak,bk)

! set pressure levels
allocate(plevi(kmi),plevo(kmo))
call mapz_pert_set (kmi,plevi)
call mapz_pert_set (kmo,plevo)

! interpolate vertically
call mapz_pert_interp ( plevi, plevo, xpi, xpo, rc)
   if (rc/=0) then
      print *,  'main: Error from mapz_pert_interp(xpo), rc=', rc
      call exit(1)
   endif

! write out result
call dyn_put ( trim(opfname), nymd, nhms, 0, xpo, rc, freq=freq, vectype=dyntype )

! clean up
deallocate(plevi,plevo)
call dyn_clean(xpi)
call dyn_clean(xpo)

end program ut_dyn_ipert
