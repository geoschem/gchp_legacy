program dyn_boot
use m_dyn, only: dyn_vect
use m_dyn, only: dyn_get
use m_dyn, only: dyn_init
use m_dyn, only: dyn_put
use m_dyn, only: dyn_clean
implicit none

type(dyn_vect) idyn,odyn

character(len=*),parameter :: myname='dyn_boot'

character(len=*),parameter :: ifile = 'idyn.nc4'
character(len=*),parameter :: ofile = 'odyn.nc4'
integer :: nstep, freq, ier, nymd, nhms, lm
integer :: prec=0
integer :: vectype=5
logical ::  ncf=.false.
logical :: pncf=.false.

print *, 'reading from: ', ifile
call dyn_get ( ifile, nymd, nhms, idyn, ier, timidx=1, freq=freq, nstep=nstep, vectype=vectype, ncf=ncf, pncf=pncf )

lm = idyn%grid%lm + 2
call dyn_init ( idyn, odyn, ier, copy=.true., vectype=vectype, lm=lm )
     if ( ier/=0 ) then
        print *, trim(myname), ': Error duplicating dyn vector(odyn), ier=', ier
        call exit(998)
     endif

print *, 'writing to: ', ofile
call dyn_put ( ofile, nymd, nhms, prec, odyn, ier, freq=freq, nstep=nstep, vectype=vectype )
     if ( ier .ne. 0 ) then
        print *, trim(myname), ': cannot write interpolated ETA file'
        call exit(999)
     endif

call dyn_clean ( odyn )
call dyn_clean ( idyn )

end program
