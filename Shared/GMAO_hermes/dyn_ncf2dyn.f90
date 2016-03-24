program ncf2dyn
use m_dyn
use m_die, only: die
implicit none

character(len=*),parameter :: myname = 'ncf2dyn'
integer,parameter :: mfiles=2
character(len=256) fnames(mfiles), odyn_file

integer nymd, nhms
integer freq, vectype, nstep, prec
integer ier
type(dyn_vect)  ncf   ! input non-compliant dyn-vector
type(dyn_vect)  dyn   ! input dyn-vector (typically at same date/time as above)

! initialization
  call init_(ier)
  if(ier<0) call usage_

! read file containing non-compliant dyn-vector
  call dyn_get ( trim(fnames(1)), nymd, nhms, ncf, ier, timidx=1, freq=freq, nstep=nstep, vectype=vectype, ncf=.true. )

! read file containing dyn-vector at same date/time as file above
  call dyn_get ( trim(fnames(2)), nymd, nhms, dyn, ier, timidx=0, freq=freq, nstep=nstep, vectype=vectype )

! check for resolution match
  if( ncf%grid%im /= dyn%grid%im .or. &
      ncf%grid%jm /= dyn%grid%jm .or. &
      ncf%grid%km /= dyn%grid%km .or. &
      ncf%grid%lm /= dyn%grid%lm ) then
      print *, 'resolution of ncf file: ', ncf%grid%im, ncf%grid%jm, ncf%grid%km, ncf%grid%lm
      print *, 'resolution of dyn file: ', dyn%grid%im, dyn%grid%jm, dyn%grid%km, dyn%grid%lm
      call die(myname,': inconsistent resolutions, aborting ...')
  endif

! merge missing fields: since fields names are properly defined in dyn (rather
! than ncf) overwrite dyn structure instead of the other way around
  dyn%phis = ncf%phis
  dyn%ps   = ncf%ps
  dyn%delp = ncf%delp
  dyn%pt   = ncf%pt
  dyn%u    = ncf%u
  dyn%v    = ncf%v
! for now handle only sphu ... oz/ql/qi left as in GEOS file
  dyn%q(:,:,:,1) = ncf%q(:,:,:,1)

! write out
  call dyn_put ( odyn_file, nymd, nhms, prec, dyn, ier, freq=freq, nstep=nstep, vectype=vectype )
  if ( ier .ne. 0 ) then
       call dyn_clean ( dyn )
       call die(trim(myname), ': cannot write out ETA file, aborting ...')
  endif

CONTAINS

  subroutine init_ (rc)

  integer, intent(out) :: rc

  character(len=*),parameter :: myname_ = 'init_'
  character(len=256) :: argv
  integer argc,i,iarg,iargc,nfiles
  rc=0
  argc =  iargc()
  if ( argc .lt. 1 ) then
     rc=-1
     return
  endif

! defaults
  vectype = 5 
  prec    = 0
  odyn_file = 'ncf2dyn.eta.nc4'

  nfiles=0
  do i = 1, 32767
     iarg = iarg + 1
     if ( iarg .gt. argc ) exit
     call GetArg ( iarg, argv )
     select case (argv)
       case ("-o")
         if ( iarg+1 .gt. argc ) then
             rc=-2
             return
         endif
         iarg = iarg + 1
         call GetArg ( iarg, odyn_file )
       case default
         nfiles = nfiles + 1
         if ( nfiles .gt. mfiles ) call die(myname_,': too many eta files')
         fnames(nfiles) = argv
     end select
  enddo
  if(nfiles/=mfiles) rc=-3

  end subroutine init_

  subroutine usage_()
  print *
  print *, '  -----------------------------------------------------------'
  print *, '  dyn_ncf2dyn.x - converts non-complaint dyn to true dyn-vect'
  print *, '  -----------------------------------------------------------'
  print *
  print *,'Usage: '
  print *,'  dyn_ncf2dyn.x [-o odyn_file ] ncf_file dyn_file'
  print *
  print *,' where '
  print *,'    ncf_file   non-complaint dyn-vector'
  print *,'    dyn_file   complaint dyn-vector at same date/time'
  print *,' -o odyn_file  output (merged) dyn-vector'
  print *

  stop
  end subroutine usage_

end program  ncf2dyn
