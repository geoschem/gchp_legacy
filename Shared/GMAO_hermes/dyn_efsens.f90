program dyn_efsens
! 
! NOTE: This code does not produce the correct result because, yet. The
!       reason is that it uses the Jgradf files from initadj, which do 
!       not correspond to E^1/2(x-xv), but rather to 2E(x-xv) which is 
!       the gradient dJ/dx, for J := (x-xv)'E(x-xv).
!
!       One simple way to come around this is have this code work from 
!       the error files, (x-xv), and have it apply an energy norm on its
!       own. Or perhaps even better, ability dyndiff to produce the scaled
!       vectors.
!  
use m_dyn,  only: dyn_init
use m_dyn,  only: dyn_get
use m_dyn,  only: dyn_put
use m_dyn,  only: dyn_vect
use m_dyn,  only: dyn_clean
use m_tick, only: tick
use m_const,only: radius_earth
use m_die,  only: die

implicit none

character(len=*),parameter :: myname = 'dyn_efsens'

integer, parameter :: MAXCASES = 1
integer, parameter :: npnts  = 3
integer, parameter :: ofreq  = 010000
integer :: ntypes=2

character(len=80)  ex0file
character(len=80)  ofile   ! output efsens file
character(len=80)  x0file  ! initial time analysis error stdv
character(len=80)  fTfile  ! final time energy-scaled error
character(len=256) fname
integer :: nymd(2),nhms(2),freq,nstep,dyntype
integer :: im,jm,km,lm
integer :: imc(npnts),jmc(npnts),kmc,kmw
integer :: fcstlen
integer :: members
integer mm,nn,ii,jj,k,it
integer iii,jjj
integer ier
logical :: verbose
logical :: nolocal
real    :: err(6,2)
real    :: adrate
real    corrlength, dist, alev
real,allocatable :: adcovloc(:,:,:,:)
real,allocatable :: covloc(:,:,:,:)
real,allocatable :: ploc(:,:,:)

type(dyn_vect) efsens  ! estimate of forecast ensemble sensitivity
type(dyn_vect) dJ      ! grid-point evaluation of del of cost
type(dyn_vect) dx      ! initial perturbation
type(dyn_vect) mferr2  ! square of ens mean forecast error at verification time
type(dyn_vect) maerr2  ! square of ens mean analysis error at intial time
type(dyn_vect) stdv(2)
type(dyn_vect) aux


call init_()

! read energy-scaled forecast error at verification time
write(fname,'(2a)') 'ensmean/', trim(fTfile)
if(verbose) print *,'Processing ', trim(fname)
call dyn_get ( trim(fname), nymd(2), nhms(2), mferr2, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
im=mferr2%grid%im; jm=mferr2%grid%jm
km=mferr2%grid%km; lm=mferr2%grid%lm

! forecast at t=T
mferr2%ps = mferr2%ps * mferr2%ps
mferr2%pt = mferr2%pt * mferr2%pt
mferr2%u  = mferr2%u  * mferr2%u 
mferr2%v  = mferr2%v  * mferr2%v 
mferr2%q  = mferr2%q  * mferr2%q 
call sump_ (mferr2,err(:,2),'final time error (E-norm): ')

! loop over members
do mm=1,members
  write(fname,'(a,i3.3,2a)') 'mem', mm, '/', trim(fTfile)
  if(verbose) print *,'Processing ', trim(fname)
  call dyn_get ( trim(fname), nymd(2), nhms(2), dJ, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
  if (mm==1) then
     call dyn_init ( im, jm, km, lm, efsens, ier, &
                     dJ%grid%ptop, dJ%grid%ks, dJ%grid%ak, dJ%grid%bk, vectype=dyntype )
          if ( ier/=0 ) then
               call die (myname, ': Error initializing dyn vector(efsens)')
          endif
  endif
  ! point-wise error difference
  dJ%ps = dJ%ps*dJ%ps - mferr2%ps
  dJ%pt = dJ%pt*dJ%pt - mferr2%pt
  dJ%u  = dJ%u *dJ%u  - mferr2%u
  dJ%v  = dJ%v *dJ%v  - mferr2%v
  dJ%q  = dJ%q *dJ%q  - mferr2%q

  ! read in initial analysis error for this member
  write(fname,'(a,i3.3,2a)') 'mem', mm, '/', trim(ex0file)
  if(verbose) print *,'Processing ', trim(fname)
  call dyn_get ( trim(fname), nymd(1), nhms(1), dx, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
 
  ! perform point-wise product of dx*dJ and accummulate in total
  efsens%ps = efsens%ps + dx%ps*dJ%ps
  efsens%pt = efsens%pt + dx%pt*dJ%pt
  efsens%u  = efsens%u  + dx%u *dJ%u
  efsens%v  = efsens%v  + dx%v *dJ%v
  efsens%q  = efsens%q  + dx%q *dJ%q

  ! clean up
  call dyn_clean(dx)
  call dyn_clean(dJ)
enddo

! read analysis error std dev
write(fname,'(2a)') 'ensrms/', trim(x0file)
if(verbose) print *,'Processing ', trim(fname)
call dyn_get ( trim(fname), nymd(1), nhms(1), maerr2, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )

! square errors
! analysis at t=0
maerr2%ps = maerr2%ps * maerr2%ps
maerr2%pt = maerr2%pt * maerr2%pt
maerr2%u  = maerr2%u  * maerr2%u 
maerr2%v  = maerr2%v  * maerr2%v 
maerr2%q  = maerr2%q  * maerr2%q 

! scale forecast sensitivities
efsens%ps = efsens%ps / maerr2%ps
efsens%pt = efsens%pt / maerr2%pt
efsens%u  = efsens%u  / maerr2%u 
efsens%v  = efsens%v  / maerr2%v 
efsens%q(:,:,:,1)  = efsens%q(:,:,:,1)  / maerr2%q(:,:,:,1)
efsens%q(:,:,:,2:) = 0.0 ! ignore anything else other than sphu
! ... should need to derive delp''s

call dyn_put ( trim(ofile), nymd(1), nhms(1), 0, efsens, ier, freq=ofreq, vectype=dyntype )

! Just as a diagnostic ...
efsens%ps = efsens%ps * efsens%ps
efsens%pt = efsens%pt * efsens%pt
efsens%u  = efsens%u  * efsens%u 
efsens%v  = efsens%v  * efsens%v 
efsens%q  = efsens%q  * efsens%q 
call sump_ (efsens,err(:,1),'Initial time error (E-norm): ')
! clean up
call dyn_clean(mferr2)
call dyn_clean(maerr2)
call dyn_clean(efsens)

contains

subroutine init_()
 implicit none

 integer i, iarg, argc, iargc
 integer ncount,nc
 character(len=255) :: argv
 real corrlen

 verbose = .false.
 nolocal = .false.
 dyntype = 5
 fcstlen = 24
 members = 3 ! 32
 adrate  = -9999.
 corrlen = 800.        ! to be read from file
 ofile   = 'efsens.eta.nc4'


 argc =  iargc()
 if ( argc .lt. 1 ) call usage()

 iarg=0
 ncount=0; nc=0
 do i = 1, 32767
    iarg = iarg + 1
    if ( iarg .gt. argc ) exit
    call GetArg ( iarg, argv )
    select case (argv)

      case ('-fcstlen')
          if ( iarg+1 .gt. argc ) call usage()
          iarg = iarg + 1
          call GetArg ( iarg, argv )
          read(argv,*) fcstlen           

      case ('-mem')
          if ( iarg+1 .gt. argc ) call usage()
          iarg = iarg + 1
          call GetArg ( iarg, argv )
          read(argv,*) members           

      case ('-adrate')
          if ( iarg+1 .gt. argc ) call usage()
          iarg = iarg + 1
          call GetArg ( iarg, argv )
          read(argv,*) adrate           

      case ('-corrlen')
          if ( iarg+1 .gt. argc ) call usage()
          iarg = iarg + 1
          call GetArg ( iarg, argv )
          read(argv,*) corrlen           

      case ("-o")
         if ( iarg+1 .gt. argc ) call usage()
         iarg = iarg + 1
         call GetArg ( iarg, argv )
         ofile = trim(argv)

      case ('-nolocal')
         nolocal=.true.

      case ('-verb')
         verbose=.true.

      case default
        ncount = ncount + 1
        if (ncount==1) then
           call GetArg ( iarg, argv )
           read(argv,*) nymd(1)
        else if (ncount==2 ) then
           call GetArg ( iarg, argv )
           read(argv,*) nhms(1)
        else 
           nc=nc+1
           if (nc .gt. 3 ) call die(myname,'too many cases')
           if(nc==1) x0file  = trim(argv)
           if(nc==2) ex0file = trim(argv)
           if(nc==3) fTfile  = trim(argv)
        endif

    end select 

 enddo
 if (nc/=3) then
    
 endif
 if (verbose) then
    write(*,'(a,i8.8,2x,i6.6)') 'Initial time: ', nymd(1), nhms(1)
 endif
 print *, 'Initial time error stdv file: ',                 trim(x0file)
 print *, 'Final time energy-scaled forecast error file: ', trim(fTfile)
 nymd(2)=nymd(1); nhms(2)=nhms(1)
 call tick(nymd(2),nhms(2),fcstlen*3600)
 if (verbose) then
    write(*,'(a,i8.8,2x,i6.6)') 'Final time: ', nymd(2), nhms(2)
 endif

 corrlength = corrlen*1000/radius_earth
 print *,' Set localization scales (horiz)=',corrlen/0.388

end subroutine init_

subroutine sump_(x,sump,msg)
implicit none
real sump(:)
type(dyn_vect) x
character(len=*) :: msg
sump(2) = sum(x%u )
sump(3) = sum(x%v )
sump(4) = sum(x%pt)
sump(5) = sum(x%ps)
sump(6) = sum(x%q(:,:,:,1) )
sump(1) = sum(sump(2:6))
write(*,'(a,1p,6(1x,e13.6))') trim(msg), sump
end subroutine sump_

subroutine usage()
     print *
     print *, '  -----------------------------------------------------------'
     print *, '  dyn_efsens - ensemble-based estimate of forecast sensivity '
     print *, '  -----------------------------------------------------------'
     print *
     print *,'Usage: '
     print *,'  Derive auto-corr only:'
     print *,'  dyn_efsens.x [-verb] [-mem MEMBERS] [-o EFSENS ] nymd nhms stdv0 aerr0 EerrT'
     print *
     print *, 'where'
     print *
     print *, '-verb              specify to set verbose on'
     print *, '-mem     MEMBERS   number of members'
     print *, '                      (DEFAULT: 32)'
     print *, '-fcstlen FCSTLEN   forecast length (hours)'
     print *, '-adrate  ADRATE    rate of localization advection'
     print *, '                      (DEFAULT: 0.0; no advection)'
     print *, '-o       EFSENS    output forecast sensitivity estimate'
     print *
     print *, 'REMARKS: '
     print *, ' 1. This program requires error files to have been generated'
     print *, '    prior to attempting to run it. E.g., see dyndiff.x'
     print *
     call exit(1)
end subroutine usage

subroutine globeloc (aloc,lat,lon)
 implicit none
 real,intent(inout) :: aloc(:,:,:)
 real,intent(in)    :: lat(:),lon(:)
 real    pi
 integer i,j

 real,allocatable:: clat(:),clon(:),slat(:),slon(:)
 if(size(aloc,3)<3) then
   print *, 'globeloc error: check 2nd dim of aloc ', size(aloc,3)
 endif
 pi=4.0*atan(1.0)
 allocate(clat(size(lat)),clon(size(lon)))
 allocate(slat(size(lat)),slon(size(lon)))
 clat=cos(lat*pi/180.)  
 slat=sin(lat*pi/180.)   
 clon=cos(lon*pi/180.)   
 slon=sin(lon*pi/180.)   
 do j=1,size(aloc,2)
   do i=1,size(aloc,1)
      aloc(i,j,1) = clat(j)*clon(i)
      aloc(i,j,2) = clat(j)*slon(i)
      aloc(i,j,3) = slat(j)
   enddo
 enddo
 deallocate(slat,slon)
 deallocate(clat,clon)
end subroutine globeloc

subroutine globeadloc (aloc,lat,lon, u,v)
 implicit none
 real,intent(inout) :: aloc(:,:,:)
 real,intent(in)    :: lat(:),lon(:)
 real,intent(in)    :: u(:,:),v(:,:)
 real    pi,pi2,halfpi
 real    adlat,adlon
 real    adtime
 integer i,j

 real,allocatable:: clat(:),clon(:),slat(:),slon(:)
 if(size(aloc,3)<3) then
   print *, 'globeloc error: check 2nd dim of aloc ', size(aloc,3)
 endif
 pi=4.0*atan(1.0)
 pi2=2.0*pi
 halfpi=0.5*pi
 adtime=adrate*fcstlen*3600./radius_earth
 allocate(clat(size(lat)),clon(size(lon)))
 allocate(slat(size(lat)),slon(size(lon)))
 clat=lat*pi/180.
 slat=lat*pi/180.
 clon=lon*pi/180.
 slon=lon*pi/180.
 do j=1,size(aloc,2)
   do i=1,size(aloc,1)
      adlon = clon(i) - u(i,j) * cos(clat(j)) * adtime
      adlat = clat(j) - v(i,j) * adtime
      if(adlat > halfpi) then
         adlat = pi - adlat
         adlon = adlon + pi
      else if(adlat < -halfpi) then
         adlat = -pi - adlat
         adlon = adlon + pi
      end if
      if(adlon > pi2) then
         adlon = mod(adlon,pi2)
      else if(adlon < 0.0) then
         adlon = mod(adlon,pi2) + pi2
      end if

      aloc(i,j,1) = cos(adlat)*cos(adlon)
      aloc(i,j,2) = cos(adlat)*sin(adlon)
      aloc(i,j,3) = sin(adlat)
   enddo
 enddo
 deallocate(slat,slon)
 deallocate(clat,clon)
end subroutine globeadloc

real function taper(r)
 ! Gaspari-Cohn taper function.
 ! r should be positive, and normalized so taper = 0 at r = 1
 ! very close to exp(-(r/c)**2), where c = 0.388
 implicit none

 real a1,a2,a3,a4,a5,a6,a7,a8,a9
 parameter(a1 = -8.0)
 parameter(a2 = 8.0)
 parameter(a3 = 5.0)
 parameter(a4 = 20.0/3.0)
 parameter(a5 = 1.0)
 parameter(a6 = 8.0/3.0)
 parameter(a7 = -10.0)
 parameter(a8 =  4.0)
 parameter(a9 = -1.0/3.0)

 real, intent(in) :: r
 if(r < a5)then
   if(r > 0.5)then
      taper = ( ( ( ( a6*r -a2 )*r +a3 )*r +a4 )*r +a7)*r + a8 + a9/r
   else
      taper = ( ( ( a1*r +a2)*r +a3 )*r -a4)*r*r + a5
   end if
 else
    taper = 0.0
 end if
end function taper

subroutine create_locs (imc,jmc,kmc)
implicit none
integer,intent(inout) :: imc(:), jmc(:), kmc

integer ii
integer idel,jdel
idel=25
jdel=25

alev = stdv(1)%grid%ak(kmc)/100. + 1000.*stdv(1)%grid%bk(kmc)
imc(1) = 30
jmc(1) = jm-30
do ii=2,size(imc,1)
   imc(ii) = imc(1)+(ii-1)*idel
   jmc(ii) = jmc(1)-(ii-1)*jdel
enddo 
do jj=1,size(imc,1)
do ii=1,size(imc,1)
   write(*,'(a,3(2x,f7.2))') 'point corr at ', stdv(1)%grid%lat(jmc(ii)), &
                                               stdv(1)%grid%lon(imc(ii)), &
                                               alev
enddo
enddo
end subroutine create_locs

end program dyn_efsens
