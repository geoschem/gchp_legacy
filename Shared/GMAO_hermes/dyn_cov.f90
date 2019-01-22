program dyn_cov

use m_dyn,  only: dyn_init
use m_dyn,  only: dyn_get
use m_dyn,  only: dyn_put
use m_dyn,  only: dyn_vect
use m_dyn,  only: dyn_clean
use m_tick, only: tick
use m_const,only: radius_earth
use m_die,  only: die

implicit none

character(len=*),parameter :: myname = 'dyn_cov'
integer, parameter :: MAXCASES = 2

integer, parameter :: npnts  = 3
integer, parameter :: ofreq  = 010000

character(len=80)  efile(MAXCASES)
character(len=80)  ofile(MAXCASES)
character(len=80)  sfile(MAXCASES)
character(len=256) fname
integer :: nymd(2),nhms(2),freq,nstep,dyntype
integer :: im,jm,km,lm
integer :: imc(npnts),jmc(npnts),kmc,kmw
integer :: fcstlen
integer :: ntypes
integer :: members
integer mm,nn,ii,jj,k,it
integer iii,jjj
integer ier
logical :: verbose
logical :: nolocal
real    :: adrate
real    corrlength, dist, alev
real,allocatable :: adcovloc(:,:,:,:)
real,allocatable :: covloc(:,:,:,:)
real,allocatable :: ploc(:,:,:)
real,allocatable :: umean(:,:),vmean(:,:)

type(dyn_vect) e_b
type(dyn_vect) e_f
type(dyn_vect) acorr(npnts*npnts)
type(dyn_vect) xcorr(npnts*npnts)
type(dyn_vect) stdv(2)
type(dyn_vect) aux


call init_()

do it=1,ntypes
   write(fname,'(2a)') 'ensrms/', trim(sfile(it))
   call dyn_get ( trim(fname), nymd(it), nhms(it), stdv(it), ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
enddo
im=stdv(1)%grid%im; jm=stdv(1)%grid%jm
km=stdv(1)%grid%km; lm=stdv(1)%grid%lm

kmc=km-22 ! wired
kmw=kmc   ! wired

if (ntypes>1.and.adrate>-9999.) then
   allocate(umean(im,jm),vmean(im,jm))
   umean=0.0;vmean=0.0
   do it=1,ntypes
      write(fname,'(2a)') 'ensmean/', trim(sfile(it))
      call dyn_get ( trim(fname), nymd(it), nhms(it), aux, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
      umean = umean + aux%u(:,:,kmw) 
      vmean = vmean + aux%v(:,:,kmw) 
      call dyn_clean (aux)
   enddo
   umean = 0.5*umean
   vmean = 0.5*vmean
   if(verbose) print *, 'rough average global wind: ', sum(umean)/float(im*jm), sum(vmean)/(im*jm)
endif

! set point for now ...
call create_locs(imc,jmc,kmc)

! cov-localization
allocate(ploc(1,1,3))
allocate(covloc(im,jm,3,npnts*npnts))
if (ntypes>1.and.adrate>-9999.) then
   allocate(adcovloc(im,jm,3,npnts*npnts))
endif
nn=0
do jjj=1,npnts
do iii=1,npnts
   nn=nn+1
   call globeloc ( ploc, stdv(1)%grid%lat(jmc(jjj):jmc(jjj)), stdv(1)%grid%lon(imc(iii):imc(iii)) )
   call globeloc ( covloc(:,:,:,nn), stdv(1)%grid%lat, stdv(1)%grid%lon )
   do jj=1,jm
      do ii=1,im
         dist = sqrt( (covloc(ii,jj,1,nn)-ploc(1,1,1))**2 + &
                      (covloc(ii,jj,2,nn)-ploc(1,1,2))**2 + &
                      (covloc(ii,jj,3,nn)-ploc(1,1,3))**2   )/corrlength
         covloc(ii,jj,1,nn) = taper(dist)
      enddo
   enddo
   if (ntypes>1.and.adrate>-9999.) then
      call globeadloc ( adcovloc(:,:,:,nn), stdv(1)%grid%lat, stdv(1)%grid%lon, umean, vmean )
      do jj=1,jm
         do ii=1,im
            dist = sqrt( (adcovloc(ii,jj,1,nn)-ploc(1,1,1))**2 + &
                         (adcovloc(ii,jj,2,nn)-ploc(1,1,2))**2 + &
                         (adcovloc(ii,jj,3,nn)-ploc(1,1,3))**2   )/corrlength
            adcovloc(ii,jj,1,nn) = taper(dist)
         enddo
      enddo
   endif
enddo
enddo
deallocate(ploc)

! if no localization ...
if( nolocal ) then
  covloc = 1.0
  if (ntypes>1.and.adrate>-9999.) adcovloc = 1.0
endif

! loop over members
do mm=1,members
  write(fname,'(a,i3.3,2a)') 'mem', mm, '/', trim(efile(1))
  if(verbose) print *,'Processing ', trim(fname)
  call dyn_get ( trim(fname), nymd(1), nhms(1), e_b, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
  if (mm==1) then
     do nn=1,size(acorr)
     call dyn_init ( im, jm, km, lm, acorr(nn), ier, &
                     e_b%grid%ptop, e_b%grid%ks, e_b%grid%ak, e_b%grid%bk, vectype=dyntype )
          if ( ier/=0 ) then
               call die (myname, ': Error initializing dyn vector(acorr)')
          endif
     enddo
  endif
  ! scale by inverse of standard deviations
  e_b%ps = e_b%ps/stdv(1)%ps
  e_b%ts = e_b%ts/stdv(1)%ts
  e_b%pt = e_b%pt/stdv(1)%pt
  e_b%u  = e_b%u /stdv(1)%u
  e_b%v  = e_b%v /stdv(1)%v
  ! auto-corr
  nn=0
  do jjj=1,npnts
  do iii=1,npnts
     nn=nn+1
     acorr(nn)%ps = ((mm-1.0)*acorr(nn)%ps + e_b%ps*e_b%ps(imc(iii),jmc(jjj)))/float(mm)
     acorr(nn)%ts = ((mm-1.0)*acorr(nn)%ts + e_b%ps*e_b%ts(imc(iii),jmc(jjj)))/float(mm)
     do k=1,km
        acorr(nn)%pt(:,:,k) = ((mm-1.0)*acorr(nn)%pt(:,:,k) + e_b%pt(:,:,k)*e_b%pt(imc(iii),jmc(jjj),kmc))/float(mm)
        acorr(nn)%u (:,:,k) = ((mm-1.0)*acorr(nn)%u (:,:,k) + e_b%u (:,:,k)*e_b%u (imc(iii),jmc(jjj),kmc))/float(mm)
        acorr(nn)%v (:,:,k) = ((mm-1.0)*acorr(nn)%v (:,:,k) + e_b%v (:,:,k)*e_b%v (imc(iii),jmc(jjj),kmc))/float(mm)
     enddo
     ! apply localization
     if(mm==members) then
        acorr(nn)%hs_stdv =  covloc(:,:,1,nn)
        acorr(nn)%ps      =  covloc(:,:,1,nn)*acorr(nn)%ps
        acorr(nn)%ts      =  covloc(:,:,1,nn)*acorr(nn)%ts
        do k=1,km
           acorr(nn)%pt(:,:,k) = covloc(:,:,1,nn)*acorr(nn)%pt(:,:,k)
           acorr(nn)%u (:,:,k) = covloc(:,:,1,nn)*acorr(nn)%u (:,:,k)
           acorr(nn)%v (:,:,k) = covloc(:,:,1,nn)*acorr(nn)%v (:,:,k)
        enddo
     endif
  enddo
  enddo

  ! cross-corr
  if (ntypes==2) then
     write(fname,'(a,i3.3,2a)') 'mem', mm, '/', trim(efile(2))
     if(verbose) print *,'Processing ', trim(fname)
     call dyn_get ( trim(fname), nymd(2), nhms(2), e_f, ier, timidx=0, freq=freq, nstep=nstep, vectype=dyntype )
     if (mm==1) then
        do nn=1,size(xcorr)
        call dyn_init ( im, jm, km, lm, xcorr(nn), ier, &
                        e_f%grid%ptop, e_f%grid%ks, e_f%grid%ak, e_f%grid%bk, vectype=dyntype )
             if ( ier/=0 ) then
                  call die (myname, ': Error initializing dyn vector(xcorr)')
             endif
        enddo
     endif
     ! scale by inverse of standard deviations
     e_f%ps = e_f%ps/stdv(2)%ps
     e_f%ts = e_f%ts/stdv(2)%ts
     e_f%pt = e_f%pt/stdv(2)%pt
     e_f%u  = e_f%u /stdv(2)%u
     e_f%v  = e_f%v /stdv(2)%v
     ! auto-corr
     nn=0
     do jjj=1,npnts
     do iii=1,npnts
        nn=nn+1
        xcorr(nn)%ps = ((mm-1.0)*xcorr(nn)%ps + e_b%ps*e_f%ps(imc(iii),jmc(jjj)))/float(mm)
        xcorr(nn)%ts = ((mm-1.0)*xcorr(nn)%ts + e_b%ps*e_f%ts(imc(iii),jmc(jjj)))/float(mm)
        do k=1,km
           xcorr(nn)%pt(:,:,k) = ((mm-1.0)*xcorr(nn)%pt(:,:,k) + e_b%pt(:,:,k)*e_f%pt(imc(iii),jmc(jjj),kmc))/float(mm)
           xcorr(nn)%u (:,:,k) = ((mm-1.0)*xcorr(nn)%u (:,:,k) + e_b%u (:,:,k)*e_f%u (imc(iii),jmc(jjj),kmc))/float(mm)
           xcorr(nn)%v (:,:,k) = ((mm-1.0)*xcorr(nn)%v (:,:,k) + e_b%v (:,:,k)*e_f%v (imc(iii),jmc(jjj),kmc))/float(mm)
        enddo
        ! apply localization
        if(mm==members) then
           if(adrate>-9999.) then
              xcorr(nn)%hs_stdv =  adcovloc(:,:,1,nn)
           else
              xcorr(nn)%hs_stdv =    covloc(:,:,1,nn)
           endif
           xcorr(nn)%ps      =  xcorr(nn)%hs_stdv*xcorr(nn)%ps
           xcorr(nn)%ts      =  xcorr(nn)%hs_stdv*xcorr(nn)%ts
           do k=1,km
              xcorr(nn)%pt(:,:,k) = xcorr(nn)%hs_stdv*xcorr(nn)%pt(:,:,k)
              xcorr(nn)%u (:,:,k) = xcorr(nn)%hs_stdv*xcorr(nn)%u (:,:,k)
              xcorr(nn)%v (:,:,k) = xcorr(nn)%hs_stdv*xcorr(nn)%v (:,:,k)
           enddo
        endif
     enddo
     enddo
     call dyn_clean(e_f)
  endif

  ! clean up
  call dyn_clean(e_b)
enddo
do nn=2,size(acorr)
   acorr(1)%hs_stdv = acorr(1)%hs_stdv + acorr(nn)%hs_stdv
   acorr(1)%ps = acorr(1)%ps + acorr(nn)%ps
   acorr(1)%ts = acorr(1)%ts + acorr(nn)%ts
   acorr(1)%pt = acorr(1)%pt + acorr(nn)%pt
   acorr(1)%u  = acorr(1)%u  + acorr(nn)%u
   acorr(1)%v  = acorr(1)%v  + acorr(nn)%v
enddo
call dyn_put ( trim(ofile(1)), nymd(1), nhms(1), 0, acorr(1), ier, freq=ofreq, vectype=dyntype )
if (ntypes==2) then
   do nn=2,size(xcorr)
       xcorr(1)%hs_stdv = xcorr(1)%hs_stdv + xcorr(nn)%hs_stdv
       xcorr(1)%ps = xcorr(1)%ps + xcorr(nn)%ps
       xcorr(1)%ts = xcorr(1)%ts + xcorr(nn)%ts
       xcorr(1)%pt = xcorr(1)%pt + xcorr(nn)%pt
       xcorr(1)%u  = xcorr(1)%u  + xcorr(nn)%u
       xcorr(1)%v  = xcorr(1)%v  + xcorr(nn)%v
   enddo
   call dyn_put ( trim(ofile(2)), nymd(2), nhms(2), 0, xcorr(1), ier, freq=ofreq, vectype=dyntype )
endif

! clean up
deallocate(covloc)
if(ntypes>1) then
  if (adrate>-9999.) then
     deallocate(adcovloc)
  endif
  do nn=1,size(xcorr)
     call dyn_clean(xcorr(nn))
  enddo
endif
do nn=1,size(acorr)
   call dyn_clean(acorr(nn))
enddo

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
 fcstlen = 0
 members = 32
 adrate  = -9999.
 corrlen = 800.        ! to be read from file

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
         ofile(1) = trim(argv)

      case ("-ox")
         if ( iarg+1 .gt. argc ) call usage()
         iarg = iarg + 1
         call GetArg ( iarg, argv )
         ofile(2) = trim(argv)

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
           if (nc .gt. 4 ) call die(myname,'too many cases')
           if(nc==1) sfile(1) = trim(argv)
           if(nc==2) efile(1) = trim(argv)
           if(nc==3) sfile(2) = trim(argv)
           if(nc==4) efile(2) = trim(argv)
        endif

    end select 

 enddo
 ntypes=nc/2
 if (verbose) then
    write(*,'(a,i8.8,2x,i6.6)') 'Will calculate auto-corr on: ', nymd(1), nhms(1)
 endif
 print *, 'Standard derivation file: ', trim(sfile(1))
 print *, 'Member error file:        ', trim(efile(1))
 if(ntypes>1) then
   nymd(2)=nymd(1); nhms(2)=nhms(1)
   call tick(nymd(2),nhms(2),fcstlen*3600)
   if (verbose) then
      write(*,'(a,i8.8,2x,i6.6)') 'Will calculate cross-corr on: ', nymd(2), nhms(2)
   endif
   print *, 'Second standard derivation file: ', trim(sfile(2))
   print *, 'Second member error file:        ', trim(efile(2))
 endif

 corrlength = corrlen*1000/radius_earth
 print *,' Set localization scales (horiz)=',corrlen/0.388

end subroutine init_

subroutine usage()
     print *
     print *, '  -----------------------------------------------------------'
     print *, '  dyn_cov - derives two-point cov/corr maps from ensemble    '
     print *, '  -----------------------------------------------------------'
     print *
     print *,'Usage: '
     print *,'  Derive auto-corr only:'
     print *,'  dyn_cov.x [-verb] [-mem MEMBERS] [-o ACORR ] nymd nhms stdvfile filename'
     print *
     print *,'  Derive cross-corr:'
     print *,'  dyn_cov.x [-verb] [-mem MEMBERS] [-o ACORR ] [-ox XCORR] '
     print *,'            [-fcstlen FCSTLEN] [-adrate ADRATE] '
     print *, '           nymd nhms stdvfile1 filename1 stdvfile2 filename2'
     print *
     print *, 'where'
     print *
     print *, '-verb              specify to set verbose on'
     print *, '-mem     MEMBERS   number of members'
     print *, '                      (DEFAULT: 32)'
     print *, '-fcstlen FCSTLEN   forecast length (hours)'
     print *, '-adrate  ADRATE    rate of localization advection'
     print *, '                      (DEFAULT: 0.0; no advection)'
     print *, '-o       ACORR     output auto-correlation file'
     print *, '-ox      XCORR     output corr-correlation file'
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

end program dyn_cov
