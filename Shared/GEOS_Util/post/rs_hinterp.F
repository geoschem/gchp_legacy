      program  main
      use MAPL_ConstantsMod
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to merge eta_hdf data into GEOS5 restarts        ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

      integer     im_out
      integer     jm_out

      character*256 dynrst, moistrst, anaeta
      character*256 topo_old
      character*256 topo_new
      character*256, allocatable :: other_rst(:)

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms,nymd_ana,nhms_ana
      integer im,jm,lm

! restart variables and topography
! --------------------------------
      real,   allocatable ::    u(:,:,:)
      real,   allocatable ::    v(:,:,:)
      real,   allocatable ::   th(:,:,:)
      real,   allocatable ::   pk(:,:,:)
      real,   allocatable ::  ple(:,:,:)
      real,   allocatable ::    q(:,:,:)
      real,   allocatable ::   ps(:,:)
      real,   allocatable ::   ts(:,:)
      real,   allocatable ::   tb(:,:)
      real,   allocatable ::    qnew(:,:,:)
      real,   allocatable ::   thnew(:,:,:)
      real,   allocatable ::    unew(:,:,:)
      real,   allocatable ::    vnew(:,:,:)
      real,   allocatable ::  plenew(:,:,:)
      real,   allocatable ::  pkenew(:,:,:)
      real,   allocatable ::   pknew(:,:,:)
      real,   allocatable :: phisold(:,:)
      real,   allocatable :: phisint(:,:)
      real,   allocatable :: phisnew(:,:)
      real,   allocatable ::   psnew(:,:)
      real,   allocatable ::   tbnew(:,:)

      real*8, allocatable ::   ak(:)
      real*8, allocatable ::   bk(:)
      real*8, allocatable ::  dum(:,:)

      real*4, allocatable :: dumold(:,:)
      real*4, allocatable :: dumnew(:,:)

      real    kappa, grav, beta, delp
      real    undef,rgas,rvap,eps
      real    qmin1,qmax1,qmin2,qmax2

      character*256, allocatable :: arg(:)
      character*1    char
      character*8    date
      character*2    hour
      character*4    xdim ,ydim
      integer m,n,nmax,nargs,iargc,i,j,L,ID,rc
      integer nlots,nrem,num,num_other_rst

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

         kappa = MAPL_KAPPA
          grav = MAPL_GRAV
          beta = 0.0065
 
          xdim = '    '
          ydim = '    '
        dynrst = 'fvcore_internal_restart'
      moistrst = 'moist_internal_restart'
        anaeta = 'x'
      nymd_ana = -999
      nhms_ana = -999
        im_out = -999
        jm_out = -999

        num_other_rst = 0

          nargs = iargc()
      if( nargs.eq.0 ) call usage()

      allocate ( arg(nargs) )
      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
             if( trim(arg(n)).eq.'-dyn'      ) then
                                   dynrst  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-moist'    ) then
                                   moistrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo_old' ) then
                                   topo_old = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo_new' ) then
                                   topo_new = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-im'       ) read(arg(n+1),*) im_out
             if( trim(arg(n)).eq.'-jm'       ) read(arg(n+1),*) jm_out

             if( trim(arg(n)).eq.'-other' ) then
                 num = 1
                 if( n+num.le.nargs ) then
                 read(arg(n+num),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+num.ne.nargs )
                 num = num+1
                 read(arg(n+num),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) num = num-1
                 allocate ( other_rst(num) )
                 do m=1,num
                 other_rst(m) = arg(n+m)
                 enddo
                 num_other_rst = num
                 endif
             endif

      enddo

      if( im_out.eq.-999 .or. jm_out.eq.-999 ) then
          print *, 'You must supply the desired output resolution IM and JM!'
          stop
      endif
      call checkfile ( trim(dynrst)   )
      call checkfile ( trim(moistrst) )
      call checkfile ( trim(topo_old) )
      call checkfile ( trim(topo_new) )
      do n=1,num_other_rst
      call checkfile ( trim(other_rst(n)) )
      enddo

! **********************************************************************
! ****    Read dycore internal Restart for RSLV, Date and Time      ****
! **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2
      close(10)

      nymd = headr1(1)*10000
     .     + headr1(2)*100
     .     + headr1(3)
      nhms = headr1(4)*10000
     .     + headr1(5)*100
     .     + headr1(6)

      if( nymd_ana.eq.-999 ) nymd_ana = nymd
      if( nhms_ana.eq.-999 ) nhms_ana = nhms

      write(xdim,103) im_out
      write(ydim,103) jm_out
      write(date,101) nymd_ana
      write(hour,102) nhms_ana/10000
  101 format(i8.8)
  102 format(i2.2)
  103 format(i4.4)

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

      print *
      print *, '   dyn restart filename: ',trim(dynrst)
      print *, ' moist restart filename: ',trim(moistrst)
      do n=1,num_other_rst
      print *, ' other restart filename: ',trim(other_rst(n))
      enddo
      print *, '             resolution: ',im,jm,lm
      print *, '                   date: ',nymd_ana,nhms_ana
      print *

      allocate (   ts(im,jm)      )
      allocate (   tb(im,jm)      )
      allocate (   ps(im,jm)      )
      allocate (   pk(im,jm,lm)   )
      allocate (  ple(im,jm,lm+1) )
      allocate (    u(im,jm,lm)   )
      allocate (    v(im,jm,lm)   )
      allocate (   th(im,jm,lm)   )
      allocate (    q(im,jm,lm)   )

      allocate (  dumold(im    ,jm    ) )
      allocate (  dumnew(im_out,jm_out) )

      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )

! **********************************************************************
! ****                    Read Topography Datasets                  ****
! **********************************************************************

      allocate ( phisold(im,jm)         )
      allocate ( phisint(im_out,jm_out) )
      allocate ( phisnew(im_out,jm_out) )

      print *, 'Reading Old Topography Dataset: ',trim(topo_old)
      open (10,file=trim(topo_old),form='unformatted',access='sequential')
      read (10)      dumold
      phisold(:,:) = dumold
      close(10)

      print *, 'Reading New Topography Dataset: ',trim(topo_new)
      open (10,file=trim(topo_new),form='unformatted',access='sequential')
      read (10)      dumnew
      phisnew(:,:) = dumnew
      close(10)

      phisold = phisold*grav
      phisnew = phisnew*grav

! **********************************************************************
! ****                  Read dycore internal Restart                ****
! **********************************************************************

      allocate ( dum(im,jm) )
      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2
      read (10) ak
      read (10) bk
          do L=1,lm
             read(10)   dum
             u(:,:,L) = dum(:,:)
          enddo
          do L=1,lm
             read(10)   dum
             v(:,:,L) = dum(:,:)
          enddo
          do L=1,lm
             read(10)    dum
             th(:,:,L) = dum(:,:)
          enddo
          do L=1,lm+1
             read(10)     dum
             ple(:,:,L) = dum(:,:)
          enddo
          do L=1,lm
             read(10)    dum
             pk(:,:,L) = dum(:,:)
          enddo
      close (10)
      deallocate ( dum )

c compute ts based on mean theta in pbl (100-mb)
c ----------------------------------------------
      ps(:,:) = ple(:,:,lm+1)
      ts(:,:) = 0.0
      do j=1,jm
      do i=1,im
         L=lm
         delp = 0.0
         do while ( delp.le.10000.0 )
            delp =  delp + ple(i,j,L+1)-ple(i,j,L)
         ts(i,j) =  ts(i,j) + th(i,j,L)*(ple(i,j,L+1)-ple(i,j,L))
         L = L-1
         enddo
         ts(i,j) = ts(i,j)/delp
      enddo
      enddo
      ts(:,:) = ts(:,:)*ple(:,:,lm+1)**kappa
      tb(:,:) = ts(:,:) + beta*phisold(:,:)/(2.0*grav)
                                                                                                                               
! **********************************************************************
! ****               Interpolate to New Resolution                  ****
! **********************************************************************

      allocate (  tbnew(im_out,jm_out)      )
      allocate (  psnew(im_out,jm_out)      )
      allocate ( plenew(im_out,jm_out,lm+1) )
      allocate ( pkenew(im_out,jm_out,lm+1) )
      allocate (  pknew(im_out,jm_out,lm)   )
      allocate (   unew(im_out,jm_out,lm)   )
      allocate (   vnew(im_out,jm_out,lm)   )
      allocate (  thnew(im_out,jm_out,lm)   )
      allocate (   qnew(im_out,jm_out,lm)   )

c interpolate topography, surface pressure, and tbar
c --------------------------------------------------
      call hinterp ( phisold,im,jm,phisint,im_out,jm_out,1,undef,1,3,.false. )
      call hinterp (      ps,im,jm,  psnew,im_out,jm_out,1,undef,1,3,.false. )
      call hinterp (      tb,im,jm,  tbnew,im_out,jm_out,1,undef,1,3,.false. )

      call ps_mod ( psnew,phisint,tbnew,phisnew,im_out,jm_out )

c reconstruct upper-level pressures based on new ps
c -------------------------------------------------
      do L=1,lm+1
      do j=1,jm_out
      do i=1,im_out
       plenew(i,j,L) = ak(L)+bk(L)*psnew(i,j)
       pkenew(i,j,L) = plenew(i,j,L)**kappa
      enddo
      enddo
      enddo
      do L=1,lm
      pknew(:,:,L) =            (pkenew(:,:,L+1)-pkenew(:,:,L))
     .             / ( kappa*log(plenew(:,:,L+1)/plenew(:,:,L)) )
      enddo

c interpolate winds
c -----------------
      call dtoa ( u,u,im,jm,lm,2 )
      call dtoa ( v,v,im,jm,lm,1 )

      call hinterp ( u,im,jm,unew,im_out,jm_out,lm,undef,-1,3,.false.)
      call hinterp ( v,im,jm,vnew,im_out,jm_out,lm,undef,-1,3,.false.)

      call atod ( unew,unew,im_out,jm_out,lm,2 )
      call atod ( vnew,vnew,im_out,jm_out,lm,1 )

c interpolate potential temperature
c ---------------------------------
      call hinterp ( th,im,jm,thnew,im_out,jm_out,lm,undef, 1,3,.false.)


! **********************************************************************
! ****                 Write dycore internal Restart                ****
! **********************************************************************

          undef = -999
      headr2(1) = im_out
      headr2(2) = jm_out

      anaeta = trim(dynrst) // '.' // xdim // 'x' // ydim
      print *
      print *, 'Creating GEOS-5 fvcore_internal_restart: ',trim(anaeta)

      allocate ( dum(im_out,jm_out) )
      open (20,file=trim(anaeta),form='unformatted',access='sequential')
      write(20) headr1
      write(20) headr2
      write(20) ak
      write(20) bk
          do L=1,lm
                       dum(:,:) = unew(:,:,L)
             write(20) dum
          enddo
          do L=1,lm
                       dum(:,:) = vnew(:,:,L)
             write(20) dum
          enddo
          do L=1,lm
                       dum(:,:) = thnew(:,:,L)
             write(20) dum
          enddo
          do L=1,lm+1
                       dum(:,:) = plenew(:,:,L)
             write(20) dum
          enddo
          do L=1,lm
                       dum(:,:) = pknew(:,:,L)
             write(20) dum
          enddo
      close (20)
      deallocate ( dum )

! **********************************************************************
! ****    Read, Interpolate, and Write MOIST Internal Restart       ****
! **********************************************************************

      anaeta = trim(moistrst) // '.' // xdim // 'x' // ydim

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      open  (20,file=trim(anaeta)  ,form='unformatted',access='sequential')

             nmax =  0
               rc =  0
      dowhile (rc.eq.0)
         read (10,iostat=rc)
      if( rc.eq.0 ) nmax = nmax + 1
      enddo
      rewind 10

      print *
      write(6,1001) 'Creating GEOS-5  moist_internal_restart: ' // trim(moistrst),nmax
      print *
 1001 format(1x,a,\,1x,'(',i5,' 2-D Arrays)' )
      nlots = nmax/100
      nrem  = nmax - nlots*100

      do  n=1,nlots+1
          if( n.eq.1 ) then
              nmax = nrem
          else
              nmax = 100
          endif
          deallocate ( q,qnew )
            allocate ( q   (im,    jm,    nmax) )
            allocate ( qnew(im_out,jm_out,nmax) )

      print *, 'Interpolating MOIST Restart for ',nmax,' 2-D Arrays ...'
      do L=1,nmax
         read (10)  dumold
         q(:,:,L) = dumold
      enddo

      call hinterp ( q,im,jm,qnew,im_out,jm_out,nmax,undef, 1,-3,.false.)

      do L=1,nmax
         dumnew = qnew(:,:,L)
         write(20) dumnew
      enddo

      enddo
      print *

      close (10)
      close (20)

! **********************************************************************
! ****    Read, Interpolate, and Write Other Internal Restart       ****
! **********************************************************************

      do m=1,num_other_rst
      anaeta = trim(other_rst(m)) // '.' // xdim // 'x' // ydim

      open  (10,file=trim(other_rst(m)),form='unformatted',access='sequential')
      open  (20,file=trim(anaeta)      ,form='unformatted',access='sequential')

             nmax =  0
               rc =  0
      dowhile (rc.eq.0)
         read (10,iostat=rc)
      if( rc.eq.0 ) nmax = nmax + 1
      enddo
      rewind 10

      write(6,1001) 'Creating GEOS-5  other_internal_restart: ' // trim(anaeta),nmax
      print *
      nlots = nmax/100
      nrem  = nmax - nlots*100

      do  n=1,nlots+1
          if( n.eq.1 ) then
              nmax = nrem
          else
              nmax = 100
          endif
          deallocate ( q,qnew )
            allocate ( q   (im,    jm,    nmax) )
            allocate ( qnew(im_out,jm_out,nmax) )

      print *, 'Interpolating Other Restart for ',nmax,' 2-D Arrays ...'
      do L=1,nmax
         read (10)  dumold
         q(:,:,L) = dumold
      enddo

!     call hinterp ( q,im,jm,qnew,im_out,jm_out,nmax,undef, 1,-3,.false.)  ! For Positive-Definate Fields
      call hinterp ( q,im,jm,qnew,im_out,jm_out,nmax,undef, 1, 3,.false.)  ! For General           Fields

      do L=1,nmax
         dumnew = qnew(:,:,L)
         write(20) dumnew
      enddo

      enddo
      print *

      close (10)
      close (20)
      enddo

      stop
      end

      subroutine writit (q,im,jm,lm)
      real q(im,jm,lm)
      real*4 a(im,jm)
      do L=1,lm
      a(:,:) = q(:,:,L)
      write(50) a
      enddo
      return
      end

      subroutine hinterp ( qin,iin,jin,qout,iout,jout,mlev,undef,msgn,norder,check )
      implicit   none
      integer    iin,jin,       iout,jout, mlev,msgn,norder
      real   qin(iin,jin,mlev), qout(iout,jout,mlev)
      real undef,pi,dlin,dpin,dlout,dpout
      real dlam(iin), lons(iout*jout), lon
      real dphi(jin), lats(iout*jout), lat
      integer i,j,loc
      logical check

      pi = 4.0*atan(1.0)
      dlin = 2*pi/iin
      dpin = pi/(jin-1)
      dlam(:) = dlin
      dphi(:) = dpin

      dlout = 2*pi/iout
      dpout = pi/(jout-1)
      
      loc = 0
      do j=1,jout
      do i=1,iout
      loc = loc + 1
      lon = -pi + (i-1)*dlout
      lons(loc) = lon
      enddo
      enddo

      loc = 0
      do j=1,jout
      lat = -pi/2.0 + (j-1)*dpout
      do i=1,iout
      loc = loc + 1
      lats(loc) = lat
      enddo
      enddo

      call interp_h ( qin,iin,jin,mlev,
     .                dlam,dphi,0.0,90.0,0.0,
     .                qout,iout*jout,lons,lats,
     .                msgn,norder,check,undef )

      return
      end

      subroutine interp_h ( q_cmp,im,jm,lm,
     .                      dlam,dphi,rotation,tilt,precession,
     .                      q_geo,irun,lon_geo,lat_geo,
     .                      msgn,norder,check,undef )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Performs a horizontal interpolation from a field on a computational grid
C    to arbitrary locations.
C
C  INPUT:
C  ======
C    q_cmp ...... Field q_cmp(im,jm,lm) on the computational grid
C    im ......... Longitudinal dimension of q_cmp
C    jm ......... Latitudinal  dimension of q_cmp
C    lm ......... Vertical     dimension of q_cmp
C    dlam ....... Computational Grid Delta Lambda
C    dphi ....... Computational Grid Delta Phi
C    rotation ... Rotation parameter lam_np (Degrees)
C    tilt ....... Rotation parameter phi_np (Degrees)
C    precession . Rotation parameter lam_0  (Degrees)
C    irun ....... Number of Output Locations
C    lon_geo .... Longitude Location of Output
C    lat_geo .... Latitude  Location of Output
C    msgn ....... Flag for scalar field  ( msgn =  1 )
C                    or vector component ( msgn = -1 )
C    norder ..... Order of Interpolation:  Bi-Linear => abs(norder) = 1
C                                          Bi-Cubic  => abs(norder) = 3
C                 Note: If norder < 0, then check for positive definite
C    check ...... Logical Flag to check for Undefined values
C
C  OUTPUT:
C  =======
C    q_geo ...... Field q_geo(irun,lm) at arbitrary locations
C
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer im,jm,lm,irun,norder,msgn
      logical check

      real      q_geo(irun,lm)
      real    lon_geo(irun)
      real    lat_geo(irun)

      real    q_cmp(im,jm,lm)
      real     dlam(im)
      real     dphi(jm)

c Local Variables
c ---------------
      integer  i,j,l,m,n
      integer, allocatable       :: ip1(:), ip0(:), im1(:), im2(:)
      integer, allocatable       :: jp1(:), jp0(:), jm1(:), jm2(:)

      integer ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1
      integer ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2
      integer jm2_for_jm2, jp1_for_jp1


c Bi-Linear Weights
c -----------------
      real, allocatable       ::    wl_ip0jp0 (:)
      real, allocatable       ::    wl_im1jp0 (:)
      real, allocatable       ::    wl_ip0jm1 (:)
      real, allocatable       ::    wl_im1jm1 (:)

c Bi-Cubic Weights
c ----------------
      real, allocatable       ::    wc_ip1jp1 (:)
      real, allocatable       ::    wc_ip0jp1 (:)
      real, allocatable       ::    wc_im1jp1 (:)
      real, allocatable       ::    wc_im2jp1 (:)
      real, allocatable       ::    wc_ip1jp0 (:)
      real, allocatable       ::    wc_ip0jp0 (:)
      real, allocatable       ::    wc_im1jp0 (:)
      real, allocatable       ::    wc_im2jp0 (:)
      real, allocatable       ::    wc_ip1jm1 (:)
      real, allocatable       ::    wc_ip0jm1 (:)
      real, allocatable       ::    wc_im1jm1 (:)
      real, allocatable       ::    wc_im2jm1 (:)
      real, allocatable       ::    wc_ip1jm2 (:)
      real, allocatable       ::    wc_ip0jm2 (:)
      real, allocatable       ::    wc_im1jm2 (:)
      real, allocatable       ::    wc_im2jm2 (:)

      real, allocatable       ::    old_lon (:)
      real, allocatable       ::    old_lat (:)
      real, allocatable       ::    old_dlam(:)
      real, allocatable       ::    old_dphi(:)

      real    ux, ap1, ap0, am1, am2
      real    uy, bp1, bp0, bm1, bm2

      real    lon_cmp(im)
      real    lat_cmp(jm)
      real    q_tmp(irun)

      real    pi,cosnp,sinnp,p1,p2,p3,eps,d
      real    lam,lam_ip1,lam_ip0,lam_im1,lam_im2
      real    phi,phi_jp1,phi_jp0,phi_jm1,phi_jm2
      real    dl,dp,lam_np,phi_np,lam_0,eps_np
      real    rotation , tilt , precession
      real    lam_geo, lam_cmp
      real    phi_geo, phi_cmp
      real    undef
      integer im1_cmp,icmp
      integer jm1_cmp,jcmp

      logical compute_weights
      real    old_rotation
      real    old_tilt
      real    old_precession
      data    old_rotation   /-999.9/
      data    old_tilt       /-999.9/
      data    old_precession /-999.9/

      parameter ( eps = 1.e-10 )

c Initialization
c --------------
      pi = 4.*atan(1.)
      dl = 2*pi/ im     ! Uniform Grid Delta Lambda
      dp =   pi/(jm-1)  ! Uniform Grid Delta Phi

c Allocate Memory for Weights and Index Locations
c -----------------------------------------------
      if(.not.allocated(old_lon)) then

      allocate (   old_dlam(im)  ,   old_dphi(jm)  )
      allocate (   old_lon(irun) ,   old_lat(irun) )
      allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
      allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
      allocate ( wc_ip1jp1(irun) , wc_ip0jp1(irun) , wc_im1jp1(irun) , wc_im2jp1(irun) )
      allocate ( wc_ip1jp0(irun) , wc_ip0jp0(irun) , wc_im1jp0(irun) , wc_im2jp0(irun) )
      allocate ( wc_ip1jm1(irun) , wc_ip0jm1(irun) , wc_im1jm1(irun) , wc_im2jm1(irun) )
      allocate ( wc_ip1jm2(irun) , wc_ip0jm2(irun) , wc_im1jm2(irun) , wc_im2jm2(irun) )
      allocate (       ip1(irun) ,       ip0(irun) ,       im1(irun) ,       im2(irun) )
      allocate (       jp1(irun) ,       jp0(irun) ,       jm1(irun) ,       jm2(irun) )
      do i=1,irun
      old_lon(i) = -999.9
      old_lat(i) = -999.9
      enddo
      do i=1,im
      old_dlam(i) = 0.0
      enddo
      do j=1,jm
      old_dphi(j) = 0.0
      enddo

      else
             i =  size (old_dlam)
             j =  size (old_dphi)
             m =  size (old_lon)
          if(i.ne.im .or. j.ne.jm .or. m.ne.irun) then
          deallocate (  old_dlam ,  old_dphi )
          deallocate (   old_lon ,   old_lat )
          deallocate ( wl_ip0jp0 , wl_im1jp0 )
          deallocate ( wl_ip0jm1 , wl_im1jm1 )
          deallocate ( wc_ip1jp1 , wc_ip0jp1 , wc_im1jp1 , wc_im2jp1 )
          deallocate ( wc_ip1jp0 , wc_ip0jp0 , wc_im1jp0 , wc_im2jp0 )
          deallocate ( wc_ip1jm1 , wc_ip0jm1 , wc_im1jm1 , wc_im2jm1 )
          deallocate ( wc_ip1jm2 , wc_ip0jm2 , wc_im1jm2 , wc_im2jm2 )
          deallocate (       ip1 ,       ip0 ,       im1 ,       im2 )
          deallocate (       jp1 ,       jp0 ,       jm1 ,       jm2 )
          allocate (   old_dlam(im)  ,   old_dphi(jm)  )
          allocate (   old_lon(irun) ,   old_lat(irun) )
          allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
          allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
          allocate ( wc_ip1jp1(irun) , wc_ip0jp1(irun) , wc_im1jp1(irun) , wc_im2jp1(irun) )
          allocate ( wc_ip1jp0(irun) , wc_ip0jp0(irun) , wc_im1jp0(irun) , wc_im2jp0(irun) )
          allocate ( wc_ip1jm1(irun) , wc_ip0jm1(irun) , wc_im1jm1(irun) , wc_im2jm1(irun) )
          allocate ( wc_ip1jm2(irun) , wc_ip0jm2(irun) , wc_im1jm2(irun) , wc_im2jm2(irun) )
          allocate (       ip1(irun) ,       ip0(irun) ,       im1(irun) ,       im2(irun) )
          allocate (       jp1(irun) ,       jp0(irun) ,       jm1(irun) ,       jm2(irun) )
          do i=1,irun
          old_lon(i) = -999.9
          old_lat(i) = -999.9
          enddo
          do i=1,im
          old_dlam(i) = 0.0
          enddo
          do j=1,jm
          old_dphi(j) = 0.0
          enddo
          endif
      endif

c Compute Input Computational-Grid Latitude and Longitude Locations
c -----------------------------------------------------------------
      lon_cmp(1) = -pi
      do i=2,im
      lon_cmp(i) = lon_cmp(i-1) + dlam(i-1)
      enddo
      lat_cmp(1) = -pi*0.5
      do j=2,jm-1
      lat_cmp(j) = lat_cmp(j-1) + dphi(j-1)
      enddo
      lat_cmp(jm) =  pi*0.5

c Check for Co-incident Grid-Point Latitude and Pole Locations
c ------------------------------------------------------------
                                          eps_np = 0.0
      do j=1,jm
      phi_cmp = lat_cmp(j)*180./pi
      if( abs( phi_cmp-tilt ).lt.1.e-3 )  eps_np =  1.e-3
      if( tilt+eps_np .gt. 90. )          eps_np = -1.e-3
      enddo

      lam_np = pi/180.*rotation
      phi_np = pi/180.*(tilt+eps_np)
      lam_0  = pi/180.*precession

      if( tilt.eq.90. ) then 
      cosnp = 0.0
      sinnp = 1.0
      else if(tilt.eq.-90.0) then
      cosnp = 0.0
      sinnp =-1.0
      else
      cosnp = cos(phi_np)
      sinnp = sin(phi_np)
      endif

c Determine if Weights Need to be Updated
c ---------------------------------------
      compute_weights =  rotation.ne.old_rotation .or.
     .                       tilt.ne.old_tilt     .or.
     .                 precession.ne.old_precession

      m = 1
      do while ( .not.compute_weights .and. m.le.irun )
      compute_weights = (lon_geo(m).ne.old_lon(m)) .or.
     .                  (lat_geo(m).ne.old_lat(m))
      m = m+1
      enddo
      i = 1
      do while ( .not.compute_weights .and. i.le.im )
      compute_weights = dlam(i).ne.old_dlam(i)
      i = i+1
      enddo
      j = 1
      do while ( .not.compute_weights .and. j.le.jm-1 )
      compute_weights = dphi(j).ne.old_dphi(j)
      j = j+1
      enddo

c Compute Weights for Computational to Geophysical Grid Interpolation
c -------------------------------------------------------------------
      if( compute_weights ) then

      old_rotation   = rotation
      old_tilt       = tilt
      old_precession = precession

#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (i,lam_geo,phi_geo,lam_cmp,phi_cmp,lam,phi)
!$omp& private (p1,p2,p3,d,icmp,jcmp,im1_cmp,jm1_cmp)
!$omp& private (lam_im2, lam_im1, lam_ip0, lam_ip1)
!$omp& private (phi_jm2, phi_jm1, phi_jp0, phi_jp1)
!$omp& private (ap1, ap0, am1, am2)
!$omp& private (bp1, bp0, bm1, bm2)
#endif
      do i=1,irun
      old_lon(i) = lon_geo(i)
      old_lat(i) = lat_geo(i)
      lam_geo    = lon_geo(i)
      phi_geo    = lat_geo(i)

      p1 = cosnp*cos(phi_geo)*cos(lam_geo+lam_0-pi)
     .   + sin(phi_geo)*sinnp
      p1 = min(p1, 1.0)
      p1 = max(p1,-1.0)
      phi_cmp = asin( p1 )

      if( tilt.eq.90.0 .or. tilt.eq.-90.0 ) then
      p2 = sinnp*cos(lam_geo+lam_0-pi)
      else
      p2 = sinnp*cos(phi_geo)*cos(lam_geo+lam_0-pi)
     .   - sin(phi_geo)*cosnp
      p2 = p2 / max( cos(phi_cmp),eps )
      p2 = min(p2, 1.0)
      p2 = max(p2,-1.0)
      endif
      p2 = acos( p2 )

      p3 = cos(phi_geo)*sin(lam_geo+lam_0-pi)
      if( p3.lt.0.0 ) p2 = -p2
      p2 = p2 + lam_np - pi
      lam_cmp = mod( p2+3.0*pi,2.0*pi ) - pi

c Determine Indexing Based on Computational Grid
c ----------------------------------------------
      im1_cmp = 1
      do icmp = 2,im
      if( lon_cmp(icmp).lt.lam_cmp ) im1_cmp = icmp
      enddo
      jm1_cmp = 1
      do jcmp = 2,jm
      if( lat_cmp(jcmp).lt.phi_cmp ) jm1_cmp = jcmp
      enddo

      im1(i) = im1_cmp
      ip0(i) = im1(i) + 1
      ip1(i) = ip0(i) + 1
      im2(i) = im1(i) - 1

      jm1(i) = jm1_cmp
      jp0(i) = jm1(i) + 1
      jp1(i) = jp0(i) + 1
      jm2(i) = jm1(i) - 1

c Fix Longitude Index Boundaries
c ------------------------------
      if(im1(i).eq.im) then
      ip0(i) = 1
      ip1(i) = 2
      endif
      if(im1(i).eq.1) then
      im2(i) = im
      endif
      if(ip0(i).eq.im) then
      ip1(i) = 1
      endif


c Compute Immediate Surrounding Coordinates
c -----------------------------------------
      lam     =  lam_cmp
      phi     =  phi_cmp

c Compute and Adjust Longitude Weights
c ------------------------------------
      lam_im2 =  lon_cmp(im2(i))
      lam_im1 =  lon_cmp(im1(i))
      lam_ip0 =  lon_cmp(ip0(i))
      lam_ip1 =  lon_cmp(ip1(i))

      if( lam_im2.gt.lam_im1 ) lam_im2 = lam_im2 - 2*pi
      if( lam_im1.gt.lam_ip0 ) lam_ip0 = lam_ip0 + 2*pi
      if( lam_im1.gt.lam_ip1 ) lam_ip1 = lam_ip1 + 2*pi
      if( lam_ip0.gt.lam_ip1 ) lam_ip1 = lam_ip1 + 2*pi


c Compute and Adjust Latitude Weights   
c Note:  Latitude Index Boundaries are Adjusted during Interpolation
c ------------------------------------------------------------------
      phi_jm1 = lat_cmp(jm1(i))
      phi_jp0 = lat_cmp(jp0(i))

      if( jm2(i).eq.0    ) then
          phi_jm2 = phi_jm1 - dphi(1)
      else
          phi_jm2 = lat_cmp(jm2(i))
      endif

      if( jm1(i).eq.jm   ) then
                           phi_jp0 = phi_jm1 + dphi(jm-1)
                           phi_jp1 = phi_jp0 + dphi(jm-2)
      endif

      if( jp1(i).eq.jm+1 ) then
          phi_jp1 = phi_jp0 + dphi(jm-1)
      else
          phi_jp1 = lat_cmp(jp1(i))
      endif


c Bi-Linear Weights
c -----------------
              d    = (lam_ip0-lam_im1)*(phi_jp0-phi_jm1)
      wl_im1jm1(i) = (lam_ip0-lam    )*(phi_jp0-phi    )/d
      wl_ip0jm1(i) = (lam    -lam_im1)*(phi_jp0-phi    )/d
      wl_im1jp0(i) = (lam_ip0-lam    )*(phi    -phi_jm1)/d
      wl_ip0jp0(i) = (lam    -lam_im1)*(phi    -phi_jm1)/d

c Bi-Cubic Weights
c ----------------
      ap1 = ( (lam    -lam_ip0)*(lam    -lam_im1)*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_ip0)*(lam_ip1-lam_im1)*(lam_ip1-lam_im2) )
      ap0 = ( (lam_ip1-lam    )*(lam    -lam_im1)*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_ip0)*(lam_ip0-lam_im1)*(lam_ip0-lam_im2) )
      am1 = ( (lam_ip1-lam    )*(lam_ip0-lam    )*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_im1)*(lam_ip0-lam_im1)*(lam_im1-lam_im2) )
      am2 = ( (lam_ip1-lam    )*(lam_ip0-lam    )*(lam_im1-lam    ) )
     .    / ( (lam_ip1-lam_im2)*(lam_ip0-lam_im2)*(lam_im1-lam_im2) )

      bp1 = ( (phi    -phi_jp0)*(phi    -phi_jm1)*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jp0)*(phi_jp1-phi_jm1)*(phi_jp1-phi_jm2) )
      bp0 = ( (phi_jp1-phi    )*(phi    -phi_jm1)*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jp0)*(phi_jp0-phi_jm1)*(phi_jp0-phi_jm2) )
      bm1 = ( (phi_jp1-phi    )*(phi_jp0-phi    )*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jm1)*(phi_jp0-phi_jm1)*(phi_jm1-phi_jm2) )
      bm2 = ( (phi_jp1-phi    )*(phi_jp0-phi    )*(phi_jm1-phi    ) )
     .    / ( (phi_jp1-phi_jm2)*(phi_jp0-phi_jm2)*(phi_jm1-phi_jm2) )

      wc_ip1jp1(i) = bp1*ap1
      wc_ip0jp1(i) = bp1*ap0
      wc_im1jp1(i) = bp1*am1
      wc_im2jp1(i) = bp1*am2

      wc_ip1jp0(i) = bp0*ap1
      wc_ip0jp0(i) = bp0*ap0
      wc_im1jp0(i) = bp0*am1
      wc_im2jp0(i) = bp0*am2

      wc_ip1jm1(i) = bm1*ap1
      wc_ip0jm1(i) = bm1*ap0
      wc_im1jm1(i) = bm1*am1
      wc_im2jm1(i) = bm1*am2

      wc_ip1jm2(i) = bm2*ap1
      wc_ip0jm2(i) = bm2*ap0
      wc_im1jm2(i) = bm2*am1
      wc_im2jm2(i) = bm2*am2

      enddo
      endif

c Interpolate Computational-Grid Quantities to Geophysical Grid Using Bi-Linear
c -----------------------------------------------------------------------------
      if( abs(norder).eq.1 ) then

      if( check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,q_tmp)
#endif
      do L=1,lm
      do i=1,irun

      if( q_cmp( im1(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jp0(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef ) then

      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )

      else
      q_tmp(i) = undef
      endif
      enddo

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      if( .not.check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,q_tmp)
#endif
      do L=1,lm
      do i=1,irun
      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
      enddo

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      endif      ! End Check for Bi-Linear Interpolation


c Interpolate Computational-Grid Quantities to Geophysical Grid Using Bi-Cubic
c ----------------------------------------------------------------------------
      if( abs(norder).eq.3 ) then

      if( check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,m,n,q_tmp)
!$omp& private (ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1)
!$omp& private (ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2)
!$omp& private (jp1_for_jp1, jm2_for_jm2)
#endif
      do L=1,lm
      do i=1,irun

      ip1_for_jp1 = ip1(i)
      ip0_for_jp1 = ip0(i)
      im1_for_jp1 = im1(i)
      im2_for_jp1 = im2(i)
      jp1_for_jp1 = jp1(i)
                m = 1

      if( jp0(i).eq.jm ) then
      ip1_for_jp1 = 1 + mod( ip1_for_jp1 + im/2 -1, im )
      ip0_for_jp1 = 1 + mod( ip0_for_jp1 + im/2 -1, im )
      im1_for_jp1 = 1 + mod( im1_for_jp1 + im/2 -1, im )
      im2_for_jp1 = 1 + mod( im2_for_jp1 + im/2 -1, im )
      jp1_for_jp1 = jm-1
      if(msgn.eq.-1) m=-1
      endif

      ip1_for_jm2 = ip1(i)
      ip0_for_jm2 = ip0(i)
      im1_for_jm2 = im1(i)
      im2_for_jm2 = im2(i)
      jm2_for_jm2 = jm2(i)
                n = 1

      if( jm1(i).eq.1 ) then
      ip1_for_jm2 = 1 + mod( ip1_for_jm2 + im/2 -1, im )
      ip0_for_jm2 = 1 + mod( ip0_for_jm2 + im/2 -1, im )
      im1_for_jm2 = 1 + mod( im1_for_jm2 + im/2 -1, im )
      im2_for_jm2 = 1 + mod( im2_for_jm2 + im/2 -1, im )
      jm2_for_jm2 = 2
      if(msgn.eq.-1) n=-1
      endif


      if( q_cmp( ip1(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( im1(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( im2(i),jp0(i),L ).ne.undef .and.

     .    q_cmp( ip1(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( ip0(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( im1(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( im2(i),jm1(i),L ).ne.undef .and.

     .    q_cmp( ip1_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( ip0_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( im1_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( im2_for_jm2,jm2_for_jm2,L ).ne.undef  .and.

     .    q_cmp( ip1_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( ip0_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( im1_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( im2_for_jp1,jp1_for_jp1,L ).ne.undef ) then

      q_tmp(i) = wc_ip1jp1(i) * q_cmp( ip1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_ip0jp1(i) * q_cmp( ip0_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im1jp1(i) * q_cmp( im1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im2jp1(i) * q_cmp( im2_for_jp1,jp1_for_jp1,L )*m

     .         + wc_ip1jp0(i) * q_cmp( ip1(i),jp0(i),L )
     .         + wc_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
     .         + wc_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wc_im2jp0(i) * q_cmp( im2(i),jp0(i),L )

     .         + wc_ip1jm1(i) * q_cmp( ip1(i),jm1(i),L )
     .         + wc_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wc_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wc_im2jm1(i) * q_cmp( im2(i),jm1(i),L )

     .         + wc_ip1jm2(i) * q_cmp( ip1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_ip0jm2(i) * q_cmp( ip0_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im1jm2(i) * q_cmp( im1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im2jm2(i) * q_cmp( im2_for_jm2,jm2_for_jm2,L )*n


      elseif( q_cmp( im1(i),jm1(i),L ).ne.undef  .and.
     .        q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .        q_cmp( im1(i),jp0(i),L ).ne.undef  .and.
     .        q_cmp( ip0(i),jp0(i),L ).ne.undef ) then

      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )

      else
      q_tmp(i) = undef
      endif
      enddo

c Check for Positive Definite
c ---------------------------
      if( norder.lt.0 ) then
      do i=1,irun
      if( q_tmp(i).ne.undef .and.
     .    q_tmp(i).lt.0.0 )  then
          q_tmp(i) = 0.0
      endif
      enddo
      endif

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      if( .not.check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,m,n,q_tmp)
!$omp& private (ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1)
!$omp& private (ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2)
!$omp& private (jp1_for_jp1, jm2_for_jm2)
#endif
      do L=1,lm
      do i=1,irun

      ip1_for_jp1 = ip1(i)
      ip0_for_jp1 = ip0(i)
      im1_for_jp1 = im1(i)
      im2_for_jp1 = im2(i)
      jp1_for_jp1 = jp1(i)
                m = 1

      if( jp0(i).eq.jm ) then
      ip1_for_jp1 = 1 + mod( ip1_for_jp1 + im/2 -1, im )
      ip0_for_jp1 = 1 + mod( ip0_for_jp1 + im/2 -1, im )
      im1_for_jp1 = 1 + mod( im1_for_jp1 + im/2 -1, im )
      im2_for_jp1 = 1 + mod( im2_for_jp1 + im/2 -1, im )
      jp1_for_jp1 = jm-1
      if(msgn.eq.-1) m=-1
      endif

      ip1_for_jm2 = ip1(i)
      ip0_for_jm2 = ip0(i)
      im1_for_jm2 = im1(i)
      im2_for_jm2 = im2(i)
      jm2_for_jm2 = jm2(i)
                n = 1

      if( jm1(i).eq.1 ) then
      ip1_for_jm2 = 1 + mod( ip1_for_jm2 + im/2 -1, im )
      ip0_for_jm2 = 1 + mod( ip0_for_jm2 + im/2 -1, im )
      im1_for_jm2 = 1 + mod( im1_for_jm2 + im/2 -1, im )
      im2_for_jm2 = 1 + mod( im2_for_jm2 + im/2 -1, im )
      jm2_for_jm2 = 2
      if(msgn.eq.-1) n=-1
      endif

      q_tmp(i) = wc_ip1jp1(i) * q_cmp( ip1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_ip0jp1(i) * q_cmp( ip0_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im1jp1(i) * q_cmp( im1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im2jp1(i) * q_cmp( im2_for_jp1,jp1_for_jp1,L )*m

     .         + wc_ip1jp0(i) * q_cmp( ip1(i),jp0(i),L )
     .         + wc_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
     .         + wc_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wc_im2jp0(i) * q_cmp( im2(i),jp0(i),L )

     .         + wc_ip1jm1(i) * q_cmp( ip1(i),jm1(i),L )
     .         + wc_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wc_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wc_im2jm1(i) * q_cmp( im2(i),jm1(i),L )

     .         + wc_ip1jm2(i) * q_cmp( ip1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_ip0jm2(i) * q_cmp( ip0_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im1jm2(i) * q_cmp( im1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im2jm2(i) * q_cmp( im2_for_jm2,jm2_for_jm2,L )*n

      enddo

c Check for Positive Definite
c ---------------------------
      if( norder.lt.0 ) then
      do i=1,irun
      if( q_tmp(i).ne.undef .and.
     .    q_tmp(i).lt.0.0 )  then
          q_tmp(i) = 0.0
      endif
      enddo
      endif

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      endif      ! End Check for Bi-Cubic Interpolation

          deallocate (  old_dlam ,  old_dphi )
          deallocate (   old_lon ,   old_lat )
          deallocate ( wl_ip0jp0 , wl_im1jp0 )
          deallocate ( wl_ip0jm1 , wl_im1jm1 )
          deallocate ( wc_ip1jp1 , wc_ip0jp1 , wc_im1jp1 , wc_im2jp1 )
          deallocate ( wc_ip1jp0 , wc_ip0jp0 , wc_im1jp0 , wc_im2jp0 )
          deallocate ( wc_ip1jm1 , wc_ip0jm1 , wc_im1jm1 , wc_im2jm1 )
          deallocate ( wc_ip1jm2 , wc_ip0jm2 , wc_im1jm2 , wc_im2jm2 )
          deallocate (       ip1 ,       ip0 ,       im1 ,       im2 )
          deallocate (       jp1 ,       jp0 ,       jm1 ,       jm2 )

      return
      end

      subroutine ps_mod ( ps,gz1,tbr,gz2,im,jm )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Modify surface pressure using new topography
C
C  Input:
C  =====
C    ps ...... Surface Pressure consistent with Topography gz1
C    gz1 ..... Old Topography
C    tbr ..... Mean Temperature Used for SLP
C    gz2 ..... New Topography
C    im ...... Zonal      Dimension
C    jm ...... Meridional Dimension
C
C  Output:
C  =======
C    ps ...... Updated Surface Pressure-Ptop consistent with Topography gz2
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
 
      use MAPL_ConstantsMod
      implicit   none
      integer    im,jm
      real    ps(im,jm)
      real    gz1(im,jm)
      real    gz2(im,jm)
      real    tbr(im,jm)
      real    k,r,g,beta
      real    psl,slp,phis1,phis2,tbar,ts1,f,fp
      real    phis,ps1,ps2,ps20,psurf
      integer i,j,n
 
      psl(psurf,phis,tbar) = psurf*exp(phis/(r*tbar))
      f(ps2) = ps2-slp*exp(-phis2/(r*(ts1*(ps2/ps1)**k
     .       + beta*phis2/(2*g))))
 
      fp(ps2) = 1-slp*exp(-phis2/(r*(ts1*(ps2/ps1)**k
     .       + beta*phis2/(2*g))))
     .       *( phis2*ts1*k*(ps2/ps1)**k )
     .       /( r*ps2*(ts1*(ps2/ps1)**k+beta*phis2/(2*g))**2 )
 
      r = MAPL_RGAS
      g = MAPL_GRAV
      k = MAPL_KAPPA
 
      beta  = 0.0065
 
      do j=1,jm
      do i=1,im
      ps1   =  ps(i,j)
      phis1 = gz1(i,j)
      phis2 = gz2(i,j)
      tbar  = tbr(i,j)
 
      slp = psl(ps1,phis1,tbar)
      ts1 = tbar - beta*phis1/(2*g)
 
      ps2  = ps1
      ps20 = 0
      n    = 0
      do while ( abs( ps2-ps20 ).gt.1.e-5 .and. n.le.50 )
      ps20 = ps2
      ps2  = ps2 - f(ps2)/fp(ps2)
      n = n + 1
      enddo
 
      ps(i,j) = ps2
      enddo
      enddo
      return
      end

      subroutine atod ( qa,qd,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'A' gridded data                  ****
C ****                     to 'D' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted left (westward),                     ****
C ****        u is shifted down (southward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift left (-dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qa(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) + qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) - qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qd(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****             shift down (-dy/2)                  ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qa(i,j+1,L)
         qay(j+jmm1,L) = -qa(i+imh,jm-j,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) + qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) - qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qd(i,j+1,L)        =  qay(j,L)
         qd(i+imh,jm-j+1,L) = -qay(j+jmm1,L)
         enddo
         enddo
      enddo

      endif

      return
      end

      subroutine dtoa ( qd,qa,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'D' gridded data                  ****
C ****                     to 'A' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted right (eastward),                    ****
C ****        u is shifted up   (northward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift right (dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qd(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) - qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) + qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qa(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****               shift up (dy/2)                   ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qd(i,j+1,L)
         qay(j+jmm1,L) = -qd(i+imh,jm-j+1,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) - qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) + qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qa(i,j+1,L)      =  qay(j,L)
         qa(i+imh,jm-j,L) = -qay(j+jmm1,L)
         enddo
         enddo

      enddo

         do L=1,lm
         do i=1,imh
         qa(i+imh,jm,L) = -qa(i,jm,L)
         qa(i,1,L)      = -qa(i+imh,1,L)
         enddo
         enddo
      endif

      return
      end

      subroutine rfftmlt (a,work,trigs,ifax,inc,jump,n,lot,isign)
      integer INC, JUMP, N, LOT, ISIGN
      real(kind=KIND(1.0)) A(N),WORK(N),TRIGS(N)
      integer IFAX(*)
!
!     SUBROUTINE "FFT991" - MULTIPLE REAL/HALF-COMPLEX PERIODIC
!     FAST FOURIER TRANSFORM
!
!     SAME AS FFT99 EXCEPT THAT ORDERING OF DATA CORRESPONDS TO
!     THAT IN MRFFT2
!
!     PROCEDURE USED TO CONVERT TO HALF-LENGTH COMPLEX TRANSFORM
!     IS GIVEN BY COOLEY, LEWIS AND WELCH (J. SOUND VIB., VOL. 12
!     (1970), 315-337)
!
!     A IS THE ARRAY CONTAINING INPUT AND OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*LOT
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N/2
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N IS THE LENGTH OF THE DATA VECTORS
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:
!         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
!         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
!
!     ORDERING OF DATA:
!         X(0),X(1),X(2),...,X(N-1)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS IN
!     PARALLEL
!
!     *** N.B. N IS ASSUMED TO BE AN EVEN NUMBER
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!
!     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
!               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
!
! THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR
!     CALL Q8QST4 ( 4HXLIB, 6HFFT99F, 6HFFT991, 10HVERSION 01)
!FPP$ NOVECTOR R
      integer NFAX, NH, NX, INK
      integer I, J, IBASE, JBASE, L, IGO, IA, LA, K, M, IB

      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      INK=INC+INC
      IF (ISIGN.EQ.+1) GO TO 30
!
!     IF NECESSARY, TRANSFER DATA TO WORK AREA
      IGO=50
      IF (MOD(NFAX,2).EQ.1) GOTO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE
!
      IGO=60
      GO TO 40
!
!     PREPROCESSING (ISIGN=+1)
!     ------------------------
!
   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60
!
!     COMPLEX TRANSFORM
!     -----------------
!
   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF (IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS, 
     *    INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS, 
     *     2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE
!
      IF (ISIGN.EQ.-1) GO TO 130
!
!     IF NECESSARY, TRANSFER DATA FROM WORK AREA
      IF (MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE
!
!     FILL IN ZEROS AT END
  110 CONTINUE
      IB=N*INC+1
!DIR$ IVDEP
      DO 120 L=1,LOT
      A(IB)=0.0
      A(IB+INC)=0.0
      IB=IB+JUMP
  120 CONTINUE
      GO TO 140
!
!     POSTPROCESSING (ISIGN=-1):
!     --------------------------
!
  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
!
  140 CONTINUE
      RETURN
      END

      subroutine fftfax (n,ifax,trigs)
      integer IFAX(13)
      integer N
      REAL(kind=KIND(1.0))      TRIGS(*)
!
! MODE 3 IS USED FOR REAL/HALF-COMPLEX TRANSFORMS.  IT IS POSSIBLE
! TO DO COMPLEX/COMPLEX TRANSFORMS WITH OTHER VALUES OF MODE, BUT
! DOCUMENTATION OF THE DETAILS WERE NOT AVAILABLE WHEN THIS ROUTINE
! WAS WRITTEN.
!
      integer I, MODE
      DATA MODE /3/
!FPP$ NOVECTOR R
      CALL FAX (IFAX, N, MODE)
      I = IFAX(1)
      IF (IFAX(I+1) .GT. 5 .OR. N .LE. 4) IFAX(1) = -99
      IF (IFAX(1) .LE. 0 ) WRITE(6,FMT="(//5X, ' FFTFAX -- INVALID N =', I5,/)") N
      IF (IFAX(1) .LE. 0 ) STOP 999
      CALL FFTRIG (TRIGS, N, MODE)
      RETURN
      END

      subroutine fft99a (a,work,trigs,inc,jump,n,lot)
      integer inc, jump, N, lot
      real(kind=KIND(1.0)) A(N),WORK(N)
      REAL(kind=KIND(1.0)) TRIGS(N)
!
!     SUBROUTINE FFT99A - PREPROCESSING STEP FOR FFT99, ISIGN=+1
!     (SPECTRAL TO GRIDPOINT TRANSFORM)
!
!FPP$ NOVECTOR R
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) C, S
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
!DIR$ IVDEP
      DO 10 L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      WORK(JA)=(A(IA)+A(IB))- 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+ 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+ 
     *     (A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))- 
     *     (A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   20 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
!DIR$ IVDEP
      DO 40 L=1,LOT
      WORK(JA)=2.0*A(IA)
      WORK(JA+1)=-2.0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fft99b (work,a,trigs,inc,jump,n,lot)
      integer INC, JUMP, N, LOT
      real(kind=KIND(1.0)) WORK(N),A(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) SCALE
      real(kind=KIND(1.0)) C, S
!
!     SUBROUTINE FFT99B - POSTPROCESSING STEP FOR FFT99, ISIGN=-1
!     (GRIDPOINT TO SPECTRAL TRANSFORM)
!
!FPP$ NOVECTOR R
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      SCALE=1.0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1
!DIR$ IVDEP
      DO 10 L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JB)=SCALE*(WORK(IA)-WORK(IB))
      A(JA+INC)=0.0
      A(JB+INC)=0.0
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      SCALE=0.5*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB)) 
     *    +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB)) 
     *    -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   20 CONTINUE
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
      SCALE=2.0*SCALE
!DIR$ IVDEP
      DO 40 L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fax (ifax,n,mode)
      integer IFAX(10)
      integer N, MODE
!FPP$ NOVECTOR R
      integer NN, K, L, INC, II, ISTOP, ITEM, NFAX, I
      NN=N
      IF (IABS(MODE).EQ.1) GO TO 10
      IF (IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF ((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
   10 K=1
!     TEST FOR FACTORS OF 4
   20 IF (MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF (NN.EQ.1) GO TO 80
      GO TO 20
!     TEST FOR EXTRA FACTOR OF 2
   30 IF (MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF (NN.EQ.1) GO TO 80
!     TEST FOR FACTORS OF 3
   40 IF (MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF (NN.EQ.1) GO TO 80
      GO TO 40
!     NOW FIND REMAINING FACTORS
   50 L=5
      INC=2
!     INC ALTERNATELY TAKES ON VALUES 2 AND 4
   60 IF (MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF (NN.EQ.1) GO TO 80
      GO TO 60
   70 L=L+INC
      INC=6-INC
      GO TO 60
   80 IFAX(1)=K-1
!     IFAX(1) CONTAINS NUMBER OF FACTORS
      NFAX=IFAX(1)
!     SORT FACTORS INTO ASCENDING ORDER
      IF (NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
      END

      subroutine fftrig (trigs,n,mode)
      REAL(kind=KIND(1.0))      TRIGS(*)
      integer N, MODE
!FPP$ NOVECTOR R
      real(kind=KIND(1.0)) PI
      integer IMODE, NN, L, I, NH, LA
      real(kind=KIND(1.0)) DEL, ANGLE
      PI=2.0*ASIN(1.0)
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF (IMODE.EQ.1) RETURN
      IF (IMODE.EQ.8) RETURN
      DEL=0.5*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF (IMODE.LE.3) RETURN
      DEL=0.5*DEL
      LA=LA+NN
      IF (MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0*SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
      DEL=0.5*DEL
      DO 50 I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
      END

      subroutine vpassm (a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,ifac,la)
      integer INC1,INC2,INC3,INC4,LOT,N,IFAC,LA
      real(kind=KIND(1.0)) A(N),B(N),C(N),D(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
!
!     SUBROUTINE "VPASSM" - MULTIPLE VERSION OF "VPASSA"
!     PERFORMS ONE PASS THROUGH DATA
!     AS PART OF MULTIPLE COMPLEX FFT ROUTINE
!     A IS FIRST REAL INPUT VECTOR
!     B IS FIRST IMAGINARY INPUT VECTOR
!     C IS FIRST REAL OUTPUT VECTOR
!     D IS FIRST IMAGINARY OUTPUT VECTOR
!     TRIGS IS PRECALCULATED TABLE OF SINES & COSINES
!     INC1 IS ADDRESSING INCREMENT FOR A AND B
!     INC2 IS ADDRESSING INCREMENT FOR C AND D
!     INC3 IS ADDRESSING INCREMENT BETWEEN As & Bs
!     INC4 IS ADDRESSING INCREMENT BETWEEN Cs & Ds
!     LOT IS THE NUMBER OF VECTORS
!     N IS LENGTH OF VECTORS
!     IFAC IS CURRENT FACTOR OF N
!     LA IS PRODUCT OF PREVIOUS FACTORS
!
      real(kind=KIND(1.0)) SIN36, COS36, SIN72, COS72, SIN60
      DATA SIN36/0.587785252292473/,COS36/0.809016994374947/, 
     *      SIN72/0.951056516295154/,COS72/0.309016994374947/,
     *      SIN60/0.866025403784437/
      integer M, IINK, JINK, JUMP, IBASE, JBASE, IGO, IA, JA, IB, JB
      integer IC, JC, ID, JD, IE, JE
      integer I, J, K, L, IJK, LA1, KB, KC, KD, KE
      real(kind=KIND(1.0)) C1, S1, C2, S2, C3, S3, C4, S4
!
!FPP$ NOVECTOR R
      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.GT.4) RETURN
      GO TO (10,50,90,130),IGO
!
!     CODING FOR FACTOR 2
!
   10 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)-B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)-B(IB+I))
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 3
!
   50 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 55 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))
      C(JC+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))
      D(JB+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I)))
      D(JC+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I)))
      I=I+INC3
      J=J+INC4
   55 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 65 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)= 
     *     C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
   65 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 4
!
   90 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 95 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      D(JC+J)=(B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I))
      C(JB+J)=(A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))
      C(JD+J)=(A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))
      D(JB+J)=(B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I))
      D(JD+J)=(B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I))
      I=I+INC3
      J=J+INC4
   95 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 105 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      C(JC+J)= 
     *     C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    -S2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      D(JC+J)= 
     *     S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    +C2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      C(JB+J)= 
     *     C1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    -S1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)= 
     *     S1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    +C1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)= 
     *     C3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    -S3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)= 
     *     S3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    +C3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  105 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 5
!
  130 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      IE=ID+IINK
      JE=JD+JINK
      DO 140 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 135 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      C(JE+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      D(JB+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      D(JE+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      C(JC+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      C(JD+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      D(JC+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      D(JD+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  135 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  140 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 160 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      DO 150 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 145 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)= 
     *     C1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JE+J)= 
     *     C4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JE+J)= 
     *     S4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      C(JD+J)= 
     *     C3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JD+J)= 
     *     S3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      I=I+INC3
      J=J+INC4
  145 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  150 CONTINUE
      JBASE=JBASE+JUMP
  160 CONTINUE
      RETURN
      END

      subroutine minmax (q,im,jm,qmin,qmax)
      real*4 q(im,jm)
      qmin = q(1,1)
      qmax = q(1,1)
      do j=1,jm
      do i=1,im
      qmin = min( qmin,q(i,j) )
      qmax = max( qmax,q(i,j) )
      enddo
      enddo
      return
      end

      subroutine checkfile ( filename )
      character(*)  filename
      logical file_exists
      inquire( file=trim(filename), exist=file_exists )
      if( file_exists )  return
      print *
      print *, 'File: ',trim(filename),' does not exist!'
      print *
      stop
      end

      subroutine usage()
      write(6,100)
 100  format(  "Usage:  "                                                                                               ,/
     .                                                                                                                  ,/
     .         " rs_hinterp_$ARCH.x -dyn      DYNRST"                                                                   ,/
     .         "                    -moist    MOISTRST"                                                                 ,/
     .         "                    -other    OtherRST1 OtherRST2 OtherRST3 ..."                                        ,/
     .         "                    -topo_old TOPO_OLD"                                                                 ,/
     .         "                    -topo_new TOPO_NEW"                                                                 ,/
     .         "                    -im       IM_OUT"                                                                   ,/
     .         "                    -jm       JM_OUT"                                                                   ,/
     .                                                                                                                  ,/
     .         "where:"                                                                                                 ,/
     .                                                                                                                  ,/
     .         "  -dyn         DYNRST:  Filename for DYNAMICS_INTERNAL_RESTART"                                         ,/
     .         "  -moist     MOISTRST:  Filename for    MOIST_INTERNAL_RESTART"                                         ,/
     .         "  -other     OtherRST:  Filename for Other Flat Binary_RESTART"                                         ,/
     .         "  -topo_old  TOPO_OLD:  Filename for OLD Topography (associated with INPUT  resolution)"                ,/
     .         "  -topo_new  TOPO_NEW:  Filename for NEW Topography (associated with OUTPUT resolution)"                ,/
     .         "  -im          IM_OUT:  IM Dimension for Output"                                                        ,/
     .         "  -jm          JM_OUT:  JM Dimension for Output"                                                        ,/
     .                                                                                                                  ,/
     .         )
      call exit(7)
      end subroutine usage
