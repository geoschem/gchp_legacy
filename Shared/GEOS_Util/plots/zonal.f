      PROGRAM  main
      implicit none
      character*120  title
      character*120  begdate

      real,   allocatable ::     vz(:,:)
      real,   allocatable ::     tz(:,:)
      real,   allocatable ::     wz(:,:)
      real,   allocatable ::    thz(:,:)
      real,   allocatable ::  vptpz(:,:)
      real,   allocatable :: vpthpz(:,:)
      real,   allocatable ::     pl(:,:)
      real,   allocatable ::     pk(:,:)
      real,   allocatable ::   strm(:,:)
      real,   allocatable ::    res(:,:)
      real,   allocatable ::  vstar(:,:)
      real,   allocatable ::  wstar(:,:)
      real*4, allocatable ::    dum(:)

      character*120, allocatable :: arg(:)
      character*120  tag, output
      real         undef, lat0
      integer      im,jm,lm,tm
      integer      j,L,n,nt,lrec
      integer      rc,nargs,iargc

      undef = 1e15

      nargs = iargc()
      if( nargs.ne.0 ) then
          allocate( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-tag' ) tag = '.' // arg(n+1)
          enddo
       else
          tag = ""
       endif

c Read TXT Files to determine resolution
c --------------------------------------
      open (10,file='LAT0' // trim(tag) // '.txt',form='formatted')
      read (10,*) lat0
      print *, 'LAT0 = ',lat0
      close(10)
      open (10,file='XDIM' // trim(tag) // '.txt',form='formatted')
      read (10,*) im
      print *, 'IM = ',im
      close(10)
      open (10,file='YDIM' // trim(tag) // '.txt',form='formatted')
      read (10,*) jm
      print *, 'JM = ',jm
      close(10)
      open (10,file='ZDIM' // trim(tag) // '.txt',form='formatted')
      read (10,*) lm
      print *, 'LM = ',lm
      close(10)
      open (10,file='TDIM' // trim(tag) // '.txt',form='formatted')
      read (10,*) tm
      print *, 'TDIM = ',tm
      close(10)
      open (10,file='BEGDATE' // trim(tag) // '.txt',form='formatted')
      read (10,*) begdate
      close(10)

      allocate(     vz(jm,lm) )
      allocate(     tz(jm,lm) )
      allocate(     wz(jm,lm) )
      allocate(    thz(jm,lm) )
      allocate(  vptpz(jm,lm) )
      allocate( vpthpz(jm,lm) )
      allocate(     pl(jm,lm) )
      allocate(     pk(jm,lm) )
      allocate(   strm(jm,lm) )
      allocate(    res(jm,lm) )
      allocate(  vstar(jm,lm) )
      allocate(  wstar(jm,lm) )
      allocate(    dum(jm)    )
                                                                                                                                
c GRADS Datasets
c --------------
      open (10,file=   'grads' // trim(tag) // '.fwrite',form='unformatted',access='direct',recl=jm*4)
      open (20,file='residual' // trim(tag) // '.data'  ,form='unformatted',access='sequential')
      open (30,file='residual' // trim(tag) // '.ctl'   ,form='formatted')

c Read data from grads.fwrite
c ---------------------------
        nt = 0
        rc = 0
      lrec = 1
      do n=1,tm
         do L=1,lm
            read(10,rec=lrec,iostat=rc)  dum ;    vz(:,L) = dum(:) ; lrec = lrec+1
         enddo
         do L=1,lm
            read(10,rec=lrec,iostat=rc)  dum ;    tz(:,L) = dum(:) ; lrec = lrec+1
         enddo
         do L=1,lm
            read(10,rec=lrec,iostat=rc)  dum ; vptpz(:,L) = dum(:) ; lrec = lrec+1
         enddo
         do L=1,lm
            read(10,rec=lrec,iostat=rc)  dum ;    pl(:,L) = dum(:) ; lrec = lrec+1
         enddo
         do L=1,lm
            read(10,rec=lrec,iostat=rc)  dum ;    pk(:,L) = dum(:) ; lrec = lrec+1
         enddo

      nt = nt+1
      do L=1,lm
      do j=1,jm
      if( abs(tz(j,L)-undef).gt.0.1 ) then
           thz(j,L) =    tz(j,L)/pk(j,L)
        vpthpz(j,L) = vptpz(j,L)/pk(j,L)
      else
           thz(j,L) = undef
        vpthpz(j,L) = undef
      endif
      enddo
      enddo

c Compute Meridional Streamfunction
c ---------------------------------
      call stream ( vz,pl,jm,lm,strm,undef )
 
c Compute Mean Vertical Velocity from Continuity
c ----------------------------------------------
      call make_w ( vz,pl,jm,lm,wz,undef )

c Compute Residual Circulation
c ----------------------------
      call residual ( vz,vpthpz,thz,wz,pl,jm,lm,res,vstar,wstar,undef )
 
      call GLAWRT  (strm  ,jm,LM,20)
      call GLAWRT  (res   ,jm,LM,20)
      call GLAWRT  (vstar ,jm,LM,20)
      call GLAWRT  (wstar ,jm,LM,20)

      enddo
      close(10)

c Write Grads Control File
c ------------------------
      output = '^residual' // trim(tag) // '.data'
      title  = 'Streamfunction and Residual Circulation'

      write(30,101) trim(output),trim(title),undef,jm,lat0,2*abs(lat0)/(jm-1),lm
      do L=1,lm
      print *, 'Pressure = ',pl(1,L)
      write(30,102) pl(1,L)
      enddo
      print *, 'Finished   , nt = ',nt
      write(30,103) nt,trim(begdate),lm,lm,lm,lm
  101 format('dset  ',a,/,
     .       'title ',a,/,
     .       'options sequential ',/,
     .       'undef ',e15.6,/,
     .       'xdef  1 linear -180 1',/,
     .       'ydef ',i4,' linear ',f8.3,2x,f8.3,/,
     .       'zdef ',i3,' levels ')
  102 format(10x,f8.3)
  103 format('tdef ',i3,' linear ',a,' 1mo',/,
     .       'vars 4 ',/,
     .       'str   ',i3,' 0 Streamfunction',/,
     .       'res   ',i3,' 0 Residual Circulation',/,
     .       'vstar ',i3,' 0 Vstar',/,
     .       'wstar ',i3,' 0 wstar',/,
     .       'endvars')

      stop
      end

      SUBROUTINE ZONAL ( A,AZ,IM,JNP,undef )
      DIMENSION  A(IM,JNP), AZ(JNP)

      DO J=1,JNP
      AZ(J) = 0.0
      IC    = 0
      DO   I = 1, IM
      IF( abs(A(I,J)-UNDEF).gt.0.1 )  THEN
      AZ(J) = AZ(J) + A(I,J)
      IC    = IC + 1
      ENDIF
      ENDDO
      IF(IC.NE.0) AZ(J) = AZ(J) / IC
      IF(IC.eq.0) AZ(J) = UNDEF
      ENDDO

      RETURN
      END
      SUBROUTINE  GLAWRT  (A, IM,LM, KTP)
      real        A   (IM,LM)
      real*4      TEM (IM)
      DO  L=1,LM
      DO  I=1,IM
      if( abs(a(i,L)).gt.1.e-20 ) then
      TEM (I) = A(I,L)
      else
      TEM (I) = 0.
      endif
      ENDDO
      WRITE(KTP)  TEM
      ENDDO
      RETURN
      END

      SUBROUTINE  GLAWRT2 (A, IM, JM,LM, KTP, undef)
      real        A   (JM,LM)
      real        TEM (JM)
      real       DPHI (JM),  qout(181)
      real lon,lat,lons(181),lats(181)
      real*4                  dum(181)
      real :: dlam(IM)

      pi      = 4.0*atan(1.0)
      dlam    = 1
      dphi(:) = pi/(jm-1)

       iout = 1
       jout = 181
      dlout = 2*pi/iout
      dpout =  pi/(jout-1)

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

      DO  L=1,LM
      DO  J=1,JM
      if( abs(a(J,L)).gt.1.e-20 ) then
      TEM (J) = A(J,L)
      else
      TEM (J) = 0.
      endif
      ENDDO
      call interp_h ( tem,1,JM,1,
     .                dlam,dphi,0.0,90.0,0.0,
     .                qout,iout*jout,lons,lats,
     .                undef )
                  dum = qout
      WRITE(KTP)  dum
      ENDDO
      RETURN
      END

      subroutine stream ( v0,p0,jm,lm,s,undef )
      use MAPL_ConstantsMod
      implicit none
      integer j,k,L,jm,lm
      real pi,dp,a,g,const,delp,phi,undef
       
      real  v(jm,lm), v0(jm,lm)
      real  s(jm,lm)
      real p0(jm,lm),  p(jm,lm)

c Invert VWND and P level index (in order to be top=>bottom)
c ----------------------------------------------------------
      do L=1,lm
      p(:,L) = p0(:,lm-L+1)
      v(:,L) = v0(:,lm-L+1)
      enddo

      pi = 4.*atan(1.)
      dp = pi/(jm-1)
      a  = MAPL_RADIUS
      g  = MAPL_GRAV

      const = 2*pi*a/g*100 / 1.e10

      do k=1,lm
      do j=1,jm
      s(j,k) = 0.0
      enddo

      do L=1,k
      do j=1,jm
                   delp = (p(j,L+1)-p(j,L-1))/2
      if(L.eq.1)   delp = (p(j,L+1)-p(j,L)  )/2
      if(L.eq.lm)  delp = (p(j,L)  -p(j,L-1))/2
      phi = -pi/2+(j-1)*dp
      if( abs(v(j,L)-undef).gt.0.1 ) then
      s(j,k) = s(j,k) + v(j,l)*delp*cos(phi)*const
      endif
      enddo
      enddo
      enddo

c Invert Streamfunction for grads output (in order to be bottom=>top)
c -------------------------------------------------------------------
      do k=1,lm
      do j=1,jm
      v(j,k) = s(j,lm-k+1)
      enddo
      enddo

      do k=1,lm
      do j=1,jm
      s(j,k) = v(j,k)
      enddo
      enddo

      return
      end
      subroutine residual ( v0,vpthp0,th0,w0,p0,jm,lm,res,vstar,wstar,undef )
      use MAPL_ConstantsMod
      implicit none
      integer j,k,L,jm,lm
      real pi,dp,a,g,H,ps,ts,rhos,z,phi,undef,delp
      real airmw,runiv,cpd,rgas,akap
       
      real     v0(jm,lm),   v(jm,lm)
      real     w0(jm,lm),   w(jm,lm)
      real vpthp0(jm,lm), th0(jm,lm)
      real vpthp (jm,lm), th (jm,lm), dthdp(jm,lm)
      real stuff (jm,lm)
      real  res  (jm,lm)
      real  vstar(jm,lm)
      real  wstar(jm,lm)
      real    s  (jm,lm)
      real p0(jm,lm), p(jm,lm), rho0(jm,lm)
      real   cosp(jm)
      real ddcosp(jm,lm)
      logical defined

      PARAMETER ( AIRMW  = MAPL_AIRMW  )
      PARAMETER ( RUNIV  = MAPL_RUNIV  )
      PARAMETER ( CPD    = MAPL_CP     )
      PARAMETER ( RGAS   = RUNIV/AIRMW )
      PARAMETER ( AKAP   = MAPL_KAPPA  )

c Invert v,th,vpthp, and P level index (in order to be top=>bottom)
c -----------------------------------------------------------------
      do L=1,lm
          w(:,L) =     w0(:,lm-L+1)
          v(:,L) =     v0(:,lm-L+1)
          p(:,L) =     p0(:,lm-L+1)
         th(:,L) =    th0(:,lm-L+1)
      vpthp(:,L) = vpthp0(:,lm-L+1)
      enddo

      pi = 4.*atan(1.)
      dp = pi/(jm-1)
      a  = MAPL_RADIUS
      g  = MAPL_GRAV
      H  = 7000.0
      ps = 1000.0
      ts =  240.0
      rhos = ps/(rgas*ts)


c Compute Mean Air Density
c ------------------------
      do L=1,lm
      do j=1,jm
             z  = -H*log(p(j,L)/ps)
      rho0(j,L) = rhos*exp(-z/H)
      enddo
      enddo
      

c Compute D(Theta)/DZ (with a forced minimum to prevent dthdz => 0)
c -----------------------------------------------------------------
      do L=2,lm-1
      do j=1,jm
         delp = p(j,L+1)-p(j,L-1)
         if( defined(th(j,L+1),undef )  .and.
     .       defined(th(j,L-1),undef ) ) then
             dthdp(j,L) = min( -0.003, ( th(j,L+1)-th(j,L-1) )/ delp )
         else
             dthdp(j,L) = undef
         endif
      enddo
      enddo
      do j=1,jm
      dthdp(j,1)  = dthdp(j,2)
      dthdp(j,lm) = dthdp(j,lm-1)
      enddo


c Compute D(cos*vpthp/dthdz)/Dphi
c -------------------------------
      do L=1,lm
      do j=1,jm
      phi = -pi/2 + (j-1)*dp
      cosp(j) = cos(phi)
      if( defined(vpthp(j,L),undef)  .and. 
     .    defined(dthdp(j,L),undef)  .and.
     .            dthdp(j,L).ne.0.0 ) then
                  stuff(j,L) = -H*cosp(j)*vpthp(j,L)/(p(j,L)*dthdp(j,L))
      else
                  stuff(j,L) = undef
      endif
      enddo
      enddo

      do L=1,lm
      do j=1,jm
      if( j.gt.1 .and. j.lt.jm ) then
          if( defined(stuff(j+1,L),undef)  .and. 
     .        defined(stuff(j-1,L),undef) ) then 
              ddcosp(j,L) = ( stuff(j+1,L)-stuff(j-1,L) )/(2*dp)
          else
              ddcosp(j,L) = undef
          endif
      else
              ddcosp(j,L) = undef
      endif
      enddo
      enddo

c Compute Wstar
c -------------
      do L=1,lm
      do j=1,jm
      if( defined(ddcosp(j,L),undef) ) then
           wstar(j,Lm-L+1) = w(j,L) + ddcosp(j,L)/(a*cosp(j))
      else
           wstar(j,Lm-L+1) =  undef
      endif
      enddo
      enddo

c Compute D(rho*vpthp/dthdz)/DZ
c -----------------------------
      do L=1,lm
      do j=1,jm
      if( defined(dthdp(j,L),undef) .and.
     .    defined(vpthp(j,L),undef) .and.
     .    dthdp(j,L).ne.0.0       ) then
          stuff(j,L) = rho0(j,L)*vpthp(j,L)/(p(j,L)*dthdp(j,L))
      else
          stuff(j,L) = undef
      endif
      enddo
      enddo

      do L=2,lm-1
      do j=1,jm
                   delp = p(j,L+1)-p(j,L-1)
      if( defined(stuff(j,L+1),undef)  .and. 
     .    defined(stuff(j,L-1),undef) ) then 
                res(j,L) = ( stuff(j,L+1)-stuff(j,L-1) )/ delp
      else
                res(j,L) = undef
      endif
      enddo
      enddo
      do j=1,jm
      res(j,1)  = res(j,2)
      res(j,lm) = res(j,lm-1)
      enddo

c Compute Vstar
c -------------
      do L=1,lm
      do j=1,jm
      if( defined( res(j,L),undef)  .and.
     .    defined(   v(j,L),undef) ) then
          res(j,L) = v(j,L) - p(j,L)*res(j,L)/rho0(j,L)
      else
          res(j,L) = undef
      endif
      vstar(j,LM-L+1) = res(j,L)
      enddo
      enddo

c Construct Streamfunction for Vstar
c ----------------------------------
      do k=1,lm
         do j=1,jm
          s(j,k) = 0.0
         enddo

         do L=1,k
         do j=1,jm
                      delp = (p(j,L+1)-p(j,L-1))/2
         if(L.eq.1)   delp = (p(j,L+1)-p(j,L)  )/2
         if(L.eq.lm)  delp = (p(j,L)  -p(j,L-1))/2
         if( defined(res(j,L),undef) ) then
             s(j,k) = s(j,k) + res(j,L)*delp*cosp(j)*rho0(j,L)*H/p(j,L)
         endif
         enddo
         enddo
      enddo

c Invert Streamfunction for grads output (in order to be bottom=>top)
c -------------------------------------------------------------------
      do L=1,lm
      do j=1,jm
      res(j,L) = s(j,LM-L+1)
      enddo
      enddo

      return
      end

      subroutine make_w ( v0,p0,jm,lm,w,undef )
      use MAPL_ConstantsMod
      implicit none
      integer j,k,L,jm,lm
       
      real     v0(jm,lm),    v(jm,lm)
      real     p0(jm,lm),    p(jm,lm)
      real      w(jm,lm), rho0(jm,lm)
      real      s(jm,lm), cosp(jm)
      real  dvcos_dphi(jm,lm)
      logical defined
      real airmw,runiv,cpd,rgas,akap
      real pi,dp,a,g,H,ps,ts,rhos,phi,z,delp,undef

      PARAMETER ( AIRMW  = MAPL_AIRMW  )
      PARAMETER ( RUNIV  = MAPL_RUNIV  )
      PARAMETER ( CPD    = MAPL_CP     )
      PARAMETER ( RGAS   = RUNIV/AIRMW )
      PARAMETER ( AKAP   = MAPL_KAPPA  )

c Invert v and P level index (in order to be top=>bottom)
c -------------------------------------------------------
      do L=1,lm
         v(:,L) = v0(:,lm-L+1)
         p(:,L) = p0(:,lm-L+1)
      enddo

      pi = 4.*atan(1.)
      dp = pi/(jm-1)
      a  = MAPL_RADIUS
      g  = MAPL_GRAV
      H  = 7000.0
      ps = 1000.0
      ts =  240.0
      rhos = ps/(rgas*ts)

      do L=1,lm
      do j=1,jm
      phi = -pi/2 + (j-1)*dp
      cosp(j)   =  cos(phi)
             z  = -H*log(p(j,L)/ps)
      rho0(j,L) = rhos*exp(-z/H)
      enddo
      enddo

      do L=1,lm
      do j=1,jm
      if( j.gt.1 .and. j.lt.jm ) then
          if( defined(v(j+1,L),undef)  .and.
     .        defined(v(j-1,L),undef) ) then
              dvcos_dphi(j,L) = ( v(j+1,L)*cosp(j+1)-v(j-1,L)*cosp(j-1) )/(2*dp)
          else
              dvcos_dphi(j,L) = undef
          endif
      else
              dvcos_dphi(j,L) = undef
      endif
      enddo
      enddo


c Construct W from Continuity
c ---------------------------
      do k=1,lm
         do j=1,jm
         s(j,k) = undef
         enddo

         do L=1,k
         do j=1,jm
                      delp = (p(j,L+1)-p(j,L-1))/2
         if(L.eq.1)   delp = (p(j,L+1)-p(j,L)  )/2
         if(L.eq.lm)  delp = (p(j,L)  -p(j,L-1))/2
         phi = -pi/2+(j-1)*dp
         if( dvcos_dphi(j,L).ne.undef ) then
             if( s(j,k).eq.undef ) s(j,k) = 0.0
                 s(j,k) = s(j,k) + dvcos_dphi(j,L)*delp*rho0(j,L)*H/(p(j,L)*a*cosp(j))
         endif
         enddo
         enddo
         do j=1,jm
         if( s(j,k).ne.undef ) s(j,k) = s(j,k)/rho0(j,k)
         enddo
      enddo

c Invert Streamfunction for grads output (in order to be bottom=>top)
c -------------------------------------------------------------------
      do k=1,lm
      do j=1,jm
      w(j,k) = s(j,lm-k+1)
      enddo
      enddo

      return
      end

      subroutine interp_h ( q_cmp,im,jm,lm,
     .                      dlam,dphi,rotation,tilt,precession,
     .                      q_geo,irun,lon_geo,lat_geo,
     .                      undef )
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
      integer im,jm,lm,irun

      real      q_geo(irun,lm)
      real    lon_geo(irun)
      real    lat_geo(irun)

      real    q_cmp(im,jm,lm)
      real     dlam(im)
      real     dphi(jm)

c Local Variables
c ---------------
      integer  i,j,l,m
      integer, allocatable       :: ip0(:), im1(:)
      integer, allocatable       :: jp0(:), jm1(:)

c Bi-Linear Weights
c -----------------
      real, allocatable       ::    wl_ip0jp0 (:)
      real, allocatable       ::    wl_im1jp0 (:)
      real, allocatable       ::    wl_ip0jm1 (:)
      real, allocatable       ::    wl_im1jm1 (:)

      real, allocatable       ::    old_lon (:)
      real, allocatable       ::    old_lat (:)
      real, allocatable       ::    old_dlam(:)
      real, allocatable       ::    old_dphi(:)

      real    lon_cmp(im)
      real    lat_cmp(jm)
      real    q_tmp(irun)

      real    pi,cosnp,sinnp,p1,p2,p3,eps,d
      real    lam,lam_ip0,lam_im1
      real    phi,phi_jp0,phi_jm1
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
      allocate (       ip0(irun) ,       im1(irun) )
      allocate (       jp0(irun) ,       jm1(irun) )
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
          deallocate (       ip0 ,       im1 )
          deallocate (       jp0 ,       jm1 )
          allocate (   old_dlam(im)  ,   old_dphi(jm)  )
          allocate (   old_lon(irun) ,   old_lat(irun) )
          allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
          allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
          allocate (       ip0(irun) ,       im1(irun) )
          allocate (       jp0(irun) ,       jm1(irun) )
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

      jm1(i) = jm1_cmp
      jp0(i) = jm1(i) + 1

c Fix Longitude Index Boundaries
c ------------------------------
      if(im1(i).eq.im) then
      ip0(i) = 1
      endif

c Compute Immediate Surrounding Coordinates
c -----------------------------------------
      lam     =  lam_cmp
      phi     =  phi_cmp

c Compute and Adjust Longitude Weights
c ------------------------------------
      lam_im1 =  lon_cmp(im1(i))
      lam_ip0 =  lon_cmp(ip0(i))

      if( lam_im1.gt.lam_ip0 ) lam_ip0 = lam_ip0 + 2*pi


c Compute and Adjust Latitude Weights   
c Note:  Latitude Index Boundaries are Adjusted during Interpolation
c ------------------------------------------------------------------
      phi_jm1 =  lat_cmp(jm1(i))
      phi_jp0 =  lat_cmp(jp0(i))

      if( jm1(i).eq.jm ) phi_jp0 = phi_jm1 + dphi(jm-1)


c Linear Weights
c --------------
              d    = (phi_jp0-phi_jm1)
      wl_ip0jm1(i) = (phi_jp0-phi    )/d
      wl_ip0jp0(i) = (phi    -phi_jm1)/d

      enddo
      endif

c Interpolate Computational-Grid Quantities to Geophysical Grid Using Bi-Linear
c -----------------------------------------------------------------------------
      do L=1,lm
      do i=1,irun

      if( q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef ) then

      q_tmp(i) = wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
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

          deallocate (  old_dlam ,  old_dphi )
          deallocate (   old_lon ,   old_lat )
          deallocate ( wl_ip0jp0 , wl_im1jp0 )
          deallocate ( wl_ip0jm1 , wl_im1jm1 )
          deallocate (       ip0 ,       im1 )
          deallocate (       jp0 ,       jm1 )

      return
      end
      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*undef
      return
      end function defined
