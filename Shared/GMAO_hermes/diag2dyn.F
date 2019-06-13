
! Code originally by Wei Tan with minor mods by Arlindo da Silva for 
! portability. Name has changed from 'diag2ana' to 'diag2dyn'.
!
! !REVISION HISTORY:
!
! Dark Ages  Wei Tan    Original code 
! 13Dec2005  da Silva   Major rewrite. Replaced hxpak with GFIO and eliminated 
!                       reading of ana.eta file; on output only (lwi,ps,u,v,pt,q) 
!                       are filled which is sufficient for replay. These files 
!                       are not good enough for a PSAS based analysis. Built in
!                       options for horizontal and ertical remapping.
!

      program diag2ana

! External dependencies

      use m_realkinds, only: r4 => kind_r4
      use m_realkinds, only: r8 => kind_r8
      use m_die, only: die
      use m_set_eta, only: set_eta
      use m_const, only: undef
      use m_mapz, only: z_map
      use m_maph, only: h_map

      use m_dyn

      implicit none

! ana variables

      real, pointer     :: lon(:), lat(:)
      real, allocatable :: ak(:), bk(:)
      real              :: ptop

! diag variables

      real, allocatable :: ps1(:,:),ps2(:,:)
      real, allocatable :: lwi1(:,:),lwi2(:,:)
      real, allocatable :: delp1(:,:,:), delp2(:,:,:)
      real, allocatable :: uwnd1(:,:,:), uwnd2(:,:,:)
      real, allocatable :: vwnd1(:,:,:), vwnd2(:,:,:) 
      real, allocatable :: theta1(:,:,:), theta2(:,:,:)
      real, allocatable :: tmpu1(:,:,:), tmpu2(:,:,:)
      real, allocatable :: sphu1(:,:,:), sphu2(:,:,:)

! output ana variables
      type(dyn_vect)    :: w_a, w_b  ! w_b is interpolated, if needed
      real, pointer :: lwi(:,:)
      real, pointer :: ps(:,:)
      real, pointer :: delp(:,:,:)
      real, pointer :: uwnd(:,:,:)
      real, pointer :: vwnd(:,:,:)
      real, pointer :: theta(:,:,:)
      real, pointer :: sphu(:,:,:)

! ana variables that are set to undef here

! others

      integer   :: i, j, k, nx, ny, nz
      logical   :: verbose = .false.

      real, allocatable :: uo(:,:)
      real, allocatable :: vo(:,:)

      character(len=255) :: diag1_fn, diag2_fn, ana_fn, expid
      character(len=3)   :: inres, outres
      integer            :: im, jm, km, nq, lm=1, in, jn, kn
      integer            :: ks, rc, nymd, nhms, fid
      real :: pint

      character(len=*), parameter :: myname = 'diag2dyn.x'
      integer  :: READ_ONLY=1, rcs(10)
      integer :: time(2), date(2)

!                             --------

!     Parse command line
!     ------------------
      call parse ( diag1_fn, diag2_fn, ana_fn, 
     &             inres, outres, expid, im, jm, km, in, jn, kn,
     &             date, time, nymd, nhms, verbose )

!     Allocate work space
!     -------------------
      call malloc_()

!     Vertical coordinates
!     --------------------
      call set_eta ( km, ks, ptop, pint, ak, bk )

!     Initialize dynamics state vector to hiold analysis fields
!     ---------------------------------------------------------
      call dyn_init ( im, jm, km, lm, w_a, rc, ptop=ptop, ak=ak, bk=bk, ks=ks )
      if ( rc /= 0 ) call die(myname,'cannot create dyn_vect - out of mem?')
      lwi => w_a%lwi
      ps => w_a%ps
      delp => w_a%delp
      uwnd => w_a%u
      vwnd => w_a%v
      theta => w_a%pt
      sphu => w_a%q(:,:,:,1)

!     Horizontal coordinates
!     ----------------------
      call grids_ ( w_a, lon, lat, im, jm )

      if (verbose) then
         print *, ' <> LON   = ', minval(lon),   maxval(lon) 
         print *, ' <> LAT   = ', minval(lat),   maxval(lat) 
         print *, ' <> Date  = ', date, nymd
         print *, ' <> Time  = ', time, nhms
      endif

!     Read first diag file
!     --------------------
      rcs = 0
      print *
      print *, ' [] Reading '//trim(diag1_fn)
      call GFIO_Open ( diag1_fn, READ_ONLY, fid, rcs(1) ) 
      call GFIO_GetVar ( fid, 'ORO', date(1), time(1), im, jm, 0, 1,  lwi1,  rcs(7) )
      call GFIO_GetVar ( fid, 'SURFP', date(1), time(1), im, jm, 0, 1,  ps1, rcs(2) )
      call GFIO_GetVar ( fid, 'U',   date(1), time(1), im, jm, 1, km, uwnd1, rcs(3) )
      call GFIO_GetVar ( fid, 'V',   date(1), time(1), im, jm, 1, km, vwnd1, rcs(4) )
      call GFIO_GetVar ( fid, 'T',   date(1), time(1), im, jm, 1, km, tmpu1, rcs(5) )
      call GFIO_GetVar ( fid, 'Q',   date(1), time(1), im, jm, 1, km, sphu1, rcs(6) )
      call GFIO_Close ( fid, rcs(7) )
      if ( any(rcs /= 0) ) call die(myname,'problems reading first diag file')

!     Read first diag file
!     --------------------
      rcs = 0
      print *, ' [] Reading '//trim(diag2_fn)
      call GFIO_Open ( diag2_fn, READ_ONLY, fid, rcs(1) ) 
      call GFIO_GetVar ( fid, 'ORO', date(2), time(2), im, jm, 0, 1,  lwi2,  rcs(7) )
      call GFIO_GetVar ( fid, 'SURFP', date(2), time(2), im, jm, 0, 1,  ps2, rcs(2) )
      call GFIO_GetVar ( fid, 'U',   date(2), time(2), im, jm, 1, km, uwnd2, rcs(3) )
      call GFIO_GetVar ( fid, 'V',   date(2), time(2), im, jm, 1, km, vwnd2, rcs(4) )
      call GFIO_GetVar ( fid, 'T',   date(2), time(2), im, jm, 1, km, tmpu2, rcs(5) )
      call GFIO_GetVar ( fid, 'Q',   date(2), time(2), im, jm, 1, km, sphu2, rcs(6) )
      call GFIO_Close ( fid, rcs(7) )
      if ( any(rcs /= 0) ) call die(myname,'problems reading first diag file')

!     Average diag fields fields
!     --------------------------
      ps   = 0.5 * (ps1 + ps2)
      sphu = 0.5 * (sphu1 + sphu2 )
      do j = 1, jm
         do i = 1, im
            lwi(i,j) = max ( lwi1(i,j), lwi2(i,j) ) ! ice will prevail
         end do
      end do

      if ( verbose ) then
         print *
         print *, ' <> PS = ', minval(ps1),   maxval(ps1) 
         print *, ' <> PS = ', minval(ps),    maxval(ps) 
         print *, ' <> PS = ', minval(ps2),   maxval(ps2)
         print *
         print *, ' <> LWI = ', minval(lwi1),   maxval(lwi1) 
         print *, ' <> LWI = ', minval(lwi),    maxval(lwi) 
         print *, ' <> LWI = ', minval(lwi2),   maxval(lwi2)
         print *
         print *, ' <> SPHU  = ', minval(sphu1), maxval(sphu1) 
         print *, ' <> SPHU  = ', minval(sphu),  maxval(sphu) 
         print *, ' <> SPHU  = ', minval(sphu2), maxval(sphu2) 
      end if

!     Calculate delp using ak, bk, and ps
!     -----------------------------------
      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
               delp1(i,j,k) = (ak(k+1) - ak(k))
     &                      + (bk(k+1) - bk(k)) * ps1(i,j)
               delp2(i,j,k) = (ak(k+1) - ak(k))
     &                      + (bk(k+1) - bk(k)) * ps2(i,j)
               delp(i,j,k)  = (ak(k+1) - ak(k))
     &                      + (bk(k+1) - bk(k)) * ps(i,j)
            enddo
         enddo
      enddo

      if (verbose) then
         print *
         print *, ' <> DELP  = ', minval(delp1), maxval(delp1) 
         print *, ' <> DELP  = ', minval(delp),  maxval(delp) 
         print *, ' <> DELP  = ', minval(delp2), maxval(delp2) 
      endif

!     Compute scaled potential temperature
!     ------------------------------------
#ifdef WEITAN_ALGORITHM
      call t2svpt ( tmpu1, sphu1, delp1, theta1, -1 , ptop, im, jm, km )
      call t2svpt ( tmpu2, sphu2, delp2, theta2, -1 , ptop, im, jm, km )
#else
      call tmpu2pt ( tmpu1, sphu1, delp1, im, jm, km, ks, ptop, theta1 )
      call tmpu2pt ( tmpu2, sphu2, delp2, im, jm, km, ks, ptop, theta2 )
#endif     

      theta = 0.5 * ( theta1 + theta2 )

      if (verbose) then
         print *
         print *, ' <> TMPU  = ', minval(tmpu1), maxval(tmpu1) 
         print *, ' <> TMPU  = ', minval(tmpu2), maxval(tmpu2) 
         print *
         print *, ' <> THETA  = ', minval(theta1), maxval(theta1) 
         print *, ' <> THETA  = ', minval(theta),  maxval(theta) 
         print *, ' <> THETA  = ', minval(theta2), maxval(theta2) 
      endif

!     A-grid to D-grid transformation for U and V
!     AMS Note: I changed the algorithm: since the 6-hour is smother than
!               the 3-hour mean it is better to interpolate the average
!     ------------------------------------------------------------------
      uwnd = 0.5 * ( uwnd1 + uwnd2 )
      vwnd = 0.5 * ( vwnd1 + vwnd2 )

      if ( verbose ) then
         print *
         print *, ' <> UWND-A  = ', minval(uwnd1), maxval(uwnd1) 
         print *, ' <> UWND-A  = ', minval(uwnd), maxval(uwnd) 
         print *, ' <> UWND-A  = ', minval(uwnd2), maxval(uwnd2) 
         print *
         print *, ' <> VWND-A  = ', minval(vwnd1), maxval(vwnd1) 
         print *, ' <> VWND-A  = ', minval(vwnd),  maxval(vwnd) 
         print *, ' <> VWND-A  = ', minval(vwnd2), maxval(vwnd2) 
      end if

      do k = 1, km
         call a2d(uwnd1(:,:,k),vwnd1(:,:,k),uo,vo,im,jm,lon)
         uwnd1(:,:,k) = uo(:,:)
         vwnd1(:,:,k) = vo(:,:)
         call a2d(uwnd2(:,:,k),vwnd2(:,:,k),uo,vo,im,jm,lon)
         uwnd2(:,:,k) = uo(:,:)
         vwnd2(:,:,k) = vo(:,:)
#ifdef WEITAN_ALGORITHM
      enddo
      uwnd = 0.5 * ( uwnd1 + uwnd2 )
      vwnd = 0.5 * ( vwnd1 + vwnd2 )
#else
         call a2d(uwnd(:,:,k),vwnd(:,:,k),uo,vo,im,jm,lon)
         uwnd(:,:,k) = uo(:,:)
         vwnd(:,:,k) = vo(:,:)
      enddo
#endif

      if ( verbose ) then
         print *
         print *, ' <> UWND-D  = ', minval(uwnd1), maxval(uwnd1) 
         print *, ' <> UWND-D  = ', minval(uwnd), maxval(uwnd) 
         print *, ' <> UWND-D  = ', minval(uwnd2), maxval(uwnd2) 
         print *
         print *, ' <> VWND-D  = ', minval(vwnd1), maxval(vwnd1) 
         print *, ' <> VWND-D  = ', minval(vwnd),  maxval(vwnd) 
         print *, ' <> VWND-D  = ', minval(vwnd2), maxval(vwnd2) 
      end if

!     Done with workspace
!     ------------------
      call mdealloc_()

!     For now, set unnecessary 2D fields to missing
!     ---------------------------------------------
      w_a%ts      = undef
      w_a%phis    = undef
      w_a%hs_stdv = undef

!     In case of remapping we may need this
!     -------------------------------------
      allocate ( ak(kn+1), bk(kn+1) )
      call set_eta ( kn, ks, ptop, pint, ak, bk )

!     Write dynamics state vector - same resolution
!     ---------------------------------------------
      print *, ' [] writing '//trim(ana_fn)
      if ( inres .eq. outres ) then

           print *, '    - same resolution: '//inres
           call dyn_put ( ana_fn, nymd, nhms, 0, w_a, rc )
           if ( rc /= 0 ) call die(myname,'cannot write output file')

!     Change vertical resolution
!     --------------------------
      else if ( inres(1:1) .eq. outres(1:1) ) then

         print *, '    - changing vertical resolution: '//inres//' -> '//outres
         print *
         call dyn_init ( im, jm, kn, lm, w_b, rc, ptop=ptop, ak=ak, bk=bk, ks=ks )
         if ( rc /= 0 ) call die(myname,'cannot create w_b' )
         call z_map ( w_a, w_b, rc, verbose = verbose )
         if ( rc /= 0 ) call die(myname,'cannot do vertical remapping' )
         call dyn_put ( ana_fn, nymd, nhms, 0, w_b, rc )
         if ( rc /= 0 ) call die(myname,'cannot write output file')
     
!     Change horizontal resolution
!     ----------------------------
      else if ( inres(2:3) .eq. outres(2:3) ) then

         print *, '    - changing horizontal resolution: '//inres//' -> '//outres
         print *
         call dyn_init ( in, jn, km, lm, w_b, rc, ptop=ptop, ak=ak, bk=bk, ks=ks )
         if ( rc /= 0 ) call die(myname,'cannot create w_b' )
         call h_map ( w_a, w_b, rc, verbose = verbose, lwifile='NONE' )
         if ( rc /= 0 ) call die(myname,'cannot do horizontal remapping' )
         call dyn_put ( ana_fn, nymd, nhms, 0, w_b, rc )
         if ( rc /= 0 ) call die(myname,'cannot write output file')

      else

!       Should be easy to fix if one ever needs it
!       ------------------------------------------
        print *, myname//': cannot change both horizontal and vertical resolutions'
        call die ( myname, 'consider using dyn2dyn.x for now...' )

      end if

!     All done
!     --------
      deallocate ( ak, bk )

      CONTAINS

      subroutine malloc_()

        nx = im
        ny = jm
        nz = km
        nq = lm

        allocate(lon(nx))
        allocate(lat(ny))
        allocate(ak(nz+1))
        allocate(bk(nz+1))
        
        allocate(ps1(nx,ny))
        allocate(ps2(nx,ny))
        allocate(lwi1(nx,ny))
        allocate(lwi2(nx,ny))
        allocate(delp1(nx,ny,nz))
        allocate(delp2(nx,ny,nz))
        allocate(uwnd1(nx,ny,nz))
        allocate(uwnd2(nx,ny,nz))
        allocate(vwnd1(nx,ny,nz))
        allocate(vwnd2(nx,ny,nz))
        allocate(tmpu1(nx,ny,nz))
        allocate(tmpu2(nx,ny,nz))
        allocate(theta1(nx,ny,nz))
        allocate(theta2(nx,ny,nz))
        allocate(sphu1(nx,ny,nz))
        allocate(sphu2(nx,ny,nz))
        allocate(uo(nx,ny))
        allocate(vo(nx,ny))
        
      end subroutine malloc_

      subroutine mdealloc_()

        deallocate(lon)
        deallocate(lat)
        deallocate(ak)
        deallocate(bk)
        
        deallocate(ps1)
        deallocate(ps2)
        deallocate(lwi1)
        deallocate(lwi2)
        deallocate(delp1)
        deallocate(delp2)
        deallocate(uwnd1)
        deallocate(uwnd2)
        deallocate(vwnd1)
        deallocate(vwnd2)
        deallocate(tmpu1)
        deallocate(tmpu2)
        deallocate(theta1)
        deallocate(theta2)
        deallocate(sphu1)
        deallocate(sphu2)
        deallocate(uo)
        deallocate(vo)
        
      end subroutine mdealloc_

      end program diag2ana


      subroutine a2d(ua,va,uo,vo,im,jm,lon)
C-----------------------------------------------------------------------
C
C Convert wind field from A to D grid
C
C-----------------------------------------------------------------------
      implicit none
C Input arguments
C
      integer im, jm
      real :: lon(im)        ! longitude
C
C Input/output arguments
C
      real :: ua(im,jm)
      real :: va(im,jm)
C
C Output arguments
      real uo(im,jm)
      real vo(im,jm)
C
C Local arguments
      real :: coslon(im)        ! cosine of longitude
      real :: sinlon(im)        ! sine of longitude
      real un, vn, us, vs
      integer imh, i, j
      real, parameter :: pi = 3.141592653
C

      uo = 0.0
      vo = 0.0
      coslon = cos(lon*pi/180.)
      sinlon = sin(lon*pi/180.)
      
      imh = im / 2
C NP
      un = 0.
      vn = 0.
C SP
      us = 0.
      vs = 0.
      j=jm-1
      do i=1,imh
         un = un + (ua(i+imh,j)-ua(i,j))*sinlon(i)
     &           + (va(i+imh,j)-va(i,j))*coslon(i)
         vn = vn + (ua(i,j)-ua(i+imh,j))*coslon(i)
     &           + (va(i+imh,j)-va(i,j))*sinlon(i)
         us = us + (ua(i+imh,2)-ua(i,2))*sinlon(i)
     &           + (va(i,2)-va(i+imh,2))*coslon(i)
         vs = vs + (ua(i+imh,2)-ua(i,2))*coslon(i)
     &           + (va(i+imh,2)-va(i,2))*sinlon(i)
      enddo
C
      un = un/im
      vn = vn/im
      us = us/im
      vs = vs/im
C
      do i=1,imh
C SP
         ua(i,1) = -us*sinlon(i) - vs*coslon(i)
         va(i,1) =  us*coslon(i) - vs*sinlon(i)
         ua(i+imh,1) = -ua(i,1)
         va(i+imh,1) = -va(i,1)
C NP
         ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
         va(i,jm) = -un*coslon(i) - vn*sinlon(i)
         ua(i+imh,jm) = -ua(i,jm)
         va(i+imh,jm) = -va(i,jm)
      enddo
C
      do j=2,jm
         do i=1,im
            uo(i,j) = 0.5*(ua(i,j) + ua(i,j-1))
         enddo
      enddo
C
      do j=2,jm-1
         do i=2,im
            vo(i,j) = 0.5*(va(i,j) + va(i-1,j))
         enddo
      enddo
C
      do j=2,jm-1
         vo(1,j) = 0.5*(va(im,j) + va(1,j))
      enddo

      return
      end subroutine a2d

      subroutine t2svpt(t1, qa, delp, t2, srev, ptop, im, jm, km)
C-----------------------------------------------------------------------
C
C Compute scaled virtual potential temperature from temperature and
C      vice versa.
C
C-----------------------------------------------------------------------
      implicit none
C Input arguments
C
      integer im, jm, km
      real :: t1(im,jm,km)       ! temperature or scaled virtual 
                                         !     potential temperature
      real :: qa(im,jm,km)       ! specific humidity
      real :: delp(im,jm,km)     ! pressure "thickness"
      real :: ptop               ! top level upper edge pressure
      integer      :: srev               ! 1 to calculate temperature
                                         ! -1 to calculate SVPT
C
C Output arguments
C
      real :: t2(im,jm,km)       ! temperature or SVPT
C
C Local work space
      real :: pe_a(im,km+1)      ! edge pressure
      real :: peln_a(im,km+1)    ! natural log of edge pressure
      real :: pk_a(im,km+1)      ! pe_a**kappa
      real :: kappa              ! r/cp
      real :: zvir               ! rh2o/rair - 1.
      integer i,j,k                      ! indexes
C-----------------------------------------------------------------------
C
      kappa=287.04/1004.64
      zvir=0.61
      pe_a(:,1)=ptop

      if (srev .eq. 1) then
C t1 is SVPT
         do j = 1, jm
            do k = 2, km + 1
               pe_a(:,k) = pe_a(:,k-1) + delp(:,j,k-1)
            enddo
            peln_a = log(pe_a)
            pk_a = pe_a ** kappa
            do k=1,km
               do i=1,im
                  t2(i,j,k) = t1(i,j,k)/kappa
     &                      *(pk_a(i,k) - pk_a(i,k+1))
     &                      /(peln_a(i,k) - peln_a(i,k+1))
     &                      /(1.0 + zvir*qa(i,j,k))
               enddo
            enddo
         enddo
      else if (srev .eq. -1) then
C t1 is temperature
         do j = 1, jm
            do k = 2, km + 1
               pe_a(:,k) = pe_a(:,k-1) + delp(:,j,k-1)
            enddo
            peln_a = log(pe_a)
            pk_a = pe_a ** kappa
            do k = 1, km
               do i = 1, im
                  t2(i,j,k) = t1(i,j,k)*kappa
     &                      /(pk_a(i,k) - pk_a(i,k+1))
     &                      *(peln_a(i,k) - peln_a(i,k+1))
     &                      *(1.0 + zvir*qa(i,j,k))
               enddo
            enddo
         enddo
       endif

       return
       end subroutine t2svpt

!..................................................................................

      subroutine tmpu2pt ( tmpu, sphu, delp, im, jm, km, ks, ptop, pt )
!
!     Computes Scaled Potential Temperature (p**cappa) * T based on an
!     algorithm by SJ Lin (from m_insitu.F).
!
      USE m_const, only: cp    => cpm
      use m_const, only: rair  => rgas
      use m_const, only: cappa => kappa
      use m_const, only: zvir

      implicit none

      integer, intent(in) :: im, jm, km, ks
      real, intent(in) :: sphu(im,jm,km)
      real, intent(in) :: delp(im,jm,km)
      real, intent(in) :: tmpu(im,jm,km)
      real, intent(in) :: ptop

      real, intent(out) :: pt(im,jm,km)

!                      -------------

      integer :: i, j, k
      real   pkz(im,jm,km)
      real   pk(im,jm,km+1)            ! pressure at edges (hPa)
      real   pe(im,km+1,jm)            ! log(pe)
      real peln(im,km+1,jm)            ! log(pe)


!     Compute pk, pe
!     --------------
      call geopm ( ptop, pe, pk, delp, im, jm, km, 1, jm,     
     &             cp, cappa )


!     Compute pkz for conversion between pt and temperature
!     -----------------------------------------------------
      call pkez ( im, jm, km, 1, jm, ptop,
     &            pe, pk, cappa, ks, peln, pkz, .false.)

      do k=1,km
         do j=1,jm
            do i=1,im
               pt(i,j,k) = tmpu(i,j,k) /pkz(i,j,k)/(1.+zvir*sphu(i,j,k))
            end do
         end do
      end do

      return

      end subroutine tmpu2pt

!..........................................................................

      subroutine geopm(ptop,pe,pk,delp,im,jm, km,jfirst, jlast, cp,akap )

! From SJ, slightly modified

      implicit none

! !INPUT PARAMETERS:

      integer im, jm, km, jfirst, jlast
      real akap, cp, ptop
      real delp(im,jm,km)

! !OUTPUT PARAMETERS
      real pe(im,km+1,jm)                ! only if id .ne. 0
      real pk(im,jm,km+1)                !

! Local:
      real pk2(im,km+1)
      integer i, j, k
      real p2d(im,km+1)
      real ptk

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p2d,ptk, pk2)
#endif

#if ( defined SGI )
c$doacross   local(i,j,k,p2d,ptk, pk2)
#endif

      do 1000 j=jfirst,jlast

        ptk  = ptop ** akap

        do i=1,im
          p2d(i,1) = ptop
          pk2(i,1) = ptk
        enddo

c Top down
        do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
            pk2(i,k) = p2d(i,k) ** akap
          enddo
        enddo

c Bottom up

          do k=1,km+1
            do i=1,im
              pe(i,k,j) = p2d(i,k)
              pk(i,j,k) = pk2(i,k)
            enddo
          enddo

1000  continue

      return
      end subroutine geopm

!..........................................................................

      subroutine pkez(im, jm, km, jfirst, jlast, ptop,
     &                pe, pk, akap,  ks, peln, pkz, eta)
C
C eta: true on eta coordinate; pk needs to be updated

C true:
C Input:  pe
C Output: pk, pkz, peln

C false:
C Input:  pk, pe
C Output: pkz

! WS 99.05.19 : Added im, jm, km as arguments
! WS 99.07.27 : Limited region to jfirst:jlast

      implicit none

! WS 99.05.19 : Removed fvcore.h

      integer im, jm, km, jfirst, jlast
      real  pe(im, km+1, jm)
      real  pk(im, jm, km+1)
      real  pkz(im, jm, km)
      real peln(im, km+1, jm)
      real ptop

      integer ks
      logical eta

      real akap

C Local
      real  pk2(im, km+1)
      real pek
      real lnp

      integer i, j, k, j1, jmm0

      j1   = max(1,jfirst)
      jmm0 = min(jm,jlast)

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,pek,lnp,pk2)
#endif

#if ( defined SGI )
c$doacross   local(i,j,k,pek,lnp,pk2)
#endif

! WS 99.07.27 : Limited region to jfirst:jlast

!!!   do 1000 j=1, jm
      do 1000 j=j1, jmm0

      if ( eta ) then

C <<<<<<<<<<< Eta cordinate Coordinate  >>>>>>>>>>>>>>>>>>>

      pek =   ptop ** akap
      lnp = log(pe(1,1,j))

      do i=1,im
          pk2(i,1)   = pek
         peln(i,1,j) = lnp
      enddo

      if(ks .ne. 0) then
      do k=2, ks+1
             pek = pe(1,k,j)**akap
             lnp = log(pe(1,k,j))
         do i=1,im
             pk2(i,k)   = pek
            peln(i,k,j) =  lnp
         enddo
      enddo

      do k=1, ks
           pek = (       pk2(1,k+1)   - pk2(1,k))   /
     &           (akap*(peln(1,k+1,j) - peln(1,k,j)) )
           do i=1,im
              pkz(i,j,k) = pek
           enddo
      enddo

      endif

      do k=ks+2,km
         do i=1,im
            pk2(i,k) = pe(i,k,j)**akap
         enddo
      enddo

      do i=1,im
         pk2(i,km+1) = pk(i,j,km+1)
      enddo

      do k=ks+2,km+1
         do i=1,im
            peln(i,k,j) =  log(pe(i,k,j))
         enddo
      enddo

      do k=ks+1,km
         do i=1,im
            pkz(i,j,k) = (pk2(i,k+1) - pk2(i,k)) /
     &                   (akap*(peln(i,k+1,j) - peln(i,k,j)) )
         enddo
      enddo

      do k=2,km
         do i=1,im
            pk(i,j,k) = pk2(i,k)
         enddo
      enddo

      else

C <<<<<<<<<<< General Coordinate  >>>>>>>>>>>>>>>>>>>

      pek =   ptop ** akap
      lnp = log(pe(1,1,j))

      do i=1,im
          pk2(i,1) = pek
         peln(i,1,j) = lnp
      enddo

      do k=2,km+1
         do i=1,im
            peln(i,k,j) =  log(pe(i,k,j))
             pk2(i,k) =  pk(i,j,k)
         enddo
      enddo

        do k=1,km
           do i=1,im
              pkz(i,j,k) = (       pk2(i,k+1) - pk2(i,k) )  /
     &                     (akap*(peln(i,k+1,j) - peln(i,k,j)) )
           enddo
        enddo

      endif
1000  continue

      return
      end subroutine pkez

! .........................................................................

      subroutine usage_ ()
      use m_die, only: die
      print *, 'SYNOPSIS  '
      print *, '       diag2dyn.x [-o fname] [-r res] diag1_fname diag2_fname'
      print *
      print *, 'DESCRIPTION'
      print *, '       Reads 3-hourly 3D diagnostic files and average them'
      print *, '       to produce a 6-hour averaged dynamic state file'
      print *, '       for replay in fvdas. The resulting "ana.eta" file'
      print *, '       only fills in (ps,u,v,t,sphu), and the other '
      print *, '       2D quantities such as ts, phis, etc are set to zero'
      print *, '       in this release.'
      print *
      print *, 'OPTIONS:'
      print *, '       -o fname   output file name template; default is'
      print *, '                 "%s.diag_dyn.eta.%y4%m2%d2_%h2z.nc4"'
      print *, '       -r res     resolution such as c55 or b55; default is'
      print *, '                  same resolution as the input diag files.'
      print *, 'BUGS'
      print *, '       In this realease, the resulting "ana.eta" file'
      print *, '       only fills in (lwi,ps,u,v,t,sphu), and the other '
      print *, '       2D quantities such as ts, phis, hs_stdv are set to undef.'
      print *, '       This is sufficient for replay puyrposes.'
      print *
      print *, 'AUTHORS'
      print *, '       Wei Tan wrote the first version called diag2ana.x;'
      print *, '       Arlindo da Silva did major rewrite to use GFIO, '
      print *, '       m_dyn, and flexible remapping'
      print *
      call die('diag2dyn.x',' not enough arguments' )
      end subroutine usage_

!..........................................................................

      subroutine parse ( diag1_fn, diag2_fn, ana_fn, in_res, out_res,
     &                   expid, im, jm, km, in, jn, kn, 
     &                   date, time, nymd, nhms, verbose )

      use m_die, only: die
      use m_StrTemplate, only: StrTemplate

      implicit NONE

      character(len=255), intent(out) :: diag1_fn, diag2_fn, ana_fn
      character(len=255), intent(out) :: expid
      integer, intent(out)            :: im, jm, km, in, jn, kn, nymd, nhms
      character(len=3),   intent(out) :: in_res, out_res
      logical, intent(out) :: verbose
      integer, intent(out) :: time(2), date(2)

      integer :: i, iarg, argc, iargc, n, fid, rc, nhms1, vid
      integer :: ntimes, nvars, ngatts
      character(len=255) argv, out_template, basen

      integer  :: READ_ONLY=1
      integer, external :: ncvid

      character(len=*), parameter :: myname = 'diag2dyn.x'
      
      print *
      print *, 'NAME'
      print *, '       diag2dyn.x - convert diag.eta into ana.eta files'
      print *

      out_template = '%s.diag_dyn.eta.%y4%m2%d2_%h2z.nc4'
      out_res = 'xxx'  ! means to be determined from diag files
      expid = 'unknown'
      verbose = .false.

      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      n = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, out_template )
         else if (index(argv,'-r' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            out_res = argv(1:3)
         else if (index(argv,'-v' ) .gt. 0 ) then
            verbose = .true.
         else
            n = n + 1
            diag1_fn = argv
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, diag2_fn )
            n = n + 1
         end if
      end do

      if ( n < 2 ) then
         print *, myname//': missing diag1/diag2 file names' 
         call usage_()
      end if

!     Query first diag file for dimensions, defaults
!     ----------------------------------------------
      call GFIO_Open ( diag1_fn, READ_ONLY, fid, rc ) 
      if ( rc /= 0 ) call die(myname,'cannot open file '//trim(diag1_fn) )
      call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars, ngatts, rc ) 
      if ( rc /= 0 ) call die(myname,'cannot get dimensions of '//trim(diag1_fn) )
      vid = ncvid (fid, 'time', rc)
      call ncagt (fid,vid,'begin_date',date(1), rc )
      call ncagt (fid,vid,'begin_time',time(1), rc )
      call GFIO_Close ( fid, rc )
      if ( rc /= 0 ) call die(myname,'cannot close '//trim(diag1_fn) ) 

      i = max(0,index ( diag1_fn, '/', back=.true. ))
      basen = diag1_fn(i+1:)
      i = index ( basen, '.' )
      if ( i > 1 ) expid = basen(1:i-1)

      if ( im == 576 .and. jm == 361 ) then
         in_res = 'd'
      else if ( im == 288 .and. jm == 181 ) then
         in_res = 'c'
      else if ( im == 144 .and. jm == 91 ) then
         in_res = 'b'
      else if ( im == 72 .and. jm == 46 ) then
         in_res = 'a'
      else
         print *, 'im, jm: ', im, jm
         call die(myname, 'cannot handle these dimensions yet')
      end if
      write(argv,'(I2)') km
      in_res(2:3) = argv(1:2)
      if ( out_res == 'xxx' ) then
         out_res = in_res
      end if

      call GFIO_Open ( diag2_fn, READ_ONLY, fid, rc ) 
      if ( rc /= 0 ) call die(myname,'cannot open file '//trim(diag1_fn) )
      vid = ncvid (fid, 'time', rc)
      call ncagt (fid,vid,'begin_date',date(2), rc )
      call ncagt (fid,vid,'begin_time',time(2), rc )
      call GFIO_Close ( fid, rc )

!     Check time consistency and calculate analysis time
!     --------------------------------------------------
      nymd = maxval(date)
      if ( minval(time) == 13000 .and. maxval(time) == 223000 ) then
           nhms = 0
      else if ( minval(time) == 43000 .and. maxval(time) == 73000 ) then
           nhms = 60000
      else if ( minval(time) == 103000 .and. maxval(time) == 133000 ) then
           nhms = 120000
      else if ( minval(time) == 163000 .and. maxval(time) == 193000 ) then
           nhms = 180000
      else
           print *, 'time = ', time
           call die(myname,'diag files have inconsistent times')
      end if

      call StrTemplate ( ana_fn, out_template, xid=expid, nymd=nymd, nhms=nhms )

!     Output resolution
!     -----------------
      read(out_res(2:3),*) kn
      if ( out_res(1:1) == 'a' ) then
         in = 72 
         jn = 46
      else if ( out_res(1:1) == 'b' ) then
         in = 144
         jn = 91
      else if ( out_res(1:1) == 'c' ) then
         in = 288
         jn = 181
      else if ( out_res(1:1) == 'd' ) then
         in = 576
         jn = 361
      else
         call die ( myname, 'cannot handle resolution ' )
      end if

!     Echo input parameters
!     ---------------------
      print *, 'INPUT PARAMETERS:'
      print *
      print *, '        Experiment ID: ', trim(expid)
      print *, '      Diag.eta file 1: ', trim(diag1_fn)
      print *, '      Diag.eta file 2: ', trim(diag2_fn)
      print *, ' Output file template: ', trim(out_template)
      print *, ' Output file name:     ', trim(ana_fn)
      print *
      if ( verbose ) then
      print *, '     Input Resolution: ', in_res, ' ',  im, jm, km 
      print *, '    Output Resolution: ', out_res, ' ', in, jn, kn
      print *, '    Input   date/time: ', date, time
      print *, '    Output  date/time: ', nymd, nhms
      print *
      end if

      end

!...............................................................

      subroutine grids_ ( w_a, lon, lat, im, jm )

      use m_dyn
      implicit none
      type(dyn_vect), intent(in) :: w_a      
      integer, intent(in) :: im, jm
      real, intent(out) :: lon(im), lat(jm)

      integer :: i, j
    
!  Create coordinate variables
!  ---------------------------
      do j = 1, jm
         lat(j) = w_a%grid%lat_min + (j-1) * w_a%grid%lat_del
      end do
      do i = 1, im
         lon(i) = w_a%grid%lon_min + (i-1) * w_a%grid%lon_del
      end do


      end 
