#include "unused_dummy.H"
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_Ana2Dyn --- Implements PSAS to FVGCM coupler
!
! !INTERFACE:
!
      MODULE  m_ana2dyn

! !USES:

      use  m_dyn                ! dynamics state vector type & methods
      use  m_dynp               ! dynamics state perturbation methods
      use  m_const

      Implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  Ana2Dyn           ! Use first guess and PSAS analysis increments
                                !  to produce analysis.
!
! !DESCRIPTION: This module implements the main coupling between PSAS
!               and FVGCM.
!
! !REVISION HISTORY:
!
! 08nov1999  da Silva  Initial specs and prologues.
! 13jun2000  da Silva  Eliminated unnecessary zonal shifts, made sure
!                      dh_a and dw_a are single valued at the poles.
! 12nov2002  Dee       Split Ana2m() into Ana2mg() and dynp_shave();
!                      support for simplified bias correction (SBC)
!
!
!EOP
!-------------------------------------------------------------------------


      CONTAINS


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Ana2Dyn --- Create analysis from PSAS Analysis Increments
!
! !INTERFACE:
!
      subroutine Ana2Dyn ( w_f, im, jm, km, du_a, dv_a, dh_a, dw_a,
     &                     glat, glon, glvm, glve,
     &                     imc, jmc, kmc, kec, ilat, ilon, ilvm, ilve,
     &                     w_a, rc,
     &                     b_f, alpha_uvh, alpha_q )

! !USES:
!
      use m_const, only: alhl, cpd, grav, kappa, rgas, zvir

      Implicit NONE

!
! !INPUT PARAMETERS:
!

      type(dyn_vect), intent(in)   :: w_f       ! First guess

                                                ! First guess dimensions
      integer, intent(in)   :: im               !  zonal
      integer, intent(in)   :: jm               !  meridional
      integer, intent(in)   :: km               !  vertical

                                                ! Analysis increments:
      real,    intent(in)   :: du_a(im,jm,km)   !   u-wind (m/s)
      real,    intent(in)   :: dv_a(im,jm,km)   !   v-wind (m/s)
      real,    intent(inout):: dh_a(im,jm,km+1) !   heights (m)
      real,    intent(in)   :: dw_a(im,jm,km)   !   mixing ratio (g/kg)


                                                ! Dynamics vector coordinates:
      real,    intent(in)   :: glon(im)         !  Longitudes in [-180,180] deg
      real,    intent(in)   :: glat(jm)         !  Latitudes  in [ -90,+90] deg
      real,    intent(in)   :: glvm(im,jm,km)   !  Pressure mid  levels (hPa)
      real,    intent(in)   :: glve(im,jm,km+1) !  Pressure edge levels (hPa)

                                                ! NOTE: The FVGCM fields are
                                                !       defined at the mid
                                                ! pressure levels.

                                                ! Analysis increment coarsening
                                                !  indices:
      integer, intent(in)   :: imc, ilon(imc)   !    zonal: kmc .le. km
      integer, intent(in)   :: jmc, ilat(jmc)   !    meridional: jmc .le. jm
      integer, intent(in)   :: kmc, ilvm(kmc)   !    vertical (mid): kmc .le. km
      integer, intent(in)   :: kec, ilve(kec)   !    vertical (edges)

      real,    intent(in), OPTIONAL :: alpha_q   ! Bias relaxation coefficient for q
      real,    intent(in), OPTIONAL :: alpha_uvh ! Bias relaxation coefficient for uvh

!
! !INPUT/OUTPUT PARAMETERS:
!
      type(dyn_vect), OPTIONAL, intent(inout) :: b_f ! First guess bias estimate

!
! !OUTPUT PARAMETERS:
!

      type(dyn_vect), intent(inout):: w_a       ! Analysis (memory must be
                                                !  allocated by the caller)

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   ...
                                                !

! !DESCRIPTION: Given the first guess fields ({\tt delp\_f, u\_f, v\_f, pt\_f,}
!               and {\tt q\_f}) from FVGCM and the analysis increments
! ({\tt du\_a, dv\_a, dh\_a} and {\tt dw\_a}) produced with PSAS this
!  routine produces the analyzed fields ({\tt delp\_a, u\_a, v\_a, pt\_a},
!  and {\tt q\_a}).
!
!  \bd

! \item[Bias correction:] \mbox{}\\
! \be
!      b_f = b_f - \alpha \delta w_a
! \ee
! with $\delta w_a$ the analysis increment.
!
! \item[Shaving method:] Shave the lowest 3 layers -- fast and most accurate.
! Have to call analysis before fvcore if shaving method is used.
!
! \item[Specific humidity/mixing ratio conversion formulas:] \mbox{}\\
!
! \be
!      q_a = { q_f + \delta w_a \( 1 - q _f \) \over
!              1 + \delta w_a \( 1 - q_f \) }
! \ee
! where $q_a/q_f$ is the analyzed/first guess specific humidity, and
! $\delta w_a$ is the mixing ratio analysis increment.
!
! \ed
!
! ! !REMARKS:
!
! \ev
! \bn
!
! \item
! The code may be faster if we eliminate the output {\tt *\_a()} arrays.
! For speed on RISC processor 3D arrays should always be avoided (at all cost).
!
! \en
! \bv
!
! !REVISION HISTORY:
!
! 08nov1999  da Silva  Initial code.
! 14DEC1999  S.-J. Lin added pk computation; this is time consuming and
!                      reddundant as pk has been computed elsewhere already.
!                      We should pass pk from model or dyn_pak at a later
!                      stage.
! 17dec1999  da Silva  Changed pk from automatic array to allocatable,
!                      minor clean up.
! 20dec1999  da Silva  Fixed pk calculation (hPa -> Pa bug)
! 24dec1999  da Silva  Fixed bug: now it properly sets w_a%phis = w_f%phis
! 23feb2000  da Silva  Introduced kec; now it returns with rc=3 when a
!                      coarser grid is specified (not ready for this yet).
! 20apr2001  J. Chern  redistribute moisture vertically to avoid supersaturation
!                       and moist instability
! 12nov2002  Dee       Cleaned up: split Ana2m() into Ana2mg() and dynp_shave()
! 09Jul2003  Todling   Partially added 2nd tracer (O3) slot (not ana-var yet).
! 13Jan2004  Sienkiewicz Bug fix; Compaq disliked simultaneous check assoc/sbc below.
! 21Oct2004  Todling   Bug fix: w_a need to be (inout); caught on Linux
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'ana2dyn'


      real, parameter :: pt_min = 1. ! min allowable pt value
      real, allocatable ::  pk(:,:,:)
      integer i, j, k, lm, ios, rcs
      logical sbc

      _UNUSED_DUMMY(glat)
      _UNUSED_DUMMY(glon)
      _UNUSED_DUMMY(ilat)
      _UNUSED_DUMMY(ilon)
      _UNUSED_DUMMY(ilvm)
      _UNUSED_DUMMY(ilve)

      sbc = .false.
      if ( present(b_f) ) sbc = .true.


!     Consistency checks
!     ------------------
      rc = 0
      lm = w_f%grid%lm
      if ( lm .lt. 1 .or. lm .gt. 2 ) then
         rc = 1
         return
      end if
      if ( .not. associated(w_a%delp) ) then  ! check only 1 field
         rc = 2
         return
      end if
      if ( sbc ) then
         if ( .not. associated(b_f%delp) ) then  ! check only 1 field
             rc = 3
             return
         end if
      end if
      if ( imc.ne.im .or. jmc.ne.jm .or. kmc.ne.km .or. kec.ne.(km+1) ) then
         rc = 4
         return
      end if

!     Allocate local work space
!     -------------------------
      allocate ( pk(im,jm,km+1), stat = ios )
      if ( ios .ne. 0 ) then
         rc = 5
         return
      end if

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,pktmp)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,pktmp)
#endif

!     Computing p**kappa: This code segment is time consuming;
!     This quantity could be pre-computed by m_dyn and carried
!     along with w_f.
!     --------------------------------------------------------
      do k=1,km+1
         do j=1,jm
            do i=1,im
               pk(i,j,k) = (100.*glve(i,j,k)) ** kappa ! hPa -> Pa
            end do
         enddo
      enddo

!
!     Create analysis state - no shaving yet
!     --------------------------------------
      call Ana2mg ( im, jm, km, lm, pk,
     &              w_f%u, w_f%v, w_f%pt, w_f%q,
     &              du_a, dv_a, dh_a, dw_a,
     &              w_a%u, w_a%v, w_a%pt, w_a%q )

      if ( minval(w_a%pt) .lt. pt_min ) rc = 6

      w_a%delp = w_f%delp
      w_a%ps   = w_f%ps


!     Update first-guess bias estimates:
!     ---------------------------------
      if ( sbc ) then
        b_f%u    = b_f%u    - alpha_uvh * ( w_a%u  - w_f%u    )
        b_f%v    = b_f%v    - alpha_uvh * ( w_a%v  - w_f%v    )
        b_f%pt   = b_f%pt   - alpha_uvh * ( w_a%pt - w_f%pt   )
        b_f%q    = b_f%q    - alpha_q   * ( w_a%q  - w_f%q    )
        b_f%phis = b_f%phis - alpha_uvh * grav * dh_a(:,:,km+1)  ! sfc geopotential
      end if


!     Adjust mass in lowest levels: 'shaving method'
!     ---------------------------------------------
      call dynp_shave ( w_a, grav*dh_a(:,:,km+1), im, jm, km, rcs )

      if ( rcs .ne. 0 ) rc = 7


!     Redistribute moisture to avoid supersaturation and moist instability
!     --------------------------------------------------------------------
      call Qredist ( im,      jm,      km,
     &     alhl,     cpd,     grav,    kappa,   rgas,    zvir,
     &     w_f%q(:,:,:,1),    glvm,    glve,    pk,
     &     w_a%delp, w_a%pt,  w_a%q(:,:,:,1)    )


!     Set fields not affected by the analysis
!     ---------------------------------------
      w_a%phis(1:im,1:jm)      = w_f%phis(1:im,1:jm)
      w_a%hs_stdv(1:im,1:jm)   = w_f%hs_stdv(1:im,1:jm)
      w_a%Ts(1:im,1:jm)        = w_f%Ts(1:im,1:jm)
      w_a%lwi(1:im,1:jm)       = w_f%lwi(1:im,1:jm)


!     All done
!     --------
      deallocate ( pk )

!     Print some diagnostics
!     ----------------------
      print *
      print *, '  du_a = ', minval(du_a), maxval(du_a)
      print *, '  dv_a = ', minval(dv_a), maxval(dv_a)
      print *, '  dh_a = ', minval(dh_a), maxval(dh_a)
      print *, '  dw_a = ', minval(dw_a), maxval(dw_a)
      print *
      print *, '  ps_a = ', minval(w_a%ps), maxval(w_a%ps)
      print *, '  ps_f = ', minval(w_f%ps), maxval(w_f%ps)
      print *, '   A-F = ', minval(w_a%ps-w_f%ps), maxval(w_a%ps-w_f%ps)
      print *
      print *, 'delp_a = ', minval(w_a%delp), maxval(w_a%delp)
      print *, 'delp_f = ', minval(w_f%delp), maxval(w_f%delp)
      print *, '   A-F = ', minval(w_a%delp-w_f%delp), maxval(w_a%delp-w_f%delp)
      print *
      print *, '   u_a = ', minval(w_a%u), maxval(w_a%u)
      print *, '   u_f = ', minval(w_f%u), maxval(w_f%u)
      print *, '   A-F = ', minval(w_a%u-w_f%u), maxval(w_a%u-w_f%u)
      print *
      print *, '   v_a = ', minval(w_a%v), maxval(w_a%v)
      print *, '   v_f = ', minval(w_f%v), maxval(w_f%v)
      print *, '   A-F = ', minval(w_a%v-w_f%v), maxval(w_a%v-w_f%v)
      print *
      print *, '  pt_a = ', minval(w_a%pt), maxval(w_a%pt)
      print *, '  pt_f = ', minval(w_f%pt), maxval(w_f%pt)
      print *, '   A-F = ', minval(w_a%pt-w_f%pt), maxval(w_a%pt-w_f%pt)
      print *
      print *, '   q_a = ', minval(w_a%q(:,:,:,1)), maxval(w_a%q(:,:,:,1))
      print *, '   q_f = ', minval(w_f%q(:,:,:,1)), maxval(w_f%q(:,:,:,1))
      print *, '   A-F = ', minval(w_a%q(:,:,:,1)-w_f%q(:,:,:,1)), maxval(w_a%q(:,:,:,1)-w_f%q(:,:,:,1))
      print *
      if (lm==2) then
      print *, '  o3_a = ', minval(w_a%q(:,:,:,2)), maxval(w_a%q(:,:,:,2))
      print *, '  o3_f = ', minval(w_f%q(:,:,:,2)), maxval(w_f%q(:,:,:,2))
      print *, '   A-F = ', minval(w_a%q(:,:,:,2)-w_f%q(:,:,:,2)), maxval(w_a%q(:,:,:,1)-w_f%q(:,:,:,2))
      print *
      end if


      end subroutine ana2dyn


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ana2mg --- Create analyzed 3D fields from PSAS Analysis Incr.
!                        on model grid
!
! !INTERFACE:
!
      subroutine Ana2mg ( im, jm, km, lm, pk,
     &                    u_f, v_f, pt_f, q_f,
     &                    du_a, dv_a, dh_a, dw_a,
     &                    u_a, v_a, pt_a, q_a )
!
! !USES:
!
      use m_const, only: cpd, grav

      Implicit NONE

!
! !INPUT PARAMETERS:
!

                                               ! First guess dimensions
      integer, intent(in)  :: im               !  zonal
      integer, intent(in)  :: jm               !  meridional
      integer, intent(in)  :: km               !  vertical
      integer, intent(in)  :: lm               !  number of tracers

                                               ! First guess fields:
                                               ! [0, 360]
      real,    intent(in)  ::     u_f(im,jm,km)  ! u-wind (m/s)
      real,    intent(in)  ::     v_f(im,jm,km)  ! v-wind (m/s)
      real,    intent(in)  ::    pt_f(im,jm,km)  ! scaled virtual potential
                                                 !  temperature (T p**kappa)
      real,    intent(in)  ::     q_f(im,jm,km,lm)  ! Specific humidity (kg/kg) & O3 (ppmv)

      real,    intent(in)  ::     pk(im,jm,km+1) ! pe**kappa


                                                ! Analysis increments:
                                                ! [-180, 180]
      real,    intent(in)   :: du_a(im,jm,km)   !   u-wind (m/s)
      real,    intent(in)   :: dv_a(im,jm,km)   !   v-wind (m/s)
      real,    intent(inout):: dh_a(im,jm,km+1) !   heights (m)
      real,    intent(in)   :: dw_a(im,jm,km)   !   mixing ratio (g/kg)

!
! !OUTPUT PARAMETERS:
!
                                                ! Analysis fields:
                                                ! [0, 360]
      real,    intent(out)  ::     u_a(im,jm,km)  ! u-wind (m/s)
      real,    intent(out)  ::     v_a(im,jm,km)  ! v-wind (m/s)
      real,    intent(out)  ::    pt_a(im,jm,km)  ! scaled virtual potential
                                                  !  temperature (T p**kappa)
      real,    intent(out)  ::     q_a(im,jm,km,lm)  ! Specific humidity (kg/kg) & O3 (ppmv)


! !DESCRIPTION:  Computes 3d analyzed fields from first guess
!                and analysis increments, using first-guess vertical
! coordinate. Ensures that height and moisture increments are single-valued
! at the poles.
!
! !REVISION HISTORY:
!
! 12nov2002  Dee        Lifted from ana2m.
!
!EOP
!-------------------------------------------------------------------------

! Local variables:

      real :: du(im,jm)
      real :: w1d(im)

      integer i, j, k
      real  dq, qf, cp, gcp
      real*8 asouth, anorth          ! polar values

       cp = cpd
      gcp = grav / cp

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k)
#endif

! make sure height increments are single valued at poles
! ------------------------------------------------------
      do k = 1, km+1
         anorth = 0.0
         asouth = 0.0
         do i = 1, im
            asouth = asouth + dh_a(i,1,k)
            anorth = anorth + dh_a(i,jm,k)
         end do
         asouth = asouth / im
         anorth = anorth / im
         do i = 1, im
            dh_a(i,1,k)  = asouth
            dh_a(i,jm,k) = anorth
         end do
      end do

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,du,w1d,dq,qf)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k,du,w1d,dq,qf)
#endif

      do k=1, km

! *******
! u-wind:
! *******

         do j=1,jm
            do i=1,im
               du(i,j) = du_a(i,j,k)
            enddo
         enddo

         do j=2,jm
            do i=1,im
               u_a(i,j,k) = u_f(i,j,k) + 0.5*(du(i,j-1)+du(i,j))
            enddo
         enddo

         do i=1,im
            u_a(i,1,k) = u_f(i,1,k)
         enddo

! *******
! v-wind:
! *******

         do j=2,jm-1
            do i=1,im
               w1d(i) = dv_a(i,j,k)
            enddo
! i=1
            v_a(1,j,k) = v_f(1,j,k) + 0.5*(w1d(im)+w1d(1))
            do i=2,im
               v_a(i,j,k) = v_f(i,j,k) + 0.5*(w1d(i-1)+w1d(i))
            enddo
         enddo

         do i=1,im
            v_a(i,1 ,k) = v_f(i, 1,k)
            v_a(i,jm,k) = v_f(i,jm,k)
         enddo



! *******
! pt:
! *******
         do j=1,jm
            do i=1,im

               pt_a(i,j,k) = pt_f(i,j,k) + gcp *
     &         (dh_a(i,j,k) - dh_a(i,j,k+1))/(pk(i,j,k+1) - pk(i,j,k))

            enddo

! *******
! Specific humidity:
! *******
            do i=1,im
               w1d(i) = dw_a(i,j,k)
            enddo

! make sure moisture increments are single valued at poles
            if ( j .eq. 1 ) then
               asouth = 0.0
               do i = 1, im
                  asouth = asouth + w1d(i)
               end do
               asouth = asouth / im
               do i = 1, im
                  w1d(i) = asouth
               end do
            end if
            if ( j .eq. jm ) then
               anorth = 0.0
               do i = 1, im
                  anorth = anorth + w1d(i)
               end do
               anorth = anorth / im
               do i = 1, im
                  w1d(i) = anorth
               end do
            end if


            do i=1,im
!
! Convert mixing ratio to specific humidity (model)
! Conversion formular:
! R = q / (1 + q);    q = R / (1 - R)
! q = mixing ratio;   R = specific humidity
!
               qf = q_f(i,j,k,1)
               dq = 0.001*w1d(i) * (1. - qf)
               q_a(i,j,k,1) = (qf + dq) / (1. + dq)
            enddo
         enddo

! *******
! Ozone:
! *******
         if ( lm == 2 ) then
             do j = 1, jm
                do i = 1, im
                   q_a(i,j,k,2) = q_f(i,j,k,2)   ! until O3 becomes an ana-var, just copy bkg
                end do
            end do
         end if

      enddo          ! End parallel k-loop

      return

      end subroutine ana2mg

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Qredist --- redistribute moisture vertically to
!                        avoid supersaturation and moist instability
!
! !INTERFACE:
!
      subroutine Qredist (    im,      jm,      km,
     &     hvap,     cp,      grav,    kappa,   rair,    zvir,
     &     q_f,      glvm,    glve,    pk,
     &     delp_a,   pt_a,    q_a      )
!
! !USES:
!
      use  m_qsat, only:  gestbl, vqsat  ! used for computing saturation specific humidity
      Implicit NONE
!
! !INPUT ARGUMENTS:
!
                                                 ! First guess dimensions
      integer, intent(in)  :: im                 ! zonal
      integer, intent(in)  :: jm                 ! meridional
      integer, intent(in)  :: km                 ! vertical

      real,    intent(in)  :: hvap               ! latent heat of vaporization (J/kg)
      real,    intent(in)  :: cp                 ! Specific heat of dry air (J/kg/K)
      real,    intent(in)  :: grav               ! acceleration due to gravity (m/s2)
      real,    intent(in)  :: kappa              ! kappa = rair / cp
      real,    intent(in)  :: rair               ! gas constant for dry air
      real,    intent(in)  :: zvir               ! rh2o / rair - 1.

                                                 ! first guess fields
      real,    intent(in)  :: q_f(im,jm,km)      ! Specific humidity (kg/kg)
      real,    intent(in)  :: glvm(im,jm,km)     ! Pressure mid  levels (hPa)
      real,    intent(in)  :: glve(im,jm,km+1)   ! Pressure edge levels (hPa)
      real,    intent(in)  :: pk(im,jm,km+1)     ! pe**kappa

                                                 ! analysis fields
      real,    intent(in)  :: delp_a(im,jm,km)   ! Delta pressure (Pa)
      real,    intent(in)  :: pt_a(im,jm,km)     ! scaled virtual potential
!
! !INPUT/OUTPUT ARGUMENTS:
!
      real,    intent(inout)  ::  q_a(im,jm,km)  ! Specific humidity (kg/kg)

!
! !DESCRIPTION: Redistribute moisture vertically to avoid supersaturation and/or
!               moist instability
!
! !REVISION HISTORY:
!
! 20Apr2001  J.D. Chern initial code.
!
!EOP
!
! Local variables

      real, parameter ::  pdist = 50000.0      ! upper bound for resdistribute moisture (Pa)
      real, parameter ::  undef = 1.e15

                                    ! Analysis fields:
      real  pe_a(im,km+1)           ! pressure at layer edges (pa)
      real  peln_a(im,km+1)         ! log( pe )
      real  pk_a(im,km+1)           ! pe**cappa (cappa = rg/cp)
      real  pm_a(im,km)             ! pressure at mid points (Pa)
      real  tv_a(im,km)             ! virtual temperature (K)
      real  tem_a(im,km)            ! temperature (K)
      real  dq_a(im,km)             ! specific humidity difference between q-a and q-f
      real  zi_a(im,km+1)           ! height above surface at interface (m)
      real  zm_a(im,km)             ! height above surface at midpoints (m)
      real  qs_a(im,km)             ! saturation specific humidity (kg/kg)
      real  es_a(im,km)             ! saturation vapor pressure (pa)
      real  engd(im,km)             ! dry static energy

      real  rrg                     ! rair / grav
      real  tmp0, tmp1, tmp2, tmp3  ! working spaces
      real  tmp4, tmp5, tmp6, tmp7  ! working spaces
      integer i,j,k                 ! index
!
      rrg         = rair / grav

!
      call gestbl()           ! initialize saturation vapor pressure table
!
!-----------------------------------------------------------------------------------
! Adjust sepcific humidity to avoid supersaturation and moist instability
!-----------------------------------------------------------------------------------
! JDC 4/9/2001

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k)
!$omp& private(pe_a,peln_a, pk_a, pm_a, tv_a, tem_a, dq_a)
!$omp& private(zi_a, zm_a, qs_a, es_a, engd)
!$omp& private(tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7)
#endif

#if ( defined SGI )
!$doacross local(i,j,k),
c$&        local(pe_a,peln_a, pk_a, pm_a, tv_a, tem_a, dq_a),
c$&        local(zi_a, zm_a, qs_a, es_a, engd),
c$&        local(tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7)
#endif
!
! (1) compute variables related to pressure
!
      do j = 1, jm
        do k = 1, km - 3
          do i = 1, im
!
! pressure fields are not changed from k = 1 to k = km-3
!
            pe_a(i,k)   =  100.0 * glve(i,j,k)       ! convert from hpa to pa
            peln_a(i,k) =  log( pe_a(i,k) )
            pk_a(i,k)   =  pk(i,j,k)
            pm_a(i,k)   =  100.0 * glvm(i,j,k)       ! convert from hpa to pa
          enddo
        enddo
!
! pressure fields could be changed  due to shaving
!
        do k = km - 2, km + 1
          do i = 1, im
            pe_a(i,k)   =  pe_a(i,k-1) + delp_a(i,j,k-1)
            peln_a(i,k) =  log( pe_a(i,k) )
            pk_a(i,k)   =  pe_a(i,k) ** kappa
          enddo
        enddo
!
        do k = km - 3, km
          do i = 1, im
            pm_a(i,k)   = 0.5 * ( pe_a(i,k) + pe_a(i,k+1) )
          enddo
        enddo
!
! (2) compute virtual temperature and temperature
!
        do k = 1, km
          do i = 1, im
            tv_a(i,k)   = pt_a(i,j,k) / kappa
     &                  * ( pk_a(i,k) - pk_a(i,k+1) )
     &                  / ( peln_a(i,k) - peln_a(i,k+1) )
            tem_a(i,k)  = tv_a(i,k) / ( 1.0 + zvir * q_a(i,j,k) )
            dq_a(i,k)   = q_a(i,j,k) - q_f(i,j,k)
          enddo
        enddo
!
! (3) compute height at middle points
!
!
! compute height above surface at interface
!
        do i = 1, im
          zi_a(i,km+1)  = 0.0
        enddo
        do k = km, 1, -1
          do i = 1, im
            zi_a(i,k)   = zi_a(i,k+1) + rrg * tv_a(i,k)
     &                  * ( peln_a(i,k+1) - peln_a(i,k) )
          end do
        end do
!
! compute heigh and dry static energy at middle points
!
        do k = 1, km
          do i = 1, im
            zm_a(i,k)   = zi_a(i,k+1) + rrg * tv_a(i,k)
     &                  * ( 1. - pe_a(i,k)
     &                  * ( peln_a(i,k+1)-peln_a(i,k) )
     &                  / delp_a(i,j,k) )
            engd(i,k)   = cp * tem_a(i,k) + grav * zm_a(i,k)
           enddo
        enddo
!
! (4) compute saturation specific humidity
!

        call vqsat ( tem_a, pm_a, es_a, qs_a, im*km, undef )

!
! (5) redistribute moisture downward when supersaturation exist
!
        do k = 1, km - 1
          do i = 1, im
            if( (dq_a(i,k) .gt. 1.0e-8 ) .and.
     &          (q_a(i,j,k) > qs_a(i,k)) ) then
              tmp0          = max( q_f(i,j,k), qs_a(i,k) )
              tmp1          = q_a(i,j,k) - tmp0
              tmp2          = tmp1 * delp_a(i,j,k) / delp_a(i,j,k+1)
              dq_a(i,k)     = dq_a(i,k) - tmp1
              q_a(i,j,k)    = tmp0
              dq_a(i,k+1)   = dq_a(i,k+1) + tmp2
              q_a(i,j,k+1)  = q_a(i,j,k+1) + tmp2
            end if
          enddo
        enddo

!
! (6) redistribute moisture upward when moist instability exist
!
        do k = km, 2, -1
          do i = 1, im
            if( dq_a(i,k) .gt. 1.0e-8 ) then
! avoid supersaturation
              if( q_a(i,j,k) > qs_a(i,k) ) then
                tmp0          = max( q_f(i,j,k), qs_a(i,k) )
                tmp1          = q_a(i,j,k) - tmp0
                tmp2          = tmp1 * delp_a(i,j,k) / delp_a(i,j,k-1)
                dq_a(i,k)     = dq_a(i,k) - tmp1
                q_a(i,j,k)    = tmp0
                if( pm_a(i,k) >= pdist ) then
                  dq_a(i,k-1)   = dq_a(i,k-1) + tmp2
                  q_a(i,j,k-1)  = q_a(i,j,k-1) + tmp2
                end if
              end if
! avoid moist instability
! tmp1 = moist static energy; tmp2 = saturated moist static energy
!
              tmp1            = engd(i,k) + hvap * q_a(i,j,k)
              tmp2            = engd(i,k-1) + hvap * qs_a(i,k-1)
              tmp3            = ( tmp2 - engd(i,k) ) / hvap
              tmp4            = max (0.0, tmp3)
              if( (tmp1 > tmp2) .and. (q_a(i,j,k) > tmp4) ) then
                tmp5          = max( q_f(i,j,k), tmp4 )
                tmp6          = q_a(i,j,k) - tmp5
                q_a(i,j,k)    = tmp5
                dq_a(i,k)     = dq_a(i,k) - tmp6
                if( pm_a(i,k) >= pdist ) then
                  tmp7         = tmp6 * delp_a(i,j,k) / delp_a(i,j,k-1)
                  dq_a(i,k-1)  = dq_a(i,k-1) + tmp7
                  q_a(i,j,k-1) = q_a(i,j,k-1) + tmp7
                end if
              end if
            endif
          end do     ! end i-loop
        end do       ! end k-loop
      end do       ! end parallel j-loop
      end subroutine Qredist

      end module m_Ana2Dyn
