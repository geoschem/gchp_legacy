!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_insitu --- Implements insitu vector simulators for FVGCM
!
! !INTERFACE:
!
      MODULE  m_insitu

! !USES:

      use  m_dyn                ! dynamics state vector type & methods
      use  m_lp                 ! surface layer parameterizations
      use  m_const, only: undef

      Implicit NONE

!
! !PUBLIC TYPES:
!
      PRIVATE
      PUBLIC  sim_vect          ! simulator vector
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  insitu_init       ! initialize simulator vector
      PUBLIC  insitu_null       ! nullify pointers
      PUBLIC  insitu_clean      ! deallocate memory use by simulator vector
      PUBLIC  insitu_mass       ! simulates insitu mass obs
      PUBLIC  insitu_wind       ! simulate  insitu wind obs
      PUBLIC  insitu_ocean      ! simulate  insitu sfc layer quantities
!
! !TO DO:
!
!     1. Interface to m_const for physical constants and undefs
!     2. Write Insitu_Surf()
!     3. Extrapolate height below surface
!
! !DESCRIPTION: This modules conatins routines to produce the model
!               equivalent of the state
!               variables (height, winds, temperature, etc. ) at observation
!  locations.
!
! !REVISION HISTORY:
!
!  08Oct1999  da Silva  Initial specification and prologues.
!  12Oct1999  da Silva  Added insitu_surf() scs.
!  01dec1999  da Silva  Changed sim_vect: q-->q3d, slp,slp_cf-->q2d
!  03dec1999  da Silva  Added ub, tb, qb
!  05oct2000  Dee       Implemented TPW simulator
!  14nov2000  Dee       Implemented TPWs simulator (tpw in a saturated
!                                                   atmosphere)
!  09feb2001  da Silva  Implemented LWI
!  14Jan2004  Todling   Added insitu_null
!  01Sep2004  RT/Guo    Pointers initialized to null
!
!EOP
!-------------------------------------------------------------------------

!BOC

      integer, parameter :: nq2d = 15  ! slp, slp_cf, etc
      integer, parameter :: nq3d = 4   ! ua, va, tmpu and sphu for now


!     Indices for packed arrays q2d:
!     ------------------------------
      integer, parameter, public :: iqslp    = 1
      integer, parameter, public :: iqslp_cf = 2

      integer, parameter, public :: iqpb = 3
      integer, parameter, public :: iqzb = 4
      integer, parameter, public :: iqps = 5
      integer, parameter, public :: iqts = 6
      integer, parameter, public :: iqqs = 7
      integer, parameter, public :: iqzs = 8
      integer, parameter, public :: iqub = 9
      integer, parameter, public :: iqtb = 10
      integer, parameter, public :: iqqb = 11
      integer, parameter, public :: iqtss = 12 ! extrapolated sfc temperature
      integer, parameter, public :: iqtpw = 13
      integer, parameter, public :: iqtpws = 14
      integer, parameter, public :: iqlwi = 15

!     Indices for packed arrays q3d:
!     ------------------------------
      integer, parameter, public :: iqu = 1
      integer, parameter, public :: iqv = 2
      integer, parameter, public :: iqt = 3
      integer, parameter, public :: iqq = 4


!     Simulator vector: transformed form of "dyn_vect" with pre-computed
!      factors to speed up insitu calculations
!     ------------------------------------------------------------------
      type sim_vect

!        Grid information
!        ----------------
         type(dyn_grid) :: grid

!        Gridded fields
!        --------------
         real,  pointer ::  ze(:,:,:)=>null()    ! height at edges   (m)
         real,  pointer ::  pe(:,:,:)=>null()    ! pressure at edges (hPa)
         real,  pointer ::  pm(:,:,:)=>null()    ! pressure at center (hPa)

         real,  pointer ::  q2d(:,:,:)=>null()   ! Packed 2d arrays for m2o_* routines:
                                                 !  iq=1   sea-level-pressure (hPa)
                                                 !  iq=2   confidence in slp
                                                 !  iq=3   pressure middle bottom layer
                                                 !  iq=4   height above sfc at middle
                                                 !           of bottom layer
                                                 !  iq=5   surface pressure [hPa]
                                                 !  iq=6   surface temperature [K]
                                                 !  iq=7   qsat(Ts)  [g/kq]
                                                 !  iq=8   surface height [m]
                                                 !  iq=9   wind speed [m/s] at bottom layer
                                                 !  iq=10  temperature [K] at bottom layer
                                                 !  iq=11  spec. hum. [g/kg] at bottom layer
                                                 !  iq=12  extrapolated sfc temperature [K]
                                                 !  iq=13  total precipitable water [mm]
                                                 !  iq=14  tpw for qsat [mm]
                                                 !  iq=15  lwi (land-water-ice mask)

         real,  pointer ::  q3d(:,:,:,:)=>null() ! Packed 3d arrays for m2o_*
                                                 !  routines:
                                                 ! iq=1 zonal wind on A-grid (m/s)
                                                 ! iq=2 meridional wind on A-grid (m/s)
                                                 ! iq=3 temperature (degree K)
                                                 ! iq=4 specific humidity (g/kg)

      end type sim_vect

!EOC

      CONTAINS


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Init --- Prepares similator vector for insitu calculations
!
! !INTERFACE:
!
      subroutine Insitu_Init ( w_f, w_s, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(dyn_vect), intent(in)   :: w_f       ! Gridded model fields

! !OUTPUT PARAMETERS:
!

      type(sim_vect), intent(out)  :: w_s       ! Transformed model fields for
                                                !  insitu calculations.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   ...
                                                !

! !DESCRIPTION: This routine allocates memory and initializes the simulator
!               vector {\tt w\_s} with the contents of the dynamics vector
!  {\tt w\_f} and other internal parameters needed for the insitu calculations.
!
! !REVISION HISTORY:
!
!  26oct1999  da Silva  Initial code.
!  01dec1999  da Silva  Introduced q2d, q3d
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Insitu_Init'
      integer err, im, jm, km

      rc = 0

!     Check whether we have a clean slate
!     -----------------------------------
      if ( associated(w_s%ze)      .or.
     &     associated(w_s%pe)      .or.
     &     associated(w_s%pm)      .or.
     &     associated(w_s%q2d)     .or.
     &     associated(w_s%q3d)               )     then

           rc = 1
           return

      endif


!     OK, allocate the necessary memory
!     ---------------------------------
      im = w_f%grid%im;  jm = w_f%grid%jm;   km = w_f%grid%km
      allocate(w_s%ze(im,km+1,jm),     stat = err); if (err.ne. 0) rc = 2
      allocate(w_s%pe(im,km+1,jm),     stat = err); if (err.ne. 0) rc = 2
      allocate(w_s%pm(im,km,jm),       stat = err); if (err.ne. 0) rc = 2
      allocate(w_s%q2d(im,jm,nq2d),    stat = err); if (err.ne. 0) rc = 2
      allocate(w_s%q3d(im,jm,km,nq3d), stat = err); if (err.ne. 0) rc = 2

      if ( rc .ne. 0 ) return


!     Initialize sim_vect
!     -------------------
      w_s%grid = w_f%grid

      call Insitu_Prep ( w_f%grid%im, w_f%grid%jm, w_f%grid%km,
     &                   w_f%delp, w_f%u, w_f%v, w_f%pt, w_f%q,
     &                   w_f%ps, w_f%grid%ptop, w_f%grid%pint, w_f%grid%ks,
     &                   w_f%ts, w_f%phis, w_f%hs_stdv, w_f%lwi,
     &                   w_s%pe, w_s%pm, w_s%ze, w_s%q2d, w_s%q3d )


      end subroutine insitu_init


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Clean --- Free memory occupied by simulator vector
!
! !INTERFACE:
!
      subroutine Insitu_Clean ( w_ s )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(sim_vect), intent(inout)   :: w_s       ! Simulator vector


! !DESCRIPTION: This routine deallocates memory occupied by the simulator
!               vector {\tt w\_s}.
!
! !REVISION HISTORY:
!
!  26oct1999  da Silva  Initial code.
!  01dec1999  da Silva  Introduced q2d, q3d
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Insitu_Clean'

      if ( associated(w_s%ze)  )      deallocate ( w_s%ze   )
      if ( associated(w_s%pe)  )      deallocate ( w_s%pe   )
      if ( associated(w_s%pm)  )      deallocate ( w_s%pm   )
      if ( associated(w_s%q2d) )      deallocate ( w_s%q2d  )
      if ( associated(w_s%q3d) )      deallocate ( w_s%q3d  )

      end subroutine insitu_clean


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Null --- Nullifies simulator pointers
!
! !INTERFACE:
!
      subroutine Insitu_Null ( w_ s )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(sim_vect), intent(inout)   :: w_s       ! Simulator vector


! !DESCRIPTION: This routine nullify the pointer contents of simulator
!               vector {\tt w\_s}.
!
! !REVISION HISTORY:
!
!  14Jan2004  Todling   Initial code.
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Insitu_Clean'

      if ( associated(w_s%ze)  )      nullify ( w_s%ze   )
      if ( associated(w_s%pe)  )      nullify ( w_s%pe   )
      if ( associated(w_s%pm)  )      nullify ( w_s%pm   )
      if ( associated(w_s%q2d) )      nullify ( w_s%q2d  )
      if ( associated(w_s%q3d) )      nullify ( w_s%q3d  )

      end subroutine insitu_null

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Mass --- Simulates insitu mass observations
!
! !INTERFACE:
!
      subroutine Insitu_Mass ( w_s, lon, lat, lev, nobs, which, opt,
     &                         Iw_f, conf, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(sim_vect), intent(in)   :: w_s       ! Gridded model fields
      integer, intent(in)          :: nobs      ! Number of observations
      real, intent(in)             :: lon(nobs) ! longitude in degrees [-180,+180]
      real, intent(in)             :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)             :: lev(nobs) ! level in Hpa

      character(len=*), intent(in) :: which     ! Which variable to simulate,
                                                !  case-insensitive options are:
                                                !
                                                ! which        description
                                                ! -----  -----------------------
                                                ! mslp   Sea-Level-Pressure (hPa)
                                                ! slpf   surface_pressure/slp
                                                ! hght   geopotential heigts (m)
                                                ! tmpu   temperature (K)
                                                ! mixr   mixing ratio (g/kg)
                                                ! sphu   specific humidity (g/kg)
                                                ! pb     pressure [hPa]
                                                !         middle of bottom layer
                                                ! zb     height above sfc [m] at
                                                !         middle of bottom layer
                                                ! zs     surface height [m]
                                                ! ts     skin temperature [K]
                                                ! qs     qsat(ts) [g/kg]
                                                ! ps     surface pressure [hPa]
                                                ! ub     wind speed [m/s] at
                                                !         middle of bottom layer
                                                ! tb     temperature [K] at
                                                !         middle of bottom layer
                                                ! qb     specific humidity [] at
                                                !         middle of bottom layer
                                                ! tss    extrapolated surface temp [K]
                                                ! tpw    total precipitable water [mm]
                                                ! tpws   tpw for qsat [mm]
                                                ! lwi land-water-ice mask

      integer, intent(in)          :: opt       ! Interpolation option:
                                                !   0  default

! !OUTPUT PARAMETERS:
!
      real, intent(out)           :: Iw_f(nobs) ! Simulated insitu vector
      real, intent(out)           :: conf(nobs) ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !


! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This version is intended for mass variables.
!               As a first cut, all interoplation is linear with log(P) in the
!               vertical.
!
!               The sea level pressure $p_{sl}$ is computed by solving the
!               {\em hydrostatic balance} under mountains:
!               \be
!                    g  z_s = C_p  \theta  \(p_{sl}^\kappa - p_s^\kappa\)
!               \ee
!               where $\theta$ is the virtual potential temperature at the lowest model
!               level, and $p_s$ is the surface pressure.
!
!               For sea level pressure, {\tt conf} is inversely proportional to
!               the surface height with the form:
!               \be
!                        conf = 1 - \( |z_s| + \sigma_s \) / H_{max}
!               \ee
!               where $z_s$ is the surface height (could be negative), $\sigma_s$ the
!               standard deviation of $z_s$ (used by GWD), and $H_{max}$ is set to
!               $8\times10^3$.
!
!               For others: $conf=0$ if {\tt lev(nobs)} is below/above the lowest/highest
!               model level; $conf=1$ otherwise.
!
!               A large value undef is given if interpolation is not successful.
!
!               The input lon coordinate can be either [-180, 180] or [0, 360]
!
! !REVISION HISTORY:
!
!  07Sep1999  da Silva  Initial specs.
!  01nov1999  da Silva  Wrapper around m2o_mass + revised comments.
!  01dec1999  da Silva  Introduced q2d/q3d
!
!EOP
!-------------------------------------------------------------------------

!     The actual work is done in m2o_mass
!     -----------------------------------
      call m2o_mass ( w_s%q2d, w_s%q3d, w_s%ze, w_s%pe, w_s%pm,
     &                w_s%grid%im, w_s%grid%jm, w_s%grid%km,
     &                lon, lat, lev, nobs, which,
     &                opt, Iw_f, conf, rc )

      end subroutine Insitu_Mass

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Wind --- Simulates insitu wind observations
!
! !INTERFACE:
!
      subroutine Insitu_Wind ( w_s, lon, lat, lev, nobs, which, opt,
     &                         Iu_f, Iv_f, conf, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(sim_vect), intent(in)   :: w_s       ! Gridded model fields
      integer, intent(in)          :: nobs      ! Number of observations
      real, intent(in)             :: lon(nobs) ! longitude in degrees [-180,+180]
      real, intent(in)             :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)             :: lev(nobs) ! level in Hpa

      character(len=*), intent(in) :: which     ! Which variable to simulate,
                                                !  case-insensitive options are:
                                                !
                                                ! which        description
                                                ! -----  -----------------------
                                                ! wind   u-wind, v-wind

      integer, intent(in)          :: opt       ! Interpolation option:
                                                !   0  default

! !OUTPUT PARAMETERS:
!
      real, intent(out)           :: Iu_f(nobs) ! Simulated insitu u-wind vector
      real, intent(out)           :: Iv_f(nobs) ! Simulated insitu v-wind vector
      real, intent(out)           :: conf(nobs) ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !

! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This version is intended for wind related variables.
!               The input lon coordinate can be either [-180, 180] or [0, 360]
!               Linear interploation in x-y and log(p) in the vertical
!               Should consider using vertical subgrid after the basic code
!               is running.
!               $conf=0$ if lev(nobs) is below/above the lowest/highest
!               model level; $conf=1$ otherwise
!               A large value undef is given if interpolation is not successful.
!
!
! !REVISION HISTORY:
!
!  12oct1999  da Silva  Initial specs.
!  01dec1999  da Silva  Introduced q2d/q3d
!
!EOP
!-------------------------------------------------------------------------

!     The actual work is done in m2o_wind
!     -----------------------------------
      call m2o_wind ( w_s%q3d, w_s%pm, w_s%grid%im, w_s%grid%jm, w_s%grid%km,
     &                lon, lat, lev, nobs,
     &                which, opt, Iu_f, Iv_f, conf, rc)


      end subroutine Insitu_Wind


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Ocean --- Simulates insitu surface layer obs over ocean
!
! !INTERFACE:
!
      subroutine Insitu_Ocean ( w_s, lon, lat, zlev, nobs, opt,
     &                          u_z, q_s, t_s,
     &                          u_b, q_b, t_b, p_b, z_b, conf, rc,
     &                          q_z, t_z )  ! optional

! !USES:
!


      Implicit NONE

! !INPUT PARAMETERS:
!

      type(sim_vect), intent(in)   :: w_s       ! Gridded model fields
      integer, intent(in)          :: nobs      ! Number of observations
      real, intent(in)             :: lon(nobs) ! longitude in degrees [-180,+180]
      real, intent(in)             :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)             :: zlev(nobs) ! height of obs [m]

      integer, intent(in)          :: opt       ! Interpolation option:
                                                !   0  default

! !OUTPUT PARAMETERS:
!

                                                ! Surface layer quantities:
      real, intent(out)            :: u_z(nobs) !   wind SPEED at zlev

                                                ! Surface quantities:
      real, intent(out)            :: t_s(nobs) !   sea surface temperature [K]
      real, intent(out)            :: q_s(nobs) !   0.98 * qsat(t_s) [g/kg]

                                                ! Model bottom layer quantities:
      real, intent(out)            :: u_b(nobs) !   wind SPEED at p_b(nobs)
      real, intent(out)            :: t_b(nobs) !   temperature [K] at p_b(nobs)
      real, intent(out)            :: q_b(nobs) !   spec. humidity [g/kg] at p_b
      real, intent(out)            :: p_b(nobs) !   pressure [hPa] at middle of
                                                !    model bottom layer
      real, intent(out)            :: z_b(nobs) !   height above sfc [m] at
                                                !    middle of model bottom layer

      real, intent(out)            :: conf(nobs) ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   can't simulate pb
                                                !  2   can't simulate zb
                                                !  3   can't simulate ub
                                                !  4   can't simulate tb
                                                !  5   can't simulate qb
                                                !  6   can't simulate ts
                                                !  7   can't simulate qs

                                                ! Optional surface layer quantities:
      real, intent(out), OPTIONAL  :: q_z(nobs) !   spec. humidity [g/kg] at zlev
      real, intent(out), OPTIONAL  :: t_z(nobs) !   temperature [K] at zlev


! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations in the surface layer. It can also optionally
!  return the same simulated fields at the midddle of the model bottom
!  layer. See figure below.
!
!  \bv
!
!     _________________________________________ p(K-1)
!
!
!
!
!                   u_b, t_b, q_b
!     ................... * ................... p_b = ( p(K)+p(K-1) ) / 2
!
!
!
!                   u_z, t_z, q_z
!     ................... * ................... z = zlev
!
!     _________________________________________ p(K) = p_s
!
!
!  \ev
!
! !REVISION HISTORY:
!
!  12oct1999  da Silva  Initial specs.
!  17nov1999  da Silva  Revised interface, initial code.
!  03dec1999  da Silva  Added ub, tb, qb
!  16nov2000  Dee       Fixed return codes
!  08feb2001  da Silva  Eliminated points contaminated by land/ice
!
!EOP
!-------------------------------------------------------------------------

      real lwi(nobs)    ! mask: 0=water, 1=land, 2=ice

      integer i, n

      real t, z, dq1, dt1, dq2, dt2, qs, theta

      theta ( T, Z ) = T + 0.01 * Z   ! pot temperature: keep this for consistency
                                      !  with m_lp()




!     No obs, nothing to do
!     ---------------------
      rc = 0
      if ( nobs .eq. 0 ) return


!     -----------------------------------------
!     TO DO: make sure (lat,lon) are over ocean
!     -----------------------------------------

!     Get pressure at middle of model bottom layer
!     --------------------------------------------
      call Insitu_Mass ( w_s, lon, lat, zlev, nobs, 'pb', opt,
     &                   p_b, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 1
         return
      end if

!     Get height above sfc at middle of model bottom layer
!     ----------------------------------------------------
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'zb', opt,
     &                   z_b, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 2
         return
      end if


!     Obtain wind speed at middle of model bottom layer
!     ------------------------------------------------
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'ub', opt,
     &                   u_b, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 3
         return
      end if


!     Get (T,q) at middle of model bottom layer
!     -----------------------------------------
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'tb', opt,
     &                   t_b, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 4
         return
      end if
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'qb', opt,
     &                   q_b, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 5
         return
      end if

!     Get sst
!     -------
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'ts', opt,
     &                   t_s, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 6
         return
      end if

!     Get q_s=qsat(sst)
!     -----------------
      call Insitu_Mass ( w_s, lon, lat, p_b, nobs, 'qs', opt,
     &                   q_s, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 7
         return
      end if

!     Get land-water-ice mask
!     -----------------------
      call Insitu_Mass ( w_s, lon, lat, zlev, nobs, 'lwi', opt,
     &                   lwi, conf, rc )
      n = count ( conf .ne. 1.0 )
      if ( rc .ne. 0 .or. n .ne. 0 ) then
         rc = 8
         return
      end if


!     Use similarity theory to obtain model fields at obs height
!     ------------------------------------------------------------
      n = 0
      call LP_Init()   ! Initialize Large-Pond surface layer package
      do i = 1, nobs

         qs  = 0.98 * q_s(i)
         dq1 = qs - q_b(i)
         dt1 = t_s(i) - theta(t_b(i),z_b(i))

!        Skip computation of dt2/dq2 if not needed
!        -----------------------------------------
         if ( present(t_z) ) then
              call LP_MoveZ ( z_b(i), u_b(i), t_b(i), dq1, dt1, t_s(i),
     &                        zlev(i), u_z(i), conf(i),
     &                        dq2, dt2 )
         else if ( present(q_z) ) then
              call LP_MoveZ ( z_b(i), u_b(i), t_b(i), dq1, dt1, t_s(i),
     &                        zlev(i), u_z(i), conf(i),
     &                        dq2 )
         else
              call LP_MoveZ ( z_b(i), u_b(i), t_b(i), dq1, dt1, t_s(i),
     &                        zlev(i), u_z(i), conf(i) )
         end if

         if ( u_z(i) .lt. 0.0 ) then
              u_z(i) = undef
              conf(i) = 0.0
         end if

         if ( conf(i) .eq. 1.0 .and. present(q_z) ) then
              q_z(i) = qs - dq2
             if ( q_z(i) .le. 0.0 ) then
                  q_z(i) = undef
                  conf(i) = 0.0
             endif
         end if

         if ( conf(i) .eq. 1.0 .and. present(t_z) ) then
              t_z(i) = t_s(i) - dt2 - 0.01 * zlev(i)
             if ( t_z(i) .lt. 0.0 ) then
                  t_z(i) = undef
                  conf(i) = 0.0
             end if
         end if

!!         if ( conf(i) .ne. 1.0 )
!!     &        write(*,'(a,3f8.2)') ': LP_MoveZ failed at (lon,lat,zlev) = ',
!!     &                                lon(i), lat(i), zlev(i)

         if ( lwi(i) .ne. 0.0 ) then
              conf(i) = 0.0           ! eliminate obs over ice/land
              n = n + 1
         end if


      end do

!     Info messages: remove for MPI
!     -----------------------------
      if ( n .ne. 0 ) 
     &     write(*,'(a,i5,a)') 'insitu_ocean: excluded ', n, 
     &                         ' obs not over water'
      n = count ( conf .ne. 1.0 ) - n
      if ( n .ne. 0 ) 
     &     write(*,'(a,i5,a)') 'insitu_ocean: LP_MoveZ failed at ', n, 
     &                         ' locations'

!     All done
!     --------
      rc = 0
      return


      end subroutine Insitu_Ocean


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Insitu_Prep --- Pack state variables for interpolation routines
!
! !INTERFACE:
!
      subroutine Insitu_Prep(im, jm, km, delp, u, v, pt, sphu,
     &                       ps,  ptop, pint, ks,
     &                       ts, phis, stdv, lwi,
     &                       pe, pm, ze, q2d, q3d )

! !USES:
!
      USE m_const, only: cp    => cpm
      USE m_const, only: rair  => rgas
      USE m_const, only: cappa => kappa
      USE m_const, only: zvir
      USE m_const, only: grav

      Implicit NONE


! !INPUT PARAMETERS:

      integer, intent(in)  :: im, jm ,km       ! grid dimensions

      real,    intent(in)  ::     u(im,jm,km)  ! u-wind
      real,    intent(in)  ::     v(im,jm,km)  ! v-wind
      real,    intent(in)  ::    pt(im,jm,km)  ! virtual potential temperature
      real,    intent(in)  ::  delp(im,jm,km)  ! Delta pressure
      real,    intent(in)  ::  sphu(im,jm,km)  ! Specific humd

      real,    intent(in)  ::  ps(im,jm)       ! Surface pressure (Pa)
      real,    intent(in)  ::  ptop            ! Pressure at top (Pa)
      real,    intent(in)  ::  pint            ! Pressure at interface (Pa)
      integer, intent(in)  ::  ks              ! no. of pure pressure layers
                                               ! Input BCs:
      real,    intent(in)  ::  ts(im,jm)       ! Surface temperature [K]
      real,    intent(in)  :: phis (im,jm)     ! surface geopotential
      real,    intent(in)  :: stdv (im,jm)     ! standard deviation (m)
      real,    intent(in)  :: lwi  (im,jm)     ! land-water-ice mask

! !OUTPUT PARAMETERS:

      real,    intent(out) ::  pe(im,km+1,jm)  ! pressure at edges (hPa)
      real,    intent(out) ::  pm(im,km  ,jm)  ! pressure at center (hPa)
      real,    intent(out) ::  ze(im,km+1,jm)  ! height at edges   (m)

                                               ! Packed 2D arrays for m2o_*()
      real,    intent(out) ::  q2d(im,jm,nq2d)

                                               ! Packed 3D arrays for m2o_*()
      real,    intent(out) ::  q3d(im,jm,km,nq3d)



! !DESCRIPTION: This routine packs the dynamics vector {\tt w\_f} in a form
!               convenient for the actual interpolation routines
!               ({\tt m2o\_mass} and {\tt m2o\_wind}). It defines several
!  handy pressure related arrays and converts winds to the A-grid.
!
! !REVISION HISTORY:
!
!  26oct1999  SJ Lin    Initial code.
!  26oct1999  da Silva  Added p_top to argument list, added ProTeX compliant
!                       prologues, removed assorted debris.
!  26oct1999  da Silva  Changed name from dyn_pack() to Insitu_prep();
!                       passed in ks, ptop, ak and bk.
!  15nov1999  da Silva  Fixed units of sphu (g/g -> g/kg)
!  01dec1999  da Silva  Introduced q2d, q3d
!  03dec1999  da Silva  Added ub, tb, qb
!  20Mar2000  da Silva  Revised SLP calculation: removed CCM3 option,
!                       introduced T_star, etc.
!  05Oct2000  Dee       Pack tpw in q2d
!  14nov2000  Dee       Pack tpws in q2d
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'insitu_prep'

! Local

      integer i, j, k

      real   pkz(im,jm,km)
      real   pk(im,jm,km+1)            ! pressure at edges (hPa)
      real peln(im,km+1,jm)            ! log(pe)

      real p_offset, p_bot
      integer k_bot, k1, k2
      real Hmax, base, rkap

      real ua(im,jm)
      real va(im,jm)
      real   sinlon(im)
      real   coslon(im)

      real rg
      real dl
      real t_ref(im,jm), p_ref(im,jm)
      real sum1, sum2

!     for tpws calculation:

      real, parameter ::  p_tpws = 100.0  ! intgrate from surface to p=p_tpws
                       !  quantities at center of layer k:
      real pmk(im,jm)  !     pressure
      real esk(im,jm)  !     saturation evaporation pressure
      real qsk(im,jm)  !     saturation specific humidity


!     TO DO: Handle this
!     ------------------
! Local model specific parameters:
!!!      real, parameter ::  cp = 1004.64        ! cp for moist air
!!!      real, parameter ::  rair = 287.04
!!!      real, parameter ::  cappa = rair/cp
!!!      real, parameter ::  zvir = 4.61e2/rair - 1.
!!!      real, parameter ::  grav = 9.80616

!     Saturation specific humidity as in CCM3's trefoce()
!     Units are g/kg
!     ---------------------------------------------------
      real T, qsat_
      qsat_ ( T ) = 640380.0 * 1000. / exp ( 5107.4 / T )

! Define logitude at the center of the finite-volume

         dl = 8.*atan(1.0) / float(im)
      do i=1,im/2
         coslon(i)      = -cos((i-1)*dl)
         coslon(i+im/2) = -coslon(i)
         sinlon(i)      = -sin((i-1)*dl)
         sinlon(i+im/2) = -sinlon(i)
      enddo

! Set up the hybrid sigma-P vertical coordinate; only ptop is needed
! for this code.

! Compute height, pk, pe

      call geopm(ptop, pe, pk, delp, ze, phis, pt, im, jm, km, 1, jm,
     &           cp, cappa, 1)

! Compute pkz for conversion between pt and temperature
      call pkez ( im, jm, km, 1, jm, ptop,
     &            pe, pk, cappa, ks, peln, pkz, .false.)

      rg = 1./ grav

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k, ua, va)
#endif

#if ( defined SGI )
c$doacross   local(i,j,k, ua, va)
#endif

      do k=1,km
! map D-grid wind to A-grid
      call d2a(u(1,1,k), v(1,1,k), ua ,va,
     &         im, jm, 1, jm, coslon, sinlon)

         do j=1,jm
            do i=1,im
               q3d(i,j,k,iqu) = ua(i,j)
               q3d(i,j,k,iqv) = va(i,j)
               q3d(i,j,k,iqt) = pt(i,j,k)*pkz(i,j,k)/(1.+zvir*sphu(i,j,k))
               q3d(i,j,k,iqq) = 1000. * sphu(i,j,k) ! g/g -> g/kg
            enddo
         enddo

      enddo

!     Constants for SLP calculation

      Hmax = 8.e3
      rkap = 1. / cappa
      p_offset = 150. ! 150 hPa above surface

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,p_bot,k_bot,k1,k2)
#endif

#if ( defined SGI )
c$doacross   local(i,j,k,p_bot,k_bot,k1,k2)
#endif
      do j=1,jm

         do k=1,km+1
            do i=1,im
               pe(i,k,j) = pe(i,k,j) * 0.01
               ze(i,k,j) = ze(i,k,j) * rg
            enddo
         enddo

         do k=1,km
            do i=1,im
               pm(i,k,j) = 0.5*(pe(i,k,j) + pe(i,k+1,j))
            enddo
         enddo

! Compute SLP and the  confidence level

! Find reference temperature by averaging Tv in a couple of
!  layers above the PBL.
         do i = 1, im
            p_bot = pm(i,km,j) - p_offset
            k_bot = -1
            do k = km, 1, -1
               k_bot = k
               if ( pe(i,k+1,j) .lt. p_bot ) exit
            end do
            if ( k_bot .lt. 2 ) then
               print *, myname//': got  k_bot<2   while computing T_ref'
               call exit(1)
            else
               k1 = k_bot - 1
               k2 = k_bot
               t_ref(i,j) = ( pt(i,j,k1) * pkz(i,j,k1) * delp(i,j,k1) +
     &                        pt(i,j,k2) * pkz(i,j,k2) * delp(i,j,k2) )
     &                    / ( delp(i,j,k1) + delp(i,j,k2) )
               p_ref(i,j) = ( pe(i,k_bot+1,j) + pe(i,k_bot-1,j) ) / 2.
            end if
         end do

      end do


! calculated SLP and extrapolated surface temperature
         call slp_ukmo ( im, jm, T_ref, p_ref, pe(1:im,km+1,1:jm),
     &                   phis(1:im,1:jm)/grav, q2d(1:im,1:jm,iqslp),
     &                   q2d(1:im,1:jm,iqtss) )


! compute SLP confidence
      do j = 1, jm
         do i=1,im
            base = abs(ze(i,km+1,j)) + stdv(i,j)
            if( base .lt. 1.) then
                q2d(i,j,iqslp_cf) = 1.
            else
                q2d(i,j,iqslp_cf) = 1. - base / Hmax
            endif
         enddo
      enddo

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j)
#endif

#if ( defined SGI )
c$doacross   local(i,j)
#endif

      do j=1,jm
         do i=1,im
            q2d(i,j,iqpb) = pm(i,km,j)
            q2d(i,j,iqzb) = ( ze(i,km,j) - ze(i,km+1,j) ) / 2.
            q2d(i,j,iqzs) = ze(i,km+1,j)
            q2d(i,j,iqts) = ts(i,j)
            q2d(i,j,iqqs) = qsat_(ts(i,j))
            q2d(i,j,iqps) = pe(i,km+1,j)
            q2d(i,j,iqub) = sqrt ( q3d(i,j,km,iqu)**2 + q3d(i,j,km,iqv)**2 )
            q2d(i,j,iqtb) = q3d(i,j,km,iqt)
            q2d(i,j,iqqb) = q3d(i,j,km,iqq)
            q2d(i,j,iqlwi) = lwi(i,j)
         end do
      end do

!     Compute tpw

      q2d(:,:,iqtpw ) = 0.0
      do k=1,km
         do j=1,jm
            do i=1,im
               q2d(i,j,iqtpw ) = q2d(i,j,iqtpw ) + delp(i,j,k) * q3d(i,j,k,iqq)
            end do
         end do
      end do
      q2d(:,:,iqtpw ) = 0.001 * rg * q2d(:,:,iqtpw )   ! (g/kg)*(kg/ms^2) -> (kg water/m^2)

!     Compute tpws: tpw of a saturated atmosphere, integrated from the surface
!                   up to p=p_tpws. Saturation specific humidity as in vqsat()
!                   (consistent with fvccm3-1.2.0)

      q2d(:,:,iqtpws) = 0.0
      call gestbl()           ! initialize saturation vapor pressure table  
      do k=km,1,-1
         do j=1,jm
            do i=1,im
               pmk(i,j) = pm(i,k,j) ! pressure at center of layer k [hPa]
            end do
         end do
         if ( maxval(pmk) .lt. p_tpws ) exit
         call vqsat(q3d(1,1,k,iqt),100.*pmk,esk,qsk,im*jm,undef)    ! qsk is qsat [g/g]
         do j=1,jm
            do i=1,im
               q2d(i,j,iqtpws) = q2d(i,j,iqtpws) + delp(i,j,k) * qsk(i,j)
            end do
         end do
      end do

      q2d(:,:,iqtpws) = rg * q2d(:,:,iqtpws)   ! (g/g)*(kg/ms^2) -> (kg water/m^2)


      return
      end subroutine insitu_prep

! .........................................................................

      subroutine geopm(ptop,pe,pk,delp,wz,hs,pt,im,jm, km,
     &                 jfirst, jlast, cp,akap,id)

      implicit none

! !INPUT PARAMETERS:

      integer im, jm, km, jfirst, jlast, id
      real akap, cp, ptop
      real hs(im,jm)
      real delp(im,jm,km)
      real pt(im,jm,km)

! !OUTPUT PARAMETERS
      real pe(im,km+1,jm)                ! only if id .ne. 0
      real wz(im,km+1,jm)                !
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
          wz(i,km+1,j) = hs(i,j)
        enddo

c Top down
        do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
            pk2(i,k) = p2d(i,k) ** akap
          enddo
        enddo

c Bottom up
        do k=km,1,-1
          do i=1,im
            wz(i,k,j) = wz(i,k+1,j)+cp*pt(i,j,k)*(pk2(i,k+1)-pk2(i,k))
          enddo
        enddo

        if(id .ne. 0) then
          do k=1,km+1
            do i=1,im
              pe(i,k,j) = p2d(i,k)
              pk(i,j,k) = pk2(i,k)
            enddo
          enddo
        endif

1000  continue

      return
      end subroutine geopm

! .........................................................................

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

      subroutine d2a(u,v,ua,va,im,jm,jfirst,jlast,coslon,sinlon)
c This is primarily for turbulence package designed for A-grid.
c Also used for output to A-grid.
! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1; removed fvcore.h
! WS 99.07.26 : Added jfirst and jlast as arguments

      implicit none
      integer im, jm, jfirst, jlast
! WS 99.07.26 : u must be ghosted N2S1
      real u(im,jm),v(im,jm),ua(im,jm),va(im,jm),coslon(im),sinlon(im)

      integer   imh
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1
      real un, vn, us, vs

c Convert D-grid winds to A-grid
c u --> ua, v --> va

      real utmp(im,jm),vtmp(im,jm)

      imh = im/2

      js = 3
      jn = jm - js + 1
      im1 = im-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 45 j=jn+1,jm-1
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jm-1
50    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,im-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) -
     &                 (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) -
     &                 (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) -
     &                 (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) -
     &                 (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) -
     &                 (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
c Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i)
     &        + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i)
     &        + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      us = us/im
      vs = vs/im

      do i=1,imh
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN
c Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i)
     &        + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i)
     &        + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      un = un/im
      vn = vn/im

      do i=1,imh
      ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
      va(i,jm) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jm) = -ua(i,jm)
      va(i+imh,jm) = -va(i,jm)
      enddo

      ENDIF

      do 100 j=2,jm-1
      do 100 i=1,im
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)
      return
      end subroutine d2a


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  slp_ukmo --- Computes sea-level pressure ("UKMO" algorithm)
!
! !INTERFACE:
!
      subroutine slp_ukmo ( im, jm, T_ref, p_ref, ps, zs, slp, T_star )

! !USES:
!
      USE m_const, only: grav
      USE m_const, only: rgas
      USE m_const, only: gamma
      Implicit NONE


! !INPUT PARAMETERS:

      integer, intent(in)   :: im, jm           ! grid dimensions

      real,    intent(in)   :: T_ref(im,jm)     ! Reference virtual temperature (K)
      real,    intent(in)   :: p_ref(im,jm)     ! Reference pressure level (hPa)
      real,    intent(in)   ::    ps(im,jm)     ! surface pressure (hPa)
      real,    intent(in)   ::    zs(im,jm)     ! topographic height (m)

! !OUTPUT PARAMETERS:

      real,    intent(out)  ::   slp(im,jm)     ! sea-level pressure (hPa)
      real,    intent(out)  ::   T_star(im,jm)  ! extrapolated temperature (K)
                                                !   valid at the surface


! !DESCRIPTION: Let's assume that the virtual temperature at the {\em fictious}
!               layer under the ground is given by
!  \be
!        T(z) = T_* + \gamma z, \qquad \gamma \equiv = 6.5 K/Km
!  \ee
!  where $T_*$ is a temperature assumed to be valid at the surface, but different
!  from model's ground temperature (which is heavily affected by the diurnal cycle.)
!  Integrating the hydrostatic equation with this particular profile of $T(z)$
!  we obtain
!  \be
!       p_0 = p_s \( {T_* + \gamma z_s \over T_*} \)^{g/(R\gamma)}
!  \ee
!  where $z_s$ is the model's topographic height, and $p_0$ is the desired sea-level
!  pressure.
!
!  In order to avoid spurious diurnal cycle in the sea-level pressure, we do not
!  use the model's ground temperature for $T_*$. Instead, we extrapolated the
!  virtual temperature from a reference level just above the PBL (say, the fifth
!  eta layer, $p ~700$ hPa) using the constant moist adiabatic lapse rate $\gamma$,
!  viz.
!  \be
!          T_* = T_{ref} \( p_s / p_{ref} \) ^{g/(R\gamma)}
!  \ee
!  where $T_{ref}$ is a reference temperature valid at level $p_{ref}$.
!  For additional information consult Woodage (1985, Meteor. Mag., 114, 1-13)
!  and Ingleby (1995, Wea. Forecasting, 10, 172-182).
!
! !REVISION HISTORY:
!
!  20Mar2000  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

      integer i, j
      real factor, yfactor

      factor  = grav / ( Rgas * gamma )
      yfactor = ( Rgas * gamma ) / grav
      do j = 1, jm
         do i = 1, im
            T_star(i,j) = T_ref(i,j)
     &                  * ( ps(i,j) / p_ref(i,j) ) ** yfactor
               slp(i,j) = ps(i,j)
     &                  * (1.0 + gamma*zs(i,j)/T_star(i,j) ) ** factor
         end do
      end do

!!!      print *, 'ps: ', minval(ps), maxval(ps)
!!!      print *, 'zs: ', minval(zs), maxval(zs)
!!!      print *, 'tr: ', minval(t_ref), maxval(t_ref)
!!!      print *, 'pr: ', minval(p_ref), maxval(p_ref)
!!!      print *, 'tss: ', minval(t_star), maxval(t_star)
!!!      print *, 'slp: ', minval(slp), maxval(slp)

      end subroutine slp_ukmo


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  vqsat --- Compute saturation specific humidity
!                      (consistent with fvccm3-1.2.0)
!
! !INTERFACE:
!
      subroutine vqsat ( t, p, es, qs, len, undef )

! !USES:

      Implicit NONE

! !INPUT PARAMETERS:

      integer, intent(in)     :: len         ! vector length
      real, intent(in)        :: t(len)      ! temperature [K]
      real, intent(in)        :: p(len)      ! pressure [Pa]
      real, intent(in)        :: undef       ! undefined value

! !OUTPUT PARAMETERS:

      real, intent(out)       :: es(len)     ! Saturation vapor pressure [g/g]
      real, intent(out)       :: qs(len)     ! Saturation specific humidity [Pa]

! !DESCRIPTION: This routine computes saturation vapor pressure [Pa] and
!             saturation specific humidity [g/g] as a function of
!             temperature [K] and pressure [Pa].
!
!----------------------------Code History-------------------------------
! Original version:  J. Hack
! Standardized:      J. Rosinski, June 1992
!                    T. Acker, March 1996
! Reviewed:          J. Hack, August 1992
! Modified:          To handle pressure level data which can have undefined
!                    values.  This code was also stripped of the use of the
!                    land index array location.
!-----------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!  17nov2000  Dee    Obtained from Sharon Nebuda, taken from fvccm3-1.2.0
!                    Added DAO prologue; worried about name of developer.
!                    Modification: Allow array of pressures.
!
!EOP
!-------------------------------------------------------------------------

C
C--------------------------Local Variables------------------------------
C
      real omeps               ! 1 - 0.622
      integer i,ii             ! Local vector indices
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C

C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
c     write(6,*) ' epsqs = ',epsqs
CDIR$ IVDEP
      do i=1,len
        if (t(i) .ne. undef) then
         es(i) = estblf(t(i))
C
C Saturation specific humidity
C
         qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))

C
C The following check is to avoid the generation of negative values
C that can occur in the upper stratosphere and mesosphere
C
         qs(i) = min(1.0,qs(i))
C
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p(i)
         end if
        else
          qs(i) = undef
          es(i) = undef
        endif
      end do

      return
C
      end subroutine vqsat


      subroutine gestbl
C-----------------------------------------------------------------------
C
C Builds saturation vapor pressure table for later lookup procedure.
C Uses Goff & Gratch (1946) relationships to generate the table
C according to a set of free parameters defined below.  Auxiliary
C routines are also included for making rapid estimates (well with 1%)
C of both es and d(es)/dt for the particular table configuration.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Aug 1992
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tmn           ! Minimum temperature entry in es lookup table
      real tmx           ! Maximum temperature entry in es lookup table
      real epsil         ! Ratio of h2o to dry air molecular weights
      real trice         ! Transition range from es over range to es over ice
      real latvap        ! Latent heat of vaporization
      real latice        ! Latent heat of fusion
      real rh2o          ! Gas constant for water vapor
      real cpair         ! Specific heat of dry air
C
C---------------------------Local variables-----------------------------
C
      real t             ! Temperature
      integer n          ! Increment counter
      integer lentbl     ! Calculated length of lookup table
      integer itype      ! Ice phase: 0 -> no ice phase
                         !            1 -> ice phase, no transition
                         !           -x -> ice phase, x degree transition
      logical ip         ! Ice phase logical flag
C
C---------------------------Statement function--------------------------
C
C
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C

C-----------------------------------------------------------------------
C
C Set es table parameters
C
      tmin   = 173.16       ! Minimum temperature entry in table
      tmax   = 375.16       ! Maximum temperature entry in table
      ttrice = 20.          ! Trans. range from es over h2o to es over ice
      icephs = .true.       ! Ice phase (true or false)
C
C Set physical constants required for es calculation
C
      epsqs  = 0.622
      hlatv  = 2.5104e6
      hlatf  = 3.336e5
      rgasv  = 4.61e2
      cp     = 1004.64
C
      lentbl = ifix(tmax-tmin+2.000001)
      if (lentbl .gt. plenest) then
         write(6,9000) tmax, tmin, plenest
         stop
      end if
C
C Begin building es table.
C Check whether ice phase requested.
C If so, set appropriate transition range for temperature
C
      if (icephs) then
         if(ttrice.ne.0.0) then
            itype = -ttrice
         else
            itype = 1
         end if
      else
         itype = 0
      end if
C
      t = tmin - 1.0
      do n=1,lentbl
         t = t + 1.0
         call gffgch(t,estbl(n),itype)
      end do
C
      do n=lentbl+1,plenest
         estbl(n) = -99999.0
      end do
C
C Table complete -- Set coefficients for polynomial approximation of
C difference between saturation vapor press over water and saturation
C pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
C is valid in the range -40 < t < 0 (degrees C).
C
C                  --- Degree 5 approximation ---
C
      pcf(1) =  5.04469588506e-01
      pcf(2) = -5.47288442819e+00
      pcf(3) = -3.67471858735e-01
      pcf(4) = -8.95963532403e-03
      pcf(5) = -7.78053686625e-05
C
C                  --- Degree 6 approximation ---
C
C-----pcf(1) =  7.63285250063e-02
C-----pcf(2) = -5.86048427932e+00
C-----pcf(3) = -4.38660831780e-01
C-----pcf(4) = -1.37898276415e-02
C-----pcf(5) = -2.14444472424e-04
C-----pcf(6) = -1.36639103771e-06
C
C
 9000 format('GESTBL: FATAL ERROR *********************************',/,
     $     ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH',
     $     ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/,
     $     ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
C
      end subroutine gestbl

      subroutine gffgch(t       ,es      ,itype   )
C-----------------------------------------------------------------------
C
C Computes saturation vapor pressure over water and/or over ice using
C Goff & Gratch (1946) relationships.  T (temperature), and itype are
C input parameters, while es (saturation vapor pressure) is an output
C parameter.  The input parameter itype serves two purposes: a value of
C zero indicates that saturation vapor pressures over water are to be
C returned (regardless of temperature), while a value of one indicates
C that saturation vapor pressures over ice should be returned when t is
C less than 273.16 degrees k.  If itype is negative, its absolute value
C is interpreted to define a temperature transition region below 273.16
C degrees k in which the returned saturation vapor pressure is a
C weighted average of the respective ice and water value.  That is, in
C the temperature range 0 => -itype degrees c, the saturation vapor
C pressures are assumed to be a weighted average of the vapor pressure
C over supercooled water and ice (all water at 0 c; all ice at -itype
C c).  Maximum transition range => 40 c
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real t          ! Temperature
      integer itype   ! Flag for ice phase and associated transition
C
C Output arguments
C
      real es         ! Saturation vapor pressure
C
C---------------------------Local variables-----------------------------
C
      real e1         ! Intermediate scratch variable for es over water
      real e2         ! Intermediate scratch variable for es over water
      real eswtr      ! Saturation vapor pressure over water
      real f          ! Intermediate scratch variable for es over water
      real f1         ! Intermediate scratch variable for es over water
      real f2         ! Intermediate scratch variable for es over water
      real f3         ! Intermediate scratch variable for es over water
      real f4         ! Intermediate scratch variable for es over water
      real f5         ! Intermediate scratch variable for es over water
      real ps         ! Reference pressure (mb)
      real t0         ! Reference temperature (freezing point of water)
      real term1      ! Intermediate scratch variable for es over ice
      real term2      ! Intermediate scratch variable for es over ice
      real term3      ! Intermediate scratch variable for es over ice
      real tr         ! Transition range for es over water to es over ice
      real ts         ! Reference temperature (boiling point of water)
      real weight     ! Intermediate scratch variable for es transition
      integer itypo   ! Intermediate scratch variable for holding itype
C
C-----------------------------------------------------------------------
C
C Check on whether there is to be a transition region for es
C
      if (itype.lt.0) then
        tr    = abs(float(itype))
        itypo = itype
        itype = 1
      else
        tr    = 0.0
        itypo = itype
      end if
      if (tr .gt. 40.0) then
        write(6,900) tr
        stop
      end if
C
      if(t .lt. (273.16 - tr) .and. itype.eq.1) go to 10
C
C Water
C
      ps = 1013.246
      ts = 373.16
      e1 = 11.344*(1.0 - t/ts)
      e2 = -3.49149*(ts/t - 1.0)
      f1 = -7.90298*(ts/t - 1.0)
      f2 = 5.02808*log10(ts/t)
      f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
      f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
      f5 = log10(ps)
      f  = f1 + f2 + f3 + f4 + f5
      es = (10.0**f)*100.0
      eswtr = es
C
      if(t.ge.273.16 .or. itype.eq.0) go to 20
C
C Ice
C
   10 continue
      t0    = 273.16
      term1 = 2.01889049/(t0/t)
      term2 = 3.56654*log(t0/t)
      term3 = 20.947031*(t0/t)
      es    = 575.185606e10*exp(-(term1 + term2 + term3))
C
      if (t.lt.(273.16 - tr)) go to 20
C
C Weighted transition between water and ice
C
      weight = min((273.16 - t)/tr,1.0)
      es = weight*es + (1.0 - weight)*eswtr
C
   20 continue
      itype = itypo
      return
C
  900 format('GFFGCH: FATAL ERROR ******************************',/,
     $       'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR',
     $       ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF',
     $       ' 40.0 DEGREES C',/, ' TR = ',f7.2)
C
      end subroutine gffgch

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  m2o_mass --- Simulated insitu mass observations (low level)
!
! !INTERFACE:
!
      subroutine m2o_mass ( q2d, q3d, ze, pe, pm, im, jm, km,
     &                      lon, lat, lev, nobs, which,
     &                      opt, Iw_f, conf, rc )

! !USES:

      Implicit NONE

! !INPUT PARAMETERS:

      integer, intent(in)  :: nobs      ! Number of observations
      real, intent(in)     :: lon(nobs) ! longitude in degrees
      real, intent(in)     :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)     :: lev(nobs) ! level in Hpa

      integer, intent(in)  :: im        ! E-W dimension
      integer, intent(in)  :: jm        ! N-S dimension
      integer, intent(in)  :: km        ! # of layers

      real, intent(in)     :: ze(im,km+1,jm)  !HGHT at layer edges (m)
      real, intent(in)     :: pe(im,km+1,jm)  !Pressure at edges (hPa)
      real, intent(in)     :: pm(im,km,jm)    !Pressure at center (hPa)

                                               ! Packed 2D arrays
      real,    intent(in)  ::  q2d(im,jm,nq2d)

                                               ! Packed 3D arrays
      real,    intent(in)  ::  q3d(im,jm,km,nq3d)


      character(len=*), intent(in) :: which ! Which variable to simulate,
!                                           !  case-insensitive options are:
!                                           !
!                                           ! which        description
!                                           ! -----  -----------------------
!                                           ! mslp   Sea-Level-Pressure (hPa)
!                                           ! slpf   surface_pressure/slp
!                                           ! hght   geopotential heigts (m)
!                                           ! tmpu   temperature (K)
!                                           ! mixr   mixing ratio (g/kg)
!                                           ! sphu   specific humidity (g/kg)
!                                           ! pb     pressure at middle of bottom
                                            !          layer [hPa]
                                            ! zb     height above sfc at middle
                                            !          of bottom layer [m]
                                            ! zs     surface height [m]
                                            ! ts     skin temperature [K]
                                            ! qs     qsat(ts) [g/kg]
                                            ! ps     surface pressure [hPa]

      integer, intent(in)          :: opt   ! Interpolation option:
!                                           !   0  default

! !OUTPUT PARAMETERS:
!
      real, intent(out)       :: Iw_f(nobs) ! Simulated insitu vector
      real, intent(out)       :: conf(nobs) ! Confidence of interpolated
!                                           !  values:
!                                           !  = 1   reliable interpolation
!                                           !  = 0   could not interpolate,
!                                           !         no interpolated value
!                                           !         is set at this point
!                                           ! Any other value between 0 and
!                                           ! 1 means partial success.
!                                           ! Although a value is produced
!                                           ! in such cases, one should
!                                           ! apply a more stringent QC for
!                                           ! these points.

      integer, intent(out)    :: rc         ! Error return code:
!                                           !  0   all is well
!                                           !  1   non-supported variable

! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This version is intended for mass variables.
!               As a first cut, all interoplation is linear with log(P) in the
!               vertical. See {\tt Inistu\_Mass()} for details.
!
! !REVISION HISTORY:
!
!  07Sep1999  da Silva  Initial specs.
!  20Oct1999  S.-J. Lin Initial tri-linear interpolation only
!                       More accurate interpolation based on PPM subgrid reconstruction
!                       to be implemented at a later stage
!  01nov1999  da Silva  Made prologue ProTeX compliant, revised comments, added
!                       supported variable check.
!  15nov1999  da Silva  Fixed mixr units in conversion formula.
!  01dec1999  da Silva  Introduced q2d, q3d
!  03dec1999  da Silva  Added ub, tb, qb
!  10dec1999  S.-J. Lin Extrapolation enabled
!  02oct2000  Dee       Introduced tolerances in vertical bracketing
!  05oct2000  Dee       Added support for tpw
!  14nov2000  Dee       Added support for tpws
!  31jan2001  da Silva  Changed definition of conf for height extrapolation:
!                         conf = max ( 0, 1 - (p_obs - p_s ) / 100. )
!                       so no extrapolation is performed if underground more
!                       than 100 hPa.
!
!EOP
!-------------------------------------------------------------------------


! Local
      integer i, j, k
      integer iq, nob
      real    obs_lon, o_lon, o_lat
      real    m_dlon, m_dlat
      real    alfa, beta, gama
      real    pe_0, pe_1
      real    pm_0, pm_1
      real    q_0, q_1
      real    undef
      parameter (undef = 1.e15)
      logical found

      real a11         !W-S
      real a12         !W-N
      real a21         !E-S
      real a22         !E-N
      real a00         ! temp

      integer i1, i2
      logical is2d      ! 2d quantities
      logical extp      ! extrapolation

      real tolb        ! tolerance for levels bracketing

!     Supported parameters: must be hardwired
!     ---------------------------------------
      integer, parameter :: nsupp = 19
      character(len=4), parameter :: supported(nsupp) =
     &                              (/ 'tmpu', 'sphu', 'mixr',
     &                                 'mslp', 'slpf', 'hght',
     &                                 'pb  ', 'zb  ', 'ts  ',
     &                                 'qs  ', 'ps  ', 'lwi ',
     &                                 'zs  ', 'ub  ', 'tb  ', 'qb  ',
     &                                 'tss ', 'tpw ', 'tpws' /)

      extp = .true.
      tolb = 10.*epsilon(1.)

!     Check whether requested is supported
!     ------------------------------------
      rc = 1
      do i = 1, nsupp
         if ( which .eq. trim(supported(i)) ) rc = 0
      end do
      if ( rc .ne. 0 ) return


!     No obs, nothing to do
!     ---------------------
      if ( nobs .eq. 0 ) return

      m_dlon = float(im) / 360.
      m_dlat = float(jm-1) / 180.

      is2d = .false.
      if( which .eq. 'tmpu') then
          iq = iqt
      elseif( which .eq. 'sphu' .or. which .eq. 'mixr') then
          iq = iqq
      else if( which .eq. 'mslp' .or. which .eq. 'slpf') then
          iq = iqslp
          is2d = .true.
      else if ( which .eq. 'pb' ) then
          iq = iqpb
          is2d = .true.
      else if ( which .eq. 'zb' ) then
          iq = iqzb
          is2d = .true.
      else if ( which .eq. 'ts' ) then
          iq = iqts
          is2d = .true.
      else if ( which .eq. 'qs' ) then
          iq = iqqs
          is2d = .true.
      else if ( which .eq. 'ps' ) then
          iq = iqps
          is2d = .true.
      else if ( which .eq. 'zs' ) then
          iq = iqzs
          is2d = .true.
      else if ( which .eq. 'ub' ) then
          iq = iqub
          is2d = .true.
      else if ( which .eq. 'tb' ) then
          iq = iqtb
          is2d = .true.
      else if ( which .eq. 'qb' ) then
          iq = iqqb
          is2d = .true.
      else if ( which .eq. 'tss' ) then
          iq = iqtss
          is2d = .true.
      else if ( which .eq. 'tpw' ) then
          iq = iqtpw
          is2d = .true.
      else if ( which .eq. 'tpws' ) then
          iq = iqtpws
          is2d = .true.
      else if ( which .eq. 'lwi' ) then
          iq = iqlwi
          is2d = .true.
!
!         Note: LWI is a discrete flag taking values {0,1,2} and yet
!               we interpolate it. This is because an interpolated
!               value different from {0,1,2} indicates a mix.
!

      endif

! This loop should parallelize very well
#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(nob, i, j, k, alfa, beta, gama, o_lon, o_lat)
!$omp& private(found, obs_lon, pe_0, pe_1, pm_0, pm_1, q_0, q_1)
!$omp& private(a11, a12, a21, a22, a00, i1, i2)
#endif

#if ( defined SGI )
c$doacross local(nob, i, j, k, alfa, beta, gama, o_lon, o_lat),
c$&        local(found, obs_lon, pe_0, pe_1, pm_0, pm_1, q_0, q_1),
c$&        local(a11, a12, a21, a22, a00, i1, i2)
#endif

      do nob =1, nobs

! First convert lon to [0, 360] if the coord. sys is [-180, 180]
      if( lon(nob) .lt. 0.) then
          obs_lon = lon(nob) + 360.
      else
          obs_lon = lon(nob)
      endif

! Locate the lower left corner of the grid-box that contains the obs point

      o_lon = 1. + obs_lon * m_dlon
      i   = min(im, int( o_lon ))
      alfa  = o_lon - i

      if(i .eq. im) then
         i1 = im
         i2 = 1
      else
         i1 = i
         i2 = i + 1
      endif

      o_lat = 1. + (lat(nob) + 90.) * m_dlat
      j   = min( jm-1, int( o_lat ) )
      beta  = o_lat - j

      if( is2d) then

          a11 = q2d(i1,j,iq)
          a21 = q2d(i2,j,iq)
          a12 = q2d(i1,j+1,iq)
          a22 = q2d(i2,j+1,iq)
          a00 = a11 + alfa * ( a21 - a11 )

          Iw_f(nob) = a00 + beta * ( a12 + alfa*(a22 - a12)  -  a00 )

! slpf = surface_pressure / slp
          if( which .eq. 'slpf' ) then
              a11 = pe(i1,km+1,j)
              a21 = pe(i2,km+1,j)
              a12 = pe(i1,km+1,j+1)
              a22 = pe(i2,km+1,j+1)
              a00 = a11 + alfa * ( a21 - a11 )
              a00 = a00 + beta * ( a12 + alfa*(a22 - a12)  -  a00 )
              Iw_f(nob) = a00 / Iw_f(nob)
           endif

! Compute confidence level for slp/slpf
          if ( which .eq. 'mslp' .or. which .eq. 'slpf' ) then
             a11 = q2d(i1,j,iqslp_cf)
             a21 = q2d(i2,j,iqslp_cf)
             a12 = q2d(i1,j+1,iqslp_cf)
             a22 = q2d(i2,j+1,iqslp_cf)
             conf(nob) = min( a11, a21, a12, a22 ) ! NOTE: needed a11, ... for PGI
          else
             conf(nob) = 1.0
          endif

      endif

      if( which .eq. 'hght' ) then

             found = .false.

! Check first to see if below surface
             a11 = pe(i1,km+1,j)
             a21 = pe(i2,km+1,j)
             a12 = pe(i1,km+1,j+1)
             a22 = pe(i2,km+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)

          if(lev(nob) .gt. pe_0 ) then
! SLP
             found = .true.
             if( extp ) then
                a11 = q2d(i1,j,  iqslp)
                a21 = q2d(i2,j,  iqslp)
                a12 = q2d(i1,j+1,iqslp)
                a22 = q2d(i2,j+1,iqslp)
                a00 = a11 + alfa * ( a21 - a11 )
                pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

                k = km + 1
                a11 = ze(i1,k,j)
                a21 = ze(i2,k,j)
                a12 = ze(i1,k,j+1)
                a22 = ze(i2,k,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

                if( (pe_1 - pe_0) .lt. 0.1) then
! SLP (pe_1) too close to surface pressure (pe_0); use hght at top of
! the bottom layer and the surface height for extrapolation.

! Recompute pe_0
                   pe_1 = pe_0
                   a11 = pe(i1,km,j)
                   a21 = pe(i2,km,j)
                   a12 = pe(i1,km,j+1)
                   a22 = pe(i2,km,j+1)
                   a00 = a11 + alfa * ( a21 - a11 )
                   pe_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)

! Recompute q_0
                   q_1 = q_0
                   a11 = ze(i1,km,j)
                   a21 = ze(i2,km,j)
                   a12 = ze(i1,km,j+1)
                   a22 = ze(i2,km,j+1)
                   a00 = a11 + alfa * (a21 - a11)
                   q_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)

                   gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                   Iw_f(nob) = q_0 + gama * (q_1 - q_0)

! conf = 1 - (p_obs - p_s)/100.             
                   conf(nob) = max(0.0,1.- abs(lev(nob) - pe_1) / 100.)

                else
                   gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                   Iw_f(nob) = q_0 * (1. - gama)
! conf = 1 - (p_obs - p_s)/100.             
                   conf(nob) = max(0.0,1.- abs(lev(nob) - pe_0) / 100.)
                endif
             else                           ! whether to extrapolate
! extp is false (no extrapolation allowed)
                Iw_f(nob) = undef
                conf(nob) = 0.
             endif                          ! p>ps
          endif

             k = km
          do while ( .not. found )

! Locating the levels bracketing lev(nob) with some tolerance

             a11 = pe(i1,k,j)
             a21 = pe(i2,k,j)
             a12 = pe(i1,k,j+1)
             a22 = pe(i2,k,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = pe(i1,k+1,j)
             a21 = pe(i2,k+1,j)
             a12 = pe(i1,k+1,j+1)
             a22 = pe(i2,k+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             if(pe_0 * (1. - tolb) .lt. lev(nob) .and.
     &          lev(nob) .le. pe_1 * (1. + tolb) ) then

                found = .true.
                a11 = ze(i1,k,j)
                a21 = ze(i2,k,j)
                a12 = ze(i1,k,j+1)
                a22 = ze(i2,k,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_0 = a00 + beta * ( a12 + alfa * (a22 - a12) -  a00)

                a11 = ze(i1,k+1,j)
                a21 = ze(i2,k+1,j)
                a12 = ze(i1,k+1,j+1)
                a22 = ze(i2,k+1,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_1 = a00 + beta * ( a12 + alfa * (a22 - a12) -  a00)

                gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                Iw_f(nob) = q_0 + gama * ( q_1 - q_0 )
                conf(nob) = 1.
             else
                 k = k-1
             endif

             if (k .eq. 0) then
! No valid interpolation/extrapolation can be obtained
                 Iw_f(nob) = undef
                 conf(nob) = 0.
                 found = .true.
             endif
          enddo

      elseif( which .eq. 'tmpu' .or. which .eq. 'sphu'
     &       .or. which .eq. 'mixr' ) then

             found = .false.
             k = km-1

          do while ( .not. found )

             a11 = pm(i1,k,j)
             a21 = pm(i2,k,j)
             a12 = pm(i1,k,j+1)
             a22 = pm(i2,k,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pm_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = pm(i1,k+1,j)
             a21 = pm(i2,k+1,j)
             a12 = pm(i1,k+1,j+1)
             a22 = pm(i2,k+1,j+1)
              a00 = a11 + alfa * ( a21 - a11 )
             pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

! Locating the levels bracketing lev(nob) with some tolerance

             if( lev(nob) .gt. pm_1 * (1. + tolb) ) then
                 k = 0
             elseif(pm_0 * (1. - tolb) .lt. lev(nob) .and.
     &              lev(nob) .le. pm_1 * (1. + tolb) ) then
                 found = .true.
             else
                 k = k-1
             endif
             if (k .eq. 0) found = .true.

          enddo

          if( k .eq. 0 ) then
              Iw_f(nob) = undef
              conf(nob) = 0.
          else

             a11 = q3d(i1,j,  k,iq)
             a21 = q3d(i2,j,  k,iq)
             a12 = q3d(i1,j+1,k,iq)
             a22 = q3d(i2,j+1,k,iq)
             a00 = a11 + alfa * ( a21 - a11 )
             q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = q3d(i1,j,  k+1,iq)
             a21 = q3d(i2,j,  k+1,iq)
             a12 = q3d(i1,j+1,k+1,iq)
             a22 = q3d(i2,j+1,k+1,iq)
             a00 = a11 + alfa * ( a21 - a11 )
             q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
             Iw_f(nob) = q_0 + gama * ( q_1 - q_0 )
             conf(nob) = 1.

             if( which .eq. 'mixr' ) then
! Convert sphu to mixr
                 Iw_f(nob) = Iw_f(nob) / ( 1.-Iw_f(nob)*0.001 )
             endif
          endif

      endif

      enddo

      rc = 0
      return

      end subroutine m2o_mass

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  m2o_wind --- Simulated insitu wind observations (low level)
!
! !INTERFACE:
!
      subroutine m2o_wind (q3d, pm, im, jm, km, lon, lat, lev, nobs,
     &                     which, opt, Iu_f, Iv_f, conf, rc)

! !USES:

      Implicit NONE

! !INPUT PARAMETERS:

      integer, intent(in)   :: im        ! E-W dimension
      integer, intent(in)   :: jm        ! N-S dimension
      integer, intent(in)   :: km        ! # of layers

      real, intent(in)      ::  q3d(im,jm,km,nq3d) ! packed 3d arrays
      real, intent(in)      :: pm(im,km,jm)   !Pressure at center (hPa)

      integer, intent(in)   :: nobs      ! Number of observations
      real, intent(in)      :: lon(nobs) ! longitude in degrees [-180,+180]
      real, intent(in)      :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)      :: lev(nobs) ! level in Hpa

      character(len=*), intent(in) :: which  ! Which variable to simulate,
!                                            !  case-insensitive options are:
!                                            !
!                                            ! which        description
!                                            ! -----  -----------------------
!                                            ! wind   u-wind, v-wind

      integer, intent(in)          :: opt    ! Interpolation option:
!                                            ! 0  default

! !OUTPUT PARAMETERS:
!
      real, intent(out)      :: Iu_f(nobs) ! Simulated insitu u-wind vector
      real, intent(out)      :: Iv_f(nobs) ! Simulated insitu v-wind vector
      real, intent(out)      :: conf(nobs) ! Confidence of interpolated
!                                          !  values:
!                                          !  = 1   reliable interpolation
!                                          !  = 0   could not interpolate,
!                                          !         no interpolated value
!                                          !         is set at this point
!                                          ! Any other value between 0 and
!                                          ! 1 means partial success.
!                                          ! Although a value is produced
!                                          ! in such cases, one should
!                                          ! apply a more stringent QC for
!                                          ! these points.
!

      integer, intent(out)   :: rc         ! Error return code:
!                                          !  0   all is well
!                                          !  1   non-supported variable
!                                          !


! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This version is intended for wind related variables.
!               The input lon coordinate can be either [-180, 180] or [0, 360]
!               Linear interploation in x-y and log(p) in the vertical
!               Should consider using vertical subgrid after the basic code
!               is running.
!               conf=0 if lev(nobs) is below/above the lowest/highest
!               model level; conf=1 otherwise
!               A large value undef is given if interpolation is not successful
!
! !REVISION HISTORY:
!
!  12oct1999  da Silva   Initial specs.
!  20oct1999  S.-J. Lin  Initial code.
!  01nov1999  da Silva   Made prologue ProTeX compliant, added supported obs check.
!  01dec1999  da Silva  Introduced q3d
!  02oct2000  Dee       Introduced tolerances in vertical bracketing
!
!EOP
!-------------------------------------------------------------------------

! Local
      integer i, j, k, nob
      real    obs_lon, o_lon, o_lat
      real    m_dlon, m_dlat
      real    alfa, beta, gama
      real    pm_0, pm_1
      real    q_0, q_1
      real    undef
      parameter (undef = 1.e15)
      logical found

      real a11         !W-S
      real a12         !W-N
      real a21         !E-S
      real a22         !E-N
      real a00         !temp

      integer i1, i2
      real tolb        ! tolerance for levels bracketing

!     Supported parameters: must be hardwired
!     ---------------------------------------
      integer, parameter :: nsupp = 1
      character(len=4), parameter :: supported(nsupp) = (/ 'wind' /)

      tolb = 10.*epsilon(1.)

!     Check whether requested is supported
!     ------------------------------------
      rc = 1
      do i = 1, nsupp
         if ( which .eq. supported(i) ) rc = 0
      end do
      if ( rc .ne. 0 ) return

      if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

! OpenMP directives can be easily mapped to SGI or CRAY's. Use OpenMP for now.

      m_dlon = float(im) / 360.
      m_dlat = float(jm-1) / 180.

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(nob, i, j, k, alfa, beta, gama, o_lon, o_lat)
!$omp& private(found, obs_lon, pm_0, pm_1, q_0, q_1)
!$omp& private(a11, a12, a21, a22, a00, i1, i2)
#endif

#if ( defined SGI )
c$doacross local(nob, i, j, k, alfa, beta, gama, o_lon, o_lat),
c$&        local(found, obs_lon, pm_0, pm_1, q_0, q_1),
c$&        local(a11, a12, a21, a22, a00, i1, i2)
#endif

      do nob =1, nobs

      if( lon(nob) .lt. 0.) then
! First convert lon to [0, 360] if the coord. sys is [-180, 180]
          obs_lon = lon(nob) + 360.
      else
          obs_lon = lon(nob)
      endif

      o_lon = 1. + obs_lon * m_dlon
        i   = min(im, int( o_lon ))
      alfa  = o_lon - i

      if(i .eq. im) then
         i1 = im
         i2 = 1
      else
         i1 = i
         i2 = i + 1
      endif

      o_lat = 1. + (lat(nob) + 90.) * m_dlat
        j   = min( jm-1, int( o_lat ) )
      beta  = o_lat - j


      found = .false.

! Check if the obs is very close to the center of the bottom layer

         a11 = pm(i1,km,j)
         a21 = pm(i2,km,j)
         a12 = pm(i1,km,j+1)
         a22 = pm(i2,km,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

      if( lev(nob) .gt. pm_1 ) then
          found = .true.
          if( lev(nob) - pm_1 .le. 1. ) then
! Perform extrapolation up to 1mb below center of bottom layer
                k = km-1
              a11 = pm(i1,k,j)
              a21 = pm(i2,k,j)
              a12 = pm(i1,k,j+1)
              a22 = pm(i2,k,j+1)
              a00 = a11 + alfa * ( a21 - a11 )
              pm_0 = a00 + beta * (a12 + alfa*(a22 - a12)  -  a00)
          else
               k = 0
          endif
      else
         k = km-1
      endif

      do while ( .not. found )

         a11 = pm(i1,k,j)
         a21 = pm(i2,k,j)
         a12 = pm(i1,k,j+1)
         a22 = pm(i2,k,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

         a11 = pm(i1,k+1,j)
         a21 = pm(i2,k+1,j)
         a12 = pm(i1,k+1,j+1)
         a22 = pm(i2,k+1,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

! Locating the levels bracketing lev(nob) with some tolerance

         if(pm_0 * (1. - tolb) .lt. lev(nob) .and.
     &              lev(nob) .le. pm_1 * (1. + tolb) ) then
                found = .true.
         else
                 k = k-1
         endif
             if (k .eq. 0) found = .true.

       enddo

       if( k .eq. 0 ) then
           Iu_f(nob) = undef
           Iv_f(nob) = undef
           conf(nob) = 0.
       else
           conf(nob) = 1.
! u-wind
           a11 = q3d(i1,j,  k,iqu)
           a21 = q3d(i2,j,  k,iqu)
           a12 = q3d(i1,j+1,k,iqu)
           a22 = q3d(i2,j+1,k,iqu)
           a00 = a11 + alfa * ( a21 - a11 )
           q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

           a11 = q3d(i1,j,  k+1,iqu)
           a21 = q3d(i2,j,  k+1,iqu)
           a12 = q3d(i1,j+1,k+1,iqu)
           a22 = q3d(i2,j+1,k+1,iqu)
           a00 = a11 + alfa * ( a21 - a11 )
           q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

           gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
           Iu_f(nob) = q_0 + gama * ( q_1 - q_0 )

! v-wind
           a11 = q3d(i1,j,  k,iqv)
           a21 = q3d(i2,j,  k,iqv)
           a12 = q3d(i1,j+1,k,iqv)
           a22 = q3d(i2,j+1,k,iqv)
           a00 = a11 + alfa * ( a21 - a11 )
           q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

           a11 = q3d(i1,j,  k+1,iqv)
           a21 = q3d(i2,j,  k+1,iqv)
           a12 = q3d(i1,j+1,k+1,iqv)
           a22 = q3d(i2,j+1,k+1,iqv)
           a00 = a11 + alfa * ( a21 - a11 )
           q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
           Iv_f(nob) = q_0 + gama * ( q_1 - q_0 )

        endif
      enddo

      rc = 0

      return
      end subroutine m2o_wind


      end MODULE m_insitu
