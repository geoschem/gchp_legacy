
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_interp --- Implements interpolation class for FVCCM
!
! !INTERFACE:
!
      MODULE  m_interp

! !USES:

      use  m_const, only: undef  ! undefined value

      Implicit NONE

!
! !PUBLIC TYPES:
!
      PRIVATE
      PUBLIC  int_grid          ! interpolator vector vector
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  interp_init       ! initialize simulator vector
      PUBLIC  interp_clean      ! deallocate memory use by simulator vector
      PUBLIC  interp_field      ! interpolated griddedsimulates interp mass obs
!
! !PUBLIC DATA MEMBERS:
!
      integer, public, parameter :: AAGRID = 0  ! field on A-grid
      integer, public, parameter :: UDGRID = 1  ! u-wind on D-grid
      integer, public, parameter :: VDGRID = 2  ! v-wind on D-grid
!
! !DESCRIPTION: This module contains routines interpolate fields to
!               observation locations.
! !REVISION HISTORY:
!
!  30May2001  da Silva   Initial specification and prologues based on m_insitu
!  12June2001 Baoyu Yin  Implementation based on m_insitu
!  07Feb2005  Baoyu Yin  Added edge and amiss option.
!  23Jan2007  da Silva   Relaxed conditions for near surface extrapolation:
!                        now, requested pressuer must be within 1 hPa or
!                        SURFACE pressure, not bottom layer pressure.

!
!EOP
!-------------------------------------------------------------------------

!BOC

!     Grid information
!     ----------------
      type int_grid

!        Zonal grid
!        ----------
         integer       :: im
         real          :: lon_min, lon_max, lon_del

!        Meridional grid
!         ---------------
         integer       :: jm
         real          :: lat_min, lat_max, lat_del

!        Vertical coordinates
!        --------------------
         integer        ::  km
         real           ::  ptop
         real,  pointer ::  pe(:,:,:)    ! pressure at edges (hPa)
         real,  pointer ::  pm(:,:,:)    ! pressure at center (hPa)

      end type int_grid

!EOC

      CONTAINS


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Interp_Init --- Initialized interpolator vector 
!
! !INTERFACE:
!

      subroutine Interp_Init ( im, jm, km, ptop, grid, rc,   & 
                               ps, ak, bk,                   & ! eta: optional
                               delp,                         & ! lcv: optional
                               pm, pe )                        ! user specified

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      integer, intent(in)            :: im   ! zonal dimension
      integer, intent(in)            :: jm   ! meridional dimension
      integer, intent(in)            :: km   ! vertical dimension
      real,    intent(in), OPTIONAL  :: ptop ! top pressure (Pa)
      
                                ! OPTIONAL vertical grid specification (eta):

      real,    intent(in), OPTIONAL  :: ps(im,jm) ! sfc pressure (Pa)
      real,    intent(in), OPTIONAL  :: ak(km+1)  ! vertical grid a coefficient
      real,    intent(in), OPTIONAL  :: bk(km+1)  ! vertical grid b coefficient

                                ! OPTIONAL vertical grid specification (lcv):

      real,    intent(in), OPTIONAL  :: delp(im,jm,km) ! pressure thickness (Pa)

                                ! OPTIONAL: either pm or (pm,pe)
      real,    intent(in), OPTIONAL  :: pm(im,jm,km)   ! mid layer z coord
      real,    intent(in), OPTIONAL  :: pe(im,jm,km+1)   ! edge z coord
                                                       ! (Use chooses unit)

! !OUTPUT PARAMETERS:
!

      type(int_grid), intent(out)  :: grid      ! grid specification

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   ...
                                                !

! !DESCRIPTION: This routine allocates memory and initializes the interpolator
!               {\tt grid} with parameters needed for the interpolation
!                calculations. On input, one must specify either {\tt delp}
!               or all of the {\tt (ps,ak,bk)} eta grid specification.
!
! !REVISION HISTORY:
!
!  30May01  da Silva  Initial specification and prologues based on m_insitu
!
!EOP
!-------------------------------------------------------------------------

     character(len=*), parameter :: myname = 'Interp_Init'
     integer    err
     integer    i, j, k
     real p2d(im,km+1)
!     logical :: lcv = .false., eta = .false.
     logical :: lcv, eta

     lcv = .false.
     eta = .false.

!    Sanity check
!    ------------
     rc = 0
     if ( im<1 .or. jm<1 ) then
          rc = 3
          return
     endif

!    Initialize dimensional attributes
!    ---------------------------------
     grid%im = im
     grid%jm = jm
     grid%km = km
     grid%ptop = ptop

     grid%lon_del = 360./im
     grid%lon_min = 0.
     grid%lon_max = grid%lon_min + (im-1) * grid%lon_del

     grid%lat_del = 180./(jm-1)
     grid%lat_min = -90.
     grid%lat_max = 90.

     allocate(grid%pe(im, km+1, jm), stat = err); if (err.ne. 0) rc = 2
     allocate(grid%pm(im, km, jm), stat = err); if (err.ne. 0) rc = 2

!    Eta coordinates
!    ---------------
     if ( present(ps) .and. present(ak) .and. present(bk) .and. km > 0 ) then

        eta = .true.

        do k = 1, km + 1
           do j = 1, jm
              do i = 1, im 
                 grid%pe(i,k,j) = (ak(k) + bk(k) * ps(i,j))
              enddo
           enddo
        enddo

!    Lagrangian control volume (lcv)
!    -------------------------------
     else if ( present(delp) ) then

      lcv = .true.

      do 1000 j=1, jm

        do i=1,im
          p2d(i,1) = ptop
        enddo

! Top down
        do k=2,km+1
          do i=1,im
            p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
          enddo
        enddo

        do k=1,km+1
            do i=1,im
              grid%pe(i,k,j) = p2d(i,k)
            enddo
        enddo

1000  continue


!    User is the boss
!    ----------------
     else if ( present(pe) .and. present(pm) ) then

        do j = 1, jm
           do i = 1, im
              do k = 1, km
                 grid%pe(i,k,j) = pe(i,j,k)
                 grid%pm(i,k,j) = pm(i,j,k)
              end do
              grid%pe(i,km+1,j) = pe(i,j,km+1) ! weird indexing
           end do
        end do

!    Only mid-layer interp
!    ---------------------
     else if ( present(pm) ) then

        grid%pe = undef ! not needed
        do j = 1, jm
           do i = 1, im
              do k = 1, km
                 grid%pm(i,k,j) = pm(i,j,k) ! weird indexing
              end do
           end do
        end do

!    Only edge-layer interp
     else if ( present(pe) ) then

        do j = 1, jm
           do i = 1, im
              do k = 1, km
                 grid%pe(i,k,j) = pe(i,j,k)
              end do
              grid%pe(i,km+1,j) = pe(i,j,km+1) ! weird indexing
           end do
        end do


!    No vertical coord (2D interp only)
!    ----------------------------------
     else 

          grid%pe = undef  ! not neeed
          grid%pm = undef  ! not neeed

          return

     endif    

!    In case of eta or lcv, convert units to hPa, and create pm from pe
!    ------------------------------------------------------------------
     if ( eta .or. lcv ) then

        do k=1,km+1
           do j =1,jm
              do i=1,im
                 grid%pe(i,k,j) = 0.01*grid%pe(i,k,j)
              enddo
           enddo
        enddo
        
        do k=1,km
           do j =1,jm
              do i=1,im
                 grid%pm(i,k,j) = 0.5*(grid%pe(i,k,j) + grid%pe(i,k+1,j))
              enddo
           enddo
        enddo

     end if


  end subroutine interp_init


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Interp_Clean --- Free memory occupied by simulator vector
!
! !INTERFACE:
!
      subroutine Interp_Clean ( grid )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(int_grid), intent(inout)   :: grid       ! Simulator vector


! !DESCRIPTION: This routine deallocates memory occupied by the interpolator
!               {\tt grid}.
!
! !REVISION HISTORY:
!
!  30May01  da Silva  Initial specification and prologues based on m_insitu
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Interp_Clean'

      if ( associated(grid%pe)  )      deallocate ( grid%pe   )
      if ( associated(grid%pm)  )      deallocate ( grid%pm   )

      end subroutine interp_clean


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Interp_Field --- Interpolates field to observation locations
!
! !INTERFACE:
!
      subroutine Interp_Field ( grid, lon, lat, lev, nobs,  &
                                im, jm, km, field, ftype,   &
                                Iw, conf, rc, co_field,     &
                                Iw_co, wz, slp, opt, edges, amiss)

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(int_grid), intent(in) :: grid      ! Grid specification
      integer, intent(in)        :: nobs      ! Number of observations
      real, intent(in)           :: lon(nobs) ! longitude in degrees [-180,+180]
      real, intent(in)           :: lat(nobs) ! latitude  in degrees [-90,90]
      real, intent(in)           :: lev(nobs) ! level in Hpa

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension
      integer, intent(in)        :: km        ! vertical dimension: 
                                              ! = 1 for 2D fields
                                              ! = km for mid-layer fields
                                              ! = km+1 for edge fields

      real, intent(in)           :: field(im,jm,km) ! field

      integer, intent(in)        :: ftype     ! Field type:
                                              !  0 - Field is on   A-grid
                                              !  1 - Field is u on D-grid
                                              !  2 - Field is v on D-grid
                                            
! !OUTPUT PARAMETERS:
!
      real, intent(out)           :: Iw(nobs)   ! Interpolated values
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
      real,  intent(in), OPTIONAL  :: co_field(im,jm,km)  ! for wind related interpolation
      real, intent(out), OPTIONAL :: Iw_co(nobs) ! Interpolated values for V wind 
      real,  intent(in), OPTIONAL  :: wz(im,km+1,jm)  ! for wind related interpolation
      real,  intent(in), OPTIONAL  :: slp(im,jm) ! for hght related interpolation
      integer, intent(in), OPTIONAL  :: opt      ! Vertical interp option:
                                                 ! 0 - linear in log-p
                                                 ! 1 - linear in p
      logical, intent(in), OPTIONAL  :: edges    ! a variable is on edges or middle layers
      real, intent(in), OPTIONAL :: amiss        ! Fill value


! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. All interoplation is linear with log(P) in the
!               vertical. Be sure to check the confidence values for each
!               of the interpolated values: {\tt conf=0} if {\tt lev(nobs)} 
!               is below/above the lowest/highest model level; $conf=1$ otherwise.
!               A large value undef is given if interpolation is not successful.
!
!               The input lon coordinate can be either [-180, 180] or [0, 360]
!
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  30May2001  da Silva  Initial specification and prologues based on m_insitu
!  07Feb2005  Baoyu Yin Added edge and amiss option.
!  28Dec2005  Todling   Fixed under vs amiss by adding myundef
!
!EOP
!-------------------------------------------------------------------------
     character(len=*), parameter :: myname = 'Interp_Field'


! Local
      integer i, j, k, nob
      real    obs_lon, o_lon, o_lat
      real    m_dlon, m_dlat
      real    alfa, beta, gama
      real    pm_0, pm_1, pmm_1
      real    pe_0, pe_1
      real    q_0, q_1
      logical found
      logical onEdges

      real a11         !W-S
      real a12         !W-N
      real a21         !E-S
      real a22         !E-N
      real a00         !temp

      integer i1, i2
      logical is2d      ! 2d quantities
      logical extp      ! extrapolation
      real tolb        ! tolerance for levels bracketing

      real ua(im,jm)
      real va(im,jm)
      real   sinlon(im)
      real   coslon(im)
      real dl
      real tmp(im,jm,km)
      real tmp1(im,jm,km)
      real fill1, fill2, myundef  

      integer iopt

      iopt = 0 ! log-p vertical interpolation by default
      onEdges = .false.
      myundef = undef
      if ( present( amiss ) ) myundef = amiss
      if (present(edges)) onEdges = edges
      if ( present(opt) ) iopt = opt

      tolb = 10.*epsilon(1.)

! To interpolate U or V wind
  if ( ftype .eq. 1 .or. ftype .eq. 2) then

!     Define logitude at the center of the finite-volume

         dl = 8.*atan(1.0) / float(im)
      do i=1,im/2
         coslon(i)      = -cos((i-1)*dl)
         coslon(i+im/2) = -coslon(i)
         sinlon(i)      = -sin((i-1)*dl)
         sinlon(i+im/2) = -sinlon(i)
      enddo

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k, ua, va)
#endif

#if ( defined SGI )
!$doacross   local(i,j,k, ua, va)
#endif

      do k=1,km
!     map D-grid wind to A-grid
             call d2a(field(1,1,k),co_field(1,1,k), ua ,va,  &
                      im, jm, 1, jm, coslon, sinlon)

            do j=1,jm
               do i=1,im
                  tmp(i,j,k) = ua(i,j)
                  tmp1(i,j,k) = va(i,j)
               enddo
            enddo
      enddo

      rc = 1

      if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

      m_dlon = float(im) / 360.
      m_dlat = float(jm-1) / 180.

#if ( defined OpenMP )
!$omp  parallel do  
!$omp& default(shared) 
!$omp& private(nob, i, j, k, alfa, beta, gama, o_lon, o_lat)  
!$omp& private(found, obs_lon, pm_0, pm_1, pmm_1, pe_1, q_0, q_1) 
!$omp& private(a11, a12, a21, a22, a00, i1, i2) 
#endif

#if ( defined SGI )
!$doacross local(nob, i, j, k, alfa, beta, gama, o_lon, o_lat), 
!$&          local(found, obs_lon, pm_0, pm_1, pmm_1, pe_1, q_0, q_1), 
!$&          local(a11, a12, a21, a22, a00, i1, i2)
#endif

      do nob =1, nobs

      if( lon(nob) .lt. 0.) then
!     First convert lon to [0, 360] if the coord. sys is [-180, 180]
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

!        Check if the obs is very close to the SURFACE

         a11 = grid%pm(i1,km,j)
         a21 = grid%pm(i2,km,j)
         a12 = grid%pm(i1,km,j+1)
         a22 = grid%pm(i2,km,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

         a11 = grid%pe(i1,km+1,j)
         a21 = grid%pe(i2,km+1,j)
         a12 = grid%pe(i1,km+1,j+1)
         a22 = grid%pe(i2,km+1,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

      if( lev(nob) .gt. pm_1 ) then
          found = .true.
          if( lev(nob) .le. pe_1*(1.+tolb) ) then
!             Perform extrapolation up to 1mb below SURFACE
              k = km-1
              a11 = grid%pm(i1,k,j)
              a21 = grid%pm(i2,k,j)
              a12 = grid%pm(i1,k,j+1)
              a22 = grid%pm(i2,k,j+1)
              a00 = a11 + alfa * ( a21 - a11 )
              pm_0 = a00 + beta * (a12 + alfa*(a22 - a12)  -  a00)
          else
               k = 0
          endif
      else
         k = km-1
      endif

      do while ( .not. found )

         a11 = grid%pm(i1,k,j)
         a21 = grid%pm(i2,k,j)
         a12 = grid%pm(i1,k,j+1)
         a22 = grid%pm(i2,k,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

         a11 = grid%pm(i1,k+1,j)
         a21 = grid%pm(i2,k+1,j)
         a12 = grid%pm(i1,k+1,j+1)
         a22 = grid%pm(i2,k+1,j+1)
          a00 = a11 + alfa * ( a21 - a11 )
         pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

!        Locating the levels bracketing lev(nob) with some tolerance

         if(pm_0 * (1. - tolb) .lt. lev(nob) .and.   &
                    lev(nob) .le. pm_1 * (1. + tolb) ) then
                found = .true.
         else
                 k = k-1
         endif
             if (k .eq. 0) found = .true.

       enddo

       if( k .eq. 0 ) then
           Iw(nob) = myundef
           Iw_co(nob) = myundef
           conf(nob) = 0.
       else
           conf(nob) = 1.
!          u-wind
              a11 = tmp(i1,j,  k)
              a21 = tmp(i2,j,  k)
              a12 = tmp(i1,j+1,k)
              a22 = tmp(i2,j+1,k)
              a00 = a11 + alfa * ( a21 - a11 )
              q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

              a11 = tmp(i1,j,  k+1)
              a21 = tmp(i2,j,  k+1)
              a12 = tmp(i1,j+1,k+1)
              a22 = tmp(i2,j+1,k+1)
              a00 = a11 + alfa * ( a21 - a11 )
              q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
              if ( iopt == 0 ) then
                 gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
              else
                 gama = (   (lev(nob))-   (pm_0))/(   (pm_1)-   (pm_0))
              end if
              Iw(nob) = q_0 + gama * ( q_1 - q_0 )
!          v-wind
              a11 = tmp1(i1,j,  k)
              a21 = tmp1(i2,j,  k)
              a12 = tmp1(i1,j+1,k)
              a22 = tmp1(i2,j+1,k)
              a00 = a11 + alfa * ( a21 - a11 )
              q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

              a11 = tmp1(i1,j,  k+1)
              a21 = tmp1(i2,j,  k+1)
              a12 = tmp1(i1,j+1,k+1)
              a22 = tmp1(i2,j+1,k+1)
              a00 = a11 + alfa * ( a21 - a11 )
              q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
              if ( iopt == 0 ) then
                 gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
              else
                 gama = (   (lev(nob))-   (pm_0))/(   (pm_1)-   (pm_0))
              end if
              Iw_co(nob) = q_0 + gama * ( q_1 - q_0 )

        endif
      
        if ( km .eq. 1 ) then
           conf(nob) = 1.
           !          u-wind
              a11 = tmp(i1,j,1)
              a21 = tmp(i2,j,1)
              a12 = tmp(i1,j+1,1)
              a22 = tmp(i2,j+1,1)
              a00 = a11 + alfa * ( a21 - a11 )
              Iw(nob) = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
           !          v-wind
              a11 = tmp1(i1,j,  1)
              a21 = tmp1(i2,j,  1)
              a12 = tmp1(i1,j+1,1)
              a22 = tmp1(i2,j+1,1)
              a00 = a11 + alfa * ( a21 - a11 )
              Iw_co(nob) = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
         end if
      enddo

      rc = 0

      return
  endif

!     Interpolation for 2D variables
      extp = .true.
      tolb = 10.*epsilon(1.)

!     ------------------------------------
      rc = 1

!     No obs, nothing to do
!     ---------------------
      if ( nobs .eq. 0 ) return

      m_dlon = float(im) / 360.
      m_dlat = float(jm-1) / 180.
      
      is2d = .false.
      if ( km .eq. 1 ) is2d = .true.

#if ( defined OpenMP )
!$omp  parallel do 
!$omp& default(shared) 
!$omp& private(nob, i, j, k, alfa, beta, gama, o_lon, o_lat) 
!$omp& private(found, obs_lon, pe_0, pe_1, pm_0, pm_1, pe_1, q_0, q_1) 
!$omp& private(a11, a12, a21, a22, a00, i1, i2) 
#endif

#if ( defined SGI )
!$doacross local(nob, i, j, k, alfa, beta, gama, o_lon, o_lat), 
!$&        local(found, obs_lon, pe_0, pe_1, pm_0, pm_1, q_0, q_1), 
!$&        local(a11, a12, a21, a22, a00, i1, i2)
#endif

  do nob =1, nobs

!     First convert lon to [0, 360] if the coord. sys is [-180, 180]
      if( lon(nob) .lt. 0.) then
          obs_lon = lon(nob) + 360.
      else
          obs_lon = lon(nob)
      endif

!     Locate the lower left corner of the grid-box that contains the obs point

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
          a11 = field(i1,j,1)
          a21 = field(i2,j,1)
          a12 = field(i1,j+1,1)
          a22 = field(i2,j+1,1)
          a00 = a11 + alfa * ( a21 - a11 )

          Iw(nob) = a00 + beta * ( a12 + alfa*(a22 - a12)  -  a00 )
          if ( abs(a11-myundef) < 0.01 .or. abs(a21-myundef) < 0.01 .or. &
               abs(a12-myundef) < 0.01 .or. abs(a22-myundef) < 0.01 ) Iw(nob) = myundef 

          conf(nob) = 1.0
          if ( abs(field(i1,j,1)-myundef) .lt. 0.01 .and. nobs .eq. im*jm .and. abs(Iw(nob)-myundef) .lt. 0.01) then
             Iw(nob) = field(i1,j,1)
	  end if
      endif

! -----------------------------------------------------------
! HGHT 

      if( ftype .eq. 3 ) then

             found = .false.

! Check first to see if below surface
             a11 = grid%pe(i1,km+1,j)
             a21 = grid%pe(i2,km+1,j)
             a12 = grid%pe(i1,km+1,j+1)
             a22 = grid%pe(i2,km+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)

          if(lev(nob) .gt. pe_0 ) then
! SLP
             found = .true.
             if( extp ) then

                a11 = slp(i1,j)
                a21 = slp(i2,j)
                a12 = slp(i1,j+1)
                a22 = slp(i2,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

                k = km + 1
                a11 = wz(i1,k,j)
                a21 = wz(i2,k,j)
                a12 = wz(i1,k,j+1)
                a22 = wz(i2,k,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

                if( (pe_1 - pe_0) .lt. 0.1) then
! SLP (pe_1) too close to surface pressure (pe_0); use hght at top of
! the bottom layer and the surface height for extrapolation.

! Recompute pe_0
                   pe_1 = pe_0
                   a11 = grid%pe(i1,km,j)
                   a21 = grid%pe(i2,km,j)
                   a12 = grid%pe(i1,km,j+1)
                   a22 = grid%pe(i2,km,j+1)
                   a00 = a11 + alfa * ( a21 - a11 )
                   pe_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)

! Recompute q_0
                   q_1 = q_0
                   a11 = wz(i1,km,j)
                   a21 = wz(i2,km,j)
                   a12 = wz(i1,km,j+1)
                   a22 = wz(i2,km,j+1)
                   a00 = a11 + alfa * (a21 - a11)
                   q_0 = a00 + beta * (a12 + alfa * (a22 - a12) - a00)
                   if ( iopt == 0 ) then
                      gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                   else
                      gama = (   (lev(nob))-   (pe_0))/(   (pe_1)-   (pe_0))
                   end if
                   Iw(nob) = q_0 + gama * (q_1 - q_0)

! conf = 1 - (p_obs - p_s)/100.             
                   conf(nob) = max(0.0,1.- abs(lev(nob) - pe_1) / 100.)

                else
                   if ( iopt == 0 ) then
                      gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                   else
                      gama = (   (lev(nob))-   (pe_0))/(   (pe_1)-   (pe_0))
                   end if
                   Iw(nob) = q_0 * (1. - gama)
! conf = 1 - (p_obs - p_s)/100.             
                   conf(nob) = max(0.0,1.- abs(lev(nob) - pe_0) / 100.)
                endif
             else                           ! whether to extrapolate
! extp is false (no extrapolation allowed)
                Iw(nob) = myundef
                conf(nob) = 0.
             endif                          ! p>ps
          endif

             k = km
          do while ( .not. found )

! Locating the levels bracketing lev(nob) with some tolerance

             a11 = grid%pe(i1,k,j)
             a21 = grid%pe(i2,k,j)
             a12 = grid%pe(i1,k,j+1)
             a22 = grid%pe(i2,k,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = grid%pe(i1,k+1,j)
             a21 = grid%pe(i2,k+1,j)
             a12 = grid%pe(i1,k+1,j+1)
             a22 = grid%pe(i2,k+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             if(pe_0 * (1. - tolb) .lt. lev(nob) .and.    &
               lev(nob) .le. pe_1 * (1. + tolb) ) then

                found = .true.
                a11 = wz(i1,k,j)
                a21 = wz(i2,k,j)
                a12 = wz(i1,k,j+1)
                a22 = wz(i2,k,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_0 = a00 + beta * ( a12 + alfa * (a22 - a12) -  a00)

                a11 = wz(i1,k+1,j)
                a21 = wz(i2,k+1,j)
                a12 = wz(i1,k+1,j+1)
                a22 = wz(i2,k+1,j+1)
                a00 = a11 + alfa * ( a21 - a11 )
                q_1 = a00 + beta * ( a12 + alfa * (a22 - a12) -  a00)
                if ( iopt == 0 ) then
                   gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
                else
                   gama = (   (lev(nob))-   (pe_0))/(   (pe_1)-   (pe_0))
                end if
                Iw(nob) = q_0 + gama * ( q_1 - q_0 )
                conf(nob) = 1.
             else
                 k = k-1
             endif

             if (k .eq. 0) then
! No valid interpolation/extrapolation can be obtained
                 Iw(nob) = myundef
                 conf(nob) = 0.
                 found = .true.
             endif
          enddo
      elseif ( .not. onEdges ) then   
! END of hght
! ----------------------------------------------------------
! Interpolation for 3D variables
      if( km .gt. 1) then

!          Use surface pressure as limit for vertical interp/exterp
           a11 = grid%pe(i1,km+1,j)
           a21 = grid%pe(i2,km+1,j)
           a12 = grid%pe(i1,km+1,j+1)
           a22 = grid%pe(i2,km+1,j+1)
           a00 = a11 + alfa * ( a21 - a11 )
          pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             found = .false.
             k = km-1

          do while ( .not. found )

             a11 = grid%pm(i1,k,j)
             a21 = grid%pm(i2,k,j)
             a12 = grid%pm(i1,k,j+1)
             a22 = grid%pm(i2,k,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pm_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = grid%pm(i1,k+1,j)
             a21 = grid%pm(i2,k+1,j)
             a12 = grid%pm(i1,k+1,j+1)
             a22 = grid%pm(i2,k+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pm_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

! Allow for extrapolation at lower layer

             if ( k .eq. km-1 ) then
                  pmm_1 = pe_1
             else
                  pmm_1 = pm_1
             endif

! Locating the levels bracketing lev(nob) with some tolerance

             if( lev(nob) .gt. pmm_1 * (1. + tolb) ) then
                 k = 0
             elseif(pm_0 * (1. - tolb) .lt. lev(nob) .and.   &
                    lev(nob) .le. pmm_1 * (1. + tolb) ) then
                 found = .true.
             else
                 k = k-1
             endif
             if (k .eq. 0) found = .true.

          enddo

          if( k .eq. 0 ) then
              Iw(nob) = myundef
              conf(nob) = 0.
          else

             a11 = field(i1,j,  k)
             a21 = field(i2,j,  k)
             a12 = field(i1,j+1,k)
             a22 = field(i2,j+1,k)
             a00 = a11 + alfa * ( a21 - a11 )
             q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             Iw(nob) = 0
             fill1 = 0
             fill2 = 0
             if (abs(a11-myundef) .lt. 0.01 .or. abs(a21-myundef) .lt. 0.01 .or. &
                 abs(a12-myundef) .lt. 0.01 .or. abs(a22-myundef) .lt. 0.01) then
                fill1 = a11  
		Iw(nob) = myundef
             endif

             a11 = field(i1,j,  k+1)
             a21 = field(i2,j,  k+1)
             a12 = field(i1,j+1,k+1)
             a22 = field(i2,j+1,k+1)
             a00 = a11 + alfa * ( a21 - a11 )
             q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             if (abs(a11-myundef) .lt. 0.01 .or. abs(a21-myundef) .lt. 0.01 .or. &
                 abs(a12-myundef) .lt. 0.01 .or. abs(a22-myundef) .lt. 0.01) then
         	fill2 = a11  
		Iw(nob) = myundef
             endif

             if ( iopt == 0 ) then
                gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
             else
                gama = (   (lev(nob))-   (pm_0))/(   (pm_1)-   (pm_0))
             end if

             if ( abs(Iw(nob)-myundef) .lt. 0.01 ) then
                if ( abs(fill1-myundef) .lt. 0.01 .or. abs(fill2-myundef) .lt. 0.01 ) then
                   Iw(nob) = myundef
		else
                   if ( grid%im*grid%jm .ne. im*jm ) then
                      Iw(nob) = myundef 
                   else
 		      Iw(nob) = fill1  + gama * ( fill2 - fill1 )
                   end if
	        end if
             else
                Iw(nob) = q_0 + gama * ( q_1 - q_0 )
             end if

             if ( Iw(nob) .eq. myundef ) then
                  conf = 0.
             else
                conf(nob) = 1.
             end if

          endif

      endif

   else ! onEdges is .true.
! Interpolation for 3D variables
      if( km .gt. 1) then

             found = .false.
             k = km-1

          do while ( .not. found )

             a11 = grid%pe(i1,k,j)
             a21 = grid%pe(i2,k,j)
             a12 = grid%pe(i1,k,j+1)
             a22 = grid%pe(i2,k,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             a11 = grid%pe(i1,k+1,j)
             a21 = grid%pe(i2,k+1,j)
             a12 = grid%pe(i1,k+1,j+1)
             a22 = grid%pe(i2,k+1,j+1)
             a00 = a11 + alfa * ( a21 - a11 )
             pe_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

! Locating the levels bracketing lev(nob) with some tolerance

             if( lev(nob) .gt. pe_1 * (1. + tolb) ) then
                 k = 0
             elseif(pe_0 * (1. - tolb) .lt. lev(nob) .and.   &
                    lev(nob) .le. pe_1 * (1. + tolb) ) then
                 found = .true.
             else
                 k = k-1
             endif
             if (k .eq. 0) found = .true.

          enddo

          if( k .eq. 0 ) then
              Iw(nob) = myundef
              conf(nob) = 0.
          else

             a11 = field(i1,j,  k)
             a21 = field(i2,j,  k)
             a12 = field(i1,j+1,k)
             a22 = field(i2,j+1,k)
             a00 = a11 + alfa * ( a21 - a11 )
             q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             Iw(nob) = 0
             fill1 = 0
             fill2 = 0
             if (abs(a11-myundef) .lt. 0.01 .or. abs(a21-myundef) .lt. 0.01 .or. &
                 abs(a12-myundef) .lt. 0.01 .or. abs(a22-myundef) .lt. 0.01) then
                fill1 = a11  
		Iw(nob) = myundef
             endif

             a11 = field(i1,j,  k+1)
             a21 = field(i2,j,  k+1)
             a12 = field(i1,j+1,k+1)
             a22 = field(i2,j+1,k+1)
             a00 = a11 + alfa * ( a21 - a11 )
             q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             if (abs(a11-myundef) .lt. 0.01 .or. abs(a21-myundef) .lt. 0.01 .or. &
                 abs(a12-myundef) .lt. 0.01 .or. abs(a22-myundef) .lt. 0.01) then
         	fill2 = a11  
		Iw(nob) = myundef
             endif

             if ( iopt == 0 ) then
                gama = (log(lev(nob))-log(pe_0))/(log(pe_1)-log(pe_0))
             else
                gama = (   (lev(nob))-   (pe_0))/(   (pe_1)-   (pe_0))
             end if

             if ( abs(Iw(nob)-myundef) .lt. 0.01 ) then
                if ( abs(fill1-myundef) .lt. 0.01 .or. abs(fill2-myundef) .lt. 0.01 ) then
                   Iw(nob) = myundef
		else
                   if ( grid%im*grid%jm .ne. im*jm ) then
                      Iw(nob) = myundef 
                   else
 		      Iw(nob) = fill1  + gama * ( fill2 - fill1 )
                   end if
	        end if
             else
                Iw(nob) = q_0 + gama * ( q_1 - q_0 )
             end if

             if ( Iw(nob) .eq. myundef ) then
                  conf = 0.
             else
                conf(nob) = 1.
             end if

          endif

      endif

    end if
  enddo

      rc = 0
      return

  

     end subroutine Interp_Field

!.................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      write(*,'(a)') trim(myname) // ': ' // trim(msg)
      call exit(1)
      end subroutine die

!.................................................................


! .........................................................................

      subroutine d2a(u,v,ua,va,im,jm,jfirst,jlast,coslon,sinlon)
! This is primarily for turbulence package designed for A-grid.
! Also used for output to A-grid.
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

! Convert D-grid winds to A-grid
! u --> ua, v --> va

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
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) - &
                      (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) -      &
                      (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) -    &
                      (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) -  &
                      (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) - &
                      (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
! Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i)  &
             + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i)  &
             + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
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
! Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i)  &
             + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i)  &
             + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
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

      end MODULE m_interp                        

