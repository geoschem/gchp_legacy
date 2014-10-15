!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: m_dynp --- FVGCM dynamics state perturbation class
!
! !INTERFACE:
!
   Module m_dynp

!USES:

   use  m_dyn            ! dynamics state class
   use  m_chars, only : uppercase

   Implicit NONE
!
! !PUBLIC MEMBER FUNCTIONS:
!
   Public dynp_init      ! initializes an perturbation vector
   Public dynp_clean     ! deallocates memory
   Public dynp_remap     ! remap to reference vertical coordinate
   Public dynp_add       ! adds a perturbation to a state vector
   Public dynp_shave     ! shaving method
   Public dynp_add_surf  ! adds delp from pert state
   Public dynp_scale     ! scale perturbation

!
! !DESCRIPTION: This module defines methods for dealing with dynamics
!               state perturbations.
!
!  A state perturbation is defined by perturbations on the 3D fields
!  u, v, pt, q, and on the 2D fields ts, phis. The vertical coordinate
!  for the 3D fields is defined by delp, as in a dynamics state.
!
!  For now, we use the same data structure for a state perturbation
!  as for a dynamics state, as defined in m_dyn. We can therefore also
!  use dyn_put/get for I/O.
!
! !REVISION HISTORY:
!
!  07nov2002  Dee       Initial code.
!  08apr2004  Todling   Added add_surf routine to handle real perturbations
!  12Dec2004  Todling   Allow adding two perturbations fields
!
!EOP
!-------------------------------------------------------------------------

!BOC

   real, parameter :: missing_val = 1.0E+15 ! hardwire this for now

   real, parameter :: reltol = 1.0e-6 ! relative tolerance for identical layers

   logical, save :: skipSHAVE = .false.     ! the default is to apply shaving
   logical, save :: gcmPERT   = .false.     ! delp is perturbation; ps   is derived
   logical, save :: anaPERT   = .false.     ! ps   is perturbation; delp is derived
   logical, save :: allPERT   = .false.     ! both fields are perturbations

!EOC

!---------------------------------------------------------------------------

CONTAINS
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_init --- Initializes a dynamics perturbation vector
!
! !INTERFACE:
!
   subroutine  dynp_init ( w, dw, rc )
!
! !USES:
!
   implicit NONE
!
! !INPUT PARAMETERS:
!
   type(dyn_vect), intent(in)    :: w      ! reference state
!
! !OUTPUT PARAMETERS:
!
   type(dyn_vect), intent(out)   :: dw     ! state perturbation
   integer, intent(out)          :: rc     ! error return code:
                                           !  0 - all is well
                                           !  1 - already allocated
                                           !  2 - not enough memory
                                           !  3 - invalid dimensions
!
! !DESCRIPTION: This routine initializes the dynamic state perturbation
!               vector {\tt dw}, based on the reference dynamic state
!  vector {\tt w}. For now, the data structure (type(dyn_vect)) used
!  for a perturbation is identical to that used for a dynamics state.
!
!  Perturbation fields are u, v, pt, q (3D), and Ts, ps (2D).
!  Coordinates are inherited from the reference state. In particular,
!  the vertical coordinate is pressure as defined by the reference ptop
!  and delp.
!
!  NOTE: ps is actually redundant, since ps = ptop + sum(delp).
!        We carry it anyway, for convenience.
!
! !REVISION HISTORY:
!
!  07nov2002 Dee      Initial code.
!  05mar2009 Todling  Add land/water/ice fractions
!
!EOP
!-------------------------------------------------------------------------

!  Allocate memory, and copy grid and metadata from the reference state
!  --------------------------------------------------------------------
   call Dyn_Init ( w, dw, rc )

!  Copy the vertical coordinate and implied surface pressure
!  ---------------------------------------------------------
   dw%delp = w%delp
   dw%ps   = w%ps

!  Initialize the perturbation fields
!  ----------------------------------
   dw%Ts   = 0.0
   dw%u    = 0.0
   dw%v    = 0.0
   dw%pt   = 0.0
   dw%phis = 0.0
   dw%q    = 0.0

!  Set meaningless fields to UNDEF
!  -------------------------------
   dw%hs_stdv   = missing_val
   dw%lwi       = missing_val
   dw%frland    = missing_val
   dw%frlandice = missing_val
   dw%frlake    = missing_val
   dw%frocean   = missing_val
   dw%frseaice  = missing_val


   end subroutine dynp_init

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_clean --- Deallocates memory used by state perturbation
!
! !INTERFACE:
!
   subroutine  dynp_clean ( dw )
!
! !USES:
!
   implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
   type(dyn_vect), intent (out) :: dw   ! dynamics state vector

! !DESCRIPTION:
!
!  Deallocates memory used by dynamics state perturbation vector.
!
! !REVISION HISTORY:
!
!  07nov2002 Dee       Initial code.
!
!EOP
!-------------------------------------------------------------------------

   call dyn_clean ( dw )

   end subroutine dynp_clean


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_remap --- remap a state perturbation
!
! !INTERFACE:
!
   subroutine  dynp_remap ( dw, w, rc, &
                                       verbose )   ! optional
!
! !USES:
!
   use  m_const

   implicit NONE

!
! !INPUT/OUTPUT PARAMETERS:
!
   type(dyn_vect), intent(inout) :: dw        ! state increment vector to
                                              !    be remapped
!
! !INPUT PARAMETERS:
!
   type(dyn_vect), intent(in)    :: w         ! dynamics state vector
   logical, intent(in), OPTIONAL :: verbose   ! if .true. then print stuff
!
! !OUTPUT PARAMETERS:
!
   integer, intent(out)          :: rc        ! error return code:
                                              !  0 - all is well
                                              !  11 - different grid dimensions
                                              !  12 - multiple tracers
                                              !  13 - zero-depth layers
                                              !  14 - no identical top layers
                                              !  other >0 - allocate error
!
! !DESCRIPTION: This routine changes the vertical coordinate field of
!               a state perturbation dw to that of the state vector w,
!  by remapping the 3D fields in dw.  If the vertical coordinates of
!  dw and w are identical on input, then nothing is done.
!
!  Requirements are:
!    - dw and w must have identical horizontal grids
!    - dw and w must have at least one identical top layer
!    - thickness of all layers must be positive everywhere
!
!  The algorithm is based on linear interpolation in log(p), with constant
!  extrapolation to the lowest layers if needed.
!
!
! !REVISION HISTORY:
!
!  26jun2001 Dee      Initial code
!  14Jul2003 Todling  Added lm=2 functionallity.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*),  parameter :: myname = 'dynp_remap'

   real, allocatable ::  z1(:), z2(:), du(:), dv(:), dpt(:), dq(:), do3(:)

   integer im, jm, km, lm
   integer i, j, k, ktop, n
   real    tol, ptop, pt, pb, z, za, zb, alpha
   logical verb


   if ( present ( verbose ) ) then
        verb = verbose
   else
        verb = .false.
   end if

!  do some checking
!  ----------------
   im = w%grid%im
   jm = w%grid%jm
   km = w%grid%km
   lm = w%grid%lm

   if ( im .ne. dw%grid%im .or. jm .ne. dw%grid%jm .or. &
        km .ne. dw%grid%km .or. lm .ne. dw%grid%lm ) then
        rc = 11
        return                ! can't handle different grid dimensions yet
   end if
   if ( lm .lt. 1 .or. lm .gt. 2 ) then
        rc = 12
        return                ! can't handle multiple tracers yet
   end if

!  find lowest identical layer ktop, based on prescribed tolerance
!  ---------------------------------------------------------------
   ptop = w%grid%ptop
   if ( abs(dw%grid%ptop-ptop) > 0.001 ) then
        rc = 14
        return          ! no identical top layers
   end if

   if ( verb ) print *, myname//': delp relative tolerance = ', reltol

   ktop = km
   do k = 1, km
        tol = reltol * maxval(w%delp(:,:,k))
        if ( minval( w%delp(:,:,k)) .lt. tol .or. minval(dw%delp(:,:,k)) .lt. tol ) then
             rc = 13
             return     ! very bad news: zero-depth layer
        end if
        if ( maxval(abs(w%delp(:,:,k)-dw%delp(:,:,k))) .gt. tol ) then
             ktop = k - 1
             exit
        end if
   end do
   if ( verb ) print *, myname//': number of identical layers = ', ktop

   if ( ktop .eq. 0 ) then
        rc = 14
        return          ! this should never happen: no identical top layers
   end if

   if ( ktop .eq. km ) then    ! all layers are identical
        rc = 0
        if ( verb ) print *, myname//': no need for remapping'
        return
   end if

!  This is the situation:
!
!                    dw    w
!  p=ptop          ----- -----
!           k=1                 k=1
!                  ----- -----
!                    .     .
!                    .     .
!                  ----- -----
!           k=ktop              k=ktop
!                  ----- -----
!
!                  -----
!                        -----
!                  -----
!                    .
!                    .   -----
!                    .     .
!                    .     .
!                  -----   .
!           k=km           .
!  p=dw%ps         -----   .
!                  /////   .
!                        ------
!                                k=km
!  p= w%ps               ------
!                        //////
!
!  On input, dw%u, dw%v, dw%pt, dw%q are defined at the centers of the layers
!  on the left.  On output, they will be defined at the centers of the layers
!  on the right. We use linear interpolation in log(p) whenever p<dw%ps.
!  If w%ps>dw%ps in a column (as in the picture above) then we extrapolate
!  by extending the depth of the lowest layer in dw.

!  NOTE: Can be made faster by taking advantage of the fact that nothing
!         needs to be done for the first ktop layers

   allocate ( z1(km), z2(km), du(km), dv(km), dpt(km), dq(km), do3(km), stat=rc )
   if ( rc .ne. 0 ) return

   do j = 1, jm
     do i = 1, im

!       compute z1(k): log(p) at center of layer k in  w
!               z2(k): log(p) at center of layer k in dw

        pt = ptop
        do k = 1, km
           pb = pt +  w%delp(i,j,k)
           z1(k) = log(0.5*(pt + pb))
           pt = pb
        enddo
        pt = ptop
        do k = 1, km
           pb = pt + dw%delp(i,j,k)
           z2(k) = log(0.5*(pt + pb))
           pt = pb
        enddo

        du  = dw%u (i,j,1:km)
        dv  = dw%v (i,j,1:km)
        dpt = dw%pt(i,j,1:km)
        dq  = dw%q (i,j,1:km,1)
        if(lm==2) do3 = dw%q (i,j,1:km,2)

        n  = 0
        zb = z1(1) ! assumes z1(1)=z2(1)
        do k = 1, km

           z = z1(k)  ! interpolate to this coordinate

           do         ! find za <= z < zb

              if ( z .lt. zb .or. n .eq. km-1) exit

              n  = n+1
              za = z2(n)
              zb = z2(n+1)

           enddo

           if ( n .eq. km ) exit

           alpha = (z - za)/(zb - za)
           dw%u (i,j,k)   = du (n) + alpha*(du (n+1) - du (n))
           dw%v (i,j,k)   = dv (n) + alpha*(dv (n+1) - dv (n))
           dw%pt(i,j,k)   = dpt(n) + alpha*(dpt(n+1) - dpt(n))
           dw%q (i,j,k,1) = dq (n) + alpha*(dq (n+1) - dq (n))
           if(lm==2) dw%q (i,j,k,2) = do3(n) + alpha*(do3(n+1) - do3(n))

        enddo ! for k

     enddo ! for i
   enddo ! for j

   deallocate ( z1, z2, du, dv, dpt, dq, do3 )


!  copy vertical coordinates and implied surface pressure
!  ------------------------------------------------------
   dw%delp = w%delp
   dw%ps   = w%ps

   end subroutine  dynp_remap

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_add_surf --- Add delp perturbation to original vert. grid
!
! !INTERFACE:
!
   subroutine dynp_add_surf ( w, pcoef, dw, rc )
!
! !USES:
!
   implicit NONE
!
! !INPUT PARAMETERS:
!
   real, intent(in) ::  pcoef             ! multiplicative factor for pert.
!
! !INPUT/OUTPUT PARAMETERS:
!
   type(dyn_vect), intent (inout) :: dw   ! perturbation state vector
   type(dyn_vect), intent (inout) :: w    ! dynamics state vector

! !OUTPUT PARAMETERS:

   integer, intent(out) :: rc

! !DESCRIPTION:
!
!  Adds delp perturbation to original delp from w state.
!
! !REVISION HISTORY:
!
!  08apr2004 Todling   Initial code.
!  24mar2005 Novakovskaia/RT   Bug fix; ptop missing in ps recalc.
!  12Dec2004 Todling   Allow adding two perturbations fields
!
!EOP
!-------------------------------------------------------------------------

    integer :: im, jm, km
    integer :: i, j, k, ierr

    rc = 0

!  do some checking
!  ----------------
   im = w%grid%im
   jm = w%grid%jm
   km = w%grid%km

   if ( im .ne. dw%grid%im .or. jm .ne. dw%grid%jm .or. &
        km .ne. dw%grid%km ) then
        rc = 11
        return                ! can't handle different grid dimensions yet
   end if

!  adjust state vector vertical coodinate w/ perturbation of vertical coord.
!  -------------------------------------------------------------------------
   w%delp = w%delp  +  pcoef * dw%delp
   dw%delp = w%delp    ! make sure dw%delp now makes sense

!  adjust surface pressure accordingly
!  -----------------------------------
   w%ps = w%grid%ptop + sum(w%delp,dim=3)
   dw%ps = w%ps

   skipSHAVE = .true.

  end subroutine  dynp_add_surf

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_add --- Add a state increment to a reference state
!
! !INTERFACE:
!
   subroutine dynp_add ( w, a, dw, rc, &
                                   verbose )  ! optional
!
! !USES:
!
   implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
   type(dyn_vect), intent(inout) ::  w        ! state vector
   type(dyn_vect), intent(in)    :: dw        ! state vector increment
!
! !INPUT PARAMETERS:
!
   real,           intent(in)    :: a         ! scalar
   logical, intent(in), OPTIONAL :: verbose   ! if .true. then print stuff
!
! !OUTPUT PARAMETERS:
!
   integer, intent(out)          :: rc        ! error return code:
                                              !  0 - all is well
                                              !  1 - zero thickness in w
                                              !  2 - w and dw coordinates differ
!
! !DESCRIPTION: This routine computes w = w '+' a*dw,
!               where '+' includes mass adjustment.
!
!  NOTE: The user must make sure that, on input, the vertical coordinates
!  of w and dw are compatible (i.e., ptop and the delp arrays must be
!  identical). See dynp_remap.
!
!  Assuming identical vertical coordinates, all field perturbations
!  are simply added to the reference fields. Then the lowest layers
!  are adjusted ('shaved') to account for the change in geopotential
!  height at the surface.
!
!  The output state w is hydrostatic with nonnegative q.
!
! !REVISION HISTORY:
!
!  08nov2002 Dee       Initial code.
!  12Dec2004 Todling   Allow adding two perturbations fields
!  11Feb2008 Todling   Bug fix: coef (a) for ps/delp was missing!
!
!EOP
!-------------------------------------------------------------------------

   character(len=*),  parameter :: myname = 'dynp_add'

   logical verb

   if ( present ( verbose ) ) then
        verb = verbose
   else
        verb = .false.
   end if

!  Check vertical coordinates
!  --------------------------
   if ( .not. allPERT ) then
      if ( .not. gcmPERT ) then
         if ( minval(abs(w%delp)) .lt. reltol) then
              if ( verb ) print *, myname, 'zero thickness in w'
              rc = 1
              return
         end if
         if ( maxval(abs(w%delp - dw%delp)/w%delp) .gt. reltol) then
              if ( verb ) print *, myname, ' w and dw coordinates differ'
              rc = 2
              return
         end if
      end if
   end if

!  Add all fields
!  --------------
   w%ts   = w%ts + a * dw%ts
   w%u    = w%u  + a * dw%u
   w%v    = w%v  + a * dw%v
   w%pt   = w%pt + a * dw%pt
   w%q    = w%q  + a * dw%q

!  Add/subtract mass from lowest layers ('shaving method'),
!                                and ensure non-negative q
!  -------------------------------------------------------
   if ( skipSHAVE ) then

        if ( verb ) print *, myname//': no shaving applied '
        if ( gcmPERT ) then
             if ( anaPERT ) then
                  w%delp = w%delp      +  a *  dw%delp
                  w%ps   = w%ps        +  a *  dw%ps
             else
                  w%delp = w%delp      +  a *  dw%delp
                  w%ps   = w%grid%ptop + sum(w%delp,dim=3)
             endif
        endif

   else
        call dynp_shave ( w, a*dw%phis, w%grid%im, w%grid%jm, w%grid%km, rc )

        if ( verb .and. rc .ne. 0 ) print *, myname, '3-layer shaving failed'
   endif

   end subroutine dynp_add


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  dynp_shave --- Add or remove mass from lowest layers
!
! !INTERFACE:
!
  subroutine dynp_shave ( w, dphis, im, jm, km, rc )
!
! !USES:
!
  use m_const, only: cpd, grav, kappa
  implicit NONE

!
! !INPUT/OUTPUT PARAMETERS:
!
  type(dyn_vect), intent(inout)     :: w     ! dynamics state vector


! !INPUT PARAMETERS:
!
  integer, intent(in)   ::         im,jm,km  ! grid dimensions
  real,    intent(in)   ::   dphis(im,jm)    ! sfc geoptl perturbation (m**2/s**2)

!
! !OUTPUT PARAMETERS:
!

  integer, intent(out), optional    :: rc    ! error return code:
                                             !  0 - all is well
                                             !  1 - shaving failed: dps too large
!
! !DESCRIPTION: This routine adjusts the layer thicknesses of the state
!               vector w to account for a change in surface geopotential
!               given by dphis
!
! !REVISION HISTORY:
!
!  12nov2002 Dee       Initial code
!
!EOP
!-------------------------------------------------------------------------

   rc = 0

   call shave_ ( im, jm, km, w%ps, w%delp, w%pt, w%q, dphis )

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  shave_ --- shaving method
!
! !INTERFACE:
!
   subroutine shave_ ( im, jm, km, ps, delp, pt, q, dphis )

!
! !USES:
!
   Implicit NONE

!
! !INPUT PARAMETERS:
!
   integer, intent(in)  :: im, jm, km
   real,    intent(in)  :: dphis(im,jm)  ! geopotential height perturbation at p=ps

!
! !INPUT/OUTPUT PARAMETERS:
!
   real,    intent(inout)  ::    ps(im,jm)      ! Surface pressure (Pa)
   real,    intent(inout)  ::  delp(im,jm,km)   ! Delta pressure (Pa)
   real,    intent(inout)  ::    pt(im,jm,km)   ! scaled virtual potential
                                                !  temperature (T p**kappa)
   real,    intent(inout)  ::     q(im,jm,km)   ! Specific humidity (kg/kg)

! !DESCRIPTION:
!
! !REVISION HISTORY:
!
!  12nov2002  Dee    Initial code: lifted from ana2m in m_ana2dyn,
!                    but compute p**kappa as needed
!
!EOP
!-------------------------------------------------------------------------

! Local variables:

   real dp(im,jm,km)    ! this can easily be avoided (only used for final
                        ! check for thin layers)

   real, parameter :: dp_fac = 0.01   ! minimum delp reduction ratio

   integer i, j, k
   real*8  tmp8                   ! high accuracy is required
   real    cp, dgp, dph, phe, dp_min
   real    pe, pekm, pekm1, pekm2, pek, pekmk, pekm1k, pekm2k

   cp = cpd

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k,tmp8, dgp, dph, phe, pe, pekm, pekm1, pekm2, pek, pekmk, pekm1k, pekm2k, dp_min)
#endif

#if ( defined SGI )
!$doacross local(i,j,k,tmp8, dgp, dph, phe, pe, pekm, pekm1, pekm2, pek, pekmk, pekm1k, pekm2k, dp_min)
#endif

   do j = 1, jm

! ******************************************
! Compute new surface pressure
! Lagrangian control-volume "Shaving" method
! applied to the lowest three layers
! ******************************************

      do i = 1, im

        dgp  = dphis(i,j)
        pe   = ps(i,j)

        pek  = pe**kappa
        pekm = pe - delp(i,j,km)

        if ( dgp .gt. 0. ) then

! The new "surface height" is above model's terrain -->>
!     surface pressure must be increased.
! Compute the new surface pressure and add the increment to the
! lowest Lagrangian layer.

          tmp8 = pek + dgp/(cp*pt(i,j,km))
          ps(i,j) = tmp8 ** (1./kappa)
          delp(i,j,km) = ps(i,j) - pekm

        else if ( dgp .lt. 0. ) then

! The new "surface" is below model's terrain -->>
!     surface pressure must be decreased.
! Compute the new surface pressure and "shave" the Lagrangian
! layers up to the 3rd layer (counted from below). This shaving allows
! a surface pressure correction up to ~100 mb with the current L55 setup

          pekmk = pekm**kappa
          pekm1 = pekm - delp(i,j,km-1)

          dph = cp*pt(i,j,km)*(pek-pekmk)
! phe is the geopotential (relative to the model surface) at the top of layer-k
          phe = dph

          if ( phe + dgp .gt. 0. ) then
!
! psa (new surface pressure) is betw ps and pe(km)
! Do partial shaving of the lowest layer
!
            tmp8 = pekmk + (pek-pekmk)*(phe+dgp)/dph
            ps(i,j) = tmp8 ** (1./kappa)
            delp(i,j,km) = ps(i,j) - pekm

          else
!
! psa (new surface pressure) is smaller than pe(km)
! The entire lowest layer will be shaved

            pekm1k = pekm1**kappa
            pekm2  = pekm1 - delp(i,j,km-2)

            dph = cp*pt(i,j,km-1)*(pekmk-pekm1k)
            phe = phe + dph

            if ( phe + dgp .gt. 0. ) then
!
! psa (new surface pressure) is between pe(km) and pe(km-1)
! Do complete shaving to the 1st layer & partial shaving to the 2nd layer
! However, for computational reason, a tiny thickness will be retained.
! This tiny layer carries little mass, momentum, and energy. Therefore, the
! exact values of (u, v) within the tiny layer are not
! critically important. But they are useful as end boundary conditions for the
! vertical mapping to be performed later in the finite-volume dycore
!
               tmp8 = pekm1k + (pekmk-pekm1k)*(phe+dgp)/dph
               ps(i,j) = tmp8 ** (1./kappa)
               delp(i,j,km  ) = dp_fac*delp(i,j,km)
               delp(i,j,km-1) = ps(i,j) - pekm1 - delp(i,j,km)

! Since the entire 1st layer is "shaved", the analyzed thermodynamic properties
! of the tiny computational layer are that of the layer immediate above.
!
               pt(i,j,km) = pt(i,j,km-1)
                q(i,j,km) =  q(i,j,km-1)

            else
!
! The entire 1st & 2nd layer will be "shaved"!
!
               pekm2k = pekm2**kappa

               delp(i,j,km  ) =  dp_fac*delp(i,j,km)
               delp(i,j,km-1) =  dp_fac*delp(i,j,km-1)
               dph = cp*pt(i,j,km-2)*(pekm1k-pekm2k)
               phe = phe + dph

               if ( phe + dgp .gt. 0. ) then

! psa (new surface pressure) is between pe(km-1) and pe(km-2)
! Do partial shaving of the 3rd layer

                 tmp8 = pekm2k + (pekm1k-pekm2k)*(phe+dgp)/dph
                 ps(i,j) = tmp8 ** (1./kappa)
                 delp(i,j,km-2) = ps(i,j) - (pekm2 + delp(i,j,km) + delp(i,j,km-1))

               else

! Partial shaving of the 3rd layer is not enough -->> the analyzed height
! increment is too large; limit the new surface pressure to be no less
! than the pressure at the top edge of the 3rd (counted from below) layer

                 delp(i,j,km-2) = dp_fac*delp(i,j,km-2)
                 ps(i,j) = delp(i,j,km-2) + delp(i,j,km-1) + delp(i,j,km) + pekm2
!
! Nonzero return code:
! -------------------
                 rc = 1

               endif

               pt(i,j,km-1) = pt(i,j,km-2)
               pt(i,j,km  ) = pt(i,j,km-2)
                q(i,j,km-1) =  q(i,j,km-2)
                q(i,j,km)   =  q(i,j,km-2)

            endif
          endif
        endif       ! End of "shaving" check
      enddo         ! End i-loop
!
! Final safety check (to prevent delp values that are too close to zero)
!
      do i = 1, im
         do k = km, km-2
            dp_min = dp_fac * dp(i,j,k)
            if ( delp(i,j,k) .lt. dp_min ) then
! Mixing of thermal properties within the tiny layer:
                pt(i,j,k) = (delp(i,j,k)*pt(i,j,k) + (dp_min-delp(i,j,k))*   &
                                  pt(i,j,k-1)) / dp_min
                q(i,j,k)  = (delp(i,j,k)*q(i,j,k) + (dp_min-delp(i,j,k))*   &
                                  q(i,j,k-1)) / dp_min
!
                delp(i,j,k-1) = delp(i,j,k-1) - dp_min + delp(i,j,k)
                delp(i,j,k  ) = dp_min
            endif
         enddo         ! end k-loop
      enddo         ! end i-loop

!
! Make sure q is non-negative.
! TO DO: Do we need a borrowing scheme? (ams)
!
      do i = 1, im
         do k = 1, km
            q(i,j,k) = max ( 0.0, q(i,j,k) )
         end do
      end do

   enddo          ! End parallel j-loop


   end subroutine shave_

   end subroutine dynp_shave

   subroutine dynp_scale ( dw, value )
   implicit none
!
! !REVISION HISTORY:
!  29 Feb 2008 Todling - scale perturbation by specific value
! !REMARKS:
!    Beware this is not meaningful in most cases
!
   type(dyn_vect) dw
   real, intent(in) :: value

   dw%u    = value * dw%u
   dw%v    = value * dw%v
   dw%pt   = value * dw%pt
   dw%q    = value * dw%q
   dw%delp = value * dw%delp
   dw%ps   = value * dw%ps
   dw%ts   = value * dw%ts
   print *, 'dynp_set: scaled field by: ', value

   end subroutine dynp_scale

!-------------------------------------------------------------------------

   subroutine dynp_set ( varname, value )
   implicit none
   character(len=*), intent(in) :: varname
   logical,          intent(in) :: value
   character(len=255) varn
   varn = uppercase(varname)
   if ( trim(varn) == 'GCMPERT'   ) gcmPERT   = value
   if ( trim(varn) == 'ANAPERT'   ) anaPERT   = value
   if ( trim(varn) == 'ALLPERT'   ) allPERT   = value
   if ( trim(varn) == 'SKIPSHAVE' ) skipSHAVE = value
   end subroutine dynp_set
  end module m_dynp
