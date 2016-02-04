module fv_dynamics_mod
#ifndef MAPL_MODE
   use constants_mod,      only: grav, pi, radius, hlv, rdgas    ! latent heat of water vapor
#else
   use MAPL_MOD
#endif
   use fv_arrays_mod,      only: FVPRC, REAL8
   use fv_control_mod,     only: dyn_timer, comm_timer
   use dyn_core_mod,       only: dyn_core, del2_cubed
   use fv_mapz_mod,        only: compute_total_energy, Lagrangian_to_Eulerian
   use fv_tracer2d_mod,    only: tracer_2d, tracer_2d_1L
   use fv_grid_tools_mod,  only: agrid
   use fv_control_mod,     only: hord_mt, hord_vt, hord_tm, hord_tr, &
                                 kord_mt, kord_wz, kord_tm, kord_tr, moist_phys, &
                                 range_warn, inline_q, z_tracer, tau, rf_center, &
                                 nf_omega, te_method, remap_t, k_top, p_ref, nwat, fv_debug, &
                                 k_split, no_dycore, replace_w, shallow_water
   use fv_grid_utils_mod,  only: da_min, ptop, cubed_to_latlon, c2l_ord2
   use fv_grid_tools_mod,  only: dx, dy, rdxa, rdya, rdxc, rdyc, area, rarea
   use fv_mp_mod,          only: is,js,ie,je, isd,jsd,ied,jed, gid, domain
   use fv_timing_mod,      only: timing_on, timing_off
   use diag_manager_mod,   only: send_data
   use fv_diagnostics_mod, only: id_divg, id_te, fv_time, prt_maxmin, range_check
   use mpp_domains_mod,    only: DGRID_NE, mpp_update_domains
   use field_manager_mod,  only: MODEL_ATMOS
   use tracer_manager_mod, only: get_tracer_index
   use fv_sg_mod,          only: neg_adj3

#ifdef WAVE_MAKER
   use time_manager_mod,   only: get_time
#endif

implicit none

  ! Include the MPI library definitons:
  include 'mpif.h'

   logical :: RF_initialized = .false.
   logical :: bad_range
   real(REAL8), allocatable ::  rf(:), rw(:)
   integer :: kmax=1
private
public :: fv_dynamics

#ifdef MAPL_MODE
  real(REAL8), parameter :: RADIUS       = MAPL_RADIUS
  real(REAL8), parameter :: PI           = MAPL_PI_R8
  real(REAL8), parameter :: RDGAS        = MAPL_RGAS
  real(REAL8), parameter :: GRAV         = MAPL_GRAV
  real(REAL8), parameter :: HLV          = MAPL_ALHL
#endif

  real(REAL8), parameter :: r1_0 = 1.0
  real(REAL8), parameter :: r1eM2 = 1.e-2

contains

!-----------------------------------------------------------------------
!     fv_dynamics :: FV dynamical core driver
!-----------------------------------------------------------------------
 
  subroutine fv_dynamics(npx, npy, npz, nq,  ng, bdt, consv_te, fill,               &
                        reproduce_sum, kappa, cp_air, zvir, ks, ncnst, n_split,     &
                        q_split, u, v, w, delz, hydrostatic, pt, delp, q,   &
                        ps, pe, pk, peln, pkz, phis, omga, ua, va, uc, vc,          &
                        ak, bk, mfx, mfy, cx, cy, ze0, hybrid_z, time_total)

    real(REAL8), intent(IN) :: bdt  ! Large time-step
    real(REAL8), intent(IN) :: consv_te
    real(REAL8), intent(IN) :: kappa, cp_air
    real(REAL8), intent(IN) :: zvir
    real(REAL8), intent(IN), optional :: time_total

    integer, intent(IN) :: npx
    integer, intent(IN) :: npy
    integer, intent(IN) :: npz
    integer, intent(IN) :: nq             ! transported tracers
    integer, intent(IN) :: ng
    integer, intent(IN) :: ks
    integer, intent(IN) :: ncnst
    integer, intent(IN) :: n_split        ! small-step horizontal dynamics
    integer, intent(IN) :: q_split        ! tracer
    logical, intent(IN) :: fill
    logical, intent(IN) :: reproduce_sum
    logical, intent(IN) :: hydrostatic
    logical, intent(IN) :: hybrid_z       ! Using hybrid_z for remapping

    real(REAL8), intent(inout), dimension(isd:ied  ,jsd:jed+1,npz) :: u ! D grid zonal wind (m/s)
    real(REAL8), intent(inout), dimension(isd:ied+1,jsd:jed  ,npz) :: v ! D grid meridional wind (m/s)
    real(REAL8), intent(inout) :: w(   isd:ied  ,jsd:jed  ,npz)  !  W (m/s)
    real(REAL8), intent(inout) :: pt(  isd:ied  ,jsd:jed  ,npz)  ! temperature (K)
    real(REAL8), intent(inout) :: delp(isd:ied  ,jsd:jed  ,npz)  ! pressure thickness (pascal)
    real(REAL8), intent(inout) :: q(   isd:ied  ,jsd:jed  ,npz, ncnst) ! specific humidity and constituents
    real(REAL8), intent(inout) :: delz(is:ie,js:je,npz)   ! delta-height (m); non-hydrostatic only
    real(REAL8), intent(inout) ::  ze0(is:ie,js:je,npz+1) ! height at edges (m); non-hydrostatic

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real(REAL8), intent(inout) :: ps  (isd:ied  ,jsd:jed)           ! Surface pressure (pascal)
    real(REAL8), intent(inout) :: pe  (is-1:ie+1, npz+1,js-1:je+1)  ! edge pressure (pascal)
    real(REAL8), intent(inout) :: pk  (is:ie,js:je, npz+1)          ! pe**cappa
    real(REAL8), intent(inout) :: peln(is:ie,npz+1,js:je)           ! ln(pe)
    real(REAL8), intent(inout) :: pkz (is:ie,js:je,npz)             ! finite-volume mean pk
    
!-----------------------------------------------------------------------
! Others:
!-----------------------------------------------------------------------
    real(REAL8), intent(inout) :: phis(isd:ied,jsd:jed)       ! Surface geopotential (g*Z_surf)
    real(FVPRC), intent(inout) :: omga(isd:ied,jsd:jed,npz)   ! Vertical pressure velocity (pa/s)
    real(FVPRC), intent(inout) :: uc(isd:ied+1,jsd:jed  ,npz) ! (uc,vc) mostly used as the C grid winds
    real(FVPRC), intent(inout) :: vc(isd:ied  ,jsd:jed+1,npz)

    real(FVPRC), intent(inout), dimension(isd:ied ,jsd:jed ,npz):: ua, va
    real(REAL8), intent(in),    dimension(npz+1):: ak, bk

! Accumulated Mass flux arrays: the "Flux Capacitor"
    real(FVPRC), intent(inout) ::  mfx(is:ie+1, js:je,   npz)
    real(FVPRC), intent(inout) ::  mfy(is:ie  , js:je+1, npz)
! Accumulated Courant number arrays
    real(FVPRC), intent(inout) ::  cx(is:ie+1, jsd:jed, npz)
    real(FVPRC), intent(inout) ::  cy(isd:ied ,js:je+1, npz)


! Local Arrays
! Local Mass flux arrays: the "Flux Capacitor"
      real(FVPRC)::  mfxL(is:ie+1, js:je,   npz)
      real(FVPRC)::  mfyL(is:ie  , js:je+1, npz)
! Local Courant number arrays
      real(FVPRC)::  cxL(is:ie+1, jsd:jed, npz)
      real(FVPRC)::  cyL(isd:ied ,js:je+1, npz)
      real(REAL8):: q2(isd:ied,jsd:jed,nq)
      real(REAL8):: te_2d(is:ie,js:je)
      real(REAL8)::   teq(is:ie,js:je)
      real(REAL8):: pfull(npz)
      real(REAL8):: gz(is:ie)
      real(REAL8), allocatable :: dp1(:,:,:)
      real(REAL8):: akap, rg, rdg, ph1, ph2, mdt, t1, t2
      integer:: kord_tracer(ncnst), status
      integer :: i,j,k, n, iq, Qs, n_map
      integer :: sphum, liq_wat, ice_wat      ! GFDL physics
      integer :: rainwat, snowwat, graupel, cld_amt
      logical used, last_step
      integer:: iwat

!     iwat = nwat
      iwat = 1

      dyn_timer = 0
      comm_timer = 0

      if ( no_dycore ) goto 911

! Empty the "flux capacitors"
      mfx=0.0
      mfy=0.0 
       cx=0.0 
       cy=0.0 

#ifdef WAVE_MAKER
      integer seconds, days
      real(REAL8)  r0, stime

         call get_time (fv_time, seconds,  days)
         r0 = pi/30.
         stime = real(seconds)/86400.*2.*pi

!$omp parallel do default(shared)
         do j=jsd,jed
            do i=isd,ied
               phis(i,j) = grav*250.*sin(agrid(i,j,1))*sin(stime) / exp( (agrid(i,j,2)/r0)**2 )
            enddo
         enddo
#endif
      allocate ( dp1(is:ie, js:je, 1:npz) )

  if (shallow_water) then
      akap  = 1.e-15
      pfull(1) = 0.5*p_ref
  else
      if ( nwat==6 ) then
             sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
           liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
           ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
           rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
           snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
           graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
           cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')
      elseif (nwat==3 ) then
             sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
           liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
           ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
           cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')
      else
           sphum = 1
           cld_amt = -1   ! to cause trouble if (mis)used
      endif

      akap  = kappa
      rg = kappa*cp_air

!$omp parallel do default(shared)
      do k=1,npz
         ph1 = ak(k  ) + bk(k  )*p_ref
         ph2 = ak(k+1) + bk(k+1)*p_ref
         pfull(k) = (ph2 - ph1) / log(ph2/ph1)
      enddo

      if ( fv_debug ) then
         call prt_maxmin('T_dyn_b',   pt, is, ie, js, je, ng, npz, r1_0, gid==0)
         call prt_maxmin('delp_b ', delp, is, ie, js, je, ng, npz, r1eM2, gid==0)
      endif

      if ( iwat==6 ) then
!$omp parallel do default(shared) 
           do k=1,npz
              do j=js,je
                 do i=is,ie
                    dp1(i,j,k) = zvir*q(i,j,k,sphum)      &
                                   - (q(i,j,k,liq_wat)    &
                                    + q(i,j,k,ice_wat)    &
                                    + q(i,j,k,rainwat)    &
                                    + q(i,j,k,snowwat)    &
                                    + q(i,j,k,graupel) )
                 enddo
              enddo
           enddo
      elseif ( iwat==3 ) then
!$omp parallel do default(shared) 
           do k=1,npz
              do j=js,je
                 do i=is,ie
                    dp1(i,j,k) = zvir*q(i,j,k,sphum) - (q(i,j,k,liq_wat)+q(i,j,k,ice_wat))
                 enddo
              enddo
           enddo
      else
!$omp parallel do default(shared) 
           do k=1,npz
              do j=js,je
                 do i=is,ie
                    dp1(i,j,k) = zvir*q(i,j,k,sphum)
                 enddo
              enddo
           enddo
      endif

#ifdef DYCORE_SOLO
  if ( .not. hydrostatic ) then
! Re-compute pkz to enforce restart reproducibility when in solo mode
   rdg = -rdgas / grav
!$omp parallel do default(shared)
  do k=1,npz
     do j=js,je
        do i=is,ie
           pkz(i,j,k) = exp( kappa*log(rdg*delp(i,j,k)*pt(i,j,k)*    &
                             (1.+zvir*q(i,j,k,sphum))/delz(i,j,k)) )
        enddo
     enddo
  enddo
  endif
#endif

!---------------------
! Compute Total Energy
!---------------------
      if ( consv_te > 0. ) then
           call compute_total_energy(is, ie, js, je, isd, ied, jsd, jed, npz,        &
                                     u, v, w, delz, pt, delp, q, dp1, pe, peln, phis, &
                                     cp_air, rg, hlv, te_2d, ua, va, teq,            &
                                     moist_phys, sphum, hydrostatic, id_te)
#ifndef MAPL_MODE
           if( id_te>0 ) then
               used = send_data(id_te, teq, fv_time)
!              te_den=1.E-9*g_sum(teq, is, ie, js, je, ng, area, 0)/(grav*4.*pi*radius**2)
!              if(gid==0)  write(*,*) 'Total Energy Density (Giga J/m**2)=',te_den
           endif
#endif
      endif

      if( tau > 0. )      &
      call Rayleigh_Friction(abs(bdt), npx, npy, npz, ks, pfull, tau, rf_center, u, v, w, pt,  &
                             ua, va, delz, cp_air, rg,  hydrostatic, .true.)


! Convert pt to virtual potential temperature * CP
!$omp parallel do default(shared) 
  do k=1,npz
     do j=js,je
        do i=is,ie
           pt(i,j,k) = cp_air*pt(i,j,k)/pkz(i,j,k) * (1.+dp1(i,j,k))
        enddo
     enddo
  enddo

  endif ! shallow_water

  t1 = MPI_Wtime(status)

  mdt = bdt / real(k_split)

  do n_map=1, k_split   ! first level of time-split

     if ( n_map==k_split )  then
          last_step = .true.
     else
          last_step = .false.
     endif

!$omp parallel do default(shared) 
      do k=1,npz
         do j=js,je
            do i=is,ie
               dp1(i,j,k) = delp(i,j,k)
            enddo
         enddo
      enddo

! Empty the local "flux capacitors"
      mfxL=0.0
      mfyL=0.0
       cxL=0.0
       cyL=0.0

      call dyn_core(npx, npy, npz, ng, sphum, 1, mdt, n_split, zvir, cp_air, akap, grav, hydrostatic, &
                    u, v, w, delz, pt, q, delp, pe, pk, phis, omga, ptop, pfull, ua, va,       & 
                    uc, vc, mfxL, mfyL, cxL, cyL, pkz, peln, ak, bk, n_map==1, last_step, time_total)

! Accumulate "flux capacitors" for export
      mfx=mfx+mfxL
      mfy=mfy+mfyL
       cx=cx+cxL
       cy=cy+cyL

  if (shallow_water) then
!$omp parallel do default(shared)
      do j=js,je
         do i=is,ie
            ps(i,j) = delp(i,j,1) / grav
         enddo
      enddo
  else
      Qs=1
      if ( inline_q ) Qs=2
! diagnose divergence:
      if ( nq /= 0 ) then    
!--------------------------------------------------------
! Perform large-time-step scalar transport using the accumulated CFL and
! mass fluxes
         call timing_on('tracer_2d')
       if ( z_tracer ) then

!$omp parallel do default(shared) private(q2)
         do k=1,npz
            do iq=Qs,nq
            do j=js,je
               do i=is,ie                   ! To_do list:
                  q2(i,j,iq) = q(i,j,k,iq)  ! The data copying can be avoided if q is
                                            ! re-dimensioned as q(i,j,nq,k)
               enddo
            enddo
            enddo
         call tracer_2d_1L(q2, dp1(is,js,k), mfxL(is,js,k), mfyL(is,js,k), &
                           cxL(is,jsd,k),  cyL(isd,js,k), npx, npy, npz,   &
                           Qs, nq, hord_tr, q_split, k, q, mdt, id_divg)
         enddo
       else
         call tracer_2d(q, dp1, mfxL, mfyL, cxL, cyL, npx, npy, npz, Qs, nq, &
                        hord_tr, q_split, mdt, id_divg)
       endif
         call timing_off('tracer_2d')
#ifndef MAPL_MODE
         if( last_step .and. id_divg>0 ) then
             used = send_data(id_divg, dp1, fv_time) 
             if(fv_debug) call prt_maxmin('divg',  dp1, is, ie, js, je, 0, npz, r1_0, gid==0)
         endif
#endif
      endif


      if ( npz > 4 ) then
!------------------------------------------------------------------------
! Peroform vertical remapping from Lagrangian control-volume to
! the Eulerian coordinate as specified by the routine set_eta.
! Note that this finite-volume dycore is otherwise independent of the vertical
! Eulerian coordinate.
!------------------------------------------------------------------------

         do iq=1,nq
                                kord_tracer(iq) = kord_tr
            if ( iq==cld_amt )  kord_tracer(iq) = 9      ! monotonic
         enddo
                                                  call timing_on('Remapping')

         call Lagrangian_to_Eulerian(last_step, consv_te, ps, pe, delp,      &
                     pkz, pk, bdt, npz, is,ie,js,je, isd,ied,jsd,jed,        &
                     nq, sphum, u,  v, w, delz, pt, q, phis, zvir, cp_air,   &
                     akap, kord_mt, kord_wz, kord_tracer, kord_tm, peln, te_2d,  &
                     ng, ua, va, omga, dp1, fill, reproduce_sum,        &
                     ak, bk, ks, ze0, te_method, remap_t, hydrostatic, hybrid_z, last_step, k_top)

                                                  call timing_off('Remapping')
         if( last_step )  then

           !if( hydrostatic ) then
!--------------------------
! Filter omega for physics:
!--------------------------
                if(nf_omega>0)    &
                call del2_cubed(omga, 0.20*da_min, npx, npy, npz, nf_omega)
           !else
!$OMP parallel do default(none) shared(is,ie,js,je,npz,omga,delp,delz,w)
           !   do k=1,npz
           !      do j=js,je
           !         do i=is,ie
           !            omga(i,j,k) = delp(i,j,k)/delz(i,j,k)*w(i,j,k)
           !         enddo
           !      enddo
           !   enddo
           !endif

         endif

      endif
  endif ! shallow_water

  enddo    ! n_map loop

  if (.not. shallow_water) then
! Convert back to temperature
  if ( iwat==6 ) then
!$omp parallel do default(shared) 
  do k=1,npz
     do j=js,je
        do i=is,ie
            pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)/(cp_air*( 1.+zvir*q(i,j,k,sphum)      &
                                                            - (q(i,j,k,liq_wat)    &
                                                             + q(i,j,k,ice_wat)    &
                                                             + q(i,j,k,rainwat)    &
                                                             + q(i,j,k,snowwat)    &
                                                             + q(i,j,k,graupel))) )
        enddo
     enddo
  enddo
  elseif ( iwat==3 ) then
!$omp parallel do default(shared) 
  do k=1,npz
     do j=js,je
        do i=is,ie
            pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)/(cp_air*( 1.+zvir*q(i,j,k,sphum)      &
                                                            - (q(i,j,k,liq_wat)    &
                                                             + q(i,j,k,ice_wat))) )
        enddo
     enddo
  enddo
  else
!$omp parallel do default(shared) 
  do k=1,npz
     do j=js,je
        do i=is,ie
            pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)/(cp_air*(1.+zvir*q(i,j,k,sphum)))
        enddo
     enddo
  enddo
  endif
  endif ! .not. shallow_water

      deallocate ( dp1 )

  if ( fv_debug ) then
       call prt_maxmin('delp_a',  delp, is, ie, js, je, ng, npz, r1eM2, gid==0)
       call prt_maxmin('T_dyn_a',   pt, is, ie, js, je, ng, npz, r1_0, gid==0)
      !call prt_maxmin('pk_a',   pk, is, ie, js, je, 0, npz+1, r1_0, gid==0)
      !call prt_maxmin('pkz_a',  pkz, is, ie, js, je, 0, npz, r1_0, gid==0)
  endif

#ifndef MAPL_MODE
  if( nwat==6 ) then
      call neg_adj3(is, ie, js, je, ng, npz,        &
                    pt, delp, q(isd,jsd,1,sphum),   &
                              q(isd,jsd,1,liq_wat), &
                              q(isd,jsd,1,rainwat), &
                              q(isd,jsd,1,ice_wat), &
                              q(isd,jsd,1,snowwat), &
                              q(isd,jsd,1,graupel), &
                              q(isd,jsd,1,cld_amt)  )
     if ( fv_debug ) then
       call prt_maxmin('SPHUM_dyn',   q(isd,jsd,1,sphum  ), is, ie, js, je, ng, npz, r1_0, gid==0)
       call prt_maxmin('liq_wat_dyn', q(isd,jsd,1,liq_wat), is, ie, js, je, ng, npz, r1_0, gid==0)
       call prt_maxmin('ice_wat_dyn', q(isd,jsd,1,ice_wat), is, ie, js, je, ng, npz, r1_0, gid==0)
       call prt_maxmin('snowwat_dyn', q(isd,jsd,1,snowwat), is, ie, js, je, ng, npz, r1_0, gid==0)
       call prt_maxmin('graupel_dyn', q(isd,jsd,1,graupel), is, ie, js, je, ng, npz, r1_0, gid==0)
!      call prt_maxmin('cld_amt_dyn', q(isd,jsd,1,cld_amt), is, ie, js, je, ng, npz, r1_0, gid==0)
     endif
  endif
#endif

911  call cubed_to_latlon(u, v, ua, va, dx, dy, rdxa, rdya, npz, 1)
     if ( fv_debug ) then
       call prt_maxmin('UA', DBLE(ua), is, ie, js, je, ng, npz, r1_0, gid==0)
       call prt_maxmin('VA', DBLE(va), is, ie, js, je, ng, npz, r1_0, gid==0)
     endif

#ifndef MAPL_MODE
  if ( range_warn ) then
       call range_check('UA_dyn', ua, is, ie, js, je, ng, npz, agrid,   &
                         gid==0, -220., 260., bad_range)
       call range_check('VA_dyn', ua, is, ie, js, je, ng, npz, agrid,   &
                         gid==0, -220., 220., bad_range)
       if (.not. shallow_water) then
         call range_check('TA_dyn', pt, is, ie, js, je, ng, npz, agrid,   &
                           gid==0, 160., 330., bad_range)
         if ( .not. hydrostatic ) &
         call range_check('W_dyn', w, is, ie, js, je, ng, npz, agrid,   &
                           gid==0, -20., 20., bad_range)
       endif
  endif
#endif

  t2 = MPI_Wtime(status)
  dyn_timer = dyn_timer + (t2-t1)

  end subroutine fv_dynamics


#ifdef OLD_RAYF

 subroutine Rayleigh_Friction(dt, npx, npy, npz, ks, pm, tau, p_c, u, v, w, pt,  &
                              ua, va, delz, cp, rg, hydrostatic, conserve)
    real(REAL8), intent(in):: dt
    real(REAL8), intent(in):: tau              ! time scale (days)
    real(REAL8), intent(in):: p_c
    real(REAL8), intent(in):: cp, rg
    real(REAL8), intent(in),  dimension(npz):: pm
    integer, intent(in):: npx, npy, npz, ks
    logical, intent(in):: hydrostatic
    logical, intent(in):: conserve
    real(REAL8), intent(inout):: u(isd:ied  ,jsd:jed+1,npz) ! D grid zonal wind (m/s)
    real(REAL8), intent(inout):: v(isd:ied+1,jsd:jed,npz) ! D grid meridional wind (m/s)
    real(REAL8), intent(inout)::  w(isd:ied,jsd:jed,npz) ! cell center vertical wind (m/s)
    real(REAL8), intent(inout):: pt(isd:ied,jsd:jed,npz) ! temp
    real(FVPRC), intent(inout):: ua(isd:ied,jsd:jed,npz) ! 
    real(FVPRC), intent(inout):: va(isd:ied,jsd:jed,npz) ! 
    real(REAL8), intent(inout):: delz(is:ie,js:je,npz)   ! delta-height (m); non-hydrostatic only
    real(REAL8), parameter:: sday = 86400.
    real(REAL8), parameter:: wfac = 10.     ! factor to amplify the drag on w
    real(REAL8) c1, pc, fac
    integer i, j, k

     kmax = max(npz/3+1, ks)

     if ( .not. RF_initialized ) then
          allocate( rf(npz) )
          allocate( rw(npz) )

          if ( p_c <= 0. ) then
               pc = pm(1)
          else
               pc = p_c
          endif

          if( gid==0 ) write(6,*) 'Rayleigh friction E-folding time [days]:'
          c1 = 1. / (tau*sday)
          do k=1,kmax
             if ( pm(k) < 30.E2 ) then
                  rf(k) = c1*(1.+tanh(log10(pc/pm(k))))
                  if( gid==0 ) write(6,*) k, 0.01*pm(k), 1./(rf(k)*sday)
                  rf(k) = 1./(1.+dt*rf(k))
                  rw(k) = 1./(1.+dt*rf(k)*wfac)
             endif
          enddo
          RF_initialized = .true.
     endif

     if(conserve) call c2l_ord2(u, v, ua, va, dx, dy, rdxa, rdya, npz)

!$omp parallel do default(shared)
     do k=1,kmax
        if ( pm(k) < 30.E2 ) then
! Add heat so as to conserve TE
          if ( conserve ) then
               fac = 0.5*(1.-rf(k)**2) / (cp-rg*ptop/pm(k))
               do j=js,je
                  do i=is,ie
                     pt(i,j,k) = pt(i,j,k) + fac*(ua(i,j,k)**2 + va(i,j,k)**2)
                  enddo
               enddo
          endif
             do j=js,je+1
                do i=is,ie
                   u(i,j,k) = u(i,j,k)*rf(k)
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   v(i,j,k) = v(i,j,k)*rf(k)
                enddo
             enddo
          if ( .not. hydrostatic ) then
             do j=js,je
                do i=is,ie
                   w(i,j,k) = w(i,j,k)*rw(k)
                enddo
             enddo
          endif
        endif
     enddo

 end subroutine Rayleigh_Friction

#else
 subroutine Rayleigh_Friction(dt, npx, npy, npz, ks, pm, tau, p_c, u, v, w, pt,  &
                              ua, va, delz, cp, rg, hydrostatic, conserve)
    real(REAL8), intent(in):: dt
    real(REAL8), intent(in):: tau              ! time scale (days)
    real(REAL8), intent(in):: p_c
    real(REAL8), intent(in):: cp, rg
    real(REAL8), intent(in),  dimension(npz):: pm
    integer, intent(in):: npx, npy, npz, ks
    logical, intent(in):: hydrostatic
    logical, intent(in):: conserve
    real(REAL8), intent(inout):: u(isd:ied  ,jsd:jed+1,npz) ! D grid zonal wind (m/s)
    real(REAL8), intent(inout):: v(isd:ied+1,jsd:jed,npz) ! D grid meridional wind (m/s)
    real(REAL8), intent(inout)::  w(isd:ied,jsd:jed,npz) ! cell center vertical wind (m/s)
    real(REAL8), intent(inout):: pt(isd:ied,jsd:jed,npz) ! temp
    real(FVPRC), intent(inout):: ua(isd:ied,jsd:jed,npz) ! 
    real(FVPRC), intent(inout):: va(isd:ied,jsd:jed,npz) ! 
    real(REAL8), intent(inout):: delz(is:ie,js:je,npz)   ! delta-height (m); non-hydrostatic only
! local:
    real(REAL8), allocatable ::  u2f(:,:,:)
    real(REAL8), parameter:: sday = 86400.
    real(REAL8), parameter:: u000 = 4900.   ! scaling velocity  **2
    real(REAL8) c1, pc, fac
    integer i, j, k

    if ( .not. RF_initialized ) then
          allocate( rf(npz) )
          allocate( rw(npz) )

          if ( p_c <= 0. ) then
               pc = pm(1)
          else
               pc = p_c
          endif

          if( gid==0 ) write(6,*) 'Rayleigh friction E-folding time [days]:'
          c1 = 1. / (tau*sday)

          kmax = 1
          do k=1,npz
             if ( pm(k) < 40.E2 ) then
                  rf(k) = c1*(1.+tanh(log10(pc/pm(k))))
                  kmax = k
                  if( gid==0 ) write(6,*) k, 0.01*pm(k), 1./(rf(k)*sday)
             else
                exit
             endif
          enddo
          if( gid==0 ) write(6,*) 'Rayleigh Friction kmax=', kmax

          RF_initialized = .true.
    endif

    allocate( u2f(isd:ied,jsd:jed,kmax) )

    call c2l_ord2(u, v, ua, va, dx, dy, rdxa, rdya, npz)
    u2f = 0.
!$omp parallel do default(shared)
    do k=1,kmax
        if ( hydrostatic ) then
           do j=js,je
              do i=is,ie
                 u2f(i,j,k) = ua(i,j,k)**2 + va(i,j,k)**2
              enddo
           enddo
        else
           do j=js,je
              do i=is,ie
                 u2f(i,j,k) = ua(i,j,k)**2 + va(i,j,k)**2 + w(i,j,k)**2
              enddo
           enddo
        endif
    enddo
                                                                call timing_on('COMM_TOTAL')
    call mpp_update_domains(u2f, domain, complete=.true.)
                                                                call timing_off('COMM_TOTAL')

!$omp parallel do default(shared)
     do k=1,kmax

        if ( conserve ) then
           if ( hydrostatic ) then
             do j=js,je
                do i=is,ie
                   pt(i,j,k) = pt(i,j,k) + 0.5*u2f(i,j,k)/(cp-rg*ptop/pm(k))      &
                             * ( 1. - 1./(1.+dt*rf(k)*sqrt(u2f(i,j,k)/u000))**2 )
                enddo
             enddo
           else
             do j=js,je
                do i=is,ie
                   delz(i,j,k) = delz(i,j,k) / pt(i,j,k)
                   pt(i,j,k) = pt(i,j,k) + 0.5*u2f(i,j,k)/(cp-rg*ptop/pm(k))      &
                             * ( 1. - 1./(1.+dt*rf(k)*sqrt(u2f(i,j,k)/u000))**2 )
                   delz(i,j,k) = delz(i,j,k) * pt(i,j,k)
                enddo
             enddo
           endif
        endif

        do j=js-1,je+1
           do i=is-1,ie+1
              u2f(i,j,k) = dt*rf(k)*sqrt(u2f(i,j,k)/u000)
           enddo
        enddo

        do j=js,je+1
           do i=is,ie
              u(i,j,k) = u(i,j,k) / (1.+0.5*(u2f(i,j-1,k)+u2f(i,j,k)))
           enddo
        enddo
        do j=js,je
           do i=is,ie+1
              v(i,j,k) = v(i,j,k) / (1.+0.5*(u2f(i-1,j,k)+u2f(i,j,k)))
           enddo
        enddo

        if ( .not. hydrostatic ) then
              do j=js,je
                 do i=is,ie
                    w(i,j,k) = w(i,j,k) / (1.+u2f(i,j,k))
                 enddo
              enddo
        endif

     enddo

     deallocate ( u2f )

 end subroutine Rayleigh_Friction
#endif

end module fv_dynamics_mod
