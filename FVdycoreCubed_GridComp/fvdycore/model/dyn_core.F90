module dyn_core_mod

  use fv_arrays_mod,      only: FVPRC, REAL4, REAL8
  use fv_control_mod,     only: comm_timer
  use mpp_mod,            only: mpp_pe 
  use mpp_domains_mod,    only: CGRID_NE, DGRID_NE, mpp_get_boundary,   &
                                mpp_update_domains
  use mpp_parameter_mod,  only: CORNER
  use fv_mp_mod,          only: domain, isd, ied, jsd, jed, is, ie, js, je, gid, square_domain
  use fv_control_mod,     only: hord_mt, hord_vt, hord_tm, hord_dp, hord_tr, n_sponge,  &
                                dddmp, d2_bg, d4_bg, d_ext, vtdm4, reset_beta, beta, m_grad_p, &
                                a2b_ord, master, fv_debug, d_con, kd3, scale_z, zd_z1, do_vort_damp, nord, &
                                fill_dp, nwat, inline_q, breed_vortex_inline,    &
                                d2_bg_k1, d2_bg_k2, d2_divg_max_k1, d2_divg_max_k2, damp_k_k1, damp_k_k2, &
                                w_max, z_min, fill_wz, convert_ke, shallow_water
  use sw_core_mod,        only: c_sw, d_sw, divergence_corner, d2a2c_vect
  use a2b_edge_mod,       only: a2b_ord2, a2b_ord4
  use nh_core_mod,        only: Riem_Solver_C, Riem_Solver, update_dz_c, update_dz_d
  use fv_grid_tools_mod,  only: rdx, rdy, rdxc, rdyc, dx, dy, area, rarea, grid_type
  use fv_grid_utils_mod,  only: edge_vect_w, edge_vect_e, edge_vect_s, edge_vect_n,  &
                                ec1, ec2, en1, en2, vlon, vlat, da_min, da_min_c
  use fv_grid_utils_mod,  only: sina_u, sina_v, cosa_u, cosa_v,          &
                                sw_corner, se_corner, ne_corner, nw_corner
  use tp_core_mod,        only: copy_corners
  use fv_timing_mod,      only: timing_on, timing_off
  use fv_diagnostics_mod, only: prt_maxmin, id_zratio, fv_time
#ifndef MAPL_MODE
!  use fv_nwp_nudge_mod,   only: breed_slp_inline
#endif
!---
  use diag_manager_mod,   only: send_data
  use test_cases_mod,      only: test_case, case9_forcing1, case9_forcing2

implicit none

  ! Include the MPI library definitons:
  include 'mpif.h'

private

INTERFACE geopk
 module procedure geopk_r4
 module procedure geopk_r8
END INTERFACE geopk

INTERFACE mix_dp
 module procedure mix_dp_r4
 module procedure mix_dp_r8
END INTERFACE mix_dp

INTERFACE init_ijk_mem
 module procedure init_ijk_mem_r4
 module procedure init_ijk_mem_r8
END INTERFACE init_ijk_mem

public :: dyn_core, del2_cubed

    real(REAL8), parameter :: r0_0 = 0.0
    real(REAL8), parameter :: r1_0 = 1.0
    real(REAL8), parameter :: r1eM2 = 1.e-2

    real(REAL8) :: ptk, rgrav
    real(REAL8) :: d3_damp
    real(REAL8), allocatable, dimension(:,:,:) :: delzc, divg, divgd, &
                                           zh, divg3, pk3, pkc, gz, &
                                           du, dv, delpc, ptc, wc
    real(FVPRC), allocatable, dimension(:,:,:) :: ut, vt, crx, cry, xfx, yfx, &
                                           heat_source
contains

!-----------------------------------------------------------------------
!     dyn_core :: FV Lagrangian dynamics driver
!-----------------------------------------------------------------------
 
 subroutine dyn_core(npx, npy, npz, ng, sphum, nq, bdt, n_split, zvir, cp, akap, grav, hydrostatic,  &
                     u,  v,  w, delz, pt, q, delp, pe, pk, phis, omga, ptop, pfull, ua, va, & 
                     uc, vc, mfx, mfy, cx, cy, pkz, peln, ak, bk, init_step, end_step, time_total)
    integer, intent(IN) :: npx
    integer, intent(IN) :: npy
    integer, intent(IN) :: npz
    integer, intent(IN) :: ng, nq, sphum
    integer, intent(IN) :: n_split
    real(REAL8)   , intent(IN) :: bdt
    real(REAL8)   , intent(IN) :: zvir, cp, akap, grav
    real(REAL8)   , intent(IN) :: ptop
    logical, intent(IN) :: hydrostatic
    logical, intent(IN) :: init_step, end_step
    real(REAL8), intent(in) :: pfull(npz)
    real(REAL8), intent(in),     dimension(npz+1) :: ak, bk
    real(REAL8), intent(inout), dimension(isd:ied  ,jsd:jed+1,npz):: u  ! D grid zonal wind (m/s)
    real(REAL8), intent(inout), dimension(isd:ied+1,jsd:jed  ,npz):: v  ! D grid meridional wind (m/s)
    real(REAL8), intent(inout) :: w(   isd:ied  ,jsd:jed  ,npz)  ! vertical vel. (m/s)
    real(REAL8), intent(inout) :: delz(is :ie   ,js :je   ,npz)  ! delta-height (m)
    real(REAL8), intent(inout) :: pt(  isd:ied  ,jsd:jed  ,npz)  ! temperature (K)
    real(REAL8), intent(inout) :: delp(isd:ied  ,jsd:jed  ,npz)  ! pressure thickness (pascal)
    real(REAL8), intent(inout) :: q(   isd:ied  ,jsd:jed  ,npz, nq)  ! 
    real(REAL8), intent(in), optional:: time_total  ! total time (seconds) since start

    integer :: OMP_GET_NUM_THREADS
!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real(REAL8), intent(inout):: phis(isd:ied,jsd:jed)      ! Surface geopotential (g*Z_surf)
    real(REAL8), intent(inout):: pe(is-1:ie+1, npz+1,js-1:je+1)  ! edge pressure (pascal)
    real(REAL8), intent(out) :: peln(is:ie,npz+1,js:je)          ! ln(pe)
    real(REAL8), intent(inout):: pk(is:ie,js:je, npz+1)        ! pe**kappa

!-----------------------------------------------------------------------
! Others:
    integer, parameter:: n_damp = 2
    real(REAL8),    parameter:: near0 = 1.E-9
    real(REAL8),    parameter:: huge_r = 1.E15
    real(REAL8),    parameter:: air_viscosity = 1.E-5   ! [m**2/sec] for T ~ 260 K
!-----------------------------------------------------------------------
    real(FVPRC), intent(inout):: omga(isd:ied,jsd:jed,npz)    ! Vertical pressure velocity (pa/s)
    real(FVPRC), intent(inout):: uc(isd:ied+1,jsd:jed  ,npz)  ! (uc, vc) are mostly used as the C grid winds
    real(FVPRC), intent(inout):: vc(isd:ied  ,jsd:jed+1,npz)
    real(FVPRC), intent(inout), dimension(isd:ied,jsd:jed,npz):: ua, va

! The Flux capacitors: accumulated Mass flux arrays
    real(FVPRC), intent(inout)::  mfx(is:ie+1, js:je,   npz)
    real(FVPRC), intent(inout)::  mfy(is:ie  , js:je+1, npz)
! Accumulated Courant number arrays
    real(FVPRC), intent(inout)::  cx(is:ie+1, jsd:jed, npz)
    real(FVPRC), intent(inout)::  cy(isd:ied ,js:je+1, npz)
! Work:
    real(REAL8), intent(inout):: pkz(is:ie,js:je,npz)  ! 

! Auto 1D & 2D arrays:
    real(REAL8):: zs(is:ie,js:je)        ! surface height (m)
    real(REAL8) wbuffer(npy+2,npz)
    real(REAL8) ebuffer(npy+2,npz)
    real(REAL8) nbuffer(npx+2,npz)
    real(REAL8) sbuffer(npx+2,npz)
! ----   For external mode:
    real(REAL8) divg2(is:ie+1,js:je+1)
    real(REAL8) wk(isd:ied,jsd:jed)
    real(REAL8) fz(is: ie+1,js: je+1)
    real(REAL8) heat_s(is:ie,js:je)
    real(REAL8) damp_vt(npz)
!-------------------------------------
!
    integer :: nord_v(npz)
    integer :: hord_m, hord_v, hord_t, hord_p, nord_k
    integer :: i,j,k, it, iq, n_con, status
    integer :: iep1, jep1
    real(REAL8)    :: beta_c, beta_d, alpha, damp_k, d_con_k
    real(REAL8)    :: dt, dt2, rdt, t1, t2
    real(REAL8)    :: d2_divg, dd_divg, d3_divg
    real(REAL8)    :: diff_z0, k1k, kapag, tmcp
    logical :: last_step

    ptk  = ptop ** akap
    dt   = bdt / real(n_split)
    dt2  = 0.5*dt
    rdt  = 1.0/dt
    rgrav = 1.0/grav

      k1k =  akap / (1.-akap)    ! rg/Cv=0.4
    kapag = -akap / grav

! New 3D form: 10-min time-scale
     d3_divg = kd3 * (dt/600.)
     d3_damp = d3_divg * da_min_c
     diff_z0 = 0.25 * scale_z**2
! Indexes:
      iep1 = ie + 1
      jep1 = je + 1
                                 call timing_on('COMM_TOTAL')
      t1 = MPI_Wtime(status)
      if ( npz>1 )   &
      call mpp_update_domains(  pt, domain, complete=.false.)
      call mpp_update_domains(delp, domain, complete=.true.)
      t2 = MPI_Wtime(status)
      comm_timer = comm_timer + (t2-t1)
                                 call timing_off('COMM_TOTAL')

      if ( init_step ) then  ! Start of the big dynamic time stepping

           allocate(    gz(isd:ied  ,jsd:jed  ,npz+1) )
           allocate(   pkc(isd:ied  ,jsd:jed  ,npz+1) )
           allocate(   ptc(isd:ied  ,jsd:jed  ,npz  ) )
           allocate( delzc(is :ie   ,js:je    ,npz  ) )
           allocate(   crx(is :ie+1 ,jsd:jed  ,npz  ) )
           allocate(   xfx(is :ie+1 ,jsd:jed  ,npz  ) )
           allocate(   cry(isd:ied  ,js :je+1 ,npz  ) )
           allocate(   yfx(isd:ied  ,js :je+1 ,npz  ) )
           allocate(  divg(isd:ied  ,jsd:jed  ,npz  ) )
           allocate( divgd(isd:ied+1,jsd:jed+1,npz  ) )
           allocate( delpc(isd:ied  ,jsd:jed  ,npz  ) )
           allocate(    wc(isd:ied  ,jsd:jed  ,npz  ) )

           allocate( ut(isd:ied, jsd:jed, npz) )
                     call init_ijk_mem(isd,ied, jsd,jed, npz, ut, r0_0)
           allocate( vt(isd:ied, jsd:jed, npz) )
                     call init_ijk_mem(isd,ied, jsd,jed, npz, vt, r0_0)

          if ( .not. hydrostatic ) then
               allocate( zh(isd:ied, jsd:jed, npz) )
               call init_ijk_mem(isd,ied, jsd,jed, npz, zh, huge_r )
               allocate ( pk3(isd:ied,jsd:jed,npz+1) )
               if( kd3 > near0 ) then 
                   allocate( divg3(isd:ied,jsd:jed,npz+1) )
                   call init_ijk_mem(isd,ied, jsd,jed, npz+1, divg3, huge_r )
               endif
          endif
          allocate( du(isd:ied,  jsd:jed+1,npz) )
                    call init_ijk_mem(isd,ied,   jsd,jed+1, npz, du, huge_r)
          allocate( dv(isd:ied+1,jsd:jed,  npz) )
                    call init_ijk_mem(isd,ied+1, jsd,jed  , npz, dv, huge_r)
      endif

      if ( beta < 1.e-5 ) beta = 0.

      if ( .not. reset_beta .and. beta > 0.  ) then
            if ( hydrostatic ) then
               call geopk(ptop, pe, peln, delp, pkc, gz, phis, pt, pkz, npz, akap, .false.)
            elseif( m_grad_p==0 ) then
!$omp parallel do default(shared)
              do j=js,je
                 do i=is,ie
                    gz(i,j,npz+1) = phis(i,j)
                 enddo
                 do k=npz,1,-1
                    do i=is,ie
                       gz(i,j,k) = gz(i,j,k+1) - delz(i,j,k)*grav
                    enddo
                 enddo
              enddo
!$omp parallel do default(shared)
              do k=npz,1,-1
                 do j=js,je
                    do i=is,ie
                       pkc(i,j,k) = pk(i,j,k)
                    enddo
                 enddo
              enddo
              t1 = MPI_Wtime(status)
              call mpp_update_domains( pkc, domain, complete=.false. )
              call mpp_update_domains( gz,  domain, complete=.true.  ) 
              t2 = MPI_Wtime(status)
              comm_timer = comm_timer + (t2-t1)
            endif        ! end hydrostatic test
            call grad1_p(du, dv, pkc, gz, delp, dt, ng, npx, npy, npz, ptop, .true.)
      endif

! Empty the "flux capacitors"
!   call init_ijk_mem(is, ie+1, js,  je,   npz, mfx, r0_0)
!   call init_ijk_mem(is, ie  , js,  je+1, npz, mfy, r0_0)
!   call init_ijk_mem(is, ie+1, jsd, jed,  npz, cx, r0_0)
!   call init_ijk_mem(isd, ied, js,  je+1, npz, cy, r0_0)

  if ( d_con > 1.0E-5 ) then
       allocate( heat_source(isd:ied, jsd:jed, npz) )
       call init_ijk_mem(isd, ied, jsd, jed, npz, heat_source, r0_0)
  endif

  if ( .not. hydrostatic ) then
      do j=js,je
         do i=is,ie
            zs(i,j) = phis(i,j) * rgrav
         enddo
      enddo
  endif

  if ( convert_ke .or. vtdm4> 1.E-3 ) then
       n_con = npz
  else
       n_con = n_damp
  endif


!-----------------------------------------------------
  do it=1,n_split
!-----------------------------------------------------

     if ( it==1 .and. reset_beta ) then
          beta_c = 0.
          beta_d = 0.
     else
          beta_c = beta
          beta_d = beta
     endif

     if ( it==n_split .and. end_step ) then
          last_step = .true.
     else
          last_step = .false.
     endif
                                                  call timing_on('COMM_TOTAL')
     t1 = MPI_Wtime(status)
     call mpp_update_domains(u, v, domain, gridtype=DGRID_NE, complete=.true.)
     t2 = MPI_Wtime(status)
     comm_timer = comm_timer + (t2-t1)
                                                 call timing_off('COMM_TOTAL')
     if ( .not. hydrostatic ) then

!$omp parallel do default(shared)
        do j=js,je
           do i=is,ie
              zh(i,j,npz) = zs(i,j) - delz(i,j,npz)
           enddo
           do k=npz-1,1,-1
              do i=is,ie
                 zh(i,j,k) = zh(i,j,k+1) - delz(i,j,k)
              enddo
           enddo
        enddo
                                 call timing_on('COMM_TOTAL')
        t1 = MPI_Wtime(status)
        call mpp_update_domains(zh, domain, complete=.false.)
        call mpp_update_domains(w,  domain, complete=.true.)
        t2 = MPI_Wtime(status)
        comm_timer = comm_timer + (t2-t1)
                                call timing_off('COMM_TOTAL')
     endif

      if (test_case>1) then
      if (shallow_water .and. test_case==9) call case9_forcing1(phis, time_total)

      beta_c = 0.    ! this truns off the c-grid part of the time-split
      if ( beta_c > 1.E-5 ) then
           call pre_cg(akap, ptop, beta_c, dt2, npz, phis, delp, pt, pkc, gz,    &
                       u, v, uc, vc, ua, va, ut, vt, hydrostatic)
           call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE, complete=.true.)
      endif
                                                     call timing_on('c_sw')
!$omp parallel do default(shared)
      do k=1,npz
         call c_sw(delpc(isd,jsd,k), delp(isd,jsd,k),  ptc(isd,jsd,k),  &
                      pt(isd,jsd,k),    u(isd,jsd,k),    v(isd,jsd,k),  &
                       w(isd,jsd,k),   uc(isd,jsd,k),   vc(isd,jsd,k),  &
                      ua(isd,jsd,k),   va(isd,jsd,k),   wc(isd,jsd,k),  &
                      ut(isd,jsd,k),   vt(isd,jsd,k), dt2, hydrostatic, .true., beta_c)
      enddo
                                                     call timing_off('c_sw')

      if( fill_dp ) call mix_dp(hydrostatic, wc, delpc, ptc, npz, ak, bk, .true.)

      if ( hydrostatic ) then
           call geopk(ptop, pe, peln, delpc, pkc, gz, phis, ptc, pkz, npz, akap, .true.)
      else
                                               call timing_on('UPDATE_DZ')
           call update_dz_c(is, ie, js, je, npz, ng, zs, zh, ut, vt, delzc, gz)
                                               call timing_off('UPDATE_DZ')

                                               call timing_on('Riem_Solver')
           call Riem_Solver_C( dt2,   is,  ie,   js,   je,   npz,   ng,   &
                               akap,  cp,  ptop, phis, wc, delzc, ptc,  &
                               delpc, gz,  pkc,  1 )
! pkc is full non-hydro pressure
                                               call timing_off('Riem_Solver')
                                               call timing_on('COMM_TOTAL')
           t1 = MPI_Wtime(status)
           if ( square_domain ) then
! The following works only for squre domain decomposition:
             call mpp_update_domains(pkc,domain,whalo=1,ehalo=1,shalo=1, nhalo=1, complete=.false.)
             call mpp_update_domains(gz ,domain,whalo=1,ehalo=1,shalo=1, nhalo=1, complete=.true.)
           else   
! Wasteful communication: 3 times larger than needed
             call mpp_update_domains(pkc, domain, complete=.false.)
             call mpp_update_domains(gz , domain, complete=.true.)
           endif
           t2 = MPI_Wtime(status)
           comm_timer = comm_timer + (t2-t1)
                                               call timing_off('COMM_TOTAL')
      endif   ! end hydro check

      call p_grad_c(beta_c, dt2, npz, delpc, pkc, gz, uc, vc, hydrostatic)


                                                     call timing_on('COMM_TOTAL')
!--------------------------------------------------------------------------------------------
!     call mpp_get_boundary(uc, vc, domain, wbufferx=wbuffer, ebufferx=ebuffer, &
!                         sbuffery=sbuffer, nbuffery=nbuffer, gridtype=CGRID_NE )
!     uc(ie+1,js:je,1:npz) = ebuffer
!     vc(is:ie,je+1,1:npz) = nbuffer
!--------------------------------------------------------------------------------------------

      t1 = MPI_Wtime(status)
      call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE, complete=.true.)
      t2 = MPI_Wtime(status)
      comm_timer = comm_timer + (t2-t1)
                                                     call timing_off('COMM_TOTAL')

      if ( fv_debug ) then
         if ( master ) write(*,*) 'istep=', it
         call prt_maxmin('DELPC', delpc, is, ie, js, je, ng, npz, r1eM2, master)
         call prt_maxmin('   UC', DBLE(uc), is, ie+1, js, je, ng, npz, r1_0, master)
         call prt_maxmin('   VC', DBLE(vc), is, ie, js, je+1, ng, npz, r1_0, master)
      endif

      if (shallow_water .and. test_case==9) call case9_forcing2(phis)
      endif !test_case>1

    if ( inline_q ) then
                                 call timing_on('COMM_TOTAL')
         call mpp_update_domains( q, domain, complete=.true.)
                                call timing_off('COMM_TOTAL')
    endif
    if ( nord>0 ) then
! Note: (ua,va) are required as input
         call divergence_corner(u, v, ua, va, divgd, npz)
                                     call timing_on('COMM_TOTAL')
         call mpp_update_domains(divgd, domain, position=CORNER)
                                    call timing_off('COMM_TOTAL')
    endif

! Partial update of [u, v]
     if ( beta_d > 0. ) then
!-------------------------
! Hydrostatic contribution
!-------------------------
!$omp parallel do default(shared)
       do k=1,npz
          do j=js,je+1
             do i=is,ie
                u(i,j,k) = u(i,j,k) + beta_d*rdx(i,j)*du(i,j,k)
             enddo
          enddo
          do j=js,je
             do i=is,ie+1
                v(i,j,k) = v(i,j,k) + beta_d*rdy(i,j)*dv(i,j,k)
             enddo
          enddo
       enddo
                                                    call timing_on('COMM_TOTAL')
       t1 = MPI_Wtime(status)
       call mpp_update_domains(u, v, domain, gridtype=DGRID_NE, complete=.true.)
       t2 = MPI_Wtime(status)
       comm_timer = comm_timer + (t2-t1)
                                                   call timing_off('COMM_TOTAL')
     endif

                                                     call timing_on('d_sw')
!$omp parallel do default(shared) private(nord_k, damp_k, d2_divg, dd_divg, hord_m, hord_v, hord_t, hord_p, wk, heat_s, d_con_k)
    do k=1,npz
       hord_m = hord_mt
       hord_t = hord_tm
       hord_v = hord_vt
       hord_p = hord_dp
       nord_k = nord
       nord_v(k) = min(2, nord)
       damp_k = dddmp
       d2_divg = min(0.20, d2_bg*(1.-3.*tanh(0.1*log(pfull(k)/pfull(npz)))))
       dd_divg = d4_bg
       d_con_k = d_con
       if ( do_vort_damp ) then
            damp_vt(k) = vtdm4
       else
            damp_vt(k) = 0.
       endif
       if ( npz==1 .or. n_sponge<0 ) then
           d2_divg = d2_bg
       elseif ( n_sponge==0 ) then
              if ( k==1 ) then
! Sponge layer:
                   damp_k = 0.
                   nord_k = 0
                   d2_divg = max(0.21-d3_divg, d2_bg, 0.05)
                   nord_v(k) = 0
                   damp_vt(k) = d2_bg_k1
              elseif ( k==2 ) then
                   damp_k = 0.
                   nord_k = 0
!                  d2_divg = max(0.05, d2_bg)
! Nov 9, 2010:
                   d2_divg = min(0.21, max(1.25*d2_bg_k2, d2_bg, 0.01))
                   nord_v(k) = 0
                   damp_vt(k) = d2_bg_k2
              endif

              if ( damp_vt(k) < 0.01 .and. nord_k>0 ) d_con_k = 0.
              if ( nord_v(k)==0 .and. damp_vt(k)>0.01 ) then
                   hord_t = 6
                   hord_v = 6
              endif
       else
           if( k <= n_sponge .and. npz>16 ) then
! Apply first order scheme for damping the sponge layer
               hord_m = 1
               hord_v = 1
               hord_t = 1
               hord_p = 1
               nord_k = 0
               damp_k = damp_k_k1
               d2_divg = min(0.20, d2_bg_k1*d2_bg)   ! 0.25 is the stability limit
               d2_divg = max(d2_divg_max_k1, d2_divg)
           elseif( k == n_sponge+1 .and. npz>24 ) then
               hord_v = 2
               hord_t = 2
               hord_p = 2
               nord_k = max(0, nord-1)
               d2_divg = min(0.20, d2_bg_k2*d2_bg)
               d2_divg = max(d2_divg_max_k2, d2_divg)
               if ( nord > 1 ) then
                    damp_k = 0.
               else
                    damp_k = damp_k_k2
               endif
           endif
       endif
       damp_k = max(damp_k, dddmp)

!--- external mode divergence damping ---
       if ( d_ext > 0. )  &
            call a2b_ord2(delp(isd,jsd,k), wk, npx, npy, is,    &
                          ie, js, je, ng, .false.)

       call d_sw(divg(isd,jsd,k), delp(isd,jsd,k),  pt(isd,jsd,k),      &
                  u(isd,jsd,k),    v(isd,jsd,k),   w(isd,jsd,k),  uc(isd,jsd,k),      &
                  vc(isd,jsd,k),   ua(isd,jsd,k),  va(isd,jsd,k), divgd(isd,jsd,k),   &
                  mfx(is, js, k),  mfy(is, js, k),  cx(is, jsd,k),  cy(isd,js, k),    &
                  crx(is, jsd,k),  cry(isd,js, k), xfx(is, jsd,k), yfx(isd,js, k),    &
                  heat_s, zvir, sphum, nq, q, k, npz, inline_q, dt,     &
                  hord_tr, hord_m, hord_v, hord_t, hord_p, nord_k, nord_v(k), damp_k,    &
                  d2_divg, dd_divg, damp_vt(k), d_con_k, hydrostatic)

     if (.not. shallow_water) then
       if ( d_ext > 0. ) then
            do j=js,jep1
               do i=is,iep1
                  ptc(i,j,k) = wk(i,j)    ! delp at cell corners
               enddo
            enddo
       endif
       if( kd3 > near0 .or. last_step ) then
! Average horizontal "convergence" to cell center
            do j=js,je
               do i=is,ie
                  omga(i,j,k) = 0.25*(divg(i,j,k)+divg(i+1,j,k)+divg(i,j+1,k)+divg(i+1,j+1,k))*delp(i,j,k)
               enddo
            enddo
       endif
       if ( d_con_k > 1.0E-5 ) then
! Average horizontal "convergence" to cell center
            do j=js,je
               do i=is,ie
                  heat_source(i,j,k) = heat_source(i,j,k) + heat_s(i,j)
               enddo
            enddo
       endif
     endif ! .not. shallow_water
    enddo         
                                                     call timing_off('d_sw')

    if( fill_dp ) call mix_dp(hydrostatic, w, delp, pt, npz, ak, bk, .false.)

    if ( d_ext > 0. ) then
          d2_divg = d_ext * da_min_c
! divg() is 3D field of horizontal convergence
! ptc is "delp" at cell corners

!$omp parallel do default(shared)
          do j=js,jep1
              do i=is,iep1
                    wk(i,j) = ptc(i,j,1)
                 divg2(i,j) = wk(i,j)*divg(i,j,1)
              enddo
              do k=2,npz
                 do i=is,iep1
                       wk(i,j) =    wk(i,j) + ptc(i,j,k)
                    divg2(i,j) = divg2(i,j) + ptc(i,j,k)*divg(i,j,k)
                 enddo
              enddo
              do i=is,iep1
                 divg2(i,j) = d2_divg*divg2(i,j)/wk(i,j)
              enddo
          enddo
    else
        divg2 = 0.
    endif


                               call timing_on('COMM_TOTAL')
    t1 = MPI_Wtime(status)
    if ( npz>1 )   &
    call mpp_update_domains(  pt, domain, complete=.false.)
    call mpp_update_domains(delp, domain, complete=.true.)
    t2 = MPI_Wtime(status)
    comm_timer = comm_timer + (t2-t1)
                             call timing_off('COMM_TOTAL')

   if (.not. shallow_water) then

     if ( hydrostatic ) then
          call geopk(ptop, pe, peln, delp, pkc, gz, phis, pt, pkz, npz, akap, .false.)

     else
        call compute_pvar(ptop, akap, npz, 0, delp, pk, peln) 
                                            call timing_on('UPDATE_DZ')
        call update_dz_d(nord_v, damp_vt, hord_tm, is, ie, js, je, npz, ng, npx, npy, &
                         zs, zh, crx, cry, xfx, yfx, delz)
                                            call timing_off('UPDATE_DZ')
        if(fill_wz) call mix_wz(w, delp, delz, pt, pk, 0, npz, delzc, last_step)

        if ( fv_debug ) then
            if ( master ) write(*,*) 'Before Riem_Solver'
            call prt_maxmin('W', w, is, ie, js, je, ng, npz, r1_0, master)
            call prt_maxmin('DELZ', delz, is, ie, js, je, 0, npz, r1_0, master)
        endif
                                                          call timing_on('Riem_Solver')
!-----------------------------------------------------------
! mgrad_p = 1: pkc is full pressure
! mgrad_p = 0: pkc is non-hydrostatic perturbation pressure
!-----------------------------------------------------------
          call Riem_Solver(dt,   is,   ie,   js,   je, npz,  ng,  &
                           akap, cp,   ptop, phis, w,  delz,      &
                           pt,   delp, gz,   pkc,  pk, pe, peln, it==1, it==n_split, m_grad_p)
                                                 call timing_off('Riem_Solver')

                                       call timing_on('COMM_TOTAL')
          if ( fv_debug ) then
              if ( master ) write(*,*) 'After Riem_Solver'
              call prt_maxmin('W', w, is, ie, js, je, ng, npz, r1_0, master)
              call prt_maxmin('DELZ', delz, is, ie, js, je, 0, npz, r1_0, master)
          endif

          if ( m_grad_p==0 ) then
!$omp parallel do default(shared)
             do k=1,npz+1
                do j=js,je
                   do i=is,ie
                      pk3(i,j,k) = pk(i,j,k)
                   enddo
                enddo
             enddo
             call mpp_update_domains(pk3, domain, complete=.false.)
          endif
          t1 = MPI_Wtime(status)
          call mpp_update_domains(pkc, domain, complete=.false.)
          call mpp_update_domains(gz , domain, complete=.true.)
          t2 = MPI_Wtime(status)
          comm_timer = comm_timer + (t2-t1)
                                       call timing_off('COMM_TOTAL')


        if( kd3 > near0 ) then
             call timing_on('3Divg_Damping')

             if ( convert_ke ) then
!$omp parallel do default(shared)
                  do k=1,npz
                     do j=js,je
                        do i=is,ie
                           zh(i,j,k) = w(i,j,k)
                        enddo
                     enddo
                  enddo
             endif

! horizontal component (D2) for w part of the damping
!$omp parallel do default(shared)
              do k=2,npz
                 do j=js,je
                    do i=is,ie
                       divg3(i,j,k) = (omga(i,j,k-1)+omga(i,j,k))/(delp(i,j,k-1)+delp(i,j,k))
                    enddo
                 enddo
              enddo

!--- Top & Bot BC:
!$omp parallel do default(shared)
              do j=js,je
                 do i=is,ie
                    divg3(i,j,    1) = omga(i,j,1)/delp(i,j,1)
! D2 at lower surface is assumed to be zero; therefore, D3 at surface is entirely
! due to dw/dz, which is 2*W(bot)/dz(bot) [w(bot)==0]
!                   divg3(i,j,npz+1) = 0.
! div2 is assumed to be vertically uniform within the lowest layer
                    divg3(i,j,npz+1) = omga(i,j,npz)/delp(i,j,npz)
                 enddo
              enddo

!$omp parallel do default(shared)
             do k=1,npz
                do j=js,je
                   do i=is,ie
! multiply w* by (-delz) as the r.h.s for the solver
                      w(i,j,k) = d3_damp*(divg3(i,j,k+1)-divg3(i,j,k)) - w(i,j,k)*delz(i,j,k)
                   enddo
                enddo
             enddo

! On input w is ws; on output w is the updated vertical velocity
! save w to zh
                                     call timing_on('IMP_SOLVER')
             call imp1_update_w(npz, d3_damp, scale_z, diff_z0, zd_z1, delz, w, zh, delp, convert_ke)
                                     call timing_off('IMP_SOLVER')

!$omp parallel do default(shared)
             do k=2,npz
                do j=js,je
                   do i=is,ie
!                     divg3 is vertical "convergence"
                      divg3(i,j,k) = (w(i,j,k)-w(i,j,k-1)) * grav/(0.5*(gz(i,j,k-1)-gz(i,j,k+1)))
                   enddo
                enddo
             enddo
! Top and bottom BCs for divg3: del(w)/delz = 0 at model top
!$omp parallel do default(shared)
             do j=js,jep1
                do i=is,iep1
                   divg3(i,j,1) = 0.
                enddo
             enddo
!$omp parallel do default(shared)
             do j=js,je
                do i=is,ie
! for 3D divergence computation, w is assumed 0 at the bottom surface
                   divg3(i,j,npz+1) = 2.*w(i,j,npz)*grav/(gz(i,j,npz+1)-gz(i,j,npz))
                enddo
             enddo
                                     call timing_on('COMM_TOTAL')
             call mpp_update_domains(divg3, domain, complete=.true.)
                                     call timing_off('COMM_TOTAL')
          call timing_off('3Divg_Damping')

!-------------------------------------------
! Apply vertical diffusion to w if scale_z>0
!-------------------------------------------
        elseif ( diff_z0 > near0 ) then
! KE fixer?
             call imp_update_w(npz, diff_z0, delz, w)
        endif

        if ( fv_debug ) then
             if ( master ) write(*,*) 'After imp_update_w'
             call prt_maxmin('W', w, is, ie, js, je, ng, npz, r1_0, master)
        endif

     endif    ! end hydro check

   endif !.not. shallow_water


    if (test_case > 1) then

     if (.not. shallow_water) then
      if ( hydrostatic .and. (breed_vortex_inline .or. it==n_split) ) then
!$omp parallel do default(shared)
           do k=1,npz+1
              do j=js,je
                 do i=is,ie
                    pk(i,j,k) = pkc(i,j,k)
                 enddo
              enddo
           enddo
      endif

! Compute omega
      if ( last_step ) then
!$omp parallel do default(shared)
         do j=js,je
            do k=2,npz
               do i=is,ie
                  omga(i,j,k) = omga(i,j,k-1) + omga(i,j,k)
               enddo
            enddo
         enddo
      endif
! Note: for non-hydrostatic and beta/=0 case, the current code works only if m_grad_p==1
     if ( .not.hydrostatic .and. m_grad_p == 0 ) then
! divg:     horizontal "convergence" at cell corners (original)
          call two_grad_p(du, dv, u, v, pkc, gz, delp, pk3, divg2, beta_d, dt, ng, npx, npy, npz)
     else
       if ( hydrostatic .and. beta > 0. ) then
          alpha = 1. - beta_d
          call grad1_p_update(divg2, u, v, du, dv, pkc, gz, delp, dt, ng, npx, npy, npz,      &
                              ptop, .true., alpha)
       else
! Beta == 0
!            i)  Hydrostatic,  OR
!           ii) Non-hydrostatic and m_grad==1
          call one_grad_p(u, v, pkc, gz, divg2, delp, dt, ng, npx, npy, npz, ptop, hydrostatic)
       endif
     endif

     else

!$omp parallel do default(shared)
       do k=1,npz
          do j=js,je+1
             do i=is,ie
                u(i,j,k) = rdx(i,j)*+u(i,j,k)
             enddo
          enddo
          do j=js,je
             do i=is,ie+1
                v(i,j,k) = rdy(i,j)*v(i,j,k)
             enddo
          enddo
       enddo    ! end k-loop

     endif ! .not. shallow_water


    if (.not. shallow_water) then
    if( kd3 > near0 ) then
       call timing_on('3Divg_Damping')
!$omp parallel do default(shared) private(wk)
    do k=2,npz+1
! divg3 was  the vertical "convergence" at half-level (Lagrangian surfaces), cell center
       call a2b_ord2(divg3(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
    enddo

!---------------------------------------------------------
! divgd: full 3D "convergence" at full-level, cell corners
!---------------------------------------------------------
! divg:     horizontal "convergence" at cell corners (original)
!$omp parallel do default(shared)
    do k=1,npz
       do j=js,jep1
          do i=is,iep1
! use zh in place of divgd
             divgd(i,j,k) = divg(i,j,k) + 0.5*(divg3(i,j,k)+divg3(i,j,k+1))
          enddo
       enddo
    enddo

! Compute pk3: full 3D "convergence" at corners, half-level
!$omp parallel do default(shared)
    do k=2,npz
       do j=js,jep1
          do i=is,iep1
             pk3(i,j,k) = 0.5*(divg(i,j,k-1)+divg(i,j,k)) + divg3(i,j,k)
          enddo
       enddo
    enddo
!--- Top & Bot BC
!$omp parallel do default(shared)
    do j=js,jep1
       do i=is,iep1
          pk3(i,j,    1) = divg(i,j,1)
! div2 is assumed zero at the bottom surface
!         pk3(i,j,npz+1) = divg3(i,j,npz+1)
! div2 is assumed to be vertically uniform within the lowest layer
          pk3(i,j,npz+1) = divg3(i,j,npz+1) + divg(i,j,npz)
       enddo
    enddo

!$omp parallel do default(shared)
    do k=1,npz+1
       do j=js,jep1
          do i=is,ie
! pkc: integral bottom/top, east-west oriented
             pkc(i,j,k) = 0.5*(pk3(i,j,k)+pk3(i+1,j,k))*(gz(i+1,j,k)-gz(i,j,k))
          enddo
       enddo
       do j=js,je
          do i=is,iep1
! divg3: bottom/top, n-s oriented
             divg3(i,j,k) = 0.5*(pk3(i,j,k)+pk3(i,j+1,k))*(gz(i,j+1,k)-gz(i,j,k))
          enddo
       enddo
    enddo

!$omp parallel do default(shared) private(divg2, fz)
    do k=1,npz
       do j=js,jep1
          do i=is,iep1
! fz: line integral along vertical walls at corners
#ifndef EDGE_FORM
! This form requires less spatial averaging
             divg2(i,j) = gz(i,j,k) - gz(i,j,k+1)       ! g*dz > 0
                fz(i,j) = divgd(i,j,k)*divg2(i,j)
             divg2(i,j) = 0.5*divg2(i,j)
#else
! Uses edges:
             divg2(i,j) = 0.5*(gz(i,j,k) - gz(i,j,k+1))       ! g*dz > 0
                fz(i,j) = (pk3(i,j,k)+pk3(i,j,k+1))*divg2(i,j)
#endif
          enddo
       enddo

!    if ( convert_ke ) then
!         0.5 * [ (u+du)**2 - u**2 ] = u*du + 0.5*du*du = du*(u+0.5*du)
!    else
!      d3_damp =  kd3 * da_min_c
       do j=js,jep1
          do i=is,ie
             u(i,j,k) = u(i,j,k) + d3_damp*( fz(i,j)-fz(i+1,j)+pkc(i,j,k)-pkc(i,j,k+1) )   &
                                 / ( dx(i,j)*(divg2(i,j)+divg2(i+1,j)) )
          enddo
       enddo
       do j=js,je
          do i=is,iep1
             v(i,j,k) = v(i,j,k) + d3_damp*( fz(i,j)-fz(i,j+1)+divg3(i,j,k)-divg3(i,j,k+1) )  &
                                 / ( dy(i,j)*(divg2(i,j)+divg2(i,j+1)) )
          enddo
       enddo
!    endif
    enddo    ! end k-loop
    call timing_off('3Divg_Damping')
    endif
!-------------------------------------------------------------------------------------------------------
#ifndef MAPL_MODE
      if ( breed_vortex_inline ) then
           call breed_slp_inline( it, dt, npz, ak, bk, phis, pe, pk, peln, pkz,     &
                                  delp, u, v, pt, q, nwat, zvir )
      endif
#endif
!-------------------------------------------------------------------------------------------------------
     endif ! .not. shallow_water

                                                                call timing_on('COMM_TOTAL')
      if( it==n_split .and. grid_type<4 ) then
! Prevent accumulation of rounding errors at overlapped domain edges:
          call mpp_get_boundary(u, v, domain, wbuffery=wbuffer, ebuffery=ebuffer,  &
                            sbufferx=sbuffer, nbufferx=nbuffer, gridtype=DGRID_NE )
#ifdef TEST_BUFF
          u(is:ie,je+1,1:npz) = nbuffer
          v(ie+1,js:je,1:npz) = ebuffer
#else
!$omp parallel do default(shared)
          do k=1,npz
             do i=is,ie
                u(i,je+1,k) = nbuffer(i-is+1,k)
             enddo
          enddo

!$omp parallel do default(shared)
          do k=1,npz
             do j=js,je
                v(ie+1,j,k) = ebuffer(j-js+1,k)
             enddo
          enddo
#endif
      endif
                                                                call timing_off('COMM_TOTAL')
      endif !test_case>1

    if ( fv_debug ) then
         if ( master ) write(*,*) 'istep=', it
         call prt_maxmin('DELP', delp, is, ie, js, je, ng, npz, r1eM2, master)
         call prt_maxmin('   U',    u, is, ie, js, je+1, ng, npz, r1_0, master)
         call prt_maxmin('   V',    v, is, ie+1, js, je, ng, npz, r1_0, master)
    endif

!-----------------------------------------------------
  enddo   ! time split loop
!-----------------------------------------------------

  if ( fv_debug ) then
       if(master) write(*,*) 'End of n_split loop'
  endif

  if (.not. shallow_water) then
  if ( d_con > 1.e-5 ) then
       call del2_cubed(heat_source, (0.20*da_min), npx, npy, npz, 3)

! Note: pt here is cp*(Virtual_Temperature/pkz)
    if ( hydrostatic ) then
!
! del(Cp*T) = - del(KE)
!
!$omp parallel do default(shared)
       do k=1,n_con
          do j=js,je
             do i=is,ie
                pt(i,j,k) = pt(i,j,k) + heat_source(i,j,k)/(delp(i,j,k)*pkz(i,j,k))
             enddo
          enddo
       enddo
    else
!$omp parallel do default(shared) private(tmcp)
       do k=1,n_con
          do j=js,je
             do i=is,ie
                pkz(i,j,k) = exp( k1k*log(kapag*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)) )
! tmcp = termperature_v * cp
                tmcp = pt(i,j,k) * pkz(i,j,k)
                delz(i,j,k) = delz(i,j,k) / tmcp
                tmcp = tmcp + heat_source(i,j,k)/delp(i,j,k)
                pt(i,j,k) = tmcp / pkz(i,j,k)
                delz(i,j,k) = delz(i,j,k) * tmcp
             enddo
          enddo
       enddo
    endif
  endif
  endif ! .not. shallow_water

  if ( end_step ) then
    deallocate(    gz )
    deallocate(   ptc )
    deallocate( delzc )
    deallocate(   crx )
    deallocate(   xfx )
    deallocate(   cry )
    deallocate(   yfx )
    deallocate(  divg )
    deallocate( divgd )
    deallocate(   pkc )
    deallocate( delpc )
    deallocate(    wc )

    if( allocated(ut))   deallocate( ut )
    if( allocated(vt))   deallocate( vt )
    if ( allocated (du) ) deallocate( du )
    if ( allocated (dv) ) deallocate( dv )
    if ( .not. hydrostatic ) then
         deallocate( zh )
         if( allocated(pk3) )    deallocate ( pk3 )
         if( allocated(divg3) ) deallocate( divg3 )
    endif

   !if ( d_con > 1.0E-5 ) then
   !   deallocate( heat_source )
   !endif
  endif
  if (allocated(heat_source)) deallocate( heat_source ) !If ncon == 0 but d_con > 1.e-5, this would not be deallocated in earlier versions of the code

  if ( fv_debug ) then
       if(master) write(*,*) 'End of dyn_core'
  endif

 end subroutine dyn_core


 subroutine pre_cg(akap, ptop, beta_c, dt2, npz, hs, delp, pt, pkc, gz,   &
                    u, v, uc, vc, ua, va, ut, vt, hydrostatic)
 real(REAL8),    intent(in):: akap, ptop, beta_c, dt2
 integer, intent(in):: npz
 real(REAL8), intent(in), dimension(isd:ied  ,jsd:jed+1,npz):: u
 real(REAL8), intent(in), dimension(isd:ied+1,jsd:jed  ,npz):: v
 real(REAL8), intent(in), dimension(isd:ied, jsd:jed ,npz):: delp, pt
 logical, intent(in):: hydrostatic
!
 real(REAL8), intent(inout):: hs(isd:ied,jsd:jed)
 real(REAL8), intent(inout), dimension(isd:ied, jsd:jed ,npz+1):: pkc, gz
 real(FVPRC), intent(inout):: uc(isd:ied+1,jsd:jed  ,npz)
 real(FVPRC), intent(inout):: vc(isd:ied  ,jsd:jed+1,npz)
 real(FVPRC), intent(out), dimension(isd:ied, jsd:jed ,npz):: ua, va, ut, vt
! Local:
 real(REAL8):: wk(is-1:ie+1,js-1:je+1)
 real(REAL8), dimension(is-1:ie+1):: p1
 real(REAL8):: beta
 integer:: i,j,k

! Re-compute pkc and gz
!$omp parallel do default(shared) private(p1)
     do j=js-1,je+1
        do i=is-1,ie+1
           gz(i,j,npz+1) = hs(i,j)
           p1(i) = ptop
           pkc(i,j,1) = ptk
        enddo
        do k=2,npz+1
           do i=is-1,ie+1
              p1(i)  = p1(i) + delp(i,j,k-1)
              pkc(i,j,k) = exp( akap*log(p1(i)) )
           enddo
        enddo
        do k=npz,1,-1
           do i=is-1,ie+1
              gz(i,j,k) = gz(i,j,k+1) + pt(i,j,k)*(pkc(i,j,k+1)-pkc(i,j,k))
           enddo
        enddo
     enddo

      beta = beta_c * dt2

!$omp parallel do default(shared) private(wk)
     do k=1,npz

        call d2a2c_vect(u(isd,jsd,k),  v(isd,jsd,k), ua(isd,jsd,k), va(isd,jsd,k),   &
                       uc(isd,jsd,k), vc(isd,jsd,k), ut(isd,jsd,k), vt(isd,jsd,k), .true.)

        if ( hydrostatic ) then
             do j=js-1,je+1
                do i=is-1,ie+1
                   wk(i,j) = pkc(i,j,k+1) - pkc(i,j,k)
                enddo
             enddo
        else
             do j=js-1,je+1
                do i=is-1,ie+1
                   wk(i,j) = delp(i,j,k)
                enddo
             enddo
        endif

        do j=js,je
           do i=is,ie+1
              uc(i,j,k) = uc(i,j,k) + beta*rdxc(i,j) / (wk(i-1,j)+wk(i,j)) *   &
                   ( (gz(i-1,j,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i-1,j,k))  &
                   + (gz(i-1,j,k) - gz(i,j,k+1))*(pkc(i-1,j,k+1)-pkc(i,j,k)) )
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              vc(i,j,k) = vc(i,j,k) + beta*rdyc(i,j) / (wk(i,j-1)+wk(i,j)) *   &
                   ( (gz(i,j-1,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i,j-1,k))  &
                   + (gz(i,j-1,k) - gz(i,j,k+1))*(pkc(i,j-1,k+1)-pkc(i,j,k)) )
           enddo
        enddo
     enddo

 end subroutine pre_cg


 subroutine p_grad_c(beta_c, dt2, npz, delpc, pkc, gz, uc, vc, hydrostatic)

 integer, intent(in):: npz
 real(REAL8),    intent(in):: beta_c, dt2
 real(REAL8), intent(in), dimension(isd:ied, jsd:jed ,npz  ):: delpc
 real(REAL8), intent(in), dimension(isd:ied, jsd:jed ,npz+1):: pkc, gz
 real(FVPRC), intent(inout):: uc(isd:ied+1,jsd:jed  ,npz)
 real(FVPRC), intent(inout):: vc(isd:ied  ,jsd:jed+1,npz)
 logical, intent(in):: hydrostatic
! Local:
 real(REAL8):: wk(is-1:ie+1,js-1:je+1)
 real(REAL8):: alpha
 integer:: i,j,k

      alpha = dt2 * (1. - beta_c)

!$omp parallel do default(shared) private(wk)
      do k=1,npz

         if ( hydrostatic ) then
              do j=js-1,je+1
                 do i=is-1,ie+1
                    wk(i,j) = pkc(i,j,k+1) - pkc(i,j,k)
                 enddo
              enddo
         else
              do j=js-1,je+1
                 do i=is-1,ie+1
                    wk(i,j) = delpc(i,j,k)
                 enddo
              enddo
         endif

         do j=js,je
            do i=is,ie+1
               uc(i,j,k) = uc(i,j,k) + alpha*rdxc(i,j) / (wk(i-1,j)+wk(i,j)) *   &
                    ( (gz(i-1,j,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i-1,j,k))  &
                    + (gz(i-1,j,k) - gz(i,j,k+1))*(pkc(i-1,j,k+1)-pkc(i,j,k)) )
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               vc(i,j,k) = vc(i,j,k) + alpha*rdyc(i,j) / (wk(i,j-1)+wk(i,j)) *   &
                    ( (gz(i,j-1,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i,j-1,k))  &
                    + (gz(i,j-1,k) - gz(i,j,k+1))*(pkc(i,j-1,k+1)-pkc(i,j,k)) )
            enddo
         enddo
      enddo

 end subroutine p_grad_c


 subroutine compute_pvar(ptop, akap, npz, ng, delp, pkc, peln) 
! Compute peln, pk, and pe?
 integer, intent(in):: npz, ng
 real(REAL8), intent(in ):: ptop, akap
 real(REAL8), intent(in ):: delp(isd:ied,jsd:jed,npz)  ! pressure thickness (pascal)
 real(REAL8), intent(out)::  pkc(is-ng:ie+ng,js-ng:je+ng,npz+1)
 real(REAL8), intent(out):: peln(is:ie,npz+1,js:je)           ! ln(pe)
!
 real(REAL8), dimension(is:ie):: p1d
 integer:: i,j,k

!$omp parallel do default(shared) private(p1d)
     do j=js,je

        do i=is,ie
           p1d(i) = ptop
           pkc(i,j,1) = ptk
        enddo
        do k=2,npz+1
           do i=is,ie
                  p1d(i)  = p1d(i) + delp(i,j,k-1)
              peln(i,k,j) = log(p1d(i))
               pkc(i,j,k) = exp( akap*peln(i,k,j) )
           enddo
        enddo

     enddo

 end subroutine compute_pvar


 subroutine imp1_update_w(km, mu, z0,  c0, c1, delz, w, w0, delp, convert_ke)
! Modifications from imp0 version: w multiplied by delz on input
!                                  vairale (delz) background diffusion
 integer, intent(in) :: km
 real(REAL8), intent(in) :: z0    ! background diffusion length scale (m)
 real(REAL8), intent(in) :: mu    ! 3D divg damping coefficient
 real(REAL8), intent(in) :: c0    ! background diffusion coefficient
 real(REAL8), intent(in) :: c1    ! variable part of the background diffusion
 real(REAL8), intent(in) :: delz(is:ie, js:je, km)  ! delta-height (m)
 real(REAL8), intent(in) :: delp(isd :ied, jsd:jed, km)  ! delta-height (m)
 real(REAL8), intent(inout) ::  w(isd:ied, jsd:jed, km) ! vertical vel. (m/s)
 real(REAL8), intent(inout) :: w0(isd:ied, jsd:jed, km) ! old w
 logical, intent(in):: convert_ke
! Local:
 real(REAL8), dimension(is:ie,km):: c, gam, dz, wt, dzn
 real(REAL8):: bet(is:ie)
 real(REAL8):: a, cd, cd1
 integer:: i, j, k

 cd  = mu + c0
 cd1 = c1 * z0

!$omp parallel do default(shared) private(bet, gam, dz, wt, a, c, dzn)
  do j=js,je

     do k=1,km
        do i=is,ie
           dzn(i,k) = -delz(i,j,k)       ! dzn > 0
        enddo
     enddo

     do k=2,km
        do i=is,ie
           dz(i,k) = 0.5*(dzn(i,k-1)+dzn(i,k))
        enddo
     enddo
     do k=1,km-1
        do i=is,ie
           c(i,k) = -(cd1 + cd/dz(i,k+1))
        enddo
     enddo

! model top:
     do i=is,ie
         bet(i) = dzn(i,1) - c(i,1)      ! bet(i) = b
        wt(i,1) = w(i,j,1) / bet(i)
     enddo

! Interior:
     do k=2,km-1
        do i=is,ie
           gam(i,k) = c(i,k-1) / bet(i)
! Note a is actually (-a)
                  a = cd1 + cd/dz(i,k)
             bet(i) = dzn(i,k) - c(i,k) + a + a*gam(i,k)
            wt(i,k) = (w(i,j,k) + a*wt(i,k-1)) / bet(i)
        enddo
     enddo

! Bottom: k = km
     do i=is,ie
        gam(i,km) = c(i,km-1) / bet(i)
! Note a is actually (-a)
         a = cd1 + cd/dz(i,km)
         wt(i,km) = (w(i,j,km)+a*wt(i,km-1))/(dzn(i,k)+a*(3.+gam(i,km)))
     enddo
 
     do k=km-1,1,-1
        do i=is,ie
           wt(i,k) = wt(i,k) - gam(i,k+1)*wt(i,k+1)
        enddo
     enddo

     do k=1,km
        do i=is,ie
           w(i,j,k) = wt(i,k)
        enddo
     enddo

     if ( convert_ke ) then
       do k=1,km
          do i=is,ie
             heat_source(i,j,k) = heat_source(i,j,k) + delp(i,j,k)*0.5*(w0(i,j,k)**2 - wt(i,k)**2)
          enddo
       enddo
     endif

  enddo

 end subroutine imp1_update_w


 subroutine imp_update_w(km, cd, delz, w)
 integer, intent(in) :: km
 real(REAL8), intent(in) :: cd
 real(REAL8), intent(in) :: delz(is :ie , js :je , km)  ! delta-height (m)
 real(REAL8), intent(inout) :: w(isd:ied, jsd:jed, km)  ! vertical vel. (m/s)
! Local:
 real(REAL8), dimension(is:ie,km):: c, gam, dz, wt
 real(REAL8):: bet(is:ie)
 real(REAL8):: a
 integer:: i, j, k

!$omp parallel do default(shared) private(bet, gam, dz, wt, a, c)
  do j=js,je

     do k=2,km
        do i=is,ie
           dz(i,k) = 0.5*(delz(i,j,k-1)+delz(i,j,k))
        enddo
     enddo
     do k=1,km-1
        do i=is,ie
           c(i,k) = -cd/(dz(i,k+1)*delz(i,j,k))
        enddo
     enddo

! model top:
     do i=is,ie
         bet(i) = 1. - c(i,1)      ! bet(i) = b
        wt(i,1) = w(i,j,1) / bet(i)
     enddo

! Interior:
     do k=2,km-1
        do i=is,ie
           gam(i,k) = c(i,k-1)/bet(i)
                  a = cd/(dz(i,k)*delz(i,j,k))
             bet(i) = (1.+a-c(i,k)) + a*gam(i,k)
            wt(i,k) = (w(i,j,k) + a*wt(i,k-1)) / bet(i)
        enddo
     enddo

! Bottom: k = km
     do i=is,ie
        gam(i,km) = c(i,km-1) / bet(i)
                a = cd/(dz(i,km)*delz(i,j,km))
         wt(i,km) = (w(i,j,km) + a*wt(i,km-1))/(1. + a + (cd+cd)/delz(i,j,km)**2 + a*gam(i,km))
     enddo
 
     do k=km-1,1,-1
        do i=is,ie
           wt(i,k) = wt(i,k) - gam(i,k+1)*wt(i,k+1)
        enddo
     enddo

     do k=1,km
        do i=is,ie
           w(i,j,k) = wt(i,k)
        enddo
     enddo
  enddo

 end subroutine imp_update_w


 subroutine two_grad_p(du, dv, u, v, pp, gh, delp, pk, divg2, beta, dt, ng, npx, npy, npz)
    integer, intent(IN) :: ng, npx, npy, npz
    real(REAL8),    intent(IN) :: beta
    real(REAL8),    intent(IN) :: dt
    real(REAL8),    intent(in) :: divg2(is:ie+1, js:je+1)
    real(REAL8), intent(inout) ::  delp(isd:ied, jsd:jed, npz)
    real(REAL8), intent(inout) ::    pp(isd:ied, jsd:jed, npz+1)  ! perturbation pressure
    real(REAL8), intent(inout) ::    pk(isd:ied, jsd:jed, npz+1)  ! p**kappa
    real(REAL8), intent(inout) ::    gh(isd:ied, jsd:jed, npz+1)  ! g * h
    real(REAL8), intent(inout) ::    du(isd:ied  ,jsd:jed+1,npz) 
    real(REAL8), intent(inout) ::    dv(isd:ied+1,jsd:jed  ,npz)
    real(REAL8), intent(inout) ::     u(isd:ied,  jsd:jed+1,npz) 
    real(REAL8), intent(inout) ::     v(isd:ied+1,jsd:jed,  npz)
! Local:
    real(REAL8) wk1(isd:ied, jsd:jed)
    real(REAL8)  wk(is: ie+1,js: je+1)
    real(REAL8)  alpha
    integer iep1, jep1
    integer i,j,k

    alpha = 1. - beta

    iep1 = ie + 1
    jep1 = je + 1

!$omp parallel do default(shared)
    do j=js,jep1
       do i=is,iep1
          pp(i,j,1) = 0.
          pk(i,j,1) = ptk
       enddo
    enddo

!$omp parallel do default(shared) private(wk1)
    do k=1,npz+1

       if ( k/=1 ) then
         if ( a2b_ord==4 ) then
           call a2b_ord4(pp(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
           call a2b_ord4(pk(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
         else
           call a2b_ord2(pp(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
           call a2b_ord2(pk(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
         endif
       endif

       if ( a2b_ord==4 ) then
           call a2b_ord4( gh(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
       else
           call a2b_ord2( gh(isd,jsd,k), wk1, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk1, wk)
    do k=1,npz

       wk1 = delp(:,:,k)
       if ( a2b_ord==4 ) then
            call a2b_ord4(wk1, wk1, npx, npy, is, ie, js, je, ng)
       else
            call a2b_ord2(wk1, wk1, npx, npy, is, ie, js, je, ng)
       endif

       do j=js,jep1
          do i=is,iep1
             wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
          enddo
       enddo

       do j=js,jep1
          do i=is,ie
! hydrostatic contributions from past time-step already added in the "beta" part
! Current gradient from "hydrostatic" components:
            du(i,j,k) =  dt / (wk(i,j)+wk(i+1,j)) *   &
                        ((gh(i,j,k+1)-gh(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) +  &
                         (gh(i,j,k)-gh(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)))
! Non-hydrostatic contribution:
             u(i,j,k) = (u(i,j,k) + divg2(i,j)-divg2(i+1,j) + alpha*du(i,j,k) + dt/(wk1(i,j)+wk1(i+1,j)) *  &
                       ((gh(i,j,k+1)-gh(i+1,j,k))*(pp(i+1,j,k+1)-pp(i,j,k))    &
                      + (gh(i,j,k)-gh(i+1,j,k+1))*(pp(i,j,k+1)-pp(i+1,j,k))))*rdx(i,j)
          enddo
       enddo
       do j=js,je
          do i=is,iep1
! Current gradient from "hydrostatic" components:
            dv(i,j,k) = dt / (wk(i,j)+wk(i,j+1)) *   &
                        ((gh(i,j,k+1)-gh(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) +  &
                         (gh(i,j,k)-gh(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
! Non-hydrostatic contribution:
             v(i,j,k) = (v(i,j,k)  + divg2(i,j)-divg2(i,j+1) + alpha*dv(i,j,k) + dt/(wk1(i,j)+wk1(i,j+1)) *  &
                       ((gh(i,j,k+1)-gh(i,j+1,k))*(pp(i,j+1,k+1)-pp(i,j,k))   &
                      + (gh(i,j,k)-gh(i,j+1,k+1))*(pp(i,j,k+1)-pp(i,j+1,k))))*rdy(i,j)
          enddo
       enddo

    enddo    ! end k-loop


 end subroutine two_grad_p



 subroutine one_grad_p(u, v, pk, gh, divg2, delp, dt, ng, npx, npy, npz,  &
                       ptop, hydrostatic)  

    integer, intent(IN) :: ng, npx, npy, npz
    real(REAL8),    intent(IN) :: dt, ptop
    logical, intent(in) :: hydrostatic
    real(REAL8),    intent(in) :: divg2(is:ie+1,js:je+1)
    real(REAL8), intent(inout) ::    pk(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::    gh(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::  delp(isd:ied,  jsd:jed  ,npz)
    real(REAL8), intent(inout) ::     u(isd:ied  ,jsd:jed+1,npz) 
    real(REAL8), intent(inout) ::     v(isd:ied+1,jsd:jed  ,npz)
! Local:
    real(REAL8), dimension(isd:ied,jsd:jed):: wk
    real(REAL8):: wk1(is:ie+1,js:je)
    real(REAL8):: wk2(is:ie,js:je+1)
    real(REAL8) top_value
    integer i,j,k

    if ( hydrostatic ) then
! pk is pe**kappa if hydrostatic
         top_value = ptk
    else
! pk is full pressure if non-hydrostatic
         top_value = ptop
    endif

!$omp parallel do default(shared) 
    do j=js,je+1
       do i=is,ie+1
          pk(i,j,1) = top_value
       enddo
    enddo

!$omp parallel do default(shared) private(wk)
    do k=2,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) 
    do j=js,je+1
       do i=is,ie
          wk2(i,j) = divg2(i,j)-divg2(i+1,j)
       enddo
    enddo

!$omp parallel do default(shared) 
    do j=js,je
       do i=is,ie+1
          wk1(i,j) = divg2(i,j)-divg2(i,j+1)
       enddo
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz

       if ( hydrostatic ) then
            do j=js,je+1
               do i=is,ie+1
                  wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
               enddo
            enddo
       else
         wk = delp(:,:,k)
         if ( a2b_ord==4 ) then
            call a2b_ord4(wk, wk, npx, npy, is, ie, js, je, ng)
         else
            call a2b_ord2(wk, wk, npx, npy, is, ie, js, je, ng)
         endif
       endif

       do j=js,je+1
          do i=is,ie
             u(i,j,k) = rdx(i,j)*(wk2(i,j)+u(i,j,k) + dt/(wk(i,j)+wk(i+1,j)) * &
                        ((gh(i,j,k+1)-gh(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) &
                       + (gh(i,j,k)-gh(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k))))
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             v(i,j,k) = rdy(i,j)*(wk1(i,j)+v(i,j,k) + dt/(wk(i,j)+wk(i,j+1)) * &
                        ((gh(i,j,k+1)-gh(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) &
                       + (gh(i,j,k)-gh(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k))))
          enddo
       enddo
    enddo    ! end k-loop

 end subroutine one_grad_p



 subroutine grad1_p(delu, delv, pk, gh,  delp, dt, ng, npx, npy, npz,  &
                    ptop, hydrostatic)  

    integer, intent(in) :: ng, npx, npy, npz
    real(REAL8),    intent(in) :: dt, ptop
    logical, intent(in) :: hydrostatic
    real(REAL8), intent(inout) ::    pk(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::    gh(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::  delp(isd:ied,  jsd:jed  ,npz)
    real(REAL8), intent(out) ::    delu(isd:ied  ,jsd:jed+1,npz) 
    real(REAL8), intent(out) ::    delv(isd:ied+1,jsd:jed  ,npz)
! Local:
    real(REAL8):: wk(isd:ied,jsd:jed)
    real(REAL8) top_value
    integer i,j,k



    if ( hydrostatic ) then
! pk is pe**kappa if hydrostatic
         top_value = ptk
    else
! pk is full pressure if non-hydrostatic
         top_value = ptop
    endif

!$omp parallel do default(shared)
    do j=js,je+1
       do i=is,ie+1
          pk(i,j,1) = top_value
       enddo
    enddo
!$omp parallel do default(shared) private(wk)
    do k=2,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz

       if ( hydrostatic ) then
            do j=js,je+1
               do i=is,ie+1
                  wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
               enddo
            enddo
       else
         wk = delp(:,:,k)
         if ( a2b_ord==4 ) then
            call a2b_ord4(wk, wk, npx, npy, is, ie, js, je, ng)
         else
            call a2b_ord2(wk, wk, npx, npy, is, ie, js, je, ng)
         endif
       endif

       do j=js,je+1
          do i=is,ie
             delu(i,j,k) = dt/(wk(i,j)+wk(i+1,j)) *  &
                         ((gh(i,j,k+1)-gh(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) &
                      + (gh(i,j,k)-gh(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)))
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             delv(i,j,k) = dt/(wk(i,j)+wk(i,j+1)) *  &
                         ((gh(i,j,k+1)-gh(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) &
                      + (gh(i,j,k)-gh(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
          enddo
       enddo
    enddo    ! end k-loop

 end subroutine grad1_p


 subroutine grad1_p_update(divg2, u, v, delu, delv, pk, gh,  delp, dt, ng, npx, npy, npz,  &
                           ptop, hydrostatic, alpha)  

    integer, intent(in) :: ng, npx, npy, npz
    real(REAL8),    intent(in) :: dt, ptop, alpha
    logical, intent(in) :: hydrostatic
    real(REAL8), intent(in):: divg2(is:ie+1,js:je+1)
    real(REAL8), intent(inout) ::    pk(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::    gh(isd:ied,  jsd:jed  ,npz+1)
    real(REAL8), intent(inout) ::  delp(isd:ied,  jsd:jed  ,npz)
    real(REAL8), intent(inout) ::     u(isd:ied  ,jsd:jed+1,npz) 
    real(REAL8), intent(inout) ::     v(isd:ied+1,jsd:jed  ,npz)

    real(REAL8), intent(out) ::    delu(isd:ied  ,jsd:jed+1,npz) 
    real(REAL8), intent(out) ::    delv(isd:ied+1,jsd:jed  ,npz)
! Local:
    real(REAL8):: wk(isd:ied,jsd:jed)
    real(REAL8) top_value
    integer i,j,k



    if ( hydrostatic ) then
! pk is pe**kappa if hydrostatic
         top_value = ptk
    else
! pk is full pressure if non-hydrostatic
         top_value = ptop
    endif

!$omp parallel do default(shared)
    do j=js,je+1
       do i=is,ie+1
          pk(i,j,1) = top_value
       enddo
    enddo
!$omp parallel do default(shared) private(wk)
    do k=2,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2(pk(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz+1
       if ( a2b_ord==4 ) then
         call a2b_ord4( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       else
         call a2b_ord2( gh(isd,jsd,k), wk, npx, npy, is, ie, js, je, ng, .true.)
       endif
    enddo

!$omp parallel do default(shared) private(wk)
    do k=1,npz

       if ( hydrostatic ) then
            do j=js,je+1
               do i=is,ie+1
                  wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
               enddo
            enddo
       else
         wk = delp(:,:,k)
         if ( a2b_ord==4 ) then
            call a2b_ord4(wk, wk, npx, npy, is, ie, js, je, ng)
         else
            call a2b_ord2(wk, wk, npx, npy, is, ie, js, je, ng)
         endif
       endif

       do j=js,je+1
          do i=is,ie
             delu(i,j,k) = dt/(wk(i,j)+wk(i+1,j)) *  &
                         ((gh(i,j,k+1)-gh(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) &
                      + (gh(i,j,k)-gh(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)))
             u(i,j,k) = (u(i,j,k) + divg2(i,j)-divg2(i+1,j) + alpha*delu(i,j,k))*rdx(i,j)
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             delv(i,j,k) = dt/(wk(i,j)+wk(i,j+1)) *  &
                         ((gh(i,j,k+1)-gh(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) &
                      + (gh(i,j,k)-gh(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
             v(i,j,k) = (v(i,j,k) + divg2(i,j)-divg2(i,j+1) + alpha*delv(i,j,k))*rdy(i,j)
          enddo
       enddo
    enddo    ! end k-loop

 end subroutine grad1_p_update


 subroutine mix_wz(w, delp, delz, pt, pkc, ng, km, zratio, last_step)
  integer, intent(in):: km, ng
  real(REAL8), intent(in):: pkc(is-ng:ie+ng, js-ng:je+ng, km+1)
  real(REAL8), intent(in), dimension(isd:ied,jsd:jed,km):: delp, pt
  real(REAL8), intent(inout) :: delz(is:ie, js:je, km)  ! delta-height (m)
  real(REAL8), intent(inout), dimension(isd:ied,jsd:jed,km):: w
  real(REAL8), intent(out):: zratio(is:ie, js:je, km)
  logical, intent(in):: last_step
! Local:
  real(REAL8):: wk(is:ie,km)
  real(REAL8):: wm, dz1, dz2, dz3, rat
  integer::ip(js:je)
  integer i, j, k, ip_sum
  logical used


!$omp parallel do default(shared) private(wk, wm, dz1, dz2, dz3, rat)
     do j=js,je

        do k=1,km
           do i=is,ie
              wk(i,k) = pt(i,j,k)*(pkc(i,j,k+1)-pkc(i,j,k))*rgrav
           enddo
        enddo

        ip(j) = 0
! Bottom: 
        k = km
          do i=is,ie
             dz2 = wk(i,k)
             zratio(i,j,k) = -delz(i,j,k)/dz2 
             if( abs(w(i,j,k)) > w_max .or. zratio(i,j,k)<z_min ) then
                wm =  (w(i,j,k-1)*delp(i,j,k-1)+w(i,j,k)*delp(i,j,k))/(delp(i,j,k-1)+delp(i,j,k))
                w(i,j,k-1) = wm
                w(i,j,k  ) = wm
                dz1 = wk(i,k-1)
                rat = (delz(i,j,k-1) + delz(i,j,k)) / (dz1 + dz2)
                delz(i,j,k-1) = rat * dz1
                delz(i,j,k  ) = rat * dz2
                ip(j) = ip(j) + 1
             endif
          enddo

        do k=km-1,2,-1
          do i=is,ie
             dz2 = wk(i,k)
             zratio(i,j,k) = -delz(i,j,k)/dz2 
             if( abs(w(i,j,k)) > w_max .or. zratio(i,j,k)<z_min ) then
! totally mix w
                wm = (w(i,j,k-1)*delp(i,j,k-1)+w(i,j,k)*delp(i,j,k)+w(i,j,k+1)*delp(i,j,k+1))   &
                   / (delp(i,j,k-1) + delp(i,j,k) + delp(i,j,k+1))
                w(i,j,k-1) = wm
                w(i,j,k  ) = wm
                w(i,j,k+1) = wm
! local hydrostatic adjustment of dz
                dz1 = wk(i,k-1)
                dz3 = wk(i,k+1)
                rat = (delz(i,j,k-1)+delz(i,j,k)+delz(i,j,k+1)) / (dz1 + dz2 + dz3)
                delz(i,j,k-1) = rat * dz1
                delz(i,j,k  ) = rat * dz2
                delz(i,j,k+1) = rat * dz3
                ip(j) = ip(j) + 1
              endif
          enddo
        enddo
     enddo

  ip_sum = sum ( ip(js:je) ) 
  if ( ip_sum>km*(je-js+1)*(ie-is+1)/100 ) write(*,*) 'Warning: Mix_wz for GID=', gid, ' total points=', ip_sum 

#ifndef MAPL_MODE
  if ( id_zratio>0 .and. last_step ) then
       zratio(is:ie,js:je,1) = 1.
       call prt_maxmin('DZ_ratio', zratio, is, ie, js, je, 0, km, r1_0, master)
       used=send_data(id_zratio, zratio, fv_time)
  endif
#endif

 end subroutine  mix_wz


 subroutine mix_dp_r8(hydrostatic, w, delp, pt, km, ak, bk, CG)
  integer, intent(IN) :: km
  real(REAL8)   , intent(IN) :: ak(km+1), bk(km+1)
  real(REAL8), intent(INOUT), dimension(isd:ied,jsd:jed,km):: pt, delp, w
  logical, intent(IN) :: hydrostatic, CG
! Local:
     real(REAL8) dp, dpmin
     integer i, j, k, ip
     integer ifirst, ilast
     integer jfirst, jlast


     if ( CG ) then
          ifirst = is-1; ilast = ie+1
          jfirst = js-1; jlast = je+1
     else
          ifirst = is; ilast = ie
          jfirst = js; jlast = je
     endif


!$omp parallel do default(shared) private(ip, dpmin, dp)
     do 1000 j=jfirst,jlast

          ip = 0

          do k=1, km-1
             dpmin = 0.01 * ( ak(k+1)-ak(k) + (bk(k+1)-bk(k))*1.E5 )
             do i=ifirst, ilast
              if(delp(i,j,k) < dpmin) then
! Remap from below and mix pt
                dp = dpmin - delp(i,j,k)
                pt(i,j,k) = (pt(i,j,k)*delp(i,j,k) + pt(i,j,k+1)*dp) / dpmin
                if ( .not.hydrostatic ) w(i,j,k) = (w(i,j,k)*delp(i,j,k) + w(i,j,k+1)*dp) / dpmin
                delp(i,j,k) = dpmin
                delp(i,j,k+1) = delp(i,j,k+1) - dp
                ip = ip + 1
              endif
            enddo
          enddo

! Bottom (k=km):
          dpmin = 0.01 * ( ak(km+1)-ak(km) + (bk(km+1)-bk(km))*1.E5 )
          do i=ifirst, ilast
            if(delp(i,j,km) < dpmin) then
! Remap from above and mix pt
              dp = dpmin - delp(i,j,km)
              pt(i,j,km) = (pt(i,j,km)*delp(i,j,km) + pt(i,j,km-1)*dp)/dpmin
              if ( .not.hydrostatic ) w(i,j,km) = (w(i,j,km)*delp(i,j,km) + w(i,j,km-1)*dp) / dpmin
              delp(i,j,km) = dpmin
              delp(i,j,km-1) = delp(i,j,km-1) - dp
              ip = ip + 1
            endif
          enddo
       if ( fv_debug .and. ip/=0 ) write(*,*) 'Warning: Mix_dp', gid, j, ip 
!      if ( ip/=0 ) write(*,*) 'Warning: Mix_dp', gid, j, ip 
1000   continue

 end subroutine  mix_dp_r8

 subroutine mix_dp_r4(hydrostatic, w, delp, pt, km, ak, bk, CG)
  integer, intent(IN) :: km
  real(REAL8)   , intent(IN) :: ak(km+1), bk(km+1)
  real(REAL4), intent(INOUT), dimension(isd:ied,jsd:jed,km):: pt, delp, w
  logical, intent(IN) :: hydrostatic, CG
! Local:
     real(REAL4) dp, dpmin
     integer i, j, k, ip
     integer ifirst, ilast
     integer jfirst, jlast


     if ( CG ) then
          ifirst = is-1; ilast = ie+1
          jfirst = js-1; jlast = je+1
     else
          ifirst = is; ilast = ie
          jfirst = js; jlast = je
     endif


!$omp parallel do default(shared) private(ip, dpmin, dp)
     do 1000 j=jfirst,jlast

          ip = 0

          do k=1, km-1
             dpmin = 0.01 * ( ak(k+1)-ak(k) + (bk(k+1)-bk(k))*1.E5 )
             do i=ifirst, ilast
              if(delp(i,j,k) < dpmin) then
! Remap from below and mix pt
                dp = dpmin - delp(i,j,k)
                pt(i,j,k) = (pt(i,j,k)*delp(i,j,k) + pt(i,j,k+1)*dp) / dpmin
                if ( .not.hydrostatic ) w(i,j,k) = (w(i,j,k)*delp(i,j,k) + w(i,j,k+1)*dp) / dpmin
                delp(i,j,k) = dpmin
                delp(i,j,k+1) = delp(i,j,k+1) - dp
                ip = ip + 1
              endif
            enddo
          enddo

! Bottom (k=km):
          dpmin = 0.01 * ( ak(km+1)-ak(km) + (bk(km+1)-bk(km))*1.E5 )
          do i=ifirst, ilast
            if(delp(i,j,km) < dpmin) then
! Remap from above and mix pt
              dp = dpmin - delp(i,j,km)
              pt(i,j,km) = (pt(i,j,km)*delp(i,j,km) + pt(i,j,km-1)*dp)/dpmin
              if ( .not.hydrostatic ) w(i,j,km) = (w(i,j,km)*delp(i,j,km) + w(i,j,km-1)*dp) / dpmin
              delp(i,j,km) = dpmin
              delp(i,j,km-1) = delp(i,j,km-1) - dp
              ip = ip + 1
            endif
          enddo
       if ( fv_debug .and. ip/=0 ) write(*,*) 'Warning: Mix_dp', gid, j, ip
!      if ( ip/=0 ) write(*,*) 'Warning: Mix_dp', gid, j, ip 
1000   continue

 end subroutine  mix_dp_r4



 subroutine geopk_r8(ptop, pe, peln, delp, pk, gh, hs, pt, pkz, km, akap, CG)

     integer, intent(IN) :: km
     real(REAL8)   , intent(IN) :: akap, ptop
     real(REAL8)   , intent(IN) :: hs(isd:ied,jsd:jed)
     real(REAL8), intent(IN), dimension(isd:ied,jsd:jed,km):: pt, delp
     logical, intent(IN) :: CG
! !OUTPUT PARAMETERS
     real(REAL8), intent(OUT), dimension(isd:ied,jsd:jed,km+1):: gh, pk
     real(REAL8), intent(OUT) :: pe(is-1:ie+1,km+1,js-1:je+1)
     real(REAL8), intent(out) :: peln(is:ie,km+1,js:je)          ! ln(pe)
     real(REAL8), intent(out) :: pkz(is:ie,js:je,km)
! !DESCRIPTION:
!    Calculates geopotential and pressure to the kappa.
! Local:
     real(REAL8) p1d(is-2:ie+2)
     real(REAL8) logp(is-2:ie+2)
     integer i, j, k
     integer ifirst, ilast
     integer jfirst, jlast

     if ( .not. CG .and. a2b_ord==4 ) then   ! D-Grid
          ifirst = is-2; ilast = ie+2
          jfirst = js-2; jlast = je+2
     else
          ifirst = is-1; ilast = ie+1
          jfirst = js-1; jlast = je+1
     endif

!$omp parallel do default(shared) private(p1d, logp)
     do 2000 j=jfirst,jlast

        do i=ifirst, ilast
           p1d(i) = ptop
           pk(i,j,1) = ptk
           gh(i,j,km+1) = hs(i,j)
        enddo

        if( j>(js-2) .and. j<(je+2) ) then
           do i=max(ifirst,is-1), min(ilast,ie+1) 
              pe(i,1,j) = ptop
           enddo
        endif

! Top down
        do k=2,km+1
          do i=ifirst, ilast
             p1d(i)  = p1d(i) + delp(i,j,k-1)
!             pk(i,j,k) = p1d(i) ** akap
! Optimized form:
             logp(i) = log(p1d(i))
             pk(i,j,k) = exp( akap*logp(i) ) 
          enddo

          if( j>(js-2) .and. j<(je+2) ) then
             do i=max(ifirst,is-1), min(ilast,ie+1) 
                pe(i,k,j) = p1d(i)
             enddo
             if( j>=js .and. j<=je) then
                do i=is,ie
                   peln(i,k,j) = logp(i)
                enddo
             endif
          endif

        enddo

! Bottom up
        do k=km,1,-1
           do i=ifirst, ilast
              gh(i,j,k) = gh(i,j,k+1) + pt(i,j,k)*(pk(i,j,k+1)-pk(i,j,k))
           enddo
        enddo

2000  continue

      if ( .not. CG ) then
! This is for hydrostatic only

!$omp parallel do default(shared)
         do k=1,km
            do j=js,je
               do i=is,ie
                  pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(akap*(peln(i,k+1,j)-peln(i,k,j)))
               enddo
            enddo
         enddo
      endif

 end subroutine geopk_r8

 subroutine geopk_r4(ptop, pe, peln, delp, pk, gh, hs, pt, pkz, km, akap, CG)

     integer, intent(IN) :: km
     real(REAL8)   , intent(IN) :: akap, ptop
     real(REAL8)   , intent(IN) :: hs(isd:ied,jsd:jed)
     real(REAL4), intent(IN), dimension(isd:ied,jsd:jed,km):: pt, delp
     logical, intent(IN) :: CG
! !OUTPUT PARAMETERS
     real(REAL8), intent(OUT), dimension(isd:ied,jsd:jed,km+1):: gh, pk
     real(REAL8), intent(OUT) :: pe(is-1:ie+1,km+1,js-1:je+1)
     real(REAL8), intent(out) :: peln(is:ie,km+1,js:je)          ! ln(pe)
     real(REAL8), intent(out) :: pkz(is:ie,js:je,km)
! !DESCRIPTION:
!    Calculates geopotential and pressure to the kappa.
! Local:
     real(REAL8) p1d(is-2:ie+2)
     real(REAL8) logp(is-2:ie+2)
     integer i, j, k
     integer ifirst, ilast
     integer jfirst, jlast

     if ( .not. CG .and. a2b_ord==4 ) then   ! D-Grid
          ifirst = is-2; ilast = ie+2
          jfirst = js-2; jlast = je+2
     else
          ifirst = is-1; ilast = ie+1
          jfirst = js-1; jlast = je+1
     endif

!$omp parallel do default(shared) private(p1d, logp)
     do 2000 j=jfirst,jlast

        do i=ifirst, ilast
           p1d(i) = ptop
           pk(i,j,1) = ptk
           gh(i,j,km+1) = hs(i,j)
        enddo

        if( j>(js-2) .and. j<(je+2) ) then
           do i=max(ifirst,is-1), min(ilast,ie+1) 
              pe(i,1,j) = ptop
           enddo
        endif

! Top down
        do k=2,km+1
          do i=ifirst, ilast
             p1d(i)  = p1d(i) + delp(i,j,k-1)
!             pk(i,j,k) = p1d(i) ** akap
! Optimized form:
             logp(i) = log(p1d(i))
             pk(i,j,k) = exp( akap*logp(i) ) 
          enddo

          if( j>(js-2) .and. j<(je+2) ) then
             do i=max(ifirst,is-1), min(ilast,ie+1) 
                pe(i,k,j) = p1d(i)
             enddo
             if( j>=js .and. j<=je) then
                do i=is,ie
                   peln(i,k,j) = logp(i)
                enddo
             endif
          endif

        enddo

! Bottom up
        do k=km,1,-1
           do i=ifirst, ilast
              gh(i,j,k) = gh(i,j,k+1) + pt(i,j,k)*(pk(i,j,k+1)-pk(i,j,k))
           enddo
        enddo

2000  continue

      if ( .not. CG ) then
! This is for hydrostatic only

!$omp parallel do default(shared)
         do k=1,km
            do j=js,je
               do i=is,ie
                  pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(akap*(peln(i,k+1,j)-peln(i,k,j)))
               enddo
            enddo
         enddo
      endif

 end subroutine geopk_r4


 subroutine del2_cubed(q, cd, npx, npy, km, nmax)
!---------------------------------------------------------------
! This routine is for filtering the omega field for the physics
!---------------------------------------------------------------
   integer, intent(in):: npx, npy, km, nmax
   real(REAL8),    intent(in):: cd            ! cd = K * da_min;   0 < K < 0.25
   real(FVPRC), intent(inout):: q(isd:ied,jsd:jed,km)
   real(REAL8), parameter:: r3  = 1./3.
   real(REAL8) :: fx(isd:ied+1,jsd:jed), fy(isd:ied,jsd:jed+1)
   real(REAL8) :: q2(isd:ied,jsd:jed)
   integer i,j,k, n, nt, ntimes

   ntimes = min(3, nmax)

                     call timing_on('COMM_TOTAL')
   call mpp_update_domains(q, domain, complete=.true.)
                     call timing_off('COMM_TOTAL')


   do n=1,ntimes
      nt = ntimes - n

!$omp parallel do default(shared) private(fx, fy)
   do k=1,km

      if ( sw_corner ) then
           q(1,1,k) = (q(1,1,k)+q(0,1,k)+q(1,0,k)) * r3
           q(0,1,k) =  q(1,1,k)
           q(1,0,k) =  q(1,1,k)
      endif
      if ( se_corner ) then
           q(ie, 1,k) = (q(ie,1,k)+q(npx,1,k)+q(ie,0,k)) * r3
           q(npx,1,k) =  q(ie,1,k)
           q(ie, 0,k) =  q(ie,1,k)
      endif
      if ( ne_corner ) then
           q(ie, je,k) = (q(ie,je,k)+q(npx,je,k)+q(ie,npy,k)) * r3
           q(npx,je,k) =  q(ie,je,k)
           q(ie,npy,k) =  q(ie,je,k)
      endif
      if ( nw_corner ) then
           q(1, je,k) = (q(1,je,k)+q(0,je,k)+q(1,npy,k)) * r3
           q(0, je,k) =  q(1,je,k)
           q(1,npy,k) =  q(1,je,k)
      endif

      if(nt>0) call copy_corners(q(isd:,jsd:,k), npx, npy, 1)
      do j=js-nt,je+nt
         do i=is-nt,ie+1+nt
            fx(i,j) = dy(i,j)*sina_u(i,j)*(q(i-1,j,k)-q(i,j,k))*rdxc(i,j)
         enddo
      enddo

      if(nt>0) call copy_corners(q(isd:,jsd:,k), npx, npy, 2)
      do j=js-nt,je+1+nt
         do i=is-nt,ie+nt
            fy(i,j) = dx(i,j)*sina_v(i,j)*(q(i,j-1,k)-q(i,j,k))*rdyc(i,j)
         enddo
      enddo

      do j=js-nt,je+nt
         do i=is-nt,ie+nt
            q(i,j,k) = q(i,j,k) + cd*rarea(i,j)*(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))
         enddo
      enddo
   enddo
   enddo

 end subroutine del2_cubed

 subroutine init_ijk_mem_r4(i1, i2, j1, j2, km, array, var)
 integer, intent(in):: i1, i2, j1, j2, km
 real(REAL4), intent(out):: array(i1:i2,j1:j2,km)
 real(REAL8), intent(in):: var
 integer:: i, j, k

!$omp parallel do default(shared)
     do k=1,km
        do j=j1,j2
           do i=i1,i2
              array(i,j,k) = var
           enddo
        enddo
     enddo

 end subroutine init_ijk_mem_r4

 subroutine init_ijk_mem_r8(i1, i2, j1, j2, km, array, var)
 integer, intent(in):: i1, i2, j1, j2, km
 real(REAL8), intent(out):: array(i1:i2,j1:j2,km)
 real(REAL8), intent(in):: var
 integer:: i, j, k

!$omp parallel do default(shared)
     do k=1,km
        do j=j1,j2
           do i=i1,i2
              array(i,j,k) = var
           enddo
        enddo
     enddo

 end subroutine init_ijk_mem_r8


end module dyn_core_mod
