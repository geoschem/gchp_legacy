module adnh_core_mod

! Notes:
! Using k_top=2 to treat the top layer hydrostatically so that delz will
! be computed using hydrostatic balance (instead of the update by
! advection of height using extrapolated winds at the model top)
!
! To do list:
! include moisture effect in pt
!------------------------------

   use constants_mod,  only: rdgas, grav
   use fv_control_mod,     only: m_split, quick_p_c, quick_p_d, uniform_ppm,   &
                             k_top, m_riem,  master
   use tp_core_mod,         only: fv_tp_2d, copy_corners
!  use fv_timing_mod,   only: timing_on, timing_off

   implicit none
   private

   public adRiem_Solver, adRiem_Solver_C, adupdate_dz_c, adupdate_dz_d
   real, parameter:: dz_max = -0.5               ! (meters)

CONTAINS 

  subroutine adupdate_dz_c(is, ie, js, je, km, ng, area,   &
                         zh, adwk)
! !INPUT PARAMETERS:
  integer, intent(in):: is, ie, js, je, ng, km
  real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng,km):: zh
  real, intent(in ):: area(is-ng:ie+ng,js-ng:je+ng)
  real, intent(out):: adwk(is-ng:ie+ng,js-ng:je+ng,km+1)  ! work array

! Local Work array:
  real, dimension(is:ie+1,js:je  ):: xfx, fx
  real, dimension(is:ie  ,js:je+1):: yfx, fy
  integer  i, j, k

  end subroutine adupdate_dz_c



  subroutine adupdate_dz_d(hord, is, ie, js, je, km, ng, npx, npy, area,    &
                         zh,adzh, crx,adcrx, cry,adcry, xfx,adxfx, yfx,adyfx,delp,addelp, n_sponge)

  integer, intent(in):: is, ie, js, je, ng, km, npx, npy
  integer, intent(in):: hord, n_sponge
  real, intent(in)   :: area(is-ng:ie+ng,js-ng:je+ng)
  real, intent(inout) ::  zh(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(inout) ::  adzh(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(inout) ::delp(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(inout) ::addelp(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(inout), dimension(is:ie+1,js-ng:je+ng,km):: crx, xfx
  real, intent(inout), dimension(is-ng:ie+ng,js:je+1,km):: cry, yfx
  real, intent(inout), dimension(is:ie+1,js-ng:je+ng,km):: adcrx, adxfx
  real, intent(inout), dimension(is-ng:ie+ng,js:je+1,km):: adcry, adyfx

!-----------------------------------------------------
! Local array:
  real, dimension(is:   ie+1, js-ng:je+ng):: crx_adv, xfx_adv
  real, dimension(is-ng:ie+ng,js:   je+1 ):: cry_adv, yfx_adv
  real, dimension(is:ie+1,js:je  ):: fx
  real, dimension(is:ie  ,js:je+1):: fy
  real  delx(is:ie+1,km), dely(is-ng:ie+ng,km)
  real :: ra_x(is:ie,js-ng:je+ng)
  real :: ra_y(is-ng:ie+ng,js:je)
!--------------------------------------------------------------------
  integer  i, j, k, iord, isd, ied, jsd, jed, lm


  end subroutine adupdate_dz_d


  subroutine adRiem_Solver_C(dt,   is,  ie,   js, je, km,   ng,  &
                           akap, cp,  ptop, w, adw, pt, adpt,  &
                           delp, addelp, ip)

   integer, intent(in):: is, ie, js, je, ng, km
   integer, intent(in):: ip       ! ip==1 pk is full pressure
   real, intent(in):: dt,  akap, cp, ptop
   real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng,km):: delp, addelp, pt, adpt
   real, intent(inout):: w(is-ng:ie+ng,js-ng:je+ng,km)
   real, intent(inout):: adw(is-ng:ie+ng,js-ng:je+ng,km)

! Local:
  real, dimension(is:ie,km  ):: pm, dm, dz2
  real, dimension(is:ie,km+1):: pem, pk2
  real gama, rgrav, ptk
  integer i, j, k
  integer m_split_c

  end subroutine adRiem_Solver_C


  subroutine adRiem_Solver(dt,   is,   ie,   js, je, km, ng,    &
                         akap, cp,   ptop, peln,adpeln, w,adw, pt,adpt,  &
                         delp, addelp, gz,adgz,   pkc,adpkc, pk,adpk, pe,adpe, ip)
!--------------------------------------------
! !OUTPUT PARAMETERS
! Ouput: gz: grav*height at edges
!        pe: full     hydrostatic pressure
!       pkc: full non-hydrostatic pressure
!--------------------------------------------
   integer, intent(in):: is, ie, js, je, km, ng
   integer, intent(in):: ip      ! ip==0 pkc is perturbation pressure
   real, intent(in):: dt         ! the BIG horizontal Lagrangian time step
   real, intent(in):: akap, cp, ptop

!jkim   logical, intent(in):: last_call
   real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng,km):: w, delp, pt
   real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng,km):: adw, addelp, adpt

   real, intent(out), dimension(is-ng:ie+ng,js-ng:je+ng,km+1):: gz, pkc
   real, intent(out), dimension(is-ng:ie+ng,js-ng:je+ng,km+1):: adgz, adpkc
   real, intent(out):: pk(is:ie,js:je,km+1)
   real, intent(out):: pe(is-1:ie+1,km+1,js-1:je+1)
   real, intent(out):: peln(is:ie,km+1,js:je)           ! ln(pe)
   real, intent(out):: adpk(is:ie,js:je,km+1)
   real, intent(out):: adpe(is-1:ie+1,km+1,js-1:je+1)
   real, intent(out):: adpeln(is:ie,km+1,js:je)           ! ln(pe)
! Local:
  real, dimension(is:ie,km):: pm, dm, dz2
  real :: pem(is:ie,km+1)
  real gama, rgrav, ptk
  integer i, j, k

  end subroutine adRiem_Solver


  subroutine adRiem_3D(ns, bdt, is, ie, js, je, ng, j, km, cp, gama, cappa, p3, dm2,    &
                     pm2, w, dz2, pt, quick_p, c_core, ktop, iad)

  integer, intent(in):: ns, is, ie, js, je, ng,  km, j
  integer, intent(in):: iad      ! time step scheme 
  integer, intent(in):: ktop     ! starting layer for non-hydrostatic dynamics
                                 ! 1: All non-hydrostatic
                                 ! 2: top sponge layer is hydrostatic
  real,    intent(in):: bdt, cp, gama, cappa
  real,    intent(in), dimension(is:ie,km):: dm2, pm2
  logical, intent(in):: quick_p       ! fast algorithm for pressure
  logical, intent(in):: c_core
  real, intent(in  ) :: pt (is-ng:ie+ng,js-ng:je+ng,km)
! IN/OUT:
  real, intent(inout):: dz2(is:ie,km)
  real, intent(inout)::   w(is-ng:ie+ng,js-ng:je+ng,km)
  real, intent(out  )::  p3(is-ng:ie+ng,js-ng:je+ng,km+1)
! --- Local 1D copyies -----
!4TAFifdef USE_2D
!4TAF  real, dimension(km,is:ie):: t2, p2, pt2
!4TAFelse
!4TAF  real, dimension(km):: c2, p2, pt2
!4TAFendif
  real, dimension(km):: r_p, r_n, rden, dz, dm, wm, dts, pdt
  real, dimension(km+1):: m_bot, m_top, r_bot, r_top, time_left, pe1, pbar, wbar

  real, parameter:: dzmx = 0.5*dz_max
  real    :: dt, rdt, grg, z_frac, t_left
  real    :: a1, b1, g2, rcp
  real    :: seq(ns)       ! time stepping sequence
  integer :: k2(km+1)
  integer :: i, k, n, ke, kt, k0, k1, k3

 end subroutine adRiem_3D

 subroutine adtime_sequence ( iad, ns, bdt, tseq )
 integer, intent(in) :: iad, ns
 real,    intent(in) :: bdt
 real, intent(out):: tseq(ns)
! local
  integer :: seq(ns)
  integer :: n, nstep
  real :: sdt

 end subroutine adtime_sequence


 subroutine adedge_profile(q1, q2, i1, i2, j1, j2, j, km, limiter, delp)
! Optimized for wind profile reconstruction:
! Developer: S.-J. Lin, NOAA/GFDL
 integer, intent(in):: i1, i2, j1, j2
 integer, intent(in):: j, km
 integer, intent(in):: limiter
 real, intent(in), optional::  delp(i1:i2,km)  ! layer thickness
 real, intent(inout), dimension(i1:i2,j1:j2,km):: q1, q2
!-----------------------------------------------------------------------
 real, dimension(i1:i2,km+1):: qe1, qe2  ! edge values
 real   d4(i1:i2)
 real  gam(i1:i2,km)
 real  gak(km)
 real  bet, a_bot, gratio, r2o3, r4o3, xt1, xt2
 integer i, k

 end subroutine adedge_profile


 subroutine adtop_edge(p, qe, is, ie, js, je, km)
! Constant grid spacing:
  integer, intent(in) :: is, ie, js, je, km
  real,    intent(in) ::  p(is:ie,js:je,km)
  real, intent(out)::    qe(is:ie,js:je)
!----------------------------------------
  real dq1, dq2
  real a3, b2, sc
  integer i,j

! three-cell parabolic subgrid distribution at model top
! three-cell PP-distribution
! Compute a,b, and c of q = aP**2 + bP + c using cell averages and delp
! a3 = a / 3
! b2 = b / 2

 end subroutine adtop_edge

 subroutine adfix_dz(is, ie, js, je, km, ng, dz)
   integer,  intent(in):: is, ie, km
   integer,  intent(in):: js, je, ng
   real,  intent(inout):: dz(is:ie, js-ng:je+ng,km)
   integer i, j, k
   logical modified

end subroutine adfix_dz
!-----------------------------------------------------------------------

end module adnh_core_mod
