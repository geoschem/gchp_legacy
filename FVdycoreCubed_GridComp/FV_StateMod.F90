#include "MAPL_Generic.h"

module FV_StateMod
!BOP
!
! !MODULE: FV_StateMod --- GEOS5/CAM Cubed-Sphere fvcore state variables/init/run/finalize
!
! !USES:
#if defined( MAPL_MODE )
   use ESMF                ! ESMF base class
   use MAPL_Mod            ! MAPL base class
#endif

   use MAPL_ConstantsMod, only: MAPL_CP, MAPL_RGAS, MAPL_RVAP, MAPL_GRAV, MAPL_RADIUS, &
                                MAPL_KAPPA, MAPL_PI_R8, MAPL_ALHL

   use fms_mod, only: fms_init, set_domain, nullify_domain
   use mpp_domains_mod, only: mpp_update_domains, CGRID_NE, DGRID_NE, mpp_get_boundary
   use mpp_parameter_mod, only: AGRID_PARAM=>AGRID, CORNER
   use fv_timing_mod,    only: timing_on, timing_off, timing_init, timing_prt
   use mpp_mod, only: mpp_pe
   use fv_mp_mod, only: gid, masterproc, domain, npes_x, npes_y

   use fv_grid_utils_mod,  only: inner_prod, mid_pt_sphere, cubed_to_latlon, &
                          vlon, vlat, es, ew, edge_vect_s,edge_vect_n,edge_vect_w,edge_vect_e, &
                          f0, fC, ptop, ptop_min, deglat
   use fv_grid_tools_mod,  only: rarea, area, globalarea, globalsum, dx, dy, dxc, dyc, &
                          dxa, dya, rdxa, rdya, rotate_winds, atob_s, grid_type, &
                          get_unit_vector
   use fv_grid_tools_mod, only: dx_const, dy_const
   use fv_control_mod, only: fv_init
   use fv_arrays_mod , only: fv_atmos_type, FVPRC, REAL4, REAL8, CNVT, tiny_number, huge_number
   use init_hydro_mod, only: p_var
   use fv_dynamics_mod, only: fv_dynamics
   use sw_core_mod, only: d2a2c_vect

   use fv_diagnostics_mod, only: prt_maxmin

implicit none
private

#include "mpif.h"

  real(REAL8), save :: elapsed_time = 0
  real(REAL8), save :: psmo0 = -999.0

! !PUBLIC DATA MEMBERS:

  logical :: FV_OFF = .false.
  logical :: DEBUG = .false.
  logical :: COLDSTART = .false.
  logical :: SW_DYNAMICS = .false.
  logical :: ADIABATIC = .false.
  logical :: FV_HYDROSTATIC = .true.
  logical :: check_surface_pressure = .false.
  integer :: CASE_ID = 11
  integer :: AdvCore_Advection = 0

  public FV_Setup, FV_InitState, FV_Run, FV_Finalize
  public FV_HYDROSTATIC, ADIABATIC, DEBUG, COLDSTART, CASE_ID, SW_DYNAMICS, AdvCore_Advection
  public FV_RESET_CONSTANTS
  public FV_To_State, State_To_FV
  public T_TRACERS, T_FVDYCORE_VARS, T_FVDYCORE_GRID, T_FVDYCORE_STATE
  public fv_getTopography
  public fv_fillMassFluxes
  public fv_computeMassFluxes
  public fv_getVerticalMassFlux
  public fv_getPK
  public fv_getOmega
  public fv_getVorticity
  public fv_getDivergence
  public fv_getEPV
  public fv_getDELZ
  public fv_getPKZ
  public fv_getQ
  public latlon_to_cubed_winds

  public INTERP_DGRID_TO_AGRID
  public INTERP_AGRID_TO_DGRID

  INTERFACE INTERP_DGRID_TO_AGRID

   MODULE PROCEDURE fv_getAgridWinds_3D
   MODULE PROCEDURE fv_getAgridWinds_2D

  END INTERFACE

  INTERFACE INTERP_AGRID_TO_DGRID

   MODULE PROCEDURE a2d3d    ! 2d to 3d
   MODULE PROCEDURE a2d2d    ! 2d to 2d

  END INTERFACE

  logical, save :: Init_FV_Domain = .true.

  integer,  parameter :: ntiles_per_pe = 1
  type(fv_atmos_type), save :: FV_Atm(ntiles_per_pe)

  type T_TRACERS
       logical                                   :: is_r4
       real(REAL8), dimension(:,:,:  ), pointer     :: content
       real(REAL4), dimension(:,:,:  ), pointer     :: content_r4
       character(LEN=ESMF_MAXSTR)                          :: tname
  end type T_TRACERS

! T_FVDYCORE_VARS contains the prognostic variables for FVdycore
  type T_FVDYCORE_VARS
       real(REAL8), dimension(:,:,:  ), pointer     :: U      => NULL() ! U winds (D-grid)
       real(REAL8), dimension(:,:,:  ), pointer     :: V      => NULL() ! V winds (D-grid)
       real(REAL8), dimension(:,:,:  ), pointer     :: PT     => NULL() ! scaled virtual pot. temp.
       real(REAL8), dimension(:,:,:  ), pointer     :: PE     => NULL() ! Pressure at layer edges
       real(REAL8), dimension(:,:,:  ), pointer     :: PKZ    => NULL() ! P^kappa mean
       real(REAL8), dimension(:,:,:  ), pointer     :: DZ     => NULL() ! Height Thickness
       real(REAL8), dimension(:,:,:  ), pointer     :: W      => NULL() ! Vertical Velocity
       type(T_TRACERS), dimension(:), pointer    :: tracer => NULL() ! Tracers
  end type T_FVDYCORE_VARS

! T_FVDYCORE_GRID contains information about the horizontal and vertical
! discretization, unlike in ARIES where these data are split into HORZ_GRID
! and VERT_GRID.  The reason for this: currently all of this information is
! initialized in one call to FVCAM dynamics_init.

  type T_FVDYCORE_GRID
!
#if defined( MAPL_MODE )
    type (MAPL_MetaComp),   pointer :: FVgenstate
    type (ESMF_Grid)                :: GRID           ! The 'horizontal' grid (2D decomp only)
#endif

    integer                         :: NG               ! Ghosting
!
    integer                         :: IS               ! Start X-index (exclusive, unghosted)
    integer                         :: IE               ! End X-index (exclusive, unghosted)
    integer                         :: JS               ! Start Y-index (exclusive, unghosted)
    integer                         :: JE               ! End Y-index (exclusive, unghosted)
!
    integer                         :: ISD              ! Start X-index (exclusive, ghosted)
    integer                         :: IED              ! End X-index (exclusive, ghosted)
    integer                         :: JSD              ! Start Y-index (exclusive, ghosted)
    integer                         :: JED              ! End Y-index (exclusive, ghosted)
!
    integer                         :: NPX             ! Full X- dim
    integer                         :: NPY             ! Full Y- dim
    integer                         :: NPZ             ! Numer of levels
    integer                         :: NPZ_P1          ! NPZ+1 (?)

    integer                         :: NTILES          ! How many log-rectangular tiles does my grid Have
                                                       ! lat-lon      = 1
                                                       ! cubed-sphere = 6

    real(REAL8), allocatable           :: DXC(:,:)     ! local C-Gird DeltaX
    real(REAL8), allocatable           :: DYC(:,:)     ! local C-Gird DeltaY 

    real(REAL8), allocatable           :: AREA(:,:)    ! local cell area
    real(REAL8)                        :: GLOBALAREA   ! global area

    integer                         :: KS              ! Number of true pressure levels (out of NPZ+1)
    real(REAL8)                        :: PTOP            ! pressure at top (ak(1))
    real(REAL8)                        :: PINT            ! initial pressure (ak(npz+1))
    real(REAL8), dimension(:), pointer :: AK => NULL()    ! Sigma mapping
    real(REAL8), dimension(:), pointer :: BK => NULL()    ! Sigma mapping
    integer                         :: N_SPONGE        ! Number of sponge layers at top-of-atmosphere
    real(REAL8)                        :: f_coriolis_angle = 0
!
! Tracers
!
    integer                         :: NQ              ! Number of advected tracers
    integer                         :: NTOTQ           ! Total number of tracers (NQ <= NC)
  end type T_FVDYCORE_GRID

  integer, parameter :: NUM_FVDYCORE_ALARMS        = 3
  integer, parameter :: NUM_TIMES      = 8
  integer, parameter :: TIME_TO_RUN  = 1

  type T_FVDYCORE_STATE
!!!    private
    type (T_FVDYCORE_VARS)               :: VARS
    type (T_FVDYCORE_GRID )              :: GRID
#if defined( MAPL_MODE )
    type (ESMF_Clock), pointer           :: CLOCK
    type (ESMF_Alarm)                    :: ALARMS(NUM_FVDYCORE_ALARMS)
#endif
    integer(kind=8)                      :: RUN_TIMES(4,NUM_TIMES)
    logical                              :: DOTIME, DODYN
    real(REAL8)                          :: DT          ! Large time step
    integer                              :: NSPLIT
    integer                              :: NUM_CALLS
  end type T_FVDYCORE_STATE

! Constants used by fvcore
    real(REAL8)                             :: pi
    real(REAL8)                             :: omega    ! angular velocity of earth's rotation
    real(REAL8)                             :: cp       ! heat capacity of air at constant pressure
    real(REAL8)                             :: radius   ! radius of the earth (m)
    real(REAL8)                             :: rgas     ! Gas constant of the air
    real(REAL8)                             :: rvap     ! Gas constant of vapor
    real(REAL8)                             :: kappa    ! kappa
    real(REAL8)                             :: grav     ! Gravity
    real(REAL8)                             :: hlv      ! latent heat of evaporation
    real(REAL8)                             :: zvir     ! RWV/RAIR-1

  real(kind=4), pointer             :: phis(:,:)

  logical :: fv_first_run = .true.

  integer :: ntracers=11

!
! !DESCRIPTION:
!
!      This module provides variables which are specific to the Lin-Rood
!      dynamical core.  Most of them were previously SAVE variables in
!      different routines and were set with an "if (first)" statement.
!
!      \begin{tabular}{|l|l|} \hline \hline
!        lr\_init    &  Initialize the Lin-Rood variables  \\ \hline
!        lr\_clean   &  Deallocate all internal data structures \\ \hline
!                                \hline
!      \end{tabular}
!
! !REVISION HISTORY:
!   2007.07.17   Putman     Created from lat-lon core
!
!EOP
!-----------------------------------------------------------------------
   real(REAL8), parameter ::  D0_0                    =   0.0
   real(REAL8), parameter ::  D0_5                    =   0.5
   real(REAL8), parameter ::  D1_0                    =   1.0
   real(REAL8), parameter ::  D2_0                    =   2.0
   real(REAL8), parameter ::  D4_0                    =   4.0
   real(REAL8), parameter ::  D180_0                  = 180.0
   real(REAL8), parameter ::  ratmax                  =  0.81

contains

!-----------------------------------------------------------------------
!
! !INTERFACE:
 subroutine FV_RESET_CONSTANTS(FV_PI, FV_OMEGA, FV_CP, FV_RADIUS, FV_RGAS, &
                               FV_RVAP, FV_KAPPA, FV_GRAV, FV_HLV, FV_ZVIR)
   real (REAL8), optional, intent(IN) :: FV_PI
   real (REAL8), optional, intent(IN) :: FV_OMEGA
   real (REAL8), optional, intent(IN) :: FV_CP
   real (REAL8), optional, intent(IN) :: FV_RADIUS
   real (REAL8), optional, intent(IN) :: FV_RGAS
   real (REAL8), optional, intent(IN) :: FV_RVAP
   real (REAL8), optional, intent(IN) :: FV_KAPPA
   real (REAL8), optional, intent(IN) :: FV_GRAV
   real (REAL8), optional, intent(IN) :: FV_HLV
   real (REAL8), optional, intent(IN) :: FV_ZVIR

   if (present(FV_PI)) then
      pi = FV_PI
   endif
   if (present(FV_OMEGA)) then
      omega = FV_OMEGA
   endif
   if (present(FV_CP)) then
      cp = FV_CP
   endif
   if (present(FV_RADIUS)) then
      radius = FV_RADIUS
   endif
   if (present(FV_RGAS)) then
      rgas = FV_RGAS
   endif
   if (present(FV_RVAP)) then
      rvap = FV_RVAP
   endif
   if (present(FV_KAPPA)) then
      kappa = FV_KAPPA
   endif
   if (present(FV_GRAV)) then
      grav = FV_GRAV
   endif
   if (present(FV_HLV)) then
      hlv = FV_HLV
   endif
   if (present(FV_ZVIR)) then
      zvir = FV_ZVIR
   endif

 end subroutine FV_RESET_CONSTANTS
!
!-----------------------------------------------------------------------
 subroutine FV_Setup(GC,LAYOUT_FILE, RC)

  use fv_control_mod, only : npx,npy,npz, ntiles, ncnst, nwat
  use fv_control_mod, only : hord_mt, hord_vt, hord_tm, hord_dp, hord_tr
  use fv_control_mod, only : kord_mt, kord_tm, kord_tr, kord_wz
  use fv_control_mod, only : use_old_omega, courant_max, n_split, m_split, k_split, q_split, master
  use fv_control_mod, only : nord, dddmp, d2_bg, d4_bg, do_vort_damp, d_con, d_ext, vtdm4, beta
  use fv_control_mod, only : k_top, m_riem, p_ref
  use fv_control_mod, only : uniform_ppm, te_method, remap_t,  inline_q, z_tracer, fv_debug
  use fv_control_mod, only : external_ic, ncep_ic, res_latlon_dynamics, res_latlon_tracers, fv_land
  use fv_control_mod, only : consv_te, fv_sg_adj, tau, tau_h2o, rf_center
  use fv_control_mod, only : nf_omega, moist_phys
  use fv_control_mod, only : hydrostatic, phys_hydrostatic,  hybrid_z, m_grad_p
  use fv_control_mod, only : Make_NH
  use fv_control_mod, only : nt_prog, nt_phys
  use fv_control_mod, only : grid_file
  use fv_control_mod, only : t_fac, kd3, w_max, z_min, replace_w
  use fv_control_mod, only : fill_dp, fill_wz
  use fv_control_mod, only : n_sponge, d2_bg_k1, d2_bg_k2
  use test_cases_mod, only : test_case, init_double_periodic

  type (ESMF_GridComp)         , intent(INOUT) :: GC
  character(LEN=*)             , intent(IN   ) :: LAYOUT_FILE
  integer, optional            , intent(OUT  ) :: RC
! Local
   character(len=ESMF_MAXSTR)       :: IAm='FV_StateMod:FV_Setup'
! Local variables

  type (ESMF_Config)           :: cf
  type (ESMF_VM)               :: VM
  integer              :: status
  real(REAL8) :: DT

  integer   :: ks                 !  True # press. levs
  integer   :: ndt,nx,ny

  type (MAPL_MetaComp),          pointer :: MAPL  => NULL()

  integer :: comm

! BEGIN

  call ESMF_VMGetCurrent(VM, rc=STATUS)
  VERIFY_(STATUS)

    call MAPL_MemUtilsWrite(VM, trim(IAm), RC=STATUS )
    VERIFY_(STATUS)

! Retrieve the pointer to the state
! ---------------------------------

  call MAPL_GetObjectFromGC (GC, MAPL,  RC=STATUS )
  VERIFY_(STATUS)

! READ LAYOUT FILE
!
! Get the layout and store directly in the GRID data structure
!

  cf = ESMF_ConfigCreate(rc=rc)
  call ESMF_ConfigLoadFile( cf, LAYOUT_FILE, rc = rc )
 
  call ESMF_ConfigGetAttribute( cf, ntiles   , label='ntiles:'   , default=6, rc = rc )

  call ESMF_ConfigGetAttribute( cf, grid_type, label='grid_type:', default=0        , rc = rc )
  call ESMF_ConfigGetAttribute( cf,  dx_const, label='dx_const:' , default=dx_const , rc = rc )
  call ESMF_ConfigGetAttribute( cf,  dy_const, label='dy_const:' , default=dy_const , rc = rc )
  ASSERT_( dx_const == dy_const )
  call ESMF_ConfigGetAttribute( cf, test_case, label='test_case:', default=11, rc = rc )

  call ESMF_ConfigGetAttribute( cf, npx, label='npx:', RC=STATUS )
  if (STATUS /= ESMF_SUCCESS) then
      call MAPL_GetResource( MAPL, npx, 'AGCM_IM:', default=32, RC=STATUS )
      VERIFY_(STATUS)
  endif
  call ESMF_ConfigGetAttribute( cf, npy, label='npy:', RC=STATUS )
  if (STATUS /= ESMF_SUCCESS) then
      call MAPL_GetResource( MAPL, npy, 'AGCM_IM:', default=32, RC=STATUS )
      VERIFY_(STATUS)
  endif
  call ESMF_ConfigGetAttribute( cf, npz, label='npz:', RC=STATUS )
  if (STATUS /= ESMF_SUCCESS) then
      call MAPL_GetResource( MAPL, npz, 'AGCM_LM:', default=72, RC=STATUS )
      VERIFY_(STATUS)
  endif

  if (npz == 1) SW_DYNAMICS = .true.
 
  call ESMF_ConfigGetAttribute( cf, npes_x, label='npes_x:', RC=STATUS )
  if (STATUS /= ESMF_SUCCESS) then
      call MAPL_GetResource( MAPL, nx, 'NX:', default=0, RC=STATUS )
      VERIFY_(STATUS)
      npes_x = nx
  endif
  call ESMF_ConfigGetAttribute( cf, npes_y, label='npes_y:', RC=STATUS )
  if (STATUS /= ESMF_SUCCESS) then
      call MAPL_GetResource( MAPL, ny, 'NY:', default=0, RC=STATUS )
      VERIFY_(STATUS)
      if (grid_type == 4) then
         npes_y = ny
      else
         npes_y = ny / 6
         ASSERT_( 6*npes_y == ny )
      end if
  endif

! Get other scalars
! -----------------

  call MAPL_GetResource( MAPL, ndt, 'RUN_DT:', default=0, RC=STATUS )
  VERIFY_(STATUS)
  DT = ndt

! Advect tracers within DynCore(AdvCore_Advection=.false.)
!             or within AdvCore(AdvCore_Advection=.true.)
  call MAPL_GetResource( MAPL, AdvCore_Advection, label='AdvCore_Advection:', default=AdvCore_Advection, rc=status )
  VERIFY_(STATUS)

! Setup Doubly Periodic Domain Info
  call MAPL_GetResource( MAPL, deglat, label='FIXED_LATS:', default=deglat, rc=status )
  VERIFY_(STATUS)

  call ESMF_ConfigGetAttribute( cf, hord_mt, label='hord_mt:', default= hord_mt, rc = rc )
  call ESMF_ConfigGetAttribute( cf, hord_vt, label='hord_vt:', default= hord_vt, rc = rc )
  call ESMF_ConfigGetAttribute( cf, hord_tm, label='hord_tm:', default= hord_tm, rc = rc )
  call ESMF_ConfigGetAttribute( cf, hord_dp, label='hord_dp:', default= hord_dp, rc = rc )
  call ESMF_ConfigGetAttribute( cf, hord_tr, label='hord_tr:', default= hord_tr, rc = rc )
  call ESMF_ConfigGetAttribute( cf, kord_mt, label='kord_mt:', default= kord_mt, rc = rc )
  call ESMF_ConfigGetAttribute( cf, kord_tm, label='kord_tm:', default= kord_tm, rc = rc )
  call ESMF_ConfigGetAttribute( cf, kord_wz, label='kord_wz:', default= kord_wz, rc = rc )
  call ESMF_ConfigGetAttribute( cf, kord_tr, label='kord_tr:', default= kord_tr, rc = rc )

! Default GEOS-5 FV3 2nd order divergence damping setup
  nord  = 0
  dddmp = 0.2
  d2_bg = 0.0075
  d4_bg = 0.0
! method for non-hydrostatic grad-p
! m_grad_p=1:  one-stage full pressure for grad_p; this option is faster
!              but it is not suitable for low horizontal resolution
! m_grad_p=0:  two-stage grad computation (best for low resolution runs)
  m_grad_p = 0
  if (npx >= 2880) m_grad_p = 1
  call ESMF_ConfigGetAttribute( cf,    nord, label='nord:'   , default= nord, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   dddmp, label='dddmp:'  , default= dddmp, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   d2_bg, label='d2_bg:'  , default= d2_bg, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   d4_bg, label='d4_bg:'  , default= d4_bg, rc = rc )
  call ESMF_ConfigGetAttribute( cf, m_grad_p , label='m_grad_p:' , default=m_grad_p, rc = rc )

  call ESMF_ConfigGetAttribute( cf,   do_vort_damp, label='do_vort_damp:'  , default= do_vort_damp, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   vtdm4, label='vtdm4:'  , default= vtdm4, rc = rc )
  call ESMF_ConfigGetAttribute( cf,    beta, label='beta:'   , default= beta, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   d_ext, label='d_ext:'  , default= d_ext, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   d_con, label='d_con:'  , default= d_con, rc = rc )
  call ESMF_ConfigGetAttribute( cf,     tau, label='tau:'    , default= tau, rc = rc )

  kd3=0 ! Disable 3D divergence damping by default
  call ESMF_ConfigGetAttribute( cf,   t_fac, label='t_fac:' , default= t_fac, rc = rc )
  call ESMF_ConfigGetAttribute( cf,     kd3, label='kd3:'   , default= kd3, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   w_max, label='w_max:' , default= w_max, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   z_min, label='z_min:' , default= z_min, rc = rc )

! It's possible that we need to replace the 
! W generated from Hydrostatic State with Omega 
! after one interation to get a good non-hydrostatic W
  call ESMF_ConfigGetAttribute( cf, replace_w, label='replace_w:' , default= .false., rc = rc )

! Mix Delp or WZ
  call ESMF_ConfigGetAttribute( cf, fill_dp, label='fill_dp:' , default= .false., rc = rc )
  call ESMF_ConfigGetAttribute( cf, fill_wz, label='fill_wz:' , default= .false., rc = rc )

  if (SW_DYNAMICS) then
     call ESMF_ConfigGetAttribute( cf, n_sponge, label='n_sponge:', default=-1, rc = rc )
  else
     call ESMF_ConfigGetAttribute( cf, n_sponge, label='n_sponge:', default=0, rc = rc )
     call WRITE_PARALLEL(n_sponge ,format='("Number of Sponge Layers : ",(   I3))')
  endif
  if (n_sponge==0) then
    d2_bg_k1=0.2
    d2_bg_k2=0.07
  endif
  call ESMF_ConfigGetAttribute( cf, d2_bg_k1, label='d2_bg_k1:', default= d2_bg_k1, rc = rc )
  call ESMF_ConfigGetAttribute( cf, d2_bg_k2, label='d2_bg_k2:', default= d2_bg_k2, rc = rc )

  call ESMF_ConfigGetAttribute( cf, use_old_omega, label='use_old_omega:', default=.true., rc = rc )

  courant_max=0.05 ! when n_split is negative dynamic subcycling is enabled to allow this maximum courant number
  call ESMF_ConfigGetAttribute( cf, courant_max, label='courant_max:', default=courant_max, rc = rc )
  call ESMF_ConfigGetAttribute( cf, n_split, label='nsplit:', default=0, rc = rc )
  call ESMF_ConfigGetAttribute( cf, m_split, label='msplit:', default=0, rc = rc )
  call ESMF_ConfigGetAttribute( cf, k_split, label='ksplit:', default=1, rc = rc )
  call ESMF_ConfigGetAttribute( cf, q_split, label='qsplit:', default=0, rc = rc )
  call ESMF_ConfigGetAttribute( cf, ncnst  , label='ncnst:' , default=1, rc = rc )
  call ESMF_ConfigGetAttribute( cf,   k_top, label='k_top:' , default=1, rc = rc )
! m_riem is the time scheme for Riem solver subcycling
! The default is 0, options range 0:5
! Note: iad=2 or 4 appear to be more stable than other options
  call ESMF_ConfigGetAttribute( cf,  m_riem, label='m_riem:', default=0, rc = rc )

  call ESMF_ConfigGetAttribute( cf, nwat     , label='nwat:'     , default=1     , rc = rc )
  call ESMF_ConfigGetAttribute( cf, consv_te , label='consv_te:' , default=consv_te, rc = rc )
  call ESMF_ConfigGetAttribute( cf, remap_t  , label='remap_t:'  , default=remap_t , rc = rc )
  call ESMF_ConfigGetAttribute( cf, z_tracer , label='z_tracer:' , default=z_tracer, rc = rc )
  call ESMF_ConfigGetAttribute( cf, inline_q , label='inline_q:' , default=inline_q, rc = rc )

 !! te_method=0 : PPM
 !! te_method=1 : cubic 
  call ESMF_ConfigGetAttribute( cf, te_method, label='te_method:', default=te_method, rc = rc )
  if (te_method == 1) kord_tm = ABS(kord_tm) !cubic - force remap to use total energy ABS(kord_tm)

  call ESMF_ConfigGetAttribute( cf, DEBUG , label='fv_debug:' , default=fv_debug, rc = rc )
  fv_debug=DEBUG

  call ESMF_ConfigGetAttribute( cf, check_surface_pressure, label='check_surface_pressure:', &
                                default= check_surface_pressure, rc = rc )

  call ESMF_ConfigGetAttribute( cf, ADIABATIC, label='ADIABATIC:', default=adiabatic, rc = rc )
  call ESMF_ConfigGetAttribute( cf, FV_OFF   , label='FV_OFF:', default=.false., rc = rc )

  call ESMF_ConfigGetAttribute( cf, grid_file, label='grid_file:', default='Inline', rc = rc )

  call ESMF_ConfigGetAttribute( cf, hydrostatic   , label='hydrostatic:'  , default=.true., rc = rc )
  call ESMF_ConfigGetAttribute( cf, hybrid_z      , label='hybrid_z:'     , default=.false., rc = rc )
  call ESMF_ConfigGetAttribute( cf, Make_NH       , label='Make_NH:'      , default=.false., rc = rc )
  FV_HYDROSTATIC = hydrostatic
!
! FV likes npx;npy in terms of cell vertices
!
  npx=npx+1 ; npy=npy+1
! 
! Constants
!
    pi     = MAPL_PI_R8
    omega  = MAPL_OMEGA    ! angular velocity of earth's rotation
    cp     = MAPL_CP       ! heat capacity of air at constant pressure
    radius = MAPL_RADIUS   ! radius of the earth (m)
    rgas   = MAPL_RGAS     ! Gas constant of the air
    rvap   = MAPL_RVAP     ! Gas constant of vapor
    kappa  = MAPL_KAPPA    ! kappa
    grav   = MAPL_GRAV     ! Gravity
    hlv    = MAPL_ALHL     ! latent heat of evaporation
    zvir   = MAPL_RVAP/MAPL_RGAS - 1.   ! RWV/RAIR-1

!
! All done with configuration
!
  call ESMF_ConfigDestroy( cf, rc = rc )

! WMP This is all done in FV_INIT now.... 
! Calculate N_SPLIT if it was specified as 0
 !if ( N_SPLIT == 0 ) n_split= INIT_NSPLIT(dt,npx-1,npy-1)
 !call WRITE_PARALLEL ( n_split ,format='("Dynamics NSPLIT: ",(i3))' )
 !call WRITE_PARALLEL ( k_split ,format='("Dynamics KSPLIT: ",(i3))' )
 !if ( (.not.hydrostatic) .and. (m_split==0) ) then
!!                    Estimates for Gnomonic grids:
 !          !===================================================
 !          !        dx (km)    dt (sc)    n_split    m_split
 !          !===================================================
 !          ! C1000:  ~10        150         16          3
 !          ! C2000:   ~5         90         18 (5 s)    2
 !          !===================================================
 !   m_split = max(1., 0.5 + abs(real(dt/k_split))/(n_split*6.) )
 ! ! m_split = max(1, CEILING(0.5 + abs(real(dt/k_split))/(n_split*3.)) )
 !   call WRITE_PARALLEL ( m_split ,format='("Dynamics MSPLIT: ",(i3))' )
 !else
 !call WRITE_PARALLEL ( m_split ,format='("Dynamics MSPLIT: ",(i3))' )
 !endif
 !call WRITE_PARALLEL ( q_split ,format='("Dynamics QSPLIT: ",(i3))' )
! WMP This is all done in FV_INIT now.... 

    call MAPL_TimerOn(MAPL,"--FMS_INIT")
    call ESMF_VMGet(VM,mpiCommunicator=comm,rc=status)
    VERIFY_(STATUS)
    call fms_init(comm)
    call MAPL_TimerOff(MAPL,"--FMS_INIT")
    call MAPL_MemUtilsWrite(VM, 'FV_StateMod: FMS_INIT', RC=STATUS )
    VERIFY_(STATUS)
! Start up FV                   
    call MAPL_TimerOn(MAPL,"--FV_INIT")
    call fv_init(FV_Atm, DT)
    call MAPL_TimerOff(MAPL,"--FV_INIT")
    call MAPL_MemUtilsWrite(VM, 'FV_StateMod: FV_INIT', RC=STATUS )
    VERIFY_(STATUS)

 ASSERT_(DT > 0.0)

  call WRITE_PARALLEL("Dynamics PE Layout ")
  call WRITE_PARALLEL(NPES_X    ,format='("NPES_X  : ",(   I3))')
  call WRITE_PARALLEL(NPES_Y    ,format='("NPES_Y  : ",(   I3))')

  call WRITE_PARALLEL((/npx,npy,npz/)       , &
    format='("Resolution of dynamics restart     =",3I5)'  )

  ks = FV_Atm(1)%ks ! ALT: this was the value when we read "old" style FV_internal restart
                    !      if needed, we could compute, ks by count(BK==0.0)
                    !      then FV will try to run slightly more efficient code
                    !      So far, GEOS-5 has used ks = 0
  ASSERT_(ks <= NPZ+1)
  call WRITE_PARALLEL(ks                          , &
     format='("Number of true pressure levels =", I5)'   )

  call MAPL_MemUtilsWrite(VM, trim(Iam), RC=STATUS )
  VERIFY_(STATUS)

  RETURN_(ESMF_SUCCESS)

contains

!-----------------------------------------------------------------------
! BOP
! !IROUTINE:  init_nsplit --- find proper value for nsplit if not specified
!
! !INTERFACE:
  integer function INIT_NSPLIT(dtime,npx,npy)
!
! !USES:
    use fv_control_mod,    only: k_split

    implicit none

! !INPUT PARAMETERS:
    real (REAL8), intent(in) :: dtime      !  time step
    integer, intent(in)   :: npx,npy    !  Global horizontal resolution

! !DESCRIPTION:
! 
!    If nsplit=0 (module variable) then determine a good value
!    for ns (used in fvdycore) based on resolution and the large-time-step
!    (dtime). The user may have to set this manually if instability occurs.
! 
! !REVISION HISTORY:
!   00.10.19   Lin     Creation
!   01.03.26   Sawyer  ProTeX documentation
!   01.06.10   Sawyer  Modified for dynamics_init framework
!   03.12.04   Sawyer  Moved here from dynamics_vars.  Now a function
!   07.16.07   Putman  Modified for cubed-sphere
!
! EOP
!-----------------------------------------------------------------------
! !LOCAL VARIABLES:
    real (REAL8)   umax
    real (REAL8)   dimx
    real (REAL8)   dim0                      ! base dimension
    real (REAL8)   dt0                       ! base time step              
    real (REAL8)   ns0                       ! base nsplit for base dimension
    integer     ns                        ! final value to be returned
                     
    parameter ( dim0 = 180.  )
    parameter ( dt0  = 1800. )
    parameter ( umax = 350.  )
 
    ns0  = 7.
    dimx = 4.0*npx
    if (grid_type < 4) then
       ns = nint ( ns0*abs(dtime)*dimx/(dt0*dim0) + 0.49 )
       if (.not. hydrostatic) ns = ns*1.5 ! time-step needs to be shortened for NH stabilitiy
       ns = max ( 1, ns )
    else
      !ns = nint ( 2.*umax*dtime/sqrt(dx_const**2 + dy_const**2) + 0.49 )
       ns = nint ( ns0*dtime/sqrt(dx_const**2 + dy_const**2) + 0.49 )
    endif

    init_nsplit = ns/k_split

    return
  end function INIT_NSPLIT
!---------------------------------------------------------------------

 end subroutine FV_Setup

 subroutine FV_InitState (STATE, CLOCK, INTERNAL, IMPORT, GC, RC)

  use fv_control_mod, only : npx,npy,npz, ntiles, ncnst, nwat
  use fv_control_mod, only : hord_mt, hord_vt, hord_tm, hord_dp, hord_tr
  use fv_control_mod, only : kord_mt, kord_tm, kord_tr, kord_wz
  use fv_control_mod, only : courant_max, n_split, m_split, k_split, q_split, master
  use fv_control_mod, only : nord, dddmp, d2_bg, d4_bg, d_con, d_ext, vtdm4, beta
  use fv_control_mod, only : k_top, m_riem, p_ref
  use fv_control_mod, only : uniform_ppm, te_method, remap_t,  inline_q, z_tracer, fv_debug
  use fv_control_mod, only : external_ic, ncep_ic, res_latlon_dynamics, res_latlon_tracers, fv_land
  use fv_control_mod, only : consv_te, fv_sg_adj, tau, tau_h2o, rf_center
  use fv_control_mod, only : nf_omega, moist_phys
  use fv_control_mod, only : hydrostatic, phys_hydrostatic,  hybrid_z, m_grad_p
  use fv_control_mod, only : Make_NH
  use fv_control_mod, only : nt_prog, nt_phys
  use fv_control_mod, only : grid_file
  use fv_control_mod, only : t_fac, kd3, w_max, z_min, replace_w
  use fv_control_mod, only : fill_dp, fill_wz
  use fv_control_mod, only : n_sponge, d2_bg_k1, d2_bg_k2
  use test_cases_mod, only : test_case, init_double_periodic 

  type (T_FVDYCORE_STATE),pointer              :: STATE

  type (ESMF_Clock), target,     intent(INOUT) :: CLOCK
  type (ESMF_GridComp)         , intent(INOUT) :: GC
  type (ESMF_State)            , intent(INOUT) :: INTERNAL
  type (ESMF_State)            , intent(INOUT) :: IMPORT
  integer, optional            , intent(OUT  ) :: RC

! Local variables

! Pointers to geography info in the MAPL MetaComp

  real,                 pointer :: LATS (:,:)
  real,                 pointer :: LONS (:,:)

  type (ESMF_TimeInterval)     :: Time2Run
  type (ESMF_VM)               :: VM
  type (T_FVDYCORE_GRID) , pointer :: GRID
  integer              :: status
  real(REAL8) :: DT

  integer   :: is ,ie , js ,je    !  Local dims
  integer   :: isc,iec, jsc,jec   !  Local dims
  integer   :: isd,ied, jsd,jed   !  Local dims
  integer   :: k                  !  Vertical loop index
  integer   :: ng
  integer   :: ndt

  integer   :: i,j

  type (ESMF_Time) :: fv_time
  integer :: days, seconds

  character(len=ESMF_MAXSTR)       :: IAm='FV:FV_InitState'

  real(REAL8), pointer                   :: AK(:) => NULL()
  real(REAL8), pointer                   :: BK(:) => NULL()
  real(REAL8), dimension(:,:,:), pointer :: U     => NULL()
  real(REAL8), dimension(:,:,:), pointer :: V     => NULL()
  real(REAL8), dimension(:,:,:), pointer :: PT    => NULL()
  real(REAL8), dimension(:,:,:), pointer :: PE    => NULL()
  real(REAL8), dimension(:,:,:), pointer :: PKZ   => NULL()
  real(REAL8), dimension(:,:,:), pointer :: DZ    => NULL()
  real(REAL8), dimension(:,:,:), pointer :: W     => NULL()
  type (MAPL_MetaComp),          pointer :: mapl  => NULL()

  real(REAL8), ALLOCATABLE :: UA(:,:,:)
  real(REAL8), ALLOCATABLE :: VA(:,:,:)
  real(REAL8), ALLOCATABLE :: UD(:,:,:)
  real(REAL8), ALLOCATABLE :: VD(:,:,:)

  logical    :: hybrid

! BEGIN

! Retrieve the pointer to the state
! ---------------------------------

  call MAPL_GetObjectFromGC (GC, MAPL,  RC=STATUS )
  VERIFY_(STATUS)

  call MAPL_GetResource( MAPL, ndt, 'RUN_DT:', default=0, RC=STATUS )
  VERIFY_(STATUS)
  DT = ndt

  STATE%GRID%FVgenstate => MAPL
  GRID => STATE%GRID     ! For convenience
  STATE%DOTIME= .TRUE.
  STATE%DT        = DT
  STATE%NSPLIT    = N_SPLIT
  GRID%NG     = 3 ; ng = 3
  GRID%NPX    = NPX-1
  GRID%NPY    = NPY-1
  GRID%NPZ    = NPZ
  GRID%NPZ_P1 = NPZ+1
  GRID%NTILES = 6
  GRID%N_SPONGE  = N_SPONGE
  GRID%NTOTQ  = MAX(1,ncnst)
  GRID%NQ     = MAX(1,ncnst)
  call ESMF_GridCompGet(gc, grid=GRID%GRID, VM=VM, rc=STATUS)
    VERIFY_(STATUS)

  call WRITE_PARALLEL(' ')
  call WRITE_PARALLEL(STATE%DT,format='("Dynamics time step : ",(F10.4))')
  call WRITE_PARALLEL(' ')

! Get pointers to internal state vars
  call MAPL_GetPointer(internal, ak, "AK",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, bk, "BK",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, u, "U",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, v, "V",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, pt, "PT",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, pe, "PE",rc=status)
  VERIFY_(STATUS)
  call MAPL_GetPointer(internal, pkz, "PKZ",rc=status)
  VERIFY_(STATUS)
  if ( .not. hydrostatic ) then
    call MAPL_GetPointer(internal, dz, "DELZ",rc=status)
    VERIFY_(STATUS)
    call MAPL_GetPointer(internal, w, "W",rc=status)
    VERIFY_(STATUS)
  endif

  call CREATE_VARS ( FV_Atm(1)%isc, FV_Atm(1)%iec, FV_Atm(1)%jsc, FV_Atm(1)%jec,     &
                     1, npz, npz+1,            &
                     U, V, PT, PE, PKZ, DZ, W, &
                     STATE%VARS )
  call MAPL_MemUtilsWrite(VM, 'FV_StateMod: CREATE_VARS', RC=STATUS )
  VERIFY_(STATUS)

  GRID%IS     = FV_Atm(1)%isc
  GRID%IE     = FV_Atm(1)%iec
  GRID%JS     = FV_Atm(1)%jsc
  GRID%JE     = FV_Atm(1)%jec
  GRID%ISD    = FV_Atm(1)%isd
  GRID%IED    = FV_Atm(1)%ied
  GRID%JSD    = FV_Atm(1)%jsd
  GRID%JED    = FV_Atm(1)%jed
  if(.not.associated(GRID%AK)) allocate(GRID%AK(size(ak)))
  if(.not.associated(GRID%BK)) allocate(GRID%BK(size(bk)))
  GRID%AK     = ak
  GRID%BK     = bk

! Local Copy of dimensions

  IS     = FV_Atm(1)%isc
  IE     = FV_Atm(1)%iec
  JS     = FV_Atm(1)%jsc
  JE     = FV_Atm(1)%jec
  ISC    = FV_Atm(1)%isc         
  IEC    = FV_Atm(1)%iec 
  JSC    = FV_Atm(1)%jsc 
  JEC    = FV_Atm(1)%jec
  ISD    = FV_Atm(1)%isd
  IED    = FV_Atm(1)%ied
  JSD    = FV_Atm(1)%jsd
  JED    = FV_Atm(1)%jed

  allocate( GRID%DXC(IS:IE,JS:JE) )
  GRID%DXC = dxc(IS:IE,JS:JE)

  allocate( GRID%DYC(IS:IE,JS:JE) )
  GRID%DYC = dyc(IS:IE,JS:JE)

  allocate( GRID%AREA(IS:IE,JS:JE) )
  GRID%AREA = area(IS:IE,JS:JE)
  GRID%GLOBALAREA = globalarea

  if (grid_type == 4) then
     fC(:,:) = 2.*MAPL_OMEGA*sin(deglat/180.*MAPL_PI_R8)
     f0(:,:) = 2.*MAPL_OMEGA*sin(deglat/180.*MAPL_PI_R8)
  else
   if (GRID%f_coriolis_angle == -999) then
     fC(:,:) = 0.0
     f0(:,:) = 0.0
   else
     do j=jsd,jed+1
        do i=isd,ied+1
           fC(i,j) = 2.*MAPL_OMEGA*( -COS(FV_Atm(1)%grid(i,j,1))*COS(FV_Atm(1)%grid(i,j,2))*SIN(GRID%f_coriolis_angle) + &
                                      SIN(FV_Atm(1)%grid(i,j,2))*COS(GRID%f_coriolis_angle) )
        enddo
     enddo
     do j=jsd,jed
        do i=isd,ied
           f0(i,j) = 2.*MAPL_OMEGA*( -COS(FV_Atm(1)%agrid(i,j,1))*COS(FV_Atm(1)%agrid(i,j,2))*SIN(GRID%f_coriolis_angle) + &
                                      SIN(FV_Atm(1)%agrid(i,j,2))*COS(GRID%f_coriolis_angle) )
        enddo
     enddo
   endif
  endif


! Check coordinate information from MAPL_MetaComp
!--------------------------------------------
    call MAPL_Get(MAPL,                &
       LATS          = LATS,           & ! These are in radians
       LONS          = LONS,           & ! These are in radians
       INTERNAL_ESMF_STATE=INTERNAL,   &
                             RC=STATUS )
    VERIFY_(STATUS)

  STATE%CLOCK => CLOCK
  call ESMF_TimeIntervalSet(Time2Run, &
                            S=nint(STATE%DT), rc=status)
  VERIFY_(status)

  STATE%ALARMS(TIME_TO_RUN) = ESMF_AlarmCreate(name="Time2Run", clock=clock, &
                              ringInterval=Time2Run, &
                              Enabled=.TRUE., rc=status) ; VERIFY_(status)
  call ESMF_AlarmEnable(STATE%ALARMS(TIME_TO_RUN), rc=status); VERIFY_(status)
  call ESMF_AlarmRingerOn(STATE%ALARMS(TIME_TO_RUN), rc=status); VERIFY_(status)

  call WRITE_PARALLEL(' ')
  call WRITE_PARALLEL(STATE%DT, &
    format='("INITIALIZED ALARM: DYN_TIME_TO_RUN EVERY ",F9.1," secs.")')

!  Clear wall clock time clocks and global budgets

  STATE%RUN_TIMES = 0
  STATE%NUM_CALLS = 0

  call ESMF_ClockGet( CLOCK, currTime=fv_time, rc=STATUS )
  VERIFY_(STATUS)
  call ESMF_TimeGet( fv_time, dayOfYear=days, s=seconds, rc=STATUS )
  VERIFY_(STATUS)

  call MAPL_GetPointer ( import, phis, 'PHIS', RC=STATUS )
  VERIFY_(STATUS)

  FV_Atm(1)%ak = ak
  FV_Atm(1)%bk = bk
  ptop = FV_Atm(1)%ak(1)
  FV_Atm(1)%q(:,:,:,:) = 0.0 ! We Don't Have QV from the Import yet

  if (COLDSTART) then

   if (grid_type == 4) then
       if ( FV_Atm(1)%make_hybrid_z ) then
         hybrid = .false.
       else
         hybrid = FV_Atm(1)%hybrid_z
       endif
       call init_double_periodic(FV_Atm(1)%u,FV_Atm(1)%v,FV_Atm(1)%w,FV_Atm(1)%pt,FV_Atm(1)%delp,FV_Atm(1)%q,FV_Atm(1)%phis, &
                                 FV_Atm(1)%ps,FV_Atm(1)%pe, &
                                 FV_Atm(1)%peln,FV_Atm(1)%pk,FV_Atm(1)%pkz, FV_Atm(1)%uc,FV_Atm(1)%vc, FV_Atm(1)%ua,FV_Atm(1)%va,        &
                                 FV_Atm(1)%ak, FV_Atm(1)%bk, FV_Atm(1)%npx, FV_Atm(1)%npy, npz, FV_Atm(1)%ng, ncnst, FV_Atm(1)%nwat,  &
                                 FV_Atm(1)%k_top, FV_Atm(1)%ndims, FV_Atm(1)%ntiles, FV_Atm(1)%dry_mass, FV_Atm(1)%mountain,       &
                                 FV_Atm(1)%moist_phys, FV_Atm(1)%hydrostatic, hybrid, FV_Atm(1)%delz, FV_Atm(1)%ze0)
     ! Copy FV to internal State
       call FV_To_State ( STATE )
       if( gid==masterproc ) write(*,*) 'Doubly Periodic IC generated LAT:', deglat
   else
     ALLOCATE( UA(isc:iec  ,jsc:jec  ,1:npz) )
     ALLOCATE( VA(isc:iec  ,jsc:jec  ,1:npz) )
     ALLOCATE( UD(isc:iec  ,jsc:jec+1,1:npz) )
     ALLOCATE( VD(isc:iec+1,jsc:jec  ,1:npz) )
     UA(isc:iec,jsc:jec,:) = STATE%VARS%U(isc:iec,jsc:jec,:)
     VA(isc:iec,jsc:jec,:) = STATE%VARS%V(isc:iec,jsc:jec,:)
     call INTERP_AGRID_TO_DGRID( UA, VA, UD, VD )
     STATE%VARS%U(isc:iec,jsc:jec,:) = UD(isc:iec,jsc:jec,:)
     STATE%VARS%V(isc:iec,jsc:jec,:) = VD(isc:iec,jsc:jec,:)
     DEALLOCATE ( UA )
     DEALLOCATE ( VA )
     DEALLOCATE ( UD )
     DEALLOCATE ( VD )
     if ( .not. FV_HYDROSTATIC) then
        W   = 0.0
        PT = PT*PKZ
        call fv_getDELZ(DZ,PT,PE)
   !    ! USE W=0.0 inplace of QV for dry test case PKZ computation
   !    call fv_getPKZ(PKZ,PT,W,PE,DZ,.TRUE.)
        PT = PT/PKZ
     endif
     call State_To_FV( STATE )
   endif ! doubly-periodic
  endif ! COLDSTART

  if (Make_NH) then
      call State_To_FV( STATE )
      call p_var(npz, isc, iec, jsc, jec, ptop, ptop_min, FV_Atm(1)%delp, &
                 FV_Atm(1)%delz, FV_Atm(1)%pt, FV_Atm(1)%ps,   &
                 FV_Atm(1)%pe, FV_Atm(1)%peln, FV_Atm(1)%pk,   &
                 FV_Atm(1)%pkz, kappa, FV_Atm(1)%q, FV_Atm(1)%ng, &
                 FV_Atm(1)%ncnst, FV_Atm(1)%dry_mass, .false., FV_Atm(1)%mountain, &
                 FV_Atm(1)%moist_phys, FV_Atm(1)%hydrostatic, FV_Atm(1)%k_top, &
                 FV_Atm(1)%nwat, Make_NH)
      call FV_To_State ( STATE )
      if( gid==masterproc ) write(*,*) 'Non-Hydrostatic ICs Generated'
  endif 
 
  if ( (gid==0)                              ) print*, ' '
  if ( (gid==0) .and. (COLDSTART)            ) print*, 'COLDSTARTING FV3'
  if ( (gid==0) .and. (ADIABATIC)            ) print*, 'FV3 being run Adiabatically'
  if ( (gid==0) .and. (.not. FV_HYDROSTATIC) ) print*, 'FV3 being run Non-Hydrostatic'
  if ( (gid==0) .and. (FV_HYDROSTATIC)       ) print*, 'FV3 being run Hydrostatic'
  if ( (gid==0) .and. (SW_DYNAMICS)          ) print*, 'FV3 being run as Shallow-Water Model: test_case=', test_case
  if ( (gid==0) .and. (grid_type == 4)       ) print*, 'FV3 being run as Doubly-Periodic: test_case=', test_case
  if ( (gid==0)                              ) print*, ' '

  if (DEBUG) call debug_fv_state('DEBUG_RESTART',STATE)
!
! Write the vertical coordinate to STDOUT
!
  if( gid.eq.0 .and. .not. SW_DYNAMICS) then
        print *
        write(6,*) ' * denotes a layer within the "sponge-layer" of the dynamics'
        write(6,100)
100     format(2x,' k ','      A(k)    ',2x,' B(k)   ',2x,'  Pref    ',2x,'  DelP',/, &
               1x,'----',3x,'----------',2x,'--------',2x,'----------',2x,'---------' )
        k=0
        if (k<=n_sponge) then
          write(6,101) k+1,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k)
        else
          write(6,102) k+1,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k)
        endif
        do k=1,ubound(ak,1)
          if (k<=n_sponge) then
             write(6,103) k+1,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k), &
                          (ak(k)-ak(k-1))*0.01 + 1000.0*(bk(k)-bk(k-1))
          else
             write(6,104) k+1,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k), &
                          (ak(k)-ak(k-1))*0.01 + 1000.0*(bk(k)-bk(k-1))
          endif
        enddo

        print *
101     format('*',2x,i3,2x,f10.6,2x,f8.4,2x,f10.4)
102     format(' ',2x,i3,2x,f10.6,2x,f8.4,2x,f10.4)
103     format('*',2x,i3,2x,f10.6,2x,f8.4,2x,f10.4,3x,f8.4)
104     format(' ',2x,i3,2x,f10.6,2x,f8.4,2x,f10.4,3x,f8.4)
  endif

  call MAPL_MemUtilsWrite(VM, 'FV_StateMod: FV Initialize', RC=STATUS )
  VERIFY_(STATUS)

  RETURN_(ESMF_SUCCESS)

end subroutine FV_InitState

subroutine FV_Run (STATE, CLOCK, RC)

  type (T_FVDYCORE_STATE),pointer              :: STATE

  type (ESMF_Clock), target,     intent(IN   ) :: CLOCK
  integer, optional            , intent(OUT  ) :: RC

! Local variables
  integer              :: status
  character(len=ESMF_MAXSTR)       :: IAm='FV:FV_Run'

  type (ESMF_Time) :: fv_time
  integer  :: days, seconds
  real(REAL8) :: time_total, psmo

  integer :: n,nn
  integer :: isc,iec,jsc,jec,ng
  integer :: isd,ied,jsd,jed
  integer :: npx,npy,npz

! Splitting for Pure Advection
  real(REAL8) :: myDT

! Begin

  call ESMF_ClockGet( CLOCK, currTime=fv_time, rc=STATUS ) 
  VERIFY_(STATUS)
  call ESMF_TimeGet( fv_time, dayOfYear=days, s=seconds, rc=STATUS )
  VERIFY_(STATUS)

  time_total = days*86400. + seconds

  isc = FV_Atm(1)%isc
  iec = FV_Atm(1)%iec
  jsc = FV_Atm(1)%jsc
  jec = FV_Atm(1)%jec
  isd = FV_Atm(1)%isd
  ied = FV_Atm(1)%ied
  jsd = FV_Atm(1)%jsd
  jed = FV_Atm(1)%jed
  npx = FV_Atm(1)%npx
  npy = FV_Atm(1)%npy
  npz = FV_Atm(1)%npz
  ng  = FV_Atm(1)%ng

  ! Be sure we have the correct PHIS and number of tracers for this run
   if (fv_first_run) then
    ! Set surface geopotential
     FV_Atm(1)%phis(isc:iec,jsc:jec) = real(phis,kind=REAL8)
     call mpp_update_domains(FV_Atm(1)%phis, domain, complete=.true.)
    ! How many tracers do we really have?
     if (AdvCore_Advection/=0) then
        if (.not. ADIABATIC) then
           FV_Atm(1)%ncnst = 5 ! In GEOS-5 Q, QLCN, QLLS, QICN, QILS should be advected inline
        else
           FV_Atm(1)%ncnst = 1 ! Q=0.0
        endif
     else
        FV_Atm(1)%ncnst = STATE%GRID%NQ
     endif
     deallocate( FV_Atm(1)%q )
     allocate  ( FV_Atm(1)%q(isd:ied  ,jsd:jed  ,npz, FV_Atm(1)%ncnst) )
    ! Mark FV setup complete
     fv_first_run = .false.
   endif

 ! Pull Tracers
  if (.not. ADIABATIC) then
    FV_Atm(1)%q(:,:,:,:) = tiny_number
    nn=1
    do n=1,FV_Atm(1)%ncnst
       if (TRIM(state%vars%tracer(n)%tname) == 'Q') then
          if (state%vars%tracer(n)%is_r4) then
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,1) = state%vars%tracer(n)%content_r4(:,:,:)
          else
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,1) = state%vars%tracer(n)%content(:,:,:)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QLCN') then
          if (state%vars%tracer(n)%is_r4) then
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,2) = state%vars%tracer(n)%content_r4(:,:,:)
          else
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,2) = state%vars%tracer(n)%content(:,:,:)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QLLS') then
          if (state%vars%tracer(n)%is_r4) then
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,3) = state%vars%tracer(n)%content_r4(:,:,:)
          else
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,3) = state%vars%tracer(n)%content(:,:,:)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QICN') then
          if (state%vars%tracer(n)%is_r4) then
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,4) = state%vars%tracer(n)%content_r4(:,:,:)
          else
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,4) = state%vars%tracer(n)%content(:,:,:)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QILS') then
          if (state%vars%tracer(n)%is_r4) then
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,5) = state%vars%tracer(n)%content_r4(:,:,:)
          else
             FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,5) = state%vars%tracer(n)%content(:,:,:)
          endif
          nn=nn+1
       endif
    enddo
    if (AdvCore_Advection==0) then
      ASSERT_( nn == 6 )
      do n=1,FV_Atm(1)%ncnst
       if ( (TRIM(state%vars%tracer(n)%tname) /= 'Q')    .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QLCN') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QLLS') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QICN') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QILS') ) then
         if (state%vars%tracer(n)%is_r4) then
            FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn) = state%vars%tracer(n)%content_r4(:,:,:)
         else
            FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn) = state%vars%tracer(n)%content(:,:,:)
         endif
         nn=nn+1
       endif
      enddo
    endif ! AdvCore_Advection
  else
    if (gid==0) print*, 'Running In Adiabatic Mode'
    if (AdvCore_Advection==0) then
      nn = 1
      do n=1,FV_Atm(1)%ncnst
         if (state%vars%tracer(n)%is_r4) then
            FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn) = state%vars%tracer(n)%content_r4(:,:,:)
         else
            FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn) = state%vars%tracer(n)%content(:,:,:)
         endif
         nn=nn+1
      enddo
    endif ! AdvCore_Advection
  endif

    myDT = state%dt

    elapsed_time = elapsed_time + myDT

    if (DEBUG) call debug_fv_state('Before Dynamics Execution',STATE)


! Update FV with Internal State
    call State_To_FV( STATE )

    if (.not. FV_OFF) then
    call set_domain(FV_Atm(1)%domain)  ! needed for diagnostic output done in fv_dynamics
    call fv_dynamics(FV_Atm(1)%npx, FV_Atm(1)%npy, FV_Atm(1)%npz, FV_Atm(1)%ncnst, FV_Atm(1)%ng,   &
                     myDT, FV_Atm(1)%consv_te, FV_Atm(1)%fill, FV_Atm(1)%reproduce_sum, kappa,   &
                     cp, zvir, FV_Atm(1)%ks, FV_Atm(1)%ncnst, FV_Atm(1)%n_split, FV_Atm(1)%q_split, &
                     FV_Atm(1)%u, FV_Atm(1)%v, FV_Atm(1)%w, FV_Atm(1)%delz,       &
                     FV_Atm(1)%hydrostatic, FV_Atm(1)%pt, FV_Atm(1)%delp, FV_Atm(1)%q, FV_Atm(1)%ps,       &
                     FV_Atm(1)%pe, FV_Atm(1)%pk, FV_Atm(1)%peln, FV_Atm(1)%pkz,                         &
                     FV_Atm(1)%phis, FV_Atm(1)%omga, FV_Atm(1)%ua, FV_Atm(1)%va, FV_Atm(1)%uc, FV_Atm(1)%vc,  &
                     FV_Atm(1)%ak, FV_Atm(1)%bk, FV_Atm(1)%mfx, FV_Atm(1)%mfy, FV_Atm(1)%cx, FV_Atm(1)%cy,    &
                     FV_Atm(1)%ze0, FV_Atm(1)%hybrid_z, time_total)
    call nullify_domain()
    endif

! Check Surface Pressure
    if (check_surface_pressure) then
     psmo = globalsum(FV_Atm(1)%ps(isc:iec,jsc:jec), npx, npy, isc,iec, jsc,jec)
     if (psmo0 == -999.0) psmo0=psmo
     if(gid==masterproc) write(6,*) '         Total surface pressure change =', (psmo-psmo0)/psmo0
    endif

! Copy FV to internal State
   call FV_To_State ( STATE )

 ! Push Tracers
  if (.not. ADIABATIC) then
    nn = 1
    do n=1,FV_Atm(1)%ncnst
       if (TRIM(state%vars%tracer(n)%tname) == 'Q') then
          if (state%vars%tracer(n)%is_r4) then
             state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,1)
          else
                state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,1)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QLCN') then
          if (state%vars%tracer(n)%is_r4) then
             state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,2)
          else
                state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,2)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QLLS') then
          if (state%vars%tracer(n)%is_r4) then
             state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,3)
          else
                state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,3)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QICN') then
          if (state%vars%tracer(n)%is_r4) then
             state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,4)
          else
                state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,4)
          endif
          nn=nn+1
       endif
       if (TRIM(state%vars%tracer(n)%tname) == 'QILS') then
          if (state%vars%tracer(n)%is_r4) then
             state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,5)
          else
                state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,5)
          endif
          nn=nn+1
       endif
    enddo
    if (AdvCore_Advection==0) then
      ASSERT_( nn == 6 )
      do n=1,FV_Atm(1)%ncnst
       if ( (TRIM(state%vars%tracer(n)%tname) /= 'Q')    .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QLCN') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QLLS') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QICN') .and. &
            (TRIM(state%vars%tracer(n)%tname) /= 'QILS') ) then
         if (state%vars%tracer(n)%is_r4) then
            state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn)
         else
            state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn)
         endif
         nn=nn+1
       endif
      enddo
    endif ! AdvCore_Advection
  else
    if (AdvCore_Advection==0) then
      nn = 1
      do n=1,FV_Atm(1)%ncnst
         if (state%vars%tracer(n)%is_r4) then
            state%vars%tracer(n)%content_r4(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn)
         else
            state%vars%tracer(n)%content(:,:,:) = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,nn)
         endif
         nn=nn+1
      enddo
    endif ! AdvCore_Advection
  endif

    if (DEBUG) call debug_fv_state('After Dynamics Execution',STATE)

    RETURN_(ESMF_SUCCESS)

end subroutine FV_Run

 subroutine FV_Finalize (STATE)

  use fv_control_mod, only : fv_end

  type (T_FVDYCORE_STATE),pointer              :: STATE

  integer isc, iec, jsc, jec
  integer isd, ied, jsd, jed
  integer npz, ng

  isc = FV_Atm(1)%isc
  iec = FV_Atm(1)%iec
  jsc = FV_Atm(1)%jsc
  jec = FV_Atm(1)%jec
  isd = FV_Atm(1)%isd
  ied = FV_Atm(1)%ied
  jsd = FV_Atm(1)%jsd
  jed = FV_Atm(1)%jed
  npz = FV_Atm(1)%npz
  ng  = FV_Atm(1)%ng

    if (DEBUG) call debug_fv_state('FV_Finalize',STATE)

      call timing_off('TOTAL')
      call fv_end(FV_Atm)

#if defined( MAPL_MODE )
!    call ESMF_GridDestroy  (STATE%GRID%GRID)
#endif

 end subroutine FV_Finalize

subroutine State_To_FV ( STATE )

   use fv_control_mod, only : k_top
!
! !INPUT PARAMETERS:

   type(T_FVDYCORE_STATE),      pointer   :: STATE

    integer               :: ISC,IEC, JSC,JEC
    integer               :: ISD,IED, JSD,JED 
    integer               :: KM, NG
    integer               :: I,J,K
    real(REAL8)              :: akap

    real(REAL8) :: uatemp(state%grid%isd:state%grid%ied, &
                          state%grid%jsd:state%grid%jed,state%grid%npz)
    real(REAL8) :: vatemp(state%grid%isd:state%grid%ied, &
                          state%grid%jsd:state%grid%jed,state%grid%npz)

    real(REAL8) :: wbuffer(state%grid%js:state%grid%je,state%grid%npz)
    real(REAL8) :: sbuffer(state%grid%is:state%grid%ie,state%grid%npz)
    real(REAL8) :: ebuffer(state%grid%js:state%grid%je,state%grid%npz)
    real(REAL8) :: nbuffer(state%grid%is:state%grid%ie,state%grid%npz)

    ISC = state%grid%is
    IEC = state%grid%ie
    JSC = state%grid%js
    JEC = state%grid%je
    ISD = state%grid%isd
    IED = state%grid%ied
    JSD = state%grid%jsd
    JED = state%grid%jed
    KM  = state%grid%npz
    NG  = state%grid%ng

    akap  = kappa
    if (SW_DYNAMICS) akap  = 1.

!------------
! Update Winds
!------------

  FV_Atm(1)%u(:,:,:) = tiny_number
  FV_Atm(1)%v(:,:,:) = tiny_number
  if (grid_type>=4) then
  ! Doubly Periodic
    uatemp(isc:iec,jsc:jec,:) = STATE%VARS%U
    vatemp(isc:iec,jsc:jec,:) = STATE%VARS%V
    call mpp_update_domains(uatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    FV_Atm(1)%u(isc:iec,jsc:jec+1,:) = uatemp(isc:iec,jsc:jec+1,:)
    FV_Atm(1)%v(isc:iec+1,jsc:jec,:) = vatemp(isc:iec+1,jsc:jec,:)
  else
    FV_Atm(1)%u(isc:iec,jsc:jec,:) = STATE%VARS%U
    FV_Atm(1)%v(isc:iec,jsc:jec,:) = STATE%VARS%V
    call mpp_get_boundary(FV_Atm(1)%u, FV_Atm(1)%v, domain, &
                          wbuffery=wbuffer, ebuffery=ebuffer, &
                          sbufferx=sbuffer, nbufferx=nbuffer, &
                          gridtype=DGRID_NE, complete=.true. )
    do k=1,km
       do i=isc,iec
          FV_Atm(1)%u(i,jec+1,k) = nbuffer(i,k)
       enddo
    enddo
    do k=1,km
       do j=jsc,jec
          FV_Atm(1)%v(iec+1,j,k) = ebuffer(j,k)
       enddo
    enddo
  endif

    if (.not. FV_Atm(1)%hydrostatic) FV_Atm(1)%w(isc:iec,jsc:jec,:) = STATE%VARS%W
 
!------------
! Update Pressures
!------------

   FV_Atm(1)%pe(:,:,:) = tiny_number
   if (SW_DYNAMICS) then
      do k=1,km+1
        do j=jsc,jec
          do i=isc,iec
            FV_Atm(1)%pe(i,k,j)   = STATE%VARS%PE(i,j,k)
          enddo
        enddo
      enddo
   else
      do k=1,km+1
        do j=jsc,jec
          do i=isc,iec
            FV_Atm(1)%pe(i,k,j)   = STATE%VARS%PE(i,j,k)
          enddo
        enddo
      enddo

      do k=1,km+1
        do j=jsc,jec
          do i=isc,iec
            FV_Atm(1)%peln(i,k,j) = log(FV_Atm(1)%pe(i,k,j))
          enddo
        enddo
      enddo

      do k=1,km+1
        do j=jsc,jec
          do i=isc,iec
            FV_Atm(1)%pk(i,j,k)   = exp( akap*FV_Atm(1)%peln(i,k,j) )
          enddo
        enddo
      enddo

      FV_Atm(1)%ps(isc:iec,jsc:jec) = FV_Atm(1)%pe(isc:iec,km+1,jsc:jec)

   endif

    FV_Atm(1)%delp(:,:,:) = tiny_number
    do k=1,km
      do j=jsc,jec
        do i=isc,iec
          FV_Atm(1)%delp(i,j,k) = FV_Atm(1)%pe(i,k+1,j) - FV_Atm(1)%pe(i,k,j) 
        enddo
      enddo
    enddo

    if (.not. SW_DYNAMICS) then

!-----------------------
! Copy PT and make Dry T 
!-----------------------
       FV_Atm(1)%pt(:,:,:) = tiny_number
       FV_Atm(1)%pt(isc:iec,jsc:jec,:) = STATE%VARS%PT*STATE%VARS%PKZ

!------------
! Get delz
!------------
       if (.not. FV_Atm(1)%hydrostatic) FV_Atm(1)%delz(isc:iec,jsc:jec,:) = STATE%VARS%DZ

!------------------------------------------------------------------------------
! Get pkz
!------------------------------------------------------------------------------
       FV_Atm(1)%pkz(isc:iec,jsc:jec,:) = STATE%VARS%PKZ

    endif


   return

end subroutine State_To_FV

subroutine FV_To_State ( STATE )

!
! !INPUT PARAMETERS:

   type(T_FVDYCORE_STATE),      pointer   :: STATE

    integer               :: ISC,IEC, JSC,JEC, KM
    integer               :: I,J

    ISC = state%grid%is
    IEC = state%grid%ie
    JSC = state%grid%js
    JEC = state%grid%je
    KM  = state%grid%npz

! Copy updated FV data to internal state
    STATE%VARS%U(:,:,:) = FV_Atm(1)%u(isc:iec,jsc:jec,:)
    STATE%VARS%V(:,:,:) = FV_Atm(1)%v(isc:iec,jsc:jec,:)
    if (.not. FV_Atm(1)%hydrostatic) STATE%VARS%W = FV_Atm(1)%w(isc:iec,jsc:jec,:)

    if (SW_DYNAMICS) then
       STATE%VARS%PE(:,:,1) = FV_Atm(1)%phis(isc:iec,jsc:jec)
       STATE%VARS%PE(:,:,2) = FV_Atm(1)%phis(isc:iec,jsc:jec) + FV_Atm(1)%delp(isc:iec,jsc:jec,1)
    else
       do j=jsc,jec
          do i=isc,iec
             STATE%VARS%PE(i,j,:) = FV_Atm(1)%pe(i,:,j)
          enddo
       enddo

!-----------------------------------
! Fill Dry Temperature to PT
!-----------------------------------
       STATE%VARS%PT  = FV_Atm(1)%pt(isc:iec,jsc:jec,:)

!------------------------------
! Get delz from FV3
!------------------------------
       if (.not. FV_Atm(1)%hydrostatic) STATE%VARS%DZ = FV_Atm(1)%delz(isc:iec,jsc:jec,:)
       
!--------------------------------
! Get pkz from FV3
!--------------------------------
       STATE%VARS%PKZ = FV_Atm(1)%pkz(isc:iec,jsc:jec,:)

!---------------------------------------------------------------------
! Convert to Dry Temperature to PT with hydrostatic pkz
!---------------------------------------------------------------------
       STATE%VARS%PT  = STATE%VARS%PT/STATE%VARS%PKZ
    endif

   return

end subroutine FV_To_State

subroutine fv_getDELZ(delz,temp,pe)
  real(REAL8), intent(OUT) :: delz(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent( IN) :: temp(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent( IN) ::   pe(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8) :: peln(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8) :: rdg
  peln = log(pe)
  rdg   = -rgas / grav
  delz = rdg*temp*(peln(:,:,2:)-peln(:,:,1:))
return 
end subroutine fv_getDELZ

subroutine fv_getQ(Q, qNAME)
  real(REAL8), intent(OUT) :: Q(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  character(LEN=*), intent(IN) :: qNAME
  integer :: isc,iec, jsc,jec, npz
  isc = FV_Atm(1)%isc
  iec = FV_Atm(1)%iec
  jsc = FV_Atm(1)%jsc
  jec = FV_Atm(1)%jec
  npz = FV_Atm(1)%npz
  if (TRIM(qNAME) == 'Q'   ) Q = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,1)
  if (TRIM(qNAME) == 'QLCN') Q = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,2)
  if (TRIM(qNAME) == 'QLLS') Q = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,3)
  if (TRIM(qNAME) == 'QICN') Q = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,4)
  if (TRIM(qNAME) == 'QILS') Q = FV_Atm(1)%q(isc:iec,jsc:jec,1:npz,5)

return
end subroutine fv_getQ

subroutine fv_getPKZ(pkz,temp,qv,pe,delz,HYDROSTATIC)
  use fv_control_mod, only : k_top
  real(REAL8), intent(OUT) ::  pkz(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent( IN) :: temp(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent( IN) ::   qv(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent( IN) ::   pe(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8), intent( IN) :: delz(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  logical    , intent( IN) :: HYDROSTATIC
! Local
  real(REAL8) ::   pk(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8) :: peln(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8) :: delp(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8) :: rdg
  integer :: i,j,k, isc,iec, jsc,jec, npz

  isc = FV_Atm(1)%isc
  iec = FV_Atm(1)%iec
  jsc = FV_Atm(1)%jsc
  jec = FV_Atm(1)%jec
  npz = FV_Atm(1)%npz

  rdg  = -rgas / grav
  peln = log(pe)
  pk   = exp( kappa*peln )
  delp = pe(:,:,2:)-pe(:,:,1:)

!OLD
! if (HYDROSTATIC) then
!    peln = log(pe)
!    pkz = (pe(:,:,2:)**kappa-pe(:,:,1:)**kappa) / (kappa*(peln(:,:,2:)-peln(:,:,1:)))
! else
!    delp = pe(:,:,2:)-pe(:,:,1:)
! !  temp should be dry temperature made virtual by QV
!    pkz = exp( kappa*log(rdg*delp*temp*(1.d0+zvir*qv)/delz) )
! endif
!OLD

!-------------------------------------------------------------------------
! Re-compute the full (nonhydrostatic) pressure due to temperature changes
!-------------------------------------------------------------------------
    if ( .not.hydrostatic ) then
      if ( k_top>1 ) then
!$omp parallel do default(shared)
         do k=1,k_top-1
            do j=jsc,jec
               do i=isc,iec
                  pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k)) /     &
                      (kappa*(peln(i,j,k+1)-peln(i,j,k)))
               enddo
            enddo
         enddo
      endif

!$omp parallel do default(shared)
      do k=k_top,npz
         do j=jsc,jec
            do i=isc,iec
! perfect gas law: p = density * rdgas * virtual_temperature
!              pkz(i,j,k) = ( rdg*delp(i,j,k)*pt(i,j,k)/delz(i,j,k) )**kappa
               pkz(i,j,k) = exp( kappa*log(rdg*delp(i,j,k)*temp(i,j,k)*    &
                                      (1.d0+zvir*qv(i,j,k))/delz(i,j,k)) )
            enddo
         enddo
      enddo
    else
!$omp parallel do default(shared)
      do k=1,npz
         do j=jsc,jec
            do i=isc,iec
               pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k)) /     &
                   (kappa*(peln(i,j,k+1)-peln(i,j,k)))
            enddo
         enddo
      enddo
    endif


return
end subroutine fv_getPKZ


subroutine a2d3d(ua, va, ud, vd)

! Move A-Grid winds/tendencies oriented on lat/lon to the D-grid cubed-sphere orientation

! !INPUT/OUTPUT PARAMETERS:
      real(REAL8)                :: ua(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec  ,FV_Atm(1)%npz) ! U-Wind
      real(REAL8)                :: va(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec  ,FV_Atm(1)%npz) ! V-Wind
      real(REAL8), intent(inout) :: ud(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec+1,FV_Atm(1)%npz) ! U-Wind
      real(REAL8), intent(inout) :: vd(FV_Atm(1)%isc:FV_Atm(1)%iec+1,FV_Atm(1)%jsc:FV_Atm(1)%jec  ,FV_Atm(1)%npz) ! V-Wind
! !Local Variables
      integer :: is ,ie , js ,je 
      integer :: npx, npy, npz
      integer :: i,j,k, im2,jm2

      real(REAL8) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,FV_Atm(1)%npz)
      real(REAL8) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,FV_Atm(1)%npz)

      real(REAL8) :: v3(FV_Atm(1)%isc-1:FV_Atm(1)%iec+1,FV_Atm(1)%jsc-1:FV_Atm(1)%jec+1,3)
      real(REAL8) :: ue(FV_Atm(1)%isc-1:FV_Atm(1)%iec+1,FV_Atm(1)%jsc  :FV_Atm(1)%jec+1,3)    ! 3D winds at edges
      real(REAL8) :: ve(FV_Atm(1)%isc  :FV_Atm(1)%iec+1,FV_Atm(1)%jsc-1:FV_Atm(1)%jec+1,3)    ! 3D winds at edges
      real(REAL8), dimension(FV_Atm(1)%isc:FV_Atm(1)%iec):: ut1, ut2, ut3
      real(REAL8), dimension(FV_Atm(1)%jsc:FV_Atm(1)%jec):: vt1, vt2, vt3

      npx = FV_Atm(1)%npx
      npy = FV_Atm(1)%npy
      npz = FV_Atm(1)%npz
      is  = FV_Atm(1)%isc
      ie  = FV_Atm(1)%iec
      js  = FV_Atm(1)%jsc
      je  = FV_Atm(1)%jec

      im2 = (npx-1)/2
      jm2 = (npy-1)/2

    uatemp(is:ie,js:je,:) = ua
    vatemp(is:ie,js:je,:) = va

    if (grid_type<4) then
   ! Cubed-Sphere
    call mpp_update_domains(uatemp, domain, complete=.false.)
    call mpp_update_domains(vatemp, domain, complete=.true.)
    do k=1, npz
! Compute 3D wind tendency on A grid
       do j=js-1,je+1
          do i=is-1,ie+1
             v3(i,j,1) = uatemp(i,j,k)*vlon(i,j,1) + vatemp(i,j,k)*vlat(i,j,1)
             v3(i,j,2) = uatemp(i,j,k)*vlon(i,j,2) + vatemp(i,j,k)*vlat(i,j,2)
             v3(i,j,3) = uatemp(i,j,k)*vlon(i,j,3) + vatemp(i,j,k)*vlat(i,j,3)
          enddo
       enddo

! A --> D
! Interpolate to cell edges
       do j=js,je+1
          do i=is-1,ie+1
             ue(i,j,1) = v3(i,j-1,1) + v3(i,j,1)
             ue(i,j,2) = v3(i,j-1,2) + v3(i,j,2)
             ue(i,j,3) = v3(i,j-1,3) + v3(i,j,3)
          enddo
       enddo

       do j=js-1,je+1
          do i=is,ie+1
             ve(i,j,1) = v3(i-1,j,1) + v3(i,j,1)
             ve(i,j,2) = v3(i-1,j,2) + v3(i,j,2)
             ve(i,j,3) = v3(i-1,j,3) + v3(i,j,3)
          enddo
       enddo

! --- E_W edges (for v-wind):
     if ( is==1 ) then
       i = 1
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_w(j)*ve(i,j-1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j-1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j-1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_w(j)*ve(i,j+1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j+1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j+1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
     if ( (ie+1)==npx ) then
       i = npx
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_e(j)*ve(i,j-1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j-1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j-1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_e(j)*ve(i,j+1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j+1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j+1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
! N-S edges (for u-wind):
     if ( js==1 ) then
       j = 1
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_s(i)*ue(i-1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i-1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i-1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_s(i)*ue(i+1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i+1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i+1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif
     if ( (je+1)==npy ) then
       j = npy
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_n(i)*ue(i-1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i-1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i-1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_n(i)*ue(i+1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i+1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i+1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif

! Update:
       do j=js,je+1
          do i=is,ie
             ud(i,j,k) = 0.5*( ue(i,j,1)*es(1,i,j,1) +  &
                               ue(i,j,2)*es(2,i,j,1) +  &
                               ue(i,j,3)*es(3,i,j,1) )
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             vd(i,j,k) = 0.5*( ve(i,j,1)*ew(1,i,j,2) +  &
                               ve(i,j,2)*ew(2,i,j,2) +  &
                               ve(i,j,3)*ew(3,i,j,2) )
          enddo
       enddo

    enddo         ! k-loop
   else
   ! Cartesian
    call mpp_update_domains(uatemp, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    do k=1,npz
       do j=js,je+1
          do i=is,ie
             ud(i,j,k) = 0.5*( uatemp(i,j,k) + uatemp(i,j-1,k) )
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             vd(i,j,k) = 0.5*( vatemp(i,j,k) + vatemp(i-1,j,k) )
          enddo
       enddo
    enddo         ! k-loop
   endif

end subroutine a2d3d

subroutine a2d2d(ua, va, ud, vd)

! Move A-Grid winds/tendencies oriented on lat/lon to the D-grid cubed-sphere orientation

! !INPUT/OUTPUT PARAMETERS:
      real(REAL8)                :: ua(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec  ) ! U-Wind
      real(REAL8)                :: va(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec  ) ! V-Wind
      real(REAL8), intent(inout) :: ud(FV_Atm(1)%isc:FV_Atm(1)%iec  ,FV_Atm(1)%jsc:FV_Atm(1)%jec+1) ! U-Wind
      real(REAL8), intent(inout) :: vd(FV_Atm(1)%isc:FV_Atm(1)%iec+1,FV_Atm(1)%jsc:FV_Atm(1)%jec  ) ! V-Wind
! !Local Variables
      integer :: is ,ie , js ,je
      integer :: npx, npy
      integer :: i,j, im2,jm2

      real(REAL8) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed)
      real(REAL8) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed)

      real(REAL8) :: v3(FV_Atm(1)%isc-1:FV_Atm(1)%iec+1,FV_Atm(1)%jsc-1:FV_Atm(1)%jec+1,3)
      real(REAL8) :: ue(FV_Atm(1)%isc-1:FV_Atm(1)%iec+1,FV_Atm(1)%jsc  :FV_Atm(1)%jec+1,3)    ! 3D winds at edges
      real(REAL8) :: ve(FV_Atm(1)%isc  :FV_Atm(1)%iec+1,FV_Atm(1)%jsc-1:FV_Atm(1)%jec+1,3)    ! 3D winds at edges
      real(REAL8), dimension(FV_Atm(1)%isc:FV_Atm(1)%iec):: ut1, ut2, ut3
      real(REAL8), dimension(FV_Atm(1)%jsc:FV_Atm(1)%jec):: vt1, vt2, vt3

      npx = FV_Atm(1)%npx
      npy = FV_Atm(1)%npy
      is  = FV_Atm(1)%isc
      ie  = FV_Atm(1)%iec
      js  = FV_Atm(1)%jsc
      je  = FV_Atm(1)%jec

      im2 = (npx-1)/2
      jm2 = (npy-1)/2

    uatemp(is:ie,js:je) = ua
    vatemp(is:ie,js:je) = va

    call mpp_update_domains(uatemp, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)

    if (grid_type<4) then
   ! Cubed-Sphere
! Compute 3D wind tendency on A grid
       do j=js-1,je+1
          do i=is-1,ie+1
             v3(i,j,1) = uatemp(i,j)*vlon(i,j,1) + vatemp(i,j)*vlat(i,j,1)
             v3(i,j,2) = uatemp(i,j)*vlon(i,j,2) + vatemp(i,j)*vlat(i,j,2)
             v3(i,j,3) = uatemp(i,j)*vlon(i,j,3) + vatemp(i,j)*vlat(i,j,3)
          enddo
       enddo

! A --> D
! Interpolate to cell edges
       do j=js,je+1
          do i=is-1,ie+1
             ue(i,j,1) = v3(i,j-1,1) + v3(i,j,1)
             ue(i,j,2) = v3(i,j-1,2) + v3(i,j,2)
             ue(i,j,3) = v3(i,j-1,3) + v3(i,j,3)
          enddo
       enddo

       do j=js-1,je+1
          do i=is,ie+1
             ve(i,j,1) = v3(i-1,j,1) + v3(i,j,1)
             ve(i,j,2) = v3(i-1,j,2) + v3(i,j,2)
             ve(i,j,3) = v3(i-1,j,3) + v3(i,j,3)
          enddo
       enddo

! --- E_W edges (for v-wind):
     if ( is==1 ) then
       i = 1
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_w(j)*ve(i,j-1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j-1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j-1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_w(j)*ve(i,j+1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j+1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j+1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
     if ( (ie+1)==npx ) then
       i = npx
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_e(j)*ve(i,j-1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j-1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j-1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_e(j)*ve(i,j+1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j+1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j+1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
! N-S edges (for u-wind):
     if ( js==1 ) then
       j = 1
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_s(i)*ue(i-1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i-1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i-1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_s(i)*ue(i+1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i+1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i+1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif
     if ( (je+1)==npy ) then
       j = npy
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_n(i)*ue(i-1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i-1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i-1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_n(i)*ue(i+1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i+1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i+1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif

! Update:
       do j=js,je+1
          do i=is,ie
             ud(i,j) = 0.5*( ue(i,j,1)*es(1,i,j,1) +  &
                             ue(i,j,2)*es(2,i,j,1) +  &
                             ue(i,j,3)*es(3,i,j,1) )
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             vd(i,j) = 0.5*( ve(i,j,1)*ew(1,i,j,2) +  &
                             ve(i,j,2)*ew(2,i,j,2) +  &
                             ve(i,j,3)*ew(3,i,j,2) )
          enddo
       enddo
   else
   ! Cartesian
       do j=js,je+1
          do i=is,ie
             ud(i,j) = 0.5*( uatemp(i,j) + uatemp(i,j-1) )
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             vd(i,j) = 0.5*( vatemp(i,j) + vatemp(i-1,j) )
          enddo
       enddo
   endif

end subroutine a2d2d

subroutine fv_getTopography(phis)
  use fv_control_mod,     only: npx,npy
  use fv_grid_tools_mod,  only: grid, agrid, dxc, dyc
  use fv_surf_map_mod,    only: surfdrv
  real(kind=4), intent(INOUT) ::   phis(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)
  real(REAL8)           :: m_phis(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed)

  call surfdrv(npx, npy, grid, agrid, area, dx, dy, dxc, dyc,  &
               m_phis, gid==masterproc)
  call mpp_update_domains( m_phis, domain, complete=.true. )

  phis = m_phis(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)

end subroutine fv_getTopography

subroutine fv_computeMassFluxes(ucI, vcI, ple, mfx, mfy, cx, cy, dt)
  use tp_core_mod,       only: fv_tp_2d
  use fv_control_mod,    only: npx,npy,npz,hord_dp,n_split
  use fv_grid_utils_mod, only: sina_u, sina_v, sin_sg
  use fv_mp_mod,         only: is,js,ie,je, isd,jsd,ied,jed, mp_reduce_max
  use fv_tracer2d_mod,   only: offline_tracer_advection
  real(REAL8), intent(IN   ) ::  ucI(is:ie,js:je,1:npz)
  real(REAL8), intent(IN   ) ::  vcI(is:ie,js:je,1:npz)
  real(REAL8), intent(IN   ) ::  ple(is:ie,js:je,1:npz+1)
  real(REAL8), intent(INOUT) ::  mfx(is:ie,js:je,1:npz)
  real(REAL8), intent(INOUT) ::  mfy(is:ie,js:je,1:npz)
  real(REAL8), intent(INOUT) ::   cx(is:ie,js:je,1:npz)
  real(REAL8), intent(INOUT) ::   cy(is:ie,js:je,1:npz)
  real(REAL8), intent(IN   ) :: dt
  integer i,j,k

! Local ghosted arrays
  real(REAL8) ::  uc(isd:ied+1,jsd:jed  ,1:npz)
  real(REAL8) ::  vc(isd:ied  ,jsd:jed+1,1:npz)
  real(REAL8) ::  ut(isd:ied+1,jsd:jed  )
  real(REAL8) ::  vt(isd:ied  ,jsd:jed+1)

  real(REAL8) ::  crx(is :ie +1,jsd:jed  )
  real(REAL8) ::  cry(isd:ied  ,js :je +1)
  real(REAL8) ::  xfx(is :ie +1,jsd:jed  )
  real(REAL8) ::  yfx(isd:ied  ,js :je +1)
  real(REAL8) :: ra_x(is :ie   ,jsd:jed  )
  real(REAL8) :: ra_y(isd:ied  ,js :je   )
  real(REAL8) ::   fx(is :ie +1,js :je   )
  real(REAL8) ::   fy(is :ie   ,js :je +1)

  real(REAL8) ::  delp(isd:ied,jsd:jed)

  real(REAL8) :: wbuffer(js:je,npz)
  real(REAL8) :: sbuffer(is:ie,npz)
  real(REAL8) :: ebuffer(js:je,npz)
  real(REAL8) :: nbuffer(is:ie,npz)

  real(REAL8) :: cmax, frac
  integer     :: it, nsplt

! Fill Ghosted arrays and update halos
  uc(is:ie,js:je,:) = ucI
  vc(is:ie,js:je,:) = vcI
  call mpp_get_boundary(uc, vc, domain, &
                        wbufferx=wbuffer, ebufferx=ebuffer, &
                        sbuffery=sbuffer, nbuffery=nbuffer, &
                        gridtype=CGRID_NE, complete=.true. )
  do k=1,npz
     do j=js,je
        uc(ie+1,j,k) = ebuffer(j,k)
     enddo
     do i=is,ie
        vc(i,je+1,k) = nbuffer(i,k)
     enddo
  enddo
  call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE, complete=.true.)

  do k=1,npz
    ! Prepare pressures for Mass Flux calculations
     delp(is:ie,js:je) = ple(:,:,k+1)-ple(:,:,k) 

     call compute_utvt(uc(isd,jsd,k), vc(isd,jsd,k), ut(isd,jsd), vt(isd,jsd), dt)
     do j=jsd,jed
        do i=is,ie+1
           xfx(i,j) = dt*ut(i,j)
        enddo
     enddo
     do j=js,je+1
        do i=isd,ied
           yfx(i,j) = dt*vt(i,j)
        enddo
     enddo
     do j=jsd,jed
        do i=is,ie+1
           if ( xfx(i,j) > 0. ) then
              crx(i,j) = xfx(i,j) * rdxa(i-1,j)
              xfx(i,j) = dy(i,j)*xfx(i,j)*sin_sg(i-1,j,3)
           else
              crx(i,j) = xfx(i,j) * rdxa(i,j)
              xfx(i,j) = dy(i,j)*xfx(i,j)*sin_sg(i,j,1)
           endif
        enddo
     enddo
     do j=js,je+1
        do i=isd,ied
           if ( yfx(i,j) > 0. ) then
              cry(i,j) = yfx(i,j) * rdya(i,j-1)
              yfx(i,j) = dx(i,j)*yfx(i,j)*sin_sg(i,j-1,4)
           else
              cry(i,j) = yfx(i,j) * rdya(i,j)
              yfx(i,j) = dx(i,j)*yfx(i,j)*sin_sg(i,j,2)
           endif
        enddo
     enddo

!#define SIMPLE_FV_MASS_FLUXES
#ifndef SIMPLE_FV_MASS_FLUXES
     if ( n_split==0 ) then
! Determine nsplt for tracer advection
        cmax = 0.
        do j=js,je
           do i=is,ie
              cmax = max(abs(crx(i,j))+(1.-sina_u(i,j)),     &
                         abs(cry(i,j))+(1.-sina_v(i,j)), cmax)
           enddo
        enddo
        call mp_reduce_max(cmax)
        nsplt = int(1.01 + cmax)
        if ( gid == 0 )  write(6,*) k, 'Tracer_2d_split=', nsplt, cmax
     else
        nsplt = n_split
     endif
     frac  = 1. / real(nsplt)
     crx = crx*frac
     cry = cry*frac
     xfx = xfx*frac
     yfx = yfx*frac
     do j=jsd,jed
        do i=is,ie
           ra_x(i,j) = area(i,j) + xfx(i,j) - xfx(i+1,j)
        enddo
     enddo
     do j=js,je
        do i=isd,ied
           ra_y(i,j) = area(i,j) + yfx(i,j) - yfx(i,j+1)
        enddo
     enddo
! Zero out accumulated mass fluxes and courant numbers
      cx(:,:,k) = 0.0
     mfx(:,:,k) = 0.0
      cy(:,:,k) = 0.0
     mfy(:,:,k) = 0.0
! Compute mass fluxes using FV3 advection
     do it=1,nsplt
        call mpp_update_domains( delp, domain, complete=.true. )
        call fv_tp_2d(delp, crx, cry, npx, npy, &
                      hord_dp, fx, fy, xfx, yfx, ra_x, ra_y)
! Update delp
        do j=js,je
           do i=is,ie
              delp(i,j) = delp(i,j) + (fx(i,j) - fx(i+1,j) +  &
                                       fy(i,j) - fy(i,j+1)) * rarea(i,j)
           enddo
        enddo
! Accumulate Mass Fluxes and Courant Number outputs
         cx(:,:,k) =  cx(:,:,k) + crx(is:ie,js:je)
        mfx(:,:,k) = mfx(:,:,k) +  fx(is:ie,js:je)
         cy(:,:,k) =  cy(:,:,k) + cry(is:ie,js:je)
        mfy(:,:,k) = mfy(:,:,k) +  fy(is:ie,js:je)
     enddo
#else
! Fill DELP ghost zones
  call mpp_update_domains(delp, domain, complete=.true.)
! Compute Mass Fluxes and Fill Courant number outputs
     do j=js,je
        do i=is,ie
       ! X-Dir 
         if (crx(i,j) > 0.) then
            fx(i,j) = xfx(i,j) * delp(i-1,j)
         else
            fx(i,j) = xfx(i,j) * delp(i,j)
         endif
       ! Y-Dir 
         if (cry(i,j) > 0.) then
            fy(i,j) = yfx(i,j) * delp(i,j-1)
         else
            fy(i,j) = yfx(i,j) * delp(i,j)
         endif
      enddo
    enddo
! Fill Mass Fluxes and Fill Courant number outputs
      cx(:,:,k) = crx(is:ie,js:je)
     mfx(:,:,k) =  fx(is:ie,js:je)
      cy(:,:,k) = cry(is:ie,js:je)
     mfy(:,:,k) =  fy(is:ie,js:je)
#endif
  enddo

return
end subroutine fv_computeMassFluxes

subroutine fv_fillMassFluxes(mfx, mfy, cx, cy)
  real(REAL8), intent(OUT) :: mfx(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) :: mfy(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) ::  cx(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) ::  cy(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  integer isc,iec,jsc,jec

  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec

  mfx(:,:,:) = FV_Atm(1)%mfx(isc:iec,jsc:jec,:)
  mfy(:,:,:) = FV_Atm(1)%mfy(isc:iec,jsc:jec,:)
   cx(:,:,:) = FV_Atm(1)%cx(isc:iec,jsc:jec,:) 
   cy(:,:,:) = FV_Atm(1)%cy(isc:iec,jsc:jec,:) 

return
end subroutine fv_fillMassFluxes

subroutine fv_getVerticalMassFlux(mfx, mfy, mfz, dt)
  real(REAL8), intent(IN   ) :: mfx(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(IN   ) :: mfy(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(  OUT) :: mfz(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  real(REAL8), intent(IN)  :: dt

  real(REAL8) :: conv(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8) :: pit(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)

  real(REAL8) :: fac

  real(REAL8) :: wbuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: sbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)
  real(REAL8) :: ebuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: nbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)
  real(REAL8) :: xfx(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(REAL8) :: yfx(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1,1:FV_Atm(1)%npz)

  integer isc,iec,jsc,jec,npz,i,j,k
  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  npz=FV_Atm(1)%npz

! Fill Ghosted arrays and update halos
  xfx(isc:iec,jsc:jec,:) = mfx
  yfx(isc:iec,jsc:jec,:) = mfy
  call mpp_get_boundary(xfx, yfx, domain, &
                        wbufferx=wbuffer, ebufferx=ebuffer, &
                        sbuffery=sbuffer, nbuffery=nbuffer, &
                        gridtype=CGRID_NE, complete=.true. )
  do k=1,npz
     do j=jsc,jec
        xfx(iec+1,j,k) = ebuffer(j,k)
     enddo
     do i=isc,iec
        yfx(i,jec+1,k) = nbuffer(i,k)
     enddo
  enddo

  fac = 1.0/(dt*MAPL_GRAV)
!
! Compute the vertical mass flux
!
!   Compute Convergence of the horizontal Mass flux
    do k=1,npz
       do j=jsc,jec
          do i=isc,iec
             conv(i,j,k) = ( xfx(i,j,k) - xfx(i+1,j,k) +  &
                             yfx(i,j,k) - yfx(i,j+1,k) ) * fac
          enddo
       enddo
    enddo
!   Surface pressure tendency
    pit(:,:) = 0.0
    do k=1,npz
       do j=jsc,jec
          do i=isc,iec
             pit(i,j) = pit(i,j) + conv(i,j,k)
          enddo
       enddo
    enddo
!   Sum over levels
    do k=2,npz
       do j=jsc,jec
          do i=isc,iec
             conv(i,j,k) = conv(i,j,k) + conv(i,j,k-1)
          enddo
       enddo
    enddo
    mfz(:,:,:) = 0.0
    do k=2,npz
       do j=jsc,jec
          do i=isc,iec
             mfz(i,j,k) = ( conv(i,j,k-1)  - FV_Atm(1)%bk(k)*pit(i,j) )/(MAPL_GRAV*area(i,j))  ! Kg/m^2/s
          enddo
       enddo
    enddo

return
end subroutine fv_getVerticalMassFlux

subroutine compute_utvt(uc, vc, ut, vt, dt)
 use fv_control_mod,    only: npx,npy
 use fv_mp_mod,         only: is,js,ie,je, isd,jsd,ied,jed
 use fv_grid_utils_mod, only: sw_corner, se_corner, ne_corner, nw_corner
 use fv_grid_utils_mod, only: sin_sg=>sin_sg,  &
                              cosa_u=>cosa_u,   cosa_v=>  cosa_v, &
                              rsin_u=>rsin_u,   rsin_v=>  rsin_v
  real(REAL8), intent(IN   ) ::  uc(isd:ied+1,jsd:jed  )
  real(REAL8), intent(IN   ) ::  vc(isd:ied  ,jsd:jed+1)
  real(REAL8), intent(INOUT) ::  ut(isd:ied+1,jsd:jed  )
  real(REAL8), intent(INOUT) ::  vt(isd:ied  ,jsd:jed+1)
  real(REAL8), intent(IN   ) :: dt
! Local vars
  real(REAL8) :: damp
  integer i,j

  if ( grid_type < 3 ) then

! Center of Cube Faces
        do j=jsd,jed
           if(j/=0 .and. j/=1 .and. j/=(npy-1) .and. j/=npy) then
             do i=is-1,ie+2
                ut(i,j) = ( uc(i,j) - 0.25 * cosa_u(i,j) *     &
                    (vc(i-1,j)+vc(i,j)+vc(i-1,j+1)+vc(i,j+1)))*rsin_u(i,j)
             enddo
           endif
        enddo
        do j=js-1,je+2
           if( j/=1 .and. j/=npy ) then
              do i=isd,ied
                 vt(i,j) = ( vc(i,j) - 0.25 * cosa_v(i,j) *     &
                    (uc(i,j-1)+uc(i+1,j-1)+uc(i,j)+uc(i+1,j)))*rsin_v(i,j)
              enddo
           endif
        enddo

! West edge:
       if ( is==1 ) then
          do j=jsd,jed
             if ( uc(1,j)*dt > 0. ) then
                ut(1,j) = uc(1,j) / sin_sg(0,j,3)
             else
                ut(1,j) = uc(1,j) / sin_sg(1,j,1)
             endif
          enddo
          do j=max(3,js), min(npy-2,je+1)
             vt(0,j) = vc(0,j) - 0.25*cosa_v(0,j)*   &
                  (ut(0,j-1)+ut(1,j-1)+ut(0,j)+ut(1,j))
             vt(1,j) = vc(1,j) - 0.25*cosa_v(1,j)*   &
                  (ut(1,j-1)+ut(2,j-1)+ut(1,j)+ut(2,j))
          enddo
       endif   ! West face

! East edge:
       if ( (ie+1)==npx ) then
          do j=jsd,jed
             if ( uc(npx,j)*dt > 0. ) then
                ut(npx,j) = uc(npx,j) / sin_sg(npx-1,j,3)
             else
                ut(npx,j) = uc(npx,j) / sin_sg(npx,j,1)
             endif
          enddo

           do j=max(3,js), min(npy-2,je+1)
              vt(npx-1,j) = vc(npx-1,j) - 0.25*cosa_v(npx-1,j)*   &
                           (ut(npx-1,j-1)+ut(npx,j-1)+ut(npx-1,j)+ut(npx,j))
              vt(npx,j) = vc(npx,j) - 0.25*cosa_v(npx,j)*   &
                         (ut(npx,j-1)+ut(npx+1,j-1)+ut(npx,j)+ut(npx+1,j))
           enddo
       endif

! South (Bottom) edge:
       if ( js==1 ) then

           do i=isd,ied
              if ( vc(i,1)*dt > 0. ) then
                   vt(i,1) = vc(i,1) / sin_sg(i,0,4)
              else
                   vt(i,1) = vc(i,1) / sin_sg(i,1,2)
              endif
           enddo

           do i=max(3,is),min(npx-2,ie+1)
              ut(i,0) = uc(i,0) - 0.25*cosa_u(i,0)*   &
                       (vt(i-1,0)+vt(i,0)+vt(i-1,1)+vt(i,1))
              ut(i,1) = uc(i,1) - 0.25*cosa_u(i,1)*   &
                       (vt(i-1,1)+vt(i,1)+vt(i-1,2)+vt(i,2))
           enddo
       endif

! North edge:
       if ( (je+1)==npy ) then
           do i=isd,ied
              if ( vc(i,npy)*dt > 0. ) then
                   vt(i,npy) = vc(i,npy) / sin_sg(i,npy-1,4)
              else
                   vt(i,npy) = vc(i,npy) / sin_sg(i,npy,2)
              endif
           enddo
           do i=max(3,is),min(npx-2,ie+1)
              ut(i,npy-1) = uc(i,npy-1) - 0.25*cosa_u(i,npy-1)*   &
                           (vt(i-1,npy-1)+vt(i,npy-1)+vt(i-1,npy)+vt(i,npy))
              ut(i,npy) = uc(i,npy) - 0.25*cosa_u(i,npy)*   &
                         (vt(i-1,npy)+vt(i,npy)+vt(i-1,npy+1)+vt(i,npy+1))
           enddo
       endif

!The following code solves a 2x2 system to get the interior parallel-to-edge 
! uc,vc values near the corners (ex: for the sw corner ut(2,1) and vt(1,2) are solved for simultaneously).
! It then computes the halo uc, vc values so as to be consistent with the computations on the facing panel.

       !The system solved is:
       !  ut(2,1) = uc(2,1) - avg(vt)*cosa_u(2,1)
       !  vt(1,2) = vc(1,2) - avg(ut)*cosa_v(1,2)
       ! in which avg(vt) includes vt(1,2) and avg(ut) includes ut(2,1)

        if( sw_corner ) then
            damp = 1. / (1.-0.0625*cosa_u(2,0)*cosa_v(1,0))
            ut(2,0) = (uc(2,0)-0.25*cosa_u(2,0)*(vt(1,1)+vt(2,1)+vt(2,0) +vc(1,0) -   &
                      0.25*cosa_v(1,0)*(ut(1,0)+ut(1,-1)+ut(2,-1))) ) * damp
            damp = 1. / (1.-0.0625*cosa_u(0,1)*cosa_v(0,2))
            vt(0,2) = (vc(0,2)-0.25*cosa_v(0,2)*(ut(1,1)+ut(1,2)+ut(0,2)+uc(0,1) -   &
                      0.25*cosa_u(0,1)*(vt(0,1)+vt(-1,1)+vt(-1,2))) ) * damp

            damp = 1. / (1.-0.0625*cosa_u(2,1)*cosa_v(1,2))
            ut(2,1) = (uc(2,1)-0.25*cosa_u(2,1)*(vt(1,1)+vt(2,1)+vt(2,2)+vc(1,2) -   &
                      0.25*cosa_v(1,2)*(ut(1,1)+ut(1,2)+ut(2,2))) ) * damp

            vt(1,2) = (vc(1,2)-0.25*cosa_v(1,2)*(ut(1,1)+ut(1,2)+ut(2,2)+uc(2,1) -   &
                      0.25*cosa_u(2,1)*(vt(1,1)+vt(2,1)+vt(2,2))) ) * damp
        endif

        if( se_corner ) then
            damp = 1. / (1. - 0.0625*cosa_u(npx-1,0)*cosa_v(npx-1,0))
            ut(npx-1,0) = ( uc(npx-1,0)-0.25*cosa_u(npx-1,0)*(   &
                            vt(npx-1,1)+vt(npx-2,1)+vt(npx-2,0)+vc(npx-1,0) -   &
                      0.25*cosa_v(npx-1,0)*(ut(npx,0)+ut(npx,-1)+ut(npx-1,-1))) ) * damp
            damp = 1. / (1. - 0.0625*cosa_u(npx+1,1)*cosa_v(npx,2))
            vt(npx,  2) = ( vc(npx,2)-0.25*cosa_v(npx,2)*(  &
                            ut(npx,1)+ut(npx,2)+ut(npx+1,2)+uc(npx+1,1) -   &
                      0.25*cosa_u(npx+1,1)*(vt(npx,1)+vt(npx+1,1)+vt(npx+1,2))) ) * damp

            damp = 1. / (1. - 0.0625*cosa_u(npx-1,1)*cosa_v(npx-1,2))
            ut(npx-1,1) = ( uc(npx-1,1)-0.25*cosa_u(npx-1,1)*(  &
                            vt(npx-1,1)+vt(npx-2,1)+vt(npx-2,2)+vc(npx-1,2) -   &
                      0.25*cosa_v(npx-1,2)*(ut(npx,1)+ut(npx,2)+ut(npx-1,2))) ) * damp
            vt(npx-1,2) = ( vc(npx-1,2)-0.25*cosa_v(npx-1,2)*(  &
                            ut(npx,1)+ut(npx,2)+ut(npx-1,2)+uc(npx-1,1) -   &
                      0.25*cosa_u(npx-1,1)*(vt(npx-1,1)+vt(npx-2,1)+vt(npx-2,2))) ) * damp
        endif

        if( ne_corner ) then
            damp = 1. / (1. - 0.0625*cosa_u(npx-1,npy)*cosa_v(npx-1,npy+1))
            ut(npx-1,npy) = ( uc(npx-1,npy)-0.25*cosa_u(npx-1,npy)*(   &
                              vt(npx-1,npy)+vt(npx-2,npy)+vt(npx-2,npy+1)+vc(npx-1,npy+1) -   &
                0.25*cosa_v(npx-1,npy+1)*(ut(npx,npy)+ut(npx,npy+1)+ut(npx-1,npy+1))) ) * damp
            damp = 1. / (1. - 0.0625*cosa_u(npx+1,npy-1)*cosa_v(npx,npy-1))
            vt(npx,  npy-1) = ( vc(npx,npy-1)-0.25*cosa_v(npx,npy-1)*(   &
                                ut(npx,npy-1)+ut(npx,npy-2)+ut(npx+1,npy-2)+uc(npx+1,npy-1) -   &
                0.25*cosa_u(npx+1,npy-1)*(vt(npx,npy)+vt(npx+1,npy)+vt(npx+1,npy-1))) ) * damp

            damp = 1. / (1. - 0.0625*cosa_u(npx-1,npy-1)*cosa_v(npx-1,npy-1))
            ut(npx-1,npy-1) = ( uc(npx-1,npy-1)-0.25*cosa_u(npx-1,npy-1)*(  &
                                vt(npx-1,npy)+vt(npx-2,npy)+vt(npx-2,npy-1)+vc(npx-1,npy-1) -  &
                0.25*cosa_v(npx-1,npy-1)*(ut(npx,npy-1)+ut(npx,npy-2)+ut(npx-1,npy-2))) ) * damp
            vt(npx-1,npy-1) = ( vc(npx-1,npy-1)-0.25*cosa_v(npx-1,npy-1)*(  &
                                ut(npx,npy-1)+ut(npx,npy-2)+ut(npx-1,npy-2)+uc(npx-1,npy-1) -  &
                0.25*cosa_u(npx-1,npy-1)*(vt(npx-1,npy)+vt(npx-2,npy)+vt(npx-2,npy-1))) ) * damp
        endif

        if( nw_corner ) then
            damp = 1. / (1. - 0.0625*cosa_u(2,npy)*cosa_v(1,npy+1))
            ut(2,npy) = ( uc(2,npy)-0.25*cosa_u(2,npy)*(   &
                          vt(1,npy)+vt(2,npy)+vt(2,npy+1)+vc(1,npy+1) -   &
                      0.25*cosa_v(1,npy+1)*(ut(1,npy)+ut(1,npy+1)+ut(2,npy+1))) ) * damp
            damp = 1. / (1. - 0.0625*cosa_u(0,npy-1)*cosa_v(0,npy-1))
            vt(0,npy-1) = ( vc(0,npy-1)-0.25*cosa_v(0,npy-1)*(  &
                            ut(1,npy-1)+ut(1,npy-2)+ut(0,npy-2)+uc(0,npy-1) -   &
                      0.25*cosa_u(0,npy-1)*(vt(0,npy)+vt(-1,npy)+vt(-1,npy-1))) ) * damp

            damp = 1. / (1. - 0.0625*cosa_u(2,npy-1)*cosa_v(1,npy-1))
            ut(2,npy-1) = ( uc(2,npy-1)-0.25*cosa_u(2,npy-1)*(  &
                            vt(1,npy)+vt(2,npy)+vt(2,npy-1)+vc(1,npy-1) -   &
                      0.25*cosa_v(1,npy-1)*(ut(1,npy-1)+ut(1,npy-2)+ut(2,npy-2))) ) * damp

            vt(1,npy-1) = ( vc(1,npy-1)-0.25*cosa_v(1,npy-1)*(  &
                            ut(1,npy-1)+ut(1,npy-2)+ut(2,npy-2)+uc(2,npy-1) -   &
                      0.25*cosa_u(2,npy-1)*(vt(1,npy)+vt(2,npy)+vt(2,npy-1))) ) * damp
        endif
           
 else
! grid_type >= 3

        do j=jsd,jed
           do i=is,ie+1
              ut(i,j) =  uc(i,j)
           enddo
        enddo

        do j=js,je+1
           do i=isd,ied
              vt(i,j) = vc(i,j)
           enddo
        enddo
 endif

end subroutine compute_utvt

subroutine fv_getOmega(omga)
  real(REAL8), intent(OUT) :: omga(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  integer isc,iec,jsc,jec

  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec

  omga(:,:,:) = FV_Atm(1)%omga(isc:iec,jsc:jec,:)

return
end subroutine fv_getOmega

subroutine fv_getPK(pkxyz)
  real(REAL8), intent(OUT) :: pkxyz(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)
  integer isc,iec,jsc,jec
  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  pkxyz(:,:,:) = FV_Atm(1)%pk(isc:iec,jsc:jec,:)
  return
end subroutine fv_getPK

subroutine fv_getVorticity(u, v, vort)
  real(REAL8), intent(IN)  ::     u(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(IN)  ::     v(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) ::  vort(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)

  real(REAL8) ::  utemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1,1:FV_Atm(1)%npz)
  real(REAL8) ::  vtemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)

  real(REAL8) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,1:FV_Atm(1)%npz)
  real(REAL8) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,1:FV_Atm(1)%npz)

  real(REAL8) :: wbuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: sbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)
  real(REAL8) :: ebuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: nbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)

  integer isc,iec,jsc,jec
  integer npz
  integer i,j,k

  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  npz = FV_Atm(1)%npz

  utemp  = 0d0
  vtemp  = 0d0
  if (grid_type>=4) then
  ! Doubly Periodic
    uatemp(isc:iec,jsc:jec,:) = u
    vatemp(isc:iec,jsc:jec,:) = v
    call mpp_update_domains(uatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    utemp(isc:iec,jsc:jec+1,:) = uatemp(isc:iec,jsc:jec+1,:)
    vtemp(isc:iec+1,jsc:jec,:) = vatemp(isc:iec+1,jsc:jec,:)
  else
    utemp(isc:iec,jsc:jec,:) = u
    vtemp(isc:iec,jsc:jec,:) = v
    call mpp_get_boundary(utemp, vtemp, domain, &
                          wbuffery=wbuffer, ebuffery=ebuffer, &
                          sbufferx=sbuffer, nbufferx=nbuffer, &
                          gridtype=DGRID_NE, complete=.true. )
    do k=1,npz
       do i=isc,iec
          utemp(i,jec+1,k) = nbuffer(i,k)
       enddo
       do j=jsc,jec
          vtemp(iec+1,j,k) = ebuffer(j,k)
       enddo
    enddo
  endif
! Calc Vorticity
    do k=1,npz
       do j=jsc,jec
          do i=isc,iec
             vort(i,j,k) = rarea(i,j)*(utemp(i,j,k)*dx(i,j)-utemp(i,j+1,k)*dx(i,j+1) - &
                                       vtemp(i,j,k)*dy(i,j)+vtemp(i+1,j,k)*dy(i+1,j))
         enddo
       enddo
    enddo
end subroutine fv_getVorticity

subroutine fv_getDivergence(uc, vc, divg)
  use fv_grid_utils_mod, only: sin_sg,  cos_sg
  real(REAL8), intent(IN)  ::    uc(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(IN)  ::    vc(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) ::  divg(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)

  real(REAL8) :: uctemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(REAL8) :: vctemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1,1:FV_Atm(1)%npz)

  real(REAL8) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,1:FV_Atm(1)%npz)
  real(REAL8) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed,1:FV_Atm(1)%npz)

  real(REAL8) :: ut(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  )
  real(REAL8) :: vt(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1)

  real(REAL8) :: wbuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: sbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)
  real(REAL8) :: ebuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: nbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)

  integer isd,ied,jsd,jed
  integer isc,iec,jsc,jec
  integer npz
  integer i,j,k

  isd=FV_Atm(1)%isd ; ied=FV_Atm(1)%ied
  jsd=FV_Atm(1)%jsd ; jed=FV_Atm(1)%jed
  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  npz = FV_Atm(1)%npz

  uctemp  = 0d0
  vctemp  = 0d0
  if (grid_type>=4) then
  ! Doubly Periodic
    uatemp(isc:iec,jsc:jec,:) = uc
    vatemp(isc:iec,jsc:jec,:) = vc
    call mpp_update_domains(uatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    uctemp(isc:iec+1,jsc:jec,:) = uatemp(isc:iec+1,jsc:jec,:)
    vctemp(isc:iec,jsc:jec+1,:) = vatemp(isc:iec,jsc:jec+1,:)
  else
    uctemp(isc:iec,jsc:jec,:) = uc
    vctemp(isc:iec,jsc:jec,:) = vc
    call mpp_get_boundary(uctemp, vctemp, domain, &
                          wbufferx=wbuffer, ebufferx=ebuffer, &
                          sbuffery=sbuffer, nbuffery=nbuffer, &
                          gridtype=CGRID_NE, complete=.true.)
    do k=1,npz
       do j=jsc,jec
          uctemp(iec+1,j,k) = ebuffer(j,k)
       enddo
       do i=isc,iec
          vctemp(i,jec+1,k) = nbuffer(i,k)
       enddo
    enddo
  endif
  call mpp_update_domains( uctemp, vctemp, domain, gridtype=CGRID_NE, complete=.true.)
! Calc Divergence
    do k=1,npz
        call compute_utvt(uctemp(isd,jsd,k), vctemp(isd,jsd,k), ut(isd,jsd), vt(isd,jsd), 1.d0)
        do j=jsc,jec
           do i=isc,iec+1
              if ( ut(i,j) > 0. ) then
                   ut(i,j) = dy(i,j)*ut(i,j)*sin_sg(i-1,j,3)
              else
                   ut(i,j) = dy(i,j)*ut(i,j)*sin_sg(i,j,1)
             endif
           enddo
        enddo
        do j=jsc,jec+1
           do i=isc,iec
              if ( vt(i,j) > 0. ) then
                   vt(i,j) = dx(i,j)*vt(i,j)*sin_sg(i,j-1,4)
              else
                   vt(i,j) = dx(i,j)*vt(i,j)*sin_sg(i,j,2)
              endif
           enddo
        enddo
        do j=jsc,jec
           do i=isc,iec
              divg(i,j,k) = rarea(i,j)*( ut(i+1,j)-ut(i,j) + &
                                         vt(i,j+1)-vt(i,j) )
           enddo
        enddo
    enddo
end subroutine fv_getDivergence

subroutine fv_getEPV(pt, vort, epv)
  real(REAL8), intent(IN)  ::    pt(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(IN)  ::  vort(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(OUT) ::   epv(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)

  real(REAL8) :: pt_e(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz+1)

  integer isc,iec,jsc,jec
  integer npz
  integer i,j,k

  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  npz = FV_Atm(1)%npz

! Get PT at layer edges
  do j=jsc,jec
     call ppme(pt(isc:iec,j,:),pt_e(isc:iec,j,:),FV_Atm(1)%delp(isc:iec,j,:),iec-isc+1,npz)
  enddo

!     Compute absolute vorticity on the D grid
!        epv = -g * (vort+f0)*dpt/dp
      do k=1,npz
        do j=jsc,jec
          do i=isc,iec
! Standard, virtual potential temp based, definition of EPV
            epv(i,j,k) = MAPL_GRAV*(vort(i,j,k)+f0(i,j))*(pt_e(i,j,k)-pt_e(i,j,k+1))  &
                       / (FV_Atm(1)%delp(i,j,k))
          enddo
        enddo
      enddo

end subroutine fv_getEPV

!------------------------------------------------------------------------------
!BOP         
!
! !IROUTINE: fv_getAgridWinds_3D
!
! !INTERFACE:
!    
subroutine fv_getAgridWinds_3D(u, v, ua, va, uc, vc, rotate)
! 
! !INPUT PARAMETERS:
  real(REAL8), intent(IN)  ::  u(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(IN)  ::  v(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  logical, optional, intent(IN) :: rotate
! 
! !OUTPUT PARAMETERS:
  real(REAL8),           intent(OUT) :: ua(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8),           intent(OUT) :: va(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), optional, intent(OUT) :: uc(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), optional, intent(OUT) :: vc(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
!
! !DESCRIPTION:
! 
! !LOCAL VARIABLES:
  real(REAL8) :: wbuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: sbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)
  real(REAL8) :: ebuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec,FV_Atm(1)%npz)
  real(REAL8) :: nbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%npz)

  integer isc,iec,jsc,jec
  integer isd,ied,jsd,jed
  integer npz
  integer i,j,k
  
  real(FVPRC) :: ut(FV_Atm(1)%isd:FV_Atm(1)%ied, FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: vt(FV_Atm(1)%isd:FV_Atm(1)%ied, FV_Atm(1)%jsd:FV_Atm(1)%jed)

  real(REAL8) ::  utemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1,1:FV_Atm(1)%npz)
  real(REAL8) ::  vtemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(FVPRC) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(FVPRC) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(FVPRC) :: uctemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed  ,1:FV_Atm(1)%npz)
  real(FVPRC) :: vctemp(FV_Atm(1)%isd:FV_Atm(1)%ied  ,FV_Atm(1)%jsd:FV_Atm(1)%jed+1,1:FV_Atm(1)%npz)
!EOP
!------------------------------------------------------------------------------
!BOC
  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  isd=FV_Atm(1)%isd ; ied=FV_Atm(1)%ied
  jsd=FV_Atm(1)%jsd ; jed=FV_Atm(1)%jed
  npz = FV_Atm(1)%npz
  
  utemp  = 0d0
  vtemp  = 0d0
  uatemp = 0d0
  vatemp = 0d0
  uctemp = 0d0
  vctemp = 0d0

  if (grid_type>=4) then
  ! Doubly Periodic
    uatemp(isc:iec,jsc:jec,:) = u
    vatemp(isc:iec,jsc:jec,:) = v
    call mpp_update_domains(uatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    utemp(isc:iec,jsc:jec+1,:) = uatemp(isc:iec,jsc:jec+1,:)
    vtemp(isc:iec+1,jsc:jec,:) = vatemp(isc:iec+1,jsc:jec,:)
  else
    utemp(isc:iec,jsc:jec,:) = u
    vtemp(isc:iec,jsc:jec,:) = v
  ! update shared edges
    call mpp_get_boundary(utemp, vtemp, domain, &
                          wbuffery=wbuffer, ebuffery=ebuffer, &
                          sbufferx=sbuffer, nbufferx=nbuffer, &
                          gridtype=DGRID_NE, complete=.true. )
    do k=1,npz
       do i=isc,iec
          utemp(i,jec+1,k) = nbuffer(i,k)
       enddo  
       do j=jsc,jec
          vtemp(iec+1,j,k) = ebuffer(j,k)
       enddo  
    enddo   
  endif

  call mpp_update_domains(utemp, vtemp, domain, gridtype=DGRID_NE, complete=.true.)
  do k=1,npz
   call d2a2c_vect(utemp(:,:,k),  vtemp(:,:,k), &
                   uatemp(:,:,k), vatemp(:,:,k), &
                   uctemp(:,:,k), vctemp(:,:,k), ut, vt, .true.)
  enddo
  if (grid_type<4 .AND. present(rotate)) then 
   if (rotate) call cubed_to_latlon(utemp  , vtemp  , &
                                    uatemp , vatemp , &
                                    dx, dy, rdxa, rdya, npz)
  endif

  ua(:,:,:) = uatemp(isc:iec,jsc:jec,:)
  va(:,:,:) = vatemp(isc:iec,jsc:jec,:)
  if (present(uc)) uc(:,:,:) = uctemp(isc:iec,jsc:jec,:)
  if (present(vc)) vc(:,:,:) = vctemp(isc:iec,jsc:jec,:)

  return
end subroutine fv_getAgridWinds_3D
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: fv_getAgridWinds_2D
!
! !INTERFACE:
!
subroutine fv_getAgridWinds_2D(u, v, ua, va, rotate)
!
! !INPUT PARAMETERS:
  real(REAL8), intent(IN)  ::  u(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)
  real(REAL8), intent(IN)  ::  v(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)
  logical, optional, intent(IN) :: rotate
!
! !OUTPUT PARAMETERS:
  real(REAL8), intent(OUT) :: ua(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)
  real(REAL8), intent(OUT) :: va(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec)
!
! !DESCRIPTION:
!
! !LOCAL VARIABLES:
  real(REAL8) :: wbuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec)
  real(REAL8) :: sbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec)
  real(REAL8) :: ebuffer(FV_Atm(1)%jsc:FV_Atm(1)%jec)
  real(REAL8) :: nbuffer(FV_Atm(1)%isc:FV_Atm(1)%iec)

  integer isc,iec,jsc,jec
  integer i,j, npz

  real(FVPRC) :: ut(FV_Atm(1)%isd:FV_Atm(1)%ied, FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: vt(FV_Atm(1)%isd:FV_Atm(1)%ied, FV_Atm(1)%jsd:FV_Atm(1)%jed)

  real(REAL8) ::  utemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed+1)
  real(REAL8) ::  vtemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: uatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: vatemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: uctemp(FV_Atm(1)%isd:FV_Atm(1)%ied+1,FV_Atm(1)%jsd:FV_Atm(1)%jed)
  real(FVPRC) :: vctemp(FV_Atm(1)%isd:FV_Atm(1)%ied,FV_Atm(1)%jsd:FV_Atm(1)%jed+1)
!EOP
!------------------------------------------------------------------------------
!BOC
  isc=FV_Atm(1)%isc ; iec=FV_Atm(1)%iec
  jsc=FV_Atm(1)%jsc ; jec=FV_Atm(1)%jec
  npz = 1

  utemp  = 0d0
  vtemp  = 0d0
  uatemp = 0d0
  vatemp = 0d0
  uctemp = 0d0
  vctemp = 0d0

  if (grid_type>=4) then
  ! Doubly Periodic
    uatemp(isc:iec,jsc:jec) = u
    vatemp(isc:iec,jsc:jec) = v
    call mpp_update_domains(uatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
    call mpp_update_domains(vatemp, domain, &
                            whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
    utemp(isc:iec,jsc:jec+1) = uatemp(isc:iec,jsc:jec+1)
    vtemp(isc:iec+1,jsc:jec) = vatemp(isc:iec+1,jsc:jec)
  else
    utemp(isc:iec,jsc:jec) = u
    vtemp(isc:iec,jsc:jec) = v
  ! update shared edges
    call mpp_get_boundary(utemp, vtemp, domain, &
                          wbuffery=wbuffer, ebuffery=ebuffer, &
                          sbufferx=sbuffer, nbufferx=nbuffer, &
                          gridtype=DGRID_NE, complete=.true. )
    do i=isc,iec
       utemp(i,jec+1) = nbuffer(i)
    enddo
    do j=jsc,jec
       vtemp(iec+1,j) = ebuffer(j)
    enddo
  endif

  call mpp_update_domains(utemp, vtemp, domain, gridtype=DGRID_NE, complete=.true.)
  call d2a2c_vect( utemp(:,:),  vtemp(:,:), &
                  uatemp(:,:), vatemp(:,:), &
                  uctemp(:,:), vctemp(:,:), ut, vt, .true.)

  if (grid_type<4 .AND. present(rotate)) then 
   if (rotate) call cubed_to_latlon(utemp  , vtemp  , &
                                    uatemp , vatemp , &
                                    dx, dy, rdxa, rdya, 1)
  endif

  ua(:,:) = uatemp(isc:iec,jsc:jec)
  va(:,:) = vatemp(isc:iec,jsc:jec)

  return
end subroutine fv_getAgridWinds_2D
!EOC
!------------------------------------------------------------------------------

subroutine latlon_to_cubed_winds(ua, va)
  real(REAL8), intent(INOUT)  ::  ua(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)
  real(REAL8), intent(INOUT)  ::  va(FV_Atm(1)%isc:FV_Atm(1)%iec,FV_Atm(1)%jsc:FV_Atm(1)%jec,1:FV_Atm(1)%npz)

  real(REAL8) :: p1(2), p2(2), p3(2), p4(2)

  integer :: i,j,k
  integer :: IS,IE,JS,JE,KM
 
  IS = FV_Atm(1)%isc
  IE = FV_Atm(1)%iec
  JS = FV_Atm(1)%jsc
  JE = FV_Atm(1)%jec
  KM = FV_Atm(1)%npz

  do k=1,km
    do j=js,je
      do i=is,ie
         call mid_pt_sphere(FV_Atm(1)%grid(i,j,1:2), FV_Atm(1)%grid(i,j+1,1:2), p1)
         call mid_pt_sphere(FV_Atm(1)%grid(i,j,1:2), FV_Atm(1)%grid(i+1,j,1:2), p2)
         call mid_pt_sphere(FV_Atm(1)%grid(i+1,j,1:2), FV_Atm(1)%grid(i+1,j+1,1:2), p3)
         call mid_pt_sphere(FV_Atm(1)%grid(i,j+1,1:2), FV_Atm(1)%grid(i+1,j+1,1:2), p4)
         call rotate_winds(ua(i,j,k), va(i,j,k), p1,p2,p3,p4, FV_Atm(1)%agrid(i,j,1:2), 2, 1)
      enddo
    enddo
  enddo

end subroutine latlon_to_cubed_winds

  subroutine CREATE_VARS (I1, IN, J1, JN, K1, KN, KP, &
       U, V, PT, PE, PKZ, DZ, W, VARS )

    integer, intent(IN   ) :: I1, IN, J1, JN, K1, KN, KP
    real(REAL8), target ::   U(I1:IN,J1:JN,K1:KN  )
    real(REAL8), target ::   V(I1:IN,J1:JN,K1:KN  )
    real(REAL8), target ::  PT(I1:IN,J1:JN,K1:KN  )
    real(REAL8), target ::  PE(I1:IN,J1:JN,K1:KP  )
    real(REAL8), target :: PKZ(I1:IN,J1:JN,K1:KN  )
    real(REAL8), target ::  DZ(I1:IN,J1:JN,K1:KN  )
    real(REAL8), target ::   W(I1:IN,J1:JN,K1:KN  )

    type (T_FVDYCORE_VARS), intent(INOUT) :: VARS

    VARS%U => U
    VARS%V => V
    VARS%PT => PT
    VARS%PE => PE
    VARS%PKZ => PKZ
    if (.not. FV_Atm(1)%hydrostatic) VARS%DZ => DZ
    if (.not. FV_Atm(1)%hydrostatic) VARS%W => W

    return
  end subroutine CREATE_VARS

  subroutine debug_fv_state(debug_txt,STATE)
  character(LEN=*), intent(IN) :: debug_txt
  type (T_FVDYCORE_STATE),pointer :: STATE
  integer   :: isc,iec, jsc,jec, npz   !  Local dims
  real(REAL8) :: fac1    = 1.0
  real(REAL8) :: fac1em2 = 1.e-2
  real(REAL8), allocatable           :: DEBUG_ARRAY(:,:,:)
  ISC    = FV_Atm(1)%isc
  IEC    = FV_Atm(1)%iec
  JSC    = FV_Atm(1)%jsc
  JEC    = FV_Atm(1)%jec
  NPZ    = FV_Atm(1)%npz
  allocate( DEBUG_ARRAY(ISC:IEC,JSC:JEC,NPZ+1) )
  if (gid==0) print*,''
  if (gid==0) print*,'--------------', TRIM(debug_txt), '--------------'
  DEBUG_ARRAY(:,:,1) = STATE%VARS%PE(:,:,1)
  call prt_maxmin('PHIS', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0,   1, fac1, gid==0)
  DEBUG_ARRAY(:,:,1) = STATE%VARS%PE(:,:,NPZ+1)
  call prt_maxmin('PS  ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0,   1, fac1em2, gid==0)
  DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%U
  call prt_maxmin('U   ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0, npz, fac1, gid==0)
  DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%V
  call prt_maxmin('V   ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0, npz, fac1 , gid==0)
  if (.not. SW_DYNAMICS) then
  DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%PT*STATE%VARS%PKZ
  call prt_maxmin('TA  ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0, npz, fac1 , gid==0)
  DEBUG_ARRAY(:,:,1:npz) = FV_Atm(1)%q(isc:iec,jsc:jec,:,1)
  call prt_maxmin('Q1  ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0, npz, fac1 , gid==0)
  DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%PKZ
  call prt_maxmin('PZ  ', DEBUG_ARRAY  , isc, iec  , jsc, jec  , 0, npz, fac1 , gid==0)
  if ( .not. FV_Atm(1)%hydrostatic) then
      DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%W
      call prt_maxmin('W   ', DEBUG_ARRAY  , isc, iec, jsc, jec, 0, npz, fac1, gid==0)
      DEBUG_ARRAY(:,:,1:npz) = STATE%VARS%DZ
      call prt_maxmin('DZ  ', DEBUG_ARRAY  , isc, iec, jsc, jec, 0, npz, fac1, gid==0)
  endif
  endif
  if (gid==0) print*,'--------------', TRIM(debug_txt), '--------------'
  if (gid==0) print*,''
  deallocate( DEBUG_ARRAY)
  end subroutine debug_fv_state

      subroutine get_latlon_vector (pp, elon, elat)
      real(REAL8), intent(IN)  :: pp(2)
      real(REAL8), intent(OUT) :: elon(3), elat(3)

         elon(1) = -SIN(pp(1))
         elon(2) =  COS(pp(1))
         elon(3) =  0.0
         elat(1) = -SIN(pp(2))*COS(pp(1))
         elat(2) = -SIN(pp(2))*SIN(pp(1))
#ifdef RIGHT_HAND
         elat(3) =  COS(pp(2))
#else
! Left-hand system needed to be consistent with rest of the codes
         elat(3) = -COS(pp(2))
#endif

      end subroutine get_latlon_vector

!----------------------------------------------------------------------- 
!BOP
! !ROUTINE:  ppme --- PPM scheme at vertical edges
!
! !INTERFACE:
      subroutine ppme(p,qe,delp,im,km)
! !USES:
      implicit none

! !INPUT PARAMETERS:
      integer,  intent(in)     ::  im, km
      real(REAL8)         , intent(in)     ::  p(im,km)
      real(REAL8), intent(in)     ::  delp(im,km)

! !INPUT/OUTPUT PARAMETERS:
      real(REAL8), intent(out)    ::  qe(im,km+1)

! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!    05.06.13   Sawyer    Inserted file ppme.F90 here, added ProTeX
!
!EOP
!-----------------------------------------------------------------------
!BOC

      integer  km1
      integer  i, k
! local arrays.
      real(REAL8) dc(im,km),delq(im,km), a6(im,km)
      real(REAL8) c1, c2, c3, tmp, qmax, qmin
      real(REAL8) a1, a2, s1, s2, s3, s4, ss3, s32, s34, s42
      real(REAL8) a3, b2, sc, dm, d1, d2, f1, f2, f3, f4
      real(REAL8) qm, dq

      real(REAL8), parameter ::  D1EM14                  =  1.0e-14
      real(REAL8), parameter ::  D3_0                    =  3.0
      real(REAL8), parameter ::  D5_0                    =  5.0
      real(REAL8), parameter ::  D8_0                    =  8.0

      km1 = km - 1

      do 500 k=2,km
      do 500 i=1,im
500   a6(i,k) = delp(i,k-1) + delp(i,k)

      do 1000 k=1,km1
      do 1000 i=1,im
      delq(i,k) = p(i,k+1) - p(i,k)
1000  continue

      do 1220 k=2,km1
      do 1220 i=1,im
      c1 = (delp(i,k-1)+D0_5*delp(i,k))/a6(i,k+1)
      c2 = (delp(i,k+1)+D0_5*delp(i,k))/a6(i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /    &
                                    (a6(i,k)+delp(i,k+1))
      qmax = max(p(i,k-1),p(i,k),p(i,k+1)) - p(i,k)
      qmin = p(i,k) - min(p(i,k-1),p(i,k),p(i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

!****6***0*********0*********0*********0*********0*********0**********72
! 4th order interpolation of the provisional cell edge value
!****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a6(i,k)
      a1 = a6(i,k-1) / (a6(i,k) + delp(i,k-1))
      a2 = a6(i,k+1) / (a6(i,k) + delp(i,k))
      qe(i,k) = p(i,k-1) + c1 + D2_0/(a6(i,k-1)+a6(i,k+1)) *        &
                ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -         &
                                delp(i,k-1)*a1*dc(i,k  ) )
12    continue

! three-cell parabolic subgrid distribution at model top

      do 10 i=1,im
! three-cell PP-distribution
! Compute a,b, and c of q = aP**2 + bP + c using cell averages and delp
! a3 = a / 3
! b2 = b / 2
      s1 = delp(i,1)
      s2 = delp(i,2) + s1
!
      s3 = delp(i,2) + delp(i,3)
      s4 = s3 + delp(i,4)
      ss3 =  s3 + s1
      s32 = s3*s3
      s42 = s4*s4
      s34 = s3*s4
! model top
      a3 = (delq(i,2) - delq(i,1)*s3/s2) / (s3*ss3)
!
      if(abs(a3) .gt. D1EM14) then
         b2 =  delq(i,1)/s2 - a3*(s1+s2)
         sc = -b2/(D3_0*a3)
         if(sc .lt. D0_0 .or. sc .gt. s1) then
             qe(i,1) = p(i,1) - s1*(a3*s1 + b2)
         else
             qe(i,1) = p(i,1) - delq(i,1)*s1/s2
         endif
      else
! Linear
         qe(i,1) = p(i,1) - delq(i,1)*s1/s2
      endif
      dc(i,1) = p(i,1) - qe(i,1)
! compute coef. for the off-centered area preserving cubic poly.
      dm = delp(i,1) / (s34*ss3*(delp(i,2)+s3)*(s4+delp(i,1)))
      f1 = delp(i,2)*s34 / ( s2*ss3*(s4+delp(i,1)) )
      f2 = (delp(i,2)+s3) * (ss3*(delp(i,2)*s3+s34+delp(i,2)*s4)   &
            + s42*(delp(i,2)+s3+s32/s2))
      f3 = -delp(i,2)*( ss3*(s32*(s3+s4)/(s4-delp(i,2))            &
            + (delp(i,2)*s3+s34+delp(i,2)*s4))                     &
            + s42*(delp(i,2)+s3) )
      f4 = ss3*delp(i,2)*s32*(delp(i,2)+s3) / (s4-delp(i,2))
      qe(i,2) = f1*p(i,1)+(f2*p(i,2)+f3*p(i,3)+f4*p(i,4))*dm
10    continue

! Bottom
! Area preserving cubic with 2nd deriv. = 0 at the surface
      do 15 i=1,im
      d1 = delp(i,km)
      d2 = delp(i,km1)
      qm = (d2*p(i,km)+d1*p(i,km1)) / (d1+d2)
      dq = D2_0*(p(i,km1)-p(i,km)) / (d1+d2)
      c1 = (qe(i,km1)-qm-d2*dq) / (d2*(D2_0*d2*d2+d1*(d2+D3_0*d1)))
      c3 = dq - D2_0*c1*(d2*(D5_0*d1+d2)-D3_0*d1**2)
      qe(i,km  ) = qm - c1*d1*d2*(d2+D3_0*d1)
      qe(i,km+1) = d1*(D8_0*c1*d1**2-c3) + qe(i,km)
15    continue
      return
!EOC
      end subroutine ppme
!----------------------------------------------------------------------- 

end module FV_StateMod

