module fv_arrays_mod
#include <fms_platform.h>
 use mpp_domains_mod,  only: domain2d
 private

#ifdef SINGLE_FV
  integer, parameter :: REAL4 = kind(1.00)
  integer, parameter :: REAL8 = kind(1.d0)
  integer, parameter :: FVPRC = REAL4  ! Full Build Precision for the model
#else
  integer, parameter :: REAL4 = 4 !kind(1.00)
  integer, parameter :: REAL8 = 8 !kind(1.d0)
  integer, parameter :: FVPRC = REAL8  ! Full Build Precision for the model
#endif

  real(REAL8), parameter :: tiny_number=1.e-15
  real(REAL8), parameter :: huge_number=1.e+15
  
  type fv_atmos_type
     type(domain2d), pointer :: domain =>NULL()
!-----------------------------------------------------------------------
! Five prognostic state variables for the f-v dynamics
!-----------------------------------------------------------------------
! dyn_state:
! D-grid prognostatic variables: u, v, and delp (and other scalars)
!
!     o--------u(i,j+1)----------o
!     |           |              |
!     |           |              |
!  v(i,j)------scalar(i,j)----v(i+1,j)
!     |           |              |
!     |           |              |
!     o--------u(i,j)------------o
!
! The C grid component is "diagnostic" in that it is predicted every time step
! from the D grid variables.
    real(REAL8), _ALLOCATABLE :: u(:,:,:)    _NULL  ! D grid zonal wind (m/s)
    real(REAL8), _ALLOCATABLE :: v(:,:,:)    _NULL  ! D grid meridional wind (m/s)
    real(REAL8), _ALLOCATABLE :: pt(:,:,:)   _NULL  ! temperature (K)
    real(REAL8), _ALLOCATABLE :: delp(:,:,:) _NULL  ! pressure thickness (pascal)
    real(REAL8), _ALLOCATABLE :: q(:,:,:,:)  _NULL  ! specific humidity and constituents

!----------------------
! non-hydrostatic state:
!----------------------------------------------------------------------
    real(REAL8), _ALLOCATABLE ::     w(:,:,:)  _NULL  ! cell center vertical wind (m/s)
    real(REAL8), _ALLOCATABLE ::  delz(:,:,:)  _NULL  ! layer thickness (meters)
    real(REAL8), _ALLOCATABLE ::   ze0(:,:,:)  _NULL  ! height at layer edges for remapping

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real(REAL8), _ALLOCATABLE :: ps (:,:)      _NULL  ! Surface pressure (pascal)
    real(REAL8), _ALLOCATABLE :: pe (:,:,: )   _NULL  ! edge pressure (pascal)
    real(REAL8), _ALLOCATABLE :: pk  (:,:,:)   _NULL  ! pe**cappa
    real(REAL8), _ALLOCATABLE :: peln(:,:,:)   _NULL  ! ln(pe)
    real(REAL8), _ALLOCATABLE :: pkz (:,:,:)   _NULL  ! finite-volume mean pk

! For phys coupling:
    real(REAL8), _ALLOCATABLE :: u_srf(:,:)    _NULL  ! Surface u-wind
    real(REAL8), _ALLOCATABLE :: v_srf(:,:)    _NULL  ! Surface v-wind
    real(REAL8), _ALLOCATABLE :: sgh(:,:)      _NULL  ! Terrain standard deviation
    real(REAL8), _ALLOCATABLE :: oro(:,:)      _NULL  ! land fraction (1: all land; 0: all water)
    real(REAL8), _ALLOCATABLE :: ts(:,:)       _NULL  ! skin temperature (sst) from NCEP/GFS (K) -- tile
 
!-----------------------------------------------------------------------
! Others:
!-----------------------------------------------------------------------
    real(REAL8), _ALLOCATABLE :: phis(:,:)     _NULL  ! Surface geopotential (g*Z_surf)
    real(FVPRC), _ALLOCATABLE :: omga(:,:,:)   _NULL  ! Vertical pressure velocity (pa/s)
    real(FVPRC), _ALLOCATABLE :: ua(:,:,:)     _NULL  ! (ua, va) are mostly used as the A grid winds
    real(FVPRC), _ALLOCATABLE :: va(:,:,:)     _NULL
    real(FVPRC), _ALLOCATABLE :: uc(:,:,:)     _NULL  ! (uc, vc) are mostly used as the C grid winds
    real(FVPRC), _ALLOCATABLE :: vc(:,:,:)     _NULL

    real(REAL8), _ALLOCATABLE :: ak(:)  _NULL
    real(REAL8), _ALLOCATABLE :: bk(:)  _NULL

! Accumulated Mass flux arrays
    real(FVPRC), _ALLOCATABLE ::  mfx(:,:,:)  _NULL
    real(FVPRC), _ALLOCATABLE ::  mfy(:,:,:)  _NULL
! Accumulated Courant number arrays
    real(FVPRC), _ALLOCATABLE ::  cx(:,:,:)  _NULL
    real(FVPRC), _ALLOCATABLE ::  cy(:,:,:)  _NULL

! Horizontal Grid descriptors
    real(REAL8), pointer :: grid(:,:,:)  _NULL  ! Leave as a pointer for now
    real(REAL8), pointer :: agrid(:,:,:)  _NULL  ! Leave as a pointer for now
    real(REAL8), pointer :: grid_g(:,:,:) _NULL  ! "global" grid (one face of a cube)
    real(REAL8) :: shift_fac, stretch_fac, target_lat, target_lon
    logical :: do_schmidt

    real(REAL8) :: consv_te

    integer :: isc, iec, jsc, jec
    integer :: isd, ied, jsd, jed
    integer :: ks, npx, npy, npz, npz_rst, ng, ntiles
    integer :: n_sponge    ! Number of sponge layers at the top of the atmosphere
    integer :: k_top       ! Starting layer for non-hydrostatic dynamics
    integer :: ncnst, pnats, ndims, k_split, n_split, m_split, q_split, print_freq
    integer :: nwat        ! water substance
    integer :: fv_sg_adj
    integer :: na_init     ! number of iteraation for the adiabatic initialization
    integer :: n_zs_filter, nord_zs_filter

! Namelist control values
    logical :: fill
    logical :: range_warn
    logical :: z_tracer
    logical :: do_Held_Suarez
    logical :: reproduce_sum
    logical :: moist_phys
    logical :: srf_init
    logical :: mountain
    logical :: non_ortho
    logical :: adjust_dry_mass
    logical :: shallow_water, hydrostatic, phys_hydrostatic
    logical :: hybrid_z, Make_NH, make_hybrid_z
    logical :: external_ic
    logical :: ncep_ic
    logical :: fv_diag_ic
    logical :: fv_land
    logical :: nudge
    logical :: tq_filter
    logical :: warm_start

    character(len=128) :: res_latlon_dynamics  ! restart file from the latlon FV core
    character(len=128) :: res_latlon_tracers   ! tracer restart file from the latlon core

    real(REAL8)    :: dry_mass

  end type fv_atmos_type

  public:: fv_atmos_type, FVPRC, REAL4, REAL8, CNVT, tiny_number, huge_number

  INTERFACE CNVT 
    module procedure cnvt0
    module procedure cnvt1
    module procedure cnvt2
    module procedure cnvt3
  END INTERFACE

  contains

  real(FVPRC) function cnvt0(dbl_var)
     real(REAL8), intent(IN) :: dbl_var
     cnvt0 = SIGN(MIN(huge_number,MAX(tiny_number,ABS(dbl_var))),dbl_var)
  end function

  function cnvt1(dbl_var)
     real(REAL8), intent(IN) :: dbl_var(:)
     real(FVPRC)  :: cnvt1(LBOUND(dbl_var,1):UBOUND(dbl_var,1))
     integer :: i
     do i=LBOUND(dbl_var,1),UBOUND(dbl_var,1)
        cnvt1(i) = SIGN(MIN(huge_number,MAX(tiny_number,ABS(dbl_var(i)))),dbl_var(i))
     enddo
  end function

  function cnvt2(dbl_var)
     real(REAL8), intent(IN) :: dbl_var(:,:)
     real(FVPRC)  :: cnvt2(LBOUND(dbl_var,1):UBOUND(dbl_var,1),&
                           LBOUND(dbl_var,2):UBOUND(dbl_var,2))
     integer :: i, j
     do j=LBOUND(dbl_var,2),UBOUND(dbl_var,2)
        do i=LBOUND(dbl_var,1),UBOUND(dbl_var,1)
           cnvt2(i,j) = SIGN(MIN(huge_number,MAX(tiny_number,ABS(dbl_var(i,j)))),dbl_var(i,j))
        enddo
     enddo
  end function

  function cnvt3(dbl_var)
     real(REAL8), intent(IN) :: dbl_var(:,:,:)
     real(FVPRC)  :: cnvt3(LBOUND(dbl_var,1):UBOUND(dbl_var,1),&
                           LBOUND(dbl_var,2):UBOUND(dbl_var,2),&
                           LBOUND(dbl_var,3):UBOUND(dbl_var,3))
     integer :: i, j, k
     do k=LBOUND(dbl_var,3),UBOUND(dbl_var,3)
        do j=LBOUND(dbl_var,2),UBOUND(dbl_var,2)
           do i=LBOUND(dbl_var,1),UBOUND(dbl_var,1)
              cnvt3(i,j,k) = SIGN(MIN(huge_number,MAX(tiny_number,ABS(dbl_var(i,j,k)))),dbl_var(i,j,k))
           enddo
        enddo
     enddo
  end function

end module fv_arrays_mod
