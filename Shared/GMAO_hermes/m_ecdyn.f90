!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: m_ecdyn --- FVGCM dynamics state class
!
! !INTERFACE:
!
   Module m_ecdyn

!USES:

   Implicit NONE
!
! !PUBLIC TYPES:
!
   Private
   Public dyn_ecvect    ! dynamics state vector
   Public dyn_ecgrid    ! 5-D grid information
   Public dyn_ecmeta    ! metadata (variable names, etc.)
   Public get_ecphis    ! metadata (variable names, etc.)

! !PUBLIC MEMBER FUNCTIONS:
!
   Public dyn_ecinit    ! initializes a dynamics state vector
   Public dyn_ecclean     ! deallocates memory
   Public dyn_ecnull      ! nullify pointers
   Public dyn_ecput       ! writes GFIO file with single instance
   Public dyn_ecget       ! reads  GFIO file with single instance
   Public dyn_ecgetdim    ! returns lat/lon/lev/tracer dimensions
   Public dyn_ecgetcoords ! returns lat/lon/lev coordinates
   Public dyn_ecstat      ! prints out vital statistics

!
! !DESCRIPTION: This module defines data types and methods for dealing
!               with the dynamics state vector.
!
! !REVISION HISTORY:
!
!  20Jul1999  da Silva  Initial code.
!  08oct1999  da Silva  Added topography
!  27oct1999  da Silva  Added topography stdv, sst and land-sea mask.
!                       Included ks/ak/bk as part of vertical grid
!                       definition. Changed z_s to phis (cgrav*z_s) per
!                       SJ's request.
!  08nov1999  da Silva  Added interface block for Dyn_Init, added Dyn_Dup_().
!  17dec1999  da Silva  Added dyn_getdim() and dyn_getcoords().
!  02Dec2000  da Silva  Changed phis_stdv --> hs_stdv
!  14feb2000  da Silva  Extended dyn_put() to write multiple times.
!  10Mar2000  da Silva  Added option to write PRS files (but not read).
!  06Nov2000  Dee       Added option to copy fields in Dyn_Dup_()
!  17Dec2001  Dee       Implemented methods to support bias correction:
!                         Dyn_Init(incr=.true.): initialize state increment
!                         Dyn_AddIncr: add/subtract a state increment
!                         Dyn_RemapIncr: remap a state increment
!  08nov2002  Dee       Moved state increment methods to m_dynp
!  30mar2004  Colarco   Added skipSPHU
!  01Sep2004  RT/Guo    Pointers initialized to null
!  31jan2005  da Silva  To be safe, replace intent(out)n declarations of
!                       dyn_ecvect with intent(inout).
!
!EOP
!-------------------------------------------------------------------------

!BOC

   real, parameter ::  missing_val = 1.0E+15 ! hardwire this for now


!  5-D Ecgrid information
!  --------------------
   type dyn_ecgrid

!     Zonal grid
!     ----------
      integer       :: ib, ie, im
      real          :: lon_min, lon_max, lon_del

!     Meridional grid
!     ---------------
      integer       :: jb, je, jm
      real          :: lat_min, lat_max, lat_del

!     Vertical grid
!     -------------
      integer       :: kb, ke, km
      integer       :: ks            ! no. of pure pressure layers
      real, pointer :: ak(:), bk(:)  ! vertical grid coefficients
      real          :: pint          ! pressure at interface
      real          :: ptop

!     Number of tracers
!     -----------------
      integer       :: lb, le, lm

   end type dyn_ecgrid

!
!  EC Metadata
!  --------
   integer, parameter :: nch = 256
   type dyn_ecmeta
       character(len=nch) :: name
       character(len=nch) :: long_name
       character(len=nch) :: units

   end type dyn_ecmeta

!  State vector with grid information
!  ----------------------------------
   type dyn_ecvect

!     Grid information
!     ----------------
      type(dyn_ecgrid) :: grid

!     Metadata for each variable
!     --------------------------
      type(dyn_ecmeta) :: phism
      type(dyn_ecmeta) :: hs_stdvm
      type(dyn_ecmeta) :: psm
      type(dyn_ecmeta) :: tsm
      type(dyn_ecmeta) :: lwim
      type(dyn_ecmeta) :: delpm
      type(dyn_ecmeta) ::  um
      type(dyn_ecmeta) ::  vm
      type(dyn_ecmeta) ::  ptm
      type(dyn_ecmeta), pointer ::  qm(:)

!     -------------
      real            :: missing_value
                                                                                                                   
!     Fields
!     ------
      real, pointer   ::   phis(:,:)   =>null()    ! topography geopotential (g*z_s)
      real, pointer   ::   hs_stdv(:,:)=>null()    ! topography height stdv
      real, pointer   ::   Ts(:,:)     =>null()    ! sea surface temperature
      real, pointer   ::   lwi(:,:)    =>null()    ! land-water-ice mask
                                                   !   lwi = 0  over ocean
                                                   !   lwi = 1  over land
                                                   !   lwi = 2  over sea ice
                                                   ! NOTE: same as ORO in FVGCM.
                                                                                                                   
      real, pointer   ::   ps(:,:)  =>null()       ! surface pressure
      real, pointer   :: delp(:,:,:)=>null()       ! pressure thickness
      real, pointer   ::    u(:,:,:)=>null()       ! zonal wind
      real, pointer   ::    v(:,:,:)=>null()       ! meridional wind
      real, pointer   ::   pt(:,:,:)=>null()       ! scaled virtual potential temperature
      real, pointer   ::    q(:,:,:,:)=>null()     ! specific humidity & tracers
                                                                                                                   
   end type dyn_ecvect


!  Interfaces
!  ----------

   interface Dyn_EcInit
      module procedure Dyn_EcInit_    ! Standard f90 interface
   end interface


!EOC

!---------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_EcInit_ --- Initializes a dynamics state vector
!
! !INTERFACE:
!
  subroutine  Dyn_EcInit_ ( im, jm, km, lm, w_f, rc,  &
                          ptop, ks, ak, bk )                ! optional
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
 integer, intent(in)            :: im   ! zonal dimension
 integer, intent(in)            :: jm   ! meridional dimension
 integer, intent(in)            :: km   ! vertical dimension
 integer, intent(in)            :: lm   ! "tracer" dimension

                                        ! OPTIONAL vertical grid specification:
 real,    intent(in), OPTIONAL  :: ptop     ! top pressure (Pa)
 integer, intent(in), OPTIONAL  :: ks       ! no. of pure pressure levels
 real,    intent(in), OPTIONAL  :: ak(km+1) ! vertical grid a coefficient
 real,    intent(in), OPTIONAL  :: bk(km+1) ! vertical grid b coefficient



!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent (inout) :: w_f   ! dynamics state vector
  integer, intent(out)         :: rc    ! error return code:
                                        !  0 - all is well
                                        !  1 - already allocated
                                        !  2 - not enough memory
                                        !  3 - invalid dimensions
!
! !DESCRIPTION: Initializes the dynamics state vector. It is assumed
!               that
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  26oct1999 da Silva  Added phis_stdv, ts, lwi, a, b
!  01nov1999 da Silva  Moved sanity check to top.
!  19dec2000 da Silva  Fixed initialization of fields with zeros;
!                      hs_stdv, Ts and lwi were missing.
!
!EOP
!-------------------------------------------------------------------------

     integer    err, l
     character*3 bfr


!    Sanity check
!    ------------
     rc = 0
     if ( im<1 .or. jm<1 .or. km<1 .or. lm<1 ) then
          rc = 3
          return
     endif

!    Check whether we have a clean slate
!    -----------------------------------
     if ( associated(w_f%u)     .or. &
          associated(w_f%v)     .or. &
          associated(w_f%pt)    .or. &
          associated(w_f%q)     .or. &
          associated(w_f%delp)  .or. &
          associated(w_f%ps)    .or. &
          associated(w_f%ts)    .or. &
          associated(w_f%lwi)   .or. &
          associated(w_f%phis)  .or. &
          associated(w_f%hs_stdv)   )     then

          rc = 1
          return

     endif

!    OK, allocate the necessary memory
!    ---------------------------------
     allocate(w_f%phis(im,jm),      stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%hs_stdv(im,jm), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ts(im,jm),      stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%lwi(im,jm),     stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ps(im,jm),      stat = err); if (err.ne. 0) rc = 2

     allocate(w_f%delp(im,jm,km), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%u(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%v(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%pt(im,jm,km),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%q(im,jm,km,lm), stat = err); if (err.ne. 0) rc = 2

     allocate(w_f%grid%ak(km+1),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%grid%bk(km+1),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%qm(lm),         stat = err); if (err.ne. 0) rc = 2

     if (rc .ne. 0) return     ! problems allocating memory


!    Initialize dimensional attributes
!    ---------------------------------
     w_f%grid%ib = 1;   w_f%grid%ie = im;   w_f%grid%im = im
     w_f%grid%jb = 1;   w_f%grid%je = jm;   w_f%grid%jm = jm
     w_f%grid%kb = 1;   w_f%grid%ke = km;   w_f%grid%km = km
     w_f%grid%lb = 1;   w_f%grid%le = lm;   w_f%grid%lm = lm

!    Horizontal grid (hardwire A-grid for now)
!    -----------------------------------------
     w_f%grid%lon_min = 0.0
     w_f%grid%lon_del = 360.0 / im
     w_f%grid%lon_max = w_f%grid%lon_min + (im-1) * w_f%grid%lon_del
     w_f%grid%lat_min = -90.0
     w_f%grid%lat_max = +90.0
     w_f%grid%lat_del = ( w_f%grid%lat_max - w_f%grid%lat_min ) / ( jm-1)


!    If user specified, set vertical grid attributes
!    -----------------------------------------------
     if ( present(ptop) ) then
          w_f%grid%ptop = ptop
     else
          w_f%grid%ptop = missing_val
     end if
     if ( present(ks) .and. present(ak) .and. present(bk) ) then
         w_f%grid%ks = ks
         w_f%grid%pint = ak(ks+1)
         w_f%grid%ak = ak
         w_f%grid%bk = bk
      else
         w_f%grid%ks = -1
         w_f%grid%pint = missing_val
         w_f%grid%ak   = missing_val
         w_f%grid%bk   = missing_val
      end if


!     --------------------------------------
!    Initialize metadata
!    -------------------
! 1/28/2008 Ravi      Updated to read Nature run.
!     --------------------------------------
!
     if ( km == 61 ) then
       call Dyn_EcSetMeta_ ( w_f )
     endif
!
     if ( km == 91 ) then
       call Dyn_NtSetMeta_ ( w_f )
     endif

!    Initialize fields with zeros
!    ----------------------------
     w_f%phis    = 0.0
     w_f%hs_stdv = 0.0
     w_f%Ts      = 0.0
     w_f%lwi     = 0.0

     w_f%ps   = 0.0
     w_f%delp = 0.0
     w_f%u    = 0.0
     w_f%v    = 0.0
     w_f%pt   = 0.0
     w_f%q    = 0.0

!    All done.
!    --------
     rc = 0

  end subroutine Dyn_EcInit_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_NtSetMeta_ --- Sets the metadata of a dynamics vector
!
! !INTERFACE:
!
  subroutine Dyn_NtSetMeta_ ( w_f )
!
! !USES:
!
  implicit NONE
!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent(inout) :: w_f    ! dynamics state vector

!
! !DESCRIPTION: This routine sets variable names and other metadata
!               attributes for a dynamics vector.
!
! !REVISION HISTORY:
!
!  18dec1999 da Silva  Initial code.
!  03Feb2003 Todling   w_f needed be declared "inout"
!  14Jul2003 Todling   Added Ozone.
!  29Sep2003 Todling   Fixed description on Ozone by I. Stajner's request
!  27Jul2009 Ravi      Fixed the description of tracer-1 and tracer-2 with ciwc and clwc
!
!EOP
!-------------------------------------------------------------------------

     integer    l, lm
     character*3 bfr

     lm = w_f%grid%lm

!    Set metadata
!    ------------
     w_f%missing_value = missing_val

     w_f%phism%name = 'zhlev1';   w_f%phism%long_name = 'Topography geopotential'
     w_f%phism%units = 'meter2/sec2';

     w_f%hs_stdvm%name = 'hs_stdv'
     w_f%hs_stdvm%long_name = 'Topography Height Standard Deviation'
     w_f%hs_stdvm%units = 'meter';

     w_f%tsm%name = 'ts';   w_f%tsm%long_name = 'Surface temperature'
     w_f%tsm%units = 'K';

     w_f%lwim%name = 'lwi';   w_f%lwim%long_name = 'Land-water-ice mask'
     w_f%lwim%units = 'unknown';

     w_f%psm%name = 'lnsphlev1';   w_f%psm%long_name = 'Surface Pressure'
     w_f%psm%units = 'Pa';

     w_f%delpm%name = 'delp';   w_f%delpm%long_name = 'Pressure Thickness'
     w_f%delpm%units = 'Pa';

     w_f%um%name = 'u';  w_f%um%long_name = 'Zonal Wind'
     w_f%um%units = 'm/s'

     w_f%vm%name = 'v'; w_f%vm%long_name = 'Meridional Wind'
     w_f%vm%units = 'm/s'

     w_f%ptm%name = 't'; w_f%ptm%long_name = ' (profile) Temperature'
     w_f%ptm%units = 'K'  ! for now

     w_f%qm(1)%name = 'q';  w_f%qm(1)%long_name = 'Specific Humidity'
     w_f%qm(1)%units = 'kg/kg**-1'

     if (lm >= 2) then   ! This assumes 2nd tracer to be ozone
         w_f%qm(2)%name = 'o3';  w_f%qm(2)%long_name = 'Ozone (passive in PSAS-based DAS)'
         w_f%qm(2)%units = 'ppmv'
     endif

!    Initialize tracers with a generic name for now
!    TO DO: Make these optional parameters
!    ----------------------------------------------
!    do l = 3, lm
!       write(bfr,'(i3.3)') l-2
!       w_f%qm(l)%name = 'tracer'//bfr;  w_f%qm(l)%long_name = 'Tracer '//bfr
!       w_f%qm(l)%units = 'unknown'
!    end do

      w_f%qm(3)%name = 'ciwc'
      w_f%qm(3)%long_name = 'Cloud ice water content'
      w_f%qm(3)%units = 'kg kg**-1'

      w_f%qm(4)%name = 'clwc'
      w_f%qm(4)%long_name = 'Cloud liquid water content'
      w_f%qm(4)%units = 'kg kg**-1'
                                                                                                                   
   end subroutine Dyn_NtSetMeta_

!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_EcSetMeta_ --- Sets the metadata of a dynamics vector
!
! !INTERFACE:
!
  subroutine Dyn_EcSetMeta_ ( w_f )
!
! !USES:
!
  implicit NONE
!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent(inout) :: w_f    ! dynamics state vector

!
! !DESCRIPTION: This routine sets variable names and other metadata
!               attributes for a dynamics vector.
!
! !REVISION HISTORY:
!
!  18dec1999 da Silva  Initial code.
!  03Feb2003 Todling   w_f needed be declared "inout"
!  14Jul2003 Todling   Added Ozone.
!  29Sep2003 Todling   Fixed description on Ozone by I. Stajner's request
!  27Jul2009 Ravi      Fixed the description of tracer-1 and tracer-2 with ciwc and clwc
!
!EOP
!-------------------------------------------------------------------------

     integer    l, lm
     character*3 bfr

     lm = w_f%grid%lm

!    Set metadata
!    ------------
     w_f%missing_value = missing_val

     w_f%phism%name = 'phis';   w_f%phism%long_name = 'Topography geopotential'
     w_f%phism%units = 'meter2/sec2';

     w_f%hs_stdvm%name = 'hs_stdv'
     w_f%hs_stdvm%long_name = 'Topography Height Standard Deviation'
     w_f%hs_stdvm%units = 'meter';

     w_f%tsm%name = 'ts';   w_f%tsm%long_name = 'Surface temperature'
     w_f%tsm%units = 'K';

     w_f%lwim%name = 'lwi';   w_f%lwim%long_name = 'Land-water-ice mask'
     w_f%lwim%units = 'unknown';

     w_f%psm%name = 'lnsphbl';   w_f%psm%long_name = 'Surface Pressure'
     w_f%psm%units = 'Pa';

     w_f%delpm%name = 'delp';   w_f%delpm%long_name = 'Pressure Thickness'
     w_f%delpm%units = 'Pa';

     w_f%um%name = 'uhbl';  w_f%um%long_name = 'Zonal Wind'
     w_f%um%units = 'm/s'

     w_f%vm%name = 'vhbl'; w_f%vm%long_name = 'Meridional Wind'
     w_f%vm%units = 'm/s'

     w_f%ptm%name = 'thbl'; w_f%ptm%long_name = 'Scaled Potential Temperature'
     w_f%ptm%units = 'unknown'  ! for now

     w_f%qm(1)%name = 'qhbl';  w_f%qm(1)%long_name = 'Specific Humidity'
     w_f%qm(1)%units = 'kg/kg'

     if (lm >= 2) then   ! This assumes 2nd tracer to be ozone
         w_f%qm(2)%name = 'o3hbl';  w_f%qm(2)%long_name = 'Ozone (passive in PSAS-based DAS)'
         w_f%qm(2)%units = 'ppmv'
     endif

!    Initialize tracers with a generic name for now
!    TO DO: Make these optional parameters
!    ----------------------------------------------
!    do l = 3, lm
!       write(bfr,'(i3.3)') l-2
!       w_f%qm(l)%name = 'tracer'//bfr;  w_f%qm(l)%long_name = 'Tracer '//bfr
!       w_f%qm(l)%units = 'unknown'
!    end do

      w_f%qm(3)%name = 'ciwc'
      w_f%qm(3)%long_name = 'Cloud ice water content'
      w_f%qm(3)%units = 'kg kg**-1'

      w_f%qm(4)%name = 'clwc'
      w_f%qm(4)%long_name = 'Cloud liquid water content'
      w_f%qm(4)%units = 'kg kg**-1'
      
                                                                                                                   
   end subroutine Dyn_EcSetMeta_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Dyn_EcDup_ --- Duplicates a dynamics vector
!
! !INTERFACE:
!
  subroutine  Dyn_EcDup_ ( w_in, w_out, rc,   &
                                      copy )     ! optional
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  type(dyn_ecvect), intent(in)    :: w_in   ! existing dynamics vector
  logical, intent(in), OPTIONAL :: copy   ! copy fields if .true.
!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent (inout)  :: w_out  ! new dynamics state vector
  integer, intent(out)          :: rc     ! error return code:
                                          !  0 - all is well
                                          !  1 - already allocated
                                          !  2 - not enough memory
                                          !  3 - invalid dimensions
!
! !DESCRIPTION: This routine duplicates an existing dynamics vector.
!               It allocates memory for the new vector {\tt w\_out},
!  setting its grid and metadata as in {\tt w\_in}. If the optional
!  parameter copy=.true., then all gridded fields are copied.
!
! !REVISION HISTORY:
!
!  08nov1999 da Silva  Initial code.
!  06nov2000 Dee       Added option to copy fields
!  19dec2000 da Silva  Fixed bug in copy: now the structure is copied
!                      (before Ts was not copied).
!  17dec2001 Dee       Added option to initialize a state increment
!  07nov2002 Dee       Moved increment methods to m_dynp
!
!EOP
!-------------------------------------------------------------------------

! Allocate memory and set grid
! ----------------------------
  call Dyn_EcInit_ ( w_in%grid%im, w_in%grid%jm, w_in%grid%km, w_in%grid%lm,  &
                   w_out, rc,                                               &
                   w_in%grid%ptop, w_in%grid%ks, w_in%grid%ak, w_in%grid%bk )

! Optionally copy fields
! ----------------------
  if ( present(copy) ) then

     if (copy) then

        w_out%phis    = w_in%phis
        w_out%hs_stdv = w_in%hs_stdv
        w_out%Ts      = w_in%Ts
        w_out%lwi     = w_in%lwi

        w_out%ps   = w_in%ps
        w_out%delp = w_in%delp
        w_out%u    = w_in%u
        w_out%v    = w_in%v
        w_out%pt   = w_in%pt
        w_out%q    = w_in%q

     end if

  end if

  end subroutine Dyn_EcDup_


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_EcInit77_ --- Initializes a dynamics vector (f77 interface)
!
! !INTERFACE:
!
  subroutine  Dyn_EcInit77_ ( im, jm, km, lm, ptop, ks, ak, bk, &
                            phis, hs_stdv, Ts, lwi, ps,     &
                            delp, u, v, pt, q, w_f, rc        )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
                                                ! Dimensions:
  integer,      intent(in)   :: im              !   zonal dimension
  integer,      intent(in)   :: jm              !   meridional dimension
  integer,      intent(in)   :: km              !   vertical dimension
  integer,      intent(in)   :: lm              !   "tracer" dimension

                                                ! Fixed Eulerian vertical
                                                !  coordinates:
  real,         intent(in)   :: ptop            !   top pressure (Pa)
  integer,      intent(in)   :: ks              !   no. of pure pressure levels
  real, TARGET, intent(in)   :: ak(km+1)        !   vertical grid a coefficient
  real, TARGET, intent(in)   :: bk(km+1)        !   vertical grid b coefficient

                                                ! 2D fields:
  real, TARGET, intent(in)   :: phis(:,:)       !  topography geopotential [m]
                                                !    (g*z_s)
  real, TARGET, intent(in)   :: hs_stdv(:,:)    !  topography height stdv
  real, TARGET, intent(in)   :: Ts(:,:)         !  sea surface temperature
  real, TARGET, intent(in)   :: lwi(:,:)        !  land-water-ice mask
  real, TARGET, intent(in)   :: ps(:,:)         !  surface pressure

                                                ! 3D+ Fields:
  real, TARGET, intent(in)   :: delp(:,:,:)     !  pressure thickness [Pa]
  real, TARGET, intent(in)   ::    u(:,:,:)     !  zonal wind [m/s]
  real, TARGET, intent(in)   ::    v(:,:,:)     !  meridional wind [m/s]
  real, TARGET, intent(in)   ::   pt(:,:,:)     !  scaled virtual potential
                                                !  temperature [Pa**kappa K]
  real, TARGET, intent(in)   ::    q(:,:,:,:)   !  specific humidity & tracers
                                                !   [kg/kg]

!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent (inout) :: w_f   ! dynamics state vector
  integer, intent(out)         :: rc    ! error return code:
                                        !  0 - all is well
                                        !  1 - already allocated
                                        !  2 - could not allocate memory
                                        !  3 - invalid dimensions
!
! !DESCRIPTION: Initializes the dynamics state vector. It is assumed
!               that
!
! !REVISION HISTORY:
!
!  18dec1999 da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

     integer err

!    Sanity check
!    ------------
     rc = 0
     if ( im<1 .or. jm<1 .or. km<1 .or. lm<1 ) then
          rc = 3
          return
     endif

!    Check whether we have a clean slate
!    -----------------------------------
     if ( associated(w_f%u)     .or. &
          associated(w_f%v)     .or. &
          associated(w_f%pt)    .or. &
          associated(w_f%q)     .or. &
          associated(w_f%delp)  .or. &
          associated(w_f%ps)    .or. &
          associated(w_f%ts)    .or. &
          associated(w_f%lwi)   .or. &
          associated(w_f%phis)  .or. &
          associated(w_f%hs_stdv)   )     then

          rc = 1
          return

     endif

     allocate(w_f%qm(lm),         stat = err); if (err.ne. 0) rc = 2
     if ( rc .ne. 0 ) return


!    Initialize dimensional attributes
!    ---------------------------------
     w_f%grid%ib = 1;   w_f%grid%ie = im;   w_f%grid%im = im
     w_f%grid%jb = 1;   w_f%grid%je = jm;   w_f%grid%jm = jm
     w_f%grid%kb = 1;   w_f%grid%ke = km;   w_f%grid%km = km
     w_f%grid%lb = 1;   w_f%grid%le = lm;   w_f%grid%lm = lm

!    Horizontal grid (hardwire A-grid for now)
!    -----------------------------------------
     w_f%grid%lon_min = 0.0
     w_f%grid%lon_del = 360.0 / im
     w_f%grid%lon_max = w_f%grid%lon_min + (im-1) * w_f%grid%lon_del
     w_f%grid%lat_min = -90.0
     w_f%grid%lat_max = +90.0
     w_f%grid%lat_del = ( w_f%grid%lat_max - w_f%grid%lat_min ) / ( jm-1)


!    Vertical grid
!    -------------
     w_f%grid%ptop =  ptop
     w_f%grid%ks   =  ks
     w_f%grid%pint =  ak(ks+1)
     w_f%grid%ak   => ak
     w_f%grid%bk   => bk


!    Assign pointers for 2D/3D fields
!    --------------------------------
     w_f%phis      => phis
     w_f%hs_stdv   => hs_stdv
     w_f%Ts        => Ts
     w_f%lwi       => lwi
     w_f%ps        => ps
     w_f%delp      => delp
     w_f%u         => u
     w_f%v         => v
     w_f%pt        => pt
     w_f%q         => q

!    Initialize metadata
!    -------------------
     call Dyn_EcSetMeta_ ( w_f )

   end subroutine Dyn_EcInit77_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecclean --- Deallocates memory used by dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_ecclean ( w_f )
!
! !USES:
!
  implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent (inout) :: w_f   ! dynamics state vector

! !DESCRIPTION:
!
!  Deallocates memory used by dynamics state vector.
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  26oct1999 da Silva  Added hs_stdv, ts, lwi, a, b
!
!EOP
!-------------------------------------------------------------------------

   if ( associated(w_f%phis) )      deallocate(w_f%phis)
   if ( associated(w_f%hs_stdv) )   deallocate(w_f%hs_stdv)
   if ( associated(w_f%ts) )        deallocate(w_f%ts)
   if ( associated(w_f%lwi) )       deallocate(w_f%lwi)

   if ( associated(w_f%delp) )      deallocate(w_f%delp)
   if ( associated(w_f%ps) )        deallocate(w_f%ps)
   if ( associated(w_f%u)  )        deallocate(w_f%u)
   if ( associated(w_f%v)  )        deallocate(w_f%v)
   if ( associated(w_f%pt) )        deallocate(w_f%pt)
   if ( associated(w_f%q)  )        deallocate(w_f%q)

   if ( associated(w_f%qm) )        deallocate(w_f%qm)
   if ( associated(w_f%grid%ak) )   deallocate(w_f%grid%ak)
   if ( associated(w_f%grid%bk) )   deallocate(w_f%grid%bk)

  end subroutine dyn_ecclean

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecnull --- Nullify pointers used by a dynamics state vectors
!
! !INTERFACE:
!
  subroutine  dyn_ecnull ( w_f )
!
! !USES:
!
  implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
  type(dyn_ecvect), intent (inout) :: w_f   ! dynamics state vector

! !DESCRIPTION:
!
!  Nullify all pointers in w_f.
!
! !REVISION HISTORY:
!
!  23dec1999 da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

   if ( associated(w_f%phis) )        nullify(w_f%phis)
   if ( associated(w_f%hs_stdv) )   nullify(w_f%hs_stdv)
   if ( associated(w_f%ts) )          nullify(w_f%ts)
   if ( associated(w_f%lwi) )         nullify(w_f%lwi)

   if ( associated(w_f%delp) )        nullify(w_f%delp)
   if ( associated(w_f%ps) )          nullify(w_f%ps)
   if ( associated(w_f%u)  )          nullify(w_f%u)
   if ( associated(w_f%v)  )          nullify(w_f%v)
   if ( associated(w_f%pt) )          nullify(w_f%pt)
   if ( associated(w_f%q)  )          nullify(w_f%q)

   if ( associated(w_f%qm) )          nullify(w_f%qm)
   if ( associated(w_f%grid%ak) )     nullify(w_f%grid%ak)
   if ( associated(w_f%grid%bk) )     nullify(w_f%grid%bk)

  end subroutine dyn_ecnull


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecput --- writes out single instance of dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_ecput ( fname, nymd, nhms, prec, w_f, rc, &
                        nstep, verbose, new, freq, epv, plevs )   ! optional
!
! !USES:
!
  implicit NONE

!
! !INPUT PARAMETERS:
!
  character(len=*),    intent(in)   :: fname  ! output file name
  integer,             intent(in)   :: nymd   ! Date: year-month-day
  integer,             intent(in)   :: nhms   ! Time: hour-min-sec
  integer,             intent(in)   :: prec   ! precision:
                                              ! 0 = 32 bits
                                              ! 1 = 64 bits
  type(dyn_ecvect), intent(in)        :: w_f    ! dynamics state vector

  integer, intent(in), OPTIONAL     :: nstep   ! FVGCM step (for restarts)
  logical, intent(in), OPTIONAL     :: verbose ! if true, send log to stdout
  logical, intent(in), OPTIONAL     :: new     ! create new file even if it
                                               ! already exists.
  integer, intent(in), OPTIONAL     :: freq    ! time frequency (HHMMSS) for
                                               ! multiple instance files
                                               ! (default: 060000)
  real, intent(in), OPTIONAL        :: epv(:,:,:) ! Ertel potential vorticity
  real, intent(in), OPTIONAL        :: plevs(:) ! pressure levels (hPa) for
                                                ! PRS files. If this paramter
                                                ! is specified the output
                                                ! file is assumed to be PRS.
!
! !OUTPUT PARAMETERS:
!

  integer, intent(out)              :: rc    ! error return code:
                                             !  0 - all is well
                                             !  >0 - errors
!
! !DESCRIPTION: Writes a GFIO file with one or more instances of the
!               dynamics state vector {\tt w\_f} valid at a given time
!  to a file named {\tt fname}. The file is created or opened, written to and
!  closed upon completion. Unlike earlier versions, it is now possible
!  to write more than one time to a single file.
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  26oct1999 da Silva  Added phis_stdv, ts, lwi, a, b
!  06dec1999 da Silva  Added nstep.
!  02Feb2000 da Silva  Changed vertical coord variable from sigma to pres.
!  14feb2000 da Silva  Added option of wrtiting to an already existing
!                      file.
!  10mar2000 da Silva  Added optional parameter plevs for PRS file.
!
!EOP
!-------------------------------------------------------------------------

   character(len=nch)              :: title, source, contact, levunits
   character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)

   real,    allocatable :: lat(:), lon(:), lev(:)
   real,    allocatable :: valid_range(:,:), packing_range(:,:)
   integer, allocatable :: kmvar(:)

   integer :: ks, im, jm, km, lm, nvars
   real    :: p, ptop, pint
   integer :: i, j, k, l, timeinc
   integer :: fid, err

   logical verb, creating, fexists

   integer, parameter :: READ_WRITE = 0

!  Reference pressure thickness assuming ps ~ 984 hPa
!  ---------------------------------------------------
   real dpref
   dpref(k) = ( w_f%grid%ak(k+1) - w_f%grid%ak(k) ) + &
               ( w_f%grid%bk(k+1) - w_f%grid%bk(k) ) * 98400.


   if ( present(verbose) ) then
        verb = verbose
   else
        verb = .false.
   end if

   if ( present(new) ) then
      creating = new
   else
      creating = .false.
   end if

! Check whether file exists
! -------------------------
  inquire ( file=trim(fname), exist=fexists )
  if ( .not. fexists ) creating = .true.       ! must create then

!  Short hand for dimensions
!  -------------------------
   im = w_f%grid%im; jm = w_f%grid%jm
   km = w_f%grid%km; lm = w_f%grid%lm
   nvars = 9 + lm                      ! phis, hs_stdv, ts, lwi,
                                       ! delp, ps, u, v, pt, q + tracers

   if ( present(epv) ) nvars = nvars+1 ! additional EPV output


!  Allocate local work space
!  -------------------------
   rc = 0
   call ecinit_ ( err )
   if ( err .ne. 0 ) then
        call ecclean_()
        rc = 1
        return
   end if


!  Create coordinate variables
!  ---------------------------
   do j = 1, jm
      lat(j) = w_f%grid%lat_min + (j-1) * w_f%grid%lat_del
   end do
   do i = 1, im
      lon(i) = w_f%grid%lon_min + (i-1) * w_f%grid%lon_del
   end do

!  Vertical coordinates: given on input (prs files)
!  ------------------------------------------------
   if ( present(plevs) ) then

        ptop = w_f%grid%ptop
        lev(1:km) = plevs(1:km)

!  Vertical coordinates: fake something for GrADS sake
!  ---------------------------------------------------
   else

      ptop = w_f%grid%ptop
      p = ptop + 0.5 * dpref(1)
      lev(1) = p
      do k = 2, km
         p = p + 0.5 * ( dpref(k-1) + dpref(k) )
         lev(k) = p
      end do
      lev(1:km) = lev(1:km) / 100.

   end if
   levunits = 'hPa'

!  Global metadata
!  ---------------
   if ( present(plevs) ) then
      title = 'FVGCM Dynamics State Vector (Pressure Coordinates)'
   else
      title = 'FVGCM Dynamics State Vector (Hybrid Coordinates)'
   end if
   source = 'Data Assimilation Office, NASA/GSFC'
   contact = 'data@dao.gsfc.nasa.gov'

!  Setup variable information
!  --------------------------
   vname(1)  = w_f%phism%name
   vtitle(1) = w_f%phism%long_name
   vunits(1) = w_f%phism%units
   kmvar(1)  = 0

   vname(2)  = w_f%hs_stdvm%name
   vtitle(2) = w_f%hs_stdvm%long_name
   vunits(2) = w_f%hs_stdvm%units
   kmvar(2)  = 0

   vname(3)  = w_f%tsm%name
   vtitle(3) = w_f%tsm%long_name
   vunits(3) = w_f%tsm%units
   kmvar(3)  = 0

   vname(4)  = w_f%lwim%name
   vtitle(4) = w_f%lwim%long_name
   vunits(4) = w_f%lwim%units
   kmvar(4)  = 0

   if ( present(plevs) ) then
      vname(5)  = 'slp'
      vtitle(5) = 'Sea level pressure'
      vunits(5) = 'hPa'
      kmvar(5)  = 0
   else
      vname(5)  = w_f%psm%name
      vtitle(5) = w_f%psm%long_name
      vunits(5) = w_f%psm%units
      kmvar(5)  = 0
   end if

   if ( present(plevs) ) then
      vname(6)  = 'hght'
      vtitle(6) = 'Geopotential height'
      vunits(6) = 'm'
      kmvar(6)  = km
   else
      vname(6)  = w_f%delpm%name
      vtitle(6) = w_f%delpm%long_name
      vunits(6) = w_f%delpm%units
      kmvar(6)  = km
   end if

   vname(7)  = w_f%um%name
   vtitle(7) = w_f%um%long_name
   vunits(7) = w_f%um%units
   kmvar(7)  = km

   vname(8)  = w_f%vm%name
   vtitle(8) = w_f%vm%long_name
   vunits(8) = w_f%vm%units
   kmvar(8)  = km

   if ( present(plevs) ) then
      vname(9)  = 'tmpu'
      vtitle(9) = 'Temperature'
      vunits(9) = 'Kelvin'
      kmvar(9)  = km
   else
      vname(9)  = w_f%ptm%name
      vtitle(9) = w_f%ptm%long_name
      vunits(9) = w_f%ptm%units
      kmvar(9)  = km
   end if

   do l = 10, 10+lm-1
      i = l - 9
      vname(l)  = w_f%qm(i)%name
      vtitle(l) = w_f%qm(i)%long_name
      vunits(l) = w_f%qm(i)%units
      kmvar(l)  = km
   end do

!  Handle EPV as last variable
!  ---------------------------
   if ( present(epv) ) then
      l = nvars
      vname(l)  = 'epv'
      vtitle(l) = 'Ertel Potential Vorticity'
      vunits(l) = 'unknown'   ! check with SJ
      kmvar(l)  = km
   end if

!  For now, do not exercise packing/valid range feature
!  -----------------------------------------------------
   do j = 1, nvars
      do i = 1, 2
         valid_range(i,j) = missing_val
         packing_range(i,j) = missing_val
      end do
   end do

!  Time attributes
!  ---------------
   if ( present(freq) ) then
      timeinc = freq
   else
      timeinc = 060000
   end if

!  Create new GFIO file ...
!  ------------------------
   if ( creating ) then

    if (verb) print *, '        [] creating GFIO file ', trim(fname)
    call GFIO_Create ( fname, title, source, contact, missing_val,  &
                       im, jm, km, lon, lat, lev, levunits,         &
                       nymd, nhms, timeinc,                         &
                       nvars, vname, vtitle, vunits, kmvar,         &
                       valid_range, packing_range, prec,            &
                       fid, err )

!  ... or open existing GFIO file ...
!  ----------------------------------
   else

    if (verb) print *, '        [] opening GFIO file ', trim(fname)

    call GFIO_Open ( fname, READ_WRITE, fid, err )

   end if

    if ( err .ne. 0 ) then
         rc = 2
         call ecclean_()
         return
    end if


!   Write the data to GFIO file
!   Note: we may want to write out 1 level at a time in case
!         the compiler does aq copy when invoking putvar().
!   --------------------------------------------------------
    if (verb) print *, '        [] writing ', trim(vname(1))
    call GFIO_PutVar ( fid, vname(1), nymd, nhms, im, jm, 0,  1, w_f%phis,   err )
    if ( err .ne. 0 ) rc = 101

    if (verb) print *, '        [] writing ', trim(vname(2))
    call GFIO_PutVar ( fid, vname(2), nymd, nhms, im, jm, 0,  1, w_f%hs_stdv,   err )
    if ( err .ne. 0 ) rc = 102

    if (verb) print *, '        [] writing ', trim(vname(3))
    call GFIO_PutVar ( fid, vname(3), nymd, nhms, im, jm, 0,  1, w_f%ts,   err )
    if ( err .ne. 0 ) rc = 103

    if (verb) print *, '        [] writing ', trim(vname(4))
    call GFIO_PutVar ( fid, vname(4), nymd, nhms, im, jm, 0,  1, w_f%lwi,   err )
    if ( err .ne. 0 ) rc = 104

    if (verb) print *, '        [] writing ', trim(vname(5))
    call GFIO_PutVar ( fid, vname(5), nymd, nhms, im, jm, 0,  1, w_f%ps,   err )
    if ( err .ne. 0 ) rc = 105

    if (verb) print *, '        [] writing ', trim(vname(6))
    call GFIO_PutVar ( fid, vname(6), nymd, nhms, im, jm, 1, km, w_f%delp, err )
    if ( err .ne. 0 ) rc = 106

    if (verb) print *, '        [] writing ', trim(vname(7))
    call GFIO_PutVar ( fid, vname(7), nymd, nhms, im, jm, 1, km, w_f%u,    err )
    if ( err .ne. 0 ) rc = 107

    if (verb) print *, '        [] writing ', trim(vname(8))
    call GFIO_PutVar ( fid, vname(8), nymd, nhms, im, jm, 1, km, w_f%v,    err )
    if ( err .ne. 0 ) rc = 108

    if (verb) print *, '        [] writing ', trim(vname(9))
    call GFIO_PutVar ( fid, vname(9), nymd, nhms, im, jm, 1, km, w_f%pt,   err )
    if ( err .ne. 0 ) rc = 109
    do l = 10, 10+lm-1
        if (verb) print *, '    [] writing ', trim(vname(l))
        call GFIO_PutVar ( fid, vname(l), nymd, nhms, im, jm, 1, km, &
                           w_f%q(1:im,1:jm,1:km,l-9), err )
         if ( err .ne. 0 ) rc = 100 + l
    end do

    if ( present(epv) ) then
       if (verb) print *, '     [] writing ', trim(vname(nvars))
       call GFIO_PutVar ( fid, vname(nvars), nymd, nhms, im, jm, 1, km, epv,  err )
       if ( err .ne. 0 ) rc = 100+nvars
    end if


!   Now save vertical grid info as attributes
!   -----------------------------------------
    if ( creating ) then
       call GFIO_PutRealAtt ( fid, 'ptop',   1, ptop,        prec, err )
       if ( err .ne. 0 ) rc = 201
     if ( .not. present(plevs) ) then
       pint = w_f%grid%pint
       ks   = w_f%grid%ks
       call GFIO_PutRealAtt ( fid, 'pint',   1, pint,        prec, err )
       if ( err .ne. 0 ) rc = 202
       call GFIO_PutIntAtt  ( fid, 'ks',     1, ks,          0,    err )
       if ( err .ne. 0 ) rc = 203
       call GFIO_PutRealAtt ( fid, 'ak',  km+1, w_f%grid%ak, prec, err )
       if ( err .ne. 0 ) rc = 204
       call GFIO_PutRealAtt ( fid, 'bk',  km+1, w_f%grid%bk, prec, err )
       if ( err .ne. 0 ) rc = 205
     end if
     if ( present(nstep) ) then
          call GFIO_PutIntAtt ( fid, 'nstep',  1, nstep, 0, err )
          if ( err .ne. 0 ) rc = 206
     endif
    end if

    if (verb) print *, '        [] closing GFIO file ', trim(fname)
    call GFIO_close ( fid, err )

!  Clean up
!  --------
   call ecclean_()

!  All done
!  --------
   return

  CONTAINS

     subroutine ecinit_ ( err )       ! allocates local memory
     integer err
     allocate ( lat(jm), lon(im), lev(km),                              &
              vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
              valid_range(2,nvars), packing_range(2,nvars),             &
              stat=err )
     end subroutine ecinit_

     subroutine ecclean_()             ! de-allocates local memory
     deallocate ( lat, lon, lev,                &
                  vname, vunits, vtitle, kmvar, &
                  valid_range, packing_range,   &
                  stat=err )
     end subroutine ecclean_


  end Subroutine dyn_ecput


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecget --- reads single instance of dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_ecget ( fname, nymd, nhms, w_f, rc, &
                        nstep, timidx, freq, skipSPHU, skipTRACERS ) ! optional
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  character(len=*),    intent(in)   :: fname  ! output file name
  integer, OPTIONAL,   intent(in)   :: timidx ! time index; by default
                                              ! last time index is returned

  logical, OPTIONAL, intent(in) :: skipSPHU   ! if true, does not read SPHU
  logical, OPTIONAL, intent(in) :: skipTRACERS ! if true, does not read any
                                               !  tracer

!
! !INPUT/OUTPUT PARAMETERS:
!
  integer,             intent(inout)   :: nymd  ! Date: year-month-day
  integer,             intent(inout)   :: nhms  ! Time: hour-min-sec
                                                ! Note: unless timidx=0,
                                                !       nymd/nhms will be
                                                !       an output parameter.


!
! !OUTPUT PARAMETERS:
!
  type(dyn_ecvect),      intent(inout)   :: w_f   ! dynamics state vector
  integer,             intent(out)   :: rc    ! error return code:
                                              !   0 - all is well
                                              !  >0 - errors
  integer, OPTIONAL,  intent(inout)    :: nstep ! FVGCM step (for restarts)
  integer, OPTIONAL,   intent(out)   :: freq  ! time frequency on file

!
! !DESCRIPTION: This routine reads GFIO files with one or more instances
!               of the dynamics state vector {\tt w\_f} from a file named
!  {\tt fname}. The file is opened, read from and closed upon completion.
!  By default, this routine returns the last time written to the file.
!  The optional parameter {\tt timidx} allows the retrieval of times
!  other than the last; use routine {\tt GFIO\_DimInquire()} to determine
!  how many times have been written to the file. When {\tt timidx=0}
!  is specified, (nymd,nhms) is used to specify the actual date required
!  (this is the only instance when nymd/nhms are input parameters).
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  26oct1999 da Silva  Added phis_stdv, ts, lwi, a, b
!  02nov1999 da Silva  Fixed bug in call do GFIO_Inquire: (nymd,nhms)
!                      were being passed instead of arrays (yyyymmdd,hhmmss).
!  12nov1999 da Silva  Made (nymd,nhms) output parameters.
!  06dec1999 da Silva  Added nstep.
!  20dec1999 da Silva  Eliminated call to dyn_ecclean(). Now, it is possible
!                      to use previous allocation if size matches.
!  24feb2000 da Silva  Now it returns "last" time on file, in case there
!                      is more than one time per file.
!  10Mar2000 da Silva  Added optinal parameter timidx.
!  04May2000 da Silva  Introduced timidx=0 option, made nymd/nhms inout.
!  23Apr2001 da Silva  Optionally returns freq.
!  10Jun2005 da Silva  Optionally does not read any tracers.
!
!EOP
!-------------------------------------------------------------------------

   character(len=nch)              :: title, source, contact, levunits
   character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)

   real,    allocatable :: lat(:), lon(:), lev(:)
   real,    allocatable :: valid_range(:,:), packing_range(:,:)
   integer, allocatable :: kmvar(:), yyyymmdd(:), hhmmss(:)

   integer :: im, jm, km, lm, nvars
   integer :: l, timinc, lbeg
   real    :: amiss

   integer, parameter :: READ_ONLY = 1
   integer :: fid, err, ngatts

   logical :: readSPHU, readTRACERS

   rc = 0

!  By default, read specific humidity
!  ----------------------------------
   if ( present(skipSPHU) ) then
        readSPHU = .not. skipSPHU
   else
        readSPHU = .true.
   end if
   if ( present(skipTRACERS) ) then
        readTRACERS = .not. skipTRACERS
   else
        readTRACERS = .true.
   end if

!  Open the file
!  -------------
   call GFIO_Open ( fname, READ_ONLY, fid, err )
   if ( err .ne. 0 ) then
      rc = 1
      call ecclean_()
      return
   end if

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
   if ( err .ne. 0 ) then
      call ecclean_()
      rc = 2
   end if
   call ecinit_ ( err )
   if ( err .ne. 0 ) then
      call ecclean_()
      rc = 3
   end if


!  Get file attributes
!  -------------------
   call GFIO_Inquire ( fid, im, jm, km, lm, nvars,     &
                       title, source, contact, amiss,  &
                       lon, lat, lev, levunits,        &
                       yyyymmdd, hhmmss, timinc,       &
                       vname, vtitle, vunits, kmvar,   &
                       valid_range , packing_range, err )
   if ( err .ne. 0 ) then
      call ecclean_()
      rc = 4
   end if

   if ( present(freq) ) then
        freq = timinc
   end if

!  Pick time to return
!  -------------------
   if ( present(timidx) ) then
        if ( timidx .eq. 0 ) then
             continue  ! nothing to do, nymd/nhms set on input
        else if ( timidx .lt. 0 .or. timidx .gt. lm ) then
           call ecclean_()
           rc = 5
           return
        else
           nymd = yyyymmdd(timidx)
           nhms = hhmmss(timidx)
        end if
   else
      nymd = yyyymmdd(lm)
      nhms = hhmmss(lm)
   end if


!
!        ERA40 run
!
   if(km == 61) then
    lm = nvars - 4          ! lm now means the trace dimensions
   endif
!
!        Nature run
!
   if(km == 91) then
    lm = nvars - 3          ! lm now means the trace dimensions
   endif

   print *,' Dyn_ecget:nvars,im,jm,km,lm ',nvars,im,jm,km,lm
   if ( lm < 1 ) then
      rc = 6
      call ecclean_()
      return
   end if


!  Allocate memory if necessary
!  ----------------------------
   call dyn_ecinit ( im, jm, km, lm, w_f, err )
   if ( err .eq. 1 ) then                     ! already allocated
        if ( w_f%grid%im .ne. im  .OR.  &
             w_f%grid%jm .ne. jm  .OR.  &
             w_f%grid%km .ne. km  .OR.  &
            (w_f%grid%lm .ne. lm  .and. readTRACERS) ) then
             rc = 7                           ! current size not compatible
             call ecclean_()
             return
        end if
   else if ( err .ne. 0 ) then
        rc = 8
        call ecclean_()
        return
   end if

!  retrieve the variables
!  ----------------------
!  call GFIO_GetVar ( fid, w_f%phism%name, nymd, nhms, &
!                     im, jm, 0, 1,  w_f%phis,   err )
!  if ( err .ne. 0 )                                        rc = 101
!  call GFIO_GetVar ( fid, w_f%hs_stdvm%name, nymd, nhms, &
!                     im, jm, 0, 1,  w_f%hs_stdv,   err )
!  if ( err .ne. 0 )                                        rc = 102
!  call GFIO_GetVar ( fid, w_f%tsm%name, nymd, nhms, &
!                     im, jm, 0, 1,  w_f%ts,   err )
!  if ( err .ne. 0 )                                        rc = 103
!  call GFIO_GetVar ( fid, w_f%lwim%name, nymd, nhms, &
!                     im, jm, 0, 1,  w_f%lwi,   err )
!  if ( err .ne. 0 )                                        rc = 104
!
!      ---------------------------------------------
!      1/29/2008 Ravi Read PS only if the input file
!      is from ERA40 (Nature run input file does not
!                     contain PS)
!      ---------------------------------------------

  if(km == 61) then
   call GFIO_GetVar ( fid, w_f%psm%name, nymd, nhms, &
                      im, jm, 0, 1,  w_f%ps,   err )

   if(err .eq. 0) then
    w_f%ps = exp(w_f%ps)
   endif
  endif

!  if ( err .ne. 0 )                                        rc = 105
!  call GFIO_GetVar ( fid, w_f%delpm%name, nymd, nhms, &
!                     im, jm, 1, km, w_f%delp, err )
   if ( err .ne. 0 )                                        rc = 106
   call GFIO_GetVar ( fid, w_f%um%name,  nymd, nhms,   &
                      im, jm, 1, km, w_f%u,    err )
   if ( err .ne. 0 )                                        rc = 107
   call GFIO_GetVar ( fid, w_f%vm%name, nymd, nhms,    &
                      im, jm, 1, km, w_f%v, err )
   if ( err .ne. 0 )                                        rc = 108
   call GFIO_GetVar ( fid, w_f%ptm%name, nymd, nhms,   &
                      im, jm, 1, km, w_f%pt,   err )
   if ( err .ne. 0 )                                        rc = 109

         lbeg = 1 ! reads in first tracer (specific humidity)

      do l = lbeg, lm
        print *,' Dyn_ecget:vname ',trim(w_f%qm(l)%name)
        call GFIO_GetVar ( fid, w_f%qm(l)%name, nymd, nhms, &
                           im, jm, 1, km, w_f%q(:,:,:,l), err )
         if ( err .eq. -40 ) then
              rc = 1109 + l       ! variable not found.
         else if ( err .ne. 0 ) then
              rc = 109 + l
         end if
      end do


!   Retrieve vertical grid attributes
!   ---------------------------------
!   call GFIO_GetRealAtt ( fid, 'ptop',   1, w_f%grid%ptop, err )
!   if ( err .ne. 0 ) rc = 201
!   call GFIO_GetRealAtt ( fid, 'pint',   1, w_f%grid%pint, err )
!   if ( err .ne. 0 ) rc = 202
!   call GFIO_GetIntAtt ( fid,  'ks',     1, w_f%grid%ks,   err )
!   if ( err .ne. 0 ) rc = 203
!   call GFIO_GetRealAtt ( fid, 'ak',  km+1, w_f%grid%ak,   err )
!   if ( err .ne. 0 ) rc = 204
!   call GFIO_GetRealAtt ( fid, 'bk',  km+1, w_f%grid%bk,   err )
!   if ( err .ne. 0 ) rc = 205
!   if ( present(nstep) ) then
!      call GFIO_GetIntAtt ( fid,  'nstep',  1, nstep,   err )
!      if ( err .ne. 0 ) rc = 206
!   end if

!   Close GFIO file
!   ---------------
    call GFIO_close ( fid, err )


!  All done
!  --------
   call ecclean_()
   return


  CONTAINS

     subroutine ecinit_ ( err )       ! allocates local memory
     integer err
     allocate ( lat(jm), lon(im), lev(km), yyyymmdd(lm), hhmmss(lm),    &
              vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
              valid_range(2,nvars), packing_range(2,nvars),             &
              stat=err )
     end subroutine ecinit_

     subroutine ecclean_()             ! de-allocates local memory
     deallocate ( lat, lon, lev, yyyymmdd, hhmmss,   &
                  vname, vunits, vtitle, kmvar,      &
                  valid_range, packing_range,        &
                  stat=err )
     end subroutine ecclean_


  end Subroutine dyn_ecget


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecgetdim  - Returns dimensions of dynamics vector
!
! !INTERFACE:
!
    subroutine Dyn_EcGetDim ( w_f, im, jm, km, lm )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!

  type(dyn_ecvect), intent(in)    :: w_f    ! dynamics state vector

!
! !OUTPUT PARAMETERS:
!
 integer, intent(out)           :: im   ! zonal dimension
 integer, intent(out)           :: jm   ! meridional dimension
 integer, intent(out)           :: km   ! vertical dimension
 integer, intent(out)           :: lm   ! "tracer" dimension

!
! !DESCRIPTION: This routine returns dimensions of a dynamics vector.
!
! !REVISION HISTORY:
!
!  17dec1999 da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

   im = w_f%grid%im
   jm = w_f%grid%jm
   km = w_f%grid%km
   lm = w_f%grid%lm

 end subroutine Dyn_ecGetDim

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecgetcoords  - Returns lat/lon/lev coordinates
!
! !INTERFACE:
!
    subroutine Dyn_EcGetCoords ( w_f, im, jm, km, lon, lat, levm, leve, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!

  type(dyn_ecvect), intent(in)    :: w_f    ! dynamics state vector
  integer, intent(in)           :: im     ! zonal dimension
  integer, intent(in)           :: jm     ! meridional dimension
  integer, intent(in)           :: km     ! vertical dimension

!
! !OUTPUT PARAMETERS:
!

  real, intent(out)             :: lon(im)    ! longitudes in [-180,+180] deg
  real, intent(out)             :: lat(jm)    ! latitude in degrees
  real, intent(out)             :: levm(im,jm,km)   ! mid level in hPa
  real, intent(out)             :: leve(im,jm,km+1) ! edge level in hPa

  integer, intent(out)          :: rc        ! error return code:
                                             !  0 - all is well
                                             !  >0 - errors
!
! !DESCRIPTION: This routine returns the lat/lon/level coordinates of a
!               dynamics vector. Notice that the longitudes are ordered
!  from 0 to 360 as in the FVGCM. However, the longitude {\em values}
!  are always in the range [-180,+180] as required by PSAS.
!
! !REVISION HISTORY:
!
!  17dec1999 da Silva  Initial code.
!  10jun2001 da Silva  Fixed lat dim bug
!
!EOP
!-------------------------------------------------------------------------

  integer i, j, k
  real dLon, dLat

! Consistency check
! -----------------
  rc = 0
  if ( im .ne. w_f%grid%im ) rc = 1
  if ( jm .ne. w_f%grid%jm ) rc = 2
  if ( km .ne. w_f%grid%km ) rc = 3
  if ( rc .ne. 0 ) return

! Horizontal grid using PSAS A-grid conventions
! ---------------------------------------------
  lon = (/ (w_f%grid%lon_min + (i-1)*w_f%grid%lon_del, i=1,im) /)
  lat = (/ (w_f%grid%lat_min + (i-1)*w_f%grid%lat_del, i=1,jm) /)

! Make sure longitude values are in hte [-180,180] range
! ------------------------------------------------------
  where ( lon .gt. 180.0 ) lon = lon - 360.0

! Edge levels
! -----------
  leve(1:im,1:jm,1) = 0.01 *  w_f%grid%ptop
  do k = 2, km+1
     leve(1:im,1:jm,k) = leve(1:im,1:jm,k-1)+0.01*w_f%delp(1:im,1:jm,k-1)
  end do

! Mid levels
! ----------
  levm(1:im,1:jm,1:km) = ( leve(1:im,1:jm,1:km) + leve(1:im,1:jm,2:km+1) ) / 2.

! All done
! --------

  end subroutine Dyn_EcGetCoords


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_ecstat --- Prints vital stats of dynamics state vector
!
! !INTERFACE:
!
  subroutine  dyn_ecstat ( lu, w_f, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  integer, intent(in)               :: lu     ! FORTRAN unit number for ASCII output
  type(dyn_ecvect), intent(in)        :: w_f    ! dynamics state vector

!
! !OUTPUT PARAMETERS:
!

  integer, intent(out), optional    :: rc    ! error return code:
                                             !  0 - all is well
                                             !  >0 - errors
!
! !DESCRIPTION: This routine prints basic stats about a dynamics state vector.
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  21Dec2001 Dee       Fixed: print delp stats for all layers
!
!EOP
!-------------------------------------------------------------------------

       integer :: im, jm, km, lm, l,  ios, k
       real, allocatable :: levs(:)
       real :: amiss

       if ( present(rc) ) rc = 0
       im = w_f%grid%im; jm = w_f%grid%jm
       km = w_f%grid%km; lm = w_f%grid%lm
       amiss = w_f%missing_value
       allocate ( levs(km), stat = ios )
       if ( ios .ne. 0 ) then
          if ( present(rc) ) rc = 1
          return
       end if

       do k = 1, km
          levs(k) = k
       end do

       call GDSTATec_ (lu,im,jm,1,w_f%phis, levs,'HGHT','lev',amiss, &
                     w_f%phism%long_name, 1 )
       call GDSTATec_ (lu,im,jm,1,w_f%hs_stdv, levs,'HGHT','lev',amiss, &
                     w_f%hs_stdvm%long_name, 1 )
       call GDSTATec_ (lu,im,jm,1,w_f%ts, levs,'TEMP','lev',amiss, &
                     w_f%tsm%long_name, 1 )
       call GDSTATec_ (lu,im,jm,1,w_f%lwi, levs,'XXXX','lev',amiss, &
                     w_f%lwim%long_name, 1 )
       call GDSTATec_ (lu,im,jm,1,w_f%ps, levs,'PRES','lev ',amiss, &
                     w_f%psm%long_name, 1 )

       call GDSTATec_ (lu,im,jm,km,w_f%delp, levs,'PRES','lev ',amiss, &
                     w_f%delpm%long_name, 1 )
       call GDSTATec_ (lu,im,jm,km,w_f%u, levs,'WIND','lev ',amiss, &
                     w_f%um%long_name, 1 )
       call GDSTATec_ (lu,im,jm,km,w_f%v, levs,'WIND','lev ',amiss, &
                     w_f%vm%long_name, 1 )
       call GDSTATec_ (lu,im,jm,km,w_f%pt,levs,'TEMP','lev ',amiss, &
                     w_f%ptm%long_name, 1 )
       do l = 1, lm
         call GDSTATec_ (lu,im,jm,km,w_f%q(1:im,1:jm,1:km,l), &
                      levs,'XXXX','lev ',amiss, &
                     w_f%qm(l)%long_name, 1 )
       end do

       deallocate ( levs )

CONTAINS

        subroutine GDSTATec_ (lu,mx,my,mz,a,h,atype,htype,amiss,header,inc)

!       Print statistics of one 3-d variable. This is from the PSAS library.
!       It is reproduced here to avoid unnecessary dependencies.

        implicit none

        integer lu              ! Output unit
        integer mx,my,mz        ! Array sizes
        real a(mx,my,mz)        ! The array
        real h(mz)              ! The argument(levels)
        character(*), intent(in) :: atype       ! Type of the variable
        character(*), intent(in) :: htype       ! Typf of the levels
        real amiss              ! missing value flag of a
        character*(*) header    ! A header message
        integer inc             ! order of the listing

        integer i,j,k
        integer kfr,kto,kinc
        integer imx,imn,jmx,jmn
        integer knt
        real amx,amn
        real avg,dev,d
        logical first

!       ..A practical value for the magnitude of the fraction of a real
!       number.

        real rfrcval
        parameter(rfrcval=1.e-5)

        character*255 dash

!       ..function


        logical spv
        real aspv
        spv(aspv)=abs((aspv-amiss)/amiss).le.rfrcval

        do i = 1, 255
           dash(i:i) = '-'
        end do
        i = len(trim(header))
        write(lu,'(//a)') trim(header)
        write(lu,'(a/)')  dash(1:i)
        if(htype.eq.'PRES') then
          write(lu,'(a,3x,a,2x,a,5x,a,6x,a,9x,a,15x,a)') 'lvl','mbar', &
            'count','mean','stdv','maxi','mini'
        elseif(htype.eq.'HGHT') then
          write(lu,'(a,2x,a,2x,a,5x,a,6x,a,9x,a,15x,a)') 'lvl','meter', &
            'count','mean','stdv','maxi','mini'
        elseif(htype.eq.'TEMP') then
          write(lu,'(a,4x,a,4x,a,5x,a,6x,a,9x,a,15x,a)') 'lvl','K', &
            'count','mean','stdv','maxi','mini'
        else
          write(lu,'(a,4x,a,4x,a,5x,a,6x,a,9x,a,15x,a)') 'lvl',htype, &
            'count','mean','stdv','maxi','mini'
        endif

!       ..Check the order of the listing, increase or decrease
        if(inc.ge.0) then
          kfr=1
          kto=mz
          kinc=1
        else
          kfr=mz
          kto=1
          kinc=-1
        endif

        do k=kfr,kto,kinc
          knt=0
          avg=0.
          do j=1,my
            do i=1,mx
              if(.not.spv(a(i,j,k))) then
                knt=knt+1
                avg=avg+a(i,j,k)
              endif
            end do
          end do
          avg=avg/max(1,knt)

          dev=0.
          do j=1,my
            do i=1,mx
              if(.not.spv(a(i,j,k))) then
                d=a(i,j,k)-avg
                dev=dev+d*d
              endif
            end do
          end do
          dev=sqrt(dev/max(1,knt-1))

          amx=a(1,1,k)
          amn=a(1,1,k)
          first=.true.
          do j=1,my
            do i=1,mx
              if(.not.spv(a(i,j,k))) then
                if(first) then
                  imx=i
                  imn=i
                  jmx=j
                  jmn=j
                  amx=a(imx,jmx,k)
                  amn=a(imn,jmn,k)
                  first=.false.
                else
                  if(a(i,j,k).gt.amx) then
                    amx=a(i,j,k)
                    imx=i
                    jmx=j
                  endif
                  if(a(i,j,k).lt.amn) then
                    amn=a(i,j,k)
                    imn=i
                    jmn=j
                  endif
                endif
              endif
            end do
          end do

          if(atype.eq.'RELH') then
            avg=avg*100.
            dev=dev*100.
            amx=amx*100.
            amn=amn*100.
          endif

          if(htype.eq.'PRES'.or.htype.eq.'HGHT') then
            if(atype.eq.'HGHT'.or.atype.eq.'STRM') then
              write(lu,'(i3,2i7,2i10,'// &
                '2(i10,a,i3,a,i3,a))') &
                k,nint(h(k)),knt,nint(avg),nint(dev), &
                nint(amx),'(',imx,',',jmx,')', &
                nint(amn),'(',imn,',',jmn,')'
            elseif(atype.eq.'TEMP'.or.atype.eq.'PRES'.or. &
              atype.eq.'WIND'.or.atype.eq.'%REH'.or. &
              atype.eq.'RELH'.or.atype.eq.'MIXR') then
              write(lu,'(i3,2i7,2f10.2,'// &
                '2(f10.2,a,i3,a,i3,a))') &
                k,nint(h(k)),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            elseif(atype.eq.'NORM') then
              write(lu,'(i3,2i7,2f10.4,'// &
                '2(f10.4,a,i3,a,i3,a))') &
                k,nint(h(k)),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            else
              write(lu,'(i3,2i7,1p,2e10.3e1,0p,'// &
                '2(1p,e10.3e1,0p,a,i3,a,i3,a))') &
                k,nint(h(k)),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            endif

          elseif(htype.eq.'TEMP') then
            if(atype.eq.'HGHT'.or.atype.eq.'STRM') then
              write(lu,'(i3,f7.2,i7,2i10,'// &
                '2(i10,a,i3,a,i3,a))') &
                k,h(k),knt,nint(avg),nint(dev), &
                nint(amx),'(',imx,',',jmx,')', &
                nint(amn),'(',imn,',',jmn,')'
            elseif(atype.eq.'TEMP'.or.atype.eq.'PRES'.or. &
              atype.eq.'WIND'.or.atype.eq.'%REH'.or. &
              atype.eq.'RELH'.or.atype.eq.'MIXR') then
              write(lu,'(i3,f7.2,i7,2f10.2,'// &
                '2(f10.2,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            elseif(atype.eq.'NORM') then
              write(lu,'(i3,f7.2,i7,2f10.4,'// &
                '2(f10.4,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev,&
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            else
              write(lu,'(i3,f7.2,i7,1p,2e10.3e1,0p,'// &
                '2(1p,e10.3e1,0p,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            endif

          else
            if(atype.eq.'HGHT'.or.atype.eq.'STRM') then
              write(lu,'(i3,1p,e10.3e1,0p,i7,2i10,'// &
                '2(i10,a,i3,a,i3,a))') &
                k,h(k),knt,nint(avg),nint(dev), &
                nint(amx),'(',imx,',',jmx,')', &
                nint(amn),'(',imn,',',jmn,')'
            elseif(atype.eq.'TEMP'.or.atype.eq.'PRES'.or. &
              atype.eq.'WIND'.or.atype.eq.'%REH'.or. &
              atype.eq.'RELH'.or.atype.eq.'MIXR') then
              write(lu,'(i3,1p,e10.3e1,0p,i7,2f10.2,'// &
                '2(f10.2,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            elseif(atype.eq.'NORM') then
              write(lu,'(i3,1p,e10.3e1,0p,i7,2f10.4,'// &
                '2(f10.4,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            else
              write(lu,'(i3,1p,e10.3e1,0p,i7,1p,2e10.3e1,0p,'// &
                '2(1p,e10.3e1,0p,a,i3,a,i3,a))') &
                k,h(k),knt,avg,dev, &
                amx,'(',imx,',',jmx,')', &
                amn,'(',imn,',',jmn,')'
            endif

          endif
        end do          ! k=kfr,kto,kinc

      end  subroutine GDSTATec_

    end subroutine dyn_ecstat
!
    subroutine get_ecphis(phisFile,nymd,nhms,field_name,phis,im,jm,rc)
    integer                      :: nymd,nhms,im,jm,err
    integer                      :: fidp,rc
    integer, parameter           :: READ_ONLY = 1
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in) :: phisFile
    real                         :: phis(:,:)

!    nymd = 20000601
!    nhms = 000000

!  Open the file
!  -------------
   call GFIO_Open ( phisFile, READ_ONLY, fidp, err )
    if(err .ne. 0) then
       print *,' Problem in Openning  the file ',trim(phisFile),' rc ',err
       stop 25
    else
     print *,' field_name ',trim(field_name)
     call GFIO_GetVar ( fidp, field_name, nymd, nhms, &
                        im, jm, 0, 1,  phis,   rc )

     if(rc .ne. 0 ) then
      print *,' Error getting PHIS from ',trim(phisFile),' rc ',rc
      stop 30
     endif
    endif

!   Close GFIO file
!   ---------------
    call GFIO_close ( fidp, err )
    if(err .ne. 0) then
       print *,' Problem in closing the file ',trim(phisFile),' rc ',err
       stop 35
    endif

     return
     end subroutine get_ecphis
  end module m_ecdyn
