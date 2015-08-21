!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: m_dyn --- FVGCM dynamics state class
!
! !INTERFACE:
!
   Module m_dyn

!USES:

   use m_const, only : undef
   use m_set_eta, only : set_eta

   Implicit NONE
!
! !PUBLIC TYPES:
!
   Private
   Public dyn_vect      ! dynamics state vector
   Public dyn_grid      ! 5-D grid information
   Public dyn_meta      ! metadata (variable names, etc.)

! !PUBLIC MEMBER FUNCTIONS:
!
   Public dyn_init      ! initializes a dynamics state vector
   Public dyn_clean     ! deallocates memory
   Public dyn_null      ! nullify pointers
   Public dyn_put       ! writes GFIO file with single instance
   Public dyn_get       ! reads  GFIO file with single instance
   Public dyn_getdim    ! returns lat/lon/lev/tracer dimensions
   Public dyn_getcoords ! returns lat/lon/lev coordinates
   Public dyn_stat      ! prints out vital statistics
   Public dyn_flip      ! flip structure horizontally and vertically

   Public dyn_setvectyp ! set internal dyn-vector type (g4 or g5)
   Public dyn_getvectyp ! retrieve dyn-vector type
   Public dyn_unsetvectyp ! unset internal dyn-vector type

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
!  19May2005  Todling   Added lat/lon to grid component of dyn-vect
!  15Jul2005  Todling   Modifications to allow GEOS-5 output to be read as if dyn-vec
!  22Apr2005  Todling   Added knobs to control dyn-vector type: G5 or G4
!  11Dec2007  Todling   Safety knobs for vectype setting
!  14Jan2009  Nadeau    Increase string variable lengths to 257; Todling to 256
!  05Mar2009  Todling   Add fraction of land/water/ice
!  14May2009  Todling   Making hardwired values more explicit
!  19Jan2010  Todling   Various clean-ups related to dynvectype 4 or 5
!
!EOP
!-------------------------------------------------------------------------

!BOC

   character(len=*), parameter :: myname = 'm_dyn'
   real,    parameter ::  missing_val = undef
   real,    parameter ::  rtol        = 1.e-3
   integer, parameter ::  nstep_def   = 15760
   integer, parameter ::  dynvectyp_def = 4      ! Default dyn-vect type is GEOS-4
   integer, save      ::  dynvectyp    = -1
   integer, parameter ::  nfix2d5 = 9              ! phis, hs_stdv, ts, ps, frlands(5)
   integer, parameter ::  nfix2d4 = 5              ! phis, hs_stdv, ts, ps, lwi
   integer, parameter ::  nfix3d  = 4              ! delp, u, v, pt
   integer, parameter ::  n3dtrc  = 4              ! number of fix tracer: q,o3,qi,ql
   integer, parameter ::  nfix4   = nfix2d4+nfix3d ! fixed vars (minus q)
   integer, parameter ::  nfix5   = nfix2d5+nfix3d ! fixed vars (minus q)
   integer, save      ::  nfix    = nfix4          ! Default as above: GEOS-4

!  5-D grid information
!  --------------------
   type dyn_grid

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
      real, pointer :: lat(:), lon(:)! horizontal grid values
      real, pointer :: ak(:), bk(:)  ! vertical grid coefficients
      real          :: pint          ! pressure at interface
      real          :: ptop

!     Number of tracers
!     -----------------
      integer       :: lb, le, lm

   end type dyn_grid


!  Metadata
!  --------
   integer, parameter :: nch = 256
   type dyn_meta

       character(len=nch) :: name
       character(len=nch) :: long_name
       character(len=nch) :: units

   end type dyn_meta


!  State vector with grid information
!  ----------------------------------
   type dyn_vect

!     Grid information
!     ----------------
      type(dyn_grid) :: grid

!     Metadata for each variable
!     --------------------------
      type(dyn_meta) :: phism
      type(dyn_meta) :: hs_stdvm
      type(dyn_meta) :: psm
      type(dyn_meta) :: tsm
      type(dyn_meta) :: frlandm
      type(dyn_meta) :: frlandicem
      type(dyn_meta) :: frlakem
      type(dyn_meta) :: froceanm
      type(dyn_meta) :: frseaicem
      type(dyn_meta) :: lwim
      type(dyn_meta) :: delpm
      type(dyn_meta) ::  um
      type(dyn_meta) ::  vm
      type(dyn_meta) ::  ptm
      type(dyn_meta), pointer ::  qm(:)=>null()

!     Missing Value
!     -------------
      real            :: missing_value

!     Fields
!     ------
      real, pointer   ::   phis(:,:)     =>null()  ! topography geopotential (g*z_s)
      real, pointer   ::   hs_stdv(:,:)  =>null()  ! topography height stdv
      real, pointer   ::   Ts(:,:)       =>null()  ! sea surface temperature
      real, pointer   ::   frland(:,:)   =>null()  ! fraction of land
      real, pointer   ::   frlandice(:,:)=>null()  ! fraction of land-ice
      real, pointer   ::   frlake(:,:)   =>null()  ! fraction of lake
      real, pointer   ::   frocean(:,:)  =>null()  ! fraction of ocean
      real, pointer   ::   frseaice(:,:) =>null()  ! fraction of sea-ice
      real, pointer   ::   lwi(:,:)      =>null()  ! land-water-ice mask
                                                   !   lwi = 0  over ocean
                                                   !   lwi = 1  over land
                                                   !   lwi = 2  over sea ice
                                                   ! NOTE: same as ORO in FVGCM.

      real, pointer   ::   ps(:,:)  =>null()       ! surface pressure

      real, pointer   :: delp(:,:,:)=>null()       ! pressure thickness
      real, pointer   ::    u(:,:,:)=>null()       ! zonal wind (d-grid or a-grid)
      real, pointer   ::    v(:,:,:)=>null()       ! meridional wind (d-grid or a-grid)
      real, pointer   ::   pt(:,:,:)=>null()       ! scaled virtual potential temperature (or v. temp)
      real, pointer   ::    q(:,:,:,:)=>null()     ! specific humidity & tracers

   end type dyn_vect

!  Interfaces
!  ----------
   interface Dyn_Init
      module procedure Dyn_Init_    ! Standard f90 interface
      module procedure Dyn_Init77_  ! f77 like interface
      module procedure Dyn_Dup_     ! Initializes "like" an existing vector
   end interface

!  Internal Interfaces
!  -------------------
   interface Hflip_
      module procedure HFlip2_    ! Horizontal flip of 2d fields
      module procedure HFlip3_    ! Horizontal flip of 3d fields
   end interface

   interface Dyn_SetVecTyp
      module procedure Dyn_SetVecTyp_
   end interface
   interface Dyn_GetVecTyp
      module procedure Dyn_GetVecTyp_
   end interface
   interface Dyn_UnsetVecTyp
      module procedure Dyn_UnsetVecTyp_
   end interface

   interface Dyn_GetDim
      module procedure Dyn_getdim1_    ! Get dims from type
      module procedure Dyn_getdim2_    ! Get dims from file
   end interface
!EOC

!---------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_Init_ --- Initializes a dynamics state vector
!
! !INTERFACE:
!
  subroutine  Dyn_Init_ ( im, jm, km, lm, w_f, rc,  &
                          ptop, ks, ak, bk, vectype )     ! optional
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
 integer, intent(in), OPTIONAL  :: vectype  ! defines dyn-vector type (g4/g5)



!
! !OUTPUT PARAMETERS:
!
  type(dyn_vect), intent (inout) :: w_f ! dynamics state vector
  integer, intent(out)           :: rc  ! error return code:
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
!  15Jul2005 Todling   Lat/lon arrays in grid definition; lon->[0,360), lat->[-90,90]
!  21Apr2007 Todling   Replaced call to Init by call to SetVectyp
!  05Mar2009 Todling   Add fraction of land/water/ice
!
!EOP
!-------------------------------------------------------------------------

     character(len=*), parameter :: myname_ = myname//'::Dyn_Init_'

     integer    i, j, err, l
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
     if ( associated(w_f%u)        .or. &
          associated(w_f%v)        .or. &
          associated(w_f%pt)       .or. &
          associated(w_f%q)        .or. &
          associated(w_f%delp)     .or. &
          associated(w_f%ps)       .or. &
          associated(w_f%ts)       .or. &
          associated(w_f%frland)   .or. &
          associated(w_f%frlandice).or. &
          associated(w_f%frlake)   .or. &
          associated(w_f%frocean)  .or. &
          associated(w_f%frseaice) .or. &
          associated(w_f%lwi)      .or. &
          associated(w_f%phis)     .or. &
          associated(w_f%hs_stdv)   )     then

          rc = 1
          return

     endif

!    OK, allocate the necessary memory
!    ---------------------------------
     allocate(w_f%phis(im,jm),     stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%hs_stdv(im,jm),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ts(im,jm),       stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frland(im,jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frlandice(im,jm),stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frlake(im,jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frocean(im,jm),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frseaice(im,jm), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%lwi(im,jm),      stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ps(im,jm),      stat = err); if (err.ne. 0) rc = 2

     allocate(w_f%delp(im,jm,km), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%u(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%v(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%pt(im,jm,km),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%q(im,jm,km,lm), stat = err); if (err.ne. 0) rc = 2

     allocate(w_f%grid%lat(jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%grid%lon(im),   stat = err); if (err.ne. 0) rc = 2
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

!    Initialize metadata
!    -------------------
     if (present(vectype)) then
       call Dyn_SetVecTyp_ ( rc, w_f, vectype=vectype )
     else
       call Dyn_SetVecTyp_ ( rc, w_f )
     endif
      if(rc/=0) return

!    Horizontal grid (hardwire A-grid for now)
!    -----------------------------------------
     if ( dynvectyp == 4 ) then
          w_f%grid%lon_min =    0.0
                      nfix = nfix4
     else if ( dynvectyp == 5 ) then
          w_f%grid%lon_min = -180.0
                      nfix = nfix5
     else
         rc=1
         print*, trim(myname_), ': unexpected Dyn vector'
         return         
     endif
     w_f%grid%lon_del = 360.0 / im
     w_f%grid%lon_max = w_f%grid%lon_min + (im-1) * w_f%grid%lon_del
     w_f%grid%lat_min = -90.0
     w_f%grid%lat_max = +90.0
     w_f%grid%lat_del = ( w_f%grid%lat_max - w_f%grid%lat_min ) / ( jm-1)

     do j = 1, jm
        w_f%grid%lat(j) = w_f%grid%lat_min + (j-1) * w_f%grid%lat_del
     enddo
     do i = 1, im
        w_f%grid%lon(i) = w_f%grid%lon_min + (i-1) * w_f%grid%lon_del
     enddo


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

!    Initialize fields with zeros
!    ----------------------------
     w_f%phis      = 0.0
     w_f%hs_stdv   = 0.0
     w_f%Ts        = 0.0
     w_f%frland    = 0.0
     w_f%frlandice = 0.0
     w_f%frlake    = 0.0
     w_f%frocean   = 0.0
     w_f%frseaice  = 0.0
     w_f%lwi       = 0.0

     w_f%ps   = 0.0
     w_f%delp = 0.0
     w_f%u    = 0.0
     w_f%v    = 0.0
     w_f%pt   = 0.0
     w_f%q    = 0.0

!    All done.
!    --------
     rc = 0

  end subroutine Dyn_Init_

  subroutine Dyn_SetVecTyp_ ( rc, w_f, vectype )
  type(dyn_vect), intent(inout), optional :: w_f    ! dynamics state vector
  integer, intent(in),optional  :: vectype
  integer, intent(out)          :: rc 
  if ( present(vectype) ) then
       dynvectyp = vectype 
  else
       dynvectyp = dynvectyp_def 
  endif
  if(dynvectyp==4) nfix=nfix4
  if(dynvectyp==5) nfix=nfix5
  if(present(w_f)) then
     call Dyn_SetMeta_ ( w_f, rc )
  else
     rc=0
  endif
  return
  end subroutine Dyn_SetVecTyp_

  subroutine Dyn_GetVecTyp_ ( vector_type )
  integer, intent(out) :: vector_type
  vector_type = dynvectyp 
  return
  end subroutine Dyn_GetVecTyp_

  subroutine Dyn_UnsetVecTyp_ ()
  dynvectyp = -1 
  return
  end subroutine Dyn_UnsetVecTyp_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_SetMeta_ --- Sets the metadata of a dynamics vector
!
! !INTERFACE:
!
  subroutine Dyn_SetMeta_ ( w_f, rc )
!
! !USES:
!
  implicit NONE
!
! !OUTPUT PARAMETERS:
!
  type(dyn_vect), intent(inout) :: w_f    ! dynamics state vector
 
  integer, optional, intent(out):: rc     ! return error code

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
!  09May2005 Todling   Added hooks for cloud condensate fields
!  09Jan2006 Todling   Revampped cloud water variable again
!  15May2006 Todling   Back to handling qi and ql for cloud water variables
!  21Apr2007 Todling   Handling g4/g5 dyn-vector types
!  17Sep2007 Todling   Slight modification of tracer-handle
!  05Mar2009  Todling   Add fraction of land/water/ice
!
!EOP
!-------------------------------------------------------------------------
 
     character(len=*), parameter :: myname_ = myname//'::Dyn_SetMeta_'

     integer    l, lm, lbeg
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

     w_f%frlandm%name = 'frland';   w_f%frlandm%long_name = 'Fraction-of-land'
     w_f%frlandm%units = '1';

     w_f%frlandicem%name = 'frlandice';   w_f%frlandicem%long_name = 'Fraction-of-land-ice'
     w_f%frlandicem%units = '1';

     w_f%frlakem%name = 'frlake';   w_f%frlakem%long_name = 'Fraction-of-lake'
     w_f%frlakem%units = '1';

     w_f%froceanm%name = 'frocean';   w_f%froceanm%long_name = 'Fraction-of-ocean'
     w_f%froceanm%units = '1';

     w_f%frseaicem%name = 'frseaice';   w_f%frseaicem%long_name = 'Fraction-of-ocean-ice'
     w_f%frseaicem%units = '1';

     w_f%lwim%name = 'lwi';   w_f%lwim%long_name = 'Land-water-ice mask'
     w_f%lwim%units = '1';

     w_f%psm%name = 'ps';   w_f%psm%long_name = 'Surface Pressure'
     w_f%psm%units = 'Pa';

     w_f%delpm%name = 'delp';   w_f%delpm%long_name = 'Pressure Thickness'
     w_f%delpm%units = 'Pa';

!    If GEOS-5: set winds w/ default A-grid names & pt slot to be virtual temperature
!    otherwise, leave vector as defined in GEOS-4
!    --------------------------------------------------------------------------------
     if ( dynvectyp==5 ) then

        w_f%um%name = 'u';  w_f%um%long_name = 'Zonal Wind'
        w_f%um%units = 'm/s'

        w_f%vm%name = 'v'; w_f%vm%long_name = 'Meridional Wind'
        w_f%vm%units = 'm/s'

        w_f%ptm%name = 'tv'; w_f%ptm%long_name = 'Virtual Temperature'
        w_f%ptm%units = 'K'

     else if ( dynvectyp==4 ) then

        w_f%um%name = 'uwnd';  w_f%um%long_name = 'Zonal Wind'
        w_f%um%units = 'm/s'
   
        w_f%vm%name = 'vwnd'; w_f%vm%long_name = 'Meridional Wind'
        w_f%vm%units = 'm/s'

        w_f%ptm%name = 'theta'; w_f%ptm%long_name = 'Scaled Potential Temperature'
        w_f%ptm%units = 'unknown'  ! for now

     else
        print*, trim(myname_), ': unexpected Dyn vector'
        rc = 99
        return

     endif

     w_f%qm(1)%name = 'sphu';  w_f%qm(1)%long_name = 'Specific Humidity'
     w_f%qm(1)%units = 'kg/kg'

     lbeg = 2
     if (lm >= lbeg) then   ! This assumes 2nd tracer to be ozone
         w_f%qm(2)%name = 'ozone';  w_f%qm(2)%long_name = 'Ozone (passive in PSAS-based DAS)'
         w_f%qm(2)%units = 'ppmv'
         lbeg = lbeg + 1
     endif

     if (lm >= lbeg) then   ! This assumes 3rd tracer to be cloud ice water
         w_f%qm(3)%name = 'qitot';  w_f%qm(3)%long_name = 'Mass Fraction Cloud Ice Water'
         w_f%qm(3)%units = 'kg/kg'
         lbeg = lbeg + 1
     endif

     if (lm >= lbeg) then   ! This assumes 4th tracer to be cloud liquid water
         w_f%qm(4)%name = 'qltot';  w_f%qm(4)%long_name = 'Mass Fraction Cloud Liquid Water'
         w_f%qm(4)%units = 'kg/kg'
         lbeg = lbeg + 1
     endif

!    Initialize tracers with a generic name for now
!    TO DO: Make these optional parameters
!    ----------------------------------------------
     do l = lbeg, lm
        write(bfr,'(i3.3)') l-2
        w_f%qm(l)%name = 'tracer'//bfr;  w_f%qm(l)%long_name = 'Tracer '//bfr
        w_f%qm(l)%units = 'unknown'
     end do

   end subroutine Dyn_SetMeta_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Dyn_Dup_ --- Duplicates a dynamics vector
!
! !INTERFACE:
!
  subroutine  Dyn_Dup_ ( w_in, w_out, rc,   &
                                      copy )     ! optional
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  type(dyn_vect), intent(in)    :: w_in   ! existing dynamics vector
  logical, intent(in), OPTIONAL :: copy   ! copy fields if .true.
!
! !OUTPUT PARAMETERS:
!
  type(dyn_vect), intent (inout):: w_out  ! new dynamics state vector
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
!  05Mar2009 Todling   Add fraction of land/water/ice
!
!EOP
!-------------------------------------------------------------------------

! Allocate memory and set grid
! ----------------------------
  call Dyn_Init_ ( w_in%grid%im, w_in%grid%jm, w_in%grid%km, w_in%grid%lm,  &
                   w_out, rc,                                               &
                   w_in%grid%ptop, w_in%grid%ks, w_in%grid%ak, w_in%grid%bk )

! Optionally copy fields
! ----------------------
  if ( present(copy) ) then

     if (copy) then

        w_out%phis      = w_in%phis
        w_out%hs_stdv   = w_in%hs_stdv
        w_out%Ts        = w_in%Ts
        w_out%frland    = w_in%frland
        w_out%frlandice = w_in%frlandice
        w_out%frlake    = w_in%frlake
        w_out%frocean   = w_in%frocean
        w_out%frseaice  = w_in%frseaice
        w_out%lwi       = w_in%lwi

        w_out%ps   = w_in%ps
        w_out%delp = w_in%delp
        w_out%u    = w_in%u
        w_out%v    = w_in%v
        w_out%pt   = w_in%pt
        w_out%q    = w_in%q

     end if

  end if

  end subroutine Dyn_Dup_


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Dyn_Init77_ --- Initializes a dynamics vector (f77 interface)
!
! !INTERFACE:
!
  subroutine  Dyn_Init77_ ( im, jm, km, lm, ptop, ks, ak, bk, &
                            phis, hs_stdv, Ts, lwi, ps,       &
                            frland, frlandice, frlake, frocean, frseaice, &
                            delp, u, v, pt, q, w_f, rc,       &
                            vectype                           )
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
  real, TARGET, intent(in)   :: ak(:)           !   vertical grid a coefficient
  real, TARGET, intent(in)   :: bk(:)           !   vertical grid b coefficient

                                                ! 2D fields:
  real, TARGET, intent(in)   :: phis(:,:)       !  topography geopotential [m]
                                                !    (g*z_s)
  real, TARGET, intent(in)   :: hs_stdv(:,:)    !  topography height stdv
  real, TARGET, intent(in)   :: Ts(:,:)         !  sea surface temperature
  real, TARGET, intent(in)   :: lwi(:,:)        !  land-water-ice mask
  real, TARGET, intent(in)   :: frland(:,:)     !  fraction of land  
  real, TARGET, intent(in)   :: frlandice(:,:)  !  fraction of land-ice
  real, TARGET, intent(in)   :: frlake(:,:)     !  fraction of lake
  real, TARGET, intent(in)   :: frocean(:,:)    !  fraction of ocean
  real, TARGET, intent(in)   :: frseaice(:,:)   !  fraction of sea-ice
  real, TARGET, intent(in)   :: ps(:,:)         !  surface pressure

                                                ! 3D+ Fields:
  real, TARGET, intent(in)   :: delp(:,:,:)     !  pressure thickness [Pa]
  real, TARGET, intent(in)   ::    u(:,:,:)     !  zonal wind [m/s]
  real, TARGET, intent(in)   ::    v(:,:,:)     !  meridional wind [m/s]
  real, TARGET, intent(in)   ::   pt(:,:,:)     !  scaled virtual potential
                                                !  temperature [Pa**kappa K]
  real, TARGET, intent(in)   ::    q(:,:,:,:)   !  specific humidity & tracers
                                                !   [kg/kg]
  integer, optional, intent(in) :: vectype      ! define whether g4 or g5 dyn-vec

!
! !OUTPUT PARAMETERS:
!
  type(dyn_vect), intent (inout) :: w_f ! dynamics state vector
  integer, intent(out)           :: rc  ! error return code:
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
!  15Jul2005 Todling   Lat/lon arrays in grid definition; lon->[0,360), lat->[-90,90]
!  04Jan2006 ENovakov  Missing allocatables!
!  11Dec2007 Todling   GEOS-5 dyn-vec support was missing
!  05Mar2009 Todling   Add fraction of land/water/ice
!  10Mar2009 Todling   lon_min redefined incorrectly!
!
!EOP
!-------------------------------------------------------------------------

     character(len=*), parameter :: myname_ = myname//'::Dyn_Init77_'

     integer i, j, err

!    Sanity check
!    ------------
     rc = 0
     if ( im<1 .or. jm<1 .or. km<1 .or. lm<1 ) then
          rc = 3
          return
     endif

!    Check whether we have a clean slate
!    -----------------------------------
     if ( associated(w_f%u)         .or. &
          associated(w_f%v)         .or. &
          associated(w_f%pt)        .or. &
          associated(w_f%q)         .or. &
          associated(w_f%delp)      .or. &
          associated(w_f%ps)        .or. &
          associated(w_f%ts)        .or. &
          associated(w_f%lwi)       .or. &
          associated(w_f%frland)    .or. &
          associated(w_f%frlandice) .or. &
          associated(w_f%frlake)    .or. &
          associated(w_f%frocean)   .or. &
          associated(w_f%frseaice)  .or. &
          associated(w_f%phis)      .or. &
          associated(w_f%hs_stdv)        ) then

          rc = 1
          return

     endif

!    OK, allocate the necessary memory
!    ---------------------------------
     allocate(w_f%phis(im,jm),      stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%hs_stdv(im,jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ts(im,jm),        stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%lwi(im,jm),       stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frland(im,jm),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frlandice(im,jm), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frlake(im,jm),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frocean(im,jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%frseaice(im,jm),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%ps(im,jm),        stat = err); if (err.ne. 0) rc = 2
                                                                                                                                               
     allocate(w_f%delp(im,jm,km), stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%u(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%v(im,jm,km),    stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%pt(im,jm,km),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%q(im,jm,km,lm), stat = err); if (err.ne. 0) rc = 2
                                                                                                                                               
     allocate(w_f%grid%lat(jm),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%grid%lon(im),   stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%grid%ak(km+1),  stat = err); if (err.ne. 0) rc = 2
     allocate(w_f%grid%bk(km+1),  stat = err); if (err.ne. 0) rc = 2

     allocate(w_f%qm(lm),         stat = err); if (err.ne. 0) rc = 2
     if ( rc .ne. 0 ) return


!    Initialize dimensional attributes
!    ---------------------------------
     w_f%grid%ib = 1;   w_f%grid%ie = im;   w_f%grid%im = im
     w_f%grid%jb = 1;   w_f%grid%je = jm;   w_f%grid%jm = jm
     w_f%grid%kb = 1;   w_f%grid%ke = km;   w_f%grid%km = km
     w_f%grid%lb = 1;   w_f%grid%le = lm;   w_f%grid%lm = lm

!    Initialize metadata
!    -------------------
     call Dyn_SetVecTyp_ ( rc, w_f, vectype=vectype )
      if(rc/=0) return
                                                                                                                                
!    Horizontal grid (hardwire A-grid for now)
!    -----------------------------------------
     if ( dynvectyp == 4 ) then
          w_f%grid%lon_min =    0.0
                      nfix = nfix4
     else if ( dynvectyp == 5 ) then
          w_f%grid%lon_min = -180.0
                      nfix = nfix5
     else
         rc=1
         print*, trim(myname_), ': unexpected Dyn vector'
         return
     endif

!    Horizontal grid (hardwire A-grid for now)
!    -----------------------------------------
     w_f%grid%lon_del = 360.0 / im
     w_f%grid%lon_max = w_f%grid%lon_min + (im-1) * w_f%grid%lon_del
     w_f%grid%lat_min = -90.0
     w_f%grid%lat_max = +90.0
     w_f%grid%lat_del = ( w_f%grid%lat_max - w_f%grid%lat_min ) / ( jm-1)

     do j = 1, jm
        w_f%grid%lat(j) = w_f%grid%lat_min + (j-1) * w_f%grid%lat_del
     enddo
     do i = 1, im
        w_f%grid%lon(i) = w_f%grid%lon_min + (i-1) * w_f%grid%lon_del
     enddo

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
     w_f%frland    => frland
     w_f%frlandice => frlandice
     w_f%frlake    => frlake
     w_f%frocean   => frocean
     w_f%frseaice  => frseaice
     w_f%ps        => ps
     w_f%delp      => delp
     w_f%u         => u
     w_f%v         => v
     w_f%pt        => pt
     w_f%q         => q

   end subroutine Dyn_Init77_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_clean --- Deallocates memory used by dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_clean ( w_f )
!
! !USES:
!
  implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
  type(dyn_vect), intent (inout) :: w_f   ! dynamics state vector

! !DESCRIPTION:
!
!  Deallocates memory used by dynamics state vector.
!
! !REVISION HISTORY:
!
!  20Jul1999 da Silva  Initial code.
!  26oct1999 da Silva  Added hs_stdv, ts, lwi, a, b
!  19May2005 Todling   Added lat/lon
!  05Mar2009 Todling   Add fraction of land/water/ice
!
!EOP
!-------------------------------------------------------------------------

   if ( associated(w_f%phis) )      deallocate(w_f%phis)
   if ( associated(w_f%hs_stdv) )   deallocate(w_f%hs_stdv)
   if ( associated(w_f%ts) )        deallocate(w_f%ts)
   if ( associated(w_f%frland) )    deallocate(w_f%frland)
   if ( associated(w_f%frlandice) ) deallocate(w_f%frlandice)
   if ( associated(w_f%frlake) )    deallocate(w_f%frlake)
   if ( associated(w_f%frocean) )   deallocate(w_f%frocean)
   if ( associated(w_f%frseaice) )  deallocate(w_f%frseaice)
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
   if ( associated(w_f%grid%lon) )  deallocate(w_f%grid%lon)
   if ( associated(w_f%grid%lat) )  deallocate(w_f%grid%lat)

  end subroutine dyn_clean

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_null --- Nullify pointers used by a dynamics state vectors
!
! !INTERFACE:
!
  subroutine  dyn_null ( w_f )
!
! !USES:
!
  implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
  type(dyn_vect), intent (inout) :: w_f   ! dynamics state vector

! !DESCRIPTION:
!
!  Nullify all pointers in w_f.
!
! !REVISION HISTORY:
!
!  23dec1999 da Silva  Initial code.
!  19May2005 Todling   Added lat/lon.
!  05Mar2009 Todling   Add fraction of land/water/ice
!
!EOP
!-------------------------------------------------------------------------

   if ( associated(w_f%phis) )        nullify(w_f%phis)
   if ( associated(w_f%hs_stdv) )     nullify(w_f%hs_stdv)
   if ( associated(w_f%ts) )          nullify(w_f%ts)
   if ( associated(w_f%frland) )      nullify(w_f%frland)
   if ( associated(w_f%frlandice) )   nullify(w_f%frlandice)
   if ( associated(w_f%frlake) )      nullify(w_f%frlake)
   if ( associated(w_f%frocean) )     nullify(w_f%frocean)
   if ( associated(w_f%frseaice) )    nullify(w_f%frseaice)
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
   if ( associated(w_f%grid%lon) )    nullify(w_f%grid%lon)
   if ( associated(w_f%grid%lat) )    nullify(w_f%grid%lat)

  end subroutine dyn_null


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_put --- writes out single instance of dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_put ( fname, nymd, nhms, prec, w_f, rc, &
                        nstep, verbose, new, freq, epv, plevs, vectype, forceflip )   ! optional
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
  type(dyn_vect), intent(inout)     :: w_f    ! dynamics state vector

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
  integer, intent(in), OPTIONAL     :: vectype  ! switch between g4/g5 dyn-vects
  logical, intent(in), OPTIONAL     :: forceflip! allows flip of data%fields only

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
!  02Jun2005 Todling   Redeclared dpref as an actual function; intel comp bug
!  22Apr2007 Todling   Allow reset var names to go between g4 and g5 dyn vect
!                      (forced to turn w_f into inout type)
!  27Apr2007 Todling   Flip lon when creating G-5 file, leave w_f untouched
!  05Mar2009 Todling   Add fraction of land/water/ice; lwi no longer written out
!  16Jan2010 Todling   Add forceflip here as already in the read
!  23Jun2010 Kokron    Initialize fliplon to .false.
!
!EOP
!-------------------------------------------------------------------------

   character(len=nch)              :: title, source, contact, levunits
   character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)

   real,    allocatable :: fld2(:,:), fld3(:,:,:)
   real,    allocatable :: lat(:), lon(:), lev(:)
   real,    allocatable :: valid_range(:,:), packing_range(:,:)
   integer, allocatable :: kmvar(:)

   integer :: ks, im, jm, km, lm, nvars
   integer :: vectype_
   real    :: p, ptop, pint
   integer :: i, j, k, l, timeinc
   integer :: fid, err, mydvect, next

   logical verb, creating, fexists, fliplon

   integer, parameter :: READ_WRITE = 0

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
   nvars = nfix + lm                  ! q + tracers
   if ( present(epv) ) nvars = nvars+1 ! additional EPV output


!  Allocate local work space
!  -------------------------
   rc = 0
   call init_ ( err )
   if ( err .ne. 0 ) then
        call clean_()
        rc = 1
        return
   end if


!  Create coordinate variables
!  ---------------------------
   lat = w_f%grid%lat
   lon = w_f%grid%lon

!  Vertical coordinates: given on input (prs files)
!  ------------------------------------------------
   if ( present(plevs) ) then

        ptop = w_f%grid%ptop
        lev(1:km) = plevs(1:km)

!  Vertical coordinates: fake something for GrADS sake
!  ---------------------------------------------------
   else

      ptop = w_f%grid%ptop
      lev(1) = ptop + 0.5 * dpref(1)
      do k = 2, km
         lev(k) = lev(k-1) + 0.5 * ( dpref(k-1) + dpref(k) )
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
   source = 'Global Modeling and Assimilation Office, NASA/GSFC'
   contact = 'data@gmao.gsfc.nasa.gov'

   if (present(vectype)) then
     vectype_=vectype
   else
     vectype_=dynvectyp
   endif

!  Allow reset of variable names
!  -----------------------------
   call dyn_setvectyp_ ( rc, w_f, vectype=vectype_ )

!  Setup variable information
!  --------------------------
   next=1
   vname(1)  = w_f%phism%name
   vtitle(1) = w_f%phism%long_name
   vunits(1) = w_f%phism%units
   kmvar(1)  = 0

   next=next+1
   vname(next)  = w_f%hs_stdvm%name
   vtitle(next) = w_f%hs_stdvm%long_name
   vunits(next) = w_f%hs_stdvm%units
   kmvar(next)  = 0

   next=next+1
   vname(next)  = w_f%tsm%name
   vtitle(next) = w_f%tsm%long_name
   vunits(next) = w_f%tsm%units
   kmvar(next)  = 0

   if ( dynvectyp==4 ) then

   next=next+1
   vname(next)  = w_f%lwim%name
   vtitle(next) = w_f%lwim%long_name
   vunits(next) = w_f%lwim%units
   kmvar(next)  = 0

   else

   next=next+1
   vname(next)  = w_f%frlandm%name
   vtitle(next) = w_f%frlandm%long_name
   vunits(next) = w_f%frlandm%units
   kmvar(next)  = 0

   next=next+1
   vname(next)  = w_f%frlandicem%name
   vtitle(next) = w_f%frlandicem%long_name
   vunits(next) = w_f%frlandicem%units
   kmvar(next)  = 0

   next=next+1
   vname(next)  = w_f%frlakem%name
   vtitle(next) = w_f%frlakem%long_name
   vunits(next) = w_f%frlakem%units
   kmvar(next)  = 0

   next=next+1
   vname(next)  = w_f%froceanm%name
   vtitle(next) = w_f%froceanm%long_name
   vunits(next) = w_f%froceanm%units
   kmvar(next)  = 0

   next=next+1
   vname(next)  = w_f%frseaicem%name
   vtitle(next) = w_f%frseaicem%long_name
   vunits(next) = w_f%frseaicem%units
   kmvar(next)  = 0

   endif

   next=next+1
   if ( present(plevs) ) then
      vname(next)  = 'slp'
      vtitle(next) = 'Sea level pressure'
      vunits(next) = 'hPa'
      kmvar(next)  = 0
   else
      vname(next)  = w_f%psm%name
      vtitle(next) = w_f%psm%long_name
      vunits(next) = w_f%psm%units
      kmvar(next)  = 0
   end if

   next=next+1
   if ( present(plevs) ) then
      vname(next)  = 'hght'
      vtitle(next) = 'Geopotential height'
      vunits(next) = 'm'
      kmvar(next)  = km
   else
      vname(next)  = w_f%delpm%name
      vtitle(next) = w_f%delpm%long_name
      vunits(next) = w_f%delpm%units
      kmvar(next)  = km
   end if

   next=next+1
   vname(next)  = w_f%um%name
   vtitle(next) = w_f%um%long_name
   vunits(next) = w_f%um%units
   kmvar(next)  = km

   next=next+1
   vname(next)  = w_f%vm%name
   vtitle(next) = w_f%vm%long_name
   vunits(next) = w_f%vm%units
   kmvar(next)  = km

   next=next+1
   if ( present(plevs) ) then
      vname(next)  = 'tmpu'
      vtitle(next) = 'Temperature'
      vunits(next) = 'Kelvin'
      kmvar(next)  = km
   else
      vname(next)  = w_f%ptm%name
      vtitle(next) = w_f%ptm%long_name
      vunits(next) = w_f%ptm%units
      kmvar(next)  = km
   end if
   if (next/=nfix) then
       print*, ' trouble: trying to write wrong number of vars (next,nfix) = ', next,nfix
       rc = 99
       return
   endif

   do l = nfix+1, nfix+lm
      i = l - nfix
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

!  If new GEOS-5 bkg fields make sure lon's are from [-180,+180]
!  -------------------------------------------------------------
   fliplon = .false.
   if ( dynvectyp==5 ) then
       if ( abs(w_f%grid%lon(1))<rtol ) then  ! if lon(1)==0, need to flip
            ! re-orient internal longitudes
            print *, 'm_dyn: treating as GEOS-5 file'
            do i = 1, im
               lon(i) = -180. + (i-1) * w_f%grid%lon_del
            enddo
            fliplon = .true.
       endif
   endif
   if ( present(forceflip) ) then
       if (forceflip) then
           fliplon = .true. ! here, there is no need to flip lon's
                            ! since type of vector defined on input
                            ! defines lon in the desired manner
                            ! (see above)
          print *, ' Force-Flipping Fields Longitudinally ... '
       endif
   endif


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
         call clean_()
         return
    end if

!   Write the data to GFIO file
!   Note: we may want to write out 1 level at a time in case
!         the compiler does aq copy when invoking putvar().
!   --------------------------------------------------------
    if (verb) print *, '        [] writing ', trim(vname(1))
                                next=1
                                fld2 = w_f%phis
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 101

    if (verb) print *, '        [] writing ', trim(vname(2))
                                next = next+1
                                fld2 = w_f%hs_stdv
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 102

    if (verb) print *, '        [] writing ', trim(vname(3))
                                next = next+1
                                fld2 = w_f%ts
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 103

    if ( dynvectyp==4 ) then

    if (verb) print *, '        [] writing ', trim(vname(4))
                                next = next+1
                                fld2 = w_f%lwi
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 104

    else

    if (verb) print *, '        [] writing ', trim(vname(4))
                                next = next+1
                                fld2 = w_f%frland
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 104

    if (verb) print *, '        [] writing ', trim(vname(5))
                                next = next+1
                                fld2 = w_f%frlandice
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 105

    if (verb) print *, '        [] writing ', trim(vname(6))
                                next = next+1
                                fld2 = w_f%frlake
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 106

    if (verb) print *, '        [] writing ', trim(vname(7))
                                next = next+1
                                fld2 = w_f%frocean
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 107

    if (verb) print *, '        [] writing ', trim(vname(8))
                                next = next+1
                                fld2 = w_f%frseaice
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 108

    endif

    if (verb) print *, '        [] writing ', trim(vname(9))
                                next = next+1
                                fld2 = w_f%ps
    if( fliplon ) call hflip_ ( fld2, im, jm )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 0,  1, fld2,   err )
    if ( err .ne. 0 ) rc = 109

    if (verb) print *, '        [] writing ', trim(vname(10))
                                next = next+1
                                fld3 = w_f%delp
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 1, km, fld3, err )
    if ( err .ne. 0 ) rc = 110

    if (verb) print *, '        [] writing ', trim(vname(11))
                                next = next+1
                                fld3 = w_f%u
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 1, km, fld3,    err )
    if ( err .ne. 0 ) rc = 111

    if (verb) print *, '        [] writing ', trim(vname(12))
                                next = next+1
                                fld3 = w_f%v
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 1, km, fld3,    err )
    if ( err .ne. 0 ) rc = 112

    if (verb) print *, '        [] writing ', trim(vname(13))
                                next = next+1
                                fld3 = w_f%pt
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
    call GFIO_PutVar ( fid, vname(next), nymd, nhms, im, jm, 1, km, fld3,   err )
    if ( err .ne. 0 ) rc = 113
    do l = nfix+1, nfix+lm
        if (verb) print *, '    [] writing ', trim(vname(l))
                                fld3 = w_f%q(1:im,1:jm,1:km,l-nfix)
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
        call GFIO_PutVar ( fid, vname(l), nymd, nhms, im, jm, 1, km, &
                           fld3, err )
         if ( err .ne. 0 ) rc = 100 + l
    end do

    if ( present(epv) ) then
       if (verb) print *, '     [] writing ', trim(vname(nvars))
                                fld3 = epv
    if( fliplon ) call hflip_ ( fld3, im, jm, km )
       call GFIO_PutVar ( fid, vname(nvars), nymd, nhms, im, jm, 1, km, fld3,  err )
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

!   Unset internal definition of dynvec type
!   ----------------------------------------
    call Dyn_UnsetVecTyp()

!  Clean up
!  --------
   call clean_()

!  All done
!  --------
   return

  CONTAINS

     subroutine init_ ( err )       ! allocates local memory
     integer err
     allocate ( lat(jm), lon(im), lev(km),                              &
                fld2(im,jm), fld3(im,jm,km),                            &
              vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
              valid_range(2,nvars), packing_range(2,nvars),             &
              stat=err )
     end subroutine init_

     subroutine clean_()             ! de-allocates local memory
     deallocate ( lat, lon, lev,                &
                  fld2, fld3,                   &
                  vname, vunits, vtitle, kmvar, &
                  valid_range, packing_range,   &
                  stat=err )
     end subroutine clean_

!  Reference pressure thickness assuming ps ~ 984 hPa
!  ---------------------------------------------------
     real function dpref (k)
     integer k
     dpref    = ( w_f%grid%ak(k+1) - w_f%grid%ak(k) ) + &
                ( w_f%grid%bk(k+1) - w_f%grid%bk(k) ) * 98400.
     end function dpref

  end Subroutine dyn_put


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_get --- reads single instance of dynamics state
!
! !INTERFACE:
!
  subroutine  dyn_get ( fname, nymd, nhms, w_f, rc, &
                        nstep, timidx, freq, skipSPHU, vectype, forceflip, & ! optional
                        ncf, pncf )                                          ! optional
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  character(len=*),    intent(in)   :: fname    ! output file name
  integer, OPTIONAL,   intent(in)   :: timidx   ! time index; by default
                                                ! last time index is returned

  logical, OPTIONAL, intent(in)     :: skipSPHU ! if true, does not read SPHU
  integer, OPTIONAL, intent(in)     :: vectype  ! dyn-vector type (g4 or g5)
  logical, OPTIONAL, intent(in)     :: forceflip! flip fields at user's request

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
  type(dyn_vect),      intent(inout) :: w_f   ! dynamics state vector
  integer,             intent(out)   :: rc    ! error return code:
                                              !   0 - all is well
                                              !  >0 - errors
  integer, OPTIONAL,   intent(out)   :: nstep ! FVGCM step (for restarts)
  integer, OPTIONAL,   intent(out)   :: freq  ! time frequency on file
  logical, OPTIONAL,   intent(in)    :: ncf   ! specify to read from non-compliant dyn file
  logical, OPTIONAL,   intent(in)    :: pncf  ! specify to read from non-compliant dyn perturbation file

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
!  20dec1999 da Silva  Eliminated call to dyn_clean(). Now, it is possible
!                      to use previous allocation if size matches.
!  24feb2000 da Silva  Now it returns "last" time on file, in case there
!                      is more than one time per file.
!  10Mar2000 da Silva  Added optional parameter timidx.
!  04May2000 da Silva  Introduced timidx=0 option, made nymd/nhms inout.
!  23Apr2001 da Silva  Optionally returns freq.
!  20May2005 Todling   Lat/lon form inquire; missing_value from inquire.
!  15Jul2005 Todling   Using set_eta to define v-grid in case no present in file;
!                      nstep set to default when not present in file; GEOS-5,
!                      flip lons to 0 to 360 when file has lons from -180 to 180
!  22Apr2007 Todling   Knob to differentiate between g4-g5 dyn-vectors
!  28Aug2007 Todling   Add forceflip (needed in case field read in needs
!                         to be converted to opposite of what is file)
!  14Aug2009 Ravi      Modified check to nvars/lm to correct for missing hs_stdv
!  20Feb2014 Todling   ncf knob to allow reading non-compliant (special) file
!  06Mar2014 Todling   pncf knob to allow reading non-compliant perturbation file created by GSI
!
!EOP
!-------------------------------------------------------------------------

   character(len=nch)              :: title, source, contact, levunits, varname
   character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)
   character(len=nch)              :: fldname

   real,    allocatable :: lat(:), lon(:), lev(:)
   real,    allocatable :: valid_range(:,:), packing_range(:,:)
   integer, allocatable :: kmvar(:), yyyymmdd(:), hhmmss(:)

   integer :: im, jm, km, lm, nvars
   integer :: l, timinc, lbeg, myvtype,i
   real    :: amiss

   integer, parameter :: READ_ONLY = 1
   integer :: fid, err, ngatts
   integer :: vectype_

   logical :: readSPHU, fliplon, ncf_, pncf_

   rc = 0

!  By default, read specific humidity
!  ----------------------------------
   if ( present(skipSPHU) ) then
        readSPHU = .not. skipSPHU
   else
        readSPHU = .true.
   end if

!  Non-complaint file knob
!  -----------------------
   if ( present(ncf) ) then
        ncf_ = ncf
        if(ncf_) print*, 'dyn_get: handling non-compliant dyn-vector file'
   else
        ncf_ = .false.
   end if
   if ( present(pncf) ) then
        pncf_ = pncf
        if(pncf_) print*, 'dyn_get: handling non-compliant dyn-vector perturbation file'
   else
        pncf_ = .false.
   end if

   if( pncf_ .and. ncf_ ) then
      rc = 1
      print *, 'm_dyn_get: cannot handle two types of non-compliant files simultaneously, rc=', rc
      return
   endif

!  Open the file
!  -------------
   call GFIO_Open ( fname, READ_ONLY, fid, err )
   if ( err .ne. 0 ) then
      rc = 1
      call clean_()
      return
   end if

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
   if ( err .ne. 0 ) then
      call clean_()
      rc = 2
   end if
   call init_ ( err )
   if ( err .ne. 0 ) then
      call clean_()
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
      call clean_()
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
           call clean_()
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

   if(present(vectype)) then
      vectype_=vectype
   else
      vectype_=dynvectyp_def
   endif
   if(vectype_==4) nfix=nfix4
   if(vectype_==5) nfix=nfix5
   if (ncf_ .or. pncf_) then
      lm = 4                              ! if non-compliant file, force lm to g5 lm 
   else
      lm =  nvars - nfix                  ! lm now means the trace dimensions
      if(lm == 3 ) then
         lm = (nvars - nfix) + 1          ! fix for ana.eta missing hs_stdv
      endif
      if ( lm < 1 ) then
         rc = 6
         call clean_()
         return
      end if
   end if


!  Allocate memory if necessary
!  ----------------------------
   call dyn_init ( im, jm, km, lm, w_f, err, vectype=vectype_ )
   if ( err .eq. 1 ) then                     ! already allocated
        if ( w_f%grid%im .ne. im  .OR.  &
             w_f%grid%jm .ne. jm  .OR.  &
             w_f%grid%km .ne. km  .OR.  &
             w_f%grid%lm .ne. lm  ) then
             rc = 7                           ! current size not compatible
             call clean_()
             return
        end if
   else if ( err .ne. 0 ) then
        rc = 8
        call clean_()
        return
   end if

!  Fill in lat/lon arrays
!  ----------------------
   if ( dynvectyp==4 ) then
       if ( abs(lon(1)+180.0)<rtol ) then
            fliplon = .true.  ! orientation of fields shifted by 180 degs from internal def
            print *, ' Flipping Fields Longitudinally to [0,360] ... '
       else if ( abs(lon(1)-w_f%grid%lon(1))<rtol ) then
            fliplon = .false.  ! orientation of fields as from internal dyn-vect definition
       else 
            rc = 9
            print *, 'm_dyn_get: cannot handle this lon ordering, rc=', rc
            call clean_()
            return
       endif
   else if ( dynvectyp==5 ) then
       if ( abs(lon(1))<rtol ) then
            fliplon = .true.  ! orientation of fields shifted by 180 degs from internal def
            print *, ' Flipping Fields Longitudinally to [-180,180] ... '
       else if ( abs(lon(1)-w_f%grid%lon(1))<rtol ) then
            fliplon = .false.  ! orientation of fields as in desired GEOS-5 order
       else 
            rc = 10
            print *, 'm_dyn_get: cannot handle this lon ordering, rc=', rc
            call clean_()
            return
       endif
   else
            rc = 11
            print *, 'm_dyn_get: cannot handle this vector type, rc=', rc
            call clean_()
            return
   endif
 
   if (ncf_ .or. pncf_) then
      if (dynvectyp/=5 ) then
         print *, 'm_dyn_get: error, non-complaint files must be g5-like (set dynvectype=5)'
         rc = 99      
         call clean_()
         return
      endif
   endif

   if ( present(forceflip) ) then
        if (forceflip) fliplon = .true.
        if (fliplon) then
           if ( abs(w_f%grid%lon(1))<rtol ) then                     ! lon in [0,360] ...
             do i = 1, im
                w_f%grid%lon(i) = -180. + (i-1) * w_f%grid%lon_del   ! force-flip it to [-180,180]
             enddo
           print *, ' Force-Flipping Fields Longitudinally to [-180,180] ... '
           else                                                      ! lon in [-180,180]
             do i = 1, im
                w_f%grid%lon(i) =         (i-1) * w_f%grid%lon_del   ! force-flip it to [0,360]
             enddo
           endif
           print *, ' Force-Flipping Fields Longitudinally to [0,360] ... '
        endif
   endif

   w_f%missing_value = amiss

!  retrieve the variables
!  ----------------------
   if (pncf_) then
      w_f%phis = 0.0
   else
      call GFIO_GetVar ( fid, w_f%phism%name, nymd, nhms, &
                         im, jm, 0, 1,  w_f%phis,   err )
      if ( err .ne. 0 )                                        rc = 101
      if (   fliplon  ) call hflip_ ( w_f%phis,im,jm )
   endif ! pncf

   if (ncf_.or.pncf_) then
      w_f%hs_stdv = 0.0
   else ! ncf
      call GFIO_GetVar ( fid, w_f%hs_stdvm%name, nymd, nhms, &
                         im, jm, 0, 1,  w_f%hs_stdv,   err )
      if ( dynvectyp==4 ) then
          if ( err .ne. 0 ) rc = 102
      else if ( dynvectyp==5 ) then
             if ( err .ne. 0 ) then
                  print*, 'dyn_get: cannot find hs_stdv, zeroing out'
                  w_f%hs_stdv = 0.0
             endif
      else 
             rc = 11
             print *, 'm_dyn_get: cannot handle this vector type, rc=', rc
             call clean_()
             return
      endif
      if (   fliplon  ) call hflip_ ( w_f%hs_stdv,im,jm )
   endif ! ncf

   if (ncf_) then
       w_f%ts  = 0.0
       w_f%frland = 0.0
       w_f%frlandice = 0.0
       w_f%frlake = 0.0
       w_f%frocean = 0.0
       w_f%frseaice = 0.0
   else ! ncf

      call GFIO_GetVar ( fid, w_f%tsm%name, nymd, nhms, &
                         im, jm, 0, 1,  w_f%ts,   err )
      if ( err .ne. 0 )                                        rc = 103
      if (   fliplon  ) call hflip_ ( w_f%ts,im,jm )

      if (pncf_) then ! perturbation files have ts but not fractions
          w_f%frland = 0.0
          w_f%frlandice = 0.0
          w_f%frlake = 0.0
          w_f%frocean = 0.0
          w_f%frseaice = 0.0
      else

         if ( dynvectyp==4 ) then
   
             call GFIO_GetVar ( fid, w_f%lwim%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%lwi,   err )
             if ( err .ne. 0 )                                        rc = 104
             if (   fliplon  ) call hflip_ ( w_f%lwi,im,jm )
   
         else
      
             call GFIO_GetVar ( fid, w_f%frlandm%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%frland,   err )
             if ( err .ne. 0 )                                        rc = 104
             if (   fliplon  ) call hflip_ ( w_f%frland,im,jm )
      
             call GFIO_GetVar ( fid, w_f%frlandicem%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%frlandice,   err )
             if ( err .ne. 0 )                                        rc = 105
             if (   fliplon  ) call hflip_ ( w_f%frlandice,im,jm )
       
             call GFIO_GetVar ( fid, w_f%frlakem%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%frlake,   err )
             if ( err .ne. 0 )                                        rc = 106
             if (   fliplon  ) call hflip_ ( w_f%frlake,im,jm )
      
             call GFIO_GetVar ( fid, w_f%froceanm%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%frocean,   err )
             if ( err .ne. 0 )                                        rc = 107
             if (   fliplon  ) call hflip_ ( w_f%frocean,im,jm )
   
             call GFIO_GetVar ( fid, w_f%frseaicem%name, nymd, nhms, &
                                im, jm, 0, 1,  w_f%frseaice,   err )
             if ( err .ne. 0 )                                        rc = 108
             if (   fliplon  ) call hflip_ ( w_f%frseaice,im,jm )

         endif

      endif ! pncf

   endif ! ncf

   call GFIO_GetVar ( fid, w_f%psm%name, nymd, nhms, &
                      im, jm, 0, 1,  w_f%ps,   err )
   if ( err .ne. 0 )                                        rc = 109
   if (   fliplon  ) call hflip_ ( w_f%ps,im,jm )

   fldname = w_f%delpm%name
   if (ncf_ ) fldname = 'dp'
   if (pncf_) fldname = 'delp'
   call GFIO_GetVar ( fid, fldname, nymd, nhms, &
                      im, jm, 1, km, w_f%delp, err )
   if ( err .ne. 0 )                                        rc = 110
   if (   fliplon  ) call hflip_ ( w_f%delp,im,jm,km )

   call GFIO_GetVar ( fid, w_f%um%name,  nymd, nhms,   &
                      im, jm, 1, km, w_f%u,    err )
   if ( err .ne. 0 ) then ! if fails, try reading as other type
       call dyn_getvectyp ( myvtype ) 
       if(myvtype==4) then
          call Dyn_SetVecTyp_ ( rc, w_f, vectype=5 ) ! yes, this's reversed as it looks
       else
          call Dyn_SetVecTyp_ ( rc, w_f, vectype=4 ) ! yes, this's reversed as it looks
       endif
       call GFIO_GetVar ( fid, varname,  nymd, nhms,   &
                          im, jm, 1, km, w_f%u,    err )
   endif
   if ( err .ne. 0 )                                        rc = 111
   if (   fliplon  ) call hflip_ ( w_f%u,im,jm,km )

   call GFIO_GetVar ( fid, w_f%vm%name, nymd, nhms,    &
                      im, jm, 1, km, w_f%v, err )
   if ( err .ne. 0 )                                        rc = 112
   if (   fliplon  ) call hflip_ ( w_f%v,im,jm,km )

   call GFIO_GetVar ( fid, w_f%ptm%name, nymd, nhms,   &
                      im, jm, 1, km, w_f%pt,   err )
   if ( err .ne. 0 )                                        rc = 113
   if (   fliplon  ) call hflip_ ( w_f%pt,im,jm,km )

   if ( readSPHU ) then
        lbeg = 1 ! reads in first tracer (specific humidity)
   else
        lbeg = min ( n3dtrc+1, lm ) ! do not
   end if 

   if (ncf_.or.pncf_) then
       w_f%q = 0.0
       if (ncf_ ) fldname = 'qv'
       if (pncf_) fldname = 'sphu'
          call GFIO_GetVar ( fid, trim(fldname), nymd, nhms, &
                             im, jm, 1, km, w_f%q(:,:,:,1), err )
       if (pncf_) then ! pert files have ozone
          fldname = 'ozone'
          call GFIO_GetVar ( fid, trim(fldname), nymd, nhms, &
                             im, jm, 1, km, w_f%q(:,:,:,2), err )
       endif
   else ! .not. ncf/pncf
      do l = lbeg, lm
           call GFIO_GetVar ( fid, w_f%qm(l)%name, nymd, nhms, &
                              im, jm, 1, km, w_f%q(:,:,:,l), err )
            if ( l .eq. 2  .and.  err .eq. -40 ) then
                 rc = 1113 + l       ! variable not found.
            else if ( l .eq. 2  .and.  err .ne. 0 ) then
                 rc = 113 + l
            else if ( l .gt. 2  .and.  err .ne. 0 ) then
                 w_f%q(:,:,:,l) = 0.0 ! for backward compatibility, if cloud fields not found, no problem
            end if
            if ( fliplon ) call hflip_ ( w_f%q(:,:,:,l),im,jm,km )
       end do
    end if ! ncf/pncf

!  Calculate LWI
!  -------------
    if(dynvectyp==5) call get_lwi_ ( w_f )

!   Retrieve vertical grid attributes
!   ---------------------------------
    call GFIO_GetRealAtt ( fid, 'ptop',   1, w_f%grid%ptop, err )
    if ( err .ne. 0 ) then   ! in case can't find ptop define all v-grid via set_eta
       call set_eta ( km,w_f%grid%ks,w_f%grid%ptop,w_f%grid%pint,w_f%grid%ak,w_f%grid%bk )
    else
       call GFIO_GetRealAtt ( fid, 'pint',   1, w_f%grid%pint, err )
       if ( err .ne. 0 ) rc = 202
       call GFIO_GetIntAtt ( fid,  'ks',     1, w_f%grid%ks,   err )
       if ( err .ne. 0 ) rc = 203
       call GFIO_GetRealAtt ( fid, 'ak',  km+1, w_f%grid%ak,   err )
       if ( err .ne. 0 ) rc = 204
       call GFIO_GetRealAtt ( fid, 'bk',  km+1, w_f%grid%bk,   err )
       if ( err .ne. 0 ) rc = 205
    endif
    if ( present(nstep) ) then
       call GFIO_GetIntAtt ( fid,  'nstep',  1, nstep,   err )
       if ( err .ne. 0 ) nstep = nstep_def   ! no longer an error
    end if

!   Close GFIO file
!   ---------------
    call GFIO_close ( fid, err )

!   Unset internal definition of dynvec type
!   ----------------------------------------
    call Dyn_UnsetVecTyp()

!  All done
!  --------
   call clean_()
   return


  CONTAINS

     subroutine init_ ( err )       ! allocates local memory
     integer err
     allocate ( lat(jm), lon(im), lev(km), yyyymmdd(lm), hhmmss(lm),    &
              vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
              valid_range(2,nvars), packing_range(2,nvars),             &
              stat=err )
     end subroutine init_

     subroutine clean_()             ! de-allocates local memory
     deallocate ( lat, lon, lev, yyyymmdd, hhmmss,   &
                  vname, vunits, vtitle, kmvar,      &
                  valid_range, packing_range,        &
                  stat=err )
     end subroutine clean_

  end Subroutine dyn_get

  subroutine hflip3_ ( q,im,jm,km )
      implicit none
      integer  im,jm,km,i,j,k
      real, intent(inout) :: q(im,jm,km)
      real, allocatable   :: dum(:)
      allocate ( dum(im) )
      do k=1,km
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j,k)
         dum(i+im/2) = q(i,j,k)
      enddo
         q(:,j,k) = dum(:)
      enddo
      enddo
      deallocate ( dum )
  end subroutine hflip3_

  subroutine hflip2_ ( q,im,jm )
      implicit none
      integer  im,jm,i,j
      real, intent(inout) :: q(im,jm)
      real, allocatable   :: dum(:)
      allocate ( dum(im) )
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j)
         dum(i+im/2) = q(i,j)
      enddo
         q(:,j) = dum(:)
      enddo
      deallocate ( dum )
  end subroutine hflip2_

  subroutine vflip_(q,im,jm,km)
     implicit none
     integer,intent(in) :: im,jm,km
     real,intent(inout) :: q(im,jm,km)
     real, allocatable  :: dum(:)
     integer i,j
     allocate(dum(km))
     do j=1,jm
        do i=1,im
           dum      = q(i,j,:)
           q(i,j,:) = dum(km:1:-1)
       end do
    end do
    deallocate(dum)
  end subroutine vflip_

  subroutine get_lwi_ ( w_f )
! Todling adaptation of L. Takacs change from elsewhere
    implicit none
    integer  im,jm
    type(dyn_vect), intent(inout) :: w_f   ! dynamics state vector
                                                   w_f%lwi = 1.0  ! Land
    where (  w_f%FROCEAN+w_f%FRLAKE >= 0.6       ) w_f%lwi = 0.0  ! Water
    where (  w_f%lwi==0 .and. w_f%FRSEAICE > 0.5 ) w_f%lwi = 2.0  ! Ice
    where (  w_f%lwi==0 .and.     w_f%TS < 271.4 ) w_f%lwi = 2.0  ! Ice

  end subroutine get_lwi_

  subroutine  Dyn_Flip ( w_f )

  implicit none

  type(dyn_vect),intent(inout)  :: w_f
 
  integer im,jm,km
  integer i,L
  real,allocatable:: dum(:)

  im=w_f%grid%im
  jm=w_f%grid%jm
  km=w_f%grid%km

  if(associated(w_f%phis))      call hflip_(w_f%phis     , im, jm)
  if(associated(w_f%hs_stdv))   call hflip_(w_f%hs_stdv  , im, jm)
  if(associated(w_f%ts))        call hflip_(w_f%ts       , im, jm)
  if(associated(w_f%frland))    call hflip_(w_f%frland   , im, jm)
  if(associated(w_f%frlandice)) call hflip_(w_f%frlandice, im, jm)
  if(associated(w_f%frlake))    call hflip_(w_f%frlake   , im, jm)
  if(associated(w_f%frocean))   call hflip_(w_f%frocean  , im, jm)
  if(associated(w_f%frseaice))  call hflip_(w_f%frseaice , im, jm)
  if(associated(w_f%lwi))       call hflip_(w_f%lwi      , im, jm)

  if(associated(w_f%ps)) call hflip_(w_f%ps, im, jm)

  if(associated(w_f%delp)) then
     call hflip_(w_f%delp, im, jm, km)
     call vflip_(w_f%delp, im, jm, km)
  endif
  if(associated(w_f%u)) then
      call hflip_(w_f%u   , im, jm, km)
      call vflip_(w_f%u   , im, jm, km)
  endif
  if(associated(w_f%v)) then
      call hflip_(w_f%v   , im, jm, km)
      call vflip_(w_f%v   , im, jm, km)
  endif
  if(associated(w_f%pt)) then
     call hflip_(w_f%pt  , im, jm, km)
     call vflip_(w_f%pt  , im, jm, km)
  endif
  if(associated(w_f%q)) then
     do L=1,size(w_f%q,4)
        call hflip_(w_f%q(:,:,:,L), im, jm, km)
        call vflip_(w_f%q(:,:,:,L), im, jm, km)
     enddo
  endif

  allocate(dum(im))
  do i=1,im/2
     dum(i) = w_f%grid%lon(i+im/2)
     dum(i+im/2) =  w_f%grid%lon(i)
  enddo
  w_f%grid%lon=dum
  deallocate(dum)

  allocate(dum(km+1))
  dum=w_f%grid%ak
  w_f%grid%ak(:)=dum(km+1:1:-1)
  dum=w_f%grid%bk
  w_f%grid%bk(:)=dum(km+1:1:-1)
  deallocate(dum)

  end subroutine Dyn_Flip

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_getdim1_  - Returns dimensions of dynamics vector
!
! !INTERFACE:
!
    subroutine Dyn_GetDim1_ ( w_f, im, jm, km, lm )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!

  type(dyn_vect), intent(in)    :: w_f    ! dynamics state vector

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

 end subroutine Dyn_GetDim1_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_getdim2_  - Returns dimensions of dynamics vector
!
! !INTERFACE:
!
    subroutine Dyn_GetDim2_ ( fname, im, jm, km, lm, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
 character(len=*), intent(in) :: fname   ! dyn-vector filename
!
! !OUTPUT PARAMETERS:
!
 integer, intent(out)         :: im      ! zonal dimension
 integer, intent(out)         :: jm      ! meridional dimension
 integer, intent(out)         :: km      ! vertical dimension
 integer, intent(out)         :: lm      ! "tracer" dimension

 integer, intent(out)         :: rc      ! return error code
                                                                                                                              
!
! !DESCRIPTION: This routine returns dimensions of a dynamics vector
!               read from a file.
!
! !REVISION HISTORY:
!
!  21Nov2007 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------

  integer :: myim, myjm, mykm, mylm
  integer :: fid, nvars, ngatts, ier
  integer, parameter :: READ_ONLY = 1

   rc = 0

!  Open the file
!  -------------
   call GFIO_Open ( trim(fname), READ_ONLY, fid, ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble reading dims from ',trim(fname)
     rc = 1
     return
   endif

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, myim, myjm, mykm, mylm, nvars, ngatts, ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble getting dims from ',trim(fname)
     rc = 2
     return
   endif

! Close GFIO file
! ---------------
  call GFIO_close ( fid, ier )

  im = myim ! w%grid%im
  jm = myjm ! w%grid%jm
  km = mykm ! w%grid%km
  lm = nvars - nfix       ! lm now means the trace dimensions
  if ( lm < 1 ) then
     rc = 3
     return
  end if

  end subroutine dyn_getdim2_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_getcoords  - Returns lat/lon/lev coordinates
!
! !INTERFACE:
!
    subroutine Dyn_GetCoords ( w_f, im, jm, km, lon, lat, levm, leve, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!

  type(dyn_vect), intent(in)    :: w_f    ! dynamics state vector
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

  end subroutine Dyn_GetCoords


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  dyn_stat --- Prints vital stats of dynamics state vector
!
! !INTERFACE:
!
  subroutine  dyn_stat ( lu, w_f, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
  integer, intent(in)               :: lu     ! FORTRAN unit number for ASCII output
  type(dyn_vect), intent(in)        :: w_f    ! dynamics state vector

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
!  05Mar2009 Todling   Add land/water/ice fractions
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

       call GDSTAT_ (lu,im,jm,1,w_f%phis, levs,'HGHT','lev',amiss, &
                     w_f%phism%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%hs_stdv, levs,'HGHT','lev',amiss, &
                     w_f%hs_stdvm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%ts, levs,'TEMP','lev',amiss, &
                     w_f%tsm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%lwi, levs,'XXXX','lev',amiss, &
                     w_f%lwim%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%frland, levs,'XXXX','lev',amiss, &
                     w_f%frlandm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%frlandice, levs,'XXXX','lev',amiss, &
                     w_f%frlandicem%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%frlake, levs,'XXXX','lev',amiss, &
                     w_f%frlakem%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%frocean, levs,'XXXX','lev',amiss, &
                     w_f%froceanm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%frseaice, levs,'XXXX','lev',amiss, &
                     w_f%frseaicem%long_name, 1 )
       call GDSTAT_ (lu,im,jm,1,w_f%ps, levs,'PRES','lev ',amiss, &
                     w_f%psm%long_name, 1 )

       call GDSTAT_ (lu,im,jm,km,w_f%delp, levs,'PRES','lev ',amiss, &
                     w_f%delpm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,km,w_f%u, levs,'WIND','lev ',amiss, &
                     w_f%um%long_name, 1 )
       call GDSTAT_ (lu,im,jm,km,w_f%v, levs,'WIND','lev ',amiss, &
                     w_f%vm%long_name, 1 )
       call GDSTAT_ (lu,im,jm,km,w_f%pt,levs,'TEMP','lev ',amiss, &
                     w_f%ptm%long_name, 1 )
       do l = 1, lm
         call GDSTAT_ (lu,im,jm,km,w_f%q(1:im,1:jm,1:km,l), &
                      levs,'XXXX','lev ',amiss, &
                     w_f%qm(l)%long_name, 1 )
       end do

       deallocate ( levs )

CONTAINS

        subroutine GDSTAT_ (lu,mx,my,mz,a,h,atype,htype,amiss,header,inc)

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

        character(len=nch) dash

!       ..function


        logical spv
        real aspv
        spv(aspv)=abs((aspv-amiss)/amiss).le.rfrcval

        do i = 1, nch
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

      end  subroutine GDSTAT_

    end subroutine dyn_stat

  end module m_dyn


