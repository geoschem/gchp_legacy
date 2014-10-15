!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: m_gfio --- Grided File I/O Class
!
! !INTERFACE:
!
   Module m_gfio

!USES:

   Implicit NONE
!
! !PUBLIC TYPES:
!
   Private
   Public gfio_vect      ! GFIO vector
   Public gfio_meta      ! GFIO global metadata
   Public gfio_data      ! GFIO variable data and metadada

! !PUBLIC MEMBER FUNCTIONS:
!
   Public gfio_init      ! initializes a dynamics state vector
   Public gfio_clean     ! deallocates memory
   Public gfio_put       ! writes GFIO file with single instance
   Public gfio_get       ! reads  GFIO file with single instance
   Public gfio_var2d     ! access function for gridded fields
   Public gfio_var3d     ! access function for gridded fields
   Public gfio_associate ! associate GFIO vector with a file
!
! !DESCRIPTION: This module defines data types and methods for dealing
!               with the GFIO related objects.
!
! !REVISION HISTORY:
!
!  29Jan2002  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

!BOC

!  GFIO global metadata
!  --------------------
   type gfio_meta
      character(len=255) :: title, source, contact, levunits
      integer       :: im, jm, km, lm, nvars, timeinc
      real, pointer :: lat(:), lon(:), lev(:), time(:)
      logical       :: file_associated
      integer       :: fid
   end type gfio_meta
      
!  GFIO data and variable metadata
!  -------------------------------
   type gfio_data
      character(len=255), pointer :: vname, vtitle, vunits
      real,    pointer :: valid_range(2), packing_range(2)
      integer, pointer :: kmvar
      real,    pointer :: data2d(:,:)   ! 2D arrays
      real,    pointer :: data3d(:,:,:) ! 3D arrays
   end type gfio_data

!  GFIO vector
!  -----------
   type gfio_vect
      type(gfio_meta)          :: meta
      type(gfio_data), pointer :: data(:)
   end type gfio_vect


!  Interfaces
!  ----------
   interface gfio_init
      module procedure gfio_init0_   ! basic, dimensional interface
      module procedure gfio_init1_   ! existing file interface
   end interface


CONTAINS

 subroutine gfio_init0_ ( nvars, im, jm, km, this, rc, &
                          timeinc, lat, lon, lev, levunits )
   implicit NONE
   integer, intent(in) :: nvars      ! number of variables
   integer, intent(in) :: im, jm, km ! zonal, meridional & vertical dimensions
   integer, intent(in) :: lm         ! time dimension
   type(gfio_vect), intent(out) :: this      ! Initialized GFIO vector
   integer, intent(out) :: rc                ! error return code
   integer, intent(in), OPTIONAL :: timeinc  ! time increment
   real,    intent(in), OPTIONAL :: lat(:)   ! latitude (deg)
   real,    intent(in), OPTIONAL :: lon(:)   ! longitude (deg)
   real,    intent(in), OPTIONAL :: lev(:)   ! level 
   character(len=*), intent(in), OPTIONAL :: levunits ! level units
 end subroutine gfio_init

 subroutine gfio_init1_ ( fname, this, rc, &
                          vars, query )
   implicit NONE
   character(len=*), intent(in) :: fname  ! GFIO file name
   type(gfio_vect), intent(out) :: this   ! Initialized GFIO vector 
   integer, intent(out) :: rc             ! error return code
   character(len=*), intent(in), OPTIONAL :: vars(:) ! optional variables
 end subroutine gfio_init

 subroutine gfio_associate ( fname, this, rc )
   implicit NONE
   character(len=*), intent(in) :: fname  ! GFIO file name
   type(gfio_vect), intent(out) :: this   ! Initialized GFIO vector 
   integer, intent(out) :: rc             ! error return code
 end subroutine gfio_init

 subroutine gfio_get ( nymd, nhms, this, rc )
   implicit NONE
   character(len=*), intent(in) :: fname  ! GFIO file name
   integer, intent(in)  :: nymd, nhms     ! date/time
   integer, intent(out) :: rc             ! error return code
   character(len=*), intent(in), OPTIONAL :: vars(:) ! variable names
 end subroutine gfio_get

 subroutine gfio_put ( nymd, nhms, this, rc, &
                       new )
   implicit NONE
   character(len=*), intent(in) :: fname  ! GFIO file name
   integer, intent(in)  :: nymd, nhms     ! date/time
   integer, intent(out) :: rc             ! error return code
   logical, intent(in)  :: new            ! whether to create a NEW file
 end subroutine gfio_put

 real function gfio_var2d ( vname, this )
   implicit NONE
   real, pointer, dimension(:,:)   :: gfio_var2d
   character(len=*), intent(in)    :: vname  ! variable name
   type(gfio_vect), intent(out)    :: this   ! Initialized GFIO vector 
 end subroutine gfio_init

 real function gfio_var3d ( vname, this )
   implicit NONE
   real, pointer, dimension(:,:,:) :: gfio_var3d
   character(len=*), intent(in)    :: vname  ! variable name
   type(gfio_vect), intent(out)    :: this   ! Initialized GFIO vector 
 end subroutine gfio_init

end Module m_GFIO
