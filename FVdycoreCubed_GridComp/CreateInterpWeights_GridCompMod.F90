
#include "MAPL_Generic.h"

!-----------------------------------------------------------------------
!              ESMA - Earth System Modeling Applications
!-----------------------------------------------------------------------
   Module CreateInterpWeights_GridCompMod

!BOP
!
! !MODULE: CreateInterpWeights_GridCompMod
!
! !USES:

   use ESMF                ! ESMF base class
   use MAPL_Mod            ! GEOS base class

! !PUBLIC MEMBER FUNCTIONS:

  implicit none
  private

  public  SetServices      ! Register component methods
!EOP

contains

!----------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices

! !DESCRIPTION:  SetServices registers Initialize, Run, and Finalize
!   methods for FV. Two stages of the FV run method are registered. The
!   first one does the dynamics calculations, and the second adds 
!   increments from external sources that appear in the Import state.
!   SetServices also creates a private internal state in which FV
!   keeps invariant or auxilliary state variables, as well as pointers to
!   the true state variables. The MAPL internal state contains the
!   true state variables and is managed by MAPL.
!
! !INTERFACE:

   Subroutine SetServices ( gc, rc )

! !ARGUMENTS:

   type(ESMF_GridComp), intent(inout) :: gc     ! gridded component
   integer, intent(out), optional     :: rc     ! return code
    

!EOP         
!----------------------------------------------------------------------
  
    integer                          :: status
    character(len=ESMF_MAXSTR)       :: IAm
    character(len=ESMF_MAXSTR)       :: COMP_NAME

    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "SetServices"
 
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  Initialize, rc=status)
    VERIFY_(STATUS)
 
! Generic SetServices
!--------------------

    call MAPL_GenericSetServices( GC, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine SetServices


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine Initialize ( gc, import, export, clock, rc )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: gc       ! composite gridded component 
  type(ESMF_State),    intent(inout) :: import   ! import state
  type(ESMF_State),    intent(inout) :: export   ! export state
  type(ESMF_Clock),    intent(inout) :: clock    ! the clock
  
  integer, intent(out), OPTIONAL     :: rc       ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error

  integer                            :: status
  character(len=ESMF_MAXSTR)         :: IAm
  character(len=ESMF_MAXSTR)         :: COMP_NAME

  type (MAPL_MetaComp), pointer      :: MAPL
  type (ESMF_Config)                 :: CF
  type (ESMF_VM)                     :: VM
  character (len=ESMF_MAXSTR)        :: strTxt
  character (len=ESMF_MAXSTR)        :: layout_file
  integer :: npx, npy
  integer :: nlon, nlat
  real, allocatable :: data_ll(:,:), data_cs(:,:)

  integer :: numtasks
  integer :: gid, pe0, pe1, pe2, pe3, pe4, pe5, pe6, pe7

! Begin
!------

    Iam = "Initialize"
    call ESMF_GridCompGet( GC, name=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Call Generic Initialize
!------------------------

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetObjectFromGC (GC, MAPL,  RC=STATUS )
    VERIFY_(STATUS)

    call ESMF_VMGetCurrent(VM, rc=status)

  write(strTxt,'(A,i5.5)') trim(Iam), __LINE__
  call MAPL_MemUtilsWrite(VM, strTxt, RC=STATUS )
  VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, layout_file, 'LAYOUT:', default='weights.rc', rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigLoadFile( cf, LAYOUT_FILE, rc = rc )
    call ESMF_ConfigGetAttribute   ( cf, npx, label = 'npx:', default=180, rc = rc )
    call write_parallel(npx)
    npy = npx*6
    call write_parallel(npy)

    call ESMF_VMGet(vm, petCount=numtasks, rc=status)
    call ESMF_VMGet(vm, localPet=gid, rc=status)

    pe0 = 0 ; pe1 = 0 ; pe2 = 0 ; pe3 = 0 ; pe4 = 0 ; pe5 = 0 ; pe6 = 0

    if (gid == pe0) then
    nlon = 144
    nlat = 91
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe1) then
    nlon = 288
    nlat = 181
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe2) then
    nlon = 360
    nlat = 181
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe3) then
    nlon = 540
    nlat = 361
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe4) then
    nlon = 720
    nlat = 361
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe5) then
    nlon = 1080
    nlat = 721
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe6) then
    nlon = 1152
    nlat = 721
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs, 1, npx, 1, npy)
    deallocate( data_ll, data_cs )
    endif

    if (gid == pe7) then
    nlon = 4*npx
    nlat = nlon/2 + 1
    allocate ( data_ll(nlon,nlat) )
    allocate ( data_cs(npx ,npy ) )
    data_ll(:,:) = 1.0
    call cube2latlon(npx, npy, nlon, nlat, data_cs, data_ll)
    deallocate( data_ll, data_cs )
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine Initialize
 
end module CreateInterpWeights_GridCompMod
 
