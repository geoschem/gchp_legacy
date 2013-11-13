! $Id: ESMF_FortranWordsize.cppF90,v 1.8.2.2 2010/02/05 20:01:13 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FortranWordsize.F90"

! ESMF FortranWordsize module
module ESMF_FortranWordsizeMod

!==============================================================================
!
! This file contains wordsize functions that are automatically
! generated from macros to handle the type/kind overloading.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >


#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC FUNCTION:

  public ESMF_FortranUDTPointerSize
  public ESMF_FortranWordsize

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FortranWordsize -- Generic interface to find Fortran data sizes
!
! !INTERFACE:

  interface ESMF_FortranWordsize

    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FortranWordsizeI1 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FortranWordsizeI2 
#endif 
 module procedure ESMF_FortranWordsizeI4 
 module procedure ESMF_FortranWordsizeI8 
 module procedure ESMF_FortranWordsizeR4 
 module procedure ESMF_FortranWordsizeR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!EOPI
  end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FortranUDTPointerSize()"
!BOPI
! !IROUTINE: ESMF_FortranUDTPointerSize - Get upper limit on size of Fortran UDT Pointer in bytes

! !INTERFACE:
  subroutine ESMF_FortranUDTPointerSize(size)
!
! !ARGUMENTS:
    integer, intent(out) :: size
!EOPI
!------------------------------------------------------------------------------
    type simple_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
    end type
    type(simple_udt), pointer :: udt_ptr1, udt_ptr2
    type(simple_udt), target :: udt
    common /udtcom/ udt_ptr1, udt_ptr2

    udt_ptr1 => udt
    udt_ptr2 => udt

    call ESMF_PointerDifference(ESMC_POINTER_SIZE, udt_ptr1, udt_ptr2, size)
    ! because of compiler introduced padding size may actually overestimate the
    ! size of the (UDT, pointer) structure, but all we need is an estimate of
    ! the upper limit

    if (size<4) then
      ! A size smaller than 4 bytes is suspicious and may indicate that the
      ! size was not determined correctly by the above code!
      size = 64 ! bytes - large enough to work on all current platforms
    endif

  end subroutine ESMF_FortranUDTPointerSize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!BOP 
! !IROUTINE: ESMF_FortranWordsize - Return the size in byte units of a scalar 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FortranWordsize() 
! function ESMF_FortranWordsize<typekind>(var, rc) 
! 
! !RETURN VALUE: 
! integer :: ESMF_FortranWordsize<typekind> 
! 
! !ARGUMENTS: 
! <type>(ESMF_KIND_<typekind>), intent(in) :: var 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return the size in units of bytes of a scalar (var) argument. 
! Valid types and kinds supported by the framework are: 
! integers of 1-byte, 2-byte, 4-byte, and 8-byte size, and 
! reals of 4-byte and 8-bytes size. 
! 
! The arguments are: 
! \begin{description} 
! \item [var] 
! Scalar of any supported type and kind 
! \item [rc] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI1(var, rc) 

 integer(ESMF_KIND_I1), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I1) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI1 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI1 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI2(var, rc) 

 integer(ESMF_KIND_I2), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I2) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI2 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI2 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI4(var, rc) 

 integer(ESMF_KIND_I4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I4) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI4 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI4 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI8(var, rc) 

 integer(ESMF_KIND_I8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I8) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI8 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI8 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeR4(var, rc) 

 real(ESMF_KIND_R4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 real(ESMF_KIND_R4) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeR4 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeR4 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeR8(var, rc) 

 real(ESMF_KIND_R8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 real(ESMF_KIND_R8) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 ! dummy argument var is only present to allow TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeR8 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeR8 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

end module ESMF_FortranWordsizeMod
subroutine f_esmf_fortranudtpointersize(size)
  ! C callable interface to ESMF_FortranUDTPointerSize()
  use ESMF_FortranWordsizeMod
  implicit none
  integer::size
  call ESMF_FortranUDTPointerSize(size)
end subroutine
subroutine f_esmf_fortranudtpointercopy(dst, src)
  ! C callable routine that makes a copy of the internal structure of a
  ! Fortran pointer to a user derived type (UDT). ESMF uses this call internally
  ! from the InternalState code in order be able to store and return the pointer
  ! to a UDT passed in by the user.
  !
  ! The implemented scheme rests on a very fundamental assumption, that the
  ! memory footprint of a Fortran pointer to a UDT is UDT-independent!
  ! Internally ESMF does not have access to the actual UDT that is defined in
  ! the user code. Instead this routine defines a dummy UDT called "simple_udt"
  ! below. The arguments are defined as wrappers that hold pointers to this
  ! dummy UDT. With the assumption that the size of the pointer to a UDT is
  ! UDT-independent the pointer assignment below "dst%udt => src%udt" will also
  ! be UDT-independent and essentially copy all of the bytes necessary from the
  ! src to the dst pointer.
  !
  ! The associated unit test src/prologue/tests/ESMF_F95PtrUTest.F90 verifies
  ! that the above assumption holds. If this test starts failing on a platform
  ! we will need to reconsider the entire approach!
  implicit none
  type simple_udt
    sequence
    real :: a, b, c
    integer :: i, j, k
  end type
  type wrapper
    sequence
    type(simple_udt), pointer:: udt
  end type
  type (wrapper):: dst
  type (wrapper):: src
  dst%udt => src%udt ! pointer association copies the dope vector
end subroutine
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOPI 
! !IROUTINE: f_esmf_fortrantkrptrcopy - portably copy Fortran TKR array pointer 
! 
! !INTERFACE: 
! ! Private name; call using f_esmf_fortrantkrptrcopy() 
! function f_esmf_fortrantkrptrcopy<rank><type><kind>(dst, src) 
! 
! !DESCRIPTION: 
! C callable routine that makes a copy of the Fortran array pointer contained 
! in {\tt src} and returns it in {\tt dst}. 
! 
!EOPI 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4di1 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7di1(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7di1 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4di2 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7di2(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7di2 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy1dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy1dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy2dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy2dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy3dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy3dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy4dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy4dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy5dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy5dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy6dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy6dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7di4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7di8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7dr4(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "f_esmf_fortrantkrptrcopy" 
 
 subroutine f_esmf_fortrantkrptrcopy7dr8(dst, src) 
 
 use ESMF_UtilTypesMod 
 implicit none 
 
 type wrapper 
 sequence 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: ptr 
 end type 
 
 type (wrapper):: dst 
 type (wrapper):: src 
 
 dst%ptr => src%ptr ! pointer association copies the dope vector 
 
 end subroutine f_esmf_fortrantkrptrcopy7dr8 
 
!---------------------------------------------------------------------------- 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

