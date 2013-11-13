! $Id: ESMF_TypeKindGet.cppF90,v 1.5.2.1 2010/02/05 20:01:17 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_TypeKindGet.F90"

! ESMF TypeKindGet module
module ESMF_TypeKindGetMod

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

      public ESMF_TypeKindGet

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_TypeKindGet"
!BOP
! !IROUTINE: ESMF_TypeKindGet -- Generic interface to return the correct
! ESMF_TypeKind parameter of a scalar.
!
! !INTERFACE:

    interface ESMF_TypeKindGet

!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_TypeKindGetI1 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_TypeKindGetI2 
#endif 
 module procedure ESMF_TypeKindGetI4 
 module procedure ESMF_TypeKindGetI8 
 module procedure ESMF_TypeKindGetR4 
 module procedure ESMF_TypeKindGetR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!EOP
    end interface

    contains

!==============================================================================
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!TODO: add interface documentation 
!BOP 
! !IROUTINE: ESMF_TypeKindGet - Return the ESMF_TypeKind parameter corresponding to a scalar 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_TypeKindGet() 
! function ESMF_TypeKindGet<typekind>(var, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_TypeKind) :: ESMF_TypeKindGet<typekind> 
! 
! !ARGUMENTS: 
! <type>(ESMF_KIND_<typekind>), intent(in) :: var 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return the ESMF_TypeKind parameter that corresponds to the scalar 
! (var) argument. Valid typekind supported by the framework are: 
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
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetI1(var, rc) 

 integer(ESMF_KIND_I1), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetI1 = ESMF_TYPEKIND_I1 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetI1 

!--------------------------------------------------------------------------- 
 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetI2(var, rc) 

 integer(ESMF_KIND_I2), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetI2 = ESMF_TYPEKIND_I2 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetI2 

!--------------------------------------------------------------------------- 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetI4(var, rc) 

 integer(ESMF_KIND_I4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetI4 = ESMF_TYPEKIND_I4 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetI4 

!--------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetI8(var, rc) 

 integer(ESMF_KIND_I8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetI8 = ESMF_TYPEKIND_I8 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetI8 

!--------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetR4(var, rc) 

 real(ESMF_KIND_R4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetR4 = ESMF_TYPEKIND_R4 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetR4 

!--------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_TypeKindGet##mtypekind" 
#define ESMF_METHOD "ESMF_TypeKindGet" 
 type(ESMF_TypeKind) function ESMF_TypeKindGetR8(var, rc) 

 real(ESMF_KIND_R8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! the purpose of dummy argument var is to support TK overloading 
 ! some compilers will notice that var is unused, and issue warnings 
 if (var==var) continue ! dummy check to quiet down compiler warnings 

 ESMF_TypeKindGetR8 = ESMF_TYPEKIND_R8 

 if (present(rc)) rc = ESMF_SUCCESS 

 end function ESMF_TypeKindGetR8 

!--------------------------------------------------------------------------- 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

end module ESMF_TypeKindGetMod
