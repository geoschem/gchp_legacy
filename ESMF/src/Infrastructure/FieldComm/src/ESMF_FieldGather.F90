! $Id: ESMF_FieldGather.cppF90,v 1.9.4.1 2010/02/05 19:56:49 svasquez Exp $
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
!
#define ESMF_FILENAME "ESMF_FieldGather.F90"
!
! ESMF Field Communications Gather module
module ESMF_FieldGatherMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldGatherMod - FieldGather routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldGather} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod
    use ESMF_ArrayMod
    implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
! <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
! <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldGather
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldGather.cppF90,v 1.9.4.1 2010/02/05 19:56:49 svasquez Exp $'

!------------------------------------------------------------------------------
    interface ESMF_FieldGather
        !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldGather1DI1 
 module procedure ESMF_FieldGather2DI1 
 module procedure ESMF_FieldGather3DI1 
 module procedure ESMF_FieldGather4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldGather5DI1 
 module procedure ESMF_FieldGather6DI1 
 module procedure ESMF_FieldGather7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldGather1DI2 
 module procedure ESMF_FieldGather2DI2 
 module procedure ESMF_FieldGather3DI2 
 module procedure ESMF_FieldGather4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldGather5DI2 
 module procedure ESMF_FieldGather6DI2 
 module procedure ESMF_FieldGather7DI2 
#endif 
#endif 
 module procedure ESMF_FieldGather1DI4 
 module procedure ESMF_FieldGather1DI8 
 module procedure ESMF_FieldGather1DR4 
 module procedure ESMF_FieldGather1DR8 
 module procedure ESMF_FieldGather2DI4 
 module procedure ESMF_FieldGather2DI8 
 module procedure ESMF_FieldGather2DR4 
 module procedure ESMF_FieldGather2DR8 
 module procedure ESMF_FieldGather3DI4 
 module procedure ESMF_FieldGather3DI8 
 module procedure ESMF_FieldGather3DR4 
 module procedure ESMF_FieldGather3DR8 
 module procedure ESMF_FieldGather4DI4 
 module procedure ESMF_FieldGather4DI8 
 module procedure ESMF_FieldGather4DR4 
 module procedure ESMF_FieldGather4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldGather5DI4 
 module procedure ESMF_FieldGather5DI8 
 module procedure ESMF_FieldGather5DR4 
 module procedure ESMF_FieldGather5DR8 
 module procedure ESMF_FieldGather6DI4 
 module procedure ESMF_FieldGather6DI8 
 module procedure ESMF_FieldGather6DR4 
 module procedure ESMF_FieldGather6DR8 
 module procedure ESMF_FieldGather7DI4 
 module procedure ESMF_FieldGather7DI8 
 module procedure ESMF_FieldGather7DR4 
 module procedure ESMF_FieldGather7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

        module procedure ESMF_FieldGatherNotRoot
    end interface
!------------------------------------------------------------------------------
contains
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! 
! !IROUTINE: ESMF_FieldGather - Gather a Fortran array from an ESMF_Field 
! 
! !INTERFACE: 
! subroutine ESMF_FieldGather<rank><type><kind>(field, farray, patch, & 
! rootPet, vm, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_Field), intent(inout) :: field 
! mtype (ESMF_KIND_mtypekind),dimension(mdim),intent(in),target :: farray 
! integer, intent(in), optional :: patch 
! integer, intent(in) :: rootPet 
! type(ESMF_VM), intent(in), optional :: vm 
! integer, intent(out), optional :: rc 
! 
! 
! !DESCRIPTION: 
! Gather the data of an {ESMF\_Field} object into the {\tt farray} located on 
! {\tt rootPET}. A single DistGrid patch of {\tt array} must be 
! gathered into {\tt farray}. The optional {\tt patch} 
! argument allows selection of the patch. For Fields defined on a single 
! patch DistGrid the default selection (patch 1) will be correct. The 
! shape of {\tt farray} must match the shape of the patch in Field. 
! 
! If the Field contains replicating DistGrid dimensions data will be 
! gathered from the numerically higher DEs. Replicated data elements in 
! numericaly lower DEs will be ignored. 
! 
! This version of the interface implements the PET-based blocking paradigm: 
! Each PET of the VM must issue this call exactly once for {\em all} of its 
! DEs. The call will block until all PET-local data objects are accessible. 
! 
! For examples and associated documentations using this method see Section 
! \ref{sec:field:usage:gather_2dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} object from which data will be gathered. 
! \item[{[farray]}] 
! The Fortran array into which to gather data. Only root 
! must provide a valid {\tt farray}. 
! \item[{[patch]}] 
! The DistGrid patch in {\tt field} from which to gather {\tt farray}. 
! By default {\tt farray} will be gathered from patch 1. 
! \item[rootPet] 
! PET that holds the valid destination array, i.e. {\tt farray}. 
! \item[{[vm]}] 
! Optional {\tt ESMF\_VM} object of the current context. Providing the 
! VM of the current context will lower the method's overhead. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Di1(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Di2(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather1Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather2Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather3Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather4Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather5Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather6Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Di4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Di8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Dr4(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldGather" 
 subroutine ESMF_FieldGather7Dr8(field, farray, & 
 patch, rootPet, vm, rc) 

 ! input arguments 
 type(ESMF_Field) :: field 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayGather to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! perform gather through internal array 
 call ESMF_ArrayGather(array, farray, patch, rootPet, vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldGather7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! -------------------------- ESMF-public method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGather"
subroutine ESMF_FieldGatherNotRoot(field, patch, rootPet, vm, rc)
        type(ESMF_Field), intent(inout) :: field
        integer, intent(in), optional :: patch
        integer, intent(in) :: rootPet
        type(ESMF_VM), intent(in), optional :: vm
        integer, intent(out), optional :: rc
        ! Local variables
        integer :: localrc ! local return code
        type(ESMF_Array) :: array
        ! Initialize return code
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
        ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
        call ESMF_FieldGet(field, array=array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_ArrayGather(array, patch, rootPet, vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        ! Return successfully
        if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_FieldGatherNotRoot
end module
