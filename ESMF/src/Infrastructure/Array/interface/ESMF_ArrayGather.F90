! $Id: ESMF_ArrayGather.cppF90,v 1.16.2.1 2010/02/05 19:52:35 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_ArrayGather.F90"
!==============================================================================
!
! ESMF ArrayGather module
module ESMF_ArrayGatherMod
!
!==============================================================================
!
! This file contains the ArrayGather() methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"


!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_ArrayGatherMod - Provide TKR overloading for ESMF_ArrayGather()
!
! !DESCRIPTION:
!
! The code in this file is part of the {\tt ESMF\_Array} class Fortran API.
!
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod ! ESMF utility types
  use ESMF_InitMacrosMod ! ESMF initializer macros
  use ESMF_BaseMod ! ESMF base class
  use ESMF_LogErrMod ! ESMF error handling
  use ESMF_LocalArrayMod
  use ESMF_ArraySpecMod
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod ! ESMF Fortran-C++ interface helper

  ! class sub modules
  use ESMF_ArrayCreateMod ! contains the ESMF_Array derived type definition
  use ESMF_ArrayGetMod

  implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayGather


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArrayGather.cppF90,v 1.16.2.1 2010/02/05 19:52:35 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayGather -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayGather

! !PRIVATE MEMBER FUNCTIONS:
!
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_ArrayGather1DI1 
 module procedure ESMF_ArrayGather2DI1 
 module procedure ESMF_ArrayGather3DI1 
 module procedure ESMF_ArrayGather4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGather5DI1 
 module procedure ESMF_ArrayGather6DI1 
 module procedure ESMF_ArrayGather7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_ArrayGather1DI2 
 module procedure ESMF_ArrayGather2DI2 
 module procedure ESMF_ArrayGather3DI2 
 module procedure ESMF_ArrayGather4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGather5DI2 
 module procedure ESMF_ArrayGather6DI2 
 module procedure ESMF_ArrayGather7DI2 
#endif 
#endif 
 module procedure ESMF_ArrayGather1DI4 
 module procedure ESMF_ArrayGather1DI8 
 module procedure ESMF_ArrayGather1DR4 
 module procedure ESMF_ArrayGather1DR8 
 module procedure ESMF_ArrayGather2DI4 
 module procedure ESMF_ArrayGather2DI8 
 module procedure ESMF_ArrayGather2DR4 
 module procedure ESMF_ArrayGather2DR8 
 module procedure ESMF_ArrayGather3DI4 
 module procedure ESMF_ArrayGather3DI8 
 module procedure ESMF_ArrayGather3DR4 
 module procedure ESMF_ArrayGather3DR8 
 module procedure ESMF_ArrayGather4DI4 
 module procedure ESMF_ArrayGather4DI8 
 module procedure ESMF_ArrayGather4DR4 
 module procedure ESMF_ArrayGather4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGather5DI4 
 module procedure ESMF_ArrayGather5DI8 
 module procedure ESMF_ArrayGather5DR4 
 module procedure ESMF_ArrayGather5DR8 
 module procedure ESMF_ArrayGather6DI4 
 module procedure ESMF_ArrayGather6DI8 
 module procedure ESMF_ArrayGather6DR4 
 module procedure ESMF_ArrayGather6DR8 
 module procedure ESMF_ArrayGather7DI4 
 module procedure ESMF_ArrayGather7DI8 
 module procedure ESMF_ArrayGather7DR4 
 module procedure ESMF_ArrayGather7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

    module procedure ESMF_ArrayGatherNotRoot

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayGather} functions.
!EOPI
  end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!===============================================================================
! ArrayGather() interfaces
!===============================================================================
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
! -------------------------- ESMF-public method ----------------------------- 
!BOP 
! 
! !IROUTINE: ESMF_ArrayGather - Gather a Fortran array from an ESMF_Array 
! 
! !INTERFACE: 
! subroutine ESMF_ArrayGather<rank><type><kind>(array, farray, patch, & 
! rootPet, vm, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_Array), intent(inout) :: array 
! mtype (ESMF_KIND_mtypekind),dimension(mdim),intent(in),target :: farray 
! integer, intent(in), optional :: patch 
! integer, intent(in) :: rootPet 
! type(ESMF_VM), intent(in), optional :: vm 
! integer, intent(out), optional :: rc 
! 
! 
! !DESCRIPTION: 
! Gather the data of an {ESMF\_Array} object into the {\tt farray} located on 
! {\tt rootPET}. A single DistGrid patch of {\tt array} must be 
! gathered into {\tt farray}. The optional {\tt patch} 
! argument allows selection of the patch. For Arrays defined on a single 
! patch DistGrid the default selection (patch 1) will be correct. The 
! shape of {\tt farray} must match the shape of the patch in Array. 
! 
! If the Array contains replicating DistGrid dimensions data will be 
! gathered from the numerically higher DEs. Replicated data elements in 
! numericaly lower DEs will be ignored. 
! 
! This version of the interface implements the PET-based blocking paradigm: 
! Each PET of the VM must issue this call exactly once for {\em all} of its 
! DEs. The call will block until all PET-local data objects are accessible. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} object from which data will be gathered. 
! \item[{[farray]}] 
! The Fortran array into which to gather data. Only root 
! must provide a valid {\tt farray}. 
! \item[{[patch]}] 
! The DistGrid patch in {\tt array} from which to gather {\tt farray}. 
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
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_i1, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_i1, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Di1 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i1, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i1, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Di1 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i1, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i1, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Di1 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i1, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i1, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Di1 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i1, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i1, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Di1 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i1, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i1, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Di1 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Di1(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i1),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i1, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i1, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Di1 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_i2, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_i2, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Di2 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i2, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i2, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Di2 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i2, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i2, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Di2 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i2, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i2, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Di2 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i2, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i2, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Di2 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i2, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i2, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Di2 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Di2(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i2),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i2, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i2, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Di2 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_i4, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_i4, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_i8, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_i8, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_r4, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_r4, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather1Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(1) ! counts vector 
 integer :: lb(1) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 1 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1)), & 
 ESMF_TYPEKIND_r8, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1)), & 
 ESMF_TYPEKIND_r8, 1, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather1Dr8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i4, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i4, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i8, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_i8, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_r4, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_r4, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather2Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(2) ! counts vector 
 integer :: lb(2) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 2 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2)), & 
 ESMF_TYPEKIND_r8, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2)), & 
 ESMF_TYPEKIND_r8, 2, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather2Dr8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i4, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i4, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i8, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_i8, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_r4, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_r4, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather3Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(3) ! counts vector 
 integer :: lb(3) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 3 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_r8, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3)), & 
 ESMF_TYPEKIND_r8, 3, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather3Dr8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i4, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i4, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i8, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_i8, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_r4, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_r4, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather4Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(4) ! counts vector 
 integer :: lb(4) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 4 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_r8, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4)), & 
 ESMF_TYPEKIND_r8, 4, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather4Dr8 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i4, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i4, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i8, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_i8, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_r4, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_r4, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather5Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(5) ! counts vector 
 integer :: lb(5) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 5 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_r8, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5)), & 
 ESMF_TYPEKIND_r8, 5, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather5Dr8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i4, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i4, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i8, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_i8, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_r4, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_r4, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather6Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(6) ! counts vector 
 integer :: lb(6) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 6 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_r8, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)), & 
 ESMF_TYPEKIND_r8, 6, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather6Dr8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Di4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i4),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i4, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i4, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Di4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Di8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 integer (ESMF_KIND_i8),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i8, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_i8, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Di8 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Dr4(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r4),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_r4, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_r4, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Dr4 
!---------------------------------------------------------------------------- 
 
! -------------------------- ESMF-public method ----------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_ArrayGather" 
 subroutine ESMF_ArrayGather7Dr8(array, farray, patch, & 
 rootPet, vm, rc) 

 type(ESMF_Array), intent(inout) :: array 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:,:),intent(in),target :: farray 
 integer, intent(in), optional :: patch 
 integer, intent(in) :: rootPet 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! Local variables 
 integer :: localrc ! local return code 
 integer :: counts(7) ! counts vector 
 integer :: lb(7) ! lb vector 
 integer :: i, localPet, count 
 type(ESMF_VM) :: vm_opt 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:,:),pointer :: fptr 
 real (ESMF_KIND_r8),dimension(:,:,:,:,:,:,:),allocatable :: farray_dummy 

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Obtain localPet 
 if (present(vm)) then 
 vm_opt = vm 
 else 
 call ESMF_VMGetCurrent(vm_opt, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 call ESMF_VMGet(vm_opt, localPet=localPet, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (localPet==rootPet) then 
 ! rootPet -> prepare counts vector 
 ! The following use of fptr is a bit of trickery to get all Fortran 
 ! compilers to cooperate. For some compilers the associated() test 
 ! will return .false. for farray of size 0. Some of those compilers 
 ! will produce a run-time error in size(fptr). Other compilers will 
 ! return .true. for the associated() test but return 0 in size(). 
 fptr => farray 
 if (associated(fptr,farray)) then 
 count = 1 ! initialize 
 do i=1, 7 
 counts(i) = size(fptr, i) 
 count = count * counts(i) 
 enddo 
 else 
 count = 0 
 endif 
 ! Since farray is an assumed shape dummy array the lower bounds in all 
 ! dimensions will start at 1 following the Fortran 90 standard. 
 lb = 1 ! 
 ! Call into the C++ interface, which will sort out optional arguments 
 if (count > 0) then 
 ! it is safe to use dummy argument farray 
 call c_ESMC_ArrayGather(array, farray(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_r8, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! it is unsafe to use dummy argument farray 
 allocate(farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))) ! allocate a single element 
 call c_ESMC_ArrayGather(array, farray_dummy(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)), & 
 ESMF_TYPEKIND_r8, 7, counts, patch, rootPet, vm, & 
 localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 deallocate(farray_dummy) 
 endif 
 else 
 ! not rootPet -> call through notRoot interface 
 call ESMF_ArrayGather(array=array, patch=patch, rootPet=rootPet, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

 end subroutine ESMF_ArrayGather7Dr8 
!---------------------------------------------------------------------------- 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! -------------------------- ESMF-public method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGather"
  subroutine ESMF_ArrayGatherNotRoot(array, patch, rootPet, vm, rc)
    type(ESMF_Array), intent(inout) :: array
    integer, intent(in), optional :: patch
    integer, intent(in) :: rootPet
    type(ESMF_VM), intent(in), optional :: vm
    integer, intent(out), optional :: rc
    ! Local variables
    integer :: localrc ! local return code
    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayGatherNotRoot(array, patch, rootPet, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_ArrayGatherNotRoot
!----------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_ArrayGatherMod
