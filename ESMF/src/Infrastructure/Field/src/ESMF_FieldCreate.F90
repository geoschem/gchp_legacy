! $Id: ESMF_FieldCreate.cppF90,v 1.65.2.1 2010/02/05 19:55:52 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_FieldCreate.F90"
!==============================================================================
!
! ESMF FieldCreate module
module ESMF_FieldCreateMod
!
!==============================================================================
!
! This file contains the FieldCreate() methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"


!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_IOSpecMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_GeomBaseMod
  use ESMF_ArrayMod
  use ESMF_ArrayGetMod
  use ESMF_ArrayCreateMod

  use ESMF_FieldMod
  use ESMF_FieldSetCoMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

  public ESMF_FieldCreate
  public ESMF_FieldCreateEmpty
  public ESMF_FieldDestroy ! Destroy a Field
  public ESMF_FieldDestruct ! For internal ESMF use only


!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldCreate.cppF90,v 1.65.2.1 2010/02/05 19:55:52 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreate - Create a new Field with data
!
! !INTERFACE:
  interface ESMF_FieldCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldCreateGridArraySpec
    module procedure ESMF_FieldCreateGridArray
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateGridData1DI1 
 module procedure ESMF_FieldCreateGridData2DI1 
 module procedure ESMF_FieldCreateGridData3DI1 
 module procedure ESMF_FieldCreateGridData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridData5DI1 
 module procedure ESMF_FieldCreateGridData6DI1 
 module procedure ESMF_FieldCreateGridData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateGridData1DI2 
 module procedure ESMF_FieldCreateGridData2DI2 
 module procedure ESMF_FieldCreateGridData3DI2 
 module procedure ESMF_FieldCreateGridData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridData5DI2 
 module procedure ESMF_FieldCreateGridData6DI2 
 module procedure ESMF_FieldCreateGridData7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateGridData1DI4 
 module procedure ESMF_FieldCreateGridData1DI8 
 module procedure ESMF_FieldCreateGridData1DR4 
 module procedure ESMF_FieldCreateGridData1DR8 
 module procedure ESMF_FieldCreateGridData2DI4 
 module procedure ESMF_FieldCreateGridData2DI8 
 module procedure ESMF_FieldCreateGridData2DR4 
 module procedure ESMF_FieldCreateGridData2DR8 
 module procedure ESMF_FieldCreateGridData3DI4 
 module procedure ESMF_FieldCreateGridData3DI8 
 module procedure ESMF_FieldCreateGridData3DR4 
 module procedure ESMF_FieldCreateGridData3DR8 
 module procedure ESMF_FieldCreateGridData4DI4 
 module procedure ESMF_FieldCreateGridData4DI8 
 module procedure ESMF_FieldCreateGridData4DR4 
 module procedure ESMF_FieldCreateGridData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridData5DI4 
 module procedure ESMF_FieldCreateGridData5DI8 
 module procedure ESMF_FieldCreateGridData5DR4 
 module procedure ESMF_FieldCreateGridData5DR8 
 module procedure ESMF_FieldCreateGridData6DI4 
 module procedure ESMF_FieldCreateGridData6DI8 
 module procedure ESMF_FieldCreateGridData6DR4 
 module procedure ESMF_FieldCreateGridData6DR8 
 module procedure ESMF_FieldCreateGridData7DI4 
 module procedure ESMF_FieldCreateGridData7DI8 
 module procedure ESMF_FieldCreateGridData7DR4 
 module procedure ESMF_FieldCreateGridData7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateGridDataPtr1DI1 
 module procedure ESMF_FieldCreateGridDataPtr2DI1 
 module procedure ESMF_FieldCreateGridDataPtr3DI1 
 module procedure ESMF_FieldCreateGridDataPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridDataPtr5DI1 
 module procedure ESMF_FieldCreateGridDataPtr6DI1 
 module procedure ESMF_FieldCreateGridDataPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateGridDataPtr1DI2 
 module procedure ESMF_FieldCreateGridDataPtr2DI2 
 module procedure ESMF_FieldCreateGridDataPtr3DI2 
 module procedure ESMF_FieldCreateGridDataPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridDataPtr5DI2 
 module procedure ESMF_FieldCreateGridDataPtr6DI2 
 module procedure ESMF_FieldCreateGridDataPtr7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateGridDataPtr1DI4 
 module procedure ESMF_FieldCreateGridDataPtr1DI8 
 module procedure ESMF_FieldCreateGridDataPtr1DR4 
 module procedure ESMF_FieldCreateGridDataPtr1DR8 
 module procedure ESMF_FieldCreateGridDataPtr2DI4 
 module procedure ESMF_FieldCreateGridDataPtr2DI8 
 module procedure ESMF_FieldCreateGridDataPtr2DR4 
 module procedure ESMF_FieldCreateGridDataPtr2DR8 
 module procedure ESMF_FieldCreateGridDataPtr3DI4 
 module procedure ESMF_FieldCreateGridDataPtr3DI8 
 module procedure ESMF_FieldCreateGridDataPtr3DR4 
 module procedure ESMF_FieldCreateGridDataPtr3DR8 
 module procedure ESMF_FieldCreateGridDataPtr4DI4 
 module procedure ESMF_FieldCreateGridDataPtr4DI8 
 module procedure ESMF_FieldCreateGridDataPtr4DR4 
 module procedure ESMF_FieldCreateGridDataPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateGridDataPtr5DI4 
 module procedure ESMF_FieldCreateGridDataPtr5DI8 
 module procedure ESMF_FieldCreateGridDataPtr5DR4 
 module procedure ESMF_FieldCreateGridDataPtr5DR8 
 module procedure ESMF_FieldCreateGridDataPtr6DI4 
 module procedure ESMF_FieldCreateGridDataPtr6DI8 
 module procedure ESMF_FieldCreateGridDataPtr6DR4 
 module procedure ESMF_FieldCreateGridDataPtr6DR8 
 module procedure ESMF_FieldCreateGridDataPtr7DI4 
 module procedure ESMF_FieldCreateGridDataPtr7DI8 
 module procedure ESMF_FieldCreateGridDataPtr7DR4 
 module procedure ESMF_FieldCreateGridDataPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


    module procedure ESMF_FieldCreateLSArraySpec
    module procedure ESMF_FieldCreateLSArray
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateLSData1DI1 
 module procedure ESMF_FieldCreateLSData2DI1 
 module procedure ESMF_FieldCreateLSData3DI1 
 module procedure ESMF_FieldCreateLSData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSData5DI1 
 module procedure ESMF_FieldCreateLSData6DI1 
 module procedure ESMF_FieldCreateLSData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateLSData1DI2 
 module procedure ESMF_FieldCreateLSData2DI2 
 module procedure ESMF_FieldCreateLSData3DI2 
 module procedure ESMF_FieldCreateLSData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSData5DI2 
 module procedure ESMF_FieldCreateLSData6DI2 
 module procedure ESMF_FieldCreateLSData7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateLSData1DI4 
 module procedure ESMF_FieldCreateLSData1DI8 
 module procedure ESMF_FieldCreateLSData1DR4 
 module procedure ESMF_FieldCreateLSData1DR8 
 module procedure ESMF_FieldCreateLSData2DI4 
 module procedure ESMF_FieldCreateLSData2DI8 
 module procedure ESMF_FieldCreateLSData2DR4 
 module procedure ESMF_FieldCreateLSData2DR8 
 module procedure ESMF_FieldCreateLSData3DI4 
 module procedure ESMF_FieldCreateLSData3DI8 
 module procedure ESMF_FieldCreateLSData3DR4 
 module procedure ESMF_FieldCreateLSData3DR8 
 module procedure ESMF_FieldCreateLSData4DI4 
 module procedure ESMF_FieldCreateLSData4DI8 
 module procedure ESMF_FieldCreateLSData4DR4 
 module procedure ESMF_FieldCreateLSData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSData5DI4 
 module procedure ESMF_FieldCreateLSData5DI8 
 module procedure ESMF_FieldCreateLSData5DR4 
 module procedure ESMF_FieldCreateLSData5DR8 
 module procedure ESMF_FieldCreateLSData6DI4 
 module procedure ESMF_FieldCreateLSData6DI8 
 module procedure ESMF_FieldCreateLSData6DR4 
 module procedure ESMF_FieldCreateLSData6DR8 
 module procedure ESMF_FieldCreateLSData7DI4 
 module procedure ESMF_FieldCreateLSData7DI8 
 module procedure ESMF_FieldCreateLSData7DR4 
 module procedure ESMF_FieldCreateLSData7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateLSDataPtr1DI1 
 module procedure ESMF_FieldCreateLSDataPtr2DI1 
 module procedure ESMF_FieldCreateLSDataPtr3DI1 
 module procedure ESMF_FieldCreateLSDataPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSDataPtr5DI1 
 module procedure ESMF_FieldCreateLSDataPtr6DI1 
 module procedure ESMF_FieldCreateLSDataPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateLSDataPtr1DI2 
 module procedure ESMF_FieldCreateLSDataPtr2DI2 
 module procedure ESMF_FieldCreateLSDataPtr3DI2 
 module procedure ESMF_FieldCreateLSDataPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSDataPtr5DI2 
 module procedure ESMF_FieldCreateLSDataPtr6DI2 
 module procedure ESMF_FieldCreateLSDataPtr7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateLSDataPtr1DI4 
 module procedure ESMF_FieldCreateLSDataPtr1DI8 
 module procedure ESMF_FieldCreateLSDataPtr1DR4 
 module procedure ESMF_FieldCreateLSDataPtr1DR8 
 module procedure ESMF_FieldCreateLSDataPtr2DI4 
 module procedure ESMF_FieldCreateLSDataPtr2DI8 
 module procedure ESMF_FieldCreateLSDataPtr2DR4 
 module procedure ESMF_FieldCreateLSDataPtr2DR8 
 module procedure ESMF_FieldCreateLSDataPtr3DI4 
 module procedure ESMF_FieldCreateLSDataPtr3DI8 
 module procedure ESMF_FieldCreateLSDataPtr3DR4 
 module procedure ESMF_FieldCreateLSDataPtr3DR8 
 module procedure ESMF_FieldCreateLSDataPtr4DI4 
 module procedure ESMF_FieldCreateLSDataPtr4DI8 
 module procedure ESMF_FieldCreateLSDataPtr4DR4 
 module procedure ESMF_FieldCreateLSDataPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateLSDataPtr5DI4 
 module procedure ESMF_FieldCreateLSDataPtr5DI8 
 module procedure ESMF_FieldCreateLSDataPtr5DR4 
 module procedure ESMF_FieldCreateLSDataPtr5DR8 
 module procedure ESMF_FieldCreateLSDataPtr6DI4 
 module procedure ESMF_FieldCreateLSDataPtr6DI8 
 module procedure ESMF_FieldCreateLSDataPtr6DR4 
 module procedure ESMF_FieldCreateLSDataPtr6DR8 
 module procedure ESMF_FieldCreateLSDataPtr7DI4 
 module procedure ESMF_FieldCreateLSDataPtr7DI8 
 module procedure ESMF_FieldCreateLSDataPtr7DR4 
 module procedure ESMF_FieldCreateLSDataPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


    module procedure ESMF_FieldCreateMeshArraySpec
    module procedure ESMF_FieldCreateMeshArray
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateMeshData1DI1 
 module procedure ESMF_FieldCreateMeshData2DI1 
 module procedure ESMF_FieldCreateMeshData3DI1 
 module procedure ESMF_FieldCreateMeshData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshData5DI1 
 module procedure ESMF_FieldCreateMeshData6DI1 
 module procedure ESMF_FieldCreateMeshData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateMeshData1DI2 
 module procedure ESMF_FieldCreateMeshData2DI2 
 module procedure ESMF_FieldCreateMeshData3DI2 
 module procedure ESMF_FieldCreateMeshData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshData5DI2 
 module procedure ESMF_FieldCreateMeshData6DI2 
 module procedure ESMF_FieldCreateMeshData7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateMeshData1DI4 
 module procedure ESMF_FieldCreateMeshData1DI8 
 module procedure ESMF_FieldCreateMeshData1DR4 
 module procedure ESMF_FieldCreateMeshData1DR8 
 module procedure ESMF_FieldCreateMeshData2DI4 
 module procedure ESMF_FieldCreateMeshData2DI8 
 module procedure ESMF_FieldCreateMeshData2DR4 
 module procedure ESMF_FieldCreateMeshData2DR8 
 module procedure ESMF_FieldCreateMeshData3DI4 
 module procedure ESMF_FieldCreateMeshData3DI8 
 module procedure ESMF_FieldCreateMeshData3DR4 
 module procedure ESMF_FieldCreateMeshData3DR8 
 module procedure ESMF_FieldCreateMeshData4DI4 
 module procedure ESMF_FieldCreateMeshData4DI8 
 module procedure ESMF_FieldCreateMeshData4DR4 
 module procedure ESMF_FieldCreateMeshData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshData5DI4 
 module procedure ESMF_FieldCreateMeshData5DI8 
 module procedure ESMF_FieldCreateMeshData5DR4 
 module procedure ESMF_FieldCreateMeshData5DR8 
 module procedure ESMF_FieldCreateMeshData6DI4 
 module procedure ESMF_FieldCreateMeshData6DI8 
 module procedure ESMF_FieldCreateMeshData6DR4 
 module procedure ESMF_FieldCreateMeshData6DR8 
 module procedure ESMF_FieldCreateMeshData7DI4 
 module procedure ESMF_FieldCreateMeshData7DI8 
 module procedure ESMF_FieldCreateMeshData7DR4 
 module procedure ESMF_FieldCreateMeshData7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldCreateMeshDataPtr1DI1 
 module procedure ESMF_FieldCreateMeshDataPtr2DI1 
 module procedure ESMF_FieldCreateMeshDataPtr3DI1 
 module procedure ESMF_FieldCreateMeshDataPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshDataPtr5DI1 
 module procedure ESMF_FieldCreateMeshDataPtr6DI1 
 module procedure ESMF_FieldCreateMeshDataPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldCreateMeshDataPtr1DI2 
 module procedure ESMF_FieldCreateMeshDataPtr2DI2 
 module procedure ESMF_FieldCreateMeshDataPtr3DI2 
 module procedure ESMF_FieldCreateMeshDataPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshDataPtr5DI2 
 module procedure ESMF_FieldCreateMeshDataPtr6DI2 
 module procedure ESMF_FieldCreateMeshDataPtr7DI2 
#endif 
#endif 
 module procedure ESMF_FieldCreateMeshDataPtr1DI4 
 module procedure ESMF_FieldCreateMeshDataPtr1DI8 
 module procedure ESMF_FieldCreateMeshDataPtr1DR4 
 module procedure ESMF_FieldCreateMeshDataPtr1DR8 
 module procedure ESMF_FieldCreateMeshDataPtr2DI4 
 module procedure ESMF_FieldCreateMeshDataPtr2DI8 
 module procedure ESMF_FieldCreateMeshDataPtr2DR4 
 module procedure ESMF_FieldCreateMeshDataPtr2DR8 
 module procedure ESMF_FieldCreateMeshDataPtr3DI4 
 module procedure ESMF_FieldCreateMeshDataPtr3DI8 
 module procedure ESMF_FieldCreateMeshDataPtr3DR4 
 module procedure ESMF_FieldCreateMeshDataPtr3DR8 
 module procedure ESMF_FieldCreateMeshDataPtr4DI4 
 module procedure ESMF_FieldCreateMeshDataPtr4DI8 
 module procedure ESMF_FieldCreateMeshDataPtr4DR4 
 module procedure ESMF_FieldCreateMeshDataPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldCreateMeshDataPtr5DI4 
 module procedure ESMF_FieldCreateMeshDataPtr5DI8 
 module procedure ESMF_FieldCreateMeshDataPtr5DR4 
 module procedure ESMF_FieldCreateMeshDataPtr5DR8 
 module procedure ESMF_FieldCreateMeshDataPtr6DI4 
 module procedure ESMF_FieldCreateMeshDataPtr6DI8 
 module procedure ESMF_FieldCreateMeshDataPtr6DR4 
 module procedure ESMF_FieldCreateMeshDataPtr6DR8 
 module procedure ESMF_FieldCreateMeshDataPtr7DI4 
 module procedure ESMF_FieldCreateMeshDataPtr7DI8 
 module procedure ESMF_FieldCreateMeshDataPtr7DR4 
 module procedure ESMF_FieldCreateMeshDataPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides an entry point for methods that create a complete
! {\tt ESMF\_Field}. These method all contain an {\tt ESMF\_Grid} and
! {\tt ESMF\_Data}. The variations allow the user to specify the data
! using either a Fortran array or an {\tt ESMF\_Array}.
  end interface
!EOPI
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! ESMF_FieldCreateBa
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateEmpty"
!BOP
! !IROUTINE: ESMF_FieldCreateEmpty - Create an empty Field (no Grid)

! !INTERFACE:
  function ESMF_FieldCreateEmpty(name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateEmpty
!
! !ARGUMENTS:
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field}
! and depends on later calls to add an {\tt ESMF\_Grid} and {\tt ESMF\_Array} to
! it. Attributes can be added to an empty Field object. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_empty_setcommit}.
!
!
! The arguments are:
! \begin{description}
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    nullify(ftype)
    nullify(ESMF_FieldCreateEmpty%ftypep)

    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                     ESMF_CONTEXT, rc)) return
    call ESMF_FieldInitialize(ftype, rc=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Default initialize Field", &
                                     ESMF_CONTEXT, rc)) return

    ! Call field construction method
    call ESMF_FieldConstructEmpty(ftype, name, iospec, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Set return values.
    ESMF_FieldCreateEmpty%ftypep => ftype

    ! Add reference to this object into ESMF garbage collection table
    ! Only call this in those Create() methods that call Construct()
    call c_ESMC_VMAddFObject(ESMF_FieldCreateEmpty, ESMF_ID_FIELD%objectID)

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateEmpty)

    call ESMF_FieldValidate(ESMF_FieldCreateEmpty, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_FieldCreateEmpty
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestroy"
!BOP
! !IROUTINE: ESMF_FieldDestroy - Free all resources associated with a Field
! !INTERFACE:
  subroutine ESMF_FieldDestroy(field, rc)
!
! !ARGUMENTS:
    type(ESMF_Field) :: field
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Releases all resources associated with the {\tt ESMF\_Field}.
!
! The arguments are:
! \begin{description}
! \item [field]
! {\tt ESMF\_Field} object.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

    if (.not.associated(field%ftypep)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Uninitialized or already destroyed Field: ftypep unassociated", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Destruct all field internals and then free field memory.
    call ESMF_FieldDestruct(field%ftypep, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! mark object invalid
    call ESMF_BaseSetStatus(field%ftypep%base, ESMF_STATUS_INVALID, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ESMF_INIT_SET_DELETED(field)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANew"
!BOPI
! !IROUTINE: ESMF_FieldConstructIANew - Construct the internals of a Field

! !INTERFACE:
  subroutine ESMF_FieldConstructIANew(ftype, geombase, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype
    type(ESMF_GeomBase) :: geombase
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Constructs all {\tt ESMF\_Field} internals, including the allocation
! of a data {\tt ESMF\_Array}. TODO: this is missing a counts argument,
! which is required if the arrayspec rank is greater than the {\tt grid} rank.
! Either that, or we must enforce that a datamap comes in, and it
! contains the counts for non-grid dims.
!
! The arguments are:
! \begin{description}
! \item [ftype]
! Pointer to an {\tt ESMF\_Field} object.
! \item [geombase]
! {\tt ESMF\_GeomBase} object.
! \item [arrayspec]
! Data specification.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_Array) :: array
    integer :: i, arrayRank, gridDimCount, gridDimCount_norep, grid_repdimcount
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_IndexFlag) :: indexflag
    integer, pointer :: distgridToArrayMap(:)
    integer, pointer :: arrayLBound(:),arrayUBound(:)
    integer :: ungriddedDimCount
    integer :: distgridDimCount, distgridDimCount_norep

    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_BaseCreate(ftype%base, "Field", name, 0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


    call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


    call ESMF_GeomBaseGet(geombase, distgrid=distgrid, dimCount=gridDimCount, &
                      indexflag=indexflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    grid_repdimcount = 0
    if (present(gridToFieldMap)) then
       do i = 1, size(gridToFieldMap)
           if(gridToFieldMap(i) == 0) grid_repdimcount = grid_repdimcount + 1
       enddo
    endif
    gridDimCount_norep = gridDimCount - grid_repdimcount

    ! Get the ungridded dimCount
    ungriddedDimCount=0
    if (present(ungriddedUBound)) then
       ungriddedDimCount=size(ungriddedUBound)
    endif

    call ESMF_DistGridGet(distgrid, dimCount=distgridDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    ! allocate distgridToArrayMap
    allocate(distgridToArrayMap(distgridDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating distgridToArrayMap", &
                                     ESMF_CONTEXT, rc)) return

    ! allocate undistributed Bounds
    allocate(arrayLBound(ungriddedDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridLBound", &
                                     ESMF_CONTEXT, rc)) return
    allocate(arrayUBound(ungriddedDimCount) , stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating gridUBound", &
                                     ESMF_CONTEXT, rc)) return

    ! Get dimmap and undistibuted bounds
    call ESMF_GeomBaseGetArrayInfo(geombase, &
                            gridToFieldMap=gridToFieldMap, &
                            ungriddedLBound=ungriddedLBound, &
                            ungriddedUBound=ungriddedUBound, &
                            distgridToArrayMap=distgridToArrayMap, &
                            undistLBound=arrayLBound, &
                            undistUBound=arrayUBound, &
                            rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! create Array
    ! For arbitrary grid: distgridDim is different from gridDimCount and
    ! the dimension of the computationalEdgeLWidth, computationslEdgeUWidth
    ! are the dimension of the array,
    distgridDimCount_norep = arrayRank - ungriddedDimCount
    array=ESMF_ArrayCreate(arrayspec=arrayspec, &
              distgrid=distgrid, &
              distgridToArrayMap=distgridToArrayMap, &
              totalLWidth=maxHaloLWidth, totalUWidth=maxHaloUWidth, &
              indexflag=indexflag, &
              undistLBound=arrayLBound, undistUBound=arrayUBound, &
              name=name, &
              rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    ! Default of gridToFieldMap should be {1,2,3...}
    if (.not. present(gridToFieldMap)) then
        do i = 1, ESMF_MAXDIM
          ftype%gridToFieldMap(i) = i
        enddo
    else
       ftype%gridToFieldMap(1:size(gridToFieldMap)) = gridToFieldMap
    endif

    if(present(ungriddedLBound)) &
       ftype%ungriddedLBound(1:size(ungriddedLBound)) = ungriddedLBound
    if(present(ungriddedUBound)) &
       ftype%ungriddedUBound(1:size(ungriddedUBound)) = ungriddedUBound
    if(present(maxHaloLWidth)) &
       ftype%maxHaloLWidth(1:size(maxHaloLWidth)) = maxHaloLWidth
    if(present(maxHaloUWidth)) &
       ftype%maxHaloUWidth(1:size(maxHaloUWidth)) = maxHaloUWidth

    ftype%array = array
    ftype%array_internal = .true.
    ftype%datastatus = ESMF_STATUS_READY
    ftype%geombase = geombase
    ftype%gridstatus = ESMF_STATUS_READY
    call ESMF_BaseSetStatus(ftype%base, ESMF_STATUS_READY, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    ftype%dimCount = gridDimCount_norep + ungriddedDimCount

    ! cleanup
    deallocate(distgridToArrayMap)
    deallocate(arrayLBound)
    deallocate(arrayUBound)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldConstructIANew
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANewArray"
!BOPI
! !IROUTINE: ESMF_FieldConstructIANewArray - Construct the internals of a Field

! !INTERFACE:
  subroutine ESMF_FieldConstructIANewArray(ftype, geombase, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype
    type(ESMF_GeomBase) :: geombase
    type(ESMF_Array), intent(in) :: array
    type(ESMF_CopyFlag), intent(in) :: copyflag
    integer, intent(in) :: gridToFieldMap(:)
    integer, intent(in),optional :: ungriddedLBound(:)
    integer, intent(in),optional :: ungriddedUBound(:)
    integer, intent(in),optional :: maxHaloLWidth(:)
    integer, intent(in),optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Constructs all {\tt ESMF\_Field} internals, including the allocation
! of a data {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item [ftype]
! Pointer to an {\tt ESMF\_Field} object.
! \item [geombase]
! {\tt ESMF\_GeomBase} object.
! \item [array]
! Data.
! \item [copyflag]
! Whether to copy the existing data space or reference directly. Valid
! values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF} (default).
! \item [staggerloc]
! Stagger location of data in grid cells. For valid
! predefined values see Section \ref{sec:opt:staggerloc}.
! To create a custom stagger location see Section
! \ref{sec:usage:staggerloc:adv}. The default
! value is {\tt ESMF\_STAGGERLOC\_CENTER}.
! \item [gridToFieldMap]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of dimensions in the {\tt grid}. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of dimensions in the {\tt grid}. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_Array) :: newarray
    integer :: ungriddedDimCount, gridDimCount, gridDimCount_norep
    integer :: grid_repdimcount, i

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Construct a default name if one is not given
    call ESMF_BaseCreate(ftype%base, "Field", name, 0, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! make sure the array is a valid object first.
    call ESMF_ArrayValidate(array, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    call ESMF_GeomBaseGet(geombase, dimCount=gridDimCount, &
                      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rc)) return

    grid_repdimcount = 0
    do i = 1, size(gridToFieldMap)
        if(gridToFieldMap(i) == 0) grid_repdimcount = grid_repdimcount + 1
    enddo
    gridDimCount_norep = gridDimCount - grid_repdimcount

    ! Get the ungridded dimCount
    ungriddedDimCount = 0
    if(present(ungriddedUBound)) then
        ungriddedDimCount=size(ungriddedUBound)
    endif

    if(present(ungriddedLBound)) then
        ftype%ungriddedLBound(1:size(ungriddedLBound)) = ungriddedLBound
    else
        ftype%ungriddedLBound = -1
    endif
    if(present(ungriddedUBound)) then
        ftype%ungriddedUBound(1:size(ungriddedUBound)) = ungriddedUBound
    else
        ftype%ungriddedUBound = -1
    endif

    if(present(maxHaloLWidth)) then
        ftype%maxHaloLWidth(1:gridDimCount_norep) = &
         maxHaloLWidth (1:gridDimCount_norep)
    else
         ftype%MaxHaloLWidth = 0
    endif

    if(present(maxHaloUWidth)) then
        ftype%maxHaloUWidth(1:gridDimCount_norep) = &
         maxHaloUWidth (1:gridDimCount_norep)
    else
         ftype%maxHaloUWidth = 0
    endif
    ftype%gridToFieldMap(1:size(gridToFieldMap)) = gridToFieldMap

    ! default copyflag value is ESMF_DATA_REF
    ftype%array_internal = .false.
    if(copyflag == ESMF_DATA_REF) then
        ftype%array = array
    else
        newarray = ESMF_ArrayCreate(array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
        ftype%array = newarray
        ftype%array_internal = .true.
    endif

    ftype%datastatus = ESMF_STATUS_READY
    ftype%geombase = geombase
    ftype%gridstatus = ESMF_STATUS_READY
    call ESMF_BaseSetStatus(ftype%base, ESMF_STATUS_READY, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    ftype%dimCount = gridDimCount_norep + ungriddedDimCount

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldConstructIANewArray
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructEmpty"
!BOPI
! !IROUTINE: ESMF_FieldConstructEmpty - Construct a Field with no GeomBase or Array
!
! !INTERFACE:
  subroutine ESMF_FieldConstructEmpty(ftypep, name, iospec, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftypep
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Constructs an empty {\tt ESMF\_Field}.
!
! The arguments are:
! \begin{description}
! \item [ftypep]
! Pointer to an {\tt ESMF\_Field} object.
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! {\tt ESMF\_Field} I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Construct a default name if one is not given
    call ESMF_BaseCreate(ftypep%base, "Field", name, 0, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! Initialize field contents
    ftypep%gridstatus = ESMF_STATUS_UNINIT
    ftypep%datastatus = ESMF_STATUS_UNINIT

    call ESMF_BaseSetStatus(ftypep%base, ESMF_STATUS_READY, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldConstructEmpty
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestruct"
!BOPI
! !IROUTINE: ESMF_FieldDestruct - Free any Field memory allocated internally
!
! !INTERFACE:
  subroutine ESMF_FieldDestruct(ftype, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldType), pointer :: ftype
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Releases all resources except the {\tt ESMF\_Field} itself.
!
! The arguments are:
! \begin{description}
! \item [ftype]
! Pointer to an {\tt ESMF\_Field} object.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    type(ESMF_Status) :: fieldstatus

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_BaseGetStatus(ftype%base, fieldstatus, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if (fieldstatus .eq. ESMF_STATUS_READY) then

      if((ftype%is_proxy .or. ftype%array_internal) .and. &
        ftype%datastatus .eq. ESMF_STATUS_READY) then
          call ESMF_ArrayDestroy(ftype%array, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

          ftype%datastatus = ESMF_STATUS_INVALID ! mark invalid

          ! mark invalid
          call ESMF_BaseSetStatus(ftype%base, ESMF_STATUS_INVALID, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

    endif

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldDestruct
!------------------------------------------------------------------------------

! ESMF_FieldCreateGe
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateGBArraySpec"
!BOPI
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and ArraySpec

! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateGBArraySpec(geombase, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateGBArraySpec
!
! !ARGUMENTS:
    type(ESMF_GeomBase) :: geombase
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field} and allocate space internally for an
! {\tt ESMF\_Array}. Return a new {\tt ESMF\_Field}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_arrayspec}.
!
! The arguments are:
! \begin{description}
! \item [geombase]
! {\tt ESMF\_GeomBase} object.
! \item [arrayspec]
! Data type and kind specification.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
    integer :: localrc ! Local error code
    logical :: rcpresent ! Return code present
    integer :: memDimCount, fieldDimCount, fieldUngriddedDimCount
    integer :: gridDimCount, gridDimCount_norep, grid_repdimcount
    integer :: fieldUndistDimCount
    integer :: i,j
    integer :: localGridToFieldMap(ESMF_MAXDIM)
    integer :: localMaxHaloLWidth (ESMF_MAXDIM)
    integer :: localMaxHaloUWidth (ESMF_MAXDIM)
    integer :: distgridToGridMap(ESMF_MAXDIM)
    logical :: flipflop(ESMF_MAXDIM)
    logical :: found
    type(ESMF_GridDecompType) :: decompType
    type(ESMF_GeomType) :: geomType
    type(ESMF_Grid) :: grid
    integer :: arbdim


    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    nullify(ftype)
    nullify(ESMF_FieldCreateGBArraySpec%ftypep)

    ! Initialize return code
    if(present(rc)) then
      rcpresent=.TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif

    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_GeomBaseGetInit,geombase,rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit, arrayspec)

    ! Check if geombase is a grid, if so, check if it is arbitrary
    decompType = ESMF_GRID_NONARBITRARY
    call ESMF_GeomBaseGet(geombase, geomType=geomType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if (geomType .eq. ESMF_GEOMTYPE_GRID) then
       call ESMF_GeomBaseGet(geombase, grid=grid, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rc)) return
       call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rc)) return
    endif

    ! Get rank of proposed Field.
    call ESMF_ArraySpecGet(arrayspec, rank=memDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

    ! Get number of grid dimensions, number
    ! of distributed grid dimensions, distgrid,
    ! number of ungridded Field dimensions,
    ! and number of undistributed Field Dimensions
    call ESMF_GeomBaseGet(geombase, dimCount=gridDimCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    ! Error Check Input
    if (present(gridToFieldMap)) then
        if (size(gridToFieldMap) .ne. gridDimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- gridToFieldMap size must equal to grid_rank", &
                ESMF_CONTEXT, rc)
            return
        endif
    endif

    grid_repdimcount = 0
    localGridToFieldMap = -1
    if (present(gridToFieldMap)) then
       localGridToFieldMap(1:gridDimCount) = &
         gridToFieldMap (1:gridDimCount)
       do i = 1, size(gridToFieldMap)
           if(gridToFieldMap(i) == 0) grid_repdimcount = grid_repdimcount + 1
       enddo
    else
      do i = 1, gridDimCount
        localGridToFieldMap(i) = i
      enddo
    endif
    gridDimCount_norep = gridDimCount - grid_repdimcount

    if (decompType .eq. ESMF_GRID_NONARBITRARY) then
     fieldDimCount = memDimCount
    else
        call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, &
     rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        ! find out how many grid dimensions are arbitrarily distributed and calculate
        ! fieldDimCount accordingly
        arbdim = 0
        do i=1,gridDimCount
          if (distgridToGridMap(i) .ne. 0) arbdim = arbdim+1
        enddo
        fieldDimCount = memDimCount + arbdim - 1

        ! If there is any replicated dimension, check if any of the arb. dimensions are replicated.
        ! If one arb dimension is replicated, all the arb. dimensions have to be replicated
        if (grid_repdimcount .ne. 0) then
          do i = 1,gridDimCount
            if(localGridToFieldMap(i) == 0) then
                found = .false.
                do j=1,arbdim
                    if (distgridToGridMap(j) .eq. i) found = .true.
                enddo
                if (found) then
                  ! one arb.dimension is a replicated dimension, check if other arb dimensions are
                  ! also replicated
                  do j=1,arbdim
                    if (distgridToGridMap(j) .ne. i) then
                        if (localGridToFieldMap(distgridToGridMap(j)) .ne. 0) then
                            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                                "- Arb. grid dimensions have to be either all replicated or not replicated", &
                                ESMF_CONTEXT, rc)
                            return
                        endif
                    endif
                  enddo
                  ! all arb. dimension are replication, jump out of the first do loop
                  ! fieldDimCount should be the same as the memDimCount
                  fieldDimCount = memDimCount
                  exit
                endif
             endif
           enddo
        endif
    endif

    if(fieldDimCount .lt. gridDimCount_norep) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
             "- Field rank must be greater than or equal to its gridded rank", &
               ESMF_CONTEXT, rc)
        return
    endif

    if(fieldDimCount .gt. gridDimCount_norep) then
      if( (.not. present(ungriddedLBound)) .or. &
          (.not. present(ungriddedUBound)) ) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
         "- ungridded bounds must be present when Field has ungridded dimension(s)", &
           ESMF_CONTEXT, rc)
        return
      endif
    endif

    fieldUngriddedDimCount = fieldDimCount-gridDimCount + grid_repdimcount
    fieldUndistDimCount = fieldDimCount-gridDimCount + grid_repdimcount

    if (present(ungriddedLBound)) then
       if (size(ungriddedLBound) .ne. fieldUngriddedDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
               "- ungriddedLBound size must equal to array_rank-grid_rank", &
               ESMF_CONTEXT, rc)
          return
       endif
    endif

    if (present(ungriddedUBound)) then
       if (size(ungriddedUBound) .ne. fieldUngriddedDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                 "- ungriddedUBound size must equal to array_rank-grid_rank", &
                 ESMF_CONTEXT, rc)
          return
       endif
    endif

    if (present(maxHaloLWidth)) then
        if (size(maxHaloLWidth) .ne. gridDimCount_norep) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- maxHaloLWidth must equal to grid dimCount", &
                ESMF_CONTEXT, rc)
            return
        endif
    endif

    if (present(maxHaloUWidth)) then
       if (size(maxHaloUWidth) .ne. gridDimCount_norep) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- maxHaloUWidth must equal to grid dimCount", &
                ESMF_CONTEXT, rc)
           return
       endif
    endif

    ! gridToFieldMap elements must be in range 1...fieldRank and unique
    ! algorithm to check element uniqueness:
    ! run time: O(ESMF_MAXDIM)
    ! memory: O(2*ESMF_MAXDIM)
    ! or O(ESMF_MAXDIM+ESMF_MAXDIM/sizeof(integer)) with bitvector
    flipflop = .false.
    do i = 1, gridDimCount
       if(localGridToFieldMap(i) .lt. 0 .and. &
         localGridToFieldMap(i) .gt. fieldDimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                 "- gridToFieldMap element must be within range 0...array rank", &
                   ESMF_CONTEXT, rc)
           return
       endif
       if(localGridToFieldMap(i) /= 0) then
           if(flipflop(localGridToFieldMap(i))) then
               call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                     "- gridToFieldMap element must be unique", &
                       ESMF_CONTEXT, rc)
               return
           endif
           flipflop(localGridToFieldMap(i)) = .true.
       endif
    enddo

     if(present(maxHaloLWidth)) then
       localMaxHaloLWidth(1:gridDimCount_norep) = &
          maxHaloLWidth (1:gridDimCount_norep)
     else
          localMaxHaloLWidth = 0
     endif

     if(present(maxHaloUWidth)) then
       localMaxHaloUWidth(1:gridDimCount_norep) = &
          maxHaloUWidth (1:gridDimCount_norep)
     else
          localMaxHaloUWidth = 0
     endif

    ! allocate Fieldtype
    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                "Allocating Field Type", &
                                ESMF_CONTEXT, rc)) return

    call ESMF_FieldInitialize(ftype, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

    ! Call construction method to allocate and initialize field internals.
    if (decompType .eq. ESMF_GRID_NONARBITRARY) then
       call ESMF_FieldConstructIANew(ftype, geombase, arrayspec, &
                                localGridToFieldMap(1:gridDimCount), ungriddedLBound, &
                                ungriddedUBound, localMaxHaloLWidth(1:gridDimCount_norep), &
                                localMaxHaloUWidth(1:gridDimCount_norep), name, &
                                iospec, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    else
          call ESMF_FieldConstructIANew(ftype, geombase, arrayspec, &
                                localGridToFieldMap(1:gridDimCount), ungriddedLBound, &
                                ungriddedUBound, name=name, &
                                iospec=iospec, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
    endif

    ! Set return values.
    ESMF_FieldCreateGBArraySpec%ftypep => ftype

    ! Add reference to this object into ESMF garbage collection table
    ! Only call this in those Create() methods that call Construct()
    call c_ESMC_VMAddFObject(ESMF_FieldCreateGBArraySpec, &
      ESMF_ID_FIELD%objectID)

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateGBArraySpec)

    call ESMF_FieldValidate(ESMF_FieldCreateGBArraySpec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
      call ESMF_BaseSetStatus(ftype%base, ESMF_STATUS_INVALID, rc=localrc)
      return
    endif

    if(rcpresent) rc = ESMF_SUCCESS

  end function ESMF_FieldCreateGBArraySpec
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateGBArray"
!BOPI
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and Array

! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateGBArray(geombase, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, maxHaloLWidth, &
    maxHaloUWidth, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateGBArray
!
! !ARGUMENTS:
    type(ESMF_GeomBase), intent(in) :: geombase
    type(ESMF_Array), intent(in) :: array
    type(ESMF_CopyFlag), intent(in), optional :: copyflag
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field}. This version of creation
! assumes the data exists already and is being
! passed in through an {\tt ESMF\_Array}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_array}.
!
! The arguments are:
! \begin{description}
! \item [geombase]
! {\tt ESMF\_GeomBase} object.
! \item [array]
! {\tt ESMF\_Array} object.
! \item [{[copyflag]}]
! Indicates whether to copy the contents of the {\tt array} or reference it directly.
! For valid values see \ref{opt:copyflag}. The default is
! {\tt ESMF\_DATA\_REF}.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
    integer :: localrc ! Local error code
    logical :: rcpresent ! Return code present

    integer :: memDimCount, fieldDimCount, fieldUngriddedDimCount
    integer :: gridDimCount, gridDimCount_norep, grid_repdimcount
    integer :: fieldUndistDimCount
    integer :: i,j
    integer :: localGridToFieldMap(ESMF_MAXDIM)
    logical :: flipflop(ESMF_MAXDIM)
    type(ESMF_CopyFlag) :: l_copyflag
    type (ESMF_IndexFlag) :: arrayIndexFlag, gridIndexFlag
    integer :: distgridToGridMap(ESMF_MAXDIM)
    logical :: found
    type(ESMF_GridDecompType) :: decompType
    type(ESMF_GeomType) :: geomType
    type(ESMF_Grid) :: grid
    integer :: arbdim

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    nullify(ftype)
    nullify(ESMF_FieldCreateGBArray%ftypep)

    ! Initialize return code
    if(present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif

    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_GeomBaseGetInit,geombase,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

    ! Check if geombase is a grid, if so, check if it is arbitrary
    decompType = ESMF_GRID_NONARBITRARY
    call ESMF_GeomBaseGet(geombase, geomType=geomType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if (geomType .eq. ESMF_GEOMTYPE_GRID) then
       call ESMF_GeomBaseGet(geombase, grid=grid, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rc)) return
       call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rc)) return
    endif

    ! Get rank of proposed Field.
    call ESMF_ArrayGet(array, rank=memDimCount, &
                       indexflag=arrayIndexFlag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

    ! Get number of grid dimensions, number
    ! of distributed grid dimensions, distgrid,
    ! number of ungridded Field dimensions,
    ! and number of undistributed Field Dimensions
    call ESMF_GeomBaseGet(geombase, dimCount=gridDimCount, &
                      indexflag=gridIndexFlag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    ! Error Check Input
    if (present(gridToFieldMap)) then
        if (size(gridToFieldMap) .ne. gridDimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- gridToFieldMap size must equal to grid_rank", &
                ESMF_CONTEXT, rc)
            return
        endif
    endif

    grid_repdimcount = 0
    localGridToFieldMap = -1
    if (present(gridToFieldMap)) then
       localGridToFieldMap(1:gridDimCount) = &
         gridToFieldMap (1:gridDimCount)
       do i = 1, size(gridToFieldMap)
           if(gridToFieldMap(i) == 0) grid_repdimcount = grid_repdimcount + 1
       enddo
    else
      do i = 1, gridDimCount
        localGridToFieldMap(i) = i
      enddo
    endif
    gridDimCount_norep = gridDimCount - grid_repdimcount

    if (decompType .eq. ESMF_GRID_NONARBITRARY) then
     fieldDimCount = memDimCount
    else
        call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, &
     rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        ! find out how many grid dimensions are arbitrarily distributed and calculate
        ! fieldDimCount accordingly
        arbdim = 0
        do i=1,gridDimCount
          if (distgridToGridMap(i) .ne. 0) arbdim = arbdim+1
        enddo
        fieldDimCount = memDimCount + arbdim - 1

        ! If there is any replicated dimension, check if any of the arb. dimensions are replicated.
        ! If one arb dimension is replicated, all the arb. dimensions have to be replicated
        if (grid_repdimcount .ne. 0) then
          do i = 1,gridDimCount
            if(localGridToFieldMap(i) == 0) then
                found = .false.
                do j=1,arbdim
                    if (distgridToGridMap(j) .eq. i) found = .true.
                enddo
                if (found) then
                  ! one arb.dimension is a replicated dimension, check if other arb dimensions are
                  ! also replicated
                  do j=1,arbdim
                    if (distgridToGridMap(j) .ne. i) then
                        if (localGridToFieldMap(distgridToGridMap(j)) .ne. 0) then
                            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                                "- Arb. grid dimensions have to be either all replicated or not replicated", &
                                ESMF_CONTEXT, rc)
                            return
                        endif
                    endif
                  enddo
                  ! all arb. dimension are replication, jump out of the first do loop
                  ! fieldDimCount should be the same as the memDimCount
                  fieldDimCount = memDimCount
                  exit
                endif
             endif
           enddo
        endif
    endif

    if(fieldDimCount .lt. gridDimCount_norep) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
             "- Field rank must be greater than or equal to its gridded rank", &
               ESMF_CONTEXT, rc)
        return
    endif

    if(fieldDimCount .gt. gridDimCount_norep) then
      if( (.not. present(ungriddedLBound)) .or. &
          (.not. present(ungriddedUBound)) ) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
         "- ungridded bounds must be present when Field has ungridded dimension(s)", &
           ESMF_CONTEXT, rc)
        return
      endif
    endif

    fieldUngriddedDimCount = fieldDimCount-gridDimCount + grid_repdimcount
    fieldUndistDimCount = fieldDimCount-gridDimCount + grid_repdimcount

    ! Error Check Input
    if (present(gridToFieldMap)) then
        if (size(gridToFieldMap) .ne. gridDimCount) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- gridToFieldMap size must equal to grid_rank", &
                ESMF_CONTEXT, rc)
            return
        endif
    endif

    if (present(ungriddedLBound)) then
       if (size(ungriddedLBound) .ne. fieldUngriddedDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
               "- ungriddedLBound size must equal to array_rank-grid_rank", &
               ESMF_CONTEXT, rc)
          return
       endif
    endif

    if (present(ungriddedUBound)) then
       if (size(ungriddedUBound) .ne. fieldUngriddedDimCount) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                 "- ungriddedUBound size must equal to array_rank-grid_rank", &
                 ESMF_CONTEXT, rc)
          return
       endif
    endif

    if (present(maxHaloLWidth)) then
        if (size(maxHaloLWidth) .ne. gridDimCount_norep) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- maxHaloLWidth must equal to grid dimCount", &
                ESMF_CONTEXT, rc)
            return
        endif
    endif

    if (present(maxHaloUWidth)) then
       if (size(maxHaloUWidth) .ne. gridDimCount_norep) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
              "- maxHaloUWidth must equal to grid dimCount", &
                ESMF_CONTEXT, rc)
           return
       endif
    endif

    if (.not.(arrayIndexFlag .eq. gridIndexFlag)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
         "- Array indexflag must be the same as the Grid indexflag", &
           ESMF_CONTEXT, rc)
        return
    endif

    if (present(copyflag)) then
        l_copyflag = copyflag
    else
        l_copyflag = ESMF_DATA_REF
    endif

    ! gridToFieldMap elements must be in range 1...fieldRank and unique
    ! algorithm to check element uniqueness:
    ! run time: O(ESMF_MAXDIM)
    ! memory: O(2*ESMF_MAXDIM)
    ! or O(ESMF_MAXDIM+ESMF_MAXDIM/sizeof(integer)) with bitvector
    flipflop = .false.
    do i = 1, gridDimCount
       if(localGridToFieldMap(i) .lt. 0 .and. &
         localGridToFieldMap(i) .gt. fieldDimCount) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                 "- gridToFieldMap element must be within range 0...array rank", &
                   ESMF_CONTEXT, rc)
           return
       endif
       if(localGridToFieldMap(i) /= 0) then
           if(flipflop(localGridToFieldMap(i))) then
               call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                     "- gridToFieldMap element must be unique", &
                       ESMF_CONTEXT, rc)
               return
           endif
           flipflop(localGridToFieldMap(i)) = .true.
       endif
    enddo

    allocate(ftype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                     ESMF_CONTEXT, rc)) return
    call ESMF_FieldInitialize(ftype, rc=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Default initialize Field", &
                                     ESMF_CONTEXT, rc)) return

    if (decompType .eq. ESMF_GRID_NONARBITRARY) then
        ! Call construction method to allocate and initialize field internals.
        call ESMF_FieldConstructIANewArray(ftype, geombase, array, l_copyflag, &
                                         localgridToFieldMap, &
                                         ungriddedLBound=ungriddedLBound, &
                                         ungriddedUBound=ungriddedUBound, &
                                         maxHaloLWidth=maxHaloLWidth, &
                                         maxHaloUWidth=maxHaloUWidth, &
                                         name=name, iospec=iospec, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
    else
            ! Call construction method to allocate and initialize field internals.
        call ESMF_FieldConstructIANewArray(ftype, geombase, array, l_copyflag, &
                                         localgridToFieldMap, &
                                         ungriddedLBound=ungriddedLBound, &
                                         ungriddedUBound=ungriddedUBound, &
                                         name=name, iospec=iospec, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
    endif

    ! Set return values.
    ESMF_FieldCreateGBArray%ftypep => ftype

    ! Add reference to this object into ESMF garbage collection table
    ! Only call this in those Create() methods that call Construct()
    call c_ESMC_VMAddFObject(ESMF_FieldCreateGBArray, ESMF_ID_FIELD%objectID)

    ESMF_INIT_SET_CREATED(ESMF_FieldCreateGBArray)

    call ESMF_FieldValidate(ESMF_FieldCreateGBArray, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
      call ESMF_BaseSetStatus(ftype%base, ESMF_STATUS_INVALID, rc=localrc)
      return
    endif

    if(rcpresent) rc = ESMF_SUCCESS

  end function ESMF_FieldCreateGBArray
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOPI 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Fortran array 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateGBData<rank><type><kind>(geombase, & 
! farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
! ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateGBData<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_GeomBase) :: geombase 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray 
! type(ESMF_IndexFlag), intent(in) :: indexflag 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: ungriddedLBound(:) 
! integer, intent(in), optional :: ungriddedUBound(:) 
! integer, intent(in), optional :: maxHaloLWidth(:) 
! integer, intent(in), optional :: maxHaloUWidth(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data array and {\tt ESMF\_GeomBase}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried but deallocating 
! the retrieved data pointer is not allowed. 
! The arguments are: 
! \begin{description} 
! \item [geombase] 
! {\tt ESMF\_GeomBase} object. The dimCount of the 
! GeomBase must be smaller than or equal to the rank of the {\tt farray}. 
! \item [farray] 
! Native fortran data array to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farray}. 
! \item [indexflag] 
! Indicate how DE-local indices are defined. See section 
! \ref{opt:indexflag} for a list of valid indexflag options. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farray} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt grid}'s dimCount. The list elements map each dimension 
! of the {\tt grid} to a dimension in the {\tt farray} by 
! specifying the appropriate {\tt farray} dimension index. The default is to 
! map all of the {\tt grid}'s dimensions against the lowest dimensions of 
! the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farray} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farray} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt grid}. Ungridded dimensions must be in the same order they are 
! stored in the {\t farray}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! \item [{[ungriddedLBound]}] 
! Lower bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[ungriddedUBound]}] 
! Upper bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[maxHaloLWidth]}] 
! Lower bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farray}. Values default 
! to 0. If values for maxHaloLWidth are specified they must be reflected in 
! the size of the {\tt farray}. That is, for each gridded dimension the 
! {\tt farray} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt minHaloLWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[maxHaloUWidth]}] 
! Upper bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farray}. Values default 
! to 0. If values for maxHaloUWidth are specified they must be reflected in 
! the size of the {\tt farray}. That is, for each gridded dimension the 
! {\tt farray} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Di1(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Di1, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Di2(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Di2, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData1Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData1Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData1Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData1Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData1Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData2Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData2Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData2Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData2Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData2Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData3Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData3Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData3Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData3Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData3Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData4Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData4Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData4Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData4Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData4Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData5Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData5Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData5Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData5Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData5Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData6Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData6Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData6Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData6Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData6Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Di4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Di4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Di8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Di8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Dr4(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Dr4, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBData" 
 function ESMF_FieldCreateGBData7Dr8(geombase, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBData7Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBData7Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBData7Dr8, & 
 geombase, farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBData7Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBData7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
#define FieldCreateGBDataPtrDoc() !---------------------------------------------------------------------------- 
!BOPI 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Fortran array pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateGBDataPtr<rank><type><kind>(geombase, & 
! farrayPtr, copyflag, gridToFieldMap, & 
! maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_GeomBase) :: geombase 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farrayPtr 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: maxHaloLWidth(:) 
! integer, intent(in), optional :: maxHaloUWidth(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data pointer and {\tt ESMF\_GeomBase}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried and deallocated when 
! copyflag is {\tt ESMF\_DATA\_REF}. Note that the {\tt ESMF\_FieldDestroy} call does not 
! deallocate the fortran data pointer in this case. This gives user more flexibility over memory management. 
! 
! 
! The arguments are: 
! \begin{description} 
! \item [geombase] 
! {\tt ESMF\_GeomBase} object. The dimCount of the 
! GeomBase must be smaller than or equal to the rank of the {\tt farrayPtr}. 
! \item [farrayPtr] 
! Native fortran data pointer to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farrayPtr}. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farrayPtr} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt grid}'s dimCount. The list elements map each dimension 
! of the {\tt grid} to a dimension in the {\tt farrayPtr} by 
! specifying the appropriate {\tt farrayPtr} dimension index. The default is to 
! map all of the {\tt grid}'s dimensions against the lowest dimensions of 
! the {\tt farrayPtr} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farrayPtr} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farrayPtr} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt grid}. Ungridded dimensions must be in the same order they are 
! stored in the {\t farrayPtr}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! \item [{[maxHaloLWidth]}] 
! Lower bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farrayPtr}. Values default 
! to 0. If values for maxHaloLWidth are specified they must be reflected in 
! the size of the {\tt farrayPtr}. That is, for each gridded dimension the 
! {\tt farrayPtr} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt minHaloLWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[maxHaloUWidth]}] 
! Upper bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farrayPtr}. Values default 
! to 0. If values for maxHaloUWidth are specified they must be reflected in 
! the size of the {\tt farrayPtr}. That is, for each gridded dimension the 
! {\tt farrayPtr} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
!---------------------------------------------------------------------------- 

!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
FieldCreateGBDataPtrDoc() 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Di1(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Di1 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Di1 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Di1, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Di1, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Di2(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Di2 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Di2 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Di2, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Di2, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr1Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr1Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr1Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr1Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr1Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr2Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr2Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr2Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr2Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr2Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr3Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr3Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr3Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr3Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr3Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr4Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr4Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr4Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr4Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr4Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr5Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr5Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr5Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr5Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr5Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr6Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr6Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr6Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr6Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr6Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Di4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Di4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Di4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Di4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Di4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Di8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Di8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Di8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Di8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Di8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Dr4(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Dr4 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Dr4 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Dr4, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Dr4, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGBDataPtr" 
 function ESMF_FieldCreateGBDataPtr7Dr8(geombase, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGBDataPtr7Dr8 

 ! input arguments 
 type(ESMF_GeomBase) :: geombase 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! local variables 
 integer :: localrc 

 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 

 ESMF_FieldCreateGBDataPtr7Dr8 = & 
 ESMF_FieldCreateEmpty(name, iospec, rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldSetCommit( & 
 ESMF_FieldCreateGBDataPtr7Dr8, & 
 geombase, farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 call ESMF_FieldValidate(ESMF_FieldCreateGBDataPtr7Dr8, & 
 rc=localrc) 

 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGBDataPtr7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
! ESMF_FieldCreateGr
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateGridArraySpec"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and ArraySpec
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateGridArraySpec(grid, arrayspec, indexflag, &
    staggerloc, gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    maxHaloLWidth, maxHaloUWidth, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateGridArraySpec
!
! !ARGUMENTS:
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    type(ESMF_IndexFlag), intent(in), optional :: indexflag
    type(ESMF_StaggerLoc), intent(in), optional :: staggerloc
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field} and allocate space internally for an
! {\tt ESMF\_Array}. Return a new {\tt ESMF\_Field}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_arrayspec}.
!
! The arguments are:
! \begin{description}
! \item [grid]
! {\tt ESMF\_Grid} object.
! \item [arrayspec]
! Data type and kind specification.
! \item[{[indexflag]}]
! Indicate how DE-local indices are defined. By default each DE's
! exclusive region is placed to start at the local index space origin,
! i.e. (1, 1, ..., 1). Alternatively the DE-local index space can be
! aligned with the global index space, if a global index space is well
! defined by the associated Grid. See section \ref{opt:indexflag}
! for a list of valid indexflag options.
! \item [{[staggerloc]}]
! Stagger location of data in grid cells. For valid
! predefined values see Section \ref{sec:opt:staggerloc}.
! To create a custom stagger location see Section
! \ref{sec:usage:staggerloc:adv}. The default
! value is {\tt ESMF\_STAGGERLOC\_CENTER}.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the Grid dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! Grid dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    type(ESMF_StaggerLoc) :: localStaggerLoc
    type(ESMF_IndexFlag) :: g_indexflag
    type(ESMF_GridDecompType) :: decompType
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if(present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit, arrayspec)
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if (decompType .eq. ESMF_GRID_ARBITRARY) then
 if ((present(indexflag)) .or. (present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
                 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", &
                 ESMF_CONTEXT, rc)
           return
        endif
    endif
    call ESMF_GridGet(grid, indexFlag=g_indexflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if (present(indexflag)) then
      if(.not. (g_indexflag .eq. indexflag)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
             "- user specified indexflag must be identical with Grid indexflag", &
               ESMF_CONTEXT, rc)
        return
      endif
    endif
    ! Set default values.
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
    (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", &
                 ESMF_CONTEXT, rc)
           return
  else
     localStaggerloc=staggerloc
  endif
    else
       localStaggerLoc = ESMF_STAGGERLOC_CENTER
    endif
    ! Create GeomBase from Grid
    geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateGridArraySpec=ESMF_FieldCreateGBArraySpec(geombase, &
        arrayspec, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, name, iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! link the Attribute hierarchies
    call c_ESMC_AttributeLink(ESMF_FieldCreateGridArraySpec%ftypep%base, &
                              grid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
    if(present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateGridArraySpec
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateGridArray"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and Array
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateGridArray(grid, array, copyflag, staggerloc, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, maxHaloLWidth, &
    maxHaloUWidth, name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateGridArray
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Array), intent(in) :: array
    type(ESMF_CopyFlag), intent(in), optional :: copyflag
    type(ESMF_StaggerLoc), intent(in), optional :: staggerloc
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    integer, intent(in), optional :: maxHaloLWidth(:)
    integer, intent(in), optional :: maxHaloUWidth(:)
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field}. This version of creation
! assumes the data exists already and is being
! passed in through an {\tt ESMF\_Array}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_array}.
!
! The arguments are:
! \begin{description}
! \item [grid]
! {\tt ESMF\_Grid} object.
! \item [array]
! {\tt ESMF\_Array} object.
! \item [{[copyflag]}]
! Indicates whether to copy the contents of the {\tt array} or reference it directly.
! For valid values see \ref{opt:copyflag}. The default is
! {\tt ESMF\_DATA\_REF}.
! \item [{[staggerloc]}]
! Stagger location of data in grid cells. For valid
! predefined values see Section \ref{sec:opt:staggerloc}.
! To create a custom stagger location see Section
! \ref{sec:usage:staggerloc:adv}. The default
! value is {\tt ESMF\_STAGGERLOC\_CENTER}.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the Grid dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! Grid dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[maxHaloLWidth]}]
! Lower bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloLWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt minHaloLWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[maxHaloUWidth]}]
! Upper bound of halo region. The size of this array is the number
! of gridded dimensions in the Field. However, ordering of the elements
! needs to be the same as they appear in the {\tt field}. Values default
! to 0. If values for maxHaloUWidth are specified they must be reflected in
! the size of the {\tt field}. That is, for each gridded dimension the
! {\tt field} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth}
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored
! in preparation for the implementation of the halo method.
! HALO OPERATION NOT IMPLEMENTED
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    type(ESMF_StaggerLoc) :: localStaggerloc
    type(ESMF_GridDecompType) :: decompType
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if (present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if (decompType .eq. ESMF_GRID_ARBITRARY) then
        if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
                 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", &
                 ESMF_CONTEXT, rc)
           return
        endif
    endif
    ! Set default values.
    if (present(staggerloc)) then
       if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. &
    (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
                 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", &
                 ESMF_CONTEXT, rc)
           return
  else
     localStaggerloc=staggerloc
  endif
    else
       localStaggerLoc = ESMF_STAGGERLOC_CENTER
    endif
    ! Create GeomBase from Grid
    geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateGridArray=ESMF_FieldCreateGBArray(geombase, array, copyflag, &
        gridToFieldMap, ungriddedLBound, ungriddedUBound, maxHaloLWidth, &
        maxHaloUWidth, name, iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    ! link the Attribute hierarchies
    call c_ESMC_AttributeLink(ESMF_FieldCreateGridArray%ftypep%base, &
                              grid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateGridArray
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and Fortran array 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateGridData<rank><type><kind>(grid, & 
! farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
! ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateGridData<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Grid) :: grid 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray 
! type(ESMF_IndexFlag), intent(in) :: indexflag 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: ungriddedLBound(:) 
! integer, intent(in), optional :: ungriddedUBound(:) 
! integer, intent(in), optional :: maxHaloLWidth(:) 
! integer, intent(in), optional :: maxHaloUWidth(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data array and {\tt ESMF\_Grid}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried but deallocating 
! the retrieved data pointer is not allowed. 
! For examples and 
! associated documentations using this method see Section 
! \ref{sec:field:usage:create_2dptr}, 
! \ref{sec:field:usage:create_2dgrid_3dptr}, 
! \ref{sec:field:usage:create_2dgrid_3dptr_map}, 
! \ref{sec:field:usage:create_2dgrid_3dptr_map_halo}, and 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}. 
! 
! The arguments are: 
! \begin{description} 
! \item [grid] 
! {\tt ESMF\_Grid} object. 
! \item [farray] 
! Native fortran data array to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farray}. 
! \item[indexflag] 
! Indicate how DE-local indices are defined. See section 
! \ref{opt:indexflag} for a list of valid indexflag options. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farray} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[staggerloc]}] 
! Stagger location of data in grid cells. For valid 
! predefined values see Section \ref{sec:opt:staggerloc}. 
! To create a custom stagger location see Section 
! \ref{sec:usage:staggerloc:adv}. The default 
! value is {\tt ESMF\_STAGGERLOC\_CENTER}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt grid}'s dimCount. The list elements map each dimension 
! of the {\tt grid} to a dimension in the {\tt farray} by 
! specifying the appropriate {\tt farray} dimension index. The default is to 
! map all of the {\tt grid}'s dimensions against the lowest dimensions of 
! the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farray} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farray} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt grid}. Ungridded dimensions must be in the same order they are 
! stored in the {\t farray}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the Grid dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! Grid dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[ungriddedLBound]}] 
! Lower bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[ungriddedUBound]}] 
! Upper bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[maxHaloLWidth]}] 
! Lower bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farray}. Values default 
! to 0. If values for maxHaloLWidth are specified they must be reflected in 
! the size of the {\tt farray}. That is, for each gridded dimension the 
! {\tt farray} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt minHaloLWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[maxHaloUWidth]}] 
! Upper bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farray}. Values default 
! to 0. If values for maxHaloUWidth are specified they must be reflected in 
! the size of the {\tt farray}. That is, for each gridded dimension the 
! {\tt farray} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Di1 = & 
 ESMF_FieldCreateGBData1Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Di1 = & 
 ESMF_FieldCreateGBData2Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Di1 = & 
 ESMF_FieldCreateGBData3Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Di1 = & 
 ESMF_FieldCreateGBData4Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Di1 = & 
 ESMF_FieldCreateGBData5Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Di1 = & 
 ESMF_FieldCreateGBData6Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Di1(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Di1 = & 
 ESMF_FieldCreateGBData7Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Di2 = & 
 ESMF_FieldCreateGBData1Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Di2 = & 
 ESMF_FieldCreateGBData2Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Di2 = & 
 ESMF_FieldCreateGBData3Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Di2 = & 
 ESMF_FieldCreateGBData4Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Di2 = & 
 ESMF_FieldCreateGBData5Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Di2 = & 
 ESMF_FieldCreateGBData6Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Di2(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Di2 = & 
 ESMF_FieldCreateGBData7Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Di4 = & 
 ESMF_FieldCreateGBData1Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Di8 = & 
 ESMF_FieldCreateGBData1Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Dr4 = & 
 ESMF_FieldCreateGBData1Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData1Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData1Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData1Dr8 = & 
 ESMF_FieldCreateGBData1Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData1Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Di4 = & 
 ESMF_FieldCreateGBData2Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Di8 = & 
 ESMF_FieldCreateGBData2Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Dr4 = & 
 ESMF_FieldCreateGBData2Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData2Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData2Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData2Dr8 = & 
 ESMF_FieldCreateGBData2Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData2Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Di4 = & 
 ESMF_FieldCreateGBData3Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Di8 = & 
 ESMF_FieldCreateGBData3Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Dr4 = & 
 ESMF_FieldCreateGBData3Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData3Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData3Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData3Dr8 = & 
 ESMF_FieldCreateGBData3Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData3Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Di4 = & 
 ESMF_FieldCreateGBData4Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Di8 = & 
 ESMF_FieldCreateGBData4Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Dr4 = & 
 ESMF_FieldCreateGBData4Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData4Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData4Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData4Dr8 = & 
 ESMF_FieldCreateGBData4Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData4Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Di4 = & 
 ESMF_FieldCreateGBData5Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Di8 = & 
 ESMF_FieldCreateGBData5Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Dr4 = & 
 ESMF_FieldCreateGBData5Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData5Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData5Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData5Dr8 = & 
 ESMF_FieldCreateGBData5Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData5Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Di4 = & 
 ESMF_FieldCreateGBData6Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Di8 = & 
 ESMF_FieldCreateGBData6Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Dr4 = & 
 ESMF_FieldCreateGBData6Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData6Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData6Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData6Dr8 = & 
 ESMF_FieldCreateGBData6Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData6Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Di4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Di4 = & 
 ESMF_FieldCreateGBData7Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Di8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Di8 = & 
 ESMF_FieldCreateGBData7Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Dr4(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Dr4 = & 
 ESMF_FieldCreateGBData7Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridData" 
 function ESMF_FieldCreateGridData7Dr8(grid, & 
 farray, indexflag, copyflag, staggerloc, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridData7Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridData7Dr8 = & 
 ESMF_FieldCreateGBData7Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridData7Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridData7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Grid and Fortran array pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateGridDataPtr<rank><type><kind>(grid, & 
! farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
! maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Grid) :: grid 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farrayPtr 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: maxHaloLWidth(:) 
! integer, intent(in), optional :: maxHaloUWidth(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data pointer and {\tt ESMF\_Grid}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried and deallocated when 
! copyflag is {\tt ESMF\_DATA\_REF}. Note that the {\tt ESMF\_FieldDestroy} call does not 
! deallocate the fortran data pointer in this case. This gives user more flexibility over memory management. 
! 
! For examples and 
! associated documentations using this method see Section 
! \ref{sec:field:usage:create_2dptr}, 
! \ref{sec:field:usage:create_2dgrid_3dptr}, 
! \ref{sec:field:usage:create_2dgrid_3dptr_map}, 
! \ref{sec:field:usage:create_2dgrid_3dptr_map_halo}, and 
! \ref{sec:field:usage:create_5dgrid_7dptr_2dungridded}. 
! 
! The arguments are: 
! \begin{description} 
! \item [grid] 
! {\tt ESMF\_Grid} object. 
! \item [farrayPtr] 
! Native fortran data pointer to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farrayPtr}. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farrayPtr} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[staggerloc]}] 
! Stagger location of data in grid cells. For valid 
! predefined values see Section \ref{sec:opt:staggerloc}. 
! To create a custom stagger location see Section 
! \ref{sec:usage:staggerloc:adv}. The default 
! value is {\tt ESMF\_STAGGERLOC\_CENTER}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt grid}'s dimCount. The list elements map each dimension 
! of the {\tt grid} to a dimension in the {\tt farrayPtr} by 
! specifying the appropriate {\tt farrayPtr} dimension index. The default is to 
! map all of the {\tt grid}'s dimensions against the lowest dimensions of 
! the {\tt farrayPtr} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farrayPtr} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farrayPtr} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt grid}. Ungridded dimensions must be in the same order they are 
! stored in the {\t farrayPtr}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the Grid dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! Grid dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[maxHaloLWidth]}] 
! Lower bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farrayPtr}. Values default 
! to 0. If values for maxHaloLWidth are specified they must be reflected in 
! the size of the {\tt farrayPtr}. That is, for each gridded dimension the 
! {\tt farrayPtr} size should be max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt minHaloLWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[maxHaloUWidth]}] 
! Upper bound of halo region. The size of this array is the number 
! of gridded dimensions in the Field. However, ordering of the elements 
! needs to be the same as they appear in the {\tt farrayPtr}. Values default 
! to 0. If values for maxHaloUWidth are specified they must be reflected in 
! the size of the {\tt farrayPtr}. That is, for each gridded dimension the 
! {\tt farrayPtr} size should max( {\tt maxHaloLWidth} + {\tt maxHaloUWidth} 
! + {\tt computationalCount}, {\tt exclusiveCount} ). Although the halo operation is not 
! implemented, the {\tt maxHaloUWidth} is checked for validity and stored 
! in preparation for the implementation of the halo method. 
! HALO OPERATION NOT IMPLEMENTED 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Di1 = & 
 ESMF_FieldCreateGBDataPtr1Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Di1 = & 
 ESMF_FieldCreateGBDataPtr2Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Di1 = & 
 ESMF_FieldCreateGBDataPtr3Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Di1 = & 
 ESMF_FieldCreateGBDataPtr4Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Di1 = & 
 ESMF_FieldCreateGBDataPtr5Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Di1 = & 
 ESMF_FieldCreateGBDataPtr6Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Di1(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Di1 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Di1 = & 
 ESMF_FieldCreateGBDataPtr7Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Di1%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Di2 = & 
 ESMF_FieldCreateGBDataPtr1Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Di2 = & 
 ESMF_FieldCreateGBDataPtr2Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Di2 = & 
 ESMF_FieldCreateGBDataPtr3Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Di2 = & 
 ESMF_FieldCreateGBDataPtr4Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Di2 = & 
 ESMF_FieldCreateGBDataPtr5Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Di2 = & 
 ESMF_FieldCreateGBDataPtr6Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Di2(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Di2 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Di2 = & 
 ESMF_FieldCreateGBDataPtr7Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Di2%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Di4 = & 
 ESMF_FieldCreateGBDataPtr1Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Di8 = & 
 ESMF_FieldCreateGBDataPtr1Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Dr4 = & 
 ESMF_FieldCreateGBDataPtr1Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr1Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr1Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr1Dr8 = & 
 ESMF_FieldCreateGBDataPtr1Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr1Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Di4 = & 
 ESMF_FieldCreateGBDataPtr2Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Di8 = & 
 ESMF_FieldCreateGBDataPtr2Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Dr4 = & 
 ESMF_FieldCreateGBDataPtr2Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr2Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr2Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr2Dr8 = & 
 ESMF_FieldCreateGBDataPtr2Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr2Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Di4 = & 
 ESMF_FieldCreateGBDataPtr3Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Di8 = & 
 ESMF_FieldCreateGBDataPtr3Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Dr4 = & 
 ESMF_FieldCreateGBDataPtr3Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr3Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr3Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr3Dr8 = & 
 ESMF_FieldCreateGBDataPtr3Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr3Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Di4 = & 
 ESMF_FieldCreateGBDataPtr4Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Di8 = & 
 ESMF_FieldCreateGBDataPtr4Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Dr4 = & 
 ESMF_FieldCreateGBDataPtr4Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr4Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr4Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr4Dr8 = & 
 ESMF_FieldCreateGBDataPtr4Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr4Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Di4 = & 
 ESMF_FieldCreateGBDataPtr5Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Di8 = & 
 ESMF_FieldCreateGBDataPtr5Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Dr4 = & 
 ESMF_FieldCreateGBDataPtr5Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr5Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr5Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr5Dr8 = & 
 ESMF_FieldCreateGBDataPtr5Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr5Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Di4 = & 
 ESMF_FieldCreateGBDataPtr6Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Di8 = & 
 ESMF_FieldCreateGBDataPtr6Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Dr4 = & 
 ESMF_FieldCreateGBDataPtr6Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr6Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr6Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr6Dr8 = & 
 ESMF_FieldCreateGBDataPtr6Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr6Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Di4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Di4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Di4 = & 
 ESMF_FieldCreateGBDataPtr7Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Di4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Di8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Di8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Di8 = & 
 ESMF_FieldCreateGBDataPtr7Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Di8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Dr4(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Dr4 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Dr4 = & 
 ESMF_FieldCreateGBDataPtr7Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Dr4%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateGridDataPtr" 
 function ESMF_FieldCreateGridDataPtr7Dr8(grid, & 
 farrayPtr, copyflag, staggerloc, gridToFieldMap, & 
 maxHaloLWidth, maxHaloUWidth, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateGridDataPtr7Dr8 

 ! input arguments 
 type(ESMF_Grid) :: grid 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 type(ESMF_StaggerLoc), intent(in), optional ::staggerloc 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: maxHaloLWidth(:) 
 integer, intent(in), optional :: maxHaloUWidth(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 
 type(ESMF_StaggerLoc) :: localStaggerloc
 type(ESMF_GridDecompType) :: decompType 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)


 call ESMF_GridGetDecompType(grid, decompType, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 if (decompType .eq. ESMF_GRID_ARBITRARY) then 
 if ((present(maxHaloLWidth)) .or. (present(maxHaloUWidth))) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- indexflag, maxHaloLWidth or maxHaloUWidth are not allowed for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 ! Set default values. 
 if (present(staggerloc)) then 
 if ((decompType .eq. ESMF_GRID_ARBITRARY) .and. & 
 (staggerloc .ne. ESMF_STAGGERLOC_CENTER)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 "- staggerloc has to be ESMF_STAGGERLOC_CENTER for arbitrary grid", & 
 ESMF_CONTEXT, rc) 
 return 
 else 
 localStaggerloc=staggerloc 
 endif 
 else 
 localStaggerLoc = ESMF_STAGGERLOC_CENTER 
 endif 

 ! Create GeomBase from Grid
 geombase=ESMF_GeomBaseCreate(grid,localstaggerloc, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateGridDataPtr7Dr8 = & 
 ESMF_FieldCreateGBDataPtr7Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 

 ! link the Attribute hierarchies 
 call c_ESMC_AttributeLink( & 
 ESMF_FieldCreateGridDataPtr7Dr8%ftypep%base, & 
 grid, localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateGridDataPtr7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
! ESMF_FieldCreateLo
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateLSArraySpec"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from LocStream and ArraySpec
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateLSArraySpec(locstream, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateLSArraySpec
!
! !ARGUMENTS:
    type(ESMF_LocStream) :: locstream
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field} and allocate space internally for an
! {\tt ESMF\_Array}. Return a new {\tt ESMF\_Field}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_locs_arrayspec}.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! {\tt ESMF\_LocStream} object.
! \item [arrayspec]
! Data type and kind specification.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the LocStream dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! LocStream dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if(present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit, arrayspec)
    ! Create GeomBase from Mesh
    geombase=ESMF_GeomBaseCreate(locstream, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateLSArraySpec=ESMF_FieldCreateGBArraySpec(geombase, &
        arrayspec, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        name=name, iospec=iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    if(present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateLSArraySpec
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateLSArray"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from LocStream and Array
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateLSArray(locstream, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateLSArray
!
! !ARGUMENTS:
    type(ESMF_LocStream), intent(in) :: locstream
    type(ESMF_Array), intent(in) :: array
    type(ESMF_CopyFlag), intent(in), optional :: copyflag
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field}. This version of creation
! assumes the data exists already and is being
! passed in through an {\tt ESMF\_Array}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_array}.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! {\tt ESMF\_LocStream} object.
! \item [array]
! {\tt ESMF\_Array} object.
! \item [{[copyflag]}]
! Indicates whether to copy the contents of the {\tt array} or reference it directly.
! For valid values see \ref{opt:copyflag}. The default is
! {\tt ESMF\_DATA\_REF}.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the LocStream dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! LocStream dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if (present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)
    ! Create GeomBase from Mesh
    geombase=ESMF_GeomBaseCreate(locstream, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateLSArray=ESMF_FieldCreateGBArray(geombase, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name=name, iospec=iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateLSArray
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from LocStream and Fortran array 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateLSData<rank><type><kind>(locstream, & 
! farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
! ungriddedUBound, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateLSData<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_LocStream) :: locstream 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray 
! type(ESMF_IndexFlag), intent(in) :: indexflag 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: ungriddedLBound(:) 
! integer, intent(in), optional :: ungriddedUBound(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data array and {\tt ESMF\_LocStream}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried but deallocating 
! the retrieved data pointer is not allowed. 
! 
! The arguments are: 
! \begin{description} 
! \item [locstream] 
! {\tt ESMF\_LocStream} object. 
! \item [farray] 
! Native fortran data array to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farray}. 
! \item[indexflag] 
! Indicate how DE-local indices are defined. See section 
! \ref{opt:indexflag} for a list of valid indexflag options. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farray} or reference directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt locstream}'s dimCount. The list elements map each dimension 
! of the {\tt locstream} to a dimension in the {\tt farray} by 
! specifying the appropriate {\tt farray} dimension index. The default is to 
! map all of the {\tt locstream}'s dimensions against the lowest dimensions of 
! the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farray} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farray} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt locstream}. Unlocstreamded dimensions must be in the same order they are 
! stored in the {\t farray}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the LocStream dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! LocStream dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[ungriddedLBound]}] 
! Lower bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than locstream dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[ungriddedUBound]}] 
! Upper bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than locstream dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Di1 = & 
 ESMF_FieldCreateGBData1Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Di1 = & 
 ESMF_FieldCreateGBData2Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Di1 = & 
 ESMF_FieldCreateGBData3Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Di1 = & 
 ESMF_FieldCreateGBData4Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Di1 = & 
 ESMF_FieldCreateGBData5Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Di1 = & 
 ESMF_FieldCreateGBData6Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Di1(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Di1 = & 
 ESMF_FieldCreateGBData7Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Di2 = & 
 ESMF_FieldCreateGBData1Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Di2 = & 
 ESMF_FieldCreateGBData2Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Di2 = & 
 ESMF_FieldCreateGBData3Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Di2 = & 
 ESMF_FieldCreateGBData4Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Di2 = & 
 ESMF_FieldCreateGBData5Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Di2 = & 
 ESMF_FieldCreateGBData6Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Di2(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Di2 = & 
 ESMF_FieldCreateGBData7Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Di4 = & 
 ESMF_FieldCreateGBData1Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Di8 = & 
 ESMF_FieldCreateGBData1Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Dr4 = & 
 ESMF_FieldCreateGBData1Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData1Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData1Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData1Dr8 = & 
 ESMF_FieldCreateGBData1Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Di4 = & 
 ESMF_FieldCreateGBData2Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Di8 = & 
 ESMF_FieldCreateGBData2Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Dr4 = & 
 ESMF_FieldCreateGBData2Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData2Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData2Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData2Dr8 = & 
 ESMF_FieldCreateGBData2Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Di4 = & 
 ESMF_FieldCreateGBData3Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Di8 = & 
 ESMF_FieldCreateGBData3Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Dr4 = & 
 ESMF_FieldCreateGBData3Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData3Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData3Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData3Dr8 = & 
 ESMF_FieldCreateGBData3Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Di4 = & 
 ESMF_FieldCreateGBData4Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Di8 = & 
 ESMF_FieldCreateGBData4Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Dr4 = & 
 ESMF_FieldCreateGBData4Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData4Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData4Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData4Dr8 = & 
 ESMF_FieldCreateGBData4Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Di4 = & 
 ESMF_FieldCreateGBData5Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Di8 = & 
 ESMF_FieldCreateGBData5Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Dr4 = & 
 ESMF_FieldCreateGBData5Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData5Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData5Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData5Dr8 = & 
 ESMF_FieldCreateGBData5Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Di4 = & 
 ESMF_FieldCreateGBData6Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Di8 = & 
 ESMF_FieldCreateGBData6Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Dr4 = & 
 ESMF_FieldCreateGBData6Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData6Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData6Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData6Dr8 = & 
 ESMF_FieldCreateGBData6Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Di4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Di4 = & 
 ESMF_FieldCreateGBData7Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Di8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Di8 = & 
 ESMF_FieldCreateGBData7Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Dr4(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Dr4 = & 
 ESMF_FieldCreateGBData7Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSData" 
 function ESMF_FieldCreateLSData7Dr8(locstream, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSData7Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSData7Dr8 = & 
 ESMF_FieldCreateGBData7Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSData7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from LocStream and Fortran array pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateLSDataPtr<rank><type><kind>(locstream, & 
! farrayPtr, copyflag, gridToFieldMap, & 
! name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_LocStream) :: locstream 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farrayPtr 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data pointer and {\tt ESMF\_LocStream}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried and deallocated when 
! copyflag is {\tt ESMF\_DATA\_REF}. Note that the {\tt ESMF\_FieldDestroy} call does not 
! deallocate the fortran data pointer in this case. This gives user more flexibility over memory management. 
! 
! The arguments are: 
! \begin{description} 
! \item [locstream] 
! {\tt ESMF\_LocStream} object. 
! \item [farrayPtr] 
! Native fortran data pointer to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farrayPtr}. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farrayPtr} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt locstream}'s dimCount. The list elements map each dimension 
! of the {\tt locstream} to a dimension in the {\tt farrayPtr} by 
! specifying the appropriate {\tt farrayPtr} dimension index. The default is to 
! map all of the {\tt locstream}'s dimensions against the lowest dimensions of 
! the {\tt farrayPtr} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farrayPtr} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farrayPtr} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt locstream}. Unlocstreamded dimensions must be in the same order they are 
! stored in the {\t farrayPtr}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the LocStream dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! LocStream dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Di1 = & 
 ESMF_FieldCreateGBDataPtr1Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Di1 = & 
 ESMF_FieldCreateGBDataPtr2Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Di1 = & 
 ESMF_FieldCreateGBDataPtr3Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Di1 = & 
 ESMF_FieldCreateGBDataPtr4Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Di1 = & 
 ESMF_FieldCreateGBDataPtr5Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Di1 = & 
 ESMF_FieldCreateGBDataPtr6Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Di1(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Di1 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Di1 = & 
 ESMF_FieldCreateGBDataPtr7Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Di2 = & 
 ESMF_FieldCreateGBDataPtr1Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Di2 = & 
 ESMF_FieldCreateGBDataPtr2Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Di2 = & 
 ESMF_FieldCreateGBDataPtr3Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Di2 = & 
 ESMF_FieldCreateGBDataPtr4Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Di2 = & 
 ESMF_FieldCreateGBDataPtr5Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Di2 = & 
 ESMF_FieldCreateGBDataPtr6Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Di2(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Di2 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Di2 = & 
 ESMF_FieldCreateGBDataPtr7Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Di4 = & 
 ESMF_FieldCreateGBDataPtr1Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Di8 = & 
 ESMF_FieldCreateGBDataPtr1Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Dr4 = & 
 ESMF_FieldCreateGBDataPtr1Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr1Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr1Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr1Dr8 = & 
 ESMF_FieldCreateGBDataPtr1Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Di4 = & 
 ESMF_FieldCreateGBDataPtr2Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Di8 = & 
 ESMF_FieldCreateGBDataPtr2Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Dr4 = & 
 ESMF_FieldCreateGBDataPtr2Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr2Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr2Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr2Dr8 = & 
 ESMF_FieldCreateGBDataPtr2Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Di4 = & 
 ESMF_FieldCreateGBDataPtr3Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Di8 = & 
 ESMF_FieldCreateGBDataPtr3Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Dr4 = & 
 ESMF_FieldCreateGBDataPtr3Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr3Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr3Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr3Dr8 = & 
 ESMF_FieldCreateGBDataPtr3Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Di4 = & 
 ESMF_FieldCreateGBDataPtr4Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Di8 = & 
 ESMF_FieldCreateGBDataPtr4Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Dr4 = & 
 ESMF_FieldCreateGBDataPtr4Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr4Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr4Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr4Dr8 = & 
 ESMF_FieldCreateGBDataPtr4Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Di4 = & 
 ESMF_FieldCreateGBDataPtr5Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Di8 = & 
 ESMF_FieldCreateGBDataPtr5Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Dr4 = & 
 ESMF_FieldCreateGBDataPtr5Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr5Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr5Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr5Dr8 = & 
 ESMF_FieldCreateGBDataPtr5Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Di4 = & 
 ESMF_FieldCreateGBDataPtr6Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Di8 = & 
 ESMF_FieldCreateGBDataPtr6Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Dr4 = & 
 ESMF_FieldCreateGBDataPtr6Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr6Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr6Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr6Dr8 = & 
 ESMF_FieldCreateGBDataPtr6Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Di4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Di4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Di4 = & 
 ESMF_FieldCreateGBDataPtr7Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Di8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Di8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Di8 = & 
 ESMF_FieldCreateGBDataPtr7Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Dr4(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Dr4 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Dr4 = & 
 ESMF_FieldCreateGBDataPtr7Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateLSDataPtr" 
 function ESMF_FieldCreateLSDataPtr7Dr8(locstream, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateLSDataPtr7Dr8 

 ! input arguments 
 type(ESMF_LocStream) :: locstream 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)


 ! Create GeomBase from LocStream
 geombase=ESMF_GeomBaseCreate(locstream,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateLSDataPtr7Dr8 = & 
 ESMF_FieldCreateGBDataPtr7Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateLSDataPtr7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
! ESMF_FieldCreateMe
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateMeshArraySpec"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from Mesh and ArraySpec
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateMeshArraySpec(mesh, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateMeshArraySpec
!
! !ARGUMENTS:
    type(ESMF_Mesh) :: mesh
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    character (len=*), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field} and allocate space internally for an
! {\tt ESMF\_Array}. Return a new {\tt ESMF\_Field}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_arrayspec}.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! {\tt ESMF\_Mesh} object.
! \item [arrayspec]
! Data type and kind specification.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the Mesh dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! Mesh dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. ! NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if(present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit, arrayspec)
    ! Create GeomBase from Mesh
    geombase=ESMF_GeomBaseCreate(mesh, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateMeshArraySpec=ESMF_FieldCreateGBArraySpec(geombase, &
        arrayspec, gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        name=name, iospec=iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    if(present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateMeshArraySpec
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateMeshArray"
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from Mesh and Array
! !INTERFACE:
  ! Private name; call using ESMF_FieldCreate()
  function ESMF_FieldCreateMeshArray(mesh, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name, iospec, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_FieldCreateMeshArray
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in) :: mesh
    type(ESMF_Array), intent(in) :: array
    type(ESMF_CopyFlag), intent(in), optional :: copyflag
    integer, intent(in), optional :: gridToFieldMap(:)
    integer, intent(in), optional :: ungriddedLBound(:)
    integer, intent(in), optional :: ungriddedUBound(:)
    character (len = *), intent(in), optional :: name
    type(ESMF_IOSpec), intent(in), optional :: iospec
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Field}. This version of creation
! assumes the data exists already and is being
! passed in through an {\tt ESMF\_Array}. For an example and
! associated documentation using this method see Section
! \ref{sec:field:usage:create_grid_array}.
!
! The arguments are:
! \begin{description}
! \item [grid]
! {\tt ESMF\_Mesh} object.
! \item [array]
! {\tt ESMF\_Array} object.
! \item [{[copyflag]}]
! Indicates whether to copy the contents of the {\tt array} or reference it directly.
! For valid values see \ref{opt:copyflag}. The default is
! {\tt ESMF\_DATA\_REF}.
! \item [{[gridToFieldMap]}]
! List with number of elements equal to the
! {\tt grid}'s dimCount. The list elements map each dimension
! of the {\tt grid} to a dimension in the {\tt field} by
! specifying the appropriate {\tt field} dimension index. The default is to
! map all of the {\tt grid}'s dimensions against the lowest dimensions of
! the {\tt field} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../).
! The values of all {\tt gridToFieldMap} entries must be greater than or equal
! to one and smaller than or equal to the {\tt field} rank.
! It is erroneous to specify the same {\tt gridToFieldMap} entry
! multiple times. The total ungridded dimensions in the {\tt field}
! are the total {\tt field} dimensions less
! the dimensions in
! the {\tt grid}. Ungridded dimensions must be in the same order they are
! stored in the {\t field}.
! If the Field dimCount is less than the Mesh dimCount then the default
! gridToFieldMap will contain zeros for the rightmost entries. A zero
! entry in the {\tt gridToFieldMap} indicates that the particular
! Mesh dimension will be replicating the Field across the DEs along
! this direction.
! \item [{[ungriddedLBound]}]
! Lower bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[ungriddedUBound]}]
! Upper bounds of the ungridded dimensions of the {\tt field}.
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded
! dimensions in the {\tt field}. All ungridded dimensions of the
! {\tt field} are also undistributed. When field dimension count is
! greater than grid dimension count, both ungriddedLBound and ungriddedUBound
! must be specified. When both are specified the values are checked
! for consistency. Note that the the ordering of
! these ungridded dimensions is the same as their order in the {\tt field}.
! \item [{[name]}]
! Field name.
! \item [{[iospec]}]
! I/O specification. NOT IMPLEMENTED
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! Local error code
    type(ESMF_GeomBase) :: geombase
    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    ! Initialize return code
    if (present(rc)) then
      rc = ESMF_RC_NOT_IMPL
    endif
    ! check init status of input parameters
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)
    ! Create GeomBase from Mesh
    geombase=ESMF_GeomBaseCreate(mesh, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    ! Create Field from GeomBase
    ESMF_FieldCreateMeshArray=ESMF_FieldCreateGBArray(geombase, array, copyflag, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, &
    name=name, iospec=iospec, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_FieldCreateMeshArray
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Mesh and Fortran array 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateMeshData<rank><type><kind>(mesh, & 
! farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
! ungriddedUBound, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateMeshData<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Mesh) :: mesh 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), target :: farray 
! type(ESMF_IndexFlag), intent(in) :: indexflag 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! integer, intent(in), optional :: ungriddedLBound(:) 
! integer, intent(in), optional :: ungriddedUBound(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data array and {\tt ESMF\_Mesh}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried but deallocating 
! the retrieved data pointer is not allowed. 
! 
! The arguments are: 
! \begin{description} 
! \item [mesh] 
! {\tt ESMF\_Mesh} object. 
! \item [farray] 
! Native fortran data array to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farray}. 
! \item[indexflag] 
! Indicate how DE-local indices are defined. See section 
! \ref{opt:indexflag} for a list of valid indexflag options. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farray} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt mesh}'s dimCount. The list elements map each dimension 
! of the {\tt mesh} to a dimension in the {\tt farray} by 
! specifying the appropriate {\tt farray} dimension index. The default is to 
! map all of the {\tt mesh}'s dimensions against the lowest dimensions of 
! the {\tt farray} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farray} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farray} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt mesh}. Unmeshded dimensions must be in the same order they are 
! stored in the {\t farray}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the Mesh dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! Mesh dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[ungriddedLBound]}] 
! Lower bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedLBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than mesh dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[ungriddedUBound]}] 
! Upper bounds of the ungridded dimensions of the {\tt field}. 
! The number of elements in the {\tt ungriddedUBound} is equal to the number of ungridded 
! dimensions in the {\tt field}. All ungridded dimensions of the 
! {\tt field} are also undistributed. When field dimension count is 
! greater than mesh dimension count, both ungriddedLBound and ungriddedUBound 
! must be specified. When both are specified the values are checked 
! for consistency. Note that the the ordering of 
! these ungridded dimensions is the same as their order in the {\tt farray}. 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Di1 = & 
 ESMF_FieldCreateGBData1Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Di1 = & 
 ESMF_FieldCreateGBData2Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Di1 = & 
 ESMF_FieldCreateGBData3Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Di1 = & 
 ESMF_FieldCreateGBData4Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Di1 = & 
 ESMF_FieldCreateGBData5Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Di1 = & 
 ESMF_FieldCreateGBData6Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Di1(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Di1 = & 
 ESMF_FieldCreateGBData7Di1(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Di2 = & 
 ESMF_FieldCreateGBData1Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Di2 = & 
 ESMF_FieldCreateGBData2Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Di2 = & 
 ESMF_FieldCreateGBData3Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Di2 = & 
 ESMF_FieldCreateGBData4Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Di2 = & 
 ESMF_FieldCreateGBData5Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Di2 = & 
 ESMF_FieldCreateGBData6Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Di2(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Di2 = & 
 ESMF_FieldCreateGBData7Di2(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Di4 = & 
 ESMF_FieldCreateGBData1Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Di8 = & 
 ESMF_FieldCreateGBData1Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Dr4 = & 
 ESMF_FieldCreateGBData1Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData1Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData1Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData1Dr8 = & 
 ESMF_FieldCreateGBData1Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Di4 = & 
 ESMF_FieldCreateGBData2Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Di8 = & 
 ESMF_FieldCreateGBData2Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Dr4 = & 
 ESMF_FieldCreateGBData2Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData2Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData2Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData2Dr8 = & 
 ESMF_FieldCreateGBData2Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Di4 = & 
 ESMF_FieldCreateGBData3Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Di8 = & 
 ESMF_FieldCreateGBData3Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Dr4 = & 
 ESMF_FieldCreateGBData3Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData3Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData3Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData3Dr8 = & 
 ESMF_FieldCreateGBData3Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Di4 = & 
 ESMF_FieldCreateGBData4Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Di8 = & 
 ESMF_FieldCreateGBData4Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Dr4 = & 
 ESMF_FieldCreateGBData4Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData4Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData4Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData4Dr8 = & 
 ESMF_FieldCreateGBData4Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Di4 = & 
 ESMF_FieldCreateGBData5Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Di8 = & 
 ESMF_FieldCreateGBData5Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Dr4 = & 
 ESMF_FieldCreateGBData5Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData5Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData5Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData5Dr8 = & 
 ESMF_FieldCreateGBData5Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Di4 = & 
 ESMF_FieldCreateGBData6Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Di8 = & 
 ESMF_FieldCreateGBData6Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Dr4 = & 
 ESMF_FieldCreateGBData6Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData6Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData6Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData6Dr8 = & 
 ESMF_FieldCreateGBData6Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Di4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Di4 = & 
 ESMF_FieldCreateGBData7Di4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Di8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Di8 = & 
 ESMF_FieldCreateGBData7Di8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Dr4(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Dr4 = & 
 ESMF_FieldCreateGBData7Dr4(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshData" 
 function ESMF_FieldCreateMeshData7Dr8(mesh, & 
 farray, indexflag, copyflag, gridToFieldMap, ungriddedLBound, & 
 ungriddedUBound, name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshData7Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target :: farray 
 type(ESMF_IndexFlag), intent(in) :: indexflag 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 integer, intent(in), optional :: ungriddedLBound(:) 
 integer, intent(in), optional :: ungriddedUBound(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshData7Dr8 = & 
 ESMF_FieldCreateGBData7Dr8(geombase, & 
 farray, indexflag=indexflag, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 ungriddedLBound=ungriddedLBound, & 
 ungriddedUBound=ungriddedUBound, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshData7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field from Mesh and Fortran array pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateMeshDataPtr<rank><type><kind>(mesh, & 
! farrayPtr, copyflag, gridToFieldMap, & 
! name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Mesh) :: mesh 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farrayPtr 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
! integer, intent(in), optional :: gridToFieldMap(:) 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} from a fortran data pointer and {\tt ESMF\_Mesh}. 
! The fortran data pointer inside {\tt ESMF\_Field} can be queried and deallocated when 
! copyflag is {\tt ESMF\_DATA\_REF}. Note that the {\tt ESMF\_FieldDestroy} call does not 
! deallocate the fortran data pointer in this case. This gives user more flexibility over memory management. 
! 
! The arguments are: 
! \begin{description} 
! \item [mesh] 
! {\tt ESMF\_Mesh} object. 
! \item [farrayPtr] 
! Native fortran data pointer to be copied/referenced in the Field 
! The Field dimension (dimCount) will be the same as the dimCount 
! for the {\tt farrayPtr}. 
! \item [{[copyflag]}] 
! Whether to copy the contents of the {\tt farrayPtr} or reference it directly. 
! For valid values see \ref{opt:copyflag}. The default is 
! {\tt ESMF\_DATA\_REF}. 
! \item [{[gridToFieldMap]}] 
! List with number of elements equal to the 
! {\tt mesh}'s dimCount. The list elements map each dimension 
! of the {\tt mesh} to a dimension in the {\tt farrayPtr} by 
! specifying the appropriate {\tt farrayPtr} dimension index. The default is to 
! map all of the {\tt mesh}'s dimensions against the lowest dimensions of 
! the {\tt farrayPtr} in sequence, i.e. {\tt gridToFieldMap} = (/1,2,3,.../). 
! The values of all {\tt gridToFieldMap} entries must be greater than or equal 
! to one and smaller than or equal to the {\tt farrayPtr} rank. 
! It is erroneous to specify the same {\tt gridToFieldMap} entry 
! multiple times. The total ungridded dimensions in the {\tt field} 
! are the total {\tt farrayPtr} dimensions less 
! the total (distributed + undistributed) dimensions in 
! the {\tt mesh}. Unmeshded dimensions must be in the same order they are 
! stored in the {\t farrayPtr}. Permutations of the order of 
! dimensions are handled via individual communication methods. For example, 
! an undistributed dimension can be remapped to a distributed dimension 
! as part of the {\tt ESMF\_ArrayRedist()} operation. 
! If the Field dimCount is less than the Mesh dimCount then the default 
! gridToFieldMap will contain zeros for the rightmost entries. A zero 
! entry in the {\tt gridToFieldMap} indicates that the particular 
! Mesh dimension will be replicating the Field across the DEs along 
! this direction. 
! \item [{[name]}] 
! Field name. 
! \item [{[iospec]}] 
! I/O specification. NOT IMPLEMENTED 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Di1 = & 
 ESMF_FieldCreateGBDataPtr1Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Di1 = & 
 ESMF_FieldCreateGBDataPtr2Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Di1 = & 
 ESMF_FieldCreateGBDataPtr3Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Di1 = & 
 ESMF_FieldCreateGBDataPtr4Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Di1 = & 
 ESMF_FieldCreateGBDataPtr5Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Di1 = & 
 ESMF_FieldCreateGBDataPtr6Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Di1(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Di1 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Di1 = & 
 ESMF_FieldCreateGBDataPtr7Di1(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Di2 = & 
 ESMF_FieldCreateGBDataPtr1Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Di2 = & 
 ESMF_FieldCreateGBDataPtr2Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Di2 = & 
 ESMF_FieldCreateGBDataPtr3Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Di2 = & 
 ESMF_FieldCreateGBDataPtr4Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Di2 = & 
 ESMF_FieldCreateGBDataPtr5Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Di2 = & 
 ESMF_FieldCreateGBDataPtr6Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Di2(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Di2 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Di2 = & 
 ESMF_FieldCreateGBDataPtr7Di2(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Di4 = & 
 ESMF_FieldCreateGBDataPtr1Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Di8 = & 
 ESMF_FieldCreateGBDataPtr1Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Dr4 = & 
 ESMF_FieldCreateGBDataPtr1Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr1Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr1Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr1Dr8 = & 
 ESMF_FieldCreateGBDataPtr1Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Di4 = & 
 ESMF_FieldCreateGBDataPtr2Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Di8 = & 
 ESMF_FieldCreateGBDataPtr2Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Dr4 = & 
 ESMF_FieldCreateGBDataPtr2Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr2Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr2Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr2Dr8 = & 
 ESMF_FieldCreateGBDataPtr2Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Di4 = & 
 ESMF_FieldCreateGBDataPtr3Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Di8 = & 
 ESMF_FieldCreateGBDataPtr3Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Dr4 = & 
 ESMF_FieldCreateGBDataPtr3Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr3Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr3Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr3Dr8 = & 
 ESMF_FieldCreateGBDataPtr3Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Di4 = & 
 ESMF_FieldCreateGBDataPtr4Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Di8 = & 
 ESMF_FieldCreateGBDataPtr4Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Dr4 = & 
 ESMF_FieldCreateGBDataPtr4Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr4Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr4Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr4Dr8 = & 
 ESMF_FieldCreateGBDataPtr4Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Di4 = & 
 ESMF_FieldCreateGBDataPtr5Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Di8 = & 
 ESMF_FieldCreateGBDataPtr5Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Dr4 = & 
 ESMF_FieldCreateGBDataPtr5Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr5Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr5Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr5Dr8 = & 
 ESMF_FieldCreateGBDataPtr5Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr5Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Di4 = & 
 ESMF_FieldCreateGBDataPtr6Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Di8 = & 
 ESMF_FieldCreateGBDataPtr6Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Dr4 = & 
 ESMF_FieldCreateGBDataPtr6Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr6Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr6Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr6Dr8 = & 
 ESMF_FieldCreateGBDataPtr6Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Di4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Di4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Di4 = & 
 ESMF_FieldCreateGBDataPtr7Di4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Di8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Di8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Di8 = & 
 ESMF_FieldCreateGBDataPtr7Di8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Dr4(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Dr4 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Dr4 = & 
 ESMF_FieldCreateGBDataPtr7Dr4(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldCreateMeshDataPtr" 
 function ESMF_FieldCreateMeshDataPtr7Dr8(mesh, & 
 farrayPtr, copyflag, gridToFieldMap, & 
 name, iospec, rc) 

 ! return value 
 type(ESMF_Field) :: ESMF_FieldCreateMeshDataPtr7Dr8 

 ! input arguments 
 type(ESMF_Mesh) :: mesh 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(in), optional :: gridToFieldMap(:) 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 integer :: localrc ! Local error code
 type(ESMF_GeomBase) :: geombase 

 ! Initialize 
 localrc = ESMF_RC_NOT_IMPL

 ! Initialize return code 
 if (present(rc)) then
 rc = ESMF_RC_NOT_IMPL
 endif 

 ! check init status of input parameters
 ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


 ! Create GeomBase from Mesh
 geombase=ESMF_GeomBaseCreate(mesh,rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
 ESMF_CONTEXT, rc)) return


 ESMF_FieldCreateMeshDataPtr7Dr8 = & 
 ESMF_FieldCreateGBDataPtr7Dr8(geombase, & 
 farrayPtr, copyflag=copyflag, & 
 gridToFieldMap=gridToFieldMap, & 
 name=name, iospec=iospec, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 


 if (present(rc)) rc = ESMF_SUCCESS 
 end function ESMF_FieldCreateMeshDataPtr7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
end module ESMF_FieldCreateMod
