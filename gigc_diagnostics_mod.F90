#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_diagnostics_mod
!
! !DESCRIPTION: Module GIGC\_DIAGNOSTICS\_MOD contains the 
!  methods to register user-defined diagnostics with the ESMF/MAPL Export state.
!  This makes them available for output through MAPL\_History.
!\\
!\\
! !INTERFACE: 
!
MODULE GIGC_Diagnostics_Mod
!
! USES:
!
  USE ESMF, ONLY : ESMF_MAXSTR, ESMF_GridComp, ESMF_State
  USE MAPL_Mod

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
! !PUBLIC DATA MEMBERS:
!
  PUBLIC :: gigc_DiagsInit
  PUBLIC :: gigc_DiagsCleanup
  PUBLIC :: gigc_PrintDiags
  PUBLIC :: gigc_AddField 
  PUBLIC :: gigc_MAPLOutField 
  PUBLIC :: gigc_MAPLRegisterDiagList
  
  ! Error success/failure definitions
  INTEGER, PARAMETER, PUBLIC  :: DIAG_SUCCESS = 0
  INTEGER, PARAMETER, PUBLIC  :: DIAG_FAILURE = -999

  TYPE(ESMF_State), PUBLIC, POINTER :: DiagExport      ! Export state
!
! !PRIVATE DATA MEMBERS:
!
  TYPE, PRIVATE :: FldData_
     ! Diagnostics information 
     CHARACTER(LEN=ESMF_MAXSTR)  :: FldNam
     CHARACTER(LEN=ESMF_MAXSTR)  :: LongNam
     CHARACTER(LEN=ESMF_MAXSTR)  :: Units
     CHARACTER(LEN=ESMF_MAXSTR)  :: Dims
     CHARACTER(LEN=ESMF_MAXSTR)  :: VLoc
     INTEGER                     :: n_Diags
  END TYPE FldData_

  !-------------------------------------------------------------------------
  ! Derived type definition for diagnostics list object
  !-------------------------------------------------------------------------
  TYPE, PRIVATE  :: DiagList
     TYPE(FldData_),     POINTER :: Fld
     TYPE(DiagList),     POINTER :: NextCont
  END TYPE DiagList

  TYPE(DiagList), POINTER, SAVE :: diags
  TYPE(ESMF_GridComp), POINTER  :: localGC          ! gridded component

  INTERFACE GIGC_MAPLOutField
     module Procedure GIGC_MAPLOutField2d
     module Procedure GIGC_MAPLOutField3d
  END INTERFACE

!
! !REMARKS:
!  -----------------------------------------------------------------------
!                                                                             
! !REVISION HISTORY:
!  18 Dec 2014 - M. Long     - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS

  SUBROUTINE gigc_DiagsInit( RC )

    INTEGER, INTENT(OUT), optional :: RC

    RC = DIAG_FAILURE

    ALLOCATE( diags )
    NULLIFY( diags%nextCont )
    diags%Fld%n_Diags = 0

    RC = DIAG_SUCCESS

  END SUBROUTINE gigc_DiagsInit

  SUBROUTINE gigc_DiagsCleanUp( RC )
    TYPE(DiagList), POINTER        :: curr, next
    INTEGER, INTENT(OUT), optional :: RC

    RC = DIAG_FAILURE

    curr => diags
    DO WHILE (ASSOCIATED(curr))
       next => curr%nextCont
       IF (ASSOCIATED(curr%Fld)) THEN
          DEALLOCATE(curr%Fld)
          NULLIFY(curr%Fld)
       END IF
       DEALLOCATE(curr)
       NULLIFY(curr)
       curr => next
    END DO

    NULLIFY(localGC)
    NULLIFY(DiagExport)

    RC = DIAG_SUCCESS

  END SUBROUTINE gigc_DiagsCleanUp

  FUNCTION F_diagGetNextInList result( currDiag )
    TYPE(DiagList), POINTER :: currDiag
    
    ! Point to head of List if passed container pointer is not yet defined.
    IF ( .NOT. ASSOCIATED ( currDiag ) ) THEN
       currDiag => diags
    ! Otherwise, just point to the next container in list
    ELSE
       currDiag => currDiag%nextCont
    END IF

  END FUNCTION F_diagGetNextInList
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_PrintDiags
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE gigc_PrintDiags

    IMPLICIT NONE
    
    type( DiagList ), pointer :: head
    type( DiagList ), pointer :: ptr
    
    ptr => head
    
!<<    print *, "The list is: "
!<<    DO WHILE ( associated(ptr) )
!<<       print *, ptr%value
!<<       ptr => ptr%nextCont
!<<    END DO
!<<    print *
  END SUBROUTINE gigc_PrintDiags
   
!EOC
!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_AddField
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE gigc_AddField( FldNam, LongNam, Units, Dims, VLoc, RC )
!
! !INPUT PARAMETERS:
!
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: FldNam         ! Short name of diagnostic quantity
    CHARACTER(LEN=*), INTENT(IN) :: LongNam        ! 
    CHARACTER(LEN=*), INTENT(IN) :: Units          ! 
    CHARACTER(LEN=*), INTENT(IN) :: Dims           ! 
    CHARACTER(LEN=*), INTENT(IN) :: Vloc           ! 

!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    INTEGER,          INTENT(OUT), OPTIONAL :: RC         ! 0 = all is well
!
! !REVISION HISTORY: 
!  18 Dec 2014 - M. Long     - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE( DiagList ), POINTER :: next

    ALLOCATE(next)

    IF ( trim(Dims) .ne. 'XY'  .and. &
         trim(Dims) .ne. 'xy'  .and. &
         trim(Dims) .ne. 'XYZ' .and. &
         trim(Dims) .ne. 'xyz' )     &
    write(*,*) 'GIGC Diagnostic Error: Dimensions not formatted correctly. ', &
               ' Must be either XY or XYZ: ', trim(FldNam); return

    IF ( trim(Dims) .eq. 'XYZ' .or.  &
         trim(Dims) .eq. 'xyz' .and. &
         trim(VLoc) .ne. 'C'  .and.  &
         trim(VLoc) .ne. 'c'  .and.  &
         trim(VLoc) .ne. 'E'  .and.  &
         trim(VLoc) .ne. 'e' )       &
    write(*,*) 'GIGC Diagnostic Error: VLoc not formatted correctly. ', &
               ' Must be either C for center or E for edge: ', trim(FldNam); &
               return

    next%Fld%FldNam  = trim(FldNam)
    next%Fld%LongNam = trim(LongNam)
    next%Fld%Units   = trim(Units)
    next%Fld%Dims    = trim(Dims) 
    next%Fld%VLoc    = trim(VLoc)
    next%Fld%n_Diags = diags%Fld%n_Diags + 1

    write(*,*) 'Added diagnostic,'

    next%nextCont    => diags%nextCont
    diags%nextCont   => next

  END SUBROUTINE gigc_AddField

  SUBROUTINE gigc_MAPLOutField2d( DiagName, data2, RC )
    CHARACTER(LEN=*), INTENT(IN)  :: DiagName
    REAL*8,           INTENT(IN)  :: data2(:,:)
    INTEGER,          INTENT(OUT) :: RC
    REAL*8, POINTER               :: ptr(:,:)

    CALL MAPL_GetPointer ( DiagExport, ptr,  trim(DiagName) )

    ptr = data2

    NULLIFY(ptr)

    RC = DIAG_SUCCESS !

  END SUBROUTINE gigc_MAPLOutField2d

  SUBROUTINE gigc_MAPLOutField3d( DiagName, data3, RC )
    CHARACTER(LEN=*), INTENT(IN)  :: DiagName
    REAL*8,           INTENT(IN)  :: data3(:,:,:)
    INTEGER,          INTENT(OUT) :: RC
    REAL*8, POINTER               :: ptr(:,:,:)

    CALL MAPL_GetPointer ( DiagExport, ptr,  trim(DiagName) )

    ptr = data3

    NULLIFY(ptr)

    RC = DIAG_SUCCESS !

  END SUBROUTINE gigc_MAPLOutField3d

  SUBROUTINE gigc_MAPLRegisterDiagList( GC, RC )
    type(ESMF_GridComp), TARGET    :: GC            ! gridded component
    TYPE(DiagList), POINTER        :: c, n          ! Current & Next, for brevity
    INTEGER                        :: n_dim, n_vloc
    INTEGER, INTENT(OUT), OPTIONAL :: RC

    RC = DIAG_FAILURE

    c => diags
    DO WHILE (ASSOCIATED(c))
       n => c%nextCont
       IF (ASSOCIATED(c%Fld)) THEN
          ! Line up Field Info with MAPL params

          ! Dimensions
          IF (trim(c%Fld%Dims) .eq. 'XYZ' .or. trim(c%Fld%Dims) .eq. 'xyz') THEN
             n_dim = MAPL_DimsHorzVert
             ! Vertical Location (VLoc)
             IF (trim(c%Fld%VLoc) .eq. 'C' .or. trim(c%Fld%VLoc) .eq. 'c') THEN
                n_vloc = MAPL_VLocationCenter
             ELSEIF (trim(c%Fld%VLoc) .eq. 'E' .or. trim(c%Fld%VLoc) .eq. 'e') THEN
                n_vloc = MAPL_VLocationEdge
             ELSE
                write(*,*) 'Cannot identify VLoc: ',c%Fld%FldNam; return
             ENDIF
          ELSEIF (trim(c%Fld%Dims) .eq. 'XY' .or. trim(c%Fld%Dims) .eq. 'xy') THEN
             n_dim  = MAPL_DimsHorzOnly
             n_vloc = MAPL_VLocationNone
          ELSE
             write(*,*) 'Cannot identify Dims: ',c%Fld%FldNam; return
          ENDIF

          ! If all is well above, then register the Diagnostic
          call MAPL_AddExportSpec(GC,                    &
               SHORT_NAME         = trim(c%Fld%FldNam),  &
               LONG_NAME          = trim(c%Fld%LongNam), &
               UNITS              = trim(c%Fld%Units),   &
               DIMS               = n_dim,               &
               VLOCATION          = n_vloc,              &
               RC                 = RC                    )
       ENDIF
       c => n
    END DO

    localGC => GC

    RC = DIAG_SUCCESS

  END SUBROUTINE gigc_MAPLRegisterDiagList
!EOC
END MODULE GIGC_Diagnostics_Mod
