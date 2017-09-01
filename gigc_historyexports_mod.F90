#if defined (ESMF_)
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_historyexports_mod.F90
!
! !DESCRIPTION: Module GIGC\_HistoryExports\_Mod serves as the interface
! between the HISTORY configuration file, the GEOS-Chem State registry,
! and the ESMF Export State. (Add more later)
!\\
!\\
! !INTERFACE: 
!
MODULE GIGC_HistoryExports_Mod
!
! !USES:
!
#include "MAPL_Generic.h"
  USE ErrCode_Mod
  USE Precision_Mod
  USE MAPL_Mod

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HistoryExports_SetServices
  PUBLIC :: HistoryExports_SetDataPointers
  PUBLIC :: CopyGCStates2Exports
  PUBLIC :: Destroy_HistoryConfig 
!
! !PRIVATE:
!
  PRIVATE :: Init_HistoryConfig
  PRIVATE :: Init_HistoryExport
  PRIVATE :: Init_HistoryExportsList
  PRIVATE :: Append_HistoryExportsList
!
! !PUBLIC TYPES
!
  ! History Configuration Object 
  TYPE, PUBLIC :: HistoryConfigObj

     CHARACTER(LEN=255)                   :: ROOT ! TODO: needed?
     CHARACTER(LEN=255)                   :: ConfigFileName
     LOGICAL                              :: ConfigFileRead
     TYPE(HistoryExportsListObj), POINTER :: HistoryExportsList
 
 END TYPE HistoryConfigObj
!
! !PRIVATE TYPES
!
  ! History Exports Linked List
  TYPE :: HistoryExportsListObj

     TYPE(HistoryExportObj), POINTER :: head
     INTEGER                         :: numExports

  END TYPE HistoryExportsListObj

  ! History Export Object
  TYPE :: HistoryExportObj

     CHARACTER(LEN=255)              :: export_name 
     CHARACTER(LEN=255)              :: field_name  
     CHARACTER(LEN=255)              :: long_name  
     CHARACTER(LEN=255)              :: units       
     CHARACTER(LEN=255)              :: vloc ! TODO: use integer param instead
     INTEGER                         :: rank        
     LOGICAL                         :: isMet
     LOGICAL                         :: isChem
     LOGICAL                         :: isDiag
     TYPE(HistoryExportObj), POINTER :: next

     ! Pointers to ESMF Export and GEOS-Chem State
     ! TODO: for now, include all possible data types...int might not work
     REAL,     POINTER :: ExportData2d(:,:)
     REAL,     POINTER :: ExportData3d(:,:,:)
     REAL(fp), POINTER :: GCStateData0d         
     REAL(fp), POINTER :: GCStateData1d(:)      
     REAL(fp), POINTER :: GCStateData2d(:,:)     
     REAL(fp), POINTER :: GCStateData3d(:,:,:)   
     REAL(f4), POINTER :: GCStateData0d_4        
     REAL(f4), POINTER :: GCStateData1d_4(:)     
     REAL(f4), POINTER :: GCStateData2d_4(:,:)   
     REAL(f4), POINTER :: GCStateData3d_4(:,:,:) 
     INTEGER,  POINTER :: GCStateData0d_I        
     INTEGER,  POINTER :: GCStateData1d_I(:)     
     INTEGER,  POINTER :: GCStateData2d_I(:,:)   
     INTEGER,  POINTER :: GCStateData3d_I(:,:,:) 

  END TYPE HistoryExportObj
!
! !PRIVATE VARIABLES
!
  INTEGER, PARAMETER    :: FIRSTCOL_HISTORY = 1
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren   -  Initial version
!EOP
!------------------------------------------------------------------------------
!BOC

CONTAINS
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_HistoryConfig 
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Init_HistoryConfig ( am_I_Root, HistoryConfig, ConfigFile, &
                                  RC )
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN) :: am_I_Root
    CHARACTER(LEN=*),    INTENT(IN) :: ConfigFile
!
! !OUTPUT PARAMETERS:
!
    TYPE(HistoryConfigObj), POINTER :: HistoryConfig
    INTEGER, INTENT(OUT)            :: RC 
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
    __Iam__('Init_HistoryConfig (gigc_diagnostics_mod.F90)')
    ALLOCATE(HistoryConfig)
    HistoryConfig%ROOT               =  ''
    HistoryConfig%ConfigFileName     =  TRIM(ConfigFile)
    HistoryConfig%ConfigFileRead     =  .FALSE.
    CALL Init_HistoryExportsList( am_I_Root, HistoryConfig, RC )
    ASSERT_( RC == GC_SUCCESS )

  END SUBROUTINE Init_HistoryConfig
!EOC
!------------------------------------------------------------------------------
!              
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_HistoryExportsList 
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Init_HistoryExportsList ( am_I_Root, HistoryConfig, RC )
!
! !USES:
!
    USE CHARPAK_MOD,    ONLY: STRSPLIT
    USE FILE_MOD,       ONLY : IOERROR
    USE INQUIREMOD,     ONLY : findFreeLUN
    USE State_Met_Mod,  ONLY : Get_State_Met_Info
    !USE State_Chem_Mod, ONLY : Get_State_Chem_Info ! TODO: implement this
    !USE State_Diag_Mod, ONLY : Get_State_Diag_Info ! TODO: implement this
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN)    :: am_I_Root
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HistoryConfigObj), POINTER :: HistoryConfig
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)   :: RC
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren - initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER               :: N, N_LAST, IU_GEOS, Rank, IOS
    CHARACTER(LEN=255)    :: LINE, SUBSTRS(500), SUBSTR
    CHARACTER(LEN=255)    :: ExportName, FieldName, State
    CHARACTER(LEN=255)    :: desc, units, vloc
    LOGICAL               :: isMet, isChem, isDiag, EOF
    TYPE(HistoryExportObj),  POINTER :: NewHistExp

    ! ================================================================
    ! Init_HistoryExportsList begins here
    ! ================================================================
    __Iam__('Init_HistoryExportsList (gigc_diagnostics_mod.F90)')

    ! Init
    desc   = ''
    units  = ''
    Rank   = 0
    vloc   = '' ! TODO: use integer param instead. define in registry_mod.
    isMet  = .FALSE.
    isChem = .FALSE.
    isDiag = .FALSE.
    EOF = .FALSE.
    NewHistExp => NULL()

    ! Create HistoryExportsList object
    ALLOCATE(HistoryConfig%HistoryExportsList)
    HistoryConfig%HistoryExportsList%numExports = 0
    HistoryConfig%HistoryExportsList%head => NULL()

    ! Open file and read line
    IU_GEOS = findFreeLun()
    OPEN( IU_GEOS, FILE=HistoryConfig%ConfigFileName, STATUS='OLD', &
          IOSTAT=IOS )
    IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, Iam // ':1' )
    READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE
    IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, Iam // ':2' )

    ! Loop over lines in file
    DO
       ! Read and split line
       READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE
       EOF = IOS < 0
       IF ( EOF ) RETURN

       ! Skip lines that do not contain gridded component name
       IF ( INDEX( LINE, 'GIGCchem' ) .le. 0 ) CYCLE

       ! For now, skip chem species since in internal state
       IF ( INDEX(LINE, 'CHEM_SPC_') > 0 ) CYCLE

       ! Get export name, state, and state variable name from line
       ! TO DO: This needs to be improved to work with all cases!!!!
       !        Perhaps the history read in GCC addresses this?
       CALL STRSPLIT( LINE(FIRSTCOL_HISTORY:), ' ', SUBSTRS, N )
       IF ( SUBSTRS(1) .eq. '#' ) CYCLE
       IF ( INDEX(LINE, '.fields') .le. 0 ) THEN
          SubStr = TRIM(SUBSTRS(1))
       ELSE 
          SubStr = TRIM(SUBSTRS(2))
       ENDIF
       CALL STRSPLIT( SubStr, "'", SUBSTRS, N )
       ExportName=SUBSTRS(1)
       CALL STRSPLIT( ExportName, "_", SUBSTRS, N )
       State=SUBSTRS(1)
       FieldName=SUBSTRS(2)

       ! Get field info from the relevant GC State module
       IF ( TRIM(State) == 'MET' ) THEN
          isMet = .TRUE.
          CALL Get_State_Met_Info( am_I_Root,   FieldName, desc=desc,   &
                                   units=units, Rank=Rank, vloc=vloc,   &
                                   RC=RC )
       ELSEIF ( TRIM(State) == 'CHEM' ) THEN
          isChem = .TRUE.
          ! TODO: State_Chem subroutine not yet created
          !CALL Get_State_Chem_Info( am_I_Root,   FieldName,  desc=desc,   &
          !                          units=units, Rank=Rank,  vloc=vloc,   &
          !                          isMet=.False., isChem=.True., &
          !                          isDiag=.FALSE., RC=RC )
       ELSEIF ( TRIM(State) == 'DIAG' ) THEN
          isDiag = .TRUE.
          ! TODO: State_Diag subroutine not yet created
          !CALL Get_State_Diag_Info( am_I_Root,   FieldName,  desc=desc,   &
          !                          units=units, Rank=Rank,   &
          !                          vloc=vloc,  isMet=.False., &
          !                          isChem=.False.,&
          !                          isDiag=.TRUE., RC=RC )
       ENDIF
       ASSERT_( RC == GC_SUCCESS )

       ! Create a new HistoryExport object
       CALL Init_HistoryExport( am_I_Root, NewHistExp,  &
                                export_name=ExportName, &
                                field_name=FieldName, &
                                long_name=desc,       &
                                units=units,          &
                                vloc=vloc,            &
                                rank=Rank,            &
                                isMet=isMet,          &
                                isChem=isChem,        &
                                isDiag=isDiag,        &
                                RC=RC )
       ASSERT_( RC == GC_SUCCESS )

       CALL Append_HistoryExportsList( am_I_Root,     NewHistExp, &
                                       HistoryConfig, RC       )
       ASSERT_( RC == GC_SUCCESS )

    ENDDO
    HistoryConfig%ConfigFileRead = .TRUE.

  END SUBROUTINE Init_HistoryExportsList
!EOC
!------------------------------------------------------------------------------
!              
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_HistoryExport 
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Init_HistoryExport ( am_I_Root,  NewHistExp, export_name,  &
                                  field_name, long_name,  units,        &
                                  vloc,       rank,       isMet,        &
                                  isChem,     isDiag,     RC  )
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN)    :: am_I_Root
!
! !OUTPUT PARAMETERS:
!
    TYPE(HistoryExportObj), POINTER :: NewHistExp
    CHARACTER(LEN=*), OPTIONAL      :: export_name
    CHARACTER(LEN=*), OPTIONAL      :: field_name
    CHARACTER(LEN=*), OPTIONAL      :: long_name
    CHARACTER(LEN=*), OPTIONAL      :: units
    CHARACTER(LEN=*), OPTIONAL      :: vloc 
    INTEGER,          OPTIONAL      :: rank
    LOGICAL,          OPTIONAL      :: isMet
    LOGICAL,          OPTIONAL      :: isChem
    LOGICAL,          OPTIONAL      :: isDiag
    INTEGER,          OPTIONAL      :: RC
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
    __Iam__('Init_HistoryExport (gigc_diagnostics_mod.F90)')
    ALLOCATE(NewHistExp)
    NewHistExp%export_name = TRIM(export_name)
    NewHistExp%field_name  = TRIM(field_name)
    NewHistExp%long_name   = TRIM(long_name)
    NewHistExp%units       = TRIM(units)
    NewHistExp%vloc        = TRIM(vloc)  ! TODO: int param from registry_mod
    NewHistExp%rank        = rank
    NewHistExp%isMet       = isMet
    NewHistExp%isMet       = isChem
    NewHistExp%isMet       = isDiag
    NewHistExp%next          => NULL()
    NewHistExp%ExportData2d  => NULL()
    NewHistExp%ExportData3d  => NULL()
    NewHistExp%GCStateData0d => NULL()
    NewHistExp%GCStateData1d => NULL()
    NewHistExp%GCStateData2d => NULL()
    NewHistExp%GCStateData3d => NULL()
    NewHistExp%GCStateData0d_4 => NULL()
    NewHistExp%GCStateData1d_4 => NULL()
    NewHistExp%GCStateData2d_4 => NULL()
    NewHistExp%GCStateData3d_4 => NULL()
    NewHistExp%GCStateData0d_I => NULL()
    NewHistExp%GCStateData1d_I => NULL()
    NewHistExp%GCStateData2d_I => NULL()
    NewHistExp%GCStateData3d_I => NULL()

  END SUBROUTINE Init_HistoryExport
!EOC
!------------------------------------------------------------------------------
!              
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Append_HistoryExportsList 
!
! !DESCRIPTION: 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Append_HistoryExportsList ( am_I_Root,     HistoryExport, &
                                         HistoryConfig, RC        )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN)    :: am_I_Root
    TYPE(HistoryExportObj), POINTER :: HistoryExport
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HistoryConfigObj), POINTER :: HistoryConfig
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)   :: RC
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren - initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HistoryExportObj),  POINTER :: NewHistExp

    ! ================================================================
    ! Append_HistoryExportsList begins here
    ! ================================================================
    __Iam__('Append_HistoryExportsList (gigc_diagnostics_mod.F90)')

    ! Add new object to the beginning of the linked list
    HistoryExport%next => HistoryConfig%HistoryExportsList%head
    HistoryConfig%HistoryExportsList%head => HistoryExport

    ! Update # of list items
    HistoryConfig%HistoryExportsList%numExports = &
         HistoryConfig%HistoryExportsList%numExports + 1

  END SUBROUTINE Append_HistoryExportsList
!EOC
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HistoryExports_SetServices
!
  ! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HistoryExports_SetServices( am_I_Root, ConfigFile, GC,  &
                                         HistoryConfig,  RC )
!
! !USES:
!
    USE ESMF, ONLY : ESMF_GridComp
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN)    :: am_I_Root
    CHARACTER(LEN=*),    INTENT(IN)    :: ConfigFile
!
! !INPUT AND OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC             ! Gridded Component
!
! !OUTPUT PARAMETERS:
!
    TYPE(HistoryConfigObj), POINTER    :: HistoryConfig  ! History config object
    INTEGER,             INTENT(OUT)   :: RC
!
! !REMARKS:
!  !
! !REVISION HISTORY: 
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HistoryExportObj),      POINTER :: current

    ! ================================================================
    ! HistoryExports_SetServices begins here
    ! ================================================================

    ! For MAPL/ESMF error handling (defines Iam and STATUS)
    __Iam__('HistoryExports_SetServices (gigc_diagnostics_mod.F90)')

    ! Create a config object if it does not already exist
    IF ( .NOT. ASSOCIATED(HistoryConfig) ) THEN
       CALL Init_HistoryConfig( am_I_Root, HistoryConfig, ConfigFile, RC )
    ENDIF

    ! TODO: Check if file already read?

    ! Loop over the History Exports list to add one export per item
    current => HistoryConfig%HistoryExportsList%head
    DO WHILE ( ASSOCIATED( current ) )
    
       ! Create an export for this item
       ! TODO: 
       !  (1) issue with adding vars with level edge
       !  (2) remove the vloc string. Use an integer param from registry
       !      (to be implemented)
       !  (3) check that var not already an export
       !  (4) determine behavior if the same var in multi history collections
       !      (currently seems to not work)
       IF ( current%rank == 3 .AND. TRIM(current%vloc) == 'CENTER' ) then
          CALL MAPL_AddExportSpec(GC,                                  &
             SHORT_NAME         = TRIM(current%export_name),           &
             LONG_NAME          = TRIM(current%long_name),             &
             UNITS              = TRIM(current%units),                 &
             DIMS               = MAPL_DimsHorzVert,                   &
             VLOCATION          = MAPL_VLocationCenter,                &
             RC                 = STATUS                              )
       ELSEIF ( current%rank == 3 .AND. TRIM(current%vloc) == 'EDGE' ) then
          CALL MAPL_AddExportSpec(GC,                                  &
             SHORT_NAME         = TRIM(current%export_name),           &
             LONG_NAME          = TRIM(current%long_name),             &
             UNITS              = TRIM(current%units),                 &
             DIMS               = MAPL_DimsHorzVert,                   &
             VLOCATION          = MAPL_VLocationEdge,                  &
             RC                 = STATUS                              )
       ELSEIF ( current%rank == 2 ) THEN
          CALL MAPL_AddExportSpec(GC,                                  &
             SHORT_NAME         = TRIM(current%export_name),           &
             LONG_NAME          = TRIM(current%long_name),             &
             UNITS              = TRIM(current%units),                 &
             DIMS               = MAPL_DimsHorzOnly,                   &
             RC                 = STATUS                              )
       ENDIF
       VERIFY_(STATUS)

       ! debugging
       IF (am_i_root) PRINT *, "Added export for history variable: " &
                                  // TRIM(current%export_name)

       current => current%next    
    ENDDO
    current => NULL()
    
  END SUBROUTINE HistoryExports_SetServices
!EOC
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: CopyGCStates2Exports
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE CopyGCStates2Exports( am_I_Root, HistoryConfig, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN) :: am_I_Root
!
! !INPUT AND OUTPUT PARAMETERS:
!
    TYPE(HistoryConfigObj), POINTER :: HistoryConfig  ! History config object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,            INTENT(OUT) :: RC
!
! !REMARKS:
!  !
! !REVISION HISTORY: 
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HistoryExportObj), POINTER :: current

    ! ================================================================
    ! CopyGCStates2Exports begins here
    ! ================================================================
    __Iam__('CopyGCStates2Exports (gigc_diagnostics_mod.F90)')

    ! Loop over the History Exports list
    current => HistoryConfig%HistoryExportsList%head
    DO WHILE ( ASSOCIATED( current ) )

       IF ( current%rank == 2 ) THEN
          IF ( ASSOCIATED ( current%GCStateData2d ) ) THEN
             current%ExportData2d = current%GCStateData2d
          ELSEIF ( ASSOCIATED ( current%GCStateData2d_4 ) ) THEN
             current%ExportData2d = current%GCStateData2d_4
          ELSEIF ( ASSOCIATED ( current%GCStateData2d_4 ) ) THEN
             current%ExportData2d = current%GCStateData2d_I
          ENDIF
       ELSEIF ( current%rank == 3 ) THEN
          IF ( ASSOCIATED ( current%GCStateData3d ) ) THEN
             current%ExportData3d = current%GCStateData3d
          ELSEIF ( ASSOCIATED ( current%GCStateData3d_4 ) ) THEN
             current%ExportData3d = current%GCStateData3d_4
          ELSEIF ( ASSOCIATED ( current%GCStateData3d_I ) ) THEN
             current%ExportData3d = current%GCStateData3d_I
          ENDIF
       ENDIF

       current => current%next    
    ENDDO
    current => NULL()

    
  END SUBROUTINE CopyGCStates2Exports
!EOC
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HistoryExports_SetDataPointers
!
! !DESCRIPTION:
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HistoryExports_SetDataPointers( am_I_Root,     EXPORT,    &
                                             HistoryConfig, State_Chm, &
                                             State_Diag,    State_Met, &
                                             RC                     )
!
! !USES:
!
    USE ESMF, ONLY : ESMF_State
    USE State_Chm_Mod,    ONLY : ChmState, Lookup_State_Chm
    USE State_Diag_Mod,   ONLY : DgnState, Lookup_State_Diag
    USE State_Met_Mod,    ONLY : MetState, Get_State_Met_Info, Lookup_State_Met
!
! !INPUT PARAMETERS:
!
    LOGICAL,         INTENT(IN)     :: am_I_Root
!
! !INPUT AND OUTPUT PARAMETERS:
!
    TYPE(ESMF_State), INTENT(INOUT), TARGET :: EXPORT ! Export state obj
    TYPE(HistoryConfigObj), POINTER :: HistoryConfig  ! History config obj
    TYPE(ChmState),   INTENT(INOUT) :: State_Chm      ! Chemistry State obj
    TYPE(MetState),   INTENT(INOUT) :: State_Met      ! Meteorology State obj
    TYPE(DgnState),   INTENT(INOUT) :: State_Diag     ! Diagnostics State obj
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC
!
! !REMARKS:
!  !
! !REVISION HISTORY: 
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HistoryExportObj), POINTER :: current

    ! ================================================================
    ! HistoryExports_SetDataPointers begins here
    ! ================================================================
    __Iam__('HistoryExports_SetDataPointers (gigc_diagnostics_mod.F90)')

    ! ewl debugging
    IF ( .NOT. ASSOCIATED(HistoryConfig) ) THEN
       if (am_i_root) then
          print *, "ERROR! HistoryConfig not associated!"
          RETURN
       endif
    ENDIF

    ! Loop over the History Exports list
    current => HistoryConfig%HistoryExportsList%head
    if (am_i_root) print *, "ewl: current name: ", TRIM(current%export_name)
    DO WHILE ( ASSOCIATED( current ) )

       ! Get pointer to GC state data (for now, assume state_met)
       CALL Lookup_State_Met( am_I_Root = am_I_Root,               &
                              State_Met = State_Met,               &
                              Variable  = current%field_name,      &
                              Ptr2d     = current%GCStateData2d,   &
                              Ptr2d_I   = current%GCStateData2d_I, &
                              Ptr3d     = current%GCStateData3d,   &
                              Ptr3d_I   = current%GCStateData3d_I, &
                              RC        = RC                      )
       ASSERT_( RC == GC_SUCCESS )

       ! ewl debugging
       if ( am_I_root ) then
          print *, "ewl: got pointer from state_met registry for ", &
               current%export_name
          if ( allocated(current%GCStateData2d)) then
             print *, current%GCStateData2d(20,:)
          elseif (allocated(current%GCStateData3d)) then
             print *, current%GCStateData3d(20,20,:)
          endif
       endif

       ! For MAPL export, need to pass a pointer of the right dimension
       IF ( current%rank == 2 ) THEN
          CALL MAPL_GetPointer ( EXPORT, current%ExportData2d, &
                                 current%export_name, __RC__ )
       ELSEIF ( current%rank == 3 ) THEN
          CALL MAPL_GetPointer ( EXPORT, current%ExportData3d, &
                                 current%export_name, __RC__ )
       ENDIF

       current => current%next    
    ENDDO
    current => NULL()
    
  END SUBROUTINE HistoryExports_SetDataPointers
!EOC
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Destroy_HistoryConfig 
!
! !DESCRIPTION: Subroutine Destroy_HistoryConfig deallocates a HistoryConfig
!  object and all of its member objects including the linked list of
!  HistoryExport objects.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Destroy_HistoryConfig ( am_I_Root, HistoryConfig, RC )
!
! !INPUT PARAMETERS:
!
    LOGICAL,            INTENT(IN)    :: am_I_Root     ! root CPU?
    TYPE(HistoryConfigObj), POINTER   :: HistoryConfig
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,            INTENT(INOUT) :: RC            ! Success?
!
! !REVISION HISTORY:
!  01 Sep 2017 - E. Lundgren - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HistoryExportObj), POINTER :: current
    TYPE(HistoryExportObj), POINTER :: next

    ! ================================================================
    ! Destroy_HistoryConfig begins here
    ! ================================================================
    __Iam__('Destroy_HistoryConfig (gigc_diagnostics_mod.F90)')

    current => NULL()
    next => NULL()

    ! Destroy each item in the linked list of HistoryExport objects
    current => HistoryConfig%HistoryExportsList%head
    IF ( ASSOCIATED( current ) ) next => current%next
    DO WHILE ( ASSOCIATED( current ) )
       DEALLOCATE( current, STAT=RC )
       ASSERT_( RC == GC_SUCCESS )
       IF ( .NOT. ASSOCIATED ( next ) ) EXIT
       current => next
       next => current%next
    ENDDO

    ! Deallocate the HistoryExportsList object
    DEALLOCATE( HistoryConfig%HistoryExportsList, STAT=RC )
    ASSERT_( RC == GC_SUCCESS )

    ! Deallocate the HistoryConfig object
    DEALLOCATE( HistoryConfig, STAT=RC )
    ASSERT_( RC == GC_SUCCESS )

    ! Final cleanup
    current => NULL()
    next    => NULL()

  END SUBROUTINE Destroy_HistoryConfig
!EOC
END MODULE GIGC_HistoryExports_Mod
#endif
