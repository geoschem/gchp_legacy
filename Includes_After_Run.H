!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !INCLUDE: Includes_After_Run.H
!
! !DESCRIPTION: This include file contains the array assignments that need
!  to be made BEFORE the call to the Run method of the 
!  GEOSCHEMchem\_GridCompMod.F90 code.  These array assignments take data out 
!  of the import and internal states and saves them into derived-type objects
!  that are used to pass information between GEOS-Chem and the ESMF interface.
!\\
!\\
!  These assignments were placed into this separate include file to avoid 
!  bogging down the GEOSCHEMchem\_GridCompMod.F90 module.
!\\
!\\
! !REVISION HISTORY: 
!  10 Oct 2012 - M. Long     - Initial version, for grid-independent model
!  10 Oct 2012 - R. Yantosca - Added ProTeX headers, updated comments
!  10 Oct 2012 - R. Yantosca - Converted to F90 free-format
!  10 Oct 2012 - R. Yantosca - Added Bromine species for v9-01-03
!  16 Oct 2012 - R. Yantosca - Changed GC_MET to State_Met
!  16 Oct 2012 - R. Yantosca - Changed GC_STATE to State_Chm
!  16 Oct 2012 - R. Yantosca - Changed GC_STATE%CSPEC to State_Chm%Species
!  16 Oct 2012 - R. Yantosca - Renamed to Includes_After_Run.H (freeform)
!  07 Mar 2013 - R. Yantosca - Now add error checks to prevent OOB errors
!  27 Oct 2014 - C. Keller   - Removed species copying into internal state.
!                              This is only required upon finalization.
!  08 May 2015 - C. Keller   - Now fill species on every (chemistry) time 
!                              step. This is critical for checkpointing.
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Pass advected tracers from GEOS-Chem tracers array to internal state
  !=========================================================================
  DO I = 1, SIZE(Int2Spc,1)
     IF ( Int2Spc(I)%ID <= 0 ) CYCLE
     IF ( .NOT. ASSOCIATED(Int2Spc(I)%Internal) ) THEN
        CYCLE
     ELSE
        Int2Spc(I)%Internal(:,:,:) = State_Chm%Species(:,:,LM:1:-1,Int2Spc(I)%ID) 
     ENDIF
  ENDDO
  State_Chm%Spc_Units = 'kg/kg total'

  !=========================================================================
  ! Various other archived variables needed in internal state. 
  !=========================================================================
  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'ARCHV_H2O2s', notFoundOK=.TRUE., __RC__ ) 
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(H2O2s) ) THEN
     Ptr3D = H2O2s
  ENDIF
  Ptr3D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'ARCHV_SO2s', notFoundOK=.TRUE., __RC__ ) 
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(SO2s) ) THEN
     Ptr3D = SO2s
  ENDIF
  Ptr3D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr2D, 'ARCHV_DRY_TOTN', notFoundOK=.TRUE., __RC__ ) 
  IF ( ASSOCIATED(Ptr2D) .AND. ASSOCIATED(DRY_TOTN) ) THEN
     Ptr2D = DRY_TOTN
  ENDIF
  Ptr2D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr2D, 'ARCHV_WET_TOTN', notFoundOK=.TRUE., __RC__ ) 
  IF ( ASSOCIATED(Ptr2D) .AND. ASSOCIATED(WET_TOTN) ) THEN
     Ptr2D = WET_TOTN
  ENDIF
  Ptr2D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'HSAVE_FOR_KPP' , notFoundOK=.TRUE., __RC__ ) 
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(HSAVE_KPP) ) THEN
     Ptr3D(:,:,1:LM-LLCHEM) = 0.0
     Ptr3D(:,:,LM:LM-LLCHEM+1:-1) = REAL(HSAVE_KPP(:,:,1:LLCHEM),KIND=ESMF_KIND_R4)
  ENDIF
  Ptr3D => NULL()

  ! Sesquiterpene concentrations. Archive internally in kgC/m3 (convert from kgC/box).
  IF ( ALLOCATED(ORVC_SESQ) ) THEN
     CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'SESQ_CARBDENS', notFoundOK=.TRUE., __RC__ ) 
     IF ( ASSOCIATED(Ptr3D) ) THEN 
        Ptr3D(:,:,LM:1:-1) = ORVC_SESQ(:,:,1:LM) / State_Met%AIRVOL(:,:,1:LM)
     ENDIF
     Ptr3D => NULL()
  ENDIF

!EOF
