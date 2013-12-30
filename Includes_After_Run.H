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
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Advected tracers and chemical species
  ! (Coge generated with Perl script gcIncAft)
  !=========================================================================

  ! Flip tracers & species in the vertical
!  State_Chm%Tracers = State_Chm%Tracers( :, :, LM:1:-1, : )
!  State_Chm%Species = State_Chm%Species( :, :, LM:1:-1, : )

  IND = Get_Indx( 'TRC_NO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_O3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_O3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_PAN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_PAN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_CO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_CO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_ALK4 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_ISOP = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HNO3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_H2O2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_ACET', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_ACET = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MEK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MEK = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_ALD2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_RCHO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MVK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MVK = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MACR', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MACR = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_PMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_PMN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_PPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_PPN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_R4N2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_PRPE = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_C3H8 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_CH2O = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_C2H6 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_N2O5 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HNO4 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MP = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_DMS', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_DMS = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_SO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_SO2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_SO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_SO4 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_SO4s = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MSA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MSA = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NH3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NH3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NH4 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NIT', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NIT = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NITs', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NITs = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_BCPI = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_OCPI = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_BCPO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_OCPO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_DST1', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_DST1 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_DST2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_DST2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_DST3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_DST3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_DST4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_DST4 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_SALA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_SALA = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_SALC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_SALC = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_Br2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_Br = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_BrO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_BrO = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HOBr = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HBr = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_BrNO2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_BrNO3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_CHBr3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_CH2Br2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_CH3Br = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MPN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_ISOPN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MOBA = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_PROPNN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HAC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HAC = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_GLYC = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MMN = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_RIP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_RIP = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_IEPOX = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_MAP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_MAP = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NO2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_NO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_NO3 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'TRC_HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) TRC_HNO2 = State_Chm%Tracers(:,:,:,IND)

  IND = Get_Indx( 'A3O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) A3O2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ACET', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ACET = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ACTA', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ACTA = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ALD2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ALD2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ALK4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ALK4 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ATO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ATO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'B3O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) B3O2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'Br', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) Br = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'Br2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) Br2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'BrNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) BrNO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'BrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) BrNO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'BrO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) BrO = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'C2H6', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) C2H6 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'C3H8', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) C3H8 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CH2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CH2O = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CH2Br2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CH2Br2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CH3Br', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CH3Br = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CH4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CH4 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CHBr3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CHBr3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'CO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) CO = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DMS', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DMS = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYBr2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYBr2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYBrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYBrNO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYCH2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYCH2O = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYH2O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYH2O2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYHBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYHBr = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYHNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYHNO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYHOBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYHOBr = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYN2O5', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYN2O5 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYNO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYPAN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYPAN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYPMN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYPMN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYPPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYPPN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYR4N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYR4N2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYMVK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYMVK = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'DRYMACR', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) DRYMACR = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'EMISSION', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) EMISSION = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'EOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) EOH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ETO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ETO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ETP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ETP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'GCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) GCO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'GLYC', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) GLYC = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'GLYX', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) GLYX = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'GP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) GP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'H', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) H = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'H2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) H2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'H2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) H2O = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'H2O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) H2O2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HAC', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HAC = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HBr = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HOBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HOBr = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HCOOH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HNO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HNO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HNO4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HNO4 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'HO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) HO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'IALD', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) IALD = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'IAO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) IAO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'IAP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) IAP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'INO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) INO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'INPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) INPN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ISN1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ISN1 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ISNP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ISNP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ISOP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ISOP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'KO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) KO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'M', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) M = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MACR', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MACR = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MAN2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MAN2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MAO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MAO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MAOP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MAOP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MAP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MAP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MCO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MEK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MEK = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MGLY', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MGLY = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MOH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MRO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MRO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MRP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MRP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MSA', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MSA = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'MVK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) MVK = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) N2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'N2O5', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) N2O5 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'NH2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) NH2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'NH3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) NH3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'NO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) NO = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'NO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) NO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'NO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) NO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) O = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) O2 = State_Chm%Species(:,:,:,IND)

!  IND = Get_Indx( 'O3_ND', State_Chm%Spec_Id, State_Chm%Spec_Name )
!  IF ( IND > 0 ) O3_ND = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'OH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) OH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PAN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PAN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PMN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PMN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PPN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PRN1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PRN1 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PRPE', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PRPE = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'PRPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) PRPN = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'R4N1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) R4N1 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'R4N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) R4N2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'R4O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) R4O2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'R4P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) R4P = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RA3P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RA3P = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RB3P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RB3P = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RCHO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RCHO = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RCO3 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RCOOH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RIO1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RIO1 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RIO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RIO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RIP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RIP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'ROH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) ROH = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'RP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) RP = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'SO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) SO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'SO4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) SO4 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'VRO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) VRO2 = State_Chm%Species(:,:,:,IND)

  IND = Get_Indx( 'VRP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) VRP = State_Chm%Species(:,:,:,IND)

  !=========================================================================
  ! Diagnostics, leave commented out for now 
  !=========================================================================

!       !-------------------------------------------------------------------
!       ! Put mean OH lifetime parameters back into the internal state
!       !
!       ! NOTE: D_AIR_MASS and D_OH_MASS are REAL*4, so we need to make
!       ! sure that we do not exceed 1e38, which is the max allowable value.
!       ! Therefore, D_AIR_MASS and D_OH_MASS will store the values divided
!       ! by 1d20 to prevent this overflow situation.
!       !-------------------------------------------------------------------
!       D_AIR_MASS     =  airMassDiag( :,:,: ) / OH_SCALE
!       D_OH_MASS      =  ohMassDiag ( :,:,: ) / OH_SCALE
!
!!       !-------------------------------------------------------------------
!!       ! Put wet deposition losses into the export state
!!       !-------------------------------------------------------------------
!!       WD_HNO3       = wdLossDiag( :,:,:, get_spc_indx('HNO3 )    
!!       WD_H2O2       = wdLossDiag( :,:,:, get_spc_indx('H2O2 )   
!!       WD_CH2O       = wdLossDiag( :,:,:, get_spc_indx('CH2O )   
!!       WD_MP         = wdLossDiag( :,:,:, get_spc_indx('MP   )   
!!       WD_SO2        = wdLossDiag( :,:,:, get_spc_indx('SO2  )   
!!       WD_SO4        = wdLossDiag( :,:,:, get_spc_indx('SO4  )   
!!       WD_SO4s       = wdLossDiag( :,:,:, get_spc_indx('SO4s )   
!!       WD_MSA        = wdLossDiag( :,:,:, get_spc_indx('MSA  )   
!!       WD_NH3        = wdLossDiag( :,:,:, get_spc_indx('NH3  )   
!!       WD_NH4        = wdLossDiag( :,:,:, get_spc_indx('NH4  )   
!!       WD_NIT        = wdLossDiag( :,:,:, get_spc_indx('NIT  )   
!!       WD_NITs       = wdLossDiag( :,:,:, get_spc_indx('NITs )   
!!       WD_BCPI       = wdLossDiag( :,:,:, get_spc_indx('BCPI )   
!!       WD_OCPI       = wdLossDiag( :,:,:, get_spc_indx('OCPI )   
!!       WD_BCPO       = wdLossDiag( :,:,:, get_spc_indx('BCPO )   
!!       WD_OCPO       = wdLossDiag( :,:,:, get_spc_indx('OCPO )   
!!       WD_ALPH       = wdLossDiag( :,:,:, get_spc_indx('ALPH )   
!!       WD_LIMO       = wdLossDiag( :,:,:, get_spc_indx('LIMO )   
!!       WD_ALCO       = wdLossDiag( :,:,:, get_spc_indx('ALCO )   
!!       WD_SOG1       = wdLossDiag( :,:,:, get_spc_indx('SOG1 )   
!!       WD_SOG2       = wdLossDiag( :,:,:, get_spc_indx('SOG2 )   
!!       WD_SOG3       = wdLossDiag( :,:,:, get_spc_indx('SOG3 )   
!!       WD_SOG4       = wdLossDiag( :,:,:, get_spc_indx('SOG4 )   
!!       WD_SOA1       = wdLossDiag( :,:,:, get_spc_indx('SOA1 )   
!!       WD_SOA2       = wdLossDiag( :,:,:, get_spc_indx('SOA2 )   
!!       WD_SOA3       = wdLossDiag( :,:,:, get_spc_indx('SOA3 )   
!!       WD_SOA4       = wdLossDiag( :,:,:, get_spc_indx('SOA4 )   
!!       WD_DST1       = wdLossDiag( :,:,:, get_spc_indx('DST1 )   
!!       WD_DST2       = wdLossDiag( :,:,:, get_spc_indx('DST2 )   
!!       WD_DST3       = wdLossDiag( :,:,:, get_spc_indx('DST3 )   
!!       WD_DST4       = wdLossDiag( :,:,:, get_spc_indx('DST4 )   
!!       WD_SALA       = wdLossDiag( :,:,:, get_spc_indx('SALA )   
!!       WD_SALC       = wdLossDiag( :,:,:, get_spc_indx('SALC )   
!!
!!       !-------------------------------------------------------------------
!!       ! Put dry deposition fluxes into the export state
!!       !-------------------------------------------------------------------
!!       DD_FX_NOx ( I, J )             = ddFluxDiag( get_spc_indx('NOx  )     
!!       DD_FX_O3  ( I, J )             = ddFluxDiag( get_spc_indx('Ox   ) 
!!       DD_FX_PAN ( I, J )             = ddFluxDiag( get_spc_indx('PAN  ) 
!!       DD_FX_HNO3( I, J )             = ddFluxDiag( get_spc_indx('HNO3 ) 
!!       DD_FX_H2O2( I, J )             = ddFluxDiag( get_spc_indx('H2O2 ) 
!!       DD_FX_N2O5( I, J )             = ddFluxDiag( get_spc_indx('N2O5 ) 
!!       DD_FX_PPN ( I, J )             = ddFluxDiag( get_spc_indx('PPN  ) 
!!       DD_FX_R4N2( I, J )             = ddFluxDiag( get_spc_indx('R4N2 ) 
!!       DD_FX_CH2O( I, J )             = ddFluxDiag( get_spc_indx('CH2O ) 
!!       DD_FX_SO2 ( I, J )             = ddFluxDiag( get_spc_indx('SO2  ) 
!!       DD_FX_SO4 ( I, J )             = ddFluxDiag( get_spc_indx('SO4  ) 
!!       DD_FX_SO4S( I, J )             = ddFluxDiag( get_spc_indx('SO4S ) 
!!       DD_FX_MSA ( I, J )             = ddFluxDiag( get_spc_indx('MSA  ) 
!!       DD_FX_NH3 ( I, J )             = ddFluxDiag( get_spc_indx('NH3  ) 
!!       DD_FX_NH4 ( I, J )             = ddFluxDiag( get_spc_indx('NH4  ) 
!!       DD_FX_NIT ( I, J )             = ddFluxDiag( get_spc_indx('NIT  ) 
!!       DD_FX_NITS( I, J )             = ddFluxDiag( get_spc_indx('NITS ) 
!!       DD_FX_BCPI( I, J )             = ddFluxDiag( get_spc_indx('BCPI ) 
!!       DD_FX_OCPI( I, J )             = ddFluxDiag( get_spc_indx('BCPO ) 
!!       DD_FX_BCPO( I, J )             = ddFluxDiag( get_spc_indx('OCPI ) 
!!       DD_FX_OCPO( I, J )             = ddFluxDiag( get_spc_indx('OCPO ) 
!!       DD_FX_ALPH( I, J )             = ddFluxDiag( get_spc_indx('ALPH ) 
!!       DD_FX_LIMO( I, J )             = ddFluxDiag( get_spc_indx('LIMO ) 
!!       DD_FX_ALCO( I, J )             = ddFluxDiag( get_spc_indx('ALCO ) 
!!       DD_FX_SOG1( I, J )             = ddFluxDiag( get_spc_indx('SOG1 ) 
!!       DD_FX_SOG2( I, J )             = ddFluxDiag( get_spc_indx('SOG2 ) 
!!       DD_FX_SOG3( I, J )             = ddFluxDiag( get_spc_indx('SOG3 ) 
!!       DD_FX_SOG4( I, J )             = ddFluxDiag( get_spc_indx('SOG4 ) 
!!       DD_FX_SOA1( I, J )             = ddFluxDiag( get_spc_indx('SOA1 ) 
!!       DD_FX_SOA2( I, J )             = ddFluxDiag( get_spc_indx('SOA2 ) 
!!       DD_FX_SOA3( I, J )             = ddFluxDiag( get_spc_indx('SOA3 ) 
!!       DD_FX_SOA4( I, J )             = ddFluxDiag( get_spc_indx('SOA4 ) 
!!       DD_FX_DST1( I, J )             = ddFluxDiag( get_spc_indx('DST1 ) 
!!       DD_FX_DST2( I, J )             = ddFluxDiag( get_spc_indx('DST2 ) 
!!       DD_FX_DST3( I, J )             = ddFluxDiag( get_spc_indx('DST3 ) 
!!       DD_FX_DST4( I, J )             = ddFluxDiag( get_spc_indx('DST4 ) 
!!       DD_FX_SALA( I, J )             = ddFluxDiag( get_spc_indx('SALA ) 
!!       DD_FX_SALC( I, J )             = ddFluxDiag( get_spc_indx('SALC ) 
!!       
!!       !-------------------------------------------------------------------
!!       ! Put dry deposition frequencies into the export state
!!       !-------------------------------------------------------------------
!!       DD_FQ_NOx ( I, J )             = ddFreqDiag( get_spc_indx('NOx  )     
!!       DD_FQ_O3  ( I, J )             = ddFreqDiag( get_spc_indx('Ox   ) 
!!       DD_FQ_PAN ( I, J )             = ddFreqDiag( get_spc_indx('PAN  ) 
!!       DD_FQ_HNO3( I, J )             = ddFreqDiag( get_spc_indx('HNO3 ) 
!!       DD_FQ_H2O2( I, J )             = ddFreqDiag( get_spc_indx('H2O2 ) 
!!       DD_FQ_N2O5( I, J )             = ddFreqDiag( get_spc_indx('N2O5 ) 
!!       DD_FQ_PPN ( I, J )             = ddFreqDiag( get_spc_indx('PPN  ) 
!!       DD_FQ_R4N2( I, J )             = ddFreqDiag( get_spc_indx('R4N2 ) 
!!       DD_FQ_CH2O( I, J )             = ddFreqDiag( get_spc_indx('CH2O ) 
!!       DD_FQ_SO2 ( I, J )             = ddFreqDiag( get_spc_indx('SO2  ) 
!!       DD_FQ_SO4 ( I, J )             = ddFreqDiag( get_spc_indx('SO4  ) 
!!       DD_FQ_SO4S( I, J )             = ddFreqDiag( get_spc_indx('SO4S ) 
!!       DD_FQ_MSA ( I, J )             = ddFreqDiag( get_spc_indx('MSA  ) 
!!       DD_FQ_NH3 ( I, J )             = ddFreqDiag( get_spc_indx('NH3  ) 
!!       DD_FQ_NH4 ( I, J )             = ddFreqDiag( get_spc_indx('NH4  ) 
!!       DD_FQ_NIT ( I, J )             = ddFreqDiag( get_spc_indx('NIT  ) 
!!       DD_FQ_NITS( I, J )             = ddFreqDiag( get_spc_indx('NITS ) 
!!       DD_FQ_BCPI( I, J )             = ddFreqDiag( get_spc_indx('BCPI ) 
!!       DD_FQ_OCPI( I, J )             = ddFreqDiag( get_spc_indx('BCPO ) 
!!       DD_FQ_BCPO( I, J )             = ddFreqDiag( get_spc_indx('OCPI ) 
!!       DD_FQ_OCPO( I, J )             = ddFreqDiag( get_spc_indx('OCPO ) 
!!       DD_FQ_ALPH( I, J )             = ddFreqDiag( get_spc_indx('ALPH ) 
!!       DD_FQ_LIMO( I, J )             = ddFreqDiag( get_spc_indx('LIMO ) 
!!       DD_FQ_ALCO( I, J )             = ddFreqDiag( get_spc_indx('ALCO ) 
!!       DD_FQ_SOG1( I, J )             = ddFreqDiag( get_spc_indx('SOG1 ) 
!!       DD_FQ_SOG2( I, J )             = ddFreqDiag( get_spc_indx('SOG2 ) 
!!       DD_FQ_SOG3( I, J )             = ddFreqDiag( get_spc_indx('SOG3 ) 
!!       DD_FQ_SOG4( I, J )             = ddFreqDiag( get_spc_indx('SOG4 ) 
!!       DD_FQ_SOA1( I, J )             = ddFreqDiag( get_spc_indx('SOA1 ) 
!!       DD_FQ_SOA2( I, J )             = ddFreqDiag( get_spc_indx('SOA2 ) 
!!       DD_FQ_SOA3( I, J )             = ddFreqDiag( get_spc_indx('SOA3 ) 
!!       DD_FQ_SOA4( I, J )             = ddFreqDiag( get_spc_indx('SOA4 ) 
!!       DD_FQ_DST1( I, J )             = ddFreqDiag( get_spc_indx('DST1 ) 
!!       DD_FQ_DST2( I, J )             = ddFreqDiag( get_spc_indx('DST2 ) 
!!       DD_FQ_DST3( I, J )             = ddFreqDiag( get_spc_indx('DST3 ) 
!!       DD_FQ_DST4( I, J )             = ddFreqDiag( get_spc_indx('DST4 ) 
!!       DD_FQ_SALA( I, J )             = ddFreqDiag( get_spc_indx('SALA ) 
!!       DD_FQ_SALC( I, J )             = ddFreqDiag( get_spc_indx('SALC )
!!
!!       !-------------------------------------------------------------------
!!       ! Put dry deposition velocities into the export state
!!       !-------------------------------------------------------------------
!!       DD_V_NOx ( I, J )              = ddVelDiag( get_spc_indx('NOx  )     
!!       DD_V_O3  ( I, J )              = ddVelDiag( get_spc_indx('Ox   ) 
!!       DD_V_PAN ( I, J )              = ddVelDiag( get_spc_indx('PAN  ) 
!!       DD_V_HNO3( I, J )              = ddVelDiag( get_spc_indx('HNO3 ) 
!!       DD_V_H2O2( I, J )              = ddVelDiag( get_spc_indx('H2O2 ) 
!!       DD_V_N2O5( I, J )              = ddVelDiag( get_spc_indx('N2O5 ) 
!!       DD_V_PPN ( I, J )              = ddVelDiag( get_spc_indx('PPN  ) 
!!       DD_V_R4N2( I, J )              = ddVelDiag( get_spc_indx('R4N2 ) 
!!       DD_V_CH2O( I, J )              = ddVelDiag( get_spc_indx('CH2O ) 
!!       DD_V_SO2 ( I, J )              = ddVelDiag( get_spc_indx('SO2  ) 
!!       DD_V_SO4 ( I, J )              = ddVelDiag( get_spc_indx('SO4  ) 
!!       DD_V_SO4S( I, J )              = ddVelDiag( get_spc_indx('SO4S ) 
!!       DD_V_MSA ( I, J )              = ddVelDiag( get_spc_indx('MSA  ) 
!!       DD_V_NH3 ( I, J )              = ddVelDiag( get_spc_indx('NH3  ) 
!!       DD_V_NH4 ( I, J )              = ddVelDiag( get_spc_indx('NH4  ) 
!!       DD_V_NIT ( I, J )              = ddVelDiag( get_spc_indx('NIT  ) 
!!       DD_V_NITS( I, J )              = ddVelDiag( get_spc_indx('NITS ) 
!!       DD_V_BCPI( I, J )              = ddVelDiag( get_spc_indx('BCPI ) 
!!       DD_V_OCPI( I, J )              = ddVelDiag( get_spc_indx('BCPO ) 
!!       DD_V_BCPO( I, J )              = ddVelDiag( get_spc_indx('OCPI ) 
!!       DD_V_OCPO( I, J )              = ddVelDiag( get_spc_indx('OCPO ) 
!!       DD_V_ALPH( I, J )              = ddVelDiag( get_spc_indx('ALPH ) 
!!       DD_V_LIMO( I, J )              = ddVelDiag( get_spc_indx('LIMO ) 
!!       DD_V_ALCO( I, J )              = ddVelDiag( get_spc_indx('ALCO ) 
!!       DD_V_SOG1( I, J )              = ddVelDiag( get_spc_indx('SOG1 ) 
!!       DD_V_SOG2( I, J )              = ddVelDiag( get_spc_indx('SOG2 ) 
!!       DD_V_SOG3( I, J )              = ddVelDiag( get_spc_indx('SOG3 ) 
!!       DD_V_SOG4( I, J )              = ddVelDiag( get_spc_indx('SOG4 ) 
!!       DD_V_SOA1( I, J )              = ddVelDiag( get_spc_indx('SOA1 ) 
!!       DD_V_SOA2( I, J )              = ddVelDiag( get_spc_indx('SOA2 ) 
!!       DD_V_SOA3( I, J )              = ddVelDiag( get_spc_indx('SOA3 ) 
!!       DD_V_SOA4( I, J )              = ddVelDiag( get_spc_indx('SOA4 ) 
!!       DD_V_DST1( I, J )              = ddVelDiag( get_spc_indx('DST1 ) 
!!       DD_V_DST2( I, J )              = ddVelDiag( get_spc_indx('DST2 ) 
!!       DD_V_DST3( I, J )              = ddVelDiag( get_spc_indx('DST3 ) 
!!       DD_V_DST4( I, J )              = ddVelDiag( get_spc_indx('DST4 ) 
!!       DD_V_SALA( I, J )              = ddVelDiag( get_spc_indx('SALA ) 
!!       DD_V_SALC( I, J )              = ddVelDiag( get_spc_indx('SALC )
!!
