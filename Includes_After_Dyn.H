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
  State_Chm%Tracers = State_Chm%Tracers( :, :, LM:1:-1, : )
  State_Chm%Species = State_Chm%Species( :, :, LM:1:-1, : )

!--  IND = Get_Indx( 'TRC_NO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_O3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_O3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_PAN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_PAN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_CO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_CO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_ALK4 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_ISOP = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HNO3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_H2O2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_ACET', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_ACET = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MEK', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MEK = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_ALD2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_RCHO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MVK', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MVK = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MACR', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MACR = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_PMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_PMN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_PPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_PPN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_R4N2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_PRPE = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_C3H8 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_CH2O = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_C2H6 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_N2O5 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HNO4 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MP = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_DMS', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_DMS = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_SO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_SO2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_SO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_SO4 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_SO4s = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MSA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MSA = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NH3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NH3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NH4 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NIT', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NIT = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NITs', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NITs = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_BCPI = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_OCPI = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_BCPO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_OCPO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_DST1', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_DST1 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_DST2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_DST2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_DST3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_DST3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_DST4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_DST4 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_SALA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_SALA = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_SALC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_SALC = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_Br2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_Br = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_BrO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_BrO = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HOBr = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HBr = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_BrNO2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_BrNO3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_CHBr3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_CH2Br2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_CH3Br = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MPN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_ISOPN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MOBA = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_PROPNN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HAC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HAC = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_GLYC = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MMN = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_RIP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_RIP = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_IEPOX = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_MAP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_MAP = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NO2 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_NO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_NO3 = State_Chm%Tracers(:,:,:,IND)
!--
!--  IND = Get_Indx( 'TRC_HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) TRC_HNO2 = State_Chm%Tracers(:,:,:,IND)
