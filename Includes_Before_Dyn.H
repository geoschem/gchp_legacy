!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !INCLUDE: Includes_Before_Run.H
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
!  16 Oct 2012 - R. Yantosca - Renamed to Includes_Before_Run.H (freeform)
!  18 Oct 2012 - R. Yantosca - Add proper unit conversions for met fields
!  23 Oct 2012 - R. Yantosca - Now add QI, QL fields to State_Met object
!  28 Nov 2012 - R. Yantosca - Now populate State_Met%SUNCOS fields from ZTH
!  13 Feb 2013 - R. Yantosca - Now get ZTH, SLR from MAPL
!  07 Mar 2013 - R. Yantosca - Now add error checks to prevent OOB errors
!  15 Mar 2013 - R. Yantosca - Now assign GCCTROPP, which is the last valid
!                              tropopause pressure, to State_Met%TROPP
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Two-dimensional fields
  !=========================================================================

  State_Met%PS1        = PS1                                 ! hPa
  State_Met%PS2        = PS2                                 ! hPa
  ! Interpolate pressure [hPa] to the end of the dynamic timestep
  State_Met%PSC2 = State_Met%PS1 + ( State_Met%PS2 -  State_Met%PS1 ) * TC2 

  !=========================================================================
  ! Three-dimensional fields on level centers
  !=========================================================================
  DO L = 1, LM    

     ! Reversed level index
     LR = LM - L + 1

     DO J = 1, JM
     DO I = 1, IM
     
        !-------------------------------------------------------------------
        ! "True" met fields (i.e. passed from GCM)
        !-------------------------------------------------------------------
!       State_Met%AREA_M2 (I,J,L)  = AREA   (I,J   )        ! m2
       State_Met%U (I,J,L)  = U(I,J,LR) 
       State_Met%V (I,J,L)  = V(I,J,LR) 

  ENDDO
  ENDDO
  ENDDO

  !=========================================================================
  ! Advected tracers and chemical species in the INTERNAL STATE
  ! (Code was generated with Perl script gcIncBef)
  !=========================================================================
!--  IND = Get_Indx( 'TRC_NO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO
!--
!--  IND = Get_Indx( 'TRC_O3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_O3
!--
!--  IND = Get_Indx( 'TRC_PAN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PAN
!--
!--  IND = Get_Indx( 'TRC_CO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CO
!--
!--  IND = Get_Indx( 'TRC_ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ALK4
!--
!--  IND = Get_Indx( 'TRC_ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ISOP
!--
!--  IND = Get_Indx( 'TRC_HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO3
!--
!--  IND = Get_Indx( 'TRC_H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_H2O2
!--
!--  IND = Get_Indx( 'TRC_ACET', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ACET
!--
!--  IND = Get_Indx( 'TRC_MEK', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MEK
!--
!--  IND = Get_Indx( 'TRC_ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ALD2
!--
!--  IND = Get_Indx( 'TRC_RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_RCHO
!--
!--  IND = Get_Indx( 'TRC_MVK', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MVK
!--
!--  IND = Get_Indx( 'TRC_MACR', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MACR
!--
!--  IND = Get_Indx( 'TRC_PMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PMN
!--
!--  IND = Get_Indx( 'TRC_PPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PPN
!--
!--  IND = Get_Indx( 'TRC_R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_R4N2
!--
!--  IND = Get_Indx( 'TRC_PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PRPE
!--
!--  IND = Get_Indx( 'TRC_C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_C3H8
!--
!--  IND = Get_Indx( 'TRC_CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH2O
!--
!--  IND = Get_Indx( 'TRC_C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_C2H6
!--
!--  IND = Get_Indx( 'TRC_N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_N2O5
!--
!--  IND = Get_Indx( 'TRC_HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO4
!--
!--  IND = Get_Indx( 'TRC_MP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MP
!--
!--  IND = Get_Indx( 'TRC_DMS', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DMS
!--
!--  IND = Get_Indx( 'TRC_SO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO2
!--
!--  IND = Get_Indx( 'TRC_SO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO4
!--
!--  IND = Get_Indx( 'TRC_SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO4s
!--
!--  IND = Get_Indx( 'TRC_MSA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MSA
!--
!--  IND = Get_Indx( 'TRC_NH3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NH3
!--
!--  IND = Get_Indx( 'TRC_NH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NH4
!--
!--  IND = Get_Indx( 'TRC_NIT', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NIT
!--
!--  IND = Get_Indx( 'TRC_NITs', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NITs
!--
!--  IND = Get_Indx( 'TRC_BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BCPI
!--
!--  IND = Get_Indx( 'TRC_OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_OCPI
!--
!--  IND = Get_Indx( 'TRC_BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BCPO
!--
!--  IND = Get_Indx( 'TRC_OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_OCPO
!--
!--  IND = Get_Indx( 'TRC_DST1', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST1
!--
!--  IND = Get_Indx( 'TRC_DST2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST2
!--
!--  IND = Get_Indx( 'TRC_DST3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST3
!--
!--  IND = Get_Indx( 'TRC_DST4', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST4
!--
!--  IND = Get_Indx( 'TRC_SALA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SALA
!--
!--  IND = Get_Indx( 'TRC_SALC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SALC
!--
!--  IND = Get_Indx( 'TRC_Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_Br2
!--
!--  IND = Get_Indx( 'TRC_Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_Br
!--
!--  IND = Get_Indx( 'TRC_BrO', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrO
!--
!--  IND = Get_Indx( 'TRC_HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HOBr
!--
!--  IND = Get_Indx( 'TRC_HBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HBr
!--
!--  IND = Get_Indx( 'TRC_BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrNO2
!--
!--  IND = Get_Indx( 'TRC_BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrNO3
!--
!--  IND = Get_Indx( 'TRC_CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CHBr3
!--
!--  IND = Get_Indx( 'TRC_CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH2Br2
!--
!--  IND = Get_Indx( 'TRC_CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH3Br
!--
!--  IND = Get_Indx( 'TRC_MPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MPN
!--
!--  IND = Get_Indx( 'TRC_ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ISOPN
!--
!--  IND = Get_Indx( 'TRC_MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MOBA
!--
!--  IND = Get_Indx( 'TRC_PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PROPNN
!--
!--  IND = Get_Indx( 'TRC_HAC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HAC
!--
!--  IND = Get_Indx( 'TRC_GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_GLYC
!--
!--  IND = Get_Indx( 'TRC_MMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MMN
!--
!--  IND = Get_Indx( 'TRC_RIP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_RIP
!--
!--  IND = Get_Indx( 'TRC_IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_IEPOX
!--
!--  IND = Get_Indx( 'TRC_MAP', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MAP
!--
!--  IND = Get_Indx( 'TRC_NO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO2
!--
!--  IND = Get_Indx( 'TRC_NO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO3
!--
!--  IND = Get_Indx( 'TRC_HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
!--  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO2
!--
!--  ! Flip tracers & species in the vertical
!--  State_Chm%Tracers = State_Chm%Tracers( :, :, LM:1:-1, : )
!--  State_Chm%Species = State_Chm%Species( :, :, LM:1:-1, : )

