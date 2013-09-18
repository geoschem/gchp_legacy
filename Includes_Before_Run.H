!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !INCLUDE: Includes_Before_Run.h
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
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Two-dimensional fields
  !=========================================================================

  State_Met%ALBD       = ALBVF                               ! unitless
  State_Met%CLDFRC     = CLDTT                               ! unitless
  State_Met%HFLUX      = SH                                  ! W/m2
  State_Met%FRCLND     = FRLAND                              ! unitless
  State_Met%GWETTOP    = WET1                                ! unitless
  State_Met%LWI        = LWI                                 ! unitless
  State_Met%PARDR      = DRPAR                               ! W/m2
  State_Met%PARDF      = DFPAR                               ! W/m2
  State_Met%PBLH       = ZPBL                                ! m
  State_Met%PRECCON    = CN_PRCP * 86400d0                   ! kg/m2/s -> mm/day
  State_Met%PRECTOT    = TPREC   * 86400d0                   ! kg/m2/s -> mm/day
  State_Met%TROPP      = TROPP   / 100d0                     ! Pa      -> hPa
  State_Met%TO3        = TO3                                 ! DU
  State_Met%TS         = TS                                  ! K
  State_Met%RADSWG     = SWNDSRF                             ! W/m2
  State_Met%SST        = TS                                  ! K
  State_Met%SUNCOS     = ZTH                                 ! unitless
  State_Met%SUNCOSmid  = ZTH                                 ! unitless
  State_Met%SUNCOSmid5 = ZTH                                 ! unitless
  State_Met%U10M       = U10M                                ! m/s
  State_Met%USTAR      = USTAR                               ! m/s
  State_Met%UVALBEDO   = ALBVF  !*** NOTE Need to populate w/ real UVALBEDO ***
  State_Met%V10M       = V10M                                ! m/s
  State_Met%Z0         = Z0H                                 ! m

!  State_Met%IREG      = IREG
!  State_Met%ILAND     = ILAND
!  State_Met%IUSE      = IUSE

 !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================
  DO L = 1, LM+1
     
     ! Reversed level index
     LR = LM - L + 1

     DO J = 1, JM
     DO I = 1, IM
        State_Met%PEDGE(I,J,L)     = PLE    (I,J,LR) / 1d2  ! Pa -> hPa 
        State_Met%CMFMC(I,J,L)     = CNV_MFC(I,J,LR)        ! kg/m2/s
     ENDDO
     ENDDO
  ENDDO

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
        State_Met%AIRDEN  (L,I,J)  = AIRDENS(I,J,LR)        ! kg/m3
        State_Met%AREA_M2 (I,J,L)  = AREA   (I,J   )        ! m2
        State_Met%CLDF    (L,I,J)  = FCLD   (I,J,LR)        ! unitless
        State_Met%DQIDTMST(I,J,L)  = DQIDT  (I,J,LR)        ! kg/kg/s
        State_Met%DQLDTMST(I,J,L)  = DQLDT  (I,J,LR)        ! kg/kg/s
        State_Met%DQVDTMST(I,J,L)  = DQDT   (I,J,LR)        ! kg/kg/s
        State_Met%DTRAIN  (I,J,L)  = CNV_MFD(I,J,LR)        ! kg/m2/s
        State_Met%MOISTQ  (L,I,J)  = DQDT   (I,J,LR)        ! 
        State_Met%QI      (I,J,L)  = QI     (I,J,LR)        ! kg/kg
        State_Met%QL      (I,J,L)  = QL     (I,J,LR)        ! kg/kg
        State_Met%RH      (I,J,L)  = RH2    (I,J,LR)        ! unitless
        State_Met%SPHU    (I,J,L)  = Q      (I,J,LR) * 1d3  ! kg/kg -> g/kg
        State_Met%T       (I,J,L)  = T      (I,J,LR)        ! K
        State_Met%TAUCLI  (I,J,L)  = TAUCLI (I,J,LR)        ! unitless
        State_Met%TAUCLW  (I,J,L)  = TAUCLW (I,J,LR)        ! unitless

        !-------------------------------------------------------------------
        ! Quantities derived from met fields
        ! NOTE: Need to populate MOISTQ eventually
        !-------------------------------------------------------------------

        ! Grid box height [m]
        State_Met%BXHEIGHT(I,J,L)  =   ZLE               (I,J,LR-1)   &
                                   -   ZLE               (I,J,LR  )

        ! Pressure thickness [hPa]
        State_Met%DELP    (L,I,J)  =   State_Met%PEDGE   (I,J,L   )   & 
                                   -   State_Met%PEDGE   (I,J,L+1 )

        ! Total optical depth [unitless]
        State_Met%OPTD    (L,I,J)  = ( State_Met%TAUCLI  (I,J,L   )   &
                                   +   State_Met%TAUCLW  (I,J,L   ) ) &
                                   *   State_Met%CLDF    (L,I,J   )

        ! Air mass in grid box [kg]
        State_Met%AD      (I,J,L)  =   State_Met%AIRDEN  (L,I,J   )   &
                                   *   AREA              (I,J     )   &
                                   *   State_Met%BXHEIGHT(I,J,L   )

        ! Grid box volume [m3]
        State_Met%AIRVOL  (I,J,L)  =   AREA              (I,J     )   &
                                   *   State_Met%BXHEIGHT(I,J,L   )
  ENDDO
  ENDDO
  ENDDO

  !=========================================================================
  ! Advected tracers and chemical species in the INTERNAL STATE
  ! (Code was generated with Perl script gcIncBef)
  !=========================================================================
   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_O3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_O3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_PAN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_PAN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_CO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_CO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_ALK4

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_ISOP

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HNO3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_H2O2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_ACET', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_ACET

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MEK', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MEK

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_ALD2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_RCHO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MVK', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MVK

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MACR', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MACR

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_PMN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_PMN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_PPN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_PPN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_R4N2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_PRPE

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_C3H8

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_CH2O

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_C2H6

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_N2O5

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HNO4

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MP', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MP

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_DMS', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_DMS

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_SO2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_SO2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_SO4', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_SO4

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_SO4s

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MSA', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MSA

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NH3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NH3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NH4', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NH4

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NIT', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NIT

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NITs', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NITs

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_BCPI

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_OCPI

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_BCPO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_OCPO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_DST1', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_DST1

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_DST2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_DST2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_DST3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_DST3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_DST4', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_DST4

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_SALA', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_SALA

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_SALC', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_SALC

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_Br2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_Br2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_Br', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_Br

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_BrO', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_BrO

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HOBr

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HBr', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HBr

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_BrNO2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_BrNO3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_CHBr3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_CH2Br2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_CH3Br

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MPN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MPN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_ISOPN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MOBA

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_PROPNN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HAC', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HAC

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_GLYC

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MMN', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MMN

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_RIP', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_RIP

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_IEPOX

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_MAP', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_MAP

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NO2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NO2

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_NO3', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_NO3

   State_Chm%Tracers( :, :, LM:1:-1, Get_Indx('TRC_HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name ) ) = TRC_HNO2

! --- 

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('A3O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = A3O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ACET', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ACET

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ACTA', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ACTA

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ALD2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ALD2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ALK4', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ALK4

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ATO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ATO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('B3O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = B3O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('Br', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = Br

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('Br2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = Br2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('BrNO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = BrNO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('BrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = BrNO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('BrO', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = BrO

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('C2H6', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = C2H6

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('C3H8', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = C3H8

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CH2O', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CH2O

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CH2Br2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CH2Br2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CH3Br', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CH3Br

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CH4', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CH4

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CHBr3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CHBr3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('CO', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = CO

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DMS', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DMS

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYBr2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYBr2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYBrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYBrNO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYCH2O', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYCH2O

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYH2O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYH2O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYHBr', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYHBr

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYHNO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYHNO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYHOBr', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYHOBr

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYN2O5', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYN2O5

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYNO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYNO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYPAN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYPAN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYPMN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYPMN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYPPN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYPPN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYR4N2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYR4N2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYMVK', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYMVK

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('DRYMACR', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = DRYMACR

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('EMISSION', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = EMISSION

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('EOH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = EOH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ETO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ETO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ETP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ETP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('GLYC', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = GLYC

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('GLYX', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = GLYX

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('H', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = H

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('H2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = H2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('H2O', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = H2O

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('H2O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = H2O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HAC', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HAC

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HBr', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HBr

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HOBr', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HOBr

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HCOOH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HNO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HNO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HNO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HNO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HNO4', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HNO4

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('HO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = HO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('IAP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = IAP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('INO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = INO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('INPN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = INPN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ISN1', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ISN1

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ISNP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ISNP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ISOP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ISOP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('KO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = KO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('M', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = M

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MACR', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MACR

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MAN2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MAN2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MAO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MAO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MAOP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MAOP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MAP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MAP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MCO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MCO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MEK', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MEK

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MGLY', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MGLY

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MOH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MOH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MRO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MRO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MRP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MRP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MSA', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MSA

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('MVK', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = MVK

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('N2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = N2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('N2O5', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = N2O5

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('NH2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = NH2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('NH3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = NH3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('NO', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = NO

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('NO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = NO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('NO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = NO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('O', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = O

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('O3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = O3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('OH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = OH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PAN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PAN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PMN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PMN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PPN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PPN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PRN1', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PRN1

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PRPE', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PRPE

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('PRPN', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = PRPN

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('R4N1', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = R4N1

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('R4N2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = R4N2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('R4O2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = R4O2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('R4P', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = R4P

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RA3P', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RA3P

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RB3P', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RB3P

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RCHO', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RCHO

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RCO3', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RCO3

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RCOOH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RIO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RIO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RIP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RIP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('ROH', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = ROH

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('RP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = RP

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('SO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = SO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('SO4', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = SO4

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('VRO2', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = VRO2

  State_Chm%Species( :, :, LM:1:-1, Get_Indx('VRP', State_Chm%Spec_Id, State_Chm%Spec_Name ) ) = VRP

