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

  State_Met%ALBD       = ALBD                                ! unitless
  State_Met%CLDFRC     = CLDFRC                              ! unitless
! State_Met%HFLUX      = SH                                  ! W/m2
  State_Met%FRCLND     = FRLAND                              ! unitless
  State_Met%GWETTOP    = GWETTOP                             ! unitless
  State_Met%LWI        = LWI                                 ! unitless
  State_Met%PARDR      = PARDR                               ! W/m2
  State_Met%PARDF      = PARDF                               ! W/m2
  State_Met%PBLH       = PBLH                                ! m
  State_Met%PRECCON    = PRECCON  * 86400d0                  ! kg/m2/s -> mm/day
  State_Met%PRECTOT    = PRECTOT  * 86400d0                  ! kg/m2/s -> mm/day
  State_Met%TROPP      = TROPP / 100d0                       ! Pa      -> hPa
  State_Met%PS1        = PS1                                 ! hPa
  State_Met%PS2        = PS2                                 ! hPa
! State_Met%TO3        = TO3                                 ! DU
  State_Met%TS         = TS                                  ! K
  State_Met%RADSWG     = RADSWG                              ! W/m2
  State_Met%SST        = TS                                  ! K
  State_Met%SUNCOS     = zenith                              ! unitless
  State_Met%SUNCOSmid  = zenith                              ! unitless
  State_Met%SUNCOSmid5 = zenith                              ! unitless
  State_Met%U10M       = U10M                                ! m/s
  State_Met%USTAR      = USTAR                               ! m/s
  State_Met%UVALBEDO   = ALBD   !*** NOTE Need to populate w/ real UVALBEDO ***
  State_Met%V10M       = V10M                                ! m/s
  State_Met%Z0         = Z0                                  ! m
  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================
  DO L = 1, LM+1
     
     ! Reversed level index
     LR = LM - L + 1

     DO J = 1, JM
     DO I = 1, IM
!       State_Met%PEDGE(I,J,L)     = PLE    (I,J,LR) / 1d2  ! Pa -> hPa 
        State_Met%CMFMC(I,J,L)     = CMFMC  (I,J,LR)        ! kg/m2/s
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
!       State_Met%AIRDEN  (L,I,J)  = AIRDENS(I,J,LR)        ! kg/m3
!       State_Met%AREA_M2 (I,J,L)  = AREA   (I,J   )        ! m2
        State_Met%CLDF    (L,I,J)  = CLDF   (I,J,LR)        ! unitless
!       State_Met%DQIDTMST(I,J,L)  = DQIDT  (I,J,LR)        ! kg/kg/s
!       State_Met%DQLDTMST(I,J,L)  = DQLDT  (I,J,LR)        ! kg/kg/s
!       State_Met%DQVDTMST(I,J,L)  = DQDT   (I,J,LR)        ! kg/kg/s
        State_Met%DTRAIN  (I,J,L)  = DTRAIN (I,J,LR)        ! kg/m2/s
!       State_Met%MOISTQ  (L,I,J)  = DQDT   (I,J,LR)        ! 
        State_Met%QI      (I,J,L)  = QI     (I,J,LR)        ! kg/kg
        State_Met%QL      (I,J,L)  = QL     (I,J,LR)        ! kg/kg
        State_Met%RH      (I,J,L)  = RH     (I,J,LR)        ! unitless
        State_Met%SPHU    (I,J,L)  = SPHU1  (I,J,LR) * 1d3  ! kg/kg -> g/kg
        State_Met%T       (I,J,L)  = TMPU1  (I,J,LR)        ! K
        State_Met%TAUCLI  (I,J,L)  = TAUCLI (I,J,LR)        ! unitless
        State_Met%TAUCLW  (I,J,L)  = TAUCLW (I,J,LR)        ! unitless

        !-------------------------------------------------------------------
        ! Quantities derived from met fields
        ! NOTE: Need to populate MOISTQ eventually
        !-------------------------------------------------------------------

        ! Grid box height [m]
!        State_Met%BXHEIGHT(I,J,L)  =   ZLE               (I,J,LR-1)   &
!                                   -   ZLE               (I,J,LR  )

        ! Pressure thickness [hPa]
!       State_Met%DELP    (L,I,J)  =   State_Met%PEDGE   (I,J,L   )   & 
!                                  -   State_Met%PEDGE   (I,J,L+1 )

        ! Total optical depth [unitless]
        State_Met%OPTD    (L,I,J)  = ( State_Met%TAUCLI  (I,J,L   )   &
                                   +   State_Met%TAUCLW  (I,J,L   ) ) &
                                   *   State_Met%CLDF    (L,I,J   )

        ! Air mass in grid box [kg]
!       State_Met%AD      (I,J,L)  =   State_Met%AIRDEN  (L,I,J   )   &
!                                  *   AREA              (I,J     )   &
!                                  *   State_Met%BXHEIGHT(I,J,L   )

        ! Grid box volume [m3]
!       State_Met%AIRVOL  (I,J,L)  =   AREA              (I,J     )   &
!                                  *   State_Met%BXHEIGHT(I,J,L   )
  ENDDO
  ENDDO
  ENDDO

  !=========================================================================
  ! Advected tracers and chemical species in the INTERNAL STATE
  ! (Code was generated with Perl script gcIncBef)
  !=========================================================================
  IND = Get_Indx( 'TRC_NO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO

  IND = Get_Indx( 'TRC_O3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_O3

  IND = Get_Indx( 'TRC_PAN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PAN

  IND = Get_Indx( 'TRC_CO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CO

  IND = Get_Indx( 'TRC_ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ALK4

  IND = Get_Indx( 'TRC_ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ISOP

  IND = Get_Indx( 'TRC_HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO3

  IND = Get_Indx( 'TRC_H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_H2O2

  IND = Get_Indx( 'TRC_ACET', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ACET

  IND = Get_Indx( 'TRC_MEK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MEK

  IND = Get_Indx( 'TRC_ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ALD2

  IND = Get_Indx( 'TRC_RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_RCHO

  IND = Get_Indx( 'TRC_MVK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MVK

  IND = Get_Indx( 'TRC_MACR', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MACR

  IND = Get_Indx( 'TRC_PMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PMN

  IND = Get_Indx( 'TRC_PPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PPN

  IND = Get_Indx( 'TRC_R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_R4N2

  IND = Get_Indx( 'TRC_PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PRPE

  IND = Get_Indx( 'TRC_C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_C3H8

  IND = Get_Indx( 'TRC_CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH2O

  IND = Get_Indx( 'TRC_C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_C2H6

  IND = Get_Indx( 'TRC_N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_N2O5

  IND = Get_Indx( 'TRC_HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO4

  IND = Get_Indx( 'TRC_MP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MP

  IND = Get_Indx( 'TRC_DMS', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DMS

  IND = Get_Indx( 'TRC_SO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO2

  IND = Get_Indx( 'TRC_SO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO4

  IND = Get_Indx( 'TRC_SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SO4s

  IND = Get_Indx( 'TRC_MSA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MSA

  IND = Get_Indx( 'TRC_NH3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NH3

  IND = Get_Indx( 'TRC_NH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NH4

  IND = Get_Indx( 'TRC_NIT', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NIT

  IND = Get_Indx( 'TRC_NITs', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NITs

  IND = Get_Indx( 'TRC_BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BCPI

  IND = Get_Indx( 'TRC_OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_OCPI

  IND = Get_Indx( 'TRC_BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BCPO

  IND = Get_Indx( 'TRC_OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_OCPO

  IND = Get_Indx( 'TRC_DST1', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST1

  IND = Get_Indx( 'TRC_DST2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST2

  IND = Get_Indx( 'TRC_DST3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST3

  IND = Get_Indx( 'TRC_DST4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_DST4

  IND = Get_Indx( 'TRC_SALA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SALA

  IND = Get_Indx( 'TRC_SALC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_SALC

  IND = Get_Indx( 'TRC_Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_Br2

  IND = Get_Indx( 'TRC_Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_Br

  IND = Get_Indx( 'TRC_BrO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrO

  IND = Get_Indx( 'TRC_HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HOBr

  IND = Get_Indx( 'TRC_HBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HBr

  IND = Get_Indx( 'TRC_BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrNO2

  IND = Get_Indx( 'TRC_BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_BrNO3

  IND = Get_Indx( 'TRC_CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CHBr3

  IND = Get_Indx( 'TRC_CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH2Br2

  IND = Get_Indx( 'TRC_CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_CH3Br

  IND = Get_Indx( 'TRC_MPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MPN

  IND = Get_Indx( 'TRC_ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_ISOPN

  IND = Get_Indx( 'TRC_MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MOBA

  IND = Get_Indx( 'TRC_PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_PROPNN

  IND = Get_Indx( 'TRC_HAC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HAC

  IND = Get_Indx( 'TRC_GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_GLYC

  IND = Get_Indx( 'TRC_MMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MMN

  IND = Get_Indx( 'TRC_RIP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_RIP

  IND = Get_Indx( 'TRC_IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_IEPOX

  IND = Get_Indx( 'TRC_MAP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_MAP

  IND = Get_Indx( 'TRC_NO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO2

  IND = Get_Indx( 'TRC_NO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_NO3

  IND = Get_Indx( 'TRC_HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF ( IND > 0 ) State_Chm%Tracers(:,:,:,IND) = TRC_HNO2

  IND = Get_Indx( 'A3O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = A3O2

  IND = Get_Indx( 'ACET', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ACET

  IND = Get_Indx( 'ACTA', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ACTA

  IND = Get_Indx( 'ALD2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ALD2

  IND = Get_Indx( 'ALK4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ALK4

  IND = Get_Indx( 'ATO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ATO2

  IND = Get_Indx( 'B3O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = B3O2

  IND = Get_Indx( 'Br', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = Br

  IND = Get_Indx( 'Br2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = Br2

  IND = Get_Indx( 'BrNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = BrNO2

  IND = Get_Indx( 'BrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = BrNO3

  IND = Get_Indx( 'BrO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = BrO

  IND = Get_Indx( 'C2H6', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = C2H6

  IND = Get_Indx( 'C3H8', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = C3H8

  IND = Get_Indx( 'CH2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CH2O

  IND = Get_Indx( 'CH2Br2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CH2Br2

  IND = Get_Indx( 'CH3Br', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CH3Br

  IND = Get_Indx( 'CH4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CH4

  IND = Get_Indx( 'CHBr3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CHBr3

  IND = Get_Indx( 'CO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = CO

  IND = Get_Indx( 'DMS', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DMS

  IND = Get_Indx( 'DRYBr2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYBr2

  IND = Get_Indx( 'DRYBrNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYBrNO3

  IND = Get_Indx( 'DRYCH2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYCH2O

  IND = Get_Indx( 'DRYH2O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYH2O2

  IND = Get_Indx( 'DRYHBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYHBr

  IND = Get_Indx( 'DRYHNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYHNO3

  IND = Get_Indx( 'DRYHOBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYHOBr

  IND = Get_Indx( 'DRYN2O5', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYN2O5

  IND = Get_Indx( 'DRYNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYNO2

  IND = Get_Indx( 'DRYO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYO3

  IND = Get_Indx( 'DRYPAN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYPAN

  IND = Get_Indx( 'DRYPMN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYPMN

  IND = Get_Indx( 'DRYPPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYPPN

  IND = Get_Indx( 'DRYR4N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYR4N2

  IND = Get_Indx( 'DRYMVK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYMVK

  IND = Get_Indx( 'DRYMACR', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = DRYMACR

  IND = Get_Indx( 'EMISSION', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = EMISSION

  IND = Get_Indx( 'EOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = EOH

  IND = Get_Indx( 'ETO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ETO2

  IND = Get_Indx( 'ETP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ETP

  IND = Get_Indx( 'GCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = GCO3

  IND = Get_Indx( 'GLYC', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = GLYC

  IND = Get_Indx( 'GLYX', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = GLYX

  IND = Get_Indx( 'GP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = GP

  IND = Get_Indx( 'H', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = H

  IND = Get_Indx( 'H2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = H2

  IND = Get_Indx( 'H2O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = H2O

  IND = Get_Indx( 'H2O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = H2O2

  IND = Get_Indx( 'HAC', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HAC

  IND = Get_Indx( 'HBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HBr

  IND = Get_Indx( 'HOBr', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HOBr

  IND = Get_Indx( 'HCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HCOOH

  IND = Get_Indx( 'HNO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HNO2

  IND = Get_Indx( 'HNO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HNO3

  IND = Get_Indx( 'HNO4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HNO4

  IND = Get_Indx( 'HO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = HO2

  IND = Get_Indx( 'IALD', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = IALD

  IND = Get_Indx( 'IAO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = IAO2

  IND = Get_Indx( 'IAP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = IAP

  IND = Get_Indx( 'INO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = INO2

  IND = Get_Indx( 'INPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = INPN

  IND = Get_Indx( 'ISN1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ISN1

  IND = Get_Indx( 'ISNP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ISNP

  IND = Get_Indx( 'ISOP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ISOP

  IND = Get_Indx( 'KO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = KO2

  IND = Get_Indx( 'M', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = M

  IND = Get_Indx( 'MACR', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MACR

  IND = Get_Indx( 'MAN2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MAN2

  IND = Get_Indx( 'MAO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MAO3

  IND = Get_Indx( 'MAOP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MAOP

  IND = Get_Indx( 'MAP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MAP

  IND = Get_Indx( 'MCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MCO3

  IND = Get_Indx( 'MEK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MEK

  IND = Get_Indx( 'MGLY', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MGLY

  IND = Get_Indx( 'MO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MO2

  IND = Get_Indx( 'MOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MOH

  IND = Get_Indx( 'MP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MP

  IND = Get_Indx( 'MRO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MRO2

  IND = Get_Indx( 'MRP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MRP

  IND = Get_Indx( 'MSA', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MSA

  IND = Get_Indx( 'MVK', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = MVK

  IND = Get_Indx( 'N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = N2

  IND = Get_Indx( 'N2O5', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = N2O5

  IND = Get_Indx( 'NH2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = NH2

  IND = Get_Indx( 'NH3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = NH3

  IND = Get_Indx( 'NO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = NO

  IND = Get_Indx( 'NO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = NO2

  IND = Get_Indx( 'NO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = NO3

  IND = Get_Indx( 'O', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = O

  IND = Get_Indx( 'O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = O2

!  IND = Get_Indx( 'O3_ND', State_Chm%Spec_Id, State_Chm%Spec_Name )
!  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = O3_ND

  IND = Get_Indx( 'OH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = OH

  IND = Get_Indx( 'PAN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PAN

  IND = Get_Indx( 'PMN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PMN

  IND = Get_Indx( 'PO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PO2

  IND = Get_Indx( 'PP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PP

  IND = Get_Indx( 'PPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PPN

  IND = Get_Indx( 'PRN1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PRN1

  IND = Get_Indx( 'PRPE', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PRPE

  IND = Get_Indx( 'PRPN', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = PRPN

  IND = Get_Indx( 'R4N1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = R4N1

  IND = Get_Indx( 'R4N2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = R4N2

  IND = Get_Indx( 'R4O2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = R4O2

  IND = Get_Indx( 'R4P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = R4P

  IND = Get_Indx( 'RA3P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RA3P

  IND = Get_Indx( 'RB3P', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RB3P

  IND = Get_Indx( 'RCHO', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RCHO

  IND = Get_Indx( 'RCO3', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RCO3

  IND = Get_Indx( 'RCOOH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RCOOH

  IND = Get_Indx( 'RIO1', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RIO1

  IND = Get_Indx( 'RIO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RIO2

  IND = Get_Indx( 'RIP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RIP

  IND = Get_Indx( 'ROH', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = ROH

  IND = Get_Indx( 'RP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = RP

  IND = Get_Indx( 'SO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = SO2

  IND = Get_Indx( 'SO4', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = SO4

  IND = Get_Indx( 'VRO2', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = VRO2

  IND = Get_Indx( 'VRP', State_Chm%Spec_Id, State_Chm%Spec_Name )
  IF ( IND > 0 ) State_Chm%Species(:,:,:,IND) = VRP

  ! Flip tracers & species in the vertical
  State_Chm%Tracers = State_Chm%Tracers( :, :, LM:1:-1, : )
  State_Chm%Species = State_Chm%Species( :, :, LM:1:-1, : )

