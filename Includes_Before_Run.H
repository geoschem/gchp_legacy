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
!  27 Oct 2014 - C. Keller   - Fill species only on first call (only those
!                              that are not advected - others will be 
!                              initialized from tracers).
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Two-dimensional fields
  !=========================================================================

  State_Met%ALBD       = ALBD                                ! unitless
  State_Met%CLDFRC     = CLDFRC                              ! unitless
  State_Met%HFLUX      = HFLUX                               ! W/m2
  State_Met%FRCLND     = FRLAND                              ! unitless
  State_Met%FRLAND     = FRLAND                              ! unitless
  State_Met%FROCEAN    = FROCEAN                             ! unitless
  State_Met%FRLAKE     = FRLAKE                              ! unitless
  State_Met%FRLANDIC   = FRLANDIC                            ! unitless
  State_Met%GWETTOP    = GWETTOP                             ! unitless
  State_Met%LWI        = LWI                                 ! unitless
  State_Met%PARDR      = PARDR                               ! W/m2
  State_Met%PARDF      = PARDF                               ! W/m2
  State_Met%PBLH       = PBLH                                ! m
  State_Met%PRECCON    = PRECCON  * 86400d0                  ! kg/m2/s -> mm/day
  State_Met%PRECTOT    = PRECTOT  * 86400d0                  ! kg/m2/s -> mm/day
  State_Met%TROPP      = TROPP / 100d0                       ! Pa      -> hPa
  State_Met%PS1        = PS1                                 ! hPa
!  State_Met%PS2        = PS2                                 ! hPa
! State_Met%TO3        = TO3                                 ! DU
  State_Met%TS         = TS                                  ! K
  State_Met%TSKIN      = TSKIN                               ! K
  State_Met%RADSWG     = RADSWG                              ! W/m2
  State_Met%SNODP      = SNODP                               ! m
  State_Met%SNOMAS     = SNOMAS                              ! kg/m2
  State_Met%SST        = TS                                  ! K
  State_Met%SUNCOS     = zenith                              ! unitless
  State_Met%SUNCOSmid  = zenith                              ! unitless
  State_Met%SUNCOSmid5 = zenith                              ! unitless
  State_Met%U10M       = U10M                                ! m/s
  State_Met%USTAR      = USTAR                               ! m/s
  State_Met%UVALBEDO   = ALBD   !*** NOTE Need to populate w/ real UVALBEDO ***
  State_Met%V10M       = V10M                                ! m/s
  State_Met%Z0         = Z0                                  ! m

  ! Calculate the Monin-Obukhov Length
  WHERE(ABS(State_Met%HFLUX) > 1.00E-32)
    State_Met%MOLENGTH(:,:) = -AIRDENS(:,:,LM) * MAPL_CP * TS * ustar**3. / (MAPL_KARMAN * MAPL_GRAV * State_Met%HFLUX)
  ELSEWHERE
    State_Met%MOLENGTH(:,:) = 1.00E+05
  END WHERE

  ! Same LAI for all 50 biomes?!??
  DO I = 1, 50
     State_Met%XLAI(:,:,I)  = LAI                                 ! unitless
  ENDDO

  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================
  DO L = 1, LM+1
     
     ! Reversed level index
     LR = LM - L + 1

     DO J = 1, JM
     DO I = 1, IM
        State_Met%PEDGE(I,J,L) = PLE(  I, J, L) / 1d2  ! Pa -> hPa
        State_Met%CMFMC(I,J,L) = CMFMC(I, J, L)    ! kg/m2/s
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
        State_Met%AREA_M2 (I,J,L)  = AREA   (I,J   )        ! m2
        State_Met%CLDF    (L,I,J)  = CLDF   (I,J,L)        ! unitless
!       State_Met%DQIDTMST(I,J,L)  = DQIDT  (I,J,L)        ! kg/kg/s
!       State_Met%DQLDTMST(I,J,L)  = DQLDT  (I,J,L)        ! kg/kg/s
!       State_Met%DQVDTMST(I,J,L)  = DQDT   (I,J,L)        ! kg/kg/s
        State_Met%DTRAIN  (I,J,L)  = CMFMC  (I,J,L)        ! kg/m2/s
!       State_Met%DQRCU   (I,J,L)  = DQRC   (I,J,L)        ! kg/kg/s 
!       State_Met%REEVAPCN(I,J,L)  = REV_CN (I,J,L)        ! kg/kg/s
!       State_Met%MOISTQ  (L,I,J)  = DQDT   (I,J,L)        ! 
        State_Met%QI      (I,J,L)  = QI     (I,J,L)        ! kg/kg
        State_Met%QL      (I,J,L)  = QL     (I,J,L)        ! kg/kg
        State_Met%RH      (I,J,L)  = RH     (I,J,L) * 1d2  ! unitless->[%]
        State_Met%SPHU    (I,J,L)  = SPHU1  (I,J,L) * 1d3  ! kg/kg -> g/kg
        State_Met%T       (I,J,L)  = TMPU1  (I,J,L)        ! K
        State_Met%TAUCLI  (I,J,L)  = TAUCLI (I,J,L)        ! unitless
        State_Met%TAUCLW  (I,J,L)  = TAUCLW (I,J,L)        ! unitless

        !-------------------------------------------------------------------
        ! Quantities derived from met fields
        ! NOTE: Need to populate MOISTQ eventually
        !-------------------------------------------------------------------

        ! Grid box height [m]
!        State_Met%BXHEIGHT(I,J,L)  =   ZLE               (I,J,LR-1)   &
!                                   -   ZLE               (I,J,LR  )

        ! Pressure thickness [hPa]
!        State_Met%DELP    (L,I,J)  =   State_Met%PEDGE   (I,J,L   )   & 
!                                   -   State_Met%PEDGE   (I,J,L+1 )

        ! Total optical depth [unitless]
        State_Met%OPTD    (L,I,J)  = ( State_Met%TAUCLI  (I,J,L   )   &
                                   +   State_Met%TAUCLW  (I,J,L   ) ) &
                                   *   State_Met%CLDF    (L,I,J   )

!        ! Air mass in grid box [kg]
!        State_Met%AD      (I,J,L)  =   State_Met%AIRDEN  (L,I,J   )   &
!                                   *   AREA              (I,J     )   &
!                                   *   State_Met%BXHEIGHT(I,J,L   )

        ! Grid box volume [m3]
!        State_Met%AIRVOL  (I,J,L)  =   AREA              (I,J     )   &
!                                   *   State_Met%BXHEIGHT(I,J,L   )

  ENDDO
  ENDDO
  ENDDO

  !=========================================================================
  ! Advected tracers in the INTERNAL STATE
  ! (Code was generated with Perl script gcIncBef)
   !=========================================================================
  IND = Get_Indx( 'NO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NO
  
  IND = Get_Indx( 'O3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_O3
  
  IND = Get_Indx( 'PAN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_PAN
  
  IND = Get_Indx( 'CO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CO
  
  IND = Get_Indx( 'ALK4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ALK4
  
  IND = Get_Indx( 'ISOP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ISOP
  
  IND = Get_Indx( 'HNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HNO3
 
  IND = Get_Indx( 'H2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_H2O2
  
  IND = Get_Indx( 'ACET', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ACET
  
  IND = Get_Indx( 'MEK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MEK
  
  IND = Get_Indx( 'ALD2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ALD2
  
  IND = Get_Indx( 'RCHO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_RCHO
  
  IND = Get_Indx( 'MVK', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MVK
  
  IND = Get_Indx( 'MACR', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MACR
  
  IND = Get_Indx( 'PMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_PMN
  
  IND = Get_Indx( 'PPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_PPN
  
  IND = Get_Indx( 'R4N2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_R4N2
  
  IND = Get_Indx( 'PRPE', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_PRPE
  
  IND = Get_Indx( 'C3H8', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_C3H8
  
  IND = Get_Indx( 'CH2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH2O
  
  IND = Get_Indx( 'C2H6', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_C2H6
  
  IND = Get_Indx( 'N2O5', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_N2O5
  
  IND = Get_Indx( 'HNO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HNO4
  
  IND = Get_Indx( 'MP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MP
  
  IND = Get_Indx( 'DMS', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_DMS
  
  IND = Get_Indx( 'SO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_SO2
  
  IND = Get_Indx( 'SO4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_SO4
  
  IND = Get_Indx( 'SO4s', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_SO4s
  
  IND = Get_Indx( 'MSA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MSA
  
  IND = Get_Indx( 'NH3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NH3
  
  IND = Get_Indx( 'NH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NH4
  
  IND = Get_Indx( 'NIT', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NIT
  
  IND = Get_Indx( 'NITs', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NITs
  
  IND = Get_Indx( 'BCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BCPI
  
  IND = Get_Indx( 'OCPI', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_OCPI
  
  IND = Get_Indx( 'BCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BCPO
  
  IND = Get_Indx( 'OCPO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_OCPO
  
  IND = Get_Indx( 'DST1', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_DST1
  
  IND = Get_Indx( 'DST2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_DST2
  
  IND = Get_Indx( 'DST3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_DST3
  
  IND = Get_Indx( 'DST4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_DST4
  
  IND = Get_Indx( 'SALA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_SALA
  
  IND = Get_Indx( 'SALC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_SALC
  
  IND = Get_Indx( 'Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_Br2
  
  IND = Get_Indx( 'Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_Br
  
  IND = Get_Indx( 'BrO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BrO
  
  IND = Get_Indx( 'HOBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HOBr
  
  IND = Get_Indx( 'HBr', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HBr
  
  IND = Get_Indx( 'BrNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BrNO2
  
  IND = Get_Indx( 'BrNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BrNO3
  
  IND = Get_Indx( 'CHBr3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CHBr3
  
  IND = Get_Indx( 'CH2Br2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH2Br2
  
  IND = Get_Indx( 'CH3Br', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH3Br
  
  IND = Get_Indx( 'MPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MPN
  
  IND = Get_Indx( 'ISOPN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ISOPN
  
  IND = Get_Indx( 'MOBA', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MOBA
  
  IND = Get_Indx( 'PROPNN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_PROPNN
  
  IND = Get_Indx( 'HAC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HAC
  
  IND = Get_Indx( 'GLYC', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_GLYC
  
  IND = Get_Indx( 'MMN', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MMN
  
  IND = Get_Indx( 'RIP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_RIP
  
  IND = Get_Indx( 'IEPOX', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_IEPOX
  
  IND = Get_Indx( 'MAP', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_MAP
  
  IND = Get_Indx( 'NO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NO2
  
  IND = Get_Indx( 'NO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_NO3
  
  IND = Get_Indx( 'HNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HNO2
  
  IND = Get_Indx( 'N2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_N2O
  
  IND = Get_Indx( 'OCS', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_OCS
  
  IND = Get_Indx( 'CH4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH4
  
  IND = Get_Indx( 'BrCl', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_BrCl
  
  IND = Get_Indx( 'HCl', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HCl
  
  IND = Get_Indx( 'CCl4', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CCl4
  
  IND = Get_Indx( 'CH3Cl', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH3Cl
  
  IND = Get_Indx( 'CH3CCl3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CH3CCl3
  
  IND = Get_Indx( 'CFCX', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CFCX
  
  IND = Get_Indx( 'HCFCX', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HCFCX
  
  IND = Get_Indx( 'CFC11', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CFC11
  
  IND = Get_Indx( 'CFC12', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_CFC12
  
  IND = Get_Indx( 'HCFC22', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HCFC22
  
  IND = Get_Indx( 'H1211', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_H1211
  
  IND = Get_Indx( 'H1301', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_H1301
  
  IND = Get_Indx( 'H2402', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_H2402
  
  IND = Get_Indx( 'Cl', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_Cl
  
  IND = Get_Indx( 'ClO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ClO
  
  IND = Get_Indx( 'HOCl', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_HOCl
  
  IND = Get_Indx( 'ClNO3', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ClNO3
  
  IND = Get_Indx( 'ClNO2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ClNO2
  
  IND = Get_Indx( 'ClOO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_ClOO
  
  IND = Get_Indx( 'OClO', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_OClO
  
  IND = Get_Indx( 'Cl2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_Cl2
  
  IND = Get_Indx( 'Cl2O2', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_Cl2O2
  
  IND = Get_Indx( 'H2O', State_Chm%Trac_Id, State_Chm%Trac_Name )
  IF( IND > 0 ) State_Chm%Tracers(:,:,:,IND ) = TRC_H2O

  ! Flip in the vertical
  State_Chm%Tracers   = State_Chm%Tracers( :, :, LM:1:-1, : )

  !=========================================================================
  ! On first call, also need to initialize the species from restart file.
  ! Only need to do this for species that are not advected, i.e. species
  ! that are not tracers (all other species arrays will be filled with
  ! tracer values anyways!).
  ! We only need to do this on the first call because afterwards, species
  ! array already contains values from previous chemistry time step
  ! (advected species will be updated with tracers)
  ! ckeller, 10/27/2014
  !=========================================================================
  IF ( FIRST ) THEN

     ! Get Generic State
     call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
     VERIFY_(STATUS)
     ! Get Internal state
     CALL MAPL_Get ( STATE, INTERNAL_ESMF_STATE=INTERNAL, __RC__ ) 

     ! Loop over all species
     DO I = 1, SIZE(State_Chm%Spec_ID,1)

        ! Skip if empty
        IF ( TRIM(State_Chm%Spec_Name(I)) == '' ) CYCLE

        ! Is this a tracer?
        IND = Get_Indx( TRIM(State_Chm%Spec_Name(I)), State_Chm%Trac_Id, State_Chm%Trac_Name )
        IF ( IND > 0 ) CYCLE

        ! Get data from internal state and copy to species array. 
        CALL MAPL_GetPointer( INTERNAL, Ptr3D, TRIM(SPFX)//TRIM(State_Chm%Spec_Name(I)), &
                              notFoundOK=.TRUE., __RC__ )
        IF ( .NOT. ASSOCIATED(Ptr3D) ) THEN
           IF ( MAPL_am_I_Root()) WRITE(*,*) 'Could not find species in INTERNAL state - will be initialized to zero: ', &
              TRIM(SPFX), TRIM(State_Chm%Spec_Name(I))
           CYCLE
        ENDIF
        State_Chm%Species(:,:,:,State_Chm%Spec_ID(I)) = Ptr3D(:,:,LM:1:-1)

        ! testing only
        if ( MAPL_am_I_Root()) write(*,*) 'Initialized species from INTERNAL state: ', TRIM(State_Chm%Spec_Name(I))

     ENDDO
  ENDIF
