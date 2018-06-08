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
  State_Met%ALBD       = ALBVF                               ! unitless
  State_Met%CLDFRC     = CLDTT                               ! unitless
  State_Met%HFLUX      = SH                                  ! W/m2
  State_Met%EFLUX      = LHFX                                ! W/m2
  State_Met%FRCLND     = FRLAND                              ! unitless
  State_Met%FRLAND     = FRLAND                              ! unitless
  State_Met%FRLANDIC   = FRLANDICE                           ! unitless
  State_Met%FROCEAN    = FROCEAN                             ! unitless
  State_Met%FRLAKE     = FRLAKE                              ! unitless
  State_Met%FRSNO      = ASNOW                               ! unitless
  State_Met%GWETTOP    = WET1                                ! unitless
  State_Met%GWETROOT   = WET2                                ! unitless
!  State_Met%ITY        = ITY                                 ! unitless
!  WHERE ( LAI /= MAPL_UNDEF ) 
!     State_Met%LAI     = LAI                                 ! unitless
!  ELSEWHERE
!     State_Met%LAI     = 0.0e0                               ! unitless
!  ENDWHERE
  State_Met%LWI        = LWI                                 ! unitless
  State_Met%PARDR      = DRPAR                               ! W/m2
  State_Met%PARDF      = DFPAR                               ! W/m2
  State_Met%PBLH       = ZPBL                                ! m
  State_Met%PRECCON    = CN_PRCP  * 86400d0 * 1d-3           ! kg/m2/s -> mm/day
  State_Met%PRECTOT    = TPREC    * 86400d0 * 1d-3           ! kg/m2/s -> mm/day
  State_Met%TROPP      = GCCTROPP / 100d0                    ! Pa      -> hPa
!  State_Met%TO3        = TO3                                 ! DU
!  State_Met%TO3        = GCCTO3                              ! DU
  State_Met%TS         = T2M                                 ! K
  State_Met%TSKIN      = TS                                  ! K
!  State_Met%SST        = TS                                  ! K
!  State_Met%RADSWG     = SWNDSRF                             ! W/m2
  State_Met%SWGDN      = SWNDSRF                             ! W/m2
  State_Met%SNOMAS     = SNOMAS                              ! kg/m2
  State_Met%SNODP      = SNOWDP                              ! m
  State_Met%SUNCOS     = zenith                              ! unitless
  State_Met%SUNCOSmid  = zenith                              ! unitless
!  State_Met%SUNCOSmid5 = zenith                              ! unitless
  State_Met%U10M       = U10M                                ! m/s
  State_Met%USTAR      = USTAR                               ! m/s
  State_Met%UVALBEDO   = ALBVF                               ! - 
  State_Met%V10M       = V10M                                ! m/s
  State_Met%Z0         = Z0H                                 ! m
  State_Met%CNV_FRC    = CNV_FRC                             ! 1 

  ! Calculate the Monin-Obukhov Length
!  WHERE(ABS(SH) > 1.00E-32)
!    State_Met%MOLENGTH(:,:) = -AIRDENS(:,:,LM) * MAPL_CP * TS * ustar**3. / (MAPL_KARMAN * MAPL_GRAV * SH)
!  ELSEWHERE
!    State_Met%MOLENGTH(:,:) = 1.00E+05
!  END WHERE

  ! Same LAI for all 50 biomes?!??
!  DO I = 1, 50
!     State_Met%XLAI (:,:,I)  = State_Met%LAI                                 ! unitless
!     State_Met%XLAI2(:,:,I)  = State_Met%LAI                                 ! unitless
!  ENDDO

  ! Treat area as 1D
  State_Met%AREA_M2(:,:,1) = AREA

  ! Surface geopotential height [m]
  State_Met%PHIS(:,:) = ZLE(:,:,LM)

  ! Surface pressure
  State_Met%PS1_WET(:,:) = PS(:,:) * 1.0e-2  ! Pa -> hPa
  State_Met%PS2_WET(:,:) = PS(:,:) * 1.0e-2  ! Pa -> hPa

  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================
  DO L = 1, LM+1
     
     ! Reversed level index
     LR = LM - L + 1

     DO J = 1, JM
     DO I = 1, IM
        State_Met%PEDGE(I,J,L)   = PLE(I,J,LR) / 1d2  ! Pa -> hPa
        State_Met%PFLLSAN(I,J,L) = PFL_LSAN(I,J,LR)   ! kg/m2/s
        State_Met%PFILSAN(I,J,L) = PFI_LSAN(I,J,LR)   ! kg/m2/s

        State_Met%CMFMC(I,J,L) = CNV_MFC(I,J,LR)    ! kg/m2/s
        State_Met%PFICU(I,J,L) = PFI_CN (I,J,LR)    ! kg/m2/s
        State_Met%PFLCU(I,J,L) = PFL_CN (I,J,LR)    ! kg/m2/s
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
!        State_Met%AREA_M2 (I,J,L)  = AREA   (I,J   )        ! m2
        State_Met%AIRDEN  (I,J,L)  = AIRDENS(I,J,LR)        ! kg/m3
        State_Met%CLDF    (I,J,L)  = FCLD   (I,J,LR)        ! unitless
!        State_Met%DQIDTMST(I,J,L)  = DQIDT  (I,J,LR)        ! kg/kg/s
!        State_Met%DQLDTMST(I,J,L)  = DQLDT  (I,J,LR)        ! kg/kg/s
        State_Met%DQRLSAN (I,J,L)  = DQRL   (I,J,LR)        ! kg/kg/s
        State_Met%DTRAIN  (I,J,L)  = CNV_MFD(I,J,LR)               ! kg/m2/s
        State_Met%DQRCU   (I,J,L)  = DQRC   (I,J,LR)        ! kg/kg/s 
        State_Met%REEVAPCN(I,J,L)  = REV_CN (I,J,LR)        ! kg/kg/s
        State_Met%T       (I,J,L)  = T      (I,J,LR)        ! K
        State_Met%TMPU1   (I,J,L)  = T      (I,J,LR)        ! K
        State_Met%TMPU2   (I,J,L)  = T      (I,J,LR)        ! K
        IF ( ASSOCIATED( State_Met%UPDVVEL ) ) THEN
           IF ( ASSOCIATED(CNV_CVW) ) THEN
              State_Met%UPDVVEL (I,J,L)  = CNV_CVW(I,J,LR)        ! hPa/s
           ELSE
              State_Met%UPDVVEL (I,J,L)  = -999.0d0
           ENDIF 
        ENDIF

!        State_Met%MOISTQ  (I,J,L)  = DQDT   (I,J,LR)        ! 
        State_Met%QI      (I,J,L)  = QITOT  (I,J,LR)        ! kg/kg
        State_Met%QL      (I,J,L)  = QLTOT  (I,J,LR)        ! kg/kg
        State_Met%RH      (I,J,L)  = RH2    (I,J,LR) * 1d2  ! unitless->[%]
        State_Met%SPHU    (I,J,L)  = Q      (I,J,LR) * 1d3  ! kg/kg -> g/kg
        State_Met%SPHU1   (I,J,L)  = Q      (I,J,LR) * 1d3  ! kg/kg -> g/kg
        State_Met%SPHU2   (I,J,L)  = Q      (I,J,LR) * 1d3  ! kg/kg -> g/kg
        State_Met%TAUCLI  (I,J,L)  = TAUCLI (I,J,LR)        ! unitless
        State_Met%TAUCLW  (I,J,L)  = TAUCLW (I,J,LR)        ! unitless

        State_Met%U       (I,J,L)  = U      (I,J,LR)        ! m s-1 
        State_Met%V       (I,J,L)  = V      (I,J,LR)        ! m s-1

        !-------------------------------------------------------------------
        ! Quantities derived from met fields
        ! NOTE: Need to populate MOISTQ eventually
        !-------------------------------------------------------------------

        ! Grid box height [m]
        State_Met%BXHEIGHT(I,J,L)  =   ZLE               (I,J,LR-1)   &
                                   -   ZLE               (I,J,LR  )

        ! Pressure thickness [hPa]
        State_Met%DELP    (I,J,L)  =   State_Met%PEDGE   (I,J,L   )   & 
                                   -   State_Met%PEDGE   (I,J,L+1 )

        ! Total optical depth [unitless]
        State_Met%OPTD    (I,J,L)  = ( State_Met%TAUCLI  (I,J,L   )   &
                                   +   State_Met%TAUCLW  (I,J,L   ) )
!!!                                   *   State_Met%CLDF    (I,J,L   )

        ! Air mass in grid box [kg]
        State_Met%AD      (I,J,L)  =   State_Met%AIRDEN  (I,J,L   )   &
                                   *   State_Met%AREA_M2 (I,J,1   )   &
                                   *   State_Met%BXHEIGHT(I,J,L   )

        ! Grid box volume [m3]
        State_Met%AIRVOL  (I,J,L)  =   State_Met%AREA_M2 (I,J,1   )   &
                                   *   State_Met%BXHEIGHT(I,J,L   )

  ENDDO
  ENDDO
  ENDDO

  !=========================================================================
  ! Pass advected tracers from internal state to GEOS-Chem tracers array
  ! Species in internal state are in kg/kg total. GEOS-Chem will convert
  ! them to kg/kg dry internally.
  !=========================================================================
  DO I = 1, SIZE(Int2Spc,1)
     IF ( Int2Spc(I)%ID <= 0 ) CYCLE
     IF ( .NOT. ASSOCIATED(Int2Spc(I)%Internal) ) THEN
        CYCLE
     ELSE
        State_Chm%Species(:,:,LM:1:-1,Int2Spc(I)%ID) = Int2Spc(I)%Internal(:,:,:)
     ENDIF
  ENDDO
  State_Chm%Spc_Units = 'kg/kg total'

  !=========================================================================
  ! Various other archived variables needed in internal state. Eventually,
  ! we may copy those only on the first time step but for now do it every
  ! time. (ckeller, 9/16/15)
  !=========================================================================
  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'ARCHV_H2O2s', notFoundOK=.TRUE., __RC__ )
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(H2O2s) ) THEN
     H2O2s = Ptr3D
  ENDIF
  Ptr3D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'ARCHV_SO2s', notFoundOK=.TRUE., __RC__ )
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(SO2s) ) THEN
     SO2s = Ptr3D
  ENDIF
  Ptr3D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr2D, 'ARCHV_DRY_TOTN', notFoundOK=.TRUE., __RC__ )
  IF ( ASSOCIATED(Ptr2D) .AND. ASSOCIATED(DRY_TOTN) ) THEN
     DRY_TOTN = Ptr2D
  ENDIF
  Ptr2D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr2D, 'ARCHV_WET_TOTN', notFoundOK=.TRUE., __RC__ )
  IF ( ASSOCIATED(Ptr2D) .AND. ASSOCIATED(WET_TOTN) ) THEN
     WET_TOTN = Ptr2D
  ENDIF
  Ptr2D => NULL()

  CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'HSAVE_FOR_KPP' , notFoundOK=.TRUE., __RC__ )
  IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(HSAVE_KPP) ) THEN
     HSAVE_KPP(:,:,1:LLCHEM) = REAL(Ptr3D(:,:,LM:LM-LLCHEM+1:-1),KIND=ESMF_KIND_R8)
  ENDIF
  Ptr3D => NULL()

  ! Sesquiterpene concentrations. Convert from kgC/m3 to kgC/box.
  IF ( ALLOCATED(ORVC_SESQ) ) THEN
     CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'SESQ_CARBDENS', notFoundOK=.TRUE., __RC__ )
     IF ( ASSOCIATED(Ptr3D) ) THEN
        ORVC_SESQ(:,:,1:LM) = Ptr3D(:,:,LM:1:-1) * State_Met%AIRVOL(:,:,1:LM)
     ENDIF
     Ptr3D => NULL()
  ENDIF
!EOF
