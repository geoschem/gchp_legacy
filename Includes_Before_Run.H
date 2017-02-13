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
!  19 Oct 2016 - R. Yantosca - Now avoid out of bounds errors.  Also reference
!                              MAPL pointers to edged data with (:,:,0:LM).
!EOP
!------------------------------------------------------------------------------
!BOC

  !=========================================================================
  ! Two-dimensional fields
  !=========================================================================

  State_Met%ALBD                = ALBD                       ! 1
  State_Met%AREA_M2(:,:,1)      = AREA                       ! m2
  State_Met%CLDFRC              = CLDFRC                     ! 1
  State_Met%HFLUX               = HFLUX                      ! W/m2
  State_Met%FRCLND              = FRLAND                     ! 1
  State_Met%FRLAND              = FRLAND                     ! 1
  State_Met%FROCEAN             = FROCEAN                    ! 1
  State_Met%FRLAKE              = FRLAKE                     ! 1
  State_Met%FRLANDIC            = FRLANDIC                   ! 1
  State_Met%GWETTOP             = GWETTOP                    ! 1
  State_Met%LAI                 = LAI                        ! 1  
  State_Met%LWI                 = LWI                        ! 1
  State_Met%PARDR               = PARDR                      ! W/m2
  State_Met%PARDF               = PARDF                      ! W/m2
  State_Met%PBLH                = PBLH                       ! m
  State_Met%PRECCON             = PRECCON  * 86400d0         ! kg/m2/s -> mm/day
  State_Met%PRECTOT             = PRECTOT  * 86400d0         ! kg/m2/s -> mm/day
  State_Met%TROPP               = TROPP                      ! hPa
  State_Met%PS1_WET             = PS1                        ! hPa
  State_Met%TS                  = TS                         ! K
  State_Met%TSKIN               = TSKIN                      ! K
  State_Met%RADSWG              = RADSWG                     ! W/m2
  State_Met%SNODP               = SNODP                      ! m
  State_Met%SNOMAS              = SNOMAS                     ! kg/m2
  State_Met%SST                 = TS                         ! K
  State_Met%SUNCOS              = zenith                     ! unitless
  State_Met%SUNCOSmid           = zenith                     ! 1
  State_Met%U10M                = U10M                       ! m/s
  State_Met%USTAR               = USTAR                      ! m/s
  !**** NOTE Need to populate w/ real UVALBEDO ***
  State_Met%UVALBEDO            = ALBD                       ! 1
  State_Met%V10M                = V10M                       ! m/s
  State_Met%Z0                  = Z0                         ! m

  ! Calculate the Monin-Obukhov Length
  WHERE( ABS( State_Met%HFLUX ) > 1.00E-32 )
     State_Met%MOLENGTH(:,:)    = -AIRDENS(:,:,LM) * MAPL_CP * TS * ustar**3. &
                                / ( MAPL_KARMAN * MAPL_GRAV * State_Met%HFLUX )
  ELSEWHERE
     State_Met%MOLENGTH(:,:)    = 1.00E+05
  ENDWHERE

  ! Same LAI for all 50 biomes?!?? (commented out by ewl, 12/16/16)
  !DO I = 1, 50
  !   State_Met%XLAI(:,:,I)      = LAI                        ! 1
  !ENDDO

  !=========================================================================
  ! Three-dimensional fields on level edges
  !=========================================================================

  State_Met%CMFMC  (:,:,1:LM+1) = CMFMC  (:,:,0:LM   )       ! kg/m2/s
  State_Met%PFICU  (:,:,1:LM+1) = PFICU  (:,:,0:LM   )       ! kg/m2/s
  State_Met%PFILSAN(:,:,1:LM+1) = PFILSAN(:,:,0:LM   )       ! kg/m2/s
  State_Met%PFLCU  (:,:,1:LM+1) = PFLCU  (:,:,0:LM   )       ! kg/m2/s
  State_Met%PFLLSAN(:,:,1:LM+1) = PFLLSAN(:,:,0:LM   )       ! kg/m2/s 

  ! NOTE: PLE is reversed in the vertical!
  State_Met%PEDGE  (:,:,1:LM+1) = PLE    (:,:,LM:0:-1) / 1d2 ! Pa -> hPa
 
  !=========================================================================
  ! Three-dimensional fields on level centers
  !=========================================================================
  State_Met%CLDF                = CLDF                       ! 1
  State_Met%DTRAIN              = DTRAIN                     ! kg/m2/s
  State_Met%DQRCU               = DQRCU                      ! kg/kg/s
  State_Met%DQRLSAN             = DQRLSAN                    ! kg/kg/s
  State_Met%QI                  = QI                         ! kg/kg
  State_Met%QL                  = QL                         ! kg/kg
  State_Met%RH                  = RH      * 1d2              ! 1 -> %
  State_Met%SPHU                = SPHU1   * 1d3              ! kg/kg -> g/kg
  State_Met%T                   = TMPU1                      ! K
  State_Met%TAUCLI              = TAUCLI                     ! 1
  State_Met%TAUCLW              = TAUCLW                     ! 1
  State_Met%REEVAPCN            = REEVAPCN                   ! kg/kg/s
  State_Met%REEVAPLS            = REEVAPLS                   ! kg/kg/s

  ! Create total optical depth field                         ! 1
  State_Met%OPTD                = ( State_Met%TAUCLI    &
                                +   State_Met%TAUCLW  ) &
                                *   State_Met%CLDF
  
  ! Set the State_Chm units to v/v dry (restart file units)
 
  !!### Debug printout
  !IF ( am_I_root ) THEN
  !   print*, '### LM      : ', LM
  !   print*, '### CMFMC    ', State_Met%CMFMC   (10,10,:)
  !   print*, '### PFICU    ', State_Met%PFICU   (10,10,:)
  !   print*, '### PFILSAN  ', State_Met%PFILSAN (10,10,:)
  !   print*, '### PFLCU    ', State_Met%PFLCU   (10,10,:)
  !   print*, '### PFLLSAN  ', State_Met%PFLLSAN (10,10,:)
  !   print*, '### PEDGE    ', State_Met%PEDGE   (10,10,:)
  !   print*, '### CLDF     ', State_Met%CLDF    (10,10,:) 
  !   print*, '### DTRAIN   ', State_Met%DTRAIN  (10,10,:) 
  !   print*, '### DQRCU    ', State_Met%DQRCU   (10,10,:) 
  !   print*, '### DQRLSAN  ', State_Met%DQRLSAN (10,10,:) 
  !   print*, '### QI       ', State_Met%QI      (10,10,:) 
  !   print*, '### QL       ', State_Met%QL      (10,10,:) 
  !   print*, '### RH       ', State_Met%RH      (10,10,:) 
  !   print*, '### SPHU     ', State_Met%SPHU    (10,10,:) 
  !   print*, '### T        ', State_Met%T       (10,10,:) 
  !   print*, '### OPTD     ', State_Met%OPTD    (10,10,:)
  !   print*, '### REEVAPCN ', State_Met%REEVAPCN(10,10,:) 
  !   print*, '### REEVAPLS ', State_Met%REEVAPLS(10,10,:) 
  !   print*, '### AREA_M2  ', State_Met%AREA_M2 (10,10,1)
  !ENDIF

  !=========================================================================
  ! Pass advected tracers from internal state to GEOS-Chem tracers array
  !=========================================================================
  DO I = 1, SIZE(Int2Chm,1)
     IF ( Int2Chm(I)%TrcID <= 0 ) CYCLE
     State_Chm%Species(:,:,:,Int2Chm(I)%TrcID) = Int2Chm(I)%Internal
  ENDDO

     ! Flip in the vertical
  State_Chm%Species   = State_Chm%Species( :, :, LM:1:-1, : )

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

        ThisSpc => State_Chm%SpcData(I)%Info

        ! Skip if empty
        IF ( TRIM(ThisSpc%Name) == '' ) CYCLE

        ! Is this a tracer?
        IND = IND_( TRIM(ThisSpc%Name ) )
        IF ( IND < 0 ) CYCLE

        ! Get data from internal state and copy to species array. 
        CALL MAPL_GetPointer( INTERNAL, Ptr3D, 'SPC_'//TRIM(ThisSpc%Name), &
                              notFoundOK=.TRUE., __RC__ )
        IF ( .NOT. ASSOCIATED(Ptr3D) ) THEN
           IF ( MAPL_am_I_Root()) WRITE(*,*) &
              'Could not find species in INTERNAL state - will be initialized to zero: ', &
              TRIM(SPFX), TRIM(ThisSpc%Name)
           State_Chm%Species(:,:,:,IND) = 1e-26
           CYCLE
        ENDIF
        State_Chm%Species(:,:,:,IND) = Ptr3D(:,:,LM:1:-1)

        ! testing only
        if ( MAPL_am_I_Root()) write(*,*) &
        'Initialized species from INTERNAL state: ', TRIM(ThisSpc%Name)
        ThisSpc => NULL()

     ENDDO
  ENDIF
