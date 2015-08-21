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
    State_Met%MOLENGTH(:,:) = &
         -AIRDENS(:,:,LM) * MAPL_CP * TS * ustar**3. / (MAPL_KARMAN * MAPL_GRAV * State_Met%HFLUX)
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
        State_Met%PEDGE(I,J,L)   = PLE(  I, J, LR) / 1d2  ! Pa -> hPa (mb)
        State_Met%CMFMC(I,J,L)   = CMFMC(I, J, L )        ! kg/m2/s
        State_Met%DQRCU(I,J,L)   = DQRCU( I, J, L )
        State_Met%PFICU(I,J,L)   = PFICU(I, J, L )
        State_Met%PFILSAN(I,J,L) = PFILSAN(I, J, L )
        State_Met%PFLCU(I,J,L)   = PFLCU(I, J, L )
        State_Met%PFLLSAN(I,J,L) = PFLLSAN(I, J, L )
     ENDDO
     ENDDO
  ENDDO

  !=========================================================================
  ! Three-dimensional fields on level centers
  !=========================================================================

  State_Met%CLDF      = CLDF           ! unitless
  State_Met%DTRAIN    = DTRAIN         ! kg/m2/s
  State_Met%QI        = QI             ! kg/kg
  State_Met%QL        = QL             ! kg/kg
  State_Met%RH        = RH      * 1d2  ! unitless->[%]
  State_Met%SPHU      = SPHU1   * 1d3  ! kg/kg -> g/kg
  State_Met%T         = TMPU1          ! K
  State_Met%TAUCLI    = TAUCLI         ! unitless
  State_Met%TAUCLW    = TAUCLW         ! unitless
  State_Met%REEVAPCN  = REEVAPCN
  State_Met%REEVAPLS  = REEVAPLS

        ! Total optical depth [unitless]
  State_Met%OPTD      = ( State_Met%TAUCLI               &
				 +   State_Met%TAUCLW  ) &
                                 *   State_Met%CLDF
  
  State_Met%AREA_M2 (:,:,1)  = AREA    ! m2

  !=========================================================================
  ! Pass advected tracers from internal state to GEOS-Chem tracers array
  !=========================================================================
  DO I = 1, SIZE(Int2Chm,1)
     IF ( Int2Chm(I)%TrcID <= 0 ) CYCLE
     State_Chm%Tracers(:,:,:,Int2Chm(I)%TrcID) = Int2Chm(I)%Internal
  ENDDO

     ! Flip in the vertical
  State_Chm%Tracers   = State_Chm%Tracers( :, :, LM:1:-1, : )

  IF ( IsChemTime .AND. Phase /= 1 ) THEN
     ! Also fill JLOP_PREVIOUS from internal state
     CALL MAPL_GetPointer( INTSTATE, Ptr3D, 'JLOP_PREV', notFoundOK=.TRUE., __RC__ )
     IF ( ASSOCIATED(Ptr3D) .AND. ALLOCATED(JLOP_PREVIOUS) ) THEN
         JLOP_PREVIOUS = NINT(Ptr3D)
     ENDIF
     Ptr3D => NULL()
  ENDIF

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
        IF ( IND < 0 ) CYCLE

        ! Get data from internal state and copy to species array. 
        CALL MAPL_GetPointer( INTERNAL, Ptr3D, TRIM(SPFX)//TRIM(State_Chm%Spec_Name(I)), &
                              notFoundOK=.TRUE., __RC__ )
        IF ( .NOT. ASSOCIATED(Ptr3D) ) THEN
           IF ( MAPL_am_I_Root()) WRITE(*,*) &
              'Could not find species in INTERNAL state - will be initialized to zero: ', &
              TRIM(SPFX), TRIM(State_Chm%Spec_Name(I))
           State_Chm%Species(:,:,:,State_Chm%Spec_ID(I)) = 1e-26
           CYCLE
        ENDIF
        State_Chm%Species(:,:,:,State_Chm%Spec_ID(I)) = Ptr3D(:,:,LM:1:-1)

        ! testing only
        if ( MAPL_am_I_Root()) write(*,*) &
        'Initialized species from INTERNAL state: ', TRIM(State_Chm%Spec_Name(I))

     ENDDO
  ENDIF
