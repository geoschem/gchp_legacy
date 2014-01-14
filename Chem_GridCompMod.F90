#include "MAPL_Generic.h"

module Chem_GridCompMod

  USE ESMF_Mod
  USE ESMF_DistGridMod
  USE MAPL_Mod

!  USE Chem_GridCompMod,     ONLY : ChemSetServices   => SetServices
  USE GC_Type_Mod
  USE GIGC_Input_Opt_Mod
  USE GIGC_State_Met_Mod
  USE GIGC_State_Chm_Mod
  USE GIGC_MPI_Wrap,        ONLY : mpiComm
  USE GIGC_Chunk_Mod
  USE GIGC_ErrCode_Mod
  USE CharPak_Mod

  implicit none
  PRIVATE

  public SetServices
 
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE                          :: Initialize_     ! Init method
  PRIVATE                          :: Run_            ! Run method  
  PRIVATE                          :: Finalize_       ! Finalize method
  PRIVATE                          :: Extract_        ! Get values from ESMF
!
! !PRIVATE TYPES:
!
  ! Legacy state
  TYPE CHEM_State
     PRIVATE
     TYPE(ESMF_Config)             :: myCF            ! Private ESMF Config obj
  END TYPE CHEM_State

  ! Hook for the ESMF
  TYPE CHEM_Wrap
     TYPE(CHEM_State), POINTER :: PTR => null()   ! Ptr to CHEM_State
  END TYPE CHEM_Wrap

  ! Objects for GEOS-Chem
  TYPE(Spec_2_Trac)                :: Coef            ! Species <-> tracer map  
  TYPE(OptInput)                   :: Input_Opt       ! Input Options
  TYPE(MetState)                   :: State_Met       ! Meteorology state
  TYPE(ChmState)                   :: State_Chm       ! Chemistry state

  INTEGER                          :: CHEM            ! GEOS-Chem Chemistry Operator
  INTEGER                          :: logLun          ! LUN for stdout logfile
  CHARACTER(LEN=ESMF_MAXSTR)       :: logFile         ! File for stdout redirect

contains

    subroutine SetServices ( GC, RC )

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional  , intent(  OUT) :: RC  ! return code

! !DESCRIPTION:  The SetServices for the Chemistry GC needs to register its
!   Initialize and Run.  It uses the MAPL_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs and runs their respective SetServices.
!
    type(esmf_vm) :: vm
    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    INTEGER    :: I
    INTEGER    :: N, ID
    INTEGER    :: NPES, MYID, NX, NY
    TYPE (MAPL_METACOMP), POINTER :: MAPL
    TYPE(CHEM_State), POINTER     :: myState        ! Legacy state
    TYPE(CHEM_Wrap)               :: wrap           ! Wrapper for myState
    CHARACTER(LEN=ESMF_MAXSTR)    :: compName       ! Gridded Component name

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // Iam

! nothing to register for this component
! --------------------------------------

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VmGet(VM, localPet=MYID, petCount=npes, rc=status)
    VERIFY_(STATUS)

   ! Set the Initialize, Run and Finalize entry points
   !--------------------------------------------------
   
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT, Initialize_, RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETRUN, Run_, RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETFINAL, Finalize_, RC=STATUS )
   VERIFY_(STATUS)

! Connect Children
! --------------------
!!    call MAPL_AddConnectivity ( GC,                           &
!!         SRC_NAME  = (/                                   /), &
!!         DST_NAME  = (/                                   /), &
!!         SRC_ID = CHEM,                                        &
!!         DST_ID = ????,                                        &
!!         RC=STATUS  )

   !=======================================================================
   ! Wrap internal state for storing in this gridded component
   ! Rename this to a "legacy state"
   !=======================================================================
   ALLOCATE( myState, stat=STATUS )
   VERIFY_(STATUS)
   wrap%ptr => myState

   myState%myCF = ESMF_ConfigCreate(__RC__)
   call ESMF_ConfigLoadFile( myState%myCF, 'GIGC_GridComp.rc', __RC__)
   
   ! Store internal state with Config object in the gridded component
   CALL ESMF_UserCompSetInternalState( GC, 'CHEM_State', wrap, STATUS )
   VERIFY_(STATUS)
   
    !=======================================================================
    !                    %%% MAPL Data Services %%%
    !=======================================================================
!EOC
!BOP
!
! !IMPORT STATE:
!
#   include "GIGCchem_ImportSpec___.h"

    ! Import from HEMCO
    call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'EMISSIONS',                         &
        LONG_NAME          = 'tracer_surface_emissions',          &
        UNITS              = 'kg/m2/s',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                            __RC__ )

!    call MAPL_AddImportSpec(GC,                                   &
!        SHORT_NAME         = 'DRYDEP',                            &
!        LONG_NAME          = 'tracer_surface_drydep_rates',       &
!        UNITS              = 'm/s',                               &
!        DIMS               = MAPL_DimsHorzOnly,                   &
!        VLOCATION          = MAPL_VLocationNone,                  &
!        DATATYPE           = MAPL_BundleItem,                     &
!                                                            __RC__ )

!
!
! !INTERNAL STATE:
!
#   include "GIGCchem_InternalSpec___.h"
!
! !EXTERNAL STATE:
!
#   include "GIGCchem_ExportSpec___.h"

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'TRACERS',                           &
        LONG_NAME          = 'tracer_volume_mixing_ratios',       &
        UNITS              = 'mol/mol',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                       RC=STATUS  )!EOP
!BOC

! Set services now
! ----------------
   call MAPL_GenericSetServices  ( GC, RC=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize_ 
!
! !DESCRIPTION: Initialize_ is the initialize method of the GIGCchem
!  gridded component.  This is a simple ESMF/MAPL wrapper which calls down
!  to the Initialize method of the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Initialize_( GC, Import, Export, Clock, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import      ! Import state
    TYPE(ESMF_State),    INTENT(INOUT) :: Export      ! Export state
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock       ! ESMF clock object
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now uses the updated Extract_ method.
!  14 Apr 2010 - R. Yantosca - Activated call to GC_CHUNK_INIT
!  15 Apr 2010 - R. Yantosca - Add extra error checks for dimensions
!  23 Apr 2010 - R. Yantosca - Now pass IDENT obj to GC_CHUNK_INIT routine
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now set Ident%VERBOSE to FALSE
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root CPU
!  08 Nov 2012 - R. Yantosca - Now pass options to G-C via Input_Opt object
!  04 Dec 2012 - R. Yantosca - Now get local CPU grid indices as well as the
!                              global grid indices from the Extract_ function
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Objects
    TYPE(ESMF_Grid)             :: Grid        ! ESMF Grid object
    TYPE(ESMF_Config)           :: MaplCF      ! ESMF Config obj (MAPL.rc)
    TYPE(ESMF_Config)           :: GeosCF      ! ESMF Config obj (GIGC*.rc) 
    TYPE(GC_Ident)              :: Ident       ! ID information 
                                       
    ! Scalars                                   
    LOGICAL                     :: am_I_Root   ! Are we on the root CPU?
    INTEGER                     :: error       ! GEOS-Chem error code
    INTEGER                     :: myPet       ! # of the CPU we are on 
    INTEGER                     :: nymdB       ! GMT date @ start of simulation
    INTEGER                     :: nymdE       ! GMT date @ end of simulation
    INTEGER                     :: nymd        ! GMT date @ current time
    INTEGER                     :: nhmsB       ! GMT time @ start of simulation
    INTEGER                     :: nhmsE       ! GMT time @ end of simulation
    INTEGER                     :: nhms        ! GMT time @ current time
    INTEGER                     :: I_LO        ! Min lon index on this CPU
    INTEGER                     :: J_LO        ! Min lat index on this CPU
    INTEGER                     :: I_HI        ! Max lon index on this CPU
    INTEGER                     :: J_HI        ! Max lat index on this CPU
    INTEGER                     :: IM          ! # of longitudes on this CPU
    INTEGER                     :: JM          ! # of latitudes  on this CPU
    INTEGER                     :: LM          ! # of levels     on this CPU
    INTEGER                     :: IM_WORLD    ! # of longitudes in global grid
    INTEGER                     :: JM_WORLD    ! # of latitudes  in global grid
    INTEGER                     :: LM_WORLD    ! # of levels     in global grid
    REAL                        :: tsChem      ! Chemistry timestep [s]
    REAL                        :: tsDyn       ! Dynamic timestep [s]
    CHARACTER(LEN=5)            :: petStr      ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName    ! Name of gridded component
     
    ! Pointer arrays
    REAL(ESMF_KIND_R4), POINTER :: lonCtr(:,:) ! Lon centers on this CPU [rad]
    REAL(ESMF_KIND_R4), POINTER :: latCtr(:,:) ! Lat centers on this CPU [rad]
    REAL, POINTER               :: PS1(:,:)    ! IMPORT: 

    ! Working variables
    TYPE(ESMF_Field)            :: field
    TYPE(ESMF_FieldBundle)      :: bundle
    INTEGER                     :: N

    TYPE(ESMF_Field)            :: hcoFIELD 
    TYPE(ESMF_FieldBundle)      :: hcoBUNDLE 
    CHARACTER(LEN=ESMF_MAXSTR)  :: hcoNAME
    INTEGER                     :: IND, TrcID 

    !=======================================================================
    ! Initialization
    !=======================================================================

    __Iam__('Initialize_')

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    VERIFY_(STATUS)
    Iam = trim( compName ) // '::' // trim( Iam )

!    ! Get my MAPL_Generic state
!    !--------------------------
!    call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
!    VERIFY_(STATUS)

    ! Initialize MAPL Generic
    !--------------------------
    CALL MAPL_GenericInitialize( GC, Import, Export, Clock, __RC__ )
    VERIFY_(STATUS)

    ! Test if we are on the root CPU
    am_I_Root = MAPL_Am_I_Root()

    ! Get various parameters from the ESMF/MAPL framework
    CALL Extract_( GC,                   &  ! Reference to this Gridded Comp 
                   Clock,                &  ! ESMF Clock object
                   Grid     = Grid,      &  ! ESMF Grid object
                   MaplCF   = MaplCF,    &  ! MAPL.rc Configuration object
                   GeosCF   = GeosCF,    &  ! GIGC*.rc Config object
                   I_LO     = I_LO,      &  ! Min longitude index on this CPU
                   J_LO     = J_LO,      &  ! Min latitude  index on this CPU
                   I_HI     = I_HI,      &  ! Max longitude index on this CPU
                   J_HI     = J_HI,      &  ! Max latitude  index on this CPU
                   IM       = IM,        &  ! # of longitudes on this CPU
                   JM       = JM,        &  ! # of latitudes  on this CPU
                   LM       = LM,        &  ! # of levels     on this CPU
                   IM_WORLD = IM_WORLD,  &  ! # of longitudes in global grid
                   JM_WORLD = JM_WORLD,  &  ! # of latitudes  in global grid
                   LM_WORLD = LM_WORLD,  &  ! # of levels     in global grid
                   nymdB    = nymdB,     &  ! YYYYMMDD @ start of simulation
                   nhmsB    = nhmsB,     &  ! hhmmss   @ end   of simulation
                   nymdE    = nymdE,     &  ! YYYMMDD  @ start of simulation
                   nhmsE    = nhmsE,     &  ! hhmmss   @ end   of simulation
                   tsChem   = tsChem,    &  ! Chemistry timestep [seconds]
                   tsDyn    = tsDyn,     &  ! Dynamics timestep  [seconds]
                   localPet = myPet,     &  ! CPU # that we are on now
                   mpiComm  = mpiComm,   &  ! MPI Communicator Handle
                   lonCtr   = lonCtr,    &  ! Lon centers on this CPU [radians]
                   latCtr   = latCtr,    &  ! Lat centers on this CPU [radians]
                   __RC__               )

    ! Name of logfile for stdout redirect
    CALL ESMF_ConfigGetAttribute( GeosCF, Ident%STDOUT_FILE,   &
                                  Label   = "STDOUT_LOGFILE:", &
                                  Default = "PET%%%%%.init",    &
                                   __RC__ )

    ! Name of log LUN # for stdout redirect
    CALL ESMF_ConfigGetAttribute( GeosCF, logLun,              &
                                  Label   = "STDOUT_LOGLUN:",  &
                                  Default = 700,               &
                                   __RC__ )

    ! Fill the remaining fields of the IDENT object
    Ident%STDOUT_LUN = logLun
    Ident%PET        = myPet

    Ident%I_AM(1)    = TRIM( compName )
    Ident%I_AM(2)    = 'Initialize_'
    Ident%LEV        = 2
    Ident%ERRMSG     = ''
    Ident%VERBOSE    = .FALSE.

    !=======================================================================
    ! Open a log file on each PET where stdout will be redirected
    !=======================================================================

    ! Replace tokens w/ PET # in the filename
    IF ( am_I_Root ) THEN
       logFile = Ident%STDOUT_FILE
       WRITE( petStr, '(i5.5)' ) myPet
       CALL StrRepl( logFile, '%%%%%', petStr )
    ENDIF

    ! Open file for stdout redirect
    IF ( am_I_Root )  THEN
!       OPEN ( UNIT=logLun, FILE=TRIM(logFile), STATUS='OLD' , ACTION='WRITE', ACCESS='APPEND' )
       OPEN ( logLun, FILE=LOGFILE, STATUS='UNKNOWN' )
    ENDIF

    ! Add descriptive header text
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(a)'   ) REPEAT( '#', 79 )
       WRITE( logLun, 100     ) TRIM( logFile ), TRIM( Iam ), myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    !=======================================================================
    ! Save values from the ESMF resource file into the Input_Opt object
    ! so that we can pass those into GeosCore/input_mod.F of GEOS-Chem
    !=======================================================================

    ! Max # of diagnostics
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_DIAG,     &
                                  Default = 70,                   &
                                  Label   = "MAX_DIAG:",  __RC__ )    

    ! Max # of tracers
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_TRCS,     &
                                  Default = 100,                  &
                                  Label   = "MAX_TRCS:",  __RC__ )    

    ! Max # of species / family tracer
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_MEMB,     &
                                  Default = 15,                   &
                                  Label   = "MAX_MEMB:",  __RC__ )    

    ! Max # of families per ND65 family tracer (not used for ESMF)
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_FAMS,     &
                                  Default = 20,                   &
                                  Label   = "MAX_FAMS:",  __RC__ )   

    ! 
    CALL ESMF_ConfigGetAttribute( GeosCF, Input_Opt%MAX_DEP,     &
                                  Default = 55,                  &
                                  Label   = "MAX_DEP:",  __RC__ )   

    !=======================================================================
    ! Initialize GEOS-Chem
    !=======================================================================

    ! Call the GIGC initialize routine

    IF ( am_I_Root ) THEN
       write(*,*) '========================'
       write(*,*) 'IM      JM      LM'
       write(*,*) (IM),' ',(JM),' ',(LM)
       write(*,*) (IM_WORLD),' ',(JM_WORLD),' ',(LM_WORLD)
       write(*,*) 
       write(*,*) '========================'
    END IF

    call MAPL_GetPointer ( IMPORT, PS1,  'PS1', RC=STATUS )
    VERIFY_(STATUS)
    State_Met%PS1        = PS1

    CALL GIGC_Chunk_Init( am_I_Root = am_I_Root, &  ! Are we on the root CPU?   
                          I_LO      = I_LO,      &  ! Min lon index on this CPU
                          J_LO      = J_LO,      &  ! Min lat index on this CPU
                          I_HI      = I_HI,      &  ! Max lon index on this CPU
                          J_HI      = J_HI,      &  ! Max lat index on this CPU
                          IM        = IM,        &  ! # lons   on this CPU
                          JM        = JM,        &  ! # lats   on this CPU
                          LM        = LM,        &  ! # levels on this CPU
                          IM_WORLD  = IM_WORLD,  &  ! # lons   in global grid
                          JM_WORLD  = JM_WORLD,  &  ! # lats   in global grid
                          LM_WORLD  = LM_WORLD,  &  ! # levels in global grid
                          nymdB     = nymdB,     &  ! YYYYMMDD @ start of run
                          nhmsB     = nhmsB,     &  ! hhmmss   @ start of run
                          nymdE     = nymdE,     &  ! YYYYMMDD @ end of run
                          nhmsE     = nhmsE,     &  ! hhmmss   @ end of run
                          tsChem    = tsChem,    &  ! Chemical timestep [s]
                          tsDyn     = tsDyn,     &  ! Dynamic  timestep [s]
                          lonCtr    = lonCtr,    &  ! Lon centers [radians]
                          latCtr    = latCtr,    &  ! Lat centers [radians]
                          Input_Opt = Input_Opt, &  ! Input Options obj
                          State_Met = State_Met, &  ! Meteorology State obj
                          State_Chm = State_Chm, &  ! Chemistry State obj
                          RC        = error )       ! Success or failure

    ! Trap the error from GEOS-Chem
    IF ( error /= GIGC_SUCCESS ) THEN 
       CALL Error_Trap_( Ident, error, __RC__ )
    ENDIF

    call ESMF_StateGet(Export, 'TRACERS', bundle, __RC__ )

    DO N = 1, Input_Opt%N_TRACERS
       call ESMF_StateGet ( Export,     &
            trim(State_Chm%Trac_Name(N)), &
            FIELD, __RC__ )

       call ESMF_AttributeSet (field,     &
            NAME  = 'TCVV',               &
            VALUE = Input_Opt%TCVV(State_Chm%Trac_ID(N)),    &
            RC = STATUS )       

       call ESMF_AttributeSet (field,     &
            NAME  = 'TRAC_ID',            &
            VALUE = State_Chm%Trac_Id(N), &
            RC = STATUS )       
       
       call ESMF_FieldBundleAdd ( BUNDLE, FIELD, __RC__ )
    ENDDO

    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LTRAN',              &
         VALUE = Input_Opt%LTRAN,      &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'N_TRACERS',          &
         VALUE = Input_Opt%N_TRACERS,  &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LFILL',              &
         VALUE = Input_Opt%LFILL,      &
         RC = STATUS )       
       
    call ESMF_AttributeSet (bundle,    &
         NAME  = 'LPRT',               &
         VALUE = Input_Opt%LPRT,       &
         RC = STATUS )       

!    call ESMF_AttributeSet (bundle,    &
!         NAME  = 'TCVV',               &
!         ValueList = Input_Opt%TCVV,   &
!         RC = STATUS )       
!    
!    call ESMF_AttributeSet (bundle,     &
!         NAME  = 'TRAC_ID',             &
!         ValueList = State_Chm%Trac_Id, &
!         RC = STATUS )       
       
   if (AM_I_ROOT .and. Input_Opt%LPRT) then
       print *, trim(Iam)//': TRACERS Bundle during Initialize():' 
       call ESMF_FieldBundlePrint ( bundle )
   end if

    !=======================================================================
    ! Match HEMCO species from HEMCO bundle with GEOS-Chem tracers 
    !=======================================================================

    call ESMF_StateGet(Import, 'EMISSIONS', hcoBUNDLE, __RC__ )
    call ESMF_FieldBundleGet( hcoBUNDLE, fieldCount=N, __RC__ )

    DO IND = 1, N 
       call ESMF_FieldBundleGet(hcoBUNDLE, IND, hcoFIELD, __RC__ ) 
       call ESMF_FieldGet( hcoFIELD, NAME=hcoNAME, __RC__)

       ! Check if there is matching ID
       TrcID =  Get_Indx( hcoNAME,              &
                          Input_Opt%ID_Tracer,  &
                          Input_Opt%Tracer_Name  )

       call ESMF_AttributeSet (hcoFIELD,  &
            NAME  = 'GIGCchem_trcID',     &
            VALUE = TrcID,                & 
                                    __RC__ )

       ! eventually add more information here
       
    ENDDO

    !=======================================================================
    ! All done
    !=======================================================================

    ! Write a header before the timestepping begins
    IF ( am_I_Root ) THEN
       WRITE( logLun, '(/,a)' ) REPEAT( '#', 79 )
       WRITE( logLun, 200     ) TRIM( compName ) // '::Run_', myPet
       WRITE( logLun, '(a,/)' ) REPEAT( '#', 79 )
    ENDIF

    ! Close the file for stdout redirect.  Reopen when executing the run method.
    IF ( am_I_Root )  CLOSE ( UNIT=logLun )

    ! Successful return
    RETURN_(ESMF_SUCCESS)

    ! Formats
100 FORMAT( '### ',                                           / &
            '### ', a ,                                       / &
            '### ', a, '  |  Initialization on PET # ', i5.5, / &
            '### ' )
200 FORMAT( '### ',                                           / &
            '### ', a, '  |  Execution on PET # ',      i5.5, / &
            '###' )

  END SUBROUTINE Initialize_

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run_
!
! !DESCRIPTION: Run_ is the run method of the GEOSCHEMchem gridded component.  
!  GC is a simple ESMF/MAPL wrapper which calls down to the Run method of 
!  the GEOS-Chem column chemistry code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run_( GC, Import, Export, Clock, RC )
!
! !USES:
!
#   include "GIGCchem_DeclarePointer___.h"        ! Ptr decls to states
    
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import      ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export      ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock       ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  06 Dec 2009 - A. da Silva - Initial version
!  08 Apr 2010 - R. Yantosca - Now uses the updated Extract_ method
!  09 Apr 2010 - R. Yantosca - Initialize Timing, GeoLoc objects
!  16 Apr 2010 - R. Yantosca - Now move the array assignments before & after
!                              the call to GC_CHUNK_RUN into separate
!                              include files, for clarity
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  02 Jun 2010 - R. Yantosca - Now use IDENT%VERBOSE to trigger debug output
!  09 Oct 2012 - R. Yantosca - Now call MAPL_Am_I_Root to test for root CPU
!  16 Oct 2012 - R. Yantosca - Now Include freeform files Includes_Before_Run.H
!                              and Includes_After_Run.H
!  13 Feb 2013 - R. Yantosca - Now call MAPL_Get_SunInsolation to return
!                              solar zenith angle and related properties
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!  
    ! Objects
    TYPE(ESMF_Grid)              :: Grid          ! ESMF Grid object
    TYPE(ESMF_Config)            :: MaplCF        ! Config (MAPL.rc)
    TYPE(ESMF_Config)            :: GeosCF        ! Config (GIGC*.rc)
    TYPE(GC_Ident)               :: Ident         ! G-C obj for ident info
                                                  
    ! Scalars                                     
    LOGICAL                      :: am_I_Root     ! Are we on the root CPU?
    INTEGER                      :: IM            ! # of lons   on this PET
    INTEGER                      :: JM            ! # of lats   on this PET
    INTEGER                      :: LM            ! # of levels on this PET
    INTEGER                      :: error         ! G-C error return code
    INTEGER(ESMF_KIND_I8)        :: advCount      ! # of clock advances
    INTEGER                      :: nymd          ! YYYY/MM/DD date
    INTEGER                      :: nhms          ! hh:mm:ss time
    INTEGER                      :: myPet         ! PET # we are on now
    INTEGER                      :: nPets         ! Total # of PETs
    INTEGER                      :: I, J, L, LR   ! Loop indices
    INTEGER                      :: N_TRC         ! Shadow var: # of tracers
    INTEGER                      :: year          ! Current year    
    INTEGER                      :: month         ! Current month
    INTEGER                      :: day           ! Current day
    INTEGER                      :: dayOfYr       ! Current day of year
    INTEGER                      :: hour          ! Current hour
    INTEGER                      :: minute        ! Current minute
    INTEGER                      :: second        ! Current second
    REAL                         :: UTC           ! Universal time
    REAL                         :: tsChem        ! Chem timestep [sec]
    REAL                         :: tsDyn         ! Dynamic timestep [sec]
    REAL                         :: hElapsed      ! Elapsed time [hours]
    REAL*4                       :: lonDeg        ! Longitude [degrees]
    REAL*4                       :: latDeg        ! Latitude [degrees]
    REAL                         :: locTime       ! Local time [hours]
    REAL*4                       :: P1, P2        ! Pressure variables
    CHARACTER(LEN=4)             :: petStr        ! String for PET #
    CHARACTER(LEN=ESMF_MAXSTR)   :: compName      ! Gridded Component name
    
    !MSL TEST VALUE 4/20/12
    REAL*4 :: TEST_VAL

    REAL*4,  ALLOCATABLE :: Z(:,:,:)

    ! Allocatable local arrays
    REAL,  ALLOCATABLE, TARGET   :: zenith(:,:)   ! Solar zenith angle
    REAL,  ALLOCATABLE, TARGET   :: solar(:,:)    ! Solar insolation

    ! Pointer arrays
    REAL(ESMF_KIND_R4), POINTER   :: lonCtr(:,:)   ! Lon centers, this CPU [rad]
    REAL(ESMF_KIND_R4), POINTER   :: latCtr(:,:)   ! Lat centers, this CPU [rad]
    
    INTEGER :: IM_WORLD, JM_WORLD
    INTEGER :: I_LO,     J_LO
    INTEGER :: I_HI,     J_HI
    INTEGER :: IND

    ! HEMCO bundle
    INTEGER :: N, trcID
    REAL    :: COEFF
    CHARACTER(LEN=ESMF_MAXSTR)   :: hcoNAME
    TYPE(ESMF_Field      )       :: hcoFIELD      ! HEMCO emission field
    TYPE(ESMF_FieldBundle)       :: hcoBUNDLE     ! HEMCO bundle
    REAL              , POINTER  :: fPtrArray(:,:,:)
    REAL(ESMF_KIND_R8), POINTER  :: fPtrVal, fPtr1D(:)

    !=======================================================================
    ! Initialization
    !=======================================================================
    
    __Iam__('Run_')

    ! Assume success
    error = GIGC_SUCCESS

    ! Are we on the root CPU?
    am_I_Root = MAPL_Am_I_Root()

    ! Traceback info
    CALL ESMF_GridCompGet( GC, name=compName, __RC__ )
    Iam = TRIM( compName ) // '::' // TRIM( Iam )

    ! Get pointers to fields in import, internal, and export states
#   include "GIGCchem_GetPointer___.h"

    ! Add Input_Opt values to GridComponent attributed for passage to 
    ! other grid-comps. Godda be a better way!

    ! Re-open file for stdout redirect
    IF ( am_I_Root )  THEN
     OPEN ( UNIT=logLun, FILE=TRIM(logFile), STATUS='OLD' , ACTION='WRITE', ACCESS='APPEND' )
    END IF

    ! Get various parameters from the ESMF/MAPL framework
    CALL Extract_( GC,                   &  ! Ref to this Gridded Component
                   Clock,                &  ! ESMF Clock object
                   Grid      = Grid,     &  ! ESMF Grid object
                   MaplCf    = MaplCF,   &  ! ESMF Config obj (MAPL*.rc) 
                   GeosCf    = GeosCF,   &  ! ESMF Config obj (GIGC*.rc)
                   tsChem    = tsChem,   &  ! Chemistry timestep [min]
                   tsDyn     = tsDyn,    &  ! Dynamic timestep [min]
                   nymd      = nymd,     &  ! Current YYYY/MM/DD date
                   nhms      = nhms,     &  ! Current hh:mm:ss time
                   year      = year,     &  ! Current year
                   month     = month,    &  ! Current month
                   day       = day,      &  ! Current day
                   dayOfYr   = dayOfYr,  &  ! Current day of year
                   hour      = hour,     &  ! Current hour
                   minute    = minute,   &  ! Current minute
                   helapsed  = hElapsed, &  ! Elapsed hours
                   advCount  = advCount, &  ! # of times clock has advanced
                   IM        = IM,       &  ! # of longitudes on this PET
                   JM        = JM,       &  ! # of latitudes  on this PET
                   LM        = LM,       &  ! # of levels     on this pET
                   IM_WORLD  = IM_WORLD, &
                   JM_WORLD  = JM_WORLD, &
                   I_LO      = I_LO,     &
                   J_LO      = J_LO,     &
                   I_HI      = I_HI,     &
                   J_HI      = J_HI,     &
                   utc       = utc,      &  ! Universal time [hours]
                   localpet  = myPet,    &  ! # of the CPU we are on now
                   petCount  = nPets,    &  ! Total # of CPUs
                   __RC__ )

   ! Make sure we have proper dimension sizes
    ASSERT_( IM >  0                )
    ASSERT_( JM >  0                )
!    ASSERT_( LM <= MAX_COLUMN       )
!    ASSERT_( LM == DimInfo%L_COLUMN )


    ! Allocate GMAO_ZTH (declared at top of module)
    IF ( .not. ALLOCATED( zenith ) ) THEN
       ALLOCATE( zenith( IM, JM ), STAT=STATUS)
       VERIFY_(STATUS)
    ENDIF

       ! Allocate GMAO_SLR (declared @ top of module)
    IF ( .not. ALLOCATED( solar ) ) THEN
       ALLOCATE( solar( IM, JM ), STAT=STATUS)
       VERIFY_(STATUS)
    ENDIF

    IF (.not. ALLOCATED( Z ) ) THEN
       ALLOCATE(Z(IM,JM,LM))
    ENDIF

    ! Call EXTRACT a second time to get the solar zenith
    ! angle and solar insolation fields
    CALL Extract_( GC,                   &  ! Ref to this Gridded Component
                   Clock,                &  ! ESMF Clock object
                   Grid      = Grid,     &  ! ESMF Grid object
                   MaplCf    = MaplCF,   &  ! ESMF Config obj (MAPL*.rc) 
                   GeosCf    = GeosCF,   &  ! ESMF Config obj (GIGC*.rc)
                   lonCtr    = lonCtr,   &  ! Lon centers on this CPU [radians]
                   latCtr    = latCtr,   &  ! Lat centers on this CPU [radians]
                   ZTH       = zenith,   &  ! Solar zenith angle
                   SLR       = solar,    &  ! Solar insolation
                   __RC__ )

    !=======================================================================
    ! Print timing etc. info to the log file outside of the (I,J) loop
    !=======================================================================

    ! Write time quantities
    IF ( am_I_Root ) THEN
       WRITE( logLun, 100 ) year, month, day, hour, minute, hElapsed
    ENDIF

    !=======================================================================
    ! Initialize fields of the IDENT object for each (I,J) location
    !=======================================================================
    Ident%STDOUT_FILE = ''
    Ident%STDOUT_LUN  = logLun
    Ident%PET         = myPet
    Ident%I_AM(1)     = TRIM( compName )
    Ident%I_AM(2)     = 'Run_'
    Ident%LEV         = 2
    Ident%ERRMSG      = ''

    !=======================================================================
    ! Populate tracer tendency array in State_Chm 
    ! ==> Get arrays from HEMCO via hcoBUNDLE
    !=======================================================================

    call ESMF_StateGet(IMPORT, 'EMISSIONS', hcoBUNDLE,  __RC__ )    
    call ESMF_FieldBundleGet(hcoBUNDLE, fieldCount=N,   __RC__ ) 
    DO IND=1, N 
       call ESMF_FieldBundleGet(hcoBUNDLE, IND, hcoFIELD, __RC__ ) 
       call ESMF_FieldGet( hcoFIELD, NAME=hcoNAME, __RC__)
       call ESMFL_BundleGetPointerToData( hcoBUNDLE, IND, fPtrArray, __RC__)

       ! Extract species ID
       call ESMF_AttributeGet (hcoFIELD,    &
            NAME  = 'GIGCchem_trcID',       &
            VALUE = trcID,            __RC__ )

       ! Conversion factor (kg/m2/s -> molec/cm2/s)
       COEFF = Input_Opt%XNUMOL(trcID) / 1.0d4

       ! Pass to State_Chm. Don't reverse vertical (right?!)
       State_Chm%Trac_Tend(:,:,:,trcID) = fPtrArray !* COEFF
    END DO

    !=======================================================================
    ! pre-Run method array assignments
    !=======================================================================

#   include "Includes_Before_Run.H"
    
    State_Met%AIRVOL = 1.e0

    where (State_Chm%Tracers .eq. 0.e0)
       State_Chm%Tracers = 1.e-36
    end where

    !=======================================================================
    ! If import restart does not exist, wait until next pass to run GIGC
    !=======================================================================

    IF(Input_Opt%haveImpRst) THEN
    !=======================================================================
    ! Execute GEOS-Chem on multiple PETs
    !=======================================================================

    ! Run the GEOS-Chem column chemistry code
     CALL GIGC_Chunk_Run( am_I_Root = am_I_Root,  &  ! Are we on the root CPU?
                          IM        = IM,         &  ! # of lons on this CPU
                          JM        = JM,         &  ! # of lats on this CPU
                          LM        = LM,         &  ! # of levs on this CPU
                          nymd      = nymd,       &  ! Current YYYY/MM/DD date
                          nhms      = nhms,       &  ! Current hh:mm:ss time
                          year      = year,       &  ! Current year
                          month     = month,      &  ! Current month
                          day       = day,        &  ! Current day
                          dayOfYr   = dayOfYr,    &  ! Current day of year
                          hour      = hour,       &  ! Current hour
                          minute    = minute,     &  ! Current minute
                          second    = second,     &  ! Current second
                          utc       = utc,        &  ! Current UTC time [hrs]
                          hElapsed  = hElapsed,   &  ! Elapsed hours
                          Input_Opt = Input_Opt,  &  ! Input Options object
                          State_Chm = State_Chm,  &  ! Chemistry State object
                          State_Met = State_Met,  &  ! Meteorology State object
                          RC        = error )        ! Success or failure?

    ! Trap the error from GEOS-Chem
     IF ( error /= GIGC_SUCCESS ) THEN 
        CALL Error_Trap_( Ident, error, __RC__ )
     ENDIF

    END IF
    !PRINT *,"Setting haveImpRst to TRUE"
    Input_Opt%haveImpRst = .TRUE.

    !=======================================================================
    ! post-Run method array assignments
    !=======================================================================

#   include "Includes_After_Run.H"

    !=======================================================================
    ! All done
    !=======================================================================

    IF ( ALLOCATED( zenith ) ) DEALLOCATE( zenith )
    IF ( ALLOCATED( solar  ) ) DEALLOCATE( solar  )

    ! Close the file for stdout redirect.
    IF ( am_I_Root )  CLOSE ( UNIT=logLun )

    ! Successful return
    RETURN_(ESMF_SUCCESS)

    ! Formats
100 FORMAT( '---> DATE: ', i4.4, '/', i2.2, '/', i2.2,      &
            '  GMT: ', i2.2, ':', i2.2, '  X-HRS: ', f11.3 )
110 FORMAT( 'Box (',i3,',',i3,') on PET ', i3, ' has coords: ', 2f7.2, &
               ' LocT = ', f9.4 )

  end subroutine Run_

  subroutine Finalize_
  end subroutine Finalize_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1 and      !
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Extract_
!
! !DESCRIPTION: GC routine extracts several common quantities from the 
!  ESMF/MAPL environment so that they can be later passed down to the 
!  grid-independent GEOS-Chem code.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Extract_( GC,       Clock,    Grid,    MaplCF, GeosCF,    &
                       localPet, petCount, I_LO,    J_LO,   I_HI,      &
                       J_HI,     IM,       JM,      LM,     IM_WORLD,  &
                       JM_WORLD, LM_WORLD, lonCtr,  latCtr, advCount,  &
                       nymdB,    nymdE,    nymd,    nhmsB,  nhmsE,     &
                       nhms,     year,     month,   day,    dayOfYr,   &
                       hour,     minute,   second,  utc,    hElapsed,  &
                       tsChem,   tsDyn,    mpiComm, ZTH,   SLR,        &
                       queryRst, RC )

!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!                                                             
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC          ! GC grid comp
!                                                             
! !OUTPUT PARAMETERS:                                         
!              
    !----------------------------------
    ! ESMF and/or MAPL quantities
    !----------------------------------
    TYPE(ESMF_Grid),     INTENT(OUT), OPTIONAL :: Grid        ! ESMF Grid obj
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: MaplCF      ! MAPL.rc
    TYPE(ESMF_Config),   INTENT(OUT), OPTIONAL :: GeosCF      ! GIGC*.rc
    INTEGER,             INTENT(OUT), OPTIONAL :: localPet    ! This PET
    INTEGER,             INTENT(OUT), OPTIONAL :: petCount    ! Total # of PETs
    INTEGER,             INTENT(OUT), OPTIONAL :: mpiComm     ! MPI Communicator Handle

    !----------------------------------
    ! Local grid coordinates 
    ! (defined on the current CPU)
    !----------------------------------
    INTEGER,             INTENT(OUT), OPTIONAL :: I_LO        ! Min lon index
    INTEGER,             INTENT(OUT), OPTIONAL :: J_LO        ! Min lat index
    INTEGER,             INTENT(OUT), OPTIONAL :: I_HI        ! Max lon index
    INTEGER,             INTENT(OUT), OPTIONAL :: J_HI        ! Max lat index
    INTEGER,             INTENT(OUT), OPTIONAL :: IM          ! Total # lons
    INTEGER,             INTENT(OUT), OPTIONAL :: JM          ! Total # lats
    INTEGER,             INTENT(OUT), OPTIONAL :: LM          ! Total # levs
    REAL(ESMF_KIND_R4),  POINTER,     OPTIONAL :: lonCtr(:,:) ! Lon ctrs [rad]
    REAL(ESMF_KIND_R4),  POINTER,     OPTIONAL :: latCtr(:,:) ! Lat ctrs [rad]

    !----------------------------------
    ! Global grid coordinates
    !----------------------------------
    INTEGER,             INTENT(OUT), OPTIONAL :: IM_WORLD    ! Global # lons
    INTEGER,             INTENT(OUT), OPTIONAL :: JM_WORLD    ! Global # lats
    INTEGER,             INTENT(OUT), OPTIONAL :: LM_WORLD    ! Global # levs
                                                              
    !----------------------------------
    ! Date and time variables
    !----------------------------------
   INTEGER(ESMF_KIND_I8),INTENT(OUT), OPTIONAL :: advCount    ! # of clock advs
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdB       ! YYYYMMDD @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nymdE       ! YYYYMMDD @ end
    INTEGER,             INTENT(OUT), OPTIONAL :: nymd        ! YYYYMMDD now
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsB       ! hhmmss @ start
    INTEGER,             INTENT(OUT), OPTIONAL :: nhmsE       ! hhmmss @ end
    INTEGER,             INTENT(OUT), OPTIONAL :: nhms        ! hhmmss now
    INTEGER,             INTENT(OUT), OPTIONAL :: year        ! UTC year 
    INTEGER,             INTENT(OUT), OPTIONAL :: month       ! UTC month
    INTEGER,             INTENT(OUT), OPTIONAL :: day         ! UTC day
    INTEGER,             INTENT(OUT), OPTIONAL :: dayOfYr     ! UTC day of year
    INTEGER,             INTENT(OUT), OPTIONAL :: hour        ! UTC hour
    INTEGER,             INTENT(OUT), OPTIONAL :: minute      ! UTC minute
    INTEGER,             INTENT(OUT), OPTIONAL :: second      ! UTC second
    REAL,                INTENT(OUT), OPTIONAL :: utc         ! UTC time [hrs]
    REAL,                INTENT(OUT), OPTIONAL :: hElapsed    ! Elapsed hours

    !-----------------------------------                     
    ! Timestep variables [seconds]          
    !-----------------------------------                     
    REAL,                INTENT(OUT), OPTIONAL :: tsChem      ! Chemistry
    REAL,                INTENT(OUT), OPTIONAL :: tsDyn       ! Dynamics

    !-----------------------------------                     
    ! Solar parameters
    !-----------------------------------                     
    REAL,                INTENT(OUT), OPTIONAL :: ZTH(:,:)    ! Solar zth angle
    REAL,                INTENT(OUT), OPTIONAL :: SLR(:,:)    ! Insolation

    !-----------------------------------------------
    ! Optional import restart file existence inquiry
    !-----------------------------------------------
    LOGICAL,             INTENT(IN),  OPTIONAL :: queryRst    ! Ask if import restart file exists

    !-----------------------------------                        
    ! Return code 
    !-----------------------------------                     
    INTEGER,             INTENT(OUT), OPTIONAL :: RC          ! 0 = all is well
!
! !REMARKS:
!  If you need to obtain a quantity not returned by this routine, you can
!  manually extract it from the MaplCF or GeosCF configuration objects.
!
! !REVISION HISTORY:
!  01 Dec 2009 - A. Da Silva - Initial version
!  07 Apr 2010 - R. Yantosca - Added ProTeX headers
!  08 Apr 2010 - R. Yantosca - Make all outputs optional
!  08 Apr 2010 - R. Yantosca - Added outputs for localPet, petCount
!  08 Apr 2010 - R. Yantosca - Added outputs for individual time values
!                              as well as elapsed time (hours)
!  13 Apr 2010 - R. Yantosca - Now take tsDyn from the MAPL "RUN_DT:" setting
!  30 Nov 2012 - R. Yantosca - Now return IM_WORLD, JM_WORLD, LM_WORLD
!  30 Nov 2012 - R. Yantosca - Now return local indices I_LO, J_LO, I_HI, J_HI
!  05 Dec 2012 - R. Yantosca - Removed latEdg argument; cosmetic changes
!  13 Feb 2013 - E. Nielsen  - Restart file inquiry for GEOS-5
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
! 
    ! Objects
    TYPE(ESMF_Time)               :: startTime      ! ESMF start time obj
    TYPE(ESMF_Time)               :: currTime       ! ESMF current time obj
    TYPE(ESMF_TimeInterval)       :: elapsedTime    ! ESMF elapsed time obj
    TYPE(ESMF_VM)                 :: VM             ! ESMF VM object
    TYPE(CHEM_State), POINTER     :: myState        ! Legacy state
    TYPE(CHEM_Wrap)               :: wrap           ! Wrapper for myState
    TYPE(MAPL_MetaComp),  POINTER :: metaComp       ! MAPL MetaComp object
    TYPE(MAPL_SunOrbit)           :: sunOrbit

    ! Scalars
    CHARACTER(len=ESMF_MAXSTR)    :: compName       ! Gridded component name
    CHARACTER(len=ESMF_MAXSTR)    :: importRstFN    ! Import restart file name
    INTEGER(ESMF_KIND_I8)         :: count          ! # of clock advances
    INTEGER                       :: locDims(3)     ! Array for local dims
    INTEGER                       :: globDims(3)    ! Array for global dims
    INTEGER                       :: doy            ! Day of year (0-365/366)
    INTEGER                       :: yyyy, mm, dd   ! Year, month, day
    INTEGER                       :: h,    m,  s    ! Hour, minute, seconds
    INTEGER                       :: IL,   IU       ! Min/max local lon indices
    INTEGER                       :: JL,   JU       ! Min/max local lat indices
    REAL                          :: elapsedHours   ! Elapsed hours of run

    !=======================================================================
    ! Initialization
    !=======================================================================

    __Iam__('Extract_')

    ! Get my name and set-up traceback handle
    CALL ESMF_GridCompGet( GC, name=compName, vm=VM, __RC__ )
    Iam = TRIM( compName ) // '::' // TRIM( Iam )

    ! Get the internal state which holds the private Config object
    CALL ESMF_UserCompGetInternalState( GC, 'CHEM_State', wrap, STATUS )
    VERIFY_(STATUS)
    myState => wrap%ptr

    ! Assume successful return
    IF ( PRESENT( RC ) ) RC = ESMF_SUCCESS

    ! Zero variables
    locDims  = 0
    globDims = 0
    IL       = 0
    JL       = 0
    IU       = 0
    JU       = 0

    !=======================================================================
    ! Extract information from ESMF VM object
    !=======================================================================

    ! Index of the PET we are on now
    IF ( PRESENT( localPet ) ) THEN
       CALL ESMF_VmGet( VM, localPet=localPet, __RC__ )
    ENDIF

    ! Total # of PETs used by this gridded component
    IF ( PRESENT( petCount ) ) THEN
       CALL ESMF_VmGet( VM, petCount=petCount, __RC__ )
    ENDIF

    ! Global MPI Communicator Handle
    IF ( PRESENT( mpiComm ) ) THEN
       CALL ESMF_VmGet( VM, mpicommunicator=mpiComm, __RC__ )
    ENDIF

    !=======================================================================
    ! Extract information from ESMF Config objects
    !=======================================================================

    ! Get the Config object based on "MAPL.rc"
    CALL ESMF_GridCompGet( GC, Config=MaplCF, __RC__ )
    
    ! Get the Config object based on "GIGCchem_GridComp.rc"
    GeosCF = myState%myCF

    ! Dynamic timestep (convert to minutes)
    IF ( PRESENT( tsDyn ) ) THEN
       CALL ESMF_ConfigGetAttribute( MaplCF, tsDyn,                       &
                                     Label="RUN_DT:",             __RC__ )
    ENDIF

    ! Chemistry timestep (convert to minutes)
    IF ( PRESENT( tsChem ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, tsChem,                      &
                                     Label="CHEMISTRY_TIMESTEP:", __RC__ )
    ENDIF

    ! Start date
    IF ( PRESENT( nymdb ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nymdB,                       &
                                     Label   = "UTC_START_DATE:", __RC__ )
    ENDIF

    ! Start time
    IF ( PRESENT( nhmsB ) ) then
       CALL ESMF_ConfigGetAttribute( GeosCF, nhmsB,                       &
                                     LABEL   = "UTC_START_TIME:", __RC__ )
    ENDIF

    ! End date
    IF ( PRESENT( nymdE ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nymdE,                       &
                                     Label   = "UTC_END_DATE:",   __RC__ )
    ENDIF

    ! End time
    IF ( PRESENT( nhmsE ) ) THEN
       CALL ESMF_ConfigGetAttribute( GeosCF, nhmsE,                       &
                                     LABEL   = "UTC_END_TIME:",  __RC__ )
    ENDIF

    !=======================================================================
    ! Does the import restart file exist?
    !=======================================================================
    
    CALL ESMF_ConfigGetAttribute( GeosCF, importRstFN,                   &
                           DEFAULT = "geoschemchem_import_rst",          &
                           LABEL   = "importRestartFileName:",  __RC__ )

    
    IF ( PRESENT( queryRst ) .AND. queryRst == .TRUE. ) THEN
     INQUIRE(FILE=TRIM(importRstFN), EXIST=Input_Opt%haveImpRst)
     IF( MAPL_AM_I_ROOT() ) THEN
      PRINT *," ",TRIM(importRstFN)," exists: ",Input_Opt%haveImpRst
      PRINT *," "
     END IF
    END IF

    !=======================================================================
    ! Extract time/date information
    !=======================================================================
    
    ! Get the ESMF time object
    CALL ESMF_ClockGet( Clock,                    &
                        startTime    = startTime, &
                        currTime     = currTime,  &
                        advanceCount = count,     &
                         __RC__ )

    ! Get individual fields from the time object
    CALL ESMF_TimeGet( currTime, yy=yyyy, mm=mm, dd=dd, dayOfYear=doy, &
                                 h=h,     m=m,   s=s,   __RC__ )

    ! Save fields for return
    IF ( PRESENT( nymd     ) ) CALL MAPL_PackTime( nymd, yyyy, mm, dd )
    IF ( PRESENT( nhms     ) ) CALL MAPL_PackTime( nhms, h,    m,  s  )
    IF ( PRESENT( advCount ) ) advCount = count
    IF ( PRESENT( year     ) ) year     = yyyy
    IF ( PRESENT( month    ) ) month    = mm
    IF ( PRESENT( day      ) ) day      = dd
    IF ( PRESENT( dayOfYr  ) ) dayOfYr  = doy
    IF ( PRESENT( hour     ) ) hour     = h
    IF ( PRESENT( minute   ) ) minute   = m
    IF ( PRESENT( second   ) ) second   = s
    IF ( PRESENT( utc      ) ) utc      = ( DBLE( h )        ) + & 
                                          ( DBLE( m )/60d0   ) + &
                                          ( DBLE( s )/3600d0 )
 
    ! Compute elapsed time since start of simulation
    elapsedTime = currTime - startTime

    ! Get time fields from the elapsedTime object
    CALL ESMF_TimeIntervalGet( elapsedTime, h=h, m=m, s=s, __RC__ )

    ! Convert to decimal hours
    elapsedHours = DBLE( h ) + ( DBLE( m )/60d0 ) + ( DBLE( s )/3600d0 )
    
    ! Save fields for return
    IF ( PRESENT( hElapsed ) ) hElapsed = elapsedHours

    !=======================================================================
    ! Extract grid information
    !=======================================================================
    IF ( PRESENT( Grid ) ) THEN
    
       ! Get the ESMF grid attached to this gridded component
       CALL ESMF_GridCompGet( GC, grid=Grid, __RC__ )

       ! Get # of dimensions on this pet, and globally
       CALL MAPL_GridGet( Grid,                                        &
                          localCellCountPerDim  = locDims,             &
                          globalCellCountPerDim = globDims,            &
                          __RC__ )
          
       ! Get the upper and lower bounds of on each PET
       CALL GridGetInterior( Grid, IL, IU, JL, JU, __RC__  )

    ENDIF

    ! Save fields for return
    IF ( PRESENT( I_LO     ) ) I_LO     = IL
    IF ( PRESENT( J_LO     ) ) J_LO     = JL
    IF ( PRESENT( I_HI     ) ) I_HI     = IU
    IF ( PRESENT( J_HI     ) ) J_HI     = JU
    IF ( PRESENT( IM       ) ) IM       = locDims(1)
    IF ( PRESENT( JM       ) ) JM       = locDims(2)
    IF ( PRESENT( LM       ) ) LM       = locDims(3)
    IF ( PRESENT( IM_WORLD ) ) IM_WORLD = globDims(1)
    IF ( PRESENT( JM_WORLD ) ) JM_WORLD = globDims(2)
    IF ( PRESENT( LM_WORLD ) ) LM_WORLD = globDims(3)

    ! Get horizontal coordinate variables
    CALL MAPL_GetObjectFromGC( GC, metaComp, __RC__ )

    ! Longitude values on this PET
    IF ( PRESENT( lonCtr ) ) THEN
       CALL MAPL_Get( metaComp, lons=lonCtr, __RC__ )
    ENDIF

    ! Latitude values on this PET
    IF ( PRESENT( latCtr ) ) THEN
       CALL MAPL_Get( metaComp, lats=latCtr, __RC__ )
    ENDIF

    !=======================================================================
    ! Get solar zenith angle enformation
    !=======================================================================
    IF ( PRESENT( ZTH    ) .and. PRESENT( SLR    )  .and. &
         PRESENT( lonCtr ) .and. PRESENT( latCtr ) ) THEN
         
       ! Get the Orbit object (of type MAPL_SunOrbit),
       ! which is used in the call to MAPL_SunGetInsolation
       CALL MAPL_Get( metaComp,                       &
                      LONS      = lonCtr,             &
                      LATS      = latCtr,             &
                      ORBIT     = sunOrbit,           &
                      __RC__                         )

       ! Get the solar zenith angle and solar insolation
       ! NOTE: ZTH, SLR are allocated outside of this routine
       CALL MAPL_SunGetInsolation( LONS  = lonCtr,    &
                                   LATS  = latCtr,    &
                                   ORBIT = sunOrbit,  &
                                   ZTH   = ZTH,       &
                                   SLR   = SLR,       &
                                   CLOCK = Clock,     &
                                   __RC__            )

    ENDIF

    !=======================================================================
    ! All done
    !=======================================================================
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Extract_
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Error_Trap_
!
! !DESCRIPTION: This routine stops the run and prints the name of the
!  offending routine if an error condition is returned.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Error_Trap_( Ident, error, RC )
!
! !INPUT PARAMETERS: 
!
    TYPE(GC_IDENT),   INTENT(IN)  :: Ident      ! Obj w/ info from ESMF
    INTEGER,          INTENT(IN)  :: error      ! Error code from GEOS-Chem
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT) :: RC         ! Return error code
!
! !REVISION HISTORY: 
!  22 Jun 2009 - R. Yantosca - Initial version
!  15 Jul 2009 - R. Yantosca - Updated for drydep, wetdep, PBL mixing
!  24 Aug 2009 - R. Yantosca - Updated for emissions reader etc. routines
!  03 Nov 2009 - R. Yantosca - Now trap error in the GC_INTERFACE
!  03 Nov 2009 - R. Yantosca - Cosmetic changes
!  14 Dec 2009 - R. Yantosca - Now trap errors in the GC_CHUNK routines
!  14 Apr 2010 - R. Yantosca - Adapted for use in GEOSCHEMchem_GridCompMod.F90
!  29 Apr 2010 - R. Yantosca - Now print error traceback info from IDENT
!  30 Apr 2010 - R. Yantosca - Now use 5 digits for PET
!  06 May 2010 - R. Yantosca - Remove redundant error codes
!  03 Jun 2010 - R. Yantosca - Remove more redundant error codes
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: N

    !=======================================================================
    ! Initialization
    !=======================================================================

     __Iam__('Error_Trap_')

    ! Traceback info
    Iam = TRIM( Ident%I_AM(1) ) // '::' // TRIM( Iam )

    !=======================================================================
    ! Error trap
    !=======================================================================

    IF ( error == GIGC_FAILURE ) THEN
       
       !--------------------------------------------------
       ! Print error traceback information
       !--------------------------------------------------
   
       ! Begin header
       WRITE( logLun, '(/,a)' ) REPEAT( '=', 79 )
       WRITE( logLun,  20     ) Ident%PET
       WRITE( logLun,  21     ) TRIM( Ident%ERRMSG )
       WRITE( logLun, '(/,a) ') 'Error traceback:' 

       ! Write the calling sequence of routines
       DO N = Ident%LEV, 1, -1
          WRITE( loglun, 22   ) N, TRIM( Ident%I_AM(N) )
       ENDDO

       ! End header
       WRITE( logLun, '(a,/)' ) REPEAT( '=', 79 )
       CLOSE( logLun          )

       ! Return w/ failure
       RETURN_(ESMF_FAILURE)

    ELSE

       ! This is now deprecated
       PRINT*, 'RC', error
       WRITE( logLun, 10 ) 'UNKNOWN FAILURE',                    Ident%PET
       RETURN_(ESMF_FAILURE)

    ENDIF

    ! FORMAT string
 10 FORMAT( a, ' PET=', i5.5                             )
 20 FORMAT( 'GEOS-Chem error encountered on PET: ', i5.5 )
 21 FORMAT( 'Message: ', a                               )
 22 FORMAT( i4, ' : ', a                                 )


  END SUBROUTINE Error_Trap_
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gridGetInterior
!
! !DESCRIPTION: Given an ESMF grid, returns the lower and upper longitude
!  and latitude indices on a given PET.
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE GridGetInterior( Grid, I1, IN, J1, JN, RC )
!
! !INPUT PARAMETERS: 
!
    TYPE(ESMF_Grid), INTENT(IN)  :: Grid   ! ESMF Grid object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(OUT) :: I1     ! Lower lon index on this PET
    INTEGER,         INTENT(OUT) :: IN     ! Upper lon index on this PET
    INTEGER,         INTENT(OUT) :: J1     ! Lower lat index on this PET
    INTEGER,         INTENT(OUT) :: JN     ! Upper lat index on this PET
    INTEGER,         INTENT(OUT) :: RC     ! Success/failure
!
! !REMARKS:
!  This was a PRIVATE routine named MAPL_GridGetInterior within MAPL_Base.F90.
!  I have pulled the source code from there.
! 
! !REVISION HISTORY: 
!  30 Nov 2012 - R. Yantosca - Initial version, based on MAPL_Base
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                               :: status
    CHARACTER(LEN=ESMF_MAXSTR)            :: IAm='MAPL_GridGetInterior'

    TYPE(ESMF_DistGrid)                   :: distGrid
    TYPE(ESMF_DELayout)                   :: LAYOUT
    INTEGER,               ALLOCATABLE    :: AL(:,:)
    INTEGER,               ALLOCATABLE    :: AU(:,:)
    INTEGER                               :: nDEs
    INTEGER                               :: deId
    INTEGER                               :: gridRank
    INTEGER                               :: deList(1)

    ! Get ESMF DistGrid object
    CALL ESMF_GridGet    ( GRID,                           &
                           dimCount        = gridRank,     &
                           distGrid        = distGrid,     &
                           __RC__ )                    
 
    ! Get ESMF DELayout object
    CALL ESMF_DistGridGet( distGRID,                       &
                           delayout        =layout,        &
                           __RC__ )                    
                                                       
                
    ! Get the # of DE's and the list of DE's
    CALL ESMF_DELayoutGet( layout,                         &
                           deCount         = nDEs,         &
                           localDeList     = deList,       &
                           __RC__ )

    deId = deList(1)

    ! Allocate memory
    ALLOCATE( AL( gridRank, 0:nDEs-1 ), stat=status )
    ALLOCATE( AU( gridRank, 0:nDEs-1 ), stat=status )

    ! Get the min/max lon/lat values on each PET
    CALL ESMF_DistGridGet( distgrid,                       &
                           minIndexPDimPDe = AL,           &
                           maxIndexPDimPDe = AU,           &
                           __RC__ )

    ! Local Lon indices
    I1 = AL( 1, deId )   ! Lower
    IN = AU( 1, deId )   ! Upper

    ! Local lat indices
    J1 = AL( 2, deId )   ! Lower
    JN = AU( 2, deId )   ! Upper
 
    ! Free memory
    DEALLOCATE(AU, AL)
   
    ! Return successfully
    RC = GIGC_SUCCESS

  END SUBROUTINE GridGetInterior
!EOC

end module Chem_GridCompMod
