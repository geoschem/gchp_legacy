#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: 
! GIGC\_GridCompMod -- this gridded component builds an ESMF application out
! of the following three components:
! - Dynamics  (GIGCdyn)
! - Chemistry (GIGCchem)
! - Emissions (HEMCO)
!
! !NOTES:
! (1) For now, the dynamics module is rather primitive and based upon netCDF
! input fields for all met. variables (U,V,etc.). This module computes 
! pressure related quantities (PEDGE, PCENTER, BOXHEIGHT, AIRDEN, AD, DELP, 
! AIRVOL) and passes these variables to chemistry and HEMCO.
! Similarly, HEMCO inherits the grid box surface area from dynamics.
! (2) Tracers and emissions are passed as 'bundle' from one component to
! another.
! (3) All 3D fields are exchanged on the 'GEOS-5' vertical levels, i.e. with
! a reversed atmosphere (level 1 is top of atmosphere). 
! 
! !INTERFACE:

module GIGC_GridCompMod

! !USES:

  use ESMF_Mod
  use MAPL_Mod

  use CHEM_GridCompMod,       only : AtmosChemSetServices     => SetServices
  use DYN_GridCompMod,        only : AtmosDynSetServices      => SetServices
  use HEMCO_GridCompMod,      only : AtmosEmisSetServices     => SetServices

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!=============================================================================

! !DESCRIPTION:
 
!EOP

  integer ::        CHEM, DYN, EMIS 

contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

    subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer,             intent(  OUT) :: RC  ! return code

! !DESCRIPTION:  The SetServices for the Physics GC needs to register its
!   Initialize and Run.  It uses the MAPL_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs (SURF, CHEM, RADIATION, MOIST, TURBULENCE) and runs their
!   respective SetServices.

!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    CHARACTER(LEN=ESMF_MAXSTR)              :: RATsProviderName
    integer                                 :: I
    type (ESMF_Config)                      :: CF
    logical                                 :: am_I_Root

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "::" // Iam

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT, Initialize, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETRUN,  Run,        RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETFINAL,  Finalize, RC=STATUS )
    VERIFY_(STATUS)
!BOP

! !IMPORT STATE:

! !EXPORT STATE:

! Create children`s gridded components and invoke their SetServices
! -----------------------------------------------------------------

    CHEM = MAPL_AddChild(GC, NAME='GIGCchem', SS=AtmosChemSetServices, RC=STATUS)
    VERIFY_(STATUS)

    EMIS = MAPL_AddChild(GC, NAME='HEMCO',   SS=AtmosEmisSetServices, RC=STATUS)
    VERIFY_(STATUS)

    DYN  = MAPL_AddChild(GC, NAME='GIGCdyn',  SS=AtmosDynSetServices, RC=STATUS)
    VERIFY_(STATUS)

! Set internal connections between the children`s IMPORTS and EXPORTS
! -------------------------------------------------------------------
#include "GIGC_Connections.H"
!BOP

! !CONNECTIONS:

! Chemistry Imports
! -----------------

!EOP

    call MAPL_TimerAdd(GC, name="RUN", RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GenericSetServices ( GC, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !IROUTINE: Initialize -- Initialize method for the composite Physics Gridded Component

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the Physics Composite Gridded Component.
!  It acts as a driver for the initializtion of the five children: Radiation, 
!  Turbulence, Moist, Chem, and Surface. It also sets up the frieldly connections
!  between the children and their sibling Turbulence, as well as with their 
!  ``uncles'' Advection and Analysis.
!
!   For the turbulence tracer bundle, U and V come from 
!   the import state, S is computed here from T and Z and kept
!   in the export state, the rest are friendlies from MOIST and CHEM.
!
!   The turbulence default behavior is a friendly with a zero flux
!   lower boundary condition and not producing a tendency.
!   Default tracers are put at the end of the bundles with a single
!   call; all others have to be done manually.
!
!   Any of the children`s exports that are friendly to advection or analysis
!   are put in the respective bundles by a single MAPL_Generic call. Remember 
!   that friendly exports are were automatically allocated by the children
!   during the initialization sequence of the entire tree below Physics, which
!   is the first thing done here.

!   The increment tracer bundles for Moist and Turbulence are created with empty fields
!   except for those tracers which have explicit tendency Exports.
!
!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)           :: IAm 
  integer                              :: STATUS
  character(len=ESMF_MAXSTR)           :: COMP_NAME

! Local derived type aliases

   type (MAPL_MetaComp),       pointer :: STATE
   type (ESMF_GridComp),       pointer :: GCS(:)
   type (ESMF_State),          pointer :: GIM(:)
   type (ESMF_State),          pointer :: GEX(:)
   type (ESMF_FieldBundle)             :: BUNDLE, iBUNDLE
   type (ESMF_Field)                   :: FIELD
   type (ESMF_State)                   :: DUMMY
   type (ESMF_Grid)                    :: GRID

   integer                             :: NUM_TRACERS
   integer                             :: I
   integer                             :: NA
   character(len=ESMF_MAXSTR), pointer :: NAMES(:)
   character(len=ESMF_MAXSTR)          :: myNAME
   character(len=ESMF_MAXSTR)          ::  iNAME

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

!define PRINT_STATES

    Iam = "Initialize"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, GRID=GRID, RC=STATUS )
!    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "::" // Iam

! Get my MAPL_Generic state
!--------------------------

    call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
    VERIFY_(STATUS)

! Get children and their im/ex states from my generic state.
!----------------------------------------------------------

    call MAPL_Get ( STATE, GCS=GCS, GIM=GIM, GEX=GEX, RC=STATUS )
    VERIFY_(STATUS)

    ! Create Atmospheric grid
    !------------------------
    call MAPL_GridCreate( GC, rc=status )
    VERIFY_(STATUS)

    ! Call Initialize for every Child
    !--------------------------------

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)


    call MAPL_TimerOn(STATE,"TOTAL")
!    call MAPL_TimerOn(STATE,"INITIALIZE")


! Count tracers
!--------------

!    call ESMF_FieldBundleGet(BUNDLE,FieldCount=NUM_TRACERS, RC=STATUS)
!    VERIFY_(STATUS)

! Get the names of all tracers to fill other turbulence bundles.
!---------------------------------------------------------------

!    allocate(NAMES(NUM_TRACERS),STAT=STATUS)
!    VERIFY_(STATUS)

!    call ESMF_FieldBundleGet(BUNDLE, NAMES,  RC=STATUS)
!    VERIFY_(STATUS)

    call MAPL_TimerOff(STATE,"RUN")
    call MAPL_TimerOff(STATE,"TOTAL")

! All Done
!---------

    RETURN_(ESMF_SUCCESS)
 end subroutine Initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: Run -- Run method for the composite Physics Gridded Component

! !INTERFACE:

   subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The run method for the physics calls the children`s
!   run methods. It also prepares inputs and couplings amongst them.
!   Its main outputs are the combined tendencies needed by the dynamics.

!EOP

! ErrLog Variables

   character(len=ESMF_MAXSTR)          :: IAm 
   integer                             :: STATUS
   character(len=ESMF_MAXSTR)          :: COMP_NAME

! Local derived type aliases

   type (MAPL_MetaComp),      pointer  :: STATE
   type (ESMF_GridComp),      pointer  :: GCS(:)
   type (ESMF_State),         pointer  :: GIM(:)
   type (ESMF_State),         pointer  :: GEX(:)
   type (ESMF_State)                   :: INTERNAL
   type (ESMF_Config)                  :: CF
   character(len=ESMF_MAXSTR),pointer  :: GCNames(:)
   integer                             :: I, L
   integer                             :: IM, JM, LM
   real                                :: DT
  
!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, config=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "::" // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(STATE,"TOTAL")
    call MAPL_TimerOn(STATE,"RUN")

! Get the children`s states from the generic state
!-------------------------------------------------

    call MAPL_Get ( STATE,   &
        GCS=GCS, GIM=GIM, GEX=GEX,       &
        IM = IM, JM = JM, LM = LM,       &
        GCNames = GCNames,               &
        INTERNAL_ESMF_STATE = INTERNAL,  &
                               RC=STATUS )
    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(CF, DT, Label="RUN_DT:" , RC=STATUS)
    VERIFY_(STATUS)

! Pointers to Imports
!--------------------

! Pointers to Exports
!--------------------

! Dynamics
!------------------

    I=DYN   
    write(*,*) 'Running Dynamics GC'
    call MAPL_TimerOn (STATE,GCNames(I))
     call ESMF_GridCompRun (GCS(I), importState=GIM(I), &
          exportState=GEX(I), clock=CLOCK, userRC=STATUS );
     VERIFY_(STATUS)
     call MAPL_GenericRunCouplers (STATE, I, CLOCK, RC=STATUS );
     VERIFY_(STATUS)
    call MAPL_TimerOff(STATE,GCNames(I))

! Emissions
! NOTE: Should that be before dynamics? 
!------------------

    I=EMIS
    write(*,*) 'Running Emissions (HEMCO)'
    call MAPL_TimerOn (STATE,GCNames(I))
     call ESMF_GridCompRun (GCS(I), importState=GIM(I), &
          exportState=GEX(I), clock=CLOCK, userRC=STATUS );
     VERIFY_(STATUS)
     call MAPL_GenericRunCouplers (STATE, I, CLOCK, RC=STATUS );
     VERIFY_(STATUS)
    call MAPL_TimerOff(STATE,GCNames(I))

! Aerosol/Chemistry
!------------------

    I=CHEM   
    write(*,*) 'Running Chemistry GC'
    call MAPL_TimerOn (STATE,GCNames(I))
     call ESMF_GridCompRun (GCS(I), importState=GIM(I), &
          exportState=GEX(I), clock=CLOCK, userRC=STATUS );
     VERIFY_(STATUS)
     call MAPL_GenericRunCouplers (STATE, I, CLOCK, RC=STATUS );
     VERIFY_(STATUS)
    call MAPL_TimerOff(STATE,GCNames(I))

    write(*,*) 'Fin'
! Done With Sim
!------------------
    call MAPL_TimerOff(STATE,"RUN")
    call MAPL_TimerOff(STATE,"TOTAL")

    RETURN_(ESMF_SUCCESS)

  end subroutine Run

!=============================================================================

  subroutine Finalize ( GC, IMPORT, EXPORT, CLOCK, RC )

    ! !ARGUMENTS:
    
    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code
    
! !DESCRIPTION: The Finalize method 

!EOP

! ErrLog Variables

     character(len=ESMF_MAXSTR)          :: IAm 
     integer                             :: STATUS
     character(len=ESMF_MAXSTR)          :: COMP_NAME
     
     integer                             :: J
     
     Iam = "Finalize"
     call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
     VERIFY_(STATUS)
     Iam = trim(COMP_NAME) // Iam

     call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=STATUS )
     VERIFY_(STATUS)

     call ESMF_StateDestroy(IMPORT, rc=status)
     VERIFY_(STATUS)
     call ESMF_StateDestroy(EXPORT, rc=status)
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine Finalize

!=============================================================================

 end module GIGC_GridCompMod
