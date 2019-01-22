#include "MAPL_Generic.h"
#include "unused_dummy.H"

module MAPL_CapGridCompMod
   use ESMF
   use MAPL_BaseMod
   use MAPL_ConstantsMod
   use MAPL_ProfMod
   use MAPL_MemUtilsMod
   use MAPL_IOMod
   use MAPL_CommsMod
   use MAPL_GenericMod
   use MAPL_LocStreamMod
   use ESMFL_Mod
   use MAPL_ShmemMod
   use MAPL_HistoryGridCompMod, only : Hist_SetServices => SetServices
   use MAPL_HistoryGridCompMod, only : HISTORY_ExchangeListWrap
   use MAPL_ExtDataGridCompMod, only : ExtData_SetServices => SetServices
   use MAPL_ExtDataGridCompMod, only : T_EXTDATA_STATE, EXTDATA_WRAP
   use MAPL_CFIOServerMod
   use MAPL_ConfigMod
   use MAPL_DirPathMod

   implicit none
   private


   public :: MAPL_CapGridComp
   public :: CAP
   public :: initialize

   type :: MAPL_CapGridComp
      private
      type (ESMF_GridComp)          :: gc
      procedure(), pointer, nopass  :: root_set_services => null()
      type (MAPL_Communicators)     :: mapl_comm
      character(len=:), allocatable :: final_file
      character(len=:), allocatable :: name

      integer                       :: heartbeat_dt
      integer                       :: nsteps
      logical                       :: amiroot
   contains
      procedure :: run
      procedure :: get_model_duration
      procedure :: get_am_i_root
   end type MAPL_CapGridComp

   ! Kludge to get certain info into the cap component
   type (MAPL_CapGridComp), protected :: cap

   include "mpif.h"

contains

   subroutine initialize(mapl_comm, root_set_services, final_file, name)
      type (MAPL_Communicators), intent(in) :: mapl_comm
      procedure() :: root_set_services
      character(len=*), optional, intent(in) :: final_file
      character(len=*), optional, intent(in) :: name

      integer :: pet, npes
      integer, allocatable :: petList(:)
      integer :: ierror
      type (ESMF_VM) :: vm

      integer :: status, rc
      character(len=ESMF_MAXSTR)   :: Iam="new_MAPL_CapGridComp"

      cap%mapl_comm = mapl_comm
      cap%root_set_services => root_set_services
      if (present(final_file)) then
         allocate(cap%final_file, source=final_file)
      end if

      if (present(name)) then
         allocate(cap%name, source=name)
      else
         allocate(cap%name, source='CAP')
      end if

      if (mapl_comm%esmfcomm /= MPI_COMM_NULL) then
         call MPI_Comm_size(mapl_comm%esmfcomm, npes, ierror)
         VERIFY_(ierror)

         allocate(petList(0:npes-1), stat=status)
         VERIFY_(status)

         call ESMF_VMGetCurrent(vm, rc=status)
         VERIFY_(status)
         call ESMF_VMget(vm, localPET=pet, rc=status)
         VERIFY_(status)
         call MPI_Gather(pet, 1, MPI_INTEGER, petList, 1, MPI_INTEGER, 0, mapl_comm%esmfcomm, ierror)
         VERIFY_(ierror)
      end if

      call MPI_Bcast(npes, 1, MPI_INTEGER, 0, mapl_comm%maplComm, ierror)
      VERIFY_(ierror)
      if (mapl_comm%esmfcomm == MPI_COMM_NULL) then
         allocate(petlist(0:npes-1), stat=status)
         VERIFY_(status)
      end if
      call MPI_Bcast(petList, npes, MPI_INTEGER, 0, mapl_comm%maplComm, ierror)
      VERIFY_(ierror)

      cap%gc = ESMF_GridCompCreate(name='MAPL_CapGridComp', petList=petList, rc=status)
      VERIFY_(status)

      call ESMF_GridCompSetServices( cap%gc, set_services, rc=status)
      VERIFY_(status)

      RETURN_(ESMF_SUCCESS)

   end subroutine initialize


   subroutine set_services(gc, rc)
      type (ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR)   :: Iam="set_services"

      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, userRoutine=run_gc, rc=status)
      VERIFY_(status)
      RETURN_(ESMF_SUCCESS)

   end subroutine set_services


   subroutine run(this, rc)
      class (MAPL_CapGridComp) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR)   :: Iam="run"

      call ESMF_GridCompRun(this%gc, rc=status)
      VERIFY_(status)
      RETURN_(ESMF_SUCCESS)

   end subroutine run

   function get_model_duration(this, rc) result (duration)
      class (MAPL_CapGridComp) :: this
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR)   :: Iam="get_model_duration"

      integer :: duration

      duration = this%nsteps * this%heartbeat_dt

      RETURN_(ESMF_SUCCESS)

   end function get_model_duration

   function get_am_i_root(this, rc) result (amiroot)
      class (MAPL_CapGridComp) :: this
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR)   :: Iam="get_am_i_root"

      logical :: amiroot

      amiroot = this%amiroot

      RETURN_(ESMF_SUCCESS)

   end function get_am_i_root

   subroutine run_gc(gc, import, export, clock, rc)
      !ARGUMENTS:
      type(ESMF_GridComp) :: GC     ! Gridded component 
      type(ESMF_State) :: import ! Import state
      type(ESMF_State) :: export ! Export state
      type(ESMF_Clock) :: clock  ! The clock
      integer, intent(out) :: RC     ! Error code:

      integer :: status
      character(len=ESMF_MAXSTR)   :: Iam="MAPL_GridCompCap::run()"

      _UNUSED_DUMMY(import)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(clock)

      call run_MAPL_GridComp(gc, rc=status)
      VERIFY_(status)
      RETURN_(ESMF_SUCCESS)

   end subroutine run_gc

   
   subroutine run_MAPL_GridComp(gc, rc)
      type (ESMF_Gridcomp) :: gc
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp)                   :: GCMGC
      type (ESMF_VM)                        :: gcmVM
      integer :: comm
      integer                      :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S
      integer                      :: N,NSTEPS
      integer                      :: NPES

      integer :: corespernode

      logical :: amIRoot_
      type (ESMF_Config) :: config
      character(len=ESMF_MAXSTR)   :: enableTimers
      character(len=ESMF_MAXSTR)   :: enableMemUtils
      integer                      :: MemUtilsMode
      integer                               :: useShmem

      integer*8, pointer           :: LSADDR(:) => null()
      integer :: status
      integer                      :: HIST
      type(ESMF_Config)            :: cf_ext

      type (T_ExtData_STATE), pointer       :: ExtData_internal_state => null()
      type (ExtData_wrap)                   :: wrap

      logical                      :: done

      character(len=ESMF_MAXSTR )           :: timerModeStr
      integer                               :: timerMode
      type(ESMF_TimeInterval)      :: Frequency
      character(len=ESMF_MAXSTR)   :: ROOT_CF
      type(ESMF_Config)            :: cf_root
      type(ESMF_Config)            :: cf_hist
      character(len=ESMF_MAXSTR)   :: ROOT_NAME
      integer                      :: printSpec

! Misc locals
!------------
      character(len=ESMF_MAXSTR)   :: EXTDATA_CF
      character(len=ESMF_MAXSTR)   :: EXPID
      character(len=ESMF_MAXSTR)   :: EXPDSC

! The children's GCs and IM/Ex states
!------------------------------------

   type(ESMF_GridComp), pointer :: GCS(:)
   type(ESMF_State),    pointer :: IMPORTS(:)
   type(ESMF_State),    pointer :: EXPORTS(:)

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------

   integer                      :: ROOT

   integer                               :: i, itemcount
   type (ESMF_Field)                     :: field
   type (ESMF_FieldBundle)               :: bundle


   type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)

      integer                      :: RUN_DT
      integer                               :: snglcol
      character(len=ESMF_MAXSTR)            :: replayMode
      integer                      :: EXTDATA
      integer :: nx
      integer :: ny

      type(ESMF_Time)              :: CurrTime
      type(ESMF_Clock)             :: clock
      type(ESMF_Clock)             :: clock_HIST
      integer                      :: HEARTBEAT_DT
      type(ESMF_Alarm)             :: PERPETUAL
      integer                      :: PERPETUAL_YEAR
      integer                      :: PERPETUAL_MONTH
      integer                      :: PERPETUAL_DAY
      logical                      :: LPERP
      character(len=ESMF_MAXSTR)   :: clockname
      character(len=ESMF_MAXSTR)   :: HIST_CF
      type(HISTORY_ExchangeListWrap) :: lswrap
      character(len=ESMF_MAXSTR )           :: DYCORE
      character(len=ESMF_MAXPATHLEN) :: user_dirpath,tempString
      logical                      :: tend,foundPath

      character(len=ESMF_MAXSTR)   :: Iam="MAPL_run_MAPL_GridComp"

      type (ESMF_VM)                        :: vm

      type (MAPL_MetaComp) :: MAPLOBJ
      procedure(), pointer :: root_set_services
      

      call ESMF_GridCompGet(gc, vm=vm, rc=status)
      VERIFY_(status)
      call ESMF_VMGet      (vm, petcount=NPES, mpiCommunicator=comm, rc=status)
      VERIFY_(status)

      AmIRoot_ = MAPL_Am_I_Root(vm)

      call MAPL_GetNodeInfo (comm=comm, rc=status)
      VERIFY_(STATUS)

      AmIRoot_ = MAPL_Am_I_Root(vm)

      cap%AmIRoot = AmIRoot_

   !  Open the CAP's configuration from CAP.rc
   !------------------------------------------

      config = ESMF_ConfigCreate (                   rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
      VERIFY_(STATUS)

   !  CAP's MAPL MetaComp
   !---------------------

      call MAPL_Set(MAPLOBJ, maplComm=cap%mapl_Comm, rc=status)
      VERIFY_(STATUS)

      call MAPL_Set (MAPLOBJ, name= cap%name, cf=CONFIG,    rc=STATUS )

   ! Check if user wants to use node shared memory (default is no)
   !--------------------------------------------------------------
      call MAPL_GetResource( MAPLOBJ, useShmem,  label='USE_SHMEM:',  default = 0, rc=STATUS )
      if (useShmem /= 0) then
         call MAPL_InitializeShmem (rc=status)
         VERIFY_(STATUS)
      end if

   !  Create Clock. This is a private routine that sets the start and 
   !   end times and the time interval of the clock from the configuration.
   !   The start time is temporarily set to 1 interval before the time in the
   !   configuration. Once the Alarms are set in intialize, the clock will
   !   be advanced to guarantee it and its alarms are in the same state as they
   !   were after the last advance before the previous Finalize.
   !---------------------------------------------------------------------------

      call MAPL_ClockInit ( MAPLOBJ, clock, NSTEPS,          rc=STATUS )
      VERIFY_(STATUS)

      cap%nsteps = NSTEPS

      clock_HIST = ESMF_ClockCreate ( clock, rc=STATUS )  ! Create copy for HISTORY
      VERIFY_(STATUS)

      CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
      VERIFY_(STATUS)

      ! We check resource for CoresPerNode (no longer needed to be in CAR.rc)
      ! If it is set in the resource, we issue an warning if the
      ! value does not agree with the detected CoresPerNode

      call ESMF_ConfigGetAttribute(config, value=N, Label="CoresPerNode:", rc=status)
      if(STATUS==ESMF_SUCCESS) then
         if (CoresPerNode /= N) then
            call WRITE_PARALLEL("WARNING: CoresPerNode set, but does NOT match detected value")
         end if
      end if

      call ESMF_VMGet      (VM, petcount=NPES, mpiCommunicator=comm, rc=status)
      VERIFY_(STATUS)
      ASSERT_(CoresPerNode<=NPES)

      call ESMF_ConfigGetAttribute(config, value=HEARTBEAT_DT, Label="HEARTBEAT_DT:", rc=status)
      VERIFY_(STATUS)
      call ESMF_TimeIntervalSet( Frequency, S=HEARTBEAT_DT, rc=status )
      VERIFY_(STATUS)

      cap%heartbeat_dt = HEARTBEAT_DT


      PERPETUAL = ESMF_AlarmCreate( clock=clock_HIST, name='PERPETUAL', ringinterval=Frequency, sticky=.false., rc=status )
      VERIFY_(STATUS)
      call ESMF_AlarmRingerOff( PERPETUAL, rc=status )
      VERIFY_(STATUS)

   ! Set CLOCK for AGCM
   ! ------------------

      call MAPL_GetResource( MAPLOBJ, PERPETUAL_YEAR,  label='PERPETUAL_YEAR:',  default = -999, rc=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource( MAPLOBJ, PERPETUAL_MONTH, label='PERPETUAL_MONTH:', default = -999, rc=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource( MAPLOBJ, PERPETUAL_DAY,   label='PERPETUAL_DAY:',   default = -999, rc=STATUS )
      VERIFY_(STATUS)

      LPERP = ( ( PERPETUAL_DAY   /= -999 ) .or. &
                ( PERPETUAL_MONTH /= -999 ) .or. &
                ( PERPETUAL_YEAR  /= -999 ) )

      if(         PERPETUAL_DAY   /= -999 ) then
         ASSERT_( PERPETUAL_MONTH /= -999 )
         ASSERT_( PERPETUAL_YEAR  /= -999 )
      endif

      if( LPERP ) then
          if (AmIRoot_) then
              if( PERPETUAL_YEAR  /= -999 ) print *, 'Using Perpetual  Year: ',PERPETUAL_YEAR
              if( PERPETUAL_MONTH /= -999 ) print *, 'Using Perpetual Month: ',PERPETUAL_MONTH
              if( PERPETUAL_DAY   /= -999 ) print *, 'Using Perpetual   Day: ',PERPETUAL_DAY
          endif

          call ESMF_ClockGet ( clock, name=clockname, rc=status )
          clockname = trim( clockname ) // '_PERPETUAL'
          call ESMF_Clockset ( clock, name=clockname, rc=status )

          call ESMF_ClockGet ( clock_HIST, name=clockname, rc=status )
          clockname = trim( clockname ) // '_PERPETUAL'
          call ESMF_Clockset ( clock_HIST, name=clockname, rc=status )

          call Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, STATUS )
          VERIFY_(STATUS)
      endif

   !  Get configurable info to create HIST 
   !  and the ROOT of the computational hierarchy
   !---------------------------------------------

   !BOR

   ! !RESOURCE_ITEM: string :: Name of ROOT's config file
      call MAPL_GetResource(MAPLOBJ, ROOT_CF,      "ROOT_CF:", default="ROOT.rc",       RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name to assign to the ROOT component
      call MAPL_GetResource(MAPLOBJ, ROOT_NAME,    "ROOT_NAME:", default="ROOT",           RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name of HISTORY's config file 
      call MAPL_GetResource(MAPLOBJ, HIST_CF,      "HIST_CF:", default="HIST.rc",        RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name of ExtData's config file
      call MAPL_GetResource(MAPLOBJ, EXTDATA_CF,   "EXTDATA_CF:", default='ExtData.rc',     RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Control Timers 
      call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", default='NO',             RC=STATUS )
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
      call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", default='NO',             RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource(MAPLOBJ, MemUtilsMode, "MAPL_MEMUTILS_MODE:", default=MAPL_MemUtilsModeBase, RC=STATUS )
      VERIFY_(STATUS)
   !EOR
      enableTimers =  ESMF_UtilStringUpperCase(enableTimers, rc=STATUS)
      VERIFY_(STATUS)

      if (enableTimers /= 'YES') then
         call MAPL_ProfDisable( rc=STATUS )
         VERIFY_(STATUS)
      else
         call MAPL_GetResource(MAPLOBJ, timerModeStr, "MAPL_TIMER_MODE:", &
                               default='MAX', RC=STATUS )
         VERIFY_(STATUS)

         timerModeStr = ESMF_UtilStringUpperCase(timerModeStr, rc=STATUS)
         VERIFY_(STATUS) 

         TestTimerMode: select case(timerModeStr)
         case("OLD")
            timerMode = MAPL_TimerModeOld      ! this has barriers
         case("ROOTONLY")
            timerMode = MAPL_TimerModeRootOnly ! this is the fastest
         case("MAX")
            timerMode = MAPL_TimerModeMax      ! this is the default
         case("MINMAX")
            timerMode = MAPL_TimerModeMinMax      ! this is the default
         case default
            ASSERT_(.false.)
         end select TestTimerMode
         call MAPL_TimerModeSet(timerMode, RC=status)
         VERIFY_(status)
      end if

      enableMemUtils = ESMF_UtilStringUpperCase(enableMemUtils, rc=STATUS)
      VERIFY_(STATUS)

      if (enableMemUtils /= 'YES') then
         call MAPL_MemUtilsDisable( rc=STATUS )
         VERIFY_(STATUS)
      else
         call MAPL_MemUtilsInit( mode=MemUtilsMode, rc=STATUS )
         VERIFY_(STATUS)
      end if

      call MAPL_GetResource( MAPLOBJ, printSpec, label='PRINTSPEC:', default = 0, rc=STATUS )
      VERIFY_(STATUS)

      call dirpaths%append(".",rc=status)
      VERIFY_(status)
      call ESMF_ConfigFindLabel(config,Label='USER_DIRPATH:',isPresent=foundPath,rc=status)
      if (foundPath) then
         tend=.false.
         do while (.not.tend)
            call ESMF_ConfigGetAttribute(config,value=user_dirpath,default='',rc=status)
            if (tempstring /= '') then
               call dirpaths%append(user_dirpath,rc=status)
               VERIFY_(status)
            end if
            call ESMF_ConfigNextLine(config,tableEnd=tend,rc=status)
            VERIFY_(STATUS)
         enddo
      end if

   ! Handle RUN_DT in ROOT_CF
   !-------------------------

      cf_root = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_root, ROOT_CF, rc=STATUS )
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_root, value=RUN_DT, Label="RUN_DT:", rc=status)
      if (STATUS == ESMF_SUCCESS) then
         if (heartbeat_dt /= run_dt) then
            if (AmIRoot_) then
               print *, "ERROR: inconsistent values of HEARTBEAT_DT and RUN_DT"
            end if
            call ESMF_VMBarrier(VM)
            RETURN_(ESMF_FAILURE)
         end if
      else
         call MAPL_ConfigSetAttribute(cf_root, value=heartbeat_dt, Label="RUN_DT:", rc=status)
         VERIFY_(STATUS)
      endif
      
   ! Add EXPID and EXPDSC from HISTORY.rc to AGCM.rc
   !------------------------------------------------
      cf_hist = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_hist, HIST_CF, rc=STATUS )
      VERIFY_(STATUS)

      call MAPL_ConfigSetAttribute(cf_hist, value=HIST_CF, Label="HIST_CF:", rc=status)
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_hist, value=EXPID,  Label="EXPID:",  rc=status)
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute(cf_hist, value=EXPDSC, Label="EXPDSC:", rc=status)
      VERIFY_(STATUS)

      call MAPL_ConfigSetAttribute(cf_root, value=EXPID,  Label="EXPID:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_root, value=EXPDSC, Label="EXPDSC:", rc=status)
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_root, value = NX, Label="NX:", rc=status)
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute(cf_root, value = NY, Label="NY:", rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_hist, value=NX,  Label="NX:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_hist, value=NY,  Label="NY:",  rc=status)
      VERIFY_(STATUS)
      
   ! Add CoresPerNode from CAP.rc to HISTORY.rc and AGCM.rc
   !-------------------------------------------------------
      call MAPL_ConfigSetAttribute(cf_root, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_hist, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
      VERIFY_(STATUS)

   ! Add a SINGLE_COLUMN flag in HISTORY.rc based on DYCORE value(from AGCM.rc)
   !---------------------------------------------------------------------------
      call ESMF_ConfigGetAttribute(cf_root, value=DYCORE,  Label="DYCORE:",  rc=status)
      VERIFY_(STATUS)
      if (DYCORE == 'DATMO') then
         snglcol = 1
         call MAPL_ConfigSetAttribute(cf_hist, value=snglcol,  Label="SINGLE_COLUMN:",  rc=status)
         VERIFY_(STATUS)
      end if

   ! Detect if this a regular replay in the AGCM.rc
   ! ----------------------------------------------
     call ESMF_ConfigGetAttribute(cf_root, value=ReplayMode, Label="REPLAY_MODE:", default="NoReplay", rc=status)
     VERIFY_(STATUS)
     

   ! Register the children with MAPL
   !--------------------------------

   !  Create Root child
   !-------------------
      call MAPL_Set(MAPLOBJ, CF=CF_ROOT, RC=STATUS)
      VERIFY_(STATUS)

      root_set_services => cap%root_set_services

      ROOT = MAPL_AddChild ( MAPLOBJ,     &
           name       = root_name,        &
           SS         = root_set_services, &
           rc=STATUS )  
      VERIFY_(STATUS)

   !  Create History child
   !----------------------

      call MAPL_Set(MAPLOBJ, CF=CF_HIST, RC=STATUS)
      VERIFY_(STATUS)

      HIST = MAPL_AddChild ( MAPLOBJ,        &
           name       = 'HIST',           &
           SS         = HIST_SetServices, &
                                rc=STATUS )  
      VERIFY_(STATUS)


   !  Create ExtData child
   !----------------------
      cf_ext = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_ext, EXTDATA_CF, rc=STATUS )
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_ext, value=RUN_DT, Label="RUN_DT:", rc=status)
      if (STATUS == ESMF_SUCCESS) then
         if (heartbeat_dt /= run_dt) then
            if (AmIRoot_) then
               print *, "ERROR: inconsistent values of HEATBEAT_DT and RUN_DT", heartbeat_dt, run_dt
            end if
            call ESMF_VMBarrier(VM)
            RETURN_(ESMF_FAILURE)
         end if
      else
         call MAPL_ConfigSetAttribute(cf_ext, value=heartbeat_dt, Label="RUN_DT:", rc=status)
         VERIFY_(STATUS)
      endif

      call MAPL_Set(MAPLOBJ, CF=CF_EXT, RC=STATUS)
      VERIFY_(STATUS)

      EXTDATA = MAPL_AddChild ( MAPLOBJ,        &
           name       = 'EXTDATA',           &
           SS         = ExtData_SetServices, &
                                rc=STATUS )  
      VERIFY_(STATUS)

   ! Add NX and NY from AGCM.rc to ExtData.rc as well as name of ExtData rc file
      call ESMF_ConfigGetAttribute(cf_root, value = NX, Label="NX:", rc=status)
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute(cf_root, value = NY, Label="NY:", rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_ext, value=NX,  Label="NX:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_ext, value=NY,  Label="NY:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_ext, value=EXTDATA_CF,  Label="CF_EXTDATA:",  rc=status)
      VERIFY_(STATUS)

   !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
   !-------------------------------------------------------------

      call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS,      RC=STATUS )
      VERIFY_(STATUS)

   ! Run as usual unless PRINTSPEC> 0 as set in CAP.rc. If set then
   ! model will not run completely and instead it will simply run MAPL_SetServices
   ! and print out the IM/EX specs. This step uses MAPL_StatePrintSpecCSV found
   ! in MAPL_Generic.F90.


      if (printSpec>0) then

         call MAPL_StatePrintSpecCSV(GCS(ROOT), printSpec, RC=status)
         VERIFY_(STATUS)
         call ESMF_VMBarrier       ( VM,                            RC=STATUS )
         VERIFY_(STATUS)

      else


   !  Initialize the Computational Hierarchy
   !----------------------------------------

      call ESMF_GridCompInitialize ( GCS(ROOT), importState=IMPORTS(ROOT), &
           exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)

   ! All the EXPORTS of the Hierachy are made IMPORTS of History
   !------------------------------------------------------------

      call ESMF_StateAdd ( IMPORTS(HIST), (/EXPORTS(ROOT)/), RC=STATUS )
      VERIFY_(STATUS)

      allocate(lswrap%ptr, stat=status)
      VERIFY_(STATUS)
      call ESMF_UserCompSetInternalState(GCS(HIST), 'MAPL_LocStreamList', &
           lswrap, STATUS)
      VERIFY_(STATUS)
      call MAPL_GetAllExchangeGrids(GCS(ROOT), LSADDR, RC=STATUS)
      VERIFY_(STATUS)
      lswrap%ptr%LSADDR_PTR => LSADDR

   ! Initialize the History
   !------------------------

      call ESMF_GridCompInitialize (   GCS(HIST), importState=IMPORTS(HIST), &
           exportState=EXPORTS(HIST), clock=CLOCK_HIST,  userRC=STATUS )
      VERIFY_(STATUS)
    
   ! Prepare EXPORTS for ExtData
   ! ---------------------------
       call ESMF_StateGet(IMPORTS(ROOT), ITEMCOUNT=ITEMCOUNT, RC=STATUS)
       VERIFY_(STATUS)
       allocate(ITEMNAMES(ITEMCOUNT), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(ITEMTYPES(ITEMCOUNT), STAT=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet(IMPORTS(ROOT), ITEMNAMELIST=ITEMNAMES, &
                          ITEMTYPELIST=ITEMTYPES, RC=STATUS)
       VERIFY_(STATUS)

       DO I=1, ITEMCOUNT
          if(ItemTypes(I) == ESMF_StateItem_Field) then
             call ESMF_StateGet(IMPORTS(ROOT), ItemNames(i), field, rc=status)
             VERIFY_(STATUS)
             
             call MAPL_StateAdd(EXPORTS(EXTDATA), field, rc=status)
             VERIFY_(STATUS)
          else if(ItemTypes(I) == ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(IMPORTS(ROOT), ItemNames(i), bundle, rc=status)
             VERIFY_(STATUS)
             call MAPL_StateAdd(EXPORTS(EXTDATA), bundle, rc=status)
             VERIFY_(STATUS)
          end if
       END DO
       deallocate(itemtypes)
       deallocate(itemnames)


   ! Initialize the ExtData
   !------------------------

      call ESMF_GridCompInitialize (   GCS(EXTDATA), importState=IMPORTS(EXTDATA), &
           exportState=EXPORTS(EXTDATA), & 
           clock=CLOCK,  userRC=STATUS )
      VERIFY_(STATUS)

   ! Finally check is this is a regular replay
   ! If so stuff gc and input state for ExtData in GCM internal state
   ! -----------------------------------------------------------------
   if (trim(replayMode)=="Regular") then
      call MAPL_GCGet(GCS(ROOT),"GCM",gcmGC,rc=status)
      VERIFY_(STATUS)
      call ESMF_GridCompGet(gcmGC,vm=gcmVM,rc=status)
      VERIFY_(STATUS)
      ASSERT_(vm==gcmVM)
      call ESMF_UserCompGetInternalState(gcmGC,'ExtData_state',wrap,status)
      VERIFY_(STATUS)
      ExtData_internal_state => wrap%ptr
      ExtData_internal_state%gc = GCS(EXTDATA)
      ExtData_internal_state%expState = EXPORTS(EXTDATA) 
   end if
    
   ! Time Loop starts by checking for Segment Ending Time
   !-----------------------------------------------------
      TIME_LOOP: do n=1,nsteps

         call MAPL_MemUtilsWrite(vm, 'MAPL_Cap:TimeLoop',           RC=STATUS )
         VERIFY_(STATUS)

         if( .not.LPERP ) then
              DONE = ESMF_ClockIsStopTime( CLOCK_HIST, RC=STATUS )
              VERIFY_(STATUS)
              if ( DONE ) exit
         endif

   ! Run the ExtData Component
   ! --------------------------

         call ESMF_GridCompRun     ( GCS(EXTDATA), importState=IMPORTS(EXTDATA), &
                                     exportState=EXPORTS(EXTDATA), &
                                     clock=CLOCK, userRC=STATUS )
         VERIFY_(STATUS)

   ! Call Record for intermediate checkpoint (if desired)
   !  Note that we are not doing a Record for History.
   ! ------------------------------------------------------

         call ESMF_GridCompWriteRestart( GCS(ROOT), importState=IMPORTS(ROOT), &
              exportState=EXPORTS(ROOT), clock=CLOCK_HIST, userRC=STATUS )
         VERIFY_(STATUS)

   ! Run the Gridded Component
   ! --------------------------

         call ESMF_GridCompRun( GCS(ROOT), importState=IMPORTS(ROOT), &
              exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
         VERIFY_(STATUS)

   ! Synchronize for Next TimeStep
   ! -----------------------------

         call ESMF_VMBarrier( VM, RC=STATUS )
         VERIFY_(STATUS)

   ! Advance the Clock before running History and Record
   ! ---------------------------------------------------

         call ESMF_ClockAdvance     ( CLOCK,                        RC=STATUS )
         VERIFY_(STATUS)
         call ESMF_ClockAdvance     ( CLOCK_HIST,                   RC=STATUS )
         VERIFY_(STATUS)

   ! Update Perpetual Clock
   ! ----------------------

         if( LPERP ) then
             call Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, STATUS )
             VERIFY_(STATUS)
         endif

         call ESMF_ClockGet ( clock, CurrTime=currTime, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
                                        MM = AGCM_MM, &
                                        DD = AGCM_DD, &
                                        H  = AGCM_H , &
                                        M  = AGCM_M , &
                                        S  = AGCM_S, rc=status )
         VERIFY_(STATUS)
         if( AmIRoot_ ) write(6,1000) AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
    1000 format(1x,'AGCM Date: ',i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)

   ! Call History Run for Output
   ! ---------------------------

         call ESMF_GridCompRun( GCS(HIST), importState=IMPORTS(HIST), &
              exportState=EXPORTS(HIST), clock=CLOCK_HIST, userRC=STATUS )
         VERIFY_(STATUS)

      enddo TIME_LOOP ! end of time loop

   !  Finalize
   !  --------

      call ESMF_GridCompFinalize( GCS(ROOT), importState=IMPORTS(ROOT),&
           exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)
      call ESMF_GridCompFinalize( GCS(HIST), importState=IMPORTS(HIST),&
           exportState=EXPORTS(HIST), clock=CLOCK_HIST,userRC=STATUS )
      VERIFY_(STATUS)
      call ESMF_GridCompFinalize( GCS(EXTDATA), importState=IMPORTS(EXTDATA),&
            exportState=EXPORTS(EXTDATA), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)

   !  Finalize itselt
   ! ----------------

      call CAP_Finalize(CLOCK_HIST, "cap_restart", rc=STATUS)
      VERIFY_(STATUS)

      call ESMF_ConfigDestroy(cf_ext, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(cf_hist, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(cf_root, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(config, RC=status)
      VERIFY_(STATUS)

      end if ! PRINTSPEC

      call MAPL_FinalizeShmem (rc=status)
      VERIFY_(STATUS)

   ! Write EGRESS file
   !------------------
      call ESMF_VMBarrier(VM)
      if(allocated(cap%final_file)) then
         if (AmIRoot_) then
            close(99)
            open (99,file=cap%final_file,form='formatted')
            close(99)
         end if
      end if

      RETURN_(ESMF_SUCCESS)
   end subroutine run_MAPL_GridComp
    

! !IROUTINE: MAPL_ClockInit -- Sets the clock

! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, nsteps, rc)

! !ARGUMENTS:

     type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
     type(ESMF_Clock),    intent(  out) :: Clock
     integer,             intent(  out) :: nsteps
     integer, optional,   intent(  out) :: rc

!  !DESCRIPTION:

!   This is a private routine that sets the start and 
!   end times and the time interval of the application clock from the configuration.
!   This time interal is the ``heartbeat'' of the application.
!   The Calendar is set to Gregorian by default. 
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!


     type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
     type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
     type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
     type(ESMF_TimeInterval)  :: duration
     type(ESMF_Calendar)      :: cal
     character(ESMF_MAXSTR)   :: CALENDAR

     integer                  :: STATUS
     character(ESMF_MAXSTR)   :: IAM="MAPL_ClockInit"

     integer        :: BEG_YY
     integer        :: BEG_MM
     integer        :: BEG_DD
     integer        :: BEG_H
     integer        :: BEG_M
     integer        :: BEG_S

     integer        :: CUR_YY
     integer        :: CUR_MM
     integer        :: CUR_DD
     integer        :: CUR_H
     integer        :: CUR_M
     integer        :: CUR_S

     integer        :: END_YY
     integer        :: END_MM
     integer        :: END_DD
     integer        :: END_H
     integer        :: END_M
     integer        :: END_S

     integer        :: DUR_YY
     integer        :: DUR_MM
     integer        :: DUR_DD
     integer        :: DUR_H
     integer        :: DUR_M
     integer        :: DUR_S

     integer        :: HEARTBEAT_DT
     integer        :: NUM_DT
     integer        :: DEN_DT

     integer        :: UNIT
     integer        :: datetime(2)

! Begin
!------

! Read Times From Config
! ----------------------

!BOR

     call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
     else

! !RESOURCE_ITEM: year :: Beginning year (integer)
        call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Beginning month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Beginning second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
! -----------------------------------------------------------
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     rc=STATUS )
     if(STATUS/=ESMF_SUCCESS) then
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )
     end if

     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! !RESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
     call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: string :: Calendar type
     call MAPL_GetResource( MAPLOBJ, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
     VERIFY_(STATUS)

!EOR

     ASSERT_(NUM_DT>=0)
     ASSERT_(DEN_DT> 0)
     ASSERT_(HEARTBEAT_DT>=0)
!     ASSERT_(NUM_DT*HEARTBEAT_DT>0)
     ASSERT_(NUM_DT<DEN_DT)

! initialize calendar to be Gregorian type
! ----------------------------------------

     if    (CALENDAR=="GREGORIAN") then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="JULIAN"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="NOLEAP"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
        VERIFY_(STATUS)
     else
        ASSERT_(.false.)
     endif

! initialize start time for Alarm frequencies
! -------------------------------------------

     call ESMF_TimeSet( StartTime, YY = BEG_YY, &
                                   MM = BEG_MM, &
                                   DD = BEG_DD, &
                                    H = BEG_H , &
                                    M = BEG_M , &
                                    S = BEG_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

     call ESMF_TimeSet(   EndTime, YY = END_YY, &
                                   MM = END_MM, &
                                   DD = END_DD, &
                                    H = END_H , &
                                    M = END_M , &
                                    S = END_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)  

! Read CAP Restart File for Current Time
! --------------------------------------

     CUR_YY = BEG_YY
     CUR_MM = BEG_MM
     CUR_DD = BEG_DD
     CUR_H  = BEG_H
     CUR_M  = BEG_M
     CUR_S  = BEG_S

     UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
     VERIFY_(STATUS)

     rewind(UNIT)
     read(UNIT,100,err=999,end=999) datetime
100  format(i8.8,1x,i6.6)

     CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

     if( MAPL_AM_I_ROOT() ) then
         write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2)") 'Read CAP restart properly, Current Date = ', CUR_YY,CUR_MM,CUR_DD
         write(6,"(a,2x,i2.2,':',i2.2,':',i2.2)") '                           Current Time = ', CUR_H ,CUR_M ,CUR_S
         print *
     endif


999  continue  ! Initialize Current time

     call FREE_FILE (UNIT)

     call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
                                  MM = CUR_MM, &
                                  DD = CUR_DD, &
                                   H = CUR_H , &
                                   M = CUR_M , &
                                   S = CUR_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

! initialize final stop time
! --------------------------

     call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
                                   MM = DUR_MM, &
                                    D = DUR_DD, &
                                    H = DUR_H , &
                                    M = DUR_M , &
                                    S = DUR_S , &
                                    startTime = currTime, &
                                    rc = STATUS  )
     VERIFY_(STATUS)

     stopTime = currTime + duration

! initialize model time step
! --------------------------

     call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
     VERIFY_(STATUS)

     nsteps = duration/timestep

! Create Clock and set it to one time step before StartTime.
! After Initialize has created all alarms, we will advance the
! clock to ensure the proper ringing state of all alarms
!-------------------------------------------------------------

     if (endTime < stopTime) then
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=EndTime, rc=STATUS )
     else
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=StopTime, rc=STATUS )
     end if
     VERIFY_(STATUS)

     call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine MAPL_ClockInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: YY, MM, DD, H, M, S
     integer, intent(  OUT) :: DATETIME(:)

     datetime(1) = 10000*YY + 100*MM + DD
     datetime(2) = 10000* H + 100* M + S
     return
   end subroutine MAPL_PackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: DATETIME(:)
     integer, intent(  OUT) :: YY, MM, DD, H, M, S

     YY =     datetime(1)/10000
     MM = mod(datetime(1),10000)/100
     DD = mod(datetime(1),100)
     H  =     datetime(2)/10000
     M  = mod(datetime(2),10000)/100
     S  = mod(datetime(2),100)
     return
   end subroutine MAPL_UnpackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine CAP_FINALIZE ( clock,filen, rc )

   type(ESMF_Clock),    intent(in   ) :: clock
   character(len=*),    optional      :: filen
   integer, optional,   intent(  out) :: rc

   integer        :: UNIT
   integer        :: datetime(2)
   integer        :: YY, MM, DD, H, M, S
   integer        :: status
   character(len=ESMF_MAXSTR), parameter :: IAm="CAP_FINALIZE"
   character(len=ESMF_MAXSTR)            :: filen_

   type(ESMF_Time)     :: CurrentTime
   
   filen_ = "cap_restart"
   if (present(filen))     filen_ = trim(filen )
   
! Retrieve Current Time for Cap Restart
! -------------------------------------

   call ESMF_ClockGet ( clock, currTime=currentTime, rc=status )
   VERIFY_(STATUS)
   call ESMF_TimeGet  ( CurrentTime, YY = YY, &
                                     MM = MM, &
                                     DD = DD, &
                                     H  = H , &
                                     M  = M , &
                                     S  = S, rc=status )
   VERIFY_(STATUS)

   CALL MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)

! Write CAP Restart File and Ending Time for Current Segment
! ----------------------------------------------------------

    if( MAPL_AM_I_ROOT() ) then
       UNIT = GETFILE( filen_, form="formatted" )
       write(unit,100) datetime
100    format(i8.8,1x,i6.6)
       call FREE_FILE (UNIT)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine CAP_FINALIZE

   subroutine Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, rc )
     type(ESMF_Clock),intent(inout) :: clock
     type(ESMF_Clock),intent(inout) :: clock_HIST
     integer,         intent(in)    :: PERPETUAL_YEAR
     integer,         intent(in)    :: PERPETUAL_MONTH
     integer,         intent(in)    :: PERPETUAL_DAY
     integer,         intent(out)   :: rc

     type(ESMF_Time)                :: currTime
     type(ESMF_Alarm)               :: PERPETUAL
     type(ESMF_Calendar)            :: cal
     integer                        :: status
     integer                        :: HIST_YY, HIST_MM, HIST_DD, HIST_H, HIST_M, HIST_S
     integer                        :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S

     character(len=ESMF_MAXSTR), parameter :: IAm="Perpetual_Clock"

     call ESMF_ClockGetAlarm ( clock_HIST, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
     VERIFY_(STATUS)
     call ESMF_AlarmRingerOff( PERPETUAL, rc=status )
     VERIFY_(STATUS)

         call ESMF_ClockGet ( clock, currTime=currTime, calendar=cal, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
                                        MM = AGCM_MM, &
                                        DD = AGCM_DD, &
                                        H  = AGCM_H , &
                                        M  = AGCM_M , &
                                        S  = AGCM_S, rc=status )
         VERIFY_(STATUS)

         call ESMF_ClockGet ( clock_HIST, CurrTime=CurrTime, calendar=cal, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = HIST_YY, &
                                        MM = HIST_MM, &
                                        DD = HIST_DD, &
                                        H  = HIST_H , &
                                        M  = HIST_M , &
                                        S  = HIST_S, rc=status )
         VERIFY_(STATUS)
#ifdef DEBUG
      block
         integer :: AmIRoot_
         type(ESMF_VM)                :: VM

!         This line is intentionally uncommented:  vm is not initialized (TLC)
         AmIRoot_ = MAPL_Am_I_Root(vm)
         if( AmIRoot_ ) then
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP M0: ",AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP H0: ",HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S
         endif
       end block
#endif
         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH == -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
         endif

         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
                                         AGCM_MM  = PERPETUAL_MONTH
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
#ifdef DEBUG
      block
         integer :: AmIRoot_
                                        
         type(ESMF_VM)                :: VM

!         This line is intentionally uncommented:  vm is not initialized (TLC)
         AmIRoot_ = MAPL_Am_I_Root(vm)
         if( AmIRoot_ ) then
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP M1: ",AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP H1: ",HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S
         endif
       end block
#endif
         endif

         if( (PERPETUAL_YEAR  == -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_MM  = PERPETUAL_MONTH
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                                                           AGCM_YY  = HIST_YY
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
         endif

         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   /= -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
                                         AGCM_MM  = PERPETUAL_MONTH
                                         AGCM_DD  = PERPETUAL_DAY
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
         endif

         call ESMF_TimeSet( CurrTime, YY = AGCM_YY, &
                                      MM = AGCM_MM, &
                                      DD = AGCM_DD, &
                                       H = AGCM_H , &
                                       M = AGCM_M , &
                                       S = AGCM_S , &
                       calendar=cal,  rc = STATUS  )
         VERIFY_(STATUS)
         call ESMFL_ClockSet ( clock, CurrTime=CurrTime, rc=status )
         VERIFY_(STATUS)

         call ESMF_TimeSet( CurrTime, YY = HIST_YY, &
                                      MM = HIST_MM, &
                                      DD = HIST_DD, &
                                       H = HIST_H , &
                                       M = HIST_M , &
                                       S = HIST_S , &
                       calendar=cal,  rc = STATUS  )
         VERIFY_(STATUS)
         call ESMFL_ClockSet ( clock_HIST, CurrTime=CurrTime, rc=status )
         VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine Perpetual_Clock

  subroutine ESMFL_ClockSet(clock, currTime, rc)
! Args
    type (ESMF_Clock)                :: clock
    type (ESMF_Time),  intent(IN   ) :: currTime
    integer, optional, intent(  OUT) :: rc

! ErrLog vars
    integer                                :: status
    character(len=ESMF_MAXSTR), parameter  :: IAm='ESMFL_ClockCreate'

! Local Vars    
    type(ESMF_Time)                        :: targetTime
    type(ESMF_Time)                        :: cTime
    type(ESMF_TimeInterval)                :: zero
    type(ESMF_TimeInterval)                :: delt
    type(ESMF_Time)                        :: ringTime
    type(ESMF_TimeInterval)                :: ringInterval
    type(ESMF_Alarm), allocatable          :: AlarmList(:)
    logical                                :: ringing
    integer                                :: I
    integer                                :: nalarms


    targetTime = currTime

! get the CurrentTime from the clock
    call ESMF_ClockGet(clock, alarmCount = nalarms, currTime=cTime, rc=status)
    VERIFY_(STATUS)

    delt = targetTime - cTime

    call ESMF_TimeIntervalSet(zero, rc=status)
    VERIFY_(STATUS)

! Get the list of current alarms in the clock
    allocate (alarmList(nalarms), stat = status)
    VERIFY_(STATUS)
    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=alarmList, alarmCount = nalarms, rc=status)
    VERIFY_(STATUS)

! Loop over all alarms
    DO I = 1, nalarms
       call ESMF_AlarmGet(alarmList(I), ringTime=ringTime, ringInterval=ringInterval, &
            ringing=ringing, rc=status)
       VERIFY_(STATUS)

! skip alarms with zero ringing interval
       if (ringInterval == zero) cycle

! Make sure the time-shift is multiple of ringing interval
       ASSERT_(MOD(delt,ringInterval) == zero)
       ringTime=ringTime + delt

       call ESMF_AlarmSet(alarmList(I), ringTime=ringTime, ringing=ringing, rc=status)
       VERIFY_(STATUS)

    END DO

! Protection in case we reset the clock outside of StopTime
    call ESMF_ClockStopTimeDisable(clock, rc=status)
    VERIFY_(STATUS)

    call ESMF_ClockSet(clock, currTime=targetTime, rc=status)
    VERIFY_(STATUS)

! We do not need the protection anymore
    call ESMF_ClockStopTimeEnable(clock, rc=status)
    VERIFY_(STATUS)

! clean-up
    deallocate(alarmList)

    RETURN_(ESMF_SUCCESS)
  end subroutine ESMFL_ClockSet

end module MAPL_CapGridCompMod
