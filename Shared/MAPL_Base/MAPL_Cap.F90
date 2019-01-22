!  $Id$


#include "MAPL_Generic.h"
#include "unused_dummy.H"

module MAPL_CapMod

!BOP

! !MODULE: MAPL_CapMod --- Implements the top entry point for MAPL components

! !USES:

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
  use MAPL_CFIOMod
  use MAPL_CapGridCompMod, only: initialize_CapGridComp => initialize
  use MAPL_CapGridCompMod, only: CAP
  use MAPL_ConfigMod
  use, intrinsic :: iso_fortran_env, only: output_unit, REAL64, INT64

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_Cap

! !DESCRIPTION: 

! \input{MAPL_CapIntro.tex}
 
!EOP


  include "mpif.h"

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOPI

! !IROUTINE: MAPL_Cap -- Implements generic Cap functionality

! !INTERFACE:

  subroutine MAPL_CAP(ROOT_SetServices, Name, AmIRoot, FinalFile, CommIn, RC)

! !ARGUMENTS:

    external                             :: ROOT_SetServices
    character*(*), optional, intent(IN ) :: Name
    logical,       optional, intent(OUT) :: AmIRoot
    character*(*), optional, intent(IN ) :: FinalFile
    integer,       optional, intent(IN ) :: CommIn
    integer,       optional, intent(OUT) :: rc

!EOPI

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------


! ESMF stuff
!-----------

   type(ESMF_Config)            :: config

   
! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="MAPL_Cap"

! Misc locals
!------------

   character(len=ESMF_MAXSTR)   :: ROOT_CF

   integer                  :: esmfcommsize
   integer                  :: myRank, ioColor, esmfColor, esmfComm, ioComm, tmpioCommRoot,tRank
   type(MAPL_Communicators) :: mapl_Comm
   integer                  :: CommCap
   integer                  :: NX1,NY1,commCnt,ioCommSize,useIOServer
   integer                  :: NPES

   type (ESMF_VM) :: VM

   integer             :: modelDuration
   integer             :: cr, cm
   real(kind=REAL64)   :: rate
   integer             :: modelStart, modelEnd, modelTime
   real(kind=REAL64)   :: modelTimeInDays, modelDurationInDays, modelDaysPerDay
   integer, parameter  :: SecondsPerDay = 86400
   integer             :: mpi_thread_support,i

!  Server stuff
!   type(Server) :: ios

! Begin
!------

   call system_clock(count_rate=cr, count_max=cm)
   rate = real(cr, kind=MAPL_R8)

   call system_clock(modelStart)

!  Initialize ESMF
!-----------------

   if (present(CommIn)) then
       CommCap = CommIn
   else
       CommCap = MPI_COMM_WORLD
   end if

   if (.not.present(CommIn)) then
      call mpi_init_thread(MPI_THREAD_MULTIPLE,mpi_thread_support,status)
      !call mpi_init(status)
      VERIFY_(STATUS) 
   end if
   call mpi_comm_size(CommCap,nPes,status)
   VERIFY_(STATUS)
   call mpi_comm_rank(CommCap,myRank,status)
   VERIFY_(STATUS)
   mapl_comm%myGlobalRank = myRank


#if defined(ENABLE_ESMF_ERR_LOGGING)
   call ESMF_Initialize (vm=vm, mpiCommunicator=commCap, rc=status)
#else
   call ESMF_Initialize (vm=vm, logKindFlag=ESMF_LOGKIND_NONE, mpiCommunicator=commCap, rc=status)
#endif
   VERIFY_(STATUS)


   useIOServer = 0
   NX1=0
   NY1=0
   esmfCommSize=0
   if (myRank == 0) then
      config = ESMF_ConfigCreate(rc=status)
      VERIFY_(status)
      call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
      VERIFY_(status)
      
      call ESMF_ConfigGetAttribute(config, value=useIOServer, Label="USE_IOSERVER:", rc=status)
      if (status /=  ESMF_SUCCESS) then ! ok to fail on this
         useIOServer = 0
      endif

      if (useIOServer /=0 ) then
         call ESMF_ConfigGetAttribute(config, value=root_cf, Label="ROOT_CF:", rc=status)
         VERIFY_(status)
         call ESMF_ConfigDestroy(config, rc=status)
         VERIFY_(status)

         config = ESMF_ConfigCreate(rc=status)
         VERIFY_(status)
         call ESMF_ConfigLoadFile   ( config, root_cf, rc=STATUS )
         VERIFY_(status)
         call ESMF_ConfigGetAttribute(config, value=nx1, Label="NX:", rc=status)
         VERIFY_(status)
         call ESMF_ConfigGetAttribute(config, value=ny1, Label="NY:", rc=status)
         VERIFY_(status)
         call ESMF_ConfigDestroy(config, rc=status)
         VERIFY_(status)
         esmfcommsize = NX1*NY1
      else
         call ESMF_ConfigDestroy(config, rc=status)
         VERIFY_(status)
         esmfcommsize = nPes
      endif
   end if
   call MPI_BCAST(useIOServer, 1, MPI_INTEGER, 0, CommCap, status)
   VERIFY_(STATUS)
   call MPI_BCAST(NX1, 1, MPI_INTEGER, 0, CommCap, status)
   VERIFY_(STATUS)
   call MPI_BCAST(NY1, 1, MPI_INTEGER, 0, CommCap, status)
   VERIFY_(STATUS)
   call MPI_BCAST(esmfcommsize, 1, MPI_INTEGER, 0, CommCap, status)
   VERIFY_(STATUS)

   if (useIOServer /= 0) then
      call run_MAPL_GridComp_A(commCap, myRank, NX1*NY1, ioCommSize, commCnt, esmfcolor, &
           & ioColor, tmpiocommroot, status)
   else
      commCnt = nPes
      esmfColor =0
      iocommsize=0
      ioColor = MPI_UNDEFINED
   endif

   mapl_comm%globalCommSize = npes
   mapl_comm%ioCommSize = ioCommSize
 
   call mpi_comm_split(CommCap,esmfColor,myRank,esmfComm,status)
   VERIFY_(STATUS)
   call mpi_comm_split(CommCap,ioColor,myRank,ioComm,status)
   VERIFY_(STATUS)

   mapl_comm%esmfCommSize = esmfcommsize
   mapl_comm%MaplCommSize = nPes

   mapl_Comm%maplComm = CommCap
   mapl_comm%esmfComm = esmfComm
   mapl_comm%ioComm = ioComm

   tRank = myRank
   if (ioComm /= MPI_COMM_NULL) then
      call MPI_BCast(tRank,1, MPI_INTEGER, 0, ioComm, status)
      VERIFY_(STATUS)
   endif
   ! now everyone in iocomm knows the global rank of root
   ! broadcast from any rank in the IOComm to everyone knows it
   if (useIOServer /= 0) then
      call MPI_Bcast(tRank,1,MPI_INTEGER,tmpIOCommRoot,commCap,status)
      VERIFY_(STATUS)
      mapl_comm%ioCommRoot = tRank
   end if

   !call MAPL_CFIOServerInitMpiTypes()

   if (useIOServer /= 0) then 
      call MAPL_FinalizeShmem(rc=status)
      VERIFY_(STATUS)
   endif

   call initialize_CapGridComp(mapl_comm, root_setservices, finalfile, name)

   ESMFCOMMIF: if (esmfComm /= MPI_COMM_NULL) then

      call cap%run(rc=status)
      VERIFY_(status)

   end if ESMFCOMMIF

   IOCOMMIF: if (ioComm /= MPI_COMM_NULL) then
      call MPI_comm_rank(mapl_comm%iocomm,mapl_comm%myIoRank,status)
      VERIFY_(STATUS)
      call MAPL_CFIOServerStart(mapl_Comm,rc=status)
      VERIFY_(STATUS)
   end if IOCOMMIF

   call MPI_Barrier(CommCap,status)
   VERIFY_(STATUS) 
!  Finalize framework
!  ------------------

   if (.not.present(CommIn)) then   
#if 0
!ALT due to a bug in MAPL (or in the garbage collection of ESMF_VMFinalize)
!we have to bypass next line
   call ESMF_Finalize (RC=status)
   VERIFY_(STATUS)
#else
   call mpi_finalize(status)
   VERIFY_(STATUS)
#endif
   end if

   if (present(AmIRoot)) then
      AmIRoot = cap%get_am_i_root()
   end if

   ! Calculate Model Throughput
   ! --------------------------

   call system_clock(modelEnd)

   modelTime = (modelEnd - modelStart)/rate
   modelDuration = cap%get_model_duration()

   modelTimeInDays     = real(modelTime,     kind=REAL64) / real(SecondsPerDay, kind=REAL64)
   modelDurationInDays = real(modelDuration, kind=REAL64) / real(SecondsPerDay, kind=REAL64)

   modelDaysPerDay = modelDurationInDays / modelTimeInDays

   if (cap%get_am_i_root()) write(OUTPUT_UNIT,'("Model Throughput:",X,F12.3,X,"days per day")') modelDaysPerDay

   RETURN_(ESMF_SUCCESS)


 end subroutine MAPL_CAP



    subroutine run_MAPL_GridComp_A(commCap, myRank, npes_cap, ioCommSize, commCnt, &
         & esmfColor, ioColor, tmpIOcommRoot, rc)
       integer, intent(in) :: commCap
       integer, intent(in) :: myRank
       integer, intent(in) :: npes_Cap
       integer, intent(out) :: ioCommSize
       integer, intent(out) :: commCnt
       integer, intent(out) :: esmfColor
       integer, intent(out) :: ioColor
       integer, intent(out) :: tmpIOcommRoot
       integer, intent(out), optional :: rc

       integer :: status

       logical :: esmfNodesDone
       integer :: i, j
       integer :: lastNode, lastrank
       character(len=ESMF_MAXSTR)   :: Iam="MAPL_run_MAPL_GridComp_A"
       logical :: io_server_share

       call MAPL_GetNodeInfo (comm=commCap, rc=status)
       VERIFY_(status)

       commCnt = 0
       ioCommSize =0
       esmfColor = MPI_UNDEFINED
       ioColor = MPI_UNDEFINED
       esmfNodesDone = .false.

       io_server_share = (size(MAPL_NodeRankList) == 1)
       
       do i=1,size(MAPL_NodeRankList)
          do j=1,size(MAPL_NodeRankList(i)%rank)
             commCnt = commCnt + 1
             if (myRank == MAPL_NodeRankList(i)%rank(j)) esmfColor = 0
             if (commCnt == npes_cap) then
                esmfNodesDone = .true.
                lastrank = j
                exit
             end if
          enddo
         if (esmfNodesDone) then
            lastNode = i
            exit
         endif
      enddo

      if (io_server_share) then ! enable testing on one node (e.g., workstation)

         tmpioCommRoot = MAPL_NodeRankList(lastNode)%rank(lastrank+1)
         do j=lastrank + 1, size(MAPL_NodeRankList(lastnode)%rank)
            ioCommSize = ioCommSize + 1
            if (myRank == MAPL_NodeRankList(i)%rank(j)) ioColor = 0
         enddo

      else ! IO server is on completely separate set of nodes

         ASSERT_(lastNode < size(MAPL_NodeRankList))
         tmpioCommRoot = MAPL_NodeRankList(lastNode+1)%rank(1)
         do i=lastNode+1,size(MAPL_NodeRankList)
            ioCommSize = ioCommSize + size(MAPL_NodeRankList(i)%rank)
            do j=1,size(MAPL_NodeRankList(i)%rank)
               if (myRank == MAPL_NodeRankList(i)%rank(j)) ioColor = 0
            enddo
         enddo

      end if

      RETURN_(ESMF_SUCCESS)
   end subroutine run_MAPL_GridComp_A

 end module MAPL_CapMod
