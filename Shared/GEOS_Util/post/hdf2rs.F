#define _DIFFERENCING_
      program  main

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to merge eta_hdf data into GEOS5 restarts        ****
! ****                                                              ****
! **** Note:  ANA files are interpolated/binned to the Model        ****
! ****        BKG restart resolution.  The resulting blended        ****
! ****        restarts are then remapped to the Model topography.   ****
! ****  26Feb2007 Todling - calc ple from top down                  ****
! ****  02Jul2007 Todling - updated to new analysis state vector    ****
! ****  10Jul2007 Todling - handling of analysis increment          ****
! ****  28Aug2007 Todling - compiled r8 (as well as all in this     ****
! ****                      dir - needed for proper handle of bkg   ****
! ****                      (e.g., zero-ob case); largely modified  ****
! ****                      treatment of increments; opt to write   ****
! ****                      out interpolated increment              ****
! ****  18Sep2007 Todling - add low resolution analysis bkg update  ****
! ****  04Mar2007 Todling - add low resolution analysis bkg update  ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

! Note: When updating analysis using increment there are at least two 
!       reasons for doing harm to the gcm fields when the increments
!       are zero: (i) different accuracy between rst files (r8) and 
!       analysis increment file (r4); and (ii) the way the pressure
!       field gets updated (particularly pk). The only fully no-data/
!       no-harm procedure is the one using the code within the compiler 
!       directive _DIFFERENCING_.
!       
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice_ana
      type ( dynamics_lattice_type ) lattice_bkg
#ifdef mpi
      include 'mpif.h'
#endif
      integer  comm,myid,npes,ierror

      integer  imaglobal,jmaglobal
      integer  imbglobal,jmbglobal
      integer  imxglobal,jmxglobal
      integer  npex,npey

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms,nymd_bkg,nhms_bkg
      integer ima,jma,lm
      integer imb,jmb
      integer, parameter :: lugrd = 50

      integer ntime,nvars,ngatts,timinc
      integer ntdum,lmdum
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)

      character*128  dynrst, bkgeta, moistrst, pchemrst, topofile
      character*128  anaeta, inceta, incout, bkgsfc
      character*128  tag
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
      character*3    iamdoing

! ANA and BKG Variables
! ---------------------
      real, allocatable ::  dp_bkgx(:,:,:)
      real, allocatable ::    u_ana(:,:,:) ,    u_bkg(:,:,:),   u_bkg0(:,:,:), u_bkgx(:,:,:)
      real, allocatable ::    v_ana(:,:,:) ,    v_bkg(:,:,:),   v_bkg0(:,:,:), v_bkgx(:,:,:)
      real, allocatable ::  thv_ana(:,:,:) ,  thv_bkg(:,:,:)
      real, allocatable ::   tv_ana(:,:,:) ,  tv_bkgx(:,:,:)
      real, allocatable ::                     th_bkg(:,:,:),  th_bkg0(:,:,:)
      real, allocatable ::   pk_ana(:,:,:) ,   pk_bkg(:,:,:), dpk_bkg (:,:,:)
      real, allocatable ::  ple_ana(:,:,:) ,  ple_bkg(:,:,:), ple_bkg0(:,:,:), ple_bkgx(:,:,:)
      real, allocatable ::  pke_ana(:,:,:) ,  pke_bkg(:,:,:), dpke_bkg(:,:,:)
      real, allocatable ::    q_ana(:,:,:) ,    q_bkg(:,:,:),   q_bkg0(:,:,:),   q_bkgx(:,:,:)
      real, allocatable ::   o3_ana(:,:,:) ,   o3_bkg(:,:,:),  o3_bkg0(:,:,:),  o3_bkgx(:,:,:)
      real, allocatable ::   ps_ana(:,:)   ,  ps_bkgx(:,:)
      real, allocatable :: phis_ana(:,:)   , phis_bkg(:,:)  , phis_bkgx(:,:)
      real, allocatable :: phis_tmp(:,:)
      real, allocatable ::    cosza(:,:)   , coszb(:,:)

      real,   allocatable ::   glo(:,:)
      real*8, allocatable ::    ak(:)
      real*8, allocatable ::    bk(:)

      real*4  kappa
      real*4  rgas,rvap,eps,grav
      real    undefa

      character*120, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,L,ID,rc,method
      logical increment,ihaveth,doremap,do_o3,showdiv

C **********************************************************************
C ****                 Initialize MPI Environment                   ****
C **********************************************************************

      call timebeg ('main')

#ifdef mpi
      call mpi_init                ( ierror ) ; comm = mpi_comm_world
      call mpi_comm_rank ( comm,myid,ierror )
      call mpi_comm_size ( comm,npes,ierror )
      npex = nint ( sqrt( float(npes) ) )
      npey = npex
      do while ( npex*npey .ne. npes )
         npex = npex-1
         npey = nint ( float(npes)/float(npex) )
      enddo
#else
      comm = 0
      npes = 1
      npex = 1
      npey = 1
      myid = 0
#endif

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

      kappa = 2.0/7.0
      grav  = 9.8
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0

           tag = 'ana'
        dynrst = 'x'
      pchemrst = 'x'
      moistrst = 'x'
      bkgeta   = 'x'
      anaeta   = 'x'
      bkgsfc   = 'x'
      inceta   = 'x'
      incout   = 'x'
      topofile = 'x'
      nymd     = -999
      nhms     = -999
      method   = -999
      doremap  = .false.

         nargs = iargc()
      if(nargs.eq.0) call usage(myid)
      allocate ( arg(nargs) )

      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
             if( trim(arg(n)).eq.'-h'        ) call usage(myid)
             if( trim(arg(n)).eq.'-help'     ) call usage(myid)
             if( trim(arg(n)).eq.'-H'        ) call usage(myid)
             if( trim(arg(n)).eq.'-Help'     ) call usage(myid)
             if( trim(arg(n)).eq.'-dynrst'   ) then
                                   dynrst  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-moistrst' ) then
                                   moistrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-pchemrst' ) then
                                   pchemrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-ana'      ) then
                                   anaeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-bkg'      ) then
                                   bkgeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-sfc'      ) then
                                   bkgsfc   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-inc'      ) then
                                   inceta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-oinc'      ) then
                                   incout   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo'     ) then
                                   topofile = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-tag'      ) then
                                   tag     = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-remap'    ) doremap = .true.
             if( trim(arg(n)).eq.'-divr'     ) method = 1
             if( trim(arg(n)).eq.'-diva'     ) method = 2
             if( trim(arg(n)).eq.'-showdiv'  ) showdiv = .true.
             if( trim(arg(n)).eq.'-nymd'     ) read(arg(n+1),*) nymd
             if( trim(arg(n)).eq.'-nhms'     ) read(arg(n+1),*) nhms
      enddo


      do_o3     = .false.
      increment = .false.
      iamdoing  = 'ANA'
      if( trim(anaeta)  .eq.'x' .and. 
     .    trim(inceta)  .eq.'x' ) then
          if( myid.eq.0 ) print *, 'You must supply either ANAETA or INCETA Files!'
          call my_finalize
          call exit (7)
          stop
      else if ( trim(anaeta) .eq. 'x' ) then ! increment is provided as input
          increment = .true.
          anaeta    = inceta
          iamdoing  = 'INC'
          do_o3     = .true.
          if( trim(dynrst)  .eq.'x' .or. 
     .        trim(moistrst).eq.'x' .or.
     .        trim(pchemrst).eq.'x' .or.
     .        trim(bkgsfc)  .eq.'x' ) then
              if( myid.eq.0 ) print *, 'You must supply DYNRST, MOISTRST, PCHEMRST, and BKGSFC Files!'
              call my_finalize
              call exit (7)
              stop
          endif
      else                                   ! analyis is provided as input
          if( trim(dynrst)  .eq.'x' .or. 
     .        trim(moistrst).eq.'x' ) then
              if( myid.eq.0 ) print *, 'You must supply at least DYNRST, and MOISTRST Files!'
              call my_finalize
              call exit (7)
              stop
          endif
          do_o3 = trim(pchemrst).ne.'x' .and. trim(bkgsfc).eq.'x'
          if ( bkgeta .ne. 'x' ) then
              if( myid.eq.0 ) print *, 'Opt not available: ANA BKG cannot be updated w/o INCs!'
              call my_finalize
              call exit (7)
              stop         
          endif
      endif


! **********************************************************************
! ****    Read Dycore Internal Restart for RSLV, Date and Time      ****
! **********************************************************************

      if( myid.eq.0 ) then
          print *
          print *, 'Reading  ',trim(dynrst),' from PE: ',myid
          open (10, file=trim(dynrst),form='unformatted',access='sequential')
          read (10) headr1
          read (10) headr2
      endif
#ifdef mpi
      call mpi_bcast ( headr1,6,mpi_integer,0,comm,ierror )
      call mpi_bcast ( headr2,5,mpi_integer,0,comm,ierror )
#endif

      nymd_bkg = headr1(1)*10000
     .         + headr1(2)*100
     .         + headr1(3)
      nhms_bkg = headr1(4)*10000
     .         + headr1(5)*100
     .         + headr1(6)

      imbglobal = headr2(1)
      jmbglobal = headr2(2)
      lm        = headr2(3)

      if( nymd.eq.-999 ) nymd = nymd_bkg
      if( nhms.eq.-999 ) nhms = nhms_bkg

      write(date,101) nymd
      write(hour,102) nhms/10000
  101 format(i8.8)
  102 format(i2.2)

      call create_dynamics_lattice ( lattice_bkg,npex,npey )
      call   init_dynamics_lattice ( lattice_bkg,comm,imbglobal,jmbglobal,lm )

      imb = lattice_bkg%im( lattice_bkg%pei )
      jmb = lattice_bkg%jm( lattice_bkg%pej )

      allocate (    q_bkg(imb,jmb,lm)   )
      allocate (   o3_bkg(imb,jmb,lm)   )
      allocate (  thv_bkg(imb,jmb,lm)   )
      allocate (  pke_bkg(imb,jmb,lm+1) )
      allocate ( phis_bkg(imb,jmb)      )

      allocate (    u_bkg(imb,jmb,lm)   )
      allocate (    v_bkg(imb,jmb,lm)   )
      allocate (   th_bkg(imb,jmb,lm)   )
      allocate (  ple_bkg(imb,jmb,lm+1) )
      allocate (   pk_bkg(imb,jmb,lm)   )
      allocate (  dpk_bkg(imb,jmb,lm)   )

      allocate (   u_bkg0(imb,jmb,lm)   )
      allocate (   v_bkg0(imb,jmb,lm)   )
      allocate ( ple_bkg0(imb,jmb,lm+1) )
      if (increment) then
          allocate (   q_bkg0(imb,jmb,lm)   )
          allocate (  o3_bkg0(imb,jmb,lm)   )
          allocate (  th_bkg0(imb,jmb,lm)   )
      endif

      allocate ( ak(lm+1) )
      allocate ( bk(lm+1) )

      if( myid.eq.0 ) then
          read (10) ak
          read (10) bk
      endif
#ifdef mpi
      call mpi_bcast ( ak,lm+1,mpi_double_precision,0,comm,ierror )
      call mpi_bcast ( bk,lm+1,mpi_double_precision,0,comm,ierror )
#endif

      call read_fv (   u_bkg0,imb,jmb,lm  ,10,lattice_bkg )
      call read_fv (   v_bkg0,imb,jmb,lm  ,10,lattice_bkg )
      if (increment) then
          call read_fv (  th_bkg0,imb,jmb,lm,10,lattice_bkg )
      else
          call read_fv ( ple_bkg0,imb,jmb,lm,10,lattice_bkg )
      endif
      call read_fv ( ple_bkg0,imb,jmb,lm+1,10,lattice_bkg )
      if (increment) then
          call read_fv ( pk_bkg,imb,jmb,lm,10,lattice_bkg )
      endif

      if( myid.eq.0 ) close(10)

! **********************************************************************
! ****              Read Moist Internal Restart                     ****
! **********************************************************************

      if( increment ) then
      if( myid.eq.0 ) then
          print *, 'Reading GEOS-5  moist_internal_restart: ',trim(moistrst)
          print *
          open  (20,file=trim(moistrst),form='unformatted',access='sequential')
      endif

!     Read first field: SPHU
!     ----------------------
      allocate ( glo(imb,jmb) )
      do L=1,lm
         call readit ( glo,imb,jmb,1,20,lattice_bkg,moistrst,rc )
                       q_bkg0(:,:,L) = glo(:,:)
      enddo
      deallocate ( glo )

      if( myid.eq.0 ) close(20)
      endif ! < increment >

! **********************************************************************
! ****              Read Pchem Internal Restart                     ****
! **********************************************************************

      if( do_o3 ) then
      if( myid.eq.0 ) then
          print *, 'Reading GEOS-5  pchem_internal_restart: ',trim(pchemrst)
          print *
          open  (30,file=trim(pchemrst),form='unformatted',access='sequential')
      endif

!     Read first field: OZONE
!     -----------------------
      allocate ( glo(imb,jmb) )
      do L=1,lm
         call readit ( glo,imb,jmb,1,30,lattice_bkg,moistrst,rc )
                       o3_bkg0(:,:,L) = glo(:,:)
      enddo
      deallocate ( glo )

      if( myid.eq.0 ) close(30)

      else 
          o3_bkg0 = 0.0
      endif  ! < do_o3 >

! **********************************************************************
! ****             Read Analysis/Increment ANA/INC File            ****
! **********************************************************************

      call timebeg(' read_ana')
      if( myid.eq.0 ) then
          print *, 'Reading  ', iamdoing, ' File from PE: ',myid
          call gfio_open       ( trim(anaeta),1,ID,rc )
          call gfio_diminquire ( id,imaglobal,jmaglobal,lm,ntime,nvars,ngatts,rc )
      endif
#ifdef mpi
      call mpi_bcast ( imaglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast ( jmaglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (        lm,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     ntime,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     nvars,1,  mpi_integer,0,comm,ierror )
#endif
      call create_dynamics_lattice ( lattice_ana,npex,npey )
      call   init_dynamics_lattice ( lattice_ana,comm,imaglobal,jmaglobal,lm )

      ima = lattice_ana%im( lattice_ana%pei )
      jma = lattice_ana%jm( lattice_ana%pej )

      allocate ( lon(imaglobal) )
      allocate ( lat(jmaglobal) )
      allocate ( lev(lm) )
      allocate ( yymmdd(ntime) )
      allocate ( hhmmss(ntime) )
      allocate (  vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate (  kmvar(nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )

      if( myid.eq.0 ) then
          call gfio_inquire ( id,imaglobal,jmaglobal,lm,ntime,nvars,
     .                        title,source,contact,undefa,
     .                        lon,lat,lev,levunits,
     .                        yymmdd,hhmmss,timinc,
     .                        vname,vtitle,vunits,kmvar,
     .                        vrange,prange,rc )
      endif
#ifdef mpi
      call mpi_bcast ( undefa,     1,lattice_ana%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( lon,imaglobal,lattice_ana%mpi_rkind,0,comm,ierror )
#endif

      allocate (   ps_ana(ima,jma)      )
      allocate (   pk_ana(ima,jma,lm)   )
      allocate (  pke_ana(ima,jma,lm+1) )
      allocate (  thv_ana(ima,jma,lm)   )

      allocate ( phis_ana(ima,jma)      )
      allocate (    u_ana(ima,jma,lm)   )
      allocate (    v_ana(ima,jma,lm)   )
      allocate (   tv_ana(ima,jma,lm)   )
      allocate (    q_ana(ima,jma,lm)   )
      allocate (   o3_ana(ima,jma,lm)   )
      allocate (  ple_ana(ima,jma,lm+1) )

      call getit ( id,'phis' ,nymd,nhms,ima,jma,0,1 ,phis_ana,lattice_ana )
      call getit ( id,'ps'   ,nymd,nhms,ima,jma,0,1 ,  ps_ana,lattice_ana )
      call getit ( id,'u'    ,nymd,nhms,ima,jma,1,lm,   u_ana,lattice_ana )
      call getit ( id,'v'    ,nymd,nhms,ima,jma,1,lm,   v_ana,lattice_ana )
      call getit ( id,'tv'   ,nymd,nhms,ima,jma,1,lm,  tv_ana,lattice_ana )
      call getit ( id,'sphu' ,nymd,nhms,ima,jma,1,lm,   q_ana,lattice_ana )
      call getit ( id,'ozone',nymd,nhms,ima,jma,1,lm,  o3_ana,lattice_ana )

      if( myid.eq.0 ) call gfio_close ( id,rc )
      call timeend(' read_ana')


!     Calculate analysis pressure edges (or increments to it)
!     -------------------------------------------------------
      if ( increment ) then
           do L=1,lm+1
              ple_ana(:,:,L) = ps_ana(:,:)*bk(L)
           enddo
      else
           do L=1,lm+1
              ple_ana(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
           enddo
      endif

      deallocate ( ps_ana )

! Check for Indexing Consistency with Model
! -----------------------------------------
      if( myid.eq.0 ) print *, 'Analysis ', iamdoing, ' File begins at Lon: ',lon(1)
      if( lon(1) .eq. 0.0 ) then
          if( myid.eq.0 ) print *, '         Flipping Horizontal Coordinate' 
          call hflip (phis_ana,ima,jma,1   ,lattice_ana )
          call hflip (   u_ana,ima,jma,lm  ,lattice_ana )
          call hflip (   v_ana,ima,jma,lm  ,lattice_ana )
          call hflip (  tv_ana,ima,jma,lm  ,lattice_ana )
          call hflip ( ple_ana,ima,jma,lm+1,lattice_ana )
          call hflip (   q_ana,ima,jma,lm  ,lattice_ana )
          call hflip (  o3_ana,ima,jma,lm  ,lattice_ana )
      endif

! Create ANA Virtual Potential Temperature
!  NOTE: If input data is increment, thv_ana
!        carries increment in virtual temperature
! -----------------------------------------------
      if ( increment ) then
           thv_ana = tv_ana
           ihaveth = .true.  ! in this case, at the point of calling remap 
	                     ! the temperature variable contains theta
      else
           pke_ana(:,:,:) = ple_ana(:,:,:)**kappa
           do L=1,lm
           pk_ana(:,:,L) = ( pke_ana(:,:,L+1)-pke_ana(:,:,L) )
     .                   / ( kappa*log(ple_ana(:,:,L+1)/ple_ana(:,:,L)) )
           enddo
           thv_ana = tv_ana/pk_ana
           ihaveth = .false.  ! in this case, at the point of calling remap 
	                      ! the temperature variable contains virtual theta
      endif

      deallocate ( lon )
      deallocate ( lat )
      deallocate ( lev )
      deallocate ( yymmdd )
      deallocate ( hhmmss )
      deallocate (  vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate (  kmvar )
      deallocate ( vrange )
      deallocate ( prange )
      deallocate ( tv_ana )


! If so, update analysis background and overwrite it
! --------------------------------------------------
      if ( bkgeta .ne. 'x' ) then

!     **********************************************************************
!     ****                   Read Analysis BKG File                     ****
!     **********************************************************************

          if( myid.eq.0 ) then
              print *, 'Reading  ', trim(bkgeta), ' File from PE: ',myid
              call gfio_open       ( trim(bkgeta),0,ID,rc )
              call gfio_diminquire ( id,imxglobal,jmxglobal,lm,ntime,nvars,ngatts,rc )
          endif
#ifdef mpi
          call mpi_bcast ( imxglobal,1,  mpi_integer,0,comm,ierror )
          call mpi_bcast ( jmxglobal,1,  mpi_integer,0,comm,ierror )
#endif
          if ( imxglobal.ne.imaglobal .and. jmxglobal.ne.jmaglobal ) then
               if( myid.eq.0 ) print *, 'Dimension of ANA background incompatible w/ INCs!'
               call my_finalize
               call exit (7)
               stop
          endif

          allocate ( lon(imxglobal) )
          allocate ( lat(jmxglobal) )
          allocate ( lev(lm) )
          allocate ( yymmdd(ntime) )
          allocate ( hhmmss(ntime) )
          allocate (  vname(nvars) )
          allocate ( vtitle(nvars) )
          allocate ( vunits(nvars) )
          allocate (  kmvar(nvars) )
          allocate ( vrange(2,nvars) )
          allocate ( prange(2,nvars)  )

          if( myid.eq.0 ) then
              call gfio_inquire ( id,imxglobal,jmxglobal,lm,ntime,nvars,
     .                            title,source,contact,undefa,
     .                            lon,lat,lev,levunits,
     .                            yymmdd,hhmmss,timinc,
     .                            vname,vtitle,vunits,kmvar,
     .                            vrange,prange,rc )
          endif
#ifdef mpi
          call mpi_bcast ( lon,imxglobal,lattice_ana%mpi_rkind,0,comm,ierror )
#endif

!         Check for Indexing Consistency with Model
!         -----------------------------------------
          if( myid.eq.0 ) print *, 'Background ', trim(bkgeta), ' File begins at Lon: ',lon(1)
          if( lon(1) .eq. 0.0 ) then
              if( myid.eq.0 ) print *, 'Longitudes in BKG file incompatible with INC file!'
              call my_finalize
              call exit (7)
              stop              
          endif

          deallocate ( lon )
          deallocate ( lat )
          deallocate ( lev )
          deallocate ( yymmdd )
          deallocate ( hhmmss )
          deallocate (  vname )
          deallocate ( vtitle )
          deallocate ( vunits )
          deallocate (  kmvar )
          deallocate ( vrange )
          deallocate ( prange )

          allocate ( phis_bkgx(ima,jma)      )
          allocate (   ps_bkgx(ima,jma)      )
          allocate (   dp_bkgx(ima,jma,lm)   )   
          allocate (    u_bkgx(ima,jma,lm)   )
          allocate (    v_bkgx(ima,jma,lm)   )
          allocate (   tv_bkgx(ima,jma,lm)   )
          allocate (    q_bkgx(ima,jma,lm)   )
          allocate (   o3_bkgx(ima,jma,lm)   )
          allocate (  ple_bkgx(ima,jma,lm+1) )

          call getit ( id,'phis' ,nymd,nhms,ima,jma,0,1 ,phis_bkgx,lattice_ana ) 
          call getit ( id,'ps'   ,nymd,nhms,ima,jma,0,1 ,  ps_bkgx,lattice_ana )
          call getit ( id,'delp' ,nymd,nhms,ima,jma,1,lm,  dp_bkgx,lattice_ana )
          call getit ( id,'u'    ,nymd,nhms,ima,jma,1,lm,   u_bkgx,lattice_ana )
          call getit ( id,'v'    ,nymd,nhms,ima,jma,1,lm,   v_bkgx,lattice_ana )
          call getit ( id,'tv'   ,nymd,nhms,ima,jma,1,lm,  tv_bkgx,lattice_ana )
          call getit ( id,'sphu' ,nymd,nhms,ima,jma,1,lm,   q_bkgx,lattice_ana )
          call getit ( id,'ozone',nymd,nhms,ima,jma,1,lm,  o3_bkgx,lattice_ana )

!         Calculate pressure edges for full resolution analysis background
!         ----------------------------------------------------------------
          do L=1,lm+1
             ple_bkgx(:,:,L) = ak(L) + ps_bkgx(:,:)*bk(L)
          enddo

!         Now simply add increments to full resolution Analysis Background
!         ----------------------------------------------------------------
          ple_bkgx = ple_bkgx + ple_ana
            u_bkgx =   u_bkgx +   u_ana
            v_bkgx =   v_bkgx +   v_ana
           tv_bkgx =  tv_bkgx + thv_ana
            q_bkgx =   q_bkgx +   q_ana
           o3_bkgx =  o3_bkgx +  o3_ana

!         Update surface pressure and delp's
!         ----------------------------------
          do L = 1, lm
             dp_bkgx(:,:,L) = ple_bkgx(:,:,L+1) - ple_bkgx(:,:,L)
          enddo
          ps_bkgx = ple_bkgx(:,:,lm+1)

!         Write out updated full resolution background (analysis)
!         -------------------------------------------------------
          call putit ( id,'ps'   ,nymd,nhms,ima,jma,0,1 ,  ps_bkgx,lattice_ana )
          call putit ( id,'delp' ,nymd,nhms,ima,jma,1,lm,  dp_bkgx,lattice_ana )
          call putit ( id,'u'    ,nymd,nhms,ima,jma,1,lm,   u_bkgx,lattice_ana )
          call putit ( id,'v'    ,nymd,nhms,ima,jma,1,lm,   v_bkgx,lattice_ana )
          call putit ( id,'tv'   ,nymd,nhms,ima,jma,1,lm,  tv_bkgx,lattice_ana )
          call putit ( id,'sphu' ,nymd,nhms,ima,jma,1,lm,   q_bkgx,lattice_ana )
          call putit ( id,'ozone',nymd,nhms,ima,jma,1,lm,  o3_bkgx,lattice_ana )
 
          if ( myid.eq.0 ) then
             call gfio_close ( id,rc )
             print *
             print *, 'Updated Analysis Background File: ', trim(bkgeta)
             print *
          endif  

          deallocate (  ple_bkgx )
          deallocate (   o3_bkgx )
          deallocate (    q_bkgx )
          deallocate (   tv_bkgx )
          deallocate (    v_bkgx )
          deallocate (    u_bkgx )
          deallocate (   dp_bkgx )
          deallocate (   ps_bkgx )
          deallocate ( phis_bkgx )

      endif ! < update analysis background >

! **********************************************************************
! ****               Read Surface Background File                   ****
! ****         Only to extract Cosine of Zenith Angle               ****
! **********************************************************************

      if ( do_o3 ) then

      call timebeg(' read_sfc')
      if( myid.eq.0 ) then
          print *, 'Reading  ', trim(bkgsfc), ' File from PE: ',myid
          call gfio_open       ( trim(bkgsfc),1,ID,rc )
          call gfio_diminquire ( id,imxglobal,jmxglobal,lmdum,ntdum,nvars,ngatts,rc )
      endif
#ifdef mpi
      call mpi_bcast ( imxglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast ( jmxglobal,1,  mpi_integer,0,comm,ierror )
#endif
      if (imxglobal.ne.imaglobal .or. jmxglobal.ne.jmaglobal ) then
          if( myid.eq.0 ) print *, 'Dimension of BKG SFC file inconsistent w/ that of ANA ETA'
          call my_finalize
          call exit (7)
      endif

      allocate ( cosza(ima,jma) )

      call getit ( id,'COSZ' ,nymd,nhms,ima,jma,0,1 ,cosza,lattice_ana )

      if( myid.eq.0 ) call gfio_close ( id,rc )
      call timeend(' read_sfc')

      else
      allocate ( cosza(ima,jma) )
      cosza = 0.0  ! it doesn't really matter what this gets set to in this case
      endif  ! < do_o3 >

      if( myid.eq.0 ) then
          print *
          print *, '    FV Restart filename: ',trim(dynrst)
          print *, '          FV resolution: ',imbglobal,jmbglobal,lm
          print *
          print *, '      Analysis filename: ',trim(anaeta)
          print *, '         ', iamdoing,' resolution: ',imaglobal,jmaglobal,lm
          print *
          print *, '                   Date: ',nymd,nhms
          print *, '                    Tag: ',trim(tag)
          print *
      endif

! **********************************************************************
! ****         Create/Update Restarts Using ANA/INC Data            ****
! **********************************************************************
      if( imaglobal.ne.imbglobal  .or. 
     .    jmaglobal.ne.jmbglobal  .or.
     .    doremap                 ) then

          if( trim(topofile).eq.'x' ) then
              if( myid.eq.0 ) print *, 'You must supply TOPO File at Model Resolution!'
              call my_finalize
              call exit (7)
              stop
          else
              if( myid.eq.0 ) print *, 'Reading  ',trim(topofile),' from PE: ',myid
              open  (10,file=trim(topofile),form='unformatted',access='sequential')
              call readit ( phis_bkg,imb,jmb,1,10,lattice_bkg,topofile,rc )
              phis_bkg = phis_bkg*grav
              close (10)
          endif
          allocate ( phis_tmp(imb,jmb) )
          allocate (    coszb(imb,jmb) )

! Interpolate ANA Data to BKG Resolution
!  NOTE: If input data is increment, thv_bkg
!        will carry increment of virtual temperature
!        after interpolations are completed
! --------------------------------------------------
          if( imaglobal.lt.imbglobal ) then
              if( myid.eq.0 ) print *, 'Interpolating ', iamdoing, ' Data to BKG Resolution'
              call hinterp (    cosza,ima,jma,   coszb,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg,1 )
              call hinterp ( phis_ana,ima,jma,phis_tmp,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg,0 )
              call hinterp (    u_ana,ima,jma,   u_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg,0 )
              call hinterp (    v_ana,ima,jma,   v_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg,0 )
              call hinterp (  thv_ana,ima,jma, thv_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg,0 )
              call hinterp (  ple_ana,ima,jma, ple_bkg,imb,jmb,lm+1,undefa,lattice_ana,lattice_bkg,0 )
              call hinterp (    q_ana,ima,jma,   q_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg,1 )
              call hinterp (   o3_ana,ima,jma,  o3_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg,1 )
          endif

! Bin ANA Data to BKG Resolution
! ------------------------------
          if( imaglobal.gt.imbglobal ) then
              if( myid.eq.0 ) print *, 'Binning       ', iamdoing, ' Data to BKG Resolution'
              call bin (    cosza,ima,jma,   coszb,imb,jmb,1   ,undefa, 0 ,lattice_ana,lattice_bkg )
              call bin ( phis_ana,ima,jma,phis_tmp,imb,jmb,1   ,undefa, 0 ,lattice_ana,lattice_bkg )
              call bin (    u_ana,ima,jma,   u_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
              call bin (    v_ana,ima,jma,   v_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
              call bin (  thv_ana,ima,jma, thv_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
              call bin (  ple_ana,ima,jma, ple_bkg,imb,jmb,lm+1,undefa, 0 ,lattice_ana,lattice_bkg )
              call bin (    q_ana,ima,jma,   q_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
              call bin (   o3_ana,ima,jma,  o3_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          endif

! BKG and ANA Horizontal Resolutions Match
! ----------------------------------------
          if( imaglobal.eq.imbglobal .and. jmaglobal.eq.jmbglobal ) then
                 coszb =    cosza
              phis_tmp = phis_ana
                 u_bkg =    u_ana
                 v_bkg =    v_ana
               thv_bkg =  thv_ana
               ple_bkg =  ple_ana
                 q_bkg =    q_ana
                o3_bkg =   o3_ana
          endif


! If so, add interpolated increment to RST background 
!  (BKG will contain analysis from this point on)
! ---------------------------------------------------
          if ( increment ) then

!              Calculate correction to pk given change in ple due to analysis ...
!              ------------------------------------------------------------------
#ifdef _DIFFERENCING_

!              ... based on finite differencing
!              --------------------------------
               allocate( dpke_bkg(imb,jmb,lm+1) )
                pke_bkg(:,:,:) =       (ple_bkg0(:,:,:))** kappa
               dpke_bkg(:,:,:) = kappa*(ple_bkg0(:,:,:))**(kappa-1)*ple_bkg(:,:,:)
               do L=1,lm
                  dpk_bkg(:,:,L) = (  ( dpke_bkg (:,:,L+1) - dpke_bkg(:,:,L) )*
     .                              log((ple_bkg0(:,:,L+1))/(ple_bkg0(:,:,L)  ))
     .                             -  (  pke_bkg (:,:,L+1) - pke_bkg (:,:,L) )*
     .                                (  ple_bkg (:,:,L+1) * ple_bkg0(:,:,L)
     .                                -  ple_bkg (:,:,L)   * ple_bkg0(:,:,L+1) )
     .                                / (ple_bkg0(:,:,L+1)*ple_bkg0(:,:,L))
     .                             )  / (kappa*( log(ple_bkg0(:,:,L+1)/ple_bkg0(:,:,L)) )**2)
               enddo
               deallocate( dpke_bkg )

#else /* _DIFFERENCING_ */

!              ... based on difference between analysis and background pressure edges
!              ----------------------------------------------------------------------
               pke_bkg(:,:,:) = (ple_bkg0(:,:,:)+ple_bkg(:,:,:))** kappa
               do L=1,lm
                  dpk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                           / ( kappa*log((ple_bkg0(:,:,L+1)+ple_bkg(:,:,L+1))/(ple_bkg0(:,:,L)+ple_bkg(:,:,L))) )
     .                          - pk_bkg(:,:,L)
               enddo

#endif /* _DIFFERENCING_ */

!              Calculate correction to dry potential temperature
!              -------------------------------------------------
               th_bkg =  - th_bkg0 * ( dpk_bkg/pk_bkg + eps*q_bkg/(1+eps*q_bkg0) ) 
     .                   + thv_bkg / (                     pk_bkg*(1+eps*q_bkg0) )

!              If so, output increments for diagnostics purposes
!              -------------------------------------------------
               if ( trim(incout) /= 'x' ) then
                    open (lugrd, file=trim(incout),form='unformatted',access='sequential')
                    call write_fv (   u_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv (   v_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv ( thv_bkg,imb,jmb,lm,lugrd,4, lattice_bkg ) ! note: output here is del_Tv
                    call write_fv ( dpk_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv ( ple_bkg(:,:,2:lm+1),imb,jmb,lm,lugrd,4, lattice_bkg ) ! note: ple minue top layer
                    call write_fv (   q_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv (  o3_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    close(lugrd)
                    if(myid==0) print *, 'Wrote model increment out to: ', trim(incout)
               endif 

!              Construct D-Grid Wind Increments
!              --------------------------------
               allocate ( glo(imbglobal,jmbglobal) )
               do L=1,lm
                                               call timebeg ('   Gather')
                                               call gather_2d  ( glo,u_bkg(1,1,L),lattice_bkg )
                                               call timeend ('   Gather')
                                               call timebeg (' atod')
                   if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imbglobal,jmbglobal,1,2 )
                                               call timeend (' atod')
                                               call timebeg ('   Scatter')
                                               call scatter_2d ( glo,u_bkg(1,1,L),lattice_bkg )
                                               call timeend ('   Scatter')
                                               call timebeg ('   Gather')
                                               call gather_2d  ( glo,v_bkg(1,1,L),lattice_bkg )
                                               call timeend ('   Gather')
                                               call timebeg (' atod')
                   if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imbglobal,jmbglobal,1,1 )
                                               call timeend (' atod')
                                               call timebeg ('   Scatter')
                                               call scatter_2d ( glo,v_bkg(1,1,L),lattice_bkg )
                                               call timeend ('   Scatter')
               enddo
               deallocate ( glo )

!              Now add increments to background 
!              --------------------------------
                           ! the following is no-data/harm because of precision
			   ! of rst vs ana files; however, in presence of remap
			   ! this is not an issue
               allocate ( ps_ana(imb,jmb) )
	       ps_ana(:,:) = ple_bkg0(:,:,lm+1) + ple_bkg(:,:,lm+1)
               do L=1,lm+1 
                  ple_bkg(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
               enddo
	       deallocate ( ps_ana )
               pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa
               do L=1,lm
                  pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
               enddo
               !ple_bkg =  ple_bkg0 + ple_bkg  ! <-- this is funny for an eta-analysis
                                               ! but the above is no-data/harm
               !pk_bkg =   pk_bkg  + dpk_bkg   ! this is no-data/no-harm

                th_bkg =   th_bkg0 +  th_bkg
                 u_bkg =    u_bkg0 +   u_bkg
                 v_bkg =    v_bkg0 +   v_bkg
                 q_bkg =    q_bkg0 +   q_bkg
		 
!              Convert analyzed O3 to OX
!              -------------------------
               call updox ( imb,jmb,lm, o3_bkg, o3_bkg0, ple_bkg, coszb )		 


!              Construct A-Grid Wind Increments (for remap purposes)
!              --------------------------------
               allocate ( glo(imbglobal,jmbglobal) )
               do L=1,lm
                                              call timebeg ('   Gather')
                                              call gather_2d  ( glo,u_bkg(1,1,L),lattice_bkg )
                                              call timeend ('   Gather')
                  if( lattice_bkg%myid.eq.0 ) call dtoa       ( glo,glo,imbglobal,jmbglobal,1,2 )
                                              call timebeg ('   Scatter')
                                              call scatter_2d ( glo,u_bkg(1,1,L),lattice_bkg )
                                              call timeend ('   Scatter')
                                              call timebeg ('   Gather')
                                              call gather_2d  ( glo,v_bkg(1,1,L),lattice_bkg )
                                              call timeend ('   Gather')
                  if( lattice_bkg%myid.eq.0 ) call dtoa       ( glo,glo,imbglobal,jmbglobal,1,1 )
                                              call timebeg ('   Scatter')
                                              call scatter_2d ( glo,v_bkg(1,1,L),lattice_bkg )
                                              call timeend ('   Scatter')
               enddo
               deallocate ( glo )
 
               thv_bkg = th_bkg  ! use thv slot for dry theta

           else ! < increment >

!             Convert analyzed O3 to OX
!             -------------------------
              if (do_o3) call  getox ( imb,jmb,lm, o3_bkg, ple_bkg, coszb )

           endif ! < increment >

           where(phis_bkg<0.0) phis_bkg = 0.0
           where(phis_tmp<0.0) phis_tmp = 0.0

!          Remap Based on TOPO Differences
!          -------------------------------
           if( myid.eq.0 ) print *, 'Remapping Data to BKG Topography'
           call timebeg(' remap')
           call remap ( ple_bkg,
     .                    u_bkg,
     .                    v_bkg,
     .                  thv_bkg,
     .                    q_bkg,
     .                   o3_bkg,
     .                 phis_tmp,phis_bkg,ak,bk,imb,jmb,lm,ihaveth )
           call timeend(' remap')
           if( myid.eq.0 ) print *, 'Remapping Complete'

!          Construct D-Grid Winds
!          ----------------------
           allocate ( glo(imbglobal,jmbglobal) )
           do L=1,lm
                                           call timebeg ('   Gather')
                                           call gather_2d  ( glo,u_bkg(1,1,L),lattice_bkg )
                                           call timeend ('   Gather')
                                           call timebeg (' atod')
               if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imbglobal,jmbglobal,1,2 )
                                           call timeend (' atod')
                                           call timebeg ('   Scatter')
                                           call scatter_2d ( glo,u_bkg(1,1,L),lattice_bkg )
                                           call timeend ('   Scatter')
                                           call timebeg ('   Gather')
                                           call gather_2d  ( glo,v_bkg(1,1,L),lattice_bkg )
                                           call timeend ('   Gather')
                                           call timebeg (' atod')
               if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imbglobal,jmbglobal,1,1 )
                                           call timeend (' atod')
                                           call timebeg ('   Scatter')
                                           call scatter_2d ( glo,v_bkg(1,1,L),lattice_bkg )
                                           call timeend ('   Scatter')
           enddo
           deallocate ( glo )

!          Re-construct PK Model Variable after remapping
!          ----------------------------------------------
           pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa
           do L=1,lm
              pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                      / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
           enddo

!          Construct Dry Potential Temperature
!          -----------------------------------
           if( increment ) then
               th_bkg = thv_bkg 
           else
               th_bkg = thv_bkg/(1+eps*q_bkg)
           endif

           deallocate (    coszb )
           deallocate ( phis_tmp )

      else ! if( imaglobal.eq.imbglobal  .and. 
           !     jmaglobal.eq.jmbglobal  .and.
           !     .not. doremap           ) then

!          Construct D-Grid Winds
!          ----------------------
           allocate ( glo(imbglobal,jmbglobal) )
           do L=1,lm
                                           call timebeg ('   Gather')
                                           call gather_2d  ( glo,u_ana(1,1,L),lattice_ana )
                                           call timeend ('   Gather')
                                           call timebeg (' atod')
               if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imaglobal,jmaglobal,1,2 )
                                           call timeend (' atod')
                                           call timebeg ('   Scatter')
                                           call scatter_2d ( glo,u_ana(1,1,L),lattice_ana )
                                           call timeend ('   Scatter')
                                           call timebeg ('   Gather')
                                           call gather_2d  ( glo,v_ana(1,1,L),lattice_ana )
                                           call timeend ('   Gather')
                                           call timebeg (' atod')
               if( lattice_ana%myid.eq.0 ) call atod       ( glo,glo,imaglobal,jmaglobal,1,1 )
                                           call timeend (' atod')
                                           call timebeg ('   Scatter')
                                           call scatter_2d ( glo,v_ana(1,1,L),lattice_ana )
                                           call timeend ('   Scatter')
           enddo
           deallocate ( glo )

!          BKG and ANA Horizontal Resolutions Match
!          ----------------------------------------
           if ( increment ) then

!              CASE:  analysis and gcm IC's have same resolution,
!                     no remapping is requested and increment is 
!                     an input field.

!              Add increment of ple to background ple, and calculate pk
!              --------------------------------------------------------
                           ! the following is no-data/harm because of precision
			   ! of rst vs ana files; however, in presence of remap
			   ! this is not an issue
               allocate ( ps_ana(imb,jmb) )
	       ps_ana(:,:) = ple_bkg0(:,:,lm+1) + ple_ana(:,:,lm+1)
               do L=1,lm+1 
                  ple_bkg(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
               enddo
	       deallocate ( ps_ana )	       

#ifdef _DIFFERENCING_

!              Calculate dpk based finite differencing
!              ---------------------------------------
               allocate( dpke_bkg(imb,jmb,lm+1) )
                pke_bkg(:,:,:)  =      (ple_bkg0(:,:,:))** kappa
               dpke_bkg(:,:,:) = kappa*(ple_bkg0(:,:,:))**(kappa-1)*ple_ana(:,:,:)
               do L=1,lm
                  dpk_bkg(:,:,L) = (  ( dpke_bkg (:,:,L+1)-dpke_bkg(:,:,L) )*
     .                              log( ple_bkg0(:,:,L+1)/ple_bkg0(:,:,L) )
     .                             -  (  pke_bkg (:,:,L+1)-pke_bkg (:,:,L) )*
     .                                (  ple_ana (:,:,L+1)*ple_bkg0(:,:,L)
     .                                -  ple_ana (:,:,L)  *ple_bkg0(:,:,L+1) )
     .                                / (ple_bkg0(:,:,L+1)*ple_bkg0(:,:,L))
     .                             )  / (kappa*( log(ple_bkg0(:,:,L+1)/ple_bkg0(:,:,L)) )**2)
               enddo
               deallocate( dpke_bkg )

#else /* _DIFFERENCING_ */

!              Calculate dpk based on diff between analyzed pk and background pk
!              -----------------------------------------------------------------
               pke_bkg = ple_bkg** kappa  ! based on analyzed pressure edges
               do L=1,lm
                  dpk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                           / ( kappa*log((ple_bkg(:,:,L+1))/(ple_bkg(:,:,L))) )
     .                           - pk_bkg(:,:,L)
               enddo

#endif /* _DIFFERENCING_ */

!              Calculate correction to dry potential temperature
!              -------------------------------------------------
               th_bkg =  - th_bkg0 * ( dpk_bkg/pk_bkg + eps*q_ana/(1+eps*q_bkg0) ) 
     .                   + thv_ana / (                     pk_bkg*(1+eps*q_bkg0) )

!              If so, output increments for diagnostics purposes
!              -------------------------------------------------
               if ( trim(incout) /= 'x' ) then
                    open (lugrd, file=trim(incout),form='unformatted',access='sequential')
                    call write_fv (   u_ana,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv (   v_ana,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv ( thv_ana,imb,jmb,lm,lugrd,4, lattice_bkg ) ! note: output here is del_Tv
                    call write_fv ( dpk_bkg,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv ( ple_ana(:,:,2:lm+1),imb,jmb,lm,lugrd,4, lattice_bkg ) ! note: ple excluding pressure edge
                    call write_fv (   q_ana,imb,jmb,lm,lugrd,4, lattice_bkg )
                    call write_fv (  o3_ana,imb,jmb,lm,lugrd,4, lattice_bkg )
                    close(lugrd)
                    if(myid==0) print *, 'Wrote model increment out to: ', trim(incout)
               endif

!              Update pk (only after correction to th has been calculated)
!              -----------------------------------------------------------
               pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa
               do L=1,lm
                  pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
               enddo

               th_bkg =  th_bkg0 +  th_bkg
                u_bkg =   u_bkg0 +   u_ana
                v_bkg =   v_bkg0 +   v_ana
                q_bkg =   q_bkg0 +   q_ana
		
!              Convert analyzed O3 to OX
!              -------------------------
               call updox ( imb,jmb,lm, o3_ana, o3_bkg0, ple_bkg, cosza )
               o3_bkg = o3_ana

           else ! < increment >

!              CASE:  analysis and gcm IC's have same resolution,
!                     no remapping is requested and analysis is 
!                     an input field.

!              Convert analyzed O3 to OX
!              -------------------------
               if(do_o3) call  getox ( imb,jmb,lm, o3_ana, ple_ana, cosza )

                 u_bkg =    u_ana
                 v_bkg =    v_ana
               thv_bkg =  thv_ana
               ple_bkg =  ple_ana
                 q_bkg =    q_ana
                o3_bkg =   o3_ana

!              Construct PK Model Variable
!              ---------------------------
               pk_bkg = pk_ana

!              Construct Dry Potential Temperature
!              -----------------------------------
               th_bkg = thv_bkg/(1+eps*q_bkg)

           endif ! < increment >

      endif ! < resolution of ana/inc >

      deallocate (   cosza )
      deallocate (   u_ana )
      deallocate (   v_ana )
      deallocate ( thv_ana )
      deallocate ( ple_ana )
      deallocate (   q_ana )
      deallocate (  o3_ana )


! Modify vertically integrated wind increment to be non-divergent
! ---------------------------------------------------------------

      if( method.eq.-999 ) then
          if( myid.eq.0 )       print *, 'No Wind Adjustment  Change to Divergence'
      else
          if ( showdiv ) then
             if( myid.eq.0 ) then
                 print *, 'Creating GEOS-5 fvcore_internal_restart: ',trim(anaeta)
                 open (lugrd,file='windiv.grd',form='unformatted',access='sequential')
             endif
             call write_d ( lugrd, u_bkg0,v_bkg0,ple_bkg0,imb,jmb,lm,lattice_bkg )  ! Background
             call write_d ( lugrd, u_bkg ,v_bkg ,ple_bkg ,imb,jmb,lm,lattice_bkg )  ! Analysis before Adjustment
          endif

          if( myid.eq.0 ) then
              if( method.eq.1 ) print *, 'Minimizing Relative Change to Divergence'
              if( method.eq.2 ) print *, 'Minimizing Absolute Change to Divergence'
                                print *, 'Calling Windfix'
          endif
          call timebeg (' windfix')
          call windfix ( u_bkg ,v_bkg ,ple_bkg ,
     .                   u_bkg0,v_bkg0,ple_bkg0,imb,jmb,lm,lattice_bkg,'D',method )
          call timeend (' windfix')

          if ( showdiv ) then
             call write_d ( lugrd, u_bkg,v_bkg,ple_bkg,imb,jmb,lm,lattice_bkg )  ! Analysis after Adjustment
             close (lugrd)
          endif

      endif


! **********************************************************************
! ****                 Write Dycore Internal Restart                ****
! **********************************************************************

      anaeta = trim(dynrst) // '.' // trim(tag) // '.' // date // '_' // hour // 'z'

      call timebeg(' writefv')
      if( myid.eq.0 ) then
          print *, 'Creating GEOS-5 fvcore_internal_restart: ',trim(anaeta)
          open (20,file=trim(anaeta),form='unformatted',access='sequential')
          write(20) headr1
          write(20) headr2
          write(20) ak
          write(20) bk
      endif

      call write_fv (   u_bkg,imb,jmb,lm  ,20,kind(ak(1)),lattice_bkg )
      call write_fv (   v_bkg,imb,jmb,lm  ,20,kind(ak(1)),lattice_bkg )
      call write_fv (  th_bkg,imb,jmb,lm  ,20,kind(ak(1)),lattice_bkg )
      call write_fv ( ple_bkg,imb,jmb,lm+1,20,kind(ak(1)),lattice_bkg )
      call write_fv (  pk_bkg,imb,jmb,lm  ,20,kind(ak(1)),lattice_bkg )

      if( myid.eq.0 ) close (20)
      call timeend(' writefv')

! **********************************************************************
! ****     Merge Moist Internal Restart with Analysis ANA Data      ****
! **********************************************************************

      anaeta = trim(moistrst) // '.' // trim(tag) // '.' // date // '_' // hour // 'z'

      call timebeg(' writemois')
      if( myid.eq.0 ) then
          print *, 'Creating GEOS-5  moist_internal_restart: ',trim(anaeta)
      endif

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      open  (20,file=trim(anaeta)  ,form='unformatted',access='sequential')

      allocate ( glo(imb,jmb) )

! First moist variable is SPHU
! ----------------------------
      do L=1,lm
         call readit ( glo,imb,jmb,1,10,lattice_bkg,moistrst,rc )
                       glo(:,:) = q_bkg(:,:,L)
         call writit ( glo,imb,jmb,1,20,lattice_bkg )
      enddo

! Copy Rest of Moist Internal State
! ---------------------------------
               rc =  0
      dowhile (rc.eq.0)
      do L=1,lm
                     call readit ( glo,imb,jmb,1,10,lattice_bkg,moistrst,rc )
         if(rc.eq.0) call writit ( glo,imb,jmb,1,20,lattice_bkg )
      enddo
      enddo

      deallocate ( glo )
      close (10)
      close (20)
      call timeend(' writemois')

! **********************************************************************
! ****     Merge Pchem Internal Restart with Analysis ANA Data      ****
! **********************************************************************

      if ( do_o3 ) then 

      anaeta = trim(pchemrst) // '.' // trim(tag) // '.' // date // '_' // hour // 'z'
      call timebeg(' writechem')
      if( myid.eq.0 ) then
          print *, 'Creating GEOS-5  pchem_internal_restart: ',trim(anaeta)
          print *
      endif
      open  (10,file=trim(pchemrst),form='unformatted',access='sequential')
      open  (20,file=trim(anaeta)  ,form='unformatted',access='sequential')

      allocate ( glo(imb,jmb) )

! First pchem variable is OZONE
! -----------------------------
      do L=1,lm
         call readit ( glo,imb,jmb,1,10,lattice_bkg,pchemrst,rc )
                       glo(:,:) = o3_bkg(:,:,L)
         call writit ( glo,imb,jmb,1,20,lattice_bkg )
      enddo
                                                                                                                                   
! Copy Rest of Pchem Internal State
! ---------------------------------
               rc =  0
      dowhile (rc.eq.0)
      do L=1,lm
                     call readit ( glo,imb,jmb,1,10,lattice_bkg,pchemrst,rc )
         if(rc.eq.0) call writit ( glo,imb,jmb,1,20,lattice_bkg )
      enddo
      enddo
                                                                                                                                   
      deallocate ( glo )
      close (10)
      close (20)
      call timeend(' writechem')

      endif ! < do_o3 >

! **********************************************************************
! ****                   Write Timing Information                   ****
! **********************************************************************

                      call timeend ('main')
      if( myid.eq.0 ) call timepri (6)
                      call my_finalize

      stop
      end

      subroutine getit ( id,name,nymd,nhms,im,jm,lbeg,lm,q,lattice )
      use dynamics_lattice_module
      implicit none
#ifdef mpi
      include 'mpif.h'
#endif
      type ( dynamics_lattice_type ) lattice
      integer L,id,nymd,nhms,im,jm,img,jmg,lbeg,lm
      real   q(im,jm,lm)
      real,allocatable :: glo(:,:,:)
      character(*) name
      integer rc,ierror
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg,lm) )
      if( lattice%myid.eq.0 ) then
          call gfio_getvar ( id,trim(name),nymd,nhms,img,jmg,lbeg,lm,glo,rc )
          if( rc.ne.0 ) print *, '!! Error Reading: ',trim(name),'  RC = ',rc
      endif
#ifdef mpi
      call mpi_bcast ( rc,1,mpi_integer,0,lattice%comm,ierror )
#endif
      if ( rc.ne.0 ) then
         call my_finalize
         call exit (7)
      endif
      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo(1,1,L),q(1,1,L),lattice )
      enddo
      call timeend ('   Scatter')
      deallocate ( glo )
      return
      end

      subroutine putit ( id,name,nymd,nhms,im,jm,lbeg,lm,q,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer L,id,nymd,nhms,im,jm,img,jmg,lbeg,lm
      real   q(im,jm,lm)
      real,allocatable :: glo(:,:,:)
      character(*) name
      integer rc
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg,lm) )
      call timebeg ('   Gather')
      do L=1,lm
      call gather_2d ( glo(1,1,L),q(1,1,L),lattice )
      enddo
      call timeend ('   Gather')
      if( lattice%myid.eq.0 ) then
          call gfio_putvar ( id,trim(name),nymd,nhms,img,jmg,lbeg,lm,glo,rc )
          if( rc.ne.0 ) print *, '!! Error Writing: ',trim(name),'  RC = ',rc
      endif
      deallocate ( glo )
      return
      end

      subroutine readit ( q,im,jm,lm,ku,lattice,filename,rc )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
#ifdef mpi
      include 'mpif.h'
#endif
      character(*) filename
      integer  im,jm,lm,L,ku,img,jmg,rc,ierror
      real   q(im,jm,lm)
      real,   allocatable :: glo(:,:)
      real*4, allocatable ::   a(:,:)
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg) )
      allocate (   a(img,jmg) )
      do L=1,lm
         if( lattice%myid.eq.0 ) then
             read(ku,iostat=rc) a 
                          glo = a
         endif
#ifdef mpi
         call mpi_bcast ( rc,1,mpi_integer,0,lattice%comm,ierror )
#endif
         if( rc.eq.0 ) then
             call timebeg ('   Scatter')
             call scatter_2d ( glo,q(1,1,L),lattice )
             call timeend ('   Scatter')
         else if( rc.gt.0 ) then
             if( lattice%myid.eq.0 ) print *, 'Error Reading File: ',trim(filename)
             call my_finalize
             call exit (7)
         endif
      enddo
      deallocate ( a,glo )
      return
      end
 
      subroutine writit ( q,im,jm,lm,ku,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  im,jm,lm,L,ku,img,jmg
      real   q(im,jm,lm)
      real,   allocatable :: glo(:,:)
      real*4, allocatable ::   a(:,:)
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg) )
      allocate (   a(img,jmg) )
      do L=1,lm
         call timebeg ('   Gather')
         call gather_2d ( glo,q(1,1,L),lattice )
         call timeend ('   Gather')
         if( lattice%myid.eq.0 ) then
                       a = glo
             write(ku) a
         endif
      enddo
      deallocate ( a,glo )
      return
      end
 
      subroutine read_fv ( q,im,jm,lm,ku,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  im,jm,lm,L,ku,img,jmg
      real   q(im,jm,lm)
      real,   allocatable :: glo4(:,:)
      real*8, allocatable :: glo8(:,:)
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo4(img,jmg) )
      allocate ( glo8(img,jmg) )
      do L=1,lm
         if( lattice%myid.eq.0 ) then
             read(ku) glo8
                      glo4 = glo8
         endif
         call timebeg ('   Scatter')
         call scatter_2d ( glo4,q(1,1,L),lattice )
         call timeend ('   Scatter')
      enddo
      deallocate ( glo4 )
      deallocate ( glo8 )
      return
      end
 
      subroutine write_fv ( q,im,jm,lm,ku,mykind,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  im,jm,lm,L,ku,img,jmg,mykind
      real   q(im,jm,lm)
      real*4, allocatable :: glo4(:,:)
      real*8, allocatable :: glo8(:,:)
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo4(img,jmg) )
      allocate ( glo8(img,jmg) )
      do L=1,lm
         call timebeg ('   Gather')
         call gather_2d ( glo4,q(1,1,L),lattice )
         call timeend ('   Gather')
         if( lattice%myid.eq.0 ) then
             if ( mykind==8 ) then
                           glo8 = glo4
                 write(ku) glo8
             else
                 write(ku) glo4
             endif
         endif
      enddo
      deallocate ( glo4 )
      deallocate ( glo8 )
      return
      end
 
      subroutine hflip ( q,im,jm,lm,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer    im,jm,lm,i,j,L,img,jmg
      real  q(im,jm,lm)
      real, allocatable :: glo(:,:)
      real, allocatable :: dum(:)
      img = lattice%imglobal
      jmg = lattice%jmglobal

      allocate ( glo(img,jmg) )
      allocate ( dum(img)     )

      do L=1,lm
         call timebeg ('   Gather')
         call gather_2d ( glo,q(1,1,L),lattice )
         call timeend ('   Gather')
         if( lattice%myid.eq.0 ) then
             do j=1,jmg
             do i=1,img/2
                dum(i) = glo(i+img/2,j)
                dum(i+img/2) = glo(i,j)
             enddo
                glo(:,j) = dum(:)
             enddo
         endif
         call timebeg ('   Scatter')
         call scatter_2d ( glo,q(1,1,L),lattice )
         call timeend ('   Scatter')
      enddo

      deallocate ( glo )
      deallocate ( dum )
      return
      end

      subroutine getoxinc ( im,jm,lm, ox, oxbkg, ple, cosza, ak )

      implicit none
      integer, intent(in)  :: im, jm, lm
      real, intent(inout)  :: ox(im,jm,lm)    ! on  input, contains inc O3
                                              ! on output, contains inc OX
      real, intent(in)     :: cosza(im,jm)    ! cosine of zenith angle
      real, intent(in)     :: ple(im,jm,lm+1) ! pressure edge increments
      real, intent(in)     :: oxbkg(im,jm,lm) ! full ox background
      real*8, intent(in)   :: ak(lm+1)

      real, parameter :: czaLimit = 1.0d-5
      integer :: i,j,L
      real    :: rlog, prsm, beta
      real, allocatable ::     pl(:,:,:)
      real, allocatable :: ro3ox1(:,:,:)
      real, allocatable :: ro3ox2(:,:,:)
      real, allocatable ::    akm(:)

      allocate ( ro3ox1(im,jm,lm) )
      allocate ( ro3ox2(im,jm,lm) )
      allocate (     pl(im,jm,lm) )
      allocate (          akm(lm) )

      ro3ox1 = 1.d0
      ro3ox2 = 0.d0
      pl  = 0.5 * ( ple(:,:,1:lm) + ple(:,:,2:lm+1) )
      akm = 0.5 * (      ak(1:lm) +      ak(2:lm+1) )
      beta = 4.5/alog(10.)
      do L = 1, lm
         do j = 1, jm
         do i = 1, im
         ! the following is to prevent taking pressures that go <0 near the top
         prsm = max(akm(lm),akm(L)+pl(i,j,L))
           if ( prsm<100.0 .and. cosza(i,j)>czaLimit ) then
              rlog = log10(0.01*(prsm))
              ro3ox1(i,j,L) = exp(1.5*(rlog)**2)
              ro3ox2(i,j,L) =     beta * rlog * pl(i,j,L)/prsm
           endif
         enddo
         enddo
      enddo
      ox = ox * 1.0d-6*ro3ox1 + oxbkg * ro3ox2

      deallocate (    akm )
      deallocate (     pl )
      deallocate ( ro3ox1 )
      deallocate ( ro3ox2 )

      end subroutine getoxinc

      subroutine getox ( im,jm,lm, ox, ple, cosza )

      implicit none
      integer, intent(in)  :: im, jm, lm
      real, intent(inout)  :: ox(im,jm,lm)    ! on  input, contains O3
                                              ! on output, contains OX
      real, intent(in)     :: cosza(im,jm)    ! cosine of zenith angle
      real, intent(in)     :: ple(im,jm,lm+1) ! pressure edges

      real, parameter :: czaLimit = 1.0d-5
      integer :: L
      real, allocatable ::    pl(:,:,:)
      real, allocatable :: ro3ox(:,:,:)

      allocate ( ro3ox(im,jm,lm) )
      allocate (    pl(im,jm,lm) )

      ro3ox = 1.d0
      pl = 0.5 * ( ple(:,:,1:lm) + ple(:,:,2:lm+1) )
      do L = 1, lm
         where (pl(:,:,L) < 100.0 .and. cosza > czaLimit)
           ro3ox(:,:,L) = exp(-1.5d0*(log10(0.01*pl(:,:,L)))**2)
         end where
      enddo
      ox = ox * 1.0d-6/ro3ox

      deallocate (    pl )
      deallocate ( ro3ox )

      end subroutine getox

      subroutine updox ( im,jm,lm, ox, oxbkg, ple, cosza )

      implicit none
      integer, intent(in)  :: im, jm, lm
      real, intent(inout)  :: ox(im,jm,lm)    ! on  input, O3 increment
                                              ! on output, OX analysis
      real, intent(in)     :: oxbkg(im,jm,lm) ! O3 background				      
      real, intent(in)     :: cosza(im,jm)    ! cosine of zenith angle
      real, intent(in)     :: ple(im,jm,lm+1) ! pressure edges

      real, parameter :: czaLimit = 1.0d-5
      integer :: L
      real, allocatable ::    pl(:,:,:)
      real, allocatable :: ro3ox(:,:,:)

      allocate ( ro3ox(im,jm,lm) )
      allocate (    pl(im,jm,lm) )

      ro3ox = 1.d0
      pl = 0.5 * ( ple(:,:,1:lm) + ple(:,:,2:lm+1) )
      do L = 1, lm
         where (pl(:,:,L) < 100.0 .and. cosza > czaLimit)
           ro3ox(:,:,L) = exp(-1.5d0*(log10(0.01*pl(:,:,L)))**2)
         end where
      enddo
      ox = oxbkg*1.0d+6*ro3ox + ox   ! update o3
      ox = ox   *1.0d-6/ro3ox        ! convert updated o3 to ox

      deallocate (    pl )
      deallocate ( ro3ox )

      end subroutine updox

      subroutine minmax (q,im,jm,L)
      real   q(im,jm)
      qmin = q(1,1)
      qmax = q(1,1)
      do j=1,jm
      do i=1,im
      qmin = min( qmin,q(i,j) )
      qmax = max( qmax,q(i,j) )
      enddo
      enddo
      print *, 'L: ',L,' qmin: ',qmin,' qmax: ',qmax
      return
      end

      subroutine usage ( myid )
      integer  ierror
      if(myid.eq.0) then
         print *, "Usage:  "
         print *
         print *, " hdf2rs_$ARCH.x  -dynrst        FV internal_restart  "
         print *, "                 -moistrst   Moist internal_restart  "
         print *, "                 -bkg        input: Background-Style bkg.eta file "
         print *, "                 -sfc        input: Background-Style bkg.sfc file "
         print *, "                 -ana        input: Analysis-Style ana.eta file   "
         print *, "                 -inc        input: Increment-Style inc.eta file   "
         print *, "                 -nymd       (Date  to Read From ANA.ETA File (YYYYMMDD),"
         print *, "                              Default: Date within DYNRST)"
         print *, "                 -nhms       (Time  to Read From ANA.ETA File (HHMMSS),"
         print *, "                              Default: Time within DYNRST)"
         print *, "                 -topo       (Optional Topography File if Different from ANA) "
         print *, "                 -divr       (Optional flag to Minimize Relative Adjustment of"
         print *, "                              Div.Inc.)"
         print *, "                 -diva       (Optional flag to Minimize Absolute Adjustment of"
         print *, "                              Div.Inc.)"
         print *, "                 -showdiv    Output file with bkg/ana/ and corrected ana div"
         print *, "                 -oinc       output: filename for output increment"
         print *, "                 -remap      forces remapping"
         print *
         print *, " NOTES: "
         print *, "  1) opt -bkg only makes sense together with -inc. In this case, "
         print *, "     bkg file is overwritten on output with bkg+inc (analysis field)"
         print *, "  2) topography specification no longer determines remap (as in older versions)"
         print *
         print *
      endif
#ifdef mpi
      call mpi_finalize (ierror)
#endif
      stop
      end
#ifdef _REDUNDANT_
      subroutine atod ( qa,qd,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'A' gridded data                  ****
C ****                     to 'D' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted left (westward),                     ****
C ****        u is shifted down (southward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift left (-dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qa(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) + qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) - qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qd(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****             shift down (-dy/2)                  ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qa(i,j+1,L)
         qay(j+jmm1,L) = -qa(i+imh,jm-j,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) + qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) - qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qd(i,j+1,L)        =  qay(j,L)
         qd(i+imh,jm-j+1,L) = -qay(j+jmm1,L)
         enddo
         enddo
      enddo

      endif

      return
      end

      subroutine dtoa ( qd,qa,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'D' gridded data                  ****
C ****                     to 'A' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted right (eastward),                    ****
C ****        u is shifted up   (northward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift right (dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qd(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) - qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) + qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qa(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****               shift up (dy/2)                   ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qd(i,j+1,L)
         qay(j+jmm1,L) = -qd(i+imh,jm-j+1,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) - qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) + qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qa(i,j+1,L)      =  qay(j,L)
         qa(i+imh,jm-j,L) = -qay(j+jmm1,L)
         enddo
         enddo

      enddo

         do L=1,lm
         do i=1,imh
         qa(i+imh,jm,L) = -qa(i,jm,L)
         qa(i,1,L)      = -qa(i+imh,1,L)
         enddo
         enddo
      endif

      return
      end

      subroutine rfftmlt (a,work,trigs,ifax,inc,jump,n,lot,isign)
      integer INC, JUMP, N, LOT, ISIGN
      real(kind=KIND(1.0)) A(N),WORK(N),TRIGS(N)
      integer IFAX(*)
!
!     SUBROUTINE "FFT991" - MULTIPLE REAL/HALF-COMPLEX PERIODIC
!     FAST FOURIER TRANSFORM
!
!     SAME AS FFT99 EXCEPT THAT ORDERING OF DATA CORRESPONDS TO
!     THAT IN MRFFT2
!
!     PROCEDURE USED TO CONVERT TO HALF-LENGTH COMPLEX TRANSFORM
!     IS GIVEN BY COOLEY, LEWIS AND WELCH (J. SOUND VIB., VOL. 12
!     (1970), 315-337)
!
!     A IS THE ARRAY CONTAINING INPUT AND OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*LOT
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N/2
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N IS THE LENGTH OF THE DATA VECTORS
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:
!         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
!         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
!
!     ORDERING OF DATA:
!         X(0),X(1),X(2),...,X(N-1)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS IN
!     PARALLEL
!
!     *** N.B. N IS ASSUMED TO BE AN EVEN NUMBER
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!
!     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
!               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
!
! THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR
!     CALL Q8QST4 ( 4HXLIB, 6HFFT99F, 6HFFT991, 10HVERSION 01)
!FPP$ NOVECTOR R
      integer NFAX, NH, NX, INK
      integer I, J, IBASE, JBASE, L, IGO, IA, LA, K, M, IB

      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      INK=INC+INC
      IF (ISIGN.EQ.+1) GO TO 30
!
!     IF NECESSARY, TRANSFER DATA TO WORK AREA
      IGO=50
      IF (MOD(NFAX,2).EQ.1) GOTO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE
!
      IGO=60
      GO TO 40
!
!     PREPROCESSING (ISIGN=+1)
!     ------------------------
!
   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60
!
!     COMPLEX TRANSFORM
!     -----------------
!
   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF (IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS, 
     *    INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS, 
     *     2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE
!
      IF (ISIGN.EQ.-1) GO TO 130
!
!     IF NECESSARY, TRANSFER DATA FROM WORK AREA
      IF (MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE
!
!     FILL IN ZEROS AT END
  110 CONTINUE
      IB=N*INC+1
!DIR$ IVDEP
      DO 120 L=1,LOT
      A(IB)=0.0
      A(IB+INC)=0.0
      IB=IB+JUMP
  120 CONTINUE
      GO TO 140
!
!     POSTPROCESSING (ISIGN=-1):
!     --------------------------
!
  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
!
  140 CONTINUE
      RETURN
      END

      subroutine fftfax (n,ifax,trigs)
      integer IFAX(13)
      integer N
      REAL(kind=KIND(1.0))      TRIGS(*)
!
! MODE 3 IS USED FOR REAL/HALF-COMPLEX TRANSFORMS.  IT IS POSSIBLE
! TO DO COMPLEX/COMPLEX TRANSFORMS WITH OTHER VALUES OF MODE, BUT
! DOCUMENTATION OF THE DETAILS WERE NOT AVAILABLE WHEN THIS ROUTINE
! WAS WRITTEN.
!
      integer I, MODE
      DATA MODE /3/
!FPP$ NOVECTOR R
      CALL FAX (IFAX, N, MODE)
      I = IFAX(1)
      IF (IFAX(I+1) .GT. 5 .OR. N .LE. 4) IFAX(1) = -99
      IF (IFAX(1) .LE. 0 ) WRITE(6,FMT="(//5X, ' FFTFAX -- INVALID N =', I5,/)") N
      IF (IFAX(1) .LE. 0 ) STOP 999
      CALL FFTRIG (TRIGS, N, MODE)
      RETURN
      END

      subroutine fft99a (a,work,trigs,inc,jump,n,lot)
      integer inc, jump, N, lot
      real(kind=KIND(1.0)) A(N),WORK(N)
      REAL(kind=KIND(1.0)) TRIGS(N)
!
!     SUBROUTINE FFT99A - PREPROCESSING STEP FOR FFT99, ISIGN=+1
!     (SPECTRAL TO GRIDPOINT TRANSFORM)
!
!FPP$ NOVECTOR R
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) C, S
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
!DIR$ IVDEP
      DO 10 L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      WORK(JA)=(A(IA)+A(IB))- 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+ 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+ 
     *     (A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))- 
     *     (A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   20 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
!DIR$ IVDEP
      DO 40 L=1,LOT
      WORK(JA)=2.0*A(IA)
      WORK(JA+1)=-2.0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fft99b (work,a,trigs,inc,jump,n,lot)
      integer INC, JUMP, N, LOT
      real(kind=KIND(1.0)) WORK(N),A(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) SCALE
      real(kind=KIND(1.0)) C, S
!
!     SUBROUTINE FFT99B - POSTPROCESSING STEP FOR FFT99, ISIGN=-1
!     (GRIDPOINT TO SPECTRAL TRANSFORM)
!
!FPP$ NOVECTOR R
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      SCALE=1.0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1
!DIR$ IVDEP
      DO 10 L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JB)=SCALE*(WORK(IA)-WORK(IB))
      A(JA+INC)=0.0
      A(JB+INC)=0.0
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      SCALE=0.5*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB)) 
     *    +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB)) 
     *    -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   20 CONTINUE
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
      SCALE=2.0*SCALE
!DIR$ IVDEP
      DO 40 L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fax (ifax,n,mode)
      integer IFAX(10)
      integer N, MODE
!FPP$ NOVECTOR R
      integer NN, K, L, INC, II, ISTOP, ITEM, NFAX, I
      NN=N
      IF (IABS(MODE).EQ.1) GO TO 10
      IF (IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF ((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
   10 K=1
!     TEST FOR FACTORS OF 4
   20 IF (MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF (NN.EQ.1) GO TO 80
      GO TO 20
!     TEST FOR EXTRA FACTOR OF 2
   30 IF (MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF (NN.EQ.1) GO TO 80
!     TEST FOR FACTORS OF 3
   40 IF (MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF (NN.EQ.1) GO TO 80
      GO TO 40
!     NOW FIND REMAINING FACTORS
   50 L=5
      INC=2
!     INC ALTERNATELY TAKES ON VALUES 2 AND 4
   60 IF (MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF (NN.EQ.1) GO TO 80
      GO TO 60
   70 L=L+INC
      INC=6-INC
      GO TO 60
   80 IFAX(1)=K-1
!     IFAX(1) CONTAINS NUMBER OF FACTORS
      NFAX=IFAX(1)
!     SORT FACTORS INTO ASCENDING ORDER
      IF (NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
      END

      subroutine fftrig (trigs,n,mode)
      REAL(kind=KIND(1.0))      TRIGS(*)
      integer N, MODE
!FPP$ NOVECTOR R
      real(kind=KIND(1.0)) PI
      integer IMODE, NN, L, I, NH, LA
      real(kind=KIND(1.0)) DEL, ANGLE
      PI=2.0*ASIN(1.0)
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF (IMODE.EQ.1) RETURN
      IF (IMODE.EQ.8) RETURN
      DEL=0.5*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF (IMODE.LE.3) RETURN
      DEL=0.5*DEL
      LA=LA+NN
      IF (MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0*SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
      DEL=0.5*DEL
      DO 50 I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
      END

      subroutine vpassm (a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,ifac,la)
      integer INC1,INC2,INC3,INC4,LOT,N,IFAC,LA
      real(kind=KIND(1.0)) A(N),B(N),C(N),D(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
!
!     SUBROUTINE "VPASSM" - MULTIPLE VERSION OF "VPASSA"
!     PERFORMS ONE PASS THROUGH DATA
!     AS PART OF MULTIPLE COMPLEX FFT ROUTINE
!     A IS FIRST REAL INPUT VECTOR
!     B IS FIRST IMAGINARY INPUT VECTOR
!     C IS FIRST REAL OUTPUT VECTOR
!     D IS FIRST IMAGINARY OUTPUT VECTOR
!     TRIGS IS PRECALCULATED TABLE OF SINES & COSINES
!     INC1 IS ADDRESSING INCREMENT FOR A AND B
!     INC2 IS ADDRESSING INCREMENT FOR C AND D
!     INC3 IS ADDRESSING INCREMENT BETWEEN As & Bs
!     INC4 IS ADDRESSING INCREMENT BETWEEN Cs & Ds
!     LOT IS THE NUMBER OF VECTORS
!     N IS LENGTH OF VECTORS
!     IFAC IS CURRENT FACTOR OF N
!     LA IS PRODUCT OF PREVIOUS FACTORS
!
      real(kind=KIND(1.0)) SIN36, COS36, SIN72, COS72, SIN60
      DATA SIN36/0.587785252292473/,COS36/0.809016994374947/, 
     *      SIN72/0.951056516295154/,COS72/0.309016994374947/,
     *      SIN60/0.866025403784437/
      integer M, IINK, JINK, JUMP, IBASE, JBASE, IGO, IA, JA, IB, JB
      integer IC, JC, ID, JD, IE, JE
      integer I, J, K, L, IJK, LA1, KB, KC, KD, KE
      real(kind=KIND(1.0)) C1, S1, C2, S2, C3, S3, C4, S4
!
!FPP$ NOVECTOR R
      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.GT.4) RETURN
      GO TO (10,50,90,130),IGO
!
!     CODING FOR FACTOR 2
!
   10 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)-B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)-B(IB+I))
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 3
!
   50 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 55 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))
      C(JC+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))
      D(JB+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I)))
      D(JC+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I)))
      I=I+INC3
      J=J+INC4
   55 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 65 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)= 
     *     C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
   65 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 4
!
   90 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 95 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      D(JC+J)=(B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I))
      C(JB+J)=(A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))
      C(JD+J)=(A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))
      D(JB+J)=(B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I))
      D(JD+J)=(B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I))
      I=I+INC3
      J=J+INC4
   95 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 105 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      C(JC+J)= 
     *     C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    -S2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      D(JC+J)= 
     *     S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    +C2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      C(JB+J)= 
     *     C1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    -S1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)= 
     *     S1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    +C1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)= 
     *     C3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    -S3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)= 
     *     S3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    +C3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  105 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 5
!
  130 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      IE=ID+IINK
      JE=JD+JINK
      DO 140 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 135 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      C(JE+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      D(JB+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      D(JE+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      C(JC+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      C(JD+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      D(JC+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      D(JD+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  135 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  140 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 160 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      DO 150 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 145 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)= 
     *     C1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JE+J)= 
     *     C4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JE+J)= 
     *     S4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      C(JD+J)= 
     *     C3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JD+J)= 
     *     S3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      I=I+INC3
      J=J+INC4
  145 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  150 CONTINUE
      JBASE=JBASE+JUMP
  160 CONTINUE
      RETURN
      END
#endif /* _REDUNDANT_ */

      subroutine bin ( qin,im_in,jm_in,qout,im_out,jm_out,lm,undef,msgn,lat_i,lat_o )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lat_i
      type ( dynamics_lattice_type ) lat_o

#ifdef mpi
      include 'mpif.h'
#endif
      integer  comm,myid,npes
      integer  imgi,jmgi,imgo,jmgo

      integer   im_in ,jm_in ,msgn, lm
      integer   im_out,jm_out, L
      real      undef
      real    qin(im_in ,jm_in ,lm)
      real   qout(im_out,jm_out,lm)

      real, allocatable :: glo_i(:,:,:)
      real, allocatable :: glo_o(:,:,:)

c Temporary Array for Binning
c ---------------------------
      integer     imax
      integer     jmax
      parameter ( imax = 360*12 )
      parameter ( jmax = 180*12 )
      real qbin ( imax,jmax )

      integer index(lm),ierror
 
      call timebeg (' bin')

      comm = lat_i%comm
      myid = lat_i%myid
      npes = lat_i%nx * lat_i%ny

      imgi = lat_i%imglobal
      jmgi = lat_i%jmglobal
      imgo = lat_o%imglobal
      jmgo = lat_o%jmglobal

      allocate ( glo_i(imgi,jmgi,lm) )
      allocate ( glo_o(imgo,jmgo,lm) )

      call timebeg ('   Gather')
      do L=1,lm
      call gather_2d ( glo_i(1,1,L),qin(1,1,L),lat_i )
      enddo
      call timeend ('   Gather')
#ifdef mpi
      call mpi_bcast ( glo_i,imgi*jmgi*lm,lat_i%mpi_rkind,0,comm,ierror )
#endif

      do L=1,lm
      index(L) = mod(L-1,npes)
      enddo

      do L=1,lm
c Parse Arbitray Field (im,jm) to 5'x5' Variable
c ----------------------------------------------
      call timebeg ('  bin_q')
      if( index(L).eq.myid ) call bin_q ( glo_i(1,1,L),imgi,jmgi,qbin,imax,jmax )
      call timeend ('  bin_q')
 
c Bin 10'x10' Variable to Output Field (im_out,jm_out)
c ----------------------------------------------------
      call timebeg ('  ave_q')
      if( index(L).eq.myid ) call ave_q ( qbin,imax,jmax,glo_o(1,1,L),imgo,jmgo,undef,msgn )
      call timeend ('  ave_q')
      enddo

#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( glo_o(1,1,L),imgo*jmgo,lat_o%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif
      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo_o(1,1,L),qout(1,1,L),lat_o )
      enddo
      call timeend ('   Scatter')

      deallocate ( glo_i )
      deallocate ( glo_o )

      call timeend (' bin')

      return
      end

      subroutine ave_q ( qbin,imax,jmax,q,im,jm,undef,msgn )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Average a (10m X 10m) input array to an output array (im,jm)
C
C  INPUT:
C  ======
C    qbin ....... Input array (imax,jmax)
C    msgn ....... Integer Flag for scalar (0) or vector (1)
C
C  OUTPUT:
C  =======
C    q .......... Output array (im,jm)
C    im ......... Longitudinal dimension of q
C    jm ......... Latitudinal  dimension of q
C
C  NOTES:
C  ======
C    Input array qbin represents values within a 5min X 5min grid-box.
C             Each box is referenced by the latitude and longitude of
C             its southwest corner, not its center point.  Thus,
C             the quantity associated with a coordinate actually
C             represents the quantity centered to the northeast of that point.
C
C    Output array q(im,jm) is assumed to be on an A-grid.
C                 q(i,j)   represents the value at the center of the grid-box.
C                 q(1,j)   is located at lon=-180.
C                 q(i,1)   is located at lat=-90.
C                 q(i,jm)  is located at lat=+90.
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer im,jm,msgn
      real  q(im,jm)
      real  dlam(im), dphi(jm)

      integer     imax
      integer     jmax
      real qbin ( imax,jmax )

      integer i,j,ibeg,iend,jbeg,jend
      integer ii,jj,itmp
      real    sum1,sum2
      real    zlat,zlon
      real    lon1,lon2,wx
      real    lat1,lat2,wy
      real    lonbeg,lonend,lat,coslat
      real    latbeg,latend
      real    undef 
      real    pi,dz 
      real    lon_cmp(im)
      real    lat_cmp(jm)
      logical defined

      pi   = 4.*atan(1.0)
      dlam = 2*pi/ im
      dphi =   pi/(jm-1)
      dz   =   pi/(jmax)

c Compute Computational Lambda's and Phi's
c ----------------------------------------
      lon_cmp(1) = -pi
      do i=2,im
      lon_cmp(i) = lon_cmp(i-1) + dlam(i-1)
      enddo
      lat_cmp(1) = -pi*0.5
      do j=2,jm-1
      lat_cmp(j) = lat_cmp(j-1) + dphi(j-1)
      enddo
      lat_cmp(jm) =  pi*0.5


c Compute average away from poles
c -------------------------------
      do j=2,jm-1
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat-dphi(j-1)/2
      latend = zlat+dphi(j)  /2
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1.+(latbeg+pi/2)/dz
      jend = 1.+(latend+pi/2)/dz

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,imax
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         endif

      enddo
      q(i,j) = sum1/sum2
      enddo
      enddo

c Compute average at South Pole
c -----------------------------
         j=1
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat
      latend = zlat+dphi(j)  /2
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1
      jend = 1.+(latend+pi/2)/dz

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,imax
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         endif

      enddo
      q(i,j) = sum1/sum2
      enddo

c Compute average at North Pole
c -----------------------------
         j=jm
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat-dphi(j-1)/2
      latend = zlat
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1.+(latbeg+pi/2)/dz
      jend = jmax

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,imax
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(qbin(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + qbin(ii,jj)*coslat*wx*wy
           sum2 = sum2 +             coslat*wx*wy
           endif
           enddo
         endif

      enddo
      q(i,j) = sum1/sum2
      enddo

c Average Pole Values
c -------------------
      if( msgn.eq.0 ) then
      sum1 = 0
         j = 0
      do i=1,im
         if( defined(q(i,1),undef) ) then
             sum1 = sum1 + q(i,1)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      q(:,1) = sum1/j
      else
      q(:,1) = undef
      endif

      sum2 = 0
         j = 0
      do i=1,im
         if( defined(q(i,jm),undef) ) then
             sum2 = sum2 + q(i,jm)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      q(:,jm) = sum2/j
      else
      q(:,jm) = undef
      endif

      endif

      return
      end
      subroutine bin_q ( q,im,jm,qbin,imax,jmax )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute a 5min X 5min binned array from an input array q(im,jm)
C
C  INPUT:
C  ======
C    q .......... Input array(im,jm)
C    im ......... Longitudinal dimension of q
C    jm ......... Latitudinal  dimension of q
C
C  OUTPUT:
C  =======
C    qbin ....... Output array (imax,jmax)
C
C  NOTES:
C  ======
C    Input array q(im,jm) is assumed to be on an A-grid.
C                q(i,j)   represents the value at the center of the grid-box.
C                q(1,j)   is located at lon=-180.
C                q(i,1)   is located at lat=-90.
C                q(i,jm)  is located at lat=+90.
C
C    Output array qbin represents values within a 5min X 5min grid-box.
C             Each box is referenced by the latitude and longitude of
C             its southwest corner, not its center point.  Thus,
C             the quantity associated with a coordinate actually
C             represents the quantity centered to the northeast of that point.
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer im,jm
      real  q(im,jm)

      integer     imax
      integer     jmax
      real qbin ( imax,jmax )

      integer i,j,ii,jj,ibeg,iend,jbeg,jend
      real    zlatc,zlonc
      real    lonbeg,lonend
      real    latbeg,latend
      real    pi,dl,dp,dz 

      pi = 4.*atan(1.0)
      dl = 2*pi/im
      dp = pi/(jm-1)
      dz = pi/(jmax)
      
      do j=1,jmax
      do i=1,imax

      zlatc = -pi/2+(j-0.5)*dz  ! Latitude  at center of bin box
      zlonc = -pi  +(i-0.5)*dz  ! Longitude at center of bin box

c Find bounding lat and lon on IMxJM grid
c ---------------------------------------
      iend   = nint( 1.+(zlonc+pi)/dl )
      lonend = -pi + (iend-1)*dl
      if( lonend.ge.zlonc ) then
          lonbeg = -pi + (iend-2)*dl
      else
          iend   = iend+1
          lonbeg = lonend
          lonend = -pi + (iend-1)*dl
      endif
      ibeg = iend-1

      jend   = nint( 1.+(zlatc+pi/2)/dp )
      latend = -pi/2 + (jend-1)*dp
      if( latend.ge.zlatc ) then
          latbeg = -pi/2 + (jend-2)*dp
      else
          jend   = jend+1
          latbeg = latend
          latend = -pi/2 + (jend-1)*dp
      endif
      jbeg = jend-1


      if(iend.gt.im) iend=iend-im

      if( zlonc.le.lonbeg+0.5*dl ) then
          ii = ibeg
      else
          ii = iend
      endif
      if( zlatc.le.latbeg+0.5*dp ) then
          jj = jbeg
      else
          jj = jend
      endif

      qbin(i,j) = q(ii,jj)
      
      enddo
      enddo

      return
      end

      subroutine hinterp ( qin,iin,jin,qout,iout,jout,mlev,undef,lat_i,lat_o,flag )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lat_i
      type ( dynamics_lattice_type ) lat_o
#ifdef mpi
      include 'mpif.h'
#endif
      integer  comm,myid,npes

      integer    iin,jin,       iout,jout, mlev, flag
      real   qin(iin,jin,mlev), qout(iout,jout,mlev)
      real undef,pi,dlin,dpin,dlout,dpout,lon,lat
      integer i,j,L,loc
      integer index(mlev),ierror
      integer imgi,jmgi,imgo,jmgo

      real, allocatable :: glo_i(:,:,:)
      real, allocatable :: glo_o(:,:,:)
      real, allocatable ::  lons(:), dlam(:)
      real, allocatable ::  lats(:), dphi(:)

      call timebeg (' hinterp')

      comm = lat_i%comm
      myid = lat_i%myid
      npes = lat_i%nx * lat_i%ny

      imgi = lat_i%imglobal
      jmgi = lat_i%jmglobal
      imgo = lat_o%imglobal
      jmgo = lat_o%jmglobal

      allocate ( glo_i(imgi,jmgi,mlev) )
      allocate ( glo_o(imgo,jmgo,mlev) )
      allocate (  lons(imgo*jmgo) )
      allocate (  lats(imgo*jmgo) )
      allocate (  dlam(imgi) )
      allocate (  dphi(jmgi) )

      do L=1,mlev
      index(L) = mod(L-1,npes)
      enddo

      pi = 4.0*atan(1.0)
      dlin = 2*pi/ imgi
      dpin =   pi/(jmgi-1)
      dlam(:) = dlin
      dphi(:) = dpin

      dlout = 2*pi/ imgo
      dpout =   pi/(jmgo-1)
      
      loc = 0
      do j=1,jmgo
      do i=1,imgo
      loc = loc + 1
      lon = -pi + (i-1)*dlout
      lons(loc) = lon
      enddo
      enddo

      loc = 0
      do j=1,jmgo
      lat = -pi/2.0 + (j-1)*dpout
      do i=1,imgo
      loc = loc + 1
      lats(loc) = lat
      enddo
      enddo

      call timebeg ('   Gather')
      do L=1,mlev
      call gather_2d ( glo_i(1,1,L),qin(1,1,L),lat_i )
      enddo
      call timeend ('   Gather')
#ifdef mpi
      call mpi_bcast ( glo_i,imgi*jmgi*mlev,lat_i%mpi_rkind,0,comm,ierror )
#endif

      do L=1,mlev
      if( index(L).eq.myid ) then
          call interp_h ( glo_i(1,1,L),imgi,jmgi,1,dlam,dphi,
     .                    glo_o(1,1,L),imgo*jmgo,lons,lats,undef,flag )
      endif
      enddo

#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,mlev
      call mpi_bcast ( glo_o(1,1,L),imgo*jmgo,lat_o%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif
      call timebeg ('   Scatter')
      do L=1,mlev
      call scatter_2d ( glo_o(1,1,L),qout(1,1,L),lat_o )
      enddo
      call timeend ('   Scatter')

      deallocate ( glo_i )
      deallocate ( glo_o )
      deallocate (  lons )
      deallocate (  lats )
      deallocate (  dlam )
      deallocate (  dphi )

      call timeend (' hinterp')
      return
      end

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*undef
      return
      end

      subroutine interp_h ( q_cmp,im,jm,lm,dlam,dphi,
     .                      q_geo,irun,lon_geo,lat_geo,undef,flag )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Performs a horizontal interpolation from a field on a computational grid
C    to arbitrary locations.
C
C  INPUT:
C  ======
C    q_cmp ...... Field q_cmp(im,jm,lm) on the computational grid
C    im ......... Longitudinal dimension of q_cmp
C    jm ......... Latitudinal  dimension of q_cmp
C    lm ......... Vertical     dimension of q_cmp
C    dlam ....... Computational Grid Delta Lambda
C    dphi ....... Computational Grid Delta Phi
C    irun ....... Number of Output Locations
C    lon_geo .... Longitude Location of Output
C    lat_geo .... Latitude  Location of Output
C    flag ....... Integer Flag to force Linear (Positive-Definite) Interpolation
C
C  OUTPUT:
C  =======
C    q_geo ...... Field q_geo(irun,lm) at arbitrary locations
C
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer im,jm,lm,irun,flag

      real      q_geo(irun,lm)
      real    lon_geo(irun)
      real    lat_geo(irun)

      real    q_cmp(im,jm,lm)
      real     dlam(im)
      real     dphi(jm)

c Local Variables
c ---------------
      integer  i,j,l
      integer, allocatable       :: ip1(:), ip0(:), im1(:), im2(:)
      integer, allocatable       :: jp1(:), jp0(:), jm1(:), jm2(:)

c Bi-Linear Weights
c -----------------
      real, allocatable       ::    wl_ip0jp0 (:)
      real, allocatable       ::    wl_im1jp0 (:)
      real, allocatable       ::    wl_ip0jm1 (:)
      real, allocatable       ::    wl_im1jm1 (:)

c Bi-Cubic Weights
c ----------------
      real, allocatable       ::    wc_ip1jp1 (:)
      real, allocatable       ::    wc_ip0jp1 (:)
      real, allocatable       ::    wc_im1jp1 (:)
      real, allocatable       ::    wc_im2jp1 (:)
      real, allocatable       ::    wc_ip1jp0 (:)
      real, allocatable       ::    wc_ip0jp0 (:)
      real, allocatable       ::    wc_im1jp0 (:)
      real, allocatable       ::    wc_im2jp0 (:)
      real, allocatable       ::    wc_ip1jm1 (:)
      real, allocatable       ::    wc_ip0jm1 (:)
      real, allocatable       ::    wc_im1jm1 (:)
      real, allocatable       ::    wc_im2jm1 (:)
      real, allocatable       ::    wc_ip1jm2 (:)
      real, allocatable       ::    wc_ip0jm2 (:)
      real, allocatable       ::    wc_im1jm2 (:)
      real, allocatable       ::    wc_im2jm2 (:)

      real    ap1, ap0, am1, am2
      real    bp1, bp0, bm1, bm2

      real    lon_cmp(im)
      real    lat_cmp(jm)
      real    q_tmp(irun)

      real    pi,d
      real    lam,lam_ip1,lam_ip0,lam_im1,lam_im2
      real    phi,phi_jp1,phi_jp0,phi_jm1,phi_jm2
      real    dl,dp
      real    lam_cmp
      real    phi_cmp
      real    undef
      integer im1_cmp,icmp
      integer jm1_cmp,jcmp

c Initialization
c --------------
      pi = 4.*atan(1.)
      dl = 2*pi/ im     ! Uniform Grid Delta Lambda
      dp =   pi/(jm-1)  ! Uniform Grid Delta Phi

c Allocate Memory for Weights and Index Locations
c -----------------------------------------------
      allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
      allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
      allocate ( wc_ip1jp1(irun) , wc_ip0jp1(irun) , wc_im1jp1(irun) , wc_im2jp1(irun) )
      allocate ( wc_ip1jp0(irun) , wc_ip0jp0(irun) , wc_im1jp0(irun) , wc_im2jp0(irun) )
      allocate ( wc_ip1jm1(irun) , wc_ip0jm1(irun) , wc_im1jm1(irun) , wc_im2jm1(irun) )
      allocate ( wc_ip1jm2(irun) , wc_ip0jm2(irun) , wc_im1jm2(irun) , wc_im2jm2(irun) )
      allocate (       ip1(irun) ,       ip0(irun) ,       im1(irun) ,       im2(irun) )
      allocate (       jp1(irun) ,       jp0(irun) ,       jm1(irun) ,       jm2(irun) )

c Compute Input Computational-Grid Latitude and Longitude Locations
c -----------------------------------------------------------------
      lon_cmp(1) = -pi
      do i=2,im
      lon_cmp(i) = lon_cmp(i-1) + dlam(i-1)
      enddo
      lat_cmp(1) = -pi*0.5
      do j=2,jm-1
      lat_cmp(j) = lat_cmp(j-1) + dphi(j-1)
      enddo
      lat_cmp(jm) =  pi*0.5

c Compute Weights for Computational to Geophysical Grid Interpolation
c -------------------------------------------------------------------
      do i=1,irun
      lam_cmp = lon_geo(i)
      phi_cmp = lat_geo(i)

c Determine Indexing Based on Computational Grid
c ----------------------------------------------
      im1_cmp = 1
      do icmp = 2,im
      if( lon_cmp(icmp).lt.lam_cmp ) im1_cmp = icmp
      enddo
      jm1_cmp = 1
      do jcmp = 2,jm
      if( lat_cmp(jcmp).lt.phi_cmp ) jm1_cmp = jcmp
      enddo

      im1(i) = im1_cmp
      ip0(i) = im1(i) + 1
      ip1(i) = ip0(i) + 1
      im2(i) = im1(i) - 1

      jm1(i) = jm1_cmp
      jp0(i) = jm1(i) + 1
      jp1(i) = jp0(i) + 1
      jm2(i) = jm1(i) - 1

c Fix Longitude Index Boundaries
c ------------------------------
      if(im1(i).eq.im) then
      ip0(i) = 1
      ip1(i) = 2
      endif
      if(im1(i).eq.1) then
      im2(i) = im
      endif
      if(ip0(i).eq.im) then
      ip1(i) = 1
      endif


c Compute Immediate Surrounding Coordinates
c -----------------------------------------
      lam     =  lam_cmp
      phi     =  phi_cmp

c Compute and Adjust Longitude Weights
c ------------------------------------
      lam_im2 =  lon_cmp(im2(i))
      lam_im1 =  lon_cmp(im1(i))
      lam_ip0 =  lon_cmp(ip0(i))
      lam_ip1 =  lon_cmp(ip1(i))

      if( lam_im2.gt.lam_im1 ) lam_im2 = lam_im2 - 2*pi
      if( lam_im1.gt.lam_ip0 ) lam_ip0 = lam_ip0 + 2*pi
      if( lam_im1.gt.lam_ip1 ) lam_ip1 = lam_ip1 + 2*pi
      if( lam_ip0.gt.lam_ip1 ) lam_ip1 = lam_ip1 + 2*pi


c Compute and Adjust Latitude Weights   
c Note:  Latitude Index Boundaries are Adjusted during Interpolation
c ------------------------------------------------------------------
      phi_jm2 =  lat_cmp(jm2(i))
      phi_jm1 =  lat_cmp(jm1(i))
      phi_jp0 =  lat_cmp(jp0(i))
      phi_jp1 =  lat_cmp(jp1(i))

      if( jm2(i).eq.0    ) phi_jm2 = phi_jm1 - dphi(1)
      if( jm1(i).eq.jm   ) then
                           phi_jp0 = phi_jm1 + dphi(jm-1)
                           phi_jp1 = phi_jp0 + dphi(jm-2)
      endif
      if( jp1(i).eq.jm+1 ) phi_jp1 = phi_jp0 + dphi(jm-1)


c Bi-Linear Weights
c -----------------
              d    = (lam_ip0-lam_im1)*(phi_jp0-phi_jm1)
      wl_im1jm1(i) = (lam_ip0-lam    )*(phi_jp0-phi    )/d
      wl_ip0jm1(i) = (lam    -lam_im1)*(phi_jp0-phi    )/d
      wl_im1jp0(i) = (lam_ip0-lam    )*(phi    -phi_jm1)/d
      wl_ip0jp0(i) = (lam    -lam_im1)*(phi    -phi_jm1)/d

c Bi-Cubic Weights
c ----------------
      ap1 = ( (lam    -lam_ip0)*(lam    -lam_im1)*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_ip0)*(lam_ip1-lam_im1)*(lam_ip1-lam_im2) )
      ap0 = ( (lam_ip1-lam    )*(lam    -lam_im1)*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_ip0)*(lam_ip0-lam_im1)*(lam_ip0-lam_im2) )
      am1 = ( (lam_ip1-lam    )*(lam_ip0-lam    )*(lam    -lam_im2) )
     .    / ( (lam_ip1-lam_im1)*(lam_ip0-lam_im1)*(lam_im1-lam_im2) )
      am2 = ( (lam_ip1-lam    )*(lam_ip0-lam    )*(lam_im1-lam    ) )
     .    / ( (lam_ip1-lam_im2)*(lam_ip0-lam_im2)*(lam_im1-lam_im2) )

      bp1 = ( (phi    -phi_jp0)*(phi    -phi_jm1)*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jp0)*(phi_jp1-phi_jm1)*(phi_jp1-phi_jm2) )
      bp0 = ( (phi_jp1-phi    )*(phi    -phi_jm1)*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jp0)*(phi_jp0-phi_jm1)*(phi_jp0-phi_jm2) )
      bm1 = ( (phi_jp1-phi    )*(phi_jp0-phi    )*(phi    -phi_jm2) )
     .    / ( (phi_jp1-phi_jm1)*(phi_jp0-phi_jm1)*(phi_jm1-phi_jm2) )
      bm2 = ( (phi_jp1-phi    )*(phi_jp0-phi    )*(phi_jm1-phi    ) )
     .    / ( (phi_jp1-phi_jm2)*(phi_jp0-phi_jm2)*(phi_jm1-phi_jm2) )

      wc_ip1jp1(i) = bp1*ap1
      wc_ip0jp1(i) = bp1*ap0
      wc_im1jp1(i) = bp1*am1
      wc_im2jp1(i) = bp1*am2

      wc_ip1jp0(i) = bp0*ap1
      wc_ip0jp0(i) = bp0*ap0
      wc_im1jp0(i) = bp0*am1
      wc_im2jp0(i) = bp0*am2

      wc_ip1jm1(i) = bm1*ap1
      wc_ip0jm1(i) = bm1*ap0
      wc_im1jm1(i) = bm1*am1
      wc_im2jm1(i) = bm1*am2

      wc_ip1jm2(i) = bm2*ap1
      wc_ip0jm2(i) = bm2*ap0
      wc_im1jm2(i) = bm2*am1
      wc_im2jm2(i) = bm2*am2

      enddo

c Interpolate Computational-Grid Quantities to Geophysical Grid
c -------------------------------------------------------------
      do L=1,lm
      do i=1,irun

      if( lat_geo(i).le.lat_cmp(2)     .or.
     .    lat_geo(i).ge.lat_cmp(jm-1)  .or.  flag.eq.1 ) then

c 1st Order Interpolation at Poles
c --------------------------------
      if( q_cmp( im1(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jp0(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef ) then

      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )

      else
      q_tmp(i) = undef
      endif

      else

c Cubic Interpolation away from Poles
c -----------------------------------
      if( q_cmp( ip1(i),jp0(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jp0(i),L ).ne.undef  .and.
     .    q_cmp( im2(i),jp0(i),L ).ne.undef  .and.

     .    q_cmp( ip1(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jm1(i),L ).ne.undef  .and.
     .    q_cmp( im2(i),jm1(i),L ).ne.undef  .and.

     .    q_cmp( ip1(i),jp1(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jp1(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jp1(i),L ).ne.undef  .and.
     .    q_cmp( im2(i),jp1(i),L ).ne.undef  .and.

     .    q_cmp( ip1(i),jm2(i),L ).ne.undef  .and.
     .    q_cmp( ip0(i),jm2(i),L ).ne.undef  .and.
     .    q_cmp( im1(i),jm2(i),L ).ne.undef  .and.
     .    q_cmp( im2(i),jm2(i),L ).ne.undef ) then

      q_tmp(i) = wc_ip1jp1(i) * q_cmp( ip1(i),jp1(i),L )
     .         + wc_ip0jp1(i) * q_cmp( ip0(i),jp1(i),L )
     .         + wc_im1jp1(i) * q_cmp( im1(i),jp1(i),L )
     .         + wc_im2jp1(i) * q_cmp( im2(i),jp1(i),L )

     .         + wc_ip1jp0(i) * q_cmp( ip1(i),jp0(i),L )
     .         + wc_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
     .         + wc_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wc_im2jp0(i) * q_cmp( im2(i),jp0(i),L )

     .         + wc_ip1jm1(i) * q_cmp( ip1(i),jm1(i),L )
     .         + wc_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wc_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wc_im2jm1(i) * q_cmp( im2(i),jm1(i),L )

     .         + wc_ip1jm2(i) * q_cmp( ip1(i),jm2(i),L )
     .         + wc_ip0jm2(i) * q_cmp( ip0(i),jm2(i),L )
     .         + wc_im1jm2(i) * q_cmp( im1(i),jm2(i),L )
     .         + wc_im2jm2(i) * q_cmp( im2(i),jm2(i),L )

      elseif( q_cmp( im1(i),jm1(i),L ).ne.undef  .and.
     .        q_cmp( ip0(i),jm1(i),L ).ne.undef  .and.
     .        q_cmp( im1(i),jp0(i),L ).ne.undef  .and.
     .        q_cmp( ip0(i),jp0(i),L ).ne.undef ) then

      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )

      else
      q_tmp(i) = undef
      endif

      endif
      enddo

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo

      deallocate ( wl_ip0jp0 , wl_im1jp0 )
      deallocate ( wl_ip0jm1 , wl_im1jm1 )
      deallocate ( wc_ip1jp1 , wc_ip0jp1 , wc_im1jp1 , wc_im2jp1 )
      deallocate ( wc_ip1jp0 , wc_ip0jp0 , wc_im1jp0 , wc_im2jp0 )
      deallocate ( wc_ip1jm1 , wc_ip0jm1 , wc_im1jm1 , wc_im2jm1 )
      deallocate ( wc_ip1jm2 , wc_ip0jm2 , wc_im1jm2 , wc_im2jm2 )
      deallocate (       ip1 ,       ip0 ,       im1 ,       im2 )
      deallocate (       jp1 ,       jp0 ,       jm1 ,       jm2 )

      return
      end

      subroutine remap ( ple,u,v,t_in,q,o3,phis_in,phis_out,ak,bk,im,jm,lm,ihaveth )

C***********************************************************************
C
C  Purpose
C     Driver for remapping fields to new topography
C
C  Argument Description
C     ple ...... model edge pressure
C     u  ....... model zonal      wind
C     v  ....... model meridional wind
C     thv  ..... model virtual potential  temperature
C     q  ....... model specific   humidity
C     o3  ...... model ozone
C     phis_in... model surface geopotential (input)
C     phis_out.. model surface geopotential (output)
C     ak ....... model vertical   dimension
C     bk ....... model vertical   dimension
C
C     im ....... zonal      dimension
C     jm ....... meridional dimension
C     lm ....... meridional dimension
C
C     ihaveth .. when .t., thv array has th not thv
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  im,jm,lm
      logical  ihaveth

c Input variables
c ---------------
      real      ple(im,jm,lm+1)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real     t_in(im,jm,lm)
      real        q(im,jm,lm)
      real       o3(im,jm,lm)
      real phis_in (im,jm)
      real phis_out(im,jm)

      real*8     ak(lm+1)
      real*8     bk(lm+1)

c Local variables
c ---------------
      real  ps     (im,jm)
      real  phi    (im,jm,lm+1)
      real  pke    (im,jm,lm+1)
      real  thv    (im,jm,lm)
      real  ple_out(im,jm,lm+1)
      real  pke_out(im,jm,lm+1)

      real    u_out(im,jm,lm)
      real    v_out(im,jm,lm)
      real  thv_out(im,jm,lm)
      real    q_in (im,jm,lm,2)
      real    q_out(im,jm,lm,2)

      real    kappa,cp,rgas,eps,rvap
      integer i,j,L

      kappa = 2.0/7.0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      cp    = rgas/kappa

c If dry theta instead of virtual
c -------------------------------
      if(ihaveth) then
         thv = t_in*(1+eps*q)
      else
         thv = t_in
      endif

c Construct Input Heights
c -----------------------
      pke(:,:,:) = ple(:,:,:)**kappa 

      phi(:,:,lm+1) = phis_in(:,:)
      do L=lm,1,-1
      phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
      enddo
      
c Compute new surface pressure consistent with output topography
c --------------------------------------------------------------
      do j=1,jm
      do i=1,im
           L = lm
           do while ( phi(i,j,L).lt.phis_out(i,j) )
           L = L-1
           enddo
           ps(i,j) = ple(i,j,L+1)*( 1+(phi(i,j,L+1)-phis_out(i,j))/(cp*thv(i,j,L)*pke(i,j,L+1)) )**(1.0/kappa)
      enddo
      enddo

c Construct fv pressure variables using new surface pressure
c ----------------------------------------------------------
      do L=1,lm+1
      do j=1,jm
      do i=1,im
       ple_out(i,j,L) = ak(L) + bk(L)*ps(i,j)
      enddo
      enddo
      enddo
      pke_out(:,:,:) = ple_out(:,:,:)**kappa 

c Map original fv state onto new eta grid
c ---------------------------------------
      q_in(:,:,:,1) =  q(:,:,:)
      q_in(:,:,:,2) = o3(:,:,:)

      call gmap ( im,jm,2 , kappa,
     .            lm, pke    ,ple    ,u    ,v    ,t_in   ,q_in ,
     .            lm, pke_out,ple_out,u_out,v_out,thv_out,q_out)

      ple(:,:,:) = ple_out(:,:,:)
        u(:,:,:) =   u_out(:,:,:)
        v(:,:,:) =   v_out(:,:,:)
      thv(:,:,:) = thv_out(:,:,:)
        q(:,:,:) =   q_out(:,:,:,1)
       o3(:,:,:) =   q_out(:,:,:,2)

      return
      end
      subroutine write_d ( lu, u,v,ple,im,jm,lm,lattice )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

      integer   im,jm,lm
      real    u(im,jm,lm)
      real    v(im,jm,lm)
      real   dp(im,jm,lm)
      real  div(im,jm,lm)
      real  ple(im,jm,lm+1)

      integer index(lm),ierror

      integer :: lu
      real, allocatable ::  uglo(:,:,:)
      real, allocatable ::  vglo(:,:,:)
      real, allocatable :: dpglo(:,:,:)
      real, allocatable ::  dglo(:,:,:)

      real, allocatable ::   sum1(:,:)
      real*4, allocatable ::  dum(:,:)

      integer L, comm, myid, npes
      integer img, jmg

      img  = lattice%imglobal
      jmg  = lattice%jmglobal
      comm = lattice%comm
      myid = lattice%myid
      npes = lattice%nx * lattice%ny

      do L=1,lm
      index (L) = mod(L-1,npes)
      dp(:,:,L) = ( ple(:,:,L+1)-ple(:,:,L) )
      enddo
 
      allocate (  uglo(img,jmg,lm) )
      allocate (  vglo(img,jmg,lm) )
      allocate (  dglo(img,jmg,lm) )
      allocate ( dpglo(img,jmg,lm) )
      allocate (   dum(img,jmg) )
      allocate (   sum1(im,jm)  )

! Construct A-Grid Winds
! ----------------------
      do L=1,lm
             call gather_2d (  uglo(1,1,L), u(1,1,L),lattice )
             call gather_2d (  vglo(1,1,L), v(1,1,L),lattice )
             call gather_2d ( dpglo(1,1,L),dp(1,1,L),lattice )
         if( lattice%myid.eq.0 ) then
             call dtoa      ( uglo(1,1,L),uglo(1,1,L),img,jmg,1,2 )
             call dtoa      ( vglo(1,1,L),vglo(1,1,L),img,jmg,1,1 )
         endif
      enddo
#ifdef mpi
      call mpi_bcast (  uglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast (  vglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( dpglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
#endif

c Compute Mass-Weighted Divergence
c --------------------------------
      do L=1,lm
      if( index(L).eq.myid ) call getdiv (uglo(1,1,L),vglo(1,1,L),dpglo(1,1,L),dglo(1,1,L),img,jmg )               
      enddo
#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( dglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif
      do L=1,lm
      call scatter_2d ( dglo(1,1,L),div(1,1,L),lattice )
      enddo

c Modify Divergence (to force vanishing vertical integral)
c --------------------------------------------------------
        sum1(:,:) = 0.0
      do L=1,lm
        sum1(:,:) = sum1(:,:) + div(:,:,L)
      enddo

c Gather and Broadcast Divergence
c -------------------------------
      call gather_2d ( dglo(1,1,1),sum1,lattice )

      if( lattice%myid.eq.0 ) then
                    dum(:,:) = dglo(:,:,1)
          write(lu) dum
      endif

      deallocate ( sum1,dum )
      deallocate ( uglo,vglo,dglo,dpglo )
      return
      end
