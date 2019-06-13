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
! ****                                                              ****
! **********************************************************************
! **********************************************************************

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
      integer  npex,npey

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms,nymd_bkg,nhms_bkg
      integer ima,jma,lm
      integer imb,jmb

      integer ntime,nvars,ngatts,timinc
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)

      character*256  dynrst, anaeta, moistrst, topofile
      character*256  tag
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

! ANA and BKG Variables
! ---------------------
      real, allocatable ::   dp_ana(:,:,:)
      real, allocatable ::    u_ana(:,:,:) ,    u_bkg(:,:,:),   u_bkg0(:,:,:)
      real, allocatable ::    v_ana(:,:,:) ,    v_bkg(:,:,:),   v_bkg0(:,:,:)
      real, allocatable ::  thv_ana(:,:,:) ,  thv_bkg(:,:,:)
      real, allocatable ::   tv_ana(:,:,:) ,   th_bkg(:,:,:)
      real, allocatable ::   pk_ana(:,:,:) ,   pk_bkg(:,:,:)
      real, allocatable ::  ple_ana(:,:,:) ,  ple_bkg(:,:,:), ple_bkg0(:,:,:)
      real, allocatable ::  pke_ana(:,:,:) ,  pke_bkg(:,:,:)
      real, allocatable ::    q_ana(:,:,:) ,    q_bkg(:,:,:)
      real, allocatable ::   o3_ana(:,:,:) ,   o3_bkg(:,:,:)
      real, allocatable ::   ps_ana(:,:)
      real, allocatable :: phis_ana(:,:)   , phis_bkg(:,:)
      real, allocatable :: phis_tmp(:,:)

      real,   allocatable ::   glo(:,:)
      real*8, allocatable ::    ak(:)
      real*8, allocatable ::    bk(:)

      real    kappa
      real    undefa,rgas,rvap,eps,grav,cp

      character*256, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,L,ID,rc,method

      logical   agrid_ana
      logical   dgrid_ana
      logical u_agrid_ana
      logical v_agrid_ana
      logical u_dgrid_ana
      logical v_dgrid_ana
      logical  tvflag_ana
      logical thvflag_ana

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
      cp    = rgas/kappa

           tag = 'ana'
        dynrst = 'x'
      moistrst = 'x'
      anaeta   = 'x'
      topofile = 'x'
      nymd     = -999
      nhms     = -999
      method   = -999

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
             if( trim(arg(n)).eq.'-ana'      ) then
                                   anaeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo'     ) then
                                   topofile = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-tag'      ) then
                                   tag     = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-divr'     ) method = 1
             if( trim(arg(n)).eq.'-diva'     ) method = 2
             if( trim(arg(n)).eq.'-nymd'     ) read(arg(n+1),*) nymd
             if( trim(arg(n)).eq.'-nhms'     ) read(arg(n+1),*) nhms
      enddo

      if( trim(anaeta)  .eq.'x' .or. 
     .    trim(dynrst)  .eq.'x' .or. 
     .    trim(moistrst).eq.'x' ) then
          if( myid.eq.0 ) print *, 'You must supply DYNRST, MOISTRST, and ANARST Files!'
          call my_finalize
          call exit (7)
          stop
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

      allocate (   u_bkg0(imb,jmb,lm)   )
      allocate (   v_bkg0(imb,jmb,lm)   )
      allocate ( ple_bkg0(imb,jmb,lm+1) )

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
      call read_fv ( ple_bkg0,imb,jmb,lm  ,10,lattice_bkg ) ! Skip theta
      call read_fv ( ple_bkg0,imb,jmb,lm+1,10,lattice_bkg )

      if( myid.eq.0 ) close(10)

! **********************************************************************
! ****                   Read Analysis ANA File                     ****
! **********************************************************************

      call timebeg(' read_ana')
      if( myid.eq.0 ) then
          print *, 'Reading  ANA File from PE: ',myid
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
          agrid_ana = .false.
          dgrid_ana = .false.
        u_agrid_ana = .false.
        v_agrid_ana = .false.
        u_dgrid_ana = .false.
        v_dgrid_ana = .false.
         tvflag_ana = .false.
        thvflag_ana = .false.
          call gfio_inquire ( id,imaglobal,jmaglobal,lm,ntime,nvars,
     .                        title,source,contact,undefa,
     .                        lon,lat,lev,levunits,
     .                        yymmdd,hhmmss,timinc,
     .                        vname,vtitle,vunits,kmvar,
     .                        vrange,prange,rc )
         do n=1,nvars
         if( trim(vname(n)).eq.'u'     ) u_agrid_ana = .true.
         if( trim(vname(n)).eq.'v'     ) v_agrid_ana = .true.
         if( trim(vname(n)).eq.'uwnd'  ) u_dgrid_ana = .true.
         if( trim(vname(n)).eq.'vwnd'  ) v_dgrid_ana = .true.
         if( trim(vname(n)).eq.'tv'    )  tvflag_ana = .true.
         if( trim(vname(n)).eq.'theta' ) thvflag_ana = .true.
         enddo
         agrid_ana = u_agrid_ana .and. v_agrid_ana
         dgrid_ana = u_dgrid_ana .and. v_dgrid_ana
      endif

#ifdef mpi
      call mpi_bcast (   agrid_ana,1,          mpi_logical,0,comm,ierror )
      call mpi_bcast (   dgrid_ana,1,          mpi_logical,0,comm,ierror )
      call mpi_bcast (  tvflag_ana,1,          mpi_logical,0,comm,ierror )
      call mpi_bcast ( thvflag_ana,1,          mpi_logical,0,comm,ierror )

      call mpi_bcast ( undefa,     1,lattice_ana%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( lon,imaglobal,lattice_ana%mpi_rkind,0,comm,ierror )
#endif

      allocate (   ps_ana(ima,jma)      )
      allocate (   dp_ana(ima,jma,lm)   )
      allocate (   pk_ana(ima,jma,lm)   )
      allocate (  pke_ana(ima,jma,lm+1) )
      allocate (  thv_ana(ima,jma,lm)   )

      allocate ( phis_ana(ima,jma)      )
      allocate (    u_ana(ima,jma,lm)   )
      allocate (    v_ana(ima,jma,lm)   )
      allocate (    q_ana(ima,jma,lm)   )
      allocate (   o3_ana(ima,jma,lm)   )
      allocate (  ple_ana(ima,jma,lm+1) )

      call getit ( id,'phis' ,nymd,nhms,ima,jma,0,1 ,phis_ana,lattice_ana )
      call getit ( id,'ps'   ,nymd,nhms,ima,jma,0,1 ,  ps_ana,lattice_ana )
      call getit ( id,'delp' ,nymd,nhms,ima,jma,1,lm,  dp_ana,lattice_ana )
      call getit ( id,'sphu' ,nymd,nhms,ima,jma,1,lm,   q_ana,lattice_ana )
      call getit ( id,'ozone',nymd,nhms,ima,jma,1,lm,  o3_ana,lattice_ana )

      if( agrid_ana ) then
          call getit ( id,'u'   ,nymd,nhms,ima,jma,1,lm,u_ana,lattice_ana )
          call getit ( id,'v'   ,nymd,nhms,ima,jma,1,lm,v_ana,lattice_ana )
      else if( dgrid_ana ) then
          call getit ( id,'uwnd',nymd,nhms,ima,jma,1,lm,u_ana,lattice_ana )
          call getit ( id,'vwnd',nymd,nhms,ima,jma,1,lm,v_ana,lattice_ana )
      endif

      if( thvflag_ana ) then
          call getit ( id,'theta',nymd,nhms,ima,jma,1,lm,thv_ana,lattice_ana )
      else if( tvflag_ana ) then
          allocate   ( tv_ana(ima,jma,lm) )
          call getit ( id,'tv'   ,nymd,nhms,ima,jma,1,lm, tv_ana,lattice_ana )
      endif

      if( myid.eq.0 ) call gfio_close ( id,rc )
      call timeend(' read_ana')

      ple_ana(:,:,1) = ak(1)
      do L=2,lm+1
      ple_ana(:,:,L) = ple_ana(:,:,L-1)+dp_ana(:,:,L-1)
      enddo

      if( tvflag_ana .and. .not.thvflag_ana ) then
          pke_ana(:,:,:) = ple_ana(:,:,:)**kappa
          do L=1,lm
          pk_ana(:,:,L) = ( pke_ana(:,:,L+1)-pke_ana(:,:,L) )
     .                  / ( kappa*log(ple_ana(:,:,L+1)/ple_ana(:,:,L)) )
          enddo
          thv_ana =  tv_ana/pk_ana
          deallocate (  tv_ana )
      endif


! Check for Indexing Consistency with Model
! -----------------------------------------
      if( myid.eq.0 ) print *, 'Analysis ANA File begins at Lon: ',lon(1)
      if( lon(1) .eq. 0.0 ) then
          if( myid.eq.0 ) print *, '         Flipping Horizontal Coordinate' 
          call hflip (phis_ana,ima,jma,1   ,lattice_ana )
          call hflip (   u_ana,ima,jma,lm  ,lattice_ana )
          call hflip (   v_ana,ima,jma,lm  ,lattice_ana )
          call hflip ( thv_ana,ima,jma,lm  ,lattice_ana )
          call hflip ( ple_ana,ima,jma,lm+1,lattice_ana )
          call hflip (   q_ana,ima,jma,lm  ,lattice_ana )
          call hflip (  o3_ana,ima,jma,lm  ,lattice_ana )
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

      if( myid.eq.0 ) then
          print *
          print *, '    FV Restart filename: ',trim(dynrst)
          print *, '          FV resolution: ',imbglobal,jmbglobal,lm
          print *
          print *, '      Analysis filename: ',trim(anaeta)
          print *, '         ANA resolution: ',imaglobal,jmaglobal,lm
          print *
          print *, '                   Date: ',nymd,nhms
          print *, '                    Tag: ',trim(tag)
          print *
      endif

! **********************************************************************
! ****                  Create FV Restart Using ANA Data            ****
! **********************************************************************

      if( imaglobal.ne.imbglobal  .or. 
     .    jmaglobal.ne.jmbglobal  .or.
     .    trim(topofile).ne.'x' ) then

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

! Construct A-Grid Winds
! ----------------------
      if( dgrid_ana .and. .not.agrid_ana ) then
          allocate ( glo(imaglobal,jmaglobal) )
          do L=1,lm
                                          call timebeg ('   Gather')
                                          call gather_2d  ( glo,u_ana(1,1,L),lattice_ana )
                                          call timeend ('   Gather')
                                          call timebeg (' dtoa')
              if( lattice_ana%myid.eq.0 ) call dtoa       ( glo,glo,imaglobal,jmaglobal,1,2 )
                                          call timeend (' dtoa')
                                          call timebeg ('   Scatter')
                                          call scatter_2d ( glo,u_ana(1,1,L),lattice_ana )
                                          call timeend ('   Scatter')
                                          call timebeg ('   Gather')
                                          call gather_2d  ( glo,v_ana(1,1,L),lattice_ana )
                                          call timeend ('   Gather')
                                          call timebeg (' dtoa')
              if( lattice_ana%myid.eq.0 ) call dtoa       ( glo,glo,imaglobal,jmaglobal,1,1 )
                                          call timeend (' dtoa')
                                          call timebeg ('   Scatter')
                                          call scatter_2d ( glo,v_ana(1,1,L),lattice_ana )
                                          call timeend ('   Scatter')
          enddo
          deallocate ( glo )
      endif

! Interpolate ANA Data to BKG Resolution if resolutions differ
! ------------------------------------------------------------
      if( imaglobal.ne.imbglobal ) then
          if( myid.eq.0 ) print *, 'Interpolating ANA Data to BKG Resolution'
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
#if 0
      if( imaglobal.gt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Binning       ANA Data to BKG Resolution'
          call bin ( phis_ana,ima,jma,phis_tmp,imb,jmb,1   ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (    u_ana,ima,jma,   u_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (    v_ana,ima,jma,   v_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (  thv_ana,ima,jma, thv_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (  ple_ana,ima,jma, ple_bkg,imb,jmb,lm+1,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (    q_ana,ima,jma,   q_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (   o3_ana,ima,jma,  o3_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
      endif
#endif

! BKG and ANA Horizontal Resolutions Match
! ----------------------------------------
      if( imaglobal.eq.imbglobal .and. jmaglobal.eq.jmbglobal ) then
          phis_tmp = phis_ana
             u_bkg =    u_ana
             v_bkg =    v_ana
           thv_bkg =  thv_ana
           ple_bkg =  ple_ana
             q_bkg =    q_ana
            o3_bkg =   o3_ana
      endif

! Remap Based on TOPO Differences
! -------------------------------
      if( myid.eq.0 ) print *, 'Remapping Data to BKG Topography'
      call timebeg(' remap')
      call remap ( ple_bkg,
     .               u_bkg,
     .               v_bkg,
     .             thv_bkg,
     .               q_bkg,
     .              o3_bkg,
     .            phis_tmp,phis_bkg,ak,bk,imb,jmb,lm )
      call timeend(' remap')

! Construct D-Grid Winds
! ----------------------
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

      else

! BKG and ANA Horizontal Resolutions Match
! ----------------------------------------

        if( agrid_ana ) then

          ! Construct D-Grid Analysis Winds from A-Grid ANA
          ! -----------------------------------------------
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
        endif

           u_bkg =   u_ana
           v_bkg =   v_ana
         thv_bkg = thv_ana
         ple_bkg = ple_ana
           q_bkg =   q_ana
          o3_bkg =  o3_ana

      endif

      deallocate (   u_ana )
      deallocate (   v_ana )
      deallocate ( thv_ana )
      deallocate ( ple_ana )
      deallocate (   q_ana )
      deallocate (  o3_ana )

! Construct PK Model Variable
! ---------------------------
      pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa

      do L=1,lm
       pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .               / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
      enddo

! Construct Dry Potential Temperature
! -----------------------------------
      th_bkg = thv_bkg/(1+eps*q_bkg)

! Modify vertically integrated wind increment to be non-divergent
! ---------------------------------------------------------------

      call write_d ( u_bkg0,v_bkg0,ple_bkg0,imb,jmb,lm,lattice_bkg )  ! Background
      call write_d ( u_bkg ,v_bkg ,ple_bkg ,imb,jmb,lm,lattice_bkg )  ! Analysis before Adjustment

      if( method.eq.-999 ) then
          if( myid.eq.0 )       print *, 'No Wind Adjustment  Change to Divergence'
      else
          if( myid.eq.0 ) then
              if( method.eq.1 ) print *, 'Minimizing Relative Change to Divergence'
              if( method.eq.2 ) print *, 'Minimizing Absolute Change to Divergence'
                                print *, 'Calling Windfix'
          endif
          call timebeg (' windfix')
          call windfix ( u_bkg ,v_bkg ,ple_bkg ,
     .                   u_bkg0,v_bkg0,ple_bkg0,imb,jmb,lm,lattice_bkg,'D',method )
          call timeend (' windfix')
      endif

      call write_d ( u_bkg,v_bkg,ple_bkg,imb,jmb,lm,lattice_bkg )  ! Analysis after Adjustment


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

      call write_fv (   u_bkg,imb,jmb,lm  ,20,lattice_bkg )
      call write_fv (   v_bkg,imb,jmb,lm  ,20,lattice_bkg )
      call write_fv (  th_bkg,imb,jmb,lm  ,20,lattice_bkg )
      call write_fv ( ple_bkg,imb,jmb,lm+1,20,lattice_bkg )
      call write_fv (  pk_bkg,imb,jmb,lm  ,20,lattice_bkg )

      if( myid.eq.0 ) close (20)
      call timeend(' writefv')

! **********************************************************************
! ****     Merge Moist Internal Restart with Analysis ANA Data      ****
! **********************************************************************

      anaeta = trim(moistrst) // '.' // trim(tag) // '.' // date // '_' // hour // 'z'

      call timebeg(' writemois')
      if( myid.eq.0 ) then
          print *, 'Creating GEOS-5  moist_internal_restart: ',trim(anaeta)
          print *
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
      type ( dynamics_lattice_type ) lattice
      integer L,id,nymd,nhms,im,jm,img,jmg,lbeg,lm
      real   q(im,jm,lm)
      real,allocatable :: glo(:,:,:)
      character(*) name
      integer rc
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg,lm) )
      if( lattice%myid.eq.0 ) then
          call gfio_getvar ( id,trim(name),nymd,nhms,img,jmg,lbeg,lm,glo,rc )
          if( rc.ne.0 ) print *, '!! Error Reading: ',trim(name),'  RC = ',rc
      endif
      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo(1,1,L),q(1,1,L),lattice )
      enddo
      call timeend ('   Scatter')
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
 
      subroutine write_fv ( q,im,jm,lm,ku,lattice )
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
         call timebeg ('   Gather')
         call gather_2d ( glo4,q(1,1,L),lattice )
         call timeend ('   Gather')
         if( lattice%myid.eq.0 ) then
                       glo8 = glo4
             write(ku) glo8
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
         print *, "                 -ana        Analysis-Style ana.eta file   "
         print *, "                 -nymd       (Date  to Read From ANA.ETA File (YYYYMMDD),"
         print *, "                              Default: Date within DYNRST)"
         print *, "                 -nhms       (Time  to Read From ANA.ETA File (HHMMSS),"
         print *, "                              Default: Time within DYNRST)"
         print *, "                 -topo       (Optional Topography File if Different from ANA) "
         print *, "                 -divr       (Optional flag to Minimize Relative Adjustment of"
         print *, "                              Div.Inc.)"
         print *, "                 -diva       (Optional flag to Minimize Absolute Adjustment of"
         print *, "                              Div.Inc.)"
         print *
         print *
      endif
#ifdef mpi
      call mpi_finalize (ierror)
#endif
      stop
      end

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

      subroutine remap ( ple,u,v,thv,q,o3,phis_in,phis_out,ak,bk,im,jm,lm )

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
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  im,jm,lm

c Input variables
c ---------------
      real      ple(im,jm,lm+1)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real      thv(im,jm,lm)
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
     .            lm, pke    ,ple    ,u    ,v    ,thv    ,q_in ,
     .            lm, pke_out,ple_out,u_out,v_out,thv_out,q_out)

      ple(:,:,:) = ple_out(:,:,:)
        u(:,:,:) =   u_out(:,:,:)
        v(:,:,:) =   v_out(:,:,:)
      thv(:,:,:) = thv_out(:,:,:)
        q(:,:,:) =   q_out(:,:,:,1)
       o3(:,:,:) =   q_out(:,:,:,2)

      return
      end
      subroutine write_d ( u,v,ple,im,jm,lm,lattice )
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
          write(33) dum
      endif

      deallocate ( sum1,dum )
      deallocate ( uglo,vglo,dglo,dpglo )
      return
      end
