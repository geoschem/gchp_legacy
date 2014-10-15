      program  main
!#define debug

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to Create IAU from ANA.ETA and BKG.ETA Files     ****
! ****                                                              ****
! **** Note:  BKG files are interpolated/binned to the ANA          ****
! ****        resolution.  The resulting IAU forcing is then        ****
! ****        binned/interpolated back to the Model resolution.     ****
! ****                                                              ****
! **** !REVISION HISTORY:                                           ****
! ****  02Apr2006  Todling  dummy arrays to r*8 (gfio-r8)           ****
! ****  03Apr2006  Todling  added O3 increment                      ****
! ****  21Jun2006  Takacs   Added BKG and ANA of different          ****
! ****                      resolutions, implemented MPI            ****
! ****  21Apr2007  Todling  Removed delp; bypass dtoa; thv to tv    ****
! ****  03Jun2008  Todling  Removed dtoa for real                   ****
! ****  01Jul2008  Daescu   Add -scale opt for obs impact           ****
! ****  04Mar2009  RT/Nadeau - Char length fom 128 to 256           ****
! ****                       - hdf suffix to nc4                    ****
! ****  12Mar2009  Takacs/RT - Merged FDDA w/ 530; interp ainc-out  ****
! ****  01Apr2009  Takacs   Made Backward Compatable for DGrid case ****
! ****  12Jun2012  Todling/Akella - add DTSDT increment             ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice_ana
      type ( dynamics_lattice_type ) lattice_bkg
      type ( dynamics_lattice_type ) lattice_out
      include 'mpif.h'
      integer  comm,myid,npes,ierror

      integer  imaglobal,jmaglobal
      integer  imbglobal,jmbglobal
      integer  imoglobal,jmoglobal
      integer  npex,npey

      integer nymd,nhms
      integer ima,jma,lm
      integer imb,jmb
      integer imo,jmo

      real   sclinc    ! variable for parametric increment (obs impact)

      integer ntime,nvars,ngatts,timinc
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable ::   nloc(:)

      character*80   bkgeta, anaeta, iaueta, biasin, biasout
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

! BIAS Variables
! --------------
      real, allocatable ::  psb_ana(:,:)   , psb_bkg(:,:)  
      real, allocatable ::  tsb_ana(:,:)   , tsb_bkg(:,:)
      real, allocatable ::   ub_ana(:,:,:) ,  ub_bkg(:,:,:)
      real, allocatable ::   vb_ana(:,:,:) ,  vb_bkg(:,:,:)
      real, allocatable ::   tb_ana(:,:,:) ,  tb_bkg(:,:,:)
      real, allocatable ::   qb_ana(:,:,:) ,  qb_bkg(:,:,:)
      real, allocatable ::   pb_ana(:,:,:) ,  pb_bkg(:,:,:)
      real, allocatable ::  o3b_ana(:,:,:) , o3b_bkg(:,:,:)

! ANA and BKG Variables
! ---------------------
      real, allocatable ::    u_ana(:,:,:) ,   u_bkg(:,:,:)
      real, allocatable ::    v_ana(:,:,:) ,   v_bkg(:,:,:)
      real, allocatable ::                     t_bkg(:,:,:)
      real, allocatable ::   tv_ana(:,:,:) ,  tv_bkg(:,:,:)
      real, allocatable ::  thv_ana(:,:,:) , thv_bkg(:,:,:)
      real, allocatable ::   pk_ana(:,:,:) ,  pk_bkg(:,:,:)
      real, allocatable ::  ple_ana(:,:,:) , ple_bkg(:,:,:)
      real, allocatable ::  pke_ana(:,:,:) , pke_bkg(:,:,:)
      real, allocatable ::    q_ana(:,:,:) ,   q_bkg(:,:,:)
      real, allocatable ::   o3_ana(:,:,:) ,  o3_bkg(:,:,:)
      real, allocatable ::   ps_ana(:,:)   ,  ps_bkg(:,:)
      real, allocatable ::   ts_ana(:,:)   ,  ts_bkg(:,:)
      real, allocatable :: phis_ana(:,:)   ,phis_bkg(:,:)
      real, allocatable ::       ak(:)
      real, allocatable ::       bk(:)
      real, allocatable ::     pref(:)

      real, allocatable ::   uglo(:,:)
      real, allocatable ::   vglo(:,:)
      real, allocatable ::    glo(:,:)

      real, pointer ::    u_tmp(:,:,:)
      real, pointer ::    v_tmp(:,:,:)
      real, pointer ::    t_tmp(:,:,:)
      real, pointer ::  thv_tmp(:,:,:)
      real, pointer ::   pk_tmp(:,:,:)
      real, pointer ::  ple_tmp(:,:,:)
      real, pointer ::  pke_tmp(:,:,:)
      real, pointer ::    q_tmp(:,:,:)
      real, pointer ::   o3_tmp(:,:,:)
      real, pointer ::   ts_tmp(:,:)
      real, pointer :: phis_tmp(:,:)

      real    kappa, tauiau, pl,pabove,pbelow,alf
      real    undefa,undefb,rgas,rvap,eps

      logical lremap
      logical   damp
      logical   agrid_ana,   agrid_bkg
      logical   dgrid_ana,   dgrid_bkg
      logical u_agrid_ana, u_agrid_bkg
      logical v_agrid_ana, v_agrid_bkg
      logical u_dgrid_ana, u_dgrid_bkg
      logical v_dgrid_ana, v_dgrid_bkg
      logical  tvflag_ana,  tvflag_bkg
      logical thvflag_ana, thvflag_bkg

      character*256, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,i,j,L,ID,rc,iostat,method

      real, parameter :: tauana = 21600.0

C **********************************************************************
C ****                 Initialize MPI Environment                   ****
C **********************************************************************

      call timebeg ('main')

      call mpi_init                ( ierror ) ; comm = mpi_comm_world
      call mpi_comm_rank ( comm,myid,ierror )
      call mpi_comm_size ( comm,npes,ierror )
      npex = nint ( sqrt( float(npes) ) )
      npey = npex
      do while ( npex*npey .ne. npes )
         npex = npex-1
         npey = nint ( float(npes)/float(npex) )
      enddo

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      kappa = 2.0/7.0

      damp   = .false.
      bkgeta = 'xxx'
      anaeta = 'xxx'
      iaueta = 'xxx'
      biasin = 'xxx'
      nymd   = -999
      nhms   = -999
      method = -999
      sclinc = 1.0

      imoglobal = -999
      jmoglobal = -999

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
             if( trim(arg(n)).eq.'-bkg'      ) then
                                   bkgeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-divr'     ) method = 1
             if( trim(arg(n)).eq.'-diva'     ) method = 2
             if( trim(arg(n)).eq.'-damp'     ) damp   = .true.
             if( trim(arg(n)).eq.'-ana'      ) then
                                   anaeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-iau'      ) then
                                   iaueta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-bias'     ) then
                                   biasin   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-nymd'  ) read(arg(n+1),*) nymd
             if( trim(arg(n)).eq.'-nhms'  ) read(arg(n+1),*) nhms
             if( trim(arg(n)).eq.'-scale' ) read(arg(n+1),*) sclinc
             if( trim(arg(n)).eq.'-imout' ) read(arg(n+1),*) imoglobal
             if( trim(arg(n)).eq.'-jmout' ) read(arg(n+1),*) jmoglobal
      enddo

      write(date,101) nymd
      write(hour,102) nhms/10000
  101 format(i8.8)
  102 format(i2.2)

      if(trim(bkgeta).eq.'xxx') bkgeta = 'geos5.bkg.eta.' // date // '_' // hour // 'z.nc4'
      if(trim(anaeta).eq.'xxx') anaeta = 'geos5.ana.eta.' // date // '_' // hour // 'z.nc4'
      if(trim(iaueta).eq.'xxx') iaueta = 'geos5.iau.eta.' // date // '_' // hour // 'z.bin'

      if( (imoglobal.ne.-999 .and. jmoglobal.eq.-999) .or.
     .    (imoglobal.eq.-999 .and. jmoglobal.ne.-999) ) then
          if( myid.eq.0 ) then
          print *
          print *, 'You must supply BOTH parameters: IMOUT & JMOUT!'
          endif
          call my_finalize
          call exit (7)
          stop
      endif

! **********************************************************************
! ****                   Read Analysis ana File                     ****
! **********************************************************************

      call timebeg(' read_ana')
      if( myid.eq.0 ) then
          print *
          print *, 'Reading  ANA File from PE: ',myid
          call gfio_open       ( trim(anaeta),1,ID,rc )
          call gfio_diminquire ( id,imaglobal,jmaglobal,lm,ntime,nvars,ngatts,rc )
      endif

      call mpi_bcast ( imaglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast ( jmaglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (        lm,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     ntime,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     nvars,1,  mpi_integer,0,comm,ierror )

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

        agrid_ana = .false.
        dgrid_ana = .false.
      u_agrid_ana = .false.
      v_agrid_ana = .false.
      u_dgrid_ana = .false.
      v_dgrid_ana = .false.
       tvflag_ana = .false.
      thvflag_ana = .false.

      if( myid.eq.0 ) then
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

      call mpi_bcast (   agrid_ana,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast (   dgrid_ana,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast (  tvflag_ana,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast ( thvflag_ana,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast ( undefa,     1,lattice_ana%mpi_rkind  ,0,comm,ierror )
      call mpi_bcast ( lon,imaglobal,lattice_ana%mpi_rkind  ,0,comm,ierror )

      if( (.not. agrid_ana .and. .not.  dgrid_ana)  .or.
     .    (.not.tvflag_ana .and. .not.thvflag_ana) ) then
          if( myid.eq.0 ) then
          print *
          print *, 'ANA_ETA file does not contain necessary data:'
          print *, '   agrid: ',  agrid_ana
          print *, '   dgrid: ',  dgrid_ana
          print *, ' tv_flag: ', tvflag_ana
          print *, 'thv_flag: ',thvflag_ana
          endif
          call my_finalize
          call exit (7)
          stop
      endif

      allocate ( phis_ana(ima,jma)      )
      allocate (   ps_ana(ima,jma)      )
      allocate (   ts_ana(ima,jma)      )
      allocate (    u_ana(ima,jma,lm)   )
      allocate (    v_ana(ima,jma,lm)   )
      allocate (  thv_ana(ima,jma,lm)   )
      allocate (    q_ana(ima,jma,lm)   )
      allocate (   o3_ana(ima,jma,lm)   )
      allocate (  ple_ana(ima,jma,lm+1) )

      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )

      call getit ( id,'phis' ,nymd,nhms,ima,jma,0,1 ,phis_ana,lattice_ana )
      call getit ( id,'ps'   ,nymd,nhms,ima,jma,0,1 ,  ps_ana,lattice_ana )
      call getit ( id,'ts'   ,nymd,nhms,ima,jma,0,1 ,  ts_ana,lattice_ana )
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

      if( myid.eq.0 ) then
          call gfio_getrealatt ( id,'ak',lm+1,ak,rc )
          call gfio_getrealatt ( id,'bk',lm+1,bk,rc )
          call gfio_close  ( id,rc )
      endif

      call mpi_bcast ( ak,lm+1,lattice_ana%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( bk,lm+1,lattice_ana%mpi_rkind,0,comm,ierror )

      call timeend(' read_ana')

! Construct Pressure Variables
! ----------------------------
      do L=1,lm+1
      ple_ana(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
      enddo

! Construct ANA THV (if necessary) for REMAPPING
! ----------------------------------------------
      if( tvflag_ana .and. .not.thvflag_ana ) then 
          allocate (  pk_ana(ima,jma,lm  ) )
          allocate ( pke_ana(ima,jma,lm+1) )
          pke_ana(:,:,:) = ple_ana(:,:,:)**kappa
          do L=1,lm
          pk_ana(:,:,L) = ( pke_ana(:,:,L+1)-pke_ana(:,:,L) )
     .                  / ( kappa*log(ple_ana(:,:,L+1)/ple_ana(:,:,L)) )
          enddo
          thv_ana =  tv_ana/pk_ana
          deallocate (  tv_ana )
          deallocate (  pk_ana )
          deallocate ( pke_ana )
      endif

      if( myid.eq.0 ) print *, 'Analysis ANA File begins at Lon: ',lon(1)
      if( lon(1) .eq. 0.0 ) then
          if( myid.eq.0 ) print *, '         Flipping Horizontal Coordinate' 
          call hflip (phis_ana,ima,jma,1   ,lattice_ana )
          call hflip (  ts_ana,ima,jma,1   ,lattice_ana )
          call hflip (   u_ana,ima,jma,lm  ,lattice_ana )
          call hflip (   v_ana,ima,jma,lm  ,lattice_ana )
          call hflip ( thv_ana,ima,jma,lm  ,lattice_ana )
          call hflip (   q_ana,ima,jma,lm  ,lattice_ana )
          call hflip (  o3_ana,ima,jma,lm  ,lattice_ana )
          call hflip ( ple_ana,ima,jma,lm+1,lattice_ana )
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

! **********************************************************************
! ****                  Read Background bkg File                    ****
! **********************************************************************

      call timebeg(' read_bkg')
      if( myid.eq.0 ) then
          print *
          print *, 'Reading  BKG File from PE: ',myid
          call gfio_open       ( trim(bkgeta),1,ID,rc )
          call gfio_diminquire ( id,imbglobal,jmbglobal,lm,ntime,nvars,ngatts,rc )
      endif

      call mpi_bcast ( imbglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast ( jmbglobal,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (        lm,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     ntime,1,  mpi_integer,0,comm,ierror )
      call mpi_bcast (     nvars,1,  mpi_integer,0,comm,ierror )

      call create_dynamics_lattice ( lattice_bkg,npex,npey )
      call   init_dynamics_lattice ( lattice_bkg,comm,imbglobal,jmbglobal,lm )

      imb = lattice_bkg%im( lattice_bkg%pei )
      jmb = lattice_bkg%jm( lattice_bkg%pej )

      allocate ( lon(imbglobal) )
      allocate ( lat(jmbglobal) )
      allocate ( lev(lm) )
      allocate ( yymmdd(ntime) )
      allocate ( hhmmss(ntime) )
      allocate (  vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate (  kmvar(nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )

        agrid_bkg = .false.
        dgrid_bkg = .false.
      u_agrid_bkg = .false.
      v_agrid_bkg = .false.
      u_dgrid_bkg = .false.
      v_dgrid_bkg = .false.
       tvflag_bkg = .false.
      thvflag_bkg = .false.

      if( myid.eq.0 ) then
          call gfio_inquire ( id,imbglobal,jmbglobal,lm,ntime,nvars,
     .                        title,source,contact,undefb,
     .                        lon,lat,lev,levunits,
     .                        yymmdd,hhmmss,timinc,
     .                        vname,vtitle,vunits,kmvar,
     .                        vrange,prange,rc )
         do n=1,nvars
         if( trim(vname(n)).eq.'u'     ) u_agrid_bkg = .true.
         if( trim(vname(n)).eq.'v'     ) v_agrid_bkg = .true.
         if( trim(vname(n)).eq.'uwnd'  ) u_dgrid_bkg = .true.
         if( trim(vname(n)).eq.'vwnd'  ) v_dgrid_bkg = .true.
         if( trim(vname(n)).eq.'tv'    )  tvflag_bkg = .true.
         if( trim(vname(n)).eq.'theta' ) thvflag_bkg = .true.
         enddo
         agrid_bkg = u_agrid_bkg .and. v_agrid_bkg
         dgrid_bkg = u_dgrid_bkg .and. v_dgrid_bkg
      endif

      call mpi_bcast (   agrid_bkg,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast (   dgrid_bkg,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast (  tvflag_bkg,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast ( thvflag_bkg,1,            mpi_logical,0,comm,ierror )
      call mpi_bcast ( undefb,     1,lattice_bkg%mpi_rkind,  0,comm,ierror )
      call mpi_bcast ( lon,imbglobal,lattice_bkg%mpi_rkind,  0,comm,ierror )

      allocate ( phis_bkg(imb,jmb)      )
      allocate (   ts_bkg(imb,jmb)      )
      allocate (   ps_bkg(imb,jmb)      )
      allocate (   pk_bkg(imb,jmb,lm)   )
      allocate (  pke_bkg(imb,jmb,lm+1) )
      allocate (  thv_bkg(imb,jmb,lm)   )

      allocate (    u_bkg(imb,jmb,lm)   )
      allocate (    v_bkg(imb,jmb,lm)   )
      allocate (    t_bkg(imb,jmb,lm)   )
      allocate (    q_bkg(imb,jmb,lm)   )
      allocate (   o3_bkg(imb,jmb,lm)   )
      allocate (  ple_bkg(imb,jmb,lm+1) )

      call getit ( id,'phis' ,nymd,nhms,imb,jmb,0,1 ,phis_bkg,lattice_bkg )
      call getit ( id,'ps'   ,nymd,nhms,imb,jmb,0,1 ,  ps_bkg,lattice_bkg )
      call getit ( id,'ts'   ,nymd,nhms,imb,jmb,0,1 ,  ts_bkg,lattice_bkg )
      call getit ( id,'sphu' ,nymd,nhms,imb,jmb,1,lm,   q_bkg,lattice_bkg )
      call getit ( id,'ozone',nymd,nhms,imb,jmb,1,lm,  o3_bkg,lattice_bkg )

      if( agrid_bkg ) then
          call getit ( id,'u'   ,nymd,nhms,imb,jmb,1,lm,u_bkg,lattice_bkg )
          call getit ( id,'v'   ,nymd,nhms,imb,jmb,1,lm,v_bkg,lattice_bkg )
      else if( dgrid_bkg ) then
          call getit ( id,'uwnd',nymd,nhms,imb,jmb,1,lm,u_bkg,lattice_bkg )
          call getit ( id,'vwnd',nymd,nhms,imb,jmb,1,lm,v_bkg,lattice_bkg )
      endif

      if( thvflag_bkg ) then
          call getit ( id,'theta',nymd,nhms,imb,jmb,1,lm,thv_bkg,lattice_bkg )
      else if( tvflag_bkg ) then
          allocate   ( tv_bkg(imb,jmb,lm) )
          call getit ( id,'tv'   ,nymd,nhms,imb,jmb,1,lm, tv_bkg,lattice_bkg )
      endif

      if( myid.eq.0 ) call gfio_close  ( id,rc )
      call timeend(' read_bkg')

! Construct Pressure Variables
! ----------------------------
      do L=1,lm+1
      ple_bkg(:,:,L) = ak(L) + ps_bkg(:,:)*bk(L)
      enddo

! Construct BKG THV (if necessary) for REMAPPING
! ----------------------------------------------
      if( tvflag_bkg .and. .not.thvflag_bkg ) then 
          pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa
          do L=1,lm
          pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .                  / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
          enddo
          thv_bkg =  tv_bkg/pk_bkg
          deallocate (  tv_bkg )
      endif

      if( myid.eq.0 ) print *, '   Model BKG File begins at Lon: ',lon(1)
      if( lon(1) .eq. 0.0 ) then
          if( myid.eq.0 ) print *, '         Flipping Horizontal Coordinate' 
          call hflip (phis_bkg,imb,jmb,1   ,lattice_bkg )
          call hflip (  ts_bkg,imb,jmb,1   ,lattice_bkg )
          call hflip (   u_bkg,imb,jmb,lm  ,lattice_bkg )
          call hflip (   v_bkg,imb,jmb,lm  ,lattice_bkg )
          call hflip ( thv_bkg,imb,jmb,lm  ,lattice_bkg )
          call hflip (   q_bkg,imb,jmb,lm  ,lattice_bkg )
          call hflip (  o3_bkg,imb,jmb,lm  ,lattice_bkg )
          call hflip ( ple_bkg,imb,jmb,lm+1,lattice_bkg )
      endif

! **********************************************************************
! ****                 Read BIAS File from Analysis                 ****
! **********************************************************************

      if( trim(biasin).ne.'xxx' ) then

          allocate ( psb_ana(ima,jma)      )
          allocate ( tsb_ana(ima,jma)      )
          allocate (  ub_ana(ima,jma,lm)   )
          allocate (  vb_ana(ima,jma,lm)   )
          allocate (  tb_ana(ima,jma,lm)   )
          allocate (  qb_ana(ima,jma,lm)   )
          allocate ( o3b_ana(ima,jma,lm)   )
          allocate (  pb_ana(ima,jma,lm+1) )

          allocate ( tsb_bkg(imb,jmb)      )
          allocate (  ub_bkg(imb,jmb,lm)   )
          allocate (  vb_bkg(imb,jmb,lm)   )
          allocate (  tb_bkg(imb,jmb,lm)   )
          allocate (  qb_bkg(imb,jmb,lm)   )
          allocate ( o3b_bkg(imb,jmb,lm)   )
          allocate (  pb_bkg(imb,jmb,lm+1) )

          if( myid.eq.0 ) then
              print *
              print *, 'Reading BIAS File: ',trim(biasin)
          endif
          open   (10,file=trim(biasin),
     .            form='unformatted',convert="big_endian",access='sequential')

          rewind (10)
          call readit ( psb_ana,ima,jma,1 ,10,lattice_ana ) ! Log(PS)
          call readit (  ub_ana,ima,jma,lm,10,lattice_ana ) ! Dummy Read VOR
          call readit (  ub_ana,ima,jma,lm,10,lattice_ana ) ! Dummy Read DIV
          call readit (  ub_ana,ima,jma,lm,10,lattice_ana ) ! D-Grid  U-Wind
          call readit (  vb_ana,ima,jma,lm,10,lattice_ana ) ! D-Grid  V-Wind
          call readit (  tb_ana,ima,jma,lm,10,lattice_ana ) ! Virtual Temperature
          call readit (  qb_ana,ima,jma,lm,10,lattice_ana ) ! Specific Humidity
          call readit ( o3b_ana,ima,jma,lm,10,lattice_ana ) ! Dummy Read
          call readit ( o3b_ana,ima,jma,lm,10,lattice_ana ) ! Ozone
          call readit ( tsb_ana,ima,jma,1 ,10,lattice_ana ) ! TSkin
          close  (10)

          call hflip  ( psb_ana,ima,jma,1 ,lattice_ana )
          call hflip  ( tsb_ana,ima,jma,1 ,lattice_ana )
          call hflip  (  ub_ana,ima,jma,lm,lattice_ana )
          call hflip  (  vb_ana,ima,jma,lm,lattice_ana )
          call hflip  (  tb_ana,ima,jma,lm,lattice_ana )
          call hflip  (  qb_ana,ima,jma,lm,lattice_ana )
          call hflip  ( o3b_ana,ima,jma,lm,lattice_ana )

          do L=1,lm+1
          pb_ana(:,:,L) = ak(L) + psb_ana(:,:)*bk(L)
          enddo

! Only Allow Moisture Bias
! ------------------------
           ub_ana = 0.0
           vb_ana = 0.0
           tb_ana = 0.0
           pb_ana = 0.0
          o3b_ana = 0.0
          tsb_ana = 0.0

! Compute Bias Tendency
! ---------------------
           ub_bkg =  ub_ana/tauana
           vb_bkg =  vb_ana/tauana
           tb_bkg =  tb_ana/tauana
           qb_bkg =  qb_ana/tauana
           pb_bkg =  pb_ana/tauana
          o3b_bkg = o3b_ana/tauana
          tsb_bkg = tsb_ana/tauana

      endif

! **********************************************************************
! ****                       Echo Input Files                       ****
! **********************************************************************

      if( myid.eq.0 ) then
          print *
          print *, '    Background filename: ',trim(bkgeta)
          print *, '         BKG resolution: ',imbglobal,jmbglobal,lm
          print *
          print *, '              agrid_bkg: ',  agrid_bkg
          print *, '              dgrid_bkg: ',  dgrid_bkg
          print *, '             tvflag_bkg: ', tvflag_bkg
          print *, '            thvflag_bkg: ',thvflag_bkg
          print *
          print *, '      Analysis filename: ',trim(anaeta)
          print *, '         ANA resolution: ',imaglobal,jmaglobal,lm
          print *
          print *, '              agrid_ana: ',  agrid_ana
          print *, '              dgrid_ana: ',  dgrid_ana
          print *, '             tvflag_ana: ', tvflag_ana
          print *, '            thvflag_ana: ',thvflag_ana
          print *
          print *, '                   Date: ',nymd,nhms
          if( imoglobal.eq.-999 .and. jmoglobal.eq.-999 ) then
          print *, '      Output resolution: ',imbglobal,jmbglobal,lm
          else
          print *, '      Output resolution: ',imoglobal,jmoglobal,lm
          endif
          print *
      endif

! **********************************************************************
! ****               Construct A-Grid Winds (if necessary)          ****
! **********************************************************************

      if( dgrid_ana .and. .not.agrid_ana ) then
      call timebeg(' dtoa')
           if( myid.eq.0 ) print *, 'Calling DTOA for ANA Winds ...'
           allocate ( uglo(imaglobal,jmaglobal) )
           allocate ( vglo(imaglobal,jmaglobal) )
           do L=1,lm
                                          call timebeg ('   Gather')
                                          call gather_2d  ( uglo,u_ana(1,1,L),lattice_ana )
                                          call gather_2d  ( vglo,v_ana(1,1,L),lattice_ana )
                                          call timeend ('   Gather')
              if( lattice_ana%myid.eq.0 ) call dtoa_winds ( uglo,vglo,uglo,vglo,imaglobal,jmaglobal,1 )
                                          call timebeg ('   Scatter')
                                          call scatter_2d ( uglo,u_ana(1,1,L),lattice_ana )
                                          call scatter_2d ( vglo,v_ana(1,1,L),lattice_ana )
                                          call timeend ('   Scatter')
           enddo
           deallocate ( uglo,vglo )
      call timeend(' dtoa')
      endif

      if( dgrid_bkg .and. .not.agrid_bkg ) then
      call timebeg(' dtoa')
           if( myid.eq.0 ) print *, 'Calling DTOA for BKG Winds ...'
           allocate ( uglo(imbglobal,jmbglobal) )
           allocate ( vglo(imbglobal,jmbglobal) )
           do L=1,lm
                                          call timebeg ('   Gather')
                                          call gather_2d  ( uglo,u_bkg(1,1,L),lattice_bkg )
                                          call gather_2d  ( vglo,v_bkg(1,1,L),lattice_bkg )
                                          call timeend ('   Gather')
              if( lattice_bkg%myid.eq.0 ) call dtoa_winds ( uglo,vglo,uglo,vglo,imbglobal,jmbglobal,1 )
                                          call timebeg ('   Scatter')
                                          call scatter_2d ( uglo,u_bkg(1,1,L),lattice_bkg )
                                          call scatter_2d ( vglo,v_bkg(1,1,L),lattice_bkg )
                                          call timeend ('   Scatter')
           enddo
           deallocate ( uglo,vglo )
      call timeend(' dtoa')
      endif

#ifdef debug
! Write Original ANA Data for Debugging
! -------------------------------------
      call writit  (phis_ana,          ima,jma,1   ,31,lattice_ana )
      call writit  (  ts_ana,          ima,jma,1   ,31,lattice_ana )
      call writit  ( ple_ana(1,1,lm+1),ima,jma,1   ,31,lattice_ana )
      call writit  (   u_ana,          ima,jma,lm  ,31,lattice_ana )
      call writit  (   v_ana,          ima,jma,lm  ,31,lattice_ana )
      call writit  ( thv_ana,          ima,jma,lm  ,31,lattice_ana )

! Write Original BKG Data for Debugging
! -------------------------------------
      call writit  (phis_bkg,          imb,jmb,1   ,41,lattice_bkg )
      call writit  (  ts_bkg,          imb,jmb,1   ,41,lattice_bkg )
      call writit  ( ple_bkg(1,1,lm+1),imb,jmb,1   ,41,lattice_bkg )
      call writit  (   u_bkg,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  (   v_bkg,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  ( thv_bkg,          imb,jmb,lm  ,41,lattice_bkg )
#endif

! **********************************************************************
! ****           Modify BKG Data (if differing resolutions)         ****
! **********************************************************************

      if( imaglobal.ne.imbglobal .or. jmaglobal.ne.jmbglobal ) then

          allocate (    u_tmp(ima,jma,lm)   )
          allocate (    v_tmp(ima,jma,lm)   )
          allocate (    t_tmp(ima,jma,lm)   )
          allocate (  thv_tmp(ima,jma,lm)   )
          allocate (    q_tmp(ima,jma,lm)   )
          allocate (   o3_tmp(ima,jma,lm)   )
          allocate (   pk_tmp(ima,jma,lm)   )
          allocate (  ple_tmp(ima,jma,lm+1) )
          allocate (  pke_tmp(ima,jma,lm+1) )
          allocate (   ts_tmp(ima,jma)      )
          allocate ( phis_tmp(ima,jma)      )

! Bin BKG file to ANA Resolution
! ------------------------------
      if( imaglobal.lt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Binning       BKG Data to ANA Resolution'
          call bin (phis_bkg,imb,jmb, phis_tmp,ima,jma,1   ,undefb, 1 ,lattice_bkg,lattice_ana )
          call bin (  ts_bkg,imb,jmb,   ts_tmp,ima,jma,1   ,undefb, 1 ,lattice_bkg,lattice_ana )
          call bin (   u_bkg,imb,jmb,    u_tmp,ima,jma,lm  ,undefb, 1 ,lattice_bkg,lattice_ana )
          call bin (   v_bkg,imb,jmb,    v_tmp,ima,jma,lm  ,undefb, 1 ,lattice_bkg,lattice_ana )
          call bin ( thv_bkg,imb,jmb,  thv_tmp,ima,jma,lm  ,undefb, 0 ,lattice_bkg,lattice_ana )
          call bin (   q_bkg,imb,jmb,    q_tmp,ima,jma,lm  ,undefb, 0 ,lattice_bkg,lattice_ana )
          call bin (  o3_bkg,imb,jmb,   o3_tmp,ima,jma,lm  ,undefb, 0 ,lattice_bkg,lattice_ana )
          call bin ( ple_bkg,imb,jmb,  ple_tmp,ima,jma,lm+1,undefb, 0 ,lattice_bkg,lattice_ana )
      endif

! Interpolate BKG file to ANA Resolution
! --------------------------------------
      if( imaglobal.gt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Interpolating BKG Data to ANA Resolution'
          call hinterp (phis_bkg,imb,jmb, phis_tmp,ima,jma,1   ,undefb,lattice_bkg,lattice_ana )
          call hinterp (  ts_bkg,imb,jmb,   ts_tmp,ima,jma,1   ,undefb,lattice_bkg,lattice_ana )
          call hinterp (   u_bkg,imb,jmb,    u_tmp,ima,jma,lm  ,undefb,lattice_bkg,lattice_ana )
          call hinterp (   v_bkg,imb,jmb,    v_tmp,ima,jma,lm  ,undefb,lattice_bkg,lattice_ana )
          call hinterp ( thv_bkg,imb,jmb,  thv_tmp,ima,jma,lm  ,undefb,lattice_bkg,lattice_ana )
          call hinterp (   q_bkg,imb,jmb,    q_tmp,ima,jma,lm  ,undefb,lattice_bkg,lattice_ana )
          call hinterp (  o3_bkg,imb,jmb,   o3_tmp,ima,jma,lm  ,undefb,lattice_bkg,lattice_ana )
          call hinterp ( ple_bkg,imb,jmb,  ple_tmp,ima,jma,lm+1,undefb,lattice_bkg,lattice_ana )
      endif

! Remap Based on TOPO Differences
! -------------------------------
          if( myid.eq.0 ) print *, 'Remapping     BKG Data to ANA Topography'
          call timebeg(' remap')
          call remap ( ple_tmp,
     .                   u_tmp,
     .                   v_tmp,
     .                 thv_tmp,
     .                   q_tmp,
     .                  o3_tmp,
     .                phis_tmp,phis_ana,ak,bk,ima,jma,lm )
          call timeend(' remap')

#ifdef debug
! Write BKG DATA at ANA Resolution for Debugging
! ----------------------------------------------
      call writit  (phis_tmp,          ima,jma,1   ,31,lattice_ana )
      call writit  (  ts_tmp,          ima,jma,1   ,31,lattice_ana )
      call writit  ( ple_tmp(1,1,lm+1),ima,jma,1   ,31,lattice_ana )
      call writit  (   u_tmp,          ima,jma,lm  ,31,lattice_ana )
      call writit  (   v_tmp,          ima,jma,lm  ,31,lattice_ana )
      call writit  ( thv_tmp,          ima,jma,lm  ,31,lattice_ana )
#endif

        deallocate ( phis_tmp          )
          allocate ( phis_tmp(imb,jmb) )

! Interpolate BKG Back to Original Resolution
! -------------------------------------------
      if( imaglobal.lt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Interpolating BKG Back to Original Resolution'
          call hinterp (phis_ana,ima,jma, phis_tmp,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg )
          call hinterp (  ts_tmp,ima,jma,   ts_bkg,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   u_tmp,ima,jma,    u_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   v_tmp,ima,jma,    v_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp ( thv_tmp,ima,jma,  thv_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   q_tmp,ima,jma,    q_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (  o3_tmp,ima,jma,   o3_bkg,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp ( ple_tmp,ima,jma,  ple_bkg,imb,jmb,lm+1,undefa,lattice_ana,lattice_bkg )
      endif

! Bin BKG Back to Original Resolution
! -----------------------------------
      if( imaglobal.gt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Binning       BKG Back to Original Resolution'
          call bin (phis_ana,ima,jma, phis_tmp,imb,jmb,1   ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (  ts_tmp,ima,jma,   ts_bkg,imb,jmb,1   ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (   u_tmp,ima,jma,    u_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (   v_tmp,ima,jma,    v_bkg,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin ( thv_tmp,ima,jma,  thv_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (   q_tmp,ima,jma,    q_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (  o3_tmp,ima,jma,   o3_bkg,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin ( ple_tmp,ima,jma,  ple_bkg,imb,jmb,lm+1,undefa, 0 ,lattice_ana,lattice_bkg )
      endif

! Remap Based on TOPO Differences
! -------------------------------
          if( myid.eq.0 ) print *, 'Remapping Modified BKG to Original Topography'
          call timebeg(' remap')
          call remap ( ple_bkg,
     .                   u_bkg,
     .                   v_bkg,
     .                 thv_bkg,
     .                   q_bkg,
     .                  o3_bkg,
     .                phis_tmp,phis_bkg,ak,bk,imb,jmb,lm )
          call timeend(' remap')

#ifdef debug
! Write Modified BKG Data for Debugging
! -------------------------------------
      call writit  (phis_tmp,          imb,jmb,1   ,41,lattice_bkg )
      call writit  (  ts_bkg,          imb,jmb,1   ,41,lattice_bkg )
      call writit  ( ple_bkg(1,1,lm+1),imb,jmb,1   ,41,lattice_bkg )
      call writit  (   u_bkg,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  (   v_bkg,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  ( thv_bkg,          imb,jmb,lm  ,41,lattice_bkg )
#endif

      endif

! Create BKG Dry Temperature
! --------------------------
      pke_bkg(:,:,:) = ple_bkg(:,:,:)**kappa
      do L=1,lm
      pk_bkg(:,:,L) = ( pke_bkg(:,:,L+1)-pke_bkg(:,:,L) )
     .              / ( kappa*log(ple_bkg(:,:,L+1)/ple_bkg(:,:,L)) )
      enddo
      t_bkg = thv_bkg*pk_bkg/(1+eps*q_bkg)

! **********************************************************************
! ****        Check for TOPO Differences if Same Resolution         ****
! **********************************************************************

      if( imaglobal.eq.imbglobal .and. jmaglobal.eq.jmbglobal ) then
          allocate ( uglo(imaglobal,jmaglobal) )
          allocate ( vglo(imaglobal,jmaglobal) )
          call gather_2d ( uglo,phis_bkg,lattice_bkg )
          call gather_2d ( vglo,phis_ana,lattice_ana )
                          lremap = .false.
          if( myid.eq.0 ) lremap = count( uglo.ne.vglo ).ne.0
          call mpi_bcast (lremap,1,mpi_logical,0,comm,ierror )
          deallocate ( uglo,vglo )

          if( lremap ) then
          if( myid.eq.0 ) print *, 'Remapping     ANA Data to BKG Topography'
          call timebeg(' remap')
          call remap ( ple_ana,
     .                   u_ana,
     .                   v_ana,
     .                 thv_ana,
     .                   q_ana,
     .                  o3_ana,
     .                phis_ana,phis_bkg,ak,bk,imb,jmb,lm )
          call timeend(' remap')
          endif
      endif

! **********************************************************************
! ****                Create ANA data at BKG resolution             ****
! **********************************************************************

        if( associated(    u_tmp ) ) deallocate (    u_tmp ) ; allocate (    u_tmp(imb,jmb,lm)   )
        if( associated(    v_tmp ) ) deallocate (    v_tmp ) ; allocate (    v_tmp(imb,jmb,lm)   )
        if( associated(    t_tmp ) ) deallocate (    t_tmp ) ; allocate (    t_tmp(imb,jmb,lm)   )
        if( associated(  thv_tmp ) ) deallocate (  thv_tmp ) ; allocate (  thv_tmp(imb,jmb,lm)   )
        if( associated(    q_tmp ) ) deallocate (    q_tmp ) ; allocate (    q_tmp(imb,jmb,lm)   )
        if( associated(   o3_tmp ) ) deallocate (   o3_tmp ) ; allocate (   o3_tmp(imb,jmb,lm)   )
        if( associated(   pk_tmp ) ) deallocate (   pk_tmp ) ; allocate (   pk_tmp(imb,jmb,lm)   )
        if( associated(  ple_tmp ) ) deallocate (  ple_tmp ) ; allocate (  ple_tmp(imb,jmb,lm+1) )
        if( associated(  pke_tmp ) ) deallocate (  pke_tmp ) ; allocate (  pke_tmp(imb,jmb,lm+1) )
        if( associated(   ts_tmp ) ) deallocate (   ts_tmp ) ; allocate (   ts_tmp(imb,jmb)      )
        if( associated( phis_tmp ) ) deallocate ( phis_tmp ) ; allocate ( phis_tmp(imb,jmb)      )

      if( imaglobal.ne.imbglobal .or. jmaglobal.ne.jmbglobal ) then

! Interpolate ANA Data to BKG Resolution
! --------------------------------------
      if( imaglobal.lt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Interpolating ANA Data to BKG Resolution'
          call hinterp (phis_ana,ima,jma, phis_tmp,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg )
          call hinterp (  ts_ana,ima,jma,   ts_tmp,imb,jmb,1   ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   u_ana,ima,jma,    u_tmp,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   v_ana,ima,jma,    v_tmp,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp ( thv_ana,ima,jma,  thv_tmp,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (   q_ana,ima,jma,    q_tmp,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp (  o3_ana,ima,jma,   o3_tmp,imb,jmb,lm  ,undefa,lattice_ana,lattice_bkg )
          call hinterp ( ple_ana,ima,jma,  ple_tmp,imb,jmb,lm+1,undefa,lattice_ana,lattice_bkg )
      endif

! Bin ANA Data to BKG Resolution
! ------------------------------
      if( imaglobal.gt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Binning       ANA Data to BKG Resolution'
          call bin (phis_ana,ima,jma, phis_tmp,imb,jmb,1   ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (  ts_ana,ima,jma,   ts_tmp,imb,jmb,1   ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (   u_ana,ima,jma,    u_tmp,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin (   v_ana,ima,jma,    v_tmp,imb,jmb,lm  ,undefa, 1 ,lattice_ana,lattice_bkg )
          call bin ( thv_ana,ima,jma,  thv_tmp,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (   q_ana,ima,jma,    q_tmp,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin (  o3_ana,ima,jma,   o3_tmp,imb,jmb,lm  ,undefa, 0 ,lattice_ana,lattice_bkg )
          call bin ( ple_ana,ima,jma,  ple_tmp,imb,jmb,lm+1,undefa, 0 ,lattice_ana,lattice_bkg )
      endif

! Remap Based on TOPO Differences
! -------------------------------
          if( myid.eq.0 ) print *, 'Remapping     ANA Data to BKG Topography'
          call timebeg(' remap')
          call remap ( ple_tmp,
     .                   u_tmp,
     .                   v_tmp,
     .                 thv_tmp,
     .                   q_tmp,
     .                  o3_tmp,
     .                phis_tmp,phis_bkg,ak,bk,imb,jmb,lm )
          call timeend(' remap')

#ifdef debug
! Write ANA DATA at BKG Resolution for Debugging
! ----------------------------------------------
      call writit  (phis_tmp,          imb,jmb,1   ,41,lattice_bkg )
      call writit  (  ts_tmp,          imb,jmb,1   ,41,lattice_bkg )
      call writit  ( ple_tmp(1,1,lm+1),imb,jmb,1   ,41,lattice_bkg )
      call writit  (   u_tmp,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  (   v_tmp,          imb,jmb,lm  ,41,lattice_bkg )
      call writit  ( thv_tmp,          imb,jmb,lm  ,41,lattice_bkg )
#endif

      else

! BKG and ANA Horizontal Resolutions Match
! ----------------------------------------
        phis_tmp = phis_ana
          ts_tmp =   ts_ana
           u_tmp =    u_ana
           v_tmp =    v_ana
         thv_tmp =  thv_ana
           q_tmp =    q_ana
          o3_tmp =   o3_ana
         ple_tmp =  ple_ana

      endif

! Create ANA Dry Temperature
! --------------------------
      pke_tmp(:,:,:) = ple_tmp(:,:,:)**kappa
      do L=1,lm
      pk_tmp(:,:,L) = ( pke_tmp(:,:,L+1)-pke_tmp(:,:,L) )
     .              / ( kappa*log(ple_tmp(:,:,L+1)/ple_tmp(:,:,L)) )
      enddo
      t_tmp = thv_tmp*pk_tmp/(1+eps*q_tmp)

! **********************************************************************
! ****           Damp Increments between Pabove and Pbelow          ****
! **********************************************************************

      if( damp ) then
          pabove = 100 ! 1-mb
          pbelow = 500 ! 5-mb
          if( myid.eq.0 ) write(6,1001) pabove/100,pbelow/100
          allocate ( pref(lm+1) )
          do L=1,lm+1
          pref(L) = ak(L) + 100000.0*bk(L)
          enddo
          do L=1,lm
            pl=0.5*(pref(L+1)+pref(L))
           alf= max( 0.0, (pl-pabove)/(pbelow-pabove) )
           if( pl.lt.pbelow )  then
               if( myid.eq.0 ) write(6,1002) L,pl/100,alf
               u_tmp(:,:,L) =   u_bkg(:,:,L) + (  u_tmp(:,:,L)-  u_bkg(:,:,L))*alf
               v_tmp(:,:,L) =   v_bkg(:,:,L) + (  v_tmp(:,:,L)-  v_bkg(:,:,L))*alf
               t_tmp(:,:,L) =   t_bkg(:,:,L) + (  t_tmp(:,:,L)-  t_bkg(:,:,L))*alf
               q_tmp(:,:,L) =   q_bkg(:,:,L) + (  q_tmp(:,:,L)-  q_bkg(:,:,L))*alf
              o3_tmp(:,:,L) =  o3_bkg(:,:,L) + ( o3_tmp(:,:,L)- o3_bkg(:,:,L))*alf
             ple_tmp(:,:,L) = ple_bkg(:,:,L) + (ple_tmp(:,:,L)-ple_bkg(:,:,L))*alf
           endif
          enddo
          if( myid.eq.0 ) print *
      endif
 1001 format(1x,'Damping Increments Between ',f5.2,' mb and ',f5.2,' mb') 
 1002 format(1x,'Level: ',i3,' Pmid: ',f6.2,' Damping Coef: ',f7.4) 

! **********************************************************************
! ****          Modify vertically integrated wind increment         ****
! ****                    to be non-divergent                       ****
! **********************************************************************

      if( method.eq.-999 ) then
          if( myid.eq.0 )       print *, 'No Wind Adjustment  Change to Divergence'
      else
          if( myid.eq.0 ) then
              if( method.eq.1 ) print *, 'Minimizing Relative Change to Divergence'
              if( method.eq.2 ) print *, 'Minimizing Absolute Change to Divergence'
                                print *, 'Calling Windfix'
          endif
          call timebeg (' windfix')
          call windfix ( u_tmp,v_tmp,ple_tmp,
     .                   u_bkg,v_bkg,ple_bkg,imb,jmb,lm,lattice_bkg,'A',method )
          call timeend (' windfix')
      endif

! **********************************************************************
! ****                Create IAU Data at BKG resolution             ****
! **********************************************************************

          ts_bkg =  sclinc*(  ts_tmp- ts_bkg )
           u_bkg =  sclinc*(   u_tmp-  u_bkg )
           v_bkg =  sclinc*(   v_tmp-  v_bkg )
           t_bkg =  sclinc*(   t_tmp-  t_bkg )
           q_bkg =  sclinc*(   q_tmp-  q_bkg )
          o3_bkg =  sclinc*(  o3_tmp- o3_bkg )
         ple_bkg =  sclinc*( ple_tmp-ple_bkg )

! Write IAU Forcing File
! ----------------------
      call timebeg (' write_iau')
      if( myid.eq.0 ) then
          print *
          print *, 'Writing IAU File: ',trim(iaueta)
      endif
      open   (10,file=trim(iaueta),form='unformatted',access='sequential')

      if( (imoglobal.eq.-999      .and. jmoglobal.eq.-999     )  .or.
     .    (imoglobal.eq.imbglobal .and. jmoglobal.eq.jmbglobal) ) then

          rewind (10)
          call writit  (   u_bkg,imb,jmb,lm  ,10,lattice_bkg )
          call writit  (   v_bkg,imb,jmb,lm  ,10,lattice_bkg )
          call writit  (   t_bkg,imb,jmb,lm  ,10,lattice_bkg )
          call writit  ( ple_bkg,imb,jmb,lm+1,10,lattice_bkg )
          call writit  (   q_bkg,imb,jmb,lm  ,10,lattice_bkg )
          call writit  (  o3_bkg,imb,jmb,lm  ,10,lattice_bkg )
          call writit  (  ts_bkg,imb,jmb,1   ,10,lattice_bkg )
          close  (10)

      else

          call create_dynamics_lattice ( lattice_out,npex,npey )
          call   init_dynamics_lattice ( lattice_out,comm,imoglobal,jmoglobal,lm )

          imo = lattice_out%im( lattice_out%pei )
          jmo = lattice_out%jm( lattice_out%pej )

          if( associated(    u_tmp ) ) deallocate (    u_tmp ) ; allocate (   u_tmp(imo,jmo,lm)   )
          if( associated(    v_tmp ) ) deallocate (    v_tmp ) ; allocate (   v_tmp(imo,jmo,lm)   )
          if( associated(    t_tmp ) ) deallocate (    t_tmp ) ; allocate (   t_tmp(imo,jmo,lm)   )
          if( associated(  ple_tmp ) ) deallocate (  ple_tmp ) ; allocate ( ple_tmp(imo,jmo,lm+1) )
          if( associated(    q_tmp ) ) deallocate (    q_tmp ) ; allocate (   q_tmp(imo,jmo,lm)   )
          if( associated(   o3_tmp ) ) deallocate (   o3_tmp ) ; allocate (  o3_tmp(imo,jmo,lm)   )
          if( associated(   ts_tmp ) ) deallocate (   ts_tmp ) ; allocate (  ts_tmp(imo,jmo)      )

! Bin BKG file to OUT Resolution
! ------------------------------
      if( imoglobal.lt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Binning IAU BKG Data to OUT Resolution'
          call bin (   u_bkg,imb,jmb,    u_tmp,imo,jmo,lm  ,undefb, 1 ,lattice_bkg, lattice_out )
          call bin (   v_bkg,imb,jmb,    v_tmp,imo,jmo,lm  ,undefb, 1 ,lattice_bkg, lattice_out )
          call bin (   t_bkg,imb,jmb,    t_tmp,imo,jmo,lm  ,undefb, 0 ,lattice_bkg, lattice_out )
          call bin ( ple_bkg,imb,jmb,  ple_tmp,imo,jmo,lm+1,undefb, 0 ,lattice_bkg, lattice_out )
          call bin (   q_bkg,imb,jmb,    q_tmp,imo,jmo,lm  ,undefb, 0 ,lattice_bkg, lattice_out )
          call bin (  o3_bkg,imb,jmb,   o3_tmp,imo,jmo,lm  ,undefb, 0 ,lattice_bkg, lattice_out )
          call bin (  ts_bkg,imb,jmb,   ts_tmp,imo,jmo,1   ,undefb, 0 ,lattice_bkg, lattice_out )
      endif

! Interpolate BKG file to OUT Resolution
! --------------------------------------
      if( imoglobal.gt.imbglobal ) then
          if( myid.eq.0 ) print *, 'Interpolating IAU BKG Data to OUT Resolution'
          call hinterp (   u_bkg,imb,jmb,    u_tmp,imo,jmo,lm  ,undefb, lattice_bkg, lattice_out )
          call hinterp (   v_bkg,imb,jmb,    v_tmp,imo,jmo,lm  ,undefb, lattice_bkg, lattice_out )
          call hinterp (   t_bkg,imb,jmb,    t_tmp,imo,jmo,lm  ,undefb, lattice_bkg, lattice_out )
          call hinterp ( ple_bkg,imb,jmb,  ple_tmp,imo,jmo,lm+1,undefb, lattice_bkg, lattice_out )
          call hinterp (   q_bkg,imb,jmb,    q_tmp,imo,jmo,lm  ,undefb, lattice_bkg, lattice_out )
          call hinterp (  o3_bkg,imb,jmb,   o3_tmp,imo,jmo,lm  ,undefb, lattice_bkg, lattice_out )
          call hinterp (  ts_bkg,imb,jmb,   ts_tmp,imo,jmo,1   ,undefb, lattice_bkg, lattice_out )
      endif

          rewind (10)
          call writit  (   u_tmp,imo,jmo,lm  ,10,lattice_out )
          call writit  (   v_tmp,imo,jmo,lm  ,10,lattice_out )
          call writit  (   t_tmp,imo,jmo,lm  ,10,lattice_out )
          call writit  ( ple_tmp,imo,jmo,lm+1,10,lattice_out )
          call writit  (   q_tmp,imo,jmo,lm  ,10,lattice_out )
          call writit  (  o3_tmp,imo,jmo,lm  ,10,lattice_out )
          call writit  (  ts_tmp,imo,jmo,1   ,10,lattice_out )
          close  (10)

      endif

      call timeend (' write_iau')

! **********************************************************************
! ****                Write AGCM_INTERNAL_RST (BIAS File)           ****
! **********************************************************************

      if( trim(biasin).ne.'xxx' ) then
      biasout = 'agcm_internal_rst'
      call timebeg (' writebias')
      if( myid.eq.0 ) then
          print *
          print *, 'Writing AGCM_INTERNAL_RST (BIAS File): ',trim(biasout)
      endif
      open   (10,file=trim(biasout),form='unformatted',access='sequential')

      rewind (10)
      call writit  (  ub_bkg,imb,jmb,lm  ,10,lattice_bkg )
      call writit  (  vb_bkg,imb,jmb,lm  ,10,lattice_bkg )
      call writit  (  tb_bkg,imb,jmb,lm  ,10,lattice_bkg )
      call writit  (  pb_bkg,imb,jmb,lm+1,10,lattice_bkg )
      call writit  (  qb_bkg,imb,jmb,lm  ,10,lattice_bkg )
      call writit  ( o3b_bkg,imb,jmb,lm  ,10,lattice_bkg )
      call writit  ( tsb_bkg,imb,jmb,1   ,10,lattice_bkg )
      close  (10)

      call timeend (' writebias')
      endif

! **********************************************************************
! ****                     Write Timing Information                 ****
! **********************************************************************

                      call timeend ('main')
      if( myid.eq.0 ) call timepri (6)

      if( myid.eq.0 ) then
          close(999)
          open (999,file='IAU_EGRESS',form='formatted')
          close(999)
      end if

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
      endif
      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo(1,1,L),q(1,1,L),lattice )
      enddo
      call timeend ('   Scatter')
      deallocate ( glo )
      return
      end

      subroutine readit ( q,im,jm,lm,ku,lattice )
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
         if( lattice%myid.eq.0 ) then
             read(ku) a
                glo = a
         endif
         call timebeg ('   Scatter')
         call scatter_2d ( glo,q(1,1,lm-L+1),lattice )
         call timeend ('   Scatter')
      enddo
      deallocate ( glo )
      deallocate ( a   )
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
      deallocate ( glo )
      deallocate ( a   )
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

      subroutine usage(myid)
      integer ierror
      if(myid.eq.0) then
      print *, "Usage:  "
      print *
      print *, " makeiau.x  -bkg        bkgeta_fname  "
      print *, "            -ana        anaeta_fname  "
      print *, "            -iau        iaueta_fname  "
      print *, "            -nymd           yyyymmdd  "
      print *, "            -nhms             hhmmss  "
      print *
      print *, "            -divr     (optional flag to Minimize Relative Adjustment "
      print *, "                       of Vertically Integrated Mass Divergence Increment)"
      print *, "            -diva     (optional flag to Minimize Absolute Adjustment "
      print *, "                       of Vertically Integrated Mass Divergence Increment)"
      print *, "            -damp     (optional flag to Damp Final Increments above 1 mb)"
      print *
      print *, "            -imout    (optional zonal      dimension output parameter"
      print *, "                                DEFAULT: bkg rslv)"
      print *, "            -jmout    (optional meridional dimension output parameter"
      print *, "                                DEFAULT: bkg rslv)"
      print *
      endif

      call my_finalize
      stop
      end

      subroutine dtoa_winds ( ud,vd,ua,va,im,jm,lm )

C ******************************************************************
C ****                                                          ****
C ****  This program converts 'D' gridded winds                 ****
C ****                     to 'A' gridded winds                 ****
C ****  using simple averaging.                                 ****
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
C ******************************************************************

      real ua(im,jm,lm), ud(im,jm,lm)
      real va(im,jm,lm), vd(im,jm,lm)

      real uz(im,jm)
      real vz(im,jm)
      real sinx(im/2)
      real cosx(im/2)

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im

      do i=1,imh
      sinx(i) = sin( -pi + (i-1)*dx )
      cosx(i) = cos( -pi + (i-1)*dx )
      enddo

      do L=1,lm

      uz(:,:) = ud(:,:,L)
      vz(:,:) = vd(:,:,L)

C *********************************************************
C ****              Average D-Grid Winds               ****
C *********************************************************

      do j=2,jm-1
         i=im
      do ip1=1,im
         ua(i,j,L) = ( uz(i,j)+uz(i,j+1) )*0.5
         va(i,j,L) = ( vz(i,j)+vz(ip1,j) )*0.5
         i=ip1
      enddo
      enddo

C *********************************************************
C ****                Fix A-Grid Pole Winds            ****
C *********************************************************

      do m=1,2
             n = (-1)**m
         jpole = 1 + (m-1)*(jm-1)
         jstar = 2 + (m-1)*(jm-3)

         upole = 0.0
         vpole = 0.0
         do i=1,imh
         upole = upole +   ( ua(i+imh,jstar,L)-ua(i,jstar,L) )*sinx(i)
     .                 + n*( va(i+imh,jstar,L)-va(i,jstar,L) )*cosx(i)
         vpole = vpole - n*( ua(i+imh,jstar,L)-ua(i,jstar,L) )*cosx(i)
     .                 +   ( va(i+imh,jstar,L)-va(i,jstar,L) )*sinx(i)
         enddo
         upole = upole / im
         vpole = vpole / im
         do i=1,imh
         ua(i    ,jpole,L) = -   upole*sinx(i) + n*vpole*cosx(i)
         va(i    ,jpole,L) = - n*upole*cosx(i) -   vpole*sinx(i)
         ua(i+imh,jpole,L) = - ua(i,jpole,L)
         va(i+imh,jpole,L) = - va(i,jpole,L)
         enddo
      enddo

C *********************************************************
C ****                   End Level Loop                ****
C *********************************************************

      enddo

      return
      end

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

      real,allocatable :: qax(:,:)
      real,allocatable ::  cx(:,:)
      real,allocatable :: qay(:,:)
      real,allocatable ::  cy(:,:)

      real,allocatable ::  sinx(:)
      real,allocatable ::  cosx(:)
      real,allocatable ::  siny(:)
      real,allocatable ::  cosy(:)
      real,allocatable :: trigx(:)
      real,allocatable :: trigy(:)

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

      allocate( qax  (   im+2 ,lm) )
      allocate(  cx  (2*(im+2),lm) )
      allocate( qay  (   2*jm ,lm) )
      allocate(  cy  (2*(2*jm),lm) )

      allocate(    cosx(im/2) )
      allocate(    sinx(im/2) )
      allocate(    cosy(jm)   )
      allocate(    siny(jm)   )
      allocate(   trigx(3*(im+1)) )
      allocate(   trigy(3*(2*jm)) )

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

      deallocate( qax )
      deallocate(  cx )
      deallocate( qay )
      deallocate(  cy )

      deallocate( cosx  )
      deallocate( sinx  )
      deallocate( cosy  )
      deallocate( siny  )
      deallocate( trigx )
      deallocate( trigy )

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

      real,allocatable :: qax(:,:)
      real,allocatable ::  cx(:,:)
      real,allocatable :: qay(:,:)
      real,allocatable ::  cy(:,:)

      real,allocatable ::  sinx(:)
      real,allocatable ::  cosx(:)
      real,allocatable ::  siny(:)
      real,allocatable ::  cosy(:)
      real,allocatable :: trigx(:)
      real,allocatable :: trigy(:)

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

      allocate( qax  (   im+2 ,lm) )
      allocate(  cx  (2*(im+2),lm) )
      allocate( qay  (   2*jm ,lm) )
      allocate(  cy  (2*(2*jm),lm) )

      allocate(    cosx(im/2) )
      allocate(    sinx(im/2) )
      allocate(    cosy(jm)   )
      allocate(    siny(jm)   )
      allocate(   trigx(3*(im+1)) )
      allocate(   trigy(3*(2*jm)) )

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

      deallocate( qax )
      deallocate(  cx )
      deallocate( qay )
      deallocate(  cy )

      deallocate( cosx  )
      deallocate( sinx  )
      deallocate( cosy  )
      deallocate( siny  )
      deallocate( trigx )
      deallocate( trigy )

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

      subroutine bin ( qin,im_in,jm_in,qout,im_out,jm_out,lm,undef,msgn,lat_i,lat_o )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lat_i
      type ( dynamics_lattice_type ) lat_o

      include 'mpif.h'
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
      real, allocatable :: qbin ( :,: )

      integer index(lm),ierror
 
      call timebeg (' bin')

      comm = lat_i%comm
      myid = lat_i%myid
      npes = lat_i%nx * lat_i%ny

      imgi = lat_i%imglobal
      jmgi = lat_i%jmglobal
      imgo = lat_o%imglobal
      jmgo = lat_o%jmglobal

      allocate ( qbin(imax,jmax)     )
      allocate ( glo_i(imgi,jmgi,lm) )
      allocate ( glo_o(imgo,jmgo,lm) )

      call timebeg ('   Gather')
      do L=1,lm
      call gather_2d ( glo_i(1,1,L),qin(1,1,L),lat_i )
      enddo
      call timeend ('   Gather')
      call mpi_bcast ( glo_i,imgi*jmgi*lm,lat_i%mpi_rkind,0,comm,ierror )

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

      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( glo_o(1,1,L),imgo*jmgo,lat_o%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)

      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo_o(1,1,L),qout(1,1,L),lat_o )
      enddo
      call timeend ('   Scatter')

      deallocate ( qbin  )
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
      real    lonbeg,lonend,lat
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

      subroutine hinterp ( qin,iin,jin,qout,iout,jout,mlev,undef,lat_i,lat_o )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lat_i
      type ( dynamics_lattice_type ) lat_o
      include 'mpif.h'
      integer  comm,myid,npes

      integer    iin,jin,       iout,jout, mlev
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
      call mpi_bcast ( glo_i,imgi*jmgi*mlev,lat_i%mpi_rkind,0,comm,ierror )

      do L=1,mlev
      if( index(L).eq.myid ) then
          call interp_h ( glo_i(1,1,L),imgi,jmgi,1,dlam,dphi,
     .                    glo_o(1,1,L),imgo*jmgo,lons,lats,undef )
      endif
      enddo

      call mpi_barrier (comm,ierror)
      do L=1,mlev
      call mpi_bcast ( glo_o(1,1,L),imgo*jmgo,lat_o%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)

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
     .                      q_geo,irun,lon_geo,lat_geo,undef )
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
      integer im,jm,lm,irun

      real      q_geo(irun,lm)
      real    lon_geo(irun)
      real    lat_geo(irun)

      real    q_cmp(im,jm,lm)
      real     dlam(im)
      real     dphi(jm)

c Local Variables
c ---------------
      integer  i,j,l,m,n
      integer, allocatable       :: ip1(:), ip0(:), im1(:), im2(:)
      integer, allocatable       :: jp1(:), jp0(:), jm1(:), jm2(:)

      integer ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1
      integer ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2
      integer jm2_for_jm2, jp1_for_jp1

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

      real    ux, ap1, ap0, am1, am2
      real    uy, bp1, bp0, bm1, bm2

      real, allocatable :: lon_cmp(:)
      real, allocatable :: lat_cmp(:)
      real, allocatable ::   q_tmp(:)

      real    pi,d
      real    lam,lam_ip1,lam_ip0,lam_im1,lam_im2
      real    phi,phi_jp1,phi_jp0,phi_jm1,phi_jm2
      real    dl,dp,phi_np,lam_0
      real    lam_geo, lam_cmp
      real    phi_geo, phi_cmp
      real    undef
      integer im1_cmp,icmp
      integer jm1_cmp,jcmp

c Initialization
c --------------
      pi = 4.*atan(1.)
      dl = 2*pi/ im     ! Uniform Grid Delta Lambda
      dp =   pi/(jm-1)  ! Uniform Grid Delta Phi

      allocate ( lon_cmp(im) )
      allocate ( lat_cmp(jm) )
      allocate ( q_tmp(irun) )

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
     .    lat_geo(i).ge.lat_cmp(jm-1) ) then

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

      deallocate ( lon_cmp )
      deallocate ( lat_cmp )
      deallocate (   q_tmp )
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

      real       ak(lm+1)
      real       bk(lm+1)

c Local variables
c ---------------
      real, allocatable ::  ps     (:,:)
      real, allocatable ::  phi    (:,:,:)
      real, allocatable ::  pke    (:,:,:)
      real, allocatable ::  ple_out(:,:,:)
      real, allocatable ::  pke_out(:,:,:)

      real, allocatable ::     delp(:,:,:)
      real, allocatable ::    u_out(:,:,:)
      real, allocatable ::    v_out(:,:,:)
      real, allocatable ::  thv_out(:,:,:)
      real, allocatable ::    q_in (:,:,:,:)
      real, allocatable ::    q_out(:,:,:,:)

      real    kappa,cp,rgas,eps,rvap
      integer i,j,L,n

      kappa = 2.0/7.0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      cp    = rgas/kappa

      allocate(  ps     (im,jm)      )
      allocate(  phi    (im,jm,lm+1) )
      allocate(  pke    (im,jm,lm+1) )
      allocate(  ple_out(im,jm,lm+1) )
      allocate(  pke_out(im,jm,lm+1) )

      allocate(     delp(im,jm,lm)   )
      allocate(    u_out(im,jm,lm)   )
      allocate(    v_out(im,jm,lm)   )
      allocate(  thv_out(im,jm,lm)   )
      allocate(    q_in (im,jm,lm,2) )
      allocate(    q_out(im,jm,lm,2) )

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

      deallocate(  ps      )
      deallocate(  phi     )
      deallocate(  pke     )
      deallocate(  ple_out )
      deallocate(  pke_out )

      deallocate(     delp )
      deallocate(    u_out )
      deallocate(    v_out )
      deallocate(  thv_out )
      deallocate(    q_in  )
      deallocate(    q_out )

      return
      end
