      program  main
      use MAPL_ConstantsMod
      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****     Program to create fv restarts from Pressure Level Data   ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm
      real     pbelow,pabove,ptop
      real     rgas,eps,rvap

      real dum
      integer     niter,i0,j0
      parameter ( niter = 5 )

! GEOS Restart Variables
! ----------------------
      real*4, allocatable :: dum4(:,:)
      real*8, allocatable :: dum8(:,:)
      real*8, allocatable ::   ak(:)
      real*8, allocatable ::   bk(:)

      integer headr1(6)
      integer headr2(5)

      real, allocatable ::  phis(:,:)

c Set analysis, fvdas, date and time
c ----------------------------------
      character*2  clm,cnhms
      character*8  cnymd

      character*256 dynrst,  mstrst, prsdata, topog, tag, ext
      character*256 dynrst2, mstrst2

      real    :: phibg, phifg, thbr1, thbr2, delth, cp
      real    ::      kappa = 2.0/7.0
      real    ::       grav = 9.80

      integer    nymd,nhms
      integer    Lbeg,Lend

c fv restart variables and topography
c -----------------------------------
      real, allocatable ::   ps(:,:)
      real, allocatable ::   dp(:,:,:)
      real, allocatable ::   pl(:,:,:)
      real, allocatable ::  ple(:,:,:)
      real, allocatable ::   er(:,:,:)
      real, allocatable ::    u(:,:,:), ud(:,:,:)
      real, allocatable ::    v(:,:,:), vd(:,:,:)
      real, allocatable ::   tv(:,:,:)
      real, allocatable ::   th(:,:,:)
      real, allocatable ::  thv(:,:,:)
      real, allocatable ::  pke(:,:,:)
      real, allocatable ::  pk (:,:,:)
      real, allocatable ::    q(:,:,:)

      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      integer timinc
      real    undef

c Analysis variables
c ------------------
      real, allocatable :: phis_ana(:,:)
      real, allocatable ::  slp_ana(:,:)
      real, allocatable ::   ps_ana(:,:)
      real, allocatable ::    u_ana(:,:,:)
      real, allocatable ::    v_ana(:,:,:)
      real, allocatable ::    z_ana(:,:,:)
      real, allocatable ::   er_ana(:,:,:)
      real, allocatable ::    q_ana(:,:,:)
      real, allocatable ::   rh_ana(:,:,:)
      real, allocatable ::    p_ana(:,:,:)
      real, allocatable ::   dp_ana(:,:,:)
      real, allocatable ::   pl_ana(:,:,:)
      real, allocatable ::    t_ana(:,:,:)
      real, allocatable ::     t_ec(:,:,:)
      real, allocatable ::    h_ana(:,:,:)
      real, allocatable ::  ple_ana(:,:,:)
      real, allocatable ::    logp (:,:,:)
      real, allocatable ::    logpl(:,:,:)
      real, allocatable ::    qdum (:,:,:)
      integer id,rc
      integer imax,jmax
      integer nvars, ngatts, ntime

      character*120, allocatable :: arg(:)
      character*120  eta_fname
      character*120   rs_fname

      logical ::   agrid = .false.
      logical ::   dgrid = .false.
      logical :: u_agrid = .false.
      logical :: v_agrid = .false.
      logical :: u_dgrid = .false.
      logical :: v_dgrid = .false.
      logical ::  tvflag = .false.
      logical :: thvflag = .false.
      logical :: lwiflag = .false.

      logical recon
      logical ihavetv,agridw
      integer precision
      integer i,j,k,L,n,nargs,iargc

c Analysis Grads CTL File Variables
c ---------------------------------
      character*256  ctlfile,format
      integer imana,jmana,lmana

      character*256, pointer :: names (:)
      character*256, pointer :: descs (:)
      integer,       pointer :: lmvars(:)
      real,          pointer ::   levs(:)
      real,          pointer ::  plevs(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)

C **********************************************************************
C ****           Initialize Filenames, Methods, etc.                ****
C **********************************************************************

         i0 = 0
         j0 = 0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      recon = .true.

         pabove = 10.00   ! 10 mb
         pbelow = 30.00   ! 30 mb
      precision = 0       ! 32-bit
        ctlfile = 'xxx'
           nymd = -999
           nhms = -999

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-dyn'   ) dynrst = trim(arg(n+1))
             if( trim(arg(n)).eq.'-moist' ) mstrst = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ncep'  ) prsdata = trim(arg(n+1))
             if( trim(arg(n)).eq.'-topo'  ) topog  = trim(arg(n+1))
             if( trim(arg(n)).eq.'-tag'   )    tag = trim(arg(n+1))
             if( trim(arg(n)).eq.'-plow ' ) read(arg(n+1), * ) pbelow
             if( trim(arg(n)).eq.'-phigh' ) read(arg(n+1), * ) pabove
             if( trim(arg(n)).eq.'-nymd'  ) read(arg(n+1), * ) nymd
             if( trim(arg(n)).eq.'-nhms'  ) read(arg(n+1), * ) nhms
             if( trim(arg(n)).eq.'-i0'    ) read(arg(n+1), * ) i0
             if( trim(arg(n)).eq.'-j0'    ) read(arg(n+1), * ) j0
             if( trim(arg(n)).eq.'-recon' ) read(arg(n+1), * ) recon
          enddo
          if( pbelow.lt.pabove ) then
                 dum = pbelow
              pbelow = pabove
              pabove = dum
          endif
      endif
      pabove = pabove*100
      pbelow = pbelow*100

      if( trim(tag).ne.'' ) tag = trim(tag) // '.'
      ext = 'nc4'

! **********************************************************************
! ****                   Read dycore internal Restart               ****
! **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

      if( nymd.eq.-999 ) nymd = headr1(1)*10000 + headr1(2)*100 + headr1(3)
      if( nhms.eq.-999 ) nhms = headr1(4)*10000 + headr1(5)*100 + headr1(6)

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

          allocate ( dum8(im,jm)       )
          allocate (    u(im,jm,lm)    )
          allocate (   ud(im,jm,lm)    )
          allocate (    v(im,jm,lm)    )
          allocate (   vd(im,jm,lm)    )
          allocate (   th(im,jm,lm)    )
          allocate (  thv(im,jm,lm)    )
          allocate (   dp(im,jm,lm)    )
          allocate (   pk(im,jm,lm)    )
          allocate (  ple(im,jm,lm+1)  )
          allocate (  pke(im,jm,lm+1)  )
          allocate (   ps(im,jm)       )
          allocate (   ak(lm+1)        )
          allocate (   bk(lm+1)        )

          read (10) ak
          read (10) bk

          do L=1,lm
             read(10) dum8 ;  ud(:,:,L) = dum8
          enddo
          do L=1,lm
             read(10) dum8 ;  vd(:,:,L) = dum8
          enddo
          do L=1,lm
             read(10) dum8 ;  th(:,:,L) = dum8 ! Note: GEOS-5 variable is DRY potential temperature
          enddo
          do L=1,lm+1
             read(10) dum8 ; ple(:,:,L) = dum8
          enddo
          do L=1,lm
             read(10) dum8 ;  pk(:,:,L) = dum8
          enddo

          close (10)

          call dtoa_winds ( ud,vd,u,v,im,jm,lm )

! Construct Pressure Variables
! ----------------------------

      ps(:,:)   = ple(:,:,lm+1)
      do L=lm,1,-1
      dp(:,:,L) =  ple(:,:,L+1)-ple(:,:,L)
      pl(:,:,L) = (ple(:,:,L+1)+ple(:,:,L))*0.5
      enddo

c     call set_eta ( lm,ptop,ak,bk )
c     do L=1,lm+1
c     ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
c     enddo
c     do L=1,lm
c     pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
c    .          / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
c     enddo

! **********************************************************************
! ****                   Read moist internal Restart                ****
! **********************************************************************

      allocate (    q(im,jm,lm) )
      allocate ( dum4(im,jm)    )

      open  (10,file=trim(mstrst),form='unformatted',access='sequential')
      do L=1,lm
         read(10)   dum4
         q(:,:,L) = dum4(:,:)  ! First moist variable is SPHU
      enddo
      close (10)

! Construct THV for REMAPPING
! ---------------------------
      thv = th*(1+eps*q)

! **********************************************************************
! ****                    Read Topography Dataset                   ****
! **********************************************************************

      allocate ( phis(im,jm) )

      print *, 'Reading Topography Dataset: ',trim(topog)
      open (10,file=trim(topog),form='unformatted',access='sequential')
      read (10) phis
      close(10)

      phis = phis*grav
#ifdef DEBUG
       call writit ( phis,im,jm,1,65 )
#endif

C **********************************************************************
C ****                       Read ANA MetaData                      ****
C **********************************************************************

      call gfio_open ( trim(prsdata),1,id,rc )
      call gfio_diminquire ( id,imana,jmana,lmana,ntime,nvars,ngatts,rc )

      allocate ( lon(imana) )
      allocate ( lat(jmana) )
      allocate ( lev(lmana) )
      allocate ( yymmdd(ntime) )
      allocate ( hhmmss(ntime) )
      allocate (  vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate (  kmvar(nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )

      call gfio_inquire ( id,imana,jmana,lmana,ntime,nvars,
     .                    title,source,contact,undef,
     .                    lon,lat,lev,levunits,
     .                    yymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    vrange,prange,rc )

          allocate ( plevs(lmana) )
          do L=1,lmana
          plevs(L) = lev(lmana-L+1)
          enddo

          print *
          print *, '     GEOS Resolution: ',im,jm,lm
          print *, '                nymd: ',nymd
          print *, '                nhms: ',nhms
          print *
          print *, '            ANA File: ',trim(prsdata)
          print *, '                rslv: ',imana,jmana,lmana
          print *, '              lon(1): ',lon(1)*180.0/3.14159
          print *, '   Reconcile Heights: ',recon
          print *
          print *, ' Number of Variables: ',nvars
          print *
          do n=1,nvars
          write(6,1001) n,trim(vname(n)),trim(vtitle(n)),kmvar(n)
          enddo
 1001     format(1x,i2,3x,a16,2x,a32,2x,i3)
          print *
                                            L = 1
          print *, '     Pressure Levels: ',L,plevs(L)
          do L=2,lmana
          print *, '                      ',L,plevs(L)
          enddo
          print *
          print *, ' Blending between: ',pbelow/100,' and ',pabove/100,' mb'
          print *

      write( cnymd,200 ) nymd
      write( cnhms,300 ) nhms/10000
  200 format(i8.8)
  300 format(i2.2)
  400 format('dset ^',a)
  600 format(a1,i2.2)

C **********************************************************************
C ****                           Get Analysis                       ****
C **********************************************************************

      allocate (   p_ana(im,jm,lmana) )
      allocate (  er_ana(im,jm,lmana) )
      allocate (   z_ana(im,jm,lmana) )
      allocate (   u_ana(im,jm,lmana) )
      allocate (   v_ana(im,jm,lmana) )
      allocate (   t_ana(im,jm,lmana) )
      allocate (   t_ec (im,jm,lmana) )
      allocate (   h_ana(im,jm,lmana) )
      allocate (   q_ana(im,jm,lmana) )
      allocate (  ps_ana(im,jm) )
      allocate (phis_ana(im,jm) )

      print *, 'Reading Analysis for Date: ',nymd,' Time: ',nhms
      print *

          call get_ana_data  ( id,ps_ana,u_ana,v_ana,t_ana,q_ana,h_ana,phis_ana,
     .                         im,jm,lmana,nymd,nhms,lon(1),
     .                         imana,jmana,lmana,nvars,names,lmvars,undef,plevs )
          t_ec = t_ana

            allocate(  dp_ana(im,jm,lm)   )
            allocate(  pl_ana(im,jm,lm)   )
            allocate( ple_ana(im,jm,lm+1) )
            allocate(   logp (im,jm,lm)   )
            allocate(   logpl(im,jm,lm)   )
          do L=1,lm+1
          ple_ana(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
          enddo
          do L=1,lm
           dp_ana(:,:,L) = ple_ana(:,:,L+1)-ple_ana(:,:,L)
           pl_ana(:,:,L) =      0.5*(ple_ana(:,:,L+1)+ple_ana(:,:,L))
             logp(:,:,L) = log( 0.5*(ple_ana(:,:,L+1)+ple_ana(:,:,L)) )
          enddo

          if( i0.ne.0 .and. j0.ne.0 ) then
              print *, 'Sample ANA Data at GEOS-5 Location: (',i0,',',j0,')'
              print *, '   ANA_PS: ',ps_ana(i0,j0)/100,'  ANA_PHIS: ',phis_ana(i0,j0)
              print *, '   ANA_UNDEF: ',undef
              print *, '   ANA Temperature and Wind Profile:'
          else
              print *, 'Sample ANA Data:'
              print *, '   ANA_PS: ',ps_ana(1,jm/2)/100,'  ANA_PHIS: ',phis_ana(1,jm/2)
              print *, '   ANA_UNDEF: ',undef
              print *, '   ANA_Temperature and Wind Profile:'
          endif
          do L=1,lmana
          p_ana(:,:,L) =      100.0*plevs(L)
          logpl(:,:,L) = log( 100.0*plevs(L) )
          if( i0.ne.0 .and. j0.ne.0 ) then
              print *, L,plevs(L),t_ana(i0,j0,L),u_ana(i0,j0,L)
          else
              print *, L,plevs(L),t_ana(1,jm/2,L),u_ana(1,jm/2,L)
          endif
          enddo
          print *

          allocate    ( qdum(im,jm,lm) )

          call interp ( qdum,u_ana,logp,logpl,p_ana,pl_ana,ple_ana,im,jm,lm,lmana,undef,'UWND',niter,i0,j0,1 )
          deallocate  ( u_ana )
            allocate  ( u_ana(im,jm,lm) )
                        u_ana = qdum

          call interp ( qdum,v_ana,logp,logpl,p_ana,pl_ana,ple_ana,im,jm,lm,lmana,undef,'VWND',niter,i0,j0,1 )
          deallocate  ( v_ana )
            allocate  ( v_ana(im,jm,lm) )
                        v_ana = qdum

          call interp ( qdum,t_ana,logp,logpl,p_ana,pl_ana,ple_ana,im,jm,lm,lmana,undef,'TMPU',niter,i0,j0,1 )
          deallocate  ( t_ana )
            allocate  ( t_ana(im,jm,lm) )
                        t_ana = qdum

          call interp ( qdum,q_ana,logp,logpl,p_ana,pl_ana,ple_ana,im,jm,lm,lmana,undef,'SPHU',niter,i0,j0,-1 )
          deallocate  ( q_ana )
            allocate  ( q_ana(im,jm,lm) )
                        q_ana = qdum

C **********************************************************************
C ****                      Remap for Analysis                      ****
C **********************************************************************

      if( i0.ne.0 .and. j0.ne.0 ) then
          print *
          print *, 'Before REMAP: '
          print *, 'GEOS5 PHIS/grav: ',phis(i0,j0)/grav,' ps: ',ps(i0,j0)/100
          print *, 'ANA   PHIS/grav: ',phis_ana(i0,j0)/grav,' ps: ',ps_ana(i0,j0)/100
          print *
      endif

      print *, 'Calling Remap'
      call remap  ( ps,dp,u,v,thv,q,phis,lm,
     .              ps_ana,dp_ana,u_ana,v_ana,t_ana,q_ana,phis_ana,lm,im,jm,1,pbelow,pabove )
      print *, '   Fini Remap'

      if( i0.ne.0 .and. j0.ne.0 ) then
          print *
          print *, 'AFter  REMAP: '
          print *, 'GEOS5 PHIS/grav: ',phis(i0,j0)/grav,' ps: ',ps(i0,j0)/100
          print *, 'ANA   PHIS/grav: ',phis_ana(i0,j0)/grav,' ps: ',ps_ana(i0,j0)/100
          print *
      endif

C **********************************************************************
C ****                      Reconcile Heights                       ****
C **********************************************************************

      cp = MAPL_CP
      do L=1,lm+1
      ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
      enddo
      pke(:,:,:) = ple(:,:,:)**kappa
      do L=1,lm
      pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
     .          / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo

      if( recon ) then
      do j=1,jm
      do i=1,im

      Lbeg  = lm
      phifg = phis(i,j)
      phibg = phifg

      do k=lmana,1,-1
          if( i.eq.i0 .and. j.eq.j0 ) then
              write(6,5001) k,p_ana(i,j,k)/100,h_ana(i,j,k),t_ec(i,j,k)
 5001         format(1x,'k: ',i3,3x,'ANA_PMAN: ',f8.3,3x,'ANA_HGHT: ',f9.3,3x,'ANA_TMPU: ',f7.3)
          endif

      if( p_ana(i,j,k).lt.ps(i,j)                 .and.  ! p_ana is above GEOS Surface Pressure 
     .    h_ana(i,j,k)-phibg/grav.gt.10.0         .and.  ! h_ana is at least 10-meters above previous level
     .    h_ana(i,j,k)-phis_ana(i,j)/grav.gt.10.0 .and.  ! h_ana is at least 10-meters above Topography
     .    p_ana(i,j,k).gt.pabove ) then                  ! p_ana is below Blending Region
          if( i.eq.i0 .and. j.eq.j0 ) print *

          do L=Lbeg,1,-1
          if( ple(i,j,L).gt.p_ana(i,j,k) ) then
              phifg = phifg + cp*thv(i,j,L)*( pke(i,j,L+1)-pke(i,j,L) )
              if( i.eq.i0 .and. j.eq.j0 ) then
              write(6,5002) L,ple(i,j,L)/100,phifg/grav,thv(i,j,L)*pk(i,j,L)
 5002         format(1x,'L: ',i3,3x,' G5_PLE: ',f8.3,3x,'G5_HGHT: ',f9.3,3x,'G5_TMPU: ',f7.3)
              endif
          else
              exit
          endif
          enddo
          Lend = L
          if( Lbeg-Lend.le.2 ) then
              phifg = phibg
              cycle
          endif

              phifg =   phifg + cp*thv(i,j,Lend)*( pke(i,j,Lend+1)-p_ana(i,j,k)**kappa )

              if( i.eq.i0 .and. j.eq.j0 ) then
                  print *
                  print *, '   Lbeg: ',Lbeg,' Lend: ',Lend,' ple(Lend): ',ple(i,j,Lend)/100
                  print *, 'ANA_HGHT: ',h_ana(i,j,k),' G5_HGHT: ',phifg/grav,' G5_HGHT0: ',phibg/grav
                  print *, 'ANA_TMPU: ',t_ec (i,j,k),' G5_TMPU: ',thv(i,j,Lend)*pk(i,j,LEND)
              endif

              thbr1 = ( grav*h_ana(i,j,k)-phibg )/( pke(i,j,Lbeg+1)-p_ana(i,j,k)**kappa )/cp
              thbr2 = ( phifg            -phibg )/( pke(i,j,Lbeg+1)-p_ana(i,j,k)**kappa )/cp
              delth = thbr1-thbr2

              if( i.eq.i0 .and. j.eq.j0 ) then
                  print *, 'ANA_THETA_BR: ',thbr1,' G5_THETA_BR: ',thbr2
                  print *, '   ANA_T_TOP: ',thbr1*p_ana(i,j,k)**kappa,' G5_T_TOP: ',thbr2*p_ana(i,j,k)**kappa
                  print *, '   ANA_T_BOT: ',thbr1*pke(i,j,Lbeg+1),' G5_T_BOT: ',thbr2*pke(i,j,Lbeg+1)
              endif

              do L=Lbeg,Lend,-1
              thv(i,j,L) = thv(i,j,L) + delth
              enddo

              phifg = phibg
              do L=Lbeg,Lend+1,-1
              phifg = phifg + cp*thv(i,j,L)*( pke(i,j,L+1)-pke(i,j,L) )
              enddo

              if( i.eq.i0 .and. j.eq.j0 ) print *, 'ANA_HGHT: ',h_ana(i,j,k),'  G5_HGHT: ',
     .               (phifg + cp*thv(i,j,Lend)*( pke(i,j,Lend+1)-p_ana(i,j,k)**kappa ))/grav

              phifg =   phifg + cp*thv(i,j,Lend)*( pke(i,j,Lend+1)-pke(i,j,Lend) )
          Lbeg  = Lend-1
          phibg = phifg
      endif
      enddo

      enddo
      enddo
      endif

! **********************************************************************
! ****                 Write dycore internal Restart                ****
! **********************************************************************

      call atod_winds ( u,v,ud,vd,im,jm,lm )

      th = thv/(1+eps*q)

      dynrst2 = trim(dynrst) // '.ncep'
      print *
      print *, 'Creating GEOS-5 fvcore_internal_restart'

      open (20,file=trim(dynrst2),form='unformatted',access='sequential')
      write(20) headr1
      write(20) headr2
      write(20) ak
      write(20) bk
          do L=1,lm
                       dum8(:,:) = ud(:,:,L)
             write(20) dum8
          enddo
          do L=1,lm
                       dum8(:,:) = vd(:,:,L)
             write(20) dum8
          enddo
          do L=1,lm
                       dum8(:,:) = th(:,:,L)
             write(20) dum8
          enddo
          do L=1,lm+1
                       dum8(:,:) = ple(:,:,L)
             write(20) dum8
          enddo
          do L=1,lm
                       dum8(:,:) = pk(:,:,L)
             write(20) dum8
          enddo
      close (20)

! **********************************************************************
! ****                  Write Moist Internal Restart                ****
! **********************************************************************

      mstrst2 = trim(mstrst) // '.ncep'

      open  (10,file=trim(mstrst) ,form='unformatted',access='sequential')
      open  (20,file=trim(mstrst2),form='unformatted',access='sequential')

      print *, 'Creating GEOS-5  moist_internal_restart'
      do L=1,lm
         read(10)  dum4
                   dum4 = q(:,:,L) ! First moist variable is SPHU
         write(20) dum4
      enddo

               rc =  0
      dowhile (rc.eq.0)
         read (10,iostat=rc)  dum4
      if( rc.eq.0 ) write(20) dum4
      enddo

      stop
      end

      subroutine interp ( q,qana,logp,logpl,pana,pl,ple,im,jm,lm,lmana,undef,name,niter,i0,j0,flag )
      implicit none
      integer im,jm,lm,lmana,niter,i0,j0,flag
      real undef
      real    q (im,jm,lm)
      real   pl (im,jm,lm)
      real  ple (im,jm,lm+1)
      real   er (im,jm,lm)
      real logp (im,jm,lm)
      real pana (im,jm,lmana)
      real qana (im,jm,lmana)
      real zana (im,jm,lmana)
      real erana(im,jm,lmana)
      real logpl(im,jm,lmana)
      character*8 name

      integer i,j,L,n

c Interpolate Analysis to GEOS Model Levels
c -----------------------------------------
          do L=1,lm
          do j=1,jm
          do i=1,im
          call  sigtopl( q(i,j,L),qana(i,j,:),logpl(i,j,:),logp(i,j,L),1,1,lmana,undef )
          enddo
          enddo
          enddo
          if( flag.eq.-1 ) then
              q = max( q,0.0 )
          endif

          if( i0.ne.0 .and. j0.ne.0 ) then
          print *
          print *, 'Initial ANA ',trim(name),' Profile at GEOS-5 Levels:'
          do L=1,lm
              print *, L,exp(logp(i0,j0,L))/100.,q(i0,j0,L)
          enddo
          print *
          else
          print *, 'Interpolating ',trim(name),' ...'
          endif

#ifdef DEBUG
          call writit (q,im,jm,lm,66)
#endif
          do n=1,niter
c            Interpolate GEOS Model Back to EC Levels and Compute Error
c            ----------------------------------------------------------
             do L=1,lmana
             do j=1,jm
             do i=1,im
                call  sigtopl( zana(i,j,L),q(i,j,:),logp(i,j,:),logpl(i,j,L),1,1,lm,undef )
                erana(i,j,L) = zana(i,j,L)-qana(i,j,L)
             enddo
             enddo
             enddo
             if( i0.ne.0 .and. j0.ne.0 ) then
             print *
             print *, 'ANA ',trim(name),' Profile Comparison, ITER: ',n
             print *, '----------------------------------------------'
             do L=1,lmana
                 print *, L,exp(logpl(i0,j0,L))/100.,zana(i0,j0,L),qana(i0,j0,L),erana(i0,j0,L)
             enddo
             print *
             endif

c            Interpolate and Add Error to GEOS Model Levels
c            ----------------------------------------------
             call interp3 ( erana,pana,im,jm,lmana, er,pl,lm,ple(1,1,lm+1) )
             q = q - er
             if( flag.eq.-1 ) then
                 q = max( q,0.0 )
             endif
#ifdef DEBUG
             call writit (q,im,jm,lm,66)
#endif
          enddo

          if( i0.ne.0 .and. j0.ne.0 ) then
          print *
          print *, 'Final ANA ',trim(name),' Profile at GEOS-5 Levels:'
          do L=1,lm
              print *, L,exp(logp(i0,j0,L))/100.,q(i0,j0,L)
          enddo
          print *
          endif

      return
      end

      subroutine get_ana_data  ( id,ps,u,v,t,q,h,phis,
     .                           im,jm,lm,nymd,nhms,lonbeg,
     .                           imana,jmana,lmana,nvars,names,lmvars,undef,plevs )
      use MAPL_ConstantsMod
      implicit none
      integer  id,im,jm,lm,nymd,nhms,rc
      integer  imana,jmana,lmana
      integer              nvars
      integer       lmvars(nvars)
      character*256  names(nvars)
      character*256  filename, format

      real     lonbeg
      real     ps(im,jm)
      real      u(im,jm,lm)
      real      v(im,jm,lm)
      real      t(im,jm,lm)
      real      h(im,jm,lm)
      real     rh(im,jm,lm)
      real      q(im,jm,lm)
      real   phis(im,jm)
      real  plevs(lm)
      real   phis2(im,jm)
      real     slp(im,jm)
      real     thv(im,jm)

      real, allocatable ::  vor  (:,:)
      real, allocatable ::  div  (:,:)
      real, allocatable ::  chi  (:,:)
      real, allocatable ::  psi  (:,:)
      real, allocatable :: dchidx(:,:)
      real, allocatable :: dchidy(:,:)
      real, allocatable :: dpsidx(:,:)
      real, allocatable :: dpsidy(:,:)

      real, allocatable :: dum2d(:,:)
      real, allocatable :: dum3d(:,:,:)
      real, allocatable :: dumu (:,:,:)
      real, allocatable :: dumv (:,:,:)

      real    undef,kappa,grav,dum,beta,cp,rgas,gamma,dp
      integer L,i,j,n,LM1

      allocate (   vor  (imana,jmana)     )
      allocate (   div  (imana,jmana)     )
      allocate (   chi  (imana,jmana)     )
      allocate (   psi  (imana,jmana)     )
      allocate (  dchidx(imana,jmana)     )
      allocate (  dchidy(imana,jmana)     )
      allocate (  dpsidx(imana,jmana)     )
      allocate (  dpsidy(imana,jmana)     )

      allocate ( dum2d(imana,jmana)       )
      allocate ( dum3d(imana,jmana,lmana) )
      allocate ( dumu (imana,jmana,lmana) )
      allocate ( dumv (imana,jmana,lmana) )

      rgas  = MAPL_RGAS
      kappa = MAPL_KAPPA
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      beta  = 6.5e-3

c Read ANA Variables
c ------------------
      call gfio_getvar ( id,'slp',nymd,nhms,imana,jmana,0,1 ,dum2d,rc )
      if( lonbeg.eq.0.0 ) call hflip( dum2d,imana,jmana,1 )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dum2d,imana,jmana,slp,im,jm,1,undef )
      else
              slp = dum2d
      endif

      call gfio_getvar     ( id,'ps',nymd,nhms,imana,jmana,0,1 ,dum2d ,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP Surface Pressure variable'
          call exit(7)
      endif
      if( lonbeg.eq.0.0 ) call hflip( dum2d,imana,jmana,1 )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dum2d,imana,jmana,ps,im,jm,1,undef )
      else
              ps = dum2d
      endif

      call gfio_getvar ( id,'hght',nymd,nhms,imana,jmana,1,lmana,dum3d,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP Height variable'
          call exit(7)
      endif
      if( lonbeg.eq.0.0 ) call hflip( dum3d,imana,jmana,lmana )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dum3d,imana,jmana,h,im,jm,lm,undef )
      else
              h = dum3d
      endif

c Winds
c -----
      call gfio_getvar ( id,'uwnd',nymd,nhms,imana,jmana,1,lmana,dumu,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP U-Wind variable'
          call exit(7)
      endif
      call gfio_getvar ( id,'vwnd',nymd,nhms,imana,jmana,1,lmana,dumv,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP V-Wind variable'
          call exit(7)
      endif

#if 0
      do L=1,lmana
         call getvordiv( dumu(1,1,L),dumv(1,1,L),vor,div,imana,jmana )
         call laplacian( vor,psi,imana,jmana )
         call laplacian( div,chi,imana,jmana )
         call gradq    ( chi,dchidx,dchidy,imana,jmana )
         call gradq    ( psi,dpsidx,dpsidy,imana,jmana )
         dumu(:,:,L) = dchidx - dpsidy
         dumv(:,:,L) = dpsidx + dchidy
      enddo
#endif

      if( lonbeg.eq.0.0 ) call hflip( dumu,imana,jmana,lmana )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dumu,imana,jmana,u,im,jm,lm,undef )
      else
              u = dumu
      endif

      if( lonbeg.eq.0.0 ) call hflip( dumv,imana,jmana,lmana )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dumv,imana,jmana,v,im,jm,lm,undef )
      else
              v = dumv
      endif

c Temperature
c -----------
      call gfio_getvar ( id,'tmpu',nymd,nhms,imana,jmana,1,lmana,dum3d,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP Temperature variable'
          call exit(7)
      endif
      if( lonbeg.eq.0.0 ) call hflip( dum3d,imana,jmana,lmana )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dum3d,imana,jmana,t,im,jm,lm,undef )
      else
              t = dum3d
      endif

c Relative Humidity
c -----------------
      call gfio_getvar ( id,'rh',nymd,nhms,imana,jmana,1,lmana,dum3d,rc )
      if( rc.ne.0 ) then
          print *, 'Could not find NCEP Rel.Hum. variable'
          call exit(7)
      endif
      if( lonbeg.eq.0.0 ) call hflip( dum3d,imana,jmana,lmana )
      if( im.ne.imana .or. jm.ne.jmana ) then
              call hinterp ( dum3d,imana,jmana,rh,im,jm,lm,undef )
      else
              rh = dum3d
      endif
      where( rh.ne.undef )
             rh = min( max( rh,0.0 ),100.0 )
      elsewhere
             rh = 20.0
      endwhere

c     do n=1,nvars
c         if( lmvars(n).gt.1 .and. mlev.gt.lmana ) then
c             do L=1,mlev-lmana
c             qdum(:,:,L) = qdum(:,:,mlev-lmana+1)
c             enddo
c         endif
c     enddo

c Compute PHIS
c ------------
      do j=1,jm
      do i=1,im
         L=1
         do while( L.lt.lm .and. plevs(L).lt.ps(i,j)/100.0 )
         L=L+1
         enddo
         LM1 = L-1
         phis(i,j) = h(i,j,L) - ( h(i,j,L)-h(i,j,LM1) )*log( 100*plevs(L)/ps(i,j) )/log( plevs(L)/plevs(LM1) )
       enddo
       enddo
       phis = phis*grav
#ifdef DEBUG
       call writit ( phis,im,jm,1,65 )
#endif

      do j=1,jm
      do i=1,im
         L=lm
         do while( L.gt.1 .and. plevs(L).gt.ps(i,j)/100.0 )
         L=L-1
         enddo
         dp = ps(i,j)/100.0 - plevs(L)
         do while( dp.le.150.0 )
         L=L-1
         dp = ps(i,j)/100.0 - plevs(L)
         enddo
         L=L+1
         gamma = kappa * (0.01*ps(i,j))**kappa * log( slp(i,j)/ps(i,j) )
     .            / ( 1.0 - 0.5*beta*rgas/grav * log( slp(i,j)/ps(i,j) ) )
         phis2(i,j) = grav*h(i,j,L)*gamma / ( gamma - plevs(L)**kappa + (0.01*ps(i,j))**kappa )
       enddo
       enddo
#ifdef DEBUG
       call writit ( phis2,im,jm,1,65 )
#endif

      do j=1,jm
      do i=1,im
         L=lm
         do while( L.gt.1 .and. plevs(L).gt.ps(i,j)/100.0 )
         L=L-1
         enddo
         gamma = rgas * log( slp(i,j)/ps(i,j) )
         phis2(i,j) = gamma * ( t(i,j,L)+beta*h(i,j,L) ) / ( 1+0.5*beta*gamma/grav )
       enddo
       enddo
#ifdef DEBUG
       call writit ( phis2,im,jm,1,65 )
#endif

c Load GMAO Variables
c -------------------
      do L=1,lm
      do j=1,jm
      do i=1,im
      call qsat (t(i,j,L),plevs(L),q(i,j,L),dum,.false.)
      q(i,j,L) = rh(i,j,L)*q(i,j,L)*0.01
      enddo
      enddo
      enddo
     
      return
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L
      real*4 z(im,jm,lm)
      real*4 q(im,jm,lm),dum(im)
      do L=1,lm
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j,L)
         dum(i+im/2) = q(i,j,L)
      enddo
         z(:,j,lm-L+1) = dum(:)
      enddo
      enddo
         q = z
      return
      end

      subroutine writit (q,im,jm,lm,ku)
      real   q (im,jm,lm)
      real*4 q2(im,jm)
      do L=lm,1,-1
      q2(:,:) = q(:,:,L)
      write(ku) q2
      enddo
      return
      end

      subroutine qsat (tt,p,q,dqdt,ldqdt)
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute Saturation Specific Humidity
C
C  INPUT:
C  ======
C    TT ......... Temperature (Kelvin)
C    P .......... Pressure (mb)
C    LDQDT ...... Logical Flag to compute QSAT Derivative
C
C  OUTPUT:
C  =======
C    Q .......... Saturation Specific Humidity
C    DQDT ....... Saturation Specific Humidity Derivative wrt Temperature
C
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      IMPLICIT NONE
      REAL TT, P, Q, DQDT
      LOGICAL LDQDT
      REAL AIRMW, H2OMW
      
      PARAMETER ( AIRMW  = 28.97      )                                         
      PARAMETER ( H2OMW  = 18.01      )                                         

      REAL ESFAC, ERFAC
      PARAMETER ( ESFAC = H2OMW/AIRMW       )
      PARAMETER ( ERFAC = (1.0-ESFAC)/ESFAC )

      real aw0, aw1, aw2, aw3, aw4, aw5, aw6
      real bw0, bw1, bw2, bw3, bw4, bw5, bw6
      real ai0, ai1, ai2, ai3, ai4, ai5, ai6
      real bi0, bi1, bi2, bi3, bi4, bi5, bi6

      real d0, d1, d2, d3, d4, d5, d6
      real e0, e1, e2, e3, e4, e5, e6
      real f0, f1, f2, f3, f4, f5, f6
      real g0, g1, g2, g3, g4, g5, g6

c ********************************************************
c ***  Polynomial Coefficients WRT Water (Lowe, 1977) ****
c ***              (Valid +50 C to -50 C)             ****
c ********************************************************

      parameter ( aw0 =  6.107799961e+00 * esfac )
      parameter ( aw1 =  4.436518521e-01 * esfac )
      parameter ( aw2 =  1.428945805e-02 * esfac )
      parameter ( aw3 =  2.650648471e-04 * esfac )
      parameter ( aw4 =  3.031240396e-06 * esfac )
      parameter ( aw5 =  2.034080948e-08 * esfac )
      parameter ( aw6 =  6.136820929e-11 * esfac )

      parameter ( bw0 = +4.438099984e-01 * esfac )
      parameter ( bw1 = +2.857002636e-02 * esfac )
      parameter ( bw2 = +7.938054040e-04 * esfac )
      parameter ( bw3 = +1.215215065e-05 * esfac )
      parameter ( bw4 = +1.036561403e-07 * esfac )
      parameter ( bw5 = +3.532421810e-10 * esfac )
      parameter ( bw6 = -7.090244804e-13 * esfac )


c ********************************************************
c ***   Polynomial Coefficients WRT Ice  (Lowe, 1977) ****
c ***              (Valid  +0 C to -50 C)             ****
c ********************************************************

      parameter ( ai0 = +6.109177956e+00 * esfac )
      parameter ( ai1 = +5.034698970e-01 * esfac )
      parameter ( ai2 = +1.886013408e-02 * esfac )
      parameter ( ai3 = +4.176223716e-04 * esfac )
      parameter ( ai4 = +5.824720280e-06 * esfac )
      parameter ( ai5 = +4.838803174e-08 * esfac )
      parameter ( ai6 = +1.838826904e-10 * esfac )

      parameter ( bi0 = +5.030305237e-01 * esfac )
      parameter ( bi1 = +3.773255020e-02 * esfac )
      parameter ( bi2 = +1.267995369e-03 * esfac )
      parameter ( bi3 = +2.477563108e-05 * esfac )
      parameter ( bi4 = +3.005693132e-07 * esfac )
      parameter ( bi5 = +2.158542548e-09 * esfac )
      parameter ( bi6 = +7.131097725e-12 * esfac )


c ********************************************************
c ***         Polynomial Coefficients WRT Ice         ****
c ***   Starr and Cox (1985) (Valid -40 C to -70 C)   ****
c ********************************************************


      parameter ( d0 = 0.535098336e+01 * esfac )
      parameter ( d1 = 0.401390832e+00 * esfac )
      parameter ( d2 = 0.129690326e-01 * esfac )
      parameter ( d3 = 0.230325039e-03 * esfac )
      parameter ( d4 = 0.236279781e-05 * esfac )
      parameter ( d5 = 0.132243858e-07 * esfac )
      parameter ( d6 = 0.314296723e-10 * esfac )

      parameter ( e0 = 0.469290530e+00 * esfac )
      parameter ( e1 = 0.333092511e-01 * esfac )
      parameter ( e2 = 0.102164528e-02 * esfac )
      parameter ( e3 = 0.172979242e-04 * esfac )
      parameter ( e4 = 0.170017544e-06 * esfac )
      parameter ( e5 = 0.916466531e-09 * esfac )
      parameter ( e6 = 0.210844486e-11 * esfac )


c ********************************************************
c ***         Polynomial Coefficients WRT Ice         ****
c ***   Starr and Cox (1985) (Valid -65 C to -95 C)   ****
c ********************************************************

      parameter ( f0 = 0.298152339e+01 * esfac )
      parameter ( f1 = 0.191372282e+00 * esfac )
      parameter ( f2 = 0.517609116e-02 * esfac )
      parameter ( f3 = 0.754129933e-04 * esfac )
      parameter ( f4 = 0.623439266e-06 * esfac )
      parameter ( f5 = 0.276961083e-08 * esfac )
      parameter ( f6 = 0.516000335e-11 * esfac )

      parameter ( g0 = 0.312654072e+00 * esfac )
      parameter ( g1 = 0.195789002e-01 * esfac )
      parameter ( g2 = 0.517837908e-03 * esfac )
      parameter ( g3 = 0.739410547e-05 * esfac )
      parameter ( g4 = 0.600331350e-07 * esfac )
      parameter ( g5 = 0.262430726e-09 * esfac )
      parameter ( g6 = 0.481960676e-12 * esfac )

      REAL        TMAX, TICE
      PARAMETER ( TMAX=323.15, TICE=273.16)
      
      REAL T, D, W, QX, DQX
      T = MIN(TT,TMAX) - TICE
      DQX = 0.
      QX  = 0.

c Fitting for temperatures above 0 degrees centigrade
c ---------------------------------------------------
      if(t.gt.0.) then
       qx = aw0+T*(aw1+T*(aw2+T*(aw3+T*(aw4+T*(aw5+T*aw6)))))
      if (ldqdt)  then
      dqx = bw0+T*(bw1+T*(bw2+T*(bw3+T*(bw4+T*(bw5+T*bw6)))))
      endif
      endif

c Fitting for temperatures between 0 and -40
c ------------------------------------------
      if( t.le.0. .and. t.gt.-40.0 ) then
        w = (40.0 + t)/40.0
       qx =     w *(aw0+T*(aw1+T*(aw2+T*(aw3+T*(aw4+T*(aw5+T*aw6))))))
     .    + (1.-w)*(ai0+T*(ai1+T*(ai2+T*(ai3+T*(ai4+T*(ai5+T*ai6))))))
      if (ldqdt)  then
      dqx =     w *(bw0+T*(bw1+T*(bw2+T*(bw3+T*(bw4+T*(bw5+T*bw6))))))
     .    + (1.-w)*(bi0+T*(bi1+T*(bi2+T*(bi3+T*(bi4+T*(bi5+T*bi6))))))
      endif
      endif

c Fitting for temperatures between -40 and -70
c --------------------------------------------
      if( t.le.-40.0 .and. t.ge.-70.0 ) then
       qx = d0+T*(d1+T*(d2+T*(d3+T*(d4+T*(d5+T*d6)))))
      if (ldqdt) then
      dqx = e0+T*(e1+T*(e2+T*(e3+T*(e4+T*(e5+T*e6)))))
      endif
      endif

c Fitting for temperatures less than -70
c --------------------------------------
      if(t.lt.-70.0) then
       qx = f0+t*(f1+t*(f2+t*(f3+t*(f4+t*(f5+t*f6)))))
      if (ldqdt) then
      dqx = g0+t*(g1+t*(g2+t*(g3+t*(g4+t*(g5+t*g6)))))
      endif
      endif

c Compute Saturation Specific Humidity
c ------------------------------------
      D = (P-ERFAC*QX)
      IF(D.LT.0.) THEN
       Q = 1.0
       IF (LDQDT)  DQDT = 0.
      ELSE
       D = 1.0 / D
       Q = MIN(QX * D,1.0)
       IF (LDQDT)  DQDT = (1.0 + ERFAC*Q) * D * DQX
      ENDIF
      RETURN
      END

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*abs(undef)
      return
      end

      subroutine getchar (name,num)
      character*2  num2
      character*3  num3
      integer      num
      character*1  junk(256)
      character*1  name(256)
      data junk /256*' '/
      equivalence ( num2,junk )
      equivalence ( num3,junk )

      num2 = '  '
      num3 = '   '

      if( num.lt.100 ) then
          write(num2,102) num
      else if( num.lt.1000 ) then
          write(num3,103) num
      endif

      name = junk

  102 format(i2.2)
  103 format(i3.3)

      return
      end

      function nsecf (nhms)
C***********************************************************************
C  Purpose
C     Converts NHMS format to Total Seconds
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
      implicit none
      integer  nhms, nsecf
      nsecf =  nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
      return
      end

      function nhmsf (nsec)
C***********************************************************************
C  Purpose
C     Converts Total Seconds to NHMS format
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
      implicit none
      integer  nhmsf, nsec
      nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
      return
      end

      function nsecf2 (nhhmmss,nmmdd,nymd)
C***********************************************************************
C  Purpose
C     Computes the Total Number of seconds from NYMD using NHHMMSS & NMMDD
C
C  Arguments   Description
C     NHHMMSS  IntervaL Frequency (HHMMSS)
C     NMMDD    Interval Frequency (MMDD)
C     NYMD     Current  Date      (YYMMDD)
C
C  NOTE:
C     IF (NMMDD.ne.0), THEN HOUR FREQUENCY HH MUST BE < 24
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )

      INTEGER YEAR, DAY, SEC, YEAR0, DAY0, SEC0

      DIMENSION  MNDY(12,4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     .           397,34*0 /

C***********************************************************************
C*                 COMPUTE # OF SECONDS FROM NHHMMSS                   *
C***********************************************************************

      nsecf2 = nsecf( nhhmmss )

      if( nmmdd.eq.0 ) return

C***********************************************************************
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *
C***********************************************************************

      DO 100 I=15,48
      MNDY(I,1) = MNDY(I-12,1) + 365
100   CONTINUE

C***********************************************************************
C*                 COMPUTE # OF SECONDS FROM NMMDD                     *
C***********************************************************************

      nsegm =     nmmdd/100
      nsegd = mod(nmmdd,100)

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(NHMS)

      IDAY   = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      month = month + nsegm
      If( month.gt.12 ) then
      month = month - 12
      year = year + 1
      endif
      IDAY2  = MNDY( MONTH ,MOD(YEAR ,4)+1 )

                    nday = iday2-iday
      if(nday.lt.0) nday = nday + 1461
                    nday = nday + nsegd

      nsecf2 = nsecf2 + nday*nsday

      return
      end

      subroutine remap  ( ps1,dp1,u1,v1,thv1,q1,phis1,lm1,
     .                    ps2,dp2,u2,v2,t2  ,q2,phis2,lm2,im,jm,nq,pbelow,pabove )

C***********************************************************************
C
C  Purpose
C     Driver for remapping of target analysis to fv model levels
C
C  Argument Description
C     ps1 ...... model surface  pressure
C     dp1 ...... model pressure thickness
C     u1 ....... model zonal      wind
C     v1 ....... model meridional wind
C     thv1 ..... model virtual potential  temperature
C     q1 ....... model specific   humidity
C     oz1 ...... model ozone
C     phis1 .... model surface geopotential
C     lm1 ...... model vertical   dimension
C
C     ps2 ...... analysis surface  pressure
C     dp2 ...... analysis pressure thickness
C     u2 ....... analysis zonal      wind
C     v2 ....... analysis meridional wind
C     t2 . ..... analysis dry-bulb temperature
C     q2 ....... analysis specific   humidity
C     oz2 ...... analysis ozone
C     phis2 .... analysis surface geopotential
C     lm2 ...... analysis vertical   dimension
C
C     im ....... zonal      dimension
C     jm ....... meridional dimension
C     nq ....... number of  trancers
C     pbelow ... pressure below which analysis is used completely
C     pabove ... pressure above which model    is used completely
C                Note: a blend is used in-between pbelow and pabove
C                If pbelow=pabove, blending code is disabled
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use MAPL_ConstantsMod
      implicit none
      integer  im,jm,nq,lm1,lm2

c fv-DAS variables
c ----------------
      real      dp1(im,jm,lm1),     dp0(im,jm,lm1)
      real       u1(im,jm,lm1),      u0(im,jm,lm1)
      real       v1(im,jm,lm1),      v0(im,jm,lm1)
      real     thv1(im,jm,lm1),    thv0(im,jm,lm1)
      real       q1(im,jm,lm1,nq),   q0(im,jm,lm1,nq)
      real      ps1(im,jm),         ps0(im,jm)

      real   phis1(im,jm)
      real      ak(lm1+1)
      real      bk(lm1+1)

c Target analysis variables
c -------------------------
      real     dp2(im,jm,lm2)
      real      u2(im,jm,lm2)
      real      v2(im,jm,lm2)
      real      t2(im,jm,lm2)
      real    thv2(im,jm,lm2)
      real      q2(im,jm,lm2,nq)
      real     ps2(im,jm)
      real   phis2(im,jm)

c Local variables
c ---------------
      real   pe0(im,jm,lm1+1)
      real   pe1(im,jm,lm1+1)
      real   pe2(im,jm,lm2+1)
      real   pk (im,jm,lm2  )
      real  pke0(im,jm,lm1+1)
      real  pke1(im,jm,lm1+1)
      real  pke2(im,jm,lm2+1)
      real  phi2(im,jm,lm2+1)

      real    kappa,cp,ptop,pbelow,pabove,pl,alf
      real    rgas,pref,tref,pkref,tstar,eps,rvap,grav
      integer i,j,L,n

      kappa = MAPL_KAPPA
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      eps   = rvap/rgas-1.0

      call set_eta ( lm1,ptop,ak,bk )

c Compute edge-level pressures
c ----------------------------
      pe1(:,:,lm1+1) = ps1(:,:)
      do L=lm1,1,-1
      pe1(:,:,L) = pe1(:,:,L+1)-dp1(:,:,L)
      enddo

c Copy input fv state into local variables
c ----------------------------------------
        ps0(:,:)     =  ps1(:,:)
        dp0(:,:,:)   =  dp1(:,:,:)
         u0(:,:,:)   =   u1(:,:,:)
         v0(:,:,:)   =   v1(:,:,:)
       thv0(:,:,:)   = thv1(:,:,:)
         q0(:,:,:,:) =   q1(:,:,:,:)
        pe0(:,:,:)   =  pe1(:,:,:)
       pke0(:,:,:)   =  pe0(:,:,:)**kappa

c Construct target analysis pressure variables
c --------------------------------------------
      do j=1,jm
      do i=1,im
      pe2(i,j,lm2+1) = ps2(i,j)
      enddo
      enddo

      do L=lm2,1,-1
      do j=1,jm
      do i=1,im
      pe2(i,j,L) = pe2(i,j,L+1) - dp2(i,j,L)
      enddo
      enddo
      enddo

      do j=1,jm
      do i=1,im
      pe2(i,j,1) = 1.0  ! Set ptop = 0.01 mb
      enddo
      enddo

      do L=1,lm2+1
      do j=1,jm
      do i=1,im
      pke2(i,j,L) = pe2(i,j,L)**kappa
      enddo
      enddo
      enddo

c Construct target virtual potential temperature
c ----------------------------------------------
      do L=1,lm2
      do j=1,jm
      do i=1,im
      pk  (i,j,L) = ( pke2(i,j,L+1)-pke2(i,j,L) )/( kappa*log(pe2(i,j,L+1)/pe2(i,j,L)) )
      thv2(i,j,L) =     t2(i,j,L)*( 1.0+eps*max(0.0,q2(i,j,L,1)) )/pk(i,j,L)
      enddo
      enddo
      enddo

c Construct target analysis heights
c ---------------------------------
      phi2(:,:,lm2+1) = phis2(:,:)
      do L=lm2,1,-1
      phi2(:,:,L) = phi2(:,:,L+1) + cp*thv2(:,:,L)*( pke2(:,:,L+1)-pke2(:,:,L) )
      enddo
      
c Compute new surface pressure consistent with fv topography
c ----------------------------------------------------------
      do j=1,jm
      do i=1,im
           L = lm2
           do while ( phi2(i,j,L).lt.phis1(i,j) )
           L = L-1
           enddo
           ps1(i,j) = pe2(i,j,L+1)*( 1 + (phi2(i,j,L+1)-phis1(i,j))/(cp*thv2(i,j,L)*pke2(i,j,L+1))  )**(1.0/kappa)
      enddo
      enddo

c Construct fv pressure variables using new surface pressure
c ----------------------------------------------------------
      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1(i,j,L) = ak(L) + bk(L)*ps1(i,j)
      pke1(i,j,L) = pe1(i,j,L)**kappa
      enddo
      enddo
      enddo

      do L=1,lm1
      do j=1,jm
      do i=1,im
       dp1(i,j,L) = pe1(i,j,L+1)-pe1(i,j,L)
      enddo
      enddo
      enddo

c Map original fv state onto new eta grid
c ---------------------------------------
      print *, '   ReMapping Original   FV-State onto New Eta Grid'
      call gmap ( im,jm,nq, kappa,
     .            lm1,  pke0,  pe0, u1,  v1,  thv1, q1,
     .            lm1,  pke1,  pe1, u0,  v0,  thv0, q0)

c Map target analysis onto fv grid
c --------------------------------
      print *, '     Mapping      Analysis-State onto New Eta Grid'
      call gmap ( im,jm,nq, kappa,
     .            lm2,  pke2,  pe2, u2,  v2,  thv2,  q2,
     .            lm1,  pke1,  pe1, u1,  v1,  thv1,  q1)

c Blend result with original fv state
c -----------------------------------
      if( pbelow.ne.pabove ) then
      print *, '   Blending    FV and Analysis States'
      do L=1,lm1
      do j=1,jm
      do i=1,im
        pl=0.5*(pe1(i,j,L+1)+pe1(i,j,L))
       alf=(pl-pabove)/(pbelow-pabove)
      if( pl.lt.pabove ) then
         u1(i,j,L) =   u0(i,j,L)
         v1(i,j,L) =   v0(i,j,L)
       thv1(i,j,L) = thv0(i,j,L)
      else if( pl.lt.pbelow ) then
         u1(i,j,L) =   u1(i,j,L)*alf +   u0(i,j,L)*(1-alf)
         v1(i,j,L) =   v1(i,j,L)*alf +   v0(i,j,L)*(1-alf)
       thv1(i,j,L) = thv1(i,j,L)*alf + thv0(i,j,L)*(1-alf)
      endif
      enddo
      enddo
      enddo
      do n=1,nq
      do L=1,lm1
      do j=1,jm
      do i=1,im
        pl=0.5*(pe1(i,j,L+1)+pe1(i,j,L))
       alf=(pl-pabove)/(pbelow-pabove)
      if( pl.lt.pabove ) then
         q1(i,j,L,n) =   q0(i,j,L,n)
      else if( pl.lt.pbelow ) then
         q1(i,j,L,n) =   q1(i,j,L,n)*alf +   q0(i,j,L,n)*(1-alf)
      endif
      enddo
      enddo
      enddo
      enddo
      endif
 
      return
      end

      subroutine gauss_lat_nmc(gaul,k)
      implicit double precision (a-h,o-z)
      dimension a(500)
      real gaul(1)
      save
      esp=1.d-14
      c=(1.d0-(2.d0/3.14159265358979d0)**2)*0.25d0
      fk=k
      kk=k/2
      call bsslz1(a,kk)
      do 30 is=1,kk
      xz=cos(a(is)/sqrt((fk+0.5d0)**2+c))
      iter=0
   10 pkm2=1.d0
      pkm1=xz
      iter=iter+1
      if(iter.gt.10) go to 70
      do 20 n=2,k
      fn=n
      pk=((2.d0*fn-1.d0)*xz*pkm1-(fn-1.d0)*pkm2)/fn
      pkm2=pkm1
   20 pkm1=pk
      pkm1=pkm2
      pkmrk=(fk*(pkm1-xz*pk))/(1.d0-xz**2)
      sp=pk/pkmrk
      xz=xz-sp
      avsp=abs(sp)
      if(avsp.gt.esp) go to 10
      a(is)=xz
   30 continue
      if(k.eq.kk*2) go to 50
      a(kk+1)=0.d0
      pk=2.d0/fk**2
      do 40 n=2,k,2
      fn=n
   40 pk=pk*fn**2/(fn-1.d0)**2
   50 continue
      do 60 n=1,kk
      l=k+1-n
      a(l)=-a(n)
   60 continue
      radi=180./(4.*atan(1.))
      do 211 n=1,k
      gaul(n)=acos(a(n))*radi-90.0
  211 continue
      return
   70 write(6,6000)
 6000 format(//5x,14herror in gauaw//)
      stop
      end

      subroutine bsslz1(bes,n)
      implicit double precision (a-h,o-z)
      dimension bes(n)
      dimension bz(50)
      data pi/3.14159265358979d0/
      data bz  / 2.4048255577d0, 5.5200781103d0,
     $  8.6537279129d0,11.7915344391d0,14.9309177086d0,18.0710639679d0,
     $ 21.2116366299d0,24.3524715308d0,27.4934791320d0,30.6346064684d0,
     $ 33.7758202136d0,36.9170983537d0,40.0584257646d0,43.1997917132d0,
     $ 46.3411883717d0,49.4826098974d0,52.6240518411d0,55.7655107550d0,
     $ 58.9069839261d0,62.0484691902d0,65.1899648002d0,68.3314693299d0,
     $ 71.4729816036d0,74.6145006437d0,77.7560256304d0,80.8975558711d0,
     $ 84.0390907769d0,87.1806298436d0,90.3221726372d0,93.4637187819d0,
     $ 96.6052679510d0,99.7468198587d0,102.888374254d0,106.029930916d0,
     $ 109.171489649d0,112.313050280d0,115.454612653d0,118.596176630d0,
     $ 121.737742088d0,124.879308913d0,128.020877005d0,131.162446275d0,
     $ 134.304016638d0,137.445588020d0,140.587160352d0,143.728733573d0,
     $ 146.870307625d0,150.011882457d0,153.153458019d0,156.295034268d0/
      nn=n
      if(n.le.50) go to 12
      bes(50)=bz(50)
      do 5 j=51,n
    5 bes(j)=bes(j-1)+pi
      nn=49
   12 do 15 j=1,nn
   15 bes(j)=bz(j)
      return
      end


      subroutine get_ozone ( ozone,pl,im,jm,lm,nymd,nhms )
      implicit none

      integer     nlats
      integer     nlevs
      parameter ( nlats = 37 )  ! 37 Latitudes
      parameter ( nlevs = 34 )  ! 34 Pressure Levels

      real     o3(nlats,nlevs)
      real   lats(nlats)
      real   levs(nlevs)

c Input Variables
c ---------------
      integer    im,jm,lm,nymd,nhms
      real ozone(im,jm,lm)
      real    pl(im,jm,lm)

c Local Variables
c ---------------
      real  xlat(im,jm)
      integer i,j,L,koz

      real        voltomas
      PARAMETER ( VOLTOMAS = 1.655E-6 )

      koz = 40

      do   j=1,jm
      do   i=1,im
      xlat(i,j) = -90. + (j-1)*180./(jm-1)
      enddo
      enddo

      call chemistry (koz,nymd,nhms,o3,lats,levs,nlats,nlevs)
      call interp_oz (o3,lats,levs,nlats,nlevs,im*jm,xlat,lm,pl,ozone)

      ozone(:,:,:) = ozone(:,:,:) * VOLTOMAS

      return
      end

      subroutine chemistry (koz,nymd,nhms,ozone,lats,levs,nlats,nlevs)
C***********************************************************************
C  PURPOSE
C     Chemistry Model
C
C  ARGUMENTS   DESCRIPTION
C     koz      Unit to read Stratospheric Ozone
C     kqz      Unit to read Stratospheric Moisture
C     nymd     Current Date
C     nhms     Current Time
C
C     chemistry .. Chemistry State Data Structure
C     grid ....... Dynamics  Grid  Data Structure
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  koz
      integer  nymd,nhms

      integer     nlats
      integer     nlevs
      real  ozone(nlats,nlevs)
      real   lats(nlats)
      real   levs(nlevs)
      real     o3(nlats,nlevs,12)

c Local Variables
c ---------------
      integer  j,L
      integer  nymd1,nhms1,nymd2,nhms2,ipls,imns
      real     facm,facp

C **********************************************************************
C ****      Read Ozone and Moisture Data (12 Monthly Means)         ****
C **********************************************************************

      call read_oz (koz,o3,lats,levs,nlats,nlevs,12)

C **********************************************************************
C ****            Update Chemistry State to Current Time            ****
C **********************************************************************

      call time_bound  ( nymd,nhms, nymd1,nhms1, nymd2,nhms2, imns,ipls )
      call interp_time ( nymd,nhms, nymd1,nhms1, nymd2,nhms2, facm,facp )

      do L = 1,nlevs
      do j = 1,nlats
      ozone(j,L) = o3(j,L,imns)*facm + o3(j,L,ipls)*facp
      enddo
      enddo

      return
      end

      subroutine read_oz (ku,oz,lats,levs,nlat,nlev,ntime)
C***********************************************************************
C  PURPOSE
C     To Read Ozone Value
C
C  ARGUMENTS   DESCRIPTION
C     ku ...... Unit to Read Ozone Data
C     oz ...... Ozone Data
C     lats .... Ozone Data Latitudes (degrees)
C     levs .... Ozone Data Levels    (mb)
C     nlat .... Number of ozone latitudes
C     nlev .... Number of ozone levels
C     ntime ... Number of ozone time values
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  ku,nlat,nlev,ntime

      real   oz(nlat,nlev,ntime)
      real*4 o3(nlat)
      real lats(nlat)
      real levs(nlev)

      integer time
      integer lat
      integer lev
      integer nrec

      real plevs(34)
      data plevs/ 0.003, 0.005, 0.007, 0.01, 0.015, 0.02, 0.03, 0.05,
     .            0.07, 0.1, 0.15, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0,
     .            3.0, 5.0, 7.0, 10.0, 15.0, 20.0, 30.0, 50.0, 70.0,
     .            100.0, 150.0, 200.0, 300.0, 500.0, 700.0, 1000.0 /

      rewind ku

c Set Ozone Data Latitudes
c ------------------------
      do   lat = 1,nlat
      lats(lat) = -90. + (lat-1)*5.
      enddo

c Set Ozone Data Levels
c ------------------------
      do   lev = 1,nlev
      levs(lev) = plevs(lev)*100
      enddo

c Read Ozone Amounts by Month and Level
c -------------------------------------
      close (ku)
      open  (ku, file="/home/ltakacs/data/bcs/TSMo3.v02.gra",
     .           form='unformatted', access='direct', recl=nlat*4)

      do time=1,ntime
      do lev=1,nlev
      nrec = lev+(time-1)*nlev*2   ! Note: 2 quantities in Ozone Dataset
      read(ku,rec=nrec) o3
           do lat=1,nlat
           oz(lat,nlev-lev+1,time) = o3(lat)
           enddo
      enddo
      enddo

      close (ku)
      return
      end

      subroutine interp_oz (ozone,lats,levs,nlats,nlevs,irun ,xlat,km,plevs,ozrad)

c Declare Modules and Data Structures
c -----------------------------------
      implicit none
      integer nlats,nlevs
      real     ozone(nlats,nlevs)
      real      lats(nlats)
      real      levs(nlevs)

      integer irun,km
      real xlat  (irun)
      real plevs (irun,km)
      real ozrad (irun,km)

c Local Variables
c ---------------
      real zero,one,o3min
      PARAMETER ( ZERO     = 0.0 )
      PARAMETER ( ONE      = 1.0 )
      PARAMETER ( O3MIN    = 1.0E-10  )

      integer  i,k,L1,L2,LM,LP
      integer  jlat,jlatm,jlatp
      real  O3INT1(IRUN,nlevs)
      real    QPR1(IRUN), QPR2(IRUN), SLOPE(IRUN)
      real     PR1(IRUN),  PR2(IRUN)

C **********************************************************************
C ****           INTERPOLATE ozone data to model latitudes           ***
C **********************************************************************

      DO 32 K=1,nlevs
      DO 34 I=1,IRUN

      DO 36 jlat = 1,nlats
      IF( lats(jlat).gt.xlat(i) ) THEN
      IF( jlat.EQ.1 ) THEN
      jlatm    = 1
      jlatp    = 1
      slope(i) = zero
        ELSE
      jlatm    = jlat-1
      jlatp    = jlat
      slope(i) = ( XLAT(I)        -lats(jlat-1) )
     .         / ( lats(jlat)-lats(jlat-1) )
      ENDIF
      GOTO 37
      ENDIF
   36 CONTINUE
      jlatm    = nlats
      jlatp    = nlats
      slope(i) = one
   37 CONTINUE
      QPR1(I) = ozone(jlatm,k)
      QPR2(I) = ozone(jlatp,k)
   34 CONTINUE

      DO 38 I=1,IRUN
      o3int1(i,k) = qpr1(i) + slope(i)*( qpr2(i)-qpr1(i) )
   38 CONTINUE

   32 CONTINUE

C **********************************************************************
C ****     INTERPOLATE latitude ozone data to model pressures        ***
C **********************************************************************

      DO 40 L2 = 1,km

      DO 44 I  = 1,IRUN
      DO 46 L1 = 1,nlevs
      IF( levs(L1).GT.PLEVS(I,L2) ) THEN
      IF( L1.EQ.1 ) THEN
          LM = 1
          LP = 2
        ELSE
          LM = L1-1
          LP = L1
      ENDIF
      GOTO 47
      ENDIF
   46 CONTINUE
            LM = nlevs-1
            LP = nlevs
   47 CONTINUE
       PR1(I) = levs (LM)
       PR2(I) = levs (LP)
      QPR1(I) =   O3INT1(I,LM)
      QPR2(I) =   O3INT1(I,LP)
   44 CONTINUE

      DO 48 I=1,IRUN
         SLOPE(I) = ( QPR1(I)-QPR2(I) )
     .            / (  PR1(I)- PR2(I) )
      ozrad(I,L2) =   QPR2(I) + ( PLEVS(I,L2)-PR2(I) )*SLOPE(I)

      if( ozrad(i,l2).lt.o3min ) then
          ozrad(i,l2) =  o3min
      endif

   48 CONTINUE
   40 CONTINUE

      RETURN
      END

      subroutine interp_time ( nymd ,nhms , 
     .                         nymd1,nhms1, nymd2,nhms2, fac1,fac2 )
C***********************************************************************        
C                                                                               
C  PURPOSE:
C  ========
C    Compute interpolation factors, fac1 & fac2, to be used in the
C    calculation of the instantanious boundary conditions, ie:
C
C               q(i,j) = fac1*q1(i,j) + fac2*q2(i,j)
C    where:
C               q(i,j) => Boundary Data valid    at (nymd  , nhms )
C              q1(i,j) => Boundary Data centered at (nymd1 , nhms1)
C              q2(i,j) => Boundary Data centered at (nymd2 , nhms2)
C                                                                               
C  INPUT:                                                                       
C  ======                                                                       
C    nymd     : Date (yymmdd) of Current Timestep
C    nhms     : Time (hhmmss) of Current Timestep
C    nymd1    : Date (yymmdd) of Boundary Data 1
C    nhms1    : Time (hhmmss) of Boundary Data 1
C    nymd2    : Date (yymmdd) of Boundary Data 2
C    nhms2    : Time (hhmmss) of Boundary Data 2
C                                                                               
C  OUTPUT:                                                                      
C  =======                                                                      
C    fac1     : Interpolation factor for Boundary Data 1
C    fac2     : Interpolation factor for Boundary Data 2
C                                                                               
C                                                                               
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        
                                                                                
      INTEGER  YEAR , MONTH , DAY , SEC                                                    
      INTEGER  YEAR1, MONTH1, DAY1, SEC1
      INTEGER  YEAR2, MONTH2, DAY2, SEC2

      real     fac1, fac2
      real     time, time1, time2
                                                                                
      INTEGER    DAYSCY                                                         
      PARAMETER (DAYSCY = 365*4+1)                                   

      REAL       MNDY(12,4)              
                                                                                
      LOGICAL FIRST                                                             
      DATA    FIRST/.TRUE./                                                     
                                                                                
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,                
     .           397,34*0 /                                                     
                                                                                
C***********************************************************************        
C*                         SET TIME BOUNDARIES                         *        
C***********************************************************************        
                                                                                
      YEAR   = NYMD / 10000                                                      
      MONTH  = MOD(NYMD,10000) / 100                                             
      DAY    = MOD(NYMD,100)                                                     
      SEC    = NSECF(NHMS)                                                       
                                                                                
      YEAR1  = NYMD1 / 10000                                                      
      MONTH1 = MOD(NYMD1,10000) / 100                                             
      DAY1   = MOD(NYMD1,100)                                                     
      SEC1   = NSECF(NHMS1)                                                       
                                                                                
      YEAR2  = NYMD2 / 10000                                                      
      MONTH2 = MOD(NYMD2,10000) / 100                                             
      DAY2   = MOD(NYMD2,100)                                                     
      SEC2   = NSECF(NHMS2)                                                       
                                                                                
C***********************************************************************        
C*                    COMPUTE DAYS IN 4-YEAR CYCLE                     *        
C***********************************************************************        
                                                                                
      IF(FIRST) THEN                                                            
      DO   I=15,48                                                       
      MNDY(I,1) = MNDY(I-12,1) + 365                                       
      ENDDO
      FIRST=.FALSE.                                                        
      ENDIF                                                                     
                                                                                
C***********************************************************************        
C*                     COMPUTE INTERPOLATION FACTORS                   *        
C***********************************************************************        
                                                                                
      time  = DAY  + MNDY(MONTH ,MOD(YEAR ,4)+1) + float(sec )/86400.                           
      time1 = DAY1 + MNDY(MONTH1,MOD(YEAR1,4)+1) + float(sec1)/86400.                           
      time2 = DAY2 + MNDY(MONTH2,MOD(YEAR2,4)+1) + float(sec2)/86400.                           

      if( time .lt.time1 ) time  = time  + dayscy
      if( time2.lt.time1 ) time2 = time2 + dayscy

      fac1  = (time2-time)/(time2-time1)
      fac2  = (time-time1)/(time2-time1)

      RETURN                                                                    
      END                                                                       

      subroutine time_bound ( nymd,nhms,nymd1,nhms1,nymd2,nhms2, imnm,imnp )
C***********************************************************************
C  PURPOSE
C     Compute Date and Time boundaries.
C
C  ARGUMENTS   DESCRIPTION
C     nymd .... Current    Date
C     nhms .... Current    Time
C     nymd1 ... Previous   Date Boundary
C     nhms1 ... Previous   Time Boundary
C     nymd2 ... Subsequent Date Boundary
C     nhms2 ... Subsequent Time Boundary
C
C     imnm .... Previous   Time Index for Interpolation
C     imnp .... Subsequent Time Index for Interpolation
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  nymd,nhms, nymd1,nhms1, nymd2,nhms2

c Local Variables
c ---------------
      integer  month,day,nyear,midmon1,midmon,midmon2
      integer  imnm,imnp
      INTEGER  DAYS(14), daysm, days0, daysp
      DATA     DAYS /31,31,28,31,30,31,30,31,31,30,31,30,31,31/

      integer nmonf,ndayf,n
      NMONF(N) = MOD(N,10000)/100
      NDAYF(N) = MOD(N,100)

C*********************************************************************
C**** Find Proper Month and Time Boundaries for Climatological Data **
C*********************************************************************

      MONTH  = NMONF(NYMD)
      DAY    = NDAYF(NYMD)

      daysm  = days(month  )
      days0  = days(month+1)
      daysp  = days(month+2)

c Check for Leap Year
c -------------------
      nyear = nymd/10000
      if( 4*(nyear/4).eq.nyear ) then
      if( month.eq.3 ) daysm = daysm+1
      if( month.eq.2 ) days0 = days0+1
      if( month.eq.1 ) daysp = daysp+1
      endif

      MIDMON1 = daysm/2 + 1
      MIDMON  = days0/2 + 1
      MIDMON2 = daysp/2 + 1


      IF(DAY.LT.MIDMON) THEN
         imnm = month
         imnp = month + 1
         nymd2 = (nymd/10000)*10000 + month*100 + midmon
         nhms2 = 000000
         nymd1 = nymd2
         nhms1 = nhms2
         call tick ( nymd1,nhms1,       -midmon  *86400 )
         call tick ( nymd1,nhms1,-(daysm-midmon1)*86400 )
      ELSE
         IMNM = MONTH + 1
         IMNP = MONTH + 2
         nymd1 = (nymd/10000)*10000 + month*100 + midmon
         nhms1 = 000000
         nymd2 = nymd1
         nhms2 = nhms1
         call tick ( nymd2,nhms2,(days0-midmon)*86400 )
         call tick ( nymd2,nhms2,       midmon2*86400 )
      ENDIF

c -------------------------------------------------------------
c Note:  At this point, imnm & imnp range between 01-14, where
c        01    -> Previous years December
c        02-13 -> Current  years January-December
c        14    -> Next     years January
c -------------------------------------------------------------

      imnm = imnm-1
      imnp = imnp-1

      if( imnm.eq.0  ) imnm = 12
      if( imnp.eq.0  ) imnp = 12
      if( imnm.eq.13 ) imnm = 1
      if( imnp.eq.13 ) imnp = 1

      return
      end

      subroutine tick (nymd,nhms,ndt)
C***********************************************************************
C  Purpose
C     Tick the Date (nymd) and Time (nhms) by NDT (seconds)
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      IF(NDT.NE.0) THEN
      NSEC = NSECF(NHMS) + NDT

      IF (NSEC.GT.86400)  THEN
      DO WHILE (NSEC.GT.86400)
      NSEC = NSEC - 86400
      NYMD = INCYMD (NYMD,1)
      ENDDO
      ENDIF   
               
      IF (NSEC.EQ.86400)  THEN
      NSEC = 0
      NYMD = INCYMD (NYMD,1)
      ENDIF   
               
      IF (NSEC.LT.00000)  THEN
      DO WHILE (NSEC.LT.0)
      NSEC = 86400 + NSEC
      NYMD = INCYMD (NYMD,-1)
      ENDDO
      ENDIF   
               
      NHMS = NHMSF (NSEC)
      ENDIF   

      RETURN  
      END    

      FUNCTION INCYMD (NYMD,M)                                                  
C***********************************************************************        
C  PURPOSE                                                                      
C     INCYMD:  NYMD CHANGED BY ONE DAY                                          
C     MODYMD:  NYMD CONVERTED TO JULIAN DATE                                    
C  DESCRIPTION OF PARAMETERS                                                    
C     NYMD     CURRENT DATE IN YYMMDD FORMAT                                    
C     M        +/- 1 (DAY ADJUSTMENT)                                           
C                                                                               
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        
                                                                                
      INTEGER NDPM(12)                                                          
      DATA    NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/             
      LOGICAL LEAP                                                              
      LEAP(NY) = MOD(NY,4).EQ.0 .AND. (MOD(NY,100).NE.0 .OR. MOD(NY,400).EQ.0)
                                                                                
C***********************************************************************        
C                                                                               
      NY = NYMD / 10000                                                         
      NM = MOD(NYMD,10000) / 100                                                
      ND = MOD(NYMD,100) + M                                                    
                                                                                
      IF (ND.EQ.0) THEN                                                         
      NM = NM - 1                                                               
      IF (NM.EQ.0) THEN                                                         
          NM = 12                                                               
          NY = NY - 1                                                           
      ENDIF                                                                     
      ND = NDPM(NM)                                                             
      IF (NM.EQ.2 .AND. LEAP(NY))  ND = 29                                      
      ENDIF                                                                     
                                                                                
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP(NY))  GO TO 20                      
                                                                                
      IF (ND.GT.NDPM(NM)) THEN                                                  
      ND = 1                                                                    
      NM = NM + 1                                                               
      IF (NM.GT.12) THEN                                                        
          NM = 1                                                                
          NY = NY + 1                                                           
      ENDIF                                                                     
      ENDIF                                                                     
                                                                                
   20 CONTINUE                                                                  
      INCYMD = NY*10000 + NM*100 + ND                                           
      RETURN                                                                    
                                                                                
C***********************************************************************        
C                      E N T R Y    M O D Y M D                                 
C***********************************************************************        
                                                                                
      ENTRY MODYMD (NYMD)                                                       

      NY = NYMD / 10000                                                         
      NM = MOD(NYMD,10000) / 100                                                
      ND = MOD(NYMD,100)                                                        
                                                                                
   40 CONTINUE                                                                  
      IF (NM.LE.1)  GO TO 60                                                    
      NM = NM - 1                                                               
      ND = ND + NDPM(NM)                                                        
      IF (NM.EQ.2 .AND. LEAP(NY))  ND = ND + 1                                  
      GO TO 40                                                                  
                                                                                
   60 CONTINUE                                                                  
      MODYMD = ND                                                               

      RETURN                                                                    
      END                                                                       

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " ec2fv.x [-ncep  ncep.data]"
      print *, "         [-ctl   ncep.ctl]"
      print *, "         [-bkg   bkg.data]"
      print *, "         [-nymd  nymd]"
      print *, "         [-nhms  nhms]"
      print *, "         [-plow  plow]"
      print *, "         [-phigh phigh]"
      print *, "         [-tag   tag]"
      print *, "         [-ozone]"
      print *
      print *, "where:"
      print *
      print *, "  -ncep   ncep.data: Filename of NCEP Pressure-Level analysis data"
      print *, "  -ctl    ncep.ctl : Filename of NCEP Pressure-Level analysis ctl"
      print *, "  -bkg     bkg.data: Filename of GMAO Background Data (ana.eta format)"
      print *
      print *, "  -plow        plow:  Pressure Level to begin blending"
      print *, "  -phigh      phigh:  Pressure Level to   end blending"
      print *
      print *, "  -nymd        nymd:  Desired date in yyyymmdd format"
      print *, "  -nhms        nhms:  Desired time in   hhmmss format"
      print *
      print *, "  -tag          tag:  Optional Prefix tag for output files"
      print *, "  -ozone              Optional Flag to add ozone"
      print *
      call exit(7)
      end

      subroutine get_slp ( ps,phis,slp,pe,pk,tv,rgas,grav,im,jm,km )
      implicit   none
      integer  im,jm,km
      real     grav
      real     rgas
      real     pk(im,jm,km)    ! layer-mean P**kappa
      real     tv(im,jm,km)    ! layer-mean virtual Temperature
      real     pe(im,jm,km+1)  ! press at layer edges (Pa)
      real     ps(im,jm)       ! surface pressure (Pa)
      real   phis(im,jm)       ! surface geopotential
      real    slp(im,jm)       ! sea-level pressure (hPa)

      real    p_offset
      real    p_bot
      real    tstar                  ! extrapolated temperature (K)
      real    tref                   ! Reference virtual temperature (K)
      real    pref                   ! Reference pressure level (Pa)
      real    pkref                  ! Reference pressure level (Pa) ** kappa
      real    dp1, dp2
      real    factor, yfactor
      real    gg
      real    gamma
      integer k_bot, k, k1, k2, i,j

      gamma    = 6.5e-3
      gg       = gamma / grav
      factor   = grav / ( Rgas * gamma )
      yfactor  =  Rgas * gg
      p_offset = 15000.                     ! 150 hPa above surface

      do j=1,jm
      do i=1,im
      p_bot = ps(i,j) - p_offset
      k_bot = -1
      do k = km, 2, -1
         if ( pe(i,j,k+1) .lt. p_bot ) then
              k_bot = k
              go to 123
         endif
      enddo
123   continue
         k1 = k_bot - 1
         k2 = k_bot
        dp1 = pe(i,j,k_bot)   - pe(i,j,k_bot-1)
        dp2 = pe(i,j,k_bot+1) - pe(i,j,k_bot)
      pkref = ( pk(i,j,k1)*dp1 + pk(i,j,k2)*dp2 ) / (dp1+dp2)
       tref = ( tv(i,j,k1)*dp1 + tv(i,j,k2)*dp2 ) / (dp1+dp2)
       pref = 0.5 * ( pe(i,j,k_bot+1) + pe(i,j,k_bot-1) )
      tstar = tref*( ps(i,j)/pref )**yfactor
      slp(i,j) = ps(i,j)*( 1.0+gg*phis(i,j)/tstar )**factor
      enddo
      enddo

      return
      end

C **********************************************************************
C ****               Read Grads CTL File for Meta Data              ****
C **********************************************************************

      subroutine read_ctl ( ctlfile,im,jm,lm,undef,format,
     .                      nvars,names,descs,lmvars,
     .                      lats,lons,levs )
      implicit none

      character*256, pointer ::  names(:)
      character*256, pointer ::  descs(:)
      integer,       pointer :: lmvars(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)
      real,          pointer ::   levs(:)

      character*256 ctlfile, format
      integer       im,jm,lm,nvars
      real          undef,dx,dy,dz
      integer       i,j,L,m,n,ndum
      character*256 dummy,name
      character*256, allocatable :: dum(:)

      open (10,file=trim(ctlfile),form='formatted')
      format = 'direct'
      do
      read(10,*,end=500) dummy

c OPTIONS
c -------
           if( trim(dummy).eq.'options' ) then
           ndum = 1
           do
              backspace(10)
              allocate ( dum(ndum) )
              read(10,*,err=101) dummy
              if( trim(dummy).eq.'options' ) then
                  backspace(10)
                  read(10,*,end=101) dummy,( dum(n),n=1,ndum )
              else
                  goto 101
              endif
              if( trim(dum(ndum)).eq.'sequential' ) format = 'sequential'
              deallocate ( dum )
              ndum = ndum + 1
           enddo
  100      format(a5)
  101      continue
           deallocate ( dum )
           endif

c XDEF
c ----
           if( trim(dummy).eq.'xdef' ) then
           backspace(10)
           read(10,*) dummy,im
                allocate( lons(im) )
                backspace(10)
                read(10,*) dummy,im,dummy,lons(1),dx
                if( trim(dummy).eq.'linear' ) then
                    do i=2,im
                    lons(i) = lons(i-1) + dx
                    enddo
                else
                    backspace(10)
                    read(10,*) dummy,n,dummy,(lons(i),i=1,im)
                endif
           endif

c YDEF
c ----
           if( trim(dummy).eq.'ydef' ) then
           backspace(10)
           read(10,*) dummy,jm
                allocate( lats(jm) )
                backspace(10)
                read(10,*) dummy,jm,dummy,lats(1),dy
                if( trim(dummy).eq.'linear' ) then
                    do j=2,jm
                    lats(j) = lats(j-1) + dy
                    enddo
                else
                    backspace(10)
                    read(10,*) dummy,n,dummy,(lats(j),j=1,jm)
                endif
           endif

c ZDEF
c ----
           if( trim(dummy).eq.'zdef' ) then
           backspace(10)
           read(10,*) dummy,lm
#if 1
                allocate( levs(lm) )
                backspace(10)
                if( lm.eq.1 ) then
                    read(10,*) dummy,lm,dummy,levs(1)
                else
                    read(10,*) dummy,lm,dummy,levs(1),dz
                endif
                if( trim(dummy).eq.'linear' ) then
                    do L=2,lm
                    levs(L) = levs(L-1) + dz
                    enddo
                else
                    backspace(10)
                    read(10,*) dummy,n,dummy,(levs(L),L=1,lm)
                endif
#endif
           endif

c UNDEF
c -----
           if( trim(dummy).eq.'undef' ) then
           backspace(10)
           read(10,*) dummy,undef
           endif

           if( trim(dummy).eq.'vars' ) then
           backspace(10)
           read(10,*) dummy,nvars
           allocate(  names(nvars) )
           allocate(  descs(nvars) )
           allocate( lmvars(nvars) )

           do n=1,nvars
           read(10,*)  names(n),lmvars(n),m,descs(n)
           if( lmvars(n).eq.0 ) lmvars(n) = 1
           enddo

           endif
      enddo
  500 continue
      rewind(10)

      if( nvars.eq.0 ) then
          print *, 'Warning, nvars = 0!'
          stop
      endif

      return
      end subroutine read_ctl

      subroutine atod_winds ( ua,va,ud,vd,im,jm,lm )

C ******************************************************************
C ****                                                          ****
C ****  This program converts 'A' gridded winds                 ****
C ****                     to 'D' gridded winds                 ****
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

      call atod ( ua,ud,im,jm,lm,2 )
      call atod ( va,vd,im,jm,lm,1 )

      return
      end

      subroutine dtoa_winds ( ud,vd,ua,va,im,jm,lm )

C ******************************************************************
C ****                                                          ****
C ****  This program converts 'D' gridded winds                 ****
C ****                     to 'A' gridded winds                 ****
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

      real sinx(im/2)
      real cosx(im/2)

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      do i=1,imh
      sinx(i) = sin( -pi + (i-1)*dx )
      cosx(i) = cos( -pi + (i-1)*dx )
      enddo

C *********************************************************
C ****              Average D-Grid Winds               ****
C *********************************************************

      call dtoa ( ud,ua,im,jm,lm,2 )
      call dtoa ( vd,va,im,jm,lm,1 )

C *********************************************************
C ****                Fix A-Grid Pole Winds            ****
C *********************************************************

      do L=1,lm

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

      subroutine hinterp ( qin,iin,jin,qout,iout,jout,mlev,undef )
      implicit   none
      integer    iin,jin,       iout,jout, mlev
      real   qin(iin,jin,mlev), qout(iout,jout,mlev)
      real undef,pi,dlin,dpin,dlout,dpout
      real dlam(iin), lons(iout*jout), lon
      real dphi(jin), lats(iout*jout), lat
      integer i,j,loc

      pi = 4.0*atan(1.0)
      dlin = 2*pi/iin
      dpin = pi/(jin-1)
      dlam(:) = dlin
      dphi(:) = dpin

      dlout = 2*pi/iout
      dpout = pi/(jout-1)
      
      loc = 0
      do j=1,jout
      do i=1,iout
      loc = loc + 1
      lon = -pi + (i-1)*dlout
      lons(loc) = lon
      enddo
      enddo

      loc = 0
      do j=1,jout
      lat = -pi/2.0 + (j-1)*dpout
      do i=1,iout
      loc = loc + 1
      lats(loc) = lat
      enddo
      enddo

      call interp_h ( qin,iin,jin,mlev,dlam,dphi,
     .                qout,iout*jout,lons,lats,undef, -pi )

      return
      end

      subroutine interp_h ( q_cmp,im,jm,lm,dlam,dphi, 
     .                       q_geo,irun,lon_geo,lat_geo, undef, lon_min )
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

      real :: lon_min

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

      real    lon_cmp(im)
      real    lat_cmp(jm)
      real    q_tmp(irun)

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
      lon_cmp(1) = lon_min   ! user supplied orign
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
          phi_jm1 =  lat_cmp(jm1(i))

      if( jm2(i).eq.0 ) then
          phi_jm2 = phi_jm1 - dphi(1)
      else
          phi_jm2 =  lat_cmp(jm2(i))
      endif

      if( jm1(i).eq.jm ) then
          phi_jp0 = phi_jm1 + dphi(jm-1)
          phi_jp1 = phi_jp0 + dphi(jm-2)
      else
          phi_jp0 =  lat_cmp(jp0(i))
          if( jp1(i).eq.jm+1 ) then
              phi_jp1 = phi_jp0 + dphi(jm-1)
          else
              phi_jp1 =  lat_cmp(jp1(i))
          endif
      endif


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

      return

      end 

      subroutine sigtopl ( qprs,q,logpl,logp,im,jm,lm,undef )
C***********************************************************************
C
C PURPOSE
C   To interpolate an arbitrary quantity from Model Vertical Grid to Pressure
C
C INPUT           
C   Q ..... Q    (im,jm,lm) Arbitrary Quantity on Model Grid
C   PKZ ... PKZ  (im,jm,lm) Pressure to the Kappa at Model Levels (From Phillips)
C   PKSRF . PKSRF(im,jm) Surface Pressure to the Kappa
C   PTOP .. Pressure at Model Top
C   P ..... Output Pressure Level (mb)     
C   IM .... Longitude Dimension of Input
C   JM .... Latitude  Dimension of Input
C   LM .... Vertical  Dimension of Input
C
C OUTPUT                                        
C   QPRS .. QPRS (im,jm) Arbitrary Quantity at Pressure p
C
C NOTE
C   Quantity is interpolated Linear in P**Kappa.
C   Between  PTOP**Kappa and PKZ(1),  quantity is extrapolated.
C   Between PKSRF**Kappa and PKZ(LM), quantity is extrapolated.
C   Undefined Model-Level quantities are not used.
C                                               
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
C
      implicit none
      integer  i,j,l,im,jm,lm

      real  qprs(im,jm)
      real  q   (im,jm,lm)
      real logpl(im,jm,lm)

      real  p,undef
      real  logp,logpmin,logpmax,temp

c Initialize to UNDEFINED
c -----------------------
      do i=1,im*jm
      qprs(i,1) = undef
      enddo

c Interpolate to Pressure Between Model Levels
c --------------------------------------------
      do L=1,lm-1
      if( all( logpl(:,:,L  )>logp ) ) exit
      if( all( logpl(:,:,L+1)<logp ) ) cycle

         do j=1,jm
         do i=1,im
         if( logp.le.logpl(i,j,L+1) .and. logp.ge.logpl(i,j,L) ) then
         temp      = ( logpl(i,j,L)-logp ) / ( logpl(i,j,L)-logpl(i,j,L+1) )

                 if( q(i,j,L)  .ne.undef  .and.
     .               q(i,j,L+1).ne.undef ) then
         qprs(i,j) = q(i,j,L+1)*temp + q(i,j,L)*(1.-temp)
            else if( q(i,j,L+1).ne.undef  .and. temp.ge.0.5 ) then
         qprs(i,j) = q(i,j,L+1)
            else if( q(i,j,L)  .ne.undef  .and. temp.le.0.5 ) then
         qprs(i,j) = q(i,j,L)  
                 endif

         endif
         enddo
         enddo

      enddo

c Set Values above and below Model Boundaries
c -------------------------------------------
      do j=1,jm
      do i=1,im
      if( logp.le.logpl(i,j,1 ) ) qprs(i,j) = q(i,j,1 )
      if( logp.ge.logpl(i,j,lm) ) qprs(i,j) = q(i,j,lm)
      enddo
      enddo

      return
      end subroutine sigtopl

      subroutine interp3 ( q1,p1,im,jm,km, q2,p2,kn, ps )
      implicit none

      integer,  intent(in) :: im                ! Longitude dimension
      integer,  intent(in) :: jm                ! Latitude  dimension
      integer,  intent(in) :: km                ! Original vertical dimension
      integer,  intent(in) :: kn                ! Target   vertical dimension
 
      real,     intent(in) ::   q1(im,jm,km)    ! Field input
      real,     intent(in) ::   p1(im,jm,km)    ! Pressure at mid-layer in Original coordinate

      real,     intent(inout)::  q2(im,jm,kn)   ! Field output
      real,     intent(in)   ::  p2(im,jm,kn)   ! Pressure at mid-layer in Target   coordinate
      real,     intent(in)   ::  ps(im,jm)      ! Surface Pressure      in Target   coordinate

!-----------------------------------------------------------------------
!
! !LOCAL VARIABLES:
      real   logpl1(im,jm,km)
      real   logpl2(im,jm,kn)
      real   dlogp1(im,jm,km)
      real   am2,am1,ap0,ap1,P,PLP1,PLP0,PLM1,PLM2,DLP0,DLM1,DLM2

      integer i,j,k,kc, LM2,LM1,LP0,LP1

! Initialization
! --------------
      logpl1 = log( p1 )
      logpl2 = log( p2 )

      do k=1,km-1
      dlogp1(:,:,k) = logpl1(:,:,k+1)-logpl1(:,:,k)
      enddo

! Interpolate Q onto target Pressures
! -----------------------------------
      do j=1,jm
      do i=1,im

      kc = 1
      dowhile ( kc.lt.km .and. logpl1(i,j,kc).lt.log(ps(i,j)) )
      kc = kc+1
      enddo
      kc = kc-1
      
      do k=1,kn
         LM1 = 1
         LP0 = 1
         do while( LP0.le.km )
            if (logpl1(i,j,LP0).lt.logpl2(i,j,k)) then
               LP0 = LP0+1
            else
               exit
            endif
         enddo
         LM1 = max(LP0-1,1)
         LP0 = min(LP0, km)

! Extrapolate Linearly in LogP above first model level
! ----------------------------------------------------
         if( LM1.eq.1 .and. LP0.eq.1 ) then
             q2(i,j,k) = q1(i,j,1)
c            q2(i,j,k) = q1(i,j,1) + ( q1(i,j,2)-q1(i,j,1) )*( logpl2(i,j,k)-logpl1(i,j,1) )
c    .                                                      /( logpl1(i,j,2)-logpl1(i,j,1) )

! Extrapolate Linearly in LogP below last model level
! ---------------------------------------------------
         else if( LM1.eq.km .and. LP0.eq.km ) then
             q2(i,j,k) = q1(i,j,kc)
c            q2(i,j,k) = q1(i,j,km) + ( q1(i,j,km)-q1(i,j,km-1) )*( logpl2(i,j,k )-logpl1(i,j,km  ) )
c    .                                                           /( logpl1(i,j,km)-logpl1(i,j,km-1) )

! Interpolate Linearly in LogP between levels 1 => 2 and km-1 => km
! -----------------------------------------------------------------
         else if( LM1.eq.1 .or. LP0.eq.km .or. 1.eq.1 ) then
             q2(i,j,k) = q1(i,j,LP0) + ( q1(i,j,LM1)-q1(i,j,LP0) )*( logpl2(i,j,k  )-logpl1(i,j,LP0) )
     .                                                            /( logpl1(i,j,LM1)-logpl1(i,j,LP0) )

! Interpolate Cubicly in LogP between other model levels
! ------------------------------------------------------
         else
              LP1 = LP0+1
              LM2 = LM1-1
             P    = logpl2(i,j,k)
             PLP1 = logpl1(i,j,LP1)
             PLP0 = logpl1(i,j,LP0)
             PLM1 = logpl1(i,j,LM1)
             PLM2 = logpl1(i,j,LM2)
             DLP0 = dlogp1(i,j,LP0)
             DLM1 = dlogp1(i,j,LM1)
             DLM2 = dlogp1(i,j,LM2)

              ap1 = (P-PLP0)*(P-PLM1)*(P-PLM2)/( DLP0*(DLP0+DLM1)*(DLP0+DLM1+DLM2) )
              ap0 = (PLP1-P)*(P-PLM1)*(P-PLM2)/( DLP0*      DLM1 *(     DLM1+DLM2) )
              am1 = (PLP1-P)*(PLP0-P)*(P-PLM2)/( DLM1*      DLM2 *(DLP0+DLM1     ) )
              am2 = (PLP1-P)*(PLP0-P)*(PLM1-P)/( DLM2*(DLM1+DLM2)*(DLP0+DLM1+DLM2) )

             q2(i,j,k) = ap1*q1(i,j,LP1) + ap0*q1(i,j,LP0) + am1*q1(i,j,LM1) + am2*q1(i,j,LM2)

         endif

      enddo
      enddo
      enddo

      return
      end
    
      SUBROUTINE GETVORDIV ( U,V,VOR,DIV,IM,JM )
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    U ....... ZONAL WIND                                    ****          
C ****    V ....... MERIDIONAL WIND                               ****          
C ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    VOR  (IM,JM) .... VORTICITY                             ****          
C ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
      real      U(IM,JM)                                               
      real      V(IM,JM)                                               
      real    VOR(IM,JM)                                               
      real    DIV(IM,JM)                                               
                                                                                
      real   P1X (IM,JM)                                               
      real   P1Y (IM,JM)                                               
      real   TMP1(IM,JM)                                               
      real   TMP2(IM,JM)                                               
      real  cosij(IM,JM)                                               
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      A    = 6.372e6
      pi   = 4.*atan(1.)
      dlon = 2*pi/ im
      dlat =   pi/(jm-1)
                                                                                
      C11 =  1.0 / (2.0*A*IM*(1.0-COS(0.5*dlat)))                           

      CX1 =  1.0 / (2.0*A*dlon)                                             
      CY1 =  1.0 / (2.0*A*dlat)                                             
                                                                                
      do j=2,jm-1
         phi = -pi/2.+(j-1)*dlat
      cosphi = cos(phi)
      do i=1,im
      cosij(i,j) = cosphi
      enddo
      enddo
      cosij(:,1)  = 0.0
      cosij(:,jm) = 0.0

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( U(ip1,j)+U(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = ( V(I,J+1)*COSIJ(I,J+1)+V(I,j)*COSIJ(I,j) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      DIV(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( V(ip1,j)+V(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = ( U(I,J+1)*COSIJ(I,J+1)+U(I,j)*COSIJ(I,j) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL VORTICITY           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         VOR (I,j) = ( TMP1(I,j)-TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      VOR(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GRADQ (Q,DQDX,DQDY,IM,JM)
C *********************************************************
C ****                                                 ****
C ****  THIS PROGRAM CALCULATES THE HORIZONTAL         ****
C ****  GRADIENT OF THE INPUT FIELD Q                  ****
C ****                                                 ****
C ****  ARGUMENTS:                                     ****
C ****    Q ....... FIELD TO BE DIFFERENTIATED         ****
C ****    DQDX .... LONGITUDINAL Q-DERIVATIVE          ****
C ****    DQDY .... MERIDIONAL   Q-DERIVATIVE          ****
C ****    IM ...... NUMBER OF LONGITUDINAL POINTS      ****
C ****    JM ...... NUMBER OF LATITUDINAL  POINTS      ****
C ****                                                 ****
C *********************************************************

      use MAPL_ConstantsMod
      implicit none
      integer  im,jm

      real    Q(IM,JM)
      real DQDX(IM,JM)
      real DQDY(IM,JM)
      real  Q1X(IM,JM)
      real  Q2X(IM,JM)
      real  Q1Y(IM,JM)
      real  Q2Y(IM,JM)
      real acos(JM)
      real sinl(IM)
      real cosl(IM)

      real cx1,cx2,cy1,cy2,uc,vc,us,vs
      real dl,dp,a,pi,fjeq,phi
      integer i,j,m,ip1,ip2,jpole,msgn

C *********************************************************
C ****               INITIALIZATION                    ****
C *********************************************************

      a  = MAPL_RADIUS
      pi = 4.0*atan(1.0)
      dl = 2.0*pi/im
      dp = pi/(jm-1)

      CX1 = 2.0 / ( 3.0*A*DL)
      CX2 = 1.0 / (12.0*A*DL)
      CY1 = 2.0 / ( 3.0*A*DP)
      CY2 = 1.0 / (12.0*A*DP)

      Q1X(:,:) = 0.0
      Q2X(:,:) = 0.0
      Q1Y(:,:) = 0.0
      Q2Y(:,:) = 0.0

      fjeq = ( jm+1 )*0.5
      do j=2,jm-1
          phi = dp * (j-fjeq)
      acos(j) = 1.0/( cos(phi) )
      enddo
      do i=1,im/2
      cosl(i)      = -cos((i-1)*dl)
      cosl(i+im/2) = -cosl(i)
      sinl(i)      = -sin((i-1)*dl)
      sinl(i+im/2) = -sinl(i)
      enddo

C *********************************************************
C ****          CALCULATE AVERAGE QUANTITIES           ****
C *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      Q1X(i  ,j) = Q(ip1,j) + Q(i,j)
      Q2X(ip1,j) = Q(ip2,j) + Q(i,j)
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=1,jm-1
      do i=1,im
      Q1Y(i,j) = Q(i,j+1) + Q(i,j)
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      Q2Y(i,j) = Q(i,j+1) + Q(i,j-1)
      enddo
      enddo

      do i=1,im/2
      Q2Y(i, 1) = Q(i,2)
      Q2Y(i,jm) = Q(i,jm-1)
      enddo

      do i=1,im/2
      Q2Y(i     , 1) = Q(i+im/2,2)    + Q2Y(i,1)
      Q2Y(i+im/2, 1) = Q2Y(i,1)
      Q2Y(i     ,jm) = Q(i+im/2,jm-1) + Q2Y(i,jm)
      Q2Y(i+im/2,jm) = Q2Y(i,jm)
      enddo

C *********************************************************
C ****             CALCULATE Q-GRADIENTS               ****
C *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      DQDX(ip1,j) =  ACOS(j) * ( ( Q1X(ip1,j)-Q1X(i,j) )*CX1
     .                         - ( Q2X(ip2,j)-Q2X(i,j) )*CX2 )
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      DQDY(i,j) = ( Q1Y(i,j)  -Q1Y(i,j-1) )*CY1
     .          - ( Q2Y(i,j+1)-Q2Y(i,j-1) )*CY2
      enddo
      enddo

C *********************************************************
C ****         CALCULATE Q-GRADIENTS (POLES)           ****
C *********************************************************

      do i=1,im/2
      Q1Y(i,      2) = Q(i,      1) + Q(i+im/2,2)
      Q1Y(i+im/2, 2) = Q(i+im/2, 1) + Q(i,     2)
      Q2Y(i,      1) = Q(i,      1) + Q(i+im/2,3)
      Q2Y(i+im/2, 1) = Q(i+im/2, 1) + Q(i,     3)

      Q1Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-1)
      Q1Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-1)
      Q2Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-2)
      Q2Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-2)
      enddo

      do i=1,im
      DQDY(i,jm) = ( Q1Y(i,jm)-Q1Y(i,jm-1) )*CY1
     .           - ( Q2Y(i,jm)-Q2Y(i,jm-1) )*CY2

      DQDY(i, 1) = ( Q1Y(i,1)-Q1Y(i,2) )*CY1
     .           - ( Q2Y(i,2)-Q2Y(i,1) )*CY2
      enddo

C   APPLY BOUNDARY CONDITIONS AT THE POLES
C ==========================================

      DO 170 M=1,2
      MSGN  = (-1)**M
      JPOLE = 1 + (M-1)*(jm - 1)

      VC = 0.0
      VS = 0.0
      DO 180 I=1,IM
      VC = VC + DQDY(I,JPOLE)*COSL(I)
      VS = VS + DQDY(I,JPOLE)*SINL(I)
  180 CONTINUE
      VC = 2.0 * VC / IM
      VS = 2.0 * VS / IM

      UC = - MSGN*VS
      US =   MSGN*VC

      DO 190 I=1,IM
      DQDX(I,JPOLE) = US*SINL(I) + UC*COSL(I)
      DQDY(I,JPOLE) = VS*SINL(I) + VC*COSL(I)
  190 CONTINUE

  170 CONTINUE

      RETURN
      END
      SUBROUTINE LAPLACIAN (DIV,VELP,im,jnp)

      integer        IM,JNP
      real       DIV(IM,JNP)
      real      VELP(IM,JNP)

      real*8, allocatable :: VP(:,:)
      real*8, allocatable ::  w(:)
      real*8, allocatable ::  bdtf(:)
      real*8, allocatable ::  bdts(:)
      real*8, allocatable ::  bdps(:)
      real*8, allocatable ::  bdpf(:)
      real*8  ts,tf,ps,pf,elmbda,pertrb,pi

      imp = im+1
      iwk = 11*jnp+6*imp

      allocate ( vp(jnp,imp) )
      allocate (    w(iwk)   )
      allocate ( bdtf(imp)   )
      allocate ( bdts(imp)   )
      allocate ( bdps(jnp)   )
      allocate ( bdpf(jnp)   )
 
      vp(:,:)=0.0
         w(:)=0.0
      bdtf(:)=0.0
      bdts(:)=0.0
      bdps(:)=0.0
      bdpf(:)=0.0
 
c Transpose the input array
c -------------------------
      do j=1,jnp
      do i=1,im
      vp(j,i)   = div(i,j)
      enddo
      vp(j,imp) =  vp(j,1)
      enddo

C === SET THE INPUT VARIABLES
      RAD = 6371000.0
      PI  = 3.14159265358979D0
      INTL=0
      TS=0.0
      TF=PI
      M=JNP-1
      MBDCND=9
      PS=0.0
      PF=2*PI
      N=IM
      NBDCND=0
      ELMBDA=0
      PERTRB=0
      IDIMF=M+1
 
      CALL PWSSSP (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,BDPS,
     *             BDPF,ELMBDA,VP,IDIMF,PERTRB,IERROR,W)

      if( ierror.ne.0 ) then
          print *, 'PWSSSP IERROR = ',ierror
          stop
      endif
 
c Scale by earth radius
c ---------------------
      do j=1,jnp
      do i=1,im
      VELP(I,J) = VP(J,I) * RAD * RAD
      enddo
      enddo
 
c Remove global mean
c ------------------
      CALL ZEROG (VELP,IM,JNP)
 
      deallocate (   vp )
      deallocate (    w )
      deallocate ( bdtf )
      deallocate ( bdts )
      deallocate ( bdps )
      deallocate ( bdpf )
      RETURN
      END

      SUBROUTINE ZEROG (VEL,IM,JNP)
      integer    IM,JNP
      real   VEL(IM,JNP)

      pi  = 4.0*atan(1.0)
      dl  = 2*pi/im
      dp  = pi/(jnp-1)
      cap = 1-cos(0.5*dp)

c Ensure unique pole values
c -------------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + vel(i,1)
      sum2 = sum2 + vel(i,jnp)
      enddo
      sum1 = sum1/im
      sum2 = sum2/im
      do i=1,im
      vel(i,1)   = sum1
      vel(i,jnp) = sum2
      enddo

c Compute global average
c ----------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + cap*vel(i,1)
      sum2 = sum2 + cap
      enddo

      do j=2,jnp-1
      cosj = cos( -pi/2 + (j-1)*dp )
      do i=1,im
      sum1 = sum1 + cosj*dp*vel(i,j)
      sum2 = sum2 + cosj*dp
      enddo
      enddo

      do i=1,im
      sum1 = sum1 + cap*vel(i,jnp)
      sum2 = sum2 + cap
      enddo

      qave = sum1/sum2

      do j=1,jnp
      do i=1,im
      vel(i,j) = vel(i,j)-qave
      enddo
      enddo

c     print *, 'Remove Global Average: ', qave

      RETURN
      END
