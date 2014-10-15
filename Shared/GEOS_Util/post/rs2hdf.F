      program  main
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to create an eta_hdf file from GEOS5 restarts    ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************


      character*256 dynrst, moistrst, topofile, bkgsfc, bkgeta,tag

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms
      integer im,jm,lm
      integer imo,jmo
      real*8  undef,rgas,rvap,eps,grav


! restart variables and topography
! --------------------------------
      real*8, allocatable ::   dp(:,:,:)
      real*8, allocatable ::    u(:,:,:)
      real*8, allocatable ::    v(:,:,:)
      real*8, allocatable ::   th(:,:,:)
      real*8, allocatable ::  thv(:,:,:)
      real*8, allocatable ::   pk(:,:,:)
      real*8, allocatable ::  ple(:,:,:)
      real*8, allocatable ::    q(:,:,:)
      real*8, allocatable ::   ps(:,:)
      real*8, allocatable ::   ak(:)
      real*8, allocatable ::   bk(:)

      real*8, allocatable ::  phis     (:,:)
      real*8, allocatable ::  topo_std (:,:)
      real*8, allocatable ::  oro      (:,:)
      real*8, allocatable ::  frland   (:,:)
      real*8, allocatable ::  frlandice(:,:)
      real*8, allocatable ::  frlake   (:,:)
      real*8, allocatable ::  frocean  (:,:)
      real*8, allocatable ::  u10m     (:,:)
      real*8, allocatable ::  v10m     (:,:)
      real*8, allocatable ::  ts       (:,:)
      real*8, allocatable ::  qs       (:,:)
      real*8, allocatable ::  snowmass (:,:)
      real*8, allocatable ::  wet1     (:,:)
      real*8, allocatable ::  tsoil1   (:,:)
      real*8, allocatable ::  oz       (:,:,:)

      real*4, allocatable ::  dum(:,:)

      character*256, allocatable :: arg(:)
      character*5    extn
      character*8    date
      character*2    hour
      integer n,nargs,iargc,i,j,L,nymd0,nhms0
      integer precision
      logical file_exists
      logical dtoa

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

          dtoa  = .false.
          undef = 1.0e15
            tag = 'geos5'
         dynrst = 'xxx'
       moistrst = 'xxx'
       topofile = 'xxx'
          nymd0 = -999
          nhms0 = -999
          imo   = -999
          jmo   = -999
      precision = 0  ! 32-bit: 0, 64-bit: 1
           extn = 'nc4'

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else

      allocate ( arg(nargs) )
      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
             if( trim(arg(n)).eq.'-h'        ) call usage()
             if( trim(arg(n)).eq.'-help'     ) call usage()
             if( trim(arg(n)).eq.'-H'        ) call usage()
             if( trim(arg(n)).eq.'-Help'     ) call usage()
             if( trim(arg(n)).eq.'-tag'    ) then
                                   tag   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-dynrst'   ) then
                                   dynrst  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo'    ) then
                                   topofile = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-moistrst' ) then
                                   moistrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-dtoa' ) dtoa = .true.
             if( trim(arg(n)).eq.'-im'   ) read(arg(n+1),*) imo
             if( trim(arg(n)).eq.'-jm'   ) read(arg(n+1),*) jmo
             if( trim(arg(n)).eq.'-nymd' ) read(arg(n+1),*) nymd0
             if( trim(arg(n)).eq.'-nhms' ) read(arg(n+1),*) nhms0
             if( trim(arg(n)).eq.'-precision' ) read(arg(n+1),*) precision
             if( trim(arg(n)).eq.'-ext'  ) read(arg(n+1),*) extn
      enddo
      endif

      if( trim(dynrst)  .eq.'xxx'   .or.
     .    trim(moistrst).eq.'xxx' ) then
          print *
          call usage()
      endif

      print *
      print *, '                    tag: ',trim(tag)
      print *, '  dyn  restart filename: ',trim(dynrst)
      print *, 'moist  restart filename: ',trim(moistrst)
      if( trim(topofile).ne.'xxx' ) then
      print *, '          topo filename: ',trim(topofile)
      endif

! **********************************************************************
! ****                   Read dycore internal Restart               ****
! **********************************************************************

      inquire ( file=trim(dynrst), exist=file_exists )
      if( file_exists ) then
      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

      if( nymd0.ne.-999 ) then
          nymd = nymd0
      else
          nymd = headr1(1)*10000 + headr1(2)*100 + headr1(3)
      endif
      if( nhms0.ne.-999 ) then
          nhms = nhms0
      else
          nhms = headr1(4)*10000 + headr1(5)*100 + headr1(6)
      endif

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)
      if( imo .eq. -999 ) imo = im
      if( jmo .eq. -999 ) jmo = jm

          print *, '      input resolution: ',im,jm,lm
          print *, '     output resolution: ',imo,jmo,lm
          print *, '                  date: ',nymd,nhms
          if( precision.eq.0 ) then
          print *, '             precision: 0 (32-bit)'
          endif
          if( precision.eq.1 ) then
          print *, '             precision: 1 (64-bit)'
          endif
          print *

          allocate (    u(im,jm,lm)    )
          allocate (    v(im,jm,lm)    )
          allocate (   th(im,jm,lm)    )
          allocate (  thv(im,jm,lm)    )
          allocate (   dp(im,jm,lm)    )
          allocate (   pk(im,jm,lm)    )
          allocate (  ple(im,jm,lm+1)  )
          allocate (   ps(im,jm)       )
          allocate (   ak(lm+1)        )
          allocate (   bk(lm+1)        )

          read (10) ak
          read (10) bk

          do L=1,lm
             read(10) ((  u(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) ((  v(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) (( th(i,j,L),i=1,im),j=1,jm)  ! Note: GEOS-5 variable is DRY potential temperature
          enddo
          do L=1,lm+1
             read(10) ((ple(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) (( pk(i,j,L),i=1,im),j=1,jm)
          enddo

          close (10)

            ps(:,:) = ple(:,:,lm+1)
          do L=lm,1,-1
          dp(:,:,L) = ple(:,:,L+1) - ple(:,:,L)
          enddo
      else
          print *, trim(dynrst),' does not exist'
          stop 
      endif

      print *
      write(6,500)
 500  format(2x,' k ','      A(k)    ',2x,' B(k)   ',2x,'  Pref    ',2x,'  DelP',/,
     .         1x,'----',3x,'----------',2x,'--------',2x,'----------',2x,'---------' )
           L=1
        write(6,501) L,ak(L)*0.01, bk(L), ak(L)*0.01 + 1000.0*bk(L)
      do L=2,lm+1
        write(6,502) L,ak(L)*0.01, bk(L), ak(L)*0.01 + 1000.0*bk(L),
     .               (ak(L)-ak(L-1))*0.01 + 1000.0*(bk(L)-bk(L-1))
      enddo

      print *
 501  format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4)
 502  format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4,3x,f8.4)

! **********************************************************************
! ****                   Read moist internal Restart                ****
! **********************************************************************

      allocate (   q(im,jm,lm) )
      allocate ( dum(im,jm)    )

      inquire ( file=trim(moistrst), exist=file_exists )
      if( file_exists ) then
      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      do L=1,lm
         read(10)   dum 
         q(:,:,L) = dum(:,:)  ! First moist variable is SPHU
      enddo
      close (10)
      endif

! **********************************************************************
! ****                          Read topo file                      ****
! **********************************************************************

      allocate ( phis     (im,jm)    )
      allocate ( topo_std (im,jm)    )
      allocate ( oro      (im,jm)    )
      allocate ( frland   (im,jm)    )
      allocate ( frlandice(im,jm)    )
      allocate ( frlake   (im,jm)    )
      allocate ( frocean  (im,jm)    )
      allocate ( u10m     (im,jm)    )
      allocate ( v10m     (im,jm)    )
      allocate ( ts       (im,jm)    )
      allocate ( qs       (im,jm)    )
      allocate ( snowmass (im,jm)    )
      allocate ( wet1     (im,jm)    )
      allocate ( tsoil1   (im,jm)    )
      allocate ( oz       (im,jm,lm) )

      phis = 0.0
      inquire ( file=trim(topofile), exist=file_exists )
      if( file_exists ) then
      open  (10,file=trim(topofile),form='unformatted',access='sequential')
      print *, 'Reading topography from: ',trim(topofile)
        read(10)   dum ; phis = dum
      close (10)
      endif

! **********************************************************************
! ****                       Adjust Fields                          ****
! **********************************************************************

      grav = 9.8
      rgas = 8314.3/28.97
      rvap = 8314.3/18.01
      eps  = rvap/rgas-1.0
      thv  = th*(1+eps*q)      ! Construct virtual potential temperature

      phis     = phis*grav
      topo_std = 0.0           ! Analysis needs to read it, but doesn't use it
      snowmass = snowmass/1000 ! Convert millimeter to meter
            oz = oz/1.655e-6

                                                  oro = 1.0  ! Land
      where ( frocean >= 0.6 .or. frlake >= 0.6 ) oro = 0.0  ! Water
      where ( oro == 0.0 .and. ts <= 272.81 )     oro = 2.0  ! Ice

      do j=1,jm
      do i=1,im
      wet1(i,j) = ( frland(i,j)*wet1(i,j) + 1.0-frland(i,j) )
      if( oro(i,j) == 1.0 ) then
       tsoil1(i,j) = tsoil1(i,j) + 273.16
      else
       tsoil1(i,j) = undef
      endif
      enddo
      enddo

      where ( frlandice == 1.0 ) snowmass = 4.0

! **********************************************************************
! ****                   Write Analysis BKG Files                   ****
! **********************************************************************

      write(date,101) nymd
      write(hour,102) nhms/10000
  101 format(i8.8)
  102 format(i2.2)

      bkgeta = trim(tag) // '.bkg.eta.' // date // '_' // hour // 'z.'//trim(extn)

      print *, 'Creating ',trim(tag),' bkg.eta HDF file: ',trim(bkgeta)
      print *

      call hflip ( phis,im,jm, 1 )
      call hflip ( ps  ,im,jm, 1 )
      call hflip ( dp  ,im,jm,lm )
      call hflip ( u   ,im,jm,lm )
      call hflip ( v   ,im,jm,lm )
      call hflip ( thv ,im,jm,lm )
      call hflip ( q   ,im,jm,lm )
      call hflip ( oz  ,im,jm,lm )

      call hflip ( u10m     ,im,jm, 1 )
      call hflip ( v10m     ,im,jm, 1 )
      call hflip ( ts       ,im,jm, 1 )
      call hflip ( qs       ,im,jm, 1 )
      call hflip ( snowmass ,im,jm, 1 )
      call hflip ( wet1     ,im,jm, 1 )
      call hflip ( tsoil1   ,im,jm, 1 )
      call hflip ( oro      ,im,jm, 1 )
      call hflip ( frland   ,im,jm, 1 )
      call hflip ( frlandice,im,jm, 1 )
      call hflip ( frlake   ,im,jm, 1 )
      call hflip ( frocean  ,im,jm, 1 )

      call dynhdf ( bkgeta,nymd,nhms,im,jm,lm,ak,bk,
     .              phis,topo_std,ts,oro,
     .              ps,dp,u,v,thv,q,oz,precision,imo,jmo,dtoa )

      stop
      end

      subroutine minmax (q,im,jm,L)
      real*8 q(im,jm)
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

      subroutine dynhdf ( filename,nymd,nhms,im,jm,lm,ak,bk,
     .                    phis,topo_std,ts,oro,ps,dp,u,v,thv,q,oz,precision,imo,jmo,dtoa )
      implicit  none
      integer   im,jm,lm,nymd,nhms
      integer   imo,jmo
      real*8      sump(im,jm)
      real*8      phis(im,jm)
      real*8       slp(im,jm)
      real*8        ps(im,jm)
      real*8        ts(im,jm)
      real*8       oro(im,jm)
      real*8  topo_std(im,jm)
      real*8       dp(im,jm,lm)
      real*8        u(im,jm,lm)
      real*8        v(im,jm,lm)
      real*8        t(im,jm,lm)
      real*8      thv(im,jm,lm)
      real*8        q(im,jm,lm)
      real*8       oz(im,jm,lm)
      real*8       pk(im,jm,lm)
      real*8      pke(im,jm,lm+1)
      real*8      ple(im,jm,lm+1)
      real*8      phi(im,jm,lm+1)
      real*8       ak(lm+1)
      real*8       bk(lm+1)

      real lats(jmo)
      real lons(imo)
      real levs(lm)
      real*8 latsd(jmo)
      real*8 lonsd(imo)

      logical dtoa
      real  ptop,dlon,dlat,pref,undef,pint
      integer i,j,L,n,timeinc,rc,ks
      character*80 filename
      character*3  ch3
      integer nvars,fid,precision

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact
      character*256, allocatable ::   vname(:)
      character*256, allocatable ::  vtitle(:)
      character*256, allocatable ::  vunits(:)
      integer,       allocatable ::   lmvar(:)
      real,          allocatable :: v_range(:,:)
      real,          allocatable :: p_range(:,:)

      real,          allocatable :: dum1(:)
      real,          allocatable :: dum2(:,:)
      real,          allocatable :: dum3(:,:,:)
      real,          allocatable :: dumu(:,:,:)
      real,          allocatable :: dumv(:,:,:)
      real   rgas,rvap,eps,kappa,grav,cp

      real   dpref
             dpref(L) = ( ak(L+1)-ak(L) ) +  ( bk(L+1)-bk(L) ) * 98400.0

      undef     = 1.0e15
      timeinc   = 060000

      cp    = 1004.16
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      kappa = 2.0/7.0
      grav  = 9.81   

! String and vars settings
! ------------------------
      title    = 'GEOS5 Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'hPa'

      nvars = 14
      allocate (     vname(nvars) )
      allocate (    vtitle(nvars) )
      allocate (    vunits(nvars) )
      allocate (     lmvar(nvars) )
      allocate ( v_range(2,nvars) )
      allocate ( p_range(2,nvars) )

       vname(01) = 'phis'
      vtitle(01) = 'Topography geopotential'
      vunits(01) = 'meter2/sec2'
       lmvar(01) =  0

       vname(02) = 'hs_stdv'
      vtitle(02) = 'Topography Height Standard Deviation'
      vunits(02) = 'm'
       lmvar(02) =  0

       vname(03) = 'ts'
      vtitle(03) = 'Surface temperature'
      vunits(03) = 'K'
       lmvar(03) =  0

       vname(04) = 'lwi'
      vtitle(04) = 'Land-water-ice mask'
      vunits(04) = 'na'
       lmvar(04) =  0

       vname(05) = 'ps'
      vtitle(05) = 'Surface Pressure'
      vunits(05) = 'Pa'
       lmvar(05) =  0

       vname(06) = 'delp'
      vtitle(06) = 'Pressure Thickness'
      vunits(06) = 'Pa'
       lmvar(06) =  lm

       vname(07) = 'uwnd'
      vtitle(07) = 'Zonal Wind'
      vunits(07) = 'm/s'
       lmvar(07) =  lm

       vname(08) = 'vwnd'
      vtitle(08) = 'Meridional Wind'
      vunits(08) = 'm/s'
       lmvar(08) =  lm

       vname(09) = 'theta'
      vtitle(09) = 'Scaled Virtual Potential Temperature'
      vunits(09) = 'K/Pa^kappa'
       lmvar(09) =  lm

       vname(10) = 'sphu'
      vtitle(10) = 'Specific Humidity'
      vunits(10) = 'kg/kg'
       lmvar(10) =  lm

       vname(11) = 'ozone'
      vtitle(11) = 'Ozone Mixing Ratio'
      vunits(11) = 'kg/kg'
       lmvar(11) =  lm

       vname(12) = 'slp'
      vtitle(12) = 'Sea Level Pressure'
      vunits(12) = 'Pa'
       lmvar(12) =  0

       vname(13) = 'tmpu'
      vtitle(13) = 'Temperature'
      vunits(13) = 'K'
       lmvar(13) =  lm

       vname(14) = 'zle'
      vtitle(14) = 'Edge Heights'
      vunits(14) = 'm'
       lmvar(14) =  lm

      v_range(:,:) = undef
      p_range(:,:) = undef

! Compute grid
! ------------
      dlon = 360.0/ imo
      dlat = 180.0/(jmo-1)

      do j=1,jmo
      latsd(j) = -90.0 + (j-1)*dlat
      enddo
      do i=1,imo
      lonsd(i) =   0.0 + (i-1)*dlon
      enddo
      do L=1,lm
      levs(L) = L
      enddo
      lats = latsd
      lons = lonsd

      sump(:,:) = 0.0
      do L=1,lm
      sump(:,:) = sump(:,:) + dp(:,:,L)
      enddo
      sump(:,:) = ps(:,:)-sump(:,:)
!     print *, 'ptop values (mb): ',sump/100
      ptop = sump(1,1)

          ptop = ak(1)
       levs(1) = ptop + 0.5 * dpref(1)
      do L = 2, lm
       levs(L) = levs(L-1) + 0.5 * ( dpref(L-1) + dpref(L) )
      enddo
       levs(1:lm) = levs(1:lm) / 100.0
 
! Create Dry Temperature and SLP
! ------------------------------
      do L=1,lm+1
      ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
      enddo
      pke(:,:,:) = ple(:,:,:)**kappa
      phi(:,:,lm+1) = phis(:,:)
      do L=lm,1,-1
      phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
       pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
     .              / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo
      t = thv*pk
      call get_slp ( ps,phis,slp,ple,pk,t,rgas,grav,im,jm,lm )
      t = t/(1+eps*q)
      phi = phi/grav

! Create GFIO file
! ----------------
      call GFIO_Create ( filename, title, source, contact, undef,
     .                   imo, jmo, lm, lons, lats, levs, levunits,
     .                   nymd, nhms, timeinc,
     .                   nvars, vname, vtitle, vunits, lmvar,
     .                   v_range, p_range, precision,
     .                   fid, rc )
! Write GFIO data
! ---------------
      allocate( dum1(lm+1)     )
      allocate( dum2(im,jm)    )
      allocate( dum3(im,jm,lm) )
      allocate( dumu(im,jm,lm) )
      allocate( dumv(im,jm,lm) )

      dumu = u
      dumv = v
      if( dtoa) call dtoa_winds ( dumu,dumv,dumu,dumv,im,jm,lm )

      dum2 = phis     ; call      putVar ( fid,vname(01),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum2 = topo_std ; call      putVar ( fid,vname(02),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum2 = ts       ; call      putVar ( fid,vname(03),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum2 = oro      ; call      putVar ( fid,vname(04),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum2 = ps       ; call      putVar ( fid,vname(05),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum3 = dp       ; call      putVar ( fid,vname(06),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum3 = dumu     ; call      putVar ( fid,vname(07),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum3 = dumv     ; call      putVar ( fid,vname(08),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum3 = thv      ; call      putVar ( fid,vname(09),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum3 = q        ; call      putVar ( fid,vname(10),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum3 = oz       ; call      putVar ( fid,vname(11),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )
      dum2 = slp      ; call      putVar ( fid,vname(12),nymd,nhms,im,jm,imo,jmo,0, 1,dum2,rc )
      dum3 = t        ; call      putVar ( fid,vname(13),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )

      dum3 = phi(:,:,1:lm) ; call      putVar ( fid,vname(14),nymd,nhms,im,jm,imo,jmo,1,lm,dum3,rc )

! Write GFIO global attributes
! ----------------------------
      ks   = 0
      pint = ak(ks+1)
                  call GFIO_PutIntAtt  ( fid,'nstep',  1,ks   ,0        ,rc )
                  call GFIO_PutRealAtt ( fid,'ptop',   1,ptop ,precision,rc )
                  call GFIO_PutRealAtt ( fid,'pint',   1,pint ,precision,rc )
                  call GFIO_PutIntAtt  ( fid,'ks',     1,ks   ,0        ,rc )
      dum1 = ak ; call GFIO_PutRealAtt ( fid,'ak',  lm+1,dum1 ,precision,rc )
      dum1 = bk ; call GFIO_PutRealAtt ( fid,'bk',  lm+1,dum1 ,precision,rc )


      call gfio_close ( fid,rc )
      return
      end

      subroutine putvar (id,name,nymd,nhms,im,jm,imo,jmo,lbeg,lm,q,rc)
      implicit none
      integer id,nymd,nhms,im,jm,lbeg,lm,rc
      integer imo,jmo
      real undef
      real q (im ,jm ,lm)
      real qo(imo,jmo,lm)
      character(*) name
      undef = 1e15
      if( im.ne.imo .or. jm.ne.jmo ) then
          call hinterp ( q,im,jm,qo,imo,jmo,lm,undef )
      else
          qo = q
      endif
      call Gfio_putVar ( id,name,nymd,nhms,imo,jmo,lbeg,lm,qo,rc )
      return
      end

      subroutine sfchdf ( filename,nymd,nhms,im,jm,lm,
     .                    phis,u10m,v10m,ts,qs,
     .                    snomas,wet1,tsoil1,
     .                    frland,frlandice,frlake,frocean,oro,precision )
      implicit  none
      integer   im,jm,lm,nymd,nhms
      real*8      phis(im,jm)
      real*8      u10m(im,jm)
      real*8      v10m(im,jm)
      real*8        ts(im,jm)
      real*8        qs(im,jm)
      real*8    snomas(im,jm)
      real*8      wet1(im,jm)
      real*8    tsoil1(im,jm)
      real*8    frland(im,jm)
      real*8 frlandice(im,jm)
      real*8    frlake(im,jm)
      real*8   frocean(im,jm)
      real*8       oro(im,jm)

      real lats(jm)
      real lons(im)
      real levs(lm)
      real*8 latsd(jm)
      real*8 lonsd(im)

      real*8  ptop,dlon,dlat,pref,undef
      integer i,j,L,n,timeinc,rc
      character*80 filename
      character*3  ch3
      integer nvars,fid,precision

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact
      character*256, allocatable ::   vname(:)
      character*256, allocatable ::  vtitle(:)
      character*256, allocatable ::  vunits(:)
      integer,       allocatable ::   lmvar(:)
      real,          allocatable :: v_range(:,:)
      real,          allocatable :: p_range(:,:)

      undef     = 1.0e15
      timeinc   = 060000

! String and vars settings
! ------------------------
      title    = 'GEOS5 Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'hPa'

      nvars = 13
      allocate (     vname(nvars) )
      allocate (    vtitle(nvars) )
      allocate (    vunits(nvars) )
      allocate (     lmvar(nvars) )
      allocate ( v_range(2,nvars) )
      allocate ( p_range(2,nvars) )

       vname(01) = 'PHIS'
      vtitle(01) = 'Topography geopotential'
      vunits(01) = 'meter2/sec2'
       lmvar(01) =  0

       vname(02) = 'TSKIN'
      vtitle(02) = 'Surface skin temperature'
      vunits(02) = 'K'
       lmvar(02) =  0

       vname(03) = 'QS'
      vtitle(03) = 'Surface Specific Humidity'
      vunits(03) = 'kg/kg'
       lmvar(03) =  0

       vname(04) = 'U10M'
      vtitle(04) = '10 meter U Wind'
      vunits(04) = 'm/s'
       lmvar(04) =  0

       vname(05) = 'V10M'
      vtitle(05) = '10 meter V Wind'
      vunits(05) = 'm/s'
       lmvar(05) =  0

       vname(06) = 'SNOWDP'
      vtitle(06) = 'Snow Depth Water Equivalent'
      vunits(06) = 'm'
       lmvar(06) =  0

       vname(07) = 'GWETTOP'
      vtitle(07) = 'Top soil layer wetness'
      vunits(07) = '0-1'
       lmvar(07) =  0

       vname(08) = 'TSOIL1'
      vtitle(08) = 'Top soil layer wetness'
      vtitle(08) = 'Surface skin temperature'
      vunits(08) = 'K'
       lmvar(08) =  0

       vname(09) = 'FRLAND'
      vtitle(09) = 'Fraction of Land'
      vunits(09) = '0-1'
       lmvar(09) =  0

       vname(10) = 'FRLANDICE'
      vtitle(10) = 'Fraction of Land Ice'
      vunits(10) = '0-1'
       lmvar(10) =  0

       vname(11) = 'FRLAKE'
      vtitle(11) = 'Fraction of Lake'
      vunits(11) = '0-1'
       lmvar(11) =  0

       vname(12) = 'FROCEAN'
      vtitle(12) = 'Fraction of Ocean'
      vunits(12) = '0-1'
       lmvar(12) =  0

       vname(13) = 'ORO'
      vtitle(13) = 'Water(0) Land(1) Ice(2) Flag'
      vunits(13) = '0-1-2'
       lmvar(13) =  0

      v_range(:,:) = undef
      p_range(:,:) = undef

! Compute grid
! ------------
      dlon = 360.0/ im
      dlat = 180.0/(jm-1)

      do j=1,jm
      latsd(j) = -90.0 + (j-1)*dlat
      enddo
      do i=1,im
      lonsd(i) =   0.0 + (i-1)*dlon
      enddo
      do L=1,lm
      levs(L) = L
      enddo
      lats = latsd
      lons = lonsd

! Create GFIO file
! ----------------
      call GFIO_Create ( filename, title, source, contact, undef,
     .                   im, jm, lm, lons, lats, levs, levunits,
     .                   nymd, nhms, timeinc,
     .                   nvars, vname, vtitle, vunits, lmvar,
     .                   v_range, p_range, precision,
     .                   fid, rc )
! Write GFIO data
! ---------------
      call Gfio_putVar ( fid,vname(01),nymd,nhms,im,jm,0, 1,phis     ,rc )
      call Gfio_putVar ( fid,vname(02),nymd,nhms,im,jm,0, 1,ts       ,rc )
      call Gfio_putVar ( fid,vname(03),nymd,nhms,im,jm,0, 1,qs       ,rc )
      call Gfio_putVar ( fid,vname(04),nymd,nhms,im,jm,0, 1,u10m     ,rc )
      call Gfio_putVar ( fid,vname(05),nymd,nhms,im,jm,0, 1,v10m     ,rc )
      call Gfio_putVar ( fid,vname(06),nymd,nhms,im,jm,0, 1,snomas   ,rc )
      call Gfio_putVar ( fid,vname(07),nymd,nhms,im,jm,0, 1,wet1     ,rc )
      call Gfio_putVar ( fid,vname(08),nymd,nhms,im,jm,0, 1,tsoil1   ,rc )
      call Gfio_putVar ( fid,vname(09),nymd,nhms,im,jm,0, 1,frland   ,rc )
      call Gfio_putVar ( fid,vname(10),nymd,nhms,im,jm,0, 1,frlandice,rc )
      call Gfio_putVar ( fid,vname(11),nymd,nhms,im,jm,0, 1,frlake   ,rc )
      call Gfio_putVar ( fid,vname(12),nymd,nhms,im,jm,0, 1,frocean  ,rc )
      call Gfio_putVar ( fid,vname(13),nymd,nhms,im,jm,0, 1,oro      ,rc )

! Write GFIO global attributes
! ----------------------------
!     call GFIO_PutRealAtt ( fid,'ptop',   1,ptop ,precision,rc )

      call gfio_close ( fid,rc )
      return
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer    im,jm,lm,i,j,L
      real*8   q(im,jm,lm),dum(im)
      do L=1,lm
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j,L)
         dum(i+im/2) = q(i,j,L)
      enddo
         q(:,j,L) = dum(:)
      enddo
      enddo
      return
      end

      subroutine get_slp ( ps,phis,slp,pe,pk,tv,rgas,grav,im,jm,km )
      implicit   none
      integer  im,jm,km
      real     grav
      real     rgas
      real*8   pk(im,jm,km)    ! layer-mean P**kappa
      real*8   tv(im,jm,km)    ! layer-mean virtual Temperature
      real*8   pe(im,jm,km+1)  ! press at layer edges (Pa)
      real*8   ps(im,jm)       ! surface pressure (Pa)
      real*8 phis(im,jm)       ! surface geopotential
      real*8  slp(im,jm)       ! sea-level pressure (hPa)

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

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " rs2hdf.x  [-dynrst     dynrst_fname]  Default: fvcore_internal_restart"
      print *, "           [-moistrst moistrst_fname]  Default: moist_internal_restart"
      print *, "           [-topo         topo_fname] Optional  Topography File"
      print *, "           [-tag                 tag] Optional  Tag"
      print *, "           [-nymd           yyyymmdd] Optional  Overriding DateStamp for Output"
      print *, "           [-nhms             hhmmss] Optional  Overriding TimeStamp for Output"
      print *, "           [-ext  filename extension] Optional  Overriding Output filename extension"
      print *
      print *, "where:"
      print *, "-----"
      print *, "  -dynrst     dynrst_fname:  Filename of dynamics internal restart"
      print *, "  -moistrst moistrst_fname:  Filename of    moist internal restart"
      print *
      print *, "creates:"
      print *, "-------"
      print *, "     tag.bkg.eta.yyyymmdd_hhz.nc4"
      print *
      call exit(7)
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
     .                qout,iout*jout,lons,lats,undef )

      return
      end subroutine hinterp

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

      return
      end subroutine interp_h
