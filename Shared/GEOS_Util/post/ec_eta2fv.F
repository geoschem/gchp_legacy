      program  main
      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****     Program to create fv restarts from ECMWF ETA Files       ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm,nq
      real     pbelow,pabove,ptop

c Set analysis, fvdas, date and time
c ----------------------------------
      character*1  hres
      character*1  string(256), blank(256)
      character*2  clm,cnhms
      character*8  cnymd

      character*256 ana_data, fv_data, topog, fv_rst, tag, ext
      data         fv_rst   /'gg2fv.rst.lcv.yyyymmdd_hhz.bin'/
      data         blank /256*' '/

      equivalence ( blank (01),tag   )
      equivalence ( string(01),fv_rst)
      equivalence ( string(15),cnymd )
      equivalence ( string(24),cnhms )

      real    ::      kappa = 2.0/7.0
      logical ::  add_ozone = .false.

      integer    nymd,nhms

c fv restart variables and topography
c -----------------------------------
      real, allocatable ::   dp(:,:,:)
      real, allocatable ::   pl(:,:,:)
      real, allocatable ::  ple(:,:,:)
      real, allocatable ::    u(:,:,:)
      real, allocatable ::    v(:,:,:)
      real, allocatable ::   tv(:,:,:)
      real, allocatable ::  thv(:,:,:)
      real, allocatable ::  pke(:,:,:)
      real, allocatable ::  pk (:,:,:)
      real, allocatable ::    q(:,:,:,:)
      real, allocatable :: phis(:,:)
      real, allocatable ::   ps(:,:)

      real,    allocatable ::     ak(:)
      real,    allocatable ::     bk(:)

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
      real, allocatable ::    h_ana(:,:,:)
      real, allocatable ::    q_ana(:,:,:,:)
      real, allocatable ::    p_ana(:,:,:)
      real, allocatable ::   dp_ana(:,:,:)
      real, allocatable ::    t_ana(:,:,:)
      integer ID,mlev,rc
      integer imax,jmax,ntime,nvars,ngatts
      integer ianal,imethod,igrid,irst

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

      logical ihavetv,agridw
      integer precision
      integer L,n,nargs,iargc,lrec

      character*256  ctlfile,format
      integer imncep,jmncep,lmncep,nvncep
      real     uncep

      character*256, pointer :: names (:)
      character*256, pointer :: descs (:)
      integer,       pointer :: lmvars(:)
      real,          pointer ::   levs(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)

C **********************************************************************
C ****           Initialize Filenames, Methods, etc.                ****
C **********************************************************************

             nq = 2       ! 1:qv, 2:oz
           mlev = -999
         pabove = 10.00   ! 10 mb
         pbelow = 30.00   ! 30 mb
      precision = 0       ! 32-bit

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-rslv' ) read(arg(n+1),600) hres,lm
             if( trim(arg(n)).eq.'-nymd' ) read(arg(n+1), * ) nymd
             if( trim(arg(n)).eq.'-nhms' ) read(arg(n+1), * ) nhms
             if( trim(arg(n)).eq.'-date' ) read(arg(n+1), * ) nymd
             if( trim(arg(n)).eq.'-time' ) read(arg(n+1), * ) nhms
             if( trim(arg(n)).eq.'-mlev' ) read(arg(n+1), * ) mlev
             if( trim(arg(n)).eq.'-tag'  )      tag = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ecmwf') ana_data = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ana'  )  fv_data = trim(arg(n+1))
             if( trim(arg(n)).eq.'-eta'  )  fv_data = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ozone')  add_ozone = .true.
             if( trim(arg(n)).eq.'-plow ') read(arg(n+1), * ) pbelow
             if( trim(arg(n)).eq.'-phigh') read(arg(n+1), * ) pabove
          enddo

          print *
          print *, '     ECMWF filename: ',trim(ana_data)
          print *, 'Background filename: ',trim( fv_data)
          print *, '         Output Tag: ',trim( tag )
          print *, '             nymd: ',nymd
          print *, '             nhms: ',nhms
          if( mlev.ne.-999 ) print *, '             mlev: ',mlev
          print *
          print *, 'Blending between ',pbelow,' and ',pabove,' mb'
          print *

      endif
         pabove = pabove*100
         pbelow = pbelow*100

          if( trim(tag).ne.'' ) tag = trim(tag) // '.'
            n = index(trim(fv_data),'.',back=.true.)
          ext = trim(fv_data(n+1:))

C **********************************************************************
C ****               Read Background/Analysis ETA File              ****
C **********************************************************************

          call gfio_open ( trim(fv_data),1,ID,rc )
          call gfio_diminquire ( ID,im,jm,lm,ntime,nvars,ngatts,rc )

          allocate ( lon(im) )
          allocate ( lat(jm) )
          allocate ( lev(lm) )
          allocate ( yymmdd(ntime) )
          allocate ( hhmmss(ntime) )
          allocate (  vname(nvars) )
          allocate ( vtitle(nvars) )
          allocate ( vunits(nvars) )
          allocate (  kmvar(nvars) )
          allocate ( vrange(2,nvars) )
          allocate ( prange(2,nvars) )

          call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                        title,source,contact,undef,
     .                        lon,lat,lev,levunits,
     .                        yymmdd,hhmmss,timinc,
     .                        vname,vtitle,vunits,kmvar,
     .                        vrange,prange,rc )

c Check FVETA Variable Names
c --------------------------
          do n=1,nvars
          if( trim(vname(n)).eq.'u'     ) u_agrid = .true.
          if( trim(vname(n)).eq.'v'     ) v_agrid = .true.
          if( trim(vname(n)).eq.'uwnd'  ) u_dgrid = .true.
          if( trim(vname(n)).eq.'vwnd'  ) v_dgrid = .true.
          if( trim(vname(n)).eq.'tv'    )  tvflag = .true.
          if( trim(vname(n)).eq.'theta' ) thvflag = .true.
          enddo
          agrid = u_agrid .and. v_agrid
          dgrid = u_dgrid .and. v_dgrid

c Allocate Space
c --------------
          allocate ( phis(im,jm) )

          allocate (  ps(im,jm)       )
          allocate (  dp(im,jm,lm)    )
          allocate (   u(im,jm,lm)    )
          allocate (   v(im,jm,lm)    )
          allocate (  tv(im,jm,lm)    )
          allocate ( thv(im,jm,lm)    )
          allocate (   q(im,jm,lm,nq) )

c Read Variables
c --------------
          call gfio_getvar ( id,'phis'   ,nymd,nhms,im,jm,0,1 ,phis,rc )
          call gfio_getvar ( id,'ps'     ,nymd,nhms,im,jm,0,1 ,ps  ,rc )
          call gfio_getvar ( id,'delp'   ,nymd,nhms,im,jm,1,lm,dp  ,rc )

          if( agrid ) then
              call gfio_getvar ( id,'u',nymd,nhms,im,jm,1,lm,u,rc )
              call gfio_getvar ( id,'v',nymd,nhms,im,jm,1,lm,v,rc )
          endif

          if( dgrid ) then
              call gfio_getvar ( id,'uwnd',nymd,nhms,im,jm,1,lm,u,rc )
              call gfio_getvar ( id,'vwnd',nymd,nhms,im,jm,1,lm,v,rc )
              call dtoa ( u,u,im,jm,lm,2 )
              call dtoa ( v,v,im,jm,lm,1 )
          endif

          if( tvflag ) then
              call gfio_getvar ( id,'tv',nymd,nhms,im,jm,1,lm,tv,rc )
          endif

          if( thvflag ) then
              call gfio_getvar ( id,'theta',nymd,nhms,im,jm,1,lm,thv,rc )
          endif

          call gfio_getvar ( id,'sphu'   ,nymd,nhms,im,jm,1,lm,q(1,1,1,1),rc )
          call gfio_getvar ( id,'ozone'  ,nymd,nhms,im,jm,1,lm,q(1,1,1,2),rc )
c         call gfio_getvar ( id,'qltot'  ,nymd,nhms,im,jm,1,lm,q(1,1,1,3),rc )
c         call gfio_getvar ( id,'qitot'  ,nymd,nhms,im,jm,1,lm,q(1,1,1,4),rc )

          call gfio_close  ( id,rc )

! Construct Pressure Variables
! ----------------------------
      allocate ( ak(lm+1) )
      allocate ( bk(lm+1) )
      allocate ( ple(im,jm,lm+1) )

      call set_eta ( lm,ptop,ak,bk )

      do L=1,lm+1
      ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
      enddo

! Construct THV (if necessary) for REMAPPING
! ------------------------------------------
      if( tvflag .and. .not.thvflag ) then
          allocate (  pk(im,jm,lm  ) )
          allocate ( pke(im,jm,lm+1) )
          pke(:,:,:) = ple(:,:,:)**kappa
          do L=1,lm
          pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
     .              / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
          enddo
          thv =  tv/pk
      endif

      write( cnymd,200 ) nymd
      write( cnhms,300 ) nhms/10000
  200 format(i8.8)
  300 format(i2.2)
  400 format('dset ^',a)
  600 format(a1,i2.2)

C **********************************************************************
C ****                   Add Climatological Ozone                   ****
C **********************************************************************

c Construct 3-D Ozone on FV Levels
c --------------------------------
      if( add_ozone ) then
      allocate (  pl(im,jm,lm)   )
      do L=lm,1,-1
       pl(:,:,L) = (ple(:,:,L+1)+ple(:,:,L))*0.5
      enddo
      call get_ozone ( q(1,1,1,2),pl,im,jm,lm,nymd,nhms )
      deallocate ( pl )
      endif

C **********************************************************************
C ****                        Get ECMWF Data                        ****
C **********************************************************************

          print *, 'Reading ECMWF Model Data for Date: ',nymd,' Time: ',nhms

          mlev = 91
c         call gfio_open ( trim(ana_data),1,ID,rc )
c         call gfio_diminquire ( ID,im,jm,mlev,ntime,nvars,ngatts,rc )

          deallocate ( ak )
          deallocate ( bk )
          deallocate ( ple)
            allocate ( ak(mlev+1) )
            allocate ( bk(mlev+1) )
            allocate ( ple(im,jm,mlev+1) )

          allocate (  dp_ana(im,jm,mlev) )
          allocate (   u_ana(im,jm,mlev) )
          allocate (   v_ana(im,jm,mlev) )
          allocate (   t_ana(im,jm,mlev) )
          allocate (   q_ana(im,jm,mlev,nq) )
          allocate (  ps_ana(im,jm) )
          allocate (phis_ana(im,jm) )

#if 1
c
c For some reason (?), we have to use the grads utility: writegrads.gs
c on the nc4 data generated by Dan (from GRIB) to remap the ECMWF YOTC data properly
c ----------------------------------------------------------------------------------
          open (55,file=trim(ana_data),form='unformatted',access='direct',recl=im*jm*4)
          lrec = 1
          read (55,rec=lrec) phis_ana ; lrec = lrec+1
          read (55,rec=lrec)   ps_ana ; lrec = lrec+1
          do L=1,mlev ; read (55,rec=lrec) q_ana(:,:,L,2) ; lrec = lrec+1 ; enddo
          do L=1,mlev ; read (55,rec=lrec) q_ana(:,:,L,1) ; lrec = lrec+1 ; enddo
          do L=1,mlev ; read (55,rec=lrec) t_ana(:,:,L)   ; lrec = lrec+1 ; enddo
          do L=1,mlev ; read (55,rec=lrec) u_ana(:,:,L)   ; lrec = lrec+1 ; enddo
          do L=1,mlev ; read (55,rec=lrec) v_ana(:,:,L)   ; lrec = lrec+1 ; enddo

          call set_eta ( mlev,ptop,ak,bk )

c         call gfio_getvar ( id,'phis',nymd,nhms,im,jm,0,1   ,phis_ana,rc )
c         call gfio_getvar ( id,'lnps',nymd,nhms,im,jm,0,1   ,  ps_ana,rc )
c         call gfio_getvar ( id,'u'   ,nymd,nhms,im,jm,1,mlev,   u_ana,rc )
c         call gfio_getvar ( id,'v'   ,nymd,nhms,im,jm,1,mlev,   v_ana,rc )
c         call gfio_getvar ( id,'t'   ,nymd,nhms,im,jm,1,mlev,   t_ana,rc )
c         call gfio_getvar ( id,'q'   ,nymd,nhms,im,jm,1,mlev,   q_ana(1,1,1,1),rc )
c         call gfio_getvar ( id,'oz'  ,nymd,nhms,im,jm,1,mlev,   q_ana(1,1,1,2),rc )

#else

c Reading this data directly does not seem to work
c ------------------------------------------------
          call gfio_getvar ( id,'Geopotential'                 ,nymd,nhms,im,jm,0,1   ,phis_ana,rc )
          call gfio_getvar ( id,'Logarithm_of_surface_pressure',nymd,nhms,im,jm,0,1   ,  ps_ana,rc )
          call gfio_getvar ( id,'U_velocity'                   ,nymd,nhms,im,jm,1,mlev,   u_ana,rc )
          call gfio_getvar ( id,'V_velocity'                   ,nymd,nhms,im,jm,1,mlev,   v_ana,rc )
          call gfio_getvar ( id,'Temperature'                  ,nymd,nhms,im,jm,1,mlev,   t_ana,rc )
          call gfio_getvar ( id,'Specific_humidity'            ,nymd,nhms,im,jm,1,mlev,   q_ana(1,1,1,1),rc )
          call gfio_getvar ( id,'Ozone_mass_mixing_ratio'      ,nymd,nhms,im,jm,1,mlev,   q_ana(1,1,1,2),rc )

          call gfio_getrealatt ( id,'ak',mlev+1,ak,rc )
          call gfio_getrealatt ( id,'bk',mlev+1,bk,rc )
#endif

          call hflip ( phis_ana,im,jm,1 )
          call hflip (   ps_ana,im,jm,1 )
          call hflip (    u_ana,im,jm,mlev )
          call hflip (    v_ana,im,jm,mlev )
          call hflip (    t_ana,im,jm,mlev )
          call hflip (    q_ana(1,1,1,1),im,jm,mlev )
          call hflip (    q_ana(1,1,1,2),im,jm,mlev )

          ps_ana = exp( ps_ana )
          do L=1,mlev+1
          ple(:,:,L) = ak(L) + ps_ana(:,:)*bk(L)
          enddo
          do L=1,mlev
          dp_ana(:,:,L) = ple(:,:,L+1)-ple(:,:,L)
          enddo

C **********************************************************************
C ****                     Adjust fv Restart                        ****
C **********************************************************************

      print *, 'Calling Remap'
      call remap  ( ps,dp,u,v,thv,q,phis,lm,
     .              ps_ana,dp_ana,u_ana,v_ana,t_ana,q_ana,phis_ana,mlev,im,jm,nq,pbelow,pabove )

C **********************************************************************
C ****                  Write HDF Eta File                          ****
C **********************************************************************

      call put_fvrst ( ps,dp,u,v,thv,q,phis,
     .                 im,jm,lm,nq,nymd,nhms,tag,ext,lon(1),
     .                 timinc,precision,dgrid,tvflag )

      stop
      end

      subroutine put_fvrst ( ps,dp,u,v,thv,q,phis,
     .                       im,jm,lm,nq,nymd,nhms,tag,ext,lonbeg,
     .                       timeinc,precision,dgrid,tvflag )
      use MAPL_BaseMod, only: MAPL_UNDEF
      implicit  none

      integer   im,jm,lm,nq,nymd,nhms
      real phis(im,jm)
      real   ps(im,jm)
      real   dp(im,jm,lm)
      real    u(im,jm,lm)
      real    v(im,jm,lm)
      real  thv(im,jm,lm)
      real    q(im,jm,lm,nq)
      logical dgrid,tvflag
      integer timeinc

      real  ple(im,jm,lm+1)
      real  pke(im,jm,lm+1)
      real   pk(im,jm,lm)
      real   tv(im,jm,lm)
      real    t(im,jm,lm)
      real  slp(im,jm)

      real lats(jm)
      real lons(im)
      real levs(lm)
      real   ak(lm+1)
      real   bk(lm+1)

      real  rgas,rvap,eps,kappa,grav
      real  ptop,pint,dlon,dlat,pref,dpref(lm),undef,lonbeg
      integer i,j,L,n,ks,rc
      character*256 tag,ext,filename, fname
      integer nvars,fid,precision,nstep,id

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

      character*1 string(256)
      character*2 cnhms
      character*8 cnymd
      equivalence ( string(01),filename )
      data filename /'gg2fv.eta.yyyymmdd_hhz'/
      equivalence ( string(11),cnymd )
      equivalence ( string(20),cnhms )

      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      kappa = 2.0/7.0
      grav  = 9.81
      nstep = 100

      undef     = MAPL_UNDEF
      write( cnymd,200 ) nymd
      write( cnhms,300 ) nhms/10000
  200 format(i8.8)
  300 format(i2.2)
      fname = trim(tag) // trim(filename) // '.' // trim(ext)
      print *, 'Creating 32-bit eta file: ',trim(fname)

      call set_eta ( lm,ptop,ak,bk )

! Construct D-Grid Winds (if necessary)
! -------------------------------------
      if (dgrid) then
        call atod ( u,u,im,jm,lm,2 )
        call atod ( v,v,im,jm,lm,1 )
      endif

! Construct T, TV, and SLP
! ------------------------
      do L=1,lm+1
      ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
      enddo
      pke(:,:,:) = ple(:,:,:)**kappa
      do L=1,lm
      pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
     .          / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo
      tv = thv*pk

      call get_slp ( ps,phis,slp,ple,pk,tv,rgas,grav,im,jm,lm )

      t(:,:,:) = tv(:,:,:)/(1+eps*q(:,:,:,1))

c String and vars settings
c ------------------------
      title    = 'FVGCM Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Data Assimilation Office, NASA/GSFC'
      contact  = 'data@dao.gsfc.nasa.gov'
      levunits = 'hPa'

                           nvars = 10
      allocate (     vname(nvars) )
      allocate (    vtitle(nvars) )
      allocate (    vunits(nvars) )
      allocate (     lmvar(nvars) )
      allocate ( v_range(2,nvars) )
      allocate ( p_range(2,nvars) )

             id  = 1
       vname(id) = 'phis'
      vtitle(id) = 'Topography geopotential'
      vunits(id) = 'meter2/sec2'
       lmvar(id) =  0

             id  =  id+1
       vname(id) = 'ps'
      vtitle(id) = 'Surface Pressure'
      vunits(id) = 'Pa'
       lmvar(id) =  0

             id  =  id+1
       vname(id) = 'slp'
      vtitle(id) = 'Sea Level Pressure'
      vunits(id) = 'Pa'
       lmvar(id) =  0

             id  =  id+1
       vname(id) = 'delp'
      vtitle(id) = 'Pressure Thickness'
      vunits(id) = 'Pa'
       lmvar(id) =  lm

             id  =  id+1
       if(dgrid)then
          vname(id) = 'uwnd'
         vtitle(id) = 'eastward_wind_on_native_D-Grid'
       else
          vname(id) = 'u'
         vtitle(id) = 'eastward_wind'
       endif
      vunits(id) = 'm/s'
       lmvar(id) =  lm

             id  =  id+1
       if(dgrid)then
          vname(id) = 'vwnd'
         vtitle(id) = 'northward_wind_on_native_D-Grid'
       else
          vname(id) = 'v'
         vtitle(id) = 'northward_wind'
       endif
      vunits(id) = 'm/s'
       lmvar(id) =  lm

             id  =  id+1
       vname(id) = 'tmpu'
      vtitle(id) = 'Temperature'
      vunits(id) = 'K'
       lmvar(id) =  lm

             id  =  id+1
      if (tvflag) then
       vname(id) = 'tv'
      vtitle(id) = 'air_virtual_temperature'
      vunits(id) = 'K'
       lmvar(id) =  lm
      else
       vname(id) = 'theta'
      vtitle(id) = 'Scaled Virtual Potential Temperature'
      vunits(id) = 'K/Pa^kappa'
       lmvar(id) =  lm
      endif

             id  =  id+1
       vname(id) = 'sphu'
      vtitle(id) = 'Specific Humidity'
      vunits(id) = 'kg/kg'
       lmvar(id) =  lm

             id  =  id+1
       vname(id) = 'ozone'
      vtitle(id) = 'Ozone'
      vunits(id) = 'ppmv'
       lmvar(id) =  lm

      v_range(:,:) = undef
      p_range(:,:) = undef

c Compute grid
c ------------
      dlon = 360.0/ im
      dlat = 180.0/(jm-1)
        ks = 41
      pint = ak(ks+1)

      do j=1,jm
      lats(j) = -90.0 + (j-1)*dlat
      enddo
      do i=1,im
      lons(i) =   lonbeg + (i-1)*dlon
      enddo
      do L=1,lm
      dpref(L) = (ak(L+1)-ak(L)) + (bk(L+1)-bk(L))*98400.0
      enddo
      pref    = ptop + 0.5*dpref(1)
      levs(1) = pref
      do L=2,lm
      pref    = pref + 0.5*( dpref(L)+dpref(L-1) )
      levs(L) = pref
      enddo
      levs(:) = levs(:)/100

c Create GFIO file
c ----------------
      call GFIO_Create ( fname, title, source, contact, undef,
     .                   im, jm, lm, lons, lats, levs, levunits,
     .                   nymd, nhms, timeinc,
     .                   nvars, vname, vtitle, vunits, lmvar,
     .                   v_range, p_range, precision,
     .                   fid, rc )
c Write GFIO data
c ---------------
      call Gfio_putVar ( fid,vname(01),nymd,nhms,im,jm,0, 1,phis      ,rc )
      call Gfio_putVar ( fid,vname(02),nymd,nhms,im,jm,0, 1,ps        ,rc )
      call Gfio_putVar ( fid,vname(03),nymd,nhms,im,jm,0, 1,slp       ,rc )
      call Gfio_putVar ( fid,vname(04),nymd,nhms,im,jm,1,lm,dp        ,rc )
      call Gfio_putVar ( fid,vname(05),nymd,nhms,im,jm,1,lm,u         ,rc )
      call Gfio_putVar ( fid,vname(06),nymd,nhms,im,jm,1,lm,v         ,rc )
      call Gfio_putVar ( fid,vname(07),nymd,nhms,im,jm,1,lm,t         ,rc )

      if( tvflag ) then
      call Gfio_putVar ( fid,vname(08),nymd,nhms,im,jm,1,lm,tv        ,rc )
      else
      call Gfio_putVar ( fid,vname(08),nymd,nhms,im,jm,1,lm,thv       ,rc )
      endif

      do n=1,nq
      call Gfio_putVar ( fid,vname(08+n),nymd,nhms,im,jm,1,lm,q(1,1,1,n),rc )
      enddo

c Write GFIO global attributes
c ----------------------------
      call GFIO_PutRealAtt ( fid,'ptop',   1,ptop ,precision,rc )
      call GFIO_PutRealAtt ( fid,'pint',   1,pint ,precision,rc )
      call GFIO_PutIntAtt  ( fid,'ks',     1,ks   ,0        ,rc )
      call GFIO_PutRealAtt ( fid,'ak',  lm+1,ak   ,precision,rc )
      call GFIO_PutRealAtt ( fid,'bk',  lm+1,bk   ,precision,rc )
      call GFIO_PutIntAtt  ( fid,'nstep',  1,nstep,0        ,rc )

      call gfio_close ( fid,rc )
      return
      end

      subroutine getfile ( ku,filename,irec )
      implicit none
      character(len=*) filename
      integer ku,irec

      if ( irec>0 ) then

          open (ku,file=trim(filename),form='unformatted',access='direct',recl=irec)
          return

      else

          open (ku,file=trim(filename),form='unformatted',access='sequential',convert='big_endian')
          read (ku, err=1001)  ! Check for BIG_ENDIAN

 5000     backspace(ku)
          return

 1001     close(ku)
          print *, 'File: ',trim(filename)
          print *, 'Failed to OPEN using BIG_ENDIAN, will try LITTLE_ENDIAN'
          open (ku,file=trim(filename),form='unformatted',access='sequential',convert='little_endian')
          read (ku, err=1002)  ! Check for LITTLE_ENDIAN
          goto 5000

 1002     continue
          print *, 'ERROR!!  File: ',trim(filename)
          print *, 'ERROR!!  is neither BIG nor LITTLE ENDIAN'
          call exit(7)

      endif
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L
      real*4 q(im,jm,lm),dum(im)
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
      defined = abs(q-undef).gt.0.1*undef
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
      pe2(i,j,1) = 1.0  ! Set ptop = 0.01 mb (rather than 0.0 mb)
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
           ps1(i,j) = pe2(i,j,L+1)*( 1+(phi2(i,j,L+1)-phis1(i,j))/(cp*thv2(i,j,L)*pke2(i,j,L+1)) )**(1.0/kappa)
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
      print *, '     Mapping         ECMWF-State onto New Eta Grid'
      call gmap ( im,jm,nq, kappa,
     .            lm2,  pke2,  pe2, u2,  v2,  thv2,  q2,
     .            lm1,  pke1,  pe1, u1,  v1,  thv1,  q1)

c Blend result with original fv state
c -----------------------------------
      if( pbelow.ne.pabove ) then
      print *, '   Blending   FV and ECMWF States'
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
      print *, " ec_eta2fv.x [-ecmwf ecmwf.data.nc4]"
      print *, "             [-ana      fv.data.nc4]"
      print *, "             [-nymd  nymd]"
      print *, "             [-nhms  nhms]"
      print *, "             [-plow  plow]"
      print *, "             [-phigh phigh]"
      print *, "             [-tag   tag]"
      print *, "             [-ozone]"
      print *
      print *, "where:"
      print *
      print *, "  -ecmwf ecmwf.data: Filename of ECMWF Model-level data"
      print *, "  -ana      fv.data: Filename of GMAO Background Data (ana.eta format)"
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

      subroutine interp_h ( q_cmp,im,jm,lm,
     .                      dlam,dphi,rotation,tilt,precession,
     .                      q_geo,irun,lon_geo,lat_geo,
     .                      msgn,norder,check,undef )
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
C    rotation ... Rotation parameter lam_np (Degrees)
C    tilt ....... Rotation parameter phi_np (Degrees)
C    precession . Rotation parameter lam_0  (Degrees)
C    irun ....... Number of Output Locations
C    lon_geo .... Longitude Location of Output
C    lat_geo .... Latitude  Location of Output
C    msgn ....... Flag for scalar field  ( msgn =  1 )
C                    or vector component ( msgn = -1 )
C    norder ..... Order of Interpolation:  Bi-Linear => abs(norder) = 1
C                                          Bi-Cubic  => abs(norder) = 3
C                 Note: If norder < 0, then check for positive definite
C    check ...... Logical Flag to check for Undefined values
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
      integer im,jm,lm,irun,norder,msgn
      logical check

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

      real, allocatable       ::    old_lon (:)
      real, allocatable       ::    old_lat (:)
      real, allocatable       ::    old_dlam(:)
      real, allocatable       ::    old_dphi(:)

      real    ux, ap1, ap0, am1, am2
      real    uy, bp1, bp0, bm1, bm2

      real    lon_cmp(im)
      real    lat_cmp(jm)
      real    q_tmp(irun)

      real    pi,cosnp,sinnp,p1,p2,p3,eps,d
      real    lam,lam_ip1,lam_ip0,lam_im1,lam_im2
      real    phi,phi_jp1,phi_jp0,phi_jm1,phi_jm2
      real    dl,dp,lam_np,phi_np,lam_0,eps_np
      real    rotation , tilt , precession
      real    lam_geo, lam_cmp
      real    phi_geo, phi_cmp
      real    undef
      integer im1_cmp,icmp
      integer jm1_cmp,jcmp

      logical compute_weights
      real    old_rotation
      real    old_tilt
      real    old_precession
      data    old_rotation   /-999.9/
      data    old_tilt       /-999.9/
      data    old_precession /-999.9/

      parameter ( eps = 1.e-10 )

c Initialization
c --------------
      pi = 4.*atan(1.)
      dl = 2*pi/ im     ! Uniform Grid Delta Lambda
      dp =   pi/(jm-1)  ! Uniform Grid Delta Phi

c Allocate Memory for Weights and Index Locations
c -----------------------------------------------
      if(.not.allocated(old_lon)) then

      allocate (   old_dlam(im)  ,   old_dphi(jm)  )
      allocate (   old_lon(irun) ,   old_lat(irun) )
      allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
      allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
      allocate ( wc_ip1jp1(irun) , wc_ip0jp1(irun) , wc_im1jp1(irun) , wc_im2jp1(irun) )
      allocate ( wc_ip1jp0(irun) , wc_ip0jp0(irun) , wc_im1jp0(irun) , wc_im2jp0(irun) )
      allocate ( wc_ip1jm1(irun) , wc_ip0jm1(irun) , wc_im1jm1(irun) , wc_im2jm1(irun) )
      allocate ( wc_ip1jm2(irun) , wc_ip0jm2(irun) , wc_im1jm2(irun) , wc_im2jm2(irun) )
      allocate (       ip1(irun) ,       ip0(irun) ,       im1(irun) ,       im2(irun) )
      allocate (       jp1(irun) ,       jp0(irun) ,       jm1(irun) ,       jm2(irun) )
      do i=1,irun
      old_lon(i) = -999.9
      old_lat(i) = -999.9
      enddo
      do i=1,im
      old_dlam(i) = 0.0
      enddo
      do j=1,jm
      old_dphi(j) = 0.0
      enddo

      else
             i =  size (old_dlam)
             j =  size (old_dphi)
             m =  size (old_lon)
          if(i.ne.im .or. j.ne.jm .or. m.ne.irun) then
          deallocate (  old_dlam ,  old_dphi )
          deallocate (   old_lon ,   old_lat )
          deallocate ( wl_ip0jp0 , wl_im1jp0 )
          deallocate ( wl_ip0jm1 , wl_im1jm1 )
          deallocate ( wc_ip1jp1 , wc_ip0jp1 , wc_im1jp1 , wc_im2jp1 )
          deallocate ( wc_ip1jp0 , wc_ip0jp0 , wc_im1jp0 , wc_im2jp0 )
          deallocate ( wc_ip1jm1 , wc_ip0jm1 , wc_im1jm1 , wc_im2jm1 )
          deallocate ( wc_ip1jm2 , wc_ip0jm2 , wc_im1jm2 , wc_im2jm2 )
          deallocate (       ip1 ,       ip0 ,       im1 ,       im2 )
          deallocate (       jp1 ,       jp0 ,       jm1 ,       jm2 )
          allocate (   old_dlam(im)  ,   old_dphi(jm)  )
          allocate (   old_lon(irun) ,   old_lat(irun) )
          allocate ( wl_ip0jp0(irun) , wl_im1jp0(irun) )
          allocate ( wl_ip0jm1(irun) , wl_im1jm1(irun) )
          allocate ( wc_ip1jp1(irun) , wc_ip0jp1(irun) , wc_im1jp1(irun) , wc_im2jp1(irun) )
          allocate ( wc_ip1jp0(irun) , wc_ip0jp0(irun) , wc_im1jp0(irun) , wc_im2jp0(irun) )
          allocate ( wc_ip1jm1(irun) , wc_ip0jm1(irun) , wc_im1jm1(irun) , wc_im2jm1(irun) )
          allocate ( wc_ip1jm2(irun) , wc_ip0jm2(irun) , wc_im1jm2(irun) , wc_im2jm2(irun) )
          allocate (       ip1(irun) ,       ip0(irun) ,       im1(irun) ,       im2(irun) )
          allocate (       jp1(irun) ,       jp0(irun) ,       jm1(irun) ,       jm2(irun) )
          do i=1,irun
          old_lon(i) = -999.9
          old_lat(i) = -999.9
          enddo
          do i=1,im
          old_dlam(i) = 0.0
          enddo
          do j=1,jm
          old_dphi(j) = 0.0
          enddo
          endif
      endif

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

c Check for Co-incident Grid-Point Latitude and Pole Locations
c ------------------------------------------------------------
                                          eps_np = 0.0
      do j=1,jm
      phi_cmp = lat_cmp(j)*180./pi
      if( abs( phi_cmp-tilt ).lt.1.e-3 )  eps_np =  1.e-3
      if( tilt+eps_np .gt. 90. )          eps_np = -1.e-3
      enddo

      lam_np = pi/180.*rotation
      phi_np = pi/180.*(tilt+eps_np)
      lam_0  = pi/180.*precession

      if( tilt.eq.90. ) then 
      cosnp = 0.0
      sinnp = 1.0
      else if(tilt.eq.-90.0) then
      cosnp = 0.0
      sinnp =-1.0
      else
      cosnp = cos(phi_np)
      sinnp = sin(phi_np)
      endif

c Determine if Weights Need to be Updated
c ---------------------------------------
      compute_weights =  rotation.ne.old_rotation .or.
     .                       tilt.ne.old_tilt     .or.
     .                 precession.ne.old_precession

      m = 1
      do while ( .not.compute_weights .and. m.le.irun )
      compute_weights = (lon_geo(m).ne.old_lon(m)) .or.
     .                  (lat_geo(m).ne.old_lat(m))
      m = m+1
      enddo
      i = 1
      do while ( .not.compute_weights .and. i.le.im )
      compute_weights = dlam(i).ne.old_dlam(i)
      i = i+1
      enddo
      j = 1
      do while ( .not.compute_weights .and. j.le.jm-1 )
      compute_weights = dphi(j).ne.old_dphi(j)
      j = j+1
      enddo

c Compute Weights for Computational to Geophysical Grid Interpolation
c -------------------------------------------------------------------
      if( compute_weights ) then

      old_rotation   = rotation
      old_tilt       = tilt
      old_precession = precession

#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (i,lam_geo,phi_geo,lam_cmp,phi_cmp,lam,phi)
!$omp& private (p1,p2,p3,d,icmp,jcmp,im1_cmp,jm1_cmp)
!$omp& private (lam_im2, lam_im1, lam_ip0, lam_ip1)
!$omp& private (phi_jm2, phi_jm1, phi_jp0, phi_jp1)
!$omp& private (ap1, ap0, am1, am2)
!$omp& private (bp1, bp0, bm1, bm2)
#endif
      do i=1,irun
      old_lon(i) = lon_geo(i)
      old_lat(i) = lat_geo(i)
      lam_geo    = lon_geo(i)
      phi_geo    = lat_geo(i)

      p1 = cosnp*cos(phi_geo)*cos(lam_geo+lam_0-pi)
     .   + sin(phi_geo)*sinnp
      p1 = min(p1, 1.0)
      p1 = max(p1,-1.0)
      phi_cmp = asin( p1 )

      if( tilt.eq.90.0 .or. tilt.eq.-90.0 ) then
      p2 = sinnp*cos(lam_geo+lam_0-pi)
      else
      p2 = sinnp*cos(phi_geo)*cos(lam_geo+lam_0-pi)
     .   - sin(phi_geo)*cosnp
      p2 = p2 / max( cos(phi_cmp),eps )
      p2 = min(p2, 1.0)
      p2 = max(p2,-1.0)
      endif
      p2 = acos( p2 )

      p3 = cos(phi_geo)*sin(lam_geo+lam_0-pi)
      if( p3.lt.0.0 ) p2 = -p2
      p2 = p2 + lam_np - pi
      lam_cmp = mod( p2+3.0*pi,2.0*pi ) - pi

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
      endif

c Interpolate Computational-Grid Quantities to Geophysical Grid Using Bi-Linear
c -----------------------------------------------------------------------------
      if( abs(norder).eq.1 ) then

      if( check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,q_tmp)
#endif
      do L=1,lm
      do i=1,irun

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
      enddo

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      if( .not.check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,q_tmp)
#endif
      do L=1,lm
      do i=1,irun
      q_tmp(i) = wl_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wl_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wl_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wl_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
      enddo

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      endif      ! End Check for Bi-Linear Interpolation


c Interpolate Computational-Grid Quantities to Geophysical Grid Using Bi-Cubic
c ----------------------------------------------------------------------------
      if( abs(norder).eq.3 ) then

      if( check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,m,n,q_tmp)
!$omp& private (ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1)
!$omp& private (ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2)
!$omp& private (jp1_for_jp1, jm2_for_jm2)
#endif
      do L=1,lm
      do i=1,irun

      ip1_for_jp1 = ip1(i)
      ip0_for_jp1 = ip0(i)
      im1_for_jp1 = im1(i)
      im2_for_jp1 = im2(i)
      jp1_for_jp1 = jp1(i)
                m = 1

      if( jp0(i).eq.jm ) then
      ip1_for_jp1 = 1 + mod( ip1_for_jp1 + im/2 -1, im )
      ip0_for_jp1 = 1 + mod( ip0_for_jp1 + im/2 -1, im )
      im1_for_jp1 = 1 + mod( im1_for_jp1 + im/2 -1, im )
      im2_for_jp1 = 1 + mod( im2_for_jp1 + im/2 -1, im )
      jp1_for_jp1 = jm-1
      if(msgn.eq.-1) m=-1
      endif

      ip1_for_jm2 = ip1(i)
      ip0_for_jm2 = ip0(i)
      im1_for_jm2 = im1(i)
      im2_for_jm2 = im2(i)
      jm2_for_jm2 = jm2(i)
                n = 1

      if( jm1(i).eq.1 ) then
      ip1_for_jm2 = 1 + mod( ip1_for_jm2 + im/2 -1, im )
      ip0_for_jm2 = 1 + mod( ip0_for_jm2 + im/2 -1, im )
      im1_for_jm2 = 1 + mod( im1_for_jm2 + im/2 -1, im )
      im2_for_jm2 = 1 + mod( im2_for_jm2 + im/2 -1, im )
      jm2_for_jm2 = 2
      if(msgn.eq.-1) n=-1
      endif


      if( q_cmp( ip1(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( ip0(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( im1(i),jp0(i),L ).ne.undef .and.
     .    q_cmp( im2(i),jp0(i),L ).ne.undef .and.

     .    q_cmp( ip1(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( ip0(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( im1(i),jm1(i),L ).ne.undef .and.
     .    q_cmp( im2(i),jm1(i),L ).ne.undef .and.

     .    q_cmp( ip1_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( ip0_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( im1_for_jm2,jm2_for_jm2,L ).ne.undef  .and.
     .    q_cmp( im2_for_jm2,jm2_for_jm2,L ).ne.undef  .and.

     .    q_cmp( ip1_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( ip0_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( im1_for_jp1,jp1_for_jp1,L ).ne.undef  .and.
     .    q_cmp( im2_for_jp1,jp1_for_jp1,L ).ne.undef ) then

      q_tmp(i) = wc_ip1jp1(i) * q_cmp( ip1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_ip0jp1(i) * q_cmp( ip0_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im1jp1(i) * q_cmp( im1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im2jp1(i) * q_cmp( im2_for_jp1,jp1_for_jp1,L )*m

     .         + wc_ip1jp0(i) * q_cmp( ip1(i),jp0(i),L )
     .         + wc_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
     .         + wc_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wc_im2jp0(i) * q_cmp( im2(i),jp0(i),L )

     .         + wc_ip1jm1(i) * q_cmp( ip1(i),jm1(i),L )
     .         + wc_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wc_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wc_im2jm1(i) * q_cmp( im2(i),jm1(i),L )

     .         + wc_ip1jm2(i) * q_cmp( ip1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_ip0jm2(i) * q_cmp( ip0_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im1jm2(i) * q_cmp( im1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im2jm2(i) * q_cmp( im2_for_jm2,jm2_for_jm2,L )*n


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
      enddo

c Check for Positive Definite
c ---------------------------
      if( norder.lt.0 ) then
      do i=1,irun
      if( q_tmp(i).ne.undef .and.
     .    q_tmp(i).lt.0.0 )  then
          q_tmp(i) = 0.0
      endif
      enddo
      endif

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      if( .not.check  ) then
#if   (openmp)
!$omp  parallel do 
!$omp& default (shared)
!$omp& private (L,i,m,n,q_tmp)
!$omp& private (ip1_for_jp1, ip0_for_jp1, im1_for_jp1, im2_for_jp1)
!$omp& private (ip1_for_jm2, ip0_for_jm2, im1_for_jm2, im2_for_jm2)
!$omp& private (jp1_for_jp1, jm2_for_jm2)
#endif
      do L=1,lm
      do i=1,irun

      ip1_for_jp1 = ip1(i)
      ip0_for_jp1 = ip0(i)
      im1_for_jp1 = im1(i)
      im2_for_jp1 = im2(i)
      jp1_for_jp1 = jp1(i)
                m = 1

      if( jp0(i).eq.jm ) then
      ip1_for_jp1 = 1 + mod( ip1_for_jp1 + im/2 -1, im )
      ip0_for_jp1 = 1 + mod( ip0_for_jp1 + im/2 -1, im )
      im1_for_jp1 = 1 + mod( im1_for_jp1 + im/2 -1, im )
      im2_for_jp1 = 1 + mod( im2_for_jp1 + im/2 -1, im )
      jp1_for_jp1 = jm-1
      if(msgn.eq.-1) m=-1
      endif

      ip1_for_jm2 = ip1(i)
      ip0_for_jm2 = ip0(i)
      im1_for_jm2 = im1(i)
      im2_for_jm2 = im2(i)
      jm2_for_jm2 = jm2(i)
                n = 1

      if( jm1(i).eq.1 ) then
      ip1_for_jm2 = 1 + mod( ip1_for_jm2 + im/2 -1, im )
      ip0_for_jm2 = 1 + mod( ip0_for_jm2 + im/2 -1, im )
      im1_for_jm2 = 1 + mod( im1_for_jm2 + im/2 -1, im )
      im2_for_jm2 = 1 + mod( im2_for_jm2 + im/2 -1, im )
      jm2_for_jm2 = 2
      if(msgn.eq.-1) n=-1
      endif

      q_tmp(i) = wc_ip1jp1(i) * q_cmp( ip1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_ip0jp1(i) * q_cmp( ip0_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im1jp1(i) * q_cmp( im1_for_jp1,jp1_for_jp1,L )*m
     .         + wc_im2jp1(i) * q_cmp( im2_for_jp1,jp1_for_jp1,L )*m

     .         + wc_ip1jp0(i) * q_cmp( ip1(i),jp0(i),L )
     .         + wc_ip0jp0(i) * q_cmp( ip0(i),jp0(i),L )
     .         + wc_im1jp0(i) * q_cmp( im1(i),jp0(i),L )
     .         + wc_im2jp0(i) * q_cmp( im2(i),jp0(i),L )

     .         + wc_ip1jm1(i) * q_cmp( ip1(i),jm1(i),L )
     .         + wc_ip0jm1(i) * q_cmp( ip0(i),jm1(i),L )
     .         + wc_im1jm1(i) * q_cmp( im1(i),jm1(i),L )
     .         + wc_im2jm1(i) * q_cmp( im2(i),jm1(i),L )

     .         + wc_ip1jm2(i) * q_cmp( ip1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_ip0jm2(i) * q_cmp( ip0_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im1jm2(i) * q_cmp( im1_for_jm2,jm2_for_jm2,L )*n
     .         + wc_im2jm2(i) * q_cmp( im2_for_jm2,jm2_for_jm2,L )*n

      enddo

c Check for Positive Definite
c ---------------------------
      if( norder.lt.0 ) then
      do i=1,irun
      if( q_tmp(i).ne.undef .and.
     .    q_tmp(i).lt.0.0 )  then
          q_tmp(i) = 0.0
      endif
      enddo
      endif

c Load Temp array into Output array
c ---------------------------------
      do i=1,irun
      q_geo(i,L) = q_tmp(i)
      enddo
      enddo
      endif

      endif      ! End Check for Bi-Cubic Interpolation

          deallocate (  old_dlam ,  old_dphi )
          deallocate (   old_lon ,   old_lat )
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
#if 0
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
