      program convert_eta

      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****    Program to convert ETA File (Resolution or Staggering)    ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm,nt

      integer  nymd  ,nhms
      integer  nymd0 ,nhms0
      integer  nymdr ,nhmsr
      integer  nymdb ,nhmsb
      integer  nymdb0,nhmsb0

      integer  hour,day,month,year


c Generic Model Variables
c -----------------------
      real, allocatable ::  q2d(:,:,:)
      real, allocatable ::  q3d(:,:,:,:)

c HDF and other Local Variables
c -----------------------------
      real,          pointer :: lon   (:)
      real,          pointer :: lat   (:)
      real,          pointer :: lev   (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      character*256, pointer :: unit2d(:), unit3d(:)
      character*256, pointer :: namesp (:)
      character*256, pointer :: name2dp(:), name3dp(:)
      character*256, pointer :: titl2dp(:), titl3dp(:)
      character*256, pointer :: unit2dp(:), unit3dp(:)
      character*256  levunits

      integer id,rc,fid,nhmsf,n2d,n3d
      integer n2dp,n3dp,nvarsp
      integer nvars,ngatts,ntime,ntimes,gfrc

      character*256, allocatable ::   arg(:)
      character*256, allocatable :: fname(:)
      character*256  topo
      character*256  name
      character*256  hdfile, tag
      character*256  ext
      character*256  grid
      character*256 psname
      character*256 dpname
      character*256 ozname
      character*256  uname, uname2
      character*256  vname, vname2
      character*256  tname, tname2
      character*256  qname
      character*256  phisname
      character*4    xdim,ydim,zdim

      character*8    date,date0
      character*4    time0
      character*2    time,hour0,mins0
      character*1    char

      integer n,m,nargs,iargc,L,nbeg,nfiles,npfiles,mlev

      real    undef
      integer i,j,ndt
      integer imout,jmout,lmout
      logical hdfcreate
      logical Ldtoa

      character*8 cdate

      interface
                subroutine read_eta_meta ( hdffile,im,jm,lm,n2d,n3d,lat,lon,lev,undef,id,
     .                                     nymdb,nhmsb,ndt,ntimes,levunits,
     .                                     nvars,names,name2d,titl2d,unit2d,name3d,titl3d,unit3d )
                real,          pointer :: lat   (:)
                real,          pointer :: lon   (:)
                real,          pointer :: lev   (:)
                character*256, pointer :: names (:)
                character*256, pointer :: name2d(:), name3d(:)
                character*256, pointer :: titl2d(:), titl3d(:)
                character*256, pointer :: unit2d(:), unit3d(:)
                character*256  hdffile
                character*256  levunits
                integer        id,im,jm,lm,n2d,n3d,nvars
                integer        nymdb,nhmsb,ndt,ntimes
                real           undef
            end subroutine read_eta_meta
      end interface

C **********************************************************************
C ****                       Initialization                         ****
C **********************************************************************

       nt    =  1
       ndt   =  0
       tag   = 'NULL'
       grid  = 'NULL'
       uname = 'NULL'
       vname = 'NULL'
       tname = 'NULL'
       qname = 'NULL'
      ozname = 'NULL'
      psname = 'NULL'
      dpname = 'NULL'
        topo = 'NULL'
       imout = -999
       jmout = -999
       lmout = -999

      phisname = 'NULL'
 
          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs

             if( trim(arg(n)).eq.'-im'      ) read(arg(n+1),*) imout
             if( trim(arg(n)).eq.'-jm'      ) read(arg(n+1),*) jmout
             if( trim(arg(n)).eq.'-lm'      ) read(arg(n+1),*) lmout
             if( trim(arg(n)).eq.'-grid'    )   grid = trim(arg(n+1))
             if( trim(arg(n)).eq.'-uname'   )  uname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-vname'   )  vname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-tname'   )  tname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-qname'   )  qname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ozname'  ) ozname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-psname'  ) psname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-dpname'  ) dpname = trim(arg(n+1))
             if( trim(arg(n)).eq.'-topo'    )   topo = trim(arg(n+1))
             if( trim(arg(n)).eq.'-tag'     )    tag = trim(arg(n+1))

             if( trim(arg(n)).eq.'-f' ) then
                 nfiles = 1
                 read(arg(n+nfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+nfiles.ne.nargs )
                 nfiles = nfiles+1
                 read(arg(n+nfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) nfiles = nfiles-1
                 allocate ( fname(nfiles) )
                 do m=1,nfiles
                 fname(m) = arg(n+m)
                 enddo
             endif
	     
           enddo
      endif

c Set Default NAMES
c -----------------
      if( trim(grid).ne.'NULL' ) then
          if( trim(grid).eq.'dtoa' ) then
              if( trim(   uname).eq.'NULL' )    uname = 'uwnd'
              if( trim(   vname).eq.'NULL' )    vname = 'vwnd'
              if( trim(   tname).eq.'NULL' )    tname = 'theta'
          endif
          if( trim(grid).eq.'atod' ) then
              if( trim(   uname).eq.'NULL' )    uname = 'u'
              if( trim(   vname).eq.'NULL' )    vname = 'v'
              if( trim(   tname).eq.'NULL' )    tname = 'tv'
          endif
      endif
              if( trim(   uname).eq.'NULL' )    uname = 'uwnd'
              if( trim(   vname).eq.'NULL' )    vname = 'vwnd'
              if( trim(   tname).eq.'NULL' )    tname = 'theta'
              if( trim(   qname).eq.'NULL' )    qname = 'sphu'
              if( trim(  ozname).eq.'NULL' )   ozname = 'ozone'
              if( trim(  psname).eq.'NULL' )   psname = 'ps'
              if( trim(  dpname).eq.'NULL' )   dpname = 'delp'
              if( trim(phisname).eq.'NULL' ) phisname = 'phis'

C **********************************************************************
C ****                Summarize Input Variables                     ****
C **********************************************************************

      print *
      print *, 'Eta Files: '
      do n=1,nfiles
      print *, n,trim(fname(n))
      enddo
      print *
      name = fname(1)
         n = index(trim(name),'.',back=.true.)
       ext = trim(name(n+1:))

C **********************************************************************
C ****              Read and Interpolate Eta File                   ****
C **********************************************************************

      do n=1,nfiles
      print *, 'Opening: ',trim(fname(n))
      call read_eta_meta ( fname(n),im,jm,lm,n2d,n3d,lat,lon,lev,undef,id,
     .                     nymdb,nhmsb,ndt,ntimes,levunits,
     .                     nvars,names,name2d,titl2d,unit2d,name3d,titl3d,unit3d )

      if( (imout.ne.-999 .or. jmout.ne.-999 .or. lmout.ne.-999) ) then
      endif 

      if( imout.eq.-999 ) imout = im
      if( jmout.eq.-999 ) jmout = jm 
      if( lmout.eq.-999 ) lmout = lm 

      if( imout.ne.im .or. jmout.ne.jm ) then
           if( trim(topo).eq.'NULL' ) then
               print *
               print *, 'You must supply Output resolution TOPOGRAPHY file!'
               print *
               stop
           endif
      endif

      allocate (   q2d(im,jm,   n2d) )
      allocate (   q3d(im,jm,lm,n3d) )

      print *
      print *, 'Input Resolution IM: ',im
      print *, 'Input Resolution JM: ',jm
      print *, 'Input Resolution LM: ',lm
      print *
      print *, '2-D Fields:'
      print *, '-----------'
      do m=1,n2d
      print *,   trim(name2d(m)),'  ',trim(unit2d(m)),' ',trim(titl2d(m))
      enddo
      print *
      print *, '3-D Fields:'
      print *, '-----------'
      do m=1,n3d
      print *,   trim(name3d(m)),'  ',trim(unit3d(m)),' ',trim(titl3d(m))
      enddo

      nymd = nymdb
      nhms = nhmsb

      print *
      print *, 'Beginning Date to   Read: ',nymdb
      print *, 'Beginning Time to   Read: ',nhmsb
      print *, 'Number of Time   Periods: ',ntimes,' (ndt: ',ndt,' seconds)'
      print *

      write(date0,1000)  nymdb
      write(hour0,2000)  nhmsb/10000
      write(mins0,2000) (nhmsb-(nhmsb/10000)*10000)/100
 1000 format(i8.8)
 2000 format(i2.2)
      time0 = trim(hour0)//trim(mins0)

          hdfile = trim(fname(n))
      if( trim(tag).ne.'NULL' ) then
          hdfile = trim(hdfile) // "." // trim(tag)
      else
          if( trim(grid).ne.'NULL' ) hdfile = trim(hdfile) // "." // trim(grid)
          if( imout.ne.im .or. jmout.ne.jm .or. lmout.ne.lm ) then
              write(xdim,103) imout
              write(ydim,103) jmout
              write(zdim,103) lmout
  103         format(i4.4)
              hdfile = trim(hdfile) // "." // xdim // "x" // ydim // "x" // zdim
          endif
      endif

      call gfio_open ( trim(fname(n)),1,id,rc ) 
      rc        =  0
      ntime     =  0
      hdfcreate = .true.
              dowhile (rc.eq.0)
              ntime = ntime + 1

              nymdr = nymd  
              nhmsr = nhms  
              print *

                  Ldtoa = .false.
                  call read_eta_data  ( id,q2d,q3d,n2d,n3d,name2d,name3d,undef,
     .                                  im,jm,lm,nymdr,nhmsr,rc,ntime,ntimes,grid,
     .                                  uname,vname,tname,Ldtoa,uname2,vname2,tname2)

              if( rc.eq.0 ) then
                  call convert ( q2d,q3d,name2d,titl2d,unit2d,name3d,titl3d,unit3d,n2d,n3d,undef,
     .                           levunits,im,jm,lm,nt,lat,lon,lev,nymd,nhms,ndt,
     .                           fid,hdfcreate,hdfile,imout,jmout,lmout,grid,
     .                           psname,dpname,uname2,vname2,tname2,qname,ozname,phisname,topo,Ldtoa)

                  call tick (nymd,nhms,ndt)
                  hdfcreate = .false.
              else
                  call gfio_close ( fid,gfrc )
                  print *, 'Created: ',trim(hdfile)
                  print *
                  print *
                  deallocate ( q2d )
                  deallocate ( q3d )
              endif
              enddo
      call gfio_close ( id,gfrc )
      enddo

      deallocate ( arg )

      stop
      end

      subroutine read_eta_data ( id,q2d,q3d,n2d,n3d,name2d,name3d,undef,
     .                           im,jm,lm,nymd,nhms,rc,ntime,ntimes,grid,
     .                           uname,vname,tname,Ldtoa,uname2,vname2,tname2)
      implicit none

      integer   im,jm,lm,nymd,nhms,id,rc
      integer   n2d,n3d,ntime,ntimes
      integer   nundef,nvndef
      real  q2d(im,jm   ,n2d)
      real  q3d(im,jm,lm,n3d)
      real    u(im,jm,lm)
      real    v(im,jm,lm)
      real    undef
      character*256    name2d(n2d)
      character*256    name3d(n3d)
      character*256    grid
      character*256    uname, uname2
      character*256    vname, vname2
      character*256    tname, tname2
      integer i,j,L,n,nu,nv,nt
      logical Ldtoa

      nu = 0
      nv = 0
      rc = 0
      if( ntime <= ntimes ) then

c Collect Eta Data
c ----------------
          do n=1,n2d
          call gfio_getvar ( id,trim(name2d(n)),nymd,nhms,im,jm,0,1,q2d(1,1,n),rc )
          if( rc.ne.0 ) then
              rc = 1  ! No more time periods in file
              return
          endif
          enddo
          print *, 'Reading nymd: ',nymd,' nhms: ',nhms

          nundef = 0
          nvndef = 0
          do n=1,n3d
          call gfio_getvar ( id,trim(name3d(n)),nymd,nhms,im,jm,1,lm,q3d(1,1,1,n),rc )

             if( trim(name3d(n)).eq.trim(uname) ) then
                    nu = n
                     u = q3d(:,:,:,n)
                 do L=1,lm
                 do j=1,jm
                 do i=1,im
                    if( u(i,j,L).eq.undef ) nundef = nundef + 1
                 enddo
                 enddo
                 enddo
             endif
             if( trim(name3d(n)).eq.trim(vname) ) then
                    nv = n
                     v = q3d(:,:,:,n)
                 do L=1,lm
                 do j=1,jm
                 do i=1,im
                    if( v(i,j,L).eq.undef ) nvndef = nvndef + 1
                 enddo
                 enddo
                 enddo
             endif
             if( trim(name3d(n)).eq.trim(tname) ) nt = n
          enddo

c Convert D-Grid Winds to A-Grid Winds
c (Assume D-Grid UWind is UNDEF at South Pole j=1)
c ------------------------------------------------
          if( trim(grid).eq.'dtoa' .or.
     .       (nundef.eq.im*lm .and. nvndef.eq.0) )  then
          print *
          print *, 'Converting D-Grid Winds to A-Grid ...'
          if( nu.eq.0 .or. nv.eq.0 ) then
              print *
              print *, 'D-Grid Winds Not Found!'
              print *, 'UNAME: ',trim(uname),' nu: ',nu
              print *, 'VNAME: ',trim(vname),' nv: ',nv
              print *
              stop
          else
              call dtoa_winds ( u,v,q3d(1,1,1,nu),q3d(1,1,1,nv),im,jm,lm )
              name3d(nu) = 'u'
              name3d(nv) = 'v'
              name3d(nt) = 'thetav'
                  uname2 = 'u'
                  vname2 = 'v'
                  tname2 = 'thetav'
                   Ldtoa = .true.
          endif
          else
                  uname2 = uname
                  vname2 = vname
                  tname2 = tname
          endif

      else
        rc = 1  ! No more time periods in file
      endif

      return
      end subroutine read_eta_data

      subroutine convert ( q2d,q3d,name2d,titl2d,unit2d,name3d,titl3d,unit3d,n2d,n3d,undef,
     .                     levunits,im,jm,lm,nt,lat,lon,lev,nymd,nhms,ninc,
     .                     id,create,filename,imo,jmo,lmo,grid,
     .                     psname,dpname,uname,vname,tname,qname,ozname,phisname,topo,Ldtoa)

      use MAPL_ConstantsMod
      use m_set_eta, only: set_eta
      implicit none

c Input Variables
c ---------------
      integer  im,jm,lm,nt,nymd,nhms,ninc,n2d,n3d
      integer  nymd0,nhms0

      real   q2d(im,jm,   n2d)
      real   q3d(im,jm,lm,n3d)

      real  phis(imo,jmo)
      real    ps(imo,jmo)
      real    dp(imo,jmo,lm)
      real     u(imo,jmo,lm)
      real     v(imo,jmo,lm)
      real     t(imo,jmo,lm)
      real     q(imo,jmo,lm,2)

      integer    imo,jmo,lmo
      real   z2d(imo,jmo,   n2d)
      real   z3d(imo,jmo,lm,n3d)

      real phiso(imo,jmo)
      real   pso(imo,jmo)
      real   dpo(imo,jmo,lmo)
      real    uo(imo,jmo,lmo)
      real    vo(imo,jmo,lmo)
      real    to(imo,jmo,lmo)
      real    qo(imo,jmo,lmo,2)
      real dum3d(imo,jmo,lmo)
      real dum2d(imo,jmo)

      character*256  name2d(n2d), titl2d(n2d), unit2d(n2d)
      character*256  name3d(n3d), titl3d(n3d), unit3d(n3d)
      character*256  filename
      character*256  topo
      character*256  levunits
      character*256  grid
      character*256 psname
      character*256 dpname
      character*256  uname
      character*256  vname
      character*256  tname
      character*256  qname
      character*256 ozname
      character*256 phisname

      real   lat(jm) ,lon (im) ,lev (lm)
      real  lats(jmo),lons(imo),levs(lmo)
      logical create
      logical Ldtoa

c Local Variables
c ---------------
      integer i,j,k,L,n,m
      integer nu,nv,kdum

      real    undef, pi,dx,dy
      real*8  ptop, rdum
      integer precision,id,timeinc,rc,nhmsf

      character*256  title
      character*256  source
      character*256  contact

      integer nvars

      character*256, allocatable :: vnames(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
      integer,       allocatable ::  lmvar(:)
      real,          allocatable :: vrange(:,:)
      real,          allocatable :: prange(:,:)

      real,   allocatable ::    ple(:,:,:)
      real,   allocatable ::  logpl(:,:,:)
      real,   allocatable :: logplo(:,:,:)
      real*8, allocatable ::     ak(:)
      real*8, allocatable ::     bk(:)

      real   dpref 
             dpref(L) = ( ak(L+1)-ak(L) ) + ( bk(L+1)-bk(L) ) * 98400.0

C **********************************************************************
C ****           Initialize Constants And Local Arrays              ****
C **********************************************************************
 
      pi = 4.*atan(1.)
      dx = 360./ imo
      dy = 180./(jmo-1)
      if( im.ne.imo .or. jm.ne.jmo ) then
          do j=1,jmo
          lats(j) =  lat(1) + (j-1)*dy
          enddo
          do i=1,imo
          lons(i) =  lon(1) + (i-1)*dx
          enddo
      else
          lats = lat
          lons = lon
      endif

      nvars = n2d + n3d

C **********************************************************************
C ****           Interpolate A-Grid Horizontal Fields               ****
C **********************************************************************

      if( im.ne.imo .or. jm.ne.jmo ) then
          write(6,100) imo,jmo
  100     format(1x,'Interpolating to Horizontal Resolution (',i4,' x ',i4,') ...')
          do n=1,n2d
          call hinterp ( q2d(1,1,n),im,jm,z2d(1,1,n),imo,jmo,1,undef,1,3,.false. )
          enddo
          do n=1,n3d
          call hinterp ( q3d(1,1,1,n),im,jm,z3d(1,1,1,n),imo,jmo,lm,undef,1,3,.false. )
          enddo
      else
          z2d=q2d
          z3d=q3d
      endif

C **********************************************************************
C ****             Create D-Grid Winds if Desired                   ****
C **********************************************************************

      if(  trim(grid).eq.'atod' .or.
     .    (trim(grid).eq.'NULL' .and. Ldtoa) ) then
          print *, 'Converting A-Grid Winds to D-Grid ...'
          nu = 0
          nv = 0
          do n=1,n3d
             if( trim(name3d(n)).eq.trim(uname) ) then
                    nu = n
                     u = z3d(:,:,:,n)
             endif
             if( trim(name3d(n)).eq.trim(vname) ) then
                    nv = n
                     v = z3d(:,:,:,n)
             endif
          enddo

          if( nu.eq.0 .or. nv.eq.0 ) then
              print *
              print *, 'A-Grid Winds Not Found!'
              print *, 'UNAME: ',trim(uname),' nu: ',nu
              print *, 'VNAME: ',trim(vname),' nv: ',nv
              print *
              stop
          else
              name3d(nu) = 'uwnd' ; uname = 'uwnd'
              name3d(nv) = 'vwnd' ; vname = 'vwnd'
              call atod_winds ( u,v,z3d(1,1,1,nu),z3d(1,1,1,nv),imo,jmo,lm )
          endif
      endif
      print *

C **********************************************************************
C ****             Interpolate to New Output Levels                 ****
C **********************************************************************

      if( imo.ne.im .or. jmo.ne.jm .or. lmo.ne.lm ) then

          do n=1,n2d
             if( trim(name2d(n)).eq.trim(  psname) )   ps = z2d(:,:,n)
             if( trim(name2d(n)).eq.trim(phisname) ) phis = z2d(:,:,n)
          enddo
          do n=1,n3d
             if( trim(name3d(n)).eq.trim(  uname) )           u = z3d(:,:,:,n)
             if( trim(name3d(n)).eq.trim(  vname) )           v = z3d(:,:,:,n)
             if( trim(name3d(n)).eq.trim(  tname) )           t = z3d(:,:,:,n)
             if( trim(name3d(n)).eq.trim( dpname) )          dp = z3d(:,:,:,n)
             if( trim(name3d(n)).eq.trim(  qname) )  q(:,:,:,1) = z3d(:,:,:,n)
             if( trim(name3d(n)).eq.trim( ozname) )  q(:,:,:,2) = z3d(:,:,:,n)
          enddo

          if( imo.ne.im .or. jmo.ne.jm .or. trim(topo).ne.'NULL' ) then
              close(55)
              open (55,file=trim(topo),form='unformatted',access='sequential')
              read (55) phiso
                        phiso = phiso*MAPL_GRAV
              close(55)
              if( lon(1).eq.0.0 ) call hflip ( phiso,imo,jmo,1 )
          else
              phiso = phis
          endif


          allocate(   ak (lm+1) )
          allocate(   bk (lm+1) )
          allocate(   ple(imo,jmo,lm+1) )
          allocate( logpl(imo,jmo,lm)   )
          call set_eta ( lm,kdum,ptop,rdum,ak,bk )
          do L=1,lm+1
          ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
          enddo
          do L=1,lm
          logpl(:,:,L) = log( 0.5*(ple(:,:,L+1)+ple(:,:,L)) )
          enddo
          deallocate( ak,bk,ple )


          allocate(     ak(lmo+1) )
          allocate(     bk(lmo+1) )
          allocate(    ple(imo,jmo,lmo+1) )
          allocate( logplo(imo,jmo,lmo)   )
          call set_eta ( lmo,kdum,ptop,rdum,ak,bk )
          pso = ps
          do L=1,lmo+1
          ple(:,:,L) = ak(L) + pso(:,:)*bk(L)
          enddo
          do L=1,lmo
             dpo(:,:,L) = ple(:,:,L+1)-ple(:,:,L)
          logplo(:,:,L) = log( 0.5*(ple(:,:,L+1)+ple(:,:,L)) )
          enddo


          print *, 'Calling REMAP ...'
          call remap  ( pso,dpo,uo,vo,to,qo,phiso,lmo,
     .                  ps ,dp ,u ,v ,t ,q ,phis ,lm ,
     .                  imo,jmo,2 )
          print *, '        REMAP Finished'


          ptop    = ak(1)
          levs(1) = ptop + 0.5 * dpref(1)
          do L = 2, lmo
          levs(L) = levs(L-1) + 0.5 * ( dpref(L-1) + dpref(L) )
          enddo
          levs(1:lmo) = levs(1:lmo) / 100.0

          deallocate( ak,bk,ple )
      else
          levs(1:lmo) = lev(1:lmo)
      endif

C **********************************************************************
C ****                     Initialize GFIO File                     ****
C **********************************************************************
 
      allocate (   vnames(nvars) )
      allocate (   vtitle(nvars) )
      allocate (   vunits(nvars) )
      allocate (    lmvar(nvars) )

      timeinc   = nhmsf(ninc)
      precision = 1  ! 64-bit
      precision = 0  ! 32-bit

      title    = 'GEOS-5 GCM (DTOA Conversion)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'

c Defined Fields
c --------------
      do m=1,n2d
             n  = m
      vnames(n) = name2d(m)
      vtitle(n) = trim(titl2d(m))
      vunits(n) = trim(unit2d(m))
       lmvar(n) =  0
      enddo

      do m=1,n3d
             n  = n2d+m
      vnames(n) = name3d(m)
      vtitle(n) = trim(titl3d(m))
      vunits(n) = trim(unit3d(m))
       lmvar(n) = lmo
      enddo

C **********************************************************************
C ****                     Value Added Products                     ****
C **********************************************************************
 
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
      vrange(:,:) = undef
      prange(:,:) = undef

      if (create) then
             call GFIO_Create ( trim(filename), title, source, contact, undef,
     .                          imo, jmo, lmo, lons, lats, levs, levunits,
     .                          nymd, nhms, timeinc,
     .                          nvars, vnames, vtitle, vunits, lmvar,
     .                          vrange, prange, precision,
     .                          id, rc )
      endif

C **********************************************************************
C ****                    Write Defined Fields                      ****
C **********************************************************************

      do n=1,n2d
             if(      trim(name2d(n)).eq.trim(  psname) ) then
                                                          dum2d = pso
             else if( trim(name2d(n)).eq.trim(phisname) ) then
                                                          dum2d = phiso
             else
                                                          dum2d = z2d(:,:,n)
             endif
             call writit( dum2d, imo,jmo,1 ,id,name2d(n),nymd,nhms,undef )
      enddo
      do n=1,n3d
             if(      trim(name3d(n)).eq.trim(  uname) ) then
                                                         dum3d = uo
             else if( trim(name3d(n)).eq.trim(  vname) ) then
                                                         dum3d = vo
             else if( trim(name3d(n)).eq.trim(  tname) ) then
                                                         dum3d = to
             else if( trim(name3d(n)).eq.trim(  qname) ) then
                                                         dum3d = qo(:,:,:,1)
             else if( trim(name3d(n)).eq.trim( ozname) ) then
                                                         dum3d = qo(:,:,:,2)
             else if( trim(name3d(n)).eq.trim( dpname) ) then
                                                         dum3d = dpo
             else
               if( lm.ne.lmo ) then
                  print *, 'Calling sigtopl for: ',trim(name3d(n))
                  do L=1,lmo
                  do j=1,jmo
                  do i=1,imo
                  call  sigtopl( dum3d(i,j,L),z3d(i,j,:,n),logpl(i,j,:),logplo(i,j,L),1,1,lm,undef )
                  enddo
                  enddo
                  enddo
               else
                   dum3d = z3d(:,:,:,n)
               endif
             endif
         call writit( dum3d,imo,jmo,lmo,id,name3d(n),nymd,nhms,undef )
      enddo

C **********************************************************************
C ****                  De-Allocate Dynamics Arrays                 ****
C **********************************************************************

      deallocate (  vnames )
      deallocate (  vtitle )
      deallocate (  vunits )
      deallocate (   lmvar )
      deallocate (  vrange )
      deallocate (  prange )
      return
      end subroutine convert

      subroutine read_eta_meta ( hdffile,im,jm,lm,n2d,n3d,lat,lon,lev,undef,id,
     .                           nymdb,nhmsb,ndt,ntime,levunits,
     .                           nvars,names,name2d,titl2d,unit2d,name3d,titl3d,unit3d )
      implicit none

      real,          pointer :: lat   (:)
      real,          pointer :: lon   (:)
      real,          pointer :: lev   (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      character*256, pointer :: unit2d(:), unit3d(:)

      character*256 hdffile
      integer       id,im,jm,lm,n2d,n3d,nvars,nsecf,timeId,ncvid
      integer       ntime,ngatts,rc,timinc,nymdb,nhmsb,ndt
      real          undef
      integer       i,j,L,m,n
      character*256 name

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
                                                                                                                     
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable ::    loc(:)
                                                                                                                     
C **********************************************************************
C ****                  Read HDF File for Meta Data                 ****
C **********************************************************************

      call gfio_open       ( trim(hdffile),1,id,rc )
      call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
                                                                                                                     
      allocate ( lon(im) )
      allocate ( lat(jm) )
      allocate ( lev(lm) )
      allocate ( yymmdd(ntime) )
      allocate ( hhmmss(ntime) )
      allocate (  vname(nvars) )
      allocate (  names(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate (  kmvar(nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
                                                                                                                     
      timinc = 0
      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,undef,
     .                    lon,lat,lev,levunits,
     .                    yymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    vrange,prange,rc )

      if( timinc .eq. 0 ) then
          timeId = ncvid (id, 'time', rc)
          call ncagt     (id, timeId, 'time_increment', timinc, rc) 
          if( timinc .eq. 0 ) then
          print *
          print *, 'Warning, GFIO Inquire states TIMINC = ',timinc
          print *, '         This will be reset to 060000 '
          print *, '         Use -ndt NNNNNN (in seconds) to overide this'
          timinc = 060000
          endif
      endif

      if( ndt.eq.0 ) ndt = nsecf (timinc)

      nymdb  = yymmdd(1)
      nhmsb  = hhmmss(1)

      if( nhmsb.lt.0 ) then
          n = nsecf(abs(nhmsb))
          nhmsb = 0
          call tick (nymdb,nhmsb,-n)
      endif

      names  = vname

      n2d = 0
      n3d = 0
      do n=1,nvars
         if(      kmvar(n) .eq. 0    .or. 
     .       trim(vname(n)).eq.'LON' .or. 
     .       trim(vname(n)).eq.'LAT') then
                  n2d = n2d + 1
             kmvar(n) = 0
         else
             n3d = n3d + 1
         endif
      enddo

      allocate( name2d(n2d)  )
      allocate( titl2d(n2d)  )
      allocate( unit2d(n2d)  )
      allocate( name3d(n3d)  )
      allocate( titl3d(n3d)  )
      allocate( unit3d(n3d)  )

      n2d = 0
      n3d = 0
      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             n2d = n2d + 1
             name2d(n2d) = vname (n)
             titl2d(n2d) = vtitle(n)
             unit2d(n2d) = vunits(n)
         else
             n3d = n3d + 1
             name3d(n3d) = vname (n)
             titl3d(n3d) = vtitle(n)
             unit3d(n3d) = vunits(n)
         endif
      enddo

      call gfio_close ( id,rc )
      return
      end subroutine read_eta_meta

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*undef
      return
      end function defined

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
      end function nsecf

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
      end function nhmsf

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
      end subroutine tick

      function incymd (NYMD,M)
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
      end function incymd

      subroutine writit (q,im,jm,lm,id,name,nymd,nhms,undef)
      integer im,jm,lm,L
      integer id,nymd,nhms,rc,lbeg
      character*256 name
      real    q (im,jm,lm)
      real    undef
                    lbeg = 1
      if( lm.eq.1 ) lbeg = 0
      write(6,100) trim(name)
  100 format(1x,'       Writing variable: ',a)
      call Gfio_putVar ( id,trim(name),nymd,nhms,im,jm,lbeg,lm,q,rc )
      return
      end subroutine writit

      subroutine usage()
      write(6,100)
 100  format(  "Usage:  "                                                                                               ,/
     .                                                                                                                  ,/
     .         "   convert_eta.x -f     fname(s)"                                                                       ,/
     .         "                [-im    imout]"                                                                         ,/
     .         "                [-jm    jmout]"                                                                         ,/
     .         "                [-lm    lmout]"                                                                         ,/
     .         "                [-grid  conversion]"                                                                    ,/
     .         "                [-topo  topo_fname]"                                                                    ,/
     .         "                [-tag   output_tag] "                                                                   ,/
     .                                                                                                                  ,/
     .         "where:"                                                                                                 ,/
     .                                                                                                                  ,/
     .         "  -f        fname(s):  Filename(s) in HDF format"                                                       ,/
     .                                                                                                                  ,/
     .         "Optional Args:"                                                                                         ,/
     .                                                                                                                  ,/
     .         "  -im          imout:  Optional Output Resolution in X (default: Input Resolution)"                     ,/
     .         "  -jm          jmout:  Optional Output Resolution in Y (default: Input Resolution)"                     ,/
     .         "  -lm          lmout:  Optional Output Resolution in Z (default: Input Resolution)"                     ,/
     .         "  -grid   conversion:  Optional Grid   Conversion (dtoa or atod)"                                       ,/
     .         "  -topo   topo_fname:  Optional Filename for Output Topography File"                                    ,/
     .         "  -tag    output_tag:  Optional Filename Tag for output: fname(s).tag (default: grid.IMxJM)"            ,/
     .                                                                                                                  ,/
     .         ) 
      call exit(7)
      end subroutine usage

      subroutine hinterp ( qin,iin,jin,qout,iout,jout,mlev,undef,msgn,norder,check )
      implicit   none
      integer    iin,jin,       iout,jout, mlev,msgn,norder
      real   qin(iin,jin,mlev), qout(iout,jout,mlev)
      real undef,pi,dlin,dpin,dlout,dpout
      real dlam(iin), lons(iout*jout), lon
      real dphi(jin), lats(iout*jout), lat
      integer i,j,loc
      logical check

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

      subroutine remap  ( ps1,dp1,u1,v1,thv1,q1,phis1,lm1,
     .                    ps2,dp2,u2,v2,thv2,q2,phis2,lm2,im,jm,nt )

c *******************************************************************************
c *****                                                                     *****
c *****   Program to remap Target analysis variables (ps2,dp2,u2,v2,t2,q2)  *****
c *****   onto Model grid variables (ps1,dp1,u1,v1,thv1,q1).  Program       *****
c *****   allows for different topographies between Analysis and Model.     *****
c *****                                                                     *****
c *******************************************************************************

      use MAPL_ConstantsMod
      use m_set_eta, only: set_eta
      implicit none
      integer  im,jm,lm1,lm2,nt

c Model variables
c ---------------
      real      dp1(im,jm,lm1)
      real       u1(im,jm,lm1)
      real       v1(im,jm,lm1)
      real     thv1(im,jm,lm1)
      real       q1(im,jm,lm1,nt)
      real      ps1(im,jm)

      real   phis1(im,jm)

c Target analysis variables
c -------------------------
      real     dp2(im,jm,lm2)
      real      u2(im,jm,lm2)
      real      v2(im,jm,lm2)
      real      t2(im,jm,lm2)
      real    thv2(im,jm,lm2)
      real      q2(im,jm,lm2,nt)
      real     ps2(im,jm)
      real   phis2(im,jm)

c Local variables
c ---------------
      real    pz(im,jm)
      real   pe0(im,jm,lm1+1)
      real   pe1(im,jm,lm1+1)
      real   pe2(im,jm,lm2+1)
      real   pk (im,jm,lm2  )
      real  pke0(im,jm,lm1+1)
      real  pke1(im,jm,lm1+1)
      real  pke2(im,jm,lm2+1)
      real  phi2(im,jm,lm2+1)
      real ptop1(im,jm)

      real*8  rdum
      real    kappa,cp,pl,dum,dum1,dum2
      real    rgas,pref,tref,pkref,tstar,eps,rvap,grav
      integer i,j,L,kdum

      real*8, allocatable :: ak(:)
      real*8, allocatable :: bk(:)

      kappa = MAPL_KAPPA
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      eps   = rvap/rgas-1.0
 
c Compute Input Edge-Level Pressures
c ----------------------------------

      allocate(   ak (lm1+1) )
      allocate(   bk (lm1+1) )
      call set_eta ( lm1,kdum,rdum,rdum,ak,bk )
      do L=1,lm1+1
         pe1(:,:,L) = ak(L) + ps1(:,:)*bk(L)
      enddo
      deallocate( ak,bk )

      ptop1(:,:) = pe1(:,:,1)

c Compute Output Edge-Level Pressures
c -----------------------------------
      allocate(   ak (lm2+1) )
      allocate(   bk (lm2+1) )
      call set_eta ( lm2,kdum,rdum,rdum,ak,bk )
      do L=1,lm2+1
         pe2(:,:,L) = ak(L) + ps2(:,:)*bk(L)
      enddo
      deallocate( ak,bk )

      pke2(:,:,:) = pe2(:,:,:)**kappa

c Construct target virtual potential temperature
c ----------------------------------------------
      do L=1,lm2
      pk(:,:,L) = ( pke2(:,:,L+1)-pke2(:,:,L) )/( kappa*log(pe2(:,:,L+1)/pe2(:,:,L)) )
      enddo

c Construct target analysis heights
c ---------------------------------
      phi2(:,:,lm2+1) = phis2(:,:)
      do L=lm2,1,-1
      phi2(:,:,L) = phi2(:,:,L+1) + cp*thv2(:,:,L)*( pke2(:,:,L+1)-pke2(:,:,L) )
      enddo
      
c Compute new surface pressure consistent with target surface pressure and topography
c -----------------------------------------------------------------------------------
      do j=1,jm
      do i=1,im
           L = lm2
           do while ( phi2(i,j,L).lt.phis1(i,j) )
           L = L-1
           enddo
           ps1(i,j) = pe2(i,j,L+1)*( 1+(phi2(i,j,L+1)-phis1(i,j))/(cp*thv2(i,j,L)*pke2(i,j,L+1)) )**(1.0/kappa)
      enddo
      enddo

c Construct model pressure variables using new surface pressure
c -------------------------------------------------------------
      print *
      allocate( ak (lm1+1) )
      allocate( bk (lm1+1) )
      call set_eta ( lm1,kdum,rdum,rdum,ak,bk )
         L = 1
      dum1 = (ak(L)+bk(L)*100000.0)/100
      write(6,1000) L,ak(L),bk(L),dum1
 1000 format(1x,'L: ',i3,4x,'ak: ',f10.3,'   bk: ',f10.8,4x,'pe: ',f8.3)
      do L=2,lm1+1
      dum2 = (ak(L)+bk(L)*100000.0)/100
      write(6,1001) L,ak(L),bk(L),dum2,dum2-dum1
 1001 format(1x,'L: ',i3,4x,'ak: ',f10.3,'   bk: ',f10.8,4x,'pe: ',f8.3,3x,'dp: ',f7.3)
      dum1 = dum2
      enddo
      print *

      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1 (i,j,L) = ak(L)+bk(L)*ps1(i,j)
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

c Map target analysis onto grid defined by new surface pressure
c -------------------------------------------------------------
      print *, 'Calling GMAP, LM_in : ',lm2
      print *, '              LM_out: ',lm1
      call gmap ( im,jm,nt, kappa,
     .            lm2,  pke2,  pe2, u2,  v2,  thv2,  q2,
     .            lm1,  pke1,  pe1, u1,  v1,  thv1,  q1 )

      deallocate( ak,bk )
      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine gmap(im, jm, nq,  akap,
     &         km,  pk3d_m,  pe3d_m, u_m,  v_m,  pt_m,  q_m,
     &         kn,  pk3d_n,  pe3d_n, u_n,  v_n,  pt_n,  q_n  )
c****6***0*********0*********0*********0*********0*********0**********72

      implicit none

      integer im, jm
      integer km, kn, nq

C Input:
C original data km-level

      real      u_m(im,jm,km)
      real      v_m(im,jm,km)
      real     pt_m(im,jm,km)
      real      q_m(im,jm,km,nq)
      real   pk3d_m(im,jm,km+1)
      real   pe3d_m(im,jm,km+1)


C Output:
C New data (kn-level)
      real      u_n(im,jm,kn)
      real      v_n(im,jm,kn)
      real     pt_n(im,jm,kn)
      real      q_n(im,jm,kn,nq)
      real   pk3d_n(im,jm,kn+1)
      real   pe3d_n(im,jm,kn+1)

c local (private)
      integer i, j, k, ic, n

      real pe1(im,km+1),pe2(im,kn+1)
      real pk1(im,km+1),pk2(im,kn+1)
      real dp1(im,km)  ,dp2(im,kn)
      real  u1(im,km)  , u2(im,kn)
      real  v1(im,km)  , v2(im,kn)
      real  t1(im,km)  , t2(im,kn)
      real  q1(im,km)  , q2(im,kn)

      real ptop
      real akap
      real ple,  pek, dak, bkh
      real undef
      real big
      parameter ( undef = 1.e15 )
      parameter (   big = 1.e10 )


      do 2000 j=1,jm

c Copy original data to local 2D arrays.

      do k=1,km+1
      do i=1,im
      pe1(i,k) = pe3d_m(i,j,k)
      pk1(i,k) = pk3d_m(i,j,k)
      enddo
      enddo

      do k=1,kn+1
      do i=1,im
      pe2(i,k) = pe3d_n(i,j,k)
      pk2(i,k) = pk3d_n(i,j,k)
      enddo
      enddo

      do k=1,km
      do i=1,im
      dp1(i,k) =  pk1(i,k+1)-pk1(i,k)
       u1(i,k) =  u_m(i,j,k)
       v1(i,k) =  v_m(i,j,k)
       t1(i,k) = pt_m(i,j,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pk2(i,k+1)-pk2(i,k)
      enddo
      enddo

c map pt
c ------
      call mappm ( km, pk1, dp1, t1, kn, pk2, dp2, t2, im, 1, 7 )

      do k=1,km
      do i=1,im
      dp1(i,k) = pe1(i,k+1)-pe1(i,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pe2(i,k+1)-pe2(i,k)
      enddo
      enddo

c map u,v
c -------
      call mappm ( km, pe1, dp1, u1, kn, pe2, dp2, u2, im, -1, 7 )
      call mappm ( km, pe1, dp1, v1, kn, pe2, dp2, v2, im, -1, 7 )

c map q
c -------
      do n=1,nq
         do k=1,km
         do i=1,im
          q1(i,k) =  q_m(i,j,k,n)
         enddo
         enddo
         call mappm ( km, pe1, dp1, q1, kn, pe2, dp2, q2, im,  0, 7 )
         do k=1,kn
         do i=1,im
           q_n(i,j,k,n) = q2(i,k)
         enddo
         enddo
      enddo

      do k=1,kn
      do i=1,im
        u_n(i,j,k) = u2(i,k)
        v_n(i,j,k) = v2(i,k)
       pt_n(i,j,k) = t2(i,k)
      enddo
      enddo

2000  continue

      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine mappm(km, pe1, dp1, q1, kn, pe2, dp2, q2, im, iv, kord)
C****6***0*********0*********0*********0*********0*********0**********72
C IV = 0: constituents
C IV = 1: potential temp
C IV =-1: winds
C
C Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
C
C pe1: pressure at layer edges (from model top to bottom surface)
C      in the original vertical coordinate
C pe2: pressure at layer edges (from model top to bottom surface)
C      in the new vertical coordinate

      parameter (kmax = 200)
      parameter (R3 = 1./3., R23 = 2./3.)

      real dp1(im,km),   dp2(im,kn),
     &      q1(im,km),    q2(im,kn),
     &     pe1(im,km+1), pe2(im,kn+1)
      integer kord

C local work arrays
      real a4(4,im,km)

      do k=1,km
         do i=1,im
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      call ppm2m(a4, dp1, im, km, iv, kord)

C Lowest layer: constant distribution
      do i=1, im
         a4(2,i,km) = q1(i,km)
         a4(3,i,km) = q1(i,km)
         a4(4,i,km) = 0.
      enddo

      do 5555 i=1,im
         k0 = 1
      do 555 k=1,kn

         if(pe2(i,k+1) .le. pe1(i,1)) then
! Entire grid above old ptop
            q2(i,k) = a4(2,i,1)
         elseif(pe2(i,k) .ge. pe1(i,km+1)) then
! Entire grid below old ps
            q2(i,k) = a4(3,i,km)
         elseif(pe2(i,k  ) .lt. pe1(i,1) .and.
     &          pe2(i,k+1) .gt. pe1(i,1))  then
! Part of the grid above ptop
            q2(i,k) = a4(1,i,1)
         else

         do 45 L=k0,km
! locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and.
     &       pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

! entire new grid is within the original grid
               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = R3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L)
     &                 - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else
! Fractional area...
              delp = pe1(i,L+1) - pe2(i,k)
              TT   = R3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+
     &               a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
              dpsum = delp
              k1 = L + 1
             goto 111
             endif
         endif
45       continue

111      continue
         do 55 L=k1,km
         if( pe2(i,k+1) .gt. pe1(i,L+1) ) then

! Whole layer..

            qsum  =  qsum + dp1(i,L)*q1(i,L)
            dpsum = dpsum + dp1(i,L)
         else
           delp = pe2(i,k+1)-pe1(i,L)
           esl  = delp / dp1(i,L)
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl*
     &           (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
          dpsum = dpsum + delp
           k0 = L
           goto 123
         endif
55       continue
        delp = pe2(i,k+1) - pe1(i,km+1)
        if(delp .gt. 0.) then
! Extended below old ps
           qsum = qsum + delp * a4(3,i,km)
          dpsum = dpsum + delp
        endif
123     q2(i,k) = qsum / dpsum
      endif
555   continue
5555  continue

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine ppm2m(a4,delp,im,km,iv,kord)
c****6***0*********0*********0*********0*********0*********0**********72
c iv = 0: positive definite scalars
c iv = 1: others
c iv =-1: winds

      implicit none

      integer im, km, lmt, iv
      integer kord
      integer i, k, km1
      real a4(4,im,km), delp(im,km)

c local arrays.
      real dc(im,km),delq(im,km)
      real h2(im,km)
      real a1, a2, a3, b2, c1, c2, c3, d1, d2, f1, f2, f3, f4
      real s1, s2, s3, s4, ss3, s32, s34, s42, sc
      real qmax, qmin, cmax, cmin
      real dm, qm, dq, tmp

C Local scalars:
      real qmp
      real lac

      km1 = km - 1

      do 500 k=2,km
      do 500 i=1,im
      delq(i,k-1) = a4(1,i,k) - a4(1,i,k-1)
500   a4(4,i,k  ) = delp(i,k-1) + delp(i,k)

      do 1220 k=2,km1
      do 1220 i=1,im
      c1 = (delp(i,k-1)+0.5*delp(i,k))/a4(4,i,k+1)
      c2 = (delp(i,k+1)+0.5*delp(i,k))/a4(4,i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /
     &                              (a4(4,i,k)+delp(i,k+1))
      qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
      qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

c****6***0*********0*********0*********0*********0*********0**********72
c 4th order interpolation of the provisional cell edge value
c****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) *
     &          ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -
     &                          delp(i,k-1)*a1*dc(i,k  ) )
12    continue

C Area preserving cubic with 2nd deriv. = 0 at the boundaries
C Top
      do i=1,im
      d1 = delp(i,1)
      d2 = delp(i,2)
      qm = (d2*a4(1,i,1)+d1*a4(1,i,2)) / (d1+d2)
      dq = 2.*(a4(1,i,2)-a4(1,i,1)) / (d1+d2)
      c1 = 4.*(a4(2,i,3)-qm-d2*dq) / ( d2*(2.*d2*d2+d1*(d2+3.*d1)) )
      c3 = dq - 0.5*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,2) = qm - 0.25*c1*d1*d2*(d2+3.*d1)
      a4(2,i,1) = d1*(2.*c1*d1**2-c3) + a4(2,i,2)
      dc(i,1) =  a4(1,i,1) - a4(2,i,1)
C No over- and undershoot condition
      cmax = max(a4(1,i,1), a4(1,i,2))
      cmin = min(a4(1,i,1), a4(1,i,2))
      a4(2,i,2) = max(cmin,a4(2,i,2))
      a4(2,i,2) = min(cmax,a4(2,i,2))
      enddo

      if(iv == 0) then
         do i=1,im
            a4(2,i,1) = max(0.,a4(2,i,1))
            a4(2,i,2) = max(0.,a4(2,i,2))
         enddo
      elseif(iv == -1) then
         do i=1,im
            if( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
         enddo
      endif

c****6***0*********0*********0*********0*********0*********0**********72

c Bottom
c Area preserving cubic with 2nd deriv. = 0 at the surface
      do 15 i=1,im
      d1 = delp(i,km)
      d2 = delp(i,km1)
      qm = (d2*a4(1,i,km)+d1*a4(1,i,km1)) / (d1+d2)
      dq = 2.*(a4(1,i,km1)-a4(1,i,km)) / (d1+d2)
      c1 = (a4(2,i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
      c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,km) = qm - c1*d1*d2*(d2+3.*d1)
      a4(3,i,km) = d1*(8.*c1*d1**2-c3) + a4(2,i,km)
      dc(i,km) = a4(3,i,km) -  a4(1,i,km)
c****6***0*********0*********0*********0*********0*********0**********72
c No over- and undershoot condition
      cmax = max(a4(1,i,km), a4(1,i,km1))
      cmin = min(a4(1,i,km), a4(1,i,km1))
      a4(2,i,km) = max(cmin,a4(2,i,km))
      a4(2,i,km) = min(cmax,a4(2,i,km))
c****6***0*********0*********0*********0*********0*********0**********72
15    continue

      if(iv .eq. 0) then
      do i=1,im
         a4(2,i,km) = max(0.,a4(2,i,km))
         a4(3,i,km) = max(0.,a4(3,i,km))
      enddo
      endif

      do 20 k=1,km1
      do 20 i=1,im
      a4(3,i,k) = a4(2,i,k+1)
20    continue
c
c f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
c

c Top 2 and bottom 2 layers always use monotonic mapping

      do k=1,2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
      enddo

      if(kord == 7) then
c****6***0*********0*********0*********0*********0*********0**********72
C Huynh's 2nd constraint
c****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=1,im
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 4000 k=3, km-2
      do 3000 i=1, im
C Right edges
         qmp   = a4(1,i,k)                 + 2.0*delq(i,k-1)
         lac   = a4(1,i,k) + 1.5*h2(i,k-1) + 0.5*delq(i,k-1)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(3,i,k) = min(max(a4(3,i,k), qmin), qmax)
C Left  edges
         qmp   = a4(1,i,k)                 - 2.0*delq(i,k)
         lac   = a4(1,i,k) + 1.5*h2(i,k+1) - 0.5*delq(i,k)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(2,i,k) = min(max(a4(2,i,k), qmin), qmax)
C Recompute A6
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
3000  continue
! Additional constraint to prevent negatives
         if (iv == 0) then
             call kmppm(dc(1,k),a4(1,1,k),im, 2)
         endif
4000  continue

      else

         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv .eq. 0) lmt = min(2, lmt)

      do k=3, km-2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, lmt)
      enddo
      endif

      do 5000 k=km1,km
         do i=1,im
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
5000  continue

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine kmppm(dm, a4, km, lmt)
c****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      real r12
      parameter (r12 = 1./12.)

      integer km, lmt
      integer i
      real a4(4,km),dm(km)
      real da1, da2, a6da
      real fmin
      real qmp

      if (lmt .eq. 3) return
! Full constraint

      if(lmt .eq. 0) then
      do 100 i=1,km
      if(dm(i) .eq. 0.) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da .lt. -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da .gt. da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
100   continue
      elseif (lmt .eq. 2) then
c Positive definite

c Positive definite constraint
      do 250 i=1,km
      if(abs(a4(3,i)-a4(2,i)) .ge. -a4(4,i)) go to 250
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
      if(fmin.ge.0.) go to 250
      if(a4(1,i).lt.a4(3,i) .and. a4(1,i).lt.a4(2,i)) then
            a4(3,i) = a4(1,i)
            a4(2,i) = a4(1,i)
            a4(4,i) = 0.
      elseif(a4(3,i) .gt. a4(2,i)) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
      else
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
      endif
250   continue

      elseif (lmt == 1) then

! Improved full monotonicity constraint (Lin)
! Note: no need to provide first guess of A6 <-- a4(4,i)

      do i=1, km
           qmp = 2.*dm(i)
         a4(2,i) = a4(1,i)-sign(min(abs(qmp),abs(a4(2,i)-a4(1,i))), qmp)
         a4(3,i) = a4(1,i)+sign(min(abs(qmp),abs(a4(3,i)-a4(1,i))), qmp)
         a4(4,i) = 3.*( 2.*a4(1,i) - (a4(2,i)+a4(3,i)) )
      enddo
      endif

      return
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L
      real   q(im,jm,lm),dum(im)
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

