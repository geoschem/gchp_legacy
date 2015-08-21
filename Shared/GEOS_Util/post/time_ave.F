      program  time_ave

      use ESMF

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

#ifdef mpi
      include 'mpif.h'
#endif

      integer  comm,myid,npes,ierror
      integer  imglobal
      integer  jmglobal
      integer  npex,npey
      logical  root

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****        Program to create time-averaged HDF files             ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm

      integer  nymd, nhms
      integer  nymd0,nhms0
      integer  nymdp,nhmsp
      integer  nymdm,nhmsm
      integer  ntod, ndt, ntods
      integer  month,  year
      integer  monthp, yearp
      integer  monthm, yearm
      integer  begdate, begtime
      integer  enddate, endtime

      integer id,rc,fid,precision,timeinc,timeid
      integer ntime,nvars,ngatts,ncvid,nvars2

      character*256, allocatable ::   arg(:)
      character*256, allocatable :: fname(:)
      character*256  template
      character*256  name
      character*256  ext
      character*256  output, doutput, hdfile, rcfile
      character*8    date,date0
      character*2    time,time0
      character*1    char
      data output  /'monthly_ave'/
      data rcfile  /'NULL'/
      data doutput /'NULL'/
      data template/'NULL'/

      integer n,m,nargs,iargc,L,nbeg,nfiles,mlev,nv,km,mvars,mv,ndvars

      real     plev,qming,qmaxg
      real     undef
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real*8,    allocatable ::    lon2(:,:), lat2(:,:)
      logical              :: twoDimLat
      real,    allocatable :: vrange(:,:), vrange2(:,:)
      real,    allocatable :: prange(:,:), prange2(:,:)
      integer, allocatable ::  kmvar(:)  ,  kmvar2(:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::   nloc(:)
      integer, allocatable ::   iloc(:)

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:),  vname2(:)
      character*256, allocatable :: vtitle(:), vtitle2(:)
      character*256, allocatable :: vunits(:), vunits2(:)
      character*256, allocatable :: coords(:), coords2(:)

      real,    allocatable ::   qmin(:)
      real,    allocatable ::   qmax(:)
      real,    allocatable ::  dumz1(:,:)
      real,    allocatable ::  dumz2(:,:)
      real,    allocatable ::    dum(:,:,:)
      real*8,  allocatable ::      q(:,:,:,:)
      integer, allocatable :: ntimes(:,:,:,:)

      integer timinc,i,j,k,nmax,kbeg,kend,loc1,loc2
      integer imax,jmax,nstar
      logical defined, tend, first, strict, diurnal, mdiurnal, lquad, ldquad
      data first  /.true./
      data strict /.true./

      type(ESMF_Config)   :: config

      integer,       allocatable ::       qloc(:,:)
      character*256, allocatable :: quadratics(:,:)
      character*256, allocatable ::    quadtmp(:,:)
      character*256, allocatable ::    aliases(:,:)
      character*256, allocatable ::   aliastmp(:,:)
      character*256  name1, name2, name3, name4, name5, dummy
      integer        nquad
      integer        nalias
      logical,       allocatable :: lzstar(:)

      integer NSECF, ntmin, ntcrit, nhmsf, nc
              NSECF(N) = N/10000*3600 + MOD(N,10000)/100* 60 + MOD(N,100)

C **********************************************************************
C ****                       Initialization                         ****
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
      root = myid.eq.0

c Read Command Line Arguments
c ---------------------------
        begdate = -999
        begtime = -999
        enddate = -999
        endtime = -999
            ndt = -999
           ntod = -999
          ntmin = -999
          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage(root)
      else
           lquad   = .TRUE.
           ldquad  = .FALSE.
           diurnal = .FALSE.
          mdiurnal = .FALSE.
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-template' ) template = arg(n+1)
             if( trim(arg(n)).eq.'-tag'      )   output = arg(n+1)
             if( trim(arg(n)).eq.'-rc'       )   rcfile = arg(n+1)
             if( trim(arg(n)).eq.'-begdate'  )     read ( arg(n+1),* ) begdate
             if( trim(arg(n)).eq.'-begtime'  )     read ( arg(n+1),* ) begtime
             if( trim(arg(n)).eq.'-enddate'  )     read ( arg(n+1),* ) enddate
             if( trim(arg(n)).eq.'-endtime'  )     read ( arg(n+1),* ) endtime
             if( trim(arg(n)).eq.'-ntmin'    )     read ( arg(n+1),* ) ntmin
             if( trim(arg(n)).eq.'-ntod'     )     read ( arg(n+1),* ) ntod
             if( trim(arg(n)).eq.'-ndt'      )     read ( arg(n+1),* ) ndt
             if( trim(arg(n)).eq.'-strict'   )     read ( arg(n+1),* ) strict
             if( trim(arg(n)).eq.'-noquad'   )     lquad = .false.

             if( trim(arg(n)).eq.'-dv'.or. trim(arg(n)).eq.'-mdv') ldquad = .true.

             if( trim(arg(n)).eq.'-d' .or. trim(arg(n)).eq.'-dv' ) then
                 diurnal = .TRUE. 
                 if( n+1.le.nargs ) then
                     read(arg(n+1),fmt='(a1)') char
                     if( char.ne.'-' ) doutput = arg(n+1)
                 endif
             endif

             if( trim(arg(n)).eq.'-md' .or. trim(arg(n)).eq.'-mdv' ) then
                 mdiurnal = .TRUE. 
                 if( n+1.le.nargs ) then
                     read(arg(n+1),fmt='(a1)') char
                     if( char.ne.'-' ) doutput = arg(n+1)
                 endif
             endif

             if( trim(arg(n)).eq.'-eta'  .or. 
     .           trim(arg(n)).eq.'-hdf' ) then  ! Backward compatable for old interface
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

      if( (diurnal.or.mdiurnal) .and. trim(doutput).eq.'NULL' ) then
           doutput = trim(output) // "_diurnal"
           if( mdiurnal ) diurnal = .FALSE.
      endif


c Read RC Quadratics
c ------------------
      if( trim(rcfile).eq.'NULL' ) then
          nquad  = 0
          nalias = 0
      else

      config = ESMF_ConfigCreate    ( rc=rc )
        call   ESMF_ConfigLoadFile  ( config, trim(rcfile),  rc=rc )
        call   ESMF_ConfigFindLabel ( config, 'QUADRATICS:', rc=rc )
        tend = .false.
        m = 0
        do while (.not.tend)
           m = m+1
           allocate( quadtmp(3,m) )
           call ESMF_ConfigGetAttribute ( config,value=name1,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=name2,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=name3,default='XXX',rc=rc )
           call ESMF_ConfigNextLine     ( config,tableEnd=tend, rc=rc )
           if( m==1 ) then
              quadtmp(1,m)     = name1
              quadtmp(2,m)     = name2
              quadtmp(3,m)     = name3
              allocate( quadratics(3,m) )
                        quadratics = quadtmp
           else
              quadtmp(1,1:m-1) = quadratics(1,:)
              quadtmp(2,1:m-1) = quadratics(2,:)
              quadtmp(3,1:m-1) = quadratics(3,:)
              quadtmp(1,m)     = name1
              quadtmp(2,m)     = name2
              quadtmp(3,m)     = name3
              deallocate( quadratics      )
                allocate( quadratics(3,m) )
                          quadratics = quadtmp
           endif
           deallocate (quadtmp)
       enddo
       nquad = m

c Read RC Aliases
c ---------------
        call   ESMF_ConfigFindLabel ( config, 'ALIASES:', rc=rc )
        tend = .false.
        m = 0
        do while (.not.tend)
           m = m+1
           allocate( aliastmp(2,m) )
           call ESMF_ConfigGetAttribute ( config,value=name1,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
           call ESMF_ConfigGetAttribute ( config,value=name2,   rc=rc )
           call ESMF_ConfigNextLine     ( config,tableEnd=tend  ,rc=rc )
           if( m==1 ) then
              aliastmp(1,m)     = name1
              aliastmp(2,m)     = name2
              allocate( aliases(2,m) )
                        aliases = aliastmp
           else
              aliastmp(1,1:m-1) = aliases(1,:)
              aliastmp(2,1:m-1) = aliases(2,:)
              aliastmp(1,m)     = name1
              aliastmp(2,m)     = name2
              deallocate( aliases      )
                allocate( aliases(2,m) )
                          aliases = aliastmp
           endif
           deallocate (aliastmp)
       enddo
       nalias = m
       endif

C **********************************************************************
C ****                        Read HDF File                         ****
C **********************************************************************

      call timebeg(' initialize')

      if( trim(template).ne.'NULL' ) then
          name = template
      else
          name = fname(1)
      endif

         n = index(trim(name),'.',back=.true.)
       ext = trim(name(n+1:))

      call gfio_open ( trim(name),1,ID,rc )
      call gfio_diminquireCF(id,imglobal,jmglobal,lm,ntime,nvars,ngatts,twoDimLat,rc)
      nvars2 = nvars

      call create_dynamics_lattice ( lattice,npex,npey )
      call   init_dynamics_lattice ( lattice,comm,imglobal,jmglobal,lm )

      im = lattice%im( lattice%pei )
      jm = lattice%jm( lattice%pej )

      allocate ( lon(imglobal)   )
      allocate ( lat(jmglobal)   )
      if (twoDimLat) then
         allocate ( lon2(imglobal,jmglobal) )
         allocate ( lat2(imglobal,jmglobal) )
      endif
      allocate ( lev(lm)         )
      allocate ( yymmdd(  ntime) )
      allocate ( hhmmss(  ntime) )
      allocate (  vname(  nvars) )
      allocate ( vtitle(  nvars) )
      allocate ( vunits(  nvars) )
      allocate (  kmvar(  nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
      allocate ( coords(  nvars) )

      call gfio_inquireCF ( id,imglobal,jmglobal,lm,ntime,nvars,
     .                      title,source,contact,undef,
     .                      lon,lat,lev,levunits,
     .                      yymmdd,hhmmss,timinc,
     .                      vname,vtitle,vunits,kmvar,
     .                      vrange,prange,coords, twoDimLat, lat2, lon2, rc)
      call gfio_close   ( id,rc )

c Set NDT for Strict Time Testing
c -------------------------------
      if( ntod.ne.-999 ) ndt = 86400
      if( ndt .eq.-999 ) ndt = nsecf (timinc)
      if( timinc .eq. 0 ) then
          timeId = ncvid (id, 'time', rc)
          call ncagt     (id, timeId, 'time_increment', timinc, rc)
          if( timinc .eq. 0 ) then
          if( root ) then
              print *
              print *, 'Warning, GFIO Inquire states TIMINC = ',timinc
              print *, '         This will be reset to 060000 '
              print *, '         Use -ndt NNN (in seconds) to overide this'
          endif
          timinc = 060000
          endif
          ndt = nsecf (timinc)
      endif

c Determine Number of Time Periods within 1-Day
c ---------------------------------------------
          ntods = 0
      if( diurnal .or. mdiurnal ) then
          if( ndt.lt.86400 ) ntods = 86400/ndt
      endif

c Set Minimum Required Times for Time Average (Default: 10 Days for Monthly Mean)
c -------------------------------------------------------------------------------
      if( ntmin.eq.-999 ) then
          if( ntod.eq.-999 ) then
              ntcrit = 10 * ( 86400.0/real(nsecf(timinc)) )
          else
              ntcrit = 10
          endif
      else
          ntcrit = ntmin
      endif

c Determine Location Index for Each Variable in File
c --------------------------------------------------
      if( root ) print *
      allocate ( nloc(nvars) )
      nloc(1) = 1
      if( root ) write(6,7000) 1,trim(vname(1)),nloc(1),trim(vtitle(1)),max(1,kmvar(1))
      do n=2,nvars
      nloc(n) = nloc(n-1)+max(1,kmvar(n-1))
      if( root ) write(6,7000) n,trim(vname(n)),nloc(n),trim(vtitle(n)),max(1,kmvar(n))
 7000 format(1x,'Primary Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,a40,2x,i2,3x,i2,3x,i2)
      enddo

      nmax  = nloc(nvars)+max(1,kmvar(nvars))-1
      allocate( dum  (im,jm,nmax) )
      allocate( dumz1(im,jm) )
      allocate( dumz2(im,jm) )

c Append Default Quadratics to User-Supplied List
c -----------------------------------------------
      if( lquad ) then
      if( nquad.eq.0 ) then
          allocate( quadratics(3,nvars) )
          do n=1,nvars
          quadratics(1,n) = trim( vname(n) )
          quadratics(2,n) = trim( vname(n) )
          quadratics(3,n) = 'XXX'
          enddo
          nquad = nvars
      else
          allocate( quadtmp(3,nquad+nvars) )
          quadtmp(1,1:nquad) = quadratics(1,:)
          quadtmp(2,1:nquad) = quadratics(2,:)
          quadtmp(3,1:nquad) = quadratics(3,:)
          do n=1,nvars
          quadtmp(1,nquad+n) = trim( vname(n) )
          quadtmp(2,nquad+n) = trim( vname(n) )
          quadtmp(3,nquad+n) = 'XXX'
          enddo
          nquad = nquad + nvars
          deallocate( quadratics   )
            allocate( quadratics(3,nquad) )
                      quadratics = quadtmp
          deallocate( quadtmp )
      endif
      endif

      allocate ( qloc(2,nquad) )
      allocate ( lzstar(nquad) ) ; lzstar = .FALSE.

c Determine Possible Quadratics
c -----------------------------
        km=kmvar(nvars)
         m=      nvars
      do n=1,nquad
         call check_quad ( quadratics(1,n),vname,nloc,nvars,aliases,nalias,qloc(1,n) )
         if( qloc(1,n)*qloc(2,n).ne.0 ) then
             m=m+1
             allocate ( iloc(m) )
                        iloc(1:m-1) = nloc
             iloc(m) = iloc(m-1)+max(1,km)
             deallocate ( nloc )
               allocate ( nloc(m) )
                          nloc = iloc
             deallocate ( iloc )
             km=kmvar( qloc(1,n) )
         endif
      enddo

      mvars = m
      nmax  = nloc(m)+max(1,km)-1

      allocate (  vname2(  mvars) )
      allocate ( vtitle2(  mvars) )
      allocate ( vunits2(  mvars) )
      allocate (  kmvar2(  mvars) )
      allocate ( vrange2(2,mvars) )
      allocate ( prange2(2,mvars) )
      allocate ( coords2(  mvars) )

       vname2(  1:nvars) = vname
      vtitle2(  1:nvars) = vtitle
      vunits2(  1:nvars) = vunits
       kmvar2(  1:nvars) = kmvar
      vrange2(:,1:nvars) = vrange
      prange2(:,1:nvars) = prange

      coords2            = ''
      coords2(  1:nvars) = coords

      if( root .and. mvars.gt.nvars ) print *
         mv=  nvars
      do nv=1,nquad
         if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
                     mv  = mv+1

           if( trim(quadratics(1,nv)).eq.trim(quadratics(2,nv)) ) then
              vname2(mv) = "Var_"         //  trim(vname(qloc(1,nv)))
             vtitle2(mv) = "Variance_of_" //  trim(vname(qloc(1,nv)))
           else
              vname2(mv) = "Cov_"            // trim(vname(qloc(1,nv))) // "_"     // trim(vname(qloc(2,nv)))
             vtitle2(mv) = "Covariance_of_"  // trim(vname(qloc(1,nv))) // "_and_" // trim(vname(qloc(2,nv)))
           endif

           if( trim(quadratics(3,nv)).ne.'XXX' ) vname2(mv) = trim(quadratics(3,nv))

               nstar = index( trim(quadratics(1,nv)),'star',back=.true. )
           if( nstar.ne.0 ) then
              lzstar(nv) = .TRUE.
             vtitle2(mv) = "Product_of_Zonal_Mean_Deviations_of_" 
     .                   // trim(vname(qloc(1,nv))) // "_and_" // trim(vname(qloc(2,nv)))
           endif

             vunits2(mv) = trim(vunits(qloc(1,nv))) // " " // trim(vunits(qloc(2,nv)))
             coords2(mv) = coords(qloc(1,nv))
              kmvar2(mv) =  kmvar(qloc(1,nv))
           vrange2(:,mv) = undef
           prange2(:,mv) = undef
         if( root ) write(6,7001) mv,trim(vname2(mv)),nloc(mv),trim(vtitle2(mv)),max(1,kmvar(qloc(1,nv))),qloc(1,nv),qloc(2,nv)
 7001    format(1x,'   Quad Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,a50,2x,i2,3x,i3,3x,i3)
         endif
      enddo

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
      deallocate ( coords )

      allocate(   qmin(nmax) )
      allocate(   qmax(nmax) )
      allocate(      q(im,jm,nmax,0:ntods) )
      allocate( ntimes(im,jm,nmax,0:ntods) )
                ntimes = 0
                     q = 0
                  qmin =  abs(undef)
                  qmax = -abs(undef)

      if( root ) then
      print *
      write(6,7002) mvars,nmax,im,jm,nmax,ntods
 7002 format(1x,'Total Number of Variables: ',i3,/
     .       1x,'Total Size: ',i5,/
     .       1x,'Allocating q(',i4,',',i3,',',i5,',0:',i2.2,')')
      print *
      print *, 'Files: '
      do n=1,nfiles
      print *, n,trim(fname(n))
      enddo
      print *
      if( ntod.eq.-999 ) then
      print *, 'Averging Time-Period NHMS: ',ntod,' (ALL Possible Time Periods Used)'
      else
      print *, 'Averging Time-Period NHMS: ',ntod
      endif
      if( begdate.ne.-999 .or. begtime.ne.-999 ) print *, 'Beginning Date for Averaging: ',begdate,begtime
      if( enddate.ne.-999 .or. endtime.ne.-999 ) print *, '   Ending Date for Averaging: ',enddate,endtime
      if( strict ) then
      print *, 'Every Time Period Required for Averaging, STRICT = ',strict
      else
      print *, 'Only Averaging Time Periods Supplied, STRICT = ',strict
      endif
      write(6,7003) ntcrit
 7003 format(1x,'Required Minimum Number of Defined Time Periods: ',i3,' (Otherwise, UNDEF)')
      print *
      endif

      call timeend(' initialize')

C **********************************************************************
C ****                      Read HDF Files                          ****
C **********************************************************************

      k = 0

      do n=1,nfiles
      if( root ) then
      call gfio_open ( trim(fname(n)),1,ID,rc )
      call gfio_diminquireCF ( id,imglobal,jmglobal,lm,ntime,nvars,ngatts,twoDimLat,rc )
      endif

#ifdef mpi
      call mpi_bcast ( imglobal,1,mpi_integer,0,comm,ierror )
      call mpi_bcast ( jmglobal,1,mpi_integer,0,comm,ierror )
      call mpi_bcast (       lm,1,mpi_integer,0,comm,ierror )
      call mpi_bcast (    ntime,1,mpi_integer,0,comm,ierror )
      call mpi_bcast (    nvars,1,mpi_integer,0,comm,ierror )
#endif

      allocate ( lon(imglobal)   )
      allocate ( lat(jmglobal)   )
      allocate ( lev(lm)         )
      allocate ( yymmdd(  ntime) )
      allocate ( hhmmss(  ntime) )
      allocate (  vname(  nvars) )
      allocate ( vtitle(  nvars) )
      allocate ( vunits(  nvars) )
      allocate (  kmvar(  nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
      allocate ( coords(  nvars) )

      if( root ) then
      call gfio_inquireCF ( id,imglobal,jmglobal,lm,ntime,nvars,
     .                      title,source,contact,undef,
     .                      lon,lat,lev,levunits,
     .                      yymmdd,hhmmss,timinc,
     .                      vname,vtitle,vunits,kmvar,
     .                      vrange,prange,coords,twoDimLat, lat2, lon2, rc )
      endif

#ifdef mpi
      call mpi_bcast ( yymmdd,ntime,mpi_integer,0,comm,ierror )
      call mpi_bcast ( hhmmss,ntime,mpi_integer,0,comm,ierror )
      call mpi_bcast ( timinc,1,    mpi_integer,0,comm,ierror )
      call mpi_bcast ( kmvar,nvars, mpi_integer,0,comm,ierror )
#endif

           do m=1,ntime
              nymd = yymmdd(m)
              nhms = hhmmss(m)
              if( nhms<0 ) then
                  nhms = nhmsf( nsecf(nhms) + 86400 )
                  call tick (nymd,nhms,-86400)
              endif

              if( ( begdate.ne.-999 .and. begtime.ne.-999 ) .and. 
     .            ( begdate.gt.nymd .or. 
     .            ( begdate.eq.nymd.and.begtime.gt.nhms ) ) ) cycle

              if( ( enddate.ne.-999 .and. endtime.ne.-999 ) .and. 
     .            ( enddate.lt.nymd .or. 
     .            ( enddate.eq.nymd.and.endtime.lt.nhms ) ) ) cycle

                               k = k+1
              if( k.gt.ntods ) k = 1
              if( ntod.eq.-999 .or. ntod.eq.nhms ) then
              if( root ) write(6,3000) nymd,nhms,timinc,trim(fname(n)),k
 3000         format(1x,'Reading nymd: ',i8.8,' nhms: ',i6.6,' TimInc: ',i6.6,'  from File: ',a,'  tod = ',i2)
              year  =     nymd/10000
              month = mod(nymd,10000)/100

c Check for Correct First Dataset
c -------------------------------
              if( strict .and. first ) then
                  nymdm = nymd
                  nhmsm = nhms
                  call tick (nymdm,nhmsm,-ndt)
                  yearm  =     nymdm/10000
                  monthm = mod(nymdm,10000)/100
                  if( year.eq.yearm .and. month.eq.monthm ) then
                      if( root ) print *, 'Date: ',nymd,' Time: ',nhms,' is NOT correct First Time Period!'
                      call my_finalize
                      stop 1
                  endif
              endif

c Check Date and Time for STRICT Time Testing
c -------------------------------------------
              if( strict .and. .not.first ) then
                  if( nymd.ne.nymdp .or. nhms.ne.nhmsp ) then
                      if( root ) print *, 'Date: ',nymdp,' Time: ',nhmsp,' not found!'
                      call my_finalize
                      stop 1
                  endif
              endif
              nymdp = nymd
              nhmsp = nhms

c Primary Fields
c --------------
              do nv=1,nvars2
                   call timebeg('   PRIME')
                   if( kmvar2(nv).eq.0 ) then
                            kbeg = 0
                            kend = 1
                   else
                            kbeg = 1
                            kend = kmvar2(nv)
                   endif
                   call timeend('   PRIME')

                   call mpi_gfio_getvar ( id,vname2(nv),nymd,nhms,im,jm,kbeg,kend,dum(1,1,nloc(nv)),lattice )

                   call timebeg('   PRIME')
                   do L=1,max(1,kmvar2(nv))
                   do j=1,jm
                   do i=1,im
                   if( defined(dum(i,j,nloc(nv)+L-1),undef) ) then
                            q(i,j,nloc(nv)+L-1,0) =      q(i,j,nloc(nv)+L-1,0) + dum(i,j,nloc(nv)+L-1)
                       ntimes(i,j,nloc(nv)+L-1,0) = ntimes(i,j,nloc(nv)+L-1,0) + 1
                       if(   qmin(nloc(nv)+L-1).gt.dum(i,j,nloc(nv)+L-1) ) qmin(nloc(nv)+L-1) = dum(i,j,nloc(nv)+L-1)
                       if(   qmax(nloc(nv)+L-1).lt.dum(i,j,nloc(nv)+L-1) ) qmax(nloc(nv)+L-1) = dum(i,j,nloc(nv)+L-1)
                       if( ntods.ne.0 ) then
                            q(i,j,nloc(nv)+L-1,k) =      q(i,j,nloc(nv)+L-1,k) + dum(i,j,nloc(nv)+L-1)
                       ntimes(i,j,nloc(nv)+L-1,k) = ntimes(i,j,nloc(nv)+L-1,k) + 1
                       endif
                   endif
                   enddo
                   enddo
                   enddo
                   call timeend('   PRIME')
              enddo

c Quadratics
c ----------
              call timebeg('   QUAD')
                 mv=  nvars2
              do nv=1,nquad
                 if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
                     mv=mv+1
                     do L=1,max(1,kmvar2(qloc(1,nv)))
                        if( lzstar(nv) ) then
                            call zstar (dum(1,1,nloc(qloc(1,nv))+L-1),dumz1,im,jm,undef,lattice)
                            call zstar (dum(1,1,nloc(qloc(2,nv))+L-1),dumz2,im,jm,undef,lattice)
                            do j=1,jm
                            do i=1,im
                            if( defined(dumz1(i,j),undef)  .and.
     .                          defined(dumz2(i,j),undef) ) then
                                     q(i,j,nloc(mv)+L-1,0) =      q(i,j,nloc(mv)+L-1,0) + dumz1(i,j)*dumz2(i,j)
                                ntimes(i,j,nloc(mv)+L-1,0) = ntimes(i,j,nloc(mv)+L-1,0) + 1
                                if( ntods.ne.0 ) then
                                     q(i,j,nloc(mv)+L-1,k) =      q(i,j,nloc(mv)+L-1,k) + dumz1(i,j)*dumz2(i,j)
                                ntimes(i,j,nloc(mv)+L-1,k) = ntimes(i,j,nloc(mv)+L-1,k) + 1
                                endif
                            endif
                            enddo
                            enddo
                        else
                            do j=1,jm
                            do i=1,im
                            if( defined(dum(i,j,nloc(qloc(1,nv))+L-1),undef)  .and.
     .                          defined(dum(i,j,nloc(qloc(2,nv))+L-1),undef) ) then
                                          q(i,j,nloc(mv)+L-1,0) = q(i,j,nloc(mv)+L-1,0) + dum(i,j,nloc(qloc(1,nv))+L-1)
     .                                                                                  * dum(i,j,nloc(qloc(2,nv))+L-1)
                                     ntimes(i,j,nloc(mv)+L-1,0) = ntimes(i,j,nloc(mv)+L-1,0) + 1
                                if( ntods.ne.0 ) then
                                          q(i,j,nloc(mv)+L-1,k) = q(i,j,nloc(mv)+L-1,k) + dum(i,j,nloc(qloc(1,nv))+L-1)
     .                                                                                  * dum(i,j,nloc(qloc(2,nv))+L-1)
                                     ntimes(i,j,nloc(mv)+L-1,k) = ntimes(i,j,nloc(mv)+L-1,k) + 1
                                endif
                            endif
                            enddo
                            enddo
                        endif
                     enddo
                 endif
              enddo
              call timeend('   QUAD')

              if( first ) then
                  nymd0 = nymd
                  nhms0 = nhms
                  first = .false.
              endif

c Update Date and Time for Strict Test
c ------------------------------------
              call tick (nymdp,nhmsp,ndt)
              yearp  =     nymdp/10000
              monthp = mod(nymdp,10000)/100

              endif ! End ntod  Test
           enddo    ! End ntime Loop within file

      if( root ) call gfio_close ( id,rc )
      if( n.ne.nfiles ) then
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
          deallocate ( coords )
      endif

      call my_barrier (comm)
      enddo

      do k=0,ntods
         if( k.eq.0 ) then
             nc = ntcrit
         else
             nc = max( 1,ntcrit/ntods )
         endif
      do n=1,nmax
      do j=1,jm
      do i=1,im
         if( ntimes(i,j,n,k).lt.nc ) then
                  q(i,j,n,k) = undef
         else
                  q(i,j,n,k) = q(i,j,n,k)/ntimes(i,j,n,k)
         endif
      enddo
      enddo
      enddo
      enddo

C **********************************************************************
C ****               Write HDF Monthly Output File                  ****
C **********************************************************************

      call timebeg(' Write_AVE')

c Check for Correct Last Dataset
c ------------------------------
      if( strict .and. ( year.eq.yearp .and. month.eq.monthp ) ) then
          if( root ) print *, 'Date: ',nymd,' Time: ',nhms,' is NOT correct Last Time Period!'
          call my_finalize
          stop 1
      endif

      write(date0,4000) nymd0/100
      write(time0,2000) nhms0/10000

      hdfile = trim(output) // "." // trim(date0) // "." // trim(ext)

 1000 format(i8.8)
 2000 format(i2.2)
 4000 format(i6.6)

      precision = 1 ! 64-bit
      precision = 0 ! 32-bit
      timeinc   = 060000
      if( root ) then
      call GFIO_CreateCF ( trim(hdfile), title, source, contact, undef,
     .                     imglobal, jmglobal, lm, lon, lat, lev, levunits,
     .                     nymd0, nhms0, timeinc,
     .                     mvars, vname2, vtitle2, vunits2, kmvar2,
     .                     vrange2, prange2, precision,
     .                     id, coords2, twoDimLat, lat2, lon2, rc )
      endif

c Primary Fields
c --------------
      if( root ) print *
      do n=1,nvars2
         if( kmvar2(n).eq.0 ) then
                  kbeg = 0
                  kend = 1
         else
                  kbeg = 1
                  kend = kmvar2(n)
         endif
         if( root ) write(6,3001) trim(vname2(n)),nloc(n),trim(hdfile)
 3001    format(1x,'Writing ',a,' at location ',i6,' into File: ',a)
         dum(:,:,1:kend) = q(:,:,nloc(n):nloc(n)+kend-1,0)
         call mpi_gfio_putVar ( id,trim(vname2(n)),nymd0,nhms0,im,jm,kbeg,kend,dum,lattice )
      enddo

c Quadratics
c ----------
         mv=  nvars2
      do nv=1,nquad
         if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
             mv=mv+1
             if( root ) write(6,3001) trim(vname2(mv)),nloc(mv),trim(hdfile)
             if( kmvar2(qloc(1,nv)).eq.0 ) then
                      kbeg = 0
                      kend = 1
             else
                      kbeg = 1
                      kend = kmvar2(qloc(1,nv))
             endif
             loc1 = nloc( qloc(1,nv) )
             loc2 = nloc( qloc(2,nv) )
             if( .not.lzstar(nv) ) then
                 where( q(:,:,nloc(mv):nloc(mv)+kend-1,0).ne.undef )
                      dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,0) - q(:,:,loc1:loc1+kend-1,0)
     .                                                                    * q(:,:,loc2:loc2+kend-1,0)
                 elsewhere
                      dum(:,:,1:kend) = undef
                 endwhere
             else
                      dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,0)
             endif
             call mpi_gfio_putVar ( id,trim(vname2(mv)),nymd0,nhms0,im,jm,kbeg,kend,dum,lattice )
         endif
      enddo

      if( root ) then
          call gfio_close ( id,rc )
          print * 
          print *, 'Created: ',trim(hdfile)
          print * 
      endif
      call timeend(' Write_AVE')

C **********************************************************************
C ****               Write HDF Monthly Diurnal Output File          ****
C **********************************************************************

      if( ntods.ne.0 ) then
      call timebeg(' Write_Diurnal')

      precision = 1 ! 64-bit
      precision = 0 ! 32-bit
      timeinc   = nhmsf( 86400/ntods )

      do k=1,ntods

      if( k.eq.1 .or. mdiurnal ) then

          write(date0,4000) nymd0/100
          write(time0,2000) nhms0/10000

          if(  diurnal ) hdfile = trim(doutput) // "." // trim(date0) // "." // trim(ext)
          if( mdiurnal ) hdfile = trim(doutput) // "." // trim(date0) // "_" // trim(time0) // "z." // trim(ext)

          if( ldquad ) then
              ndvars = mvars  ! Include Quadratics in Diurnal Files
          else
              ndvars = nvars2 ! Only Include Primary Fields in Diurnal Files (Default)
          endif
          if( root ) then
          call GFIO_CreateCF ( trim(hdfile), title, source, contact, undef,
     .                       imglobal, jmglobal, lm, lon, lat, lev, levunits,
     .                       nymd0, nhms0, timeinc,
     .                       ndvars, vname2(1:ndvars), vtitle2(1:ndvars), vunits2(1:ndvars), kmvar2(1:ndvars),
     .                       vrange2(:,1:ndvars), prange2(:,1:ndvars), precision,
     .                       id, coords2(1:ndvars), twoDimLat, lat2, lon2, rc )
          endif
      endif

c Primary Fields
c --------------
      do n=1,nvars2
         if( kmvar2(n).eq.0 ) then
                  kbeg = 0
                  kend = 1
         else
                  kbeg = 1
                  kend = kmvar2(n)
         endif
         dum(:,:,1:kend) = q(:,:,nloc(n):nloc(n)+kend-1,k)
         call mpi_gfio_putVar ( id,trim(vname2(n)),nymd0,nhms0,im,jm,kbeg,kend,dum,lattice )
      enddo

c Quadratics
c ----------
      if( ndvars.eq.mvars ) then
         mv=  nvars2
      do nv=1,nquad
         if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
             mv=mv+1
             if( kmvar2(qloc(1,nv)).eq.0 ) then
                      kbeg = 0
                      kend = 1
             else
                      kbeg = 1
                      kend = kmvar2(qloc(1,nv))
             endif
             loc1 = nloc( qloc(1,nv) )
             loc2 = nloc( qloc(2,nv) )
             if( .not.lzstar(nv) ) then
                 where( q(:,:,nloc(mv):nloc(mv)+kend-1,0).ne.undef )
                      dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,k) - q(:,:,loc1:loc1+kend-1,k)
     .                                                                    * q(:,:,loc2:loc2+kend-1,k)
                 elsewhere
                      dum(:,:,1:kend) = undef
                 endwhere
             else
                      dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,k)
             endif
             call mpi_gfio_putVar ( id,trim(vname2(mv)),nymd0,nhms0,im,jm,kbeg,kend,dum,lattice )
         endif
      enddo
      endif

      if( root .and. mdiurnal ) then
          call gfio_close ( id,rc )
          print *, 'Created: ',trim(hdfile)
      endif
      call tick (nymd0,nhms0,ndt)
      enddo

      if( root .and. diurnal ) then
          call gfio_close ( id,rc )
          print *, 'Created: ',trim(hdfile)
      endif
      if( root ) print * 

      call timeend(' Write_Diurnal')
      endif

C **********************************************************************
C ****                 Write Min/Max Information                    ****
C **********************************************************************

      if( root ) print *
      do n=1,nvars2
         do L=1,max(1,kmvar2(n))
         if( kmvar2(n).eq.0 ) then
                  plev = 0
         else
                  plev = lev(L)
         endif

#ifdef mpi
         call mpi_reduce( qmin(nloc(n)+L-1),qming,1,mpi_real,mpi_min,0,comm,ierror )
         call mpi_reduce( qmax(nloc(n)+L-1),qmaxg,1,mpi_real,mpi_max,0,comm,ierror )
#else
         qming = qmin(nloc(n)+L-1)
         qmaxg = qmax(nloc(n)+L-1)
#endif
         if( root ) then
         if(L.eq.1) then
            write(6,3101) trim(vname2(n)),plev,qming,qmaxg
         else
            write(6,3102) trim(vname2(n)),plev,qming,qmaxg
         endif
         endif
 3101    format(1x,'Primary Field: ',a20,' Level: ',f9.3,'  Min: ',g15.8,'  Max: ',g15.8)
 3102    format(1x,'               ',a20,' Level: ',f9.3,'  Min: ',g15.8,'  Max: ',g15.8)
         enddo
      call my_barrier (comm)
      if( root ) print *
      enddo
      if( root ) print *

C **********************************************************************
C ****                     Timing Information                       ****
C **********************************************************************

                 call timeend ('main')
      if( root ) call timepri (6)

      call my_finalize
      stop
      end

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*abs(undef)
      return
      end

      subroutine mpi_gfio_getvar ( id,name,nymd,nhms,im,jm,lbeg,lm,q,lattice )
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
      call timebeg ('  GetVar')
      if( lattice%myid.eq.0 ) then
          call gfio_getvar ( id,trim(name),nymd,nhms,img,jmg,lbeg,lm,glo,rc )
      endif
      call timeend ('  GetVar')
      call timebeg ('   Scatter')
      do L=1,lm
      call scatter_2d ( glo(1,1,L),q(1,1,L),lattice )
      enddo
      call timeend ('   Scatter')
      deallocate ( glo )
      return
      end

      subroutine mpi_gfio_putvar ( id,name,nymd,nhms,im,jm,lbeg,lm,q,lattice )
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
      call timebeg ('  PutVar')
      if( lattice%myid.eq.0 ) then
          call gfio_putvar ( id,trim(name),nymd,nhms,img,jmg,lbeg,lm,glo,rc )
      endif
      call timeend ('  PutVar')
      deallocate ( glo )
      return
      end

      subroutine zstar (q,qp,im,jm,undef,lattice)
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer im,jm,i,j,n
      real  q(im,jm),undef,qz(jm)
      real qp(im,jm)
      logical defined
      call zmean ( q,qz,im,jm,undef,lattice )
      do j=1,jm
           if( qz(j).eq. undef ) then
               qp(:,j) = undef
           else
               do i=1,im
                  if( defined( q(i,j),undef) ) then
                              qp(i,j) = q(i,j) - qz(j)
                  else
                              qp(i,j) = undef
                  endif
               enddo
           endif
      enddo
      return
      end

      subroutine check_quad ( quad,vname,nloc,nvars,aliases,nalias,qloc )
      implicit none
      character*256  quad(2), aliases(2,nalias), vname(nvars)
      integer  nvars, nalias, qloc(2), nloc(nvars)
      integer  m,n

c Initialize Location of Quadratics
c ---------------------------------
      qloc = 0

c Check Quadratic Name against HDF Variable Names
c -----------------------------------------------
      do n=1,nvars
             if( trim(vname(n)).eq.trim(quad(1)) ) qloc(1) = n
             if( trim(vname(n)).eq.trim(quad(2)) ) qloc(2) = n
      enddo

c Check Quadratic Name against Aliases
c ------------------------------------
      do m=1,nalias
         if( trim(quad(1)).eq.trim(aliases(1,m)) ) then
             do n=1,nvars
             if( trim(vname(n)).eq.trim(quad(1)) .or.
     .           trim(vname(n)).eq.trim(aliases(2,m)) ) then
             qloc(1) = n
             exit
             endif
             enddo
         endif
         if( trim(quad(2)).eq.trim(aliases(1,m)) ) then
             do n=1,nvars
             if( trim(vname(n)).eq.trim(quad(2)) .or.
     .           trim(vname(n)).eq.trim(aliases(2,m)) ) then
             qloc(2) = n
             exit
             endif
             enddo
         endif
      enddo
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

      subroutine usage(root)
      integer ierror
      logical root
      if(root) then
      write(6,100)
 100  format(  "Usage:  ",/,/
     .         " time_ave.x -hdf      Filenames (in HDF format)",/
     .         "           <-template TEMPLATE>"    ,/
     .         "           <-tag      TAG>"    ,/
     .         "           <-rc       RCFILE>" ,/
     .         "           <-ntod     NTOD>"   ,/
     .         "           <-ntmin    NTMIN>"  ,/
     .         "           <-strict   STRICT>" ,/
     .         "           <-d>"               ,/
     .         "           <-md>"              ,/,/
     .         "where:",/,/
     .         "  -hdf      Filenames:  Filenames (in HDF format) to average",/
     .         "  -template  TEMPLATE:  Filename to use as template if HDF files differ (default: 1st Filename)",/
     .         "  -begdate   yyyymmdd:  Optional Parameter for Date to Begin Averaging",/
     .         "  -begtime     hhmmss:  Optional Parameter for Time to Begin Averaging",/
     .         "  -enddate   yyyymmdd:  Optional Parameter for Date to   End Averaging",/
     .         "  -endtime     hhmmss:  Optional Parameter for Time to   End Averaging",/
     .         "  -tag            TAG:  Optional TAG for Output File (default: monthly_ave)",/
     .         "  -rc          RCFILE:  Optional Resource Filename for Quadratics (default: no quadratics)",/
     .         "  -ntod          NTOD:  Optional Time-Of-Day (HHMMSS) to average (default: all time periods)",/
     .         "  -ntmin        NTMIN:  Optional Parameter for Required Min. TimePeriods (default: 10 days equiv)",/
     .         "  -strict      STRICT:  Optional Logical Parameter for Strict Time Testing (default: .true.)",/
     .         "  -d             DTAG:  Optional Parameter to Create & Tag          Monthly Mean Diurnal File  ",
     .                                  "(all times included)",/
     .         "  -md            DTAG:  Optional Parameter to Create & Tag Multiple Monthly Mean Diurnal Files ",
     .                                  "(one time  per file)",/
     .         "  -dv            DTAG:  Like -d  but includes Diurnal Variances",/
     .         "  -mdv           DTAG:  Like -md but includes Diurnal variances",/
     .         )
      endif
      call my_finalize
      stop
      end

