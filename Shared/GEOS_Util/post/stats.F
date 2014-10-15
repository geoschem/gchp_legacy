      program stats

      use ESMF
      implicit none

      type(ESMF_Config) :: config

      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields

      type collection
           integer                    ::  n2d
           integer                    ::  n3d
           character*256              ::  name
           type(fields),  allocatable :: fields_2d(:)
           type(fields),  allocatable :: fields_3d(:)
      endtype collection

      type(fields),       pointer     ::   fields_2d(:)
      type(fields),       pointer     ::   fields_3d(:)
      type(collection),   target, allocatable :: collections(:)

      integer im,jm,nr,nl
      parameter ( im = 144 )        ! x-dimension for unified comparison
      parameter ( jm = 91  )        ! y-dimension for unified comparison
      parameter ( nr = 10  )        ! r-dimension (number of geographic regions )
      parameter ( nl = 10  )        ! l-dimension (number of upper-air levels )

      integer,parameter :: luout = 90 ! output file unit for formatted datafile w/ results

      integer        rc, fcst_file, iii
      logical        gmaopy,failed
      character*256  record
      character*20   cstnm,cdate,cstat,czlev,cstep,ceast,cwest,cnorth,csouth
      character*256  fcsource,averify

      real  f(im,jm), fvar(im,jm), varf(im,jm), fstar(im,jm), fsprime(im,jm)    ! forecast    variables
      real  a(im,jm), avar(im,jm), vara(im,jm), astar(im,jm), asprime(im,jm)    ! analysis    variables
      real  c(im,jm), cvar(im,jm), varc(im,jm), cstar(im,jm)                    ! climatology variables

      real, allocatable ::  corr(:,:,:,:)      ! Note: Hardwired for 100 time periods (Max)
      real, allocatable ::   rms(:,:,:,:,:)    ! Note: Hardwired for 100 time periods (Max)
      real*4 dum(nr)

      real    zlev(nl)
      data    zlev / 1000.0, 850.0, 700.0, 500.0, 400.0, 
     .                300.0, 250.0, 200.0, 150.0, 100.0 / 
      real    pref

      real  zlat1(nr),  lat1
      real  zlat2(nr),  lat2
      real  zlon1(nr),  lon1
      real  zlon2(nr),  lon2

      data zlat1  / -90.,  20., -20., -80.,   0.,  0., -90.,-90.,  20.,  30. /                  
      data zlat2  /  90.,  80.,  20., -20.,  90., 90.,   0.,  0.,  60.,  60. /                  
      data zlon1  /-180.,-180.,-180.,-180.,-180.,  0.,-180.,  0.,-140., -10. /                  
      data zlon2  / 180., 180., 180., 180.,   0.,180.,   0.,180.,- 60.,  30. /                  

      character(len=7) region(nr)
      data region / 'global ', 'n.hem  ','tropics','s.hem  ',
     .              'nw.quad', 'ne.quad','sw.quad','se.quad',
     .              'america', 'europe ' /

      character*1    char
      character*256  name, suffix
      character*256, allocatable :: qname(:)
      character*256, allocatable :: qdesc(:)

      character*256                  tag,expid
      character*256                  dummy
      character*256, allocatable ::    arg(:)
      character*256, allocatable :: name2d(:)
      character*256, allocatable :: name3d(:)
      character*256, allocatable ::  fname(:)
      character*256, allocatable ::  aname(:)
      character*256, allocatable ::  cname(:)
      character*256                  ename  ! Systematic Error File
      integer nfiles
      integer num_fcst_files
      integer num_clm_files
      integer num_ana_files
      integer len,n2d,n3d,nc,ncoll
      integer i,j,k,L,m,nsecf,nymd,nhms
      integer iregion,ibeg,iend,jbeg,jend
      integer ntime,nfield,nlev,lev,level,lmax

      real     undef
      real    fundef
      real    eundef
      real    aundef
      real    cundef
      integer fcst_im,fcst_jm,nargs
      integer n,nt,time,ntimes,nvars
      integer       yymmdd(100)
      integer       hhmmss(100)
      character*256  vname(500)
      logical check_names
      logical printout
      logical defined
      logical syserr
      data    syserr /.false./

      real    rho,d1,d2,d3, pi,dl,dp
      real    r1,r2,r3
      real    c1,c2,c3
      real    fmean, amean, cmean, area,phi,cosp
      integer nymdb, nhmsb, nymdc
      integer nymde, nhmse, nhmsc
      integer month, day, year, hour, minute, fhour, timinc, nfreq, ndt0, ndt, num

      character*7   xdate
      character*9   bdate, edate
      character*4   bhour, ehour
      character*256 rcfile  
      character*256 outfile  
      character*256 statfile
      character*256 datafile
      character*256 ctlfile1
      character*256 ctlfile2
      character*256 ctlfile3
      character*3   months(12)
      data months /'JAN','FEB','MAR','APR','MAY','JUN',
     .             'JUL','AUG','SEP','OCT','NOV','DEC'/

      integer  ndates
      integer  dates(3,1000)
      integer  iargc

! Default Aliases
! ---------------
      character*256 descr_p, alias_p (2)
      character*256 descr_u, alias_u (4)
      character*256 descr_v, alias_v (4)
      character*256 descr_t, alias_t (4)
      character*256 descr_q, alias_q (3)
      character*256 descr_h, alias_h (4)

      data alias_p  /'slp','slprs'/
      data alias_u  /'u','uwnd','ugrd','ugrdprs'/
      data alias_v  /'v','vwnd','vgrd','vgrdprs'/
      data alias_t  /'t','tmpu','tmp' ,'tmpprs' /
      data alias_q  /'q','sphu','qv' /
      data alias_h  /'h','hght','hgt' ,'hgtprs' /

      data descr_p  /'Sea Level Pressure'/
      data descr_u  /'Zonal U-Wind'/
      data descr_v  /'Meridional V-Wind'/
      data descr_t  /'Temperature'/
      data descr_q  /'Specific Humidity'/
      data descr_h  /'Height'/

! **********************************************************************
! ****                       Initialization                         ****
! **********************************************************************
                                                                                                          
      call timebeg ('main')
      call timebeg ('_init')
      call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, rc=rc)

      pref   =  500
      nfreq  = -999
      fhour  =  120
      expid  = ""
      rcfile = "NULL"
      outfile ="NULL"
      fcsource="NULL"
      averify ="NULL"
 
          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
                                                                                                          
             if( trim(arg(n)).eq.'-fcst' ) then
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
                 num_fcst_files = nfiles
             endif

             if( trim(arg(n)).eq.'-ana' ) then
                 nfiles = 1
                 read(arg(n+nfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+nfiles.ne.nargs )
                 nfiles = nfiles+1
                 read(arg(n+nfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) nfiles = nfiles-1
                 allocate ( aname(nfiles) )
                 do m=1,nfiles
                 aname(m) = arg(n+m)
                 enddo
                 num_ana_files = nfiles
             endif

             if( trim(arg(n)).eq.'-cli' ) then
                 nfiles = 1
                 read(arg(n+nfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+nfiles.ne.nargs )
                 nfiles = nfiles+1
                 read(arg(n+nfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) nfiles = nfiles-1
                 allocate ( cname(nfiles) )
                 do m=1,nfiles
                 cname(m) = arg(n+m)
                 enddo
                 num_clm_files = nfiles
             endif

             if( trim(arg(n)).eq.'-syserr' ) then
                                             ename  = trim(arg(n+1))
                                             syserr = .true.
             endif
             if( trim(arg(n)).eq.'-tag'    ) expid = trim(arg(n+1))
             if( trim(arg(n)).eq.'-fhour'  ) read(arg(n+1),*) fhour
             if( trim(arg(n)).eq.'-nfreq'  ) read(arg(n+1),*) nfreq
             if( trim(arg(n)).eq.'-pref'   ) read(arg(n+1),*) pref
             if( trim(arg(n)).eq.'-rc'     ) read(arg(n+1),fmt='(a)') rcfile
             if( trim(arg(n)).eq.'-o'      ) read(arg(n+1),fmt='(a)') outfile
             if( trim(arg(n)).eq.'-verif'  ) read(arg(n+1),*) averify
             if( trim(arg(n)).eq.'-fcsrc'  ) read(arg(n+1),*) fcsource

           enddo
      endif

      print *
      print *, 'Computing Stats for Forecast File(s):'
      print *, '-------------------------------------'
      do n=1,num_fcst_files
      write(6,1001) trim(fname(n))
      enddo
 1001 format(1x,a)
      print *

      print *, 'Verifying Analysis File(s):'
      print *, '---------------------------'
      do n=1,num_ana_files
      write(6,1001) trim(aname(n))
      enddo
      print *

      print *, 'Climatology File(s):'
      print *, '-------------------'
      do n=1,num_clm_files
      write(6,1001) trim(cname(n))
      enddo
      print *

      if( syserr ) then
      print *, 'Systematic Error File:'
      print *, '----------------------'
          write(6,1001) trim(ename)
      print *
      endif

      if( rcfile /= "NULL" ) then
      print *, 'Stats RC File: ',trim(rcfile)
      print *, '-------------- '
      print *
      endif

!     Sanity check
!     ------------
      gmaopy=.false.
      if( trim(outfile) /= "NULL" ) then
          failed=.false.
          if(trim(averify) =="NULL") failed=.true.
          if(trim(fcsource)=="NULL") failed=.true.
          if(failed) then
             print* ,' ERROR: when GMAOpy output requested the following must be specified:'
             print* ,'        -fcsrc FORECAST     (e.g., gmao)'
             print* ,'        -verif VERIFICATION (e.g., ncep)'
             print* ,' Aborting ...'
             call exit(1)
          endif
          gmaopy=.true.
      endif

! **********************************************************************
! ****                  Set Default FIELD Variables                 ****
! **********************************************************************

      if( rcfile == 'NULL' ) then

          ncoll = 1
          allocate ( collections(1) )
                     collections(1)%name = 'default'

                         n2d = 1
          collections(1)%n2d = n2d
          allocate ( collections(1)%fields_2d(1) )
          allocate ( collections(1)%fields_2d(1)%anal(im,jm,1) )
          allocate ( collections(1)%fields_2d(1)%fcst(im,jm,1) )
          allocate ( collections(1)%fields_2d(1)%clim(im,jm,1) )
          allocate ( collections(1)%fields_2d(1)%serr(im,jm,1) )
          allocate ( collections(1)%fields_2d(1)%alias(2) )
                     collections(1)%fields_2d(1)%name = 'p'
                     collections(1)%fields_2d(1)%msgn =  0
                     collections(1)%fields_2d(1)%type = 'scalar'
                     collections(1)%fields_2d(1)%alias = alias_p
                     collections(1)%fields_2d(1)%desc  = descr_p

                         n3d = 5
          collections(1)%n3d = n3d
          allocate( collections(1)%fields_3d(5) )
          do n=1,n3d
          allocate ( collections(1)%fields_3d(n)%anal(im,jm,nl) )
          allocate ( collections(1)%fields_3d(n)%fcst(im,jm,nl) )
          allocate ( collections(1)%fields_3d(n)%clim(im,jm,nl) )
          allocate ( collections(1)%fields_3d(n)%serr(im,jm,nl) )
          enddo
          allocate ( collections(1)%fields_3d(1)%alias(4) )
                     collections(1)%fields_3d(1)%name = 'u'
                     collections(1)%fields_3d(1)%msgn =  1
                     collections(1)%fields_3d(1)%type = 'vector'
                     collections(1)%fields_3d(1)%alias = alias_u
                     collections(1)%fields_3d(1)%desc  = descr_u
          allocate ( collections(1)%fields_3d(2)%alias(4) )
                     collections(1)%fields_3d(2)%name = 'v'
                     collections(1)%fields_3d(2)%msgn =  1
                     collections(1)%fields_3d(2)%type = 'vector'
                     collections(1)%fields_3d(2)%alias = alias_v
                     collections(1)%fields_3d(2)%desc  = descr_v
          allocate ( collections(1)%fields_3d(3)%alias(4) )
                     collections(1)%fields_3d(3)%name = 't'
                     collections(1)%fields_3d(3)%msgn =  0
                     collections(1)%fields_3d(3)%type = 'scalar'
                     collections(1)%fields_3d(3)%alias = alias_t
                     collections(1)%fields_3d(3)%desc  = descr_t
          allocate ( collections(1)%fields_3d(4)%alias(3) )
                     collections(1)%fields_3d(4)%name = 'q'
                     collections(1)%fields_3d(4)%msgn =  0
                     collections(1)%fields_3d(4)%type = 'scalar'
                     collections(1)%fields_3d(4)%alias = alias_q
                     collections(1)%fields_3d(4)%desc  = descr_q
          allocate ( collections(1)%fields_3d(5)%alias(4) )
                     collections(1)%fields_3d(5)%name = 'h'
                     collections(1)%fields_3d(5)%msgn =  0
                     collections(1)%fields_3d(5)%type = 'scalar'
                     collections(1)%fields_3d(5)%alias = alias_h
                     collections(1)%fields_3d(5)%desc  = descr_h

      else

! **********************************************************************
! ****              Load Variables from CONFIG RC File              ****
! **********************************************************************
                                                                                                          
      config = ESMF_ConfigCreate   ( rc=rc )
        call   ESMF_ConfigLoadFile ( config, trim(rcfile),  rc=rc )

        call ESMF_ConfigFindLabel      ( config, LABEL='COLLECTIONS:',  rc=rc )
        if( rc == ESMF_SUCCESS ) then
            ncoll = ESMF_ConfigGetLen  ( config, LABEL='COLLECTIONS:',  rc=rc )
            allocate( collections(ncoll) )
            call ESMF_ConfigFindLabel  ( config, LABEL='COLLECTIONS:',  rc=rc )
            do m=1,ncoll
               call ESMF_ConfigGetAttribute( config, collections(m)%name,  rc=rc )
            enddo

            do m=1,ncoll
            call ESMF_ConfigFindLabel      ( config, trim(collections(m)%name)//'.fields_2d:',  rc=rc )
            if( rc == ESMF_SUCCESS ) then
                n2d = ESMF_ConfigGetLen    ( config, LABEL=trim(collections(m)%name)//'.fields_2d:',  rc=rc )
                collections(m)%n2d = n2d
                allocate( collections(m)%fields_2d(n2d) )
                call  ESMF_ConfigFindLabel ( config, trim(collections(m)%name)//'.fields_2d:',  rc=rc )
                do n=1,n2d
                call ESMF_ConfigGetAttribute( config, collections(m)%fields_2d(n)%name,  rc=rc )
                allocate ( collections(m)%fields_2d(n)%anal(im,jm,1) )
                allocate ( collections(m)%fields_2d(n)%fcst(im,jm,1) )
                allocate ( collections(m)%fields_2d(n)%clim(im,jm,1) )
                allocate ( collections(m)%fields_2d(n)%serr(im,jm,1) )
                enddo

                do n=1,n2d
                call  ESMF_ConfigFindLabel   ( config, 'TYPE_' // trim(collections(m)%fields_2d(n)%name) // ':',  rc=rc )
                call  ESMF_ConfigGetAttribute( config, collections(m)%fields_2d(n)%type,  rc=rc )
                                                       collections(m)%fields_2d(n)%desc = 'Unknown'
                if( check_names( collections(m)%fields_2d(n)%type,'aerosol' ) ) collections(m)%fields_2d(n)%msgn = 0
                if( check_names( collections(m)%fields_2d(n)%type,'scalar'  ) ) collections(m)%fields_2d(n)%msgn = 0
                if( check_names( collections(m)%fields_2d(n)%type,'vector'  ) ) collections(m)%fields_2d(n)%msgn = 1

                len = ESMF_ConfigGetLen    ( config, LABEL='ALIAS_' // trim(collections(m)%fields_2d(n)%name) // ':',  rc=rc )
                    if( rc == ESMF_SUCCESS ) then
                    allocate( collections(m)%fields_2d(n)%alias(len) )
                    call  ESMF_ConfigFindLabel ( config, 'ALIAS_' // trim(collections(m)%fields_2d(n)%name) // ':',  rc=rc )
                    do k=1,len
                    call ESMF_ConfigGetAttribute( config, collections(m)%fields_2d(n)%alias(k),  rc=rc )
                         if(  check_names( collections(m)%fields_2d(n)%alias(k),'slp' ) ) collections(m)%fields_2d(n)%desc = descr_p
                    enddo
                    endif
                enddo

             else
                collections(m)%n2d = 0
             endif

            call ESMF_ConfigFindLabel      ( config, trim(collections(m)%name)//'.fields_3d:',  rc=rc )
            if( rc == ESMF_SUCCESS ) then
                n3d = ESMF_ConfigGetLen    ( config, LABEL=trim(collections(m)%name)//'.fields_3d:',  rc=rc )
                collections(m)%n3d = n3d
                allocate( collections(m)%fields_3d(n3d) )
                call  ESMF_ConfigFindLabel ( config, trim(collections(m)%name)//'.fields_3d:',  rc=rc )
                do n=1,n3d
                call ESMF_ConfigGetAttribute( config, collections(m)%fields_3d(n)%name,  rc=rc )
                allocate ( collections(m)%fields_3d(n)%anal(im,jm,nl) )
                allocate ( collections(m)%fields_3d(n)%fcst(im,jm,nl) )
                allocate ( collections(m)%fields_3d(n)%clim(im,jm,nl) )
                allocate ( collections(m)%fields_3d(n)%serr(im,jm,nl) )
                enddo

                do n=1,n3d
                call  ESMF_ConfigFindLabel   ( config, 'TYPE_' // trim(collections(m)%fields_3d(n)%name) // ':',  rc=rc )
                call  ESMF_ConfigGetAttribute( config, collections(m)%fields_3d(n)%type,  rc=rc )
                                                                 collections(m)%fields_3d(n)%desc = 'Unknown'
                if( check_names( collections(m)%fields_3d(n)%type,'aerosol' ) ) collections(m)%fields_3d(n)%msgn = 0
                if( check_names( collections(m)%fields_3d(n)%type,'scalar'  ) ) collections(m)%fields_3d(n)%msgn = 0
                if( check_names( collections(m)%fields_3d(n)%type,'vector'  ) ) collections(m)%fields_3d(n)%msgn = 1

                len = ESMF_ConfigGetLen    ( config, LABEL='ALIAS_' // trim(collections(m)%fields_3d(n)%name) // ':',  rc=rc )
                    if( rc == ESMF_SUCCESS ) then
                    allocate( collections(m)%fields_3d(n)%alias(len) )
                    call  ESMF_ConfigFindLabel ( config, 'ALIAS_' // trim(collections(m)%fields_3d(n)%name) // ':',  rc=rc )
                    do k=1,len
                    call ESMF_ConfigGetAttribute( config, collections(m)%fields_3d(n)%alias(k),  rc=rc )
                         if(  check_names( collections(m)%fields_3d(n)%alias(k),'u' ) ) collections(m)%fields_3d(n)%desc = descr_u
                         if(  check_names( collections(m)%fields_3d(n)%alias(k),'v' ) ) collections(m)%fields_3d(n)%desc = descr_v
                         if(  check_names( collections(m)%fields_3d(n)%alias(k),'t' ) ) collections(m)%fields_3d(n)%desc = descr_t
                         if(  check_names( collections(m)%fields_3d(n)%alias(k),'q' ) ) collections(m)%fields_3d(n)%desc = descr_q
                         if(  check_names( collections(m)%fields_3d(n)%alias(k),'h' ) ) collections(m)%fields_3d(n)%desc = descr_h
                    enddo
                    endif
                enddo

             else
                collections(m)%n3d = 0
             endif

            enddo ! End Collection Loop
        endif     ! End Collection Test

      endif

! **********************************************************************
! ****                    Initialize Forecast Files                 ****
! **********************************************************************

      call timebeg ('__init_fcst')
      call init_fcst ( fname,num_fcst_files,dates,ndates,timinc,fundef,collections,ncoll )
      call timeend ('__init_fcst')

!     If so, open file to store scores for GMAOpy tool
!     ------------------------------------------------
      if (gmaopy) then
          open(luout,file=trim(outfile),form='formatted')
                              ! the following is in alphabetic order
          write(luout,'(5a)') 'count|date|domain_name|east|',
     .                        'expver|forecast|level|levtype|',
     .                        'north|variable|source|south|',
     .                        'statistic|step|type|value|',
     .                        'verify|west'
      endif

      call timeend ('_init')

! **********************************************************************
! ****              Compute STATS for Each Collection               ****
! **********************************************************************
                                                                                                          
      call timebeg ('_colls')

      do nc = 1,ncoll

         n2d = collections(nc)%n2d
         n3d = collections(nc)%n3d

         fields_2d => collections(nc)%fields_2d
         fields_3d => collections(nc)%fields_3d

         if( trim(expid).ne."" ) then
             if( trim(collections(nc)%name).ne.'default' ) then
                  tag = trim(expid) // "." // trim(collections(nc)%name) // "."
             else
                  tag = trim(expid) // "."
             endif
         else
             if( trim(collections(nc)%name).ne.'default' ) then
                  tag =                      trim(collections(nc)%name) // "."
             else
                  tag = ""
             endif
         endif
 
      undef  = 1.0e15
      nymdb  = dates(1,1)
      nhmsb  = dates(2,1)
      do n=1,ndates
      if( dates(1,n).lt.nymdb )                           nymdb = dates(1,n)
      if( dates(1,n).eq.nymdb .and. dates(2,n).lt.nhmsb ) nhmsb = dates(2,n)
      enddo
      nymde  = nymdb
      nhmse  = nhmsb
      call tick (nymde,nhmse,fhour*3600)

          ndt0 = nsecf(timinc)
      if( nfreq.eq.-999 ) then
          ndt  = nsecf(timinc)
      else
          ndt  = nsecf(nfreq)
      endif

      nymd = nymdb
      nhms = nhmsb

      write(bdate,3001) nymdb
      write(xdate,3003) nhmsb
      statfile = trim(tag) // 'globl.' // bdate // "." // xdate // ".data"
      open (51,file=trim(statfile),form='unformatted',access='sequential')

      print *
      print *, '           Begdate: ',nymdb,' ',nhmsb
      print *, '           Enddate: ',nymde,' ',nhmse
      print *, 'Forecast Frequency: ',ndt/3600,' (hrs)'
      print *

! **********************************************************************
! ****                Define variables and descriptions             ****
! **********************************************************************
                                                                                                          
      allocate(  corr(nr,nl,n2d+n3d,100)   )  ! Note: Hardwired for 100 time periods (Max)
      allocate(   rms(nr,nl,n2d+n3d,100,5) )  ! Note: Hardwired for 100 time periods (Max)

      allocate( qname(6*(n2d+n3d)) )
      allocate( qdesc(6*(n2d+n3d)) )

      do n=1,6
         if(n.eq.1) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'cor    '
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Anomaly Correlation'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'cor    '
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Anomaly Correlation'
         enddo
         endif

         if(n.eq.2) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'rms    '
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Root Mean Square Error'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'rms    '
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Root Mean Square Error'
         enddo
         endif

         if(n.eq.3) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'rms_ran'
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Root Mean Square Error Random'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'rms_ran'
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Root Mean Square Error Random'
         enddo
         endif

         if(n.eq.4) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'rms_bar'
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Root Mean Square Error Bias'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'rms_bar'
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Root Mean Square Error Bias'
         enddo
         endif

         if(n.eq.5) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'rms_dis'
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Root Mean Square Error Dissipation'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'rms_dis'
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Root Mean Square Error Dissipation'
         enddo
         endif

         if(n.eq.6) then
         do k=1,n2d
            qname(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%name) // 'rms_dsp'
            qdesc(k    +(n-1)*(n2d+n3d)) = trim(fields_2d(k)%desc) // ' Root Mean Square Error Dispersion'
         enddo
         do k=1,n3d
            qname(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%name) // 'rms_dsp'
            qdesc(k+n2d+(n-1)*(n2d+n3d)) = trim(fields_3d(k)%desc) // ' Root Mean Square Error Dispersion'
         enddo
         endif
      enddo

c ----------------------------------

      if( n2d.ne.0 .or. n3d.ne.0 ) then
          print *, 'COLLECTION: ',trim(collections(nc)%name)
          print *
      endif
      if( n2d.ne.0 ) then
          do n=1,n2d
          print *, 'FIELDS_2D: ',trim(fields_2d(n)%name) 
          print *, '     DESC: ',trim(fields_2d(n)%desc) 
          print *, '  ALIASES: ',((trim(fields_2d(n)%alias(k)),' '),k=1,size(fields_2d(n)%alias))
          print *
          enddo
      endif
      if( n3d.ne.0 ) then
          do n=1,n3d
          print *, 'FIELDS_3D: ',trim(fields_3d(n)%name)
          print *, '     DESC: ',trim(fields_3d(n)%desc) 
          print *, '  ALIASES: ',((trim(fields_3d(n)%alias(k)),' '),k=1,size(fields_3d(n)%alias))
          print *
          enddo
      endif
          print *

C **********************************************************************

      pi = 4.0*atan(1.0)
      dl = 2*pi/im
      dp = pi/(jm-1)

c Loop over Forecast Times
c ------------------------
      nt = 0
      do while( (nymd.lt.nymde) .or.
     .          (nymd.eq.nymde .and. nhms.le.nhmse) )

             nt = nt + 1
         if( nt.eq.1 ) then
             hour = 0
         else
             call interp_time ( nymdb,nhmsb, nymd,nhms, nt, num )
             hour = num*(nt-1)
         endif
         printout = .false.

c Read Forecast
c -------------
         call timebeg ('__read_fcst')
         call read_fcst( fname,num_fcst_files,dates,ndates,nymd,nhms,fields_2d,fields_3d,n2d,n3d,im,jm,nl,zlev,fundef )
         call timeend ('__read_fcst')

         nhms = 10000*(nhms/10000) ! Kludge to strip off minutes/seconds from Model Time

c Find Analysis and Climatology for Forecast Time: nymd,nhms
c ----------------------------------------------------------
      call timebeg ('__read_anal')
      call read_anal     ( nymd,nhms,fields_2d,fields_3d,n2d,n3d,im,jm,nl,zlev,aname,num_ana_files,aundef )
      call timeend ('__read_anal')

      call timebeg ('__read_clim')
      call read_clim_hdf ( nymd,nhms,fields_2d,fields_3d,n2d,n3d,im,jm,nl,zlev,cname,num_clm_files,cundef )
      call timeend ('__read_clim')

c Read Systematic Error File and Modify Forecast Fields
c -----------------------------------------------------
        if( syserr ) then
            call timebeg ('__read_serr')
            call read_syserr( fields_2d,fields_3d,n2d,n3d,im,jm,nl,zlev,ename,ndt,eundef )
            call timeend ('__read_serr')

            do n=1,n2d
            do j=1,jm
            do i=1,im
            if( defined( fields_2d(n)%fcst(i,j,1),fundef ) .and. 
     .          defined( fields_2d(n)%serr(i,j,1),eundef ) ) 
     .                   fields_2d(n)%fcst(i,j,1) = fields_2d(n)%fcst(i,j,1) - fields_2d(n)%serr(i,j,1)
            enddo
            enddo
            enddo

            do n=1,n3d
            do L=1,nl
            do j=1,jm
            do i=1,im
            if( defined( fields_3d(n)%fcst(i,j,L),fundef ) .and. 
     .          defined( fields_3d(n)%serr(i,j,L),eundef ) ) 
     .                   fields_3d(n)%fcst(i,j,L) = fields_3d(n)%fcst(i,j,L) - fields_3d(n)%serr(i,j,L)
            enddo
            enddo
            enddo
            enddo
        endif

! Write Global Data
! -----------------
        do n=1,n2d
        call writit ( fields_2d(n)%fcst,im,jm,1 ,fundef,undef )
        enddo
        do n=1,n3d
        call writit ( fields_3d(n)%fcst,im,jm,nl,fundef,undef )
        enddo

        do n=1,n2d
        call writit ( fields_2d(n)%anal,im,jm,1 ,aundef,undef )
        enddo
        do n=1,n3d
        call writit ( fields_3d(n)%anal,im,jm,nl,aundef,undef )
        enddo

        do n=1,n2d
        call writit ( fields_2d(n)%clim,im,jm,1 ,cundef,undef )
        enddo
        do n=1,n3d
        call writit ( fields_3d(n)%clim,im,jm,nl,cundef,undef )
        enddo


c Loop over Geographical Regions
c ------------------------------
      call timebeg ('__stats')
      do 1000 iregion = 1,nr
      lat1 = zlat1(iregion)
      lat2 = zlat2(iregion)
      lon1 = zlon1(iregion)
      lon2 = zlon2(iregion)

c Determine beginning and ending i&j for Region
c ---------------------------------------------
      call bounds (lat1,lat2,lon1,lon2,
     .             jbeg,jend,ibeg,iend,im,jm)


c Loop over Fields and Levels
c ---------------------------
      do 2000 nfield = 1,n2d+n3d

      if( nfield.le.n2d ) then
             n = nfield
          nlev = 1
          name = fields_2d(n)%name
      else
             n = nfield-n2d
          nlev = nl
          name = fields_3d(n)%name
      endif

      do 3000 lev = 1,nlev

      if( nfield.le.n2d ) then
          c(:,:) = fields_2d(n)%clim(:,:,1)
          f(:,:) = fields_2d(n)%fcst(:,:,1)
          a(:,:) = fields_2d(n)%anal(:,:,1)
      else
          c(:,:) = fields_3d(n)%clim(:,:,lev)
          f(:,:) = fields_3d(n)%fcst(:,:,lev)
          a(:,:) = fields_3d(n)%anal(:,:,lev)
      endif

c Compute Regional Area Means
c ---------------------------
      fmean = 0
      amean = 0
      cmean = 0
      area  = 0
      do j=jbeg,jend
      phi = -pi/2 + (j-1)*dp
      cosp = cos(phi)
      do i=ibeg,iend
         if( defined(f(i,j),fundef)   .and.
     .       defined(a(i,j),aundef)   .and.
     .       defined(c(i,j),cundef) )  then
         fmean = fmean + f(i,j)*cosp
         amean = amean + a(i,j)*cosp
         cmean = cmean + c(i,j)*cosp
         area  = area  +        cosp
         endif
      enddo
      enddo
      if( area.ne.0.0 ) then
          fmean = fmean / area
          amean = amean / area
          cmean = cmean / area
      else
          fmean = fundef
          amean = aundef
          cmean = cundef
      endif


c Define Deviations from Area Means
c ---------------------------------
      do j=jbeg,jend
      do i=ibeg,iend
      if( defined(f(i,j),fundef)   .and.
     .    defined(a(i,j),aundef)   .and.
     .    defined(c(i,j),cundef) )  then
              fstar(i,j) = f(i,j)-fmean
              astar(i,j) = a(i,j)-amean
              cstar(i,j) = c(i,j)-cmean
      else
              fstar(i,j) = fundef
              astar(i,j) = aundef
              cstar(i,j) = cundef
      endif
      enddo
      enddo


c Subtract Climatology Deviations and Compute Variances
c -----------------------------------------------------
      do j=jbeg,jend
      do i=ibeg,iend
      if( defined(fstar(i,j),fundef)   .and.
     .    defined(astar(i,j),aundef)   .and.
     .    defined(cstar(i,j),cundef) )  then
      fsprime(i,j) =   fstar(i,j)-  cstar(i,j)
      asprime(i,j) =   astar(i,j)-  cstar(i,j)
         fvar(i,j) = fsprime(i,j)*fsprime(i,j)
         avar(i,j) = asprime(i,j)*asprime(i,j)
         cvar(i,j) = fsprime(i,j)*asprime(i,j)
         varf(i,j) =   fstar(i,j)*  fstar(i,j)
         vara(i,j) =   astar(i,j)*  astar(i,j)
         varc(i,j) =   fstar(i,j)*  astar(i,j)
      else
      fsprime(i,j) = fundef
      asprime(i,j) = aundef
         fvar(i,j) = fundef
         avar(i,j) = aundef
         cvar(i,j) = cundef
         varf(i,j) = fundef
         vara(i,j) = aundef
         varc(i,j) = cundef
      endif
      enddo
      enddo


c Compute Regional Mean Variances and Anomaly Correlation
c -------------------------------------------------------
        c1 = 0
        c2 = 0
        c3 = 0
        d1 = 0
        d2 = 0
        d3 = 0
        r1 = 0
        r2 = 0
        r3 = 0
      area = 0
      do j=jbeg,jend
      phi = -pi/2 + (j-1)*dp
      cosp = cos(phi)
      do i=ibeg,iend
      if( defined(   f(i,j),fundef)   .and.
     .    defined(   a(i,j),aundef)   .and.
     .    defined(fvar(i,j),fundef)   .and.
     .    defined(avar(i,j),aundef)   .and.
     .    defined(cvar(i,j),cundef) ) then
        c1 = c1 + cvar(i,j) * cosp
        c2 = c2 + fvar(i,j) * cosp
        c3 = c3 + avar(i,j) * cosp
        d1 = d1 + varc(i,j) * cosp
        d2 = d2 + varf(i,j) * cosp
        d3 = d3 + vara(i,j) * cosp
        r1 = r1 + (    f(i,j)-    a(i,j))**2 * cosp
        r2 = r2 + (fstar(i,j)-astar(i,j))**2 * cosp
        r3 = r3 + (fmean     -amean     )**2 * cosp
      area = area  +          cosp
      endif
      enddo
      enddo
      if( area.ne.0 ) then
          c1 = c1 / area
          c2 = c2 / area
          c3 = c3 / area
          d1 = d1 / area
          d2 = d2 / area
          d3 = d3 / area
          r1 = r1 / area
          r2 = r2 / area
          r3 = r3 / area
      else
          c1 = undef
          c2 = undef
          c3 = undef
          d1 = undef
          d2 = undef
          d3 = undef
          r1 = undef
          r2 = undef
          r3 = undef
      endif

      if( c1.ne.undef ) then
      corr(iregion,lev,nfield,nt)   = c1/(sqrt(c2)*sqrt(c3))
       rms(iregion,lev,nfield,nt,1) = sqrt(r1)
       rms(iregion,lev,nfield,nt,2) = sqrt(r2)
       rms(iregion,lev,nfield,nt,3) = sqrt(r3)

                                rho = d1/(sqrt(d2)*sqrt(d3))
       rms(iregion,lev,nfield,nt,4) = (sqrt(d2)-sqrt(d3))**2 + (fmean-amean)**2
       rms(iregion,lev,nfield,nt,5) = 2*(1-rho)*sqrt(d2)*sqrt(d3)
      else
      corr(iregion,lev,nfield,nt)   = undef
       rms(iregion,lev,nfield,nt,1) = undef
       rms(iregion,lev,nfield,nt,2) = undef
       rms(iregion,lev,nfield,nt,3) = undef
       rms(iregion,lev,nfield,nt,4) = undef
       rms(iregion,lev,nfield,nt,5) = undef
      endif

      if( n3d.gt.0 .and. nfield.gt.n2d ) then
          if( trim(fields_3d(n)%name).eq.'h' .and. iregion.eq.2 .and. zlev(lev).eq.pref ) then
              write(6,1003) int(pref),nymd,nhms,hour,corr(iregion,lev,nfield,nt),fmean,amean,cmean
              printout = .true.
          endif
      endif

          ! if requested, write out stats info for GMAOpy
          ! ---------------------------------------------
          if (gmaopy) then
            do iii=0,5
              if (iii==0) then
                 cstnm = 'cor'
                 write( cstat,*) corr(iregion,lev,nfield,nt)
              endif 
              if (iii==1) then
                 cstnm = 'rms'
                 write( cstat,*) rms (iregion,lev,nfield,nt,iii)
              endif 
              if (iii==2) then
                 cstnm = 'rms_ran'
                 write( cstat,*) rms (iregion,lev,nfield,nt,iii)
              endif 
              if (iii==3) then
                 cstnm = 'rms_bar'
                 write( cstat,*) rms (iregion,lev,nfield,nt,iii)
              endif 
              if (iii==4) then
                 cstnm = 'rms_dis'
                 write( cstat,*) rms (iregion,lev,nfield,nt,iii)
              endif 
              if (iii==5) then
                 cstnm = 'rms_dsp'
                 write( cstat,*) rms (iregion,lev,nfield,nt,iii)
              endif 
              write( cdate,'(i8.8,i2.2)') nymdb, nhmsb/10000
              write( czlev,*) zlev(lev)
              write( cstep,*) hour
              write( ceast,*) zlon2(iregion)
              write( cwest,*) zlon1(iregion)
              write(cnorth,*) zlat2(iregion)
              write(csouth,*) zlat1(iregion)
              write(record,'(f3.1,34a)') 0.0, '|',  ! count (dummy for this code)
     .                   trim(adjustl(cdate)),'|',  ! date
     .         trim(adjustl(region(iregion))),'|',  ! domain_name
     .                   trim(adjustl(ceast)),'|',  ! east
     .                   trim(adjustl(expid)),'|',  ! expver
     .                trim(adjustl(fcsource)),'|',  ! forecast
     .                   trim(adjustl(czlev)),'|',  ! level
     .                                   'pl','|',  ! levtype
     .                  trim(adjustl(cnorth)),'|',  ! north
     .                    trim(adjustl(name)),'|',  ! variable
     .                trim(adjustl(fcsource)),'|',  ! source=forecast
     .                  trim(adjustl(csouth)),'|',  ! south
     .                   trim(adjustl(cstnm)),'|',  ! statistic
     .                   trim(adjustl(cstep)),'|',  ! step
     .                                   'fc','|',  ! type
     .                   trim(adjustl(cstat)),'|',  ! value
     .                 trim(adjustl(averify)),'|',  ! verify
     .                   trim(adjustl(cwest))       ! west
              write(luout,'(a)') trim(adjustl(record))
            enddo ! loop over all available stats (this is wired-in)
          endif

 3000 continue  ! end level  loop
 2000 continue  ! end field  loop
 1000 continue  ! end region loop
      call timeend ('__stats')

      if( .not.printout )  write(6,1004) nymd,nhms,hour

      call tick (nymd,nhms,ndt)
      if( hour.eq.fhour ) exit
      enddo     ! end        loop for all     forecast files
      print *

c Write out correlation and rms data
c ----------------------------------
      year   =     nymdb/10000
      month  = mod(nymdb,10000)/100
      day    = mod(nymdb,100)
      hour   =     nhmsb/10000
      minute = mod(nhmsb,10000)/100

      write(bdate,3001) nymdb
      write(bhour,3004) nhmsb/10000
      write(edate,3002) nymde
      write(ehour,3004) nhmse/10000
 3001 format('b',i8.8)
 3002 format('e',i8.8)
 3003 format('x',i6.6)
 3004 format('_',i2.2,'z')

      close(51)
      datafile = trim(tag) // 'globl.' // bdate // bhour // "." // edate // ehour // ".data"
      call system ("/bin/mv " // trim(statfile) // " " // trim(datafile) )

      statfile = trim(tag) // 'stats.' // bdate // bhour // "." // edate // ehour // ".data"
      open (85,file=trim(statfile),form='unformatted',access='sequential')

      ctlfile1 = trim(tag) // 'stats.' // bdate // bhour // "." // edate // ehour // ".ctl1"
      open (86,file=trim(ctlfile1),form='formatted'  ,access='sequential')

      ctlfile2 = trim(tag) // 'stats.' // bdate // bhour // "." // edate // ehour // ".ctl2"
      open (87,file=trim(ctlfile2),form='formatted'  ,access='sequential')

      ctlfile3 = trim(tag) // 'globl.' // bdate // bhour // "." // edate // ehour // ".ctl"
      open (88,file=trim(ctlfile3),form='formatted'  ,access='sequential')

c -----------------------------------------------

      do ntime =1,nt

      do nfield=1,n2d+n3d
         if( nfield.le.n2d ) then
             nlev = 1
         else
             nlev = nl
         endif

         do level=1,nlev
         do i=1,nr
         dum(i) = corr(i,level,nfield,ntime)
         enddo
         write(85) dum
         enddo
      enddo

      do m=1,5
      do nfield=1,n2d+n3d
         if( nfield.le.n2d ) then
             nlev = 1
         else
             nlev = nl
         endif
         do level=1,nlev
         do i=1,nr
         dum(i) = rms(i,level,nfield,ntime,m)
         enddo
         write(85) dum
         enddo
      enddo
      enddo

      enddo

c Write out correlation tabl ctl1 (uniform times)
c -----------------------------------------------
      call interp_time ( nymdb,nhmsb, nymde,nhmse, nt, num )

      write(86,5001) trim(statfile)
      write(86,5002) 
      write(86,5003) undef
      write(86,5004) nr
      write(86,5005) 
      write(86,5006) nl,(zlev(k),k=1,nl)
      write(86,5007) nt,                months(month),year, num
      write(86,5008) 6*(n2d+n3d) ! Note: 6 Types of Statistical Output per Field (cor, rms, rms_bar, rms_ran, rms_dis, rms_dsp)
      do k=1,6
      do n=1,n2d+n3d
         m=n+(k-1)*(n2d+n3d)
         if( n.le.n2d ) then
             lmax = 0
         else
             lmax = nl
         endif
      write(86,5009) trim(qname(m)),lmax,trim(qdesc(m))
      enddo
      enddo
      write(86,5010)

c Write out correlation tabl ctl2 (actual times)
c ----------------------------------------------
      write(87,5001) trim(statfile)
      write(87,5002) 
      write(87,5003) undef
      write(87,5004) nr
      write(87,5005) 
      write(87,5006) nl,(zlev(k),k=1,nl)
      write(87,6007) nt,hour,minute,day,months(month),year, num
      write(87,5008) 6*(n2d+n3d)    ! Note: 6 Types of Statistical Output per Field (cor, rms, rms_bar, rms_ran, rms_dis, rms_dsp)
      do k=1,6
      do n=1,n2d+n3d
         m=n+(k-1)*(n2d+n3d)
         if( n.le.n2d ) then
             lmax = 0
         else
             lmax = nl
         endif
      write(87,5009) trim(qname(m)),lmax,trim(qdesc(m))
      enddo
      enddo
      write(87,5010)

c Write out global data tabl ctl3 (actual times)
c ----------------------------------------------
      write(88,5001) trim(datafile)
      write(88,5002) 
      write(88,5003) undef
      write(88,8004) im, 360.0/float(im)
      write(88,8005) jm, 180.0/float(jm-1)
      write(88,5006) nl,(zlev(k),k=1,nl)
      write(88,6007) nt,hour,minute,day,months(month),year, num
      write(88,5008) 3*(n2d+n3d)   ! Note: 3 Sets of Fields (forecast, analysis, climatology)
      do m=1,3
         if( m.eq.1 ) suffix = 'f'
         if( m.eq.2 ) suffix = 'a'
         if( m.eq.3 ) suffix = 'c'
      do n=1,n2d+n3d
         if( n.le.n2d ) then
             lmax = 0
             name = trim(fields_2d(n)%name) // trim(suffix)
            dummy = trim(fields_2d(n)%desc)
         else
             lmax = nl
             name = trim(fields_3d(n-n2d)%name) // trim(suffix)
            dummy = trim(fields_3d(n-n2d)%desc)
         endif
      write(88,5009)trim(name),lmax,trim(dummy)
      enddo
      enddo
      write(88,5010)

 5001 format('DSET    ^',a)
 5002 format('TITLE   Stats',/,'FORMAT  sequential big_endian')
 5003 format('UNDEF  ',g12.6)
 5004 format('XDEF  ',i3,' LINEAR  1 1')
 8004 format('XDEF  ',i3,' LINEAR  -180 ',f8.5)
 5005 format('YDEF    1 LINEAR  1 1')
 8005 format('YDEF  ',i3,' LINEAR   -90 ',f8.5)
 5006 format('ZDEF  ',i3,' LEVELS  ',5(f7.2,1x),/,
     .    50(18x,                    5(f7.2,1x),/) )
 5007 format('TDEF  ',i3,' LINEAR  ',   '00:00',   'Z01'   ,a3,i4,'  ',i2.2,'hr')
 6007 format('TDEF  ',i3,' LINEAR  ',i2.2,':',i2.2,'Z',i2.2,a3,i4,'  ',i2.2,'hr')
 5008 format('VARS  ',i3)
 5009 format(a,2x,i3,' 0 ',a)
 5010 format('ENDVARS')

 1003     format(1x,i4,'-mb NH Height at ',i8,2x,i6.6,' (',i3,' hrs)  corr: ',f9.7,
     .           3x,'fcst: ',f7.2,2x,'ana: ',f7.2,2x,'cli: ',f7.2,' (m)')
 1004     format(1x,'Date: ',i8,2x,i6.6,' (',i3,' hrs)')

! **********************************************************************
! ****                       End Collection Loop                    ****
! **********************************************************************
                                                                                                          
      deallocate( corr  )
      deallocate( rms   )
      deallocate( qname )
      deallocate( qdesc )

      close(85)
      close(86)
      close(87)
      close(88)

      enddo

      call timeend ('_colls')

      if (gmaopy) close(luout)

c Write Timing Information
c ------------------------
      call timeend ('main')
      call timepri (6)

      stop
      end

      SUBROUTINE BOUNDS (LAT1,LAT2,LONG1,LONG2,
     .                   JBEG,JEND,IBEG,IEND,IM,JM)
      real lat1,lat2,long1,long2                  
      IMP1 = IM+1                                
      PI   = 180.                               
      DL   = 2.*PI/IM                          
      DP   =    PI/(JM-1)                     
      jbeg = int((lat1 + 90.)/dp)+1          
      jend = int((lat2 + 90.)/dp)+1         
      IF(  JBEG.GT.JM)            JBEG = JM
      IF( (JEND-1)*DP-90.LT.LAT2) JEND=  JEND+1      
                                                    
      IF(  JEND.GT.JM)            JEND = JM        
                                                 
      ibeg = int((long1+180.)/dl)+1             
      iend = int((long2+180.)/dl)+1            
      IF( IBEG.GT.IM)              IBEG = IM  
      IF((IEND-1)*DL-180.LT.LONG2) IEND=IEND+1
      IF( IEND.GT.IM)              IEND = IM 

c     write(6,100) lat1 ,jbeg, lat2,jend,
c    .             long1,ibeg,long2,iend
  100 format(/1x,f6.1,' (',i3,')',2x,f6.1,' (',i3,')',/,
     .        1x,f6.1,' (',i3,')',2x,f6.1,' (',i3,')',/)
      return                                
      end 

      subroutine read_clim_hdf ( nymd,nhms,fields_2d,fields_3d,n2d,n3d,idim,jdim,nl,zlev,cli_files,num,undef )
      implicit none
      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields
      integer n2d,n3d
      type(fields) :: fields_2d(n2d)
      type(fields) :: fields_3d(n3d)

      integer          num
      integer          nymd,nhms
      integer          idim,jdim,nl
      real                  zlev(nl)

      real, allocatable, save :: var2d_1( :,:,:   )
      real, allocatable, save :: var3d_1( :,:,:,: )

      real, allocatable, save :: var2d_2( :,:,:   )
      real, allocatable, save :: var3d_2( :,:,:,: )

      integer       nid,ncl,im,jm,lm,nvars,rc
      integer       ntime,ngatts,timinc
      real          undef
      integer       L,m,n
      character*256 cli_files(num)
                                                                                                          
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      real,    allocatable ::      q(:,:)
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable ::  kmvar(:)

      integer, allocatable, save :: id(:)
      integer, allocatable, save :: yymmdd(:,:)
      integer, allocatable, save :: hhmmss(:,:)

      logical check_names
      logical first, found, shift, defined
      integer LL, i,j,k,kk, loc, len
      data shift /.false./
      data first /.true./ 
      save

      integer   ISCM, ISCP, ISC
      real      FACM, FACP
      integer   MIDMON
      integer   IMNP,     IMNM
      integer   IMONP,    IMONM
      DATA      IMONP/0/, IMONM/0/

      INTEGER   YEAR, MONTH, DAY, SEC
      INTEGER   DAYS(12)
      DATA      DAYS /31,28,31,30,31,30,31,31,30,31,30,31/

      INTEGER    NSECF, NMONF, NDAYF
      NSECF(N) = N/10000*3600 + MOD(N,10000)/100* 60 + MOD(N,100)
      NMONF(N) = MOD(N,10000)/100
      NDAYF(N) = MOD(N,100)
       
C*********************************************************************
C****     Find Proper Month Boundaries from INPUT Date and Time   ****
C*********************************************************************

      SEC    = NSECF(NHMS)
      MONTH  = NMONF(NYMD)
      DAY    = NDAYF(NYMD)

      MIDMON = DAYS(MONTH)/2 + 1 

      IF(DAY.LT.MIDMON) THEN
         imnm = month - 1
         imnp = month
      ELSE
         IMNM = MONTH
         IMNP = MONTH + 1
      ENDIF

      if( imnm.eq.0  ) imnm = 12
      if( imnp.eq.13 ) imnp = 1

C*********************************************************************
C****         Open Climatology DataSet and Initialization         ****
C*********************************************************************

      if( first ) then
          allocate ( yymmdd(12,num) )
          allocate ( hhmmss(12,num) )
          allocate ( id(num)        )

          do n=1,num
             call gfio_open ( cli_files(n),1,id(n),rc )
             if( rc.ne.0 ) then
                 print *, 'Climatology File: ',trim(cli_files(n)),' NOT found!'
                 call exit(1)
                 stop
             endif
             call gfio_diminquire ( id(n),im,jm,lm,ntime,nvars,ngatts,rc )

             if( ntime.ne.12 ) then
                 print *, 'Climatology data should consist of 12 monthly means'
                 print *, 'Current file: ',trim(cli_files(n)),' contains ',ntime,' time periods!'
                 stop
             endif

             if( first ) then
                 allocate ( lon(im) )
                 allocate ( lat(jm) )
                 allocate ( lev(lm) )
                 allocate (  vname(nvars) )
                 allocate ( vtitle(nvars) )
                 allocate ( vunits(nvars) )
                 allocate (  kmvar(nvars) )
                 allocate ( vrange(2,nvars) )
                 allocate ( prange(2,nvars) )
             endif
                                                                                                          
             call gfio_inquire ( id(n),im,jm,lm,ntime,nvars,
     .                           title,source,contact,undef,
     .                           lon,lat,lev,levunits,
     .                           yymmdd(1,n),hhmmss(1,n),timinc,
     .                           vname,vtitle,vunits,kmvar,
     .                           vrange,prange,rc )
             first = .false.
          enddo

          if( lon(1).eq.0.0 ) then
          !   print *, 'Climatology data begins at lon: ',lon(1)
          !   print *, 'Horizontal Shift will be performed'
          !   print *
              shift = .true.
          endif

          allocate ( var2d_1(idim,jdim,   n2d), var2d_2(idim,jdim,   n2d) )
          allocate ( var3d_1(idim,jdim,nl,n3d), var3d_2(idim,jdim,nl,n3d) )
      endif

      if( size(var2d_1,3).ne.n2d ) then
          deallocate( var2d_1,var2d_2 )
            allocate( var2d_1(idim,jdim,   n2d), var2d_2(idim,jdim,   n2d) )
      endif
      if( size(var3d_1,4).ne.n3d ) then
          deallocate( var3d_1,var3d_2 )
            allocate( var3d_1(idim,jdim,nl,n3d), var3d_2(idim,jdim,nl,n3d) )
      endif

c Initial Forecast Fields to ZERO
c -------------------------------
      do n=1,n2d
         fields_2d(n)%clim = 0.0
      enddo
      do n=1,n3d
         fields_3d(n)%clim = 0.0
      enddo

c Determine Climatology TOD Dataset
c ---------------------------------
      if( num.eq.1 ) then
          nid = id(num)
          ncl =    num
      else
          nid = -999
          do n=1,num
          if( nhms.eq.hhmmss(1,n) ) then
              nid = id(n)
              ncl = n
          endif
          enddo
          if( nid.eq.-999 ) then
              print *, 'Climatology Datasets do not have desired TOD: ',nhms
              stop
          endif
      endif

C*********************************************************************
C****                Read for (-) Month and (+) Month             ****
C*********************************************************************

         IMONM = IMNM
         IMONP = IMNP

      allocate ( q(im,jm) )

      do k=1,ntime
      MONTH = NMONF(yymmdd(k,ncl))

      if( month.eq.imnm .or. month.eq.imnp ) then
      do n=1,nvars

         do m=1,n2d
            len = size( fields_2d(m)%alias )
            do kk = 1,len
            if( check_names( vname(n),fields_2d(m)%alias(kk) ) ) then
             call gfio_getvar ( nid,vname(n),yymmdd(k,ncl),hhmmss(k,ncl),im,jm,0,1,q,rc )
             if( shift ) call hshift ( q,im,jm )
                 if( check_names( fields_2d(m)%name,'p' ) ) then
                     do j=1,jm
                     do i=1,im
                     if( defined(q(i,j),undef) .and. q(i,j).gt.10000.0 ) then
                                 q(i,j) = q(i,j)*0.01  ! Convert Pa=>mb
                     endif
                     enddo
                     enddo
                 endif
                 if( check_names( fields_2d(m)%type,'aerosol' ) ) then
                     do j=1,jm
                     do i=1,im
                     if( defined(q(i,j),undef) ) q(i,j) = log( q(i,j)+0.01 )
                     enddo
                     enddo
                 endif
             call bin ( q,im,jm,fields_2d(m)%clim,idim,jdim,undef,fields_2d(m)%msgn )
            endif
            enddo
         enddo


         do L=1,nl
                                     loc = -1
            do LL=1,lm
            if( lev(LL).eq.zlev(L) ) loc = LL
            enddo

            do m=1,n3d
               len = size( fields_3d(m)%alias )
               do kk = 1,len
               if( check_names( vname(n),fields_3d(m)%alias(kk) ) ) then
                   if( loc.ne.-1 ) then
                       call gfio_getvar ( nid,vname(n),yymmdd(k,ncl),hhmmss(k,ncl),im,jm,loc,1,q,rc )
                   else
                       q = undef
                   endif
                   if( shift ) call hshift ( q,im,jm )
                               call bin    ( q,im,jm,fields_3d(m)%clim(1,1,L),idim,jdim,undef,fields_3d(m)%msgn )

               endif
               enddo
            enddo

         enddo

      enddo

         if( month.eq.imnm ) then
             do kk=1,n2d
             var2d_1(:,:,kk) = fields_2d(kk)%clim(:,:,1)
             enddo
             do kk=1,n3d
             var3d_1(:,:,:,kk) = fields_3d(kk)%clim(:,:,:)
             enddo
         endif
         if( month.eq.imnp ) then
             do kk=1,n2d
             var2d_2(:,:,kk) = fields_2d(kk)%clim(:,:,1)
             enddo
             do kk=1,n3d
             var3d_2(:,:,:,kk) = fields_3d(kk)%clim(:,:,:)
             enddo
         endif
      endif
      enddo

      deallocate ( q )

C*********************************************************************
C****             INTERPOLATE DATA TO CURRENT TIME                ****
C*********************************************************************

      IF(DAY.LT.MIDMON) THEN
               ISC  = (DAY+DAYS(IMONM))*86400 + SEC
      ELSE
               ISC  = (DAY            )*86400 + SEC
      ENDIF

      ISCM = (DAYS(IMONM)/2+1               )*86400
      ISCP = (DAYS(IMONP)/2+1 + DAYS(IMONM) )*86400

      FACP = (ISC-ISCM) / REAL(ISCP-ISCM)
      FACM = 1-FACP

c     write(6,100) imonm,facm,imonp,facp
c100  format(1x,'Interpolating Climatology for Month ',i2,
c    .          ' (',f5.3,') and Month ',i2,' (',f5.3,')')

      do n=1,n2d
      do J=1,JDIM
      do I=1,IDIM
         if( var2d_1(i,j,n).ne.undef .and. var2d_2(i,j,n).ne.undef ) then
             fields_2d(n)%clim(I,J,1) = var2d_1(I,J,n)*FACM + var2d_2(I,J,n)*FACP
         else
             if( var2d_1(i,j,n).ne.undef ) then
                 fields_2d(n)%clim(I,J,1) = var2d_1(I,J,n)
             else
                 if( var2d_2(i,j,n).ne.undef ) then
                     fields_2d(n)%clim(I,J,1) = var2d_2(I,J,n)
                 else
                     fields_2d(n)%clim(I,J,1) = undef
                 endif
             endif
         endif
      enddo
      enddo
      enddo

      do n=1,n3d
      do L=1,NL
      do J=1,JDIM
      do I=1,IDIM
         if( var3d_1(i,j,L,n).ne.undef .and. var3d_2(i,j,L,n).ne.undef ) then
             fields_3d(n)%clim(I,J,L) = var3d_1(I,J,L,n)*FACM + var3d_2(I,J,L,n)*FACP
         else
             if( var3d_1(i,j,L,n).ne.undef ) then
                 fields_3d(n)%clim(I,J,L) = var3d_1(I,J,L,n)
             else
                 if( var3d_2(i,j,L,n).ne.undef ) then
                     fields_3d(n)%clim(I,J,L) = var3d_2(I,J,L,n)
                 else
                     fields_3d(n)%clim(I,J,L) = undef
                 endif
             endif
         endif
      enddo
      enddo
      enddo
      enddo

      return
      end

      subroutine read_clim_bin ( nymd,nhms,p,u,v,t,q,h,idim,jdim,ldim,undef )
 
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C*    Note:  Climatology Data is in Grads Format from the files:       *
C*                          ncep_1x1_clim.ctl                          *
C*                          ncep_1x1_clim.data                         *
C*    Climatology Data is stored:  January through December            *
C***********************************************************************
 
      PARAMETER   ( IM = 360 )                                                 
      PARAMETER   ( JM = 181 )                                                 
      PARAMETER   ( LM =  10 )                                                 

      real*4 bum(IM,JM)
      real   dum(IM,JM)

      real p ( IDIM,JDIM      )                                                  
      real u ( IDIM,JDIM,LDIM )                                                  
      real v ( IDIM,JDIM,LDIM )                                                  
      real t ( IDIM,JDIM,LDIM )                                                  
      real q ( IDIM,JDIM,LDIM )                                                  
      real h ( IDIM,JDIM,LDIM )                                                  

      real, allocatable, save :: p1( :,:   )                                                  
      real, allocatable, save :: u1( :,:,: )                                                  
      real, allocatable, save :: v1( :,:,: )                                                  
      real, allocatable, save :: t1( :,:,: )                                                  
      real, allocatable, save :: q1( :,:,: )                                                  
      real, allocatable, save :: h1( :,:,: )                                                  

      real, allocatable, save :: p2( :,:   )                                                  
      real, allocatable, save :: u2( :,:,: )                                                  
      real, allocatable, save :: v2( :,:,: )                                                  
      real, allocatable, save :: t2( :,:,: )                                                  
      real, allocatable, save :: q2( :,:,: )                                                  
      real, allocatable, save :: h2( :,:,: )                                                  

      integer LL
      real    undef
      logical first
      data    first /.true./
       
      DATA IMONP/0/, IMONM/0/, ku /90/
      SAVE    imonp, imonm
      
      INTEGER   DAYS(12)
      DATA      DAYS /31,28,31,30,31,30,31,31,30,31,30,31/

      NSECF(N) = N/10000*3600 + MOD(N,10000)/100* 60 + MOD(N,100)
      NMONF(N) = MOD(N,10000)/100
      NDAYF(N) = MOD(N,100)
       
      if( first ) then
      open  (90,file='ncep_1x1_clim.data',
     .          form='unformatted',access='direct',recl=im*jm )
c    .          form='unformatted',access='direct',recl=im*jm*4,
c    .          convert='little_endian')
      allocate ( p1(idim,jdim)     , p2(idim,jdim)      )
      allocate ( u1(idim,jdim,ldim), u2(idim,jdim,ldim) ) 
      allocate ( v1(idim,jdim,ldim), v2(idim,jdim,ldim) ) 
      allocate ( t1(idim,jdim,ldim), t2(idim,jdim,ldim) ) 
      allocate ( q1(idim,jdim,ldim), q2(idim,jdim,ldim) ) 
      allocate ( h1(idim,jdim,ldim), h2(idim,jdim,ldim) ) 
      first = .false.
      endif

      undef = 1e15

C*********************************************************************
C****                 FIND PROPER MONTH BOUNDARIES                ****
C*********************************************************************
      
      SEC    = NSECF(NHMS)
      MONTH  = NMONF(NYMD)
      DAY    = NDAYF(NYMD)

      MIDMON = DAYS(MONTH)/2 + 1

      IF(DAY.LT.MIDMON) THEN
         imnm = month - 1
         imnp = month 
      ELSE
         IMNM = MONTH
         IMNP = MONTH + 1
      ENDIF

      if( imnm.eq.0  ) imnm = 12
      if( imnp.eq.13 ) imnp = 1

C*********************************************************************
C****           READ DATA SET TWICE TO GET BOTH MONTHS            ****
C*********************************************************************

      IF(IMONM.NE.IMNM .OR. IMONP.NE.IMNP) THEN

         IMONM = IMNM
         IMONP = IMNP

c Read First Month
c ----------------
                          month1 = imonm
        if( month1.lt.1 ) month1 = month1 + 12

        n = (month1-1)*(1+LM*5)
        read(ku,rec=n+1) bum
        dum = bum
        call bin ( dum,im,jm,p1,idim,jdim,undef,0 )

        do L=1,LM
        read(ku,rec=n+1+L) bum
        dum = bum
        call bin ( dum,im,jm,u1(1,1,L),idim,jdim,undef,1 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+LM) bum
        dum = bum
        call bin ( dum,im,jm,v1(1,1,L),idim,jdim,undef,1 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+2*LM) bum
        dum = bum
        call bin ( dum,im,jm,t1(1,1,L),idim,jdim,undef,0 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+3*LM) bum
        dum = bum
        call bin ( dum,im,jm,q1(1,1,L),idim,jdim,undef,0 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+4*LM) bum
        dum = bum
        call bin ( dum,im,jm,h1(1,1,L),idim,jdim,undef,0 )
        enddo

c Read Second Month
c -----------------
                          month2 = imonp
        if( month2.lt.1 ) month2 = month2 + 12

        n = (month2-1)*(1+LM*5)
        read(ku,rec=n+1) bum
        dum = bum
        call bin ( dum,im,jm,p2,idim,jdim,undef,0 )

        do L=1,LM
        read(ku,rec=n+1+L) bum
        dum = bum
        call bin ( dum,im,jm,u2(1,1,L),idim,jdim,undef,1 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+LM) bum
        dum = bum
        call bin ( dum,im,jm,v2(1,1,L),idim,jdim,undef,1 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+2*LM) bum
        dum = bum
        call bin ( dum,im,jm,t2(1,1,L),idim,jdim,undef,0 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+3*LM) bum
        dum = bum
        call bin ( dum,im,jm,q2(1,1,L),idim,jdim,undef,0 )
        enddo

        do L=1,LM
        read(ku,rec=n+1+L+4*LM) bum
        dum = bum
        call bin ( dum,im,jm,h2(1,1,L),idim,jdim,undef,0 )
        enddo

        ENDIF

C*********************************************************************
C****             INTERPOLATE DATA TO CURRENT TIME                ****
C*********************************************************************

      IF(DAY.LT.MIDMON) THEN
               ISC  = (DAY+DAYS(IMONM))*86400 + SEC
      ELSE
               ISC  = (DAY            )*86400 + SEC
      ENDIF

      ISCM = (DAYS(IMONM)/2+1               )*86400
      ISCP = (DAYS(IMONP)/2+1 + DAYS(IMONM) )*86400
         
      FACP = (ISC-ISCM) / REAL(ISCP-ISCM)
      FACM = 1-FACP

c     write(6,100) imonm,facm,imonp,facp
c100  format(1x,'Interpolating Climatology for Month ',i2,
c    .          ' (',f5.3,') and Month ',i2,' (',f5.3,')')

      do J=1,JDIM
      do I=1,IDIM
      p(I,J) = p1(I,J)*FACM + p2(I,J)*FACP
      enddo
      enddo
      do L=1,LDIM
      do J=1,JDIM
      do I=1,IDIM
      u(I,J,L) = u1(I,J,L)*FACM + u2(I,J,L)*FACP
      v(I,J,L) = v1(I,J,L)*FACM + v2(I,J,L)*FACP
      t(I,J,L) = t1(I,J,L)*FACM + t2(I,J,L)*FACP
      q(I,J,L) = q1(I,J,L)*FACM + q2(I,J,L)*FACP
      h(I,J,L) = h1(I,J,L)*FACM + h2(I,J,L)*FACP
      enddo
      enddo
      enddo

      RETURN                                                                    
      END                                                                       

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*abs(undef)
      return
      end

      subroutine hshift ( q,im,jm )
      real q(im,jm), dum(im,jm)
      dum(1:im/2,:)    =   q(1:im/2,:)
        q(1:im/2,:)    =   q(1+im/2:im,:)
        q(1+im/2:im,:) = dum(1:im/2,:)
      return
      end

      subroutine minmax (q,im,jm,undef)
      real   q(im,jm)
      qmin = q(1,1)
      qmax = q(1,1)
      do j=1,jm
      do i=1,im
      if(q(i,j).ne.undef) qmin = min( qmin,q(i,j) )
      if(q(i,j).ne.undef) qmax = max( qmax,q(i,j) )
      enddo
      enddo
      print *, ' qmin: ',qmin,' qmax: ',qmax
      return
      end

      subroutine bin ( qin,im_in,jm_in,qout,im_out,jm_out,undef,msgn )
      implicit  none
      integer   im_in ,jm_in ,msgn
      integer   im_out,jm_out
      real      undef
      real    qin(im_in ,jm_in )
      real   qout(im_out,jm_out)
      real q10x10(360*6,180*6)
 
c Parse Arbitray Field (im,jm) to 10'x10' Variable
c ------------------------------------------------
      call timebeg ('___bin')
      call bin_10x10 ( qin,im_in,jm_in,q10x10 )
 
c Bin 10'x10' Variable to Output Field (im_out,jm_out)
c ----------------------------------------------------
      call averaged_10x10 ( q10x10,qout,im_out,jm_out,undef,msgn )
      call timeend ('___bin')

      return
      end

      subroutine averaged_10x10 ( z10x10,z,im,jm,undef,msgn )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Average a (10m X 10m) input array to an output array (im,jm)
C
C  INPUT:
C  ======
C    z10x10 ..... Input array(360*6,180*6)
C    msgn ....... Integer Flag for scalar (0) or vector (1)
C
C  OUTPUT:
C  =======
C    z .......... Output array(im,jm)
C    im ......... Longitudinal dimension of z
C    jm ......... Latitudinal  dimension of z
C
C  NOTES:
C  ======
C    Input array z10x10  represents values within a 10min X 10min grid-box.
C             Each box is referenced by the latitude and longitude of
C             its southwest corner, not its center point.  Thus,
C             the height associated with a coordinate actually
C             represents the heights centered to the northeast of that point.
C
C    Output array z(im,jm) is assumed to be on an A-grid.
C                 z(i,j)   represents the value at the center of the grid-box.
C                 z(1,j)   is located at lon=-180.
C                 z(i,1)   is located at lat=-90.
C                 z(i,jm)  is located at lat=+90.
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer im,jm,msgn
      real  z(im,jm)
      real  dlam(im), dphi(jm)
      real  z10x10(360*6,180*6)

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

      call timebeg ('____ave_10x10')
      pi   = 4.*atan(1.)
      dz   = pi/(6.*180)
      dlam = 2*pi/ im
      dphi =   pi/(jm-1)

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
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
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
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
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
      jend = 1080

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
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
      enddo

c Average Pole Values
c -------------------
      if( msgn.eq.0 ) then
      sum1 = 0
         j = 0
      do i=1,im
         if( defined(z(i,1),undef) ) then
             sum1 = sum1 + z(i,1)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      z(:,1) = sum1/j
      else
      z(:,1) = undef
      endif

      sum2 = 0
         j = 0
      do i=1,im
         if( defined(z(i,jm),undef) ) then
             sum2 = sum2 + z(i,jm)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      z(:,jm) = sum2/j
      else
      z(:,jm) = undef
      endif

      endif

      call timeend ('____ave_10x10')
      return
      end

      subroutine bin_10x10 ( z,im,jm,z10x10 )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute a (10m X 10m) array binned from an input array (im,jm)
C
C  INPUT:
C  ======
C    z .......... Input array(im,jm)
C    im ......... Longitudinal dimension of z
C    jm ......... Latitudinal  dimension of z
C
C  OUTPUT:
C  =======
C    z10x10 ..... Output array(360*6,180*6)
C
C  NOTES:
C  ======
C    Input array z(im,jm) is assumed to be on an A-grid.
C                z(i,j)   represents the value at the center of the grid-box.
C                z(1,j)   is located at lon=-180.
C                z(i,1)   is located at lat=-90.
C                z(i,jm)  is located at lat=+90.
C
C    Output array z10x10  represents values within a 10min X 10min grid-box.
C             Each box is referenced by the latitude and longitude of
C             its southwest corner, not its center point.  Thus,
C             the height associated with a coordinate actually
C             represents the heights centered to the northeast of that point.
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer im,jm
      real  z(im,jm)
      real z10x10(360*6,180*6)

      integer i,j,ii,jj,ibeg,iend,jbeg,jend
      real    zlatc,zlonc
      real    lonbeg,lonend,lat
      real    latbeg,latend
      real    pi,dl,dp,dz 

      call timebeg ('____bin_10x10')
      pi = 4.*atan(1.)
      dl = 2*pi/im
      dp = pi/(jm-1)
      dz = pi/(6.*180)
      
      do j=1,180*6
      do i=1,360*6

      zlatc = -pi/2+(j-0.5)*dz  ! Latitude  at center of 10x10 box
      zlonc = -pi  +(i-0.5)*dz  ! Longitude at center of 10x10 box

c Find bounding lat and lon on IMxJM grid
c ---------------------------------------
      iend = nint( 1.+(zlonc+pi)/dl )
      lonend = -pi + (iend-1)*dl
      if( lonend.ge.zlonc ) then
      lonbeg = -pi + (iend-2)*dl
      else
      iend = iend+1
      lonbeg = lonend
      lonend = -pi + (iend-1)*dl
      endif
      ibeg = iend-1

      jend = nint( 1.+(zlatc+pi/2)/dp )
      latend = -pi/2 + (jend-1)*dp
      if( latend.ge.zlatc ) then
      latbeg = -pi/2 + (jend-2)*dp
      else
      jend = jend+1
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

      z10x10(i,j) = z(ii,jj)
      
      enddo
      enddo

      call timeend ('____bin_10x10')
      return
      end

      subroutine read_anal( nymd,nhms,fields_2d,fields_3d,n2d,n3d,idim,jdim,nl,zlev,ana_files,num_ana_files,undef )
      implicit none
      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields
      type(fields) :: fields_2d(n2d)
      type(fields) :: fields_3d(n3d)

      integer       nymd,nhms,n2d,n3d
      integer       idim,jdim,nl,num_ana_files
      real          zlev(nl)

      integer       id,im,jm,lm,nvars,rc
      integer       ntime,ngatts,timinc
      real          undef
      integer       L,m,n
      character*256 ana_files(num_ana_files)
                                                                                                          
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      real,    allocatable ::      q(:,:)
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable ::  dates(:,:)
      integer, allocatable ::  datez(:,:)

      logical found_2d(n2d)
      logical found_3d(n3d)

      logical check_names
      logical shift, defined, first
      integer num, LL, i,j,k, loc, ndates, len
      data id    /0/
      data num   /0/
      data shift /.false./
      data first /.true./
      save


c Read All ANA Files to Gather Date and Time Information
c ------------------------------------------------------
      if( first ) then
      call timebeg ('__init_anal')
      print *, 'Examining ANA Files ...'
      print *
      ndates = 0
      do num=1,num_ana_files
         call gfio_open       ( ana_files(num),1,id,rc )
         call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
                                                                                                          
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

          if( ndates.eq.0 ) then
              ndates = ndates + ntime
              allocate ( dates(3,ndates) )
              allocate ( datez(3,ndates) )
              dates(1,ndates-ntime+1:ndates) = yymmdd(:)
              dates(2,ndates-ntime+1:ndates) = hhmmss(:)
              dates(3,ndates-ntime+1:ndates) = num
              datez = dates
          else
              deallocate(dates)
              ndates = ndates + ntime
              allocate ( dates(3,ndates) )
              dates(1,1:ndates-ntime) = datez(1,1:ndates-ntime)
              dates(2,1:ndates-ntime) = datez(2,1:ndates-ntime)
              dates(3,1:ndates-ntime) = datez(3,1:ndates-ntime)
              dates(1,ndates-ntime+1:ndates) = yymmdd(:)
              dates(2,ndates-ntime+1:ndates) = hhmmss(:)
              dates(3,ndates-ntime+1:ndates) = num
              deallocate(datez)
              allocate ( datez(3,ndates) )
              datez = dates
          endif

          call gfio_close(id,rc)
          deallocate ( lon,lat,lev,yymmdd,hhmmss,vname,vtitle,vunits,kmvar,vrange,prange )
      enddo
      first = .false.
      call timeend ('__init_anal')
      endif


c Initial Analysis Fields to NOT Found
c ------------------------------------
      do n=1,n2d
      found_2d(n) = .false.
      enddo
      do n=1,n3d
      found_3d(n) = .false.
      enddo

c Read Appropriate ANA Files to Get Data
c --------------------------------------
      do num=1,ndates
      if( dates(1,num).eq.nymd .and. dates(2,num).eq.nhms ) then
          call gfio_open       ( ana_files(dates(3,num)),1,id,rc )
          call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
                                                                                                          
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

              shift = .false.
          if( lon(1).eq.0.0 ) then
          !   print *, 'Analysis data begins at lon: ',lon(1)
          !   print *, 'Horizontal Shift will be performed'
          !   print *
              shift = .true.
          endif

          allocate ( q(im,jm) )

          do n=1,nvars

             do m=1,n2d
             if( .not.found_2d(m) ) then
                len = size( fields_2d(m)%alias )
                do k = 1,len
                if( check_names( vname(n),fields_2d(m)%alias(k) ) ) then
                found_2d(m) = .true.
                 call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,0,1,q,rc )
                 if( shift ) call hshift ( q,im,jm )
                     if( check_names( fields_2d(m)%name,'p' ) ) then
                         do j=1,jm
                         do i=1,im
                         if( defined(q(i,j),undef) .and. q(i,j).gt.10000.0 ) then
                                     q(i,j) = q(i,j)*0.01  ! Convert Pa=>mb
                         endif
                         enddo
                         enddo
                     endif
                     if( check_names( fields_2d(m)%type,'aerosol' ) ) then
                         do j=1,jm
                         do i=1,im
                         if( defined(q(i,j),undef) ) q(i,j) = log( q(i,j)+0.01 )
                         enddo
                         enddo
                     endif
                 call bin ( q,im,jm,fields_2d(m)%anal,idim,jdim,undef,fields_2d(m)%msgn )
                endif
                enddo
             endif
             enddo

         do m=1,n3d
         if( .not.found_3d(m) ) then
             len = size( fields_3d(m)%alias )
             do k = 1,len
               if( check_names( vname(n),fields_3d(m)%alias(k) ) ) then
                   found_3d(m) = .true.

                   do L=1,nl
                                            loc = -1
                   do LL=1,lm
                   if( lev(LL).eq.zlev(L) ) loc = LL
                   enddo
                   if( loc.ne.-1 ) then
                       call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,loc,1,q,rc )
                   else
                       q = undef
                   endif
                   if( shift ) call hshift ( q,im,jm )
                               call bin    ( q,im,jm,fields_3d(m)%anal(1,1,L),idim,jdim,undef,fields_3d(m)%msgn )
                   enddo
               endif
             enddo
         endif
         enddo

          enddo
          call gfio_close(id,rc)
          deallocate ( q,lon,lat,lev,yymmdd,hhmmss,vname,vtitle,vunits,kmvar,vrange,prange )
      endif
      enddo

c Set Analysis Fields to UNDEF if NOT Found
c -----------------------------------------
      do n=1,n2d
      if( .not.found_2d(n) ) fields_2d(n)%anal = undef
      enddo
      do n=1,n3d
      if( .not.found_3d(n) ) fields_3d(n)%anal = undef
      enddo

      return
      end

      subroutine init_fcst( fname,nfiles,dates,ndates,timinc,undef,collections,ncoll )
      implicit none
      integer  ncoll
      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields
      type collection
           integer                    ::  n2d
           integer                    ::  n3d
           character*256              ::  name
           type(fields),  allocatable :: fields_2d(:)
           type(fields),  allocatable :: fields_3d(:)
      endtype collection

      type(collection) :: collections(ncoll)

      integer       n2d,n3d
      integer       id,im,jm,lm,nvars,rc
      integer       ntime,ngatts,timinc,nfiles
      real          undef
      integer       j,k,L,m,n,len
      character*256 fname(nfiles)
                                                                                                          
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
                                                                                                          
      integer  dates(3,1000)
      integer  ndates,num
      logical  check_names

! Read All Forecast Files to Gather Date and Time Information
! -----------------------------------------------------------
      print *, 'Examining Forecast Files ...'
      ndates = 0
      do num=1,nfiles
         call gfio_open       ( fname(num),1,id,rc )
         call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
                                                                                                          
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

          do n=1,nvars
             do j=1,ncoll
                do m=1,collections(j)%n2d
                len = size( collections(j)%fields_2d(m)%alias )
                   do k = 1,len
                   if( check_names( vname(n),collections(j)%fields_2d(m)%alias(k) ) ) then
                       if( trim(vtitle(n)).ne.'**' ) collections(j)%fields_2d(m)%desc = vtitle(n)
                   endif
                   enddo
                enddo
                do m=1,collections(j)%n3d
                len = size( collections(j)%fields_3d(m)%alias )
                   do k = 1,len
                   if( check_names( vname(n),collections(j)%fields_3d(m)%alias(k) ) ) then
                       if( trim(vtitle(n)).ne.'**' ) collections(j)%fields_3d(m)%desc = vtitle(n)
                   endif
                   enddo
                enddo
             enddo
          enddo

              ndates = ndates + ntime

              if( ndates.gt.1000 ) then
                  print *
                  print *, 'Forecast Files have exceeded 1000 Time Periods!'
                  print *
                  stop
              endif

              dates(1,ndates-ntime+1:ndates) = yymmdd(:)
              dates(2,ndates-ntime+1:ndates) = hhmmss(:)
              dates(3,ndates-ntime+1:ndates) = num

          call gfio_close(id,rc)
          deallocate ( lon,lat,lev,yymmdd,hhmmss,vname,vtitle,vunits,kmvar,vrange,prange )
      enddo

      return
      end

      subroutine read_fcst( fname,nfiles,dates,ndates,nymd,nhms,fields_2d,fields_3d,n2d,n3d,idim,jdim,nl,zlev,undef )
      implicit none
      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields
      integer         n2d,n3d
      type(fields) :: fields_2d(n2d)
      type(fields) :: fields_3d(n3d)

      logical       check_names
      logical       defined
      integer       nymd,nhms
      integer       idim,jdim,nl
      real          zlev(nl)

      integer       id,im,jm,lm,nvars,rc
      real          undef
      integer       i,j,k,L,m,n
      integer       LL,loc,len
      logical       shift
      integer       ndates,nfiles,num
      integer        dates(3,ndates)

      real,    allocatable ::      q(:,:)

      character*256  fname(nfiles)
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
      character*256, allocatable :: vname (:)

      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
                                                                                                          
      integer  ntime,ngatts,timinc
      logical  found_2d(n2d)
      logical  found_3d(n3d)

c Initial Forecast Fields to NOT Found
c ------------------------------------
      do n=1,n2d
      found_2d(n) = .false.
      enddo
      do n=1,n3d
      found_3d(n) = .false.
      enddo

c Loop Through All Dates within Forecast Files
c --------------------------------------------
      do num=1,ndates
      if( dates(1,num).eq.nymd .and. dates(2,num).eq.nhms ) then

      call gfio_open       ( fname(dates(3,num)),1,id,rc )
      call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
      allocate ( lon(im) )
      allocate ( lat(jm) )
      allocate ( lev(lm) )
      allocate ( yymmdd(ntime) )
      allocate ( hhmmss(ntime) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate (  vname(nvars) )
      allocate (  kmvar(nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
                                                                                                          
      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,undef,
     .                    lon,lat,lev,levunits,
     .                    yymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    vrange,prange,rc )

          shift = .false.
      if( lon(1).eq.0.0 ) then
      !   print *, 'Forecast data begins at lon: ',lon(1)
      !   print *, 'Horizontal Shift will be performed'
      !   print *
          shift = .true.
      endif
                                                                                                          
      allocate ( q(im,jm) )

c READ Forecast Fields from Forecast Files
c ----------------------------------------
      do n=1,nvars

         do m=1,n2d
         if( .not.found_2d(m) ) then
            len = size( fields_2d(m)%alias )
            do k = 1,len
            if( check_names( vname(n),fields_2d(m)%alias(k) ) ) then
             found_2d(m) = .true.
             call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,0,1,q,rc )
             if( shift ) call hshift ( q,im,jm )
                 if( check_names( fields_2d(m)%name,'p' ) ) then
                     do j=1,jm 
                     do i=1,im 
                     if( defined(q(i,j),undef) .and. q(i,j).gt.10000.0 ) then
                                 q(i,j) = q(i,j)*0.01  ! Convert Pa=>mb
                     endif
                     enddo
                     enddo
                 endif
                 if( check_names( fields_2d(m)%type,'aerosol' ) ) then
                     do j=1,jm
                     do i=1,im
                     if( defined(q(i,j),undef) ) q(i,j) = log( q(i,j)+0.01 )
                     enddo
                     enddo
                 endif
             call bin ( q,im,jm,fields_2d(m)%fcst,idim,jdim,undef,fields_2d(m)%msgn )
            endif
            enddo
         endif
         enddo

         do m=1,n3d
         if( .not.found_3d(m) ) then
             len = size( fields_3d(m)%alias )
             do k = 1,len
               if( check_names( vname(n),fields_3d(m)%alias(k) ) ) then
                   found_3d(m) = .true.

                   do L=1,nl
                                            loc = -1
                   do LL=1,lm
                   if( lev(LL).eq.zlev(L) ) loc = LL
                   enddo

                   if( loc.ne.-1 ) then
                       call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,loc,1,q,rc )
                   else
                       q = undef
                   endif
                   if( shift ) call hshift ( q,im,jm )
                               call bin    ( q,im,jm,fields_3d(m)%fcst(1,1,L),idim,jdim,undef,fields_3d(m)%msgn )
                   enddo

               endif
             enddo
         endif
         enddo

      enddo ! End Loop: n=1,nvars

      call gfio_close(id,rc)
      deallocate ( q,lon,lat,lev,yymmdd,hhmmss,vname,vtitle,vunits,kmvar,vrange,prange )

      endif ! End Dates Test
      enddo ! End Loop: num=1,ndates

c Set Forecast Fields to UNDEF if NOT Found
c -----------------------------------------
      do n=1,n2d
      if( .not.found_2d(n) ) fields_2d(n)%fcst = undef
      enddo
      do n=1,n3d
      if( .not.found_3d(n) ) fields_3d(n)%fcst = undef
      enddo

      return
      end

      subroutine read_syserr( fields_2d,fields_3d,n2d,n3d,idim,jdim,nl,zlev,efile,ndt,undef )
      implicit none

      type fields
           integer                    ::  msgn
           character*256              ::  name
           character*256              ::  desc
           character*256              ::  type
           character*256, allocatable ::  alias(:)
           real,          allocatable ::   anal(:,:,:)
           real,          allocatable ::   fcst(:,:,:)
           real,          allocatable ::   clim(:,:,:)
           real,          allocatable ::   serr(:,:,:)
      endtype fields
      integer      ::  n2d,n3d
      type(fields) :: fields_2d(n2d)
      type(fields) :: fields_3d(n3d)

      logical       check_names

      integer       nymd ,nhms
      integer       nymd0,nhms0
      integer       idim,jdim,nl
      real          pa(idim,jdim)
      real          ua(idim,jdim,nl)
      real          va(idim,jdim,nl)
      real          ta(idim,jdim,nl)
      real          qa(idim,jdim,nl)
      real          ha(idim,jdim,nl)
      real          zlev(nl)

      integer       id,im,jm,lm,nvars,rc
      integer       ntime,ngatts,timinc
      integer       nsecf
      real          undef
      integer       k,L,m,n
      integer       len,ndt,num,nt
      character*256 efile
                                                                                                          
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      real,    allocatable ::      q(:,:)
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
                                                                                                          
      logical found, shift, defined
      integer LL, i,j, loc
      data id    /0/
      data num   /0/
      data shift /.false./
      save

   10 continue
      if( id.eq.0 ) then
          call gfio_open ( efile,1,id,rc )
          if( rc.ne.0 ) then
              print *, 'Systematic Error File: ',trim(efile),' NOT found!'
              stop
          endif
          call gfio_diminquire ( id,im,jm,lm,ntime,nvars,ngatts,rc )
                                                                                                          
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

          if( ndt .ne. nsecf(timinc) ) then
              print *, 'Error!'
              print *, 'Forecast         File Frequency: ',ndt,' (sec)'
              print *, 'Systematic Error File Frequency: ',nsecf(timinc),' (sec)'
              stop
          endif

          if( lon(1).eq.0.0 ) then
          !   print *, 'Systematic Error data begins at lon: ',lon(1)
          !   print *, 'Horizontal Shift will be performed'
          !   print *
              shift = .true.
          endif

          nymd0 = yymmdd(1)
          nhms0 = hhmmss(1)
      endif
                                                                                                          
!     Tick Clock for Next Desired Time Period 
!     ---------------------------------------
      nymd = nymd0
      nhms = nhms0
      nt   = num*ndt
      call tick (nymd,nhms,nt)
      num  = num+1

      found = .false.
      do n=1,ntime
      if( yymmdd(n).eq.nymd .and. hhmmss(n).eq.nhms ) found = .true.
      enddo
      if( .not.found ) then
           write(6,100) nymd,nhms
           do n=1,n2d
              fields_2d(n)%serr = 0.0
           enddo
           do n=1,n3d
              fields_3d(n)%serr = 0.0
           enddo
           return
      endif
 100  format(/,1x,'Cannot find matching SysError Time for ',i8,2x,i6.6,'  ZEROs will be used',/)

      allocate ( q(im,jm) )

      do n=1,n2d
         fields_2d(n)%serr = 0.0
      enddo
      do n=1,n3d
         fields_3d(n)%serr = 0.0
      enddo

      do n=1,nvars

         do m=1,n2d
            len = size( fields_2d(m)%alias )
            do k = 1,len
            if( check_names( vname(n),fields_2d(m)%alias(k) ) ) then
             call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,0,1,q,rc )
             if( shift ) call hshift ( q,im,jm )
                 if( check_names( fields_2d(m)%name,'p' ) ) then
                     do j=1,jm
                     do i=1,im
                     if( defined(q(i,j),undef) .and. q(i,j).gt.10000.0 ) then
                                 q(i,j) = q(i,j)*0.01  ! Convert Pa=>mb
                     endif
                     enddo
                     enddo
                 endif
                 if( check_names( fields_2d(m)%type,'aerosol' ) ) then
                     do j=1,jm
                     do i=1,im
                     if( defined(q(i,j),undef) ) q(i,j) = log( q(i,j)+0.01 )
                     enddo
                     enddo
                 endif
             call bin ( q,im,jm,fields_2d(m)%serr,idim,jdim,undef,fields_2d(m)%msgn )
            endif
            enddo
         enddo


         do L=1,nl
                                     loc = -1
            do LL=1,lm
            if( lev(LL).eq.zlev(L) ) loc = LL
            enddo

            do m=1,n3d
               len = size( fields_3d(m)%alias )
               do k = 1,len
               if( check_names( vname(n),fields_3d(m)%alias(k) ) ) then
                   if( loc.ne.-1 ) then
                       call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,loc,1,q,rc )
                   else
                       q = 0.0
                   endif
                   if( shift ) call hshift ( q,im,jm )
                               call bin    ( q,im,jm,fields_3d(m)%serr(1,1,L),idim,jdim,undef,fields_3d(m)%msgn )

               endif
               enddo
            enddo


         enddo
      enddo

      deallocate ( q )
      return
      end

      subroutine interp_time ( nymd1,nhms1, nymd2,nhms2, ntimes, num )

      integer  nymd1, nhms1, nymd2, nhms2, ntimes, num
      INTEGER  YEAR1, MONTH1, DAY1, SEC1
      INTEGER  YEAR2, MONTH2, DAY2, SEC2

      real     time1, time2

      INTEGER    DAYSCY
      PARAMETER (DAYSCY = 365*4+1)

      REAL MNDY(12,4), DUM(48)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,397,34*0/
      NSECF(N) = N/10000*3600 + MOD(N,10000)/100* 60 + MOD(N,100)
      EQUIVALENCE ( DUM(1), MNDY(1,1) )

      DO   I=15,48
      DUM(I) = DUM(I-12) + 365
      ENDDO

!     DO   I=15,48
!     MNDY(I,1) = MNDY(I-12,1) + 365
!     ENDDO

      YEAR1  = NYMD1 / 10000
      MONTH1 = MOD(NYMD1,10000) / 100
      DAY1   = MOD(NYMD1,100)
      SEC1   = NSECF(NHMS1)

      YEAR2  = NYMD2 / 10000
      MONTH2 = MOD(NYMD2,10000) / 100
      DAY2   = MOD(NYMD2,100)
      SEC2   = NSECF(NHMS2)

      time1 = DAY1 + MNDY(MONTH1,MOD(YEAR1,4)+1) + float(sec1)/86400.
      time2 = DAY2 + MNDY(MONTH2,MOD(YEAR2,4)+1) + float(sec2)/86400.

      if( time2.lt.time1 ) time2 = time2 + dayscy

      num   = 24.0*(time2-time1)/float(ntimes-1)

      RETURN
      END

      subroutine writit ( q,im,jm,lm,qundef,undef )
      implicit none
      integer    im,jm,lm
      real     q(im,jm,lm)
      real*4 dum(im,jm)
      real*4 qundef, undef
      logical defined
      integer i,j,L
      do L=1,lm
      do j=1,jm
      do i=1,im
         if( defined(q(i,j,L),qundef) ) then
          dum(i,j) = q(i,j,L)
         else
          dum(i,j) = undef
         endif
      enddo
      enddo
      write(51) dum
      enddo
      return
      end

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " stats_$ARCH.x -fcst fcst_fname(s)"
      print *, "               -ana   ana_fname(s)"
      print *, "               -cli   climatology(s)"
      print *, "              <-tag   tag>"
      print *, "              <-pref  PREF>"
      print *, "              <-nfreq HHMMSS>"
      print *, "           <-syserror SYSERR>"
      print *
      print *, "where:"
      print *
      print *, "  -fcst  fcst_fname(s):  Filename(s)  of pressure-level forecast    data (HDF)"
      print *, "  -ana    ana_fname(s):  Filename(s)  in pressure-level analysis    data (HDF)"
      print *, "  -cli  climatology(s):  Filename(s)  in pressure-level climatology data (HDF)"
      print *, "                         Note:"
      print *, "                          A single Mean Climatology, or a"
      print *, "                          Set of 4 Climatologies (00,06,12,18z) may be used"
      print *
      print *, "  -tag    tag         :  Optional Tag for Output Names"
      print *, "  -pref   PREF        :  Optional Reference Pressure for STD Output Printing"
      print *, "                                  (Default: 500-mb)"
      print *, "  -nfreq  HHMMSS      :  Optional Frequency for Forecast Time Periods"
      print *, "  -syserr SYSERR File :  Optional Systematic Error File"
      print *, "                                  to be Subtracted from Forecasts"
      print *
      print *, "creates:"
      print *
      print *, "Tag_Name.stats.b{YYYYMMDD}.e{YYYYMMDD}.data"
      print *, "Tag_Name.stats.b{YYYYMMDD}.e{YYYYMMDD}.ctl1"
      print *, "Tag_Name.stats.b{YYYYMMDD}.e{YYYYMMDD}.ctl2"
      print *
      call exit(7)
      end subroutine usage

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

      function check_names (name1,name2)
      implicit none
      logical  check_names
      character(*)   name1,name2
      integer  len,i,j
      character*256 uname1,uname2
      character*1   c

c Convert name1 to All UpperCase uname1
c -------------------------------------
      len = len_trim(name1)
      uname1 = ''
      do i=1,len
         c = name1(i:i)
         if( ichar(c).ge.97 .and. ichar(c).le.122 ) then
             c = achar( ichar(c)-32 )
         endif
         uname1 = trim(uname1) // c
      enddo

c Convert name2 to All UpperCase uname2
c -------------------------------------
      len = len_trim(name2)
      uname2 = ''
      do i=1,len
         c = name2(i:i)
         if( ichar(c).ge.97 .and. ichar(c).le.122 ) then
             c = achar( ichar(c)-32 )
         endif
         uname2 = trim(uname2) // c
      enddo

c Compare uname1 and uname2
c -------------------------
      check_names = ( trim(uname1) == trim(uname2) )
      return
      end
