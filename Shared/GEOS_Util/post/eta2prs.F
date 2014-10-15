      program  main

      implicit none
      include 'alias.com'

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****         Program to create prs output from an eta file        ****
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
      integer  im_out, jm_out


c Generic Model Variables
c -----------------------
      real, allocatable ::   ps(:,:)
      real, allocatable ::   dp(:,:,:)
      real, allocatable ::  q2d(:,:,:)
      real, allocatable ::  q3d(:,:,:,:)


c HDF and other Local Variables
c -----------------------------
      logical,       pointer :: Lsurf (:)
      real,          pointer :: lon   (:)
      real,          pointer :: lat   (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      character*256, pointer :: unit2d(:), unit3d(:)
      character*256, pointer :: namesp (:)
      character*256, pointer :: name2dp(:), name3dp(:)
      character*256, pointer :: titl2dp(:), titl3dp(:)
      character*256, pointer :: unit2dp(:), unit3dp(:)

      integer id,rc,fid,nhmsf,n2d,n3d
      integer idpr,n2dp,n3dp,nvarsp
      integer nvars,ngatts,ntime,ntimes,gfrc

      real,          allocatable :: plevs(:)
      character*256, allocatable ::   arg(:)
      character*256, allocatable :: fname(:)
      character*256, allocatable :: prfname(:)
      character*256  dummy,  name
      character*256  output, hdfile
      character*256  ftype
      character*256  ext

      character*8    date,date0
      character*4    time0
      character*2    time,hour0,mins0
      character*1    char
      data output  /'eta2prs'/

      integer n,m,nargs,iargc,L,nbeg,nfiles,npfiles,mlev

      real*8   lonbeg
      real     undef
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

      real, allocatable ::  dum2d(:,:)
      real, allocatable ::  dum3d(:,:,:)

      integer i,j,ndt
      integer imax,jmax
      logical hdf, quad
      logical nopres
      logical hdfcreate
      logical edges
      logical underg
      real    ptop

      character*8 cdate

      interface
                subroutine read_hdf_meta ( hdffile,im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,id,
     .                                     nymdb,nhmsb,ndt,ntimes,
     .                                     nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )
                logical,       pointer :: Lsurf (:)
                real,          pointer :: lat   (:)
                real,          pointer :: lon   (:)
                character*256, pointer :: names (:)
                character*256, pointer :: name2d(:), name3d(:)
                character*256, pointer :: titl2d(:), titl3d(:)
                character*256, pointer :: unit2d(:), unit3d(:)
                character*256 hdffile
                integer       id,im,jm,lm,n2d,n3d,nvars
                integer       nymdb,nhmsb,ndt,ntimes
                real          undef
                real*8        lonbeg
            end subroutine read_hdf_meta
      end interface

C **********************************************************************
C ****                       Initialization                         ****
C **********************************************************************

      call timebeg ('main')
      ftype   = 'xxx'
      im_out  = -999
      jm_out  = -999
      nymd0   = -999
      nhms0   = -999
      nymdb0  = -999
      nhmsb0  = -999
      ptop    =  1.0
      nt      =  1
      ndt     =  0
      hdf     =  .true.
      quad    =  .true.
      nopres  =  .false.
      underg  =  .false.

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-ptop'   ) read(arg(n+1),*) ptop
             if( trim(arg(n)).eq.'-im'     ) read(arg(n+1),*) im_out
             if( trim(arg(n)).eq.'-jm'     ) read(arg(n+1),*) jm_out
             if( trim(arg(n)).eq.'-nymd'   ) read(arg(n+1),*) nymd0
             if( trim(arg(n)).eq.'-nhms'   ) read(arg(n+1),*) nhms0
             if( trim(arg(n)).eq.'-nymdb'  ) read(arg(n+1),*) nymdb0
             if( trim(arg(n)).eq.'-nhmsb'  ) read(arg(n+1),*) nhmsb0
             if( trim(arg(n)).eq.'-ndt'    ) read(arg(n+1),*) ndt
             if( trim(arg(n)).eq.'-hdf'    ) read(arg(n+1),*) hdf
             if( trim(arg(n)).eq.'-noquad' ) quad    = .false.
             if( trim(arg(n)).eq.'-ana'    ) ftype   = 'ana'
             if( trim(arg(n)).eq.'-underg' ) underg  = .true.
             if( trim(arg(n)).eq.'-tag'    ) output  = arg(n+1)

             if( trim(arg(n)).eq.'-levs' ) then
                 mlev = 1
                 read(arg(n+mlev),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+mlev.lt.nargs )
                 mlev = mlev+1
                 read(arg(n+mlev),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) mlev = mlev-1
                 allocate ( plevs(mlev) )
                 do m=1,mlev
                 read(arg(n+m),*) plevs(m)
                 enddo
             endif

             if( trim(arg(n)).eq.'-eta' ) then
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
	     
	     if( trim(arg(n)).eq.'-prs' ) then
	         nopres = .true.
                 npfiles = 1
                 read(arg(n+npfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+npfiles.ne.nargs )
                 npfiles = npfiles+1
                 read(arg(n+npfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' )  npfiles = npfiles-1
                 allocate ( prfname(npfiles) )
                 do m=1,npfiles
                 prfname(m) = arg(n+m)
                 enddo
		 if( npfiles .ne. nfiles) then
		   print *,' need same number of pressure,diag files',
     .                     ' nfiles= ',nfiles,'   npfiles= ',npfiles
                   stop
		 endif
             endif
	     
           enddo
      endif

C **********************************************************************
C ****                        Read HDF Meta Data                    ****
C **********************************************************************

      if( nopres) then      
          call read_hdf_meta ( fname(1),im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,id,
     .                         nymdb,nhmsb,ndt,ntimes,
     .                         nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )
      
          call read_hdf_meta ( prfname(1),im,jm,lm,n2dp,n3dp,lat,lon,lonbeg,undef,idpr,
     .                         nymdb,nhmsb,ndt,ntimes,
     .                         nvarsp,namesp,Lsurf,name2dp,titl2dp,unit2dp,name3dp,titl3dp,unit3dp )

      else
          call read_hdf_meta ( fname(1),im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,id,
     .                         nymdb,nhmsb,ndt,ntimes,
     .                         nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )

      endif

C **********************************************************************
C ****                Summarize Input Variables                     ****
C **********************************************************************

      allocate (    ps(im,jm)        )
      allocate (    dp(im,jm,lm)     )
      allocate (   q2d(im,jm,   n2d) )
      allocate (   q3d(im,jm,lm,n3d) )
      allocate ( dum2d(im,jm)        )
      allocate ( dum3d(im,jm,lm)     )

      if( im_out.eq.-999 ) im_out = im
      if( jm_out.eq.-999 ) jm_out = jm


c Define Beginning Date and Time to Read
c --------------------------------------
      if(  nymdb0 /= -999 ) nymdb = nymdb0
      if(  nhmsb0 /= -999 ) nhmsb = nhmsb0


c Define Date and Time to Write in Output
c ---------------------------------------
      if(  nymd0 == -999 ) then
           nymd   = nymdb
      else
           nymd   = nymd0
      endif
      if(  nhms0 == -999 ) then
           nhms   = nhmsb
      else
           nhms   = nhms0
      endif

                    print *
                    print *, '   Output Resolution  im: ',im_out
                    print *, '   Output Resolution  jm: ',jm_out
                    print *
                    print *, 'Beginning Date to   Read: ',nymdb
                    print *, 'Beginning Time to   Read: ',nhmsb
                    print *
                    print *, 'Beginning Date to  Write: ',nymd
                    print *, 'Beginning Time to  Write: ',nhms
                    print *, '          Time Increment: ',nhmsf(ndt),' (',ndt,' seconds)'
                    print *
                    print *, '                      lm: ',lm
                    print *, '                   plevs: ',(plevs(L),L=1,mlev)
                    print *
                    print *, '2-D Fields:'
                    do n=1,n2d
                    print *,   trim(name2d(n)),'  ',trim(unit2d(n)),' ',trim(titl2d(n))
                    enddo
                    print *
                    print *, '3-D Fields:'
                    do n=1,n3d
                    print *,   trim(name3d(n)),'  ',trim(unit3d(n)),' ',trim(titl3d(n))
                    enddo
                    print *

      if( nopres ) then
        print *, 'Pressure Files (for ps,delp info): '
        do n=1,nfiles
	  print *, n,trim(prfname(n))
	enddo
        print *
      endif

      print *, 'Eta Files: '
      do n=1,nfiles
      print *, n,trim(fname(n))
      enddo
      print *
      name = fname(1)
         n = index(trim(name),'.',back=.true.)
       ext = trim(name(n+1:))

      plevs(1:mlev) = plevs(mlev:1:-1)*100

C **********************************************************************
C ****              Read and Interpolate Eta File                   ****
C **********************************************************************

      edges = .false.

      do n=1,nfiles
      if( nopres ) print *, 'Opening: ',trim(prfname(n))
                   print *, 'Opening: ',trim(  fname(n))

      write(date0,1000)  nymd
      write(hour0,2000)  nhms/10000
      write(mins0,2000) (nhms-(nhms/10000)*10000)/100
 1000 format(i8.8)
 2000 format(i2.2)
      time0 = trim(hour0)//trim(mins0)

      if(hdf) then
         hdfile = trim(output) // "." // trim(date0) // "_" // trim(time0) // "z." // trim(ext)
      else
         hdfile = trim(output) // "." // trim(date0) // "_" // trim(time0) // "z.bin"
      endif

          call gfio_close ( id,rc )
          call gfio_open  ( trim(fname(n)),1,id,rc ) 
      if( nopres ) then
          call gfio_close ( idpr,rc )
          call gfio_open  ( trim(prfname(n)),1,idpr,rc )
      endif

      rc        =  0
      ntime     =  0
      hdfcreate = .true.
              dowhile (rc.eq.0)
              ntime = ntime + 1
              print *

              nymdr = nymd  
              nhmsr = nhms  

              if( (nymd.eq.nymdb .or. nymd.eq.nymd0)  .and. 
     .            (nhms.eq.nhmsb .or. nhms.eq.nhms0) ) then
                  if( nymdb /= nymd ) nymdr = nymdb
                  if( nhmsb /= nhms ) nhmsr = nhmsb
              endif

              print *, 'Reading nymd: ',nymdr,' nhms: ',nhmsr
	      if( nopres ) then
		  call read_hdf_data  ( idpr,dp,ps,q2d,q3d,n2dp,n3dp,name2dp,name3dp,.true.,.false.,
     .                                  im,jm,lm,nymdr,nhmsr,rc,ntime,ntimes,edges,ftype )
		  call read_hdf_data  ( id,dum3d,dum2d,q2d,q3d,n2d,n3d,name2d,name3d,.false.,.true.,
     .                                  im,jm,lm,nymdr,nhmsr,rc,ntime,ntimes,edges,ftype )
              else
                  call read_hdf_data  ( id,dp,ps,q2d,q3d,n2d,n3d,name2d,name3d,.true.,.true.,
     .                                  im,jm,lm,nymdr,nhmsr,rc,ntime,ntimes,edges,ftype )
	      endif

              if( rc.eq.0 ) then
                  print *, 'Writing nymd: ',nymd,' nhms: ',nhms
                  call timebeg (' Eta2Prs')
                  call eta2prs ( dp,ps,ptop,q2d,q3d,name2d,titl2d,unit2d,name3d,titl3d,unit3d,n2d,n3d,undef,
     .                           im,jm,lm,nt,plevs,mlev,im_out,jm_out,lat,lon,lonbeg,nymd,nhms,ndt,
     .                           fid,hdf,hdfcreate,hdfile,quad,edges,ftype,underg )
                  call timeend (' Eta2Prs')
                  call tick (nymd,nhms,ndt)
                  hdfcreate = .false.
              else
                  if(hdf) then
                     call gfio_close ( fid,gfrc )
                  else
                     close(55)
                  endif
                  print *, 'Created: ',trim(hdfile)
                  print *
                  print *
              endif
              enddo
      enddo

c Write Timing Information
c ------------------------
      call timeend ('main')
      call timepri (6)

      deallocate ( dp,ps,arg )

      stop
      end

      subroutine read_hdf_meta ( hdffile,im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,id,
     .                           nymdb,nhmsb,ndt,ntime,
     .                           nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )
      implicit none

      logical,       pointer :: Lsurf (:)
      real,          pointer :: lat   (:)
      real,          pointer :: lon   (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      character*256, pointer :: unit2d(:), unit3d(:)

      character*256 hdffile
      integer       id,im,jm,lm,n2d,n3d,nvars,nsecf,timeId,ncvid
      integer       ntime,ngatts,rc,timinc,nymdb,nhmsb,ndt
      real          undef
      real*8        lonbeg
      integer       L,m,n
      character*256 dummy,name

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
                                                                                                                     
      real,    allocatable ::    lev(:)
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
      names  = vname
      lonbeg = lon(1)
                                                                                                                     
      n2d = 0
      n3d = 0
      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             n2d = n2d + 1
         else
             n3d = n3d + 1
         endif
      enddo

      allocate( Lsurf(nvars) )
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

      return
      end subroutine read_hdf_meta

      subroutine read_hdf_data ( id,dp,ps,q2d,q3d,n2d,n3d,name2d,name3d,lprs,leta,
     .                           im,jm,lm,nymd,nhms,rc,ntime,ntimes,edges,ftype )
      implicit none
      include 'alias.com'

      integer   im,jm,lm,nymd,nhms,id,idpr,rc
      integer   n2d,n3d,ntime,ntimes
      real   ps(im,jm)
      real   dp(im,jm,lm)
      real  q2d(im,jm   ,n2d)
      real  q3d(im,jm,lm,n3d)
      character*256   name2d(n2d)
      character*256   name3d(n3d)
      character*256   ftype
      logical edges
      logical lprs,leta,zflip
      integer i,j,L,n

      real, allocatable :: pl(:,:,:)

         ps = -999
         dp = -999
      zflip = .false.
      
      rc = 0
      if( ntime <= ntimes ) then

c Search for Pressure Information
c -------------------------------
      if( lprs ) then
          do n=1,n2d
          if( match( c_ps,name2d(n) ) ) then
              call timebeg ('   GetVar')
	      call gfio_getvar ( id,trim(name2d(n)),nymd,nhms,im,jm,0, 1,ps,rc )          
	      call timeend ('   GetVar')
              if( rc.ne.0 ) then
                  rc = 1  ! No more time periods in file
                  return
              endif
          endif
          enddo

          edges = .false.

! First Priority:  Check for DP within File
! -----------------------------------------
          do n=1,n3d
              if( match( c_dp,name3d(n) ) ) then  ! 1st-Priority
                  call timebeg ('   GetVar')
	          call gfio_getvar ( id,trim(name3d(n)),nymd,nhms,im,jm,1,lm,dp,rc )
                  call timeend ('   GetVar')
                  if( dp(1,1,1).gt.dp(1,1,lm) ) then
                      dp(:,:,1:lm) = dp(:,:,lm:1:-1)
                      zflip = .true.
                  endif
              endif
          enddo

! Second Priority:  Check for PLE within File
! -------------------------------------------
          if( dp(1,1,1).eq.-999 ) then
          do n=1,n3d
              if( match( c_ple,name3d(n) ) ) then  ! 2nd-Priority
                  edges = .true.
                  call timebeg ('   GetVar')
                  call gfio_getvar ( id,trim(name3d(n)),nymd,nhms,im,jm,1,lm,dp,rc )
                  if( dp(1,1,1).gt.dp(1,1,lm) ) then
                      dp(:,:,1:lm) = dp(:,:,lm:1:-1)
                      zflip = .true.
                  endif
                  call timeend ('   GetVar')
                  ps(:,:)   = dp(:,:,lm)
              endif
          enddo
          endif

! Third Priority:  Check for PL within File
! -----------------------------------------
          if( dp(1,1,1).eq.-999 ) then
          do n=1,n3d
              if( match( c_pl,name3d(n) ) ) then  ! 3rd-Priority
                  allocate( pl(im,jm,lm) )
                  call timebeg ('   GetVar')
                  call gfio_getvar ( id,trim(name3d(n)),nymd,nhms,im,jm,1,lm,pl,rc )
                  call timeend ('   GetVar')
                  if( pl(1,1,1).gt.pl(1,1,lm) ) then
                      pl(:,:,1:lm) = pl(:,:,lm:1:-1)
                      zflip = .true.
                  endif
                  dp(:,:,lm) = 2*( ps(:,:)-pl(:,:,lm) )
                  do L=lm-1,1,-1
                  dp(:,:,L) = 2*( pl(:,:,L+1)-0.5*dp(:,:,L+1)-pl(:,:,L) )
                  enddo
                  deallocate( pl )
              endif
          enddo
          endif

	  do L=1,lm
          do j=1,jm
          do i=1,im
          if( ps(i,j).eq.-999 .or. dp(i,j,L).eq.-999 ) then
              print *, 'PS and DelP were not created!'
              stop
          endif
          enddo
          enddo
          enddo
      endif

c Collect Eta Data
c ----------------
      if( leta ) then
          call timebeg ('   GetVar')
          do n=1,n2d
          call gfio_getvar ( id,trim(name2d(n)),nymd,nhms,im,jm,0,1,q2d(1,1,n),rc )
          if( rc.ne.0 ) then
              rc = 1  ! No more time periods in file
              return
          endif
          enddo
          do n=1,n3d
          call gfio_getvar ( id,trim(name3d(n)),nymd,nhms,im,jm,1,lm,q3d(1,1,1,n),rc )
             if( zflip ) then
                 q3d(:,:,1:lm,n) = q3d(:,:,lm:1:-1,n)
             endif
             if( trim(name3d(n)).eq.'uwnd' .and. trim(ftype).eq.'ana' ) then
                 call dtoa ( q3d(1,1,1,n),q3d(1,1,1,n),im,jm,lm,2 )
             endif
             if( trim(name3d(n)).eq.'vwnd' .and. trim(ftype).eq.'ana' ) then
                 call dtoa ( q3d(1,1,1,n),q3d(1,1,1,n),im,jm,lm,1 )
             endif
          enddo
          call timeend ('   GetVar')
      endif

      else
        rc = 1  ! No more time periods in file
      endif

      return
      end subroutine read_hdf_data

      subroutine eta2prs ( dp,ps,ptop,q2d,q3d,name2d,titl2d,unit2d,mame3d,titl3d,unit3d,n2d,n3d,undef,
     .                     im,jm,lm,nt,plev,mlev,im_out,jm_out,latin,lonin,lonbeg,nymd,nhms,ninc,
     .                     id,hdf,create,filename,quad,edges,ftype,underg )
      use MAPL_ConstantsMod
      implicit none
      include 'alias.com'

c Input Variables
c ---------------
      integer  im,jm,lm,nt,mlev,im_out,jm_out,nymd,nhms,ninc,n2d,n3d
      integer  nymd0,nhms0

      real   dp(im,jm,lm)
      real   ps(im,jm)
      real  q2d(im,jm,   n2d)
      real  q3d(im,jm,lm,n3d)

      character*256  name2d(n2d), titl2d(n2d), unit2d(n2d)
      character*256  name3d(n3d), titl3d(n3d), unit3d(n3d)
      character*256  mame3d(n3d)
      character*256  filename
      character*256  ftype

      real   plev(mlev)  ! Target Pressures for Output
      real   ptop        ! Model Top Pressure
      real*8 lonbeg, dlon, dlat
      real*8 lat(jm_out),lon(im_out)
      real   latin(jm),lonin(im)
      logical hdf, create, quad, edges, underg

c Local Variables
c ---------------
      integer i,j,k,L,n,m
      real    logpe(im,jm,lm+1)
      real    logpl(im,jm,lm)
      real    logps(im,jm)
      real    logpt(im,jm)

      real, allocatable ::   pk (:,:,:)     ! Model pressure at edge-levels
      real, allocatable ::   pke(:,:,:)     ! Model pressure at edge-levels
      real, allocatable ::   pe(:,:,:)      ! Model pressure at edge-levels
      real, allocatable :: qprs(:,:,:)      ! Interpolated Quantity

      real    lats(jm_out),lons(im_out),levs(mlev)
      real    undef,eps,grav,cp,rgas,rvap
      integer precision,id,timeinc,rc,nhmsf

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact

      integer nvars
      integer i_u, i_v, i_t, i_tv, i_q, i_th, i_thv, i_phis, i_slp

      character*256, allocatable ::  vname(:) ,  vname2(:)
      character*256, allocatable :: vtitle(:) , vtitle2(:)
      character*256, allocatable :: vunits(:) , vunits2(:)
      integer,       allocatable ::  lmvar(:) ,  lmvar2(:)
      real,          allocatable :: vrange(:,:)
      real,          allocatable :: prange(:,:)

      real  pref,tref,pkref,dum,kappa
       

c Value-Added Products
c --------------------
      real, allocatable ::    up(:,:,:), uprime(:,:,:)
      real, allocatable ::    vp(:,:,:), vprime(:,:,:)
      real, allocatable ::    tp(:,:,:), tprime(:,:,:)
      real, allocatable ::    qp(:,:,:)
      real, allocatable ::    hp(:,:,:)
      real, allocatable ::  sphu(:,:,:)
      real, allocatable :: qstar(:,:,:)
      real, allocatable ::  tmpu(:,:,:)
      real, allocatable ::    rh(:,:,:)
      real, allocatable ::    th(:,:,:)
      real, allocatable ::    tv(:,:,:)
      real, allocatable ::   thv(:,:,:)
      real, allocatable ::   phi(:,:,:)
      real, allocatable ::  phis(:,:)
      real, allocatable ::   slp(:,:)
      real, allocatable ::   tpw(:,:)

      integer nuu, nvv, ntt, nqq, nuv, nut, nuq, nqs, nrh
      integer nvt, nvq, nupvp, nvptp, nh, ntp, nslp, ntpw

C **********************************************************************
C ****           Initialize Constants And Local Arrays              ****
C **********************************************************************
 
      call timebeg ('   Setup*')

      nvars = n2d + n3d
      kappa = MAPL_KAPPA
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      eps   = rvap/rgas-1.0

      allocate ( qstar(im,jm,mlev)  )
      allocate (  qprs(im,jm,mlev)  )
      allocate (    rh(im,jm,mlev)  )
      allocate (    qp(im,jm,mlev)  )

      allocate (    pe(im,jm,lm+1)  )
      allocate (    pk(im,jm,lm)    )
      allocate (    pke(im,jm,lm+1) )

c Load Variable Names into Local Temporary due to Analysis's Non-Conventional Notation
c ------------------------------------------------------------------------------------
      name3d = mame3d

C **********************************************************************
C ****                     Initialize GFIO File                     ****
C **********************************************************************
 
      allocate (    vname(nvars) )
      allocate (   vtitle(nvars) )
      allocate (   vunits(nvars) )
      allocate (    lmvar(nvars) )

      timeinc   = nhmsf(ninc)
      precision = 1  ! 64-bit
      precision = 0  ! 32-bit

      title    = 'GEOS-5 GCM (Pressure Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'mb'

c Defined Fields
c --------------
      do m=1,n2d
             n  = m
       vname(n) = name2d(m)
      vtitle(n) = trim(titl2d(m))
      vunits(n) = trim(unit2d(m))
       lmvar(n) =  0
      enddo

      do m=1,n3d

c Fix Analysis Non-Conventional Names
c -----------------------------------
      if( trim(ftype).eq.'ana' .and. trim(name3d(m)).eq.'theta' ) then
          name3d(m) = 'thetav'
          titl3d(m) = 'Scaled Virtual Potential Temperature'
      endif
             n  = n2d+m
       vname(n) = name3d(m)
      vtitle(n) = trim(titl3d(m))
      vunits(n) = trim(unit3d(m))
       lmvar(n) =  mlev
      enddo

C **********************************************************************
C ****                     Value Added Products                     ****
C **********************************************************************
 
      i_phis = 0
      i_u    = 0
      i_v    = 0
      i_t    = 0
      i_q    = 0
      i_th   = 0
      i_tv   = 0
      i_thv  = 0
      i_slp  = 0
      nqs    = 0
      ntp    = 0
      nrh    = 0
      nslp   = 0
      ntpw   = 0

      do n=1,n2d
         if( match( c_phis,name2d(n) ) ) i_phis = n
         if( match( c_slp ,name2d(n) ) ) i_slp  = n
      enddo
      do n=1,n3d
         if( match( c_u   ,name3d(n) ) ) i_u    = n
         if( match( c_v   ,name3d(n) ) ) i_v    = n
         if( match( c_t   ,name3d(n) ) ) i_t    = n
         if( match( c_q   ,name3d(n) ) ) i_q    = n
         if( match( c_th  ,name3d(n) ) ) i_th   = n
         if( match( c_tv  ,name3d(n) ) ) i_tv   = n
         if( match( c_thv ,name3d(n) ) ) i_thv  = n
      enddo

c Create Temperature (if possible)
c --------------------------------
      if( i_t .eq.0 .and.
     .  ( i_th.ne.0 .or. i_thv.ne.0 .or. i_tv.ne.0) ) then
          allocate ( tp(im,jm,mlev), tmpu(im,jm,lm) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             ntp = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'tmpu'
          if( i_th.ne.0 .or. ( i_thv.ne.0 .and. i_q.ne.0 ) .or. ( i_tv.ne.0 .and. i_q.ne.0 ) ) then
               vtitle(  nvars  ) = 'Temperature'
          else
               vtitle(  nvars  ) = 'Virtual Temperature'
          endif
         vunits(  nvars  ) = 'K'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c Create QSAT (if possible)
c -------------------------
      if( i_t .ne.0 .or. ntp.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nqs = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'qsat'
         vtitle(  nvars  ) = 'Saturation Specific Humidity'
         vunits(  nvars  ) = 'g/g'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c Create Relative Humidity (if possible)
c --------------------------------------
      if( nqs.ne.0 .and. i_q.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nrh = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'rh'
         vtitle(  nvars  ) = 'Relative Humidity'
         vunits(  nvars  ) = 'nondim'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

   
c uwnd*uwnd
c ---------
      if( quad .and. i_u.ne.0 ) then
          allocate ( up(im,jm,mlev), uprime(im,jm,mlev) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nuu = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'uu'
         vtitle(  nvars  ) = 'Quadratic UWND*UWND'
         vunits(  nvars  ) = 'm**2/s**2'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c vwnd*vwnd
c ---------
      if( quad .and. i_v.ne.0 ) then
          allocate ( vp(im,jm,mlev), vprime(im,jm,mlev) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nvv = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'vv'
         vtitle(  nvars  ) = 'Quadratic VWND*VWND'
         vunits(  nvars  ) = 'm**2/s**2'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c tmpu*tmpu
c ---------
      if( quad .and. ( i_t.ne.0 .or. ntp.ne.0) ) then
          if( .not. allocated(tp) ) allocate ( tp(im,jm,mlev) )
          allocate ( tprime(im,jm,mlev) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             ntt = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'tt'
         vtitle(  nvars  ) = 'Quadratic TMPU*TMPU'
         vunits(  nvars  ) = 'K**2'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c sphu*sphu
c ---------
      if( quad .and. i_q.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nqq = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'qq'
         vtitle(  nvars  ) = 'Quadratic SPHU*SPHU'
         vunits(  nvars  ) = 'nondim'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c uwnd*vwnd
c ---------
      if( quad .and. i_u.ne.0 .and. i_v.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nuv = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'uv'
         vtitle(  nvars  ) = 'Quadratic UWND*VWND'
         vunits(  nvars  ) = 'm**2/sec**2'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c uwnd*tmpu
c ---------
      if( quad .and. i_u.ne.0 .and. ( i_t.ne.0 .or. ntp.ne.0) ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nut = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'ut'
         vtitle(  nvars  ) = 'Quadratic UWND*TMPU'
         vunits(  nvars  ) = 'K m/sec'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c uwnd*sphu
c ---------
      if( quad .and. i_u.ne.0 .and. i_q.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nuq = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'uq'
         vtitle(  nvars  ) = 'Quadratic UWND*SPHU'
         vunits(  nvars  ) = 'm/sec'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c vwnd*tmpu
c ---------
      if( quad .and. i_v.ne.0 .and. ( i_t.ne.0 .or. ntp.ne.0 ) ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nvt = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'vt'
         vtitle(  nvars  ) = 'Quadratic VWND*TMPU'
         vunits(  nvars  ) = 'K m/sec'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c vwnd*sphu
c ---------
      if( quad .and. i_v.ne.0 .and. i_q.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
             nvq = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'vq'
         vtitle(  nvars  ) = 'Quadratic VWND*SPHU'
         vunits(  nvars  ) = 'm/sec'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c uprime*vprime
c -------------
      if( quad .and. i_u.ne.0 .and. i_v.ne.0 ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
           nupvp = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'upvp'
         vtitle(  nvars  ) = 'Quadratic UPRIME*VPRIME (Departure from zonal mean)'
         vunits(  nvars  ) = 'm**2/sec**2'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c vprime*tprime
c -------------
      if( quad .and. i_v.ne.0 .and. ( i_t.ne.0 .or. ntp.ne.0 ) ) then
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
           nvptp = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'vptp'
         vtitle(  nvars  ) = 'Quadratic VPRIME*TPRIME (Departure from zonal mean)'
         vunits(  nvars  ) = 'K m/sec'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
      endif

c heights
c -------
      if( i_phis.ne.0 .and.
     .              ( (i_t .ne.0 .and. i_q.ne.0) .or.
     .                (i_tv.ne.0 .and. i_q.ne.0) .or.
     .                (i_th.ne.0 .and. i_q.ne.0) .or. i_thv.ne.0 ) ) then
          allocate ( phis(im,jm), hp(im,jm,mlev) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
              nh = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'hght'
         vtitle(  nvars  ) = 'Heights'
         vunits(  nvars  ) = 'm'
          lmvar(  nvars  ) =  mlev
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )

         if( i_slp.eq.0 ) then
          allocate ( slp(im,jm) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
           nslp  = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'slp'
         vtitle(  nvars  ) = 'Sea-Level Pressure'
         vunits(  nvars  ) = 'Pa'
          lmvar(  nvars  ) =  0
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
         endif

      endif

c tpw
c ---
         if( i_q.ne.0 ) then
          allocate ( tpw(im,jm) )
          allocate ( vname2(nvars), vtitle2(nvars), vunits2(nvars), lmvar2(nvars) )
          vname2 = vname
         vtitle2 = vtitle
         vunits2 = vunits
          lmvar2 = lmvar
           nvars = nvars + 1
           ntpw  = nvars
         deallocate ( vname, vtitle, vunits, lmvar )
           allocate ( vname(nvars), vtitle(nvars), vunits(nvars), lmvar(nvars) )
          vname(1:nvars-1) =  vname2
         vtitle(1:nvars-1) = vtitle2
         vunits(1:nvars-1) = vunits2
          lmvar(1:nvars-1) =  lmvar2
          vname(  nvars  ) = 'tpw'
         vtitle(  nvars  ) = 'Total Precipitable Water Vapor'
         vunits(  nvars  ) = 'kg m-2'
          lmvar(  nvars  ) =  0
         deallocate ( vname2, vtitle2, vunits2, lmvar2 )
         endif

c Compute grid
c ------------
      if( im.eq.im_out .and. jm.eq.jm_out ) then
          lat = latin
          lon = lonin
      else
          dlon = 360.0/ im_out
          dlat = 180.0/(jm_out-1)

          do j=1,jm_out
          lat(j) = -90.0 + (j-1)*dlat
          enddo
          do i=1,im_out
          lon(i) =  lonbeg + (i-1)*dlon
          enddo
      endif

      lons = lon
      lats = lat
      levs(:) = plev(:)/100

c Create GFIO file
c ----------------
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
      vrange(:,:) = undef
      prange(:,:) = undef

      if (create) then
         if (hdf) then
             call GFIO_Create ( trim(filename), title, source, contact, undef,
     .                          im_out, jm_out, mlev, lons, lats, levs, levunits,
     .                          nymd, nhms, timeinc,
     .                          nvars, vname, vtitle, vunits, lmvar,
     .                          vrange, prange, precision,
     .                          id, rc )
         else
             open (55,file=trim(filename),form='unformatted',access='sequential')
         endif
      endif

      call timeend ('   Setup*')

C **********************************************************************
C ****                  Compute edge-level pressures                ****
C **********************************************************************

      call timebeg ('   Pkappa*')
      if( .not.edges) then
           if( ps(1,1).ne.0.0 ) then
           pe(:,:,1) = ptop
           do L=1,lm-1
           pe(:,:,L+1) = pe(:,:,L) + dp(:,:,L)
           enddo
           pe(:,:,lm+1) = ps(:,:)

           logpe = log(pe)
           pke   = pe**kappa

c Compute mid-level pressures to KAPPA
c ------------------------------------
           do L=1,lm
           pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )/( kappa*log(pe(:,:,L+1)/pe(:,:,L)) )
           enddo
           endif
      endif
      call timeend ('   Pkappa*')

c Compute Temperature (if needed)
c -------------------------------
      if( ntp.ne.0 ) then
      call timebeg ('   TMPU*')
             allocate(   tv(im,jm,lm) )
             allocate(   th(im,jm,lm) )
             allocate(  thv(im,jm,lm) )
             allocate( sphu(im,jm,lm) )
                       sphu = 0
      do n=1,n3d
         if( n.eq.i_q   ) sphu = q3d(:,:,:,n)
         if( n.eq.i_th  ) th   = q3d(:,:,:,n)
         if( n.eq.i_tv  ) tv   = q3d(:,:,:,n)
         if( n.eq.i_thv ) thv  = q3d(:,:,:,n)
      enddo
         if( i_thv.ne.0 ) then
             tmpu = thv*pk/(1+eps*sphu)
         else if ( i_th.ne.0 ) then
             tmpu = th*pk
         else if ( i_tv.ne.0 ) then
             tmpu = tv/(1+eps*sphu)
         endif

           deallocate( tv,th,thv,sphu )
      call timeend ('   TMPU*')
      endif

C **********************************************************************
C ****                    Write Defined Fields                      ****
C **********************************************************************

      call timebeg ('   Logs*')
      if( n3d.ne.0  ) then
          if( edges ) then
              logpl(:,:,:) = log( dp(:,:,: ) )
              logps(:,:)   = log( dp(:,:,lm) )
              logpt(:,:)   = log( dp(:,:,1 ) )
          else
              do L=1,lm
              do j=1,jm
              do i=1,im
              logpl(i,j,L) = log( 0.5*( pe(i,j,L+1)+pe(i,j,L) ) )
              enddo
              enddo
              enddo
              logps(:,:) = log( pe(:,:,lm+1) )
              logpt(:,:) = log( pe(:,:,1) )
          endif
      endif
      call timeend ('   Logs*')

      call timebeg ('   Q2D*')
      do n=1,n2d
         call writit( q2d(1,1,n),im,jm,im_out,jm_out,1,id,name2d(n),nymd,nhms,undef,hdf )
         if( n.eq.i_phis .and. allocated(phis) ) phis = q2d(:,:,n)
      enddo
      call timeend ('   Q2D*')

      call timebeg ('   Q3D*')
      do n=1,n3d
         do L=1,mlev
         call  sigtopl( qprs(1,1,L),q3d(1,1,1,n),logpl,logps,logpt,log(plev(L)),im,jm,lm,undef,underg,0 )
         enddo
         call writit( qprs,im,jm,im_out,jm_out,mlev,id,name3d(n),nymd,nhms,undef,hdf )
         if( n.eq.i_u ) then
             if( .not. allocated(up) ) allocate ( up(im,jm,mlev) )
             up = qprs
         endif
         if( n.eq.i_v ) then
             if( .not. allocated(vp) ) allocate ( vp(im,jm,mlev) )
             vp = qprs
         endif
         if( n.eq.i_t ) then
             if( .not. allocated(tp) ) allocate ( tp(im,jm,mlev) )
             tp = qprs
         endif
         if( n.eq.i_q ) then
             if( .not. allocated(qp) ) allocate ( qp(im,jm,mlev) )
             qp = qprs
         endif
      enddo
      call timeend ('   Q3D*')

      if( ntp.ne.0 ) then
      call timebeg ('   TMPU*')
         do L=1,mlev
         call  sigtopl( tp(1,1,L),tmpu(1,1,1),logpl,logps,logpt,log(plev(L)),im,jm,lm,undef,underg,0 )
         enddo
         call writit( tp,im,jm,im_out,jm_out,mlev,id,vname(ntp),nymd,nhms,undef,hdf )
      call timeend ('   TMPU*')
      endif

C **********************************************************************
C ****                      Compute RH and QSAT                     ****
C **********************************************************************

      if( nqs.ne.0 ) then
      call timebeg ('   QSAT*')
         do L=1,mlev
         do j=1,jm
         do i=1,im
         if( tp(i,j,L).ne.undef ) then
             call qsat (tp(i,j,L),plev(L)*0.01,qstar(i,j,L),dum,.false.)
         else
             qstar(i,j,L) = undef
         endif
         enddo
         enddo
         enddo
         call writit( qstar,im,jm,im_out,jm_out,mlev,id,vname(nqs),nymd,nhms,undef,hdf )

         if( nrh.ne.0 ) then
         do L=1,mlev
         do j=1,jm
         do i=1,im
         if( qp(i,j,L).ne.undef .and. qstar(i,j,L).ne.undef ) then
             rh(i,j,L) = qp(i,j,L)/qstar(i,j,L)
         else
             rh(i,j,L) = undef
         endif
         enddo
         enddo
         enddo
         call writit( rh,im,jm,im_out,jm_out,mlev,id,vname(nrh),nymd,nhms,undef,hdf )
         endif

      call timeend ('   QSAT*')
      endif

C **********************************************************************
C ****                       Compute Heights                        ****
C **********************************************************************

      call timebeg ('   HGHTs*')
      do n=1,n3d
         if( n.eq.i_t   ) then
             allocate( tmpu(im,jm,lm) )
                       tmpu = q3d(:,:,:,n)
         endif
         if( n.eq.i_q   ) then
             allocate( sphu(im,jm,lm) )
                       sphu = q3d(:,:,:,n)
         endif
         if( n.eq.i_th  ) then
             allocate( th(im,jm,lm) )
                       th = q3d(:,:,:,n)
         endif
         if( n.eq.i_tv  ) then
             allocate( tv(im,jm,lm) )
                       tv = q3d(:,:,:,n)
         endif
         if( n.eq.i_thv ) then
             allocate( thv(im,jm,lm) )
                       thv = q3d(:,:,:,n)
         endif
      enddo

      if( allocated(hp) ) then
          allocate( phi(im,jm,lm+1) )
          phi(:,:,lm+1) = phis(:,:)
          if( allocated(thv) ) then
              print *, '       Method 1 for Heights: Cp*THV'
              do L=lm,1,-1
              phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
              enddo
              phi(:,:,:) = phi(:,:,:)/grav
              do L=1,mlev
c             call intgeop ( hp(1,1,L),thv,pke(1,1,2),pk,phis,plev(L),undef,im,jm,lm )
              call sigtopl ( hp(1,1,L),phi,logpe,logps,logpt,log(plev(L)),im,jm,lm+1,undef,underg,1 )
              enddo
          else if( allocated(th) .and. allocated(sphu) ) then
              print *, '       Method 2 for Heights: Cp*TH*(1+eps*QV)'
              allocate( thv(im,jm,lm) )
              thv = th*(1+eps*sphu)
              do L=lm,1,-1
              phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
              enddo
              phi(:,:,:) = phi(:,:,:)/grav
              do L=1,mlev
c             call intgeop ( hp(1,1,L),thv,pke(1,1,2),pk,phis,plev(L),undef,im,jm,lm )
              call sigtopl ( hp(1,1,L),phi,logpe,logps,logpt,log(plev(L)),im,jm,lm+1,undef,underg,1 )
              enddo
          else if( allocated(tmpu) .and. allocated(sphu) ) then
              print *, '       Method 3 for Heights: Cp*T*(1+eps*QV)/PK'
              allocate( thv(im,jm,lm) )
              thv = tmpu*(1+eps*sphu)/pk
              do L=lm,1,-1
              phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
              enddo
              phi(:,:,:) = phi(:,:,:)/grav
              do L=1,mlev
c             call intgeop ( hp(1,1,L),thv,pke(1,1,2),pk,phis,plev(L),undef,im,jm,lm )
              call sigtopl ( hp(1,1,L),phi,logpe,logps,logpt,log(plev(L)),im,jm,lm+1,undef,underg,1 )
              enddo
          else if( allocated(tv) .and. allocated(sphu) ) then
              print *, '       Method 4 for Heights: Cp*Tv*/PK'
              allocate( thv(im,jm,lm) )
              thv = tv/pk
              do L=lm,1,-1
              phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
              enddo
              phi(:,:,:) = phi(:,:,:)/grav
              do L=1,mlev
c             call intgeop ( hp(1,1,L),thv,pke(1,1,2),pk,phis,plev(L),undef,im,jm,lm )
              call sigtopl ( hp(1,1,L),phi,logpe,logps,logpt,log(plev(L)),im,jm,lm+1,undef,underg,1 )
              enddo
          endif
          deallocate( phi )

          if( nslp.ne.0 ) then
              call get_slp ( ps,phis,slp,pe,pk,thv,rgas,grav,im,jm,lm )
          endif

      endif
      call timeend ('   HGHTs*')


C **********************************************************************
C ****                Write value Added Products                    ****
C **********************************************************************

      call timebeg ('   QUAD*')
      if( quad .and. allocated(up) ) call zprime ( up,uprime,im,jm,mlev,undef )
      if( quad .and. allocated(vp) ) call zprime ( vp,vprime,im,jm,mlev,undef )
      if( quad .and. allocated(tp) ) call zprime ( tp,tprime,im,jm,mlev,undef )

      if( quad .and. allocated(up) ) 
     .       call writit2( up,    up,    im,jm,im_out,jm_out,mlev,id,vname(nuu  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(vp) ) 
     .       call writit2( vp,    vp,    im,jm,im_out,jm_out,mlev,id,vname(nvv  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(tp) ) 
     .       call writit2( tp,    tp,    im,jm,im_out,jm_out,mlev,id,vname(ntt  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(qp) ) 
     .       call writit2( qp,    qp,    im,jm,im_out,jm_out,mlev,id,vname(nqq  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(up)  .and.
     .               allocated(vp) ) 
     .       call writit2( up,    vp,    im,jm,im_out,jm_out,mlev,id,vname(nuv  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(up)  .and.
     .               allocated(tp) ) 
     .       call writit2( up,    tp,    im,jm,im_out,jm_out,mlev,id,vname(nut  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(up)  .and.
     .               allocated(qp) ) 
     .       call writit2( up,    qp,    im,jm,im_out,jm_out,mlev,id,vname(nuq  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(vp)  .and.
     .               allocated(tp) ) 
     .       call writit2( vp,    tp,    im,jm,im_out,jm_out,mlev,id,vname(nvt  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(vp)  .and.
     .               allocated(qp) ) 
     .       call writit2( vp,    qp,    im,jm,im_out,jm_out,mlev,id,vname(nvq  ),nymd,nhms,undef,hdf )
      if( quad .and. allocated(up)  .and.
     .               allocated(vp) ) 
     .       call writit2( uprime,vprime,im,jm,im_out,jm_out,mlev,id,vname(nupvp),nymd,nhms,undef,hdf )
      if( quad .and. allocated(vp)  .and.
     .               allocated(tp) ) 
     .       call writit2( vprime,tprime,im,jm,im_out,jm_out,mlev,id,vname(nvptp),nymd,nhms,undef,hdf )

      if( allocated(hp)  ) call writit (  hp,im,jm,im_out,jm_out,mlev,id,vname(nh)  ,nymd,nhms,undef,hdf )
      if( allocated(slp) ) call writit(  slp,im,jm,im_out,jm_out,1   ,id,vname(nslp),nymd,nhms,undef,hdf )

      if( allocated(tpw) ) then
          tpw = 0
          do L=1,lm
          tpw = tpw + q3d(:,:,L,i_q)*dp(:,:,L)
          enddo
          tpw = tpw / grav
          call writit( tpw,im,jm,im_out,jm_out,1,id,vname(ntpw),nymd,nhms,undef,hdf )
      endif

      call timeend ('   QUAD*')

C **********************************************************************
C ****                  De-Allocate Dynamics Arrays                 ****
C **********************************************************************

      if( allocated(up) ) deallocate (up)
      if( allocated(vp) ) deallocate (vp)
      if( allocated(tp) ) deallocate (tp)
      if( allocated(hp) ) deallocate (hp)
      if( allocated(qp) ) deallocate (qp)

      if( allocated(uprime) ) deallocate (uprime)
      if( allocated(vprime) ) deallocate (vprime)
      if( allocated(tprime) ) deallocate (tprime)

      if( allocated(phis) ) deallocate (phis )
      if( allocated(tmpu) ) deallocate (tmpu )
      if( allocated(sphu) ) deallocate (sphu )
      if( allocated(th  ) ) deallocate (th   )
      if( allocated(tv  ) ) deallocate (tv   )
      if( allocated(thv ) ) deallocate (thv  )
      if( allocated(slp ) ) deallocate (slp  )
      if( allocated(tpw ) ) deallocate (tpw  )

      deallocate (pk,pe,pke)
      deallocate (    qprs )
      deallocate (   qstar )
      deallocate (      rh )
      deallocate (   vname )
      deallocate (  vtitle )
      deallocate (  vunits )
      deallocate (   lmvar )
      deallocate (  vrange )
      deallocate (  prange )
      return
      end subroutine eta2prs

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

      subroutine sigtopl ( qprs,q,logpl,logps,logpt,logp,im,jm,lm,undef,underg,flag )
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
      use MAPL_ConstantsMod
      implicit none
      integer  i,j,l,im,jm,lm,flag

      real  qprs(im,jm)
      real  q   (im,jm,lm)
      real logpl(im,jm,lm)
      real logps(im,jm)
      real logpt(im,jm)

      real  p,undef,kappa
      real  logp,logpmin,logpmax,temp
      logical underg

      kappa = MAPL_KAPPA
     
      call timebeg ('   SigToP')
c Initialize to UNDEFINED
c -----------------------
      do j=1,jm
      do i=1,im
      qprs(i,j) = undef
      enddo
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

      do j=1,jm
      do i=1,im

c Extrapolate to Pressure between Ptop and First Model Level
c ----------------------------------------------------------
      if( logp.le.logpl(i,j,1) .and. logp.ge.logpt(i,j) ) then
          temp  = ( logpl(i,j,1)-logp ) / ( logpl(i,j,1)-logpl(i,j,2) )
          if( q(i,j,1).ne.undef .and. q(i,j,2).ne.undef ) then
             qprs(i,j) = q(i,j,2)*temp + q(i,j,1)*(1.-temp)
          else if( q(i,j,1).ne.undef ) then
             qprs(i,j) = q(i,j,1)
          endif
      endif

c Extend or Extrapolate Lowest Level Values to the Surface or below
c -----------------------------------------------------------------
      if( logp.gt.logpl(i,j,lm) ) then
          if( logp.le.logps(i,j) .or. underg ) then
              if( flag.eq.0 ) then 
                  qprs(i,j) = q(i,j,lm)
              else
                  temp  = ( logpl(i,j,lm-1)-logp ) / ( logpl(i,j,lm-1)-logpl(i,j,lm) )
                  qprs(i,j) = q(i,j,lm)*temp + q(i,j,lm-1)*(1.-temp)
              endif
          endif
      endif

      enddo
      enddo

      call timeend ('   SigToP')
      return
      end subroutine sigtopl

      subroutine zprime ( a,ap,im,jnp,lm,undef )
      real     a(im,jnp,lm), ap(im,jnp,lm), az(jnp)
                                                                                                              
      call timebeg ('   Zprime')
      DO L=1,LM
      DO J=1,JNP
      AZ(J) = 0.0
      IC    = 0
      DO   I = 1, IM
      IF( abs(A(I,J,L)-UNDEF).gt.0.1 )  THEN
      AZ(J) = AZ(J) + A(I,J,L)
      IC    = IC + 1
      ENDIF
      ENDDO
      IF(IC.NE.0) AZ(J) = AZ(J) / IC
      IF(IC.eq.0) AZ(J) = UNDEF
      ENDDO
                                                                                                              
      do j=1,jnp
      do i=1,im
      if( abs(a(i,j,L)-undef).gt.0.1 ) then
      ap(i,j,L) = a(i,j,L)-az(j)
      else
      ap(i,j,L) = undef
      endif
      enddo
      enddo
      ENDDO
      call timeend ('   Zprime')
                                                                                                              
      return
      end subroutine zprime
 
      subroutine writit (q,im,jm,im_out,jm_out,lm,id,name,nymd,nhms,undef,hdf)
      integer im,jm,lm,L
      integer id,nymd,nhms,rc,lbeg
      character*256 name
      logical hdf
      real    q (im,jm,lm), qout(im_out,jm_out,lm)
      real    undef
                    lbeg = 1
      if( lm.eq.1 ) lbeg = 0
      print *, '       Writing variable: ',trim(name)
      if( im.eq.im_out .and. jm.eq.jm_out ) then
          qout = q
      else
          call hinterp ( q,im,jm,qout,im_out,jm_out,lm,undef )
      endif
      call timebeg ('   PutVar')
      if(hdf) then
         call Gfio_putVar ( id,trim(name),nymd,nhms,im_out,jm_out,lbeg,lm,qout,rc )
      else
         do L=lm,1,-1
         write(55) qout(:,:,L) 
         enddo
      endif
      call timeend ('   PutVar')
      return
      end subroutine writit

      subroutine writit2 (q1,q2,im,jm,im_out,jm_out,lm,id,name,nymd,nhms,undef,hdf)
      integer im,jm,lm,L
      integer id,nymd,nhms,rc,lbeg
      character*256 name
      logical hdf
      real   q1(im,jm,lm),q2(im,jm,lm),qout(im_out,jm_out,lm)
      real   qq(im,jm,lm)
      real   undef
      do L=1,lm
      do j=1,jm
      do i=1,im
         if( q1(i,j,L).ne.undef .and. q2(i,j,L).ne.undef ) then
             qq(i,j,L) = q1(i,j,L)*q2(i,j,L)
         else
             qq(i,j,L) = undef
         endif
      enddo
      enddo
      enddo
                    lbeg = 1
      if( lm.eq.1 ) lbeg = 0
      print *, '       Writing variable: ',trim(name)
      if( im.eq.im_out .and. jm.eq.jm_out ) then
          qout = qq
      else
          call hinterp ( qq,im,jm,qout,im_out,jm_out,lm,undef )
      endif
      call timebeg ('   PutVar')
      if(hdf) then
         call Gfio_putVar ( id,trim(name),nymd,nhms,im_out,jm_out,lbeg,lm,qout,rc )
      else
         do L=lm,1,-1
         write(55) qout(:,:,L) 
         enddo
      endif
      call timeend ('   PutVar')
      return
      end subroutine writit2

      subroutine intgeop (PHI,THB,PKE,PKO,PHISS,PSTR,UNDEF,im,jm,lm)
C***********************************************************************        
C                                                                               
C INPUT                                                                         
C   THB .... POTENTIAL TEMPERATURE (T/PKO) AT FULL LEVELS                       
C   PKE .... P TO THE KAPPA AT THE BASE OF EACH LAYER                           
C   PKO .... P TO THE KAPPA AT FULL LEVELS                                      
C   PHISS .. SURFACE GEOPOTENTIAL                                               
C   PSTR ... OUTPUT PRESSURE LEVEL (SCALAR) IN MB                               
C   UNDEF .. UNDEFINED VALUE                                                    
C   TEM .... SCRATCH ARRAY                                                      
C                                                                               
C OUTPUT                                                                        
C   PHI .... GEOPOTENTIAL HEIGHT (M) AT PSTR                                    
C                                                                               
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      use MAPL_ConstantsMod
      implicit none
      integer im,jm,lm

      real THB(im*jm,lm), PKE(im*jm,lm), PKO(im*jm,lm)
      real PHI(im*jm),  PHISS(im*jm)   , TEM(im*jm,5)                

      real, PARAMETER :: HALF=.5
      real, PARAMETER ::  ONE=1.
      real, PARAMETER ::  TWO=2.
      real g,cp,akp,pstr,pkstr,undef,pkmin,pkmax
      integer i,L
                                                                                
      G   = MAPL_GRAV
      CP  = MAPL_CP
      AKP = MAPL_KAPPA
                                                                                
      PKSTR  = PSTR ** AKP                                                      
      DO 10 I=1,im*jm                                                           
      PHI(I) = UNDEF                                                            
   10 CONTINUE                                                                  
                                                                                
      PKMIN = PKO(1,lm)                                                      
      DO 20 I=2,im*jm                                                           
      IF( PKO(I,lm).LT.PKMIN ) PKMIN = PKO(I,lm)                          
   20 CONTINUE                                                                  
                                                                                
      IF( PKSTR.GE.PKMIN ) THEN                                                 
      DO 30 I=1,im*jm                                                           
      IF (PKSTR .LE. PKE(I,lm) .AND. PKSTR .GE. PKO(I,lm)) THEN           
      PHI(I) = PHISS(I) + (PKE(I,lm)-PKSTR) * THB(I,lm)*CP                
      ENDIF                                                                     
   30 CONTINUE                                                                  
      ENDIF                                                                     
                                                                                
      DO 40 I=1,im*jm                                                           
      TEM(I,5) = PHISS(I)                                                       
     .         +  (PKE(I,lm)-PKO(I,lm)) * THB(I,lm)*CP                 
   40 CONTINUE                                                                  
                                                                                
      DO 310 L=lm,2,-1                                                       
                                                                                
      PKMAX = PKO(1,L)                                                          
      DO 50 I=2,im*jm                                                           
      IF( PKO(I,L).GT.PKMAX ) PKMAX = PKO(I,L)                                  
   50 CONTINUE                                                                  
      IF( PKSTR.GE.PKMAX ) GO TO 320                                            
                                                                                
      DO 60 I=1,im*jm                                                           
      TEM(I,1) = THB(I,L-1)*(PKE(I,L-1)-PKO(I,L-1))                             
     .         + THB(I,L  )*(PKO(I,L  )-PKE(I,L-1))                             
                                                                                
      IF( PKSTR.GE.PKE(I,L-1) ) THEN                                            
      TEM(I,3) = PKO(I,L)                                                       
      TEM(I,2) = TEM(I,5)                                                       
      TEM(I,4) = THB(I,L)                                                       
      ENDIF                                                                     
                                                                                
      TEM(I,5) = TEM(I,5) + TEM(I,1) * CP                                       
                                                                                
      IF( PKSTR.LT.PKE(I,L-1) ) THEN                                            
      TEM(I,3) = PKO(I,L-1)                                                     
      TEM(I,2) = TEM(I,5  )                                                     
      TEM(I,4) = THB(I,L-1)                                                     
      ENDIF                                                                     
                                                                                
      IF( PKSTR.GE.PKO(I,L-1) .AND. PKSTR.LT.PKO(I,L) ) THEN                    
      PHI(I) = TEM(I,2)                                                         
      ENDIF                                                                     
                                                                                
      IF(L.EQ.2) THEN                                                           
      IF( PKSTR.LT.PKO(I,1) ) PHI(I) = TEM(I,2)                                 
      ENDIF                                                                     
                                                                                
      TEM(I,1) =  TEM(I,1)        / (PKO(I,L)-PKO(I,L-1))                       
      TEM(I,2) = (TEM(I,3)-PKSTR) / (TEM(I,3)-PKE(I,L-1))                       
                                                                                
      TEM(I,1) =  TEM(I,1) *      TEM(I,2)                                      
     .         +  TEM(I,4) * (TWO-TEM(I,2))                                     
      TEM(I,1) =  TEM(I,1) * (TEM(I,3)-PKSTR)                                   
                                                                                
      IF( PKSTR.GE.PKO(I,L-1) .AND. PKSTR.LT.PKO(I,L)) THEN                     
      PHI(I) = PHI(I) + TEM(I,1) * (CP*HALF)                                    
      ENDIF                                                                     
                                                                                
      IF(L.EQ.2) THEN                                                           
      IF( PKSTR.LT.PKO(I,1) ) PHI(I) = PHI(I) + TEM(I,1)*(CP*HALF)              
      ENDIF                                                                     
                                                                                
   60 CONTINUE                                                                  
  310 CONTINUE                                                                  
  320 CONTINUE                                                                  
                                                                                
      DO 70 I=1,im*jm                                                           
      IF( PHI(I).NE.UNDEF ) PHI(I) = PHI(I) * (ONE/G)                           
   70 CONTINUE                                                                  
      RETURN                                                                    
      end subroutine intgeop

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
      phi_jm2 =  lat_cmp( min(max(1,jm2(i)),jm) )
      phi_jm1 =  lat_cmp( min(max(1,jm1(i)),jm) )
      phi_jp0 =  lat_cmp( min(max(1,jp0(i)),jm) )
      phi_jp1 =  lat_cmp( min(max(1,jp1(i)),jm) )

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

      subroutine usage()
      write(6,100)
 100  format(  "Usage:  "                                                                                               ,/
     .                                                                                                                  ,/
     .         " eta2prs_$ARCH.x -levs  plev(s)"                                                                        ,/
     .         "                 -eta   eta_fname(s)"                                                                   ,/
     .                                                                                                                  ,/
     .         "                [-prs   prs_fname(s)]"                                                                  ,/
     .         "                [-nymd  nymd]"                                                                          ,/
     .         "                [-nhms  nhms]"                                                                          ,/
     .         "                [-nymdb nymdb]"                                                                         ,/
     .         "                [-nhmsb nhmsb]"                                                                         ,/
     .         "                [-ndt   ndt]"                                                                           ,/
     .         "                [-im    im_out]"                                                                        ,/
     .         "                [-jm    jm_out]"                                                                        ,/
     .         "                [-ptop  ptop]"                                                                          ,/
     .         "                [-hdf   Logical HDF Flag]"                                                              ,/
     .         "                [-tag   output_tag] "                                                                   ,/
     .         "                [-underg ]"                                                                             ,/
     .         "                [-ana    ]"                                                                             ,/
     .         "                [-noquad ]"                                                                             ,/
     .                                                                                                                  ,/
     .         "where:"                                                                                                 ,/
     .                                                                                                                  ,/
     .         "  -levs     plev(s):  Pressure   Level(s)  for output file (bottom to top, i.e: 1000 850 700  etc)"     ,/
     .         "  -eta eta_fname(s):  Filename(s) in eta   HDF format"                                        ,/
     .                                                                                                                  ,/
     .         "Optional Args:"                                                                                         ,/
     .                                                                                                                  ,/
     .         "  -prs  prs_fname(s):  Filename(s) in HDF format containing pressure information"                       ,/
     .                                                                                                                  ,/
     .         "  -nymd         nymd:  Optional Date YYYYMMDD  for Internal Metadata on Output"                         ,/
     .         "  -nhms         nhms:  Optional Time   HHMMSS  for Internal Metadata on Output"                         ,/
     .         "  -nymdb       nymdb:  Optional Date YYYYMMDD  to  begin processing  of Input"                                    ,/
     .         "  -nhmsb       nhmsb:  Optional Time   HHMMSS  to  begin processing  of Input"                                    ,/
     .         "  -ndt           ndt:  Optional Time Increment to  Override HDF default"                                ,/
     .         "  -tag    output_tag:  Optional Filename Tag for output file (default: eta2prs)"                        ,/
     .         "  -im         im_out:  Optional Output Resolution in X (default: Input Resolution)"                     ,/
     .         "  -jm         jm_out:  Optional Output Resolution in Y (default: Input Resolution)"                     ,/
     .         "  -ptop         ptop:  Optional PTOP variable used for PLE calculatoin (default: PTOP=0.01 mb)"         ,/
     .                                                                                                                  ,/
     .         "  -underg           :  Optional flag to enable extrapolation underground (default: off)"                ,/
     .         "  -ana              :  Used to process ana.eta and bkg.eta files from analysis"                         ,/
     .         "  -noquad           :  Implies No Computation of Quadratics (default: quadratics are computed )"        ,/
     .         "  -hdf              :  Logical Flag for HDF Output (default: .TRUE., .FALSE. implies flat binary file)" ,/
     .         ) 
      call exit(7)
      end subroutine usage
      subroutine get_slp ( ps,phis,slp,pe,pk,thv,rgas,grav,im,jm,km )
      implicit   none
      integer  im,jm,km
      real     grav
      real     rgas
      real     pk(im,jm,km)    ! layer-mean P**kappa
      real    thv(im,jm,km)    ! layer-mean virtual Temperature
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

      tv = thv*pk
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
      IF(D.LE.0.) THEN
       Q = 1.0
       IF (LDQDT)  DQDT = 0.
      ELSE
       D = 1.0 / D
       Q = MIN(QX * D,1.0)
       IF (LDQDT)  DQDT = (1.0 + ERFAC*Q) * D * DQX
      ENDIF
      RETURN
      END

