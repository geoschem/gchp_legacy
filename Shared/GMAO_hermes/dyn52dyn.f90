      program  main
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****        Program to create bkg_eta and bkg_sfc files for       ****
! ****             Analysis from GCM HISTORY output                 ****
! ****                                                              ****
! ****  HISTORY:                                                    ****
! ****  16Dec2004  Takacs   Changed thv to th for GEOS-5 GCM        ****
! ****  10Nov2004  Todling  Added handle for existing hdf files     ****
! ****  20Apr2005  Takacs   Read hdf files from GCM HISTORY         ****
! ****  29Apr2005  Todling  Using set_eta from hermes; this sets ks ****
! ****                        and pint properly.                    ****
! ****  23Jun2005  Todling  Added freq/changed timinc def(asyn-omf) ****
! **********************************************************************
! **********************************************************************


      character*80  tmpsfc, tmpeta
      character*80  bkgsfc, bkgeta
      character*80  expid
      character*10  str

      integer nymd,nhms
      integer im,jm,lm
      integer iprec,precision
      real    undef,rgas,rvap,eps,ptop,kappa

! hdf variables
! -------------
      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

      real,    allocatable ::   vars(:,:,:)
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
      integer, allocatable ::    loc(:)

      character*120, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,i,j,L
      integer id,rc,timinc,nmax,kbeg,kend,freq
      integer ntime,nvars,ngatts

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

       expid = 'geos5'
      bkgeta = 'NONE'
      bkgsfc = 'NONE'
   precision = 1  ! default: 64-bit output
      freq   = 6  ! default: frequency of bkg is 6-hrs


         nargs = iargc()
      if(nargs==0) call usage()
      allocate ( arg(nargs) )
      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
             if( trim(arg(n)).eq.'-h'        ) call usage()
             if( trim(arg(n)).eq.'-help'     ) call usage()
             if( trim(arg(n)).eq.'-H'        ) call usage()
             if( trim(arg(n)).eq.'-Help'     ) call usage()
             if( trim(arg(n)).eq.'-expid'    ) then
                                   expid   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-tmpeta'   ) then
                                   tmpeta  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-tmpsfc'   ) then
                                   tmpsfc  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-bkgeta'   ) then
                                   bkgeta  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-bkgsfc'   ) then
                                   bkgsfc  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-prec'    ) then
                                   str     = trim(arg(n+1))
                                   read(str,*) iprec
                                   if(iprec==32)precision=0
                                   if(iprec==64)precision=1
             endif
             if( trim(arg(n)).eq.'-freq'    ) then
                                   str     = trim(arg(n+1))
                                   read(str,*) freq
             endif
      enddo
                                  print *
                                  print *, '                    expid: ',trim(expid)
                                  print *, 'INPUT    bkg.eta filename: ',trim(tmpeta)
                                  print *, 'INPUT    bkg.sfc filename: ',trim(tmpsfc)
      if( trim(bkgeta) /= 'NONE') print *, 'OUTPUT   bkg.eta filename: ',trim(bkgeta)
      if( trim(bkgsfc) /= 'NONE') print *, 'OUTPUT   bkg.sfc filename: ',trim(bkgsfc)
                                  print *

! **********************************************************************
! ****                       Read bkg eta file                      ****
! **********************************************************************

      call gfio_open       ( trim(tmpeta),1,id,rc )
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

      timinc = 0
      call gfio_inquire ( id,im,jm,lm,ntime,nvars,      &
                          title,source,contact,undef,   &
                          lon,lat,lev,levunits,         &
                          yymmdd,hhmmss,timinc,         &
                          vname,vtitle,vunits,kmvar,    &
                          vrange,prange,rc )

      if( timinc == 0 ) timinc = freq * 10000
      if( timinc == 0 ) timinc = 06   * 10000

      allocate ( loc(nvars) )
      loc(1) = 1
      do n=2,nvars
      loc(n) = loc(n-1)+max(1,kmvar(n-1))
      enddo
      nmax = loc(nvars)+max(1,kmvar(nvars))-1

      allocate( vars(im,jm,nmax) )

      print * 
      print *, '             Reading File: ',trim(tmpeta)
      print *, '   Number of Time Periods: ',ntime,'  TimeInc: ',timinc

      nymd = yymmdd(ntime)
      nhms = hhmmss(ntime)

      print *, '               resolution: ',im,jm,lm
      print *, '                     date: ',nymd,nhms
      print *

      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             kbeg = 0
             kend = 1
         else
             kbeg = 1
             kend = kmvar(n)
         endif
         call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,kbeg,kend,vars(1,1,loc(n)),rc )
         if( rc.eq.0 ) then
             print *, '         Reading Variable: ',trim(vname(n))
         else      
             print *, '  Failed Reading Variable: ',trim(vname(n))
             stop
         endif
         call hflip ( vars(1,1,loc(n)),im,jm,kend )
      enddo

      call gfio_close ( id,rc )

! **********************************************************************
! ****                 Write bkg.eta File for Analysis              ****
! **********************************************************************

      write(date,'(i8.8)') nymd
      write(hour,'(i2.2)') nhms/10000

      if( trim(bkgeta) == 'NONE') bkgeta = trim(expid) // '.bkg.eta.' // date // '_' // hour // 'z.nc4'
      print *

      call write_hdf ( bkgeta,nymd,nhms,im,jm,lm,loc,nmax,undef,timinc,     &
                       nvars,vname,vtitle,vunits,kmvar,vrange,prange, vars, &
                       precision )

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

      deallocate ( loc  )
      deallocate ( vars )

! **********************************************************************
! ****                       Read bkg sfc file                      ****
! **********************************************************************

      call gfio_open       ( trim(tmpsfc),1,id,rc )
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

      timinc = 0
      call gfio_inquire ( id,im,jm,lm,ntime,nvars,      &
                          title,source,contact,undef,   &
                          lon,lat,lev,levunits,         &
                          yymmdd,hhmmss,timinc,         &
                          vname,vtitle,vunits,kmvar,    &
                          vrange,prange,rc )

      if( timinc == 0 ) timinc = 060000

      allocate ( loc(nvars) )
      loc(1) = 1
      do n=2,nvars
      loc(n) = loc(n-1)+max(1,kmvar(n-1))
      enddo
      nmax = loc(nvars)+max(1,kmvar(nvars))-1

      allocate( vars(im,jm,nmax) )

      print * 
      print *, '             Reading File: ',trim(tmpsfc)
      print *, '   Number of Time Periods: ',ntime,'  TimeInc: ',timinc

      nymd = yymmdd(ntime)
      nhms = hhmmss(ntime)

      print *, '               resolution: ',im,jm,lm
      print *, '                     date: ',nymd,nhms
      print *

      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             kbeg = 0
             kend = 1
         else
             kbeg = 1
             kend = kmvar(n)
         endif
         call gfio_getvar ( id,vname(n),nymd,nhms,im,jm,kbeg,kend,vars(1,1,loc(n)),rc )
         if( rc.eq.0 ) then
             print *, '         Reading Variable: ',trim(vname(n))
         else      
             print *, '  Failed Reading Variable: ',trim(vname(n))
             stop
         endif
         call hflip ( vars(1,1,loc(n)),im,jm,kend )
      enddo

      call gfio_close ( id,rc )

! **********************************************************************
! ****                 Write bkg.sfc File for Analysis              ****
! **********************************************************************

      write(date,'(i8.8)') nymd
      write(hour,'(i2.2)') nhms/10000

      if( trim(bkgsfc) == 'NONE') bkgsfc = trim(expid) // '.bkg.sfc.' // date // '_' // hour // 'z.nc4'
      print *

      call write_hdf ( bkgsfc,nymd,nhms,im,jm,lm,loc,nmax,undef,timinc,    &
                       nvars,vname,vtitle,vunits,kmvar,vrange,prange, vars,& 
                       precision )

      stop
      end

      subroutine write_hdf ( filename,nymd,nhms,im,jm,lm,loc,nmax,undef,timinc,   &
                             nvars,vname,vtitle,vunits,kmvar,vrange,prange, vars, &
                             precision )
      use m_set_eta
      implicit  none
      integer   im,jm,lm,nymd,nhms,nvars,nmax
      real     vars(im,jm,nmax)

      integer    loc(nvars)
      integer  kmvar(nvars)
      real  vrange(2,nvars)
      real  prange(2,nvars)

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact
      character*256   vname(nvars)
      character*256  vtitle(nvars)
      character*256  vunits(nvars)

      real lats(jm)
      real lons(im)

      real  ptop,dlon,dlat,pref,undef,pint
      integer i,j,L,n,timinc,rc,ks,kbeg,kend
      character*80 filename
      character*3  ch3
      integer  fid,precision,err
      logical  file_exists
      integer, parameter :: READ_WRITE = 0

      real, allocatable ::   ak(:)
      real, allocatable ::   bk(:)
      real, allocatable :: levs(:)
      real  dpref
            dpref(L) = ( ak(L+1)-ak(L) ) +  ( bk(L+1)-bk(L) ) * 98400.0


! String and vars settings
! ------------------------
      title    = 'GEOS5 Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'hPa'

! Compute grid
! ------------
      dlon = 360.0/ im
      dlat = 180.0/(jm-1)

      do j=1,jm
      lats(j) = -90.0 + (j-1)*dlat
      enddo
      do i=1,im
      lons(i) =   0.0 + (i-1)*dlon
      enddo

      if( lm.ne.0 ) then
          allocate ( levs(lm) )
          allocate ( ak(lm+1) )
          allocate ( bk(lm+1) )
          call set_eta ( lm,ks,ptop,pint,ak,bk )
          levs(1) = ptop + 0.5 * dpref(1)
          do L = 2, lm
          levs(L) = levs(L-1) + 0.5 * ( dpref(L-1) + dpref(L) )
          enddo
          levs(1:lm) = levs(1:lm) / 100.0
      else
          allocate ( levs(1) )
                     levs(1) = 1
      endif

! Check whether file exists
! -------------------------
      inquire ( file=trim(filename), exist=file_exists )

! Create/Open GFIO file
! ---------------------
      if ( file_exists ) then
           print *, 'Open existing ',trim(filename)
           call GFIO_Open ( filename, READ_WRITE, fid, err )
      else
           print *, 'Creating ',trim(filename)
           call GFIO_Create ( filename, title, source, contact, undef, &
                              im, jm, lm, lons, lats, levs, levunits,  &
                              nymd, nhms, timinc,                      &
                              nvars, vname, vtitle, vunits, kmvar,     &
                              vrange, prange, precision,               &
                              fid, rc )
      endif

! Write GFIO data
! ---------------
      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             kbeg = 0
             kend = 1
         else
             kbeg = 1
             kend = kmvar(n)
         endif
         call gfio_putvar ( fid,vname(n),nymd,nhms,im,jm,kbeg,kend,vars(1,1,loc(n)),rc )
         if( rc.eq.0 ) then
             print *, '         Writing Variable: ',trim(vname(n))
         else      
             print *, '  Failed Writing Variable: ',trim(vname(n))
             stop
         endif
      enddo

! Write GFIO global attributes
! ----------------------------
      if( lm.ne.0 ) then
          call GFIO_PutIntAtt  ( fid,'nstep',  1,ks   ,0        ,rc )
          call GFIO_PutRealAtt ( fid,'ptop',   1,ptop ,precision,rc )
          call GFIO_PutRealAtt ( fid,'pint',   1,pint ,precision,rc )
          call GFIO_PutIntAtt  ( fid,'ks',     1,ks   ,0        ,rc )
          call GFIO_PutRealAtt ( fid,'ak',  lm+1,ak   ,precision,rc )
          call GFIO_PutRealAtt ( fid,'bk',  lm+1,bk   ,precision,rc )
      endif

      call gfio_close ( fid,rc )
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

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " dyn52dyn.x   -expid    expid"
      print *, "             -tmpeta   tmpeta_fname "
      print *, "             -tmpsfc   tmpsfc_fname "
      print *, "            [-bkgeta   bkgeta_fname]"
      print *, "            [-bkgsfc   bkgsfc_fname]"
      print *, "            [-prec     precision   ]"
      print *
      print *, "where:"
      print *, "-----"
      print *, "  -expid          expid:  Experiment  ID"
      print *, "  -tmpeta  tmpeta_fname:  Filename of Temporary GCM bkg.eta file"
      print *, "  -tmpsfc  tmpsfc_fname:  Filename of Temporary GCM bkg.sfc file"
      print *
      print *, "  -bkgeta  bkgeta_fname:  Filename of Final GCM bkg.eta file (optional)"
      print *, "  -bkgsfc  bkgsfc_fname:  Filename of Final GCM bkg.sfc file (optional)"
      print *
      print *, "  -prec    PREC           where PREC=32 or 64 for 32 or 64 bits output"
      print *, "  -freq    FREQ           specify frequency of background; default: 6 (hr)"
      print *
      call exit(7)
      end

