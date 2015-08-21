      program  main

      use MAPL_IOMod
 
      implicit none

      integer headr1(6)
      integer headr2(5)
      integer im,jm,lm,L
      integer nymd,nhms

      real*8, allocatable ::   ak(:)
      real*8, allocatable ::   bk(:)
      real*8, allocatable ::  dum(:,:)
      real*4, allocatable ::    u(:,:,:)
      real*4, allocatable ::    v(:,:,:)
      real*4, allocatable ::   th(:,:,:)
      real*4, allocatable ::  ple(:,:,:)
      real*4, allocatable ::   pk(:,:,:)
      real*4, allocatable ::  ak4(:)
      real*4, allocatable ::  bk4(:)

      character*512 dynrst
      character*512, allocatable :: arg(:)
      integer   nargs,iargc,n
      logical   HEADER
      type(MAPL_NCIO) :: NCIO
      integer :: filetype,rc

      HEADER = .false.

          nargs = iargc()
      if( nargs.eq.0 ) stop

      allocate ( arg(nargs) )
      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
         if( trim(arg(n)).eq.'-h' ) HEADER = .true.
         if( trim(arg(n)).ne.'-h' ) dynrst = trim(arg(n))
      enddo

! **********************************************************************
! ****    Read dycore internal Restart for RSLV, Date and Time      ****
! **********************************************************************

      call MAPL_NCIOGetFileType(dynrst,filetype)
      if (filetype ==0) then
         if (header) then
            NCIO = MAPL_NCIOOpen(dynrst,rc=rc)
            call MAPL_NCIOGetDimSizes(NCIO,lon=im,lat=jm,lev=lm,date=nymd,time=nhms)
            print *, im,jm,lm,nymd,nhms
            stop
         else
            write(*,*)"fvrst can not make gfio file from nc4 restart yet"
            stop
         end if
      end if

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

      nymd = headr1(1)*10000
     .     + headr1(2)*100
     .     + headr1(3)
      nhms = headr1(4)*10000
     .     + headr1(5)*100
     .     + headr1(6)

      if( HEADER ) then
          print *, im,jm,lm,nymd,nhms
          stop
      endif

      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )
      allocate (  ak4(lm+1)       )
      allocate (  bk4(lm+1)       )
      allocate (  dum(im,jm)      )
      allocate (    u(im,jm,lm)   )
      allocate (    v(im,jm,lm)   )
      allocate (   th(im,jm,lm)   )
      allocate (   pk(im,jm,lm)   )
      allocate (  ple(im,jm,lm+1) )

! **********************************************************************
! ****                  Read dycore internal Restart                ****
! **********************************************************************

      read (10) ak ; ak4 = ak
      read (10) bk ; bk4 = bk

      do L=1,lm
         read(10) dum ;   u(:,:,L) = dum
      enddo
      do L=1,lm
         read(10) dum ;   v(:,:,L) = dum
      enddo
      do L=1,lm
         read(10) dum ;  th(:,:,L) = dum
      enddo
      do L=1,lm+1
         read(10) dum ; ple(:,:,L) = dum
      enddo
      do L=1,lm
         read(10) dum ;  pk(:,:,L) = dum
      enddo
      close (10)

      call writit ( u,v,th,ple,pk,ak4,bk4,im,jm,lm,nymd,nhms,dynrst )
      stop
      end

      subroutine writit ( u,v,th,ple,pk,ak,bk,im,jm,lm,nymd,nhms,dynrsti )
      implicit  none
      integer   im,jm,lm,nymd,nhms
      real*4        u(im,jm,lm)
      real*4        v(im,jm,lm)
      real*4       th(im,jm,lm)
      real*4       pk(im,jm,lm)
      real*4      ple(im,jm,lm+1)
      real*4       ak(lm+1)
      real*4       bk(lm+1)

      real lats(jm)
      real lons(im)
      real levs(lm)
      real*8 latsd(jm)
      real*8 lonsd(im)

      real  ptop,dlon,dlat,pref,undef,pint
      integer i,j,L,n,timeinc,rc,ks
      character*512 dynrsti,dynrsto
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
      real   rgas,rvap,eps,kappa,grav,cp

      real   dpref
             dpref(L) = ( ak(L+1)-ak(L) ) +  ( bk(L+1)-bk(L) ) * 98400.0

      undef     = 1.0e15
      timeinc   = 060000

      precision = 1 ! 64-bit
      precision = 0 ! 32-bit
 
! String and vars settings
! ------------------------
      dynrsto  = trim(dynrsti) // ".nc4"
      title    = 'GEOS5 Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'hPa'

      nvars = 06
      allocate (     vname(nvars) )
      allocate (    vtitle(nvars) )
      allocate (    vunits(nvars) )
      allocate (     lmvar(nvars) )
      allocate ( v_range(2,nvars) )
      allocate ( p_range(2,nvars) )

       vname(01) = 'u'
      vtitle(01) = 'Zonal Wind'
      vunits(01) = 'm/s'
       lmvar(01) =  lm

       vname(02) = 'v'
      vtitle(02) = 'Meridional Wind'
      vunits(02) = 'm/s'
       lmvar(02) =  lm

       vname(03) = 'th'
      vtitle(03) = 'Scaled Potential Temperature'
      vunits(03) = 'K/Pa^kappa'
       lmvar(03) =  lm

       vname(04) = 'ple'
      vtitle(04) = 'Edge Pressures'
      vunits(04) = 'Pa'
       lmvar(04) =  lm

       vname(05) = 'ps'
      vtitle(05) = 'Surface Pressure'
      vunits(05) = 'Pa'
       lmvar(05) =  0

       vname(06) = 'pk'
      vtitle(06) = 'P**kappa'
      vunits(06) = 'Pa^kappa'
       lmvar(06) =  lm

      v_range(:,:) = undef
      p_range(:,:) = undef

! Compute grid
! ------------
      if( jm.eq.6*im ) then
          do j=1,jm
          latsd(j) = j
          enddo
          do i=1,im
          lonsd(i) = i
          enddo
      else
          dlon = 360.0/ im
          dlat = 180.0/(jm-1)
          do j=1,jm
          latsd(j) = -90.0  + (j-1)*dlat
          enddo
          do i=1,im
          lonsd(i) = -180.0 + (i-1)*dlon
          enddo
      endif

      lats = latsd
      lons = lonsd
      do L=1,lm
      levs(L) = L
      enddo

          ptop = ak(1)
       levs(1) = ptop + 0.5 * dpref(1)
      do L = 2, lm
       levs(L) = levs(L-1) + 0.5 * ( dpref(L-1) + dpref(L) )
      enddo
       levs(1:lm) = levs(1:lm) / 100.0

! Create GFIO file
! ----------------
      call GFIO_Create ( dynrsto, title, source, contact, undef,
     .                   im, jm, lm, lons, lats, levs, levunits,
     .                   nymd, nhms, timeinc,
     .                   nvars, vname, vtitle, vunits, lmvar,
     .                   v_range, p_range, precision,
     .                   fid, rc )
! Write GFIO data
! ---------------
      allocate( dum1(lm+1)     )
      allocate( dum2(im,jm)    )
      allocate( dum3(im,jm,lm) )

      dum3 = u             ; call Gfio_putVar ( fid,vname(01),nymd,nhms,im,jm,1,lm,dum3,rc )
      dum3 = v             ; call Gfio_putVar ( fid,vname(02),nymd,nhms,im,jm,1,lm,dum3,rc )
      dum3 = th            ; call Gfio_putVar ( fid,vname(03),nymd,nhms,im,jm,1,lm,dum3,rc )
      dum3 = ple(:,:,1:lm) ; call Gfio_putVar ( fid,vname(04),nymd,nhms,im,jm,1,lm,dum3,rc )
      dum2 = ple(:,:,lm+1) ; call Gfio_putVar ( fid,vname(05),nymd,nhms,im,jm,0,1 ,dum2,rc )
      dum3 = pk            ; call Gfio_putVar ( fid,vname(06),nymd,nhms,im,jm,1,lm,dum3,rc )

! Write GFIO global attributes
! ----------------------------
      ks   = 0
      pint = ak(ks+1)
      dum1 = ak ; call GFIO_PutRealAtt ( fid,'ak',  lm+1,dum1 ,precision,rc )
      dum1 = bk ; call GFIO_PutRealAtt ( fid,'bk',  lm+1,dum1 ,precision,rc )


      call gfio_close ( fid,rc )
      return
      end
