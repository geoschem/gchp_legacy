      program  bkgeta_fix

      use m_set_eta, only: set_eta
      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****        Program to Modify GEOS-5 GCM BKG.ETA file             ****
c ****        -----------------------------------------             ****
c ****   1)  Flip Horizontal Longitudes (from -180,180 to 0,360)    ****
c ****   2)  Add  AK & BK Global Attributes                         ****
c ****   3)  Create LWI based on Land/Water/Ice Fractions           ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm
      integer  nymd, nhms

      integer id1,id2,rc,precision,timinc,timeid
      integer ntime,nvars,ngatts,ncvid

      character*256, allocatable :: arg(:)
      character*256  input, output

      integer n,m,nargs,iargc,L,nvars2

      real     plev
      real     undef
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:), vrange2(:,:)
      real,    allocatable :: prange(:,:), prange2(:,:)
      integer, allocatable ::  kmvar(:)  ,  kmvar2(:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::   nloc(:)

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:),  vname2(:)
      character*256, allocatable :: vtitle(:), vtitle2(:)
      character*256, allocatable :: vunits(:), vunits2(:)

      integer i,j,k,nmax,kbeg,kend,ks
      real    lonbeg,ptop,pint,dlon

      real,    allocatable ::  ak(:)
      real,    allocatable ::  bk(:)
      real,    allocatable ::  q1(:,:,:)  ! Input  Fields
      real,    allocatable ::  q2(:,:,:)  ! Output Fields
      real,    allocatable :: lwi(:,:)

      logical  Llwi

c  Reference pressure thickness assuming ps ~ 984 hPa
c  ---------------------------------------------------
      real dpref
           dpref(k) = ( ak(k+1)-ak(k) ) + ( bk(k+1)-bk(k) ) * 98400.

C **********************************************************************
C ****                    Read Command Line Arguments               ****
C **********************************************************************

          nargs = iargc()
      if( nargs.ne.3 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-o' ) output = arg(n+1)
          enddo
                                         input = arg(nargs)
      endif

C **********************************************************************
C ****            Read Input BKG.ETA File Attributes                ****
C **********************************************************************

      call gfio_open ( trim(input),1,id1,rc )
      call gfio_diminquire ( id1,im,jm,lm,ntime,nvars,ngatts,rc )

      allocate ( lon(im)         )
      allocate ( lat(jm)         )
      allocate ( lev(lm)         )
      allocate ( yymmdd(  ntime) )
      allocate ( hhmmss(  ntime) )
      allocate (  vname(  nvars) )
      allocate ( vtitle(  nvars) )
      allocate ( vunits(  nvars) )
      allocate (  kmvar(  nvars) )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )

      call gfio_inquire ( id1,im,jm,lm,ntime,nvars,
     .                    title,source,contact,undef,
     .                    lon,lat,lev,levunits,
     .                    yymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    vrange,prange,rc )

c Set TIMINC (if zero)
c --------------------
      if( timinc .eq. 0 ) then
          timeId = ncvid (id1, 'time', rc)
          call ncagt     (id1, timeId, 'time_increment', timinc, rc)
          if( timinc .eq. 0 ) then
              print *
              print *, 'Warning, GFIO Inquire states TIMINC = ',timinc
              print *, '         This will be reset to 060000 '
              print *, '         Use -ndt NNN (in seconds) to overide this'
          timinc = 060000
          endif
      endif

C **********************************************************************
C ****                       Check Longitudes                       ****
C **********************************************************************

          lonbeg = lon(1)
      if( lonbeg.lt.0.0 ) then
          print *
          print *, 'Flipping Longitudes ...'
          dlon = 360.0 / im
          do i=1,im
          lon(i) = (i-1)*dlon
          enddo
      endif

C **********************************************************************
C ****                    Set AK & BK  and  PLEVS                   ****
C **********************************************************************

      allocate ( ak(lm+1) )
      allocate ( bk(lm+1) )
      call set_eta( lm, ks, ptop, pint, ak, bk)
      k = 1
      dowhile ( bk(k).eq.0.0 )
      k = k+1
      enddo
      ks = k-2
      ptop = ak(1)
      pint = ak(ks+1)

c Print AK & BK Table
c -------------------
      print *
      write(6,100)
 100  format(2x,' k ','      A(k)    ',2x,' B(k)   ',2x,'  Pref    ',2x,'  DelP',/,
     .       1x,'----',3x,'----------',2x,'--------',2x,'----------',2x,'---------' )
      k=1
      write(6,101) k,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k)
      do k=2,LM+1
      write(6,102) k,ak(k)*0.01, bk(k), ak(k)*0.01 + 1000.0*bk(k),
     .             (ak(k)-ak(k-1))*0.01 + 1000.0*(bk(k)-bk(k-1))
      enddo
      print *
      print *, '  KS     = ',ks
      print *, 'PTOP(mb) = ',ptop/100
      print *, 'PINT(mb) = ',pint/100
 101  format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4)
 102  format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4,3x,f8.4)

c Reset PLEV values
c -----------------
      lev(1) = ptop + 0.5 * dpref(1)
      do k = 2, lm
         lev(k) = lev(k-1) + 0.5 * ( dpref(k-1) + dpref(k) )
      end do
      lev(1:lm) = lev(1:lm) / 100.

      levunits = 'hPa'

C **********************************************************************
C ****     Determine Location Index for Each Variable in File       ****
C **********************************************************************

      allocate ( nloc(nvars) )
      print *
      nloc(1) = 1
      write(6,7000) 1,trim(vname(1)),nloc(1),trim(vtitle(1)),max(1,kmvar(1))
      do n=2,nvars
      nloc(n) = nloc(n-1)+max(1,kmvar(n-1))
      write(6,7000) n,trim(vname(n)),nloc(n),trim(vtitle(n)),max(1,kmvar(n))
 7000 format(1x,'BKG.ETA Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,a40,2x,i2,3x,i2,3x,i2)
      enddo
      print *

      nmax = nloc(nvars)+max(1,kmvar(nvars))-1
      allocate(  q1(im,jm,nmax) )
      allocate(  q2(im,jm,lm)   )
      allocate( lwi(im,jm)      )

C **********************************************************************
C ****               Read and Write BKG.ETA File Data               ****
C **********************************************************************

c Loop Over Time
c --------------
      do m=1,ntime
             nymd = yymmdd(m)
             nhms = hhmmss(m)
      print *, 'Reading for Date: ',nymd,' Time: ',nhms

c Read Fields in Input BKG.ETA File
c ---------------------------------
      do n=1,nvars
         if( kmvar(n).eq.0 ) then
             kbeg = 0
             kend = 1
         else
             kbeg = 1
             kend = kmvar(n)
         endif
         write(6,7002) n,trim(vname(n)),nloc(n),max(1,kmvar(n))
         call gfio_getvar ( id1,vname(n),nymd,nhms,im,jm,kbeg,kend,q1(1,1,nloc(n)),rc )
         if( lonbeg.lt.0.0 ) call hflip( q1(1,1,nloc(n)),im,jm,kend )
      enddo
 7002 format(1x,'Reading BKG.ETA Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,'KDIM: ',i2)

C **********************************************************************
C ****              Compute LWI from Model Land Fractions           ****
C ****              -------------------------------------           ****
C ****  1) Call GET_LWI to Compute LWI from Model Land Fractions    ****
C ****  2) If Successful, Check if LWI is already a BKG Variable    ****
C ****     If     Available,  Simply Replace Old LWI with New LWI   ****
C ****     If NOT Available,  Add New LWI Variable to BKG File      ****
C ****  3) If NOT Successful, Simply write Existing Variables       ****
C **********************************************************************

c Compute LWI from Model Land Fractions
c -------------------------------------
      call get_lwi ( id1,nymd,nhms,im,jm,lwi,undef,rc )

      if( rc.eq.0 ) then  ! Successful Computation of LWI

          if( lonbeg.lt.0.0 ) call hflip( lwi,im,jm,1 )

          Llwi = .false.
          do n=1,nvars
          if( trim(vname(n)).eq.'lwi' ) then
              q1(:,:,nloc(n)) =  lwi
              Llwi = .true.
          endif
          enddo

          if( Llwi )  then
                      nvars2 = nvars
          allocate (  vname2(  nvars) )
          allocate ( vtitle2(  nvars) )
          allocate ( vunits2(  nvars) )
          allocate (  kmvar2(  nvars) )
          allocate ( vrange2(2,nvars) )
          allocate ( prange2(2,nvars) )
                      vname2 = vname
                     vtitle2 = vtitle
                     vunits2 = vunits
                      kmvar2 = kmvar
                     vrange2 = vrange
                     prange2 = prange
          else
                      nvars2 = nvars+1
          allocate (  vname2(  nvars2) )
          allocate ( vtitle2(  nvars2) )
          allocate ( vunits2(  nvars2) )
          allocate (  kmvar2(  nvars2) )
          allocate ( vrange2(2,nvars2) )
          allocate ( prange2(2,nvars2) )
                      vname2(1:nvars ) = vname
                     vtitle2(1:nvars ) = vtitle
                     vunits2(1:nvars ) = vunits
                      kmvar2(1:nvars ) = kmvar
                   vrange2(:,1:nvars ) = vrange
                   prange2(:,1:nvars ) = prange

                      vname2(nvars2) = 'lwi'
                     vtitle2(nvars2) = 'Land-water-ice mask'
                     vunits2(nvars2) = 'unknown'
                      kmvar2(nvars2) = 0
                   vrange2(:,nvars2) = undef
                   prange2(:,nvars2) = undef
          endif

      else  ! No Model Land Fractions Available for LWI

                      nvars2 = nvars
          allocate (  vname2(  nvars) )
          allocate ( vtitle2(  nvars) )
          allocate ( vunits2(  nvars) )
          allocate (  kmvar2(  nvars) )
          allocate ( vrange2(2,nvars) )
          allocate ( prange2(2,nvars) )
                      vname2 = vname
                     vtitle2 = vtitle
                     vunits2 = vunits
                      kmvar2 = kmvar
                     vrange2 = vrange
                     prange2 = prange
      endif


c Create Output BKG.ETA File
c --------------------------
      if( m.eq.1 ) then
      precision = 0 ! 32-bit
      print *
      call GFIO_Create ( trim(output), title, source, contact, undef,
     .                   im, jm, lm, lon, lat, lev, levunits,
     .                   nymd, nhms, timinc,
     .                   nvars2, vname2, vtitle2, vunits2, kmvar2,
     .                   vrange2, prange2, precision,id2,rc )
      print *, 'Creating Output File: ',trim(output),' RC = ',rc,' ID = ',id2
      endif

c Write Fields to Output BKG.ETA File
c -----------------------------------
      do n=1,nvars
         if( kmvar2(n).eq.0 ) then
                   kbeg = 0
                   kend = 1
         else
                   kbeg = 1
                   kend = kmvar2(n)
         endif
         write(6,3001) n,trim(vname2(n)),nloc(n),kend
         q2(:,:,1:kend) = q1(:,:,nloc(n):nloc(n)+kend-1)
         call gfio_putVar ( id2,trim(vname(n)),nymd,nhms,im,jm,kbeg,kend,q2,rc )
      enddo
      do n=nvars+1,nvars2
         kend = 1
         write(6,3001) n,trim(vname2(n)),nmax+1,kend
         call gfio_putVar ( id2,trim(vname2(n)),nymd,nhms,im,jm,0,1,lwi,rc )
      enddo
 3001 format(1x,'Writing BKG.ETA Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,'KDIM: ',i2)

c End Time Loop
c -------------
      enddo

C **********************************************************************
C ****                 Add GFIO Global Attributes                   ****
C **********************************************************************

      call GFIO_PutRealAtt ( id2,'ptop' ,   1,ptop ,precision,rc )
      call GFIO_PutRealAtt ( id2,'pint' ,   1,pint ,precision,rc )
      call GFIO_PutIntAtt  ( id2,'ks'   ,   1,ks   ,precision,rc )
      call GFIO_PutRealAtt ( id2,'ak'   ,lm+1,ak   ,precision,rc )
      call GFIO_PutRealAtt ( id2,'bk'   ,lm+1,bk   ,precision,rc )
      call GFIO_PutIntAtt  ( id2,'nstep',   1,n    ,precision,rc )

      call gfio_close ( id1,rc )
      call gfio_close ( id2,rc )

      deallocate ( lon )
      deallocate ( lat )
      deallocate ( lev )
      deallocate ( yymmdd )
      deallocate ( hhmmss )
      deallocate (  vname, vname2 )
      deallocate ( vtitle,vtitle2 )
      deallocate ( vunits,vunits2 )
      deallocate (  kmvar, kmvar2 )
      deallocate ( vrange,vrange2 )
      deallocate ( prange,prange2 )
      deallocate ( q1,q2  )
      deallocate ( ak,bk  )
      deallocate ( lwi    )

      stop
      end

      subroutine get_lwi ( id,nymd,nhms,im,jm,lwi,undef,rc )
      implicit none
      integer  nymd,nhms
      integer  im,jm,id,rc
      real lwi(im,jm),undef

      real, allocatable :: TS       (:,:)
      real, allocatable :: FRLAKE   (:,:)
      real, allocatable :: FROCEAN  (:,:)
      real, allocatable :: FRSEAICE (:,:)

      allocate( TS       (im,jm) )
      allocate( FRLAKE   (im,jm) )
      allocate( FROCEAN  (im,jm) )
      allocate( FRSEAICE (im,jm) )

! Create LWI Using Model Land Fractions
! -------------------------------------
                call GFIO_GetVar ( id, 'ts'       , nymd, nhms, im, jm, 0, 1, TS,        rc )
      if(rc==0) call GFIO_GetVar ( id, 'FRLAKE'   , nymd, nhms, im, jm, 0, 1, FRLAKE,    rc )
      if(rc==0) call GFIO_GetVar ( id, 'FROCEAN'  , nymd, nhms, im, jm, 0, 1, FROCEAN,   rc )
      if(rc==0) call GFIO_GetVar ( id, 'FRSEAICE' , nymd, nhms, im, jm, 0, 1, FRSEAICE,  rc )
      if(rc==0) then
                                             lwi = 1.0  ! Land
      where (  FROCEAN+FRLAKE >= 0.6       ) lwi = 0.0  ! Water
      where (  lwi==0 .and. FRSEAICE > 0.5 ) lwi = 2.0  ! Ice
      where (  lwi==0 .and.     TS < 271.4 ) lwi = 2.0  ! Ice

      else
               lwi = undef
      endif

      deallocate( TS        )
      deallocate( FRLAKE    )
      deallocate( FROCEAN   )
      deallocate( FRSEAICE  )

      return
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer    im,jm,lm,i,j,L
      real     q(im,jm,lm),dum(im)
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
      write(6,100)
 100  format(/,"Usage:  ",/,
     .         "gcmbkg2ana.x -o Output_Name Input_Name",/
     .         )
      stop
      end
