      program  main

      implicit none

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

      real, allocatable ::         co_surf(:,:)  
      real, allocatable ::         co_p500(:,:)  
      real, allocatable ::     cobbae_surf(:,:)  
      real, allocatable ::     cobbae_p500(:,:)  
      real, allocatable ::   coembbae_surf(:,:)  


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

      integer ids,idp,rc,fid,nhmsf,n2d,n3d
      integer idpr,n2dp,n3dp,nvarsp
      integer nvars,ngatts,ntime,ntimes,gfrc

      character*256, allocatable ::    arg(:)
      character*256, allocatable :: fnames(:)
      character*256, allocatable :: fnamep(:)
      character*256  dummy,  name
      character*256  output, file
      character*256  ftype
      character*256  ext

      character*8    date,date0
      character*4    time0
      character*2    time,hour0,mins0
      character*1    char
      data output  /'ascii'/

      integer n,m,nargs,iargc,L,nbeg,nfiles,nsfiles,npfiles,mlev

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
      logical edges
      real    ptop
      real    pi,zlat,zlon

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

      pi = 4.0*atan(1.0)
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

          nargs = iargc()
      if( nargs.eq.0 ) then
          stop
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs

             if( trim(arg(n)).eq.'-surf' ) then
                 nsfiles = 1
                 read(arg(n+nsfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+nsfiles.ne.nargs )
                 nsfiles = nsfiles+1
                 read(arg(n+nsfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) nsfiles = nsfiles-1
                 allocate ( fnames(nsfiles) )
                 do m=1,nsfiles
                 fnames(m) = arg(n+m)
                 enddo
             endif
	     
             if( trim(arg(n)).eq.'-p500' ) then
                 npfiles = 1
                 read(arg(n+npfiles),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+npfiles.ne.nargs )
                 npfiles = npfiles+1
                 read(arg(n+npfiles),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) npfiles = npfiles-1
                 allocate ( fnamep(npfiles) )
                 do m=1,npfiles
                 fnamep(m) = arg(n+m)
                 enddo
             endif
	     
           enddo
      endif

C **********************************************************************
C ****                        Read HDF Meta Data                    ****
C **********************************************************************

      call read_hdf_meta ( fnames(1),im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,ids,
     .                     nymdb,nhmsb,ndt,ntimes,
     .                     nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )

      call read_hdf_meta ( fnamep(1),im,jm,lm,n2d,n3d,lat,lon,lonbeg,undef,idp,
     .                     nymdb,nhmsb,ndt,ntimes,
     .                     nvars,names,Lsurf,name2d,titl2d,unit2d,name3d,titl3d,unit3d )

C **********************************************************************
C ****                Summarize Input Variables                     ****
C **********************************************************************

      allocate (    ps(im,jm)        )
      allocate (    dp(im,jm,lm)     )
      allocate (   q2d(im,jm,   n2d) )
      allocate (   q3d(im,jm,lm,n3d) )
      allocate ( dum2d(im,jm)        )
      allocate ( dum3d(im,jm,lm)     )

      allocate (       co_surf(im,jm) )
      allocate (       co_p500(im,jm) )
      allocate (   cobbae_surf(im,jm) )
      allocate (   cobbae_p500(im,jm) )
      allocate ( coembbae_surf(im,jm) )


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
                    print *, 'Beginning Date to   Read: ',nymdb
                    print *, 'Beginning Time to   Read: ',nhmsb
                    print *
                    print *, 'Beginning Date to  Write: ',nymd
                    print *, 'Beginning Time to  Write: ',nhms
                    print *, '          Time Increment: ',nhmsf(ndt),' (',ndt,' seconds)'
                    print *
                    print *, '                      lm: ',lm
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

      print *, 'Surf Files: '
      do n=1,nsfiles
      print *, n,trim(fnames(n))
      enddo
      print *
      print *, 'P500 Files: '
      do n=1,npfiles
      print *, n,trim(fnamep(n))
      enddo
      print *

C **********************************************************************
C ****              Read and Interpolate Eta File                   ****
C **********************************************************************

      do n=1,nsfiles
                   print *, 'Opening: ',trim(fnames(n))
                   print *, 'Opening: ',trim(fnamep(n))

      write(date0,1000)  nymd
      write(hour0,2000)  nhms/10000
      write(mins0,2000) (nhms-(nhms/10000)*10000)/100
 1000 format(i8.8)
 2000 format(i2.2)
      time0 = trim(hour0)

          call gfio_close ( ids,rc )
          call gfio_close ( idp,rc )
          call gfio_open  ( trim(fnames(n)),1,ids,rc ) 
          call gfio_open  ( trim(fnamep(n)),1,idp,rc ) 

      rc        =  0
      ntime     =  0
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
                  print *

	          call gfio_getvar ( ids,'COEMbbae',nymd,nhms,im,jm,0, 1,coembbae_surf,rc )          

	          call gfio_getvar ( ids,'CO',nymd,nhms,im,jm,1, 1,co_surf,rc )          
	          call gfio_getvar ( idp,'CO',nymd,nhms,im,jm,1, 1,co_p500,rc )          

	          call gfio_getvar ( ids,'CObbae',nymd,nhms,im,jm,1, 1,cobbae_surf,rc )          
	          call gfio_getvar ( idp,'CObbae',nymd,nhms,im,jm,1, 1,cobbae_p500,rc )          

                  if( rc.eq.0 ) then
                  file = trim(output) // "." // trim(date0) // "_" // trim(time0) // "z"
                  open ( 88,file=trim(file),form='formatted'  ,access='sequential')
                  write(88,*) 'nymd: ',nymdr,' nhms: ',nhmsr
                  write(88,1005)

                  coembbae_surf = coembbae_surf*86400
                        co_surf =       co_surf*1e9
                        co_p500 =       co_p500*1e9
                    cobbae_surf =   cobbae_surf*1e9
                    cobbae_p500 =   cobbae_p500*1e9

                   do j=1,jm
                       zlat = lat(j)
                   if( zlat.ge.30 .and. zlat.le.70 ) then
                       do i=1,im
                           zlon = lon(i)
                       if( zlon.ge.30 .and. zlon.le.60 ) then

                       write(88,1010) zlat,zlon,coembbae_surf(i,j),
     .                                                co_surf(i,j),
     .                                            cobbae_surf(i,j),
     .                                                co_p500(i,j),
     .                                            cobbae_p500(i,j) 
                       endif
                       enddo
                   endif
                   enddo

                   call tick (nymd,nhms,ndt)

                  else
                   print *
                   print *
                  endif
              enddo
      enddo

c Write Timing Information
c ------------------------

      deallocate ( dp,ps,arg )

 1005 format(1x,/,1x,
     . '  LAT     LON         coembbae          co_surf        cobbae_surf          co_p500         cobbae_p500',/,
     . ' -------------------------------------------------------------------------------------------------------')
 1010 format(1x,f6.3,3x,f6.3,4x,5(g,3x))

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

