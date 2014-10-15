      program  main

      implicit none
      include 'alias.com'

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****      Program to check for consistency between filename       ****
c ****      and internal HDF datestamp                              ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm,nt

      integer  nymd,nhms
      integer  im_out, jm_out
      integer  nymd0,nhms0,hour,day,month,year
      integer  nymdb,nhmsb


c Generic Model Variables
c -----------------------
      real, allocatable ::   ps(:,:)
      real, allocatable ::   dp(:,:,:)
      real, allocatable ::  q2d(:,:,:)
      real, allocatable ::  q3d(:,:,:,:)


c HDF and other Local Variables
c -----------------------------
      logical,       pointer :: Lsurf (:)
      character*128, pointer :: names (:)
      character*128, pointer :: name2d(:), name3d(:)
      character*128, pointer :: titl2d(:), titl3d(:)

      integer id,rc,fid,nhmsf,n2d,n3d
      integer nvars,ngatts,ntime,ntimes,gfrc

      real,          allocatable :: plevs(:)
      character*128, allocatable ::   arg(:)
      character*128, allocatable :: fname(:)
      character*128  dummy,  name
      character*128  output, hdfile, ctlfile

      character*8    date,date0
      character*2    time,time0
      character*1    char
      data output  /'eta2prs'/

      integer n,m,nargs,iargc,L,nbeg,nfiles,mlev
      integer ny,nm,nd

      real*8   lonbeg
      real     undef
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)

      character*128  title
      character*128  source
      character*128  contact
      character*128  levunits
      character*128, allocatable ::  vname(:)
      character*128, allocatable :: vtitle(:)
      character*128, allocatable :: vunits(:)

      integer i,j,ndt
      integer imax,jmax
      logical hdf, quad
      logical hdfcreate
      logical ctl_exists
      logical edges

      character*8 cdate

      interface
                subroutine read_ctl ( ctlfile,im,jm,lm,n2d,n3d,lonbeg,undef,
     .                                nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
                logical,       pointer :: Lsurf (:)
                character*128, pointer :: names (:)
                character*128, pointer :: name2d(:), name3d(:)
                character*128, pointer :: titl2d(:), titl3d(:)
                character*128 ctlfile
                integer       im,jm,lm,n2d,n3d,nvars
                real          undef
                real*8        lonbeg
            end subroutine read_ctl
                subroutine read_hdf ( hdffile,im,jm,lm,n2d,n3d,lonbeg,undef,id,
     .                                nymdb,nhmsb,ndt,ntimes,
     .                                nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
                logical,       pointer :: Lsurf (:)
                character*128, pointer :: names (:)
                character*128, pointer :: name2d(:), name3d(:)
                character*128, pointer :: titl2d(:), titl3d(:)
                character*128 hdffile
                integer       id,im,jm,lm,n2d,n3d,nvars
                integer       nymdb,nhmsb,ndt,ntimes
                real          undef
                real*8        lonbeg
            end subroutine read_hdf
      end interface

C **********************************************************************
C ****                       Initialization                         ****
C **********************************************************************

      ctlfile = 'xxx'
      im_out  = -999
      jm_out  = -999
      nymd0   = -999
      nhms0   = -999
      nt      =  1
      ndt     =  0
      hdf     =  .true.
      quad    =  .false.

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-im'   ) read(arg(n+1),*) im_out
             if( trim(arg(n)).eq.'-jm'   ) read(arg(n+1),*) jm_out
             if( trim(arg(n)).eq.'-nymd' ) read(arg(n+1),*) nymd0
             if( trim(arg(n)).eq.'-nhms' ) read(arg(n+1),*) nhms0
             if( trim(arg(n)).eq.'-ndt'  ) read(arg(n+1),*) ndt
             if( trim(arg(n)).eq.'-hdf'  ) read(arg(n+1),*) hdf
             if( trim(arg(n)).eq.'-quad' ) quad    = .true.
             if( trim(arg(n)).eq.'-flat' ) hdf     = .false.
             if( trim(arg(n)).eq.'-tag'  ) output  = arg(n+1)
             if( trim(arg(n)).eq.'-ctl'  ) ctlfile = arg(n+1)

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
           enddo
      endif

C **********************************************************************
C ****                  Read Grads CLT or HDF Meta Data             ****
C **********************************************************************

! Check whether ctl file exists
! -----------------------------

          call read_hdf ( fname(1),im,jm,lm,n2d,n3d,lonbeg,undef,id,
     .                    nymdb,nhmsb,ndt,ntimes,
     .                    nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
          call gfio_close ( id,rc )


C **********************************************************************
C ****                Summarize Input Variables                     ****
C **********************************************************************

      allocate (  ps(im,jm)        )
      allocate (  dp(im,jm,lm)     )
      allocate ( q2d(im,jm,   n2d) )
      allocate ( q3d(im,jm,lm,n3d) )

      if( im_out.eq.-999 ) im_out = im
      if( jm_out.eq.-999 ) jm_out = jm

      if(  nymd0 == -999 ) nymd0  = nymdb
      if(  nhms0 == -999 ) nhms0  = nhmsb

                    print *
                    print *, '            im: ',im_out
                    print *, '            jm: ',jm_out
                    print *, '            lm: ',lm
                    print *, 'Beginning Date: ',nymd0
                    print *, 'Beginning Time: ',nhms0
                    print *, 'Time Increment: ',nhmsf(ndt),' (',ndt,' seconds)'
                    print *

      print *, 'Files: '
      do n=1,nfiles
      print *, n,trim(fname(n))
      enddo
      print *


C **********************************************************************
C ****              Read and Interpolate Eta File                   ****
C **********************************************************************

      nymd   =  nymd0
      nhms   =  nhms0
      edges  = .false.

      do n=1,nfiles
      write(date0,1000) nymd
      write(time0,2000) nhms/10000
 1000 format(i8.8)
 2000 format(i2.2)

      rc = 0
      ntime     =  0
              dowhile (rc.eq.0)
              ntime = ntime + 1

              call read_hdf ( fname(n),im,jm,lm,n2d,n3d,lonbeg,undef,id,
     .                        nymdb,nhmsb,ndt,ntimes,
     .                        nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
              call gfio_close ( id,rc )


              if( nymd.ne.nymdb .or. nhms.ne.nhmsb ) then
                  print *, 'Opening: ',trim(fname(n))
                  print *, 'Checking for nymd: ',nymd,' nhms: ',nhms
                  print *, '       Found nymd: ',nymdb,' nhms: ',nhmsb
                  print *
              endif

              rc = 1
              NY = NYMD / 10000
              NM = MOD(NYMD,10000) / 100 + 1
              ND = MOD(NYMD,100)
              if( nm.gt.12 ) then
                  nm = 1
                  ny = ny+1
              endif

              nymd = ny*10000 + nm*100 + nd
              enddo
      enddo

      deallocate ( dp,ps,arg )

      stop
      end

      subroutine readit (q,im,jm,lm,ku)
      implicit none
      integer    im,jm,lm,ku,L
      real     q(im,jm,lm)
      real*4 dum(im,jm)
      do L=1,lm
         read(ku)   dum
         q(:,:,L) = dum(:,:)
      enddo
      return
      end subroutine readit

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

      subroutine read_hdf ( hdffile,im,jm,lm,n2d,n3d,lonbeg,undef,id,
     .                      nymd0,nhms0,ndt,ntime,
     .                      nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
      implicit none

      logical,       pointer :: Lsurf (:)
      character*128, pointer :: names (:)
      character*128, pointer :: name2d(:), name3d(:)
      character*128, pointer :: titl2d(:), titl3d(:)

      character*128 hdffile
      integer       id,im,jm,lm,n2d,n3d,nvars,nsecf
      integer       ntime,ngatts,rc,timinc,nymd0,nhms0,ndt
      real          undef
      real*8        lonbeg
      integer       L,m,n
      character*128 dummy,name

      character*128  title
      character*128  source
      character*128  contact
      character*128  levunits
      character*128, allocatable ::  vname(:)
      character*128, allocatable :: vtitle(:)
      character*128, allocatable :: vunits(:)
                                                                                                                     
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
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
                                                                                                                     
      nymd0  = yymmdd(1)
      nhms0  = hhmmss(1)

      return
      end subroutine read_hdf

      subroutine read_ctl ( ctlfile,im,jm,lm,n2d,n3d,lonbeg,undef,
     .                      nvars,names,Lsurf,name2d,titl2d,name3d,titl3d )
      implicit none

      logical,       pointer :: Lsurf (:)
      character*128, pointer :: names (:)
      character*128, pointer :: name2d(:), name3d(:)
      character*128, pointer :: titl2d(:), titl3d(:)

      character*128 ctlfile
      integer       im,jm,lm,n2d,n3d,nvars
      real          undef
      real*8        lonbeg
      integer       L,m,n
      character*128 dummy,name

C **********************************************************************
C ****               Read Grads CLT File for Meta Data              ****
C **********************************************************************

      open (10,file=trim(ctlfile),form='formatted')
      n2d = 0
      n3d = 0
      do
      read(10,*,end=500) dummy

           if( trim(dummy).eq.'xdef' ) then
           backspace(10)
           read(10,*) dummy,im,dummy,lonbeg
           endif

           if( trim(dummy).eq.'ydef' ) then
           backspace(10)
           read(10,*) dummy,jm
           endif

           if( trim(dummy).eq.'zdef' ) then
           backspace(10)
           read(10,*) dummy,lm
           endif

           if( trim(dummy).eq.'undef' ) then
           backspace(10)
           read(10,*) dummy,undef
           endif

           if( trim(dummy).eq.'vars' ) then
           backspace(10)
           read(10,*) dummy,nvars
           allocate( names(nvars) )
           do n=1,nvars
           read(10,*) names(n),L
               if( L.eq.0 ) then
                   n2d = n2d + 1
               else
                   n3d = n3d + 1
               endif
           enddo
           endif
      enddo
  500 continue
      rewind(10)

      if( n2d.eq.0 .and. n3d.eq.0 ) then
          print *, 'Warning, n2d = n3d = 0!'
          stop
      endif

      allocate( Lsurf(nvars) )
      allocate( name2d(n2d)  )
      allocate( titl2d(n2d)  )
      allocate( name3d(n3d)  )
      allocate( titl3d(n3d)  )

      n2d = 0
      n3d = 0
      do
      read(10,*,end=501) dummy
           if( trim(dummy).eq.'vars' ) then
           backspace(10)
           read(10,*) dummy,nvars
           do n=1,nvars
           read(10,*) name,L
               backspace(10)
               if( L.eq.0 ) then
                   Lsurf(n) = .true.
                   n2d = n2d + 1
                   read(10,*) name2d(n2d),L,m,titl2d(n2d)
               else
                   Lsurf(n) = .false.
                   n3d = n3d + 1
                   read(10,*) name3d(n3d),L,m,titl3d(n3d)
               endif
           enddo
           endif
      enddo
  501 continue

      return
      end subroutine read_ctl

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " checkdate_$ARCH.x -eta  eta_fname(s)"
      print *
      print *, "where:"
      print *
      print *, "  -eta eta_fname(s):  Filename(s) in eta HDF format"
      print *
      call exit(7)
      end subroutine usage
