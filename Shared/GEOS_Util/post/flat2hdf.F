      program  main

      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****     Program to create HDF output from a flat binary file     ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm,lm,nt

      integer  nymd,nhms
      integer  nymd0,nhms0
      integer  nymdb,nhmsb
      integer  lrec
      data     lrec /0/


c Generic Model Variables
c -----------------------
      real, allocatable ::  q2d(:,:,:)
      real, allocatable ::  q3d(:,:,:,:)


c HDF and other Local Variables
c -----------------------------
      logical,       pointer :: Lsurf (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      real,          pointer ::   levs(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)

      integer rc,fid,nhmsf,n2d,n3d
      integer nvars,ntime,gfrc

      character*256, allocatable ::   arg(:)
      character*256, allocatable :: fname(:)
      character*256  tag, endian
      character*256  hdfile, ctlfile

      character*8    date0
      character*2    time0
      character*1    char

      integer n,m,nargs,iargc,L,nfiles

      real     undef

      integer ndt
      logical hdfcreate
      logical ctl_exists
      logical yrev

      logical ecmwf

      interface
                subroutine read_ctl ( ctlfile,im,jm,lm,n2d,n3d,undef,lrec,
     .                                nvars,names,Lsurf,name2d,titl2d,name3d,titl3d,
     .                                lats,lons,levs,yrev,endian )
                logical,       pointer :: Lsurf (:)
                character*256, pointer :: names (:)
                character*256, pointer :: name2d(:), name3d(:)
                character*256, pointer :: titl2d(:), titl3d(:)
                character*256  ctlfile
                character*256  endian
                real,          pointer :: lats(:)
                real,          pointer :: lons(:)
                real,          pointer :: levs(:)
                integer        im,jm,lm,n2d,n3d,nvars,lrec
                real           undef
                logical        yrev
            end subroutine read_ctl
      end interface

C **********************************************************************
C ****                       Initialization                         ****
C **********************************************************************

      tag     = 'xxx'
      ctlfile = 'xxx'
      nymd0   = -999
      nhms0   = -999
      nt      =  1
      ndt     =  0
      yrev    =  .false.
      ecmwf   =  .false.

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-nymd' ) read(arg(n+1),*) nymd0
             if( trim(arg(n)).eq.'-nhms' ) read(arg(n+1),*) nhms0
             if( trim(arg(n)).eq.'-ndt'  ) read(arg(n+1),*) ndt
             if( trim(arg(n)).eq.'-ctl'  ) ctlfile = arg(n+1)
             if( trim(arg(n)).eq.'-tag'  )     tag = arg(n+1)
             if( trim(arg(n)).eq.'-ecmwf')   ecmwf = .true.

             if( trim(arg(n)).eq.'-flat' ) then
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
C ****                     Read Grads CLT File                      ****
C **********************************************************************

! Check whether ctl file exists
! -----------------------------
      inquire ( file=trim(ctlfile), exist=ctl_exists )

      if( ctl_exists ) then
          call read_ctl ( ctlfile,im,jm,lm,n2d,n3d,undef,lrec,
     .                    nvars,names,Lsurf,name2d,titl2d,name3d,titl3d,
     .                    lats,lons,levs,yrev,endian )

      else
          print *, 'No CTL file provided!'
          stop
      endif

C **********************************************************************
C ****                Summarize Input Variables                     ****
C **********************************************************************

      allocate ( q2d(im,jm,   n2d) )
      allocate ( q3d(im,jm,lm,n3d) )

      if(  nymd0 == -999 ) nymd0  = nymdb
      if(  nhms0 == -999 ) nhms0  = nhmsb

                    print *
                    print *, '            im: ',im
                    print *, '            jm: ',jm
                    print *, '            lm: ',lm
                    print *, 'Beginning Date: ',nymd0
                    print *, 'Beginning Time: ',nhms0
                    print *, 'Time Increment: ',nhmsf(ndt),' (',ndt,' seconds)'
                    print *, '          yrev: ',yrev
                    print *
                    print *, 'Levels: ',(levs(L),L=1,lm)
                    print *
                    print *, '2-D Fields:'
                    do n=1,n2d
                    print *,   trim(name2d(n)),'  ',trim(titl2d(n))
                    enddo
                    print *
                    print *, '3-D Fields:'
                    do n=1,n3d
                    print *,   trim(name3d(n)),'  ',trim(titl3d(n))
                    enddo
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

      do n=1,nfiles
      write(date0,1000) nymd
      write(time0,2000) nhms/10000
 1000 format(i8.8)
 2000 format(i2.2)

      if( trim(tag).eq.'xxx' ) then
          hdfile = trim(fname(n)) // ".nc4"
      else
          hdfile = trim(tag) // ".nc4"
      endif

      close(10)
      if( lrec.eq.-1 ) then
          print *, 'Opening ',trim(fname(n)),' access = sequential, endian: ',trim(endian)
          if( trim(endian).eq.'big_endian' ) then
              open (10,file=trim(fname(n)),form='unformatted',access='sequential',convert='big_endian')
          else
              open (10,file=trim(fname(n)),form='unformatted',access='sequential')
          endif
      else
          print *, 'Opening ',trim(fname(n)),' access = direct, endian: ',trim(endian)
          if( trim(endian).eq.'big_endian' ) then
              open (10,file=trim(fname(n)),form='unformatted',access='direct',recl=im*jm*4,convert='big_endian')
          else
              open (10,file=trim(fname(n)),form='unformatted',access='direct',recl=im*jm*4)
          endif
      endif

      rc = 0
      ntime     =  0
      hdfcreate = .true.
      print *
              dowhile (rc.eq.0)
              ntime = ntime + 1
              print *, 'nymd: ',nymd,' nhms: ',nhms

              call read_flat_data ( 10,q2d,q3d,nvars,Lsurf,im,jm,lm,
     .                              lrec,rc )

              if( rc.eq.0 ) then
                  call flat2hdf ( q2d,q3d,name2d,name3d,titl2d,titl3d,n2d,n3d,undef,
     .                           im,jm,lm,lats,lons,levs,nymd,nhms,ndt,
     .                           fid,hdfcreate,hdfile,yrev,ecmwf )
                  call tick (nymd,nhms,ndt)
                  hdfcreate = .false.
              else
                  call gfio_close ( fid,gfrc )
                  lrec = 0
                  print *
                  print *, 'Created: ',trim(hdfile)
                  print *
              endif
              enddo
      enddo

      deallocate ( arg )

      stop
      end

      subroutine read_flat_data ( ku,q2d,q3d,nvars,
     .                            Lsurf,im,jm,lm,lrec,rc )
      implicit none

      integer   im,jm,lm,ku,lrec,rc
      integer   nvars
      real  q2d(im,jm   ,1)
      real  q3d(im,jm,lm,1)
      real  dummy
      logical Lsurf(nvars)

      integer k,m,n

c Test for End of File
c --------------------
      rc = 0
      if( lrec.eq.-1 ) then
          print *, 'Testing for EOF on sequential file ...'
          read(ku,err=999,end=999)
          backspace(ku)
      else
          print *, 'Testing for EOF on direct access file, lrec = ',lrec+1
c         read(ku,err=999,rec=lrec+1)
          read(ku,err=999,rec=lrec+1) dummy
      endif
      goto 1000
  999 continue
      print *, 'End of File reached'
      rc = -1
      return

 1000 continue

C **********************************************************************
C ****                     Read Diagnostic Data                     ****
C **********************************************************************

      m = 0
      n = 0
      do k=1,nvars
      if( Lsurf(k) ) then
          m = m+1
          call readit (q2d(1,1,m),im,jm,1,ku,lrec)
      else
          n = n+1
          call readit (q3d(1,1,1,n),im,jm,lm,ku,lrec)
      endif
      enddo

      return
      end subroutine read_flat_data

      subroutine readit (q,im,jm,lm,ku,lrec)
      implicit none
      integer    im,jm,lm,ku,L,lrec
      real     q(im,jm,lm)
      real*4 dum(im,jm)
      do L=1,lm
      if( lrec.eq.-1 ) then
          read(ku)              dum
      else
          read(ku,rec=lrec+1)   dum
                 lrec=lrec+1
      endif
         q(:,:,L) = dum(:,:)
      enddo
      return
      end subroutine readit

      subroutine flat2hdf ( q2d,q3d,name2d,name3d,titl2d,titl3d,n2d,n3d,undef,
     .                     im,jm,lm,lats,lons,levs,nymd,nhms,ninc,
     .                     id,create,filename,yrev,ecmwf )
      implicit none

c Input Variables
c ---------------
      integer  im,jm,lm,nymd,nhms,ninc,n2d,n3d
      logical  ecmwf

      real  q2d(im,jm,   n2d)
      real  q3d(im,jm,lm,n3d)

      character*256  name2d(n2d), titl2d(n2d)
      character*256  name3d(n3d), titl3d(n3d)
      character*256  filename

      logical create
      logical yrev

c Local Variables
c ---------------
      integer n,m,n3dEC

      real    lats(jm),lons(im),levs(lm)
      real    undef, dum
      integer precision,id,timeinc,rc,nhmsf
      integer i,j,L

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact

      integer nvars,idx

      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
      integer,       allocatable ::  lmvar(:)
      real,          allocatable :: vrange(:,:)
      real,          allocatable :: prange(:,:)
      real,          allocatable ::   tmpu(:,:,:)
      real,          allocatable ::   sphu(:,:,:)
      real,          allocatable ::     rh(:,:,:)

C **********************************************************************
C ****           Initialize Constants And Local Arrays              ****
C **********************************************************************
 
      if( ecmwf ) then ! add SPHU for ECMWF
          n3dEC = 1
      else
          n3dEC = 0
      endif

      nvars = n2d + n3d + n3dEC

C **********************************************************************
C ****                     Initialize GFIO File                     ****
C **********************************************************************
 
      allocate (    vname(nvars) )
      allocate (   vtitle(nvars) )
      allocate (   vunits(nvars) )
      allocate (    lmvar(nvars) )
      allocate (  tmpu(im,jm,lm) )
      allocate (  sphu(im,jm,lm) )
      allocate (    rh(im,jm,lm) )

      timeinc   = nhmsf(ninc)
      precision = 1  ! 64-bit
      precision = 0  ! 32-bit

      title    = 'Flat to HDF Format Conversion'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'level'

c Defined Fields
c --------------
      do m=1,n2d
             n  = m
             idx=index(name2d(m),'=')
       if(idx>0) then
       vname(n) = name2d(m)(1:idx-1)
       name2d(m) = trim(vname(n))
       else
       vname(n) = name2d(m)
       endif
      vtitle(n) = trim(titl2d(m))
      vunits(n) = 'unknown'
       lmvar(n) =  0
      enddo

      do m=1,n3d
             n  = n2d+m
             idx=index(name3d(m),'=')
       if(idx>0) then
       vname(n) = name3d(m)(1:idx-1)
       name3d(m) = trim(vname(n))
       else
       vname(n) = name3d(m)
       endif
      vtitle(n) = trim(titl3d(m))
      vunits(n) = 'unknown'
       lmvar(n) =  lm
      enddo

      if( ecmwf ) then
             n  = n2d+n3d+n3dEC
       vname(n) = 'sphu'
      vtitle(n) = 'Specific_Humidity_from_RH_x_QSAT'
      vunits(n) = 'kg/kg'
       lmvar(n) =  lm
      endif

C **********************************************************************
C ****                     Value Added Products                     ****
C **********************************************************************
 
c Create GFIO file
c ----------------
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )
      vrange(:,:) = undef
      prange(:,:) = undef

      if (create) then
             call GFIO_Create ( trim(filename), title, source, contact, undef,
     .                          im, jm, lm, lons, lats, levs, levunits,
     .                          nymd, nhms, timeinc,
     .                          nvars, vname, vtitle, vunits, lmvar,
     .                          vrange, prange, precision,
     .                          id, rc )
      endif

C **********************************************************************
C ****                    Write Defined Fields                      ****
C **********************************************************************

      do n=1,n2d
         call writit( q2d(1,1,n)  ,im,jm,0,1,id,name2d(n),nymd,nhms,yrev )
      enddo

      do n=1,n3d
         call writit( q3d(1,1,1,n),im,jm,1,lm,id,name3d(n),nymd,nhms,yrev )
         if( name3d(n).eq.'Temperature'       ) tmpu = q3d(:,:,:,n)
         if( name3d(n).eq.'Relative_humidity' )   rh = q3d(:,:,:,n)
      enddo

      if( ecmwf ) then
          do L=1,lm
          do j=1,jm
          do i=1,im
             call qsat (tmpu(i,j,L),levs(L),sphu(i,j,L),dum,.false.)
             sphu(i,j,L) = rh(i,j,L)*sphu(i,j,L)*0.01
          enddo
          enddo
          enddo
          call writit( sphu,im,jm,1,lm,id,vname(nvars),nymd,nhms,yrev )
      endif
     
C **********************************************************************
C ****                  De-Allocate Dynamics Arrays                 ****
C **********************************************************************

      deallocate (   vname )
      deallocate (  vtitle )
      deallocate (  vunits )
      deallocate (   lmvar )
      deallocate (  vrange )
      deallocate (  prange )
      deallocate (  tmpu   )
      deallocate (  sphu   )
      deallocate (  rh     )
      return
      end subroutine flat2hdf

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

      subroutine read_ctl ( ctlfile,im,jm,lm,n2d,n3d,undef,lrec,
     .                      nvars,names,Lsurf,name2d,titl2d,name3d,titl3d,
     .                      lats,lons,levs,yrev,endian )
      implicit none

      logical,       pointer :: Lsurf (:)
      character*256, pointer :: names (:)
      character*256, pointer :: name2d(:), name3d(:)
      character*256, pointer :: titl2d(:), titl3d(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)
      real,          pointer ::   levs(:)

      character*256 ctlfile
      integer       im,jm,lm,n2d,n3d,nvars,lrec
      real          undef,dx,dy,dz
      integer       i,j,L,n,ndum
      character*256 dummy,name,endian,dimstring
      character*256, allocatable :: dum(:)
      logical        yrev

      endian = 'NULL'

C **********************************************************************
C ****               Read Grads CLT File for Meta Data              ****
C **********************************************************************

      open (10,file=trim(ctlfile),form='formatted')
      n2d = 0
      n3d = 0
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
              if( trim(dum(ndum)).eq.'sequential'    ) lrec   = -1
              if( trim(dum(ndum)).eq.'yrev'          ) yrev   = .true.
              if( trim(dum(ndum)).eq.'big_endian'    ) endian = 'big_endian'
              if( trim(dum(ndum)).eq.'little_endian' ) endian = 'little_endian'
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
                   read(10,*) name2d(n2d),L,dimstring,titl2d(n2d)
               else
                   Lsurf(n) = .false.
                   n3d = n3d + 1
                   read(10,*) name3d(n3d),L,dimstring,titl3d(n3d)
               endif
           enddo
           endif
      enddo
  501 continue

      return
      end subroutine read_ctl

      subroutine writit (q,im,jm,lbeg,lm,id,name,nymd,nhms,yrev)
      integer im,jm,lm,L
      integer id,nymd,nhms,rc,lbeg
      character*256 name
      logical       yrev
      real    q (im,jm,lm),qdum(jm)

      if( yrev ) then
          do L=1,lm
          do i=1,im
             do j=1,jm
             qdum(jm-j+1) = q(i,j,L)
             enddo
             do j=1,jm
             q(i,j,L) = qdum(j)
             enddo
          enddo
          enddo
      endif
         call Gfio_putVar ( id,trim(name),nymd,nhms,im,jm,lbeg,lm,q,rc )
      print *, '       Writing variable: ',trim(name)
      return
      end subroutine writit

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " flat2hdf_$ARCH.x  -flat  fname(s)"
      print *, "                   -ctl   ctl_fname"
      print *, "                   -nymd  nymd"
      print *, "                   -nhms  nhms"
      print *, "                   -ndt   ndt"
      print *
      print *, "where:"
      print *
      print *, "  -flat   fname(s):  Filename(s) in flat real*4 binary format"
      print *, "  -ctl   ctl_fname:  CTL Filename for flat binary files"
      print *, "  -nymd       nymd:  Beginning  YYYYMMDD"
      print *, "  -nhms       nhms:  Beginning    HHMMSS"
      print *, "  -ndt         ndt:  Time Increment (secs)"
      print *
      print *, "Note:"
      print *, "  ALL Grads Keywords MUST BE lowercase (eg: xdef, options, sequential, etc.)"
      print *
      print *
      call exit(7)
      end subroutine usage

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
