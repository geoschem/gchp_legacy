      program  main
      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****     Program to create ETA.ANA file from NCEP sigma files     ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************

      integer  im,jm
      integer  :: lm = 72 
      integer  :: nq =  3 ! 1:qv, 2:oz, 3:ql

c Set analysis, fvdas, date and time
c ----------------------------------
      character*2  cnhms
      character*8  cnymd

      character*256 ana_data, tag, ext

      integer    nymd,nhms

c fv restart variables and topography
c -----------------------------------
      real, allocatable ::   dp(:,:,:)
      real, allocatable ::    u(:,:,:)
      real, allocatable ::    v(:,:,:)
      real, allocatable ::  thv(:,:,:)
      real, allocatable ::    q(:,:,:,:)
      real, allocatable :: phis(:,:)
      real, allocatable ::   ps(:,:)

      integer :: timinc = 21600

c Analysis variables
c ------------------
      real, allocatable :: phis_ana(:,:)
      real, allocatable ::   ps_ana(:,:)
      real, allocatable ::    u_ana(:,:,:)
      real, allocatable ::    v_ana(:,:,:)
      real, allocatable ::    q_ana(:,:,:,:)
      real, allocatable ::   dp_ana(:,:,:)
      real, allocatable ::    t_ana(:,:,:)
      integer mlev

      character*120, allocatable :: arg(:)

      integer precision
      integer n,nargs,iargc

c NCEP Grads CTL File Variables
c -----------------------------
      character*256  ctlfile,format
      integer imncep,jmncep,lmncep,nvncep
      real     uncep

      character*256, pointer :: names (:)
      character*256, pointer :: descs (:)
      integer,       pointer :: lmvars(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)

      interface
                subroutine read_ctl ( ctlfile,imncep,jmncep,lmncep,uncep,format,
     .                                nvncep,names,descs,lmvars,
     .                                lats,lons,nymd,nhms )
                character*256  ctlfile, format
                character*256, pointer :: names (:)
                character*256, pointer :: descs (:)
                integer,       pointer :: lmvars(:)
                real,          pointer :: lats(:)
                real,          pointer :: lons(:)
                integer        imncep,jmncep,lmncep,nvncep
                integer        nymd,nhms
                real           uncep
            end subroutine read_ctl
      end interface

C **********************************************************************
C ****           Initialize Filenames, Methods, etc.                ****
C **********************************************************************

      precision = 0       ! 32-bit
            ext = 'nc4'
            tag = ''

          nargs = iargc()
      if( nargs.eq.0 ) then
          call usage()
      else
          allocate ( arg(nargs) )
          do n=1,nargs
          call getarg(n,arg(n))
          enddo
          do n=1,nargs
             if( trim(arg(n)).eq.'-ncep' ) ana_data = trim(arg(n+1))
             if( trim(arg(n)).eq.'-ctl'  )  ctlfile = trim(arg(n+1))
             if( trim(arg(n)).eq.'-tag'  )      tag = trim(arg(n+1))
          enddo
      endif

          if( trim(tag).ne.'' ) tag = trim(tag) // '.'

C **********************************************************************
C ****                 Read NCEP Grads CTL File                     ****
C **********************************************************************

          call read_ctl ( ctlfile,imncep,jmncep,lmncep,uncep,format,
     .                    nvncep,names,descs,lmvars,
     .                    lats,lons,nymd,nhms )

          print *
          print *, '      NCEP filename: ',trim(ana_data)
          print *, '      NCEP CTL File: ',trim(ctlfile)
          print *, '             Format: ',trim(format)
          print *, '        Date & Time: ',nymd,nhms
          print *, '               rslv: ',imncep,jmncep,lmncep
          print *, '              undef: ',uncep
          print *
          print *, 'Number of Variables: ',nvncep
          print *
          do n=1,nvncep
          write(6,1001) trim(names(n)),trim(descs(n)),lmvars(n)
          enddo
 1001     format(1x,a8,2x,a20,2x,i3)
          print *

            im = imncep
            jm = jmncep
          mlev = lmncep

      write( cnymd,200 ) nymd
      write( cnhms,300 ) nhms/10000
  200 format(i8.8)
  300 format(i2.2)
  400 format('dset ^',a)
  600 format(a1,i2.2)

C **********************************************************************
C ****                      Get NCEP Analysis                       ****
C **********************************************************************

          print *, 'Reading NCEP Analysis for Date: ',nymd,' Time: ',nhms

          allocate (  dp_ana(im,jm,mlev)    )
          allocate (   u_ana(im,jm,mlev)    )
          allocate (   v_ana(im,jm,mlev)    )
          allocate (   t_ana(im,jm,mlev)    )
          allocate (   q_ana(im,jm,mlev,nq) )
          allocate (  ps_ana(im,jm) )
          allocate (phis_ana(im,jm) )

          call get_ncep_map ( ana_data,format,ps_ana,dp_ana,u_ana,v_ana,t_ana,q_ana,phis_ana,
     .                        im,jm,mlev,nq,nvncep,names,lmvars )

C **********************************************************************
C ****               Check for Negative Humidity                    ****
C **********************************************************************

      do n=1,nq
      call QFILL ( q_ana(1,1,1,n),dp_ana,im,jm,mlev )
      enddo

C **********************************************************************
C ****                     Adjust fv Restart                        ****
C **********************************************************************

      print *, 'Calling Remap'

      allocate (  dp(im,jm,lm)    )
      allocate (   u(im,jm,lm)    )
      allocate (   v(im,jm,lm)    )
      allocate ( thv(im,jm,lm)    )
      allocate (   q(im,jm,lm,nq) )
      allocate (  ps(im,jm) )
      allocate (phis(im,jm) )

      phis = phis_ana
        ps =   ps_ana

      call remap  ( ps,dp,u,v,thv,q,lm,
     .              ps_ana,dp_ana,u_ana,v_ana,t_ana,q_ana,mlev,im,jm,nq )


C **********************************************************************
C ****                 Write NCEP ANA.ETA File                      ****
C **********************************************************************

      call put_fveta ( ps,dp,u,v,thv,q,phis,
     .                 im,jm,lm,nq,nymd,nhms,tag,ext,lons(1),
     .                 timinc,precision )

      stop
      end

      subroutine QFILL ( Q,DP,IM,JM,LM )
      implicit  none
      integer   IM,JM,LM,L
      real    Q(IM,JM,LM)
      real   DP(IM,JM,LM)
      real*8, allocatable, dimension(:,:) :: QTEMP1
      real*8, allocatable, dimension(:,:) :: QTEMP2
      allocate(QTEMP1(IM,JM))
      allocate(QTEMP2(IM,JM))

      QTEMP1 = 0.0
      do L=1,LM
      QTEMP1(:,:) = QTEMP1(:,:) + Q(:,:,L)*DP(:,:,L)
      enddo

      where( Q < 0.0 ) Q = 0.0

      QTEMP2 = 0.0
      do L=1,LM
      QTEMP2(:,:) = QTEMP2(:,:) + Q(:,:,L)*DP(:,:,L)
      enddo

      where( qtemp2.ne.0.0 )
             qtemp2 = max( qtemp1/qtemp2, 0.0 )
      end where

      do L=1,LM
      Q(:,:,L) = Q(:,:,L)*qtemp2(:,:)
      enddo

      deallocate(QTEMP1)
      deallocate(QTEMP2)
      return
      end

      subroutine put_fveta ( ps,dp,u,v,thv,q,phis,
     .                       im,jm,lm,nq,nymd,nhms,tag,ext,lonbeg,
     .                       timeinc,precision )
      use MAPL_BaseMod, only: MAPL_UNDEF
      use m_set_eta, only: set_eta
      implicit  none

      integer   im,jm,lm,nq,nymd,nhms
      real phis(im,jm)
      real   ps(im,jm)
      real   dp(im,jm,lm)
      real    u(im,jm,lm)
      real    v(im,jm,lm)
      real  thv(im,jm,lm)
      real    q(im,jm,lm,nq)
      integer timeinc

      real  ple(im,jm,lm+1)
      real  pke(im,jm,lm+1)
      real   pk(im,jm,lm)
      real   tv(im,jm,lm)
      real    t(im,jm,lm)

      real lats(jm)
      real lons(im)
      real levs(lm)
      real   ak(lm+1)
      real   bk(lm+1)

      real  rgas,rvap,eps,kappa,grav
      real  ptop,dlon,dlat,pref,dpref(lm),undef,lonbeg,pint
      integer i,j,L,m,n,rc,ks
      character*256 tag,ext,filename, fname
      integer nvars,fid,precision

      character*256  levunits
      character*256  title
      character*256  source
      character*256  contact
      character*256, allocatable, dimension(:) ::   vname
      character*256, allocatable, dimension(:) ::  vtitle
      character*256, allocatable, dimension(:) ::  vunits
      integer,       allocatable, dimension(:) ::   lmvar

      real,          allocatable :: v_range(:,:)
      real,          allocatable :: p_range(:,:)

      character*2 cnhms
      character*8 cnymd

      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      kappa = 2.0/7.0
      grav  = 9.80

      undef = MAPL_UNDEF
      write( cnymd,200 ) nymd
      write( cnhms,300 ) nhms/10000
  200 format(i8.8)
  300 format(i2.2)
      fname = trim(tag) // 'gg2eta.' // trim(cnymd) // '_' // trim(cnhms) // 'z.' // trim(ext)
      print *, 'Creating 32-bit eta file: ',trim(fname)

      call set_eta ( lm,ks,ptop,pint,ak,bk )

! Construct T, TV
! ---------------
      do L=1,lm+1
      ple(:,:,L) = ak(L) + ps(:,:)*bk(L)
      enddo
      pke(:,:,:) = ple(:,:,:)**kappa
      do L=1,lm
      pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )
     .          / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo
      tv = thv*pk
      t(:,:,:) = tv(:,:,:)/(1+eps*q(:,:,:,1))

c String and vars settings
c ------------------------
      title    = 'FVGCM Dynamics State Vector (Hybrid Coordinates)'
      source   = 'Goddard Modeling and Assimilation Office, NASA/GSFC'
      contact  = 'data@gmao.gsfc.nasa.gov'
      levunits = 'hPa'

      nvars = 9

      allocate (   vname(nvars) )
      allocate (  vtitle(nvars) )
      allocate (  vunits(nvars) )
      allocate (   lmvar(nvars) )
      allocate ( v_range(2,nvars) )
      allocate ( p_range(2,nvars) )

             n  =  1
       vname(n) = 'phis'
      vtitle(n) = 'Topography geopotential'
      vunits(n) = 'meter2/sec2'
       lmvar(n) =  0

             n  =  n + 1
       vname(n) = 'ps'
      vtitle(n) = 'Surface Pressure'
      vunits(n) = 'Pa'
       lmvar(n) =  0

             n  =  n + 1
       vname(n) = 'dp'
      vtitle(n) = 'Pressure Thickness'
      vunits(n) = 'Pa'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'u'
      vtitle(n) = 'eastward_wind'
      vunits(n) = 'm/s'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'v'
      vtitle(n) = 'northward_wind'
      vunits(n) = 'm/s'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'tv'
      vtitle(n) = 'air_virtual_temperature'
      vunits(n) = 'K'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'qv'
      vtitle(n) = 'Specific Humidity Vapor'
      vunits(n) = 'kg/kg'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'ozone'
      vtitle(n) = 'Ozone'
      vunits(n) = 'ppmv'
       lmvar(n) =  lm

             n  =  n + 1
       vname(n) = 'ql'
      vtitle(n) = 'Mass Fraction Cloud Liquid Water'
      vunits(n) = 'kg/kg'
       lmvar(n) =  lm

      v_range(:,:) = undef
      p_range(:,:) = undef

c Compute grid
c ------------
      dlon = 360.0/ im
      dlat = 180.0/(jm-1)

      do j=1,jm
      lats(j) = -90.0 + (j-1)*dlat
      enddo
      do i=1,im
      lons(i) =   lonbeg + (i-1)*dlon
      enddo
      do L=1,lm
      dpref(L) = (ak(L+1)-ak(L)) + (bk(L+1)-bk(L))*98400.0
      enddo
      pref    = ptop + 0.5*dpref(1)
      levs(1) = pref
      do L=2,lm
      pref    = pref + 0.5*( dpref(L)+dpref(L-1) )
      levs(L) = pref
      enddo
      levs(:) = levs(:)/100

c Create GFIO file
c ----------------
      call GFIO_Create ( fname, title, source, contact, undef,
     .                   im, jm, lm, lons, lats, levs, levunits,
     .                   nymd, nhms, timeinc,
     .                   nvars, vname, vtitle, vunits, lmvar,
     .                   v_range, p_range, precision,
     .                   fid, rc )
c Write GFIO data
c ---------------
                                                                       n = 1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,0, 1,phis,rc ) ; n = n+1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,0, 1,ps  ,rc ) ; n = n+1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,1,lm,dp  ,rc ) ; n = n+1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,1,lm,u   ,rc ) ; n = n+1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,1,lm,v   ,rc ) ; n = n+1
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,1,lm,tv  ,rc ) ; n = n+1
      do m=1,nq
      call Gfio_putVar ( fid,vname(n),nymd,nhms,im,jm,1,lm,q(1,1,1,m),rc ) ; n = n+1
      enddo

c Write GFIO global attributes
c ----------------------------
      call GFIO_PutRealAtt ( fid,'ak',  lm+1,ak   ,precision,rc )
      call GFIO_PutRealAtt ( fid,'bk',  lm+1,bk   ,precision,rc )

      call gfio_close ( fid,rc )
      return
      end

      subroutine getfile ( ku,filename,irec )
      implicit none
      character(len=*) filename
      integer ku,irec

      if ( irec>0 ) then

          open (ku,file=trim(filename),form='unformatted',access='direct',recl=irec)
          return

      else

          open (ku,file=trim(filename),form='unformatted',access='sequential',convert='big_endian')
          read (ku, err=1001)  ! Check for BIG_ENDIAN

 5000     backspace(ku)
          return

 1001     close(ku)
          print *, 'File: ',trim(filename)
          print *, 'Failed to OPEN using BIG_ENDIAN, will try LITTLE_ENDIAN'
          open (ku,file=trim(filename),form='unformatted',access='sequential',convert='little_endian')
          read (ku, err=1002)  ! Check for LITTLE_ENDIAN
          goto 5000

 1002     continue
          print *, 'ERROR!!  File: ',trim(filename)
          print *, 'ERROR!!  is neither BIG nor LITTLE ENDIAN'
          call exit(7)

      endif
      end

      subroutine get_ncep_map ( filename,format,ps,dp,u,v,t,q,phis,im,jm,mlev,nq,
     .                          nvars,names,lmvars )
      use MAPL_ConstantsMod
      implicit none
      integer  im,jm,mlev,nq
      integer              nvars
      integer       lmvars(nvars)
      character*256  names(nvars)
      character*256  filename, format

      real     ps(im,jm)
      real     dp(im,jm,mlev)
      real      u(im,jm,mlev)
      real      v(im,jm,mlev)
      real      t(im,jm,mlev)
      real      q(im,jm,mlev,nq)
      real   phis(im,jm)

      real*4,  allocatable :: qncep(:,:,:,:)

      real,   parameter :: voltomas =  1.655E-6
      real*4, parameter :: undef4   = -9.99E+33
      real    undef,kappa,grav
      integer L,i,j,n,irec,mrec

      inquire(iolength=irec) undef4
      irec = im*jm*irec

      if( trim(format).eq.'direct' ) then
          call getfile ( 30,trim(filename),irec )
      else 
          call getfile ( 30,trim(filename),0 )
      endif

      allocate ( qncep(im,jm,mlev,nvars) )

      undef = undef4
      kappa = MAPL_KAPPA
      grav  = MAPL_GRAV
      qncep = 0.0   ! Initialize all NCEP variables to zero

c Read NCEP Variables
c -------------------
      mrec = 1
      do n=1,nvars
          if( trim(format).eq.'direct' ) then
              do L=1,lmvars(n)
              read(30,rec=mrec) ((qncep(i,j,mlev-L+1,n),i=1,im),j=jm,1,-1)
              mrec=mrec+1
              enddo
          else
              do L=1,lmvars(n)
              read(30)          ((qncep(i,j,mlev-L+1,n),i=1,im),j=jm,1,-1)
              enddo
          endif
      enddo


c Load GMAO Variables
c -------------------
      do n=1,nvars
      if( trim(names(n)).eq.'HS' ) phis = qncep(:,:,mlev,n)       ! Surface Geopotential
      if( trim(names(n)).eq.'PS' )   ps = qncep(:,:,mlev,n)       ! Surface Pressure
      if( trim(names(n)).eq.'DP' )   dp = qncep(:,:,:,   n)       ! Surface Pressure Thickness
      if( trim(names(n)).eq.'T'  )    t = qncep(:,:,:,   n)       ! Sensible/Dry-bulb Temperature
      if( trim(names(n)).eq.'U'  )    u = qncep(:,:,:,   n)       ! U-Wind
      if( trim(names(n)).eq.'V'  )    v = qncep(:,:,:,   n)       ! V-Wind
      if( trim(names(n)).eq.'Q'  )    q(:,:,:,1) = qncep(:,:,:,n) ! Specific Humidity
      if( trim(names(n)).eq.'Q2' )    q(:,:,:,2) = qncep(:,:,:,n) ! Ozone
      if( trim(names(n)).eq.'Q3' )    q(:,:,:,3) = qncep(:,:,:,n) ! Cloud Liquid Water
      enddo

c Scale GMAO Variables
c --------------------
      phis = phis*grav
      q(:,:,:,2) = q(:,:,:,2)/voltomas

      deallocate ( qncep )
      close (30)
      return
      end

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " gg2fv.x [-ncep  ncep.data]"
      print *, "         [-ctl   ncep.ctl]"
      print *, "         [-tag   tag]"
      print *
      print *, "where:"
      print *
      print *, "  -ncep ncep.data: Filename of NCEP sigma-level analysis data (from ss2gg)"
      print *, "  -ctl  ncep.ctl : Filename of NCEP sigma-level analysis ctl  (from ss2gg)"
      print *, "  -tag       tag : Optional Prefix tag for output files"
      print *
      call exit(7)
      end

      subroutine read_ctl ( ctlfile,im,jm,lm,undef,format,
     .                      nvars,names,descs,lmvars,
     .                      lats,lons,nymd,nhms )
C **********************************************************************
C ****               Read Grads CTL File for Meta Data              ****
C **********************************************************************

      implicit none

      character*256, pointer ::  names(:)
      character*256, pointer ::  descs(:)
      integer,       pointer :: lmvars(:)
      real,          pointer ::   lats(:)
      real,          pointer ::   lons(:)

      character*256 ctlfile, format
      integer       im,jm,lm,nvars
      real          undef,dx,dy
      integer       i,j,m,n,ndum,len
      integer       nymd,nhms
      character*1   c
      character*256 dummy,udummy
      character*256 cyear,chour,cmonth,cday,cnymd,cnhms
      character*256, allocatable :: dum(:)

      open (10,file=trim(ctlfile),form='formatted')
      format = 'direct'
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
              if( trim(dum(ndum)).eq.'sequential' ) format = 'sequential'
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

c TDEF
c ----
           if( trim(dummy).eq.'tdef' ) then
               backspace(10)
               read(10,*) dummy,j,dummy,dummy
               len = len_trim(dummy)
               udummy = ''
               do i=1,len
                  c = dummy(i:i)
                  if( ichar(c).ge.97 .and. ichar(c).le.122 ) then
                      c = achar( ichar(c)-32 )
                  endif
                  udummy = trim(udummy) // c
               enddo
               dummy  = udummy
               chour  =  dummy(1:2)
               cday   =  dummy(4:5)
               cmonth =  dummy(6:8)
               cyear  =  dummy(9:12)
               if( cmonth == 'JAN' ) cmonth = '01'
               if( cmonth == 'FEB' ) cmonth = '02'
               if( cmonth == 'MAR' ) cmonth = '03'
               if( cmonth == 'APR' ) cmonth = '04'
               if( cmonth == 'MAY' ) cmonth = '05'
               if( cmonth == 'JUN' ) cmonth = '06'
               if( cmonth == 'JUL' ) cmonth = '07'
               if( cmonth == 'AUG' ) cmonth = '08'
               if( cmonth == 'SEP' ) cmonth = '09'
               if( cmonth == 'OCT' ) cmonth = '10'
               if( cmonth == 'NOV' ) cmonth = '11'
               if( cmonth == 'DEC' ) cmonth = '12'

               cnymd = trim(cyear) // trim(cmonth) // trim(cday)
               cnhms = trim(chour) // '0000'
               read( cnymd,* ) nymd
               read( cnhms,* ) nhms
           endif

c ZDEF
c ----
           if( trim(dummy).eq.'zdef' ) then
           backspace(10)
           read(10,*) dummy,lm
#if 0
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
#endif
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
           allocate(  names(nvars) )
           allocate(  descs(nvars) )
           allocate( lmvars(nvars) )

           do n=1,nvars
           read(10,*)  names(n),lmvars(n),m,descs(n)
           if( lmvars(n).eq.0 ) lmvars(n) = 1
           enddo

           endif
      enddo
  500 continue
      rewind(10)

      if( nvars.eq.0 ) then
          print *, 'Warning, nvars = 0!'
          stop
      endif

      return
      end subroutine read_ctl

      subroutine remap  ( ps1,dp1,u1,v1,thv1,q1,lm1,
     .                    ps2,dp2,u2,v2,t2  ,q2,lm2,im,jm,nq )

C***********************************************************************
C
C  Purpose
C     Driver for remapping input analysis (2) to output model levels (1)
C
C  Argument Description
C     ps1 ...... model surface  pressure
C     dp1 ...... model pressure thickness
C     u1 ....... model zonal      wind
C     v1 ....... model meridional wind
C     thv1 ..... model virtual potential  temperature
C     q1 ....... model specific   humidity
C     oz1 ...... model ozone
C     lm1 ...... model vertical   dimension
C
C     ps2 ...... analysis surface  pressure
C     dp2 ...... analysis pressure thickness
C     u2 ....... analysis zonal      wind
C     v2 ....... analysis meridional wind
C     t2 . ..... analysis dry-bulb temperature
C     q2 ....... analysis specific   humidity
C     oz2 ...... analysis ozone
C     lm2 ...... analysis vertical   dimension
C
C     im ....... zonal      dimension
C     jm ....... meridional dimension
C     nq ....... number of  tracers
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use MAPL_ConstantsMod
      use m_set_eta, only: set_eta
      implicit none
      integer  im,jm,lm1,lm2,nq

c fv-DAS variables
c ----------------
      real      dp1(im,jm,lm1)
      real       u1(im,jm,lm1)
      real       v1(im,jm,lm1)
      real     thv1(im,jm,lm1)
      real       q1(im,jm,lm1,nq)
      real      ps1(im,jm)

      real      ak(lm1+1)
      real      bk(lm1+1)

c Target analysis variables
c -------------------------
      real     dp2(im,jm,lm2)
      real      u2(im,jm,lm2)
      real      v2(im,jm,lm2)
      real      t2(im,jm,lm2)
      real    thv2(im,jm,lm2)
      real      q2(im,jm,lm2,nq)
      real     ps2(im,jm)

c Local variables
c ---------------
      real   pe1(im,jm,lm1+1)
      real   pe2(im,jm,lm2+1)
      real   pk2(im,jm,lm2  )
      real  pke1(im,jm,lm1+1)
      real  pke2(im,jm,lm2+1)

      real    kappa,cp,ptop,pl,alf,pint
      real    rgas,pref,tref,pkref,tstar,eps,rvap,grav
      integer i,j,L,n,ks

      kappa = MAPL_KAPPA
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      eps   = rvap/rgas-1.0

c Construct target analysis pressure variables
c --------------------------------------------
      do j=1,jm
      do i=1,im
      pe2(i,j,lm2+1) = ps2(i,j)
      enddo
      enddo

      do L=lm2,1,-1
      do j=1,jm
      do i=1,im
      pe2(i,j,L) = pe2(i,j,L+1) - dp2(i,j,L)
      enddo
      enddo
      enddo

      do j=1,jm
      do i=1,im
      pe2(i,j,1) = max( pe2(i,j,1),1.0 )  ! Set ptop = 0.01 mb (rather than 0.0 mb from NCEP)
      enddo
      enddo

      do L=1,lm2+1
      do j=1,jm
      do i=1,im
      pke2(i,j,L) = pe2(i,j,L)**kappa
      enddo
      enddo
      enddo

c Construct target virtual potential temperature
c ----------------------------------------------
      do L=1,lm2
      do j=1,jm
      do i=1,im
       pk2(i,j,L) = ( pke2(i,j,L+1)-pke2(i,j,L) )/( kappa*log(pe2(i,j,L+1)/pe2(i,j,L)) )
      thv2(i,j,L) =     t2(i,j,L)*( 1.0+eps*max(0.0,q2(i,j,L,1)) )/pk2(i,j,L)
      enddo
      enddo
      enddo

c Construct fv pressure variables using surface pressure and AK & BK
c ------------------------------------------------------------------
      call set_eta ( lm1,ks,ptop,pint,ak,bk )

      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1(i,j,L) = ak(L) + bk(L)*ps1(i,j)
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

c Map Input Analysis onto fv grid
c -------------------------------
      call gmap ( im,jm,nq, kappa,
     .            lm2,  pke2,  pe2, u2,  v2,  thv2,  q2,
     .            lm1,  pke1,  pe1, u1,  v1,  thv1,  q1)

      return
      end
