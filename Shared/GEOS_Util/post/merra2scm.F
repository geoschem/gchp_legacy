
      program  main

      implicit none
c **********************************************************************
c ****                                                              ****
c ****  Program to create scm driving dataset from MERRA output     ****
c ****                                                              ****
c **********************************************************************

      integer begdate,begtime,enddate,endtime
      real lonbegin,lonend,latbegin,latend
!      parameter ( begdate = 19970618 )
c      parameter ( begdate = 19921220 )
c      parameter ( begdate = 19921228 )
!      parameter ( begtime = 000000 )
!      parameter ( enddate = 19970717)
c      parameter ( enddate = 19921130)
c      parameter ( enddate = 19930110)
c      parameter ( enddate = 19930102)
c      parameter ( enddate = 19930202)
!      parameter ( endtime = 230000 )

! arm97

!      parameter ( begdate = 19970618 )
!      parameter ( begtime = 000000 )
!      parameter ( enddate = 19970717)
!      parameter ( endtime = 230000 )
!      parameter ( lonbegin = -100.000 )
!      parameter ( lonend = -95.0000 )
!      parameter ( latbegin = 34.0000 )
!      parameter ( latend = 38.0000 )

! coare
!      parameter ( begdate = 19921101 )
!      parameter ( begtime = 000000 )
!      parameter ( enddate = 19930228)
!      parameter ( endtime = 230000 )

!      parameter ( lonbegin = 152.000 )
!      parameter ( lonend = 158.0000 )
!      parameter ( latbegin = -5.0000 )
!      parameter ( latend = 2.0000 )

! arm scsmex

      parameter ( begdate =  20130310)
      parameter ( begtime = 000000 )
      parameter ( enddate =  20130601)
c      parameter ( enddate =  20130320)
      parameter ( endtime = 230000 )
c      parameter ( lonbegin = -158.000 )
c      parameter ( lonend = -156.0000 )
c      parameter ( latbegin = 70.0000 )
c      parameter ( latend = 72.0000 )

c      parameter ( lonbegin = -158.000 )
c      parameter ( lonend = -156.0000 )
c      parameter ( latbegin = 50.0000 )
c      parameter ( latend = 52.0000 )

      parameter ( lonbegin = -52.000 )
      parameter ( lonend = -50.0000 )
      parameter ( latbegin = 66.0000 )
      parameter ( latend = 68.0000 )


c Generic Model Variables
c -----------------------
      real, allocatable ::  ps(:,:)
      real, allocatable ::  ts(:,:)
      real, allocatable ::  eflux(:,:)
      real, allocatable ::  hflux(:,:)
      real, allocatable ::  prectot(:,:)
      real, allocatable ::  t(:,:,:)
      real, allocatable ::  qv(:,:,:)
      real, allocatable ::  u(:,:,:)
      real, allocatable ::  v(:,:,:)
      real, allocatable ::  omega(:,:,:)
      real, allocatable ::  dtdtdyn(:,:,:)
      real, allocatable ::  dqvdtdyn(:,:,:)
      real, allocatable ::  plevs(:)
      real, allocatable ::  lonN(:)
      real, allocatable ::  latN(:)
      real, allocatable ::  lonC(:)
      real, allocatable ::  latC(:)
c--------SVETA---------------       
      real, allocatable ::  dtdtana(:,:,:)
      real, allocatable ::  dqvdtana(:,:,:)
c-------------------------------------
      real psscm(1000), tsscm(1000), efluxscm(1000), hfluxscm(1000), prectotscm(1000)
      real tscm(1000,100), qvscm(1000,100), uscm(1000,100), vscm(1000,100)
      real omegascm(1000,100), dtdtdynscm(1000,100), dqvdtdynscm(1000,100)
c------------SVETA-------------------
      real dtdtanascm(1000,100), dqvdtanascm(1000,100)
c---------------------------------
      integer yearsscm(1000), monthsscm(1000), daysscm(1000), hoursscm(1000), minutesscm(1000)
      real timesscm(1000),plevsscm(100)
      integer modymd
      real undef

      character*256 dsnprefix(5)
      character*256 dirname

      integer id,id1,id2,id3,n,ntimes,nymd,nhms,nvars,rc
      integer imN,jmN,lmN,imC,jmC,lmC
      integer index

      dirname = '/discover/nobackup/aeichman/barrow/data/'
      dsnprefix(1) = 'd5_merra_jan98.tavg1_2d_slv_Nx'
      dsnprefix(2) = 'd5_merra_jan98.tavg1_2d_flx_Nx'
      dsnprefix(3) = 'd5_merra_jan98.tavg3_3d_tdt_Cp'
      dsnprefix(4) = 'd5_merra_jan98.tavg3_3d_qdt_Cp'
      dsnprefix(5) = 'd5_merra_jan98.inst3_3d_asm_Cp'

!      dsnprefix(1) = 'd5_merra_jan98.tavg1_2d_slv_Nx'
!      dsnprefix(2) = 'd5_merra_jan98.tavg1_2d_flx_Nx'
!      dsnprefix(3) = 'd5_merra_jan98.tavg3_3d_tdt_Cp'
!      dsnprefix(4) = 'd5_merra_jan98.tavg3_3d_qdt_Cp'
!      dsnprefix(5) = 'd5_merra_jan98.inst3_3d_asm_Cp'


      nymd = begdate
      nhms = begtime
      n = 0

CC  Loop over all input times

      do while (nymd.le.enddate)
      n = n+1
      print *,' nymd ',nymd,' nhms ',nhms
      
      yearsscm(n) = nymd/10000
      monthsscm(n) = (nymd - yearsscm(n)*10000)/100
      daysscm(n) = nymd - yearsscm(n)*10000 - monthsscm(n)*100
      hoursscm(n) = nhms/10000
      minutesscm(n) = (nhms - hoursscm(n)*10000)/100
      timesscm(n) = float(modymd(nymd)) + float(hoursscm(n))/24.

      call read_file_diminfo1(id,dirname,dsnprefix(5),nymd,nhms,imC,jmC,lmC,nvars,ntimes,rc)
      allocate(t(imC,jmC,lmC))
      allocate(qv(imC,jmC,lmC))
      allocate(u(imC,jmC,lmC))
      allocate(v(imC,jmC,lmC))
      allocate(omega(imC,jmC,lmC))
      allocate(plevs(lmC))
      allocate(lonC(imC))
      allocate(latC(jmC))
      call read_asm_data (id,t,qv,u,v,omega,latC,lonC,imC,jmC,lmC,plevs,nvars,ntimes,undef,rc)
      do index=1,lmC
       plevsscm(index) = plevs(index)
      enddo
      deallocate(plevs)

      call tick(nymd,nhms,1800)
      call read_file_diminfo3(id1,id2,id3,dirname,dsnprefix(1),nymd,nhms,imN,jmN,lmN,nvars,ntimes,rc)
      allocate(lonN(imN))
      allocate(latN(jmN))
      allocate(ps(imN,jmN))
      allocate(ts(imN,jmN))
      allocate(plevs(1))
      call read_slv_data (id1,id2,id3,ps,ts,latN,lonN,imN,jmN,lmN,plevs,nvars,ntimes,rc)

      call read_file_diminfo3(id1,id2,id3,dirname,dsnprefix(2),nymd,nhms,imN,jmN,lmN,nvars,ntimes,rc)
      allocate(eflux(imN,jmN))
      allocate(hflux(imN,jmN))
      allocate(prectot(imN,jmN))
      call read_flx_data (id1,id2,id3,eflux,hflux,prectot,latN,lonN,imN,jmN,lmN,plevs,nvars,ntimes,rc)
      deallocate(plevs)

      call tick(nymd,nhms,3600)
      call read_file_diminfo1(id,dirname,dsnprefix(3),nymd,nhms,imC,jmC,lmC,nvars,ntimes,rc)
      allocate(dtdtdyn(imC,jmC,lmC))
      allocate(plevs(lmC))
      call read_tdt_data (id,dtdtdyn,latC,lonC,imC,jmC,lmC,plevs,nvars,ntimes,undef,rc)
c----------------------SVETA---------------------------
      
      call read_file_diminfo1(id,dirname,dsnprefix(3),nymd,nhms,imC,jmC,lmC,nvars,ntimes,rc)
      allocate(dtdtana(imC,jmC,lmC))
      call read_tdtana_data (id,dtdtana,latC,lonC,imC,jmC,lmC,plevs,nvars,ntimes,undef,rc)
c===========================================================================     
      call read_file_diminfo1(id,dirname,dsnprefix(4),nymd,nhms,imC,jmC,lmC,nvars,ntimes,rc)
      allocate(dqvdtdyn(imC,jmC,lmC))
      call read_qdt_data (id,dqvdtdyn,latC,lonC,imC,jmC,lmC,plevs,nvars,ntimes,undef,rc)
c-----------SVETA-----------------------------------------
      call read_file_diminfo1(id,dirname,dsnprefix(4),nymd,nhms,imC,jmC,lmC,nvars,ntimes,rc)
      allocate(dqvdtana(imC,jmC,lmC))
      call read_qdtana_data (id,dqvdtana,latC,lonC,imC,jmC,lmC,plevs,nvars,ntimes,undef,rc)    
c-----------------------------------------------------------------
      call getmylatlon2(latbegin,lonbegin,latend,lonend,latN,lonN,imN,jmN,ps,psscm(n))
      call getmylatlon2(latbegin,lonbegin,latend,lonend,latN,lonN,imN,jmN,ts,tsscm(n))
      call getmylatlon2(latbegin,lonbegin,latend,lonend,latN,lonN,imN,jmN,eflux,efluxscm(n))
      call getmylatlon2(latbegin,lonbegin,latend,lonend,latN,lonN,imN,jmN,hflux,hfluxscm(n))
      call getmylatlon2(latbegin,lonbegin,latend,lonend,latN,lonN,imN,jmN,prectot,prectotscm(n))
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,t,undef,tscm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,qv,undef,qvscm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,u,undef,uscm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,v,undef,vscm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,omega,undef,omegascm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,dtdtdyn,undef,dtdtdynscm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,dqvdtdyn,undef,dqvdtdynscm)
c------------------SVETA----------------------
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,dtdtana,undef,dtdtanascm)
      call getmylatlon3(n,latbegin,lonbegin,latend,lonend,latC,lonC,imC,jmC,lmC,dqvdtana,undef,dqvdtanascm)
c----------------------------------
      call tick(nymd,nhms,5400)

      deallocate(t)
      deallocate(qv)
      deallocate(u)
      deallocate(v)
      deallocate(omega)
      deallocate(lonC)
      deallocate(latC)
      deallocate(lonN)
      deallocate(latN)
      deallocate(plevs)
      deallocate(ps)
      deallocate(ts)
      deallocate(eflux)
      deallocate(hflux)
      deallocate(prectot)
      deallocate(dtdtdyn)
      deallocate(dqvdtdyn)
      deallocate(dtdtana)
      deallocate(dqvdtana)
      enddo

      print *,' number of time levels ',n
      call writer_meta(n,lmC,plevsscm,timesscm,yearsscm,monthsscm,daysscm,hoursscm,minutesscm)
c      call writer_upper(n,lmC,tscm,qvscm,uscm,vscm,omegascm,dtdtdynscm,dqvdtdynscm)
      call writer_upper(n,lmC,tscm,qvscm,uscm,vscm,omegascm,dtdtdynscm,dqvdtdynscm,dtdtanascm,dqvdtanascm)

      call writer_surface(n,psscm,tsscm,efluxscm,hfluxscm,prectotscm)

      stop
      end

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_file_diminfo1(id,dirname,dsnprefix,nymd,nhms,im,jm,lm,nvars,ntimes,rc)
      implicit none
      integer id,nymd,nhms,im,jm,lm,nvars,ntimes,rc
      character * 256 dirname,dsnprefix

      character * 256 dsname
      integer ngatts
      character*8 date0
      character*4 time0
      character*2 hour0,mins0

      write(date0,1000)  nymd
      write(hour0,2000)  nhms/10000
      write(mins0,2000) (nhms-(nhms/10000)*10000)/100
 1000 format(i8.8)
 2000 format(i2.2)
      time0 = trim(hour0)//trim(mins0)
      dsname = trim(dirname) // trim(dsnprefix) // "." // trim(date0) // "_" // trim(time0) // "z.hdf"

      call gfio_open(trim(dsname),1,id,rc)

      if( rc.ne.0) then
       print *,' Something wrong with file open ',trim(dsname)
       stop
      endif

      call gfio_diminquire (id,im,jm,lm,ntimes,nvars,ngatts,rc )

      end subroutine read_file_diminfo1

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_file_diminfo3 (id1,id2,id3,dirname,dsnprefix,nymd,nhms,im,jm,lm,nvars,ntimes,rc)
      implicit none
      integer id1,id2,id3,nymd,nhms,im,jm,lm,nvars,ntimes,rc
      character * 256 dirname,dsnprefix

      character * 256 dsname
      integer ngatts
      character*8 date0
      character*4 time0
      character*2 hour0,mins0
      integer locnymd, locnhms

      locnymd = nymd
      locnhms = nhms
      write(date0,1000)  locnymd
      write(hour0,2000)  locnhms/10000
      write(mins0,2000) (locnhms-(locnhms/10000)*10000)/100
 1000 format(i8.8)
 2000 format(i2.2)
      time0 = trim(hour0)//trim(mins0)
      dsname = trim(dirname) // trim(dsnprefix) // "." // trim(date0) // "_" // trim(time0) // "z.hdf"

      call gfio_open(trim(dsname),1,id1,rc)

      if( rc.ne.0) then
       print *,' Something wrong with file open ',trim(dsname)
       stop
      endif

      call tick(locnymd,locnhms,3600)
      write(date0,1000)  locnymd
      write(hour0,2000)  locnhms/10000
      write(mins0,2000) (locnhms-(locnhms/10000)*10000)/100
      time0 = trim(hour0)//trim(mins0)
      dsname = trim(dirname) // trim(dsnprefix) // "." // trim(date0) // "_" // trim(time0) // "z.hdf"

      call gfio_open(trim(dsname),1,id2,rc)

      if( rc.ne.0) then
       print *,' Something wrong with file open ',trim(dsname)
       stop
      endif

      call tick(locnymd,locnhms,3600)
      write(date0,1000)  locnymd
      write(hour0,2000)  locnhms/10000
      write(mins0,2000) (locnhms-(locnhms/10000)*10000)/100
      time0 = trim(hour0)//trim(mins0)
      dsname = trim(dirname) // trim(dsnprefix) // "." // trim(date0) // "_" // trim(time0) // "z.hdf"

      call gfio_open(trim(dsname),1,id3,rc)

      if( rc.ne.0) then
       print *,' Something wrong with file open ',trim(dsname)
       stop
      endif

      call gfio_diminquire (id1,im,jm,lm,ntimes,nvars,ngatts,rc )

      end subroutine read_file_diminfo3

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_slv_data (id1,id2,id3,ps,ts,lats,lons,im,jm,lm,plevs,nvars,ntime,rc)
      implicit none

      integer   id1,id2,id3,im,jm,lm,nvars,ntime,rc
      real ts(im,jm), ps(im,jm), lats(jm), lons(im), plevs(1)
      real ts1(im,jm), ts2(im,jm), ts3(im,jm)
      real ps1(im,jm), ps2(im,jm), ps3(im,jm)

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      real amiss
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

C **********************************************************************
C ****      Read HDF File for Some Info  and the data               ****
C **********************************************************************
      call gfio_inquire ( id1,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )
      call gfio_getvar ( id1,'PS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ps1,rc )          
      call gfio_getvar ( id1,'TS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ts1,rc )          

C **********************************************************************

      call gfio_inquire ( id2,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )
      call gfio_getvar ( id2,'PS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ps2,rc )          
      call gfio_getvar ( id2,'TS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ts2,rc )          

C **********************************************************************
      call gfio_inquire ( id3,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )
      call gfio_getvar ( id3,'PS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ps3,rc )          
      call gfio_getvar ( id3,'TS',yyyymmdd(1),hhmmss(1),im,jm,0, 1,ts3,rc )          
C **********************************************************************

      ts = (ts1+ts2+ts3)/3.
      ps = (ps1+ps2+ps3)/3.

      call gfio_close ( id1,rc )
      call gfio_close ( id2,rc )
      call gfio_close ( id3,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_slv_data

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_flx_data (id1,id2,id3,eflux,hflux,prectot,lats,lons,im,jm,lm,plevs,nvars,ntime,rc)
      implicit none

      integer   id1,id2,id3,im,jm,lm,nvars,ntime,rc
      real eflux(im,jm), hflux(im,jm), prectot(im,jm), lats(jm), lons(im), plevs(1)
      real eflux1(im,jm), eflux2(im,jm), eflux3(im,jm)
      real hflux1(im,jm), hflux2(im,jm), hflux3(im,jm)
      real prectot1(im,jm), prectot2(im,jm), prectot3(im,jm)

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real amiss
      real, allocatable :: valrange(:,:),packrange(:,:)

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

C **********************************************************************
C ****          Read HDF File for Some Info and Data                ****
C **********************************************************************

      call gfio_inquire ( id1,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

      call gfio_getvar ( id1,'EFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,eflux1,rc )          
      call gfio_getvar ( id1,'HFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,hflux1,rc )          
      call gfio_getvar ( id1,'PRECTOT',yyyymmdd(1),hhmmss(1),im,jm,0, 1,prectot1,rc )          
C **********************************************************************

      call gfio_inquire ( id2,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

      call gfio_getvar ( id2,'EFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,eflux2,rc )          
      call gfio_getvar ( id2,'HFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,hflux2,rc )          
      call gfio_getvar ( id2,'PRECTOT',yyyymmdd(1),hhmmss(1),im,jm,0, 1,prectot2,rc )          
C **********************************************************************

      call gfio_inquire ( id3,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

      call gfio_getvar ( id3,'EFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,eflux3,rc )          
      call gfio_getvar ( id3,'HFLUX',yyyymmdd(1),hhmmss(1),im,jm,0, 1,hflux3,rc )          
      call gfio_getvar ( id3,'PRECTOT',yyyymmdd(1),hhmmss(1),im,jm,0, 1,prectot3,rc )          
C **********************************************************************

      eflux = (eflux1+eflux2+eflux3)/3.
      hflux = (hflux1+hflux2+hflux3)/3.
      prectot = (prectot1+prectot2+prectot3)/3.

      call gfio_close ( id1,rc )
      call gfio_close ( id2,rc )
      call gfio_close ( id3,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_flx_data

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_tdt_data (id,tdt,lats,lons,im,jm,lm,plevs,nvars,ntime,amiss,rc)
      implicit none

      integer   id,im,jm,lm,nvars,ntime,rc
      real tdt(im,jm,lm), lats(jm), lons(im), plevs(lm)
      real amiss

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)

C **********************************************************************
C ****                  Read HDF File for Some Info                 ****
C **********************************************************************

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

C **********************************************************************
C ****                  Read HDF File for Data                      ****
C **********************************************************************

      call gfio_getvar ( id,'DTDTDYN',yyyymmdd(1),hhmmss(1),im,jm,1,lm,tdt,rc )

      call gfio_close ( id,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_tdt_data

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_tdtana_data (id,tdtana,lats,lons,im,jm,lm,plevs,nvars,ntime,amiss,rc)
      implicit none

      integer   id,im,jm,lm,nvars,ntime,rc
      real tdtana(im,jm,lm), lats(jm), lons(im), plevs(lm)
      real amiss

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)


C **********************************************************************
C ****                  Read HDF File for Some Info                 ****
C **********************************************************************

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

C **********************************************************************
C ****                  Read HDF File for Data                      ****
C **********************************************************************

      call gfio_getvar ( id,'DTDTANA',yyyymmdd(1),hhmmss(1),im,jm,1,lm,tdtana,rc )

      call gfio_close ( id,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_tdtana_data
 



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_qdt_data (id,qdt,lats,lons,im,jm,lm,plevs,nvars,ntime,amiss,rc)
      implicit none

      integer   id,im,jm,lm,nvars,ntime,rc
      real qdt(im,jm,lm), lats(jm), lons(im), plevs(lm)
      real amiss

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)

C **********************************************************************
C ****                  Read HDF File for Some Info                 ****
C **********************************************************************

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

C **********************************************************************
C ****                  Read HDF File for Data                      ****
C **********************************************************************

      call gfio_getvar ( id,'DQVDTDYN',yyyymmdd(1),hhmmss(1),im,jm,1,lm,qdt,rc )

      call gfio_close ( id,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_qdt_data

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_qdtana_data (id,qdtana,lats,lons,im,jm,lm,plevs,nvars,ntime,amiss,rc)
      implicit none

      integer   id,im,jm,lm,nvars,ntime,rc
      real qdtana(im,jm,lm), lats(jm), lons(im), plevs(lm)
      real amiss

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)

C **********************************************************************
C ****                  Read HDF File for Some Info                 ****
C **********************************************************************

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

C **********************************************************************
C ****                  Read HDF File for Data                      ****
C **********************************************************************

      call gfio_getvar ( id,'DQVDTANA',yyyymmdd(1),hhmmss(1),im,jm,1,lm,qdtana,rc )

      call gfio_close ( id,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_qdtana_data



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine read_asm_data (id,t,qv,u,v,omega,lats,lons,im,jm,lm,plevs,nvars,ntime,amiss,rc)
      implicit none

      integer   id,im,jm,lm,nvars,ntime,rc
      real t(im,jm,lm), qv(im,jm,lm), u(im,jm,lm), v(im,jm,lm), omega(im,jm,lm) 
      real lats(jm), lons(im), plevs(lm)
      real amiss

      integer timinc
      integer, allocatable :: yyyymmdd(:),hhmmss(:),kmvar(:)
      character * 256 title,source,contact,levunits
      character * 256, allocatable :: vname(:),vtitle(:),vunits(:) 
      real, allocatable :: valrange(:,:),packrange(:,:)


C **********************************************************************
C ****                  Read HDF File for Some Info                 ****
C **********************************************************************

      allocate ( yyyymmdd(1) )
      allocate ( hhmmss(1) )
      allocate ( kmvar(nvars) )
      allocate ( vname(nvars) )
      allocate ( vtitle(nvars) )
      allocate ( vunits(nvars) )
      allocate ( valrange(2,nvars) )
      allocate ( packrange(2,nvars) )

      call gfio_inquire ( id,im,jm,lm,ntime,nvars,
     .                    title,source,contact,amiss,
     .                    lons,lats,plevs,levunits,
     .                    yyyymmdd,hhmmss,timinc,
     .                    vname,vtitle,vunits,kmvar,
     .                    valrange,packrange,rc )

C **********************************************************************
C ****                  Read HDF File for Data                      ****
C **********************************************************************

      call gfio_getvar ( id,'T',yyyymmdd(1),hhmmss(1),im,jm,1,lm,t,rc )
      call gfio_getvar ( id,'QV',yyyymmdd(1),hhmmss(1),im,jm,1,lm,qv,rc )
      call gfio_getvar ( id,'U',yyyymmdd(1),hhmmss(1),im,jm,1,lm,u,rc )
      call gfio_getvar ( id,'V',yyyymmdd(1),hhmmss(1),im,jm,1,lm,v,rc )
      call gfio_getvar ( id,'OMEGA',yyyymmdd(1),hhmmss(1),im,jm,1,lm,omega,rc )

      call gfio_close ( id,rc )

      deallocate ( yyyymmdd )
      deallocate ( hhmmss )
      deallocate ( kmvar )
      deallocate ( vname )
      deallocate ( vtitle )
      deallocate ( vunits )
      deallocate ( valrange )
      deallocate ( packrange )

      return
      end subroutine read_asm_data

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine getmylatlon2(latbegin,lonbegin,latend,lonend,lats,lons,
     .                                 imN,jmN,fieldin,fieldscm)
      implicit none
      real latbegin,lonbegin,latend,lonend
      real lats(jmN),lons(imN)
      integer imN,jmN
      real fieldin(imN,jmN),fieldscm

      real dlat, dlon
      integer ii,jj,ibegin,iend,jbegin,jend,itot

      dlat = abs(lats(1)-lats(2))
      dlon = abs(lons(1)-lons(2))
      ibegin = nint(  abs(-180.-lonbegin)/dlon )
      iend = nint(  abs(-180.-lonend)/dlon )
      jbegin = nint(  abs(-90.-latbegin)/dlat )
      jend = nint(  abs(-90.-latend)/dlat )
      itot = (iend-ibegin+1) * (jend-jbegin+1)
      fieldscm = 0.

      do ii=ibegin,iend
      do jj=jbegin,jend
       fieldscm = fieldscm + fieldin(ii,jj)/itot
      enddo
      enddo

      return
      end subroutine getmylatlon2

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine getmylatlon3(n,latbegin,lonbegin,latend,lonend,lats,lons,
     .                                 imC,jmC,lmC,fieldin,undef,fieldscm)
      implicit none
      real latbegin,lonbegin,latend,lonend
      real lats(jmC),lons(imC)
      integer n,imC,jmC,lmC
      real fieldin(imC,jmC,lmC),fieldscm(1000,100)
      real undef

      integer ii,jj,kk,ibegin,iend,jbegin,jend,itot
      real dlat, dlon
      logical undefs

      undefs = .false.
      dlat = abs(lats(1)-lats(2))
      dlon = abs(lons(1)-lons(2))
      ibegin = nint(  abs(-180.-lonbegin)/dlon )
      iend = nint(  abs(-180.-lonend)/dlon )
      jbegin = nint(  abs(-90.-latbegin)/dlat )
      jend = nint(  abs(-90.-latend)/dlat )
      itot = (iend-ibegin+1) * (jend-jbegin+1)
      do kk = 1,lmC
       fieldscm(n,kk) = 0.
      enddo

      do kk = 1,lmC
      do ii=ibegin,iend
      do jj=jbegin,jend
       fieldscm(n,kk) = fieldscm(n,kk) + fieldin(ii,jj,kk)/itot
      enddo
      enddo
       if(fieldscm(n,kk).gt.undef/1000.) then
        fieldscm(n,kk)=undef
        undefs = .true.
       endif
      enddo

      if(undefs) call extrap(n,lmC,undef,fieldscm)

      return
      end subroutine getmylatlon3

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine writer_meta(n,lmC,plevsscm,timesscm,yearsscm,monthsscm,daysscm,hoursscm,minutesscm)
      implicit none
      integer n,lmC
      real plevsscm(lmC),timesscm(n)
      integer yearsscm(n),monthsscm(n),daysscm(n),hoursscm(n),minutesscm(n)

      integer ii

      open (20,file='merra_scm.dat',form='formatted',access='sequential')

      write(20,1000)
      write(20,*) n
      write(20,1001)
      write(20,*) lmC
      write(20,1002)
      write(20,*) 'mb'
      write(20,3000)(plevsscm(ii),ii=1,lmC)
      write(20,*) 'time TIME(nt)'
      write(20,3000)(timesscm(ii),ii=1,n)
      write(20,*) 'year YY(nt)'
      write(20,3000)(float(yearsscm(ii)),ii=1,n)
      write(20,*) 'month MO(nt)'
      write(20,3000)(float(monthsscm(ii)),ii=1,n)
      write(20,*) 'day DD(nt)'
      write(20,3000)(float(daysscm(ii)),ii=1,n)
      write(20,*) 'hour HH(nt)'
      write(20,3000)(float(hoursscm(ii)),ii=1,n)
      write(20,*) 'minute MM(nt)'
!      write(20,1008)
      write(20,3000)(float(minutesscm(ii)),ii=1,n)

 1000 format(' time length (nt)')
 1001 format(' number of pressure levels (np)')
 1002 format(' pressure levels P(np)')
 1003 format('Time t(nt):')
 1004 format('Year yy(nt):')
 1005 format('Month mo(nt):')
 1006 format('Day day(nt):')
 1007 format('Hour hh(nt):')
 1008 format('Minutes mm(nt):')
 2000 format(5e15.7)
 3000 format(1es15.7)

      return
      end subroutine writer_meta

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine writer_upper(n,lmC,tscm,qvscm,uscm,vscm,omegascm,dtdtdynscm,dqvdtdynscm,dtdtanascm,dqvdtanascm)
      implicit none
      integer n,lmC
      real tscm(1000,100),qvscm(1000,100),uscm(1000,100),vscm(1000,100),omegascm(1000,100)
      real dtdtdynscm(1000,100),dqvdtdynscm(1000,100),dtdtanascm(1000,100),dqvdtanascm(1000,100)

      integer ii,kk
      integer numlev,numtim
      real zeros(1000,100)

      numlev=lmC
      numtim=n

      zeros = 0.

      write(20,*),"Number of Multi-Level Fields: "
      write(20,*),"         11"

      write(20,*), 1,  " & Temp_(K)"
      write(20,*),"K"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(tscm(kk,ii),ii=1,numlev)
      enddo

c      write(20,*), 2,  " & H2O_Mixing_Ratio_(g/kg)"
      write(20,*), 2,  " & H2O_Mixing_Ratio_(kg/kg)"
      write(20,*),"g/kg"
      do kk=1,numtim
       write(20,*), "nt=", kk
c       write(20,2000)(qvscm(kk,ii)*1000.,ii=1,numlev)
       write(20,2000)(qvscm(kk,ii),ii=1,numlev)
      enddo

      write(20,*),  3,  " & U_wind_(m/sec)"
      write(20,*),"m/sec"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(uscm(kk,ii),ii=1,numlev)
      enddo

      write(20,*),  4,  " & V_wind_(m/sec)"
      write(20,*),"m/sec"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(vscm(kk,ii),ii=1,numlev)
      enddo

      write(20,*), 5,  " & Omega_(mb/hour)"
      write(20,*),"mb/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(omegascm(kk,ii)*36.,ii=1,numlev)
      enddo

      write(20,*), 6,  " & Horizontal_Temp_Advec_(K/hour) dtdtdynscm"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(dtdtdynscm(kk,ii)*3600.,ii=1,numlev)
      enddo

      write(20,*), 7,  " & Vertical_Temp_Advec_(K/hour)"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(zeros(kk,ii),ii=1,numlev)
      enddo

      write(20,*), 8,  " & Horizontal_q_Advec_(g/kg/hour)dqvdtdynscm"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(dqvdtdynscm(kk,ii)*3600000.,ii=1,numlev)
      enddo

      write(20,*), 9,  " & Vertical_q_Advec_(g/kg/hour)"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(zeros(kk,ii),ii=1,numlev)
      enddo

c---------------SVETA-------------
      write(20,*), 10,  " & Horizontal_Temp_Advec_(K/hour) dtdtanascm"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(dtdtanascm(kk,ii)*3600.,ii=1,numlev)
      enddo

      write(20,*), 11,  " & Horizontal_q_Advec_(g/kg/hour)dqvdtanascm"
      write(20,*),"K/hour"
      do kk=1,numtim
       write(20,*), "nt=", kk
       write(20,2000)(dqvdtanascm(kk,ii)*3600.,ii=1,numlev)
      enddo
 
 2000 format(1es15.7)

      return
      end subroutine writer_upper

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine writer_surface(n,psscm,tsscm,efluxscm,hfluxscm,prectotscm)
      implicit none
      integer n
      real psscm(n),tsscm(n),efluxscm(n),hfluxscm(n),prectotscm(n)

      integer ii

!      write(20,1000)
!      write(20,1001)
!      write(20,1002)
      write(20,*),'Number of Single-Level Fields:'     
      write(20,*), 5     


      write(20,*), 1,  " & SH_(W/m**2)"
      write(20,*),"W/m**2"
      do ii = 1,n
       write(20,2000)hfluxscm(ii)
      enddo

      write(20,*), 2,  " & LH_(W/m**2)"
      write(20,*),"W/m**2"
      do ii = 1,n
       write(20,2000)efluxscm(ii)
      enddo

      write(20,*), 3,  " & TS_(K)"
      write(20,*),"K"
      do ii = 1,n
       write(20,2000)tsscm(ii)
      enddo

      write(20,*), 4,  " & PS_(mb)"
      write(20,*),"mb"
      do ii = 1,n
       write(20,2000)psscm(ii)/100.
      enddo

      write(20,*), 5,  " & Prec_(mm/hour)"
      write(20,*),"mm/hour"
      do ii = 1,n
       write(20,2000)prectotscm(ii)*3600.
      enddo


! 1000 format('Number of Single-Level Fields:')
! 1001 format('   5 ')
! 1002 format('  SH_(W/m**2)  ','  LH_(W/m**2)  ','  TS_(K)       ','  PS_(mb)      ',' Prec_(mm/hour)')
! 2000 format(5e15.7)
 2000 format(1es15.7)

      return
      end subroutine writer_surface

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine extrap(n,lmC,undef,fieldscm)
      implicit none
      integer n,lmC
      real undef,fieldscm(1000,100)

      integer kk
      real valabove

      valabove=fieldscm(n,lmC)
      do kk=lmC-1,1,-1
       if(fieldscm(n,kk).eq.undef) then
        fieldscm(n,kk)=valabove
       else
        valabove = fieldscm(n,kk)
       endif
      enddo

      return
      end subroutine extrap

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      function nsecf (nhms)
C***********************************************************************
C     Converts NHMS format to Total Seconds
C***********************************************************************
      implicit none
      integer  nhms, nsecf
      nsecf =  nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
      return
      end function nsecf

      function nhmsf (nsec)
C***********************************************************************
C     Converts Total Seconds to NHMS format
C***********************************************************************
      implicit none
      integer  nhmsf, nsec
      nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
      return
      end function nhmsf

      subroutine tick (nymd,nhms,ndt)
C***********************************************************************
C     Tick the Date (nymd) and Time (nhms) by NDT (seconds)
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
C     INCYMD:  NYMD CHANGED BY ONE DAY                                          
C     MODYMD:  NYMD CONVERTED TO JULIAN DATE                                    
C  DESCRIPTION OF PARAMETERS                                                    
C     NYMD     CURRENT DATE IN YYMMDD FORMAT                                    
C     M        +/- 1 (DAY ADJUSTMENT)                                           
C***********************************************************************        

      INTEGER NDPM(12)
      DATA    NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      LOGICAL LEAP
      DATA    NY00     / 1900 /
      LEAP(NY) = MOD(NY,4).EQ.0 .AND. (NY.NE.0 .OR. MOD(NY00,400).EQ.0)

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
