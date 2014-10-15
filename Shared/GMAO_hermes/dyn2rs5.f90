      program  main
      use m_dyn
      use m_gfio_getfld, only : gfio_getfld
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to merge eta_hdf data into GEOS5 restarts        ****
! ****                                                              ****
! **** HISTORY:                                                     ****
! **** 11Nov2004 Todling Replaced straight gfio calls by m_dyn get  ****
! ****                   to allow reading from a file w/ mult times ****
! **** 06Jun2005 LPChang/RT Added O3/OX capability                  ****
! **** 23Jan2006 Elena N. Fixed logic to fill o3 values             ****
! ****                    if    oxrequest = .true.                  ****
! **** 26Jan2006 LPChang/RT Added 5 more species to ox_internal_rst ****
! ****                      after Ox; renamed pchem_internal_rst    ****
! **** 23Feb2006 Todling  - Make sure OX and SPHU not negative      ****
! ****                    - Bug fix: o3 gfio_get needed flip flds   ****
! **** 16Mar2006 LPChang  - fix to cvzLimit (can't be zero!!)       ****
! **** 22Mar2006 Todling  - bug fix: reshape caused compiler to err **** 
! **** 24Apr2006 Todling  - update # species for Eros GCM           ****
! **********************************************************************
! **********************************************************************

      real, parameter :: czaLimit = 1.0e-5
      integer, parameter :: ntracer = 7  ! for now: wired in number of tracers

      character*255 dynrst, moistrst, oxrst, anaeta, anasfc

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms,nymd_ana,nhms_ana
      integer im,jm,lm,nc

      logical oxrequest, oxexist

! restart variables and topography
! --------------------------------
      real*8, allocatable ::   dp(:,:,:)
      real*8, allocatable ::    u(:,:,:)
      real*8, allocatable ::    v(:,:,:)
      real*8, allocatable ::   th(:,:,:)
      real*8, allocatable ::  thv(:,:,:)
      real*8, allocatable ::   pk(:,:,:)
      real*8, allocatable ::  ple(:,:,:)
      real*8, allocatable ::  pke(:,:,:)
      real*8, allocatable ::    q(:,:,:)
      real*8, allocatable ::   o3(:,:,:)
      real*8, allocatable ::   ox(:,:,:)
      real*8, allocatable ::   pl(:,:,:)
      real*8, allocatable ::ro3ox(:,:,:)
      real*8, allocatable ::cosza(:,:)
      real*8, allocatable ::   ps(:,:)
      real*8, allocatable :: phis(:,:)
      real*8, allocatable ::   ak(:)
      real*8, allocatable ::   bk(:)

      type(dyn_vect) w_a  ! fv-dynamics vector in eta (analysis)

      real*4, allocatable ::  dum(:,:)
      real*4, allocatable ::  dum_chem(:,:,:,:)
      real    kappa
      real    undef,rgas,rvap,eps

      character*120, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,i,j,L,ID,rc

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

         kappa = 2.0/7.0

        dynrst = 'fvcore_internal_restart'
      moistrst = 'moist_internal_restart'
         oxrst = 'pchem_internal_restart'
     oxrequest = .false.
        anaeta = 'x'
        anasfc = 'x'
      nymd_ana = -999
      nhms_ana = -999

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
             if( trim(arg(n)).eq.'-dynrst'   ) then
                                   dynrst  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-moistrst' ) then
                                   moistrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-ana'      ) then
                                   anaeta   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-ox'       ) then
                                   oxrst    = trim(arg(n+1))
                                  oxrequest = .true.
             endif
             if( trim(arg(n)).eq.'-sfc'      ) then
                                   anasfc   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-nymd' ) read(arg(n+1),*) nymd_ana
             if( trim(arg(n)).eq.'-nhms' ) read(arg(n+1),*) nhms_ana
      enddo

! **********************************************************************
! ****    Read dycore internal Restart for RSLV, Date and Time      ****
! **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2
      close(10)

      nymd = headr1(1)*10000 &
           + headr1(2)*100   &
           + headr1(3)
      nhms = headr1(4)*10000 &
           + headr1(5)*100   &
           + headr1(6)

      if( nymd_ana.eq.-999 ) nymd_ana = nymd
      if( nhms_ana.eq.-999 ) nhms_ana = nhms

      write(date,'(i8.8)') nymd_ana
      write(hour,'(i2.2)') nhms_ana/10000

      if(trim(anaeta).eq.'x') anaeta = 'geos5.ana.eta.' // date // '_' // hour // 'z.hdf'

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

      print *
      print *, '  dyn  restart filename: ',trim(dynrst)
      print *, 'moist  restart filename: ',trim(moistrst)
      if(oxrequest) &
      print *, '   ox  restart filename: ',trim(oxrst)
      print *, '      analysis filename: ',trim(anaeta)
      print *, '             resolution: ',im,jm,lm
      print *, '                   date: ',nymd_ana,nhms_ana
      print *

      allocate (   ps(im,jm)      )
      allocate (   dp(im,jm,lm)   )
      allocate (   pk(im,jm,lm)   )
      allocate (  ple(im,jm,lm+1) )
      allocate (  pke(im,jm,lm+1) )
      allocate (    u(im,jm,lm)   )
      allocate (    v(im,jm,lm)   )
      allocate (   th(im,jm,lm)   )
      allocate (  thv(im,jm,lm)   )
      allocate (    q(im,jm,lm)   )
      allocate (   o3(im,jm,lm)   )

      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )

! **********************************************************************
! ****                   Read Analysis ana File                     ****
! **********************************************************************

      call dyn_get ( anaeta, nymd_ana, nhms_ana, w_a, rc, timidx=0 )
      ps  = w_a%ps
      dp  = w_a%delp
      u   = w_a%u
      v   = w_a%v
      thv = w_a%pt
      ak  = w_a%grid%ak
      bk  = w_a%grid%bk
      q   = reshape(w_a%q(:,:,:,1),(/im,jm,lm/))

      inquire(file=trim(oxrst),exist=oxexist)
      oxrequest = oxrequest .and. oxexist

      if ( oxrequest )  o3  = reshape(w_a%q(:,:,:,2),(/im,jm,lm/))

      call dyn_clean ( w_a )

      ple(:,:,lm+1) = ps(:,:)
      do L=lm,1,-1
      ple(:,:,L) = ple(:,:,L+1)-dp(:,:,L)
      enddo

! Enforce model top consistency
! -----------------------------
       dp(:,:,1) = ple(:,:,2)-ak(1)
      ple(:,:,1) = ple(:,:,2)-dp(:,:,1)

      pke(:,:,:) = ple(:,:,:)**kappa

      do L=1,lm
       pk(:,:,L) = ( pke(:,:,L+1)-pke(:,:,L) )/( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo

! Read original OX, convert ana-O3 to OX and update OX
! ----------------------------------------------------
      if ( oxrequest ) then

           allocate (    ox(im,jm,lm) )

           allocate ( cosza(im,jm)    )
           allocate ( ro3ox(im,jm,lm) )
           allocate (    pl(im,jm,lm) )
           call gfio_getfld ( trim(anasfc), nymd_ana, nhms_ana, im, jm, &
                              1, (/'COSZ'/), ro3ox )
           cosza(:,:) = ro3ox(:,:,1)  ! ro3ox used as auxliar array
           ro3ox = 1.0
           pl = 0.5 * ( ple(:,:,1:lm) + ple(:,:,2:lm+1) )
           do l = 1, lm
              where (pl(:,:,l) < 100.0 .and. cosza > czaLimit)
                ro3ox(:,:,l) = exp(-1.5*(log10(0.01*pl(:,:,l)))**2)
              end where
           enddo
           ox = o3 * 1.e-6/ro3ox

           deallocate (    pl )
           deallocate ( ro3ox )
           deallocate ( cosza )

      endif

! Construct Dry Potential Temperature
! -----------------------------------
      rgas = 8314.3/28.97
      rvap = 8314.3/18.01
      eps  = rvap/rgas-1.0
      th   = thv/(1+eps*q)

      call hflip (   u,im,jm,lm   )
      call hflip (   v,im,jm,lm   )
      call hflip (   q,im,jm,lm   )
      if( oxrequest ) call hflip (  ox,im,jm,lm   )
      call hflip (  th,im,jm,lm   )
      call hflip (  pk,im,jm,lm   )
      call hflip ( ple,im,jm,lm+1 )

! **********************************************************************
! ****                 Write dycore internal Restart                ****
! **********************************************************************

      anaeta = trim(dynrst) // '.ana.' // date // '_' // hour // 'z'
      print *, 'Creating GEOS-5 fvcore_internal_restart: ',trim(anaeta)

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      open (20,file=trim(anaeta),form='unformatted',access='sequential')

      read (10) headr1
      read (10) headr2
      write(20) headr1
      write(20) headr2

      read (10) ak
      read (10) bk
      write(20) ak
      write(20) bk

          do L=1,lm
             write(20) ((  u(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(20) ((  v(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(20) (( th(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm+1
             write(20) ((ple(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(20) (( pk(i,j,L),i=1,im),j=1,jm)
          enddo

      close (10)
      close (20)

! **********************************************************************
! ****     Merge moist internal Restart with Analysis ana file      ****
! **********************************************************************

      allocate ( dum(im,jm)    )
      anaeta = trim(moistrst) // '.ana.' // date // '_' // hour // 'z'
      print *, 'Creating GEOS-5  moist_internal_restart: ',trim(anaeta)
      print *

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      open  (20,file=trim(anaeta)  ,form='unformatted',access='sequential')

      do L=1,lm
         read (10) dum       ! First moist variable is SPHU
                   where (q(:,:,L) > 0.0) dum(:,:) = q(:,:,L)
         write(20) dum       ! First moist variable is SPHU
      enddo
               rc =  0
      do while (rc.eq.0)
      do L=1,lm
         read   (10,iostat=rc) dum       ! Copy moist internal state
         if(rc.eq.0) write(20) dum
      enddo
      enddo
      close (10)
      close (20)

! **********************************************************************
! ****     Merge OX internal Restart with Analysis O3 field         ****
! **********************************************************************
                                                                                                                                  
      if ( oxrequest ) then

        anaeta = trim(oxrst) // '.ana.' // date // '_' // hour // 'z'
        print *, 'Creating GEOS-5  pchem restart: ',trim(anaeta)
        print *
        open  (10,file=trim(oxrst)  ,form='unformatted',access='sequential')
        allocate ( dum_chem(im,jm,lm,ntracer) )
        do nc = 1, ntracer             ! read in all species in chem restart file
        do L=1,lm
           read(10) dum
           dum_chem(:,:,L,nc) = dum
        enddo
        enddo
        close (10)
        where (ox>0.0) dum_chem(:,:,:,1) = ox(:,:,:)  ! overwrite OX in restart file
        open  (20,file=trim(anaeta)  ,form='unformatted',access='sequential')
        do nc = 1, ntracer             ! write out all species in chem restart file
        do L=1,lm
           dum(:,:) = dum_chem(:,:,L,nc)
           write(20) dum
        enddo
        enddo
        close (20)
        deallocate (dum_chem)
        deallocate (ox)
      endif

! End gracefully
! --------------
      deallocate (  dum )
      deallocate (   bk )
      deallocate (   ak )
      deallocate (   o3 )
      deallocate (    q )
      deallocate (  thv )
      deallocate (   th )
      deallocate (    v )
      deallocate (    u )
      deallocate (  pke )
      deallocate (  ple )
      deallocate (   pk )
      deallocate (   dp )
      deallocate (   ps )

      stop
      end

      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L,imh
      real   q(im,jm,lm),dum(im)
      imh = im/2
      do L=1,lm
      do j=1,jm
      do i=1,imh
         dum(i) = q(i+imh,j,L)
         dum(i+imh) = q(i,j,L)
      enddo
         q(:,j,L) = dum(:)
      enddo
      enddo
      return
      end

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " hdf2rs.x  [-dynrst     dynrst_fname]  Default: fvcore_internal_restart"
      print *, "           [-moistrst moistrst_fname]  Default: moist_internal_restart"
      print *, "           [-ox          pchem_fname]  Default: pchem_internal_restart"
      print *, "           [-ana           ana_fname]  Default: geos5.ana.eta.yyyymmdd_hhz.hdf"
      print *, "           [-sfc           sfc_fname]  Default: geos5.ana.sfc.yyyymmdd_hhz.hdf"
      print *, "           [-nymd           yyyymmdd] Optional  Overriding DateStamp for Analysis Input"
      print *, "           [-nhms             hhmmss] Optional  Overriding TimeStamp for Analysis Input"
      print *
      print *, "where:"
      print *, "-----"
      print *, "  -dynrst     dynrst_fname:  Filename of dynamics internal restart"
      print *, "  -moistrst moistrst_fname:  Filename of    moist internal restart"
      print *, "  -ox          pchem_fname:  Filename of    pchem internal restart"
      print *, "  -ana           ana_fname:  Filename of    analysis ana.eta file"
      print *, "  -sfc           sfc_fname:  Filename of    surface background/analysis file"
      print *
      print *, "creates:"
      print *, "-------"
      print *, "    fvcore_internal_restart.ana.yyyymmdd_hhz"
      print *, "     moist_internal_restart.ana.yyyymmdd_hhz"
      print *, "     pchem_internal_restart.ana.yyyymmdd_hhz"
      print *
      call exit(7)
      end
