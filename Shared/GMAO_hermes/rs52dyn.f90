      program  main
      use m_dyn
      use m_set_eta, only : set_eta
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****     Program to create eta_hdf data from GEOS5 restarts       ****
! ****     Necessary for applications relying on the                ****
! ****     initial step on trajectory, such as SVEC and SENS runs   ****
! ****                                                              ****
! **** HISTORY:                                                     ****
! **** 23Jan2006 Elena N./RT Adopted dyn2rs5.f90 code               ****
! ****                       to create this utility                 ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

      integer, parameter :: frq = 3000
      logical, parameter :: fnew = .true.

      character*255 dynrst, moistrst, oxrst, anaeta, trajout
      character*255 expid

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms,nymd_ana,nhms_ana
      integer im,jm,lm
      integer ntrac

      logical oxrequest, oxexist, pick_nymd, pick_nhms

! restart variables and topography
! --------------------------------
      real, pointer ::    u(:,:,:)
      real, pointer ::    v(:,:,:)
      real, pointer ::   th(:,:,:)
      real, pointer ::  thv(:,:,:)
      real, pointer :: q(:,:,:,:)
      real, pointer ::   ps(:,:)
      real, pointer ::   ak(:)
      real, pointer ::   bk(:)

      real*4, pointer ::   dum(:,:)    ! SPHU and tracers in GEOS-5 restart files
                                       ! are in real*4 

      type(dyn_vect) w_a  ! output fv-dynamics vector 
      type(dyn_vect) w_f  ! input fv-dynamics ana.eta vector  

      real    undef,rgas,rvap,eps

      character*120, allocatable :: arg(:)
      character*8    date
      character*2    hour
      integer n,nargs,iargc,i,j,L,rc

      integer prec, ks
      real*8  ptop, pint

! **********************************************************************
! ****                  Initialize Filenames and constants          ****
! **********************************************************************

          rgas = 8314.3/28.97
          rvap = 8314.3/18.01
          eps  = rvap/rgas-1.0

        dynrst = 'fvcore_internal_restart'
      moistrst = 'moist_internal_restart'
         oxrst = 'ox_internal_restart'
     oxrequest = .false.
     pick_nymd = .false.
     pick_nhms = .false.
         ntrac = 1
        anaeta = 'x'
       trajout = 'x'
         expid = 'geos5'
      nymd_ana = -999
      nhms_ana = -999
          prec = 0

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
             if( trim(arg(n)).eq.'-trajout' ) then
                                   trajout = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-ox'       ) then
                                   oxrst    = trim(arg(n+1))
                                  oxrequest = .true.
             endif
             if( trim(arg(n)).eq.'-expid'     ) then
                                   expid    = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-nymd'      ) then
                                   read(arg(n+1),*) nymd_ana
                                        pick_nymd = .true.
             endif
             if( trim(arg(n)).eq.'-nhms'      ) then
                                   read(arg(n+1),*) nhms_ana
                                        pick_nhms = .true.
             endif
      enddo

      if(oxrequest) ntrac = 2
! **********************************************************************
! ****    Read dycore internal Restart for RSLV, Date and Time      ****
! **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

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
      if(trim(trajout).eq.'x') trajout = trim(expid)//'.traj.lcv.' // date // '_' // hour // 'z.hdf'

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
      print *, '   traj output filename: ',trim(trajout)
      print *

      allocate (   ps(im,jm)      )
      allocate (    u(im,jm,lm)   )
      allocate (    v(im,jm,lm)   )
      allocate (   th(im,jm,lm)   )
      allocate (  thv(im,jm,lm)   )
      allocate (    q(im,jm,lm,ntrac)   )
      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )
      allocate ( dum(im,jm)       )  

      read (10) ak
      read (10) bk

          do L=1,lm
             read(10) ((  u(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) ((  v(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) (( th(i,j,L),i=1,im),j=1,jm)
          enddo


      close(10)

! **********************************************************************
! ****     Merge moist internal Restart with fvcore dynamic internal Restart      ****
! **********************************************************************
                                                                                                                                               
      print *, 'Reading GEOS-5  moist_internal_restart: ',trim(moistrst)
      print *
                                                                                                                                               
      open  (10,file=trim(moistrst),form='unformatted',access='sequential')

      do L=1,lm
         read (10) dum
             q(:,:,L,1) = dum(:,:)      ! First moist variable is SPHU
      enddo

      close (10)

! **********************************************************************
! **********************************************************************
! ****     Read original OX restart                                            ****
! **********************************************************************
      inquire(file=trim(oxrst),exist=oxexist)
      if ( oxrequest ) then
      
        if ( oxexist) then

           print *, 'Reading GEOS-5  ox_internal_restart: ',trim(oxrst)
           print *
                                                                                                                                               
           open  (20,file=trim(oxrst)  ,form='unformatted',access='sequential')
                                                                                                                                               
           do L=1,lm
              read (20) dum
                  q(:,:,L,2) = dum(:,:) 
           enddo
           close (20)

        else
           print *, 'GEOS-5  ox_internal_restart: ',trim(oxrst),' is not found'
           print *, 'Filling ox values to zero'
           print *

           q(:,:,:,2) = 0.d0 
        endif

      endif

! Construct Virtual Potential Temperature
! ----------------------------------------
      thv(:,:,:)   = th(:,:,:)*(1+eps*q(:,:,:,1))

! **********************************************************************
! **** Flip fields - specific to GEOS-5 restart files               ****
! *********************************************************************

      call hflip (   u,im,jm,lm   )
      call hflip (   v,im,jm,lm   )
      do L = 1, ntrac
         call hflip (   q(:,:,:,L),im,jm,lm   )
      enddo
      call hflip (  thv,im,jm,lm   )

! **********************************************************************
! ****                   Read Analysis ana File                     ****
! **********************************************************************
      call dyn_null (w_f)

      if (pick_nymd .and. pick_nhms) then
           call dyn_get (trim(anaeta), nymd_ana, nhms_ana, w_f, rc, timidx = 0)
      else
           call dyn_get (trim(anaeta), nymd_ana, nhms_ana, w_f, rc)
      endif

      call set_eta ( lm, ks, ptop, pint, ak, bk )

      call dyn_null (w_a)

      call dyn_init ( im, jm, lm, ntrac, w_a, rc, ptop, ks, ak, bk )

      w_a%phis      = w_f%phis
      w_a%hs_stdv   = w_f%hs_stdv
      w_a%Ts        = w_f%Ts
      w_a%lwi       = w_f%lwi
      w_a%ps        = w_f%ps
      w_a%delp      = w_f%delp      ! assuming here that model consistency 
                                    ! ps (:,:) = sum(dp(:,:,:),3) + ptop
                                    ! has already been enforced in anaeta dyn vector 

      w_a%u       => u
      w_a%v       => v
      w_a%pt      => thv
      w_a%q       => q

      call dyn_null ( w_f )

      call dyn_put ( trim(trajout), nymd, nhms, prec, w_a, rc, &
                        new = fnew, freq = frq )

        if(rc/=0) then
          print *, 'Error from dyn_put'
        else
          print*,'Wrote out traj.lcv to ',trim(trajout)
        endif

      call dyn_null ( w_a )

! **********************************************************************
! ****                 Write dycore internal Restart                ****
! **********************************************************************


! End gracefully
! --------------
      deallocate (  dum )
      deallocate (   bk )
      deallocate (   ak )
      deallocate (   q )
      deallocate (  thv )
      deallocate (   th )
      deallocate (    v )
      deallocate (    u )
      deallocate (   ps )

      stop
      end
! **********************************************************************
! **********************************************************************
      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L,imh
      real    ::  q(im,jm,lm)
      real    ::  dum(im)
      imh=im/2
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

! **********************************************************************
! **********************************************************************
      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " rs52dyn.x  [-dynrst     dynrst_fname]  Default: fvcore_internal_restart"
      print *, "           [-moistrst moistrst_fname]  Default: moist_internal_restart"
      print *, "           [-ox             ox_fname]  Default: ox_internal_restart"
      print *, "                                       If [-ox ox_fname] is specified,"
      print *, "                                       but the file ox_name is not found"
      print *, "                                       ox values will be filled with zeros" 
      print *, "           [-ana           ana_fname]  Default: geos5.ana.eta.yyyymmdd_hhz.hdf"
      print *, "           [-expid             expid]  Default: geos5"
      print *, "           [-trajout   trajout_fname]  Default: geos5.traj.lcv.yyyymmdd_hhz.hdf"
      print *, "           [-nymd           yyyymmdd] Optional  Setting DateStamp for ana.eta file"
      print *, "                                                Will be a DateStamp in traj output"
      print *, "                                      Default:  Same as in fvcore_internal_restart"
      print *, "           [-nhms             hhmmss] Optional  Setting TimeStamp for ana.eta file"
      print *, "                                                Will be a TimeStamp in traj output"
      print *, "                                      Default:  Same as in fvcore_internal_restart"
      print *
      print *, "where:"
      print *, "-----"
      print *, "  -dynrst     dynrst_fname:  Filename of dynamics internal restart"
      print *, "  -moistrst moistrst_fname:  Filename of    moist internal restart"
      print *, "  -ox             ox_fname:  Filename of    ox    internal restart"
      print *, "  -ana           ana_fname:  Filename of     analysis ana.eta file"
      print *, "  -trajout   trajout_fname:  Filename of output GEOS-5 trajectory file" 
      print *
      print *, "creates:"
      print *, "-------"
      print *, "    expid.traj.lcv.yyyymmdd_hhz"
      print *
      print *, " Last updated 26Jan2006, Elena N."
      print *
      call exit(7)
      end

