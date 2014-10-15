      program  main
      implicit none

! ************************************************************************
! ************************************************************************
! ****                                                                ****
! ****   Program to insert DRY MASS value of 983.24 mb into Restarts  ****
! ****                                                                ****
! ************************************************************************
! ************************************************************************

      character*256 :: Usage="rs_scale.x  fv_internal_rst  moist_internal_rst  <grid_cell_area>"
      character*256 :: dynrst, mstrst
      character*256 :: afile="NULL"
      character*256 :: arg(3)

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms
      integer im,jm,lm
      integer i,j,L,n,rc,nargs,iargc

! restart variables
! -----------------
      real*8,   allocatable ::    u(:,:,:)
      real*8,   allocatable ::    v(:,:,:)
      real*8,   allocatable ::   th(:,:,:)
      real*8,   allocatable ::   pk(:,:,:)
      real*8,   allocatable ::   dp(:,:,:)
      real*8,   allocatable ::  pke(:,:,:)
      real*8,   allocatable ::  ple(:,:,:)
      real*8,   allocatable :: dum8(:,:)
      real*8,   allocatable ::   ak(:)
      real*8,   allocatable ::   bk(:)

      real,   allocatable ::   qv(:,:,:)
      real,   allocatable :: qlls(:,:,:)
      real,   allocatable :: qlcn(:,:,:)
      real,   allocatable :: cfls(:,:,:)
      real,   allocatable :: cfcn(:,:,:)
      real,   allocatable :: qils(:,:,:)
      real,   allocatable :: qicn(:,:,:)
      real,   allocatable :: area(:,:)
      real,   allocatable :: dum4(:,:)

      real*8, allocatable ::    gsum(:,:)
      real*8, allocatable ::    qsum(:,:)
      real*8, allocatable ::   psold(:,:)
      real*8, allocatable ::   psnew(:,:)
      real*8, allocatable :: pdryold(:,:)
      real*8, allocatable :: pdrynew(:,:)

      real*8, parameter   ::    pdry_ave = 98324.0_8
      real*8              :: pdryold_ave
      real*8              :: pdrynew_ave
      real                :: kappa

      kappa = 2.0/7.0

! **********************************************************************

  nargs = iargc()
  if( nargs<2 .or. nargs>3 ) then
     print *, "Wrong Number of arguments: ", i
     print *, trim(Usage)
     call exit(1)
  end if

  do n=1,nargs
  call getarg(n,arg(n))
  enddo

! Open Dynamics and Moist Internal Restarts
! -----------------------------------------
     read(arg(1),'(a)') dynrst
     open(unit=10, file=trim(dynrst), form='unformatted')

     read(arg(2),'(a)') mstrst
     open(unit=20, file=trim(mstrst), form='unformatted')

     if( nargs.eq.3 ) then
     read(arg(3),'(a)') afile
     open(unit=30, file=trim(afile), form='unformatted')
     endif
 
! **********************************************************************
! ****                  Read dycore internal Restart                ****
! **********************************************************************

      read (10) headr1
      read (10) headr2

      nymd = headr1(1)*10000 &
           + headr1(2)*100   &
           + headr1(3)
      nhms = headr1(4)*10000 &
           + headr1(5)*100   &
           + headr1(6)

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

      print *
      print *, '   dyn restart filename: ',trim(dynrst)
      print *, ' moist restart filename: ',trim(mstrst)
      print *, '             resolution: ',im,jm,lm
      print *, '                   date: ',nymd,nhms
      print *

      allocate (   ak(lm+1)       )
      allocate (   bk(lm+1)       )

      read (10) ak
      read (10) bk

      allocate (    u(im,jm,lm)   )
      allocate (    v(im,jm,lm)   )
      allocate (   th(im,jm,lm)   )
      allocate (   pk(im,jm,lm)   )
      allocate (  ple(im,jm,lm+1) )
      allocate (  pke(im,jm,lm+1) )

      do L=1,lm
         read(10)  u(:,:,L)
      enddo
      do L=1,lm
         read(10)  v(:,:,L)
      enddo
      do L=1,lm
         read(10)  th(:,:,L)
      enddo
      do L=1,lm+1
         read(10) ple(:,:,L)
      enddo
      do L=1,lm
         read(10)  pk(:,:,L)
      enddo

      close (10)

      allocate ( dp(im,jm,lm) )
      do L=1,lm
         dp(:,:,L) = ple(:,:,L+1)-ple(:,:,L)
      enddo

! **********************************************************************
! ****                   Read moist internal Restart                ****
! **********************************************************************

      allocate (   qv(im,jm,lm) )
      allocate ( qlls(im,jm,lm) )
      allocate ( qlcn(im,jm,lm) )
      allocate ( cfls(im,jm,lm) )
      allocate ( cfcn(im,jm,lm) )
      allocate ( qils(im,jm,lm) )
      allocate ( qicn(im,jm,lm) )

      do L=1,lm
         read(20)   qv(:,:,L)
      enddo
      do L=1,lm
         read(20) qlls(:,:,L)
      enddo
      do L=1,lm
         read(20) qlcn(:,:,L)
      enddo
      do L=1,lm
         read(20) cfls(:,:,L)
      enddo
      do L=1,lm
         read(20) cfcn(:,:,L)
      enddo
      do L=1,lm
         read(20) qils(:,:,L)
      enddo
      do L=1,lm
         read(20) qicn(:,:,L)
      enddo

      close (20)

! **********************************************************************
! ****                 Compute/Import Grid-Cell Area                ****
! **********************************************************************

           allocate ( area(im,jm) )
      call Get_Area ( area,im,jm,afile  )

! **********************************************************************
! ****                       Constrain PDRY                         ****
! **********************************************************************

      allocate ( gsum(5,2)   )
      allocate ( qsum(im,jm) )

      do n=1,5
         qsum = 0.0_8
         do L=1,lm
         if( n.eq.1 ) qsum = qsum +   qv(:,:,L)*dp(:,:,L)
         if( n.eq.2 ) qsum = qsum + qlls(:,:,L)*dp(:,:,L)
         if( n.eq.3 ) qsum = qsum + qlcn(:,:,L)*dp(:,:,L)
         if( n.eq.4 ) qsum = qsum + qils(:,:,L)*dp(:,:,L)
         if( n.eq.5 ) qsum = qsum + qicn(:,:,L)*dp(:,:,L)
         enddo
         call AreaMean( qsum, area, gsum(n,1), im,jm )
      enddo

! --------------------------------------

      allocate (   psold(im,jm) )
      allocate (   psnew(im,jm) )
      allocate ( pdryold(im,jm) )
      allocate ( pdrynew(im,jm) )

      qsum = 0.0_8
      do L=1,lm
      qsum = qsum + (  qv(:,:,L) + &
                     qlls(:,:,L) + &
                     qlcn(:,:,L) + &
                     qils(:,:,L) + &
                     qicn(:,:,L) ) * dp(:,:,L)
      enddo
        psold = ple(:,:,lm+1)
      pdryold = ple(:,:,lm+1) - qsum  ! Subtract Total Water Content

      call AreaMean( pdryold, area, pdryold_ave, im,jm )

      pdrynew = pdryold * ( pdry_ave/pdryold_ave )

        psnew = pdrynew + qsum

      do L=1,lm+1
      do j=1,jm
      do i=1,im
       ple(i,j,L) = ple(i,j,L) + bk(L)*( psnew(i,j)-psold(i,j) )
      enddo
      enddo
      enddo

      pke = ple**kappa
      do L=1,lm
      dp(:,:,L) =             ple(:,:,L+1)-ple(:,:,L)
      pk(:,:,L) =            (pke(:,:,L+1)-pke(:,:,L)) &
                / ( kappa*log(ple(:,:,L+1)/ple(:,:,L)) )
      enddo

! --------------------------------------

      do n=1,5
         qsum = 0.0_8
         do L=1,lm
         if( n.eq.1 ) qsum = qsum +   qv(:,:,L)*dp(:,:,L)
         if( n.eq.2 ) qsum = qsum + qlls(:,:,L)*dp(:,:,L)
         if( n.eq.3 ) qsum = qsum + qlcn(:,:,L)*dp(:,:,L)
         if( n.eq.4 ) qsum = qsum + qils(:,:,L)*dp(:,:,L)
         if( n.eq.5 ) qsum = qsum + qicn(:,:,L)*dp(:,:,L)
         enddo
         call AreaMean( qsum, area, gsum(n,2), im,jm )
      enddo

        qv =   qv * ( gsum(1,1)/gsum(1,2) )
      qlls = qlls * ( gsum(2,1)/gsum(2,2) )
      qlcn = qlcn * ( gsum(3,1)/gsum(3,2) )
      qils = qils * ( gsum(4,1)/gsum(4,2) )
      qicn = qicn * ( gsum(5,1)/gsum(5,2) )

      qsum = 0.0_8
      do L=1,lm
      qsum = qsum + (  qv(:,:,L) + &
                     qlls(:,:,L) + &
                     qlcn(:,:,L) + &
                     qils(:,:,L) + &
                     qicn(:,:,L) ) * dp(:,:,L)
      enddo
      pdrynew = ple(:,:,lm+1) - qsum  ! Subtract Total Water Content

      call AreaMean( pdrynew, area, pdrynew_ave, im,jm )

      write(6,1001) pdrynew_ave/100,pdryold_ave/100,pdrynew_ave/pdryold_ave
 1001 format(1x,'PSDRY_NEW: ',g,'  PSDRY_OLD: ',g,'  RATIO: ',g)

! **********************************************************************
! ****                 Write dycore internal Restart                ****
! **********************************************************************

      allocate ( dum8(im,jm) )

      open (10,file=trim(dynrst),form='unformatted',access='sequential')

      dynrst = trim(dynrst) // '.scaled'
      print *
      print *, 'Creating GEOS-5 fvcore_internal_restart: ',trim(dynrst)
      open (20,file=trim(dynrst),form='unformatted',access='sequential')

      read (10) headr1
      read (10) headr2
      read (10) ak
      read (10) bk

      write(20) headr1
      write(20) headr2
      write(20) ak
      write(20) bk

          do L=1,lm
             read (10) dum8
             write(20) u(:,:,L)
          enddo
          do L=1,lm
             read (10) dum8
             write(20) v(:,:,L)
          enddo
          do L=1,lm
             read (10) dum8
             write(20) th(:,:,L)
          enddo
          do L=1,lm+1
             read (10) dum8
             write(20) ple(:,:,L)
          enddo
          do L=1,lm
             read (10) dum8
             write(20) pk(:,:,L)
          enddo

                rc =  0
      do while (rc.eq.0)
         read (10,iostat=rc)     dum8
         if( rc.eq.0 ) write(20) dum8
      enddo

      close (10)
      close (20)

! **********************************************************************
! ****                  Write moist internal Restart                ****
! **********************************************************************

      allocate ( dum4(im,jm) )

      open (10,file=trim(mstrst),form='unformatted',access='sequential')

      mstrst = trim(mstrst) // '.scaled'
      print *
      print *, 'Creating GEOS-5  moist_internal_restart: ',trim(mstrst)
      open (20,file=trim(mstrst),form='unformatted',access='sequential')

      do L=1,lm
         read (10) dum4
         write(20)   qv(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) qlls(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) qlcn(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) cfls(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) cfcn(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) qils(:,:,L)
      enddo
      do L=1,lm
         read (10) dum4
         write(20) qicn(:,:,L)
      enddo

                rc =  0
      do while (rc.eq.0)
         read (10,iostat=rc)     dum4
         if( rc.eq.0 ) write(20) dum4
      enddo

      close (10)
      close (20)

      stop
      end

! --------------------------------------------------------------

      subroutine Get_Area ( area,im,jm,afile )
      implicit   none
      integer    im,jm
      real  area(im,jm)
      character*256 :: afile
      character*256 :: Usage="rs_scale.x  fv_internal_rst  moist_internal_rst  <grid_cell_area>"

      real*8, allocatable :: cosp(:)
      real*8, allocatable ::   da(:,:)
      real*8              :: pi,dl,dp,acap,qdum
      integer i,j

      ! LatLon Area
      ! -----------
      if( jm.ne.im*6 .and. trim(afile).eq.'NULL' ) then
      allocate( da(im,jm) )
      allocate(  cosp(jm) )

      pi = 4.0D0 * Datan(1.0D0)
      dl = 2.0D0 * pi/ im
      dp =         pi/(jm-1)

      do j=2,jm-1
         cosp(j) = Dcos( -pi/2 + (j-1)*dp )
      enddo

      do j=1,jm
         if ( j == 1  ) then
              dA(:,j) = 2*pi*(1-Dcos(dp/2))/im
         else if ( j == jm ) then
              dA(:,j) = 2*pi*(1-Dcos(dp/2))/im
         else
              dA(:,j) = cosp(j) * dl*dp
         endif
      enddo

      area = dA

      deallocate( da   )
      deallocate( cosp )

      else
      ! Cube Area
      ! ---------
        if( trim(afile).ne.'NULL' ) then
            read(30) area
        else
            print *, "You must provide a file for the Grid-Cell Area"
            print *, trim(Usage)
            call exit(1)
        endif
      endif

      return
      end subroutine Get_Area

! --------------------------------------------------------------

      subroutine AreaMean ( q,area,qave,im,jm )
      implicit     none
      integer      im,jm
      real*4  area(im,jm)
      real*8     q(im,jm)
      real*8     qave
      real*8     qdum

      integer i,j

      qave = 0.0_8
      qdum = 0.0_8
      do j=1,jm
         do i=1,im
            qave = qave + q(i,j)*area(i,j)
            qdum = qdum +        area(i,j)
         enddo
      enddo

      qave = qave / qdum

      return
      end subroutine AreaMean

