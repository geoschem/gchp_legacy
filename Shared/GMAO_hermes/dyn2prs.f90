      program dyn2prs

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dyn2prs: converts dynamics vector from eta to pressure
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use m_dyn
      use m_insitu
      use m_const, only : grav, undef

      implicit NONE

! !DESCRIPTION: Uses the {\em Insitu Package} {\tt m\_insitu} to convert
!               the dynamics state vector from hybrid eta coordinates to
!  pressure coordinates. The current version hardwires the same 36 pressure
!  levels used by GEOS-3 DAS.
!
!
! !REVISION HISTORY:
!
!  09Mar2000  da Silva  Initial code.
!  23Apr2001  da Silva  Made RUC compliant
!  15May2001  E. Yeh    Added four options for output file: -im value -jm value
!                       -South value -West value.               
!
!-------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname = 'dyn2prs'

!     File names
!     ----------
      integer, parameter :: MFILES = 512 ! max.   number of input files
      character(len=255) :: etafiles(MFILES), etafile
      character(len=255) :: prsfile
      integer            :: nfiles       ! actual no. of input files

!     Vertical pressure levels
!     ------------------------
      integer, parameter :: MLEVS = 36  ! max.   number pressure levels
      integer            :: nlevs       ! actual number pressure levels
      real               :: plevs(MLEVS) ! pressule levels in hPa

!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) w_e  ! dynamics vector in eta
      type(dyn_vect) w_p  ! dynamics vector in pressure
      type(sim_vect) w_s  ! simulator vector


!     Obs-like coordinates
!     --------------------
      integer  :: im, jm, km, nobs, nobs1
      real, allocatable :: lat(:), lon(:), lev(:), conf(:)
      integer  :: imOut, jmOut
      real     :: xWest, ySouth

!     Locals
!     ------
      integer, parameter :: READ_ONLY = 1
      integer fid, nvars, ngatts
      integer ios, rc, iopt, ifile
      integer ntimes, n, freq, nymd, nhms, prec
 

!                                 *******


!  Initialize
!  ----------     
   call Init_ ( mfiles, etafiles, nfiles, prsfile, mlevs, plevs, nlevs, &
      imOut, jmOut, xWest, ySouth )


!  Loop over input eta files
!  -------------------------
   do ifile = 1, nfiles

      etafile = etafiles(ifile)

!     Determine how many time levels on file
!     --------------------------------------
      call GFIO_Open ( etafile, READ_ONLY, fid, rc )
      if ( rc .ne. 0 ) then
         call die(myname,'cannot open GFIO file '//trim(etafile))
      end if
      call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars, ngatts, rc)
      if ( rc .ne. 0 ) then
         call die(myname,'problems getting dimensions' )
      end if
      call GFIO_Close ( fid, rc )
      
!     set imOut and jmOut as im and jm if not defined
!     -----------------------------------------------
      if (imOut .eq. -1) imOut = im     ! user not defined
      if (jmOut .eq. -1) jmOut = jm

!     Allocate memory for obs-like vectors
!     ------------------------------------
      nobs = imOut * jmOut * nlevs
      nobs1 = imOut * jmOut
      allocate ( lat(nobs), lon(nobs), lev(nobs), conf(nobs), stat=ios ) 
      if ( ios .ne. 0 ) call die (myname,'cannot allocate memory')

!     For each time on file...
!     ------------------------
      do n = 1, ntimes

!        Get ETA data for this time
!        --------------------------
         call dyn_get ( etafile, nymd, nhms, w_e, rc, timidx=n, freq=freq )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read dynamics vector file')
         end if


!        set xWest and ySouth as min of w_e if not defined
!        -------------------------------------------------
         if ( xWest .eq. undef)  xWest  = w_e%grid%lon_min
         if ( ySouth .eq. undef) ySouth = w_e%grid%lat_min

!        Generate lat-lon-plevs grid
!        ---------------------------
         call get_coords_()

!        Initialize simulator vector (needed for insitu calculations)
!        -----------------------------------------------------------
         call Insitu_Init ( w_e, w_s, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot initialize w_s' )

!        Initialize dynamics vector in pressure coordinates
!        Note: ks, ak, and bk are not relevant for PRS files
!        ----------------------------------------------------
         call Dyn_Init ( imOut, jmOut, nlevs, 1, w_p, rc,  &
                         w_e%grid%ptop, w_e%grid%ks, &
                         w_e%grid%ak,   w_e%grid%bk )
         if ( rc .ne. 0 ) call die(myname,'cannot initialize w_p' )

!        Update w_p with xWest and ySouth
!        --------------------------------
         w_p%grid%lon_min = xWest
         w_p%grid%lon_del = 360.0 / imOut
         w_p%grid%lon_max = w_p%grid%lon_min + (imOut-1) * w_p%grid%lon_del
         w_p%grid%lat_min = ySouth
         w_p%grid%lat_del = 180.0 / ( jmOut-1)
         if (ySouth .gt. 0.0) w_p%grid%lat_del = -w_p%grid%lat_del
         w_p%grid%lat_max = w_p%grid%lat_min + (jmOut-1) * w_p%grid%lat_del
 
!        Use insitu package for the eta-->prs calculation
!        Notice that for PRS files we redefine variables:
!                    ps   := slp
!                    delp := hght
!                    pt   := tmpu
!        This fact is communicated to dyn_put() by specifying
!        the optional parameter PLEVS.
!        ----------------------------------------------------
         iopt = 1

         call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'zs', iopt, &
                            w_p%phis, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate topography geopotential' )
         w_p%phis = w_p%phis * grav
         call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'ts', iopt, &
                            w_p%ts, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate surface temperature' )
         call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'lwi', iopt, &
                            w_p%lwi, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate land-water-ice mask' )

         call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'mslp', iopt, &
                            w_p%ps, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate mslp' )
         call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'hght', iopt, &
                            w_p%delp, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate hght' )
         call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'tmpu', iopt, &
                            w_p%pt, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate tmpu' )
         call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'sphu', iopt, &
                            w_p%q, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate sphu' )

         call Insitu_Wind ( w_s, lon, lat, lev, nobs, 'wind', iopt, &
                            w_p%u, w_p%v, conf, rc )
         if ( rc .ne. 0 ) call die(myname,'cannot simulate wind' )


!        Keep other 2D fields the same
!        -----------------------------
!         w_p%phis(1:im,1:jm)        = w_e%phis(1:im,1:jm)
!         w_p%hs_stdv(1:im,1:jm)     = w_e%hs_stdv(1:im,1:jm)
!         w_p%ts(1:im,1:jm)          = w_e%ts(1:im,1:jm)
!         w_p%lwi(1:im,1:jm)         = w_e%lwi(1:im,1:jm)
         w_p%hs_stdv(1:imOut,1:jmOut) = undef 

!        Write out pressure file
!        -----------------------
         prec = 0               ! 32 bits
         write(*,'(a,i8,i3,a,i8)') myname // ': writing ' // trim(prsfile) // &
                                ' on ', nymd, nhms/10000, 'Z, freq = ', freq 
         call dyn_put ( prsfile, nymd, nhms, prec, w_p, rc, &
                        freq=freq, plevs=plevs )
         if ( rc .ne. 0 ) call die (myname,'cannot write PRS file' )

!        Clean up mess
!        -------------
         call    dyn_clean ( w_e )
         call    dyn_clean ( w_p )
         call insitu_clean ( w_s )

      end do

      deallocate ( lat, lon, lev, conf )

   end do ! loop over files

!  All done
!  --------
   call exit(0)

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn2prs
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( mfiles, etafiles, nfiles, prsfile, &
                         mlevs, plevs, nlevs,               &
                         imOut, jmOut, xWest, ySouth )

      implicit NONE

      integer,       intent(in)  :: mfiles  ! max. number of eta files
      integer,       intent(in)  :: mlevs   ! max. number of pressure levs

                                            ! dynamics file names (eta)
      character*255, intent(out) :: etafiles(mfiles) 
      character*255, intent(out) :: prsfile ! dynamics file name (prs)
                                            ! pressure levs (hPa)
      real,          intent(out) :: plevs(mlevs) 
      integer,       intent(out) :: nfiles  ! actual no. of eta files
      integer,       intent(out) :: nlevs   ! actual no. of pressure levels
      
      integer,       intent(out) :: imOut, jmOut  ! number of grid pts output in lon/lat
      real,          intent(out) :: xWest, ySouth ! Starting coordinates for lon/lat

!
! !REVISION HISTORY:
!       09Mar2000  da Silva  Initial code.
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer i, iarg, argc, iargc
      character(len=255) :: etafile, argv

      print *
      print *, '     -----------------------------------------------'
      print *, '     dyn2prs - convert dynamics file from eta to prs'
      print *, '     -----------------------------------------------'
      print *

!     Parse command line
!     ------------------
      prsfile = 'dyn.prs.hdf'
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()

      iarg = 0
      nfiles = 0
      imOut = -1
      jmOut = -1
      xWest = undef
      ySouth = undef

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )
         select case (argv)
           case ("-South")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv, *) ySouth
             if (ySouth .ne. 90. .and. ySouth .ne. -90.) call usage()
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, prsfile )
           case ("-im")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv, *) imOut
           case ("-jm")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv, *) jmOut
           case ("-West")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv, *) xWest
             if (xWest .gt. 0.0) xWest = xWest - 360.
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call die(myname,'too many eta files')
             etafiles(nfiles) = argv
         end select
      end do

      if ( nfiles .lt. 1 ) call usage()

!     If not specified, come up with a good name for prs file
!     -------------------------------------------------------
      etafile = etafiles(1)
      if ( trim(prsfile) .eq. 'dyn.prs.hdf' ) then 
         i = index ( etafile, '.eta.' )
         if ( i .gt. 1 ) then
            prsfile = etafile(1:i-1) // '.prs.' // etafile(i+5:)
         end if
      end if

!     For now hardwire pressure levels
!     --------------------------------
      if ( MLEVS .lt. 36 ) then
        call die(myname,'increase MLEVS to at least 36' )
      end if
      nlevs = 36
      plevs(1:nlevs) = (/                                                  &
                       0.2, 0.4, 1., 2., 3., 5., 7., 10., 20., 30., 40.,   &
                       50., 70., 100., 150., 200., 250., 300., 350., 400., & 
                       450., 500., 550., 600., 650., 700., 750., 800.,     &
                       825., 850., 875., 900., 925., 950., 975., 1000.     &
                       /)


!     Echo the parameters
!     -------------------
      print *
      print *, '------------------------------------------------------------------'
      print *, '  Eta     Dynamics state files: ', nfiles
      do i = 1, nfiles
      print *, '                                ', trim(etafiles(i))
      end do
      print *, '  Pressure Dynamics state file: ', trim(prsfile)
      print *, '  Pressure Levels: ', nlevs
      write(*,'(20x,5F9.2)') ( plevs(i), i = 1, nlevs )
      print *, '-------------------------------------------------------------------'
      print *

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *,'Usage: '
      print *,'  dyn2prs.x [-o prsfile -im imOut -jm jmOut -West xWest -South ySouth] etafile(s)'
      print *
      print *, 'where'
      print *
      print *, '-o prsfile    output dynamics vector file in'
      print *, '              pressure coordinates. Default:'
      print *, '              same as first "etafile" with substring'
      print *, '              ".eta." replaced with ".prs."'
      print *, '-im imOut     number of grid points output in longitude'
      print *, '-jm jmOut     number of grid points output in latitude'
      print *, '-West xWest   output starting longitude (degree)'
      print *, '-South ySouth output starting latitude (-90 or 90 degree)'
      print *, '-etafile(s)   input dynamics vector file in'
      print *, '              hybrid (eta) coordinates'
      print *
      call exit(1)
      end subroutine usage
      
!.................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      write(*,'(a)') trim(myname) // ': ' // trim(msg)
      call exit(1)
      end subroutine die

!.................................................................

      subroutine get_coords_()

!
!     Generates lat-lon-lev coordinates of gridded fields for
!     interfacing with m_insitu.
!
      character(len=*), parameter :: myname = 'get_coords_'

      integer i, j, k, m
      real x, y, z, xDel, yDel

      yDel = 180.0 / (jmOut - 1)
      if (ySouth .gt. 0.0) yDel = -yDel
      xDel = 360.0 / imOut
      m = 0
      do k = 1, nlevs
         z = plevs(k)
         do j = 1, jmOut
  !          y = w_e%grid%lat_min + (j-1.0) * w_e%grid%lat_del
            y = ySouth + (j-1.0) * yDel
            do i = 1, imOut
  !             x = w_e%grid%lon_min + (i-1.0) * w_e%grid%lon_del
               x = xWest + (i-1.0) * xDel
               if ( x .gt. 180.0 ) x = x - 360.0

               m = m + 1
               lon(m) = x
               lat(m) = y
               lev(m) = z

            end do
         end do
      end do

      if ( m .ne. nobs ) call die(myname,'inconsistent m/nobs' )               

    end subroutine get_coords_

  end program dyn2prs
