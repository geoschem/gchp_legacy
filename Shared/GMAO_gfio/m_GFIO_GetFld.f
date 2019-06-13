      module  m_GFIO_GetFld

! !USES:

      implicit none

      PRIVATE
      PUBLIC GFIO_GetFld

      interface GFIO_GetFld; module procedure
     &          GFIO_Get_Fall_,  ! GEOS-3
     &          GFIO_Get_2d_,    ! GEOS-3
     &          GFIO_Get_3d_     ! GEOS-3
      end interface
      interface Fix_Undef_In; module procedure
     &          Fix_Undef_In_
      end interface

!  Internal Interfaces
!  -------------------
      interface Hflip_
        module procedure HFlip2_    ! Horizontal flip of 2d fields
        module procedure HFlip3_    ! Horizontal flip of 3d fields
      end interface

      character(len=*), parameter :: myname = 'GFIO_GetFld'

      logical, parameter :: verb   = .true.
      integer, parameter :: stdout = 6
      integer, parameter :: stderr = 6
      real   , parameter :: UNDEF  = 1.e15
      real   , parameter :: rtol   = 1.e-3  ! 25Sep2007 Todling: Adjusted tol

      CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Get_Fall_ --- 
! 
! !INTERFACE:
!
      subroutine GFIO_Get_Fall_ ( gfiofile, nymd, nhms, im, jm, mlev, plevs,
     &                            nvars2d, vname2d, fld2d,
     &                            nvars3d, vname3d, fld3d,
     &                            stat )

! !USES:

      Implicit NONE

!
! !INPUT PARAMETERS: 
!


      character*(*) gfiofile       ! Input GFIO file name
                                   ! Synoptic time:
      integer       nymd           !  Year-month-day, e.g.,  19971012
      integer       nhms           !  hour-minute-sec, e.g.,   120000
      integer       im
      integer       jm
      integer       mlev
      integer       nvars2d
      integer       nvars3d
      character*(*) vname2d(nvars2d)
      character*(*) vname3d(nvars3d)
      real          plevs(mlev)

! !OUTPUT PARAMETERS:

      real          fld2d(im,jm,nvars2d)
      real          fld3d(im,jm,mlev,nvars3d)

      integer, intent(out), optional :: stat

!
! !DESCRIPTION: Read grided analysis fields using GFIO pre- and post-
!               analysis files.
!
! !TO DO:
!   replace all exits by output return code
!
! !REMARKS:
!
! !REVISION HISTORY: 
!
!  21Nov2000 Todling   Initial code.
!  24Feb2006 Todling   Adapted to handle flipped fields from GEOS-5.
!  11Jul2007 Stassi/RT Fix for lon precision issue between G5 and G4 files.
!  16Jul2008 Todling   Lon check now using 64-bin array
!
!EOP
!-------------------------------------------------------------------------
!_BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Get_Fall_'

                                   ! Grid Information
                                   !    direction
      integer       ier
      integer       j, k

      integer       ngatts              ! number of attributes on file
      integer       nvars, nvars_in
      integer       im_max, jm_max, mlev_max, lm_max
      integer       n2dim, n3dim
      integer       iv, icount
      integer       ncid
      integer       dimSize, dimId
      integer       ncvid
      character*255 dimName
      integer       kloc(1), levcnt
      logical       usrlevs
      logical       fliplon

      real*8  mxlon, mnlon
      real*4, allocatable :: lon32(:)
      real*8, allocatable :: lon64(:)
      real, allocatable :: lev(:)
      real, allocatable :: aux(:,:,:,:)

!     Define constants
!     ----------------
      nvars = nvars2d + nvars3d
      usrlevs = .true.
      if ( present(stat) ) stat = 0

!     Open GFIO file
!     --------------
      im_max   = im
      jm_max   = jm
      mlev_max = mlev
      call GFIO_Open ( trim(gfiofile), 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( nvars .gt. nvars_in ) then
             write(stdout,'(a,2(a,i5))') myname_,
     &                    ': Warning, no. of variable in file: ', nvars_in,
     &                    ' smaller than requested no. of vars: ', nvars
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed longitudinal dim'
            stop 7
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed latitudinal dim'
            stop 7
         endif

         if (mlev_max .ne. mlev) then
            write(stdout,'(2a,i5)') myname_,
     &           '-WARNING: trying to read unallowed number of levels'
            usrlevs = .false.
         endif


      else

         write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         flush (stdout)
         if ( present(stat) ) then
           stat = 99
           return
         else
           stop 7
         end if

      endif


!     OK, let's read data from file
!     =============================
      icount = 0
      n2dim  = im * jm
      n3dim  = n2dim * mlev

!     Get longitude array to check on GEOS-5 flipped fields
!     -----------------------------------------------------
      allocate ( lon64(im), stat=ier )
        if (ier/=0) then
            write(stderr,'(2a,i10,2a)') myname_, 'Alloc(lon) error '
            flush (stderr); stop 7
        endif

      call ncdinq (ncid, 1, dimName, dimSize, ier)
         if (ier/=0)then
             write(stderr,'(2a)') myname_, 'Error(ncdinq)'
             flush (stderr); stop 7
         endif
      dimId = ncvid (ncid, dimName, ier)
         if (ier/=0)then
             write(stderr,'(2a)') myname_, 'Error(ncvid)'
             flush (stderr); stop 7
         endif

!     Caution: will try to read as 64-bit first ...
!     ---------------------------------------------
      call ncvgt ( ncid, dimId, 1, im, lon64, ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_, 
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', 'lon'
             flush(stderr); stop 7
            endif

!     ... if absurd lons, read as 32-bit
!     ----------------------------------
      mnlon = minval(lon64)
      mxlon = maxval(lon64)
      if ( mnlon < -200.0 .or. mxlon > 400.0 ) then
         allocate ( lon32(im), stat=ier )
         call ncvgt ( ncid, dimId, 1, im, lon32, ier )
         lon64=lon32
         deallocate ( lon32 )
      endif

!     Is orientation of fields shifted by 180 degs from internal def?
!     ---------------------------------------------------------------
      fliplon = .false.
      if ( abs(lon64(1)+180.0)<rtol ) then
        fliplon = .true.  ! orientation of fields shifted by 180 degs from internal def
        print *, ' Flipping Fields Longitudinally ... '
      endif
      deallocate ( lon64 )

!     First read surface fields
!     -------------------------
      do iv = 1, nvars2d
      call Gfio_GetVar ( ncid, vname2d(iv), nymd, nhms,
     &                   im, jm, 0, 1, fld2d(1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_, 
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', trim(vname2d(iv))
             stop 7
           else
             call fix_undef_in_ ( n2dim, fld2d(:,:,iv:iv), vname2d(iv) )
           end if
           if( fliplon ) call hflip_ ( fld2d(1:im,1:jm,iv), im, jm )
           icount = icount + 1
      end do

!     Now read upper-air fields
!     -------------------------
      if ( usrlevs ) then
        do iv = 1, nvars3d
           call Gfio_GetVar ( ncid, vname3d(iv), nymd, nhms,
     &                        im, jm, 1, mlev, fld3d(1,1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10)') myname_,
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', trim(vname3d(iv))
             stop 7
           else
             call fix_undef_in_ ( n3dim, fld3d(:,:,:,iv:iv), vname3d(iv) )
           end if
           if( fliplon ) call hflip_ ( fld3d(1:im,1:jm,1:mlev,iv), im, jm, mlev )
           icount = icount + 1
        end do

      else

        allocate ( aux(im,jm,mlev_max,1), lev(mlev_max), stat=ier )
             if ( ier .ne. 0 ) then
                  write(stderr,'(2a,i10)') myname_, 
     .                         ': Alloc(aux), ier = ', ier
                  stop 7
             end if
        call Gfio_GetVar ( ncid, 'Height', nymd, nhms,
     &                     mlev_max, 1, 0, 1, lev, ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10)') myname_,
     &                    ': Gfio_GetVar (lev), ier = ', ier
             stop 7
           end if

        do iv = 1, nvars3d
           call Gfio_GetVar ( ncid, vname3d(iv), nymd, nhms,
     &                        im, jm, 1, mlev_max, aux, ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', trim(vname3d(iv))
             stop 7
           else
             call fix_undef_in_ ( im*jm*mlev_max, aux, vname3d(iv) )
           end if
           if( fliplon ) call hflip_ ( fld3d(1:im,1:jm,1:mlev_max,iv), im, jm, mlev_max )

           levcnt = 0
           fld3d(:,:,:,iv:iv) = undef
           do k = 1, mlev
              kloc = maxloc(lev(1:mlev_max),
     .               mask=abs(log(plevs(k))-log(lev(1:mlev_max))).lt.1e-5)
              j    = kloc(1)
              if (j>0) then
                 fld3d(:,:,k:k,iv:iv) = aux(:,:,j:j,1:1)
                 levcnt = levcnt + 1
              end if
           end do
           if ( levcnt.ne.mlev ) then
             write(stdout,'(a,2(i10))') myname_,
     &                    'WARNING: Requested levs not found, levcnt = ', 
     &                     levcnt, ' levs requested ', mlev
           end if
           icount = icount + 1
        end do

        deallocate ( aux, lev, stat=ier )

      end if

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )
      if ( ier .ne. 0 ) then
           write(stdout,'(2a,i7)') myname_,
     &                  ': WARNING, problems closing GFIO file, ier = ', ier
      end if

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  read ', icount, ' variables from ', trim(gfiofile)


!     All done
!     --------
      return
      end subroutine GFIO_Get_Fall_
!EOC

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Get_2d_ --- 
! 
! !INTERFACE:
!
      subroutine GFIO_Get_2d_ ( gfiofile, nymd, nhms, im, jm,
     &                          nvars, vname, fld )

! !USES:

      Implicit NONE

!
! !INPUT PARAMETERS: 
!


      character*(*) gfiofile       ! Input GFIO file name
                                   ! Synoptic time:
      integer       nymd           !  Year-month-day, e.g.,  19971012
      integer       nhms           !  hour-minute-sec, e.g.,   120000
      integer       im
      integer       jm
      integer       nvars
      character*(*) vname(nvars)

! !OUTPUT PARAMETERS:

      real          fld(im,jm,nvars)

!
! !DESCRIPTION: Read grided analysis fields using GFIO pre- and post-
!               analysis files.
!
! !TO DO:
!
! !REMARKS:
!
! !REVISION HISTORY: 
!
!  21Nov2000 Todling   Initial code.
!  24Feb2006 Todling   Adapted to handle flipped fields from GEOS-5.
!  11Jul2007 Stassi/RT Fix for lon precision issue between G5 and G4 files.
!  16Jul2008 Todling   Lon check now using 64-bin array
!
!EOP
!-------------------------------------------------------------------------
!_BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Get_2d_'

                                   ! Grid Information
                                   !    direction
      integer       ier

      integer       ngatts              ! number of attributes on file
      integer       nvars_in
      integer       im_max, jm_max, mlev_max, lm_max
      integer       n2dim
      integer       iv, icount
      integer       ncid
      integer       ncvid
      integer       dimId, dimSize
      character*255 dimName

      real*8  mxlon, mnlon
      real*4, allocatable :: lon32(:)
      real*8, allocatable :: lon64(:)
     
      logical       fliplon

!     Open GFIO file
!     --------------
      im_max   = im
      jm_max   = jm
      call GFIO_Open ( trim(gfiofile), 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( nvars .gt. nvars_in ) then
             write(stdout,'(a,2(a,i5))') myname_,
     &                    ': Warning, no. of variable in file: ', nvars_in,
     &                    ' smaller than requested no. of vars: ', nvars
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed longitudinal dim'
            stop 7
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed latitudinal dim'
            stop 7
         endif


      else

         write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         flush (stdout)
         stop 7

      endif


!     OK, let's read data from file
!     =============================
      icount = 0
      n2dim  = im * jm

!     Get longitude array to check on GEOS-5 flipped fields
!     -----------------------------------------------------
      allocate ( lon64(im), stat=ier )
        if (ier/=0) then
            write(stderr,'(2a,i10,2a)') myname_, 'Alloc(lon) error '
            flush (stdout); stop 7
        endif
            
      call ncdinq (ncid, 1, dimName, dimSize, ier)
         if (ier/=0)then
             write(stderr,'(2a)') myname_, 'Error(ncdinq)'
             flush (stderr); stop 7
         endif
      dimId = ncvid (ncid, dimName, ier)
         if (ier/=0)then
             write(stderr,'(2a)') myname_, 'Error(ncvid)'
             flush (stderr); stop 7
         endif

!     Caution: will try to read as 64-bit first ...
!     ---------------------------------------------
      call ncvgt ( ncid, dimId, 1, im, lon64, ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', 'lon'
             flush(stderr); stop 7
            endif

!     ... if absurd lons, read as 32-bit
!     ----------------------------------
      mnlon = minval(lon64)
      mxlon = maxval(lon64)
      if ( mnlon < -200.0 .or. mxlon > 400.0 ) then
         allocate ( lon32(im), stat=ier )
         call ncvgt ( ncid, dimId, 1, im, lon32, ier ) 
         lon64=lon32
         deallocate ( lon32 )
      endif

!     Is orientation of fields shifted by 180 degs from internal def?
!     ---------------------------------------------------------------
      fliplon = .false.
      if ( abs(lon64(1)+180.0)<rtol ) then
        fliplon = .true.  ! orientation of fields shifted by 180 degs from internal def
        print *, ' Flipping Fields Longitudinally ... '
      endif
      deallocate ( lon64 )

!     First read surface fields
!     -------------------------
      do iv = 1, nvars
      call Gfio_GetVar ( ncid, vname(iv), nymd, nhms,
     &                   im, jm, 0, 1, fld(1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10)') myname_, 
     &                    ': Gfio_GetVar, ier = ', ier, 
     &                    ' for variable ', trim(vname(iv))
             stop 7
           else
             call fix_undef_in_ ( n2dim, fld(:,:,iv:iv), vname(iv) )
           end if
           if( fliplon ) call hflip_ ( fld(1:im,1:jm,iv), im, jm )
           icount = icount + 1
      end do

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )
      if ( ier .ne. 0 ) then
           write(stdout,'(2a,i7)') myname_,
     &                  ': WARNING, problems closing GFIO file, ier = ', ier
      end if

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  read ', icount, ' variables from ', trim(gfiofile)


!     All done
!     --------
      return
      end subroutine GFIO_Get_2d_
!_EOC


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Get_3d_ --- 
! 
! !INTERFACE:
!
      subroutine GFIO_Get_3d_ ( gfiofile, nymd, nhms, im, jm, mlev,
     &                          nvars, vname, fld, 
     &                          plevs, verbose, stat )   ! optional

! !USES:

      Implicit NONE

!
! !INPUT PARAMETERS: 
!


      character*(*) gfiofile       ! Input GFIO file name
                                   ! Synoptic time:
      integer       nymd           !  Year-month-day, e.g.,  19971012
      integer       nhms           !  hour-minute-sec, e.g.,   120000
      integer       im
      integer       jm
      integer       mlev
      integer       nvars
      character*(*) vname(nvars)
      real, intent(in), optional ::  plevs(mlev)
      logical, intent(in), optional :: verbose

! !OUTPUT PARAMETERS:

      real          fld(im,jm,mlev,nvars)

! !INPUT/OUTPUT PARAMETERS:

      integer, intent(out), optional :: stat   ! stat=1 incompatible dims
                                               ! stat=2 date/time not found
                                               ! stat=3 var not found
                                               ! stat=4 cannot alloc mem
!
! !DESCRIPTION: Read grided analysis fields using GFIO pre- and post-
!               analysis files.
!
! !TO DO:
!
! !REMARKS:
!
! !REVISION HISTORY: 
!
!  21Nov2000 Todling   Initial code.
!  05Oct2004 Todling   Generalized interface in an attempt to remove redundant
!                      interface added by adjoint testing group.
!  24Feb2006 Todling   Adapted to handle flipped fields from GEOS-5.
!  01Feb2007 Todling   Add check for ROOT pe
!  11Jul2007 Stassi/RT Fix for lon precision issue between G5 and G4 files.
!  27Sep2007 Todling   Replace ROOT check by verbose
!  16Jul2008 Todling   Lon check now using 64-bin array
!
!EOP
!-------------------------------------------------------------------------
!_BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Get_3d_'

                                   ! Grid Information
                                   !    direction
      integer       ier
      integer       j, k

      integer       ngatts              ! number of attributes on file
      integer       nvars_in
      integer       im_max, jm_max, mlev_max, lm_max
      integer       n2dim, n3dim
      integer       iv, icount
      integer       ncid
      integer       dimId, dimSize
      integer       ncvid
      integer       kloc(1), levcnt
      character*255 dimName
      logical       usrlevs
      logical       fliplon
      logical       verbose_

      real*8  mxlon, mnlon
      real*4, allocatable :: lon32(:)
      real*8, allocatable :: lon64(:)
      real, allocatable :: lev(:)
      real, allocatable :: aux(:,:,:,:)

      verbose_ = verb
      if(present(verbose)) verbose_ = verbose

!     Open GFIO file
!     --------------
      im_max   = im
      jm_max   = jm
      mlev_max = mlev
      usrlevs = .true.
      if(present(stat)) stat = 0
      call GFIO_Open ( trim(gfiofile), 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( nvars .gt. nvars_in ) then
             if(verbose_) write(stdout,'(a,2(a,i5))') myname_,
     &                    ': Warning, no. of variable in file: ', nvars_in,
     &                    ' smaller than requested no. of vars: ', nvars
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            if(verbose_) write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed longitudinal dim'
            if ( present(stat) ) then
                 stat = 1
                 return
            else
                 stop 7
            endif
         endif

         if (jm_max .ne. jm) then
            if(verbose_) write(stderr,'(2a,i5)') myname_,
     &           ': trying to read unallowed latitudinal dim'
            if ( present(stat) ) then
                 stat = 1
                 return
            else
                 stop 7
            endif
         endif

         if (mlev_max .ne. mlev) then
            if(verbose_) write(stdout,'(2a,i5)') myname_,
     &           '-WARNING: trying to read unallowed number of levels'
            usrlevs = .false.
         endif


      else

         if(verbose_) write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         flush (stdout)
         if ( present(stat) ) then
              stat = 2
              return
         else
              stop 7
         endif
      endif


!     OK, let's read data from file
!     =============================
      icount = 0
      n2dim  = im * jm
      n3dim  = n2dim * mlev

!     Get longitude array to check on GEOS-5 flipped fields
!     -----------------------------------------------------
      allocate ( lon64(im), stat=ier )
        if (ier/=0) then
            if(verbose_) write(stderr,'(2a,i10,2a)') myname_, 'Alloc(lon) error '
            flush (stdout)
            stop 7
        endif

      call ncdinq (ncid, 1, dimName, dimSize, ier)
         if (ier/=0)then
             if(verbose_) write(stderr,'(2a)') myname_, 'Error(ncdinq)'
             flush (stderr); stop 7
         endif
      dimId = ncvid (ncid, dimName, ier)
         if (ier/=0)then
             if(verbose_) write(stderr,'(2a)') myname_, 'Error(ncvid)'
             flush (stderr); stop 7
         endif

!     Caution: will try to read as 64-bit first ...
!     ---------------------------------------------
      call ncvgt ( ncid, dimId, 1, im, lon64, ier )
           if ( ier .ne. 0 ) then
             if(verbose_) write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', 'lon'
             flush(stderr); stop 7
            endif

!     ... if absurd lons, read as 32-bit
!     ----------------------------------
      mnlon = minval(lon64)
      mxlon = maxval(lon64)
      if ( mnlon < -200.0 .or. mxlon > 400.0 ) then
         allocate ( lon32(im), stat=ier )
         call ncvgt ( ncid, dimId, 1, im, lon32, ier )
         lon64=lon32
         deallocate ( lon32 )
      endif

!     Is orientation of fields shifted by 180 degs from internal def
!     -------------------------------------------------------------
      fliplon = .false.
      if ( abs(lon64(1)+180.0)<rtol ) then
        fliplon = .true.  ! orientation of fields shifted by 180 degs from internal def
        print *, ' Flipping Fields Longitudinally ... '
      endif
      deallocate ( lon64 )

!     Now read upper-air fields
!     -------------------------
      if ( usrlevs ) then

        do iv = 1, nvars
          call Gfio_GetVar ( ncid, vname(iv), nymd, nhms,
     &                     im, jm, 1, mlev, fld(1,1,1,iv), ier )
             if ( ier .ne. 0 ) then
               if(verbose_) write(stderr,'(2a,i10,2a)') myname_,
     &                      ': Gfio_GetVar  ier = ', ier,
     &                      ' for variable ', trim(vname(iv))
               if ( present(stat) ) then
                    stat = 3 
                    return
               else
                    stop 7
               endif
             else
               call fix_undef_in_ ( n3dim, fld(:,:,:,iv:iv), vname(iv) )
             end if
             if( fliplon ) call hflip_ ( fld(1:im,1:jm,1:mlev,iv), im, jm, mlev )
             icount = icount + 1
        end do

      else

        allocate ( aux(im,jm,mlev_max,1), lev(mlev_max), stat=ier )
             if ( ier .ne. 0 ) then
                  if(verbose_) write(stderr,'(2a,i10)') myname_,
     .                         ': Alloc(aux), ier = ', ier
                  if ( present(stat) ) then
                       stat = 4
                       return
                  else
                       stop 7
                  endif
             end if
        call Gfio_GetVar ( ncid, 'Height', nymd, nhms,
     &                     mlev_max, 1, 0, 1, lev, ier )
           if ( ier .ne. 0 ) then
             if(verbose_) write(stderr,'(2a,i10)') myname_,
     &                    ': Gfio_GetVar (lev), ier = ', ier
             if ( present(stat) ) then
                  stat = 3 
                  return
             else
                  stop 7
             endif
           end if

        do iv = 1, nvars
           call Gfio_GetVar ( ncid, vname(iv), nymd, nhms,
     &                        im, jm, 1, mlev_max, aux, ier )
           if ( ier .ne. 0 ) then
             if(verbose_) write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_GetVar, ier = ', ier,
     &                    ' for variable ', trim(vname(iv))
             if ( present(stat) ) then
                  stat = 3 
                  return
             else
                  stop 7
             endif
           else
             call fix_undef_in_ ( im*jm*mlev_max, aux, vname(iv) )
           end if
           if( fliplon ) call hflip_ ( fld(1:im,1:jm,1:mlev_max,iv), im, jm, mlev_max )

           levcnt = 0
           fld(:,:,:,iv:iv) = undef
           if (present(plevs) ) then
               do k = 1, mlev
                  kloc = maxloc(lev(1:mlev_max), 
     .                   mask=abs(log(plevs(k))-log(lev(1:mlev_max))).lt.1e-5)
                  j    = kloc(1)
                  if (j>0) then
                     fld(:,:,k:k,iv:iv) = aux(:,:,j:j,1:1)
                     levcnt = levcnt + 1
                  end if
               end do
               if ( levcnt.ne.mlev ) then
                    if(verbose_) write(stderr,'(a,2(i10))') myname_,
     &                      'WARNING: Requested levs not found, levcnt = ',
     &                      levcnt, ' levs requested ', mlev
               end if
          else
               fld(:,:,1:mlev,iv:iv) = aux(:,:,1:mlev,1:1)
          endif  ! < present(plevs) >
          icount = icount + 1
        end do

        deallocate ( aux, lev, stat=ier )

      end if

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )
      if ( ier .ne. 0 ) then
           if(verbose_) write(stdout,'(2a,i7)') myname_,
     &                  ': WARNING, problems closing GFIO file, ier = ', ier
      end if

      if(verbose_) write(stdout,'(2a,i12,2a)') myname_,
     &            ':  read ', icount, ' variables from ', trim(gfiofile)
      if(verbose_) write(stdout,'(2a,i8.8,a,i6.6)') myname_, 
     &            ': nymd, nhms = ', nymd,' ', nhms


!     All done
!     --------
      return
      end subroutine GFIO_Get_3d_
!EOC

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Fix_Undef_In_ - Fixes undef values when input is 32 bit
! 
! !INTERFACE:
!
      subroutine Fix_Undef_In_ ( ndim, var, vname, verbose )
!
! !USES:

      use m_fpe, only: isnan

      implicit none


! !INPUT PARAMETERS: 
 
      integer  ndim
      character*(*) vname
      logical, intent(in), optional ::  verbose

! !INPUT/OUTPUT PARAMETERS: 
 
      real     var(ndim)

! !DESCRIPTION: 
!
! !REVISION HISTORY: 
!
!  13Apr00   Todling    - Initial code.
!  23Jun2010 Kokron     - eliminate previous calculation of val_max_in
!                         because it would fail if the input field
!                         contained NaNs
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'*'//'Fix_Undef_In_'

      integer  i
      integer  nonmiss, numnan
      real     tol
      real     val_max_in, val_max_out
      logical  verbose_

      tol   = 0.001 * undef

      verbose_ = verb
      if ( present(verbose) ) verbose_ = verbose

!     Make sure there is no precision problem with undef's
!     ----------------------------------------------------
      nonmiss = 0; numnan = 0.; val_max_in=0.
      do i= 1,ndim
         if (isnan(var(i))) then
            numnan=numnan+1
            var(i) = undef
         elseif ( abs(var(i)-undef) .lt. tol ) then
            nonmiss = nonmiss + 1
            var(i) = undef
         end if
      end do
      val_max_out = maxval(var)
      if (nonmiss.ne.0) then
         if(verbose_)
     &   write(stdout,'(2a,i10,3a)') myname_, ': Fixed ', nonmiss,
     .                             ' values from input with strange undef',
     .                             ' for variable ', trim(vname)
         flush(stdout)
         if ( val_max_out .ne. undef ) then
            write(stdout,*) ' Largest  value on  input: ',  val_max_in
            write(stdout,*) ' Largest  value on output: ',  val_max_out
            write(stdout,*) ' Undef        value spec.: ',  undef
            write(stdout,*) ' Correction not done. Aborting ... '
            stop 7
         end if
      end if
      if (numnan .ne. 0) then
         write(stdout,'(2a,i10,3a)') myname_, ': There were ',numnan,
     .                             ' NaN values from input',
     .                             ' for variable ', trim(vname)
         flush(stdout)
      endif

      return
      end subroutine Fix_Undef_In_

      subroutine hflip3_ ( q,im,jm,km )
      implicit none
      integer  im,jm,km,i,j,k
      real, intent(inout) :: q(im,jm,km)
      real, allocatable   :: dum(:)
      allocate ( dum(im) )
      do k=1,km
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j,k)
         dum(i+im/2) = q(i,j,k)
      enddo
         q(:,j,k) = dum(:)
      enddo
      enddo
      deallocate ( dum )
      end subroutine hflip3_
                                                                                                         
      subroutine hflip2_ ( q,im,jm )
      implicit none
      integer  im,jm,i,j
      real, intent(inout) :: q(im,jm)
      real, allocatable   :: dum(:)
      allocate ( dum(im) )
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j)
         dum(i+im/2) = q(i,j)
      enddo
         q(:,j) = dum(:)
      enddo
      deallocate ( dum )
      end subroutine hflip2_


      end module m_GFIO_GetFld
