      module  m_GFIO_PutFld

! !USES:

      implicit none

      PRIVATE
      PUBLIC GFIO_PutFld

      interface GFIO_PutFld; module procedure
     &          GFIO_Put_Fall_,  ! GEOS-3
     &          GFIO_Put_F2d_,   ! GEOS-3
     &          GFIO_Put_F2d2_,  ! GEOS-3
     &          GFIO_Put_F3d_,   ! GEOS-3
!    &          GFIO_Put_Fall4_, ! GEOS-4
!    &          GFIO_Put_F2d4_,  ! GEOS-4
     &          GFIO_Put_F3d4_   ! GEOS-4
      end interface

!  Internal Interfaces
!  -------------------
      interface Hflip_
        module procedure HFlip2_    ! Horizontal flip of 2d fields
        module procedure HFlip3_    ! Horizontal flip of 3d fields
      end interface

      character(len=*), parameter :: myname = 'GFIO_PutFld'

      integer, parameter :: stdout = 6
      integer, parameter :: stderr = 6
      integer, parameter :: iprec_def = 0  ! 32 bit file as default
      real   , parameter :: UNDEF  = 1.e15

      CONTAINS  

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Put_Fall_ --- Writes out GFIO 3d & 2d file
! 
! !INTERFACE:
!
      subroutine GFIO_Put_Fall_ ( title,  source,   gfio_bn,
     &                             nymd,    nhms,     ndalt,  levunits,
     &                             im,      jm,      mlev,
     &                            lat,     lon,       lev,
     &                        nvars2d, vname2d, vtitle2d, vunits2d, fld2d,
     &                        nvars3d, vname3d, vtitle3d, vunits3d, fld3d,
     &                        iprec, untag )

! !USES:


      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      character*(*) title          ! A title for the data set
      character*(*) source         ! Where it came from
      character*(*) gfio_bn

                                   ! Synoptic time:
      integer   nymd               !  Year-month-day, e.g.,  19971012
      integer   nhms               !  hour-minute-sec, e.g.,   120000

      integer   ndalt              ! Frequency of output/analysis
      character*(*) levunits       ! units of level dimension, e.g.,
                                   !   "hPa", "sigma_level"

                                   ! Grid Information
      integer   im                 !    number of points in the zonal direction 
      integer   jm                 !    number of points in the meridional 
                                   !    direction
      integer   mlev               !    number of points in the vertical

      real      lat(jm)
      real      lon(im)
      real      lev(mlev)
      
      integer   nvars2d
      integer   nvars3d
      character*(*) vname2d (nvars2d)
      character*(*) vunits2d(nvars2d)  
      character*(*) vtitle2d(nvars2d)
      character*(*) vname3d (nvars3d)
      character*(*) vunits3d(nvars3d)  
      character*(*) vtitle3d(nvars3d)

      real      fld2d(im,jm,nvars2d)      ! Surface fields
      real      fld3d(im,jm,mlev,nvars3d)   ! Upper-Air fields

      integer, intent(in), optional ::  iprec ! Precision of data 32(0)/64(1) bit
      logical, intent(in), optional ::  untag ! when .t. output on untagged file
!
! !DESCRIPTION: Writes out grided analysis fields using GFIO.
!
! !TO DO:  Discontinue support to pheonix anal.prs
!
! !REMARKS:
!    1. Cannot output pre-/post analysis files more frequently than 1 hr.
!
! !REVISION HISTORY: 
!
!  06Mar99   Todling   Addapted from History_Ana for sigA program.
!  22Oct99   Todling   Modified to output only increments.
!
!EOP
!-------------------------------------------------------------------------
!BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Put_Fall_'

      integer       ier, year, month, day, hour
      integer       nobs_oi, iv,  i, j, k, ia, ios

      integer       julday
      external      julday

      character*80    contact           ! Who to contact about the data set
      parameter     ( contact = 'data@gmao.gsfc.nasa.gov' )



!OUT  integer         vid(nvars2d+nvars3d)        ! variable handle
      character*80    vname(nvars2d+nvars3d)      ! variable short name, e.g., "hght"
      character*80    vtitle(nvars2d+nvars3d)     ! variable long name, e.g.,
                                        !   "Geopotential Height"
      character*80    vunits(nvars2d+nvars3d)     ! variable units, e.g., "meter/second"
      integer         kmvar(nvars2d+nvars3d)      ! number of levels for variable; it can
                                        !  either 0 (2-D fields) or equal to km

      real            valid_range(2,nvars2d+nvars3d)    !   range of variables
      real            packing_range(2,nvars2d+nvars3d)  !   16-bit compression range 

                                       ! Variable defined in existing GFIO file
      character*255 title_in           !   Title   in existing GFIO file
      character*255 source_in          !   Source  in existing GFIO file 
      character*255 contact_in         !   Contact in existing GFIO file
      real          undef_in

      integer       lm_max_max         ! Maximum number of time stamps in file

      integer       nvars
      integer       ngatts             ! number of attributes on file
      integer       kmvar_in(nvars2d+nvars3d)
!OUT  integer       vid_in(nvars2d+nvars3d)      ! variable handle
      character*80  vtitle_in(nvars2d+nvars3d)   ! variable long name in existing file
      character*257 vname_in(nvars2d+nvars3d)
      character*255 vunits_in(nvars2d+nvars3d)

      character*255 levunits_in
      integer       ndt_in
      integer       nvars_in

      integer       im_max, jm_max, mlev_max, lm_max
      integer       precision


!     Internal "memory" for GFIO output
!     ---------------------------------
      integer       last_nymd, last_nhms, ncid
      save          last_nymd, last_nhms, ncid
      character*255 gfiofile
      save          gfiofile

      data last_nymd        / -1 /
      data last_nhms        / -1 /
      data ncid             / -1 /


!     Define fields information properly
!     ----------------------------------
      nvars = nvars2d+nvars3d
      do iv = 1, nvars2d
         vname (iv) = trim( vname2d(iv))
         vtitle(iv) = trim(vtitle2d(iv))
         vunits(iv) = trim(vunits2d(iv))
         kmvar (iv) = 0
      end do
      do iv = 1, nvars3d
         vname (iv+nvars2d) = trim( vname3d(iv))
         vtitle(iv+nvars2d) = trim(vtitle3d(iv))
         vunits(iv+nvars2d) = trim(vunits3d(iv))
         kmvar (iv+nvars2d) = mlev
      end do


!     Define constants
!     ----------------
      do i = 1, 2
      do j = 1, nvars
        valid_range(i,j)   = undef    !  TO DO: set to no packing for now
        packing_range(i,j) = undef    !  TO DO: set to no packing for now
      end do
      end do


!                        -----------     
!                        GFIO Output
!                        -----------     

!     Dates, etc
!     ----------
      year  = int(nymd/10000)
      if ( year .lt. 1900 ) year = 1900 + year 
      month = int((nymd-year*10000)/100)
      day   = int(nymd-year*10000-month*100)
      hour  = nhms / 10000

!     The very first time in this routine for the present run
!     check for the existence of previous Gfio files corres-
!     ponding to the current date
!     -------------------------------------------------------
      if ( (ndalt/10000) .lt. 1 ) then
         write(stdout,'(3a)') myname_,
     .                ': Program cannot handle pre-/post- analysis ',
     .                ' output more frequently than 1 hour '
         return
      end if
      lm_max_max = max(1,24/(ndalt/10000))
!_OUT allocate ( nymd_in(lm_max_max), nhms_in(lm_max_max), stat=ier )
!_OUT if ( ier .ne. 0 ) then
!_OUT     write(stderr,'(2a,i5)') myname_,
!_OUT.          ': allocate(lm) error, stat =', ier
!_OUT     call exit(7)
!_OUT endif

!     Open GFIO file
!     --------------
      if ( present(untag) ) then
        if ( untag ) then
             gfiofile = trim(gfio_bn) 
        else
             write(gfiofile,'(i8.8)',iostat=ios) nymd
             gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
        end if
      else
        write(gfiofile,'(i8.8)',iostat=ios) nymd
        gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
      end if
      im_max   = im
      jm_max   = jm
      mlev_max = mlev
      lm_max   = lm_max_max

      call GFIO_Open ( gfiofile, 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( last_nymd .eq. -1 ) then
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file opened from previous run'
         else
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file re-opened '
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed longitudinal dim'
            call exit(7)
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed latitudinal dim'
            call exit(7)
         endif

!_OUT    if (lm_max .gt. lm_max_max) then
!_OUT       write(stderr,'(2a,i5)') myname_,
!_OUT&           ': trying to write unallowed number of times'
!_OUT       call exit(7)
!_OUT    endif

         if (mlev_max .ne. mlev) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed number of levels'
            call exit(7)
         endif


      else

         write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         call flush (stdout)

         precision = iprec_def
         if ( present(iprec) ) precision = iprec

!        create a new GFIO file when file doesn't exist
!        ----------------------------------------------
         call Gfio_Create ( gfiofile, 
     &                      trim(title), trim(source), trim(contact), undef,
     &                      im, jm, mlev, lon, lat, lev, levunits, 
     &                      nymd, nhms, ndalt,
     &                      nvars, vname, vtitle, vunits, kmvar,
     &                      valid_range, packing_range, precision,
     &                      ncid, ier )

         if ( ier .ne. 0 ) then
           write(stderr,'(2a,i10)') myname_,  
     &                  ': Gfio_Create error, ier = ', ier
           call exit(7)
         end if
         print *, 'History_Ana: created Gfio file ', trim(gfiofile)

      endif


!     OK, let's write the data to file
!     ================================

!     First write surface fields
!     --------------------------
      do iv = 1, nvars2d
      call Gfio_PutVar ( ncid, vname2d(iv), nymd, nhms,
     &                   im, jm, 0, 1, fld2d(1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname2d(iv))
             call exit(7)
           end if
      end do

!     Now write upper-air fields
!     --------------------------
      do iv = 1, nvars3d
      call Gfio_PutVar ( ncid, vname3d(iv), nymd, nhms,
     &                   im, jm, 1, mlev, fld3d(1,1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname3d(iv))
             call exit(7)
           end if
      end do

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  wrote ', nvars, ' variables to ', trim(gfiofile)


!_OUT deallocate ( nymd_in, nhms_in )

      last_nymd = nymd
      last_nhms = nhms

!     All done
!     --------
      return
      end subroutine GFIO_Put_Fall_
!EOC

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Put_F2d_ --- Writes out GFIO 2d file
! 
! !INTERFACE:
!
      subroutine GFIO_Put_F2d_ ( title,  source,   gfio_bn,
     &                             nymd,    nhms,     ndalt,  levunits,
     &                             im,      jm,      mlev,
     &                            lat,     lon,       lev,
     &                        nvars, vname, vtitle, vunits, fld,
     &                        iprec, untag )

! !USES:


      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      character*(*) title          ! A title for the data set
      character*(*) source         ! Where it came from
      character*(*) gfio_bn

                                   ! Synoptic time:
      integer   nymd               !  Year-month-day, e.g.,  19971012
      integer   nhms               !  hour-minute-sec, e.g.,   120000

      integer   ndalt              ! Frequency of output/analysis
      character*(*) levunits       ! units of level dimension, e.g.,
                                   !   "hPa", "sigma_level"

                                   ! Grid Information
      integer   im                 !    number of points in the zonal direction 
      integer   jm                 !    number of points in the meridional 
                                   !    direction
      integer   mlev               !    number of points in the vertical

      real      lat(jm)
      real      lon(im)
      real      lev(mlev)
      
      integer   nvars
      character*(*) vname (nvars)
      character*(*) vunits(nvars)  
      character*(*) vtitle(nvars)

      real      fld(im,jm,nvars)      ! Surface fields

      integer, intent(in), optional ::  iprec ! Precision of data 32(0)/64(1) bit
      logical, intent(in), optional ::  untag ! when .t. output on untagged file
!
! !DESCRIPTION: Writes out grided analysis fields using GFIO.
!
! !TO DO:  Discontinue support to pheonix anal.prs
!
! !REMARKS:
!    1. Cannot output pre-/post analysis files more frequently than 1 hr.
!
! !REVISION HISTORY: 
!
!  06Mar99   Todling   Addapted from History_Ana for sigA program.
!  22Oct99   Todling   Modified to output only increments.
!
!EOP
!-------------------------------------------------------------------------
!BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Put_F2d_'

      integer       ier, year, month, day, hour
      integer       nobs_oi, iv,  i, j, k, ia, ios

      integer       julday
      external      julday

      character*80    contact           ! Who to contact about the data set
      parameter     ( contact = 'data@gmao.gsfc.nasa.gov' )

      integer         kmvar(nvars)      ! number of levels for variable; it can
                                        !  either 0 (2-D fields) or equal to km

      real            valid_range(2,nvars)    !   range of variables
      real            packing_range(2,nvars)  !   16-bit compression range 

                                       ! Variable defined in existing GFIO file
      character*255 title_in           !   Title   in existing GFIO file
      character*255 source_in          !   Source  in existing GFIO file 
      character*255 contact_in         !   Contact in existing GFIO file
      real          undef_in

      integer       lm_max_max         ! Maximum number of time stamps in file
!_OUT integer, allocatable :: nymd_in(:)
!_OUT integer, allocatable :: nhms_in(:) 

      integer       ngatts             ! number of attributes on file
      integer       kmvar_in(nvars)
      character*80  vtitle_in(nvars)   ! variable long name in existing file
      character*257 vname_in(nvars)
      character*255 vunits_in(nvars)

      character*255 levunits_in
      integer       ndt_in
      integer       nvars_in

      integer       im_max, jm_max, mlev_max, lm_max
      integer       precision


!     Internal "memory" for GFIO output
!     ---------------------------------
      integer       last_nymd, last_nhms, ncid
      save          last_nymd, last_nhms, ncid
      character*255 gfiofile
      save          gfiofile

      data last_nymd        / -1 /
      data last_nhms        / -1 /
      data ncid             / -1 /


!     Define fields information properly
!     ----------------------------------
      do iv = 1, nvars
         kmvar (iv) = 0
      end do

!     Define constants
!     ----------------
      do i = 1, 2
      do j = 1, nvars
        valid_range(i,j)   = undef    !  TO DO: set to no packing for now
        packing_range(i,j) = undef    !  TO DO: set to no packing for now
      end do
      end do


!                        -----------     
!                        GFIO Output
!                        -----------     

!     Dates, etc
!     ----------
      year  = int(nymd/10000)
      if ( year .lt. 1900 ) year = 1900 + year 
      month = int((nymd-year*10000)/100)
      day   = int(nymd-year*10000-month*100)
      hour  = nhms / 10000

!     The very first time in this routine for the present run
!     check for the existence of previous Gfio files corres-
!     ponding to the current date
!     -------------------------------------------------------
      if ( (ndalt/10000) .lt. 1 ) then
         write(stdout,'(3a)') myname_,
     .                ': Program cannot handle pre-/post- analysis ',
     .                ' output more frequently than 1 hour '
         return
      end if
      lm_max_max = max(1,24/(ndalt/10000))
!_OUT allocate ( nymd_in(lm_max_max), nhms_in(lm_max_max), stat=ier )
!_OUT if ( ier .ne. 0 ) then
!_OUT     write(stderr,'(2a,i5)') myname_,
!_OUT.          ': allocate(lm) error, stat =', ier
!_OUT     call exit(7)
!_OUT endif

!     Open GFIO file
!     --------------
      if ( present(untag) ) then
        if ( untag ) then
             gfiofile = trim(gfio_bn)
        else
             write(gfiofile,'(i8.8)',iostat=ios) nymd
             gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
        end if
      else
        write(gfiofile,'(i8.8)',iostat=ios) nymd
        gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
      end if
      im_max   = im
      jm_max   = jm
      mlev_max = mlev
      lm_max   = lm_max_max

      call GFIO_Open ( gfiofile, 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( last_nymd .eq. -1 ) then
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file opened from previous run'
         else
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file re-opened '
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed longitudinal dim'
            call exit(7)
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed latitudinal dim'
            call exit(7)
         endif

!_OUT    if (lm_max .gt. lm_max_max) then
!_OUT       write(stderr,'(2a,i5)') myname_,
!_OUT&           ': trying to write unallowed number of times'
!_OUT       call exit(7)
!_OUT    endif

         if (mlev_max .ne. mlev) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed number of levels'
            call exit(7)
         endif


      else

         write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         call flush (stdout)

         precision = iprec_def
         if ( present(iprec) ) precision = iprec

!        create a new GFIO file when file doesn't exist
!        ----------------------------------------------
         call Gfio_Create ( gfiofile, 
     &                      trim(title), trim(source), trim(contact), undef,
     &                      im, jm, mlev, lon, lat, lev, levunits, 
     &                      nymd, nhms, ndalt,
     &                      nvars, vname, vtitle, vunits, kmvar,
     &                      valid_range, packing_range, precision,
     &                      ncid, ier )

         if ( ier .ne. 0 ) then
           write(stderr,'(2a,i10)') myname_,  
     &                  ': Gfio_Create error, ier = ', ier
           call exit(7)
         end if
         print *, 'History_Ana: created Gfio file ', trim(gfiofile)

      endif


!     First write surface fields
!     --------------------------
      do iv = 1, nvars
      call Gfio_PutVar ( ncid, vname(iv), nymd, nhms,
     &                   im, jm, 0, 1, fld(1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname(iv))
             call exit(7)
           end if
      end do

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  wrote ', nvars, ' variables to ', trim(gfiofile)


      last_nymd = nymd
      last_nhms = nhms

!     All done
!     --------
      return
      end subroutine GFIO_Put_F2d_
!EOC

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Put_F2d2_ --- Writes out GFIO 2d file
! 
! !INTERFACE:
!
      subroutine GFIO_Put_F2d2_ ( gfiofile, nymd, nhms, im, jm,
     &                            nvars, vname, fld )

! !USES:


      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      character*(*) gfiofile

                                   ! Synoptic time:
      integer   nymd               !  Year-month-day, e.g.,  19971012
      integer   nhms               !  hour-minute-sec, e.g.,   120000

      integer   ndalt              ! Frequency of output/analysis

                                   ! Grid Information
      integer   im                 !    number of points in the zonal direction 
      integer   jm                 !    number of points in the meridional 
                                   !    direction
      real      lat(jm)
      real      lon(im)
      
      integer   nvars
      character*(*) vname (nvars)

      real      fld(im,jm,nvars)      ! Surface fields

! !DESCRIPTION: Writes out 2d grided fields using GFIO.
!
! !REVISION HISTORY: 
!
!  06Mar99   Todling   Addapted from History_Ana for sigA program.
!  22Oct99   Todling   Modified to output only increments.
!  11Jul06   Todling   Simplified further from original F2d
!
!EOP
!-------------------------------------------------------------------------
!BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Put_F2d2_'

      integer       ier, year, month, day, hour
      integer       iv,  i, j, k, ia, ios

      integer       julday
      external      julday

      character*80    contact           ! Who to contact about the data set
      parameter     ( contact = 'data@gmao.gsfc.nasa.gov' )

                                       ! Variable defined in existing GFIO file
      integer       precision


!     Internal "memory" for GFIO output
!     ---------------------------------
      integer  ncid
      save     ncid
      data     ncid / -1 /

!     Open GFIO file
!     --------------
      call GFIO_Open ( trim(gfiofile), 0, ncid, ier )

!     First write surface fields
!     --------------------------
      do iv = 1, nvars
         call Gfio_PutVar ( ncid, vname(iv), nymd, nhms,
     &                      im, jm, 0, 1, fld(1,1,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname(iv))
             call exit(7)
           end if
      end do

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  wrote ', nvars, ' variables to ', trim(gfiofile)

!     All done
!     --------
      return
      end subroutine GFIO_Put_F2d2_
!EOC
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Put_F3d_ --- Writes out GFIO 3d file
! 
! !INTERFACE:
!
      subroutine GFIO_Put_F3d_ ( title,  source,   gfio_bn,
     &                             nymd,    nhms,     ndalt,  levunits,
     &                             im,      jm,      mlev,
     &                            lat,     lon,       lev,
     &                        nvars, vname, vtitle, vunits, fld,
     &                        iprec, untag )

! !USES:


      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      character*(*) title          ! A title for the data set
      character*(*) source         ! Where it came from
      character*(*) gfio_bn

                                   ! Synoptic time:
      integer   nymd               !  Year-month-day, e.g.,  19971012
      integer   nhms               !  hour-minute-sec, e.g.,   120000

      integer   ndalt              ! Frequency of output/analysis
      character*(*) levunits       ! units of level dimension, e.g.,
                                   !   "hPa", "sigma_level"

                                   ! Grid Information
      integer   im                 !    number of points in the zonal direction 
      integer   jm                 !    number of points in the meridional 
                                   !    direction
      integer   mlev               !    number of points in the vertical

      real      lat(jm)
      real      lon(im)
      real      lev(mlev)
      
      integer   nvars
      character*(*) vname (nvars)
      character*(*) vunits(nvars)  
      character*(*) vtitle(nvars)

      real      fld(im,jm,mlev,nvars)   ! Upper-Air fields

      integer, intent(in), optional ::  iprec ! Precision of data 32(0)/64(1) bit
      logical, intent(in), optional ::  untag ! when .t. output on untagged file
!
! !DESCRIPTION: Writes out grided analysis fields using GFIO.
!
! !TO DO:  Discontinue support to pheonix anal.prs
!
! !REMARKS:
!    1. Cannot output pre-/post analysis files more frequently than 1 hr.
!
! !REVISION HISTORY: 
!
!  06Mar99   Todling   Addapted from History_Ana for sigA program.
!  22Oct99   Todling   Modified to output only increments.
!  10Dec02   SC/EM     Minor modification for Halem
!
!EOP
!-------------------------------------------------------------------------
!BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Put_F3d_'

      integer       ier, year, month, day, hour
      integer       nobs_oi, iv,  i, j, k, ia, ios

      integer       julday
      external      julday

      character*80    contact           ! Who to contact about the data set
      parameter     ( contact = 'data@gmao.gsfc.nasa.gov' )



      integer         kmvar(nvars)      ! number of levels for variable; it can
                                        !  either 0 (2-D fields) or equal to km

      real            valid_range(2,nvars)    !   range of variables
      real            packing_range(2,nvars)  !   16-bit compression range 

                                       ! Variable defined in existing GFIO file
      character*255 title_in           !   Title   in existing GFIO file
      character*255 source_in          !   Source  in existing GFIO file 
      character*255 contact_in         !   Contact in existing GFIO file
      real          undef_in

      integer       lm_max_max         ! Maximum number of time stamps in file
!_OUT integer, allocatable :: nymd_in(:)
!_OUT integer, allocatable :: nhms_in(:) 

      integer       ngatts             ! number of attributes on file
      integer       kmvar_in(nvars)
!OUT  integer       vid_in(nvars)      ! variable handle
      character*80  vtitle_in(nvars)   ! variable long name in existing file
      character*257 vname_in(nvars)
      character*255 vunits_in(nvars)

      character*255 levunits_in
      integer       ndt_in
      integer       nvars_in

      integer       im_max, jm_max, mlev_max, lm_max
      integer       precision


!     Internal "memory" for GFIO output
!     ---------------------------------
      integer       last_nymd, last_nhms, ncid
      save          last_nymd, last_nhms, ncid
      character*255 gfiofile
      save          gfiofile

      data last_nymd        / -1 /
      data last_nhms        / -1 /
      data ncid             / -1 /


!     Define fields information properly
!     ----------------------------------
      do iv = 1, nvars
         kmvar (iv) = mlev
      end do


!     Define constants
!     ----------------
      do i = 1, 2
      do j = 1, nvars
        valid_range(i,j)   = undef    !  TO DO: set to no packing for now
        packing_range(i,j) = undef    !  TO DO: set to no packing for now
      end do
      end do


!                        -----------     
!                        GFIO Output
!                        -----------     

!     Dates, etc
!     ----------
      year  = int(nymd/10000)
      if ( year .lt. 1900 ) year = 1900 + year 
      month = int((nymd-year*10000)/100)
      day   = int(nymd-year*10000-month*100)
      hour  = nhms / 10000

!     The very first time in this routine for the present run
!     check for the existence of previous Gfio files corres-
!     ponding to the current date
!     -------------------------------------------------------
      if ( (ndalt/10000) .lt. 1 ) then
         write(stdout,'(3a)') myname_,
     .                ': Program cannot handle pre-/post- analysis ',
     .                ' output more frequently than 1 hour '
         return
      end if
      lm_max_max = max(1,24/(ndalt/10000))
!_OUT allocate ( nymd_in(lm_max_max), nhms_in(lm_max_max), stat=ier )
!_OUT if ( ier .ne. 0 ) then
!_OUT     write(stderr,'(2a,i5)') myname_,
!_OUT.          ': allocate(lm) error, stat =', ier
!_OUT     call exit(7)
!_OUT endif

!     Open GFIO file
!     --------------
      if ( present(untag) ) then
        if ( untag ) then
             gfiofile = trim(gfio_bn)
        else
             write(gfiofile,'(i8.8)',iostat=ios) nymd
             gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
        end if
      else
        write(gfiofile,'(i8.8)',iostat=ios) nymd
        gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
      end if
      im_max   = im
      jm_max   = jm
      mlev_max = mlev
      lm_max   = lm_max_max

      call GFIO_Open ( gfiofile, 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( last_nymd .eq. -1 ) then
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file opened from previous run'
         else
             write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file re-opened '
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed longitudinal dim'
            call exit(7)
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed latitudinal dim'
            call exit(7)
         endif

!_OUT    if (lm_max .gt. lm_max_max) then
!_OUT       write(stderr,'(2a,i5)') myname_,
!_OUT&           ': trying to write unallowed number of times'
!_OUT       call exit(7)
!_OUT    endif

         if (mlev_max .ne. mlev) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed number of levels'
            call exit(7)
         endif


      else

         write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         call flush (stdout)

         precision = iprec_def
         if ( present(iprec) ) precision = iprec

!        create a new GFIO file when file doesn't exist
!        ----------------------------------------------
         call Gfio_Create ( gfiofile, 
     &                      trim(title), trim(source), trim(contact), undef,
     &                      im, jm, mlev, lon, lat, lev, levunits, 
     &                      nymd, nhms, ndalt,
     &                      nvars, vname, vtitle, vunits, kmvar,
     &                      valid_range, packing_range, precision,
     &                      ncid, ier )

         if ( ier .ne. 0 ) then
           write(stderr,'(2a,i10)') myname_,  
     &                  ': Gfio_Create error, ier = ', ier
           call exit(7)
         end if
         print *, 'History_Ana: created Gfio file ', trim(gfiofile)

      endif

!     Now write upper-air fields
!     --------------------------
      do iv = 1, nvars
         call Gfio_PutVar ( ncid, vname(iv), nymd, nhms,
     &                      im, jm, 1, mlev, fld(1:im,1:jm,1:mlev,iv), ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname(iv))
             call exit(7)
           end if
      end do


!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  wrote ', nvars, ' variables to ', trim(gfiofile)


      last_nymd = nymd
      last_nhms = nhms

!     All done
!     --------
      return
      end subroutine GFIO_Put_F3d_
!EOC

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Put_F3d4_ --- Writes out GFIO 3d file
! 
! !INTERFACE:
!
      subroutine GFIO_Put_F3d4_ ( title,  source,  gfio_bn,
     &                             nymd,   nhms,     ndalt,
     &                             im,       jm,        km,  
     &                             ptop,     ks,        ak,  bk,
     &                        nvars, vname, vtitle, vunits, fld, rc,
     &                        plevs, iprec, untag, verbose, forceflip )

! !USES:


      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      character*(*) title          ! A title for the data set
      character*(*) source         ! Where it came from
      character*(*) gfio_bn

                                   ! Synoptic time:
      integer   nymd               !  Year-month-day, e.g.,  19971012
      integer   nhms               !  hour-minute-sec, e.g.,   120000

      integer   ndalt              ! Frequency of output/analysis

                                   ! Grid Information
      integer   im                 !    number of points in the zonal direction 
      integer   jm                 !    number of points in the meridional 
                                   !    direction
      integer   km                 !    number of points in the vertical

      real, intent(in), OPTIONAL    :: plevs(:) ! pressure levels (hPa) for
                                                ! PRS files. If this paramter
                                                ! is specified the output
                                                ! file is assumed to be PRS.

      real,    intent(in)  :: ptop     ! top pressure (Pa)
      integer, intent(in)  :: ks       ! no. of pure pressure levels
      real,    intent(in)  :: ak(km+1) ! vertical grid a coefficient
      real,    intent(in)  :: bk(km+1) ! vertical grid b coefficient
      
      integer   nvars
      character*(*) vname (nvars)
      character*(*) vunits(nvars)  
      character*(*) vtitle(nvars)

      real      fld(im,jm,km,nvars)   ! Upper-Air fields

      integer, intent(in), optional ::  iprec ! Precision of data 32(0)/64(1) bit
      logical, intent(in), optional ::  verbose ! controls writeout
      logical, intent(in), optional ::  untag ! when .t. output on untagged file
      logical, intent(in), optional ::  forceflip ! when .t. will flip lons

      integer, INTENT (out) :: rc
!
! !DESCRIPTION: Writes out grided analysis fields using GFIO.
!
! !TO DO:  Discontinue support to pheonix anal.prs
!
! !REMARKS:
!    1. Cannot output pre-/post analysis files more frequently than 1 hr.
!
! !REVISION HISTORY: 
!
!  06Mar99   Todling   Addapted from History_Ana for sigA program.
!  22Oct99   Todling   Modified to output only increments.
!  28Oct03   Todling   Adapted to handle GEOS-4 fields.
!  25Sep07   Todling   Force flip lon; always writes GEOS-4 convention
!
!EOP
!-------------------------------------------------------------------------
!BOC

      character(len=*), parameter :: myname_ = myname//'*'//'GFIO_Put_F3d4_'

      integer       ier, year, month, day, hour
      integer       nobs_oi, iv,  i, j, k, ia, ios

      integer       julday
      external      julday

      character*80    contact           ! Who to contact about the data set
      parameter     ( contact = 'data@gmao.gsfc.nasa.gov' )

      character(len=255) levunits       ! units of level dimension, e.g.,
                                        !   "hPa", "sigma_level"

      integer         kmvar(nvars)      ! number of levels for variable; it can
                                        !  either 0 (2-D fields) or equal to km

      real            valid_range(2,nvars)    !   range of variables
      real            packing_range(2,nvars)  !   16-bit compression range 

                                       ! Variable defined in existing GFIO file
      character*255 title_in           !   Title   in existing GFIO file
      character*255 source_in          !   Source  in existing GFIO file 
      character*255 contact_in         !   Contact in existing GFIO file
      real          undef_in

      integer       lm_max_max         ! Maximum number of time stamps in file

      integer       ngatts             ! number of attributes on file
      integer       kmvar_in(nvars)
      character*80  vtitle_in(nvars)   ! variable long name in existing file
      character*257 vname_in(nvars)
      character*255 vunits_in(nvars)

      integer       ndt_in
      integer       nvars_in

      logical       sayit
      integer       im_max, jm_max, mlev_max, lm_max
      integer       precision

      real          lat_min, lat_max, lat_del 
      real          lon_min, lon_max, lon_del 
      real          p
      logical       fliplon

      real, allocatable :: lat(:), lon(:), lev(:)
      real, allocatable :: aux(:,:,:)

!     Internal "memory" for GFIO output
!     ---------------------------------
      integer       last_nymd, last_nhms, ncid
      save          last_nymd, last_nhms, ncid
      character*255 gfiofile
      save          gfiofile

      data last_nymd        / -1 /
      data last_nhms        / -1 /
      data ncid             / -1 /

!     Reference pressure thickness assuming ps ~ 984 hPa
!     ---------------------------------------------------
      real dpref
      dpref(k) = ( ak(k+1) - ak(k) ) +
     .           ( bk(k+1) - bk(k) ) * 98400.


!     Define fields information properly
!     ----------------------------------
      do iv = 1, nvars
         kmvar (iv) = km
      end do


!     Define constants
!     ----------------
      do i = 1, 2
      do j = 1, nvars
        valid_range(i,j)   = undef    !  TO DO: set to no packing for now
        packing_range(i,j) = undef    !  TO DO: set to no packing for now
      end do
      end do

!     Allocate local work space
!     -------------------------
      rc = 0
      call init_ ( ier )
      if ( ier .ne. 0 ) then
           call clean_()
           rc = 1
           return
      end if

      fliplon = .false.
      if (present(forceflip)) then
          if(forceflip) then 
             fliplon = .true.
             print *, trim(myname_), ': Will Flip Fields Longitudinally'
          endif
      endif

!     Create coordinate variables
!     ---------------------------
      lat_min = -90.0
      lat_max = +90.0
      lat_del = ( lat_max - lat_min ) / ( jm-1)
      do j = 1, jm
         lat(j) = lat_min + (j-1) * lat_del
      end do
      if ( fliplon ) then
          lon_min = -180.0
      else
          lon_min = 0.0
      endif
      lon_del = 360.0 / im 
      do i = 1, im
         lon(i) = lon_min + (i-1) * lon_del
      end do

!     Vertical coordinates: given on input (prs files)
!     ------------------------------------------------
      if ( present(plevs) ) then

          lev(1:km) = plevs(1:km)

!     Vertical coordinates: fake something for GrADS sake
!     ---------------------------------------------------
      else

        p = ptop + 0.5 * dpref(1)
        lev(1) = p
        do k = 2, km
           p = p + 0.5 * ( dpref(k-1) + dpref(k) )
           lev(k) = p
        end do
        lev(1:km) = lev(1:km) / 100.

      endif
      levunits = 'hPa'

      sayit = .false.
      if (present(verbose)) then
          if(verbose) sayit=verbose
      endif

!                        -----------     
!                        GFIO Output
!                        -----------     

!     Dates, etc
!     ----------
      year  = int(nymd/10000)
      if ( year .lt. 1900 ) year = 1900 + year 
      month = int((nymd-year*10000)/100)
      day   = int(nymd-year*10000-month*100)
      hour  = nhms / 10000

!     Time attributes
!     ---------------
!_OUT if ( ndalt >= 010000 ) then
!_OUT      lm_max_max = max(1,24/(ndalt/10000))
!_OUT else if ( ndalt >= 000100 .and. ndalt < 010000 ) then
!_OUT      lm_max_max = max(1,24*60/(ndalt/100))
!_OUT else
!_OUT    write(stderr,'(3a)') myname_,
!_OUT.                ': Program cannot handle GFIO file ',
!_OUT.                ' output more frequently than 1 min '
!_OUT    return
!_OUT endif
      
!     Open GFIO file
!     --------------
      if ( present(untag) ) then
        if ( untag ) then
             gfiofile = trim(gfio_bn)
        else
             write(gfiofile,'(i8.8)',iostat=ios) nymd
             gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
        end if
      else
        write(gfiofile,'(i8.8)',iostat=ios) nymd
        gfiofile = trim(gfio_bn) // '.prs.t' // trim(gfiofile)
      end if
      im_max   = im
      jm_max   = jm
      mlev_max = km

      call GFIO_Open ( gfiofile, 0, ncid, ier )

      if ( ier .eq. 0 ) then

         call GFIO_DimInquire ( ncid, im_max, jm_max, mlev_max, lm_max,
     &                          nvars_in, ngatts, ier)

         if ( last_nymd .eq. -1 ) then
             if(sayit)
     &       write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file opened from previous run'
         else
             if(sayit)
     &       write(stdout,'(2a)') myname_,
     &                    ': Gfio_Open file re-opened '
         end if

!        Check consistency of Gfio file
!        ------------------------------
         if (im_max .ne. im) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed longitudinal dim'
            call exit(7)
         endif

         if (jm_max .ne. jm) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed latitudinal dim'
            call exit(7)
         endif

         if (mlev_max .ne. km) then
            write(stderr,'(2a,i5)') myname_,
     &           ': trying to write unallowed number of levels'
            call exit(7)
         endif


      else

         if(sayit)
     .   write(stdout,'(2a)') myname_,
     .                ': No GFIO file for current date '
         call flush (stdout)

         precision = iprec_def
         if ( present(iprec) ) precision = iprec

!        create a new GFIO file when file doesn't exist
!        ----------------------------------------------
         call Gfio_Create ( gfiofile, 
     &                      trim(title), trim(source), trim(contact), undef,
     &                      im, jm, km, lon, lat, lev, levunits, 
     &                      nymd, nhms, ndalt,
     &                      nvars, vname, vtitle, vunits, kmvar,
     &                      valid_range, packing_range, precision,
     &                      ncid, ier )

         if ( ier .ne. 0 ) then
           write(stderr,'(2a,i10)') myname_,  
     &                  ': Gfio_Create error, ier = ', ier
           call exit(7)
         end if
         print *, myname_, ': Created Gfio file ', trim(gfiofile)

      endif


!     Now write upper-air fields
!     --------------------------
      allocate(aux(im,jm,km))
      do iv = 1, nvars
         aux(:,:,:) = fld(1:im,1:jm,1:km,iv)
         if( fliplon ) call hflip_ ( aux, im, jm, km )
         call Gfio_PutVar ( ncid, vname(iv), nymd, nhms,
     &                      im, jm, 1, km, aux, ier )
           if ( ier .ne. 0 ) then
             write(stderr,'(2a,i10,2a)') myname_,
     &                    ': Gfio_PutVar, ier = ', ier,
     &                    ' for variable ', trim(vname(iv))
             call exit(7)
           end if
      end do
      deallocate(aux)

!     Close GFIO file now
!     -------------------
      call Gfio_Close ( ncid, ier )

      write(stdout,'(2a,i12,2a)') myname_,
     &            ':  wrote ', nvars, ' variables to ', trim(gfiofile)
      write(stdout,'(2a,i8.8,a,i6.6)') myname_, 
     &            ': nymd, nhms = ', nymd,' ', nhms


      last_nymd = nymd
      last_nhms = nhms

!     Clean up
!     --------
      call clean_()

!     All done
!     --------
      return

      CONTAINS

      subroutine init_ ( err )       ! allocates local memory
      integer err
      allocate ( lat(jm), lon(im), lev(km),  stat=err )
      end subroutine init_

      subroutine clean_()             ! de-allocates local memory
      deallocate ( lat, lon, lev )
      end subroutine clean_

      end subroutine GFIO_Put_F3d4_
!EOC

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
      end module m_GFIO_PutFld
