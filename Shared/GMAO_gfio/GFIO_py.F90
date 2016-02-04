!
! Simple wrappers for Python interface
!

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Create -- Creates a DAO gridded file for writing
! 
! !DESCRIPTION: This routine is used to open a new file for a GFIO stream.
!               Packing is not yet supported.  Information about each opened
!               stream is stored in a COMMON block contained in gfio.h.  
!               This information is later used by GFIO\_PutVar.  GFIO\_Open
!               should be used to open an existing file for reading or writing.
!
! !INTERFACE:
!
      subroutine gfioCreate ( fname, title, source, contact, amiss, &
                              im, jm, km, lon, lat, levs, levunits, &
                              yyyymmdd_beg, hhmmss_beg, timinc,     &
                              nvars, vname, vtitle, vunits, kmvar,  &
                              valid_range, packing_range, prec,     &
                              fid, rc )                             
!
! !USES:
!
      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS: 
!
                                                   ! ------- Global Metadata ------
      character(len=*), intent(in) :: fname        ! File name
      character(len=*), intent(in) :: title        ! A title for the data set
      character(len=*), intent(in) :: source       ! Source of data, e.g. NASA/DAO
      character(len=*), intent(in) :: contact      ! Who to contact about the data set, e.g.,
                                                   !   'Contact data@gmao.gsfc.nasa.gov'
      real, intent(in) ::            amiss         ! Missing value such as 1.0E15

                                                   ! ------- Dimension Metadata -------
      integer, intent(in) ::         im            ! size of longitudinal dimension
      integer, intent(in) ::         jm            ! size of latitudinal  dimension
      integer, intent(in) ::         km            ! size of vertical     dimension 
                                                   !   (surface only=1)
      real, intent(in) ::            lon(im)       ! longitude of center of gridbox in 
                                                   !   degrees east of Greenwich (can be 
                                                   !   -180 -> 180 or 0 -> 360)
      real, intent(in) ::            lat(jm)       ! latitude of center of gridbox in 
                                                   !   degrees north of equator
      real, intent(in) ::            levs(km)      ! Level (units given by levunits) of
                                                   !   center of gridbox
      character(len=*), intent(in) ::   levunits   ! units of level dimension, e.g.,
                                                   !   "millibar", "hPa", or "sigma_level"
      integer, intent(in) ::        yyyymmdd_beg   ! First year-month-day to be written 
      integer, intent(in) ::          hhmmss_beg   ! First hour-minute-second to be written
      integer, intent(in) ::         timinc        ! Increment between output times (HHMMSS)

                                                   ! ------- Variable Metadata -------
      integer, intent(in) ::         nvars         ! number of variables in file
      character(len=8192), intent(in) ::   vname   ! variable short name, e.g., "hght"
      character(len=8192), intent(in) ::   vtitle  ! variable long name, e.g.,
                                                   !   "Geopotential Height"
      character(len=8192), intent(in) ::   vunits  ! variable units, e.g., "meter/second"
      integer, intent(in) ::         kmvar(nvars)  ! number of levels for variable; it can
                                                   !   either be 0 (2-D fields) or equal to km

      real, intent(in) ::    valid_range(2,nvars)  ! Variable valid range; GFIO_PutVar
                                                   ! will return a non-fatal error if a value is 
                                                   ! outside of this range. IMPORTANT: If packing
                                                   ! is not desired for a given variable, YOU MUST
                                                   ! set both components of valid_range to amiss.

                                                   ! ------ Packing Metadata ----
      real, intent(in) ::   packing_range(2,nvars) ! Packing range to be used for 16-bit packing 
                                                   ! of each variable. IMPORTANT: If packing is not 
                                                   ! desired for a given variable, YOU MUST set both
                                                   ! components of packing_range to amiss.
                                                   ! NOTE:
                                                   ! * The packing algorithm sets all values
                                                   !    outside the packing range to missing.
                                                   ! * The larger the packing range, the greater
                                                   !    the loss of precision.
      integer, intent(in) ::        prec           ! Desired precision of data:
                                                   !   0 = 32 bit
                                                   !   1 = 64 bit
                                                   !   NOTE: mixing precision in the same 
                                                   !   * Mixing 32 and 64 bit precision in the 
                                                   !      same file is not supported.
                                                   !   * If packing is turned on for a variable,
                                                   !      the prec flag is ignored.
    
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: fid     ! File handle
      integer, intent(out) :: rc      ! Error return code:
                                      !  rc = 0   all is well
                                      !  rc = -1  time increment is 0
                                      !  rc = -18 incorrect time increment
                                      !
                                      !  NetCDF Errors
                                      !  -------------
                                      !  rc = -30  error from nccre (file create)
                                      !  rc = -31  error from ncddef
                                      !  rc = -32  error from ncvdef (dimension variable)
                                      !  rc = -33  error from ncaptc (dimension attribute)
                                      !  rc = -34  error from ncvdef (variable)
                                      !  rc = -35  error from ncaptc (variable attribute)
                                      !  rc = -36  error from ncaptc/ncapt (global attribute)
                                      !  rc = -37  error from ncendf
                                      !  rc = -38  error from ncvpt (dimension variable)
                                    

! !REVISION HISTORY: 
!
!  1997.09.13  da Silva/Lucchesi  Initial interface design.
!  1997.09.22  Lucchesi           Added timinc to interface.
!  1998.02.10  Lucchesi           Added support for applications running with
!                                 64-bit reals.
!  1998.02.17  Lucchesi           Added time_inc, begin_time, and begin_date 
!                                 attributes to the time dimension.
!  1998.03.30  Lucchesi           Documentation expanded.  Clean-up of code.
!  1998.07.07  Lucchesi           Removed vids from argument list
!  1998.07.09  Lucchesi           Converted timinc to seconds before saving
!  1998.10.09  Lucchesi           Precision flag, documentation changes.
!  1998.10.27  Lucchesi           Added support for packing and range checks
!  1998.11.18  Lucchesi           Modified timinc to be HHMMSS as given by user
!  1999.01.04  Lucchesi           Changed variable initialization
!  1999.03.30  Lucchesi           Added 'positive=down' attribute to lev.
!  2009.04.28  Lucchesi           Changed lon/lat/lev from float to double.
!
!EOP
!-------------------------------------------------------------------------

      character(len=MAXCHR) ::   vname_(nvars)  ! variable short name, e.g., "hght"
      character(len=MAXCHR) ::   vtitle_(nvars) ! variable long name, e.g.,
                                                !   "Geopotential Height"
      character(len=MAXCHR) ::   vunits_(nvars) ! variable units, e.g., "meter/second"

      integer :: i
      character(len=8192) ::   vname__
      character(len=8192) ::   vtitle__
      character(len=8192) ::   vunits__

      vname__ = vname
      vtitle__ = vtitle
      vunits__ = vunits


      do i = 1, nvars
         call getnext_(vname__, vname_(i),rc)
         if ( rc/= 0 ) return
         call getnext_(vtitle__,vtitle_(i),rc)
         if ( rc/= 0 ) return
         call getnext_(vunits__,vunits_(i),rc)
         if ( rc/= 0 ) return
      end do

            call GFIO_Create ( fname, title, source, contact, amiss, &
                               im, jm, km, lon, lat, levs, levunits, &
                               yyyymmdd_beg, hhmmss_beg, timinc,     &
                               nvars, vname_, vtitle_, vunits_, kmvar,  &
                               valid_range, packing_range, prec,     &
                               fid, rc )                             

      contains
        subroutine getnext_(str,value,rc)
          character(len=8192), intent(inout) :: str
          character(len=MAXCHR), intent(out) :: value
          integer, intent(out) :: rc
          integer :: i
          rc = 0
          i = index(str,':')
          if ( i<1 ) then
             rc = -1
             return
          end if
          value = str(1:i-1)
          str = str(i+1:)
         end subroutine getnext_

      end subroutine GfioCreate

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
! !ROUTINE:  GFIO_Open -- Opens an existing DAO gridded file 
!
!
! !DESCRIPTION: This routine opens an existing DAO gridded file.  The file
!               mode will be read/write.  If the application already knows
!               the contents of the file, it may begin interaction with the
!               file using the returned file handle.  Otherwise, the file
!               handle can be used with the "inquire" routines to gather 
!               information about the contents.  A negative return code 
!               indicates there were problems opening the file.
!
!
! !INTERFACE:
!
      subroutine gfioOpen ( fname, fmode, fid, rc )

!
! !USES:
!

      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"

!
! !INPUT PARAMETERS:
!

      character(len=*), intent(in) ::  fname  ! File name
      integer,          intent(in) ::  fmode  ! File mode:  
                                              !   0 for READ-WRITE 
                                              !   non-zero for READ-ONLY

!
! !OUTPUT PARAMETERS:
!

      integer, intent(out) :: fid   ! File handle
      integer, intent(out) :: rc    ! Error return code:
                                    !   rc = 0    All is well
                                    !   rc = -39  error from ncopn (file open)
! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------

      call GFIO_Open ( fname, fmode, fid, rc )

    end subroutine gfioOpen

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_PutVar -- Write a variable to the file
! 
! !DESCRIPTION: This routine is used to write a variable to an open GFIO 
!               stream.  Multiple vertical levels can be written at one 
!               time provided they are contiguous in memory.  Date and time 
!               must be consistent with the time increment and the starting 
!               date/time as defined in GFIO\_Create.  Times must fall on 
!               minute boundaries to allow GrADS to work.  Error checking is 
!               done for dimensions that are out of bounds.
!
! !INTERFACE:
!
      subroutine gfioPutVar ( fid, vname, yyyymmdd, hhmmss, &
                              im, jm, kbeg, kount, grid,  &
                              rc )  
!
! !USES:

      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS: 
!
      integer, intent(in) ::        fid                 ! File handle
      character(len=*), intent(in) ::  vname            ! Variable name
      integer, intent(in) ::        yyyymmdd            ! Year-month-day, e.g., 19971003
      integer, intent(in) ::        hhmmss              ! Hour-minute-second, e.g., 120000
 
      integer, intent(in) ::         im                 ! size of longitudinal dimension
      integer, intent(in) ::         jm                 ! size of latitudinal  dimension
      integer, intent(in) ::         kbeg               ! first level to write; if 2-D grid
                                                        !   use kbeg = 0.
      integer, intent(in) ::         kount              ! number of levels to write
      real, intent(in) ::            grid(im,jm,kount)  ! Gridded data to write at this time
                                     

! !OUTPUT PARAMETERS:
 
      integer, intent(out) ::        rc  ! Error return code:
                         !  rc =  0  all is well
                         !  rc = -2  time is inconsistent with increment 
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary    
                         !  rc = -7  error in diffdate              
                         !  rc = -12  error determining default precision

                         !  rc = -13  error determining variable type
                         !  rc = -15  data outside of valid range
                         !  rc = -16  data outside of packing range
                         !  rc = -17  data outside of pack and valid range
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -45  error from ncvpt
                         !  rc = -46  error from ncvgt
                         !  rc = -52  error from ncvinq
                         !  rc = -53  error from ncagtc/ncagt

! !REVISION HISTORY: 
!
!EOP
!-------------------------------------------------------------------------

   call GFIO_PutVar (fid, vname, yyyymmdd, hhmmss, im, jm, kbeg, kount, grid, rc)

 end subroutine gfioPutVar

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetVar -- Read a variable from the file
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Gfio\_open.
!
! !INTERFACE:
!
      subroutine gfioGetVarT ( fid, vname, yyyymmdd, hhmmss, &
                               im, jm, kbeg, kount, grid, rc, fid2)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer, intent(in) ::        fid              ! File handle
      character(len=*), intent(in) ::  vname         ! Variable name
      integer, intent(in) ::        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer, intent(in) ::          hhmmss         ! Hour-minute-second, e.g., 120000
      integer, intent(in) ::         im              ! size of longitudinal dimension
      integer, intent(in) ::         jm              ! size of latitudinal  dimension
      integer, intent(in) ::         kbeg            ! first level to read; if 2-D grid
                                                     !  set kbeg = 0.
      integer, intent(in) ::         kount           ! number of levels to read
      integer, intent(in) ::         fid2            ! File handle

      real, intent(out) ::    grid(im,jm,kount)      ! Gridded data read for this time
      integer, intent(out) :: rc                     ! Error return code:
!
!EOP
!--------------------------------------------------------------------------------------------------

      call GFIO_GetVarT ( fid, vname, yyyymmdd, hhmmss, im, jm, kbeg, kount, grid, rc, fid2)

    end subroutine gfioGetVarT

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetVar -- Read a variable from the file
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Gfio\_open.
!
! !INTERFACE:
!
      subroutine gfioGetVar ( fid, vname, yyyymmdd, hhmmss, &
                              im, jm, kbeg, kount, grid, rc)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer, intent(in) ::        fid              ! File handle
      character(len=*), intent(in) ::  vname         ! Variable name
      integer, intent(in) ::        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer, intent(in) ::          hhmmss         ! Hour-minute-second, e.g., 120000
      integer, intent(in) ::         im              ! size of longitudinal dimension
      integer, intent(in) ::         jm              ! size of latitudinal  dimension
      integer, intent(in) ::         kbeg            ! first level to read; if 2-D grid
                                                     !  set kbeg = 0.
      integer, intent(in) ::         kount           ! number of levels to read

      real, intent(out) ::    grid(im,jm,kount)      ! Gridded data read for this time
      integer, intent(out) :: rc                     ! Error return code:
!
!EOP
!--------------------------------------------------------------------------------------------------

      call GFIO_GetVar ( fid, vname, yyyymmdd, hhmmss, im, jm, kbeg, kount, grid, rc)

    end subroutine gfioGetVar

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_DimInquire -- Gets dimension information from a GFIO file.
!
! !DESCRIPTION: This routine is used to get dimension information from
!               an existing GFIO file.  This dimension information can 
!               subsequently be used to allocate arrays for reading data
!               from the file.  For more complete information about the 
!               contents of a file, Gfio\_Inquire should be used.

! !INTERFACE:
!
      subroutine gfioDimInquire (fid,im,jm,km,lm,nvars,ngatts,rc)
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer,intent(in) :: fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: im     ! Size of longitudinal dimension
      integer, intent(out) :: jm     ! Size of latitudinal dimension
      integer, intent(out) :: km     ! Size of vertical dimension
                                     !   km=0 if surface-only file
      integer, intent(out) :: lm     ! Number of times 
      integer, intent(out) :: nvars  ! Number of variables
      integer, intent(out) :: ngatts ! Number of global attributes
      integer, intent(out) :: rc     ! Error return code:
 
                                     !  rc = 0    all is well
                                     !  rc = -19  unable to identify coordinate variable
                                 
                                     !  NetCDF Errors
                                     !  -------------
                                     !  rc = -40  error from ncvid
                                     !  rc = -41  error from ncdid or ncdinq (lat or lon)
                                     !  rc = -42  error from ncdid or ncdinq (lev)
                                     !  rc = -43  error from ncvid (time variable)
                                     !  rc = -47  error from ncdid or ncdinq (time)
                                     !  rc = -48  error from ncinq
                                     !  rc = -53  error from ncagtc/ncagt

! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------

      call GFIO_DimInquire (fid,im,jm,km,lm,nvars,ngatts,rc)

    end subroutine gfioDimInquire

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Inquire -- Get information about a GFIO file.
! 
! !DESCRIPTION: This routine is used to get as much information as possible
!               about the contents of a GFIO file.  The file handle (fid) is
!               passed in and detailed information about dimensions and 
!               variables are returned to the application.  A simpler inquire
!               routine for dimension information is Gfio\_DimInquire.
!
! !INTERFACE:
!
      subroutine  gfioInquire ( fid, im, jm, km, lm, nvars,    &
                                title, source, contact, amiss, &
                                lon, lat, levs, levunits,      &
                                yyyymmdd, hhmmss, timinc,      &
                                vname, vtitle, vunits, kmvar,  &
                                valid_range , packing_range, rc)
!
! !USES:
!
      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"

!
! !INPUT PARAMETERS: 
!

                                    ! ------- Global Metadata ------
      integer, intent(in) :: fid    ! File handle from GFIO_open

      integer, intent(in) :: im     ! size of longitudinal dimension
      integer, intent(in) :: jm     ! size of latitudinal  dimension
      integer, intent(in) :: km     ! size of vertical     dimension
      integer, intent(in) :: lm     ! size of time         dimension
                                    ! On input, (im,jm,km,lm) contains the
                                    !  size of arrays (lon,lat,lev,yyyymmdd)
                                    !  as declared in the calling program.
      integer, intent(in) :: nvars  ! number of variables on file

!
! !OUTPUT PARAMETERS:
!
      character(len=MAXCHR), intent(out) ::   title    ! Title of the data set
      character(len=MAXCHR), intent(out) ::   source   ! Where it came from
      character(len=MAXCHR), intent(out) ::   contact  ! Who to contact about the data set
      real, intent(out) ::            amiss            ! Missing value 

                                                       ! ------- Dimension Metadata -------
      real, intent(out) ::            lon(im)          ! longitude of center of gridbox in
                                                       !   degrees east of Greenwich (can be
                                                       !   -180 -> 180 or 0 -> 360)
      real, intent(out) ::            lat(jm)          ! latitude of center of gridbox in
                                                       !   degrees north of equator
      real, intent(out) ::            levs(km)         ! Level (units given by levunits) of
                                                       !   center of gridbox
      character(len=MAXCHR), intent(out) ::   levunits ! units of level dimension, e.g.,
                                                       !   "hPa", "sigma_level"
      integer, intent(out) ::        yyyymmdd(lm)      ! Year-month-day on file 
      integer, intent(out) ::          hhmmss(lm)      ! Hour-minute-second on file 
      integer, intent(out) ::            timinc        ! Time increment. 

                                                       ! ------- Variable Metadata -------
      character(len=8192), intent(out) ::   vname      ! variable short name, e.g., "hght"
      character(len=8192), intent(out) ::   vtitle     ! variable long name, e.g.,
                                                                  !   "Geopotential Height"
      character(len=8192), intent(out) ::   vunits     ! variable units, e.g., "meter/second"
      integer, intent(out) ::         kmvar(nvars)     ! number of levels for variable; it can
                                                       !  either be 0 (2-D fields) or equal to km
      real, intent(out) ::    valid_range(2,nvars)     ! Variable valid range; set to
                                                       !   "amiss" if not known.
 
                                                       ! ------ Packing Metadata ----
      real, intent(out) ::  packing_range(2,nvars)     ! Variable packing range used
                                                       !  for 16-bit packing. If packing was not
                                                       !  used then returned values will be amiss. 
                                                       !  NOTE: all unpacking is done transparently
                                                       !       by GFIO_GetVar(). 
    
      integer, intent(out) ::    rc                    ! Error return code:
                                                       !   rc = 0    all is well
                                                       !   rc = -3  number of levels is incompatible with file
                                                       !   rc = -4  im is incompatible with file
                                                       !   rc = -5  jm is incompatible with file
                                                       !   rc = -8  lm is incompatible with file
                                                       !   rc = -9  nvars is incompatible with file
                                                       !   rc = -14  error in getdate
                                                       !   rc = -20  vname strings too short
                                                       !
                                                       !  NetCDF Errors
                                                       !  -------------
                                                       !  rc = -41  error from ncdid or ncdinq (lat or lon)
                                                       !  rc = -42  error from ncdid or ncdinq (lev)
                                                       !  rc = -43  error from ncvid (time variable)
                                                       !  rc = -47  error from ncdid or ncdinq (time)
                                                       !  rc = -48  error from ncinq
                                                       !  rc = -50  error from ncagtc (level attribute)
                                                       !  rc = -51  error from ncagtc/ncagt (global attribute)
                                                       !  rc = -52  error from ncvinq
                                                       !  rc = -53  error from ncagtc/ncagt

! !FUTURE ENHANCEMENT:
!                   Next release should include a flag for precision.
!
! !REVISION HISTORY: 
!
!EOP
!-------------------------------------------------------------------------

!     Not yet able to pass array character back to python
!     ---------------------------------------------------
      character(len=MAXCHR) ::   vname_(nvars)  ! variable short name, e.g., "hght"
      character(len=MAXCHR) ::   vtitle_(nvars) ! variable long name, e.g.,
                                                !   "Geopotential Height"
      character(len=MAXCHR) ::   vunits_(nvars) ! variable units, e.g., "meter/second"

      integer :: i, im_, jm_, km_, lm_, nvars_
      im_ = im
      jm_ = jm
      km_ = km
      lm_ = lm
      nvars_ = nvars

      call gfio_Inquire ( fid, im_, jm_, km_, lm_, nvars_,    &
                                title, source, contact, amiss, &
                                lon, lat, levs, levunits,      &
                                yyyymmdd, hhmmss, timinc,      &
                                vname_, vtitle_, vunits_, kmvar,  &
                                valid_range , packing_range, rc)

      vname = ''
      vtitle = ''
      vunits = ''
      do i = 1, nvars
         vname = trim(vname) // trim(vname_(i)) // ':'
         vtitle = trim(vtitle) // trim(vtitle_(i)) // ':'
         vunits = trim(vunits) // trim(vunits_(i)) // ':'
      end do
      
    end subroutine gfioInquire

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Close -- Closes file
!
! !DESCRIPTION: This routine is used to close an open GFIO stream.

! !INTERFACE:
!
      subroutine gfioClose ( fid, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer, intent(in) :: fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: rc ! Error return code:

                                 !   rc = 0    all is well
                                 !
                                 !  NetCDF Errors
                                 !  -------------
                                 !   rc = -54  error from ncclos (file close)
! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.03.30  Lucchesi           Documentation expanded.  Clean-up of code.
!                                 Added rc.
!
!EOP
!-------------------------------------------------------------------------

      call GFIO_Close ( fid, rc )

    end subroutine gfioClose


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GetBegDateTime - Get begin date/time of file
!
! !DESCRIPTION: This routine returns the begin date/begin time on file.
!               For a native GFIO file, it simply returns the value of
!  global attributes begin_date/begin_time. If these do no exist then
!  it attempts to parse the COARDS compliant time unit.
!
! !INTERFACE:
!
      subroutine gfioGetBegDateTime ( fid, begDate, begTime, incSecs, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer, intent(in) :: fid      ! file ID
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: begDate  ! beginning date
      integer, intent(out) :: begTime  ! beginning time
      integer, intent(out) :: incSecs  ! time increment in secs
      integer, intent(out) :: rc       ! error return code
!
! !REVISION HISTORY:
!
!  03mar2010  da Silva  Introduced for python interface
!
!EOP
!-----------------------------------------------------------------------------------------
      call GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
    end subroutine gfioGetBegDateTime

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  gfioInterpxy --- Interpolates 3D field to observation locations
!
! !INTERFACE:
!
  subroutine gfioInterpXY ( lon, lat, nobs, im, jm, km, lon_0, gField, oField )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      integer, intent(in)        :: nobs      ! Number of observations
      real,    intent(in)        :: lon(nobs) ! longitude in degrees [-180,+180]
      real,    intent(in)        :: lat(nobs) ! latitude  in degrees [-90,90]

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension
      integer, intent(in)        :: km        ! vertical dimension: 
                                              ! = 1 for 2D fields
                                              ! = km for mid-layer fields
                                              ! = km+1 for edge fields

      real,    intent(in)        :: lon_0     ! longitude orign of gridded field

                                              ! Gridded Field
      real,    intent(in)        :: gField(im,jm,km) 

! !OUTPUT PARAMETERS:
!
                                              ! Interpolated profile
      real,    intent(out)       :: oField(km,nobs)

! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This routine implements only the horizontal
!  interpolation.
!
!  IMPORTANT:   The input lon coordinates must be in [lon_0, lon_0+360].
!               The input field cannot have any UNDEFs.
!               For now, grid assumed to be global and zonally periodic
!               Latitudes must be in the range [-90,90]
!               Observation and grid must have the same reference, that is,
!               if input lon is in the range [-180,180] then grid must be 
!               in the same range.
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  10feb2010  da Silva  Simplified m_interp routine for profile interpolation.
!
!EOP
!-------------------------------------------------------------------------
 
    character(len=*), parameter :: myname = 'InterpXY_'


! Local
      integer i, j, k, nob
      real    o_lon, o_lat
      real    m_dlon, m_dlat
      real    alfa, beta

      real a11(km)         !W-S
      real a12(km)         !W-N
      real a21(km)         !E-S
      real a22(km)         !E-N
      real a00(km)         !temp

      integer i1, i2

!                         ------

     if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

     m_dlon = float(im) / 360.
     m_dlat = float(jm-1) / 180.
     
!    Loop over observations
!    ----------------------
     do nob = 1, nobs

!       Longitude
!       ---------
        o_lon = 1. + (lon(nob)-lon_0) * m_dlon
        i   = min(im, int( o_lon ))
        alfa  = o_lon - i
        if(i .eq. im) then
           i1 = im
           i2 = 1
        else
           i1 = i
           i2 = i + 1
        endif
        
!       Latitude
!       --------
        o_lat = 1. + (lat(nob) + 90.) * m_dlat
        j   = min( jm-1, int( o_lat ) )
        beta  = o_lat - j
        
        a11 = gField(i1,j,  :)
        a21 = gField(i2,j,  :)
        a12 = gField(i1,j+1,:)
        a22 = gField(i2,j+1,:)
        a00 = a11 + alfa * ( a21 - a11 )

        oField(:,nob) = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
     
  end do ! loop over obs
  
  return
  
end subroutine gfioInterpXY

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  gfioInterpNN --- Nearest Neighbor interpolation
!
! !INTERFACE:
!
  subroutine gfioInterpNN ( lon, lat, nobs, im, jm, km, lon_0, gField, oField )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      integer, intent(in)        :: nobs      ! Number of observations
      real,    intent(in)        :: lon(nobs) ! longitude in degrees [-180,+180]
      real,    intent(in)        :: lat(nobs) ! latitude  in degrees [-90,90]

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension
      integer, intent(in)        :: km        ! vertical dimension: 
                                              ! = 1 for 2D fields
                                              ! = km for mid-layer fields
                                              ! = km+1 for edge fields

      real,    intent(in)        :: lon_0     ! longitude orign of gridded field

                                              ! Gridded Field
      real,    intent(in)        :: gField(im,jm,km) 

! !OUTPUT PARAMETERS:
!
                                              ! Interpolated profile
      real,    intent(out)       :: oField(km,nobs)
 
! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This routine implements only the horizontal
!  interpolation using a simple nearest neighbor algorithm.
!
!  IMPORTANT:   The input lon coordinates must be in [lon_0, lon_0+360].
!               The input field cannot have any UNDEFs.
!               For now, grid assumed to be global and zonally periodic
!               Latitudes must be in the range [-90,90]
!               Observation and grid must have the same reference, that is,
!               if input lon is in the range [-180,180] then grid must be 
!               in the same range.
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  10feb2010  da Silva  Simplified m_interp routine for profile interpolation.
!
!EOP
!-------------------------------------------------------------------------
 
    character(len=*), parameter :: myname = 'InterpNN_'


! Local
      integer i, j, nob
      real    o_lon, o_lat
      real    m_dlon, m_dlat
!                         ------

     if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

     m_dlon = float(im) / 360.
     m_dlat = float(jm-1) / 180.
     
!    Loop over observations
!    ----------------------
     do nob = 1, nobs

        i = 1 + nint( (lon(nob)-lon_0) * m_dlon)
        j = max(1,min(jm,1 + nint( (lat(nob) + 90.) * m_dlat)))
        if ( i>im ) i = i - im

#if 0
        if ( (i<1).or.(i>im).or.(j<1).or.(j>jm) ) then
           print *, '>>>> ERROR <<<< invalid indices ', i, j
           return
        end if
#endif

        ! if ( (mod(nob,100)==0) .and. i==593 .and. j==721 ) print *, '*** nearest ', i, j, nob-1, gField(i,j,:) 

        oField(:,nob) = gField(i,j,:)

  end do ! loop over obs
  
  return
  
end subroutine gfioInterpNN

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  gfioCoordNN --- Nearest Neighbor interpolation
!
! !INTERFACE:
!
  subroutine gfioCoordNN ( lon, lat, nobs, im, jm, lon_0, &
                            iCoord, jCoord )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      integer, intent(in)        :: nobs      ! Number of observations
      real,    intent(in)        :: lon(nobs) ! longitude in degrees [-180,+180]
      real,    intent(in)        :: lat(nobs) ! latitude  in degrees [-90,90]

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension

      real,    intent(in)        :: lon_0     ! longitude orign of gridded field

! !OUTPUT PARAMETERS:
!
                                              ! Coordinates
      integer, intent(out)       :: iCoord(nobs)
      integer, intent(out)       :: jCoord(nobs)

! !DESCRIPTION: This routine is a companion to gfioInterpNN; it returns the (i,j) indices
!               used for the interpolation.
!
!  IMPORTANT:   The input lon coordinates must be in [lon_0, lon_0+360].
!               The input field cannot have any UNDEFs.
!               For now, grid assumed to be global and zonally periodic
!               Latitudes must be in the range [-90,90]
!               Observation and grid must have the same reference, that is,
!               if input lon is in the range [-180,180] then grid must be 
!               in the same range.
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  10feb2010  da Silva  Simplified m_interp routine for profile interpolation.
!
!EOP
!-------------------------------------------------------------------------
 
    character(len=*), parameter :: myname = 'CoordNN_'


! Local
      integer i, j, nob
      real    o_lon, o_lat
      real    m_dlon, m_dlat
!                         ------

     if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

     m_dlon = float(im) / 360.
     m_dlat = float(jm-1) / 180.
     
!    Loop over observations
!    ----------------------
     do nob = 1, nobs

        i = 1 + nint( (lon(nob)-lon_0) * m_dlon)
        j = max(1,min(jm,1 + nint( (lat(nob) + 90.) * m_dlat)))

        if ( i>im ) i = i - im

        iCoord(nob) = i
        jCoord(nob) = j

  end do ! loop over obs
  
  return
  
end subroutine gfioCoordNN

