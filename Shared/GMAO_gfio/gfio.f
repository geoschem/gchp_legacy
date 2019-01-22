!! #define __NCVERBOS__ NCVERBOS
#define __NCVERBOS__ 0
#include "unused_dummy.H"

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
!  !TITLE: The Grid File I/O (Gfio) Library \\ Version 1.0.8
!
!  !AUTHORS: Rob Lucchesi and  Arlindo da Silva
!
!  !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
!  !DATE: November 2, 1999 (Original design October 1997)
!
!  !INTRODUCTION: System Overview
!
!   \begin{verbatim}
!
!    Basic Requirements:
!    ------------------
!
!    (1) Design an interface for writing HDF format data files from 
!        the June 1998 GEOS-3 production system without requiring the 
!        direct insertion of HDF Toolkit calls in the code.
!
!    (2) Design this interface to be flexible enough to support usage in
!        other DAO applications that read or write HDF data.
!
!    (3) Output files should conform the COARDS conventions. This allows the 
!        data to be immediately usable by GrADS, other visualization packages 
!        and utilites such as ncdump.
!                          
!    (4) Provide a library that is callable from a Fortran 77 application
!        with a portable interface.
!
!    (5) The library must also be callable by C, perhaps with the use of a
!        tool like Cfortran.h.
!
!    The primary motivation behind GFIO is to provide an easy way for
!    the GEOS-DAS to write HDF format data while hiding calls to the HDF
!    libraries.  Additionally, it is hoped that this library will be of general 
!    use for reading or writing HDF files in applications other than the 
!    GEOS-DAS.
!
!    The typical calling sequence for creating a file would be:
!
!        GFIO_Create(...)
!      
!        GFIO_PutVar(...)
!        GFIO_PutVar(...)
!        GFIO_PutVar(...)
!            .
!            .
!            .
!        GFIO_Close(...)
!
!    One could subsequently open the file for more writing with:
!
!        GFIO_Open(...)
!
!    NOTES:
!    -----
!
!    * Surface data is permitted in the same file as upper air data, however
!      all upper air data must be defined with the same number of levels but
!      it is not necessary to write data for each defined level.  In the case
!      that data is not written for a given level, HDF will put fills.
!
!    * Packing is not yet implemented.
!
!    * The time increment cannot be defined as 0, even if only writing one
!      time.
!
!    * Files are written using the NetCDF interface provided in the HDF
!      library.  The files conform to the COARDS conventions, meaning they
!      have specific metadata defined by the convention.  
!
!    * As of v1.0.8 some generic COARDS compliant files (as those written by
!      by LATS4d) can be read with GFIO.
!
!   \end{verbatim}
!
!EOI
!-------------------------------------------------------------------------

!
!                    INTERFACE FOR WRITING A FILE
!                    ----------------------------
!
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Create -- Wrapper around GFIO_Create1 
! 
! !DESCRIPTION: This routine is used to open a new file for a GFIO stream.
!               Packing is not yet supported.  Information about each opened
!               stream is stored in a COMMON block contained in gfio.h.  
!               This information is later used by GFIO\_PutVar.  GFIO\_Open
!               should be used to open an existing file for reading or writing.
!
!               The routine call GFIO_Create1 and create a NETCDF4/HDF by
!               default.
!
! !INTERFACE:
!
        subroutine GFIO_Create ( fname, title, source, contact, amiss,
     &                         im, jm, km, lon, lat, levs, levunits, 
     &                         yyyymmdd_beg, hhmmss_beg, timinc,
     &                         nvars, vname, vtitle, vunits, kmvar,
     &                         valid_range, packing_range, prec,
     &                         fid, rc )
        
      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"

!
! !INPUT PARAMETERS: 
!
                                    ! ------- Global Metadata ------
      character*(*)   fname         ! File name
      character*(*)   title         ! A title for the data set
      character*(*)   source        ! Source of data, e.g. NASA/DAO
      character*(*)   contact       ! Who to contact about the data set, e.g.,
                                    ! 'Contact data@gmao.gsfc.nasa.gov'
      real            amiss         ! Missing value such as 1.0E15

                                    ! ------- Dimension Metadata -------
      integer         im            ! size of longitudinal dimension
      integer         jm            ! size of latitudinal  dimension
      integer         km            ! size of vertical     dimension 
                                    ! (surface only=1)
      real            lon(im)       ! longitude of center of gridbox in 
                                    ! degrees east of Greenwich (can be 
                                    ! -180 -> 180 or 0 -> 360)
      real            lat(jm)       ! latitude of center of gridbox in 
                                    ! degrees north of equator
      real            levs(km)      ! Level (units given by levunits) of
                                    !   center of gridbox
      character*(*)   levunits      ! units of level dimension, e.g.,
                                    !   "millibar", "hPa", or "sigma_level"
      integer        yyyymmdd_beg   ! First year-month-day to be written 
      integer          hhmmss_beg   ! First hour-minute-second to be written
      integer         timinc        ! Increment between output times (HHMMSS)

                                    ! ------- Variable Metadata -------
      integer         nvars         ! number of variables in file
      character*(*)   vname(nvars)  ! variable short name, e.g., "hght"
      character*(*)   vtitle(nvars) ! variable long name, e.g.,
                                    !   "Geopotential Height"
      character*(*)   vunits(nvars) ! variable units, e.g., "meter/second"
      integer         kmvar(nvars)  ! number of levels for variable; it can
                                    !  either be 0 (2-D fields) or equal to km

      real    valid_range(2,nvars)  ! Variable valid range; GFIO_PutVar
                                    ! will return a non-fatal error if a value is 
                                    ! outside of this range. IMPORTANT: If packing
                                    ! is not desired for a given variable, YOU MUST
                                    ! set both components of valid_range to amiss.
                                    ! ------ Packing Metadata ----
      real   packing_range(2,nvars) ! Packing range to be used for 16-bit packing 
                                    ! of each variable. IMPORTANT: If packing is not 
                                    ! desired for a given variable, YOU MUST set both
                                    ! components of packing_range to amiss.
                                    ! NOTE:
                                    ! * The packing algorithm sets all values
                                    !    outside the packing range to missing.
                                    ! * The larger the packing range, the greater
                                    !    the loss of precision.
      integer        prec           ! Desired precision of data:
                                    !   0 = 32 bit
                                    !   1 = 64 bit
                                    !   NOTE: mixing precision in the same 
                                    !   * Mixing 32 and 64 bit precision in the 
                                    !      same file is not supported.
                                    !   * If packing is turned on for a variable,
                                    !      the prec flag is ignored.

!      integer, intent(in), optional ::  nf_kind ! Desired file format NETCDF4/HDF5 or NETCDF3 nf_kind
      integer        nf_kind           ! Desired file format NETCDF4/HDF5 or NETCDF3 nf_kind
                                    !  1 = NETCDF4/HDF5
	                            !  0 = NETCDF3

!
! !OUTPUT PARAMETERS:
!
      integer        fid     ! File handle
      integer        rc      ! Error return code:
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
!  2010.09.15  Nadeau             Wrapper to create NetCDF4/HDF5 file

        nf_kind=1
            call GFIO_Create1 ( fname, title, source, contact, amiss,
     &       im, jm, km, lon, lat, levs, levunits, 
     &       yyyymmdd_beg, hhmmss_beg, timinc,
     &       nvars, vname, vtitle, vunits, kmvar,
     &       valid_range, packing_range, prec,
     &       nf_kind, fid,  rc )

        end
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Create1 -- Creates a DAO gridded file for writing
! 
! !DESCRIPTION: This routine is used to open a new file for a GFIO stream.
!               Packing is not yet supported.  Information about each opened
!               stream is stored in a COMMON block contained in gfio.h.  
!               This information is later used by GFIO\_PutVar.  GFIO\_Open
!               should be used to open an existing file for reading or writing.
!
! !INTERFACE:
!
      subroutine GFIO_Create1 ( fname, title, source, contact, amiss,
     &                         im, jm, km, lon, lat, levs, levunits, 
     &                         yyyymmdd_beg, hhmmss_beg, timinc,
     &                         nvars, vname, vtitle, vunits, kmvar,
     &                         valid_range, packing_range, prec,
     &                         nf_kind, fid, rc )
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
      character*(*)   fname         ! File name
      character*(*)   title         ! A title for the data set
      character*(*)   source        ! Source of data, e.g. NASA/DAO
      character*(*)   contact       ! Who to contact about the data set, e.g.,
                                    ! 'Contact data@gmao.gsfc.nasa.gov'
      real            amiss         ! Missing value such as 1.0E15

                                    ! ------- Dimension Metadata -------
      integer         im            ! size of longitudinal dimension
      integer         jm            ! size of latitudinal  dimension
      integer         km            ! size of vertical     dimension 
                                    ! (surface only=1)
      real            lon(im)       ! longitude of center of gridbox in 
                                    ! degrees east of Greenwich (can be 
                                    ! -180 -> 180 or 0 -> 360)
      real            lat(jm)       ! latitude of center of gridbox in 
                                    ! degrees north of equator
      real            levs(km)      ! Level (units given by levunits) of
                                    !   center of gridbox
      character*(*)   levunits      ! units of level dimension, e.g.,
                                    !   "millibar", "hPa", or "sigma_level"
      integer        yyyymmdd_beg   ! First year-month-day to be written 
      integer          hhmmss_beg   ! First hour-minute-second to be written
      integer         timinc        ! Increment between output times (HHMMSS)

                                    ! ------- Variable Metadata -------
      integer         nvars         ! number of variables in file
      character*(*)   vname(nvars)  ! variable short name, e.g., "hght"
      integer         vmode         ! variable type
      character*(*)   vtitle(nvars) ! variable long name, e.g.,
                                    !   "Geopotential Height"
      character*(*)   vunits(nvars) ! variable units, e.g., "meter/second"
      integer         kmvar(nvars)  ! number of levels for variable; it can
                                    !  either be 0 (2-D fields) or equal to km

      real    valid_range(2,nvars)  ! Variable valid range; GFIO_PutVar
                                    ! will return a non-fatal error if a value is 
                                    ! outside of this range. IMPORTANT: If packing
                                    ! is not desired for a given variable, YOU MUST
                                    ! set both components of valid_range to amiss.
                                    ! ------ Packing Metadata ----
      real   packing_range(2,nvars) ! Packing range to be used for 16-bit packing 
                                    ! of each variable. IMPORTANT: If packing is not 
                                    ! desired for a given variable, YOU MUST set both
                                    ! components of packing_range to amiss.
                                    ! NOTE:
                                    ! * The packing algorithm sets all values
                                    !    outside the packing range to missing.
                                    ! * The larger the packing range, the greater
                                    !    the loss of precision.
      integer        prec           ! Desired precision of data:
                                    !   0 = 32 bit
                                    !   1 = 64 bit
                                    !   NOTE: mixing precision in the same 
                                    !   * Mixing 32 and 64 bit precision in the 
                                    !      same file is not supported.
                                    !   * If packing is turned on for a variable,
                                    !      the prec flag is ignored.

!      integer, intent(in), optional ::  nf_kind ! Desired file format NETCDF4/HDF5 or NETCDF3 nf_kind
      integer        nf_kind           ! Desired file format NETCDF4/HDF5 or NETCDF3 nf_kind
                                    !  1 = NETCDF4/HDF5
	                            !  0 = NETCDF3
    
!
! !OUTPUT PARAMETERS:
!
      integer        fid     ! File handle
      integer        rc      ! Error return code:
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
!  2018.05.08  Todling            Add attribute coordinate for MAPL compatibility
!
!EOP
!-------------------------------------------------------------------------

      ! REAL*4 variables for 32-bit output to netCDF file.

      real*4 amiss_32
      real*8 lon_64(im), lat_64(jm), levs_64(km)
      real*4 scale_32, offset_32
      real*4 high_32,low_32
      integer vid(nvars)
      integer i, j
      integer timeid, latid, lonid, levid
      integer timedim, latdim, londim, levdim
      integer dims3D(4), dims2D(3)
      integer corner(4), edges(4)
#ifdef HAS_NETCDF4
      integer :: chunksizes(4)
#endif
      character*80 timeUnits 
      logical surfaceOnly
      integer year,mon,day,hour,min,sec
      integer err

! Variables for packing

      integer*2 amiss_16
      real*4 pRange_32(2,nvars),vRange_32(2,nvars)
      logical packflag

! Set metadata strings.  These metadata values are specified in the 
! COARDS conventions

      character (len=50) :: lonName = "longitude"
      character (len=50) :: lonUnits = "degrees_east"
      character (len=50) :: latName = "latitude"
      character (len=50) :: latUnits = "degrees_north"
      character (len=50) :: levName = "vertical level"
c                           levUnits: specified by user in argument list
      character (len=50) :: timeName = "time"
c                           timeUnits: string is built below
      character (len=50) :: conventions = "COARDS"
      character (len=50) :: history = "File written by GFIO v1.0.8"

      
#if defined(HAS_NETCDF4)
      _UNUSED_DUMMY(nf_kind)
#endif


      amiss_16 = PACK_FILL

! Variable initialization

      surfaceOnly = .TRUE.

! Basic error-checking.

      if (timinc .eq. 0) then
        rc=-1
        return
      endif

! Check to see if there is only surface data in this file definition

      do i=1,nvars
        if (kmvar(i) .NE. 0) then
          surfaceOnly = .FALSE.
          exit
        endif
      enddo

! Convert double-precision output variables to single-precision

      do i=1,im
         lon_64(i) = lon(i)
      enddo
      do i=1,jm
         lat_64(i) = lat(i)
      enddo
      do i=1,km
         levs_64(i) = levs(i)
      enddo
      do j=1,nvars
        do i=1,2
           vRange_32(i,j) = valid_range(i,j)
           pRange_32(i,j) = packing_range(i,j)
        enddo
      enddo

      amiss_32 = amiss

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

! Create the new NetCDF file. [ Enter define mode. ]

#if defined(HAS_NETCDF4)
      if( nf_kind  .eq. 1) then
          rc = nf_create (fname, IOR(IOR(NF_CLOBBER,NF_NETCDF4),NF_CLASSIC_MODEL), fid) !NETCDF4/HDF5
      else 
!          rc = nf_create (fname, IOR(NF_CLOBBER,NF_CLASSIC_MODEL), fid)  !NETCDF3
          fid = nccre (fname, NCCLOB, rc)
      endif
#else
      fid = nccre (fname, NCCLOB, rc)
#endif
      if (err("Create: can't create file",rc,-30) .LT. 0) return

! Define dimensions.

      londim = ncddef (fid, 'lon', im, rc)
      if (err("Create: error defining lon",rc,-31) .LT. 0) return
      latdim = ncddef (fid, 'lat', jm, rc)
      if (err("Create: error defining lat",rc,-31) .LT. 0) return
      if (.NOT. surfaceOnly) then
        levdim = ncddef (fid, 'lev', km, rc)
        if (err("Create: error defining lev",rc,-31) .LT. 0) return
      endif
      timedim = ncddef(fid, 'time', NCUNLIM, rc)
      if (err("Create: error defining time",rc,-31) .LT. 0) return

! Define dimension variables.

      lonid = ncvdef (fid, 'lon', NCDOUBLE, 1, londim, rc)
      if (err("Create: error creating lon",rc,-32) .LT. 0) return
      latid = ncvdef (fid, 'lat', NCDOUBLE, 1, latdim, rc)
      if (err("Create: error creating lat",rc,-32) .LT. 0) return
      if (.NOT. surfaceOnly) then
        levid = ncvdef (fid, 'lev', NCDOUBLE, 1, levdim, rc)
        if (err("Create: error creating lev",rc,-32) .LT. 0) return
      endif
      timeid = ncvdef (fid, 'time', NCLONG, 1, timedim, rc)
      if (err("Create: error creating time",rc,-32) .LT. 0) return

! Set attributes for dimensions.

      call ncaptc (fid,lonid,'long_name',NCCHAR,LEN_TRIM(lonName),
     .             lonName,rc)
      if (err("Create: error creating lon attribute",rc,-33) .LT. 0)
     .   return
      call ncaptc (fid,lonid,'units',NCCHAR,LEN_TRIM(lonUnits),
     .             lonUnits,rc)
      if (err("Create: error creating lon attribute",rc,-33) .LT. 0) 
     .   return

      call ncaptc (fid,latid,'long_name',NCCHAR,LEN_TRIM(latName),
     .             latName,rc)
      if (err("Create: error creating lat attribute",rc,-33) .LT. 0) 
     .   return
      call ncaptc (fid,latid,'units',NCCHAR,LEN_TRIM(latUnits),
     .             latUnits,rc)
      if (err("Create: error creating lat attribute",rc,-33) .LT. 0) 
     .   return

      if (.NOT. surfaceOnly) then
        call ncaptc (fid,levid,'long_name',NCCHAR,LEN_TRIM(levName),
     .               levName,rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)
     .      return
        call ncaptc (fid,levid,'units',NCCHAR,LEN_TRIM(levunits),
     .               levunits,rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)
     .      return
        call ncaptc (fid,levid,'positive',NCCHAR,LEN_TRIM('down'),
     .               'down',rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)
     .      return
        if (trim(levunits).eq.'layer') then
           call ncaptc (fid,levid,'coordinate',NCCHAR,LEN_TRIM('eta'),
     .                  'eta',rc)
           if (err("Create: error creating lev attribute",rc,-33) .LT. 0)
     .         return
        endif
      endif

      call ncaptc (fid, timeid, 'long_name', NCCHAR, LEN_TRIM(timeName),
     .             timeName, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)
     .   return

!ams      write (dateString,200) yyyymmdd_beg, hhmmss_beg
!ams 200   format (I8,I6)
!ams      read (dateString,201) year,mon,day,hour,min,sec
!ams 201   format (I4,5I2)

      call GFIO_parseIntTime ( yyyymmdd_beg, year, mon, day )
      call GFIO_parseIntTime (   hhmmss_beg, hour, min, sec )

      write (timeUnits,202) year,mon,day,hour,min,sec
202   format ('minutes since ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',
     .         I2.2,':',I2.2)
      call ncaptc (fid, timeid, 'units', NCCHAR, LEN_TRIM(timeUnits), 
     .             timeUnits, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)
     .   return
      
!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)

      call GFIO_parseIntTime ( timinc, hour, min, sec )

      if ( sec .NE. 0) then
        print *, 'GFIO_Create: Time increments not on minute',
     .           ' boundaries are not currently allowed.'
        rc = -18
        return
      endif
      call ncapt (fid, timeid, 'time_increment', NCLONG, 1, timInc, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)
     .   return
      call ncapt (fid,timeid,'begin_date',NCLONG,1,yyyymmdd_beg,rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)
     .   return
      call ncapt (fid,timeid,'begin_time',NCLONG,1,hhmmss_beg,rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)
     .   return


      if (.NOT. surfaceOnly) then
	dims3D(4) = timedim
        dims3D(3) = levdim
        dims3D(2) = latdim
        dims3D(1) = londim
      endif
      
      dims2D(3) = timedim
      dims2D(2) = latdim
      dims2D(1) = londim

      scale_32 = 1.0     ! No packing for now.
      offset_32 = 0.0    ! No packing for now.

! Set up packing attributes for each variable.  
! Define physical variables.  Set attributes for physical variables.

      do i=1,nvars
        scale_32 = 1.0                        ! default to no packing.
        offset_32 = 0.0
        if (pRange_32(1,i) .NE. amiss_32 .OR. pRange_32(2,i) .NE. 
     .  amiss_32) then
          if (pRange_32(1,i) .GT. pRange_32(2,i)) then
            high_32 = pRange_32(1,i)
            low_32  = pRange_32(2,i)
          else
            high_32 = pRange_32(2,i)
            low_32  = pRange_32(1,i)
          endif
          scale_32 = (high_32 - low_32)/PACK_BITS*2
          offset_32 = high_32 - scale_32*PACK_BITS
          if (scale_32 .EQ. 0.0) then              ! If packing range is 0,
             scale_32 = 1.0                        ! no packing.
             offset_32 = 0.0
             packflag = .FALSE.
          else
             packflag = .TRUE.
          endif
        else
          packflag = .FALSE.
        endif
        if ( kmvar(i) .eq. 0 ) then
          if (packflag) then
            vid(i) = ncvdef (fid, vname(i), NCSHORT, 3, dims2D, rc)
            vmode=NCSHORT;
          else if (prec .EQ. 1) then
            vid(i) = ncvdef (fid, vname(i), NCDOUBLE, 3, dims2D, rc)
            vmode=NCDOUBLE;
          else
            vid(i) = ncvdef (fid, vname(i), NCFLOAT, 3, dims2D, rc)
            vmode=NCFLOAT;
          endif
          if (err("Create: error defining variable",rc,-34) .LT. 0) 
     .         return

#if defined(HAS_NETCDF4)
	  if( nf_kind .eq. 1 ) then 
              chunksizes(3) = 1          ! time
              chunksizes(2) = jm
              chunksizes(1) = im
              rc = nf_def_var_chunking(fid, vid(i), NF_CHUNKED, chunksizes)
              if (err("Create: error defining chunking",rc,-34) .LT. 0) 
     .           return
          endif
#endif

        else
          if (packflag) then
            vid(i) = ncvdef (fid, vname(i), NCSHORT, 4, dims3D, rc)
            vmode=NCSHORT;
          else if (prec .EQ. 1) then
            vid(i) = ncvdef (fid, vname(i), NCDOUBLE, 4, dims3D, rc)
            vmode=NCDOUBLE;
          else
            vid(i) = ncvdef (fid, vname(i), NCFLOAT, 4, dims3D, rc)
            vmode=NCFLOAT;
          endif
          if (err("Create: error defining variable",rc,-34) .LT. 0) 
     .         return

#if defined(HAS_NETCDF4)
	  if( nf_kind .eq. 1 ) then 
              chunksizes(4) = 1           ! time
              chunksizes(3) = 1           ! level
              chunksizes(2) = jm
              chunksizes(1) = im
              rc = nf_def_var_chunking(fid, vid(i), NF_CHUNKED, chunksizes)
              if (err("Create: error defining chunking",rc,-34) .LT. 0) 
     .            return
           endif
#endif
        endif

        call ncaptc (fid, vid(i), 'long_name', NCCHAR, 
     .               LEN_TRIM(vtitle(i)),vtitle(i), rc)
        if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
        call ncaptc (fid, vid(i), 'units', NCCHAR, 
     .               LEN_TRIM(vunits(i)),vunits(i), rc)
        if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return

        if (packflag) then
          if (vmode .EQ. NCSHORT) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss_16,rc)
          end if 
          if (vmode .EQ. NCFLOAT) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss_32,rc)
          end if 
          if (vmode .EQ. NCDOUBLE) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss,rc)
          end if 
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
          if ( scale_32 .ne. 1.0 .or. offset_32 .ne. 0.0 ) then
          call ncapt (fid,vid(i),'scale_factor',NCFLOAT,1,scale_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .         .LT. 0) return
          call ncapt (fid,vid(i),'add_offset',NCFLOAT,1,offset_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .         .LT. 0) return
          call ncapt (fid,vid(i),'packmin',NCFLOAT,1,low_32,rc)
          if (err("Create: error defining variable attribute",rc,-35) 
     .        .LT. 0) return
          call ncapt (fid,vid(i),'packmax',NCFLOAT,1,high_32,rc)
          if (err("Create: error defining variable attribute",rc,-35) 
     .        .LT. 0) return
          end if
          call ncapt (fid,vid(i),'missing_value',NCSHORT,1,amiss_16,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
          call ncapt (fid,vid(i),'fmissing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
        else
          if (vmode .EQ. NCSHORT) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss_16,rc)
          end if 
          if (vmode .EQ. NCFLOAT) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss_32,rc)
          end if 
          if (vmode .EQ. NCDOUBLE) then
            call ncapt (fid,vid(i),'_FillValue',vmode,1,amiss,rc)
          end if 
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
          if ( scale_32 .ne. 1.0 .or. offset_32 .ne. 0.0 ) then
          call ncapt (fid,vid(i),'scale_factor',NCFLOAT,1,scale_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .         .LT. 0) return
          call ncapt (fid,vid(i),'add_offset',NCFLOAT,1,offset_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .         .LT. 0) return
          end if
          call ncapt (fid,vid(i),'missing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
          call ncapt (fid,vid(i),'fmissing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .     .LT. 0) return
        endif

        if (vRange_32(1,i) .NE. amiss_32 .OR. vRange_32(2,i) .NE. 
     .      amiss_32) then
          if (vRange_32(1,i) .GT. vRange_32(2,i)) then
            high_32 = vRange_32(1,i)
            low_32  = vRange_32(2,i)
          else
            high_32 = vRange_32(2,i)
            low_32  = vRange_32(1,i)
          endif
          call ncapt (fid,vid(i),'vmin',NCFLOAT,1,low_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .        .LT. 0) return
          call ncapt (fid,vid(i),'vmax',NCFLOAT,1,high_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .        .LT. 0) return
        else
          call ncapt (fid,vid(i),'vmin',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .        .LT. 0) return
          call ncapt (fid,vid(i),'vmax',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining variable attribute",rc,-35)
     .        .LT. 0) return

        endif
      enddo
 
! Define global file attributes.

      call ncaptc (fid,NCGLOBAL,'Conventions',NCCHAR,
     .             LEN_TRIM(conventions),conventions,rc)
      if (err("Create: error defining Conventions",rc,-36).LT. 0) 
     .   return
      call ncaptc (fid,NCGLOBAL,'Source',NCCHAR,LEN_TRIM(source),
     .             source,rc)
      if (err("Create: error defining Source",rc,-36).LT. 0) return
      call ncaptc (fid,NCGLOBAL,'Title',NCCHAR,LEN_TRIM(title),title,
     .             rc)
      if (err("Create: error defining Title",rc,-36).LT. 0) return
      call ncaptc (fid,NCGLOBAL,'Contact',NCCHAR,LEN_TRIM(contact),
     .             contact,rc)
      if (err("Create: error defining Contact",rc,-36).LT. 0) return
      call ncaptc (fid,NCGLOBAL,'History',NCCHAR,LEN_TRIM(history),
     .             history,rc)
      if (err("Create: error defining History",rc,-36).LT. 0) return

! Exit define mode.

      call ncendf (fid, rc)
      if (err("Create: error exiting define mode",rc,-37) .LT. 0) 
     .  return

! Write out dimension variables.

      corner(1) = 1
      edges(1) = im
      call ncvpt (fid, lonid, corner, edges, lon_64, rc)
      if (err("Create: error writing lons",rc,-38) .LT. 0) return

      corner(1) = 1
      edges(1) = jm
      call ncvpt (fid, latid, corner, edges, lat_64, rc)
      if (err("Create: error writing lats",rc,-38) .LT. 0) return

      if (.NOT. surfaceOnly) then
        corner(1) = 1
        edges(1) = km
        call ncvpt (fid, levid, corner, edges, levs_64, rc)
        if (err("Create: error writing levs",rc,-38) .LT. 0) return
      endif

      corner(1) = 1
      edges(1) = 1
      call ncvpt (fid, timeid, corner, edges, 0, rc)
      if (err("Create: error writing times",rc,-38) .LT. 0) return

      rc=0
      return
      end

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
      subroutine GFIO_Open ( fname, fmode, fid, rc )

!
! !USES:
!

      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"

!
! !INPUT PARAMETERS:
!

      character*(*)   fname         ! File name
      integer         fmode         ! File mode:  
                                    !   0 for READ-WRITE 
                                    !   non-zero for READ-ONLY

!
! !OUTPUT PARAMETERS:
!

      integer        fid            ! File handle
      integer        rc             ! Error return code:
                                    !   rc = 0    All is well
                                    !   rc = -39  error from ncopn (file open)
! !REVISION HISTORY:
!
!  1998.07.02   Lucchesi             Initial interface design.
!  1998.07.07   Lucchesi             Initial coding.
!  1998.12.09   Lucchesi             Corrected for ncopn bug.
!EOP
!-------------------------------------------------------------------------

       integer err

        if ( fmode .EQ. 0) then
           fid = ncopn(fname, NCRDWR, rc)
        else         
           fid = ncopn(fname, NCNOWRIT, rc)
        endif
        if (fid .LT. 0) then    ! ncopn has a bug.  error codes should
           rc = fid             ! be returned in rc, but in reality they
        endif                   ! are returned in fid.  

       if (err("Open: error opening file",rc,-39) .NE. 0) return

       rc = 0
       return
       end

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
      subroutine GFIO_PutVar ( fid, vname, yyyymmdd, hhmmss,
     &                         im, jm, kbeg, kount, grid, 
     &                         rc )  
!
! !USES:

      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS: 
!
      integer        fid                 ! File handle
      character*(*)  vname               ! Variable name
      integer        yyyymmdd            ! Year-month-day, e.g., 19971003
      integer        hhmmss              ! Hour-minute-second, e.g., 120000
 
      integer         im                 ! size of longitudinal dimension
      integer         jm                 ! size of latitudinal  dimension
      integer         kbeg               ! first level to write; if 2-D grid
                                         !   use kbeg = 0.
      integer         kount              ! number of levels to write
      real            grid(im,jm,kount)  ! Gridded data to write at this time
                                     

! !OUTPUT PARAMETERS:
 
      integer        rc  ! Error return code:
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
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.02.10 Lucchesi            Added support for applications running with
!                                 64-bit reals.
!  1998.03.30 Lucchesi            Documentation expanded.  Clean-up of code.
!  1998.07.02 Lucchesi            Replaced vid with vname in argument list &
!                                 made related mods to code.
!  1998.09.24 Lucchesi            Changed error codes, removed DIM_CHECK if-def
!  1998.10.27 Lucchesi            Added support for packing and range checks
!  1998.12.15 Lucchesi            Added support for skipping times (allTimes)
!  1999.01.04 Lucchesi            Fixed bug in skipping times (allTimes)/also 
!                                 changed variable initialization.
!  1999.07.13 Lucchesi            Changes for REAL or INT time dimension
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, dimSize, dimId, timeType
      character*(MAXCHR) dimName
      integer corner(4), edges(4)
      integer vid
      integer seconds, DiffDate, timeIndex
      integer minutes                       ! added as a work-around
      integer i, j, k
      integer begDate, begTime, timInc
      integer err
      integer hour,min,sec,incSecs
      integer, allocatable ::  allTimes(:)
      integer fillTime

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:)
      real*8, allocatable :: grid_64(:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NCVINQ

      character*(MAXCHR) varName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing and range checking

      integer*2, allocatable :: grid_16(:,:,:)
      real*4, allocatable :: fminutes_32(:)
      real*4 high_32, low_32, amiss_32
      real*4 scale_32, offset_32
      logical outRange
      logical outPRange

! Variable initialization

      outRange = .FALSE.
      outPRange = .FALSE.

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.
! MAXCHR is a CPP define in the gfio.h file.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'GFIO_PutVar warning: MAXNCNAM is larger than ',
     .           'dimName array size.'
      endif

! Determine NetCDF variable ID.

      vid = ncvid (fid, vname, rc)
      if (err("PutVar: variable not defined",rc,-40) .NE. 0) return

! Basic error checking
      dimId = ncdid (fid, 'lon', rc)
      if (err("PutVar: can't get ID for lon",rc,-41) .NE. 0) return
      call ncdinq (fid, dimId, dimName, dimSize, rc)
      if (err("PutVar: can't get info for lon",rc,-41) .NE. 0) return
      if (dimSize .ne. im) then
        rc = -4
        return
      endif

      dimId = ncdid (fid, 'lat', rc)
      if (err("PutVar: can't get ID for lat",rc,-41) .NE. 0) return
      call ncdinq (fid, dimId, dimName, dimSize, rc)
      if (err("PutVar: can't get info for lat",rc,-41) .NE. 0) return
      if (dimSize .ne. jm) then
        rc = -5
        return
      endif

      if (kbeg .NE. 0) then
        dimId = ncdid (fid, 'lev', rc)
        if (err("PutVar: can't get ID for lev",rc,-42) .NE. 0) return
        call ncdinq (fid, dimId, dimName, dimSize, rc)
        if (err("PutVar: can't get info for lev",rc,-42) .NE. 0) return
        if (kbeg-1 + kount .gt. dimSize) then
          rc = -3
          return
        endif
      endif

! Determine number of seconds since starting date/time.

      timeId = ncvid (fid, 'time', rc)
      if (err("PutVar: time not defined",rc,-43) .NE. 0) return
      call ncagt (fid, timeId, 'begin_date', begDate, rc)
      if (err("PutVar: missing begin_date",rc,-44) .NE. 0) return
      call ncagt (fid, timeId, 'begin_time', begTime, rc)
      if (err("PutVar: missing begin_time",rc,-44) .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

      if (seconds .lt. 0) then
        print *, 'GFIO_PutVar: Error code from diffdate.  Problem with',
     .           ' date/time.'
        rc = -7
        return
      endif
      if ( MOD (seconds,60) .eq. 0 ) then 
        minutes = seconds / 60
      else
        print *, 'GFIO_PutVar: Currently, times must fall on minute ',
     .           'boundaries.'
        rc = -6
        return
      endif
 
! Confirm that this time is consistent with the starting time coupled with
! the time increment.

      call ncagt (fid, timeId, 'time_increment', timInc, rc)
      if (err("PutVar: missing time increment",rc,-44) .NE. 0) return
      
! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)

      call GFIO_parseIntTime ( timinc, hour, min, sec )

      incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'GFIO_putvar: Absolute time of ',seconds,' not ',
     .           'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=1
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=1
      else
        corner(1)=1
        corner(2)=1
        corner(3)=kbeg
        corner(4)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=1
      endif

! Check variable against valid range.

      call ncagt (fid, vid, 'vmin', low_32, rc)
      if (err("PutVar: can't get vmin",rc,-53) .NE. 0) return
      call ncagt (fid, vid, 'vmax', high_32, rc)
      if (err("PutVar: can't get vmax",rc,-53) .NE. 0) return
      call ncagt (fid, vid, 'fmissing_value', amiss_32, rc)
      if (err("PutVar: can't get fmissing_value",rc,-53) .NE. 0) return
      if (low_32 .NE. amiss_32 .OR. high_32 .NE. amiss_32) then
        do k=1,kount
          do j=1,jm
            do i=1,im
              if (grid(i,j,k) .GT. high_32 .OR. grid(i,j,k) .LT. 
     .        low_32) then
                outRange = .TRUE.
                goto 100
              endif
            enddo
          enddo
        enddo
100     continue
      endif
      
! Determine if we are writing single- or double-precision.

      call ncvinq (fid, vid, varName, type, nvDims, vDims, nvAtts, rc)
      if (err("PutVar: error in variable inquire",rc,-52) .NE. 0) return

! Write variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          call ncvpt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCDOUBLE) then               ! 64-bit
          allocate (grid_64(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_64(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_64, rc)
          deallocate (grid_64)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'packmax', high_32, rc)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'packmin', low_32, rc)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT. 
     .          high_32) then
                  grid_16(i,j,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_16, rc)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          allocate (grid_32(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_32(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_32, rc)
          deallocate (grid_32)
        else if (type .EQ. NCDOUBLE) then                ! 64-bit
          call ncvpt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'packmax', high_32, rc)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'packmin', low_32, rc)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT.
     .          high_32) then
                  grid_16(i,j,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_16, rc)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("PutVar: error writing variable",rc,-45) .NE. 0) return

! Read time dimension scale and fill all values up to the current time.
! This will insure missing times are defined with the proper time value.

      call ncdinq (fid, timeId, dimName, dimSize, rc)
      dimSize = dimSize - 1                           ! We've already written the 
                                                      ! the new time.
      allocate ( allTimes (MAX(timeIndex,dimSize)) )
      allocate ( fminutes_32 (MAX(timeIndex,dimSize)) )
      call ncvinq (fid,timeId,dimName,timeType,nvDims,vDims,nvAtts,rc)

      if (dimSize .GT. 0) then
        ! Depending on the version of GFIO used to write the file, the Time
        ! dimension variable can either be floating point or integer.

        corner(1)=1
        edges(1)=dimSize

        if (timeType .EQ. NCFLOAT) then
          call ncvgt (fid,timeId,corner,edges,fminutes_32,rc)
          do i=1,dimSize
            allTimes(i) = INT(fminutes_32(i))
          enddo
        else if (timeType .EQ. NCLONG) then
          call ncvgt (fid,timeId,corner,edges,allTimes,rc)
        endif
        if (err("PutVar: error reading times from file",rc,-46) .NE. 0)
     .      return
      endif

      ! This loop fills the time dimension scale based on the time increment 
      ! specified in GFIO_Create.  If GFIO ever changes to support variable 
      ! time increments, this code MUST be changed.   

      do i=1,timeIndex-1
        fillTime = (i-1) * incSecs/60
        allTimes(i) = fillTime
      enddo
      allTimes(timeIndex) = minutes

! Write filled time array to file.

      corner(1)=1
      edges(1)=timeIndex

      if (timeType .EQ. NCFLOAT) then
        do i=1,timeIndex
          fminutes_32(i) = INT(allTimes(i))
        enddo
        call ncvpt (fid,timeId,corner,edges,fminutes_32,rc)
      else if (timeType .EQ. NCLONG) then
        call ncvpt (fid,timeId,corner,edges,allTimes,rc)
      endif
      if (err("PutVar: error writing time",rc,-38) .NE. 0) return

      if (outRange .AND. outPRange) then
        rc = -17
      else if (outPRange) then
        rc = -16
      else if (outRange) then
        rc = -15
      else
        rc = 0
      endif

      deallocate ( allTimes )
      deallocate ( fminutes_32 )

      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_PutVarT -- Write a variable to the file over a number of times
! 
! !DESCRIPTION: This routine is used to write a variable to an open GFIO 
!               stream.  Multiple vertical levels can be written at one 
!               time provided they are contiguous in memory.  Date and time 
!               must be consistent with the time increment and the starting 
!               date/time as defined in GFIO\_Create.  Times must fall on 
!               minute boundaries to allow GrADS to work.  Error checking is 
!               done for dimensions that are out of bounds.
!               Extension of GFIO_PutVar to try and do multiple time write
!
! !INTERFACE:
!
      subroutine GFIO_PutVarT ( fid, vname, yyyymmdd, hhmmss,
     &                          im, jm, kbeg, kount, nbeg, ncount, grid, 
     &                          rc )  
!
! !USES:

      Implicit NONE  
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS: 
!
      integer        fid                 ! File handle
      character*(*)  vname               ! Variable name
      integer        yyyymmdd            ! First Year-month-day, e.g., 19971003
      integer        hhmmss              ! First Hour-minute-second, e.g., 120000
 
      integer         im                 ! size of longitudinal dimension
      integer         jm                 ! size of latitudinal  dimension
      integer         kbeg               ! first level to write; if 2-D grid
                                         !   use kbeg = 0.
      integer         kount              ! number of levels to write
      integer         nbeg               ! first time index to write
      integer         ncount             ! number of time slices to write
      real            grid(im,jm,kount,ncount)  ! Gridded data to write at this time
                                     

! !OUTPUT PARAMETERS:
 
      integer        rc  ! Error return code:
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
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.02.10 Lucchesi            Added support for applications running with
!                                 64-bit reals.
!  1998.03.30 Lucchesi            Documentation expanded.  Clean-up of code.
!  1998.07.02 Lucchesi            Replaced vid with vname in argument list &
!                                 made related mods to code.
!  1998.09.24 Lucchesi            Changed error codes, removed DIM_CHECK if-def
!  1998.10.27 Lucchesi            Added support for packing and range checks
!  1998.12.15 Lucchesi            Added support for skipping times (allTimes)
!  1999.01.04 Lucchesi            Fixed bug in skipping times (allTimes)/also 
!                                 changed variable initialization.
!  1999.07.13 Lucchesi            Changes for REAL or INT time dimension
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, dimSize, dimId, timeType
      character*(MAXCHR) dimName
      integer corner(4), edges(4)
      integer vid
      integer seconds, DiffDate, timeIndex
      integer minutes                       ! added as a work-around
      integer i, j, k, l
      integer begDate, begTime, timInc
      integer err
      integer hour,min,sec,incSecs
      integer, allocatable ::  allTimes(:)
      integer fillTime

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:,:)
      real*8, allocatable :: grid_64(:,:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NCVINQ

      character*(MAXCHR) varName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing and range checking

      integer*2, allocatable :: grid_16(:,:,:,:)
      real*4, allocatable :: fminutes_32(:)
      real*4 high_32, low_32, amiss_32
      real*4 scale_32, offset_32
      logical outRange
      logical outPRange

      _UNUSED_DUMMY(nbeg)
! Variable initialization

      outRange = .FALSE.
      outPRange = .FALSE.

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(NCVERBOS)

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.
! MAXCHR is a CPP define in the gfio.h file.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'GFIO_PutVar warning: MAXNCNAM is larger than ',
     .           'dimName array size.'
      endif

! Determine NetCDF variable ID.

      vid = ncvid (fid, vname, rc)
      if (err("PutVar: variable not defined",rc,-40) .NE. 0) return

! Basic error checking
      dimId = ncdid (fid, 'lon', rc)
      if (err("PutVar: can't get ID for lon",rc,-41) .NE. 0) return
      call ncdinq (fid, dimId, dimName, dimSize, rc)
      if (err("PutVar: can't get info for lon",rc,-41) .NE. 0) return
      if (dimSize .ne. im) then
        rc = -4
        return
      endif

      dimId = ncdid (fid, 'lat', rc)
      if (err("PutVar: can't get ID for lat",rc,-41) .NE. 0) return
      call ncdinq (fid, dimId, dimName, dimSize, rc)
      if (err("PutVar: can't get info for lat",rc,-41) .NE. 0) return
      if (dimSize .ne. jm) then
        rc = -5
        return
      endif

      if (kbeg .NE. 0) then
        dimId = ncdid (fid, 'lev', rc)
        if (err("PutVar: can't get ID for lev",rc,-42) .NE. 0) return
        call ncdinq (fid, dimId, dimName, dimSize, rc)
        if (err("PutVar: can't get info for lev",rc,-42) .NE. 0) return
        if (kbeg-1 + kount .gt. dimSize) then
          rc = -3
          return
        endif
      endif

! Determine number of seconds since starting date/time.

      timeId = ncvid (fid, 'time', rc)
      if (err("PutVar: time not defined",rc,-43) .NE. 0) return
      call ncagt (fid, timeId, 'begin_date', begDate, rc)
      if (err("PutVar: missing begin_date",rc,-44) .NE. 0) return
      call ncagt (fid, timeId, 'begin_time', begTime, rc)
      if (err("PutVar: missing begin_time",rc,-44) .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

      if (seconds .lt. 0) then
        print *, 'GFIO_PutVar: Error code from diffdate.  Problem with',
     .           ' date/time.'
        rc = -7
        return
      endif
      if ( MOD (seconds,60) .eq. 0 ) then 
        minutes = seconds / 60
      else
        print *, 'GFIO_PutVar: Currently, times must fall on minute ',
     .           'boundaries.'
        rc = -6
        return
      endif
 
! Confirm that this time is consistent with the starting time coupled with
! the time increment.

      call ncagt (fid, timeId, 'time_increment', timInc, rc)
      if (err("PutVar: missing time increment",rc,-44) .NE. 0) return
      
! Convert time increment to seconds.

!      write (strBuf,203) timinc
!203   format (I6)
!      read (strBuf,204) hour, min, sec
!204   format (3I2)
      call GFIO_parseIntTime ( timinc, hour, min, sec )
      incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'GFIO_putvar: Absolute time of ',seconds,' not ',
     .           'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Load starting indicies.
      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=1
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=ncount
      else
        corner(1)=1
        corner(2)=1
        corner(3)=kbeg
        corner(4)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=ncount
      endif

! Check variable against valid range.

      call ncagt (fid, vid, 'vmin', low_32, rc)
      if (err("PutVar: can't get vmin",rc,-53) .NE. 0) return
      call ncagt (fid, vid, 'vmax', high_32, rc)
      if (err("PutVar: can't get vmax",rc,-53) .NE. 0) return
      call ncagt (fid, vid, 'fmissing_value', amiss_32, rc)
      if (err("PutVar: can't get fmissing_value",rc,-53) .NE. 0) return
      if (low_32 .NE. amiss_32 .OR. high_32 .NE. amiss_32) then
       do l = 1, ncount
        do k=1,kount
          do j=1,jm
            do i=1,im
              if (grid(i,j,k,l) .GT. high_32 .OR. grid(i,j,k,l) .LT. 
     .        low_32) then
                outRange = .TRUE.
                goto 100
              endif
            enddo
          enddo
        enddo
       enddo
100    continue
      endif
      
! Determine if we are writing single- or double-precision.

      call ncvinq (fid, vid, varName, type, nvDims, vDims, nvAtts, rc)
      if (err("PutVar: error in variable inquire",rc,-52) .NE. 0) return

! Write variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          call ncvpt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCDOUBLE) then               ! 64-bit
          allocate (grid_64(im,jm,kount,ncount))
          do l = 1, ncount
           do k=1,kount
            do j=1,jm
              do i=1,im
                grid_64(i,j,k,l) = grid(i,j,k,l)
              enddo
            enddo
           enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_64, rc)
          deallocate (grid_64)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'packmax', high_32, rc)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'packmin', low_32, rc)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount,ncount))
          do l = 1, ncount
           do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k,l) .LT. low_32 .OR. grid(i,j,k,l) .GT. 
     .          high_32) then
                  grid_16(i,j,k,l) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k,l) = (grid(i,j,k,l) - offset_32)/scale_32
                endif
              enddo
            enddo
           enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_16, rc)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          allocate (grid_32(im,jm,kount,ncount))
          do l = 1, ncount
           do k=1,kount
            do j=1,jm
              do i=1,im
                grid_32(i,j,k,l) = grid(i,j,k,l)
              enddo
            enddo
           enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_32, rc)
          deallocate (grid_32)
        else if (type .EQ. NCDOUBLE) then                ! 64-bit
          call ncvpt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'packmax', high_32, rc)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'packmin', low_32, rc)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount,ncount))
          do l = 1, ncount
           do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k,l) .LT. low_32 .OR. grid(i,j,k,l) .GT.
     .          high_32) then
                  grid_16(i,j,k,l) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k,l) = (grid(i,j,k,l) - offset_32)/scale_32
                endif
              enddo
            enddo
           enddo
          enddo
          call ncvpt (fid, vid, corner, edges, grid_16, rc)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("PutVar: error writing variable",rc,-45) .NE. 0) return

! Read time dimension scale and fill all values up to the current time.
! This will insure missing times are defined with the proper time value.

      call ncdinq (fid, timeId, dimName, dimSize, rc)
!prc
!      dimSize = dimSize - 1                           ! We've already written the 
                                                      ! the new time.
!      allocate ( allTimes (MAX(timeIndex,dimSize)) )
!      allocate ( fminutes_32 (MAX(timeIndex,dimSize)) )

! PRC
! This part I don't yet understand.  What I want to do is fill in all times from timeindex:timeindex+ncount-1
      allocate ( allTimes (MAX(timeIndex+ncount-1,dimSize)) )
      allocate ( fminutes_32 (MAX(timeIndex+ncount-1,dimSize)) )

! PRC
!      if (dimSize .GT. 0) then
!        ! Depending on the version of GFIO used to write the file, the Time
!        ! dimension variable can either be floating point or integer.

!        corner(1)=1
!        edges(1)=dimSize

        call ncvinq (fid,timeId,dimName,timeType,nvDims,vDims,nvAtts,rc)

!        if (timeType .EQ. NCFLOAT) then
!          call ncvgt (fid,timeId,corner,edges,fminutes_32,rc)
!          do i=1,dimSize
!            allTimes(i) = INT(fminutes_32(i))
!          enddo
!        else if (timeType .EQ. NCLONG) then! prc
!          call ncvgt (fid,timeId,corner,edges,allTimes,rc)
!        endif
!        if (err("PutVar: error reading times from file",rc,-46) .NE. 0)
!     .      return
!      endif

      ! This loop fills the time dimension scale based on the time increment 
      ! specified in GFIO_Create.  If GFIO ever changes to support variable 
      ! time increments, this code MUST be changed.   

      do i=1,timeIndex+ncount-1
        fillTime = (i-1) * incSecs/60
        allTimes(i) = fillTime
      enddo
      allTimes(timeIndex) = minutes

! Write filled time array to file.

      corner(1)=timeIndex
      edges(1)=ncount

      if (timeType .EQ. NCFLOAT) then
        do i=1,timeIndex
          fminutes_32(i) = INT(allTimes(i))
        enddo
        call ncvpt (fid,timeId,corner,edges,fminutes_32,rc)
      else if (timeType .EQ. NCLONG) then
        call ncvpt (fid,timeId,corner,edges,allTimes,rc)
      endif
      if (err("PutVar: error writing time",rc,-38) .NE. 0) return

      if (outRange .AND. outPRange) then
        rc = -17
      else if (outPRange) then
        rc = -16
      else if (outRange) then
        rc = -15
      else
        rc = 0
      endif

      deallocate ( allTimes )
      deallocate ( fminutes_32 )

      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!
!BOP
!
! !ROUTINE:  GFIO_GetVarT -- Read a variable from the file with interpolation
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Gfio\_open. Unlike {\tt Get\_Var()},
!               this routine will interpolate in time if necessary.
!               If interpolation is required between two times in different
!               files, two file IDs can be passed in.
!               GFIO\_GetVarT is a wrapper to the original GFIO\_GetVar
!                          to avoild the optional cyclic argument.
!
! !INTERFACE:
!
      subroutine GFIO_GetVarT ( fid, vname, yyyymmdd, hhmmss,
     &                          im, jm, kbeg, kount, grid, rc,
     &                          fid2)
!
! !USES:
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      integer        fid2             ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
                                     
      logical         cyclic          ! whether time dimension is periodic
!
! !OUTPUT PARAMETERS:
!
      real         grid(im,jm,kount)  ! Gridded data read for this time
      integer         rc              ! Error return code:

! !REVISION HISTORY:
!
!  2004.11.08 Ravi        Wrapper for GFIO_GetvarT
!
!
!EOP


       cyclic = .false.
        call GFIO_GetVarT1 ( fid, vname, yyyymmdd, hhmmss,
     &                       im, jm, kbeg, kount, grid, rc,
     &                       cyclic, fid2 )
       return
       end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!
!BOP
!
! !ROUTINE:  GFIO_GetVarT1 -- Read a variable from the file with interpolation 
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Gfio\_open. Unlike {\tt Get\_Var()},
!               this routine will interpolate in time if necessary.
!               If interpolation is required between two times in different
!               files, two file IDs can be passed in.
!
! !INTERFACE:
!
      subroutine GFIO_GetVarT1 ( fid, vname, yyyymmdd, hhmmss,
     &                          im, jm, kbeg, kount, grid, rc, 
     &                          cyclic,fid2 )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid,fid2         ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read

      logical         cyclic          ! whether time dimension is periodic
!
! !OUTPUT PARAMETERS:
!
      real         grid(im,jm,kount)  ! Gridded data read for this time
      integer  rc        ! Error return code:
                         !  rc  = 0   all is well
                         !  rc \= 0   abnormal exit (TO DO: list these)

! !REVISION HISTORY:
!
!  1999.11.02 da Silva    Initial code. Undocumented.
!  2001.10.11 B. Yin      Added optional fid2 parameter
!
!
!EOP
!
!-------------------------------------------------------------------------

      integer begDate, begTime, incSecs, timeIndex1, timeIndex2
      integer secs, secs1, secs2, nymd1, nymd2, nhms1, nhms2
      integer err, diffdate, i, j, k 

      real    grid2(im,jm,kount)  ! workspace for interpolation
      real    alpha, amiss, gfio_getmissing

      logical tcyclic


! By default time dimension is not periodic

      tcyclic = cyclic

      rc = 0

!     Get beginning time & date.  Calculate offset seconds from start.
!     ----------------------------------------------------------------
      call GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
      if (err("GetVar: could not determine begin_date/begin_time",rc,-44) 
     &    .NE. 0) return

      secs = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

      if ( .not. tcyclic ) then
         if (yyyymmdd .LT. begDate .OR. (begDate .EQ. yyyymmdd .AND.
     .        hhmmss .LT. begTime) .or. secs .LT. 0) then
            print *, 'GFIO_GetVar: Requested time earlier than first time.'
            rc = -7
            return
         endif
      end if

!     Determine brackting times
!     -------------------------
      if ( secs >= 0 ) then
         timeIndex1 = secs/incSecs + 1
      else
         timeIndex1 = secs/incSecs
      end if
      timeIndex2 = timeIndex1 + 1
      secs1 = (timeIndex1-1) * incSecs
      secs2 = (timeIndex2-1) * incSecs
      call GetDate ( begDate, begTime, secs1, nymd1, nhms1, rc )
      call GetDate ( begDate, begTime, secs2, nymd2, nhms2, rc )

!ams  print *, 'Cyclic: ', tcyclic
!ams  print *, 'Time 1: ', secs1, nymd1, nhms1
!ams  print *, 'Time  : ', secs,  yyyymmdd, hhmmss
!ams  print *, 'Time 2: ', secs2, nymd2, nhms2
!ams  print *, 'Time Indices: ', timeIndex1, timeIndex2  
!ams  print *, 'incSecs: ', incSecs
!ams  print *, 'begDate/time: ', begDate, begTime

!     Read grids at first time with GetVar()
!     --------------------------------------
      call GFIO_GetVar1 ( fid, vname, nymd1, nhms1, 
     &                   im, jm, kbeg, kount, grid, tcyclic, rc )
      if ( rc .ne. 0 ) return    

      if ( secs1 .eq. secs ) return   ! no interpolation needed


!     Read grids at second time with GetVar()
!     ---------------------------------------
      call GFIO_GetVar1 ( fid, vname, nymd2, nhms2, 
     &                   im, jm, kbeg, kount, grid2, tcyclic, rc)
      if ( rc .ne. 0 ) then
         if ( fid /= fid2 )         
     &       call GFIO_GetVar1 ( fid2, vname, nymd2, nhms2,
     &                          im, jm, kbeg, kount, grid2, tcyclic, rc )
         
         if ( rc .ne. 0 ) return    
      end if

!     Get missing value
!     -----------------
      amiss = GFIO_GetMissing ( fid, rc )
      if ( rc .ne. 0 ) return

!     Do interpolation
!     ----------------
      alpha = float(secs - secs1)/float(secs2 - secs1)
!ams  print *, 'alpha = ', alpha
      do k = 1, kount
         do j = 1, jm
            do i = 1, im
               if ( grid(i,j,k) .ne. amiss .and. grid2(i,j,k) .ne. amiss ) then
                  grid(i,j,k) = grid(i,j,k) 
     &                        + alpha * (grid2(i,j,k) - grid(i,j,k))
               else
                  grid(i,j,k) = amiss
               end if
            end do
         end do
      end do

!     All done
!     --------
      rc = 0

      return
      end

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
!               GFIO\_GetVar, a wrapper to the original GFIO\_GetVar
!                          to avoild the optional cyclic argument.
!
! !INTERFACE:
!
      subroutine GFIO_GetVar ( fid, vname, yyyymmdd, hhmmss,
     &                         im, jm, kbeg, kount, grid, rc)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
      logical         cyclic          ! whether time dimension is periodic
      real         grid(im,jm,kount)  ! Gridded data read for this time
      integer         rc              ! Error return code:
!
!EOP

      cyclic = .false.
      call GFIO_GetVar1 ( fid, vname, yyyymmdd, hhmmss,
     &                         im, jm, kbeg, kount, grid,
     &                         cyclic,rc )

      return
      end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!
!BOP
!
! !ROUTINE:  GFIO_GetVar1 -- Read a variable from the file 
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Gfio\_open.
!
! !INTERFACE:
!
      subroutine GFIO_GetVar1 ( fid, vname, yyyymmdd, hhmmss,
     &                         im, jm, kbeg, kount, grid,
     &                         cyclic,rc)
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
      logical         cyclic          ! whether time dimension is periodic

!
! !OUTPUT PARAMETERS:
!
      real         grid(im,jm,kount)  ! Gridded data read for this time
      integer  rc        ! Error return code:
                         !  rc = 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file  
                         !  rc = -5  jm is incompatible with file  
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq


! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.07.07 Lucchesi            Combined two GetVar routines into this one.
!  1998.09.24 Lucchesi            Updated error codes.
!  1999.06.21 Lucchesi            Bug fixed.  Unable to read HDF-EOS files
!                                 because was still looking for "lon" and "lat"
!  1999.06.21 Lucchesi            Added a check for time too early.
!  1999.11.02 da Silva            Made LATS4D compatible.
!  2004.02.23 da Silva            Added cyclic option
!  2008.12.05  Kokron             Changed ncvid of a dimension to ncdid to make NetCDF4 happy
!  2009.04.07  Lucchesi           Removed assumption that dimension vars are at the top of the file.
!  2010.06.22  Lucchesi           Fixed dimension var issue introduced in 2009 that prevents reading HDF-EOS
!
!
!EOP
!-------------------------------------------------------------------------

      integer timeId, begDate, begTime, seconds, minutes
      integer corner(4), edges(4), timeIndex, timeShift, lm
      integer vid
      integer DiffDate
      integer err
      integer i,j,k
      integer incSecs

! Variables for working with dimensions

      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits 
      character*(MAXCHR) varName
      integer dimSize, dimId
      integer nDims,nvars,ngatts, dimsFound
      integer varType, index, IdentifyDim

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:)
      real*8, allocatable :: grid_64(:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NCVINQ

      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing

      integer*2, allocatable :: grid_16(:,:,:)
      integer*2 amiss_16
      real*4 amiss_32
      real*4 scale_32, offset_32

      logical tcyclic
! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

! By default time dimension is not periodic

           tcyclic = cyclic

!ams  print *, 'GetVar has cyclic ', cyclic
         
! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.
! MAXCHR is a CPP define in the gfio.h file.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'GFIO_GetVar warning: MAXNCNAM is larger than ',
     .           'dimName array size.'
      endif

! Get basic information from file.

      call ncinq (fid, nDims, nvars, ngatts, dimId, rc)
      if (err("DimInqure: ncinq failed",rc,-48) .NE. 0)return

! Subtract dimension variables from the variable count.
! Extract dimension information

      dimsFound = 0
      do i=1,nvars
        call ncvinq (fid,i,varName,varType,nvDims,vDims,nvAtts,rc)
        if (err("GFIO_GetVar1: variable inquire error",rc,-52) .NE. 0)
     .      return
        if (nvDims .EQ. 1) then
          nvars = nvars - 1
          dimId = ncdid (fid, varName, rc)
          if ( rc .ne. 0 ) then   ! Must not be a dim scale
             cycle
          endif
          dimsFound = dimsFound + 1
!         if (err("GFIO_GetVar1: ncdid failed",rc,-41) .NE. 0) return
          call ncagtc (fid, i, 'units', dimUnits, MAXCHR, rc)
          if (err("DimInqure: could not get units for dimension",rc,-53)
     .       .NE. 0) return
          call ncdinq (fid, dimId, dimName, dimSize, rc)
          if (err("DimInqure: can't get dim info",rc,-41) .NE. 0) return
          index = IdentifyDim (dimName, dimUnits)
          if ( index .EQ. 0 ) then
            if (dimSize .ne. im) then
              rc = -4
              im = dimSize
              return
            endif
          else if ( index .EQ. 1 ) then
            if (dimSize .ne. jm) then
              rc = -5
              jm = dimSize
              return
            endif
          else if ( index .EQ. 2 ) then
            if (kount .gt. dimSize) then
              rc = -3
              return
            endif
          else if ( index .EQ. 3 ) then
              timeId = dimId
              lm = dimSize
          else
            print *, 'GFIO_GetVar: Coordinate variable ',
     .               TRIM(dimName),' with units of ',TRIM(dimUnits),
     .               ' is not understood.'
            rc = -19
            return
          endif
          if ( dimsFound .eq. nDims ) exit
        endif
      enddo

! Determine NetCDF variable ID.

      vid = ncvid (fid, vname, rc)
      if (err("GetVar: variable not defined",rc,-40) .NE. 0) return
 
! Get beginning time & date.  Calculate offset seconds from start.

!ams      call ncagt (fid, timeId, 'begin_date', begDate, rc)
!ams     if (err("GetVar: missing begin_date",rc,-44) .NE. 0) return
!ams     call ncagt (fid, timeId, 'begin_time', begTime, rc)
!ams     if (err("GetVar: missing begin_time",rc,-44) .NE. 0) return

      call GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
      if (err("GetVar: could not determine begin_date/begin_time",rc,-44) 
     &    .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

! Make sure input time are valid, if time is not periodic

!ams  print *, '+++ incSecs, begDate, begTime: ', incsecs, begDate, begTime
!ams  print *, '+++ seconds, yyyymmdd, hhmmss: ', seconds, yyyymmdd, hhmmss

      if ( .not. tcyclic ) then
         if (seconds .LT. 0) then
            print *, 'GFIO_GetVar: Error code from diffdate.  Problem with',
     .           ' date/time.'
            rc = -7
            return
         endif
         if (yyyymmdd .LT. begDate .OR. (begDate .EQ. yyyymmdd .AND.
     .        hhmmss .LT. begTime) ) then
            print *, 'GFIO_GetVar: Requested time earlier than first time.'
            rc = -7
            return
         endif

      end if

      if ( MOD (seconds,60) .eq. 0 ) then
        minutes = seconds / 60
      else
        print *, 'GFIO_GetVar: Currently, times must fall on minute ',
     .           'boundaries.'
        rc = -6
        return
      endif

! Determine the time index from the offset and time increment.

!ams      call ncagt (fid, timeId, 'time_increment', timInc, rc)
!ams      if (err("GetVar: missing time increment",rc,-44) .NE. 0) return

! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)
!ams       incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'GFIO_getvar: Absolute time of ',seconds,' not ',
     .           'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Wrap time index around if time dimension is periodic

!ams  print *, '--- Time Index: ', timeIndex

      if ( tcyclic ) then
         timeShift = mod ( timeIndex, lm )
         if ( timeShift > 0 ) then
            timeIndex = timeShift 
         else 
            timeIndex = lm + timeShift
         end if
      end if

!ams  print *, '+++ Time Index, timeShift: ', timeIndex, timeShift

! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=1
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=1
      else
        corner(1)=1
        corner(2)=1
        corner(3)=kbeg
        corner(4)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=1
      endif

! Determine data type.

      call ncvinq (fid, vid, varName, type, nvDims, vDims, nvAtts, rc)
      if (err("GetVar: error in variable inquire",rc,-52) .NE. 0) return

! Read variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          call ncvgt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCDOUBLE) then               ! 64-bit
          allocate (grid_64(im,jm,kount))
          call ncvgt (fid, vid, corner, edges, grid_64, rc)
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid(i,j,k) = grid_64(i,j,k)
              enddo
            enddo
          enddo
          deallocate (grid_64)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'missing_value', amiss_16, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'fmissing_value', amiss_32, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          call ncvgt (fid, vid, corner, edges, grid_16, rc)
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid_16(i,j,k) .EQ. amiss_16 ) then
                  grid(i,j,k) = amiss_32
                else
                  grid(i,j,k) = scale_32*grid_16(i,j,k) + offset_32
                endif
              enddo
            enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NCFLOAT) then                     ! 32-bit
          allocate (grid_32(im,jm,kount))
          call ncvgt (fid, vid, corner, edges, grid_32, rc)
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid(i,j,k) = grid_32(i,j,k)
              enddo
            enddo
          enddo
          deallocate (grid_32)
        elseif (type .EQ. NCDOUBLE) then                ! 64-bit
          call ncvgt (fid, vid, corner, edges, grid, rc)
        else if (type .EQ. NCSHORT) then
          call ncagt (fid, vid, 'scale_factor', scale_32, rc)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'add_offset', offset_32, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'missing_value', amiss_16, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          call ncagt (fid, vid, 'fmissing_value', amiss_32, rc)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          call ncvgt (fid, vid, corner, edges, grid_16, rc)
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid_16(i,j,k) .EQ. amiss_16 ) then
                  grid(i,j,k) = amiss_32
                else
                  grid(i,j,k) = scale_32*grid_16(i,j,k) + offset_32
                endif
              enddo
            enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("GetVar: error reading variable",rc,-46) .NE. 0) return
 
      rc = 0
      return
      end

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
      subroutine GFIO_DimInquire (fid,im,jm,km,lm,nvars,ngatts,rc)
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer     im     ! Size of longitudinal dimension
      integer     jm     ! Size of latitudinal dimension
      integer     km     ! Size of vertical dimension
                         !   km=0 if surface-only file
      integer     lm     ! Number of times 
      integer     nvars  ! Number of variables
      integer     ngatts ! Number of global attributes
      integer     rc     ! Error return code:

                         !  rc = 0    all is well
                         !  rc = -19  unable to identify coordinate variable
                         !
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
!  1998.07.02  Lucchesi    Initial interface design.
!  1998.08.05  Lucchesi    Added "ngatts"
!  1998.09.24  Lucchesi    Revamped error codes
!  1998.12.22  Lucchesi    Added IdentifyDim and associated code
!  1999.01.04  Lucchesi    Changed variable initialization
!  2008.12.05  Kokron      Changed ncvid of a dimension to ncdid to make NetCDF4 happy
!  2009.04.07  Lucchesi    Removed assumption that dimension vars are at the top of the file.
!  2010.06.22  Lucchesi    Fixed dimension var issue introduced in 2009 that prevents reading HDF-EOS
!  2011.10.11  Lucchesi/   Adjusted the computation of "nvars" to accurately account for two sets of
!              Govindarahu dimension variables in HDF-EOS files.  (This algorithm fails if 1-D science variables 
!                          are part of the file).
!
!EOP
!-------------------------------------------------------------------------

      integer dimId, i
      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits
      character*(MAXCHR) vname
      integer dimSize
      integer nDims,nvars0
      integer err
      logical surfaceOnly
      integer IdentifyDim, index
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts, dimsFound

! Initialize variables

      surfaceOnly = .FALSE.

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

! Check FID here.

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.
! MAXCHR is a CPP define in the gfio.h file.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'GFIO_DimInquire warning: MAXNCNAM is larger than ',
     .           'dimName array size.'
      endif

! Get basic information from file.
 
    
      call ncinq (fid, nDims, nvars0, ngatts, dimId, rc)
      if (err("DimInquire: ncinq failed",rc,-48) .NE. 0)return

      if (nDims .EQ. 3) then
        surfaceOnly = .TRUE.
      endif

! Subtract dimension variables from the variable count.
! Extract dimension information

      dimsFound=0
      nvars = nvars0
      do i=1,nvars0
        call ncvinq (fid,i,vname,varType,nvDims,vDims,nvAtts,rc)
        if (err("DimInquire: variable inquire error",rc,-52) .NE. 0) 
     .      return
        if (nvDims .EQ. 1) then     ! If one dimensional SDS, it must be a dimension scale.
          nvars = nvars - 1
          dimId = ncdid (fid, vname, rc)
!         if (err("DimInquire: ncdid failed",rc,-41) .NE. 0) return
          if ( rc .ne. 0 ) then  ! Must not be a dim scale.
             cycle
          endif
          dimsFound = dimsFound + 1
          call ncdinq (fid, dimId, dimName, dimSize, rc)  
          if (err("DimInqure: can't get dim info",rc,-41) .NE. 0) return
!         call ncagtc (fid, dimId, 'units', dimUnits, MAXCHR, rc)
          call ncagtc (fid, i, 'units', dimUnits, MAXCHR, rc)
          if (err("DimInquire: could not get units for dimension",rc,-53)
     .         .NE. 0) cycle
          index = IdentifyDim (dimName, dimUnits)
          if ( index .EQ. 0 ) then
            im = dimSize
          else if ( index .EQ. 1 ) then
            jm = dimSize
          else if ( index .EQ. 2 ) then
            km = dimSize
          else if ( index .EQ. 3 ) then
            lm = dimSize
          else
            print *, 'GFIO_DimInquire: Coordinate variable ',
     .               TRIM(dimName),' with units of ',TRIM(dimUnits),
     .               ' is not understood.'
            rc = -19
            return
          endif
!         if ( dimsFound .eq. nDims ) exit
        endif
      enddo

      if (surfaceOnly) then
        km=0
      endif

      rc=0
      return
      end

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
      subroutine GFIO_Inquire ( fid, im, jm, km, lm, nvars,
     &                        title, source, contact, amiss,
     &                        lon, lat, levs, levunits, 
     &                        yyyymmdd, hhmmss, timinc, 
     &                        vname, vtitle, vunits, kmvar,
     &                        valid_range , packing_range, rc)
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
      integer        fid            ! File handle from GFIO_open
!
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer         im            ! size of longitudinal dimension
      integer         jm            ! size of latitudinal  dimension
      integer         km            ! size of vertical     dimension
      integer         lm            ! size of time         dimension
                                    ! On input, (im,jm,km,lm) contains the
                                    !  size of arrays (lon,lat,lev,yyyymmdd)
                                    !  as declared in the calling program.
                                    ! On output, (im,jm,km,lm) contains the 
                                    !  size of the coordinate variables
                                    !  (lon,lat,lev,yyyymmdd) on file.
      integer         nvars         ! number of variables on file

!
! !OUTPUT PARAMETERS:
!
      character*(*)   title         ! Title of the data set
      character*(*)   source        ! Where it came from
      character*(*)   contact       ! Who to contact about the data set
      real            amiss         ! Missing value 

                                    ! ------- Dimension Metadata -------
      real            lon(im)       ! longitude of center of gridbox in
                                    ! degrees east of Greenwich (can be
                                    ! -180 -> 180 or 0 -> 360)
      real            lat(jm)       ! latitude of center of gridbox in
                                    ! degrees north of equator
      real            levs(km)      ! Level (units given by levunits) of
                                    !   center of gridbox
      character*(*)   levunits      ! units of level dimension, e.g.,
                                    !   "hPa", "sigma_level"
      integer        yyyymmdd(lm)   ! Year-month-day on file 
      integer          hhmmss(lm)   ! Hour-minute-second on file 
      integer            timinc     ! Time increment. 

                                    ! ------- Variable Metadata -------
      character*(*)   vname(nvars)  ! variable short name, e.g., "hght"
      character*(*)   vtitle(nvars) ! variable long name, e.g.,
                                    !   "Geopotential Height"
      character*(*)   vunits(nvars) ! variable units, e.g., "meter/second"
      integer         kmvar(nvars)  ! number of levels for variable; it can
                                    !  either be 0 (2-D fields) or equal to km
      real    valid_range(2,nvars)  ! Variable valid range; set to
                                    !   "amiss" if not known.
 
                                    ! ------ Packing Metadata ----
      real  packing_range(2,nvars)  ! Variable packing range used
                                    !  for 16-bit packing. If packing was not
                                    !  used then returned values will be amiss. 
                                    ! NOTE: all unpacking is done transparently
                                    !       by GFIO_GetVar(). 
    
      integer    rc      ! Error return code:
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
                         !  rc = -54  error from ncainq

! !FUTURE ENHANCEMENT:
!                   Next release should include a flag for precision.
!
! !REVISION HISTORY: 
!
!  1998.07.02   Lucchesi   Initial interface design.
!  1998.07.17   Lucchesi   Initial coding.
!  1998.10.09   Lucchesi   Restructured return codes.
!  1998.12.24   Lucchesi   Modified to read non-GFIO files.
!  1999.01.04   Lucchesi   Changed variable initialization
!  1999.07.13   Lucchesi   Changes for REAL or INT time dimension
!  1999.11.02   da Silva   Made LATS4D compatible.
!  2000.10.23   Lucchesi   Updated calculation of time increment after
!                          fixing bugs in GetBegDateTime.
!  2001.04.23   da Silva   Fixed timInc bug for RUC
!  2002.12.24   Takacs/RT  Bug fix in calc of timInc (was dividing by 0)
!  2008.12.05   Kokron     Changed ncvid of a dimension to ncdid to make NetCDF4 happy
!  2009.04.07   Lucchesi   Removed assumption that dimension vars are at the top of the file.
!  2010.06.22   Lucchesi   Fixed dimension var issue introduced in 2009 that prevents reading HDF-EOS
!
!
!EOP
!-------------------------------------------------------------------------

! Local Variables

      integer timeType, dimType
      integer timeId, latId, lonId, levId, dimId, incSecs
      integer nDims, recdim, ngatts, seconds
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts
      integer yyyymmdd_beg, hhmmss_beg, hour, min
      integer start1D, minutes(lm)
      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits
      character*(MAXCHR) vnameTemp
      integer IdentifyDim, index, dimsFound
      integer dimSize
      integer i
      logical surfaceOnly
      logical noTimeInfo
      integer attType, attLen
      integer fV                 ! found variables - excludes dimension vars
      integer allVars            ! all variables - includes dimension vars

      ! REAL*4 variables for 32 bit input from netCDF file.

      real*4 fminutes_32(lm)
      real*8 fminutes_64(lm)
      real*4 lon_32(im), lat_32(jm), levs_32(km)
      real*8 lon_64(im), lat_64(jm), levs_64(km)
      real*4 amiss_32
      real*4 pRange_32(2,nvars),vRange_32(2,nvars)
      integer err
      logical is_gfio

! Initialize variables

      fV = 0
      surfaceOnly = .FALSE.
      noTimeInfo = .FALSE.
      is_gfio = .true.       ! start assuming file was written by GFIO

! Make NetCDF errors non-fatal, and DO NOT issue warning messages.

      call ncpopt(2) ! Give error messages, but don't die

! Check length of vname string

      if (LEN(vname(1)) .lt. MAXNCNAM) then
        print *,'GFIO_Inquire: length of vname array must be at least ',
     .            MAXNCNAM,' bytes.'
        rc = -20
        return
      endif

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.
! MAXCHR is a CPP define in the gfio.h file.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'GFIO_Inquire warning: MAXNCNAM is larger than ',
     .           'dimName array size.'
      endif

! Get basic information from the file

      call ncinq (fid,nDims,allVars,ngatts,recdim,rc)
      if (err("Inqure: ncinq failed",rc,-48) .NE. 0) return

      if (nDims .EQ. 3) then
        surfaceOnly = .TRUE.
      endif

! Extract dimension information and check against inputs

      dimsFound = 0
      do i=1,allVars
        call ncvinq (fid,i,vnameTemp,varType,nvDims,vDims,nvAtts,rc)
        if (err("GFIO_Inquire: variable inquire error",rc,-52) .NE. 0)
     .      return
        if (nvDims .EQ. 1) then
           dimId = ncdid (fid, vnameTemp, rc)
           if ( rc .ne. 0 ) then   ! Must not be a dim scale.
             cycle
           endif
           dimsFound = dimsFound + 1
!          if (err("Inquire: ncdid failed",rc,-40) .NE. 0) return
           call ncdinq (fid, dimId, dimName, dimSize, rc)
           if (err("Inqure: can't get dim info",rc,-41) .NE. 0) return
!          call ncagtc (fid, dimId, 'units', dimUnits, MAXCHR, rc)
           call ncagtc (fid, i, 'units', dimUnits, MAXCHR, rc)
           if (err("Inqure: could not get units for dimension",rc,-53)
     .         .NE. 0) return
           index = IdentifyDim (dimName, dimUnits)
           if ( index .EQ. 0 ) then
              if (dimSize .ne. im) then
                rc = -4
                im = dimSize
                return
              else 
                lonId = dimId
                lonId = i
              endif
           else if ( index .EQ. 1 ) then
             if (dimSize .ne. jm) then
               rc = -5
               jm = dimSize
               return
             else
               latId = dimId
               latId = i
             endif
           else if ( index .EQ. 2 ) then
             if (km .ne. dimSize) then
               rc = -3
               km = dimSize
               return
             else
               levId = dimId
               levId = i
             endif
           else if ( index .EQ. 3 ) then
              if (lm .ne. dimSize) then
               rc = -8
               lm = dimSize
               return
             else
               timeId = dimId
               timeId = i
             endif
           else
             print *, 'GFIO_Inquire: Coordinate variable ',
     .                TRIM(dimName),' with units of ',TRIM(dimUnits),
     .                ' is not understood.'
             rc = -19
             return
           endif
           if ( dimsFound .eq. nDims ) exit
        endif
      enddo


      start1D=1

! Get dimension values (coordinates)

!     Dimension values in a native GFIO file are 32 bit.
!     However, LATS4d uses double for coordinate variables.

      call ncvinq (fid,lonId,dimName,dimType,nvDims,vDims,nvAtts,rc)
      if ( dimType .eq. NCFLOAT ) then
         call ncvgt (fid,lonId,start1D,im,lon_32,rc)
         if (err("Inquire: error reading 32-bit lons",rc,-49) .LT. 0) return
         do i=1,im
            lon(i)=lon_32(i)
         enddo
      else if ( dimType .eq. NCDOUBLE ) then
         is_gfio = .false.  ! this is not a GFIO file, probably LATS4D
         call ncvgt (fid,lonId,start1D,im,lon_64,rc)
         if (err("Inquire: error reading 64-bit lons",rc,-49) .LT. 0) return
         do i=1,im
            lon(i)=lon_64(i)
         enddo
      else
         if (err("Inquire: unsupported lon type",-1,-49) .LT. 0) return
      endif

      call ncvinq (fid,latId,dimName,dimType,nvDims,vDims,nvAtts,rc)
      if ( dimType .eq. NCFLOAT ) then
         call ncvgt (fid,latId,start1D,jm,lat_32,rc)
         if (err("Inquire: error reading 32-bit lats",rc,-49) .LT. 0) return
         do i=1,jm
            lat(i)=lat_32(i)
         enddo
      else  if ( dimType .eq. NCDOUBLE ) then
         call ncvgt (fid,latId,start1D,jm,lat_64,rc)
         if (err("Inquire: error reading 32-bit lats",rc,-49) .LT. 0) return
         do i=1,jm
            lat(i)=lat_64(i)
         enddo
      else
         if (err("Inquire: unsupported lat type",-1,-49) .LT. 0) return
      endif


      if (.NOT. surfaceOnly) then
         call ncvinq (fid,levId,dimName,dimType,nvDims,vDims,nvAtts,rc)
         if ( dimType .eq. NCFLOAT ) then
            call ncvgt (fid,levId,start1D,km,levs_32,rc)
            if (err("Inquire: error reading 32-bit levs",rc,-49) .LT. 0) return
            do i=1,km
               levs(i)=levs_32(i)
            enddo
         else  if ( dimType .eq. NCDOUBLE ) then
            call ncvgt (fid,levId,start1D,km,levs_64,rc)
            if (err("Inquire: error reading 32-bit levs",rc,-49) .LT. 0) return
            do i=1,km
               levs(i)=levs_64(i)
            enddo
         else
            if (err("Inquire: unsupported lev type",-1,-49) .LT. 0) return
         endif
      end if

      ! Depending on the version of GFIO used to write the file, the Time 
      ! dimension variable can either be floating point or integer.
      ! Note: LATS4d uses double for coordinate variables.

      call ncvinq (fid,timeId,dimName,timeType,nvDims,vDims,nvAtts,rc)
      if (timeType .EQ. NCFLOAT) then
        call ncvgt (fid,timeId,start1D,lm,fminutes_32,rc) 
        do i=1,lm
          minutes(i) = INT (fminutes_32(i))
        enddo
      else if (timeType .EQ. NCDOUBLE) then
        call ncvgt (fid,timeId,start1D,lm,fminutes_64,rc) 
        do i=1,lm
          minutes(i) = INT (fminutes_64(i))
        enddo
      else if (timeType .EQ. NCLONG) then
        call ncvgt (fid,timeId,start1D,lm,minutes,rc) 
      endif
      if (err("Inquire: error reading times",rc,-49) .LT. 0) return


! Get dimension attributes.

      if (.NOT. surfaceOnly) then
        call ncagtc (fid,levid,'units',levunits,LEN(levunits),rc)
        if (err("Inquire: error reading lev units",rc,-50) .LT. 0) 
     .      return
      endif


      noTimeInfo = .FALSE.
!ams      call ncagt (fid,timeid,'time_increment',timinc,rc)
!ams      if (rc .NE. 0) then
!ams         print *, 'GFIO_Inquire: Warning. Time increment not found.'
!ams      endif
      
!ams      call ncagt (fid,timeid,'begin_date',yyyymmdd_beg,rc)
!ams      if (rc .NE. 0) then
!ams         print *, 'GFIO_Inquire: Warning. begin_date not found.',
!ams     .            ' No time/date information will be returned.'
!ams         noTimeInfo = .TRUE.
!ams      endif

!ams      call ncagt (fid,timeid,'begin_time',hhmmss_beg,rc)
!ams      if (rc .NE. 0) then
!ams         print *, 'GFIO_Inquire: Warning. begin_time not found.',
!ams     .            ' No time/date information will be returned.'
!ams         noTimeInfo = .TRUE.
!ams      endif

          call GetBegDateTime ( fid, yyyymmdd_beg, hhmmss_beg, incSecs, rc )
          if ( rc .ne. 0 ) noTimeInfo = .TRUE.

!ams          print *, '--- incSecs, begDate, begTime: ', incsecs, yyyymmdd_beg, hhmmss_beg

! Calculate and load YYYYMMDD and HHMMSS values.
! New algorithm for calculating increment time was added.  The new method takes advantage 
! of information returned by GetBegDateTime, which was added to GFIO by A. da Silva. (RL, Oct2000)

      if (.NOT. noTimeInfo) then
        if ( lm .ge. 1 ) then   !ams: changed lm.gt.1 to lm.ge.1
           hour = incSecs/3600
           if (hour == 0) hour=1
           min = mod(incSecs,3600*hour)/60
!_RT       timInc = hour*10000 + min*100
           timInc = incSecs/3600*10000 + mod(incSecs,3600)/60*100 + mod(incSecs,60)
        end if
!RL     hour = incTime/60
!RL     min  = mod(incTime,60)
!RL     timInc = hour*10000 + min*100

        do i=1,lm
!ams           call GetDate (yyyymmdd_beg,hhmmss_beg,minutes(i)*60,
!ams     .                   yyyymmdd(i),hhmmss(i),rc)
!RL        seconds = (minutes(i) - minutes(1)) * incSecs / incTime
           seconds = incSecs * (i-1)
           call GetDate (yyyymmdd_beg,hhmmss_beg, seconds,
     .                   yyyymmdd(i),hhmmss(i),rc)
           if (rc .LT. 0) then
             print *, "GFIO_Inquire: error in getdate"
             rc = -14
             return
           endif
!ams           print *, '--- index,   yyyymmdd, hhmmss: ', i, yyyymmdd(i), hhmmss(i)

        enddo
      else
        timInc = 000100   ! default: 1 minute
      endif

! Get global attributes for native GFIO files only

      if ( is_gfio ) then

         call ncagtc (fid,NCGLOBAL,'Title',title,LEN(title),rc)
         if (rc .NE. 0) then
            print *, 'GFIO_Inquire: Warning. Global attribute Title ',
     .           'not found.'
         endif
         call ncagtc (fid,NCGLOBAL,'Source',source,LEN(source),rc)
         if (rc .NE. 0) then
            print *, 'GFIO_Inquire: Warning. Global attribute Title ',
     .           'not found.'
         endif
         call ncagtc (fid,NCGLOBAL,'Contact',contact,LEN(contact),rc)
         if (rc .NE. 0) then
            print *, 'GFIO_Inquire: Warning. Global attribute Title ',
     .           'not found.'
         endif

      else

         Title =   'Unknown'
         Source =  'Unknown'
         Contact = 'Unknown'

      end if

! Get missing value.  GFIO assumes this to be the same for all variables.
! The check for "missing_value" if "fmissing_value" fails is for backward
! compatability with files created by the pre-release of GFIO.

      do i= 1, allVars
        call ncvinq (fid,i,vnameTemp,varType,nvDims,vDims,nvAtts,rc)
        if (err("Inquire: variable inquire error",rc,-52) .NE. 0) return
        if (nvDims .EQ. 1) then   ! coord variable
          cycle
        else                      ! noon-coord variable
           if ( is_gfio ) then
              call ncagt (fid, i,'fmissing_value',amiss_32,rc)
           else
              rc = -1
           end if
           if (rc .NE. 0) then
               call ncainq (fid, i, 'missing_value', attType, attLen, rc)
              if (rc.eq.0 .and. attType .EQ. NCFLOAT) then

!RL 04/15/2012   call ncagt (fid, allVars, 'missing_value', amiss_32, rc)  BUG FIXED
                 call ncagt (fid, i, 'missing_value', amiss_32, rc)
                 
                 if (rc .ne. 0) call ncagt (fid, 1, 'missing_value', amiss_32, rc)
                 if (err("Inquire: error getting missing value",rc,-53) 
     .                .NE. 0) return
              else
                    print *, 
     .              'GFIO_Inquire: Cannot find missing value, assuming 1E+15'
                    amiss_32 = 1.0E+15
              end if
           endif
           exit    ! just check first non-ccordinate variable
        endif
      end do
      amiss = amiss_32

! Get variable information.

      do i=1,allVars
        call ncvinq (fid,i,vnameTemp,varType,nvDims,vDims,nvAtts,rc)
        if (err("Inquire: variable inquire error",rc,-52) .NE. 0) return
        if (nvDims .EQ. 1) then
          cycle
        else
          fV = fV + 1
          vname(fV) = vnameTemp
        endif
        if (nvDims .EQ. 3) then
          kmvar(fV)=0
        else
          kmvar(fV)=km
        endif
        call ncagtc (fid,i,'long_name',vtitle(fV),LEN(vtitle(fV)), rc)
        if (err("Inquire: variable attribute error",rc,-53) .NE. 0) 
     .     return
        call ncagtc (fid,i,'units',vunits(fV),LEN(vunits(fV)),rc)
        if ( rc .NE. 0 ) then 
           !!! print *, "Inquire: Cannot find variable units attribute"
        endif
        
        ! Get packing ranges and valid ranges.  Errors are not fatal 
        ! since these attributes are optional.

        call ncagt (fid, i, 'packmin', pRange_32(1,fV), rc)
        if (rc .NE. 0) then
          packing_range(1,fV) = amiss
        else
          packing_range(1,fV) = pRange_32(1,fV)
        endif
        call ncagt (fid, i, 'packmax', pRange_32(2,fV), rc)
        if (rc .NE. 0) then
          packing_range(2,fV) = amiss
        else
          packing_range(2,fV) = pRange_32(2,fV)
        endif
        call ncagt (fid, i, 'vmin', vRange_32(1,fV), rc)
        if (rc .NE. 0) then
          valid_range(1,fV) = amiss
        else
          valid_range(1,fV) = vRange_32(1,fV)
        endif
        call ncagt (fid, i, 'vmax', vRange_32(2,fV), rc)
        if (rc .NE. 0) then
          valid_range(2,fV) = amiss
        else
          valid_range(2,fV) = vRange_32(2,fV)
        endif
         
      enddo

      if (fV .NE. nvars) then
        rc = -9
        nvars = fV
        return
      endif

      rc=0
      call ncpopt(__NCVERBOS__)  ! back to chatty netcdf

      return
      end

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
      subroutine GFIO_Close ( fid, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:

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

      integer err

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

      call ncclos (fid, rc)
      if (err("Close: error closing file",rc,-54) .NE. 0) return

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_PutIntAtt -- Write a user-defined integer attribute
!
! !DESCRIPTION: This routine allows the user to define an integer
!               attribute in an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_PutIntAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      integer        buf(count) ! Buffer with integer values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from ncaptc/ncapt (global attribute)
                         !   rc = -55  error from ncredf (enter define mode)
                         !   rc = -56  error from ncedf (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!
!EOP
!-------------------------------------------------------------------------

      integer*4 dummy32
      integer*8 dummy64
      integer i

      integer*4, allocatable :: buf32(:)
      integer*8, allocatable :: buf64(:)
      integer err

      call ncredf ( fid, rc )
      if (err("PutIntAtt: could not enter define mode",rc,-55) .NE. 0)
     .    return

      if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then     ! -i4
        call ncapt ( fid, NCGLOBAL, name, NCLONG, count, buf, rc ) ! 32-bit out

      else if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then  ! -i4
        allocate ( buf64(count) )                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        call ncapt ( fid, NCGLOBAL, name, NCDOUBLE, count, buf64, rc )
        deallocate (buf64)

      else if  (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then  ! -i8
        allocate ( buf32(count) )                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        call ncapt ( fid, NCGLOBAL, name, NCLONG, count, buf32, rc )
        deallocate (buf32)

      else if (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then   ! -i8
        call ncapt ( fid, NCGLOBAL, name, NCDOUBLE, count, buf, rc ) ! 64-bit out

      else 
        rc = -12
        return
      endif
      if (err("PutIntAtt: error writing attribute",rc,-36) .NE. 0)
     .    return

      call ncendf ( fid, rc )
      if (err("PutIntAtt: could not exit define mode",rc,-56) .NE. 0)
     .    return

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_PutRealAtt -- Write a user-defined real attribute
!
! !DESCRIPTION: This routine allows the user to define a real
!               attribute in an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_PutRealAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      real           buf(count) ! Buffer with real values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from ncaptc/ncapt (global attribute)
                         !   rc = -55  error from ncredf (enter define mode)
                         !   rc = -56  error from ncedf (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!
!EOP
!-------------------------------------------------------------------------

      real*4 dummy32
      real*8 dummy64
      real r
      integer i
      real*4, allocatable :: buf32(:)
      real*8, allocatable :: buf64(:)
      integer err

      call ncredf ( fid, rc )
      if (err("PutRealAtt: could not enter define mode",rc,-55) .NE. 0)
     .    return

      if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 0) then        ! -r4
        call ncapt ( fid, NCGLOBAL, name, NCFLOAT, count, buf, rc ) ! 32-bit out

      else if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 1) then  ! -r4
        allocate (buf64(count))                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        call ncapt ( fid, NCGLOBAL, name, NCDOUBLE, count, buf64, rc )
        deallocate (buf64)

      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 0) then  ! -r8
        allocate (buf32(count))                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        call ncapt ( fid, NCGLOBAL, name, NCFLOAT, count, buf32, rc )
        deallocate (buf32)
       
      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 1) then    ! -r8
        call ncapt ( fid, NCGLOBAL, name, NCDOUBLE, count, buf, rc ) ! 64-bit out
 
      else
        rc = -12
        return
      endif
      if (err("PutRealAtt: error writing attribute",rc,-36) .NE. 0)
     .    return

      call ncendf ( fid, rc )
      if (err("PutRealAtt: could not exit define mode",rc,-56) .NE. 0)
     .    return

      rc = 0
      return
      end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_PutCharAtt -- Write a user-defined character attribute
!
! !DESCRIPTION: This routine allows the user to define a character (string)
!               attribute in an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_PutCharAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of characters to write
      character      buf(count) ! Buffer containing string
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from ncaptc/ncapt (global attribute)
                         !   rc = -55  error from ncredf (enter define mode)
                         !   rc = -56  error from ncedf (exit define mode)
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      integer err

      call ncredf ( fid, rc )
      if (err("PutCharAtt: could not enter define mode",rc,-55) .NE. 0) 
     .    return
      call ncaptc ( fid, NCGLOBAL, name, NCCHAR, count, buf, rc )
      if (err("PutCharAtt: error writing attribute",rc,-36) .NE. 0) 
     .    return
      call ncendf ( fid, rc )
      if (err("PutCharAtt: could not exit define mode",rc,-56) .NE. 0) 
     .    return

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetAttNames -- Get global attribute names
!
! !DESCRIPTION: This routine allows the user to get the names of
!               global attributes.
!
! !INTERFACE:
!
      subroutine GFIO_GetAttNames ( fid, ngatts, aname, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer     ngatts        ! Expected number of attributes (input)
                                ! Actual number of attributes (output if rc=-2)
!
! !OUTPUT PARAMETERS:
!
      character*(*)  aname(ngatts)  ! Array of attribute names
      integer   rc       ! Error return code:
                         !  rc =  0  all is well
                         !  rc = -10  ngatts is incompatible with file
                         !  rc = -11  character string not long enough
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -48  error from ncinq
                         !   rc = -57  error from ncanam

! !REVISION HISTORY:
!
!  1998.08.05  Lucchesi           Initial interface design.
!  1998.08.05  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      integer ngattsFile, i
      integer nDims,dimSize,recDim 
      integer err

! Make NetCDF errors non-fatal, but issue warning messages.

      call ncpopt(__NCVERBOS__)

! Check number of attributes against file

      call ncinq (fid,nDims,dimSize,ngattsFile,recdim,rc)
      if (err("GetAttNames: ncinq failed",rc,-48) .NE. 0) return
      if (ngattsFile .NE. ngatts) then
        rc = -10
        ngatts = ngattsFile
        return
      endif

! Check length of aname string

      if (LEN(aname(1)) .lt. MAXNCNAM) then
        print *,'GFIO_GetAttNames: length of aname array must be at ',
     .          'least ',MAXNCNAM,' bytes.'
        rc = -11
        return
      endif

! Read global attribute names

      do i=1,ngatts
        call ncanam (fid, NCGLOBAL, i, aname(i), rc)
        if (err("GetAttNames: error reading attribute name",rc,-57) 
     .      .NE. 0) return
      enddo

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_AttInquire -- Get information about an attribute
!
! !DESCRIPTION: This routine allows the user to get information about
!               a global attribute of an open GFIO file.  This is most
!               useful for determining the number of values stored in an
!               attribute.
!
! !INTERFACE:
!
      subroutine GFIO_AttInquire ( fid, name, type, count, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !OUTPUT PARAMETERS:
!
      integer type       ! Code for attribute type
                         !   0 = integer
                         !   1 = real
                         !   2 = character
                         !   3 = 64-bit real
                         !   4 = 64-bit integer
                         !  -1 = other
      integer count      ! Number of items (length of array)
      integer rc         ! Error return code:
                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -58  error from ncainq

!
! !NOTES:  The returned integer "type" for 64-bit integer is not supported
!          in the current implementation of netCDF/HDF.  When a user writes a
!          64-bit integer attribute using PutIntAtt, it is actually saved as
!          a 64-bit real by the HDF library.  Thus, upon reading the attribute, 
!          there is no way for HDF/GFIO to distinguish it from a REAL number.  
!          The user must realize this variable is really an integer and call 
!          GetIntAtt to read it.  Even for a 64-bit integer, type=4 will never
!          be returned unless there are changed to HDF/netCDF.
!
!
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error codes, added type assignment.
!
!EOP
!-------------------------------------------------------------------------

      integer nctype
      integer err

      call ncainq (fid, NCGLOBAL, name, nctype, count, rc)
      if (err("AttInquire: error reading attribute info",rc,-58)
     .      .NE. 0) return
      if (nctype .EQ. NCLONG) then
        type = 0
      elseif (nctype .EQ. NCFLOAT) then
        type = 1
      elseif (nctype .EQ. NCCHAR) then
        type = 2
      elseif (nctype .EQ. NCDOUBLE) then
        type = 3
      else
        type = -1
      endif

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetIntAtt -- Read a user-defined integer attribute
!
! !DESCRIPTION: This routine allows the user to read an integer 
!               attribute from an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_GetIntAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct count of this attribute
!
! !OUTPUT PARAMETERS:
!
      integer   buf(count) ! Buffer with integer values
      integer   rc         ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !   rc = -12  error determining default precision
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from ncaptc/ncapt (global attribute)
                           !   rc = -51  error from ncagtc/ncagt (global attribute)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.  Added 64-bit support.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      integer err, i
      integer*4 dummy32
      integer*8 dummy64
      integer*4, allocatable :: buf32(:)
      integer*8, allocatable :: buf64(:)

      call ncainq (fid, NCGLOBAL, name, type, length, rc)
      if (err("GetIntAtt: error reading attribute info",rc,-58)
     .      .NE. 0) return

      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif

      if ( type .NE. NCLONG .AND. type .NE. NCDOUBLE) then
        rc = -2
        return
      endif
      if ( HUGE(dummy32) .EQ. HUGE(i)) then
        if ( type .EQ. NCLONG ) then          ! -i4 32bit
          call ncagt ( fid, NCGLOBAL, name, buf, rc )
        else            ! type .EQ. NCDOUBLE
          allocate (buf64(count))             ! -i4 64bit
          call ncagt ( fid, NCGLOBAL, name, buf64, rc )
          do i=1,count
            buf(i) = buf64(i)
          enddo
          deallocate (buf64)
        endif
      else if (HUGE(dummy64) .EQ. HUGE(i)) then
        if ( type .EQ. NCLONG ) then
          allocate (buf32(count))             ! -i8 32bit
          call ncagt ( fid, NCGLOBAL, name, buf32, rc )
          do i=1,count
            buf(i) = buf32(i)
          enddo
          deallocate (buf32)
        else            ! type .EQ. NCDOUBLE
          call ncagt ( fid, NCGLOBAL, name, buf, rc )  ! -i8 64bit
        endif
      else
        rc = -12
        return
      endif
      if (err("GetIntAtt: error reading attribute value",rc,-51)
     .      .NE. 0) return

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetRealAtt -- Read a user-defined real attribute
!
! !DESCRIPTION: This routine allows the user to read a real
!               attribute from an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_GetRealAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct number of attributes
!
! !OUTPUT PARAMETERS:
!
      real     buf(count)  ! Buffer with real values
      integer  rc          ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !   rc = -12  error determining default precision
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from ncaptc/ncapt (global attribute)
                           !   rc = -51  error from ncagtc/ncagt (global attribute)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.  Added 64-bit support.
!  1999.08.23  Lucchesi           Changed .OR. to .AND.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      integer err
      real r
      integer i
      real*4 dummy32
      real*8 dummy64
      real*4, allocatable :: buf32(:)
      real*8, allocatable :: buf64(:)

      call ncainq (fid, NCGLOBAL, name, type, length, rc)
      if (err("GetRealAtt: error reading attribute info",rc,-58)
     .      .NE. 0) return

      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif
      if ( type .NE. NCFLOAT .AND. type .NE. NCDOUBLE) then
        rc = -2
        return
      endif

      if ( HUGE(dummy32) .EQ. HUGE(r)) then
        if ( type .EQ. NCFLOAT ) then         ! -r4 32bit
          call ncagt ( fid, NCGLOBAL, name, buf, rc )
        else            ! type .EQ. NCDOUBLE
          allocate (buf64(count))             ! -r4 64bit
          call ncagt ( fid, NCGLOBAL, name, buf64, rc )
          do i=1,count
            buf(i) = buf64(i)
          enddo
          deallocate (buf64)
        endif
      else if (HUGE(dummy64) .EQ. HUGE(r)) then
        if ( type .EQ. NCFLOAT ) then
          allocate (buf32(count))             ! -r8 32bit
          call ncagt ( fid, NCGLOBAL, name, buf32, rc )
          do i=1,count
            buf(i) = buf32(i)
          enddo
          deallocate (buf32)
        else            ! type .EQ. NCDOUBLE
          call ncagt ( fid, NCGLOBAL, name, buf, rc )  ! -r8 64bit
        endif
      else
        rc = -12
        return
      endif
      if (err("GetRealAtt: error reading attribute value",rc,-51)
     .      .NE. 0) return

      rc = 0
      return
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetCharAtt -- Read a user-defined character attribute
!
! !DESCRIPTION: This routine allows the user to read a character
!               attribute from an open GFIO file.
!
! !INTERFACE:
!
      subroutine GFIO_GetCharAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct number of attributes
!
! !OUTPUT PARAMETERS:
!
      character buf(count) ! Buffer with character values
      integer   rc         ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from ncaptc/ncapt (global attribute)
                           !   rc = -51  error from ncagtc/ncagt (global attribute)
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      integer err

      call ncainq (fid, NCGLOBAL, name, type, length, rc)
      if (err("GetCharAtt: error reading attribute info",rc,-58)
     .      .NE. 0) return
      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif
      if ( type .NE. NCCHAR) then
        rc = -2
        return
      endif

      call ncagtc ( fid, NCGLOBAL, name, buf, count, rc )
      if (err("GetCharAtt: error reading attribute value",rc,-51)
     .      .NE. 0) return

      rc = 0
      return
      end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 610.1                !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetVarUnits --- Read variable attribute 'units'
!
! !DESCRIPTION: Read variable attribute 'units'
!
! !INTERFACE:

      subroutine GFIO_GetVarUnits ( fid, name, units, rc )

!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:

      integer          fid      ! file id
      character(len=*) name     ! variable name

!
! !OUTPUT PARAMETERS:
!
      character(len=*) units    ! variable units
      integer          rc       ! error code

!
! !REVISION HISTORY:
!
!  2013.07.05  Darmenov, A.  Initial code.
!
!EOP
!-------------------------------------------------------------------------

! Local 
      integer vid
      integer attType, attLen
      integer err

      
      call ncpopt(0)

! Initialize units
      units = 'unknown'

! Determine NetCDF variable ID
      vid = ncvid (fid, name, rc)
      if (err("GFIO_GetVarUnits: variable not defined",rc,-40) .ne. 0)
     .       return
      
! Determine the type of attribute units
      call ncainq (fid, vid, 'units', attType, attLen, rc)

      if (err("GFIO_GetVarUnits: could not get attribute 'units'",
     .        rc,-54) .ne. 0) return

! Read the attribute 'units'
      if (attType .eq. NCCHAR) then
          call ncagtc (fid, vid, 'units', units, len(units), rc)

          if (err("GFIO_GetVarUnits: could not get attribute 'units'",
     .        rc,-53) .NE. 0) return
      else
          if (err("GFIO_GetVarUnits: attribute 'units' is not " //
     .            "of type NCCHAR", rc,-54) .ne. 0) return
      end if

      rc = 0
      return
      end
      
