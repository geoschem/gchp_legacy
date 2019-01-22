        subroutine GFIO_CreateCF ( fname, title, source, contact, amiss,
     &                         im, jm, km, lon, lat, levs, levunits, 
     &                         yyyymmdd_beg, hhmmss_beg, timinc,
     &                         nvars, vname, vtitle, vunits, kmvar,
     &                         valid_range, packing_range, prec,
     &                         fid, coordinates, twoDimLat, lat2, lon2, rc )
        
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
      character*(*) coordinates(*) ! attribute for CF compliance
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

! ALT: Additional argument for CF
      logical     twoDimLat ! indicator if "true" two dimentional lat/lon are used 
      real*8        lat2(im,jm)
      real*8        lon2(im,jm)

! !REVISION HISTORY: 
!
!  2010.09.15  Nadeau             Wrapper to create NetCDF4/HDF5 file

        nf_kind=1
            call GFIO_CreateCF1 ( fname, title, source, contact, amiss,
     &       im, jm, km, lon, lat, levs, levunits, 
     &       yyyymmdd_beg, hhmmss_beg, timinc,
     &       nvars, vname, vtitle, vunits, kmvar,
     &       valid_range, packing_range, prec,
     &       nf_kind, fid,  coordinates, twoDimLat, lat2, lon2, rc )

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
      subroutine GFIO_CreateCF1 ( fname, title, source, contact, amiss,
     &                         im, jm, km, lon, lat, levs, levunits, 
     &                         yyyymmdd_beg, hhmmss_beg, timinc,
     &                         nvars, vname, vtitle, vunits, kmvar,
     &                         valid_range, packing_range, prec,
     &                         nf_kind, fid, coordinates, 
     &                         twoDimLat, lat2, lon2, rc )
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
      character*(*)  coordinates(*) ! attribute for CF compliance
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
! ALT: Additional argument for CF
      logical     twoDimLat ! indicator if "true" two dimentional lat/lon are used 
      real*8        lat2(im,jm)
      real*8        lon2(im,jm)
                                    

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
      integer lat2id, lon2id
      integer timedim, latdim, londim, levdim
      integer dims3D(4), dims2D(3)
      integer dimsLatLon2D(2)
      integer corner(4), edges(4)
      integer corner2d(2), edges2d(2)
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
      character (len=50) :: lon2Name = "LON"
      character (len=50) :: lon2Units = "degrees_east"
      character (len=50) :: lat2Name = "LAT"
      character (len=50) :: lat2Units = "degrees_north"
      character (len=50) :: levName = "vertical level"
c                           levUnits: specified by user in argument list
      character (len=50) :: timeName = "time"
c                           timeUnits: string is built below
      character (len=50) :: conventions = "COARDS"
      character (len=50) :: history = "File written by GFIO v1.0.8"
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

      call ncpopt(NCVERBOS)

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
! Handle possible two dimentional ("true") coordinates (CF)
      if (twoDimLat) then
         dimsLatLon2D(1) = londim
         dimsLatLon2D(2) = latdim
         lon2id = ncvdef (fid, lon2Name, NCDOUBLE, 2, dimsLatLon2D, rc)
         if (err("Create: error creating lon2",rc,-32) .LT. 0) return
         lat2id = ncvdef (fid, lat2Name, NCDOUBLE, 2, dimsLatLon2D, rc)
         if (err("Create: error creating lat2",rc,-32) .LT. 0) return
      endif
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

      if (twoDimLat) then
         call ncaptc (fid,lon2id,'long_name',NCCHAR,LEN_TRIM(lon2Name),
     .        lon2Name,rc)
         if (err("Create: error creating lon2 attribute",rc,-33) .LT. 0)
     .        return
         call ncaptc (fid,lon2id,'units',NCCHAR,LEN_TRIM(lon2Units),
     .        lon2Units,rc)
         if (err("Create: error creating lon2 attribute",rc,-33) .LT. 0) 
     .        return

         call ncaptc (fid,lat2id,'long_name',NCCHAR,LEN_TRIM(lat2Name),
     .        lat2Name,rc)
         if (err("Create: error creating lat attribute",rc,-33) .LT. 0) 
     .        return
         call ncaptc (fid,lat2id,'units',NCCHAR,LEN_TRIM(lat2Units),
     .        lat2Units,rc)
         if (err("Create: error creating lat2 attribute",rc,-33) .LT. 0) 
     .        return
      endif

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
          block
          integer :: chunksizes(4)
	  if( nf_kind .eq. 1 ) then 
              chunksizes(3) = 1          ! time
              chunksizes(2) = jm
              chunksizes(1) = im
              rc = nf_def_var_chunking(fid, vid(i), NF_CHUNKED, chunksizes)
              if (err("Create: error defining chunking",rc,-34) .LT. 0) 
     .           return
          endif
          end block
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
          block
          integer :: chunksizes(4)
	  if( nf_kind .eq. 1 ) then 
              chunksizes(4) = 1           ! time
              chunksizes(3) = 1           ! level
              chunksizes(2) = jm
              chunksizes(1) = im
              rc = nf_def_var_chunking(fid, vid(i), NF_CHUNKED, chunksizes)
              if (err("Create: error defining chunking",rc,-34) .LT. 0) 
     .            return
          endif
          end block
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

        if (coordinates(i) /= '') then
           call ncaptc (fid, vid(i), 'coordinates', NCCHAR, 
     .                  LEN_TRIM(coordinates(i)),coordinates(i), rc)
           if (err("Create: error defining variable attribute",rc,-35)
     .         .LT. 0) return
        end if

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

      if (twoDimLat) then
         corner2d(1) = 1
         corner2d(2) = 1
         edges2d(1) = im
         edges2d(2) = jm
         call ncvpt (fid, lon2id, corner2d, edges2d, lon2, rc)
         if (err("Create: error writing lons2",rc,-38) .LT. 0) return
         call ncvpt (fid, lat2id, corner2d, edges2d, lat2, rc)
         if (err("Create: error writing lats2",rc,-38) .LT. 0) return
      endif

      rc=0
      return
      end

      subroutine GFIO_InquireCF ( fid, im, jm, km, lm, nvars,
     &                        title, source, contact, amiss,
     &                        lon, lat, levs, levunits, 
     &                        yyyymmdd, hhmmss, timinc, 
     &                        vname, vtitle, vunits, kmvar,
     &                        valid_range , packing_range,
     &                        coordinates, twoDimLat, lat2, lon2, rc )
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
    
      character*(*) coordinates(nvars) ! attribute for CF compliance
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
! ALT: Additional argument for CF
      logical     twoDimLat ! indicator if "true" two dimentional lat/lon are used 
      real*8        lat2(im,jm)
      real*8        lon2(im,jm)
                                    


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
      integer lat2Id, lon2Id, size
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
      integer corner2d(2), edges2d(2)
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
           if ( dimsFound >= nDims ) cycle
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
!ALT           if ( dimsFound .eq. nDims ) exit
        else if (nvDims .EQ. 2 .AND. twoDimLat) then
           dimId = i
           dimName = vnameTemp
           call ncagtc (fid, i, 'units', dimUnits, MAXCHR, rc)
           if (err("Inqure: could not get units for "//trim(dimName),rc,-53)
     .         .NE. 0) return
           index = IdentifyDim (dimName, dimUnits)
           if ( index .EQ. 0 ) then
              lon2Id = i
           else if ( index .EQ. 1 ) then
              lat2Id = i
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

      if (twoDimLat) then
         size = im*jm

         call ncvinq (fid,lon2Id,dimName,dimType,nvDims,vDims,nvAtts,rc)
         if ( dimType .eq. NCDOUBLE ) then

            corner2d(1) = 1
            corner2d(2) = 1
            edges2d(1) = im
            edges2d(2) = jm
            call ncvgt (fid, lon2Id, corner2d, edges2d, lon2, rc)
            if (err("Inquire: error reading 64-bit lons",rc,-49) .LT. 0) return
         else
            if (err("Inquire: unsupported lon2 type",-1,-49) .LT. 0) return
         endif

         call ncvinq (fid,lat2Id,dimName,dimType,nvDims,vDims,nvAtts,rc)
         if ( dimType .eq. NCDOUBLE ) then

            call ncvgt (fid, lat2Id, corner2d, edges2d, lat2, rc)
            if (err("Inquire: error reading 64-bit lats",rc,-49) .LT. 0) return
         else
            if (err("Inquire: unsupported lat2 type",-1,-49) .LT. 0) return
         endif

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
        if (nvDims .EQ. 1 .OR. nvDims .EQ. 2) then   ! coord variable
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
        if (nvDims .EQ. 1 .OR. nvDims .EQ. 2) then
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
           print *, "Inquire: Cannot find variable units attribute"
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
        call ncagtc (fid, i,'coordinates', coordinates(fV),
     .               LEN(coordinates(fV)),rc)
        if (rc .NE. 0) then
           coordinates(fV) = ''
        endif
      enddo

      if (fV .NE. nvars) then
        rc = -9
        nvars = fV
        return
      endif

      rc=0
      call ncpopt(NCVERBOS)  ! back to chatty netcdf

      return
      end


#define __NCVERBOS__ 0
      subroutine GFIO_DimInquireCF (fid,im,jm,km,lm,nvars,ngatts,twoDimLat,rc)
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
      logical     twoDimLat ! indicator if "true" two dimentional lat/lon are used 
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
!
!EOP
!-------------------------------------------------------------------------

      integer dimId, i
      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits
      character*(MAXCHR) vname
      integer dimSize
      integer nDims
      integer err
      logical surfaceOnly
      integer IdentifyDim, index
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts, dimsFound

! Initialize variables

      surfaceOnly = .FALSE.
      twoDimLat = .FALSE.

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
 
    
      call ncinq (fid, nDims, nvars, ngatts, dimId, rc)
      if (err("DimInquire: ncinq failed",rc,-48) .NE. 0)return

      if (nDims .EQ. 3) then
        surfaceOnly = .TRUE.
      endif

! Subtract dimension variables from the variable count.
! Extract dimension information

      dimsFound=0
      do i=1,nvars
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
     .         .NE. 0) return
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
!          if ( dimsFound .eq. nDims ) exit
        endif
        if (nvDims .EQ. 2) then     ! Workaround "true" 2d LATs and LONs
          nvars = nvars - 1
          twoDimLat = .TRUE.
        end if
      enddo

      if (surfaceOnly) then
        km=0
      endif

      rc=0
      return
      end
