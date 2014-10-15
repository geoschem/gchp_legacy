!
      SUBROUTINE read_OSTIA (ncFileName, VARNAME, NLAT, NLON, VAR, myUNDEF)
!---------------------------------------------------------------------------
          USE netcdf
          IMPLICIT NONE

          CHARACTER (LEN = *),               INTENT(IN)    :: ncFileName
          CHARACTER (LEN = *),               INTENT(IN)    :: VARNAME
          INTEGER,                           INTENT(IN)    :: NLAT, NLON
          REAL,                              INTENT(IN)    :: myUNDEF
          REAL, DIMENSION(NLON,NLAT),        INTENT(OUT)   :: VAR

! GET TO KNOW THESE BY ncdump -h
          REAL, PARAMETER :: sst_FillValue       = -32768
          REAL, PARAMETER :: sst_offset          =  273.15
          REAL, PARAMETER :: sst_scale_factor    =  0.01
          REAL, PARAMETER :: ice_FillValue       = -128
          REAL, PARAMETER :: ice_offset          =  0.0
          REAL, PARAMETER :: ice_scale_factor    =  0.01
          
! netCDF ID for the file and data variable.
          INTEGER :: ncid, varid1, varid2, varid3, varid4
          REAL    :: FillValue
          REAL    :: offset
          REAL    :: scale_factor
!---------------------------------------------------------------------------
! Open the file. NF90_NOWRITE tells netCDF we want read-only access.
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fOPEN.html#NF90_005fOPEN
          CALL check( nf90_open(ncFileName, nf90_nowrite, ncid))

! Get the varid of the data variable, based on its name.
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fINQ_005fVARID.html

! Get lat
!         CALL check( nf90_inq_varid(ncid, "lat", varid1))
!         CALL check( nf90_get_var(ncid, varid1,  LAT))

! Get lon
!         CALL check( nf90_inq_varid(ncid, "lon", varid2))
!         CALL check( nf90_get_var(ncid, varid2,  LON))

! Get VAR
          CALL check( nf90_inq_varid(ncid, trim(VARNAME), varid3))
          CALL check( nf90_get_var(ncid, varid3, VAR))

! Close nc file.
          CALL check( nf90_close(ncid))
! .....................................................................
! Use scale factor & offset

          if( trim(VARNAME)=="analysed_sst" ) then
              FillValue    = sst_FillValue
              offset       = sst_offset
              scale_factor = sst_scale_factor
          endif
          if( trim(VARNAME)=="sea_ice_fraction" ) then
              FillValue    = ice_FillValue
              offset       = ice_offset
              scale_factor = ice_scale_factor
          endif

          WHERE( VAR /= FillValue)
              VAR = scale_factor * VAR + offset
          ENDWHERE

! Unify undef
          WHERE( VAR == FillValue)
                 VAR = myUNDEF
          ENDWHERE
!---------------------------------------------------------------------------
      END SUBROUTINE read_OSTIA
!
