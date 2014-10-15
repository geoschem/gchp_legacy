!
      SUBROUTINE read_Reynolds(ncFileName, VARNAME, NLAT, NLON, VAR, FillValue)
!---------------------------------------------------------------------------
          USE netcdf
          IMPLICIT NONE

          CHARACTER (LEN = *),                    INTENT(IN)    :: ncFileName
          CHARACTER (LEN = *),                    INTENT(IN)    :: VARNAME
          INTEGER,                                INTENT(IN)    :: NLAT, NLON
          REAL,                                   INTENT(OUT)   :: FillValue
          REAL, DIMENSION(NLON,NLAT),             INTENT(OUT)   :: VAR

! GET TO KNOW THESE BY ncdump -h
          REAL, PARAMETER :: sst_FillValue       = -999
          REAL, PARAMETER :: sst_offset          =  273.15              ! we need sst in K, Reynolds has it in deg C
          REAL, PARAMETER :: sst_scale_factor    =  0.01
          REAL, PARAMETER :: ice_FillValue       = -999
          REAL, PARAMETER :: ice_offset          =  0.0
          REAL, PARAMETER :: ice_scale_factor    =  0.01
          
! netCDF ID for the file and data variable.
          INTEGER :: ncid, varid1, varid2, varid3, varid4
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

! Get SST
          CALL check( nf90_inq_varid(ncid, trim(VARNAME), varid3))
          CALL check( nf90_get_var(ncid, varid3, VAR))

! Close nc file.
          CALL check( nf90_close(ncid))
! .....................................................................
! Use scale factor & offset

          if( trim(VARNAME)=="sst" ) then
              FillValue    = sst_FillValue
              offset       = sst_offset
              scale_factor = sst_scale_factor
          endif
          if( trim(VARNAME)=="ice" ) then
              FillValue    = ice_FillValue
              offset       = ice_offset
              scale_factor = ice_scale_factor
          endif

          WHERE( VAR /= FillValue)
              VAR = scale_factor * VAR + offset
          ENDWHERE
!---------------------------------------------------------------------------
      CONTAINS
          SUBROUTINE check(status)
                USE netcdf
                INTEGER, INTENT (IN) :: status
                IF (status /= nf90_noerr) THEN
                              PRINT *, TRIM(nf90_strerror(status))
                              STOP "Stopped"
                END IF
          END SUBROUTINE check
!---------------------------------------------------------------------------
      END SUBROUTINE read_Reynolds
!
