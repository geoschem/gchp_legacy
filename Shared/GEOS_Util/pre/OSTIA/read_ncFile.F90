!
      SUBROUTINE read_ncFile(ncFileName, NLAT, NLON, NTIME, LAT, LON, SST, FRACI)
!---------------------------------------------------------------------------
          USE netcdf
          IMPLICIT NONE

          CHARACTER (LEN = *),               INTENT(IN)    :: ncFileName
          INTEGER,                           INTENT(IN)    :: NLAT, NLON, NTIME
          REAL, DIMENSION(NLAT),             INTENT(INOUT) :: LAT
          REAL, DIMENSION(NLON),             INTENT(INOUT) :: LON
          REAL, DIMENSION(NLON,NLAT, NTIME), INTENT(INOUT) :: SST
          REAL, DIMENSION(NLON,NLAT, NTIME), INTENT(INOUT) :: FRACI

! GET TO KNOW THESE BY ncdump -h
          REAL, PARAMETER :: sst_FillValue       = -32768
          REAL, PARAMETER :: sst_add_offset      =  273.15
          REAL, PARAMETER :: sst_scale_factor    =  0.01
          REAL, PARAMETER :: seaIce_FillValue    = -128
          REAL, PARAMETER :: seaIce_add_offset   =  0.0
          REAL, PARAMETER :: seaIce_scale_factor =  0.01
          
! netCDF ID for the file and data variable.
          INTEGER :: ncid, varid1, varid2, varid3, varid4
!---------------------------------------------------------------------------
! Open the file. NF90_NOWRITE tells netCDF we want read-only access.
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fOPEN.html#NF90_005fOPEN
          CALL check( nf90_open(ncFileName, nf90_nowrite, ncid))

! Get the varid of the data variable, based on its name.
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fINQ_005fVARID.html

! Get lat
          CALL check( nf90_inq_varid(ncid, "lat", varid1))
          CALL check( nf90_get_var(ncid, varid1,  LAT))

! Get lon
          CALL check( nf90_inq_varid(ncid, "lon", varid2))
          CALL check( nf90_get_var(ncid, varid2,  LON))

! Get SST
          CALL check( nf90_inq_varid(ncid, "analysed_sst", varid3))
          CALL check( nf90_get_var(ncid, varid3, SST))

! Get FRACI
          CALL check( nf90_inq_varid(ncid, "sea_ice_fraction", varid4))
          CALL check( nf90_get_var(ncid, varid4, FRACI))

! Close nc file.
          CALL check( nf90_close(ncid))
! .....................................................................
! Use scale factor & offset

          WHERE( SST /= sst_FillValue)
              SST = sst_scale_factor * SST + sst_add_offset
          ENDWHERE

          WHERE( FRACI /= seaIce_FillValue)
              FRACI = seaIce_scale_factor * FRACI + seaIce_add_offset
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
      END SUBROUTINE read_ncFile
!
