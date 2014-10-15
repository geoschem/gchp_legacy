!
      SUBROUTINE read_Reynolds(ncFileName, NLAT, NLON, LAT, LON, &
                               SST, ICE, MASK, myUNDEF)
!---------------------------------------------------------------------------
          USE netcdf
          IMPLICIT NONE

          CHARACTER (LEN = *),             INTENT(IN)    :: ncFileName
          INTEGER,                         INTENT(IN)    :: NLAT, NLON
          REAL,                            INTENT(IN)    :: myUNDEF
          REAL, DIMENSION(NLAT),           INTENT(OUT)   :: LAT
          REAL, DIMENSION(NLON),           INTENT(OUT)   :: LON
          REAL, DIMENSION(NLON,NLAT),      INTENT(OUT)   :: MASK
          REAL, DIMENSION(NLON,NLAT),      INTENT(OUT)   :: SST
          REAL, DIMENSION(NLON,NLAT),      INTENT(OUT)   :: ICE

! GET TO KNOW THESE BY ncdump -h
          REAL, PARAMETER :: sst_FillValue       = -999
          REAL, PARAMETER :: sst_offset          =  273.15              ! we need sst in K, Reynolds has it in deg C
          REAL, PARAMETER :: sst_scale_factor    =  0.01
          REAL, PARAMETER :: ice_FillValue       = -999
          REAL, PARAMETER :: ice_offset          =  0.0
          REAL, PARAMETER :: ice_scale_factor    =  0.01
          
! netCDF ID for the file and data variable.
          INTEGER :: ncid, varid1, varid2, varid3, varid4
!         INTEGER :: iLON
!         REAL    :: dLon
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
          CALL check( nf90_inq_varid(ncid, "sst", varid3))
          CALL check( nf90_get_var(ncid, varid3, SST))
! Get Ice Concentration
          CALL check( nf90_inq_varid(ncid, "ice", varid4))
          CALL check( nf90_get_var(ncid, varid4, ICE))
! Close nc file.
          CALL check( nf90_close(ncid))
! .....................................................................
! Use scale factor & offset
          WHERE( SST /= sst_FillValue)
              SST = sst_scale_factor * SST + sst_offset
          ENDWHERE
          WHERE( ICE /= ice_FillValue)
              ICE = ice_scale_factor * ICE + ice_offset
          ENDWHERE
! Unify undef
          WHERE( SST == sst_FillValue)
              SST = myUNDEF
          ENDWHERE
          WHERE( ICE == ice_FillValue)
              ICE = myUNDEF
          ENDWHERE
! .....................................................................
! Reynolds has lon: (0, 360). 
! IF lon needs to be between (-180, 180) -> Flip.
!         dLon  = 360.0d0/REAL(NLON)
!         LON(1) = -180.0d0 +  dLon/2.0d0
!         DO iLon = 2, NLON
!             LON(iLon) = LON(iLon-1) + dLon
!         END DO
!         CALL hflip( ICE, NLON, NLAT)
!         CALL hflip( SST, NLON, NLAT)

          MASK = 1.0d0
          WHERE (SST == myUNDEF)
              MASK = 0.0d0                     ! Zero over land. ONE over water
          ENDWHERE
!---------------------------------------------------------------------------
      END SUBROUTINE read_Reynolds
!
