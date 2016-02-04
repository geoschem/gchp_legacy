          SUBROUTINE check(status)
            USE netcdf
            INTEGER, INTENT (INOUT) :: status
            IF (status /= nf90_noerr) THEN

              PRINT *, TRIM(nf90_strerror(status))
              PRINT *, "OPS: ERROR in reading NetCDF file for SST & SIC BCs"
              PRINT *, "NO SST and ICE Boundary Conditions!"
              call exit(99)
           

            END IF
          END SUBROUTINE check
