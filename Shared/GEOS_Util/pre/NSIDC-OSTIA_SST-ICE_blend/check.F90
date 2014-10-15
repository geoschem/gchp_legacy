          SUBROUTINE check(status)
                USE netcdf
                INTEGER, INTENT (IN) :: status
                IF (status /= nf90_noerr) THEN
                              PRINT *, TRIM(nf90_strerror(status))
                              STOP "Stopped"
                END IF
          END SUBROUTINE check
