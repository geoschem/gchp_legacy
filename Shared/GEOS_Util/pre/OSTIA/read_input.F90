!
      SUBROUTINE read_input(inputFile, today, tomrw, fileNames, NLAT, NLON)
!---------------------------------------------------------------------------
          IMPLICIT NONE

          CHARACTER (LEN = *),  INTENT(IN)    :: inputFile
          CHARACTER (LEN = *),  INTENT(OUT)   :: today, tomrw
          CHARACTER (LEN = *),  INTENT(OUT)   :: fileNames(2)
          INTEGER,              INTENT(OUT)   :: NLAT, NLON

          
          INTEGER, PARAMETER                  :: iDebug =0
!---------------------------------------------------------------------------
!       READ *, inputFileName
        OPEN (UNIT = 21, FILE = inputFile, STATUS = 'old')
        READ (21, *) today, tomrw, fileNames(1), fileNames(2), NLAT, NLON
        CLOSE(21)

        IF( iDebug) THEN
          PRINT *, today
          PRINT *, tomrw
          PRINT *, fileNames(1)
          PRINT *, fileNames(2)
          PRINT *, NLAT, NLON
        END IF
!---------------------------------------------------------------------------
      END SUBROUTINE read_input
!
