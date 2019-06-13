!
!       NOTES:
!              1. REYNOLDS file name format changes-- cannot be hardcoded!
!      .......................................................................
!
      SUBROUTINE read_input(inputFile, iDebug, today, tomrw, fileName, NLAT, NLON, &
                            iMerra, iERR)
!---------------------------------------------------------------------------
          IMPLICIT NONE

          CHARACTER (LEN = *),  INTENT(IN)    :: inputFile
          INTEGER,              INTENT(IN)    :: iDebug
          CHARACTER (LEN = *),  INTENT(OUT)   :: today, tomrw
          CHARACTER (LEN = *),  INTENT(OUT)   :: fileName(2)
          INTEGER,              INTENT(OUT)   :: NLAT, NLON
          INTEGER,              INTENT(OUT)   :: iMerra
          INTEGER,              INTENT(OUT)   :: iERR

!---------------------------------------------------------------------------
!       READ *, inputFileName
        OPEN (UNIT = 21, FILE = inputFile, STATUS = 'old')

!       Read one-line input
!       READ (21, *) today, tomrw,                                       &
!                    fileName(1), fileName(2), fileName(3), fileName(4), &
!                    NLAT, NLON, iDumpOstia_Reynolds_ICE

!       Read multi-line input
        READ (21, '(A)') today
        READ (21, '(A)') tomrw
        READ (21, '(A)') fileName(1)                                 ! Reynolds file
        READ (21, '(A)') fileName(2)                                 ! OSTIA    file
        READ (21, '(I5)') NLAT
        READ (21, '(I5)') NLON
        READ (21, '(I5)') iMerra                                      ! process for MERRA?
        CLOSE(21)
!      .......................................................................
!      CHECK USER INPUT. Die if not correct
!      All other checks must be done here.
!      .......................................................................
        iERR = 0
        IF( today == tomrw)  THEN
          iERR = 1
          PRINT *, 'Processing Start date: ', today
          PRINT *, 'is SAME as End date:   ', tomrw
          PRINT *, 'End date must be AFTER Start date'
        END IF

!       CHECK OSTIA FILE NAME WITH DATE if path of file is NOT input
!       tmp_char  = fileName(4)
!       tmp_today = tmp_char(1:8)
!       IF ( tmp_today /= today) THEN
!         iERR = 1
!         PRINT *, 'OSTIA file: ', tmp_char
!         PRINT *, 'is NOT for the Start date: ', today
!         PRINT *, '1st eight char of file name should be START date'
!       END IF
!      .......................................................................
        IF( iDebug /= 0 ) THEN
          PRINT *, '---------------------------------------'
          PRINT *, 'From read_input: '
          PRINT *, 'Today:         ', today
          PRINT *, 'Tomorrow:      ', tomrw
          PRINT *, 'Reynolds file: ', fileName(1)
          PRINT *, 'OSTIA    file: ', fileName(2)
          PRINT *, 'NLAT & NLON:   ', NLAT, NLON
          PRINT *, 'iMerra:        ', iMerra
          PRINT *, '---------------------------------------'
        END IF
!---------------------------------------------------------------------------
      END SUBROUTINE read_input
!

