!
      SUBROUTINE read_nsidc_icec(fileName, NX, NY, ice, mask, myUNDEF)
!-------------------------------------------------------------------------------
         IMPLICIT NONE

         CHARACTER(LEN =*), INTENT(IN)       :: fileName
         INTEGER,           INTENT(IN)       :: NX
         INTEGER,           INTENT(IN)       :: NY
         REAL,              INTENT(IN)       :: myUNDEF
         REAL,              INTENT(OUT)      :: mask(NX, NY)
         REAL,              INTENT(OUT)      :: ice (NX, NY)

         INTEGER, PARAMETER                  :: iDebug = 0

         CHARACTER*1                         :: header(300)
         CHARACTER*1                         :: int_ice(NX, NY)
         INTEGER                             :: myRecLen

         INTEGER                             :: xx(NX, NY)
!       ....................................................................
         IF( iDebug) PRINT *, 'reading nsidc file: ', fileName

         INQUIRE(iolength=myRecLen) header(1:300), int_ice(1:NX, 1:NY)

         OPEN( UNIT = 21, FILE = fileName, FORM = 'unformatted', &
               ACCESS = 'direct', recl = myRecLen, STATUS = 'old')
         READ (21, rec=1 ) header, int_ice
         CLOSE(21)
         IF( iDebug) THEN
           PRINT *, 'Before fixing unsigned to signed int---'
           PRINT *, MINVAL(ICHAR(int_ice) )
           PRINT *, MAXVAL(ICHAR(int_ice) )
         END IF
         xx = ICHAR(int_ice)
         IF( iDebug) THEN
           PRINT *, 'After fixing unsigned to signed int---'
           PRINT *, MINVAL(xx)
           PRINT *, MAXVAL(xx)
!          WRITE(88,*) xx
         END IF
! fix data (land, missing, mask)
! http://nsidc.org/data/docs/daac/nsidc0051_gsfc_seaice.gd.html#paramrange

! Data was scaled by a factor of 250- convert to fraction of sea-ice \in [0, 1].
         WHERE ( xx < 251)
             ice     = xx/250.0d0
             mask    = 1.0d0
         END WHERE
! Circular mask used in the Arctic OCEAN to cover the irregularly-shaped data
! gap around the pole (caused by the orbit inclination and instrument swath)
         WHERE( xx == 251) 
             ice     = myUNDEF
             mask    = 1.0d0
         END WHERE
! Unused
         WHERE( xx == 252) 
                ice = myUNDEF       
         END WHERE
! CoastLines
         WHERE( xx == 253) 
                ice  = myUNDEF
                mask = 0.0d0
         END WHERE
! Superimposed land mask
         WHERE( xx == 254) 
                ice  = myUNDEF
                mask = 0.0d0
         END WHERE
! Assuming Missing data is over water. Why would NSIDC care about data over Land!!
         WHERE( xx == 255) 
                ice  = myUNDEF
                mask = 1.0d0
         END WHERE
                  
         IF( iDebug) THEN
            PRINT *, '*************************************************'
            PRINT *, 'From read_nsidc_icec'
            PRINT *, 'record length of data read= ', myRecLen
            PRINT *,  'Header: '
            PRINT *,  header
            PRINT *, 'data value @ 1st & last grid pts:'
            PRINT *, ice(1,1), ice(NX,NY)
            PRINT *, 'min & max value of data: ', MINVAL(ice), MAXVAL(ice)
!           WRITE(99,*) ice(150,1:NY)
         END IF
!---------------------------------------------------------------------------
      END SUBROUTINE read_nsidc_icec
!

