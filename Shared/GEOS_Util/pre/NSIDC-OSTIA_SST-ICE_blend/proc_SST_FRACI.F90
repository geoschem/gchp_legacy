!
PROGRAM proc_SST_FRACI
!---------------------------------------------------------------------------
        IMPLICIT NONE

        INTEGER, PARAMETER      :: iDebug                 =  0

        REAL,    PARAMETER      :: myUNDEF                = 1.0e15
        REAL,    PARAMETER      :: TempLow                = 273.15d0 ! low sst (in deg K) below which there is ice
        REAL,    PARAMETER      :: Ice_thr                = 1.0e-4   ! threshold on ice concentration- related to TempLow

        INTEGER, PARAMETER      :: reynolds_NLAT          = 720
        INTEGER, PARAMETER      :: reynolds_NLON          = 1440

        INTEGER, PARAMETER      :: ostia_NLAT             = 3600
        INTEGER, PARAMETER      :: ostia_NLON             = 7200

        CHARACTER (LEN = 100)   :: inputBuffer, inputFile
        CHARACTER (LEN = 150)   :: fileNames(2)
        CHARACTER (LEN = 8)     :: today, tomrw

        CHARACTER (LEN = 150)   :: fileName_Reynolds, fileName_Ostia

        CHARACTER (LEN = 40)    :: fileName_reynolds_SST, fileName_reynolds_ICE
        CHARACTER (LEN = 40)    :: fileName_ostia_SST,    fileName_ostia_ICE

        INTEGER                 :: iERR, iMerra
        INTEGER                 :: NLAT_out
        INTEGER                 :: NLON_out
        INTEGER                 :: iLon, iLat, k

        REAL                    :: reynolds_LAT        (reynolds_NLAT), reynolds_LON(reynolds_NLON)
        REAL                    :: reynolds_SST_native (reynolds_NLON,  reynolds_NLAT)
        REAL                    :: reynolds_ICE_native (reynolds_NLON,  reynolds_NLAT)

        REAL, ALLOCATABLE       :: reynolds_SST_eigth(:,:), reynolds_ICE_eigth(:,:)
        REAL, ALLOCATABLE       :: ostia_SST_native  (:,:),   ostia_SST_eigth (:,:)
        REAL, ALLOCATABLE       :: ostia_ICE_native  (:,:),   ostia_ICE_eigth (:,:)

        REAL                    :: sstave, diff_SST, diff_ICE

        REAL                    :: HEADER(14)
        CHARACTER(LEN = 4)      :: today_Year, tomrw_Year
        CHARACTER(LEN = 2)      :: today_Mon,  tomrw_Mon, today_Day, tomrw_Day
        INTEGER                 :: today_iYear, tomrw_iYear
        INTEGER                 :: today_iMon,  tomrw_iMon, today_iDay, tomrw_iDay
!       ....................................................................

!---------------------------------------------------------------------------
!       Read all input data parameters (time to proc, files to proc, output resolution)
        CALL getarg(1,inputBuffer)
        READ(inputBuffer, *) inputFile
        CALL read_input(inputFile, iDebug, today, tomrw, fileNames, NLAT_out, NLON_out, iMerra, iERR)
!---------------------------------------------------------------------------
        IF( iERR == 0) THEN
             PRINT *, 'Processing SST and ICE data from: ', today, '...To... ', tomrw
        ELSE
             PRINT *, 'User input is not in correct format- for this program to work!'
             PRINT *, 'SEE ABOVE LOG FOR DETAILS'
             STOP
        END IF

        fileName_Reynolds = fileNames(1)
        fileName_Ostia    = fileNames(2)
!------------------------------Read input files----------------------------
!       Read Reynolds 
!       SST               -> reynolds_SST_native 
!       Ice concentration -> reynolds_ICE_native
        CALL read_Reynolds(fileName_Reynolds, reynolds_NLAT, reynolds_NLON,                        &
                           reynolds_LAT, reynolds_LON,                                             &
                           reynolds_SST_native, reynolds_ICE_native, myUNDEF)

!       Read Ostia 
!       SST               -> ostia_SST_native
!       Ice concentration -> ostia_ICE_native
        ALLOCATE( ostia_SST_native(ostia_NLON, ostia_NLAT) )
        ALLOCATE( ostia_ICE_native(ostia_NLON,ostia_NLAT) )
        CALL read_Ostia( fileName_Ostia, "analysed_sst",     ostia_NLAT, ostia_NLON, ostia_SST_native, myUNDEF )
        CALL read_Ostia( fileName_Ostia, "sea_ice_fraction", ostia_NLAT, ostia_NLON, ostia_ICE_native, myUNDEF )

!------------------------------Process SST & ICE fields--------------------
!       reynolds ice has undef in open water as well. make that to 0.
        WHERE( (reynolds_SST_native .ne. myUNDEF) .and. (reynolds_ICE_native == myUNDEF))
             reynolds_ICE_native = 0.0d0
        END WHERE

!       Reynolds(SST, ICE): (1) flip, (2) interp to 1/8 deg
        ALLOCATE( reynolds_SST_eigth(NLON_out,      NLAT_out     ))
        ALLOCATE( reynolds_ICE_eigth(NLON_out,      NLAT_out     ))

        CALL hflip              ( reynolds_SST_native, reynolds_NLON, reynolds_NLAT )
        CALL hflip              ( reynolds_ICE_native, reynolds_NLON, reynolds_NLAT )

        CALL interp_to_eight_deg( reynolds_SST_native, reynolds_NLON, reynolds_NLAT,                &
                                  reynolds_SST_eigth, NLON_out, NLAT_out, myUNDEF)
        CALL interp_to_eight_deg( reynolds_ICE_native, reynolds_NLON, reynolds_NLAT,                &
                                  reynolds_ICE_eigth, NLON_out, NLAT_out, myUNDEF)

!---------------------------------------------------------------------------
!       Ostia(SST, ICE): bin to 1/8 deg.
        ALLOCATE( ostia_SST_eigth(NLON_out,  NLAT_out ))
        ALLOCATE( ostia_ICE_eigth(NLON_out,  NLAT_out ))

        CALL bin2bin( ostia_SST_native, ostia_NLON, ostia_NLAT, ostia_SST_eigth, NLON_out, NLAT_out, myUNDEF )
        CALL bin2bin( ostia_ICE_native, ostia_NLON, ostia_NLAT, ostia_ICE_eigth, NLON_out, NLAT_out, myUNDEF )
!---------------------------------------------------------------------------
!       Get Great Lakes SST and ICE from Reynolds into OSTIA (if needed)
        DO iLat = 1046, 1120
         DO iLon = 695, 842
            IF( (ostia_SST_eigth(iLon,iLat).eq.myUNDEF) .and. (reynolds_SST_eigth(iLon,iLat).ne.myUNDEF) ) THEN
                 ostia_SST_eigth(iLon,iLat) = reynolds_SST_eigth(iLon,iLat)
            END IF

            IF( (ostia_ICE_eigth(iLon,iLat).eq.myUNDEF) .and. (reynolds_ICE_eigth(iLon,iLat).ne.myUNDEF) ) THEN
                 ostia_ICE_eigth(iLon,iLat) = reynolds_ICE_eigth(iLon,iLat)    ! if OSTIA had no ice in Great Lakes, get data from Reynolds
            END IF
         END DO
        END DO
!---------------------------------------------------------------------------
!       Caspian Sea ice: there is no ice info in OSTIA ICE, when temp < freezing point, fix this problem.
        DO iLat = 1000, 1120
         DO iLon = 1800, 1890

            ! 1st. handle SST. if OSTIA SST = undef, then use Reynolds SST
            IF( (ostia_SST_eigth(iLon,iLat).eq.myUNDEF) .and. (reynolds_SST_eigth(iLon,iLat).ne.myUNDEF) ) THEN
                 ostia_SST_eigth(iLon,iLat) = reynolds_SST_eigth(iLon,iLat)
            END IF

            ! if sst < freezing temp
            IF( ostia_SST_eigth(iLon,iLat) <= 275.0d0) THEN
              ! if there is no ice data or ice ~ 0.0 
              IF( (ostia_ICE_eigth(iLon,iLat) .eq. myUNDEF) .or. (ostia_ICE_eigth(iLon,iLat) <= Ice_thr)) THEN
                  IF( (reynolds_ICE_eigth(iLon,iLat) .ne. myUNDEF) .and. (reynolds_ICE_eigth(iLon,iLat) > Ice_thr)) THEN
                     ostia_ICE_eigth(iLon,iLat)  = reynolds_ICE_eigth(iLon,iLat)                         ! use Reynolds ice
                  ELSE                                                                                   ! there is nothing useful from Reynolds, use empirical fit
                     ! this could be computed based on SST from OSTIA/Reynolds.
                     ostia_ICE_eigth(iLon,iLat)  = MIN( 1.0d0, MAX(-0.017451*((ostia_SST_eigth(iLon,iLat)- 271.38)/0.052747) + 0.96834, 0.0d0))
                  END IF
              END IF

              IF( iDebug /= 0 ) PRINT *, ostia_SST_eigth(iLon,iLat), ostia_ICE_eigth(iLon,iLat)
            END IF  ! IF( ostia_SST_eigth(iLon,iLat) <= 275.0d0)

            IF( reynolds_SST_eigth(iLon,iLat) <= 275.0d0) THEN
              IF( (reynolds_ICE_eigth(iLon,iLat) .eq. myUNDEF) .or. (reynolds_ICE_eigth(iLon,iLat) <= Ice_thr)) &
                   reynolds_ICE_eigth(iLon,iLat) = MIN( 1.0d0, MAX(-0.017451*((reynolds_SST_eigth(iLon,iLat)- 271.38)/0.052747) + 0.96834, 0.0d0))
            END IF

         END DO
        END DO
!
        IF( iMerra == 1) THEN
!       only needed for CASPIAN, because Reynolds has sst & ice in Great Lakes!
          DO iLat = 500, 560
            DO iLon = 900, 945
               IF( reynolds_SST_native(iLon,iLat) <= 275.0d0) THEN
                  IF( (reynolds_ICE_native(iLon,iLat) .eq. myUNDEF) .or. (reynolds_ICE_native(iLon,iLat) <= Ice_thr)) &
                   reynolds_ICE_native(iLon,iLat) = MIN( 1.0d0, MAX(-0.017451*((reynolds_SST_native(iLon,iLat)- 271.38)/0.052747) + 0.96834, 0.0d0))
               END IF
            END DO
          END DO
        END IF
!---------------------------------------------------------------------------
!       Fill up values over land

        CALL fill_Land (ostia_SST_eigth,       NLON_out, NLAT_out, myUNDEF) 
        CALL fill_Land (ostia_ICE_eigth,       NLON_out, NLAT_out, myUNDEF) 

        CALL fill_Land (reynolds_SST_eigth,    NLON_out, NLAT_out, myUNDEF)
        CALL fill_Land (reynolds_ICE_eigth,    NLON_out, NLAT_out, myUNDEF)

        IF( iMerra == 1) THEN
            CALL fill_Land (reynolds_SST_native,  reynolds_NLON, reynolds_NLAT, myUNDEF)
            CALL fill_Land (reynolds_ICE_native,  reynolds_NLON, reynolds_NLAT, myUNDEF)
        END IF
!---------------------------------------------------------------------------
! SST values over Antarctic land - for ice, it does not matter which way, since it is over *land*
!---------------------------------------------------------------------------
        sstave = 0.0d0
        DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( ostia_SST_eigth(iLon, iLat) .EQ. myUNDEF) 
              iLat = iLat + 1
           END DO
           sstave = sstave + ostia_SST_eigth(iLon, iLat)
        END DO
        sstave = sstave/NLON_out
        DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( ostia_SST_eigth(iLon, iLat) .EQ. myUNDEF) 
              ostia_SST_eigth(iLon, iLat) = sstave
              iLat = iLat + 1
           END DO
        END DO
!*
        sstave = 0.0d0
        DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( reynolds_SST_eigth(iLon, iLat) .EQ. myUNDEF)
              iLat = iLat + 1
           END DO
           sstave = sstave + reynolds_SST_eigth(iLon, iLat)
        END DO
        sstave = sstave/NLON_out
        DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( reynolds_SST_eigth(iLon, iLat) .EQ. myUNDEF)
              reynolds_SST_eigth(iLon, iLat) = sstave
              iLat = iLat + 1
           END DO
        END DO
!*
        IF( iMerra == 1) THEN
          sstave = 0.0d0
          DO iLon = 1, NLON_out
             iLat = 1
             DO WHILE( reynolds_SST_native(iLon, iLat) .EQ. myUNDEF)
                iLat = iLat + 1
             END DO
             sstave = sstave + reynolds_SST_native(iLon, iLat)
          END DO
          sstave = sstave/NLON_out
          DO iLon = 1, NLON_out
             iLat = 1
             DO WHILE( reynolds_SST_native(iLon, iLat) .EQ. myUNDEF)
                reynolds_SST_native(iLon, iLat) = sstave
                iLat = iLat + 1
             END DO
          END DO
        END IF
!*
        DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( ostia_ICE_eigth(iLon, iLat) .EQ. myUNDEF)
              iLat = iLat + 1
           END DO
           DO k = 1, iLat-1
              ostia_ICE_eigth(iLon, k) = ostia_ICE_eigth(iLon, iLat)
           END DO
        END DO

       DO iLon = 1, NLON_out
           iLat = 1
           DO WHILE( reynolds_ICE_eigth(iLon, iLat) .EQ. myUNDEF)
              iLat = iLat + 1
           END DO
           DO k = 1, iLat-1
              reynolds_ICE_eigth(iLon, k) = reynolds_ICE_eigth(iLon, iLat)
           END DO
        END DO

        IF( iMerra == 1) THEN
          DO iLon = 1, NLON_out
             iLat = 1
             DO WHILE( reynolds_ICE_native(iLon, iLat) .EQ. myUNDEF)
                iLat = iLat + 1
             END DO
             DO k = 1, iLat-1
                reynolds_ICE_native(iLon, k) = reynolds_ICE_native(iLon, iLat)
             END DO
          END DO
        END IF
!---------------------------------------------------------------------------
        diff_SST = SUM( ABS(reynolds_SST_eigth-ostia_SST_eigth))/(NLON_out*NLAT_out)
        diff_ICE = SUM( ABS(reynolds_ICE_eigth-ostia_ICE_eigth))/(NLON_out*NLAT_out)
        IF( diff_SST > 2.0d0) PRINT *, 'CAUTION! SST of OSTIA and Reynolds differ by Threshold; CHECK!!'
        IF( diff_ICE > 0.20d0)PRINT *, 'CAUTION! ICE of OSTIA and Reynolds differ by Threshold; CHECK!!'
!---------------------------------------------------------------------------
!       Header info.  Start & end dates: format: YYYYMMDDHHMMSS; Hour,min,Sec are set to zero.
        today_Year    = today(1:4);      tomrw_Year    = tomrw(1:4)
        today_Mon     = today(5:6);      tomrw_Mon     = tomrw(5:6)
        today_Day     = today(7:8);      tomrw_Day     = tomrw(7:8)

        READ( today_Year, 98) today_iYear
        READ( tomrw_Year, 98) tomrw_iYear

        READ( today_Mon,  99) today_iMon
        READ( tomrw_Mon,  99) tomrw_iMon

        READ( today_Day,  99) today_iDay
        READ( tomrw_Day,  99) tomrw_iDay

        HEADER(1)    = REAL(today_iYear); HEADER(7)     = REAL(tomrw_iYear)
        HEADER(2)    = REAL(today_iMon);  HEADER(8)     = REAL(tomrw_iMon)
        HEADER(3)    = REAL(today_iDay);  HEADER(9)     = REAL(tomrw_iDay)
        HEADER(4:6)  = 0.0;               HEADER(10:12) = 0.0

        HEADER(13)   = REAL(NLON_out);    HEADER(14)    = REAL(NLAT_out)
!---------------------------------------------------------------------------
!       Write out for OSTIA fields for FP & RPIT
!       SST, ICE:
        fileName_ostia_SST = 'Ostia_sst_'        // today //'.bin'
        fileName_ostia_ICE = 'Ostia_ice_'        // today //'.bin'

        OPEN (UNIT = 991, FILE = fileName_ostia_SST, FORM = 'unformatted', STATUS = 'new')
        OPEN (UNIT = 992, FILE = fileName_ostia_ICE, FORM = 'unformatted', STATUS = 'new')

        WRITE(991) HEADER
        WRITE(992) HEADER
        WRITE(991) ostia_SST_eigth
        WRITE(992) ostia_ICE_eigth
        CLOSE(991)
        CLOSE(992)
!---------------------------------------------------------------------------
        IF( iMerra == 1) &
          HEADER(13)   = REAL(reynolds_NLON);    HEADER(14)    = REAL(reynolds_NLAT)
!---------------------------------------------------------------------------
!       Write out Reynolds fields for MERRA-2
!       SST, ICE:
        fileName_reynolds_SST  = 'Reynolds_sst_' // today //'.bin'
        fileName_reynolds_ICE  = 'Reynolds_ice_' // today //'.bin'

        OPEN (UNIT = 993, FILE = fileName_reynolds_SST, FORM = 'unformatted', STATUS = 'new')
        OPEN (UNIT = 994, FILE = fileName_reynolds_ICE, FORM = 'unformatted', STATUS = 'new')

        WRITE(993) HEADER
        WRITE(994) HEADER

        IF( iMerra == 1) THEN
           WRITE(993) reynolds_SST_native
           WRITE(994) reynolds_ICE_native
        ELSE
           WRITE(993) reynolds_SST_eigth
           WRITE(994) reynolds_ICE_eigth
        END IF

        CLOSE(993)
        CLOSE(994)
!---------------------------------------------------------------------------
        IF( iERR == 0) PRINT *, '...Finished!'
!---------------------------------------------------------------------------
 98     FORMAT(I4)
 99     FORMAT(I4)
!---------------------------------------------------------------------------
        DEALLOCATE(reynolds_SST_eigth)
        DEALLOCATE(reynolds_ICE_eigth)

        DEALLOCATE(ostia_SST_native)
        DEALLOCATE(ostia_ICE_native)

        DEALLOCATE(ostia_ICE_eigth)
        DEALLOCATE(ostia_SST_eigth)
!---------------------------------------------------------------------------
END PROGRAM proc_SST_FRACI
!
