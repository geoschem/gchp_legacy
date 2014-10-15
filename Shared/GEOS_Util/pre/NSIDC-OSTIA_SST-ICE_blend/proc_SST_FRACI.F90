!
PROGRAM proc_SST_FRACI
!---------------------------------------------------------------------------
        IMPLICIT NONE

        INTEGER, PARAMETER      :: iDebug                 =  0
        INTEGER, PARAMETER      :: iOstia_Reynolds_ICE    =  1       ! always process & blend ostia ice with Reynolds ice. 

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
        CHARACTER (LEN = 20)    :: fileName_output_SST, fileName_output_ICE

        INTEGER                 :: iERR
        INTEGER                 :: NLAT_out
        INTEGER                 :: NLON_out
        INTEGER                 :: iLon, iLat, k

        REAL                    :: reynolds_LAT        (reynolds_NLAT), reynolds_LON(reynolds_NLON)
        REAL                    :: reynolds_SST_native (reynolds_NLON,  reynolds_NLAT)
        REAL                    :: reynolds_ICE_native (reynolds_NLON,  reynolds_NLAT)
        REAL                    :: reynolds_ls_MASK    (reynolds_NLON,  reynolds_NLAT)

        REAL, ALLOCATABLE       :: reynolds_SST_eigth(:,:), reynolds_ICE_eigth(:,:)
        REAL, ALLOCATABLE       :: ostia_SST_native  (:,:),   ostia_SST_eigth (:,:)
        REAL, ALLOCATABLE       :: ostia_ICE_native  (:,:),   ostia_ICE_eigth (:,:)

        REAL                    :: sstave

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
        CALL read_input(inputFile, iDebug, today, tomrw, fileNames, NLAT_out, NLON_out, iERR)
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
                           reynolds_SST_native, reynolds_ICE_native, reynolds_ls_MASK, myUNDEF)

!       Read Ostia 
!       SST               -> ostia_SST_native
        ALLOCATE( ostia_SST_native(ostia_NLON, ostia_NLAT) )
        CALL read_Ostia( fileName_Ostia, "analysed_sst", ostia_NLAT, ostia_NLON, ostia_SST_native, myUNDEF )
!---------------------------------------------------------------------------
!       Process Reynolds SST: (1) flip, (2) interp to 1/8 deg
        ALLOCATE( reynolds_SST_eigth(NLON_out,      NLAT_out     ))

        CALL hflip              ( reynolds_SST_native, reynolds_NLON, reynolds_NLAT )
        CALL interp_to_eight_deg( reynolds_SST_native, reynolds_NLON, reynolds_NLAT,                &
                                  reynolds_SST_eigth, NLON_out, NLAT_out, myUNDEF)

!       Process Ostia SST: bin to 1/8 deg.
        ALLOCATE( ostia_SST_eigth(NLON_out,      NLAT_out     ))
        CALL bin2bin( ostia_SST_native, ostia_NLON, ostia_NLAT, ostia_SST_eigth, NLON_out, NLAT_out, myUNDEF )

!       User wants Ostia-Reynolds blended ICE as well...
        IF( iOstia_Reynolds_ICE) THEN
                ALLOCATE( reynolds_ICE_eigth(NLON_out,      NLAT_out     ))
             
                CALL hflip              ( reynolds_ICE_native, reynolds_NLON, reynolds_NLAT )   
                CALL interp_to_eight_deg( reynolds_ICE_native, reynolds_NLON, reynolds_NLAT,       &
                                          reynolds_ICE_eigth, NLON_out, NLAT_out, myUNDEF)

                ALLOCATE( ostia_ICE_native(ostia_NLON,ostia_NLAT) )
                ALLOCATE( ostia_ICE_eigth (NLON_out,  NLAT_out) )
                CALL read_Ostia( fileName_Ostia, "sea_ice_fraction", ostia_NLAT, ostia_NLON, ostia_ICE_native, myUNDEF )
                CALL bin2bin   ( ostia_ICE_native, ostia_NLON, ostia_NLAT, ostia_ICE_eigth, NLON_out, NLAT_out, myUNDEF )
        END IF 

!       Merge Great Lakes SST and ICE from Reynolds into OSTIA (if needed)
        DO iLat = 1046, 1120
         DO iLon = 695, 842
            IF( (ostia_SST_eigth(iLon,iLat).eq.myUNDEF) .and. (reynolds_SST_eigth(iLon,iLat).ne.myUNDEF) ) THEN
                 ostia_SST_eigth(iLon,iLat) = reynolds_SST_eigth(iLon,iLat)
            END IF

            IF( iOstia_Reynolds_ICE) THEN
                IF( (ostia_ICE_eigth(iLon,iLat).eq.myUNDEF) .and. (reynolds_ICE_eigth(iLon,iLat).ne.myUNDEF) ) THEN
                     ostia_ICE_eigth(iLon,iLat) = reynolds_ICE_eigth(iLon,iLat)
                END IF
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
              ! if there is ice data in reynolds, use Reynolds Ice
              IF( (reynolds_ICE_eigth(iLon,iLat) .eq. myUNDEF) .or. (reynolds_ICE_eigth(iLon,iLat) <= Ice_thr)) THEN 
                   ostia_ICE_eigth(iLon,iLat) = reynolds_ICE_eigth(iLon,iLat)
              ELSE 
                   ostia_ICE_eigth(iLon,iLat) = MIN( 1.0d0, MAX(-0.017451*((ostia_SST_eigth(iLon,iLat)- 271.38)/0.052747) + 0.96834, 0.0d0))
              END IF
            END IF  ! IF( ostia_SST_eigth(iLon,iLat) <= 275.0d0)

            IF( iDebug) PRINT *, ostia_SST_eigth(iLon,iLat), ostia_ICE_eigth(iLon,iLat)

            IF( reynolds_SST_eigth(iLon,iLat) <= 275.0d0) THEN
              IF( (reynolds_ICE_eigth(iLon,iLat) .eq. myUNDEF) .or. (reynolds_ICE_eigth(iLon,iLat) <= Ice_thr)) &
                   reynolds_ICE_eigth(iLon,iLat) = MIN( 1.0d0, MAX(-0.017451*((reynolds_SST_eigth(iLon,iLat)- 271.38)/0.052747) + 0.96834, 0.0d0))
            END IF

         END DO
        END DO
!---------------------------------------------------------------------------
!       Fill up values over land
        CALL fill_Land (ostia_SST_eigth, NLON_out, NLAT_out, myUNDEF) 

        IF( iOstia_Reynolds_ICE)                                                                   &
              CALL fill_Land (ostia_ICE_eigth, NLON_out, NLAT_out, myUNDEF) 
!---------------------------------------------------------------------------
! two ways to handle SST values over Antarctic land- Prefer 1st method- below.
! for ice, it does not matter which way, since it is over *land*
!---------------------------------------------------------------------------
!       1st way: Set SST over Antartica to Average Sea-Ice_Line Value
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

!       2nd way: Set UNDEF Values over Antarctica to First Defined Value in Latitude
!       DO iLon = 1, NLON_out
!          iLat = 1
!          DO WHILE( ostia_SST_eigth(iLon, iLat) .EQ. myUNDEF)
!             iLat = iLat + 1
!          END DO
!          DO k = 1, iLat-1
!                 ostia_SST_eigth(iLon, k) = ostia_SST_eigth(iLon, iLat)
!          END DO
!       END DO

        IF( iOstia_Reynolds_ICE) THEN
           DO iLon = 1, NLON_out
              iLat = 1
              DO WHILE( ostia_ICE_eigth(iLon, iLat) .EQ. myUNDEF)
                 iLat = iLat + 1
              END DO
              DO k = 1, iLat-1
                  ostia_ICE_eigth(iLon, k) = ostia_ICE_eigth(iLon, iLat)
              END DO
           END DO
        END IF
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
!       Write out 
        fileName_output_SST  = 'sst_'   // today //'.bin'
        fileName_output_ICE  = 'fraci_' // today //'.bin'

        OPEN (UNIT = 991, FILE = fileName_output_SST, FORM = 'unformatted', STATUS = 'new')
        OPEN (UNIT = 992, FILE = fileName_output_ICE, FORM = 'unformatted', STATUS = 'new')

        WRITE(991) HEADER
        WRITE(991) ostia_SST_eigth
        WRITE(992) HEADER
        WRITE(992) ostia_ICE_eigth

        CLOSE(991)
        CLOSE(992)

        IF( iERR == 0)                                                                             &
             PRINT *, '...Finished!'
!---------------------------------------------------------------------------
        IF( iDebug) THEN
           WRITE(101) reynolds_ICE_native
           WRITE(102) reynolds_ls_MASK
        END IF
!---------------------------------------------------------------------------
 98     FORMAT(I4)
 99     FORMAT(I4)
!---------------------------------------------------------------------------
        IF( iOstia_Reynolds_ICE) THEN
                  DEALLOCATE(reynolds_ICE_eigth)
                  DEALLOCATE(ostia_ICE_native)
                  DEALLOCATE(ostia_ICE_eigth)
        END IF

        DEALLOCATE(ostia_SST_native)
        DEALLOCATE(ostia_SST_eigth)
        DEALLOCATE(reynolds_SST_eigth)
!---------------------------------------------------------------------------
END PROGRAM proc_SST_FRACI
!
