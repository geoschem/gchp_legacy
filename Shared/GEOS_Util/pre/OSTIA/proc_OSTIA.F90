!
PROGRAM proc_OSTIA
!---------------------------------------------------------------------------
        IMPLICIT NONE

! Name of data file.
        CHARACTER (LEN = 100)          :: in_file 
        CHARACTER (LEN = 8)            :: today, tomrw

! GET TO KNOW THESE BY ncdump -h
        REAL, PARAMETER    :: sst_FillValue       = -32768
        REAL, PARAMETER    :: sst_add_offset      =  273.15
        REAL, PARAMETER    :: sst_scale_factor    =  0.01
        REAL, PARAMETER    :: seaIce_FillValue    = -128
        REAL, PARAMETER    :: seaIce_add_offset   = 0.0
        REAL, PARAMETER    :: seaIce_scale_factor = 0.01

        INTEGER, PARAMETER :: NLAT_in          = 3600         
        INTEGER, PARAMETER :: NLON_in          = 7200         
        INTEGER, PARAMETER :: NTIME            = 1         

        INTEGER            :: NLAT_out
        INTEGER            :: NLON_out

        REAL, ALLOCATABLE :: LAT(:), LON(:) , SST(:,:,:), FRACI(:,:,:)
        REAL, ALLOCATABLE :: SST_full(:,:), FRACI_full(:,:)
        REAL, ALLOCATABLE :: SST_bin(:,:),  FRACI_bin(:,:)

        CHARACTER(LEN = 4):: today_Year, tomrw_Year
        CHARACTER(LEN = 2):: today_Mon,  tomrw_Mon, today_Day, tomrw_Day

        INTEGER           :: today_iYear, tomrw_iYear
        INTEGER           :: today_iMon,  tomrw_iMon, today_iDay, tomrw_iDay

        REAL              :: HEADER(14)

        INTEGER           :: j, i
!       ....................................................................

!       Get info. on today, tomorrow and input OSTIA- nc file
!---------------------------------------------------------------------------

        OPEN (UNIT = 21, FILE = 'input_stuff.txt')
        READ (21, *) today, tomrw, in_file, NLAT_out, NLON_out
        CLOSE(21)

        today_Year    = today(1:4);      tomrw_Year    = tomrw(1:4)
        today_Mon     = today(5:6);      tomrw_Mon     = tomrw(5:6)
        today_Day     = today(7:8);      tomrw_Day     = tomrw(7:8)

!       PRINT *, 'Processing data from: ', today, '...To... ', tomrw
!       PRINT *, 'input file: ', in_file

! Get fields from daily ostia netCDF file
!---------------------------------------------------------------------------
        ALLOCATE( LAT(NLAT_in))
        ALLOCATE( LON(NLON_in))
        ALLOCATE( SST  (NLON_in, NLAT_in, NTIME))
        ALLOCATE( FRACI(NLON_in, NLAT_in, NTIME))

        CALL read_ncFile(in_file, NLAT_in, NLON_in, NTIME, LAT, LON, SST, FRACI)

! Fill up over land
!---------------------------------------------------------------------------
        ALLOCATE( SST_full  (NLON_in, NLAT_in))
        ALLOCATE( FRACI_full(NLON_in, NLAT_in))

        CALL fillUp_Land(NLAT_in, NLON_in, LAT, LON, SST(:,:,1), FRACI(:,:,1), SST_full, FRACI_full)

        DO j=1,NLAT_in
          DO i=1,NLON_in
           IF( FRACI_full(i,j).lt.0.0 .OR. FRACI_full(i,j).gt.1.0 ) THEN
               PRINT *, 'FRACI_FULL: ',i,j,FRACI_full(i,j)
           ENDIF
          ENDDO
        ENDDO

! Bin to output grid.
!---------------------------------------------------------------------------
        ALLOCATE( SST_bin  (NLON_out, NLAT_out))
        ALLOCATE( FRACI_bin(NLON_out, NLAT_out))

        CALL bin2bin(   SST_full, NLON_in, NLAT_in,   SST_bin, NLON_out, NLAT_out)
        CALL bin2bin( FRACI_full, NLON_in, NLAT_in, FRACI_bin, NLON_out, NLAT_out)

        DO j=1,NLAT_out
          DO i=1,NLON_out
           IF( fraci_bin(i,j).LT.0.0 .OR. fraci_bin(i,j).GT.1.0 ) THEN
               PRINT *, 'FRACI_BIN: ',i,j,fraci_bin(i,j)
           ENDIF
          ENDDO
        ENDDO

! Dump SST & FRACI with header (YYYYMMDD start & end, nlon, nlat)
!---------------------------------------------------------------------------
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

        HEADER(13)   = REAL(NLON_out);        HEADER(14)    = REAL(NLAT_out)     
! .............
        WRITE(91) HEADER
        WRITE(91) SST_bin

        WRITE(92) HEADER
        WRITE(92) FRACI_bin
!---------------------------------------------------------------------------
 98     FORMAT(I4)
 99     FORMAT(I4)

        DEALLOCATE(LAT);       DEALLOCATE(LON) 
        DEALLOCATE(SST);       DEALLOCATE(FRACI) 
        DEALLOCATE(SST_full);  DEALLOCATE(FRACI_full)
        DEALLOCATE(SST_bin);   DEALLOCATE(FRACI_bin) 
!---------------------------------------------------------------------------
END PROGRAM proc_OSTIA
!
