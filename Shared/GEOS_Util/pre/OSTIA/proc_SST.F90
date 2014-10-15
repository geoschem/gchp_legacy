!
PROGRAM proc_SST
!---------------------------------------------------------------------------
        IMPLICIT NONE

        CHARACTER (LEN = 100)   :: inputBuffer, inputFile
        CHARACTER (LEN = 100)   :: fileNames(2)
        CHARACTER (LEN = 8)     :: today, tomrw

        CHARACTER (LEN = 100)   :: fileName_Reynolds, fileName_OSTIA
        INTEGER                 :: NLAT_out
        INTEGER                 :: NLON_out

        INTEGER, PARAMETER      :: GEOS_NLAT              = 1440        
        INTEGER, PARAMETER      :: GEOS_NLON              = 2880

        INTEGER, PARAMETER      :: ostia_NLAT             = 3600         
        INTEGER, PARAMETER      :: ostia_NLON             = 7200
        REAL,    PARAMETER      :: ostia_sst_FillValue    = -32768

        INTEGER, PARAMETER      :: reynolds_NLAT          = 720         
        INTEGER, PARAMETER      :: reynolds_NLON          = 1440
        REAL,    PARAMETER      :: reynolds_sst_FillValue = -999

        INTEGER                 :: flag_Reynolds

        REAL, ALLOCATABLE       :: GEOS_SST(:,:)

        REAL, ALLOCATABLE       :: reynolds_LAT(:), reynolds_LON(:), reynolds_DUM(:,:)
        REAL, ALLOCATABLE       ::                                   reynolds_SST(:,:)


        REAL, ALLOCATABLE       :: ostia_LAT   (:),    ostia_LON(:),    ostia_DUM(:,:)
        REAL, ALLOCATABLE       ::                                      ostia_SST(:,:)

        REAL                    :: HEADER(14)
        CHARACTER(LEN = 4)      :: today_Year, tomrw_Year
        CHARACTER(LEN = 2)      :: today_Mon,  tomrw_Mon, today_Day, tomrw_Day
        INTEGER                 :: today_iYear, tomrw_iYear
        INTEGER                 :: today_iMon,  tomrw_iMon, today_iDay, tomrw_iDay

        INTEGER                 :: i,j
        REAL                    :: sstave
!       ....................................................................

!       Get input
!---------------------------------------------------------------------------
!       CALL getarg(1,inputBuffer)
!       READ(inputBuffer, *) inputFile
        CALL read_input('input_stuff.txt', today, tomrw, fileNames, NLAT_out, NLON_out, flag_Reynolds)
        fileName_Reynolds  = fileNames(1)
        fileName_OSTIA     = fileNames(2)

        PRINT *, 'Processing SST data from: ', today, '...To... ', tomrw

! SST from Reynolds
!---------------------------------------------------------------------------

             ALLOCATE( reynolds_LAT(reynolds_NLAT) )
             ALLOCATE( reynolds_LON(reynolds_NLON) )
             ALLOCATE( reynolds_DUM(reynolds_NLON, reynolds_NLAT) )
             ALLOCATE( reynolds_SST(    GEOS_NLON,     GEOS_NLAT) )

             CALL read_Reynolds(fileName_Reynolds, reynolds_NLAT, reynolds_NLON, 1, 1, &
                                reynolds_LAT, reynolds_LON, reynolds_DUM)

             CALL hflip           ( reynolds_DUM,reynolds_NLON,reynolds_NLAT )
             CALL interp_reynolds ( reynolds_DUM,reynolds_NLON,reynolds_NLAT,                       &
                                    reynolds_SST,GEOS_NLON,GEOS_NLAT,reynolds_sst_FillValue )

             where( reynolds_SST == reynolds_sst_FillValue )
                    reynolds_SST =     ostia_sst_FillValue
             endwhere
             write(55) reynolds_SST
!---------------------------------------------------------------------------

! SST from OSTIA
!---------------------------------------------------------------------------

        ALLOCATE( ostia_LAT(ostia_NLAT) )
        ALLOCATE( ostia_LON(ostia_NLON) )
        ALLOCATE( ostia_DUM(ostia_NLON,ostia_NLAT) )
        ALLOCATE( ostia_SST( GEOS_NLON, GEOS_NLAT) )

        CALL read_Ostia( fileName_Ostia, ostia_NLAT, ostia_NLON, 1, ostia_LAT, ostia_LON, ostia_DUM )

        CALL bin2bin( ostia_DUM, ostia_NLON, ostia_NLAT, ostia_SST, GEOS_NLON, GEOS_NLAT, ostia_sst_FillValue )

        write(55) ostia_SST

! Merge Great Lakes SST from Reynolds into OSTIA
!-----------------------------------------------
        do j=1046,1120
        do i=695,842
           if( reynolds_SST(i,j).ne.ostia_sst_FillValue ) ostia_SST(i,j) = reynolds_SST(i,j)
        enddo
        enddo
      
        write(55) ostia_SST

! Set SST over Antartica to Average Sea-Ice_Line Value
!-----------------------------------------------------
        sstave = 0.0
        do i=1,GEOS_NLON
           j=1
        do while( ostia_SST(i,j).eq.ostia_sst_FillValue )
           j=j+1
        enddo
        sstave = sstave + ostia_SST(i,j)
        enddo
        sstave = sstave / GEOS_NLON
        do i=1,GEOS_NLON
           j=1
        do while( ostia_SST(i,j).eq.ostia_sst_FillValue )
                  ostia_SST(i,j) = sstave
                              j  = j+1
        enddo
        enddo
        
! Fill Values over LAND
!----------------------
!       ALLOCATE( GEOS_SST(GEOS_NLON,GEOS_NLAT) )
!       GEOS_SST = ostia_SST
!       do j=1,GEOS_NLAT
!       call Interp1d( GEOS_NLON, ostia_SST(1,j), ostia_sst_FillValue, GEOS_SST(1,j) )
!       enddo
!       write(55)  GEOS_SST

        call fill_Land ( ostia_SST,GEOS_NLON,GEOS_NLAT,ostia_sst_FillValue )
        write(55) ostia_SST

!---------------------------------------------------------------------------

!            WRITE(91) HEADER
!            WRITE(91) reynolds_SST

!            WRITE(92) HEADER
!            WRITE(92) grLakes_SST
!---------------------------------------------------------------------------

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

        HEADER(13)   = REAL(NLON_out);        HEADER(14)    = REAL(NLAT_out)     
! .............
!       WRITE(93) HEADER
!       WRITE(93) ostia_SST
!---------------------------------------------------------------------------
 98     FORMAT(I4)
 99     FORMAT(I4)

        DEALLOCATE(reynolds_LAT); DEALLOCATE(reynolds_LON)
        DEALLOCATE(reynolds_SST); DEALLOCATE(reynolds_DUM)

        DEALLOCATE(ostia_LAT)
        DEALLOCATE(ostia_LON) 
        DEALLOCATE(ostia_SST)
        DEALLOCATE(ostia_DUM)
!---------------------------------------------------------------------------
END PROGRAM proc_SST
!
      subroutine hflip ( q,im,jm )
      implicit none
      integer  im,jm,i,j,L
      real   q(im,jm),dum(im)
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j)
         dum(i+im/2) = q(i,j)
      enddo
         q(:,j) = dum(:)
      enddo
      return
      end

      subroutine fill_Land (q,im,jm,undef)
      implicit none
      integer  im,jm
      real     undef
      real   q(im,jm)
      integer i,j,k,L,n,i0,nundef
      real   qz(im)
      real dist,dq

      do j=1,jm
         qz = q(:,j)
         nundef = count( qz.eq.undef )
         if( nundef.eq.im .or. nundef.eq.0 ) cycle

         do i0=1,im
         if( q(i0,j).ne.undef ) exit
         enddo
         do k=i0,im+i0-1
            L=k
            if(L.gt.im) L=L-im
            qz(k-i0+1) = q(L,j)
         enddo

         do i=2,im
         if( qz(i).ne.undef ) cycle
             do k=i+1,im
                if( qz(k).eq.undef ) cycle
                dist = k-i+1
                dq = ( qz(k)-qz(i-1) )/dist
                exit
             enddo
             if( k.eq.im+1) then
                dist = k-i+1
                dq = ( qz(1)-qz(i-1) )/dist
             endif
             do L=i,k-1
                qz(L) = qz(i-1) + (L-i+1)*dq
             enddo
         enddo

         do k=i0,im+i0-1
            L=k
            if(L.gt.im) L=L-im
            q(L,j) = qz(k-i0+1)
         enddo

      enddo

      return
      end

          SUBROUTINE Interp1d(NVAR, VAR_in, UNDEF, VAR_out) 
              IMPLICIT NONE
              
              INTEGER,   INTENT(IN)      :: NVAR                        ! NLON or NLAT

              REAL,      INTENT(IN)      :: UNDEF                       ! UNDEF value
              REAL,      INTENT(IN)      :: VAR_in (NVAR)               ! field with UNDEF
              REAL,      INTENT(INOUT)   :: VAR_out(NVAR)               ! field with filled UNDEF

              INTEGER                    :: k, i1, i2
              REAL                       :: sl
              INTEGER                    :: nUNDEF
              INTEGER, ALLOCATABLE       :: loc_UNDEF(:)
              REAL,    ALLOCATABLE       :: VAR_temp(:)
              REAL                          VarAxis(NVAR)               ! either LAT(:) or LON(:)

              nUNDEF = COUNT(VAR_in == UNDEF)

              IF ( (nUNDEF == NVAR) .OR. (nUNDEF == 0) ) THEN           ! land or sea everywhere. 
                  RETURN                                     
              ELSE
                  ALLOCATE( loc_UNDEF(nUNDEF))
                  ALLOCATE( VAR_temp (NVAR))
              END IF
              do k=1,nvar
              varaxis(k) = k
              enddo
              
! Find location of undef
              i1 = 1
              DO k = 1, NVAR
                   IF ( VAR_in(k) == UNDEF) THEN
                      loc_UNDEF(i1) = k
                      i1 = i1 + 1
                   ELSE
                      VAR_out(k) = VAR_in(k)
                   END IF
              END DO

! Always need to have end points filled up- for interp to work!
              IF ( loc_UNDEF(1) == 1) THEN
                    VAR_out(1) = SUM( VAR_in, (VAR_in /= UNDEF))/ MAX( 1, COUNT( VAR_in /= UNDEF))
              END IF
              IF ( loc_UNDEF(nUNDEF) == NVAR) THEN
                    VAR_out(NVAR) = SUM( VAR_in, (VAR_in /= UNDEF))/ MAX( 1, COUNT( VAR_in /= UNDEF))
              END IF

! Fill up undef locations by 1st. order Interp
              VAR_temp = VAR_in
              DO k = 1, nUNDEF
                   i1 = loc_UNDEF(k)
                   i2 = i1 + 1
                   if( i1.eq.1    ) then
                       VAR_temp(i1) = VAR_out(i1)
                       cycle
                   endif
                   if( i1.eq.NVAR ) then
                       VAR_temp(i1) = VAR_out(i1)
                       cycle
                   endif
                  
                   DO WHILE ( VAR_in(i2) == UNDEF)
                        i2 = i2 + 1
                        if( i2.gt.NVAR ) exit
                   END DO
                   if( i2.gt.NVAR ) then
                       i2 =  NVAR
                       VAR_temp(i2) = VAR_out(NVAR)
                   endif
                   
                   sl = (VAR_temp(i2) - VAR_temp(i1-1))/(VarAxis(i2) - VarAxis(i1-1)) 
                   VAR_out (i1) = VAR_temp(i1-1) + sl * (VarAxis(i1) - VarAxis(i1-1))

                   VAR_temp = VAR_out
              END DO

              DEALLOCATE( loc_UNDEF)
              DEALLOCATE( VAR_temp)
          END SUBROUTINE Interp1d
!
