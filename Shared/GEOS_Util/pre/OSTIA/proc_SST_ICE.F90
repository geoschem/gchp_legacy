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

        INTEGER, PARAMETER      :: reynolds_NLAT          = 720         
        INTEGER, PARAMETER      :: reynolds_NLON          = 1440

        REAL                    ::    ostia_sst_FillValue
        REAL                    ::    ostia_ice_FillValue
        REAL                    :: reynolds_sst_FillValue
        REAL                    :: reynolds_ice_FillValue
        REAL,    PARAMETER      :: FillValue = 1.0e15

        REAL, ALLOCATABLE       :: GEOS_SST(:,:)
        REAL, ALLOCATABLE       :: GEOS_ICE(:,:)

        REAL, ALLOCATABLE       :: reynolds_DUM(:,:)
        REAL, ALLOCATABLE       :: reynolds_SST(:,:)
        REAL, ALLOCATABLE       :: reynolds_ICE(:,:)

        REAL, ALLOCATABLE       :: ostia_DUM(:,:)
        REAL, ALLOCATABLE       :: ostia_SST(:,:)
        REAL, ALLOCATABLE       :: ostia_ICE(:,:)

        REAL                    :: HEADER(14)
        CHARACTER(LEN = 4)      :: today_Year, tomrw_Year
        CHARACTER(LEN = 2)      :: today_Mon,  tomrw_Mon, today_Day, tomrw_Day
        INTEGER                 :: today_iYear, tomrw_iYear
        INTEGER                 :: today_iMon,  tomrw_iMon, today_iDay, tomrw_iDay

        INTEGER                 :: i,j,k
        REAL                    :: qave
!       ....................................................................

!       Get input
!---------------------------------------------------------------------------
!       CALL getarg(1,inputBuffer)
!       READ(inputBuffer, *) inputFile
        CALL read_input('input_stuff.txt', today, tomrw, fileNames, NLAT_out, NLON_out)
        fileName_Reynolds  = fileNames(1)
        fileName_OSTIA     = fileNames(2)

        PRINT *, 'Processing SST and ICE data from: ', today, '...To... ', tomrw

! READ from Reynolds
!---------------------------------------------------------------------------

        ALLOCATE( reynolds_DUM(reynolds_NLON, reynolds_NLAT) )
        ALLOCATE( reynolds_SST(    GEOS_NLON,     GEOS_NLAT) )
        ALLOCATE( reynolds_ICE(    GEOS_NLON,     GEOS_NLAT) )

        print *, 'Reading Reynolds Data for: SST'
        CALL read_Reynolds(fileName_Reynolds, "sst", reynolds_NLAT, reynolds_NLON, reynolds_DUM, reynolds_sst_FillValue)

        CALL hflip           ( reynolds_DUM,reynolds_NLON,reynolds_NLAT )
        CALL interp_reynolds ( reynolds_DUM,reynolds_NLON,reynolds_NLAT,                       &
                               reynolds_SST,GEOS_NLON,GEOS_NLAT,reynolds_sst_FillValue )

        print *, 'Reading Reynolds Data for: ICE'
        CALL read_Reynolds(fileName_Reynolds, "ice", reynolds_NLAT, reynolds_NLON, reynolds_DUM, reynolds_ice_FillValue)

        CALL hflip           ( reynolds_DUM,reynolds_NLON,reynolds_NLAT )
        CALL interp_reynolds ( reynolds_DUM,reynolds_NLON,reynolds_NLAT,                       &
                               reynolds_ICE,GEOS_NLON,GEOS_NLAT,reynolds_ice_FillValue )

!---------------------------------------------------------------------------

! READ from OSTIA
!---------------------------------------------------------------------------

        ALLOCATE( ostia_DUM(ostia_NLON,ostia_NLAT) )
        ALLOCATE( ostia_SST( GEOS_NLON, GEOS_NLAT) )
        ALLOCATE( ostia_ICE( GEOS_NLON, GEOS_NLAT) )

        print *, 'Reading OSTIA    Data for: SST'
        CALL read_Ostia( fileName_Ostia, "analysed_sst", ostia_NLAT, ostia_NLON, ostia_DUM, ostia_sst_FillValue )
        CALL bin2bin( ostia_DUM, ostia_NLON, ostia_NLAT, ostia_SST, GEOS_NLON, GEOS_NLAT, ostia_sst_FillValue )

        print *, 'Reading OSTIA    Data for: ICE'
        CALL read_Ostia( fileName_Ostia, "sea_ice_fraction", ostia_NLAT, ostia_NLON, ostia_DUM, ostia_ice_FillValue)
        CALL bin2bin( ostia_DUM, ostia_NLON, ostia_NLAT, ostia_ICE, GEOS_NLON, GEOS_NLAT, ostia_ice_FillValue )


! UNIFY Fill Values
!---------------------------------------------------------------------------
        where( reynolds_SST == reynolds_sst_FillValue )
               reynolds_SST =               FillValue
        endwhere
        where( reynolds_ICE == reynolds_ice_FillValue )
               reynolds_ICE =               FillValue
        endwhere
        where(    ostia_SST ==    ostia_sst_FillValue )
                  ostia_SST =               FillValue
        endwhere
        where(    ostia_ICE ==    ostia_ice_FillValue )
                  ostia_ICE =               FillValue
        endwhere

!       write(55) reynolds_SST
!       write(55) reynolds_ICE
!       write(55)    ostia_SST
!       write(55)    ostia_ICE


! Merge Great Lakes SST and ICE from Reynolds into OSTIA (if needed)
!-------------------------------------------------------------------
        do j=1046,1120
        do i=695,842
           if( ostia_SST(i,j).eq.FillValue .and. reynolds_SST(i,j).ne.FillValue ) ostia_SST(i,j) = reynolds_SST(i,j)
           if( ostia_ICE(i,j).eq.FillValue .and. reynolds_ICE(i,j).ne.FillValue ) ostia_ICE(i,j) = reynolds_ICE(i,j)
        enddo
        enddo
      
!       write(55) ostia_SST
!       write(55) ostia_ICE

! Fill Values over LAND
!----------------------
        call fill_Land ( ostia_SST,GEOS_NLON,GEOS_NLAT,FillValue )
        call fill_Land ( ostia_ICE,GEOS_NLON,GEOS_NLAT,FillValue )

! Set Remaining UNDEV Values over Antarctica to First Defined Value in Latitude
!------------------------------------------------------------------------------
        do i=1,GEOS_NLON
           j=1
        do while( ostia_SST(i,j).eq.FillValue )
           j=j+1
        enddo
        do k=1,j-1
                  ostia_SST(i,k) = ostia_SST(i,j)
        enddo
        enddo
        
        do i=1,GEOS_NLON
           j=1
        do while( ostia_ICE(i,j).eq.FillValue )
           j=j+1
        enddo
        do k=1,j-1
                  ostia_ICE(i,k) = ostia_ICE(i,j)
        enddo
        enddo
        
!       write(55) ostia_SST
!       write(55) ostia_ICE

!---------------------------------------------------------------------------
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
        WRITE(91) HEADER
        WRITE(91) ostia_SST
        WRITE(92) HEADER
        WRITE(92) ostia_ICE
!---------------------------------------------------------------------------
 98     FORMAT(I4)
 99     FORMAT(I4)

        DEALLOCATE(reynolds_SST)
        DEALLOCATE(reynolds_ICE)
        DEALLOCATE(reynolds_DUM)

        DEALLOCATE(ostia_SST)
        DEALLOCATE(ostia_ICE)
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

