!
! Based on locate.for
! see: ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/tools/locate.for

      SUBROUTINE convert_nsidc_grid(iHem, lat, lon, ii, kk)
!---------------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER,  INTENT(IN)    :: iHem      ! which Hemisphere? 1= NH, 2= SH
      REAL,     INTENT(IN)    :: lat, lon  ! lat & lon  values for which we need (i,j) coordinates on nsidc grid          
      INTEGER,  INTENT(OUT)   :: ii, kk    ! (i,j) coordinates on nsidc grid
!---------------------------------------------------------------------------
      REAL                    :: SLAT,E,RE,PI, E2
      REAL                    :: alat,alon,x,y
      REAL                    :: cell(2), xydist(2,2)
      REAL                    :: SGN, delta
      INTEGER                 :: numy(2,3)
      INTEGER                 :: gtype
      INTEGER                 :: iTrans
      INTEGER                 :: i, j
      INTEGER                 :: jj

      INTEGER, PARAMETER      :: iSTRICT_Protection = 0

      data numy     / 896, 664, 448, 332, 224, 166 /
      data cell     / 12.5d0 , 25.0d0 /
      data xydist   / 3850.0d0 , 5350.0d0 , 3950.0d0 , 3950.0d0 /

      SLAT = 70.d0
      RE = 6378.273d0
      E2 = .006693883d0
      PI = 3.141592654d0
      E =  SQRT(E2)
! ..........................................................................
! SSM/I grid cell size. gtype: (1) 12.5 Km, (2) 25.0 Km
      gtype = 2 
! Define the sign and meridian offset (delta) for the SSM/I grids.
        IF ( iHem.eq.1) THEN
           SGN   = 1.0d0
           delta = 45.d0
        ELSE
           SGN   = -1.0d0
           delta = 0.0d0
        ENDIF
! Translation Type. iTrans: (1) Convert I,J to Latitude, Longitude, (2) Convert Latitude, Longitude to I,J
        iTrans = 2       
!--------------------------------------------------------------------------
! Start translation
!
        IF( iTrans.EQ.1) THEN
! Obtain the I,J position of the grid cell to transform to Latitude 
! and Longitude
          print *, 'Enter the column number'
          if(iHem.eq.1 .and. gtype.eq.2) &
                          print *,'the valid range is (1-304)'
          if(iHem.eq.2 .and. gtype.eq.2) &
                          print *,'the valid range is (1-316)'
          read *, j
          print *, 'Enter the row number'
          if(iHem.eq.1 .and. gtype.eq.2) &
                          print *,'the valid range is (1-448)'
          if(iHem.eq.2 .and. gtype.eq.2) &
                          print *,'the valid range is (1-332)'
          read *, i
! Convert I,J pairs to x and y distances from origin. For some image
! display programs, the grid will be 'flipped' in the 'Y' direction.
!+
! Changed j for i and i for j to be consistant, NAS (11/95).
!-
          x  = ((j-1)*cell(gtype))-(xydist(1,ihem)-cell(gtype)/2.d0)
          kk = numy(ihem,gtype)-(i-1)
          y  = ((kk-1)*cell(gtype))-(xydist(2,ihem)-cell(gtype)/2.d0)
! Transform x and y distances to latitude and longitude
          CALL mapxy (x,y,alat,alon,SLAT,SGN,E,RE)
! Transform radians to degrees.
          alon   =  alon*180.d0/PI
          alat   =  alat*180.d0/PI
          alon   =  alon-delta
! Convert longitude to positive degrees
          IF ( alon .LE. 0.0d0)      alon  =  alon+360.d0
          IF ( alon .GE. 360.0d0)    alon  =  alon-360.d0
! Print the latitude and longitude for the center of the I,J cell.
          PRINT *, alat, alon
!
        ELSE  ! iTrans = 2
! Obtain the latitude and longitude pair and transform to cell where
! that pair is located.
!         PRINT *,'Enter latitude and longitude (positive values):'
!         READ *,lat,lon
! Transform degrees to radians
          alat   =   ABS(lat)*PI/180.d0
          alon   =   (lon+delta)*PI/180.d0
! Transform latitude and longitude to x and y distances from origin
          CALL mapll (x,y,alat,alon,SLAT,SGN,E,RE)
! Convert x and y distances from origin to I,J pair (ii,jj)
          ii  =  NINT((x+xydist(1,ihem)-cell(gtype)/2.d0)/cell(gtype))+1
          jj  =  NINT((y+xydist(2,ihem)-cell(gtype)/2.d0)/cell(gtype))+1
! Flip grid orientation in the 'Y' direction
          kk=numy(ihem,gtype)-(jj-1)
! Print the I,J location of the cell
!         PRINT *, ii, kk

!------------------------MAKE SURE [I,J] location makes sense!!------------
          IF( iSTRICT_Protection) THEN
             IF( (iHem .EQ.1) .AND. (gtype .EQ.2)) THEN
                IF( (ii .LT. 1) .OR. (ii .GT. 304)) THEN
                   PRINT *, 'NSIDC Grid conversion Out of bounds'
                   PRINT *, 'iX = ', ii, ' for NH'
                   STOP
                END IF
             END IF
             IF( (iHem .EQ.2) .AND. (gtype .EQ.2)) THEN
                IF( (ii .LT. 1) .OR. (ii .GT. 316)) THEN
                   PRINT *, 'NSIDC Grid conversion Out of bounds'
                   PRINT *, 'iX = ', ii, ' for SH'
                   STOP
                END IF
             END IF

             IF( (iHem .EQ.1) .AND. (gtype .EQ.2)) THEN
                IF( (kk .LT. 1) .OR. (kk .GT. 448)) THEN
                   PRINT *, 'NSIDC Grid conversion Out of bounds'
                   PRINT *, 'iY = ', kk, ' for NH'
                   STOP
                END IF
             END IF
             IF( (iHem .EQ.2) .AND. (gtype .EQ.2)) THEN
                IF( (kk .LT. 1) .OR. (kk .GT. 332)) THEN
                   PRINT *, 'NSIDC Grid conversion Out of bounds'
                   PRINT *, 'iY = ', kk, ' for SH'
                   STOP
                END IF
             END IF
          END IF  ! IF( iSTRICT_Protection)
!--------------------------------------------------------------------------
        END IF    !IF( itrans.EQ.1)
!--------------------------------------------------------------------------
        CONTAINS
!---------------------------------------------------------------------------
            SUBROUTINE mapxy(X,Y,ALAT,ALONG,SLAT,SGN,E,RE)
!$*****************************************************************************
!$                                                                            *
!$                                                                            *
!$    DESCRIPTION:                                                            *
!$                                                                            *
!$    This subroutine converts from Polar Stereographic (X,Y) coordinates     *
!$    to geodetic latitude and longitude for the polar regions. The equations *
!$    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
!$    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
!$    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
!$    details.                                                                *
!$                                                                            *
!$                                                                            *
!$    ARGUMENTS:                                                              *
!$                                                                            *
!$    Variable    Type        I/O    Description                              *
!$                                                                            *
!$    X          REAL*4        I     Polar Stereographic X Coordinate (km)    *
!$    Y          REAL*4        I     Polar Stereographic Y Coordinate (km)    *
!$    ALAT       REAL*4        O     Geodetic Latitude (degrees, +90 to -90)  *
!$    ALONG      REAL*4        O     Geodetic Longitude (degrees, 0 to 360)   *
!$                                                                            *
!$                                                                            *
!$                  Written by C. S. Morris - April 29, 1985                  *
!$                  Revised by C. S. Morris - December 11, 1985               *
!$                                                                            *
!$                  Revised by V. J. Troisi - January 1990
!$                  SGN - provide hemisphere dependency (+/- 1)
!$
!$*****************************************************************************
            IMPLICIT NONE
            REAL*4             :: X,Y,ALAT,ALONG,E,E2
            REAL               :: SLAT, SGN, RE, SL, RHO, CM, T, CHI
!$*****************************************************************************
!$                                                                            *
!$    DEFINITION OF CONSTANTS:                                                *
!$                                                                            *
!$    Conversion constant from degrees to radians = 57.29577951.              *
            REAL, PARAMETER    :: CDR  = 57.29577951d0
            REAL, PARAMETER    :: PI   = 3.141592654d0
!$                                                                            *
!$*****************************************************************************
             E2=E*E
             SL = SLAT*PI/180.d0
             RHO=SQRT(X**2+Y**2)
      IF ( RHO.GT.0.1d0) THEN

             CM    =COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
             T     =TAN((PI/4.0)-(SL/(2.0)))/((1.0-E*SIN(SL))/(1.0+E*SIN(SL)))**(E/2.0)

             IF ( ABS(SLAT-90.d0).LT.1.E-5) THEN
                 T = RHO*SQRT((1.d0+E)**(1.d0+E)*(1.d0-E)**(1.d0-E))/2.d0/RE
             ELSE
                 T = RHO*T/(RE*CM)
             END IF

             CHI=(PI/2.0d0)-2.0d0*ATAN(T)
             ALAT=CHI+((E2/2.0d0)+(5.0d0*E2**2.0d0/24.0d0)+(E2**3.0d0/12.0d0))*SIN(2*CHI)+   &
                      ((7.0*E2**2.0/48.0)+(29.0*E2**3/240.0))*SIN(4.0*CHI)+                  &
                      (7.0*E2**3.0/120.0)*SIN(6.0*CHI)
             ALAT=SGN*ALAT
             ALONG=ATAN2(SGN*X,-SGN*Y)
             ALONG=SGN*ALONG
             RETURN
      ELSE
             ALAT  =  90.d0*SGN
             ALONG =  0.0d0
             RETURN
      END IF

      END SUBROUTINE mapxy
!---------------------------------------------------------------------------
      SUBROUTINE mapll(X,Y,ALAT,ALONG,SLAT,SGN,E,RE)
!$*****************************************************************************
!$                                                                            *
!$                                                                            *
!$    DESCRIPTION:                                                            *
!$                                                                            *
!$    This subroutine converts from geodetic latitude and longitude to Polar  *
!$    Stereographic (X,Y) coordinates for the polar regions.  The equations   *
!$    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
!$    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
!$    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
!$    details.                                                                *
!$                                                                            *
!$                                                                            *
!$    ARGUMENTS:                                                              *
!$                                                                            *
!$    Variable    Type        I/O    Description                              *
!$                                                                            *
!$    ALAT       REAL*4        I     Geodetic Latitude (degrees, +90 to -90)  *
!$    ALONG      REAL*4        I     Geodetic Longitude (degrees, 0 to 360)   *
!$    X          REAL*4        O     Polar Stereographic X Coordinate (km)    *
!$    Y          REAL*4        O     Polar Stereographic Y Coordinate (km)    *
!$                                                                            *
!$                                                                            *
!$                  Written by C. S. Morris - April 29, 1985                  *
!$                  Revised by C. S. Morris - December 11, 1985               *
!$                                                                            *
!$                  Revised by V. J. Troisi - January 1990                    *
!$                  SGN - provides hemisphere dependency (+/- 1)              *
!$                  Revised by Xiaoming Li - October 1996                     *
!$                  Corrected equation for RHO                                *
!$*****************************************************************************
            REAL*4             :: X,Y,ALAT,ALONG,E,E2,SLAT,MC
            REAL               :: SGN, RE, T, RHO, SL, TC
!$*****************************************************************************
!$                                                                            *
!$    DEFINITION OF CONSTANTS:                                                *
!$                                                                            *
!$    Conversion constant from degrees to radians = 57.29577951.              *
            REAL, PARAMETER    :: CDR  = 57.29577951d0
            REAL, PARAMETER    :: PI   = 3.141592654d0
!$                                                                            *
!$*****************************************************************************
            E2=E*E

!     Compute X and Y in grid coordinates.
            IF ( ABS(ALAT).LT.PI/2.d0) THEN
               T=TAN(PI/4.d0-ALAT/2.d0)/((1.d0-E*SIN(ALAT))/(1.d0+E*SIN(ALAT)))**(E/2.d0)
               IF (ABS(90.d0-SLAT).LT.1.E-5) THEN
                  RHO=2.d0*RE*T/((1.d0+E)**(1.d0+E)*(1.d0-E)**(1.d0-E))**(1/2.d0)
               ELSE
                  SL=SLAT*PI/180.d0
                  TC=TAN(PI/4.d0-SL/2.d0)/((1.d0-E*SIN(SL))/(1.d0+E*SIN(SL)))**(E/2.d0)
                  MC=COS(SL)/SQRT(1.0d0-E2*(SIN(SL)**2))
                  RHO=RE*MC*T/TC
               END IF
               Y = -RHO*SGN*COS(SGN*ALONG)
               X =  RHO*SGN*SIN(SGN*ALONG)
            ELSE
               X = 0.0d0
               Y = 0.0d0
            END IF
      END SUBROUTINE mapll
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
      END SUBROUTINE convert_nsidc_grid
!

