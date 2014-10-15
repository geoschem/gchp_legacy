!
      SUBROUTINE get_nsidc_icec(fileName_NH, fileName_SH, iDebug,      &
                                NLAT, NLON,                            &
                                LAT, LON, reynolds_ICE, reynolds_MASK, &
                                ICE, MASK, myUNDEF)
!---------------------------------------------------------------------------
          IMPLICIT NONE
          CHARACTER (LEN = *),          INTENT(IN)       :: fileName_NH, fileName_SH
          INTEGER,                      INTENT(IN)       :: iDebug
          INTEGER,                      INTENT(IN)       :: NLAT, NLON
          REAL, DIMENSION(NLAT),        INTENT(IN)       :: LAT
          REAL, DIMENSION(NLON),        INTENT(IN)       :: LON
          REAL,                         INTENT(IN)       :: myUNDEF
          REAL,    DIMENSION(NLON,NLAT),INTENT(IN)       :: reynolds_ICE
          REAL,    DIMENSION(NLON,NLAT),INTENT(IN)       :: reynolds_MASK

          REAL,    DIMENSION(NLON,NLAT),INTENT(OUT)      :: ICE
          REAL,    DIMENSION(NLON,NLAT),INTENT(OUT)      :: MASK

!         ..................................................................
          INTEGER, PARAMETER   :: nh_nx =  304, nh_ny = 448
          INTEGER, PARAMETER   :: sh_nx =  316, sh_ny = 332

          REAL                 :: nh_icec(nh_nx, nh_ny)
          REAL                 :: sh_icec(sh_nx, sh_ny)

          REAL                 :: nh_mask(nh_nx, nh_ny)
          REAL                 :: sh_mask(sh_nx, sh_ny)

          INTEGER              :: iLat, iLon
          INTEGER              :: iX,   iY
!---------------------------------------------------------------------------
! Initialize land-sea mask
          MASK = reynolds_MASK
! Initialize Ice Concentration
          ICE =  reynolds_ICE
!---------------------------------------------------------------------------
! Read NSIDC NH Ice Concentration
          CALL read_nsidc_icec(fileName_NH, nh_nx, nh_ny, &
                               nh_icec, nh_mask, myUNDEF)
! Read NSIDC SH Ice Concentration
          CALL read_nsidc_icec(fileName_SH, sh_nx, sh_ny, &
                               sh_icec, sh_mask, myUNDEF)
!---------------------------------------------------------------------------
! Get NSIDC NH ice on a regular lat-lon grid
! Start @ 40N and go up to NP
          DO iLat = 521, NLAT       ! LAT(521) ~ 40N
            DO iLon = 1, NLON
              CALL convert_nsidc_grid(1, LAT(iLat), LON(iLon), iX, iY)  ! iHem= 1 => NH

!             SSMI/S grid is not regular and has gaps. Make sure iX & iY are in bounds.
              IF( (iX .LT. 1) .OR. (iX .GT. nh_nx) .OR. &
                  (iY .LT. 1) .OR. (iY .GT. nh_ny)) THEN          
                   ICE(iLon, iLat)  = reynolds_ICE(iLon, iLat) ! If they are out of bounds, use Reynolds Ice.
              ELSE
                   ICE(iLon, iLat)  = nh_icec(iX, iY)
                   MASK(iLon, iLat) = nh_mask(iX, iY)
              END IF

            END DO
          END DO

! Get NSIDC SH ice on a regular lat-lon grid
! Start @ SP and go up to 40S
          DO iLat = 1, 201       ! LAT(201) ~ 40S
            DO iLon = 1, NLON
              CALL convert_nsidc_grid(2, LAT(iLat), LON(iLon), iX, iY)  ! iHem= 2 => SH

!             SSMI/S grid is not regular and has gaps. Make sure iX & iY are in bounds.
              IF( (iX .LT. 1) .OR. (iX .GT. sh_nx) .OR. &
                  (iY .LT. 1) .OR. (iY .GT. sh_ny)) THEN          
                   ICE(iLon, iLat)  = reynolds_ICE(iLon, iLat) ! If they are out of bounds, use Reynolds Ice.
              ELSE
                   ICE(iLon, iLat)  = sh_icec(iX, iY)
                   MASK(iLon, iLat) = sh_mask(iX, iY)
              END IF

            END DO
          END DO
!---------------------------------------------------------------------------
! Fill up Arctic pole void with Reynolds Ice
! Note that ICE is = myUNDEF (because nh_ice is undef there)
!         DO iLAT = 710, NLAT
!              ICE(:, iLat) = reynolds_ICE(:, iLat)
!         END DO
!
!---------------------------------------------------------------------------
! NSIDC Missing data 
!             IF( (ICE(iLon, iLat) == myUNDEF) .AND. (MASK(iLon, iLat) == 1) .AND. &
!                 (reynolds_ICE(iLon, iLat) /= myUNDEF)) THEN
!                  ICE(iLon, iLat) = reynolds_ICE(iLon, iLat)
!             END IF
! Interpolate across NP to fill up the gap
          CALL fill_NPOLE(ICE, NLON, NLAT, myUNDEF)
!---------------------------------------------------------------------------
! Unify values and fill NSIDC missing values w/ Reynolds
        WHERE ( (MASK ==1) .AND. (ICE == myUNDEF)  .AND. &      ! Mask: Zero over land. ONE over water
                (reynolds_ICE /= myUNDEF))                      ! Mask set in read_Reynolds
                ICE = reynolds_ICE
        END WHERE
! Unify Ice concentration value with Mask over water
        WHERE ( (MASK ==1) .AND. (ICE == myUNDEF))
            ICE = 0.0d0   ! set open ocean ice to 0.
        END WHERE
!---------------------------------------------------------------------------
! lon needs to be between (-180, 180). Flip to (0, 360)
        CALL hflip (ICE, NLON, NLAT)
!---------------------------------------------------------------------------
        IF( iDebug) THEN
            DO iLat = 1, nh_ny
               DO iLon = 1, nh_nx
                 WRITE(113,*) nh_mask(iLon, iLat), nh_icec(iLon, iLat)
               END DO 
            END DO 
            DO iLat = 1, sh_ny
               DO iLon = 1, sh_nx
                 WRITE(114,*) sh_mask(iLon, iLat), sh_icec(iLon, iLat)
               END DO 
            END DO 
        END IF           
!---------------------------------------------------------------------------
      END SUBROUTINE get_nsidc_icec
!
