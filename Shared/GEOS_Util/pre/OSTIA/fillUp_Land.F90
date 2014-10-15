!
        SUBROUTINE fillUp_Land(NLAT, NLON, LAT, LON, SST_in, ICE_in, SST_out, ICE_out)
!----------------------------------------------------------------------
!/gpfsm/dnb31/sakella/processData/OLDstuff/get_SST_FRACI/land_fill.m
          IMPLICIT NONE

! GET TO KNOW THESE BY ncdump -h
          REAL, PARAMETER :: sst_FillValue    = -32768
          REAL, PARAMETER :: Ice_FillValue    = -128

          INTEGER,           INTENT(IN)    :: NLAT, NLON

          REAL,              INTENT(IN)    :: LAT(NLAT)
          REAL,              INTENT(IN)    :: LON(NLON)

          REAL,              INTENT(IN)    :: SST_in(NLON,NLAT)
          REAL,              INTENT(IN)    :: ICE_in(NLON,NLAT)

          REAL,              INTENT(INOUT) :: SST_out(NLON,NLAT)
          REAL,              INTENT(INOUT) :: ICE_out(NLON,NLAT)

! Local variables
          INTEGER                          :: k
          REAL, ALLOCATABLE                :: SST_lon(:, :), SST_lat(:, :)
!---------------------------------------------------------------------------
          ALLOCATE( SST_lon(NLON,NLAT)); ALLOCATE( SST_lat(NLON,NLAT))

          SST_out  = SST_in
          ICE_out  = ICE_in
          
          CALL fix_Antarctic(NLAT, NLON, LAT, SST_out, ICE_out, &
                             sst_FillValue, Ice_FillValue) 

! interp along lat and lon
!----------------------------------------------------------------------
          SST_lon = SST_out
          DO k = 1, NLAT
               CALL Interp1d(NLON, LON, SST_out(:,k), sst_FillValue, SST_lon(:,k)) 
          END DO

          SST_lat = SST_out
          DO k = 1, NLON
               CALL Interp1d(NLAT, LAT, SST_out(k,:), sst_FillValue, SST_lat(k,:)) 
          END DO

          WHERE( (SST_lon > 320) .OR. (SST_lon <= 260))          ! few lat, >60N have no sea at all.
             SST_lon = SST_lat
          END WHERE 

! combine lon & lat interp fields
!----------------------------------------------------------------------
          WHERE( SST_in == sst_FillValue)
              SST_out = 0.5*(SST_lon + SST_lat)
          ELSE WHERE
              SST_out = SST_in
          END WHERE

          DEALLOCATE( SST_lon); DEALLOCATE( SST_lat)

! Where ice fraction is NaN, fill it with 0.
!----------------------------------------------------------------------
          WHERE ( ICE_out == Ice_FillValue)
               ICE_out = 0.0
          END WHERE

!----------------------------------------------------------------------
        CONTAINS
!----------------------------------------------------------------------
          SUBROUTINE fix_Antarctic(NLAT, NLON, LAT, SST, ICE, sst_UNDEF, Ice_UNDEF) 
! Replace NaN over Antarctic with some values

              IMPLICIT NONE
              
              INTEGER,   INTENT(IN)      :: NLAT, NLON

              REAL,      INTENT(IN)      :: sst_UNDEF, Ice_UNDEF
              REAL,      INTENT(IN)      :: LAT(NLAT)
              REAL,      INTENT(INOUT)   :: SST(NLON,NLAT)
              REAL,      INTENT(INOUT)   :: ICE(NLON,NLAT)

              INTEGER                    :: iLat
              REAL                       :: min_sst
!             ------------------------------------------------------
              min_sst   =  MIN (MINVAL(SST, (SST /= sst_UNDEF)), 273.15)

              DO iLat = 1, 600                            ! Antarctic land mass should be within lat(600)
                 WHERE (SST(:,iLat) == sst_UNDEF)            
                   SST(:,iLat) = min_sst                  ! set it to the coldest temp.
                 ENDWHERE

                 WHERE ( ICE(:,iLat) == Ice_UNDEF) 
                   ICE(:,iLat) = 1.0                      ! assume Antarctic LAND mass is always FROZEN, EVERYWHERE.
                 ENDWHERE
              END DO
            
          END SUBROUTINE fix_Antarctic
!----------------------------------------------------------------------
          SUBROUTINE Interp1d(NVAR, VarAxis, VAR_in, UNDEF, VAR_out) 
! Replace NaN along any latitude
              IMPLICIT NONE
              
              INTEGER,   INTENT(IN)      :: NVAR                        ! NLON or NLAT

              REAL,      INTENT(IN)      :: UNDEF                       ! UNDEF value
              REAL,      INTENT(IN)      :: VarAxis(NVAR)               ! either LAT(:) or LON(:)
              REAL,      INTENT(IN)      :: VAR_in (NVAR)               ! field with UNDEF
              REAL,      INTENT(INOUT)   :: VAR_out(NVAR)               ! field with filled UNDEF

              INTEGER                    :: k, i1, i2
              REAL                       :: sl
              INTEGER                    :: nUNDEF
              INTEGER, ALLOCATABLE       :: loc_UNDEF(:)
              REAL,    ALLOCATABLE       :: VAR_temp(:)

              nUNDEF = COUNT(VAR_in == UNDEF)

              IF ( (nUNDEF == NVAR) .OR. (nUNDEF == 0) ) THEN           ! land or sea everywhere. 
                  RETURN                                     
              ELSE
                  ALLOCATE( loc_UNDEF(nUNDEF))
                  ALLOCATE( VAR_temp (NVAR))
              END IF
              
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
!----------------------------------------------------------------------
        END SUBROUTINE fillUp_Land
!
