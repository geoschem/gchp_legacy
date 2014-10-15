      SUBROUTINE bin2bin ( qi,imi,jmi,qo,imo,jmo,undef )
!***********************************************************************
!
!  PURPOSE:
!  ========
!    Bin an input field, qi(imi,jmi), to an output array qo(imo,jmo)
!
!  INPUT:
!  ======
!    qi ......... Input array(imi,jmi)
!
!  OUTPUT:
!  =======
!    qo ......... Output array(imo,jmo)
!
!  NOTES:
!  ======
!    Input and Output arrays are assumed Dateline_Edge and Pole_Edge.
!             Each box is referenced by the latitude and longitude of
!             its southwest corner, not its center point.  Thus,
!             the value associated with a coordinate actually
!             represents the value centered to the northeast of that point.
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      IMPLICIT NONE

      INTEGER,    INTENT(IN)   :: imi, jmi
      INTEGER,    INTENT(IN)   :: imo, jmo

      REAL,       INTENT(IN)   :: undef
      REAL,       INTENT(IN)   :: qi(imi,jmi)
      REAL,       INTENT(OUT)  :: qo(imo,jmo)

! Local variables
      INTEGER                  :: i,j,ibeg,iend,jbeg,jend
      INTEGER                  :: ii,jj,itmp
      real*8   dlam, dphi
      real*8    sum1,sum2
      real*8    zlat,zlon
      real*8    lon1,lon2,wx
      real*8    lat1,lat2,wy
      real*8    lonbeg,lonend,lat,coslat
      real*8    latbeg,latend
      real*8    pi,dz
      real*8    lon_out(imo)
      real*8    lat_out(jmo)

      pi   = 4*DATAN(1.0D0)
      dz   =   pi/jmi
      dlam = 2*pi/imo
      dphi =   pi/jmo

! Compute Output Lons and Lats
! ----------------------------
      lon_out(1) = -pi+0.5D0*dlam
      DO i = 2, imo
        lon_out(i) = lon_out(i-1) + dlam
      ENDDO

      lat_out(1) = -pi*0.5D0+0.5D0*dphi
      DO j = 2, jmo-1
        lat_out(j) = lat_out(j-1) + dphi
      ENDDO
      lat_out(jmo) =  pi*0.5D0-0.5D0*dphi

! Bin Input Array to Output Array
! -------------------------------
      DO j=1,jmo
         DO i=1,imo

           zlat = lat_out(j); zlon = lon_out(i)

           latbeg = zlat-dphi*0.5D0; latend = zlat+dphi*0.5D0
           lonbeg = zlon-dlam*0.5D0; lonend = zlon+dlam*0.5D0

            ibeg = 0.5D0+(lonbeg+pi)      /dz
            iend = 0.5D0+(lonend+pi)      /dz
            jbeg = 0.5D0+(latbeg+pi*0.5D0)/dz
            jend = 0.5D0+(latend+pi*0.5D0)/dz

! Check for Begin and End Errors due to Truncation
! ------------------------------------------------
      lon2 = -pi +  ibeg   *dz
      lon1 = -pi + (iend-1)*dz
      if( lon2.lt.lonbeg ) ibeg = ibeg + 1
      if( lon1.gt.lonend ) ibeg = ibeg - 1
      if( ibeg.gt.imi ) then
          print *, 'ERROR after Truncation Check!'
          print *, 'ibeg: ',ibeg
          stop
      endif
      if( ibeg.lt.0 ) then
          print *, 'ERROR after Truncation Check!'
          print *, 'ibeg: ',ibeg
          stop
      endif

      lat2 = -pi*0.5D0 +  jbeg   *dz
      lat1 = -pi*0.5D0 + (jend-1)*dz
      if( lat2.lt.latbeg ) jbeg = jbeg + 1
      if( lat1.gt.latend ) jbeg = jbeg - 1

            IF( jbeg.lt.0 .or. jend.gt.jmi ) THEN
              PRINT *, 'Bounding jbeg jend values Error for (i,j): ',i,j
              PRINT *, 'jbeg: ',jbeg,' latbeg: ',latbeg*180/pi
              PRINT *, 'jend: ',jend,' latend: ',latend*180/pi
              STOP
            ENDIF
            IF( jbeg.lt.1 ) jbeg = 1

            sum1 = 0.0D0 ; sum2 = 0.0D0

            DO jj=jbeg,jend
             lat2 = -pi*0.5D0 +  jj       *dz
             lat  = -pi*0.5D0 + (jj-0.5D0)*dz
             lat1 = -pi*0.5D0 + (jj-1)    *dz

             coslat = max( min(dcos(lat),1.0D0),0.0D0 )

             if( lat2.lt.latbeg .or. lat1.gt.latend ) then
                 print *
                 print *, ' ERROR!'
                 print *
                 print *, '     j: ',j
                 print *
                 print *, 'latend: ',latend*180/pi
                 print *, '  zlat: ',zlat  *180/pi
                 print *, 'latbeg: ',latbeg*180/pi
                 print *
                 print *, '    jj: ',jj
                 print *, '  jend: ',jend,' r_jend: ',0.5D0+(latend+pi*0.5D0)/dz
                 print *, '  jbeg: ',jbeg,' r_jbeg: ',0.5D0+(latbeg+pi*0.5D0)/dz
                 print *
                 print *, 'lat2  : ',lat2  *180/pi
                 print *, 'lat   : ',lat   *180/pi
                 print *, 'lat1  : ',lat1  *180/pi
                 print *
                 print *, 'lat2-latbeg: ',lat2-latbeg
                 print *, 'lat1-latend: ',lat1-latend
                 stop
             endif

                                  wy = 1.0D0
             IF( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
             IF( lat2.gt.latend ) wy = (latend-lat1)/dz

             IF( iend.gt.imi ) THEN
               PRINT *, 'Bounding iend Error for (i,j): ',i,j
               PRINT *, 'jbeg: ',jbeg,' latbeg: ',latbeg*180/pi
               PRINT *, 'jend: ',jend,' latend: ',latend*180/pi
               PRINT *, 'iend: ',iend,' lonend: ',lonend*180/pi
               PRINT *, 'iend: ',iend,' r_iend: ',0.5D0+(lonend+pi)/dz
               STOP
             ENDIF

             IF(ibeg.ge.1) THEN
               DO ii=ibeg,iend
                 lon1 = -pi  + (ii-1)*dz; lon2 = -pi  +  ii   *dz
                                      wx = 1.0D0
                 IF( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
                 IF( lon2.gt.lonend ) wx = (lonend-lon1)/dz
                 IF( wx.lt.0.0 .or. wy.lt.0.0 ) THEN
                     PRINT *
                     PRINT *, '     i: ',i
                     PRINT *
                     PRINT *, 'lonend: ',lonend*180/pi
                     PRINT *, '  zlon: ',zlon  *180/pi
                     PRINT *, 'lonbeg: ',lonbeg*180/pi
                     PRINT *
                     PRINT *, '    ii: ',ii
                     PRINT *, '  iend: ',iend,' r_iend: ',0.5D0+(latend+pi)/dz
                     PRINT *, '  ibeg: ',ibeg,' r_ibeg: ',0.5D0+(latbeg+pi)/dz
                     PRINT *
                     PRINT *, '  lon2: ',lon2  *180/pi
                     PRINT *, '  lon1: ',lon1  *180/pi
                     PRINT *, '    wx: ',wx
                     PRINT *
                     PRINT *, '     j: ',j
                     PRINT *
                     PRINT *, 'latend: ',latend*180/pi
                     PRINT *, '  zlat: ',zlat  *180/pi
                     PRINT *, 'latbeg: ',latbeg*180/pi
                     PRINT *
                     PRINT *, '    jj: ',jj
                     PRINT *, '  jend: ',jend,' r_jend: ',0.5D0+(latend+pi*0.5D0)/dz
                     PRINT *, '  jbeg: ',jbeg,' r_jbeg: ',0.5D0+(latbeg+pi*0.5D0)/dz
                     PRINT *
                     PRINT *, 'lat2  : ',lat2  *180/pi
                     PRINT *, 'lat   : ',lat   *180/pi
                     PRINT *, 'lat1  : ',lat1  *180/pi
                     PRINT *, '    wy: ',wy
                     STOP
                 ENDIF
                 if( qi(ii,jj).ne.undef ) then
                     sum1 = sum1 + qi(ii,jj)*coslat*wx*wy
                     sum2 = sum2 +           coslat*wx*wy
                 endif
               ENDDO
             ELSE
                 itmp = 0.5D0+(lonbeg+0.1D0*dz+3*pi)/dz
                 IF( itmp.gt.imi ) THEN
                   PRINT *, 'Bounding itmp Error for (i,j): ',i,j
                   PRINT *, 'jbeg: ',jbeg,' latbeg: ',latbeg*180/pi
                   PRINT *, 'jend: ',jend,' latend: ',latend*180/pi
                   PRINT *, 'itmp: ',itmp,' lontmp: ',lonbeg*180/pi
                   STOP
                 ENDIF
               DO ii=itmp,imi
                 lon1 = -pi  + (ii-1)*dz; lon2 = -pi  +  ii   *dz
                                           wx = 1.0D0
                 IF( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
                 IF( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
                 IF( wx.lt.0.0 .or. wy.lt.0.0 ) THEN
                     PRINT *
                     PRINT *, '     i: ',i
                     PRINT *
                     PRINT *, 'lonend: ',lonend*180/pi
                     PRINT *, '  zlon: ',zlon  *180/pi
                     PRINT *, 'lonbeg: ',lonbeg*180/pi
                     PRINT *
                     PRINT *, '    ii: ',ii
                     PRINT *, '  iend: ',iend,' r_iend: ',0.5D0+(latend+pi)/dz
                     PRINT *, '  ibeg: ',ibeg,' r_ibeg: ',0.5D0+(latbeg+pi)/dz
                     PRINT *
                     PRINT *, '  lon2: ',lon2  *180/pi
                     PRINT *, '  lon1: ',lon1  *180/pi
                     PRINT *, '    wx: ',wx
                     PRINT *
                     PRINT *, '     j: ',j
                     PRINT *
                     PRINT *, 'latend: ',latend*180/pi
                     PRINT *, '  zlat: ',zlat  *180/pi
                     PRINT *, 'latbeg: ',latbeg*180/pi
                     PRINT *
                     PRINT *, '    jj: ',jj
                     PRINT *, '  jend: ',jend,' r_jend: ',0.5D0+(latend+pi*0.5D0)/dz
                     PRINT *, '  jbeg: ',jbeg,' r_jbeg: ',0.5D0+(latbeg+pi*0.5D0)/dz
                     PRINT *
                     PRINT *, 'lat2  : ',lat2  *180/pi
                     PRINT *, 'lat   : ',lat   *180/pi
                     PRINT *, 'lat1  : ',lat1  *180/pi
                     PRINT *, '    wy: ',wy
                     STOP
                 ENDIF
                 if( qi(ii,jj).ne.undef ) then
                     sum1 = sum1 + qi(ii,jj)*coslat*wx*wy
                     sum2 = sum2 +           coslat*wx*wy
                 endif
               ENDDO
               DO ii=1,iend
                  lon1 = -pi  + (ii-1)*dz; lon2 = -pi  +  ii   *dz
                                       wx = 1.0D0
                  IF( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
                  IF( lon2.gt.lonend ) wx = (lonend-lon1)/dz
                  IF( wx.lt.0.0 .or. wy.lt.0.0 ) THEN
                      PRINT *
                      PRINT *, '     i: ',i
                      PRINT *
                      PRINT *, 'lonend: ',lonend*180/pi
                      PRINT *, '  zlon: ',zlon  *180/pi
                      PRINT *, 'lonbeg: ',lonbeg*180/pi
                      PRINT *
                      PRINT *, '    ii: ',ii
                      PRINT *, '  iend: ',iend,' r_iend: ',0.5D0+(latend+pi)/dz
                      PRINT *, '  ibeg: ',ibeg,' r_ibeg: ',0.5D0+(latbeg+pi)/dz
                      PRINT *
                      PRINT *, '  lon2: ',lon2  *180/pi
                      PRINT *, '  lon1: ',lon1  *180/pi
                      PRINT *, '    wx: ',wx
                      PRINT *
                      PRINT *, '     j: ',j
                      PRINT *
                      PRINT *, 'latend: ',latend*180/pi
                      PRINT *, '  zlat: ',zlat  *180/pi
                      PRINT *, 'latbeg: ',latbeg*180/pi
                      PRINT *
                      PRINT *, '    jj: ',jj
                      PRINT *, '  jend: ',jend,' r_jend: ',0.5D0+(latend+pi*0.5D0)/dz
                      PRINT *, '  jbeg: ',jbeg,' r_jbeg: ',0.5D0+(latbeg+pi*0.5D0)/dz
                      PRINT *
                      PRINT *, 'lat2  : ',lat2  *180/pi
                      PRINT *, 'lat   : ',lat   *180/pi
                      PRINT *, 'lat1  : ',lat1  *180/pi
                      PRINT *, '    wy: ',wy
                      STOP
                  ENDIF
                  if( qi(ii,jj).ne.undef ) then
                      sum1 = sum1 + qi(ii,jj)*coslat*wx*wy
                      sum2 = sum2 +           coslat*wx*wy
                  endif
               ENDDO
             ENDIF ! IF(ibeg.ge.1)
            ENDDO   ! DO jj=jbeg,jend

            if( sum2.ne.0.0D0 ) then
                qo(i,j) = sum1/sum2
            else
                qo(i,j) = undef
            endif

         ENDDO
      ENDDO

      RETURN
!--------------------------------------------------------------------------------
      END SUBROUTINE bin2bin

