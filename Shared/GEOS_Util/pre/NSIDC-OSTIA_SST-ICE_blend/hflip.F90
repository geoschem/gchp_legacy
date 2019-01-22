!
! flip lat: [0, 360] to [-180, 180]
          SUBROUTINE hflip( q, im, jm)
!---------------------------------------------------------------------------
          IMPLICIT NONE
          INTEGER  im,jm,i,j
          REAL   q(im,jm), dum(im)

            DO j=1,jm
              DO i=1,im/2
                 dum(i)      = q(i+im/2,j)
                 dum(i+im/2) = q(i,j)
              ENDDO
              q(:,j) = dum(:)
            ENDDO
!---------------------------------------------------------------------------
          END SUBROUTINE hflip
!
