!
! from Larry- to fill up NP data void
      SUBROUTINE fill_NPOLE (q,im,jm,undef)
!---------------------------------------------------------------------------
      IMPLICIT NONE

       INTEGER,                      INTENT(IN)       :: im,jm
       REAL,                         INTENT(IN)       :: undef
       REAL,                         INTENT(INOUT)    :: q(im,jm)

       INTEGER               i,j,j1,j2,iph,num
       REAL                  qi(im,jm)
       REAL                  qo(im,jm)
!---------------------------------------------------------------------------
       qi = q
       qo = q

       do i=1,im
           j1=jm
           do while( qi(i,j1).eq.undef )
              j1=j1-1
           end do
           num = jm-j1

!          go across the NP
           iph = i + im/2
           if( iph.gt.im ) iph = iph-im
           j2=jm
           do while( qi(iph,j2).eq.undef )
              j2=j2-1
           end do

!          linearly interp
           num = num+jm-j2
           do j=j1+1,jm
              qo(i,j) = qi(i,j1) + (qi(iph,j2)-qi(i,j1))*(j-j1)/num
           enddo
       enddo

       q = qo

      RETURN
!---------------------------------------------------------------------------
      END SUBROUTINE fill_NPOLE
!

