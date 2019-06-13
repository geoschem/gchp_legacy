!

      SUBROUTINE fill_Land (q,im,jm,undef)
!---------------------------------------------------------------------------
      implicit none

      integer  im,jm
      real     undef
      real     q(im,jm)
      integer  i,j,k,L,i0,nundef
      real     qz(im)
      real     dist,dq
!---------------------------------------------------------------------------

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

      enddo ! j=1,jm

      return
!---------------------------------------------------------------------------
      END SUBROUTINE fill_Land
!

