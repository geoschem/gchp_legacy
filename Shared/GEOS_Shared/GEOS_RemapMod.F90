module GEOS_RemapMod

implicit none
private
public remap
interface remap; module procedure & 
          remap4_
end interface

contains

      subroutine remap4_ ( ple,u,v,thv,q,o3,phis_in,phis_out,ak,bk,im,jm,lm )

!***********************************************************************
!
!  Purpose
!     Driver for remapping fields to new topography
!
!  Argument Description
!     ple ...... model edge pressure
!     u  ....... model zonal      wind
!     v  ....... model meridional wind
!     thv  ..... model virtual potential  temperature
!     q  ....... model specific   humidity
!     o3  ...... model ozone
!     phis_in... model surface geopotential (input)
!     phis_out.. model surface geopotential (output)
!     ak ....... model vertical   dimension
!     bk ....... model vertical   dimension
!
!     im ....... zonal      dimension
!     jm ....... meridional dimension
!     lm ....... meridional dimension
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      use GEOS_GmapMod, only: gmap     
      implicit none
      integer  im,jm,lm

! Input variables
! ---------------
      real      ple(im,jm,lm+1)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real      thv(im,jm,lm)
      real        q(im,jm,lm)
      real       o3(im,jm,lm)
      real phis_in (im,jm)
      real phis_out(im,jm)

      real       ak(lm+1)
      real       bk(lm+1)

! Local variables
! ---------------
      real, allocatable ::  ps     (:,:)
      real, allocatable ::  phi    (:,:,:)
      real, allocatable ::  pke    (:,:,:)
      real, allocatable ::  ple_out(:,:,:)
      real, allocatable ::  pke_out(:,:,:)

      real, allocatable ::     delp(:,:,:)
      real, allocatable ::    u_out(:,:,:)
      real, allocatable ::    v_out(:,:,:)
      real, allocatable ::  thv_out(:,:,:)
      real, allocatable ::    q_in (:,:,:,:)
      real, allocatable ::    q_out(:,:,:,:)

      real    kappa,cp,rgas,eps,rvap
      integer i,j,L,n

      kappa = 2.0/7.0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      cp    = rgas/kappa

      allocate(  ps     (im,jm)      )
      allocate(  phi    (im,jm,lm+1) )
      allocate(  pke    (im,jm,lm+1) )
      allocate(  ple_out(im,jm,lm+1) )
      allocate(  pke_out(im,jm,lm+1) )

      allocate(     delp(im,jm,lm)   )
      allocate(    u_out(im,jm,lm)   )
      allocate(    v_out(im,jm,lm)   )
      allocate(  thv_out(im,jm,lm)   )
      allocate(    q_in (im,jm,lm,2) )
      allocate(    q_out(im,jm,lm,2) )

! Construct Input Heights
! -----------------------
      pke(:,:,:) = ple(:,:,:)**kappa 

      phi(:,:,lm+1) = phis_in(:,:)
      do L=lm,1,-1
      phi(:,:,L) = phi(:,:,L+1) + cp*thv(:,:,L)*( pke(:,:,L+1)-pke(:,:,L) )
      enddo
      
! Compute new surface pressure consistent with output topography
! --------------------------------------------------------------
      do j=1,jm
      do i=1,im
           L = lm
           do while ( phi(i,j,L).lt.phis_out(i,j) )
           L = L-1
           enddo
           ps(i,j) = ple(i,j,L+1)*( 1+(phi(i,j,L+1)-phis_out(i,j))/(cp*thv(i,j,L)*pke(i,j,L+1)) )**(1.0/kappa)
      enddo
      enddo

! Construct fv pressure variables using new surface pressure
! ----------------------------------------------------------
      do L=1,lm+1
      do j=1,jm
      do i=1,im
!!!    ple_out(i,j,L) =      ak(L) + bk(L)*  ps(i,j)
       ple_out(i,j,L) = ple(i,j,L) + bk(L)*( ps(i,j)-ple(i,j,lm+1) )
      enddo
      enddo
      enddo
      pke_out(:,:,:) = ple_out(:,:,:)**kappa 

! Map original fv state onto new eta grid
! ---------------------------------------
      q_in(:,:,:,1) =  q(:,:,:)
      q_in(:,:,:,2) = o3(:,:,:)

      call gmap ( im,jm,2 , kappa, &
                  lm, pke    ,ple    ,u    ,v    ,thv    ,q_in , &
                  lm, pke_out,ple_out,u_out,v_out,thv_out,q_out)

      ple(:,:,:) = ple_out(:,:,:)
        u(:,:,:) =   u_out(:,:,:)
        v(:,:,:) =   v_out(:,:,:)
      thv(:,:,:) = thv_out(:,:,:)
        q(:,:,:) =   q_out(:,:,:,1)
       o3(:,:,:) =   q_out(:,:,:,2)

      deallocate(  ps      )
      deallocate(  phi     )
      deallocate(  pke     )
      deallocate(  ple_out )
      deallocate(  pke_out )

      deallocate(     delp )
      deallocate(    u_out )
      deallocate(    v_out )
      deallocate(  thv_out )
      deallocate(    q_in  )
      deallocate(    q_out )

      return
      end subroutine remap4_

end module GEOS_RemapMod
