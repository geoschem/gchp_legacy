#include "unused_dummy.H"
!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !MODULE: m_topo_remap --- Wrapper to remap dyn vector due to topo change
!
! !INTERFACE:
module m_topo_remap

! !USES:

use m_dyn
implicit none
private

! !PUBLIC MEMBER FUNCTIONS:
public dyn_topo_remap  ! nothing else should be made public
public dyn_real_eta    ! nothing else should be made public

!
interface dyn_topo_remap
  module procedure dyn_topo_remap_
  module procedure topo_remap0_
  module procedure topo_remap1_
end interface
interface dyn_real_eta
  module procedure dyn_real_eta_
end interface

character(len=*), parameter :: myname = 'm_topo_remap'

! !DESCRIPTION: This module is a wrapper based on Larry Takacs wrapper
!               of S. J. Lin routine to remap dyn-vector give a change
!               in the topography.
! !REMARKS:
!   1. Nothing beyond dyn_topo_remap should ever be made public to 
!      to avoid conflict with similar routines elsewhere.
!   2. In all of these, wind-handling needs to be revisited since
!      SJ original routines are for d-grid winds (far as I know).
!
! !SEE ALSO: m_mapz, m_maph
!
! !REVISION HISTORY:
!
!  23Feb2013  Todling    Initial (wrapper) code.
!  20Nov2013  Todling    Add interface for converting to real-eta
!
!EOP
!-------------------------------------------------------------------------
CONTAINS
      subroutine dyn_topo_remap_( w_f,phis_new, dyntype, info )
      implicit none
      integer,intent(in) :: dyntype
      type(dyn_vect) w_f
      real,intent(in) :: phis_new(w_f%grid%im,w_f%grid%jm)
      integer,optional::info

      character(len=*), parameter :: myname_ = myname//'*dyn_topo_remap'
      real,allocatable ::  pk(:,:,:)
      real,allocatable :: pke(:,:,:)
      real,allocatable :: ple(:,:,:)
      real,allocatable :: thv(:,:,:)
      real kappa
      integer k,im,jm,km

      kappa = 2.0/7.0
      im=w_f%grid%im
      jm=w_f%grid%jm
      km=w_f%grid%km

!     check to see if remap is needed
!     only remap when max diff in topo larger than 1% of max value of topo
      if(maxval(abs(phis_new-w_f%phis))<0.01*maxval(w_f%phis)) then
        print*, myname_, ': no remap necessary, returning ...'
        if (present(info)) then
            info = 1
        endif
        return
      endif
      if (present(info)) then
         info = 0
      endif
      allocate(thv(im,jm,km))
      if(dyntype==5) then
         allocate ( pk(im,jm,km  ))
         allocate (ple(im,jm,km+1))
         allocate (pke(im,jm,km+1))
         ple(:,:,1) = w_f%grid%ak(1)
         do k=1,km
            ple(:,:,k+1) = ple(:,:,k) + w_f%delp(:,:,k)
         enddo
       ! convert virtual temperature to virtual potential temperature
         pke(:,:,:) = ple(:,:,:)**kappa
         do k=1,km
            pk(:,:,k) = ( pke(:,:,k+1)-pke(:,:,k) ) &
                       / ( kappa*log(ple(:,:,k+1)/ple(:,:,k)) )
         enddo
         thv = w_f%pt/pk
         deallocate (  pk )
         deallocate ( pke )
         deallocate ( ple )
      else 
          print *, ' not coded for old dyn'
          call exit(1)
          stop
      endif

      call topo_remap0_( w_f%ps,w_f%delp,w_f%u,w_f%v,thv,w_f%q,w_f%phis,phis_new, &
                         w_f%grid%ak,w_f%grid%bk,&
                         im,jm,km,4)

      if(dyntype==5) then
         allocate (ple(im,jm,km+1))
         allocate (pke(im,jm,km+1))
         allocate ( pk(im,jm,km  ))
         ple(:,:,1) = w_f%grid%ak(1)
         do k=1,km
            ple(:,:,k+1) = ple(:,:,k) + w_f%delp(:,:,k)
         enddo
         pke(:,:,:) = ple(:,:,:)**kappa
         do k=1,km
            pk(:,:,k) = ( pke(:,:,k+1)-pke(:,:,k) ) &
                       / ( kappa*log(ple(:,:,k+1)/ple(:,:,k)) )
         enddo
       ! convert virtual potential temperature to virtual temperature
         w_f%pt = thv*pk
         deallocate (  pk )
         deallocate ( pke )
         deallocate ( ple )
      endif
      w_f%phis = phis_new ! for consistency

      deallocate(thv)
      end subroutine dyn_topo_remap_

      subroutine topo_remap0_( ps,delp,u,v,thv,q,phis_in,phis_out,ak,bk,im,jm,lm,nq )

!***********************************************************************
!
!  Purpose
!     Driver for remapping fields to new topography
!
!  Argument Description
!     ps ....... model surface pressure
!     delp ..... model pressure thickness
!     u  ....... model zonal      wind
!     v  ....... model meridional wind
!     thv  ..... model virtual potential  temperature
!     q  ....... model specific   humidity; ozone; others
!     phis_in... model surface geopotential (input)
!     phis_out.. model surface geopotential (output)
!     ak ....... model vertical   dimension
!     bk ....... model vertical   dimension
!
!     im ....... zonal      dimension
!     jm ....... meridional dimension
!     lm ....... meridional dimension
!     nq ....... number of tracers including spec. hum.
!
! 24Feb2013  Todling  Addapted for dyn-vect needs
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer  im,jm,lm,nq

! Input variables
! ---------------
      real       ps(im,jm)
      real     delp(im,jm,lm)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real      thv(im,jm,lm)
      real        q(im,jm,lm,nq)
      real phis_in (im,jm)
      real phis_out(im,jm)

      real       ak(lm+1)
      real       bk(lm+1)

! Local variables
! ---------------
      integer L
      real,allocatable :: ple(:,:,:)

      allocate(ple(im,jm,lm+1))

      ple(:,:,1) = ak(1)
      do L=1,lm
!        ple(:,:,L) = ak(L) + bk(L)*ps(:,:)
         ple(:,:,L+1) = ple(:,:,L) + delp(:,:,L)
      enddo
      do L=1,lm
         delp(:,:,L) = ple(:,:,L+1)-ple(:,:,L)
      enddo

      call topo_remap1_( ple,u,v,thv,q,phis_in,phis_out,ak,bk,im,jm,lm,nq )

      ps   = ple(:,:,lm+1)
      delp = ple(:,:,2:)-ple(:,:,:lm)

      deallocate(ple)
      end subroutine topo_remap0_

      subroutine topo_remap1_( ple,u,v,thv,q,phis_in,phis_out,ak,bk,im,jm,lm,nq )

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
!     q  ....... model specific   humidity; ozone; others
!     phis_in... model surface geopotential (input)
!     phis_out.. model surface geopotential (output)
!     ak ....... model vertical   dimension
!     bk ....... model vertical   dimension
!
!     im ....... zonal      dimension
!     jm ....... meridional dimension
!     lm ....... meridional dimension
!     nq ....... number of tracers including spec. hum.
!
! 24Feb2013  Todling  Addapted for dyn-vect needs
! 05Sep2013  Takacs((RT) Change ple_out calculation not to lean on ak
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer  im,jm,lm,nq

! Input variables
! ---------------
      real      ple(im,jm,lm+1)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real      thv(im,jm,lm)
      real        q(im,jm,lm,nq)
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

      real, allocatable ::    u_out(:,:,:)
      real, allocatable ::    v_out(:,:,:)
      real, allocatable ::  thv_out(:,:,:)
      real, allocatable ::    q_in (:,:,:,:)
      real, allocatable ::    q_out(:,:,:,:)

      real    kappa,cp,rgas,eps,rvap
      integer i,j,L

      _UNUSED_DUMMY(ak)

      kappa = 2.0/7.0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      cp    = rgas/kappa

      allocate(  ps     (im,jm) )
      allocate(  phi    (im,jm,lm+1) )
      allocate(  pke    (im,jm,lm+1) )
      allocate(  ple_out(im,jm,lm+1) )
      allocate(  pke_out(im,jm,lm+1) )

      allocate(    u_out(im,jm,lm)   )
      allocate(    v_out(im,jm,lm)   )
      allocate(  thv_out(im,jm,lm)   )
      allocate(    q_in (im,jm,lm,nq))
      allocate(    q_out(im,jm,lm,nq))

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
!       ple_out(i,j,L) = ak(L) + bk(L)*ps(i,j)
        ple_out(i,j,L) = ple(i,j,L) + bk(L)*( ps(i,j)-ple(i,j,lm+1) )
      enddo
      enddo
      enddo
      pke_out(:,:,:) = ple_out(:,:,:)**kappa 

! Map original fv state onto new eta grid
! ---------------------------------------
      q_in(:,:,:,:) =  q(:,:,:,:)

      call gmap_( im,jm,nq, kappa, &
                  lm, pke    ,ple    ,u    ,v    ,thv    ,q_in , &
                  lm, pke_out,ple_out,u_out,v_out,thv_out,q_out)

      ple(:,:,:)   = ple_out(:,:,:)
        u(:,:,:)   =   u_out(:,:,:)
        v(:,:,:)   =   v_out(:,:,:)
      thv(:,:,:)   = thv_out(:,:,:)
        q(:,:,:,:) =   q_out(:,:,:,:)

      deallocate(  ps      )
      deallocate(  phi     )
      deallocate(  pke     )
      deallocate(  ple_out )
      deallocate(  pke_out )

      deallocate(    u_out )
      deallocate(    v_out )
      deallocate(  thv_out )
      deallocate(    q_in  )
      deallocate(    q_out )

      return
      end subroutine topo_remap1_

      subroutine dyn_real_eta_( w_f, dyntype, info )
      implicit none
      integer,intent(in) :: dyntype
      type(dyn_vect) w_f
      integer,optional::info

      character(len=*), parameter :: myname_ = myname//'*dyn_topo_remap'
      real,allocatable ::  pk(:,:,:)
      real,allocatable :: pke(:,:,:)
      real,allocatable :: ple_cur(:,:,:)
      real,allocatable :: ple_new(:,:,:)
      real,allocatable :: thv(:,:,:)
      real kappa
      integer k,im,jm,km

      kappa = 2.0/7.0
      im=w_f%grid%im
      jm=w_f%grid%jm
      km=w_f%grid%km

!     check to see if remap is needed
!     only remap when max diff in ps larger than 1% of max value of ps
!     if(maxval(abs(ps_new-w_f%ps))<0.01*maxval(w_f%ps)) then
!       print*, myname_, ': no remap necessary, returning ...'
!       if (present(info)) then
!           info = 1
!       endif
!       return
!     endif
      if (present(info)) then
         info = 0
      endif
      allocate(ple_cur(im,jm,km+1))
      allocate(ple_new(im,jm,km+1))
      allocate(    thv(im,jm,km))
      if(dyntype==5) then
         allocate ( pk(im,jm,km  ))
         allocate (pke(im,jm,km+1))
         ple_cur(:,:,1) = w_f%grid%ak(1)
         ple_new(:,:,1) = w_f%grid%ak(1)
         do k=1,km
            ple_cur(:,:,k+1) = ple_cur(:,:,k) + w_f%delp(:,:,k)
            ! overwrite delp in w_f and force it to be eta-consistent
            w_f%delp(:,:,k) = w_f%grid%ak(k+1)-w_f%grid%ak(k) + &
                             (w_f%grid%bk(k+1)-w_f%grid%bk(k))*w_f%ps
            ple_new(:,:,k+1) = ple_new(:,:,k) + w_f%delp(:,:,k)
         enddo
       ! convert virtual temperature to virtual potential temperature
         pke(:,:,:) = ple_cur(:,:,:)**kappa
         do k=1,km
            pk(:,:,k) = ( pke(:,:,k+1)-pke(:,:,k) ) &
                       / ( kappa*log(ple_cur(:,:,k+1)/ple_cur(:,:,k)) )
         enddo
         thv = w_f%pt/pk
         deallocate (  pk )
         deallocate ( pke )
      else 
          print *, ' not coded for old dyn'
          call exit(1)
          stop
      endif

      call ps_remap0_( ple_cur,ple_new,w_f%u,w_f%v,thv,w_f%q, &
                       w_f%grid%ak,w_f%grid%bk,&
                       im,jm,km,4)

      if(dyntype==5) then
         allocate (pke(im,jm,km+1))
         allocate ( pk(im,jm,km  ))
       ! convert virtual potential temperature to virtual temperature
         pke(:,:,:) = ple_new(:,:,:)**kappa
         do k=1,km
            pk(:,:,k) = ( pke(:,:,k+1)-pke(:,:,k) ) &
                       / ( kappa*log(ple_new(:,:,k+1)/ple_new(:,:,k)) )
         enddo
         w_f%pt = thv*pk
         deallocate (  pk )
         deallocate ( pke )
      endif

      deallocate(thv)
      deallocate(ple_new)
      deallocate(ple_cur)
      end subroutine dyn_real_eta_

      subroutine ps_remap0_( ple,ple_out,u,v,thv,q,ak,bk,im,jm,lm,nq )

!***********************************************************************
!
!  Purpose
!     Driver for remapping fields lcv fields to consistent eta-coordinate
!
!  Argument Description
!     ple ...... model edge pressure
!     u  ....... model zonal      wind
!     v  ....... model meridional wind
!     thv  ..... model virtual potential  temperature
!     q  ....... model specific   humidity; ozone; others
!     ak ....... model vertical   dimension
!     bk ....... model vertical   dimension
!     ple_out .. target pressure levels
!
!     im ....... zonal      dimension
!     jm ....... meridional dimension
!     lm ....... meridional dimension
!     nq ....... number of tracers including spec. hum.
!
! 20Oct2013  Todling  Addapted for dyn-vect needs
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer  im,jm,lm,nq

! Input variables
! ---------------
      real      ple(im,jm,lm+1)
      real  ple_out(im,jm,lm+1)
      real        u(im,jm,lm)
      real        v(im,jm,lm)
      real      thv(im,jm,lm)
      real        q(im,jm,lm,nq)

      real       ak(lm+1)
      real       bk(lm+1)

! Local variables
! ---------------
      real, allocatable ::  pke    (:,:,:)
      real, allocatable ::  pke_out(:,:,:)

      real, allocatable ::    u_out(:,:,:)
      real, allocatable ::    v_out(:,:,:)
      real, allocatable ::  thv_out(:,:,:)
      real, allocatable ::    q_in (:,:,:,:)
      real, allocatable ::    q_out(:,:,:,:)

      real    kappa,cp,rgas,eps,rvap

      _UNUSED_DUMMY(ak)
      _UNUSED_DUMMY(bk)

      kappa = 2.0/7.0
      rgas  = 8314.3/28.97
      rvap  = 8314.3/18.01
      eps   = rvap/rgas-1.0
      cp    = rgas/kappa

      allocate(  pke    (im,jm,lm+1) )
      allocate(  pke_out(im,jm,lm+1) )

      allocate(    u_out(im,jm,lm)   )
      allocate(    v_out(im,jm,lm)   )
      allocate(  thv_out(im,jm,lm)   )
      allocate(    q_in (im,jm,lm,nq))
      allocate(    q_out(im,jm,lm,nq))

! Construct fv pressure variables using new surface pressure
! ----------------------------------------------------------
      pke(:,:,:)     = ple    (:,:,:)**kappa 
      pke_out(:,:,:) = ple_out(:,:,:)**kappa 

! Map original fv state onto new eta grid
! ---------------------------------------
      q_in(:,:,:,:) =  q(:,:,:,:)

      call gmap_( im,jm,nq, kappa, &
                  lm, pke    ,ple    ,u    ,v    ,thv    ,q_in , &
                  lm, pke_out,ple_out,u_out,v_out,thv_out,q_out)

        u(:,:,:)   =   u_out(:,:,:)
        v(:,:,:)   =   v_out(:,:,:)
      thv(:,:,:)   = thv_out(:,:,:)
        q(:,:,:,:) =   q_out(:,:,:,:)

      deallocate(  pke_out )
      deallocate(  pke     )

      deallocate(    q_out )
      deallocate(    q_in  )
      deallocate(  thv_out )
      deallocate(    v_out )
      deallocate(    u_out )

      return
      end subroutine ps_remap0_

      subroutine gmap_(im, jm, nq,  akap, &
               km,  pk3d_m,  pe3d_m, u_m,  v_m,  pt_m,  q_m, &
               kn,  pk3d_n,  pe3d_n, u_n,  v_n,  pt_n,  q_n  )

      implicit none

      integer im, jm
      integer km, kn, nq

! Input:
! original data km-level

      real      u_m(im,jm,km)
      real      v_m(im,jm,km)
      real     pt_m(im,jm,km)
      real      q_m(im,jm,km,nq)
      real   pk3d_m(im,jm,km+1)
      real   pe3d_m(im,jm,km+1)

      real   pk3d_n(im,jm,kn+1)
      real   pe3d_n(im,jm,kn+1)

! Output:
! New data (kn-level)
      real      u_n(im,jm,kn)
      real      v_n(im,jm,kn)
      real     pt_n(im,jm,kn)
      real      q_n(im,jm,kn,nq)

! local (private)
      integer i, j, k, n

      real pe1(im,km+1) ,pe2(im,kn+1)
      real pk1(im,km+1) ,pk2(im,kn+1)
      real dp1(im,km)   ,dp2(im,kn)
      real  u1(im,km)   , u2(im,kn)
      real  v1(im,km)   , v2(im,kn)
      real  t1(im,km)   , t2(im,kn)
      real  q1(im,km,nq), q2(im,kn,nq)

      real akap
      real undef
      real big
      parameter ( undef = 1.e15 )
      parameter (   big = 1.e10 )


      _UNUSED_DUMMY(akap)


#if   (openmp)
!$omp  parallel do
!$omp& default (shared)
!$omp& private (i,j,k,pe1,pe2,pk1,pk2,dp1,dp2,u1,v1,t1,q1)
!$omp& private (u2,v2,t2,q2)
#endif
      do 2000 j=1,jm

! Copy original data to local 2D arrays.

      do k=1,km+1
      do i=1,im
      pe1(i,k) = pe3d_m(i,j,k)
      pk1(i,k) = pk3d_m(i,j,k)
      enddo
      enddo

      do k=1,kn+1
      do i=1,im
      pe2(i,k) = pe3d_n(i,j,k)
      pk2(i,k) = pk3d_n(i,j,k)
      enddo
      enddo

      do k=1,km
      do i=1,im
      dp1(i,k) =  pk1(i,k+1)-pk1(i,k)
       u1(i,k) =  u_m(i,j,k)
       v1(i,k) =  v_m(i,j,k)
       t1(i,k) = pt_m(i,j,k)
      enddo
      enddo
      do n=1,nq
      do k=1,km
      do i=1,im
       q1(i,k,n) =  q_m(i,j,k,n)
      enddo
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pk2(i,k+1)-pk2(i,k)
      enddo
      enddo

! map pt
! ------
      call mappm_( km, pk1, dp1, t1, kn, pk2, dp2, t2, im, 1, 7 )

      do k=1,km
      do i=1,im
      dp1(i,k) = pe1(i,k+1)-pe1(i,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pe2(i,k+1)-pe2(i,k)
      enddo
      enddo

! map u,v,q,oz
! ------------
      do n=1,nq
      call mappm_( km, pe1, dp1, q1(1,1,n), kn, pe2, dp2, q2(1,1,n), im,  0, 7 )
      enddo
      call mappm_( km, pe1, dp1, u1, kn, pe2, dp2, u2, im, -1, 7 )
      call mappm_( km, pe1, dp1, v1, kn, pe2, dp2, v2, im, -1, 7 )

      do k=1,kn
      do i=1,im
        u_n(i,j,k) = u2(i,k)
        v_n(i,j,k) = v2(i,k)
       pt_n(i,j,k) = t2(i,k)
      enddo
      enddo
      do n=1,nq
      do k=1,kn
      do i=1,im
        q_n(i,j,k,n) = q2(i,k,n)
      enddo
      enddo
      enddo

2000  continue

      return
      end subroutine gmap_


!****6***0*********0*********0*********0*********0*********0**********72
      subroutine mappm_(km, pe1, dp1, q1, kn, pe2, dp2, q2, im, iv, kord)
!****6***0*********0*********0*********0*********0*********0**********72
! IV = 0: constituents
! IV = 1: potential temp
! IV =-1: winds
!
! Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
!
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate

      integer, parameter :: kmax = 200
      real   , parameter :: R3 = 1./3., R23 = 2./3.

      integer km,kn,im,iv,kord
      real dp1(im,km),   dp2(im,kn), &
            q1(im,km),    q2(im,kn), &
           pe1(im,km+1), pe2(im,kn+1)

! local work arrays
      real a4(4,im,km)
      real delp,dpsum,esl,PR,PL,TT,qsum
      integer i,k,L,k0,k1

      _UNUSED_DUMMY(dp2)

      do k=1,km
         do i=1,im
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      call ppm2m_(a4, dp1, im, km, iv, kord)

! Lowest layer: constant distribution
      do i=1, im
         a4(2,i,km) = q1(i,km)
         a4(3,i,km) = q1(i,km)
         a4(4,i,km) = 0.
      enddo

      do 5555 i=1,im
         k0 = 1
      do 555 k=1,kn

         if(pe2(i,k+1) .le. pe1(i,1)) then
! Entire grid above old ptop
            q2(i,k) = a4(2,i,1)
         elseif(pe2(i,k) .ge. pe1(i,km+1)) then
! Entire grid below old ps
            q2(i,k) = a4(3,i,km)
         elseif(pe2(i,k  ) .lt. pe1(i,1) .and. &
                pe2(i,k+1) .gt. pe1(i,1))  then
! Part of the grid above ptop
            q2(i,k) = a4(1,i,1)
         else

         do 45 L=k0,km
! locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and. &
             pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

! entire new grid is within the original grid
               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = R3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L) &
                       - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else
! Fractional area...
              delp = pe1(i,L+1) - pe2(i,k)
              TT   = R3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+ &
                     a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
              dpsum = delp
              k1 = L + 1
             goto 111
             endif
         endif
45       continue

111      continue
         do 55 L=k1,km
         if( pe2(i,k+1) .gt. pe1(i,L+1) ) then

! Whole layer..

            qsum  =  qsum + dp1(i,L)*q1(i,L)
            dpsum = dpsum + dp1(i,L)
         else
           delp = pe2(i,k+1)-pe1(i,L)
           esl  = delp / dp1(i,L)
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl* &
                 (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
          dpsum = dpsum + delp
           k0 = L
           goto 123
         endif
55       continue
        delp = pe2(i,k+1) - pe1(i,km+1)
        if(delp .gt. 0.) then
! Extended below old ps
           qsum = qsum + delp * a4(3,i,km)
          dpsum = dpsum + delp
        endif
123     q2(i,k) = qsum / dpsum
      endif
555   continue
5555  continue

      return
      end subroutine mappm_

      subroutine ppm2m_(a4,delp,im,km,iv,kord)
! iv = 0: positive definite scalars
! iv = 1: others
! iv =-1: winds

      implicit none

      integer im, km, lmt, iv
      integer kord
      integer i, k, km1
      real a4(4,im,km), delp(im,km)

! local arrays.
      real dc(im,km),delq(im,km)
      real h2(im,km)
      real a1, a2, c1, c2, c3, d1, d2
      real qmax, qmin, cmax, cmin
      real qm, dq, tmp

! Local scalars:
      real qmp
      real lac

      km1 = km - 1

      do 500 k=2,km
      do 500 i=1,im
      delq(i,k-1) = a4(1,i,k) - a4(1,i,k-1)
500   a4(4,i,k  ) = delp(i,k-1) + delp(i,k)

      do 1220 k=2,km1
      do 1220 i=1,im
      c1 = (delp(i,k-1)+0.5*delp(i,k))/a4(4,i,k+1)
      c2 = (delp(i,k+1)+0.5*delp(i,k))/a4(4,i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) / &
                                    (a4(4,i,k)+delp(i,k+1))
      qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
      qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

!****6***0*********0*********0*********0*********0*********0**********72
! 4th order interpolation of the provisional cell edge value
!****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) * &
                ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) - &
                                delp(i,k-1)*a1*dc(i,k  ) )
12    continue

! Area preserving cubic with 2nd deriv. = 0 at the boundaries
! Top
      do i=1,im
      d1 = delp(i,1)
      d2 = delp(i,2)
      qm = (d2*a4(1,i,1)+d1*a4(1,i,2)) / (d1+d2)
      dq = 2.*(a4(1,i,2)-a4(1,i,1)) / (d1+d2)
      c1 = 4.*(a4(2,i,3)-qm-d2*dq) / ( d2*(2.*d2*d2+d1*(d2+3.*d1)) )
      c3 = dq - 0.5*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,2) = qm - 0.25*c1*d1*d2*(d2+3.*d1)
      a4(2,i,1) = d1*(2.*c1*d1**2-c3) + a4(2,i,2)
      dc(i,1) =  a4(1,i,1) - a4(2,i,1)
! No over- and undershoot condition
      cmax = max(a4(1,i,1), a4(1,i,2))
      cmin = min(a4(1,i,1), a4(1,i,2))
      a4(2,i,2) = max(cmin,a4(2,i,2))
      a4(2,i,2) = min(cmax,a4(2,i,2))
      enddo

      if(iv == 0) then
         do i=1,im
            a4(2,i,1) = max(0.,a4(2,i,1))
            a4(2,i,2) = max(0.,a4(2,i,2))
         enddo
      elseif(iv == -1) then
         do i=1,im
            if( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
         enddo
      endif

!****6***0*********0*********0*********0*********0*********0**********72

! Bottom
! Area preserving cubic with 2nd deriv. = 0 at the surface
      do 15 i=1,im
      d1 = delp(i,km)
      d2 = delp(i,km1)
      qm = (d2*a4(1,i,km)+d1*a4(1,i,km1)) / (d1+d2)
      dq = 2.*(a4(1,i,km1)-a4(1,i,km)) / (d1+d2)
      c1 = (a4(2,i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
      c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,km) = qm - c1*d1*d2*(d2+3.*d1)
      a4(3,i,km) = d1*(8.*c1*d1**2-c3) + a4(2,i,km)
      dc(i,km) = a4(3,i,km) -  a4(1,i,km)
!****6***0*********0*********0*********0*********0*********0**********72
! No over- and undershoot condition
      cmax = max(a4(1,i,km), a4(1,i,km1))
      cmin = min(a4(1,i,km), a4(1,i,km1))
      a4(2,i,km) = max(cmin,a4(2,i,km))
      a4(2,i,km) = min(cmax,a4(2,i,km))
!****6***0*********0*********0*********0*********0*********0**********72
15    continue

      if(iv .eq. 0) then
      do i=1,im
         a4(2,i,km) = max(0.,a4(2,i,km))
         a4(3,i,km) = max(0.,a4(3,i,km))
      enddo
      endif

      do 20 k=1,km1
      do 20 i=1,im
      a4(3,i,k) = a4(2,i,k+1)
20    continue
!
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
!

! Top 2 and bottom 2 layers always use monotonic mapping

      do k=1,2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm_(dc(1,k),a4(1,1,k),im, 0)
      enddo

      if(kord == 7) then
!****6***0*********0*********0*********0*********0*********0**********72
! Huynh's 2nd constraint
!****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=1,im
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 4000 k=3, km-2
      do 3000 i=1, im
! Right edges
         qmp   = a4(1,i,k)                 + 2.0*delq(i,k-1)
         lac   = a4(1,i,k) + 1.5*h2(i,k-1) + 0.5*delq(i,k-1)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(3,i,k) = min(max(a4(3,i,k), qmin), qmax)
! Left  edges
         qmp   = a4(1,i,k)                 - 2.0*delq(i,k)
         lac   = a4(1,i,k) + 1.5*h2(i,k+1) - 0.5*delq(i,k)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(2,i,k) = min(max(a4(2,i,k), qmin), qmax)
! Recompute A6
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
3000  continue
! Additional constraint to prevent negatives
         if (iv == 0) then
             call kmppm_(dc(1,k),a4(1,1,k),im, 2)
         endif
4000  continue

      else

         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv .eq. 0) lmt = min(2, lmt)

      do k=3, km-2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm_(dc(1,k),a4(1,1,k),im, lmt)
      enddo
      endif

      do 5000 k=km1,km
         do i=1,im
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm_(dc(1,k),a4(1,1,k),im, 0)
5000  continue

      return
      end subroutine ppm2m_

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine kmppm_(dm, a4, km, lmt)
!****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      real r12
      parameter (r12 = 1./12.)

      integer km, lmt
      integer i
      real a4(4,km),dm(km)
      real da1, da2, a6da
      real fmin
      real qmp

      if (lmt .eq. 3) return
! Full constraint

      if(lmt .eq. 0) then
      do 100 i=1,km
      if(dm(i) .eq. 0.) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da .lt. -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da .gt. da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
100   continue
      elseif (lmt .eq. 2) then
! Positive definite

! Positive definite constraint
      do 250 i=1,km
      if(abs(a4(3,i)-a4(2,i)) .ge. -a4(4,i)) go to 250
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
      if(fmin.ge.0.) go to 250
      if(a4(1,i).lt.a4(3,i) .and. a4(1,i).lt.a4(2,i)) then
            a4(3,i) = a4(1,i)
            a4(2,i) = a4(1,i)
            a4(4,i) = 0.
      elseif(a4(3,i) .gt. a4(2,i)) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
      else
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
      endif
250   continue

      elseif (lmt == 1) then

! Improved full monotonicity constraint (Lin)
! Note: no need to provide first guess of A6 <-- a4(4,i)

      do i=1, km
           qmp = 2.*dm(i)
         a4(2,i) = a4(1,i)-sign(min(abs(qmp),abs(a4(2,i)-a4(1,i))), qmp)
         a4(3,i) = a4(1,i)+sign(min(abs(qmp),abs(a4(3,i)-a4(1,i))), qmp)
         a4(4,i) = 3.*( 2.*a4(1,i) - (a4(2,i)+a4(3,i)) )
      enddo
      endif

      return
      end subroutine kmppm_

end module m_topo_remap
