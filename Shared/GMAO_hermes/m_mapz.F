!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !MODULE: m_mapz --- Performs vertical interpolation of rst-like fields
!                     
!
! !INTERFACE:
!
      module m_mapz

!USES:

      use m_dyn
      use m_set_eta, only: set_eta

      implicit NONE
 
! !PUBLIC MEMBER FUNCTIONS:
 
      PRIVATE
      PUBLIC set_eta      ! sets definition of eta coordinates
      PUBLIC z_map        ! horizontal interpolation of fields in d_rst
      PUBLIC drymadj      ! mass adjustment
      PUBLIC z_mapping    !
      PUBLIC map1_ppm     !

! !PUBLIC MEMBER FUNCTIONS:

      interface z_map
         module procedure z_map_dyn_
      end interface

      interface z_mapping
         module procedure z_mapping0_
         module procedure z_mapping1_
      end interface

      interface get_org_data
         module procedure get_org_data0_
         module procedure get_org_data1_
      end interface

      interface drymadj
         module procedure drymadj_
      end interface

      interface map1_ppm
         module procedure map1_ppm_
      end interface


! !DESCRIPTION: This module is a wrapper to the original mapz program
!               of S. J. Lin. This is to facilitate interpolating
!               fields in the dyn vector as well as simply those
!               in typical fv-rst files.
!
! !REMARKS:
!
!   The original main program written by S.J. has been left here
!   untouched and is "ifdef-ed" out - as it makes no sense in a 
!   module (see bottom of this file).
!
! !SEE ALSO: dyn2dyn
!
! !REVISION HISTORY:
!
!  23Sep2004  Todling    Initial (wrapper) code.
!  22Dec2004  Todling    Made z_mapping public to allow mapz.x to be built
!  12Jan2005  Todling    Added ak,bk's for GEOS-5 72 level state.
!  29dec2005  da Silva   Added option to force remapping even when ptop
!                        and nlevs are the same. This is useful when
!                        converting from lcv to eta coordinates. 
!  17Jan2006  Ravi       Made routine map1_ppm as PUBLIC, added an interface
!                        map1_ppm_ and  changed all the calls and the routine
!                        map1_ppm_.
!
!EOP
!-------------------------------------------------------------------------
 
      CONTAINS

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  z_map_dyn_ --- Vertical interpolation of dyn(eta) vectors
!
! !INTERFACE:
!
      subroutine z_map_dyn_ ( w_i, w_o, rc,
     .                        verbose, force )  ! optionals
!
! !USES:
!
      implicit NONE
!
! !INPUT PARAMETERS:
!
      type(dyn_vect), intent(in)    :: w_i    ! original state vector

      logical, optional, intent(in) :: verbose! when .t., echoes summary information
      logical, optional, intent(in) :: force  ! force remapping even when 
                                              ! vertical layers have not changed
!
! !OUTPUT PARAMETERS:
!
      type(dyn_vect), intent(inout)   :: w_o    ! interpolated state vector
      integer, intent(out)          :: rc     ! error return code:
                                              ! -1 - no need to interpolate
                                              !  0 - all is well
                                              !  1 - inconsistent horizontal resolution
                                              !  2 - tracers on output not init right
!
! !DESCRIPTION: Interpolates state vectors between different horizontal
!               resolutions; based on S. J. Lin's original maph code.
!
! !REVISION HISTORY:
!
!  23Sep2005 Todling   Initial code.
!
!EOP
!----------------------------------------------------------------------------

      integer im, jm, km, lm
      integer in, jn, kn, ln
      integer ksn
      integer ier
      real    ptopm      ! original ptop
      real    ptopn, pintn

! Declare pointers
! ----------------
                                           ! input fields
      real, pointer ::  lwim(:,:)          !   land-water-ice mask
      real, pointer :: phism(:,:)          !   surface geopotential
      real, pointer ::   hsm(:,:)          !   topography height stdv
      real, pointer ::   tsm(:,:)          !   sea surface temperature
      real, pointer ::   psm(:,:)          !   surface pressure    
      real, pointer ::    um(:,:,:)        !   zonal wind on D-grid
      real, pointer ::    vm(:,:,:)        !   meridional wind
      real, pointer ::   ptm(:,:,:)        !   scaled potential temperature
      real, pointer ::    qm(:,:,:,:)      !   specific humidity & mixing ratios
      real, pointer :: delpm(:,:,:)        !   pressure thickness

                                           ! output fields
      real, pointer ::   akn(:)            !   ak of eta vertical grid
      real, pointer ::   bkn(:)            !   bk of eta vertical grid
      real, pointer ::   psn(:,:)          !   surface pressure    
      real, pointer ::    un(:,:,:)        !   zonal wind on D-grid
      real, pointer ::    vn(:,:,:)        !   meridional wind
      real, pointer ::   ptn(:,:,:)        !   scaled potential temperature
      real, pointer ::    qn(:,:,:,:)      !   specific humidity & mixing ratios
      real, pointer :: delpn(:,:,:)        !   pressure thickness

      logical :: verb, force_it

      if ( present(verbose) ) then
           verb = verbose
      else
           verb = .false.
      end if      
      if ( present(force) ) then
           force_it = force
      else
           force_it = .false.
      end if      


      im = w_i%grid%im
      jm = w_i%grid%jm
      km = w_i%grid%km
      lm = w_i%grid%lm

      ptopm = w_i%grid%ptop

      in = w_o%grid%im
      jn = w_o%grid%jm
      kn = w_o%grid%km   ! Set target vertical resolution
      ln = w_o%grid%lm   ! Set target vertical resolution

! Make sure horizontal resolution is consistent
! ---------------------------------------------
      if ( (in/=im) .or. (jn/=jm) ) then
           rc = 1
           return
       endif

! Check for properly initialized number of tracers in output
! ----------------------------------------------------------
      if ( ln/=lm ) then
           rc = 2
           return
       endif

! Fill in pointers in
! -------------------
       lwim => w_i%lwi
      phism => w_i%phis
        hsm => w_i%hs_stdv
        tsm => w_i%ts
        
        psm => w_i%ps
         um => w_i%u
         vm => w_i%v
        ptm => w_i%pt
         qm => w_i%q
      delpm => w_i%delp

! Fill in pointers out
! --------------------
       w_o%lwi     =  w_i%lwi
       w_o%phis    =  w_i%phis
       w_o%hs_stdv =  w_i%hs_stdv
       w_o%ts      =  w_i%ts

       w_o%ps      =  w_i%ps
        psn        => w_o%ps  ! ps could be changed in drymadj (though this feature not on)

        akn => w_o%grid%ak
        bkn => w_o%grid%bk
         un => w_o%u
         vn => w_o%v
        ptn => w_o%pt
         qn => w_o%q
      delpn => w_o%delp

      rc = 0

!**********************
! Perform interpolation
!**********************
      call z_mapping (im, jm, km, lm, ptopm, kn,
     .                    um, vm, ptm, qm, delpm,
     .                psn,un, vn, ptn, qn, delpn,
     .               ptopn, pintn, ksn, akn, bkn, ier,
     .               verbose=verbose, force=force_it )
        if(ier<0) then
            rc = -1  
            return
        endif

!**********************************
! Now set remaining part dyn vector
!**********************************
      w_o%grid%ptop = ptopn
      w_o%grid%pint = pintn
      w_o%grid%ks   = ksn

      end subroutine z_map_dyn_ 

      subroutine z_mapping1_(im, jm, km, nc, ptop_old, nl,
     .                           um, vm, ptm, qm, delpm,
     .                           ps,u,v,pt,q,delp,
     .                     ptop, pint, ks, ak, bk, rc,
     .                                      verbose, force )  ! optionals

      implicit none
      integer im, jm, km, nc, nl

! Original data with km layers
! vertical resolution of the target is nl

! Original
      real     um(im,jm,km)        !zonal wind on D-grid
      real     vm(im,jm,km)        !meridional wind
      real    ptm(im,jm,km)        !scaled potential temperature
      real     qm(im,jm,km,nc)     !specific humidity & mixing ratios
      real  delpm(im,jm,km)        ! pressure thickness
      real ptop_old

      real    pem(im,km+1,jm)      !pressure at layer edges
      real    pkm(im,jm,km+1)

      logical, optional :: verbose ! echoes summaries or not
      logical, optional :: force   ! echoes summaries or not

! New data with nl layers
      real    ps(im, jm)     
      real     u(im,jm,nl)        !zonal wind on D-grid
      real     v(im,jm,nl)        !meridional wind
      real    pt(im,jm,nl)        !scaled potential temperature
      real     q(im,jm,nl,nc)     !specific humidity & mixing ratios
      real  delp(im,jm,nl)        ! pressure thickness

      real    pe(im,nl+1,jm)      !pressure at layer edges
      real    pk(im,jm,nl+1)

      real    ptop, pint
      integer ks
      real    ak(nl+1), bk(nl+1)

      integer, intent(out) :: rc

      integer i,j,k
      real  rair, rh2o, cpair, akap, zvir
      real  pmax, pmin
      logical verb, force_it

      verb  = .false.
      if (present(verbose)) then
         if(verbose) verb = .true.
      endif

      if ( present(force) ) then
           force_it = force
      else
           force_it = .false.
      end if      

! Constants:
      rair   = 287.04
      rh2o   = 4.61e2
      cpair  = 1004.64
      akap  = rair/cpair
      zvir   = rh2o/rair - 1.

      rc = 0
      if(verb) write(6,*) 'zvir=',zvir

c Set up the NEW reference vertical coordinate system

      call set_eta(nl, ks, ptop, pint, ak, bk)

      if(verb) write(6,*) 'new PTOP=', ptop, pint, ks

!     Unless we want to force it, do not remap if ptop and levels are
!      teh same. This is useful for lcv-eta conversions
!     ---------------------------------------------------------------
      if ( .not. force_it ) then
         if( km .eq. nl  .and. abs(ptop-ptop_old) .le. 1.e-2) then
            rc = -1             ! no need to interpolate vertically
            return
         end if
      end if

! get original data with km layers
	call get_org_data(delpm,pem,im,jm,km,ptop_old)

	do k=1,km+1
	do j=1,jm
	do i=1,im
           pkm(i,j,k) = pem(i,k,j) ** akap
	enddo
	enddo
	enddo

      if (verb) then
      pmax = vmax(pem,pmin,im*jm*(km+1))
      write(6,*) 'max PEm =', pmax, ' min PEm=', pmin

      pmax = vmax(delpm,pmin,im*jm*km)
      write(6,*) 'max delp =', pmax, ' min delp=', pmin
      endif

C Mapping.
      call gmap(im, jm, nc,    akap,
     &          km,  pkm,  pem,         um,  vm,  ptm,  qm,
     &          nl,  pk ,  pe , delp ,  u ,  v ,  pt ,  q)

      if (verb) then
      write(6,*) ' '
      write(6,*) 'Done mapping'
      write(6,*) 'PTOP=', pe(1,1,1)
      endif

! Fix Dry air mass
!     call drymadj_( im, jm, nl, 1, jm,
!    &              .true., ak, bk, ps, delp, nc, q, 1 )

      if (verb) then
      pmax = vmax(ps,pmin,im*jm)
      write(6,*) 'max ps =', pmax, ' min =', pmin

      pmax = vmax(delp,pmin,im*jm*nl)
      write(6,*) 'max delp =', pmax, ' min delp=', pmin

      pmax = vmax(pe,pmin,im*jm*(nl+1))
      write(6,*) 'max PE =', pmax, ' min PE=', pmin

      pmax = vmax(u,pmin,im*jm*nl)
      write(6,*) 'max u =', pmax, ' min u=', pmin

      pmax = vmax(v,pmin,im*jm*nl)
      write(6,*) 'max v =', pmax, ' min v=', pmin

      pmax = vmax(PT,pmin,im*jm*nl)
      write(6,*) 'max PT =', pmax, ' min PT=', pmin

      pmax = vmax(q,pmin,im*jm*nl)
      write(6,*) 'max q =', pmax, ' min q=', pmin
      endif  ! < verbose >


      end subroutine z_mapping1_

      subroutine z_mapping0_(iuic, iout, im, jm, km, nc, ptop_old, nl) 

      implicit none
      integer iuic, iout
      integer im, jm, km, nc, nl

! Original data with km layers
! vertical resolution of the target is nl

! Original
      real     um(im,jm,km)        !zonal wind on D-grid
      real     vm(im,jm,km)        !meridional wind
      real    ptm(im,jm,km)        !scaled potential temperature
      real     qm(im,jm,km,nc)     !specific humidity & mixing ratios
      real    pem(im,km+1,jm)      !pressure at layer edges
      real  delpm(im,jm,km)        ! pressure thickness
      real    pkm(im,jm,km+1)
      real ptop_old

! New data with nl layers
      real    ps(im, jm)     
      real     u(im,jm,nl)        !zonal wind on D-grid
      real     v(im,jm,nl)        !meridional wind
      real    pt(im,jm,nl)        !scaled potential temperature
      real     q(im,jm,nl,nc)     !specific humidity & mixing ratios
      real    pe(im,nl+1,jm)      !pressure at layer edges
      real  delp(im,jm,nl)        ! pressure thickness
      real    pk(im,jm,nl+1)
      real ak(nl+1), bk(nl+1)


      integer nymd, nhms, nstep
      integer i,j,k,ks
      real    rair, rh2o, cpair, akap, zvir
      real    pmax, pmin, ptop, pint

      
! Constants:
      rair   = 287.04
      rh2o   = 4.61e2
      cpair  = 1004.64
      akap  = rair/cpair
      zvir   = rh2o/rair - 1.

      write(6,*) 'zvir=',zvir

c Set up the NEW reference vertical coordinate system

      call set_eta(nl, ks, ptop, pint, ak, bk)

      write(6,*) 'PTOP=', ptop, pint, ks

C get original data with km layers
	call get_org_data(ps, delpm,um,vm,ptm,qm,pem,im,jm,km,nc,
     &                    nymd, nhms, iuic, nstep, ptop_old)

      write(6,*) ' '
      write(6,*) 'NYMD=',nymd,' NHMS=',nhms
      write(6,*) 'nstep=', nstep

      if( km .eq. nl  .and. abs(ptop-ptop_old) .le. 1.e-2) then

      write(6,*) 'No mapping will be performed'
      write(6,*) 'New date/time information?'
      write(6,*) 'NYMD=?'
      read(*,*) nymd
      write(6,*) 'NHMS=?'
      read(*,*) nhms
      write(6,*) 'nstep='
      read(*,*)  nstep

! write original dyn rest file.
      call rst_dyn(im, jm, km, -1, iout, delpm, um, vm, ptm, nc,
     &             qm   , ps ,  nymd,  nhms, nstep )
      else
 
	do k=1,km+1
	do j=1,jm
	do i=1,im
           pkm(i,j,k) = pem(i,k,j) ** akap
	enddo
	enddo
	enddo

      pmax = vmax(pem,pmin,im*jm*(km+1))
      write(6,*) 'max PEm =', pmax, ' min PEm=', pmin

      pmax = vmax(delpm,pmin,im*jm*km)
      write(6,*) 'max delp =', pmax, ' min delp=', pmin

C Mapping.
      call gmap(im, jm, nc,    akap,
     &          km,  pkm,  pem,         um,  vm,  ptm,  qm,
     &          nl,  pk ,  pe , delp ,  u ,  v ,  pt ,  q)

      write(6,*) ' '
      write(6,*) 'Done mapping'
      write(6,*) 'PTOP=', pe(1,1,1)

C Fix Dry air mass
!     call drymadj_( im, jm, nl, 1, jm,
!    &              .true., ak, bk, ps, delp, nc, q, 1 )

C write new dyn rest file.
      call rst_dyn(im, jm, nl, -1 , iout , delp, u, v, pt, nc,
     &             q   , ps ,  nymd,  nhms, nstep )

      pmax = vmax(ps,pmin,im*jm)
      write(6,*) 'max ps =', pmax, ' min =', pmin

      pmax = vmax(delp,pmin,im*jm*nl)
      write(6,*) 'max delp =', pmax, ' min delp=', pmin

      pmax = vmax(pe,pmin,im*jm*(nl+1))
      write(6,*) 'max PE =', pmax, ' min PE=', pmin

      pmax = vmax(u,pmin,im*jm*nl)
      write(6,*) 'max u =', pmax, ' min u=', pmin

      pmax = vmax(v,pmin,im*jm*nl)
      write(6,*) 'max v =', pmax, ' min v=', pmin

      pmax = vmax(PT,pmin,im*jm*nl)
      write(6,*) 'max PT =', pmax, ' min PT=', pmin

      pmax = vmax(q,pmin,im*jm*nl)
      write(6,*) 'max q =', pmax, ' min q=', pmin

      endif

      end subroutine z_mapping0_

      subroutine get_org_data0_(ps,delp,u,v,pt,q,pe,im,jm,km,nq,
     &                          nymd, nhms, iuic, nstep, ptop_old)

      integer  im, jm, km
      integer  iuic, nq

      real    ps(im,jm)     
      real     u(im,jm,km)        !zonal wind on D-grid
      real     v(im,jm,km)        !meridional wind
      real    pt(im,jm,km)        !scaled potential temperature
      real     q(im,jm,km,nq)     !specific humidity & mixing ratios
      real    pe(im,km+1,jm)      !pressure at layer edges
      real  delp(im,jm,km)        ! pressure thickness
      real ptop_old

      integer i, j, k
      integer nymd,  nhms, nstep

      call rst_dyn(im, jm, km, 1 , iuic , delp, u, v, pt, nq,
     &             q   , ps ,  nymd,  nhms, nstep )

      write(6,*) 'NYMD=',nymd,' NHMS=',nhms, nstep

	do j=1,jm
           do i=1,im
              pe(i,1,j) = ptop_old
           enddo
	enddo

	do k=1,km
	do j=1,jm
	do i=1,im
           pe(i,k+1,j) = pe(i,k,j) + delp(i,j,k)
	enddo
	enddo
        enddo

	return
	end subroutine get_org_data0_

      subroutine get_org_data1_(delp,pe,im,jm,km,ptop_old)

      implicit none
      integer  im, jm, km

      real    pe(im,km+1,jm)      !pressure at layer edges
      real  delp(im,jm,km)        ! pressure thickness
      real ptop_old

      integer i, j, k

        do j=1,jm
           do i=1,im
              pe(i,1,j) = ptop_old
           enddo
        enddo

        do k=1,km
        do j=1,jm
        do i=1,im
         pe(i,k+1,j) = pe(i,k,j) + delp(i,j,k)
        enddo
        enddo
        enddo

        return
      end subroutine get_org_data1_


c****6***0*********0*********0*********0*********0*********0**********72
      subroutine rst_dyn(im, jm, km, itp , iuic , delp, u, v, pt, nq,
     &                   q   , ps ,  nymd,  nhms, nstep )
c****6***0*********0*********0*********0*********0*********0**********72

      implicit none
      integer  im, jm
      integer  km

      integer nymd, nhms, nstep
      integer nq, iuic
      integer itp
      integer i, j, k, l

      real    u(im,jm,km),    v(im,jm,km),  pt(im,jm,km),
     &        delp(im,jm,km),
     &        q(im,jm,km,nq), ps(im,jm)

      real pmax, pmin

      if(itp.ge.0) then

      read(iuic) nstep, nymd, nhms
      read(iuic) ps, delp, u, v, pt

      if(itp .eq. 0) then
         nstep = 0
         write(6,*) 'nstep reset to zero in rst_dyn()'
      endif

      pmax = vmax(u,pmin,im*jm*km)
      write(6,*) 'max u =', pmax, ' min u=', pmin

      pmax = vmax(v,pmin,im*jm*km)
      write(6,*) 'max v =', pmax, ' min v=', pmin

      pmax = vmax(pt,pmin,im*jm*km)
      write(6,*) 'max PT =', pmax, ' min PT=', pmin

      if(nq .ne. 0) then
!_RT     read(iuic) q
         do l = 1, nq
            read(iuic) (((q(i,j,k,l),i=1,im),j=1,jm),k=1,km)
            pmax = vmax(q(1,1,1,l),pmin,im*jm*km)
            write(6,*) 'max q =', pmax, ' min q=', pmin
         enddo
      endif

      write(6,*) 'done reading ic for the dycore',nymd, nhms, nstep

	if(nstep .eq. 0) then
	do k=1,km

 	do j=1,jm
 	do i=1,im
 	q(i,j,k,1) = max(1.2e-12, q(i,j,k,1))
 	enddo
 	enddo

c     	call polavg(delp(1,1,k), im, jm)
c     	call polavg(pt(1,1,k), im, jm)
c     	call polavg(q(1,1,k,1), im, jm)

	enddo
	endif

      else

c Write

      rewind iuic
      write(iuic) nstep, nymd, nhms
      write(iuic) ps, delp,u,v,pt
      if(nq .ne. 0) write(iuic) q

      endif

      return
      end subroutine rst_dyn


c****6***0*********0*********0*********0*********0*********0**********72
      subroutine gmap(im, jm, nq,  akap,
     &         km,  pk3d_m,  pe3d_m,          u_m,  v_m,  pt_m,  q_m,
     &         kn,  pk3d_n,  pe3d_n, delp_n,  u_n,  v_n,  pt_n,  q_n  )
c****6***0*********0*********0*********0*********0*********0**********72

      implicit none
      
      integer im, jm
      integer km, kn, nq

C Input:
C original data km-level

      real      u_m(im,jm,km)
      real      v_m(im,jm,km)
      real     pt_m(im,jm,km)
      real      q_m(im,jm,km,*)
      real   pk3d_m(im,jm,km+1)
      real   pe3d_m(im,km+1,jm)


C Output:
C New data (kn-level)
      real   delp_n(im,jm,kn)
      real      u_n(im,jm,kn)
      real      v_n(im,jm,kn)
      real     pt_n(im,jm,kn)
      real      q_n(im,jm,kn,*)
      real   pk3d_n(im,jm,kn+1)
      real   pe3d_n(im,kn+1,jm)

      real ak(kn+1)
      real bk(kn+1)

c local (private)
      integer i, j, k, ic

      real pe0(im,km+1),pe1(im,km+1),pe2(im,kn+1),pe3(im,kn+1)
      real pk1(im,km+1),pk2(im,kn+1)
      real pkz(im,kn)

      real ptop,  pint
      integer ks
      real akap
      real ple,  pek, dak, bkh
      real undef
      real big
      parameter ( undef = 1.e15 )
      parameter (   big = 1.e10 )


      do 2000 j=1,jm
! new
         
         call set_eta(kn, ks, ptop, pint, ak, bk)
c Copy original data to local 2D arrays.

      do k=1,km+1
      do i=1,im
      pe1(i,k) = pe3d_m(i,k,j)
      pk1(i,k) = pk3d_m(i,j,k)
      enddo
      enddo

	pek = ptop**akap
      do i=1,im
         pe0(i,   1) = pe1(i,1)
         pe3(i,   1) = ptop

         pe2(i,   1) = ptop
         pe2(i,kn+1) = pe1(i,km+1)

         pk2(i,   1) = pek
         pk2(i,kn+1) = pk1(i,km+1)
      enddo

      if(ks .ne. 0) then
      do 800 k=2,ks+1
         ple = ak(k)
         pek = ple ** akap
      do 800 i=1,im
      pe2(i,k) = ple
      pe3(i,k) = ple
800   pk2(i,k) = pek
      endif

      do 900 k=ks+2,kn
      ple = ak(k)
      pek = bk(k)
         do i=1,im
            pe2(i,k) = ple + pek*pe1(i,km+1)
            pk2(i,k) = pe2(i,k) ** akap
         enddo
900   continue

c map pt

      call map1_ppm_( km,   pk1,   pt_m,
     &               kn,   pk2,   pt_n,
     &               im, 1, im, j, 1, jm, 1, 3, undef)

! Fix out of bound (beyond original ptop)
      do k=1, ks+1
         do i=1,im
            pkz(i,k) = 0.5*(pk2(i,k) + pk2(i,k+1))
         enddo
      enddo

      do k=ks,1,-1
         do i=1,im
            if( pt_n(i,j,k) .gt. big ) then
! Isothermal extension
                pt_n(i,j,k) = pt_n(i,j,k+1)*pkz(i,k+1)/pkz(i,k)
            endif
         enddo
      enddo

      if(ks .ne. 0) then
      do k=1,ks
         dak = ak(k+1) - ak(k)
         do i=1,im
           delp_n(i,j,k) = dak
         enddo
      enddo
      endif

      do k=ks+1,kn
         do i=1,im
           delp_n(i,j,k) = pe2(i,k+1) - pe2(i,k)
         enddo
      enddo

c Map constituents
      if(nq .ne. 0) then

      do ic=1,nq
      call map1_ppm_( km,   pe1,   q_m(1,1,1,ic),
     &               kn,   pe2,   q_n(1,1,1,ic),
     &               im, 1, im, j, 1, jm, 0, 3, undef)

! Fix out of bound (beyond original ptop)
      do k=ks,1,-1
         do i=1,im
            if( q_n(i,j,k,ic) .gt. big ) then
                q_n(i,j,k,ic) = q_n(i,j,k+1,ic)
            endif
         enddo
      enddo

      enddo
      endif

c map U
      if(j .ne. 1) then

      do k=2,km+1
         do i=1,im
            pe0(i,k) = 0.5*(pe1(i,k)+pe3d_m(i,k,j-1))
         enddo
      enddo

      do k=ks+2,kn+1
      bkh = 0.5*bk(k)
         do i=1,im
            pe3(i,k) = ak(k) + bkh*(pe1(i,km+1)+pe3d_m(i,km+1,j-1))
         enddo
      enddo

      call map1_ppm_( km,   pe0,   u_m,
     &               kn,   pe3,   u_n,
     &               im, 1, im, j, 1, jm, 1, 3, undef)
! Fix out of bound (beyond original ptop)
      do k=ks,1,-1
         do i=1,im
            if( u_n(i,j,k) .gt. big ) then
                u_n(i,j,k) = u_n(i,j,k+1)
            endif
         enddo
      enddo

      endif

c map v
      if(j .ne. 1 .and. j .ne. jm) then

      do k=2,km+1
      do i=2,im
      pe0(i,k) = 0.5*(pe1(i,k)+pe1(i-1,k))
      enddo
      enddo

c      i=1
      do k=2,km+1
      pe0(1,k) = 0.5*(pe1(1,k)+pe1(im,k))
      enddo

      do k=ks+2,kn+1
         do i=2,im
            pe3(i,k) = 0.5*(pe2(i,k)+pe2(i-1,k))
         enddo
      enddo
c i=1
      do k=ks+2,kn+1
         pe3(1,k) = 0.5*(pe2(1,k)+pe2(im,k))
      enddo

      call map1_ppm_( km,   pe0,   v_m,
     &               kn,   pe3,   v_n,
     &               im, 1, im, j, 1, jm, 1, 3, undef)
! Fix out of bound (beyond original ptop)
      do k=ks,1,-1
         do i=1,im
            if( v_n(i,j,k) .gt. big ) then
                v_n(i,j,k) = v_n(i,j,k+1)
            endif
         enddo
      enddo
      endif

c Update PE and PKHT
      do k=1,kn+1
         do i=1,im
            pe3d_n(i,k,j) = pe2(i,k)
            pk3d_n(i,j,k) = pk2(i,k)
         enddo
      enddo

2000  continue

      return
      end subroutine gmap

      function vmax(a,pmin,n)
      implicit none
      real vmax
      integer n, i
      real pmin, pmax
      real a(*)
      pmax =a(n)
      pmin =a(n)
      do 10 i=1,n-1
      pmax = max(pmax,a(i))
      pmin = min(pmin,a(i))
10    continue
      vmax = pmax
      return
      end function vmax

!-----------------------------------------------------------------------
!BOP
! !ROUTINE:  gmean --- Calculate the mean of a 2D field
!
! !INTERFACE:
!****6***0*********0*********0*********0*********0*********0**********72
      function gmean(im,jm,jfirst,jlast,q)
!****6***0*********0*********0*********0*********0*********0**********72
! !USES:
      implicit none

      real gmean

! !INPUT PARAMETERS:
      integer im, jm                       ! Horizontal dimensions
      integer jfirst, jlast                ! Latitude strip
      real q(im,jfirst:jlast)              ! 2D field 
! !DESCRIPTION:
!     Calculate the mean of a 2D field
!
! !REVISION HISTORY:
!   WS  99.11.02:  Documentation; cleaning; jfirst:jlast
!
!EOP
!---------------------------------------------------------------------
!BOC

      integer i, j, j2, jmm1
      real dp, acap, area, gsum, xsum
      real, allocatable, save :: sine(:),cosp(:),sinp(:),cose(:)
      real dl
      logical first
      data first /.true./

      save acap,area
 
      j2   = max( 2, jfirst )
      jmm1 = min( jm-1, jlast )
      if(first) then
        allocate( sine(jm),cosp(jm),sinp(jm),cose(jm) )
        call setrig(im,jm,dp,dl,cosp,cose,sinp,sine)

c scaled polar cap area.
        acap = im*(1.+sine(2)) / dp

        area = 2.*acap
        do j=2,jm-1
          area = area + cosp(j)*im
        enddo
 
        first = .false.
      endif

      gsum = 0.0
      if ( jfirst .eq. 1  ) gsum = gsum + q(1,1)*acap
      if ( jlast  .eq. jm ) gsum = gsum + q(1,jm)*acap

      do j=j2,jmm1
        xsum = 0.
        do i=1,im
          xsum = xsum + q(i,j)
        enddo
        gsum = gsum + xsum*cosp(j)
      enddo
      gmean = gsum/area

      return
!EOC
      end function gmean
!---------------------------------------------------------------------


c****6***0*********0*********0*********0*********0*********0**********72
      subroutine setrig(im,jm,dp,dl,cosp,cose,sinp,sine)
c****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      integer im, jm
      integer j, jm1
      real sine(jm),cosp(jm),sinp(jm),cose(jm)
      real dp, dl
      double precision pi, ph5

      jm1 = jm - 1
      pi  = 4.d0 * datan(1.d0)
      dl  = (pi+pi)/dble(im)
      dp  = pi/dble(jm1)

      do 10 j=2,jm
         ph5  = -0.5d0*pi + (dble(j-1)-0.5d0)*(pi/dble(jm1))
10    sine(j) = dsin(ph5)

      cosp( 1) =  0.
      cosp(jm) =  0.

      do 80 j=2,jm1
80    cosp(j) = (sine(j+1)-sine(j)) / dp

c Define cosine at edges..

      do 90 j=2,jm
90    cose(j) = 0.5 * (cosp(j-1) + cosp(j))
      cose(1) = cose(2)

      sinp( 1) = -1.
      sinp(jm) =  1.

      do 100 j=2,jm1
100   sinp(j) = 0.5 * (sine(j) + sine(j+1))

      write(6,*) 'SETRIG called. ',im,jm
      return
      end subroutine setrig


      subroutine wrt3dr(iout,im,jm,km,a3,a2)
      implicit none
      integer iout, im, jm, km
      integer i, j, k
      real a3(im,jm,*)
      real*4 a2(im,jm)

      do 50 k=km,1,-1
      do 10 j=1,jm
      do 10 i=1,im
      if(abs(a3(i,j,k)) .lt. 1.e-25) then
         a2(i,j) = 0.
      else
         a2(i,j) = a3(i,j,k)
      endif
10    continue
      write(iout) a2
50    continue
      return
      end subroutine wrt3dr



!-----------------------------------------------------------------------
!BOP
! !ROUTINE: drymadj_ --- Total dry air mass
!
! !INTERFACE:

      subroutine drymadj_( im, jm, km, jfirst, jlast,
     &                    moun, ak, bk, ps, delp, nq, q, id )

! !USES:

      implicit   none

! !INPUT PARAMETERS:
      integer im, jm, km     ! Dimensions
      integer jfirst, jlast  ! Latitude strip
      integer       id       ! 0:  checking total dry mass only
                             ! 1:  checking total dry mass and adjust
      integer nq             ! Number of tracers         
      logical moun
      real    ak(km+1)
      real    bk(km+1)

! !INPUT/OUTPUT PARAMETERS:
      real delp(im,jfirst:jlast,km)     ! pressure thickness
      real    q(im,jfirst:jlast,km,nq) 
      real   ps(im,jfirst:jlast)        ! surface pressure

! !DESCRIPTION:
!  Perform adjustment of the total dry-air-mass while preserving total
!  tracer mass
!  Developer: S.-J. Lin
!
! !REVISION HISTORY:
!   WS  99.10.26:  Revision; documentation; removed fvcore.h
!   WS  99.11.02:  Limited to jfirst:jlast
!   SJL 00.03.20:
!
!EOP
!---------------------------------------------------------------------
!BOC

      real psd(im,jfirst:jlast)     ! surface pressure  due to dry air mass

      real drym                     ! dry air mass in pascals
      parameter ( drym = 98222. )

      integer   i, j, k
      real  psmo
      real  psdry
      real  dpd
      integer ic

! Check global maximum/minimum
      PSMO = gmean(im,jm,jfirst,jlast,ps)
      write(6,*) 
     &        'Total (moist) surface pressure before adjustment = ',
     &         PSMO

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j,k)
#endif

#if ( defined CRAY )
!mic$*       private(i,j,k)
#endif

#if ( defined IRIX64 ) || ( defined PGI )
!$doacross local(i,j,k)
#endif

      do 1000 j=jfirst,jlast
        do i=1,im
          psd(i,j) = ak(1)
        enddo

        if(nq .ne. 0) then
          do k=1, km
            do i=1,im
              psd(i,j) = psd(i,j) +  delp(i,j,k) * ( 1. - q(i,j,k,1) )
            enddo
          enddo
        else
          do k=1, km
            do i=1,im
              psd(i,j) = psd(i,j) +  delp(i,j,k)
            enddo
          enddo
        endif
1000  continue

      psdry = gmean(im,jm,jfirst,jlast,psd(1,jfirst))
      write(6,*) 'mean (dry) surface pressure = ', psdry
      if( id .eq. 0) return

      if(moun) then
        dpd = drym - psdry
      else
        dpd = 1000.*100. - psdry
      endif

      write(6,*) 'dry mass to be added (pascals) =', dpd

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j)
#endif

#if defined( CRAY ) && !defined( CRAY_T3E )
!mic$ do all private(i,j,ic)
#endif

#if ( defined IRIX64 )
!$doacross local(i,j, ic)
#endif

      do 2000 j=jfirst,jlast
        do i=1,im
           ps(i,j) = ps(i,j) + dpd
        enddo

        do ic=1,nq
           do i=1,im
              q(i,j,km,ic) =  q(i,j,km,ic)*delp(i,j,km) /
     &                      ( delp(i,j,km) + dpd )
           enddo
        enddo

        do i=1,im
           delp(i,j,km) = delp(i,j,km) + dpd
        enddo
2000  continue

      PSMO = gmean(im,jm,jfirst,jlast,ps)
      write(6,*) 
     &            'Total (moist) surface pressure after adjustment = ',
     &            PSMO

      return
!EOC
      end subroutine drymadj_
!---------------------------------------------------------------------

      subroutine map1_ppm_( km,   pe1,   q1,
     &                     kn,   pe2,   q2, 
     &       im, i1, i2, j, jfirst, jlast, iv, kord, undef)

! IV = 0: constituents
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate

      implicit none

      real       r3, r23
      parameter (r3 = 1./3., r23 = 2./3.)

! Input:
      integer i1, i2
      integer im, jfirst, jlast, iv, kord
      integer km                             ! Original vertical dimension
      integer kn                             ! Target vertical dimension

      real pe1(im,km+1)
      real pe2(im,kn+1)
      real  q1(im,jfirst:jlast,km)
      real undef
! Output
      real  q2(im,jfirst:jlast,kn)

! Local
      real   dp1(i1:i2,km)
      real  q4(4,i1:i2,km)

      integer i, j, k, l, ll, k0
      real    pl, pr, qsum, delp, esl

      do k=1,km
         do i=i1,i2
             dp1(i,k) = pe1(i,k+1) - pe1(i,k)
            q4(1,i,k) = q1(i,j,k)
         enddo
      enddo

! Compute vertical subgrid distribution
      call ppm2m( q4, dp1, km, i1, i2, iv, kord )

! Mapping
      do 1000 i=i1,i2
         k0 = 1
      do 555 k=1,kn
      do 100 l=k0,km
! locate the top edge: pe2(i,k)
      if(pe2(i,k) .lt. pe1(i,1)) then
!        q2(i,j,k) = q4(2,i,1)
         q2(i,j,k) = undef
         goto 555
      elseif(pe2(i,k) .ge. pe1(i,l) .and. pe2(i,k) .le. pe1(i,l+1)) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if(pe2(i,k+1) .le. pe1(i,l+1)) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            q2(i,j,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))
     .                    *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
               k0 = l
               goto 555
          else
! Fractional area...
            qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+
     .              q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*
     .               (r3*(1.+pl*(1.+pl))))
              do ll=l+1,km
! locate the bottom edge: pe2(i,k+1)
                 if(pe2(i,k+1) .gt. pe1(i,ll+1) ) then
! Whole layer..
                 qsum = qsum + dp1(i,ll)*q4(1,i,ll)
                 else
                 delp = pe2(i,k+1)-pe1(i,ll)
                  esl = delp / dp1(i,ll)
                 qsum = qsum + delp*(q4(2,i,ll)+0.5*esl*
     .                 (q4(3,i,ll)-q4(2,i,ll)+q4(4,i,ll)*(1.-r23*esl)))
                 k0 = ll
                 goto 123
                 endif
              enddo
              goto 123
           endif
      endif
100   continue
123   q2(i,j,k) = qsum / ( pe2(i,k+1) - pe2(i,k) )
555   continue
1000  continue

      return
      end subroutine map1_ppm_

      subroutine ppm2m(a4, delp, km, i1, i2, iv, kord)

! iv = 0: positive definite scalars
! iv = 1: others

      implicit none
! Input
      integer km, lmt, iv
      integer i1, i2
      integer kord
      real    delp(i1:i2,km)
      real    a4(4,i1:i2,km)

! local arrays.
      real   dc(i1:i2,km)
      real   h2(i1:i2,km)
      real delq(i1:i2,km)

      real a1, a2, c1, c2, c3, d1, d2
      real qmax, qmin, cmax, cmin
      real qm, dq, tmp

! Local scalars:
      integer i, k, km1
      real qmp
      real lac
      integer it

      km1 = km - 1
       it = i2 - i1 + 1

      do k=2,km
         do i=i1,i2
            delq(i,k-1) =   a4(1,i,k) - a4(1,i,k-1)
            a4(4,i,k  ) = delp(i,k-1) + delp(i,k)
         enddo
      enddo

      do k=2,km1
         do i=i1,i2
            c1 = (delp(i,k-1)+0.5*delp(i,k))/a4(4,i,k+1)
            c2 = (delp(i,k+1)+0.5*delp(i,k))/a4(4,i,k)
            tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /
     &                              (a4(4,i,k)+delp(i,k+1))
            qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
            qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
            dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
         enddo
      enddo

!****6***0*********0*********0*********0*********0*********0**********72
! 4th order interpolation of the provisional cell edge value
!****6***0*********0*********0*********0*********0*********0**********72

      do k=3,km1
      do i=i1,i2
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) *
     &          ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -
     &                          delp(i,k-1)*a1*dc(i,k  ) )
      enddo
      enddo

! Area preserving cubic with 2nd deriv. = 0 at the boundaries
! Top
      do i=i1,i2
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

      if(iv .eq. 0) then
         do i=i1,i2
            a4(2,i,1) = max(0.,a4(2,i,1))
            a4(2,i,2) = max(0.,a4(2,i,2))
         enddo
      endif

! Bottom
! Area preserving cubic with 2nd deriv. = 0 at the surface
      do i=i1,i2
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
      enddo

      if(iv .eq. 0) then
      do i=i1,i2
         a4(2,i,km) = max(0.,a4(2,i,km))
         a4(3,i,km) = max(0.,a4(3,i,km))
      enddo
      endif

      do k=1,km1
         do i=i1,i2
            a4(3,i,k) = a4(2,i,k+1)
         enddo
      enddo
 
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
 
! Top 2 and bottom 2 layers always use monotonic mapping
      do k=1,2
         do i=i1,i2
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(i1,k), a4(1,i1,k), it, 0)
      enddo

      if(kord .eq. 7) then
!****6***0*********0*********0*********0*********0*********0**********72
! Huynh's 2nd constraint
!****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=i1,i2
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 1000 k=3, km-2
        do i=i1,i2
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
        enddo
! Additional constraint to prevent negatives
         if (iv .eq. 0) then
             call kmppm(dc(i1,k), a4(1,i1,k), it, 2)
         endif
1000  continue

      else
 
         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv .eq. 0) lmt = min(2, lmt)

      do k=3, km-2
         do i=i1,i2
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(i1,k), a4(1,i1,k), it, lmt)
      enddo
      endif

      do k=km1,km
         do i=i1,i2
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(i1,k), a4(1,i1,k), it, 0)
      enddo

      return
      end subroutine ppm2m

      subroutine kmppm(dm, a4, im, lmt)
      implicit none

      real       r12
      parameter (r12 = 1./12.)

      integer im, lmt
      real    a4(4,*)
      real    dm(*)

      integer i
      real da1, da2, a6da
      real fmin

      if ( lmt .eq. 3 ) return
! Full constraint

      if(lmt .eq. 0) then
      do i=1,im
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
      enddo

      elseif (lmt .eq. 2) then
! Positive definite constraint

      do i=1,im
      if( abs(a4(3,i)-a4(2,i)) .lt. -a4(4,i) ) then
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
      if( fmin .lt. 0. ) then
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
      endif
      endif
      enddo

      elseif (lmt .eq. 1) then
              write(6,*) 'semi-monotonic not implemented!'
              stop
      endif

      return
      end subroutine kmppm

      end module m_mapz
     
!---------------------------------------------------------------------------


#ifdef _ORIGINAL_MAIN_

      program mapz

! ****************
! Vertical mapping
! ****************

! this program is for mapping from an arbitrary vertical domain
! with to an arbitrary vertical domain with the same surface pressure
! and same horizontal resolution

! Developer: S.-J. Lin
! Mar 27, 2000

      integer im, jm, nl
      integer ih

! Horizontal resolution: (im,jm)
! Number of constituents including water vapor: nc
      parameter ( nc = 1)

      real ptop

! Original data with km layers

! vertical resolution of the target: nl

      write(6,*) '*************************************************'
      write(6,*) 'This program is for the vertical mapping of fvgcm'
      write(6,*) 'dynamical restart file: d_rst'
      write(6,*) '*************************************************'

      write(6,*) ' '
      write(6,*) 'Resoultion? Choose from the following:'
      write(6,*) '0: 4x5;  1: 2x2.5;  2: 1x1.25;  3: 0.5x0.625'
      read(*,*) ih

      if( ih .eq. 0) then
          im = 72
          jm = 46
      elseif( ih .eq. 1) then
          im = 144
          jm = 91
      elseif( ih .eq. 2) then
          im = 288
          jm = 181
      elseif( ih .eq. 3) then
          im = 576
          jm = 361
      else
          write(6,*) 'No suitable resolution chosen'
          stop
      endif
      
      write(6,*) 'Original vertical dimension km=?'
      read(*,*) km

      write(6,*) 'Original model top (Pa)'
      read(*,*) ptop

      write(6,*) 'Vertical dimension of the target nl=?'
      read(*,*) nl

      write(6,*) ' '
      write(6,*) 'Input file name is assumed to be d_rst'
      write(6,*) 'Output file will be d_rst_new'
      iuic = 71
      iout = 81
      open (unit=iuic,file='d_rst',form='unformatted',status='old')
      open (unit=iout,file='d_rst_new',form='unformatted',
     &      status='unknown')

      call z_mapping(iuic, iout, im, jm, km, nc, ptop, nl) 

      end
#endif /* _ORIGINAL_MAIN_ */
