#include "unused_dummy.H"
!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !MODULE: m_maph_pert --- Performs horizontal interpolation of pert fields:
!                          u, v, pt, delp, q
!                     
!
! !INTERFACE:
!
      module m_maph_pert

!USES:

      use m_dyn
      use m_interpack,    only: interpack_terpv
      use m_interpack_ad, only: interpack_terpv_ad
      use m_chars,        only: lowercase

      implicit NONE
 
! !PUBLIC MEMBER FUNCTIONS:
 
      PRIVATE
      PUBLIC h_map_pert        ! horizontal interpolation of fields in d_rst

! !PUBLIC MEMBER FUNCTIONS:

      interface h_map_pert; module procedure &
         h_map_pert_,&
         simple_interp_
      end interface

! !DESCRIPTION: Adopted m_maph to create this program 
!
! !REMARKS:
!
! !SEE ALSO: dyn2dyn
!
! !REVISION HISTORY:
!
!  23Sep2004  Todling    Initial (wrapper) code.
!  30Sep2004  Todling    Using set_eta and drymadj from m_mapz (when needed).
!  27Apr2006  Elena N.   Adopted m_maph.F90 to write this code
!
!EOP
!-------------------------------------------------------------------------
 
      logical, parameter :: echo = .false.

      CONTAINS

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  h_map_pert_ --- Interpolates pert vector
!
! !INTERFACE:
!
   subroutine  h_map_pert_ ( w_i, w_o, rc, &
                            phis_yes, ts_yes, adm, verbose, lwifile ) ! optionals
!
! !USES:
!
   implicit NONE
!
! !INPUT PARAMETERS:
!
   type(dyn_vect), intent(in)    :: w_i    ! original state vector

   logical, optional, intent(in) :: verbose          ! when .t., echoes summary information
   logical, optional, intent(in) :: phis_yes, ts_yes, adm
   character(len=*), optional, intent(in) :: lwifile ! filename with LWI field
!
! !OUTPUT PARAMETERS:
!
   type(dyn_vect), intent(inout)   :: w_o    ! interpolated state vector
   integer, intent(out)          :: rc     ! error return code:
                                           !  0 - all is well
                                           !  1 - inconsistent vertical resolution
                                           !  2 - tracers on output not init right
                                           !  3 - LWI not defined
!
! !DESCRIPTION: Interpolates state vectors between different horizontal
!               resolutions; based on S. J. Lin's original maph code.
!
! !REVISION HISTORY:
!
!  23Sep2005 Todling   Initial code.
!  27Apr2006  Elena N.   Adopted m_maph.F90 to write this code
!
!EOP
!----------------------------------------------------------------------------

      integer im, jm, km, lm
      integer in, jn, kn, ln
      integer k
      logical verb, phis_flag, ts_flag, adm_flag

! Declare pointers
! ----------------
                                           ! input fields
      real, pointer :: phism(:,:)          !   surface geopotential
      real, pointer ::   hsm(:,:)          !   topography height stdv
      real, pointer ::   tsm(:,:)          !   sea surface temperature
      real, pointer ::    um(:,:,:)        !   zonal wind on D-grid
      real, pointer ::    vm(:,:,:)        !   meridional wind
      real, pointer ::   ptm(:,:,:)        !   scaled potential temperature
      real, pointer ::    qm(:,:,:,:)      !   specific humidity & mixing ratios
      real, pointer :: delpm(:,:,:)        !   pressure thickness
                                           ! output fields
      real, pointer :: phisn(:,:)          !   surface geopotential
      real, pointer ::   hsn(:,:)          !   topography height stdv
      real, pointer ::   tsn(:,:)          !   sea surface temperature
      real, pointer ::    un(:,:,:)        !   zonal wind on D-grid
      real, pointer ::    vn(:,:,:)        !   meridional wind
      real, pointer ::   ptn(:,:,:)        !   scaled potential temperature
      real, pointer ::    qn(:,:,:,:)      !   specific humidity & mixing ratios
      real, pointer :: delpn(:,:,:)        !   pressure thickness

! -------------------------------------------
      _UNUSED_DUMMY(lwifile)

      verb = .false.
      if (present(verbose)) verb = verbose 
      phis_flag = .false.
      if (present(phis_yes)) phis_flag = phis_yes 
      ts_flag = .false.
      if (present(ts_yes)) ts_flag = ts_yes
      adm_flag = .false.
      if (present(adm)) then
         adm_flag = adm
      end if
! -------------------------------------------

      im = w_i%grid%im
      jm = w_i%grid%jm
      km = w_i%grid%km
      lm = w_i%grid%lm

      in = w_o%grid%im
      jn = w_o%grid%jm
      kn = w_o%grid%km
      ln = w_o%grid%lm

     print *, 'IN HMAP_PERT: '
     print *, im, jm, km, lm, ' im, jm, km, lm'
     print *, in, jn, kn, ln, ' in, jn, kn, ln'

! Make sure vertical resolution is consistent
! -------------------------------------------
      if ( kn/=km ) then
           rc = 1
           return
       endif

! Check for properly initialized number of tracers in output
! ----------------------------------------------------------
      if (( ln/=lm ).and.( ln > 0 ).and.( lm > 0 )) then
           print *, '    WILL INTERPOLATE SPHU only   '
           print *, ' FILL OTHER TRACERS WITH ZERO FOR NOW '
!           rc = 2
!           return
       endif

! Fill in pointers in
! -------------------
         um => w_i%u
         vm => w_i%v
        ptm => w_i%pt
         qm => w_i%q
      delpm => w_i%delp
      phism => w_i%phis
        hsm => w_i%hs_stdv
        tsm => w_i%ts

! Fill in pointers out
! --------------------
         un => w_o%u
         vn => w_o%v
        ptn => w_o%pt
         qn => w_o%q
      delpn => w_o%delp
      phisn => w_o%phis
        hsn => w_o%hs_stdv
        tsn => w_o%ts


!**********************
! Perform interpolation
!**********************
      print *, 'START INTERPOLATION'
      if (( phis_flag ).and.( ts_flag )) then
              call h_map_pert_do_ (im, jm, km, lm,        &
                          um, vm, ptm, qm, delpm, &
                          in, jn, ln,             &
                          un, vn, ptn, qn, delpn, &
                          phism, hsm, tsm,        &
                          phisn, hsn, tsn,        &
                                        verbose = verb)

      else if (( phis_flag ).and.(.not. ts_flag )) then
	      call h_map_pert_do_ (im, jm, km, lm,        &
                          um, vm, ptm, qm, delpm, &
                          in, jn, ln,             &
                          un, vn, ptn, qn, delpn, &
                          phism, hsm,             &
                          phisn, hsn,             &
                                       verbose = verb)

      else if (( ts_flag ).and.(.not. phis_flag )) then 
              call h_map_pert_do_ (im, jm, km, lm,        &
                          um, vm, ptm, qm, delpm, &
                          in, jn, ln,             &
                          un, vn, ptn, qn, delpn, &
                          tsm,                    &
                          tsn,                    &
                                        verbose = verb)

      else
              call h_map_pert_do_ (im, jm, km, lm,        &
                          um, vm, ptm, qm, delpm, &
                          in, jn, ln,             &
                          un, vn, ptn, qn, delpn, &
                                        verbose = verb)
      endif

      print *, 'DONE INTERPOLATION'

      w_o%u    => un
      w_o%v    => vn
     w_o%pt    => ptn
      w_o%q    => qn
    w_o%delp   => delpn 
    w_o%phis   => phisn
 w_o%hs_stdv   => hsn
      w_o%ts   => tsn

      if ( adm_flag ) then
         w_o%ps = 0.0
         do k=1,size(w_o%delp,3)
            w_o%ps = w_o%ps + (w_o%grid%bk(k+1)-w_o%grid%bk(k))*w_o%delp(:,:,k)
         enddo
      else
         w_o%ps = 0.0
         do k=1,size(w_o%delp,3)
            w_o%ps = w_o%ps + w_o%delp(:,:,k)
         enddo
      endif

      end subroutine h_map_pert_ 

!*****************************************************************
      subroutine h_map_pert_do_(im, jm, km, lm, & 
                                um, vm, ptm, qm, delpm, &
                                in, jn, ln,             &
                                un, vn, ptn, qn, delpn, &
                                phism, hsm, tsm,        &
                                phisn, hsn, tsn,        &
                                                 verbose) 
!
! Purpose: perform horizontal mapping of d_rst
!
      implicit none

      integer im, jm, km, lm 
      integer in, jn, ln
      logical, optional :: verbose

! Original
      real     um(im,jm,km)        !zonal wind on D-grid
      real     vm(im,jm,km)        !meridional wind
      real    ptm(im,jm,km)        !scaled potential temperature
      real     qm(im,jm,km,lm)     !specific humidity & mixing ratios
      real  delpm(im,jm,km)        ! pressure thickness
      real, optional :: phism(im,jm)  !   surface geopotential
      real, optional ::   hsm(im,jm)  !   topography height stdv
      real, optional ::   tsm(im,jm)  !   sea surface temperature


! New data 
      real     un(in,jn,km)        !zonal wind on D-grid
      real     vn(in,jn,km)        !meridional wind
      real    ptn(in,jn,km)        !scaled potential temperature
      real     qn(in,jn,km,ln)     !specific humidity & mixing ratios
      real  delpn(in,jn,km)        ! pressure thickness
      real, optional :: phisn(in,jn)  !   surface geopotential
      real, optional ::   hsn(in,jn)  !   topography height stdv
      real, optional ::   tsn(in,jn)  !   sea surface temperature

      real pmax, pmin

      real dx1, dx2
      real dy1, dy2

      real lon1(im+1)
      real lon2(in+1)

! Pole to pole
      real sin1(jm+1)
      real sin2(jn+1)

      integer i, j
      real dl, dp
      real cosp(jm), cose(jm), sine(jm), sinp(jm)
      real sinlon(im), coslon(im), cosl5(im), sinl5(im)
      integer imh
      logical verb
      double precision pi, zamda, zam5

      imh = im/2

      pi = 4.d0 * datan(1.d0)
      call setrig(im,jm,dp,dl,cosp,cose,sinp,sine)

        do i=1,imh
           zam5          = (dble(i-1)-0.5d0) * dl
           cosl5(i)      = -dcos(zam5)
           cosl5(i+imh)  = -cosl5(i)
           sinl5(i)      = -dsin(zam5)
           sinl5(i+imh)  = -sinl5(i)

           zamda         = dble(i-1)*dl
           coslon(i)     = -dcos(zamda)
           coslon(i+imh) = -coslon(i)
           sinlon(i)     = -dsin(zamda)
           sinlon(i+imh) = -sinlon(i)
        enddo

      dx1 = 360./float(im)
      dx2 = 360./float(in)

      dy1 = pi/float(jm-1)
      dy2 = pi/float(jn-1)

      do i=1,im+1
         lon1(i) = dx1 * (-0.5 + (i-1) )
      enddo

      do i=1,in+1
         lon2(i) = dx2 * (-0.5 + (i-1) )
      enddo

         sin1(   1) = -1.
         sin1(jm+1) =  1.

         sin2(   1) = -1.
         sin2(jn+1) =  1.

           do j=2,jm
              sin1(j) = sin( -0.5*pi + dy1*(-0.5+float(j-1)) )
           enddo

           do j=2,jn
              sin2(j) = sin( -0.5*pi + dy2*(-0.5+float(j-1)) )
           enddo

!**************
      verb = .false.
      if ( present(verbose) ) verb = verbose

!**************
! Map ts
!**************
      if (present(tsm).and.present(tsn) ) then
          call map_a2a(1 , im, jm, lon1, sin1, tsm,  &
                           in, jn, lon2, sin2, tsn, 0, 0)
      endif

!**************
! Map hs
!**************
      if (present(hsm).and.present(hsn) ) then
          call map_a2a(1 , im, jm, lon1, sin1, hsm,  &
                           in, jn, lon2, sin2, hsn, 0, 0)
      endif

!**************
! Map phis
!**************
      if (present(phism).and.present(phisn) ) then
          call map_a2a(1 , im, jm, lon1, sin1, phism,  & 
                           in, jn, lon2, sin2, phisn, 0, 0)
      endif

!**************
! Map delp
!**************
      call map_a2a(km , im, jm, lon1, sin1, delpm, &
                        in, jn, lon2, sin2, delpn, 0, 0)

!**************
! Map pt
!**************
      call map_a2a(km , im, jm, lon1, sin1, ptm, &
                        in, jn, lon2, sin2, ptn, 0, 0)
      if(verb) call pmaxmin('PT_new',ptn, pmin, pmax,in*jn, km, 1. )

!**************
! Constituents:
!**************
!      do ic=1,nc
!          call map_a2a(km, im, jm, lon1, sin1, qm(1,1,1,ic), &
!                           in, jn, lon2, sin2, qn(1,1,1,ic), 0, 0)
!          if(verb) call pmaxmin('Q_new', qn(1,1,1,ic), pmin, pmax,in*jn, km, 1. )
!      enddo
          call map_a2a(km, im, jm, lon1, sin1, qm(1,1,1,1), &
                           in, jn, lon2, sin2, qn(1,1,1,1), 0, 0)
          if(verb) call pmaxmin('Q_new', qn(1,1,1,1), pmin, pmax,in*jn, km, 1. )

          if (ln > 1) qn(:,:,:,2:ln) = 0.d0


!**************
! Initialize u-wind and v_wind to zero: values at te poles will stay zeros, 
!  since the segment below does not redefine those
!**************
      un = 0.d0
      vn = 0.d0
!**************
! Map u-wind
!**************
! ig = 1                   ! data defined "half dy" from pole
         sin1( 1) = -1.
         sin1(jm) =  1.

         sin2( 1) = -1.
         sin2(jn) =  1.


           do j=2,jm-1
              sin1(j) = sin( -0.5*pi + dy1*float(j-1) )
           enddo

           do j=2,jn-1
              sin2(j) = sin( -0.5*pi + dy2*float(j-1) )
           enddo

! Note: u(i,j=1,k) are NOT defined

      call map_a2a(km, im, jm, lon1, sin1, um,  &
                       in, jn, lon2, sin2, un, 1, 1)
      if(verb) call pmaxmin('U_new', un, pmin, pmax, in*jn, km, 1. )

!**************
! Map v-wind
!**************
! v loated half-dx west cell center


      do i=1,im+1
         lon1(i) = dx1 * float(i-2)
      enddo

      do i=1,in+1
         lon2(i) = dx2 * float(i-2)
      enddo

         sin1(   1) = -1.
         sin1(jm+1) =  1.

         sin2(   1) = -1.
         sin2(jn+1) =  1.

      do j=2,jm
         sin1(j) = sin( -0.5*pi + dy1*(-0.5+float(j-1)) )
      enddo

      do j=2,jn
         sin2(j) = sin( -0.5*pi + dy2*(-0.5+float(j-1)) )
      enddo

      call map_a2a(km , im, jm, lon1, sin1, vm,  &
                        in, jn, lon2, sin2, vn, 0, 1)
      if(verb) call pmaxmin('V_new', vn, pmin, pmax, in*jn, km, 1. )

      if(verb) write(6,*) ' '
      if(verb) write(6,*) 'Done horizontal mapping'

! The user may wish to modify nymd, nhms, and nstep here for "branch run"

      return
      end subroutine h_map_pert_do_

!**************

      subroutine map_a2a(km, im, jm, lon1, sin1, q1,  &
                         in, jn, lon2, sin2, q2, ig, iv)

! Horizontal arbitrary grid to arbitrary grid conservative high-order mapping
! S.-J. Lin

      implicit none
      integer km
      integer im, jm
      integer in, jn
      integer ig                ! ig=0: pole to pole; ig=1 j=1 is half-dy

                                ! north of south pole
      integer iv                ! iv=0: scalar; iv=1: vector
      real q1(im,jm,km)

      real lon1(im+1)
      real lon2(in+1)

      real sin1(jm+1)
      real sin2(jn+1)

! Output
      real q2(in,jn,km)
! local
      integer i,j,k
      real qtmp(in,jm)

      do k=1, km
        if( im .eq. in ) then
            do j=1,jm-ig
               do i=1,im
                  qtmp(i,j+ig) = q1(i,j+ig,k)
               enddo
            enddo
        else
! Mapping in the E-W direction
         call xmap(im, jm-ig, lon1, q1(1,1+ig,k),  &
                   in, lon2, qtmp(1,1+ig) )
        endif

        if( jm .eq. jn ) then
            do j=1,jm-ig
               do i=1,in

                  q2(i,j+ig,k) = qtmp(i,j+ig)
               enddo
            enddo
        else
! Mapping in the N-S direction
         call ymap(in, jm, sin1, qtmp(1,1+ig),  &
                       jn, sin2, q2(1,1+ig,k), ig, iv)
        endif
      enddo

      return
      end subroutine map_a2a

!**************

      subroutine ymap(im, jm, sin1, q1, jn, sin2, q2, ig, iv)

! Routine to perform area preserving mapping in N-S from an arbitrary
! resolution to another.
!
! sin1 (1) = -1 must be south pole; sin1(jm+1)=1 must be N pole.
!
! sin1(1) < sin1(2) < sin1(3) < ... < sin1(jm) < sin1(jm+1)
! sin2(1) < sin2(2) < sin2(3) < ... < sin2(jn) < sin2(jn+1)
!
! Developer: S.-J. Lin
! First version: piece-wise constant mapping
! Apr 1, 2000
! Last modified:

      implicit none

! Input
      integer im              ! original E-W dimension
      integer jm              ! original N-S dimension
      integer jn              ! Target N-S dimension
      integer ig              ! ig=0: scalars from S.P. to N. P.
                              ! D-grid v-wind is also ig 0
                              ! ig=1: D-grid u-wind
      integer iv              ! iv=0 scalar; iv=1: vector
      real    sin1(jm+1-ig)   ! original southern edge of the cell
                              ! sin(lat1)
!     real    q1(im,jm-ig)    ! original data at center of the cell
      real    q1(im,jm)       ! original data at center of the cell
      real    sin2(jn+1-ig)   ! Target cell's southern edge
                              ! sin(lat2)

! Output
!     real    q2(im,jn-ig)    ! Mapped data at the target resolution
      real    q2(im,jn)       ! Mapped data at the target resolution

! Local
      integer i, j0, m, mm
      integer j

! PPM related arrays
      real   al(im,jm)
      real   ar(im,jm)
      real   a6(im,jm)
      real  dy1(jm)

      real  r3, r23
      parameter ( r3 = 1./3., r23 = 2./3. )
      real pl, pr, qsum, esl
      real dy, sum

      do j=1,jm-ig
         dy1(j) = sin1(j+1) - sin1(j)
      enddo


! ***********************
! Area preserving mapping
! ***********************

! Construct subgrid PP distribution
      call ppm_lat(im, jm, ig, q1, al, ar, a6, 3, iv)

      do 1000 i=1,im
         j0 = 1
      do 555 j=1,jn-ig
      do 100 m=j0,jm-ig
!
! locate the southern edge: sin2(i)
!
      if(sin2(j) .ge. sin1(m) .and. sin2(j) .le. sin1(m+1)) then
         pl = (sin2(j)-sin1(m)) / dy1(m)
         if(sin2(j+1) .le. sin1(m+1)) then
! entire new cell is within the original cell
            pr = (sin2(j+1)-sin1(m)) / dy1(m)
            q2(i,j) = al(i,m) + 0.5*(a6(i,m)+ar(i,m)-al(i,m)) &
                          *(pr+pl)-a6(i,m)*r3*(pr*(pr+pl)+pl**2)
               j0 = m
               goto 555
          else
! South most fractional area
            qsum = (sin1(m+1)-sin2(j))*(al(i,m)+0.5*(a6(i,m)+ &
                    ar(i,m)-al(i,m))*(1.+pl)-a6(i,m)* &
                     (r3*(1.+pl*(1.+pl))))

              do mm=m+1,jm-ig
! locate the eastern edge: sin2(j+1)
                 if(sin2(j+1) .gt. sin1(mm+1) ) then
! Whole layer
                     qsum = qsum + dy1(mm)*q1(i,mm)
                 else
! North most fractional area
                     dy = sin2(j+1)-sin1(mm)
                    esl = dy / dy1(mm)
                   qsum = qsum + dy*(al(i,mm)+0.5*esl*  &
                         (ar(i,mm)-al(i,mm)+a6(i,mm)*(1.-r23*esl)))
                     j0 = mm
                     goto 123
                 endif
              enddo
              goto 123
           endif
      endif
100   continue
123   q2(i,j) = qsum / ( sin2(j+1) - sin2(j) )
555   continue
1000  continue

! Final processing for poles

      if ( ig .eq. 0 .and. iv .eq. 0 ) then

! South pole

           sum = 0.
         do i=1,im
           sum = sum + q2(i,1)
         enddo

           sum = sum / float(im)
         do i=1,im
           q2(i,1) = sum
         enddo

! North pole:
           sum = 0.

         do i=1,im
           sum = sum + q2(i,jn)
         enddo

           sum = sum / float(im)
         do i=1,im
           q2(i,jn) = sum
         enddo

      endif

      return
      end subroutine ymap

      subroutine ppm_lat(im, jm, ig, q, al, ar, a6, jord, iv)
      implicit none

!INPUT
! ig=0: scalar pole to pole
! ig=1: D-grid u-wind; not defined at poles because of staggering

      integer im, jm                      !  Dimensions
      integer ig
      real  q(im,jm-ig)
      real al(im,jm-ig)
      real ar(im,jm-ig)
      real a6(im,jm-ig)
      integer jord
      integer iv                             ! iv=0 scalar
                                             ! iv=1 vector
! Local
      real dm(im,jm-ig)
      real    r3
      parameter ( r3 = 1./3. )
      integer i, j, im2, iop, jm1
      real tmp, qmax, qmin
      real qop

! Compute dm: linear slope

      do j=2,jm-1-ig
         do i=1,im
            dm(i,j) = 0.25*(q(i,j+1) - q(i,j-1))
            qmax = max(q(i,j-1),q(i,j),q(i,j+1)) - q(i,j)
            qmin = q(i,j) - min(q(i,j-1),q(i,j),q(i,j+1))
            dm(i,j) = sign(min(abs(dm(i,j)),qmin,qmax),dm(i,j))
         enddo
      enddo

      im2 = im/2
      jm1 = jm - 1


!Poles:

      if (iv .eq. 1 ) then
!
!*********
! u-wind (ig=1)
! v-wind (ig=0)
!*********
!
! SP
          do i=1,im
              if( i .le. im2) then
                  qop = -q(i+im2,2-ig)
              else
                  qop = -q(i-im2,2-ig)
              endif
              tmp = 0.25*(q(i,2) - qop)
              qmax = max(q(i,2),q(i,1), qop) - q(i,1)
              qmin = q(i,1) - min(q(i,2),q(i,1), qop)
              dm(i,1) = sign(min(abs(tmp),qmax,qmin),tmp)
           enddo
! NP
           do i=1,im
              if( i .le. im2) then
                  qop = -q(i+im2,jm1)
              else
                  qop = -q(i-im2,jm1)
              endif
              tmp = 0.25*(qop - q(i,jm1-ig))
              qmax = max(qop,q(i,jm-ig), q(i,jm1-ig)) - q(i,jm-ig)
              qmin = q(i,jm-ig) - min(qop,q(i,jm-ig), q(i,jm1-ig))
              dm(i,jm-ig) = sign(min(abs(tmp),qmax,qmin),tmp)
           enddo
      else
!
!*********
! Scalar:
!*********
! This code segment currently works only if ig=0
! SP
          do i=1,im2
            tmp = 0.25*(q(i,2)-q(i+im2,2))
            qmax = max(q(i,2),q(i,1), q(i+im2,2)) - q(i,1)
            qmin = q(i,1) - min(q(i,2),q(i,1), q(i+im2,2))
            dm(i,1) = sign(min(abs(tmp),qmax,qmin),tmp)
          enddo

          do i=im2+1,im
            dm(i, 1) =  - dm(i-im2, 1)
          enddo
! NP
          do i=1,im2
            tmp = 0.25*(q(i+im2,jm1)-q(i,jm1))
            qmax = max(q(i+im2,jm1),q(i,jm), q(i,jm1)) - q(i,jm)
            qmin = q(i,jm) - min(q(i+im2,jm1),q(i,jm), q(i,jm1))
            dm(i,jm) = sign(min(abs(tmp),qmax,qmin),tmp)
          enddo

          do i=im2+1,im
            dm(i,jm) =  - dm(i-im2,jm)
          enddo
      endif

      do j=2,jm-ig
        do i=1,im
          al(i,j) = 0.5*(q(i,j-1)+q(i,j)) + r3*(dm(i,j-1) - dm(i,j))
        enddo
      enddo

      do j=1,jm-1-ig
        do i=1,im
          ar(i,j) = al(i,j+1)
        enddo
      enddo

      if ( iv .eq. 1 ) then
! Vector:
! ig=0
        if ( ig .eq. 0 ) then
          do i=1,im2
            al(i,    1) = -al(i+im2,2)
            al(i+im2,1) = -al(i,    2)
          enddo

          do i=1,im2
            ar(i,    jm) = -ar(i+im2,jm1)
            ar(i+im2,jm) = -ar(i,    jm1)
          enddo
        else
! ig=1
! SP
          do i=1,im
             if( i .le. im2) then
                 iop = i+im2
             else
                 iop = i-im2
             endif
             al(i,1) = 0.5*(q(i,1)-q(iop,1)) - r3*(dm(iop,1) + dm(i,1))
          enddo
! NP
          do i=1,im
             if( i .le. im2) then
                 iop = i+im2
             else
                 iop = i-im2
             endif
             ar(i,jm1) = 0.5*(q(i,jm1)-q(iop,jm1)) -  &
                         r3*(dm(iop,jm1) + dm(i,jm1))
          enddo
        endif
      else
! Scalar (works for ig=0 only):
          do i=1,im2
            al(i,    1) = al(i+im2,2)
            al(i+im2,1) = al(i,    2)
          enddo

          do i=1,im2
            ar(i,    jm) = ar(i+im2,jm1)
            ar(i+im2,jm) = ar(i,    jm1)
          enddo
      endif

      do j=1,jm-ig
        do i=1,im
          a6(i,j) = 3.*(q(i,j)+q(i,j) - (al(i,j)+ar(i,j)))
        enddo
        call lmppm(dm(1,j), a6(1,j), ar(1,j),  &
                   al(1,j),  q(1,j), im, jord-3)
      enddo

      return
      end subroutine ppm_lat

      subroutine xmap(im, jm, lon1, q1, in, lon2, q2)

! Routine to perform area preserving mapping in E-W from an arbitrary
! resolution to another.
! Periodic domain will be assumed, i.e., the eastern wall bounding cell
! im is lon1(im+1) = lon1(1); Note the equal sign is true geographysically.
!
! lon1(1) < lon1(2) < lon1(3) < ... < lon1(im) < lon1(im+1)
! lon2(1) < lon2(2) < lon2(3) < ... < lon2(in) < lon2(in+1)
!
! Developer: S.-J. Lin
! First version: piece-wise constant mapping
! Apr 1, 2000
! Last modified:

      implicit none

! Input
      integer im              ! original E-W dimension
      integer in              ! Target E-W dimension
      integer jm              ! original N-S dimension

      real    lon1(im+1)      ! original western edge of the cell
      real    q1(im,jm)       ! original data at center of the cell
      real    lon2(in+1)      ! Target cell's western edge

! Output
      real    q2(in,jm)       ! Mapped data at the target resolution

! Local
      integer i1, i2
      integer i, i0, m, mm
      integer j

! PPM related arrays
      real qtmp(-im:im+im)
      real   al(-im:im+im)
      real   ar(-im:im+im)
      real   a6(-im:im+im)
      real   x1(-im:im+im+1)
      real  dx1(-im:im+im)
      real  r3, r23
      parameter ( r3 = 1./3., r23 = 2./3. )
      real pl, pr, qsum, esl
      real dx
      integer iord
      data iord /3/
      logical found

      do i=1,im+1
         x1(i) = lon1(i)
      enddo

      do i=1,im
         dx1(i) = x1(i+1) - x1(i)
      enddo

! check to see if ghosting is necessary

!**************
! Western edge:
!**************
          found = .false.
          i1 = 1
      do while ( .not. found )
         if( lon2(1) .ge. x1(i1) ) then
             found = .true.
         else
                  i1 = i1 - 1
             if (i1 .lt. -im) then
                 write(6,*) 'failed in xmap'
                 stop
             else
                 x1(i1) = x1(i1+1) - dx1(im+i1)
                dx1(i1) = dx1(im+i1)

             endif
         endif
      enddo

!**************
! Eastern edge:
!**************
          found = .false.
          i2 = im+1
      do while ( .not. found )
         if( lon2(in+1) .le. x1(i2) ) then
             found = .true.
         else

                  i2 = i2 + 1
             if (i2 .gt. 2*im) then
                 write(6,*) 'failed in xmap'
                 stop
             else
                dx1(i2-1) = dx1(i2-1-im)
                 x1(i2) = x1(i2-1) + dx1(i2-1)
             endif
         endif
      enddo

!     write(6,*) 'i1,i2=',i1,i2

      do 1000 j=1,jm

! ***********************
! Area preserving mapping
! ***********************

! Construct subgrid PP distribution
      call ppm_cycle(im, q1(1,j), al(1), ar(1), a6(1), qtmp(0), iord)

! check to see if ghosting is necessary

! Western edge
          if ( i1 .le. 0 ) then
               do i=i1,0
                  qtmp(i) = qtmp(im+i)
                    al(i) = al(im+i)
                    ar(i) = ar(im+i)
                    a6(i) = a6(im+i)
               enddo
          endif

! Eastern edge:
          if ( i2 .gt. im+1 ) then
             do i=im+1,i2-1
                qtmp(i) = qtmp(i-im)
                  al(i) =   al(i-im)
                  ar(i) =   ar(i-im)
                  a6(i) =   a6(i-im)
             enddo
          endif

         i0 = i1

      do 555 i=1,in
      do 100 m=i0,i2-1
!
! locate the western edge: lon2(i)
!
      if(lon2(i) .ge. x1(m) .and. lon2(i) .le. x1(m+1)) then
         pl = (lon2(i)-x1(m)) / dx1(m)
         if(lon2(i+1) .le. x1(m+1)) then
! entire new grid is within the original grid
            pr = (lon2(i+1)-x1(m)) / dx1(m)
            q2(i,j) = al(m) + 0.5*(a6(m)+ar(m)-al(m))  &
                          *(pr+pl)-a6(m)*r3*(pr*(pr+pl)+pl**2)
               i0 = m
               goto 555
          else
! Left most fractional area
            qsum = (x1(m+1)-lon2(i))*(al(m)+0.5*(a6(m)+ &
                    ar(m)-al(m))*(1.+pl)-a6(m)*         &
                     (r3*(1.+pl*(1.+pl))))
              do mm=m+1,i2-1
! locate the eastern edge: lon2(i+1)
                 if(lon2(i+1) .gt. x1(mm+1) ) then
! Whole layer
                     qsum = qsum + dx1(mm)*qtmp(mm)
                 else
! Right most fractional area
                     dx = lon2(i+1)-x1(mm)
                    esl = dx / dx1(mm)
                   qsum = qsum + dx*(al(mm)+0.5*esl*  &
                         (ar(mm)-al(mm)+a6(mm)*(1.-r23*esl)))
                     i0 = mm
                     goto 123
                 endif

              enddo
              goto 123

           endif
      endif
100   continue
123   q2(i,j) = qsum / ( lon2(i+1) - lon2(i) )
555   continue
1000  continue

      return
      end subroutine xmap

      subroutine ppm_cycle(im, q, al, ar, a6, p, iord)
      implicit none

      real r3
      parameter ( r3 = 1./3. )

! Input 
      integer im, iord
      real  q(1)
! Output
      real al(1)
      real ar(1)
      real a6(1)
      real  p(0:im+1)

! local
      real  dm(0:im)
      integer i, lmt
      real tmp, qmax, qmin

         p(0) = q(im)
      do i=1,im
         p(i) = q(i)
      enddo
         p(im+1) = q(1)

! 2nd order slope
      do i=1,im
         tmp = 0.25*(p(i+1) - p(i-1))
         qmax = max(p(i-1), p(i), p(i+1)) - p(i)
         qmin = p(i) - min(p(i-1), p(i), p(i+1))
         dm(i) = sign(min(abs(tmp),qmax,qmin), tmp)
      enddo
         dm(0) = dm(im)

      do i=1,im
         al(i) = 0.5*(p(i-1)+p(i)) + (dm(i-1) - dm(i))*r3
      enddo

      do i=1,im-1
         ar(i) = al(i+1)
      enddo
         ar(im) = al(1)

      if(iord .le. 6) then
         do i=1,im
            a6(i) = 3.*(p(i)+p(i)  - (al(i)+ar(i)))
         enddo
         lmt = iord - 3
         if(lmt.le.2) call lmppm(dm(1),a6(1),ar(1),al(1),p(1),im,lmt)
      else
         call huynh(im, ar(1), al(1), p(1), a6(1), dm(1))
      endif

      return
      end subroutine ppm_cycle

      subroutine lmppm(dm, a6, ar, al, p, im, lmt)
      implicit none
      real r12
      parameter ( r12 = 1./12. )

      integer im, lmt
      integer i
      real a6(im),ar(im),al(im),p(im),dm(im)
      real da1, da2, fmin, a6da

! LMT = 0: full monotonicity
! LMT = 1: semi-monotonic constraint (no undershoot)

! LMT = 2: positive-definite constraint

      if(lmt.eq.0) then

! Full constraint
      do 100 i=1,im
      if(dm(i) .eq. 0.) then
         ar(i) = p(i)
         al(i) = p(i)
         a6(i) = 0.
      else
         da1  = ar(i) - al(i)
         da2  = da1**2
         a6da = a6(i)*da1
         if(a6da .lt. -da2) then
            a6(i) = 3.*(al(i)-p(i))
            ar(i) = al(i) - a6(i)
         elseif(a6da .gt. da2) then
            a6(i) = 3.*(ar(i)-p(i))
            al(i) = ar(i) - a6(i)
         endif
      endif
100   continue

      elseif(lmt.eq.1) then
! Semi-monotonic constraint
      do 150 i=1,im
      if(abs(ar(i)-al(i)) .ge. -a6(i)) go to 150
      if(p(i).lt.ar(i) .and. p(i).lt.al(i)) then
            ar(i) = p(i)
            al(i) = p(i)
            a6(i) = 0.
      elseif(ar(i) .gt. al(i)) then

            a6(i) = 3.*(al(i)-p(i))
            ar(i) = al(i) - a6(i)
      else
            a6(i) = 3.*(ar(i)-p(i))
            al(i) = ar(i) - a6(i)
      endif
150   continue
      elseif(lmt.eq.2) then
! Positive definite constraint
      do 250 i=1,im
      if(abs(ar(i)-al(i)) .ge. -a6(i)) go to 250
      fmin = p(i) + 0.25*(ar(i)-al(i))**2/a6(i) + a6(i)*r12
      if(fmin.ge.0.) go to 250
      if(p(i).lt.ar(i) .and. p(i).lt.al(i)) then
            ar(i) = p(i)
            al(i) = p(i)
            a6(i) = 0.
      elseif(ar(i) .gt. al(i)) then
            a6(i) = 3.*(al(i)-p(i))
            ar(i) = al(i) - a6(i)
      else
            a6(i) = 3.*(ar(i)-p(i))
            al(i) = ar(i) - a6(i)
      endif
250   continue
      endif
      return
      end subroutine lmppm

      subroutine huynh(im, ar, al, p, d2, d1)

! Enforce Huynh's 2nd constraint in 1D periodic domain

      implicit none
      integer im, i
      real ar(im)
      real al(im)
      real  p(im)
      real d2(im)
      real d1(im)

! Local scalars:
      real pmp
      real lac
      real pmin
      real pmax

! Compute d1 and d2
         d1(1) = p(1) - p(im)
      do i=2,im
         d1(i) = p(i) - p(i-1)
      enddo

      do i=1,im-1
         d2(i) = d1(i+1) - d1(i)
      enddo
         d2(im) = d1(1) - d1(im)

! Constraint for AR
!            i = 1
         pmp   = p(1) + 2.0 * d1(1)
         lac   = p(1) + 0.5 * (d1(1)+d2(im)) + d2(im) 
         pmin  = min(p(1), pmp, lac)
         pmax  = max(p(1), pmp, lac)
         ar(1) = min(pmax, max(ar(1), pmin))

      do i=2, im
         pmp   = p(i) + 2.0*d1(i)
         lac   = p(i) + 0.5*(d1(i)+d2(i-1)) + d2(i-1)
         pmin  = min(p(i), pmp, lac)
         pmax  = max(p(i), pmp, lac)
         ar(i) = min(pmax, max(ar(i), pmin))
      enddo

! Constraint for AL
      do i=1, im-1
         pmp   = p(i) - 2.0*d1(i+1)
         lac   = p(i) + 0.5*(d2(i+1)-d1(i+1)) + d2(i+1)
         pmin  = min(p(i), pmp, lac)
         pmax  = max(p(i), pmp, lac)
         al(i) = min(pmax, max(al(i), pmin))
      enddo

! i=im
         i = im
         pmp    = p(im) - 2.0*d1(1)
         lac    = p(im) + 0.5*(d2(1)-d1(1)) + d2(1)
         pmin   = min(p(im), pmp, lac)
         pmax   = max(p(im), pmp, lac)
         al(im) = min(pmax, max(al(im), pmin))

! compute A6 (d2)
      do i=1, im
         d2(i) = 3.*(p(i)+p(i)  - (al(i)+ar(i)))
      enddo
      return
      end subroutine huynh

! Parallelized utility routine for computing/printing
! max/min of an input array
!
      subroutine pmaxmin( qname, a, pmin, pmax, im, jm, fac )

#define CPP_PRT_PREFIX

      implicit none

      character*(*)  qname
      integer im, jm
      integer i, j
      real a(im,jm)

      real qmin(jm), qmax(jm)
      real pmax, pmin
      real fac                     ! multiplication factor

#if ( defined OpenMP )
!$omp  parallel do
!$omp& default(shared)
!$omp& private(i,j, pmax, pmin)
#endif

#if ( defined IRIX64 )
!$doacross   local(i,j),
!$&          share(im,jm, pmax, pmin)

#endif

      do j=1,jm
         pmax = a(1,j)
         pmin = a(1,j)
         do i=2,im
            pmax = max(pmax, a(i,j))
            pmin = min(pmin, a(i,j))
         enddo
         qmax(j) = pmax
         qmin(j) = pmin
      enddo
!
! Now find max/min of amax/amin
!
            pmax = qmax(1)
            pmin = qmin(1)
         do j=2,jm
            pmax = max(pmax, qmax(j))
            pmin = min(pmin, qmin(j))
         enddo

#if ( defined CPP_PRT_PREFIX )
      write(*,*) qname, ' max = ', pmax*fac, ' min = ', pmin*fac
#endif

      return
      end subroutine pmaxmin


!****6***0*********0*********0*********0*********0*********0**********72
      subroutine rst_dyn(im, jm, km, itp , iuic , delp, u, v, pt, nq, &
                         q   , ps ,  nymd,  nhms, nstep )
!****6***0*********0*********0*********0*********0*********0**********72

      implicit none
      integer  im, jm
      integer  km

      integer nymd, nhms, nstep
      integer nq, iuic
      integer itp
      integer i, j, k, l

      real    u(im,jm,km),    v(im,jm,km),  pt(im,jm,km), &
              delp(im,jm,km), &
              q(im,jm,km,nq), ps(im,jm)


      real pmax, pmin

      if(itp.ge.0) then

      read(iuic) nstep, nymd, nhms
      read(iuic) ps, delp, u, v, pt

      if(itp .eq. 0) then
         nstep = 0
         if(echo) write(6,*) 'nstep reset to zero in rst_dyn()'
      endif

      call pmaxmin('U', u, pmin, pmax, im*jm, km, 1. )
      call pmaxmin('V', v, pmin, pmax, im*jm, km, 1. )
      call pmaxmin('PT', pt, pmin, pmax, im*jm, km, 1. )

      if(nq .ne. 0) then
!_RT     read(iuic) q
         do l = 1, nq
            read(iuic) (((q(i,j,k,l),i=1,im),j=1,jm),k=1,km)
            call pmaxmin('Q', q(1,1,1,l), pmin, pmax, im*jm, km, 1. )
         enddo
      endif

      if(echo) write(6,*) 'done reading ic for the dycore',nymd, nhms, nstep

        if(nstep .eq. 0) then
        do l=1,nq  ! _RT
        do k=1,km

        do j=1,jm
        do i=1,im
           q(i,j,k,l) = max(1.2e-12, q(i,j,k,l))
        enddo
        enddo

        enddo
        enddo
        endif

      else

! Write

      rewind iuic
      write(iuic) nstep, nymd, nhms
      write(iuic) ps, delp,u,v,pt
!_RT  if(nq .ne. 0) write(iuic) q
      do l = 1, nq
         write(iuic) (((q(i,j,k,l),i=1,im),j=1,jm),k=1,km)
      end do

      endif

      return
      end subroutine rst_dyn

!****6***0*********0*********0*********0*********0*********0**********72
      function gmean(im,jm,jfirst,jlast,q)
!****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      real gmean

! !INPUT PARAMETERS:
      integer im, jm                       ! Horizontal dimensions
      integer jfirst, jlast                ! Latitude strip
      real q(im,jfirst:jlast)              ! 2D field

      integer i, j, j2, jmm1
      real dp, acap, area, gsum, xsum
      real sine(jm),cosp(jm),sinp(jm),cose(jm)
      real dl

      j2   = max( 2, jfirst )
      jmm1 = min( jm-1, jlast )
      call setrig(im,jm,dp,dl,cosp,cose,sinp,sine)

! scaled polar cap area.
        acap = im*(1.+sine(2)) / dp

        area = 2.*acap
        do j=2,jm-1
          area = area + cosp(j)*im
        enddo

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
      end function gmean

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine setrig(im,jm,dp,dl,cosp,cose,sinp,sine)
!****6***0*********0*********0*********0*********0*********0**********72
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

! Define cosine at edges..

      do 90 j=2,jm
90    cose(j) = 0.5 * (cosp(j-1) + cosp(j))
      cose(1) = cose(2)

      sinp( 1) = -1.
      sinp(jm) =  1.

      do 100 j=2,jm1
100   sinp(j) = 0.5 * (sine(j) + sine(j+1))

      if(echo) write(6,*) 'SETRIG called. ',im,jm
      return
      end subroutine setrig

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine vpol5(u,v,im,jm,coslon,sinlon,cosl5,sinl5,jfirst,jlast)
!****6***0*********0*********0*********0*********0*********0**********72
      implicit none

! WS 99.05.05 :  Restructured to split up SP and NP

      integer im, jm, jfirst, jlast
      real u(im,jfirst:jlast),v(im,jfirst:jlast),coslon(im), &
           sinlon(im),cosl5(im),sinl5(im)

! Local:


      integer i, imh
      real uanp(im), uasp(im), vanp(im), vasp(im)
      real un, vn, us, vs, r2im

! WS 99.05.25 :  Replaced conversions of IMR with IM
      r2im = 0.5d0/dble(im)
      imh  = im / 2

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN

!
! Treat SP
!
      do i=1,im
      uasp(i) = u(i,  2) + u(i,  3)
      enddo

      do i=1,im-1
      vasp(i) = v(i,  2) + v(i+1,  2)
      enddo
      vasp(im) = v(im,  2) + v(1,  2)

! Projection at SP

      us = 0.
      vs = 0.

      do i=1,imh
      us = us + (uasp(i+imh)-uasp(i))*sinlon(i) &
              + (vasp(i)-vasp(i+imh))*coslon(i)
      vs = vs + (uasp(i+imh)-uasp(i))*coslon(i) &
              + (vasp(i+imh)-vasp(i))*sinlon(i)
      enddo

      us = us*r2im
      vs = vs*r2im

! get V-wind at SP

      do i=1,imh
      v(i,  1) =  us*cosl5(i) - vs*sinl5(i)
      v(i+imh,  1) = -v(i,  1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN

!
! Treat NP
!
      do i=1,im
      uanp(i) = u(i,jm-1) + u(i,jm)
      enddo

      do i=1,im-1
      vanp(i) = v(i,jm-1) + v(i+1,jm-1)
      enddo
      vanp(im) = v(im,jm-1) + v(1,jm-1)

! Projection at NP

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (uanp(i+imh)-uanp(i))*sinlon(i) &
              + (vanp(i+imh)-vanp(i))*coslon(i)
      vn = vn + (uanp(i)-uanp(i+imh))*coslon(i) &
              + (vanp(i+imh)-vanp(i))*sinlon(i)
      enddo
      un = un*r2im
      vn = vn*r2im

! get V-wind at NP

      do i=1,imh
      v(i,jm) = -un*cosl5(i) - vn*sinl5(i)
      v(i+imh,jm) = -v(i,jm)
      enddo

      ENDIF

      return
      end subroutine vpol5

      subroutine get_lwi_ ( lwi, im, jm, rc, lwifile )
! This is a temporary routine until I have an interpolation routine that
! can interpolate lwi properly by voting.; Todling, 24Sep2004
      use m_ioutil, only : luavail
      implicit none
      integer, intent(in)  :: im, jm
      integer, intent(out) :: rc
      character(len=*),intent(in),optional :: lwifile
      real,    intent(out) :: lwi(im,jm)
      integer  lu
      logical  fexist
      character(len=255) :: fname
      rc = 0
      if(present(lwifile))then
         fname = trim(lwifile)
      else
         fname = 'p_rst'
      endif
      inquire(file=trim(fname),exist=fexist)
      if(.not.fexist)then
         print *, 'get_lwi: cannot find LWI file'
         rc = 1  ! cannot find lwi file
         return
      endif
      lu = luavail()
      open(lu,file=trim(fname),form='unformatted',access='sequential')
      read(lu,err=99) lwi
      close(lu)
      return
99    rc = 2 ! problems reading field
      print *, 'get_lwi: problems reading LWI field '
      end subroutine get_lwi_

      subroutine simple_interp_ ( wi, wo, pertype, rc )
      implicit none
!
! !REVISION HISTORY:
!
!   12Jan2008  Todling Wrote this quickly since I don't think the code above works
!   07Oct2010  Todling/Errico  Patched temporarily to avoid error from adjoint of bilinear interpolation 
! 
! TO DO: replace both forward interpolation with SJ's area-preserving interp; 
!        use adjoint of SJ's area-preserving interp - to be developed.
! 
      character(len=*) :: pertype  ! defines type of perturbation being interpolated (tl/ad)
      type(dyn_vect) :: wi
      type(dyn_vect) :: wo
      real, allocatable :: fld3di(:,:,:), fld3do(:,:,:), dum(:,:,:)
      real :: ratlat, ratlon, delw
      integer, intent(out) :: rc
      integer :: L
      integer :: imi,jmi,kmi,lmi
      integer :: imo,jmo,kmo,lmo
      character(len=255) :: pertype_
      imi = wi%grid%im
      jmi = wi%grid%jm
      kmi = wi%grid%km
      lmi = wi%grid%lm
      imo = wo%grid%im
      jmo = wo%grid%jm
      kmo = wo%grid%km
      lmo = wo%grid%lm
      ratlat = float(jmi)/jmo
      ratlon = float(imi)/imo
      delw   = ratlat*ratlon
      pertype_ = lowercase(trim(pertype))
      rc=0
      if ( kmo/=kmi ) then
           rc = 97
           print *, 'simple_interp_: interp failed, inconsistent number of levels'
           return
      endif
      if ( lmo/=lmi ) then
           rc = 97
           print *, 'simple_interp_: interp failed, inconsistent number of tracers'
           return
      endif

      allocate ( dum(imo,jmo,1) )
      allocate ( fld3di(imi,jmi,1), fld3do(imo,jmo,1) )

      fld3di(:,:,1) = wi%phis; fld3do =0.d0
      call interpack_terpv ( imi,jmi,1,fld3di,  imo,jmo,1,fld3do, rc, lon0=wi%grid%lon(1) )
      wo%phis = fld3do(:,:,1)

      fld3di(:,:,1) = wi%ps; fld3do =0.d0
      if ( pertype_ == 'gradient' .or. pertype_ == 'tlm' ) then
           call interpack_terpv ( imi,jmi,1,fld3di,  imo,jmo,1,fld3do, rc, lon0=wi%grid%lon(1) )
      else if ( pertype_ == 'adm' ) then
!          if (imo>=imi.and.jmo>=jmi) then  ! dirty fix for now
               call interpack_terpv    ( imi,jmi,1,     fld3di,  imo,jmo,1,fld3do,  rc, lon0=wi%grid%lon(1) )
               fld3do = delw * fld3do
!          else
!              dum=0.d0
!              call interpack_terpv_ad ( imo,jmo,1,dum, fld3do,  imi,jmi,1,fld3di,  rc, lon0=wi%grid%lon(1) )
!          endif
      else
           rc = 98
           print *, 'simple_interp_: interp failed, unknown choice for pertype', trim(pertype)
           return
      endif
      wo%ps = fld3do(:,:,1)

      deallocate ( fld3di, fld3do, dum )
      allocate ( dum(imo,jmo,kmo) )

!     zero out fields applicable to perturbation structure
!     ----------------------------------------------------
      wo%ts  = 0.0
      wo%lwi = 0.0

      if ( pertype_ == 'gradient' .or. pertype_ == 'tlm' ) then

           call interpack_terpv ( imi,jmi,kmi,wi%u,     imo,jmo,kmo,wo%u,    rc, lon0=wi%grid%lon(1) )
           call interpack_terpv ( imi,jmi,kmi,wi%v,     imo,jmo,kmo,wo%v,    rc, lon0=wi%grid%lon(1) )
           call interpack_terpv ( imi,jmi,kmi,wi%pt,    imo,jmo,kmo,wo%pt,   rc, lon0=wi%grid%lon(1) )
           call interpack_terpv ( imi,jmi,kmi,wi%delp,  imo,jmo,kmo,wo%delp, rc, lon0=wi%grid%lon(1) )
           do L=1,lmo
               call interpack_terpv ( imi,jmi,kmi,wi%q(:,:,:,L), imo,jmo,kmo,wo%q(:,:,:,L), rc, lon0=wi%grid%lon(1) )
           enddo
           write(6,'(2(a,2i5),2x,a,f7.2)') 'simple_interp_: complete (TL) interpolation from ', imi,jmi, ' to ', imo, jmo, 'lon0 =', wi%grid%lon(1)
 
      else if ( pertype_ == 'adm' ) then

!          if (imo>=imi.and.jmo>=jmi) then  ! dirty fix for now
               dum = 0.d0; wo%u=0.d0
               call interpack_terpv    ( imi,jmi,kmi,wi%u,          imo,jmo,kmo,wo%u,    rc, lon0=wi%grid%lon(1) )
               dum = 0.d0; wo%v=0.d0
               call interpack_terpv    ( imi,jmi,kmi,wi%v,          imo,jmo,kmo,wo%v,    rc, lon0=wi%grid%lon(1) )
               dum = 0.d0; wo%pt=0.d0
               call interpack_terpv    ( imi,jmi,kmi,wi%pt,         imo,jmo,kmo,wo%pt,   rc, lon0=wi%grid%lon(1) )
               dum = 0.d0; wo%delp=0.d0
               call interpack_terpv    ( imi,jmi,kmi,wi%delp,       imo,jmo,kmo,wo%delp, rc, lon0=wi%grid%lon(1) )
               wo%q = 0.d0
               do L=1,lmo
                   dum = 0.d0
                   call interpack_terpv    ( imi,jmi,kmi,wi%q(:,:,:,L),       imo,jmo,kmo,wo%q(:,:,:,L), rc, lon0=wi%grid%lon(1) )
               enddo

               wo%u   = delw * wo%u
               wo%v   = delw * wo%v
               wo%pt  = delw * wo%pt
               wo%delp= delw * wo%delp
               wo%q   = delw * wo%q

               write(6,'(2(a,2i5),2x,a,f7.2)') 'simple_interp_: complete (pseudo-AD) interpolation from ', imi,jmi, ' to ', imo, jmo, 'lon0 =', wi%grid%lon(1)
!          else
!              dum = 0.d0; wo%u=0.d0
!              call interpack_terpv_ad ( imo,jmo,kmo,dum, wo%u,     imi,jmi,kmi,wi%u,    rc, lon0=wi%grid%lon(1) )
!              dum = 0.d0; wo%v=0.d0
!              call interpack_terpv_ad ( imo,jmo,kmo,dum, wo%v,     imi,jmi,kmi,wi%v,    rc, lon0=wi%grid%lon(1) )
!              dum = 0.d0; wo%pt=0.d0
!              call interpack_terpv_ad ( imo,jmo,kmo,dum, wo%pt,    imi,jmi,kmi,wi%pt,   rc, lon0=wi%grid%lon(1) )
!              dum = 0.d0; wo%delp=0.d0
!              call interpack_terpv_ad ( imo,jmo,kmo,dum, wo%delp,  imi,jmi,kmi,wi%delp, rc, lon0=wi%grid%lon(1) )
!              wo%q = 0.d0
!              do L=1,lmo
!                  dum = 0.d0
!                  call interpack_terpv_ad ( imo,jmo,kmo,dum, wo%q(:,:,:,L),  imi,jmi,kmi,wi%q(:,:,:,L), rc, lon0=wi%grid%lon(1) )
!              enddo
!              write(6,'(2(a,2i5),2x,a,f7.2)') 'simple_interp_: complete (AD) interpolation from ', imi,jmi, ' to ', imo, jmo, 'lon0 =', wi%grid%lon(1)
!          endif

      else
           rc = 99
           print *, 'simple_interp_: interp failed, unknown choice for pertype', trim(pertype)
           return
      endif

      deallocate ( dum )
      end subroutine simple_interp_

      end module m_maph_pert
