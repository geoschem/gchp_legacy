      module m_dyn_util

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_dyn_util: utilities for dyn-vect
!
! !USES:
!
      use m_dyn

      implicit NONE
      private

! !DESCRIPTION: Collection of utilities related to dyn-vect
!
! !REVISION HISTORY:
!
!  30Aug2017   Todling    Initial wrap from collected utils
!
!-------------------------------------------------------------------------
!EOP

   public :: dyn_util_tv2t
   public :: Dyn_Scale_by_TotEne

   interface Dyn_Util_Tv2T
      module procedure tv2t_
   end interface
   interface Dyn_Scale_by_TotEne
      module procedure scale_by_totene_
   end interface
   interface Dyn_TotEne_Weights
      module procedure TotEne_Weights_
   end interface
   interface Dyn_TotEne_Dotp
      module procedure dotp_
   end interface

CONTAINS

!.................................................................
      subroutine tv2t_ (t,q)
      use m_const, only : zvir
      implicit none
      real, intent(inout) :: t(:,:,:)
      real, intent(in)    :: q(:,:,:)
      integer :: i,j,k
      integer :: im,jm,km
      im=size(t,1)
      jm=size(t,2)
      km=size(t,3)
      do k=1,km
        do j=1,jm
          do i=1,im
            t(i,j,k)=t(i,j,k)/(1.d0+zvir*q(i,j,k))
          enddo
        enddo
      enddo
      end subroutine tv2t_ 
!.................................................................
      subroutine scale_by_totene_(x,eps_eer,anorm,jnorm,projlon,projlat,projlev,&
                                  nymd,nhms,ntype,optene,normlz,ps,delp)
      use m_const, only: cp=>cpm
      use m_const, only: rd=>rgas
      use m_const, only: tref=>tstd
      use m_const, only: pstd
      use m_const, only: alhl

!
! This code has been checked and shown to agree (very closely) with the
! energy result from initadj. Note that initadj embeds the following 
! definition of J and dJ/dx:
!           J = e^T E e
!       dJ/dx = 2 E e
! with E = 1/2 sum(energy_density) dA dp
!
      implicit none

      type(dyn_vect),intent(inout) :: x
      real, intent(in) :: eps_eer
      character(len=*), intent(in) :: anorm
      character(len=*), intent(in) :: jnorm
      real,    intent(in) :: projlat(2), projlon(2)
      integer, intent(in) :: projlev(2)
      integer, intent(in) :: nymd, nhms
      character(len=*), intent(in), optional :: ntype ! norm type: L2 or Ene (default)
      integer, intent(in), optional :: optene ! -2=E^-1; -1=E^-1/2; 1=E^1/2; 2=E
      logical, intent(in), optional :: normlz ! vector will come out normalized to 1
      real, intent(in), optional :: ps(:,:)
      real, intent(in), optional :: delp(:,:,:)

      real, allocatable :: w2d(:,:)
      real, allocatable :: w3d(:,:,:)

      real, parameter :: pref=100.*pstd
      real fac,fact,ufac,tfac,qfac,pfac
      real dot(6)
      integer :: i,j,k,npt
      integer :: optene_
      logical :: normlz_
 
      normlz_ = .false.
      if(present(normlz)) then
        normlz_ = normlz
      endif
      optene_ = 1
      if(present(optene)) then
        optene_ = optene
      endif
      allocate(w2d(x%grid%im,x%grid%jm))
      allocate(w3d(x%grid%im,x%grid%jm,x%grid%km))

      call TotEne_Weights_(x,projlon,projlat,projlev,w2d,w3d,optene=optene,ps=ps,delp=delp)

      print *, 'norm used: ', trim(anorm)
      print *, 'ene-scale: eps_eer: ', eps_eer

      if ( trim(ntype) == 'L2' .or. trim(ntype) == 'l2' ) then
         pfac = 1.0
         ufac = 1.0
         tfac = 1.0
         qfac = 1.0
      else
         fact=1.0/sqrt(2.) ! 1/2 factor in front of energy definition
         pfac = fact*sqrt(rd*tref)/pref
         ufac = fact
         tfac = fact*sqrt(cp/tref)
         qfac = fact*alhl*sqrt(eps_eer/(cp*tref))
      endif

      if(optene_==-1) then
        pfac = 1.0/pfac
        ufac = 1.0/ufac
        tfac = 1.0/tfac
        if(eps_eer>1.e-10) then
           qfac = 1.0/qfac
        else
           qfac = 0.0
        endif
      endif
      if(optene_==-2) then
        pfac = 1.0/(pfac*pfac)
        ufac = 1.0/(ufac*ufac)
        tfac = 1.0/(tfac*tfac)
        if(eps_eer>1.e-10) then
           qfac = 1.0/(qfac*qfac)
        else
           qfac = 0.0
        endif
      endif
      if(optene_==2) then
        pfac = pfac*pfac
        ufac = ufac*ufac
        tfac = tfac*tfac
        qfac = qfac*qfac
      endif

!     LPO: to be done properly ...
      do k=1,x%grid%km
         x%u (:,:,k)   = x%u (:,:,k)   * ufac * w3d(:,:,k)
         x%v (:,:,k)   = x%v (:,:,k)   * ufac * w3d(:,:,k)
         x%pt(:,:,k)   = x%pt(:,:,k)   * tfac * w3d(:,:,k)
      enddo
      if (trim(anorm)=='twe') then
         do k=1,x%grid%km
            x%q (:,:,k,1) = x%q (:,:,k,1) * qfac * w3d(:,:,k)
         enddo
      else
         x%q (:,:,:,1) = 0.0
      endif
      x%q(:,:,:,2:) = 0.0
      x%ts = 0.0
      x%delp = 0.0
      do k=1,x%grid%km
         x%delp(:,:,k) = x%ps * sqrt(pfac) * w3d(:,:,k)! * pfac
      enddo
      x%ps = x%ps * sqrt(pfac) * w2d
      if (optene_>0.or.normlz_) then
         call dotp_(x,dot,anorm,jnorm,nymd,nhms)
      endif
      if ( normlz_ ) then
         fac = 1.0/sqrt(dot(1))
         x%u = fac * x%u
         x%v = fac * x%v
         x%pt= fac * x%pt
         x%q = fac * x%q
         x%ps= fac * x%ps
         x%delp = fac * x%delp
         ! univariate normalization ...
!        fac = 1.0
!        x%u = fac * x%u /sqrt(dot(2))
!        x%v = fac * x%v /sqrt(dot(3))
!        x%pt= fac * x%pt/sqrt(dot(4))
!        x%ps= fac * x%ps/sqrt(dot(5))
!        x%q = fac * x%q /sqrt(dot(6))
!        x%delp = fac * x%delp /sqrt(dot(5))
         call dotp_(x,dot,anorm,jnorm,nymd,nhms)
      endif

      deallocate(w3d)
      deallocate(w2d)
      return
      end subroutine scale_by_totene_

!.................................................................

      subroutine TotEne_Weights_(x,projlon,projlat,projlev,w2d,w3d,optene,ps,delp)
      use m_const, only: pstd
      implicit none
      type(dyn_vect) x
      real,    intent(in) :: projlat(2), projlon(2)
      integer, intent(in) :: projlev(2)
      integer, intent(in), optional :: optene
      real,intent(in),optional :: ps(:,:)
      real,intent(in),optional :: delp(:,:,:)
      real :: w2d(:,:), w3d(:,:,:)
      
      real, parameter :: pref=100.*pstd
      real, allocatable :: ple (:,:,:)
      real, allocatable :: dsig(:,:,:)
      real, allocatable :: hlpo(:,:)
      real, allocatable :: vlpo(:)
      real, allocatable :: rlat(:),rlon(:)
      real, allocatable :: jweights(:,:),glats(:,:)
      real pi,crlat,sumcl
      real clat,clon
      real rlon1,rlon2,rlat1,rlat2
      real rlon0
      integer im,jm,km
      integer i,j,k
      integer optene_

      optene_=1
      if(present(optene)) then
         optene_=optene
      endif
      im=x%grid%im
      jm=x%grid%jm
      km=x%grid%km
      pi=4.0*atan(1.0)
      if (x%grid%lon(1)<0.0) then
         rlon1 = pi * (projlon(1)-180.)/180.
         rlon2 = pi * (projlon(2)-180.)/180.
      else
         rlon1 = pi * projlon(1)/180.
         rlon2 = pi * projlon(2)/180.
      endif
      rlat1 = pi * projlat(1)/180.
      rlat2 = pi * projlat(2)/180.

      allocate(hlpo(im,jm))
      allocate(vlpo(km))
      allocate(rlon(im),rlat(jm))

      vlpo=0.0
      do k=1,km
         if(k >= projlev(1) .and. k <= min(km,projlev(2)) ) vlpo(k) = 1.0
      enddo

      rlon0 = pi*x%grid%lon(1)/180.
      rlat  = pi*x%grid%lat/180.
      rlon  = pi*x%grid%lon/180.
      hlpo=0.0
      if (rlon1 < rlon2) then
         do j = 1, jm
            clat = rlat(j)
            if ( clat >= rlat1 .and. clat <= rlat2 ) then
              do i = 1, im
                 if ( rlon(i) >= rlon1 .and. rlon(i) <= rlon2 ) hlpo(i,j) = 1.0
              end do
            end if
         end do
      else
         do j = 1, jm
            clat = rlat(j)
            if ( clat >= rlat1 .and. clat <= rlat2 ) then
              do i = 1, im
                 if ( rlon(i) >= rlon1 .or. rlon(i) <= rlon2 ) hlpo(i,j) = 1.0
              end do
            end if
         end do
      endif ! (rlon1 < rlon2 for t)

      allocate (jweights(jm,2),glats(jm,2))
      call horiz_grid_ (jm, jweights, glats)
!     2d weights
      do j=1,jm
         crlat=cos(pi*x%grid%lat(j)/180.)
         do i=1,im
           w2d(i,j) = jweights(j,2)
         enddo
      enddo
      deallocate (jweights,glats)
      sumcl = 1.0 / im ! this is so it match Ron''s weights more closely
      if (optene_==2) then
         do j=1,jm
            w2d(:,j)=hlpo(:,j)*(w2d(:,j)*sumcl)
         enddo
      endif
      if (optene_==-2) then
         do j=1,jm
            w2d(:,j)=hlpo(:,j)/(w2d(:,j)*sumcl)
         enddo
      endif
      if (optene_==-1) then
         do j=1,jm
            w2d(:,j)=hlpo(:,j)/sqrt(w2d(:,j)*sumcl)
         enddo
      endif
      if (optene_==1) then
         w2d = sqrt(w2d*sumcl)
         w2d = w2d*hlpo
      endif

!     3d weights
      allocate(ple (im,jm,km+1))
      allocate(dsig(im,jm,km  ))
      if (present(ps)) then
         if ( present(delp) ) then
            print *, 'using ref delp and ps to define dsig'
            do k=1,km
               dsig(:,:,k)=delp(:,:,k)/ps(:,:)
            enddo
         else
            do k=1,km+1
               ple(:,:,k)=x%grid%ak(k)+x%grid%bk(k)*ps     !state dependent as in initadj
            enddo
            do k=1,km
               dsig(:,:,k)=(ple(:,:,k+1)-ple(:,:,k))/ps
            enddo
         endif
      else
         do k=1,km+1
            ple(:,:,k)=x%grid%ak(k)+x%grid%bk(k)*pref      ! set to ref pressure
         enddo
         do k=1,km
            dsig(:,:,k)=(ple(:,:,k+1)-ple(:,:,k))/pref
         enddo
      endif
      if (optene_==1) then
         dsig=sqrt(dsig)
      endif
      if (optene_==-1) then
         dsig=1.0/sqrt(dsig)
      endif
      if (optene_==-2) then
         dsig=1.0/dsig
      endif
      if (optene_==2) then
         dsig=dsig
      endif
      do k=1,km
         w3d(:,:,k)=vlpo(k)*dsig(:,:,k)*w2d
      enddo

      deallocate(rlon,rlat)
      deallocate(vlpo)
      deallocate(hlpo)
      deallocate(dsig)
      deallocate(ple) 
      end subroutine TotEne_Weights_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: horiz_grid_ --- Determine some D-grid information required
!
!
! !INTERFACE:
!
      subroutine horiz_grid_ (jn1, jweights, glats)

!USES:

      implicit none

! !INPUT PARAMETERS:

      integer,  intent(in)  :: jn1    

! !OUTPUT PARAMETERS:

      real, intent(out) :: jweights(jn1,2) ! area weights (1) u, (2) T   
      real, intent(out) :: glats(jn1,2)    ! degrees lats (1) u, (2) T   

! !INPUT/OUTPUT PARAMETERS:
 
! !DESCRIPTION:
!
!  Determine some D-grid information required
!
!  i=1 on v field corresponds to long=0
!  i=1 on T,u fields corresponds to long=0+360/(2*im1)
!  v defined on same lats as T, excluding poles
!  u defined between T lats, but on T lons
!

! !SEE ALSO:
!
  
! !REVISION HISTORY:
!
!  04Jun2003  R. Errico  Initial algorithm
!
!EOP
!-------------------------------------------------------------------------
!
!  local variables

      integer   :: i,j
      real  :: slats(jn1,2)  ! sines of latitude for (1)u and (2)T
      real  :: pi, pi180
      real  :: rlat, rlat_half
      real  :: tlat, ulat
      
      pi=4.d0*datan(1.d0)
      pi180=180.d0/pi
      rlat=pi/dble(jn1-1)
      rlat_half=0.5d0*rlat

      tlat=-0.5d0*pi   ! latitude of south pole
      glats(1,1)=pi180*rlat  ! a special value since otherwise not used
      glats(1,2)=pi180*tlat  
      slats(1,1)=0.d0  ! a value not used
      slats(1,2)=-1.d0
      do j=2,jn1
        ulat=tlat+rlat_half
        tlat=tlat+rlat
        glats(j,1)=pi180*ulat
        glats(j,2)=pi180*tlat
        slats(j,1)=dsin(ulat)
        slats(j,2)=dsin(tlat)
      enddo
!
       jweights(1,1)=0.d0  ! not used
       jweights(1,2)=0.5d0*(1.d0+slats(2,1))
       do j=2,jn1-1
         jweights(j,1)=0.5d0*(slats(j,2)-slats(j-1,2))
         jweights(j,2)=0.5d0*(slats(j+1,1)-slats(j,1))
       enddo
       jweights(jn1,1)=0.5d0*(slats(jn1,2)-slats(jn1-1,2))
       jweights(jn1,2)=0.5d0*(1.d0-slats(jn1,1))
    
       end subroutine horiz_grid_

!.................................................................

      subroutine dotp_(x,dot,anorm,jnorm,nymd,nhms)
      use m_ioutil, only : luavail
      implicit none
      character(len=*), intent(in) :: anorm, jnorm
      integer nymd, nhms
      real dot(:)
      type(dyn_vect) x
      integer lu
      dot(2) = sum(x%u *x%u )
      dot(3) = sum(x%v *x%v )
      dot(4) = sum(x%pt*x%pt)
      dot(5) = sum(x%ps*x%ps)
      dot(6) = sum(x%q(:,:,:,1:1)*x%q(:,:,:,1:1))
      dot(1) = sum(dot(2:6))
      write(*,'(a)') 'sum, u, v, t, ps, q'
      write(*,'(6(1x,f15.12))') dot
      if (trim(jnorm) /= "NULL" ) then
         lu = luavail()
         open  (lu, file=trim(jnorm), form='formatted')
         write (lu, '(a,i8.8,a,i6.6,2a)') 'Date: ', nymd, ' Time: ', nhms, ' Norm type: ', trim(anorm)
         write (lu, '(a)') 'sum, u, v, t, ps, q'
         write (lu, '(6f20.12)') dot
         close (lu)
      endif
      end subroutine dotp_

!.................................................................

  end module m_dyn_util
