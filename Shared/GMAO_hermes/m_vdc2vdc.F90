!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !MODULE: m_vdc2vdc --- Map vertical diff. coefficients between resolutions
!                     
!
! !INTERFACE:
!
      module m_vdc2vdc

!USES:

      use m_realkinds, only : r8 => kind_r8
      
      use m_const, only : Cp     => cpm
      use m_const, only : R      => rgas
      use m_const, only : kappa
      use m_const, only : zvir
      
      use m_GFIO_PutFld, only : GFIO_PutFld
      use m_GFIO_GetFld, only : GFIO_GetFld
      
      use m_StrTemplate        ! grads style templates
      use m_inpak90
      use m_stdio, only : stdout

      implicit NONE
 
! !PUBLIC MEMBER FUNCTIONS:
 
      PRIVATE
      PUBLIC vdc2vdc        ! general interpolation routine
      PUBLIC vdc2vdc_init   ! stores coef arrays after they've been gathered by gcm
      PUBLIC vdc2vdc_null   ! nullifies pointers
      PUBLIC vdc2vdc_clean  ! clean up (dealloc) coef array

      PUBLIC nvars, icah, icam, icch, iccm
      PUBLIC def_RCvdc

! !PUBLICH TYPES:
      
      interface vdc2vdc_init ; module procedure init_   ; end interface
      interface vdc2vdc      ; module procedure vdc2vdc_; end interface
      interface vdc2vdc_null ; module procedure null_   ; end interface
      interface vdc2vdc_clean; module procedure clean_  ; end interface


! !DESCRIPTION: This module performs general interpolation of dyn vector.
!
! !SEE ALSO: dyn2dyn, m_mapz, m_maph
!
! !REMARKS: 
!    The interpolation of coeffs can be invoked in two ways: off-line when using
!    the stand-alone program dyn2dyn.x; on-line when running the GCM and calling
!    m_dynout. In this latter, the array coef functions essentially as a common 
!    block; the clean way of exercizing the functionality of this module avoiding
!    the "common block" approach would be to pass a pointer around in the GCM.
!    To avoid deep intrusion in the GCM the present way was chosen.
!
! !REVISION HISTORY:
!
!  27Sep2004  Todling    Initial modularization.
!  06Oct2006  Todling    Tested and solved issue w/ coef array. 
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'm_vdc2vdc'

      character(len=*), parameter :: def_RCvdc = 'vdc2vdc.rc'   ! rc file that trigers action
                                                                !   within this module

      integer, parameter :: nvars=4       ! number of diffusion coefs
      integer, parameter :: icah =1       ! index of cah in vcoef in file   
      integer, parameter :: icam =2       ! index of cam in vcoef in file
      integer, parameter :: icch =3       ! index of cch in vcoef in file
      integer, parameter :: iccm =4       ! index of ccm in vcoef in file
      character(len=3), parameter :: names(nvars)=(/'cah','cam','cch','ccm'/)

      logical, parameter :: untag = .true.
      character(len=8), parameter :: units(nvars)=(/  &
                                           'T       ',&
                                           'kgm/s   ',&
                                           'T       ',&
                                           'kgm/s   '/) 
      character(len=8), parameter :: titles(nvars)=(/ &
                                           'cah     ',&
                                           'cam     ',&
                                           'cch     ',&
                                           'ccm     '/)
                                                                                                
      character(len=*), parameter :: title  = 'vertical diffusion parameters'
      character(len=*), parameter :: source = 'data@gmao.gsfc.nasa.gov'

      integer, parameter :: prec_def = 0        ! 0=32bits precision of traj file
      logical, parameter :: verb_def = .false.  ! run in silence
      logical, parameter :: debug    = .false.  ! internal purpose printout (only for debugging)

      logical, save :: storeptrj                ! specifies whether v.d.c.'s are in mem or not
      integer, save :: ptrjfrq                  ! frequency of available input


                                                ! The following array unfortunately works as a common block
     real(r8), save, allocatable, target, dimension(:,:,:,:) :: coef  ! upper and lower diagonal elements 
                                                                      !   of matrix used in vdiff@fvgcm 

      CONTAINS

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: vdc2vdc --- Converts vertical diff. coeffs. between resolutions
!
! !INTERFACE:

     subroutine vdc2vdc_ ( im, jm, km, ptop_old, pt_old, delp_old, &
                           in, jn, kn, ptop_new, pt_new, delp_new, &
			                 ks_new, ak_new,   bk_new, &
			   nymd, nhms, rc, verbose, prec, expid, RCfile )

! !INPUT PARAMETERS:

      integer,  intent(in) :: im, jm, km            ! original dimension
      integer,  intent(in) :: in, jn, kn            ! target dimension
      integer,  intent(in) :: ks_new
      
      integer,  intent(in) :: nymd, nhms            ! date/time
      real(r8), intent(in) :: ptop_old              ! original pressure at top
      real(r8), intent(in) ::   pt_old(im,jm,km)    ! virt. pot. temp. on old grid
      real(r8), intent(in) :: delp_old(im,jm,km)    ! interface p-thicknesses on old grid 

      real(r8), intent(in) ::   ak_new(km+1)
      real(r8), intent(in) ::   bk_new(km+1)
      real(r8), intent(in) :: ptop_new              ! target pressure at top
      real(r8), intent(in) ::   pt_new(in,jn,kn)    ! virt. pot. temp. on new grid
      real(r8), intent(in) :: delp_new(in,jn,kn)    ! interface p-thicknesses on new grid

      integer,          intent(in), optional :: prec    ! precision of traj file
      logical,          intent(in), optional :: verbose ! controls print out of info
      character(len=*), intent(in), optional :: expid   ! experiment name      
      character(len=*), intent(in), optional :: RCfile  ! Resource filename 
      
! !OUTPUT PARAMETERS:

      integer, intent(out) :: rc                    ! returned error code
                                                    !   rc=1 unable to setup
                                                    !   rc=2 unable to alloc()
                                                    !   rc=3 unable to read in old coefs
                                                    !   rc=4 unable to point to old coefs
                                                    !   rc=5 unable to release memory
      
! !DESCRIPTION: This module performs general interpolation of dyn vector.
!
! !REVISION HISTORY:
!
!  29Sep2004  Todling    Initial code.
!  04Feb2005  Gelaro/RT  Logic to skip mapping if no interpolation
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'::vdc2vdc_'
      
      real(r8), pointer     :: coef_old(:,:,:,:)     ! matrix elements on old grid
      real(r8), allocatable :: coef_new(:,:,:,:)     ! matrix elements on old grid

      character(len=255) :: fname_old, fname_new
      logical :: verb, valid
      integer :: ier
 
      rc = 0
     

      if(present(verbose)) then
         verb = verbose
      else
         verb = verb_def
      endif


! Setup interpolation-related resource parameters
! ------------------------------------------------
      call set_ ( nymd, nhms, fname_old, fname_new, ier, expid=expid, RCfile=RCfile )              
          if (ier/=0) then
	      rc = 1
	      return
	  endif

! Read in old vertical diffusion coefficients
! ------------------------------------------- 
      if ( .not. storeptrj ) then

          allocate ( coef_old(im,jm,km,4),stat=ier )
             if (ier/=0) then
                 print *, trim(myname_), ': error Alloc(coef_old), ier=', ier
                 rc = 2
                 return
             endif	  
          call get_ ( fname_old, nymd, nhms, im, jm, km, coef_old, ier )
              if (ier/=0) then
	          rc = 3
	          return
	      endif

      else

          valid = size(coef,dim=1)==im .and. size(coef,dim=2)==jm .and. &
                  size(coef,dim=3)==km .and. size(coef,dim=4)==nvars
          if (valid) then
              coef_old => coef
          else
              rc = 4 
              return
          endif

      endif
	  
! Grab memory
! -----------
       allocate ( coef_new(in,jn,kn,nvars),stat=ier )
           if (ier/=0) then
	      print *, trim(myname_), ': error Alloc(coef_new), ier=', ier
	      rc = 2
	      return
	  endif

! Interpolate vertical diffusion coefficients
! -------------------------------------------
      if ( im==in  .and. jm==jn  .and.  km==kn ) then
         coef_new = coef_old
      else
         call map_ ( im, jm, km, ptop_old, pt_old, delp_old, coef_old, &
                     in, jn, kn, ptop_new, pt_new, delp_new, coef_new, verb )
      endif

! Release some memory
! -------------------
      if ( .not. storeptrj ) then

        deallocate ( coef_old,stat=ier )
           if (ier/=0) then
	      print *, trim(myname_), ': error Dealloc(coef_old), ier=', ier
	      rc = 5
	      return
   	   endif

      else

         nullify(coef_old)

      endif
	 
! Write out old vertical diffusion coefficients
! ---------------------------------------------
      call put_ ( fname_new, nymd, nhms,                                  &
                  in, jn, kn, coef_new, ptop_new, ks_new, ak_new, bk_new, &		
	          ier, prec=prec )
 
! Release memory
! --------------
        deallocate ( coef_new,stat=ier )
           if (ier/=0) then
	      print *, trim(myname_), ': error Dealloc(coef_new), ier=', ier
	      rc = 5
	      return
	 endif
      
      end subroutine vdc2vdc_
      
!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: map_ --- Maps vertical diffision coeffs; internal interface
!
! !INTERFACE:

!How to specify grid dimensions
!Specify icah etc.
!need read/write routines

      subroutine map_ ( im, jm, km, ptop_old, pt_old, delp_old, coef_old, &
                        in, jn, kn, ptop_new, pt_new, delp_new, coef_new, verbose )
     
! !USES:
     
      implicit none

! !INPUT PARAMETERS:

      integer, intent(in) :: im, jm, km              ! original dimension
      integer, intent(in) :: in, jn, kn              ! target dimension
      logical, intent(in) :: verbose       ! controls print out of info


      real(r8), intent(in)  :: pt_new(  in,jn,kn)    ! virt. pot. temp. on new grid
      real(r8), intent(in)  :: delp_new(in,jn,kn)    ! interface p-thicknesses on new grid

      real(r8), intent(in) :: ptop_old              ! original pressure at top
      real(r8), intent(in) :: ptop_new              ! target  pressure at top
      real(r8), intent(in) :: pt_old  (im,jm,km)   ! virt. pot. temp. on old grid
      real(r8), intent(in) :: delp_old(im,jm,km)   ! interface p-thicknesses on old grid 

! !INPUT/OUTPUT PARAMETERS:

      real(r8), intent(inout)  :: coef_old(im,jm,km,nvars)! matrix elements on old grid


! !OUTPUT PARAMETERS:

      real(r8), intent(out) :: coef_new(in,jn,kn,nvars)  ! matrix elements on new grid

! !DESCRIPTION:  Program to map vertical diffusion matrix elements used by the 
!  TLM and ADM simplified physics codes from one grid to another.
!  This is done by 
!  (1) computing vertical diffusion coefficients K from the original 
!      matrix elements and fields. This is not an exact inverse of 
!      the original calculation since the fields used are not those
!      available as the saved trajectory.
!  (2) interpolating K horizontally using a bilinear formula
!  (3) interpolating K vertically using a linear formula and, where 
!      extrapolating above or below the original grid is required, 
!      assuming a zero gradient.
!  (4) computing new matrix elements from the new interpolated 
!      K and trajectory field.
!  
!  The array cam(:,klast) contains the effective coefficients for 
!      surface drag.  These coefficients are simply horizontally 
!      (bilinearly) interpolated, with no reference to the dynamic 
!      fields or change in vertical spacing.
!
!
!
!      * a1 
!     c2  * a2
!        c3  * a3
!    
!
!             c(N-1)    * a(N-1)
!                    c(N)      *  
!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none;
!                        turned into a subroutine
!
!EOP
!-------------------------------------------------------------------------

! surface drag coefficients stored in cam
!

      real(r8), allocatable  :: fac(:,:,:,:)

! half-transformed matrix elements defined for new horiz. but old vert. grid       

      real(r8)  :: coef_half(in,jn,km,nvars) 
      real(r8)  :: delp_half(in,jn,km) 

!  Remove field-dependent factor from matrix elements
     
      allocate (fac(im,jm,km,2))
      call field_factor_(im,jm,km,ptop_old,kappa,delp_old,pt_old,fac)

      coef_old(:,:,1:km-1,icam)=coef_old(:,:,1:km-1,icam) &
                                    /fac(:,:,1:km-1,1)
      coef_old(:,:,1:km-1,icah)=coef_old(:,:,1:km-1,icah) &
                                    /fac(:,:,1:km-1,1)
      coef_old(:,:,2:  km,iccm)=coef_old(:,:,2:  km,iccm) &
                                    /fac(:,:,2:  km,2)
      coef_old(:,:,2:  km,icch)=coef_old(:,:,2:  km,icch) &
                                    /fac(:,:,2:  km,2)
      deallocate (fac)

!  Interpolate diffusion coefficients

      call horiz_interp_(in,jn,im,jm,km,coef_old,delp_old,   &
                              coef_half,delp_half) 

      if(verbose) call printc_ (in,jn,km,nvars,1,coef_half,'half coefs 1',1)

      call vert_interp_(in,jn,kn,km,icah,icam,icch,iccm,     &
                              ptop_old,ptop_new,             &
                              delp_new,delp_half,coef_half,coef_new)

!  Replace field-dependent factor from matrix elements
     
      allocate (fac(in,jn,kn,2))
      call field_factor_(in,jn,kn,ptop_new,kappa,delp_new,pt_new,fac)

      coef_new(:,:,1:kn-1,icam)=coef_new(:,:,1:kn-1,icam) &
                                    *fac(:,:,1:kn-1,1)
      coef_new(:,:,1:kn-1,icah)=coef_new(:,:,1:kn-1,icah) &
                                    *fac(:,:,1:kn-1,1)
      coef_new(:,:,2:  kn,iccm)=coef_new(:,:,2:  kn,iccm) &
                                    *fac(:,:,2:  kn,2)
      coef_new(:,:,2:  kn,icch)=coef_new(:,:,2:  kn,icch) &
                                    *fac(:,:,2:  kn,2)
      deallocate (fac)

      end subroutine map_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: field_factor_ --- Determine field-dependent from diffusion coeffs
!
! !INTERFACE:

      Subroutine field_factor_(im,jm,km,ptop,kappa,delp,pt,fac)

! !USES:

      implicit none
            
! !INPUT PARAMETERS:

      integer,  intent(in)  :: im,jm,km        ! original grid dimensions
      real(r8), intent(in)  :: ptop            ! top pressure
      real(r8), intent(in)  :: kappa           ! R/Cp
      real(r8), intent(in)  :: delp(im,jm,km)  ! delta_p between interfaces
      real(r8), intent(in)  :: pt(im,jm,km)    ! virt pot. temp. on data levels

! !OUTPUT PARAMETERS:
      
      real(r8), intent(out) :: fac(im,jm,km,2) ! field-dependent factor
   
! !DESCRIPTION: Determine field-dependent factor that multiplies vertical 
!     eddy diffusion coefficients to form diffusion matrix elements.
!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none.
!
!EOP
!-------------------------------------------------------------------------

!
!      Derived From NCAR physics routine vdiff, using:
!
!      for k=1,plev-1:  
!          tmp2 = ztodt*gorsq*rpdeli(i,k)*(potbar(i,k+1)**2)
!          cah(i,k  ) = kvh(i,k+1)*tmp2*rpdel(i,k  )
!          cam(i,k  ) = kvm(i,k+1)*tmp2*rpdel(i,k  )
!          cch(i,k+1) = kvh(i,k+1)*tmp2*rpdel(i,k+1)
!          ccm(i,k+1) = kvm(i,k+1)*tmp2*rpdel(i,k+1)
!
!     where
!           rpdeli(k)= 1./(pmid(k+1)-pmid(k))
!           rpdel (k)= 1./(pint(k+1)-pint(k)) = 1./delp(k)
!     and for k=2,nl
!           potbar(k) = pint(k)/(0.5*(t(k) + t(k-1)))
!           pint is interface pressure, indexed 1,...,nl+1

! Local variables:
      integer  :: i,j,k
      real(r8) :: delpe(km)  ! same as delp; p-thickness between interfaces
      real(r8) :: delpd(km)  ! p-thickness between data levels
      real(r8) :: pe(km+1)   ! p at interfaces
      real(r8) :: pkd(km)    ! p**kappa at data levels
      real(r8) :: ptfac   
      real(r8) :: t(km)      ! temperature derived from pt
!
      do j=1,jm
        do i=1,im 
          delpe(1:km)=delp(i,j,1:km)
          call pkappa1_(km,delpe,ptop,kappa,delpd,pe,pkd)   
          t(1:km)=pt(i,j,1:km)*pkd(1:km)
          do k=1,km-1    
            ptfac=2.d0*pe(k+1)/(t(k)+t(k+1))      
            fac(i,j,k,1)=ptfac**2/(delpe(k)*delpd(k))     ! ca
            fac(i,j,k+1,2)=ptfac**2/(delpe(k+1)*delpd(k)) ! cc
          enddo
        enddo
      enddo

      end subroutine field_factor_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: pkappa1_ --- calculates p to the kappa and delp
!
! !INTERFACE:

      subroutine pkappa1_(nl,delpe,ptop,kappa,delpd,pe,pkd)   
 
! !USES:

      implicit none

! !INPUT PARAMETERS:
           
      integer,  intent(in)  :: nl         ! vertical dimension of grid
      real(r8), intent(in)  :: delpe(nl)  ! delta p between interfaces
      real(r8), intent(in)  :: ptop       ! grid top p
      real(r8), intent(in)  :: kappa      ! R/Cp
 
! !OUTPUT PARAMETERS:
 
      real(r8), intent(out) :: delpd(nl)  ! delta p between data levels
      real(r8), intent(out) :: pe(nl+1)   ! p at layer interfaces
      real(r8), intent(out) :: pkd(nl)    ! p at data levels

! !DESCRIPTION: calculates p to the kappa and delp.
!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none
!  03Dec2004  Kokron     Altix does not have alog; fix with if block
!  26jan2007  da Silva   Revised Kokron's fix: no need for ifdef here;
!                        log() is ALWAYS preferable to alog()
!
!EOP
!-------------------------------------------------------------------------
!
!  Local variables

      integer               :: k
      real(r8)              :: pd(nl)     ! p at data levels
      real(r8)              :: pke(nl+1)  ! p**kappa at interfaces
      real(r8)              :: peln(nl+1) ! ln(p)  at interfaces
     
!
!  pkd, delpd, pd are computed like pk, 1/rdelp1, pmid1 in NCAR physics 
!  "d" refers to data level; "e" is edge or interface value

      pe(1)=ptop
      pke(1)=pe(1)**kappa
      peln(1)=log(pe(1))
      do k=2,nl+1
        pe(k)=pe(k-1)+delpe(k-1)
        pke(k)=pe(k)**kappa
	peln(k)=log(pe(k))
      enddo
      do k=1,nl
        pd(k)=0.5*(pe(k+1)+pe(k))
        pkd(k)=(pke(k+1)-pke(k)) / (kappa*(peln(k+1)-peln(k)))
      enddo
      do k=1,nl-1
        delpd(k)=pd(k+1)-pd(k)
      enddo

      end subroutine pkappa1_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: horiz_interp_ --- horizontally interpolates vertical diff. coeffs.
!
! !INTERFACE:

     subroutine horiz_interp_(in,jn,im,jm,km,coef_old,delp_old,   &
                              coef_half,delp_half) 

! !USES:

     implicit none
     
! !INPUT PARAMETERS:

   integer,  intent(in) :: in ! number of E-W points of new grid
   integer,  intent(in) :: jn ! number of S-N points of new grid
   integer,  intent(in) :: im ! number of E-W points of old grid
   integer,  intent(in) :: jm ! number of S-N points of old grid
   integer,  intent(in) :: km ! number of vertical points of old
   real(r8), intent(in)  :: coef_old(im,jm,km,4)  ! coefs on old grid
   real(r8), intent(in)  :: delp_old(im,jm,km)  ! delp  on old grid

! !OUTPUT PARAMETERS:

   real(r8), intent(out) :: coef_half(in,jn,km,4) 
                                      ! coefs on new horizontal grid
                                      ! but still on old vertical grid 
   real(r8), intent(out) :: delp_half(in,jn,km) ! delp on new horizontal grid
                                      ! but still on old vertical grid 

! !DESCRIPTION: Perform horizontal interpolation using bilinear interpolation, 
! assuming that fields are on the T-component subgrid of the D grid.  (This 
! assumption allows a simple algorithm for determining the closest point 
! on the old grid that is to the SW of each point on the new grid.)
!
! The location of a S-N grid point with respect to the equator being 0, 
!    as a fraction of the pole to pole distance is
!                        y = (j-1)/(J-1) - 0.5 
!    where j is the point index and J is the number of S-N points.
!     
! The location of a E-W grid point, measured with respect to the prime 
!    meridian as a fraction of the distance around the latitude circle, 
!    and assuming the first point is displaced 1/2 grid distance to the E 
!    of the prime meridian, is 
!                        x = (i-0.5)/I
!    where i is the point index and I is the number of E-W points.
!     
! Perform the same interpolation for delp, to define a half-transfomed
! vertical coordinate for use in the second half (i.e., vertical) 
! transform.
!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none
!
!EOP
!-------------------------------------------------------------------------
!
!  Local variables:
   integer  :: k       ! vertical grid index
   integer  :: n       ! index for type of coefficient
   integer  :: i_new, j_new   ! coordinates of point on new grid
   integer  :: i_old, j_old   ! closest point to the SW on old grid
   integer  :: i_old_p1, j_old_p1   ! closest point to the NE on old grid
   real     :: iratio  ! ratio of E-W spacings on the 2 grids
   real     :: jratio  ! ratio on S-N spacings on the 2 grids
   real     :: dx1    ! longitudinal distance between new point and the
                      ! closest point to the SW on the old grid, measured
                      ! as a fraction of longitudinal grid spacing 
   real     :: dy1    ! latitudinal distance between new point and the
                      ! closest point to the SW on the old grid, measured
                      ! as a fraction of latitudinal grid spacing 
   real     :: dx2,dy2        ! complimentary fractional distances
   real     :: ri_old, rj_old ! location of new point within the old
                              ! coordinate system     


! This routine assumes that the fields are on the T component subgrid of 
! a D-grid


      iratio=real(im)/real(in)         ! for equally-spaced longitudes
      jratio=real(jm-1)/real(jn-1)     ! for T fields on D grid
!  
      do i_new=1,in
        ri_old = iratio*(i_new-0.5)+0.5
        i_old=int(ri_old)
        dx1=ri_old-i_old
        dx2=1.-dx1
        i_old_p1=i_old+1
        if (i_old == 0) then
           i_old=im
           i_old_p1=1
        endif
        if (i_old == im) then
           i_old_p1=1
        endif
!
        do j_new=1,jn
          rj_old = jratio*(j_new-1.)+1.
          j_old=int(rj_old)
          if (j_old <    1) j_old=1
          dy1=rj_old-j_old
          dy2=1.-dy1
          if (j_old > jm-1) then 
            j_old=jm-1      
            dy1=1.
            dy2=0.
          endif
          j_old_p1=j_old+1
!
          do k=1,km
            delp_half(i_new,j_new,k) =                   & 
                  dx2*dy2*delp_old(i_old   ,j_old,   k)  &
                + dx1*dy2*delp_old(i_old_p1,j_old   ,k)  &
                + dx2*dy1*delp_old(i_old   ,j_old_p1,k)  &
                + dx1*dy1*delp_old(i_old_p1,j_old_p1,k) 
            do n=1,4
              coef_half(i_new,j_new,k,n) =                 & 
                  dx2*dy2*coef_old(i_old   ,j_old,   k,n)  &
                + dx1*dy2*coef_old(i_old_p1,j_old   ,k,n)  &
                + dx2*dy1*coef_old(i_old   ,j_old_p1,k,n)  &
                + dx1*dy1*coef_old(i_old_p1,j_old_p1,k,n) 
            enddo
          enddo
!
        enddo     ! loop over j
      enddo       ! loop over i      

      end subroutine horiz_interp_  

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: vert_interp_ --- vertically interpolates vertical diff. coeffs.
!
! !INTERFACE:

      subroutine vert_interp_(in,jn,kn,km,icah,icam,icch,iccm,     &
                              ptop_old,ptop_new,                   &
                              delp_new,delp_half,coef_half,coef_new)

! !USES:

      implicit none

 
! !INPUT PARAMETERS:

   integer,  intent(in) :: in ! number of E-W points of new grid
   integer,  intent(in) :: jn ! number of S-N points of new grid
   integer,  intent(in) :: kn ! number of vertical points of new grid
   integer,  intent(in) :: km ! number of vertical points of old grid
   integer,  intent(in) :: icah, icam, icch, iccm
   real(r8), intent(in) :: delp_new(in,jn,kn)  ! delp on new grid
   real(r8), intent(in) :: ptop_old  ! pressure at top of old grid 
   real(r8), intent(in) :: ptop_new  ! pressure at top of new grid 
   real(r8), intent(in) :: delp_half(in,jn,km) ! delp on new horizontal grid
                                     ! but still on old vertical grid 
   real(r8), intent(in) :: coef_half(in,jn,km,4) 
                                     ! coefs on new horizontal grid
                                     ! but still on old vertical grid 
! !OUPUT PARAMETERS:

   real(r8), intent(out) :: coef_new(in,jn,kn,4) ! coefs on new grid
 
! !DESCRIPTION: Vertically interpolates vertical diffusion coefficients to the
!  new vertical grid from half-transformed values defined for the new
!  horizontal grid but the old vertical grid.
!
!  Coefficients are assumed defined at the kmax-1 model interfaces.
!  The coefficient indexing for cch and ccm is augmented by 1 (the index
!  refers to the level whose time tendency the coef is used to compute).
!  The interpolation is linear in pressure, with extrapolations below the 
!  lowest old interface or above the first old interface asume 0 gradient.
!
!  The actual eddy diffusion coefficients at any interface pressure level 
!  that are used to define either an upper or lower diagonal matrix 
!  element.should be identical.  The values being interpoalted here, however, 
!  are not the actual diffusion coefficients but just proportioanl to them, 
!  but with different proportionality constants for different diagonals.
!  Therefore, nothing is done here to ensure this property. Unless such
!  ensurance is critical for some propertyin the TLM, then it is deemed
!  acceptable to omit it. 
!
!  coefs at level k=kn for index icam contain the surface drag coefficients
!  that are not interpolated but simply copied.
!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none
!
!EOP
!-------------------------------------------------------------------------

! Local variables:
 
   integer  :: i,j     ! horizontal coordinates of new grid
   integer  :: k       ! vertical grid index for new grid
   integer  :: ko      ! vertical grid index for old grid
   integer  :: kh1     ! 
   integer  :: kh2     ! either flag 0, or index of first level for pold>pnew

   real     :: pn     ! interface pressure for new grid (assuming top=0) 
   real     :: ph2    ! interface pressure on old grid (assuming top=0)
   real     :: ph1    ! previous value of ph1
   real     :: a

      do i=1,in
        do j=1,jn
    
          coef_new(i,j,1,icch)=0.
          coef_new(i,j,1,iccm)=0.
          coef_new(i,j,kn,icah)=0.
          coef_new(i,j,kn,icam)=coef_half(i,j,km,icam)

          pn=ptop_new
          ph1=ptop_old+delp_half(i,j,1)
          kh2=1 

          do k=1,kn-1

            pn=pn+delp_new(i,j,k)
            ph2=ph1
            kh1=max(kh2-1,1) 
            kh2=0
!
! Find first occurance of p(half-transformed grid) >= p(new grid)         
            do ko=kh1,km-1
              if ( (kh2 == 0) .and. (ph2 >= pn) ) then 
                kh2=ko                  
              endif
              if ( (kh2 == 0) .and. (ko < km-1) ) then 
                ph1=ph2        
                ph2=ph2+delp_half(i,j,ko+1)        
              endif
            enddo

            if (kh2 == 1) then
! new level has p less than lowest p-value on old grid
              coef_new(i,j,  k,icah)=coef_half(i,j,1,icah)
              coef_new(i,j,  k,icam)=coef_half(i,j,1,icam)
              coef_new(i,j,k+1,icch)=coef_half(i,j,2,icch)
              coef_new(i,j,k+1,iccm)=coef_half(i,j,2,iccm)
         
            else if (kh2 == 0) then 
! new level has p greater than greatest p-value on old grid                
              coef_new(i,j,  k,icah)=coef_half(i,j,km-1,icah) 
              coef_new(i,j,  k,icam)=coef_half(i,j,km-1,icam)                 
              coef_new(i,j,k+1,icch)=coef_half(i,j,  km,icch) 
              coef_new(i,j,k+1,iccm)=coef_half(i,j,  km,iccm)
            else
! new level sandwhiched between old model levels 
              a=(pn-ph1)/(ph2-ph1)        
              coef_new(i,j,k+1,icch)=     a*coef_half(i,j,kh2+1,icch)  &
                                    +(1.-a)*coef_half(i,j,  kh2,icch)
              coef_new(i,j,k+1,iccm)=     a*coef_half(i,j,kh2+1,iccm)  &
                                    +(1.-a)*coef_half(i,j,  kh2,iccm)        
              coef_new(i,j,  k,icah)=     a*coef_half(i,j,  kh2,icah)  &
                                    +(1.-a)*coef_half(i,j,kh2-1,icah)
              coef_new(i,j,  k,icam)=     a*coef_half(i,j,  kh2,icam)  &
                                    +(1.-a)*coef_half(i,j,kh2-1,icam)          
            endif
   
          enddo  ! loop over k vertical index for new grid   
        enddo    ! loop over j
      enddo      ! loop over i


      end subroutine vert_interp_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: printc_ --- vertically interpolates vertical diff. coeffs.
!
! !INTERFACE:

      subroutine printc_ (im,jm,km,num,numc,coefs,caption,iform)

! !USES:

      implicit none

! !INPUT PARAMETERS:

      integer, intent(in) :: im,jm,km,num,numc,iform
      real(r8), intent(in) :: coefs(im,jm,km,num)
      character(len=*), intent(in) :: caption

!
! !REVISION HISTORY:
!
!  ?????2004  Errico     Initial module.
!  27Sep2004  Todling    Added protex-prologue; modularized; implicit none
!
!EOP
!-------------------------------------------------------------------------

      integer :: j,k
   
      print *,' '
      print *,caption,'   format=',iform

      if (iform==1) then
        do j=jm,1,-1
          write(stdout,'(a,i2)'  )'j=',j
          write(stdout,'(12f8.3)')coefs(:,j,2,numc)
        enddo
      endif

      if (iform==2) then
        do k=1,km
          write(stdout,'(a,i2)'  )'k=',k
          write(stdout,'(12f8.3)')coefs(1,:,k,numc)
        enddo
      endif
    
      end subroutine printc_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_ --- retrieve vertical diff. coeffs. from file
!
! !INTERFACE:

      subroutine get_ ( fname, nymd, nhms, im, jm, km, vcoef3d, rc )
      
! !USES:
      
      implicit none

! !INPUT PARAMETERS:

      character(len=*), intent(in) :: fname     ! filename template
      
      integer, intent(in) :: nymd, nhms  ! date/time
      integer, intent(in) :: im, jm, km  ! 3d field resolution
           
      
! !OUTPUT PARAMETERS:
      
      real(r8), intent(out)  :: vcoef3d(im,jm,km,4)  ! vert. diff. coeffs.
      integer,  intent(out)  :: rc                   ! returned error code
!
! !REVISION HISTORY:
!
!  29Sep2004  Todling    Initial code.
!
!EOP
!-------------------------------------------------------------------------
      
      character(len=*), parameter :: myname_ = myname//'::get_'
      
      integer :: sptrjfrq, snhms
      integer :: ier
      
      rc = 0
      
      sptrjfrq = nsecf_(ptrjfrq)
      snhms    = nsecf_(nhms)
      if (mod(snhms,sptrjfrq)==0) then
          call GFIO_GetFld ( fname, nymd, nhms, im, jm, km, nvars, names, vcoef3d, stat=ier )
               if (ier/=0) then
	          print *, trim(myname_), ': failed to read vert. diff. coefs.'
                  rc = 1
                  return
               endif      
      endif             
      
      end subroutine get_

!--------------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: put_ --- write out vertical diff. coeffs. from file
!
! !INTERFACE:

      subroutine put_ ( fname, nymd, nhms,   &
                        im, jm, km, vcoef3d, ptop, ks, ak,  bk,	&		
			rc, prec ) 

! !INPUT PARAMETERS:

      character(len=*), intent(in) :: fname  ! filename template

 
      integer,  intent(in) :: nymd, nhms        ! date/time
      integer,  intent(in) :: im, jm, km        ! 3d field resolution
      integer,  intent(in) :: ks
      
      real(r8), intent(in) :: ptop
      real(r8), intent(in) :: ak(km+1)
      real(r8), intent(in) :: bk(km+1)
      real(r8), intent(in) :: vcoef3d(im,jm,km,4)  ! vert. diff. coeffs.

      integer,  intent(in), optional :: prec       ! precision of output
      
! !OUTPUT PARAMETERS:
      
      integer,  intent(out)  :: rc                   ! returned error code

!
! !REVISION HISTORY:
!
!  29Sep2004  Todling    Initial code.
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'::put_'
      
      integer :: sptrjfrq, snhms, lprec
      integer :: ier
     
      rc = 0

      if(present(prec))then
        lprec = prec
      else
        lprec = prec_def
      endif
      
      if ( ptrjfrq .gt. 0 ) then
           sptrjfrq = nsecf_(ptrjfrq)
           snhms    = nsecf_(nhms)
           if ( mod(snhms,sptrjfrq) == 0 ) then
               call GFIO_PutFld( title,source,fname, &
                                 nymd,   nhms,  ptrjfrq,  &
                                 im, jm, km, ptop, ks, ak, bk, &
                                 nvars, names, titles, units, vcoef3d, ier,&
                                 iprec=lprec,  untag=untag )
		if (ier/=0) then
		    rc = 1
		    print *, trim(myname_), ': failed to write vert. diff. coefs.'
		    return
		endif    		 
           endif
      endif
      
      end subroutine put_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: set_ --- General paramater setup for singular vector calc
! 
! !INTERFACE:
!
      subroutine set_ ( nymd, nhms, fnametrj_old, fnametrj_new, rc, expid, RCfile )
 
! !USES: 
    
      implicit None

! !INPUT PARAMETERS: 

     integer,          intent(in) :: nymd
     integer,          intent(in) :: nhms
     character(len=*), intent(in), optional :: expid    ! experiment name
     character(len=*), intent(in), optional :: RCfile   ! filename of resources
!
! !OUTPUT PARAMETERS:
!
     character(len=*), intent(out) :: fnametrj_old   ! old trajectory
     character(len=*), intent(out) :: fnametrj_new   ! new trajectory
     
     integer, intent(out) :: rc                 ! return error code:
                                                !  1 = cannot load resource

! !FILES USED:  dyn2dyn.rc
!
! !DESCRIPTION:  Initializes basic parameters to interpolate dyn vector.
!
! !REVISION HISTORY: 
!
!   28Sep2004  Todling    Initial code.
!
!EOP
!---------------------------------------------------------------------------

      character(len=*), parameter :: myname_   = myname//'::set_'

      character(len=255) template
      character(len=255) d2drc

      integer            ival, iret

!     Default grid: a18
!     -----------------
      integer, parameter :: ptrjfrq_def = 060000  ! 6-hr trajectory update

      rc = 0

!     Load resources
!     --------------
      d2drc = ' '
      if ( present(RCfile) ) then
        d2drc = trim(RCfile)
      else
        call getenv('VDC2VDC_RC',d2drc)		! Unix binding
        if(d2drc.eq.' ') d2drc=def_RCvdc	! default name
      endif
      call i90_loadf (trim(d2drc), iret)
      if( iret .ne. 0) then
         write(stdout,'(3a)') myname_,': Warning, I90_loadf cannot find file ', trim(d2drc)
         rc = 1
         return
      end if
      write(stdout,'( a  )') '-----------------------------------------------------'
      write(stdout,'(3a  )') myname_, ': Reading resource file: ', trim(d2drc)
      write(stdout,'( a,/)') '-----------------------------------------------------'


!     Read in trajectory frequency
!     ----------------------------
      call I90_label('vdc2vdc_frequency:', iret)
      if (iret .eq. 0) then
        ival = I90_GInt(iret)
        if (iret .ne. 0) then
          ptrjfrq = ptrjfrq_def
          write(stdout,'(2a,i5)') myname_,': Frequency of phys traj not found ', ptrjfrq
        else
          ptrjfrq = ival
        end if
      end if
      write(stdout,'(a,i6.6)') 'Frequency of phys trajectory ', ptrjfrq

!     Determine input file name
!     -------------------------
      if (present(expid)) then
          fnametrj_old = trim(expid) // '.vdc.high.nc4'
      else
          fnametrj_old = 'vdc.high.nc4'
      endif
      call i90_label ( 'vdc2vdc_vdc_old_filename:', iret )
      if ( iret .ne. 0 ) then
         write(stdout,'(2a)') 'taking input phys traj default filename: ', trim(fnametrj_old)
      else
        call i90_gtoken ( template, iret )
        if ( iret==0 ) then
           call strTemplate ( fnametrj_new, template, 'GRADS',  xid=expid, &
                              nymd=nymd, nhms=nhms, stat=iret )
           write(stdout,'(2a)') 'input physics trjactory filename: ', trim(fnametrj_old)
	else
	   write(stdout,'(2a)') 'taking input phys traj default filename: ', trim(fnametrj_old)
	endif		      
      endif

!     Determine output file name
!     --------------------------
      if (present(expid)) then
          write(fnametrj_new,'(2a,i8.8,a,i2.2,a)') &
                trim(expid), '.ptrj.prs.', nymd, '_', nymd/10000, 'z.nc4' 
      else
          write(fnametrj_new,'(a,i8.8,a,i2.2,a)') &
                'ptrj.prs.', nymd, '_', nymd/10000, 'z.nc4'        
      endif
      call i90_label ( 'vdc2vdc_vdc_new_filename:', iret )
      if ( iret .ne. 0 ) then
         write(stdout,'(2a)') 'taking output phys traj default filename: ', trim(fnametrj_new)
      else
        call i90_gtoken ( template, iret )
        if ( iret==0 ) then
           call strTemplate ( fnametrj_new, template, 'GRADS', xid=expid, &
                              nymd=nymd, nhms=nhms, stat=iret )
         write(stdout,'(2a)') 'output physics trajectory filename: ', trim(fnametrj_new)
	else
	   write(stdout,'(2a)') 'taking output phys traj default filename: ', trim(fnametrj_new)
	endif		      
      endif
      
!     release resource file:
!     ---------------------
      call I90_release()

      return
      end subroutine set_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: init_ --- Initializes array of coefs to be written out
!
! !INTERFACE:
!
      subroutine init_ ( pnt, im, jm, km, nv, rc )

      implicit none

! !INPUT PARAMETERS:

      integer, intent(in)  :: im, jm, km, nv
      real, pointer :: pnt(:,:,:,:)

! !OUTPUT PARAMETERS:

      integer, intent(out) :: rc

! !DESCRIPTION:   Initializes array of coefs to be written out and pointer
!    coming from GCM; only invoked when running on-line , i.e., full gcm. 
!
! !SEE ALSO: m_phytrj in fvgcm/phys
!
! !REVISION HISTORY:
!
!   28Sep2004  Todling    Initial code.
!
!EOP
!---------------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'::init_'
      integer :: ier
      logical :: iexist

      rc = 0
      storeptrj = .false.

      inquire(file=def_RCvdc,exist=iexist)
      if(.not.iexist) return     ! nothing happens in the absence of RC file

        if (nv/=nvars ) then
            print *, trim(myname_), ': unacceptable nvars'
            rc = 1
            return
        endif
      allocate (coef(im,jm,km,nvars),stat=ier)
        if (ier/=0) then
            print *, trim(myname_), ': cannot alloc() memory'
            rc = 2
            return
        endif

      pnt => coef
      storeptrj = .true.

      if(debug) print *, trim(myname_), ': stored vertical diff coeffs to be written out'
      
      end subroutine init_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: null_ --- Nullify pointers set by init_
!
! !INTERFACE:
!
      subroutine null_ ( pnt )
      real, pointer :: pnt(:,:,:,:)

! !DESCRIPTION:  Nullifies pointer set by init_.
!
! !SEE ALSO: m_phytrj in fvgcm/phys
!
! !REVISION HISTORY:
!
!   28Sep2004  Todling    Initial code.
!
!EOP
!---------------------------------------------------------------------------
      nullify(pnt)
      end subroutine null_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: clean_ --- Clean up memory used by this package
!
! !INTERFACE:
!
      subroutine clean_ ( rc )

      implicit none

! !OUTPUT PARAMETERS:

      integer, intent(out) :: rc

! !DESCRIPTION:  Clean up storaged used to writeout interpolated coeffs;
!    only invoked when running on-line , i.e., full gcm. 
!
! !SEE ALSO: m_phytrj in fvgcm/phys
!
! !REVISION HISTORY:
!
!   28Sep2004  Todling    Initial code.
!
!EOP
!---------------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'::clean_'
      integer :: ier

      rc = 0
      if(.not.storeptrj) return

      deallocate (coef,stat=ier)
        if (ier/=0) then
            print *, trim(myname_), ': cannot dealloc() memory'
            rc = 1
            return
        endif

      storeptrj = .false.
      if(debug) print *, trim(myname_), ': released vertical diff coeffs used for write-out'

      end subroutine clean_

      function nsecf_( n )
      implicit none
      integer nsecf_
      integer n
      nsecf_ = n/10000*3600 + mod(n,10000)/100* 60 + mod(n,100)
      end function nsecf_

  end module m_vdc2vdc
