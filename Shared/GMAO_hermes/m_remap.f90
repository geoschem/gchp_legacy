!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_remap --- Implements remapping of Dynamics Vectors
!
! !INTERFACE:
!
      MODULE  m_remap

! !USES:
      
      use m_dyn
      use m_const, only: undef  ! undefined value
      use m_mapz, only: map1_ppm

      Implicit NONE

!
! !PUBLIC TYPES:
!
      PRIVATE
      PUBLIC  rmp_grid          ! interpolator vector vector
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  rmp_init       ! initialize simulator vector
      PUBLIC  rmp_clean      ! deallocate memory use by simulator vector
      PUBLIC  rmp_wind       ! interpolated griddedsimulates interp mass obs
      PUBLIC  rmp_mass       ! interpolated griddedsimulates interp mass obs
      PUBLIC  remap_d2a      ! convert d-grid to a-grid (SJ's version)
!
! !PUBLIC DATA MEMBERS:
!
      integer, public, parameter :: AAGRID = 0  ! field on A-grid
      integer, public, parameter :: UDGRID = 1  ! u-wind on D-grid
      integer, public, parameter :: VDGRID = 2  ! v-wind on D-grid
!
! !DESCRIPTION: This module contains routines remap fvDAS fields to
!               generic lagrangian control values. Routines in mapz_cw.F
!               are needed to run this module
! !REVISION HISTORY:
!
! 29Jan2002  da Silva  First crack.
! 12Jul2010  Todling   Turn d2a public; fix map1_ppm interface call to q
!
!EOP
!-------------------------------------------------------------------------

!BOC

!     Grid information
!     ----------------
      type rmp_grid

!        Zonal grid
!        ----------
         integer       :: im
         real          :: lon_min, lon_max, lon_del

!        Meridional grid
!         ---------------
         integer       :: jm
         real          :: lat_min, lat_max, lat_del

!        Vertical coordinates
!        --------------------
         integer        ::  km
         real           ::  ptop
         real,  pointer ::  pe(:,:,:)    ! pressure at edges (hPa)
         real,  pointer ::  pm(:,:,:)    ! pressure at center (hPa)

      end type rmp_grid

!EOC

      interface remap_d2a
         module procedure d2a_
      end interface

      CONTAINS


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rmp_Init --- Initializes remapping grid
!
! !INTERFACE:
!
      subroutine rmp_Init ( im, jm, km, ptop, grid, rc,   &
                            ps, ak, bk,                   & ! eta: optional
                            delp )                          ! lcv: optional


! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      integer, intent(in)            :: im   ! zonal dimension
      integer, intent(in)            :: jm   ! meridional dimension
      integer, intent(in)            :: km   ! vertical dimension
      real,    intent(in)            :: ptop ! top pressure (Pa)
      
                                ! OPTIONAL target vertical grid specification (eta):

      real,    intent(in), OPTIONAL  :: ps(im,jm) ! sfc pressure (Pa)
      real,    intent(in), OPTIONAL  :: ak(km+1)  ! vertical grid a coefficient
      real,    intent(in), OPTIONAL  :: bk(km+1)  ! vertical grid b coefficient

                                ! OPTIONAL target vertical grid specification (lcv):

      real,    intent(in), OPTIONAL  :: delp(im,jm,km) ! pressure thickness (Pa)


! !OUTPUT PARAMETERS:
!

      type(rmp_grid), intent(out)  :: grid      ! grid specification

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   ...
                                                !

! !DESCRIPTION: This routine allocates memory and initializes the interpolator
!               {\tt grid} with parameters needed for the interpolation
!                calculations. On input, one must specify either {\tt delp}
!               or all of the {\tt (ps,ak,bk)} eta grid specification.
!
! !REVISION HISTORY:
!
!  30May01  da Silva  Initial specification and prologues based on m_insitu
!
!EOP
!-------------------------------------------------------------------------

     character(len=*), parameter :: myname = 'rmp_Init'
     integer    err
     integer    i, j, k 
     real p2d(im,km+1)


!    Sanity check
!    ------------
     rc = 0
     if ( im<1 .or. jm<1 ) then
          rc = 3
          return
     endif

!    Initialize dimensional attributes
!    ---------------------------------
     grid%im = im
     grid%jm = jm
     grid%km = km
     grid%ptop = ptop

     grid%lon_del = 360./im
     grid%lon_min = 0.
     grid%lon_max = grid%lon_min + (im-1) * grid%lon_del

     grid%lat_del = 180./(jm-1)
     grid%lat_min = -90.
     grid%lat_max = 90.

     allocate(grid%pe(im, km+1, jm), stat = err); if (err.ne. 0) rc = 2
     allocate(grid%pm(im, km, jm), stat = err); if (err.ne. 0) rc = 2

     if ( present(ps) .and. present(ak) .and. present(bk) .and. km > 0 ) then

        do k = 1, km + 1
           do j = 1, jm
              do i = 1, im 
                 grid%pe(i,k,j) = (ak(k) + bk(k) * ps(i,j))
                 enddo
              enddo
           enddo

     elseif( present(delp) ) then

        do 1000 j=1, jm

          do i=1,im
            p2d(i,1) = ptop
            enddo

! Top down
          do k=2,km+1
            do i=1,im
              p2d(i,k)  = p2d(i,k-1) + delp(i,j,k-1)
              enddo
            enddo

          do k=1,km+1
            do i=1,im
              grid%pe(i,k,j) = p2d(i,k)
              enddo
            enddo

1000      continue


     else
          rc=1
          return

     endif    


     do k=1,km+1
        do j =1,jm
           do i=1,im
             grid%pe(i,k,j) = grid%pe(i,k,j)
   	     enddo
   	   enddo
        enddo

     do k=1,km
        do j =1,jm
           do i=1,im
             grid%pm(i,k,j) = 0.5*(grid%pe(i,k,j) + grid%pe(i,k+1,j))
    	     enddo
    	   enddo
        enddo
 
  end subroutine rmp_init

!*************************************************************************
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rmp_Clean --- Free memory occupied by simulator vector
!
! !INTERFACE:
!
      subroutine rmp_Clean ( grid )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(rmp_grid), intent(inout)   :: grid       ! Simulator vector


! !DESCRIPTION: This routine deallocates memory occupied by the interpolator
!               {\tt grid}.
!
! !REVISION HISTORY:
!
!  30May01  da Silva  Initial specification and prologues based on m_insitu
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'rmp_Clean'

      if ( associated(grid%pe)  )      deallocate ( grid%pe   )
      if ( associated(grid%pm)  )      deallocate ( grid%pm   )

      end subroutine rmp_clean
 





!*************************************************************************
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rmp_wind --- Calculates remaped winds from dynamics vector
!
! !INTERFACE:
!
      subroutine rmp_wind ( w_f, grid, vname, u_n, v_n, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(dyn_vect), intent(in) :: w_f       ! dynamics state vector
      type(rmp_grid), intent(in) :: grid      ! target grid specification

      character(len=*), intent(in) :: vname   ! Variable name:
                                              !   a-wind (wind on A-grid)
                                              !   d-wind (wind on D-grid)

                                            
! !OUTPUT PARAMETERS:
!
      real, intent(out)           :: u_n(grid%im,grid%jm,grid%km)   ! remaped zonal wind on target grid
      real, intent(out)           :: v_n(grid%im,grid%jm,grid%km)   ! remaped meridional wind on target grid

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !
! !DESCRIPTION: This routine ...
!
! !REVISION HISTORY:
!
!  30May01  da Silva  Initial specification and prologues based on m_insitu
!
!EOP
!-------------------------------------------------------------------------
      real,    allocatable :: ua(:,:), va(:,:), coslon(:), sinlon(:)        
      real,    allocatable :: pem(:,:,:), pkm(:,:,:), pe0(:,:), pe3(:,:)        
      integer i, j, k, im, jm, km, kn
      real rair, rh2o, cpair, akap, dl 

      im = w_f%grid%im
      jm = w_f%grid%jm
      km = w_f%grid%km
      kn = grid%km

      allocate ( ua(im,jm) )
      allocate ( va(im,jm) )
      allocate ( coslon(im) )
      allocate ( sinlon(im) )
      allocate ( pem(im,km+1,jm) )
      allocate ( pkm(im,jm,km+1) )
      allocate ( pe0(im,km+1) )
      allocate ( pe3(im,kn+1) )

      rair   = 287.04
      rh2o   = 4.61e2
      cpair  = 1004.64
      akap  = rair/cpair

!  do vertical mapping first, then D to A grid if needed
!  construct pressure arrays of input grid on w_f

!  input vertical grid
      do k = 1,km+1
      do j=1,jm
      do i=1,im
           pem(i,k,j) = w_f%grid%ak(k) + w_f%grid%bk(k) * w_f%ps(i,j)
	enddo
	enddo
        enddo

!  input vertical grid
      do k=1,km+1
      do j=1,jm
      do i=1,im
          pkm(i,j,k) = pem(i,k,j) ** akap
        enddo
        enddo
        enddo


      do 2000 j=1,jm

! map U
      if(j .ne. 1) then

! input vertical grid
      do k=2,km+1
         do i=1,im
            pe0(i,k) = 0.5*(pem(i,k,j)+pem(i,k,j-1))
         enddo
      enddo

! output vertical grid sigma
      do k=2,kn+1
         do i=1,im
            pe3(i,k) =  0.5*(grid%pe(i,k,j)+grid%pe(i,k,j-1))
         enddo
      enddo


!     write(6,*) 'look here ',pe0,pe3,im,jm

      call map1_ppm( km,   pe0,   w_f%u, &
                     kn,   pe3,   u_n, &
                     im, 1, im, j, 1, jm, 1, 3, undef)

      endif

! map v
      if(j .ne. 1 .and. j .ne. jm) then

! i.ne.1 input vertical grid
      do k=2,km+1
      do i=2,im
      pe0(i,k) = 0.5*(pem(i,k,j)+pem(i-1,k,j))
      enddo
      enddo

! i.eq.1 input vertical grid

      do k=2,km+1
      pe0(1,k) = 0.5*(pem(1,k,j)+pem(im,k,j))
      enddo

! i.ne.1 output vertical grid
      do k=2,kn+1
      do i=2,im
      pe3(i,k) = 0.5*(grid%pe(i,k,j) + grid%pe(i-1,k,j))
      enddo
      enddo

! i=1 output vertical grid
      do k=2,kn+1
         pe3(1,k) = 0.5*(grid%pe(1,k,j)+grid%pe(im,k,j))
      enddo

     call map1_ppm( km,   pe0,   w_f%v, &
                    kn,   pe3,   v_n, &
                    im, 1, im, j, 1, jm, 1, 3, undef)

      endif


2000  continue


     if (vname.eq.'d-wind') return
!     Horizontal mapping from Dgrid to Agrid
      dl = 8.*atan(1.0) / float(im)
      do i=1,im/2
    	 coslon(i)	 = -cos((i-1)*dl)
    	 coslon(i+im/2) = -coslon(i)
    	 sinlon(i)	 = -sin((i-1)*dl)
    	 sinlon(i+im/2) = -sinlon(i)
         enddo
      do k=1,kn
        call d2a_(u_n(:,:,k), v_n(:,:,k), ua ,va,&
                  im, jm, 1, jm, coslon, sinlon)
        u_n(:,:,k) = ua
        v_n(:,:,k) = va
        enddo
     if (vname.eq.'a-wind') return
     rc = 1
     return

     end subroutine rmp_wind

!*************************************************************************
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rmp_mass --- Calculates remaped temperature and constituents
!                          from dynamics vector
!
! !INTERFACE:
!
      subroutine rmp_mass ( w_f, grid, vname, a_n, rc  )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(dyn_vect), intent(in) :: w_f       ! dynamics state vector
      type(rmp_grid), intent(in) :: grid      ! target Grid specification

      character(len=*), intent(in) :: vname   ! Variable name:
                                              !   'pt'  scaled potential temperature (K)
                                              !   'tmpu'  temperature (K)
                                              !   'sphu'  spec humidity (g/kg)

                                            
! !OUTPUT PARAMETERS:
!
      real, intent(out)           :: a_n(grid%im,grid%jm,grid%km)   ! remaped zonal wind on traget vertical 
      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !

      real,    allocatable :: pem(:,:,:), pkm(:,:,:) 
      real,    allocatable :: pe1(:,:), pe2(:,:),pk1(:,:),pk2(:,:)        
      integer i, j, k, im, jm, km, kn
      real rair, rh2o, cpair, akap, pmax, pmin

      rair   = 287.04
      rh2o   = 4.61e2
      cpair  = 1004.64
      akap  = rair/cpair

      im = w_f%grid%im
      jm = w_f%grid%jm
      km = w_f%grid%km
      kn = grid%km

      allocate ( pem(im,km+1,jm) )
      allocate ( pkm(im,jm,km+1) )
      allocate ( pe1(im,km+1) )
      allocate ( pe2(im,kn+1) )
      allocate ( pk1(im,km+1) )
      allocate ( pk2(im,kn+1) )

      a_n = 0
      rc = 1

!  do vertical mapping first, then D to A grid if needed
!  construct pressure arrays of input grid on w_f

!  input vertical grid
      do k = 1,km+1
      do j=1,jm
      do i=1,im
           pem(i,k,j) = w_f%grid%ak(k) + w_f%grid%bk(k) * w_f%ps(i,j)
	enddo
	enddo
        enddo

!  input vertical grid
      do k=1,km+1
      do j=1,jm
      do i=1,im
          pkm(i,j,k) = pem(i,k,j) ** akap
        enddo
        enddo
        enddo

      do 2000 j=1,jm

!  input vertical grid
      do k=1,km+1
      do i=1,im
      pe1(i,k) = pem(i,k,j)
      pk1(i,k) = pkm(i,j,k)
      enddo
      enddo

!  output vertical grid
      do k=1,kn+1
      do i=1,im
      pe2(i,k) = grid%pe(i,k,j)
      pk2(i,k) = grid%pe(i,k,j) ** akap
      enddo
      enddo


! map scaled potential temperature
      if(vname .eq.'tmpu'.or.vname.eq.'pt')  then
         rc=0
        call map1_ppm( km,   pk1,   w_f%pt, &
     	  	       kn,   pk2,   a_n, &
     		       im, 1, im, j, 1, jm, 1, 3, undef)

        if (vname .eq. 'tmpu') then
        do k=1,kn
          a_n(:,j,k) = a_n(:,j,k)*((( grid%pe(:,k,j) + grid%pe(:,k+1,j) )*.5)**akap)   
          enddo
          endif
        endif

! Map constituents
      if(vname .eq. 'sphu') then 
        rc=0
        call map1_ppm( km,   pe1,   w_f%q(:,:,:,1), &
     	  	       kn,   pe2,   a_n, &
     		       im, 1, im, j, 1, jm, 0, 3, undef)
        endif


2000  continue


     end subroutine rmp_mass

      subroutine d2a_(u,v,ua,va,im,jm,jfirst,jlast,coslon,sinlon)
! This is primarily for turbulence package designed for A-grid.
! Also used for output to A-grid.
! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1; removed fvcore.h
! WS 99.07.26 : Added jfirst and jlast as arguments

      implicit none
      integer im, jm, jfirst, jlast
! WS 99.07.26 : u must be ghosted N2S1
      real u(im,jm),v(im,jm),ua(im,jm),va(im,jm),coslon(im),sinlon(im)

      integer   imh
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1 
      real un, vn, us, vs

! Convert D-grid winds to A-grid
! u --> ua, v --> va

      real utmp(im,jm),vtmp(im,jm)

      imh = im/2

      js = 3
      jn = jm - js + 1
      im1 = im-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 45 j=jn+1,jm-1
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jm-1
50    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,im-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) -&
                       (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) -&
                       (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) -&
                       (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) -&
                       (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) - &
                       (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
! Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i) &
              + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i) &
              + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      us = us/im
      vs = vs/im

      do i=1,imh
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN
!Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      un = un/im
      vn = vn/im

      do i=1,imh
      ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
      va(i,jm) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jm) = -ua(i,jm)
      va(i+imh,jm) = -va(i,jm)
      enddo

      ENDIF

      do 100 j=2,jm-1
      do 100 i=1,im
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)
      return
      end subroutine d2a_
  
   end MODULE m_remap

