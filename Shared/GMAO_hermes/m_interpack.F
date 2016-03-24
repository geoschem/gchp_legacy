      module m_interpack

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
! !TITLE:  Interpolation Package
!
! !AUTHORS: Unknown (put together by R. Todling)
!
! !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
! !DATE: 17 April 1998
!
! !INTRODUCTION: What is it?
!
!  This package encompasses the routines used in GEOS-2 for performing
!  interpolation of the first guess to observation routines.
!
! !REVISION HISTORY: 
!
!  06Jul2007  Todling  Wrapped as module
!  05Jul2011  Todling  Add terpv3dr4_ (though not in the best way)
!
!  \bigskip
!
!EOI
!-------------------------------------------------------------------------

      implicit none
      
      private
      
      public interpack_terpv
      
      interface  interpack_terpv; module procedure
     &           terpv_,
     &           terpv2d_,
     &           terpv2dr4_,
     &           terpv3d_,
     &           terpv3dr4_
      end interface

      character(len=*), parameter :: myname = 'm_interpack'

      real, parameter :: UNDEF    = 1.e15
      real, parameter :: LON0_DEF = 0.

      CONTAINS

      subroutine terpv3dr4_ ( im1, jm1, km1, f, im2, jm2, km2, fint, rc,
     &                        lon0, fill ) ! optionals

      implicit none

      integer, intent(in)    :: im1, jm1, km1    ! dim of input vector
      integer, intent(in)    :: im2, jm2, km2    ! dim to interpolate to
      integer, intent(out)   :: rc
      real(4), intent(in)    ::    f(im1,jm1,km1)
      real(4), intent(inout) :: fint(im2,jm2,km2)
      real(4), intent(in), optional :: lon0
      real(4), intent(in), optional :: fill

      real(8) ::  f_r8(im1,jm1,km1)
      real(8) :: fint_r8(im2,jm2,km2)

      f_r8 = f
      fint_r8 = fint

      call terpv3d_ ( im1, jm1, km1, f_r8, im2, jm2, km2, fint_r8, rc,
     &                real(lon0,8), real(fill,8) ) ! optionals

      fint = fint_r8

      end subroutine terpv3dr4_

      subroutine terpv3d_ ( im1, jm1, km1, f, im2, jm2, km2, fint, rc,
     &                      lon0, fill ) ! optionals

      implicit none

      integer, intent(in)    :: im1, jm1, km1    ! dim of input vector
      integer, intent(in)    :: im2, jm2, km2    ! dim to interpolate to
      integer, intent(out)   :: rc
      real,    intent(in)    ::    f(im1,jm1,km1)
      real,    intent(inout) :: fint(im2,jm2,km2)
      real,    intent(in), optional :: lon0
      real,    intent(in), optional :: fill

      character(len=*), parameter :: myname_ = myname//'*terpv3d_'
      integer k

      rc=0
      if (km1/=km2) then
          rc=99
          return
      endif

      do k = 1, km1
         call terpv2d_ ( im1, jm1, f(:,:,k), im2, jm2, fint(:,:,k), lon0=lon0, fill=fill )
      enddo

      end subroutine terpv3d_

      subroutine terpv2dr4_ ( im1, jm1, f, im2, jm2, fint,
     &                        lon0, fill ) ! optionals

      implicit none

      integer, intent(in)    :: im1, jm1     ! dim of input vector
      integer, intent(in)    :: im2, jm2     ! dim to interpolate to
      real(4), intent(in)    ::    f(im1,jm1)
      real(4), intent(inout) :: fint(im2,jm2)
      real, intent(in), optional :: lon0
      real, intent(in), optional :: fill

      real, allocatable, dimension(:,:) :: f_r8, fint_r8
    
      allocate(f_r8(im1,jm1))
      allocate(fint_r8(im2,jm2))

      f_r8=f
      call terpv2d_ ( im1, jm1, f_r8, im2, jm2, fint_r8,
     &                lon0=lon0, fill=fill )
      fint=fint_r8
      deallocate(fint_r8)
      deallocate(f_r8)

      end subroutine terpv2dr4_

      subroutine terpv2d_ ( im1, jm1, f, im2, jm2, fint,
     &                      lon0, fill ) ! optionals

      implicit none

      integer, intent(in)    :: im1, jm1     ! dim of input vector
      integer, intent(in)    :: im2, jm2     ! dim to interpolate to
      real,    intent(in)    ::    f(im1,jm1)
      real,    intent(inout) :: fint(im2,jm2)
      real,    intent(in), optional :: lon0
      real,    intent(in), optional :: fill

      integer  i,j,ij,len
      real, allocatable :: aux(:)
      real, allocatable :: lats(:)
      real, allocatable :: lons(:)
      real lon0_

      len = im2*jm2
      allocate( aux(len), lats(len), lons(len) )

                        lon0_ = LON0_DEF
      if(present(lon0)) lon0_ = lon0

      ij=0
      do j=1,jm2
         do i=1,im2
            ij=ij+1
            lats(ij) = -90.  + (j-1)*180./(jm2-1)
            lons(ij) = lon0_ + (i-1)*360./ im2
         enddo
      enddo

      call terpv_ ( lats, lons, aux, len, f, im1, jm1, deglon0=lon0_, fill=fill ) 

      fint = reshape(aux(1:len),(/im2,jm2/))

      deallocate( aux, lats, lons )

      end subroutine terpv2d_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Terpv --- Bi-linear interpolation to lat/lon locations
! 
! !INTERFACE:
!
      subroutine Terpv_ ( DEGLAT, DEGLON, FINT, LEN, F, IM, JNP,
     &                    deglon0, fill ) ! optionals
!
! !USES:

      Implicit None

! !INPUT PARAMETERS: 
 
      integer, intent(in)  :: len
      integer, intent(in)  :: IM, JNP
      real,    intent(in)  :: DEGLAT(LEN)
      real,    intent(in)  :: DEGLON(LEN)
      real,    intent(in)  :: F(IM,JNP)
      real,    intent(in), optional  :: DEGLON0     ! starting longitude
      real,    intent(in), optional  :: FILL        ! undefined value
      
! !OUTPUT PARAMETERS:

      real,    intent(inout) :: FINT(LEN)

! !DESCRIPTION: This routine performs bilinear interpolation of a field, 
!               f, to a set of lat/lon locations. Notice that
!  deglat(i) is assumed to be between  -90.0 and  +90.0, and that
!  deglon(i) is assumed to be between -180.0 and +180.0.
!
! !REVISION HISTORY: 
!
!  unknown   unknown    Initial code. There was no revision history
!                       recorded in this file.
!  13aug97   Todling    Switched position of common block with
!                       declaration of F, so that im and jnp
!                       be defined before used 
!  09Oct97   da Silva   Added ProTeX compliant prologue. Changed
!                       #include "file"  into  include "file" when
!                       applicable. 
!  23Oct97   Todling    Passing im, jnp to eliminate common /contrl/
!  24Oct97   Todling    Incrementally changing value of fill
!  18Feb98   Todling    Declared implicit none.
!  24Feb98   Todling    Value of fill from getcon()
!  06Jul07   Todling    Declared intent; removed starting lon assumption
!
!EOP
!-------------------------------------------------------------------------



      INTEGER           II
      INTEGER           I0
      INTEGER           I1
      INTEGER           J0
      INTEGER           J1
      REAL              QDEGDX
      REAL              QDEGDY
      REAL              WTI1
      REAL              WTJ1
      REAL              XI
      REAL              YJ
      REAL              DEGLON0_
      real       a, b, c, d
      real       d1, d2, d3, d4
      integer    ifl
 
      real    fill_
 
      fill_ = UNDEF
      if(present(fill)) fill_ = fill
      DEGLON0_ = LON0_DEF
      if(present(DEGLON0)) DEGLON0_ = DEGLON0

      QDEGDX = IM / 360.0
      QDEGDY = (JNP-1) / 180.0
 
      DO 100 II = 1, LEN
 
         YJ    = ( DEGLAT(II) +  90.0 ) * QDEGDY + 1.0
         J0    = YJ
         J0    = MAX0( J0, 1 )
         J0    = MIN0( J0, JNP-1 )
         J1    = J0 + 1
 
         XI    = ( DEGLON(II) - DEGLON0_  ) * QDEGDX + 1.0
         IF( XI.GE.(IM+1) ) XI = XI - IM
         IF( XI.LT.1 ) XI = XI + IM
         I0    = XI
         I0    = MAX0( I0, 1 )
         I0    = MIN0( I0, IM )
         I1    = I0 + 1
         IF( I0.EQ.IM ) I1 = 1
 
         WTJ1  = YJ - J0
         WTI1  = XI - I0
 
c Screen for fill values on level - replace fill values according 
c to predetermined criteria. 
 
C
C For brevity, the four corner points will be identified as follows:
C
C         X X   8               X X   4
C         X X                   X X
C         C  (D3)               D  (D4)
C
C
C
C
C         X X   1               X X   2
C         X X                   X X
C         A   (D1)              B   (D2)
C
C
C
C Assign corner values
C
      A = F(I0,J0)
      B = F(I1,J0)
      C = F(I0,J1)
      D = F(I1,J1)
 
C Identify pattern of filled points by using a 4 bit binary representation,
C e.g., 2 (decimal) = 0010 (binary) = corner B only is filled. 
 
      IFL  = 0
 
      IF( A .EQ. FILL_) THEN
              IFL = 1
      ENDIF
      IF( B .EQ. FILL_) THEN
              IFL = IFL + 2
      ENDIF
      IF( D .EQ. FILL_) THEN
              IFL = IFL + 4
      ENDIF
      IF( C .EQ. FILL_) THEN
              IFL = IFL + 8
      ENDIF
 
C
C Filled values are repalced with values obtained from the other corners
C so that the horizontal interpolation can be done.  Note that there are 
C 16 special cases of filled patterns. 
C
      IF( IFL .EQ. 0 ) THEN
              D1 = A
              D2 = B
              D3 = C
              D4 = D
      ELSE IF( IFL .EQ. 1 ) THEN
              D1 = .5 * ( B + C )
              D2 = B
              D3 = C
              D4 = D
      ELSE IF( IFL .EQ. 2 ) THEN
              D1 = A
              D2 = .5 * ( A + D )
              D3 = C
              D4 = D
      ELSE IF( IFL .EQ. 3 ) THEN
              D1 = .5 * ( C + D )
              D2 = D1
              D3 = C
              D4 = D
      ELSE IF( IFL .EQ. 4 ) THEN
              D1 = A
              D2 = B
              D3 = C
              D4 = .5 * ( B + C )
      ELSE IF( IFL .EQ. 5 ) THEN
              D1 = .5 * ( B + C )
              D2 = B
              D3 = C
              D4 = D1
      ELSE IF( IFL .EQ. 6 ) THEN
              D1 = A
              D2 = .5 * ( A + C )
              D3 = C
              D4 = D2
      ELSE IF( IFL .EQ. 7 ) THEN
              D1 = C
              D2 = C
              D3 = C
              D4 = C
      ELSE IF( IFL .EQ. 8 ) THEN
              D1 = A
              D2 = B
              D3 = .5 * ( A + D )
              D4 = D
      ELSE IF( IFL .EQ. 9 ) THEN
              D1 = .5 * ( B + D )
              D2 = B
              D3 = D1
              D4 = D
      ELSE IF( IFL .EQ. 10) THEN
              D1 = A
              D2 = .5 * ( A + D )
              D3 = D2
              D4 = D
      ELSE IF( IFL .EQ. 11) THEN
              D1 = D
              D2 = D
              D3 = D
              D4 = D
      ELSE IF( IFL .EQ. 12) THEN
              D1 = A
              D2 = B
              D3 = .5 * ( A + B )
              D4 = D3
      ELSE IF( IFL .EQ. 13) THEN
              D1 = B
              D2 = B
              D3 = B
              D4 = B
      ELSE IF( IFL .EQ. 14) THEN
              D1 = A
              D2 = A
              D3 = A
              D4 = A
      ELSE IF( IFL .EQ. 15) THEN
              D1 = FILL_
              D2 = FILL_
              D3 = FILL_
              D4 = FILL_
      ENDIF
 
C  HORIZONTAL INTERPOLATION
 
      if (d1.eq.fill_) then 
         fint(ii) =  fill_ 
      else 
         A = D1 
         B = D2 
         C = D3 
         D = D4 
         fint(ii) =  WTJ1  * ( WTI1*D+(1.0-WTI1)*C )
     $        + (1.0-WTJ1) * ( WTI1*B+(1.0-WTI1)*A )
      end if 
 
  100 CONTINUE
 
      RETURN
      END SUBROUTINE TERPV_

      end module m_interpack
