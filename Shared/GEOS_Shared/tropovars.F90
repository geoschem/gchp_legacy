      subroutine tropovars (im,jm,lm,ple,pl,                               &
                            tmpu,sphu,epv,TropP1,TropP2,TropP3,TropT,TropQ )

!********************************************************************
!!                     Subroutine tropopause
!********************************************************************
!
!!ROUTINE:  tropopause
!
!!DESCRIPTION:
!
!     This routine finds the tropopause pressure and temperature from
!     model temperature profiles.  It finds these values for a
!     given 3-dimensional grid.  The algorithm is based on the
!     temperature profile only.  It is similar to visually inspecting
!     a Skew-T Log P diagram for the left-most kink in the temperature
!     profile.  In a standard Skew-T Log P diagram the isotherms
!     intersect the Log P scale at a 45 degree angle.  To more
!     effectively isolate the tropopause,  this angle can be adjusted.
!     That’s how this program works.  It adjusts the angle at which
!     the isotherms intersect the Log P axis via the coeffiecient
!     “alpha.” It simply looks for the furthest point to the left on
!     the profile.  The routine defines the leftward position of each
!     temperature point as xfact:
!
!         xfact = (alpha * temperature) - log (pressure)
!
!     The tropopause for a given profile is where xfact is a minimum.
!     Uncertainty can occur when the kink is not very distinct.  This
!     is where the selection of alpha becomes important.  Optimal
!     values for alpha appear to be between .02 and .04.  For lower
!     values of alpha, the tropopause selection will favor lower
!     levels (higher P).  For higher values of alpha, the selection
!     will favor higher levels (lower P).  A value of alpha = .03
!     appears to be optimal in generating tropopause values within
!     the range indicated in the Handbook of Geophysics and the Space
!     Environment, AFGL, chapt 14, 1985.
!
!!INPUT PARAMETERS:
!
!     alpha   = see discussion above ((log mb)/deg K)
!     im      = number of longitude grid points
!     jm      = number of latitude grid points
!     lm      = number of model levels
!     ple     = pressures at model edges      (Pa)
!     pl      = pressures at model mid-layers (Pa)
!     TmpU    = 3-d array of gridded temperature on model level (K)
!     SphU    = 3-d array of gridded spec.hum.   on model level (kg/kg)
!     EPV     = 3-d array of gridded Ertels Potential Vorticity
!
!!OUTPUT PARAMETERS:
!
!     TropP1 = array of gridded tropopause pressures (Pa) based on Thermal Estimate
!     TropP2 = array of gridded tropopause pressures (Pa) based on EPV     Estimate
!     TropP3 = array of gridded tropopause pressures (Pa) based on Blended Estimate
!     TropT  = array of gridded tropopause temperatures (K)
!     TropQ  = array of gridded tropopause specific humidity (kg/kg)
!
!!REVISION HISTORY:
!
!     Created 25 Jun 97 by Jim Stobie
!
!********************************************************************

  use MAPL_Mod

      implicit none

! Passed Variables

      integer im
      integer jm
      integer lm

      real alpha
      real pl   (im,jm,lm)    ! Pressure at model mid-layers
      real ple  (im,jm,lm+1)  ! Pressure at model edges
      real TmpU (im,jm,lm)
      real Sphu (im,jm,lm)
      real Epv  (im,jm,lm)

      real TropP1(im,jm)
      real TropP2(im,jm)
      real TropP3(im,jm)
      real TropT (im,jm)
      real TropQ (im,jm)

! Local Variables

      integer i,j,k          !loop variables
      integer kend           !end index for model level search
      integer kstart         !start index for model level search
      integer kepv           !index for tropopause level (epv = 3e-6)
      integer ktherm         !index for tropopause level
      integer kblend         !index for tropopause level

      real phigh         !highest pressure for search
      real plow          !lowest pressure for search
      real undef         !value for undefined variables
      real xfacmn        !minimum x-factor, see prologue
      real xfact(lm)     !x-factor, see prologue

      undef = MAPL_UNDEF
      alpha = 0.03  ! Empirical Value for Nominal Conditions

!----------------------------------------------------------------
! Set vertical limits on search.  Tropopause search will be
! limited to the range between plow and phigh (mb).
! According to Handbook of Geophysics and the Space Environment,
! AFGL, 1985, pg 14-6, the lowest tropopause values are near 8 km
! in the polar winter (approx 350 mb) and the highest near 18 km
! in the tropics (approx 80 mb).
!----------------------------------------------------------------

!--------------------
! Loop over lat/lon Grid
!--------------------

        do j = 1, jm
        do i = 1, im

        plow  = 0.04*ple(i,j,lm+1)
        phigh = 0.75*ple(i,j,lm+1)

!-------------------------------------------------------
! Find pressure range for search.  Search will begin
! at first model level edge above phigh, and end
! at the first model level edge below plow.
!-------------------------------------------------------

               kend = lm
            do while (ple(i,j,kend).GE.phigh)
               kend = kend-1
            enddo

               kstart = 1
            do while (ple(i,j,kstart).le.plow)
               kstart = kstart+1
            enddo

!-----------------------------------------------------
! Calculate pressure of the model layer midpoints.
! Then calculate xfact for these points.  See prologue
! for description of xfact.
!-----------------------------------------------------

            do k = kstart, kend
              xfact(k) = alpha * TmpU(i,j,k) - log10(pl(i,j,k)*0.01)
            end do

!-----------------------------------------------
! Tropopause is level for which xfact is minimum
!-----------------------------------------------

            xfacmn = 100000.
            ktherm = 0
            kepv   = 0

! EPV Estimate from Above (First Occurance)
! -----------------------------------------
!           do k = kstart+1,kend+1
            do k = kstart+1,lm
               if( abs(epv(i,j,k)).lt.3.0e-6 ) then
                   kepv = k-1
                   exit
               endif
            enddo

            if( kepv.EQ.kstart .or. kepv.EQ.lm-1 .or. kepv.EQ.0 ) then
                      kepv  = 0
                tropp2(i,j) = undef
            else
                tropp2(i,j) = pl(i,j,kepv)
            end if

! Thermal Estimate
! ----------------
            do k = kstart, kend
               if( xfact(k).LT.xfacmn ) then
                   xfacmn = xfact(k)
                   ktherm = k
               endif
            enddo

            if( ktherm.EQ.kstart .or. ktherm.EQ.kend .or. ktherm.EQ.0 ) then
                    ktherm  = 0
                tropp1(i,j) = undef
            else
                tropp1(i,j) = pl(i,j,ktherm)
            end if

!-------------------------------------------------------
! If the minimum value of xfact is at the upper or lower
! boundary of the search, then the tropopause has not
! been sucessfully isolated.  In this case the grid point 
! value is filled with the undefined value.
!-------------------------------------------------------

            kblend = max( ktherm,kepv )

            if( kblend.EQ.kstart .or. kblend.EQ.kend .or. kblend.EQ.0 ) then

              tropp3(i,j) = undef
              tropt (i,j) = undef
              TropQ (i,j) = undef

            else

!------------------------------------------------------
! If the tropopause has been successfully isolated
!     store tropopause pressure    in TropP
! and store tropopause temperature in TropT.
!------------------------------------------------------

              tropp3(i,j) =   pl(i,j,kblend)
              tropt (i,j) = TmpU(i,j,kblend)
              tropq (i,j) = SphU(i,j,kblend)

            end if

        end do
        end do

      return
      end subroutine tropovars
