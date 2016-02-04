!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  WetRemovalMod --- Aerosol Large-Scale Wet Removal Module
!
! !INTERFACE:
!

   module  WetRemovalMod

! !USES:

   use Chem_Mod
   use Chem_ConstMod, only: grav, von_karman, cpd        ! Constants !

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  WetRemovalGOCART

!
! !DESCRIPTION:
!
!  This module implements various wet removal schemes
!
! !REVISION HISTORY:
!
!  08Jan2010 - Colarco, first crack
!
!EOP
  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0

!-------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: WetRemovalGOCART - Calculate aerosol wet removal due
!                               to large scale processes.
!
! !INTERFACE:
!

   subroutine WetRemovalGOCART ( i1, i2, j1, j2, km, n1, n2, cdt, &
                                 qa, ple, tmpu, rhoa, dqcond, precc, precl, &
                                 fluxout, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, n1, n2, km
   real, intent(in)    :: cdt
   TYPE(Chem_Array), pointer           :: qa(:)          ! tracer array will go here
   real, pointer, dimension(:,:,:)     :: ple, tmpu, rhoa, dqcond
   real, pointer, dimension(:,:)       :: precc, precl
   TYPE(Chem_Array), pointer           :: fluxout        ! tracer loss flux [kg m-2 s-1]

! !OUTPUT PARAMETERS:
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 

! !DESCRIPTION: Calculates the updated species concentration due to wet
!               removal.  As written, intended to function for large
!               scale (not convective) wet removal processes
!
! !REVISION HISTORY:
!
!  08Jan2010 - Colarco, based on GOCART implementation, does not
!                       include any size dependent term
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   character(len=*), parameter :: myname = 'WetRemovalGOCART'
   integer  ::  i, j, k, iit, n, nbins, LH, kk, ios
   real :: pdog(i1:i2,j1:j2,km)      ! air mass factor dp/g [kg m-2]
   real :: pls, pcv, pac             ! ls, cv, tot precip [mm day-1]
   real :: qls(km), qcv(km)          ! ls, cv portion dqcond [kg m-3 s-1]
   real :: qmx, qd, A                ! temporary variables on moisture
   real :: F, B, BT                  ! temporary variables on cloud, freq.
   real, allocatable :: fd(:,:)      ! flux across layers [kg m-2]
   real, allocatable :: DC(:)        ! scavenge change in mass mixing ratio

!  Rain parameters from Liu et al.
   real, parameter :: B0_ls = 1.0e-4
   real, parameter :: F0_ls = 1.0
   real, parameter :: XL_ls = 5.0e-4
   real, parameter :: B0_cv = 1.5e-3
   real, parameter :: F0_cv = 0.3
   real, parameter :: XL_cv = 2.0e-3
!  Duration of rain: ls = model timestep, cv = 1800 s (<= cdt)
   real            :: Td_ls
   real, parameter :: Td_cv = 1800.


!  Efficiency of dust wet removal (since dust is really not too hygroscopic)
!  Applied only to in-cloud scavenging
   real :: effRemoval

   rc=0

!  Initialize local variables
!  --------------------------
   Td_ls = cdt
   nbins = n2-n1+1
   if( associated(fluxout%data2d) ) fluxout%data2d(i1:i2,j1:j2) = 0.0

!  Allocate the dynamic arrays
   allocate(fd(km,nbins),stat=ios)
   if(ios .ne. 0) stop
   allocate(dc(nbins),stat=ios)
   if(ios .ne. 0) stop

!  Accumulate the 3-dimensional arrays of rhoa and pdog
   pdog = (ple(:,:,1:km)-ple(:,:,0:km-1)) / grav

!  Loop over spatial indices
   do j = j1, j2
    do i = i1, i2

!    Check for total precipitation amount
!    Assume no precip in column if precl+precc = 0
     pac = precl(i,j) + precc(i,j)
     if(pac .le. 0.) goto 100
     pls = precl(i,j)
     pcv = precc(i,j)

!    Initialize the precipitation fields
     qls(:)  = 0.
     qcv(:)  = 0.
     fd(:,:) = 0.

!    Find the highest model layer experiencing rainout.  Assumes no
!    scavenging if T < 258 K
     LH = 0
     do k = 1, km
      if(dqcond(i,j,k) .lt. 0. .and. tmpu(i,j,k) .gt. 258.) then
       LH = k
       goto 15
      endif
     end do
 15  continue
     if(LH .lt. 1) goto 100

!    convert dqcond from kg water/kg air/s to kg water/m3/s and reverse
!    sign so that dqcond < 0. (positive precip) means qls and qcv > 0.
     do k = LH, km
      qls(k) = -dqcond(i,j,k)*pls/pac*rhoa(i,j,k)
!      qcv(k) = -dqcond(i,j,k)*pcv/pac*rhoa(i,j,k)
     end do

!    Loop over vertical to do the scavenging!
     do k = LH, km

!-----------------------------------------------------------------------------
!   (1) LARGE-SCALE RAINOUT:             
!       Tracer loss by rainout = TC0 * F * exp(-B*dt)
!         where B = precipitation frequency,
!               F = fraction of grid box covered by precipitating clouds.
!       We assume that tracer scavenged by rain is falling down to the
!       next level, where a fraction could be re-evaporated to gas phase
!       if Qls is less then 0 in that level.
!-----------------------------------------------------------------------------
      if (qls(k) .gt. 0.) then
       F  = F0_ls / (1. + F0_ls*B0_ls*XL_ls/(qls(k)*cdt/Td_ls))
       B  = B0_ls/F0_ls +1./(F0_ls*XL_ls/qls(k)) 
       BT = B * Td_ls
       if (BT.gt.10.) BT = 10.               !< Avoid overflow >
!      Adjust du level:
       do n = 1, nbins
        effRemoval = qa(n1+n-1)%fwet
        DC(n) = qa(n1+n-1)%data3d(i,j,k) * F * effRemoval *(1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        qa(n1+n-1)%data3d(i,j,k) = qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
       end do
!      Flux down:  unit is kg m-2
!      Formulated in terms of production in the layer.  In the revaporation step
!      we consider possibly adding flux from above...
       do n = 1, nbins
        Fd(k,n) = DC(n)*pdog(i,j,k)
       end do

      end if                                    ! if Qls > 0  >>>

!-----------------------------------------------------------------------------
! * (2) LARGE-SCALE WASHOUT:
! *     Occurs when rain at this level is less than above.
!-----------------------------------------------------------------------------
      if(k .gt. LH .and. qls(k) .ge. 0.) then
       if(qls(k) .lt. qls(k-1)) then
!       Find a maximum F overhead until the level where Qls<0.
        Qmx   = 0.
        do kk = k-1,LH,-1
         if (Qls(kk).gt.0.) then
          Qmx = max(Qmx,Qls(kk))
         else
          goto 333
         end if
        end do

 333    continue
        F = F0_ls / (1. + F0_ls*B0_ls*XL_ls/(Qmx*cdt/Td_ls))
        if (F.lt.0.01) F = 0.01
!-----------------------------------------------------------------------------
!  The following is to convert Q(k) from kgH2O/m3/sec to mm/sec in order
!  to use the Harvard formula.  Convert back to mixing ratio by multiplying
!  by rhoa.  Multiply by pdog gives kg/m2/s of precip.  Divide by density
!  of water (=1000 kg/m3) gives m/s of precip and multiply by 1000 gives
!  units of mm/s (omit the multiply and divide by 1000).
!-----------------------------------------------------------------------------

        Qd = Qmx /rhoa(i,j,k)*pdog(i,j,k)
        if (Qd.ge.50.) then
         B = 0.
        else
         B = Qd * 0.1
        end if
        BT = B * cdt
        if (BT.gt.10.) BT = 10.

!       Adjust du level:
        do n = 1, nbins
         DC(n) = qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         qa(n1+n-1)%data3d(i,j,k) = qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
         if( associated(fluxout%data2d) ) then
          fluxout%data2d(i,j) = fluxout%data2d(i,j)+DC(n)*pdog(i,j,k)/cdt
         endif
        end do

       end if
      end if                                    ! if ls washout  >>>

!-----------------------------------------------------------------------------
!  (3) CONVECTIVE RAINOUT:
!      Tracer loss by rainout = dd0 * F * exp(-B*dt)
!        where B = precipitation frequency,
!              F = fraction of grid box covered by precipitating clouds.
!-----------------------------------------------------------------------------

      if (qcv(k) .gt. 0.) then
       F  = F0_cv / (1. + F0_cv*B0_cv*XL_cv/(Qcv(k)*cdt/Td_cv))
       B  = B0_cv
       BT = B * Td_cv
       if (BT.gt.10.) BT = 10.               !< Avoid overflow >

!      Adjust du level: 
       do n = 1, nbins
        effRemoval = qa(n1+n-1)%fwet
        DC(n) = qa(n1+n-1)%data3d(i,j,k) * F * effRemoval * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        qa(n1+n-1)%data3d(i,j,k) = qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
       end do

!------  Flux down:  unit is kg. Including both ls and cv.
       do n = 1, nbins
        Fd(k,n) = Fd(k,n) + DC(n)*pdog(i,j,k)
       end do

      end if                                  ! if Qcv > 0   >>>

!-----------------------------------------------------------------------------
!  (4) CONVECTIVE WASHOUT:
!      Occurs when rain at this level is less than above.
!-----------------------------------------------------------------------------

      if (k.gt.LH .and. Qcv(k).ge.0.) then
       if (Qcv(k).lt.Qcv(k-1)) then
!-----  Find a maximum F overhead until the level where Qls<0.
        Qmx   = 0.
        do kk = k-1, LH, -1
         if (Qcv(kk).gt.0.) then
          Qmx = max(Qmx,Qcv(kk))
         else
          goto 444
         end if
        end do

 444    continue
        F = F0_cv / (1. + F0_cv*B0_cv*XL_cv/(Qmx*cdt/Td_cv))
        if (F.lt.0.01) F = 0.01
!-----------------------------------------------------------------------------
!  The following is to convert Q(k) from kgH2O/m3/sec to mm/sec in order
!  to use the Harvard formula.  Convert back to mixing ratio by multiplying
!  by rhoa.  Multiply by pdog gives kg/m2/s of precip.  Divide by density
!  of water (=1000 kg/m3) gives m/s of precip and multiply by 1000 gives
!  units of mm/s (omit the multiply and divide by 1000).
!-----------------------------------------------------------------------------

        Qd = Qmx / rhoa(i,j,k)*pdog(i,j,k)
        if (Qd.ge.50.) then
         B = 0.
        else
         B = Qd * 0.1
        end if
        BT = B * cdt
        if (BT.gt.10.) BT = 10.

!       Adjust du level:
        do n = 1, nbins
         DC(n) = qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         qa(n1+n-1)%data3d(i,j,k) = qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
         if( associated(fluxout%data2d) ) then
          fluxout%data2d(i,j) = fluxout%data2d(i,j)+DC(n)*pdog(i,j,k)/cdt
         endif
        end do

       end if
      end if                                    ! if cv washout  >>>

!-----------------------------------------------------------------------------
!  (5) RE-EVAPORATION.  Assume that SO2 is re-evaporated as SO4 since it
!      has been oxidized by H2O2 at the level above. 
!-----------------------------------------------------------------------------
!     Add in the flux from above, which will be subtracted if reevaporation occurs
      if(k .gt. LH) then
       do n = 1, nbins
        Fd(k,n) = Fd(k,n) + Fd(k-1,n)
       end do

!      Is there evaporation in the currect layer?
       if (-dqcond(i,j,k) .lt. 0.) then
!       Fraction evaporated = H2O(k)evap / H2O(next condensation level).
        if (-dqcond(i,j,k-1) .gt. 0.) then

          A =  abs(  dqcond(i,j,k) * pdog(i,j,k)    &
            /      ( dqcond(i,j,k-1) * pdog(i,j,k-1))  )
          if (A .gt. 1.) A = 1.

!         Adjust tracer in the level
          do n = 1, nbins
           DC(n) =  Fd(k-1,n) / pdog(i,j,k) * A
           qa(n1+n-1)%data3d(i,j,k) = qa(n1+n-1)%data3d(i,j,k) + DC(n)
           qa(n1+n-1)%data3d(i,j,k) = max(qa(n1+n-1)%data3d(i,j,k),1.e-32)
!          Adjust the flux out of the bottom of the layer
           Fd(k,n)  = Fd(k,n) - DC(n)*pdog(i,j,k)
          end do

        endif
       endif                                   ! if -moistq < 0
      endif
     end do  ! k

     do n = 1, nbins
      if( associated(fluxout%data2d) ) then
       fluxout%data2d(i,j) = fluxout%data2d(i,j)+Fd(km,n)/cdt
      endif
     end do

 100 continue
    end do   ! i
   end do    ! j

   deallocate(fd,DC,stat=ios)

   end subroutine WetRemovalGOCART

   end module WetRemovalMod
