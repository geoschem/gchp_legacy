module fv_tracer2d_mod
      use fv_arrays_mod,      only: FVPRC, REAL4, REAL8
      use tp_core_mod,        only: fv_tp_2d
      use fv_grid_tools_mod,  only: area, rarea, dxa, dya, dx, dy
      use fv_grid_utils_mod,  only: sina_u, sina_v, sin_sg
      use fv_mp_mod,          only: gid, domain, mp_reduce_max,   &
                                    ng,isd,ied,jsd,jed,is,js,ie,je
      use mpp_domains_mod,    only: mpp_update_domains, CGRID_NE, mpp_get_boundary
      use fv_timing_mod,      only: timing_on, timing_off

implicit none
private

public :: tracer_2d, tracer_2d_1L, offline_tracer_advection


contains

!-----------------------------------------------------------------------
! !ROUTINE: Perform 2D horizontal-to-lagrangian transport
!-----------------------------------------------------------------------

subroutine tracer_2d_1L(q, dp0, mfx, mfy, cx, cy, npx, npy, npz, Qs, nq, hord,  &
                        q_split, k, q3, dt, id_divg, dpA)
      integer, intent(IN) :: npx, npy, npz
      integer, intent(IN) :: k
      integer, intent(IN) :: Qs    ! first tracer to advect
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord
      integer, intent(IN) :: q_split
      integer, intent(IN) :: id_divg
      real(REAL8)   , intent(IN) :: dt
      real(REAL8), intent(INOUT) :: q(isd:ied,jsd:jed,nq)       ! 2D Tracers
      real(REAL8), intent(INOUT) ::q3(isd:ied,jsd:jed,npz,nq)   ! Tracers
      real(REAL8), intent(INOUT) :: dp0(is:ie,js:je)            ! DELP before dyn_core
      real(REAL8), optional, intent(OUT) :: dpA(is:ie,js:je)    ! DELP after advection

      real(FVPRC), intent(IN) ::  cx(is:ie+1,jsd:jed)  ! Courant Number X-Dir
      real(FVPRC), intent(IN) ::  cy(isd:ied,js :je +1)  ! Courant Number Y-Dir
      real(FVPRC), intent(IN) :: mfx(is:ie+1,js:je)    ! Mass Flux X-Dir
      real(FVPRC), intent(IN) :: mfy(is:ie  ,js:je+1)    ! Mass Flux Y-Dir

! Local Arrays
      real(FVPRC) :: fx(is:ie+1,js:je )
      real(FVPRC) :: fy(is:ie , js:je+1)
      real(FVPRC) :: mfx2(is:ie+1,js:je)
      real(FVPRC) :: mfy2(is:ie  ,js:je+1)
      real(FVPRC) ::  cx2(is:ie+1,jsd:jed)
      real(FVPRC) ::  cy2(isd:ied,js :je +1)

      real(REAL8) :: dp1(is:ie,js:je)
      real(REAL8) :: dp2(is:ie,js:je)
      real(FVPRC) :: ra_x(is:ie,jsd:jed)
      real(FVPRC) :: ra_y(isd:ied,js:je)
      real(FVPRC) :: xfx(is:ie+1,jsd:jed)
      real(FVPRC) :: yfx(isd:ied,js: je+1)
      real(FVPRC) :: cmax
      real(FVPRC) :: frac, rdt
      integer :: nsplt
      integer :: i,j,it,iq

      do j=jsd,jed
         do i=is,ie+1
            if (cx(i,j) > 0.) then
                xfx(i,j) = cx(i,j)*dxa(i-1,j)*dy(i,j)*sin_sg(i-1,j,3)
            else
                xfx(i,j) = cx(i,j)*dxa(i,j)*dy(i,j)*sin_sg(i,j,1)
            endif
         enddo
      enddo

      do j=js,je+1
         do i=isd,ied
            if (cy(i,j) > 0.) then
                yfx(i,j) = cy(i,j)*dya(i,j-1)*dx(i,j)*sin_sg(i,j-1,4)
            else
                yfx(i,j) = cy(i,j)*dya(i,j)*dx(i,j)*sin_sg(i,j,2)
            endif
         enddo
      enddo


      if ( q_split==0 ) then
! Determine nsplt for tracer advection
         cmax = 0.
         do j=js,je
            do i=is,ie
               cmax = max(abs(cx(i,j))+(1.-sina_u(i,j)),     &
                          abs(cy(i,j))+(1.-sina_v(i,j)), cmax)
            enddo
         enddo
         call mp_reduce_max(cmax)
         nsplt = int(1.01 + cmax)
         if ( gid == 0 .and. nsplt > 5 )  write(6,*) k, 'Tracer_2d_split=', nsplt, cmax
      else
         nsplt = q_split
      endif


      frac  = 1. / real(nsplt)

          do j=jsd,jed
             do i=is,ie+1
                cx2(i,j) =  cx(i,j) * frac
                xfx(i,j) = xfx(i,j) * frac
             enddo
          enddo

          do j=js,je
             do i=is,ie+1
                mfx2(i,j) = mfx(i,j) * frac
             enddo
          enddo

          do j=js,je+1
             do i=isd,ied
                cy2(i,j) =  cy(i,j) * frac
               yfx(i,j) = yfx(i,j) * frac
             enddo
          enddo

          do j=js,je+1
             do i=is,ie
                mfy2(i,j) = mfy(i,j) * frac
             enddo
          enddo

      do j=jsd,jed
         do i=is,ie
            ra_x(i,j) = area(i,j) + xfx(i,j) - xfx(i+1,j)
         enddo
      enddo

      do j=js,je
         do i=isd,ied
            ra_y(i,j) = area(i,j) + yfx(i,j) - yfx(i,j+1)
         enddo
      enddo

      do j=js,je
         do i=is,ie
            dp1(i,j) = dp0(i,j)
         enddo
      enddo

      do it=1,nsplt

         do j=js,je
            do i=is,ie
               dp2(i,j) = dp1(i,j) + (mfx2(i,j) - mfx2(i+1,j) +  &
                          mfy2(i,j) - mfy2(i,j+1)) * rarea(i,j)
            enddo
         enddo

         call timing_on('COMM_TOTAL')
              call timing_on('COMM_TRAC')
         call mpp_update_domains( q, domain, complete= .true. )
              call timing_off('COMM_TRAC')
         call timing_off('COMM_TOTAL')

         call timing_on("fv_tp_2d")
         do iq=Qs,nq
            call fv_tp_2d( q(isd:,jsd:,iq), cx2, cy2, npx, npy, hord, fx, fy, &
                           xfx, yfx, ra_x, ra_y, mfx=mfx2, mfy=mfy2 )
            if( it==nsplt ) then
            do j=js,je
               do i=is,ie
                  q3(i,j,k,iq) = (q(i,j,iq)*dp1(i,j) + (fx(i,j)-fx(i+1,j) + &
                                  fy(i,j)-fy(i,j+1))*rarea(i,j)) / dp2(i,j)
               enddo
            enddo
            else
            do j=js,je
               do i=is,ie
                  q(i,j,iq) = (q(i,j,iq)*dp1(i,j) + (fx(i,j)-fx(i+1,j) + &
                              fy(i,j)-fy(i,j+1))*rarea(i,j)) / dp2(i,j)
               enddo
            enddo
           endif
         enddo
         call timing_off("fv_tp_2d")

         if ( it/=nsplt ) then
              do j=js,je
                 do i=is,ie
                    dp1(i,j) = dp2(i,j)
                 enddo
              enddo
         endif
     enddo  ! nsplt

     if ( id_divg > 0 ) then
         rdt = 1./(frac*dt)

         do j=js,je
            do i=is,ie
               dp0(i,j) = (xfx(i+1,j)-xfx(i,j) + yfx(i,j+1)-yfx(i,j))*rarea(i,j)*rdt
            enddo
         enddo
     endif

     if (present(dpA)) then
        dpA=dp2
     endif

end subroutine tracer_2d_1L

subroutine tracer_2d(q, dp1, mfx, mfy, cx, cy, npx, npy, npz,   &
                     Qs, nq,  hord, q_split, dt, id_divg, dpA)

      integer, intent(IN) :: npx
      integer, intent(IN) :: npy
      integer, intent(IN) :: npz
      integer, intent(IN) :: Qs    ! first tracer to advect
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord
      integer, intent(IN) :: q_split
      integer, intent(IN) :: id_divg
      real(REAL8)   , intent(IN) :: dt
      real(REAL8), intent(INOUT) :: q(isd:ied,jsd:jed,npz,nq)   ! Tracers
      real(REAL8), intent(INOUT) :: dp1(is:ie,js:je,npz)        ! DELP before dyn_core
      real(REAL8), optional, intent(OUT) :: dpA(is:ie,js:je,npz)! DELP after advection

      real(FVPRC), intent(INOUT) ::  cx(is:ie+1,jsd:jed  ,npz)  ! Courant Number X-Dir
      real(FVPRC), intent(INOUT) ::  cy(isd:ied,js :je +1,npz)  ! Courant Number Y-Dir
      real(FVPRC), intent(INOUT) :: mfx(is:ie+1,js:je,  npz)    ! Mass Flux X-Dir
      real(FVPRC), intent(INOUT) :: mfy(is:ie  ,js:je+1,npz)    ! Mass Flux Y-Dir

! Local Arrays
      real(FVPRC) :: fx(is:ie+1,js:je )
      real(FVPRC) :: fy(is:ie , js:je+1)

      real(REAL8) :: dp2(is:ie,js:je)
      real(FVPRC) :: ra_x(is:ie,jsd:jed)
      real(FVPRC) :: ra_y(isd:ied,js:je)
      real(FVPRC) :: xfx(is:ie+1,jsd:jed  ,npz)
      real(FVPRC) :: yfx(isd:ied,js: je+1, npz)
      real(FVPRC) :: cmax(npz)
#ifdef IBM_FIX
      real(FVPRC) :: cmax_x, cmax_y
#endif
      real(FVPRC) :: c_global
      real(FVPRC) :: frac, rdt
      integer :: nsplt
      integer :: i,j,k,it,iq

!$omp parallel do default(shared)
      do k=1,npz
         do j=jsd,jed
            do i=is,ie+1
               if (cx(i,j,k) > 0.) then
                  xfx(i,j,k) = cx(i,j,k)*dxa(i-1,j)*dy(i,j)*sin_sg(i-1,j,3)
               else
                  xfx(i,j,k) = cx(i,j,k)*dxa(i,j)*dy(i,j)*sin_sg(i,j,1)
               endif
            enddo
         enddo
         do j=js,je+1
            do i=isd,ied
               if (cy(i,j,k) > 0.) then
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j-1)*dx(i,j)*sin_sg(i,j-1,4)
               else
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j)*dx(i,j)*sin_sg(i,j,2)
               endif
            enddo
         enddo
      enddo

!--------------------------------------------------------------------------------
  if ( q_split == 0 ) then
  
! Determine nsplt

#ifdef IBM_FIX
!$omp parallel do default(shared) private(cmax_x, cmax_y)
      do k=1,npz
        cmax(k) = 0.
        do j=js,je
          do i=is,ie
            cmax_x  = max(abs(cx(i,j,k))+1.-sina_u(i,j), cmax(k))
            cmax_y  = max(abs(cy(i,j,k))+1.-sina_v(i,j), cmax(k))
            cmax(k) = max(cmax_x, cmax_y)
          enddo
        enddo
      enddo
#else 
!$omp parallel do default(shared) 
      do k=1,npz
         cmax(k) = 0.
         do j=js,je
            do i=is,ie
               cmax(k) = max(abs(cx(i,j,k))+1.-sina_u(i,j), abs(cy(i,j,k))+1.-sina_v(i,j), cmax(k))
            enddo
         enddo
      enddo
      call mp_reduce_max(cmax,npz)
#endif

! find global max courant number and define nsplt to scale cx,cy,mfx,mfy
      c_global = cmax(1)
      if ( npz /= 1 ) then                ! if NOT shallow water test case
         do k=2,npz
            c_global = max(cmax(k), c_global)
         enddo
      endif
      nsplt = int(1. + c_global)
      if ( gid == 0 .and. nsplt > 5 )  write(6,*) 'Tracer_2d_split=', nsplt, c_global
   else
      nsplt = q_split
   endif
!--------------------------------------------------------------------------------

   frac  = 1. / real(nsplt)

      if( nsplt /= 1 ) then
!$omp parallel do default(shared)
          do k=1,npz
             do j=jsd,jed
                do i=is,ie+1
                   cx(i,j,k) =  cx(i,j,k) * frac
                   xfx(i,j,k) = xfx(i,j,k) * frac
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   mfx(i,j,k) = mfx(i,j,k) * frac
                enddo
             enddo

             do j=js,je+1
                do i=isd,ied
                   cy(i,j,k) =  cy(i,j,k) * frac
                  yfx(i,j,k) = yfx(i,j,k) * frac
                enddo
             enddo

             do j=js,je+1
                do i=is,ie
                  mfy(i,j,k) = mfy(i,j,k) * frac
                enddo
             enddo
          enddo
      endif

    do it=1,nsplt

            call timing_on('COMM_TOTAL')
              call timing_on('COMM_TRAC')
       call mpp_update_domains( q, domain, complete=.true. )
              call timing_off('COMM_TRAC')
            call timing_off('COMM_TOTAL')

!$omp parallel do default(shared) private(ra_x, ra_y, dp2, fx, fy)
      do k=1,npz

         do j=jsd,jed
            do i=is,ie
               ra_x(i,j) = area(i,j) + xfx(i,j,k) - xfx(i+1,j,k)
            enddo
         enddo
         do j=js,je
            do i=isd,ied
               ra_y(i,j) = area(i,j) + yfx(i,j,k) - yfx(i,j+1,k)
            enddo
         enddo

         do j=js,je
            do i=is,ie
               dp2(i,j) = dp1(i,j,k) + (mfx(i,j,k) - mfx(i+1,j,k) +  &
                          mfy(i,j,k) - mfy(i,j+1,k)) * rarea(i,j)
            enddo
         enddo

         call timing_on("fv_tp_2d")
         do iq=Qs,nq
            call fv_tp_2d(q(isd:,jsd:,k,iq), cx(is:,jsd:,k), cy(isd:,js:,k), &
                          npx, npy, hord, fx, fy,            &
                          xfx(is:,jsd:,k), yfx(isd:,js:,k), ra_x, ra_y, &
                          mfx=mfx(is:,js:,k), mfy=mfy(is:,js:,k))

            do j=js,je
               do i=is,ie
                  q(i,j,k,iq) = ( q(i,j,k,iq)*dp1(i,j,k) + &
                                (fx(i,j)-fx(i+1,j) + fy(i,j)-fy(i,j+1))*rarea(i,j) ) / dp2(i,j)
               enddo
            enddo

         enddo
         call timing_off("fv_tp_2d")

         do j=js,je
            do i=is,ie
               dp1(i,j,k) = dp2(i,j)
            enddo
         enddo


      enddo ! npz

   enddo  ! nsplt

! rescale mass fluxes back to original
   mfx = mfx / frac
   mfy = mfy / frac
    cx =  cx / frac
    cy =  cy / frac

   if (present(dpA)) then
      dpA=dp1
   endif

   if ( id_divg > 0 ) then
        rdt = 1./(frac*dt)
!$omp parallel do default(shared)
        do k=1,npz
        do j=js,je
           do i=is,ie
              dp1(i,j,k) = (xfx(i+1,j,k)-xfx(i,j,k) + yfx(i,j+1,k)-yfx(i,j,k))*rarea(i,j)*rdt
           enddo
        enddo
        enddo
   endif

end subroutine tracer_2d

subroutine offline_tracer_advection(q, ple0, ple1, mfx, mfy, cx, cy, ak, bk, ptop, npx, npy, npz,   &
                                    nq, hord, kord, q_split, dt, z_tracer, fill)

      use fv_mapz_mod,        only: map1_q2
      use fv_fill_mod,        only: fillz

      integer, intent(IN) :: npx
      integer, intent(IN) :: npy
      integer, intent(IN) :: npz
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord
      integer, intent(IN) :: kord
      integer, intent(IN) :: q_split
      logical, intent(IN) :: z_tracer
      logical, intent(IN) :: fill
      real(FVPRC), intent(IN   ) :: dt
      real(FVPRC), intent(IN   ) ::ple0(is:ie,js:je,npz+1)      ! DELP before dyn_core
      real(FVPRC), intent(INOUT) ::ple1(is:ie,js:je,npz+1)      ! DELP after dyn_core
      real(FVPRC), intent(IN   ) ::  cx(is:ie,js:je,npz)        ! Courant Number X-Dir
      real(FVPRC), intent(IN   ) ::  cy(is:ie,js:je,npz)        ! Courant Number Y-Dir
      real(FVPRC), intent(IN   ) :: mfx(is:ie,js:je,npz)        ! Mass Flux X-Dir
      real(FVPRC), intent(IN   ) :: mfy(is:ie,js:je,npz)        ! Mass Flux Y-Dir
      real(FVPRC), intent(INOUT) ::   q(is:ie,js:je,npz,nq)     ! Tracers
      real(REAL8), intent(IN   ) ::  ak(npz+1)                  ! AK for remapping
      real(REAL8), intent(IN   ) ::  bk(npz+1)                  ! BK for remapping
      real(REAL8), intent(IN   ) :: ptop

! Local Arrays
      real(REAL8) ::   xL(isd:ied+1,jsd:jed  ,npz)  ! X-Dir for MPP Updates
      real(REAL8) ::   yL(isd:ied  ,jsd:jed+1,npz)  ! Y-Dir for MPP Updates
      real(FVPRC) ::  cxL(is :ie +1,jsd:jed  ,npz)  ! Courant Number X-Dir
      real(FVPRC) ::  cyL(isd:ied  ,js :je +1,npz)  ! Courant Number Y-Dir
      real(FVPRC) :: mfxL(is :ie +1,js :je   ,npz)  ! Mass Flux X-Dir
      real(FVPRC) :: mfyL(is :ie   ,js :je +1,npz)  ! Mass Flux Y-Dir
      real(REAL8) ::  dpL(is :ie   ,js :je   ,npz)  ! Pressure Thickness
      real(REAL8) ::  dpA(is :ie   ,js :je   ,npz)  ! Pressure Thickness
! Local Tracer Arrays
      real(REAL8) ::   q1(is:ie  ,js:je, npz   )! 2D Tracers
      real(REAL8) ::   q2(isd:ied  ,jsd:jed     ,nq)! 2D Tracers
      real(REAL8) ::   q3(isd:ied  ,jsd:jed, npz,nq)! 3D Tracers
! Local Buffer Arrarys
      real(REAL8) :: wbuffer(js:je,npz)
      real(REAL8) :: sbuffer(is:ie,npz)
      real(REAL8) :: ebuffer(js:je,npz)
      real(REAL8) :: nbuffer(is:ie,npz)
! Local Remap Arrays
      real(REAL8)  pe1(is:ie,npz+1)
      real(REAL8)  pe2(is:ie,npz+1)
      real(REAL8)  dp2(is:ie,js:je,npz)
! Local indices
      integer     :: i,j,k,n,iq
      real(REAL8) :: dtR8

      real(REAL8) :: scalingFactor
      !real(REAL8) :: scalingFactors(npz)
! Time-step
    dtR8=dt

! Fill CX/CY C-Grid boundaries and update ghost regions
    xL(is:ie,js:je,:) = cx(:,:,:)
    yL(is:ie,js:je,:) = cy(:,:,:)
    call mpp_get_boundary(xL, yL, domain, &
                          wbufferx=wbuffer, ebufferx=ebuffer, &
                          sbuffery=sbuffer, nbuffery=nbuffer, &
                          gridtype=CGRID_NE )
    xL(ie+1,js:je,:) = ebuffer
    yL(is:ie,je+1,:) = nbuffer
    call mpp_update_domains( xL, yL, domain, gridtype=CGRID_NE, complete=.true.)
    cxL(is:ie+1,jsd:jed,:) = xL(is:ie+1,jsd:jed,:)
    cyL(isd:ied,js:je+1,:) = yL(isd:ied,js:je+1,:)

! Fill MFX/MFY C-Grid boundaries
    xL(is:ie,js:je,:) = mfx(:,:,:)
    yL(is:ie,js:je,:) = mfy(:,:,:)
    call mpp_get_boundary(xL, yL, domain, &
                          wbufferx=wbuffer, ebufferx=ebuffer, &
                          sbuffery=sbuffer, nbuffery=nbuffer, &
                          gridtype=CGRID_NE )
    xL(ie+1,js:je,:) = ebuffer
    yL(is:ie,je+1,:) = nbuffer
    mfxL(is:ie+1,js:je,:) = xL(is:ie+1,js:je,:)
    mfyL(is:ie,js:je+1,:) = yL(is:ie,js:je+1,:)

! Fill local tracers and pressure thickness
    dpL(:,:,:) = ple0(:,:,2:npz+1) - ple0(:,:,1:npz)
    q3(is:ie,js:je,:,:) = q(is:ie,js:je,:,:)

    if ( z_tracer ) then
!$omp parallel do default(shared) private(q2)
       do k=1,npz
         do iq=1,nq
            do j=js,je
               do i=is,ie                   ! To_do list:
                  q2(i,j,iq) = q3(i,j,k,iq) ! The data copying can be avoided if q is
                                            ! re-dimensioned as q(i,j,nq,k)
               enddo
            enddo
         enddo
         call tracer_2d_1L(q2, dpL(is,js,k), mfxL(is,js,k), mfyL(is,js,k), &
                           cxL(is,jsd,k),  cyL(isd,js,k), npx, npy, npz,   &
                           1, nq, hord, q_split, k, q3, dtR8, 0, dpA=dpA)
       enddo
    else
         call tracer_2d(q3, dpL, mfxL, mfyL, cxL, cyL, npx, npy, npz, 1, nq, &
                        hord, q_split, dtR8, 0, dpA=dpA)
    endif

!------------------------------------------------------------------
! Re-Map constituents
! Do remapping one tracer at a time; seems to be faster
! It requires less memory than mapn_ppm
!------------------------------------------------------------------
       do iq=1,nq
          do j=js,je
           ! pressures mapping from (dpA is new delp after tracer_2d)
             pe1(:,1) = ptop
             do k=2,npz+1
               pe1(:,k) = pe1(:,k-1) + dpA(:,j,k-1)
             enddo
           ! pressures mapping to
             pe2(:,1) = ptop
             pe2(:,npz+1) = pe1(:,npz+1)
             do k=2,npz
                 pe2(:  ,k) = ak(k) + bk(k)*pe1(:,npz+1) 
             enddo
             do k=1,npz
                dp2(:,j,k) = pe2(:,k+1) - pe2(:,k)
             enddo
             call map1_q2(npz, pe1, q3(isd,jsd,1,iq),     &
                          npz, pe2, q1(:,j,:), dp2(:,j,:),              &
                          is, ie, 0, kord, j, isd, ied, jsd, jed) !, .true.)
             if (fill) call fillz(ie-is+1, npz, 1, q1(:,j,:), dp2(:,j,:))
          enddo

          ! Rescale tracers based on ple1 at destination timestep
          !------------------------------------------------------
          scalingFactor = calcScalingFactor(q1, dp2, ple1, npx, npy, npz)
          !scalingFactors = computeScalingFactors(q1, dp2, ple1, npx, npy, npz)

          ! Return tracers
          !---------------
          q(is:ie,js:je,1:npz,iq) = q1(is:ie,js:je,1:npz) * scalingFactor
          !do k = 1,npz
          !   do j = js,je
          !      do i = is,ie
          !         q(i,j,k,iq) = q1(i,j,k) * scalingFactors(k)
          !      enddo
          !   enddo
          !enddo

        end do ! nq loop

end subroutine offline_tracer_advection

!------------------------------------------------------------------------------------

         function calcScalingFactor(q1, dp2, ple1, npx, npy, npz) result(scaling)
         use mpp_mod, only: mpp_sum
         integer, intent(in) :: npx
         integer, intent(in) :: npy
         integer, intent(in) :: npz
         real(REAL8), intent(in) :: q1(:,:,:)
         real(REAL8), intent(in) :: dp2(:,:,:)
         real(REAL8), intent(in) :: ple1(:,:,:)
         real(REAL8) :: scaling

         integer :: k
         real(REAL8) :: partialSums(2,npz), globalSums(2)
         real(REAL8), parameter :: TINY_DENOMINATOR = tiny(1.d0)

         !-------
         ! Compute partial sum on local array first to minimize communication.
         ! This algorithm will not be strongly repdroducible under changes do domain
         ! decomposition, but uses far less communication bandwidth (and memory BW)
         ! then the preceding implementation.
         !-------
         do k = 1, npz
            ! numerator
            partialSums(1,k) = sum(q1(:,:,k)*dp2(:,:,k)*area(is:ie,js:je))
            ! denominator
            partialSums(2,k) = sum(q1(:,:,k)*(ple1(:,:,k+1)-ple1(:,:,k))*area(is:ie,js:je))
         end do

         globalSums(1) = sum(partialSums(1,:))
         globalSums(2) = sum(partialSums(2,:))

         call mpp_sum(globalSums, 2)

         if (globalSums(2) > TINY_DENOMINATOR) then
            scaling =  globalSums(1) / globalSums(2)
            !#################################################################
            ! This line was added to ensure strong reproducibility of the code
            ! SDE 2017-02-17: Disabled as it compromises mass conservation
            !#################################################################
            !scaling = REAL(scaling, KIND=REAL4)
         else
            scaling = 1.d0
         end if

         end function calcScalingFactor

!------------------------------------------------------------------------------------

      function computeScalingFactors(q1, dp2, ple1, npx, npy, npz) result(scaling)
         use mpp_mod, only: mpp_sum
         integer, intent(in) :: npx
         integer, intent(in) :: npy
         integer, intent(in) :: npz
         real(REAL8), intent(in) :: q1(:,:,:)
         real(REAL8), intent(in) :: dp2(:,:,:)
         real(REAL8), intent(in) :: ple1(:,:,:)
         real(REAL8) :: scaling(npz)
         
         integer :: k
         real(REAL8) :: partialSums(2,npz), globalSums(2,npz)
         real(REAL8), parameter :: TINY_DENOMINATOR = tiny(1.d0)
         
         !-------
         ! Compute partial sum on local array first to minimize communication.
         ! This algorithm will not be strongly repdroducible under changes do domain
         ! decomposition, but uses far less communication bandwidth (and memory BW)
         ! then the preceding implementation.
         !-------
         do k = 1, npz
            ! numerator
            partialSums(1,k) = sum(q1(:,:,k)*dp2(:,:,k)*area(is:ie,js:je))
            ! denominator
            partialSums(2,k) = sum(q1(:,:,k)*(ple1(:,:,k+1)-ple1(:,:,k))*area(is:ie,js:je))
         end do

         call mpp_sum(partialSums, 2*npz)
         globalSums = partialSums

         do k = 1, npz
            if (partialSums(2,k) > TINY_DENOMINATOR) then
               scaling(k) =  globalSums(1,k) / globalSums(2,k)
            else
               scaling(k) = 1.d0
            end if
         end do

      end function computeScalingFactors


end module fv_tracer2d_mod
