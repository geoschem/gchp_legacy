 module fv_surf_map_mod

      use fv_arrays_mod,     only: REAL8
      use fms_mod,           only: file_exist, check_nml_error,            &
                                   open_namelist_file, close_file, stdlog, &
                                   mpp_pe, mpp_root_pe, FATAL, error_mesg
      use mpp_mod,           only: get_unit
      use mpp_domains_mod,   only: mpp_update_domains
      use constants_mod,     only: grav, radius
#ifdef MARS_GCM
      use fms_mod,           only: read_data
      use fms_io_mod,        only: field_size
#endif

      use fv_grid_utils_mod, only: great_circle_dist, latlon2xyz, v_prod, normalize_vect,  &
                                   g_sum, global_mx, sin_sg, vect_cross, stretch_factor
      use fv_mp_mod,         only: domain, ng, is,js,ie,je, isd,jsd,ied,jed, &
                                   gid, mp_stop, mp_reduce_min, mp_reduce_max
      use fv_timing_mod,     only: timing_on, timing_off

      implicit none

      real(REAL8) pi
      private
      real(REAL8), allocatable:: sgh_g(:,:), oro_g(:,:), zs_g(:,:)
!-----------------------------------------------------------------------
! NAMELIST
!    Name, resolution, and format of XXmin USGS datafile
!      1min            ---------> 1.85 km
!         nlon = 10800 * 2
!         nlat =  5400 * 2
!      2min            ---------> 3.7 km
!         nlon = 10800
!         nlat =  5400
!      5min
!         nlon = 4320
!         nlat = 2160
!    surf_format:      netcdf (default)
!                      binary
      integer           ::  nlon = 21600
      integer           ::  nlat = 10800
#ifdef MARS_GCM
      character(len=128)::  surf_file = "INPUT/mars_topo.nc"
      character(len=6)  ::  surf_format = 'netcdf'
      character(len=80) :: field_name 
      integer           :: fld_dims(4)
      real(REAL8), allocatable :: rtopo(:,:)
#else
      character(len=128)::  surf_file = "INPUT/topo1min.nc"
      character(len=6)  ::  surf_format = 'netcdf'
#endif
      real(REAL8) da_min, cos_grid
      real(REAL8), parameter :: r1eM2 = 1.e-2

      namelist /surf_map_nml/ surf_file,surf_format,nlon,nlat
!
      public  sgh_g, oro_g, zs_g
      public  surfdrv
      public  del2_cubed_sphere, del4_cubed_sphere
      public  map_to_cubed_simple

      contains

      subroutine surfdrv(npx, npy, grid, agrid,   &
                         area, dx, dy, dxc, dyc, phis, master) 

      implicit         none
#include <netcdf.inc>
      integer, intent(in):: npx, npy
      logical master

    ! INPUT arrays
      real(REAL8), intent(in)::area(is-ng:ie+ng, js-ng:je+ng)
      real(REAL8), intent(in):: dx(is-ng:ie+ng, js-ng:je+ng+1)
      real(REAL8), intent(in):: dy(is-ng:ie+ng+1, js-ng:je+ng)
      real(REAL8), intent(in)::dxc(is-ng:ie+ng+1, js-ng:je+ng)
      real(REAL8), intent(in)::dyc(is-ng:ie+ng, js-ng:je+ng+1)

      real(REAL8), intent(in):: grid(is-ng:ie+ng+1, js-ng:je+ng+1,2)
      real(REAL8), intent(in):: agrid(is-ng:ie+ng, js-ng:je+ng,2)

    ! OUTPUT arrays
      real(REAL8), intent(out):: phis(is-ng:ie+ng, js-ng:je+ng)
! Local:
      real(REAL8), allocatable :: z2(:,:)
! Position of edges of the box containing the original data point:
      integer          londim
      integer          latdim
      character(len=80) :: topoflnm
      real(kind=4), allocatable ::  ft(:,:), zs(:,:)
      real(REAL8), allocatable :: lon1(:),  lat1(:)
      real(REAL8) dx1, dx2, dy1, dy2, lats, latn, r2d
      real(REAL8) da_max
      real(REAL8)        cd2, zmean, z2mean, delg
!     real(REAL8)        z_sp, f_sp, z_np, f_np
      integer i, j, n
      integer igh, jt
      integer ncid, lonid, latid, ftopoid, htopoid
      integer jstart, jend, start(4), nread(4)
      integer status

      call read_namelist

!
! surface file must be in NetCDF format
!
      if ( file_exist(surf_file) .and. surf_format == "netcdf") then

          status = nf_open (surf_file, NF_NOWRITE, ncid)
          if (status .ne. NF_NOERR) call handle_err(status)
  
          status = nf_inq_dimid (ncid, 'lon', lonid)
          if (status .ne. NF_NOERR) call handle_err(status)
          status = nf_inq_dimlen (ncid, lonid, londim)
          if (status .ne. NF_NOERR) call handle_err(status)
          nlon = londim

          status = nf_inq_dimid (ncid, 'lat', latid)
          if (status .ne. NF_NOERR) call handle_err(status)
          status = nf_inq_dimlen (ncid, latid, latdim)
          if (status .ne. NF_NOERR) call handle_err(status)
          nlat = latdim

          if ( master ) write(*,*) 'Opening USGS datset file:', surf_file, surf_format, nlon, nlat
  
      else
        call error_mesg ( 'surfdrv','Raw IEEE data format no longer supported !!!', FATAL )
      endif

      allocate ( lat1(nlat+1) )
      allocate ( lon1(nlon+1) )

       pi = 4.0 * datan(1.0d0)
      r2d = 180./pi

      cos_grid = cos( 2.*pi/real(nlat) )   ! two-data_grid distance

      dx1 = 2.*pi/real(nlon)
      dy1 = pi/real(nlat)

      do i=1,nlon+1
         lon1(i) = dx1 * real(i-1)    ! between 0 2pi
      enddo

         lat1(1) = - 0.5*pi
         lat1(nlat+1) =  0.5*pi
      do j=2,nlat
         lat1(j) = -0.5*pi + dy1*(j-1)
      enddo

!-------------------------------------
! Compute raw phis and oro
!-------------------------------------
      call timing_on('map_to_cubed')

      if (surf_format == "netcdf") then

!  Find latitude strips reading data
         lats =  pi/2.
         latn = -pi/2.
         do j=js,je
            do i=is,ie
               lats = min( lats, grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
               latn = max( latn, grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
            enddo
         enddo

! Enlarge the search zone:
! To account for the curvature of the coordinates:
         delg = max( 0.2*(latn-lats), pi/real(npx-1), 2.*pi/real(nlat) )
         lats = max( -0.5*pi, lats - delg )
         latn = min(  0.5*pi, latn + delg )

         jstart = 1
         do j=2,nlat
            if ( lats < lat1(j) ) then
                 jstart = j-1
                 exit
            endif
         enddo
         jstart = max(jstart-1, 1)

         jend = nlat
         do j=2,nlat
            if ( latn < lat1(j+1) ) then
                 jend = j+1
                 exit
            endif
         enddo
         jend = min(jend+1, nlat)

         jt = jend - jstart + 1
         igh = nlon/8 + nlon/(2*(npx-1))

         if (master) write(*,*) 'Terrain dataset =', nlon, 'jt=', jt
         if (master) write(*,*) 'igh (terrain ghosting)=', igh

         status = nf_inq_varid (ncid, 'ftopo', ftopoid)
         if (status .ne. NF_NOERR) call handle_err(status)
         nread = 1;   start = 1
         nread(1) = nlon
         start(2) = jstart; nread(2) = jend - jstart + 1

         allocate ( ft(-igh:nlon+igh,jt) )
         status = nf_get_vara_real (ncid, ftopoid, start, nread, ft(1:nlon,1:jt))
         if (status .ne. NF_NOERR) call handle_err(status)

         do j=1,jt
            do i=-igh,0
               ft(i,j) = ft(i+nlon,j)
            enddo
            do i=nlon+1,nlon+igh
               ft(i,j) = ft(i-nlon,j)
            enddo
         enddo

         status = nf_inq_varid (ncid, 'htopo', htopoid)
         if (status .ne. NF_NOERR) call handle_err(status)
         allocate ( zs(-igh:nlon+igh,jt) )
         status = nf_get_vara_real (ncid, htopoid, start, nread, zs(1:nlon,1:jt))
         if (status .ne. NF_NOERR) call handle_err(status)
         status = nf_close (ncid)
         if (status .ne. NF_NOERR) call handle_err(status)
! Ghost Data
         do j=1,jt
            do i=-igh,0
               zs(i,j) = zs(i+nlon,j)
            enddo
            do i=nlon+1,nlon+igh
               zs(i,j) = zs(i-nlon,j)
            enddo
         enddo

      endif

! special SP treatment:
!     if ( jstart == 1 ) then
!          call zonal_mean(nlon, zs(1,1), z_sp)
!          call zonal_mean(nlon, ft(1,1), f_sp)
!     endif

      allocate ( oro_g(isd:ied, jsd:jed) )
      allocate ( sgh_g(isd:ied, jsd:jed) )
                                                     call timing_on('map_to_cubed')
      call map_to_cubed_raw(igh, nlon, jt, lat1(jstart:jend+1), lon1, zs, ft, grid, agrid,  &
                            phis, oro_g, sgh_g, master, npx, npy, jstart, jend)
                                                     call timing_off('map_to_cubed')
      deallocate ( zs )
      deallocate ( ft )
      deallocate ( lon1 )
      deallocate ( lat1 )
      if(master) write(*,*) 'map_to_cubed_raw: master PE done'

! Account for small-earth test cases
      if ( abs(radius-6371.e3) > 0.1 ) then
           do j=js,je
              do i=is,ie
                 phis(i,j) = phis(i,j) * (radius/6371.e3)
              enddo
           enddo
           if(master) write(*,*) 'Small-Earth terrain adjustment done!'
      endif

      allocate (  zs_g(is:ie, js:je) )
      allocate ( z2(is:ie,js:je) )
      do j=js,je
         do i=is,ie
            zs_g(i,j) = phis(i,j)
            z2(i,j) = phis(i,j)**2
         enddo
      enddo
!--------
! Filter:
!--------
      call global_mx(phis, ng, da_min, da_max)
      zmean  = g_sum(zs_g, is, ie, js, je, ng, area, 1)
      z2mean = g_sum(z2 ,  is, ie, js, je, ng, area, 1)
      if ( master ) then
           write(*,*) 'Before filter ZS min=', da_min, ' Max=', da_max,' Mean=',zmean
           write(*,*) '*** Mean variance *** =', z2mean
      endif

      call remove_ice_sheets (agrid(isd,jsd,1), agrid(isd,jsd,2), oro_g )
      call global_mx(oro_g, ng, da_min, da_max)
      if ( master ) write(*,*) 'ORO min=', da_min, ' Max=', da_max

      call global_mx(area, ng, da_min, da_max)
                                                    call timing_on('Terrain_filter')
! Del-2:
      cd2 = 0.20*da_min
      if ( npx>512 ) then
      if ( npx<=721 ) then
           call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 1, cd2)
      elseif( npx <= 1001 ) then
           call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 2, cd2)
      elseif( npx <= 2001 ) then
           call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 4, cd2)
      else
           call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 6, cd2)
      endif
      endif

! MFCT Del-4:
      if ( npx<=91 ) then
         call del4_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 1)
      elseif( npx<=181 ) then
         call del4_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 2)
      elseif( npx<=361 ) then
         call del4_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 4)
      else
         call del4_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 6)
      endif

! Final pass
      if( npx >= 721 .and. npx<1001 ) then
          call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 1, cd2)
      elseif( npx >= 1001 .and. npx<=2001 ) then
          call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 2, cd2)
      elseif( npx>2001 ) then
          call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, 4, cd2)
      endif

      do j=js,je
         do i=is,ie
            z2(i,j) = phis(i,j)**2
         end do
      end do

      call global_mx(phis, ng, da_min, da_max)
      zmean  = g_sum(phis(is:ie,js:je), is, ie, js, je, ng, area, 1)
      z2mean = g_sum(z2,                is, ie, js, je, ng, area, 1)
      deallocate ( z2 )

      if ( master ) then
           write(*,*) 'After filter Phis min=', da_min, ' Max=', da_max, 'Mean=', zmean
           write(*,*) '*** Mean variance *** =', z2mean
      endif

      do j=js,je
         do i=is,ie
            phis(i,j) =  grav * phis(i,j)
            if ( sgh_g(i,j) <= 0. ) then
                 sgh_g(i,j) = 0.
            else
                 sgh_g(i,j) = sqrt(sgh_g(i,j))
            endif
         end do
      end do

      call global_mx(sgh_g, ng, da_min, da_max)
      if ( master ) write(*,*) 'Before filter SGH min=', da_min, ' Max=', da_max


!-----------------------------------------------
! Filter the standard deviation of mean terrain:
!-----------------------------------------------
      call global_mx(area, ng, da_min, da_max)
      call del4_cubed_sphere(npx, npy, sgh_g, area, dx, dy, dxc, dyc, 1)
      call global_mx(sgh_g, ng, da_min, da_max)
      if ( master ) write(*,*) 'After filter SGH min=', da_min, ' Max=', da_max
      do j=js,je
         do i=is,ie
            sgh_g(i,j) = max(0., sgh_g(i,j))
         enddo
      enddo
                                                    call timing_off('Terrain_filter')


 end subroutine surfdrv


 subroutine del2_cubed_sphere(npx, npy, q, area, dx, dy, dxc, dyc, nmax, cd)
      integer, intent(in):: npx, npy
      integer, intent(in):: nmax
      real(REAL8), intent(in):: cd
    ! INPUT arrays
      real(REAL8), intent(in)::area(isd:ied,  jsd:jed)
      real(REAL8), intent(in)::  dx(isd:ied,  jsd:jed+1)
      real(REAL8), intent(in)::  dy(isd:ied+1,jsd:jed)
      real(REAL8), intent(in):: dxc(isd:ied+1,jsd:jed)
      real(REAL8), intent(in):: dyc(isd:ied,  jsd:jed+1)
    ! OUTPUT arrays
      real(REAL8), intent(inout):: q(is-ng:ie+ng, js-ng:je+ng)
! Local:
      real(REAL8) ddx(is:ie+1,js:je), ddy(is:ie,js:je+1)
      integer i,j,n

      call mpp_update_domains(q,domain,whalo=ng,ehalo=ng,shalo=ng,nhalo=ng)

! First step: average the corners:
      if ( is==1 .and. js==1 ) then
           q(1,1) = (q(1,1)*area(1,1)+q(0,1)*area(0,1)+q(1,0)*area(1,0))  &
                  / (       area(1,1)+       area(0,1)+       area(1,0) )
           q(0,1) =  q(1,1)
           q(1,0) =  q(1,1)
      endif
      if ( (ie+1)==npx .and. js==1 ) then
           q(ie, 1) = (q(ie,1)*area(ie,1)+q(npx,1)*area(npx,1)+q(ie,0)*area(ie,0)) &
                    / (        area(ie,1)+         area(npx,1)+        area(ie,0))
           q(npx,1) =  q(ie,1)
           q(ie, 0) =  q(ie,1)
      endif
      if ( (ie+1)==npx .and. (je+1)==npy ) then
           q(ie, je) = (q(ie,je)*area(ie,je)+q(npx,je)*area(npx,je)+q(ie,npy)*area(ie,npy))  &
                     / (         area(ie,je)+          area(npx,je)+          area(ie,npy))
           q(npx,je) =  q(ie,je)
           q(ie,npy) =  q(ie,je)
      endif
      if ( is==1 .and. (je+1)==npy ) then
           q(1, je) = (q(1,je)*area(1,je)+q(0,je)*area(0,je)+q(1,npy)*area(1,npy))   &
                    / (        area(1,je)+        area(0,je)+         area(1,npy))
           q(0, je) =  q(1,je)
           q(1,npy) =  q(1,je)
      endif


      do n=1,nmax
         if( n>1 ) call mpp_update_domains(q,domain,whalo=ng,ehalo=ng,shalo=ng,nhalo=ng)
         do j=js,je
            do i=is,ie+1
               ddx(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(q(i-1,j)-q(i,j))/dxc(i,j)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               ddy(i,j) = dx(i,j)*(q(i,j-1)-q(i,j))/dyc(i,j) &
                        *0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))
            enddo
         enddo

         do j=js,je
            do i=is,ie
               q(i,j) = q(i,j) + cd/area(i,j)*(ddx(i,j)-ddx(i+1,j)+ddy(i,j)-ddy(i,j+1))
            enddo
         enddo
      enddo

 end subroutine del2_cubed_sphere


 subroutine del4_cubed_sphere(npx, npy, q, area, dx, dy, dxc, dyc, nmax)
      integer, intent(in):: npx, npy, nmax
      real(REAL8), intent(in)::area(isd:ied,  jsd:jed)
      real(REAL8), intent(in)::  dx(isd:ied,  jsd:jed+1)
      real(REAL8), intent(in)::  dy(isd:ied+1,jsd:jed)
      real(REAL8), intent(in):: dxc(isd:ied+1,jsd:jed)
      real(REAL8), intent(in):: dyc(isd:ied,  jsd:jed+1)
      real(REAL8), intent(inout):: q(is-ng:ie+ng, js-ng:je+ng)
! diffusivity
      real(REAL8) :: diff(is-1:ie+1,js-1:je+1)
! diffusive fluxes: 
      real(REAL8) :: fx2(is:ie+1,js:je), fy2(is:ie,js:je+1)
      real(REAL8) :: fx4(is:ie+1,js:je), fy4(is:ie,js:je+1)
      real(REAL8), dimension(isd:ied,jsd:jed):: d2, win, wou 
      real(REAL8), dimension(is:ie,js:je):: qlow, qmin, qmax
      real(REAL8), parameter:: esl = 1.E-20
      integer i,j, n

  do n=1,nmax
      call mpp_update_domains(q,domain)

! First step: average the corners:
      if ( is==1 .and. js==1 ) then
           q(1,1) = (q(1,1)*area(1,1)+q(0,1)*area(0,1)+q(1,0)*area(1,0))  &
                  / (       area(1,1)+       area(0,1)+       area(1,0) )
           q(0,1) =  q(1,1)
           q(1,0) =  q(1,1)
      endif
      if ( (ie+1)==npx .and. js==1 ) then
           q(ie, 1) = (q(ie,1)*area(ie,1)+q(npx,1)*area(npx,1)+q(ie,0)*area(ie,0)) &
                    / (        area(ie,1)+         area(npx,1)+        area(ie,0))
           q(npx,1) =  q(ie,1)
           q(ie, 0) =  q(ie,1)
      endif
      if ( (ie+1)==npx .and. (je+1)==npy ) then
           q(ie, je) = (q(ie,je)*area(ie,je)+q(npx,je)*area(npx,je)+q(ie,npy)*area(ie,npy))  &
                     / (         area(ie,je)+          area(npx,je)+          area(ie,npy))
           q(npx,je) =  q(ie,je)
           q(ie,npy) =  q(ie,je)
      endif
      if ( is==1 .and. (je+1)==npy ) then
           q(1, je) = (q(1,je)*area(1,je)+q(0,je)*area(0,je)+q(1,npy)*area(1,npy))   &
                    / (        area(1,je)+        area(0,je)+         area(1,npy))
           q(0, je) =  q(1,je)
           q(1,npy) =  q(1,je)
      endif

     do j=js,je
        do i=is,ie
           qmin(i,j) = min(q(i,j-1), q(i-1,j), q(i,j), q(i+1,j), q(i,j+1))
           qmax(i,j) = max(q(i,j-1), q(i-1,j), q(i,j), q(i+1,j), q(i,j+1))
        enddo
     enddo

     do j=js-1,je+1
        do i=is-1,ie+1
           diff(i,j) = 0.18*area(i,j) ! area dependency is needed for stretched grid
        enddo
    enddo

!--------------
! Compute del-2
!--------------
!     call copy_corners(q, npx, npy, 1)
      do j=js,je
         do i=is,ie+1
            fx2(i,j) = 0.25*(diff(i-1,j)+diff(i,j))*dy(i,j)*(q(i-1,j)-q(i,j))/dxc(i,j)          &
                           *(sin_sg(i,j,1)+sin_sg(i-1,j,3))
         enddo
      enddo

!     call copy_corners(q, npx, npy, 2)
      do j=js,je+1
         do i=is,ie
            fy2(i,j) = 0.25*(diff(i,j-1)+diff(i,j))*dx(i,j)*(q(i,j-1)-q(i,j))/dyc(i,j) &
                           *(sin_sg(i,j,2)+sin_sg(i,j-1,4))
         enddo
      enddo

      do j=js,je
         do i=is,ie
            d2(i,j) = (fx2(i,j)-fx2(i+1,j)+fy2(i,j)-fy2(i,j+1)) / area(i,j)
! Low order monotonic solution
            qlow(i,j) = q(i,j) + d2(i,j)
            d2(i,j) = diff(i,j)* d2(i,j)
         enddo
      enddo

      call mpp_update_domains(d2,domain)

!---------------------
! Compute del4 fluxes:
!---------------------
!     call copy_corners(d2, npx, npy, 1)
      do j=js,je
         do i=is,ie+1
            fx4(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(d2(i,j)-d2(i-1,j))/dxc(i,j)-fx2(i,j)
         enddo
      enddo

!     call copy_corners(d2, npx, npy, 2)
      do j=js,je+1
         do i=is,ie
            fy4(i,j) = dx(i,j)*(d2(i,j)-d2(i,j-1))/dyc(i,j) &
                     *0.5*(sin_sg(i,j,2)+sin_sg(i,j-1,4))-fy2(i,j)
         enddo
      enddo

!----------------
! Flux limitting:
!----------------
#ifndef NO_MFCT_FILTER
      do j=js,je
         do i=is,ie
            win(i,j) = max(0.,fx4(i,  j)) - min(0.,fx4(i+1,j)) +   &
                       max(0.,fy4(i,  j)) - min(0.,fy4(i,j+1)) + esl
            wou(i,j) = max(0.,fx4(i+1,j)) - min(0.,fx4(i,  j)) +   &
                       max(0.,fy4(i,j+1)) - min(0.,fy4(i,  j)) + esl
            win(i,j) = max(0., qmax(i,j) - qlow(i,j)) / win(i,j)*area(i,j)
            wou(i,j) = max(0., qlow(i,j) - qmin(i,j)) / wou(i,j)*area(i,j)
         enddo
      enddo

      call mpp_update_domains(win,domain, complete=.false.)
      call mpp_update_domains(wou,domain, complete=.true.)

      do j=js,je
         do i=is,ie+1
            if ( fx4(i,j) > 0. ) then
                 fx4(i,j) = min(1., wou(i-1,j), win(i,j)) * fx4(i,j) 
            else
                 fx4(i,j) = min(1., win(i-1,j), wou(i,j)) * fx4(i,j) 
            endif
         enddo
      enddo
      do j=js,je+1
         do i=is,ie
            if ( fy4(i,j) > 0. ) then
                 fy4(i,j) = min(1., wou(i,j-1), win(i,j)) * fy4(i,j) 
            else
                 fy4(i,j) = min(1., win(i,j-1), wou(i,j)) * fy4(i,j) 
            endif
         enddo
      enddo
#endif

! Update:
      do j=js,je
         do i=is,ie
            q(i,j) = qlow(i,j) + (fx4(i,j)-fx4(i+1,j)+fy4(i,j)-fy4(i,j+1))/area(i,j)
         enddo
      enddo

  enddo    ! end n-loop

 end subroutine del4_cubed_sphere

    subroutine map_to_cubed_simple(im, jm, lat1, lon1, q1, grid, agrid, q2, npx, npy)

! Input
      integer, intent(in):: im,jm         ! original dimensions
      integer, intent(in):: npx, npy
      real(REAL8), intent(in):: lat1(jm+1)       ! original southern edge of the cell [-pi/2:pi/2]
      real(REAL8), intent(in):: lon1(im+1)       ! original western edge of the cell [0:2*pi]
      real(REAL8), intent(in):: q1(im,jm)        ! original data at center of the cell

      real(REAL8), intent(in)::  grid(is-ng:ie+ng+1, js-ng:je+ng+1,2)
      real(REAL8), intent(in):: agrid(is-ng:ie+ng,   js-ng:je+ng,  2)

! Output
      real(REAL8), intent(out):: q2(is-ng:ie+ng, js-ng:je+ng) ! Mapped data at the target resolution

! Local
      real(REAL8), allocatable:: qt(:,:), ft(:,:), lon_g(:)
      real(REAL8) lat_g(jm)
      real(REAL8) pc(3), p2(2), pp(3), grid3(3, is:ie+1, js:je+1)
      integer i,j, np
      integer igh
      integer ii, jj, i1, i2, j1, j2
      integer ifirst, ilast
      real(REAL8) qsum, fsum, hsum, lon_w, lon_e, lat_s, lat_n, r2d
      real(REAL8) delg, dlat
!     integer, parameter:: lat_crit = 15             ! 15 * (1/30) = 0.5 deg
      integer:: lat_crit
      integer, parameter:: ig = 2
      real(REAL8) q1_np, q1_sp, f1_np, f1_sp, h1_sp, h1_np, pi5, deg0
      logical inside

      character (len=125) :: errString

      pi5 = 0.5 * pi
      r2d = 180./pi

!     lat_crit = jm / min(360, 4*(npx-1))    ! 0.5  (deg) or larger
      lat_crit = jm / min(720, 8*(npx-1))    ! 0.25 (deg) or larger
      lat_crit = MAX(1,lat_crit)

      dlat = 180./real(jm)

      igh = im/4 + 1

      allocate (    qt(-igh:im+igh,jm) )
      allocate ( lon_g(-igh:im+igh   ) )

! Ghost the input coordinates:
      do i=1,im
         lon_g(i) = 0.5*(lon1(i)+lon1(i+1))
      enddo

      do i=-igh,0
         lon_g(i) = lon_g(i+im)
      enddo
      do i=im+1,im+igh
         lon_g(i) = lon_g(i-im)
      enddo

      do j=1,jm
         lat_g(j) = 0.5*(lat1(j)+lat1(j+1))
      enddo

! Ghost Data
      do j=1,jm
         do i=1,im
            qt(i,j) = q1(i,j)
         enddo
         do i=-igh,0
            qt(i,j) = qt(i+im,j)
         enddo
         do i=im+1,im+igh
            qt(i,j) = qt(i-im,j)
         enddo
      enddo

      do j=js,je+1
         do i=is,ie+1
            call latlon2xyz(grid(i,j,1:2), grid3(1,i,j))
         enddo
      enddo

! Compute values very close to the poles:
!----
! SP:
!----
     qsum = 0.
     hsum = 0.
     np   = 0
     do j=1,lat_crit
        do i=1,im
           np = np + 1
           qsum = qsum + q1(i,j)
        enddo
     enddo
     q1_sp = qsum / real(np)

     hsum = 0.
     do j=1,lat_crit
        do i=1,im
           hsum = hsum + (q1_sp-q1(i,j))**2
        enddo
     enddo
     h1_sp = hsum / real(np)

     if(gid==0) write(*,*) 'SP:', q1_sp
!----
! NP:
!----
     qsum = 0.
     np   = 0
     do j=jm-lat_crit+1,jm
        do i=1,im
           np = np + 1
           qsum = qsum + q1(i,j)
        enddo
     enddo
     q1_np = qsum / real(np)

     hsum = 0.
     do j=jm-lat_crit+1,jm
        do i=1,im
           hsum = hsum + (q1_np-q1(i,j))**2
        enddo
     enddo
     h1_np = hsum / real(np)

     if(gid==0) write(*,*) 'NP:', q1_np
     if(gid==0) write(*,*) 'surf_map: Search started ....'

      do 4444 j=js,je
         do 4444 i=is,ie

            lat_s = min( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2) )
            lat_n = max( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2) )

            if ( r2d*lat_n < (lat_crit*dlat - 90.) ) then
                 q2(i,j) = q1_sp
                 go to 4444
            elseif ( r2d*lat_s > (90. - lat_crit*dlat) ) then
                 q2(i,j) = q1_np
                 go to 4444
            endif

            j1 = nint( (pi5+lat_s)/(pi/real(jm)) ) - ig
            j2 = nint( (pi5+lat_n)/(pi/real(jm)) ) + ig
            j1 = max(1,  j1)
            j2 = min(jm, j2)

            lon_w = min( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) )
            lon_e = max( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) )
            if ( (lon_e - lon_w) > pi ) then
                 i1 = nint( (lon_e-2.*pi)/(2.*pi/real(im)) )
                 i2 = nint(  lon_w       /(2.*pi/real(im)) )
            else
                 i1 = nint( lon_w / (2.*pi/real(im)) )
                 i2 = nint( lon_e / (2.*pi/real(im)) )
            endif

            i1 = max(  -igh, i1 - ig)
            i2 = min(im+igh, i2 + ig)

              np = 0
            qsum = 0.
            do jj=j1,j2
               p2(2) = lat_g(jj)
               do ii=i1,i2
                  p2(1) = lon_g(ii)
                  call latlon2xyz(p2, pp)
                  inside=inside_p4(grid3(1,i,j), grid3(1,i+1,j), grid3(1,i+1,j+1), grid3(1,i,j+1), pp, r1eM2)
                  if ( inside ) then
                      np = np + 1
                      qsum = qsum + qt(ii,jj)
                  endif
               enddo
            enddo

            if ( np > 0 ) then
                 q2(i,j) = qsum / real(np)
            else
                 write(errString, "('Surf_map failed for GID=',i6.6,'(lon,lat)=',6f10.3)") &
                                  gid,agrid(i,j,1)*r2d,agrid(i,j,2)*r2d, lon_w*r2d, lon_e*r2d, lat_s*r2d, lat_n*r2d
                 call error_mesg('map_to_cubed_simple',errString, FATAL)
            endif

4444  continue

      deallocate (   qt )
      deallocate (lon_g )

      end subroutine map_to_cubed_simple

 subroutine map_to_cubed_raw(igh, im, jt, lat1, lon1, zs, ft,  grid, agrid,  &
                              q2, f2, h2, master, npx, npy, jstart, jend)

! Input
      integer, intent(in):: igh, im, jt
      integer, intent(in):: npx, npy
      real(REAL8), intent(in):: lat1(jt+1)       ! original southern edge of the cell [-pi/2:pi/2]
      real(REAL8), intent(in):: lon1(im+1)       ! original western edge of the cell [0:2*pi]
      real(kind=4), intent(in), dimension(-igh:im+igh,jt):: zs, ft
      real(REAL8), intent(in)::  grid(isd:ied+1, jsd:jed+1,2)
      real(REAL8), intent(in):: agrid(isd:ied,   jsd:jed,  2)
      logical, intent(in):: master
      integer, intent(in):: jstart, jend
! Output
      real(REAL8), intent(out):: q2(isd:ied,jsd:jed) ! Mapped data at the target resolution
      real(REAL8), intent(out):: f2(isd:ied,jsd:jed) ! oro
      real(REAL8), intent(out):: h2(isd:ied,jsd:jed) ! variances of terrain
! Local
      real(REAL8) :: lon_g(-igh:im+igh)
      real(REAL8) lat_g(jt), cos_g(jt), e2(2)
      real(REAL8) grid3(3, is:ie+1, js:je+1)
      real(REAL8), dimension(3):: p1, p2, p3, p4, pc, pp
      real(REAL8), dimension(3):: vp_12, vp_23, vp_34, vp_14
      integer i,j, np, k
      integer ii, jj, i1, i2, j1, j2, min_pts
      real(REAL8) th1, aa, asum,  qsum, fsum, hsum, lon_w, lon_e, lat_s, lat_n, r2d
      real(REAL8) qsp, fsp, hsp
      real(REAL8) qnp, fnp, hnp
      real(REAL8) delg, th0, tmp1, prod1, prod2, prod3, prod4
      integer ig_lon, jp
      integer:: lat_crit
      real(REAL8) pi5, pi2

      pi2 = pi + pi
      pi5 = 0.5 * pi
      r2d = 180./pi

      do i=1,im
         lon_g(i) = 0.5*(lon1(i)+lon1(i+1))
      enddo
      do i=-igh,0
         lon_g(i) = lon_g(i+im)
      enddo
      do i=im+1,im+igh
         lon_g(i) = lon_g(i-im)
      enddo

      do j=1,jt
         lat_g(j) = 0.5*(lat1(j)+lat1(j+1))
         cos_g(j) = cos( lat_g(j) )
      enddo

      do j=js,je+1
         do i=is,ie+1
            call latlon2xyz(grid(i,j,1:2), grid3(1,i,j))
         enddo
      enddo

     if(master) write(*,*) 'surf_map: Search started ....'

! stretch_factor * pi5/(npx-1)  / (pi/nlat)
    lat_crit = nint( stretch_factor*real(nlat)/real(npx-1) ) 
    lat_crit = min( jt, max( 4,  lat_crit ) )

    if ( jstart==1 ) then
         write(*,*) gid, 'lat_crit=', r2d*lat_g(lat_crit)
    elseif ( jend==nlat ) then
!        write(*,*) gid, 'lat_crit=', r2d*lat_g(jt-lat_crit+1)
    endif

!----
! SP:
!----
    iF ( jstart == 1 ) then
         asum = 0.
         qsum = 0.
         fsum = 0.
         hsum = 0.
         do j=1,lat_crit
            aa = cos_g(j)
            do i=1,im
               asum = asum + aa
               qsum = qsum + zs(i,j)*aa
               fsum = fsum + ft(i,j)*aa
            enddo
         enddo
         qsp = qsum / asum
         fsp = fsum / asum
         hsum = 0.
         np = 0
         do j=1,lat_crit
            do i=1,im 
               np = np + 1
               hsum = hsum + (qsp-zs(i,j))**2
            enddo
         enddo
         hsp = hsum / real(np)
!        write(*,*) 'SP GID, zs_sp, f_sp, hsp=', gid, qsp, fsp, hsp
     endif
!----
! NP:
!----
    iF ( jend == nlat ) then
         asum = 0.
         qsum = 0.
         fsum = 0.
         hsum = 0.
         do jp=jend-lat_crit+1, jend
            j = jp - jstart + 1
            aa = cos_g(j)
            do i=1,im
               asum = asum + aa
               qsum = qsum + zs(i,j)*aa
               fsum = fsum + ft(i,j)*aa
            enddo
         enddo
         qnp = qsum / asum
         fnp = fsum / asum
         hsum = 0.
         np = 0
         do jp=jend-lat_crit+1, jend
            j = jp - jstart + 1
            do i=1,im 
               np = np + 1
               hsum = hsum + (qnp-zs(i,j))**2
            enddo
         enddo
         hnp = hsum / real(np)
!        write(*,*) 'NP GID, zs_np, f_np, hnp=', gid, qnp, fnp, hnp
     endif

    min_pts = 999999
    do j=js,je
       do i=is,ie

          if ( agrid(i,j,2) < -pi5+stretch_factor*pi5/real(npx-1) ) then
! SP:
               q2(i,j) = qsp
               f2(i,j) = fsp
               h2(i,j) = hsp
               goto 4444
          elseif ( agrid(i,j,2) > pi5-stretch_factor*pi5/real(npx-1) ) then
! NP:
               q2(i,j) = qnp
               f2(i,j) = fnp
               h2(i,j) = hnp
               goto 4444
          endif

          lat_s = min( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
          lat_n = max( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
! Enlarge the search zone:
             delg = max( 0.2*(lat_n-lat_s), pi/real(npx-1), pi2/real(nlat) )
            lat_s = max(-pi5, lat_s - delg)
            lat_n = min( pi5, lat_n + delg)

            j1 = nint( (pi5+lat_s)/(pi/real(nlat)) ) - 1
            if ( lat_s*r2d < (-90.+90./real(npx-1)) ) j1 = 1
            j1 = max(jstart,  j1)

            j2 = nint( (pi5+lat_n)/(pi/real(nlat)) ) + 1
            if ( lat_n*r2d > (90.-90./real(npx-1))  ) j2 = nlat
            j2 = min(jend, j2)

            j1 = j1 - jstart + 1
            j2 = j2 - jstart + 1

            lon_w = min( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) ) 
            lon_e = max( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) )

            if ( (lon_e-lon_w) > pi ) then
                 i1 = -nint( (pi2-lon_e)/pi2 * real(im) ) - 1
                 i2 =  nint( lon_w/pi2 * real(im) ) + 1
            else
                 i1 = nint( lon_w/pi2 * real(im) ) - 1
                 i2 = nint( lon_e/pi2 * real(im) ) + 1
            endif

! Enlarge the search zone:
            ig_lon = max(1, (i2-i1)/8)
            i1 = max(  -igh, i1 - ig_lon)
            i2 = min(im+igh, i2 + ig_lon)

              np = 0
            qsum = 0.
            fsum = 0.
            hsum = 0.
            asum = 0.
!
!            4----------3
!           /          /
!          /    pp    /
!         /          /
!        1----------2
!
            do k=1,3
               p1(k) = grid3(k,i,  j)
               p2(k) = grid3(k,i+1,j)
               p3(k) = grid3(k,i+1,j+1)
               p4(k) = grid3(k,i,j+1)
               pc(k) = p1(k) + p2(k) + p3(k) + p4(k)
            enddo
            call normalize_vect( pc )
 
            th0 = min( v_prod(p1,p3), v_prod(p2, p4) )
            th1 = min( cos_grid, cos(0.25*acos(max(v_prod(p1,p3), v_prod(p2, p4)))))

            call  vect_cross(vp_12, p1, p2)
            call  vect_cross(vp_23, p2, p3)
            call  vect_cross(vp_34, p3, p4)
            call  vect_cross(vp_14, p1, p4)

            prod1 = v_prod(p3, vp_12)
            prod2 = v_prod(p1, vp_23)
            prod3 = v_prod(p1, vp_34)
            prod4 = v_prod(p2, vp_14)

            do jj=j1,j2
                  aa = cos_g(jj)
               e2(2) = lat_g(jj)
               do ii=i1,i2
                  e2(1) = lon_g(ii)
                  call latlon2xyz(e2, pp)
! Check two extrems:
                  tmp1 = v_prod(pp, pc)
! Points that are close to center:
                  if ( tmp1 > th1 ) goto 1111    ! inside
! check to exclude points too far away:
                  if ( tmp1 < th0 ) goto 2222    ! outside
! Check if inside the polygon
                  if ( v_prod(pp, vp_12)*prod1 < 0. ) goto 2222
                  if ( v_prod(pp, vp_23)*prod2 < 0. ) goto 2222
                  if ( v_prod(pp, vp_34)*prod3 < 0. ) goto 2222
                  if ( v_prod(pp, vp_14)*prod4 < 0. ) goto 2222
1111                np = np + 1
                  qsum = qsum + zs(ii,jj)*aa
                  fsum = fsum + ft(ii,jj)*aa
                  hsum = hsum + zs(ii,jj)**2
                  asum = asum + aa
2222              continue
               enddo
            enddo

            if ( np > 0 ) then
                 q2(i,j) = qsum / asum
                 f2(i,j) = fsum / asum
                 h2(i,j) = hsum / real(np) - q2(i,j)**2
                 min_pts = min(min_pts, np)
            else
                 write(*,*) 'min and max lat_g is ', r2d*minval(lat_g), r2d*maxval(lat_g), mpp_pe()
                 write(*,*) 'Surf_map failed for GID=', gid, i,j, '(lon,lat)=', r2d*agrid(i,j,1),r2d*agrid(i,j,2)
                 write(*,*) '[jstart, jend]', jstart, jend
                 stop
            endif
4444  continue
      enddo
    enddo

      if(master) write(*,*) 'surf_map: minimum pts per cell (master PE)=', min_pts
      if ( min_pts <3 ) then
           if(master) write(*,*) 'Warning: too few points used in creating the cell mean terrain !!!'
      endif

 end subroutine map_to_cubed_raw


 logical function inside_p4(p1, p2, p3, p4, pp, th0)

      real(REAL8), intent(in):: p1(3), p2(3), p3(3), p4(3)
      real(REAL8), intent(in):: pp(3)
      real(REAL8), intent(in):: th0
! A * B = |A| |B| cos(angle)
! Local:
      real(REAL8) v1(3), v2(3), vp(3)
      real(REAL8) tmp1
      integer k

      inside_p4 = .false.

! 1st check: to exclude points too far away:
      do k=1,3
         vp(k) = p1(k) + p2(k) + p3(k) + p4(k)
      enddo
      call normalize_vect( vp )

      tmp1 = v_prod(pp, vp)
      if ( tmp1 < th0 ) then           ! outside
           return
      endif

!                                   1st segment: 1---2
      call  vect_cross(vp, p1, p2)
      if ( v_prod(pp, vp)*v_prod(p3, vp) < 0. ) return
!                                   2nd segment: 2---3
      call  vect_cross(vp, p2, p3)
      if ( v_prod(pp, vp)*v_prod(p1, vp) < 0. ) return
!                                   3rd segment: 3---4
      call  vect_cross(vp, p3, p4)
      if ( v_prod(pp, vp)*v_prod(p1, vp) < 0. ) return
!                                   4th segment: 1---4
      call  vect_cross(vp, p1, p4)
      if ( v_prod(pp, vp)*v_prod(p2, vp) < 0. ) return

      inside_p4 = .true.

 end function inside_p4



 subroutine handle_err(status)
#include <netcdf.inc>
      integer          status

      if (status .ne. nf_noerr) then
        print *, nf_strerror(status)
        stop 'Stopped'
      endif

 end subroutine  handle_err

 subroutine remove_ice_sheets (lon, lat, lfrac )
!---------------------------------
! Bruce Wyman's fix for Antarctic
!--------------------------------- 
      real(REAL8), intent(in)    :: lon(isd:ied,jsd:jed), lat(isd:ied,jsd:jed)
      real(REAL8), intent(inout) :: lfrac(isd:ied,jsd:jed)
        
! lon   = longitude in radians
! lat   = latitude in radians
! lfrac = land-sea mask (land=1, sea=0)
            
      integer :: i, j
      real(REAL8) :: dtr, phs, phn
            
      dtr = acos(0.)/90.
      phs = -83.9999*dtr                                  
!     phn = -78.9999*dtr
      phn = -76.4*dtr
            
      do j = js, je
         do i = is, ie
         if ( lat(i,j) < phn ) then
                              ! replace all below this latitude
         if ( lat(i,j) < phs ) then
              lfrac(i,j) = 1.0
              cycle
         endif
                              ! replace between 270 and 360 deg
         if ( sin(lon(i,j)) < 0. .and. cos(lon(i,j)) > 0.) then
              lfrac(i,j) = 1.0
              cycle 
         endif
         endif
         enddo
      enddo
 end subroutine remove_ice_sheets


!#######################################################################
! reads the namelist file, write namelist to log file,
! and initializes constants

subroutine read_namelist

   integer :: unit, ierr, io
!   real(REAL8)    :: dtr, ght

!  read namelist

   if ( file_exist('input.nml')) then
      unit = open_namelist_file ( )
      ierr=1; do while (ierr /= 0)
         read  (unit, nml=surf_map_nml, iostat=io, end=10)
         ierr = check_nml_error(io,'surf_map_nml')
      enddo
 10   call close_file (unit)
   endif

!  write version and namelist to log file

   if (mpp_pe() == mpp_root_pe()) then
     unit = stdlog()
     write (unit, nml=surf_map_nml)
   endif

end subroutine read_namelist

subroutine zonal_mean(im, p, zmean)
! replace p with its zonal mean
   integer, intent(in):: im
   real(kind=4), intent(inout):: p(im)
   real(REAL8), intent(out):: zmean
   integer i

   zmean = 0.
   do i=1,im
      zmean = zmean + p(i)
   enddo
   zmean = zmean / real(im)
   do i=1,im
      p(i) = zmean
   enddo
end subroutine zonal_mean


 end module fv_surf_map_mod
