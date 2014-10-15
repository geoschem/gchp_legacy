program grid_setup

! ifort -I/usr/include -I/usr/local/include -assume byterecl -O3 -i4 -r8 -w95 -o grid_setup.x grid_setup.F90 -L/usr/local/lib -lnetcdf
!./grid_setup.x
! --- Setup Symmetric Cubed-Sphere Grid for Input to FVCORE ---
! 
! NPX (X- dimension) =?
!45
! NPY (Y- dimension) =?
!45
! Input Grid Type (Conformal, Spring, Elliptic, Gnomonic) =?
!Elliptic
! Input Grid File =?
!'/home/wmp/Cubed-Sphere/Grid_Generator/GRIDS/Neu-0.350_Dir-1.000_grid_0045x0045.dat'
! ROT_LON (Lon- Rotation[deg]) =[0]
!-8
! ROT_LAT (Lat- Rotation[deg]) =[0]
!0
! Reading: 
! /home/wmp/Cubed-Sphere/Grid_Generator/GRIDS/Neu-0.350_Dir-1.000_grid_0045x0045.
! dat
! npx=          45  npy=          45
! -0.785398163397448      -0.615479708670387     
!  0.785398163397448       0.615479708670387     
! Writing 
! Cubed-Sphere_Elliptic_grid_0045x0045.dat                                       
!
! '/home/wmp/Cubed-Sphere/Grid_Generator/GRIDS/Neu-0.350_Dir-1.000_grid_0045x0045.dat'
! '/home/sjl/mgrid/spring_dyn/Spring_Grid'
! '/home/wmp/GRIDS/generate_grids/generate_horizontal_grid/'
!
      implicit      none

      integer, parameter :: ntiles=6
      integer :: npx,npy
      character*120 :: grid_type, grid_file_in, grid_file_out
      integer :: fileLun

      integer, parameter :: ndims=2
      integer, parameter :: ng=0
      real*8, allocatable :: grid_R8(:,:)
      real  , allocatable :: grid(:,:,:,:)
      real  , allocatable :: area(:,:,:)
      real  , allocatable :: dx(:,:,:)
      real  , allocatable :: dy(:,:,:)

      real :: rot_lon
      real :: rot_lat

      real :: x1,y1,z1
      real :: x2,y2,z2
      integer :: i,j,k,n

      real :: pi, todeg, torad
      pi = 4.0*atan(1.0)
      todeg = 180.0/pi
      torad = pi/180.0

      write(*,*) '--- Setup Symmetric Cubed-Sphere Grid for Input to FVCORE ---'
      write(*,*) ''
      write(*,*) 'NPX (X- dimension) =?'
      read(*,*) npx
      write(*,*) 'NPY (Y- dimension) =?'
      read(*,*) npy
      write(*,*) 'Input Grid Type (Conformal, Spring, Elliptic, Gnomonic) =?'
      read(*,*) grid_type
      write(*,*) 'Input Grid File =?'
      read(*,*) grid_file_in

      rot_lon=0.0
      rot_lat=0.0
      write(*,*) 'ROT_LON (Lon- Rotation[deg]) =[0]'
      read(*,*) rot_lon
      write(*,*) 'ROT_LAT (Lat- Rotation[deg]) =[0]'
      read(*,*) rot_lat

      write(grid_file_out,200) trim(grid_type),npx,npy
 200  format('Cubed-Sphere_',A,'_grid_',i4.4,'x',i4.4,'.dat')

      allocate( grid_R8(npx  ,npy  ) )
      allocate(    grid(npx  ,npy  ,ndims,ntiles) )
      allocate(    area(npx-1,npy-1,ntiles) )
      allocate(      dx(npx-1,npy  ,ntiles) )
      allocate(      dy(npx  ,npy-1,ntiles) )

      print*, 'Reading: ', trim(grid_file_in)
      if (TRIM(grid_type) == 'Spring') then
         fileLun=16
         open(fileLun, file=grid_file_in, form='unformatted', access='sequential')
         read(fileLun) grid_R8  ! Lons
         do j=1,npy
            do i=1,npx
               grid(i,j,1,1) = grid_R8(i,j) - pi  ! -PI to PI
            enddo
         enddo
         read(fileLun) grid_R8  ! Lats
         do j=1,npy
            do i=1,npx
               grid(i,j,2,1) = grid_R8(i,j)
            enddo
         enddo
         close(unit=fileLun)
      elseif (TRIM(grid_type) == 'Elliptic') then
         fileLun=16
         open(unit=fileLun,file=grid_file_in, form='unformatted', access='sequential')
         do n=1,ntiles
            do k=1,ndims
               read(unit=fileLun) grid_R8(1:npx,1:npy)
               do j=1,npy
                  do i=1,npx
                     grid(i,j,k,n) = grid_R8(i,j)
                     if (ABS(grid(i,j,k,n)) < 1.e-10) grid(i,j,k,n) = 0.0
                  enddo
               enddo
            enddo
         enddo
         close(unit=fileLun)
      elseif (TRIM(grid_type) == 'Gnomonic') then
         do n=1,ntiles
            call get_gnomonic_grid(npx, npy, grid(:,:,:,n), n)
         enddo
      else
         call get_conformal_grid(grid_type,grid_file_in,grid,area,dx,dy,ng,npx,npy,ndims,ntiles)
      endif

  ! mirror_grid assumes that the tile=1 is centered on equator and greenwich meridian Lon[-pi,pi] 
      call mirror_grid(grid,ng,npx,npy,ndims,ntiles)

      do n=1,ntiles
         do j=1,npy
            do i=1,npx
               x1 = grid(i,j,1,n)
               y1 = grid(i,j,2,n)
               z1 = 1.0
               call rot_3d( 3, x1, y1, z1, rot_lon*torad, x2, y2, z2, 1)  ! rotate about the z-axis
               call rot_3d( 2, x2, y2, z2, rot_lat*torad, x1, y1, z1, 1)  ! rotate about the x-axis
             ! fix Poles
               if ( ABS(y1)>=(PI/2.0)-1.e-5 ) then
                  x1=0.0
               endif
             ! fix dateline/greenwich meridian
               if ( (ABS(x1)<=1.e-5) .or. (ABS(x1)>=(PI)-1.e-5) ) then
                  if (ABS(x1)<=1.e-5) x1 = 0.0
                  if (ABS(x1)>=(PI)-1.e-5) x1 = PI
               endif
               grid(i,j,1,n) = x1
               grid(i,j,2,n) = y1
            enddo
         enddo
      enddo

!---SJL-----------------------------------------
      write(*,*) 'npx=', npx, ' npy=', npy
      write(*,*) grid(1,1,1,1),   grid(1,1,2,1)
      write(*,*) grid(npx,npy,1,1), grid(npx,npy,2,1)
!---SJL-----------------------------------------

      write(*,*) 'Writing ', grid_file_out
      fileLun=15
      open(unit=fileLun,file=grid_file_out, form='unformatted', access='sequential', &
           status='unknown')
      do n=1,ntiles
         do k=1,2
            do j=1,npy
               do i=1,npx
                  if (k==1) grid(i,j,k,n) = grid(i,j,k,n) + pi
                  grid_R8(i,j) = grid(i,j,k,n)
               enddo
            enddo
            write(unit=fileLun) grid_R8(1:npx,1:npy)
         enddo
         write(*,*) 'npx=', npx, ' npy=', npy
         write(*,*) grid(1,1,1,n),   grid(1,1,2,n)
         write(*,*) grid(npx,npy,1,n), grid(npx,npy,2,n)
      enddo
      close(unit=fileLun)

end

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     get_gnomonic_grid :: get a face of the cubed grid in xyz coordinates
!
      subroutine get_gnomonic_grid(nsx, nsy, grid, region)
      implicit      none

         integer, intent(IN) :: nsx, nsy
         real  , intent(INOUT) :: grid(nsx, nsy, 3)
         integer, intent(IN) :: region

         real   :: alpha(nsx)
         real   :: beta(nsy)
         real   :: capX, capY, capZ
         integer :: i,j

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0

         do i=1,nsx
            alpha(i) = pi/4. * ( REAL(i-1.)/REAL(nsx-1.) - 0.5) * 2.0
         enddo

         do j=1,nsy
            beta(j) = pi/4. * ( REAL(j-1.)/REAL(nsy-1.) - 0.5) * 2.0
         enddo

         if (region == 1) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = capZ
                  grid(i,j,2) = capX*capZ
                  grid(i,j,3) = capY*capZ
               enddo
            enddo

         elseif (region == 2) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = capZ*(-1.0)
                  grid(i,j,2) = (-1.0)*capX*capZ
                  grid(i,j,3) = capY*capZ
               enddo
            enddo

         elseif (region == 3) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = (-1.0)*capX*capZ
                  grid(i,j,2) = capZ
                  grid(i,j,3) = capY*capZ
               enddo
            enddo

         elseif (region == 4) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = capX*capZ
                  grid(i,j,2) = (-1.0)*capZ
                  grid(i,j,3) = capY*capZ
               enddo
            enddo

         elseif (region == 5) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = (-1.0)*capY*capZ
                  grid(i,j,2) = capX*capZ
                  grid(i,j,3) = capZ
               enddo
            enddo

         elseif (region == 6) then

            do i=1,nsx
               do j=1,nsy
                  capX = TAN(alpha(i))
                  capY = TAN(beta(j))
                  capZ = (1 + capX*capX + capY*capY)**(-0.5)
                  grid(i,j,1) = capY*capZ
                  grid(i,j,2) = capX*capZ
                  grid(i,j,3) = (-1.0)*capZ
               enddo
            enddo

         endif

      end subroutine get_gnomonic_grid
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!
      subroutine get_conformal_grid(grid_name,grid_file,grid_global,area_global,dx_global,dy_global,ng,npx,npy,ndims,nregions)
      implicit      none
#include "netcdf.inc"

         character*120, intent(IN) :: grid_name, grid_file
         integer, intent(IN)    :: ng,npx,npy,ndims,nregions
         real   , intent(INOUT) :: grid_global(1-ng:npx  +ng,1-ng:npy  +ng,ndims,1:nregions)
         real   , intent(INOUT) :: area_global(1:npx-1,1:npy-1,1:nregions)
         real   , intent(INOUT) ::   dx_global(1:npx-1,1:npy  ,1:nregions)
         real   , intent(INOUT) ::   dy_global(1:npx  ,1:npy-1,1:nregions)
         integer :: i,j,n,n1,n2,ntile
                  
         integer :: ncid
         integer :: rcode
         integer :: varID
         integer :: ibeg(2), iend(2)
         character(150) :: ncfile

         real :: gg_tmp(1-ng:npx  +ng,1-ng:npy  +ng,ndims,1:nregions)

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0

            write(*,298) trim(grid_file),npx,npy,nregions,TRIM(grid_name)

 298     format('Using conformal grid : ',A,i4.4,'x',i4.4,'x',i1.1,'-',A)
 299     format(A,'/',i4.4,'x',i4.4,'x',i1.1,'-',A,'/horizontal_grid.',i1.1,'.nc')

           do ntile=1,nregions
              write(ncfile,299) trim(grid_file),npx,npy,nregions,TRIM(grid_name),ntile
! Open netCDF file.
              ncid=ncopn(ncfile,ncnowrit,rcode)
! Get Geographic X/Y values
              ibeg(1) = 1
              ibeg(2) = 1
              iend(1) = npx
              iend(2) = npy
              varID = NCVID(ncid, 'x', rcode)! get ID
              call ncvgt (ncid, varID, ibeg, iend, gg_tmp(1:npx,1:npy,1:1,ntile:ntile), rcode)
              varID = NCVID(ncid, 'y', rcode)! get ID
              call ncvgt (ncid, varID, ibeg, iend, gg_tmp(1:npx,1:npy,2:2,ntile:ntile), rcode)
              ibeg(1) = 1
              ibeg(2) = 1
              iend(1) = npx-1
              iend(2) = npy-1
              varID = NCVID(ncid, 'area', rcode)! get ID
              call ncvgt (ncid, varID, ibeg, iend, area_global(1:npx-1,1:npy-1,ntile:ntile), rcode)
              ibeg(1) = 1
              ibeg(2) = 1
              iend(1) = npx-1
              iend(2) = npy
              varID = NCVID(ncid, 'dx', rcode)! get ID
              call ncvgt (ncid, varID, ibeg, iend, dx_global(1:npx-1,1:npy,ntile:ntile), rcode)
              ibeg(1) = 1
              ibeg(2) = 1
              iend(1) = npx
              iend(2) = npy-1
              varID = NCVID(ncid, 'dy', rcode)! get ID
              call ncvgt (ncid, varID, ibeg, iend, dy_global(1:npx,1:npy-1,ntile:ntile), rcode)
! Close netCDF file
              call ncclos(ncid, rcode)
              gg_tmp(:,:,:,ntile:ntile) = gg_tmp(:,:,:,ntile:ntile)*torad
           enddo

           grid_global(:,:,:,:) = gg_tmp(:,:,:,:)

      end subroutine get_conformal_grid
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!
      subroutine mirror_grid(grid_global,ng,npx,npy,ndims,nregions)
      implicit      none
         integer, intent(IN)    :: ng,npx,npy,ndims,nregions
         real   , intent(INOUT) :: grid_global(1-ng:npx  +ng,1-ng:npy  +ng,ndims,1:nregions)
         integer :: i,j,n,n1,n2,nreg
         real :: x1,y1,z1, x2,y2,z2, ang

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0

!
!    Mirror Across the 0-longitude
!
         nreg = 1
         do j=1,ceiling(npy/2.)
            do i=1,ceiling(npx/2.)

            x1 = 0.25 * (ABS(grid_global(i        ,j        ,1,nreg)) + &
                         ABS(grid_global(npx-(i-1),j        ,1,nreg)) + &
                         ABS(grid_global(i        ,npy-(j-1),1,nreg)) + &
                         ABS(grid_global(npx-(i-1),npy-(j-1),1,nreg)))
            grid_global(i        ,j        ,1,nreg) = SIGN(x1,grid_global(i        ,j        ,1,nreg))
            grid_global(npx-(i-1),j        ,1,nreg) = SIGN(x1,grid_global(npx-(i-1),j        ,1,nreg))
            grid_global(i        ,npy-(j-1),1,nreg) = SIGN(x1,grid_global(i        ,npy-(j-1),1,nreg))
            grid_global(npx-(i-1),npy-(j-1),1,nreg) = SIGN(x1,grid_global(npx-(i-1),npy-(j-1),1,nreg))

            y1 = 0.25 * (ABS(grid_global(i        ,j        ,2,nreg)) + &
                         ABS(grid_global(npx-(i-1),j        ,2,nreg)) + &
                         ABS(grid_global(i        ,npy-(j-1),2,nreg)) + &
                         ABS(grid_global(npx-(i-1),npy-(j-1),2,nreg)))
            grid_global(i        ,j        ,2,nreg) = SIGN(y1,grid_global(i        ,j        ,2,nreg))
            grid_global(npx-(i-1),j        ,2,nreg) = SIGN(y1,grid_global(npx-(i-1),j        ,2,nreg))
            grid_global(i        ,npy-(j-1),2,nreg) = SIGN(y1,grid_global(i        ,npy-(j-1),2,nreg))
            grid_global(npx-(i-1),npy-(j-1),2,nreg) = SIGN(y1,grid_global(npx-(i-1),npy-(j-1),2,nreg))

           ! force dateline/greenwich-meridion consitency
            if (mod(npx,2) /= 0) then
              if ( (i==1+(npx-1)/2.0) ) then
                 grid_global(i,j        ,1,nreg) = 0.0
                 grid_global(i,npy-(j-1),1,nreg) = 0.0
              endif
            endif

            enddo
         enddo

         do nreg=2,nregions
           do j=1,npy
             do i=1,npx

               x1 = grid_global(i,j,1,1)
               y1 = grid_global(i,j,2,1)
               z1 = 1.0

               if (nreg == 2) then
                  ang = -90.*torad
                  call rot_3d( 3, x1, y1, z1, ang, x2, y2, z2, 1)  ! rotate about the z-axis
               elseif (nreg == 3) then
                  ang = -90.*torad
                  call rot_3d( 3, x1, y1, z1, ang, x2, y2, z2, 1)  ! rotate about the z-axis
                  ang = 90.*torad
                  call rot_3d( 1, x2, y2, z2, ang, x1, y1, z1, 1)  ! rotate about the x-axis
                  x2=x1
                  y2=y1
                  z2=z1

           ! force North Pole and dateline/greenwich-meridion consitency
                  if (mod(npx,2) /= 0) then
                     if ( (i==1+(npx-1)/2.0) .and. (i==j) ) then
                        x2 = 0.0
                        y2 = pi/2.0
                     endif
                     if ( (j==1+(npy-1)/2.0) .and. (i < 1+(npx-1)/2.0) ) then
                        x2 = 0.0
                     endif
                     if ( (j==1+(npy-1)/2.0) .and. (i > 1+(npx-1)/2.0) ) then
                        x2 = pi
                     endif
                  endif

               elseif (nreg == 4) then
                  ang = -180.*torad
                  call rot_3d( 3, x1, y1, z1, ang, x2, y2, z2, 1)  ! rotate about the z-axis
                  ang = 90.*torad
                  call rot_3d( 1, x2, y2, z2, ang, x1, y1, z1, 1)  ! rotate about the x-axis
                  x2=x1
                  y2=y1
                  z2=z1

               ! force dateline/greenwich-meridion consitency
                  if (mod(npx,2) /= 0) then
                    if ( (j==1+(npy-1)/2.0) ) then
                       x2 = pi
                    endif
                  endif

               elseif (nreg == 5) then
                  ang = 90.*torad
                  call rot_3d( 3, x1, y1, z1, ang, x2, y2, z2, 1)  ! rotate about the z-axis
                  ang = 90.*torad
                  call rot_3d( 2, x2, y2, z2, ang, x1, y1, z1, 1)  ! rotate about the y-axis
                  x2=x1
                  y2=y1
                  z2=z1
               elseif (nreg == 6) then
                  ang = 90.*torad
                  call rot_3d( 2, x1, y1, z1, ang, x2, y2, z2, 1)  ! rotate about the y-axis
                  ang = 0.*torad
                  call rot_3d( 3, x2, y2, z2, ang, x1, y1, z1, 1)  ! rotate about the z-axis
                  x2=x1
                  y2=y1
                  z2=z1

           ! force South Pole and dateline/greenwich-meridion consitency
                  if (mod(npx,2) /= 0) then
                     if ( (i==1+(npx-1)/2.0) .and. (i==j) ) then
                        x2 = 0.0
                        y2 = -pi/2.0
                     endif
                     if ( (i==1+(npx-1)/2.0) .and. (j > 1+(npy-1)/2.0) ) then
                        x2 = 0.0
                     endif
                     if ( (i==1+(npx-1)/2.0) .and. (j < 1+(npy-1)/2.0) ) then
                        x2 = pi
                     endif
                  endif

               endif

               grid_global(i,j,1,nreg) = x2
               grid_global(i,j,2,nreg) = y2

              enddo
            enddo
          enddo

      end subroutine mirror_grid
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!                 
!     rot_3d :: rotate points on a sphere in xyz coords (convert angle from
!               degrees to radians if necessary)
!
      subroutine rot_3d(axis, x1in, y1in, z1in, angle, x2out, y2out, z2out, convert)
      implicit      none
                  
         integer, intent(IN) :: axis         ! axis of rotation 1=x, 2=y, 3=z
         real , intent(IN)    :: x1in, y1in, z1in
         real , intent(IN)    :: angle        ! angle to rotate in radians
         real , intent(OUT)   :: x2out, y2out, z2out
         integer, intent(IN), optional :: convert ! if present convert input point
                                                  ! from spherical to cartesian, rotate, 
                                                  ! and convert back
                     
         real  :: c, s
         real  :: x1,y1,z1, x2,y2,z2

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0

         if ( present(convert) ) then
           call spherical_to_cartesian(x1in, y1in, z1in, x1, y1, z1)
         else  
           x1=x1in
           y1=y1in
           z1=z1in
         endif

         c = COS(angle)
         s = SIN(angle)

         SELECT CASE(axis)

            CASE(1)
               x2 =  x1
               y2 =  c*y1 + s*z1
               z2 = -s*y1 + c*z1
            CASE(2)
               x2 = c*x1 - s*z1
               y2 = y1
               z2 = s*x1 + c*z1
            CASE(3)
               x2 =  c*x1 + s*y1
               y2 = -s*x1 + c*y1
               z2 = z1
            CASE DEFAULT
              write(*,*) "Invalid axis: must be 1 for X, 2 for Y, 3 for Z."

         END SELECT

         if ( present(convert) ) then
           call cartesian_to_spherical(x2, y2, z2, x2out, y2out, z2out)
         else
           x2out=x2
           y2out=y2
           z2out=z2
         endif

      end subroutine rot_3d
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!      
!     cartesian_to_spherical :: convert from xyz coordinates to spherical coords
!      
      subroutine cartesian_to_spherical(x, y, z, lon, lat, r)
      implicit      none
         
         real , intent(IN)  :: x, y, z
         real , intent(OUT) :: lon, lat, r
         real  :: S

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0

         r = SQRT(x*x + y*y + z*z)
         lon = ATAN2(y,x)
         lat = ACOS(z/r) - pi/2.

      end subroutine cartesian_to_spherical
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------
         
!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     spherical_to_cartesian :: convert from spheircal coordinates to xyz coords
! 
      subroutine spherical_to_cartesian(lon, lat, r, x, y, z)
      implicit      none
    
         real , intent(IN)  :: lon, lat, r
         real , intent(OUT) :: x, y, z

         real :: pi, todeg, torad
         pi = 4.0*atan(1.0)
         todeg = 180.0/pi
         torad = pi/180.0
 
         x = r * COS(lon) * SIN(lat + pi/2.)
         y = r * SIN(lon) * SIN(lat + pi/2.)
         z = r * COS(lat + pi/2.)
    
      end subroutine spherical_to_cartesian
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------


