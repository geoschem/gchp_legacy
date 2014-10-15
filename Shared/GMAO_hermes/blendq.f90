   program blend

!
!   Simple code to blend the moisture from 2 native restart files
!   above a certain layer.
!
!   A. da SIlva, March 2000.

   implicit NONE

   character(len=255) :: geosfn, amipfn

   real, allocatable :: ps(:,:), delp(:,:,:), u(:,:,:), v(:,:,:), pt(:,:,:)
   real, allocatable :: q(:,:,:,:)
   real*4, allocatable :: q32(:,:)

   integer lu, im, jm, km, k, k1, k2, ios, nstep, nymd, nhms, nstep1, nymd1, nhms1
   real factor

   lu = 10

   print *
   print *, '   Blending SPHU Restart in the Stratosphere'
   print *
   print *, 'Enter dimentions: im, jm, km '
   read  *, im, jm, km

   print *, 'Enter name of AMIP file (e.g., "d_rst.amip")'
   read(*,'(a)') amipfn

   print *, 'Enter name of interpolated GEOS file (e.g., "d_rst.geos")'
   read(*,'(a)') geosfn

   print *
   print *, 'Output files will be named "d_rst.blend" and "q_rst.grads"'
   print *

   print *, 'Enter k1, k2 for blending. For k<=k1 we will have q(amip), '
   print *, ' for k>=k2 we will have q(geos)'
   read  *, k1, k2

   if ( k1 .gt. k2 .or. k1 .lt. 1 .or. k2 .gt. km ) then
      print *, 'invalid k1, k2 = ', k1, k2
      call exit(1)
   end if

!  Allocated memory
!  ----------------
   allocate ( ps(im,jm), delp(im,jm,km), u(im,jm,km), v(im,jm,km), &
              pt(im,jm,km), q(im,jm,km,3), q32(im,jm), stat=ios )
   if ( ios .ne. 0 ) then
      print *, 'cannot allocate memory'
      call exit(1)
   endif

!  Read AMIP file
!  --------------
   open(lu,file=trim(amipfn),form='unformatted')
   read(lu) nstep1, nymd1, nhms1
   read(lu) ps,delp,u,v,pt
   read(lu) q(1:im,1:jm,1:km,1)
   close(lu)

!  Read GEOS file
!  --------------
   open(lu,file=trim(geosfn),form='unformatted')
   read(lu) nstep, nymd, nhms
   read(lu) ps,delp,u,v,pt
   read(lu) q(1:im,1:jm,1:km,2)
   close(lu)

   if ( nstep .ne. nstep1 .or. &
        nymd .ne. nymd1   .or. &
        nhms .ne. nhms1 ) then
      print *, 'incompatible headers '
      print *, 'amip: ', nstep1, nymd1, nhms1
      print *, 'geos: ', nstep, nymd, nhms
   end if

!  Perform the blending
!  --------------------
   q(1:im,1:jm,1:k1 ,3) = q(1:im,1:jm,1:k1,1)  ! AMIP
   q(1:im,1:jm,k2:km,3) = q(1:im,1:jm,k2:km,2) ! GEOS
   do k = k1, k2
     factor = float(k1 - k) / float(k1 - k2)
     q(1:im,1:jm,k,3) = q(1:im,1:jm,k,1)                       &
                      + factor * ( q(1:im,1:jm,k,2) - q(1:im,1:jm,k,1) )
   end do
      
!  write blended restart
!  ---------------------
   open(lu,file='d_rst.blend',form='unformatted')
   write(lu) nstep, nymd, nhms
   write(lu) ps,delp,u,v,pt
   write(lu) q(1:im,1:jm,1:km,3)
   close(lu)

!  Write grads output for checking
!  -------------------------------
   open(lu,file='q_rst.bin',form='unformatted')
   do k = 1, km
      q32(1:im,1:jm) = q(1:im,1:jm,k,1)
      write(lu) q32
   end do
   do k = 1, km
      q32(1:im,1:jm) = q(1:im,1:jm,k,2)
      write(lu) q32
   end do
   do k = 1, km
      q32(1:im,1:jm) = q(1:im,1:jm,k,3)
      write(lu) q32
   end do
   close(lu)

   stop
   end program blend

