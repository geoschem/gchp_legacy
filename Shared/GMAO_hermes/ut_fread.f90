!
! Simple driver to test reading of gfio prog.sig files.
!

   Program ut_fread

   real, pointer :: sige(:)
   real, pointer :: phis(:,:)
   real, pointer :: ps(:,:)
   real, pointer :: uwnd(:,:,:)
   real, pointer :: vwnd(:,:,:)
   real, pointer :: tmpu(:,:,:)
   real, pointer :: sphu(:,:,:)
   real, pointer :: rh(:,:,:)

   real ptop

   integer im, jm, km, rc

   character(len=*), parameter :: fname = 'fg.sig.hdf', myname='ut_fread'


!  Get dimensions
!  --------------
   call ProgSig_Dim ( fname, im, jm, km, rc )
   if ( rc .ne. 0 ) then
      print *, myname//': cannot get dims'
      stop
   else
      print *, 'dimensions are: ', im, jm, km
   end if

!  Allocate memory
!  ---------------
   allocate ( phis(im,jm), ps(im,jm), uwnd(im,jm,km), vwnd(im,jm,km), &
              tmpu(im,jm,km), sphu(im,jm,km), rh(im,jm,km), sige(km+1), &
              stat = rc )
   if ( rc .ne. 0 ) then
      print *, myname//': cannot allocate memory'
      stop
   else
     print *, myname // ': mem allocated'
   end if

!  read the data
!  -------------
   nymd = 19971221
   nhms = 0
   call ProgSig_Read ( fname, nymd, nhms,  &
                       im, jm, km, ptop, sige,  &
                       phis, ps, uwnd, vwnd, tmpu, sphu, rh, rc )
   if ( rc .ne. 0 ) then
      print *, myname//': cannot read data', rc
   end if

!  Summary
!  -------
   print *
   print *, 'ptop = ', ptop
   call minmax ( 'sige', sige, 1,  1, km )
   call minmax ( 'phis', phis, im, jm, 1 )
   call minmax ( 'ps',   ps,   im, jm, 1 )
   call minmax ( 'uwnd', uwnd, im, jm, km )
   call minmax ( 'vwnd', vwnd, im, jm, km )
   call minmax ( 'tmpu', tmpu, im, jm, km )
   call minmax ( 'sphu', sphu, im, jm, km )
   call minmax ( 'rh',   rh,   im, jm, km )

 end Program ut_fread

      subroutine minmax (name, f, m, n, l)

      implicit         none

      character*(*)      name

      integer          m, n, l
      integer          i, j, k

      real             f(m,n,l)
      real             fmax
      real             fmin
      real mean
      real big
      parameter (big = 1.e14)
      integer count
      real mean
      logical hasmiss 

      hasmiss = .false.
      fmax = - big
      fmin = + big
      mean = 0.
      count = 0

      do k = 1, l
        do j = 1, n
          do i = 1, m
            if( abs(f(i,j,k)) .lt. big ) then
                fmax = max(fmax,f(i,j,k))
                fmin = min(fmin,f(i,j,k))
                mean = mean + f(i,j,k)
                count = count + 1
            else
                hasmiss = .true.
            endif
          end do
        end do
      end do

      if( count .ne. 0 ) mean = mean / count

      if ( hasmiss ) then
      write(6,*) name // ' max, min, mean = ', fmax, fmin, mean, ' M'
      else
      write(6,*) name // ' max, min, mean = ', fmax, fmin, mean 
      endif

      return
      end


