!
!  Simple driver to read FVGCM SST files.
!

   program ut_sst

   character(len=*), parameter :: fname = 'sst.nc'

   character(len=nch)              :: title, source, contact, levunits
   character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)

   real,    allocatable :: lat(:), lon(:), lev(:)
   real,    allocatable :: valid_range(:,:), packing_range(:,:)
   integer, allocatable :: kmvar(:), yyyymmdd(:), hhmmss(:)

   integer :: im, jm, km, lm, nvars, rc
   integer :: l, timinc
   real    :: amiss

   real, allocatable :: sst(:,:)

   integer, parameter :: READ_ONLY = 1
   integer :: fid, err, ngatts

   rc = 0

!  Open the file
!  -------------
   call GFIO_Open ( fname, READ_ONLY, fid, err )
   if ( err .ne. 0 ) then
      rc = 1
      return
   end if

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
   if ( err .ne. 0 ) then
      rc = 2
      return
   end if   
   call init_ ( err )
   if ( err .ne. 0 ) then
      call clean_()
      rc = 3
   end if

!  Get file attributes
!  -------------------
   call GFIO_Inquire ( fid, im, jm, km, lm, nvars,     &
                       title, source, contact, amiss,  &
                       lon, lat, lev, levunits,        &
                       yyyymmdd, hhmmss, timinc,             &
                       vname, vtitle, vunits, kmvar,   &
                       valid_range , packing_range, err )
   if ( err .ne. 0 ) then
      call clean_()
      rc = 4
   end if

   print *, 'yyyymmdd: ', yyyymmdd(1:lm)
   print *, '  hhmmss: ',   hhmmss(1:lm)

   print *
   print *, 'Enter nymd, nhms: '
   read  *, nymd, nhms


   allocate ( sst(im,jm) )

   call GFIO_GetVarT ( fid, 'SST', nymd, nhms, &
                       im, jm, 0, 1, sst, err )
   if ( err .ne. 0 ) then
        print *, 'could not interpolate sst'
        stop
     end if

   call minmax ( 'sst:', sst, im, jm, 1 )

   deallocate(sst)

   stop


   CONTAINS


     subroutine init_ ( err )       ! allocates local memory
     integer err
     allocate ( lat(jm), lon(im), lev(km), yyyymmdd(lm), hhmmss(lm),    & 
              vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
              valid_range(2,nvars), packing_range(2,nvars),             &
              stat=err )
     end subroutine init_

     subroutine clean_()             ! de-allocates local memory
     deallocate ( lat, lon, lev, yyyymmdd, hhmmss,   &
                  vname, vunits, vtitle, kmvar,      &
                  valid_range, packing_range,        &
                  stat=err )
     end subroutine clean_


   end program ut_sst

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



