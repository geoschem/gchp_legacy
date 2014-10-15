!
! Unit test for insitu_ocean from m_insitu.
!

    Program ut_ocean

    use m_dyn
    use m_insitu
    use m_lp

    Implicit NONE

    type(dyn_vect) w_f   ! gcm state vector
    type(sim_vect) w_s   ! simulator vector (transformed w_f for interpolation)

    character(len=*), parameter :: myname = 'ut_ocean'

    integer, parameter :: im = 144, jm = 91, km = 26, nobs=im*jm*km

    real p1(nobs), z1(nobs), u1(nobs),  q1(nobs), t1(nobs) ! (u,t,q) at bottom
    real ts(nobs), qs(nobs)
    real u2(nobs), q2(nobs), t2(nobs) ! (u,t,q) at sfc layer

    real lat(nobs), lon(nobs), lev(nobs), conf(nobs)

    real zmin, zmax, dz, zlevs(km)
    real amiss 

    integer i, j, k, n, nymd, nhms, iopt, iu, ier, kmfv


!   Vertical levels
!   ---------------
    zmin = 0.
    zmax = 50.
    dz = (zmax - zmin ) / float(km - 1)
    do k = 1, km
       zlevs(k) = zmin + (k-1) * dz
       if ( zlevs(k) .eq. 0 ) zlevs(k) = 0.001
!       print *, 'Levels: ', k, zlevs(k)
    end do

!   Read state variables
!   --------------------
    nymd = 19971221
    nhms = 0
    print *, myname//': reading gridded fields'
    call dyn_get ( 'dyn.HDF', nymd, nhms, w_f, ier )
    if ( ier .ne. 0 ) then
       print *, myname//': could not read gridded fields, ier =', ier
       stop
    else
       print *, myname//': just read gridded fields'
    end if

!   Initialize simulator vector (needed for insitu calculations)
!   -----------------------------------------------------------
    call Insitu_Init ( w_f, w_s, ier )
    if ( ier .ne. 0 ) then
       print *, myname//': could not created sim vect, ier =', ier
       stop
    else
       print *, myname//': just initialized simulator vector'
    end if
    call dyn_clean ( w_f )  ! do not need this anymore


!   Generate coordinates
!   --------------------
    n = 0
    do k = 1, km
       do j = 1, jm
          do i = 1, im
             n = n + 1
             lon(n) = (i-1) * 2.5
             lat(n) = -90. + (j-1)*2.0
             lev(n) = zlevs(k)   
          end do
       end do
    end do
  


   iopt = 0

!  grads output
!  ------------
   iu = 10
   open(iu,file='ut_ocean.grd',form='unformatted')

!  Initialize all fields to undef
!  ------------------------------
   amiss = 1.E+15 
   qs = amiss
   u1 = amiss
   t1 = amiss
   q1 = amiss
   u2 = amiss
   t2 = amiss
   q2 = amiss

!  Go for it
!  ---------
   iopt = 0
   call Insitu_Ocean ( w_s, lon, lat, lev, nobs, iopt,  &   
                       u2, qs, ts,                     & 
                       u1, q1, t1, p1, z1, conf, ier,   &
                       q2, t2 )                 

   if ( ier .ne. 0 ) then
      print *, myname//': error from Insitu_Ocean, rc = ', ier
      stop
   endif


!  Maskout non-oceanic points as in ut_lp
!  --------------------------------------
    kmfv = w_s%grid%km
    n = 0
    do k = 1, km
       do j = 1, jm
          do i = 1, im
            n = n + 1
            if ( w_s%ze(i,kmfv+1,j) .gt. 0.5 .or. conf(n) .ne. 1.0 ) then
                u2(n) = amiss
                q2(n) = amiss
                t2(n) = amiss
             end if
          end do
       end do
    end do


!  write out results
!  ----------------
   qs = 0.98 * qs
   call write_grads ( ' ts ', iu, ts, im*jm, 1 )
   call write_grads ( ' qs ', iu, qs, im*jm, 1 )
   call write_grads ( ' z1 ', iu, z1, im*jm, 1 )
   call write_grads ( ' u1 ', iu, u1, im*jm, 1 )
   call write_grads ( ' t1 ', iu, t1, im*jm, 1 )
   call write_grads ( ' q1 ', iu, q1, im*jm, 1 )

   call write_grads ( ' u2 ', iu, u2, im*jm, km )
   call write_grads ( ' t2 ', iu, t2, im*jm, km )
   call write_grads ( ' q2 ', iu, q2, im*jm, km )


   close(iu)

!  Clean up mess
!  -------------
   call insitu_clean ( w_s )

   stop ! all done

 end Program ut_ocean


!................................................................

      subroutine write_grads ( name, iu, x, m, n )

      character(len=*) name
      real x(m,n)

      call minmax (name, x, m, n, 1)

      do j = 1, n
         write(iu) ( x(i,j), i = 1, m )
      end do

      end


!................................................................


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

