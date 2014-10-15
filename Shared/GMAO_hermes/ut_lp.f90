!
! Unit test for testing m_lp. 
!
! NOTE: This code could evolve into a general purpose utility to compute
!       surface marine fluxes from a GFIO dyn_vect file.
!

    Program ut_lp

    use m_dyn
    use m_insitu
    use m_lp

    Implicit NONE

    type(dyn_vect) w_f   ! gcm state vector
    type(sim_vect) w_s   ! simulator vector (transformed w_f for interpolation)

    character(len=*), parameter :: myname = 'ut_lp'

    integer, parameter :: im = 144, jm = 91, km = 26, nobs=im*jm*km

    real z1(im,jm), u1(im,jm),  q1(im,jm), t1(im,jm)  ! (u,t,q) at bottom
    real zs(im,jm), ts(im,jm), qs(im,jm)
    real z2, u2(im,jm,km), q2(im,jm,km), t2(im,jm,km) ! (u,t,q) at sfc layer
    real cd(im,jm), ce(im,jm), ct(im,jm), zdl(im,jm)

    real zlevs(km)

    integer i, j, k, kmfv, nymd, nhms, iopt, iu
    integer nconv, nfail, ier
    real amiss, conf, zmin, zmax, dz, dt1, dq1, dt2, dq2
    real t, z, qsat_, theta

!     Saturation specific humidity as in CCM3's trefoce()
!     Units are g/kg
!     ---------------------------------------------------
    qsat_ ( T )    = 640380.0 * 1000. / exp ( 5107.4 / T )
    theta ( T, Z ) = T + 0.01 * Z


!   Vertical levels
!   ---------------
    zmin = 0.
    zmax = 50.
    dz = (zmax - zmin ) / float(km - 1)
    do k = 1, km
       zlevs(k) = zmin + (k-1) * dz
       if ( zlevs(k) .eq. 0 ) zlevs(k) = 0.001
       print *, 'Levels: ', k, zlevs(k)
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
    ts = w_f%ts(1:im,1:jm) ! save this because w_s does not have it yet
    call dyn_clean ( w_f )  ! do not need this anymore

   iopt = 0

!  grads output
!  ------------
   iu = 10
   open(iu,file='ut_lp.grd',form='unformatted')

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

   cd = amiss
   ct = amiss
   ce = amiss
   zdl = amiss


!  Initialize sfc layer package
!  ----------------------------
   call lp_init()

!  For each  ocean gridpoint (based on topography)
!  -----------------------------------------------
   kmfv = w_s%grid%km
   nconv = 0
   nfail = 0
   do j = 1, jm
      do i = 1, im

         zs(i,j) = w_s%ze(i,kmfv+1,j) 
         z1(i,j) = ( w_s%ze(i,kmfv,j) - w_s%ze(i,kmfv+1,j) ) / 2.

!        Quantities at lowest model layer
!        --------------------------------
         u1(i,j) = sqrt( w_s%q3d(i,j,kmfv,1)**2 + w_s%q3d(i,j,kmfv,2)**2 )
         t1(i,j) = w_s%q3d(i,j,kmfv,3)
         q1(i,j) = w_s%q3d(i,j,kmfv,4)
         qs(i,j) = 0.98 * qsat_ ( ts(i,j) ) 
         dt1  = ts(i,j) - theta(t1(i,j),z1(i,j))
         dq1  = qs(i,j) - q1(i,j)         

!        Over land? This is rough
!        ------------------------
         if ( zs(i,j) .gt. 0.5 ) cycle

!        above middle of model lowest layer, set to missing
!        --------------------------------------------------
         if ( z2 .gt. z1(i,j) ) cycle

!        Convert from lowest model layer to surface layer
!        ------------------------------------------------
         do k = 1, km

            z2 = zlevs(k)  ! height to convert to

            call LP_MoveZ ( z1(i,j), u1(i,j), t1(i,j), dq1, dt1, ts(i,j),  &
                            z2, u2(i,j,k), conf, dq2, dt2, &
                            CD(i,j), CT(i,j), CE(i,j), zdL(i,j) )  

            if ( conf .eq. 1.0 ) then
            
               t2(i,j,k) = ts(i,j) - dt2 - 0.01 * z2
               q2(i,j,k) = qs(i,j) - dq2
            
               nconv = nconv + 1
     
            else

               u2(i,j,k) = amiss
               t2(i,j,k) = amiss
               q2(i,j,k) = amiss

               nfail = nfail + 1

            end if

         end do

      end do
   end do

   print *,myname//': no. of converging gridpoints: ', nconv
   print *,myname//': no. of failing    gridpoints: ', nfail, &
              nint(100.0*nfail/(nconv+nfail+0.000001)), '%'



!  write out results
!  ----------------
   call write_grads ( ' zs ', iu, zs, im*jm, 1 )
   call write_grads ( ' ts ', iu, ts, im*jm, 1 )
   call write_grads ( ' qs ', iu, qs, im*jm, 1 )
   call write_grads ( ' z1 ', iu, z1, im*jm, 1 )
   call write_grads ( ' u1 ', iu, u1, im*jm, 1 )
   call write_grads ( ' t1 ', iu, t1, im*jm, 1 )
   call write_grads ( ' q1 ', iu, q1, im*jm, 1 )

   call write_grads ( ' cd ', iu, cd,    im*jm, 1 )
   call write_grads ( ' ct ', iu, ct,    im*jm, 1 )
   call write_grads ( ' ce ', iu, ce,    im*jm, 1 )
   call write_grads ( 'zdl ', iu, zdl,   im*jm, 1 )

   call write_grads ( ' u2 ', iu, u2, im*jm, km )
   call write_grads ( ' t2 ', iu, t2, im*jm, km )
   call write_grads ( ' q2 ', iu, q2, im*jm, km )


   close(iu)

!  Clean up mess
!  -------------
   call insitu_clean ( w_s )

   stop ! all done

 end Program ut_lp


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

