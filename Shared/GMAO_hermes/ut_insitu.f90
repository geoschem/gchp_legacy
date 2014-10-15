!
! Unit test for m_ods and portions of the ODS library
!

    Program ut_insitu    

    use m_dyn
    use m_insitu

    type(dyn_vect) w_f   ! gcm state vector
    type(sim_vect) w_s   ! simulator vector (transformed w_f for interpolation)

    character(len=*), parameter :: myname = 'ut_insitu'

    integer, parameter :: im = 144, jm = 91, km = 20, nobs=im*jm*km

    real lat(nobs), lon(nobs), lev(nobs), conf(nobs), u(nobs), v(nobs)

    real plevs(km)

    plevs = (/ 1000., 925.,  850., 700., 500., 400., 300., 250., 200., &
               150.,  100.,  70., 50., 30., 20., 10., 5., 2., 1., 0.4 /)


  
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
!!!       call dyn_stat ( 6, w_f )
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

!   Simulate coordinates
!   --------------------
    n = 0
    ampl = 0.01   ! perturbation amplitude
    ampl = 0.0
    do k = 1, km
       z = plevs(k)
       do j = 1, jm
          y = -90. + (j-1) * 2
          do i = 1, im
             x = -180. + (i-1) * 2.5

             pert = ampl * ranf()  ! perturb coords by this amount

             n = n + 1
             lon(n) = x + pert
             lat(n) = y + pert
             lev(n) = exp(log(z) + pert)

             lon(n) = max ( -180., lon(n) )
             lon(n) = min ( +180., lon(n) )

             lat(n) = max ( -90., lat(n) )
             lat(n) = min ( +90., lat(n) )

          end do
       end do
    end do
    print *, myname//': created fake obs locations'

   iopt = 0

!  grads output
!  ------------
   iu = 10
   open(iu,file='ut_insitu.grd',form='unformatted')


!  test mass fields
!  ----------------
   nobs1 = im*jm
   call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'mslp', iopt, u, conf, ier )
   if ( ier .ne. 0 ) then
      print *, 'could not simulate mslp'
   else
      call minmax ( 'mslp:', u, nobs1, 1, 1 )
      call write_grads ( 'mslp', iu, u, conf, plevs, im*jm, 1 )
   end if

   call Insitu_Mass ( w_s, lon, lat, lev, nobs1, 'slpf', iopt, u, conf, ier )
   if ( ier .ne. 0 ) then
      print *, 'could not simulate slpf'
   else
      call minmax ( 'slpf:', u, nobs1, 1, 1 )
      call write_grads ( 'slpf', iu, u, conf, plevs, im*jm, 1 )
   end if

   call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'hght', iopt, u, conf, ier )
   if ( ier .ne. 0 ) then
      print *, 'could not simulate hght'
   else
      call minmax ( 'hght:', u, nobs, 1, 1 )
      call write_grads ( 'hght', iu, u, conf, plevs, im*jm, km )
   end if

   call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'tmpu', iopt, u, conf, ier )
   if ( ier .ne. 0 ) then
      print *, 'could not simulate tmpu'
   else
      call minmax ( 'tmpu:', u, nobs, 1, 1 )
      call write_grads ( 'tmpu', iu, u, conf, plevs, im*jm, km )
   end if

   call Insitu_Mass ( w_s, lon, lat, lev, nobs, 'mixr', iopt, u, conf, ier )
   if ( ier .ne. 0 ) then
      print *, 'could not simulate mixr'
   else
      call minmax ( 'mixr:', u, nobs, 1, 1 )
      call write_grads ( 'mixr', iu, u, conf, plevs, im*jm, km )
   endif

!  test winds
!  ----------
   call Insitu_Wind ( w_s, lon, lat, lev, nobs, 'wind', iopt, u, v, conf, ier )
   if ( ier .ne. 0 ) then
         print *, 'could not simulate winds'
   else
      call minmax ( 'uwnd:', u, nobs, 1, 1 )
      call minmax ( 'vwnd:', v, nobs, 1, 1 )
      call write_grads ( 'uwnd', iu, u, conf, plevs, im*jm, km )
      call write_grads ( 'vwnd', iu, v, conf, plevs, im*jm, km )
   end if

   close(iu)

!  Clean up mess
!  -------------
   call insitu_clean ( w_s )

   stop ! all done

 end Program ut_insitu


!................................................................

      subroutine write_grads ( name, iu, x, conf, plevs, m, n )

      character(len=*) name
      real x(m,n), conf(m,n), plevs(n)

      amiss = 1.e+15
      do j = 1, n
        ngood = 0
        nbad  = 0
        nsus  = 0
        do i = 1, m
            if ( conf(i,j) .eq. 0. ) then
               nbad = nbad + 1
               x(i,j) = amiss
            else if ( conf(i,j) .eq. 1. ) then
               ngood = ngood + 1
            else if ( conf(i,j) .ne. 0. ) then
               nsus = nsus + 1
               x(i,j) = amiss 
            end if
         end do
         if ( nbad .ne. 0 .or. nsus .ne. 0 ) &
         print *, name // ': lev, bad, suspect = ', plevs(j), nbad, nsus
      end do

      do j = 1, n
         write(iu) ( x(i,j), i = 1, m )
      end do

      end


!................................................................


      subroutine ranset ( iseed )
      integer iseed, idum
      real    dummy
      idum = - max(abs(iseed),1)
      dummy = ran2(idum)
      return
      end

      function ranf() ! random numbers between -1 and 1
      integer idum
      save idum
      data idum / 0 /
      ranf = 2 * ( ran2(idum) - 0.5 )
      return
      end

      function ran2(idum)
      integer idum


!     Uniform random number generator based on Numerical Recipes 2.
!     Returns a uniform random deviate between 0.0 and 1.0.
!     Set idum to any negative number to reinitialize the sequence.
!

      integer im1,im2,imm1,ia1,ia2,iq1,iq2,ir1,ir2,ntab,ndiv
      real am,eps,rnmx
      parameter (im1=2147483563,im2=2147483399,am=1./im1,imm1=im1-1, &
     ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211,ir2=3791, &
     ntab=32,ndiv=1+imm1/ntab,eps=1.2e-7,rnmx=1.-eps)
      integer idum2,j,k,iv(ntab),iy
      save iv,iy,idum2
      data idum2/123456789/, iv/ntab*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do  j=ntab+8,1,-1
          k=idum/iq1
          idum=ia1*(idum-k*iq1)-k*ir1
          if (idum.lt.0) idum=idum+im1
          if (j.le.ntab) iv(j)=idum
       end do
        iy=iv(1)
      endif
      k=idum/iq1
      idum=ia1*(idum-k*iq1)-k*ir1
      if (idum.lt.0) idum=idum+im1
      k=idum2/iq2
      idum2=ia2*(idum2-k*iq2)-k*ir2
      if (idum2.lt.0) idum2=idum2+im2
      j=1+iy/ndiv
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+imm1
      ran2 = min(am*iy,rnmx)
      return
      end


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

