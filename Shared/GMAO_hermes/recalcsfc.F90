!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1    !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ReCalcSFC:  Corrects certain near surface fields after vortex relocation
!
     Program ReCalcSFC 

! !INTERFACE:
!
!     See usage()
!
! !USES:
!

      use m_dyn          ! dynamics state vector
      use m_insitu       ! insitu vector
      use m_GFIO_PutFld  ! quick put to gfio file
      use m_GFIO_GetFld  ! quick get from gfio file
      use m_FileResolv   ! file resolver
      use m_StrTemplate  ! string template expansion

      use m_die          ! error handling
      use m_zeit         ! timer

      implicit NONE
                                                                                                                     
! !DESCRIPTION: 
!
! !REVISION HISTORY:
!
!  11Jul2006  Todling     Extracted from A. da Silva's ana5sfc.x
!  24Nov2009  RT/Zhang    Updated interface to dyn_get for GEOS-5; d2a handled accordingly
!  25Jun2010  RT/Meta     Adjust so upperair and surface longitudes are consistent,
!                           calculations are made in g4 [0-360] coordinates
!  13Jul2016  RT/Meta     Pass dyntype to insitu init
!
!EOP
!-----------------------------------------------------------------------
                                                                                                                     
      character(len=*), parameter :: myname = 'ReCalcSFC'
                                                                                                                     
      integer, parameter :: nvars = 8
      
      character(len=4), parameter :: myvars(nvars) =  &
                      (/'U2M ','V2M ','Q2M ','T2M ','U10M','V10M','Q10M','T10M'/)

      character(len=255) obkgeta, rbkgeta, bkgsfc

      integer :: im, jm, km
      integer :: rc
      integer :: nymd, nhms, dyntype
      logical :: debug
      real, allocatable :: sfc(:,:,:)           ! surface bkg fields (input and output)
      real, allocatable :: lon(:), coslon(:), sinlon(:) ! longitude, and its cosine and sine


!     Dynamics vectors
!     ---------------
      type (dyn_vect) :: w_b                    ! upper-air background

!     Bottom vectors
!     --------------
      type btm_vect
           real, pointer ::  zb(:,:)  ! height of bottom layer
           real, pointer ::  tb(:,:)  ! temperature at bottom layer
           real, pointer ::  qb(:,:)  ! specific hum. at bottom layer
           real, pointer ::  ub(:,:)  ! zonal wind at bottom layer
           real, pointer ::  vb(:,:)  ! meridional wind at bottom layer
           real, pointer ::  ts(:,:)  ! skin tempeature
           real, pointer ::  qs(:,:)  ! saturation q at sfc: q_s(ts)

           real, pointer :: u2m(:,:), u10m(:,:)  ! u wind
           real, pointer :: v2m(:,:), v10m(:,:)  ! v wind
           real, pointer :: t2m(:,:), t10m(:,:)  ! temp 
           real, pointer :: q2m(:,:), q10m(:,:)  ! temp 
      end type btm_vect

      type(btm_vect) :: x_f      ! original background surface fields
      type(btm_vect) :: x_a      ! relocated background surface fields

      call zeit_ci ( 'recalcsfc' )
                                                                                                                     
!     Parse command line
!     ------------------
      call Init ( obkgeta, rbkgeta, bkgsfc, nymd, nhms, dyntype, debug )
                                                                                                                     
!     Get needed fields from relocated background [in GEOS-4 coords (0-360)]
!     ----------------------------------------------------------------------
      if (dyntype == 5) then
         call dyn_get ( obkgeta, nymd, nhms, w_b, rc, timidx=1, &
              vectype=dyntype, forceflip=.true. )
      else
         call dyn_get ( obkgeta, nymd, nhms, w_b, rc, timidx=1, vectype=dyntype )
      end if

      im =  w_b%grid%im
      jm =  w_b%grid%jm
      km =  w_b%grid%km

      allocate ( lon(im), coslon(im), sinlon(im) )
      lon(:)   = w_b%grid%lon(:)

!     Set 2D fields from bkg.eta fields
!     ---------------------------------
      call malloc_ (x_f,im,jm)
      call eta_set_( w_b, x_f )

      call dyn_clean ( w_b )
      if(debug) print *, 'Extracted low-level fields from original upper-air background'
      if(debug) write(6,'(a,i8.8,1x,i6.6)') 'Date and Time read: ', nymd, nhms

!     Get needed fields from relocated background [in GEOS-4 coords (0-360)]
!     ----------------------------------------------------------------------
      if (dyntype == 5) then
         call dyn_get ( rbkgeta, nymd, nhms, w_b, rc, timidx=1, &
              vectype=dyntype, forceflip=.true. )
      else
         call dyn_get ( rbkgeta, nymd, nhms, w_b, rc, timidx=1, vectype=dyntype )
      end if

      if( w_b%grid%im .ne. im .or. w_b%grid%jm .ne. jm ) & 
          call die(myname,'Inconsistent upper-air background dims')

!     Set 2D fields from bkg.eta fields
!     ---------------------------------
      call malloc_ (x_a,im,jm)
      call eta_set_( w_b, x_a )

      call dyn_clean ( w_b )
      if(debug) print *, 'Extracted low-level fields from relocated upper-air background'
      if(debug) write(6,'(a,i8.8,1x,i6.6)') 'Date and Time read: ', nymd, nhms

!     Get original low-level surface wind
!     -----------------------------------
      allocate ( sfc(im,jm,nvars) )
      call gfio_getfld ( trim(bkgsfc), nymd, nhms, im, jm, nvars, myvars, sfc )

      if(debug) print *, 'Extracted surface fields from original sfc background'

!     Compute near surface meteorology
!     --------------------------------
      call  uvtq_set_ ( im, jm, 2.0, &
                        sfc(1,1,1), sfc(1,1,2), sfc(1,1,3), sfc(1,1,4), &
                        sfc(1,1,1), sfc(1,1,2), sfc(1,1,3), sfc(1,1,4)  )
      call  uvtq_set_ ( im, jm, 10.0, &
                        sfc(1,1,5), sfc(1,1,6), sfc(1,1,7), sfc(1,1,8), &
                        sfc(1,1,5), sfc(1,1,6), sfc(1,1,7), sfc(1,1,8)  )

      if(debug) print *, 'Recalculated near surface fields'

!     Flip fields for consistency with GEOS-5 GCM surface output
!     ----------------------------------------------------------
      call hflip ( sfc, im, jm, nvars ) 

!     Get original low-level surface wind
!     -----------------------------------
      call gfio_putfld ( trim(bkgsfc), nymd, nhms, im, jm, nvars, myvars, sfc )

!     Clean up
!     --------
      deallocate ( sfc )
      deallocate ( lon, coslon, sinlon )
      call mdalloc_(x_a)
      call mdalloc_(x_f)

      if(debug) print *, 'Overwrote surface file: ', trim(bkgsfc)
      call zeit_co ( 'recalcsfc' )

      call exit(0)

      CONTAINS

      subroutine malloc_ ( x, im, jm )
        implicit NONE
        type(btm_vect), intent(inout) :: x
        integer, intent(in) :: im, jm ! horizontal dimensions
        integer ios
        if ( debug ) print *, ' [] allocating memory'
        allocate ( x%zb(im,jm), x%ub(im,jm), x%vb(im,jm),  x%tb(im,jm), x%qb(im,jm), &
                   x%ts(im,jm),  x%qs(im,jm), &
                   stat = ios )
        if ( ios .ne. 0 ) &
             call die(myname,'cannot allocate analysis 2D fields' )

        allocate ( x%u2m(im,jm), x%u10m(im,jm), x%v2m(im,jm), x%v10m(im,jm), &
                   x%t2m(im,jm), x%t10m(im,jm), x%q2m(im,jm), x%q10m(im,jm), &
                   stat = ios )
        if ( ios .ne. 0 ) &
             call die(myname,'cannot allocate analysis 2D fields' )

      end subroutine malloc_

      subroutine mdalloc_ ( x )
        implicit NONE
        type(btm_vect), intent(inout) :: x
        integer ios
        if ( debug ) print *, ' [] allocating memory'
        deallocate ( x%u2m, x%u10m, x%v2m, x%v10m, &
                     x%t2m, x%t10m, x%q2m, x%q10m, &
                     stat = ios )
        if ( ios .ne. 0 ) &
             call die(myname,'cannot deallocate analysis 2D fields' )
                                                                                                                                 
        deallocate ( x%zb, x%ub, x%vb,  x%tb, x%qb, &
                     x%ts,  x%qs, &
                     stat = ios )
        if ( ios .ne. 0 ) &
             call die(myname,'cannot deallocate analysis 2D fields' )
                                                                                                                                 
      end subroutine mdalloc_

subroutine eta_set_(w,x)      ! set needed quantities from analysis
   implicit NONE
   type(dyn_vect), intent(inout) :: w
   type(btm_vect), intent(inout)   :: x
   type(sim_vect) :: w_s
   integer        :: rc, i, j, k, kb, ncount
   real :: zmin, pi, d2r
   if ( debug ) print *, ' [] Setting analysis fields...'
    ncount = 0
    call Insitu_Init ( w, w_s, rc, dyntype=dyntype )
    if ( rc .ne. 0 ) call die ( myname, 'cannot create simulator vector' )
    do j = 1, jm
       do i = 1, im                        ! determine bottom layer
          zmin = 10. ! at least 10m above sfc (used to be zpd+z0m)
          do k = km, 1, -1 
             kb = k

             x%zb(i,j) = (w_s%ze(i,k,j) - w_s%ze(i,k+1,j))/2.
             if ( x% zb(i,j) .ge. zmin ) then
                 exit ! avoid very thin shaved layers
             end if		 
          end do
          if ( kb /= km ) then
               ncount = ncount + 1
          end if

!         x%pb(i,j)  = 100 * ( w_s%pe(i,kb,j) + w_s%pe(i,kb+1,j) ) / 2
          x%tb(i,j)  = w_s%q3d(i,j,kb,iqt)
          x%qb(i,j)  = w_s%q3d(i,j,kb,iqq) / 1000. ! g/kg -> kg/kg
          x%ub(i,j)  = w_s%q3d(i,j,kb,iqu)
          x%vb(i,j)  = w_s%q3d(i,j,kb,iqv)
          x%ts(i,j)  = w_s%q2d(i,j,iqts)

       end do
    end do

    ! not sure if the kb logic above is still needed
    if ( ncount > 0 ) then
       print *, " <> number of times kb /= km: ", ncount
    end if

!   From D to A
!   -----------
    if ( dyntype==4 ) then
      pi = 4. * atan(1.)
      d2r = pi / 180.
      coslon = cos(d2r * lon)
      sinlon = sin(d2r * lon)
      if ( debug ) print *, ' D Grid: u_b = ', minval(x%ub), maxval(x%ub)  
      if ( debug ) print *, ' D Grid: v_b = ', minval(x%vb), maxval(x%vb)  
      call d2a ( x%ub, x%vb, im, jm, 1, jm, coslon, sinlon )
      if ( debug ) print *, ' A Grid: u_b = ', minval(x%ub), maxval(x%ub)  
      if ( debug ) print *, ' A Grid: v_b = ', minval(x%vb), maxval(x%vb)  
      print *, 'Converted low-level winds to A-grid'
    endif

    call Insitu_Clean ( w_s )

end subroutine eta_set_ 

subroutine uvtq_set_ ( im, jm, z, uz_f, vz_f, qz_f, tz_f, &
                                  uz_a, vz_a, qz_a, tz_a )
!
! Computes u,v,tq, at 2 and 10m (any level z, really)
!
   integer, intent(in) :: im, jm
   real, intent(in)                    ::  z ! 2 or 10
   real, intent(in),  dimension(im,jm) ::  uz_f, vz_f, qz_f, tz_f
   real, intent(out), dimension(im,jm) ::  uz_a, vz_a, qz_a, tz_a

   real eps, gamma, factor, thz_f, thz_a, thb_f, thb_a
   integer i, j
   integer :: nu0, nt0, nq0, ntb
   integer :: nu1, nt1, nq1

   x_a%qs = x_f%qs ! uses bkg q_s as analysis does modify this

   eps = tiny( x_f%u10m(1,1) )
   gamma = 0.01 ! K/m
   nu0 = 0
   nt0 = 0
   nq0 = 0
   nu1 = 0
   nt1 = 0
   nq1 = 0
   ntb = 0

   do j = 1, jm
      do i = 1, im

!        Winds
!        -----
         factor = ( sqrt(uz_f(i,j)**2 + vz_f(i,j)**2) + eps ) / &
                  ( sqrt(x_f%ub(i,j)**2 + x_f%vb(i,j)**2) + eps ) 
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         uz_a(i,j) = factor * x_a%ub(i,j)
         vz_a(i,j) = factor * x_a%vb(i,j)

         if ( factor .eq. 0.0 ) nu0 = nu0 + 1
         if ( factor .eq. 1.0 ) nu1 = nu1 + 1

!        Temperature (calculation is in terms of theta ~= T + gamma * z)
!        ---------------------------------------------------------------
         thz_f = tz_f(i,j)   + gamma * z
         thb_f = x_f%tb(i,j) + gamma * x_f%zb(i,j)
         factor = ( thz_f - x_f%ts(i,j) + eps ) / ( thb_f - x_f%ts(i,j) + eps )
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         thb_a = x_a%tb(i,j) + gamma * x_a%zb(i,j)
         thz_a = (1.0 - factor ) * x_a%ts(i,j) + factor * thb_a
         tz_a(i,j) = thz_a - gamma * x_a%zb(i,j)

         if ( factor .eq. 0.0 ) nt0 = nt0 + 1
         if ( factor .eq. 1.0 ) nt1 = nt1 + 1

!        Humidity 
!        --------
         factor = (   qz_f(i,j) - x_f%qs(i,j) + eps ) / &
                  ( x_f%qb(i,j) - x_f%qs(i,j) + eps ) 
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         qz_a(i,j) = ( 1.0 - factor ) * x_a%qs(i,j) + factor * x_a%qb(i,j)

         if ( factor .eq. 0.0 ) nq0 = nq0 + 1
         if ( factor .eq. 1.0 ) nq1 = nq1 + 1

      end do
   end do

#ifdef NEVER
   if ( debug ) then

      factor = 100. / float ( im*jm )
      print *
      print *, ' [] uvtq: nu = ', nu0, nu1, nint(factor*nu0), nint(factor*nu1)
      print *, ' [] uvtq: nq = ', nq0, nq1, nint(factor*nq0), nint(factor*nq1)
      print *, ' [] uvtq: nt = ', nt0, nt1, nint(factor*nt0), nint(factor*nt1)
      print *, ' [] uvtq: ntb= ', ntb

   end if
#endif

 end subroutine uvtq_set_

    end program ReCalcSFC
!.......................................................................
      subroutine hflip ( q,im,jm,lm )
      implicit none
      integer  im,jm,lm,i,j,L
      real   q(im,jm,lm),dum(im)
      do L=1,lm
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j,L)
         dum(i+im/2) = q(i,j,L)
      enddo
         q(:,j,L) = dum(:)
      enddo
      enddo
      return
      end

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!noBOP
! !IROUTINE: Init --- Initialize SfcReloc
!
! !INTERFACE:
!
      subroutine Init ( obkgeta, rbkgeta, bkgsfc, nymd, nhms, dyntype, debug )
 
! !USES:
 
      use m_die
      implicit NONE
 
! !OUTPUT PARAMETERS:

      character(len=*), intent(inout) :: obkgeta
      character(len=*), intent(inout) :: rbkgeta
      character(len=*), intent(inout) ::  bkgsfc

      integer, intent(out) :: nymd
      integer, intent(out) :: nhms
      integer, intent(out) :: dyntype
 
! !DESCRIPTION: Parses command line.
!
! !REVISION HISTORY:
!
!     11Jul2006   Todling   Initial code.
!     24Nov2009   RT/Zhang  Add -g5 for GEOS-5 support.
!
!noEOP
!-----------------------------------------------------------------------
 
      character(len=*), parameter :: myname = 'Init'
 
      integer i, iarg, argc, iargc, n
      logical debug
      character(len=255) argv
 
      print *
      print *, '     ---------------------------------------------------------------'
      print *, '      SfcReloc - near surface fields recalculation after relocation '
      print *, '     ---------------------------------------------------------------'
      print *
 
      nymd = -1
      nhms = -1
      dyntype = 4
 
!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 3 ) call usage()
                                                                                                                     
      iarg = 0
      n = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) then
              exit
         endif
         call GetArg ( iArg, argv )
         if (index(argv,'-v' ) .gt. 0 ) then
             debug = .true.
         else if (index(argv,'-g5' ) .gt. 0 ) then
             dyntype = 5
         else
            n = n + 1
            if ( n .eq. 1 ) then
                 rbkgeta = trim(argv)
            else if ( n .eq. 2 ) then
                 obkgeta = trim(argv)
            else if ( n .eq. 3 ) then
                 bkgsfc  = trim(argv)
            else
             call usage()
          end if
       end if
      end do
 
      if ( n .eq. 0 ) then
         call warn ( myname, 'missing background files' )
         call usage()
      end if
 
      end subroutine Init

!.......................................................................
                                                                                                                     
      subroutine usage()
      use m_die
      print *
      print *, 'USAGE:  '
      print *
      print *, ' recalcsfc.x [-v]  rlc.eta bkg.eta bkg.sfc'
      print *
      print *, 'where'
      print *
      print *, ' bkg.eta    input original  upper-air background file name'
      print *, ' rlc.eta    input relocated upper-air background file name'
      print *, ' bkg.sfc    surface file name; on input original, on output relocated'
      print *
      print *, ' -v        verbose mode'
      print *
      call die('recalcsfc.x','not enough arguments' )
      end subroutine usage

      subroutine d2a(u,v,im,jm,jfirst,jlast,coslon,sinlon)
! This is primarily for turbulence package designed for A-grid.
! Also used for output to A-grid.
! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1; removed fvcore.h
! WS 99.07.26 : Added jfirst and jlast as arguments

      implicit none
      integer im, jm, jfirst, jlast
! WS 99.07.26 : u must be ghosted N2S1
      real u(im,jm),v(im,jm),ua(im,jm),va(im,jm),coslon(im),sinlon(im)

      integer   imh
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1
      real un, vn, us, vs

! Convert D-grid winds to A-grid
! u --> ua, v --> va

      real utmp(im,jm),vtmp(im,jm)

      imh = im/2

      js = 3
      jn = jm - js + 1
      im1 = im-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 45 j=jn+1,jm-1
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jm-1
50    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,im-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) - &
                       (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) - &
                       (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) - &
                       (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) - &
                       (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) - &
                       (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
! Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i) &
              + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i) &
              + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      us = us/im
      vs = vs/im

      do i=1,imh
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN
! Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      un = un/im
      vn = vn/im

      do i=1,imh
      ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
      va(i,jm) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jm) = -ua(i,jm)
      va(i+imh,jm) = -va(i,jm)
      enddo

      ENDIF

      do 100 j=2,jm-1
      do 100 i=1,im
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)

      u = ua
      v = va

      return
      end subroutine d2a




