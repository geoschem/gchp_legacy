      program  main
      use MAPL_ConstantsMod
      implicit none

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****  Program to remap GEOS-5 FV & MOIST restarts in the vertical ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************


      character*256 dynrst, moistrst, topo, datmodynrst
      character*256, allocatable :: other_rst(:)

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms
      integer im,jm,lm_in,lm_out,nt,rc
      real    undef, kappa, grav

! restart variables and topography
! --------------------------------
      real*8, allocatable ::   dp_in(:,:,:)
      real*8, allocatable ::    u_in(:,:,:)
      real*8, allocatable ::    v_in(:,:,:)
      real*8, allocatable ::  thv_in(:,:,:)
      real*8, allocatable ::   pk_in(:,:,:)
      real*8, allocatable ::  ple_in(:,:,:)
      real*8, allocatable ::    q_in(:,:,:,:)
      real*8, allocatable ::   ps_in(:,:)
      real*8, allocatable ::   ak_in(:)
      real*8, allocatable ::   bk_in(:)
      real*8, allocatable ::    phis(:,:)
      real*8, allocatable ::  pke_in(:,:,:)

      real*8, allocatable ::   dp_out(:,:,:)
      real*8, allocatable ::    u_out(:,:,:)
      real*8, allocatable ::    v_out(:,:,:)
      real*8, allocatable ::  thv_out(:,:,:)
      real*8, allocatable ::   pk_out(:,:,:)
      real*8, allocatable ::  pke_out(:,:,:)
      real*8, allocatable ::  ple_out(:,:,:)
      real*8, allocatable ::    q_out(:,:,:,:)
      real*8, allocatable ::   ps_out(:,:)
      real*8, allocatable ::   ak_out(:)
      real*8, allocatable ::   bk_out(:)

! Two extras if we are on the cubed sphere grid
! ---------------------------------------------
      real*8, allocatable ::   dz_out(:,:,:)
      real*8, allocatable ::    w_out(:,:,:)

      real*4, allocatable ::  dum(:,:)

      real*4, allocatable ::   dum1(:)
      real*4, allocatable ::  dum3e(:,:,:)
      real*4, allocatable ::  dum3l(:,:,:)

! MAT New fields
! --------------
      
      real*8, allocatable ::    t_in(:,:,:)
      real*8, allocatable ::   om_in(:,:,:)
      real*8, allocatable ::  oml_in(:,:,:)
      real*8, allocatable :: pref_in(:)

      real*8, allocatable ::    t_out(:,:,:)
      real*8, allocatable ::   om_out(:,:,:)
      real*8, allocatable ::  oml_out(:,:,:)
      real*8, allocatable :: pref_out(:)

      real*8, allocatable ::   dp_fv_in(:,:,:)
      real*8, allocatable ::    u_fv_in(:,:,:)
      real*8, allocatable ::    v_fv_in(:,:,:)
      real*8, allocatable ::  thv_fv_in(:,:,:)
      real*8, allocatable ::   pk_fv_in(:,:,:)
      real*8, allocatable ::  ple_fv_in(:,:,:)
      real*8, allocatable ::   ps_fv_in(:,:)
      real*8, allocatable ::  pke_fv_in(:,:,:)
      real*8, allocatable ::  oml_fv_in(:,:,:)

      real*8, allocatable ::   dp_fv_out(:,:,:)
      real*8, allocatable ::    u_fv_out(:,:,:)
      real*8, allocatable ::    v_fv_out(:,:,:)
      real*8, allocatable ::  thv_fv_out(:,:,:)
      real*8, allocatable ::   pk_fv_out(:,:,:)
      real*8, allocatable ::  ple_fv_out(:,:,:)
      real*8, allocatable ::   ps_fv_out(:,:)
      real*8, allocatable ::  pke_fv_out(:,:,:)
      real*8, allocatable ::  oml_fv_out(:,:,:)

      character*256, allocatable :: arg(:)
      character*8    date
      character*1    char
      character*2    hour
      character*4    cim,cjm,clm
      integer m,n,nargs,iargc,i,j,L
      integer num,num_other_rst,nbeg,nend
      integer, allocatable :: nt_other(:)
      logical  verbose

      real eps, dums
      real*8 dums8
      integer im_fv, jm_fv

! **********************************************************************
! ****                      Initialize Filenames                    ****
! **********************************************************************

       verbose = .false.
        lm_out = -999
         undef = 1.0e15
        dynrst = 'fvcore_internal_rst'
   datmodynrst = 'datmodyn_internal_rst'
      moistrst = 'moist_internal_rst'
      num_other_rst = 0

         nargs = iargc()
      if(nargs == 0 ) call usage()
      allocate ( arg(nargs) )
      do n=1,nargs
      call getarg(n,arg(n))
      enddo
      do n=1,nargs
             if( trim(arg(n)).eq.'-h'        ) call usage()
             if( trim(arg(n)).eq.'-help'     ) call usage()
             if( trim(arg(n)).eq.'-H'        ) call usage()
             if( trim(arg(n)).eq.'-Help'     ) call usage()
             if( trim(arg(n)).eq.'-v'        ) verbose = .true.
             if( trim(arg(n)).eq.'-verbose'  ) verbose = .true.

             if( trim(arg(n)).eq.'-dynrst'   ) then
                                   dynrst  = trim(arg(n+1))
             endif

             if( trim(arg(n)).eq.'-datmodynrst'   ) then
                                   datmodynrst  = trim(arg(n+1))
             endif

             if( trim(arg(n)).eq.'-moistrst' ) then
                                   moistrst = trim(arg(n+1))
             endif

             if( trim(arg(n)).eq.'-topo'     ) then
                                   topo     = trim(arg(n+1))
             endif

             if( trim(arg(n)).eq.'-other' ) then
                 num = 1
                 if( n+num.le.nargs ) then
                 read(arg(n+num),fmt='(a1)') char
                 do while (char.ne.'-' .and. n+num.ne.nargs )
                 num = num+1
                 read(arg(n+num),fmt='(a1)') char
                 enddo
                 if( char.eq.'-' ) num = num-1
                 allocate ( other_rst(num) )
                 do m=1,num
                 other_rst(m) = arg(n+m)
                 enddo
                 num_other_rst = num
                 endif
             endif

             if( trim(arg(n)).eq.'-lm'       ) read(arg(n+1),*) lm_out
      enddo
      if( lm_out.eq.-999 ) then
          print *
          print *, 'You must supply Output Vertical Resolution!'
          print *
          stop
      endif

      print *
      print *, '       dyn  restart filename: ',trim(dynrst)
      print *, '  datmodyn  restart filename: ',trim(datmodynrst)
      print *, '     moist  restart filename: ',trim(moistrst)
      print *, '               topo filename: ',trim(topo)
      do n=1,num_other_rst
      print *, ' other restart filename: ',trim(other_rst(n))
      enddo

! **********************************************************************
! ****                   Read dycore internal Restart               ****
! **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

      nymd = headr1(1)*10000 + headr1(2)*100 + headr1(3)
      nhms = headr1(4)*10000 + headr1(5)*100 + headr1(6)

      im = 1
      jm = 1
      lm_in = headr2(3)

      print *, '   input FV resolution: ',im,jm,lm_in
      print *, '                  date: ',nymd,nhms
      print *

      allocate (    u_fv_in(im,jm,lm_in)    )
      allocate (    v_fv_in(im,jm,lm_in)    )
      allocate (  thv_fv_in(im,jm,lm_in)    )
      allocate (   dp_fv_in(im,jm,lm_in)    )
      allocate (   pk_fv_in(im,jm,lm_in)    )
      allocate (  ple_fv_in(im,jm,lm_in+1)  )
      allocate (  pke_fv_in(im,jm,lm_in+1)  )
      allocate (   ps_fv_in(im,jm)       )
      allocate (   ak_in(lm_in+1)        )
      allocate (   bk_in(lm_in+1)        )
      allocate (  oml_fv_in(im,jm,lm_in  )  )

      read (10) ak_in
      read (10) bk_in
      if (verbose) write (*,*) "ak_in: ", ak_in
      if (verbose) write (*,*) "bk_in: ", bk_in

      do L=1,lm_in
         read(10) u_fv_in(1,1,L)
      enddo
      if (verbose) write (*,*) "u_fv_in: ", u_fv_in
      do L=1,lm_in
         read(10) v_fv_in(1,1,L)
      enddo
      if (verbose) write (*,*) "v_fv_in: ", v_fv_in
      do L=1,lm_in
         read(10) thv_fv_in(1,1,L)
      enddo
      if (verbose) write (*,*) "thv_fv_in: ", thv_fv_in
      do L=1,lm_in+1
         read(10) ple_fv_in(1,1,L)
      enddo
      if (verbose) write (*,*) "ple_fv_in: ", ple_fv_in
      do L=1,lm_in
         read(10) pk_fv_in(1,1,L) 
      enddo
      if (verbose) write (*,*) "pk_fv_in: ", pk_fv_in

      close (10)

      ps_fv_in(:,:) = ple_fv_in(:,:,lm_in+1)
      do L=lm_in,1,-1
      dp_fv_in(:,:,L) = ple_fv_in(:,:,L+1) - ple_fv_in(:,:,L)
      enddo

      oml_fv_in = 0.0

! **********************************************************************
! ****                 Read datmodyn internal Restart               ****
! **********************************************************************

      open (10,file=trim(datmodynrst),form='unformatted',access='sequential')

      im    = 1
      jm    = 1
      !lm_in is from the fv restart

      print *, ' input datmo resolution: ',im,jm,lm_in
      print *, '                   date: ',nymd,nhms
      print *

      allocate ( pref_in(      lm_in+1)  )
      allocate (  ple_in(im,jm,lm_in+1)  )
      allocate (    t_in(im,jm,lm_in  )  )
      allocate (    u_in(im,jm,lm_in  )  )
      allocate (    v_in(im,jm,lm_in  )  )
      allocate (   om_in(im,jm,lm_in+1)  )
      allocate (   ps_in(im,jm        )  )
      allocate (   dp_in(im,jm,lm_in  )  )

      allocate (  thv_in(im,jm,lm_in  )  )
      allocate (   pk_in(im,jm,lm_in  )  )
      allocate (  pke_in(im,jm,lm_in+1)  )
      allocate (  oml_in(im,jm,lm_in  )  )

      allocate (    dum1(      lm_in+1)  )
      allocate (   dum3l(im,jm,lm_in  )  )
      allocate (   dum3e(im,jm,lm_in+1)  )

      read (10) dum1
      pref_in = dum1
      if (verbose) write (*,*) "pref_in: ", pref_in

      do L=1,lm_in+1
         read(10) dum3e(1,1,L)
         ple_in(1,1,L) = dum3e(1,1,L)
      enddo
      if (verbose) write (*,*) "ple_in: ", ple_in
      do L=1,lm_in
         read(10) dum3l(1,1,L)
         t_in(1,1,L) = dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "t_in: ", t_in
      do L=1,lm_in
         read(10) dum3l(1,1,L)
         u_in(1,1,L) = dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "u_in: ", u_in
      do L=1,lm_in
         read(10) dum3l(1,1,L)
         v_in(1,1,L) = dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "v_in: ", v_in
      do L=1,lm_in+1
         read(10) dum3e(1,1,L)
         om_in(1,1,L) = dum3e(1,1,L)
      enddo
      if (verbose) write (*,*) "om_in: ", om_in

      close (10)

      ps_in(:,:) = ple_in(:,:,lm_in+1)
      do L=lm_in,1,-1
         dp_in(:,:,L) = ple_in(:,:,L+1) - ple_in(:,:,L)
      enddo

      deallocate ( dum1)
      deallocate (dum3l)
      deallocate (dum3e)

! **********************************************************************
! ****                   Read Moist Internal Restart                ****
! **********************************************************************

      allocate (  dum(im,jm) )

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')

      nt =  0
      rc =  0
      nbeg = 0
      do while (rc.eq.0)
        read (10,iostat=rc)  dum
        if( rc.eq.0 ) then
            nt = nt + 1
            allocate( q_out(im,jm,lm_in,nt) )
                      q_out(:,:,1,nt)   = dum
                      do L=2,lm_in
                      read (10,iostat=rc) dum
                      q_out(:,:,L,nt)   = dum
                      enddo
             if( nt.eq.1) then
                 allocate( q_in (im,jm,lm_in,nt) )
                           q_in (:,:,:,nt) = q_out(:,:,:,nt)
             else
                           q_out(:,:,:,1:nt-1) = q_in(:,:,:,1:nt-1)
               deallocate( q_in )
                 allocate( q_in (im,jm,lm_in,nt) )
                           q_in = q_out
             endif
             print '(a,i4)', 'Reading Moist Restart      for Field # ',nt-nbeg
             if (verbose) write(*,*) 'Field #', nt-nbeg, ': ', q_in(:,:,:,nt-nbeg)
             deallocate( q_out )
        endif
      enddo
      print *

      close (10)

! **********************************************************************
! ****                   Read Other 3D-Restarts                     ****
! **********************************************************************

      allocate( nt_other(0:num_other_rst) )
                nt_other(0) = nt

      if( num_other_rst.ne.0 ) then

       ! n = 0
      nbeg = nt
      do m=1,num_other_rst
        !open (10,file=trim(other_rst(m)),form='unformatted',access='direct',recl=im*jm*4)
         open (10,file=trim(other_rst(m)),form='unformatted',access='sequential')
         rc =  0
         do while (rc.eq.0)
          ! n = n+1
          !read (10,iostat=rc,rec=n) dum
           read (10,iostat=rc)       dum
           if( rc.eq.0 ) then
               nt = nt + 1
               allocate( q_out(im,jm,lm_in,nt) )
                         q_out(:,:,1,nt)   = dum
                         do L=2,lm_in
                        !n = n+1
                        !read (10,iostat=rc,rec=n) dum
                         read (10,iostat=rc)       dum
                         if( rc.eq.0 ) then
                             q_out(:,:,L,nt)   = dum
                         else
                             print *, trim(other_rst(m)),' not 3D!'
                             stop
                         endif
                         enddo
                         q_out(:,:,:,1:nt-1) = q_in(:,:,:,1:nt-1)
                  deallocate( q_in )
                    allocate( q_in (im,jm,lm_in,nt) )
                              q_in = q_out
                print '(a,i2,a,i4)', 'Reading Other_Restart # ',m,' for Field # ',nt-nbeg
                deallocate( q_out )
           endif
         enddo
         nt_other(m) = nt-nbeg
                nbeg = nt
         print *
         close (10)
      enddo

      endif

! **********************************************************************
! ****                    Read Topography Datasets                  ****
! **********************************************************************

      allocate ( phis(im,jm) )

      print *, 'Reading Topography Dataset: ',trim(topo)
      open (10,file=trim(topo),form='unformatted',access='sequential')
      read (10)   dum
      close(10)

      grav = MAPL_GRAV
      phis = dum*grav

! **********************************************************************
! ****                         Remap State                          ****
! **********************************************************************
                                                                                                                      
      allocate (   ak_out(lm_out+1)        )
      allocate (   bk_out(lm_out+1)        )

      allocate ( pref_out(lm_out+1)        )
      allocate (  ple_out(im,jm,lm_out+1)  )
      allocate (    t_out(im,jm,lm_out)    )
      allocate (    u_out(im,jm,lm_out)    )
      allocate (    v_out(im,jm,lm_out)    )
      allocate (   om_out(im,jm,lm_out+1)  )
      allocate (  oml_out(im,jm,lm_out)    )

      allocate (  thv_out(im,jm,lm_out)    )
      allocate (   dp_out(im,jm,lm_out)    )
      allocate (   pk_out(im,jm,lm_out)    )
      allocate (  pke_out(im,jm,lm_out+1)  )
      allocate (    q_out(im,jm,lm_out,nt) )
      allocate (   ps_out(im,jm)           )
      allocate (   dz_out(im,jm,lm_out)    )
      allocate (    w_out(im,jm,lm_out)    )

      allocate (    u_fv_out(im,jm,lm_out)    )
      allocate (    v_fv_out(im,jm,lm_out)    )
      allocate (  thv_fv_out(im,jm,lm_out)    )
      allocate (   dp_fv_out(im,jm,lm_out)    )
      allocate (   pk_fv_out(im,jm,lm_out)    )
      allocate (  ple_fv_out(im,jm,lm_out+1)  )
      allocate (  pke_fv_out(im,jm,lm_out+1)  )
      allocate (   ps_fv_out(im,jm)       )
      allocate (  oml_fv_out(im,jm,lm_out)    )

! ------------------
! MAT First remap FV
! ------------------

! ----------------------------------

      print *, 'Calling REMAP for fvcore...'
      call remap  ( ps_fv_out,dp_fv_out,u_fv_out,v_fv_out,oml_fv_out,thv_fv_out,q_out,phis,lm_out, &
                    ps_fv_in ,dp_fv_in ,u_fv_in ,v_fv_in ,oml_fv_in, thv_fv_in ,q_in ,phis,lm_in , &
                    im,jm,nt,ak_out,bk_out,pref_out,verbose )
      print *, '        REMAP Finished'

! ----------------------------------

      kappa = MAPL_KAPPA

      ple_fv_out(:,:,lm_out+1) = ps_fv_out(:,:)
      do L=lm_out,1,-1
      ple_fv_out(:,:,L) = ple_fv_out(:,:,L+1)-dp_fv_out(:,:,L)
      enddo
      dz_out = 0.
       w_out = 0.

! Ensure top edge = ptop
! ----------------------
       dp_fv_out(:,:,1) = ple_fv_out(:,:,2)-ak_out(1)
      ple_fv_out(:,:,1) = ple_fv_out(:,:,2)-dp_fv_out(:,:,1)

      pke_fv_out(:,:,:) = ple_fv_out(:,:,:)**kappa

      do L=1,lm_out
      pk_fv_out(:,:,L) = ( pke_fv_out(:,:,L+1)-pke_fv_out(:,:,L) ) &
                       / ( kappa*log(ple_fv_out(:,:,L+1)/ple_fv_out(:,:,L)) )
      enddo


! -----------------------------------------------------------------
! MAT Before calling REMAP, we must have THV. Construct this from T
! -----------------------------------------------------------------

      eps   = MAPL_RVAP/MAPL_RGAS-1.0
      pke_in(:,:,:) = ple_in(:,:,:)**MAPL_KAPPA

      do L = 1, lm_in
         pk_in (:,:,L) = ( pke_in(:,:,L+1)-pke_in(:,:,L) ) / ( MAPL_KAPPA*log(ple_in(:,:,L+1)/ple_in(:,:,L)) )
         thv_in(:,:,L) = t_in(:,:,L)*(1.0+eps*q_in(:,:,L,1)) / pk_in(:,:,L)
      end do

! -----------------------------------------------------------------
! MAT REMAP can only really handle level variables. Omega is edge.
!     Create a level version
! -----------------------------------------------------------------

      do L = 1, lm_in
         oml_in(:,:,L) = 0.5*(om_in(:,:,L)+om_in(:,:,L+1))
      end do

! -----------------------
! MAT Now remap for datmo
! -----------------------

      print *, 'Calling REMAP for datmo ...'
      call remap  ( ps_out,dp_out,u_out,v_out,oml_out,thv_out,q_out,phis,lm_out, &
                    ps_in ,dp_in ,u_in ,v_in ,oml_in, thv_in ,q_in ,phis,lm_in , &
                    im,jm,nt,ak_out,bk_out,pref_out,verbose )
      print *, '        REMAP Finished'

! ----------------------------------

      kappa = MAPL_KAPPA

      ple_out(:,:,lm_out+1) = ps_out(:,:)
      do L=lm_out,1,-1
      ple_out(:,:,L) = ple_out(:,:,L+1)-dp_out(:,:,L)
      enddo
      dz_out = 0.
       w_out = 0.

! Ensure top edge = ptop
! ----------------------
       dp_out(:,:,1) = ple_out(:,:,2)-ak_out(1)
      ple_out(:,:,1) = ple_out(:,:,2)-dp_out(:,:,1)

      pke_out(:,:,:) = ple_out(:,:,:)**kappa

      do L=1,lm_out
      pk_out(:,:,L) = ( pke_out(:,:,L+1)-pke_out(:,:,L) ) &
                    / ( kappa*log(ple_out(:,:,L+1)/ple_out(:,:,L)) )
      enddo

! -----------------------------------------------------------------
! MAT After calling REMAP, we must have T. Construct this from THV
! -----------------------------------------------------------------

      do L = 1, lm_out
         t_out(:,:,L) = (thv_out(:,:,L)*pk_out(:,:,L))/(1.0+eps*q_out(:,:,L,1))
      end do

! ---------------------------------------
! MAT Now reconvert oml to get om on edge
! ---------------------------------------

      om_out(:,:,1) = 0.0
      do L = 2, lm_out
         om_out(:,:,L) = 0.5*(oml_out(:,:,L-1)+oml_out(:,:,L))
      end do
      om_out(:,:,lm_out+1) = 0.0

! **********************************************************************
! ****                  Write dycore internal Restart               ****
! **********************************************************************

      write(date,101) nymd
      write(hour,102) nhms/10000
      write(cim ,103) im
      write(cjm ,103) jm
      write(clm ,103) lm_out
  101 format(i8.8)
  102 format(i2.2)
  103 format(i4.4)

      dynrst = trim(dynrst) // '.r' // cim  // 'x' // cjm  // 'x' // clm //      &
                               '.e' // date // '_' // hour // 'z'

      print *
      print *, '              Creating: ',trim(dynrst)
      print *, '     output resolution: ',im,jm,lm_out
      print *, '                  date: ',nymd,nhms
      print *
      open (10,file=trim(dynrst),form='unformatted',access='sequential')

      headr2(3) = lm_out
      write(10) headr1
      write(10) headr2

      write(10) ak_out
      write(10) bk_out
      if (verbose) write(*,*) "ak_out: ", ak_out
      if (verbose) write(*,*) "bk_out: ", bk_out

      do L=1,lm_out
         write(10) u_fv_out(1,1,L)
      enddo
      if (verbose) write(*,*) "u_fv_out: ", u_fv_out
      do L=1,lm_out
         write(10) v_fv_out(1,1,L)
      enddo
      if (verbose) write(*,*) "v_fv_out: ", v_fv_out
      do L=1,lm_out
         write(10) thv_fv_out(1,1,L)
      enddo
      if (verbose) write(*,*) "thv_fv_out: ", thv_fv_out
      do L=1,lm_out+1
         write(10) ple_fv_out(1,1,L)
      enddo
      if (verbose) write(*,*) "ple_fv_out: ", ple_fv_out
      do L=1,lm_out
         write(10) pk_fv_out(1,1,L) 
      enddo
      if (verbose) write(*,*) "pk_fv_out: ", pk_fv_out

! if we are on the cube (check for jm being equal to im*6) write out dz and w
      if(jm.eq.im*6) then
         do L=1,lm_out
            write(10) dz_out(1,1,L)
         enddo
         do L=1,lm_out
            write(10) w_out(1,1,L)
         enddo
         print *,' Cubed Sphere Grid: Writing zero fields of DZ and W '
         print *
      endif

      close (10)

! **********************************************************************
! ****                Write datmodyn internal Restart               ****
! **********************************************************************

      allocate (    dum1(      lm_out+1)  )
      allocate (   dum3l(im,jm,lm_out  )  )
      allocate (   dum3e(im,jm,lm_out+1)  )

      write(date,201) nymd
      write(hour,202) nhms/10000
      write(cim ,203) im
      write(cjm ,203) jm
      write(clm ,203) lm_out
  201 format(i8.8)
  202 format(i2.2)
  203 format(i4.4)

      datmodynrst = trim(datmodynrst) // '.r' // cim  // 'x' // cjm  // 'x' // clm //      &
                               '.e' // date // '_' // hour // 'z'

      print *
      print *, '              Creating: ',trim(datmodynrst)
      print *, '     output resolution: ',im,jm,lm_out
      print *, '                  date: ',nymd,nhms
      print *
      open (10,file=trim(datmodynrst),form='unformatted',access='sequential')

      dum1 = pref_out
      write (10) dum1
      if (verbose) write (*,*) "pref: ", pref_out

      do L=1,lm_out+1
         dum3e(1,1,L) = ple_out(1,1,L)
         write(10) dum3e(1,1,L)
      enddo
      if (verbose) write (*,*) "ple: ", ple_out
      do L=1,lm_out
         dum3l(1,1,L) = t_out(1,1,L)
         write(10) dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "t: ", t_out
      do L=1,lm_out
         dum3l(1,1,L) = u_out(1,1,L)
         write(10) dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "u: ", u_out
      do L=1,lm_out
         dum3l(1,1,L) = v_out(1,1,L)
         write(10) dum3l(1,1,L)
      enddo
      if (verbose) write (*,*) "v: ", v_out
      do L=1,lm_out+1
         dum3e(1,1,L) = om_out(1,1,L)
         write(10) dum3e(1,1,L)
      enddo
      if (verbose) write (*,*) "om: ", om_out
      close (10)

      deallocate ( dum1)
      deallocate (dum3l)
      deallocate (dum3e)

! **********************************************************************
! ****                  Write moist internal Restart                ****
! **********************************************************************

      moistrst = trim(moistrst) // '.r' // cim  // 'x' // cjm  // 'x' // clm //  &
                                   '.e' // date // '_' // hour // 'z'
      print *, '              Creating: ',trim(moistrst)

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      do n=1,nt_other(0)
      print '(a,i4)',      '                         Writing Moist Restart      for Field # ',n
      if (verbose) write(*,*) 'Field #', n, ': ', q_out(:,:,:,n)
      do L=1,lm_out
         dum(:,:) = q_out(:,:,L,n)
         write(10)  dum
      enddo
      enddo
      print *
      close (10)

! **********************************************************************
! ****                  Write Other Internal Restarts               ****
! **********************************************************************

      if( num_other_rst.ne.0 ) then

      nbeg = nt_other(0) + 1
      do m=1,num_other_rst
         nend = nbeg + nt_other(m)-1

         other_rst(m) = trim(other_rst(m)) // '.r' // cim  // 'x' // cjm  // 'x' // clm //  &
                                              '.e' // date // '_' // hour // 'z'
         print *, '              Creating: ',trim(other_rst(m))

         open (10,file=trim(other_rst(m)),form='unformatted',access='sequential')

         do n=nbeg,nend
         print '(a,i2,a,i4)', '                         Writing Other_Restart # ',m,' for Field # ',n-nbeg+1
         do L=1,lm_out
            dum(:,:) = q_out(:,:,L,n)
            write(10)  dum
         enddo
         enddo
         close (10)
         nbeg = nend + 1
         print *
      enddo

      endif

      stop
      end

      subroutine remap  ( ps1,dp1,u1,v1,om1,thv1,q1,phis1,lm1, &
                          ps2,dp2,u2,v2,om2,thv2,q2,phis2,lm2,im,jm,nt,ak1,bk1,pref1,verbose )

! *******************************************************************************
! *****                                                                     *****
! *****   Program to remap Target analysis variables (ps2,dp2,u2,v2,t2,q2)  *****
! *****   onto Model grid variables (ps1,dp1,u1,v1,thv1,q1).  Program       *****
! *****   allows for differenct topographies between Analysis and Model.    *****
! *****                                                                     *****
! *******************************************************************************

      use MAPL_ConstantsMod
      use m_set_eta, only: set_eta
      implicit none
      integer  im,jm,lm1,lm2,nt

! Output Model Variables
! ----------------------
      real      dp1(im,jm,lm1)
      real       u1(im,jm,lm1)
      real       v1(im,jm,lm1)
      real      om1(im,jm,lm1)
      real     thv1(im,jm,lm1)
      real       q1(im,jm,lm1,nt)
      real      ps1(im,jm)
      real    phis1(im,jm)
      real      ak1(lm1+1)
      real      bk1(lm1+1)
      real    pref1(lm1+1)

! Input Analysis Variables
! ------------------------
      real     dp2(im,jm,lm2)
      real      u2(im,jm,lm2)
      real      v2(im,jm,lm2)
      real     om2(im,jm,lm2)
      !real      t2(im,jm,lm2)
      real    thv2(im,jm,lm2)
      real      q2(im,jm,lm2,nt)
      real     ps2(im,jm)
      real   phis2(im,jm)
      real     ak2(lm2+1)
      real     bk2(lm2+1)

      logical  verbose

! Local variables
! ---------------
      real   pe1(im,jm,lm1+1)
      real   pe2(im,jm,lm2+1)
      real   pk (im,jm,lm2  )
      real  pke1(im,jm,lm1+1)
      real  pke2(im,jm,lm2+1)
      real  phi2(im,jm,lm2+1)

      real, allocatable :: plevs(:)

      real    kappa,cp,dum,dum1,dum2
      real    rgas,eps,rvap,grav
      integer i,j,L,kdum

      kappa = MAPL_KAPPA
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      eps   = rvap/rgas-1.0

! Create AK & BK for each vertical dimension
! ------------------------------------------
      call set_eta ( lm1,kdum,dum,dum,ak1,bk1 )
      call set_eta ( lm2,kdum,dum,dum,ak2,bk2 )

      if( verbose ) then
          allocate( plevs(lm2) )
          do L=1,lm2
          plevs(L) = 0.5*( ak2(L)   + 100000.0*bk2(L)   &
                         + ak2(L+1) + 100000.0*bk2(L+1) )
          enddo
          print *, 'Input PLEVS:'
          print *, (plevs(L)/100,L=1,lm2)

          deallocate( plevs )
            allocate( plevs(lm1) )
          do L=1,lm1
          plevs(L) = 0.5*( ak1(L)   + 100000.0*bk1(L)   &
                         + ak1(L+1) + 100000.0*bk1(L+1) )
          enddo
          print *, 'Output PLEVS:'
          print *, (plevs(L)/100,L=1,lm1)
          deallocate( plevs )

      endif

! Create Pressure Variables
! -------------------------
      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1 (i,j,L) = ak1(L)+bk1(L)*ps1(i,j)
       pke1(i,j,L) = pe1(i,j,L)**kappa
      enddo
      enddo
      enddo
      do L=1,lm1
      do j=1,jm
      do i=1,im
       dp1(i,j,L) = pe1(i,j,L+1)-pe1(i,j,L)
      enddo
      enddo
      enddo

      do L=1,lm2+1
      do j=1,jm
      do i=1,im
       pe2 (i,j,L) = ak2(L)+bk2(L)*ps2(i,j)
       pke2(i,j,L) = pe2(i,j,L)**kappa
      enddo
      enddo
      enddo
      do L=1,lm2
      do j=1,jm
      do i=1,im
       dp2(i,j,L) = pe2(i,j,L+1)-pe2(i,j,L)
      enddo
      enddo
      enddo

! MAT Fill pref1
! --------------
      do L=1,lm1+1
      pref1(L) = ak1(L)   + 100000.0*bk1(L)
      enddo

! Construct target virtual potential temperature
! ----------------------------------------------
      do L=1,lm2
      pk(:,:,L) = ( pke2(:,:,L+1)-pke2(:,:,L) )/( kappa*log(pe2(:,:,L+1)/pe2(:,:,L)) )
      enddo

! Construct target analysis heights
! ---------------------------------
      phi2(:,:,lm2+1) = phis2(:,:)
      do L=lm2,1,-1
      phi2(:,:,L) = phi2(:,:,L+1) + cp*thv2(:,:,L)*( pke2(:,:,L+1)-pke2(:,:,L) )
      enddo
      
! Compute new surface pressure consistent with target surface pressure and topography
! -----------------------------------------------------------------------------------
      do j=1,jm
      do i=1,im
           L = lm2
           do while ( phi2(i,j,L).lt.phis1(i,j) )
           L = L-1
           enddo
           ps1(i,j) = pe2(i,j,L+1)*( 1+(phi2(i,j,L+1)-phis1(i,j))/(cp*thv2(i,j,L)*pke2(i,j,L+1)) )**(1.0/kappa)
      enddo
      enddo

! Construct model pressure variables using new surface pressure
! -------------------------------------------------------------
      print *
         L = 1
      dum1 = (ak1(L)+bk1(L)*100000.0)/100
      write(6,1000) L,ak1(L),bk1(L),dum1
 1000 format(1x,'L: ',i3,4x,'ak: ',f10.3,'   bk: ',f10.8,4x,'pe: ',f8.3)
      do L=2,lm1+1
      dum2 = (ak1(L)+bk1(L)*100000.0)/100
      write(6,1001) L,ak1(L),bk1(L),dum2,dum2-dum1
 1001 format(1x,'L: ',i3,4x,'ak: ',f10.3,'   bk: ',f10.8,4x,'pe: ',f8.3,3x,'dp: ',f7.3)
      dum1 = dum2
      enddo
      print *

      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1 (i,j,L) = ak1(L)+bk1(L)*ps1(i,j)
       pke1(i,j,L) = pe1(i,j,L)**kappa
      enddo
      enddo
      enddo

      do L=1,lm1
      do j=1,jm
      do i=1,im
       dp1(i,j,L) = pe1(i,j,L+1)-pe1(i,j,L)
      enddo
      enddo
      enddo

! Map target analysis onto grid defined by new surface pressure
! -------------------------------------------------------------
      print *, 'Calling GMAP, LM_in : ',lm2
      print *, '              LM_out: ',lm1
      call gmap ( im,jm,nt, kappa,                      &
                  lm2,  pke2,  pe2, u2,  v2, om2, thv2,  q2,&
                  lm1,  pke1,  pe1, u1,  v1, om1, thv1,  q1 )

      return
      end

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine gmap(im, jm, nq,  akap,                     &
               km,  pk3d_m,  pe3d_m, u_m,  v_m, om_m, pt_m,  q_m, &
               kn,  pk3d_n,  pe3d_n, u_n,  v_n, om_n, pt_n,  q_n  )
!****6***0*********0*********0*********0*********0*********0**********72

      implicit none

      integer im, jm
      integer km, kn, nq

! Input:
! original data km-level

      real      u_m(im,jm,km)
      real      v_m(im,jm,km)
      real     om_m(im,jm,km)
      real     pt_m(im,jm,km)
      real      q_m(im,jm,km,nq)
      real   pk3d_m(im,jm,km+1)
      real   pe3d_m(im,jm,km+1)


! Output:
! New data (kn-level)
      real      u_n(im,jm,kn)
      real      v_n(im,jm,kn)
      real     om_n(im,jm,kn)
      real     pt_n(im,jm,kn)
      real      q_n(im,jm,kn,nq)
      real   pk3d_n(im,jm,kn+1)
      real   pe3d_n(im,jm,kn+1)

! local (private)
      integer i, j, k, n

      real pe1(im,km+1),pe2(im,kn+1)
      real pk1(im,km+1),pk2(im,kn+1)
      real dp1(im,km)  ,dp2(im,kn)
      real  u1(im,km)  , u2(im,kn)
      real  v1(im,km)  , v2(im,kn)
      real om1(im,km)  ,om2(im,kn)  
      real  t1(im,km)  , t2(im,kn)
      real  q1(im,km)  , q2(im,kn)

      real akap
      real undef
      real big
      parameter ( undef = 1.e15 )
      parameter (   big = 1.e10 )


      do 2000 j=1,jm

! Copy original data to local 2D arrays.

      do k=1,km+1
      do i=1,im
      pe1(i,k) = pe3d_m(i,j,k)
      pk1(i,k) = pk3d_m(i,j,k)
      enddo
      enddo

      do k=1,kn+1
      do i=1,im
      pe2(i,k) = pe3d_n(i,j,k)
      pk2(i,k) = pk3d_n(i,j,k)
      enddo
      enddo

      do k=1,km
      do i=1,im
      dp1(i,k) =  pk1(i,k+1)-pk1(i,k)
       u1(i,k) =  u_m(i,j,k)
       v1(i,k) =  v_m(i,j,k)
      om1(i,k) = om_m(i,j,k)
       t1(i,k) = pt_m(i,j,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pk2(i,k+1)-pk2(i,k)
      enddo
      enddo

! map pt
! ------
      call mappm ( km, pk1, dp1, t1, kn, pk2, dp2, t2, im, 1, 7 )

      do k=1,km
      do i=1,im
      dp1(i,k) = pe1(i,k+1)-pe1(i,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pe2(i,k+1)-pe2(i,k)
      enddo
      enddo

! map u,v
! -------
      call mappm ( km, pe1, dp1, u1, kn, pe2, dp2, u2, im, -1, 7 )
      call mappm ( km, pe1, dp1, v1, kn, pe2, dp2, v2, im, -1, 7 )

! map om (like u,v)
! -----------------
      call mappm ( km, pe1, dp1,om1, kn, pe2, dp2,om2, im, -1, 7 )

! map q
! -------
      do n=1,nq
         do k=1,km
         do i=1,im
          q1(i,k) =  q_m(i,j,k,n)
         enddo
         enddo
         call mappm ( km, pe1, dp1, q1, kn, pe2, dp2, q2, im,  0, 7 )
         do k=1,kn
         do i=1,im
           q_n(i,j,k,n) = q2(i,k)
         enddo
         enddo
      enddo

      do k=1,kn
      do i=1,im
        u_n(i,j,k) = u2(i,k)
        v_n(i,j,k) = v2(i,k)
       om_n(i,j,k) =om2(i,k)
       pt_n(i,j,k) = t2(i,k)
      enddo
      enddo

2000  continue

      return
      end


!****6***0*********0*********0*********0*********0*********0**********72
      subroutine mappm(km, pe1, dp1, q1, kn, pe2, dp2, q2, im, iv, kord)
!****6***0*********0*********0*********0*********0*********0**********72
! IV = 0: constituents
! IV = 1: potential temp
! IV =-1: winds
!
! Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
!
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate

      parameter (kmax = 200)
      parameter (R3 = 1./3., R23 = 2./3.)

      real dp1(im,km),   dp2(im,kn)
      real  q1(im,km),    q2(im,kn)
      real pe1(im,km+1), pe2(im,kn+1)
      integer kord

! local work arrays
      real a4(4,im,km)

      do k=1,km
         do i=1,im
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      call ppm2m(a4, dp1, im, km, iv, kord)

! Lowest layer: constant distribution
      do i=1, im
         a4(2,i,km) = q1(i,km)
         a4(3,i,km) = q1(i,km)
         a4(4,i,km) = 0.
      enddo

      do 5555 i=1,im
         k0 = 1
      do 555 k=1,kn

         if(pe2(i,k+1) .le. pe1(i,1)) then
! Entire grid above old ptop
            q2(i,k) = a4(2,i,1)
         elseif(pe2(i,k) .ge. pe1(i,km+1)) then
! Entire grid below old ps
            q2(i,k) = a4(3,i,km)
         elseif(pe2(i,k  ) .lt. pe1(i,1) .and. &
                pe2(i,k+1) .gt. pe1(i,1))  then
! Part of the grid above ptop
            q2(i,k) = a4(1,i,1)
         else

         do 45 L=k0,km
! locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and. &
             pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

! entire new grid is within the original grid
               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = R3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L) &
                       - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else
! Fractional area...
              delp = pe1(i,L+1) - pe2(i,k)
              TT   = R3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+ &
                     a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
              dpsum = delp
              k1 = L + 1
             goto 111
             endif
         endif
45       continue

111      continue
         do 55 L=k1,km
         if( pe2(i,k+1) .gt. pe1(i,L+1) ) then

! Whole layer..

            qsum  =  qsum + dp1(i,L)*q1(i,L)
            dpsum = dpsum + dp1(i,L)
         else
           delp = pe2(i,k+1)-pe1(i,L)
           esl  = delp / dp1(i,L)
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl* &
                 (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
          dpsum = dpsum + delp
           k0 = L
           goto 123
         endif
55       continue
        delp = pe2(i,k+1) - pe1(i,km+1)
        if(delp .gt. 0.) then
! Extended below old ps
           qsum = qsum + delp * a4(3,i,km)
          dpsum = dpsum + delp
        endif
123     q2(i,k) = qsum / dpsum
      endif
555   continue
5555  continue

      return
      end

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine ppm2m(a4,delp,im,km,iv,kord)
!****6***0*********0*********0*********0*********0*********0**********72
! iv = 0: positive definite scalars
! iv = 1: others
! iv =-1: winds

      implicit none

      integer im, km, lmt, iv
      integer kord
      integer i, k, km1
      real a4(4,im,km), delp(im,km)

! local arrays.
      real dc(im,km),delq(im,km)
      real h2(im,km)
      real a1, a2, c1, c2, c3, d1, d2
      real qmax, qmin, cmax, cmin
      real qm, dq, tmp

! Local scalars:
      real qmp
      real lac

      km1 = km - 1

      do 500 k=2,km
      do 500 i=1,im
      delq(i,k-1) = a4(1,i,k) - a4(1,i,k-1)
500   a4(4,i,k  ) = delp(i,k-1) + delp(i,k)

      do 1220 k=2,km1
      do 1220 i=1,im
      c1 = (delp(i,k-1)+0.5*delp(i,k))/a4(4,i,k+1)
      c2 = (delp(i,k+1)+0.5*delp(i,k))/a4(4,i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) / &
                                    (a4(4,i,k)+delp(i,k+1))
      qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
      qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

!****6***0*********0*********0*********0*********0*********0**********72
! 4th order interpolation of the provisional cell edge value
!****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) * &
                ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) - &
                                delp(i,k-1)*a1*dc(i,k  ) )
12    continue

! Area preserving cubic with 2nd deriv. = 0 at the boundaries
! Top
      do i=1,im
      d1 = delp(i,1)
      d2 = delp(i,2)
      qm = (d2*a4(1,i,1)+d1*a4(1,i,2)) / (d1+d2)
      dq = 2.*(a4(1,i,2)-a4(1,i,1)) / (d1+d2)
      c1 = 4.*(a4(2,i,3)-qm-d2*dq) / ( d2*(2.*d2*d2+d1*(d2+3.*d1)) )
      c3 = dq - 0.5*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,2) = qm - 0.25*c1*d1*d2*(d2+3.*d1)
      a4(2,i,1) = d1*(2.*c1*d1**2-c3) + a4(2,i,2)
      dc(i,1) =  a4(1,i,1) - a4(2,i,1)
! No over- and undershoot condition
      cmax = max(a4(1,i,1), a4(1,i,2))
      cmin = min(a4(1,i,1), a4(1,i,2))
      a4(2,i,2) = max(cmin,a4(2,i,2))
      a4(2,i,2) = min(cmax,a4(2,i,2))
      enddo

      if(iv == 0) then
         do i=1,im
            a4(2,i,1) = max(0.,a4(2,i,1))
            a4(2,i,2) = max(0.,a4(2,i,2))
         enddo
      elseif(iv == -1) then
         do i=1,im
            if( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
         enddo
      endif

!****6***0*********0*********0*********0*********0*********0**********72

! Bottom
! Area preserving cubic with 2nd deriv. = 0 at the surface
      do 15 i=1,im
      d1 = delp(i,km)
      d2 = delp(i,km1)
      qm = (d2*a4(1,i,km)+d1*a4(1,i,km1)) / (d1+d2)
      dq = 2.*(a4(1,i,km1)-a4(1,i,km)) / (d1+d2)
      c1 = (a4(2,i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
      c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,km) = qm - c1*d1*d2*(d2+3.*d1)
      a4(3,i,km) = d1*(8.*c1*d1**2-c3) + a4(2,i,km)
      dc(i,km) = a4(3,i,km) -  a4(1,i,km)
!****6***0*********0*********0*********0*********0*********0**********72
! No over- and undershoot condition
      cmax = max(a4(1,i,km), a4(1,i,km1))
      cmin = min(a4(1,i,km), a4(1,i,km1))
      a4(2,i,km) = max(cmin,a4(2,i,km))
      a4(2,i,km) = min(cmax,a4(2,i,km))
!****6***0*********0*********0*********0*********0*********0**********72
15    continue

      if(iv .eq. 0) then
      do i=1,im
         a4(2,i,km) = max(0.,a4(2,i,km))
         a4(3,i,km) = max(0.,a4(3,i,km))
      enddo
      endif

      do 20 k=1,km1
      do 20 i=1,im
      a4(3,i,k) = a4(2,i,k+1)
20    continue
!
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
!

! Top 2 and bottom 2 layers always use monotonic mapping

      do k=1,2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
      enddo

      if(kord == 7) then
!****6***0*********0*********0*********0*********0*********0**********72
! Huynh's 2nd constraint
!****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=1,im
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 4000 k=3, km-2
      do 3000 i=1, im
! Right edges
         qmp   = a4(1,i,k)                 + 2.0*delq(i,k-1)
         lac   = a4(1,i,k) + 1.5*h2(i,k-1) + 0.5*delq(i,k-1)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(3,i,k) = min(max(a4(3,i,k), qmin), qmax)
! Left  edges
         qmp   = a4(1,i,k)                 - 2.0*delq(i,k)
         lac   = a4(1,i,k) + 1.5*h2(i,k+1) - 0.5*delq(i,k)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(2,i,k) = min(max(a4(2,i,k), qmin), qmax)
! Recompute A6
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
3000  continue
! Additional constraint to prevent negatives
         if (iv == 0) then
             call kmppm(dc(1,k),a4(1,1,k),im, 2)
         endif
4000  continue

      else

         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv .eq. 0) lmt = min(2, lmt)

      do k=3, km-2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, lmt)
      enddo
      endif

      do 5000 k=km1,km
         do i=1,im
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
5000  continue

      return
      end

!****6***0*********0*********0*********0*********0*********0**********72
      subroutine kmppm(dm, a4, km, lmt)
!****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      real r12
      parameter (r12 = 1./12.)

      integer km, lmt
      integer i
      real a4(4,km),dm(km)
      real da1, da2, a6da
      real fmin
      real qmp

      if (lmt .eq. 3) return
! Full constraint

      if(lmt .eq. 0) then
      do 100 i=1,km
      if(dm(i) .eq. 0.) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da .lt. -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da .gt. da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
100   continue
      elseif (lmt .eq. 2) then
! Positive definite

! Positive definite constraint
      do 250 i=1,km
      if(abs(a4(3,i)-a4(2,i)) .ge. -a4(4,i)) go to 250
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
      if(fmin.ge.0.) go to 250
      if(a4(1,i).lt.a4(3,i) .and. a4(1,i).lt.a4(2,i)) then
            a4(3,i) = a4(1,i)
            a4(2,i) = a4(1,i)
            a4(4,i) = 0.
      elseif(a4(3,i) .gt. a4(2,i)) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
      else
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
      endif
250   continue

      elseif (lmt == 1) then

! Improved full monotonicity constraint (Lin)
! Note: no need to provide first guess of A6 <-- a4(4,i)

      do i=1, km
           qmp = 2.*dm(i)
         a4(2,i) = a4(1,i)-sign(min(abs(qmp),abs(a4(2,i)-a4(1,i))), qmp)
         a4(3,i) = a4(1,i)+sign(min(abs(qmp),abs(a4(3,i)-a4(1,i))), qmp)
         a4(4,i) = 3.*( 2.*a4(1,i) - (a4(2,i)+a4(3,i)) )
      enddo
      endif

      return
      end
      subroutine minmax ( q,name,im,jm,lm )
      character*4 name
      real q(im,jm,lm)
      qmax = q(1,1,1)
      qmin = q(1,1,1)
      do l=1,lm
      do j=1,jm
      do i=1,im
      qmax = max( qmax,q(i,j,L) )
      qmin = min( qmin,q(i,j,L) )
      enddo
      enddo
      enddo
      print *, name,' max: ',qmax,' min: ',qmin
      return
      end
      subroutine usage()
      write (*,*) "Usage:  "
      write (*,*)
      write (*,*) " rs_vinterp.x -dynrst      dynrst_fname      Default: fvcore_internal_restart"
      write (*,*) "              -datmodynrst datmodynrst_fname Default: datmodyn_internal_restart"
      write (*,*) "              -moistrst    moistrst_fname    Default: moist_internal_restart"
      write (*,*) "              -topo        topo_fname"
      write (*,*) "              -lm LM"
      write (*,*)
      write (*,*) "where:"
      write (*,*) "-----"
      write (*,*) "  -dynrst           dynrst_fname:  Filename of dynamics internal restart"
      write (*,*) "  -datmodynrst datmodynrst_fname:  Filename of datmodyn internal restart"
      write (*,*) "  -moistrst       moistrst_fname:  Filename of    moist internal restart"
      write (*,*) "  -topo               topo_fname:  Filename of    topography"
      write (*,*) "  -lm                         LM:  Output Veritcal Resolution"
      write (*,*)
      write (*,*) "creates updated restarts at new LM resolution"
      write (*,*) "---------------------------------------------"
      write (*,*)
      call exit(7)
      end
