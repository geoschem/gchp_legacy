      program  main
      use MAPL_ConstantsMod
      implicit none

c **********************************************************************
c **********************************************************************
c ****                                                              ****
c ****  Program to remap GEOS-5 FV & MOIST restarts in the vertical ****
c ****                  to ARIES DYNAMICAL CORE                     ****
c ****                                                              ****
c **********************************************************************
c **********************************************************************


      character*256 dynrst, moistrst, bkgeta, topo, iaurst

      integer headr1(6)
      integer headr2(5)
      integer nymd,nhms
      integer im,jm,lm,nt,iostat,rc
      real    undef, kappa, grav

c restart variables and topography
c --------------------------------
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

      real*8, allocatable ::   logps (:,:)
      real*8, allocatable ::   logpt (:,:)
      real*8, allocatable ::   logpl (:,:,:)
      real*8                   logp

      real*4, allocatable ::  dum(:,:)
      real*4, allocatable ::  zox(:,:)  ! Zonal Mean odd-ox

      real*8, allocatable :: dudti(:,:,:)
      real*8, allocatable :: dvdti(:,:,:)
      real*8, allocatable :: dtdti(:,:,:)
      real*8, allocatable :: dpdti(:,:,:)
      real*8, allocatable :: dqdti(:,:,:)
      real*8, allocatable :: dodti(:,:,:)

      real*8, allocatable :: dudto(:,:,:)
      real*8, allocatable :: dvdto(:,:,:)
      real*8, allocatable :: dtdto(:,:,:)
      real*8, allocatable :: dpdto(:,:,:)
      real*8, allocatable :: dqdto(:,:,:)
      real*8, allocatable :: dodto(:,:,:)

      character*256, allocatable :: arg(:)
      character*8    date
      character*2    hour,clm
      character*3    cim,cjm
      integer n,nargs,iargc,i,j,L,nymd0,nhms0

C **********************************************************************
C ****                      Initialize Filenames                    ****
C **********************************************************************

         undef = 1.0e15
        dynrst = 'fvcore_internal_restart'
      moistrst = 'moist_internal_restart'
        iaurst = 'xxx'

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
             if( trim(arg(n)).eq.'-dynrst'   ) then
                                   dynrst  = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-moistrst' ) then
                                   moistrst = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-iau'      ) then
                                   iaurst   = trim(arg(n+1))
             endif
             if( trim(arg(n)).eq.'-topo'     ) then
                                   topo     = trim(arg(n+1))
             endif
      enddo

      print *
      print *, '  dyn  restart filename: ',trim(dynrst)
      print *, 'moist  restart filename: ',trim(moistrst)
      if( trim(iaurst).ne.'xxx' ) then
      print *, '  iau  restart filename: ',trim(iaurst)
      endif
      print *, '          topo filename: ',trim(topo)

C **********************************************************************
C ****                   Read dycore internal Restart               ****
C **********************************************************************

      open (10,file=trim(dynrst),form='unformatted',access='sequential')
      read (10) headr1
      read (10) headr2

      nymd = headr1(1)*10000 + headr1(2)*100 + headr1(3)
      nhms = headr1(4)*10000 + headr1(5)*100 + headr1(6)

      im = headr2(1)
      jm = headr2(2)
      lm = headr2(3)

          print *, '      input resolution: ',im,jm,lm
          print *, '                  date: ',nymd,nhms
          print *

          allocate (    u_in(im,jm,lm)    )
          allocate (    v_in(im,jm,lm)    )
          allocate (  thv_in(im,jm,lm)    )
          allocate (   dp_in(im,jm,lm)    )
          allocate (   pk_in(im,jm,lm)    )
          allocate (  ple_in(im,jm,lm+1)  )
          allocate (   ps_in(im,jm)       )
          allocate (   ak_in(lm+1)        )
          allocate (   bk_in(lm+1)        )

          read (10) ak_in
          read (10) bk_in

          do L=1,lm
             read(10) ((  u_in(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) ((  v_in(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) ((thv_in(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm+1
             read(10) ((ple_in(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             read(10) (( pk_in(i,j,L),i=1,im),j=1,jm)
          enddo

          close (10)

            ps_in(:,:) = ple_in(:,:,lm+1)
          do L=lm,1,-1
          dp_in(:,:,L) = ple_in(:,:,L+1) - ple_in(:,:,L)
          enddo

C **********************************************************************
C ****                   Read Moist Internal Restart                ****
C **********************************************************************

      allocate (  dum(im,jm) )

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')

      nt =  0
      rc =  0
      dowhile (rc.eq.0)
        read (10,iostat=rc)  dum
        if( rc.eq.0 ) then
            nt = nt + 1
            allocate( q_out(im,jm,lm,nt) )
                      q_out(:,:,1,nt)   = dum
                      do L=2,lm
                      read (10,iostat=rc) dum
                      q_out(:,:,L,nt)   = dum
                      enddo
             if( nt.eq.1) then
                 allocate( q_in (im,jm,lm,nt) )
                           q_in (:,:,:,nt) = q_out(:,:,:,nt)
             else
                           q_out(:,:,:,1:nt-1) = q_in(:,:,:,1:nt-1)
               deallocate( q_in )
                 allocate( q_in (im,jm,lm,nt) )
                           q_in = q_out
             endif
             print *, 'Reading Moist Restart for Field # ',nt
             deallocate( q_out )
        endif
      enddo

      close (10)

C **********************************************************************
C ****                      Read AGCM Import Restart                ****
C **********************************************************************

      if( trim(iaurst).ne.'xxx' ) then

      open  (10,file=trim(iaurst),form='unformatted',access='sequential')

      allocate( dudti(im,jm,lm)   )
      allocate( dvdti(im,jm,lm)   )
      allocate( dtdti(im,jm,lm)   )
      allocate( dpdti(im,jm,lm+1) )
      allocate( dqdti(im,jm,lm)   )
      allocate( dodti(im,jm,lm)   )

      do L=1,lm
         read (10)     dum
         dudti(:,:,L) = dum
      enddo
      do L=1,lm
         read (10)     dum
         dvdti(:,:,L) = dum
      enddo
      do L=1,lm
         read (10)     dum
         dtdti(:,:,L) = dum
      enddo
      do L=1,lm+1
         read (10)     dum
         dpdti(:,:,L) = dum
      enddo
      do L=1,lm
         read (10)     dum
         dqdti(:,:,L) = dum
      enddo
      do L=1,lm
         read (10)     dum
         dodti(:,:,L) = dum
      enddo

      close (10)
      endif

! **********************************************************************
! ****                    Read Topography Datasets                  ****
! **********************************************************************

      allocate ( phis(im,jm) )

      print *, 'Reading Topography Dataset: ',trim(topo)
      open (10,file=trim(topo),form='unformatted',access='sequential')
      read (10)   dum
      close(10)

      kappa = MAPL_KAPPA
       grav = MAPL_GRAV
       phis = dum*grav

C **********************************************************************
C ****                         Remap State                          ****
C **********************************************************************
                                                                                                                      
      allocate (    u_out(im,jm,lm)    )
      allocate (    v_out(im,jm,lm)    )
      allocate (  thv_out(im,jm,lm)    )
      allocate (   dp_out(im,jm,lm)    )
      allocate (   pk_out(im,jm,lm)    )
      allocate (  pke_out(im,jm,lm+1)  )
      allocate (  ple_out(im,jm,lm+1)  )
      allocate (    q_out(im,jm,lm,nt) )
      allocate (   ps_out(im,jm)       )
      allocate (   ak_out(lm+1)        )
      allocate (   bk_out(lm+1)        )

c Remap on Native Grid
c --------------------
      call remap  ( ps_out,dp_out,u_out,v_out,thv_out,q_out,phis,lm,
     .              ps_in ,dp_in ,u_in ,v_in ,thv_in ,q_in ,phis,lm,
     .              im,jm,nt,ak_out,bk_out )

      ple_out(:,:,lm+1) = ps_out(:,:)
      do L=lm,1,-1
      ple_out(:,:,L) = ple_out(:,:,L+1)-dp_out(:,:,L)
      enddo

c Ensure top edge = ptop
c ----------------------
       dp_out(:,:,1) = ple_out(:,:,2)-ak_out(1)
      ple_out(:,:,1) = ple_out(:,:,2)-dp_out(:,:,1)

      pke_out(:,:,:) = ple_out(:,:,:)**kappa

      do L=1,lm
      pk_out(:,:,L) = ( pke_out(:,:,L+1)-pke_out(:,:,L) )
     .              / ( kappa*log(ple_out(:,:,L+1)/ple_out(:,:,L)) )
      enddo

c Interpolate Remapped Winds From D-Grid to C-Grid
c ------------------------------------------------
      call dtoa ( u_out,u_out,im,jm,lm,2 )
      call dtoa ( v_out,v_out,im,jm,lm,1 )

      call atoc ( u_out,u_out,im,jm,lm,1 )
      call atoc ( v_out,v_out,im,jm,lm,2 )

      call windfix ( u_out,v_out,ple_out,
     .               u_in, v_in, ple_in, 
     .               im,jm,lm,1 )

C **********************************************************************
C ****                   Simple Interpolation of IAU                ****
C **********************************************************************

      if( trim(iaurst).ne.'xxx' ) then

         allocate( dudto(im,jm,lm)   )
         allocate( dvdto(im,jm,lm)   )
         allocate( dtdto(im,jm,lm)   )
         allocate( dpdto(im,jm,lm+1) )
         allocate( dqdto(im,jm,lm)   )
         allocate( dodto(im,jm,lm)   )

         allocate(  logps(im,jm)     )
         allocate(  logpt(im,jm)     )

c Mid-Level Fields
c ----------------
         allocate(  logpl(im,jm,lm) )

         do L=1,lm
            logpl(:,:,L) = log( 0.5*( ple_in(:,:,L+1)+ple_in(:,:,L) ) )
         enddo
            logps(:,:) = log( ple_in(:,:,lm+1) )
            logpt(:,:) = log( ple_in(:,:,1)       )

         do j=1,jm
         do i=1,im
         do L=1,lm
            logp = log( 0.5*( ple_out(i,j,L+1)+ple_out(i,j,L) ) )
            call sigtopl( dudto(i,j,L),dudti(i,j,1:lm),logpl(i,j,1:lm),logps(i,j),logpt(i,j),logp,1,1,lm,undef )
            call sigtopl( dvdto(i,j,L),dvdti(i,j,1:lm),logpl(i,j,1:lm),logps(i,j),logpt(i,j),logp,1,1,lm,undef )
            call sigtopl( dtdto(i,j,L),dtdti(i,j,1:lm),logpl(i,j,1:lm),logps(i,j),logpt(i,j),logp,1,1,lm,undef )
            call sigtopl( dqdto(i,j,L),dqdti(i,j,1:lm),logpl(i,j,1:lm),logps(i,j),logpt(i,j),logp,1,1,lm,undef )
            call sigtopl( dodto(i,j,L),dodti(i,j,1:lm),logpl(i,j,1:lm),logps(i,j),logpt(i,j),logp,1,1,lm,undef )
         enddo
         enddo
         enddo

c Edge-Level Fields
c ---=-------------
         do L=1,lm+1
            dpdto(:,:,L) = ak_out(L) + dpdti(:,:,lm+1)*bk_out(L)
         enddo

      endif

C **********************************************************************
C ****                  Write dycore internal Restart               ****
C **********************************************************************

      write(date,101) nymd
      write(hour,102) nhms/10000
      write(cim ,103) im
      write(cjm ,103) jm
      write(clm ,102) lm
  101 format(i8.8)
  102 format(i2.2)
  103 format(i3.3)

      dynrst = trim(dynrst) // '.r' // cim  // 'x' // cjm  // 'x' // clm // 
     .                         '.e' // date // '_' // hour // 'z'

      moistrst = trim(moistrst) // '.r' // cim  // 'x' // cjm  // 'x' // clm // 
     .                             '.e' // date // '_' // hour // 'z'

      print *
      print *, '              Creating: ',trim(dynrst)
      print *, '              Creating: ',trim(moistrst)
      print *, '     output resolution: ',im,jm,lm
      print *, '                  date: ',nymd,nhms
      print *
      open (10,file=trim(dynrst),form='unformatted',access='sequential')

      headr2(3) = lm
      write(10) headr1
      write(10) headr2

      write(10) ak_out
      write(10) bk_out

          do L=1,lm
             write(10) ((  u_out(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(10) ((  v_out(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(10) ((thv_out(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm+1
             write(10) ((ple_out(i,j,L),i=1,im),j=1,jm)
          enddo
          do L=1,lm
             write(10) (( pk_out(i,j,L),i=1,im),j=1,jm)
          enddo

          close (10)

C **********************************************************************
C ****                  Write moist internal Restart                ****
C **********************************************************************

      open  (10,file=trim(moistrst),form='unformatted',access='sequential')
      do n=1,nt
      do L=1,lm
         dum(:,:) = q_out(:,:,L,n)
         write(10)  dum
      enddo
      enddo
      close (10)

C **********************************************************************
C ****                     Write AGCM Import Restart                ****
C **********************************************************************

      if( trim(iaurst).ne.'xxx' ) then

      iaurst = trim(iaurst) // '.r' // cim  // 'x' // cjm  // 'x' // clm // 
     .                         '.e' // date // '_' // hour // 'z'

      print *, '              Creating: ',trim(iaurst)
      print *
      open  (10,file=trim(iaurst),form='unformatted',access='sequential')

      do L=1,lm
                   dum = dudto(:,:,L)
         write(10) dum
      enddo
      do L=1,lm
                   dum = dvdto(:,:,L)
         write(10) dum
      enddo
      do L=1,lm
                   dum = dtdto(:,:,L)
         write(10) dum
      enddo
      do L=1,lm+1
                   dum = dpdto(:,:,L)
         write(10) dum
      enddo
      do L=1,lm
                   dum = dqdto(:,:,L)
         write(10) dum
      enddo
      do L=1,lm
                   dum = dodto(:,:,L)
         write(10) dum
      enddo

      close (10)
      endif

      stop
      end

      subroutine remap  ( ps1,dp1,u1,v1,thv1,q1,phis1,lm1,
     .                    ps2,dp2,u2,v2,thv2,q2,phis2,lm2,im,jm,nt,ak,bk )

c *******************************************************************************
c *****                                                                     *****
c *****   Program to remap Target analysis variables (ps2,dp2,u2,v2,t2,q2)  *****
c *****   onto Model grid variables (ps1,dp1,u1,v1,thv1,q1).  Program       *****
c *****   allows for differenct topographies between Analysis and Model.    *****
c *****                                                                     *****
c *******************************************************************************

      use MAPL_ConstantsMod
      implicit none
      integer  im,jm,lm1,lm2,nt

c Output Model variables
c ----------------------
      real      dp1(im,jm,lm1)
      real       u1(im,jm,lm1)
      real       v1(im,jm,lm1)
      real     thv1(im,jm,lm1)
      real       q1(im,jm,lm1,nt)
      real      ps1(im,jm)

      real   phis1(im,jm)
      real      ak(lm1+1)
      real      bk(lm1+1)
      real    sige(lm1+1)

c Input Analysis variables
c ------------------------
      real     dp2(im,jm,lm2)
      real      u2(im,jm,lm2)
      real      v2(im,jm,lm2)
      real      t2(im,jm,lm2)
      real    thv2(im,jm,lm2)
      real      q2(im,jm,lm2,nt)
      real     ps2(im,jm)
      real   phis2(im,jm)

c Local variables
c ---------------
      real    pz(im,jm)
      real   pe0(im,jm,lm1+1)
      real   pe1(im,jm,lm1+1)
      real   pe2(im,jm,lm2+1)
      real   pk (im,jm,lm2  )
      real  pke0(im,jm,lm1+1)
      real  pke1(im,jm,lm1+1)
      real  pke2(im,jm,lm2+1)

      real    kappa,cp,pl,dum
      real    rgas,pref,ptop,psrf,tref,pkref,tstar,eps,rvap,grav
      integer i,j,L

      if( lm1.ne.lm2 ) then
          print *, 'Output Vertical Levels must match INPUT'
          stop
      endif

      kappa = MAPL_KAPPA
      rgas  = MAPL_RGAS
      rvap  = MAPL_RVAP
      grav  = MAPL_GRAV
      cp    = MAPL_CP
      eps   = rvap/rgas-1.0

c Construct Analysis Edge-Pressures from Input PS and DP
c ------------------------------------------------------
      call set_eta ( lm2,dum,ak,bk )

      do L=lm2+1,1,-1
      pe2(:,:,L) = ak(L) + bk(L)*ps2(:,:)
      enddo
      pke2(:,:,:) = pe2(:,:,:)**kappa

c Construct Virtual Potential Temperature
c ---------------------------------------
      thv2 = thv2*(1+eps*q2(:,:,:,1))

      
c Convert Eta Surface to Standard Sigma Surface
c ---------------------------------------------
      psrf = 100000.0
      ptop = ak(1)
      do L=1,lm1+1
      pref    = ak(L)+bk(L)*psrf
      sige(L) = ( pref-ptop )/( psrf-ptop )
      enddo
      sige(1)     = 0.0
      sige(lm1+1) = 1.0
      do L=1,lm1+1
      bk(L) =         sige(L)
      ak(L) = ptop*(1-sige(L))
      enddo

c Compute Output pressure variables
c ---------------------------------
      ps1 = ps2

      do L=1,lm1+1
      do j=1,jm
      do i=1,im
       pe1 (i,j,L) = ak(L)+bk(L)*ps1(i,j)
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

c Map target analysis onto grid defined by new surface pressure
c -------------------------------------------------------------
      print *, 'Calling GMAP, LM_in : ',lm2
      print *, '              LM_out: ',lm1
      call gmap ( im,jm,nt, kappa,
     .            lm2,  pke2,  pe2, u2,  v2,  thv2,  q2,
     .            lm1,  pke1,  pe1, u1,  v1,  thv1,  q1 )

c Construct Output Dry Potential Temperature
c ------------------------------------------
      thv1 = thv1/(1+eps*q1(:,:,:,1))

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine gmap(im, jm, nq,  akap,
     &         km,  pk3d_m,  pe3d_m, u_m,  v_m,  pt_m,  q_m,
     &         kn,  pk3d_n,  pe3d_n, u_n,  v_n,  pt_n,  q_n  )
c****6***0*********0*********0*********0*********0*********0**********72

      implicit none

      integer im, jm
      integer km, kn, nq

C Input:
C original data km-level

      real      u_m(im,jm,km)
      real      v_m(im,jm,km)
      real     pt_m(im,jm,km)
      real      q_m(im,jm,km,nq)
      real   pk3d_m(im,jm,km+1)
      real   pe3d_m(im,jm,km+1)


C Output:
C New data (kn-level)
      real      u_n(im,jm,kn)
      real      v_n(im,jm,kn)
      real     pt_n(im,jm,kn)
      real      q_n(im,jm,kn,nq)
      real   pk3d_n(im,jm,kn+1)
      real   pe3d_n(im,jm,kn+1)

c local (private)
      integer i, j, k, ic, n

      real pe1(im,km+1),pe2(im,kn+1)
      real pk1(im,km+1),pk2(im,kn+1)
      real dp1(im,km)  ,dp2(im,kn)
      real  u1(im,km)  , u2(im,kn)
      real  v1(im,km)  , v2(im,kn)
      real  t1(im,km)  , t2(im,kn)
      real  q1(im,km)  , q2(im,kn)

      real ptop
      real akap
      real ple,  pek, dak, bkh
      real undef
      real big
      parameter ( undef = 1.e15 )
      parameter (   big = 1.e10 )


      do 2000 j=1,jm

c Copy original data to local 2D arrays.

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
       t1(i,k) = pt_m(i,j,k)
      enddo
      enddo

      do k=1,kn
      do i=1,im
      dp2(i,k) = pk2(i,k+1)-pk2(i,k)
      enddo
      enddo

c map pt
c ------
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

c map u,v
c -------
      call mappm ( km, pe1, dp1, u1, kn, pe2, dp2, u2, im, -1, 7 )
      call mappm ( km, pe1, dp1, v1, kn, pe2, dp2, v2, im, -1, 7 )

c map q
c -------
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
       pt_n(i,j,k) = t2(i,k)
      enddo
      enddo

2000  continue

      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine mappm(km, pe1, dp1, q1, kn, pe2, dp2, q2, im, iv, kord)
C****6***0*********0*********0*********0*********0*********0**********72
C IV = 0: constituents
C IV = 1: potential temp
C IV =-1: winds
C
C Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
C
C pe1: pressure at layer edges (from model top to bottom surface)
C      in the original vertical coordinate
C pe2: pressure at layer edges (from model top to bottom surface)
C      in the new vertical coordinate

      parameter (kmax = 200)
      parameter (R3 = 1./3., R23 = 2./3.)

      real dp1(im,km),   dp2(im,kn),
     &      q1(im,km),    q2(im,kn),
     &     pe1(im,km+1), pe2(im,kn+1)
      integer kord

C local work arrays
      real a4(4,im,km)

      do k=1,km
         do i=1,im
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      call ppm2m(a4, dp1, im, km, iv, kord)

C Lowest layer: constant distribution
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
         elseif(pe2(i,k  ) .lt. pe1(i,1) .and.
     &          pe2(i,k+1) .gt. pe1(i,1))  then
! Part of the grid above ptop
            q2(i,k) = a4(1,i,1)
         else

         do 45 L=k0,km
! locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and.
     &       pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

! entire new grid is within the original grid
               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = R3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L)
     &                 - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else
! Fractional area...
              delp = pe1(i,L+1) - pe2(i,k)
              TT   = R3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+
     &               a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
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
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl*
     &           (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
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

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine ppm2m(a4,delp,im,km,iv,kord)
c****6***0*********0*********0*********0*********0*********0**********72
c iv = 0: positive definite scalars
c iv = 1: others
c iv =-1: winds

      implicit none

      integer im, km, lmt, iv
      integer kord
      integer i, k, km1
      real a4(4,im,km), delp(im,km)

c local arrays.
      real dc(im,km),delq(im,km)
      real h2(im,km)
      real a1, a2, a3, b2, c1, c2, c3, d1, d2, f1, f2, f3, f4
      real s1, s2, s3, s4, ss3, s32, s34, s42, sc
      real qmax, qmin, cmax, cmin
      real dm, qm, dq, tmp

C Local scalars:
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
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /
     &                              (a4(4,i,k)+delp(i,k+1))
      qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
      qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

c****6***0*********0*********0*********0*********0*********0**********72
c 4th order interpolation of the provisional cell edge value
c****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) *
     &          ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -
     &                          delp(i,k-1)*a1*dc(i,k  ) )
12    continue

C Area preserving cubic with 2nd deriv. = 0 at the boundaries
C Top
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
C No over- and undershoot condition
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

c****6***0*********0*********0*********0*********0*********0**********72

c Bottom
c Area preserving cubic with 2nd deriv. = 0 at the surface
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
c****6***0*********0*********0*********0*********0*********0**********72
c No over- and undershoot condition
      cmax = max(a4(1,i,km), a4(1,i,km1))
      cmin = min(a4(1,i,km), a4(1,i,km1))
      a4(2,i,km) = max(cmin,a4(2,i,km))
      a4(2,i,km) = min(cmax,a4(2,i,km))
c****6***0*********0*********0*********0*********0*********0**********72
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
c
c f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
c

c Top 2 and bottom 2 layers always use monotonic mapping

      do k=1,2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
      enddo

      if(kord == 7) then
c****6***0*********0*********0*********0*********0*********0**********72
C Huynh's 2nd constraint
c****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=1,im
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 4000 k=3, km-2
      do 3000 i=1, im
C Right edges
         qmp   = a4(1,i,k)                 + 2.0*delq(i,k-1)
         lac   = a4(1,i,k) + 1.5*h2(i,k-1) + 0.5*delq(i,k-1)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(3,i,k) = min(max(a4(3,i,k), qmin), qmax)
C Left  edges
         qmp   = a4(1,i,k)                 - 2.0*delq(i,k)
         lac   = a4(1,i,k) + 1.5*h2(i,k+1) - 0.5*delq(i,k)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
         a4(2,i,k) = min(max(a4(2,i,k), qmin), qmax)
C Recompute A6
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

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine kmppm(dm, a4, km, lmt)
c****6***0*********0*********0*********0*********0*********0**********72
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
c Positive definite

c Positive definite constraint
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

      subroutine usage()
      print *, "Usage:  "
      print *
      print *, " rs_vinterp.x   [-dynrst     dynrst_fname]  Default: fvcore_internal_restart"
      print *, "                [-moistrst moistrst_fname]  Default: moist_internal_restart"
      print *, "                [-topo         topo_fname]  "
      print *
      print *, "where:"
      print *, "-----"
      print *, "  -dynrst     dynrst_fname:  Filename of dynamics internal restart"
      print *, "  -moistrst moistrst_fname:  Filename of    moist internal restart"
      print *, "  -topo         topo_fname:  Filename of topography dataset"
      print *
      print *, "creates:"
      print *, "-------"
      print *, "     New dynrst & moistrst restart files for ARIES"
      print *
      call exit(7)
      end

      SUBROUTINE CTOA ( qc,qa,im,jm,km,itype )
C                                                                       
C ******************************************************************    
C ****                                                          ****    
C ****  This program converts 'C' gridded data                  ****    
C ****                     to 'A' gridded data.                 ****    
C ****                                                          ****    
C ****  An FFT shift transformation is made in x for itype = 1  ****    
C ****  An FFT shift transformation is made in y for itype = 2  ****    
C ****                                                          ****    
C ******************************************************************    

      real*8 qa   (im,jm,km)
      real*8 qc   (im,jm,km)
      real*8 qz   (im,jm,km)

      real*8 qcx  (   im+2 ,km)                                          
      real*8  cx  (2*(im+2),km)                                          
      real*8 qcy  (   2*jm ,km)                                          
      real*8  cy  (2*(2*jm),km)                                          

      real*8 cosx (im/2), sinx(im/2)
      real*8 cosy (jm)  , siny(jm)
      integer IFX  (100) , IFY (100)
      REAL*8    TRIGX(3*(IM+1))
      REAL*8    TRIGY(3*(2*JM+1))                                          
      REAL*8    pi,dx,dy

      parameter ( zero  = 0.0 )
      parameter ( ahalf = 0.5 )

      imh   = IM/2
      JMM1  = JM-1
      JMM2  = JM-2
      JMM3  = JM-3
      JP    = 2*jmm1

      PI    = 4.*ATAN(1.)
      DX    = 2*PI/IM
      DY    = PI/JMM1

C *********************************************************             
C ****     define sinx, cosx and trigs coefficients    ****             
C *********************************************************             

      call fftfax (im,ifx,trigx)
      do k=1,imh
      thx = k*dx*ahalf
      cosx(k) = cos(thx)
      sinx(k) = sin(thx)
      enddo

      call fftfax (jp,ify,trigy)
      do l=1,jmm1
      thy = l*dy*ahalf
      cosy(l) = cos(thy)
      siny(l) = sin(thy)
      enddo

C *********************************************************             
C ****       load input array into temporary array     ****             
C *********************************************************             

      do i = 1,im*jm*km
      qz(i,1,1) = qc(i,1,1)
      enddo

C *********************************************************             
C ****           stagger in x-direction                ****             
C *********************************************************             

      if (itype.eq.1) then
      do j=1,jm

      do l=1,km
      do i=1,im
      qcx(i,l) = qz(i,j,l)
      enddo
      enddo
      call rfftmlt (qcx,cx,trigx,ifx,1 ,im+2,im,km,-1)                   

      do l=1,km
      do k=1,imh
      kr = 2*k+1
      ki = 2*k+2
      crprime = qcx(kr,l)*cosx(k) + qcx(ki,l)*sinx(k)
      ciprime = qcx(ki,l)*cosx(k) - qcx(kr,l)*sinx(k)
      qcx(kr,l) = crprime
      qcx(ki,l) = ciprime
      enddo
      enddo

      call rfftmlt (qcx,cx,trigx,ifx,1 ,im+2,im,km, 1)                   
      do l=1,km
      do i=1,im
      qa(i,j,l) = qcx(i,l)
      enddo
      enddo
      enddo

      endif

C *********************************************************             
C ****           stagger in y-direction                ****             
C *********************************************************             

      if (itype.eq.2) then

      do i=1,imh

      do l=1,km
      do j=1,jmm1
      qcy(j,l)      =  qz(i,j,l)
      qcy(j+jmm1,l) = -qz(i+imh,jmm1-j+1,l)
      enddo
      enddo

      call rfftmlt (qcy,cy,trigy,ify,1 ,jp+2,jp,km,-1)                   

      do l=1,km
      do k=1,jmm1
      kr = 2*k+1
      ki = 2*k+2
      crprime = qcy(kr,l)*cosy(k) + qcy(ki,l)*siny(k)
      ciprime = qcy(ki,l)*cosy(k) - qcy(kr,l)*siny(k)
      qcy(kr,l) = crprime
      qcy(ki,l) = ciprime
      enddo
      enddo

      call rfftmlt (qcy,cy,trigy,ify,1 ,jp+2,jp,km, 1)                   

      do l=1,km
      do j=1,jmm1
      qa(i,j,l)          =  qcy(j,l)
      qa(i+imh,jm-j+1,l) = -qcy(j+jmm1,l)
      enddo
      enddo
      enddo
      do l=1,km
      do i=1,imh
      qa(i+imh,1,l) = -qa(i,1,l)
      qa(i,jm,l)    = -qa(i+imh,jm,l)
      enddo
      enddo
      endif

      return
      END

      SUBROUTINE ATOC ( qa,qc,im,jm,km,itype )
!
! ******************************************************************
! ****                                                          ****
! ****  This program converts 'A' gridded data                  ****
! ****                     to 'C' gridded data.                 ****
! ****                                                          ****
! ****  An FFT shift transformation is made in x for itype = 1  ****
! ****  An FFT shift transformation is made in y for itype = 2  ****
! ****                                                          ****
! ******************************************************************

      real*8 qa   (im,jm,km)
      real*8 qc   (im,jm,km)
      real*8 qz   (im,jm,km)

      real*8 qax  (   im+2 ,km)
      real*8  cx  (2*(im+2),km)
      real*8 qay  (   2*jm ,km)
      real*8  cy  (2*(2*jm),km)

      real*8 cosx (im/2), sinx(im/2)
      real*8 cosy (jm)  , siny(jm)
      integer IFX  (100) , IFY (100)
      REAL*8      TRIGX(  3*(IM+1))
      REAL*8      TRIGY(3*(2*JM+1))
      REAL*8      dx,dy,pi

      parameter ( zero  = 0.0 )
      parameter ( ahalf = 0.5 )

      JMM1  = JM-1
      JMM2  = JM-2
      JMM3  = JM-3
      JP    = 2*jmm1

      IMD2  = IM/2
      PI    = 4.0*ATAN(1.0)
      DX    = 2*PI/IM
      DY    = PI/JMM1

! *********************************************************
! ****     define sinx, cosx and trigs coefficients    ****
! *********************************************************

      call fftfax (im,ifx,trigx)
      do k=1,im/2
      thx = k*dx*ahalf
      cosx(k) = cos(thx)
      sinx(k) = sin(thx)
      enddo

      call fftfax (jp,ify,trigy)
      do l=1,jmm1
      thy = l*dy*ahalf
      cosy(l) = cos(thy)
      siny(l) = sin(thy)
      enddo

! *********************************************************
! ****       load input array into temporary array     ****
! *********************************************************

      do i = 1,im*jm*km
      qz(i,1,1) = qa(i,1,1)
      enddo

! *********************************************************
! ****           stagger in x-direction                ****
! *********************************************************

      if (itype.eq.1) then
      do j=1,jm

      do l=1,km
      do i=1,im
      qax(i,l) = qz(i,j,l)
      enddo
      enddo
      call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,km,-1)

      do l=1,km
      do k=1,IMD2
      kr = 2*k+1
      ki = 2*k+2
      crprime = qax(kr,l)*cosx(k) - qax(ki,l)*sinx(k)
      ciprime = qax(ki,l)*cosx(k) + qax(kr,l)*sinx(k)
      qax(kr,l) = crprime
      qax(ki,l) = ciprime
      enddo
      enddo

      call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,km, 1)
      do l=1,km
      do i=1,im
      qc(i,j,l) = qax(i,l)
      enddo
      enddo

      enddo

      endif

C *********************************************************
C ****           stagger in y-direction                ****
C *********************************************************

      if (itype.eq.2) then

      do i=1,im/2

      do l=1,km
      do j=1,jmm1
      qay(j,l)      =  qz(i,j,l)
      qay(j+jmm1,l) = -qz(i+imd2,jm-j+1,l)
      enddo
      enddo

      call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,km,-1)

      do l=1,km
      do k=1,jmm1
      kr = 2*k+1
      ki = 2*k+2
      crprime = qay(kr,l)*cosy(k) - qay(ki,l)*siny(k)
      ciprime = qay(ki,l)*cosy(k) + qay(kr,l)*siny(k)
      qay(kr,l) = crprime
      qay(ki,l) = ciprime
      enddo
      enddo

      call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,km, 1)

      do l=1,km
      do j=1,jmm1
      qc(i,j,l)             =  qay(j,l)
      qc(i+imd2,jmm1-j+1,l) = -qay(j+jmm1,l)
      enddo
      enddo

      enddo

      endif

      RETURN
      END

      subroutine writit (q,im,jm,lm,ku)
      real q(im,jm,lm)
      real*4 a(im,jm)
      do L=1,lm
      a(:,:) = q(:,:,L)
      write(ku) a
      enddo
      return
      end

      subroutine atod ( qa,qd,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'A' gridded data                  ****
C ****                     to 'D' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted left (westward),                     ****
C ****        u is shifted down (southward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift left (-dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qa(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) + qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) - qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qd(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****             shift down (-dy/2)                  ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qa(i,j+1,L)
         qay(j+jmm1,L) = -qa(i+imh,jm-j,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) + qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) - qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qd(i,j+1,L)        =  qay(j,L)
         qd(i+imh,jm-j+1,L) = -qay(j+jmm1,L)
         enddo
         enddo
      enddo

      endif

      return
      end

      subroutine dtoa ( qd,qa,im,jm,lm,itype )
 
C ******************************************************************
C ****                                                          ****
C ****  This program converts 'D' gridded data                  ****
C ****                     to 'A' gridded data.                 ****
C ****                                                          ****
C ****  The D-Grid Triplet is defined as:                       ****
C ****                                                          ****
C ****              u(i,j+1)                                    ****
C ****                |                                         ****
C ****     v(i,j)---delp(i,j)---v(i+1,j)                        ****
C ****                |                                         ****
C ****              u(i,j)                                      ****
C ****                                                          ****
C ****  Thus, v is shifted right (eastward),                    ****
C ****        u is shifted up   (northward)                     ****
C ****                                                          ****
C ****  An FFT shift transformation is made in x for itype = 1  ****
C ****  An FFT shift transformation is made in y for itype = 2  ****
C ****                                                          ****
C ******************************************************************

      real qa   (im,jm,lm)
      real qd   (im,jm,lm)

      real qax  (   im+2 ,lm)
      real  cx  (2*(im+2),lm)
      real qay  (   2*jm ,lm)
      real  cy  (2*(2*jm),lm)

      real    cosx (im/2), sinx(im/2)
      real    cosy (jm)  , siny(jm)
      real      trigx(3*(im+1))
      real      trigy(3*(2*jm))

      integer IFX (100)
      integer IFY (100)

      jmm1  = jm-1
      jp    = 2*jmm1

      imh   = im/2
      pi    = 4.0*atan(1.0)
      dx    = 2*pi/im
      dy    = pi/jmm1

C *********************************************************
C ****             shift right (dx/2)                  ****
C *********************************************************

      if (itype.eq.1) then

         call fftfax (im,ifx,trigx)

         do k=1,imh 
         thx = k*dx*0.5
         cosx(k) = cos(thx)
         sinx(k) = sin(thx)
         enddo

      do j=1,jm
         do L=1,lm
         do i=1,im
         qax(i,L) = qd(i,j,L)
         enddo
         enddo
         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm,-1)

         do L=1,lm
         do k=1,imh 
         kr = 2*k+1
         ki = 2*k+2
         crprime = qax(kr,L)*cosx(k) - qax(ki,L)*sinx(k)
         ciprime = qax(ki,L)*cosx(k) + qax(kr,L)*sinx(k)
         qax(kr,L) = crprime
         qax(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qax,cx,trigx,ifx,1 ,im+2,im,lm, 1)
         do L=1,lm
         do i=1,im
         qa(i,j,L) = qax(i,L)
         enddo
         enddo
      enddo

      endif

C *********************************************************
C ****               shift up (dy/2)                   ****
C *********************************************************

      if (itype.eq.2) then

         call fftfax (jp,ify,trigy)

         do L=1,jmm1
         thy = L*dy*0.5
         cosy(L) = cos(thy)
         siny(L) = sin(thy)
         enddo

      do i=1,imh 
         do L=1,lm
         do j=1,jmm1
         qay(j,L)      =  qd(i,j+1,L)
         qay(j+jmm1,L) = -qd(i+imh,jm-j+1,L)
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm,-1)

         do L=1,lm
         do k=1,jmm1
         kr = 2*k+1
         ki = 2*k+2
         crprime = qay(kr,L)*cosy(k) - qay(ki,L)*siny(k)
         ciprime = qay(ki,L)*cosy(k) + qay(kr,L)*siny(k)
         qay(kr,L) = crprime
         qay(ki,L) = ciprime
         enddo
         enddo

         call rfftmlt (qay,cy,trigy,ify,1 ,jp+2,jp,lm, 1)

         do L=1,lm
         do j=1,jmm1
         qa(i,j+1,L)      =  qay(j,L)
         qa(i+imh,jm-j,L) = -qay(j+jmm1,L)
         enddo
         enddo

      enddo

         do L=1,lm
         do i=1,imh
         qa(i+imh,jm,L) = -qa(i,jm,L)
         qa(i,1,L)      = -qa(i+imh,1,L)
         enddo
         enddo
      endif

      return
      end

      subroutine rfftmlt (a,work,trigs,ifax,inc,jump,n,lot,isign)
      integer INC, JUMP, N, LOT, ISIGN
      real(kind=KIND(1.0)) A(N),WORK(N),TRIGS(N)
      integer IFAX(*)
!
!     SUBROUTINE "FFT991" - MULTIPLE REAL/HALF-COMPLEX PERIODIC
!     FAST FOURIER TRANSFORM
!
!     SAME AS FFT99 EXCEPT THAT ORDERING OF DATA CORRESPONDS TO
!     THAT IN MRFFT2
!
!     PROCEDURE USED TO CONVERT TO HALF-LENGTH COMPLEX TRANSFORM
!     IS GIVEN BY COOLEY, LEWIS AND WELCH (J. SOUND VIB., VOL. 12
!     (1970), 315-337)
!
!     A IS THE ARRAY CONTAINING INPUT AND OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*LOT
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N/2
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N IS THE LENGTH OF THE DATA VECTORS
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:
!         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
!         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
!
!     ORDERING OF DATA:
!         X(0),X(1),X(2),...,X(N-1)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS IN
!     PARALLEL
!
!     *** N.B. N IS ASSUMED TO BE AN EVEN NUMBER
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!
!     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
!               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
!
! THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR
!     CALL Q8QST4 ( 4HXLIB, 6HFFT99F, 6HFFT991, 10HVERSION 01)
!FPP$ NOVECTOR R
      integer NFAX, NH, NX, INK
      integer I, J, IBASE, JBASE, L, IGO, IA, LA, K, M, IB

      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      INK=INC+INC
      IF (ISIGN.EQ.+1) GO TO 30
!
!     IF NECESSARY, TRANSFER DATA TO WORK AREA
      IGO=50
      IF (MOD(NFAX,2).EQ.1) GOTO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE
!
      IGO=60
      GO TO 40
!
!     PREPROCESSING (ISIGN=+1)
!     ------------------------
!
   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60
!
!     COMPLEX TRANSFORM
!     -----------------
!
   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF (IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS, 
     *    INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS, 
     *     2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE
!
      IF (ISIGN.EQ.-1) GO TO 130
!
!     IF NECESSARY, TRANSFER DATA FROM WORK AREA
      IF (MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE
!
!     FILL IN ZEROS AT END
  110 CONTINUE
      IB=N*INC+1
!DIR$ IVDEP
      DO 120 L=1,LOT
      A(IB)=0.0
      A(IB+INC)=0.0
      IB=IB+JUMP
  120 CONTINUE
      GO TO 140
!
!     POSTPROCESSING (ISIGN=-1):
!     --------------------------
!
  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
!
  140 CONTINUE
      RETURN
      END

      subroutine fftfax (n,ifax,trigs)
      integer IFAX(13)
      integer N
      REAL(kind=KIND(1.0))      TRIGS(*)
!
! MODE 3 IS USED FOR REAL/HALF-COMPLEX TRANSFORMS.  IT IS POSSIBLE
! TO DO COMPLEX/COMPLEX TRANSFORMS WITH OTHER VALUES OF MODE, BUT
! DOCUMENTATION OF THE DETAILS WERE NOT AVAILABLE WHEN THIS ROUTINE
! WAS WRITTEN.
!
      integer I, MODE
      DATA MODE /3/
!FPP$ NOVECTOR R
      CALL FAX (IFAX, N, MODE)
      I = IFAX(1)
      IF (IFAX(I+1) .GT. 5 .OR. N .LE. 4) IFAX(1) = -99
      IF (IFAX(1) .LE. 0 ) WRITE(6,FMT="(//5X, ' FFTFAX -- INVALID N =', I5,/)") N
      IF (IFAX(1) .LE. 0 ) STOP 999
      CALL FFTRIG (TRIGS, N, MODE)
      RETURN
      END

      subroutine fft99a (a,work,trigs,inc,jump,n,lot)
      integer inc, jump, N, lot
      real(kind=KIND(1.0)) A(N),WORK(N)
      REAL(kind=KIND(1.0)) TRIGS(N)
!
!     SUBROUTINE FFT99A - PREPROCESSING STEP FOR FFT99, ISIGN=+1
!     (SPECTRAL TO GRIDPOINT TRANSFORM)
!
!FPP$ NOVECTOR R
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) C, S
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
!DIR$ IVDEP
      DO 10 L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      WORK(JA)=(A(IA)+A(IB))- 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+ 
     *     (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+ 
     *     (A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))- 
     *     (A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   20 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
!DIR$ IVDEP
      DO 40 L=1,LOT
      WORK(JA)=2.0*A(IA)
      WORK(JA+1)=-2.0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fft99b (work,a,trigs,inc,jump,n,lot)
      integer INC, JUMP, N, LOT
      real(kind=KIND(1.0)) WORK(N),A(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
      integer NH, NX, INK, IA, IB, JA, JB, K, L
      integer IABASE, IBBASE, JABASE, JBBASE
      real(kind=KIND(1.0)) SCALE
      real(kind=KIND(1.0)) C, S
!
!     SUBROUTINE FFT99B - POSTPROCESSING STEP FOR FFT99, ISIGN=-1
!     (GRIDPOINT TO SPECTRAL TRANSFORM)
!
!FPP$ NOVECTOR R
      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      SCALE=1.0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1
!DIR$ IVDEP
      DO 10 L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JB)=SCALE*(WORK(IA)-WORK(IB))
      A(JA+INC)=0.0
      A(JB+INC)=0.0
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      SCALE=0.5*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
!DIR$ IVDEP
      DO 20 L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB)) 
     *    +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB)) 
     *    -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *     -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   20 CONTINUE
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
      SCALE=2.0*SCALE
!DIR$ IVDEP
      DO 40 L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
   40 CONTINUE
!
   50 CONTINUE
      RETURN
      END

      subroutine fax (ifax,n,mode)
      integer IFAX(10)
      integer N, MODE
!FPP$ NOVECTOR R
      integer NN, K, L, INC, II, ISTOP, ITEM, NFAX, I
      NN=N
      IF (IABS(MODE).EQ.1) GO TO 10
      IF (IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF ((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
   10 K=1
!     TEST FOR FACTORS OF 4
   20 IF (MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF (NN.EQ.1) GO TO 80
      GO TO 20
!     TEST FOR EXTRA FACTOR OF 2
   30 IF (MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF (NN.EQ.1) GO TO 80
!     TEST FOR FACTORS OF 3
   40 IF (MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF (NN.EQ.1) GO TO 80
      GO TO 40
!     NOW FIND REMAINING FACTORS
   50 L=5
      INC=2
!     INC ALTERNATELY TAKES ON VALUES 2 AND 4
   60 IF (MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF (NN.EQ.1) GO TO 80
      GO TO 60
   70 L=L+INC
      INC=6-INC
      GO TO 60
   80 IFAX(1)=K-1
!     IFAX(1) CONTAINS NUMBER OF FACTORS
      NFAX=IFAX(1)
!     SORT FACTORS INTO ASCENDING ORDER
      IF (NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
      END

      subroutine fftrig (trigs,n,mode)
      REAL(kind=KIND(1.0))      TRIGS(*)
      integer N, MODE
!FPP$ NOVECTOR R
      real(kind=KIND(1.0)) PI
      integer IMODE, NN, L, I, NH, LA
      real(kind=KIND(1.0)) DEL, ANGLE
      PI=2.0*ASIN(1.0)
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF (IMODE.EQ.1) RETURN
      IF (IMODE.EQ.8) RETURN
      DEL=0.5*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF (IMODE.LE.3) RETURN
      DEL=0.5*DEL
      LA=LA+NN
      IF (MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0*SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
      DEL=0.5*DEL
      DO 50 I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
      END

      subroutine vpassm (a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,ifac,la)
      integer INC1,INC2,INC3,INC4,LOT,N,IFAC,LA
      real(kind=KIND(1.0)) A(N),B(N),C(N),D(N)
      REAL(kind=KIND(1.0))      TRIGS(N)
!
!     SUBROUTINE "VPASSM" - MULTIPLE VERSION OF "VPASSA"
!     PERFORMS ONE PASS THROUGH DATA
!     AS PART OF MULTIPLE COMPLEX FFT ROUTINE
!     A IS FIRST REAL INPUT VECTOR
!     B IS FIRST IMAGINARY INPUT VECTOR
!     C IS FIRST REAL OUTPUT VECTOR
!     D IS FIRST IMAGINARY OUTPUT VECTOR
!     TRIGS IS PRECALCULATED TABLE OF SINES & COSINES
!     INC1 IS ADDRESSING INCREMENT FOR A AND B
!     INC2 IS ADDRESSING INCREMENT FOR C AND D
!     INC3 IS ADDRESSING INCREMENT BETWEEN As & Bs
!     INC4 IS ADDRESSING INCREMENT BETWEEN Cs & Ds
!     LOT IS THE NUMBER OF VECTORS
!     N IS LENGTH OF VECTORS
!     IFAC IS CURRENT FACTOR OF N
!     LA IS PRODUCT OF PREVIOUS FACTORS
!
      real(kind=KIND(1.0)) SIN36, COS36, SIN72, COS72, SIN60
      DATA SIN36/0.587785252292473/,COS36/0.809016994374947/, 
     *      SIN72/0.951056516295154/,COS72/0.309016994374947/,
     *      SIN60/0.866025403784437/
      integer M, IINK, JINK, JUMP, IBASE, JBASE, IGO, IA, JA, IB, JB
      integer IC, JC, ID, JD, IE, JE
      integer I, J, K, L, IJK, LA1, KB, KC, KD, KE
      real(kind=KIND(1.0)) C1, S1, C2, S2, C3, S3, C4, S4
!
!FPP$ NOVECTOR R
      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.GT.4) RETURN
      GO TO (10,50,90,130),IGO
!
!     CODING FOR FACTOR 2
!
   10 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)-B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)-B(IB+I))
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 3
!
   50 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 55 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))
      C(JC+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))
      D(JB+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I)))
      D(JC+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I)))
      I=I+INC3
      J=J+INC4
   55 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 65 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)= 
     *     C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    -S2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) 
     *    +C2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
   65 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 4
!
   90 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 95 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      D(JC+J)=(B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I))
      C(JB+J)=(A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))
      C(JD+J)=(A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))
      D(JB+J)=(B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I))
      D(JD+J)=(B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I))
      I=I+INC3
      J=J+INC4
   95 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 105 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      C(JC+J)= 
     *     C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    -S2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      D(JC+J)= 
     *     S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) 
     *    +C2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      C(JB+J)= 
     *     C1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    -S1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)= 
     *     S1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) 
     *    +C1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)= 
     *     C3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    -S3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)= 
     *     S3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) 
     *    +C3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  105 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 5
!
  130 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      IE=ID+IINK
      JE=JD+JINK
      DO 140 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 135 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      C(JE+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *   +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      D(JB+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      D(JE+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *   -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      C(JC+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      C(JD+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *   +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      D(JC+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      D(JD+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *   -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  135 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  140 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 160 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      DO 150 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 145 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)= 
     *     C1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JB+J)= 
     *     S1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JE+J)= 
     *     C4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    -S4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JE+J)= 
     *     S4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) 
     *       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) 
     *    +C4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) 
     *       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JC+J)= 
     *     C2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JC+J)= 
     *     S2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      C(JD+J)= 
     *     C3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    -S3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JD+J)= 
     *     S3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) 
     *       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) 
     *    +C3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) 
     *       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      I=I+INC3
      J=J+INC4
  145 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  150 CONTINUE
      JBASE=JBASE+JUMP
  160 CONTINUE
      RETURN
      END

      subroutine checkfile ( filename )
      character(*)  filename
      logical file_exists
      inquire( file=trim(filename), exist=file_exists )
      if( file_exists )  return
      print *
      print *, 'File: ',trim(filename),' does not exist!'
      print *
      stop
      end

      subroutine windfix ( ua,va,plea,
     .                     ub,vb,pleb,
     .                     im,jm,lm,method )

      integer    im,jm,lm,method
      real    ua(im,jm,lm)
      real    va(im,jm,lm)
      real  plea(im,jm,lm+1)
      real    ub(im,jm,lm)
      real    vb(im,jm,lm)
      real  pleb(im,jm,lm+1)

      real     u(im,jm,lm)
      real     v(im,jm,lm)
      real   dpa(im,jm,lm)
      real   dpb(im,jm,lm)
      real  diva(im,jm,lm)
      real  divb(im,jm,lm)

      integer index(lm),ierror

      real, allocatable ::  uglo(:,:,:)
      real, allocatable ::  vglo(:,:,:)
      real, allocatable ::  dglo(:,:,:)
      real, allocatable :: dpglo(:,:,:)

      real, allocatable ::   sum1(:,:)
      real, allocatable ::   sum2(:,:)
      real, allocatable ::   sum3(:,:)
      real, allocatable :: lambda(:,:)

      real, allocatable ::    vor(:,:)
      real, allocatable ::    div(:,:)
      real, allocatable ::    chi(:,:)
      real, allocatable ::    psi(:,:)
      real, allocatable ::   chix(:,:)
      real, allocatable ::   chiy(:,:)
      real, allocatable ::   psix(:,:)
      real, allocatable ::   psiy(:,:)

      integer L, comm, myid, npes
      integer img, jmg

      img  = im
      jmg  = jm

      allocate (  uglo(img,jmg,lm) )
      allocate (  vglo(img,jmg,lm) )
      allocate (  dglo(img,jmg,lm) )
      allocate ( dpglo(img,jmg,lm) )

      allocate (   vor(img,jmg)    )
      allocate (   div(img,jmg)    )
      allocate (   chi(img,jmg)    )
      allocate (   psi(img,jmg)    )
      allocate (  chix(img,jmg)    )
      allocate (  chiy(img,jmg)    )
      allocate (  psix(img,jmg)    )
      allocate (  psiy(img,jmg)    )

      allocate (   sum1(im,jm)    )
      allocate (   sum2(im,jm)    )
      allocate (   sum3(im,jm)    )
      allocate ( lambda(im,jm)    )

C **********************************************************************
C ****   Modify Winds to Produce Vanishing Divergence Increment     ****
C **********************************************************************

! Compute Pressure Thickness
! --------------------------
      do L=1,lm
        dpa(:,:,L) = ( plea(:,:,L+1)-plea(:,:,L) )
        dpb(:,:,L) = ( pleb(:,:,L+1)-pleb(:,:,L) )
      enddo

c Compute Divergence on D-Grid
c ----------------------------
      do L=1,lm
      call getdivd (ub(1,1,L),vb(1,1,L),dpb(1,1,L),divb(1,1,L),img,jmg )               
      enddo

c Perform 5 Iterations for Convergence
c ------------------------------------
      print *
      do iter = 1,5

c Compute Divergence on C-Grid
c ----------------------------
      do L=1,lm
      call getdivc (ua(1,1,L),va(1,1,L),dpa(1,1,L),diva(1,1,L),img,jmg )               
      enddo

c Minimize Change
c ---------------
      if( method.eq.0 ) then
          sum1(:,:) = 0.0
          sum2(:,:) = 0.0
          sum3(:,:) = 0.0
          do L=1,lm
          sum1(:,:) = sum1(:,:) +  diva(:,:,L)
          sum2(:,:) = sum2(:,:) +  divb(:,:,L)
          enddo
                 lambda = (sum1-sum2) / lm
                 lambda =  sum1 / lm
          do L=1,lm
          diva(:,:,L) = -lambda(:,:)
          enddo
          sum3(:,:) = sum1(:,:)
          do L=1,lm
          sum3(:,:) = sum3(:,:) +  diva(:,:,L)
          enddo
      endif

c Minimize Relative Change
c ------------------------
      if( method.eq.1 ) then
          sum1(:,:) = 0.0
          sum2(:,:) = 0.0
          sum3(:,:) = 0.0
          do L=1,lm
          sum1(:,:) = sum1(:,:) +  diva(:,:,L)
          sum2(:,:) = sum2(:,:) +  divb(:,:,L)
          sum3(:,:) = sum3(:,:) + (diva(:,:,L)-divb(:,:,L))**2
          enddo
          where( sum3 .ne. 0.0 )
                 lambda = (sum1-sum2) / sum3
          elsewhere
                 lambda = 0.0
          endwhere
          do L=1,lm
          diva(:,:,L) = -lambda(:,:)*( diva(:,:,L)-divb(:,:,L) )**2
          enddo
          sum3(:,:) = sum1(:,:)
          do L=1,lm
          sum3(:,:) = sum3(:,:) +  diva(:,:,L)
          enddo
      endif

c Minimize Absolute Change
c ------------------------
      if( method.eq.2 ) then
          sum1(:,:) = 0.0
          sum2(:,:) = 0.0
          sum3(:,:) = 0.0
          do L=1,lm
          sum1(:,:) = sum1(:,:) +  diva(:,:,L)
          sum2(:,:) = sum2(:,:) +  divb(:,:,L)
          sum3(:,:) = sum3(:,:) +   dpa(:,:,L)**2
          enddo
          lambda = (sum1-sum2) / sum3
          do L=1,lm
          diva(:,:,L) = -lambda(:,:)*dpa(:,:,L)**2
          enddo
          sum3(:,:) = sum1(:,:)
          do L=1,lm
          sum3(:,:) = sum3(:,:) +  diva(:,:,L)
          enddo
      endif

      call writit ( sum1,im,jm,1,55 )
      call writit ( sum2,im,jm,1,55 )
      call writit ( sum3,im,jm,1,55 )

      dsum = 0.0
      do j=1,jm
      do i=1,im
      dsum = dsum + ( sum1(i,j)-sum2(i,j) )**2
      enddo
      enddo

c Compute Wind Increments Associated with Divergence Increment
c ------------------------------------------------------------
      do L=1,lm
          call laplace (diva(1,1,L),chi,img,jmg)
          call gradq   (chi,  chix,chiy,img,jmg)

      do j=1,jm
         i = im
      do ip1=1,im
      sum1(i,j) = 2*chix(i,j)/( dpa(i,j,L)+dpa(ip1,j,L) )
         i=ip1
      enddo
      enddo
      chix = sum1

      do j=1,jm-1
      do i=1,im
      sum1(i,j) = 2*chiy(i,j)/( dpa(i,j,L)+dpa(i,j+1,L) )
      enddo   
      enddo   
      sum1(:,jm) = 0.0
      chiy = sum1

          ua(:,:,L) = ua(:,:,L) + chix(:,:)
          va(:,:,L) = va(:,:,L) + chiy(:,:)
      enddo

      print *, 'Iter # ',iter,' Dsum: ',dsum
      enddo
      print *

      deallocate ( sum1,sum2,sum3,lambda )
      deallocate ( chi,chix,chiy         )
      deallocate ( uglo,vglo,dglo,dpglo  )

      return
      end

      subroutine minmax (q,qname,im,jm,qmin,qmax)
      real q(im,jm)
      character*(*) qname
      do j=1,jm
      do i=1,im
      if( q(i,j).gt.qmax ) qmax = q(i,j)
      if( q(i,j).lt.qmin ) qmin = q(i,j)
      enddo
      enddo
      return
      end

      SUBROUTINE GETDIVD ( U,V,DP,DIV,IM,JM )
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    U ....... ZONAL WIND                                    ****          
C ****    V ....... MERIDIONAL WIND                               ****          
C ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
      real      U(IM,JM)                                               
      real      V(IM,JM)                                               
      real     DP(IM,JM)                                               
      real    DIV(IM,JM)                                               
                                                                                
      real   u1y (IM,JM)                                               
      real   v1x (IM,JM)                                               
      real   P1X (IM,JM)                                               
      real   P1Y (IM,JM)                                               
      real   TMP1(IM,JM)                                               
      real   TMP2(IM,JM)                                               
      real  cosij(IM,JM)                                               
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      A    = 6.372e6
      pi   = 4.*atan(1.)
      dlon = 2*pi/ im
      dlat =   pi/(jm-1)
                                                                                
      C11 =  1.0 / (4.0*A*IM*(1.0-COS(0.5*dlat)))                           

      CX1 =  1.0 / (4.0*A*dlon)                                             
      CY1 =  1.0 / (4.0*A*dlat)                                             
                                                                                
      do j=2,jm-1
         phi = -pi/2.+(j-1)*dlat
      cosphi = cos(phi)
      do i=1,im
      cosij(i,j) = cosphi
      enddo
      enddo
      cosij(:,1)  = 0.0
      cosij(:,jm) = 0.0

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
      DO i=1,im
         u1y(i,j) = ( U(i,j)+U(i,j+1) )*0.5
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=2,jm-1                                                           
         i=im
      DO Ip1=1,im
         v1x(I,j) = ( V(I,J)+V(Ip1,j) )*0.5
         i=ip1
      ENDDO                                                                     
      ENDDO                                                                     
         v1x(:, 1) = 0.0
         v1x(:,jm) = 0.0
                                                                                
C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( U1y(ip1,j)+U1y(i,j) )*( DP(ip1,j)+DP(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = ( V1x(I,J+1)*COSIJ(I,J+1)+V1x(I,j)*COSIJ(I,j) )*( DP(I,J+1)+DP(I,J) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO 100 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      DIV(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GETDIVC( U,V,DP,DIV,IM,JM )
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    U ....... ZONAL WIND                                    ****          
C ****    V ....... MERIDIONAL WIND                               ****          
C ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
      real      U(IM,JM)                                               
      real      V(IM,JM)                                               
      real     DP(IM,JM)                                               
      real    DIV(IM,JM)                                               
                                                                                
      real   P1X (IM,JM)                                               
      real   P1Y (IM,JM)                                               
      real   TMP1(IM,JM)                                               
      real   TMP2(IM,JM)                                               
      real  cosij(IM,JM)                                               
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      A    = 6.372e6
      pi   = 4.*atan(1.)
      dlon = 2*pi/ im
      dlat =   pi/(jm-1)
                                                                                
      C11 =  1.0 / (4.0*A*IM*(1.0-COS(0.5*dlat)))                           

      CX1 =  1.0 / (4.0*A*dlon)                                             
      CY1 =  1.0 / (4.0*A*dlat)                                             
                                                                                
      do j=2,jm-1
         phi = -pi/2.+(j-1)*dlat
      cosphi = cos(phi)
      do i=1,im
      cosij(i,j) = cosphi
      enddo
      enddo
      cosij(:,1)  = 0.0
      cosij(:,jm) = 0.0

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( 2*U(i,j) )*( DP(ip1,j)+DP(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = V(I,j)*( COSIJ(I,J+1)+COSIJ(I,j) )*( DP(I,J+1)+DP(I,J) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO 100 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      DIV(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GETDIV ( U,V,DP,DIV,IM,JM )
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    U ....... ZONAL WIND                                    ****          
C ****    V ....... MERIDIONAL WIND                               ****          
C ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
      real      U(IM,JM)                                               
      real      V(IM,JM)                                               
      real     DP(IM,JM)                                               
      real    DIV(IM,JM)                                               
                                                                                
      real   P1X (IM,JM)                                               
      real   P1Y (IM,JM)                                               
      real   TMP1(IM,JM)                                               
      real   TMP2(IM,JM)                                               
      real  cosij(IM,JM)                                               
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      A    = 6.372e6
      pi   = 4.*atan(1.)
      dlon = 2*pi/ im
      dlat =   pi/(jm-1)
                                                                                
      C11 =  1.0 / (4.0*A*IM*(1.0-COS(0.5*dlat)))                           

      CX1 =  1.0 / (4.0*A*dlon)                                             
      CY1 =  1.0 / (4.0*A*dlat)                                             
                                                                                
      do j=2,jm-1
         phi = -pi/2.+(j-1)*dlat
      cosphi = cos(phi)
      do i=1,im
      cosij(i,j) = cosphi
      enddo
      enddo
      cosij(:,1)  = 0.0
      cosij(:,jm) = 0.0

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( U(ip1,j)+U(i,j) )*( DP(ip1,j)+DP(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = ( V(I,J+1)*COSIJ(I,J+1)+V(I,j)*COSIJ(I,j) )*( DP(I,J+1)+DP(I,J) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO 100 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      DIV(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GRADQ0(Q,DQDX,DQDY,IM,JM)
C *********************************************************
C ****                                                 ****
C ****  THIS PROGRAM CALCULATES THE HORIZONTAL         ****
C ****  GRADIENT OF THE INPUT FIELD Q                  ****
C ****                                                 ****
C ****  ARGUMENTS:                                     ****
C ****    Q ....... FIELD TO BE DIFFERENTIATED         ****
C ****    DQDX .... LONGITUDINAL Q-DERIVATIVE          ****
C ****    DQDY .... MERIDIONAL   Q-DERIVATIVE          ****
C ****    IM ...... NUMBER OF LONGITUDINAL POINTS      ****
C ****    JM ...... NUMBER OF LATITUDINAL  POINTS      ****
C ****                                                 ****
C *********************************************************

      use MAPL_ConstantsMod
      implicit none
      integer  im,jm

      real    Q(IM,JM)
      real DQDX(IM,JM)
      real DQDY(IM,JM)
      real  Q1X(IM,JM)
      real  Q2X(IM,JM)
      real  Q1Y(IM,JM)
      real  Q2Y(IM,JM)
      real acos(JM)
      real sinl(IM)
      real cosl(IM)

      real cx1,cx2,cy1,cy2,uc,vc,us,vs
      real dl,dp,a,pi,fjeq,phi
      integer i,j,m,ip1,ip2,jpole,msgn

C *********************************************************
C ****               INITIALIZATION                    ****
C *********************************************************

      a  = MAPL_RADIUS
      pi = 4.0*atan(1.0)
      dl = 2.0*pi/im
      dp = pi/(jm-1)

      CX1 = 2.0 / ( 3.0*A*DL)
      CX2 = 1.0 / (12.0*A*DL)
      CY1 = 2.0 / ( 3.0*A*DP)
      CY2 = 1.0 / (12.0*A*DP)

      Q1X(:,:) = 0.0
      Q2X(:,:) = 0.0
      Q1Y(:,:) = 0.0
      Q2Y(:,:) = 0.0

      fjeq = ( jm+1 )*0.5
      do j=2,jm-1
          phi = dp * (j-fjeq)
      acos(j) = 1.0/( cos(phi) )
      enddo
      do i=1,im/2
      cosl(i)      = -cos((i-1)*dl)
      cosl(i+im/2) = -cosl(i)
      sinl(i)      = -sin((i-1)*dl)
      sinl(i+im/2) = -sinl(i)
      enddo

C *********************************************************
C ****          CALCULATE AVERAGE QUANTITIES           ****
C *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      Q1X(i  ,j) = Q(ip1,j) + Q(i,j)
      Q2X(ip1,j) = Q(ip2,j) + Q(i,j)
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=1,jm-1
      do i=1,im
      Q1Y(i,j) = Q(i,j+1) + Q(i,j)
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      Q2Y(i,j) = Q(i,j+1) + Q(i,j-1)
      enddo
      enddo

      do i=1,im/2
      Q2Y(i, 1) = Q(i,2)
      Q2Y(i,jm) = Q(i,jm-1)
      enddo

      do i=1,im/2
      Q2Y(i     , 1) = Q(i+im/2,2)    + Q2Y(i,1)
      Q2Y(i+im/2, 1) = Q2Y(i,1)
      Q2Y(i     ,jm) = Q(i+im/2,jm-1) + Q2Y(i,jm)
      Q2Y(i+im/2,jm) = Q2Y(i,jm)
      enddo

C *********************************************************
C ****             CALCULATE Q-GRADIENTS               ****
C *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      DQDX(ip1,j) =  ACOS(j) * ( ( Q1X(ip1,j)-Q1X(i,j) )*CX1
     .                         - ( Q2X(ip2,j)-Q2X(i,j) )*CX2 )
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      DQDY(i,j) = ( Q1Y(i,j)  -Q1Y(i,j-1) )*CY1
     .          - ( Q2Y(i,j+1)-Q2Y(i,j-1) )*CY2
      enddo
      enddo

C *********************************************************
C ****         CALCULATE Q-GRADIENTS (POLES)           ****
C *********************************************************

      do i=1,im/2
      Q1Y(i,      2) = Q(i,      1) + Q(i+im/2,2)
      Q1Y(i+im/2, 2) = Q(i+im/2, 1) + Q(i,     2)
      Q2Y(i,      1) = Q(i,      1) + Q(i+im/2,3)
      Q2Y(i+im/2, 1) = Q(i+im/2, 1) + Q(i,     3)

      Q1Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-1)
      Q1Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-1)
      Q2Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-2)
      Q2Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-2)
      enddo

      do i=1,im
      DQDY(i,jm) = ( Q1Y(i,jm)-Q1Y(i,jm-1) )*CY1
     .           - ( Q2Y(i,jm)-Q2Y(i,jm-1) )*CY2

      DQDY(i, 1) = ( Q1Y(i,1)-Q1Y(i,2) )*CY1
     .           - ( Q2Y(i,2)-Q2Y(i,1) )*CY2
      enddo

C   APPLY BOUNDARY CONDITIONS AT THE POLES
C ==========================================

      DO 170 M=1,2
      MSGN  = (-1)**M
      JPOLE = 1 + (M-1)*(jm - 1)

      VC = 0.0
      VS = 0.0
      DO 180 I=1,IM
      VC = VC + DQDY(I,JPOLE)*COSL(I)
      VS = VS + DQDY(I,JPOLE)*SINL(I)
  180 CONTINUE
      VC = 2.0 * VC / IM
      VS = 2.0 * VS / IM

      UC = - MSGN*VS
      US =   MSGN*VC

      DO 190 I=1,IM
      DQDX(I,JPOLE) = US*SINL(I) + UC*COSL(I)
      DQDY(I,JPOLE) = VS*SINL(I) + VC*COSL(I)
  190 CONTINUE

  170 CONTINUE

      RETURN
      END
      SUBROUTINE GRADQ (Q,DQDX,DQDY,IM,JM)
C *********************************************************
C ****                                                 ****
C ****  THIS PROGRAM CALCULATES THE HORIZONTAL         ****
C ****  GRADIENT OF THE INPUT FIELD Q                  ****
C ****                                                 ****
C ****  ARGUMENTS:                                     ****
C ****    Q ....... FIELD TO BE DIFFERENTIATED         ****
C ****    DQDX .... LONGITUDINAL Q-DERIVATIVE          ****
C ****    DQDY .... MERIDIONAL   Q-DERIVATIVE          ****
C ****    IM ...... NUMBER OF LONGITUDINAL POINTS      ****
C ****    JM ...... NUMBER OF LATITUDINAL  POINTS      ****
C ****                                                 ****
C *********************************************************

      use MAPL_ConstantsMod
      implicit none
      integer  im,jm

      real    Q(IM,JM)
      real DQDX(IM,JM)
      real DQDY(IM,JM)
      real acos(JM)

      real cx1,cx2,cy1,cy2,uc,vc,us,vs
      real dl,dp,a,pi,fjeq,phi
      integer i,j,m,ip1,ip2,jpole,msgn

C *********************************************************
C ****               INITIALIZATION                    ****
C *********************************************************

      a  = MAPL_RADIUS
      pi = 4.0*atan(1.0)
      dl = 2.0*pi/im
      dp = pi/(jm-1)

      CX1 = 1.0 / (A*DL)
      CY1 = 1.0 / (A*DP)

      fjeq = ( jm+1 )*0.5
      do j=2,jm-1
          phi = dp * (j-fjeq)
      acos(j) = 1.0/( cos(phi) )
      enddo

C *********************************************************
C ****             CALCULATE Q-GRADIENTS               ****
C *********************************************************

      do j   = 2,jm-1
         i   = im
      do ip1 = 1,im
      DQDX(i,j) =  ACOS(j)*( Q(ip1,j)-Q(i,j) )*CX1
         i   = ip1
      enddo
      enddo

      do j=1,jm-1
      do i=1,im
      DQDY(i,j) = ( Q(i,j+1)-Q(i,j) )*CY1
      enddo
      enddo

      RETURN
      END
      SUBROUTINE LAPLACE (DIV,VELP,im,jnp)

      integer        IM,JNP
      real       DIV(IM,JNP)
      real      VELP(IM,JNP)

      real*8, allocatable :: VP(:,:)
      real*8, allocatable ::  w(:)
      real*8, allocatable ::  bdtf(:)
      real*8, allocatable ::  bdts(:)
      real*8, allocatable ::  bdps(:)
      real*8, allocatable ::  bdpf(:)
      real*8  ts,tf,ps,pf,elmbda,pertrb,pi

      imp = im+1
      iwk = 11*jnp+6*imp

      allocate ( vp(jnp,imp) )
      allocate (    w(iwk)   )
      allocate ( bdtf(imp)   )
      allocate ( bdts(imp)   )
      allocate ( bdps(jnp)   )
      allocate ( bdpf(jnp)   )
 
      vp(:,:)=0.0
         w(:)=0.0
      bdtf(:)=0.0
      bdts(:)=0.0
      bdps(:)=0.0
      bdpf(:)=0.0
 
c Transpose the input array
c -------------------------
      do j=1,jnp
      do i=1,im
      vp(j,i)   = div(i,j)
      enddo
      vp(j,imp) =  vp(j,1)
      enddo

C === SET THE INPUT VARIABLES
      RAD = 6371000.0
      PI  = 3.14159265358979D0
      INTL=0
      TS=0.0
      TF=PI
      M=JNP-1
      MBDCND=9
      PS=0.0
      PF=2*PI
      N=IM
      NBDCND=0
      ELMBDA=0
      PERTRB=0
      IDIMF=M+1
 
      CALL PWSSSP (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,BDPS,
     *             BDPF,ELMBDA,VP,IDIMF,PERTRB,IERROR,W)

      if( ierror.ne.0 ) then
          print *, 'PWSSSP IERROR = ',ierror
          stop
      endif
 
c Scale by earth radius
c ---------------------
      do j=1,jnp
      do i=1,im
      VELP(I,J) = VP(J,I) * RAD * RAD
      enddo
      enddo
 
c Remove global mean
c ------------------
      CALL ZEROG (VELP,IM,JNP)
 
      deallocate (   vp )
      deallocate (    w )
      deallocate ( bdtf )
      deallocate ( bdts )
      deallocate ( bdps )
      deallocate ( bdpf )
      RETURN
      END

      SUBROUTINE ZEROG (VEL,IM,JNP)
      integer    IM,JNP
      real   VEL(IM,JNP)

      pi  = 4.0*atan(1.0)
      dl  = 2*pi/im
      dp  = pi/(jnp-1)
      cap = 1-cos(0.5*dp)

c Ensure unique pole values
c -------------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + vel(i,1)
      sum2 = sum2 + vel(i,jnp)
      enddo
      sum1 = sum1/im
      sum2 = sum2/im
      do i=1,im
      vel(i,1)   = sum1
      vel(i,jnp) = sum2
      enddo

c Compute global average
c ----------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + cap*vel(i,1)
      sum2 = sum2 + cap
      enddo

      do j=2,jnp-1
      cosj = cos( -pi/2 + (j-1)*dp )
      do i=1,im
      sum1 = sum1 + cosj*dp*vel(i,j)
      sum2 = sum2 + cosj*dp
      enddo
      enddo

      do i=1,im
      sum1 = sum1 + cap*vel(i,jnp)
      sum2 = sum2 + cap
      enddo

      qave = sum1/sum2

      do j=1,jnp
      do i=1,im
      vel(i,j) = vel(i,j)-qave
      enddo
      enddo

c     print *, 'Remove Global Average: ', qave

      RETURN
      END

      SUBROUTINE VORDIV ( UZ,VZ,VOR,DIV,IMAX,JMAX )                              
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES VORTICITY AND DIVERGENCE          ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    UZ ...... ZONAL WIND                                    ****          
C ****    VZ ...... MERIDIONAL WIND                               ****          
C ****    IMAX .... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JMAX .... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    VOR  (IMAX,JMAX) .... VORTICITY                         ****          
C ****    DIV  (IMAX,JMAX) .... DIVERGENCE                        ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
      real     UZ(IMAX,JMAX)                                               
      real     VZ(IMAX,JMAX)                                               
      real    DIV(IMAX,JMAX)                                               
      real    VOR(IMAX,JMAX)                                               
                                                                                
      real   P1X (IMAX,JMAX)                                               
      real   P2X (IMAX,JMAX)                                               
      real   P1Y (IMAX,JMAX)                                               
      real   P2Y (IMAX,JMAX)                                               
      real   TMP1(IMAX,JMAX)                                               
      real   TMP2(IMAX,JMAX)                                               
      real  cosij(IMAX,JMAX)                                               
                                                                                
      PARAMETER ( ZERO    =  0.0 )                                              
      PARAMETER ( ONE     =  1.0 )                                              
      PARAMETER ( TWO     =  2.0 )                                              
      PARAMETER ( THREE   =  3.0 )                                              
      PARAMETER ( FOUR    =  4.0 )                                              
      PARAMETER ( TWELVE  = 12.0 )                                              
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      im      = imax
      imm1    = im-1

      IMR     = IMAX
      JMR     = JMAX-1
      IMJMR   = IMR*(JMR+1)
      IMJMR   = IMR*(JMR+1)
      IMJMM1R = IMR* JMR
      IMJMM2R = IMR*(JMR-1)

      A  = 6.372e6
      pi = 4.*atan(1.)
      dl = 2*pi/imr
      dp =   pi/jmr
                                                                                
      C11 =  FOUR / (THREE *A*IM*(ONE-COS(DP))    )                           
      C12 =  ONE  / (THREE *A*IM*(ONE-COS(TWO*DP)))                           
      CX1 =  TWO  / (THREE *A*DL)                                             
      CX2 =  ONE  / (TWELVE*A*DL)                                             
      CY1 =  TWO  / (THREE *A*DP)                                             
      CY2 =  ONE  / (TWELVE*A*DP)                                             
                                                                                
      do j=1,jmax
      phi = -pi/2.+(j-1)*dp
      cosphi = cos(phi)
      do i=1,imax
      cosij(i,j) = cosphi
      enddo
      enddo

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO   j=1,jmax                                                           
      DO   I=1,imax                                                           
      P1X (I,j) = ZERO                                                          
      P2X (I,j) = ZERO                                                          
      P1Y (I,j) = ZERO                                                          
      P2Y (I,j) = ZERO                                                          
      TMP1(I,j) = UZ(I,j)                                             
      TMP2(I,j) = VZ(I,j)*COSIJ(I,j)                                  
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      P1X(I  ,j) = TMP1(I+1,j) + TMP1(I,j)                                     
      P2X(I+1,j) = TMP1(I+2,j) + TMP1(I,j)                                     
      ENDDO                                                                     
      ENDDO                                                                     

      DO   J=2,JMR                                                             
      P1X(IM,J) = TMP1(1,J) + TMP1(IM  ,J)                                     
      P2X(IM,J) = TMP1(1,J) + TMP1(IMM1,J)                                     
      P2X( 1,J) = TMP1(2,J) + TMP1(IM  ,J)                                     
      ENDDO                                                                     
                                                                                
      DO   j=1,jmax-1                                                           
      DO   I=1,imax                                                           
      P1Y(I, j) = TMP2(I,J+1) + TMP2(I,j)                                        
      ENDDO                                                                     
      ENDDO                                                                     
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      P2Y(I, j) = TMP2(I,j+1) + TMP2(I,j-1)                                        
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
      DO   I=1,IMR                                                             
      P2Y(I,  1) = ZERO                                                        
      P2Y(I,jmax) = ZERO                                                        
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      TMP1(I+1,j) = ( P1X(I+1,j)-P1X(I,j) )*CX1                               
     .            - ( P2X(I+2,j)-P2X(I,j) )*CX2                               
      ENDDO                                                                     
      ENDDO                                                                     
      DO    J=2,JMR                                                             
      TMP1( 1,J) = ( P1X( 1,J)-P1X(IM  ,J) )*CX1                              
     .           - ( P2X( 2,J)-P2X(IM  ,J) )*CX2                              
      TMP1(IM,J) = ( P1X(IM,J)-P1X(IMM1,J) )*CX1                              
     .           - ( P2X( 1,J)-P2X(IMM1,J) )*CX2                              
      ENDDO                                                                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
     .          - ( P2Y(I,j+1)-P2Y(I,j-1) )*CY2                                  
      DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO 100 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jmax-1)                                                
      JPH    = 1 + (M-1)*(jmax-2)                                               
      JSTAR1 = 2 + (M-1)*(jmax-3)                                               
      JSTAR2 = 3 + (M-1)*(jmax-5)
                                                                                
      SUM11  = ZERO                                                             
      SUM12  = ZERO                                                             
      DO    I=1,IMR                                                            
      SUM11  = SUM11 + P1Y(I,JPH   )                                           
      SUM12  = SUM12 + P2Y(I,JSTAR1)                                           
      ENDDO                                                                     
                                                                                
      DO  I=1,IMR                                                            
      DIV(I,JPOLE) = - MSGN(M) * ( C11*SUM11 - C12*SUM12 )                    
      ENDDO                                                                     
                                                                                
  100 CONTINUE                                                                  
                                                                                
C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
      DO   j=1,jmax                                                           
      DO   I=1,imax                                                           
      P1X (I,j) = ZERO                                                          
      P2X (I,j) = ZERO                                                          
      P1Y (I,j) = ZERO                                                          
      P2Y (I,j) = ZERO                                                          
      TMP1(I,j) = VZ(I,j)                                             
      TMP2(I,j) = UZ(I,j)*COSIJ(I,j)                                  
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      P1X(I  ,j) = TMP1(I+1,j) + TMP1(I,j)                                     
      P2X(I+1,j) = TMP1(I+2,j) + TMP1(I,j)                                     
      ENDDO                                                                     
      ENDDO                                                                     

      DO   J=2,JMR                                                             
      P1X(IM,J) = TMP1(1,J) + TMP1(IM  ,J)                                     
      P2X(IM,J) = TMP1(1,J) + TMP1(IMM1,J)                                     
      P2X( 1,J) = TMP1(2,J) + TMP1(IM  ,J)                                     
      ENDDO                                                                     
                                                                                
      DO   j=1,jmax-1                                                           
      DO   I=1,imax                                                           
      P1Y(I, j) = TMP2(I,J+1) + TMP2(I,j)                                        
      ENDDO                                                                     
      ENDDO                                                                     
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      P2Y(I, j) = TMP2(I,j+1) + TMP2(I,j-1)                                        
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
      DO   I=1,IMR                                                             
      P2Y(I,  1) = ZERO                                                        
      P2Y(I,jmax) = ZERO                                                        
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL VORTICITY           ****                     
C *********************************************************                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      TMP1(I+1,j) = ( P1X(I+1,j)-P1X(I,j) )*CX1                               
     .            - ( P2X(I+2,j)-P2X(I,j) )*CX2                               
      ENDDO                                                                     
      ENDDO                                                                     
      DO    J=2,JMR                                                             
      TMP1( 1,J) = ( P1X( 1,J)-P1X(IM  ,J) )*CX1                              
     .           - ( P2X( 2,J)-P2X(IM  ,J) )*CX2                              
      TMP1(IM,J) = ( P1X(IM,J)-P1X(IMM1,J) )*CX1                              
     .           - ( P2X( 1,J)-P2X(IMM1,J) )*CX2                              
      ENDDO                                                                     
                                                                                
      DO   j=2,jmax-1                                                           
      DO   I=1,imax                                                           
      TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
     .          - ( P2Y(I,j+1)-P2Y(I,j-1) )*CY2                                  
      VOR (I,j) = ( TMP1(I,j)-TMP2(I,j) )/(cosij(i,j))                        
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL VORTICITY  AT POLES    ****                     
C *********************************************************                     
                                                                                
      DO 200 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jmax-1)                                                
      JPH    = 1 + (M-1)*(jmax-2)                                               
      JSTAR1 = 2 + (M-1)*(jmax-3)                                               
      JSTAR2 = 3 + (M-1)*(jmax-5)
                                                                                
      SUM11  = ZERO                                                             
      SUM12  = ZERO                                                             
      DO    I=1,IMR                                                            
      SUM11  = SUM11 + P1Y(I,JPH   )                                           
      SUM12  = SUM12 + P2Y(I,JSTAR1)                                           
      ENDDO                                                                     
                                                                                
      DO  I=1,IMR                                                            
      VOR(I,JPOLE) =   MSGN(M) * ( C11*SUM11 - C12*SUM12 )                    
      ENDDO                                                                     
                                                                                
  200 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       

      subroutine sigtopl ( qprs,q,logpl,logps,logpt,logp,im,jm,lm,undef )
C                                               
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
C    
      implicit none
      integer  i,j,l,im,jm,lm

      real*8  qprs(im,jm)
      real*8  q   (im,jm,lm)
      real*8 logpl(im,jm,lm)
      real*8 logps(im,jm)
      real*8 logpt(im,jm)

      real*8  p,undef
      real*8  logp,logpmin,logpmax,temp
     
c Initialize to UNDEFINED
c -----------------------
      do i=1,im*jm
      qprs(i,1) = undef
      enddo

c Interpolate to Pressure Between Model Levels
c --------------------------------------------
      do L=1,lm-1
      if( all( logpl(:,:,L  )>logp ) ) exit
      if( all( logpl(:,:,L+1)<logp ) ) cycle

         do j=1,jm
         do i=1,im
         if( logp.le.logpl(i,j,L+1) .and. logp.ge.logpl(i,j,L) ) then
         temp      = ( logpl(i,j,L)-logp ) / ( logpl(i,j,L)-logpl(i,j,L+1) )

                 if( q(i,j,L)  .ne.undef  .and. 
     .               q(i,j,L+1).ne.undef ) then
         qprs(i,j) = q(i,j,L+1)*temp + q(i,j,L)*(1.-temp)
            else if( q(i,j,L+1).ne.undef  .and. temp.ge.0.5 ) then
         qprs(i,j) = q(i,j,L+1)
            else if( q(i,j,L)  .ne.undef  .and. temp.le.0.5 ) then
         qprs(i,j) = q(i,j,L)
                 endif

         endif
         enddo
         enddo

      enddo

      do j=1,jm
      do i=1,im
c Extrapolate to Pressure between Ptop and First Model Level
c ----------------------------------------------------------
      if( logp.le.logpl(i,j,1) .and. logp.ge.logpt(i,j) ) then
          qprs(i,j) = q(i,j,1)
      endif

! Extend Lowest Level Value to the Surface
! ----------------------------------------
      if( logp.le.logps(i,j) .and. logp.gt.logpl(i,j,lm ) ) then
          qprs(i,j) = q(i,j,lm)
      endif

      enddo
      enddo

      return
      end subroutine sigtopl
