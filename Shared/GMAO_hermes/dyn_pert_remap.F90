   program dyn_pert_remap
!  must run as mpirun -np 1

   use m_dyn, only: dyn_vect
   use m_dyn, only: dyn_init
   use m_dyn, only: dyn_get
   use m_dyn, only: dyn_put
   use m_dyn, only: dyn_stat

   use m_maph_pert, only: h_map_pert 

   use m_die, only: die

   implicit none

   character(len=*), parameter :: myname = 'dyn_pert_remap'

   type(dyn_vect) :: idyn
   type(dyn_vect) :: odyn
   type(dyn_vect) :: xdyn

   character(len=255) :: ifile,ofile

   character(len=255)  argv
   character(len=3) opt
   integer iargc
   integer iarg,argc,nymd,nhms,freq
   integer im,jm
   integer ierr
   integer idim,odim
   real dotp0(1),dotp1(1),dotp2(1) !,ddot_
 
   argc = iargc()
   if ( argc < 1 ) then
        print *
        print *, "Purpose: interpolate perturbation vector."
        print *, "         Ouput file is dyn-like file with: "
        print *
        print *, "Usage: dyn_pert_remap.x im jm opt ifile ofile "
        print *
        print *, "  im    - number of longitude grid points for output fields "
        print *, "  jm    - number of latitude  grid points for output fields "
        print *, "  opt   - ppm, tlm or adm (last 2 are bi-linear interps)"
        print *, "  ifile - input filename "
        print *, "  ofile - output filename"
        print *
        stop
   end if
   
   iarg = 1
   call GetArg ( iarg, argv )
   read(argv,*) im
   iarg = iarg + 1
   call GetArg ( iarg, argv )
   read(argv,*) jm
   iarg = iarg + 1
   call GetArg ( iarg, opt )
   iarg = iarg + 1
   call GetArg ( iarg, ifile )
   iarg = iarg + 1
   call GetArg ( iarg, ofile )
      
!  read input dyn vector
!  ---------------------
   call dyn_get ( trim(ifile), nymd, nhms, idyn, ierr, timidx=1, freq=freq, vectype=5 )
   print* , "Read input file: ", trim(ifile)
   print *, 'Horizontal resolution of  input: ', idyn%grid%im, ' x ', idyn%grid%jm

!  allocate dyn for ouput dyn-vector
!  ---------------------------------
   if (im==idyn%grid%im .or. jm==idyn%grid%jm ) then
      print *, 'No interpolation needed ...'
      call dyn_init ( idyn, odyn, ierr, copy=.true., vectype=5 )
   else ! same resolution ... just copy field
      call dyn_init ( im, jm, idyn%grid%km, idyn%grid%lm, odyn, ierr, &
                      idyn%grid%ptop, idyn%grid%ks, idyn%grid%ak, idyn%grid%bk, vectype=5 )
        if(ierr/=0)then
          print *, 'trouble in init ', ierr
          call exit(99)
        endif

      if ( opt == 'ppm' ) then
         print *, 'Using area-preserving interpolation'
         call h_map_pert ( idyn, odyn, ierr )  ! SJ interpolation
      endif
      if ( opt == 'tlm' ) then
         print *, 'Using bi-linear interpolation'
         call h_map_pert ( idyn, odyn, 'tlm', ierr )  ! bi-linear interpolation
      endif
      if ( opt == 'adm' ) then
         print *, 'Using bi-linear interpolation'
         call h_map_pert ( idyn, odyn, 'adm', ierr )  ! bi-linear interpolation
      endif
   endif
   if ( opt == 'dot' ) then
        call dyn_init ( idyn%grid%im, idyn%grid%jm, idyn%grid%km, idyn%grid%lm, xdyn, ierr, &
                        idyn%grid%ptop, idyn%grid%ks, idyn%grid%ak, idyn%grid%bk, vectype=5 )
        if(ierr/=0)then
          print *, 'trouble in init ', ierr
          call exit(99)
        endif
        idim = idyn%grid%im * idyn%grid%jm * idyn%grid%km 
        odim = odyn%grid%im * odyn%grid%jm * odyn%grid%km 

        dotp0(1) = ddot_(idim,idyn%u,1,idyn%u,1) 
        call h_map_pert ( idyn, odyn, 'tlm', ierr )  !        bi-linear interpolation
        dotp1(1) = ddot_(odim,odyn%u,1,odyn%u,1) 

        call h_map_pert ( odyn, xdyn, 'adm', ierr )  ! adm of bi-linear interpolation
        dotp2(1) = ddot_(idim,xdyn%u,1,xdyn%u,1) 
        print *, 'Forward  dot: ', dotp1(1)
        print *, 'Backward dot: ', dotp2(1)
        print *, 'Rel error : ', abs(dotp1(1)-dotp2(1))/dotp0(1)
   endif

! write out
!  print* , "Writing output file: ", trim(ofile)
!  print *, 'Horizontal resolution of output: ', im, ' x ', jm
   if ( opt == 'dot' ) then
       print *, 'sum(ps) = ',sum(idyn%ps), sum(odyn%ps)
       print *, 'sum(pt) = ',sum(idyn%pt), sum(odyn%pt)
       call dyn_put ( trim(ofile), nymd, nhms, 0, xdyn, ierr, new=.true., freq=freq, vectype=5 )
   else
       call dyn_put ( trim(ofile), nymd, nhms, 0, odyn, ierr, new=.true., freq=freq, vectype=5 )
   endif


 contains

! For some reason, this BLAS function cannot be found by the
! intel compiler on some Linux platforms. (Arlindo)
!

      real FUNCTION DDOT_(N,DX,INCX,DY,INCY)
!     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      real DX(*),DY(*)
!     ..
!
!  Purpose
!  =======
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(!)
!
!
!     .. Local Scalars ..
      real*8 DTEMP
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      DDOT_ = 0.0d0
      DTEMP = 0.0d0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DTEMP + DX(IX)*DY(IY)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      DDOT_ = DTEMP
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF (N.LT.5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) + &
                  DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50 CONTINUE
   60 DDOT_ = DTEMP
      RETURN
      END function ddot_

   end program dyn_pert_remap
