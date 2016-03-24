    program dyndot 
    use m_dyn
    use m_ioutil, only : luavail
    use m_die, only: die
    implicit none 
    character(len=*), parameter :: myname='dyndot'
    integer,parameter :: dyntype=5
    integer,parameter :: nfiles=2
    integer nymd, nhms, lu, n, freq, vectype, prec, ier, nstep
    integer i, nf, iarg, argc, ndim2, ndim3, intarg, iargc
    character(len=255) :: dynfile(nfiles)
    character(len=255) argv
    type(dyn_vect) w_1
    type(dyn_vect) w_2
    logical use_ps
    real :: total, dotp(5)
    integer im,jm,km,lm,k,rc
    integer im1,jm1,km1,lm1
    integer im2,jm2,km2,lm2
    logical adm(nfiles)
    logical pncf

    prec = 0
    vectype = 5
    use_ps=.false. ! default is to use delp
    adm=.false.
    pncf=.false.

    argc = iargc()
    if ( argc < 1 ) call usage_()
    nf=0
    do i = 1, 32767
       iarg = iarg + 1
       if ( iarg .gt. argc ) exit
       call GetArg ( iarg, argv )
       select case (argv)
          case ("-use_ps")
             use_ps = .true.
          case ("-adm1")
             adm(1) = .true.
          case ("-adm2")
             adm(2) = .true.
          case ("-pncf")
             pncf = .true.
!         case ("-adm")
!            if ( iarg+1 .gt. argc ) call usage_()
!            iarg = iarg + 1
!            call GetArg ( iarg, argv )
!            read(argv,*) intarg
!            if(intarg>0) adm(1)=.true.
!            iarg = iarg + 1
!            call GetArg ( iarg, argv )
!            read(argv,*) intarg
!            if(intarg>0) adm(2)=.true.
          case default
             nf = nf + 1
             if ( nf .gt. nfiles ) call die(myname,'too many eta files')
             dynfile(nf) = trim(argv)
       end select
    enddo
                                                                                                                                    
    do nf=1,nfiles
       write(6,'(a,i3,2a)') "Dyn Vector ", nf , ": ", trim(dynfile(nf))
    enddo

    call dyn_getdim ( trim(dynfile(1)), im1, jm1, km1, lm1, rc )
    call dyn_getdim ( trim(dynfile(2)), im2, jm2, km2, lm2, rc )
    if(km1/=km2) then ! ignore diff in lm for now
       print *, trim(myname), ': km/lm file 1 = ', km1,lm1
       print *, trim(myname), ': km/lm file 2 = ', km2,lm2
       call die(myname,'inconsistent km/lm')
    endif
    im=min(im1,im2)
    jm=min(jm1,jm2)
    km=min(km1,km2)
    lm=min(lm1,lm2)
    ndim2 = im*jm

!   check dimensions and take proper action
    n=1
    if ( im1==im2 .and. jm1==jm2 ) then
       call dyn_get ( trim(dynfile(1)), nymd, nhms, w_1, ier, timidx=n, &
                                      freq=freq, nstep=nstep, vectype=vectype, pncf=pncf )
       call dyn_get ( trim(dynfile(2)), nymd, nhms, w_2, ier, timidx=n, &
                                      freq=freq, nstep=nstep, vectype=vectype, pncf=pncf )
    else

       if (im1<im2 .and. jm1<jm2 ) then

          call dyn_get ( trim(dynfile(1)), nymd, nhms, w_1, ier, timidx=n, &
                                         freq=freq, nstep=nstep, vectype=vectype, pncf=pncf )


!         Initialize dimension of output (interpolated) vector
!         ----------------------------------------------------
          call dyn_init ( im, jm, km, lm, w_2, rc, & 
                          w_1%grid%ptop, w_1%grid%ks, w_1%grid%ak, w_1%grid%bk, vectype=dyntype )
            if ( rc/=0 ) then
                 call die (myname, ': Error initializing dyn vector(w_2)')
            endif

          call getvector_ ( dynfile(2), w_2, adm(1) )

!         Recalculate ps for consistency w/ delp
!         --------------------------------------
          w_2%ps = 0.0 ! perturbations
          do k=1,km
             w_2%ps = w_2%ps + w_2%delp(:,:,k)
          enddo

       else

          call dyn_get ( trim(dynfile(2)), nymd, nhms, w_2, ier, timidx=n, &
                                         freq=freq, nstep=nstep, vectype=vectype, pncf=pncf )


!         Initialize dimension of output (interpolated) vector
!         ----------------------------------------------------
          call dyn_init ( im, jm, km, lm, w_1, rc, & 
                          w_2%grid%ptop, w_2%grid%ks, w_2%grid%ak, w_2%grid%bk, vectype=dyntype )
            if ( rc/=0 ) then
                 call die (myname, ': Error initializing dyn vector(w_1)')
            endif

          call getvector_ ( dynfile(1), w_1, adm(2) )

       endif

    endif
    ndim3 = ndim2 * km
    call dotall_ (w_1,w_2) 

    contains

    subroutine getvector_ (fname,x,adm)
    use m_maph_pert, only: h_map_pert
    implicit none
    character(len=*) fname
    type(dyn_vect) x
    type(dyn_vect) www
    logical adm

          call dyn_get ( trim(fname), nymd, nhms, www, rc, timidx=n, &
                         freq=freq, vectype=dyntype, pncf=pncf )


!         Interpolate to required resolution
!         ----------------------------------
          call h_map_pert ( www, x, rc, adm=adm )
               if ( rc/=0 ) then
                    call dyn_clean ( x )
                    call dyn_clean ( www )
                    print *, 'h_map error code = ', rc
                    call die(myname,' failed in h_map')
               else
                    print*, myname, ': interpolated perturbation: ', trim(fname) 
                    print*, myname, ': from ', www%grid%im, 'x', www%grid%jm, ' to ', x%grid%im, 'x', x%grid%jm
                    call dyn_clean ( www )
               endif

    end subroutine getvector_

    subroutine dotall_ (x,y)
    type(dyn_vect) x,y
    real ddot_

    dotp(1) = ddot_(ndim3,x%u ,1,y%u ,1)
    dotp(2) = ddot_(ndim3,x%v ,1,y%v ,1)
    dotp(3) = ddot_(ndim3,x%pt,1,y%pt,1)
    if ( use_ps ) then
       dotp(4) = ddot_(ndim2,x%ps  ,1,y%ps  ,1)
    else
       dotp(4) = ddot_(ndim3,x%delp,1,y%delp,1)
    endif
    dotp(5) = ddot_(ndim3,x%q(:,:,:,1) ,1,y%q(:,:,:,1) ,1)  ! only 1st entry in q for now
    total   = sum(dotp)

    lu= luavail()
    open(lu, file='dyndot.txt', form='formatted')
    write (lu, '(1p,6e16.6)') total, dotp(1:5)
    close(lu)
 
    end subroutine dotall_

    subroutine usage_
       print *, "Purpose: Calculate dot product between two dyn vectors"
       print *, "         Needless to say user should know what he/she "
       print *, "         is calculation or else this is meaningless."
       print *, "   "
       print *, "Usage: dyndot.x [options] dynfile1 dynfile2 "
       print *, "   "
       print *, " Optional arguments:  "
       print *, "   "
       print *, " -use_ps    Specify to use PS instead of DELP in dot product "
       print *, " -adm1      Specify if 1st file is a sensitivity vector"
       print *, " -adm2      Specify if 2nd file is a sensitivity vector"
       print *, "   "
       print *, "Output written to file dyndot.txt"
       stop
    end subroutine usage_

    end program dyndot

!
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
      END


