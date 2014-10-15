    use m_dyn
    use m_ioutil, only : luavail
    implicit none 
    integer nymd, nhms, lu, n, freq, vectype, prec, ier, nstep
    integer iarg, argc, ndim2, ndim3, iargc
    character(len=255) :: dynfile1, dynfile2
    type(dyn_vect) w_1
    type(dyn_vect) w_2
    real :: total, dotp(5), ddot_
    prec = 0
    vectype = 5
    n = 1

    argc = iargc()
    if ( argc < 1 ) then
       print *, "Purpose: Calculate dot product between two dyn vectors"
       print *, "         Needless to say user should know what he/she "
       print *, "         is calculation or else this is meaningless."
       print *, "   "
       print *, "Usage: dyndot.x dynfile1 dynfile2 "
       print *, "   "
       print *, "   "
       print *, "Output written to file dyndot.txt"
       stop
    end if
                                                                                                                                    
    iarg = 1
    call GetArg ( iarg, dynfile1 )
    print * , "Dyn Vector 1: ", trim(dynfile1)

    iarg = iarg + 1
    call GetArg ( iarg, dynfile2 )
    print * , "Dyn Vector 2: ", trim(dynfile2)

    call dyn_get ( trim(dynfile1), nymd, nhms, w_1, ier, timidx=n, &
                               freq=freq, nstep=nstep, vectype=vectype )
    call dyn_get ( trim(dynfile2), nymd, nhms, w_2, ier, timidx=n, &
                               freq=freq, nstep=nstep, vectype=vectype )

    ndim2 = w_1%grid%im * w_1%grid%jm
    ndim3 = ndim2       * w_1%grid%km

    dotp(1) = ddot_(ndim3,w_1%u ,1,w_2%u ,1) 
    dotp(2) = ddot_(ndim3,w_1%v ,1,w_2%v ,1) 
    dotp(3) = ddot_(ndim3,w_1%pt,1,w_2%pt,1) 
    dotp(4) = ddot_(ndim3,w_1%delp,1,w_2%delp,1) 
    dotp(5) = ddot_(ndim3,w_1%q ,1,w_2%q ,1)  ! only 1st entry in q for now
    total   = sum(dotp)

    lu= luavail()
    open(lu, file='dyndot.txt', form='formatted')
    write (lu, '(1p,6e13.6)') total, dotp(1:5)
    close(lu)

    end


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


