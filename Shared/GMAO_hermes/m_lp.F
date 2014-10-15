!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_lp --- Implements Large-Pond Surface Layer Parameterizations
! 
! !INTERFACE:
!

      MODULE  m_lp

! !USES:

!      Implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
      PRIVATE
      PUBLIC   LP_Init         ! Initialize this package
      PUBLIC   LP_MoveZ        ! Convert (u,dt,dq) from z1 to z2

!
! !DESCRIPTION: This module implements the surface layer formulation
!               using Large and Pond parementerizations. This code
!  has been adapted from da Silva et al (1994) UWM/COADS system,
!  with modifications to ensure compatibility with FVGCM.
!
! !REVISION HISTORY: 
!
!  12Nov1999  da Silva  Initial code reusing UWM/COADS Large-pond.f.
!  16Nov2000  Dee       Replaced STOP by nonzero return code in LP10
!
!EOP
!-------------------------------------------------------------------------

!      Internal data structure from include file lp.h
!      ----------------------------------------------
       integer, parameter::  nzdl = 50, zdla = -5.0, zdlb = 0.0 

       real psim(nzdl), bbm(nzdl), ccm(nzdl), ddm(nzdl)
       real psit(nzdl), bbt(nzdl), cct(nzdl), ddt(nzdl)
       real zfit(nzdl)


      CONTAINS

      subroutine LP_Init()

!      include 'lp.h'

      deltaz = ( zdlb - zdla )/float(nzdl - 1)

!     Calculate psi's
!     ---------------
      do i = 1, nzdl
         zfit(i) = float(i - 1) * deltaz + zdla
         zfit(i) = zfit(i)
         psim(i) = EXPSIM ( zfit(i) )
         psit(i) = EXPSIT ( zfit(i) )
      end do

!     Calculate spline fit
!     --------------------
      call SPLINE ( bbm, ccm, ddm, nzdl, zfit, psim )
      call SPLINE ( bbt, cct, ddt, nzdl, zfit, psit )


      return

      end subroutine LP_Init

!.....................................................................



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  LP_MoveZ --- Moves (u,dt,dq) from z1 to z2
! 
! !INTERFACE:
!
      subroutine LP_MoveZ ( z1, u1, t1, dq1, dt1, ts,
     &                      z2, u2, conf, 
     &                      dq2, dt2, CD10, CT10, CE10, z10dL )  ! optional


! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS: 
!
                                           ! Original quantities at z1:
      real, intent(in)            :: z1    !   original height above sfc [m] 
      real, intent(in)            :: u1    !   wind SPEED (m/s) at z1 
      real, intent(in)            :: t1    !   temperature [K]  at z1
      real, intent(in)            :: dt1   !   delta pot temperature [K]:
!                                                dt1 = sst - theta(z1)
      real, intent(in)            :: dq1   !   delta spec. humidity [g/kg] 
!                                                dq1 = 0.98*q_s(SST) - q(z1)


                                           ! NOTE: For consistency, use
                                           !        theta = T + 0.01 * Z


                                           ! Surface quantities:
      real, intent(in)            :: ts    !   sea surface temperature

      real, intent(in)            :: z2    ! Desired new height above sfc [m] 

! !OUTPUT PARAMETERS: 
!
                                           ! Surface layer quantities:
      real, intent(out)           :: u2    !   wind SPEED at zlev
      real, intent(out)           :: conf  ! Confidence 
      real, intent(out), OPTIONAL :: dt2   !   delta pot temp [K] at z2
      real, intent(out), OPTIONAL :: dq2   !   delta spec. hum. [g/kg] at z2
      real, intent(out), OPTIONAL :: CD10  !   drag coeff at 10m
      real, intent(out), OPTIONAL :: CT10  !   Stanton number
      real, intent(out), OPTIONAL :: CE10  !   Dalton number
      real, intent(out), OPTIONAL :: Z10DL !   10/L, where L is Monin-Obukhov
                                           !    length
      

! !DESCRIPTION: This routine converts wind speed, temperature and
!               and specific humidty from z1 to z2 using similarity
!  theory.
!
!
!  \bv
!
!     _________________________________________ p(K-1)
!     
!     
!     
!     
!                   u1, t1, q1
!     ................... * ................... p1 = ( p(K)+p(K-1) ) / 2
!     
!     
!     
!                   u2, t2, q2
!     ................... * ................... z = zlev
!     
!     _________________________________________ p(K) = p_s
!   
!  
!  \ev         
!
! !REVISION HISTORY: 
!
!  17nov1999  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

      real    CD, CT, CE, z1dl, z2dl, u10, ta10, dt10, dq10
      real    zlnz1, zlnz2 
      integer rc 


!     Consistency check
!     -----------------
      if ( z1 .le. 0 .or. z2 .le. 0 ) then
           conf = 0.0
           return
      end if

!     Convert from z1 to standard 10m
!     -------------------------------
      call LPZ ( CD, CT, CE, z1dl, u10, ta10, dt10, dq10,
     .           z1, u1, t1, dt1, dq1, rc )

!     Did It converge?
!     ----------------
      if ( rc .ne. 0 ) then
         conf = 0.0
         return
      else
         conf = 1.0
      end if


!     Conversion from z1 to z2
!     ------------------------
      if ( z2 .eq. 10.0 ) then        ! already there at 10m

           u2 = u10
           if ( present(dt2) ) dt2 = dt10
           if ( present(dq2) ) dq2 = dq10

      else

         z2dl  = (z2/z1) * z1dl
         zlnz1 = log( z1 / 10. )
         zlnz2 = log( z2 / 10. )

         u2  = u1 * RM ( CD, z2dl, z2, zlnz2 )
     .            / RM ( CD, z1dl, z1, zlnz1 ) 

         if ( present ( dt2 ) ) then 
             dt2 = dt1 * RT ( CD, CT, z2dl, z2, zlnz2 )
     .                 / RT ( CD, CT, z1dl, z1, zlnz1 ) 
         end if

         if ( present ( dq2 ) ) then
             dq2 = dq1 * RE ( CD, CE, z2dl, z2, zlnz2 )
     .                 / RE ( CD, CE, z1dl, z1, zlnz1 )
         end if


      endif

!     Optional diagnostics
!     --------------------
      if ( present(cd10)  ) CD10  = CD
      if ( present(ce10)  ) CE10  = CE
      if ( present(ct10)  ) CT10  = CT
      if ( present(z10dl) ) Z10DL = ( 10.0 / z1 ) * z1dl

!     All done
!     --------
      rc = 0
      return

      end subroutine LP_MoveZ 

!.....................................................................

      subroutine LPZ ( CD, CT, CE, zdl, w10, ta10, dth10, dq10,
     .                 z, w, ta, dth, dq, rc )

!     This routine calculates the corrected values for
!     CD, CT, CE, w, ta, dth, and dq from height z to 10m.
!     The routine assumes that dth is theta(sea) - theta(air).
!     Upon input, however, dth may be t(sea) - t(air) if one
!     does not need the accuracy that the theta values provide.
!     Units for input/output: w in m/s, ta in K, dth in K, dq
!     in g/kg. Upon output zdl is actually 10/L.
!
!     Notice that CD = CD(z=10), CT = CT(z=10), etc...
!

      integer rc

!     No. of iteration and tolerance
!     ------------------------------
      parameter ( itmax = 100, eps = 1. E -4, gamma = 0.01 )

      common / lpneut / CDN, fofzdl

      real zlnz

      rc = 0
      zlnz = log( z / 10. )
      m = 0

!     First guess
!     -----------
      w10 = w
      ta10 = ta
      dth10 = dth
      dq10 = dq

      Ts = dth + ta + gamma * z
      Tsg10 = Ts - gamma * 10.

!     -------------------------------------
!     NOTE: zdl below actually means 10 / L
!     -------------------------------------

!     Trivial case: z = 10. No height adjustment
!     ------------------------------------------
      if ( z .eq. 10. ) then
         call LP10 ( CD, CT, CE, zdl, w, ta, dth, dq, rc )
         return
      end if

!     Otherwise, iterations are needed
!     Find first guess zdl
!     --------------------------------
      call LP10 ( CD, CT, CE, ozdl, w10, ta10, dth10, dq10, rc )
      if ( rc .ne. 0 ) return

!     Adjust values
!     -------------
      w10   = w   / RM ( CD,     ozdl, z, zlnz )
      dth10 = dth / RT ( CD, CT, ozdl, z, zlnz )
      dq10  = dq  / RE ( CD, CE, ozdl, z, zlnz )
      ta10  = Tsg10 - dth10

!     Iterate...
!     ----------
      do i = 1, itmax

!     Calculate a z/l
!     ---------------
         call LP10( CD, CT, CE, zdl, w10, ta10, dth10, dq10, rc )
         if ( rc .ne. 0 ) return

!     Find adjusted values
!     --------------------
         w10   = w   / RM( CD,     zdl, z, zlnz )
         fofzdl = CD / CDN
         dth10 = dth / RT( CD, CT, zdl, z, zlnz )
         dq10  = dq  / RE( CD, CE, zdl, z, zlnz )
!        ta10  = Tsg10 - dth10
         m = m + 1

!     If zdl and ozdl are close enough, we're done
!     --------------------------------------------
         if ( abs( zdl - ozdl ) .le. abs(eps * ozdl) ) then
            return
         end if

!     If not, we must iterate again
!     -----------------------------
         ozdl = zdl

      end do

!     Failure to converge
!     -------------------
      rc = 1
      return

!      print *, 'zdl didnt converge after 100 iter: w, w10, dt = ',
!     .          w, w10, dth
!      if ( w * w10 .lt. 0. ) then
!         print *,'bad correction:w w10 dth dth10 dq dq10 ta ta10'
!         print *, w, w10, dth, dth10, dq, dq10, ta, ta10
!      end if

      return
      end subroutine LPZ

*......................................................................

      subroutine LP10 ( CD, CT, CE, ZDL, Winput, Ta, DT, DQ, rc )

      real  CD, CT, CE, ZDL, Winput, Ta, DT, DQ
      integer rc

!        This subroutine returns the drag coefficient computed using
!     the Large and Pond (1981,1982) formulation.  This version
!     iterates the drag coefficients/stability correction.
!     Units:  Winput in m/s, Ta in K, DT in K, DQ in g/kg

!                                     
      parameter ( cappa = 0.4, g = 9.81, Z = 10., cgZ = cappa * g * Z )
      parameter ( pi = 3.1415926, pid2 = 0.5 * pi )

!     maximum number of iterations and fractional error
!     -------------------------------------------------
      parameter ( ITMAX = 15, EPS = 1.e-4 )

      common / lpneut / CDN, fofzdl

!     lower bound on W (1 m/s) to avoid singularities
!     (consistent with Tremberth et al., 1989)
!     -----------------------------------------------
      W = max ( 1., Winput )

!     first compute neutral drag coefficient
!     (for now uses L&P original formula; same as
!      Harrison 1989).  The number below also
!      comes from Harrison's paper.
!     --------------------------------------
!!!      if ( W .ge. 11. ) then
!!!           CDN = ( 0.49 + 0.065 * W ) * 1. E -3
!!!      else
!!!           CDN = 1.205 * 1. E -3
!!!      end if

#ifdef UWMCOADS

!     Trenberth et al. dependence.
!     ----------------------------
      if ( W .ge. 10. ) then
           CDN = ( 0.49 + 0.065 * W ) * 1. E -3
      else if ( W .ge. 3. ) then
           CDN = 1.14 * 1. E -3
      else
           CDN = ( 0.62 + 1.56 / W ) * 1. E -3
      end if

!     Neutral Dalton number
!     ---------------------
      CEN = 1.2 * 1. E -3

!     neutral Stanton number (Pond's notes)
!     -------------------------------------
      sguess = - DT
      iter = 0
 1000 continue
      if ( sguess .lt. 0. ) then
           CTN = 1.2 * 1. E -3
      else
           CTN = 0.75 * 1. E -3
      end if

#else


!     CCM3 formulation
!     ----------------

!     Neutral drag coefficient
!     -----------------------
      CDN = 0.0027 / W + 0.000142 + 0.0000764 * W

!     Neutral Dalton number
!     ---------------------
      CEN = 0.0346 * sqrt ( cdn )

!     neutral Stanton number
!     ----------------------
      sguess = - DT
      iter = 0
 1000 continue
      if ( sguess .lt. 0. ) then
           CTN = 0.0327 * sqrt ( cdn )
      else
           CTN = 0.0180 * sqrt ( cdn )
      end if

#endif

!     stability independent part of ZDL
!     ---------------------------------
      S  = - ( cgZ / ( W**2. * Ta ) ) 
     .       * ( DT + (1.72 E -6) * (Ta**2.) * DQ )


!     first guess for ZDL
!     -------------------
      oZDL = S * CTN / CDN**1.5

!     now iterate to find stability parameter
!     ---------------------------------------
      do 10 it = 1, ITMAX

!        CD/CT with previous ZDL
!        -----------------------
         call FORMCD ( CD, f, oZDL, CDN )
         call FORMCT ( CT, oZDL, CTN, CDN, f )
         call FORMCE ( CE, oZDL, CEN, CDN, f )

!        update ZDL
!        ----------
         ZDL = S * CT / CD**1.5

!        good enough?
!        ------------
         if ( abs(oZDL-ZDL) .le. abs(EPS*oZDL) ) go to 11

         oZDL = ZDL

 10   continue
      print *, 'CDLP: W, Ta, DT, DQ, Z/L: ', W, Ta, DT, DQ, ZDL
      print *,  'CDLP: ZDL iteration did not converge.'

 11   continue


!     consistency check (the first guess is sign(Z/L) = sign(-DT),
!      if not, iterate just once)
!     ------------------------------------------------------------
      if ( ZDL * sguess .lt. 0. ) then
           iter = iter + 1
!!!        if ( iter .gt. 1 ) stop 'CDLP: too many steps.'
           if ( iter .gt. 1 ) then
                print *, 'CDLP: too many steps.'
                rc = 1
                return
           end if
           sguess = ZDL
           go to 1000
      end if


!     compute CD with final ZDL estimate
!     ----------------------------------
      call FORMCD ( CD, f, ZDL, CDN )   
      call FORMCT ( CT, ZDL, CTN, CDN, f )
      call FORMCE ( CE, ZDL, CEN, CDN, f )

ccc   print *, 'W CD CDN f ', W, f*CDN, CDN, f, it

      rc = 0
      return
      end subroutine LP10 

!......................................................................

      subroutine FORMCD ( CD, f, ZDL, CDN )

!        Given the stabilitility parameter ZDL and the neutral
!     drag coefficient CDN, this function returns the full CD
!     and the stability correction f.

      parameter ( cappa = 0.4 )

!     compute stability correction
!     ----------------------------
      psi = SPSIM( ZDL )
      
      f = 1. / ( 1. - sqrt(CDN) * psi / cappa ) ** 2.

!     form the drag coefficeint
!     -------------------------
      CD = CDN * f

      return
      end subroutine FORMCD 

!......................................................................

      subroutine FORMCT ( CT, ZDL, CTN, CDN, f )

!        Given the stabilitility parameter ZDL, the neutral
!     drag coefficient CDN, this subroutine returns the full CT
!     including the stability correction.

      parameter ( cappa = 0.4 )

!     compute stability correction
!     ----------------------------
      psi = SPSIT( ZDL )
      
      g = f / ( 1. - psi * CTN / ( cappa * sqrt(CDN) ) )

!     form the coefficeint
!     --------------------
      CT = CTN * g

      return
      end subroutine FORMCT 

!......................................................................

      subroutine FORMCE ( CE, ZDL, CEN, CDN, f )

!     Given the stability ZDL, CDN, CEN, and the ratio f,
!     this routine returns CE with stability correction.

      parameter ( cappa = 0.4 )

!     compute stability correction
!     ----------------------------
      psi = SPSIT( ZDL )

      g = f / ( 1. - psi * CEN / ( cappa * sqrt(CDN) ) )

!     Form the coefficient
!     --------------------
      CE = CEN * g

      return
      end subroutine FORMCE 

!..................................................................

      function RM ( CD, z10dl, z, zlnz )

      real zlnz

      cappa = 0.4

      s = zlnz - SPSIM ( z / 10. * z10dl  ) + SPSIM( z10dl )
      RM = 1. + ( sqrt( CD ) / cappa ) * s

      return
      end function RM 

!...................................................................

      function RT ( CD, CT, z10dl, z, zlnz )

      real zlnz

      sig = zlnz - SPSIT( z / 10. * z10dl ) + SPSIT( z10dl )
      RT = 1. + ( CT / sqrt( CD ) ) * sig

      return
      end function RT 

!....................................................................

      function RE ( CD, CE,  z10dl, z, zlnz )

      real zlnz

      sig = zlnz - SPSIT( z / 10. * z10dl ) + SPSIT( z10dl )
      RE = 1. + ( CE / sqrt( CD ) ) * sig

      return
      end function RE 

!.....................................................................

      function SPSIM ( zdl )

!!!      include 'lp.h'

      if ( zdl .le. zdla .or. zdl .ge. zdlb ) then
         SPSIM = EXPSIM ( zdl )
      else
         SPSIM = SEVAL( zdl, zfit, psim, nzdl, bbm, ccm, ddm ) 
      end if

      return
      end function SPSIM 

!....................................................................

      function SPSIT ( zdl )

!!!      include 'lp.h'

      if ( zdl .le. zdla .or. zdl .ge. zdlb ) then
         SPSIT = EXPSIT ( zdl )
      else
         SPSIT =  SEVAL( zdl, zfit, psit, nzdl, bbt, cct, ddt )
      end if

      return
      end function SPSIT 

!.....................................................................

      function EXPSIM ( zdl )

      parameter ( pid = 3.14159 / 2.0 )

      if ( zdl .gt. 0. ) then
         EXPSIM = -7. * zdl
      else
         x = ( 1. - 16. * zdl )**0.25
         EXPSIM = 2. * log( 0.5 * ( 1. + x ) )  
     .          + log( 0.5 * ( 1.0 + x**2. ) )
     .          - 2.0 * atan(x) + pid
      end if

      return
      end function EXPSIM

!.....................................................................

      function EXPSIT( zdl )

      if ( zdl .gt. 0. ) then
         EXPSIT = -7. * zdl
      else
         x = ( 1. - 16. * zdl )**0.25
         EXPSIT = 2.0 * log( 0.5 * ( 1.0 + x**2. ) )
      end if

      return
      end function EXPSIT


      subroutine spline ( b, c, d, n, x, y )
 
      integer  n
      real     b(n), c(n), d(n), x(n), y(n)
 
! **********************************************************************
! *   FIRST VERSION: 02/13/86 (AMS)   CURRENT VERSION: 02/13/86 (AMS)  *
! *--------------------------------------------------------------------*
! *                                                                    *
! *     THIS ROUTINE COMPUTES THE COEFFICIENTS  B(I), C(I), D(I),      *
! *   I = 1, ..., N FOR A CUBIC INTERPOLATING SPLINE.                  *
! *                                                                    *
! *    S(X) = Y(I) + B(I) * ( X - X(I) ) + C(I) * ( X - X(I) )**2      *
! *                + D(I) * ( X - X(I) )**3                            *
! *                                                                    *
! *--------------------------------------------------------------------*
! *                                                                    *
! *   ON INPUT                                                         *
! *   --------                                                         *
! *   N   ---   NUMBER OF DATA POINTS OR KNOTS ( N .GE. 2 )            *
! *   X   ---   THE ABSCISSAS OF THE KNOTS IN STRICTLY INCREASING      *
! *             ORDER                                                  *
! *   Y   ---   THE ORDINATES OF THE KNOTS                             *
! *                                                                    *
! *   ON OUTPUT                                                        *
! *   ---------                                                        *
! *   B, C, D   ---   ARRAYS OF SPLINE COEFFICIENTS AS DEFINED ABOVE.  *
! *                                                                    *
! *--------------------------------------------------------------------*
! *                                                                    *
! *   INTERPRETAION:                                                   *
! *   -------------                                                    *
! *                                                                    *
! *   Y(I)   ---   S    ( X(I) )                                       *
! *   B(I)   ---   S'   ( X(I) )                                       *
! *   C(I)   ---   S''  ( X(I) ) / 2                                   *
! *   D(I)   ---   S''' ( X(I) ) / 6  ( DERIVATIVE FROM THE RIGHT )    *
! *                                                                    *
! *   WHERE ' DENOTES DIFFERENTIATION. THE ACCOMPANYING SUBPROGRAM     *
! *   FUNCTION  SEVAL  CAN BE USED TO EVALUATE THE SPLINE.             *
! *                                                                    *
! **********************************************************************

      integer  nm1, ib, i
      real     t

      nm1 = n - 1
      if ( n .lt. 2 )  return
      if ( n .lt. 3 )  go to 50

!     SET UP TRIDIAGONAL SYSTEM
!     -------------------------
 
!     B = DIAGONAL, D = DIAGONAL, C = RIGHT HAND SIDE
!     -----------------------------------------------
      d(1) = x(2) - x(1)
      c(2) = ( y(2) - y(1) ) / d(1)
      do 10 i = 2, nm1
         d(i)   = x(i+1) - x(i)
         b(i)   = 2.0 * ( d(i-1) + d(i) )
         c(i+1) = ( y(i+1) - y(i) ) / d(i)
         c(i)   = c(i+1) - c(i)
 10   continue
 
!     END CONDITIONS. THIRD DERIVATIVES AT  X(1)  AND  X(N)
!     OBTAINED FROM DIVIDED DIFFERENCES
!     -----------------------------------------------------
      b(1) = - d(1)
      b(n) = - d(n-1)
      c(1) = 0.0
      c(n) = 0.0
      if ( n .eq. 3 )  go to 15
      c(1) = c(3)   / ( x(4) - x(2)   )
     1     - c(2)   / ( x(3)   - x(1)   )
      c(n) = c(n-1) / ( x(n) - x(n-2) )
     1     - c(n-2) / ( x(n-1) - x(n-3) )
      c(1) = c(1) * d(1)**2 / ( x(4) - x(1) )
      c(n) = -c(n) * d(n-1)**2 / ( x(n) - x(n-3) )
 
!     FORWARD ELIMINATION
!     -------------------
 15   do 20 i = 2, n
          t    = d(i-1) / b(i-1)
          b(i) = b(i)  -  t * d(i-1)
          c(i) = c(i)  -  t * c(i-1)
 20   continue
 
!     BACK SUBSTITUTION
!     -----------------
      c(n) = c(n) / b(n)
      do 30 ib = 1, nm1
         i = n - ib
         c(i) = ( c(i) - d(i) * c(i+1) ) / b(i)
 30   continue
 
!     C(I) NOW CONTAINS SIGMA(I)
!     --------------------------
 
!     COMPUTE POLYNOMIAL COEFFICIENTS
!     -------------------------------
      b(n) = ( y(n) - y(nm1) ) / d(nm1)
     1     + d(nm1) * ( c(nm1) + 2.0 * c(n) )
      do 40 i = 1, nm1
         b(i) = ( y(i+1) - y(i) ) / d(i)
     1        - d(i) * ( c(i+1) + 2.0 * c(i) )
         d(i) = ( c(i+1) - c(i) ) / d(i)
         c(i) = 3.0 * c(i)
 40   continue
      c(n) = 3.0 * c(n)
      d(n) = d(n-1)
 
      return
 
 50   b(1) = ( y(2) - y(1) ) / ( x(2) - x(1) )
      c(1) = 0.0
      d(1) = 0.0
      b(2) = b(1)
      c(2) = 0.0
      d(2) = 0.0

      return

      end subroutine spline

!----------------------------------------------------------------------

      real function seval ( u, x, y, n, b, c, d )

      integer  n
      real     u, x(n), y(n), b(n), c(n), d(n)
! **********************************************************************
! *   FIRST VERSION: 02/13/86 (AMS)   CURRENT VERSION: 02/13/86 (AMS)  *
! *--------------------------------------------------------------------*
! *                                                                    *
! *       THIS ROUTINE  RETURNS THE CUBIC SPLINE FUNCTION              *
! *                                                                    *
! *    SEVAL = Y(I) + B(I) * ( U - X(I) ) + C(I) * ( U - X(I) )**2     *
! *                 + D(I) * ( U - X(I) )**3                           *
! *                                                                    *
! *    WHERE  X(I) .LT. U .LT. X(I+1), USING HORNER'S RULE.            *
! *       IF U .LT. X(1) THEN I=1 IS USED, IF U .GE. X(N) THEN I=N     *
! *    IS USED.                                                        *
! *                                                                    *
! *--------------------------------------------------------------------*
! *                                                                    *
! *   ON INPUT                                                         *
! *   --------                                                         *
! *   N        ---   NUMBER OF DATA POINTS OR KNOTS ( N .GE. 2 )       *
! *   U        ---   ABSCISSA AT WHICH THE SPLINE IS TO BE EVALUATED   *
! *   X, Y     ---   THE ARRAYS OF ABSCISSAS AND ORDINATES.            *
! *   B, C, D  ---   ARRAYS OF SPLINE COEFFICIENTS AS DEFINED ABOVE.   *
! *                                                                    *
! *      IF  U  IS NOT IN THE SAME INTERVAL AS THE PREVIOUS CALL,      *
! *   THEN A BINARY SEARCH IS PERFORMED TO DETERMINE THE PROPER        *
! *   INTERVAL.                                                        *
! *                                                                    *
! **********************************************************************

      save i
      integer  i, j, k
      real     dx

      data i / 1 /

      if ( i .ge. n      ) i = 1
      if ( u .lt. x(i)   ) go to 10
      if ( u .le. x(i+1) ) go to 30

!     BINARY SEARCH
!     -------------
 10   i = 1
      j = n + 1
 20   k = ( i + j ) / 2
      if ( u .lt. x(k) )  j = k
      if ( u .ge. x(k) )  i = k
      if ( j .gt. i+1  )  go to 20

!     EVALUATE SPLINE
!     ---------------
 30   dx    = u - x(i)
      seval = y(i)
     .      + dx * ( b(i) + dx * ( c(i) + dx * d(i) ) )

      return

      end function seval 

!----------------------------------------------------------------------

      end module m_lp
