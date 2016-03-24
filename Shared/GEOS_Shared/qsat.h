
!  $Id$

      real, parameter :: ESFAC = GEOS_H2OMW/GEOS_AIRMW
      real, parameter :: ERFAC = (1.0-ESFAC)/ESFAC
      real, parameter :: MAX_MIXING_RATIO = 1.  
      real, parameter :: ZEROC   = GEOS_TICE
      real, parameter :: TMIX    = -20.

#ifdef LIKE_NCAR

      real,    parameter :: TMIN       =  150.
      real,    parameter :: TMAX       =  370.
      real,    save      :: ESTBLE(nint(TMAX-TMIN)+1)
      real,    save      :: ESTBLW(nint(TMAX-TMIN)+1)
      real,    save      :: ESTBLX(nint(TMAX-TMIN)+1)
      logical, save      :: FIRST=.true.

#else

      real, parameter :: TMIN    = -95.
      real, parameter :: TSTARR1 = -75.
      real, parameter :: TSTARR2 = -65.
      real, parameter :: TSTARR3 = -50.
      real, parameter :: TSTARR4 = -40.
      real, parameter :: TMAX    = +60.

      real, parameter :: B6 = 6.136820929E-11*ESFAC
      real, parameter :: B5 = 2.034080948E-8 *ESFAC
      real, parameter :: B4 = 3.031240396E-6 *ESFAC
      real, parameter :: B3 = 2.650648471E-4 *ESFAC
      real, parameter :: B2 = 1.428945805E-2 *ESFAC
      real, parameter :: B1 = 4.436518521E-1 *ESFAC
      real, parameter :: B0 = 6.107799961E+0 *ESFAC
      real, parameter :: BI6= 1.838826904E-10*ESFAC
      real, parameter :: BI5= 4.838803174E-8 *ESFAC
      real, parameter :: BI4= 5.824720280E-6 *ESFAC
      real, parameter :: BI3= 4.176223716E-4 *ESFAC
      real, parameter :: BI2= 1.886013408E-2 *ESFAC
      real, parameter :: BI1= 5.034698970E-1 *ESFAC
      real, parameter :: BI0= 6.109177956E+0 *ESFAC
      real, parameter :: S16= 0.516000335E-11*ESFAC
      real, parameter :: S15= 0.276961083E-8 *ESFAC
      real, parameter :: S14= 0.623439266E-6 *ESFAC
      real, parameter :: S13= 0.754129933E-4 *ESFAC
      real, parameter :: S12= 0.517609116E-2 *ESFAC
      real, parameter :: S11= 0.191372282E+0 *ESFAC
      real, parameter :: S10= 0.298152339E+1 *ESFAC
      real, parameter :: S26= 0.314296723E-10*ESFAC
      real, parameter :: S25= 0.132243858E-7 *ESFAC
      real, parameter :: S24= 0.236279781E-5 *ESFAC
      real, parameter :: S23= 0.230325039E-3 *ESFAC
      real, parameter :: S22= 0.129690326E-1 *ESFAC
      real, parameter :: S21= 0.401390832E+0 *ESFAC
      real, parameter :: S20= 0.535098336E+1 *ESFAC

#endif
