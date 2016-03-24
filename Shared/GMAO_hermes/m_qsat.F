!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_qsat --- Contains routines to compute saturation specific humidity
!                      (consistent with fvccm3-1.2.0)
!
! !INTERFACE:
!
      MODULE  m_qsat

! !USES:

      Implicit NONE
!
! !PUBLIC TYPES:
!
      PRIVATE
      PUBLIC  gestbl            ! initialize saturation vapor pressure table  
      PUBLIC  vqsat             ! compute saturation specific humidity
!
! !DESCRIPTION: This module conatins routines to compute saturation specific humidity
!
! !REVISION HISTORY:
!
!  02Jan2002  J Chern   Initial specification and prologues.
!
!EOP
!-------------------------------------------------------------------------

      CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  vqsat --- Compute saturation specific humidity
!                      (consistent with fvccm3-1.2.0)
!
! !INTERFACE:
!
      subroutine vqsat ( t, p, es, qs, len, undef )

! !USES:

      Implicit NONE

! !INPUT PARAMETERS:

      integer, intent(in)     :: len         ! vector length
      real, intent(in)        :: t(len)      ! temperature [K]
      real, intent(in)        :: p(len)      ! pressure [Pa]
      real, intent(in)        :: undef       ! undefined value

! !OUTPUT PARAMETERS:

      real, intent(out)       :: es(len)     ! Saturation vapor pressure [g/g]
      real, intent(out)       :: qs(len)     ! Saturation specific humidity [Pa]

! !DESCRIPTION: This routine computes saturation vapor pressure [Pa] and
!             saturation specific humidity [g/g] as a function of
!             temperature [K] and pressure [Pa].
!
!----------------------------Code History-------------------------------
! Original version:  J. Hack
! Standardized:      J. Rosinski, June 1992
!                    T. Acker, March 1996
! Reviewed:          J. Hack, August 1992
! Modified:          To handle pressure level data which can have undefined
!                    values.  This code was also stripped of the use of the
!                    land index array location.
!-----------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!  17nov2000  Dee    Obtained from Sharon Nebuda, taken from fvccm3-1.2.0
!                    Added DAO prologue; worried about name of developer.
!                    Modification: Allow array of pressures.
!
!EOP
!-------------------------------------------------------------------------

C
C--------------------------Local Variables------------------------------
C
      real omeps               ! 1 - 0.622
      integer i,ii             ! Local vector indices
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C

C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
c     write(6,*) ' epsqs = ',epsqs
CDIR$ IVDEP
      do i=1,len
        if (t(i) .ne. undef) then
         es(i) = estblf(t(i))
C
C Saturation specific humidity
C
         qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))

C
C The following check is to avoid the generation of negative values
C that can occur in the upper stratosphere and mesosphere
C
         qs(i) = min(1.0,qs(i))
C
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p(i)
         end if
        else
          qs(i) = undef
          es(i) = undef
        endif
      end do

      return
C
      end subroutine vqsat


      subroutine gestbl
C-----------------------------------------------------------------------
C
C Builds saturation vapor pressure table for later lookup procedure.
C Uses Goff & Gratch (1946) relationships to generate the table
C according to a set of free parameters defined below.  Auxiliary
C routines are also included for making rapid estimates (well with 1%)
C of both es and d(es)/dt for the particular table configuration.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Aug 1992
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tmn           ! Minimum temperature entry in es lookup table
      real tmx           ! Maximum temperature entry in es lookup table
      real epsil         ! Ratio of h2o to dry air molecular weights
      real trice         ! Transition range from es over range to es over ice
      real latvap        ! Latent heat of vaporization
      real latice        ! Latent heat of fusion
      real rh2o          ! Gas constant for water vapor
      real cpair         ! Specific heat of dry air
C
C---------------------------Local variables-----------------------------
C
      real t             ! Temperature
      integer n          ! Increment counter
      integer lentbl     ! Calculated length of lookup table
      integer itype      ! Ice phase: 0 -> no ice phase
                         !            1 -> ice phase, no transition
                         !           -x -> ice phase, x degree transition
      logical ip         ! Ice phase logical flag
C
C---------------------------Statement function--------------------------
C
C
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C

C-----------------------------------------------------------------------
C
C Set es table parameters
C
      tmin   = 173.16       ! Minimum temperature entry in table
      tmax   = 375.16       ! Maximum temperature entry in table
      ttrice = 20.          ! Trans. range from es over h2o to es over ice
      icephs = .true.       ! Ice phase (true or false)
C
C Set physical constants required for es calculation
C
      epsqs  = 0.622
      hlatv  = 2.5104e6
      hlatf  = 3.336e5
      rgasv  = 4.61e2
      cp     = 1004.64
C
      lentbl = ifix(tmax-tmin+2.000001)
      if (lentbl .gt. plenest) then
         write(6,9000) tmax, tmin, plenest
         stop
      end if
C
C Begin building es table.
C Check whether ice phase requested.
C If so, set appropriate transition range for temperature
C
      if (icephs) then
         if(ttrice.ne.0.0) then
            itype = -ttrice
         else
            itype = 1
         end if
      else
         itype = 0
      end if
C
      t = tmin - 1.0
      do n=1,lentbl
         t = t + 1.0
         call gffgch(t,estbl(n),itype)
      end do
C
      do n=lentbl+1,plenest
         estbl(n) = -99999.0
      end do
C
C Table complete -- Set coefficients for polynomial approximation of
C difference between saturation vapor press over water and saturation
C pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
C is valid in the range -40 < t < 0 (degrees C).
C
C                  --- Degree 5 approximation ---
C
      pcf(1) =  5.04469588506e-01
      pcf(2) = -5.47288442819e+00
      pcf(3) = -3.67471858735e-01
      pcf(4) = -8.95963532403e-03
      pcf(5) = -7.78053686625e-05
C
C                  --- Degree 6 approximation ---
C
C-----pcf(1) =  7.63285250063e-02
C-----pcf(2) = -5.86048427932e+00
C-----pcf(3) = -4.38660831780e-01
C-----pcf(4) = -1.37898276415e-02
C-----pcf(5) = -2.14444472424e-04
C-----pcf(6) = -1.36639103771e-06
C
C
 9000 format('GESTBL: FATAL ERROR *********************************',/,
     $     ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH',
     $     ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/,
     $     ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
C
      end subroutine gestbl

      subroutine gffgch(t       ,es      ,itype   )
C-----------------------------------------------------------------------
C
C Computes saturation vapor pressure over water and/or over ice using
C Goff & Gratch (1946) relationships.  T (temperature), and itype are
C input parameters, while es (saturation vapor pressure) is an output
C parameter.  The input parameter itype serves two purposes: a value of
C zero indicates that saturation vapor pressures over water are to be
C returned (regardless of temperature), while a value of one indicates
C that saturation vapor pressures over ice should be returned when t is
C less than 273.16 degrees k.  If itype is negative, its absolute value
C is interpreted to define a temperature transition region below 273.16
C degrees k in which the returned saturation vapor pressure is a
C weighted average of the respective ice and water value.  That is, in
C the temperature range 0 => -itype degrees c, the saturation vapor
C pressures are assumed to be a weighted average of the vapor pressure
C over supercooled water and ice (all water at 0 c; all ice at -itype
C c).  Maximum transition range => 40 c
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real t          ! Temperature
      integer itype   ! Flag for ice phase and associated transition
C
C Output arguments
C
      real es         ! Saturation vapor pressure
C
C---------------------------Local variables-----------------------------
C
      real e1         ! Intermediate scratch variable for es over water
      real e2         ! Intermediate scratch variable for es over water
      real eswtr      ! Saturation vapor pressure over water
      real f          ! Intermediate scratch variable for es over water
      real f1         ! Intermediate scratch variable for es over water
      real f2         ! Intermediate scratch variable for es over water
      real f3         ! Intermediate scratch variable for es over water
      real f4         ! Intermediate scratch variable for es over water
      real f5         ! Intermediate scratch variable for es over water
      real ps         ! Reference pressure (mb)
      real t0         ! Reference temperature (freezing point of water)
      real term1      ! Intermediate scratch variable for es over ice
      real term2      ! Intermediate scratch variable for es over ice
      real term3      ! Intermediate scratch variable for es over ice
      real tr         ! Transition range for es over water to es over ice
      real ts         ! Reference temperature (boiling point of water)
      real weight     ! Intermediate scratch variable for es transition
      integer itypo   ! Intermediate scratch variable for holding itype
C
C-----------------------------------------------------------------------
C
C Check on whether there is to be a transition region for es
C
      if (itype.lt.0) then
        tr    = abs(float(itype))
        itypo = itype
        itype = 1
      else
        tr    = 0.0
        itypo = itype
      end if
      if (tr .gt. 40.0) then
        write(6,900) tr
        stop
      end if
C
      if(t .lt. (273.16 - tr) .and. itype.eq.1) go to 10
C
C Water
C
      ps = 1013.246
      ts = 373.16
      e1 = 11.344*(1.0 - t/ts)
      e2 = -3.49149*(ts/t - 1.0)
      f1 = -7.90298*(ts/t - 1.0)
      f2 = 5.02808*log10(ts/t)
      f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
      f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
      f5 = log10(ps)
      f  = f1 + f2 + f3 + f4 + f5
      es = (10.0**f)*100.0
      eswtr = es
C
      if(t.ge.273.16 .or. itype.eq.0) go to 20
C
C Ice
C
   10 continue
      t0    = 273.16
      term1 = 2.01889049/(t0/t)
      term2 = 3.56654*log(t0/t)
      term3 = 20.947031*(t0/t)
      es    = 575.185606e10*exp(-(term1 + term2 + term3))
C
      if (t.lt.(273.16 - tr)) go to 20
C
C Weighted transition between water and ice
C
      weight = min((273.16 - t)/tr,1.0)
      es = weight*es + (1.0 - weight)*eswtr
C
   20 continue
      itype = itypo
      return
C
  900 format('GFFGCH: FATAL ERROR ******************************',/,
     $       'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR',
     $       ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF',
     $       ' 40.0 DEGREES C',/, ' TR = ',f7.2)
C
      end subroutine gffgch

      end MODULE m_qsat
