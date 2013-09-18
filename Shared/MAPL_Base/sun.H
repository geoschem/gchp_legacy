! $Id: sun.H,v 1.8 2010-04-13 14:10:20 ltakacs Exp $


      type(MAPL_SunOrbit),               intent(IN ) :: ORBIT
      real,                              intent(IN ) :: LONS DIMENSIONS
      real,                              intent(IN ) :: LATS DIMENSIONS
      real,                              intent(OUT) :: ZTH  DIMENSIONS
      real,                              intent(OUT) :: SLR  DIMENSIONS
      type(ESMF_TimeInterval), optional, intent(INout) :: INTV
      type(ESMF_Clock),        optional, intent(IN ) :: CLOCK
      integer,                 optional, intent(IN ) :: TIME
      type(ESMF_Time),         optional, intent(IN ) :: currTime
      real,                    optional, intent(OUT) :: DIST
      real,                    optional, intent(OUT) :: ZTHB DIMENSIONS
      real,                    optional, intent(OUT) :: ZTHD DIMENSIONS
      integer,                 optional, intent(OUT) :: RC

!     Locals

      character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_SunGetInsolation"
      integer                               :: STATUS

      integer           :: IDAY, IDAYP1
      integer           :: NT
      real              :: FAC, ZS, ZC, ANG, AA, DD
      real*8            :: SECS

      integer           :: YEAR
      integer           :: SEC_OF_DAY
      integer           :: DAY_OF_YEAR
      integer           :: L
      type (ESMF_Time)  :: CURRENTTIME
      type (ESMF_Time)  :: firstofyear
      type (ESMF_Clock) :: MYCLOCK
      type (ESMF_TimeInterval)  :: ts
      real,    dimension THE_SIZE :: ZTT, SLT, Y, ZTB, ZTD, NCC

!     Begin
      
      if (present(currTime)) then
         CURRENTTIME = CURRTIME
      else
         if (present(CLOCK)) then
            call ESMF_ClockGet(      CLOCK, currTime=CURRENTTIME, RC=STATUS)
         else
            call ESMF_ClockGet(ORBIT%CLOCK, currTime=CURRENTTIME, RC=STATUS)
         end if
         VERIFY_(STATUS)
      end if

      if(.not.present(INTV)) then

         call ESMF_TimeGet(CURRENTTIME, YY=YEAR, S=SEC_OF_DAY, &
              dayOfYear=DAY_OF_YEAR, RC=STATUS)
         VERIFY_(STATUS)
         YEAR = mod(YEAR-1,ORBIT%YEARS_PER_CYCLE)
         IDAY = YEAR*int(ORBIT%YEARLEN)+DAY_OF_YEAR

         if(present(TIME)) then
          if(TIME.ne.0 .and. TIME.lt.10) then
            if(TIME==MAPL_SunDailyMean) then  ! Daily mean Insolation
               SLR  = sin(LATS)*ORBIT%ZS(IDAY)
               ZTH  = cos(LATS)*ORBIT%ZC(IDAY)
               Y    =  max(-1.0,-SLR/ZTH)
               where (Y < 1.0)
                  Y   = ACOS(Y)
                  SLR = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLR + SIN(Y)*ZTH)
                  ZTH = SLR*MAPL_PI / (4.0*Y)
               elsewhere
                  SLR = 0.0
                  ZTH = 0.0
               endwhere
               if(present(ZTHB)) ZTHB = ZTH
               if(present(ZTHD)) ZTHD = ZTH
               if(present(DIST)) DIST = ORBIT%PP(IDAY)
               RETURN_(ESMF_SUCCESS)
            elseif(TIME==MAPL_SunAnnualMean) then  ! Annual mean Insolation
               if(YEAR==0) then
                  L=1
               else
                  L=0
               endif
               SLR  = 0.0
               ZTH  = 0.0
               do IDAY = YEAR*ORBIT%DAYS_PER_CYCLE+(2-L),(YEAR+1)*ORBIT%DAYS_PER_CYCLE+L
                  SLT  = sin(LATS)*ORBIT%ZS(IDAY)
                  ZTT  = cos(LATS)*ORBIT%ZC(IDAY)
                  Y    =  max(-1.0,-SLT/ZTT)
                  where (Y < 1.0)
                     Y   = ACOS(Y)
                     SLT = (4.0*ORBIT%PP(IDAY)/MAPL_PI) * (Y *SLR + SIN(Y)*ZTH)
                     ZTT = SLR*MAPL_PI / (4.0*Y)
                  elsewhere
                     SLT = 0.0
                     ZTH = 0.0
                  endwhere
                  SLR = SLR + SLT
                  ZTH = ZTH + ZTT
               end do
               SLR = SLR / float(ORBIT%DAYS_PER_CYCLE)
               ZTH = ZTH / float(ORBIT%DAYS_PER_CYCLE)
               if(present(ZTHB)) ZTHB = ZTH
               if(present(ZTHD)) ZTHD = ZTH
               if(present(DIST)) DIST = 1.0
               RETURN_(ESMF_SUCCESS)
            else  ! Solar declination fixed at TIME
               call GETIDAY(IDAY,TIME,ORBIT,RC=STATUS)
               VERIFY_(STATUS)
               IDAYP1 = IDAY
            end if
          else
            IDAYP1 = mod(IDAY,ORBIT%DAYS_PER_CYCLE) + 1
          end if
         else
            IDAYP1 = mod(IDAY,ORBIT%DAYS_PER_CYCLE) + 1
         endif

         ASSERT_(IDAY   <= 1461 .AND. IDAY   > 0)
         ASSERT_(IDAYP1 <= 1461 .AND. IDAYP1 > 0)

         FAC = real(SEC_OF_DAY)/86400.
         ANG = 2.0*MAPL_PI*FAC

         ZS  = ORBIT%ZS(IDAYP1)*FAC + ORBIT%ZS(IDAY)*(1.-FAC)
         ZC  = ORBIT%ZC(IDAYP1)*FAC + ORBIT%ZC(IDAY)*(1.-FAC)
         AA  = ORBIT%PP(IDAYP1)*FAC + ORBIT%PP(IDAY)*(1.-FAC)

         ZTH =         ZS*SIN(LATS) +       ZC*COS(LATS) &
              * (sin(ANG)*SIN(LONS) - cos(ANG)*COS(LONS))
         ZTH = max(ZTH, 0.0)
         SLR = ZTH*AA

         if(present(ZTHB)) ZTHB = ZTH
         if(present(ZTHD)) ZTHD = ZTH
         if(present(DIST)) DIST = AA

      else 

         call ESMF_TimeIntervalGet (INTV, S_R8=SECS, rc=STATUS)
         VERIFY_(STATUS)

         if(SECS>1500.) then
            NT = nint(SECS/300.0)
         else
            NT = 5
         end if

         TS = INTV/dble(NT)

         MYCLOCK =  ESMF_ClockCreate("SUN_CLOCK", &
              TimeStep  = ts                   , &
              StartTime = CURRENTTIME          , &
              StopTime  = CURRENTTIME + INTV   , &
              rc        = STATUS                 )
         VERIFY_(STATUS)

         SLR = 0.0
         ZTH = 0.0

         if(present(ZTHD)) then
            ZTHD = 0.0
            NCC  = 0.0
         end if
         if(present(ZTHB)) ZTHB = 0.0
         if(present(DIST)) DIST = 0.0

         call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
              CLOCK=MYCLOCK, TIME=TIME, DIST=DD, RC=STATUS)
         VERIFY_(STATUS)

         do L=1,NT
            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5
            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if
            if(present(DIST)) DIST = DIST + 0.5*DD

            call ESMF_ClockAdvance    ( MYCLOCK, RC=STATUS )
            VERIFY_(STATUS)
            call MAPL_SunGetInsolation( LONS, LATS, ORBIT, ZTT, SLT, &
                 CLOCK=MYCLOCK, TIME=TIME, ZTHB=ZTB, ZTHD=ZTD, DIST=DD, RC=STATUS)
            VERIFY_(STATUS)

            SLR = SLR +     SLT*0.5
            ZTH = ZTH + ZTT*SLT*0.5
            if(present(ZTHB)) ZTHB = ZTHB + 0.5*ZTT
            if(present(ZTHD)) then
               where(ZTT>0.0)
                  ZTHD = ZTHD + 0.5*ZTT
                  NCC  = NCC  + 0.5
               end where
            end if
            if(present(DIST)) DIST = DIST + 0.5*DD
         enddo

         where(SLR > 0.0)
            SLR = SLR/dble(NT)
            ZTH = ZTH/dble(NT)
            ZTH = ZTH / SLR
         elsewhere
            ZTH = 0.0
            SLR = 0.0
         end where

         if(present(ZTHB)) ZTHB = ZTHB/dble(NT)

         if(present(ZTHD)) then
            where(NCC>0.0)
               ZTHD = ZTHD/dble(NCC)
            end where
         end if

         if(present(DIST)) DIST = DIST/dble(NT)

         call ESMF_ClockDestroy(myclock, rc=status)
         VERIFY_(STATUS)
      endif

! Temporary Code fix for CGILS SCM cases only. Set insolation and cos z to known values
!  (can be removed when the daily average sun code is working, ie, time value of 5)
         if(present(TIME)) then
          if(TIME.eq.10) then
               SLR = 0.3278
               ZTH = 0.6087247
               if(present(ZTHB)) ZTHB = ZTH
               if(present(ZTHD)) ZTHD = ZTH
               if(present(DIST)) DIST = ORBIT%PP(IDAY)
          elseif(TIME.eq.11) then
               SLR = 0.3449
               ZTH = 0.5962
               if(present(ZTHB)) ZTHB = ZTH
               if(present(ZTHD)) ZTHD = ZTH
               if(present(DIST)) DIST = ORBIT%PP(IDAY)
          elseif(TIME.eq.12) then
               SLR = 0.3461
               ZTH = 0.5884
               if(present(ZTHB)) ZTHB = ZTH
               if(present(ZTHD)) ZTHD = ZTH
               if(present(DIST)) DIST = ORBIT%PP(IDAY)
          endif
         endif

    RETURN_(ESMF_SUCCESS)

