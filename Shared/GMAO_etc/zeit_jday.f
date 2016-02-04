!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: zeit_jday() --- Returns the Julian day
!
      integer function zeit_jday ( CalDate )
!
! !INPUT PARAMETERS:
      implicit NONE
      integer  CalDate  ! Calendar date in the format YYYYMMDD
                        !   where YYYY is the year, MM is the
                        !   month and DD is the day.  A negative
                        !   number implies that the year is B.C.

*     Other variables
*     ---------------
      integer     Year
      integer     Month
      integer     Day
      integer     iGreg  ! Gregorian Calendar adopted Oct 12, 1582
      parameter ( iGreg = 15 + 31 * ( 10 + 12 * 1582 ) )
      integer     JulDay
      integer     jy, jm, ja

      Year   =       CalDate / 10000
      Month  = mod ( CalDate,  10000 ) / 100
      Day    = mod ( CalDate,    100 )
 
*     Change year 0 to year 1
*     -----------------------
      if ( Year  .eq. 0 ) Year = 1

*     Account for the nonexisting year 0
*     ----------------------------------
      if ( Year  .lt. 0 ) Year = Year + 1

      if ( Month .gt. 2 ) then
         jy = Year
         jm = Month + 1

      else
         jy = Year  - 1
         jm = Month + 13

      endif

      JulDay = int ( 365.25  * jy )
     .       + int ( 30.6001 * jm )
     .       + Day + 1720995

*     Test whether to change to Gregorian Celendar
*     --------------------------------------------
      if ( Day + 31 * ( Month + 12 * Year ) .ge. iGreg) then
        ja     = int ( 0.01 * jy )
        Julday = JulDay + 2 - ja + int ( 0.25 * ja )

      endif

      zeit_jday = JulDay

      return
      end
