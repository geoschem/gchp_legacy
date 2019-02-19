!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
!  !TITLE: Returns a new date/time from an initial date/time and offset
!
!  !AUTHORS: Rob Lucchesi
!
!  !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
!  !DATE: July 20, 1998   
!
!EOI
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: GetDate --- Returns a new date/time from an initial date/time 
!                       and offset
!
! !INTERFACE:
!

       subroutine GetDate (yyyymmdd_1,hhmmss_1,offset,
     .                     yyyymmdd_2,hhmmss_2,rc)

! 
! !USES:
!
       use, intrinsic :: iso_fortran_env, only: INT64

       implicit none

!
! !INPUT PARAMETERS:
!
       
       integer yyyymmdd_1               ! Initial date in YYYYYMMDD format
       integer hhmmss_1                 ! Initial time in HHMMSS format
       integer offset                   ! Offset to add (in seconds)

!
! !OUTPUT PARAMETERS:
!
       integer yyyymmdd_2               ! New date in YYYYMMDD format
       integer hhmmss_2                 ! New time in HHMMSS format
       integer rc                       ! Return code. (<0 = error)
!
! !DESCRIPTION:   This subroutine returns a new date and time in yyyymmdd
!                 and hhmmss format given and initial date, time, and
!                 offset in seconds.  The routine converts the input date
!                 and time to julian seconds, adds the offset, and converts
!                 back to yyyymmdd and hhmmss format.  This routine has been
!                 tested for Y2K compiance.
!
! !REVISION HISTORY:
!
!  1998.07.20  Lucchesi    Initial version.
!  2010.05.11  Lucchesi  Integer for julian seconds changed to 64-bit. StartDate
!                        constant no longer needed.
!
!EOP
!-------------------------------------------------------------------------

      integer julday

      integer year1,mon1,day1,hour1,min1,sec1
      integer year2,mon2,day2,hour2,min2,sec2
      integer(kind=INT64) julian1
      integer(kind=INT64) julsec, remainder

! Error checking.

      if (hhmmss_1 .lt. 0 .or. hhmmss_1 .ge. 240000 ) then
         rc=-1
         return
      endif

! Convert Date/Time strings to integer variables.

!ams       write (dateString, 200) yyyymmdd_1
!ams 200   format (I8)
!ams       read (dateString, 201) year1, mon1, day1
!ams 201   format (I4,2I2)
!ams       write (dateString, 202) hhmmss_1
!ams 202   format (I6)
!ams       read (dateString, 203) hour1, min1, sec1
!ams 203   format (3I2)

      call GFIO_parseIntTime ( yyyymmdd_1, year1, mon1, day1 )
      call GFIO_parseIntTime (   hhmmss_1, hour1, min1, sec1 )

! Get Julian Day and subtract off a constant (Julian days since 01/01/1850)
 
      julian1 = julday (mon1, day1, year1)
       
! Calculcate Julian seconds

      julsec = (julian1-1)*86400 + hour1*3600 + min1*60 + sec1

! Add offset and calculate new julian day.

      julsec = julsec + offset
      julian1 = INT(julsec/86400) + 1
      remainder = MOD(julsec,86400)
 
! Convert julian day to YYYYMMDD.

      call caldat (julian1, mon2, day2, year2)

! Calculate HHMMSS from the remainder.

      hour2 = INT(remainder/3600)
      remainder = MOD(remainder,3600)
      min2 = INT(remainder/60)
      sec2 = MOD(remainder,60)

! Build YYYYMMDD and HHMMSS variables.

      yyyymmdd_2 = year2*10000 + mon2*100 + day2
      hhmmss_2 = hour2*10000 + min2*100 + sec2

      rc = 0
      return
      end
