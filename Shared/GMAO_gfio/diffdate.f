!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
!  !TITLE: Calculate difference in seconds between two times.
!
!  !AUTHORS: Rob Lucchesi
!
!  !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
!  !DATE: October 17, 1997
!
!EOI
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: DiffDate --- Calculates the number of seconds between two times.
!
! !INTERFACE:
!

       integer function DiffDate (yyyymmhh_1,hhmmss_1,yyyymmhh_2,
     .                             hhmmss_2)

! 
! !USES:
!

       implicit none

!
! !INPUT PARAMETERS:
!
       
       integer yyyymmhh_1               ! First date in YYYYYMMDD format
       integer hhmmss_1                 ! First time in HHMMSS format
       integer yyyymmhh_2               ! Second date in YYYYMMDD format
       integer hhmmss_2                 ! Second time in HHMMSS format

!
! !OUTPUT PARAMETERS:
!
!                 Integer function returns number of seconds between the
!                 the times given as input.  -1 is returned in the event 
!                 of an error.  
!
! !DESCRIPTION:   This function returns the number of seconds between two
!                 times.  Each time is specified with two integers, one 
!                 representing a date in the format YYYYMMDD and one 
!                 representing a time in the format HHMMSS.  This function 
!                 determines the Julian day of each date using the "julday"
!                 function from the book "Numerical Recipes in FORTRAN, the 
!                 art of scientific computing (2nd Ed.), by William H. Press, 
!                 Saul A. Teukolsky, William T. Vetterling, and Brian P. 
!                 Flannery (Cambridge University Press, 1992).  The difference 
!                 between the two times is then calculated and returned.  The 
!                 times need not be in chronological order as the function returns 
!                 the abs value.  -1 is returned in the event of an error.
!
! !REVISION HISTORY:
!
!  17Oct97   Lucchesi    Initial version.
!  2010.05.11 Lucchesi  Integer for julian seconds changed to 64-bit.  StartDate
!                        constant no longer needed.
!
!EOP
!-------------------------------------------------------------------------

       integer julday

       integer year1,mon1,day1,hour1,min1,sec1
       integer year2,mon2,day2,hour2,min2,sec2
!      integer julian1, julian2, julsec1, julsec2
       integer(kind=8) julian1, julian2, julsec1, julsec2

       character*8 dateString

! Error checking.

!      print *, 'HERE in diffdate ',yyyymmhh_1,yyyymmhh_2
!rl    if (yyyymmhh_1 .lt. 19000000 .or. yyyymmhh_1 .gt. 21000000 ) then
!rl      DiffDate=-1
!rl      return
!rl    endif
!rl    if (yyyymmhh_2 .lt. 19000000 .or. yyyymmhh_2 .gt. 21000000 ) then
!rl      DiffDate=-1
!rl      return
!rl    endif
       if (hhmmss_1 .lt. 0 .or. hhmmss_1 .ge. 240000 ) then
         DiffDate=-1
         return
       endif
       if (hhmmss_2 .lt. 0 .or. hhmmss_2 .ge. 240000 ) then
         DiffDate=-1
         return
       endif

! Convert Date/Time strings to integer variables.

       write (dateString, 200) yyyymmhh_1
200    format (I8)
       read (dateString, 201) year1, mon1, day1
201    format (I4,2I2)
       write (dateString, 200) yyyymmhh_2
       read (dateString, 201) year2, mon2, day2
       write (dateString, 202) hhmmss_1
202    format (I6)
       read (dateString, 203) hour1, min1, sec1
203    format (3I2)
       write (dateString, 202) hhmmss_2
       read (dateString, 203) hour2, min2, sec2

! Get Julian Days and subtract off a constant (Julian days since 7/14/66)
 
       julian1 = julday (mon1, day1, year1)
       julian2 = julday (mon2, day2, year2)
      
! Calculcate Julian seconds

       julsec1 = (julian1-1)*86400 + hour1*3600 + min1*60 + sec1
       julsec2 = (julian2-1)*86400 + hour2*3600 + min2*60 + sec2
       
!!!       DiffDate = iabs (julsec2 - julsec1)
       DiffDate = julsec2 - julsec1

       return
       end
