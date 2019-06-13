!! #define __NCVERBOS__ NCVERBOS
#define __NCVERBOS__ 0

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  IdentifyDim - Identify a cooridate variable
!
! !DESCRIPTION: This function attempts to identify a coordiante variable
!               from the name or units of the variable.  It does so by 
!               attempting to match the units specified in the COARDS 
!               conventions or by checking the name against commonly used
!               names.
! !INTERFACE:
!
      integer function IdentifyDim (dimName, dimUnits)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      character*(*) dimName  ! Name of the coordinate variable
      character*(*) dimUnits ! Units of the coordinate variable
!
! !RETURN VALUES:
!
!     0 = X dimension (longitude)
!     1 = Y dimension (latitude)
!     2 = Z dimension (level)
!     3 = Time
!    -1 = Unable to determine dimension
!
! !REVISION HISTORY:
!
!  1998.12.22  Lucchesi       Initial coding.
!  1999.11.02  da Silva       Made LATS4D compatible,
!  2001.01.02  da Silva       Cimcurventing PGI bugs.
!
!EOP
!-------------------------------------------------------------------------

        if (TRIM(dimUnits) .EQ. "degrees_north" ) then
            IdentifyDim = 1
            return
         end if

        if (TRIM(dimUnits) .EQ. "hPa" ) then
            IdentifyDim = 2
            return
        end if

        if ( trim(dimName) .eq. "time" ) then 
            IdentifyDim = 3
            return
        end if


        if (TRIM(dimUnits) .EQ. "degrees_east" .OR.
     .       trim(dimName)  .eq. "longitude"    .OR.
     .       trim(dimName)  .eq. "lon"  ) then
            IdentifyDim = 0
        else if (TRIM(dimUnits) .EQ. "degrees_north" ) then
            IdentifyDim = 1
        else if (  trim(dimName)  .eq. "latitude"    .OR.
     .             trim(dimName)  .eq. "lat"  ) then
            IdentifyDim = 1
        else if (INDEX(dimName,"lev") .NE. 0 .OR.
     .           INDEX(dimName,"Height") .NE. 0) then
          IdentifyDim = 2
        else if (TRIM(dimUnits) .EQ. "mb" .OR.
     .           TRIM(dimUnits) .EQ. "millibar" .OR.
     .           TRIM(dimUnits) .EQ. "sigma_level" .OR.
     .           TRIM(dimUnits) .EQ. "hPa") then
          IdentifyDim = 2
        else if (INDEX(dimName,"TIME") .EQ. 0 .OR.
     .           INDEX(dimName,"TIME:EOSGRID") .EQ. 0 .OR.
     .           INDEX(dimName,"time") .EQ. 0 .OR.
     .           INDEX(dimName,"Time") .EQ. 0) then
          IdentifyDim = 3
        else
          IdentifyDim = -1
        endif

        return
        end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GetBegDateTime - Get begin date/time of file
!
! !DESCRIPTION: This routine returns the begin date/begin time on file.
!               For a native GFIO file, it simply returns the value of
!  global attributes begin_date/begin_time. If these do no exist then
!  it attempts to parse the COARDS compliant time unit.
!
! !INTERFACE:
!
      subroutine GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      integer fid      ! file ID
!
! !OUTPUT PARAMETERS:
!
      integer begDate  ! beginning date
      integer begTime  ! beginning time
      real*8  incSecsr8
      integer incSecs  ! time increment in secs
      integer rc       ! error return code
!
! !REVISION HISTORY:
!
!  1999.11.01  da Silva  Initial code.
!  1999.11.08  da Silva  Generic time coordinate variable (no name assumed).
!  2000.10.18  Lucchesi  Added ParseTimeUnits subroutine to handle a wider
!                        variety of Units formats.
!  2003.11.02  da Silva  On some lats4d file with 1 time, it was returning
!                        incSecs = 0; now it returns incSecs = 1, so no
!                        division by zero occurs later on.
!
!EOP
!-------------------------------------------------------------------------

      integer i, timeId, hour, min, sec, corner(1), err, timInc
      integer year, month, day
      character(len=MAXCHR) timeUnits, dimUnits

      character*(MAXCHR) varName, dimName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts, dimSize
      integer nDims, nvars, ngatts, dimId, IdentifyDim

!     Time conversion local variables
      real*4    rtime
      real*8    dtime
      integer*2 itime
      integer*4 ltime
      real*8   t1, t2

!     Start by determing the ID of the time coordinate variable
!     ---------------------------------------------------------
      timeId = -1
      call ncinq (fid, nDims, nvars, ngatts, dimId, rc)
      if (err("GetBegDateTime: ncinq failed",rc,-48) .NE. 0)return
      do i=1,nDims
        call ncdinq (fid, i, dimName, dimSize, rc)  
        if (err("GetBegDateTime: can't get dim info",rc,-41) .NE. 0) return
        dimId = ncvid (fid, dimName, rc)
        if (err("GetBegDateTime: ncvid failed",rc,-40) .NE. 0) return
        call ncagtc (fid, dimId, 'units', dimUnits, MAXCHR, rc)
        if (err("GetBegDateTime: could not get units for dimension",rc,-53)
     .       .NE. 0) return
        if ( IdentifyDim (dimName, dimUnits) .eq. 3 ) then
             timeId = dimId
             timeUnits = dimUnits
             exit
        end if
      end do

      if ( timeId .lt. 0 ) then
         rc = -43
         print *, "GetBegDateTime: could not find time coord"
         return
      end if

      call ncpopt(0)  ! tell netcdf to shut up

!     Try assuming this file has been written with GFIO
!     -------------------------------------------------
      call ncagt (fid, timeId, 'begin_date', begDate, rc)
      if ( rc .eq. 0 ) then
           call ncagt (fid, timeId, 'begin_time', begTime, rc)
      end if

!     Well, it must be a native GFIO file
!     -----------------------------------
      if ( rc .eq. 0 ) then
         call ncagt (fid, timeId, 'time_increment', timInc, rc)
         if (err("GetBegDateTime: missing time increment",rc,-44) .NE. 0) return
 !ams        write (strTmp,'(i6)') timinc
 !ams        read (strTmp,'(3I2)') hour, min, sec

         call GFIO_parseIntTime ( timinc, hour, min, sec )

         incSecsr8 = hour*3600 + min*60 + sec
         incSecs = max ( 1, int(incSecsr8) )
!ams     print *, 'begdate, begtime, incsecs: ',begdate, begtime, incsecs
         return                               ! all done.
      end if

!     If could not find begin_date/begin_time attributes
!     then this is not a native GFIO file. In this case
!     attempt to parse the COARDS compliant time units
!     --------------------------------------------------
!ams      call ncagtc (fid, timeId, 'units', timeUnits, MAXCHR, rc)
!ams      if (err("GetBegDateTime: missing time.units",rc,-44) .NE. 0) return
      i = index(timeUnits,'since')
      if ( i .le. 0 ) then
          if (err("GetBegDateTime: invalid time units",1,-44) .NE. 0) return
      endif

!     Call to ParseTimeUnits replaces an internal read, that made assumptions
!     about the format of the Time Units string that were not always true.  
!     (RL: 10/2000)

      call ParseTimeUnits ( timeUnits, year, month, day, hour, min, sec, rc )
      begDate = year*10000 + month*100 + day
      begTime = hour*10000 + min*100   + sec

!     Determine time increment.
!     -------------------------
      call ncvinq (fid, timeID, varName, type, nvDims, vDims, 
     &     nvAtts, rc)
      if (err("GetBegDateTime: error in time variable inquire",
     &    rc,-52) .NE. 0) return
     
      if ( type .eq. NCFLOAT )  then
           corner(1) = 1
           call ncvgt1(fid,timeID,corner,rtime,rc)
           t1 = int(rtime) 
           corner(1) = 2
           call ncvgt1(fid,timeID,corner,rtime,rc)
           t2 = int(rtime)
      else if ( type .eq. NCDOUBLE ) then
           corner(1) = 1
           call ncvgt1(fid,timeID,corner,dtime,rc)
           t1 = dtime
!ams       print *, t1, dtime, rc
           corner(1) = 2
           call ncvgt1(fid,timeID,corner,dtime,rc)
           t2 = dtime
!ams       print *, t2, dtime, rc
      else if ( type .eq. NCSHORT  ) then
           corner(1) = 1
           call ncvgt1(fid,timeID,corner,itime,rc)
           t1 = itime
           corner(1) = 2
           call ncvgt1(fid,timeID,corner,itime,rc)
           t2 = itime
      else if ( type .eq. NCLONG   ) then
           corner(1) = 1
           call ncvgt1(fid,timeID,corner,ltime,rc)
           t1 = ltime
           corner(1) = 2
           call ncvgt1(fid,timeID,corner,ltime,rc)
           t2 = ltime
      else
           if (err("GetBegDateTime: invalid time data type",
     &         1,-44) .NE. 0) return
      endif


!     Convert time increment to seconds if necessary
!     ----------------------------------------------
      incSecsr8 = t2 - t1
      if ( timeUnits(1:6) .eq.  'minute' ) then
           incSecsr8 = incSecsr8 * 60 
      else if ( timeUnits(1:4) .eq. 'hour'   ) then
           incSecsr8 = incSecsr8 * 60 * 60 
      else if ( timeUnits(1:3) .eq.  'day' ) then
           incSecsr8 = incSecsr8 * 60 * 60 * 24
      else
           if (err("GetBegDateTime: invalid time unit name",
     &         1,-44) .NE. 0) return
      endif

!dn      print *,timeUnits,incSecsr8
      incSecs = max ( 1, int(incSecsr8) )

!ams      print *, 'begdate, begtime, incsecs: ',begdate, begtime, incsecs

      rc = 0 ! all done
      call ncpopt(__NCVERBOS__)

      return
      end


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_GetMissing --- Return missing value
!
! !DESCRIPTION: Returns missing value on file
!
! !INTERFACE:

      real function GFIO_GetMissing ( fid, rc )

!
! !USES:
!
      Implicit NONE
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:

      integer fid      ! file id
      integer rc       ! Error code

!
! !REVISION HISTORY:
!
!  1999.11.01  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

      integer nDims, recdim, ngatts
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts
      character*(MAXCHR) vnameTemp
      integer i
      logical surfaceOnly
      integer attType, attLen
      integer allVars            ! all variables - includes dimension vars

      real*4 amiss_32
      integer err

! Get basic information from the file

      call ncpopt(0)

      call ncinq (fid,nDims,allVars,ngatts,recdim,rc)
      if (err("Inqure: ncinq failed",rc,-48) .NE. 0) return

      if (nDims .EQ. 3) then
        surfaceOnly = .TRUE.
      endif

      do i= 1, allVars
        call ncvinq (fid,i,vnameTemp,varType,nvDims,vDims,nvAtts,rc)
        if (err("GFIO_GetMissing: variable inquire error",rc,-52) .NE. 0) return
        if (nvDims .EQ. 1) then   ! coord variable
          cycle
        else                      ! noon-coord variable
           call ncagt (fid, i,'fmissing_value',amiss_32,rc)
           if (rc .NE. 0) then
               call ncainq (fid, i, 'missing_value', attType, attLen, rc)
              if (rc.eq.0 .and. attType .EQ. NCFLOAT) then
!DN 06/01/2012   call ncagt (fid, allVars, 'missing_value', amiss_32, rc)  BUG FIXED
                 call ncagt (fid, i, 'missing_value', amiss_32, rc)
                 if (err("GFIO_GetMissing: error getting missing value",rc,-53) 
     .                .NE. 0) return
              else
                    print *, 
     .              'GFIO_GetMissing: Cannot find missing value, assuming 1E+15'
                    amiss_32 = 1.0E+15
              end if
           endif
           exit    ! just check first non-ccordinate variable
        endif
      end do

      GFIO_GetMissing = amiss_32

      call ncpopt(__NCVERBOS__)

      rc = 0
      end

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  ParseTimeUnits -- Parse the COARDS time units string 
!
! !DESCRIPTION: This subroutine takes as input the COARDS metadata time 
!               units string and parses out the date included in the string.
!               This date typically represents the first time in a COARDS
!               HDF files.
! !INTERFACE:
!
      subroutine ParseTimeUnits ( TimeUnits, year, month, day, hour, min, sec, rc )
!
! !USES:
!
      implicit none
      include "netcdf.inc"
      include "gfio.h"
!
! !INPUT PARAMETERS:
!
      character*(MAXCHR) TimeUnits      ! Units metadata string from the Time coord var
!
! !OUTPUT PARAMETERS:
!
      integer        year               ! 4-digit year
      integer        month              ! month
      integer        day                ! day
      integer        hour               ! hour
      integer        min                ! minute
      integer        sec                ! second
      integer        rc                 ! return code
                                        !  0 = no error
                                        ! -1 = problem parsing string

! !REVISION HISTORY:
!
!  2000.10.18  Lucchesi       Initial coding.
!  2001.08.13  Lucchesi       Modified to better parse time:units string (needed for lats4d support)
!
!EOP
!-------------------------------------------------------------------------


      ! Local variables

      integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
      integer datepos(2)
      integer strlen
      integer firstdash, lastdash
      integer firstcolon, lastcolon
      integer lastspace

      strlen = LEN_TRIM (TimeUnits)
      
      firstdash = index(TimeUnits, '-')
      lastdash  = index(TimeUnits, '-', BACK=.TRUE.)
      !
      ! For GDS time:units are days since 1-1-1 00:00:0.0
      !
      datepos(1) = firstdash - 1
      datepos(2) = lastdash + 2 
      if ( TimeUnits(datepos(1):datepos(2)) .EQ. '1-1-1' ) then
         ! Take care of URL date
         year  = 1
         month = 1
         day   = 1 
         hour  = 0
         min   = 0
         sec   = 0
         rc = 0
         return
      endif
      if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
        rc = -1
        return
      endif
      
      ypos(2) = firstdash - 1
      mpos(1) = firstdash + 1
      ypos(1) = ypos(2) - 4

      mpos(2) = lastdash - 1
      dpos(1) = lastdash + 1
      dpos(2) = dpos(1) + 2

      read ( TimeUnits(ypos(1):ypos(2)), * ) year
      read ( TimeUnits(mpos(1):mpos(2)), * ) month
      read ( TimeUnits(dpos(1):dpos(2)), * ) day

      firstcolon = index(TimeUnits, ':')

      if (firstcolon .LE. 0) then
        
        ! If no colons, check for hour.

        ! The logic assumes that the timeunits has a terminating null beyond the hour
        ! if it does not we will add one so that it correctly parses the time
        if (TimeUnits(strlen:strlen) /= char(0)) then
           TimeUnits = trim(TimeUnits)//char(0)
           strlen=len_trim(TimeUnits)
        endif
        lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
        if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
          hpos(1) = lastspace+1
          hpos(2) = strlen-1
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          min  = 0
          sec  = 0
        else
!ams       Please avoid casual prints, this is BAD under MPI
!!!          print *, 'ParseTimeUnits: Assuming a starting time of 00z'
          hour = 0
          min  = 0
          sec  = 0
        endif

      else
        hpos(1) = firstcolon - 2
        hpos(2) = firstcolon - 1
        lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
        if ( lastcolon .EQ. firstcolon ) then
          mpos(1) = firstcolon + 1
          mpos(2) = firstcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          sec = 0
        else 
          mpos(1) = firstcolon + 1
          mpos(2) = lastcolon - 1
          spos(1) = lastcolon + 1
          spos(2) = lastcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          read (TimeUnits(spos(1):spos(2)), * ) sec
        endif
      endif
       
      rc = 0
      return
      end

      subroutine GFIO_parseIntTime ( hhmmss, hour, min, sec )      
      integer, intent(in)  :: hhmmss
      integer, intent(out) :: hour, min, sec 
      hour = hhmmss / 10000
      min  = mod(hhmmss,10000)/100
      sec  = mod(hhmmss,100)
      end
