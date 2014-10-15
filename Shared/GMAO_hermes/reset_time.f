      Implicit NONE

      INCLUDE 'netcdf.inc'
      INTEGER NCID, RCODE, fid
      INTEGER timeId
      CHARACTER*31 dimName        ! variable name
      INTEGER  dimSize
      INTEGER iarg, argc
      INTEGER date, hhmmss_beg, inc
      INTEGER fdate, fhhmmss_beg, finc
      character(len=256)  fileName
      character(len=256)  argv
      integer :: iargc
      integer hh, mm, ss
      character(len=2)  chh, cmm, css
      integer yyyy, mon, day
      character(len=4)  cyyyy
      character(len=2)  cmon, cday
      character*80 timeUnits 

      argc = iargc()
      if ( argc < 1 ) then 
         print *, "Usage: reset_time.x fname yyyymmdd hhmmss time_inc"
         print *, "   "

         print *, "Reset begin_date, begin_time and time_increment."
         print *, "Use negative number to skip. For example, "
         print *, "   "
         print *, "reset_time.x file_name -9 60000 -9 "
         print *, "will only modify begin_time"
         print *, "Please note: for HDF-EOS format, TIME:EOSGRID will"
         print *, "             be modified, but Time will be NOT."
         stop
      end if

      iarg = 1
      call GetArg ( iarg, fileName )
!      print * , "fileName: ", trim(fileName)
      iarg = iarg + 1
      call GetArg ( iarg, argv )
      read(argv,*) date
!      print * , "date: ", date
      iarg = iarg + 1
      call GetArg ( iarg, argv )
      read(argv,*) hhmmss_beg
!      print * , "hhmmss_beg: ", hhmmss_beg
      iarg = iarg + 1
      call GetArg ( iarg, argv )
      read(argv,*) inc
!      print * , "inc: ", inc

      call ncpopt(NCVERBOS)

!     open file
      fid = ncopn (fileName, NCWRITE, RCODE)
      timeId = ncvid (fid, 'time', RCODE)
      if (RCODE .ne. 0) timeId = ncdid (fid, 'time', RCODE)
      if (RCODE .ne. 0) timeId = ncvid (fid, 'TIME:EOSGRID', RCODE)
      if (RCODE .ne. 0) timeId = ncdid (fid, 'TIME:EOSGRID', RCODE)
      if (RCODE .ne. 0) then
         print *, "Can't get ID for time"
         stop
      end if
!      timeId = ncdid (fid, 'time', RCODE)

!     get time dimension
      call ncdinq (fid, timeId, dimName, dimSize, RCODE)
!     write begin_date if necessary
      if ( date .gt. 0) then
         call ncapt (fid,timeid,'begin_date',NCLONG,1,date,RCODE)
         if (RCODE .ne. 0) print *, "problem in setting begin_date"
      end if
!     read begin_date
      call ncagt (fid,timeid,'begin_date',fdate,RCODE)
!     write begin_time if necessary
      if ( hhmmss_beg .ge. 0) then
         call ncapt (fid,timeid,'begin_time',NCLONG,1,hhmmss_beg,RCODE)
         if (RCODE .ne. 0) print *, "problem in setting begin_time"
      end if
!     read begin_time
      call ncagt (fid,timeid,'begin_time',fhhmmss_beg,RCODE)

!     create time unit from begin_date and begin_time
      hh = fhhmmss_beg/10000
      mm = (fhhmmss_beg - hh*10000)/100 
      ss = fhhmmss_beg - hh*10000 - mm*100
      write(chh,'(I2.2)') hh
      write(cmm,'(I2.2)') mm
      write(css,'(I2.2)') ss
      yyyy = fdate/10000
      mon = (fdate-yyyy*10000)/100 
      day = fdate- yyyy*10000 - mon*100
      write(cyyyy,'(I4.4)') yyyy
      write(cmon,'(I2.2)') mon
      write(css,'(I2.2)') day
      write (timeUnits,202) yyyy,mon,day,hh,mm,ss
202   format ('minutes since ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',
     .         I2.2,':',I2.2)
!      print *, "timeUnits: ", trim(timeUnits)
      call ncaptc (fid, timeid, 'units', NCCHAR, len(TRIM(timeUnits)),
     .             timeUnits, RCODE)
      if (RCODE .ne. 0) print *, "problem in setting time uint"

!     write time_increment if necessary
      if ( inc .gt. 0) then
         call ncapt (fid,timeid,'time_increment',NCLONG,1,inc,RCODE)
         if (RCODE .ne. 0) print *, "problem in setting time_increment"
      end if
      call ncclos(fid, RCODE)
      end
