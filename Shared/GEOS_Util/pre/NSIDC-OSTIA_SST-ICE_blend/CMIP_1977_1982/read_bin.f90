!
! to create a python usable module:
! f2py -c -m <module_name> <file_name>.f90 
! to create a signature file:
! f2py -h <module_name>.pyf <file_name>.f90
! .....................................................................
   SUBROUTINE read_bin( fileName,                                                       &
                        nymd1, nhms1, nymd2, nhms2, NLON, NLAT, SST)
   IMPLICIT NONE

   CHARACTER (LEN = 200), INTENT(IN)  :: fileName

   INTEGER,             INTENT(OUT)   :: nymd1, nhms1
   INTEGER,             INTENT(OUT)   :: nymd2, nhms2

   INTEGER,             INTENT(OUT)   :: NLON
   INTEGER,             INTENT(OUT)   :: NLAT

   REAL,                INTENT(OUT)   :: SST(1440, 720)

! LOCAL VARS
   real    year1,month1,day1,hour1,min1,sec1
   real    year2,month2,day2,hour2,min2,sec2
   real    dum1,dum2
   integer rc
!     ....................................................................
      open (10,file=fileName,form='unformatted',access='sequential', STATUS = 'old')
!     ....................................................................
      rc = 0
      do while (rc.eq.0)
         read (10,iostat=rc)  year1,month1,day1,hour1,min1,sec1,                            &
                              year2,month2,day2,hour2,min2,sec2,dum1,dum2
         if( rc.eq.0 ) then
          nymd1 = nint( year1*10000 )  + nint (month1*100)  +  nint( day1 )
          nhms1 = nint( hour1*10000 )  + nint (  min1*100)  +  nint( sec1 )
          nymd2 = nint( year2*10000 )  + nint (month2*100)  +  nint( day2 )
          nhms2 = nint( hour2*10000 )  + nint (  min2*100)  +  nint( sec2 )
          NLON  = nint(dum1)
          NLAT  = nint(dum2)

          IF( NLON /= 1440) PRINT *, 'ERROR in LON dimension in file: ', fileName
          IF( NLAT /= 720)  PRINT *, 'ERROR in LAT dimension in file: ', fileName
          read (10,iostat=rc)  SST
         end if
      end do 
      close(10)

      print *, year1,month1,day1,hour1,min1,sec1 
      print *, year2,month2,day2,hour2,min2,sec2,dum1,dum2
      print *, 'rc = ', rc             
!---------------------------------------------------------------------------
    END SUBROUTINE read_bin
! .....................................................................
!

