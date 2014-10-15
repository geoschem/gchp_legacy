!
! to create a python usable module:
! f2py -c -m write_cmip_bin write_cmip_bin.f90 
! to create a signature file:
! f2py -h write_cmip_bin.pyf write_cmip_bin.f90
! .....................................................................
   SUBROUTINE write_bin( today_str,                                                      &
                         today_yr, today_mon, today_day,                                 &
                         tomrw_yr, tomrw_mon, tomrw_day,                                 &
                         SST, ICE)

   IMPLICIT NONE
      
   CHARACTER (LEN = 8), INTENT(IN)   :: today_str

   REAL,                INTENT(IN)   :: today_yr
   REAL,                INTENT(IN)   :: tomrw_yr
   REAL,                INTENT(IN)   :: today_mon
   REAL,                INTENT(IN)   :: tomrw_mon
   REAL,                INTENT(IN)   :: today_day
   REAL,                INTENT(IN)   :: tomrw_day

!  INTEGER,             INTENT(IN)   :: NLON
!  INTEGER,             INTENT(IN)   :: NLAT

   REAL,                INTENT(IN)   :: SST(1440, 720)
   REAL,                INTENT(IN)   :: ICE(1440, 720)

! LOCAL VARS
   REAL                  :: HEADER(14)
   CHARACTER (LEN = 40)  :: fileName_SST, fileName_ICE
!     ....................................................................
! Header info.  Start & end dates: format: YYYYMMDDHHMMSS; Hour,min,Sec are set to zero.

     HEADER(1)    = today_yr
     HEADER(2)    = today_mon
     HEADER(3)    = today_day
     HEADER(4:6)  = 0.0

     HEADER(7)     = tomrw_yr
     HEADER(8)     = tomrw_mon
     HEADER(9)     = tomrw_day
     HEADER(10:12) = 0.0
     HEADER(13)    = 1440.0
     HEADER(14)    = 720.0
!---------------------------------------------------------------------------
!    Write out binary files for GCM to read
     fileName_SST  = 'CMIP_clim_adj_sst_' // today_str //'.bin'
     fileName_ICE  = 'CMIP_clim_adj_ice_' // today_str//'.bin'

        OPEN (UNIT = 991, FILE = fileName_SST, FORM = 'unformatted', STATUS = 'new')
        OPEN (UNIT = 992, FILE = fileName_ICE, FORM = 'unformatted', STATUS = 'new')

        WRITE(991) HEADER
        WRITE(991) SST
        CLOSE(991)

        WRITE(992) HEADER
        WRITE(992) ICE
        CLOSE(992)
!---------------------------------------------------------------------------
    END SUBROUTINE write_bin
! .....................................................................
!
