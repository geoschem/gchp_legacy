!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ProgSig_Dim - Returns dimensions of GEOS-DAS prog.sig GFIO files
! 
! !INTERFACE:
!
    subroutine ProgSig_Dim ( fname, im, jm, km, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS: 
!
      character(len=*), intent(in) :: fname     ! Input file name

! !OUTPUT PARAMETERS:

      integer, intent(out)          :: im
      integer, intent(out)          :: jm
      integer, intent(out)          :: km

      integer, intent(out)          :: rc       ! Error code:
                                                ! 0 - all is well
                                                ! ...


! !DESCRIPTION: This routine returns dimensions of a prog.sieg filke.
!
! !REVISION HISTORY: 
!
!  02NOV1999  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

   integer, parameter :: READ_ONLY = 1

   integer :: fid, err, lm, nvars, ngatts

   rc = 0

   
!  Open the file
!  -------------
   call GFIO_Open ( fname, READ_ONLY, fid, err )
   if ( err .ne. 0 ) then
      rc = 1
      return
   end if

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
   if ( err .ne. 0 ) then
      rc = 2
      return
   end if   

!  Close GFIO file
!  ---------------
   call GFIO_close ( fid, err )

!  All done
!  --------
   rc = 0
   return

 end subroutine ProgSig_Dim

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ProgSig_Read --- Reads GEOS-DAS prog.sig GFIO files
! 
! !INTERFACE:
!
    subroutine ProgSig_Read ( fname, nymd, nhms,  &
                              im, jm, km, ptop, sige,  &
                              phis, ps, uwnd, vwnd, tmpu, sphu, rh, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS: 
!
      character(len=*), intent(in) :: fname      ! Input file name
      integer, intent(in)          :: nymd       ! year-month-day
      integer, intent(in)          :: nhms       ! hour-min-sec

      integer, intent(in)          :: im
      integer, intent(in)          :: jm
      integer, intent(in)          :: km

! !OUTPUT PARAMETERS:

      real,    intent(out)    :: ptop             ! in Pa
      real,    intent(out)    :: sige(km+1)

      real,    intent(out)    :: phis(im,jm)    
      real,    intent(out)    :: ps(im,jm)        ! in Pa
      real,    intent(out)    :: uwnd(im,jm,km)
      real,    intent(out)    :: vwnd(im,jm,km)
      real,    intent(out)    :: tmpu(im,jm,km)
      real,    intent(out)    :: sphu(im,jm,km)   ! in kg/kg
      real,    intent(out)    :: rh(im,jm,km)

      integer, intent(out)          :: rc       ! Error code:
                                                ! 0 - all is well
                                                ! ...


! !DESCRIPTION: This routine reads a prog.sig file from GEOS-DAS in HDF
!               format, created by GFIO. Use ProgSigDim for getting the
! memory allocation requirements.
!
! !REVISION HISTORY: 
!
!  02NOV1999  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

   integer, parameter :: READ_ONLY = 1

   integer :: fid, err 
   integer :: err1, err2, err3, err4, err5, err6, err7
   integer :: one, kp1

   rc = 0

!  Open the file
!  -------------
   call GFIO_Open ( fname, READ_ONLY, fid, err )
   if ( err .ne. 0 ) then
      rc = 1
      return
   end if

!  OK, just read the data in
!  -------------------------
   print *, 'Dimensions: ', im, jm, km
   call GFIO_GetVar ( fid, 'PHIS', nymd, nhms, im, jm, 0, 1,  phis, err1 )
   if ( err1 .eq. 0 ) then
      call GFIO_GetVar ( fid, 'PS',   nymd, nhms, im, jm, 0, 1,  ps,   err2 )
      call GFIO_GetVar ( fid, 'UWND', nymd, nhms, im, jm, 1, km, uwnd, err3 )
      call GFIO_GetVar ( fid, 'VWND', nymd, nhms, im, jm, 1, km, vwnd, err4 )
      call GFIO_GetVar ( fid, 'TMPU', nymd, nhms, im, jm, 1, km, tmpu, err5 )
      call GFIO_GetVar ( fid, 'SPHU', nymd, nhms, im, jm, 1, km, sphu, err6 )
      call GFIO_GetVar ( fid, 'RH',   nymd, nhms, im, jm, 1, km, rh,   err7 )
   else
      print *, 'progsig: attempting lower case variable names...'
      call GFIO_GetVar ( fid, 'phis', nymd, nhms, im, jm, 0, 1,  phis, err1 )
      call GFIO_GetVar ( fid, 'ps',   nymd, nhms, im, jm, 0, 1,  ps,   err2 )
      call GFIO_GetVar ( fid, 'uwnd', nymd, nhms, im, jm, 1, km, uwnd, err3 )
      call GFIO_GetVar ( fid, 'vwnd', nymd, nhms, im, jm, 1, km, vwnd, err4 )
      call GFIO_GetVar ( fid, 'tmpu', nymd, nhms, im, jm, 1, km, tmpu, err5 )
      call GFIO_GetVar ( fid, 'sphu', nymd, nhms, im, jm, 1, km, sphu, err6 )
      call GFIO_GetVar ( fid, 'rh',   nymd, nhms, im, jm, 1, km, rh,   err7 )
   endif

   if ( err1.ne.0 .or. err2.ne.0 .or. err3.ne.0 .or. err4.ne.0 .or. &
        err5.ne.0 .or. err6.ne.0 .or. err7.ne.0 ) then
        rc = 2
        return
   end if

!  Get attributes
!  --------------
   one = 1; kp1 = km+1
   call GFIO_GetRealAtt ( fid, 'PTOP',  one,  ptop, err1 )
   call GFIO_GetRealAtt ( fid, 'SIGE',  kp1, sige, err2 )

!  Try to recover in a very particular case
!  ----------------------------------------
   if ( err1.ne.0 .or. err2.ne.0 ) then

      if ( km .eq. 48 ) then

           print *
           print *, 'progsig: could not find PTOP/SIGE on file'
           print *, 'progsig: using hardwired values for km=48'
           print *

           ptop = 0.01
           sige(1:kp1) = (/ 0.0, 1.768e-05, 4.7500002e-05, 9.2000002e-05, &
           0.000155, 0.000245, 0.00036999999, 0.00053999998, 0.000765, &
           0.00106, 0.00144, 0.00192, 0.0025299999, 0.0033, 0.00428, &
           0.0055, 0.0070400001, 0.0089750001, 0.011405, 0.01448,&
           0.018379999, 0.023329999, 0.0296, 0.03754, 0.04761,&
           0.060350001, 0.076480001, 0.096900001, 0.12268, 0.155, &
           0.19464, 0.24195001, 0.29705, 0.35949999, 0.4278, 0.5,&
           0.57410002, 0.6476, 0.71820003, 0.78299999, 0.83899999,&
           0.884, 0.91949999, 0.94679999, 0.96710002, 0.98150003,&
           0.99119997, 0.99709511, 1.0 /) 

      else if ( km .eq. 70 ) then

           print *
           print *, 'progsig: could not find PTOP/SIGE on file'
           print *, 'progsig: using hardwired values for km=70'
           print *

           ptop = 0.01
           sige(1:kp1) = (/ &
             0.0,           6.0163502E-06, 1.3708200E-05, 2.2952399E-05, &
             3.4018401E-05, 4.8020898E-05, 6.5941298E-05, 8.8650100E-05, &
             1.1778500E-04, 1.5501599E-04, 2.0240400E-04, 2.6248299E-04, &
             3.3834801E-04, 4.3376599E-04, 5.5329700E-04, 7.0243899E-04, &
             8.8777998E-04, 1.1180050E-03, 1.4030660E-03, 1.7548969E-03, &
             2.1877419E-03, 2.7185469E-03, 3.3673861E-03, 4.1579502E-03, &
             5.1180860E-03, 6.2803938E-03, 7.6828832E-03, 9.3696872E-03, &
             1.1391840E-02, 1.3808100E-02, 1.6685840E-02, 2.0101970E-02, &
             2.4143871E-02, 2.8910421E-02, 3.4510002E-02, 4.1076310E-02, &
             4.8719998E-02, 5.7573721E-02, 6.7811079E-02, 7.9636462E-02, &
             9.3252078E-02, 0.1088781,     0.1267550,     0.1471300,     &
             0.1703000,     0.1965500,     0.2262500,     0.2595500,     &
             0.2962800,     0.3360700,     0.3784500,     0.4230300,     &
             0.4694000,     0.5170351,     0.5654188,     0.6138495,     &
             0.6615992,     0.7078959,     0.7519437,     0.7929700,     &
             0.8303000,     0.8635700,     0.8927438,     0.9179000,     &
             0.9392300,     0.9570000,     0.9715000,     0.9829000,     &
             0.9914000,     0.9970951,     1.0 /)

      else 

           rc = 3 
           return 

      end if

   end if


!  Fix units
!  ---------
   ptop = 100.0 * ptop
   ps   = 100.0 * ps
   sphu = sphu / 1000.

!  Close GFIO file
!  ---------------
   call GFIO_close ( fid, err )


!  All done
!  --------
   rc = 0
   return


 end subroutine ProgSig_Read

