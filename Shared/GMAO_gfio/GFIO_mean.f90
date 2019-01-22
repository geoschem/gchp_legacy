   Program GFIO_mean

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_mean --- Computing monthly means.
!
!
! !USAGE: see the routine usage() below
!
! !USES:
!
   Implicit NONE

! !DESCRIPTION: Computing fvdas monthly means.
!
! !REVISION HISTORY:
!
!  ??Jan2005  Govindaraju  Initial design.
!  25Jan2006  da Silva     Removed inVars(:) as it was not used and did not
!                          compile on Absoft.
!  24Mar2006  da Silva     Implemented linear combination of files.
!  15Dec2009  Redder       Bug fix by changing the array, field, to a 
!                          scalar (and the name to field_val) which also
!                          removed unnecessary use of memory.(Bug affected
!                          only the program efficiency, not the results)
!  14Oct2012  Todling      Fix for time increment when specified at command
!                          line (program was not listening to command line)
!-------------------------------------------------------------------------
!EOP


   character(len=*), parameter :: myname = 'GFIO_mean'

  
!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------

!     integer, parameter :: mFiles = 256       ! Max. number of input files
!     integer, parameter :: mVars  = 256       ! Max. number of variables
!     integer, parameter :: mLevs  = 256       ! Max. number of levels    

      integer, parameter :: mFiles = 1000      ! Max. number of input files
      integer, parameter :: mVars  = 500       ! Max. number of variables
      integer, parameter :: mLevs  = 300       ! Max. number of levels    


!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


      integer            :: nFiles             ! Actual number of input files
      character(len=255) :: inFiles(mFiles)    ! Input file names
      character(len=255) :: out_total_file     ! Output running total file name
      character(len=255) :: out_counter_file   ! Output running counter file name
      character(len=255) :: outFile            ! Output file name
      integer  :: inc_hhmmss                   ! increment hours specified from command line

      real              :: alpha(mFiles)   ! linear combination factors
      logical           :: linear_comb     ! whether doing linear combination
      logical           :: xswap           ! whether to swap lon-dimension: [0,360)->[-180,180)


      integer  :: im                           ! zonal dimension
      integer  :: jm                           ! meridional dimension
      integer  :: km                           ! vertical dimension

      integer  :: iflag                        ! Initial flag
      integer  :: irflag                       ! Monthly Mean flag


      real, pointer     :: lev(:)              ! levels in hPa (km)
 
      integer           :: nLevs = 0           ! total number of levels
      real, pointer     :: Levs(:)             ! vertical levels
      character(len=25) :: cLevs(mLevs)        ! Character reprsentation of levels

      integer           :: nVars               ! Actual number of variables
      character(len=64) :: outVars(mVars)      ! output variable names (nVars)
      character(len=64) :: outUnits(mVars)     ! Units of output variables (nVars)
    
      integer          :: outPrec              ! Output file precision:
                                               ! 0 = 32 bits,  1 = 64bits

!                              -----------------------
!                                Variable Work Space
!                              -----------------------

      real, pointer ::  inField(:,:,:)         ! Input variable
      real, pointer ::  write_out(:,:,:)       ! Input variable
      real, pointer :: outField(:,:,:)         ! Ouput variable
      real, pointer :: cntField(:,:,:)         ! Ouput variable

      real, pointer   :: total3d(:,:,:,:)          ! Monthly total for 3d
      real, pointer   :: total1d(:,:,:)          ! Monthly total for 1d
      real,pointer :: kount3d(:,:,:,:)        ! Kounter for 3d
      real,pointer :: kount1d(:,:,:)          ! Kounter for 1d

!                                  Local Work Space
!                              -----------------------

      integer iff, it, iv, itest, i, j, k
      real field_val


!                              -----------------------
!                                  Output Meta Data
!                              -----------------------

      character(len=255) :: title              ! meta data title
      character(len=255) :: source             ! data source
      character(len=255) :: contact            ! contact org.   
      character(len=255) :: levunits           ! Vertical levels
      character(len=25)  :: append             ! im*jm

      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            !
      integer          :: yyyymmdd_new          ! Date
      integer          :: hhmmss_new            !
      integer          :: ndate   ! Date
      integer          :: yyyymmdd1,yyyymmdd2  ! Date
      integer          :: yyyymmddp,hhmmssp    ! previous Date & time
      integer          :: yyyymmdd3,hhmmss3    ! previous Date & time
      integer          :: yyyymmddp1           ! previous Date + 1
      integer          :: hhmmss1              ! Time
      integer          :: ntime 
      integer          :: timinc,timinc_new    ! Time increment
      integer          :: timinc_save          ! Time increment

      integer          :: in_fmode = 1         ! non-zero for READ-ONLY
      integer          :: out_fmode = 0        ! 0 for READ-WRITE 
      integer          :: fid                  ! input file ID
      integer          :: out_fid              ! output file ID
      integer          :: fidt                 ! output running total file ID
      integer          :: fidc                 ! output running counter file ID
      integer          :: nkount
      integer          :: rc                   ! return error code

      character(len=255) :: vtitle(mVars)      ! output title
      character(len=255) :: vunits(mVars)      ! output title
      character(len=257) :: vName(mVars)       ! output variable names (nVars)
      integer            :: outKm(mVars)       ! number of levels for variables;
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)


!                              -----------------------
!                                  eta information 
!                              -----------------------

      integer           :: im_e                ! input zonal dimension       
      integer           :: jm_e                ! input meridional dimension       
      integer           :: km_e                ! input vertical dimension    
      integer           :: lm_e                ! input time dimension    
      integer           :: nVars_e             ! input number of variables   
      integer           :: buf(3)
      integer           :: iundef0
      real              :: undef               ! Missing value
      real              :: undef0              ! Missing value
      real, pointer     :: lon_e(:)            ! longitudes in deg (im)
      real, pointer     :: lat_e(:)            ! latitudes in deg (jm)
      real, pointer     :: lev_e(:)            ! levels in hPa (km)
      integer, pointer  :: kmVar_e(:)          ! Number of vertical levels for variables

      character(len=255) :: vtitle_in(mVars)   ! output title
      real              :: valid_range(2, mVars)
      real              :: packing_range(2, mVars)
      integer           :: ngatts              ! Number of attributes for GFIO
      logical              initial,file_exist,rms
!.................................................................................


   initial = .true.
   nkount = 0

!  Get user input
!  --------------
   call  Init_ ( mFiles, nFiles, inFiles, outFile,                            &
                 out_total_file,out_counter_file,iflag,irflag,                &
                 im, jm, km, nLevs, Levs,                                     &
                 mVars, nVars, outVars, outPrec, append,inc_hhmmss,           &
                 alpha, rms, linear_comb, xswap ) 

!  Need to check whether this will work if iflag /= 1; for now disable it
!  ----------------------------------------------------------------------
   if ( linear_comb.and. iflag /= 1 ) &
        call die ( myname, 'for now, linear combination mode only with iflag=1' )

!  --------------------------------------------------------------------------------
!      Set the totals and the counter initialization flag.
!      iflag   0     Starting date of the month, initialize the totals and counters.
!              1     Compute monthly means.
!              2     donot open the monthly mean dataset.
!              3     Compute monthly means using the accumulated totals and counters.

!      irflag  0     Continue saving the running totals.
!              1     Accumulate the totals using the previous accumulated totals
!                    and compute monthly means.
!              2     Donot accumulate the totals, compute the monthly means using
!                    the previously accumulated totals and coutners.
!              3     No more input data to be added.
!  --------------------------------------------------------------------------------

    if(iflag == 3 .and. irflag == 3) then

!      -----------------------------------------
!       Compute monthly means from the existing
!       totals and the counters.
!      -----------------------------------------

       call tot2mean(out_total_file,out_counter_file,outFile,outPrec,mVars)
       stop
    endif

!  -------------------------
!  Loop over input files ...
!  -------------------------
   do iff = 1, nFiles

!    Open GFIO file
!    --------------
     call GFIO_Open ( inFiles(iff), in_fmode, fid, rc )
     if ( rc /= 0 )  call die (myname, 'can not open input files')

!    Determine on file
!    ------------------
     call GFIO_DimInquire ( fid, im_e, jm_e, km_e, lm_e, nvars_e, ngatts, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')

!     -------------------------------------------------------------------
!     Allocate and initialize the 3d total and counters to accumulate the
!     monthly means.
!     -------------------------------------------------------------------

     if(iff == 1) then
      print *,' im_e,jm_e,km_e,nvars_e = ',im_e,jm_e,km_e,nvars_e
      allocate ( total3d(im_e,jm_e,km_e,nvars_e), total1d(im_e,jm_e,nvars_e),stat=rc )
      allocate ( kount3d(im_e,jm_e,km_e,nvars_e), kount1d(im_e,jm_e,nvars_e),stat=rc )
      total3d = 0.0
      total1d = 0.0
      kount3d = 0.0
      kount1d = 0.0
      initial = .false.
!
!    ------------------------------------------------------------
!      If the irflag is 1 or 2, read the running totals and counters
!      from the existing running total and counter files.
!    ------------------------------------------------------------

      if(iflag /= 1 .and. (irflag == 1 .or. irflag == 2)) then
       print *, ' Going in to get_rtotals *** '
       call get_rtotals(out_total_file,out_counter_file,buf,mVars,  &
                        yyyymmdd3,hhmmss3,total3d, kount3d,total1d, &
                        kount1d,fidt,fidc)
       print *, ' Out from  get_rtotals *** '
       yyyymmddp = buf(1)
       hhmmssp = buf(2)
       nkount = buf(3)
      endif
     endif

!    Allocate memory for meta data
     allocate ( yyyymmdd(lm_e),hhmmss(lm_e),lon_e(im_e),lat_e(jm_e),lev_e(km_e), &
                kmVar_e(mVars), lev(km_e), stat = rc )
     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon_e,lat_e,lev')

!    Get meta data
     call GFIO_Inquire ( fid, im_e, jm_e, km_e, lm_e, nVars_e,  &
                               title, source, contact, undef,   &
                               lon_e, lat_e, lev_e, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_e,  &
                               valid_range , packing_range, rc)

!    print *,' timinc ',timinc
     timinc_save = timinc

     if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire')

!    Find long names and number of levels for meta data
!    -------------------------------
     if (nVars .le. 0) then
        nVars = nVars_e
     end if

     lev = lev_e
     if (nLevs .le. 0) then 
!       print *, "setting level information."
        nLevs = km_e
        km = km_e
        allocate(Levs(nLevs), stat=rc)
        Levs = lev_e
     end if
        
     do iv = 1, nVars 
        if (nVars .eq. nVars_e) then
           outVars(iv) = vname(iv)
           outUnits(iv) = vunits(iv)
           vtitle_in(iv) = vtitle(iv)
           outKm(iv) = kmVar_e(iv)
           vtitle_in(iv) = vtitle(iv)
           valid_range_prs(1,iv) = valid_range(1,iv)
           packing_range_prs(1,iv) = packing_range(1,iv)
           valid_range_prs(2,iv) = valid_range(2,iv)
           packing_range_prs(2,iv) = packing_range(2,iv)
        else
           do itest = 1, nVars_e
              if ( outVars(iv) .eq. vname(itest) ) then
                 vtitle_in(iv) = vtitle(itest)
                 outUnits(iv) = vunits(itest)
                 vtitle_in(iv) = vtitle(itest)
                 outKm(iv) = kmVar_e(itest)
                 valid_range_prs(1,iv) = valid_range(1,itest)
                 packing_range_prs(1,iv) = packing_range(1,itest)
                 valid_range_prs(2,iv) = valid_range(2,itest)
                 packing_range_prs(2,iv) = packing_range(2,itest)
              end if
           end do
        end if
     end do


!     ------------------------------------------------------------------
!        Acquire the date and time only if the monthly mean flag is set.
!     ------------------------------------------------------------------

     if(initial) then
      yyyymmdd1    = yyyymmdd(1)
      hhmmss1    = 120000
      if(inc_hhmmss==999999) then
         timinc_new = 060000
      else
         timinc_new = inc_hhmmss
      endif
     endif

      if ( iff == 1 ) then
        if((iflag /= 1 .and. iflag /= 3) .and. (irflag == 1 .or. irflag == 2)) then
       
         yyyymmddp1 = yyyymmddp + 1
         if(yyyymmdd(1) /= yyyymmddp1 ) then
           print *,' Previous totaled date ==> ',yyyymmddp
           print *,' Current date ==> ',yyyymmdd(1)
           print *,' Current time ==> ',hhmmss(1)
           print *,' Previously stored time periods ==> ',nkount
 
           call die (myname, ' Check the continuation date and time ')
          endif
         endif

!        Create output GFIO file
!        -----------------------
         if ( nLevs .le. 0 )  then
            nLevs = km
            allocate(Levs(km), stat = rc)
            if ( rc /= 0 )  call die (myname, 'cannot allocate Levs')
            Levs = lev_e
         end if
!       print *, "Levs=",Levs
         


         yyyymmdd1    = yyyymmdd(1)
         if(inc_hhmmss==999999) then
            timinc_new = 060000
         else
            timinc_new = inc_hhmmss
         endif

      end if
!        yyyymmdd1    = yyyymmdd(1)
!        hhmmss1      = hhmmss(1)

      if(iff == nFiles) yyyymmdd2 = yyyymmdd(1)

!     Loop over times on file
!     -----------------------

       print *, ' Reading ',trim( inFiles(iff)),' lm_e ',lm_e, ', alpha = ', alpha(iff), linear_comb

      do it = 1, lm_e

         nkount = nkount + 1

!        Loop over variables
!        -------------------
         do iv = 1, nVars 
            !print *,' ',trim(outVars(iv))
      
!           Read variable from GFIO file
!           ----------------------------
            if ( outKm(iv) > 0 ) then             ! 3D file
!           Allocated memory 
               allocate ( inField(im_e,jm_e,km_e),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate inField  ')
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                                  im_e, jm_e, 1, km_e, inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D file')
            else                                       ! 2D file
               allocate ( inField(im_e, jm_e, 1),stat = rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate inField ')
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                                  im_e, jm_e, 0, 1, inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')
            end if


!           -------------------------------------------------
!           Accumulate the totals and the counters to compute
!           monthly means.
!           -------------------------------------------------
!            print *,' nLevs ==> ',nLevs,outKm(iv),trim(outVars(iv))

            undef0 = undef
            if(undef > 1.e+10) then
             undef0 = 1.e+10
            endif

            if(undef > 900.0 .and. undef < 10000.0) then
             iundef0 = int(undef)
             undef0  = float(iundef0) 
            endif

            if ( outKm(iv) > 0 ) then
               do k = 1, nLevs    
                 do  j = 1,jm_e
                  do  i = 1,im_e
!                  if(inField(i,j,k) < 1.e+10) then
!                  if(inField(i,j,k) < undef0) then
                   if( defined(inField(i,j,k),undef)) then
                    field_val = alpha(iff) * inField(i,j,k)
                    if(rms) then
                     field_val = field_val * field_val
                    endif
                    total3d(i,j,k,iv) = total3d(i,j,k,iv) + field_val
                    kount3d(i,j,k,iv) = kount3d(i,j,k,iv) + 1.0
                   endif
                  end do
                 end do
               end do
            else
    
!               print *, ' Computing totals for ',trim(outVars(iv)),' in ',trim(inFiles(iff))
                 do  j = 1,jm_e
                  do  i = 1,im_e
!                  if(inField(i,j,1) < 1.e+10) then
!                  if(inField(i,j,1) < undef0) then
                   if( defined(inField(i,j,1),undef)) then
                    field_val = alpha(iff) * inField(i,j,1)
                    if(rms) then
                     field_val = field_val * field_val
                    endif
                    total1d(i,j,iv) = total1d(i,j,iv) + field_val
                    kount1d(i,j,iv) = kount1d(i,j,iv) + 1.0
                   endif
                  end do
                 end do
            endif
            deallocate( inField)
         end do  ! variables
      end do ! times

!     Close input file
!     ----------------
      call GFIO_Close ( fid, rc )

   end do ! input files
!
     if ( inc_hhmmss .ne. 999999 ) then
         timinc_new = inc_hhmmss
     else
         timinc_new = timinc_save
     end if

    hhmmss1    = 120000
    if(iflag == 1 .or. iflag == 3 .or. irflag == 2 ) then
      if(yyyymmdd_new == 999999) then
        yyyymmdd_new = (yyyymmdd1+yyyymmdd2)/2
        hhmmss_new = 120000
        print *,' yyyymmdd_new, hhmmss_new ,timinc_new ==> ',yyyymmdd_new, hhmmss_new,timinc_new
      endif

    if((iflag == 3 .and. irflag == 2) ) then
        yyyymmdd_new = (yyyymmdd3+yyyymmdd2)/2
        hhmmss_new = 120000
        print *,' yyyymmdd_new, hhmmss_new ,timinc_new ==> ',yyyymmdd_new, hhmmss_new,timinc_new
      endif

       
         inquire(file=trim(outFile),exist=file_exist)
         if(file_exist) then
           call GFIO_Open ( outFile, out_fmode, out_fid, rc )
         else
           if(rms) then
              title = 'RMS'
           else
              title = 'MEAN'
           endif

           call GFIO_Create ( outFile, title, source, contact, undef,         &
                              im_e, jm_e, km_e, lon_e, lat_e, Levs, levunits, &
                              yyyymmdd_new,hhmmss_new,timinc_new,             &
                              nVars, outVars, vtitle_in, outUnits, outKm,     &
                              valid_range_prs, packing_range_prs, outPrec,    &
                              out_fid, rc )
         endif

!        print *, ' rc ==> ',rc
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')

         print *,' Computing the monthly means '
    elseif(iflag == 0) then
         print *,' GFIO_Create ',trim(out_total_file)
         if(ndate /= 999999) then
          yyyymmdd3 = ndate
         else
          yyyymmdd3 = yyyymmdd1
         endif
         if(ntime /= 999999) then
          hhmmss3   = ntime
         else
          hhmmss3   = hhmmss1
         endif
         print *,' yyyymmdd1,hhmmss1,timinc_new ',yyyymmdd3,hhmmss3,timinc_new
         call GFIO_Create ( out_total_file, title, source, contact, undef,  &
                            im_e, jm_e, km_e, lon_e, lat_e, Levs, levunits, &
                            yyyymmdd3,hhmmss3,timinc_new,                   &
                            nVars, outVars, vtitle_in, outUnits, outKm,     &
                            valid_range_prs, packing_range_prs, outPrec,    &
                            fidt, rc )
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')

         print *,'  GFIO_Create ',trim(out_counter_file)
         call GFIO_Create ( out_counter_file, title, source, contact, undef, &
                            im_e, jm_e, km_e, lon_e, lat_e, Levs, levunits,  &
                            yyyymmdd3,hhmmss3,timinc_new,                    &
                            nVars, outVars, vtitle_in, outUnits, outKm,      &
                            valid_range_prs, packing_range_prs, outPrec,     &
                            fidc, rc )
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')
         print *,' Creation is completed '
    endif


!   In linear combination mode, all counts reset to 1 as there is no 
!   normalization
!   -----------------------------------------------------------------
    if ( linear_comb ) then
         kount3d = 1.0
         kount1d = 1.0 
    endif

!        Loop over variables
!        -------------------

        if(irflag <= 1) then
          print *,' Adding the global attributes ', yyyymmdd(lm_e),hhmmss(lm_e),nkount
         buf(1) = yyyymmdd(lm_e)
         buf(2) = hhmmss(lm_e)
         buf(3) = nkount
         call GFIO_PutIntAtt ( fidt, 'yymmddp', 1, buf(1),outPrec, rc )
         call GFIO_PutIntAtt ( fidt, 'hhmmssp', 1, buf(2),outPrec, rc )
         call GFIO_PutIntAtt ( fidt, 'ntimep',  1, buf(3),outPrec, rc )
         call GFIO_PutIntAtt ( fidc, 'yymmddp', 1, buf(1),outPrec, rc )
         call GFIO_PutIntAtt ( fidc, 'hhmmssp', 1, buf(2),outPrec, rc )
         call GFIO_PutIntAtt ( fidc, 'ntimep',  1, buf(3),outPrec, rc )
        endif

        do iv = 1, nVars 
         if(ndate /= 999999) yyyymmdd_new = ndate
         if(ntime /= 999999) hhmmss_new = ntime
         if(iflag == 1 .or. irflag == 2) then
            if ( outKm(iv) > 0 ) then
              allocate (outField(im_e,jm_e,km_e),stat=rc )
              do k = 1, km_e 
               do j = 1,jm_e
                do i = 1,im_e
                 if(kount3d(i,j,k,iv) > 0) then
                  outField(i,j,k) = total3d(i,j,k,iv)/kount3d(i,j,k,iv)
                  if(rms) then
                   outField(i,j,k) = sqrt(outField(i,j,k))
                  endif
                 else
                  outField(i,j,k) = undef
                 endif
                end do
               end do
              end do

!           Write monthly mean variable to output file
!           -----------------------------------------           
               allocate ( write_out(im_e,jm_e,nLevs), stat = rc)
               if ( rc /= 0 )  call die (myname, 'cannot allocate write_out')

               write_out = outField
               call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd_new,hhmmss_new,  &
                                 im_e, jm_e, 1, nLevs, write_out, rc )
               if ( rc /= 0 )  call die (myname, 'can not write')
               deallocate(outField,write_out)
            else
               allocate ( outField(im_e, jm_e, 1),  stat = rc )
               do j = 1,jm_e
                do i = 1,im_e
                 if(kount1d(i,j,iv) > 0) then
                  outField(i,j,1) = total1d(i,j,iv)/kount1d(i,j,iv)
                  if(rms) then
                   outField(i,j,1) = sqrt(outField(i,j,1))
                  endif
                 else
                  outField(i,j,1) = undef
                 endif
                end do
               end do

               call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd_new,hhmmss_new,  &
                                 im_e, jm_e, 0, 1, outField, rc )
               if ( rc /= 0 )  call die(myname, 'can not write 2D file' )
               deallocate(outField)
            end if
           else
             if(ndate /= 999999) yyyymmdd3 = ndate
             if(ntime /= 999999) hhmmss3   = ntime
             if(irflag <= 1) then

              if ( outKm(iv) > 0 ) then
               allocate (outField(im_e,jm_e,km_e),stat=rc )
               allocate (cntField(im_e,jm_e,km_e),stat=rc )

               do k = 1, km_e 
                 do j = 1,jm_e
                  do i = 1,im_e
                     outField(i,j,k) = total3d(i,j,k,iv)
                     cntField(i,j,k) = kount3d(i,j,k,iv)
                  end do
                 end do
               end do
               call GFIO_PutVar (fidt,outVars(iv),yyyymmdd3,hhmmss3,  &
                                 im_e, jm_e, 1, nLevs, outField, rc )

               call GFIO_PutVar (fidc,outVars(iv),yyyymmdd3,hhmmss3,  &
                                 im_e, jm_e, 1, nLevs, cntField, rc )
              else
               allocate ( outField(im_e, jm_e, 1),  stat = rc )
               allocate ( cntField(im_e, jm_e, 1),  stat = rc )
               do j = 1,jm_e
                do i = 1,im_e
                  outField(i,j,1) = total1d(i,j,iv)
                  cntField(i,j,1) = kount1d(i,j,iv)
                end do
               end do
 
               call GFIO_PutVar (fidt,outVars(iv),yyyymmdd3,hhmmss3,  &
                                 im_e, jm_e, 0, 1, outField, rc )

               call GFIO_PutVar (fidc,outVars(iv),yyyymmdd3,hhmmss3,  &
                                 im_e, jm_e, 0, 1, cntField, rc )
               if ( rc /= 0 )  call die(myname, 'can not write 2D file' )
              endif
              deallocate(outField,cntField)

            endif
           endif
         end do  ! variables

      deallocate( total3d, total1d,kount3d,kount1d)
!     if ( outKm(iv) .gt. 0 ) deallocate( write_out )
!     deallocate( outField )


!  Close output file
!  ----------------
   if(iflag == 1 .or. irflag == 2) then
    call GFIO_Close ( out_fid, rc )
   endif

   if( iflag /= 1 .and. irflag <= 1) then
    call GFIO_Close ( fidt, rc )
    call GFIO_Close ( fidc, rc )
   endif


!  All done
!  --------
   call exit(0)

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_ --- Parses command line 
! 
! !INTERFACE:
!

   subroutine Init_ ( mFiles, nFiles, inFiles, outFile,                          &
                      out_total_file, out_counter_file,iflag,irflag,             &
                      im, jm, km, nLevs, Levs,                                   &
                      mVars, nVars, outVars, outPrec, append,inc_hhmmss, &
                      alpha, rms, linear_comb, xswap ) 

!
! !USES:
!
   Implicit NONE

!
! !INPUT PARAMETERS: 
!

      integer, intent(in)  :: mFiles           !  Max. number of input files as
                                               !   declared in calling program
      integer, intent(in)  :: mVars            !  Max. number of variables


!
! !OUTPUT PARAMETERS:
!

      integer, intent(out)          :: nFiles           !  Actual number of input files
      character(len=*), intent(out) :: inFiles(:)       !  Input file names
      character(len=*), intent(out) :: out_total_file   !  Input/output total files
      character(len=*), intent(out) :: out_counter_file !  Input/output counter file
      character(len=*), intent(out) :: outFile          !  Output file name 
      character(len=*), intent(out) :: append       !  im*jm
      integer  :: inc_hhmmss                        ! increment hours specified from command line


      integer, intent(out)  :: im              !  zonal dimension
      integer, intent(out)  :: jm              !  meridional dimension
      integer, intent(out)  :: km              !  vertical dimension
      integer, intent(out)  :: iflag           !  Initial flag
      integer, intent(out)  :: irflag          !  Running total flag.

      real, pointer         :: Levs(:)         ! actual levels
      integer, intent(out)  :: nLevs           ! actual number of levels
      

      integer,          intent(out) :: nVars        ! Actual number of variables
      character(len=*), intent(out) :: outVars(:)   ! output variable names (nVars)

      real,             intent(out) :: alpha(mFiles) ! coeficients for linear combination
      logical,          intent(out) :: linear_comb  ! whether or not doing linear combination
      logical,          intent(out) :: xswap  ! whether or not to swap lon-
                                              !  dimension; use this to change
                                              !  from [0,36) to [-180,180).

      logical,             intent(out) :: rms ! Flag for rms (true compute the rms
                                              ! false compute mean, default)

      integer, intent(out)          :: outPrec ! Output file precision:
                                               ! 0 = 32 bits,  1 = 6 4bits

! !DESCRIPTION: This routine initializes {\tt fvReduce}. It parses the command.
!
! !REVISION HISTORY: 
!
! Oct2002  Baoyu Yin Initial design and prologue.
!
!EOP
!-------------------------------------------------------------------------

   integer             iarg, argc
   integer :: iargc
   character(len=2048)  argv

   character(len=255)   Vars(mVars)

   integer, parameter :: mKm = 256  ! max no of levels

   integer i,  n, rc, ios
   logical :: debug = .false.
   character(len=10) nLx, nLy

print *
print *, "-------------------------------------------------------------------"
print *, "GFIO_mean - Compute monthly means.                                  "
print *, "-------------------------------------------------------------------"
print *

   argc = iargc()
   if ( argc < 1 ) call usage_()

!  Defaults
!  --------
   nFiles = 0
   nVars = 0
   outFile = 'GFIO_mean.prs.nc4'
   out_total_file = 'GFIO_total.prs.nc4'
   out_counter_file = 'GFIO_counter.prs.nc4'

   outPrec = 0
   km = -1
   im =  288 
   jm =  181 
   nLx = '288'
   nLy = '181'
   iflag  = 1
   irflag = 2
   rms    = .false.
   inc_hhmmss = 999999
   append = '288x181'
   yyyymmdd_new = 999999
   hhmmss_new = 999999
   ndate      = 999999
   ntime      = 999999

   iarg = 0
   do i = 1, 32767
      iarg = iarg + 1
      if ( iarg .gt. argc ) then
           exit
      endif
      call GetArg ( iArg, argv )
      if(index(argv,'-o') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, outFile )
      elseif(index(argv,'-tfile') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, out_total_file )
      elseif(index(argv,'-cfile') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, out_counter_file )
      else if(index(argv,'-inc') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) inc_hhmmss
      else if(index(argv,'-date') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) ndate
         yyyymmdd_new = ndate
      else if(index(argv,'-time') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) ntime
         hhmmss_new = ntime
      else if(index(argv,'-xswap') .gt. 0 ) then
           xswap = .true.
      else if(index(argv,'-rms') .gt. 0 ) then
           rms = .true.
      else if(index(argv,'-iflag') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) iflag
      else if(index(argv,'-irflag') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) irflag
      else if(index(argv,'-vars') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mVars, Vars, nVars )
      else if(index(argv,'-levels') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mLevs, cLevs, nLevs )
         allocate( Levs(nLevs), stat = rc)
         if ( rc /= 0 )  call die (myname, 'wrong in allocating nLevs')

         km = nLevs
         do k = 1, nLevs
            read(cLevs(k),*) Levs(k)
         enddo
      else if(index(argv,'-prec') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) outPrec
      else if(index(argv,'-d') .gt. 0 ) then
         debug = .true.
      else
         nFiles = nFiles + 1
         inFiles(nFiles) = argv
      end if

   end do

   if ( outPrec .eq. 32 ) outPrec = 0   
   if ( outPrec .eq. 64 ) outPrec = 1   

   if(iflag < 3 .and. irflag < 3) then
    if ( nFiles .eq. 0 ) call die (myname, 'no files specified')
   endif

!  Resolve variable classes
!  ------------------------
   do n = 1, nVars
       outVars(n) = Vars(n)
   end do 

!  Handle linear combination specification. This is specificed as part 
!  of the file name: Example"
!                   GFIO_mean:  1:filename1.nc4 -1:filename2.nc4 will
!  will subtract filename2 from filename2. The format for each file
!  name is "alpha:filename". If alpha is omitted it is assumed 1.
!  This utility will work in linear combination mode if at least one 
!  alpha is specfied
!......................................................................
   alpha = 1.0
   linear_comb = .false.
   xswap = .false.
   do n = 1, nFiles
      i = index(inFiles(n),',') 
      if ( i > 1 ) then
           read(inFiles(n)(1:i-1),*,iostat=ios) alpha(n)
           if ( ios /= 0 ) then
                print *, 'GFIO_mean: invalid alpha in '//trim(inFiles(n))
                call exit(1)
           else
               infiles(n) =  inFiles(n)(i+1:)
               linear_comb = .true.
           end if
      end if
   end do

   print *, 'Input   Files: ', nFiles
   if ( linear_comb ) then
      do i = 1, nFiles
         print *, "               ", trim(inFiles(i)), ', alpha = ', alpha(i)
      end do
   else
      do i = 1, nFiles
         print *, "               ", trim(inFiles(i)), alpha(i)
      end do
   end if

!   print *
!   print *, 'Output   File: ', trim(outFile), ', prec = ', outPrec
!   print *

   if ( nLevs .gt. 0 ) then
      write(*,'(a,i3,/(10x,6f10.2))') '        Levels: ', nLevs,Levs
   end if 
   print *
   if ( nVars .gt. 0 ) then
      write(*,'(a,i3,/(10x,6a10))') '     Variables: ', nVars, outVars(1:nVars)
   end if 

   end subroutine Init_


    subroutine Usage_()
   
print *, "NAME"
print *, "   GFIO_mean  Computes mean or linear combination of files"
print *
print *, "SYNOPYSIS"
print *, "   GFIO_mean  [options] input_fname(s)"
print *
print *, "OPTIONS"
print *
print *, "  -o         ofname    output file name (default: GFIO_mean.prs.nc4)"
print *, "  -tfile     tfname    output Running total file (default: GFIO_total.prs.nc4)"
print *, "  -cfile     cfname    output file name (default: GFIO_counter.prs.nc4)" 
print *, "  -prec n     precision: "
print *, "                    n=0 for 32 bits (default) "
print *, "                    n=1 for 64 bits"
print *, "  -vars      varn     actual variable names, e.g., -vars hght,uwnd"
print *, "                      (default: all variables in the input file)"
print *, "  -date      ndate    Date to be intialized for the monthly mean (if not given will be computed)."
print *, "  -time      ntime    Time to be intialized for the monthly mean (if not given will be computed)."
print *, "  -iflag     flag     Initial flag. 0  Starting point for the running total"
print *, "                                       Running total and counter files will be created. "
print *, "                                    1  Compute the monthly means  (DEFAULT).            "
print *, "                                    2  Donot open the monthly mean dataset.             "
print *, "                                    3  Compute monthly means using the accumulated      "
print *, "                                       totals and counters.                             "
print *
print *, "  -irflag    flag                   0  Continue to accumulate the running totals and    "
print *, "                                       counters.                                        "
print *, "                                    1   Accumulate the totals using the previously      "
print *, "                                        accumulated totals                              "
print *, "                                    2  Save the running totals or compute               "
print *, "                                       monthly means (DEFAULT).                         "
print *, "  -rms       flag                   true, Computes rms                                   "
print *, "                                    false, Computes mean (default)                       "
print * 
print *, "  -levels vertical levels (default: all levels in the input file)"
print *, "  -xswap  swap longitudinal dimension; useful to change from [0,360) to"
print *, "          -[180,180) or vice versa"
print *
print *, "DESCRIPTION"
print *, "  Computes FVDAS monthly means or accumulated totals and counters."
print *
print *, "    Start accumulate the totals and the counters from the starting date and time.  "
print *
print *, "   ex:  GFIO_mean.x -irflag 0 -iflag 0 -tfile totals.nc4 -cfile counters.nc4 input_files"
print * 
print *, "   Continue to accumulate the totals and counters using next set of data. "
print * 
print *, "   ex:  GFIO_mean.x -iflag 2 -irflag 1 -tfile totals.nc4 -cfile counters.nc4 input_files"
print *
print *, "   Compute the monthly means already accumulated totals and counters without any input files."
print * 
print *, "   ex:  GFIO_mean.x -iflag 3 -irflag 3 -o out_file -tfile totals.nc4 -cfile counters.nc4 input_files"
print *
print *, "   Compute monthly means using the previously accumulated totals and counters. "
print * 
print *, "   ex:  GFIO_mean.x -iflag 3 -irflag 2  -o out_file -tfile totals.nc4 -cfile counters.nc4 input_files"
print *
print *, "   Compute monthly means. "
print * 
print *, "   ex:  GFIO_mean.x -iflag 1 -irflag 2 -o output_file  input_files"
print * 
print *, "   Compute linear combination of files: result = 1.2*file1 - 3.2 * file2 + file3"
print * 
print *, "   ex:  GFIO_mean.x -o output_file  1.2,file1.nc4 -3.2,file2.nc4 file3.nc4"
print * 
print *, "   This feature requires iflag=1 for now. You need at least one occurence of"
print *, "   ',' in the file name to trigger the linear combination mode."


    call die ( myname, 'exiting' )

    end subroutine Usage_

!............................................................................

    subroutine split_ ( tok, str, mList, List, nList )
    implicit NONE
    character(len=1), intent(in)  :: tok  ! delimitter
    character(len=*), intent(in)  :: str  ! string
    integer,          intent(in)  :: mList
    character(len=*), intent(out) :: List(mList)
    integer,          intent(out) :: nList

    integer i, l, n, j

    i = 1
    l = len_trim(str)
    nList = 0
    do n = 1, mList
       j = index(trim(str(i:)),tok) - 1
       if ( j < 1 ) then
            nList = nList + 1
            List(nList) = str(i:)
            exit
       end if
       nList = nList + 1
       List(nList) = str(i:i+j-1)
       i = i + j + 1
       if ( i > l ) then
            exit
       endif
    end do

    end subroutine split_

!
      subroutine die(myname,string)
      character(len=*) myname
      character(len=*) string
!
      print *, ' --------------------------------'
      print *, '        ',myname
      print *, '   ',string
      print *, ' --------------------------------'
      call exit(1)
      return
      end subroutine die
!
     subroutine get_rtotals(out_total_file,out_counter_file,buf,mVars,    &
                            yyyymmdd3,hhmmss3,total3d, kount3d,total1d, &
                            kount1d,fidt,fidc)
                                                                                                                          
      character(len=255) :: title              ! meta data title
      character(len=255) :: source             ! data source
      character(len=255) :: contact            ! contact org.
      character(len=255) :: levunits           ! Vertical levels
      real               :: undef
                                                                                                                               
      character(len=*), intent(out) :: out_total_file   !  Input/output total files
      character(len=*), intent(out) :: out_counter_file !  Input/output counter file
      integer          :: fidt               ! Output running total file   ID
      integer          :: fidc               ! Output running counter file ID
      integer          :: out_fmode = 0      ! 0 for READ-WRITE
      integer          :: yyyymmdd3,hhmmss3  ! date & time
      integer          :: im_t,jm_t,km_t,lm_t,it
      integer          :: nvars_t,ngattst,rc,mVars
      integer          :: buf(3)
      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            ! time
      real, pointer     :: lon_t(:)            ! longitudes in deg (im)
      real, pointer     :: lat_t(:)            ! latitudes in deg (jm)
      real*8, pointer     :: lev_t(:)            ! levels in hPa (km)
      integer, pointer  :: kmVar_t(:)          ! Number of vertical levels for variables

      real, pointer   :: total3d(:,:,:,:)        ! Monthly total for 3d
      real, pointer   :: total1d(:,:,:)          ! Monthly total for 1d
      real, pointer   :: InField(:,:,:)          ! Monthly total for 1d
      real,pointer :: kount3d(:,:,:,:)        ! Kounter for 3d
      real,pointer :: kount1d(:,:,:)          ! Kounter for 1d
      character(len=255) :: vtitle(mVars)      ! output title
      character(len=255) :: vunits(mVars)      ! output title
      character(len=257) :: vName(mVars)       ! output variable names (nVars)
      integer            :: timinc
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)

       
!    Open GFIO running total and counter files
!    --------------
     call GFIO_Open ( out_total_file, out_fmode, fidt, rc )
     if ( rc /= 0 )  call die (myname, 'can not open total file')
       
     call GFIO_Open ( out_counter_file, out_fmode, fidc, rc )
     if ( rc /= 0 )  call die (myname, 'can not open counter file')
!    Determine on file
!    ------------------
     call GFIO_DimInquire ( fidt, im_t, jm_t, km_t, lm_t, nvars_t, ngattst, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')

       
!    Allocate memory for meta data

     allocate ( yyyymmdd(lm_t),hhmmss(lm_t),lon_t(im_t),lat_t(jm_t),lev_t(km_t), &
                kmVar_t(nVars_t), lev(km_t), stat = rc )
!    Get meta data
       
     call GFIO_Inquire ( fidt, im_t, jm_t, km_t, lm_t, nVars_t,  &
                               title, source, contact, undef,   &
                               lon_t, lat_t, lev_t, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_t,  &
                               valid_range_prs , packing_range_prs, rc)
       
     print *,' yyyymmdd, hhmmss, timinc ',yyyymmdd, hhmmss, timinc

     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon,lat,lev')
       
         it = 1

         call GFIO_GetIntAtt ( fidt, 'yymmddp', 1, buf(1), rc )
         call GFIO_GetIntAtt ( fidt, 'hhmmssp', 1, buf(2), rc )
         call GFIO_GetIntAtt ( fidt, 'ntimep',  1, buf(3), rc )
         call GFIO_GetIntAtt ( fidc, 'yymmddp', 1, buf(1), rc )
         call GFIO_GetIntAtt ( fidt, 'hhmmssp', 1, buf(2), rc )
         call GFIO_GetIntAtt ( fidt, 'ntimep',  1, buf(3), rc )
       
!        Loop over variables
!        -------------------
         do iv = 1, nVars_t
       
!           Read variable from GFIO file
       
            if ( kmVar_t(iv) > 0 ) then             ! 3D file
!           Allocated memory
               allocate ( inField(im_t,jm_t,km_t),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate  ')
       
               call GFIO_GetVar ( fidt, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 1, km_t, inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D total file')
               total3d(1:im_t,1:jm_t,1:km_t,iv) = inField(1:im_t,1:jm_t,1:km_t)
       
               call GFIO_GetVar ( fidc, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 1, km_t, inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D counter file')

               kount3d(1:im_t,1:jm_t,1:km_t,iv) = int(inField(1:im_t,1:jm_t,1:km_t))
            else                                       ! 2D file
               allocate ( inField(im_t, jm_t, 1),stat = rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate ')
               call GFIO_GetVar ( fidt, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 0, 1, inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')

               total1d(1:im_t,1:jm_t,iv) = inField(1:im_t,1:jm_t,1)
               call GFIO_GetVar ( fidc, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 0, 1, inField, rc )
               kount1d(1:im_t,1:jm_t,iv) = int(inField(1:im_t,1:jm_t,1))
            end if
          end do
          yyyymmdd3 = yyyymmdd(1)
          hhmmss3   = hhmmss(1)
       
          deallocate ( inField,stat=rc )
          deallocate ( yyyymmdd,hhmmss,lon_t,lat_t,lev_t, &
                       kmVar_t, lev)
!         call GFIO_Close ( fidt, rc )
!         call GFIO_Close ( fidc, rc )
        end subroutine  get_rtotals
!
     subroutine tot2mean(out_total_file,out_counter_file,outFile,outPrec,mVars)
                                                                                                                          
      character(len=255) :: title              ! meta data title
      character(len=255) :: source             ! data source
      character(len=255) :: contact            ! contact org.
      character(len=255) :: levunits           ! Vertical levels
                                                                                                                               
      character(len=*), intent(inout) :: outFile          !  Input/output total files
      character(len=*), intent(inout) :: out_total_file   !  Input/output total files
      character(len=*), intent(inout) :: out_counter_file !  Input/output counter file
      integer          :: out_fid              ! monthly mean output file ID
      integer          :: fidt               ! Output running total file   ID
      integer          :: fidc               ! Output running counter file ID
      integer          :: out_fmode = 0      ! 0 for READ-WRITE
      integer          :: im_t,jm_t,km_t,lm_t,it
      integer          :: nvars_t,ngattst,rc,mVars
      integer          :: buf(3)
      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            ! time
      integer          :: yyyymmdd_new          ! Date
      integer          :: hhmmss_new            ! time
      real, pointer     :: lon_t(:)            ! longitudes in deg (im)
      real, pointer     :: lat_t(:)            ! latitudes in deg (jm)
      real*8, pointer     :: lev_t(:)            ! levels in hPa (km)
      integer, pointer  :: kmVar_t(:)          ! Number of vertical levels for variables
      integer           :: timinc_new
      real, pointer   :: totField(:,:,:)       !  totals
      real, pointer   :: kntField(:,:,:)       ! Counters    
      real, pointer   :: outField(:,:,:)       ! Monthly 
      character(len=255) :: vtitle(mVars)      ! output title
      character(len=255) :: vunits(mVars)      ! output title
      character(len=257) :: vName(mVars)       ! output variable names (nVars)
      integer            :: timinc
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars),undef
      integer          :: outPrec              ! Output file precision:

       
!    Open GFIO running total and counter files
!    --------------
     call GFIO_Open ( out_total_file, out_fmode, fidt, rc )
     if ( rc /= 0 )  call die (myname, 'can not open total file')
       
     call GFIO_Open ( out_counter_file, out_fmode, fidc, rc )
     if ( rc /= 0 )  call die (myname, 'can not open counter file')
!    Determine on file
!    ------------------
     call GFIO_DimInquire ( fidt, im_t, jm_t, km_t, lm_t, nvars_t, ngattst, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')

!    Allocate memory for meta data

     allocate ( yyyymmdd(lm_t),hhmmss(lm_t),lon_t(im_t),lat_t(jm_t),lev_t(km_t), &
                kmVar_t(nVars_t), lev(km_t), stat = rc )
!    Get meta data
       
     call GFIO_Inquire ( fidt, im_t, jm_t, km_t, lm_t, nVars_t,  &
                               title, source, contact, undef,   &
                               lon_t, lat_t, lev_t, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_t,  &
                               valid_range_prs , packing_range_prs, rc)
       
     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon,lat,lev')

       
         it = 1

         call GFIO_GetIntAtt ( fidt, 'yymmddp', 1, buf(1), rc )
         call GFIO_GetIntAtt ( fidt, 'hhmmssp', 1, buf(2), rc )
         call GFIO_GetIntAtt ( fidt, 'ntimep',  1, buf(3), rc )
         call GFIO_GetIntAtt ( fidc, 'yymmddp', 1, buf(1), rc )
         call GFIO_GetIntAtt ( fidt, 'hhmmssp', 1, buf(2), rc )
         call GFIO_GetIntAtt ( fidt, 'ntimep',  1, buf(3), rc )

         yyyymmdd_new = (buf(1) + yyyymmdd(1))/2
         hhmmss_new = 120000
         timinc_new = timinc
         print *,' yyyymmdd,hhmmss ',yyyymmdd,hhmmss
         print *,'  yyyymmdd_new hhmmss_new timinc_new ', yyyymmdd_new,hhmmss_new,timinc_new
         
! 
!            ----------------------------------------------
!              Create the monthly mean output file.
!            ----------------------------------------------

         call GFIO_Create ( outFile, title, source, contact, undef,           &
                            im_t, jm_t, km_t, lon_t, lat_t, lev_t, levunits,  &
                            yyyymmdd_new,hhmmss_new,timinc_new,               &
                            nVars_t, vname, vtitle, vunits, kmVar_t,          &
                            valid_range_prs, packing_range_prs, outPrec,      &
                            out_fid, rc )
!        print *, ' rc ==> ',rc
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')

!        Loop over variables
!        -------------------
         do iv = 1, nVars_t
       
!           Read variable from GFIO file
       
            if ( kmVar_t(iv) > 0 ) then             ! 3D file
!           Allocated memory
               allocate ( totField(im_t,jm_t,km_t),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate  ')
               allocate ( kntField(im_t,jm_t,km_t),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate  ')
               allocate ( outField(im_t,jm_t,km_t),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate  ')
       
               call GFIO_GetVar ( fidt, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 1, km_t, totField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D total file')
       
               call GFIO_GetVar ( fidc, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 1, km_t, kntField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D counter file')
 
               print *,' Field ',vname(iv)
               do  k = 1,km_t
                do j = 1,jm_t
                 do  i = 1,im_t
                   if(kntField(i,j,k) > 0) then
                     outField(i,j,k) = totField(i,j,k)/kntField(i,j,k)
                   else
                     outField(i,j,k) = undef
                   endif
                 end do
                end do
               end do

               call GFIO_PutVar (out_fid,vname(iv),yyyymmdd_new,hhmmss_new,  &
                                 im_t, jm_t, 1, km_t, outField, rc )

            else                                       ! 2D file
               allocate ( totField(im_t,jm_t,1),stat=rc )
               allocate ( kntField(im_t,jm_t,1),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate ')
               call GFIO_GetVar ( fidt, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 0, 1, totField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')

               call GFIO_GetVar ( fidc, vname(iv), yyyymmdd(it), hhmmss(it), &
                                  im_t, jm_t, 0, 1, kntField, rc )

                print *,' Field ',vname(iv)
                do j = 1,jm_t
                 do  i = 1,im_t
                   if(kntField(i,j,k) > 0) then
                     outField(i,j,1) = totField(i,j,1)/kntField(i,j,1)
                   else
                     outField(i,j,1) = undef
                   endif
                 end do
                end do

               call GFIO_PutVar (out_fid,vname(iv),yyyymmdd_new,hhmmss_new,  &
                                 im_t, jm_t, 0, 1, outField, rc )
            end if
            deallocate(outField,totField,kntField)
          end do
       
          deallocate ( yyyymmdd,hhmmss,lon_t,lat_t,lev_t, &
                       kmVar_t, lev)
          call GFIO_Close ( out_fid, rc )
          call GFIO_Close ( fidt, rc )
          call GFIO_Close ( fidc, rc )
        end subroutine  tot2mean

        function defined ( q,undef )
        use m_fpe, only: isnan

        implicit none
        logical  defined

        real     q,undef
!
!         Check for NaNs
!
        if(isNan(q)) then
!        print *,' q: ',q,undef
         q = undef
        endif

        defined = abs(q-undef).gt.0.1*abs(undef)
        end function defined
end Program GFIO_mean
