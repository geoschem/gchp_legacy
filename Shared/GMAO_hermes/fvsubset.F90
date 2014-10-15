   Program subset

!-------------------------------------------------------------------------
!         NASA/GSFC, Global Modeling and Assimilation Office (GMAO)      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  subset --- subsetting eta files. 
!
!
! !USAGE: see the routine usage() below
!
! !USES:
!
   use m_set_eta, only : set_eta
   use m_die
   use m_inpak90
   use m_chars

   Implicit NONE

! !DESCRIPTION: subsetting eta files.
!
! !REVISION HISTORY:
!
!  Oct2003  Baoyu Yin Initial design and prototyping.
!  Mar2004  Baoyu Yin Added -cvs option and -west option
!  02Nov2004 Todling  Removed dependency on FVGCM; getting set_eta from hermes
!-------------------------------------------------------------------------
!EOP


   character(len=*), parameter :: myname = 'subset'

  
!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------

      integer, parameter :: mFiles = 256       ! Max. number of input files
      integer, parameter :: mVars  = 256       ! Max. number of variables
      integer, parameter :: mLevs  = 256       ! Max. number of levels    


!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


      integer            :: nFiles             ! Actual number of input files
      character(len=255) :: inFiles(mFiles)    ! Input file names
      character(len=255) :: outFile            ! Output file name

      integer           :: km                  ! vertical dimension
      integer, pointer  :: levNums(:)          ! vertical level numbers
      integer           :: nLevs = 0           ! total number of levels
      real, pointer     :: Levs(:)             ! vertical levels
      character(len=25) :: cLevs(mLevs)        ! Character reprsentation of levels

      integer           :: nVars               ! Actual number of variables
      character(len=64) :: outVars(mVars)      ! output variable names (nVars)
      character(len=64) :: inVars(mVars)       ! input variable names (nVars)
      character(len=64) :: outUnits(mVars)     ! Units of output variables (nVars)
    
      integer           :: outPrec             ! Output file precision:
                                               ! 0 = 32 bits,  1 = 64bits

!                              -----------------------
!                                Variable Work Space
!                              -----------------------

      real, pointer ::  inField(:,:,:)         ! Input variable
      real, pointer ::  inTmp(:,:)             ! Input variable
      real, pointer ::  outTmp(:,:)            ! Output variable
      real, pointer :: outField(:,:,:)         ! Output variable

      integer       :: begDate      ! starting date of output
      integer       :: curDate      ! current date
      integer       :: begTime      ! starting time of output
      integer       :: incTime      ! time increment of output
      integer       :: curTime=0    ! current time
      integer       :: preTime      ! previous time
      integer       :: curDaye      ! current date
      logical       :: timeInterp

      integer iff                              ! file counter
      integer it                               ! time counter
      integer iv                               ! variable counter
      integer itest, ii, isp, i, j, k               
      integer          :: in_fmode = 1         ! non-zero for READ-ONLY
      integer          :: out_fmode = 0        ! 0 for READ-WRITE 
      integer          :: fid1                 ! input file ID
      integer          :: fid2                 ! input file ID
      integer          :: out_fid              ! output file ID
      integer          :: rc                   ! return error code
      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            !
      integer          :: timinc               ! Time increment


!                              -----------------------
!                                  Output Meta Data
!                              -----------------------

      character(len=255) :: title              ! meta data title
      character(len=255) :: title_cvs          ! meta data title with a CVS tag
      character(len=255) :: source             ! data source
      character(len=255) :: source_cvs         ! data source with a CVS tag
      character(len=255) :: contact            ! contact org.   
      character(len=255) :: levunits           ! Vertical levels
      character(len=255) :: cvsFile            ! CVS file name containing CVS version
      character(len=255) :: cvsV               ! CVS version
      integer            :: cvsV_yes=0
      real               :: missing_val
      character(len=255) :: vtitle(mVars)      ! output title
      character(len=255) :: vunits(mVars)      ! output title
      character(len=255) :: vName(mVars)       ! output variable names (nVars)
      integer            :: outKm(mVars)       ! number of levels for variables;
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)
      real, pointer     :: akOut(:)            ! output eta level parameter a
      real, pointer     :: bkOut(:)            ! output eta level parameter b


!                              -----------------------
!                                  eta information 
!                              -----------------------

      integer           :: im_e                ! input zonal dimension       
      integer           :: jm_e                ! input meridional dimension       
      integer           :: km_e                ! input vertical dimension    
      integer           :: lm_e                ! input time dimension    
      integer           :: nVars_e             ! input number of variables   
      real              :: amiss               ! Missing value
      real, pointer     :: lon_e(:)            ! longitudes in deg (im)
      real, pointer     :: lon_o(:)            ! output longitudes in deg (im)
      real, pointer     :: lat_e(:)            ! latitudes in deg (jm)
      real, pointer     :: lev_e(:)            ! levels in hPa (km)
      integer, pointer  :: kmVar_e(:)          ! Number of vertical levels for variables

      character(len=255) :: vtitle_in(mVars)   ! output title
      real              :: valid_range(2, mVars)
      real              :: packing_range(2, mVars)
      integer           :: ngatts              ! Number of attributes for GFIO

      real, pointer     :: ak(:)               ! model eta level parameter a
      real, pointer     :: bk(:)               ! model eta level parameter b
      integer           :: ks                  ! interface level (not needed)
      real              :: ptop                ! top pressure level 
      real              :: pint                ! interface pressure level 
      real              :: xWest               ! starting point for lon   
!.................................................................................

!  Get user input
!  --------------
   call  Init_ ( mFiles, nFiles, inFiles, outFile, cvsFile,          &
                 km, levNums, nLevs, Levs, mVars, nVars, outVars,    &
                 inVars, begDate, begTime, incTime, xWest, outPrec) 
   if (len(trim(cvsFile)) .ge. 1 ) then
      open (11, file=cvsFile, iostat=rc)
      if ( rc .eq. 0 )  then
         read(11, '(a)') cvsV
         cvsV_yes = 1
      end if
   end if

!  Loop over input files ...
!  -------------------------
   do iff = 1, nFiles

!    Open GFIO file
!    --------------
     call GFIO_Open ( inFiles(iff), in_fmode, fid1, rc )
     if ( rc /= 0 )  call die (myname, 'can not open input file '//inFiles(iff))
     if ( iff < nFiles ) then
        call GFIO_Open ( inFiles(iff+1), in_fmode, fid2, rc )
        if ( rc /= 0 )  call die (myname, 'can not open input file '//inFiles(iff+1) )
     else
        fid2 = fid1
     end if

!    Determine on file
!    ------------------
     call GFIO_DimInquire ( fid1, im_e, jm_e, km_e, lm_e, nvars_e, ngatts, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')

!    Allocate memory for meta data
     allocate ( yyyymmdd(lm_e),hhmmss(lm_e),lon_e(im_e),lat_e(jm_e),lev_e(km_e), &
                kmVar_e(mVars), stat = rc )
     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon,lat')

!    Get meta data
     call GFIO_Inquire ( fid1, im_e, jm_e, km_e, lm_e, nVars_e, &
                               title, source, contact, amiss,   &
                               lon_e, lat_e, lev_e, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_e,  &
                               valid_range , packing_range, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire')

!    Do time interpolation and selection.
     if (begDate .ge. yyyymmdd(lm_e) .and. curTime .gt. hhmmss(lm_e) +timinc) cycle

!    Find long names, number of levels and other meta data
!    -------------------------------
     if (nVars .le. 0) then   ! No variables selected -- all vars included
        nVars = nVars_e
        do i = 1, nVars
           outVars(i) = vname(i)
           inVars(i) = vname(i)
        end do
     end if

     if (nLevs .le. 0 .and. km .le. 0) then ! choose all levels
        nLevs = km_e
!        km = km_e
        allocate(Levs(nLevs), stat=rc)
        Levs = lev_e
     end if
     
     if ( km .gt. 0) then ! command line -level option overwrites rc file levels
        nLevs = km
        allocate(Levs(nLevs), stat=rc)
        do i = 1, km
           Levs(i) = lev_e(levNums(i))
        end do
     end if
     
        
!    Select variables from input file
     do iv = 1, nVars 
        do itest = 1, nVars_e
           if ( uppercase(inVars(iv)) .eq. uppercase(vname(itest)) ) then
              vtitle_in(iv) = vtitle(itest)
              outUnits(iv) = vunits(itest)
              outKm(iv) = kmVar_e(itest)
              if (outKm(iv) .gt. 0 .and. nLevs .gt. 0) outKm(iv) = nLevs
              valid_range_prs(1,iv) = valid_range(1,itest)
              packing_range_prs(1,iv) = packing_range(1,itest)
              valid_range_prs(2,iv) = valid_range(2,itest)
              packing_range_prs(2,iv) = packing_range(2,itest)
           end if
        end do
     end do


!    GFIO output file is created only once.  
      if ( iff == 1 ) then
!        Create output GFIO file
!        -----------------------
         if (begDate < 0 .or. begTime < 0 .or. incTime < 0) then ! no time interpolation
            begDate = yyyymmdd(1)
            begTime = hhmmss(1)
            incTime = timinc 
            timeInterp = .false.
         else
            timeInterp = .true.
         end if

         contact = 'data@gmao.gsfc.nasa.gov'
         source = 'Global Modeling and Assimilation Office'
         call getTag(cvsV, title, source, cvsV_yes, title_cvs, source_cvs)

         allocate(lon_o(im_e))
         do i = 1, im_e
            lon_o(i) = xWest + (i-1) * 360. / im_e
         end do

!  Create output file with all the required metadata
!  -------------------------------------------------
         call GFIO_Create ( outFile,title_cvs,source_cvs,contact,amiss,    &
                            im_e, jm_e, nLevs, lon_o, lat_e, Levs,         &
                            levunits, begDate, begTime, incTime,           &
                            nVars, outVars, vtitle_in, outUnits, outKm,    &
                            valid_range_prs, packing_range_prs, outPrec,   &
                            out_fid, rc )
         if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')

         allocate(ak(km_e+1), bk(km_e+1), stat=rc)
         call set_eta(km_e,ks,ptop,pint,ak,bk)

         curTime = begTime
         curDate = begDate
      end if

!     Loop over times on file
!     -----------------------
      do it = 1, lm_e
        
        
!      do time interpolation if needed.
       do

         if (.not. timeInterp .and. curDate .gt. yyyymmdd(it)) then
             exit
         endif
         if (timeInterp .and. curDate .gt. begDate) then
             exit
         endif   

!        Loop over variables
!        -------------------
         do iv = 1, nVars 
      
!           Read variable from GFIO file
!           ----------------------------
!           Allocated memory 
            allocate ( inField(im_e,jm_e,km_e), outField(im_e,jm_e,nLevs), &
                       inTmp(im_e,jm_e), outTmp(im_e,jm_e), stat=rc )
            if ( rc /= 0 )  call die (myname, 'can not allocate  ')

!           read 3D variable
            if (outKm(iv) .gt. 0) then
               call GFIO_GetVarT( fid1, inVars(iv), curDate, curTime, &
                               im_e, jm_e, 1, km_e, inField, rc, fid2 )
               if ( rc .ne. 0) call GFIO_GetVarT( fid2, inVars(iv), curDate, curTime, &
                               im_e, jm_e, 1, km_e, inField, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D file')
            else
!           read 2D variable
               call GFIO_GetVarT( fid1, inVars(iv), curDate, curTime, &
                               im_e, jm_e, 0, 1, inTmp, rc, fid2 )
               if ( rc .ne. 0) call GFIO_GetVarT( fid2, inVars(iv), curDate, curTime, &
                               im_e, jm_e, 0, 1, inTmp, rc, fid2 )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')
            end if
              

!           Interpolate from Input to Output grid
!           -------------------------------------
            if (xWest .gt. 0) call die (myname, 'xWest should be in [-180,0]')

            isp = abs(xWest*im_e/360)

!           Vertical level selections for 3D variables.
            if ( outKm(iv) > 0 ) then
               do k = 1, nLevs    
                  if (km .gt. 0) then
                     ii = levNums(k)
                     if ( lon_e(1) .lt. 0 .and. lon_o(1) .lt. 0 ) then 
                                                      ! [-180,180] -> [-180,180]
                        do i =1, im_e
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i,j,ii) 
                          end do
                        end do
                     else if ( lon_e(1) .lt. 0 ) then ! [-180,180] to [0,360]
                        do i =1, im_e/2
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i+im_e/2,j,ii) 
                          end do
                        end do
                        do i =im_e/2+1, im_e
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i-im_e/2,j,ii) 
                          end do
                        end do
                     else            ! [0,360] -> [0.360] or [-180,180]
                        do i = 1, isp 
                          do j = 1, jm_e
                             outField(i,j,k) = inField(im_e-isp+i,j,ii)
                          end do
                        end do
                        do i = isp+1, im_e 
                          do j = 1, jm_e
                             outField(i,j,k) = inField(i-isp,j,ii)
                          end do
                        end do
                     end if
                  else
                     ii = 1
                     do while ( abs(Levs(k)-lev_e(ii))/lev_e(ii) .gt. 0.01 )
                         ii = ii + 1
                         if ( ii .gt. km_e ) call die (myname, 'check -levels option')
                     end do
                     if ( lon_e(1) .lt. 0 .and. lon_o(1) .lt. 0 ) then
                                                      ! [-180,180] -> [-180,180]
                        do i =1, im_e
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i,j,ii)
                          end do
                        end do
                     else if ( lon_e(1) .lt. 0 ) then ! [-180,180] to [0,360]
                        do i =1, im_e/2
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i+im_e/2,j,ii)
                          end do
                        end do
                        do i =im_e/2+1, im_e
                          do j = 1, jm_e
                            outField(i,j,k) = inField(i-im_e/2,j,ii)
                          end do
                        end do
                     else      ! [0,360] -> [0.360] or [-180,180]
                        do i = 1, isp
                          do j = 1, jm_e
                             outField(i,j,k) = inField(im_e-isp+i,j,ii)
                          end do
                        end do
                        do i = isp+1, im_e
                          do j = 1, jm_e
                             outField(i,j,k) = inField(i-isp,j,ii)
                          end do
                        end do
                     end if
                  end if
                  
               end do

               if (iff .eq. 1 .and. it .eq. 1) then
                  allocate(akOut(nLevs+1), bkOut(nLevs+1), stat=rc)
                  if (km .gt. 0) then
                     do  k = 1, nLevs
                        akOut(k) = ak(levNums(k)) 
                        bkOut(k) = bk(levNums(k)) 
                     end do
                     akOut(nLevs+1) = ak(levNums(nLevs)+1) 
                     bkOut(nLevs+1) = bk(levNums(nLevs)+1) 
                  else
                     do  k = 1, nLevs
                        ii = 1
                        do while ( abs(Levs(k)-lev_e(ii))/lev_e(ii) .gt. 0.01 )
                            ii = ii + 1
                            if ( ii .gt. km_e ) call die (myname, 'check -levels option')
                        end do

                        akOut(k) = ak(ii) 
                        bkOut(k) = bk(ii) 
                     end do
                     akOut(nLevs+1) = ak(ii+1) 
                     bkOut(nLevs+1) = bk(ii+1) 
                  end if

                  call GFIO_PutRealAtt (out_fid, 'ptop', 1, ptop, outPrec, rc )
                  call GFIO_PutRealAtt (out_fid, 'ak', nLevs+1, akOut, outPrec, rc )
                  call GFIO_PutRealAtt (out_fid, 'bk', nLevs+1, bkOut, outPrec, rc )
               end if
            end if

!           Write interpolate variable to output file
!           -----------------------------------------           
!           write 3D variables
            if (outKm(iv) .gt. 0) then
               call GFIO_PutVar (out_fid,outVars(iv),curDate,curTime,  &
                              im_e, jm_e, 1, nLevs, outField, rc )
               if ( rc /= 0 )  call die (myname, 'can not write 3D var')
            else
!           write 2D variables
               if ( lon_e(1) .lt. 0 .and. lon_o(1) .lt. 0 ) then
                                                      ! [-180,180] -> [-180,180]
                  do i =1, im_e
                    do j = 1, jm_e
                      outTmp(i,j) = inTmp(i,j)
                    end do
                  end do
               else if ( lon_e(1) .lt. 0 ) then       ! [-180,180] to [0,360]
                  do i =1, im_e/2
                    do j = 1, jm_e
                      outTmp(i,j) = inTmp(i+im_e/2,j)
                    end do
                  end do
                  do i =im_e/2+1, im_e
                    do j = 1, jm_e
                      outTmp(i,j) = inTmp(i-im_e/2,j)
                    end do
                  end do
               else                  ! [0,360] -> [0.360] or [-180,180]
                  do i = 1, isp
                    do j = 1, jm_e
                       outTmp(i,j) = inTmp(im_e-isp+i,j)
                    end do
                  end do
                  do i = isp+1, im_e
                    do j = 1, jm_e
                       outTmp(i,j) = inTmp(i-isp,j)
                    end do
                  end do

               end if

               call GFIO_PutVar (out_fid,outVars(iv),curDate,curTime,  &
                              im_e, jm_e, 0, 1, outTmp, rc )
               if ( rc /= 0 )  call die (myname, 'can not write 2D var')
            end if

            deallocate( inField, outField, inTmp, outTmp )

         end do  ! variables

        preTime = curTime
        curTime = curTime + incTime

        if ( curTime .ge. 240000 ) then
           curTime = curTime - 240000
           curDate = incymd(curDate, 1)
           exit 
        end if
        if ( curTime .gt. mod(hhmmss(it)+timinc, 240000) ) then
             exit
        endif	   
        if ( .not. timeInterp .and. preTime .eq. hhmmss(it)) then
              exit
        endif

       end do ! end "do" while loop
      end do ! times


!     Close input file
!     ----------------
      call GFIO_Close ( fid1, rc )
      if ( iff .ne. nFiles) then
         call GFIO_Close ( fid2, rc )
      end if

   end do ! input files

!  Close output file
!  ----------------
   call GFIO_Close ( out_fid, rc )


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
   subroutine Init_ ( mFiles, nFiles, inFiles, outFile, cvsFile,         &
                      km, levNums, nLevs, Levs, mVars, nVars, outVars,   &
                      inVars, begDate, begTime, incTime, xWest, outPrec) 

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
      integer, intent(out)          :: nFiles       !  Actual number of input files
      character(len=*), intent(out) :: inFiles(:)   !  Input file names
      character(len=*), intent(out) :: outFile      !  Output file name 
      character(len=*), intent(out) :: cvsFile      !  containing CVS tag

      integer, intent(out)  :: km                ! vertical dimension
      integer, pointer      :: levNums(:)        ! vertical level numbers

      real, pointer         :: Levs(:)           ! actual levels
      integer, intent(out)  :: nLevs             ! actual number of levels
      
      integer,          intent(out) :: nVars        ! Actual number of variables
      character(len=*), intent(out) :: outVars(:)   ! output variable names (nVars)
      character(len=*), intent(out) :: inVars(:)    ! input variable names (nVars)

      integer,          intent(out) :: begDate      ! starting date of output
      integer,          intent(out) :: begTime      ! starting time of output
      integer,          intent(out) :: incTime      ! time increment of output

      real, intent(out)             :: xWest   ! output lon starting point
      integer, intent(out)          :: outPrec ! Output file precision:
                                               ! 0 = 32 bits,  1 = 6 4bits

! !DESCRIPTION: This routine initializes {\tt subset}. It parses the command.
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
   character(len=255)   rcfile, var, Vars(mVars)
   character(len=2048) label

   integer, parameter :: mKm = 256  ! max no of levels
   integer levels(mKm)

   integer i, j, n, p, nVars0, rc
   logical :: debug = .false.

print *
print *, "-------------------------------------------------------------------"
print *, "subset - subsetting eta files. "
print *, "-------------------------------------------------------------------"
print *

   argc = iargc()
   if ( argc < 1 ) call usage_()

!  Defaults
!  --------
   nFiles = 0
   nVars = 0
   outFile = 'subset.eta.nc4'
   rcfile = 'subset.rc'
   cvsFile = ' '
   outPrec = 0
   km = -1
   nLevs = 0
   begDate = -9999
   begTime = -9999
   incTIme = -9999
   xWest = -180.0

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
      else if(index(argv,'-begDate') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) begDate
      else if(index(argv,'-begTime') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) begTime
      else if(index(argv,'-incTime') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) incTime
      else if(index(argv,'-west') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) xWest
      else if(index(argv,'-rc') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, rcfile )
      else if(index(argv,'-cvs') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, cvsFile )
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

!         km = nLevs
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

!                  -------------------
!                  Parse Resource File
!                  -------------------

!  Load resource file
!  ------------------
   call i90_loadf ( rcfile, rc )
   if (rc .eq. 0 .and. km .lt. 0 .and. nLevs .eq. 0) then
      call i90_label ( 'vertical_level_numbers:', rc )
      if ( rc .eq. 0 ) then
         km = 0
         do i = 1, mKm
            p = i90_gint(rc)
            if ( rc .ne. 0 ) then
                 exit
            endif
            km = km + 1
            levels(km) = p
         end do
         allocate(levNums(km), stat = rc)
         do i = 1, km
            levNums(i) = levels(i)
         end do
      end if
   end if

!  Resolve variable classes
!  ------------------------
   nVars0 = nVars  ! number before expanding var classes
   nVars = 0       ! final number with var classes expanded
   do n = 1, nVars0

!     Simple variable name
!     --------------------
      if ( Vars(n)(1:1) .ne. '@' ) then
           nVars = nVars + 1
           outVars(nVars) = Vars(n)

!     Variable class
!     --------------
      else

          label = 'VarClass*'//trim(Vars(n)(2:))//':'
          call i90_label(trim(label),rc)
          if ( rc .ne. 0 ) then
             call die(myname,'cannot find variable class '//trim(vars(n)))
          end if

          j = nVars + 1
          do i = j, mVars
             call i90_gtoken ( var, rc )
             if ( rc .ne. 0 ) then
                  exit
             endif
             nVars = nVars + 1
             outVars(nVars) = var
          end do

      end if

   end do

!  Load Variable translation table
!  -------------------------------
   do n = 1, nVars

!     Defaults
!     --------
      inVars(n) = outVars(n)

!     Change defaults if entry exists on translation table
!     ----------------------------------------------------
      call i90_label ( uppercase(trim(outVars(n))), rc )
      if (rc .eq. 0) then
         call i90_gtoken ( inVars(n), rc )
         if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
      else
         call i90_label ( lowercase(trim(outVars(n))), rc )
         if (rc .eq. 0) then
            outVars(n) = lowercase(outVars(n))
            call i90_gtoken ( inVars(n), rc )
            if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
         else
            call i90_label ( trim(outVars(n)), rc )
            if (rc .eq. 0) then
               call i90_gtoken ( inVars(n), rc )
               if ( rc .ne. 0 ) call die(myname,'cannot get input vars' )
            end if
         end if
      end if

   end do

   if ( nFiles .eq. 0 ) call die (myname, 'no files specified')

!......................................................................


   print *, 'Input   Files: ', nFiles
   do i = 1, nFiles
      print *, "               ", trim(inFiles(i))
   end do
!   print *
!   print *, 'Output   File: ', trim(outFile), ', prec = ', outPrec
!   print *

   if ( nLevs .gt. 0 ) then
      write(*,'(a,i3,/(10x,6f10.2))') '        Levels: ', nLevs,Levs
   end if 
   if ( km .gt. 0 ) then
      write(*,'(a,i3,/(10x,6I10))') '        Level Numbers: ', km, levNums
   end if 
   print *
   if ( nVars .gt. 0 ) then
      print *, "     Output Variables:        Input Variables:"
      do n = 1, nVars
         write(*,'(9x,2a25)') outVars(n),  inVars(n)
      end do
   end if 

   end subroutine Init_


    subroutine Usage_()
   
print *, "NAME"
print *, "   subset  subsetting eta files."
print *
print *, "SYNOPYSIS"
print *, "   subset  [options] input_fname(s)"
print *
print *, "OPTIONS"
print *
print *, "  -o fname    output file name "
print *, "  -rc rcFile  resource file"
print *, "  -cvs cvsFile file with CVS tag"
print *, "  -west lon    starting point in output file"
print *, "  -prec n     precision: "
print *, "                    n=0 for 32 bits (default) "
print *, "                    n=1 for 64 bits"
print *, "  -vars varn  actual variable names, e.g., -vars hght,uwnd"
print *, "              or variable class, e.g., -vars @tavg3d_met_e"
print *, "  -levels vertical levels (default: all levels in the input file)"
print *, "  -begDate yyyymmdd   Starting date"
print *, "  -begTime hhmmss     Starting time"
print *, "  -incTime hhmmss     Time increment"
print *
print *, "DESCRIPTION"
print *, "  subsetting eta files."
print *

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


      function incymd (nymd,m)
      implicit none
      integer nymd,m
      integer ny,nm,nd,incymd,ny00
      integer ndpm(12)
      data    ndpm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      logical leap
      data    ny00     / 1900 /
      leap(ny) = mod(ny,4).eq.0 .and. (ny.ne.0 .or. mod(ny00,400).eq.0)
!
      ny = nymd / 10000
      nm = mod(nymd,10000) / 100
      nd = mod(nymd,100) + m
!
      if (nd.eq.0) then
      nm = nm - 1
      if (nm.eq.0) then
          nm = 12
          ny = ny - 1
      endif
      nd = ndpm(nm)
      if (nm.eq.2 .and. leap(ny))  nd = 29
      endif

      if (nd.eq.29 .and. nm.eq.2 .and. leap(ny))  go to 20

      if (nd.gt.ndpm(nm)) then
      nd = 1
      nm = nm + 1
      if (nm.gt.12) then
          nm = 1
          ny = ny + 1
      endif
      endif
!
   20 continue
      incymd = ny*10000 + nm*100 + nd
      return
      end function incymd

      subroutine getTag(cvsV,title,source,cvsV_yes,title_cvs,source_cvs)
         character(len=255), intent(in) :: cvsV         ! CVS version
         character(len=255), intent(in) :: title        ! meta data title
         character(len=255), intent(in) :: source       ! data source
         integer, intent(in)            :: cvsV_yes     ! data source
         character(len=255), intent(out) :: title_cvs   ! meta data title
         character(len=255), intent(out) :: source_cvs  ! data source
       
         integer :: t_len, ii

         t_len = 254
         do  ii= 254, 1, -1
            if (title(ii:ii) .eq. ' ' .or. ichar(title(ii:ii)) .le. 0) then
               t_len = t_len - 1
            else
               exit
            end if
         end do
         if ( cvsV_yes .eq. 1 ) then
            title_cvs = title(1:t_len) //'. '// cvsV(1:len(cvsV))
         else
            title_cvs = title(1:t_len)
         end if
                                                                                      
         t_len = 254
         do ii = 254, 1, -1
            if (source(ii:ii) .eq. ' ' .or. ichar(source(ii:ii)) .le. 0) then
               t_len = t_len - 1
            else
               exit
            end if
         end do
         if ( cvsV_yes .eq. 1 ) then
            source_cvs = source(1:t_len) //'. '// cvsV(1:len(cvsV))
         else
            source_cvs = source(1:t_len)
         end if

      end subroutine getTag
end Program subset
