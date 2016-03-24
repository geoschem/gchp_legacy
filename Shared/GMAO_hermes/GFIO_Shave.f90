
          Program GFIO_Shave
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_Shave --- Shave nbits from a given GFIO file.
!
!
! !USAGE: see the routine usage() below
!
   use m_die

   Implicit NONE

! !DESCRIPTION: Shaves the given bits from a GFIO file.                     
!
! !REVISION HISTORY:
!
!   Oct2011  Govindaraju  Initial design and coding
!-------------------------------------------------------------------------
!EOP

   character(len=*), parameter :: myname = 'GFIO_Shave'

  
!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------


      integer, parameter :: mVars  = 300       ! Max. number of variables
      integer, parameter :: mLevs  = 300       ! Max. number of levels    


!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


      integer            :: nFiles             ! Actual number of input files
      character(len=256) :: inFile             ! Input file names
      character(len=256) :: outFile            ! Output file name
      integer  :: inc_hhmmss                   ! increment hours specified from command line

      integer  :: im                           ! zonal dimension
      integer  :: jm                           ! meridional dimension
      integer  :: km                           ! vertical dimension

      integer  :: nbits                        ! Shaving bits
      real, pointer     :: lon(:)              ! longitudes in deg (im)
      real, pointer     :: lat(:)              ! latitudes in deg (jm)
      real, pointer     :: lev(:)              ! levels in hPa (km)
 
      integer           :: nLevs = 0           ! total number of levels
      real, pointer     :: Levs(:)             ! vertical levels
      character(len=25) :: cLevs(mLevs)        ! Character reprsentation of levels

      integer           :: nVars               ! Actual number of variables
      integer           :: nVars0              ! Actual number of variables
      character(len=64) :: inVars(mVars)       ! Input  variable names (nVars)
      character(len=64) :: outVars(mVars)      ! output variable names (nVars)
      character(len=64) :: uvar                ! output variable names (nVars)
      character(len=64) :: outUnits(mVars)   ! Units of output variables (nVars)
      integer           :: nskip_Vars          ! Number of Variables to skip
      character(len=256) :: skipVars(mVars)     ! variable names (nVars) to skip
      

      character(len=64) :: uname               ! Uwind name

      integer          :: outPrec              ! Output file precision:
                                               ! 0 = 32 bits,  1 = 64bits

!                              -----------------------
!                                Variable Work Space
!                              -----------------------

      real, pointer ::  inField(:,:,:)         ! Input variable
      real, pointer ::  u(:,:,:)         ! Working array 
      real, pointer ::  v(:,:,:)         ! Working array 
      real, pointer ::  uofn(:,:,:)         ! Working array 
      real, pointer ::  vofn(:,:,:)         ! Working array 
      real, pointer :: outField(:,:,:)         ! Ouput variable
      real, pointer ::  ofn(:,:,:)              ! Ouput variable


!                                  Local Work Space
!                              -----------------------

      integer count, iff, it, iv, lm, itest, ii, i, j, k
      integer ig,is
      real dx,dx1, dx2,dy, dy1, dy2
      double precision pi
      logical ps_found,delp_found,ptop_found
      logical uv_flag,u_flag,v_flag


!                              -----------------------
!                                  Output Meta Data
!                              -----------------------

      character(len=256) :: title              ! meta data title
      character(len=256) :: source             ! data source
      character(len=256) :: contact            ! contact org.   
      character(len=256) :: levunits           ! Vertical levels
      real               :: missing_val

      integer, pointer :: yyyymmdd(:)          ! Date
      integer, pointer :: hhmmss(:)            !
      integer          :: ndate                ! Date
      integer          :: ndate_old            ! Date
      integer          :: yyyymmddp,hhmmssp    ! previous Date & time
      integer          :: ntimep               ! counter for total number of times previously accumulated.
      integer          :: ntime 
      integer          :: ntime_old 
      integer          :: timinc               ! Time increment
      integer          :: timinc_save          ! Time increment

      integer          :: in_fmode = 1         ! non-zero for READ-ONLY
      integer          :: out_fmode = 0        ! 0 for READ-WRITE 
      integer          :: fid                  ! input file ID
      integer          :: out_fid              ! output file ID
      integer          :: fidt                 ! output running total file ID
      integer          :: fidc                 ! output running counter file ID
      integer          :: rc, rc1,rc2,jq       ! return error code
      integer          :: i2,j2,i1,j1

      character(len=256) :: vtitle(mVars)      ! output title
      character(len=256) :: out_title(mVars)      ! output title
      character(len=256) :: vunits(mVars)      ! output title
      character(len=256) :: vname(mVars)       ! output variable names (nVars)
      integer            :: outKm(mVars)       ! number of levels for variables;
      real              :: valid_range_prs(2, mVars)
      real              :: packing_range_prs(2, mVars)
      real              :: ptop,pint,ptop32,pint32,ple,pint55,ptop55,pintOut,ksOut
      real              :: ptopOut
      real              :: rx,ry


!                              -----------------------
!                                  eta information 
!                              -----------------------

      integer           :: im_e                ! input zonal dimension       
      integer           :: jm_e                ! input meridional dimension       
      integer           :: km_e                ! input vertical dimension    
      integer           :: lm_e                ! input time dimension    
      integer           :: in_e                ! output zonal dimension       
      integer           :: jn_e                ! output meridional dimension       
      integer           :: kn_e                ! output vertical dimension    
      integer           :: in                  ! output zonal dimension       
      integer           :: jn                  ! output meridional dimension       

      integer           :: nVars_e             ! input number of variables   
      integer           :: kn            ! Output number of vertical levels
      integer           :: ks32,ks_e,ks55                  
      integer           :: buf(3)
      real              :: undef               ! Missing value
      real, pointer     :: lon_e(:)            ! longitudes in deg (im)
      real, pointer     :: lat_e(:)            ! latitudes in deg (jm)
      real, pointer     :: lat_e2(:)           ! latitudes in deg (jm)
      real, pointer     :: ak55(:)             ! vertical grid a coefficien
      real, pointer     :: bk55(:)             ! vertical grid a coefficien
      real, pointer     :: ak32(:)             ! vertical grid a coefficien
      real, pointer     :: bk32(:)             ! vertical grid a coefficien
      real, pointer     :: akOut(:)            ! Output vertical grid a coefficien
      real, pointer     :: bkOut(:)            ! Output vertical grid a coefficien
      real, pointer     :: dpref(:)            ! vertical grid a coefficien
      real, pointer     :: pe3d_m(:,:,:)       ! Input Pressure edges 
      real, pointer     :: gaus_ps(:,:)        ! working array          
      real, pointer     :: gaus_delp(:,:,:)        ! working array          
      real, pointer     :: gaus_inField(:,:,:) ! working array          
      real*8, pointer   :: lev_e(:)            ! levels in eta (km)
      integer, pointer  :: kmVar_e(:)          ! Number of vertical levels for variables
      integer, pointer  :: kmVar_e2(:)         ! Number of vertical levels for variables

      character(len=256) :: vtitle_in(mVars)   ! output title
      real              :: valid_range(2, mVars)
      real              :: packing_range(2, mVars)
      real              :: p,bkh
      integer           :: km_e1,kn1,iz,na,n
      integer           :: ngatts              ! Number of attributes for GFIO
      integer           :: imin,jmin,xmin,imax,jmax,xmax,gaussian
      logical           :: initial,file_exist,hintrp,zintrp
      logical           :: qaflag,ptflag,udflag,vdflag,ucflag,vcflag
      logical           :: sphflag,shftlon,lon_shift,noshave,skip
!.................................................................................


    initial = .true.
    hintrp = .false.

!  Get user input
!  --------------

   call  Init_ ( inFile, outFile,noshave,nbits,nskip_vars,skipVars,outPrec)

!    Open GFIO file
!    --------------
     call GFIO_Open ( inFile, in_fmode, fid, rc )
     if ( rc /= 0 )  call die (myname, 'can not open input files')

!    Determine on file
!    ------------------
     call GFIO_DimInquire ( fid, im_e, jm_e, km_e, lm_e, nvars_e, ngatts, rc)
     if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')
       print *,' im_e,im_e,km_e ',im_e,jm_e,km_e


!    Allocate memory for meta data

     if(associated(lev))       deallocate(lev)
     if(associated(kmVar_e))   deallocate(kmVar_e)
     if(associated(kmVar_e2))   deallocate(kmVar_e2)
     if(associated(lev_e))     deallocate(lev_e)
     if(associated(lat_e))     deallocate(lat_e)
     if(associated(lon_e))     deallocate(lon_e)
     if(associated(hhmmss))    deallocate(hhmmss)
     if(associated(yyyymmdd))  deallocate(yyyymmdd)

     allocate ( yyyymmdd(lm_e),hhmmss(lm_e),lon_e(im_e),lat_e(jm_e),lev_e(km_e), &
                kmVar_e(mVars),kmVar_e2(mVars), lev(km_e), stat = rc )
     if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd,hhmmss,lon_e,lat_e,lev')

!    Get meta data
     call GFIO_Inquire ( fid, im_e, jm_e, km_e, lm_e, nVars_e,  &
                               title, source, contact, undef,   &
                               lon_e, lat_e, lev_e, levunits,   &
                               yyyymmdd, hhmmss, timinc,        &
                               vname, vtitle, vunits, kmVar_e,  &
                               valid_range , packing_range, rc)

     print *,"nVars_e: ",nVars_e
     if(nskip_vars > 0 ) then
      nVars0 = 0
      do n = 1,nVars_e
       skip = .false.
       do na = 1,nskip_vars
        if(trim(vname(n)) == trim(skipVars(na))) then 
         skip = .true.
        endif
       end do

       if( .not. skip) then
        nVars0 = nVars0 + 1
        print *,trim(vname(n)), nVars0
        kmVar_e2(nVars0) = kmVar_e(n)
        outVars(nvars0) = vname(n)
        out_title(nvars0) = vtitle(n)
        outUnits(nvars0)  = vunits(n)
       endif
      end do
     else
      nvars0 = nVars_e
      do n = 1,nVars_e
        kmVar_e2(n) = kmVar_e(n)
        outVars(n) = vname(n)
        out_title(n) = vtitle(n)
        outUnits(n)  = vunits(n)
      end do
     endif
     nVars_e = nvars0

     print *,' nskip_vars,nVars_e,nVars0: ',nskip_vars,nVars_e,nVars0
     


     if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire')

           call GFIO_Create ( outFile, title, source, contact, undef,         &
                              im_e, jm_e, km_e, lon_e, lat_e, Lev_e, levunits,&
                              yyyymmdd,hhmmss,timinc,                         &
                              nVars_e, outVars, out_title, outUnits,          &
                              kmVar_e2,valid_range,packing_range,outPrec,      &
                              out_fid, rc )


           if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')



!     Loop over times on file
!     -----------------------

      do it = 1,lm_e
       print *, ' [] Reading ',trim( inFile)//' at ', &
                    yyyymmdd(it), hhmmss(it) 



!        Loop over variables
!        -------------------
         do iv = 1, nVars_e 
      
!           print *,' iv,kmVar_e2,vname ',iv,kmVar_e2(iv),trim(outVars(iv))

!           Read variable from GFIO file
!           ----------------------------
            if ( kmVar_e2(iv) > 0 ) then             ! 3D file

!            Allocated memory 

               if(associated(gaus_inField)) deallocate(gaus_inField)
               allocate (gaus_inField(im_e,jm_e,km_e),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate gaus_inField 3D')

!!!               print *,' jm_e ',jm_e
               
               print *,' GFIO_GetVar:  yyyymmdd,hhmmss ',yyyymmdd(it),hhmmss(it),trim(outVars(iv))
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                                  im_e, jm_e, 1, km_e,gaus_inField, rc )

               if ( .not. noshave ) then
                 print *,' Shaving ',trim(outVars(iv))
                 call Shave_field(gaus_inField,im_e,jm_e,km_e,nbits,undef=undef)
               endif
                    
               call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                                 im_e, jm_e, 1, km_e, gaus_inField,rc )

               if ( rc /= 0 ) call die (myname, 'something wrong in GFIO_PutVarT for 3D file')
            else                                       ! 2D file
               if(associated(gaus_inField)) deallocate(gaus_inField)
               allocate ( gaus_inField(im_e,jm_e,1),stat = rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate gaus_inField 2D')
               
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                                  im_e, jm_e, 0, 1, gaus_inField, rc )

               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')

               if ( .not. noshave ) then
                 call Shave_field(gaus_inField,im_e,jm_e,1,nbits,undef=undef)
               endif
               call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                                 im_e, jm_e, 0, 1,gaus_inField, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutVarT for 2D file')
            endif
              
            if(associated(gaus_inField))  deallocate(gaus_inField)

         end do  ! variables

          if(associated(lon))  deallocate(lon,stat=rc)
          if ( rc /= 0 )  call die (myname, 'something wrong in deallocate lon')
          if(associated(lat))  deallocate(lat,stat=rc)
          if ( rc /= 0 )  call die (myname, 'something wrong in deallocate lat')

      end do     ! times

         deallocate ( yyyymmdd,hhmmss,lon_e,lat_e,lev_e, &
                      kmVar_e, lev)

!     Close input file
!     ----------------


    call GFIO_Close ( fid, rc )
    if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_Close fid')
    
    call GFIO_Close ( out_fid, rc )
    if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_Close out_fid')

!  All done
!  --------
   call exit(0)

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_ --- Parses command line 
! 
! !INTERFACE:
!

   subroutine Init_ ( inFile, outFile,noshave,nbits,nskip_vars,skipVars,outPrec)

!
! !USES:
!
   Implicit NONE

!
! !INPUT PARAMETERS: 
!



!
! !OUTPUT PARAMETERS:
!

      character(len=*), intent(out) :: inFile           !  Input file names
      character(len=*), intent(out) :: outFile          !  Output file name 
      character(len=*), intent(out) :: skipVars(:)     !  Vars to skip
      integer  :: inc_hhmmss                        ! increment hours specified from command line


      integer, intent(out)          :: outPrec    ! Output file precision:
      integer, intent(out)          :: nbits   ! number of bits to shave
      integer, intent(out)          :: nskip_vars  
                                               ! 0 = 32 bits,  1 = 64 bits
      logical                       :: noshave ! Flag to shave number of bits.
                                               ! default  .false.
                                               ! to (0,360)

! !DESCRIPTION: This routine initializes and parses the command.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
!-------------------------------------------------------------------------

   integer              iarg, argc

   character(len=2048)  argv

   character(len=256)   rcfile, label, var, Vars(mVars)

   integer, parameter :: mKm = 256  ! max no of levels

   integer i, j, n, nVars0, rc, ios
   real    xWest, p
   logical :: debug = .false.
   character(len=10) nLx, nLy
   character (len=1) resolution


   argc = command_argument_count()
   if ( argc < 1 ) call usage_()

!  Defaults
!  --------
   noshave   = .false.
   nbits   = 10
   outFile = 'GFIO_shave.eta.nc4'

   nskip_vars = 0

   outPrec = 0

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
      else if(index(argv,'-noshave') .gt. 0 ) then
         noshave = .true.
      else if(index(argv,'-nbits') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         read(argv,*) nbits
      else if(index(argv,'-skipvars') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage_()
         iarg = iarg + 1
         call GetArg ( iArg, argv )
         call split_ ( ',', argv, mVars, Vars, nskip_vars )
      else if(index(argv,'-prec') .gt. 0 ) then
         call GetArg ( iArg, argv )
         read(argv,*) outPrec
      else
         inFile = argv
      end if

   end do

   if ( outPrec .eq. 32 ) outPrec = 0   
   if ( outPrec .eq. 64 ) outPrec = 1   
   
   print *,"nskip_vars: ",nskip_vars
   if ( nskip_vars > 0 ) then
     do n = 1, nskip_Vars
       skipVars(n) = Vars(n)
       print *,' ',n,' ',trim(skipVars(n))
     end do
   endif


!......................................................................


!   print *, "               ", trim(inFile)
!   print *, 'Output   File: ', trim(outFile), ', prec = ', outPrec
   end subroutine Init_


    subroutine Usage_()
   
print *, "NAME"
print *, "   GFIO_Shave  Shaves number of bits for each variable of a given file. "
print *, "             run through n4zip the output file, to reduce the file size."
print *
print *, "SYNOPYSIS"
print *, "   GFIO_Shave [options] input_fname"
print *
print *, "OPTIONS"
print *
print *, "  -o         ofname    output file name (default: GFIO_Shave.nc4)"
print *, "  -prec n     precision: "
print *, "                    n=0 for 32 bits (default) "
print *, "                    n=1 for 64 bits"
print *, "  -noshave          Do not shave the input field bits."
print *, "  -skipvars         Do not output the variables (default: write all)."
print *, "                    (ex: -skipvars var1,var2,var3)  "                 
print *, "  -nbits            Number of bits to shave."
print *, "                    default 10              "
print *
print *, "DESCRIPTION"
print *, "  Shaves the variables in the input file."
print *
print *, "   ex:  GFIO_Shave.x -nbits 14 -o outfile input_file"
print * 

    call die ( myname, 'exiting' )

    end subroutine Usage_

!............................................................................

!
      subroutine builtin_die(myname,string)
      character(len=*) myname
      character(len=*) string
!
      print *, ' --------------------------------'
      print *, '        ',myname
      print *, '   ',string
      print *, ' --------------------------------'
      call exit(1)
      return
      end subroutine builtin_die
! -------------------------------------------------------------------
!      
      subroutine Shave_field(ofn,in_e,jn_e,kn_r,xbits,undef)
         integer          :: in_e,jn_e,kn_r,k,has_undef,xbits,rc
         real             :: ofn(:,:,:),x(in_e,jn_e)
         real *4          :: xr(in_e,jn_e),undef_
         real, OPTIONAL, intent(in) :: undef    ! missing value
         integer, external :: ShaveMantissa32

         
         
          if ( present(undef) ) then
           undef_ = undef
           has_undef = 1
          else
           undef_ = 1.0
           undef_ = huge(undef_)   ! why not?
           has_undef = 0
          endif
         do k = 1,kn_r
          x(:,:) = ofn(:,:,k) 
          xr     = x  ! compiled r8 this will convert to r4.
          rc = ShaveMantissa32 ( xr, xr, size(x), xbits, has_undef, undef_, size(x) )
          if(rc == 0) then
            ofn(:,:,k) = xr(:,:)
          else
            print *,' ERROR in ShaveMantissa32:rc,level,var ',rc,k,trim(outVars(iv))
            stop 30
          endif
         end do
      end subroutine Shave_field
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
   end Program GFIO_Shave
