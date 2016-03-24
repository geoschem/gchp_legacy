
Program GFIO_remap
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GFIO_remap --- Remapping the data.
!
!
! !USAGE: see the routine usage() below
!
! !USES:  m_set_eta and m_mapz
!

   use m_set_eta, only: set_eta
   use m_mapz
   use m_maph
   use m_die

   Implicit NONE

! !DESCRIPTION: Converting 55 level mass fields to given level mass fields.
!
! !REVISION HISTORY:
!
!   Jan2006  Govindaraju  Initial design and coding
! 03Aug2006  da Silva     Minor clean up.
! 11Apr2014  Thompson     With input from Andrea Molod, get code working
!                         again for Ganymed-4_0. Fixed one bug if -nlev
!                         passed in is greater than that of the input file.
!                         Also, converted nearly all pointers to allocatable
!                         arrays to assuage confusion.
!-------------------------------------------------------------------------
!EOP

   character(len=*), parameter :: myname = 'GFIO_remap'


!                              -----------------------
!                               Hardwired Parameters
!                              -----------------------

   integer, parameter :: mFiles = 300       ! Max. number of input files
   integer, parameter :: mVars  = 300       ! Max. number of variables
   integer, parameter :: mLevs  = 300       ! Max. number of levels    


!                              -----------------------
!                              User Defined Parameters
!                              -----------------------


   integer            :: nFiles             ! Actual number of input files
   integer            :: kount              ! Variable counter
   character(len=256) :: inFiles(mFiles)    ! Input file names
   character(len=256) :: outFile            ! Output file name
   character(len=256) :: psFile             ! Input surface pressure file 
   integer  :: inc_hhmmss                   ! increment hours specified from command line

   integer  :: im                           ! zonal dimension
   integer  :: jm                           ! meridional dimension
   integer  :: km                           ! vertical dimension

   integer  :: iflag                        ! Initial flag
   integer  :: irflag                       ! Monthly Mean flag

   real big
   real zero
   real, allocatable :: lon(:)              ! longitudes in deg (im)
   real, allocatable :: lat(:)              ! latitudes in deg (jm)
   real, allocatable :: lev(:)              ! levels in hPa (km)
   real, allocatable :: lev0(:)             ! levels in hPa (km)
   real              :: rair,cpair,akap,pr

   integer           :: nLevs = 0           ! total number of levels
   real, pointer     :: Levs(:) => NULL()   ! vertical levels
   real, allocatable :: Levs0(:)            ! vertical levels
   character(len=25) :: cLevs(mLevs)        ! Character reprsentation of levels

   integer           :: nVars,nVars0        ! Actual number of variables
   character(len=64) :: inVars(mVars)       ! Input  variable names (nVars)
   character(len=64) :: outVars(mVars)      ! output variable names (nVars)
   character(len=64) :: uvar                ! output variable names (nVars)
   character(len=64) :: outUnits(mVars)     ! Units of output variables (nVars)

   integer           :: nqaVars             ! Actual number of variables
   integer           :: nptVars             ! Actual number of variables
   integer           :: nsphVars            ! Actual number of variables
   integer           :: nudVars             ! Actual number of variables
   integer           :: nvdVars             ! Actual number of variables
   integer           :: nucVars             ! Actual number of variables
   integer           :: nvcVars             ! Actual number of variables
   integer           :: jt

   character(len=64) :: qaVars(mVars)       ! output variable names (nqaVars)
   character(len=64) :: ptVars(mVars)       ! output variable names (nptVars)
   character(len=64) :: sphVars(mVars)      ! output variable names (nsphVars)
   character(len=64) :: udVars(mVars)       ! output variable names (nudVars)
   character(len=64) :: vdVars(mVars)       ! output variable names (nvdVars)
   character(len=64) :: ucVars(mVars)       ! output variable names (nucVars)
   character(len=64) :: vcVars(mVars)       ! output variable names (nvcVars)

   character (len=1) resolution

   integer          :: outPrec              ! Output file precision:
                                            !  0 = 32 bits,  1 = 64bits

!                              -----------------------
!                                Variable Work Space
!                              -----------------------

   real, allocatable :: inField(:,:,:)      ! Input variable
   real, allocatable :: u(:,:,:)            ! Working array 
   real, allocatable :: v(:,:,:)            ! Working array 
   real, allocatable :: uofn(:,:,:)         ! Working array 
   real, allocatable :: vofn(:,:,:)         ! Working array 
   real, allocatable :: outField(:,:,:)     ! Output variable
   real, allocatable :: ofn(:,:,:)          ! Output variable


!                                  Local Work Space
!                              -----------------------

   integer iff, it, iv, lm, itest, ii, i, j, k
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
   character(len=25)  :: append             ! im*jm
   real               :: missing_val

   integer, allocatable :: yyyymmdd(:)      ! Date
   integer, allocatable :: hhmmss(:)        !
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
   integer          :: nkount
   integer          :: rc, rc1,rc2,jq       ! return error code
   integer          :: i2,j2,i1,j1

   character(len=256) :: vtitle(mVars)      ! output title
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
   integer           :: jme_tmp             ! input meridional dimension       
   integer           :: jme_old             ! input meridional dimension       
   integer           :: jme_new             ! input meridional dimension       
   integer           :: km_e                ! input vertical dimension    
   integer           :: lm_e                ! input time dimension    
   integer           :: in_e                ! output zonal dimension       
   integer           :: jn_e                ! output meridional dimension       
   integer           :: kn_e                ! output vertical dimension    
   integer           :: in                  ! output zonal dimension       
   integer           :: jn                  ! output meridional dimension       

   integer           :: nVars_e             ! input number of variables   
   integer           :: kn                  ! Output number of vertical levels
   integer           :: ks32,ks_e,ks55                  
   real              :: undef               ! Missing value
   real, allocatable :: lon_e(:)            ! longitudes in deg (im)
   real, allocatable :: lat_e(:)            ! latitudes in deg (jm)
   real, allocatable :: lat_e2(:)           ! latitudes in deg (jm)
   real, allocatable :: ak55(:)             ! vertical grid a coefficien
   real, allocatable :: bk55(:)             ! vertical grid a coefficien
   real, allocatable :: ak32(:)             ! vertical grid a coefficien
   real, allocatable :: bk32(:)             ! vertical grid a coefficien
   real, allocatable :: akOut(:)            ! Output vertical grid a coefficien
   real, allocatable :: bkOut(:)            ! Output vertical grid a coefficien
   real, allocatable :: dpref(:)            ! vertical grid a coefficien
   real, allocatable :: pe3d_m(:,:,:)       ! Input Pressure edges 
   real, allocatable :: gaus_ps(:,:)        ! working array          
   real, allocatable :: gaus_delp(:,:,:)    ! working array          
   real, allocatable :: gaus_inField(:,:,:) ! working array          
   real, allocatable :: gaus_inField0(:,:,:) ! working array          
   real, allocatable :: pe3(:,:,:)          ! Output Pressure edges
   real, allocatable :: pe0(:,:)            ! Input Pressure edges (im,kn)
   real, allocatable :: pe1(:,:)            ! Input Pressure edges (im,kn)
   real, allocatable :: pe2(:,:)            ! Input Pressure edges (im,kn)
   real, allocatable :: pe4(:,:)            ! output Pressure edges (im,kn)
   real, allocatable :: pk0(:,:)            ! Input p ** kappa  (im,km)
   real, allocatable :: pk1(:,:)            ! Output p ** kappa (im,kn)
   real, allocatable :: ps(:,:)             ! Surface Pressure
   real, allocatable :: ps0(:,:,:)             ! Surface Pressure
   real, allocatable :: delp(:,:,:)         ! Pressure delta
   real, allocatable :: delp0(:,:,:)         ! Pressure delta
   real, allocatable :: dln0(:,:,:)         ! Pressure delta
   real*8, allocatable   :: lev_e(:)        ! levels in eta (km)
   integer, allocatable  :: kmVar_e(:)      ! Number of vertical levels for variables

   character(len=256) :: vtitle_in(mVars)   ! output title
   real              :: valid_range(2, mVars)
   real              :: packing_range(2, mVars)
   real              :: p,bkh, pref_in
   integer           :: km_e1,kn1
   integer           :: ngatts              ! Number of attributes for GFIO
   integer           :: nkl,nkls            ! index
   integer           :: gaussian
   logical           :: initial,file_exist,hintrp
   logical           :: qaflag,ptflag,udflag,vdflag,ucflag,vcflag
   logical           :: sphflag,shftlon,lon_shift
   logical           :: shaveFlag,clim_flag,add_delp
   integer           :: prefProvided
!.................................................................................


   initial = .true.
   nkount = 0
   hintrp = .false.

!  Get user input
!  --------------

   call  Init_ ( mFiles, nFiles, inFiles, outFile,psFile,resolution,      &
         in, jn, km, nLevs, Levs,kn,ndate,ntime,                  &
         mVars, nVars, inVars,nqaVars,nptVars,nsphVars,nudVars,   &
         nvdVars,nucVars,nvcVars,outVars,qaVars,ptVars,sphVars,   &
         udVars,vdVars,ucVars,vcVars,outPrec,append,shftlon,      &
         shaveFlag,prefProvided,clim_flag,add_delp,inc_hhmmss) 

   uv_flag = .false.
   u_flag  = .false.
   v_flag  = .false.

   if(nqaVars > 0) then
      do jt = 1,nqaVars
         print *,trim(qaVars(jt))
      end do
   endif

   if(nptVars > 0) then
      do jt = 1,nptVars
         print *,trim(ptVars(jt))
      end do
   endif

   if(nudVars > 0) then
      do jt = 1,nudVars
         print *,trim(udVars(jt))
      end do
   endif

   if(nvdVars > 0) then
      do jt = 1,nvdVars
         print *,trim(vdVars(jt))
      end do
   endif

   if(nucVars > 0) then
      do jt = 1,nucVars
         print *,trim(ucVars(jt))
      end do
   endif

   if(nvcVars > 0) then
      do jt = 1,nvcVars
         print *,trim(vcVars(jt))
      end do
   endif

   if(nsphVars > 0) then
      do jt = 1,nsphVars
         print *,trim(sphVars(jt))
      end do
   endif

   rair = 287.04
   cpair = 1004.64
   akap = rair/cpair
   big  = 1.e+10
   zero  = 0.0
   pi = 4.d0 * datan(1.d0)



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

      if(resolution == 'N' ) then
         hintrp = .false.
         in = im_e
         jn = jm_e
      else
         hintrp = .true.
      endif
!      print *,' im_e,im_e ',im_e,jm_e
!      print *,' in,in ',in,jn


!    Allocate memory for meta data

      if(allocated(lev))       deallocate(lev)
      if(allocated(kmVar_e))   deallocate(kmVar_e)
      if(allocated(lev_e))     deallocate(lev_e)
      if(allocated(lat_e))     deallocate(lat_e)
      if(allocated(lon_e))     deallocate(lon_e)
      if(allocated(hhmmss))    deallocate(hhmmss)
      if(allocated(yyyymmdd))  deallocate(yyyymmdd)

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


      if ( rc /= 0 )  call die (myname, 'can not do GFIO_Inquire')
      jme_tmp = jm_e
      if(lat_e(1) > -90.0 .and. lat_e(jm_e) < 90.0) then
         gaussian = 0
         if(allocated(ps)) deallocate(ps)
         allocate(ps(im_e,jm_e))
         if(allocated(delp)) deallocate(delp)
         allocate(delp(im_e,jm_e,km_e))
         if(allocated(lat_e2)) deallocate(lat_e2)
         allocate(lat_e2(jme_tmp))

         lat_e2 = lat_e

      else
         gaussian = 1
         if(allocated(ps)) deallocate(ps)
         allocate(ps(im_e,jm_e))
         if(allocated(delp)) deallocate(delp)
         allocate(delp(im_e,jm_e,km_e))
         if(allocated(lat_e2)) deallocate(lat_e2)
         allocate(lat_e2(jme_tmp))
         lat_e2 = lat_e
      endif
!!!     print *,' lat_e,gaussian ',lat_e(1),lat_e(jm_e), gaussian

!    Find long names and number of levels for meta data
!    -------------------------------
!     print *,' nVars,nVars_e ',nVars,nVars_e

!    if (nVars .le. 0) then
!       nVars = nVars_e
!    end if

      if(nqaVars == 0) nVars = nVars_e

      lev = lev_e
      if (nLevs .le. 0) then 
!       print *, "setting level information."
         nLevs = max(km_e,kn)
         km = max(km_e,kn)
         if(associated(Levs)) deallocate(Levs)
         allocate(Levs(nLevs), stat=rc)
         Levs = lev_e
      end if

      uvar = 'u'
      do iv = 1, nVars 
         if (nVars .eq. nVars_e) then
            outVars(iv) = vname(iv)
            outUnits(iv) = vunits(iv)
            vtitle_in(iv) = vtitle(iv)
            outKm(iv) = kmVar_e(iv)
            vtitle_in(iv) = vtitle(iv)
!          valid_range_prs(1,iv) = valid_range(1,iv)
            packing_range_prs(1,iv) = undef
!          valid_range_prs(2,iv) = undef
            packing_range_prs(2,iv) = packing_range(2,iv)

            valid_range_prs(1,iv) = -1.e+30
            valid_range_prs(2,iv) = 1.e+30
!          packing_range_prs(1,iv) = -1.e+30
!          packing_range_prs(2,iv) = 1.e+30
         else
            do itest = 1, nVars_e
               if ( outVars(iv) .eq. vname(itest) ) then
                  vtitle_in(iv) = vtitle(itest)
                  outUnits(iv) = vunits(itest)
                  vtitle_in(iv) = vtitle(itest)
                  outKm(iv) = kmVar_e(itest)
!                valid_range_prs(1,iv) = valid_range(1,itest)
                  packing_range_prs(1,iv) = undef
!                valid_range_prs(2,iv) = valid_range(2,itest)
                  packing_range_prs(2,iv) = undef

                  valid_range_prs(1,iv) = -1.e+30
!                packing_range_prs(1,iv) = -1.e+30
                  valid_range_prs(2,iv) = 1.e+30
!                packing_range_prs(2,iv) = 1.e+30
               endif
            end do
         endif
!       print *,'  packing_range_prs: ',iv,packing_range_prs(1,iv),packing_range_prs(2,iv)
      end do
!       print *,' nVars, nVars_e ', nVars, nVars_e

      print *
      print *, ' <> Output variables:'
      do itest = 1,nVars
         print *, '    ', itest, trim(outVars(itest))
      end do
      print *

      if(kn < 0) kn = km_e

      if(gaussian == 0) then
         if(allocated(pe3d_m)) deallocate(pe3d_m)
         allocate(pe3d_m(im_e,jm_e,km_e+1))
      else
         if(allocated(pe3d_m)) deallocate(pe3d_m)
         allocate(pe3d_m(im_e,jm_e,km_e+1))
      endif

      if(gaussian == 0) then
         jme_new = jme_tmp 
      else
         jme_new = jme_tmp
      endif


      if(km_e == 0 ) then
         km_e1 = 72
         kn1 = 72
      else
         km_e1 = km_e
         kn1 = kn
      endif

      do it = 1, lm_e

!         ---------------------------------------------------------
!             Set ak and bk for vertical interpolation.
!         ---------------------------------------------------------
         if(allocated(ak55)) deallocate(ak55)
         allocate(ak55(km_e1+1))
         if(allocated(bk55)) deallocate(bk55)
         allocate(bk55(km_e1+1))

         if(allocated(ak32)) deallocate(ak32)
         allocate(ak32(kn1+1))
         if(allocated(bk32)) deallocate(bk32)
         allocate(bk32(kn1+1))

         call set_eta ( km_e1, ks55, ptop55, pint55, ak55, bk55 )
!!!          print *,' out from set_eta ks55:km_e,kn,km_e1,kn1 ',km_e,kn,km_e1,kn1
         call set_eta ( kn1, ks32, ptop32, pint32, ak32, bk32 )
!!!          print *,' out from set_eta ks32'

!!!     if(kn /= km_e ) then
        if(kn /= km_e .or. (add_delp)) then
         if(allocated(dpref)) deallocate(dpref)
         if(allocated(pe2)) deallocate(pe2)
         if(allocated(pk1)) deallocate(pk1)
         if(allocated(pe4)) deallocate(pe4)
         if(allocated(pe3)) deallocate(pe3)
         if(allocated(pe1)) deallocate(pe1)
         if(allocated(pe0)) deallocate(pe0)
         if(allocated(pk0)) deallocate(pk0)

         allocate(pk0(im_e,km_e+1))
         allocate(pe0(im_e,km_e+1))
         allocate(pe1(im_e,km_e+1))
         allocate(pe3(im_e,jm_e,kn+1))
         allocate(pe4(im_e,kn+1))
         allocate(pk1(im_e,kn+1))
         allocate(pe2(im_e,kn+1))
         allocate(dpref(kn+1))


!        ---------------------------------------------------------
!           Check for the Surface Pressure from the user defined
!           variables or from the input file. Exit when not found.
!        ---------------------------------------------------------

         ps_found = .false.
         delp_found = .false.

         do iv = 1, nVars_e
            if ( vname(iv) == 'SURFP' .or. vname(iv) == 'PS' .or. &
                  vname(iv) == 'surfp' .or. vname(iv) == 'ps' ) then
               ps_found = .true.
               if(allocated(gaus_ps)) deallocate(gaus_ps)
               allocate(gaus_ps(im_e,jm_e))
               call GFIO_GetVar ( fid, vname(iv), yyyymmdd(it), hhmmss(it), &
                     im_e, jme_tmp, 0, 1, gaus_ps, rc )
               if ( rc /= 0 ) then
                  call die (myname, 'can not GetVar gaus_ps')
               endif
               ps = 0.0
               ps = gaus_ps
               if(allocated(gaus_ps)) deallocate(gaus_ps) 
            endif

            if ( vname(iv) == 'DELP' .or. vname(iv) == 'delp' ) then
               delp_found = .true.
               if(allocated(gaus_delp)) deallocate(gaus_delp)
               allocate(gaus_delp(im_e,jme_tmp,km_e))
               call GFIO_GetVar ( fid, vname(iv), yyyymmdd(it), hhmmss(it), &
                     im_e, jme_tmp, 1, km_e, gaus_delp, rc )
               if ( rc /= 0 )  call die (myname, 'can not GetVar gaus_delp ')

               if(lev_e(1) > lev_e(km_e)) then
                 print *,  vname(iv), ' lev_e are surface to top ',lev_e(1),lev_e(km_e), ' flipping the data'
                 do nkl = 1,km_e
                   nkls = km_e-nkl+1
                   delp(:,:,nkls) = gaus_delp(:,:,nkl)
                 end do
               else
                 delp = gaus_delp
                 if(allocated(gaus_delp)) deallocate(gaus_delp)
               endif
            endif
         end do

         call GFIO_GetRealAtt ( fid, 'ptop',   1, ptop, rc )
         if ( rc /= 0 ) then 
            print *,'  ', myname, 'can not GFIO_GetRealAtt ptop' 
            ptop_found = .false.
         else
            ptop_found = .true.
         endif

!           print *,' ps_found, delp_found ', ps_found,' ', delp_found
!        if(.not. ps_found) then
         if(.not. delp_found) then
            print *,' **** DELP  DOES NOT EXIST in ',trim(inFiles(iff)),' *****' 
            print *,' **** TRYING constuct DELP FROM ',trim(psFile), ' *****'

            call get_surfp(psFile,lm_e,km_e,it,yyyymmdd(it), hhmmss(it),ptop,ptop55,ak55,bk55,ps,delp, &
                  ps_found,delp_found,ptop_found,clim_flag) 
            print *,' Out from get_surfp '
         endif

         call get_pe(im_e,jm_e,km_e,ptop,delp,   &
               akap,pe3d_m)

         call cmp_pe(im_e,jm_e,km_e,ks_e,ptop, delp,           &
               ptop32,kn,ks32,ak32,bk32,pe3d_m,pe3)

         nLevs = kn

         if (prefProvided == 0) then
            ! MAT Use calculated pref_in from input ak bk
            pref_in = ( ( 2 * lev_e(km_e) - (ak55(km_e+1)+ak55(km_e)) ) / (bk55(km_e+1) + bk55(km_e)) ) * 100.
         else
            ! Use the provided value for pref_in. Note, before this was hardcoded to either
            ! 98400. or 100000.
            pref_in = real(prefProvided)
         endif

!         Reference pressure thickness
!         ----------------------------
         do  k = 1,nLevs

            dpref(k) = (ak32(k+1) - ak32(k)) + (bk32(k+1)-bk32(k)) * pref_in

         end do

         p = ptop32 + 0.5 * dpref(1)
!         levs(1) = p/100.0
         levs(1) = p
         do k = 2,kn
            p = p + 0.5 * ( dpref(k-1) + dpref(k) )
!           levs(k) = p/100.0
            levs(k) = p
!!!          print *,levs(k-1),dpref(k-1),dpref(k),levs(k)
         end do
         levs(1:nLevs) = levs(1:nLevs)/100.
        endif

!        if ( nLevs .le. 0 )  then
!           nLevs = km_e
!           allocate(Levs(km_e), stat = rc)
!           if ( rc /= 0 )  call die (myname, 'cannot allocate Levs')
!           Levs = lev_e
!        endif

!        print *, "Levs=",Levs

!        Create output GFIO file
!        -----------------------

         if( iff == 1 .and. it == 1) then
            if(hintrp) then
               in_e = in
               jn_e = jn
               kn_e = kn
               if(allocated(lat)) deallocate(lat)
               if(allocated(lon)) deallocate(lon)
               allocate(lon(in_e))
               allocate(lat(jn_e))

               lon(1) = lon_e(1)
               if(shftlon) then
                  if(lon_e(1) < 0.0) then
                     lon(1) = 0.0
                  else
                     lon(1) = -180.0
                  endif
               endif

               call get_latlon(lon,lat,in_e,jn_e)
            else
               in_e = im_e
               jn_e = jm_e
               kn_e = kn
               if(allocated(lat)) deallocate(lat)
               if(allocated(lon)) deallocate(lon)
               allocate(lon(in_e))
               allocate(lat(jn_e))

               lon(1) = lon_e(1)
               if(shftlon) then
                  if(lon_e(1) < 0.0) then
                     lon(1) = 0.0
                  else
                     lon(1) = -180.0
                  endif
               endif
               call get_latlon(lon,lat,in_e,jn_e)
            endif

            ndate_old = yyyymmdd(it)
            ntime_old = hhmmss(it)

            if(ndate /= 999999) then
               yyyymmdd(it) = ndate
            endif

            if(ntime /= 999999) then
               hhmmss(it) = ntime
            endif

            if ( timinc < 60 ) then
               print *
               print *, "               W A R N I N G"
               print *, myname//': # of time steps on input file: ', lm_e
               print *, myname//': invalid time increment = ', timinc
               print *, myname//': reseting it to 060000'
               timinc =  inc_hhmmss
               print *
            end if

!!!           print *,' timinc ',timinc

!!!            print *,' GFIO_Create: yyyymmdd,hhmmss,nVars ',yyyymmdd(it),hhmmss(it),nVars,in_e,jn_e,kn_e

!!!           print *, 'timinc = ', timinc

           
            if ( km_e  == kn1 .and. lev_e(1) > lev_e(nLevs)) then
             if (associated(Levs)) deallocate(Levs)
             allocate(Levs(nLevs),stat=rc)
             do nkl = 1,kn_e
              nkls = kn_e - nkl + 1
              Levs(nkls) = lev_e(nkl)
             end do
            else
              if(km_e == kn1 .and. lev_e(1) == 1) then
               Levs = lev_e
              endif
            endif

            if(add_delp) then
             nVars0 = nVars + 1
             outVars(nVars0)           = 'DELP'
             vtitle_in(nVars0)         = "pressure_thickness"
             outUnits(nVars0)          = "pressure_thickness"
             outKm(nVars0)             = kn_e
             valid_range_prs(1,nVars0) = -1.e+30
             valid_range_prs(2,nVars0) = 1.e+30
             nVars0 = nVars0 + 1
             outVars(nVars0)           = 'PS'
             vtitle_in(nVars0)         = "Surface Pressure"
             outUnits(nVars0)          = "Pa"
             outKm(nVars0)             = 0
             valid_range_prs(1,nVars0) = -1.e+30
             valid_range_prs(2,nVars0) = 1.e+30
            else
             nVars0 = nVars
            endif


            call GFIO_Create ( outFile, title, source, contact, undef,         &
                  in_e, jn_e, kn_e, lon, lat, Levs, levunits,     &
                  yyyymmdd,hhmmss,timinc,                         &
                  nVars0, outVars, vtitle_in, outUnits, outKm,    &
                  valid_range_prs, packing_range_prs, outPrec,    &
                  out_fid, rc )


            if(ndate /= 999999) then
               yyyymmdd(it) = ndate_old
            endif

            if(ntime /= 999999) then
               hhmmss(it)   = ntime_old
            endif

            if ( rc /= 0 )  call die (myname, 'wrong in GFIO_Create')
         endif


!     Loop over times on file
!     -----------------------

         print *, ' [] Reading ',trim( inFiles(iff))//' at ', &
               yyyymmdd(it), hhmmss(it) 


         nkount = nkount + 1

!        Loop over variables
!        -------------------
!        print *,' nVars ',nVars
         kount = 0
         do iv = 1, nVars 

            udflag = .false.
            if(nudVars > 0) then
               do jt = 1,nudVars
                  print *,' ',trim(outVars(iv)),' ',trim(udVars(jt))
                  if(outVars(iv) .eq. udVars(jt)) then
                     udflag = .true.
                     print *,' ',trim(outVars(iv)),' ',trim(udVars(jt)),' ',udflag
                  endif
               end do
            endif

            vdflag = .false.
            if(nvdVars > 0) then
               do jt = 1,nvdVars
                  print *,' ',trim(outVars(iv)),' ',trim(vdVars(jt))
                  if(outVars(iv) .eq. vdVars(jt)) then
                     vdflag = .true.
                  endif
                  print *,' ',trim(outVars(iv)),' ',trim(vdVars(jt)),' ',vdflag
               end do
            endif
!          print *,' udflag: ',udflag,' vdflag: ',vdflag,'nudVars: ',nudVars,' nvdVars: ',nvdVars

            ptflag = .false.
            if( nptVars > 0) then
               do jt = 1,nptVars
                  if(outVars(iv) == ptVars(jt)) then
                     ptflag = .true.
                  endif
               end do
            endif

            qaflag = .false.
            if( nqaVars > 0) then
               do jt = 1,nqaVars
                  if(outVars(iv) == qaVars(jt)) then
                     qaflag = .true.
                  endif
               end do
            endif

            sphflag = .false.
            if( nsphVars > 0) then
               do jt = 1,nsphVars
                  if(outVars(iv) == sphVars(jt)) then
                     sphflag = .true.
                  endif
               end do
            endif

!           print *,' iv,outVar ',iv,trim(outVars(iv))

!           Read variable from GFIO file
!           ----------------------------
            if ( outKm(iv) > 0 ) then             ! 3D file
               kount = kount + 1

!            Allocated memory 

               if(allocated(gaus_inField)) deallocate(gaus_inField)
               allocate (gaus_inField(im_e,jme_tmp,km_e),stat=rc )

!!!               print *,' jm_e, jme_tmp ',jm_e,jme_tmp
               if(allocated(inField)) deallocate(inField)
               allocate ( inField(im_e,jme_tmp,km_e),stat=rc )

               if ( rc /= 0 )  call die (myname, 'can not allocate  inField ')

!!!               print *,' GFIO_GetVar:  yyyymmdd,hhmmss ',yyyymmdd(it),hhmmss(it),outVars(iv)
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                     im_e, jme_tmp, 1, km_e,gaus_inField, rc )

                if(lev_e(1) > lev_e(km_e)) then
                  if(allocated(gaus_inField0)) deallocate(gaus_inField0)
                  allocate (gaus_inField0(im_e,jme_tmp,km_e),stat=rc )
                   print *,  outVars(iv), ' lev_e are surface to top ',lev_e(1),lev_e(km_e), ' flipping the data'


                   do nkl = 1,km_e
                    nkls = km_e-nkl+1
                    gaus_inField0(:,:,nkls) = gaus_inField(:,:,nkl)
                   end do
                   gaus_inField = gaus_inField0
               endif

!
!             --------------------------------------------------------------------
!                   If the Input Field is on Gaussian grid then call a routine
!                   to add a pole points to the scaller or vector fields.
!                     Courtesy of Dr.Ron Errico.
!             --------------------------------------------------------------------
!
               if(gaussian == 0) then
                  if(allocated(inField)) deallocate(inField)
                  allocate(inField(im_e,jme_tmp,km_e), stat = rc2)
                  if ( rc2 /= 0 )  call die (myname, 'can not allocate inField ')

                  if(trim(outVars(iv)) /= 'uwnd' .and. trim(outVars(iv)) /= 'vwnd' .and.  &
                        trim(outVars(iv)) /= 'UWND' .and. trim(outVars(iv)) /= 'VWND' .and.  &
                        trim(outVars(iv)) /= 'u' .and. trim(outVars(iv)) /= 'v' .and. &
                        trim(outVars(iv)) /= 'U' .and. trim(outVars(iv)) /= 'V')then  
!                   print *,' outVar:gaus_inField ',trim(outVars(iv))
!                   call minmaxpe(gaus_inField,im_e,jme_tmp,kn_e)
                     uv_flag = .false.

                     inField = 0.0
                     inField = gaus_inField

!                   print *,' outVar:inField ',trim(outVars(iv))

                  else
                     uv_flag = .true.

                     if(trim(outVars(iv)) == 'uwnd' .or. trim(outVars(iv)) == 'UWND' .or.  &
                           trim(outVars(iv)) == 'u' .or. trim(outVars(iv)) == 'U') then 

                        u_flag = .true.

                        allocate ( u(im_e,jme_new,km_e),stat=rc )
                        if ( rc /= 0 )  call die (myname, 'can not allocate  u ')
                        u = 0.0
                        u = gaus_inField

                        allocate ( v(im_e,jme_new,km_e),stat=rc )
                        if ( rc /= 0 )  call die (myname, 'can not allocate  v ')

                        call GFIO_GetVar ( fid, 'v', yyyymmdd(it),                &
                              hhmmss(it),im_e, jme_tmp, 1, km_e, gaus_inField, rc )

                        v = 0.0
                        v = gaus_inField

!                      print *,' outVar:u ',trim(outVars(iv))
!                      call minmaxpe(u,im_e,jme_new,kn_e)
!                      print *,' outVar:v '
!                      call minmaxpe(v,im_e,jme_new,kn_e)

                        if(allocated(inField)) deallocate(inField)
                        allocate(inField(im_e,jme_new,km_e), stat = rc2)
                        if ( rc2 /= 0 )  call die (myname, 'can not allocate inField ')

                        inField = u
                        if(allocated(u)) deallocate(u)

                     elseif(outVars(iv) == 'vwnd' .or. outVars(iv) == 'VWND' .or.  &
                           outVars(iv) == 'v' .or. outVars(iv) == 'V') then 
                        if(allocated(inField)) deallocate(inField)
                        allocate(inField(im_e,jme_new,km_e), stat = rc2)
                        if ( rc2 /= 0 )  call die (myname, 'can not allocate inField ')
                        inField = v
                        v_flag = .true.
                        if(allocated(v)) deallocate(v)
                     endif
                  endif
               else                     ! non-gaussian

                  uv_flag = .true.
                  if(trim(outVars(iv)) /= 'uwnd' .and. trim(outVars(iv)) /= 'vwnd' .and.  &
                        trim(outVars(iv)) /= 'UWND' .and. trim(outVars(iv)) /= 'VWND' .and.  &
                        trim(outVars(iv)) /= 'u' .and. trim(outVars(iv)) /= 'v' .and. &
                        trim(outVars(iv)) /= 'U' .and. trim(outVars(iv)) /= 'V')then  
                     uv_flag = .false.
                  endif

                  u_flag = .false.
                  if(trim(outVars(iv)) == 'UWND' .or. trim(outVars(iv)) == 'uwnd' .or.  &
                        trim(outVars(iv)) == 'U' .or. trim(outVars(iv)) == 'u') then
                     u_flag = .true.
                  endif

                  v_flag = .false.
                  if(trim(outVars(iv)) == 'VWND' .or. trim(outVars(iv)) == 'vwnd' .or.  &
                        trim(outVars(iv)) == 'V' .or. trim(outVars(iv)) == 'v') then
                     v_flag = .true.
                  endif

                  inField = gaus_inField
               endif                     ! gaussian 

               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 3D file')


               ndate_old = yyyymmdd(it)
               ntime_old = hhmmss(it)

               if(ndate /= 999999) then
                  yyyymmdd(it) = ndate
               endif

               if(ntime /= 999999) then
                  hhmmss(it) = ntime
               endif

               if(allocated(outField)) deallocate(outField)
               allocate ( outField(im_e,jme_new,kn),stat=rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate  outField')

               lon_shift = .false.
!              if(kn /= km_e) then
               if(kn /= km_e  .or. (add_delp)) then

                  print *,' ptop,im_e,jme_new,km_e,in,jn,kn ',ptop,im_e,jme_new,km_e,in,jn,kn

                   
                  if(add_delp) then
                    if(allocated(delp0)) deallocate(delp0)
                    allocate(delp0(in,jn,kn))

                   call z_interp("DELP",delp,pe3d_m,ptop,im_e,jme_new,km_e,            &
                                 qaflag,ptflag,sphflag,udflag,vdflag,ucflag,vcflag,    &
                                 ptop32,ks32,ak32,bk32,pe3,in,jn,kn, delp0)

                  endif
                   
                   call z_interp(outVars(iv),inField,pe3d_m,ptop,im_e,jme_new,km_e,    &
                                 qaflag,ptflag,sphflag,udflag,vdflag,ucflag,vcflag,    &
                                 ptop32,ks32,ak32,bk32,pe3,in,jn,kn, outField)

               else  
                  outField = inField
               endif                           ! kn /= km_e

!              if(shftlon) then
!               call shift_field(outField,in,jn,kn)
!               lon_shift = .true.
!              endif

               if(hintrp) then
                  in_e = in
                  jn_e = jn
                  kn_e = kn
                  if(allocated(ofn)) deallocate(ofn)
                  allocate(ofn(in_e,jn_e,kn_e))

                  if((.not. udflag) .and. (.not. vdflag)) then
                     ig = 0
                     is = 0
                  else if(udflag) then
                     ig = 1
                     is = 1
                  else if(.not. udflag) then
                     ig = 0
                     is = 1
                  else if(vdflag) then
                     ig = 0
                     is = 1
                  endif

                  ofn = 0.0

!                  print *,' h_interp: ig,is: ',ig,is
                  if (add_delp) then
                    if(allocated(dln0)) deallocate(dln0)
                    allocate(dln0(in_e,jn_e,kn_e))
                    call h_interp(delp0,im_e,jme_new,lat_e2,gaussian,ig,is,kn_e,in_e,jn_e,dln0)
                  endif
                  call h_interp(outField,im_e,jme_new,lat_e2,gaussian,ig,is,kn_e,in_e,jn_e,ofn)

!                  if((shftlon) .and. (.not. lon_shift)) then
!                   call shift_field(ofn,in_e,jn_e,kn_e)
!                  endif

!                 if(gaussian == 0) then
                  if( (.not.udflag) .and. (.not. vdflag)) then
!                   print *,' udflag: ',udflag,' vdflag: ',vdflag
                     ofn(:,1,:) = 0.0
                     ofn(:,jn_e,:) = 0.0
                     if(.not. uv_flag) then
                        print *,' **** Calling ploe_tq:ofn **** '
                        call pole_tq(in_e,jn_e,kn_e,ofn,0)

                        if(shftlon) then
                           if(add_delp) then
                             call shift_field(dln0,in_e,jn_e,kn_e)
                           endif
                           call shift_field(ofn,in_e,jn_e,kn_e)
                        endif

                        if(shaveFlag) then
                           if(add_delp) then
                             call Shave_field(dln0,in_e,jn_e,kn_e,undef=undef)
                           endif
                           call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
                        endif

                        if(add_delp) then
                           call GFIO_PutVar (out_fid,"DELP",yyyymmdd(it),hhmmss(it),  &
                                             in_e, jn_e, 1, kn_e, dln0, rc )
                        endif

                        call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                              in_e, jn_e, 1, kn_e, ofn, rc )
                     elseif(u_flag) then
                        if(allocated(uofn)) deallocate(uofn)
                        allocate(uofn(in_e,jn_e,kn_e), stat = rc)
                        uvar = outVars(iv)
                        uofn = ofn
                        u_flag = .false.
                     elseif(v_flag) then
                        if(allocated(vofn)) deallocate(vofn)
                        allocate(vofn(in_e,jn_e,kn_e), stat = rc)
                        vofn = ofn
                        print *,' **** Calling ploe_uv:ofn **** '
!                     call pole_uv (in_e,jn_e,kn_e,uofn,vofn,0)
                        ofn = uofn

                        if(shaveFlag) then
                           if(add_delp) then
                             call Shave_field(dln0,in_e,jn_e,kn_e,undef=undef)
                           endif
                           call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
                        endif

                        if(shftlon) then
                           if(add_delp) then
                             call shift_field(dln0,in_e,jn_e,kn_e)
                           endif
                           call shift_field(ofn,in_e,jn_e,kn_e)
                        endif

                        if(add_delp) then
                          call GFIO_PutVar (out_fid,"DELP",yyyymmdd(it),hhmmss(it),  &
                                            in_e, jn_e, 1, kn_e, dln0, rc )
                        endif
                        call GFIO_PutVar (out_fid,uvar,yyyymmdd(it),hhmmss(it),  &
                              in_e, jn_e, 1, kn_e, ofn, rc )
                        ofn = vofn

                        if(shaveFlag) then
                           call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
                        endif

                        if(shftlon) then
                           call shift_field(ofn,in_e,jn_e,kn_e)
                        endif

                        call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                              in_e, jn_e, 1, kn_e, ofn, rc )
                        if(allocated(uofn)) deallocate(uofn)
                        if(allocated(vofn)) deallocate(vofn)
                        v_flag = .false.
                        uv_flag = .false.
                     endif
                  else
!                   print *,' udflag: ',udflag,' vdflag: ',vdflag
                     if(shaveFlag) then
                      if(add_delp) then
                        call Shave_field(dln0,in_e,jn_e,kn_e,undef=undef)
                      endif
                        call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
                     endif

                     if(shftlon) then
                      if(add_delp) then
                        call shift_field(dln0,in_e,jn_e,kn_e)
                      endif
                        call shift_field(ofn,in_e,jn_e,kn_e)
                     endif

                     if(add_delp) then
                       call GFIO_PutVar (out_fid,"DELP",yyyymmdd(it),hhmmss(it),  &
                                         in_e, jn_e, 1, kn_e, dln0, rc )
                     endif

                     call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                           in_e, jn_e, 1, kn_e, ofn, rc )
                  endif
!                  else
!                   call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
!                   call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
!                                     in_e, jn_e, 1, kn_e, ofn, rc )
!                  endif

                  if ( rc /= 0 ) call die (myname, 'something wrong in GFIO_PutVarT for 3D file')
               else                 ! non-hintrp                            
                  in_e = im_e
                  jn_e = jm_e
                  kn_e = kn

                  if(add_delp) then
                   if(allocated(dln0)) deallocate(dln0)
                   allocate(dln0(in_e,jn_e,kn_e),stat = rc )
                   dln0 = delp0
                   if(shaveFlag) then
                     call Shave_field(dln0,in_e,jn_e,kn_e,undef=undef)
                   endif

                   if(shftlon) then
                     call shift_field(dln0,in_e,jn_e,kn_e)
                   endif

                  endif

                  if(allocated(ofn)) deallocate(ofn)
                  allocate(ofn(in_e,jn_e,kn_e),stat = rc )
                  if ( rc /= 0 )  call die (myname, 'Can not allocate ofn ' )
                  ofn = outField
!                print *,' size(outField): ',  size(outField)
!                print *,' ofn: ',  ofn(1,1,:)
!                print *,' out_fid: ', out_fid
!
!                 --------------------------------------------------------
!                 Ravi 20070725  Do not remove the dummy call to minmaxpe.
!                   Could be a GFIO memory problem for 3d Variables. 
!                 --------------------------------------------------------

!                 call minmaxpe(ofn,in_e,jn_e,kn_e)
                  if(shaveFlag) then
                     call Shave_field(ofn,in_e,jn_e,kn_e,undef=undef)
                  endif

                  if(shftlon) then
                     call shift_field(ofn,in_e,jn_e,kn_e)
                  endif

                  call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                        in_e, jn_e, 1, kn_e, ofn, rc )
                  if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutVarT for 3D file')

                  if(add_delp) then
                   print *,' *** We are here *** '
                   call GFIO_PutVar (out_fid,"DELP",yyyymmdd(it),hhmmss(it),  &
                                     in_e, jn_e, 1, kn_e, dln0, rc )

!                  print *, ' *** Writing PS *** '

!                  call minmaxps(ps,in_e,jn_e,1)

                   call GFIO_PutVar (out_fid,"PS",yyyymmdd(it),hhmmss(it),  &
                                     in_e, jn_e, 0, 1, ps, rc )
                   if(rc /= 0 )  then
                     print *, ' rc: ',rc
                     call die (myname, 'something wrong in GFIO_PutVarT for 2D Array PS' )
                   endif
                 endif
                  
               endif                 !hintrp
            else                                       ! 2D file
               if(allocated(gaus_inField)) deallocate(gaus_inField)
               allocate ( gaus_inField(im_e,jme_tmp,1),stat = rc )
               if ( rc /= 0 )  call die (myname, 'can not allocate gaus_inField 2D')
               call GFIO_GetVar ( fid, outVars(iv), yyyymmdd(it), hhmmss(it), &
                     im_e, jme_tmp, 0, 1, gaus_inField, rc )

               if(allocated(inField)) deallocate(inField)
               allocate ( inField(im_e, jme_tmp, 1),stat = rc )
               inField = 0.0
               inField = gaus_inField

               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_GetVarT for 2D file')

               !**************
               ! Map lwi - simple nearest neighbors for now
               !**************

               if(hintrp) then
                  in_e = in
                  jn_e = jn
                  kn_e = 1
                  ig = 0
                  is = 0
                  if(allocated(ofn)) deallocate(ofn)
                  allocate(ofn(in_e,jn_e,kn_e),stat = rc)
                  if ( rc /= 0 )  call die (myname, 'can not allocate ')
                  if (vname(iv) == 'lwi' .or. vname(iv) == 'LWI') then

                     dx1 = 360./float(im_e)
                     dy1 = pi/float(jm_e-1)

                     dx2 = 360./float(in_e)
                     dy2 = pi/float(jn_e-1)

                     rx = dx2 / dx1
                     ry = dy2 / dy1
                     do j2 = 1, jn_e
                        j1 = nint ( 1 + ry * ( j2 - 1 ) )
                        j1 = max ( 1, min(j1,jm_e) )
                        do i2 = 1, in_e
                           i1 = nint ( 1 + rx * ( i2 - 1 ) )
                           i1 = max ( 1, min(i1,im_e) )
                           ofn(i2,j2,1) = inField(i1,j1,1)
                        end do
                     end do
                  else
                     call h_interp(inField,im_e,jme_new,lat_e2,gaussian,ig,is,kn_e,in_e,jn_e,ofn)
                  endif
               else
                  in_e = im_e
                  jn_e = jm_e
                  if(allocated(ofn)) deallocate(ofn)
                  allocate(ofn(in_e,jn_e,1),stat = rc)
                  if ( rc /= 0 )  call die (myname, 'can not allocate ')
                  ofn = inField
               endif

!              if((shftlon)) then
!                call shift_field(ofn,in_e,jn_e,1)
!              endif

!              print *,' GFIO_PutVar 2d: outVars,it,date,time ', trim(outVars(iv)),in_e,jn_e,kn_e,it,yyyymmdd(it),hhmmss(it)
               if(gaussian == 0) then
                  ofn(:,1,:) = 0.0
                  ofn(:,jn_e,:) = 0.0
                  call pole_tq(in_e,jn_e,kn_e,ofn,0)
               endif

               ndate_old = yyyymmdd(it)
               ntime_old = hhmmss(it)

               if(ndate /= 999999) then
                  yyyymmdd(it) = ndate
               endif

               if(ntime /= 999999) then
                  hhmmss(it) = ntime
               endif

               if(shaveFlag) then
                  call Shave_field(ofn,in_e,jn_e,1,undef=undef)
               endif

               if(shftlon) then
                  call shift_field(ofn,in_e,jn_e,1)
               endif

               print *,' outVars(',iv,'): ',outVars(iv)
               call GFIO_PutVar (out_fid,outVars(iv),yyyymmdd(it),hhmmss(it),  &
                     in_e, jn_e, 0, 1, ofn, rc )
               if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutVarT for 2D file')
            endif

            if(allocated(ofn))      deallocate( ofn)
            if(allocated(dln0))      deallocate( dln0)
            if(allocated(outField)) deallocate(outField)
            if(allocated(inField))  deallocate( inField)
            if(allocated(gaus_inField))  deallocate(gaus_inField)
            if(allocated(gaus_inField0))  deallocate(gaus_inField0)
            if(allocated(Levs0))  deallocate(Levs0)

            if(ndate /= 999999) then
               yyyymmdd(it) = ndate_old
            endif

            if(ntime /= 999999) then
               hhmmss(it)   = ntime_old
            endif

!!!            print *, ' iv,out_var ',iv,' ',yyyymmdd(it),hhmmss(it),trim(outVars(iv))
         end do  ! variables

         if(allocated(lon))  deallocate(lon,stat=rc)
         if ( rc /= 0 )  call die (myname, 'something wrong in deallocate lon')
         if(allocated(lat))  deallocate(lat,stat=rc)
         if ( rc /= 0 )  call die (myname, 'something wrong in deallocate lat')

!            Write the Attributes.

         if(iff == 1 .and. it == 1 ) then
            if(allocated(bkOut)) deallocate(bkOut)
            if(allocated(akOut)) deallocate(akOut)
            allocate(akOut(nLevs+1))
            allocate(bkOut(nLevs+1))
            if(kn /= km_e) then
               ptopOut = ptop32
               pintOut = pint32
               ksOut = ks32
               akOut = ak32
               bkOut = bk32
               if(allocated(ak32))  deallocate(ak32)
               if(allocated(bk32))  deallocate(bk32)
            else
               ptopOut = ptop55
               pintOut = pint55
               ksOut = ks55
               akOut = ak55
               bkOut = bk55
               if(allocated(ak55))  deallocate(ak55)
               if(allocated(bk55))  deallocate(bk55)
            endif

            call GFIO_PutRealAtt (out_fid, 'ptop', 1, ptopOut, outPrec, rc )
            if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutRealAtt ptop')

            call GFIO_PutRealAtt (out_fid, 'pint', 1, pintOut, outPrec, rc )
            if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutRealAtt pint')

            call GFIO_PutRealAtt (out_fid, 'ks', 1, ksOut, outPrec, rc )
            if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutRealAtt ks')

            call GFIO_PutRealAtt (out_fid, 'ak', nLevs+1, akOut, outPrec, rc )
            if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutRealAtt ak')

            call GFIO_PutRealAtt (out_fid, 'bk', nLevs+1, bkOut, outPrec, rc )
            if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_PutRealAtt bk')

            if(allocated(akOut)) deallocate(akOut)
            if(allocated(bkOut)) deallocate(bkOut)

         endif
      end do     ! times

      deallocate ( yyyymmdd,hhmmss,lon_e,lat_e,lev_e, &
            kmVar_e, lev)

!     Close input file
!     ----------------

      call GFIO_Close ( fid, rc )
      if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_Close fid')

   end do ! input files

   call GFIO_Close ( out_fid, rc )
   if ( rc /= 0 )  call die (myname, 'something wrong in GFIO_Close out_fid')

   if(allocated(lat_e2)) deallocate(lat_e2)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate lat_e2')
   if(allocated(dpref)) deallocate(dpref,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate dpref')
   if(allocated(pe2))  deallocate(pe2,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe2')
   if(allocated(pk1))  deallocate(pk1,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pk1')
   if(allocated(pe4))  deallocate(pe4,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe4')
   if(allocated(pe3))  deallocate(pe3,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe3')
   if(allocated(pe1))  deallocate(pe1,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe1')
   if(allocated(pe0))  deallocate(pe0,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe0')
   if(allocated(pk0))  deallocate(pk0,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pk0')
   if(allocated(delp)) deallocate(delp,stat=rc)
   if(allocated(delp0)) deallocate(delp0)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate delp')
   if(allocated(ps))   deallocate(ps,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate ps')
   if(allocated(pe3d_m))   deallocate(pe3d_m,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate pe3d_m')
   if(allocated(ak32))   deallocate(ak32,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate ak32')
   if(allocated(bk32))   deallocate(bk32,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate bk32')
   if(allocated(ak55))   deallocate(ak55,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate ak55')
   if(allocated(bk55))   deallocate(bk55,stat=rc)
   if ( rc /= 0 )  call die (myname, 'something wrong in deallocate bk55')

!  All done
!  --------
   call exit(0)

CONTAINS


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  cmp_pe --- Computes the Pressure edges.
!
! !INTERFACE:
!
   subroutine cmp_pe(im_e,jm_e,km_e,ks_e,ptop_old,delp,        &
         ptop32,kn,ks32,ak32,bk32,pe3d_m,pe3)

!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer           :: im_e,jm_e,km_e,kn
      real              :: ak32(:)            
      real              :: bk32(:)            
      real              :: delp(:,:,:)        ! Input  pressure
      real              :: ptop32
      real              :: ptop_old
      real              :: pe3d_m(:,:,:)
      integer           :: ks_e,ks32

!
! !OUTPUT PARAMETERS:
!

      real              :: pe3(:,:,:)         ! Output pressure edges


!    WORK AREA
      real, allocatable :: pe1(:,:)
      real, allocatable :: pk1(:,:)
      real, allocatable :: pk2(:,:)
      real, allocatable :: pk3d_m(:,:,:)
      real              :: cpair,akap
      real              :: pek,ple

      integer           :: i,j,k

! !DESCRIPTION: This routine coputes Presure edges for the input and output.
!
! !REVISION HISTORY:
!
! Jan 2006 Ravi C. Govindaraju  Initial design and prologue.
!
!EOP
!-------------------------------------------------------
!          Input pressure edges.
!-------------------------------------------------------

      rair = 287.04
      cpair = 1004.64
      akap = rair/cpair

!         print *,' ks_e.ks32 ',ks_e,ks32

      allocate(pk3d_m(im_e,jm_e,km_e+1))
      allocate(pe1(im_e,km_e+1))
      allocate(pk1(im_e,km_e+1))
      allocate(pk2(im_e,kn+1))

!   ---------------------------------------
!         Original data
!   ---------------------------------------

      do k=1,km_e+1
         do j=1,jm_e
            do i=1,im_e
               pk3d_m(i,j,k)  = pe3d_m(i,j,k) ** akap
            enddo
         enddo
      enddo

!       print *,' cmp_pe:pe3d_m '
!       call minmaxpe(pe3d_m,im_e,jm_e,km_e+1)

      do  j = 1,jm_e
         do  k = 1,km_e+1
            do i = 1,im_e
               pe1(i,k) = pe3d_m(i,j,k)
               pk1(i,k) = pk3d_m(i,j,k)
            end do
         end do

!-------------------------------------------------------
!          Output pressure edges.
!-------------------------------------------------------

         pek = ptop32 * akap
         do  i = 1,im_e

            pe3(i,j,1) = ptop32
            pe3(i,j,kn+1) = pe1(i,km_e+1)

            pk2(i,1) = pek
            pk2(i,kn+1) = pk1(i,km_e+1)
         end do

         if(ks32 .ne. 0) then
            do  k = 2,ks32+1
               ple = ak32(k)
               pek = ple ** akap
               do  i = 1,im_e
                  pe3(i,j,k) = ple
                  pk2(i,k) = pek
               end do
            end do
         endif

         do k = ks32+2,kn
            ple = ak32(k)
            pek = bk32(k)
            do i= 1,im_e
               pe3(i,j,k) = ple + pek * pe1(i,km_e+1)
            end do
         end do
      end do

      deallocate(pk3d_m)
      deallocate(pe1)
      deallocate(pk1)
      deallocate(pk2)
   end subroutine cmp_pe

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  get_surfp --- Constructs and read the surface pressure file.
!
! !INTERFACE:
!

   subroutine get_surfp(psFile,lm_e,kmre,it,ndate, nhms,ptop,ptop55,ak55,bk55,ps,delp, &
         ps_found,delp_found,ptop_found,clim_flag) 

!
! !USES:
!
      Implicit NONE

      character(len=*), parameter :: myname = 'get_surfp'
!
! !INPUT PARAMETERS:


      character(len=*)   :: psFile             ! Input surface pressure file 
      integer            :: lm_e               ! Number of input time periods.
      integer            :: kmre               ! Number of levels
      integer            :: ndate              ! Required date
      integer            :: nhms               ! Require  time
      integer            :: it   
      logical            :: delp_found,ptop_found,ps_found
      real               :: ptop55
      real               :: ak55(:),bk55(:)

!    WORK AREA

      character(len=256) :: title              ! meta data title
      character(len=256) :: source             ! data source
      character(len=256) :: contact            ! contact org.   
      character(len=256) :: levunits           ! Vertical levels
      character(len=25)  :: append             ! im*jm
      character(len=256) :: vtitle(mVars)      ! output title
      character(len=256) :: vunits(mVars)      ! output title
      character(len=256) :: vname(mVars)       ! output variable names (nVars)
      character(len=256) :: surfFile           ! Input surface pressure file 

      real               :: valid_range_prs(2, mVars)
      real               :: packing_range_prs(2, mVars)
      real               :: undef
      real, allocatable  :: lon_e(:)           ! latitudes in deg (jm)
      real, allocatable  :: lat_e(:)           ! latitudes in deg (jm)
      real, allocatable  :: ps1(:,:)           ! Surface pressure
      real, allocatable  :: delp1(:,:,:)       ! Delp
      real, allocatable  :: gaus_ps(:,:)       ! ps
      real, allocatable  :: gaus_delp(:,:,:)   ! ps
      real, allocatable  :: pe_ec(:,:,:)       ! EC Pressure edges.
      real*8, allocatable  :: lev_e(:)         ! levels in eta (km)

      integer, allocatable :: yyyymmdd0(:)      ! Date
      integer, allocatable :: hhmmss0(:)        !
      integer, allocatable :: kmVar_e(:)       ! Number of vertical levels for variables
      integer            :: rc,fidps,timinc
      integer            :: ps_fmode = 1       ! non-zero for READ-ONLY
      integer            :: ngatts             ! Number of attributes for GFIO
      integer            :: nVars_e,jm_old,jme_tmp       
      integer            :: im_e,jm_e,km_e,lm_e1,i,j,k

!
! !OUTPUT PARAMETERS:
!
      real               :: ps(:,:)
      real               :: delp(:,:,:),ptop
      logical            :: clim_flag

! !DESCRIPTION: This routine constructs the surface file and extracts the surface pressure.
!
! !REVISION HISTORY:
!
! Jan 2006 Ravi C. Govindaraju  Initial design and prologue.
!
!EOP
!-------------------------------------------------------------------------


!          ------------------------------------------
!             Construct or confirm psFile.
!          ------------------------------------------

      call get_psfile(psFile,surfFile,ndate,nhms,clim_flag,it)
      print *,' out from get_psfile '

!          Open GFIO file
!          --------------
      call GFIO_Open ( surfFile, ps_fmode, fidps, rc )
      if ( rc /= 0 )  call die (myname, 'can not open ps files')

!    Determine on file
!    ------------------
      call GFIO_DimInquire ( fidps, im_e, jme_tmp, km_e, lm_e1, nvars_e, ngatts, rc)
      if ( rc /= 0 )  call die (myname, 'can not do GFIO_DimInquire')


!    Allocate memory for meta data
!    ------------------
      if(allocated(pe_ec)) deallocate(pe_ec)
      allocate(pe_ec(im_e,jm_e,kmre+1),stat=rc)

      allocate ( yyyymmdd0(lm_e1),hhmmss0(lm_e1),lon_e(im_e),lat_e(jme_tmp),lev_e(km_e), &
            kmVar_e(mVars), stat = rc )
      if ( rc /= 0 )  call die (myname, 'can not allocate yyyymmdd0,hhmmss0,lon_e,lat_e,lev')

      print *,' get_surfp: allocation is completed'

!        Get meta data
!    ------------------

      print *,' get_surfp: inquire ',lm_e1,im_e,jme_tmp,km_e

      call GFIO_Inquire ( fidps, im_e, jme_tmp, km_e, lm_e1, nVars_e,  &
            title, source, contact, undef,           &
            lon_e, lat_e, lev_e, levunits,           &
            yyyymmdd0, hhmmss0, timinc,                &
            vname, vtitle, vunits, kmVar_e,          &
            valid_range , packing_range, rc)

      print *,' get_surfp: inquire is completed'
      if ( rc /= 0 )  call die (myname, ' can not do GFIO_Inquire ')

!        ps_found = .false.
      jm_old = jme_tmp
      do iv = 1,nVars_e
         if(.not. ps_found) then
            if ( vname(iv) == 'SURFP' .or. vname(iv) == 'PS' .or. &
                  vname(iv) == 'surfp' .or. vname(iv) == 'ps' .or. &
                  vname(iv) == 'lnsphlev1' ) then
               ps_found = .true.
               if(allocated(gaus_ps)) deallocate(gaus_ps)
               allocate(gaus_ps(im_e,jm_old))
               print *,' ',trim(vname(iv))
               call GFIO_GetVar ( fidps, vname(iv), yyyymmdd0(1), hhmmss0(1),   &
                     im_e, jm_old, 0, 1,gaus_ps, rc )
               if ( rc /= 0 )  call die (myname, ' can not GetVar ps ')

!
!                 If the input grid is on gaussian then adjust the pole points.
!
               if (vname(iv) == 'lnsphlev1' ) then
                  gaus_ps = exp(gaus_ps)
                  print *,' get_surf:ps '
!            call minmaxps(ps,im_e,jm_old,1)

                  if(allocated(pe_ec)) deallocate(pe_ec)
                  allocate(pe_ec(im_e,jm_old,kmre+1),stat=rc)
                  ps = 0.0
                  ps = gaus_ps
               else
                  ps = 0.0
                  ps = gaus_ps

                  if ( lon_e(1) .lt. 0 ) then ! [-180,180] to [0,360]
                     allocate(ps1(im_e,jme_tmp))
                     do i =1, im_e/2
                        do j = 1, jme_tmp
                           ps1(i,j) = ps(i+im_e/2,j)
                        end do
                     end do

                     do i =im_e/2+1, im_e
                        do j = 1, jme_tmp
                           ps1(i,j) = ps(i-im_e/2,j)
                        end do
                     end do
                     ps = ps1
                     deallocate (ps1)
                  endif
               endif
            endif
         endif

!        print *,' ps_found, delp_found ',ps_found,delp_found
         if(.not. delp_found) then
            jme_tmp = jm_old
            if(allocated(gaus_delp)) deallocate(gaus_delp)
            allocate(gaus_delp(im_e,jm_old,kmre))
            if ( vname(iv) == 'DELP' .or. vname(iv) == 'delp' ) then
               call GFIO_GetVar ( fidps, vname(iv), yyyymmdd0(1), hhmmss0(1), &
                     im_e, jm_old, 1, kmre, gaus_delp, rc )

               if ( rc /= 0 ) call die (myname, ' can not GetVar gaus_delp')

               delp = 0.0
               delp = gaus_delp
               if(allocated(gaus_delp)) deallocate(gaus_delp)
               jme_tmp = jm_old
               delp_found = .true.
            endif
         endif
      end do

      if(.not. delp_found) then
         print *,' ** Constructing delp **',kmre
         if(allocated(pe_ec)) deallocate(pe_ec)
         allocate(pe_ec(im_e,jm_old,kmre+1),stat=rc)

         do k = 1,kmre+1
            pe_ec(:,:,k) = ak55(k) + bk55(k) * ps
         end do
         print *,' *** pe_ec is constructed *** ',jme_tmp,im_e

         do j = 1,jme_tmp
            do i = 1,im_e
               if(pe_ec(i,j,1) <= 0.0) pe_ec(i,j,1) = 0.00001
            end do
         end do
         print *,' *** pe_ec is constructed *** '

         do k = 1,kmre
            delp(:,:,k) = pe_ec(:,:,k+1)-pe_ec(:,:,k)
         end do
         print *,' get_surf:delp '
!             call minmaxpe(delp,im_e,jm_e,kmre)
      endif

      if ( lon_e(1) .lt. 0 ) then ! [-180,180] to [0,360]
         allocate(delp1(im_e,jme_tmp,kmre))
         do  k = 1,kmre
            do i =1, im_e/2
               do j = 1, jme_tmp
                  delp1(i,j,k) = delp(i+im_e/2,j,k)
               end do
            end do

            do i =im_e/2+1, im_e
               do j = 1, jme_tmp
                  delp1(i,j,k) = delp(i-im_e/2,j,k)
               end do
            end do
         end do
         delp = delp1
         deallocate (delp1)
      endif

      if(.not. ptop_found) then
         call GFIO_GetRealAtt ( fidps, 'ptop',   1, ptop, rc )
         if ( rc /= 0 )  then
            print *,' ',trim(myname),' can not GFIO_GetRealAtt ptop,set to 1.0'
            ptop = ptop55
         endif
      endif

!     In case ps needed in original shape.

      allocate(ps1(im_e,jme_tmp))
      do i =1, im_e/2
       do j = 1, jme_tmp
        ps1(i,j) = ps(i+im_e/2,j)
       end do
      end do

      do i =im_e/2+1, im_e
       do j = 1, jme_tmp
         ps1(i,j) = ps(i-im_e/2,j)
       end do
     end do
!    ps = ps1
     deallocate (ps1)

      if((.not. ps_found) .and. (.not. delp_found)) then
         print *,' **** PS OR SURFP DOES NOT EXIST in ',trim(psFile),' *****' 
         call die(myname,'exiting...')
      endif
      call GFIO_Close ( fidps, rc )
      if ( rc /= 0 )  call die (myname, 'can not GFIO_Close ')

      deallocate(kmVar_e,stat=rc)
      deallocate(lev_e,stat=rc)
      deallocate(lat_e,stat=rc)
      deallocate(lon_e,stat=rc)
      deallocate(hhmmss0,stat=rc)
      deallocate(yyyymmdd0,stat=rc)
      if(allocated(pe_ec))  deallocate(pe_ec,stat=rc)
      if(allocated(gaus_ps))  deallocate(gaus_ps,stat=rc)
      if(allocated(gaus_delp))  deallocate(gaus_delp,stat=rc)

      if ( rc /= 0 )  call die (myname, 'can not deallocate ')
   end subroutine get_surfp

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  get_psfile --- Constructs surface pressure file.
!
! !INTERFACE:
!


   subroutine get_psfile(psFile,surfFile,ndate,nhms,clim_flag,it)
!
! !USES:
!
      Implicit NONE

!
! !INPUT PARAMETERS:

      character(len=*)  :: psFile
      integer           :: ndate,nhms,ihms

! WORK AREAS
      character(len=8)  :: cdate0              ! Date in character form
      character(len=6)  :: cdate              ! Date in character form
      character(len=8)  :: ctime              ! Time in character form
      character(len=4)  :: syyyy
      character(len=2)  :: smm,cmm
      integer           :: i,it
      logical           :: clim_flag

!
! !OUTPUT PARAMETERS:
!
      character(len=*) surfFile

! !DESCRIPTION: This routine constructs the surface file.
!
! !REVISION HISTORY:
!
! Jan 2006 Ravi C. Govindaraju  Initial design and prologue.
!
!EOP
!-------------------------------------------------------------------------

!                 extract the index number to locate the "hdf" tag.
!                ----------------------------------------------------

      i = max(0,index ( psFile, '.', back=.true. ))
                print *,' ',trim(psFile)
!
!                 ----------------------------
!                  Confirm  the "hdf" tag.
!                 ----------------------------


!
!
!                 -------------------------------------------------------
!                   Construct the dataset with the date and time tags.
!                 -------------------------------------------------------
!
!        write(cdate,'(i8)') ndate
         write(cdate0,'(i8)') ndate
         cdate = cdate0(1:6)
         cmm = cdate0(5:6)
!        ihms = nhms /10000
!        write(ctime,'("0",i4)') nhms

         if (clim_flag) then
           surfFile = trim(psFile) // trim(cmm) // 'clm.nc4'
         else
!                   print *,' date,time,ihms ',ndate,nhms,ihms
!                   print *,' cdate,ctime ',cdate,ctime
!          surfFile = trim(psFile) // '.' // trim(cdate) // '_' // trim(ctime) // 'z.nc4'
           surfFile = trim(psFile)
         endif

      print *,'  ',trim(surfFile)
      inquire ( file=trim(surfFile), exist=file_exist )
      if(.not. file_exist) then
         print *,' ***** PSFILE ',trim(surfFile),' ***** DOES NOT EXIST *****'
         call die(myname,'exiting...')
      endif
   end subroutine get_psfile

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_ --- Parses command line 
! 
! !INTERFACE:
!

   subroutine Init_ ( mFiles, nFiles, inFiles, outFile,psFile,resolution,      &
         im, jm, km, nLevs, Levs,kn,ndate,ntime,                  &
         mVars, nVars, inVars, nqaVars,nptVars,nsphVars,nudVars,  &
         nvdVars,nucVars,nvcVars,outVars,qaVars,ptVars,sphVars,   &
         udVars,vdVars,ucVars,vcVars,outPrec,append,shftlon,      &
         shaveFlag,prefProvided,clim_flag,add_delp,inc_hhmmss) 

!
! !USES:
!
      Implicit NONE

!
! !INPUT PARAMETERS: 
!

      integer, intent(in)  :: mFiles  !  Max. number of input files as
                                      !    declared in calling program
      integer, intent(in)  :: mVars   !  Max. number of variables


!
! !OUTPUT PARAMETERS:
!

      integer, intent(out)          :: nFiles     !  Actual number of input files
      character(len=*), intent(out) :: inFiles(:) !  Input file names
      character(len=*), intent(out) :: outFile    !  Output file name 
      character(len=*), intent(out) :: psFile     !  Surface pressure File     
      character(len=*), intent(out) :: append     !  im*jm
      integer  :: inc_hhmmss                      ! increment hours specified from command line


      integer, intent(out)  :: im                 !  zonal dimension
      integer, intent(out)  :: jm                 !  meridional dimension
      integer, intent(out)  :: km                 !  vertical dimension

      real, pointer         :: Levs(:)            ! actual levels
      integer, intent(out)  :: nLevs              ! actual number of levels
      integer, intent(out)  :: kn                 ! number of output vertical levels 
      integer               :: ndate,ntime        ! actual number of levels


      integer,          intent(out) :: nVars      ! Actual number of variables
      character(len=*), intent(out) :: inVars(:)  ! Input  variable names (nVars)
      character(len=*), intent(out) :: outVars(:) ! output variable names (nVars)

      integer,          intent(out) :: nqaVars    ! Actual number of variables
      integer,          intent(out) :: nptVars    ! Actual number of variables
      integer,          intent(out) :: nsphVars   ! Actual number of variables
      integer,          intent(out) :: nudVars    ! Actual number of variables
      integer,          intent(out) :: nvdVars    ! Actual number of variables
      integer,          intent(out) :: nucVars    ! Actual number of variables
      integer,          intent(out) :: nvcVars    ! Actual number of variables

      character(len=*), intent(out) :: qaVars(:)  ! output variable names (nqaVars)
      character(len=*), intent(out) :: ptVars(:)  ! output variable names (nptVars)
      character(len=*), intent(out) :: udVars(:)  ! output variable names (nudVars)
      character(len=*), intent(out) :: vdVars(:)  ! output variable names (nvdVars)
      character(len=*), intent(out) :: ucVars(:)  ! output variable names (nucVars)
      character(len=*), intent(out) :: vcVars(:)  ! output variable names (nvcVars)
      character(len=*), intent(out) :: sphVars(:) ! output variable names (nsphVars)

      integer, intent(out)          :: outPrec    ! Output file precision:
                                                  !  0 = 32 bits,  1 = 64 bits
      logical, intent(out)          :: shftlon    ! Flag to shift Longitude (-180,180)
                                                  !  to (0,360)
      logical, intent(out)          :: shaveFlag  ! Flag to enable shaving of output
      integer, intent(out)          :: prefProvided  ! Provided reference pressure
      logical, intent(out)          :: clim_flag  ! Flag for ps_file 
      logical, intent(out)          :: add_delp  ! Flag for ps_file 
                                                  ! climatology or synoptic  ps file.


! !DESCRIPTION: This routine initializes and parses the command.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
!-------------------------------------------------------------------------

      integer             iarg, argc

      character(len=2048)  argv

      integer, parameter :: mKm = 256  ! max no of levels

      integer i, j, rc
      logical :: debug = .false.
      character(len=10) nLx, nLy
      character (len=1) resolution

      print *
      print *, "-------------------------------------------------------------------"
      print *, "GFIO_remap - Remapping the data.                                   "
      print *, "-------------------------------------------------------------------"
      print *

      argc = command_argument_count()
      if ( argc < 1 ) call usage_()

!  Defaults
!  --------
      nFiles   = 0
      nVars    = 0
      nqaVars  = 0
      nptVars  = 0
      nudVars  = 0
      nvdVars  = 0
      nucVars  = 0
      nvcVars  = 0
      nsphVars = 0
      outFile = 'GFIO_remap.eta.nc4'
      psFile  = 'NONE'
      clim_flag = .false.
      add_delp = .false.
      kn = -1
      shaveFlag = .false.
      prefProvided = 0

      outPrec = 0
      km = -1
      im =  288 
      jm =  181 
      nLx = '288'
      nLy = '181'
      iflag  = 1
      irflag = 2
      inc_hhmmss = 99999999
      append = '288x181'
      ndate      = 999999
      ntime      = 999999
      shftlon    = .false.
      resolution = 'N'
      inVars = ''

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
         else if(index(argv,'-psfile') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, psFile )
         else if(index(argv,'-clim') .gt. 0 ) then
            clim_flag = .true.
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
         else if(index(argv,'-time') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) ntime
         else if(index(argv,'-nlev') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) kn
         else if(index(argv,'-shift') .gt. 0 ) then
            shftlon = .true.
         else if(index(argv,'-like_pt') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, ptVars, nptVars )
         else if(index(argv,'-like_q-agrid') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, qaVars, nqaVars )
         else if(index(argv,'-like_u-dgrid') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, udVars, nudVars )
         else if(index(argv,'-like_sph') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, sphVars, nsphVars )
         else if(index(argv,'-like_v-dgrid') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, vdVars, nvdVars )
         else if(index(argv,'-like_u-cgrid') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, ucVars, nucVars )
         else if(index(argv,'-like_v-cgrid') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mVars, vcVars, nvcVars )
         else if(index(argv,'-res') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) resolution
         else if(index(argv,'-pref') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) prefProvided
            if (prefProvided < 1000) then
               call die (myname, "prefProvided should be in Pascals. Your value seems a bit low. Is it in millibars?")
            end if
         else if(index(argv,'-levels') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            call split_ ( ',', argv, mLevs, cLevs, nLevs )
            if(associated(Levs)) deallocate(Levs)
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
         else if(index(argv,'-debug') .gt. 0 ) then
            debug = .true.
         else if(index(argv,'-shave') .gt. 0 ) then
            shaveFlag = .true.
         else if(index(argv,'-add_delp') .gt. 0 ) then
            add_delp = .true.
         else
            nFiles = nFiles + 1
            inFiles(nFiles) = argv
         end if

      end do

      if ( nFiles /= 1 )  call die (myname, 'program only works for one input file')

      if ( outPrec .eq. 32 ) outPrec = 0   
      if ( outPrec .eq. 64 ) outPrec = 1   

      if(resolution == 'A' .or. resolution == 'a') then
         im = 72
         jm = 46
      endif
      if(resolution == 'B' .or. resolution == 'b') then
         im = 144
         jm = 91
      endif
      if(resolution == 'C' .or. resolution == 'c') then
         im = 288
         jm = 181
      endif
      if(resolution == 'D' .or. resolution == 'd') then
         im = 576
         jm = 361
      endif
      if(resolution == 'E' .or. resolution == 'e') then
         im = 1152
         jm = 721
      endif
      if(resolution == 'F' .or. resolution == 'f') then
         im = 2304
         jm = 1441
      endif

      if(resolution == '1') then
         im =  90
         jm =  46
      endif

      if(resolution == '2') then
         im = 180
         jm =  91
      endif

      if(resolution == '3') then
         im = 360
         jm = 181
      endif

      if(resolution == '4') then
         im = 720
         jm = 361
      endif

      if(resolution == '5') then
         im = 1440
         jm = 721
      endif

      if(resolution == '6') then
         im = 540
         jm = 361
      endif


!  Resolve variable classes
!  ------------------------

      nVars = 0

      if(nqaVars > 0) then
         do jt = 1,nqaVars
            nVars = nVars + 1
            outVars(nVars) = qaVars(jt)
         end do
      endif

      if(nudVars > 0) then
         do jt = 1,nudVars
            nVars = nVars + 1
            outVars(nVars) = udVars(jt)
         end do
      endif

      if(nvdVars > 0) then
         do jt = 1,nvdVars
            nVars = nVars + 1
            outVars(nVars) = vdVars(jt)
         end do
      endif

      if(nucVars > 0) then
         do jt = 1,nucVars
            nVars = nVars + 1
            outVars(nVars) = ucVars(jt)
         end do
      endif

      if(nvcVars > 0) then
         do jt = 1,nvcVars
            nVars = nVars + 1
            outVars(nVars) = vcVars(jt)
         end do
      endif

      if(nptVars > 0) then
         do jt = 1,nptVars
            nVars = nVars + 1
            outVars(nVars) = ptVars(jt)
         end do
      endif

      if(nsphVars > 0) then
         do jt = 1,nsphVars
            nVars = nVars + 1
            outVars(nVars) = sphVars(jt)
         end do
      endif

!......................................................................


      print *, 'Input   Files: ', nFiles
      do i = 1, nFiles
         print *, "               ", trim(inFiles(i))
      end do
!   print *
!   print *, 'Output   File: ', trim(outFile), ', prec = ', outPrec
!   print *

      if ( prefProvided .gt. 0 ) then
         write (*,*)
         write (*,'(X,A,X,I6,X,A)') "Using a provided reference pressure of", prefProvided, "Pascals."
      end if

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
      print *, "   GFIO_remap  Remaps to the user requested resolution. "
      print *
      print *, "SYNOPSIS"
      print *, "   GFIO_remap [options] input_fname"
      print *
      print *, "OPTIONS"
      print *
      print *, "  -o         ofname    output file name (default: GFIO_remap.nc4)"
      print *, "  -prec n    precision: "
      print *, "                    n=0 for 32 bits (default) "
      print *, "                    n=1 for 64 bits"
      print *, "  -shift            Shifts the longitude from (-180,180) to (0,360) or "
      print *, "                    vice-versa  for the output fields."
      print *, "  -like_q-agrid     varn    default (A- grid) "
      print *, "  -like_pt          Vertical interpolation as Potential Temperature "
      print *, "  -like_sph         Vertical interpolation as Specific Humidity     "
      print *, "  -like_u-dgrid     Treat the variables as U in D-grid. "
      print *, "  -like_v-dgrid     Treat the variables as V in D-grid. "
      print *, "  -like_u-cgrid     Treat the variables as U in C-grid (not yet implemented)."
      print *, "  -like_v-cgrid     Treat the variables as V in C-grid (not yet implemented)."
      print *, "  -date      ndate  Date to be intialized for the monthly mean (if not given"
      print *, "                    will be computed)."
      print *, "  -time      ntime  Time to be intialized for the monthly mean (if not given."
      print *, "                    will be computed)."
      print *, "  -inc       hhmmss Output Time Increment (default from the Input file)."
      print *, "  -nlev      kn     Number of output vertical levels (to be interpolated)"
      print *, "  -res       res    The Output horizontal resolution; a   72x46        4x5   "  
      print *, "                                                      b   144x91      2.5x2.5"
      print *, "                                                      c   288x181     1.25x1 "
      print *, "                                                      d   576x361     0.625x0.5"
      print *, "                                                      e  1152x721     0.3125x0.25"

      print *, "                                                      1    90x046      4x4  "
      print *, "                                                      2   180x091      2x2  "
      print *, "                                                      3   360x181      1x1  "
      print *, "                                                      4   720x361    0.5x0.5"
      print *, "                                                      5  1440x721   0.25x0.25"
      print *, "                                                      6   540x361   0.66667x0.5:"   
      print * 
      print *, "  -levels           vertical levels (default: all levels in the input file)"
      print *, "  -psfile    psFile ana.eta file name (Needed only when the delp is missing "
      print *, "                    from the input file -- Provide an eta file where the delp"
      print *, "                    resides.) "
      print *, "  -clim      psFile is a climatology file ( Individual monthly climatology file"
      print *, "                       otherwise individual synoptic file   default) "
      print *, "  -shave            shave bits from output (default: do not shave output)"
      print *, "  -pref      press  integer value in Pascals of reference pressure (default: "
      print *, "                    use ak's and bk's to calculate reference pressure)"
      print *
      print *, "DESCRIPTION"
      print *, "  Remaps the input data to user specified resolution."
      print *
      print *, "   ex:  GFIO_remap.x -o outfile input_file"
      print *, "   ex:  GFIO_remap.x -psfile expno.psfile.yyyy -clim -o output_file input_file"
      print *, "                         mmclm.nc4 will be internally added to the psfile." 
      print *, "   ex:  GFIO_remap.x -psfile expno.psfile -o output_file input_file"
      print *, "                 yymmmdd_hhmmz.nc4 will be internally added to the psfile." 
      print * 

      call die ( myname, 'exiting' )

   end subroutine Usage_

!............................................................................

!  subroutine split_ ( tok, str, mList, List, nList )
!     implicit NONE
!     character(len=1), intent(in)  :: tok  ! delimitter
!     character(len=*), intent(in)  :: str  ! string
!     integer,          intent(in)  :: mList
!     character(len=*), intent(out) :: List(mList)
!     integer,          intent(out) :: nList

!     integer i, l, n, j

!     i = 1
!     l = len_trim(str)
!     print * 

!     call die ( myname, 'exiting' )

!  end subroutine Usage_

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

!   ------------------------------------------------------------------
!BOP
!
! !ROUTINE:  get_pe --- Comptes sigma edges from the input dataset.
!
! !INTERFACE:
!
   subroutine get_pe(im_e,jm_e,km_e,ptop_old,delp,   &
         akap,pe3d_m)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer   :: im_e,jm_e,km_e
      real      :: delp(:,:,:),akap,ptop_old

! !OUTPUT PARAMETERS  (input parameter)

      real      :: pe3d_m(:,:,:)

! WORK AREA
      integer   :: i,j,k

! !DESCRIPTION: This routine initializes and parses the command.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      do j=1,jm_e
         do i=1,im_e
            pe3d_m(i,j,1) = ptop_old
         enddo
      enddo

      do k=1,km_e
         do j=1,jm_e
            do i=1,im_e
               pe3d_m(i,j,k+1) = pe3d_m(i,j,k) + delp(i,j,k)
            enddo
         enddo
      enddo
   end subroutine get_pe
!-------------------------------------------------------------
!BOP
!
! !ROUTINE:  delp32 --- Compute output Pressure deltas.
!
! !INTERFACE: delp32
!

   subroutine delp32(im_e,jm_e,kn,ks32,ak,delp,pe3)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer :: im_e,jm_e,kn,ks32
      real    :: ak(:),pe3(:,:,:),dak

! !OUTPUT PARAMETERS: 

      real    :: delp(:,:,:)

!  WORK AREA

      integer :: i,j,k

! !DESCRIPTION: This routine Computes the output Pressure deltas.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      do j = 1,jm_e
         do k=1,ks32
            dak = ak(k+1) - ak(k)
            do i=1,im_e
               delp(i,j,k) = dak
            enddo
         enddo

         do k=ks32+1,kn
            do i=1,im_e
               delp(i,j,k) = pe3(i,j,k+1) - pe3(i,j,k)
            enddo
         enddo
      end do
   end subroutine delp32
! ---------------------------------------------------------------
!
!BOP
!
! !ROUTINE:  minmaxpe --- Compute min and max for a given variable.
!
! !INTERFACE: minmaxpe
!
   subroutine minmaxpe(pe3d_m,ime,jme,kmre)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:

      real  :: pe3d_m(:,:,:)
      integer :: ime,jme,kmre

! !OUTPUT PARAMETERS: NONE

! WORK AREA PAMETERS:

      real  :: pemin,pemax
      integer :: i,j,k
      integer :: imin_loc,jmin_loc,imax_loc,jmax_loc

! !DESCRIPTION: This routine Computes the min and max for a given variable.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      print *,'minmaxpe: ime,jme,kmre ',ime,jme,kmre
      do  k = 1,kmre
         pemin = 10.e+20
         pemax = -10.e+20
         do  j = 1,jme
            do i = 1,ime
               if(pe3d_m(i,j,k) < pemin) then
                  pemin = pe3d_m(i,j,k)
                  imin_loc = i
                  jmin_loc = j
               endif
               if(pe3d_m(i,j,k) > pemax) then
                  pemax = pe3d_m(i,j,k)
                  imax_loc = i
                  jmax_loc = j
               endif
            end do
         end do
       print *,' k,pemin,pemax ',k,pemin,pemax
      end do
   end subroutine minmaxpe
! ---------------------------------------------------------------
!
!BOP
!
! !ROUTINE:  minmaxps --- Compute min and max for a given variable.
!
! !INTERFACE: minmaxps
!
   subroutine minmaxps(ps,im_e,jm_e,kmre)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:

      real  :: ps(:,:)
      integer :: im_e,jm_e,kmre

! !OUTPUT PARAMETERS: NONE

! WORK AREA PAMETERS:

      real  :: pemin,pemax
      integer :: i,j,k
      integer :: imin_loc,jmin_loc,imax_loc,jmax_loc

! !DESCRIPTION: This routine Computes the min and max for a given variable.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      print *,'minmaxps: im_e,jm_e,kmre ',im_e,jm_e,kmre
      pemin = 10.e+20
      pemax = -10.e+20
      do  j = 1,jm_e
         do i = 1,im_e
            if(ps(i,j) < pemin) then
               pemin = ps(i,j)
               imin_loc = i
               jmin_loc = j
            endif
            if(ps(i,j) > pemax) then
               pemax = ps(i,j)
               imax_loc = i
               jmax_loc = j
            endif
         end do
      end do
       print *,' pemin,pemax ',pemin,pemax
   end subroutine minmaxps
! ----------------------------------------------------------------------------------------

!
!BOP
!
! !ROUTINE:  h_interp --- Interpolate from Input Horizontal resolution to output
!                         resolution.
!
! !INTERFACE: h_interp
!
   subroutine h_interp(outField,im,jm,lat_e2,gaussian,ig,iv,kn,in,jn,ofn)
!
! !USES:
      use m_maph
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      real    :: outField(:,:,:),lat_e2(:)
      integer :: im,jm,gaussian
      integer ig                ! ig=0: pole to pole; ig=1 j=1 is half-dy

! north of south pole
      integer iv                ! iv=0: scalar; iv=1: vector

!
! !OUTPUT PARAMETERS:
      real    :: ofn(:,:,:)
      integer :: in,jn,kn

!  WORK AREA

      real dx1, dx2
      real dy1, dy2, dyr

      real lon1(im+1)
      real lon2(in+1)

! Pole to pole
      real sin1(jm+1)
      real sin2(jn+1)

      integer i, j
      real sinlon(im), coslon(im), cosl5(im), sinl5(im)
      integer imh, k
      double precision pi



! !DESCRIPTION: This routine Computes the min and max for a given variable.
!
! !REVISION HISTORY: 
!
! Jan 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      imh = im/2
      pi = 4.d0 * datan(1.d0)

!     print *,' h_interp: calling get_cose '
      call get_cose(sinlon,coslon,cosl5,sinl5, im,jm)
!     print *,' h_interp: Out from get_cose '

      dx1 = 360./float(im)
      dx2 = 360./float(in)

      dy1 = pi/float(jm-1)
      dy2 = pi/float(jn-1)


!**************
! Map surface pressure
!**************

      do i=1,im+1
         lon1(i) = dx1 * (-0.5 + (i-1) )
      enddo

      print *,' h_interp: gaussian,im,jm,in,jn,kn ',gaussian,im,jm,in,jn,kn

      sin1(   1) = -1.
      sin1(jm+1) =  1.
      if( gaussian /= 0) then

         do j=2,jm
            sin1(j) = sin( -0.5*pi + dy1*(-0.5+float(j-1)) )
         enddo

      else
!       write(6,2000) 
         do j=2,jm
            dyr = lat_e2(j)-lat_e2(j-1)
            dy1 = dyr * (pi/180.0)
            sin1(j) = sin( -0.5*pi + dy1*(-0.5+float(j-1)) )
!         write(6,1000) j,lat_e2(j),lat_e2(j-1),dyr,dy1,sin1(j) 
!1000        format(2x,i4,5(2x,e10.4))
2000        format(/2x,'  j ',2x,'  lat(j)  ',2x,' lat(j-1) ',2x,'dlat(deg) ',2x,'dlat(rad) ',2x,' sin1 ',/)
         enddo
      endif

      do i=1,in+1
         lon2(i) = dx2 * (-0.5 + (i-1) )
      enddo

      sin2(   1) = -1.
      sin2(jn+1) =  1.

      do j=2,jn
         sin2(j) = sin( -0.5*pi + dy2*(-0.5+float(j-1)) )
      enddo


      if(ig >=0 .and. iv > 0) then                                                                                                                            
         if(ig > 0) then
!**************
! Map u-wind
!**************
! ig = 1                   ! data defined "half dy" from pole
            sin1( 1) = -1.
            sin1(jm) =  1.

            sin2( 1) = -1.
            sin2(jn) =  1.


            do j=2,jm-1
               sin1(j) = sin( -0.5*pi + dy1*float(j-1) )
            enddo

            do j=2,jn-1
               sin2(j) = sin( -0.5*pi + dy2*float(j-1) )
            enddo

         else
!**************
! Map v-wind
!**************
! v located half-dx west cell center


            do i=1,im+1
               lon1(i) = dx1 * float(i-2)
            enddo

            do i=1,in+1
               lon2(i) = dx2 * float(i-2)
            enddo

            sin1(   1) = -1.
            sin1(jm+1) =  1.

            sin2(   1) = -1.
            sin2(jn+1) =  1.

            do j=2,jm
               sin1(j) = sin( -0.5*pi + dy1*(-0.5+float(j-1)) )
            enddo

            do j=2,jn
               sin2(j) = sin( -0.5*pi + dy2*(-0.5+float(j-1)) )
            enddo
         endif
      endif


!         print *,' h_interp:im,jm,kn,ig,iv ',im,jm,kn,ig,iv
!         print *,' h_interp:lon1 ',lon1
!         print *,' '
!         print *,' h_interp:in,jn,kn,ig,iv ',in,jn,kn,ig,iv
!         print *,' h_interp:lon2 ',lon2

!          print *,' h_interp: Calling map_a2a'

      call map_a2a(kn, im, jm, lon1, sin1, outField,  &
            in, jn, lon2, sin2, ofn, ig,iv)

!          print *,' h_interp: Out from map_a2a'
   end subroutine h_interp
!
   subroutine get_cose(sinlon,coslon,cosl5,sinl5, im,jm)
      use m_maph
      integer  :: imh,im,jm,i

      real cosp(jm), cose(jm), sine(jm), sinp(jm)
      real coslon(im), sinlon(im),cosl5(im),sinl5(im)
      real dl, dp
      double precision pi, zamda, zam5

      imh = im/2

      pi = 4.d0 * datan(1.d0)
      call setrig(im,jm,dp,dl,cosp,cose,sinp,sine)

!         get D-grid v-wind at poles:

      do i=1,imh
         zam5          = (dble(i-1)-0.5d0) * dl
         cosl5(i)      = -dcos(zam5)
         cosl5(i+imh)  = -cosl5(i)
         sinl5(i)      = -dsin(zam5)
         sinl5(i+imh)  = -sinl5(i)

         zamda         = dble(i-1)*dl
         coslon(i)     = -dcos(zamda)
         coslon(i+imh) = -coslon(i)
         sinlon(i)     = -dsin(zamda)
         sinlon(i+imh) = -sinlon(i)
      enddo
   end subroutine get_cose

! ----------------------------------------------------------------------------------------

!
!BOP
!
! !ROUTINE:  get_latlon --- Creates Longitudes and latitudes.
!
! !INTERFACE: get_latlon
!
   subroutine get_latlon(lon,lat,in_e,jn_e)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer :: in_e,jn_e
      real    :: lon(:),lat(:)
!
! !OUTPUT PARAMETERS:
!     real    :: lon(:),lat(:)

!  WORKING VARIABLES
      real    :: dx,dy
      integer :: i,j

! !DESCRIPTION: This routine Computes the min and max for a given variable.
!
! !REVISION HISTORY: 
!
! Feb 2006 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------


      dx = 360./float(in_e)
      dy = 180.0/float(jn_e-1)

!
!      ----------------------------------------
!        If the input file is GEOS-5,
!        set the longitude from -180 to 180.
!      ----------------------------------------

!     print *,' in_e,jn_e,dx,dy ', in_e,jn_e,dx,dy
      do  i = 2,in_e
!      lon(i) = lon(i-1) + dx
         lon(i) = lon(1) + (i-1) * dx
      end do

      lat(1) = -90.0
      do  j = 2,jn_e
!      lat(j) = lat(j-1) + dy
         lat(j) = lat(1) + (j-1) * dy
      end do
!     print *,' get_latlon: lat ',lat
!     print *,' '
!     print *,' get_latlon: llon'
!     print *,' lon ',lon
   end subroutine get_latlon

! ----------------------------------------------------------------------------------------

!
!BOP
!
! !ROUTINE:  shift_field --- Shifts the field from (-180,180) to (0,360)
!
! !INTERFACE: shift_field
!
   subroutine shift_field(ofn,in_e,jn_e,kn_e)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
      integer       :: in_e,jn_e,kn_e,ind2
      real          :: ofn(:,:,:)
!
! !OUTPUT PARAMETERS:
!          ofn
!  WORKING VARIABLES
      real    :: array(in_e,jn_e)
      integer :: i,j,k

! !DESCRIPTION: This routine Shifts the array from (-180,180) to (0,360).
!
! !REVISION HISTORY: 
!
! Feb 20100113 Ravi C. Govindaraju Initial design and prologue.
!
!EOP
! ---------------------------------------------------------------

      ind2 = in_e/2
      do  k = 1,kn_e
         array(1:ind2,:) = ofn(ind2+1:in_e,:,k)
         array(ind2+1:in_e,:) = ofn(1:ind2,:,k)
         ofn(:,:,k) = array
      end do
   end subroutine shift_field
!
! -------------------------------------------------------------------------------------
!BOP
!
!
! !ROUTINE:  z_interp --- Remapping the data.
!
! !INTERFACE:
!
   subroutine z_interp(outVar,inField,pe3d_m,ptop,im_e,jm_e,km_e, &
         qaflag,ptflag,sphflag,udflag,vdflag,ucflag,vcflag, &
         ptop32,ks32,ak32,bk32,pe3,in,jn,kn, outField)
!
! !USES:
!
      use m_set_eta, only: set_eta
      use m_mapz

      Implicit NONE
!
! !INPUT PARAMETERS:

      real              :: pe3(:,:,:)                ! Pressure edges.
      real              :: ak32(:)                   ! vertical grid a coefficients
      real              :: bk32(:)                   ! vertical grid a coefficients
      real              :: pe3d_m(:,:,:)             ! Input Mid level Pressure edges 
      real              :: inField(:,:,:)            ! Input Field
      real              :: ptop,ptop32               ! Input ptop and the output ptop
      integer           :: im_e,jm_e,km_e,in,jn,kn   ! Input and output array boundaries.
      integer           :: ks32
      character (len=*) :: outVar                    ! Requested Variable name
      logical           :: qaflag,ptflag,udflag,vdflag,ucflag,vcflag
      logical           :: sphflag

!
! !OUTPUT PARAMETERS:
!
      real              :: outField(:,:,:)           !  Vertically interpolated field.

!  WORKING ARRAYS

      real              :: pe0(im_e,km_e+1)    ! Input Pressure edges (im,kn)
      real              :: pe1(im_e,km_e+1)    ! Input Pressure edges (im,kn)
      real              :: pe2(im_e,kn+1)      ! Input Pressure edges (im,kn)
      real              :: pe4(im_e,kn+1)      ! output Pressure edges (im,kn)
      real              :: pk0(im_e,km_e+1)    ! Input p ** kappa  (im,km)
      real              :: pk1(im_e,kn+1)      ! Output p ** kappa (im,kn)
      real              :: rair,cpair,akap,big ! Constants
      real              :: ple
      integer           :: i,j,k               ! loop indexes
      double precision pi

      rair = 287.04
      cpair = 1004.64
      akap = rair/cpair
      big  = 1.e+10
      pi = 4.d0 * datan(1.d0)

      if ( outVar == 'DELP' .or. outVar == 'delp' ) then
         call delp32(im_e,jm_e,kn,ks32,ak32,outField,pe3)
!                 print *,' outVar ',outVar
!                 call minmaxpe(outField,im_e,jm_e,kn)
      else                                                   ! non-DELP
!                print *,' outVar ',outVar
         outField = 0.0
         do  j = 1,jm_e
            do  k = 1,km_e+1
               do i = 1,im_e
                  pe1(i,k) =  pe3d_m(i,j,k)
                  pr = pe3d_m(i,j,k)
                  pk0(i,k) = pr ** akap
               end do
            end do
!!!               print *,' z_interp: we are here'
            do  k = 1,kn+1
               do i = 1,im_e 
                  pe2(i,k) =  pe3(i,j,k)
                  pr  = pe3(i,j,k)
                  pk1(i,k) = pr ** akap
               end do
            end do

            do  i = 1,im_e
               pe0(i,1) = pe1(i,1)
               pe4(i,1) = ptop32
            end do
!!!               print *,' z_interp: pe4 computed'

            if(ks32 .ne. 0) then
               do  k = 2,ks32+1
                  ple = ak32(k)
                  do  i = 1,im_e
                     pe4(i,k) = ple
                  end do
               end do
            endif        !ks32
!!!               print *,' z_interp: ks32 loop compleated'


            if((.not. udflag) .and. (.not.vdflag) .and. (.not. ptflag) &
                  .and. (.not. sphflag)) then

!!!                print *,' z_interp: calling map1_ppm'
               call map1_ppm( km_e,   pk0,   inField,                  &
                     kn,     pk1,   outField,                 &
                     im_e, 1, im_e, j, 1, jm_e, 1, 7, undef)
!!!                print *,' z_interp: Out from map1_ppm'
            endif
! map pt
            if(ptflag) then
               call map1_ppm( km_e,   pk0,   inField,                  &
                     kn,     pk1,   outField,                 &
                     im_e, 1, im_e, j, 1, jm_e, 1, 7, undef)
               ig = 0
               is = 0
               kn_e = kn

! Fix out of bound (beyond original ptop)

!                     do k= ks32,1,-1
!                       do i=1,im_e
!                        if( outField(i,j,k) .gt. big ) then
!                          outField(i,j,k) = outField(i,j,k+1)
!                        endif
!                       enddo
!                     enddo
            endif

! map sphu
            if(sphflag) then
               call map1_ppm( km_e,   pe1,   inField,                  &
                     kn,     pe2,   outField,                 &
                     im_e, 1, im_e, j, 1, jm_e, 0, 7, undef)
               ig = 0
               is = 0
               kn_e = kn

! Fix out of bound (beyond original ptop)

!                     do k= ks32,1,-1
!                       do i=1,im_e
!                        if( outField(i,j,k) .gt. big ) then
!                          outField(i,j,k) = outField(i,j,k+1)
!                        endif
!                       enddo
!                     enddo

! For like specific humidity, fix values that go negative by setting to level
! above.

               do k= 1,kn
                  do i=1,im_e
                     if( outField(i,j,k) .lt. zero ) then
                        !print *,'Found less than zero constituent!'
                        !print *,'outVar: ', trim(outVar)
                        !print *,'i,j,k: ', i,j,k
                        !print *,'outField(i,j,k-1): ',outField(i,j,k-1)
                        !print *,'outField(i,j,k  ): ',outField(i,j,k)
                        !print *,''
                        !print *,'Setting value to previous value'
                        if (k > 1) then
                           outField(i,j,k) = outField(i,j,k-1)
                        else
                           outField(i,j,k) = 0.
                        endif
                        !print *,'outField(i,j,k-2): ',outField(i,j,k-2)
                        !print *,'outField(i,j,k-1): ',outField(i,j,k-1)
                        !print *,'outField(i,j,k  ): ',outField(i,j,k)
                        !if (k < kn) then
                           !print *,'outField(i,j,k+1): ',outField(i,j,k+1)
                        !endif
                        !print *,''
                        !print *,''
                     endif
                  enddo
               enddo

            endif

! map u

            if((udflag) .or. (vdflag)) then
               if(j .ne. 1) then

                  do k=2,km_e+1
                     do i=1,im_e
                        pe0(i,k) = 0.5*(pe1(i,k)+pe3d_m(i,j-1,k))
                     enddo
                  enddo

                  do k=ks32+2,kn+1
                     bkh = 0.5*bk32(k)
                     do i=1,im_e
                        pe4(i,k) = ak32(k) + bkh*(pe1(i,km_e+1)+pe3d_m(i,j-1,km_e+1))
                     enddo
                  enddo

                  call map1_ppm( km_e,   pe0,   inField,                  &
                        kn,     pe4,   outField,                 &
                        im_e, 1, im_e, j, 1, jm_e, 1, 7, undef)

! Fix out of bound (beyond original ptop)

!                     do k= ks32,1,-1
!                       do i=1,im_e
!                        if( outField(i,j,k) .gt. big ) then
!                          outField(i,j,k) = outField(i,j,k+1)
!                        endif
!                       enddo
!                     enddo

               endif              ! j = 1  U-wind
               ig = 1
               is = 1
               kn_e = kn
            endif               ! Uwind

! map v
            if(j .ne. 1 .and. j .ne. jm_e) then
               if(vdflag) then

                  do k=2,km_e+1
                     do i=2,im_e
                        pe0(i,k) = 0.5*(pe1(i,k)+pe1(i-1,k))
                     enddo
                  enddo

!      i=1
                  do k=2,km_e+1
                     pe0(1,k) = 0.5*(pe1(1,k)+pe1(im_e,k))
                  enddo

                  do k=ks32+2,kn+1
                     do i=2,im_e
                        pe4(i,k) = 0.5*(pe2(i,k)+pe2(i-1,k))
                     enddo
                  enddo
! i=1
                  do k=ks32+2,kn+1
                     pe4(1,k) = 0.5*(pe2(1,k)+pe2(im_e,k))
                  enddo

                  call map1_ppm( km_e,   pe0,   inField,                      &
                        kn,     pe4,   outField,                     &
                        im_e, 1, im_e, j, 1, jm_e, 1, 7, undef)
! Fix out of bound (beyond original ptop)

!                      do k=ks32,1,-1
!                       do i=1,im_e
!                        if( outField(i,j,k) .gt. big ) then
!                          outField(i,j,k) = outField(i,j,k+1)
!                        endif
!                       enddo
!                      enddo

               endif                      ! Vwind
               ig = 0
               is = 1
               kn_e = kn
            endif                        ! j /= 1

! Fix out of bound (beyond original ptop)

            do k=ks32,1,-1
               do i=1,im_e
                  if( outField(i,j,k) .gt. big ) then
                     outField(i,j,k) = outField(i,j,k+1)
                  endif
               enddo
            enddo
         end do                         ! j loop
      endif                           ! delp

   end subroutine z_interp

!
!  X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
!
   SUBROUTINE pole_tq (nlons,nlats,nlevs,f,ns)
!
! Specify pole value of scalar fields (e.g., t, q) as mean along 
! first non-pole grid latitude.
!
      implicit none
!
! input      
      integer :: nlons
      integer :: nlats
      integer :: nlevs
      integer :: ns  ! 1 for north pole, =-1 for south pole, =0 for both
                     ! both input and output
      real    :: f(nlons,nlats,nlevs)     
! local
      integer :: i, j, k
      real    :: fsum, fav
!
! Loop over levels 
      do k=1,nlevs
! 
         if (ns <= 0) then  ! Fill south pole
            j=1
            fsum=0.
            do i=1,nlons
               fsum=fsum+f(i,j+1,k)
            enddo
            fav=fsum/nlons
            do i=1,nlons
               f(i,j,k)=fav
            enddo
         endif
!
         if (ns >= 0) then  ! Fill north pole
            j=nlats
            fsum=0.
            do i=1,nlons
               fsum=fsum+f(i,j-1,k)
            enddo
            fav=fsum/nlons
            do i=1,nlons
               f(i,j,k)=fav
            enddo
         endif
!          
      enddo  ! loop over levels
!
   end subroutine pole_tq
!      
!
!  X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
!
   SUBROUTINE pole_uv (nlons,nlats,nlevs,u,v,ns)
!
! Specify wind field at the pole as equal to the zonal wave-number one 
! component of the field at the first non-pole grid latitude. u and v 
! components must be same amplitude with a phase shift of pi
!
      implicit none
!
!
! input      
      integer :: nlons
      integer :: nlats
      integer :: nlevs
      integer :: ns  ! 1 for north pole, =-1 for south pole, =0 for both
                     ! both input and output
      real    :: u(nlons,nlats,nlevs)     
      real    :: v(nlons,nlats,nlevs)     
! local
      integer :: i, j, k
      real    :: twopi, pifac, a
      real    :: pcoslon(nlons)
      real    :: psinlon(nlons)
!
! The indexes for the spectral coefficients are: 
! (1) levels, (2) real or imaginary, (3) S or N pole, u or v
      real    :: coef(nlevs,2,2) ! spectral coefficients
      real    :: coef1, coef2
      real    :: fac
!
! Compute cos and sine of each longitude (for the purpose here, it does
! not matter what the actual starting longitude is). 
      twopi=8.*atan(1.)
      pifac=twopi/nlons
      do i=1,nlons
         a=pifac*(i-1)
         pcoslon(i)=cos(a)
         psinlon(i)=sin(a)
      enddo
!
      if (ns <= 0) then  ! compute values for south pole 
!
! compute Fourier coef for zonal wavenumber 1 along latitude next to pole 
         j=2
         coef(:,:,:)=0.
         do i=1,nlons
            do k=1,nlevs
               coef(k,1,1)=coef(k,1,1)+pcoslon(i)*u(i,j,k)
               coef(k,2,1)=coef(k,2,1)-psinlon(i)*u(i,j,k)
               coef(k,1,2)=coef(k,1,2)+pcoslon(i)*v(i,j,k)
               coef(k,2,2)=coef(k,2,2)-psinlon(i)*v(i,j,k)
            enddo
         enddo
         fac=2./nlons  ! factor of 2 because end result is 2*Real Part  
         coef(:,:,:)=fac*coef(:,:,:)
!
! Average the coefficients of u estimated from the u field directly 
! and the estimate from the v field using coef(u) = i*coef(v) for S.H.
! (and reversed sign for N.H.)
         do k=1,nlevs
            coef1=0.5*(coef(k,1,1)-coef(k,2,2))
            coef2=0.5*(coef(k,2,1)+coef(k,1,2))      
!
! Copy the averaged estimates for the u coefficients
            coef(k,1,1)=coef1
            coef(k,2,1)=coef2
!
! Specify the coefficients for v using coef(v) = -i*coef(u) for S.H.
! (and reversed sign for N.H.)
            coef(k,1,2)= coef2
            coef(k,2,2)=-coef1
         enddo
!
! Construct wind at south
         j=1
         do i=1,nlons
            do k=1,nlevs
               u(i,j,k)=coef(k,1,1)*pcoslon(i)-coef(k,2,1)*psinlon(i)
               v(i,j,k)=coef(k,1,2)*pcoslon(i)-coef(k,2,2)*psinlon(i)
            enddo
         enddo
!
      endif   ! test on whether to do south pole requested
!
      if (ns >= 0) then  ! compute values for north pole 
!
! compute Fourier coef for zonal wavenumber 1 along latitude next to pole 
         j=nlats-1
         coef(:,:,:)=0.
         do i=1,nlons
            do k=1,nlevs
               coef(k,1,1)=coef(k,1,1)+pcoslon(i)*u(i,j,k)
               coef(k,2,1)=coef(k,2,1)-psinlon(i)*u(i,j,k)
               coef(k,1,2)=coef(k,1,2)+pcoslon(i)*v(i,j,k)
               coef(k,2,2)=coef(k,2,2)-psinlon(i)*v(i,j,k)
            enddo
         enddo
         fac=2./nlons  ! factor of 2 because end result is 2*Real Part  
         coef(:,:,:)=fac*coef(:,:,:)
!
! Average the coefficients of u estimated from the u field directly 
! and the estimate from the v field using coef(u) = -i*coef(v) for N.H.
         do k=1,nlevs
            coef1=0.5*(coef(k,1,1)+coef(k,2,2))
            coef2=0.5*(coef(k,2,1)-coef(k,1,2))      
!
! Copy the averaged estimates for the u coefficients
            coef(k,1,1)=coef1
            coef(k,2,1)=coef2
!
! Specify the coefficients for v using coef(v) = i*coef(u) for N.H.
            coef(k,1,2)=-coef2
            coef(k,2,2)= coef1
         enddo
!
! Construct wind at north pole
         j=nlats
         do i=1,nlons
            do k=1,nlevs
               u(i,j,k)=coef(k,1,1)*pcoslon(i)-coef(k,2,1)*psinlon(i)
               v(i,j,k)=coef(k,1,2)*pcoslon(i)-coef(k,2,2)*psinlon(i)
            enddo
         enddo
!
      endif   ! test on whether to do north pole requested
!  
   end subroutine pole_uv
!
!      
   subroutine Shave_field(ofn,in_e,jn_e,kn_r,undef)
      integer          :: in_e,jn_e,kn_r,k,has_undef,xbits,rc
      real             :: ofn(:,:,:),x(in_e,jn_e)
      real *4          :: xr(in_e,jn_e),undef_
      real, OPTIONAL, intent(in) :: undef    ! missing value
      integer, external :: ShaveMantissa32



      xbits = 14
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
end Program GFIO_remap
