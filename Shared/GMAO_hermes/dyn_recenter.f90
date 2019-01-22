      program dyn_recenter

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dyn_recenter: recenters dyn-vector
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use m_dyn, only: dyn_init
      use m_dyn, only: dyn_getdim
      use m_dyn, only: dyn_get
      use m_dyn, only: dyn_put
      use m_dyn, only: dyn_stat
      use m_dyn, only: dyn_vect
      use m_dyn, only: dyn_clean

      use m_mapz, only : z_map
      use m_maph, only : h_map
      use m_maph_pert, only: h_map_pert
      use m_topo_remap, only: dyn_topo_remap

      use m_set_eta, only: set_eta
      use m_inpak90
      use m_stdio, only : stdout,stderr

      implicit NONE

! !DESCRIPTION: Used in context of ensemble DAS to recenter dyn-vector
!               around desired mean. That is, assuming the ensemble mean
!               is x_m, and a desired center mean is x_a, this program
!               reads multiple members x_e of an ensemble of dyn-vectors
!               and calculates: x_e(i) := x_e(i) - x_m + x_a, for ensemble
!               member i.
!
!               When additive inflation is applied the result is:
!               x_e(i) := x_e(i) + x_p(i) - x_m + x_a, when x_p(i) represents
!               the inflating perturbation.
!
! !REVISION HISTORY:
!
!  30sep2011   Todling    Initial code
!  11nov2011   Todling    Handle variable resolution
!  27jan2012   Todling    Add relaxation to central analysis above given level
!  21apr2012   Todling    Add capability to apply additive inflation
!  14may2012   ElAkkraoui Modify damping of additive inflation and avoid inflating ozone
!  06nov2012   Todling    Move threshold for additive pert  from 0.1 mb to 10 mb
!  10feb2013   Todling    Handle special case when ensemble mean resolution
!                         differs from resolution of members
!  24feb2013   Todling    Implement two strategies for remapping:
!                          (i)  remaps both ensmean and member to central topo
!                          (ii) remaps both ensmean and central to member topo
!  25feb2013   ElAkkraoui/RT  Reset threshold of inf to mesh top of blending
!  01Jul2015   Todling    Revisit handling of tracers
!  16Jul2015   Todling    Revisit handling of surface pressure
!  01Jan2017   MJ Kim     Brute-force defense against negative q
!  09Apr2018   Todling    Add vertically-varying additive inflation mechanism
!  11Apr2018   Todling    Additive inflation only applied over ocean for ps/delp
!  09May2018   Todling    Ability to vertically interpolate central ana to mem res
!
! !See Also:
!
!    dyn_inflate.x - program to estimate vertically varying inflation coeffs
!
! !Remarks:
!   1) For the purpose of diagnostics it seems more appropriate to remap
!      to the central topography; but for the purpose of cycling it seems
!      the proper thing to do is to remap to the member's topography. Recall
!      that to construct the IAU increment, the analysis is remapped to the 
!      background topography, since for equal analysis and gcm resolutions
!      there is no remapping we better retain the topography of each member.
!      Another way of saying this is: it's pointless to remap to a topography
!      (that of the central) not related to that of the member (gcm)
!      integrations.
!
!-------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname = 'dyn_recenter'

!     File names
!     ----------
      integer, parameter :: MFILES = 3   ! at least 3 files as input
      character(len=255) :: files(MFILES)! hold names of files involved

      character(len=255) :: dyn_inflate  ! file with pert for additive inflation
      character(len=255) :: dyn_out      ! difference output file name


!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) x_e  ! ensemble member
      type(dyn_vect) x_m  ! ensemble mean/new mean
      type(dyn_vect) x_x  ! extra type used when interpolation needed
      type(dyn_vect) x_v  ! auxiliar vector for dealing w/ vertical interpolation

!     Locals
!     ------
      integer, parameter :: READ_ONLY = 1
      integer nfiles
      integer fid, nvars, ngatts
      integer rc
      integer ntimes, k, freq, nymd, nhms, nymd0, nhms0
      integer im, jm, km, lm, dyntype
      integer imm, jmm, kmm, lmm
      integer ks
      integer lm_mean,lm_pert,lm_central
      real    pabove,pbelow,pkthresh,alpha
      real(8) ptop,pint
      real(8),allocatable:: ak(:),bk(:)
      real,allocatable:: phis(:,:)
      real,pointer:: frocean(:,:)
      real,pointer:: ainf(:,:)
      real :: ainf_ps, ainf_ts
      logical remap2central,remap2member
      logical damp
      logical verbose
      logical gotphis
      logical pncf
      logical ocean_only
      character(len=50), allocatable :: vars(:)
      character(len=30) :: remapinfo
      
!  Initialize
!  ----------     
   call Init_ ( dyntype, mfiles, files, damp, remap2central, remap2member, pkthresh, alpha, pncf, verbose )

!  Determine how many time levels in file
!  --------------------------------------
   call GFIO_Open ( files(1), READ_ONLY, fid, rc )
   if ( rc .ne. 0 ) then
      call die(myname,'cannot open GFIO file '//trim(files(1)))
   end if
   call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars, ngatts, rc)
   if ( rc .ne. 0 ) then
      call die(myname,'problems getting dimensions' )
   end if
   call GFIO_Close ( fid, rc )

   if (ntimes>1) then
      call die(myname,'cannot handle files with multiple times')
   endif
   call dyn_getdim ( files(1), im, jm, km, lm, rc ) ! as foolish as it seems this gives slightly diff info than what's above
      
!  Get first dyn-vector (ensemble member)
!  --------------------------------------
   call dyn_get ( files(1), nymd, nhms, x_e, rc, timidx=1, freq=freq, vectype=dyntype )
   if ( rc .ne. 0 ) then
      call die(myname,'cannot read ensemble member file')
   else
      print *, trim(myname), ': read ens member ', trim(files(1))
   end if

   allocate(phis(x_e%grid%im,x_e%grid%jm))
   gotphis = getphis_(phis,remapinfo)
   imm=x_e%grid%im
   jmm=x_e%grid%jm
   kmm=x_e%grid%km
   lmm=x_e%grid%lm
   lm_mean=lmm

   allocate(frocean(x_e%grid%im,x_e%grid%jm))
   frocean=x_e%frocean

!  Get original (ensemble) mean
!  ----------------------------
   if ( files(2)/='NONE' ) then

      call dyn_getdim ( files(2), imm, jmm, kmm, lmm, rc )
      if(km/=kmm) then ! ignore diff in lm for now
         print *, trim(myname), ': km/lm members     = ', km ,lm
         print *, trim(myname), ': km/lm central mean= ', kmm,lmm
         call die(myname,'inconsistent km/lm')
      endif
      lm_mean=lmm

      if(im/=imm.or.jm/=jmm) then ! handle case when resolution of ensemble mean
                                  ! differs from members; this only happens in dual
                                  ! resoluton and when making ensemble mean equal central
                                  ! for blending purposes

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

!        Read full ensemble mean
!        -----------------------
         call dyn_get ( files(2), nymd, nhms, x_x, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         else
            print *, trim(myname), ': read ens mean ', trim(files(2))
         end if

!        Interpolate to required resolution
!        ----------------------------------
         x_m%grid%lm = min(lm_mean,lm) ! only interp minimal set of fields (others are zero)
         call h_map ( x_x, x_m, rc, lwifile='NONE', dgrid=.false. )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_m )
                   print *, 'h_map error code = ', rc
                   call die(myname,' failed in h_map')
              else
                   call dyn_clean ( x_x )
              endif
         x_m%grid%lm = lm ! reset lm-dim(x_m)
         print*, myname, ': interpolated ensemble mean analysis to member resolution'
         print*, myname, ': from ', imm, 'x', jmm, ' to ', im, 'x', jm
         print*, myname, ': mean lm= ', lm_mean, ', member lm=', lm

      else

         call dyn_get ( files(2), nymd, nhms, x_m, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read ensemble mean file')
         else
            print *, trim(myname), ': read ens mean ', trim(files(2))
         end if

      endif

!     If so, remap whatever appropriate
!     ---------------------------------
      if (gotphis) then
          if (remap2central) then
             if (trim(files(2))/=trim(files(3))) then
                call dyn_topo_remap ( x_m, phis, dyntype, info=rc )
                if(rc==0) print*, myname, ': ensmean ', trim(remapinfo)
             endif
             call dyn_topo_remap ( x_e, phis, dyntype, info=rc )
             if(rc==0) print*, myname, ': member ', trim(remapinfo)
          endif
          if (remap2member) then
              call dyn_topo_remap ( x_m, phis, dyntype, info=rc )
              if(rc==0) print*, myname, ': ensmean ', trim(remapinfo)
          endif
      endif

!     Remove original mean from member
!     --------------------------------
      call my_saxpy_(.false.,x_e,x_m,x_e)

!     Clean up mean
!     -------------
      call dyn_clean (x_m) 
   endif

!  If so, apply additive inflation (x_m below stores x_p=perturbation)
!  -------------------------------------------------------------------
   if ( trim(dyn_inflate) .ne. 'NONE' ) then

!     Get mean to recenter ensemble around
!     -------------------------------------
      call dyn_getdim ( trim(dyn_inflate), imm, jmm, kmm, lmm, rc )
      if(km/=kmm) then ! ignore diff in lm for now
         print *, trim(myname), ': km/lm members     = ', km ,lm
         print *, trim(myname), ': km/lm perturbation= ', kmm,lmm
         call die(myname,'inconsistent km/lm')
      endif
      lm_pert=lmm

      if(im/=imm.or.jm/=jmm) then
 
!        Read full resolution perturbation
!        ---------------------------------
         call dyn_get ( trim(dyn_inflate), nymd0, nhms0, x_x, rc, timidx=1, freq=freq, vectype=dyntype, pncf=pncf )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         else
            print *, trim(myname), ': read ens pert ', trim(dyn_inflate)
         end if

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, x_x%grid%lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_e)')
              endif
   
!        Interpolate to required resolution
!        ----------------------------------
!_RT     x_m%grid%lm=min(lm_pert,lm) ! only interp minimal set of fields (others are zero) 
         call h_map_pert ( x_x, x_m, rc )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_m )
                   print *, 'h_map error code = ', rc
                   call die(myname,' failed in h_map')
              else
                   call dyn_clean ( x_x )
              endif
!_RT     x_m%grid%lm = lm ! reset lm-dim(x_m)
         print*, myname, ': interpolated additive perturbation to member resolution'
         print*, myname, ': from ', imm, 'x', jmm, ' to ', im, 'x', jm

      else

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( imm, jmm, kmm, lm_pert, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

!_RT     x_m%grid%lm = lm_pert ! read only q variables in file
         call dyn_get ( trim(dyn_inflate), nymd0, nhms0, x_m, rc, timidx=1, freq=freq, vectype=dyntype, pncf=pncf )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read inflating perturbation file')
         else
            print *, trim(myname), ': read ens pert ', trim(dyn_inflate)
         end if
!_RT     x_m%grid%lm = lm ! reset number of q variables to total dim

      endif

!     Add inflating perturbation: x_m=x_p inflating vector
!     --------------------------
      if(ocean_only) then
         call my_sscal_(alpha,ainf,ainf_ps,ainf_ts,x_m,omask=frocean)
      else
         call my_sscal_(alpha,ainf,ainf_ps,ainf_ts,x_m)
      endif
      call my_saxpy_(.true.,x_m,x_e,x_e,pklim=pkthresh) ! do not inflate above pkthresh   

!     Clean up mean
!     -------------
      call dyn_clean (x_m) 

   endif ! <inflation>

!  Get mean to recenter ensemble around
!  -------------------------------------
   if ( files(3)/='NONE') then
      call dyn_getdim ( files(3), imm, jmm, kmm, lmm, rc )
      lm_central=lmm

      if(im/=imm.or.jm/=jmm.or.km/=kmm) then

!        If needed, start by vertically interpolating central analysis down to member vertical levels
!        --------------------------------------------------------------------------------------------
         if (km==kmm) then

!            Read full resolution central analysis
!            -------------------------------------
             call dyn_get ( files(3), nymd, nhms, x_x, rc, timidx=1, freq=freq, vectype=dyntype )
             if ( rc .ne. 0 ) then
                call die(myname,'cannot read new mean file')
             else
                print *, trim(myname), ': read central ', trim(files(3))
             end if

         else

!            Read full resolution central analysis
!            -------------------------------------
             call dyn_get ( files(3), nymd, nhms, x_v, rc, timidx=1, freq=freq, vectype=dyntype )
             if ( rc .ne. 0 ) then
               call die(myname,'cannot read new mean file')
             else
               print *, trim(myname), ': read central ', trim(files(3))
             end if
             print *, trim(myname), ': Vertically interpolating from km= ', x_v%grid%km, ' to km= ', km
 
!            Initialize dimension of vertically interpolated vector
!            -------------------------------------------------------
             allocate(ak(km+1),bk(km+1))
             call set_eta ( km,ks,ptop,pint,ak,bk )
                if (any(abs(ak-x_e%grid%ak)>1.e-10).or.any(abs(bk-x_e%grid%bk)>1.e-10)) then
                  call die(myname,'inconsistent ak/bk')
                endif
             call dyn_init ( x_v%grid%im, x_v%grid%jm, km, x_v%grid%lm, x_x, rc, vectype=dyntype, ks=ks, ak=ak, bk=bk )
                if ( rc/=0 ) then
                   call die(myname,': Error initializing dyn vector(x_x)')
                endif
             deallocate(ak,bk)

!            Map to desired resolution
!            -------------------------
             call z_map ( x_v, x_x, rc, verbose=verbose, force=.false. )
               if (rc<0) then
                 call die(myname,': vertical interpolation failed')
              endif

!            Clean up
!            --------
             call dyn_clean ( x_v )

         endif

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, x_x%grid%lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

!        Interpolate to required resolution
!        ----------------------------------
!_RT     x_m%grid%lm = min(lm_central,lm) ! fix lm of interpolated fields 
!_RT     x_x%grid%lm = x_m%grid%lm        ! fix lm of input fields
         call h_map ( x_x, x_m, rc, lwifile='NONE', dgrid=.false. )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_m )
                   print *, 'h_map error code = ', rc
                   call die(myname,' failed in h_map')
              else
                   call dyn_clean ( x_x )
              endif
         x_m%grid%lm = lm ! reset lm_dim(x_m)
         print*, myname, ': interpolated central analysis to member resolution'
         print*, myname, ': from ', imm, 'x', jmm, ' to ', im, 'x', jm


      else ! Resolution of members is consistent w/ that of central analysis
           ! Simply read in mean analysis

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( imm, jmm, kmm, lm_central, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

!        x_m%grid%lm = lm_central
         call dyn_get ( files(3), nymd, nhms, x_m, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if
!        x_m%grid%lm = lm

      endif

!     If so, remap central to member
!     ------------------------------
      if (gotphis .and. remap2member) then
          call dyn_topo_remap ( x_m, phis, dyntype, info=rc )
          if(rc==0) print*, myname, ': central ', trim(remapinfo)
      endif

!     Add new mean to member
!     ----------------------
      call my_saxpy_(.true.,x_e,x_m,x_e)

!     Clean up mean
!     -------------
      call dyn_clean (x_m) 

   endif

!  When requested, summarize results
!  ---------------------------------
   if (verbose) then
       call dyn_stat(6, x_e, rc)
       if ( rc .ne. 0 ) then
          call die(myname,'cannot process dyn_stat')
       end if
   end if

!  If requested write *.hdf file with a header from dyn(1)
!  -------------
   if ( trim(dyn_out) .ne. 'NONE' ) then
        call dyn_put ( trim(dyn_out), nymd, nhms, 0, x_e, rc, freq=freq, vectype=dyntype )
   else ! by default: overwrite input files
        call dyn_put ( trim(files(1)), nymd, nhms, 0, x_e, rc, freq=freq, vectype=dyntype )
   endif 

!  Clean up
!  --------
   if(associated(ainf)) deallocate(ainf)
   if(allocated(phis)) deallocate(phis)
   call dyn_clean ( x_e )

!  All done
!  --------
   close(999)
   open (999,file='DYNRECENTER_EGRESS',form='formatted')
   close(999)
   call exit(0)

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn_recenter
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( dyntype, mfiles, files, damp, remap2central, remap2member, &
                         pkthresh, alpha, pncf, verbose )

      implicit NONE

      integer,       intent(out) :: dyntype ! 4=geos4, 5=geos5
      integer,       intent(in)  :: mfiles  ! max. number of eta files
                                            ! dynamics file names (eta)
      character*255, intent(out) :: files(mfiles) 
      logical, intent(out) :: damp
      logical, intent(out) :: remap2central, remap2member
      logical, intent(out) :: verbose
      real   , intent(out) :: pkthresh      ! threashold pressure above which
                                            !   no additive inflation is applied
      real   , intent(out) :: alpha         ! multiplicative parameter to scale
                                            !   perturbations used for inflation
      logical, intent(out) :: pncf          ! perturbation is non-compliant dyn-field
      
!
! !REVISION HISTORY:
!    
!  30sep2011   Todling    Initial code
!  21Apr2012   Todling    Add opt for additive inflation
!  22Apr2012   Todling    Add opt for multiplicative factor
!  24Feb2013   Todling    Add opt to bypass remapping
!  03Apr2017   Todling    RC-opt; allow for vertically varying add-infl coeffs
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc
      integer irow,nlevs, ier, iv, nivars
      character(len=255) :: argv
      character(len=255) :: token
      character(len=255) :: RCfile
      character(len=255) :: tablename
      logical  doremap

      RCfile = 'NONE'
      files = 'NONE'
      dyn_inflate = 'NONE'
      dyn_out = 'NONE'
      dyntype  = 4        ! default: GEOS-4 files
      pabove = 100.0      ! 1-mb lower limit for additive inflation (above: no inflation)
      pbelow = 500.0      ! 5-mb upper limit for additive inflation (below: full inflation)
      pkthresh = pabove   ! default: ignore additive inflation above pkthresh (Pa)
      alpha   = -999.0    ! default: do not apply multiplicative factor
      damp    = .false.
      remap2central = .false. ! remap ensmean and member to central topography
      remap2member  = .true.  ! remap ensmean and central to member topography
      doremap = .true.
      pncf = .false.
      verbose = .false.
      ocean_only = .false.

      print *
      print *, '     -------------------------------------'
      print *, '     dyn_recenter - recenter ensemble mean'
      print *, '     -------------------------------------'
      print *

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )
         
         select case (argv)
           case ("-g5")
             dyntype = 5
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, dyn_out )
           case ("-a")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) alpha
           case ("-inflate")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, dyn_inflate )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage()
           case ("-verbose")
             verbose =.true.
           case ("-pncf")
             pncf =.true.
           case ("-damp")
             damp =.true.
           case ("-noremap")
             doremap = .false.
           case ("-remap2central")
             remap2central = .true.
             remap2member  = .false.
           case ("-rc")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, RCfile )
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call usage()
             files(nfiles) = argv

         end select

      end do
      ainf_ps = alpha
      ainf_ts = alpha

      if ( nfiles .lt. 1 ) call usage()

!     Load resources: some of these will overwrite command line settings
!     --------------
      if ( trim(RCfile) .ne. 'NONE' ) then
        call i90_loadf (trim(RCfile), iret)
        if( iret .ne. 0) then
           write(stdout,'(3a)') trim(myname),': Warning, I90_loadf cannot find file', trim(RCfile)
           rc = 1
           return
        end if
        write(stdout,'( a  )') '---------------------------------------------------------'
        write(stdout,'(3a  )') trim(myname), ': Reading resource file: ', trim(RCfile)
        write(stdout,'( a,/)') '---------------------------------------------------------'


!       Read in number of levels in background field
!       --------------------------------------------
        nlevs = -1
        call I90_label('dyn_recenter_nlevs:', iret)
        if (iret==0) then
           nlevs = I90_GInt(iret)
        else
           write(stderr,'(2a,i5)') trim(myname),': cannot determine no. of levels, aborting ... '
           call exit(1)
        end if

!       Read in ocean-only option
!       -------------------------
        call I90_label('dyn_recenter_2d_ocean_only:', iret)
        if (iret==0) then
           call I90_GToken(token, iret)
           if (iret==0) then
              if(trim(token) == 'yes' .or. trim(token) == 'YES' ) then
                 ocean_only = .true.
              endif
           endif
           write(stderr,'(3a)') trim(myname),': 2d-ocean-only: ', trim(token)
        end if

!       Read in number of levels in background field
!       --------------------------------------------
        nivars=0
        call I90_label('dyn_recenter_add_inf_vars:', iret)
        do while (iret==0)  ! figure out how many variables ...
           call I90_GToken(token, iret )
           if (iret==0) then
               nivars = nivars + 1
           endif
        end do
        allocate(vars(nivars))
        nivars= 0 
        call I90_label('dyn_recenter_add_inf_vars:', iret)
        do while (iret==0)  ! read again to get variables ...
           call I90_GToken(token, iret )
           if (iret==0) then
               nivars = nivars + 1
               vars(nivars) = trim(token)
           endif
        end do

!       Read table with variable types
!       ------------------------------
        allocate(ainf(nlevs,nivars))
        do iv=1,nivars ! for each variable ...
           write(tablename,'(3a,i3.3,a)') 'dyn_recenter_add_inf_', trim(vars(iv)), '_', nlevs, '::'
           call I90_label(trim(tablename), iret)
           if (iret/=0) then
              write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                 ': trying to read ', trim(tablename)
              call exit(2)
           end if
           irow = 0
           write(stdout,'(a)') ' Reading vertically varying inflation ...'
           do while (iret==0)                   ! read table entries
              call I90_GLine ( iret )           ! iret=-1: end of file; +1: end of table
              if (iret==0.and.irow<=nlevs) then ! OK, we have next row of table
                  irow = irow + 1
    
                  call I90_GToken(token, ier )
                  if(ier/=0) then
                    write(stderr,'(2a,i5)') trim(myname),': cannot read 1st entry in table, aborting ...'
                    call exit(3)
                  endif
                  call I90_GToken(token, ier )
                  if(ier/=0) then
                    write(stderr,'(2a,i5)') trim(myname),': cannot read 2nd entry in table, aborting ...'
                    call exit(4)
                  endif
                  read(token,*) ainf(irow,iv) 
              end if
           end do
           if(irow/=nlevs) then
             write(stderr,'(2a,i5)') trim(myname),': inconsistent number of levels in table, aborting ...'
             call exit(4)
           endif
        end do ! iv

!       Read in additive inflation for PS
!       ---------------------------------
        call I90_label('dyn_recenter_add_inf_ps:', iret)
        if (iret==0) then
           ainf_ps = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot addinf_coeff(ps), aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get addinf_coeff(ps) from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf coeff (ps) = ', ainf_ps 

!       Read in additive inflation for TS
!       ---------------------------------
        call I90_label('dyn_recenter_add_inf_ts:', iret)
        if (iret==0) then
           ainf_ts = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot addinf_coeff(ts), aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get addinf_coeff(ts) from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf coeff (ts)= ', ainf_ts 

!       Read in additive inflation pressure threshold (Pa)
!       -------------------------------------------------
        call I90_label('dyn_addinf_pthreshold_pa:', iret)
        if (iret==0) then
           pkthresh = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot pkthresh, aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get pthreshold from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf pthreshold = ', pkthresh 

!       release resource file:
!       ---------------------
        call I90_release()

      endif ! RCfile


!     Error check
!     -----------
      iret=0
      if (remap2central) then
         if(remap2member) iret = 1
      endif
      if (remap2member) then
         if(remap2central) iret = 1
      endif
      if(iret/=0) then
         print*, 'invalid remapping choice'
         call usage()
      endif
      if (.not.doremap) then ! make sure all remap is turned off
         remap2member = .false.
         remap2central= .false.
      endif

!     Echo the parameters
!     -------------------
      if (verbose) then
         print *
         print *, '  Eta     Dynamics state files: '
         print *, '  ------------------------------'
         print *, 'ensemble member: ', trim(files(1))
         print *, 'ensemble mean:   ', trim(files(2))
         print *, 'new mean:        ', trim(files(3))
         if(remap2member ) print *, 'Will remap to member  topography'
         if(remap2central) print *, 'Will remap to central topography'
      endif

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *,'Usage: '
      print *
      print *,'  dyn_recenter.x [options] '
      print *, '                 x_e(i) x_m x_a [-o output file]'
      print *
      print *, 'where [options]'
      print *
      print *, '-h              Help (optional)'
      print *, '-g5             Treats files as GEOS-5 files'
      print *, '-damp           Apply damp to levels above 5mb'
      print *, '-noremap        Force no-remap whatsoever'
      print *, '-remap2central  Remap member and ensmean to central'
      print *, '                (Default: remap central and ensmean to member'
      print *, '-a       factor Multiplicative factor for inflating perturbations'
      print *, '-inflate fname  filename containing inflating perturbations'
      print *, '-verbose        echoes general information'
      print *, '-o       fname  filename of resulting recentered fields'
      print *, '                (CAUTION, default: overwrite x_e(i) file)'
      print *
      print *, ' Required inputs:'  
      print *, '  x_e(i)  - filename of field to be recenter '
      print *, '  x_m     - original mean'
      print *, '  x_a     - new mean around which member gets recentered'
      print *
      print *, ' Remarks: '
      print *, '  1. This program is used in context of ensemble DAS to recenter '
      print *, '     dyn-vector around desired mean. That is, assuming the ensemble'
      print *, '     mean is x_m, and a desired center mean is x_a, this program '
      print *, '     reads multiple members x_e of an ensemble of dyn-vectors '
      print *, '     and calculates: x_e(i) = x_e(i)) - x_m + x_a, for ensemble '
      print *, '     member i. '
      print *, '  2. There are a million ways to write a more efficient code'
      print *, '     for this - indeed one might need to do this using ESMF'
      print *, '     to better handle high-resolution fields.'
      print *
      call exit(1)
      end subroutine usage
      
!.................................................................
      subroutine my_saxpy_(add,x,y,z,pklim)
      implicit none
!
! !REVISION HISTORY:
!    
!  16jul2015   Todling    Revisit ps - recalc after delp change
!  10oct2015   Todling    Bug fix in ps inc (ptop was incorrect); spotted by Wei Gu
!    

      logical, intent(in)         :: add
      type(dyn_vect), intent(in)  :: x,y
      type(dyn_vect), intent(inout) :: z
      real   ,intent(in),optional :: pklim
!     logical var might seem foolish, but it avoids floating point
!     operator that would be required by passing 1.0 or -1.0
      integer k,km,lm_min
      real,allocatable,dimension(:)::pref
      real alf,pk,pklim_
      pklim_ = 0.0
      lm_min=min(size(z%q,4),min(size(x%q,4),size(y%q,4)))
      if (present(pklim)) then
          pklim_=pklim
      endif
      if ( add ) then
          z%ts = x%ts   + y%ts
          km=size(x%delp,3)
          if(verbose) write(6,'(1x,a,2(f5.2,a))') 'Damping Increments Between ', &
                                       pabove/100,' mb and ' ,pbelow/100, ' mb'
          allocate ( pref(km+1) )
          do k=1,km+1
             pref(k) = x%grid%ak(k) + 100000.0*x%grid%bk(k)
          enddo
          do k=1,km
             pk =0.5*(pref(k+1)+pref(k))
             if( pk.lt.pklim_ ) cycle  ! do not add perturbation above pklim
             if( pk.lt.pbelow .and. damp )  then
               alf=max( 0.0, (pk-pabove)/(pbelow-pabove) )
               if(verbose) write(6,'(1x,a,i3,a,f6.2,a,f7.4)') 'Level: ',k,&
                                    ' Pmid: ',pk/100,' Damping Coef: ',alf
               z%delp(:,:,k)        = alf*x%delp(:,:,k)        + y%delp(:,:,k)
               z%u(:,:,k)           = alf*x%u(:,:,k)           + y%u(:,:,k)
               z%v(:,:,k)           = alf*x%v(:,:,k)           + y%v(:,:,k)
               z%pt(:,:,k)          = alf*x%pt(:,:,k)          + y%pt(:,:,k)
               if(present(pklim)) then
                  z%q(:,:,k,1)        = alf*x%q(:,:,k,1)       + y%q(:,:,k,1)
               else
                  z%q(:,:,k,1:lm_min) = alf*x%q(:,:,k,1:lm_min) + y%q(:,:,k,1:lm_min)
               endif
             else
               z%delp(:,:,k)        = x%delp(:,:,k)            + y%delp(:,:,k)
               z%u(:,:,k)           = x%u(:,:,k)               + y%u(:,:,k)
               z%v(:,:,k)           = x%v(:,:,k)               + y%v(:,:,k)
               z%pt(:,:,k)          = x%pt(:,:,k)              + y%pt(:,:,k)
               if(present(pklim)) then
                 z%q(:,:,k,1)        = max(0.0,x%q(:,:,k,1)        + y%q(:,:,k,1))
               else
                 z%q(:,:,k,1:lm_min) = max(0.0,x%q(:,:,k,1:lm_min) + y%q(:,:,k,1:lm_min))
               endif
             endif
          enddo
          deallocate(pref)
      else
          z%ts        = x%ts   - y%ts
          z%delp      = x%delp - y%delp
          z%u         = x%u    - y%u
          z%v         = x%v    - y%v
          z%pt        = x%pt   - y%pt
          z%q(:,:,:,1:lm_min) = x%q(:,:,:,1:lm_min) - y%q(:,:,:,1:lm_min)
      endif

!     Recalculate ps for consistency w/ delp
!     --------------------------------------
      z%ps = z%grid%ptop
      do k=1,size(z%delp,3)
         z%ps = z%ps + z%delp(:,:,k)
      enddo
      end subroutine my_saxpy_
!.................................................................
      subroutine my_sscal_(alpha,ainf,ainf_ps,ainf_ts,z,omask)
      real,intent(in) :: alpha
      real,pointer,intent(in) :: ainf(:,:)
      real,pointer,intent(in),optional :: omask(:,:)
      real,intent(in) :: ainf_ps,ainf_ts
      type(dyn_vect), intent(inout) :: z
      real factor
      integer nlevs,iv
      if(associated(ainf)) then
        print *, 'Vertically varying additive inflation coefficients:'
        nlevs = size(ainf,1)
        if(ainf_ps<0.0) then
          factor = 0.0
        else
          factor = ainf_ps
        endif
        if(present(omask)) then
          print *, 'Masking out non-ocean surfaces ...'
          where(omask>0.99)
             z%ps  = factor*z%ps
          elsewhere
             z%ps  = 0.0
          end where
          do k = 1, nlevs
             where(omask>0.99)
               z%delp(:,:,k)   = factor*z%delp(:,:,k)
             elsewhere
               z%delp(:,:,k)   = 0.0
             end where
          enddo
        else
           z%ps  = factor*z%ps
           do k = 1, nlevs
              z%delp(:,:,k)   = factor*z%delp(:,:,k)
           enddo
        endif
        if(ainf_ts<0.0) then
          factor = 0.0
        else
          factor = ainf_ts
        endif
        if(present(omask)) then
          where(omask>0.99)
             z%ts  = factor*z%ts
          elsewhere
             z%ts  = 0.0
          end where
        else
          z%ts  = factor*z%ts
        endif
        do iv=1,size(vars,1)
           if(trim(vars(iv))=='u') then
              do k = 1, nlevs
                 z%u   (:,:,k)   = ainf(k,iv)*z%u   (:,:,k)
              enddo
           endif
           if(trim(vars(iv))=='v') then
              do k = 1, nlevs
                 z%v   (:,:,k)   = ainf(k,iv)*z%v   (:,:,k)
              enddo
           endif
           if(trim(vars(iv))=='tv') then
              do k = 1, nlevs
                 z%pt  (:,:,k)   = ainf(k,iv)*z%pt  (:,:,k)
              enddo
           endif
           if(trim(vars(iv))=='sphu') then
              do k = 1, nlevs
                 z%q   (:,:,k,1) = ainf(k,iv)*z%q   (:,:,k,1)
              enddo
           endif
           if(trim(vars(iv))=='ozone') then
              do k = 1, nlevs
                 z%q   (:,:,k,2) = ainf(k,iv)*z%q   (:,:,k,2)
              enddo
           endif
           if(trim(vars(iv))=='qitot') then
              do k = 1, nlevs
                 z%q   (:,:,k,3) = ainf(k,iv)*z%q   (:,:,k,3)
              enddo
           endif
           if(trim(vars(iv))=='qltot') then
              do k = 1, nlevs
                 z%q   (:,:,k,4) = ainf(k,iv)*z%q   (:,:,k,4)
              enddo
           endif
           if(trim(vars(iv))=='qrtot') then
              do k = 1, nlevs
                 z%q   (:,:,k,5) = ainf(k,iv)*z%q   (:,:,k,5)
              enddo
           endif
           if(trim(vars(iv))=='qstot') then
              do k = 1, nlevs
                 z%q   (:,:,k,6) = ainf(k,iv)*z%q   (:,:,k,6)
              enddo
           endif
        enddo ! iv
        return
      endif
      if(alpha<-990.) return
      z%ts        = ainf_ts*z%ts
      z%ps        = ainf_ps*z%ps
      z%delp      = alpha*z%delp
      z%u         = alpha*z%u
      z%v         = alpha*z%v
      z%pt        = alpha*z%pt
      z%q(:,:,:,:) = alpha*z%q(:,:,:,:)
      end subroutine my_sscal_
!.................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      write(*,'(a)') trim(myname) // ': ' // trim(msg)
      call exit(1)
      end subroutine die

!.................................................................
      logical function getphis_(phis,remapinfo)

      character(len=*),intent(out) :: remapinfo
      real,intent(inout) :: phis(:,:)
      type(dyn_vect) x_a

      getphis_ = .false.
      remapinfo='not remapped'
      if(remap2central.or.remap2member) then
         if(remap2member) then      ! will remap ensemble mean and central to member
            phis=x_e%phis
            remapinfo='remapped to member topography'
            getphis_ = .true.
            return
         else
            remapinfo='remapped to central topography'
         endif
      else ! nothing to do
         return
      endif

!  Get mean to recenter ensemble around
!  -------------------------------------
   if ( files(3)/='NONE') then
      call dyn_getdim ( files(3), imm, jmm, kmm, lmm, rc )
      if(km/=kmm) then ! ignore diff in lm for now
         print *, trim(myname), ': km/lm members     = ', km ,lm
         print *, trim(myname), ': km/lm central mean= ', kmm,lmm
         call die(myname,'inconsistent km/lm')
      endif
      if(im/=imm.or.jm/=jmm) then

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, lm, x_a, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_a)')
              endif

!        Read full resolution central analysis
!        -------------------------------------
         call dyn_get ( files(3), nymd, nhms, x_x, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if

!        Interpolate to required resolution
!        ----------------------------------
         x_a%grid%lm = min(x_a%grid%lm,x_x%grid%lm)  ! fix lm
         x_x%grid%lm = x_a%grid%lm ! fix lm
         call h_map ( x_x, x_a, rc, lwifile='NONE', dgrid=.false. )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_a )
                   print *, 'h_map error code = ', rc
                   call die(myname,' failed in h_map')
              else
                   call dyn_clean ( x_x )
              endif
         print*, myname, ': interpolated central analysis to member resolution'
         print*, myname, ': from ', imm, 'x', jmm, ' to ', im, 'x', jm

      else ! Resolution of members is consistent w/ that of central analysis
           ! Simply read in mean analysis

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( imm, jmm, kmm, lmm, x_a, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_a)')
              endif

         call dyn_get ( files(3), nymd, nhms, x_a, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if

      endif

!     Save phis
!     ---------
      phis = x_a%phis
      getphis_ = .true.

!     Clean up mean
!     -------------
      call dyn_clean (x_a) 

   endif

   end function getphis_

  end program dyn_recenter
