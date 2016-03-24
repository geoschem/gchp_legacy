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

      use m_maph, only : h_map
      use m_maph_pert, only: h_map_pert
      use m_topo_remap, only: dyn_topo_remap

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
!
! Remarks:
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

!     Locals
!     ------
      integer, parameter :: READ_ONLY = 1
      integer nfiles
      integer fid, nvars, ngatts
      integer rc
      integer ntimes, k, freq, nymd, nhms, nymd0, nhms0, prec
      integer im, jm, km, lm, system, dyntype
      integer imm, jmm, kmm, lmm
      integer lm_mean,lm_pert,lm_central
      real    pkthresh, alpha
      real,allocatable:: phis(:,:)
      logical remap2central,remap2member
      logical damp
      logical verbose
      logical gotphis
      logical pncf
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
   end if

   allocate(phis(x_e%grid%im,x_e%grid%jm))
   gotphis = getphis_(phis,remapinfo)
   imm=x_e%grid%im
   jmm=x_e%grid%jm
   kmm=x_e%grid%km
   lmm=x_e%grid%lm
   lm_mean=lmm

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
 
!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_e)')
              endif
   
!        Read full resolution perturbation
!        ---------------------------------
         call dyn_get ( trim(dyn_inflate), nymd0, nhms0, x_x, rc, timidx=1, freq=freq, vectype=dyntype, pncf=pncf )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if

!        Interpolate to required resolution
!        ----------------------------------
         x_m%grid%lm=min(lm_pert,lm) ! only interp minimal set of fields (others are zero) 
         call h_map_pert ( x_x, x_m, rc )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_m )
                   print *, 'h_map error code = ', rc
                   call die(myname,' failed in h_map')
              else
                   call dyn_clean ( x_x )
              endif
         x_m%grid%lm = lm ! reset lm-dim(x_m)
         print*, myname, ': interpolated additive perturbation to member resolution'
         print*, myname, ': from ', imm, 'x', jmm, ' to ', im, 'x', jm

      else

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( imm, jmm, kmm, lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

         x_m%grid%lm = lm_pert ! read only q variables in file
         call dyn_get ( trim(dyn_inflate), nymd0, nhms0, x_m, rc, timidx=1, freq=freq, vectype=dyntype, pncf=pncf )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read inflating perturbation file')
         end if
         x_m%grid%lm = lm ! reset number of q variables to total dim

      endif

!     Add inflating perturbation: x_m=x_p inflating vector
!     --------------------------
      call my_sscal_(alpha,x_m)
      call my_saxpy_(.true.,x_m,x_e,x_e,pklim=pkthresh) ! do not inflate above 10mb   

!     Clean up mean
!     -------------
      call dyn_clean (x_m) 

   endif ! <inflation>

!  Get mean to recenter ensemble around
!  -------------------------------------
   if ( files(3)/='NONE') then
      call dyn_getdim ( files(3), imm, jmm, kmm, lmm, rc )
      if(km/=kmm) then ! ignore diff in lm for now
         print *, trim(myname), ': km/lm members     = ', km ,lm
         print *, trim(myname), ': km/lm central mean= ', kmm,lmm
         call die(myname,'inconsistent km/lm')
      endif
      lm_central=lmm

      if(im/=imm.or.jm/=jmm) then

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

!        Read full resolution central analysis
!        -------------------------------------
         call dyn_get ( files(3), nymd, nhms, x_x, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if

!        Interpolate to required resolution
!        ----------------------------------
         x_m%grid%lm = min(lm_central,lm) ! fix lm of interpolated fields 
         x_x%grid%lm = x_m%grid%lm        ! fix lm of input fields
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
         call dyn_init ( imm, jmm, kmm, lm, x_m, rc, &
                         x_e%grid%ptop, x_e%grid%ks, x_e%grid%ak, x_e%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_m)')
              endif

         x_m%grid%lm = lm_central
         call dyn_get ( files(3), nymd, nhms, x_m, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read new mean file')
         end if
         x_m%grid%lm = lm

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
                         pkthresh, alpha, pncf, &
                         verbose )

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
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc
      character(len=255) :: argv
      logical  doremap

      files = 'NONE'
      dyn_inflate = 'NONE'
      dyn_out = 'NONE'
      dyntype  = 4        ! default: GEOS-4 files
      pkthresh = 100.0    ! default: ignore additive inflation above pkthresh (Pa)
      alpha   = -999.0    ! default: do not apply multiplicative factor
      damp    = .false.
      remap2central = .false. ! remap ensmean and member to central topography
      remap2member  = .true.  ! remap ensmean and central to member topography
      doremap = .true.
      pncf = .false.
      verbose = .false.

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
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call usage()
             files(nfiles) = argv

         end select

      end do

      if ( nfiles .lt. 1 ) call usage()

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
      real alf,pk,pabove,pbelow,pklim_
      pklim_ = 0.0
      pabove = 100.0 ! 1-mb
      pbelow = 500.0 ! 5-mb
      lm_min=min(x%grid%lm,y%grid%lm)
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
                 z%q(:,:,k,1)         = x%q(:,:,k,1)       + y%q(:,:,k,1)
               else
                 z%q(:,:,k,1:lm_min) = x%q(:,:,k,1:lm_min) + y%q(:,:,k,1:lm_min)
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
      z%ps = 0.0
      do k=1,size(z%delp,3)
         z%ps = z%ps + z%delp(:,:,k)
      enddo
      end subroutine my_saxpy_
!.................................................................
      subroutine my_sscal_(alpha,z)
      real,intent(in) :: alpha
      type(dyn_vect), intent(inout) :: z
      if(alpha<-990.) return
      z%ts        = alpha*z%ts
      z%ps        = alpha*z%ps
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
